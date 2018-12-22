unit commands_file;

interface


uses
{$IFDEF MSWINDOWS}
{$DEFINE USE_COPY_FILE_EX}
  windows,
{$ENDIF}
  commandprocessor, typex,classes, systemx, tickcount, commandicons, ioutils, debug, numbers, orderlyinit, stringx;



{$IFDEF ALLOW_COMPRESSION}
const
  MAX_COMPRESS_SIZE = 100000000;
{$ENDIF}

type
  TfileOp = (foCopy, foMove);
  TMultiMode = (mmToBest, mmToAll);

  TFileCopyCommand = class(TCommand)
  private
    FSource: string;
    FDest: string;
    FSize: int64;
    FLatency: cardinal;
    FFileOp: TfileOp;
    FCompress: boolean;
    FLAstTime: cardinal;
    FStartTime: cardinal;
    FLastTransfer: nativefloat;
    FStatFrom: TResourceHealthData;
    FStatTo: TResourceHealthData;
    FWakingUp: boolean;
    procedure RecalculateExpense;
    procedure SetDest(const Value: string);
    procedure SetSource(const Value: string);
    procedure SetSize(const Value: int64);
    procedure SetLatency(const Value: cardinal);
    procedure AssignStats;
    procedure ChooseFinalDestination;
    procedure SetFileOp(const Value: TfileOp);
  public
    constructor Create;override;

    property WakingUp: boolean read FWakingUp write FWakingUp;
    property Source: string read FSource write SetSource;
    property Destination: string read FDest write SetDest;
    property Size: int64 read FSize write SetSize;
    property Latency: cardinal read FLatency write SetLatency;
    property Compress: boolean read FCompress write FCompress;
    procedure DoExecute;override;
    property FileOp: TfileOp read FFileOp write SetFileOp;
    class function BeginCopyFile(sSource, sTarget: string): TfilecopyCommand;
    procedure UpdateProgress(TotalTransferred, TotalSize: int64);

  end;

  TfileMoveCommand = class(TFileCopycommand)
  public
    constructor Create;override;
    class function BeginMoveFile(sSource, sTarget: string): TfileMoveCommand;


  end;

  TFileDeleteCommand = class(TCommand)
  private
    FFile: string;
  public
    procedure InitExpense;override;
    procedure DoExecute;override;
    property FileToDelete: string read FFile write FFile;
  end;


function CopyProgressRoutine(
    TotalFileSize, TotalBytesTransferred, StreamSize,StreamBytesTransferred: int64;
    dwStreamNumber: cardinal;
    dwCallbackReason: cardinal;
    hSourcefile, hDestinationFile: Thandle;
    lpData: pointer): DWORD;stdcall;






var
  fileCommands: TCommandProcessor;

implementation

uses
  sysutils;

procedure TFileCopyCommand.AssignStats;
begin
      if assigned(self.Processor) then begin
        FStatFrom := Self.Processor.ResourceStats.GetStatForResource(ExtractNetworkRoot(Source));
        FStatTo := Self.Processor.ResourceStats.GetStatForResource(ExtractNetworkRoot(Destination));
      end;
end;

class function TFilecopycommand.BeginCopyFile(sSource,
  sTarget: string): TfilecopyCommand;
begin
  result := TfileCopycommand.create;
  result.Source := sSource;
  result.Destination := sTarget;
  result.start;



end;

class function TFileMovecommand.BeginMoveFile(sSource,
  sTarget: string): TfileMoveCommand;
begin
  result := TfileMovecommand.create;
  result.Source := sSource;
  result.Destination := sTarget;
  result.FileOp := fomove;
  result.start;


end;

procedure TFileCopyCommand.ChooseFinalDestination;
var
  sl: TStringlist;
  t: integer;
  r, rMin: nativefloat;
begin
  rMin := 9999999;
  sl := TStringlist.create;
  try
    sl.text := FDest;
    for t:= 0 to sl.count-1 do begin
      r := self.Processor.GetCurrentScheduledCustomResourceExpense(ExtractNetworkRoot(sl[t]));
      if r< rMin then begin
        rMin := r;
        FDest := sl[t];
      end;
    end;

  finally
    sl.free;
  end;

end;

constructor TFilecopycommand.Create;
begin
  inherited;
  CPUExpense := 0;
  FLatency := 0;
  FLAstTime := GetTicker;
  Icon := @CMD_ICON_FILE_COPY;

//  DiskExpenseByString[GetDrive(Source)] := 1;
//  DiskExpenseByString[GetDrive(Destination)] := 1;
//  NetworkExpense := 1;
end;

procedure TFilecopycommand.DoExecute;
var
  i: longbool;
  sExt: string;
  sPath: string;
begin
  inherited;
  try

    if Processor = nil then
      raise Exception.create('No processor set');

    if (zpos(#13, FDest) > -1) or (zpos(#10, FDest) > -1) then
      ChooseFinalDestination;

    self.Status := FSource+'->'+FDest;


    sPath := extractFilePath(FDest);
    if sPath <> '' then
      ForceDirectories(spath);


{$IFNDEF WINDOWS}
  {$Message Hint 'FileCopycommand on IOS/Andriod does not set Attributes'}
{$ELSE}
    if fileExists(FDest) then begin
      FileSetAttr(PChar(FDest), (FileGetAttr(PChar(FDest)) and not faReadOnly));
    end;
{$ENDIF}


    if FileOP = foCopy then begin
      Debug.Log(self,'Start copy of '+FSource, 'filecopy');

      {$IFDEF IOS}
        {$Message Hint 'FileCopycommand on IOS does not set Attributes'}
      {$ELSE}

        {$IFDEF ALLOW_COMPRESSION}
        if (TFileAttribute.faCompressed in TFile.GetAttributes(FSource)) then begin
          Resources.SetResourceUsage(ExtractNetworkRoot(FSource),1.0);
//        FStatFrom := Self.Processor.ResourceStats.GetStatForResource(ExtractNetworkRoot(FSource));


        //DiskExpenseByString[GEtDrive(FSource)] := 1.0;
          WaitForResources;
        end;
        {$ENDIF}
      {$ENDIF}
      FStartTime := GetTicker;
{$IFDEF USE_COPY_FILE_EX}
      i := CopyFileEx(PChar(Fsource), PChar(FDest), @CopyProgressRoutine, pointer(self), nil, 0);
{$ELSE}
      i := true;
      try
        TFile.Copy(Fsource,FDest, true);
      except
        i := false;
      end;
{$ENDIF}
      if (i) then
        Debug.Log(self,'Copy Finished '+FSource, 'filecopy')
      else begin
        Debug.Log(self,'Copy FAILED! '+FSource+'->'+FDest, 'error');
        self.CResult := false;
      end;
    end else begin
      Debug.Log(self,'Start move of '+FSource, 'filecopy');
      {$IFDEF ALLOW_COMPRESSION}
      if Compress and (TfileAttribute.faCompressed in TFile.GetAttributes(FSource)) then begin
        Resources.SetResourceUsage(ExtractNetworkRoot(FSource),1.0);
//        FStatFrom := Self.Processor.ResourceStats.GetStatForResource(ExtractNetworkRoot(FSource));
//        DiskExpenseByString[GEtDrive(FSource)] := 1.0;
        WaitForResources;
      end;
      {$ENDIF}
      FStartTime := GetTicker;
{$IFDEF USE_COPY_FILE_EX}
      i := MoveFileWithProgress(PChar(Fsource), PChar(FDest), @CopyProgressRoutine, pointer(self),MOVEFILE_COPY_ALLOWED+MOVEFILE_REPLACE_EXISTING);
{$ELSE}
      i := true;
      try
        TFile.Move(Fsource, FDest);
      except
        i := false;
      end;
{$ENDIF}
      if (i) then begin
        Debug.Log(self,'move Finished '+FSource, 'filecopy')
      end
      else
      begin
        Debug.Log(self,'move FAILED! '+FSource, 'filecopy');
        Self.CResult := false;
      end;
    end;

  //  if fileExists(FDest) then begin
  //    FileSetAttr(PChar(FDest), FILE_ATTRIBUTE_COMPRESSED or (FileGetAttr(PChar(FDest)) and not faReadOnly));
  //  end;


{$IFDEF ALLOW_COMPRESSION}
    if false and Compress then begin
      try

        Source := '';//recalculates expense while compressing on target
        sExt := lowercase(extractfileext(FDest));

        if (sExt <> '.jpg')
        and (sExt <> '.tar')
        and (sExt <> '.mov')
        and  (sExt <> '.zip')
        and  (sExt <> '.mpg')
        and  (sExt <> '.mp4')
        and  (sExt <> '.mp3')
        and  (sExt <> '.ogg')
        and  (sExt <> '.gif')
        and  (sExt <> '.rar')
        and  (sExt <> '.cab')
        and  (sExt <> '.wma')
        and  (pos(')', FDest)=0)
        and (Size > 0)
        and (Size < MAX_COMPRESS_SIZE)
        then begin
          RunProgramAndWait('compact', '/C "'+FDest+'"', '', true, true);
        end;
      except
      end;
    end;
{$ENDIF}
  except
  end;

  //CopyFile(Pchar(Fsource), Pchar(FDest), false);
end;


procedure TFilecopycommand.RecalculateExpense;
var
  r: real;
  r2: real;
begin
  Lock;
  REsources.Lock;
  try
    Resources.Clear;

  //  if Source = '' then Source := ' ';
  //  if Destination = '' then exit;


    if Latency > 200 then
      FLatency := 200;

    r := (200-FLatency)/200;

    MemoryExpense := 1/30;
//    Debug.Log('Calc Expense for: '+source+'->'+destination);
//    Debug.Log('Latency: '+floattostr(Latency),'filecopy');
//    Debug.Log('Latency factor: '+floattostr(r),'filecopy');


    if r < 0.75 then
      r := 0.75;

    if FileOp = foMove then begin
      if lowercase(ExtractNetworkRoot(source)) = lowercase(ExtractNetworkRoot(destination)) then begin
        r := 0.0;
      end;
    end else begin

    end;


    try
      try

        r2 := Size;
//        if r2 < 32768 then r2 := 32768;

        if r2 > 0 then begin
          r2 := r2 / 32768;
          if r2 > 1 then r2 := 1;

          if r = 0 then
            r := 0.0
          else
            r := r2*r;
          if r < 0 then r := 0;
        end else
          r := 0.0;


//        if (r < 1) and (r>0) then begin
//          r := r * r;
//        end;

      finally
      end;

//      r := r * 4000;
      if r > 1 then r := 1;
      if r < (1/2) then r := (1/2);
//
//      Debug.Log('Source: '+GetDrive(Source),'filecopy');
//      Debug.Log('Destination: '+GetDrive(Destination),'filecopy');


      Resources.SetResourceUsage(ExtractNetworkroot(Source), Resources.GetResourceUsage(ExtractNetworkroot(Source))+(r));
      Resources.SetResourceUsage(ExtractNetworkroot(Destination), Resources.GetResourceUsage(ExtractNetworkroot(Destination))+(r));
      AssignStats;


//      Debug.Log('End Expense: '+floattostr(r),'filecopy');

    except

    end;
  finally
    Resources.Unlock;
    Unlock;
  end;


end;

procedure TFilecopycommand.SetDest(const Value: string);
begin
  FDest := Value;
  UniqueString(FDest);
  RecalculateExpense;
end;

procedure TFileCopyCommand.SetFileOp(const Value: TfileOp);
begin
  FFileOp := Value;
  if value = foMove then
    Icon := @CMD_ICON_FILE_MOVe
  else
    Icon := @CMD_ICON_FILE_COPY;

end;

procedure TFilecopycommand.SetLatency(const Value: cardinal);
begin
  FLatency := Value;
  RecalculateExpense;
end;

procedure TFilecopycommand.SetSize(const Value: int64);
begin
  FSize := Value;
  RecalculateExpense;
end;

procedure TFilecopycommand.SetSource(const Value: string);
begin
  FSource := value;
  UniqueString(FSource);
  Size := idglobalprotocols.FileSizeByName(value);
  RecalculateExpense;
end;

procedure TFileCopyCommand.UpdateProgress(TotalTransferred, TotalSize: int64);
var
  tm: Cardinal;
  tmDelta, tmTotal, TransferDelta: nativefloat;
  r1,r2: nativefloat;
begin
  if (TotalSize div 1000000) < 100 then begin
    StepCount := TotalSize div 10000;
    Step := TotalTransferred div 10000;
  end else begin
    StepCount := TotalSize div 1000000;
    Step := TotalTransferred div 1000000;
  end;

  tm := GetTicker;
  tmDelta := GetTimeSince(tm, FLastTime) / 1000;
  tmTotal := GetTimeSince(tm, FStartTime) / 1000;
  TransferDelta := TotalTransferred- FLastTransfer;
  FLastTransfer := TotalTransferred;
  FLastTime := tm;

  if tmDelta = 0 then exit;
  if self.WakingUp then begin
    self.WakingUp := false;
    exit;
  end;

  AssignStats;


  if TransferDelta > 0 then begin
    if assigned(FStatTo) then begin
      FStatTo.ApplyStat(tmTotal, TotalTransferred);
//      if TotalSize > LARGE_FILE_THRESHOLD then begin
//        self.Resources.SetResourceUsage(FStatTo.Resource,(TransferDelta/tmDelta) / FStatTo.MaxLarge);
//      end else begin
//        self.Resources.SetResourceUsage(FStatTo.Resource,(TransferDelta/tmDelta) / FStatTo.MaxSmall);
//      end;
    end;

    if assigned(FStatFrom) then begin
      FStatFrom.ApplyStat(tmTotal, TotalTransferred);
//      if TotalSize > LARGE_FILE_THRESHOLD then begin
//        self.Resources.SetResourceUsage(FStatFrom.Resource,(TransferDelta/tmDelta) / FStatFrom.MaxLarge);
//      end else begin
//        self.Resources.SetResourceUsage(FStatFrom.Resource,(TransferDelta/tmDelta) / FStatFrom.MaxSmall);
//      end;
    end;

    if assigned(FStatFrom) and assigned(FStatTo) then begin
      if TotalSize > LARGE_FILE_THRESHOLD then begin
        FSTatTo.MaxLarge  := greaterof(FStatTo.MaxLarge, 1);
        FSTatFrom.MaxLarge  := greaterof(FStatTo.MaxLarge, 1);
        r1 := (TransferDelta/tmDelta) / FStatTo.MaxLarge;
        r2 := (TransferDelta/tmDelta) / FStatFrom.MaxLarge;
      end
      else begin
        FSTatTo.MaxSmall  := greaterof(FStatTo.MaxSmall, 1);
        FSTatFrom.MaxSmall  := greaterof(FStatTo.MaxSmall, 1);
        r1 := (TransferDelta/tmDelta) / FStatTo.MAxSmall;
        r2 := (TransferDelta/tmDelta) / FStatFrom.MAxSmall;
      end;

      //ONE OF THE TWO must be above 50% usage or they will both be considered
      //maxed out
      if (r1 > 0.4) or (r2 > 0.4) then begin
        self.Resources.SetResourceUsage(FStatFrom.Resource,r2);
        self.Resources.SetResourceUsage(FStatTo.Resource,r1);
      end else begin
          if (r1 < 0.1) and (r2 < 0.1) then begin
            self.Resources.SetResourceUsage(FStatFrom.Resource,lesserof(r2*5,1.0));
            self.Resources.SetResourceUsage(FStatTo.Resource,lesserof(r1*5,1.0));
            self.WaitForResources(30000);
            self.WakingUp := true;
          end;
          //FLAstTime := GetTicker;
      end;



    end;

  end;
end;


{ TfileMoveCommand }

constructor TfileMoveCommand.Create;
begin
  inherited;
  FileOp := foMove;
end;





{ TFileCopyLookupTable }


function CopyProgressRoutine(
    TotalFileSize, TotalBytesTransferred, StreamSize,StreamBytesTransferred: int64;
    dwStreamNumber: cardinal;
    dwCallbackReason: cardinal;
    hSourcefile, hDestinationFile: Thandle;
    lpData: pointer): DWORD;stdcall;
var
  fc: TFileCopyCommand;
begin
  fc := TFileCopycommand(lpData);
  fc.UpdateProgress(TotalBytesTransferred, TotalFileSize);
  result := 0;

end;


procedure oinit;
begin
  filecommands := TCommandProcessor.create(nil,'File Commands');
end;
procedure ofinal;
begin
  if assigned(filecommands) then begin
    filecommands.free;
    filecommands := nil;
  end;

end;


{ TFileDeleteCommand }

procedure TFileDeleteCommand.DoExecute;
begin
  inherited;
  if FileExists(FileToDelete) then
    DeleteFile(FileToDelete);

end;

procedure TFileDeleteCommand.InitExpense;
begin
  inherited;
  CPUExpense := 0.0;
end;

initialization
  orderlyinit.init.RegisterProcs('commands_file', oinit, ofinal, 'Debug,ManagedThread');

finalization


end.
