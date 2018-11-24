unit ServerVideoParser;
{x$INLINE AUTO}
{$DEFINE USE_PRIVATE_CP}

interface

uses
  soundinterfaces, commandicons, ffmpeg_tools, windows, advancedgraphics_dx, easyimage, graphics, debug,
  sysutils, fileserviceClientEx, dir, dirfile, stringx,  commands_file, huestogram, fastbitmap, ColorBlending, colorconversion,
  types, activex, betterobject, classes, commandprocessor, memoryfilestream, advancedgraphics, tickcount,
  systemx,  typex, generics.Collections, soundtools, multibuffermemoryfilestream;

const
  HUESTOGRAM_FRAMETIME = 1 / 30;
  REMOTE_HUE_CAPACITY = 8;

type
  TLocalStream = TMultiBufferMemoryFileStream;
  ThueDEbugVisualizer = class(TDrawingBoard)
  private
    FH: THuestogram;
  protected
    procedure DoDraw;override;
    property H: THuestogram read FH write FH;
  end;

  Tcmd_RenderHuestogram_FromJVid = class(TCommand)
  private
    FFileName: string;
    FHighres: boolean;
    FOutFileName: string;
    procedure SEtFileName(const Value: string);
  public
    procedure InitExpense;override;
    procedure DoExecute;override;
    property FileName: string read FFileName write SEtFileName;
    property OutputFileName: string read FOutFileName write FOutFileName;
    function HueFileName: string;
    property Highres: boolean read FHighres write FHighres;
  end;

  Tcmd_RenderHuestogram_FromDir = class(TCommand)
  private
    FFileName: string;
    FWidth: integer;
    FHeight: integer;
    FLengthInSeconds: real;
    FRemote: boolean;
    FHighRes: boolean;
    FTempDir: string;
    FOutputFileName: string;
    function GetDirectory: string;
    procedure SetRemote(const Value: boolean);
    procedure SetFileName(const Value: string);
  public
    procedure InitExpense;override;
    procedure DoExecute; override;
    procedure DoExecuteLocal;
    procedure DoExecuteRemote;
    procedure DoExecuteLocalMulti;
    procedure PrepareFrames;
    procedure CleanupFrames;
    procedure AbandonFiles;
    property directory: string read GetDirectory;

    property FileName: string read FFileName write SetFileName;
    property OutputFileName: string read FOutputFileName write FOutputFileName;
    function HueFileName: string;


    property Width: integer read FWidth write FWidth;
    property height: integer read FHeight write FHeight;
    property LengthinSeconds: real read FLengthInSeconds write FLengthInSeconds;
    property Remote: boolean read FRemote write SetRemote;
    function GetLengthFromBoog: nativefloat;
    function mp3file: string;
    property HighRes: boolean read FHighRes write FHighres;
    property TempDir: string read FTempDir write FTempDir;
  end;

  Tcmd_RenderHuestogram_FromDirFrame_FromDir = class(TCommand)
  private
    FOutput: Thuestogram;
    FFrameRate: real;
    FFrameNumber: integer;
    FMasterCommand: Tcmd_RenderHuestogram_FromDir;
    FFileName: string;
    function GetFrameTime: real;

  public
    procedure InitExpense;override;
    Destructor Destroy; override;

    procedure DoExecute; override;

    property FrameRate: real read FFrameRate write FFrameRate;
    property FrameNumber: integer read FFrameNumber write FFrameNumber;
    property FrameTime: real read GetFrameTime;
    property FileName: string read FFileName write FFileName;
    property Output: Thuestogram read FOutput;

    property MasterCommand
      : Tcmd_RenderHuestogram_FromDir read FMasterCommand write FMasterCommand;

  end;

  Tcmd_RenderHuestogram = Tcmd_RenderHuestogram_FromDir;

function GEthuestogramFromFile(sFile: string): THuestogram;

implementation

{ TDX2d_UT }

{ Tcmd_RenderHuestogram_FromDir }

procedure Tcmd_RenderHuestogram_FromDir.AbandonFiles;
var
  s : string;
begin
  s := changefileext(filename, '.rw0');
  deletefile(s);
  s := changefileext(filename, '.rw1');
  deletefile(s);
  s := changefileext(filename, '.hue');
  deletefile(s);





end;


procedure Tcmd_RenderHuestogram_FromDir.CleanupFrames;
begin
  dir.DeleteFolder(self.directory);
end;

procedure Tcmd_RenderHuestogram_FromDir.DoExecute;
begin
  inherited;
  if comparetext(extractfileext(filename),'.mp3') = 0 then
    exit;
  if comparetext(extractfileext(filename),'.wav') = 0 then
    exit;

  if not fileexists(filename) then
    exit;

  if Remote then
    DoExecuteRemote
  else
    DoExecuteLocalMulti;

end;

procedure Tcmd_RenderHuestogram_FromDir.DoExecuteLocal;
type
  TFrameInfo = record

  end;
VAR
  Output: TLocalStream;
  rTime: real;
  fb: TFastBitmap;
  tmStart: cardinal;
  tmnow: cardinal;
  t: integer;
  p: TCommandProcessor;
  c: TframeInfo;
  framerate: nativefloat;
  framenumber: nativeint;
  h: Thuestogram;
  //cl: TList<Tcmd_RenderHuestogram_FromDirFrame_FromDir>;
  dir: TDirectory;
  iTotalFrames: nativeint;
  iTargetFrame: nativeint;
{$IFDEF USE_PRIVATE_CP}
  cpLocal: TCommandProcessor;
{$ENDIF}
begin
  PrepareFrames;
  try
{$IFDEF USE_PRIVATE_CP}
    cpLocal := TCommandProcessor.create(nil, 'CP Hue');
{$ENDIF}
    try
      iTargetFrame := 0;

      Output := nil;
      CPuExpense := 0;
      // create video parsers
      //cl := TList<Tcmd_RenderHuestogram_FromDirFrame_FromDir>.create;

      try
        // setup output stream
        Status := ExtractFileName(FileName);
    //    p := TCommandProcessor.Create(nil, 'Hue build processor');
    //    p := self.Processor.NewSubProcessor;
        try
          Output := TLocalStream.create(HueFileName,
            fmCreate);
          Output.seek(0, 0);
          try
            rTime := 0;

            if self.FLengthInSeconds = 0 then begin
              cresult := false;
              self.ErrorMessage := 'No length available';
              Step := 0;
              Status := 'Cleaning up old shit';
              CLeanupFrames;
              AbandonFiles;
              Step := 1;
            end;
    //        Resources.SetResourceUsage('HueFrames', 1.0);
    //        WaitForResources;
            dir := Tdirectory.Create(directory, '*.jpg', 0,0, false, true);
            try
              iTotalFrames := dir.Filecount;
              rTime := 0;
              tmStart := GetTicker; // for rate calculation
              iTargetFrame := 0;
              for  t:= 0 to dir.Filecount-1 do  begin
                tmnow := GetTicker; // for rate calculation
                FrameRate := dir.Filecount / self.FLengthInSeconds ;
                FrameNumber := round((30/framerate)*t);
                h := Gethuestogramfromfile(dir.Files[t].FullName);
                try
                  while FrameNumber > iTargetFrame do begin
                    h.SaveToStream(output);
                    inc(iTargetFrame);
                  end;
                finally
                  h.Free;
                end;

                if GEtTimeSince(tmnow, tmStart) > 0 then begin
                  Status := floatprecision(LengthinSeconds, 1)
                    + ' Rate:' + floatprecision
                    (rTime / (GEtTimeSince(tmnow, tmStart) / 1000), 3);
                end;

                rTime := rTime + HUESTOGRAM_FRAMETIME;

                Step := round(rTime);
                StepCount := trunc(LengthinSeconds + 1);

              end;
            finally
              dir.free;
            end;
          finally
          end;
        finally
          // cleanup output stream
          Output.Free;
        end;
      finally
        // cleanup
      end;
    finally
    end;
  finally
    CleanupFrames;
  end;

end;

procedure Tcmd_RenderHuestogram_FromDir.DoExecuteLocalMulti;
VAR
  Output: TLocalStream;
  rTime: real;
  fb: TFastBitmap;
  tmStart: cardinal;
  tmnow: cardinal;
  t: integer;
  p: TCommandProcessor;
  c: Tcmd_RenderHuestogram_FromDirFrame_FromDir;
  cl: TList<Tcmd_RenderHuestogram_FromDirFrame_FromDir>;
  dir: TDirectory;
  iTotalFrames: nativeint;
  iTargetFrame: nativeint;
{$IFDEF USE_PRIVATE_CP}
  cpLocal: TCommandProcessor;
{$ENDIF}
begin
  PrepareFrames;
  try
{$IFDEF USE_PRIVATE_CP}
    cpLocal := TCommandProcessor.create(nil, 'CP Hue');
{$ENDIF}
    try
      iTargetFrame := 0;

      Output := nil;
      CPuExpense := 0;
      // create video parsers
      cl := TList<Tcmd_RenderHuestogram_FromDirFrame_FromDir>.create;

      try
        // setup output stream
        Status := ExtractFileName(FileName);
    //    p := TCommandProcessor.Create(nil, 'Hue build processor');
    //    p := self.Processor.NewSubProcessor;
        try
          Output := TLocalStream.create(HueFileName,
            fmCreate);
          Output.seek(0, 0);
          try
            rTime := 0;

            if self.FLengthInSeconds = 0 then begin
              cresult := false;
              self.ErrorMessage := 'No length available';
              Step := 0;
              Status := 'Cleaning up old shit';
              CLeanupFrames;
              AbandonFiles;
              Step := 1;
            end;
    //        Resources.SetResourceUsage('HueFrames', 1.0);
    //        WaitForResources;
            dir := Tdirectory.Create(directory, '*.jpg', 0,0, false, true);
            try
              iTotalFrames := dir.Filecount;
              for  t:= 0 to dir.Filecount-1 do  begin
                c := Tcmd_RenderHuestogram_FromDirFrame_FromDir.create;
                c.MasterCommand := self;
                c.FrameRate := dir.Filecount / self.FLengthInSeconds ;
                c.FrameNumber := round((30/c.framerate)*t);
                c.filename := dir.files[t].fullname;
                cl.add(c);
{$IFDEF USE_PRIVATE_CP}
                C.sTART(cpLocal);
{$ELSE}
                c.Start();
{$ENDIF}
              end;
            finally
              dir.free;
            end;

            rTime := 0;
            tmStart := GetTicker; // for rate calculation
            iTargetFrame := 0;
            while cl.count > 0 do begin
              tmnow := GetTicker; // for rate calculation

              c := cl[0];
              //rTime := c.FrameTime;


              try
                c.WaitFor;
                if cl.Count > 1 then begin
                  while cl[1].FrameNumber > iTargetFrame do begin
                    c.Output.SaveToStream(output);
                    inc(iTargetFrame);
                  end;
                end else begin
                  c.Output.SaveToStream(output);
                  inc(iTargetFrame);
                end;

                //GLOG.Debug('Frame '+inttostr(iTargetFrame));


                if GEtTimeSince(tmnow, tmStart) > 0 then begin
                  Status := c.output.Debug + ' ...' + floatprecision(rTime, 1)
                    + '/' + floatprecision(LengthinSeconds, 1)
                    + ' Rate:' + floatprecision
                    (rTime / (GEtTimeSince(tmnow, tmStart) / 1000), 3);
                end;

                rTime := rTime + HUESTOGRAM_FRAMETIME;
              finally
                if cl.Count > 1 then begin
                  if cl[1].FrameTime < rTime then begin
                    c.free;
                    cl.delete(0);
                  end;
                end else begin
                  c.free;
                  cl.delete(0);
                end;
              end;

              Step := round(rTime);
              StepCount := trunc(LengthinSeconds + 1);
            end;


            // wait for final frames
          finally
    //        p.Free;
    //        self.Processor.DoneWithSubProcess(p);
          end;

        finally
          // cleanup output stream
          Output.Free;
        end;
      finally
        // cleanup
        while cl.Count > 0 do begin
          c := cl[0];
          c.Free;
          cl.Delete(0);
        end;
        cl.free;
      end;
    finally
{$IFDEF USE_PRIVATE_CP}
      if cpLocal.commandcount > 0 then begin
        raise Exception.create('cplocal still has commands!');
      end;
      cpLocal.free;
{$ENDIF}
    end;
  finally
    CleanupFrames;
  end;



end;


procedure Tcmd_RenderHuestogram_FromDir.DoExecuteRemote;
var
  fsc: TFileServiceClientEx;
  f2: string;
begin
  fsc := TFileServiceClientEx.create(DEFAULT_HOST, DEFAULT_PORT);
  try
    DiskExpenseByFileName[ResolveFilename(FileName)] := 1.0;
    Status := 'putting '+extractfilename(FileName);
    fsc.PutFileEx(FileName, extractfilename(FileName));
    DiskExpenseByFileName[ResolveFilename(FileName)] := 0.0;
    Status := 'remote build '+extractfilename(FileName);
    fsc.BuildHueFile(extractfilename(FileName), LengthinSeconds);
    f2 := changefileext(FileName, '.hue');
    DiskExpenseByFileName[ResolveFilename(FileName)] := 1.0;
    Status := 'getting '+extractfilename(FileName);
    fsc.GetFileEx(extractfilename(f2), f2);
    DiskExpenseByFileName[ResolveFilename(FileName)] := 0.0;
    Status := 'cleaning up '+extractfilename(FileName);
    fsc.DeleteFile(extractfilename(f2));
    fsc.DeleteFile(extractfilename(FileName));

  finally
    fsc.Free;
  end;
end;

function Tcmd_RenderHuestogram_FromDir.GetDirectory: string;
begin
  if TempDir <> '' then begin
    result := TempDir+ExtractFileName(filename)+'.dir';
  end else begin
    result := filename+'.dir';
  end;
end;

function Tcmd_RenderHuestogram_FromDir.GetLengthFromBoog: nativefloat;
var
  boog: TSoundStream;
  bh: TBoogerHeader;
  c: Tcmd_RenderHuestogram_FromDir;

begin
  result:= 0;
  boog := TSoundStream.Create(mp3file, fmOpenread + fmShareDenynone);
  try
    result := boog.LengthInSeconds;
  finally
    boog.Free;
  end;

end;

function Tcmd_RenderHuestogram_FromDir.HueFileName: string;
begin
  if FOutputFileName = '' then
    result := FileName+'.hue'
  else
    result := OutputFileName+'.hue';

end;

procedure Tcmd_RenderHuestogram_FromDir.InitExpense;
begin
  Icon := @CMD_ICON_HUE;
  CPuExpense := 0;
  resources.SetResourceUsage('hue', 1/TThread.ProcessorCount);
//  MemoryExpense := 1;
end;

function Tcmd_RenderHuestogram_FromDir.mp3file: string;
begin
  result := filename+'.mp3';

end;

{ Tcmd_RenderHuestogram_FromDirFrame_FromDir }

destructor Tcmd_RenderHuestogram_FromDirFrame_FromDir.Destroy;
begin
  FOutput.Free;
  FOutput := nil;
  inherited;
end;

function GEthuestogramFromFile(sFile: string): THuestogram;
var
  fb: TFastBitmap;
  db: ThueDEbugVisualizer;
begin
  fb := nil;

  try
    fb := nil;
    fb := TFastBitmap.create;

    fb.LoadFromFile(sFile);
    result := THuestogram.Create;
    try
      result.FromFastBitMap(fb);
      result.CalcPrimes;
    except
      result.Free;
      result := nil;
      raise;
    end;
  finally
    fb.Free;
  end;
end;
procedure Tcmd_RenderHuestogram_FromDirFrame_FromDir.DoExecute;
var
  fb: TFastBitmap;
  db: ThueDEbugVisualizer;
begin
  inherited;

  FOutput := GEthuestogramFromFile(FileName);
  DeleteFile(FileName);

end;

function Tcmd_RenderHuestogram_FromDirFrame_FromDir.GetFrameTime: real;
begin
  result := FrameNumber/FrameRate;
end;

procedure Tcmd_RenderHuestogram_FromDirFrame_FromDir.InitExpense;
begin
  inherited;
  Icon := @CMD_ICON_HUE;
  Resources.SetResourceUsage('hueframe', 1/(TThread.ProcessorCount*3));
  CPUExpense := 1;
end;

procedure Tcmd_RenderHuestogram_FromDir.PrepareFrames;
begin
  if highres then
    ffmpeg_tools.ExtractVideoFrames(self.FileName, self.directory, 400,225)
  else
    ffmpeg_tools.ExtractVideoFrames(self.FileName, self.directory, 72,48);

  if FLengthInSEconds = 0 then
    FLengthInSEconds := GetLengthFromBoog;
end;

procedure Tcmd_RenderHuestogram_FromDir.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure Tcmd_RenderHuestogram_FromDir.SetRemote(const Value: boolean);
begin
  Lock;
  try
    FRemote := Value;
    if value then begin
      Resources.SetResourceUsage('remote_hue', 1/REMOTE_HUE_CAPACITY);
    end else begin
      CPuExpense := 1.0;
    end;
  finally
    Unlock;
  end;
end;

{ ThueDEbugVisualizer }

procedure ThueDEbugVisualizer.DoDraw;
var
  r,rr: nativefloat;
  t: nativeint;
begin
  inherited;
    self.Prepare;
    AlphaOp := aoNone;

    if assigned(h) then begin
      rr := 0;
      r := 0;
      boundx1 := 0;
      boundx2 := h.sum;
      boundy1 := 0;
      boundy2 := 1;
      try
        for t := 0 to 8 do begin
          rr := r + h.hues[t].count;

          Rectangle(r,0,rr,1,primary_hues[t], true, false);

          r := rr;
        end;
      finally
      end;
    end;

//    canvas_rectangle_fill(0,0,100,100, primarycolor, secondarycolor, primarycolor, secondarycolor);

end;

{ Tcmd_RenderHuestogram_FromJVid }

procedure Tcmd_RenderHuestogram_FromJVid.DoExecute;
var
  jvi: TJVIdReader;
  rtime: nativefloat;
  fb: TFastBitmap;
  db: ThueDEbugVisualizer;
  output: THuestogram;
  outputstream: TMemoryFileStream;
begin
  inherited;
  jvi := TJVIdReader.Create;
  try
    rTime := 0;
    outputstream := TMemoryFileStream.Create(self.HueFileName, fmCreate);
    try
      jvi.LoadFromFile(self.FileName);
      while rTime < (jvi.mainheader.numberofframes / jvi.mainheader.framerate) do begin
        Stepcount := (jvi.mainheader.numberofframes);
        Step := round(rTime * jvi.mainheader.framerate);
        jvi.GotoTime(rTime);

        fb := nil;

        try

          fb := nil;
          fb := TFastBitmap.create;
          try
            fb.LoadFromMemory_JPG(jvi.framememory, jvi.framememorylength);
            output := THuestogram.Create;
            output.FromFastBitMap(fb);
            output.CalcPrimes;
            output.SaveToStream(outputstream);
          except
            on e:exception do begin
                status := e.message;
            end;
          end;

        finally
          fb.Free;
          //DeleteFile(FileName);
        end;

        rTime := rTime + (1/30);





      end;
    finally
      outputstream.Free;
    end;

  finally
    jvi.Free;
  end;

end;

function Tcmd_RenderHuestogram_FromJVid.HueFileName: string;
begin
  result := changefileext(outputfilename, '.hue');
end;

procedure Tcmd_RenderHuestogram_FromJVid.InitExpense;
begin
  inherited;
  Icon := @CMD_ICON_HUE;
end;

procedure Tcmd_RenderHuestogram_FromJVid.SEtFileName(const Value: string);
begin
  FFileName := Value;
  if FOutFileName = '' then
    FOutFileName := changefileext(value, '.hue');
end;

end.
