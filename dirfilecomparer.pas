unit dirfilecomparer;

interface
{x$DEFINE USE_MFS}
uses
  typex, systemx, MemoryFileStream, sysutils, helpers_stream, numbers, stringx, commandprocessor, tickcount, classes;

type
  TCopyRecommendation = (crNoCopy, crCopyToLeft, crCopyToRight, crNoCopyRightIsNewer, crCopyDisabled);
  TCompareFileDAta = record
    DAte: TDateTime;
    PercentZeros: single;
    Size: int64;
    Zeros: int64;
    FileName: string;
    function ToString: string;
  end;

  TCompareFileResults = record
    Different: int64;
    OperationCompleted: boolean;
    IdenticalBytes: int64;
    Left, Right: TCompareFileData;
    ContentsIdentical: boolean;
    SizeOfComparedContents: int64;
    CopyRecommendation: TCopyRecommendation;
    function Identical: boolean;
    function PercentIdentical: single;
    function ToSTring: string;
  end;

  Tcmd_CompareFiles_Slow = class(TCommand)
  private
    FF2: string;
    FF1: string;
    FResult: TCompareFileResults;
    procedure SetResult(const Value: TCompareFileResults);
    procedure SetF1(const Value: string);
    procedure SEtF2(const Value: string);
  public
    IgnoreDateChecks: boolean;
    CompareOnly: boolean;
    CompareAll: boolean;
    Zerocheck: boolean;
    DiffToConsole: boolean;
    property f1: string read FF1 write SetF1;
    property f2: string read FF2 write SEtF2;
    property Result: TCompareFileResults read FResult write SetResult;
    procedure DoExecute; override;
  end;


  TCmd_CompareFile_AnyInFolder = class(TCommand)
  protected
    procedure DoExecute; override;
  public
    File1: string;
    Dir2: string;
    out_Matched: string;


  end;

function Begin_CompareFiles_Slow(f1,f2: string): Tcmd_CompareFiles_Slow;
function End_CompareFiles_Slow(c: Tcmd_CompareFiles_Slow): TCompareFileResults;
function CompareFiles_Slow(f1,f2: string): TCompareFileResults;



implementation

uses
  dir, dirfile;

function CompareFiles_Slow(f1,f2: string): TCompareFileResults;
begin
  result := End_CompareFiles_Slow(Begin_CompareFiles_Slow(f1,f2));
end;

function Begin_CompareFiles_Slow(f1,f2: string): Tcmd_CompareFiles_Slow;
begin
  result := Tcmd_CompareFiles_Slow.Create;
  result.f1 := f1;
  result.f2 := f2;
  result.start;
end;
function End_CompareFiles_Slow(c: Tcmd_CompareFiles_Slow): TCompareFileResults;
begin
  c.WaitFor;
  result := c.result;
  c.Free;
  c := nil;

end;


{ TCompareFileResults }

function TCompareFileResults.Identical: boolean;
begin
  result := ContentsIdentical and (Left.Size = Right.Size);
end;

function TCompareFileResults.PercentIdentical: single;
begin
  if SizeOfComparedContents = 0 then
    result := 1
  else
    result := IdenticalBytes / SizeOfComparedContents;

end;



function TCompareFileResults.ToSTring: string;
begin
  result := booltostrex(Identical, 'Files are Identical', 'Files DIFFER!');
  if Not Identical then begin
    result := result + NEWLINE+ booltostrex(ContentsIdentical, 'Contents are Identical', 'Contents DIFFER!');
    if not ContentsIdentical then begin
      result := result +NEWLINE+ Left.ToString;
      result := result +NEWLINE+ right.toString;
      result := result + NEWLINE + commaize(different)+' different of compared '+commaize(SizeOfComparedContents);
    end;
  end;
end;

{ TCompareFileDAta }

function TCompareFileDAta.ToString: string;
begin
  result := 'File: '+filename+NEWLINE;
  result := result + 'Date  :'+datetimetostr(date)+NEWLINE;
  result := result + 'Zeros :'+IntToStr(self.Zeros)+' '+floatprecision(self.PercentZeros*100, 0)+'% of compared common parts'+NEWLINE;
  result := result + 'Size :'+IntToStr(self.Size);
end;

{ Tcmd_CompareFiles_Slow }

procedure Tcmd_CompareFiles_Slow.DoExecute;
type
  TFileCompareOps = record
{$ifdef USE_MFS}
    fs: TMemoryFileStream;
{$ELSE}
    fs: TFileStream;
{$ENDIF}
    buf: pbyte;
    justgot: ni;
  end;
var
  ops: array[0..1] of TFileCompareOps;
  cx,x: int64;
  b,bb: byte;
  tmStart, tmDif: ticker;
  z1,z2: int64;
  idb: int64;
  t: ni;
const
  BUFSZ = 262144;
begin
  inherited;
  fillmem(@result, sizeof(result), 0);
  FResult.CopyRecommendation := crNoCopy;

  if fileexists(f1) then
    Fresult.Left.DAte := FileDateToDateTimeEx(FileAge(f1));
  if fileexists(f2) then
    Fresult.Right.DAte := FileDateToDateTimeEx(FileAge(f2));
  Fresult.left.FileName := f1;
  Fresult.Right.filename := f2;

  if not fileexists(f2) then begin
    FResult.CopyRecommendation := crCopyToRight;
    exit;
  end else
  if not fileexists(f1) then begin
    FResult.CopyRecommendation := crCopyToLeft;
    exit;
  end;

  ops[0].buf := GetMemory(BUFSZ);
  ops[1].buf := GetMemory(BUFSZ);
  try

  {$IFDEF USE_MFS}
    ops[0].fs := TMemoryFileStream.create(f1, fmOpenRead+fmShareDenyNone);
    ops[1].fs := TMemoryFileStream.create(f2, fmOpenRead+fmShareDenyNone);
  {$ELSE}
    ops[0].fs := TFileStream.create(f1, fmOpenRead+fmShareDenyNone);
    ops[1].fs := TFileStream.create(f2, fmOpenRead+fmShareDenyNone);
  {$ENDIF}

    ops[0].fs.seek(0,0);
    ops[1].fs.seek(0,0);
  {$IFDEF USE_MFS}
    ops[0].fs.buffersize := (100*million);
    ops[1].fs.buffersize := (100*million);
    ops[0].fs.BufferSEgments := 40;
    ops[1].fs.BufferSEgments := 40;
  {$ENDIF}

    FResult.left.size := ops[0].fs.size;
    FResult.right.size := ops[1].fs.size;
    cx := lesserof(ops[0].fs.size, ops[1].fs.size);
    Fresult.SizeOfComparedContents := cx;
    Fresult.ContentsIdentical := true;
    StepCount := cx;
    Step := 0;
    tmStart := GEtTicker;
    z1 := 0;
    z2 := 0;
    idb := 0;
    x := 0;
  //  if cx > (100*million) then
  //    cx := 100*million;
    while cx > 0 do begin
      ops[0].justgot := stream_guaranteeread_NoExceptions(ops[0].fs, ops[0].buf, lesserof(cx, BUFSZ));
      ops[1].justgot := stream_guaranteeread_NoExceptions(ops[1].fs, ops[1].buf, lesserof(cx, BUFSZ));

      if ops[0].justgot <> ops[1].justgot then begin
        Fresult.contentsidentical := false;
        exit;
      end;

      for t:= 0 to ops[0].justgot-1 do begin
        b := ops[0].buf[t];
        bb := ops[1].buf[t];
        FResult.SizeOfComparedContents := FResult.SizeOfComparedContents+1;
        if b <> bb then begin
          if DiffToConsole then begin
             WriteLn('Dif @'+inttohex(x+t,1)+' '+inttohex(b,2)+' vs '+inttohex(bb,2)+' aligns at '+commaize(( x+t) mod 262144)+'      ');
          end;
          FResult.Different := Fresult.different + 1;
          Fresult.contentsidentical := false;
          if not CompareAll then
           break;

        end else
          inc(idb);

        if b = 0 then
          inc(z1);

        if bb = 0 then
          inc(z2);

      end;

      inc(x,ops[0].justgot);
      dec(cx, ops[0].justgot);
      Step := x;
      tmDif := GetTimesince(tmStart);
      if tmDif > 0 then begin
        Status := 'Comparing ... '+commaize(Step div tmDif)+' bytes/ms    ';
      end;

      if not CompareAll then begin
        if not FResult.ContentsIdentical then
          break;
      end;

    end;

    Fresult.IdenticalBytes := idb;
    Fresult.Left.Zeros := z1;
    Fresult.Right.Zeros := z2;
    if result.SizeOfComparedContents > 0 then begin
      Fresult.Left.PercentZeros := result.left.zeros / result.SizeOfComparedContents;
      Fresult.Right.PercentZeros := result.Right.zeros / result.SizeOfComparedContents;
    end;

    if not FResult.Identical then begin
      if CompareAll then begin

        if (ZeroCheck) and (FResult.Left.PercentZeros > FResult.Right.PercentZeros) then begin
          if FResult.Left.PercentZeros > 0.9 then
            FResult.CopyRecommendation := crCopyToLeft;
        end else begin
  //        if FResult.Right.PercentZeros > 0.9 then
            FResult.CopyRecommendation := crCopyToRight;
        end;
      end else begin
        if (FileAge(f2) > FileAge(f1)) and (not IgnoreDateChecks) then
          FResult.CopyRecommendation := crNoCopyRightIsNewer
        else
          FResult.CopyRecommendation := crCopyToRight;
      end;
    end;


    Fresult.OperationCompleted := true;
  finally
    freememory(ops[0].buf);
    freememory(ops[1].buf);
    ops[0].fs.free;
    ops[1].fs.free;
    if CompareOnly then
      FResult.CopyRecommendation := crCopyDisabled

  end;

end;

procedure Tcmd_CompareFiles_Slow.SetF1(const Value: string);
begin
  FF1 := Value;
  uniqueString(ff1);
end;

procedure Tcmd_CompareFiles_Slow.SEtF2(const Value: string);
begin
  FF2 := Value;
  uniqueString(ff2);
end;

procedure Tcmd_CompareFiles_Slow.SetResult(const Value: TCompareFileResults);
begin
  FResult := Value;
end;

{ TCmd_CompareFile_AnyInFolder }

procedure TCmd_CompareFile_AnyInFolder.DoExecute;
var
  dir: TDirectory;
  fil: TFileInformation;
  t: ni;
  cl: TCommandList<Tcmd_CompareFiles_Slow>;
  cf: Tcmd_CompareFiles_Slow;
begin
  inherited;
  dir := Tdirectory.create(dir2, '*.*', 0,0, false, false);
  cl := TCommandList<Tcmd_CompareFiles_Slow>.create;
  try
    while dir.getnextfile(fil) do begin
      cf := Tcmd_CompareFiles_Slow.create;
      cf.f1 := File1;
      cf.f2 := fil.fullname;
      cf.Start;
      cl.add(cf);
    end;

    while cl.count > 0 do begin
      cf := cl[0];
      cf.RaiseExceptions := false;
      cf.WaitFor;
      cl.delete(0);


      if cf.Result.ContentsIdentical then begin
        self.out_Matched := cf.f1;
      end;
      cf.Free;

    end;
  finally
    cl.free;
  end;


end;

end.
