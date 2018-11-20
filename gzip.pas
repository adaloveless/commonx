unit gzip;

interface



uses
  tools, systemx, typex, exe, stringx, sysutils, memoryfilestream, classes, commandprocessor;

procedure Zip(sFile: string);
procedure unZip(sfile: string);
function IsGzip(sFile: string): boolean;

type
  Tcmd_GZip = class(TCommand)
  private
    FFileName: string;
  public
    property FileName: string read FFileName write FFIleName;
    procedure DoExecute;
  end;

  Tcmd_GUnZip = class(TCommand)
  private
    FFileName: string;
  public
    property FileName: string read FFileName write FFIleName;
    procedure DoExecute;
  end;



implementation

procedure Zip(sFile: string);
var
  c: Tcmd_RunExe;
begin
  RunProgramAndWait(findtool('gzip.exe'), sFile, extractfilepath(sFile), true, false);

  {
  c := Tcmd_RunExe.create;
  try

    c.Prog := dllpath+'gzip.exe';
    c.Params := quote(sFile);
    c.WorkingDir := extractfilepath(sFile);
    c.Hide := true;
    c.CPUExpense := 1.0;
//    c.Resources.SetResourceUsage(ExtractNetworkRoot(sFile), 1.0);
    c.Start;
    c.WaitFor;
  finally
    c.Free;
  end;}

end;

procedure unZip(sfile: string);
var
  c: Tcmd_RunExe;
begin
  RunProgramAndWait(findtool('gunzip.exe'), sFile, extractfilepath(sFile), true, false);
{  c := Tcmd_RunExe.create;
  try
    c.Prog := dllpath+'gunzip.exe';
    c.Params := quote(sFile);
    c.WorkingDir := extractfilepath(sFile);
    c.Hide := true;
    c.Start;
    c.WaitFor;
  finally
    c.Free;
  end;}

end;

function IsGzip(sFile: string): boolean;
var
  mfs: TMemoryFileStream;
  b1,b2: byte;
begin
  mfs := TMemoryFileStream.create(sFile, fmopenRead+fmShareDenynone);
  mfs.buffersize := 20;
  try
    mfs.Seek(0, sobeginning);

    b1 := 0;
    b2 := 0;
    mfs.Read(b1,1);
    mfs.Read(b2,1);

    result := (b1 = $1f) and (b2 = $8b);

  finally
    mfs.free;
  end;

end;



{ Tcmd_GUnZip }

procedure Tcmd_GUnZip.DoExecute;
begin
  RunProgramAndWait(findtool('gzip.exe'), FileName, extractfilepath(filename), true, false);
end;

{ Tcmd_GZip }

procedure Tcmd_GZip.DoExecute;
begin
  RunProgramAndWait(findtool('gunzip.exe'), FileName, extractfilepath(filename), true, false);

end;

end.
