unit FileServiceServerImpLib;

{GEN}
{TYPE IMPLIB}
{RQFILE FileServiceRQs.txt}
{END}


interface
uses
  windows, memoryfilestream, FileTransfer, FileServiceServer, nvidiatools,
    sysutils, rdtpprocessor, dir, dirfile, classes, systemx, debug,
    betterobject, spam, stringx, exe, rdtpserverlist,orderlyinit,
    helpers_stream, consolelock, commandprocessor, rdtp_file, sharefinder,
    SoundConversion_Windows, SoundConversions_CommandLine, numbers;

  type
  TFileServiceServer = class(TFileServiceServerBase)
  private
  protected
  public
  {INTERFACE_START}
    function RQ_PutFile(oFile:TFileTransferReference):boolean;overload;override;
    function RQ_GetFile(var oFile:TFileTransferReference):boolean;overload;override;
    function RQ_OpenFile(sFile:string; out oFile:TFileTransferReference; iMode:integer):boolean;overload;override;
    function RQ_CloseFile(oFile:TFileTransferReference):boolean;overload;override;
    function RQ_Dir(sRemotePath:string):TDirectory;overload;override;
    function RQ_GetUpgradePath(sProgramName:string):string;overload;override;
    function RQ_GetUpgradeScript(sProgramName:string; iFromVersion:integer; iToVersion:integer):string;overload;override;
    function RQ_GetUpgradeVersion(sProgramName:string; bBeta:boolean):integer;overload;override;
    function RQ_GetFileChecksum(sFile:string):TAdvancedFileChecksum;overload;override;
    function RQ_BuildHueFile(sFile:string; LengthInSeconds:real):boolean;overload;override;
    function RQ_DeleteFile(sFile:string):boolean;overload;override;
    function RQ_Execute(sPath:string; sProgram:string; sParams:string):boolean;overload;override;
    function RQ_BuildHueFileFromStream(str:TStream; sExt:string; LengthInSeconds:real):TStream;overload;override;
    function RQ_EchoStream(strin:TStream):TStream;overload;override;
    function RQ_AppendTextFile(filename:string; text:string):boolean;overload;override;
    function RQ_GetFileSize(filename:string):int64;overload;override;
    function RQ_ExecuteAndCapture(sPath:string; sProgram:string; sParams:string):string;overload;override;
    function RQ_GetFileList(sRemotePath:string; sFileSpec:string; attrmask:integer; attrresult:integer):TRemoteFileArray;overload;override;
    function RQ_StartExeCommand(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single):int64;overload;override;
    function RQ_GetCommandStatus(handle:int64; out status:string; out step:int64; out stepcount:int64; out finished:boolean):boolean;overload;override;
    function RQ_EndCommand(handle:int64):boolean;overload;override;
    function RQ_GetCPUCount():int64;overload;override;
    function RQ_GetCommandResourceConsumption(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single):boolean;overload;override;
    function RQ_EndExeCommand(handle:int64):string;overload;override;
    function RQ_GetEXECommandStatus(handle:int64; out status:string; out finished:boolean; out consoleCapture:string):boolean;overload;override;
    function RQ_StartExeCommandEx(sPath:string; sProgram:string; sParams:string; cpus:single; memgb:single; ext_resources:string):int64;overload;override;
    function RQ_GetCommandResourceConsumptionEx(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single; out gpusUsed:single; out gpuMax:single; out ext_resources:string):boolean;overload;override;
    function RQ_GetGPUList():string;overload;override;
    function RQ_GetGPUCount():integer;overload;override;
    function RQ_StartExeCommandExFFMPEG(sPath:string; sProgram:string; sParams:string; sGPUParams:string; cpus:single; memgb:single; gpu:single):int64;overload;override;
    function RQ_FileExists(sFile:string):boolean;overload;override;
    function RQ_PathExists(sPath:string):boolean;overload;override;

{INTERFACE_END}
  end;

function GetBestGPU: integer;
function FindSharedExeCommandResource(handle:int64): Tcmd_RunExe;
function FindFileResource(proc: TRDTPProcessor; iHandle: THandle): TMemoryFileStream;
function FindCommandResource(proc: TRDTPProcessor; handle: int64): TCommand;
function FindExeCommandResource(proc: TRDTPProcessor; handle: int64): Tcmd_RunExe;
function ResolveSharePath(sFile: string): string;

var
  CPWerk: TcommandProcessor;

implementation

uses
  soundtools, servervideoparser;
function GetGPUnUsage(gpu: integer): single;
begin
  result := 0.0;
  var l := BGCmd.Locki;
  for var t:= 0 to BGCmd.commandcount-1 do begin
    var c := BGCmd.Commands[t];
    if not c.iscomplete then begin
      result := result + c.Resources.GetResourceUsage('GPU'+gpu.ToString);
    end;
  end;
end;

function GetBestGPU: integer;
var
  t: integer;
  o: TObject;
  gpusUsed: single;
begin
  var l := BGCmd.Locki;
  var bestuse: single := 999999;
  var bestT: integer := -1;
  for t:= 0 to 15 do begin
    var thisuse := GetGPUnUsage(t);
    if thisuse < bestuse then  begin
      bestuse := thisuse;
      bestT := t;
      if bestuse = 0.0 then
        exit(t);//might as well exit because nothing will be less than 0.0
    end;
  end;
  result := bestT;
end;


function FindFileResource(proc: TRDTPProcessor; iHandle: THandle): TMemoryFileStream;
var
  t: integer;
  o: TObject;
begin
  result := nil;
  for t:= 0 to proc.Resources.count-1 do begin
    o := proc.resources[t];
    if o is TMemoryFileStream then with o as TMemoryFileStream do begin
      if handle = iHandle then begin
        result := o as TMemoryFileStream;
        break;
      end;
    end;
  end;

end;

function FindCommandResource(proc: TRDTPProcessor; handle: int64): TCommand;
var
  t: integer;
  o: TObject;
begin
  result := nil;
  for t:= 0 to proc.Resources.count-1 do begin
    o := proc.resources[t];
    if o is Tcommand then with o as Tcommand do begin
      if handle = int64(pointer(o)) then begin
        result := o as Tcommand;
        break;
      end;
    end;
  end;
end;

function FindExeCommandResource(proc: TRDTPProcessor; handle: int64): Tcmd_RunExe;
var
  t: integer;
  o: TObject;
begin
  result := nil;
  for t:= 0 to proc.Resources.count-1 do begin
    o := proc.resources[t];
    if o is Tcommand then with o as Tcmd_RunExe do begin
      if handle = int64(pointer(o)) then begin
        result := o as Tcmd_RunExe;
        break;
      end;
    end;
  end;
end;

function FindSharedExeCommandResource(handle:int64): Tcmd_RunExe;
begin
  result := nil;
  var l := BGCmd.LockI;
  for var t:= 0 to BGCmd.CommandCount-1 do begin
    var c := BGCmd.Commands[t];
    if int64(pointer(c))=handle then begin
      if c is Tcmd_RunExe then
        exit(Tcmd_RunExe(c));
    end;
  end;
end;


function TFileServiceServer.RQ_PathExists(sPath: string): boolean;
begin
  result := DirectoryExists(resolvesharepath(sPath));
end;

function TFileServiceServer.RQ_putFile(ofile: TFileTransferReference): boolean;
var
  fs: TMemoryFileStream;
  //a: array [0..512000] of byte;
//  aa: PByte;
  iWritten, iJustWritten: int64;
  b: PByte;
begin
  result := true;
//  GetMem(aa, 2000000);
  try

    fs := FindFileResource(self, oFile.o.handle);
    if fs = nil then begin
      raise Exception.create('The file handle was invalid reading file: '+oFile.o.FileName);
    end;
    try
      fs.Seek(oFile.o.StartBlock,0);
  //    GetMem(b, ofile.Length);
      b := ofile.o.Buffer;
//      MoveMem32(@aa[0],b, oFile.Length);
      iWritten := 0;
      repeat
        iJustWritten := fs.Write(b[iWritten], oFile.o.Length-iWritten);
        if iJustWritten < 1 then break;
        inc(iWritten, iJustWritten);
      until iWritten = oFile.o.length;
      if oFile.o.EOF then begin
        FileSetDAte(fs.handle,DateTimeToFileDate(oFile.o.FileDate));
      end;

    finally
  //    fs.free;
      FreeMem(oFile.o.Buffer);
      oFile.o.Buffer := nil;
    end;
  finally
//    FreeMem(aa);
  end;


end;



function TFileServiceServer.RQ_GetCommandResourceConsumption(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single):boolean;
begin
  var ext_resources: string := '';
  var gpu: single;
  var gpumax: single;
  result := RQ_GetCommandResourceConsumptionEx(cpusUsed, cpuMax,memGBUsed, memGBMax, gpu,gpumax, ext_resources);

end;

function TFileServiceServer.RQ_GetCommandResourceConsumptionEx(out cpusUsed:single; out cpuMax:single; out memGBUsed:single; out memGBMax:single; out gpusUsed:single; out gpuMax:single; out ext_resources:string):boolean;
var
  t: integer;
  o: TObject;
begin
  //note note note.... this will bite me in the ass later!
  //this returns SCHEDULED resource consumption, not ACTIVE consumption
  //and is used for the purpose of balancing scheduling over multiple
  //computers, inactive commands will be considered as consuming resources
  //however completed commands will not
  cpusUsed := 0;
  memgbUsed := 0;
  result := true;
  var l := BGCmd.Locki;
  for t:= 0 to BGCmd.commandcount-1 do begin
    var c := BGCmd.Commands[t];
    if not c.iscomplete then begin
      cpusUsed := cpusUsed + c.CPUExpense;
      memgbused := memgbUsed + c.MemoryExpenseGB;
      GPUsUsed := GPUsUsed + c.Resources.GetResourceUsage('GPUExpense');
    end;
  end;

  var slh := StringToStringlisth('');
  slh.o.add('GPUExpense='+floatprecision(GPUsUsed,4));
  ext_resources := slh.o.text;


  cpuMax := GetEnabledCPUCount;
  memGBMax := GetPhysicalMemory / (1000000000.0);
  gpumax := BGCMd.GetResourceLimit('GPUExpense');


end;

function TFileServiceServer.RQ_GetCommandStatus(handle: int64;
  out status: string; out step, stepcount: int64; out finished: boolean): boolean;
begin
  var c := FindCommandResource(self, handle);
  result := c <> nil;
  if result then begin
    status := c.status;
    step := c.step;
    stepcount := c.stepcount;
    finished := c.iscomplete;
  end;
end;

function TFileServiceServer.RQ_GetCPUCount: int64;
begin
  result := systemx.GetEnabledCPUCount;
end;

function TFileServiceServer.RQ_GetEXECommandStatus(handle: int64;
  out status: string; out finished: boolean;
  out consoleCapture: string): boolean;
begin
  var c := FindSharedExeCommandResource(handle);
  result := c <> nil;
  if result then begin
    status := c.status;
    finished := c.iscomplete;
    consoleCapture := c.DrainConsole;
  end;

end;

function TFileServiceServer.RQ_StartExeCommand(sPath, sProgram, sParams: string;
  cpus, memgb: single): int64;
begin
  result := RQ_StartExeCommandEx(resolvesharepath(sPath), sProgram, sParams, cpus, memgb, '');

end;

function TFileServiceServer.RQ_StartExeCommandEx(sPath, sProgram,
  sParams: string; cpus, memgb: single; ext_resources: string): int64;
var
  c: Tcmd_RunExe;
begin
  sPath := resolvesharepath(sPath);
  c := Tcmd_Runexe.create;
  c.Prog := slash(sPath)+sProgram;
  c.prog := stringreplace(c.prog, '%dllpath%', dllpath, [rfReplaceAll, rfIgnoreCase]);
  c.Params := sParams;
  c.WorkingDir := extractfilepath(c.Prog);
  c.Hide := true;
  c.CPUExpense := cpus;
  c.MemoryExpenseGB := memgb;
  c.CaptureConsoleoutput := true;
  var slh := stringToStringListH(ext_resources);
  for var t:= 0 to slh.o.count-1 do begin
    c.Resources.SetResourceUsage(slh.o.KeyNames[t], strtofloat(slh.o.ValueFromIndex[t]));
  end;
  var gu :=c.Resources.GetResourceUsage('GPUExpense');
  if gu > 0.0 then begin
    var g := GetBestGPU;
    c.resources.setresourceUsage('GPU'+g.tostring,gu);
  end;
  c.Start;
//  self.Resources.Add(c);
  result := int64(pointer(c));

end;

function TFileServiceServer.RQ_StartExeCommandExFFMPEG(sPath, sProgram,
  sParams: string; sGPUParams:string; cpus, memgb: single; gpu: single): int64;
var
  c: Tcmd_RunExe;
begin
  sPath := resolvesharepath(sPath);
  c := Tcmd_Runexe.create;
  c.Prog := slash(sPath)+sProgram;
  c.prog := stringreplace(c.prog, '%dllpath%', dllpath, [rfReplaceAll, rfIgnoreCase]);
  c.Params := sParams;
  c.WorkingDir := extractfilepath(c.Prog);
  c.Hide := true;
  c.CPUExpense := cpus;
  c.MemoryExpenseGB := memgb;
  c.CaptureConsoleoutput := true;
//  var slh := stringToStringListH(ext_resources);
//  for var t:= 0 to slh.o.count-1 do begin
//    c.Resources.SetResourceUsage(slh.o.KeyNames[t], strtofloat(slh.o.ValueFromIndex[t]));
//  end;
  if sGPUParams <> '' then begin
    var gu :=gpu;
    if gu > 0.0 then begin
      var g := GetBestGPU;
      if g >=0 then begin
        c.Resources.SetResourceUsage('GPUExpense', gpu);
        c.resources.setresourceUsage('GPU'+g.tostring,gu);
        c.Params := stringreplace(sGPUParams,'##gpu##', g.tostring, [rfReplaceAll,rfIgnoreCase]);
  //      c.Params := '-hwaccel_device '+g.ToString+' '+c.Params;
        c.CPUExpense := 1;
      end;
    end;
  end;
  c.Start;
//  self.Resources.Add(c);
  result := int64(pointer(c));

end;

function TFileServiceServer.RQ_GetFile(var oFile:TFileTransferReference):boolean;
var
  fs: TMemoryFileStream;
//  a: array [0..512000] of byte;
  iRead: int64;
  b: pointer;
begin

  result := true;
  fs := FindFileResource(self, oFile.o.handle);
  if fs = nil then begin
    raise Exception.create('The file handle was invalid reading file: '+oFile.o.FileName);
  end;
  try
    fs.BufferSize := lesserof(2000000, greaterof(ofile.o.length, fs.BufferSegmentSize))*fs.BufferSEgments;
    fs.BufferSEgments := 3;
    fs.Seek(oFile.o.StartBlock,0);
    GetMem(b, ofile.o.Length);
    iRead := fs.Read(pbyte(b)^, oFile.o.Length);

//    MoveMem32(b, @a[0], iRead);
    //oFile.Init();
    oFile.o.eof := fs.Position >= fs.Size;

//(iRead < oFile.Length);
    oFile.o.length := iRead;
    oFile.o.containsData := true;
    oFile.o.buffer := b;
    if oFile.o.eof then begin
      oFile.o.FileDate := FileDateToDateTime(FileGetDate(fs.handle));
    end;




  finally
//    fs.free;
  end;

end;

//------------------------------------------------------------------------------
function TFileServiceServer.RQ_OpenFile(sFile:string; out oFile:TFileTransferREference; iMode:integer):boolean;
var
  r: TMemoryFileStream;
begin
  Debug.Log('open file transfer '+sFile);
  if iMode = fmCreate then
    ForceDirectories(extractfilepath(resolvesharepath(sFile)));

  try
    r := TMemoryFileStream.create(resolvesharepath(sFile), iMode);

    oFile := THolder<TFileTransferREferenceObj>.create;
    oFile.o := TFileTransferREferenceObj.create;
    self.Resources.Add(r);
    oFile.o.FileName := sFile;
    oFile.o.Handle := r.handle;
    result := true;
  except
    on E: Exception do begin
      if not fileexists(resolvesharepath(sFile)) then begin
        raise EFileNotFoundException.create('File not found: '+resolvesharepath(sFile));
      end else
        raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TFileServiceServer.RQ_AppendTextFile(filename, text: string): boolean;
var
  fs: TFileStream;
  bytes: TBytes;
  fullname: string;
begin
  LockConsole;
  try
    fullname := resolvesharepath(filename);
    if not fileexists(fullname) then begin
      ForceDirectories(extractfilepath(fullname));
      SaveStringAsFile(fullname, text);
    end else begin

      fs := TFileStream.create(fullname, fmOpenWrite+fmShareExclusive);
      try
        fs.seek(0, soEnd);
        bytes := TEncoding.UTF8.GetBytes(text);
        stream_guaranteewrite(fs,@bytes[0], length(bytes));
      finally
        fs.free;
      end;
    end;
  finally
    UnlockConsole;
  end;
end;

function TFileServiceServer.RQ_BuildHueFile(sFile: string; LengthInSeconds: real): boolean;
var
  c: Tcmd_RenderHuestogram;
  sExt, sTemp, sTemp1: string;
  shue: string;
begin
  result := false;

  sTemp := sFile;

{$IFDEF BUILD_SAFE}
  sTemp1 := changefileext(sTemp, '.mpeg');
{$ELSE}
  sTemp1 := sTemp;
{$ENDIF}
  sHue :=changefileext(sTemp, '.hue');

  sTemp1 := resolvesharepath(sTemp1);
  sTemp := resolvesharepath(sTemp);
{$IFDEF BUILD_SAFE}
  if fileexists(sTemp1) then
    deletefile(sTemp1);
{$ENDIF}

{$IFDEF BUILD_SAFE}
  EndVideoToSafeVideo(soundtools.BeginVideoToSafeVideo(sTemp, sTemp1));
{$ENDIF}

  c := Tcmd_RenderHuestogram.create;
  try
    c.FileName := sTemp1;
    c.LengthinSeconds := LengthInSeconds;
    c.Start;
    c.WaitFor;
    result := true;
  finally
    c.Free;
  end;


//  if fileexists(changefileext(sTemp, '.hue')) then
//    deletefile(changefileext(sTemp, '.hue'));
//
//  copyfile(sTemp1, changefileext(sTemp, '.hue'));

  if fileexists(sTemp) then
    deletefile(sTemp);

  if fileexists(sTemp1) then
    deletefile(sTemp1);

end;

function TFileServiceServer.RQ_BuildHueFileFromStream(str: TStream;
  sExt: string; LengthInSeconds: real): TStream;
var
  sTemp, sTemp1: string;
  sfs: TfileStream;
  c: Tcmd_RenderHuestogram;
begin
  sTemp := systemx.GetTempPath+inttostr(GetCurrentThreadId)+sExt;
  sTemp1 := systemx.GetTempPath+inttostr(GetCurrentThreadId);
  if fileexists(sTemp) then
    deletefile(sTemp);

  if fileexists(sTemp1) then
    deletefile(sTemp1);


  sfs := TFileStream.create(sTemp, fmCreate);
  try
    sfs.CopyFrom(str, str.Size);
  finally
    sfs.Free;
  end;

  EndVideoToSafeVideo(BeginVideoToSafeVideo(sTemp, sTemp1));

  c := Tcmd_RenderHuestogram.create;
  try
    c.FileName := sTemp1;
    c.Width := 720 div 4;
    c.height := 480 div 4;
    c.Start;
    c.WaitFor;
  finally
    c.Free;
  end;

  result := TFileStream.create(sTemp1, fmOpenRead+fmShareDenyNone);



end;

function TFileServiceServer.RQ_CloseFile(oFile:TFileTransferReference):boolean;
var
  o: TObject;
  t: integer;
begin
  for t:= 0 to self.resources.count-1 do begin
    o := TObject(self.resources[t]);
    if o is TMemoryFileStream then with o as TMemoryFileStream do begin
      if handle = oFile.o.handle then begin
        self.resources.remove(o);
        Free;
      end;
    end;

  end;
  result := true;
end;



function TFileServiceServer.RQ_DeleteFile(sFile: string): boolean;
begin
  Deletefile(resolvesharepath(sfile));
  result := Not FileExists(resolvesharepath(sFile));
end;

function TFileServiceServer.RQ_Dir(sRemotePath:string):TDirectory;
var
  dir: TDirectory;
begin
  var s := 'Dir of: '+sRemotePath+' resolves to '+resolvesharepath(sRemotePath)+' ';


  result := Tdirectory.create(resolvesharepath(sRemotePath), ALL_FILES, 0,0, false, true, false);
  s := s + 'and has '+result.Filecount.tostring+' files';
//  SaveStringAsFile(dllpath+'dir.txt',s);
  Debug.Log(s);

//  raise Exception.create('unimplemented');

//TODO -cunimplemented: unimplemented block
end;


function TFileServiceServer.RQ_EchoStream(strin: TStream): TStream;
begin
  result := TMemoryStream.Create;
  result.CopyFrom(strin, strin.Size);
end;

function TFileServiceServer.RQ_EndCommand(handle: int64): boolean;
begin
  var c := FindCommandResource(self, handle);
  Resources.Remove(c);
  c.RaiseExceptions := false;
  c.WaitFor;
  c.Free;
end;

function TFileServiceServer.RQ_EndExeCommand(handle: int64): string;
begin
  var c:= FindSharedExeCommandResource(handle);
  result := c.consoleoutput;
  self.Resources.Remove(c);
  c.waitfor;
  c.free;

end;

function TFileServiceServer.RQ_Execute(sPath, sProgram,
  sParams: string): boolean;
var
  c: Tcmd_RunExe;
begin
  c := Tcmd_Runexe.create;
  c.Prog := slash(resolvesharepath(sPath))+sProgram;
  c.prog := stringreplace(c.prog, '%dllpath%', dllpath, [rfReplaceAll, rfIgnoreCase]);
  c.Params := sParams;
  c.WorkingDir := extractfilepath(c.Prog);
  c.Hide := true;
  c.Start;
  c.WaitFor;
  c.Free;
  result := true;
end;

function TFileServiceServer.RQ_ExecuteAndCapture(sPath, sProgram,
  sParams: string): string;
var
  c: Tcmd_RunExe;
  sTemp: string;
begin
  sPath := resolvesharepath(sPath);
  c := Tcmd_Runexe.create;
  c.Prog := slash(sPath)+sProgram;
  sTemp := gettemppath+inttostr(getcurrentthreadid)+'.output.txt';
  c.Params := sParams+'>'+sTemp;
  c.batchwrap := true;
  c.WorkingDir := extractfilepath(c.Prog);
  c.Hide := true;
  c.Start;
  c.WaitFor;
  c.Free;
  result := loadfileasstring(sTemp);
end;

function TFileServiceServer.RQ_FileExists(sFile: string): boolean;
begin
  result := FileExists(resolvesharepath(sFile));
end;

function TFileServiceServer.RQ_GetUpgradePath(sProgramName:string):string;
begin
  result := resolvesharepath('UpgradeRepos\');
end;
function TFileServiceServer.RQ_GetUpgradeScript(sProgramName:string; iFromVersion:integer; iToVersion:integer):string;
var
  s1,s2: string;
  t: integer;
begin
  s2 := LoadStringFromFile(resolvesharepath('UpgradeRepos\'+sProgramName+'\upgrade_script.txt'));

  for t := 0 to iFromVersion+1 do begin
    //if the version is not declared in the file, then use the rest of the file
    if not SplitString(s2, '>>>'+inttostr(t)+'<<<', s1,s2) then
      s2 := s1;
  end;

  SplitString(s2, '>>>'+inttostr(iToVersion+1)+'<<<', s1,s2);
  result := s1;





end;

function TFileServiceServer.RQ_GetUpgradeVersion(sProgramName:string; bBeta:boolean):integer;
begin
 if bBeta then begin
    result := strtoint(Trim(LoadStringFromFile(DLLPath+'UpgradeRepos\'+sProgramName+'\beta_version.txt')));
  end else begin
    result := strtoint(Trim(LoadStringFromFile(DLLPath+'UpgradeRepos\'+sProgramName+'\release_version.txt')));
  end;
end;



function TFileServiceServer.RQ_GetFileChecksum(sFile:string):TAdvancedFileChecksum;
begin
  Debug.Log('Calculate checksum for '+resolvesharepath(sFile));
  result.Calculate(resolvesharepath(sFile));
end;


function TFileServiceServer.RQ_GetFileList(sRemotePath: string; sfileSpec: string; attrmask, attrresult: integer): TRemoteFileArray;
var
  dir: TDirectory;
  fil: TFileInformation;
  t: integer;
begin
  dir := nil;
  try
    dir := TDirectory.create(sRemotePath, sFileSpec, attrmask, attrresult, false, false, false);
    setlength(result, dir.Filecount);
    for t:= 0 to dir.filecount-1 do begin
      fil := dir.files[t];
      result[t].name := fil.name;
      result[t].path := fil.Path;
      result[t].date := fil.Date;
      result[t].attributes := 0;
      result[t].size := fil.Size;
    end;
  finally
    dir.free;
  end;

end;

function TFileServiceServer.RQ_GetFileSize(filename: string): int64;
begin
  result := dirfile.GetFileSize(resolvesharepath(filename));
end;


function TFileServiceServer.RQ_GetGPUCount: integer;
begin
    result := nvidiatools.GetGPUCount;
end;

function TFileServiceServer.RQ_GetGPUList: string;
begin
  result := nvidiatools.GetGPUList.o.Text;

end;

function ResolveSharePath(sFile: string): string;
begin
  if zcopy(sFile, 1,1) = ':' then
    exit(sfile);

  if zcopy(sFile,0,2) = '\\' then
    exit(sFile);

  result := sharefinder.GetFileServiceSharePath+sFile;


end;

procedure oinit;
begin
  CPWerk := TCommandProcessor.create(nil, 'CPWerk');
  RDTPServers.RegisterRDTPProcessor('FileService', TFileServiceServer);
end;

procedure ofinal;
begin
  CPWerk.Detach;
  CPWerk.free;
  CPWerk := nil;
end;
initialization

orderlyinit.init.RegisterProcs('FileServiceServerImplib', oinit, ofinal, 'CommandProcessor,RDTPServerList');




end.



