unit FileServiceServerImpLib;

{GEN}
{TYPE IMPLIB}
{RQFILE FileServiceRQs.txt}
{END}


interface
uses
  windows, memoryfilestream, FileTransfer, FileServiceServer,
    sysutils, rdtpprocessor, dir, dirfile, classes, systemx,
    betterobject, spam, stringx, exe, rdtpserverlist,orderlyinit,
    helpers.stream, consolelock, commandprocessor, rdtp_file,
    SoundConversion_Windows, SoundConversions_CommandLine;

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

{INTERFACE_END}
  end;

function FindFileResource(proc: TRDTPProcessor; iHandle: THandle): TMemoryFileStream;

var
  CPWerk: TcommandProcessor;

implementation

uses
  soundtools, servervideoparser;

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
    end;
  finally
//    FreeMem(aa);
  end;


end;


function TFileServiceServer.RQ_GetFile(var oFile:TFileTransferReference):boolean;
var
  fs: TMemoryFileStream;
  a: array [0..512000] of byte;
  iRead: int64;
  b: pointer;
begin

  result := true;
  fs := FindFileResource(self, oFile.o.handle);
  if fs = nil then begin
    raise Exception.create('The file handle was invalid reading file: '+oFile.o.FileName);
  end;
  try
    fs.Seek(oFile.o.StartBlock,0);
    iRead := fs.Read(a, oFile.o.Length);
    GetMem(b, ofile.o.Length);
    MoveMem32(b, @a[0], iRead);
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
  r := TMemoryFileStream.create(sFile, iMode);

  oFile := THolder<TFileTransferREferenceObj>.create;
  oFile.o := TFileTransferREferenceObj.create;
  self.Resources.Add(r);
  oFile.o.FileName := sFile;
  oFile.o.Handle := r.handle;
  result := true;
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
    fullname := dllpath+filename;
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
  Deletefile(sfile);
  result := Not FileExists(sFile);
end;

function TFileServiceServer.RQ_Dir(sRemotePath:string):TDirectory;
var
  dir: TDirectory;
begin
  result := Tdirectory.create(sRemotePath, '*.*', 0,0, false, true, false);
//  raise Exception.create('unimplemented');

//TODO -cunimplemented: unimplemented block
end;


function TFileServiceServer.RQ_EchoStream(strin: TStream): TStream;
begin
  result := TMemoryStream.Create;
  result.CopyFrom(strin, strin.Size);
end;

function TFileServiceServer.RQ_Execute(sPath, sProgram,
  sParams: string): boolean;
var
  c: Tcmd_RunExe;
begin
  c := Tcmd_Runexe.create;
  c.Prog := slash(sPath)+sProgram;
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

function TFileServiceServer.RQ_GetUpgradePath(sProgramName:string):string;
begin
  result := DLLPath+'UpgradeRepos\';
end;
function TFileServiceServer.RQ_GetUpgradeScript(sProgramName:string; iFromVersion:integer; iToVersion:integer):string;
var
  s1,s2: string;
  t: integer;
begin
  s2 := LoadStringFromFile(DLLPath+'UpgradeRepos\'+sProgramName+'\upgrade_script.txt');

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
  result.Calculate(sFile);
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
  result := dirfile.GetFileSize(filename);
end;


procedure oinit;
begin
  CPWerk := TCommandProcessor.create(nil, 'CPWerk');

end;

procedure ofinal;
begin
  CPWerk.Detach;
  CPWerk.free;
  CPWerk := nil;
end;
initialization

orderlyinit.init.RegisterProcs('FileServiceServerImplib', oinit, ofinal, 'CommandProcessor');

RDTPServers.RegisterRDTPProcessor('FileService', TFileServiceServer);


end.



