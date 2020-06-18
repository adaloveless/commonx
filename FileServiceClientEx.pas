unit FileServiceClientEx;
//makes use of FileServiceClient to transfer files from remote servers

interface

uses
  debug, typex, numbers, memoryfilestream, FileServiceClient, dir, dirfile, sysutils, classes, systemx, commandprocessor, helpers_stream;

const
{$IFDEF CPUx64}
  PUT_CHUNK_SIZE = 262144*4*16;
  GET_CHUNK_SIZE = 262144*4*16;
{$ELSE}
  PUT_CHUNK_SIZE = 262144*2;
  GET_CHUNK_SIZE = 262144*2;
{$ENDIF}
  DEFAULT_HOST = '192.168.101.12';
//  DEFAULT_HOST = 'localhost';
  DEFAULT_PORT = '876';
type
  TFileServiceClientEx = class(TFileServiceClient)
  public
    connattempts: ni;
    procedure GetFileEx(sRemoteFile: string; sLocalFile: string; prog: PProgress = nil);
    procedure GetFileVerifyEx(sRemoteFile: string; sLocalFile: string; prog: PProgress = nil);
    procedure PutFileVerifyEx(sLocalFile: string; sRemoteFile: string; prog: PProgress = nil);

    procedure PutFileEx(sLocalFile: string; sRemoteFile: string; prog: PProgress = nil);overload;
    procedure PutFileEx(sLocalFile: string; sRemoteFile: string; prog: TProc<TProgress>);overload;
    procedure GetFolder(sRemoteFolder: string; sLocalFolder: string; prog: TProc<TProgress>);
    procedure PutFolder(sLocalFolder,sRemoteFolder: string; prog: TProc<TProgress>);

  end;

  Tcmd_FileEx = class(TCommand)
  public
    cli: TFileServiceClientEx;
    RemoteFile, LocalFile: string;
  end;

  Tcmd_GetFileEx = class(Tcmd_FileEx)
  public
    procedure DoExecute; override;
  end;
  Tcmd_PutFileEx = class(Tcmd_FileEx)
  public
    procedure DoExecute; override;
  end;

{ TFileServiceClientEx }
function GetFileCheckSum(sFile: string): TAdvancedfileChecksum;

implementation


function GetFileCheckSum(sFile: string): TAdvancedfileChecksum;
begin
  if not sysutils.fileexists(sFile) then begin
    result.bytesum := 0;
    result.bytexor := 0;
    result.bytecount := 0;
    exit();
  end;
  result := dir.GetFileCheckSum(sFile);
end;
procedure TFileServiceClientEx.GetFileEx(sRemoteFile, sLocalFile: string; prog: PProgress = nil);
var
  ftr: TFileTransferReference;
  iStart: int64;
  fs: TFileStream;
  iToWrite, iWritePos, iTotal: int64;
  chunksize: int64;
begin
  OpenFile_Async(sRemoteFile, fmOpenRead+fmShareDenyNone);
  GetFileSize_Async(sRemoteFile);

  ftr := nil;
  if OpenFile_Response(ftr) then //CREATES TFileTransferReference
  try
    iTotal := GetFileSize_Response();
    if prog <> nil then begin
      prog.stepcount := iTotal;
      prog.step := 0;
    end;

    iStart := 0;
    if sysutils.FileExists(sLocalFile) then
      Deletefile(sLocalFile);
    ForceDirectories(extractfilepath(sLocalFile));

    fs := TFileStream.create(sLocalFile, fmCreate);
    try

      if iTotal > 0 then begin
        chunksize := GET_CHUNK_SIZE;
        iWritePos := 0;
        repeat
          ftr.o.StartBlock := iStart;
          iToWRite := lesserof(chunksize, iTotal-iWritePos);
          ftr.o.Length := iToWrite;
          ftr.o.ContainsData := false;
          ftr.o.FreeBuffer;
          GetFile_Async(ftr);  //destroys TFileTransferReference
          GetFile_Response(ftr); //creates TFileTransferReference
          stream_GuaranteeWrite(fs, ftr.o.buffer, ftr.o.Length);
          if ftr.o.length < 0 then
            raise ECritical.create('ftr length < 0');

          if prog<> nil then
            prog.step := iWritePos;

          if ftr.o.eof then begin
{$IFDEF MSWINDOWS}
            fileSetDate(fs.Handle, DatetimeToFileDate(ftr.o.FileDate));
{$ENDIF}
          end;
          inc(iStart,ftr.o.Length);
          inc(iWritePos, ftr.o.Length);
        until iWritePos = iTotal;
        CloseFile_Async(ftr);  //destroys TfileTransferReference;
      end else begin
        chunksize := 262144*4*16;
        iWritePos := 0;
        repeat
          ftr.o.StartBlock := iStart;
          iToWRite := lesserof(chunksize, iTotal-iWritePos);
          ftr.o.Length := iToWrite;
          ftr.o.Buffer := nil;
          ftr.o.ContainsData := false;
          GetFile_Async(ftr);
          if prog<> nil then
            inc(prog.step, ftr.o.Length);
          inc(iStart,ftr.o.Length);
          inc(iWritePos, iToWrite);
        until iWritePos = iTotal;

        CloseFile_Async(ftr);


        iStart := 0;
        iWritePos := 0;
        prog.step := 0;
        repeat
          ftr.o.StartBlock := iStart;
          iToWRite := lesserof(chunksize, iTotal-iWritePos);
          ftr.o.Length := iToWrite;
          ftr.o.Buffer := nil;
          ftr.o.ContainsData := false;
  //        GetFile_Async(ftr);
          GetFile_Response(ftr);
          stream_GuaranteeWrite(fs, ftr.o.buffer, ftr.o.Length);
          if prog<> nil then
            inc(prog.step, ftr.o.Length);

          if ftr.o.eof then begin
{$IFDEF MSWINDOWS}
            fileSetDate(fs.Handle, DatetimeToFileDate(ftr.o.FileDate));
{$ELSE}
{$ENDIF}
          end;
          inc(iStart,ftr.o.Length);

          inc(iWritePos, iToWrite);
        until iWritePos = iTotal;

      end;

    finally
      fs.free;
    end;
  finally
    ftr.o.ContainsData := false;
    self.CloseFile_REsponse();
//    ftr.free;
  end
  else
    raise ECritical.create('Failed to open remote file '+sRemoteFile);



end;



procedure TFileServiceClientEx.PutFileEx(sLocalFile, sRemotefile: string; prog: PProgress = nil);
var
  ftr: TFileTransferReference;
  fs: TMemoryFileStream;
  iPOs: integer;
//  a: array [0..PUT_SIZE] of byte;
//  aa: PByte;
  b: PByte;
  iToWrite: int64;
begin
//  GetMem(aa, PUT_SIZE);
  try
    fs := TMemoryFileStream.create(sLocalFile, fmOpenRead+fmShareDenyWrite);
    try
      if prog <> nil then begin
        prog.stepcount := Dirfile.GetFileSize(sLocalFile);
        prog.step := 0;
      end;
      if self.OpenFile(sRemoteFile, ftr, fmCReate) then
      try

        iPos := 0;
        //call putfile for every block of the file
        repeat
          iToWrite := lesserof(PUT_CHUNK_SIZE, fs.Size-fs.Position);
          GEtMem(b, iToWrite);
          ftr.o.Buffer := b;
          ftr.o.StartBlock := iPos;
          ftr.o.Length := stream_guaranteeread(fs, ftr.o.Buffer, iToWrite);//fs.Read(b[0], PUT_SIZE);
          inc(iPos, ftr.o.Length);
           if prog<> nil then
            inc(prog.step, ftr.o.Length);

          ftr.o.EOF := fs.Position >= fs.Size;
          ftr.o.FileDate := FileDateToDateTime(FileGetDAte(fs.handle));
          ftr.o.ContainsData := true;
          self.PutFile(ftr);
          ftr.o.FreeBuffer;


        until ftr.o.eof;
      finally
        ftr.o.Buffer := nil;
        ftr.o.length := 0;
        self.CloseFile(ftr);
//        ftr.o.free;
      end;
    finally
      fs.free;
    end;
  finally
//    FreeMem(aa);
  end;


end;

{ Tcmd_GetFileEx }

procedure Tcmd_GetFileEx.DoExecute;
begin
  inherited;
  cli.GetFileEx(remotefile, localfile, @self.volatile_progress);
end;

{ Tcmd_PutFileEx }

procedure Tcmd_PutFileEx.DoExecute;
begin
  inherited;
  cli.GetFileEx(localfile, remotefile, @self.volatile_progress);
end;

procedure TFileServiceClientEx.GetFileVerifyEx(sRemoteFile, sLocalFile: string;
  prog: PProgress);
var
  csRemote, csLocal: TAdvancedFileChecksum;
  tries: ni;
begin
  //allow for quick exit if checksums already match
  if sysutils.fileexists(sLocalFile) then begin
    GetFileChecksum_Async(sRemoteFile);
    csLocal := fileserviceclientex.GetFileCheckSum(sLocalFile);
    csRemote := GetFileChecksum_REsponse();
    if csLocal = csRemote then begin
      exit;
    end;
  end;


  tries := 0;
  repeat
    //get the file
    GetFileEx(sRemoteFile, sLocalFile, prog);

    //exit if files are identical... else repeat
    if sysutils.fileexists(sLocalFile) then begin
      GetFileChecksum_Async(sRemoteFile);
      csLocal := fileserviceclientex.GetFileCheckSum(sLocalFile);
      csRemote := GetFileChecksum_REsponse();
      if csLocal = csRemote then begin
        exit;
      end;
    end;


    inc(tries);
  until tries >9;

  raise ECritical.create('failed to download or verify download of '+sRemoteFile);

end;

procedure TFileServiceClientEx.GetFolder(sRemoteFolder, sLocalFolder: string;
  prog: TProc<TProgress>);
begin
  Debug.Log('Get Remote folder '+sRemoteFolder+' to '+sLocalFolder);
  sLocalFolder := FixSlashes(sLocalFolder);
  forcedirectories(sLocalFolder);
  var d := Dir(sRemoteFolder);
  var fi: TFileinformation;
  while d.GetNextFile(fi) do begin
    GetFileCheckSum_Async(fi.FullName);
    var loc := sLocalFolder;
    debug.log(loc);
    loc := fixslashes(loc);
    debug.log(loc);
    loc := slash(loc);
    debug.log(loc);
    var finame := fixslashes(fi.name);

    loc := loc + finame;
    debug.log(loc);
    var csLocal := FileServiceClientEx.getfilechecksum(slash(fixslashes(sLocalFolder))+fixslashes(fi.name));
    var cs := GetFileChecksum_Response;
    if cs = csLocal then begin
      Debug.Log('Skipping '+fi.FullName);
    end else begin
      Debug.Log('Get Remote file '+fi.FullName+' to '+slash(sLocalFolder)+fixslashes(fi.name));
      GetFileEx(fi.FullName,slash(sLocalFolder)+fi.name);
    end;
  end;

  while d.getnextfolder(fi) do begin
    GetFolder(fi.FullName, slash(sLocalFolder)+fi.name, nil);
  end;

end;

procedure TFileServiceClientEx.PutFileEx(sLocalFile, sRemoteFile: string;
  prog: TProc<TProgress>);
var
  ftr: TFileTransferReference;
  fs: TMemoryFileStream;
  iPOs: integer;
//  a: array [0..PUT_SIZE] of byte;
//  aa: PByte;
  b: PByte;
  iToWrite: int64;
  pprog: TProgress;
begin
//  GetMem(aa, PUT_SIZE);
  try
    fs := TMemoryFileStream.create(sLocalFile, fmOpenRead+fmShareDenyNone);
    try
      pprog.stepcount := Dirfile.GetFileSize(sLocalFile);
      pprog.step := 0;
      if self.OpenFile(sRemoteFile, ftr, fmCreate) then
      try

        iPos := 0;
        //call putfile for every block of the file
        repeat
          iToWrite := lesserof(PUT_CHUNK_SIZE, fs.Size-fs.Position);
          GEtMem(b, iToWrite);
          ftr.o.Buffer := b;
          ftr.o.StartBlock := iPos;
          ftr.o.Length := stream_guaranteeread(fs, ftr.o.Buffer, iToWrite);//fs.Read(b[0], PUT_SIZE);
          inc(iPos, ftr.o.Length);
          inc(pprog.step, ftr.o.Length);
          prog(pprog);

          ftr.o.EOF := fs.Position >= fs.Size;
          ftr.o.FileDate := FileDateToDateTime(FileGetDAte(fs.handle));
          ftr.o.ContainsData := true;
          self.PutFile(ftr);
          ftr.o.FreeBuffer;


        until ftr.o.eof;
      finally
        ftr.o.Buffer := nil;
        ftr.o.length := 0;
        self.CloseFile(ftr);
//        ftr.o.free;
      end;
    finally
      fs.free;
    end;
  finally
//    FreeMem(aa);
  end;


end;

procedure TFileServiceClientEx.PutFileVerifyEx(sLocalFile, sRemoteFile: string;
  prog: PProgress);
var
  csRemote, csLocal: TAdvancedFileChecksum;
  tries: ni;
begin
  //allow for quick exit if checksums already match
  if sysutils.fileexists(sLocalFile) then begin
    GetFileChecksum_Async(sRemoteFile);
    csLocal := fileserviceclientex.GetFileCheckSum(sLocalFile);
    csRemote := GetFileChecksum_REsponse();
    if csLocal = csRemote then begin
      exit;
    end;
  end;


  tries := 0;
  repeat
    //get the file
    PutFileEx(sLocalFile, sRemoteFile, prog);

    //exit if files are identical... else repeat
    if sysutils.fileexists(sLocalFile) then begin
      GetFileChecksum_Async(sRemoteFile);
      csLocal := fileserviceclientex.GetFileCheckSum(sLocalFile);
      csRemote := GetFileChecksum_REsponse();
      if csLocal = csRemote then begin
        exit;
      end;
    end;


    inc(tries);
  until tries >9;

  raise ECritical.create('failed to push or verify upload of '+sRemoteFile);

end;

procedure TFileServiceClientEx.PutFolder(sLocalFolder,sRemoteFolder: string;
  prog: TProc<TProgress>);
begin
  Debug.Log('Put local folder '+sLocalFolder+' to '+sRemoteFolder);
  sLocalFolder := FixSlashes(sLocalFolder);
  forcedirectories(sLocalFolder);
  var d := TDirectory.create(sLocalFolder, ALL_FILES,0,0);
  var fi: TFileinformation;
  while d.GetNextFile(fi) do begin
    if fi.name = '.DS_Store' then continue;
    var remfile := winslash(slash(sRemoteFolder))+fi.Name;
    GetFileCheckSum_Async(remfile);
    var csLocal := FileServiceClientEx.getfilechecksum(fi.fullname);
    var cs :TAdvancedFileChecksum;
    try
      cs := GetFileChecksum_Response;
    except
      cs.init;
    end;
    if cs = csLocal then begin
      Debug.Log('Skipping '+fi.FullName);
    end else begin
      Debug.Log('Put file '+fi.FullName+' to '+remfile);
      PutFileEx(fi.FullName,remfile);
    end;
  end;

  while d.getnextfolder(fi) do begin
    var nextfolder := winslash(slash(sRemoteFolder))+fi.name;
    PutFolder(fi.FullName, nextfolder, nil);
  end;

end;

end.
