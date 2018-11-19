unit FileServiceClientEx;
//makes use of FileServiceClient to transfer files from remote servers

interface

uses
  typex, numbers, memoryfilestream, FileServiceClient, dir, dirfile, sysutils, classes, systemx, commandprocessor, helpers.stream;

const
  DEFAULT_HOST = '192.168.101.12';
//  DEFAULT_HOST = 'localhost';
  DEFAULT_PORT = '876';
type
  TFileServiceClientEx = class(TFileServiceClient)
  public
    procedure GetFileEx(sRemoteFile: string; sLocalFile: string; prog: PProgress = nil);
    procedure PutFileEx(sLocalFile: string; sRemoteFile: string; prog: PProgress = nil);
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


implementation

procedure TFileServiceClientEx.GetFileEx(sRemoteFile, sLocalFile: string; prog: PProgress = nil);
var
  ftr: TFileTransferReference;
  iStart: int64;
  fs: TFileStream;
  iToWrite, iWritePos, iTotal: int64;
  t: ni;
begin
  OpenFile_Async(sRemoteFile, fmOpenRead+fmShareDenyNone);
  GetFileSize_Async(sRemoteFile);

  if OpenFile_Response(ftr) then
  try
    iTotal := GetFileSize_Response();
    if prog <> nil then begin
      prog.stepcount := iTotal;
      prog.step := 0;
    end;

    iStart := 0;
    if FileExists(sLocalFile) then
      Deletefile(sLocalFile);
    ForceDirectories(extractfilepath(sLocalFile));

    fs := TFileStream.create(sLocalFile, fmCreate);
    try
      iWritePos := 0;
      repeat
        ftr.StartBlock := iStart;
        iToWRite := lesserof(262144, iTotal-iWritePos);
        ftr.Length := iToWrite;
        ftr.Buffer := nil;
        ftr.ContainsData := false;
        GetFile_Async(ftr);
        inc(iStart,ftr.Length);
        inc(iWritePos, iToWrite);
      until iWritePos = iTotal;

      CloseFile_Async(ftr);


      iStart := 0;
      iWritePos := 0;
      repeat
        ftr.StartBlock := iStart;
        iToWRite := lesserof(262144, iTotal-iWritePos);
        ftr.Length := iToWrite;
        ftr.Buffer := nil;
        ftr.ContainsData := false;
//        GetFile_Async(ftr);
        GetFile_Response(ftr);
        stream_GuaranteeWrite(fs, ftr.buffer, ftr.Length);
        if prog<> nil then
          inc(prog.step, ftr.Length);

        if ftr.eof then begin
          fileSetDate(fs.Handle, DatetimeToFileDate(ftr.FileDate));
        end;
        inc(iStart,ftr.Length);

        inc(iWritePos, iToWrite);
      until iWritePos = iTotal;

    finally
      fs.free;
    end;
  finally
    ftr.ContainsData := false;
    self.CloseFile_REsponse();
    ftr.free;
  end
  else
    raise ECritical.create('Failed to open remote file '+sRemoteFile);



end;



procedure TFileServiceClientEx.PutFileEx(sLocalFile, sRemotefile: string; prog: PProgress = nil);
const PUT_SIZE = 4000000;
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
    fs := TMemoryFileStream.create(sLocalFile, fmOpenRead+fmShareDenyNone);
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
          iToWrite := lesserof(PUT_SIZE, fs.Size-fs.Position);
          GEtMem(b, iToWrite);
          ftr.Buffer := b;
          ftr.StartBlock := iPos;
          ftr.Length := stream_guaranteeread(fs, ftr.Buffer, PUT_SIZE);//fs.Read(b[0], PUT_SIZE);
          inc(iPos, ftr.Length);

          ftr.EOF := fs.Position >= fs.Size;
          ftr.FileDate := FileDateToDateTime(FileGetDAte(fs.handle));
          ftr.ContainsData := true;
          self.PutFile(ftr);
          ftr.FreeBuffer;


        until ftr.eof;
      finally
        ftr.Buffer := nil;
        ftr.length := 0;
        self.CloseFile(ftr);
        ftr.free;
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
  cli.GetFileEx(remotefile, localfile, @self.progress);
end;

{ Tcmd_PutFileEx }

procedure Tcmd_PutFileEx.DoExecute;
begin
  inherited;
  cli.GetFileEx(localfile, remotefile, @self.progress);
end;

end.
