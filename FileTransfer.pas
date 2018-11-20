unit FileTransfer;
//makes use of FileServiceClient to transfer files from remote servers

interface

uses
  memoryfilestream, FileServiceClient, dir, dirfile, sysutils, classes;

implementation

type
  TFileTransferClientEx = class(TFileServiceClient)
  public
    procedure GetFileEx(sRemoteFile: ansistring; sLocalFile: ansistring);

  end;

{ TFileServiceClientEx }

procedure TFileTransferClientEx.GetFileEx(sRemoteFile, sLocalFile: ansistring);
var
  ftr: TFileTransferReference;
  iStart: int64;
  fs: TMemoryFileStream;
begin
  ftr := TFileTransferReference.create;
  try
    ftr.FileName := sRemoteFile;
    iStart := 0;
    if FileExists(sLocalFile) then
      Deletefile(sLocalFile);
    fs := TMemoryFileStream.create(sLocalFile, fmCreate);
    repeat
      ftr.StartBlock := iStart;
      ftr.Length := 3600;
      ftr.Buffer := nil;
      ftr.ContainsData := false;
      GetFile(ftr);
      if ftr.eof then begin
        fileSetDate(fs.Handle, DatetimeToFileDate(ftr.FileDate));
      end;      

    until ftr.EOF;
  finally
    ftr.free;
  end;



end;

end.
