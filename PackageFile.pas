unit PackageFile;

interface

uses
  systemx, stringx, typex, classes, sysutils, betterfilestream, sharedobject, helpers_stream, dir, dirfile, zlib;


type
  TWriteStream = TCompressionStream;
  TReadStream = TDecompressionStream;

  TPackageMainHeader = packed record
  end;
  TPackageItemHeader = packed record
  private
    function GetName: string;
    procedure SEtNAme(const Value: string);

  public
    Fname: array [0..1024] of char;
    DataSize: int64;
    CompressedSize: int64;
    procedure INit;
    property NAme: string read GetName write SEtNAme;

  end;

  TPackageFile <_BACKSTREAM_: betterfilestream.TFileStreamWithVirtualConstructors> = class(TSharedObject)
  private
    FFileNAme: string;
    s: _BACKSTREAM_;
    procedure SetFileNAme(const Value: string);
  public

    procedure Init;override;
    procedure Detach;override;
    procedure New(sFileName: string);
    procedure Load(sFileNAme: string);

    property FileName: string read FFileNAme write FFileNAme;
    function SeekToFileStart(sNAme: string): TPackageItemHeader;
    property Stream: _BACKSTREAM_ read s;
    function Append(itm: TPackageItemHeader): TStream;
    procedure AddFromFolder(sDir: string);
    procedure Reset;
    function GetNextFile(out itm: TPackageItemHeader; out str: TStream): boolean;
    function GetAssetStream(sAssetName: string; bUnique: boolean=true): TStream;
  end;

  TBasicPackageFile = class(TPackageFile<TMoreBetterFileStream>)
  public
  end;


implementation

{ TPackageItemHeader }

function TPackageItemHeader.GetName: string;
begin
  result := FName;
end;

function TPackageFile<_BACKSTREAM_>.GetAssetStream(sAssetName: string;
  bUnique: boolean): TStream;
var
  itm: TPackageItemHeader;
  dcs: TDecompressionStream;
begin
  result := nil;
  itm := self.SeekToFileStart(sAssetName);
  if itm.DataSize > 0 then begin
    dcs := TDecompressionStream.Create(self.s);
    try
      result := TMemoryStream.create;
      stream_GuaranteeCopy(dcs, result, itm.DataSize);
      result.seek(0,0);
    finally
      dcs.free;
    end;
  end;


end;

function TPackageFile<_BACKSTREAM_>.GetNextFile(out itm: TPackageItemHeader; out str: TStream): boolean;
begin
  if s.position = s.size then
    exit(false);

  stream_GuaranteeRead(s, Pbyte(@itm), sizeof(TPackageItemHeader));
  str := TDecompressionStream.create(s);
  exit(true);

end;

procedure TPackageItemHeader.INit;
begin
  FillMem(pbyte(@self), sizeof(self), 0);
end;

procedure TPackageFile<_BACKSTREAM_>.Reset;
begin
  s.seek(0,0);
end;

procedure TPackageItemHeader.SEtNAme(const Value: string);
begin
  movemem32(@FName[0], @value[STRZ], (length(value)+1)*sizeof(char));
end;

{ TPackageFile<_BACKSTREAM_> }

function TPackageFile<_BACKSTREAM_>.Append(
  itm: TPackageItemHeader): TStream;
var
  cs: TCompressionStream;
begin
  s.seek(0,soEnd);
  stream_GuaranteeWrite(s, Pbyte(@itm), sizeof(itm));
  cs := TCompressionStream.create(s);
  result := cs;
end;

procedure TPackageFile<_BACKSTREAM_>.Detach;
begin
  inherited;
  s.free;
  s := nil;
end;

procedure TPackageFile<_BACKSTREAM_>.Init;
begin
  inherited;

end;

procedure TPackageFile<_BACKSTREAM_>.Load(sFileNAme: string);
begin
  FileNAme := sFileNAme;
  s := _BACKSTREAM_.create(filename, fmOpenREad+fmShareDenyNone);
end;

procedure TPackageFile<_BACKSTREAM_>.AddFromFolder(sDir: string);
var
  dir: TDirectory;
  fi: TFileInformation;
  sRelName: string;
  itm: TPackageItemHeader;
  fs: TFileStream;
  sTargetTempStream: TStream;
  spos: int64;
begin
  sDir := slash(sDir);
  dir := TDirectory.create(sDir, '*.*', 0,0, true, false, false);
  try
    while dir.GetNextFile(fi) do begin
      sRelName := fi.FullName;
      if comparetext(extractfileext(sRelname), '.gpk')=0 then
        continue;

      sRelName := zcopy(sRelName, length(sDir), length(sRelName)-length(sDir));
      itm.init;
      itm.Name := sRelName;
      fs := TFileStream.create(fi.fullname, fmOpenREad+fmShareDenyNone);
      fs.seek(0,0);
      itm.datasize := fs.Size;
      try
        sTargetTempStream := Append(itm);
        spos := stream.position;//this is a damn mess
        try
          stream_GuaranteeCopy(fs, sTargetTempStream, fs.Size);
          sTargetTempSTream.free;   //<---IMportant!  stuff doesn't get committed to the back stream until the compression stream is freed
          sTargetTempStream := nil;
          stream.seek(spos,0);
          itm.CompressedSize := stream.size - spos;
          stream.seek(0-sizeof(itm), soCurrent);
          stream_guaranteewrite(stream, @itm, sizeof(itm));


        finally
          if sTargetTempSTream <> nil then begin
            sTargetTempSTream.free;
            sTargetTempStream := nil;
          end;
        end;

      finally
        fs.Free;
      end;
    end;
  finally
    dir.free;
  end;
end;

procedure TPackageFile<_BACKSTREAM_>.New(sFileName: string);
begin
  FileNAme := sFileNAme;
  s := _BACKSTREAM_.create(filename, fmCReate);
end;

function TPackageFile<_BACKSTREAM_>.SeekToFileStart(
  sNAme: string): TPackageItemHeader;
begin
  s.seek(sizeof(TPackageMainHEader), soBeginning);

  repeat
    if s.position = s.size then begin
      raise ECritical.create('item not found in package: '+sName);
    end;

    stream_GuaranteeRead(s, Pbyte(@result), sizeof(result));
    if comparetext(result.NAme, sNAme)=0 then begin
      break;
    end else begin
      s.seek(result.CompressedSize, soFromCurrent);
    end;

  until false //(forever);





end;

procedure TPackageFile<_BACKSTREAM_>.SetFileNAme(const Value: string);
begin
  FFileNAme := Value;
end;

end.
