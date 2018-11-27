unit PXL.Archives;
{
  This file is part of Asphyre Framework, also known as Pascal eXtended Library (PXL).
  Copyright (c) 2000 - 2015  Yuriy Kotsarenko

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.
}
interface

{$INCLUDE PXL.Config.inc}

uses
  Classes, PXL.TypeDef;

{ Archive Header structure:
    Signature     - 4 bytes (Typically 'PXLA')
    Record Count  - longint
    Table Offset  - longword

  Archive Table structure:
    Key Name      - 2 bytes (length) + X bytes (UTF-8 text)
    Offset        - longword

  Archive Record structure:
    Record Type   -  byte (4 bits - record type, 4 bits - security)

    Original Size -  longword
    Physical Size -  longword
    File Date     -  int64

    Checksum      -  longword (CRC32)
    Init Vector   -  uint64 (optional, only if encrypted)

    DataBlock     -  N bytes (equals to Physical Size) }

type
  TArchive = class
  public type
    TFormat = (PXLA, ASVF, ASDb, VTDb);

    TOpenMode = (Update, ReadOnly, Overwrite);
    TEntryType = (AnyFile, Image, Font);
    TAccessType = (AnyFile, Resource, Packaged);

    TRecordEntry = class
    private
      FKey: UniString;
      FEntryType: TEntryType;
      FOriginalSize: Cardinal;
      FPhysicalSize: Cardinal;
      FDateTime: TDateTime;
      FChecksum: Cardinal;
      FOffset: Cardinal;
      FInitVector: UInt64;
      FSecure: Boolean;
      FExtraSize: Integer;
      FExtraField: Pointer;
    protected
      constructor CreateClone(const Source: TRecordEntry);

      procedure Reset;
      procedure CopyFrom(const Source: TRecordEntry);

      property Offset: Cardinal read FOffset;
      property InitVector: UInt64 read FInitVector;
    public
      property Key: UniString read FKey;
      property EntryType: TEntryType read FEntryType;
      property OriginalSize: Cardinal read FOriginalSize;
      property PhysicalSize: Cardinal read FPhysicalSize;
      property DateTime: TDateTime read FDateTime;
      property Checksum: Cardinal read FChecksum;
      property Secure: Boolean read FSecure;
      property ExtraSize: Integer read FExtraSize;
      property ExtraField: Pointer read FExtraField;

      constructor Create;
      destructor Destroy; override;
    end;

    TDefaultPackagePathProvider = procedure(const Sender: TObject; var PackagePath: StdString) of object;
  public class var
    AccessType: TAccessType;
    AccessInstance: SizeUInt;
    DefaultPackagePathProvider: TDefaultPackagePathProvider;
  private const
    // The size of additional security data added to the record's header.
    EntrySecuritySize: Cardinal = 8;

    // Temporary archive name to be used when deleting or overwriting records.
    TempFileText: StdString = 'archive.temp.pxla';
  private
    FFormat: TFormat;
    FOpenMode: TOpenMode;
    FFileName: StdString;
    FFileSize: Integer;
    FPassword: Pointer;

    EntryList: array of TRecordEntry;

    TableOffset: Cardinal;
    FReady: Boolean;

    SearchList: array of Integer;
    SearchDirty: Boolean;

    // The size in bytes of the archive's header.
    function ArchiveHeaderSize: Cardinal; inline;

    { In original record position, this offset determines where record data is allocated. This is used for ReadRecord
      method to get directly to record data. Also used for removing records. The encryption initial vector (see below)
      is added optionally for secure records. }
    function EntryDataOffset: Cardinal; inline;

    function GetEntry(const Index: Integer): TRecordEntry;
    function GetEntryCount: Integer;

    function FixPlatformFileName(const NewFileName: StdString): StdString;
    procedure ResetEntryList;
    procedure ResetBasicState;

    procedure SetFileName(const Value: StdString);

    function CreateEmptyFile: Boolean;
    function GetPackagePath: StdString;
    function CreateReadStream: TStream;

    procedure InitEntryList(const EntryCount: Integer);
    procedure ReadSignature(const Stream: TStream);
    function ReadHeader: Boolean;
    function WriteHeader: Boolean;
    function ReadEntryTable: Boolean;
    function WriteEntryTable: Boolean;
    function ReadEntryHeaders: Boolean;

    function ReadDetailedEntryList: Boolean;
    function RefreshArchive: Boolean;

    function CompressDataBlock(const Source: Pointer; const SourceSize: Integer; out Data: Pointer;
      out DataSize: Integer): Boolean;
    function DecompressDataBlock(const Source: Pointer; const SourceSize: Integer; out Data: Pointer;
      const DataSize: Integer): Boolean;
    function DecompressToMemStream(const Source: Pointer; const SourceSize: Integer;
      const DestStream: TMemoryStream): Boolean;

    procedure EncryptDataBlock(const Data: Pointer; const DataSize: Integer; out InitVec: UInt64);
    procedure DecryptDataBlock(const Data: Pointer; const DataSize: Integer; const InitVec: UInt64);

    function CreateNewEntry: TRecordEntry;

    procedure InitSearchList;
    procedure SearchListSwap(const Index1, Index2: Integer);
    function SearchListCompare(const Value1, Value2: Integer): Integer;
    function SearchListSplit(const Start, Stop: Integer): Integer;
    procedure SearchListSort(const Start, Stop: Integer);
    procedure UpdateSearchList;
  public
    constructor Create;
    destructor Destroy; override;

    function Refresh: Boolean;
    function OpenFile(const AFileName: StdString): Boolean;

    function IndexOf(const Key: UniString): Integer;

    function WriteEntry(const Key: UniString; const Source: Pointer; const SourceSize: Integer;
      const EntryType: TEntryType; const DateTime: TDateTime): Boolean; overload;

    function WriteEntry(const Key: UniString; const Source: Pointer; const SourceSize: Integer;
      const EntryType: TEntryType = TEntryType.AnyFile): Boolean; overload;

    function WriteStream(const Key: UniString; const Stream: TStream; const EntryType: TEntryType;
      const DateTime: TDateTime): Boolean; overload;

    function WriteStream(const Key: UniString; const Stream: TStream;
      const EntryType: TEntryType = TEntryType.AnyFile): Boolean; overload;

    function ReadEntry(const Key: UniString; out Data: Pointer; out DataSize: Integer): Boolean;
    function ReadStream(const Key: UniString; Stream: TStream): Boolean;
    function ReadMemStream(const Key: UniString; const MemStream: TMemoryStream): Boolean;

    function RenameEntry(const Key, NewKey: UniString): Boolean;
    function RemoveEntry(const Key: UniString): Boolean;

    property Format: TFormat read FFormat write FFormat;

    property OpenMode: TOpenMode read FOpenMode write FOpenMode;
    property FileName: StdString read FFileName write SetFileName;

    property Ready: Boolean read FReady;
    property FileSize: Integer read FFileSize;
    property Password: Pointer read FPassword write FPassword;

    property EntryCount: Integer read GetEntryCount;
    property Entries[const Index: Integer]: TRecordEntry read GetEntry; default;
  end;

  TArchivePassword = class
  private type
    TRequestType = (ProvideKey, BurnKey);

    TPasswordRequestCallback = procedure(const Sender: TObject; const Archive: TArchive;
      const RequestType: TRequestType) of object;

    TCallbackItem = record
      Callback: TPasswordRequestCallback;
      ItemID: Cardinal;
    end;
  private
    Items: array of TCallbackItem;
    CurrentID: Cardinal;

    Authorized: Boolean;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} AuthArchive: TArchive;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} AuthSender: TObject;
    AuthIndex: Integer;

    function NextID: Cardinal;
    function IndexOf(const ItemID: Cardinal): Integer;
    procedure Remove(const Index: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Subscribe(const Callback: TPasswordRequestCallback): Cardinal;
    procedure Unsubscribe(const EventID: Cardinal);

    function Authorize(const Sender: TObject; const Archive: TArchive): Boolean;
    procedure Unauthorize;

    procedure ProvideKey(const Key: Pointer);
  private
    class var FCurrent: TArchivePassword;
  public
    class property Current: TArchivePassword read FCurrent;
  end;

implementation

uses
{$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}

  {$IFDEF ANDROID}
    Posix.Unistd, Posix.Stdio,
  {$ENDIF}
{$ENDIF}

  Types, SysUtils, DateUtils, Math, PXL.Data, PXL.Classes;

{$REGION 'TArchive.TRecordEntry'}

constructor TArchive.TRecordEntry.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;

  Reset;
end;

constructor TArchive.TRecordEntry.CreateClone(const Source: TRecordEntry);
begin
  Create;
  CopyFrom(Source);
end;

destructor TArchive.TRecordEntry.Destroy;
begin
  try
    if FExtraField <> nil then
      FreeMem(FExtraField);
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

procedure TArchive.TRecordEntry.Reset;
begin
  FKey := '';
  FEntryType := TEntryType.AnyFile;
  FOriginalSize := 0;
  FPhysicalSize := 0;
  FDateTime := 0.0;
  FChecksum := 0;
  FOffset := 0;
  FInitVector := 0;
  FSecure := False;
  FExtraSize := 0;
  FreeMemAndNil(FExtraField);
end;

procedure TArchive.TRecordEntry.CopyFrom(const Source: TRecordEntry);
begin
  FKey := Source.FKey;
  FEntryType := Source.FEntryType;
  FOriginalSize := Source.FOriginalSize;
  FPhysicalSize := Source.FPhysicalSize;
  FDateTime := Source.FDateTime;
  FChecksum := Source.FChecksum;
  FOffset := Source.FOffset;
  FInitVector := Source.FInitVector;
  FSecure := Source.FSecure;

  FExtraSize := Source.FExtraSize;
  if FExtraSize > 0 then
  begin
    ReallocMem(FExtraField, FExtraSize);
    Move(Source.FExtraField^, FExtraField^, FExtraSize);
  end
  else
    FreeMemAndNil(FExtraField);
end;

{$ENDREGION}
{$REGION 'TArchive'}

constructor TArchive.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;

  FOpenMode := TOpenMode.Update;
  FFileName := '';
  FPassword := nil;

  ResetBasicState;

  SearchDirty := False;
end;

destructor TArchive.Destroy;
begin
  try
    ResetEntryList;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TArchive.ArchiveHeaderSize: Cardinal;
begin
  case FFormat of
    TFormat.VTDb: Result := 32;
    TFormat.ASVF: Result := 16;
  else
    Result := 12; // PXLA / ASDb
  end;
end;

function TArchive.EntryDataOffset: Cardinal;
begin
  case FFormat of
    TFormat.VTDb: Result := 22;
    TFormat.ASDb: Result := 44;
  else
    Result := 21; // PXLA / ASVF
  end;
end;

function TArchive.GetEntryCount: Integer;
begin
  Result := Length(EntryList);
end;

function TArchive.GetEntry(const Index: Integer): TRecordEntry;
begin
  if (Index >= 0) and (Index < Length(EntryList)) then
    Result := EntryList[Index]
  else
    Result := nil;
end;

function TArchive.FixPlatformFileName(const NewFileName: StdString): StdString;
begin
{$IFDEF MSWINDOWS}
  Result := StringReplace(NewFileName, '/', '\', [rfReplaceAll, rfIgnoreCase]);
{$ELSE}
  Result := StringReplace(NewFileName, '\', '/', [rfReplaceAll, rfIgnoreCase]);
{$ENDIF}
end;

procedure TArchive.ResetEntryList;
var
  I: Integer;
begin
  for I := Length(EntryList) - 1 downto 0 do
    EntryList[I].Free;

  SetLength(EntryList, 0);
end;

procedure TArchive.ResetBasicState;
begin
  FFileSize := 0;
  TableOffset := ArchiveHeaderSize;
  FReady := False;
  FFormat := TFormat.PXLA;
end;

procedure TArchive.SetFileName(const Value: StdString);
var
  PrevFileName: StdString;
begin
  PrevFileName := FFileName;

  FFileName := FixPlatformFileName(Value);

  if not SameText(FFileName, PrevFileName) then
    if Length(FFileName) > 0 then
    begin
      FReady := RefreshArchive;

      if (not FReady) then
        FFileName := '';
    end
    else
    begin
      ResetEntryList;
      ResetBasicState;

      SearchDirty := True;
    end;
end;

function TArchive.OpenFile(const AFileName: StdString): Boolean;
var
  NewFileName: StdString;
begin
  Result := True;

  NewFileName := FixPlatformFileName(AFileName);

  if Length(NewFileName) < 1 then
  begin
    if Length(FFileName) > 0 then
    begin
      FFileName := '';

      ResetEntryList;
      ResetBasicState;

      SearchDirty := True;
    end;

    Exit;
  end;

  if not SameText(FFileName, NewFileName) then
  begin
    FFileName := NewFileName;

    FReady := RefreshArchive;
    Result := FReady;
  end;
end;

function TArchive.CreateEmptyFile: Boolean;
var
  InpFileName: StdString;
  Stream: TFileStream;
begin
  if (FOpenMode = TOpenMode.ReadOnly) or (AccessType = TAccessType.Resource) or (FFormat <> TFormat.PXLA) then
    Exit(False);

  InpFileName := FFileName;

  if AccessType = TAccessType.Packaged then
    InpFileName := GetPackagePath + ExtractFileName(FFileName);

  try
    Stream := TFileStream.Create(InpFileName, fmCreate or fmShareExclusive);
  except
    Exit(False);
  end;

  try
    Result := True;
    try
      // --> Signature
      Stream.PutByte(Ord('P'));
      Stream.PutByte(Ord('X'));
      Stream.PutByte(Ord('L'));
      Stream.PutByte(Ord('A'));
      // --> Entry Count
      Stream.PutLongInt(0);
      // --> Table Offset
      Stream.PutLongWord(ArchiveHeaderSize);
    except
      Result := False;
    end;
  finally
    Stream.Free;
  end;

  ResetEntryList;

  if Result then
  begin
    FFileSize := ArchiveHeaderSize;
    TableOffset := ArchiveHeaderSize;

    if FOpenMode = TOpenMode.Overwrite then
      FOpenMode := TOpenMode.Update;
  end
  else
  begin
    FFileSize := 0;
    TableOffset := 0;
  end;

  SearchDirty := True;
end;

procedure TArchive.InitEntryList(const EntryCount: Integer);
var
  I: Integer;
begin
  if Length(EntryList) > 0 then
    ResetEntryList;

  if EntryCount < 1 then
    Exit;

  SetLength(EntryList, EntryCount);

  for I := 0 to EntryCount - 1 do
    EntryList[I] := TRecordEntry.Create;

  SearchDirty := True;
end;

function TArchive.GetPackagePath: StdString;
begin
  Result := ExtractFilePath(ParamStr(0));

  if Assigned(DefaultPackagePathProvider) then
    DefaultPackagePathProvider(Self, Result);
end;

function TArchive.CreateReadStream: TStream;
begin
  Result := nil;
  if (FOpenMode = TOpenMode.Overwrite) or ((FOpenMode = TOpenMode.Update) and (AccessType = TAccessType.Resource)) then
    Exit;

  try
    case FOpenMode of
      TOpenMode.Update:
        Result := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);

      TOpenMode.ReadOnly:
        case AccessType of
          TAccessType.AnyFile:
            Result := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);

          TAccessType.Resource:
            Result := TResourceStream.Create(AccessInstance, ExtractFileName(FFileName), RT_RCDATA);

          TAccessType.Packaged:
            Result := TFileStream.Create(GetPackagePath + ExtractFileName(FFileName), fmOpenRead or fmShareDenyWrite);
        end;
    end;
  except
    Exit;
  end;
end;

procedure TArchive.ReadSignature(const Stream: TStream);
var
  Values: array[0..3] of Byte;
  I: Integer;
begin
  for I := 0 to 3 do
    Values[I] := Stream.GetByte;

  if (Values[0] = Ord('v')) and (Values[1] = Ord('t')) and (Values[2] = Ord('d')) and (Values[3] = Ord('m')) then
    FFormat := TFormat.VTDb
  else if (Values[0] = Ord('A')) and (Values[1] = Ord('S')) and (Values[2] = Ord('D')) and (Values[3] = Ord('b')) then
    FFormat := TFormat.ASDb
  else if (Values[0] = Ord('A')) and (Values[1] = Ord('S')) and (Values[2] = Ord('V')) and (Values[3] = Ord('F')) then
    FFormat := TFormat.ASVF
  else
    FFormat := TFormat.PXLA;
end;

function TArchive.ReadHeader: Boolean;
var
  Stream: TStream;
  EntryCount: Integer;
begin
  ResetEntryList;

  SearchDirty := True;

  Stream := CreateReadStream;
  if Stream = nil then
    Exit(False);

  try
    try
      // --> Signature
      ReadSignature(Stream);
      // --> Entry Count
      EntryCount := Stream.GetLongInt;
      // --> Table Offset
      if FFormat = TFormat.ASVF then
        TableOffset := Stream.GetInt64
      else
        TableOffset := Stream.GetLongWord;

      FFileSize := Stream.Size;
    except
      Exit(False);
    end;

    // The offset to record table should always be valid, no matter if there are no records stored in the archive.
    if TableOffset < ArchiveHeaderSize then
      Exit(False);
  finally
    Stream.Free;
  end;

  Result := True;

  // Set the total number of records and set them to empty values.
  if EntryCount > 0 then
    InitEntryList(EntryCount);
end;

function TArchive.WriteHeader: Boolean;
var
  Stream: TFileStream;
begin
  if (FOpenMode = TOpenMode.ReadOnly) or (AccessType = TAccessType.Resource) or (FFormat <> TFormat.PXLA) then
    Exit(False);

  try
    Stream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
  except
    Exit(False);
  end;

  try
    Result := True;
    try
      // Skip the signature, it is supposed to be valid.
      Stream.Position := 4;

      // --> Entry Count
      Stream.PutLongInt(Length(EntryList));
      // --> Table Offset
      Stream.PutLongWord(TableOffset);

      FFileSize := Stream.Size;
    except
      Result := False;
    end;
  finally
    Stream.Free;
  end;
end;

function TArchive.ReadEntryTable: Boolean;
var
  Stream: TStream;
  I: Integer;
begin
  Stream := CreateReadStream;
  if Stream = nil then
    Exit(False);

  try
    Result := True;
    try
      Stream.Position := TableOffset;

      for I := 0 to Length(EntryList) - 1 do
        case FFormat of
          TFormat.VTDb,
          TFormat.ASDb:
            begin
              // --> Key
              EntryList[I].FKey := Stream.GetLongString;
              // --> Offset
              EntryList[I].FOffset := Stream.GetLongWord;
            end;

          TFormat.ASVF:
            begin
              // --> Key
              EntryList[I].FKey := Stream.GetMediumString;
              // --> Offset
              EntryList[I].FOffset := Stream.GetInt64;
            end;

        else
          begin // PXLA
            // --> Key
            EntryList[I].FKey := Stream.GetMediumString;
            // --> Offset
            EntryList[I].FOffset := Stream.GetLongWord;
          end;
        end;

      FFileSize := Stream.Size;
    except
      Result := False;
    end;
  finally
    Stream.Free;
  end;

  SearchDirty := True;
end;

function TArchive.WriteEntryTable: Boolean;
var
  Stream: TFileStream;
  I: Integer;
begin
  if (FOpenMode = TOpenMode.ReadOnly) or (AccessType = TAccessType.Resource) or (FFormat <> TFormat.PXLA) then
    Exit(False);

  try
    Stream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
  except
    Exit(False);
  end;

  try
    Result := True;
    try
      Stream.Position := TableOffset;

      for I := 0 to Length(EntryList) - 1 do
      begin
        // --> Key
        Stream.PutMediumString(EntryList[I].FKey);
        // --> Offset
        Stream.PutLongWord(EntryList[I].FOffset);
      end;

      FFileSize := Stream.Size;
    except
      Result := False;
    end;
  finally
    Stream.Free;
  end;
end;

function TArchive.ReadEntryHeaders: Boolean;
var
  Stream: TStream;
  I, Value: Integer;
  EntryType: TEntryType;
begin
  Stream := CreateReadStream;
  if Stream = nil then
    Exit(False);

  try
    Result := True;
    try
      for I := 0 to Length(EntryList) - 1 do
      begin
        Stream.Position := EntryList[I].FOffset;

        case FFormat of
          TFormat.VTDb:
            begin
              // --> Record Type
              Value := Stream.GetWord;

              case Value of
                1: EntryType := TEntryType.Image;
                5: EntryType := TEntryType.Font;
              else
                EntryType := TEntryType.AnyFile;
              end;

              EntryList[I].FEntryType := EntryType;

              // --> Original Size
              EntryList[I].FOriginalSize := Stream.GetLongWord;
              // --> Physical Size
              EntryList[I].FPhysicalSize := Stream.GetLongWord;
              // --> Checksum
              EntryList[I].FChecksum := Stream.GetLongWord;
              // --> File Date
              EntryList[I].FDateTime := Stream.GetDouble;

              EntryList[I].FSecure := False; // not supported in VTDb
            end;

          TFormat.ASDb:
            begin
              // --> Record Type
              Value := Stream.GetWord;

              if Value = 1 then
                EntryType := TEntryType.Image
              else
                EntryType := TEntryType.AnyFile;

              EntryList[I].FEntryType := EntryType;

              // --> Original Size
              EntryList[I].FOriginalSize := Stream.GetLongWord;
              // --> Physical Size
              EntryList[I].FPhysicalSize := Stream.GetLongWord;
              // --> File Date
              EntryList[I].FDateTime := Stream.GetDouble;
              // --> Checksum
              EntryList[I].FExtraSize := SizeOf(TMD5Checksum);
              ReallocMem(EntryList[I].FExtraField, SizeOf(TMD5Checksum));
              Stream.ReadBuffer(EntryList[I].FExtraField^, SizeOf(TMD5Checksum));
              // --> Secure
              EntryList[I].FSecure := WordBool(Stream.GetWord);
              // --> Init Vector
              EntryList[I].FInitVector := Stream.GetUInt64;
            end;

          TFormat.ASVF:
            begin
              // --> Record Type + Security
              Value := Stream.GetByte;

              EntryList[I].FEntryType := TEntryType(Value and $0F);
              EntryList[I].FSecure := (Value shr 4) > 0;

              // --> Original Size
              EntryList[I].FOriginalSize := Stream.GetLongWord;
              // --> Physical Size
              EntryList[I].FPhysicalSize := Stream.GetLongWord;
              // --> File Date
              EntryList[I].FDateTime := Stream.GetDouble;
              // --> Checksum
              EntryList[I].FChecksum := Stream.GetLongWord;

              // --> Init Vector (only if secure)
              if (EntryList[I].Secure) then
                EntryList[I].FInitVector := Stream.GetUInt64;
            end;

        else
          begin // PXLA
            // --> Record Type + Security
            Value := Stream.GetByte;

            EntryList[I].FEntryType := TEntryType(Value and $0F);
            EntryList[I].FSecure := (Value shr 4) > 0;

            // --> Original Size
            EntryList[I].FOriginalSize := Stream.GetLongWord;
            // --> Physical Size
            EntryList[I].FPhysicalSize := Stream.GetLongWord;
            // --> File Date
            EntryList[I].FDateTime := UnixToDateTime(Stream.GetInt64);
            // --> Checksum
            EntryList[I].FChecksum := Stream.GetLongWord;

            // --> Init Vector (only if secure)
            if (EntryList[I].Secure) then
              EntryList[I].FInitVector := Stream.GetUInt64;
          end;
        end;
      end;

      FFileSize := Stream.Size;
    except
      Result := False;
    end;
  finally
    Stream.Free;
  end;
end;

function TArchive.ReadDetailedEntryList: Boolean;
begin
  if not ReadHeader then
    Exit(False);

  if not ReadEntryTable then
    Exit(False);

  Result := ReadEntryHeaders;
end;

function TArchive.RefreshArchive: Boolean;
begin
  case FOpenMode of
    TOpenMode.Update:
      if not FileExists(FFileName) then
        Result := CreateEmptyFile
      else
        Result := ReadDetailedEntryList;

    TOpenMode.ReadOnly:
      Result := ReadDetailedEntryList;

    TOpenMode.Overwrite:
      Result := CreateEmptyFile;

  else
    Result := False;
  end;
end;

function TArchive.Refresh: Boolean;
begin
  if Length(FFileName) > 0 then
    Result := RefreshArchive
  else
    Result := False;
end;

function TArchive.CompressDataBlock(const Source: Pointer; const SourceSize: Integer; out Data: Pointer;
  out DataSize: Integer): Boolean;
const
  BufferGrow = 5; // default: 5 (in %)

  // For the same purpose as BufferGrow, this value is simply added to the
  // buffer size previously increased by BufferGrow (for very short buffers).
  BufferGrowAdd = 256; // default: 256
var
  CodeBuf: Pointer;
  BufferSize: Cardinal;
begin
  BufferSize := Ceil((Cardinal(SourceSize) * (100 + BufferGrow)) / 100) + BufferGrowAdd;
  GetMem(CodeBuf, BufferSize);

  try
    DataSize := CompressData(Source, CodeBuf, SourceSize, BufferSize, TCompressionLevel.BestCompression);
    if DataSize < 1 then
      Exit(False);

    GetMem(Data, DataSize);
    Move(CodeBuf^, Data^, DataSize);
  finally
    FreeMem(CodeBuf);
  end;

  Result := True;
end;

function TArchive.DecompressDataBlock(const Source: Pointer; const SourceSize: Integer; out Data: Pointer;
  const DataSize: Integer): Boolean;
var
  OutSize: Integer;
begin
  GetMem(Data, DataSize);
  OutSize := DecompressData(Source, Data, SourceSize, DataSize);

  if (OutSize < 1) or (Int64(OutSize) <> DataSize) then
  begin
    FreeMemAndNil(Data);
    Exit(False);
  end;

  Result := True;
end;

function TArchive.DecompressToMemStream(const Source: Pointer; const SourceSize: Integer;
  const DestStream: TMemoryStream): Boolean;
var
  OutSize: Integer;
begin
  if DestStream.Size < 1 then
    Exit(False);

  OutSize := DecompressData(Source, DestStream.Memory, SourceSize, DestStream.Size);
  if (OutSize < 1) or (Int64(OutSize) <> DestStream.Size) then
    Exit(False);

  Result := True;
end;

procedure TArchive.EncryptDataBlock(const Data: Pointer; const DataSize: Integer; out InitVec: UInt64);
begin
  if FPassword = nil then
  begin
    InitVec := 0;
    Exit;
  end;

  TCipherBlock(InitVec).Init;
  EncryptData(Data, Data, DataSize, TCipherBlock(InitVec), PCipherKey(FPassword)^);
end;

procedure TArchive.DecryptDataBlock(const Data: Pointer; const DataSize: Integer; const InitVec: UInt64);
begin
  if FPassword = nil then
    Exit;

  DecryptData(Data, Data, DataSize, TCipherBlock(InitVec), PCipherKey(FPassword)^);
end;

function TArchive.CreateNewEntry: TRecordEntry;
var
  Index: Integer;
begin
  Index := Length(EntryList);
  SetLength(EntryList, Index + 1);

  EntryList[Index] := TRecordEntry.Create;
  Result := EntryList[Index];
end;

function TArchive.WriteEntry(const Key: UniString; const Source: Pointer; const SourceSize: Integer;
  const EntryType: TEntryType; const DateTime: TDateTime): Boolean;
var
  Value: Integer;
  Data: Pointer;
  DataSize: Integer;
  Stream: TStream;
  InitVector: UInt64;
  Checksum: Cardinal;
  EntryOffset: Cardinal;
  NewEntry: TRecordEntry;
begin
  // (1) Validate current mode and access type.
  if (FOpenMode = TOpenMode.ReadOnly) or (AccessType = TAccessType.Resource) or (FFormat <> TFormat.PXLA) then
    Exit(False);

  // (2) If the file is not on the disk, create an empty archive first.
  if (not FileExists(FFileName)) and (not CreateEmptyFile) then
    Exit(False);

  // (3) If the entry already exists, remove it first.
  if IndexOf(Key) <> -1 then
  begin
    RemoveEntry(Key);

    // If the entry still persists (it wasn't deleted), there is nothing we can do.
    if IndexOf(Key) <> -1 then
      Exit(False);
  end;

  // (4) Calculate CRC32 checksum.
  Checksum := ChecksumCRC32(Source, SourceSize);

  // (5) Compress entry data.
  if not CompressDataBlock(Source, SourceSize, Data, DataSize) then
    Exit(False);

  try
    // (6) Encrypt compressed data block (if done on uncompressed data, it will make compression inefficient).
    EncryptDataBlock(Data, DataSize, InitVector);

    // (7) Write newly compressed (and possibly encrypted) data block to archive.
    try
      Stream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
    except
      Exit(False);
    end;

    try
      EntryOffset := TableOffset;

      try
        Stream.Position := EntryOffset;

        // --> Record Type + Security
        Value := Integer(EntryType) and $0F;
        if FPassword <> nil then
          Value := Value or $F0;

        Stream.PutByte(Value);

        // --> Original Size
        Stream.PutLongWord(SourceSize);
        // --> Physical Size
        Stream.PutLongWord(DataSize);
        // --> File Date
        Stream.PutInt64(DateTimeToUnix(DateTime));
        // --> Checksum
        Stream.PutLongWord(Checksum);

        // --> Init Vector (only if secure)
        if FPassword <> nil then
          Stream.PutUInt64(InitVector);

        // --> Record Data
        Stream.WriteBuffer(Data^, DataSize);
      except
        Exit(False);
      end;

      // (8) Update the position of the record table, which should be located exactly at the end of the written record.
      TableOffset := Stream.Position;
    finally
      Stream.Free;
    end;
  finally
    FreeMem(Data);
  end;

  // (9) Add new entry to the record table.
  NewEntry := CreateNewEntry;
  NewEntry.FKey := Key;
  NewEntry.FOffset := EntryOffset;
  NewEntry.FEntryType := EntryType;
  NewEntry.FOriginalSize := SourceSize;
  NewEntry.FPhysicalSize := DataSize;
  NewEntry.FDateTime := DateTime;
  NewEntry.FChecksum := Checksum;
  NewEntry.FSecure := FPassword <> nil;
  NewEntry.FInitVector := InitVector;

  SearchDirty := True;

  // (10) Write the new record table.
  if not WriteEntryTable then
    Exit(False);

  // (11) Write the new archive header.
  if not WriteHeader then
    Exit(False);

  Result := True;
end;

function TArchive.WriteEntry(const Key: UniString; const Source: Pointer; const SourceSize: Integer;
  const EntryType: TEntryType): Boolean;
begin
  Result := WriteEntry(Key, Source, SourceSize, EntryType, Now);
end;

function TArchive.WriteStream(const Key: UniString; const Stream: TStream; const EntryType: TEntryType;
  const DateTime: TDateTime): Boolean;
var
  Data: Pointer;
  DataSize: Integer;
begin
  if (FOpenMode = TOpenMode.ReadOnly) or (AccessType = TAccessType.Resource) then
    Exit(False);

  DataSize := Stream.Size - Stream.Position;
  Data := AllocMem(DataSize);

  try
    try
      Stream.ReadBuffer(Data^, DataSize);
    except
      Exit(False);
    end;

    Result := WriteEntry(Key, Data, DataSize, EntryType, DateTime);
  finally
    FreeMem(Data);
  end;
end;

function TArchive.WriteStream(const Key: UniString; const Stream: TStream; const EntryType: TEntryType): Boolean;
begin
  Result := WriteStream(Key, Stream, EntryType, Now);
end;

function TArchive.ReadEntry(const Key: UniString; out Data: Pointer; out DataSize: Integer): Boolean;
var
  PhysBuf: Pointer;
  PhysSize, DataOffset: Cardinal;
  Index: Integer;
  Stream: TStream;
  ChecksumOK: Boolean;
begin
  if FOpenMode = TOpenMode.Overwrite then
    Exit(False);

  // Find the record in the table to retrieve its offset in the archive.
  Index := IndexOf(Key);
  if Index = -1 then
    Exit(False);

  // If the record is encrypted, the password is required to proceed.
  if EntryList[Index].FSecure and (FPassword = nil) then
    Exit(False);

  // (1) Open the archive for reading data.
  Stream := CreateReadStream;
  if Stream = nil then
    Exit(False);

  try
    // Assign the original data size, for convenience.
    DataSize := EntryList[Index].FOriginalSize;

    // (2) Create temporary buffers, which will contain compressed data.
    PhysSize := EntryList[Index].FPhysicalSize;
    GetMem(PhysBuf, PhysSize);

    try
      // Calculate the position in the archive of the record's data block.
      DataOffset := EntryList[Index].FOffset + EntryDataOffset;
      if EntryList[Index].FSecure then
        Inc(DataOffset, EntrySecuritySize);

      // (3) Read the record from the archive.
      try
        // Move to the position of the data block in the archive.
        Stream.Position := DataOffset;

        // Read the record's data from the archive.
        Stream.ReadBuffer(PhysBuf^, PhysSize);
      except
        Exit(False);
      end;

      // (4) If the record is secure, decrypt the data before decompression.
      if EntryList[Index].FSecure then
        DecryptDataBlock(PhysBuf, PhysSize, EntryList[Index].InitVector);

      // (5) Decompress the record's data to retrieve original block.
      if not DecompressDataBlock(PhysBuf, PhysSize, Data, DataSize) then
        Exit(False);
    finally
      FreeMem(PhysBuf);
    end;
  finally
    Stream.Free;
  end;

  // (6) Validate the checksum.
  if FFormat = TFormat.ASDb then
    ChecksumOK := ChecksumMD5(Data, DataSize) = PMD5Checksum(EntryList[Index].FExtraField)^
  else
    ChecksumOK := ChecksumCRC32(Data, DataSize) = EntryList[Index].FChecksum;

  if not ChecksumOK then
  begin
    FreeMemAndNil(Data);
    DataSize := 0;
    Exit(False);
  end;

  Result := True;
end;

function TArchive.ReadStream(const Key: UniString; Stream: TStream): Boolean;
var
  Data: Pointer;
  DataSize: Integer;
begin
  if not ReadEntry(Key, Data, DataSize) then
    Exit(False);

  try
    try
      Stream.WriteBuffer(Data^, DataSize);
    except
      Exit(False);
    end;
  finally
    FreeMem(Data);
  end;

  Result := True;
end;

function TArchive.ReadMemStream(const Key: UniString; const MemStream: TMemoryStream): Boolean;
var
  PhysBuf: Pointer;
  PhysSize: Integer;
  Index: Integer;
  Stream: TStream;
  DataOffset: Cardinal;
begin
  if FOpenMode = TOpenMode.Overwrite then
    Exit(False);

  // Find the record in the table to retrieve its offset in the archive.
  Index := IndexOf(Key);
  if Index = -1 then
    Exit(False);

  // If the record is encrypted, the password is required to proceed.
  if EntryList[Index].FSecure and (FPassword = nil) then
    Exit(False);

  // (1) Open the archive for reading only.
  Stream := CreateReadStream;
  if Stream = nil then
    Exit(False);

  try
    // Assign the original data size, for convenience.
    MemStream.SetSize(EntryList[Index].FOriginalSize);

    // Create temporary buffers, which will contain compressed data.
    PhysSize := EntryList[Index].PhysicalSize;
    GetMem(PhysBuf, PhysSize);

    try
      // Calculate the position in the archive of the record's data block.
      DataOffset := EntryList[Index].FOffset + EntryDataOffset;
      if EntryList[Index].FSecure then
        Inc(DataOffset, EntrySecuritySize);

      // (2) Read the record from the archive.
      try
        // Move to the position of the data block in the archive.
        Stream.Position := DataOffset;

        // Read the record's data from the archive.
        Stream.ReadBuffer(PhysBuf^, PhysSize);
      except
        Exit(False);
      end;

      // If the record is secure, decrypt the data before decompression.
      if EntryList[Index].FSecure then
        DecryptDataBlock(PhysBuf, PhysSize, EntryList[Index].InitVector);

      // (3) Decompress the record's data to retrieve original block.
      if not DecompressToMemStream(PhysBuf, PhysSize, MemStream) then
        Exit(False);
    finally
      FreeMem(PhysBuf);
    end;
  finally
    Stream.Free;
  end;

  // (4) Calculate and verify the checksum to verify that the data is genuine.
  if ChecksumCRC32(MemStream.Memory, MemStream.Size) <> EntryList[Index].FChecksum then
  begin
    MemStream.Clear;
    Exit(False);
  end;

  Result := True;
end;

function TArchive.RenameEntry(const Key, NewKey: UniString): Boolean;
var
  Index: Integer;
begin
  if (FOpenMode <> TOpenMode.Update) or (AccessType <> TAccessType.AnyFile) or (FFormat <> TFormat.PXLA) then
    Exit(False);

  Index := IndexOf(Key);
  if (Index = -1) or (IndexOf(NewKey) <> -1) then
    Exit(False);

  EntryList[Index].FKey := NewKey;
  Result := WriteEntryTable;

  SearchDirty := True;
end;

function TArchive.RemoveEntry(const Key: UniString): Boolean;
var
  NewEntries: array of TRecordEntry;
  InStream, OutStream: TFileStream;
  TempFileName: StdString;
  I, Index, NewIndex: Integer;
  TempData: Pointer;
  TempDataSize: Integer;
  NewTableOffset: Int64;
begin
  if (FOpenMode <> TOpenMode.Update) or (AccessType = TAccessType.Resource) or (FFormat <> TFormat.PXLA) then
    Exit(False);

  // (1) Retrieve record index.
  Index := IndexOf(Key);
  if Index = -1 then
    Exit(False);

  // (2) Open the source archive for reading only.
  try
    InStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  except
    Exit(False);
  end;

  try
    // (3) Create temporary archive for writing.
    TempFileName := ExtractFilePath(FFileName) + TempFileText;

    try
      OutStream := TFileStream.Create(TempFileName, fmCreate or fmShareExclusive);
    except
      Exit(False);
    end;

    try
      // Write new tentative archive header.
      // --> Signature
      OutStream.PutByte(Ord('P'));
      OutStream.PutByte(Ord('X'));
      OutStream.PutByte(Ord('L'));
      OutStream.PutByte(Ord('0'));
      // --> Record Count
      OutStream.PutLongInt(Length(NewEntries) - 1);
      // --> Table Offset
      OutStream.PutLongWord(ArchiveHeaderSize);

      // Copy records from the source archive to destination, without modifying their contents.
      SetLength(NewEntries, Length(EntryList) - 1);

      for I := 0 to Length(NewEntries) - 1 do
        NewEntries[I] := nil;

      try
        NewIndex := 0;

        for I := 0 to Length(EntryList) - 1 do
        if (I <> Index) then
        begin
          // Copy the record's contents and update its offset.
          NewEntries[NewIndex] := TRecordEntry.CreateClone(EntryList[I]);
          NewEntries[NewIndex].FOffset := OutStream.Position;

          // Allocate the memory to hold the entire's record block, including its header and compressed data.
          TempDataSize := NewEntries[NewIndex].FPhysicalSize + EntryDataOffset;
          if (NewEntries[NewIndex].FSecure) then
            Inc(TempDataSize, EntrySecuritySize);

          TempData := AllocMem(TempDataSize);
          try
            try
              // Read the data from source archive.
              InStream.Position := EntryList[I].FOffset;
              InStream.ReadBuffer(TempData^, TempDataSize);

              // Write the data to the destination archive.
              OutStream.WriteBuffer(TempData^, TempDataSize);
            except
              Exit(False);
            end;
          finally
            FreeMem(TempData);
          end;

          Inc(NewIndex);
        end;

        NewTableOffset := OutStream.Position;

        // (4) Write the new record table and update destination archive header.
        try
          for I := 0 to Length(NewEntries) - 1 do
          begin
            // --> Key
            OutStream.PutMediumString(NewEntries[I].Key);
            // --> Offset
            OutStream.PutLongWord(NewEntries[I].Offset);
          end;

          // Write an updated value for the record table offset.
          OutStream.Position := 4;
          // --> Entry Count
          OutStream.PutLongInt(Length(NewEntries));
          // --> Table Offset
          OutStream.PutLongWord(NewTableOffset);
        except
          Exit(False);
        end;
      finally
        for I := Length(NewEntries) - 1 downto 0 do
          NewEntries[I].Free;

        SetLength(NewEntries, 0);
      end;
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;

  try
    DeleteFile(FFileName);
    RenameFile(TempFileName, FFileName);
  except
    Exit(False);
  end;

  Result := ReadDetailedEntryList;

  SearchDirty := True;
end;

procedure TArchive.InitSearchList;
var
  NeedCount, I: Integer;
begin
  NeedCount := Length(EntryList);

  if Length(SearchList) <> NeedCount then
    SetLength(SearchList, NeedCount);

  for I := 0 to NeedCount - 1 do
    SearchList[I] := I;
end;

procedure TArchive.SearchListSwap(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := SearchList[Index1];

  SearchList[Index1] := SearchList[Index2];
  SearchList[Index2] := TempValue;
end;

function TArchive.SearchListCompare(const Value1, Value2: Integer): Integer;
begin
  Result := CompareText(EntryList[Value1].Key, EntryList[Value2].Key);
end;

function TArchive.SearchListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := SearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (SearchListCompare(SearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (SearchListCompare(SearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      SearchListSwap(Left, Right);
  end;

  SearchListSwap(Start, Right);

  Result := Right;
end;

procedure TArchive.SearchListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := SearchListSplit(Start, Stop);

    SearchListSort(Start, SplitPt - 1);
    SearchListSort(SplitPt + 1, Stop);
  end;
end;

procedure TArchive.UpdateSearchList;
var
  ListCount: Integer;
begin
  InitSearchList;

  ListCount := Length(SearchList);
  if ListCount > 1 then
    SearchListSort(0, ListCount - 1);

  SearchDirty := False;
end;

function TArchive.IndexOf(const Key: UniString): Integer;
var
  Lo, Hi, Mid, Res: Integer;
begin
  if SearchDirty then
    UpdateSearchList;

  Result := -1;

  Lo := 0;
  Hi := Length(SearchList) - 1;

  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    Res := CompareText(EntryList[SearchList[Mid]].Key, Key);

    if Res = 0 then
    begin
      Result := SearchList[Mid];
      Break;
    end;

    if Res > 0 then
      Hi := Mid - 1
    else
      Lo := Mid + 1;
  end;
end;

{$ENDREGION}
{$REGION 'TArchivePassowrd'}

constructor TArchivePassword.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TArchivePassword.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TArchivePassword.NextID: Cardinal;
begin
  Result := CurrentID;
  Inc(CurrentID);
end;

function TArchivePassword.IndexOf(const ItemID: Cardinal): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(Items) - 1 do
    if Items[I].ItemID = ItemID then
      Exit(I);

  Result := -1;
end;

function TArchivePassword.Subscribe(const Callback: TPasswordRequestCallback): Cardinal;
var
  Index: Integer;
begin
  Result := NextID;

  Index := Length(Items);
  SetLength(Items, Index + 1);

  Items[Index].Callback := Callback;
  Items[Index].ItemID := Result;
end;

procedure TArchivePassword.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(Items)) then
    Exit;

  for I := Index to Length(Items) - 2 do
    Items[I] := Items[I + 1];

  SetLength(Items, Length(Items) - 1);
end;

procedure TArchivePassword.Unsubscribe(const EventID: Cardinal);
begin
  Unauthorize;
  Remove(IndexOf(EventID));
end;

function TArchivePassword.Authorize(const Sender: TObject; const Archive: TArchive): Boolean;
var
  I: Integer;
begin
  Authorized := False;
  AuthArchive := Archive;
  AuthSender := Sender;
  AuthIndex := -1;

  for I := 0 to Length(Items) - 1 do
  begin
    Items[I].Callback(AuthSender, AuthArchive, TRequestType.ProvideKey);

    if Authorized then
    begin
      AuthIndex := I;
      Break;
    end;
  end;

  Result := Authorized;
  if not Result then
  begin
    AuthArchive := nil;
    AuthSender := nil;
  end;
end;

procedure TArchivePassword.Unauthorize;
begin
  if Authorized and (AuthIndex >= 0) and (AuthIndex < Length(Items)) then
    Items[AuthIndex].Callback(AuthSender, AuthArchive, TRequestType.BurnKey);

  Authorized := False;
  AuthIndex := -1;
  AuthArchive := nil;
  AuthSender := nil;
end;

procedure TArchivePassword.ProvideKey(const Key: Pointer);
begin
  if AuthArchive <> nil then
  begin
    AuthArchive.Password := Key;
    Authorized := True;
  end;
end;

{$ENDREGION}

initialization
  TArchive.AccessType := TArchive.TAccessType.AnyFile;
  TArchive.AccessInstance := 0;

  TArchivePassword.FCurrent := TArchivePassword.Create;

finalization
  FreeAndNil(TArchivePassword.FCurrent);

end.
