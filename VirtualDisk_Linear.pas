unit VirtualDisk_Linear;
//{$message Error 'Don''t Use'}
{x$DEFINE USE_STANDARD_STREAM}
{x$DEFINE VERIFY_WRITES}
{x$DEFINE SMALL_TEST}
{x$DEFINE USE_VAT}
interface

uses
  standardlist, clusteredpointerlist,helpers.stream, systemx, typex, classes, sysutils, generics.collections.fixed, betterobject, sharedobject, MultiBufferMemoryFileStream, debug;

const
{$IFNDEF SMALL_TEST}
  BIG_BLOCK_SIZE_IN_BLOCKS = 65536;
  MAX_BLOCKS_IN_VAT = 65536*8;

{$ELSE}
  BIG_BLOCK_SIZE_IN_BLOCKS = 4;
  MAX_BLOCKS_IN_VAT = 32;
{$ENDIF}

type
{$IFDEF USE_STANDARD_STREAM}
  //TVDStream = TFileStream;
{$ELSE}
  TVDStream = TMultiBufferMemoryFileStream;
{$ENDIF}

{$IFDEF USE_VAT}
  TVirtualAddressRecord = packed record
    Marker: cardinal;
    StartingBlock: cardinal;
    Address: int64;
  end;

  PVirtualAddressRecord = ^TVirtualAddressRecord;

  TVirtualAddressTable = packed record
    //Version: cardinal;
    Version: int64;
    NextPhysicalAddress: int64;
    table: array[0..MAX_BLOCKS_IN_VAT] of TVirtualAddressRecord;
    marker: int64;
    procedure Init;
    procedure Allocate(LBACount: int64);
    function GetTableEntryForLBA(lba: int64): PVirtualAddressRecord;
    procedure ReadFromStream(s: TStream);
    procedure FlushToStream(s: TStream);



  end;
{$ENDIF}

  TAbstractVirtualdisk = class(TSharedObject)
  strict
  private
    function GetIdentifier: string;
    procedure SetIdentifier(const Value: string);
  protected
    function TailSize: int64; protected
    FSize: int64;
{$ifdef use_vat}
    vat: TVirtualAddressTable;
{$endif}
    FBlockSize: nativeint;
    FRequestedSize: int64;
    FIdentifier: string;
    procedure SetSize(const Value: int64);virtual;
    procedure OuterSetBlockSize(const Value: nativeint);
    procedure SetBlockSize(const Value: nativeint);virtual;abstract;
    function BlockMargin: int64;
{$ifdef use_vat}
    procedure LoadVat;virtual;abstract;
    procedure SaveVat;virtual;abstract;
{$endif}
    function NewPhysicalBlockAddress: int64;virtual;abstract;
  public
    constructor Create;override;
    property BlockSize: nativeint read FBlockSize write OuterSetBlockSize;
    property Size: int64 read FSize write SetSize;
    function BlockCount: int64;
    function TailStartAddr: int64;
    function VirtualToPhysicalAddr(addr: int64): int64;
    procedure ReadBlock(lba: int64; p: pbyte);virtual;abstract;
    function ReadBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;virtual;abstract;
    function WriteBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;virtual;abstract;
    procedure WriteBlock(lba: int64; p: pbyte);virtual;abstract;
    procedure GuaranteeReadBlocks(lba: int64; cnt: nativeint; p: pbyte);
    procedure GuaranteeWriteBlocks(lba: int64; cnt: nativeint; p: pbyte);


    procedure GrowIfNeededBlock(lba: int64);virtual;abstract;
    procedure GrowIfNeededAddr(size: int64);virtual;abstract;
    procedure WriteData(addr: int64; cnt: nativeint;p:pbyte);virtual;abstract;
    procedure ReadData(addr: int64; cnt: nativeint; p: pbyte);virtual;abstract;
    procedure ChangeSingleByte(iAddr: int64; b: byte);virtual;abstract;
    function ReadSingleByte(iAddr: int64): byte;virtual;abstract;
    procedure Preallocate(lba: int64);virtual;abstract;
    property Identifier: string read GetIdentifier write SetIdentifier;
  end;



  TFileBasedVirtualdisk = class(TAbstractVirtualDisk)
  protected
    function GetfileName: string;virtual;abstract;
    procedure SetFileNAme(const Value: string);virtual;abstract;
  public
    property FileName: string read GetfileName write SetFileNAme;
  end;



  TFileBlock = class(TBetterOBject, IIndexable<int64>)
  protected
    function GetIndexValue: int64;
  public
    data: pointer;
    size: nativeint;
    addr: int64;
    dirty: boolean;

    procedure Read(FStream: TStream);
    procedure Write(FStream: TStream);
  end;

  TBLockListList = TStandardList<int64, TFileBlock>;

  TBlockLevelCacheList = class(TBetterObject)
  protected
    FList: TBlockListList;
  public
    constructor CReate;override;
    destructor Destroy;override;
    procedure AddBlock(b: TFIleBlock);
    function IndexOfBlock(addr: int64): nativeint;
    function HasBlock(addr: int64): boolean;
  end;


  TVirtualDisk = class(TFileBasedVirtualdisk)
  private
    procedure ReadBlock(lba: int64; p: pbyte);
    procedure WriteBlock(lba: int64; p: pbyte);
  protected


    procedure SetSize(const Value: int64);override;
    function GetfileName: string;override;
    procedure SetFileNAme(const Value: string);override;
    procedure SetBlockSize(const Value: nativeint);override;
{$ifdef use_vat}
    procedure LoadVat;override;
    procedure SaveVat;override;
    function NewPhysicalBlockAddress: int64;override;
{$endif}
  public
    FStream: TVDStream;
    constructor Create;override;
    destructor Destroy;override;
    function ReadBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;override;
    function WriteBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;override;

    procedure GrowIfNeededBlock(lba: int64);override;
    procedure GrowIfNeededAddr(size: int64);override;
    procedure WriteData(addr: int64; cnt: nativeint;p:pbyte);override;
    procedure ReadData(addr: int64; cnt: nativeint; p: pbyte);override;
    procedure ChangeSingleByte(iAddr: int64; b: byte);override;
    function ReadSingleByte(iAddr: int64): byte;override;
    procedure Preallocate(lba: int64);override;

  end;

  TVirtualDiskList = class(TList<TVirtualDisk>);



implementation

{ TVirtualDisk }

procedure TVirtualDisk.ChangeSingleByte(iAddr: int64; b: byte);
var
  block: array[0..511] of byte;
  iOFF: int64;
begin
  self.ReadBlock(iAddr div BlockSize, @block[0]);
  iOFF := iAddr mod BlockSize;

  block[iOFF] := b;
  self.WriteBlock(iAddr div BlockSize, @block[0]);



end;

constructor TVirtualDisk.Create;
begin
  inherited;
  FBlockSize := 512;



end;

destructor TVirtualDisk.Destroy;
begin
//  Debug.Log('Stream is Destroyed.  Size is '+inttostr(FStream.size));
  FStream.Free;
  FStream := nil;
  inherited;
end;

function TVirtualDisk.GetfileName: string;
begin
  lock;
  try
    if FStream = nil then
      result := ''
    else
      result := FStream.FileName;

  finally
    unlock;
  end;
end;

procedure TVirtualDisk.GrowIfNeededBlock(lba: int64);
var
  m: Pbyte;
  t: int64;
begin
  exit;
(*  if (lba+1)*Blocksize < FStream.Size then
    exit;


  t := FStream.Size div BlockSize;//start at last block

  m := GetMemory(BlockSize);
  try
    Debug.Log('Growing stream to:'+inttostr((t+1)*blocksize));
    Debug.Log('Position is now '+inttostr(FStream.position));
    while t <=lba do begin


      FillMem(m, BlockSize, 0);
      FStream.Seek(t*blocksize, soBeginning);

      Stream_GuaranteeWrite(FStream, m, BlockSize);
      inc(t);
    end;
    Debug.Log('Stream is now '+inttostr(FStream.Size)+' bytes in size.');
    Debug.Log('Position is now '+inttostr(FStream.position));

  finally
    freememory(m);
  end;*)

end;

{$ifdef use_vat}
procedure TVirtualDisk.LoadVat;
begin

  vat.ReadFromStream(FStream);
  if FStream.Size < sizeof(vat) then begin
    vat.FlushToStream(FStream);
  end;

end;
{$endif}

{$ifdef use_vat}
function TVirtualDisk.NewPhysicalBlockAddress: int64;
begin
  result := vat.NextPhysicalAddress;
  if result = 0 then
  result := sizeof(vat);
  vat.NextPhysicalAddress := result+(BlockSize*BIG_BLOCK_SIZE_IN_BLOCKS);



end;
{$endif}

procedure TVirtualDisk.GrowIfNeededAddr(size: int64);
begin
  GrowIfNeededBlock((size div blocksize)+1);
end;


procedure TVirtualDisk.Preallocate(lba: int64);
begin
  exit;
//  self.BlockSize := blocksize;
  GrowIfNeededBlock(lba);
{$IFNDEF USE_STANDARD_STREAM}
  Fstream.Flush;
{$ENDIF}
  FileName := FileName;

end;

procedure TVirtualDisk.ReadBlock(lba: int64; p: pbyte);
begin
  ReadBlocks(lba, 1, p);
end;

function TVirtualDisk.ReadBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;
var
  actual_addr: int64;
  end_actual_addr: int64;
begin
  result := cnt;

  lock;
  try
    GrowIfNeededBlock(lba+(cnt-1));
    actual_addr := VirtualToPhysicalAddr(lba*FBlockSize);
    end_actual_addr := VirtualToPhysicalAddr((lba+cnt) * FBlockSize);

    //if the blocks are not all consecutive, we'll need to reduce the number of blocks we can write
    if (end_actual_addr - actual_addr) <> (cnt * BlockSize) then begin
      end_actual_addr := (((lba div BIG_BLOCK_SIZE_IN_BLOCKS)+1) * BIG_BLOCK_SIZE_IN_BLOCKS) * BlockSize;
      result := (end_actual_addr - actual_addr) div BlockSize;
      cnt := result;
      if (end_actual_addr - actual_addr) <> (cnt * BlockSize) then begin
        raise ECritical.create('Block segment fault failure.');
      end;
    end;
    //Debug.Log(inttohex(lba*FBlocksize, 2)+'<-actual_addr(rb)->'+inttohex(actual_addr, 2));


    if (lba*BlockSize) >= (Size+Blocksize) then
      raise ECritical.create('Address beyond the size of the disk! '+inttohex(lba*BlockSize,2));


    FStream.Seek(actual_addr,0);
    Stream_GuaranteeRead(FStream, p, FBlockSize*cnt);

  finally
    Unlock;
  end;

end;

procedure TVirtualDisk.ReadData(addr: int64; cnt: nativeint; p: pbyte);
begin
  if (cnt mod BlockSize) <> 0 then
    raise ECRitical.create('Cnt is not a multiple of block size');

  if (addr mod BlockSize) <> 0 then
    raise ECRitical.create('addr is not a multiple of block size');


  GuaranteeReadBlocks(addr div BlockSize, cnt div BlockSize, p);

end;

function TVirtualDisk.ReadSingleByte(iAddr: int64): byte;
var
  block: array[0..511] of byte;
  iOFF: int64;
begin
  self.ReadBlock(iAddr div BlockSize, @block[0]);
  iOFF := iAddr mod BlockSize;

  result := block[iOFF];
//  self.WriteBlock(iAddr div BlockSize, @block[0]);

end;

{$ifdef use_vat}
procedure TVirtualDisk.SaveVat;
begin
  vat.FlushToStream(FStream);
end;
{$endif}

procedure TVirtualDisk.SetBlockSize(const Value: nativeint);
begin
  if filename <> '' then
    raise Exception.Create('set block size only when filename not set');

  FBlockSize := Value;
end;

procedure TVirtualDisk.SetFileNAme(const Value: string);
var
  c: byte;
begin
  lock;
  try
    if assigned(FStream) then begin
      FStream.Free;
      FStream := nil;
    end;

    if fileexists(value) then
      FStream := TVDStream.Create(value, fmOpenReadWrite+fmShareExclusive)
    else
      FStream := TVDStream.Create(value, fmCreate);

{$IFNDEF USE_STANDARD_STREAM}
    FStream.AllowReadPastEOF := true;
    FStream.BufferSize := 256000000;
    FStream.BufferSEgments := 256;
    FStream.DisableLookAhead := true;
{$ENDIF}


{$ifdef use_vat}
    LoadVat;
{$else}
  if FStream <> nil then begin
    if FStream.Size < size then begin
      FStream.Seek(size-1, 0);
      c := $55;
      FStream.Write(c, 1);
      FStream.Flush;
    end;
  end;
{$endif}



//    Debug.Log('Stream is CReated.  Size is '+inttostr(FStream.size));


  finally
    unlock;
  end;
end;

procedure TVirtualDisk.SetSize(const Value: int64);
var
  c: byte;
begin
  inherited;
  {$ifndef use_vat}
  if FStream <> nil then begin
    if FStream.Size < size then begin
      FStream.Seek(value-1, 0);
      c := $55;
      FStream.Write(@c, 1);
      FStream.Flush;
    end;
  end;

  {$endif}
end;

procedure TVirtualDisk.WriteBlock(lba: int64; p: pbyte);
begin
  WriteBlocks(lba, 1, p);
end;

function TVirtualDisk.WriteBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;
var
  actual_addr: int64;
  end_actual_addr: int64;
begin
  result := cnt;

  lock;
  try
    GrowIfNeededBlock(lba+(cnt-1));
    actual_addr := VirtualToPhysicalAddr(lba*FBlockSize);
    end_actual_addr := VirtualToPhysicalAddr((lba+cnt) * FBlockSize);

    //if the blocks are not all consecutive, we'll need to reduce the number of blocks we can write
    if (end_actual_addr - actual_addr) <> (cnt * BlockSize) then begin
      end_actual_addr := (((lba div BIG_BLOCK_SIZE_IN_BLOCKS)+1) * BIG_BLOCK_SIZE_IN_BLOCKS) * BlockSize;
      result := (end_actual_addr - actual_addr) div BlockSize;
      cnt := result;
      if (end_actual_addr - actual_addr) <> (cnt * BlockSize) then begin
        raise ECritical.create('Block segment fault failure.');
      end;
    end;
    //Debug.Log(inttohex(lba*FBlocksize, 2)+'<-actual_addr(wb)->'+inttohex(actual_addr, 2));


    if (lba*BlockSize) >= (Size+Blocksize) then
      raise ECritical.create('Address beyond the size of the disk! '+inttohex(lba*BlockSize,2));


    FStream.Seek(actual_addr,0);
    Stream_GuaranteeWrite(FStream, p, FBlockSize*cnt);

  finally
    Unlock;
  end;

end;

procedure TVirtualDisk.WriteData(addr: int64; cnt: nativeint; p: pbyte);
begin
  if (cnt mod BlockSize) <> 0 then
    raise ECRitical.create('Cnt is not a multiple of block size');

  if (addr mod BlockSize) <> 0 then
    raise ECRitical.create('addr is not a multiple of block size');


  GuaranteeWriteBlocks(addr div BlockSize, cnt div BlockSize, p);
end;

{ TFileBasedVirtualdisk }


{ TFileBlock }

function TFileBlock.GetIndexValue: int64;
begin
  result := addr;
end;

procedure TFileBlock.Read(FStream: TStream);
begin
  FStream.Seek(self.addr, 0);
  Stream_GuaranteeRead(FStream, data, size);



end;

procedure TFileBlock.Write(FStream: TStream);
begin
  FStream.Seek(self.addr, 0);
  Stream_GuaranteeWrite(FStream, data, size);

end;

{ TBlockLevelCacheList }

procedure TBlockLevelCacheList.AddBlock(b: TFIleBlock);
begin
  self.FList.Add(b);

end;

constructor TBlockLevelCacheList.CReate;
begin
  inherited;
  FList := TBLockListList.Create;
end;

destructor TBlockLevelCacheList.Destroy;
begin
  FList.free;
  FList := nil;

  inherited;
end;

function TBlockLevelCacheList.HasBlock(addr: int64): boolean;
begin
  result := FList.Has(addr);

end;

function TBlockLevelCacheList.IndexOfBlock(addr: int64): nativeint;
begin
  result := fList.IndexOf(addr);
end;



{ TAbstractVirtualdisk }

function TAbstractVirtualdisk.BlockCount: int64;
begin
  result := Size div BlockSize;
end;

function TAbstractVirtualdisk.BlockMargin: int64;
begin
  result := BlockSize*64;
end;

constructor TAbstractVirtualdisk.Create;
begin
  inherited;
  FBlockSize := 512;
{$ifdef use_vat}
  vat.init;
{$endif}
end;

function TAbstractVirtualdisk.GetIdentifier: string;
begin
  Lock;
  try
    result := FIdentifier;
  finally
    Unlock;
  end;
end;

procedure TAbstractVirtualdisk.GuaranteeReadBlocks(lba: int64; cnt: nativeint;
  p: pbyte);
var
  iJustRead, iRead: nativeint;
begin
  //Debug.Log('GRB:'+inttostr(lba)+','+inttostr(cnt));
  iRead := 0;
  while iRead < cnt do begin
    iJustRead := ReadBlocks(lba, cnt, @p[iRead*BlockSize]);
    inc(lba, iJustRead);
    dec(cnt, iJustRead);
  end;


end;

procedure TAbstractVirtualdisk.GuaranteeWriteBlocks(lba: int64; cnt: nativeint;
  p: pbyte);
var
  iJustWrote, iWrote: nativeint;
begin
  //Debug.Log('GWB:'+inttostr(lba)+','+inttostr(cnt));
  iWrote := 0;
  while iWrote < cnt do begin
    iJustWrote := WriteBlocks(lba, cnt, @p[iWrote*BlockSize]);
    inc(lba, iJustWrote);
    dec(cnt, iJustWrote);
  end;
end;

procedure TAbstractVirtualdisk.OuterSetBlockSize(const Value: nativeint);
begin
  FBlockSize := Value;
  FSize := (int64(FRequestedSize) div int64(BlockSize)) * int64(FBlockSize);
  SetBlockSize(value);
end;





procedure TAbstractVirtualdisk.SetIdentifier(const Value: string);
begin
  Lock;
  try
    FIdentifier := 'iqn-2008-08.com.digitaltundrallc:'+systemx.GetComputerName+'-'+value;
  finally
    Unlock;
  end;
end;

procedure TAbstractVirtualdisk.SetSize(const Value: int64);
begin
  FRequestedSize := value;
  fSize := (FRequestedSize div BlockSize) * BlockSize;

end;

function TAbstractVirtualdisk.TailStartAddr: int64;
begin
  result := Size - TailSize;
end;

function TAbstractVirtualDisk.TailSize: int64;
begin
  result := 8192 * BlockSize;
end;

function TAbstractVirtualdisk.VirtualToPhysicalAddr(addr: int64): int64;
{$ifndef use_vat}
begin
  result := addr;
end;
{$else}
var
  r: PVirtualAddressRecord;
  tindex: nativeint;
begin

  result := addr;
  tindex := addr div (BlockSize* BIG_BLOCK_SIZE_IN_BLOCKS);
  r := vat.GetTableEntryForLBA(addr div blocksize);

  if r.address = 0 then begin
    r.Address := NewPhysicalBlockAddress;
    r.Marker := $55555555;
    r.StartingBlock := tindex * BIG_BLOCK_SIZE_IN_BLOCKS ;
    SaveVat;
  end;

  result := r.Address+(addr mod (BIG_BLOCK_SIZE_IN_BLOCKS * BlockSize));


end;
{$endif}

{ TVirtualAddressTable }


{$ifdef use_vat}
procedure TVirtualAddressTable.FlushToStream(s: TStream);
begin
  s.Seek(0,0);
  Stream_GuaranteeWrite(s, @self, sizeof(self));
end;

function TVirtualAddressTable.GetTableEntryForLBA(
  lba: int64): PVirtualAddressRecord;
var
  t: nativeint;
begin
  lba := lba shr 16;
  result := @table[lba];


end;

procedure TVirtualAddressTable.Init;
begin
  marker := $5555555555555555;
end;

procedure TVirtualAddressTable.ReadFromStream(s: TStream);
begin
  s.Seek(0,0);
  Stream_GuaranteeRead(s, @self, sizeof(self));
end;
{$endif}

end.

