unit VirtualDisk;
{x$DEFINE USE_STANDARD_STREAM}
{x$DEFINE VERIFY_WRITES}
{x$DEFINE SMALL_TEST}
{$DEFINE USE_VAT}
interface

uses
  numbers, virtualdiskconstants, standardlist, clusteredpointerlist,helpers.stream, systemx, typex, classes, sysutils, generics.collections.fixed, betterobject, sharedobject, MultiBufferMemoryFileStream, debug;

const
{$IFNDEF SMALL_TEST}
  LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS = 65536;
  LEGACY_MAX_BLOCKS_IN_VAT = 65536*8;

{$ELSE}
//  BIG_BLOCK_SIZE_IN_BLOCKS = 4;
//  MAX_BLOCKS_IN_VAT = 32;
{$ENDIF}

type
{$IFDEF USE_STANDARD_STREAM}
  TVDStream = TFakeMBFS;
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
    table: array[0..LEGACY_MAX_BLOCKS_IN_VAT] of TVirtualAddressRecord;
    marker: int64;
    procedure Init;
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


  TVirtualDisk_SimpleNonLinear = class(TFileBasedVirtualdisk)
  private
    FCacheSize: int64;
    FJunk: int64;
    procedure SetCAcheSize(const Value: int64);
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
    procedure ReadBlock(lba: int64; p: pbyte);override;
    procedure WriteBlock(lba: int64; p: pbyte);override;
    function ReadBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;override;
    function WriteBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;override;
  public
    FStream: TVDStream;
    constructor Create;override;
    destructor Destroy;override;
    procedure FlexWrite(const addr: int64; const cnt: nativeint; p:pbyte);
    procedure FlexRead(const addr:int64;const cnt: nativeint; p:pbyte);

    procedure GrowIfNeededBlock(lba: int64);override;
    procedure GrowIfNeededAddr(size: int64);override;
    procedure WriteData(addr: int64; cnt: nativeint;p:pbyte);override;
    procedure ReadData(addr: int64; cnt: nativeint; p: pbyte);override;
    procedure ChangeSingleByte(iAddr: int64; b: byte);override;
    function ReadSingleByte(iAddr: int64): byte;override;
    procedure Preallocate(lba: int64);override;
    property CacheSize: int64 read FCacheSize write SetCAcheSize;
    property CachedStripes: int64 read FJunk write FJunk;
  end;

  //TVirtualDisk = class(TVirtualDisk_SimpleNonLinear);

  //TVirtualDiskList = class(TList<TVirtualDisk>);



implementation

{ TVirtualDisk }

procedure TVirtualDisk_SimpleNonLinear.ChangeSingleByte(iAddr: int64; b: byte);
var
  block: array[0..(BLOCKSIZE-1)] of byte;
  iOFF: int64;
begin
  self.ReadBlock(iAddr div BlockSize, @block[0]);
  iOFF := iAddr mod BlockSize;

  block[iOFF] := b;
  self.WriteBlock(iAddr div BlockSize, @block[0]);



end;

constructor TVirtualDisk_SimpleNonLinear.Create;
begin
  inherited;
  FCacheSize := 32*MEGA;


end;

destructor TVirtualDisk_SimpleNonLinear.Destroy;
begin
//  Debug.Log('Stream is Destroyed.  Size is '+inttostr(FStream.size));
  FStream.Free;
  FStream := nil;
  inherited;
end;

procedure TVirtualDisk_SimpleNonLinear.FlexRead(const addr: int64;
  const cnt: nativeint; p: pbyte);
var
  pblock: pbyte;
  iCan, itot,ipos, iEnd, iOff: int64;
begin
  pblock := getmemory(blocksize);
  try
    iPos :=addr;
    iEnd := ipos + cnt;
    iTot := 0;
    while iPos < (iEnd) do begin
      iOff := iPos mod blocksize;
      iCan := lesserof(blocksize - iOff, iEnd-iPos);
      readblock(addr div blocksize, pblock);
      movemem32(@p[iTot], @pblock[iOff], iCAn);
      inc(iTot, iCan);
      inc(iPos, iCAn);
    end;


  finally
    freememory(pblock);
  end;


end;

procedure TVirtualDisk_SimpleNonLinear.FlexWrite(const addr: int64;
  const cnt: nativeint; p: pbyte);
var
  pblock: pbyte;
  iCan, itot,ipos, iEnd, iOff: int64;
begin
  pblock := getmemory(blocksize);
  try
    iPos :=addr;
    iEnd := ipos + cnt;
    iTot := 0;
    while iPos < (iEnd) do begin
      iOff := iPos mod blocksize;
      iCan := lesserof(blocksize - iOff, iEnd-iPos);
      //we need to read the block before writing it if the offset is in the middle somewhere
      if (iOff > 0) or ((iEnd-iPos) < 512) then
        readblock(addr div blocksize, pblock);
      movemem32(@pblock[iOff], @p[iTot], iCAn);
      writeblock(addr div blocksize, pblock);

      inc(iTot, iCan);
      inc(iPos, iCAn);
    end;


  finally
    freememory(pblock);
  end;
end;

function TVirtualDisk_SimpleNonLinear.GetfileName: string;
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

procedure TVirtualDisk_SimpleNonLinear.GrowIfNeededBlock(lba: int64);
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
procedure TVirtualDisk_SimpleNonLinear.LoadVat;
begin

  vat.ReadFromStream(FStream);
  if FStream.Size < sizeof(vat) then begin
    vat.FlushToStream(FStream);
  end;

end;
{$endif}

{$ifdef use_vat}
function TVirtualDisk_SimpleNonLinear.NewPhysicalBlockAddress: int64;
begin
  result := vat.NextPhysicalAddress;
  if result = 0 then
  result := sizeof(vat);
  vat.NextPhysicalAddress := result+(BlockSize*LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS);



end;
{$endif}

procedure TVirtualDisk_SimpleNonLinear.GrowIfNeededAddr(size: int64);
begin
  GrowIfNeededBlock((size div blocksize)+1);
end;


procedure TVirtualDisk_SimpleNonLinear.Preallocate(lba: int64);
begin
  exit;
//  self.BlockSize := blocksize;
  GrowIfNeededBlock(lba);
{$IFNDEF USE_STANDARD_STREAM}
  Fstream.Flush;
{$ENDIF}
  FileName := FileName;

end;

procedure TVirtualDisk_SimpleNonLinear.ReadBlock(lba: int64; p: pbyte);
begin
  ReadBlocks(lba, 1, p);
end;

function TVirtualDisk_SimpleNonLinear.ReadBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;
var
  actual_addr: int64;
  end_actual_addr: int64;
begin
  result := cnt;

  lock;
  try
    GrowIfNeededBlock(lba+(cnt-1));
    actual_addr := VirtualToPhysicalAddr(lba*BlockSize);
    end_actual_addr := VirtualToPhysicalAddr((lba+cnt) * BlockSize);

    //if the blocks are not all consecutive, we'll need to reduce the number of blocks we can write
    if (end_actual_addr - actual_addr) <> (cnt * BlockSize) then begin
      //change the end actual address
      begin
        //find the beginning of the actual_addr bigblock
        end_actual_addr := VirtualToPhysicalAddr(((lba div LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS) * LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS)*BlockSize);
        end_actual_addr := end_actual_addr + (LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS * BlockSize);
      end;

      result := (end_actual_addr - actual_addr) div BlockSize;
      cnt := result;
      if (end_actual_addr - actual_addr) <> (cnt * BlockSize) then begin
        raise ECritical.create('Block segment fault failure.');
      end;
    end;
//    Debug.Log(inttohex(lba*Blocksize, 2)+'<-actual_addr(rb)->'+inttohex(actual_addr, 2));


    if (lba*BlockSize) >= (Size+Blocksize) then
      raise ECritical.create('Address beyond the size of the disk! '+inttohex(lba*BlockSize,2));


    FStream.Seek(actual_addr,0);
    Stream_GuaranteeRead(FStream, p, BlockSize*cnt);

  finally
    Unlock;
  end;

end;

procedure TVirtualDisk_SimpleNonLinear.ReadData(addr: int64; cnt: nativeint; p: pbyte);
begin
  if (cnt mod BlockSize) <> 0 then
    raise ECRitical.create('Cnt is not a multiple of block size');

  if (addr mod BlockSize) <> 0 then
    raise ECRitical.create('addr is not a multiple of block size');


  GuaranteeReadBlocks(addr div BlockSize, cnt div BlockSize, p);

end;

function TVirtualDisk_SimpleNonLinear.ReadSingleByte(iAddr: int64): byte;
var
  block: array[0..(BLOCKSIZE-1)] of byte;
  iOFF: int64;
begin
  self.ReadBlock(iAddr div BlockSize, @block[0]);
  iOFF := iAddr mod BlockSize;

  result := block[iOFF];
//  self.WriteBlock(iAddr div BlockSize, @block[0]);

end;

{$ifdef use_vat}
procedure TVirtualDisk_SimpleNonLinear.SaveVat;
begin
  vat.FlushToStream(FStream);
end;
{$endif}

procedure TVirtualDisk_SimpleNonLinear.SetBlockSize(const Value: nativeint);
begin
  if filename <> '' then
    raise Exception.Create('set block size only when filename not set');

end;

procedure TVirtualDisk_SimpleNonLinear.SetCAcheSize(const Value: int64);
begin
  FCacheSize := Value;
{$IFNDEF USE_STANDARD_STREAM}
  if assigned(FStream) then
    FStream.BufferSize := FCacheSize;
{$ENDIF}

end;

procedure TVirtualDisk_SimpleNonLinear.SetFileNAme(const Value: string);
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
//    FStream.MinimumPrefetchSize := 999999999;
    FStream.AllowReadPastEOF := true;
    FStream.BufferSize := 256000000;
    FStream.BufferSEgments := 256;
    FStream.DisableLookAhead := true;
    FStream.BufferSize := FCacheSize;
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

procedure TVirtualDisk_SimpleNonLinear.SetSize(const Value: int64);
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

procedure TVirtualDisk_SimpleNonLinear.WriteBlock(lba: int64; p: pbyte);
begin
  WriteBlocks(lba, 1, p);
end;

function TVirtualDisk_SimpleNonLinear.WriteBlocks(lba: int64; cnt: nativeint; p: pbyte): nativeint;
var
  actual_addr: int64;
  end_actual_addr: int64;
begin
  result := cnt;

  lock;
  try
    GrowIfNeededBlock(lba+(cnt-1));
    actual_addr := VirtualToPhysicalAddr(lba*BlockSize);
    end_actual_addr := VirtualToPhysicalAddr((lba+cnt) * BlockSize);

    //if the blocks are not all consecutive, we'll need to reduce the number of blocks we can write
    if (end_actual_addr - actual_addr) <> (cnt * BlockSize) then begin
      //change the end actual address
      begin
        //find the beginning of the actual_addr bigblock
        end_actual_addr := VirtualToPhysicalAddr(((lba div LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS) * LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS)*BlockSize);
        end_actual_addr := end_actual_addr + (LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS * BlockSize);
      end;

      result := (end_actual_addr - actual_addr) div BlockSize;
      cnt := result;
      if (end_actual_addr - actual_addr) <> (cnt * BlockSize) then begin
        raise ECritical.create('Block segment fault failure.');
      end;
    end;
//    Debug.Log(inttohex(lba*Blocksize, 2)+'<-actual_addr(wb)->'+inttohex(actual_addr, 2));


    if (lba*BlockSize) >= (Size+Blocksize) then
      raise ECritical.create('Address beyond the size of the disk! '+inttohex(lba*BlockSize,2));


    FStream.Seek(actual_addr,0);
    Stream_GuaranteeWrite(FStream, p, BlockSize*cnt);

  finally
    Unlock;
  end;

end;

procedure TVirtualDisk_SimpleNonLinear.WriteData(addr: int64; cnt: nativeint; p: pbyte);
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

function TAbstractVirtualDisk.BlockCount: int64;
begin
  result := Size div BlockSize;
end;

function TAbstractVirtualDisk.BlockMargin: int64;
begin
  result := BlockSize*64;
end;

constructor TAbstractVirtualDisk.Create;
begin
  inherited;
{$ifdef use_vat}
  vat.init;
{$endif}
end;

function TAbstractVirtualDisk.GetIdentifier: string;
begin
  Lock;
  try
    result := FIdentifier;
  finally
    Unlock;
  end;
end;

procedure TAbstractVirtualDisk.GuaranteeReadBlocks(lba: int64; cnt: nativeint;
  p: pbyte);
var
  iJustRead, iRead: nativeint;
begin
//  Debug.Log('GRB:'+inttostr(lba)+','+inttostr(cnt));
  iRead := 0;
  while iRead < cnt do begin
    iJustRead := ReadBlocks(lba, cnt, @p[iRead*BlockSize]);
    inc(lba, iJustRead);
    dec(cnt, iJustRead);
  end;


end;

procedure TAbstractVirtualDisk.GuaranteeWriteBlocks(lba: int64; cnt: nativeint;
  p: pbyte);
var
  iJustWrote, iWrote: nativeint;
begin
//  Debug.Log('GWB:'+inttostr(lba)+','+inttostr(cnt));
  iWrote := 0;
  while iWrote < cnt do begin
    iJustWrote := WriteBlocks(lba, cnt, @p[iWrote*BlockSize]);
    inc(lba, iJustWrote);
    dec(cnt, iJustWrote);
  end;
end;

procedure TAbstractVirtualDisk.OuterSetBlockSize(const Value: nativeint);
begin
  FSize := (int64(FRequestedSize) div int64(BlockSize)) * int64(BlockSize);
  SetBlockSize(value);
end;





procedure TAbstractVirtualDisk.SetIdentifier(const Value: string);
begin
  Lock;
  try
    FIdentifier := 'iqn-2008-08.com.digitaltundrallc:'+systemx.GetComputerName+'-'+value;
  finally
    Unlock;
  end;
end;

procedure TAbstractVirtualDisk.SetSize(const Value: int64);
begin
  FRequestedSize := value;
  fSize := (FRequestedSize div BlockSize) * BlockSize;

end;

function TAbstractVirtualDisk.TailStartAddr: int64;
begin
  result := Size - TailSize;
end;

function TAbstractVirtualDisk.TailSize: int64;
begin
  result := 8192 * BlockSize;
end;

function TAbstractVirtualDisk.VirtualToPhysicalAddr(addr: int64): int64;
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
  tindex := addr div (BlockSize* LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS);
  r := vat.GetTableEntryForLBA(addr div blocksize);

  if r.address = 0 then begin
    r.Address := NewPhysicalBlockAddress;
    r.Marker := $55555555;
    r.StartingBlock := tindex * LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS ;
    SaveVat;
  end;

  result := r.Address+(addr mod (LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS * BlockSize));


end;
{$endif}

{ TVirtualAddressTable }


{$ifdef use_vat}
procedure TVirtualAddressTable.FlushToStream(s: TStream);
begin
  s.Seek(0,0);
  Stream_GuaranteeWrite(s, Pbyte(@self), sizeof(self));
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
  Stream_GuaranteeRead(s, Pbyte(@self), sizeof(self));
end;
{$endif}

end.

