unit VirtualDisk_NonRAID;
{$DEFINE USE_STANDARD_STREAM}
{x$DEFINE VERIFY_WRITES}
{x$DEFINE SMALL_TEST}
{$DEFINE USE_VAT}
{$DEFINE PAYLOAD_FILE_HEADER}
{$INLINE AUTO}
{x$DEFINE BUFFER_BLOCKS}
{x$DEFINE ALLOW_RAID}
interface

uses
  betterfilestream, stringx, unittest, numbers, tickcount , applicationparams, standardlist, clusteredpointerlist,helpers.stream, systemx, typex, classes, sysutils, generics.collections, betterobject, sharedobject, MultiBufferMemoryFileStream, debug, orderlyinit, managedthread, commandprocessor, queuestream;

const
  MAX_BUFFERED_BLOCKS = 4;
  BUFFER_BLOCK_SHIFT = 2;
{$IFNDEF SMALL_TEST}
  _BIG_BLOCK_SIZE_IN_BLOCKS = 65536*4;//!<<<-------  CHANGE THESE TWO TOGETHER
  BIG_BLOCK_SHIFT = 18;//!<<<<<<<<<<<<<<<-------  CHANGE THESE TWO TOGETHER
  MAX_BLOCKS_IN_VAT = 65536*4;

{$ELSE}
  BIG_BLOCK_SIZE_IN_BLOCKS = 4;
  MAX_BLOCKS_IN_VAT = 32;
{$ENDIF}
  INVALID_ADDRESS = $FFFFFFFFFFFFFFFF;

type
{$IFDEF USE_STANDARD_STREAM}
  TVDStream = TSharedFileStream;
{$ELSE}
  TVDStream = TSharedFileStream;
{$ENDIF}



  TVirtualDiskPayloadFileHeader = packed record
    Version:int64;
    _NextPhysicalAddress: int64;
    BufferSegmentSize:int64;
    BufferCount:int64;
    EnableReadAhead:boolean;
    function NextPhysicalAddressXX(stride: int64): int64;
    procedure Init;
  end;

  TVirtualDiskBigBlockHeader = packed record
    TotalSizeIncludingHeaders: int64;
    HeaderStart: int64;
    PayloadStart: int64;
    FooterStart: int64;
    VirtualAddress: int64;
    function PayloadSize: int64;
    function VAlid(vaddr: int64): boolean;
  end;

  TPayloadStream = class(TVDStream)
  private
    function GetNextBigBlockAddress: int64;
    procedure SetNextBigBlockAddress(addr: int64);
    function GetLastBigBlockFooterAddr: int64;
    function GetLastBigBlockHeaderAddr: int64;

    function GetPFH: TVirtualDiskPayloadFileHeader;
    procedure SetPFH(pfh: TVirtualDiskPayloadFileHeader);
    function GetLastBigBlockHeader: TVirtualDiskBigBlockHeader;

    function GetLastPayloadAddress: int64;
    function CanDropBlock(iAddr: int64; out iBlockedBy: int64): boolean;
    procedure AddBigBlock(const pfh: TVirtualDiskPayloadFileHeader);
    function DropBigBlock(out pfh: TVirtualDiskPayloadFileHeader; out p: pbyte;
      out iSize: int64): boolean;
    function GetBigBlockHeaderFromPhysicalStart(const addr: int64): TVirtualDiskBigBLockHeader;
  public


    function Expand(iSize: int64; virtual_addr: int64): int64;
    procedure CheckInit;
    function IsEmpty: boolean;
    constructor Create(const AFileName: string; Mode: Cardinal;
      Rights: Cardinal; Flags: Cardinal); override;

  end;
  TFilePhysical=packed record
    FileID: smallint;
    Physical: int64;
    procedure InitVirgin;
  end;
  PFilePhysical = ^TFilePhysical;
{$IFDEF USE_VAT}
  TVirtualAddressRecord = packed record
    Marker: WORD;
{$IFNDEF ALLOW_RAID}
    FileID: smallint;
    PhysicalAddress: int64;//if not RAID then FileID is moved outside array
    FileIDs: array [1..19] of TFilePhysical;
    junk: smallint;
    //  ^^^^  this section should be the same size/layout as below
{$ELSE}
    //  vvvv  this section should be the same size/layout as above
    FileIDs: array [0..19] of TFilePhysical;
    FileCount: smallint;
{$ENDIF}
    StartingBlock: cardinal;
    procedure ChangeFileID(ifrom, ito: ni);
    procedure InitVirgin;
{$IFDEF ALLOW_RAID}
    function IndexOf(iFileID, iPhysical: int64): ni;overload;
    function IndexOf(iFileID: ni): ni;overload;
    function GetFP(iFileID, iPhysical: int64): PFilePhysical;overload;
    function GetFP(iFileID: ni): PFilePhysical;overload;
{$ENDIF}
{$IFDEF ALLOW_RAID}
    class operator Equal(const a,b: TVirtualAddressRecord): boolean;
{$ENDIF}
    function DebugString: string;
    function IsAssigned: boolean;
  end;
  TPayloadFileInformation = packed record
  strict private
    function GetNameAsString: string;
    procedure SetNameAsString(const Value: string);
  public

    size_limit: int64;
    used_space:int64;//this is only updated when requested from the client
    greenrating: int64;
    latencyrating: int64;
    name: array[0..2047] of char;

    property NameAsString: string read GetNameAsString write SetNameAsString;

  end;

  PVirtualAddressRecord = ^TVirtualAddressRecord;

  TVirtualDiskPayloadConfiguration = packed record
    filelist: array[0..2047] of TPayloadFileInformation;//<<<!!!!----- -MUsT BE LAST
    procedure AddPayload(sFile: string; max_size: int64);
    function FindUnusedSlot: nativeint;
    function GetMarhshalSize: nativeint;
  end;

  PVirtualDiskPayloadConfiguration = ^TvirtualDiskPayloadConfiguration;

  TVirtualAddressTable = packed record
    //Version: cardinal;
    Version: int64;
    OpenTime: int64;
    CloseConfirm: int64;
    PayloadConfig: TVirtualDiskPayloadConfiguration;
    table: array[0..MAX_BLOCKS_IN_VAT] of TVirtualAddressRecord;
    reserved: cardinal;
    marker: int64;
    DefaultBufferSegmentCount: int64;
    DefaultBufferSegmentSize:int64;
    DefaultBufferReadAhead: boolean;
    persistent_end: int64;
    dirtyStart: int64;
    dirtyEnd: int64;
    procedure InitVirgin;
    function GetTableEntryForLBA(lba: int64): PVirtualAddressRecord;
    procedure ReadFromStream(s: TStream);
    procedure FlushToStream(s: TStream; bForce: boolean);


  private
    function GetFileCount: cardinal;
    function GetTableEntryPhysicalAddress(iFileID: nativeint;
      physical: int64): PVirtualAddressRecord;
  public
    property FileCount: cardinal read GetFileCount;
    function DebugVatSTructure: string;
    procedure MarkTableEntryDirty(idx: ni);
    procedure MarkTableEntryDirtyByPtr(ptr: PVirtualAddressRecord);
    procedure MarkDirty(iStart, iEnd: int64);overload;
    procedure MarkDirty(ptr: pointer; iLEngth: ni);overload;
    function PersistedSize: ni;
    procedure SetOpenMarker;
    procedure SetCloseMarker;
    function LocalAddr(p: pointer): int64;
  end;
{$ENDIF}

{$IFDEF BUFFER_BLOCKS}
  TVDBlockBuffer = record
  public
    StartingBlock: int64;
    ReadFromDisk: boolean;
    dirty: array[0..MAX_BUFFERED_BLOCKS-1] of boolean;
    data: array[0..2047] of byte;
    function FirstDirty(iAfter: ni): ni;
    function LAstDirty(iAfter: ni): ni;
    function CoversBlock(lba: int64): boolean;
    function AnyDirty: boolean;
    procedure Init;
  end;
{$ENDIF}

  TAbstractVirtualDisk_Advanced = class(TSharedObject)
  strict
  private
    function GetIdentifier: string;
    procedure SetIdentifier(const Value: string);
  strict protected
    function TailSize: int64; protected
    FSize: int64;
    vat: TVirtualAddressTable;
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
    procedure SaveVatAndConfig;virtual;abstract;
{$endif}
    function NewPhysicalBlockAddress(virtual_addr: int64): TVirtualAddressRecord;virtual;abstract;
  public

    constructor Create;override;
    procedure BeforeDestruction;override;
    destructor Destroy;override;
    property BlockSize: nativeint read FBlockSize write OuterSetBlockSize;
    property Size: int64 read FSize write SetSize;
    function BlockCount: int64;
    function TailStartAddr: int64;
    function VirtualToPhysicalAddr(addr: int64): TVirtualAddressRecord;
    procedure FlexWrite(const addr: int64; const cnt: nativeint; p:pbyte);
    procedure FlexRead(const addr:int64;const cnt: nativeint; p:pbyte);

    procedure ReadBlock(const lba: int64; const p: pbyte);virtual;abstract;
    function ReadBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;virtual;abstract;
    function WriteBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;virtual;abstract;
    procedure WriteBlock(const lba: int64; const p: pbyte);virtual;abstract;
    procedure GuaranteeReadBlocks(lba: int64; cnt: nativeint; p: pbyte);
    procedure GuaranteeWriteBlocks(lba: int64; cnt: nativeint; p: pbyte);



    procedure GrowIfNeededBlock(lba: int64);virtual;abstract;
    procedure GrowIfNeededAddr(size: int64);virtual;abstract;
    procedure WriteData(const addr: int64; const cnt: nativeint;p:pbyte);virtual;abstract;
    procedure ReadData(const addr: int64; const cnt: nativeint; p: pbyte);virtual;abstract;
    procedure ChangeSingleByte(iAddr: int64; b: byte);virtual;abstract;
    function ReadSingleByte(iAddr: int64): byte;virtual;abstract;
    procedure Preallocate(lba: int64);virtual;abstract;
    property Identifier: string read GetIdentifier write SetIdentifier;

  end;



  TFileBasedVirtualdisk = class(TAbstractVirtualDisk_Advanced)
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


  TVirtualDisk_Advanced = class(TFileBasedVirtualdisk)
  private
    FCacheSize: int64;
    scrubber: TExternalEVentThread;
    scrubber_file_index: ni;
    bRepaired: boolean;
    FMigrating: boolean;
{$IFDEF BUFFER_BLOCKS}
    buffer: TVDBlockBuffer;
{$ENDIF}

    procedure SetCAcheSize(const Value: int64);
    procedure ScrubberExecute(thr: TExternalEventThread);

    procedure Repair;
    procedure Repair_payload(ps: TPayloadStream; id: ni);
    function GEtOverQuotaStream(out id: nativeint): TPayloadStream;
    function GEtUnderQuotaStream(out id: nativeint; bOnlyReadyStreams: boolean): TPayloadStream;overload;
    function GEtUnderQuotaStream(out id: nativeint): TPayloadStream;overload;
    procedure MovePayloadBigBlock(FromID, toID: nativeint; fromstream, tostream: TPayloadStream);
    procedure CheckVatSanity;
  protected
    procedure ReadBlock(const lba: int64; const p: pbyte);override;
    procedure WriteBlock(const lba: int64; const p: pbyte);override;

    procedure SetSize(const Value: int64);override;
    function GetfileName: string;override;
    procedure SetFileNAme(const Value: string);override;
    procedure SetBlockSize(const Value: nativeint);override;
{$ifdef use_vat}
    procedure LoadVat;override;
    procedure SaveVat;override;
    procedure SaveVatAndConfig;override;
    function NewPhysicalBlockAddress(virtual_addr: int64): TVirtualAddressRecord;override;
{$endif}
  public
    FVATStream: TVDStream;
    FPayloadStreams: TList<TPayloadStream>;
    constructor Create;override;
    destructor Destroy;override;
    procedure LoadPayloadStreams;
    procedure DestroyPayloadStreams;
    property Migrating: boolean read FMigrating write FMigrating;


    function ReadBlocks_Direct_Single(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
    function WriteBlocks_Direct_Single(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;

    function ReadBlocks_Direct_Parity(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
    function WriteBlocks_Direct_Parity(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;


    function ReadBlocks_Direct(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
    function WriteBlocks_Direct(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
{$IFDEF BUFFER_BLOCKS}
    function ReadBlocks_Buffered(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
    function WriteBlocks_Buffered(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
{$ENDIF}

{$IFDEF BUFFER_BLOCKS}
    procedure SyncBuffer(lba: int64; bForWriting: boolean);
{$ENDIF}
    procedure GuaranteeReadBlocks_Direct(lba: int64; cnt: nativeint; p: pbyte);
    procedure GuaranteeWriteBlocks_Direct(lba: int64; cnt: nativeint; p: pbyte);



    function ReadBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;override;
    function WriteBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;override;

    procedure GrowIfNeededBlock(lba: int64);override;
    procedure GrowIfNeededAddr(size: int64);override;
    procedure WriteData(const addr: int64; const cnt: nativeint;p:pbyte);override;
    procedure ReadData(const addr: int64; const cnt: nativeint; p: pbyte);override;
    procedure ChangeSingleByte(iAddr: int64; b: byte);override;
    function ReadSingleByte(iAddr: int64): byte;override;
    procedure Preallocate(lba: int64);override;
    property CacheSize: int64 read FCacheSize write SetCAcheSize;
    function DebugVatStructure: string;
    procedure AddPayload(sFile: string; size_limit: int64);
    function GetPayloadConfig: PVirtualDiskPayloadConfiguration;
    procedure SetPayloadQuota(iFileID: ni; max_size: int64);
    procedure SetDefaultCacheParams(iSegmentSize, iSegmentCount: ni; bReadAhead: boolean);
    procedure DecommissionPayload(sFile: string);
  end;

  TVirtualDisk = class(TVirtualDisk_Advanced);

  TVirtualDiskList = class(TList<TVirtualDisk>);
  TVirtualDiskHub = class(TSharedObject)
  public
    vdlist: TVirtualDiskList;
    constructor create;override;
    destructor Destroy;override;

    procedure LoadConfiguration;
  end;

procedure CreateVirtualDiskHub;
procedure DestroyVirtualDiskHub;


function GetTinyBlockSize(iRaidCount: ni): ni;inline;
function GetBigBlockSizeInBytes(iRAidCount: ni; bIncludingHeaders: boolean = false): ni;inline;



var
  BS : array[1..31] of ni;
  vdh: TVirtualDiskHub;


implementation

{ TVirtualDisk }


function GetBigBlockSizeInBytes(iRAidCount: ni; bIncludingHeaders: boolean = false): ni;inline;
begin
  result := GetTinyBlockSize(iRaidCount) * _BIG_BLOCK_SIZE_IN_BLOCKS;
  if bIncludingHeaders then
    result := result + (2*sizeof(TVirtualDiskBigBlockHeader));
end;

function GetTinyBlockSize(iRaidCount: ni): ni;
begin
  result := BS[iRaidCount];

end;

procedure TVirtualDisk_Advanced.AddPayload(sFile: string; size_limit: int64);
begin
  Lock;
  try
    if size_limit < 0 then size_limit := -1;
    vat.PayloadConfig.AddPayload(sfile, size_limit);
    SaveVatAndConfig;
    //FVatStream.FinalizeBuffers;
    LoadPayloadStreams;
  finally
    Unlock;
  end;
end;




procedure TVirtualDisk_Advanced.ChangeSingleByte(iAddr: int64; b: byte);
var
  block: array[0..511] of byte;
  iOFF: int64;
begin
  self.ReadBlock(iAddr div BlockSize, @block[0]);
  iOFF := iAddr mod BlockSize;

  block[iOFF] := b;
  self.WriteBlock(iAddr div BlockSize, @block[0]);



end;

constructor TVirtualDisk_Advanced.Create;
begin
  inherited;
{$IFDEF BUFFER_BLOCKS}
  buffer.init;
{$ENDIF}
  FBlockSize := 512;
  FCacheSize := 256*MEGA;
  FPayloadStreams := TList<TPayloadStream>.create;

  vat.InitVirgin;


end;

function TVirtualDisk_Advanced.GEtOverQuotaStream(out id: nativeint): TPayloadStream;
var
  t: ni;
  pfc: TPayloadFileInformation;
  str: TPayloadStream;
  i1, i2: int64;
begin
  id := -1;
  result := nil;
  for t:= 0 to FPayloadStreams.Count-1 do begin
    str := FPayloadStreams[t];
    if str = nil then continue;
    i1 := str.Size-sizeof(TVirtualDiskPayloadFileHeader);
    i2 := vat.PayloadConfig.filelist[t].size_limit;
    if (i1 > i2) and (i2 >=0) then begin
      Debug.Log(str.FileName+' is over quota ... size:'+inttostr(i1));
      result := str;
      id := t;
      break;
    end else begin
      //Debug.ConsoleLog(str.FileName+' not over quota');
    end;
  end;

end;

function TVirtualDisk_Advanced.GEtUnderQuotaStream(out id: nativeint): TPayloadStream;
var
  t: nativeint;
  ps: TPayloadStream;
begin
  result := GetUnderQuotaStream(id, false);
  if result = nil then
    result := GEtUnderQuotaStream(id, true);


  for t:= 0 to FPayloadStreams.count-1 do begin
    ps := FPayloadStreams[t];
    if t = id then continue;
    if ps <> nil then begin
      ps.Lock;
      try
        //ps.BeginFLushandPrefetch(-1);
      finally
        ps.unlock;
      end;
    end;
  end;


end;
procedure TVirtualDisk_Advanced.MovePayloadBigBlock(FromID, toID: nativeint; fromstream,tostream: TPayloadStream);
var
  pfh_from, pfh_to: TVirtualDiskPayloadFileHeader;
var
  buf: pointer;
  i: int64;
  npa: int64;
  pva: PVirtualAddressRecord;
  bbh_from, bbh_to: TVirtualDiskBigBLockHeader;
  pf: PFilePhysical;
begin
//  fromstream.SeekLock;
//  tostream.SeekLock;
  try

  fillmem(@pfh_to, sizeof(pfh_to), 0);
  fillmem(@pfh_from, sizeof(pfh_to), 0);
  fromStream.Seek(0,0);
  toStream.Seek(0,0);

  //get header from files
  if fromstream.size > sizeof(pfh_from) then
    pfh_from := fromStream.GetPFH;

  if tostream.size > sizeof(pfh_to) then
    pfh_to := toStream.GetPFH;


  //walk backwards the size of 1 big-block
  i := fromStream.GetLastBigBlockFooterAddr;
  if i < 0 then exit;
  pfh_from._NextPhysicalAddress := i;
  bbh_from := fromStream.GetLastBigBlockHeader;


  //expand the target stream
  toStream.Expand(GetBigBlockSizeInBytes(1), bbh_from.VirtualAddress);
  pfh_to := toStream.GetPFH;

  //get physical address to move to
  bbh_to := tostream.GetLastBigBlockHeader;

  //get new physical address
  npa := bbh_to.PayloadStart;

  //make sure we have an entry for this
  pva := vat.GetTableEntryPhysicalAddress(FromID, bbh_from.PayloadStart);
  if (pva <> nil) then begin

    //buf := getmemory(1024000);
    try
      //tostream.CopyFrom(fromstream,  GetBigBlockSizeInBytes(1));
      Debug.ConsoleLog('Move From:'+inttostr(fromid)+':'+inttohex(pfh_from._NextPhysicalAddress,0)+' to '+inttostr(toid)+':'+inttohex(npa,0));
      fromstream.Seek(bbh_from.PayloadStart, 0);
      tostream.Seek(bbh_to.PayloadStart,0);
      //tostream.CopyFrom(fromstream, bbh_from.PayloadSize);


//      Stream_GuaranteeRead(fromstream, buf,  bbh_from.PayloadSize);
//      Stream_GuaranteeWrite(tostream, buf,  bbh_to.PayloadSize);
      Stream_GuaranteeCopy(fromStream, tostream, bbh_from.PayloadSize);

      Debug.ConsoleLog('Completed Move From:'+inttostr(fromid)+':'+inttohex(pfh_from._NextPhysicalAddress,0)+' to '+inttostr(toid)+':'+inttohex(npa,0));

    finally
      //Freememory(buf);
    end;

    //Save physical Addres to "to" file
    toStream.SetPFH(pfh_to);

    //change fileid in vat
{$IFDEF ALLOW_RAID}
    pva.ChangeFileID(fromid, toid);
    pf := pva.GetFP(toid);
    if pf <> nil then begin
      pf.Physical := npa;
    end;
{$ELSE}
    pva.FileID := toID;
    pva.PhysicalAddress := npa;
{$ENDIF}

    vat.MarkTableEntryDirtyByPtr(pva);
    SaveVat;

  end;

  //save physical address back to "from" file
  pfh_from._NextPhysicalAddress := fromStream.GetLastBigBlockHeaderAddr;
  fromStream.SetPFH(pfh_from);


  //reduce size of from file
  fromSTream.Size := pfh_from._NextPhysicalAddress
  finally
//    fromstream.seekunlock;
//    tostream.SeekUnlock;
  end;

end;
procedure TVirtualDisk_Advanced.CheckVatSanity;
begin
  exit;

end;
procedure TVirtualDisk_Advanced.ScrubberExecute(thr: TExternalEventThread);
var
  underid: nativeint;
  overid: nativeint;
  over, under: TPayloadStream;
  tm1,tm2: ticker;
begin
  Lock;
  try
    Repair;


    over := GEtOverQuotaStream(overid);
    if assigned(over) then begin


      under := GetUnderQuotaStream(underid);


      if assigned(under) then begin
        Migrating := true;
//        Debug.ConsoleLog(self.vat.DebugVatSTructure);
//        Debug.ConsoleLog('AFter:');
        tm1 := GEtTicker;
        MovePayloadBigBlock(overid, underid, over,under);
        tm2 := getTicker;
        thr.ColdRunInterval := greaterof(gettimesince(tm2,tm1)*1,10);


//        Debug.ConsoleLog(self.vat.DebugVatSTructure);
        //thr.RunHot := true;
      end else begin
        thr.RunHot := false;
        Migrating := false;
      end;
    end else begin
      Migrating := false;
      thr.RunHot := false;
      CheckVATSanity;
    end;
  finally
    Unlock;
  end;






end;

function TVirtualDisk_Advanced.DebugVatStructure: string;
begin
  result := vat.DebugVatSTructure;
end;

procedure TVirtualDisk_Advanced.DecommissionPayload(sFile: string);
var
  t: ni;
  s: string;
begin
  Lock;
  try
    for t := low(vat.payloadconfig.filelist) to high(vat.PayloadConfig.filelist) do begin
      s := vat.PayloadConfig.filelist[t].nameasstring;
      if filenamecompare(s, sfile) then begin
        if not self.FPayloadStreams[t].Size <= SizeOf(TVirtualDiskPayloadFileHeader) then begin
          raise EUserError.create('not empty, wait for empty and try again.');
        end else begin
          vat.PayloadConfig.filelist[t].nameAsString := '';
          SaveVatAndConfig;
//          FVatStream.FinalizeBuffers;
          LoadPayloadStreams;
          DeleteFile(s);

        end;
      end;
    end;

  finally
    Unlock;
  end;

end;

destructor TVirtualDisk_Advanced.Destroy;
begin
//  Debug.Log('Stream is Destroyed.  Size is '+inttostr(FStream.size));
  scrubber.stop;
  scrubber.WaitForFinish;
  tpm.NoNeedThread(scrubber);
  scrubber := nil;

{$IFDEF BUFFER_BLOCKS}
  syncbuffer(-1, true);
{$ENDIF}


  DestroyPayloadStreams;



  FPayLoadStreams.Free;
  FPayloadStreams := nil;

  vat.SetCloseMarker;
  SaveVat;
  FVatStream.Free;
  FVatStream := nil;
  inherited;
end;

procedure TVirtualDisk_Advanced.DestroyPayloadStreams;
var
  ps: TVDStream;
begin
  while FPayloadStreams.count > 0 do begin
    ps := FPayloadStreams[FPayloadStreams.count-1];
    ps.free;
    ps := nil;
    FPayloadStreams.delete(FPayloadStreams.count-1);
  end;
end;

function TVirtualDisk_Advanced.GetfileName: string;
begin
  lock;
  try
    if FVatStream = nil then
      result := ''
    else
      result := FVatStream.FileName;

  finally
    unlock;
  end;
end;

function TVirtualDisk_Advanced.GetPayloadConfig: PVirtualDiskPayloadConfiguration;
var
  t: ni;
begin
  Lock;
  try
    result := @vat.PayloadConfig;
    for t := low(result.filelist) to high(result.filelist) do begin
      if t < FPayloadStreams.count then begin
        if self.FPayloadStreams[t] <> nil then begin
          result.filelist[t].used_space := FPayloadStreams[t].Size;
        end;
      end else begin
        result.filelist[t].used_space := 0;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TVirtualDisk_Advanced.GEtUnderQuotaStream(out id: nativeint;
  bOnlyReadyStreams: boolean): TPayloadStream;
var
  t: ni;
  pfc: TPayloadFileInformation;
  str: TPayloadStream;
  i1:int64;
  i2: int64;
  sz_min, sz: int64;
begin
  id := -1;
  result := nil;
  sz_min := -1;
  for t:= 0 to FPayloadStreams.Count-1 do begin

    str := FPayloadStreams[t];
    if str = nil then continue;
    i1 := str.Size;
    i1 := i1 + GetBigBlockSizeInBytes(1,true);

    i2 := vat.PayloadConfig.filelist[t].size_limit;
    if i2 = 0 then
      continue;

    if (i1 < i2) or (i2 = -1) and ((not bOnlyReadyStreams)) then begin
      if (str.Size < sz_min) or (sz_min = -1) then begin
        result := str;
        id := t;
        sz_min := str.size;
        //break;
      end;

    end;
  end;

end;

procedure TVirtualDisk_Advanced.GrowIfNeededBlock(lba: int64);
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

procedure TVirtualDisk_Advanced.GuaranteeReadBlocks_Direct(lba: int64;
  cnt: nativeint; p: pbyte);
var
  iJustRead, iRead: nativeint;
begin
//  Debug.Log('GRB:'+inttostr(lba)+','+inttostr(cnt));
  iRead := 0;
  while iRead < cnt do begin
    iJustRead := ReadBlocks_Direct(lba, cnt, @p[iRead*BlockSize]);
    inc(lba, iJustRead);
    dec(cnt, iJustRead);
    inc(iREad, iJustREad);
  end;


end;

procedure TVirtualDisk_Advanced.GuaranteeWriteBlocks_Direct(lba: int64;
  cnt: nativeint; p: pbyte);
var
  iJustWrote, iWrote: nativeint;
begin
//  Debug.Log('GWB:'+inttostr(lba)+','+inttostr(cnt));
  iWrote := 0;
  while iWrote < cnt do begin
    iJustWrote := WriteBlocks_Direct(lba, cnt, @p[iWrote*BlockSize]);
    inc(lba, iJustWrote);
    dec(cnt, iJustWrote);
    inc(iWrote, iJustWrote);
  end;
end;

{$ifdef use_vat}
procedure TVirtualDisk_Advanced.LoadPayloadStreams;
var
  t: integer;
  s: string;
  ps: TpayloadStream;
  pfh: TVirtualDiskPayloadFileHeader;
begin
  DestroyPayloadStreams;//incase some were already defined

  if self.vat.FileCount = 0 then begin
    s := changefileext(self.FileName,'.vdpayload');
    movemem32(@self.vat.PayloadConfig.filelist[0].name[0], @s[STRZ], sizeof(char)*length(s));
    self.vat.PayloadConfig.filelist[0].size_limit := -1;
  end;

  for t:= low(self.vat.payloadConfig.filelist) to high(self.vat.payloadconfig.filelist) do begin
    s := self.vat.payloadconfig.filelist[t].name;
    if s <> '' then begin
      if fileexists(s) then begin
        ps := TPayloadStream.create(s, fmOpenReadWrite+fmShareExclusive);
      end else begin
        ps := TPayloadStream.create(s, fmCreate);
        pfh.Init;
        ps.seek(0,soBeginning);
        stream_guaranteewrite(ps, @pfh, sizeof(pfh));
        ps.free;
        ps := TPayloadStream.create(s, fmOpenReadWrite+fmShareDenyNone);

      end;

      //ps.AllowReadPastEOF := true;
//      ps.BufferSize := 32*65536;
//      ps.BufferSEgments := 32;
//      ps.DisableLookAhead := true;
//      ps.BufferSize := FCacheSize;

      FPayloadStreams.add(ps);
    end else begin
      FPayloadStreams.add(nil);
    end;
  end;
end;

procedure TVirtualDisk_Advanced.LoadVat;
begin

  if FVatStream.Size < vat.PersistedSize then begin
    vat.InitVirgin;
    vat.FlushToStream(FVatStream, true);
  end else begin
    vat.ReadFromStream(FVatStream);
    vat.FlushToStream(FVatStream,true);
  end;



end;
{$endif}

{$ifdef use_vat}
function TVirtualDisk_Advanced.NewPhysicalBlockAddress(virtual_addr: int64): TVirtualAddressRecord;
var
  fid: ni;
  fs: TPayloadStream;
  tindex: ni;
begin

  fs := self.GEtUnderQuotaStream(fid);

  if fs = nil then begin
    raise ECritical.Create('out of space');
  end;

  fs.Expand(GetBigBlockSizeInBytes(1), virtual_addr);

  result.InitVirgin;
{$IFNDEF ALLOW_RAID}
  result.FileID := fid;
  result.PhysicalAddress := fs.GetLastBigBlockHeader.PayloadStart;
{$ELSE}
  result.FileIDs[0].FileID := fid;
  result.FileIDs[0].Physical := fs.GetLastBigBlockHeader.PayloadStart;
  result.FileCount := 1;
{$ENDIF}
  result.StartingBlock := virtual_addr div blocksize;
  tindex := virtual_addr div (BlockSize* _BIG_BLOCK_SIZE_IN_BLOCKS);
  vat.table[tindex] := result;
  vat.MarkTableEntryDirty(tindex);

  debug.log('New Physical:'+result.DebugString);
  debug.log('New Vat:'+self.vat.DebugVatSTructure);



end;
{$endif}

procedure TVirtualDisk_Advanced.GrowIfNeededAddr(size: int64);
begin
  GrowIfNeededBlock((size div blocksize)+1);
end;


procedure TVirtualDisk_Advanced.Preallocate(lba: int64);
begin
  exit;
//  self.BlockSize := blocksize;
  GrowIfNeededBlock(lba);
{$IFNDEF USE_STANDARD_STREAM}
  FVatstream.Flush;
{$ENDIF}
  FileName := FileName;

end;

procedure TVirtualDisk_Advanced.ReadBlock(const lba: int64; const p: pbyte);
begin
  ReadBlocks(lba, 1, p);
end;

function TVirtualDisk_Advanced.ReadBlocks(const lba: int64; const cnt: nativeint;
  p: pbyte): nativeint;
begin
{$IFDEF BUFFER_BLOCKS}
  result := ReadBlocks_Buffered(lba, cnt, p);
{$ELSE}
  result := ReadBlocks_Direct(lba, cnt, p);
{$ENDIF}
end;

{$IFDEF BUFFER_BLOCKS}
function TVirtualDisk_Advanced.ReadBlocks_Buffered(lba: int64; cnt: nativeint;
  p: pbyte): nativeint;
var
  ioffset, iByteOffset: nativeint;
  iToRead: nativeint;
begin
  //sync up the buffer to the desired position
  SyncBuffer(lba, false);

  //determine which slot in the buffer we're using
  iOffset := lba and (int64($03));


  //READ the data FROM the buffer
  iByteOffset := $200 * iOffset;

  cnt := lesserof(cnt, MAX_BUFFERED_BLOCKS-iOffset);
  iToRead := lesserof(cnt, MAX_BUFFERED_BLOCKS-iOffset);
  result := iToREad;
  iToRead := iToREad * FBlockSize;

  movemem32(p, @Self.buffer.data[iByteOffset], iToRead);

  //mark dirty
  //buffer.dirty[iOffset] := true;


end;
{$ENDIF}

function TVirtualDisk_Advanced.ReadBlocks_Direct(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
begin
  result := ReadBlocks_Direct_Single(lba, cnt, p);
end;

function TVirtualDisk_Advanced.ReadBlocks_Direct_Parity(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TVirtualDisk_Advanced.ReadBlocks_Direct_Single(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
var
  actual_addr: TVirtualAddressRecord;
  end_actual_addr: TVirtualAddressRecord;
  bDifferentFiles: boolean;
  lcnt: nativeint;
  ps: TPayloadStream;
begin
  lcnt := cnt;
  result := cnt;

  lock;
  try
    GrowIfNeededBlock(lba+(lcnt-1));
    actual_addr := VirtualToPhysicalAddr(lba*FBlockSize);
    end_actual_addr := VirtualToPhysicalAddr((lba+lcnt) * FBlockSize);
{$IFDEF ALLOW_RAID}
    bDifferentFiles := actual_addr.FileIDs[0].FileID <> end_actual_addr.FileIDs[0].FileID;
{$ELSE}
    bDifferentFiles := actual_addr.FileID <> end_actual_addr.FileID;
{$ENDIF}

    //if the blocks are not all consecutive, we'll need to reduce the number of blocks we can write
{$IFNDEF ALLOW_RAID}
    if bDifferentFiles or ((end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <> (lcnt * BlockSize)) then begin
{$ELSE}
    if bDifferentFiles or ((end_actual_addr.FileIDs[0].Physical - actual_addr.FileIDs[0].physical) <> (lcnt * BlockSize)) then begin
{$ENDIF}
      //change the end actual address
      begin
        //find the beginning of the actual_addr bigblock
{$IFDEF ALLOW_RAID}
        end_actual_addr.FileIDs[0] := actual_addr.FileIDs[0];
{$ELSE}
        end_actual_addr.FileID := actual_addr.FileID;
{$ENDIF}
        end_actual_addr := VirtualToPhysicalAddr(((lba div _BIG_BLOCK_SIZE_IN_BLOCKS) * _BIG_BLOCK_SIZE_IN_BLOCKS)*FBlockSize);
{$IFNDEF ALLOW_RAID}
        end_actual_addr.PhysicalAddress := end_actual_addr.PhysicalAddress + (_BIG_BLOCK_SIZE_IN_BLOCKS * FBlockSize);
{$ELSE}
        end_actual_addr.fileids[0].physical := end_actual_addr.fileids[0].physical + (_BIG_BLOCK_SIZE_IN_BLOCKS * FBlockSize);
{$ENDIF}
      end;

{$IFNDEF ALLOW_RAID}
      result := (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) div BlockSize;
{$ELSE}
      result := (end_actual_addr.fileids[0].physical - actual_addr.fileids[0].physical) div BlockSize;
{$ENDIF}
      lcnt := result;
{$IFNDEF ALLOW_RAID}
      if (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <> (lcnt * BlockSize) then begin
{$ELSE}
      if (end_actual_addr.fileids[0].physical - actual_addr.fileids[0].physical) <> (lcnt * BlockSize) then begin
{$ENDIF}
        raise ECritical.create('Block segment fault failure.');
      end;
    end;
//    Debug.Log(inttohex(lba*FBlocksize, 2)+'<-actual_addr(rb)->'+inttohex(actual_addr, 2));


    if (lba*BlockSize) >= (Size+Blocksize) then
      raise ECritical.create('Address beyond the size of the disk! '+inttohex(lba*BlockSize,2));


{$IFDEF ALLOW_RAID}
    ps := FPayloadStreams[actual_addr.FileIDs[0].FileID];
    ps.seeklock;
    try
    ps.Seek(actual_addr.FileIDs[0].Physical,0);
    Stream_GuaranteeRead(ps, p, FBlockSize*lcnt);
    finally
    ps.seekunlock;
    end;
{$ELSE}
    ps := FPayloadStreams[actual_addr.FileID];
//    ps.seeklock;
    try
    FPayloadStreams[actual_addr.FileID].Seek(actual_addr.PhysicalAddress,0);
    Stream_GuaranteeRead(FPayloadStreams[actual_addr.FileID], p, FBlockSize*lcnt);
    finally
//    ps.seekunlock;
    end;

{$ENDIF}

  finally
    Unlock;
  end;

end;

procedure TVirtualDisk_Advanced.ReadData(const addr: int64; const cnt: nativeint; p: pbyte);
begin
  Lock;
  try
    if (cnt mod BlockSize) <> 0 then
      raise ECRitical.create('Cnt is not a multiple of block size');

    if (addr mod BlockSize) <> 0 then
      raise ECRitical.create('addr is not a multiple of block size');


    if addr = 3356491776 then
      GuaranteeReadBlocks(addr div BlockSize, cnt div BlockSize, p)
    else
      GuaranteeReadBlocks(addr div BlockSize, cnt div BlockSize, p);
  finally
    Unlock;
  end;

end;

function TVirtualDisk_Advanced.ReadSingleByte(iAddr: int64): byte;
var
  block: array[0..511] of byte;
  iOFF: int64;
begin
  self.ReadBlock(iAddr div BlockSize, @block[0]);
  iOFF := iAddr mod BlockSize;

  result := block[iOFF];
//  self.WriteBlock(iAddr div BlockSize, @block[0]);

end;

procedure TVirtualDisk_Advanced.Repair;
var
  t: ni;
  ta: PVirtualAddressRecord;
  pfh: TVirtualDiskPayloadFileHeader;
  ps: TPayloadStream;
  bbh: TVirtualDiskBigBLockHeader;
  a,b: int64;
  sPre, sPost: string;
begin
{$IFNDEF ALLOW_RAID}
  sPre := self.vat.DebugVatSTructure;
  if bRepaired then exit;
  bRepaired := true;
  Debug.ConsoleLog(self.debugvatstructure);
  lock;
  try

    //go through the vat
    for t:= low(self.vat.table) to high(self.vat.table) do begin
      ta := @self.vat.table[t];
      if ta.FileID = -1 then begin
        if not (ta.physicaladdress = -1) then begin
          ta.PhysicalAddress := -1;
          bRepaired := false;
        end;
      end;
      //check that the vat entry exists in the physical
      if ta.fileid >= 0 then begin
        ps := FPayloadStreams[ta.fileid];
        if ps <> nil then begin
          if (ta.FileID > (self.FPayloadStreams.count-1)) or (self.FPayloadStreams[ta.FileID] = nil) then begin
            Debug.ConsoleLog('Fixing bogus fileid at '+inttostr(t)+' was '+inttostr(ta.FileID));
            ta.FileID := -1;
            ta.PhysicalAddress := -1;
            brepaired := false;
          end;
          a := (ta.physicaladdress + GetBigBlockSizeInBytes(1,false)+sizeof(TVirtualDiskBigBlockHeader));
          b :=  FPayloadStreams[ta.FileID].Size;
          if a > b then begin
            Debug.ConsoleLog('Fixing bogus fileid at '+inttostr(t)+' was '+inttostr(ta.FileID));
            ta.FileID := -1;
            ta.PhysicalAddress := -1;
            brepaired := false;
          end;
          if ta.physicaladdress > 0 then begin
            bbh := ps.GetBigBlockHeaderFromPhysicalStart(ta.PhysicalAddress);
            if not bbh.VAlid(ta.StartingBlock*BlockSize) then begin
              Debug.ConsoleLog('BIg Block header is bogus, removing from vat');
              ta.fileid := -1;
              ta.physicaladdress := -1;
              bREpaired := false;
            end;
          end;
        end;

      end;


      if ta.FileID < 0 then
        continue;


      if self.FPayloadStreams[ta.FileID].Size > ta.PhysicalAddress then begin
        //check that the vat entry is not truncated
        a := self.FPayloadStreams[ta.FileID].Size;
        b := (ta.PhysicalAddress + (1*sizeof(TVirtualDiskBigBlockHeader)) + GetBigBlockSizeInBytes(1));
        if a >= b then begin
            //good
        end else begin
          Debug.ConsoleLog('Fixing:'+inttohex(ta.StartingBlock,2));
          //truncate file... this block is shit
          self.FPayloadStreams[ta.FileID].Size := ta.PhysicalAddress;
          pfh := self.FPayloadStreams[ta.FileID].GetPFH;
          pfh._NextPhysicalAddress := ta.PhysicalAddress;
          self.FPayloadStreams[ta.FileID].SetPFH(pfh);
          ta.FileID := -1;//this block is lost (may be recovered from an orphan check)... which requires scanning starting at the payload files
          brepaired := false;
        end;
      end;

    end;

    for t:= 0 to FPayloadStreams.count-1 do begin
      if FPayloadStreams[t] <> nil then
        Repair_payload(FPayloadStreams[t], t);
    end;

    if not bRepaired then begin
      vat.MarkDirty(0, sizeof(vat));
      SaveVAt;
    end;


  finally
    unlock;
  end;
  sPOst := DebugVatStructure;
  SaveStringAsFile(self.FileName+'.repairinfo.txt', 'Pre:'+NEWLINE+sPre+NEWLINE+'Post:'+NEWLINE+sPost);
{$ENDIF}
end;

procedure TVirtualDisk_Advanced.Repair_payload(ps: TPayloadStream; id: ni);
var
  pfh: TVirtualdiskpayloadfileheader;
  bbh,bbf: TVirtualDiskBigBlockHeader;
  vad: int64;
begin
{$IFNDEF ALLOW_RAID}
  if (ps.Size-sizeof(TVirtualDiskPayloadFileHEader)) mod (2*sizeof(bbf)+GetBigBlockSizeInBytes(1)) <> 0 then begin
    DEbug.Log(ps.filename+' is invalid size.... truncating.');
    ps.Size := (ps.Size-sizeof(TVirtualDiskPayloadFileHEader)) div (2*sizeof(bbf)+GetBigBlockSizeInBytes(1) * (2*sizeof(bbf)+GetBigBlockSizeInBytes(1) ))+sizeof(TVirtualDiskPayloadFileHEader);
    brepaired := false;
  end;

  pfh := ps.GetPFH;
  //move to the first payload
  ps.Seek(sizeof(TVirtualdiskpayloadfileheader), soBeginning);

  while ps.Position < ps.Size do begin
    //get the header
    Stream_guaranteeRead(ps, @bbh, sizeof(bbh), true);
    ps.seek(ps.position+ getbigblocksizeinbytes(1), soBeginning);
    //get the footer
    Stream_guaranteeRead(ps, @bbf, sizeof(bbh), true);

    if bbh.VirtualAddress = bbf.VirtualAddress then begin
      vad := bbh.virtualaddress;
      if vad > (int64(MAX_BLOCKS_IN_VAT) * int64(_BIG_BLOCK_SIZE_IN_BLOCKS* Fblocksize)) then begin
        Debug.Log('Problem '+inttostr(vad));
        brepaired := false;
      end;

      if vad < 0 then begin
        Debug.Log('Problem '+inttostr(vad));
        brepaired := false;
      end;


      if bbh.VirtualAddress = bbf.VirtualAddress then
      if self.vat.table[bbh.VirtualAddress div (FBlockSize * _BIG_BLOCK_SIZE_IN_BLOCKS)].FileID = -1 then begin
        Debug.ConsoleLog('Found orphaned payload, assigning');
        self.vat.table[bbh.virtualaddress div (FBlockSize * _BIG_BLOCK_SIZE_IN_BLOCKS)].FileID := id;
        brepaired := false;
      end;

    end;
  end;
  pfh._NextPhysicalAddress := ps.Size;
  ps.SetPFH(pfh);

{$ENDIF}

end;

{$ifdef use_vat}
procedure TVirtualDisk_Advanced.SaveVat;
begin
  vat.FlushToStream(FVATStream, false);
end;

procedure TVirtualDisk_Advanced.SaveVatAndConfig;
begin
  vat.FlushToStream(FVATStream, true);
end;

{$endif}

procedure TVirtualDisk_Advanced.SetBlockSize(const Value: nativeint);
begin

  if filename <> '' then
    raise Exception.Create('set block size only when filename not set');

  FBlockSize := 512;//always 512
end;

procedure TVirtualDisk_Advanced.SetCAcheSize(const Value: int64);
begin
  FCacheSize := Value;
{$IFNDEF USE_STANDARD_STREAM}
//  if assigned(FStream) then
//    FStream.BufferSize := FCacheSize;
{$ENDIF}

end;

procedure TVirtualDisk_Advanced.SetDefaultCacheParams(iSegmentSize,
  iSegmentCount: ni; bReadAhead: boolean);
var
  t: ni;
  ps: TVDStream;
begin
  Lock;
  try
    vat.DefaultBufferSegmentSize := iSegmentSize;
    vat.DefaultBufferSegmentCount := iSegmentCount;
    vat.DefaultBufferReadAhead := bReadAhead;
    vat.MarkDirty(@vat.DefaultBufferSegmentSize, sizeof(vat.DefaultBufferSegmentSize));
    vat.MarkDirty(@vat.DefaultBufferSegmentCount, sizeof(vat.DefaultBufferSegmentCount));
    vat.MarkDirty(@vat.DefaultBufferReadAhead, sizeof(vat.DefaultBufferReadAhead));
    SaveVat;
    for t:= 0 to FPayloadStreams.Count-1 do begin
      ps := FPayloadStreams[t];
      if ps = nil then continue;
//      ps.BufferSize := vat.DefaultBufferSegmentSize * vat.DefaultBufferSegmentCount;
//      ps.BufferSEgments := vat.DefaultBufferSegmentCount;
//      ps.DisableLookAhead := not bReadAhead;
    end;
  finally
    Unlock;
  end;



end;

procedure TVirtualDisk_Advanced.SetFileNAme(const Value: string);
var
  c: byte;
begin
  lock;
  try
    if assigned(FVATStream) then begin
      FVATStream.Free;
      FVATStream := nil;
    end;

    if fileexists(value) then
      FVATStream := TVDStream.Create(value, fmOpenReadWrite+fmShareExclusive)
    else begin
      FVATStream := TVDStream.Create(value, fmCreate);
    end;


{$IFNDEF USE_STANDARD_STREAM}
//    FStream.MinimumPrefetchSize := 999999999;
    FVATStream.AllowReadPastEOF := true;
    FVATStream.BufferSize := 256000000;
    FVATStream.BufferSEgments := 256;
    FVATStream.DisableLookAhead := true;
    FVATStream.BufferSize := FCacheSize;
{$ENDIF}


{$ifdef use_vat}
    LoadVat;
    vat.SetOpenMarker;
    SaveVat;
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

  LoadPayloadStreams;
  if scrubber = nil then begin
    scrubber := TExternalEventThread.create(self, nil, true, ScrubberExecute);
    scrubber.start;
  end;


//    Debug.Log('Stream is CReated.  Size is '+inttostr(FStream.size));


  finally
    unlock;
  end;
end;

procedure TVirtualDisk_Advanced.SetPayloadQuota(iFileID: ni; max_size: int64);
begin
  lock;
  try
    if max_size < 0 then max_size := -1;
    vat.PayloadConfig.filelist[iFileID].size_limit := max_size;
    SaveVatAndConfig;
//    FVatStream.FInalizeBuffers;
  finally
    unlock;
  end;
end;

procedure TVirtualDisk_Advanced.SetSize(const Value: int64);
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

{$IFDEF BUFFER_BLOCKS}
procedure TVirtualDisk_Advanced.SyncBuffer(lba: int64; bForWriting: boolean);
var
  iStart, iEnd: ni;
  iDone: ni;
begin
  //determine the LBA we actally want
  lba := lba and (not int64($03));

  if bForWriting then begin
    //if for writing and lba matches... always exit
    if lba = buffer.startingblock then
      exit;
  end else begin
    //if not for writing, exit only if the buffer has been read from disk
    if buffer.ReadFromDisk and (lba = buffer.startingblock) then
      exit;
  end;


  //flush buffer
  iStart := 0;
  //if any blocks are dirtys
  if buffer.AnyDirty then begin
    //if the starting block is valid
    if buffer.startingblock >=0 then
    while iStart < MAX_BUFFERED_BLOCKS do begin
      //find the first dirty block
      iStart := buffer.FirstDirty(iStart);
      if iStart >= MAX_BUFFERED_BLOCKS then break;
      //find the block after the last dirty block
      iEnd := buffer.LastDirty(iStart);

      //write the blocks to disk
      GuaranteeWriteBlocks_Direct(buffer.StartingBlock+iStart, iEnd-iStart, @buffer.data[iStart*512]);
      iStart := iEnd;
    end;
  end;

  //reset the buffer
  buffer.init;
  //setup the starting block for the buffer
  buffer.StartingBlock := lba;

  //if it is invalid location, then don't attempt fetch, get out of here.
  if lba < 0 then
    exit;

  //if it is for reading, then fetch it
  if not bForWriting then begin
    //flag that this buffer was read from disk
    buffer.ReadFromDisk := true;
    //always read max number of blocks
    GuaranteeREadBlocks_Direct(buffer.StartingBlock, MAX_BUFFERED_BLOCKS, @buffer.data[0]);
  end;




end;
{$ENDIF}

procedure TVirtualDisk_Advanced.WriteBlock(const lba: int64; const p: pbyte);
begin
  WriteBlocks(lba, 1, p);
end;

function TVirtualDisk_Advanced.WriteBlocks(const lba: int64; const cnt: nativeint;
  p: pbyte): nativeint;
begin
{$IFDEF BUFFER_BLOCKS}
  result := WriteBlocks_Buffered(lba, cnt, p);
{$ELSE}
  result := WriteBlocks_Direct(lba, cnt, p);
{$ENDIF}
end;

{$IFDEF BUFFER_BLOCKS}
function TVirtualDisk_Advanced.WriteBlocks_Buffered(lba: int64; cnt: nativeint;
  p: pbyte): nativeint;
var
  ioffset, iByteOffset: nativeint;
  iToWrite: nativeint;
begin
  //sync up the buffer to the desired position
  SyncBuffer(lba, true);

  //determine which slot in the buffer we're using
  iOffset := lba and (int64($03));


  //write the data to the buffer
  iByteOffset := $200 * iOffset;
  iToWrite := lesserof(cnt, MAX_BUFFERED_BLOCKS-iOffset);
  result := iToWrite;
  iToWrite := iToWRite * FBlockSize;


  //move data buffer offset $200 * 0,1,2,or 3
  //                                        data
  //                                             size of data (count * blocksize)
  movemem32(@Self.buffer.data[iByteOffset], p,   iToWrite);

  //mark dirty
  buffer.dirty[iOffset] := true;

end;
{$ENDIF}

function TVirtualDisk_Advanced.WriteBlocks_Direct(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
begin
  result := WriteBlocks_Direct_Single(lba, cnt, p);
end;

function TVirtualDisk_Advanced.WriteBlocks_Direct_Parity(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TVirtualDisk_Advanced.WriteBlocks_Direct_Single(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
var
  actual_addr: TVirtualAddressRecord;
  end_actual_addr: TVirtualAddressRecord;
  bDifferentFiles: boolean;
  totalsize: int64;
  startingbigblock: int64;
  startingaddress: int64;
  fs: TVDStream;
  lcnt: nativeint;
begin
  lcnt := cnt;

  result := cnt;

  lock;
  try
    totalsize := lcnt * BlockSize;
    startingbigblock := (lba div _BIG_BLOCK_SIZE_IN_BLOCKS) * _BIG_BLOCK_SIZE_IN_BLOCKS;
    startingAddress := lba * blocksize;
    if (startingaddress) >= (Size+Blocksize) then
      raise ECritical.create('Address beyond the size of the disk! '+inttohex(lba*BlockSize,2));


    GrowIfNeededBlock(lba+(lcnt-1));
    actual_addr := VirtualToPhysicalAddr(lba*FBlockSize);
    end_actual_addr := VirtualToPhysicalAddr((lba+lcnt) * FBlockSize);
{$IFDEF ALLOW_RAID}
    if not (actual_addr.startingblock = end_actual_addr.startingblock) then begin
{$ELSE}
    //if the blocks are not all consecutive, we'll need to reduce the number of blocks we can write
    bDifferentFiles := actual_addr.FileID <> end_actual_addr.FileID;
    if bDifferentFiles or ((end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <> (totalsize)) then begin
{$ENDIF}


      //change the end actual address
      begin
        //find the beginning of the actual_addr bigblock

        end_actual_addr := VirtualToPhysicalAddr(startingbigblock*FBlockSize);
{$IFNDEF ALLOW_RAID}
        end_actual_addr.PhysicalAddress := end_actual_addr.PhysicalAddress + (_BIG_BLOCK_SIZE_IN_BLOCKS * FBlockSize);
{$ELSE}
        end_actual_addr := vat.GetTableEntryForLBA(startingbigblock)^;
        //end_actual_addr := VirtualToPhysicalAddr(((startingbigblock) * _BIG_BLOCK_SIZE_IN_BLOCKS)*FBlockSize);
        end_actual_addr.fileids[0].physical := end_actual_addr.fileids[0].physical+GEtBigblocksizeInBytes(1, false);
{$ENDIF}
      end;

      //actual that CAN be written
{$IFNDEF ALLOW_RAID}
      result := (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) div BlockSize;
{$ELSE}
      result := (end_actual_addr.FileIDs[0].Physical - actual_addr.FileIDs[0].Physical) div BlockSize;
{$ENDIF}
      lcnt := result;
      totalsize := lcnt * BlockSize;
{$IFNDEF ALLOW_RAID}
      if (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <> (totalsize) then begin
        raise ECritical.create('Block segment fault failure.  '+ inttostr((end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress))+','+inttostr(totalsize));
{$ELSE}
      if (end_actual_addr.FileIDs[0].Physical - actual_addr.FileIDs[0].Physical) <> (totalsize) then begin
        raise ECritical.create('Block segment fault failure.  ');
{$ENDIF}

      end;
    end;
//    Debug.Log(inttohex(lba*FBlocksize, 2)+'<-actual_addr(wb)->'+inttohex(actual_addr, 2));




//    Debug.Log('About to write to virtual disk at '+inttohex(actual_addr.Address,0)+' for lba '+inttohex(lba,0)+' lcnt:'+inttostr(lcnt));

{$IFDEF ALLOW_RAID}
    fs := FPayloadStreams[actual_addr.FileIds[0].fileid];
    fs.SeekLock;
    try
    fs.Seek(actual_addr.FileIds[0].physical,0);
{$ELSE}
    fs := FPayloadStreams[actual_addr.FileId];
//    fs.SeekLock;
    try
    fs.Seek(actual_addr.PhysicalAddress,0);
{$ENDIF}
    Stream_GuaranteeWrite(fs, p, FBlockSize*lcnt)
    finally
//      fs.seekunlock;
    end;


  finally
    Unlock;
  end;

end;

procedure TVirtualDisk_Advanced.WriteData(const addr: int64; const cnt: nativeint; p: pbyte);
begin
  Lock;
  try
    if (cnt mod BlockSize) <> 0 then
      raise ECRitical.create('Cnt is not a multiple of block size');

    if (addr mod BlockSize) <> 0 then
      raise ECRitical.create('addr is not a multiple of block size');


    GuaranteeWriteBlocks(addr div BlockSize, cnt div BlockSize, p);
  finally
    Unlock;
  end;
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



{ TAbstractVirtualDisk_Advanced }

procedure TAbstractVirtualDisk_Advanced.BeforeDestruction;
begin
  SaveVatAndConfig;
  inherited;

end;

function TAbstractVirtualDisk_Advanced.BlockCount: int64;
begin
  result := Size div BlockSize;
end;

function TAbstractVirtualDisk_Advanced.BlockMargin: int64;
begin
  result := BlockSize*64;
end;

constructor TAbstractVirtualDisk_Advanced.Create;
begin
  inherited;
  FBlockSize := 512;
{$ifdef use_vat}
  vat.initvirgin;
{$endif}
end;

destructor TAbstractVirtualDisk_Advanced.Destroy;
begin
  inherited;
end;

procedure TAbstractVirtualDisk_Advanced.FlexRead(const addr: int64;
  const cnt: nativeint; p: pbyte);
var
  pblock: pbyte;
  iCan, itot,ipos, iEnd, iOff: int64;
begin
  pblock := getmemory(blocksize);

  try
    fillmem(pblock, blocksize, 0);
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

procedure TAbstractVirtualDisk_Advanced.FlexWrite(const addr: int64;
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

function TAbstractVirtualDisk_Advanced.GetIdentifier: string;
begin
  Lock;
  try
    result := FIdentifier;
  finally
    Unlock;
  end;
end;

procedure TAbstractVirtualDisk_Advanced.GuaranteeReadBlocks(lba: int64; cnt: nativeint;
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
    inc(iRead, iJustRead);
  end;


end;




procedure TAbstractVirtualDisk_Advanced.GuaranteeWriteBlocks(lba: int64; cnt: nativeint;
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
    inc(iWrote, iJustWrote);
  end;
end;


procedure TAbstractVirtualDisk_Advanced.OuterSetBlockSize(const Value: nativeint);
begin
  FBlockSize := Value;
  FSize := (int64(FRequestedSize) div int64(BlockSize)) * int64(FBlockSize);
  SetBlockSize(value);
end;






procedure TAbstractVirtualDisk_Advanced.SetIdentifier(const Value: string);
begin
  Lock;
  try
    FIdentifier := 'iqn-2008-08.com.digitaltundrallc:'+systemx.GetComputerName+'-'+value;
  finally
    Unlock;
  end;
end;

procedure TAbstractVirtualDisk_Advanced.SetSize(const Value: int64);
begin
  FRequestedSize := value;
  fSize := (FRequestedSize div BlockSize) * BlockSize;

end;

function TAbstractVirtualDisk_Advanced.TailStartAddr: int64;
begin
  result := Size - TailSize;
end;

function TAbstractVirtualDisk_Advanced.TailSize: int64;
begin
  result := 8192 * BlockSize;
end;

function TAbstractVirtualDisk_Advanced.VirtualToPhysicalAddr(addr: int64): TVirtualAddressRecord;
{$ifndef use_vat}
begin
  result := addr;
end;
{$else}
var
  r: PVirtualAddressRecord;
  tindex: nativeint;
  varr: TVirtualAddressRecord;
begin
//  debug.log('Input:'+inttostr(addr));

//  result.PhysicalAddress := addr;
  tindex := addr div (BlockSize* _BIG_BLOCK_SIZE_IN_BLOCKS);
  r := vat.GetTableEntryForLBA(addr div blocksize);

//  debug.log('Initial translation:'+inttostr(r.Address));
{$IFDEF ALLOW_RAID}
  if r.FileCount = 0 then begin
{$ELSE}
  if not r.IsAssigned then begin
{$ENDIF}
    //{$MEssage Error 'r.Address can be 0 now... must initialize differently'}

    varr := NewPhysicalBlockAddress((addr div BlockSize) * BlockSize);
    r := vat.GetTableEntryForLBA(addr div blocksize);

{$IFNDEF ALLOW_RAID}
    r.PhysicalAddress := varr.PhysicalAddress;
{$ELSE}
    //r^ := varr;
{$ENDIF}
    //r.Marker := varr.Marker;
{$IFDEF ALLOW_RAID}
    //r.FileIDs[0] := varr.FileIDs[0];
{$ELSE}
    //r.FileID := varr.FileID;
{$ENDIF}
    //r.StartingBlock := tindex * _BIG_BLOCK_SIZE_IN_BLOCKS ;
    vat.MarkTableEntryDirtyByPtr(r);
    SaveVat;
  end;

{$IFDEF ALLOW_RAID}
  result := r^;

{$ELSE}
  result.FileID := r.FileID;
{$ENDIF}
//  ConsoleLog('Prefinal:'+inttostr(r.Address));
{$IFDEF ALLOW_RAID}
  result.fileids[0].Physical := r.fileids[0].Physical+(addr mod (_BIG_BLOCK_SIZE_IN_BLOCKS * BlockSize));
{$ELSE}
  result.PhysicalAddress := r.PhysicalAddress+(addr mod (_BIG_BLOCK_SIZE_IN_BLOCKS * BlockSize));
{$ENDIF}
//  ConsoleLog('Final:'+inttostr(result.Address));

{$IFDEF ALLOW_RAID}
{$ELSE}
  if result.PhysicalAddress < 0 then
    raise ECritical.create('wtf!');
{$ENDIF}


end;
{$endif}

{ TVirtualAddressTable }


{$ifdef use_vat}
function TVirtualAddressTable.DebugVatSTructure: string;
var
  t: ni;
  sl: TStringlist;
begin
  sl := TSTringlist.create;
  try
    t := 0;
    while t< high(self.table) do begin

{$IFDEF ALLOW_RAID}
      if self.table[t].FileIDs[0].Physical >=0 then
        sl.add(inttohex(self.table[t].StartingBlock, 8)+' '+ inttohex(self.table[t].FileIDs[0].Physical, 8)+' '+inttohex(self.table[t].FileIDs[0].Physical,16));
{$ELSE}
      if self.table[t].FileID >=0 then
        sl.add(inttohex(self.table[t].StartingBlock, 8)+' '+ inttohex(self.table[t].FileID, 8)+' '+inttohex(self.table[t].PhysicalAddress,16));

{$ENDIF}
      inc(t);
    end;

    result := sl.text;
  finally
    sl.free;
  end;


end;


procedure TVirtualAddressTable.FlushToStream(s: TStream; bForce:boolean);
var
  iStart, iEnd: int64;
begin
//  if (iStart < 0) and (iEnd < 0) then exit;
  dirtyStart := -1;
  dirtyEnd := -1;

  iStart := dirtyStart;
  iEnd := dirtyEnd;
  if (iStart < 0) or bForce then iStart := 0;
  if (iEnd < 0) or bForce then iEnd := pbyte(@self.persistent_end)-pbyte(@self);

  s.Seek(iStart,0);
//  Debug.Log('Flush:'+NEWLINE+self.DebugVatSTructure);
  Stream_GuaranteeWrite(s, pbyte(@self)+iStart, (iEnd-iStart)+1);
  dirtyStart := -1;
  dirtyEnd := -1;

end;

function TVirtualAddressTable.GetFileCount: cardinal;
var
  t: nativeint;
begin
  if reserved = 0 then begin
    t := 0;
    while t < high(self.payloadconfig.filelist) do begin
      if self.payloadconfig.filelist[T].NAme <> '' then begin
        inc(reserved);
      end;
      inc(t);
    end;
  end;

  result := reserved;


end;


function TVirtualAddressTable.GetTableEntryForLBA(
  lba: int64): PVirtualAddressRecord;
var
  t: nativeint;
begin
  lba := lba shr BIG_BLOCK_SHIFT;
  result := @table[lba];


end;


function TVirtualAddressTable.GetTableEntryPhysicalAddress(iFileID: nativeint;
  physical: int64): PVirtualAddressRecord;
var
  t: nativeint;
  pf : PFilePhysical;
begin
  result := nil;
  for t:= low(table) to high(table) do begin
{$IFDEF ALLOW_RAID}
    pf := table[t].GetFP(iFileID, physical);
    if pf <> nil then begin
      result := @table[t];
      break;
    end;
{$ELSE}
    if (table[t].fileid = iFileID) and (table[t].PhysicalAddress = physical) then begin
      result := @table[t];
      break;
    end;
{$ENDIF}
  end;

//  if result = nil then
//    raise ECritical.create('Physical Address not found.  File is corrupt.');



end;


procedure TVirtualAddressTable.InitVirgin;
var
  t: ni;
begin
  marker := $5555;
  for t:= low(self.table) to high(table) do begin
    table[t].initvirgin;
    table[t].StartingBlock := t*_BIG_BLOCK_SIZE_IN_BLOCKS;
    MarkTableEntryDirty(t);
  end;

end;

function TVirtualAddressTable.LocalAddr(p: pointer): int64;
begin
  result := pbyte(p)-pbyte(@self);

end;

procedure TVirtualAddressTable.MarkDirty(iStart, iEnd: int64);
begin
  if (iStart <self.dirtyStart) or (self.dirtyStart < 0) then begin
    dirtyStart := iStart;
  end;

  if (iENd >self.dirtyEnd) or (self.dirtyEnd < 0) then begin
    dirtyEnd := iENd;
  end;

end;

procedure TVirtualAddressTable.MarkDirty(ptr: pointer; iLEngth: ni);
var
  iStart: int64;
begin
  iStart := localaddr(ptr);
  MArkDirty(iStart, iStart+iLength-1);
end;

procedure TVirtualAddressTable.MarkTableEntryDirty(idx: ni);
var
  iStart, iEnd: int64;
begin
  iStart := (idx * sizeof(TvirtualAddressRecord))+int64(pbyte(@table[0]));
  iEnd := iStart + sizeof(TVirtualAddressRecord)-1;

  MArkDirty(iStart,iEnd);

end;

procedure TVirtualAddressTable.MarkTableEntryDirtyByPtr(
  ptr: PVirtualAddressRecord);
var
  iStart, iEnd: int64;
begin
  iStart := pbyte(ptr)-pbyte(@self.table[0]);
  iEnd := iStart + sizeof(TVirtualAddressRecord)-1;
  if (iStart <self.dirtyStart) or (self.dirtyStart < 0) then begin
    dirtyStart := iStart;
  end;

  if (iENd >self.dirtyEnd) or (self.dirtyEnd < 0) then begin
    dirtyEnd := iENd;
  end;

end;


function TVirtualAddressTable.PersistedSize: ni;
begin
  result := pbyte(@self.persistent_end)-pbyte(@self);

end;

procedure TVirtualAddressTable.ReadFromStream(s: TStream);
var
  sz: ni;
begin
  s.Seek(0,0);
  sz := nativeint(pbyte(@self.persistent_end)-pbyte(@self));
  Stream_GuaranteeRead(s, @self, sz);
//  Debug.Log('Read STructure:'+NEWLINE+self.DebugVatSTructure);
  reserved := 0;
end;
procedure TVirtualAddressTable.SetCloseMarker;
begin
  closeconfirm := opentime;
  MArkDirty(@Closeconfirm, sizeof(Closeconfirm));

end;

procedure TVirtualAddressTable.SetOpenMarker;
begin
  OpenTime := tickcount.GetTicker;
  MarkDirty(@OpenTime, sizeof(opentime));
end;

{$endif}

{ TVirtualAddressRecord }

procedure TVirtualAddressRecord.ChangeFileID(ifrom, ito: ni);
var
  t: ni;
begin
{$IFNDEF ALLOW_RAID}
  fileid := iTO;
{$ELSE}

  for t:= low(fileids) to low(fileids)+FileCount-1 do begin
    if fileids[t].FileID = iFrom then
      fileids[t].FileID := iTo;
  end;
{$ENDIF}

end;

{$IFDEF ALLOW_RAID}
function TVirtualAddressRecord.GetFP(iFileID, iPhysical: int64): PFilePhysical;
var
  i: ni;
begin
  result := nil;
  i := IndexOf(iFileID, iPhysical);
  if i >= 0 then begin
    result := @fileids[i];
  end;
end;
{$ENDIF}

{$IFDEF ALLOW_RAID}
function TVirtualAddressRecord.IndexOf(iFileID, iPhysical: int64): ni;
var
  t: ni;
begin
  result := -1;
  for t:= low(fileids) to (low(fileids)+filecount-1) do begin
    if (fileids[t].FileID = iFileID) and (fileids[t].Physical = iPhysical) then begin
      result := t;
      exit;
    end;
  end;
end;
{$ENDIF}

{$IFDEF ALLOW_RAID}
class operator TVirtualAddressRecord.Equal(const a,
  b: TVirtualAddressRecord): boolean;
begin
  result := a.startingblock = b.startingblock;
end;

function TVirtualAddressRecord.GetFP(iFileID: ni): PFilePhysical;
var
  i: ni;
begin
  result := nil;
  i := IndexOf(iFileID);
  if i >= 0 then begin
    result := @fileids[i];
  end;
end;
{$ENDIF}

{$IFDEF ALLOW_RAID}
function TVirtualAddressRecord.IndexOf(iFileID: ni): ni;
var
  t: ni;
begin
  result := -1;
  for t:= low(fileids) to (low(fileids)+filecount-1) do begin
    if (fileids[t].FileID = iFileID) then begin
      result := t;
      exit;
    end;
  end;

end;
{$ENDIF}

function TVirtualAddressRecord.DebugString: string;
begin
{$IFDEF ALLOW_RAID}
  result := '[va:'+inttohex(self.StartingBlock*512,0)+']'+'[f:RAIDME:RAIDME]';
{$ELSE}
  result := '[va:'+inttohex(self.StartingBlock*512,0)+']'+'[f:'+inttostr(fileid)+'::'+inttohex(self.PhysicalAddress,0);
{$ENDIF}

end;

procedure TVirtualAddressRecord.InitVirgin;
var
  t: ni;
begin
{$IFDEF ALLOW_RAID}
  for t:= low(fileids) to high(fileids) do begin
    fileids[t].InitVirgin;
  end;
{$ELSE}
  FileID := -1;
  PhysicalAddress := -1;
  junk := 1;
{$ENDIF}
  Marker := $5555;

end;

function TVirtualAddressRecord.IsAssigned: boolean;
begin
{$IFNDEF ALLOW_RAID}
  result := (FileID > -1) and (PhysicalAddress > -1);
{$ELSE}
  result := FileCount > 0;
{$ENDIF}
end;

{ TVirtualDiskPayloadFileHeader }

procedure TVirtualDiskPayloadFileHeader.Init;
begin
  _NextPhysicalAddress := sizeof(TVirtualDiskPayloadFileHEader);
end;

function TVirtualDiskPayloadFileHeader.NextPhysicalAddressXX(stride: int64): int64;
begin
  if _NExtPhysicalAddress = 0 then
    _NExtPhysicalAddress := sizeof(self);

  result := _NextPhysicalAddress;

  inc(_NExtPhysicalAddress, stride);

end;

{ TVirtualDiskHub }

constructor TVirtualDiskHub.create;
begin
  inherited;
  vdlist := TVirtualDisklist.create;
  LoadConfiguration;

end;

destructor TVirtualDiskHub.Destroy;
var
  vd: TVirtualDisk;
begin
  while (vdList.count > 0) do begin
    vd := vdList[0];
    vdList.Delete(0);
    vd.DetachAndFree;
    vd := nil;
  end;

  vdList.free;
  inherited;
end;

procedure TVirtualDiskHub.LoadConfiguration;
var
  t,iCount: nativeint;
  ap: TAppParams;
  vd: TVirtualDisk;
begin
  ap := NeedAppParams;
  try
    iCount := ap.GetItemEx('DiskCount', 0);
    for t := 0 to iCount-1 do begin
      vd := TVirtualDisk.create;
      vd.BlockSize := ap.GetItemEx('LUN'+inttostr(t)+':BlockSize', 512);
      vd.Size := ap.GetItemEx('LUN'+inttostr(t)+':Size', int64(256*MEGA));
      vd.FileName := ap.GetItemEx('LUN'+inttostr(t)+':FileName', '');
      vd.Identifier := ap.GetItemEx('LUN'+inttostr(t)+':Identifier', 'LUN'+inttostr(t));
      vd.CacheSize := ap.GetItemEx('LUN'+inttostr(t)+':CacheSize', int64(2048+MEGA));
      vdList.add(vd);


    end;

  finally
    NoNeedAppParams(ap);
  end;
end;

procedure CreateVirtualDiskHub;
begin
  vdh := TVirtualDiskHub.create;

end;
procedure DestroyVirtualDiskHub;
begin
  vdh.free;
  vdh := nil;
end;

procedure oinit;
var
  t: ni;
  tiny, rem: ni;
begin

  for t:= low(bs) to high(bs) do begin
    tiny := 512;
    if t > 1 then begin
      tiny := 512 div (t-1);
      if ((512 mod (t-1)) > 0) then begin
        tiny := tiny + 1;
      end;

    end;
    bs[t] := tiny;
  end;
  CreateVirtualDiskHub;

end;
procedure ofinal;
begin
  DestroyVirtualDiskHub;
end;

{ TVirtualDiskPayloadConfiguration }

procedure TVirtualDiskPayloadConfiguration.AddPayload(sFile: string;
  max_size: int64);
var
  i: ni;
begin
  i := FindUnusedSlot;
  if i < 0 then
    raise ECritical.create('Could not add payload.  Could not find unused payload slot.');


  if fileexists(sFile) then deletefile(sFile);
  filelist[i].NameAsString := sFile;
  filelist[i].size_limit := max_size;


end;

function TVirtualDiskPayloadConfiguration.FindUnusedSlot: nativeint;
var
  t: ni;
begin
  result := -1;
  for t:= 0 to high(filelist) do begin
    if filelist[t].name = '' then begin
      result := t;
      exit;
    end;
  end;

end;

function TVirtualDiskPayloadConfiguration.GetMarhshalSize: nativeint;
var
  t: ni;
begin
  for t := high(filelist) downto low(filelist) do begin
    result := ni(@filelist[t])+sizeof(filelist[t])-ni(@self);
    if filelist[t].name <> '' then begin
      exit;
    end;
  end;

end;

{ TPayloadFileInformation }

function TPayloadFileInformation.GetNameAsString: string;
begin
  result := name;
end;


procedure TPayloadFileInformation.SetNameAsString(const Value: string);
begin
  fillmem(@name[0], sizeof(name), 0);
  movemem32(@name[0], @value[STRZ], length(value)*sizeof(char));

end;

{ TVDBlockBuffer }

{$IFDEF BUFFER_BLOCKS}
function TVDBlockBuffer.AnyDirty: boolean;
var
  t: ni;
begin
  result := false;
  for t := low(dirty) to high(dirty) do begin
    result := dirty[t];
    if result then exit;
  end;

end;

function TVDBlockBuffer.CoversBlock(lba: int64): boolean;
begin
  if (StartingBlock < 0) or (lba < StartingBlock) or (lba >= StartingBlock+MAX_BUFFERED_BLOCKS) then
    result := false
  else
    result := true;

end;

function TVDBlockBuffer.FirstDirty(iAfter: ni): ni;
begin
  result := iAfter;
  while (not dirty[result]) and (result < MAX_BUFFERED_BLOCKS) do
    inc(result);

end;

procedure TVDBlockBuffer.Init;
begin
  fillmem(@self, sizeof(self)-2048, 0);
  StartingBlock := -1;
end;

function TVDBlockBuffer.LAstDirty(iAfter: ni): ni;
begin
  result := iAfter;
  while (dirty[result]) and (result < MAX_BUFFERED_BLOCKS) do
    inc(result);

end;
{$ENDIF}



{ TPayloadStream }

procedure TPayloadStream.AddBigBlock(const pfh: TVirtualDiskPayloadFileHeader);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPayloadStream.CanDropBlock(iAddr: int64;
  out iBlockedBy: int64): boolean;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TPayloadStream.CheckInit;
var
  pfh: TVirtualDiskPayloadFileHeader;
begin
  if self.size < SizeOf(TVirtualDiskPayloadFileHeader) then begin
    pfh.Init;
    self.SetPFH(pfh);
  end;
end;



function TPayloadStream.DropBigBlock(out pfh: TVirtualDiskPayloadFileHeader;
  out p: pbyte; out iSize: int64): boolean;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPayloadStream.Expand(iSize: int64; virtual_addr: int64): int64;
var
  bbh: TVirtualDiskBigBlockHeader;
  addr: int64;
  pfh: TVirtualDiskPayloadFileHeader;
begin
  CheckInit;
  addr := Self.GetNextBigBlockAddress;
  bbh.HeaderStart := addr;
  bbh.TotalSizeIncludingHeaders := iSize+(sizeof(TVirtualDiskBigBlockHeader)*2);
  bbh.PayloadStart := addr + sizeof(TVirtualDiskBigBlockHEader);
  bbh.FooterStart := addr + iSize;
  bbh.VirtualAddress := virtual_addr;
  self.Seek(addr, soBeginning);
  //Header
  Stream_GuaranteeWrite(self, @bbh, sizeof(bbh));
  //payload
  Stream_WriteZeros(self, iSize);
  //footer
  Stream_GuaranteeWrite(self, @bbh, sizeof(bbh));
  result := self.Size;
  pfh := self.GetPFH;
  pfh._NextPhysicalAddress := Size;
  self.SetPFH(pfh);


end;

function TPayloadStream.GetBigBlockHeaderFromPhysicalStart(const addr: int64): TVirtualDiskBigBLockHeader;
begin
  fillmem(@result, sizeof(result), 0);

  if size < addr then
    exit;


  seek(addr-sizeof(TVirtualDiskBigBlockHeader), soBeginning);
  Stream_GuaranteeRead(self, @result, sizeof(result));
end;


function TPayloadStream.GetLastBigBlockFooterAddr: int64;
begin
  result := size-sizeof(TVirtualDiskBigBlockHEader);
end;

function TPayloadStream.GetLastBigBlockHeader: TVirtualDiskBigBlockHeader;
begin
  self.Seek(self.GetLastBigBlockFooterAddr, soBeginning);
  Stream_GuaranteeRead(self, @result, sizeof(result));

end;

function TPayloadStream.GetLastBigBlockHeaderAddr: int64;
begin
  result := size-GetLastBigBlockHeader.TotalSizeIncludingHeaders;
end;

function TPayloadStream.GetLastPayloadAddress: int64;
begin
  result := self.GetLastBigBlockHeader.PayloadStart;
end;

function TPayloadStream.GetNextBigBlockAddress: int64;
begin
  RESULT := getpfh._NextPhysicalAddress;
  if result > size then
    result := size;
end;



function TPayloadStream.GetPFH: TVirtualDiskPayloadFileHeader;
begin
  self.Seek(0,soBeginning);
  Stream_GuaranteeREad(self, @result, sizeof(result));
end;

function TPayloadStream.IsEmpty: boolean;
begin
  result := size <= sizeof(TVirtualDiskPayloadFileHeader);
end;

procedure TPayloadStream.SetNextBigBlockAddress(addr: int64);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TPayloadStream.SetPFH(pfh: TVirtualDiskPayloadFileHeader);
begin
  self.Seek(0,soBeginning);
  Stream_GuaranteeWrite(self, @pfh, sizeof(pfh));

end;

{ TVirtualDiskBigBlockHeader }

function TVirtualDiskBigBlockHeader.PayloadSize: int64;
begin
  result := TotalSizeIncludingHeaders - (sizeof(self)*2);
end;

function TVirtualDiskBigBlockHeader.VAlid(vaddr: int64): boolean;
begin
  result := (TotalSizeIncludingHeaders > (2*sizeof(self)))
    and     (VirtualAddress = vaddr)
    and     (PayloadSTart = (HeaderStart + sizeof(self)));
end;

{ TFilePhysical }

procedure TFilePhysical.InitVirgin;
begin
  FileID := -1;
  Physical := -1;
end;

initialization
  orderlyinit.init.RegisterProcs('VirtualDisk_Advanced', oinit, ofinal, 'ManagedThread,CommandProcessor,ApplicationParams');



end.

