unit VirtualDisk_Advanced_ScubberCollapse;
{x$DEFINE USE_STANDARD_STREAM}
{x$DEFINE VERIFY_WRITES}
{x$DEFINE SMALL_TEST}
{$DEFINE RELEASE}
{$DEFINE USE_VAT}
{$DEFINE PAYLOAD_FILE_HEADER}
{x$INLINE AUTO}
{$DEFINE BUFFER_BLOCKS}
{$DEFINE ALLOW_RAID}
{$DEFINE AlLOW_COMPARE}
{x$DEFINE USE_COMPARATIVE}
{x$DEFINE DETAILED_DEBUGGING}
{$DEFINE ASYNC_READS}
{x$DEFINE USE_LINKED_RAID}
{$Define USE_VAT_HINTS}
{$IFDEF RELEASE}
{$DEFINE RECONST_DEBUG}
{$ENDIF}
{$DEFINE USE_PRIORITY_SORT}
{$DEFINE BRING_ONLINE_DEBUG}
{x$DEFINE MQ}

interface

uses
{$IFDEF MQ}
  globalMultiQueue,
{$ENDIF}
  system.Types, system.rtlconsts, better_collections, ringstats, betterfilestream,
  queuestream, raid, stringx, unittest, numbers, tickcount , applicationparams, standardlist, clusteredpointerlist,helpers.stream, systemx, typex, classes, sysutils, Pgenerics.collections, betterobject, sharedobject, MultiBufferMemoryFileStream, debug, orderlyinit, managedthread, simplequeue,
{$IFDEF ALLOW_COMPARE}
  virtualdisk,
{$ENDIF}
  commandprocessor, virtualdiskconstants, win2003;

const
  PAYLOAD_FLAGS = 0;
  VAT_FLAGS = FILE_FLAG_WRITE_THROUGH;
  MAX_BUFFERED_BLOCKS = RAID_STRIPE_SIZE_IN_BLOCKS; //<----- //MUST DIVIDE EVENLY INTO _BIG_BLOCK_SIZE_IN_BLOCKS
  BUFFER_BLOCK_SHIFT = RAID_STRIPE_SIZE_IN_BLOCKS_SHIFT;  //------|---change together
  BLOCK_BUFFER_SIZE = MAX_BUFFERED_BLOCKS shl BLOCKSHIFT;
  DEFAULT_RAID_CALC_COUNT = 512;

{$IFNDEF SMALL_TEST}
  //BIG_BLOCK_SHIFT = 12;//!<<<<<<<<<<<<<<<-------  CHANGE THESE TWO TOGETHER

  MAX_BLOCKS_IN_VAT = 65536*16;
  MAX_RAID_STRIPES = MAX_BLOCKS_IN_VAT * BIG_BLOCK_SIZE_IN_STRIPES;


{$ELSE}
{$ENDIF}
  INVALID_ADDRESS = $FFFFFFFFFFFFFFFF;

type
{$IFDEF USE_STANDARD_STREAM}
  TVDStream = TFakeMBFS;
{$ELSE}
  TVDStream = TFakeMBFS;
{$ENDIF}

  TVirtualDisk_Advanced = class;//forward

  TVirtualDiskPayloadFileHeader = packed record
    Version:int64;
    _junk_NextPhysicalAddress: int64;
    BufferSegmentSize:int64;
    BufferCount:int64;
    EnableReadAhead:boolean;
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
  PVirtualDiskBigBlockHeader = ^TVirtualDiskBigBlockHeader;

  TPayloadStream = class(TAdaptiveQueuedFileStream)
  private
    FCollapsable: boolean;
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
    stats: TRingStats;
    last_collapse_time: ticker;
    constructor Create(const AFileName: string; Mode: cardinal; Rights: cardinal; Flags: cardinal); override;
    destructor Destroy;override;

    function Expand(iSizeBeyondCurrentExcludingHeaders: int64; virtual_addr: int64): int64;
    procedure CheckInit;
    function IsEmpty: boolean;
    procedure MoveBigBlock(fromAddr, toAddr: int64; sz: int64);
    property Collapsable: boolean read FCollapsable write FCollapsable;

  end;
  TFilePhysical=packed record
    FileID: smallint;
    PhysicalAddr: int64;
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
    FPs: array [0..19] of TFilePhysical;
    FileCount: smallint;
{$ENDIF}
    StartingBlock: int64;
    procedure ChangeFileID(ifrom, ito: fi; newphysical: int64);
    procedure InitVirgin(iStartingBlock: int64);
{$IFDEF ALLOW_RAID}
    function IndexOf(iFileID, iPhysical: int64): fi;overload;inline;
    function IndexOf(iFileID: fi): fi;overload;inline;
    function GetFP(iFileID, iPhysical: int64): PFilePhysical;overload;inline;
    function GetFP(iFileID: fi): PFilePhysical;overload;inline;
    function HasFile(iFileID: fi): boolean;inline;
    procedure RemoveFile(idx: fi);
 {$ENDIF}
{$IFDEF ALLOW_RAID}
    class operator Equal(const a,b: TVirtualAddressRecord): boolean;
{$ENDIF}
    function DebugString: string;
    function IsAssigned: boolean;
    function HasUndefinedLocations: boolean;
  end;
  TPayloadFileInformation = packed record
  strict private
    function GetNameAsString: string;
    procedure SetNameAsString(const Value: string);
  private
    const BIT_FLAG_MISSING = 0;

    function GetFlagMissing: boolean;
    procedure SetFlagMissing(const Value: boolean);

  public

    size_limit: int64;
    used_space:int64;//this is only updated when requested from the client
    physical: smallint;
    priority: smallint;
    flags: cardinal;
    latencyrating: int64;
    name: array[0..2047] of char;

    property NameAsString: string read GetNameAsString write SetNameAsString;
    property Flag_Missing: boolean read GetFlagMissing write SetFlagMissing;



  end;
  PPayloadFileInformation = ^TPayloadFileInformation;

  PVirtualAddressRecord = ^TVirtualAddressRecord;

  TVirtualDiskPayloadConfiguration = packed record
  strict private
    Fstatus: array[0..255] of char;
  public
    filelist: array[0..2047] of TPayloadFileInformation;//<<<!!!!----- -MUsT BE LAST
    procedure AddPayload(sFile: string; max_size: int64; iPhysical: fi; iPriority: fi; iFlags: int64);
    function FindUnusedSlot: nativeint;
    function GetMarhshalSize: nativeint;
    function GetStatus: string;
    procedure SetStatus(const Value: string);
    property Status: string read GetStatus write SetStatus;

  end;

  PVirtualDiskPayloadConfiguration = ^TvirtualDiskPayloadConfiguration;


  TVirtualAddressTable = packed record
    //Version: cardinal;
    Version: int64;
    OpenTime: int64;
    CloseConfirm: int64;
    PayloadConfig: TVirtualDiskPayloadConfiguration;
    reserved: cardinal;
    marker: int64;
    DefaultBufferSegmentCount: int64;
    DefaultBufferSegmentSize:int64;
    DefaultBufferReadAhead: boolean;
    block_count: int64;
    table: array[0..MAX_BLOCKS_IN_VAT-1] of TVirtualAddressRecord;

    dirtyStart: int64;
    dirtyEnd: int64;
    needsbackup: boolean;
    procedure InitVirgin;
    function GetTableEntryForLBA(lba: int64): PVirtualAddressRecord;
    procedure ReadFromStream(s: TStream);
    procedure FlushToStream(s: TStream; bForce: boolean);
    function persistent_End: pbyte;
    function PersistentSize: int64;



  private
    function GetFileCount: cardinal;
    function GetTableEntryPhysicalAddress(iFileID: nativeint;
      physical: int64): PVirtualAddressRecord;
    function FindVARToRAIDUp: PVirtualAddressRecord;
  public
    property FileCount: cardinal read GetFileCount;
    function DebugVatSTructure: string;
    procedure MarkTableEntryDirty(idx: fi);
    procedure MarkTableEntryDirtyByPtr(ptr: PVirtualAddressRecord);
    procedure MarkDirty(const iStart, iEnd: int64);overload;
    procedure MarkDirty(ptr: pointer; iLEngth: fi);overload;
    function PersistedSize: int64;
    procedure SetOpenMarker;
    procedure SetCloseMarker;
    function LocalAddr(p: pointer): int64;
    function FindVARToBuddyUp: PVirtualAddressRecord;
    function FindVARToReconstitute: PVirtualAddressRecord;
    function FindVarWithHighestPhysicalForPayload(iPayloadID: fi): PVirtualAddressRecord;
    function FindFilePhysical(iDISKID: fi; greaterthan: int64): PFilePhysical;
    function VerifyGaps(bThrowExceptions: boolean): boolean;
    function VerifyGapSingle(idx: int64): boolean;
    function GetSafePhysicalAddr(iFileID: fi): int64;
    function HasFile(iPayloadID: fi): boolean;
    procedure ChangeAllFiles(iFromID: fi; iToID: fi);
    function CountPopulatedTableEntries: ni;
    function FindGapLocation(sz: fi; fileid: fi; out physical: int64; not_this_physical: int64): boolean;
    function FindNextBlockForFileIDInVat(fileid: fi; beyond: int64): PVirtualAddressRecord;
  end;

  PVirtualAddressTable = ^TVirtualAddressTable;


  Tcmd_VerifyGapsSinglePart = class(TQueueItem)
  private
    FBlock: int64;
    Fu: ni;
    FResult: boolean;
    fvat: PVirtualAddressTable;
  public
    property result: boolean read FResult write FResult;
    property u: ni read Fu write Fu;
    property block: int64 read FBlock write FBlock;
    property vat: PVirtualAddressTable read fvat write fvat;
    procedure DoExecute;override;
  end;


  Tcmd_VerifyGapsPArt = class(TCommand)
  private
    Fselfie: PVirtualAddressTable;
    FFileIndex: ni;
    Fmodd: ni;
    Fdivd: ni;
  public
    error: string;
    procedure DoExecute;override;
    property selfie: PVirtualAddressTable read Fselfie write FSelfie;
    //property FileIndex: ni read FFileIndex write FFileIndex;
    property modd: ni read Fmodd write Fmodd;
    property divd: ni read Fdivd write FDivd;
  end;


{$ENDIF}

{$IFDEF BUFFER_BLOCKS}
  TVDBlockBuffer = record
  public
    StartingBlock: int64;
    ReadFromDisk: boolean;
    data: array[0..BLOCK_BUFFER_SIZE-1] of byte;
    dirty: array[0..MAX_BUFFERED_BLOCKS-1] of boolean;
    function FirstDirty(iAfter: ni): ni;
    function FirstClean(iAfter: ni): ni;
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
  strict
  private
    FEnableOptionalDebugging: boolean; protected
    function TailSize: int64;
  strict protected
    FopStatus: string;
  private
    function GetStatus: string;
  protected
    FSize: int64;

{$IFDEF USE_VAT_HINTS}
    vathints: array[0..MAX_RAID_STRIPES-1] of boolean;
{$ENDIF}
    FRequestedSize: int64;
    FIdentifier: string;
    procedure SetSize(const Value: int64);virtual;
    function BlockMargin: int64;
{$ifdef use_vat}
    //procedure LoadVat;virtual;abstract;
    //procedure SaveVat;virtual;abstract;
    //procedure SaveVatAndConfig(sTo: string = '');virtual;abstract;
{$endif}
    //function NewPhysicalBlockAddress(virtual_addr: int64): TVirtualAddressRecord;virtual;abstract;
  public
    vat: TVirtualAddressTable;
    constructor Create;override;
    procedure BeforeDestruction;override;
    destructor Destroy;override;
    property Size: int64 read FSize write SetSize;
    function BlockCount: int64;
    function TailStartAddr: int64;

    procedure ReadBlock(const lba: int64; const p: pbyte);virtual;abstract;
    function ReadBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;virtual;abstract;
    function WriteBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;virtual;abstract;
    procedure WriteBlock(const lba: int64; const p: pbyte);virtual;abstract;
    procedure GuaranteeReadBlocks(lba: int64; cnt: nativeint; p: pbyte);
    procedure GuaranteeWriteBlocks(lba: int64; cnt: nativeint; p: pbyte);

    procedure GrowIfNeededBlock(lba: int64);virtual;abstract;
    procedure GrowIfNeededAddr(size: int64);virtual;abstract;
    procedure FlexWrite(const addr: int64; const cnt: nativeint; p:pbyte);
    procedure FlexRead(const addr:int64;const cnt: nativeint; p:pbyte);
    procedure WriteData(const addr: int64; const cnt: nativeint;p:pbyte);virtual;abstract;
    procedure ReadData(const addr: int64; const cnt: nativeint; p: pbyte);virtual;abstract;
    procedure ChangeSingleByte(iAddr: int64; b: byte);virtual;abstract;
    function ReadSingleByte(iAddr: int64): byte;virtual;abstract;
    procedure Preallocate(lba: int64);virtual;abstract;
    property Identifier: string read GetIdentifier write SetIdentifier;
    property EnableOptionalDebugging: boolean read FEnableOptionalDebugging write FEnableOptionalDebugging;
    procedure optdebug(s: string);
    property OperationalStatus: string read FOpStatus write FOpStatus;
    function GEtVerboseDebug(iBigBlock: ni): string;overload;
    function GetVerboseDebug(sToFile: string): boolean;overload;

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




  Tcmd_VirtualDiskBringOnline = class(TCommand)
  private
    Fvd: TvirtualDisk_Advanced;
  public
    exlock: TCLXCriticalSection;
    procedure Init;override;
    destructor Destroy;override;
    property vd: TvirtualDisk_Advanced read Fvd write FVd;
    procedure DoExecute;override;

  end;


  TPayloadPrioritySort = record
    str: TPayloadStream;
    index: ni;
  end;
  TVirtualDisk_Advanced = class(TFileBasedVirtualdisk)
  private
    vatlastbackedup: ticker;
    FCacheSize: int64;
    scrubber: TExternalEVentThread;
    scrubber_file_index: ni;
    bRepaired: boolean;
    FMigrating: boolean;
    FOperational: boolean;
    reconstituting: boolean;
    bInReconstitution: boolean;
    lastconstitutionCheck: ticker;
    writes_at_last_scrub, reads_at_last_scrub: int64;
{$IFDEF BUFFER_BLOCKS}

{$ENDIF}
    cmd_BringOnline: Tcmd_VirtualDiskBringOnline;
    online_bigblocks: array[0..MAX_BLOCKS_IN_VAT-1] of boolean;

    bGapsChecked: boolean;
    FPayloadsOK: boolean;
    FCAchedSTripes: ni;
    FAllowDriveSkipping: boolean;
    FprioritySort: array of TPayloadPrioritySort;

    function BringBigBlockOnline(idx: ni): boolean;
    procedure SetCAcheSize(const Value: int64);
    procedure ScrubberExecute(thr: TExternalEventThread);
    procedure EvalRunHot(thr: TExternalEventThread);
    procedure BackupVat;
    procedure FlushBuffers(iOlderThan: ticker; bHalf: boolean = false; bOnlyIfALLDirty: boolean= false);
    function BuddyUp: boolean;
    function RaidUP: boolean;
    function Reconstitute(pvar: PVirtualAddressRecord = nil): boolean;
    function ForceReconstitution(pvar: PVirtualAddressRecord): boolean;
    function FindHighestOverVar(fileid: ni): PVirtualAddressRecord;
    function Collapse(fileid: ni): boolean;
    function FindHighestOverVar_AfterCollapse(fileid: ni): PVirtualAddressRecord;

    procedure RepairbigBlock(varr: PVirtualAddressRecord; ps: TPayloadStream; pbbh: TVirtualDiskBigBlockHeader);
    procedure Repair_payload(ps: TPayloadStream; id: ni);
    function GEtOverQuotaStream(out id: nativeint): TPayloadStream;
    function GEtUnderQuotaStream(notOf: PVirtualAddressREcord; out id: nativeint; bOnlyReadyStreams: boolean; iAllowPhysical: ni = -1): TPayloadStream;overload;
    function PhysicalIsInVar(vart: PVirtualAddressRecord; iPhysical: ni): boolean;
    function GEtUnderQuotaStreamxx(notOf: PVirtualAddressREcord; out id: nativeint): TPayloadStream;overload;
    procedure MovePayloadBigBlock(pvar: PVirtualAddressRecord; FromID, toID: nativeint; fromstream, tostream: TPayloadStream; bMirror: boolean);
    procedure CheckVatSanity;
    procedure SortPayloadPriorities;

    function HasRaid(startingblock: int64): boolean;inline;
    function IndexOfRaid(startingBlock: int64): ni;inline;
    function NeedRaid(startingblock: int64): TRaidCalculator;inline;

    procedure FetchRaid(r: TRaidCalculator);
    procedure FetchAndMergeRaid(r: TRaidCalculator);
    procedure FlushRaid(r: TRaidCalculator);
    function IsStreamUnderQuota(t: nativeint): boolean;
    function VirtualToPhysicalAddr(addr: int64): TVirtualAddressRecord;
    function GetBusiestPhysical(const vart: PVirtualAddressRecord): ni;
    procedure Shovel(thr: TExternalEventThread; bShrinkOnly: boolean);
    procedure ShrinkPayloads;
    function GEtCachedSTripes: ni;
    procedure SetCachedSTripes(const Value: ni);
    function GetLeastBusyPhysical(): ni;

  protected
    procedure ReadBlock(const lba: int64; const p: pbyte);override;
    procedure WriteBlock(const lba: int64; const p: pbyte);override;

    procedure SetSize(const Value: int64);override;
    function GetfileName: string;override;
    procedure SetFileNAme(const Value: string);override;
{$ifdef use_vat}
    procedure LoadVat;//override;
    procedure QuickCheckVat;
    procedure SaveVat(bAll: boolean);//override;
    procedure SaveVatAndConfig(sTo: string = '');//override;
    function NewPhysicalBlockAddress(virtual_addr: int64): TVirtualAddressRecord;
{$endif}
  public
    writes: int64;
    reads: int64;

    lastpayloadchange: ticker;
    bRaidFetched: boolean;
    Repairlog: TStringlist;
    FRaids: TRaidList;
    FVATStream: TVDStream;
    FPayloadStreams: TList<TPayloadStream>;
//    FCurrentVat: PVirtualAddressRecord;
    property AllowDriveSkipping: boolean read FAllowDriveSkipping write FAllowDriveSkipping;
    constructor Create;override;
    destructor Destroy;override;
    procedure BeforeDestruction;override;
    procedure LoadPayloadStreams;
    procedure StopBringOnline;
    procedure StartBringOnline;
    procedure DestroyPayloadStreams;
    property PayloadsOk: boolean read FPayloadsOK;
    property Operational: boolean read FOperational;

    property Migrating: boolean read FMigrating write FMigrating;
    procedure LogRepair(s: string);
    function GetRepairLog: string;
    function DrainRepairLog: string;
    procedure ClearRepairLog;


    function ReadBlocks_Direct_Single(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
    function WriteBlocks_Direct_Single(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;



    function ReadBlocks_RaidCalc(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;inline;
    function WriteBlocks_RaidCalc(const rc: TRaidCalculator; const lba: int64; const cnt: nativeint; p: pbyte): nativeint;inline;

    function ReadBlocks_Direct(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
    function WriteBlocks_Direct(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;


{$IFDEF BUFFER_BLOCKS}
    function ReadBlocks_Buffered(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
    function WriteBlocks_Buffered(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
{$ENDIF}

{$IFDEF BUFFER_BLOCKS}
    function SyncAwayBuffer(idx: ni = -1; bOnlyIfallDirty: boolean = false): boolean;
    procedure SyncBuffer(lba: int64; bForWriting: boolean);inline;
{$ENDIF}
    procedure GuaranteeReadBlocks_Direct(lba: int64; cnt: nativeint; p: pbyte);
    procedure GuaranteeWriteBlocks_Direct(lba: int64; cnt: nativeint; p: pbyte);




    function ReadBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;override;
    function WriteBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;override;
    procedure WriteData(const addr: int64; const cnt: nativeint;p:pbyte);override;
    procedure ReadData(const addr: int64; const cnt: nativeint; p: pbyte);override;


    procedure GrowIfNeededBlock(lba: int64);override;
    procedure GrowIfNeededAddr(size: int64);override;
    procedure ChangeSingleByte(iAddr: int64; b: byte);override;
    function ReadSingleByte(iAddr: int64): byte;override;
    procedure Preallocate(lba: int64);override;
    property CacheSize: int64 read FCacheSize write SetCAcheSize;
    function DebugVatStructure: string;
    procedure AddPayload(sFile: string; size_limit: int64; iPhysical: ni; iPriority: ni; iFlags: int64);
    function GetPayloadConfig: PVirtualDiskPayloadConfiguration;
    procedure SetPayloadQuota(iFileID: ni; max_size: int64);
    procedure SetPayloadPhysical(iFileID: ni; physical: int64);
    procedure SetPayloadPriority(iFileID: ni; priority: int64);
    procedure SetDefaultCacheParams(iSegmentSize, iSegmentCount: ni; bReadAhead: boolean);
    procedure DecommissionPayload(sFile: string);
    procedure UnpauseScrubber;
    procedure ReFunctPayload(iPayloadID: ni; sNewSource: string);
    procedure ReSourcePayload(iPayloadID: ni; sNewSource: string);
    procedure BeginRepair;
    property CachedSTripes: ni read GEtCachedSTripes write SetCachedSTripes;
    procedure QuickOnline;
  end;

  TVirtualDisk_Comparative = class(TFileBasedVirtualdisk)
  private
    FCAcheSize: int64;
    FJunk: int64;
    function GetOperational: boolean;
  public
    comp1: TVirtualDisk_Advanced;
    comp2: virtualdisk.TVirtualDisk_SimpleNonLinear;
    constructor Create;override;
    destructor Destroy;override;


    procedure ReadBlock(const lba: int64; const p: pbyte);override;
    procedure WriteBlock(const lba: int64; const p: pbyte);override;

    procedure SetSize(const Value: int64);override;
    function GetfileName: string;override;
    procedure SetFileNAme(const Value: string);override;
    function ReadBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;override;
    function WriteBlocks(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;override;
    procedure WriteData(const addr: int64; const cnt: nativeint;p:pbyte);override;
    procedure ReadData(const addr: int64; const cnt: nativeint; p: pbyte);override;

    property CAcheSize: int64 read FCAcheSize write FCacheSize;

    procedure AddPayload(sFile: string; size_limit: int64; iPhysical: ni; iPriority: ni; iFlags: int64);
    procedure DecommissionPayload(sFile: string);
    function GetPayloadConfig: PVirtualDiskPayloadConfiguration;
    procedure SetDefaultCacheParams(iSegmentSize, iSegmentCount: ni; bReadAhead: boolean);
    procedure SetPayloadQuota(iFileID: ni; max_size: int64);
    property Operational: boolean read GetOperational;
    procedure ClearRepairLog;
    function GetRepairLog: string;
    function DrainRepairLog: string;
    procedure BeginRepair;
    function DebugVatStructure: string;
    procedure RefunctPayload(id: ni; sFile: string);
    procedure ReSourcePayload(id: ni; sFile: string);
    procedure SetPayloadPhysical(id: ni; phys: ni);
    procedure SetPayloadPriority(id: ni; pri: ni);
    procedure UnpauseScrubber;
    property CachedStripes: int64 read FJunk write FJunk;
    procedure QuickOnline;
  end;

  TVirtualDisk_Fake = class(TVirtualDisk_SimpleNonLinear)
  public
    procedure AddPayload(sFile: string; size_limit: int64; iPhysical: ni; iPriority: ni; iFlags: int64);
    procedure DecommissionPayload(sFile: string);
    function GetPayloadConfig: PVirtualDiskPayloadConfiguration;
    procedure SetDefaultCacheParams(iSegmentSize, iSegmentCount: ni; bReadAhead: boolean);
    procedure SetPayloadQuota(iFileID: ni; max_size: int64);
    procedure BeginRepair;


  end;

{$IFDEF USE_COMPARATIVE}
  TVirtualDisk = class(TVirtualDisk_Comparative);
{$ELSE}
  TVirtualDisk = class(TVirtualDisk_Advanced);
{$ENDIF}




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


function IsWithin(iStart, iSpan: int64; iTest: int64): boolean;

function GetBigBlockSizeInBytes(iRAidCount: ni; bIncludingHeaders: boolean = false): ni;inline;

function BigBlockAlign(virt: int64): int64;inline;
function RaidStripeAlign(virt: int64): int64;inline;

var
  vdh: TVirtualDiskHub;


implementation

{ TVirtualDisk }

function IsWithin(iStart, iSpan: int64; iTest: int64): boolean;
begin
  if iTest < iStart then begin
    result := false;
    exit;
  end;

  if iTest >= iStart + iSpan then begin
    result := false;
    exit;
  end;
  result := true;
end;

function BigBlockAlign(virt: int64): int64;inline;
begin
  result := (virt div (BLOCKSIZE*_BIG_BLOCK_SIZE_IN_BLOCKS)) *(BLOCKSIZE*_BIG_BLOCK_SIZE_IN_BLOCKS);
end;

function RaidStripeAlign(virt: int64): int64;inline;
begin
  result := (virt div RAID_STRIPE_SIZE_IN_BYTES) *RAID_STRIPE_SIZE_IN_BYTES;

end;
function GetBigBlockSizeInBytes(iRAidCount: ni; bIncludingHeaders: boolean = false): ni;inline;
begin
  result := TRaidCalculator.GetPieceSizePAdded(iRaidCount) * (_BIG_BLOCK_SIZE_IN_BLOCKS shr RAID_STRIPE_SIZE_IN_BLOCKS_SHIFT);
  if bIncludingHeaders then
    result := result + (2*sizeof(TVirtualDiskBigBlockHeader));
end;


procedure TVirtualDisk_Advanced.AddPayload(sFile: string; size_limit: int64; iPhysical: ni; iPriority: ni; iFlags: int64);
begin
  Lock;
  try
    if size_limit < 0 then size_limit := -1;
    lastpayloadchange := getticker;
    vat.PayloadConfig.AddPayload(sfile, size_limit, iPhysical, iPriority, iFlags);
    SaveVatAndConfig;
    FVatStream.FinalizeBuffers;
    LoadPayloadStreams;

  finally
    Unlock;
  end;
end;




procedure TVirtualDisk_Advanced.BackupVat;
var
  s: string;
  t: ni;
  ps: TPayloadStream;
  r:ni;
begin
  if GetTimeSince(self.vatlastbackedup) > 300000 then begin
    lock;
    try
      if vat.needsbackup then begin
        r := GetLeastBusyPhysical();
        t := r;
        //for t:= 0 to FPayloadStreams.count-1 do begin
          ps := FPayloadStreams[t];
          if ps <> nil then begin
            try
              SaveVatAndConfig(ps.FileNAme+'.vat.backup');
            except
            end;
          end;
        //end;
        vat.needsbackup := false;
      end;

      vatlastbackedup := getticker;

    finally
      unlock;
    end;
  end;
end;

procedure TVirtualDisk_Advanced.BeforeDestruction;
var
  t: ni;
begin
  inherited;
  scrubber.stop;
  scrubber.WaitForFinish;
  tpm.NoNeedThread(scrubber);
  scrubber := nil;
  StopBringOnline;



  for t:= 0 to FRaids.count-1 do begin
    SyncAwayBuffer(t);
  end;

  SyncBuffer(-1, true);
  SaveVatAndConfig;
end;

procedure TVirtualDisk_Advanced.BeginRepair;
begin
  bRepaired := false;
end;

function TVirtualDisk_Advanced.BringBigBlockOnline(idx: ni): boolean;
var
  t,u: ni;
  ta: PVirtualAddressRecord;
  pfh: TVirtualDiskPayloadFileHeader;
  ps, ps2: TPayloadStream;
  bbh: TVirtualDiskBigBLockHeader;
  a,b: int64;
  itemp: int64;
  bChanged: boolean;
begin
  //if the block is already online then forget it
  result := false;
  if online_bigblocks[idx] then
    exit;

  result := true;

  bChanged := false;

  lock;
  try


    t := idx;
    vat.payloadconfig.Status := 'Bring Online @'+inttohex(idx,0);


    ta := @self.vat.table[t];

{$IFDEF BRING_ONLINE_DEBUG}if ta.filecount > 0 then Debug.ConsoleLog('Bring Online @'+inttohex(idx,0));{$ENDIF}

    //check that the block does not overlap other blocks
    if not vat.VerifyGapSingle(idx) then begin
      LogRepair('GAP ERROR! @idx='+inttohex(idx,0)+' will force reconstitution.');
      ForceReconstitution(ta);
    end;


    //if any files are idx -1 then we need to reconstitute this block
    if ta.HasUndefinedLocations then begin
      if not bInReconstitution then begin
        LogRepair('VAR has undefined locations! @idx='+inttohex(idx,0)+' will force reconstitution.');

        LogRepair('Before:'+NEWLINE+vat.DebugVatSTructure);
        ForceReconstitution(ta);
        LogRepair('After:'+NEWLINE+vat.DebugVatSTructure);
      end;
    end;

    //go through each of the files
    for u := 0 to ta.FileCount-1 do
    //check that the vat entry exists in the physical
    if ta.FPs[u].fileid >= 0 then begin
      ps := FPayloadStreams[ta.FPs[u].fileid];
      if ps <> nil then begin

        //IF THERE ARE TWO OR FEWER entires (mirrored or single)
        if ta.FileCount < 3 then begin
          //if the stream is missing break the mirror or drop alltogether
          if (ta.FPs[u].fileid > (self.FPayloadStreams.count-1)) or (self.FPayloadStreams[ta.FPs[u].fileid] = nil) then begin
            LogRepair('Breaking mirror at '+inttostr(t)+' was '+inttostr(ta.FPs[u].fileid));
            ta.removefile(u);
            bChanged := true;
          end;

          //if the physical address is beyond the end of the file, then break/drop
          a := (ta.FPs[u].physicalAddr + GetBigBlockSizeInBytes(1,false)+sizeof(TVirtualDiskBigBlockHeader));
          b :=  FPayloadStreams[ta.FPs[u].fileid].Size;
          if a > b then begin
            LogRepair('Fixing bogus fileid at '+inttostr(t)+' was '+inttostr(ta.FPs[u].fileid)+' a='+inttostr(a)+' b='+inttostr(b));
            ta.removefile(u);
            vat.MarkTableEntryDirtyByPtr(ta);
            SaveVat(false);
            bChanged := true;
          end;

          //if the big block header in the payload is not valid, break/drop
          if ta.FPs[u].physicalAddr > 0 then begin
            bbh := ps.GetBigBlockHeaderFromPhysicalStart(ta.FPs[u].physicalAddr);
            if not bbh.VAlid(ta.StartingBlock shl BLOCKSHIFT) then begin
              LogRepair('BIg Block header is bogus, removing from vat');
              ta.removefile(u);
              vat.MarkTableEntryDirtyByPtr(ta);
              SaveVat(false);
              bChanged := true;
            end;
          end;
        end
        //else if there are more than 2-files, operate in raid mode
        else begin
          //determine how big the file SHOULD be to accomodate this block
          iTemp := ta.FPs[u].PhysicalAddr+ GetBigBlockSizeInBytes(ta.filecount, false)+sizeof(TVirtualDiskBigBlockHeader);
          //if the file isn't big enough, then expand the file to it's intended size
          if ps.Size < iTemp then begin
            iTEmp := vat.GEtSafePhysicalAddr(ta.FPs[u].FileID);
            LogRepair('File has incomplete block, growing to '+inttostr(iTemp));
            ps.GrowFile(vat.GEtSafePhysicalAddr(ta.FPs[u].FileID));
            LogRepair('RAID5 piece could not be found. Expanding file size to accomodate.');
            ta.FPs[u].PhysicalAddr := ps.Expand(GetBigBlockSizeInBytes(ta.filecount, false), ta.StartingBlock shl BLOCKSHIFT);
            bChanged := true;
          end;
        end;
      end;

      //
      if ta.FPs[u].fileid < 0 then
        ps2 := nil
      else
        ps2 := self.FPayloadStreams[ta.FPs[u].fileid];
      if ps2 = nil then begin
        //if we're here, then we have blocks assigned to files that officially don't exist anymore
        //these blocks cannot be found with a re-Source
        //therefore we will mark them as -1 and, if there is enough redundancy,
        //they can be rebuilt via Reconstitution.

        //NOT SURE WE WANT TO DO THIS
        //ta.FPs[u].physicaladdr := -1;
        //ta.FPs[u].FileID := -1;
      end else
      if self.FPayloadStreams[ta.FPs[u].fileid].Size > ta.FPs[u].physicalAddr then begin
        //check that the vat entry is not truncated
        a := self.FPayloadStreams[ta.FPs[u].fileid].Size;
        b := (ta.FPs[u].physicalAddr + (1*sizeof(TVirtualDiskBigBlockHeader)) + GetBigBlockSizeInBytes(ta.filecount));
        if a >= b then begin
            //good
        end else begin
          LogRepair('Fixing:'+inttohex(ta.StartingBlock,2));
          //truncate file... this block is shit
          self.FPayloadStreams[ta.FPs[u].fileid].Size := ta.FPs[u].physicalAddr;

{$IFDEF REDUNDANT_PHYSICAL}
          pfh := self.FPayloadStreams[ta.FPs[u].fileid].GetPFH;
          pfh._NextPhysicalAddress := ta.FPs[u].physical;
          self.FPayloadStreams[ta.FPs[u].fileid].SetPFH(pfh);
{$ENDIF}

          ta.removefile(u);//<--- should mark the fileid -1 so that we can reconstitute
          ForceReconstitution(ta);
          bChanged := true;
        end;
      end;

    end;


    if bChanged then begin
      vat.MarkDirty(0, sizeof(vat));
      SaveVAt(false);
    end;

    bRepaired := false;

    online_bigblocks[idx] := true;
  finally
    unlock;
  end;
end;

function TVirtualDisk_Advanced.BuddyUp: boolean;
var
  vart: PVirtualAddressRecord;
  toID: ni;
  toS, fromS: TPayloadStream;
begin

  result := false;

  Lock;
  try
    vart := vat.FindVARToBuddyUp;

    if vart <> nil then begin
      toS := GEtUnderQuotaStream(vart, toID, false);
      if toS <> nil then begin
        vat.PayloadConfig.status := 'buddying up';
        fromS := FPayloadStreams[vart.FPs[0].fileid];
        if fromS <> nil then begin
          MovePayloadBigBlock(vart, vart.FPs[0].fileid, toID, fromS, toS, true);
          result := true;
        end else
          result := false;

      end;
    end;


  finally
    Unlock;
  end;

end;

procedure TVirtualDisk_Advanced.ChangeSingleByte(iAddr: int64; b: byte);
var
  block: array[0..BLOCKSIZE-1] of byte;
  iOFF: int64;
begin
  self.ReadBlock(iAddr shr BLOCKSHIFT, @block[0]);
  iOFF := iAddr mod BlockSize;

  block[iOFF] := b;
  self.WriteBlock(iAddr shr BLOCKSHIFT, @block[0]);



end;

constructor TVirtualDisk_Advanced.Create;
begin
  inherited;

  RepairLog := TStringlist.create;
  FCacheSize := 256*MEGA;
  FPayloadStreams := TList<TPayloadStream>.create;

  vat.InitVirgin;
  FRaids := TRaidList.create;

  CachedSTripes := 32;


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

function TVirtualDisk_Advanced.GEtUnderQuotaStreamxx(notOf: PVirtualAddressREcord; out id: nativeint): TPayloadStream;
var
  t: nativeint;
  ps: TPayloadStream;
begin
  result := GetUnderQuotaStream(notof, id, false);
  if result = nil then
    result := GEtUnderQuotaStream(notof,id, true);


{
  for t:= 0 to FPayloadStreams.count-1 do begin
    ps := FPayloadStreams[t];
    if t = id then continue;
    if ps <> nil then begin
      ps.Lock;
      try
        ps.BeginFLushandPrefetch(-1);
      finally
        ps.unlock;
      end;
    end;
  end;}
end;
procedure TVirtualDisk_Advanced.MovePayloadBigBlock(pvar: PVirtualAddressRecord; FromID, toID: nativeint; fromstream,tostream: TPayloadStream; bMirror: boolean);
var
  pfh_from, pfh_to: TVirtualDiskPayloadFileHeader;
var
  buf: pointer;
  i: int64;
  npa: int64;
  //pva: PVirtualAddressRecord;
  bbh_from, bbh_to: TVirtualDiskBigBLockHeader;
  pf: PFilePhysical;
  sOP: string;
begin
{$IFDEF USESEEKLOCK}  fromstream.SeekLock;{$ENDIF}
{$IFDEF USESEEKLOCK}  tostream.SeekLock;{$ENDIF}
  try

  if bMirror then sOP := 'Copy' else sOp := 'Move';

//  debug.consolelog(inttostr(toStream.size)+' // '+inttostr(fromstream.size));

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
{$IFDEF REDUNDANT_PHYSICAL}
  pfh_from._NextPhysicalAddress := i;
{$ENDIF}
  bbh_from := fromStream.GetBigBlockHeaderFromPhysicalStart(pvar.GetFP(fromId).PhysicalAddr);

  //expand the target stream
  npa := toStream.Expand(bbh_from.PayloadSize, bbh_from.VirtualAddress);
  pfh_to := toStream.GetPFH;

  bbh_to := toStream.GetLastBigBlockHeader;

  //make sure we have an entry for this
  if (pvar <> nil) then begin

    //buf := getmemory(1024000);
    try
      //tostream.CopyFrom(fromstream,  GetBigBlockSizeInBytes(1));
//      Debug.ConsoleLog(sOP+' From:'+inttostr(fromid)+':???? to '+inttostr(toid)+':'+inttohex(npa,0));
      fromstream.Seek(bbh_from.PayloadStart, 0);
      tostream.Seek(bbh_to.PayloadStart,0);
      //tostream.CopyFrom(fromstream, bbh_from.PayloadSize);


//      Stream_GuaranteeRead(fromstream, buf,  bbh_from.PayloadSize);
//      Stream_GuaranteeWrite(tostream, buf,  bbh_to.PayloadSize);
      Stream_GuaranteeCopy(fromStream, tostream, bbh_from.PayloadSize);

//      Debug.ConsoleLog('Completed '+sOP+' From:'+inttostr(fromid)+':???? to '+inttostr(toid)+':'+inttohex(npa,0));

    finally
      //Freememory(buf);
    end;

    //Save physical Addres to "to" file
    toStream.SetPFH(pfh_to);

    //change fileid in vat
{$IFDEF ALLOW_RAID}
    if bMirror then begin
      pvar.FPs[pvar.FileCount].FileID := toID;
      inc(pvar.FileCount);
      pf := pvar.GetFP(toid);
      pf.PhysicalAddr := npa;
    end else begin
      pvar.ChangeFileID(fromid, toid,bbh_to.PayloadStart);
      pf := pvar.GetFP(toid);
      if pf <> nil then begin
        pf.PhysicalAddr := npa;
      end;
    end;


{$ELSE}
    pva.FileID := toID;
    pva.PhysicalAddress := npa;
{$ENDIF}

    vat.MarkTableEntryDirtyByPtr(pvar);
    SaveVat(false);

  end;

  //save physical address back to "from" file
{$IFDEF REDUNDANT_PHYSICAL}
  pfh_from._NextPhysicalAddress := fromStream.GetLastBigBlockHeaderAddr;
  fromStream.SetPFH(pfh_from);
{$ENDIF}



  //reduce size of from file (unless we're copying for buddy-up purposes)
  if not bMirror then begin
    fromSTream.Size := fromStream.Size - GetBigBlockSizeInBytes(pvar.FileCount, true);
  end;
  finally
{$IFDEF USESEEKLOCK}    fromstream.seekunlock;{$ENDIF}
{$IFDEF USESEEKLOCK}    tostream.SeekUnlock;{$ENDIF}
  end;

end;
procedure TVirtualDisk_Advanced.CheckVatSanity;
begin
  exit;

end;
procedure TVirtualDisk_Advanced.ClearRepairLog;
begin
  Lock;
  try
    RepairLog.Clear;
  finally
    Unlock;
  end;
end;

function TVirtualDisk_Advanced.Collapse(fileid: ni): boolean;
var
  pvar: PVirtualAddressRecord;
  sz: integer;
  gap_address: int64;
  ps: TPayloadStream;
  fp: PFilePhysical;
begin
  result := false;

  if FPayloadStreams[fileid].Collapsable = false then
    exit;//if the hint is false, then exit for performance purposes

  //get the highest over var for the particular file
  pvar := FindHighestOverVar(fileid);
  if pvar = nil then
    exit;

  //determine the size of the payload
  sz := GetBigBlockSizeInBytes(pvar.FileCount, true);


  ps := self.FPayloadStreams[fileid];
  fp := pvar.GetFP(fileid);
  //query VAT to find a gap that is suitable for this block
  if self.vat.FindGapLocation({size required}sz, {fileid}fileid,{out} gap_address, fp.physicaladdr) then begin
    //if a gap was found, copy the data from the old location to the new one.



    Debug.ConsoleLog('Moving Big Block from: '+inttohex(fp.physicaladdr,1)+' to '+inttohex(gap_address,1));
    ps.MoveBigBlock(fp.PhysicalAddr, gap_address, sz);
    fp.PhysicalAddr := gap_address;
    self.vat.MarkTableEntryDirtyByPtr(pvar);
    self.SaveVat(false);
  end else begin
    FPayloadStreams[fileid].collapsable := false;/// this is a hint, but it may not be accurate because "sz" may vary... but it won't totally be the end of the world if collapsing is postponed

  end;




end;

procedure TVirtualDisk_Advanced.ScrubberExecute(thr: TExternalEventThread);
var
  underid: nativeint;
  overid: nativeint;
  over, under: TPayloadStream;
  tm3,tm1,tm2: ticker;
  pvar: PVirtualAddressRecord;
begin
  try
    Foperational := PayloadsOK;

    if operational then begin
      FlushBuffers(60000);
      BackupVat;
      EvalRunHot(thr);
    end;

  if cmd_BRingOnline <> nil then
    ecs(cmd_BringOnline.exlock);
  try

//  if assigned(cmd_BringOnline) and (not cmd_BringOnline.IsComplete) then
//    exit;

    if not PayloadsOk then
      exit;


    if GetTimeSince(lastpayloadchange) < 60000 then begin
      thr.coldruninterval := 1000;
      exit;
    end;


    //buddy shovelling
    Lock;
    try
      tm1 := GetTicker;
      if BuddyUp then begin//if stuff was done in the buddy-up function, then exit immediately
        tm2 := GetTicker;
        thr.coldruninterval := GetTimeSince(tm2,tm1)*3;
        exit;
      end;
    finally
      Unlock;
    end;

    //raid shovelling
    Lock;
    try
      if RaidUP then begin
        tm2 := GetTicker;
        thr.coldruninterval := GetTimeSince(tm2,tm1);
        exit;
      end;
    finally
      Unlock;
    end;

    //reconstitution
    if (GEtTimeSince(lastconstitutionCheck) > 60000) or reconstituting then begin
      tm1 := GetTicker;
      reconstituting := Reconstitute;
      if reconstituting then
        Shovel(thr, true);
      tm2 := getTicker;
      thr.ColdRunInterval := GetTimeSince(tm2,tm1);
      lastconstitutionCheck := getTicker;
      if reconstituting then exit;
    end;

    //single shovelling
    Shovel(thr, false);
  finally
    writes_at_last_scrub := writes;
    reads_at_last_scrub := reads;
  end;
  finally
    if cmd_BRingOnline <> nil then
      lcs(cmd_BringOnline.exlock);
  end;



end;

procedure TVirtualDisk_Advanced.Shovel(thr: TExternalEventThread; bShrinkOnly: boolean);
var
  underid: nativeint;
  overid: nativeint;
  over, under: TPayloadStream;
  tm1,tm2: ticker;
  pvar: PVirtualAddressRecord;
  pf: PFilePhysical;
begin
  Lock;
  try

//    Debug.Log('Scrub in');

    //determine OVER quota stream
    over := GEtOverQuotaStream(overid);

    if assigned(over) then begin

      //find VAR with highest address for that fileid
      pvar := FindHighestOverVar_AfterCollapse(overid);


      if pvar = nil then begin
        self.vat.PayloadConfig.Status := 'There are over-quota payloads, but  they do not have entires in the VAT.  Freeing space.';
        over.Size := 0;
        over.CheckInit;

        thr.RunHot := false;
      end else begin

        self.vat.PayloadConfig.Status := 'Migrating.';
        //find a stream to move this block to that isn't already in use by the VAR and is
        under := GetUnderQuotaStream(pvar, underid, false, vat.PayloadConfig.filelist[overid].physical);

        if assigned(under) then begin
          self.vat.PayloadConfig.Status := 'There are over-quota payloads, but  they do not have entires in the VAT.  Freeing space.';
          Migrating := true;
  //        Debug.ConsoleLog(self.vat.DebugVatSTructure);
  //        Debug.ConsoleLog('AFter:');
          tm1 := GEtTicker;
          MovePayloadBigBlock(pvar, overid, underid, over,under,false);
          tm2 := getTicker;
          thr.ColdRunInterval := greaterof(gettimesince(tm2,tm1)*1,10);


  //        Debug.ConsoleLog(self.vat.DebugVatSTructure);
          //thr.RunHot := true;
        end else begin
          thr.RunHot := false;
          Migrating := false;
        end;
      end;
    end else begin
       self.vat.PayloadConfig.Status := 'Nothing is migrating.';
       Migrating := false;
       thr.ColdRunInterval := 10000;
       thr.runhot := false;
      CheckVATSanity;
    end;
  finally
//    Debug.Log('Scrub out');
    Unlock;
  end;






end;

procedure TVirtualDisk_Advanced.ShrinkPayloads;
var
  expectedsize: int64;
  t: ni;
  ps: TPayloadStream;
  vart: PVirtualAddressRecord;
  fp: PFilePhysical;
  str: TPayloadStream;
begin
  for t:= 0 to FPayloadStreams.count-1 do begin
    if FPayloadStreams[t] <> nil then begin
      vart := vat.FindVarWithHighestPhysicalForPayload(t);

      if vart <> nil then begin
        fp := vart.GetFP(t);
        if fp <> nil then begin
          expectedsize := fp.PhysicalAddr + GetBigBlockSizeInBytes(vart.FileCount, false) + Sizeof(TVirtualDiskBigBlockHEader);
          str := FPayloadStreams[t];
          if str <> nil then begin
            if str.Size > expectedsize then
              str.Size := expectedSize//truncate the size of the stream, if allowed
            else begin

              if ((self.vat.PayloadConfig.filelist[fp.FileID].size_limit - str.Size) < 10000000)
              or (GetTimeSInce(str.last_collapse_time) > 60000)
              or (GetFreeSpaceOnPath(self.vat.PayloadConfig.filelist[fp.FileID].name) < self.vat.PayloadConfig.filelist[fp.FileID].size_limit ) then begin
                str.last_collapse_time := Getticker;
                str.collapsable := true;
              end;
            end;
          end;

        end;
      end else begin
        str := FPayloadStreams[t];
        if str <> nil then
          str.size := 0;
      end;
    end;
  end;
end;

procedure TVirtualDisk_Advanced.SortPayloadPriorities;
var
  t,u,f,idx: fi;
begin
  setlength(FprioritySort, FPayloadStreams.Count);
  idx := 0;
  for u := 0 to 32767 do begin
    for f:= 0 to high(vat.PayloadConfig.filelist) do begin
      if vat.PayloadConfig.filelist[f].priority = u then begin
        if FPayloadStreams[f] <> nil then begin
          FPrioritySort[idx].str := FPayloadStreams[f];
          FPrioritySort[idx].index := f;
          inc(idx);
        end;
      end;
    end;
  end;

  setlength(FPrioritySort, idx);


end;

procedure TVirtualDisk_Advanced.StartBringOnline;
begin
  lock;
  try
    StopBringOnline;



    cmd_BringOnline := Tcmd_VirtualDiskBringOnline.create;
    cmd_BringOnline.vd := self;
    cmd_BringOnline.start;
  finally
    Unlock;
  end;
end;

procedure TVirtualDisk_Advanced.StopBringOnline;
begin
  lock;
  try
    if assigned(cmd_BringOnline) then begin
      if not cmd_BringOnline.IsComplete then
        cmd_BringOnline.Cancel;
    end;
  finally
    unlock;
  end;

  if assigned(cmd_BringOnline) then begin
    cmd_BringOnline.WaitFor;
    lock;
    try
      cmd_BringOnline.WaitFor;
      cmd_BringOnline.free;
      cmd_BringOnline := nil;
    finally
      unlock;
    end;
  end;


end;

function TVirtualDisk_Advanced.DebugVatStructure: string;
begin
  result := vat.DebugVatSTructure;
  GEtVerboseDebug('e:\shit.txt');
end;

procedure TVirtualDisk_Advanced.DecommissionPayload(sFile: string);
var
  t: ni;
  s: string;
  str: TPayloadStream;
begin
  Lock;
  try
    for t := low(vat.payloadconfig.filelist) to high(vat.PayloadConfig.filelist) do begin
      s := vat.PayloadConfig.filelist[t].nameasstring;
      str := FPayloadStreams[t];
      if filenamecompare(s, sfile) then begin
        if (str<>nil) and (not (self.FPayloadStreams[t].Size <= SizeOf(TVirtualDiskPayloadFileHeader))) then begin
          raise EUserError.create('not empty, wait for empty and try again.');
        end else begin
          lastpayloadchange := getticker;
          vat.PayloadConfig.filelist[t].nameAsString := '';
          SaveVatAndConfig;
          FVatStream.FinalizeBuffers;
                                                 end;
      end;
    end;
    LoadPayloadStreams;
    DeleteFile(s);

  finally
    Unlock;
  end;

end;

destructor TVirtualDisk_Advanced.Destroy;
var
  rc: TRaidCalculator;
begin

//  Debug.Log('Stream is Destroyed.  Size is '+inttostr(FStream.size));

{$IFDEF BUFFER_BLOCKS}
  syncbuffer(-1, true);
{$ENDIF}



  while FRaids.Count > 0 do begin
    rc := FRaids[0] as TRaidCalculator;
    SyncAwayBuffer(0);
    rc := FRaids[0] as TRaidCalculator;
    FRaids.delete(0);

    rc.free;
    rc := nil;
  end;

  SaveVat(true);
  DestroyPayloadStreams;



  FPayLoadStreams.Free;
  FPayloadStreams := nil;

  vat.SetCloseMarker;
  SaveVat(false);
  if bRepaired then begin
    if fileexists(getfilename+'.lck') then
      deletefile(getfilename+'.lck');
  end;
  FVatStream.Free;
  FVatStream := nil;


  FRaids.free;
  FRaids := nil;
  RepairLog.free;
  RepairLog := nil;



  inherited;
end;

procedure TVirtualDisk_Advanced.DestroyPayloadStreams;
var
  ps: TPayloadStream;
begin
  StopBringOnline;
  while FPayloadStreams.count > 0 do begin
    ps := FPayloadStreams[FPayloadStreams.count-1];
    if ps<>nil then
      ps.free;

    ps := nil;
    FPayloadStreams.delete(FPayloadStreams.count-1);
  end;
end;

function TVirtualDisk_Advanced.DrainRepairLog: string;
begin
  Lock;
  try
    result := RepairLog.text;
    Repairlog.Clear;
  finally
    Unlock;
  end;
end;

procedure TVirtualDisk_Advanced.EvalRunHot(thr: TExternalEventThread);
begin
  thr.runhot := (reads_at_last_scrub = reads) and (writes_at_last_scrub = writes) and Operational;
end;

{$IFDEF ALLOW_RAID}
procedure TVirtualDisk_Advanced.FetchAndMergeRaid(r: TRaidCalculator);
var
  rCopied: TRaidCalculator;
  iFirstdirty, iFirstClean: ni;
begin
  rCopied := TRaidCalculator.create;
  try
    //save the current raid calc
    rCopied.CopyFrom(r);

    //fetch from disk (overwrites current)
    FetchRAid(r);

    iFirstDirty := 0;
    iFirstClean := 0;
    while iFirstClean < RAID_STRIPE_SIZE_IN_BLOCKS do begin
      iFirstDirty := rCopied.FirstDirty(iFirstClean);
      iFirstClean := rCopied.FirstClean(iFirstDirty);
      if iFirstDIRty < RAID_STRIPE_SIZE_IN_BLOCKS then begin
        movemem32(r.byteptr(iFirstDirty shl BLOCKSHIFT),rCopied.byteptr(iFirstDirty shl BLOCKSHIFT),  (iFirstClean-iFirstDirty) shl BLOCKSHIFT);
      end;
    end;
    r._dirty := rCopied._dirty;
    r.AnyDirty := rCopied.AnyDirty;

  finally
    rCopied.free;
    rCopied := nil;
    //FRaids.CheckForDeadObjects;
  end;


end;

function TVirtualDisk_Advanced.GetBusiestPhysical(const vart: PVirtualAddressRecord): ni;
var
  t: nativeint;
  ps: TPayloadStream;
  max, mine: nativefloat;
  fid: ni;
begin
  max := -1;
  result := 0;
  for t:= 0 to vart.FileCount-1 do begin
    fid := vart.FPs[t].fileid;
    if fid < 0 then begin
      result := fid;
      break;
    end;
    ps := FPayloadStreams[fid];
    if ps = nil then begin
      result := t;
      break;
    end else begin
      mine := (ps.estimated_queue_size+1)*ps.stats.GetAverage;
      if mine > max then begin
        result := t;
        max := mine;
      end;
    end;
  end;

end;

function TVirtualDisk_Advanced.GetLeastBusyPhysical(): ni;
var
  t: nativeint;
  ps: TPayloadStream;
  max, mine: ni;
  fid: ni;
begin
  max := 9999999999;
  result := 0;
  for t:= 0 to vat.FileCount-1 do begin
    fid := t;
    if fid < 0 then begin
      result := fid;
      break;
    end;
    ps := FPayloadStreams[fid];
    if ps = nil then begin
      result := t;
      break;
    end else begin
      mine := ps.estimated_queue_size;
      if ps.estimated_queue_size < max then begin
        result := t;
        max := mine;
      end;
    end;
  end;

end;



function TVirtualDisk_Advanced.GEtCachedSTripes: ni;
begin
  result := FRaids.Count;

end;

procedure TVirtualDisk_Advanced.FetchRaid(r: TRaidCalculator);
var
  t: ni;
  ps: TPayloadStream;
  fp: PFilePhysical;
  bo,so : int64;
  bs: int64;
  byte_off: int64;
  a: array[0..31] of TQueueItem;
  localvat: PVirtualAddressRecord;
  driveToSkip: ni;
  tidx: ni;
  bFlushimmediate: boolean;
  tmStart, tmEnd: ticker;
begin
  bFlushImmediate := false;
  tidx := r.Startingblock div _BIG_BLOCK_SIZE_IN_BLOCKS;
  BringBigBlockOnline(tidx);
  localvat := @vat.table[tidx];


  if localvat.HasUndefinedLocations then begin
    if not bInReconstitution then
      ForceReconstitution(localvat);
  end;

  r.PayloadSizeInBytes := RAID_STRIPE_SIZE_IN_BYTES;
//  if buffer.startingBlock < FCurrentVat.StartingBlock then
//    raise ECritical.create('something amiss');

  if localvat.FileCount=0 then begin
    NewPhysicalBlockAddress(BigBlockAlign(r.StartingBlock shl BLOCKSHIFT));
    //since localvat is a pointer into the VAT, its contents will be initialized by call to
    //NewphysicalBlockAddress
  end;






//  if buffer.startingBlock < FCurrentVat.StartingBlock then
//    raise ECritical.create('something amiss');


//  fillmem(@a[0], sizeof(a),0);
  driveToSkip := -1;
  if allowdriveskipping then begin
    if localvat.FileCount > 1 then
      drivetoSkip := GetBusiestPhysical(localvat)
  end;


  for t := 0 to localvat.FileCount-1 do begin
    ps := nil;

    if t = drivetoskip then continue;

    //get the file-physical record
    fp := @localvat.FPs[t];

    //determine the block-size for the RAID pieces
    bs := r.GetPieceSizePAdded(localvat.FileCount);
    a[t] := nil;

    //get the payload stream
    if fp.fileid > -1 then begin
      ps := FPayloadStreams[fp.FileID];


      if ps <> nil then begin

        //determine the block offset in the big-block
        bo := r.StartingBlock-localvat.StartingBlock;//block offset

        //determine STRIPE offset (in number of stripes)
        so := bo div RAID_STRIPE_SIZE_IN_BLOCKS;


        //determine the byte offset in the big-block
        byte_off := bs*so;
        if (byte_off < 0) then
          raise ECritical.create('checkit: byte_off < 0.  Bad.');

    //    ps.LockSize;
        try
    {$IFDEF USESEEKLOCK}      ps.SeekLock;{$ENDIF}
          try
            //seek to [start of the physical location] + [byte offset post raid]
            ps.Seek(fp.PhysicalAddr+byte_off, soBeginning);
            //write piece of data to physical location
            if not ps.IsAfterEOF(ps.Position) then begin
{$IFDEF DETAILED_DEBUGGING}
              if EnableOptionalDebugging then
                OptDebug('buffer.startingblock='+inttostr(r.StartingBlock)+' will be fetched to raid['+inttostr(fp.FileID)+'] from physical=0x'+inttohex(ps.Position,0))
              else
              if r.startingblock = getraidbase(380249) then begin
                Debug.log('buffer.startingblock='+inttostr(r.StartingBlock)+' will be fetched to raid['+inttostr(fp.FileID)+'] from physical=0x'+inttohex(ps.Position,0))
              end;


{$ENDIF}
{$IFDEF ASYNC_READS}
                a[t] := ps.BeginAdaptiveRead(@r.pieces[t], bs);
                a[t].tmUserMarker := GetHighResTicker;
{$ELSE}
                Stream_GuaranteeRead(ps, @r.pieces[t], bs);
{$ENDIF}

{$IFDEF DETAILED_DEBUGGING}
            if EnableOptionalDebugging then
            OptDebug('^read = '+memorydebugstring(@r.pieces[t], 128))
            else
            if r.startingblock = getraidbase(380249) then begin
            Debug.Log('^read = '+memorydebugstring(@r.pieces[t], 128));
            end;


{$ENDIF}

            end
            else begin
              fillmem(@r.pieces[t], bs, 0);
              a[t] := nil;
            end;
          finally
    {$IFDEF USESEEKLOCK}        ps.SeekUnlock;{$ENDIF}
          end;
        finally
    //      ps.UnlockSize;
        end;
      end;
    end;

    if ps = nil then begin
      fillmem(@r.pieces[t], bs, 0);
    end;

  end;

{$IFDEF ASYNC_READS}
  for t := 0 to localvat.FileCount-1 do begin
    if t = drivetoskip then continue;

    fp := @localvat.FPs[t];
    if fp.fileid <0 then
      continue;

    ps := FPayloadStreams[fp.FileID];


    if assigned(a[t]) then begin
      a[t].WAitFor;
      tmEnd := GetHighResTicker;
      if assigned(ps) then
        ps.stats.AddStat(tmEnd-a[t].tmUSerMarker);
      a[t].Free;
      a[t] := nil;
    end;
  end;
{$ENDIF}

  //calculate result
  r.Drives := localvat.FileCount;
  r.invalid_determined := false;
  r.Skipped_Read := drivetoskip;
  r.DetermineInvalidPiece(driveToSkip);

  bFlushImmediate := r.invalid_count > BoolToint(AllowDriveSkipping);

  //BAD PIECE FOUND{BPF}
  if r.invalid_count > 1 then begin
{BPF}
    if driveToSkip >= 0 then begin
{BPF} //determine the block-size for the RAID pieces
{BPF} bs := r.GetPieceSizePAdded(localvat.FileCount);
{BPF} //determine the block offset in the big-block
{BPF} bo := r.StartingBlock-localvat.StartingBlock;//block offset
{BPF} //determine STRIPE offset (in number of stripes)
{BPF} so := bo div RAID_STRIPE_SIZE_IN_BLOCKS;
{BPF} //determine the byte offset in the big-block
{BPF} byte_off := bs*so;
{BPF} fp := @localvat.FPs[DriveToSkip];
{BPF} ps := FPayloadStreams[fp.fileid];
{BPF} if ps = nil then begin
{BPF}   FopStatus := 'Broken raid';
{BPF}   Foperational := false;
{BPF}   exit;
{BPF} end;
{BPF}
{BPF} ps.Seek(fp.PhysicalAddr+byte_off, soBeginning);

{$IFDEF ASYNC_READS}
{BPF} tmStart := GetHighResTicker;
{BPF} a[DriveToSkip] := ps.BeginAdaptiveRead(@r.pieces[DriveToSkip], bs);
{BPF} a[DriveToSkip].WAitFor;
{BPF} a[DriveToSkip].Free;
{BPF} a[DriveToSkip] := nil;
{BPF} tmEnd := GetHighResTicker;
{BPF} ps.stats.AddStat(tmEnd-tmStart);

{$ELSE}

{BPF} tmStart := GetHighResTicker;
{BPF} Stream_GuaranteeRead(ps, @r.pieces[DriveToSkip], bs);
{BPF} tmEnd := GetHighResTicker;
{BPF} ps.stats.AddStat(tmEnd-tmStart);

{$ENDIF}
    end else begin
      FopStatus := 'Broken raid';
//      Foperational := false;
    end;
    r.invalid_determined := false;
  end;

  r.PiecesToSingle;
  r.readfromdisk := true;
  if bFlushimmediate then
    FlushRaid(r);

end;
{$ENDIF}

{$IFDEF ALLOW_RAID}
function TVirtualDisk_Advanced.FindHighestOverVar(
  fileid: ni): PVirtualAddressRecord;
var
  t: ni;
  pvar: PVirtualAddressREcord;
  pf: PFilePhysical;
  result_pf: PFilePhysical;
begin
  result_pf := nil;
  result := nil;
  for t:= low(vat.table) to high(vat.table) do begin
    pvar := @vat.table[t];
    pf := pvar.GetFP(fileid);
    if pf <> nil then begin
      if (result = nil) or ((result_pf<>nil) and (pf.physicalAddr > result_pf.physicalAddr)) then begin
        if (result = nil) or (result_pf=nil) then begin
          result := pvar;
          result_pf := pf;
        end else begin
          if (pf.PhysicalAddr >= vat.PayloadConfig.filelist[result_pf.FileID].size_limit) or (vat.PayloadConfig.filelist[result_pf.FileID].priority < 0) then begin
          result := pvar;
          result_pf := pf;
          end;
        end;
      end;

    end;
  end;


end;
function TVirtualDisk_Advanced.FindHighestOverVar_AfterCollapse(
  fileid: ni): PVirtualAddressRecord;
var
  t: fi;
begin
  Collapse(fileid);
  result := FindHighestOverVar(fileid);




end;

{$ENDIF}

{$IFDEF ALLOW_RAID}
procedure TVirtualDisk_Advanced.FlushBuffers(iOlderThan: ticker; bHalf: boolean = false; bOnlyIfALLDirty: boolean= false);
var
  c: TRaidCalculator;
  t: ni;
  tmNow: ticker;
  down2: ni;
begin
  Lock;
  try
    tmNow := GETticker;
    if bHalf then
      down2 := FRaids.count shr 1
    else
      down2 := 0;

    for t := FRaids.count-1 downto down2 do begin
      c := FRaids[t] as TRaidCalculator;
      if (tmNow - c.dirtytime) > iOlderThan then begin
        SyncAwayBuffer(t, bHalf{only if all dirty});
      end;
    end;
  finally
    Unlock;
  end;


  //FlushFileCache;

end;

procedure TVirtualDisk_Advanced.FlushRaid(r: TRaidCalculator);
var
  t: ni;
  ps: TPayloadStream;
  fp: PFilePhysical;
  bo,so : int64;
  bs: int64;
  byte_off: int64;
  localvat: PVirtualAddressRecord;
  newfunct_id: ni;
  newfunct_stream: TPayloadStream;
  tidx,tt: ni;
  tmStart, tmEnd: ticker;
begin
  if r.STartingBlock = 2949124 then
    debug.log('trap raid');

  //calculate the RAID
  if r.StartingBlock < 0 then begin
    r.init;
    exit;
  end;

  if r.startingblock = 429 then
    enableoptionaldebugging := true;


  tidx := r.startingblock div _BIG_BLOCK_SIZE_IN_BLOCKS;
  BringBigBlockOnline(tidx);

  if tidx >= MAX_BLOCKS_IN_VAT then
    raise Ecritical.create('wtf');
  localvat := @vat.table[tidx];
  if localvat.startingblock < 0 then
    localvat.startingblock := tidx * _BIG_BLOCK_SIZE_IN_BLOCKS;

  r.PayloadSizeInBytes := RAID_STRIPE_SIZE_IN_BYTES;
  if localvat.FileCount=0 then begin
    NewPhysicalBlockAddress(BigBlockAlign(r.StartingBlock shl BLOCKSHIFT));
        Debug.ConsoleLog('New Physical Block:'+localvat.DebugString);
  end;



  r.Drives := localvat.FileCount;
  r.SingleToPieces;

  for t := 0 to localvat.FileCount-1 do begin
    //get the file-physical record
    fp := @localvat.FPs[t];

    //get the payload stream
    if fp.FileID < 0 then begin
      ps := nil;
    end else begin
      ps := FPayloadStreams[fp.FileID];
    end;

    if ps = nil then begin
      //raise ECritical.create('wtf');
      //this piece cannot be flushed
      //find new functional piece
      ps := GEtUnderQuotaStream(@localvat, newfunct_id, false);
      if ps = nil then begin
        Foperational := false;
        fOPStatus := 'need new payloads for reconsitution of bad blocks';
        continue;//our data is corrupt
      end;
    end;


    //determine the block offset in the big-block
    bo := r.StartingBlock-localvat.StartingBlock;//block offset
    if (bo < 0) or (bo >= _BIG_BLOCK_SIZE_IN_BLOCKS) then begin
      FOpStatus := 'block offset invalid '+inttostr(bo);
      FOperational := false;
      ForceReconstitution(localvat);

    end;


    //determine the block-size for the RAID pieeces
    bs := r.GetPieceSizePadded(localvat.FileCount);

    //determine STRIPE offset (in number of stripes)
    so := bo div RAID_STRIPE_SIZE_IN_BLOCKS;

    //determine the byte offset in the big-block
    byte_off := bs*so;

{$IFDEF USESEEKLOCK}    ps.SeekLock;{$ENDIF}
    try
      //seek to [start of the physical location] + [byte offset post raid]

      ps.Seek(fp.PhysicalAddr+byte_off, soBeginning);

{      if ps.Position = $7603f9  then begin
        Debug.Log('Trap');
        for tt := 0 to r.drives-1 do begin
          Debug.Log('['+inttostr(tt)+']=0x'+inttohex(r.pieces[t]._piece_checksum,0)+', '+inttohex(r.pieces[t]._stripe_checksum,0));
        end;
        r.SingleToPieces;
        for tt := 0 to r.drives-1 do begin
          Debug.Log('['+inttostr(tt)+']=0x'+inttohex(r.pieces[t]._piece_checksum,0)+', '+inttohex(r.pieces[t]._stripe_checksum,0));
        end;
      end;}

      //write piece of data to physical location
{$IFDEF DETAILED_DEBUGGING}
      if EnableOptionalDebugging then begin
        OptDebug('buffer.startingblock='+inttostr(r.StartingBlock)+' is flushed to ['+inttostr(fp.FileID)+'].physical=0x'+inttohex(ps.Position,0));
        OptDebug('^write = '+memorydebugstring(@r.pieces[t], 128));
      end else
      if r.startingblock = getraidbase(380249) then begin
        Debug.Log('buffer.startingblock='+inttostr(r.StartingBlock)+' is flushed to ['+inttostr(fp.FileID)+'].physical=0x'+inttohex(ps.Position,0));
        Debug.Log('^write = '+memorydebugstring(@r.pieces[t], 128));
      end;


{$ENDIF}
      tmStart := GetHighResTicker;
      Stream_GuaranteeWrite(ps, @r.pieces[t], bs);
      tmEnd := GetHighResTicker;
      ps.stats.AddStat(tmEnd-tmStart);
    finally
{$IFDEF USESEEKLOCK}      ps.SeekUnlock;{$ENDIF}
    end;
  end;


  r.ClearDirty;
end;
function TVirtualDisk_Advanced.ForceReconstitution(
  pvar: PVirtualAddressRecord): boolean;
begin
  result := Reconstitute(pvar);
end;

{$ENDIF}

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

function TVirtualDisk_Advanced.GetRepairLog: string;
begin
  Lock;
  try
    result := Repairlog.text;
  finally
    Unlock;
  end;
end;

function TVirtualDisk_Advanced.GEtUnderQuotaStream(notof: PVirtualAddressRecord; out id: nativeint;
  bOnlyReadyStreams: boolean;
  iAllowPhysical: ni //special allowance for when we're looking to move a payload from 1 physical location.  We will allow it to move to another payload on the same location
  ): TPayloadStream;
var
  t,f: ni;
  pfc: TPayloadFileInformation;
  str: TPayloadStream;
  i1:int64;
  i2: int64;
  sz_min, sz: int64;
  prior: ni;
begin
  id := -1;
  result := nil;
  sz_min := -1;

{$IFNDEF USE_PRIORITY_SORT}
  prior := 0;
  while prior < 32767 do begin
    for t:= 0 to FPayloadStreams.Count-1 do begin
      str := FPayloadStreams[t];
{$ELSE}
//    prior := vat.PayloadConfig.filelist[FPrioritySort[0].index].priority;
    prior :=  -1;//Fpayloadstreams[FPayloadStreams[FPrioritySort[0].index].priority;
    for f:= 0 to high(FPrioritySort) do begin
      str := FPrioritySort[f].str;
      t := FPrioritySort[f].index;
{$ENDIF}
      if str = nil then continue;


{$IFNDEF USE_PRIORITY_SORT}
      if (prior >= 0) and (vat.PayloadConfig.filelist[t].priority <> prior) then
        continue;
{$ENDIF}


      i1 := str.Size;
      i1 := i1 + GetBigBlockSizeInBytes(1,true);

      i2 := vat.PayloadConfig.filelist[t].size_limit;
      if i2 = 0 then
        continue;

      if GetFreeSpaceOnPath(extractfilepath(str.FileNAme)) < (BIG_BLOCK_SIZE_IN_BYTES*32) then
        continue;


      if (i1 < i2) or (i2 = -1) then begin
        if (str.Size < sz_min) or (sz_min = -1) then begin
          if ((not notof.HasFile(t)) and (not  PhysicalIsInVar(notof, vat.PayloadConfig.filelist[t].physical))) or (vat.PayloadConfig.filelist[t].physical = iAllowPhysical) then begin
            result := str;
            id := t;
            {$IFDEF USE_PRIORITY_SORT}
              prior := vat.PayloadConfig.filelist[t].priority;
            {$ENDIF}
            sz_min := str.size;
          end;
        end;

      end;
{$IFNDEF USE_PRIORITY_SORT}
    end;
{$ENDIF}

    if result <> nil then             break;

    inc(prior);
  end;

end;

function TVirtualDisk_Advanced.IndexOfRaid(startingBlock: int64): ni;
var
  rc: TRaidCalculator;
  t: ni;
begin
  raise ECritical.create('not implemented... code is wrong.');
  result := t;
  for t:= 0 to FRaids.count-1 do begin
    rc := FRaids[t] as TRaidCalculator;
    if rc.startingblock = startingblock then begin
      result := t;
      break;
    end;
  end;
end;

function TVirtualDisk_Advanced.IsStreamUnderQuota(t: nativeint): boolean;
var
  pfc: TPayloadFileInformation;
  str: TPayloadStream;
  i1:int64;
  i2: int64;
begin
  result := false;



  str := FPayloadStreams[t];
  if str = nil then exit;

  result := GetFreeSpaceOnPath(extractfilepath(str.FileNAme)) >= (BIG_BLOCK_SIZE_IN_BYTES*32);


  i1 := str.Size;
  i1 := i1 + GetBigBlockSizeInBytes(1,true);

  i2 := vat.PayloadConfig.filelist[t].size_limit;

  result := ((i1 < i2) or (i2 = -1)) and (vat.PayloadConfig.filelist[t].priority >= 0);

end;


procedure TVirtualDisk_Advanced.GrowIfNeededBlock(lba: int64);
var
  m: Pbyte;
  t: int64;
begin
  exit;
(*  if (lba+1) shl BLOCKSHIFT < FStream.Size then
    exit;


  t := FStream.Size shr BLOCKSHIFT;//start at last block

  m := GetMemory(BlockSize);
  try
    Debug.Log('Growing stream to:'+inttostr((t+1) shl BLOCKSHIFT));
    Debug.Log('Position is now '+inttostr(FStream.position));
    while t <=lba do begin


      FillMem(m, BlockSize, 0);
      FStream.Seek(t shl BLOCKSHIFT, soBeginning);

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
    iJustRead := ReadBlocks_Direct(lba, cnt, @p[iRead shl BLOCKSHIFT]);
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
    iJustWrote := WriteBlocks_Direct(lba, cnt, @p[iWrote shl BLOCKSHIFT]);
    inc(lba, iJustWrote);
    dec(cnt, iJustWrote);
    inc(iWrote, iJustWrote);
  end;
end;


function TVirtualDisk_Advanced.HasRaid(startingblock: int64): boolean;
begin
  result := IndexOfRaid(startingblock) >= 0;

end;

{$ifdef use_vat}
procedure TVirtualDisk_Advanced.LoadPayloadStreams;
var
  t: integer;
  s: string;
  ps: TpayloadStream;
  pfh: TVirtualDiskPayloadFileHeader;
  bAllGood: boolean;
begin
  DestroyPayloadStreams;//incase some were already defined

  bAllGood := true;
  try
    if self.vat.FileCount = 0 then begin
      s := changefileext(self.FileName,'.vdpayload');
      movemem32(@self.vat.PayloadConfig.filelist[0].name[0], @s[STRZ], sizeof(char)*length(s));
      self.vat.PayloadConfig.filelist[0].size_limit := -1;
    end;

    for t:= low(self.vat.payloadConfig.filelist) to high(self.vat.payloadconfig.filelist) do begin
      ps := nil;
      s := self.vat.payloadconfig.filelist[t].name;
      if s <> '' then begin
        if fileexists(s) then begin
          try
            ps := TPayloadStream.create(s, fmOpenReadWrite+fmShareDenyNone, 0, PAYLOAD_FLAGS);


            self.vat.payloadconfig.filelist[t].Flag_Missing := false;
          except
            self.vat.payloadconfig.filelist[t].nameAsString := '';
            self.vat.payloadconfig.filelist[t].size_limit := 0;
            self.vat.payloadconfig.filelist[t].Flag_Missing := true;
            savevatandconfig;
            ps := nil;
          end;
        end else begin
          if not vat.HasFile(t) then
          try
            ps := nil;
            try
              try
                ps := TPayloadStream.create(s, fmCreate, 0, PAYLOAD_FLAGS);
              except
                ps := nil;
              end;
              if ps <> nil then begin
                pfh.Init;
                ps.seek(0,soBeginning);
                stream_guaranteewrite(ps, @pfh, sizeof(pfh));
              end;
            finally
              ps.free;
            end;

            ps := TPayloadStream.create(s, fmOpenReadWrite+fmShareDenyNone, 0, PAYLOAD_FLAGS);
          except
            self.vat.payloadconfig.filelist[t].nameAsString := '';
          end else begin

            OperationalStatus := 'Missing '+s;
            self.vat.payloadconfig.filelist[t].Flag_Missing := true;
            FOperational := false;
            FPayloadsOk := false;
            bAllGood := false;
          end;
        end;

        if assigned(ps) then begin
          //ps.AllowReadPastEOF := true;
          ps.BufferSize := 32*65536;
          ps.BufferSEgments := 32;
          ps.DisableLookAhead := false;
  //      ps.BufferSize := FCacheSize;
        end;

        FPayloadStreams.add(ps);
      end else begin
        FPayloadStreams.add(nil);
      end;
    end;
    SortPayloadPriorities;
  finally

    FPayloadsOK := bAllGood;
    if FPayloadsOK then
      StartBringOnline;
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

  QuickCheckVat;



end;
procedure TVirtualDisk_Advanced.LogRepair(s: string);
begin
  RepairLog.Add(s);
  Debug.ConsoleLog(s);
  Debug.Log(s);
end;

{$endif}

{$ifdef use_vat}
function TVirtualDisk_Advanced.NeedRaid(startingblock: int64): TRaidCalculator;
var
  rc: TRaidCalculator;
  t: ni;
  x: ni;
  tableidx: ni;
begin
  result := nil;
  x := 0;
  tableidx := startingblock div RAID_STRIPE_SIZE_IN_BLOCKS;
{$IFDEF USE_VAT_HINTS}
  if vathints[tableidx] then
{$ENDIF}
  for t:= 0 to FRaids.count-1 do begin
    rc := FRaids[t] as TRaidCalculator;
    x := t;
    if rc.Detached then
      raise ECritical.create('wtf');
    if rc = nil then
      raise ECritical.create('wtf');
    if rc.startingblock = startingblock then begin
      result := rc;
      break;
    end;
  end;

  if result = nil then begin

    result := FRaids[x] as TRaidCalculator;


    if SyncAwayBuffer then
      //sync up to half the buffers if the syncaway buffer was dirty
      //(this means we're in heavy write mode) and flushing the buffers
      //together means that we'll have a better chance to perform write combines.
      FlushBuffers(12,true, true);


    x := FRaids.count-1;
    //result := FRaids[x] as TRaidCalculator;
    result := FRaids[x] as TRaidCalculator;

{$IFDEF USE_VAT_HINTS}
    if result <> nil then begin
      if result.startingblock >=0 then
        vathints[result.startingblock div RAID_STRIPE_SIZE_IN_BLOCKS] := false;
    end;
{$ENDIF}


    result.Init;
    result.startingblock := startingblock;

  end;

  if (x <> 0) and (result <> nil) then begin
    Fraids.Remove(result);
{$IFDEF USE_LINKED_RAID}
    FRaids.AddFirst(result);
{$ELSE}
    //FRaids.AddFirst(result);
    FRaids.Insert(0, result);
{$ENDIF}
    FRaids.CheckForDeadObjects;
  end;

{$IFDEF USE_VAT_HINTS}
  vathints[tableidx] := true;
{$ENDIF}

  result.lastused := GetTicker;

end;

function TVirtualDisk_Advanced.NewPhysicalBlockAddress(virtual_addr: int64): TVirtualAddressRecord;
{$IFNDEF ALLOW_RAID}
var
  fid: ni;
  fs: TPayloadStream;
  tindex: ni;
begin
  virtual_addr := (virtual_addr div (BLOCKSIZE * _BIG_BLOCK_SIZE_In_BLOCKS)) * (BLOCKSIZE * _BIG_BLOCK_SIZE_In_BLOCKS);

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
  result.StartingBlock := virtual_addr shr BLOCKSHIFT;
  tindex := virtual_addr div (BlockSize* _BIG_BLOCK_SIZE_IN_BLOCKS);
  vat.table[tindex] := result;
  vat.MarkTableEntryDirty(tindex);


  debug.log('New Physical:'+result.DebugString);
  debug.log('New Vat:'+self.vat.DebugVatSTructure);
end;
{$ELSE}
var
  fs: TPayloadStream;
  tindex: ni;
  t: ni;
  iPriority: ni;
  blockstart: int64;
  a: array[0..31] of ni;
  aidx: ni;
  localvat: PVirtualAddressRecord;
    function Has(ai: ni): boolean;
    var
      tt: ni;
    begin
      result := false;
      for tt := 0 to aidx-1 do begin
        if a[tt] = ai then begin
          result := true;
          exit;
        end;
      end;

    end;
begin

  //init result
  result.InitVirgin(((virtual_addr shr BLOCKSHIFT) div _BIG_BLOCK_SIZE_IN_BLOCKS)*_BIG_BLOCK_SIZE_IN_BLOCKS);


  //put all under-quota streams in the list
  aidx := 0;
  iPriority := 0;
  while result.filecount = 0 do begin
    for t:= 0 to FPayloadStreams.Count-1 do begin
      fs := FPayloadStreams[t];
      if fs = nil then continue;

      if vat.payloadconfig.filelist[t].priority =iPriority then
      if IsStreamUnderQuota(t) then begin
        if not Has(vat.PayloadConfig.filelist[t].physical) then begin
          //add the stream to the list
          result.FPs[result.FileCount].FileID := t;
          inc(result.fileCount);
          a[aidx] := vat.PayloadConfig.filelist[t].physical;//result.fileids[result.FileCount].physical;
          inc(aidx);
        end;
      end;
    end;
    inc(iPriority);
    if iPriority = 32767 then begin
      iPriority := 0;
      self.FOperational := false;
      operationalStatus := 'Could not allocate new physical block.';
      break;
//      Unlock;
//      try
//        systemx.beep(1000,4000);
//        sleep(4000);
//      finally
//        Lock;
//      end;
    end;
  end;

  //now that we know how many there are... we need to expand them all
  for t:= 0 to result.filecount-1 do begin
    fs := FPayloadStreams[result.FPs[t].FileID];
    fs.CheckInit;
    //blockstart := fs.Size;

     result.FPs[t].PhysicalAddr := fs.Expand(GetBigBlockSizeInBytes(result.FileCount, false), virtual_addr);
  end;

  result.StartingBlock := virtual_addr shr BLOCKSHIFT;
  //determine table index
  tindex := virtual_addr div (BlockSize* _BIG_BLOCK_SIZE_IN_BLOCKS);
  //put in table
  localvat := @vat.table[tindex];
  localvat^ := result;

  //mark part of table dirty
  vat.MarkTableEntryDirty(tindex);
  //save changes to vat
  SaveVat(false);
{$IFDEF DETAILED_DEBUGGING}
  debug.log('New Physical Spans '+inttostr(result.filecount)+' payloads.');
  debug.log('New Physical:'+result.DebugString);
  debug.log('New Vat:'+self.vat.DebugVatSTructure);
{$ENDIF}
{$IFDEF RECONST_DEBUG}
  debug.log('New Physical Spans '+inttostr(result.filecount)+' payloads.');
  debug.log('New Physical:'+result.DebugString);
  debug.log('New Vat:'+self.vat.DebugVatSTructure);
{$ENDIF}


end;
{$ENDIF}

{$endif}

procedure TVirtualDisk_Advanced.GrowIfNeededAddr(size: int64);
begin
  GrowIfNeededBlock((size shr BLOCKSHIFT)+1);
end;


function TVirtualDisk_Advanced.PhysicalIsInVar(vart: PVirtualAddressRecord;
  iPhysical: ni): boolean;
var
  t: ni;
  id: ni;
begin
  result := false;
  for t:= 0 to vart.filecount-1 do begin
    id := vart.FPs[t].FileID;
    if id < 0 then
      continue;
    if vat.PayloadConfig.filelist[id].physical = iPhysical then begin
      result := true;
      break;
    end;
  end;
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

procedure TVirtualDisk_Advanced.QuickCheckVat;
var
  t: ni;
begin
  for t:= low(vat.table) to high(vat.table) do begin
    vat.table[t].startingblock := t * (_BIG_BLOCK_SIZE_IN_BLOCKS);
  end;

end;

procedure TVirtualDisk_Advanced.QuickOnline;
var
  t: ni;
begin

  for t:= 0 to MAX_BLOCKS_IN_VAT-1 do begin
    online_bigblocks[t] := true;
  end;
  reconstituting := false;
  bInReconstitution := false;
end;

function TVirtualDisk_Advanced.RaidUP: boolean;
var
  vart: PVirtualAddressRecord;
  toID: ni;
  toS: TPayloadStream;
  t: ni;
  a: array[0..BLOCKSIZE-1] of byte;
begin
  result := false;

  Lock;
  try
    vart := vat.FindVARToRaidUp;

    if vart <> nil then begin
      toS := GEtUnderQuotaStream(vart, toID, false);
      if toS <> nil then begin
        vat.PayloadConfig.status := 'RAID up';
        //allocate space in a new file
        vart.ChangeFileID(-1, toID, toS.Expand(GetBigBlockSizeInBytes(vart.filecount, false), vart.StartingBlock shl BLOCKSHIFT));

        //read and write-back all the RAID blocks in the big block
        for t:= 0 to _BIG_BLOCK_SIZE_IN_BLOCKS-1 do begin
          self.ReadBlock(vart.StartingBlock+t, @a[0]);
          self.WriteBlock(vart.StartingBlock+t, @a[0]);
        end;
        result := true;
      end;
    end;


  finally
    Unlock;
  end;

end;

procedure TVirtualDisk_Advanced.ReadBlock(const lba: int64; const p: pbyte);
begin
  ReadBlocks(lba, 1, p);
end;

function TVirtualDisk_Advanced.ReadBlocks(const lba: int64; const cnt: nativeint;
  p: pbyte): nativeint;
begin
  if not bInReconstitution then
    reads := (reads + 1) and $7FFFFFFFFFFFFFFF;

{$IFDEF BUFFER_BLOCKS}
  result := ReadBlocks_Buffered(lba, cnt, p);
{$ELSE}
  result := ReadBlocks_Direct(lba, cnt, p);
{$ENDIF}
end;

{$IFDEF BUFFER_BLOCKS}
function TVirtualDisk_Advanced.ReadBlocks_Buffered(const lba: int64; const cnt: nativeint;
  p: pbyte): nativeint;
var
  ioffset, iByteOffset: nativeint;
  iToRead: nativeint;
  cnt2: nativeint;
begin
  Lock;
  try
    //sync up the buffer to the desired position
    SyncBuffer(lba, false);

    //determine which slot in the buffer we're using
    iOffset := lba and (int64(MAX_BUFFERED_BLOCKS-1));

    //READ the data FROM the buffer
    iByteOffset := BLOCKSIZE * iOffset;

    cnt2 := cnt;
    cnt2 := lesserof(cnt2, MAX_BUFFERED_BLOCKS-iOffset);
    iToRead := lesserof(cnt2, MAX_BUFFERED_BLOCKS-iOffset);
    result := iToREad;
    iToRead := iToREad shl BLOCKSHIFT;

    if p <> nil then
      movemem32(p, TRaidCalculator(Fraids[0]).byteptr(iByteOffset), iToRead);

    //mark dirty
    //buffer.dirty[iOffset] := true;
  finally
    Unlock;
  end;


end;
{$ENDIF}

function TVirtualDisk_Advanced.ReadBlocks_Direct(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
begin
  result := ReadBlocks_Direct_Single(lba, cnt, p);
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
    actual_addr := VirtualToPhysicalAddr(lba shl BLOCKSHIFT);
    end_actual_addr := VirtualToPhysicalAddr((lba+lcnt) shl BLOCKSHIFT);
{$IFDEF ALLOW_RAID}
    bDifferentFiles := actual_addr.FPs[0].FileID <> end_actual_addr.FPs[0].FileID;
{$ELSE}
    bDifferentFiles := actual_addr.FileID <> end_actual_addr.FileID;
{$ENDIF}

    //if the blocks are not all consecutive, we'll need to reduce the number of blocks we can write
{$IFNDEF ALLOW_RAID}
    if bDifferentFiles or ((end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <> (lcnt shl BLOCKSHIFT)) then begin
{$ELSE}
    if bDifferentFiles or ((end_actual_addr.FPs[0].PhysicalAddr - actual_addr.FPs[0].physicalAddr) <> (lcnt shl BLOCKSHIFT)) then begin
{$ENDIF}
      //change the end actual address
      begin
        //find the beginning of the actual_addr bigblock
{$IFDEF ALLOW_RAID}
        end_actual_addr.FPs[0] := actual_addr.FPs[0];
{$ELSE}
        end_actual_addr.FileID := actual_addr.FileID;
{$ENDIF}
        end_actual_addr := VirtualToPhysicalAddr(((lba div _BIG_BLOCK_SIZE_IN_BLOCKS) * _BIG_BLOCK_SIZE_IN_BLOCKS) shl BLOCKSHIFT);
{$IFNDEF ALLOW_RAID}
        end_actual_addr.PhysicalAddress := end_actual_addr.PhysicalAddress + (_BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT);
{$ELSE}
        end_actual_addr.FPs[0].physicalAddr := end_actual_addr.FPs[0].physicalAddr + (_BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT);
{$ENDIF}
      end;

{$IFNDEF ALLOW_RAID}
      result := (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) shr BLOCKSHIFT;
{$ELSE}
      result := (end_actual_addr.FPs[0].physicalAddr - actual_addr.FPs[0].physicalAddr) shr BLOCKSHIFT;
{$ENDIF}
      lcnt := result;
{$IFNDEF ALLOW_RAID}
      if (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <> (lcnt shl BLOCKSHIFT) then begin
{$ELSE}
      if (end_actual_addr.FPs[0].physicalAddr - actual_addr.FPs[0].physicalAddr) <> (lcnt shl BLOCKSHIFT) then begin
{$ENDIF}
        raise ECritical.create('Block segment fault failure.');
      end;
    end;
//    Debug.Log(inttohex(lba shl BLOCKSHIFT, 2)+'<-actual_addr(rb)->'+inttohex(actual_addr, 2));


    if (lba shl BLOCKSHIFT) >= (Size+Blocksize) then
      raise ECritical.create('Address beyond the size of the disk! '+inttohex(lba shl BLOCKSHIFT,2));


{$IFDEF ALLOW_RAID}
    ps := FPayloadStreams[actual_addr.FPs[0].FileID];
{$IFDEF USESEEKLOCK}    ps.seeklock;{$ENDIF}
    try
    ps.Seek(actual_addr.FPs[0].PhysicalAddr,0);
    Stream_GuaranteeRead(ps, p, BLOCKSIZE*lcnt);
    finally
{$IFDEF USESEEKLOCK}    ps.seekunlock;{$ENDIF}
    end;
{$ELSE}
    ps := FPayloadStreams[actual_addr.FileID];
{$IFDEF USESEEKLOCK}    ps.seeklock;{$ENDIF}
    try
    FPayloadStreams[actual_addr.FileID].Seek(actual_addr.PhysicalAddress,0);
    Stream_GuaranteeRead(FPayloadStreams[actual_addr.FileID], p, BLOCKSIZE*lcnt);
    finally
{$IFDEF USESEEKLOCK}    ps.seekunlock;{$ENDIF}
    end;

{$ENDIF}

  finally
    Unlock;
  end;

end;

function TVirtualDisk_Advanced.ReadBlocks_RaidCalc(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
var
  lba_off: int64;
  iCanRead: int64;
begin
  //determine offset into RAID space
  lba_off := lba-TRaidCalculator(Fraids[0]).StartingBlock;
  if (lba_off < 0) or (lba_off >= RAID_STRIPE_SIZE_IN_BLOCKS) then begin
    raise ECritical.create('Something is wrong and lba is not within RAID stripe.');
  end;

  iCanRead := lesserof(RAID_STRIPE_SIZE_IN_BLOCKS - lba_off, cnt);

  movemem32(p, TRaidCalculator(Fraids[0]).byteptr((lba_off shl BLOCKSHIFT)), iCanRead shl BLOCKSHIFT);

  result := iCanRead;


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
      GuaranteeReadBlocks(addr shr BLOCKSHIFT, cnt shr BLOCKSHIFT, p)
    else
      GuaranteeReadBlocks(addr shr BLOCKSHIFT, cnt shr BLOCKSHIFT, p);
  finally
    Unlock;
  end;

end;

function TVirtualDisk_Advanced.ReadSingleByte(iAddr: int64): byte;
var
  block: array[0..BLOCKSIZE-1] of byte;
  iOFF: int64;
begin
  self.ReadBlock(iAddr shr BLOCKSHIFT, @block[0]);
  iOFF := iAddr mod BlockSize;

  result := block[iOFF];
//  self.WriteBlock(iAddr shr BLOCKSHIFT, @block[0]);

end;

function TVirtualDisk_Advanced.Reconstitute(pvar: PVirtualAddressRecord = nil): boolean;
var
  varsave: TVirtualAddressRecord;
  p: pbyte;
  t,fid: fi;
begin
  result := false;              //start with result false
  if bINReconstitution then     //if already reconstituting then exit (recursion danger?)
    exit;
  result := false;
  bInReconstitution := true;    //flag that we're reconstituting

  Lock;                         //lock the whole disk
  try

    if pvar = nil then
      pvar := vat.FindVARToReconstitute;  //look for something to reconstitute... (priority < 0)
    if pvar <> nil then begin
      //save the original record in case there's a problem
      Debug.ConsoleLog('Needs Reconstitution:'+ pvar.debugstring);
      varSave := pvar^;
      //flag all these files as potentially collapsable
//      for t:= 0 to varSave.FileCount-1 do begin
//        FPayloadStreams[varsave.FPs[t].FileID].Collapsable := true;
//      end;
      try


        p := GetMemory(_BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT);
        try
          //read the entire big block
          GuaranteeReadBlocks(pvar.StartingBlock, _BIG_BLOCK_SIZE_IN_BLOCKS, p);    //read the blocks from the stuff, as-if we're just reading the drive
          Debug.ConsoleLog('Reconst Read Complete');
          //----------------

          //wipe out the var
          pvar.InitVirgin(varsave.startingblock);                                   //completely wipte the vat table entry as virgin causing a complete reselection of locations for this data.
          pvar.StartingBlock := varSave.StartingBlock;
          //self.FRaids[0].Init;

          //rewrite the entire big block (should use only drives with positive priority)
          GuaranteeWriteBlocks(pvar.StartingBlock, _BIG_BLOCK_SIZE_IN_BLOCKS, p);
          Debug.ConsoleLog('Reconst Write Complete');
          Debug.ConsoleLog('Reconst Flush');
          self.FlushBuffers(0,false, false);
          Debug.ConsoleLog('Reconst Flush Complete');

          result := true;

        finally
          Freememory(p);
        end;

        vat.MarkTableEntryDirtyByPtr(pvar);

      except
        pvar^ := varSave;
      end;

    end;
    ShrinkPayloads;  //shrink the payloads
    //flag all these files as potentially collapsable
    for t:= 0 to varSave.FileCount-1 do begin
      Debug.ConsoleLog('Reconst Collapse '+inttostr(t));
      fid := varsave.FPs[t].FileID;

      if fid >=0 then
        Collapse(fid);
    end;



  finally
    Unlock;
    bInReconstitution := false;
  end;





  reconstituting := result;


end;

procedure TVirtualDisk_Advanced.ReFunctPayload(iPayloadID: ni;
  sNewSource: string);
var
  fs: TFileStream;
begin
  Lock;
  try
    SaveVatandConfig;
    vat.PayloadConfig.filelist[iPayloadID].NameAsString := sNewSource;
    try
      if not fileexists(sNewSource) then begin
        fs := nil;
        try
          fs := TFileStream.Create(sNewSource, fmCReate);
        finally
          fs.free;
        end;
        fs := nil;
      end;
    except
    end;
    LoadPayloadStreams;
    vat.ChangeAllFiles(iPAyloadID, -1);
    bRepaired := false;

    //raise ECritical.create('Refunct isn''t fully implemented, implement the refunct process.');
  finally
    unlock;
  end;
end;

procedure TVirtualDisk_Advanced.RepairbigBlock(varr: PVirtualAddressRecord; ps: TPayloadStream;
  pbbh: TVirtualDiskBigBlockHeader);
begin
  if pbbh.FooterStart+SizeOf(TVirtualDiskBigBlockHeader) >= ps.size then begin
    ps.GrowFile(pbbh.FooterStart+SizeOf(TVirtualDiskBigBlockHeader));
    LogRepair('BBH extends after EOF.  Extending big block, data loss might occur, but RAID5 will be responsible for repairs');
  end;

end;

procedure TVirtualDisk_Advanced.Repair_payload(ps: TPayloadStream; id: ni);
var
  pfh: TVirtualdiskpayloadfileheader;
  bbh,bbf: TVirtualDiskBigBlockHeader;
  vad: int64;
begin
  exit;
//  if (ps.Size-sizeof(TVirtualDiskPayloadFileHEader)) mod (2*sizeof(bbf)+GetBigBlockSizeInBytes(1)) <> 0 then begin
//    DEbug.Log(ps.filename+' is invalid size.... truncating.');
//    ps.Size := (ps.Size-sizeof(TVirtualDiskPayloadFileHEader)) div (2*sizeof(bbf)+GetBigBlockSizeInBytes(1) * (2*sizeof(bbf)+GetBigBlockSizeInBytes(1) ))+sizeof(TVirtualDiskPayloadFileHEader);
//    brepaired := false;
//  end;

  pfh := ps.GetPFH;
  //move to the first payload
  ps.Seek(sizeof(TVirtualdiskpayloadfileheader), soBeginning);

  while ps.Position < ps.Size do begin
    //get the header
    Stream_guaranteeRead(ps, @bbh, sizeof(bbh), true);
    ps.seek(ps.position+ bbh.PayloadSize, soBeginning);
    //get the footer
    Stream_guaranteeRead(ps, @bbf, sizeof(bbh), true);

    if bbh.VirtualAddress = bbf.VirtualAddress then begin
      vad := bbh.virtualaddress;
      if vad > (int64(MAX_BLOCKS_IN_VAT) * int64(_BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT)) then begin
        LogRepair('Problem '+inttostr(vad)+' is beyond the end of the VAT.');
        brepaired := false;
      end;

      if vad < 0 then begin
        LogRepair('Problem '+inttostr(vad)+' is < 0.');
        brepaired := false;
      end;

{
      if bbh.VirtualAddress = bbf.VirtualAddress then
      if self.vat.table[bbh.VirtualAddress div (BLOCKSIZE * _BIG_BLOCK_SIZE_IN_BLOCKS)].FPs[u].fileid = -1 then begin
        Debug.ConsoleLog('Found orphaned payload, assigning');
        self.vat.table[bbh.virtualaddress div (BLOCKSIZE * _BIG_BLOCK_SIZE_IN_BLOCKS)].FPs[u].fileid := id;
        brepaired := false;
      end;
}

    end;
  end;
{$IFDEF REDUNDANT_PHYSICAL}
  pfh._NextPhysicalAddress := ps.Size;
  ps.SetPFH(pfh);
{$ENDIF}


end;

procedure TVirtualDisk_Advanced.ReSourcePayload(iPayloadID: ni;
  sNewSource: string);
begin
  Lock;
  try
    vat.PayloadConfig.filelist[iPayloadID].NameAsString := sNewSource;
    SaveVatandConfig;
    LoadPayloadStreams;
  finally
    unlock;
  end;
end;

{$ifdef use_vat}
procedure TVirtualDisk_Advanced.SaveVat(bAll: boolean);
begin
  vat.FlushToStream(FVATStream, bAll);
//  SaveStringAsFile(FVatStream.FileName+'.debug.txt', vat.DebugVatSTructure);
end;

procedure TVirtualDisk_Advanced.SaveVatAndConfig(sTo: string = '');
var
  fs: TFileStream;
begin
  vat.FlushToStream(FVATStream, true);

  if sTo <> '' then begin
    try
      fs := nil;
      try
        fs := TFileStream.create(sTo, fmCreate);

        FVatStream.Seek(0,soBeginning);
        Stream_GuaranteeCopy(FVatStream, fs, FVatStream.size);

      finally
        fs.free;
      end;
    except
    end;
  end;

//  SaveStringAsFile(FVatStream.FileName+'.debug.txt', vat.DebugVatSTructure);
  //FVatStream.FInalizeBuffers;
end;

{$endif}


procedure TVirtualDisk_Advanced.SetCachedSTripes(const Value: ni);
var
  rc: TRaidCAlculator;
begin
  Lock;
  try
    while FRaids.Count < Value do
      FRaids.Add(TRaidCalculator.create);
    while FRaids.Count > value do begin
      SyncAwayBuffer(FRaids.count-1);
      rc := TRaidCAlculator(FRaids[FRaids.Count-1]);
      rc.free;
      FRaids.Delete(FRaids.count-1);
      rc := nil;
    end;
  finally
    Unlock;
  end;
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
  ps: TPayloadStream;
begin
  Lock;
  try
    vat.DefaultBufferSegmentSize := iSegmentSize;
    vat.DefaultBufferSegmentCount := iSegmentCount;
    vat.DefaultBufferReadAhead := bReadAhead;
    vat.MarkDirty(@vat.DefaultBufferSegmentSize, sizeof(vat.DefaultBufferSegmentSize));
    vat.MarkDirty(@vat.DefaultBufferSegmentCount, sizeof(vat.DefaultBufferSegmentCount));
    vat.MarkDirty(@vat.DefaultBufferReadAhead, sizeof(vat.DefaultBufferReadAhead));
    SaveVat(false);
    for t:= 0 to FPayloadStreams.Count-1 do begin
      ps := FPayloadStreams[t];
      if ps = nil then continue;
      ps.BufferSize := vat.DefaultBufferSegmentSize * vat.DefaultBufferSegmentCount;
      ps.BufferSEgments := vat.DefaultBufferSegmentCount;
      ps.DisableLookAhead := not bReadAhead;
    end;
  finally
    Unlock;
  end;



end;

procedure TVirtualDisk_Advanced.SetFileNAme(const Value: string);
var
  c: byte;
  iSize,bc: int64;
begin
  try
    Forcedirectories(extractfilepath(value));
  except
  end;

  if fileexists(getfilename+'.lck') then
    deletefile(getfilename+'.lck');

  bRepaired := not fileexists(value+'.lck');
  if not bRepaired then
    deletefile(value+'.lck');

  SaveStringAsFile(value+'.lck', 'weeeEEEEeeee');

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
    iSize := size;
    bc := vat.block_count;
    LoadVat;
    if vat.block_count < bc then
      Size := iSize;

    vat.SetOpenMarker;

    SaveVat(false);
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
    scrubber.name := 'Scrubber for '+value;
    scrubber.start;
  end;


//    Debug.Log('Stream is CReated.  Size is '+inttostr(FStream.size));


  finally
    unlock;
  end;
end;

procedure TVirtualDisk_Advanced.SetPayloadPhysical(iFileID: ni;
  physical: int64);
begin
  lock;
  try
    lastpayloadchange := getticker;
    vat.PayloadConfig.filelist[iFileID].physical := physical;
    SaveVatAndConfig;
    FVatStream.FInalizeBuffers;

  finally
    unlock;
  end;

end;

procedure TVirtualDisk_Advanced.SetPayloadPriority(iFileID: ni;
  priority: int64);
begin
  lock;
  try
    lastpayloadchange := getticker;
    vat.PayloadConfig.filelist[iFileID].priority := priority;
    SortPayloadPriorities;
    SaveVatAndConfig;
    FVatStream.FInalizeBuffers;

  finally
    unlock;
  end;

end;

procedure TVirtualDisk_Advanced.SetPayloadQuota(iFileID: ni; max_size: int64);
begin
  lock;
  try
    if max_size < 0 then max_size := -1;
    lastpayloadchange := getticker;
    vat.PayloadConfig.filelist[iFileID].size_limit := max_size;
    SaveVatAndConfig;
    FVatStream.FInalizeBuffers;

  finally
    unlock;
  end;
end;

procedure TVirtualDisk_Advanced.SetSize(const Value: int64);
var
  c: byte;
begin
  inherited;
  vat.block_count := 1+ (value div (BIG_BLOCK_SIZE_IN_BYTES));
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
function  TVirtualDisk_Advanced.SyncAwayBuffer(idx: ni = -1; bOnlyIfallDirty: boolean = false): boolean;
//returns TRUE if buffer was dirty
var
  buffer: TRaidCalculator;
  iStart, iEnd: ni;
  iDone: ni;
  iWRote: ni;
  bFirst: boolean;
  bReadingToSame: boolean;
  iOldStart, iOldEnd: int64;
begin
  result := false;
  Lock;
  try

    if idx = -1 then
      buffer := Fraids[FRaids.count-1] as TRaidCalculator
    else
      buffer := Fraids[idx] as TRaidCalculator;

    //flush buffer
    iStart := 0;
    iEnd := -1;
    //if any blocks are dirtys
    if buffer.AnyDirty then begin
      if not buffer.AllDirty then begin
        if bOnlyIfallDirty THEN
          EXIT;

        bFirst := true;
        //if the starting block is valid
        if buffer.startingblock >=0 then
        while iStart < MAX_BUFFERED_BLOCKS do begin
          //find the first dirty block
          iStart := buffer.FirstDirty(iEnd);
          if iStart >= MAX_BUFFERED_BLOCKS then break;
          //find the block after the last dirty block
          iEnd := buffer.FirstClean(iStart);

          if bFirst then begin
            //if this block was not completely written through then we need to read the old stuff from the disk
            if (iEnd = MAX_BUFFERED_BLOCKS) and (iStart = 0)then begin
              //skip the fetch
            end else begin
              if not buffer.readfromdisk then
                FetchAndMergeRaid(buffer);

              buffer.readfromdisk := true;
            end;
          end;

//          bFirst := false;

          //write the blocks to RAID calculator
//          iWRote := WriteBlocks_RaidCalc(buffer, buffer.StartingBlock+iStart, iEnd-iStart, buffer.byteptr(iStart shl BLOCKSHIFT));
//          iStart := iStart + iWrote;
          //iStart := iEnd;
        end;
      end else begin
//        iWRote := WriteBlocks_RaidCalc(buffer, buffer.StartingBlock, RAID_STRIPE_SIZE_IN_BLOCKS, buffer.byteptr(0));
      end;

      FlushRaid(buffer);    //this does the actual splitting into pieces based on VAT information
      result := true;
    end;

    buffer.Cleardirty;


  finally

    Unlock;
  end;


end;

procedure TVirtualDisk_Advanced.SyncBuffer(lba: int64; bForWriting: boolean);
var
  iStart, iEnd: ni;
  iDone: ni;
  iWRote: ni;
  bFirst: boolean;
  bReadingToSame: boolean;
  bLAtentRead: boolean;
  iOldStart, iOldEnd: int64;
  buffer: TraidCalculator;
begin

  lba := (lba div MAX_BUFFERED_BLOCKS) * MAX_BUFFERED_BLOCKS;

  if bForWriting then begin
    //if this is for writing then we just need to get a buffer, period
    buffer := NeedRaid(lba)
  end else begin
    //else we need to
    //1. get a buffer
    buffer := NeedRaid(lba);

    {$IFDEF USE_VAT_HINTS}
      if buffer.startingblock >=0 then
        vathints[buffer.startingblock div RAID_STRIPE_SIZE_IN_BLOCKS] := true;
    {$ENDIF}


    //if this buffer was not read from the disk
    if buffer.readfromdisk then
      exit
    else begin
      //if it has been written to
      if (buffer.AnyDirty) then begin
        //fetch but merge written blocks
        if not (buffer.AllDirty) then
          FetchAndMergeRaid(buffer);
      end
      else
        //fetch blocks
        FetchRaid(buffer);
    end;
  end;



end;






procedure TVirtualDisk_Advanced.UnpauseScrubber;
begin
  lastpayloadchange := 0;
end;

{$ENDIF}

procedure TVirtualDisk_Advanced.WriteBlock(const lba: int64; const p: pbyte);
begin
  WriteBlocks(lba, 1, p);
end;

var
  hits: integer;

function TVirtualDisk_Advanced.WriteBlocks(const lba: int64; const cnt: nativeint;
  p: pbyte): nativeint;
begin

  if not bInReconstitution then
    writes := (writes + 1) and $7FFFFFFFFFFFFFFF;


{$IFDEF BUFFER_BLOCKS}
  result := WriteBlocks_Buffered(lba, cnt, p);
{$ELSE}
  result := WriteBlocks_Direct(lba, cnt, p);
{$ENDIF}
end;

{$IFDEF BUFFER_BLOCKS}
function TVirtualDisk_Advanced.WriteBlocks_Buffered(const lba: int64; const cnt: nativeint;
  p: pbyte): nativeint;
var
  i,ioffset, iByteOffset: nativeint;
  iToWrite: nativeint;
  buffer: TRaidCalculator;
begin
  Lock;
  try
    //sync up the buffer to the desired position
    SyncBuffer(lba, true);//sets up FRaids[0] to be what we want

    //determine which slot in the buffer we're using
    iOffset := lba and (int64(MAX_BUFFERED_BLOCKS-1));


    //write the data to the buffer
    iByteOffset := iOffset shl BLOCKSHIFT;
    iToWrite := lesserof(cnt, MAX_BUFFERED_BLOCKS-iOffset);
    result := iToWrite;
    iToWrite := iToWRite shl BLOCKSHIFT;


    //move data buffer offset $200 * 0,1,2,or 3
    //                                        data
    //                                             size of data (count shl BLOCKSHIFT)
    buffer := TRaidCalculator(Fraids[0]);

    movemem32(buffer.byteptr(iByteOffset), p, iToWrite);

    //mark dirty
    for i := iOffset to (iOffset+result)-1 do begin
      buffer.dirty[i] := true;    //!FIXME! Set them all dirty?  Is that important?

    end;
  finally
    Unlock;
  end;

end;
{$ENDIF}

function TVirtualDisk_Advanced.WriteBlocks_Direct(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
begin
  result := WriteBlocks_Direct_Single(lba, cnt, p);
end;

function TVirtualDisk_Advanced.WriteBlocks_Direct_Single(const lba: int64; const cnt: nativeint; p: pbyte): nativeint;
var
  actual_addr: TVirtualAddressRecord;
  end_actual_addr: TVirtualAddressRecord;
  bDifferentFiles: boolean;
  totalsize: int64;
  startingbigblock: int64;
  startingaddress: int64;
  fs: TPayloadStream;
  lcnt: nativeint;
begin
  lcnt := cnt;

  result := cnt;

  lock;
  try
    totalsize := lcnt shl BLOCKSHIFT;
    startingbigblock := (lba div _BIG_BLOCK_SIZE_IN_BLOCKS) * _BIG_BLOCK_SIZE_IN_BLOCKS;
    startingAddress := lba shl BLOCKSHIFT;
    if (startingaddress) >= (Size+Blocksize) then
      raise ECritical.create('Address beyond the size of the disk! '+inttohex(lba shl BLOCKSHIFT,2));


    GrowIfNeededBlock(lba+(lcnt-1));
    actual_addr := VirtualToPhysicalAddr(lba shl BLOCKSHIFT);
    end_actual_addr := VirtualToPhysicalAddr((lba+lcnt) shl BLOCKSHIFT);
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

        end_actual_addr := VirtualToPhysicalAddr(startingbigblock shl BLOCKSHIFT);
{$IFNDEF ALLOW_RAID}
        end_actual_addr.PhysicalAddress := end_actual_addr.PhysicalAddress + (_BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT);
{$ELSE}
        end_actual_addr := vat.GetTableEntryForLBA(startingbigblock)^;
        //end_actual_addr := VirtualToPhysicalAddr(((startingbigblock) * _BIG_BLOCK_SIZE_IN_BLOCKS) shl BLOCKSHIFT);
        end_actual_addr.FPs[0].physicalAddr := end_actual_addr.FPs[0].physicalAddr+GEtBigblocksizeInBytes(1, false);
{$ENDIF}
      end;

      //actual that CAN be written
{$IFNDEF ALLOW_RAID}
      result := (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) shr BLOCKSHIFT;
{$ELSE}
      result := (end_actual_addr.FPs[0].PhysicalAddr - actual_addr.FPs[0].PhysicalAddr) shr BLOCKSHIFT;
{$ENDIF}
      lcnt := result;
      totalsize := lcnt shl BLOCKSHIFT;
{$IFNDEF ALLOW_RAID}
      if (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <> (totalsize) then begin
        raise ECritical.create('Block segment fault failure.  '+ inttostr((end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress))+','+inttostr(totalsize));
{$ELSE}
      if (end_actual_addr.FPs[0].PhysicalAddr - actual_addr.FPs[0].PhysicalAddr) <> (totalsize) then begin
        raise ECritical.create('Block segment fault failure.  ');
{$ENDIF}

      end;
    end;
//    Debug.Log(inttohex(lba shl BLOCKSHIFT, 2)+'<-actual_addr(wb)->'+inttohex(actual_addr, 2));




//    Debug.Log('About to write to virtual disk at '+inttohex(actual_addr.Address,0)+' for lba '+inttohex(lba,0)+' lcnt:'+inttostr(lcnt));

{$IFDEF ALLOW_RAID}
    fs := FPayloadStreams[actual_addr.FPs[0].fileid];
{$IFDEF USESEEKLOCK}    fs.SeekLock;{$ENDIF}
    try
    fs.Seek(actual_addr.FPs[0].physicalAddr,0);
{$ELSE}
    fs := FPayloadStreams[actual_addr.FileId];
{$IFDEF USESEEKLOCK}    fs.SeekLock;{$ENDIF}
    try
    fs.Seek(actual_addr.PhysicalAddress,0);
{$ENDIF}
    Stream_GuaranteeWrite(fs, p, BLOCKSIZE*lcnt)
    finally
{$IFDEF USESEEKLOCK}      fs.seekunlock;{$ENDIF}
    end;


  finally
    Unlock;
  end;

end;

function TVirtualDisk_Advanced.WriteBlocks_RaidCalc(const rc: TRaidCalculator; const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
var
  lba_off: int64;
  iCanRead: int64;
  buffer: TRaidCalculator;
begin
  //determine offset into RAID space
  buffer := rc;
  lba_off := lba-buffer.StartingBlock;
  if (lba_off < 0) or (lba_off >= RAID_STRIPE_SIZE_IN_BLOCKS) then begin
    raise ECritical.create('Something is wrong and lba is not within RAID stripe.');
  end;

  iCanRead := lesserof(RAID_STRIPE_SIZE_IN_BLOCKS - lba_off, cnt);

  movemem32(buffer.byteptr(lba_off shl BLOCKSHIFT),p, iCanRead shl BLOCKSHIFT);

  result := iCanRead;

end;

{$IFDEF ALLOW_RAID}
procedure TVirtualDisk_Advanced.WriteData(const addr: int64; const cnt: nativeint; p: pbyte);
begin
  Lock;
  try
    if (cnt mod BlockSize) <> 0 then
      raise ECRitical.create('Cnt is not a multiple of block size');

    if (addr mod BlockSize) <> 0 then
      raise ECRitical.create('addr is not a multiple of block size');


    GuaranteeWriteBlocks(addr shr BLOCKSHIFT, cnt shr BLOCKSHIFT, p);
  finally
    Unlock;
  end;
end;
{$ENDIF}

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

  inherited;

end;

function TAbstractVirtualDisk_Advanced.BlockCount: int64;
begin
  result := Size shr BLOCKSHIFT;
end;

function TAbstractVirtualDisk_Advanced.BlockMargin: int64;
begin
  result := BlockSize*64;
end;

constructor TAbstractVirtualDisk_Advanced.Create;
begin
  inherited;
{$ifdef use_vat}
  vat.initvirgin;
{$endif}
  enableoptionalDebugging := true;
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
    iPos :=addr;
    iEnd := ipos + cnt;
    iTot := 0;
    while iPos < (iEnd) do begin
      iOff := iPos mod blocksize;
      iCan := lesserof(blocksize - iOff, iEnd-iPos);
      readblock(addr shr BLOCKSHIFT, pblock);
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
        readblock(addr shr BLOCKSHIFT, pblock);
      movemem32(@pblock[iOff], @p[iTot], iCAn);
      writeblock(addr shr BLOCKSHIFT, pblock);

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

function TAbstractVirtualDisk_Advanced.GetStatus: string;
begin
  result := vat.PayloadConfig.Status;
end;

function TAbstractVirtualDisk_Advanced.GEtVerboseDebug(sToFile: string): boolean;
var
  t: ni;
  sl: TStringlist;
  s: string;
begin
  result := true;

  sl := TStringlist.create;
  try
    for t := 0 to vat.block_count-1 do begin
      if vat.table[t].FileCount > 0 then begin
        s := GetVerboseDebug(t);
        if s <> '' then
          sl.add(s);
      end;
    end;
    sl.SaveToFile(stoFile);
  finally

    sl.free;
  end;




end;


function TAbstractVirtualDisk_Advanced.GEtVerboseDebug(iBigBlock: ni): string;
var
  t,u: ni;
  sl: TStringList;
  a: array[0..BLOCKSIZE-1] of byte;
  vart: PVirtualAddressRecord;
begin
  result := '';
  exit;
  vart := @self.vat.table[iBigBlock];
  lock;
  try
    sl := TStringlist.create;
    try
      sl.Add('***********BIG BLOCK '+inttohex(vart.StartingBlock shl BLOCKSHIFT,0));
      for t:= 0 to _BIG_BLOCK_SIZE_IN_BLOCKS-1 do begin
        self.ReadBlock(vart.StartingBlock+t, @a[0]);
        for u:= 0 to BLOCKSIZE-1 do begin
          if (u mod 64) <> 0 then continue;
          sl.add(memorydebugstring(@a[u], 63));
        end;
      end;
      result := sl.text;
    finally
      sl.free;
    end;

  finally
    unlock;
  end;

end;

procedure TAbstractVirtualDisk_Advanced.GuaranteeReadBlocks(lba: int64; cnt: nativeint;
  p: pbyte);
var
  iJustRead, iRead: nativeint;
  pp: pbyte;
begin
//  Debug.Log('GRB:'+inttostr(lba)+','+inttostr(cnt));
  iRead := 0;
  iJustRead := 0;
  pp := p;
  while cnt > 0 do begin
    iJustRead := ReadBlocks(lba, cnt, pp);
    inc(pp, iJustRead);
    inc(lba, iJustRead);
    dec(cnt, iJustRead);
    inc(iRead, iJustRead);
  end;


end;




procedure TAbstractVirtualDisk_Advanced.GuaranteeWriteBlocks(lba: int64; cnt: nativeint;
  p: pbyte);
var
  iJustWrote: nativeint;
  pp: pbyte;
begin
//  Debug.Log('GWB:'+inttostr(lba)+','+inttostr(cnt));
  pp := p;
  while cnt > 0 do begin
    iJustWrote := WriteBlocks(lba, cnt, pp);
    inc(lba, iJustWrote);
    dec(cnt, iJustWrote);
    inc(pp, iJustWrote shl BLOCKSHIFT);
  end;
end;







procedure TAbstractVirtualDisk_Advanced.optdebug(s: string);
begin
  if EnableOptionalDebugging then
    Debug.Log(s);

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
  fSize := (FRequestedSize shr BLOCKSHIFT) shl BLOCKSHIFT;

end;

function TAbstractVirtualDisk_Advanced.TailStartAddr: int64;
begin
  result := Size - TailSize;
end;

function TAbstractVirtualDisk_Advanced.TailSize: int64;
begin
  result := 8192 shl BLOCKSHIFT;
end;

function TVirtualDisk_Advanced.VirtualToPhysicalAddr(addr: int64): TVirtualAddressRecord;
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
  r := vat.GetTableEntryForLBA(addr shr BLOCKSHIFT);

//  debug.log('Initial translation:'+inttostr(r.Address));
{$IFDEF ALLOW_RAID}
  if r.FileCount = 0 then begin
{$ELSE}
  if not r.IsAssigned then begin
{$ENDIF}
    //{$MEssage Error 'r.Address can be 0 now... must initialize differently'}

    varr := NewPhysicalBlockAddress((addr shr BLOCKSHIFT) shl BLOCKSHIFT);
    r := vat.GetTableEntryForLBA(addr shr BLOCKSHIFT);

{$IFNDEF ALLOW_RAID}
    r.PhysicalAddress := varr.PhysicalAddress;
{$ELSE}
    //r^ := varr;
{$ENDIF}
    //r.Marker := varr.Marker;
{$IFDEF ALLOW_RAID}
    //r.FPs[0] := varr.FPs[0];
{$ELSE}
    //r.FileID := varr.FileID;
{$ENDIF}
    //r.StartingBlock := tindex * _BIG_BLOCK_SIZE_IN_BLOCKS ;
    vat.MarkTableEntryDirtyByPtr(r);
    SaveVat(false);
  end;

{$IFDEF ALLOW_RAID}
  result := r^;

{$ELSE}
  result.FileID := r.FileID;
{$ENDIF}
//  ConsoleLog('Prefinal:'+inttostr(r.Address));
{$IFDEF ALLOW_RAID}
  result.FPs[0].PhysicalAddr := r.FPs[0].PhysicalAddr+(addr mod (_BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT));
{$ELSE}
  result.PhysicalAddress := r.PhysicalAddress+(addr mod (_BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT));
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
procedure TVirtualAddressTable.ChangeAllFiles(iFromID, iToID: fi);
var
  t, u: ni;
  vart: PVirtualAddressRecord;
  pf: PFilePhysical;
begin
  for t:= 0 to high(table) do begin
    vart := @table[t];
    for u := 0 to vart.FileCount-1 do begin
      pf := @vart.FPs[u];
      if pf.FileID = iFromID then
        pf.FileID := iToID;

    end;

  end;

end;

function TVirtualAddressTable.CountPopulatedTableEntries: ni;
var
  t: ni;
begin
  result := 0;
  for t:= 0 to block_count-1 do begin
    if table[t].FileCount > 0 then
      inc(result);
  end;

end;

function TVirtualAddressTable.DebugVatSTructure: string;
var
  t,u: ni;
  sl: TStringlist;
begin
  sl := TSTringlist.create;
  try
    t := 0;
    while t< high(self.table) do begin

{$IFDEF ALLOW_RAID}
      if self.table[t].FPs[0].PhysicalAddr >=0 then
        sl.add(self.table[t].DebugString);
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


function TVirtualAddressTable.FindFilePhysical(iDISKID: fi;
  greaterthan: int64): PFilePhysical;
var
  t,u: ni;
  vart: PVirtualAddressRecord;
  pf: PFilePhysical;
begin
  result := nil;
  for t:= low(table) to high(table) do begin
    vart := @table[t];
    for u := 0 to vart.filecount-1 do begin
      pf := vart.GetFP(iDISKID);
      if pf <> nil then begin
        if pf.PhysicalAddr > greaterthan then begin
          if (result = nil) or (result.PhysicalAddr > pf.PhysicalAddr) then begin
            result := pf;
          end;
        end;
      end;
    end;
  end;
end;

function TVirtualAddressTable.FindGapLocation(sz, fileid: fi; out physical: int64; not_this_physical: int64): boolean;
var
  t: fi;
  vart: PVirtualAddressRecord;
  fp: PFilePhysical;
  gapsize: int64;
  varnext: PVirtualAddressREcord;
  varnext2: PVirtualAddressRecord;
  fpnext: PFilePhysical;
  bbs: int64;
begin
  physical := $FFFFFFFFFFFFFFFF;
  result := false;
  //check the first block
  varnext2 := FindNextBlockForFileIDInVat(fileid, sizeof(TVirtualDiskPayloadFileHeader));
  if varnext2 <> nil then begin
    fpnext := varnext2.GetFP(fileid);

    //calculate size of gap from the beginning of the file to the first block taking into account the size of the header
    gapsize := (fpnext.PhysicalAddr - sizeof(TVirtualDiskPayloadFileHeader));

    //if the gap is big enough, then use it
    if (gapsize >= sz) then begin
      physical := sizeof(TVirtualDiskPayloadFileHeader)-1;
      result := true;
      exit;
    end;
  end;


  for t:= low(table) to high(table) do begin
    vart := @table[t];
    if not vart.HasFile(fileid) then
      continue;
    //get the physical couple for this particular file in the VAT
    fp := vart.GetFP(fileid);

    //get the block that follows this one in the file
    varnext := FindNextBlockForFileIDInVat(fileid, fp.PhysicalAddr);;
    //if no next block is found, continue
    if varnext = nil then
      continue;


    fpnext := varnext.GetFP(fileid);
    if fpnext = nil then
      continue;


    //calculate size of gap taking into account the size of current block
    bbs := GetBigBlockSizeInBytes(vart.FileCount, true);
    gapsize := (fpnext.PhysicalAddr - fp.PhysicalAddr) - bbs;

    //if the gap is big enough, then use it
    if (gapsize >= sz) and ((fp.physicaladdr+bbs) > not_this_physical) then begin
      self.MarkTableEntryDirty(t);
      physical := fp.physicaladdr + bbs;
      result := true;
      exit;
    end;



  end;
end;

function TVirtualAddressTable.FindNextBlockForFileIDInVat(fileid: fi;
  beyond: int64): PVirtualAddressRecord;
var
  t: fi;
  vart: PVirtualAddressRecord;
  bHas: boolean;
  iMin: int64;
  fp: PFilePhysical;
begin
  iMin := $7fffffffffffffff;

  result := nil;
  //return the minimum physical address that is greater than "beyond"
  for t:= low(table) to high(table) do begin
    vart := @table[t];
    bHas := vart.HasFile(fileid);
    if not bHas then continue;
    fp := vart.GetFP(fileid);
    if (fp.PhysicalAddr < iMin) and (fp.PhysicalAddr > beyond) then begin
      iMin := fp.PhysicalAddr;
      result := vart;
    end;
  end;

end;

function TVirtualAddressTable.FindVARToBuddyUp: PVirtualAddressRecord;
var
  t: fi;
begin
  result := nil;
  for t:= low(table) to high(table) do begin
    if (table[t].FileCount = 1) and (table[t].FPs[0].FileID > -1) then begin
      result := @table[t];
      exit;
    end;
  end;
end;

function TVirtualAddressTable.FindVARToRAIDUp: PVirtualAddressRecord;
var
  t: fi;
begin
  result := nil;
  for t:= low(table) to high(table) do begin
    if table[t].HasFile(-1) then begin
      result := @table[t];
      exit;
    end;
  end;
end;


function TVirtualAddressTable.FindVARToReconstitute: PVirtualAddressRecord;
var
  t,u: fi;
  max_phys: int64;
  pf: PFilePhysical;

begin
  result := nil;
  max_phys := -1;
  //find the block with the highest physical address on a drive with a negative priority
  for t:= low(table) to high(table) do begin
    for u := 0 to table[t].FileCount-1 do begin
      pf := @table[t].FPs[u];
      if pf.FileID >= 0 then begin
        //IF any drives in the table are marked for reconstitution
        if PayloadConfig.filelist[pf.fileid].priority < 0 then begin
          //if the physical address for this block is highest then save it as the result
          if pf.PhysicalAddr > max_phys then begin
            result := @table[t];
            max_phys := pf.PhysicalAddr;
          end;
        end;
      end;
    end;
  end;
end;

function TVirtualAddressTable.FindVarWithHighestPhysicalForPayload(
  iPayloadID: fi): PVirtualAddressRecord;
var
  pf: PFilePhysical;
  t: ni;
  max: int64;
begin
  result := nil;
  max := -1;

  for t:= 0 to high(table) do begin
    pf := table[t].GetFP(iPayloadID);
    if pf <> nil then begin
      if pf.PhysicalAddr > max then begin
        max := pf.physicalAddr;
        result := @table[t];
      end;
    end;
  end;


end;

procedure TVirtualAddressTable.FlushToStream(s: TStream; bForce:boolean);
var
  iStart, iEnd: int64;
begin
  if (not bforce) and ((dirtySTart = -1) and (dirtyEnd = 0)) then
    exit;
//  if (iStart < 0) and (iEnd < 0) then exit;
//  dirtyStart := -1;
//  dirtyEnd := -1;

  iStart := dirtyStart;
  iEnd := dirtyEnd;
  if (iStart < 0) or bForce then iStart := 0;
  if (iEnd < 0) or bForce then
    iEnd := pbyte(self.persistent_end)-pbyte(@self);

  s.Seek(iStart,0);
//  Debug.Log('Flush:'+NEWLINE+self.DebugVatSTructure);
  Stream_GuaranteeWrite(s, pbyte(@self)+iStart, (iEnd-iStart)+1);
  dirtyStart := -1;
  dirtyEnd := 0;
  if s is TMultiBufferMemoryFileStream then
    TMultiBufferMemoryFileStream(s).Flush;



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


function TVirtualAddressTable.GetSafePhysicalAddr(iFileID: fi): int64;
var
  t,u: ni;
  vart: PVirtualAddressRecord;
  fpf: PFilePhysical;
begin

  vart := FindVarWithHighestPhysicalForPayload(iFileID);
  result := vart.GetFP(iFileID).PhysicalAddr+GetBigBlockSizeInBytes(vart.FileCount, false)+SizeOf(TVirtualDiskBigBlockHEader);

  if vart <> nil then begin
    result := vart.GetFP(iFileID).PhysicalAddr+GetBigBlockSizeInBytes(vart.FileCount, false)+SizeOf(TVirtualDiskBigBlockHEader);
  end;

end;

function TVirtualAddressTable.GetTableEntryForLBA(
  lba: int64): PVirtualAddressRecord;
var
  t: nativeint;
begin
  lba := lba div _BIG_BLOCK_SIZE_IN_BLOCKS;
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


function TVirtualAddressTable.HasFile(iPayloadID: fi): boolean;
begin
  result := FindVarWithHighestPhysicalForPayload(iPayloadID) <> nil;
end;

procedure TVirtualAddressTable.InitVirgin;
var
  t: ni;
begin
  marker := $5555;
  for t:= low(self.table) to high(table) do begin
    table[t].initvirgin(t * _BIG_BLOCK_SIZE_IN_BLOCKS);
    table[t].StartingBlock := t*_BIG_BLOCK_SIZE_IN_BLOCKS;
    MarkTableEntryDirty(t);
  end;

end;

function TVirtualAddressTable.LocalAddr(p: pointer): int64;
begin
  result := pbyte(p)-pbyte(@self);

end;

procedure TVirtualAddressTable.MarkDirty(const iStart, iEnd: int64);
begin
  needsbackup := true;
  if (iStart <self.dirtyStart) or (self.dirtyStart < 0) then begin
    if iStart >= persistentsize then begin
      debug.consolelog('wtf iStart='+inttostr(iStart));
      dirtystart := 0;
    end else
      dirtyStart := iStart;
  end;

  if (iENd >self.dirtyEnd) or (self.dirtyEnd < 0) then begin
    dirtyEnd := iENd;
  end;

end;

procedure TVirtualAddressTable.MarkDirty(ptr: pointer; iLEngth: fi);
var
  iStart: int64;
begin
  needsbackup := true;
  iStart := localaddr(ptr);
  MArkDirty(iStart, iStart+iLength-1);
end;

procedure TVirtualAddressTable.MarkTableEntryDirty(idx: fi);
var
  iStart, iEnd: int64;
begin
  iStart := (idx * sizeof(TvirtualAddressRecord))+(int64(pbyte(@table[0])-int64(@self)));
  iEnd := iStart + sizeof(TVirtualAddressRecord)-1;

  MArkDirty(iStart,iEnd);

end;

procedure TVirtualAddressTable.MarkTableEntryDirtyByPtr(
  ptr: PVirtualAddressRecord);
var
  iStart, iEnd: int64;
begin
  needsbackup := true;
  iStart := pbyte(ptr)-pbyte(@self.table[0]);
  iEnd := iStart + sizeof(TVirtualAddressRecord)-1;
  if (iStart <self.dirtyStart) or (self.dirtyStart < 0) then begin
    dirtyStart := iStart;
  end;

  if (iENd >self.dirtyEnd) or (self.dirtyEnd < 0) then begin
    dirtyEnd := iENd;
  end;

end;


function TVirtualAddressTable.Persistent_End: pbyte;
begin
  result := pbyte(ni(@table[0])+ni(block_count * SizeOf(TVirtualAddressRecord)));
end;

function TVirtualAddressTable.PersistedSize: int64;
begin
  result := pbyte(self.persistent_end)-pbyte(@self);

end;

function TVirtualAddressTable.PersistentSize: int64;
begin
  result := pbyte(self.persistent_end)-pbyte(@self)
end;

procedure TVirtualAddressTable.ReadFromStream(s: TStream);
var
  sz: ni;
begin
  s.Seek(0,0);
  sz := nativeint(pbyte(self.persistent_end)-pbyte(@self));
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

function TVirtualAddressTable.VerifyGaps(bThrowExceptions: boolean): boolean;
var
  t,cpus: ni;
  fpf: PFilePhysical;
  c: array of Tcmd_VerifyGapsPArt;
begin
  result := true;

  setlength(c,length(payloadconfig.filelist));

  cpus := GetProcessorCount-1;
  if cpus = 0 then cpus := 1;
  for t:= 0 to cpus-1 do begin
    c[t] := nil;
//    if payloadconfig.filelist[t].NameAsString <> '' then begin
      c[t] := Tcmd_VerifyGapsPArt.Create;
      c[t].selfie := @self;
      c[t].divd := cpus;
      c[t].modd := t;
//      c[t].FileIndex := t;
      c[t].CPUExpense := 1.0;
      //c[t].MemoryExpense := 1.0;
      c[t].start;
//    end;
  end;

  for t:= 0 to cpus-1 do begin
    if c[t] <> nil then begin
      c[t].WaitFor;
      if c[t].error <>'' then begin
        if bThrowExceptions then begin
          raise ECritical.create(c[t].error);
        end else begin
          result := false;
        end;
      end;
      c[t].free;
      c[t] := nil;
    end;
  end;
end;

function TVirtualAddressTable.VerifyGapSingle(idx: int64): boolean;
{$IFDEF MQ}
var
  t,u: ni;
  vart: PVirtualAddressRecord;
  a: array[0..15] of Tcmd_VerifyGapsSinglePart;
begin
  result := true;
  vart := @self.table[idx];
  for t:= 0 to vart.FileCount-1 do begin
    a[t] := Tcmd_VerifyGapsSinglePart.Create;
    a[t].vat := @self;
    a[t].u := t;
    a[t].block := idx;
    gmq.AddItem(a[t]);
  end;

  for t:= 0 to vart.FileCount-1 do begin
    a[t].WAitFor;
    a[t].Free;
    result := result and a[t].result;
  end;
end;
{$ELSE}
var
  t,u: ni;
  vart: PVirtualAddressRecord;
  fp, fpf: PFilePhysical;
begin
  result := true;
  t := idx;
  vart := @self.table[t];
  for u := 0 to vart.FileCount-1 do begin
    //u := self.FileIndex;// to self.fileindex do begin
    //find file physical that is next after this one
    fp := @vart.FPs[u];
    if fp<>nil then begin
      fpf := self.FindFilePhysical(fp.FileID, fp.PhysicalAddr);
      if fpf <> nil then begin
        if (fpf.PhysicalAddr - vart.fps[u].PhysicalAddr) < GetBigBlockSizeInBytes(vart.FileCount, true) then begin
          //vart.RemoveFile(vart.FPs[u].FileID);
          result := false;
        end;
      end;
    end;
  end;
end;
{$ENDIF}

{$endif}

{ TVirtualAddressRecord }

procedure TVirtualAddressRecord.ChangeFileID(ifrom, ito: fi; newphysical: int64);
var
  t: ni;
begin
{$IFNDEF ALLOW_RAID}
  fileid := iTO;
{$ELSE}

  for t:= low(FPs) to low(FPs)+FileCount-1 do begin
    if FPs[t].FileID = iFrom then begin
      FPs[t].FileID := iTo;
      FPs[t].PhysicalAddr := newphysical;
    end;
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
    result := @FPs[i];
  end;
end;
{$ENDIF}

{$IFDEF ALLOW_RAID}
function TVirtualAddressRecord.IndexOf(iFileID, iPhysical: int64): fi;
var
  t: ni;
begin
  result := -1;
  for t:= low(FPs) to (low(FPs)+filecount-1) do begin
    if (FPs[t].FileID = iFileID) and (FPs[t].PhysicalAddr = iPhysical) then begin
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

function TVirtualAddressRecord.GetFP(iFileID: fi): PFilePhysical;
var
  i: ni;
begin
  result := nil;
  i := IndexOf(iFileID);
  if i >= 0 then begin
    result := @FPs[i];
  end;
end;
function TVirtualAddressRecord.HasFile(iFileID: fi): boolean;
begin
  result := IndexOf(iFileID) >=0;
end;

function TVirtualAddressRecord.HasUndefinedLocations: boolean;
var
  t: ni;
begin
  result := false;
  for t:= 0 to filecount-1 do begin
    if FPs[t].FileID < 0 then begin
      result := true;
      exit;
    end;
  end;

end;

{$ENDIF}

{$IFDEF ALLOW_RAID}
function TVirtualAddressRecord.IndexOf(iFileID: fi): fi;
var
  t: ni;
begin
  result := -1;
  for t:= low(FPs) to (low(FPs)+filecount-1) do begin
    if (FPs[t].FileID = iFileID) then begin
      result := t;
      exit;
    end;
  end;

end;
{$ENDIF}

function TVirtualAddressRecord.DebugString: string;
var
  t: ni;
begin
{$IFDEF ALLOW_RAID}
  result := '[va:'+inttohex(self.StartingBlock shl BLOCKSHIFT,0)+']';//+'[f:RAIDME:RAIDME]';
  for t:= 0 to self.FileCount-1 do begin
    result := result + '[f'+inttostr(FPs[t].fileid)+'::'+inttohex(self.FPs[t].PhysicalAddr,0)+']';
  end;

{$ELSE}
  result := '[va:'+inttohex(self.StartingBlock shl BLOCKSHIFT,0)+']'+'[f:'+inttostr(fileid)+'::'+inttohex(self.PhysicalAddress,0)+']';
{$ENDIF}

end;

procedure TVirtualAddressRecord.InitVirgin(iStartingBlock: int64);
var
  t: ni;
begin
{$IFDEF ALLOW_RAID}
  for t:= low(FPs) to high(FPs) do begin
    FPs[t].InitVirgin;
  end;
  filecount := 0;
{$ELSE}
  FileID := -1;
  PhysicalAddress := -1;
  junk := 1;
{$ENDIF}
  Marker := $5555;
  StartingBlock := iStartingblock;

end;

function TVirtualAddressRecord.IsAssigned: boolean;
begin
{$IFNDEF ALLOW_RAID}
  result := (FileID > -1) and (PhysicalAddress > -1);
{$ELSE}
  result := FileCount > 0;
{$ENDIF}
end;

procedure TVirtualAddressRecord.RemoveFile(idx: fi);
var
  t: ni;
begin
  //if it is a mirror, then we can just remove it
  if filecount = 2 then begin
    for t:= idx to FileCount-2 do begin
      FPs[t] := FPs[t+1];
    end;
    dec(filecount);
  end else begin
    //if this is 1-drive or RAID then we need to flag this as a missing chunk
    FPs[idx].physicalAddr := -1;
    FPs[idx].FileID := -1;
  end;







end;

{ TVirtualDiskPayloadFileHeader }

procedure TVirtualDiskPayloadFileHeader.Init;
begin
  _junk_NextPhysicalAddress := sizeof(TVirtualDiskPayloadFileHEader);
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
      vd.Size := ap.GetItemEx('LUN'+inttostr(t)+':Size', int64(256*MEGA));
      vd.FileName := ap.GetItemEx('LUN'+inttostr(t)+':FileName', '');
      vd.Identifier := ap.GetItemEx('LUN'+inttostr(t)+':Identifier', 'LUN'+inttostr(t));
      vd.CacheSize := ap.GetItemEx('LUN'+inttostr(t)+':CacheSize', int64(2048+MEGA));
      vd.CachedStripes := ap.GetItemEx('LUN'+inttostr(t)+':CachedSTripes', DEFAULT_RAID_CALC_COUNT);
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


  //CreateVirtualDiskHub;

end;
procedure ofinal;
begin
  //DestroyVirtualDiskHub;
end;

{ TVirtualDiskPayloadConfiguration }

procedure TVirtualDiskPayloadConfiguration.AddPayload(sFile: string;
  max_size: int64; iPhysical: fi; iPriority: fi; iFlags: int64);
var
  i: ni;
begin
  i := FindUnusedSlot;
  if i < 0 then
    raise ECritical.create('Could not add payload.  Could not find unused payload slot.');


  if fileexists(sFile) then deletefile(sFile);
  filelist[i].NameAsString := sFile;
  filelist[i].size_limit := max_size;
  filelist[i].physical := iPhysical;
  filelist[i].priority := iPriority;
  filelist[i].flags := iFlags;



  
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

function TVirtualDiskPayloadConfiguration.GetStatus: string;
begin
  result := FStatus;
end;

procedure TVirtualDiskPayloadConfiguration.SetStatus(const Value: string);
begin
  fillmem(@FStatus[0], sizeof(FStatus), 0);
  movemem32(@FStatus[0], @value[STRZ], length(value)*sizeof(char));
end;

{ TPayloadFileInformation }

function TPayloadFileInformation.GetFlagMissing: boolean;
begin
  result := BitGet(@self.flags, BIT_FLAG_MISSING);
end;

function TPayloadFileInformation.GetNameAsString: string;
begin
  result := name;
end;


procedure TPayloadFileInformation.SetFlagMissing(const Value: boolean);
begin
  BitSet(@self.flags, BIT_FLAG_MISSING, value);
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

function TVDBlockBuffer.FirstClean(iAfter: ni): ni;
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

constructor TPayloadStream.Create(const AFileName: string; Mode: cardinal; Rights: cardinal; Flags: cardinal);
begin
  inherited Create(AfileName, Mode, Rights, Flags);
  CheckInit;
  stats := TRingStats.Create;
  FCollapsable := true;
  self.FQueue.MaxItemsInQueue := 32;
end;


destructor TPayloadStream.Destroy;
begin
  stats.free;
  stats := nil;
  inherited;
end;

function TPayloadStream.DropBigBlock(out pfh: TVirtualDiskPayloadFileHeader;
  out p: pbyte; out iSize: int64): boolean;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPayloadStream.Expand(iSizeBeyondCurrentExcludingHeaders: int64; virtual_addr: int64): int64;
var
  bbh: TVirtualDiskBigBlockHeader;
  addr: int64;
  pfh: TVirtualDiskPayloadFileHeader;
begin
  CheckInit;
  addr := Self.GetNextBigBlockAddress;
  bbh.HeaderStart := addr;
  bbh.TotalSizeIncludingHeaders := iSizeBeyondCurrentExcludingHeaders+(sizeof(TVirtualDiskBigBlockHeader)*2);
  bbh.PayloadStart := addr + sizeof(TVirtualDiskBigBlockHEader);
  result := bbh.PayloadStart;
  bbh.FooterStart := bbh.PayloadStart + iSizeBeyondCurrentExcludingHeaders;
  bbh.VirtualAddress := virtual_addr;
  self.Seek(addr, soBeginning);
  //Header
  Stream_GuaranteeWrite(self, @bbh, sizeof(bbh));
  //payload
  Stream_WriteZeros(self, iSizeBeyondCurrentExcludingHeaders);
  //footer
  Stream_GuaranteeWrite(self, @bbh, sizeof(bbh));
  //result := self.Size;

{$IFDEF REDUNDANT_PHYSICAL}
  pfh := self.GetPFH;
  pfh._NextPhysicalAddress := Size;
  self.SetPFH(pfh);
{$ENDIF}



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
var
  iLast: int64;
begin

  iLast := self.GetLastBigBlockFooterAddr;
  //debug.consolelog('last:'+inttostr(iLast)+' size:'+inttostr(size));
  self.Seek(iLAst, soBeginning);
  try
    Stream_GuaranteeRead(self, @result, sizeof(result),true)
  except
    Debug.ConsoleLog('Something is very wrong and we couldn''t read the last bigblock header... trying to recover.');
    Debug.Log('Something is very wrong and we couldn''t read the last bigblock header... trying to recover.');
    self.FinalizeBuffers;
    iLast := self.GetLastBigBlockFooterAddr;
    Stream_GuaranteeRead(self, @result, sizeof(result),true)
  end;


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
  result := size;
{$IFDEF REDUNDANT_PHYSICAL}
  RESULT := getpfh._NextPhysicalAddress;
  if result > size then
    result := size;
{$ENDIF}

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

procedure TPayloadStream.MoveBigBlock(fromAddr, toAddr: int64; sz: int64);
var
  p: pbyte;
  head: PVirtualDiskBigBlockHeader;
begin
  p := GetMemory(sz);
  try
    self.Seek(fromAddr, soBeginning);
    Stream_GuaranteeRead(self, p, sz, true);

    head := PVirtualDiskBigBlockHeader(p);
    head.HeaderStart := toAddr;
    head.PayloadStart := toAddr + SizeOf(TVirtualDiskBigBlockHeader);
    head.FooterStart := head.PayloadStart+(sz-(2*sizeof(TVirtualDiskBigBlockHeader)));

    self.Seek(toAddr, soBeginning);
    Stream_GuaranteeWrite(self, p, sz);

  finally
    FreeMemory(p);
  end;


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
//    and     (VirtualAddress = vaddr)
    and     (PayloadSTart = (HeaderStart + sizeof(self)));
end;

{ TFilePhysical }

procedure TFilePhysical.InitVirgin;
begin
  FileID := -1;
  PhysicalAddr := -1;
end;

{ TVirtualDisk_Comparative }

procedure TVirtualDisk_Comparative.AddPayload(sFile: string; size_limit: int64;
  iPhysical, iPriority: ni; iFlags: int64);
begin
  comp1.AddPayload(sFile, size_limit, iPhysical, iPriority, iFlags);

end;

procedure TVirtualDisk_Comparative.BeginRepair;
begin
  comp1.BeginRepair;
end;

procedure TVirtualDisk_Comparative.ClearRepairLog;
begin
  comp1.ClearRepairLog;
end;

constructor TVirtualDisk_Comparative.Create;
begin
  inherited;
  comp1 := TVirtualDisk_Advanced.Create;
  comp2 := TVirtualDisk_SimpleNonLinear.create;
end;

function TVirtualDisk_Comparative.DebugVatStructure: string;
begin
  result := comp1.DebugVatStructure;
end;

procedure TVirtualDisk_Comparative.DecommissionPayload(sFile: string);
begin
  comp1.DecommissionPayload(sFile);
end;

destructor TVirtualDisk_Comparative.Destroy;
var
  a: array[0..BLOCKSIZE-1] of byte;
begin
  comp1.ReadBlock(0, @a[0]);
  SaveSTringASFile(comp1.FileName+'.comp1.0.txt', memorydebugstring(@a[0], BLOCKSIZE));
  comp2.ReadBlock(0, @a[0]);
  SaveSTringASFile(comp2.FileName+'.comp2.0.txt', memorydebugstring(@a[0], BLOCKSIZE));

  comp1.free;
  comp2.free;




  inherited;
end;

function TVirtualDisk_Comparative.DrainRepairLog: string;
begin
  result := comp1.DrainRepairLog;
end;

function TVirtualDisk_Comparative.GetfileName: string;
begin
  result := comp1.FileName;
end;



function TVirtualDisk_Comparative.GetOperational: boolean;
begin
  result := Self.comp1.Operational;
end;

function TVirtualDisk_Comparative.GetPayloadConfig: PVirtualDiskPayloadConfiguration;
begin
  result := comp1.GetPayloadConfig;
end;

function TVirtualDisk_Comparative.GetRepairLog: string;
begin
  result := comp1.GetRepairLog;
end;

procedure TVirtualDisk_Comparative.QuickOnline;
begin
  comp1.quickonline;
end;

procedure TVirtualDisk_Comparative.ReadBlock(const lba: int64; const p: pbyte);
var
  temp: array[0..BLOCKSIZE-1] of byte;
begin
  inherited;
  comp2.ReadBlock(lba, p);
  comp1.ReadBlock(lba, @temp[0]);
  if not CompareMem(p, @temp[0], BLOCKSIZE) then begin
    raise ECritical.create('Compare Fail');
  end;

end;

function TVirtualDisk_Comparative.ReadBlocks(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
var
  pp: pbyte;
begin
//  if lba = $0 then
//    Debug.Log('Trap');
  result := comp2.ReadBlocks(lba, cnt, p);
  Debug.Log('ReadBlocks Read '+inttostr(result)+' @'+inttohex(lba,0));
  pp := GEtMemory(result shl BLOCKSHIFT);
  try

    comp1.GuaranteeReadBlocks(lba, result, pp);
    if not Comparemem(pp, p, result) then begin
      Debug.Log('Bad Read nl '+MemoryDebugString(p, result shl BLOCKSHIFT));
      Debug.Log('Bad Read adv '+MemoryDebugString(pp, result shl BLOCKSHIFT));
      comp1.GuaranteeWriteBlocks(lba, result, p);
      //halt;
      //raise ECritical.create('bad read');

    end;
  finally
    Freememory(pp);
  end;



end;

procedure TVirtualDisk_Comparative.ReadData(const addr: int64;
  const cnt: nativeint; p: pbyte);
begin
  inherited;
  if (cnt mod BlockSize) <> 0 then
    raise ECRitical.create('Cnt is not a multiple of block size');

  if (addr mod BlockSize) <> 0 then
    raise ECRitical.create('addr is not a multiple of block size');

  GuaranteeReadBlocks(addr shr BLOCKSHIFT, cnt shr BLOCKSHIFT, p);

end;



procedure TVirtualDisk_Comparative.RefunctPayload(id: ni; sFile: string);
begin
  comp1.ReFunctPayload(id, sFile);
end;

procedure TVirtualDisk_Comparative.ReSourcePayload(id: ni; sFile: string);
begin
  comp1.ReSourcePayload(id, sFile);
end;

procedure TVirtualDisk_Comparative.SetDefaultCacheParams(iSegmentSize,
  iSegmentCount: ni; bReadAhead: boolean);
begin
  comp1.SetDefaultCacheParams(iSegmentSize, iSegmentCount, bReadAhead);
end;

procedure TVirtualDisk_Comparative.SetFileNAme(const Value: string);
begin
  inherited;
  comp1.FileName := value;
  comp2.FileName := value+'compare.vdnl';
end;

procedure TVirtualDisk_Comparative.SetPayloadPhysical(id, phys: ni);
begin
  comp1.SetPayloadPhysical(id, phys);
end;

procedure TVirtualDisk_Comparative.SetPayloadPriority(id, pri: ni);
begin
  comp1.SetPayloadPriority(id, pri);
end;

procedure TVirtualDisk_Comparative.SetPayloadQuota(iFileID: ni;
  max_size: int64);
begin
  comp1.SetPayloadQuota(iFileID, max_size);
end;

procedure TVirtualDisk_Comparative.SetSize(const Value: int64);
begin
  inherited;
  comp1.Size := value;
  comp2.size := value;
end;

procedure TVirtualDisk_Comparative.UnpauseScrubber;
begin
  comp1.UnpauseScrubber;
end;

procedure TVirtualDisk_Comparative.WriteBlock(const lba: int64; const p: pbyte);
begin
  inherited;
  comp1.WriteBlock(lba, p);
  comp2.WriteBlock(lba, p);

end;

function TVirtualDisk_Comparative.WriteBlocks(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
begin
//  if IsWithin(lba, cnt, $5e3688) then
//    Debug.Log('Trap');


//  if lba = $0 then
//    Debug.Log('Trap');

  result := comp2.WriteBlocks(lba, cnt, p);
//  Debug.ConsoleLog('WriteBlocks Wrote '+inttostr(result)+' @'+inttohex(lba,0));
  comp1.GuaranteeWriteBlocks(lba, result, p);
end;

procedure TVirtualDisk_Comparative.WriteData(const addr: int64;
  const cnt: nativeint; p: pbyte);
begin
  inherited;
  if (cnt mod BlockSize) <> 0 then
    raise ECRitical.create('Cnt is not a multiple of block size');

  if (addr mod BlockSize) <> 0 then
    raise ECRitical.create('addr is not a multiple of block size');

  GuaranteeWRiteBlocks(addr shr BLOCKSHIFT, cnt shr BLOCKSHIFT, p);

end;

{ TVirtualDisk_Fake }

procedure TVirtualDisk_Fake.AddPayload(sFile: string; size_limit: int64;
  iPhysical, iPriority: ni; iFlags: int64);
begin
  //
end;

procedure TVirtualDisk_Fake.BeginRepair;
begin
  //comp1.beginrepair;
end;

procedure TVirtualDisk_Fake.DecommissionPayload(sFile: string);
begin
//
end;

function TVirtualDisk_Fake.GetPayloadConfig: PVirtualDiskPayloadConfiguration;
begin
  raise ECRitical.create('not implemented');
end;

procedure TVirtualDisk_Fake.SetDefaultCacheParams(iSegmentSize,
  iSegmentCount: ni; bReadAhead: boolean);
begin
  //
end;

procedure TVirtualDisk_Fake.SetPayloadQuota(iFileID: ni; max_size: int64);
begin
  //
end;

{ Tcmd_VerifyGapsPArt }

procedure Tcmd_VerifyGapsPArt.DoExecute;
var
  t,u: ni;
  vart: PVirtualAddressRecord;
  fp, fpf: PFilePhysical;
begin
  inherited
  Cresult := true;

  StepCount := MAX_BLOCKS_IN_VAT;

  for t:= 0 to MAX_BLOCKS_IN_VAT-1 do begin
    if (t mod divd) <> modd then continue;
    Step := t;
    vart := @selfie.table[t];
    for u := vart.FileCount-1 downto 0 do begin
      //u := self.FileIndex;// to self.fileindex do begin
      //find file physical that is next after this one
      fp := @vart.FPs[u];
      if fp<>nil then begin
        fpf := selfie.FindFilePhysical(fp.FileID, fp.PhysicalAddr);
        if fpf <> nil then begin
          if (fpf.PhysicalAddr - vart.fps[u].PhysicalAddr) < GetBigBlockSizeInBytes(vart.FileCount, true) then begin
            //vart.RemoveFile(vart.FPs[u].FileID);
            Cresult := false;
            error := 'GAP ERROR! in file #'+inttostr(vart.FPs[u].FileID)+' 0x'+inttohex(fpf.PhysicalAddr,0)+'-0x'+inttohex(fpf.PhysicalAddr,0)+'='+inttohex(fpf.PhysicalAddr - vart.fps[u].PhysicalAddr,0)+' minimum is:'+inttohex(GetBigBlockSizeInBytes(vart.FileCount, true),0);
          end;
        end;
      end;
    end;
  end;
end;



{ Tcmd_VirtualDiskRepair }


{ Tcmd_VirtualDiskBringOnline }

destructor Tcmd_VirtualDiskBringOnline.Destroy;
begin
  dcs(exlock);
  inherited;

end;

procedure Tcmd_VirtualDiskBringOnline.DoExecute;
var
  t: ni;
  tm,tm2: ticker;
  vdlast, vdthis: int64;
  bOld: boolean;
begin
  inherited;
  tm2 := 1000;
  consolelog('exeuting @'+inttohex(int64(pointer(self)),2));
  Debug.log(classname+' is starting.');

  vdlast := vd.reads_at_last_scrub xor vd.writes_at_last_scrub;
  vdthis := vdlast;
  StepCount := MAX_BLOCKS_IN_VAT;
  vd.AllowDriveSkipping := false;
  for t:= MAX_BLOCKS_IN_VAT-1 downto 0 do begin
    ecs(exlock);
    try

      if (t mod $10) = 0 then begin
        Step := t;
        Status := 'bringing online @0x'+inttohex(t,0);
        vdlast := vd.reads_at_last_scrub xor vd.writes_at_last_scrub;
      end;

      while not vd.trylock do begin
        sleep(0);
        if IsCancelled then
          exit;
      end;
      try
        tm := tickcount.getticker;
        if vd.BringBigBlockOnline(t) then
        if vd.vat.table[t].FileCount > 0 then begin

          bold := vd.bInReconstitution;
          vd.bInReconstitution := true;
          try
//!
            vd.ReadBlock(t*_BIG_BLOCK_SIZE_IN_BLOCKS, nil);
            vd.ReadBlock((t*_BIG_BLOCK_SIZE_IN_BLOCKS)+(_BIG_BLOCK_SIZE_IN_BLOCKS-1), nil);
          finally
            vd.bInReconstitution := bold;
          end;
        end;
        tm2 := gettimesince(tm);
      finally
        vd.Unlock;
      end;

      if IsCancelled then
        exit;

    finally
      lcs(exlock);
    end;

    vdthis := vd.reads_at_last_scrub xor vd.writes_at_last_scrub;

    if vdthis <> vdlast then begin
        sleepex(greaterof(lesserof(tm2*20,30000), 2000));

      end else begin
        sleepex(tm2 div 10);
      end;

      vdlast := vdthis;


  end;

  vd.AllowDriveSkipping := true;
  Debug.log(classname+' is about to finish.');




end;

procedure Tcmd_VirtualDiskBringOnline.Init;
begin
  inherited;
  ics(exlock);
end;

{ Tcmd_VerifyGapsSinglePart }

procedure Tcmd_VerifyGapsSinglePart.DoExecute;
var
  t: ni;
  vart: PVirtualAddressRecord;
  fp, fpf: PFilePhysical;
begin
  inherited;
  result := true;
  t := block;
  vart := @vat.table[t];
  //for u := 0 to vart.FileCount-1 do begin
    //u := self.FileIndex;// to self.fileindex do begin
    //find file physical that is next after this one
    fp := @vart.FPs[u];
    if fp<>nil then begin
      fpf := vat.FindFilePhysical(fp.FileID, fp.PhysicalAddr);
      if fpf <> nil then begin
        if (fpf.PhysicalAddr - vart.fps[u].PhysicalAddr) < GetBigBlockSizeInBytes(vart.FileCount, true) then begin
          //vart.RemoveFile(vart.FPs[u].FileID);
          result := false;
        end;
      end;
    end;
  //end;
end;

initialization
  orderlyinit.init.RegisterProcs('VirtualDisk_Advanced', oinit, ofinal, 'ManagedThread,CommandProcessor,ApplicationParams,raid');
  hits := 0;


end.

