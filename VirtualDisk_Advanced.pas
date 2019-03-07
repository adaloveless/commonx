unit VirtualDisk_Advanced;

{x$DEFINE DISABLE_3AM_ENFORCEMENT}
{x$DEFINE COMPARE_ENTIRE_VAT_ON_SAVE}
{x$DEFINE NO_SCRUBBER}
{x$DEFINE PAYLOAD_IS_UNBUFFERED}//<!--------------------
{$DEFINE ASYNC_READS}//uses multiple threads to read from each RAID piece simultaneously - if disabled blocks during each RAID piece read
{$DEFINE PAYLOAD_HAS_QUEUED_INTERFACE}
{x$DEFINE DONT_USE_QUEUED}
{x$DEFINE ASYNC_REBUILD}


{x$DEFINE ALLOW_DRIVE_SKIPPING}
{$DEFINE PREFETCH_ON_BRING_ONLINE}
{x$DEFINE RECORD_OPS}
{x$DEFINE PLAY_OPS}
{$DEFINE FIX_OP_CS}

{$IFDEF RECORD_OPS}
  {$DEFINE USE_OPLOG}
{$ENDIF}

{$IFDEF PLAY_OPS}
  {$DEFINE USE_OPLOG}
  {$DEFINE TERMINATE_AFTER_PLAYBACK}
{$ENDIF}
{$IFDEF USE_OPLOG}
  {$DEFINE NO_SCRUBBER}
{$ENDIF}
{x$DEFINE LOCK_GUARANTEED_OPS}



{x$DEFINE ALERT_WRITE}
{x$DEFINE VDREAD_LOG}
{x$DEFINE QUEUE_ITEM_DEBUG}
{x$DEFINE DISABLE_VD_SIDEFETCH}//<<------------
{$DEFINE DEBUG_NEWPHYSICAL}
{$DEFINE ALLOW_SYNCHRONOUS_READS}//<<-----we want this? BUT BUT only once all backup-restores are complete
{x$DEFINE ALLOW_DRIVE_SKIPPING}//ok to enable
{$DEFINE ALWAYS_FORCE_FLUSH_LOCK}
{$DEFINE PAYLOAD_HAS_QUEUED_INTERFACE}
{x$DEFINE ALLOW_SIDEFETCH_REQUEUE}
{x$DEFINE USE_STANDARD_STREAM }
{ x$DEFINE VERIFY_WRITES }
{ x$DEFINE SMALL_TEST }
{x$DEFINE ENABLE_IDLE_QUEUE}
{$DEFINE RELEASE}
{$DEFINE USE_VAT}
{$DEFINE ENABLE_DEEP_CHECKS_IN_BRING_ONLINE}
{$DEFINE READ_BEFORE_WRITE_VD_QUEUE}//<<--- I haven't had any real problems with this lately
{x$DEFINE ALLOW_PRIMARY_READS_TO_BE_SYNCHRONOUS}
{x$DEFINE DONT_PRIORITIZE}//<<-----------------------------
{$DEFINE PAYLOAD_FILE_HEADER}
{ x$INLINE AUTO }
{$DEFINE ALLOW_COLLAPSE}
{$DEFINE BUFFER_BLOCKS}
{$DEFINE ALLOW_RAID}
{$DEFINE AlLOW_COMPARE}
{$DEFINE STATIC_VAT_HINTS}
{x$DEFINE USE_COMPARATIVE }
{x$DEFINE DETAILED_DEBUGGING }

{$DEFINE USE_LINKED_RAID }//x
{$Define USE_VAT_HINTS }//x
{$IFDEF RELEASE}
{$ENDIF}
{$DEFINE USE_PRIORITY_SORT}
{ x$DEFINE MQ }
{$DEFINE SHIFTOPT}
{$DEFINE VD_QUEUE}
{$DEFINE WALK_FLUSH}//x
interface



uses
{$IFDEF MQ}
  globalMultiQueue,
{$ENDIF}
  windows, adminalert, globaltrap,
  linked_list_btree_compatible, system.Types, system.rtlconsts, better_collections, ringstats,lockqueue,
  betterfilestream, gapanal, btree, virtualdiskparams, COMMANDICONS,
  queuestream, raid, stringx, unittest, numbers, tickcount, applicationparams, rtti_helpers,
  standardlist, clusteredpointerlist, helpers.stream, systemx, typex, classes, commands_system,
  sysutils, generics.collections.fixed, betterobject, sharedobject, RDTPARchiveClient,
  MultiBufferMemoryFileStream, debug, orderlyinit, managedthread, simplequeue, fileproxy,
{$IFDEF ALLOW_COMPARE}
  virtualdisk,
{$ENDIF}
  commandprocessor, virtualdiskconstants, arclogshipper, arcmap, zonelog;

const
  CONCURRENT_SOURCE_ARCHIVER_CONNECTIONS = 16;
  SET_CS = 2;
  FILE_COUNT_VAT_FUXORED = -1;
  FILE_COUNT_NO_FETCH_FROM_SOURCE_ARCHIVE = -4;
  DEFAULT_CACHED_STRIPES = 2048*3;
//  DEFAULT_CACHED_STRIPES = 32;
  MAX_DIRTY_BUFFERS = DEFAULT_CACHED_STRIPES shr 2;
//  ALLOWED_PREFETCH_STRIPES = 256;
  BIG_BLOCK_ALIGN_POINT = (BLOCKSIZE * _BIG_BLOCK_SIZE_IN_BLOCKS);
  BIG_BLOCK_BYTE_ALIGN_MASK: int64 = not int64(BIG_BLOCK_ALIGN_POINT - 1);

  PAYLOAD_FLAGS = 0;//FILE_FLAG_WRITE_THROUGH;
  VAT_FLAGS = FILE_FLAG_WRITE_THROUGH;
  MAX_BUFFERED_BLOCKS = RAID_STRIPE_SIZE_IN_BLOCKS;
  // <----- //MUST DIVIDE EVENLY INTO _BIG_BLOCK_SIZE_IN_BLOCKS
  BUFFER_BLOCK_SHIFT = RAID_STRIPE_SIZE_IN_BLOCKS_SHIFT;
  // ------|---change together
  BLOCK_BUFFER_SIZE = MAX_BUFFERED_BLOCKS * BLOCKSIZE;

  //DEFAULT_CACHED_STRIPES = 128;
  MAX_PRIORITY = 255;
  MIN_PRIORITY = 0;
  PLACED_BY_COLLAPSE = $60;//
  PLACED_BY_BUDDYUP = $61;
  PLACED_BY_RAIDUP = $62;
  PLACED_BY_RECONST = $63;
  PLACED_BY_EXPAND = $64;
  PLACED_BY_REBUILD = $65;
  PLACED_BY_INIT = $66;
  PLACED_BY_QUOTA = $67;
  PLACED_BY_TABLEERR0 = $68;
  PLACED_BY_TABLEERR1 = $69;
  PLACED_BY_TABLEERR2 = $6a;
  PLACED_BY_VIRTUAL_TO_PHYSICAL = $6b;


{$IFNDEF SMALL_TEST}
  // BIG_BLOCK_SHIFT = 12;//!<<<<<<<<<<<<<<<-------  CHANGE THESE TWO TOGETHER

{$ELSE}
{$ENDIF}
  INVALID_ADDRESS = $FFFFFFFFFFFFFFFF;

const
CMD_ICON_REBUIlD: TCommandIcon = (BitDepth: 32; RDepth:8;GDepth:8;BDepth:8;ADepth:8;Height:32;Width:32;
 data:
(
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$05000000,$1F000000,$2E000000,$2D000000,$1C000000,$04000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$16000000,$64000000,$AB000000,$F5000000,$FF000000,$FF000000,$FF000000,$FF000000,$F1000000,$A9000000,$62000000,$11000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$15000000,$87000000,$F5000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$EC000000,$7A000000,$11000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$01000000,$51000000,$DF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$E2000000,$4F000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$04000000,$89000000,$FF000000,$FF000000,$FF000000,$FF000000,$F5000000,$A8000000,$65000000,$32000000,$0B000000,$0E000000,$3A000000,$6C000000,$AB000000,$F4000000,$FF000000,$FF000000,$FF000000,$FD000000,$70000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$01000000,$8B000000,$FF000000,$FF000000,$FF000000,$FA000000,$9B000000,$1F000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$24000000,$AA000000,$FF000000,$FF000000,$FF000000,$FF000000,$72000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$56000000,$FF000000,$FF000000,$FF000000,$E9000000,$48000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$59000000,$F0000000,$FF000000,$FF000000,$FC000000,$4B000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$19000000,$E3000000,$FF000000,$FF000000,$E8000000,$38000000,$00000000,$10000000,$4F000000,$4F000000,$4F000000,$4F000000,$4F000000,$4F000000,$4F000000,$4F000000,$4F000000,$4F000000,$02000000,$00000000,$35000000,$E6000000,$FF000000,$FF000000,$E6000000,$1C000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$92000000,$FF000000,$FF000000,$F8000000,$43000000,$00000000,$00000000,$34000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$06000000,$00000000,$00000000,$4D000000,$FF000000,$FF000000,$FF000000,$92000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$1E000000,$FC000000,$FF000000,$FF000000,$93000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$A9000000,$FF000000,$FF000000,$E9000000,$09000000,$00000000,$00000000),
($00000000,$00000000,$71000000,$FF000000,$FF000000,$EF000000,$16000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$76000000,$96000000,$FF000000,$85000000,$85000000,$FF000000,$87000000,$71000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$2E000000,$FE000000,$FB000000,$94000000,$58000000,$00000000,$00000000),
($00000000,$00000000,$C2000000,$FF000000,$FF000000,$9B000000,$00000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$4C000000,$76000000,$FF000000,$60000000,$60000000,$FF000000,$63000000,$46000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$00000000,$A4000000,$60000000,$27000000,$0A000000,$00000000,$00000000),
($00000000,$12000000,$FF000000,$FF000000,$FF000000,$53000000,$00000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$00000000,$05000000,$4B000000,$FB000000,$78000000,$00000000,$00000000),
($00000000,$31000000,$FF000000,$FF000000,$FE000000,$11000000,$00000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$83000000,$A0000000,$FF000000,$8D000000,$8D000000,$FF000000,$92000000,$7E000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$00000000,$4B000000,$FB000000,$FF000000,$FF000000,$77000000,$00000000),
($00000000,$42000000,$FF000000,$FF000000,$F2000000,$00000000,$00000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$40000000,$6D000000,$FF000000,$55000000,$55000000,$FF000000,$58000000,$3A000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$4B000000,$FB000000,$FF000000,$FF000000,$FF000000,$FF000000,$87000000),
($77000000,$FE000000,$FF000000,$FF000000,$FF000000,$EF000000,$52000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$D2000000,$DC000000,$FF000000,$DC000000,$DD000000,$FF000000,$D7000000,$D0000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$01000000,$22000000,$FF000000,$FF000000,$FF000000,$25000000,$01000000),
($00000000,$76000000,$FF000000,$FF000000,$F0000000,$41000000,$00000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$8F000000,$A9000000,$FF000000,$98000000,$98000000,$FF000000,$9D000000,$8B000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$00000000,$36000000,$FF000000,$FF000000,$FD000000,$09000000,$00000000),
($00000000,$00000000,$76000000,$F8000000,$55000000,$07000000,$00000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$40000000,$6D000000,$FF000000,$55000000,$55000000,$FF000000,$58000000,$3A000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$00000000,$5E000000,$FF000000,$FF000000,$E3000000,$00000000,$00000000),
($00000000,$00000000,$0C000000,$2C000000,$4A000000,$8D000000,$00000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$BE000000,$CD000000,$FF000000,$C8000000,$C8000000,$FF000000,$C6000000,$BC000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$00000000,$9C000000,$FF000000,$FF000000,$B5000000,$00000000,$00000000),
($00000000,$00000000,$64000000,$73000000,$F2000000,$E3000000,$0E000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$A7000000,$BC000000,$FF000000,$AF000000,$AF000000,$FF000000,$B3000000,$A4000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$1F000000,$F8000000,$FF000000,$FF000000,$70000000,$00000000,$00000000),
($00000000,$00000000,$26000000,$FD000000,$FF000000,$FF000000,$89000000,$00000000,$00000000,$00000000,$34000000,$FF000000,$40000000,$6D000000,$FF000000,$51000000,$52000000,$FF000000,$58000000,$3A000000,$FF000000,$06000000,$00000000,$00000000,$00000000,$9A000000,$FF000000,$FF000000,$EF000000,$0D000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$9D000000,$FF000000,$FF000000,$F5000000,$36000000,$00000000,$00000000,$34000000,$FF000000,$B2000000,$C4000000,$FF000000,$51000000,$52000000,$FF000000,$BB000000,$AF000000,$FF000000,$06000000,$00000000,$00000000,$41000000,$FF000000,$FF000000,$FF000000,$9B000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$26000000,$F4000000,$FF000000,$FF000000,$DD000000,$1E000000,$00000000,$18000000,$78000000,$78000000,$78000000,$78000000,$26000000,$26000000,$78000000,$78000000,$78000000,$78000000,$03000000,$00000000,$1A000000,$D8000000,$FF000000,$FF000000,$F4000000,$2A000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$6E000000,$FF000000,$FF000000,$FF000000,$D2000000,$30000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$3C000000,$D7000000,$FF000000,$FF000000,$FD000000,$6B000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$01000000,$A0000000,$FF000000,$FF000000,$FF000000,$F8000000,$80000000,$16000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$18000000,$90000000,$FF000000,$FF000000,$FF000000,$FF000000,$86000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$05000000,$A1000000,$FF000000,$FF000000,$FF000000,$FF000000,$F1000000,$8C000000,$49000000,$21000000,$0E000000,$0D000000,$24000000,$4E000000,$8D000000,$EF000000,$FF000000,$FF000000,$FF000000,$FD000000,$88000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$01000000,$78000000,$F6000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$F7000000,$71000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$32000000,$AF000000,$F8000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$FF000000,$F4000000,$A4000000,$30000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$20000000,$81000000,$C6000000,$F6000000,$FF000000,$FF000000,$FF000000,$FF000000,$F3000000,$C5000000,$7F000000,$19000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$04000000,$2F000000,$4B000000,$4A000000,$2A000000,$02000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000)
 ));
  MAX_PAYLOADS_PER_VD = 2048;

type
  TRecordFileStream = TFileStream;
  TPrioritizerStage = (psDowngradeFrom, psUpgradeTo);
  TBringOnlineStatus = (bolOnline, bolBringing, bolStall);
  TSelfCheckResult = (scrOk, scrSomeBad, scrCritical);
  TBufferStatus = (bsNoBuffersToFlush, bsLockWasBusy, bsFlushedSome, bsFlushingTakingTooLong, bsDiskTooActive);
{$IFDEF USE_STANDARD_STREAM}
  TVDStream = TFakeMBFS;
  TVATStream = TFileStream;
{$ELSE}
  TVDStream = TUnbufferedFileStream;
  TVATStream = TUnbufferedFileStream;
{$ENDIF}
  TVirtualDisk_Advanced = class; // forward
  TVirtualDiskPriority = class;//forward
  TResurrector = class;//forward

  TVDQueueITem = class(TQueueItem)
  public
    vd: TVirtualDisk_Advanced;
  end;


  TVDReadCommand = class(TVDQueueItem)
  protected
    procedure DoExecute; override;
  public

    addr: int64;
    count: int64;
    p: pbyte;
    constructor Create;override;
    destructor Destroy;override;
    procedure Init;override;
  end;

  TVDSideFetch = class(TVDQueueItem)
  public
    procedure DoExecute; override;
    procedure Init;override;
  end;

  TVDWriteCommand = class(TVDQueueITem)
  protected
    procedure DoExecute; override;
  public
    addr: int64;
    count: int64;
    p: pbyte;
    constructor Create;override;
    destructor Destroy;override;
    procedure Detach;override;
  end;


  TVDSideFetchThread = class(TManagedThread)
  protected
    procedure DoExecute; override;
  public
    vd: TVirtualDisk_Advanced;
  end;

  TVDQUeue = class(TSimpleQueue)
  protected

    function GetRead: TVDReadCommand;inline;
{$IFDEF READ_BEFORE_WRITE_VD_QUEUE}
    function GetNextItem: TQueueItem; override;
{$ENDIF}
  public
    procedure Init; override;
    procedure InitFromPool; override;

  end;

  TCSArray = array[0..4] of int64;
  TRecordedOp = packed record
  strict private
    function GetCS(idx: ni): int64;
    procedure SetCS(idx: ni; const Value: int64);
  public
    op: byte;
    block: int64;
    blockcount: int64;
    fcs0,fcs1,fcs2,fcs3,fcs4: int64;
    procedure Init;
    function DebugString: string;
    property cs[idx: ni]: int64 read GetCS write SetCS;
  end;

  TVirtualDiskPayloadFileHeader = packed record
    Version: int64;
    _junk_NextPhysicalAddress: int64;
    BufferSegmentSize: int64;
    BufferCount: int64;
   EnableReadAhead: boolean;
    procedure Init;
  end;

  TVirtualDiskBigBlockHeader = packed record
    TotalSizeIncludingHeaders: int64;
    HeaderStart: int64;//8
    PayloadStart: int64;//16
    FooterStart: int64;//32
    VirtualAddress: int64;//40
    CheckSum: int64;//48
    function PayloadSize: int64; inline;
    function EndOfFooter: int64; inline;
    function VAlid(vaddr: int64): boolean;
//    function CheckAddressValidity(payload_size: ni; pVar: PVirtualAddressRecord): boolean;
    function CheckAddressValidity(payload_size: ni; pVar: pointer; fileidx: ni): boolean;
    procedure UpdateCheckSum;
    function ExpectedCheckSum: int64;
    procedure UpdateFooterStart;
    function BeyondAddress:int64;
  end;

  PVirtualDiskBigBlockHeader = ^TVirtualDiskBigBlockHeader;


  TPayloadStream = class({$IFNDEF PAYLOAD_IS_UNBUFFERED}TAdvancedAdaptiveQueuedFileStream{$ELSE}TUnbufferedFileStream{$ENDIF})
  private
    FCollapsable: boolean;
    function GetNextBigBlockAddress(iTotalSizeIncludingHeaders: int64): int64;
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
    function GetBigBlockHeaderFromPhysicalStart(const addr: int64)
      : TVirtualDiskBigBlockHeader;
    function GetBigBlockFooterFromPhysicalStart(const addr: int64; sz: int64)
      : TVirtualDiskBigBlockHeader;
  {$IFNDEF PAYLOAD_IS_UNBUFFERED}
    function GetBigBlockHeaderFromPhysicalStart_Begin(const addr: int64;
      var bbh: TVirtualDiskBigBlockHeader): TObject;
    function GetBigBlockHeaderFromPhysicalStart_End(const o: TObject;
      var bbh: TVirtualDiskBigBlockHeader): TVirtualDiskBigBlockHeader;
    function GetBigBlockFooterFromPhysicalStart_Begin(const addr: int64;
      sz: int64; var bbh: TVirtualDiskBigBlockHEader): TObject;
    function GetBigBlockFooterFromPhysicalStart_End(const o: TObject;
      var bbh: TVirtualDiskBigBlockHeader): TVirtualDiskBigBlockHeader;
  {$ENDIF}
  protected
    procedure SetSize(const Value: Int64); override;

  public
    gaps: TGapAnalyzer;
    stats: TRingStats;
    lastbigblocksync: int64;
    constructor Create(const AFileName: string; Mode: cardinal;
      Rights: cardinal; Flags: cardinal); override;
    destructor Destroy; override;

    function Expand(iSizeBeyondCurrentExcludingHeaders: int64;
      virtual_addr: int64; bConsume: boolean=false): int64;
    procedure CheckInit; inline;
    function IsEmpty: boolean;
    procedure MoveBigBlock(fromAddr, toAddr: int64; sz: int64);
    property Collapsable: boolean read FCollapsable write FCollapsable;
  end;

  TFilePhysical = packed record
    FileID: smallint;
    PhysicalAddr_afterheader: int64;
    procedure InitVirgin;
    function CheckAndCorrect(filecount: ni; ps: TPayLoadStream): boolean;
  end;

  PFilePhysical = ^TFilePhysical;
{$IFDEF USE_VAT}

  TVirtualAddressRecord = packed record
    placedby: byte;
    priority_target: byte;
{$IFNDEF ALLOW_RAID}
    FileID: smallint;
    PhysicalAddress: int64; // if not RAID then FileID is moved outside array
    FileIDs: array [1 .. 19] of TFilePhysical;
    // ^^^^  this section should be the same size/layout as below
{$ELSE}
    // vvvv  this section should be the same size/layout as above
    FPs: array [0 .. 19] of TFilePhysical;
    FileCount: smallint;
{$ENDIF}
    FStartingBlock: int64;
  private
    function GetVirtualAddress: int64;
    procedure SetVatIndex(const Value: int64);
  public
    function GetVatIndex: int64;inline;
    procedure ChangeFileID(ifrom, ito: ni; newphysical: int64);
    procedure InitVirgin(iStartingBlock: int64);
{$IFDEF ALLOW_RAID}
    function IndexOf(iFileID, iPhysical: int64): ni; overload; inline;
    function IndexOf(iFileID: ni): ni; overload; inline;
    function GetFP(iFileID, iPhysical: int64): PFilePhysical; overload; inline;
    function GetFP(iFileID: ni): PFilePhysical; overload; inline;
    function HasFile(iFileID: ni): boolean; inline;
    function HasFileCount(iFileID: ni): ni;inline;
    procedure RemoveFile(idx: ni);
    function InvalidCount: ni;
{$ENDIF}
{$IFDEF ALLOW_RAID}
    class operator Equal(const a, b: TVirtualAddressRecord): boolean;
{$ENDIF}
    procedure SetStartingblock(const Value: int64);
    function DebugString: string;
    function IsAssigned: boolean;
    function HasUndefinedLocations: boolean;
    function SelfCheckOk(myidx: ni; vda: TVirtualDisk_Advanced): TSelfCheckResult;
    property VatIndex: int64 read GetVatIndex write SetVatIndex;
    property StartingBlock: int64 read FStartingBlock write SetStartingblock;
    function IsDead: boolean;
    property VirtualAddress: int64 read GetVirtualAddress;

  end;

  TPayloadFileInformation = packed record
  strict private
    function GetNameAsString: string;
    procedure SetNameAsString(const Value: string);
  private const
    BIT_FLAG_MISSING = 0;

    function GetFlagMissing: boolean;
    procedure SetFlagMissing(const Value: boolean);

  public

    size_limit: int64;
    used_space: int64; // this is only updated when requested from the client
    physical: smallint;
    priority: smallint;
    Flags: cardinal;
    latencyrating: int64;
    name: array [0 .. 2047] of char;

    property NameAsString: string read GetNameAsString write SetNameAsString;
    property Flag_Missing: boolean read GetFlagMissing write SetFlagMissing;

  end;

  PPayloadFileInformation = ^TPayloadFileInformation;

  PVirtualAddressRecord = ^TVirtualAddressRecord;

  Tcmd_BBHFetch = class(TCommand)
  public
    pbbh: PVirtualDiskBigBlockHeader;
    ps: TPayloadStream;
    phys: int64;
    procedure DoExecute; override;
    procedure InitExpense; override;
    procedure Init; override;

  end;
  TVirtualDiskPayloadConfiguration = packed record
  strict private
    Fstatus: array [0 .. 255] of char;
  public
    filelist: array [0 .. MAX_PAYLOADS_PER_VD-1] of TPayloadFileInformation;
    // <<<!!!!----- -MUsT BE LAST
    procedure AddPayload(sFile: string; max_size: int64; iPhysical: ni;
      iPriority: ni; iFlags: int64);
    function FindUnusedSlot: nativeint;
    function GetMarhshalSize: nativeint;

  end;

  PVirtualDiskPayloadConfiguration = ^TVirtualDiskPayloadConfiguration;

  TVatStat = packed record
    Callups: int64;
    Writes:int64;
    procedure Init;
    procedure Discount;
  end;

  PVatStat = ^TVatStat;

  TVatStatEx = packed record
    stat: PVatStat;
    rec: PVirtualAddressRecord;
    startingblock: int64;
  end;

  TVatStats = packed record
    version: int64;
    big_blocks: array[0..MAX_BLOCKS_IN_VAT-1] of TVatStat;
    procedure Init;
    procedure Discount;
  end;

  TVirtualAddressTable = packed record
    // Version: cardinal;
    Version: int64;
    OpenTime: int64;
    CloseConfirm: int64;
    PayloadConfig: TVirtualDiskPayloadConfiguration;
    reserved: cardinal;
    Marker: int64;
    DefaultBufferSegmentCount: int64;
    DefaultBufferSegmentSize: int64;
    DefaultBufferReadAhead: boolean;
    MaxDiskSpan: byte;//0 = unlimited
    MinDiskSpan: byte;
    junk2: byte;
    junk3: byte;
    table: array [0 .. MAX_BLOCKS_IN_VAT - 1] of TVirtualAddressRecord;
    XXSourceArchive: array[0..2048] of widechar;
    XXSourceArchivePinID: int64;
    TargetArchive: array[0..2048] of widechar;
    end_of_persistence: ni;
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
    function GetTableEntryPhysicalAddress(iFileID: nativeint; physical: int64)
      : PVirtualAddressRecord;
    function FindVARToRAIDUp: PVirtualAddressRecord;
  public
    property PayloadCount: cardinal read GetFileCount;
    function DebugVatSTructure: string;
    procedure MarkTableEntryDirty(idx: ni);
    procedure MarkTableEntryDirtyByPtr(ptr: PVirtualAddressRecord);
    procedure MarkDirtyRange(const iStart, iEnd: int64);
    procedure MarkDirtyByLength(const iStart, iLEngth: ni);overload;
    procedure MarkDirtyByLength(ptr: pointer; iLEngth: ni);overload;
    function PersistedSize: ni;
    procedure SetOpenMarker;
    procedure SetCloseMarker;
    function LocalAddr(p: pointer): int64;
    function FindVARToBuddyUp: PVirtualAddressRecord;
    function FindVARToReconstitute: PVirtualAddressRecord;
    function FindVarWithHighestPhysicalForPayload(iPayloadID: ni)
      : PVirtualAddressRecord;
    function FindFilePhysical(iDISKID: ni; greaterthan: int64): PFilePhysical;
    function VerifyGaps(bThrowExceptions: boolean): boolean;
    function VerifyGapSingle(idx: int64; out violations: array of int64): boolean;
    function GetSafePhysicalAddr(iFileID: ni): int64;
    function HasFile(iPayloadID: ni): boolean;
    procedure ChangeAllFiles(iFromID: ni; iToID: ni);
    function CountPopulatedTableEntries: ni;
    function FindGapLocation_Deprecated(sz: fi; FileID: fi; out physical: int64;
      not_this_physical: int64): boolean;
    function FindNextBlockForFileIDInVat_Deprecated(FileID: fi; beyond: int64)
      : PVirtualAddressRecord;
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
    procedure DoExecute; override;
  end;

  Tcmd_VerifyGapsPArt = class(TCommand)
  private
    Fselfie: PVirtualAddressTable;
    FFileIndex: ni;
    Fmodd: ni;
    Fdivd: ni;
  public
    error: string;
    procedure DoExecute; override;
    property selfie: PVirtualAddressTable read Fselfie write Fselfie;
    // property FileIndex: ni read FFileIndex write FFileIndex;
    property modd: ni read Fmodd write Fmodd;
    property divd: ni read Fdivd write Fdivd;
  end;

{$ENDIF}
{$IFDEF BUFFER_BLOCKS}

  TVDBlockBuffer = record
  public
    StartingBlock: int64;
    ReadFromDisk: boolean;
    data: array [0 .. BLOCK_BUFFER_SIZE - 1] of byte;
    dirty: array [0 .. MAX_BUFFERED_BLOCKS - 1] of boolean;
    function FirstDirty(iAfter: ni): ni;
    function FirstClean(iAfter: ni): ni;
    function CoversBlock(lba: int64): boolean;
    function AnyDirty: boolean;
    procedure Init;
  end;
{$ENDIF}

  TAbstractVirtualDisk_Advanced = class(TFakeLockQueue)
  strict private
    function GetIdentifier: string;
    procedure SetIdentifier(const Value: string);
  strict private
    FEnableOptionalDebugging: boolean;
  protected
    Fstatus: string;
    function TailSize: int64;
  strict protected
    FopStatus: string;
  private
    function GetStatus: string;
    procedure SetStatus(Value: string);
  protected
    FSize: int64;

{$IFDEF USE_VAT_HINTS}
    {$IFDEF STATIC_VAT_HINTS}
    mem_vathints: PByte;
    {$ELSE}
    vathints: array of boolean;
    {$ENDIF}

{$ENDIF}
    FRequestedSize: int64;
    FIdentifier: string;
    procedure SetSize(const Value: int64); virtual;
    function BlockMargin: int64;
{$IFDEF use_vat}
    // procedure LoadVat;virtual;abstract;
    // procedure SaveVat;virtual;abstract;
    // procedure SaveVatAndConfig(sTo: string = '');virtual;abstract;
{$ENDIF}
    // function NewPhysicalBlockAddress(virtual_addr: int64): TVirtualAddressRecord;virtual;abstract;
  strict protected
    procedure ReadBlock(const lba: int64; const p: pbyte); virtual; abstract;
    function ReadBlocks(const lba: int64; const cnt: nativeint; p: pbyte)
      : nativeint; virtual; abstract;
    function WriteBlocks(const lba: int64; const cnt: nativeint; p: pbyte; bDontArchive: boolean = false)
      : nativeint; virtual; abstract;
    procedure WriteBlock(const lba: int64; const p: pbyte); virtual; abstract;
  public
    ActiveUseTime: ticker;
    hint_requests_waiting: boolean;
    vat: TVirtualAddressTable;
    shipper: TArcLogShipper;
    zonerevs: TArcMap;
    disableArcRestore: boolean;
    backgroundlock: TLockQueuedObject;
    constructor Create; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    property Size: int64 read FSize write SetSize;
    function BlockCount: int64;
    function TailStartAddr: int64;


    procedure GuaranteeReadBlocks(lba: int64; cnt: nativeint; p: pbyte);
    procedure GuaranteeWriteBlocks(lba: int64; cnt: nativeint; p: pbyte; bDontArchive: boolean = false);

    procedure GrowIfNeededBlock(lba: int64); virtual; abstract;
    procedure GrowIfNeededAddr(Size: int64); virtual; abstract;
    procedure FlexWrite(const addr: int64; const cnt: nativeint; p: pbyte);
    procedure FlexRead(const addr: int64; const cnt: nativeint; p: pbyte);
    procedure WriteData(const addr: int64; const cnt: nativeint; p: pbyte);
      virtual; abstract;
    procedure ReadData(const addr: int64; const cnt: nativeint; p: pbyte);
      virtual; abstract;
    function ReadData_Begin(const addr: int64; const cnt: nativeint;
      p: pbyte): TVDReadCommand;virtual;abstract;
    procedure ReadData_End(o: TVDReadCommand);virtual;abstract;
    procedure ChangeSingleByte(iAddr: int64; b: byte); virtual; abstract;
    function ReadSingleByte(iAddr: int64): byte; virtual; abstract;
    procedure Preallocate(lba: int64); virtual; abstract;
    property Identifier: string read GetIdentifier write SetIdentifier;
    property EnableOptionalDebugging: boolean read FEnableOptionalDebugging
      write FEnableOptionalDebugging;
    procedure optdebug(s: string);
    property OperationalStatus: string read FopStatus write FopStatus;
    function GEtVerboseDebug(iBigBlock: ni): string; overload;
    function GEtVerboseDebug(sToFile: string): boolean; overload;
    procedure PreShutdown;
    property Status: string read GetStatus write SetStatus;
    function VerifyAgainstArchive(zone: integer; out csa,csb, difstart: int64): boolean;virtual;abstract;
  end;


  TFileBasedVirtualdisk = class(TAbstractVirtualDisk_Advanced)
  protected
    function GetfileName: string; virtual; abstract;
    procedure SetFileNAme(const Value: string); virtual; abstract;
  public
    property FileName: string read GetfileName write SetFileNAme;
  end;

  TFileBlock = class(TBetterOBject, IIndexable<int64>)
  protected
    function GetIndexValue: int64;
  public
    data: pointer;
    Size: nativeint;
    addr: int64;
    dirty: boolean;

    procedure Read(FStream: TStream);
    procedure Write(FStream: TStream);
  end;

  TBLockListList = TStandardList<int64, TFileBlock>;

  TBlockLevelCacheList = class(TBetterOBject)
  protected
    FList: TBLockListList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddBlock(b: TFileBlock);
    function IndexOfBlock(addr: int64): nativeint;
    function HasBlock(addr: int64): boolean;
  end;

  TPayloadPrioritySort = record
    str: TPayloadStream;
    index: ni;
  end;


  Tcmd_SourceFetch = class(TCommand)
  public
    arc: TRDTPArchiveClient;
    zidx: int64;
    pin: TDateTime;
    archivename: string;
    pointer: TDynByteArray;
    procedure Init;override;
    procedure InitExpense;override;
    procedure DoExecute;override;
  end;

  TResurrector = class(TManagedthread)
  private
    function GetArchiveName: string;
    function GetPin: TDateTime;
    function GetArchiveHost: string;
    function GetArchivePort: string;
    function AllZeros: boolean;
  protected
    repeatscan: boolean;
    idx: int64;
    seekopt: int64;
    forwardcount: int64;
    scratch: array[0..BIG_BLOCK_SIZE_IN_BYTES-1] of byte;
    cli: TRDTPArchiveClient;
    tablecopy: array [0 .. MAX_BLOCKS_IN_VAT - 1] of TVirtualAddressRecord;
    property Pin: TDateTime read GetPin;
    property ArchiveHost: string read GetArchiveHost;
    property ArchiveEndpoint: string read GetArchivePort;
    property ArchiveName: string read GetArchiveName;
    function Rebuild: boolean;
    procedure CopysourceTable;
    procedure Prep;
    procedure Give;
    procedure DoExecute;override;
    procedure OnFinish;override;
    procedure Connect;
    procedure Disconnect(bHard: boolean = false);
    procedure FastForward;
    procedure Reset;
  public
    disk: TVirtualDisk_Advanced;
    finished: boolean;
    procedure InitFromPool;override;
    procedure Detach;override;
  end;

  Tcmd_RebuildOperation = class(TCommand)
  private
    FsourceFetchCommands: array[0..ARC_ZONES_PER_BIG_BLOCK-1] of Tcmd_SourceFetch;
    function GetStartAddr: int64;
  protected
    procedure DoExecute; override;
  public
    d: TVirtualDisk_Advanced;
    fromtarget: boolean;
    bbid: int64;
    completed: int64;
    total: int64;
    data: array[0..BIG_BLOCK_SIZE_IN_BYTES-1] of byte;
    procedure InitExpense; override;
    property StartAddr: int64 read GetStartAddr;
    procedure SourceRebuild;
    function TargetRebuild: boolean;


  end;


  TVirtualDisk_Advanced = class(TFileBasedVirtualdisk)
  private
    sfthread: TVDSideFetchThread;
    vatlastbackedup: ticker;
    FCacheSize: int64;
    scrubber: TExternalEVentThread;
    prioritizer: TVirtualDiskPriority;
    scrubber_file_index: ni;
    bRepaired: boolean;
    FMigrating: boolean;
    FOperational: boolean;
    reconstituting: boolean;
    bInReconstitution: boolean;
    lastconstitutionCheck: ticker;
    vatstats: TVatStats;
    writes_at_last_scrub, reads_at_last_scrub: int64;
{$IFDEF BUFFER_BLOCKS}
{$ENDIF}
    online_bigblocks: array [0 .. MAX_BLOCKS_IN_VAT - 1] of boolean;
    last_critical_work_time: ticker;
    last_noncritical_work_time: ticker;
    non_critical_work_interval: ticker;
    FBringOnlineIdx: int64;
    FSourceArchivers: array[0..CONCURRENT_SOURCE_ARCHIVER_CONNECTIONS-1] of TRDTPARchiveClient;
    FsourceFetchCommands: array[0..ARC_ZONES_PER_BIG_BLOCK-1] of Tcmd_SourceFetch;
    daSourcePrefetchResults: array[0..ARC_ZONES_PER_BIG_BLOCK-1] of TDynByteArray;
    tmLAstBringOnline: ticker;
    FLastScrubberExecuteTime: ticker;
    RecheckDiskOnCompletion: boolean;
    bGapsChecked: boolean;
    FPayloadsOK: boolean;
    FCAchedSTripes: ni;
    FAllowDriveSkipping: boolean;
    rc: TRaidCalculator;
    last_buffer_count: ni;
    last_flush_status: TBufferStatus;
    WorkingFromSourceArchive: boolean;
    WorkingFromTargetArchive: boolean;
    tmLAstShovel: ticker;

    resurrector: TResurrector;

    FprioritySort: array of TPayloadPrioritySort;
    function BringBigBlockOnline(idx: ni): boolean;
    function DeepCheck(idx: ni): boolean;
    function BringBigBlockOffline(idx: ni): boolean;

    procedure SetCAcheSize(const Value: int64);
    procedure ScrubberExecute(thr: TExternalEVentThread);
    function PercentPayloadBuffersFlushed: single;
    function ContinueBringOnline(thr: TExternalEventThread): TBringOnlineStatus;
    procedure EvalRunHot(thr: TExternalEVentThread);
    procedure BackupVat;
    function FlushBuffers(bForceLock: boolean; iOlderThan: ticker; bFindAllDirty: boolean = false;
      bOnlyIfALLDirty: boolean = false; bOnlyIfDiskInactive: boolean = true): TBufferStatus;
    function BuddyUp: boolean;
    function RaidUP: boolean;
    function Reconstitute(pvar: PVirtualAddressRecord = nil): boolean;
    procedure ZoneLog(idx: int64; sMessage: string);
    function ForceReconstitution(pvar: PVirtualAddressRecord): boolean;
    function FindHighestOverVar(FileID: ni; out bOver: boolean): PVirtualAddressRecord;
    function Collapse(FileID: ni): boolean;
    function FindHighestOverVar_AfterCollapse(FileID: ni)
      : PVirtualAddressRecord;

    procedure RepairbigBlock(varr: PVirtualAddressRecord; ps: TPayloadStream;
      pbbh: TVirtualDiskBigBlockHeader);
    procedure Repair_payload(ps: TPayloadStream; id: ni);
    function GEtOverQuotaStream(out id: nativeint): TPayloadStream;
    function GEtUnderQuotaStream(notOf: PVirtualAddressRecord;
      out id: nativeint; bOnlyReadyStreams: boolean; iAllowPhysical: ni = -1)
      : TPayloadStream; overload;
    function PhysicalIsInVar(vart: PVirtualAddressRecord;
      iPhysical: ni): boolean;
    function GEtUnderQuotaStreamxx(notOf: PVirtualAddressRecord;
      out id: nativeint): TPayloadStream; overload;
    procedure MovePayloadBigBlock(pvar: PVirtualAddressRecord;
      FromID, toID: nativeint; fromstream, tostream: TPayloadStream;
      bMirror: boolean; placed_by_reason: byte);
    procedure CheckVatSanity;
    procedure SortPayloadPriorities;

    function HasRaid(StartingBlock: int64): boolean; //inline;
    // function IndexOfRaid(startingBlock: int64): ni;inline;
    function NeedRaid(StartingBlock: int64): TRaidAssembledBuffer;

    function TryFetchRaid(r: TRaidAssembledBuffer): TRaidFetchStatus;
    function FixFetchRaid(r: TRaidAssembledBuffer): TRaidFetchStatus;inline;

    procedure FetchAndMergeRaid(r: TRaidAssembledBuffer);
    procedure FlushRaid(r: TRaidAssembledBuffer);
    function IsStreamUnderQuota(t: nativeint): boolean;
    function VirtualToPhysicalAddr(addr: int64): TVirtualAddressRecord;
    function GetBusiestPhysical(const vart: PVirtualAddressRecord): ni;
    procedure Shovel(thr: TExternalEVentThread; bShrinkOnly: boolean);
    procedure ShrinkPayloads;
    function GEtCachedSTripes: ni;
    procedure SetCachedSTripes(const Value: ni);
    function GetLeastBusyPhysical(): ni;
    function DefragMentPayload(iFileID: fi): boolean;
    function FlushBuffersInBlockOrder(iStartingBlock: int64; bOnlyIfAllDirty: boolean; bOnlyIfDiskInactive: boolean): TBufferStatus;
    function StreamHasGaps(t: nativeint; out physaddr: int64): boolean;
    function GEtSourceARchive(idx: ni): TRDTPArchiveClient;
    procedure SyncRebuildOperation(bbid: int64);
    procedure StartRebuildOperation(bbid: int64; fromtarget: boolean);
    procedure CompleteRebuildOperation(bWait: boolean);

    function WaitForRebuildLBARegion(lba, bytelength: int64) : boolean;

    procedure REbuildBigBlockFromSourceArchive(bbid: int64);
    function REbuildBigBlockFromTargetArchive(bbid: int64): boolean;
    function MaxPercentPayloadBuffersUnFlushed: single;
  protected
    currentRebuildOperation: Tcmd_RebuildOperation;
    procedure ReadBlock(const lba: int64; const p: pbyte); override;
    procedure WriteBlock(const lba: int64; const p: pbyte); override;

    procedure SetSize(const Value: int64); override;
    function GetfileName: string; override;
    procedure SetFileNAme(const Value: string); override;
{$IFDEF use_vat}
    procedure LoadVat; // override;
    procedure QuickCheckVat;
    procedure SaveVat(bAll: boolean); // override;
    procedure SaveVatAndConfig(sTo: string = ''); // override;
    function NewPhysicalBlockAddress(virtual_addr: int64; iMinPriority, iMaxPriority: ni; placed_by_reason: byte)
      : TVirtualAddressRecord;
{$ENDIF}
    procedure RecordVarInGapTrees(const vart: TVirtualAddressRecord);
    procedure RemoveVatFromGapTrees(const vart: TVirtualAddressRecord);
  protected
    queue: TVDQueue;
    function ReadBlocks(const lba: int64; const cnt: nativeint; p: pbyte)
      : nativeint; override;
    function WriteBlocks(const lba: int64; const cnt: nativeint; p: pbyte; bDontArchive: boolean = false)
      : nativeint; override;
  public
    online: boolean;
    enablelegittagging: boolean;
    rsPrefetch: TRingstats;
    rs0,rs1,rs2,rs3,rs4,rs5,rs6,rs7: TRingSTats;
    csScrubber: TCLXCRiticalSection;
    SourceArchiveHost, SourceARchiveEndpoint: string;
    SourceArchive: string;
    SourceArchivePinID: TDateTime;


    writes: int64;
    reads: int64;
    primary_buffer: TRaidAssembledBuffer;
    lastpayloadchange: ticker;
    bRaidFetched: boolean;
    prefetch_payload_idx: ni;
    Repairlog: TStringlist;
    // FRaids: TRaidList;
    FRaidsByBlock: TRaidTree;
    FRaidsByDirtyTime: TRaidLinkedList<TRaidTreeItem_byDirtyTime>;
    FRaidsByLAstUsed: TRaidLinkedList<TRaidTreeItem_byLastUsedTime>;
    FVATStream: TVatStream;
    FPayloadStreams: TBetterList<TPayloadStream>;
    shuttingdown: boolean;
    back_allowed_prefetches: ni;
    front_allowed_prefetches: ni;
    back_prefetchposition: int64;
    front_prefetchposition:int64;
    front_prefetchwritemode: boolean;
    front_RecommendedPrefetchAllowance: ni;
    stall_time: ticker;
    tmLastSideFetch: ticker;
    disablecallupstats: boolean;
{$IFDEF USE_OPLOG}
    oplog: TRecordFileStream;
{$ENDIF}
    // FCurrentVat: PVirtualAddressRecord;
    HighestAssignedPayload: ni;
    procedure SanityCheckPayloads;
    procedure Detach; override;
    procedure StopShipper;
    property AllowDriveSkipping: boolean read FAllowDriveSkipping
      write FAllowDriveSkipping;
    constructor Create; override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function VerifyAgainstArchive(zone: integer; out csa,csb, difstart: int64): boolean;override;

    procedure StartScrubber;
    procedure StopScrubber;
    procedure LoadPayloadStreams;
    procedure updatePayloadStreams;
    procedure StopBringOnline;
    procedure StartBringOnline;
    procedure RecheckDisk;
    procedure SyncAwayAllBuffers;
    procedure DestroyPayloadStreams;
    property PayloadsOk: boolean read FPayloadsOK;
    property Operational: boolean read FOperational;

    property Migrating: boolean read FMigrating write FMigrating;
    procedure LogRepair(idx: int64; s: string);
    function GetRepairLog: string;
    function DrainRepairLog: string;
    procedure ClearRepairLog;
    procedure REstartBringOnline;

    function ReadBlocks_Direct_Single(const lba: int64; const cnt: nativeint;
      p: pbyte): nativeint;
    function WriteBlocks_Direct_Single(const lba: int64; const cnt: nativeint;
      p: pbyte): nativeint;

    function ReadBlocks_RaidCalc(const lba: int64; const cnt: nativeint;
      p: pbyte): nativeint; inline;
    function WriteBlocks_RaidCalc(const rab: TRaidAssembledBuffer;
      const lba: int64; const cnt: nativeint; p: pbyte): nativeint; inline;

    function ReadBlocks_Direct(const lba: int64; const cnt: nativeint; p: pbyte)
      : nativeint;
    function WriteBlocks_Direct(const lba: int64; const cnt: nativeint;
      p: pbyte): nativeint;

{$IFDEF BUFFER_BLOCKS}
    function ReadBlocks_Buffered(const lba: int64; const cnt: nativeint;
      pCanBeNil: pbyte): nativeint;
    function WriteBlocks_Buffered(const lba: int64; const cnt: nativeint;
      p: pbyte; bDontArchive: boolean = false): nativeint;
{$ENDIF}
{$IFDEF BUFFER_BLOCKS}
    function SyncAwayBuffer(buf: TRaidAssembledBuffer;
      bOnlyIfALLDirty: boolean = false): boolean;
    procedure SyncBuffer(lba: int64; bForWriting: boolean);
{$ENDIF}
    procedure GuaranteeReadBlocks_Direct(lba: int64; cnt: nativeint; p: pbyte);
    procedure GuaranteeWriteBlocks_Direct(lba: int64; cnt: nativeint; p: pbyte);


    procedure WriteData(const addr: int64; const cnt: nativeint;
      p: pbyte); override;
    procedure ReadData(const addr: int64; const cnt: nativeint;
      p: pbyte); override;
    function ReadData_Begin(const addr: int64; const cnt: nativeint;
      p: pbyte): TVDReadCommand;override;
    procedure ReadData_End(o: TVDReadCommand);override;


    procedure SyncWriteData(const addr: int64; const cnt: nativeint;
      p: pbyte);inline;
    procedure SyncReadData(const addr: int64; const cnt: nativeint;
      p: pbyte);inline;

    procedure GrowIfNeededBlock(lba: int64); override;
    procedure GrowIfNeededAddr(Size: int64); override;
    procedure ChangeSingleByte(iAddr: int64; b: byte); override;
    function ReadSingleByte(iAddr: int64): byte; override;
    procedure Preallocate(lba: int64); override;
    property CacheSize: int64 read FCacheSize write SetCAcheSize;
    function DebugVatSTructure: string;
    procedure AddPayload(sFile: string; size_limit: int64; iPhysical: ni;
      iPriority: ni; iFlags: int64);
    function GetPayloadConfig: PVirtualDiskPayloadConfiguration;
    procedure SetPayloadQuota(iFileID: ni; max_size: int64);
    procedure SetPayloadPhysical(iFileID: ni; physical: int64);
    procedure SetPayloadPriority(iFileID: ni; priority: int64);
    procedure SetDefaultCacheParams(iSegmentSize, iSegmentCount: ni;
      bReadAhead: boolean);
    procedure DecommissionPayload(sFile: string);
    procedure UnpauseScrubber;
    procedure ReFunctPayload(iPayloadID: ni; sNewSource: string);
    procedure ReSourcePayload(iPayloadID: ni; sNewSource: string);
    procedure BeginRepair;
    property CachedSTripes: ni read GEtCachedSTripes write SetCachedSTripes;
    procedure QuickOnline;
    procedure AnalyzeGaps;
    procedure SelfTestOld;
    procedure SelfTest(testid: ni);
    procedure LoadVatStats;
    procedure SaveVatStats;
    property SourceArchivers[idx: ni]: TRDTPArchiveClient read GetSourceArchive;
    function VerifyArcZone(zoneidx: ni; bUseSourceArchive: boolean = false): boolean;
    function RepairArcZone(zoneidx: ni): boolean;
    procedure LoadFromConfiguration(ap: TAppParams; sPrefix: string);
    procedure ResetRepair;
    function IsOnline: boolean;
    procedure vdqueue_onidle(sender: Tobject);
    procedure VD_SmartSideFetch;
    procedure TryHintNExtBigBlock(fileid: ni; bb: int64);overload;
    procedure TryHintNExtBigBlock(bb: int64; notin: PVirtualAddressRecord);overload;
    function PRefetchPayload(const fileid: ni; const  startsearch_byte: int64; const maxsearch_bblocks: int64; const ubs: TUnbufferedFileStream; bWRiteMode: boolean): boolean;
    procedure PrefetchAllPayloads(pos: int64; bStopOnHit: boolean; bWRiteMode: boolean);
    procedure DumpCacheStatus(sl: TStringlist);
    procedure Front_CancelPrefetches;
    procedure Front_StartPrefetches;
    procedure Front_CalculateRecommendedPrefetches;
    procedure ResetZone(zid: int64);
    function BigBlocksInDisk: int64;
    function QueueFull: boolean;
    function WaitForQueueSpace(itimeout: ni = -1): boolean;
    procedure ReplayLog;
    procedure DumpBigBlock(bbid: int64);
  end;

  TVirtualDisk_Comparative = class(TFileBasedVirtualdisk)
  private
    FCacheSize: int64;
    FJunk: int64;
    function GetOperational: boolean;
  public
    comp1: TVirtualDisk_Advanced;
    comp2: virtualdisk.TVirtualDisk_SimpleNonLinear;
    constructor Create; override;
    destructor Destroy; override;

    procedure ReadBlock(const lba: int64; const p: pbyte); override;
    procedure WriteBlock(const lba: int64; const p: pbyte); override;

    procedure SetSize(const Value: int64); override;
    function GetfileName: string; override;
    procedure SetFileNAme(const Value: string); override;
    function ReadBlocks(const lba: int64; const cnt: nativeint; p: pbyte)
      : nativeint; override;
    function WriteBlocks(const lba: int64; const cnt: nativeint; p: pbyte; bDontArchive: boolean = false)
      : nativeint; override;
    procedure WriteData(const addr: int64; const cnt: nativeint;
      p: pbyte); override;
    procedure ReadData(const addr: int64; const cnt: nativeint;
      p: pbyte); override;

    property CacheSize: int64 read FCacheSize write FCacheSize;

    procedure AddPayload(sFile: string; size_limit: int64; iPhysical: ni;
      iPriority: ni; iFlags: int64);
    procedure DecommissionPayload(sFile: string);
    function GetPayloadConfig: PVirtualDiskPayloadConfiguration;
    procedure SetDefaultCacheParams(iSegmentSize, iSegmentCount: ni;
      bReadAhead: boolean);
    procedure SetPayloadQuota(iFileID: ni; max_size: int64);
    property Operational: boolean read GetOperational;
    procedure ClearRepairLog;
    function GetRepairLog: string;
    function DrainRepairLog: string;
    procedure BeginRepair;
    function DebugVatSTructure: string;
    procedure ReFunctPayload(id: ni; sFile: string);
    procedure ReSourcePayload(id: ni; sFile: string);
    procedure SetPayloadPhysical(id: ni; phys: ni);
    procedure SetPayloadPriority(id: ni; pri: ni);
    procedure UnpauseScrubber;
    property CachedSTripes: int64 read FJunk write FJunk;
    procedure QuickOnline;
    procedure ResetRepair;
  end;

  TVirtualDisk_Fake = class(TVirtualDisk_SimpleNonLinear)
  public
    procedure AddPayload(sFile: string; size_limit: int64; iPhysical: ni;
      iPriority: ni; iFlags: int64);
    procedure DecommissionPayload(sFile: string);
    function GetPayloadConfig: PVirtualDiskPayloadConfiguration;
    procedure SetDefaultCacheParams(iSegmentSize, iSegmentCount: ni;
      bReadAhead: boolean);
    procedure SetPayloadQuota(iFileID: ni; max_size: int64);
    procedure BeginRepair;
    procedure ResetRepair;
  end;

{$IFDEF USE_COMPARATIVE}

  TVirtualDisk = class(TVirtualDisk_Comparative);
{$ELSE}
  TVirtualDisk = class(TVirtualDisk_Advanced);
{$ENDIF}
  TVirtualDiskList = class(TBetterList<TVirtualDisk>);

  TVirtualDiskHub = class(TSharedObject)
  public
    vdlist: TVirtualDiskList;
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadConfiguration;
    procedure AddDiskFromConfiguration(ap: TAppParams; t: ni);
    procedure NewDisk(di: TNewDiskParams);
    procedure PreShutdown;
  end;

  TVirtualDiskPriority = class(TManagedThread)
  type
    TRepResult = (repNothingToDo, repFailed, repSuccess);
  private
    nextindex: int64;
    workingpriority: ni;
    prepped: boolean;
    lastpreptime: ticker;
    enable: boolean;
    skip_priority: ni;
    stage: TPrioritizerStage;
    maxpriorityassigned: ni;
    function RunTierStorageSimulation(iPriorityTier: ni): int64;
    procedure CopyVat;
    function SortPass: boolean;
    procedure SortTableCopy;
    procedure Swap(var a,b: TVatSTatEx);
    procedure AssignPriority(iPriority, iCount: int64);
    procedure Reset;
    function SortPassBubble: boolean;
    function SortPassRandom: boolean;
    function SortPassButterFly(step: ni): boolean;
  protected
    procedure DoExecute; override;
  public
    vd: TVirtualDisk_Advanced;
    tablecopy: array[0..MAX_BLOCKS_IN_VAT-1] of TVatStatEx;
    payloadpriorities: array[0..2047] of ni;
    procedure Prep;
    function ContinueReprioritize: TRepResult;
  end;

  Tcmd_ReplayLog = class(TCommand)
  public
    vda: TVirtualdisk_Advanced;
    procedure DoExecute; override;

  end;

procedure CreateVirtualDiskHub;
procedure DestroyVirtualDiskHub;

function IsWithin(iStart, iSpan: int64; iTest: int64): boolean;

function GetBigBlockSizeInBytes(iRAidCount: ni;
  bIncludingHeaders: boolean = false): ni; inline;

function BigBlockAlign(virt: int64): int64; inline;
function RaidStripeAlign(virt: int64): int64; inline;


function ValidPayloadID(id: ni): boolean;
function ValidStripeFileID(id: ni): boolean;


var
  g_FORCE_VD_PREFETCH: ni = 501;
  gQuickFix: boolean;
  gQuickFixNeedsSave: boolean;
  vdh: TVirtualDiskHub;

implementation

{$IFDEF TERMINATE_AFTER_PLAYBACK}
uses
  forms, uMainGUI, messages;
{$ENDIF}

{ TVirtualDisk }
function ValidPayloadID(id: ni): boolean;
begin
  result := (id < MAX_PAYLOADS_PER_VD) and (id >=0);

end;

function ValidStripeFileID(id: ni): boolean;
begin
  result := (id <= 16) and (id >=0);
end;//ok


function IsWithin(iStart, iSpan: int64; iTest: int64): boolean;
begin
  if iTest < iStart then
  begin
    result := false;
    exit;
  end;

  if iTest >= iStart + iSpan then
  begin
    result := false;
    exit;
  end;
  result := true;
end;//ok

function BigBlockAlign(virt: int64): int64; inline;
begin
{$IFDEF SHIFTOPT}
  result := virt and BIG_BLOCK_BYTE_ALIGN_MASK;
{$ELSE}
  result := (virt div ALIGNER) * (ALIGNER);
{$ENDIF}
end;//ok

function RaidStripeAlign(virt: int64): int64; inline;
begin
{$IFDEF SHIFTOPT}
  result := virt and RAID_ALIGN_BYTE;
{$ELSE}
  result := (virt shr STRIPE_SHIFT) shl STRIPE_SHIFT;
{$ENDIF}
end;//ok

function GetBigBlockSizeInBytes(iRAidCount: ni;
  bIncludingHeaders: boolean = false): ni; inline;
const
  LOCALDIV = (_BIG_BLOCK_SIZE_IN_BLOCKS div RAID_STRIPE_SIZE_IN_BLOCKS);
begin
  if iRaidCount < 0 then begin
    raise ECritical.create('Raid count cannot be '+inttostr(iRaidCount));
  end;
  result := TRaidCalculator.GetPieceSizePAdded(iRAidCount) * LOCALDIV;
  if bIncludingHeaders then
    result := result + (2 * sizeof(TVirtualDiskBigBlockHeader));
end;//ok

procedure TVirtualDisk_Advanced.AddPayload(sFile: string; size_limit: int64;
  iPhysical: ni; iPriority: ni; iFlags: int64);
var
  l: TLock;
begin
  StopScrubber;
  try
    l := getLock;
    try
      if size_limit < 0 then
        size_limit := -1;
      lastpayloadchange := getticker;
      vat.PayloadConfig.AddPayload(sFile, size_limit, iPhysical,
        iPriority, iFlags);
      SaveVatAndConfig;
      //FVATStream.FinalizeBuffers;
      LoadPayloadStreams;

    finally
      UnlockLock(l);
    end;
  finally
    StartScrubber;
  end;
end;//ok

function TVirtualDisk_Advanced.PercentPayloadBuffersFlushed: single;
var
  t: ni;
  ps: TPayloadStream;
  tot: single;
begin
  Lock;
  try
    tot := 0;
    result := 0;
    for t:= 0 to self.FPayloadStreams.count-1 do begin
      ps := FPayloadStreams[t];
      if ps <> nil then begin
        tot := tot + 1;
{$IFDEF PAYLOAD_IS_UNBUFFERED}
        result := result + ps.PercentBuffersFlushed;
{$ENDIF}
      end;
    end;
    result := result / tot;
  finally
    Unlock;
  end;
end;//ok


function TVirtualDisk_Advanced.MaxPercentPayloadBuffersUnFlushed: single;
var
  t: ni;
  ps: TPayloadStream;
  tmp: single;
begin
  Lock;
  try
    result := 0;
    for t:= 0 to self.FPayloadStreams.count-1 do begin
      ps := FPayloadStreams[t];

      if ps <> nil then begin
{$IFDEF PAYLOAD_IS_UNBUFFERED}
        tmp := 1- ps.PercentBuffersFlushed;
        if tmp > result then
          result := tmp;
{$ENDIF}
      end;
    end;
    result := result;
  finally
    Unlock;
  end;
end;//ok

procedure TVirtualDisk_Advanced.AnalyzeGaps;
var
  t, u: fi;
  vart: PVirtualAddressRecord;
  fp: PFilePhysical;
  iStart, iLen,istreamsize: int64;
  // a: array of int64;
  compare: int64;
  bRetry: boolean;
begin

  Debug.Log('Checking Streams');
  for u := 0 to FPayloadStreams.count-1 do
  begin


    if FPayloadStreams[u] = nil then
      continue;
    FPayloadStreams[u].gaps.Clear;
    if FPayloadStreams[u] <> nil then
    begin
      FPayloadStreams[u].gaps.Length := FPayloadStreams[u].Size;
      if FPayloadStreams[u].gaps.Length >=sizeof(TVirtualDiskPayloadFileHeader) then begin

        FPayloadStreams[u].gaps.ConsumeRegion(0, sizeof(TVirtualDiskPayloadFileHeader));
      end;
      vart := self.vat.FindVarWithHighestPhysicalForPayload(u);
      iStreamSize := Fpayloadstreams[u].Size;
      if vart = nil then begin
        iLen := SizeOf(Tvirtualdiskpayloadfileheader);
      end else begin
        fp := vart.GetFP(u);
        iLen := fp.PhysicalAddr_afterheader+(GetBigBlockSizeInBytes(vart.FileCount)+sizeof(TVirtualDiskBigBlockHeader));
//        debug.log(self, 'highest physical for file '+inttostr(u)+' is 0x'+inttohex(iLen, 1));
//        debug.log(self, 'payload size is '+inttostr(u)+' is 0x'+inttohex(FpayloadStreams[u].size, 1));
      end;
      if ilen < iStreamSize then begin
        logrepair(-2, 'freeing payload space that appears to be empty at end. Will lose '+commaize(Fpayloadstreams[u].size-iLen)+' new size='+commaize(ilen)+' old='+commaize(iStreamSize));
        FPayloadStreams[u].Size := iLen;
        FPayloadStreams[u].gaps.Length := iLen;
      end;

    end;

  end;

  SanityCheckPayloads;

  //FOR all table entries
  for t := low(vat.table) to high(vat.table) do
  begin
    if (t and $FFFF) = 0 then begin
      Debug.Log('Checking GAPS:'+inttohex(t,0));
    end;
    vart := @vat.table[t];

    //for all files in this table entry
    for u := 0 to vart.FileCount - 1 do
    begin
      //get file physical entry
      fp := @vart.FPs[u];

      //if file is defined and payload object exists
      if (fp.FileID>=0) and (FPayloadStreams[fp.FileID] <> nil)  then
      begin
        //determine start and length
        //start is physical address MINuS the big block header length
        iStart := fp.PhysicalAddr_afterheader - sizeof(TVirtualDiskBigBlockHeader);
        //length includes header and footer
        iLen := GetBigBlockSizeInBytes(vart.FileCount, true);
        if iStart < 0 then begin
          Self.LogRepair(t,'payload start would be negative, invalidating');
          fp.FileID := -1;
          vat.MarkTableEntryDirtyByPtr(vart);
        end else
        // debug.Log(inttostr(fp.fileid)+' will consume ['+inttohex(iStart, 0)+'-'+inttohex(iLen, 0)+']');
        if iStart+iLen > Fpayloadstreams[fp.fileid].size then begin
          Self.LogRepair(t,'payload is beyond the end of file, invalidating');
          fp.FileID := -1;
          vat.MarkTableEntryDirtyByPtr(vart);

        end else
        if not FPayloadStreams[fp.FileID].gaps.CanConsumeRegion(iStart, iLen)
        then
        begin
          Self.LogRepair(t,'File '+fp.fileid.tostring+' GAP ERROR! Placed By: '+vart.placedby.tostring+' could not consume region ['+inttohex(iStart,1)+'-'+inttohex(iStart+iLen-1,1)+']');
          fp.FileID := -1;
          vat.MarkTableEntryDirtyByPtr(vart);
        end
        else
        begin
          FPayloadStreams[fp.FileID].gaps.ConsumeRegion(iStart, iLen);
        end;
      end;
    end;
  end;

  SaveVat(false);
  SanityCheckPayloads;

  for u := 0 to FPayloadStreams.count - 1 do
  begin
    IF FPayloadStreams[u] = nil then
      continue;

    //debug.Log('Check payload ' + inttostr(u));
    repeat
      bRetry := false;

      vart := vat.FindVarWithHighestPhysicalForPayload(u);
      if vart <> nil then
      begin
        compare := vart.GetFP(u).PhysicalAddr_afterheader + GetBigBlockSizeInBytes
          (vart.FileCount, false) + sizeof(TVirtualDiskBigBlockHeader);
        if compare <>
          ((vart.GetFP(u).PhysicalAddr_afterheader - sizeof(TVirtualDiskBigBlockHeader)) +
          GetBigBlockSizeInBytes(vart.FileCount, true)) then
        begin
          raise ECritical.Create('wtf maths error.  seriously this should not happen');
        end;
        if compare < FPayloadStreams[u].gaps.Length then
        begin
          logrepair(vart.vatindex, 'length error, there''s extra space at the end of file ' +
            inttostr(u));
          FPayloadStreams[u].gaps.Length := compare;
          FPayloadStreams[u].Size := compare;
          if FPayloadStreams[u].gaps.Length <> compare then
            raise ECritical.Create('length setting did not stick in b-tree');
          // vat.MarkTableEntryDirtyByPtr(vart);

          bRetry := true;
          //vart.GetFP(u).FileID := -1;
          vat.MarkTableEntryDirtyByPtr(vart);
        end;
      end;
    until bRetry = false;
    SanityCheckPayloads;
  end;

end;

procedure TVirtualDisk_Advanced.BackupVat;
var
  s: string;
  t: ni;
  ps: TPayloadStream;
  r: ni;
  l: TLock;
begin
  exit;
  if GetTimeSince(Self.vatlastbackedup) > 300000 then
  begin
    debug.Log('backing up vat.');
    l := GetLock;
    try
      if vat.needsbackup then
      begin
        r := GetLeastBusyPhysical();
        t := r;
        // for t:= 0 to FPayloadStreams.count-1 do begin
        ps := FPayloadStreams[t];
        if ps <> nil then
        begin
          try
            SaveVatAndConfig(ps.FileName + '.vat.backup');
          except
            on E: Exception do begin
              AlertAdmin('err when Save backup vat:'+e.Message);
            end;
          end;
        end;
        // end;
        vat.needsbackup := false;
      end;

      vatlastbackedup := getticker;

    finally
      unlocklock(l);
      debug.Log('backing up vat....complete');
    end;
  end;

end;

procedure TVirtualDisk_Advanced.BeforeDestruction;
var
  t: ni;
begin
  Debug.Log(self,'Before Destruction');
  inherited;
  Debug.Log(self,'Disabling shipper');
  Debug.Log(self,'Stopping Resurrector');
  Resurrector.BeginStop;
  Debug.Log(self,'Stopping Bring-online Thread');
  StopBringOnline;
  Debug.Log(self,'Stopping Scrubber Thread');
  StopScrubber;
  Debug.Log(self,'Stopping Shipper Thread');
  StopShipper;
  Debug.Log(self,'Killing Resurrector');
  Resurrector.EndStop;
  REsurrector.WaitFor;
  TPM.NoNeedthread(Resurrector);
  resurrector := nil;

  Debug.Log(self,'Flushing buffers');
  SyncAwayAllBuffers;

  Debug.Log(self,'Stopping pending source fetches');
  for t:= 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
    try
      Debug.Log(self,t.tostring);
      if FsourceFetchcommands[t] <> nil then begin
        FsourceFetchcommands[t].waitfor;
        FsourceFetchcommands[t].free;
        FsourceFetchcommands[t] := nil;
      end;
    except
    end;
  end;

  Debug.Log(self,'Killing Active source fetch clients');
  for t:= 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
    if FsourceArchivers[t]<>nil then begin
      Debug.Log(self,t.tostring);
      FsourceArchivers[t].Disconnect;
    end;
  end;

  Debug.Log(self,'Destrying Active source fetch clients');
  for t:= 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
    if FsourceArchivers[t]<>nil then begin
      Debug.Log(self,t.tostring);
      FsourceArchivers[t].free;
      FsourceArchivers[t] := nil;
    end;
  end;


  Debug.Log(self,'STopping Queue');
  queue.Stop;
  TPM.NoNeedthread(queue);
  queue := nil;
  Debug.Log(self,'Destroying buffers');
  //SyncBuffer(-1, true);
  while not (FlushBuffers(true, 0, false, false) = bsNoBuffersToFlush) do
    sleep(100);
  CachedStripes := 32;
  Debug.Log(self,'Saving VAT and Config');
  SaveVatAndConfig;
  Debug.Log(self,'Destroying Shipper - Pushing Checksums');
  shipper.Free;//DO THIS LATER than the stop because it pushes the checksums.  We want the checksums to be pushed after all the disk buffers have been flushed
  shipper := nil;
  Debug.Log(self,'Saving Stats');
  SaveVatStats;
end;

procedure TVirtualDisk_Advanced.BeginRepair;
begin
  bRepaired := false;
end;//ok

function TVirtualDisk_Advanced.BigBlocksInDisk: int64;
begin
  result := ((size-1) div BIG_BLOCK_SIZE_IN_BYTES)+1;
end;//ok

function TVirtualDisk_Advanced.BringBigBlockOffline(idx: ni): boolean;
begin
  result := false;
  if (idx >= 0 ) and (idx < MAX_BLOCKS_IN_VAT) then begin
    online_bigblocks[idx] := false;
    exit(true);
  end;
end;//ok

function TVirtualDisk_Advanced.BringBigBlockOnline(idx: ni): boolean;
var
  t, u: ni;
  ta: PVirtualAddressRecord;
  pfh: TVirtualDiskPayloadFileHeader;
  ps, ps2: TPayloadStream;
  bbhs: array[0..15] of TVirtualDiskBigBlockHeader;
  qis: array[0..15] of Tcmd_BBHFetch;
  a, b: int64;
  fp: PFilePhysical;
  itemp: int64;
  bChanged: boolean;
  violations: array of int64;
  l: TLock;
  scr: TSelfCheckResult;
  tmOnTime: ticker;
  tmSince: ticker;
begin

  if (idx  < 0) or (idx >= MAX_BIG_BLOCKS) then
    raise ECritical.create('Big Block index cannot be '+inttohex(idx,1));
  // if the block is already online then forget it
  result := false;
  if online_bigblocks[idx] then
    exit;

  if idx = 0 then
    debug.log('trap');

  tmOnTime := getticker;
  result := true;

  bChanged := false;

  l := GetLock;
  try
    AllowDriveSkipping := false;
    FillMem(@bbhs[0], sizeof(bbhs), 0);


    t := idx;
    Status := 'Bring Online @' + inttohex(idx, 0);

    ta := @Self.vat.table[t];
//    Debug.Log('bring online '+inttohex(idx,1)+ta.DebugString);

    scr := ta.SelfCheckOk(t, self);
    if gQuickFixNeedsSave then begin
      SaveVat(true);
      gQuickFixNeedsSave := true;
    end;
    if not (scr = scrOK) then begin
      vat.MarkTableEntryDirtyByPtr(ta);
      //self.AnalyzeGaps;
      bChanged := true;

      if scr = scrCritical then begin
        if shipper.enabled then begin
          RebuildBigBlockFromTargetArchive(t);
        end else begin
          RecheckDiskOnCompletion := true;
//        REstartBringOnline;
        end;
      end;
    end;

    if ta.FileCount <= 0 then begin
      online_bigblocks[idx] := true;
      result := false;
      exit;
    end;

    if ta.StartingBlock <> (t*_BIG_BLOCK_SIZE_IN_BLOCKS) then begin
      LogRepair(t,'Fixing vat @'+inttohex(t,1)+', starting block doesn''t match index');
      ta.StartingBlock := (t*_BIG_BLOCK_SIZE_IN_BLOCKS);
      ta.FileCount := FILE_COUNT_VAT_FUXORED;
      vat.MarkTableEntryDirtyByPtr(ta);
    end;

    // check that the blocks physical addresses are all sane
    for u := 0 to ta.FileCount-1 do begin
      fp := @ta.FPs[t];
      if ta.FPs[t].FileID >= 0 then begin
        PS := FPayloadStreams[ta.FPs[t].FileID];
        fp.CheckAndCorrect(ta.FileCount, ps);
      end;
    end;

    // check that the block does not overlap other blocks
    if not vat.VerifyGapSingle(idx, violations) then
    begin
      LogRepair(idx,'GAP ERROR! @idx=' + inttohex(idx, 0) +
        ' will force reconstitution.');
      ta := @Self.vat.table[idx];
      //ForceReconstitution(ta);
      vat.MarkTableEntryDirtyByPtr(ta);
    end;

    // if any files are idx -1 then we need to reconstitute this block
    if ta.HasUndefinedLocations or (ta.fileCount=0) then
    begin
      if not bInReconstitution then
      begin
        LogRepair(ta.vatindex, 'VAR has undefined locations! @idx=' + inttohex(idx, 0) +
          ' will force reconstitution.');
{$IFDEF DETAILED_DEBUGGING}
        debug.Log(self,'Before:' + NEWLINE + vat.DebugVatSTructure);
{$ENDIF}
        ForceReconstitution(ta);
        vat.MarkTableEntryDirtyByPtr(ta);
        bChanged  := true;
{$IFDEF DETAILED_DEBUGGING}
        debug.Log(self,'After:' + NEWLINE + vat.DebugVatSTructure);
{$ENDIF}
      end;
    end;

{$IFDEF PREFETCH_ON_BRING_ONLINE}
    PrefetchAllPayloads(
        (idx * BIG_BLOCK_SIZE_IN_BYTES)-SizeOf(TVirtualDiskBigBlockHeader),
         true, false);
{$ENDIF}
    // go through each of the files
    for u := 0 to ta.FileCount - 1 do begin
      // check that the vat entry exists in the physical
      if ta.FPs[u].FileID >= 0 then
      begin
        ps := FPayloadStreams[ta.FPs[u].FileID];
        if ps <> nil then
        begin

          // IF THERE ARE TWO OR FEWER entires (mirrored or single)
          if ta.FileCount < 3 then
          begin
            // if the stream is missing break the mirror or drop alltogether
            if (ta.FPs[u].FileID > (Self.FPayloadStreams.count - 1)) or
              (Self.FPayloadStreams[ta.FPs[u].FileID] = nil) then
            begin
              LogRepair(ta.vatindex, 'Breaking mirror at ' + inttohex(t,1) + ' was ' +
                inttostr(ta.FPs[u].FileID));
              ta.RemoveFile(u);
              vat.MarkTableEntryDirtyByPtr(ta);
              bChanged := true;
            end;

            // if the physical address is beyond the end of the file, then break/drop
            a := (ta.FPs[u].PhysicalAddr_afterheader + GetBigBlockSizeInBytes(1, false) +
              sizeof(TVirtualDiskBigBlockHeader));
            b := FPayloadStreams[ta.FPs[u].FileID].Size;
            if a > b then
            begin
              LogRepair(ta.vatindex, 'Fixing bogus fileid at ' + inttohex(t,1) + ' was ' +
                inttostr(ta.FPs[u].FileID) + ' a=' + inttostr(a) + ' b=' +
                inttostr(b));
              ta.RemoveFile(u);
              vat.MarkTableEntryDirtyByPtr(ta);
              SaveVat(false);
              bChanged := true;
            end;
          end;
        end;
      end;
    end;

    // go through each of the files AGAIN
    for u := 0 to ta.FileCount - 1 do begin
      // check that the vat entry exists in the physical
      if ta.FPs[u].FileID >= 0 then
      begin
        ps := FPayloadStreams[ta.FPs[u].FileID];
        if ps <> nil then
        begin
{$DEFINE USE_ANON_QUEUE}
{$IFNDEF USE_ANON_QUEUE}
          bbhs[u] := ps.GetBigBlockHeaderFromPhysicalStart
             (ta.FPs[u].PhysicalAddr);
{$ELSE}
          qis[u] := Tcmd_BBHFetch.Create;
          qis[u].phys := ta.FPs[u].PhysicalAddr_afterheader;
          qis[u].pbbh := @bbhs[u];
          qis[u].ps := ps;
          qis[u].Priority := bpHighest;
          qis[u].Start;
{$ENDIF}
        end;
      end;
    end;

    // go through each of the files AGAIN
    for u := 0 to ta.FileCount - 1 do begin
      // check that the vat entry exists in the physical
      if ta.FPs[u].FileID >= 0 then
      begin
        ps := FPayloadStreams[ta.FPs[u].FileID];
        if ps <> nil then
        begin
{$IFDEF USE_ANON_QUEUE}
//            if not qis[u].WAitFor(8000) then
//              Debug.Log(self, 'wtf');
            try
              while not qis[u].WAitFor(8000) do
                Debug.Log('Waiting for: '+ps.filename);
//              qis[u].WAitFor();
            except
              on E: Exception do begin
                Debug.Log('Exception during BBH fetch: '+e.message);
              end;
            end;
            qis[u].Free;
            qis[u] := nil;
{$ENDIF}
            // if the big block header in the payload is not valid, break/drop
            if ta.FPs[u].PhysicalAddr_afterheader > 0 then
            begin
{
              //It is better to NOT do this one because a simple tweak fix like this
              //could yield incorrect but sane results wreaking havoc over the disk.
              if bbhs[u].TotalSizeIncludingHeaders <> GetBigBlockSizeInBytes(ta.FileCount,true) then begin
                LogRepair(ta.vatindex, 'Big Block Header Size does not match expected, correcting.');
                bbhs[u].TotalSizeIncludingHeaders := GetBigBlockSizeInBytes(ta.FileCount,true);
                bbhs[u].PayloadStart := ta.FPs[u].PhysicalAddr;
                bbhs[u].HeaderStart := ta.FPs[u].PhysicalAddr - sizeof(TVirtualDiskBigBlockHeader);
                bbhs[u].FooterStart :=ta.FPs[u].PhysicalAddr+GetBigBlockSizeInBytes(ta.FileCount,false);
                bbhs[u].UpdateCheckSum;
                ps.Seek(bbhs[u].HeaderStart,soBeginning);
                stream_guaranteewrite(ps, pbyte(@bbhs[u]), sizeof(bbhs[u]));
                ps.seek(bbhs[u].FooterStart,soBeginning);
                stream_guaranteewrite(ps, pbyte(@bbhs[u]), sizeof(bbhs[u]));
                vat.MarkTableEntryDirtyByPtr(ta);
              end;
}
              if not bbhs[u].VAlid(ta.StartingBlock * BLOCKSIZE) then
              begin
                LogRepair(ta.vatindex, 'Big Block header is bogus, removing from vat');
                ta.RemoveFile(u);
                vat.MarkTableEntryDirtyByPtr(ta);
                SaveVat(false);
                bChanged := true;
              end;

            end
            // else if there are more than 2-files, operate in raid mode
            else
            begin
              // determine how big the file SHOULD be to accomodate this block
              itemp := ta.FPs[u].PhysicalAddr_afterheader + GetBigBlockSizeInBytes
                (ta.FileCount, false) + sizeof(TVirtualDiskBigBlockHeader);
              // if the file isn't big enough, then expand the file to it's intended size
              if ps.Size < itemp then
              begin
                itemp := vat.GetSafePhysicalAddr(ta.FPs[u].FileID);
                LogRepair(ta.vatindex, 'File has incomplete block, growing to ' +
                  inttostr(itemp));
                ps.GrowFile(vat.GetSafePhysicalAddr(ta.FPs[u].FileID));
                LogRepair(ta.vatindex, 'RAID5 piece could not be found. Expanding file size to accomodate.');
                ta.FPs[u].PhysicalAddr_afterheader :=
                  ps.Expand(GetBigBlockSizeInBytes(ta.FileCount, false),
                  ta.StartingBlock * BLOCKSIZE);
                FPayloadStreams[ta.FPs[u].FileID].gaps.Length := ps.Size;
                FPayloadStreams[ta.FPs[u].FileID].gaps.ConsumeRegion
                  (ta.FPs[u].PhysicalAddr_afterheader - sizeof(TVirtualDiskBigBlockHeader),
                  GetBigBlockSizeInBytes(ta.FileCount, true));
                vat.MarkTableEntryDirtyByPtr(ta);
                bChanged := true;
              end;
            end;
          end;
        end;
        //
        if ta.FPs[u].FileID < 0 then
          ps2 := nil
        else
          ps2 := Self.FPayloadStreams[ta.FPs[u].FileID];
        if ps2 = nil then
        begin
          // if we're here, then we have blocks assigned to files that officially don't exist anymore
          // these blocks cannot be found with a re-Source
          // therefore we will mark them as -1 and, if there is enough redundancy,
          // they can be rebuilt via Reconstitution.

          // NOT SURE WE WANT TO DO THIS
          // ta.FPs[u].physicaladdr := -1;
          // ta.FPs[u].FileID := -1;
        end
        else if Self.FPayloadStreams[ta.FPs[u].FileID].Size > ta.FPs[u].PhysicalAddr_afterheader
        then
        begin
          // check that the vat entry is not truncated
          a := Self.FPayloadStreams[ta.FPs[u].FileID].Size;
          b := (ta.FPs[u].PhysicalAddr_afterheader + (1 * sizeof(TVirtualDiskBigBlockHeader)) + GetBigBlockSizeInBytes(ta.FileCount));
          if a >= b then
          begin
            // good
          end
          else
          begin
            LogRepair(ta.vatindex, 'Fixing:' + inttohex(ta.StartingBlock, 2));
            // truncate file... this block is shit
//            Self.FPayloadStreams[ta.FPs[u].FileID].Size :=
//              ta.FPs[u].PhysicalAddr;

{$IFDEF REDUNDANT_PHYSICAL}
            pfh := Self.FPayloadStreams[ta.FPs[u].FileID].GetPFH;
            pfh._NextPhysicalAddress := ta.FPs[u].physical;
            Self.FPayloadStreams[ta.FPs[u].FileID].SetPFH(pfh);
{$ENDIF}
            ta.RemoveFile(u);
            // <--- should mark the fileid -1 so that we can reconstitute
            ForceReconstitution(ta);
            vat.MarkTableEntryDirtyByPtr(ta);

            bChanged := true;
          end;
        end;

      end;

    if bChanged then
    begin
      //vat.MarkDirty(0, sizeof(vat));
      SaveVat(false);
//      self.AnalyzeGaps;
    end;

    bRepaired := false;

    online_bigblocks[idx] := true;
    result := true;

    {$IFDEF ENABLE_DEEP_CHECKS_IN_BRING_ONLINE}
      DeepCheck(idx);
    {$ENDIF}

    if tmSince > 50 then
      ZoneLog(idx,tmSince.tostring+' ms. bring online '+inttohex(idx,1)+ta.DebugString);

  finally
    //savevat(false);
{$IFDEF ALLOW_DRIVE_SKIPPING}
    AllowDriveSkipping := true;
{$ENDIF}
    tmSince := gettimesince(tmOntime);
    UnlockLock(l);

  end;
end;//ok


function TVirtualDisk_Advanced.BuddyUp: boolean;
var
  vart: PVirtualAddressRecord;
  toID,fromid: ni;
  toS, fromS: TPayloadStream;
  l: Tlock;
label rep;
begin

  result := false;
  if self.vat.MaxDiskSpan = 1 then exit;

  l := Getlock;
  try
    vart := vat.FindVARToBuddyUp;


    if vart <> nil then
    begin
//      Debug.Log('Needs to buddy up: '+vart.debugstring);

      if not online_bigblocks[vart.VatIndex] then begin
//        Debug.Log('block was not online, most buddy up later');
        exit(false);
      end;


      toS := GEtUnderQuotaStream(vart, toID, false);
      if toS <> nil then
      begin
        Status := 'buddying up ' + vart.DebugString;
        ZoneLog(vart.vatindex,status);
        fromID := vart.FPs[0].FileID;
        if fromID < 0 then begin
          fromID := vart.FPs[1].FileID;
          if FromID < 0 then begin
            ForceReconstitution(vart);
            exit;
          end;
        end;

        fromS := FPayloadStreams[fromID];
        if fromS <> nil then
        begin
          MovePayloadBigBlock(vart, fromID, toID, fromS, toS, true, PLACED_BY_BUDDYUP);
          ZoneLog(vart.vatindex,'after payload move, file size is '+inttohex(tos.size,0));
          ZoneLog(vart.vatindex,'after payload move, '+vart.DebugString);
          if vart.IsDead then
            ZoneLog(vart.vatindex,'DEAD AFTER MIRROR!');


          result := true;
        end
        else
          result := false;

      end else
        result := false;
    end;

  finally
    Unlocklock(l);
  end;

end;//ok

procedure TVirtualDisk_Advanced.ChangeSingleByte(iAddr: int64; b: byte);
var
  block: array [0 .. BLOCKSIZE - 1] of byte;
  iOFF: int64;
begin
  Self.ReadBlock(iAddr shr BLOCKSHIFT, @block[0]);
  iOFF := iAddr shr BLOCKSHIFT;

  block[iOFF] := b;
  Self.WriteBlock(iAddr shr BLOCKSHIFT, @block[0]);

end;//ok

constructor TVirtualDisk_Advanced.Create;
begin
  front_RecommendedPrefetchAllowance := 256;
  rsPrefetch := TRingStats.create;
  rsPRefetch.Size := 256;
  rs0 := TRingStats.create;
  rs1 := TRingStats.create;
  rs2 := TRingStats.create;
  rs3 := TRingStats.create;
  rs4 := TRingStats.create;
  rs5 := TRingStats.create;
  rs6 := TRingStats.create;
  rs7 := TRingStats.create;


  inherited;


  icsSC(csScrubber, 0);
  rc := TRaidCalculator.Create;

  Repairlog := TStringlist.Create;
  FCacheSize := 256 * MEGA;
  FPayloadStreams := TBetterList<TPayloadStream>.Create;

  vat.InitVirgin;
  FRaidsByBlock := TRaidTree.Create;
  FRaidsByBlock.SortMethod := rtsBlock;
  FRaidsByDirtyTime := TRaidLinkedList<TRaidTreeItem_ByDirtyTime>.Create;
  FRaidsByLAstUsed := TRaidLinkedList<TRaidTreeItem_ByLastUsedTime>.Create;

  CachedSTripes := 32;
  primary_buffer := FRaidsByLastUsed.LastItem.rab;
  queue := tpm.needthread<TVDQueue>(nil);
  queue.betterpriority := bpTimeCritical;
  queue.MaxItemsInQueue := 2; //DO NOT set higher than # of tolerable resurrections before disk timeout
  queue.OnIdle := self.vdqueue_onidle;
{$IFDEF ENABLE_IDLE_QUEUE}
  queue.noworkruninterval := 1;
{$ENDIF}
  //queue.AutoMaintainIdleInterval := true;
  queue.Start;

  sfthread := TPM.Needthread<TVDSideFetchThread>(nil);
  sfthread.vd := self;
  sfthread.Loop := true;
  sfthread.ColdRunInterval := 1;
  sfthread.RunHot := true;
  sfthread.HasWork := false;
  sfthread.betterPriority := bpHighest;
  sfthread.start;

  resurrector := TPM.Needthread<TResurrector>(nil);
  resurrector.Loop := true;
  resurrector.disk := self;



end;

function TVirtualDisk_Advanced.GEtOverQuotaStream(out id: nativeint)
  : TPayloadStream;
var
  t: ni;
  pfc: TPayloadFileInformation;
  str: TPayloadStream;
  i1, i2: int64;
begin
  id := -1;
  result := nil;
  for t := 0 to FPayloadStreams.count - 1 do
  begin
    str := FPayloadStreams[t];
    if str = nil then
      continue;
    i1 := str.Size - sizeof(TVirtualDiskPayloadFileHeader);
    i2 := vat.PayloadConfig.filelist[t].size_limit;
    if ((i1 > i2) and (i2 >= 0))
//    or ((Self.vat.PayloadConfig.filelist[t].priority < 0) and (i1 >= sizeof(TVirtualDiskPayloadFileHeader)))
    then
    begin
      debug.Log(self,str.FileName + ' is over quota ... size:' + inttostr(i1));
      result := str;
      id := t;
      break;
    end
    else
    begin
      // Debug.Log(str.FileName+' not over quota');
    end;
  end;

end;//ok

function TVirtualDisk_Advanced.GEtUnderQuotaStreamxx
  (notOf: PVirtualAddressRecord; out id: nativeint): TPayloadStream;
var
  t: nativeint;
  ps: TPayloadStream;
begin
  result := GEtUnderQuotaStream(notOf, id, false);
  if result = nil then
    result := GEtUnderQuotaStream(notOf, id, true);

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
    end; }
end;

procedure TVirtualDisk_Advanced.MovePayloadBigBlock(pvar: PVirtualAddressRecord;
  FromID, toID: nativeint; fromstream, tostream: TPayloadStream;
  bMirror: boolean; placed_by_reason: byte);
var
  pfh_from, pfh_to: TVirtualDiskPayloadFileHeader;
var
  buf: pointer;
  i: int64;
  npa: int64;
  fromaddr,toaddr:int64;
  // pva: PVirtualAddressRecord;
  bbh_from, bbh_to: TVirtualDiskBigBlockHeader;
  pf, fpTemp: PFilePhysical;
  sOP: string;
  iOldSize: int64;
  sz:int64;
  frompay, topay: int64;
begin
{$IFDEF USESEEKLOCK} fromstream.SeekLock; {$ENDIF}
{$IFDEF USESEEKLOCK} tostream.SeekLock; {$ENDIF}
  try

    if bMirror then
      sOP := 'Copy'
    else
      sOP := 'Move';

    ZoneLog(pvar.vatindex,'Start of '+sOp+' operation');
    // debug.Log(inttostr(toStream.size)+' // '+inttostr(fromstream.size));
    bringbigblockonline(pvar.VatIndex);

{$IFDEF REDUNDANT_PHYSICAL}
    pfh_from._NextPhysicalAddress := i;
{$ENDIF}
    bbh_from := fromstream.GetBigBlockHeaderFromPhysicalStart(pvar.GetFP(FromID).PhysicalAddr_afterheader);
    bbh_from.CheckAddressValidity(GetBigBlockSizeInBytes(pvar.FileCount, false), pvar, fromid);
    fromaddr := bbh_from.headerstart;
    if not bbh_from.VAlid(fromAddr) then begin
      fpTemp := pvar.GetFP(FromID);
      if fpTemp <> nil then
        fpTemp.FileID := -1;

      BringBigBlockOffline(pvar.VatIndex);
      BringBigBlockOnline(pvar.VatIndex);
      //pvar.FileCount := -2;//???? why mark the whole vat bad because of one bad bbh?
      vat.MarkTableEntryDirtyByPtr(Pvar);
      exit;
    end;

    // expand the target stream
    iOldSize := tostream.Size;
    npa := tostream.Expand(GetBigBlockSizeInBytes(greaterof(pvar.FileCount,1), false), bbh_from.VirtualAddress,true);
    FPayloadStreams[toID].gaps.Length := tostream.Size;
    pfh_to := tostream.GetPFH;

    bbh_to := tostream.GetBigBlockHeaderFromPhysicalStart(npa);
    toaddr := bbh_to.headerstart;
    toaddr := bbh_to.payloadstart;


    // make sure we have an entry for this
    if (pvar <> nil) then
    begin
      sz := getbigblocksizeinbytes(pvar.FileCount,false);
      //todo 1: check and set totalsizeincludingheaders as part of bringblockonline

      // buf := getmemory(1024000);
      try
        // tostream.CopyFrom(fromstream,  GetBigBlockSizeInBytes(1));
        // Debug.Log(sOP+' From:'+inttostr(fromid)+':???? to '+inttostr(toid)+':'+inttohex(npa,0));
        if not bMirror then
          FPayloadStreams[FromID].gaps.FreeRegion(fromaddr, sz);
        // FGapTrees[ToID].Length := greaterof(FGapTrees[toid].length, bbh_to.EndofFooter); don't do this, the region has already been consumed


        ZoneLog(pvar.vatindex,sOp+' payload from '+inttostr(fromid)+' to '+inttostr(toid));
        ZoneLog(pvar.vatindex,'from @'+inttohex(fromaddr,1)+' to @'+inttohex(toaddr,1)+' '+inttohex(sz,1)+' bytes.');
        frompay := bbh_from.PayloadStart;
        topay := bbh_to.PayloadStart;


{$IFDEF HEADER_COPY}
        //WE PROBABLY DONT WANT TO COPY THE HEADER AND FOOTER
        //- this is bacause calling Expand() earlier sets up a bigblock for us at the target
        //- with propert headers and footers
        //- also don't let the checksum presence fool you
        //- the checksum only applies to the header (which should be correct)
        //- big blocks are too big for checksum maintenance... only the headers
        //- are maintained.
        fromstream.Seek(fromaddr, 0);
        tostream.Seek(toaddr, 0);
        stream_guaranteeWrite(tostream, @bbh_to, sizeof(bbh_to));
{$ELSE}
        fromstream.Seek(bbh_from.payloadstart, 0);
        tostream.Seek(bbh_to.payloadstart, 0);
{$ENDIF}

        Stream_GuaranteeCopy(fromstream, tostream,sz);

{$IFDEF HEADER_COPY}
        stream_guaranteeWrite(tostream, @bbh_to, sizeof(bbh_to));
{$ENDIF}


        // Debug.Log('Completed '+sOP+' From:'+inttostr(fromid)+':???? to '+inttostr(toid)+':'+inttohex(npa,0));

      finally
        // Freememory(buf);
      end;

      // Save physical Addres to "to" file
      tostream.SetPFH(pfh_to);

      // change fileid in vat
{$IFDEF ALLOW_RAID}
      if bMirror then
      begin
        pvar.FPs[pvar.FileCount].FileID := toID;
        inc(pvar.FileCount);
        pf := pvar.GetFP(toID);
        pf.PhysicalAddr_afterheader := npa;
      end
      else
      begin
        pvar.ChangeFileID(FromID, toID, bbh_to.PayloadStart);
        pf := pvar.GetFP(toID);
        if pf <> nil then
        begin
          pf.PhysicalAddr_afterheader := npa;
        end;
      end;

{$ELSE}
      pva.FileID := toID;
      pva.PhysicalAddress := npa;
{$ENDIF}
      pvar.placedby := placed_by_reason;
      vat.MarkTableEntryDirtyByPtr(pvar);
      SaveVat(false);

    end;

    // save physical address back to "from" file
{$IFDEF REDUNDANT_PHYSICAL}
    pfh_from._NextPhysicalAddress := fromstream.GetLastBigBlockHeaderAddr;
    fromstream.SetPFH(pfh_from);
{$ENDIF}
    // reduce size of from file (unless we're copying for buddy-up purposes)
    if not bMirror then
    begin
      if fromstream.size = bbh_from.BeyondAddress then begin
        fromstream.Size := fromstream.Size - GetBigBlockSizeInBytes
          (pvar.FileCount, true);
        fromstream.gaps.Length := fromstream.Size;
      end;
    end;
  finally
{$IFDEF USESEEKLOCK} fromstream.seekunlock; {$ENDIF}
{$IFDEF USESEEKLOCK} tostream.seekunlock; {$ENDIF}
  end;

end;

procedure TVirtualDisk_Advanced.CheckVatSanity;
begin
  exit;

end;

procedure TVirtualDisk_Advanced.ClearRepairLog;
var
  l: TLock;
begin
  l := getLock;
  try
    Repairlog.Clear;
  finally
    UnlockLock(l);
  end;
end;

function TVirtualDisk_Advanced.ContinueBringOnline(thr: TExternalEventThread): TBringOnlineStatus;
var
//  t: ni;
  tm, tm2: ticker;
  vdlast, vdthis: int64;
  bOld: boolean;
  bWas: boolean;
  l : TLock;
begin
  inherited;
  result := bolOnline;
  try
  if FBringOnlineIdx < 0 then exit;
  tm2 := 1000;

  vdlast := reads_at_last_scrub xor writes_at_last_scrub;
  vdthis := vdlast;

//  t := FBringOnlineIdx;
  //if (FBringOnlineIdx and $F) = 0 then
  scrubber.Status := 'Bring Online:' + inttohex(FBringOnlineIdx, 1);

  if fbringonlineidx = $115 then
    debug.log(self, '115');
  try
    if (FBringOnlineIdx and $f) = 0 then
    begin
      vdlast := reads_at_last_scrub xor writes_at_last_scrub;
    end;


    if not trygetlock(l) then
    begin
      result := bolStall;
      exit;
    end;
    try
      tm := tickcount.getticker;
      scrubber.RunHot := FBringOnlineIdx > 0;
      repeat
        bWas := not BringBigBlockOnline(FBringOnlineIdx);

        if bWas then
          if FBringOnlineIdx = 0 then break else
            dec(FBringOnlineIdx);
//        if (FBringOnlineIdx and $FFFF) = 0 then
//           scrubber.Status := 'Bring Online:' + inttohex(FBringOnlineIdx, 1);
      until not bWas;

      scrubber.Status := 'Bring Online:' + inttohex(FBringOnlineIdx, 1);


      if not bWas then
        if vat.table[FBringOnlineIdx].FileCount > 0 then
        begin

          bOld := bInReconstitution;
          bInReconstitution := true;
          try

            ReadBlock(FBringOnlineIdx * _BIG_BLOCK_SIZE_IN_BLOCKS, nil);
            ReadBlock((FBringOnlineIdx * _BIG_BLOCK_SIZE_IN_BLOCKS) +
              (_BIG_BLOCK_SIZE_IN_BLOCKS - 1), nil);
            tmLastBringOnline := GetTicker;
          finally
            bInReconstitution := bOld;
          end;
        end
      else
        result := bolBringing;
      tm2 := GetTimeSince(tm);
    finally
      UnlockLock(l);
    end;

  finally
  end;

  vdthis := reads_at_last_scrub xor writes_at_last_scrub;

  if vdthis <> vdlast then
  begin
    sleepex(greaterof(lesserof(tm2 * 20, 30000), 1000));

    vdlast := vdthis;
  end;

  if FBringOnlineIdx >= 0 then
  begin
    dec(FBringOnlineIdx);
    result := bolBringing;
    // do not start scrubbing/raiding/buddying up until all this is done
    if FbringOnlineIdx < 0 then begin
      scrubber.status := 'online';
      if REcheckDiskOnCompletion then begin
        RecheckDiskOnCompletion := false;
        FBringOnlineIdx := MAX_BLOCKS_IN_VAT-1;
      end else
        result := bolOnline;
    end;
  end
  else
  begin
    AllowDriveSkipping := true;
    debug.Log(self,'bring online is about to finish.');

  end;
  finally
    if result = bolOnline then
      online := true;
  end;
end;

function TVirtualDisk_Advanced.Collapse(FileID: ni): boolean;
var
  pvar: PVirtualAddressRecord;
  sz: integer;
  // gap_address: int64;
  ps: TPayloadStream;
  fp: PFilePhysical;
  gi: TGapInfo;
  movefrom, moveto: int64;
  bIsFinalRegion: boolean;
  bOver: boolean;
begin
  try
{$IFNDEF ALLOW_COLLAPSE}
  exit(false);
{$ENDIF}
  result := false;
  if FileID < 0 then
    exit;

  if FileID >= MAX_PAYLOADS_PER_VD then
    raise ECritical.create('FileID cannot be '+inttostr(FileID));

  if FPayloadStreams[FileID].Collapsable = false then
    exit; // if the hint is false, then exit for performance purposes

  // get the highest over var for the particular file
  pvar := FindHighestOverVar(FileID, bOver);
  if pvar = nil then begin
    exit;
  end;
  ZoneLog(pvar.vatindex, 'Collape');
  // determine the size of the payload
  sz := GetBigBlockSizeInBytes(pvar.FileCount, true);

  //get the payload stream
  ps := Self.FPayloadStreams[FileID];
  if ps = nil then
    exit;

  //get the file-physical record
  fp := pvar.GetFP(FileID);

  if not bOver then begin
    ps.Size := fp.PhysicalAddr_afterheader+GetBigBlockSizeInBytes(pvar.FileCount)+sizeof(TvirtualDiskBigBlockHeader);
    ps.gaps.length := ps.size;
    exit;
  end;

  // query VAT to find a gap that is suitable for this block
  // if self.vat.FindGapLocation_Deprecated({size required}sz, {fileid}fileid, gap_address, fp.physicaladdr) then begin
  movefrom := fp.PhysicalAddr_afterheader - sizeof(TVirtualDiskBigBlockHeader);
  if movefrom >= ps.size then
    exit;

  gi := ps.gaps.FindGap({size}sz, {lessthan} movefrom, {minimize fragmentation}true);

  // if a gap was found, copy the data from the old location to the new one.
  if gi.Length > 0 then
  begin
      ZoneLog(pvar.vatindex,'COLLAPSE Moving Big Block from: ' + inttohex(movefrom, 1) + ' to '
      + inttohex(gi.start, 1));
//{$IFDEF DETAILED_DEBUGGING}
    debug.Log('Vat BEFORE:' + Self.vat.DebugVatSTructure);
//{$ENDIF}
    moveto := gi.start;
    ZoneLog(pvar.vatindex,'Moving From: ' + inttohex(movefrom, 1));
    ZoneLog(pvar.vatindex,'Moving To: ' + inttohex(moveto, 1));
    bIsFinalRegion := ps.gaps.Length = (movefrom + sz);
//    if not bIsFinalRegion then begin
//      debug.Log('somethign is wrong, this is not the final region.... aborting');
//      debug.Log(ps.gaps.getdebuggaps);
//      exit;
//    end;
    ps.gaps.ConsumeRegion(moveto, sz);
    ps.MoveBigBlock(movefrom, moveto, sz);
    ps.gaps.FreeRegion(movefrom, sz);
    if bIsFinalRegion then begin
      if movefrom < ps.size then begin
        ps.Size := movefrom;
        ps.gaps.length := movefrom;
      end;
    end;

    FPayloadStreams[FileID].gaps.Length := ps.Size;

    fp.PhysicalAddr_afterheader := moveto + sizeof(TVirtualDiskBigBlockHeader);
    pvar.placedby := PLACED_BY_COLLAPSE;
    Self.vat.MarkTableEntryDirtyByPtr(pvar);
    Self.SaveVat(false);
//{$IFDEF DETAILED_DEBUGGING}
//    ZoneLog(pvar.vatindex,'Vat AFTER:' + Self.vat.DebugVatSTructure);
//{$ENDIF}
  end
  else
  begin
    FPayloadStreams[FileID].Collapsable := false;
    /// this is a hint, but it may not be accurate because "sz" may vary... but it won't totally be the end of the world if collapsing is postponed
  end;
  except
    on E:Exception do begin
      Debug.log('collapse fail! in fileid '+fileid.tostring+' '+e.Message);
      exit(false);
    end;

  end;

end;

procedure TVirtualDisk_Advanced.CompleteRebuildOperation(bWait: boolean);
begin
  if currentRebuildOperation = nil then
    exit;
  //if the existing rebuild operation is complete, then cleanup
  if currentRebuildOperation.IsComplete or (bWait and currentRebuildOperation.waitfor) then begin
    currentRebuildOperation.Free;
    currentRebuildoperation := nil;
  end;

end;

procedure TVirtualDisk_Advanced.ScrubberExecute(thr: TExternalEVentThread);
var
  underid: nativeint;
  overid: nativeint;
  over, under: TPayloadStream;
  tm3, tm1, tm2: ticker;
  pvar: PVirtualAddressRecord;
  l: TLock;
  postsleeptime: ticker;
const
  SCRUBBER_COLD_INTERVAL = 1000;
  MIN_ACTIVE_USE_WAIT_TIME = 8000;
  MAX_ACTIVE_USE_WAIT_TIME = 12000;
  MAX_SHOVEL_INTERVAL = 1000;
  MAX_CONSTITUTION_CHECK_INTERVAL = 60000;
begin
{$IFDEF NO_SCRUBBER}
  thr.runhot := false;
  exit;
{$ENDIF}
  postsleeptime := 0;
  backgroundlock.LockWrite;

  if gettimesince(last_critical_work_time) > 50 then begin
    if Operational then
    begin
      if trygetlock(l) then
      try
//          Debug.Log('scrubber flush buffers');
        last_flush_status := FlushBuffers(false,10000,false,false);
        if last_flush_status = bsNoBuffersToFlush then begin
//            Debug.Log('scrubber flush buffers (1)');
          last_flush_status := FlushBuffers(false,1000,false,true)
        end else begin
//            Debug.Log('scrubber flush buffers (2)');
          last_flush_status := FlushBuffers(false,10000,true,true);
        end;

        BackupVat;
        EvalRunHot(thr);
        thr.stepcount := MAX_DIRTY_BUFFERS;
        thr.step := MAX_DIRTY_BUFFERS-self.FRaidsByDirtyTime.Count;
        last_critical_work_time := getticker;
      finally
        Unlocklock(l);
      end;

    end;
  end;



  try
  if GEtTimeSince(tmLastBringOnline) > 10000 then begin
    thr.IterationComplete;
    ContinueBringOnline(thr);
  end;
  if self.queue.estimated_backlog_size > 0 then begin
    postsleeptime := 10;
//    exit;
  end;
  if (GetTimeSince(ActiveUseTime) < MIN_ACTIVE_USE_WAIT_TIME) and (gettimesince(FLastScrubberExecuteTime) < Max_ACTIVE_USE_WAIT_TIME) then begin
    postsleeptime := 100;
//    exit;
  end;
  thr.IterationComplete;

  try
    tm1 := GetTicker;
    tm2 := gettimesince(tm1, FLastScrubberExecuteTime);
    if tm2 < 100 then begin
      postsleeptime := 100-tm2;
      exit;
    end;

    FLastSCrubberExecuteTime := tm1;

    FOperational := PayloadsOk;


    if hint_requests_waiting then begin
      thr.RunHot := false;
      thr.coldruninterval := SCRUBBER_COLD_INTERVAL;
      exit;
    end;

// CAUSES DEADLOCK
//    if MaxPercentPayloadBuffersUnFlushed > 0.2 then begin
//      postsleeptime := 50;
//      exit;
//    end;






    if hint_requests_waiting then begin
      thr.RunHot := false;
      thr.coldruninterval := SCRUBBER_COLD_INTERVAL;
      exit;
    end;

    if GetTimeSince(lastpayloadchange) < 60000 then
    begin
      non_critical_work_interval := 4000;
      exit;
    end;

    case ContinueBringOnline(thr) of
      bolStall: begin
        thr.coldruninterval := SCRUBBER_COLD_INTERVAL;
        postsleeptime := stall_time;
        stall_time := 100;
        stall_time := stall_time * 2;
        if stall_time > 4000 then
          stall_time := 4000;
        exit;
      end;
      bolBringing: begin
        thr.ColdRunInterval := 1000;
        stall_time := 100;
      end;
    end;


    if gettimesince(LAST_NONcritical_work_time) < non_critical_work_interval then begin
      exit;
    end;

    last_noncritical_work_time := getticker;
{$DEFINE AGGRESSIVE_NON_CRITICAL_WORK}
{$IFDEF AGGRESSIVE_NON_CRITICAL_WORK}
    non_critical_work_interval := 100;
{$ENDIF}


    try

      // if assigned(cmd_BringOnline) and (not cmd_BringOnline.IsComplete) then
      // exit;

      if not PayloadsOk then
        exit;



      tm1 := getticker;

      if gettimesince(tmLastShovel) > MAX_SHOVEL_INTERVAL then begin
        // buddy shovelling
        if TryGetLock(l) then
        try
//          Debug.Log('scrubber BuddyUp');
          if BuddyUp then
          begin // if stuff was done in the buddy-up function, then exit immediately
            tm2 := getticker;
            non_critical_work_interval := GetTimeSince(tm2, tm1){ * 2};
            exit;
          end;
        finally
          UnlockLock(l);
        end;

        // raid shovelling
        if TryGetLock(l) then
        try
          if RaidUP then
          begin
//            Debug.Log('scrubber RAIDUp');
            tm2 := getticker;
            non_critical_work_interval := GetTimeSince(tm2, tm1){ * 2};
            exit;
          end;
        finally
          UnlockLock(l);
        end;
      end;

      // reconstitution
      if (GetTimeSince(lastconstitutionCheck) > MAX_CONSTITUTION_CHECK_INTERVAL) or reconstituting then
      begin
        tm1 := getticker;
//        Debug.Log('scrubber reconstitute check');
        reconstituting := Reconstitute;
        if hint_requests_waiting then begin
          thr.RunHot := false;
          thr.coldruninterval := SCRUBBER_COLD_INTERVAL;
          exit;
        end;
        if reconstituting then begin
//          Debug.Log('scrubber reconst shovel');
          Shovel(thr, true);
        end;
        if hint_requests_waiting then begin
          thr.RunHot := false;
          thr.coldruninterval := SCRUBBER_COLD_INTERVAL;
          exit;
        end;
        tm2 := getticker;
        non_critical_work_interval := round(GetTimeSince(tm2, tm1)* 1.0);
        lastconstitutionCheck := getticker;
        if reconstituting then
          exit;
      end;

      // single shovelling
      if gettimesince(tmLastShovel) > MAX_SHOVEL_INTERVAL then begin
//        Debug.Log('scrubber shovel');
        Shovel(thr, false);
        tmLAstShovel := getTicker;
      end;
      if hint_requests_waiting then begin
        thr.RunHot := false;
        thr.coldruninterval := SCRUBBER_COLD_INTERVAL;
        exit;
      end;
    finally
      writes_at_last_scrub := writes;
      reads_at_last_scrub := reads;
    end;
  finally
  end;
  finally
    backgroundlock.UnlockWrite;
    if postsleeptime > 0 then
      sleep(postsleeptime);
  end;

end;

procedure TVirtualDisk_Advanced.Shovel(thr: TExternalEVentThread;
  bShrinkOnly: boolean);
var
  underid: nativeint;
  overid: nativeint;
  over, under: TPayloadStream;
  tm1, tm2: ticker;
  pvar: PVirtualAddressRecord;
  pf: PFilePhysical;
  l: TLock;
begin
  if TryGetLock(l) then
  try

    // Debug.Log('Scrub in');

    // determine OVER quota stream
    over := GEtOverQuotaStream(overid);

    if assigned(over) then
    begin

      // find VAR with highest address for that fileid
      FPayloadStreams[overid].Collapsable := true;
      pvar := FindHighestOverVar_AfterCollapse(overid);

      if pvar = nil then
      begin
        Status :=
          'There are over-quota payloads, but  they do not have entires in the VAT.  Freeing space.';
//        over.Size := 0;
//        over.CheckInit;
//        ZoneLog(pvar.vatindex,Status);
        EvalRunHot(thr);
      end
      else
      begin

        Status := 'Migrating.';
        ZoneLog(pvar.vatindex,Status);
        // find a stream to move this block to that isn't already in use by the VAR and is
        under := GEtUnderQuotaStream(pvar, underid, false, vat.PayloadConfig.filelist[overid].physical);

        if assigned(under) then
        begin
          //?????????????
          Status :='There are over-quota payloads, but  they do not have entires in the VAT.  Freeing space.';
          ZoneLog(pvar.vatindex,Status);
          //??????????????

          //move the payload from the over to the under quota stream
          Migrating := true;
          // Debug.Log(self.vat.DebugVatSTructure);
          // Debug.Log('AFter:');
          tm1 := getticker;
          MovePayloadBigBlock(pvar, overid, underid, over, under, false, PLACED_BY_QUOTA);
          tm2 := getticker;
          thr.coldruninterval := greaterof(GetTimeSince(tm2, tm1) * 1, 10);

          // Debug.Log(self.vat.DebugVatSTructure);
          // thr.RunHot := true;
        end
        else
        begin
          EvalRunHot(thr);
        end;
      end;
    end
    else
    begin
      Status := 'Nothing is migrating.';
      Migrating := false;
      //thr.coldruninterval := 10000;
      EvalRunHot(thr);
      CheckVatSanity;
    end;
  finally
    // Debug.Log('Scrub out');
    UnlockLock(l);
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
  exit;
  for t := 0 to FPayloadStreams.count - 1 do
  begin
    if FPayloadStreams[t] <> nil then
    begin
      vart := vat.FindVarWithHighestPhysicalForPayload(t);
      if vart <> nil then
      begin
        fp := vart.GetFP(t);
        if fp <> nil then
        begin
          expectedsize := vart.GetFP(t).PhysicalAddr_afterheader + GetBigBlockSizeInBytes
            (vart.FileCount, false) + sizeof(TVirtualDiskBigBlockHeader);
          str := FPayloadStreams[t];
          if str <> nil then
          begin

            if str.Size > expectedsize then begin
              str.Size := expectedsize;
              // truncate the size of the stream, if allowed
              str.gaps.length:= expectedsize;
            end
            else
              str.Collapsable := true;
          end;

        end;
      end
      else
      begin
        str := FPayloadStreams[t];
        if str <> nil then
          str.Size := 0;
      end;
    end;
  end;
end;

procedure TVirtualDisk_Advanced.SortPayloadPriorities;
var
  t, u, f, idx: fi;
  bAdded: boolean;
  minsz, minsz_gt: int64;
  thissz: int64;
  ps: TPayloadStream;
  high_u: fi;
  pri: fi;
  s: string;
begin
  setlength(FprioritySort, FPayloadStreams.count);
  idx := 0;
  high_u := 0;
  u := 0;
  minsz := -10;
  minsz_gt := -11;
  thissz := -12;
  repeat
    minsz_gt := -2;
    repeat
      bAdded := false;
      //find the minimum size that is greater than the previously added size
      minsz := -1;
      for f := 0 to high(vat.PayloadConfig.filelist) do
      begin
        pri := vat.PayloadConfig.filelist[f].priority;
        high_u := greaterof(pri, high_u);//remember the highest priority we see
        if pri = u then
        begin
          ps := FPayloadStreams[f];
          if ps <> nil then
          begin
            thissz := ps.getsize;
            if (thissz > minsz_gt) and ((thissz < minsz) or (minsz = -1)) then
              minsz := thissz;//<<--all files of this size will be added in next step
          end;
        end;
      end;


      for f := 0 to high(vat.PayloadConfig.filelist) do
      begin
        if vat.PayloadConfig.filelist[f].priority = u then
        begin
          ps := FPayloadStreams[f];
          if ps <> nil then
          begin
            thissz := ps.size;
            if thissz = minsz then begin
              FprioritySort[idx].str := FPayloadStreams[f];
              FprioritySort[idx].index := f;
              inc(idx);
              bAdded := true;
              minsz_gt := minsz;//remember our size because the next round we want to add stuff greater than this
            end;
          end;
        end;
      end;
    until not bAdded;
    inc(u);
  until u > high_u;

  IF idx =0 then begin
    s := 'idx is zero at end of priority sort! '+minsz_gt.tostring+' '+minsz.tostring+' '+thissz.tostring+' '+FPayloadStreams[0].size.tostring;
    raise Ecritical.create(s);
  end;
  setlength(FprioritySort, idx);

end;

procedure TVirtualDisk_Advanced.StartBringOnline;
begin
  FBringOnlineIdx := MAX_BLOCKS_IN_VAT - 1;
  debug.Log(self,classname + ' is starting.');
  AllowDriveSkipping := false;
end;

procedure TVirtualDisk_Advanced.StartRebuildOperation(bbid: int64;
  fromtarget: boolean);
begin
  currentRebuildOperation := Tcmd_RebuildOperation.create;
  currentRebuildOperation.d := self;
  currentRebuildOperation.bbid := bbid;
  currentRebuildOperation.fromtarget := fromTarget;
  currentRebuildOperation.Start;


end;

procedure TVirtualDisk_Advanced.StartScrubber;
begin
  ecs(csScrubber);
  try
    if scrubber = nil then
    begin
      debug.Log('starting scrubber');
      scrubber := TPM.Needthread<TExternalEVentThread>(nil);
      scrubber.Loop := true;
      scrubber.OnExecute := ScrubberExecute;
      scrubber.name := 'Scrubber for ' + FileName;
      scrubber.start;
      debug.Log('scrubber started');

      debug.Log('starting prioritizer');
      prioritizer := TPM.Needthread<TVirtualDiskPriority>(nil);
      prioritizer.vd := self;
      prioritizer.start;
      debug.Log('prioritizer started');

    end;
  finally
    lcs(csSCrubber);
  end;
end;

procedure TVirtualDisk_Advanced.StopBringOnline;
begin
  FBringOnlineIdx := 0;

end;

procedure TVirtualDisk_Advanced.StopScrubber;
begin
  ecs(csScrubber);
  try
    if assigned(scrubber) then
    begin
      debug.Log('Stopping prioritizer');
      prioritizer.stop;
      debug.Log('Stopping scrubber');
      scrubber.stop;
      debug.Log('wait for prioritizer');
      prioritizer.WaitForFinish;
      debug.Log('prioritizer Finished, sending to pool');
      TPM.NoNeedThread(prioritizer);
      debug.Log('wait for scrubber');
      scrubber.WaitForFinish;
      debug.Log('Scrubber Finished, sending to pool');
      TPM.NoNeedThread(scrubber);
      scrubber := nil;
      prioritizer := nil;
      debug.Log('Scrubber and prioritizer Stopped');
    end;
  finally
    lcs(csScrubber);
  end;
end;

procedure TVirtualDisk_Advanced.StopShipper;
begin
  if Assigned(shipper) then begin
    shipper.enabled := false;
    shipper.StopThread;
  end;
end;

function TVirtualDisk_Advanced.DebugVatSTructure: string;
begin
  result := vat.DebugVatSTructure;
  //GEtVerboseDebug('e:\shit.txt');
end;

procedure TVirtualDisk_Advanced.DecommissionPayload(sFile: string);
var
  t: ni;
  s: string;
  str: TPayloadStream;
  l: TLock;
begin
  l := GetLock;
  try
    for t := low(vat.PayloadConfig.filelist)
      to high(vat.PayloadConfig.filelist) do
    begin
      s := vat.PayloadConfig.filelist[t].NameAsString;
      str := FPayloadStreams[t];
      if filenamecompare(s, sFile) then
      begin
        if (str <> nil) and
          (not(Self.FPayloadStreams[t].Size <=
          sizeof(TVirtualDiskPayloadFileHeader))) then
        begin
          raise EUserError.Create('not empty, wait for empty and try again.');
        end
        else
        begin
          lastpayloadchange := getticker;
          vat.PayloadConfig.filelist[t].NameAsString := '';
          SaveVatAndConfig;
//          FVATStream.FinalizeBuffers;
          UpdatePayloadStreams;
          DeleteFile(s);

        end;
      end;
    end;

  finally
    UnlockLock(l);
  end;

end;

function TVirtualDisk_Advanced.DeepCheck(idx: ni): boolean;
var
  t: ni;
  a: array of byte;
begin
//  setlength(a, BIG_BLOCK_SIZE_IN_BYTES);
//  self.ReadData(idx shl BIG_BLOCK_BYTE_SHIFT, BIG_BLOCK_SIZE_IN_BYTES, @a[0]);
  result := true;

end;

destructor TVirtualDisk_Advanced.Destroy;
var
  rab: TRaidAssembledBuffer;
  itm: TRaidTreeItem;
  t: ni;
begin

  Debug.Log(self, 'Destroying '+self.classname);
  // Debug.Log('Stream is Destroyed.  Size is '+inttostr(FStream.size));
// GEtVerboseDebug('e:\shit.txt');
{$IFDEF BUFFER_BLOCKS}
  //SyncBuffer(-1, true);
{$ENDIF}
//  GEtVerboseDebug('e:\shit2.txt');
  Debug.Log(self, 'Final Buffer Flush and Destroy');
  while true do
  begin
    itm := FRaidsByBlock.FirstItem as TRaidTreeItem;
    if itm = nil then
      break;

    rab := itm.rab;
    SyncAwayBuffer(rab);
    FRaidsByBlock.Remove(itm);
    rab.free;
    rab := nil;
  end;
//  GEtVerboseDebug('e:\shit3.txt');


  FRaidsByBlock.ClearBruteForce; //wipes the root node without reblance
  FRaidsByDirtyTime.ClearBruteForce;//wipes the root node without reblance
  FRaidsByLAstUsed.ClearBruteForce;//wipes the root node without reblance

  rc.free;

  Debug.Log(self, 'Final Vat Save');
  SaveVat(true);
  Debug.Log(self, 'destroying Payload streams');
  DestroyPayloadStreams;

  FPayloadStreams.free;
  FPayloadStreams := nil;

  Debug.Log(self, 'Final Final Vat Save after close marker');
  vat.SetCloseMarker;
  SaveVat(false);
  if bRepaired then
  begin
    Debug.Log(self, 'Deleting lck file');
    if fileexists(GetfileName + '.lck') then
      DeleteFile(GetfileName + '.lck');
  end;
  FVATStream.free;
  FVATStream := nil;

  FRaidsByLastUsed.free;
  FRaidsByBlock.free;
  FRaidsByDirtyTime.free;

  FRaidsByBlock := nil;
  FRaidsByDirtyTime := nil;
  FRaidsByLastUsed := nil;

  Repairlog.free;
  Repairlog := nil;

  for t := low(FSourceArchivers) to high(FSourceArchivers) do begin
    if FSourceArchivers[t] <> nil then begin
//      FSourceArchivers[t].disconnect;
      FSourceArchivers[t].free;
      FSourceArchivers[t] := nil;
    end;
  end;

  dcs(csScrubber);



  inherited;
  rs0.free;
  rs1.free;
  rs2.free;
  rs3.free;
  rs4.free;
  rs5.free;
  rs6.free;
  rs7.free;
  rsPrefetch.free;
{$IFDEF USE_OPLOG}
  oplog.free;
  oplog := nil;
{$ENDIF}

end;

procedure TVirtualDisk_Advanced.DestroyPayloadStreams;
var
  ps: TPayloadStream;
begin
  StopBringOnline;
  StopScrubber;
  while FPayloadStreams.count > 0 do
  begin
    ps := FPayloadStreams[FPayloadStreams.count - 1];
    if ps <> nil then begin
      Debug.Log(self, 'Destroy: '+ps.filename);
      ps.free;
    end;

    ps := nil;
    FPayloadStreams.delete(FPayloadStreams.count - 1);
  end;
end;

procedure TVirtualDisk_Advanced.Detach;
begin
  if detached then
    exit;

  sfthread.Stop;
  sfthread.SafeWaitFor;
  TPM.NoNeedthread(sfthread);
  sfthread := nil;

  CompleteREbuildOperation(true);

  inherited;

end;

function TVirtualDisk_Advanced.DrainRepairLog: string;
var
  l: TLock;
begin
  l := getLock;
  try
    result := Repairlog.text;
    Repairlog.Clear;
  finally
    UnlockLock(l);
  end;
end;

procedure TVirtualDisk_Advanced.DumpBigBlock(bbid: int64);
var
  fs: TUnbufferedFileStream;
  p: pbyte;
begin
  fs := TUnbufferedFileStream.create(dllpath+inttohex(bbid,16)+'.dmp', fmCreate);
  try
    p := GetMemory(BIG_BLOCK_SIZE_IN_BYTES);
    try
      self.guaranteeReadBlocks(bbid shl BIG_BLOCK_BLOCK_SHIFT, _BIG_BLOCK_SIZE_IN_BLOCKS, p);
      stream_GuaranteewRITE(fs, p, BIG_BLOCK_SIZE_IN_BYTES);
    finally
      freememory(p);
    end;

  finally
    fs.free;
  end;

end;

procedure TVirtualDisk_Advanced.DumpCacheStatus(sl: TStringlist);
var
  t: ni;
  s: string;
begin
  Lock;
  try
    for t:= 0 to FRAidsByLastUSed.Count-1 do begin
      s := TRaidTreeItem_ByLastUsedTime(FRaidsByLAstUsed.Items[t]).rab.DebugSTring;
      sl.add(s);

    end;
  finally
    Unlock;
  end;

end;

procedure TVirtualDisk_Advanced.EvalRunHot(thr: TExternalEVentThread);
begin
  thr.RunHot := (((reads_at_last_scrub = reads) and
    (writes_at_last_scrub = writes) and Operational)) or (last_flush_status = bsFlushedSome {FRaidsByDirtyTime.count > 0}) or Migrating;
          //  ^^^^ IDLE and OPERATIONAL               or      DIRTY STUFF
end;

{$IFDEF ALLOW_RAID}

procedure TVirtualDisk_Advanced.FetchAndMergeRaid(r: TRaidAssembledBuffer);
var
  rCopied: TRaidAssembledBuffer;
  iFirstdirty, iFirstClean: ni;
  rfs:TRaidFetchStatus;
begin
  rCopied := TRaidAssembledBuffer.Create;
  try
    // save the current raid calc
    rCopied.CopyFrom(r);

    // fetch from disk (overwrites current)
    rfs := FixFetchRaid(r);

    iFirstdirty := 0;
    iFirstClean := 0;
    while iFirstClean < RAID_STRIPE_SIZE_IN_BLOCKS do
    begin
      iFirstdirty := rCopied.FirstDirty(iFirstClean);
      iFirstClean := rCopied.FirstClean(iFirstdirty);
      if (iFirstDirty >= 0 ) and (iFirstdirty < RAID_STRIPE_SIZE_IN_BLOCKS) then
      begin
        movemem32(r.byteptr(iFirstdirty * BLOCKSIZE),
          rCopied.byteptr(iFirstdirty shl BLOCKSHIFT), (iFirstClean - iFirstdirty) shl BLOCKSHIFT);
      end; //else
//        Debug.Log('CRITICAL');
    end;
    r._dirty := rCopied._dirty;
    r.AnyDirty := rCopied.AnyDirty;

  finally
    rCopied.free;
    rCopied := nil;
    // FRaids.CheckForDeadObjects;
  end;

end;

function TVirtualDisk_Advanced.GetBusiestPhysical
  (const vart: PVirtualAddressRecord): ni;
var
  t: nativeint;
  ps: TPayloadStream;
  max, mine: nativefloat;
  fid: ni;
begin
  max := -1;
  result := 0;
  for t := 0 to vart.FileCount - 1 do
  begin
    fid := vart.FPs[t].FileID;
    if ValidPayLoadID(fid) then
    begin
      result := fid;
      break;
    end;
    ps := FPayloadStreams[fid];
    if ps = nil then
    begin
      result := t;
      break;
    end
    else
    begin
{x$DEFINE DONT_USE_QUEUED}
{$IFDEF DONT_USE_QUEUED}
     mine := ps.DirtyBufferCount;
{$ELSE}
{$IFDEF PAYLOAD_HAS_QUEUED_INTERFACE}
     mine := (ps.estimated_queue_size + 1) * ps.stats.PeriodicAverage;
{$ELSE}
     mine := random(vart.FileCount-1);
{$ENDIF}
{$ENDIF}

      if mine > max then
      begin
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
  max := 99999999;
  result := 0;
  for t := 0 to vat.PayloadCount - 1 do
  begin
    fid := t;
    if validPayloadid(fid) then
    begin
      result := fid;
      break;
    end;
    ps := FPayloadStreams[fid];
    if ps = nil then
    begin
      result := t;
      break;
    end
    else
    begin
{$IFDEF DONT_USE_QUEUED}
     mine :=  ps.DirtyBufferCount;
{$ELSE}
{$IFDEF PAYLOAD_HAS_QUEUED_INTERFACE}
     mine := round((ps.estimated_queue_size + 1) * ps.stats.PeriodicAverage);
{$ELSE}
    mine := random(vat.FileCount);
{$ENDIF}
{$ENDIF}
      if mine < max then
      begin
        result := t;
        max := mine;
      end;
    end;
  end;

end;

function TVirtualDisk_Advanced.GEtCachedSTripes: ni;
begin
  result := FRaidsByBlock.count;

end;

var
  raidlock: ni = 0;
function TVirtualDisk_Advanced.TryFetchRaid(r: TRaidAssembledBuffer): TRaidFetchStatus;
var
  t: ni;
  ps: TPayloadStream;
  fp: PFilePhysical;
  bo, so: int64;
  bs: int64;
  tt: ni;
  byte_off: int64;
  a: array [0 .. 31] of TQueueItem;
  waithandles: array[0..31] of THandle;
  //handleidx: array[0..31] of ni;
  localvat: PVirtualAddressRecord;
  driveToSkip: ni;
  tidx: ni;
  hidx: ni;
  seekpos: int64;
  bFlushimmediate: boolean;
  tmStart, tmEnd: ticker;
  bWasNewBlock: boolean;
  rr: Pbyte;
  tmStartRaid: ticker;
begin
//  rs1.BeginTime;
//  rs4.begintime;
  if raidlock > 0 then
    raise ECritical.create('shit is fucked up by '+inttostr(raidlock));

  raidlock := getcurrentThreadid;


  try
    result := TRaidFetchStatus.rsCritical;
  //  if r.StartingBlock  = 0 then
  //    Debug.Log('HERE!');
    hidx := 0;
    bWasNewBlock := false;
  //  rs5.BeginTime;
  //  rs5.EndTime;
  //  if rs5.NewBAtch then
  //    Debug.Log('RC Reset Time: '+rs5.DebugTiming);
    bFlushimmediate := false;
    tidx := r.StartingBlock shr BIG_BLOCK_BLOCK_SHIFT; //div _BIG_BLOCK_SIZE_IN_BLOCKS;

    BringBigBlockOnline(tidx);

    localvat := @vat.table[tidx];
{$IFDEF DETAILED_DEBUGGING}
    if localvat.HAsFileCount(-1) > 0 then begin
      Debug.Log('Read with parity rebuild. '+vat.debugvatstructure);
    end;
{$ENDIF}

    //xxx- PrefetchAllPayloads(r.startingblock shl BLOCKSHIFT,true,false);

    if localvat.startingblock <> (tidx shl BIG_BLOCK_BLOCK_SHIFT) then begin
      LogRepair(localvat.vatindex, 'Fixing vat @'+inttohex(FBringOnlineIdx,1)+', starting block doesn''t match index');
      localvat.startingblock := (tidx shl BIG_BLOCK_BLOCK_SHIFT);
      vat.MarkTableEntryDirtyByPtr(localvat);
    end;


  //  rs6.begintime;
    if localvat.HasUndefinedLocations then
    begin

      if not bInReconstitution then begin
        Debug.log(self, 'Raid has undefined locations, force reconstitution: '+localvat.debugstring);
        ForceReconstitution(localvat);
      end;
    end;

  //  rs6.EndTime;
  //  if rs6.NewBAtch then
  //    Debug.Log('Check Defined Time: '+rs6.DebugTiming);



  //  rs7.begintime;
    r.PayloadSizeInBytes := RAID_STRIPE_SIZE_IN_BYTES;
    // if buffer.startingBlock < FCurrentVat.StartingBlock then
    // raise ECritical.create('something amiss');

    if localvat.FileCount <= 0 then
    begin
      NewPhysicalBlockAddress(BigBlockAlign(r.StartingBlock * BLOCKSIZE), localvat.priority_target, localvat.priority_target, PLACED_BY_EXPAND);
      //The new data pointers were created, but we need to see if this is
      //available in the backup
      bWasNewBlock := true;
    end;



    // if buffer.startingBlock < FCurrentVat.StartingBlock then
    // raise ECritical.create('something amiss');

    // fillmem(@a[0], sizeof(a),0);
    driveToSkip := -1;
    if AllowDriveSkipping then
    begin
      if localvat.FileCount > 1 then
        driveToSkip := GetBusiestPhysical(localvat)
    end;


    // determine the block offset in the big-block
    bo := r.StartingBlock - localvat.StartingBlock; // block offset
    if (bo < 0) then
      raise ECritical.create('bo is < 0, this data wouldn''t belong here');
    // determine STRIPE offset (in number of stripes)
    so := bo shr STRIPE_SHIFT;

  {$IFDEF DETAILED_DEBUGGING}
    Debug.log(self, 'Fetch raid: so='+inttostr(so)+' '+localvat.debugstring);
  {$ENDIF}


    //CANCEL ALL PREFETCHES!  else we might slow down uncomfortably
    if assigned(self.sfthread) then
      self.sfthread.runhot := false;
    for t := 0 to localvat.FileCount - 1 do
    begin
      // get the file-physical record
      fp := @localvat.FPs[t];

      // get the payload stream
      if fp.FileID > -1 then
      begin
        ps := FPayloadStreams[fp.FileID];
        if ps = nil then continue;
        {$IFDEF PAYLOAD_IS_UNBUFFERED}
        ps.CancelPrefetches;
        {$ELSE}
  //      TUnbufferedFileSTream(ps.understream).allowed_prefetches := 0;
        {$ENDIF}
      end;
    end;

    for t := 0 to localvat.FileCount - 1 do begin
      rc.pieceDebug[t].Init;
    end;
  //  rs7.EndTime;
  //  if rs7.NewBAtch then
  //    Debug.Log('Prefetch Setup Time: '+rs7.DebugTiming);



    rc.REset;
    for t := 0 to localvat.FileCount - 1 do
    begin
      ps := nil;

      if t = driveToSkip then
        continue;

      // get the file-physical record
      fp := @localvat.FPs[t];

      // determine the block-size for the RAID pieces
      bs := rc.GetPieceSizePAdded(localvat.FileCount);
      a[t] := nil;

      // get the payload stream
      if fp.FileID < 0 then begin
        rc.invalid_piece := t;
        rc.invalid_determined := true;
      end else
      begin
        ps := FPayloadStreams[fp.FileID];

        if ps <> nil then
        begin



          // determine the byte offset in the big-block
          byte_off := bs * so;
          if (byte_off < 0) then
            raise ECritical.Create('checkit: byte_off < 0.  Bad.');

          // ps.LockSize;
          try
  {$IFDEF USESEEKLOCK} ps.SeekLock; {$ENDIF}
            try
              // seek to [start of the physical location] + [byte offset post raid]
              seekpos := fp.PhysicalAddr_afterheader + byte_off;
              if (seekpos < 0) or (seekpos >= ps.size) then
                  raise Ecritical.create('BAD SEEK POSITION: '+inttostr(seekpos));
  {$IFNDEF ASYNC_READS}
              ps.Seek(seekpos, soBeginning);
  {$ENDIF}
                rc.pieceDebug[t].piece_idx := t;
                rc.pieceDebug[t].big_block_index := tidx;
                rc.pieceDebug[t].PayloadAddr := ps.Position;
                rc.pieceDebug[t].FileOrigin := ps.FileNAme;
                rc.pieceDebug[t].BigBlockFileBaseAddr :=  fp.PhysicalAddr_afterheader;
              // write piece of data to physical location
              if not ps.IsAfterEOF(seekpos) then
              begin
  {$IFDEF DETAILED_DEBUGGING}
                if EnableOptionalDebugging then
                  optdebug('buffer.startingblock=' + inttostr(r.StartingBlock) +
                    ' will be fetched to raid[' + inttostr(fp.FileID) +
                    '] from physical=0x' + inttohex(ps.Position, 0))
                else if r.StartingBlock = getraidbase(380249) then
                begin
                  debug.Log(self,'buffer.startingblock=' + inttostr(r.StartingBlock) +
                    ' will be fetched to raid[' + inttostr(fp.FileID) +
                    '] from physical=0x' + inttohex(seekpos, 0))
                end;

  {$ENDIF}
  {$IFDEF ASYNC_READS}
  {$IFDEF DETAILED_DEBUGGING}
                Debug.Log(self, 'Beginning Adaptive Read for FetchRaid piece['+inttostr(t)+'] seek='+inttohex(fp.PhysicalAddr_AfterHeader + byte_off,0)+' length='+inttohex(bo,0)+' end='+inttohex(fp.PhysicalAddr_AfterHeader + byte_off+bo,0)+' stream size='+inttohex(ps.size,0));
  {$ENDIF}

                a[t] := ps.BeginRead(seekpos,pbyte(@rc.pieces[t]), bs, {$IFDEF ALLOW_PRIMARY_READS_TO_BE_SYNCHRONOUS}false{$ELSE}true{$ENDIF});
  //              a[t] := ps.BeginAdaptiveRead(pbyte(@rc.pieces[t]), bs, {$IFDEF ALLOW_PRIMARY_READS_TO_BE_SYNCHRONOUS}false{$ELSE}true{$ENDIF});
                waithandles[hidx] := a[t].FWaitSignal.Handle;
                //handleidx[hidx] := t;
                //a[t].tmUserMarker := GetHighResTicker;
                inc(hidx);
  //              if (t > 1) and (a[t-2] <> nil) then
  //                a[t-2].waitfor;
  {$ELSE}
                Stream_GuaranteeRead(ps, @rc.pieces[t], bs);
  {$ENDIF}
  {$IFDEF DETAILED_DEBUGGING}
  {$ENDIF}
              end
              else
              begin
                fillmem(pbyte(@rc.pieces[t]), bs, 0);
                a[t] := nil;
              end;
            finally
  {$IFDEF USESEEKLOCK} ps.seekunlock; {$ENDIF}
            end;
          finally
            // ps.UnlockSize;
          end;
        end;
      end;

      if ps = nil then
      begin
        fillmem(pbyte(@rc.pieces[t]), bs, 0);
      end;

    end;


  //  rs4.EndTime;
  //  if rs4.NewBAtch then
  //    Debug.Log('Pre Read Time: '+rs4.DebugTiming);

  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


  {$IFDEF ASYNC_READS}
  //  Self.TryHintNExtBigBlock(localvat.GetVatIndex+1, localvat);
    //WaitForMultipleObjects(hidx, @waithandles[0], true, $FFFFFFFF);
  //  rs2.begintime;
    for t := 0 to localvat.FileCount - 1 do
    begin
      if t = driveToSkip then
        continue;

      if a[t] = nil then
        continue;
      a[t].waitfor;

      fp := @localvat.FPs[t];
      if fp.FileID < 0 then
        continue;

      if assigned(a[t]) then
      begin
        a[t].free;
        a[t] := nil;
      end;
    end;
  //  rs2.EndTime;
  //  if rs2.NewBAtch then
  //    Debug.Log('Stream Wait Time: '+rs2.DebugTiming);

  {$ENDIF}
  //  rs3.BeginTime;
    // calculate result
    rc.Drives := localvat.FileCount;
    rc.Skipped_Read := driveToSkip;
    if not rc.invalid_determined then
      rc.DetermineInvalidPiece(driveToSkip);
    rc.assembled := r;
    bFlushImmediate := false;

    // BAD PIECE FOUND{BPF}

    result := rsSuccess;
    //if >= 1 bad pieces found
    //or >= 2 bad pieces found (with drive skipping enabled)
    if (rc.invalid_count > BoolToint(AllowDriveSkipping)) then
    begin

      { BPF }
      if ((rc.invalid_count - BoolToint(AllowDriveSkipping)) <=1) and (rc.drives > 1)  then
      begin
        bFlushImmediate := true;
        if driveToSKip >=0 then begin
          //FETCH OUR FINAL (skipped) drive
          { BPF } // determine the block-size for the RAID pieces
          { BPF } bs := rc.GetPieceSizePAdded(localvat.FileCount);
          { BPF } // determine the block offset in the big-block
          { BPF } bo := r.StartingBlock - localvat.StartingBlock; // block offset
          { BPF } // determine STRIPE offset (in number of stripes)
          { BPF } so := bo shr STRIPE_SHIFT; // so := bo shr STRIPE_SHIFT;
          { BPF } // determine the byte offset in the big-block
          { BPF } byte_off := bs * so;
          { BPF } fp := @localvat.FPs[driveToSkip];
          { BPF } ps := FPayloadStreams[fp.FileID];
          { BPF } if ps = nil then
          { BPF } begin
          { BPF }   FopStatus := 'Broken raid';
          { BPF }   FOperational := false;
          { BPF }   exit;
          { BPF } end;
          { BPF } seekpos := fp.PhysicalAddr_afterheader + byte_off;
          { BPF } if (seekpos < 0) or (seekpos >= ps.size) then
                    raise Ecritical.create('BAD SEEK POSITION: '+inttostr(seekpos)+'  physaddr = '+inttostr(fp.PhysicalAddr_afterheader)+' byte_off='+inttostr(byte_off)+ ' driveToSkip='+inttostr(drivetoskip));
          { BPF } ps.Seek(seekpos, soBeginning);

    {$IFDEF ASYNC_READS}
          { BPF } tmStart := GetHighResTicker;
          { BPF } a[driveToSkip] := ps.BeginAdaptiveRead
            (pbyte(@rc.pieces[driveToSkip]), bs, true);
          { BPF } a[driveToSkip].WAitFor;
          { BPF } a[driveToSkip].free;
          { BPF } a[driveToSkip] := nil;
          { BPF } tmEnd := GetHighResTicker;
          { BPF } ps.stats.AddStat(tmEnd - tmStart);

    {$ELSE}
          Debug.Log(self, 'Repairing BAD RAID PIECE!');
          { BPF } tmStart := GetHighResTicker;
          { BPF } Stream_GuaranteeRead(ps, @rc.pieces[driveToSkip], bs);
          { BPF } tmEnd := GetHighResTicker;
          { BPF } ps.stats.AddStat(tmEnd - tmStart);

    {$ENDIF}
        end;

        result := TRaidFetchStatus.rsRepaired;
        if rc.invalid_piece >= 0 then begin
          Debug.Log('not sure why I''m here');
{$IFDEF KILL_BB_ON_BROKEN_RAID}
          Debug.Log('Marking Piece '+rc.invalid_piece.tostring+' invalid (-1)');
          localvat.FPs[rc.invalid_piece].FileID := -1;
          vat.MarkTableEntryDirtyByPtr(localvat);
          SaveVat(false);
{$ENDIF}
          tmStartRAID := GetTicker;
  //        while self.RaidUP do begin
  //          Debug.Log('Raid up');
  //          if gettimesince(tmstartRAID) > 1000 then
  //            break;
  //        end;

        end;
      end
      else
      begin
        FopStatus := 'Broken raid';
        // Foperational := false;
        Debug.Log('Recover From Archive!!!!!');
        if (shipper.Archive <> '') and Shipper.Enabled then begin
          shipper.pauseforfetch := true;
          try
            try
              Debug.Log('Fetch Log for stripe repair @block '+inttohex(r.startingblock,1));
              while not shipper.FetchLog(r.StartingBlock, RAID_STRIPE_SIZE_IN_BLOCKS,r.byteptr(0)) do begin
                FopStatus := 'Broken raid';
                Debug.Log('Retry Fetch Log');
                sleep(1000);
              end;
              r.SetDirtyRange(0, RAID_STRIPE_SIZE_IN_BLOCKS);
              r.ReadFromDisk := true;
              flushRaid(r);
              result := rsRecoveredFromArchive;
            finally
            end;
          finally
            shipper.pauseforfetch := false;
          end;
        end;
        exit;////<<---------------------EARLY EXIT
      end;
      rc.invalid_determined := false;
    end;


    rc.PiecesToSingle;
    if r.StartingBlock =   1713351168 then
      Debug.Log('CS: '+commaize(r.GetChecksum));


    r.ReadFromDisk := true;


    if bFlushimmediate then begin
      Debug.Log(self, 'Writing reconstructed RAID piece');
      FlushRaid(r);
    end;

    PrefetchAllPayloads(((r.startingblock) shl BLOCKSHIFT)+RAID_STRIPE_SIZE_IN_BYTES,false,false);
  //  rs3.EndTime;
  //  if rs3.NewBAtch then
  //    Debug.Log('Post Read Time: '+rs3.DebugTiming);
  finally
//    rs1.EndTime;
//    if rs1.NewBAtch then
//      Debug.Log('Raid Fetch Time: '+rs1.DebugTiming);
    if raidlock <> GetCurrentThreadID then
      raise ECRitical.create('more shit is fucked up');
    raidlock := 0;
  end;

end;
{$ENDIF}
{$IFDEF ALLOW_RAID}

function TVirtualDisk_Advanced.FindHighestOverVar(FileID: ni; out bOver: boolean)
  : PVirtualAddressRecord;
var
  t: ni;
  pvar: PVirtualAddressRecord;
  pf: PFilePhysical;
  x: int64;
  result_pf: PFilePhysical;
begin
  result_pf := nil;
  result := nil;
  bOver := false;

  pvar := vat.FindVarWithHighestPhysicalForPayload(FileID);
  if pvar <> nil then begin
    pf := pvar.GetFP(fileid);
    if pf = nil then exit;

    result_pf := pf;
    result := pvar;

    x := pf.PhysicalAddr_afterheader+getbigblocksizeinbytes(pvar.filecount)+(sizeof(TvirtualDiskBigBlockHeader)*1);
    if (x > vat.PayloadConfig.filelist[FileID].size_limit)
    or (vat.PayloadConfig.filelist[FileID].priority < 0)
    then begin
      bOver := true;
    end;
  end;
  exit;

  for t := low(vat.table) to high(vat.table) do
  begin
    pvar := @vat.table[t];
    pf := pvar.GetFP(FileID);
    if pf <> nil then
    begin
      if (result = nil) or
        ((result_pf <> nil) and (pf.PhysicalAddr_afterheader > result_pf.PhysicalAddr_afterheader)) then
      begin
        if (result = nil) or (result_pf = nil) then
        begin
          result := pvar;
          result_pf := pf;
        end
        else
        begin
          if (pf.PhysicalAddr_afterheader >= vat.PayloadConfig.filelist[result_pf.FileID]
            .size_limit) or (vat.PayloadConfig.filelist[result_pf.FileID]
            .priority < 0) then
          begin
            if ((pf.PhysicalAddr_afterheader > result_pf.PhysicalAddr_afterheader)) then begin
              result := pvar;
              result_pf := pf;
            end;
          end;
        end;
      end;

    end;
  end;

end;

function TVirtualDisk_Advanced.FindHighestOverVar_AfterCollapse(FileID: ni)
  : PVirtualAddressRecord;
var
  t: fi;
  bOver: boolean;
begin
  Collapse(FileID);
  result := FindHighestOverVar(FileID, bOver);
  if not bOver then
    result := nil;

end;

function TVirtualDisk_Advanced.FixFetchRaid(
  r: TRaidAssembledBuffer): TRaidFetchStatus;
begin
  result := TryFetchRaid(r);
  if result <> rsSuccess then
    result := TryFetchRaid(r);
end;

{$ENDIF}
{$IFDEF ALLOW_RAID}

function TVirtualDisk_Advanced.FlushBuffersInBlockOrder(iStartingBlock: int64; bOnlyIfAllDirty: boolean; bOnlyIfDiskInactive: boolean): TBufferStatus;
var
  itm, next: TRaidTreeItem_byBlock;
  tm: ticker;
  vart: PVirtualAddressRecord;
  maxq: ni;
  t: ni;
  ps: TPayloadStream;
begin
  result := bsNoBuffersToFlush;
  if bOnlyIfDiskInactive then begin
    if (hint_requests_waiting or (queue.estimated_queue_size > 0))  then begin
      exit(bsdiskTooActive);
    end;
  end;
  tm := GEtTicker;
  repeat
    itm := TRaidTreeItem_ByBlock(FRaidsByBlock.Search_StartingBlock_Exact(iStartingBlock));
    if itm = nil then break;

    //determine if QUEUE is filling up at target payloads
    vart := @vat.table[itm.rab.vatindex];
    maxq := 0;
    if vart.filecount > 0 then begin
      for t:= 0 to vart.filecount-1 do begin
        ps := self.FPayloadStreams[t];
        if ps = nil then
          maxq := 999999
        else begin
          maxq := greaterof(maxq,ps.estimated_queue_size);
        end;
      end;
    end;

    if maxq > queuestream.UNBUFFERED_BUFFERED_PARTS shr 2 then
      break;



//    if TRaidTreeItem_byBlock(itm).rab.StartingBlock = self.primary_buffer.StartingBlock then
//      exit;

    if itm.rab.AnyDirty then begin
      if (SyncAwayBuffer(itm.rab, bOnlyIfAllDirty)) then begin
        //FRaidsByDirtyTime.Remove(itm.rab.treeItemDirtyTime, true);//!No - done by sync away buffer under lock
        result := bsFlushedSome;
      end;
    end else begin
      break;
    end;
    inc(iStartingBlock, RAID_STRIPE_SIZE_IN_BLOCKS);
    if (hint_requests_waiting or (queue.estimated_queue_size > 0)) {and (gettimesince(tm) > 100)} then begin
      exit(bsDisktooActive);
    end;
  until itm = nil;
end;

function TVirtualDisk_Advanced.FlushBuffers(bForceLock: boolean; iOlderThan: ticker;
  bFindAllDirty: boolean = false; bOnlyIfALLDirty: boolean = false; bOnlyIfDiskInactive: boolean = true): TBufferStatus;
var
  next, itm, last: TRaidTreeItem_ByDirtyTime;
  c: TRaidAssembledBuffer;
  t: ni;
  tmNow: ticker;
  cnt: ni;
  l: TLock;
  bGot: boolean;
  bs: TBufferStatus;
const
  MAX_FLUSH_LOCK_TIME = 9;
begin
  result := bsNoBuffersToFlush;
  {$DEFINE ALWAYS_FORCE_FLUSH_LOCK}
  {$IFNDEF ALWAYS_FORCE_FLUSH_LOCK}
  if bForceLock then begin
  {$ENDIF}
    bGot := true;
    l := GetLock();
  {$IFNDEF ALWAYS_FORCE_FLUSH_LOCK}
  end else begin
    bGot := tryGetLock(l);
  end;
  {$ENDIF}

  if bGot then
  begin
    try
      tmNow := getticker;
      cnt := FRaidsByDirtyTime.count;

//      debug.Log('there are reportedly '+inttostr(cnt)+' buffers to flush.');

      last := nil;
      itm := FRaidsByDirtyTime.firstitem;



      if itm = nil then begin
        result := bsNoBuffersToFlush;
        last_buffer_count := 0;
        exit;

      end;
      if bFindAllDirty then
      while not itm.rab.AllDirty do begin
        itm := TRaidTreeItem_ByDirtyTime(itm.NExt);
        if itm = nil then begin
          result := bsNoBuffersToFlush;
          exit;
        end;
      end;

      c := itm.rab;
//      if cnt = last_buffer_count then begin
//        while (tmNow - c.DirtyTime) < iOlderthan do begin
//          iOlderThan := iolderthan - 1000;
//          last_buffer_count := 0;
//        end;
//      end;


      while true do begin
        if itm = nil then break;

        if itm = last then
          break; // we're spinning our wheels... get out of here, there's nothing to do

        last := itm;
        next := TRaidTreeItem_ByDirtyTime(itm.next);//<!!!----- it is important to get the next item before doing the flush, because we will end up with links disconnected

        c := itm.rab;

        last_buffer_count := cnt;


        if not c.AnyDirty then begin
          debug.Log('clean buffer in dirty list');
          FRaidsByDirtyTime.Remove(itm, true);
          itm := next;
        end;
        if (tmNow - c.dirtytime) >= iOlderThan then
        begin
{$IFNDEF WALK_FLUSH}
          if SyncAwayBuffer(c, bOnlyIfALLDirty { only if all dirty } ) then begin
            //FRaidsByDirtyTime.Remove(itm, true); //no! done by syncawaybuffer under lock
          end;
{$ELSE}
          bs := FlushBuffersInBlockOrder(c.StartingBlock, bOnlyIfAllDirty, bOnlyIfDiskInactive);
          result := bs;
          if bs <> bsFlushedSome then begin
            if bs <> bsNoBuffersToFlush then begin
              if SyncAwayBuffer(c, bOnlyIfAllDirty) then
                result := bsFlushedSome
              else
                result := bsNoBuffersToFlush;
              exit;
            end;

          end;

          break;

{$ENDIF}

          // ^^^ syncAWayBuffer will cause the linked list to resort for time, so LastItem should change (in theory)
          itm := next;
        end else
          if cnt > 64 then
            break; //break any time the buffers are not old enough


        if gettimesince(tmNow) > MAX_FLUSH_LOCK_TIME then begin
          result := bsFlushingTakingTooLong;
          break;
        end;
      end;
      result := bsFlushedSome;
    finally

      UnlockLock(l);
    end;
  end
  else begin
    result := bsLockWasBusy;
  end;
end;

procedure TVirtualDisk_Advanced.FlushRaid(r: TRaidAssembledBuffer);
var
  t: ni;
  ps: TPayloadStream;
  fp: PFilePhysical;
  bo, so: int64;
  bs: int64;
  byte_off: int64;
  localvat: PVirtualAddressRecord;
  newfunct_id: ni;
  newfunct_stream: TPayloadStream;
  tidx, tt: ni;
  tmStart, tmEnd: ticker;
  bRetry: boolean;
begin

  if r.Archive then begin
    if r.AnyDirty then begin
      zonerevs.IncrementZoneRevForStartingBlock(r.StartingBlock);
      if shipper <> nil then
        if shipper.enabled then
          shipper.LogThis(r.StartingBlock, RAID_STRIPE_SIZE_IN_BLOCKS, pbyte(@r.single[0]));
    end;
    r.Archive := false;
  end;


//   if r.StartingBlock = 0 then begin
//     debug.Log('trap raid flush');
//   end;

  // calculate the RAID
  if r.StartingBlock < 0 then
  begin
    r.Init;
    exit;
  end;

  tidx := r.StartingBlock SHR BIG_BLOCK_BLOCK_SHIFT;
  BringBigBlockOnline(tidx);

  rc.REset;
  if tidx >= MAX_BLOCKS_IN_VAT then
    raise ECritical.Create('wtf');
  localvat := @vat.table[tidx];
  if localvat.StartingBlock < 0 then
    localvat.StartingBlock := tidx * _BIG_BLOCK_SIZE_IN_BLOCKS;

  r.PayloadSizeInBytes := RAID_STRIPE_SIZE_IN_BYTES;
  if localvat.FileCount <= 0 then
  begin
    NewPhysicalBlockAddress(BigBlockAlign(r.StartingBlock * BLOCKSIZE), localvat.priority_target,localvat.priority_target, PLACED_BY_EXPAND);
  end;

  PrefetchAllPayloads(r.StartingBlock shl BLOCKSHIFT, true,true);

  rc.Drives := localvat.FileCount;
  rc.assembled := r;
  for t := 0 to localvat.FileCount - 1 do begin
    rc.pieceDebug[t].Init;
    fp := @localvat.FPs[t];
    if fp.fileid >= 0 then begin

      ps := FPayloadStreams[fp.fileid];
      rc.pieceDebug[t].piece_idx := t;
      rc.pieceDebug[t].FileOrigin := ps.FileName;
      rc.pieceDebug[t].big_block_index := tidx;
      rc.pieceDebug[t].PayloadAddr := -1;
      rc.pieceDebug[t].BigBlockFileBaseAddr := fp.PhysicalAddr_afterheader;
    end;
  end;

  rc.SingleToPieces;


  repeat
    bRetry := false;
    for t := 0 to localvat.FileCount - 1 do
    begin
      fp := @localvat.FPs[t];
      if fp.FileID < 0 then begin
        NewPhysicalBlockAddress(BigBlockAlign(r.StartingBlock * BLOCKSIZE),localvat.priority_target,localvat.priority_target, PLACED_BY_TABLEERR0);
        bRetry := true;
        break;
      end;
      ps := FPayloadStreams[fp.fileid];
      if ps = nil then begin
        Debug.Log(self, 'Payload stream is nil so marking piece invalid (-1)');
        fp.FileID := -1;
        NewPhysicalBlockAddress(BigBlockAlign(r.StartingBlock * BLOCKSIZE),localvat.priority_target,localvat.priority_target, PLACED_BY_TABLEERR1);
        bRetry := true;
        break;
      end;
      if ((fp.PhysicalAddr_afterheader-sizeof(TVirtualDiskbigblockheader))+GetBigBlockSizeInBytes(localvat.filecount,true)) > ps.Size then begin
        NewPhysicalBlockAddress(BigBlockAlign(r.StartingBlock * BLOCKSIZE),localvat.priority_target,localvat.priority_target, PLACED_BY_TABLEERR2);
        bRetry := true;
        break;
      end;
    end;
  until not bRetry;

  for t := 0 to localvat.FileCount - 1 do
  begin
    // get the file-physical record
    fp := @localvat.FPs[t];

    // get the payload stream
    if fp.FileID < 0 then
    begin
      ps := nil;
    end
    else
    begin
      ps := FPayloadStreams[fp.FileID];
    end;

    if ps = nil then
    begin
      // raise ECritical.create('wtf');
      // this piece cannot be flushed
      // find new functional piece
      ps := GEtUnderQuotaStream(localvat, newfunct_id, false);
      if ps = nil then
      begin
        FOperational := false;
        FopStatus := 'need new payloads for reconsitution of bad blocks';
        continue; // our data is corrupt
      end;
    end;

    // determine the block offset in the big-block
    bo := r.StartingBlock - localvat.StartingBlock; // block offset
    if (bo < 0) or (bo >= _BIG_BLOCK_SIZE_IN_BLOCKS) then
    begin
      FopStatus := 'block offset invalid ' + inttostr(bo);
      FOperational := false;
      ForceReconstitution(localvat);

    end;

    // determine the block-size for the RAID pieeces
    bs := rc.GetPieceSizePAdded(localvat.FileCount);

    // determine STRIPE offset (in number of stripes)
    so := bo shr STRIPE_SHIFT;

    // determine the byte offset in the big-block
    byte_off := bs * so;

{$IFDEF USESEEKLOCK} ps.SeekLock; {$ENDIF}
    try
      // seek to [start of the physical location] + [byte offset post raid]

//      if r.StartingBlock = 1664 then
//        debug.Log('trap raid flush');


      // write piece of data to physical location

//      if r.StartingBlock = 1664 then begin
//        debug.Log('trap raid flush 2');
//        debug.log(memorydebugstring(pbyte(@rc.pieces[t])+48128, 512),'');
//      end;

      tmStart := GetHighResTicker;
      {$IFNDEF PAYLOAD_IS_UNBUFFERED}
//      ps.Lock;
      try
        ps.Seek(fp.PhysicalAddr_afterheader + byte_off, soBeginning);
        Stream_GuaranteeWrite(ps, pbyte(@rc.pieces[t]), bs);
      finally
        //ps.Unlock;
      end;

      {$ELSE}
      ps.WriteBehind(fp.PhysicalAddr_afterheader + byte_off, pbyte(@rc.pieces[t]), bs);
      ps.prefetchwritemode := true;
      ps.prefetchposition := fp.PhysicalAddr_afterheader+byte_off;
      {$ENDIF}
      tmEnd := GetHighResTicker;
      ps.stats.AddStat(tmEnd - tmStart);
    finally
{$IFDEF USESEEKLOCK} ps.seekunlock; {$ENDIF}
    end;
  end;
  PrefetchAllPayloads((r.StartingBlock shl BLOCKSHIFT) + RAID_STRIPE_SIZE_IN_BYTES, false,true);

  //r.ClearDirty;
end;

function TVirtualDisk_Advanced.ForceReconstitution
  (pvar: PVirtualAddressRecord): boolean;
begin
  result := Reconstitute(pvar);
end;

procedure TVirtualDisk_Advanced.Front_CalculateRecommendedPrefetches;
var
  rec,cx, mx: ni;
  r: single;
  bt: TRaidTreeItem_ByLastUsedTime;
begin
  mx := front_RecommendedPrefetchAllowance;
  r := 0;
  cx := 256;
  bt := FRaidsByLAstUsed.LastItem;
  while cx > 0 do begin
    if bt.rab.legit then
      r := r + 1;
    bt := TRaidTreeItem_ByLastUsedTime(bt.Prev);
    if bt = nil then
      break;
    dec(cx);
  end;
  r := r/256;

  if (r) > 0.50 then
    inc(mx)
  else
    mx := round(mx * +r);

  mx := greaterof(1, lesserof(CachedSTripes shr 8, mx));
  mx := g_FORCE_VD_PREFETCH;
  front_RecommendedPrefetchAllowance := mx;
//  sfthread.stepcount := mx;

end;


procedure TVirtualDisk_Advanced.Front_CancelPrefetches;
var
  was: ni;
  l: TLock;
begin
  exit;
  l := GetLock;
  try
    was := front_allowed_prefetches;
    front_allowed_prefetches := 0;
  finally
    unlocklock(l);
  end;
//  sfthread.waitforidle;
end;

procedure TVirtualDisk_Advanced.Front_StartPrefetches;
var
  l: TLock;
begin
//  l := GetLock;
  try
  front_allowed_prefetches := FRont_RecommendedPrefetchAllowance;
  if front_allowed_prefetches > 0 then begin
    sfthread.runhot := true;
    sfthread.haswork := true;
  end;
  finally
//    UnlockLock(l);
  end;
end;

{$ENDIF}

//Score/Ratio:
//Sort VAT table by size (but maybe not actually sort it because RAM allocation might lead to swapping)
//Count number of big blocks that might fit into tier 0
//Set priority_target on table


procedure TVirtualDiskPriority.AssignPriority(iPriority, iCount: int64);
var
  cx: int64;
begin
  if nextindex >= MAX_BLOCKS_IN_VAT then
    exit;

  cx := iCount;
  while cx > 0 do begin
    tablecopy[nextindex].rec.priority_target := iPriority;
    vd.vat.MarkTableEntryDirty(tablecopy[nextindex].startingblock shr BIG_BLOCK_BLOCK_SHIFT);
    vd.vat.MarkTableEntryDirty(nextindex);
    //Debug.Log('Mark '+inttohex(nextindex,1)+' dirty');
//    if cx mod 100 = 0 then begin
//      Debug.Log('Save'+inttostr(cx));
//      vd.SaveVat(false);
//    end;
  //vd.vat.MarkTableEntryDirtyByPtr(tablecopy[nextindex].rec);
    inc(nextindex);
    dec(cx);
  end;

end;

function TVirtualDiskPriority.ContinueReprioritize: TRepResult;
var
  localvat: PVirtualAddressRecord;
  fp: PFilePhysical;
  ps: TPayloadStream;
  pri, pri_target, fid: ni;
  l: TLock;
begin
{$IFDEF DONT_PRIORITIZE}
  sleep(100);
  exit;
{$ENDIF}

  try
    if not enable then exit(TRepResult.repNothingToDo);

    //VOLATILE read because seeking this pointer should not change at any time
    // the disk is running
    localvat := Self.tablecopy[nextindex].rec;

    if localvat.filecount <= 0 then
      exit(repNothingToDo);

    //check if the priority has changed
    fid := localvat.FPs[0].FileID;//look at the first payload's priority for the zone

    //if not provisioned then exit
    if fid < 0 then
      exit(repNothingToDo);


    //determine the CURRENT priority of the first payload in the zone
    pri := payloadpriorities[fid];
    //determine the TARGET prioirty of the the zone
    pri_target := Self.tablecopy[nextindex].rec.priority_target;

    if stage = psDowngradeFrom then begin
      if pri >= workingpriority then
        exit(repNothingToDo);
      if pri_target >= pri then
        exit(repNothingToDo);
    end;
    if stage = psUpgradeTo then begin
      if pri_target <> workingpriority then
        exit(repNothingToDo);
      if pri_target > pri then
        exit(repNothingToDo);
    end;

    //if they are then same, then do nothing
    if pri = pri_target then
      exit(repNothingToDo);

{x$IFDEF NU}
    // i think this is an experiment to try and skip over continually failing reconsts
    if pri = skip_priority then
      exit(repNothingToDo);
{x$ENDIF}


    //reconstitute this block (upon allocation of new physical, priority_target will be taken into account);
    step := nextindex;
    stepcount := MAX_BLOCKS_IN_VAT;
    //YES we will reconstitute this zone
    status := 'Reconstitute '+inttostr(nextindex);
    zl.log(nextindex, nextindex.tohexstring+' is switching from priority '+pri.tostring+' to '+pri_target.tostring);

    vd.Reconstitute(localvat);
{x$IFDEF NU}
    //after reconsistution, check that priority_target remains the same
    //if it changed then we should skip targeting this priority from now on
    if localvat.priority_target <> pri_target then begin
      skip_priority := pri_target;
      Debug.log('tried priority '+pri_target.tostring+' but got '+localvat.priority_target.tostring);
      exit(repFailed);
    end;
{x$ENDIF}

    exit(repSuccess);
  except
    on E: Exception do begin
      status := e.message;
      AlertAdmin(e.Message);
      exit(repFailed);
    end;
  end;

end;

procedure TVirtualDiskPriority.CopyVat;
var
  t: ni;
  l: TLock;
begin
  l := vd.GetLock;
  try
    for t:= 0 to MAX_BLOCKS_IN_VAT-1 do begin
      tablecopy[t].rec := @vd.vat.table[t];
      tablecopy[t].stat := @vd.vatstats.big_blocks[t];
      tablecopy[t].startingblock := t shl BIG_BLOCK_BLOCK_SHIFT;
      if tablecopy[t].rec.FileCount <= 0 then
        tablecopy[t].stat.Init;
    end;

    for t:= 0 to high(vd.vat.PayloadConfig.filelist) do begin
      payloadpriorities[t] := vd.vat.PayloadConfig.filelist[t].priority;
    end;

    vd.unlocklock(l);
  finally
  end;

end;


procedure TVirtualDiskPriority.DoExecute;
var
  tm: ticker;
  daypart: double;
begin
  inherited;
  Loop := true;
  runhot := false;
{x$DEFINE DISABLE_3AM_ENFORCEMENT}
{$IFNDEF DISABLE_3AM_ENFORCEMENT}
  if not vd.IsOnline then
    exit;
{$ENDIF}

  if (not Prepped) or (GEtTimeSince(lastpreptime) > (60000*60*24)) then begin
    daypart := now-trunc(now);
{$IFNDEF DISABLE_3AM_ENFORCEMENT}
    if daypart < (1/8) then begin
      status := 'Waiting until 3AM';
      sleep(4000);
      exit;
    end;
    if daypart > (2/8) then begin
      status := 'Waiting for next day';
      sleep(4000);
      exit;
    end;
{$ENDIF}
    Status := 'Prep';
    Prep;
    enable := true;
  end;

  if enable then begin
    daypart := now-trunc(now);
{$IFNDEF DISABLE_3AM_ENFORCEMENT}
    if daypart < (1/8) then begin
      status := 'Waiting until 3AM';
      sleep(4000);
      exit;
    end;
    if daypart > (2/8) then begin
      status := 'Waiting for next day';
      sleep(4000);
      exit;
    end;
{$ENDIF}
    tm := GetTicker;
    case ContinueReprioritize of
      repNothingToDo: begin
        runhot := true;
      end;
      repSuccess: begin
        runhot := false;
        coldruninterval := GetTimeSince(tm) * 20;
      end;
      repFailed: begin
        runhot := false;
        coldruninterval := GetTimeSince(tm) * 20;
        nextindex := -1;
      end;

    end;

    dec(nextindex);
    if nextindex < 0 then begin

      dec(workingpriority);
      if stage = psDowngradeFrom then begin
        status := 'Downgrade from '+workingpriority.tostring;
        Debug.Log(self, status);
      end else begin
        status := 'Upgrade to '+workingpriority.tostring;
        Debug.Log(self, status);
      end;

      Debug.Log(self, 'Working on priority '+workingpriority.tostring);
      nextIndex := MAX_BLOCKS_IN_VAT-1;

      if workingpriority < 0 then begin
        if stage = psDowngradeFrom then begin
          Debug.Log(self, 'Switching to UPGRADE stage');
          stage := psUpgradeTo;
          workingpriority := maxpriorityassigned;
          nextIndex := MAX_BLOCKS_IN_VAT-1;
        end
        else begin
          step := 1;
          stepcount := 1;
          status := 'Done!' + datetimetostr(now);
          coldruninterval := 10000;
          enable := false;
        end;
      end;
    end;
  end;
end;

procedure TVirtualDiskPriority.Prep;
var
  t: ni;
  iBlocks: ni;
  l: TLock;
begin
  //sort vat by callups
  l := vd.GetLock;
  try
    skip_priority := -1;
    Status := 'Copy Vat';
    dEBUG.lOG('Copy vat for preprioritize.');
    CopyVat;
    dEBUG.lOG('Sort vat for preprioritize.');
    Status := 'Sort Vat';
    SortTableCopy;
    t := 0;
    nextIndex := 0;
    workingpriority := 254;
    stage := psDowngradeFrom;
    Debug.Log('Tier Storage Simulation');
    Debug.Log('-----------------------');
    maxpriorityassigned := -1;
    while t < 256 do begin
      Status := 'Priority Sort '+inttostr(t);
      //count number of blocks that could be allocated in priority t
      iBlocks := RunTierStorageSimulation(t);
      //assign x number of blocks to priority t
      AssignPriority(t, iBlocks);
      if iBlocks > 0 then begin
        Debug.Log('Tier '+t.tostring+': '+iBlocks.tostring+' big blocks');
        if maxpriorityassigned < t then begin
          maxpriorityassigned := t;

        end;
      end;
      workingpriority := maxpriorityassigned;

      inc(t);
    end;
    vd.SaveVat(false);

    Prepped := true;
    lastpreptime := GEtTicker;

    Reset;
  finally
    vd.unlocklock(l);
  end;

end;

procedure TVirtualDiskPriority.Reset;
begin
  NextIndex := MAX_BLOCKS_IN_VAT-1;
  workingpriority := maxpriorityassigned;
  stage := psDowngradeFrom;
end;

function TVirtualDiskPriority.RunTierStorageSimulation(
  iPriorityTier: ni): int64;
var
  l: TLock;
  ps: TPayloadStream;
  iSize, iSizeMAx, iSpace: ni;
  spacesimulator: array[0..2048] of int64;
  iAllocated, iLastAllocated: int64;
  t: ni;
  function SpaceAvailable: int64;
  var
    tt: ni;
  begin
    result := 0;
    for tt := 0 to high(spacesimulator) do begin
      inc(result, spacesimulator[tt]);
    end;
  end;

  function HaveSpace(iDriveSpan: ni): boolean;
  var
    iRaidSize: ni;
    tt: ni;
    iFoundDrives: ni;
  begin
    iRaidSize := GetBigBlockSizeInBytes(iDriveSpan)+(2*sizeof(TVirtualDiskBigBlockHeader));
    iFoundDrives := 0;
    for tt := 0 to high(spacesimulator) do begin
      if spacesimulator[tt] > iRaidSize then begin
        inc(iFoundDrives);
        if (iFoundDrives = iDriveSpan) then
          exit(true);
      end;
    end;

    exit(false);
  end;

  //-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
  function AllocateSpace(iDriveSpan: ni): boolean;
  var
    iRaidSize: ni;
    tt: ni;
    iFoundDrives: ni;
  begin
    iRaidSize := GetBigBlockSizeInBytes(iDriveSpan)+(2*sizeof(TVirtualDiskBigBlockHeader));
    iFoundDrives := 0;
    for tt := 0 to high(spacesimulator) do begin
      if spacesimulator[tt] > iRaidSize then begin
        inc(iFoundDrives);
        dec(spacesimulator[tt], iRAidSize);
        if (iFoundDrives = iDriveSpan) then
          exit(true);
      end;
    end;

    exit(false);
  end;
  //-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
  function AllocateBest: boolean;
  var
    tt: ni;
  begin
    for tt := greaterof(vd.vat.MaxDiskSpan, 1) downto 1 do begin
      if HaveSpace(tt) then begin
        exit(AllocateSpace(tt));
      end;
    end;
    exit(false);
  end;
//////////////////////////////////////
begin
  l := vd.GetLock;
  try//determine how much space is at each payload path quota/or space left
    FillMem(pbyte(@spacesimulator[0]), sizeof(spacesimulator), 0);
    for t:= 0 to vd.FPayloadStreams.Count-1 do begin
      ps := vd.FPayloadStreams[t];
      //don't tally space from tiers that are not of this priority
      if vd.vat.PayloadConfig.filelist[t].priority <> iPriorityTier then
        continue;
      if ps <> nil then begin
        //determine max allowed size of payload file
        iSizeMax := vd.vat.PayloadConfig.filelist[t].size_limit;
        //if no quote is defined, allow currentsize + free space on path
        if (iSizeMax < 0) then
          iSizeMAx := GetFreeSpaceOnPath(extractfilepath(ps.FileNAme))+ps.Size;
        //determine how much gap space is in the file and add that to total estimate
        iSizeMax := iSizeMax{+ps.gaps.sum};

        spacesimulator[t] := iSizeMAx;
      end;
    end;

    //now that we've determined how much actual space there is to
    //consume, we will run a simulation to determine how that space
    //might be optimally consumed
    iAllocated := 0;
    while AllocateBest do
      inc(iAllocated, 1);

    result := iAllocated;

  finally
    vd.UnlockLock(l);
  end;
end;


function TVirtualDiskPriority.SortPass: boolean;
var
  step: ni;
begin
  result := false;
  step := $40000000;
  while step > 0 do begin
    result := result or SortPassButterFly(step);
    step := step shr 1;
  end;


end;

function TVirtualDiskPriority.SortPassBubble: boolean;
var
  t: ni;
begin
  result := false;
  t := 0;
  while t < MAX_BLOCKS_IN_VAT-2 do begin
    if tablecopy[t].stat.Callups < tablecopy[t+1].stat.Callups then begin
      Swap(tablecopy[t], tablecopy[t+1]);
      t := greaterof(0, t-1);
      result := true;
    end;
    inc(t)
  end;
end;

function TVirtualDiskPriority.SortPassButterFly(step: ni): boolean;
var
  t: ni;
  i1, i2: ni;
  cu1, cu2: int64;
begin
  result := false;
  t := 0;
  while (t+step) < MAX_BLOCKS_IN_VAT-1 do begin
    i1 := t;
    i2 := t+step;
    if (i1 < MAX_BLOCKS_IN_VAT)
    and (i2 < MAX_BLOCKS_IN_VAT) then begin
      cu1 := tablecopy[i1].stat.Callups;
      cu2 := tablecopy[i2].stat.Callups;
      if cu1 < cu2 then begin
        Swap(tablecopy[i1], tablecopy[i2]);
        result := true;
      end else
      if cu1=cu2 then begin
        if tablecopy[i1].startingblock < tablecopy[i2].startingblock then begin
          Swap(tablecopy[i1], tablecopy[i2]);
          result := true;
        end;
      end;
    end;
    inc(t,step)
  end;
end;

function TVirtualDiskPriority.SortPassRandom: boolean;
var
  t,tt: ni;
begin
  result := false;
  tt := MAX_BLOCKS_IN_VAT;
  while tt > 0 do begin
    t := random(MAX_BLOCKS_IN_VAT-2);
    if tablecopy[t].stat.Callups < tablecopy[t+1].stat.Callups then begin
      Swap(tablecopy[t], tablecopy[t+1]);
      result := true;
    end;
    dec(tt)
  end;
end;


procedure TVirtualDiskPriority.SortTableCopy;
var
  t: ni;
begin
  vd.vatstats.discount;
  t := 0;
  while SortPass do begin
    if (t and $FF) = 0 then
      Status := 'Sorting pass: '+t.ToString;
    inc(t);
  end;

end;

procedure TVirtualDiskPriority.Swap(var a, b: TVatSTatEx);
var
  c: TVatStatEx;
begin
  c := a;
  a := b;
  b := c;
end;

function TVirtualDisk_Advanced.GetfileName: string;
var
  l: Tlock;
begin
  l := getLock;
  try
    if FVATStream = nil then
      result := ''
    else
      result := FVATStream.FileName;

  finally
    UnlockLock(l);
  end;
end;

function TVirtualDisk_Advanced.GetPayloadConfig
  : PVirtualDiskPayloadConfiguration;
var
  t: ni;
  l: TLock;
begin
  l := GetLock;
  try
    result := @vat.PayloadConfig;
    for t := low(result.filelist) to high(result.filelist) do
    begin
      if t < FPayloadStreams.count then
      begin
        if Self.FPayloadStreams[t] <> nil then
        begin
          result.filelist[t].used_space := FPayloadStreams[t].Size;
        end;
      end
      else
      begin
        result.filelist[t].used_space := 0;
      end;
    end;
  finally
    UnlockLock(l);
  end;
end;

function TVirtualDisk_Advanced.GetRepairLog: string;
var
  l: TLock;
begin
  l := getLock;
  try
    result := Repairlog.text;
  finally
    UnlockLock(l);
  end;
end;

function TVirtualDisk_Advanced.GEtSourceARchive(idx: ni): TRDTPArchiveClient;
begin
  if FSourceArchivers[idx] = nil then begin
    FSourceArchivers[idx] := TRDTPArchiveClient.create(SourceARchiveHost, '420');
    FSourceArchivers[idx].Host := SourceARchiveHost;
    FSourceArchivers[idx].endPoint := SourceARchiveEndpoint;
  end;

  result := FSourceArchivers[idx];
end;

function TVirtualDisk_Advanced.GEtUnderQuotaStream(notOf: PVirtualAddressRecord;
  out id: nativeint; bOnlyReadyStreams: boolean; iAllowPhysical: ni
  // special allowance for when we're looking to move a payload from 1 physical location.  We will allow it to move to another payload on the same location
  ): TPayloadStream;
var
  t, f: ni;
  pfc: TPayloadFileInformation;
  str: TPayloadStream;
  i1: int64;
  i2: int64;
  sz_min, sz: int64;
  prior: ni;
begin
  id := -1;
  result := nil;
  sz_min := -1;

{$IFNDEF USE_PRIORITY_SORT}
  prior := 0;
  while prior < 32767 do
  begin
    for t := 0 to FPayloadStreams.count - 1 do
    begin
      str := FPayloadStreams[t];
{$ELSE}
  // prior := vat.PayloadConfig.filelist[FPrioritySort[0].index].priority;
  prior := -1;
  SortPayloadPriorities;
  // Fpayloadstreams[FPayloadStreams[FPrioritySort[0].index].priority;
  for f := 0 to high(FprioritySort) do
  begin
    str := FprioritySort[f].str;
    t := FprioritySort[f].index;
{$ENDIF}
    if str = nil then
      continue;

{$IFNDEF USE_PRIORITY_SORT}
    if (prior >= 0) and (vat.PayloadConfig.filelist[t].priority <> prior) then
      continue;
{$ENDIF}
    i1 := str.Size;
    i1 := i1 + GetBigBlockSizeInBytes(1, true);

    i2 := vat.PayloadConfig.filelist[t].size_limit;
    if i2 = 0 then
      continue;

    if GetFreeSpaceOnPath(extractfilepath(str.FileName)) <
      (int64(BIG_BLOCK_SIZE_IN_BYTES) * 4) then
      continue;

    if (i1 < i2) or (i2 = -1) then
    begin
      if (str.Size < sz_min) or (sz_min = -1) then
      begin
        if ((not notOf.HasFile(t)) and (not PhysicalIsInVar(notOf,
          vat.PayloadConfig.filelist[t].physical))) or
          (vat.PayloadConfig.filelist[t].physical = iAllowPhysical) then
        begin
          result := str;
          id := t;
{$IFDEF USE_PRIORITY_SORT}
          prior := vat.PayloadConfig.filelist[t].priority;
{$ENDIF}
          sz_min := str.Size;
        end;
      end;

    end;
{$IFNDEF USE_PRIORITY_SORT}
  end;
{$ENDIF}
  //if result <> nil then
    //break;

  inc(prior);
end;

end;

function TVirtualDisk_Advanced.StreamHasGaps(t: nativeint; out physaddr: int64): boolean;
var
  ps: TPayloadStream;
  gi: TGapInfo;
begin
  result := false;
  if t< 0 then exit;
  ps := FPayloadStreams[t];
  if ps = nil then exit;
  gi := ps.gaps.findgap(GetBigBlockSizeInBytes(1, true), ps.size, false);
  physaddr := gi.start;
  result := physaddr >=0;
end;

function TVirtualDisk_Advanced.IsOnline: boolean;
begin
  result := FBringOnlineIdx <= 0;
end;

function TVirtualDisk_Advanced.IsStreamUnderQuota(t: nativeint): boolean;
var
  pfc: TPayloadFileInformation;
  str: TPayloadStream;
  i1: int64;
  i2: int64;
  IFREE: int64;
begin
  result := false;

  str := FPayloadStreams[t];
  if str = nil then
    exit;
  iFree := GetFreeSpaceOnPath(extractfilepath(str.FileName));
  result := iFree >=
    (int64(BIG_BLOCK_SIZE_IN_BYTES) * 4);
  IF not RESULT THEN BEGIN
    Debug.log(self, str.filename+' is low on space.  Size: '+friendlysizename(iFree));
  end else begin
  i1 := str.Size;
  i1 := i1 + GetBigBlockSizeInBytes(1, true);

  i2 := vat.PayloadConfig.filelist[t].size_limit;

  result := ((i1 < i2) or (i2 = -1)) and
    (vat.PayloadConfig.filelist[t].priority >= 0);
  END;
end;

procedure TVirtualDisk_Advanced.GrowIfNeededBlock(lba: int64);
var
  m: pbyte;
  t: int64;
begin
  exit;
  (* if (lba+1)*Blocksize < FStream.Size then
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
    end; *)

end;

procedure TVirtualDisk_Advanced.GuaranteeReadBlocks_Direct(lba: int64;
  cnt: nativeint; p: pbyte);
var
  iJustRead, iRead: nativeint;
begin
  // Debug.Log('GRB:'+inttostr(lba)+','+inttostr(cnt));
  iRead := 0;
  while iRead < cnt do
  begin
    iJustRead := ReadBlocks_Direct(lba, cnt, @p[iRead * BLOCKSIZE]);

    inc(lba, iJustRead);
    dec(cnt, iJustRead);
    inc(iRead, iJustRead);
  end;

end;

procedure TVirtualDisk_Advanced.GuaranteeWriteBlocks_Direct(lba: int64;
  cnt: nativeint; p: pbyte);
var
  iJustWrote, iWrote: nativeint;
begin
  // Debug.Log('GWB:'+inttostr(lba)+','+inttostr(cnt));
  iWrote := 0;
  while iWrote < cnt do
  begin
    iJustWrote := WriteBlocks_Direct(lba, cnt, @p[iWrote * BLOCKSIZE]);
    inc(lba, iJustWrote);
    dec(cnt, iJustWrote);
    inc(iWrote, iJustWrote);
  end;
end;

function TVirtualDisk_Advanced.HasRaid(StartingBlock: int64): boolean;
begin
  startingblock := startingblock and STRIPE_ALIGN_BLOCK_MASK;
  result := FRaidsByBlock.Search_StartingBlock_Exact(StartingBlock) <> nil;

end;

{$IFDEF use_vat}

procedure TVirtualDisk_Advanced.LoadFromConfiguration(ap: TAppParams;
  sPrefix: string);
var
  s: string;
begin
  Size := ap.GetItemEx(sPrefix + ':Size', int64(256 * MEGA));

  Identifier := ap.GetItemEx(sPrefix + ':Identifier',sPrefix);
  CacheSize := ap.GetItemEx(sPrefix + ':CacheSize',int64(2048 + MEGA));
  CachedStripes := ap.GetItemEx(sPrefix+':CachedSTripes', DEFAULT_CACHED_STRIPES);
  shipper.Host := ap.GetItemEx(sPrefix+':ArcTargetHost', '');
  shipper.EndPoint := ap.GetItemEx(sPrefix+':ArcTargetEndPoint', '420');
  shipper.Archive := ap.GetItemEx(sPrefix+':ArcTargetArchiveName', '');
  shipper.enabled := shipper.Archive <> '';
  SourceArchiveHost := ap.GetItemEx(sPrefix+':ArcSourceHost', '');
  SourceArchive := ap.GetItemEx(sPrefix+':ArcSourceArchiveName', '');
  SourceARchiveEndPoint := ap.GetItemEx(sPrefix+':ArcTargetEndPoint', '420');
  s := ap.GetItemEx(sPrefix+':ArcSourcePin', '0');
  if s = '0' then
    SourceArchivePinID := 0
  else
    SourceARchivePinID := strtodatetime(s);

  FileName := ap.GetItemEx(sPrefix + ':FileName', '');//<!!!!!---- DO THIS LAST

end;

procedure TVirtualDisk_Advanced.LoadPayloadStreams;
begin
  UpdatePayloadStreams;
end;

procedure TVirtualDisk_Advanced.updatePayloadStreams;
var
  t: integer;
  s: string;
  ps: TPayloadStream;
  pfh: TVirtualDiskPayloadFileHeader;
  bAllGood: boolean;
begin
  bAllGood := true;
  try
    if Self.vat.PayloadCount = 0 then
    begin
      s := changefileext(Self.FileName, '.vdpayload');
      movemem32(@Self.vat.PayloadConfig.filelist[0].name[0], @s[STRZ],
        sizeof(char) * Length(s));
      Self.vat.PayloadConfig.filelist[0].size_limit := -1;
    end;

    while FPayloadStreams.count <= high(Self.vat.PayloadConfig.filelist) do
      FPayloadStreams.add(nil);

    for t := low(Self.vat.PayloadConfig.filelist)
      to high(Self.vat.PayloadConfig.filelist) do
    begin
      ps := nil;
      s := Self.vat.PayloadConfig.filelist[t].name;
      if s = '' then
      begin
        if assigned(FPayloadstreams[t]) then begin
          FPayloadstreams[t].free;
          FPayloadstreams[t] := nil;
        end;
      end else
      begin
        if FPayloadStreams[t] <> nil then
          continue;

        if fileexists(s) then
        begin
          try
            Debug.Log(self, 'Loading payload '+s);
            ps := TPayloadStream.Create(s, fmOpenReadWrite + fmShareExclusive, 0,
              PAYLOAD_FLAGS);

            Self.vat.PayloadConfig.filelist[t].Flag_Missing := false;
          except
            on e: exception do begin
              AlertAdmin('err in LoadPayloadStreams: '+e.message);
              Self.vat.PayloadConfig.filelist[t].NameAsString := '';
              Self.vat.PayloadConfig.filelist[t].size_limit := 0;
              Self.vat.PayloadConfig.filelist[t].Flag_Missing := true;
              SaveVatAndConfig;
              ps := nil;
            end;
          end;
        end
        else
        begin
          if not vat.HasFile(t) then
            try
              ps := nil;
              try
                try
                  Debug.Log(self, 'CReating payload '+s);
                  ps := TPayloadStream.Create(s, fmCreate, 0, PAYLOAD_FLAGS);

                except
                  on e: EXception do begin
                    AlertAdmin('err in LoadPayloadStreams2: '+e.message);
                    ps := nil;
                  end;
                end;
                if ps <> nil then
                begin
                  pfh.Init;
                  ps.Seek(0, soBeginning);
                  Stream_GuaranteeWrite(ps, pbyte(@pfh), sizeof(pfh));
                end;
              finally
                ps.free;
              end;

              ps := TPayloadStream.Create(s, fmOpenReadWrite + fmShareDenyNone,
                0, PAYLOAD_FLAGS);
            except
              on e: EXception do begin
                AlertAdmin('err in LoadPayloadStreams3: '+e.message);
                Self.vat.PayloadConfig.filelist[t].NameAsString := '';
              end;
            end
          else
          begin

            OperationalStatus := 'Missing ' + s;
            Debug.Log(self, 'OPSTATUS: '+operationalstatus);
            Self.vat.PayloadConfig.filelist[t].Flag_Missing := true;
            FOperational := false;
            FPayloadsOK := false;
            bAllGood := false;
          end;
        end;

        if assigned(ps) then
        begin
{$IFDEF PAYLOAD_HAS_QUEUED_INTERFACE}
          // ps.AllowReadPastEOF := true;
//          ps.BufferSize := 32 * 65536;
//          ps.BufferSEgments := 32;
//          ps.DisableLookAhead := false;
          // ps.BufferSize := FCacheSize;
{$ENDIF}
        end;
        FPayloadStreams[t] := ps;
      end;

    end;
    Debug.log(self, 'Sorting Payload Priorities');
    SortPayloadPriorities;
    AnalyzeGaps;
    Debug.log(self, 'Sanity Check Payloads');
    SanityCheckPayloads;
    Debug.log(self, 'Analyze Gaps Again');
    AnalyzeGaps;
  finally

    FPayloadsOK := bAllGood;
    if FPayloadsOK then
    BEGIN
      Debug.log(self, 'Starting Scrubber');
      StartBringOnline;
      StartScrubber;
    end;

  end;

  Debug.log(self, 'Determine Highest Payload');
  HIghestAssignedPayload := -1;
  for t:= FPAyloadStreams.count-1 downto 0 do begin
    if FPayloadSTreams[t] <> nil then begin
      HighestAssignedPAyload := t;
      break;
    end;
  end;
  Debug.log(self, 'Load Finished');
end;

procedure TVirtualDisk_Advanced.LoadVat;
var
  p: PVirtualAddressRecord;
begin

  if FVATStream.Size < vat.PersistedSize then
  begin
    vat.InitVirgin;
    vat.FlushToStream(FVATStream, true);
  end
  else
  begin
    vat.ReadFromStream(FVATStream);
    vat.FlushToStream(FVATStream, true);
  end;

  QuickCheckVat;


//  p := @vat.table[10];
//  vat.MarkTableEntryDirty(10);
//  vat.MarkTableEntryDirtyByPtr(p);

end;

procedure TVirtualDisk_Advanced.LoadVatStats;
var
  ubs: TUnbufferedFileStream;
  sFile: string;
  bExists: boolean;

begin
  sFile := Self.FileName+'.vatstats';
  bExists := fileexists(sFile);
  vatstats.init;
  if bExists then begin
    ubs := nil;
    try
      ubs := TUnbufferedFileStream.create(sFile, fmOpenRead+fmShareExclusive);
      stream_GuaranteeRead(ubs, pbyte(@vatstats), lesserof(sizeof(vatstats), ubs.Size));
    finally
      ubs.free;
      ubs := nil;
    end;
  end;

end;

procedure TVirtualDisk_Advanced.LogRepair(idx: int64; s: string);
begin
  if idx = -2 then begin
    RestartBringOnline;
  end else
  if (idx >=0) and (idx < MAX_BLOCKS_IN_VAT)then begin

    online_bigblocks[idx] := false;
  end;

  //RecheckDiskOnCompletion := true;
  Repairlog.add(s);
  Repairlog.SaveToFile(self.FileName+'.repairlog');
  ZoneLog(idx, s);
  //debug.Log(s);
  debug.Log(self,'REPAIR:'+s);
end;

{$ENDIF}
{$IFDEF use_vat}

function TVirtualDisk_Advanced.NeedRaid(StartingBlock: int64)
  : TRaidAssembledBuffer;
var
  rab: TRaidAssembledBuffer;
  itm: TRaidTreeItem_ByLAstUsedTime;
  itm_blk: TRaidTreeitem_ByBlock;
  t: ni;
  x: ni;
  tableidx: ni;
begin
  result := nil;
  x := 0;
  if primary_buffer.StartingBlock = StartingBlock then begin
    primary_buffer.LAstUsed := getticker;
    result := primary_buffer;
    if enablelegittagging then
      result.legit := true;
    exit;
  end;
  tableidx := StartingBlock shr STRIPE_SHIFT;


{$IFDEF USE_VAT_HINTS}
  itm_blk := nil;
  if tableidx >= MAX_RAID_STRIPES then
    raise ECritical.create('raid out of range '+tableidx.tostring);
  if mem_vathints[tableidx]<>0 then
{$ENDIF}
  itm_blk := TRaidTreeitem_ByBlock(FRaidsByBlock.Search_StartingBlock_Exact(StartingBlock));
  if itm_blk <> nil then
  begin
    if itm_blk.rab.Startingblock <> Startingblock then
      raise ECritical.create('search returned '+inttostr(itm_blk.rab.startingblock)+' when I searched for '+inttostr(StartingBlock));

    result := itm_blk.rab;
    primary_buffer := result;
    result.lastused := getticker;
    if enablelegittagging then
      result.legit := true;
    exit;
  end;

  if result = nil then
  begin

    itm := FRaidsByLastUsed.firstitem;
    result := itm.rab;
    rab := result;


    if SyncAwayBuffer(rab) then
      // sync up to half the buffers if the syncaway buffer was dirty
      // (this means we're in heavy write mode) and flushing the buffers
      // together means that we'll have a better chance to perform write combines.
      FlushBuffers(true, 1000, true, true);

{$IFDEF USE_VAT_HINTS}
    if result <> nil then
    begin
      if result.StartingBlock >= 0 then
        mem_vathints[result.StartingBlock shr STRIPE_SHIFT] := 0;
    end;
{$ENDIF}
//    FRaidsByBlock.DebugItems_ByBlock;
    FRaidsByBlock.Remove(result.treeItemBlock, true);
//    FRaidsByBlock.DebugItems_ByBlock;
    if result.legit then
      rsPrefetch.AddStat(1.0)
    else
      rsPrefetch.AddStat(0.0);
    if rsPrefetch.NewBAtch then
      Front_CalculateRecommendedPrefetches;
    result.Init;
    if enablelegittagging then
      result.legit := true;
    result.StartingBlock := StartingBlock;
    FRaidsByBlock.add(result.treeItemBlock);

  end;

{$IFDEF USE_VAT_HINTS}
  mem_vathints[tableidx] := 1;
{$ENDIF}
  FRaidsByLastUsed.Remove(result.treeItemLastUsed, true);
  result.lastused := getticker;
  FRaidsByLastUsed.add(result.treeItemLastUsed);
  primary_buffer := result;
end;

function TVirtualDisk_Advanced.NewPhysicalBlockAddress(virtual_addr: int64; iMinPriority, iMaxPriority: ni; placed_by_reason: byte)
  : TVirtualAddressRecord;
var
  fs: TPayloadStream;
  tindex: ni;
  t,tt: ni;
  xxx: ni;
  iPriority: ni;
  blockstart: int64;
  vidx: int64;
  physaddrs: array [0..31] of int64;
  a: array [0 .. 31] of ni;
  aidx: ni;
  payloadindex: ni;
  localvat: PVirtualAddressRecord;
  iExpandBy: int64;
  iOldSize: int64;
  iStart, iEnd: int64;
  fp: PFilePhysical;
  bbh: TVirtualDiskBigBlockHeader;
  BRetry: boolean;
  bUnder: boolean;
  bHAsGaps: boolean;
  iExpandExt: ni;
  iPriAssigned: ni;
  function Has(ai: ni): boolean;
  var
    tt: ni;
  begin
    result := false;
    for tt := 0 to aidx - 1 do
    begin
      if a[tt] = ai then
      begin
        result := true;
        exit;
      end;
    end;

  end;

begin
  tindex := virtual_addr shr BIG_BLOCK_BYTE_SHIFT;
  virtual_addr := tindex shl BIG_BLOCK_BYTE_SHIFT;
  iPriAssigned := 255;
  vIDX  := ((virtual_addr shr BLOCKSHIFT) shr BIG_BLOCK_BLOCK_SHIFT);
{$IFDEF DEBUG_NEWPHYSICAL}zl.log(vIDX,'Enter NewPhysicalBlockAddress for BB:'+inttohex(vIDX,1)+' addr@'+inttohex(virtual_addr, 1)+' Priority-Range['+iMinPriority.tostring+'-'+iMAxPriority.tostring+']');{$ENDIF}
  // init result
  // todo  2: this next line can be implementd with a mask
  result.InitVirgin(vIDX);


  // put all under-quota streams in the list
  aidx := 0;
  iPriority := iMinPriority;
  while result.FileCount = 0 do
  begin
    SortPayloadPriorities;
    if iMaxPriority = iMinPriority then begin
      if high(FPrioritySort) < 0 then
        raise ECritical.create('priority sort has 0 elements');
      payloadindex := FPrioritySort[high(FprioritySort)].index;
      if (payloadindex < 0) or (payloadindex >  FPayloadstreams.count) then
        raise ECritical.create('payload index invalid '+inttostr(payloadIndex));
      xxx := vat.PayloadConfig.filelist[payloadindex].priority;


      if iMinPriority > xxx then begin
        iMinPriority := xxx;
      end;
    end;
    for tt := 0 to high(FprioritySort) do
    begin
      t := FPrioritySort[tt].index;
      fs := FprioritySort[tt].str;

      //if this file is of the right priority
      if not (vat.PayloadConfig.filelist[t].priority = iPriority) then begin
{$IFDEF DEBUG_NEWPHYSICAL}
//        zl.log(vIDX,'NOT Priority '+iPriority.ToString+' - '+vat.PayloadConfig.filelist[t].name);
{$ENDIF}
      end
      else
      begin
{$IFDEF DEBUG_NEWPHYSICAL}
        zl.log(vIDX,'Priority '+iPriority.ToString+' is '+vat.PayloadConfig.filelist[t].name);
{$ENDIF}
        physaddrs[t] := -1;
        bHasGaps := StreamHasGaps(t, physaddrs[t]);
{$IFDEF DEBUG_NEWPHYSICAL}
        if physaddrs[t] >=0 then
          zl.log(vIDX,'StreamHasGaps @'+inttohex(physaddrs[t],1)+' '+fs.FileNAme)
        else
          zl.log(vIDX,'No Gaps in Stream '+fs.FileNAme);
{$ENDIF}
        bUnder := IsStreamUnderQuota(t);
{$IFDEF DEBUG_NEWPHYSICAL}
        if bUnder then begin
          zl.log(vIDX,'Stream Is Under Quota '+FriendlySizeName(fs.Size)+' '+fs.filename);
        end else begin
          zl.log(vIDX,'Stream Is NOT Under Quota '+FriendlySizeName(fs.Size)+' '+fs.filename);
        end;
{$ENDIF}
        if bUnder or bHasGaps then
        begin
          if  Has(vat.PayloadConfig.filelist[t].physical){physical drive} then begin
            zl.log(vIDX,'We already have physical drive '+vat.PayloadConfig.filelist[t].physical.tostring+'. '+fs.filename);
          end else
          begin
            // also if there's plenty of free space
            if not (GetFreeSpaceOnPath(extractfilepath(fs.FileName)) > (150 * million))
            then begin
{$IFDEF DEBUG_NEWPHYSICAL}zl.log(vIDX,'SKipping Path because of low disk space '+fs.filename);{$ENDIF}
            end else
            begin
              // add the stream to the list
{$IFDEF DEBUG_NEWPHYSICAL}zl.log(vIDX,'Will Use ['+inttostr(t)+']'+fs.FileNAme);{$ENDIF}
              result.FPs[result.FileCount].FileID := t;
              inc(result.FileCount);
              iPriAssigned := iPriority;
              a[aidx] := vat.PayloadConfig.filelist[t].physical;//<--physical drive number not address
              // result.fileids[result.FileCount].physical;
              inc(aidx);
              if ((self.vat.MaxDiskSpan > 0) and (result.filecount >= Self.vat.MaxDiskSpan))
              then begin
{$IFDEF DEBUG_NEWPHYSICAL}
                zl.log(vIDX,'Disk span LImit reached.');
{$ENDIF}
                break;
              end;
            end;
          end;
        end;
      end;
    end;
    //*******STOP HERE IF we found at least two disks on the priority
//    if (result.filecount >= 2) then begin
//      break;
//    end;
    inc(iPriority);
    //==========IMPORTANT-=====
    if iPriority > iMaxPriority then begin
      IF (iMinPriority = MIN_PRIORITY) and (iMAxPriority=MAX_PRIORITY) then
      begin
        zl.log(vIDX,'*************KILLING DISK, NO PLACE TO PUT DATA******');
        iPriority := 0;
        Self.FOperational := false;
        OperationalStatus := 'Could not allocate new physical block.';
        break;
      end;

      if ((self.vat.MaxDiskSpan >1) and (result.FileCount < 2)) or (result.FileCount < 1) then begin
        //if WE've already seached for ANYThiNG beyond our preferrend priority
        IF (iMinPriority > MIN_PRIORITY) and (iMAxPriority=MAX_PRIORITY) then begin
          //We have no choice but to search for anything and everything
  {$IFDEF DEBUG_NEWPHYSICAL}zl.log(vIDX,'******* Trying different priority because we didn''t find anything up to 255');{$ENDIF}
          exit(NEwPhysicalBlockAddress(virtual_addr, MIN_PRIORITY, MAX_PRIORITY, placed_by_reason))//RECURSIVE CALL without BOUNDS
        end
        //else if this is our first targeted search and we found nothing...
        else begin
          //..expand our search to anything we target or any lower priority zones
  {$IFDEF DEBUG_NEWPHYSICAL}zl.log(vIDX,'******* Trying different priority (up to 255) because specific priority wasn''t found');{$ENDIF}
          exit(NEwPhysicalBlockAddress(virtual_addr, iMinPriority, MAX_PRIORITY, placed_by_reason));//RECURSIVE CALL without BOUNDS
        end;
      end;
    end;
    //^^^^^^^^^^^^^^^^^^^^^^^^^^

  end;

  // now that we know how many there are... we need to expand them all
{$IFDEF DEBUG_NEWPHYSICAL}zl.log(tindex,'New physical will span '+result.filecount.tostring+' disks.');{$ENDIF}
  for t := 0 to result.FileCount - 1 do
  begin
    tt := result.FPs[t].FileID;
    fs := FPayloadStreams[tt];
    fs.CheckInit;
    // blockstart := fs.Size;

    iOldSize := fs.Size;
    iExpandBy := GetBigBlockSizeInBytes(result.FileCount, false);
    fp := @result.FPs[t];

    repeat
      bRetry := false;
{$DEFINE TRUST_EXPAND_TO_FILL_GAPS}
{$IFNDEF TRUST_EXPAND_TO_FILL_GAPS}
      if physaddrs[tt] < 0 then begin
{$ENDIF}
        fp.PhysicalAddr_afterheader := fs.Expand(iExpandBy, virtual_addr);
{$IFNDEF TRUST_EXPAND_TO_FILL_GAPS}
      end
      else begin
        fp.physicaladdr := physaddrs[tt]+sizeof(TVirtualDiskBigBlockHeader);
        bbh.HeaderStart := physaddrs[tt];
        bbh.PayloadStart := fp.physicaladdr;
        bbh.VirtualAddress := virtual_addr;
        bbh.TotalSizeIncludingHeaders := GetBigBlockSizeInBytes(result.FileCount, true);
        bbh.UpdateFooterStart;

        bbh.UpdateCheckSum;
        fs.Seek(bbh.HeaderStart, soBeginning);
        // Header
        Stream_GuaranteeWrite(fs, pbyte(@bbh), sizeof(bbh));
        // payload
        Stream_WriteZeros(fs, iExpandBy);
        // footer
        Stream_GuaranteeWrite(fs, pbyte(@bbh), sizeof(bbh));
//        fs.gaps.ConsumeRegion(physaddrs[t], iExpandBy+(sizeof(TVirtualDiskBigBlockHeader) shl 1));
      end;
{$ENDIF}
      fs.gaps.Length := fs.Size;
      iStart := fp.PhysicalAddr_afterheader-sizeof(TVirtualdiskbigblockheader);
      iEnd := iStart + (iExpandBy-1);
      iExpandExt := iExpandBy+(sizeof(TVirtualDiskBigBlockHeader) shl 1);
      if not fs.gaps.canconsumeregion(iStart,iExpandExt) then begin
        zl.log(tindex,'Payload reports that it cannot consume region that was previously chosen @'+inttohex(iStart,1)+' sz='+friendlysizename(iExpandExt));
        logrepair(tt,fs.gaps.GetDebugGaps);
        logrepair(t,'wtf... cannot consume region?  '+inttohex(iStart,1)+'-'+inttohex(iEnd, 1));
        StreamHasGaps(result.FPs[t].FileID, physaddrs[tt]);
        raise ECritical.create('crash!');
        bRetry := true;
      end;
    until bRetry = false;

    // !NO! GETS Consumed by RecordVarInGapTrees//FGapTrees[fp.fileid].ConsumeRegion(iOldSize, fs.size-iOldSize);
  end;

  result.StartingBlock := virtual_addr shr BLOCKSHIFT;
  // determine table index
  tindex := virtual_addr shr BIG_BLOCK_BYTE_SHIFT;
  // put in table
  localvat := @vat.table[tindex];
  result.priority_target := iPriAssigned;
  localvat^ := result;



  // mark part of table dirty
  vat.MarkTableEntryDirty(tindex);
  vat.MarkTableEntryDirtyByPtr(localvat);
  // save changes to vat
  SaveVat(false);
  zl.log(tindex,'New physical chosen: '+result.debugstring);

{$IFDEF DETAILED_DEBUGGING}
  zl.log(tindex,'New Physical Spans ' + inttostr(result.FileCount) +
    ' payloads.');
  zl.log(tindex,'New Physical chosn:' + result.DebugString);
  zl.log(tindex,'New Vat:' + Self.vat.DebugVatSTructure);
{$ENDIF}
  Self.RecordVarInGapTrees(result);

end;
{$ENDIF}

procedure TVirtualDisk_Advanced.GrowIfNeededAddr(Size: int64);
begin
  GrowIfNeededBlock((Size shr BLOCKSHIFT) + 1);
end;

function TVirtualDisk_Advanced.PhysicalIsInVar(vart: PVirtualAddressRecord;
  iPhysical: ni): boolean;
var
  t: ni;
  id: ni;
begin
  result := false;
  for t := 0 to vart.FileCount - 1 do
  begin
    id := vart.FPs[t].FileID;
    if id < 0 then
      continue;
    if vat.PayloadConfig.filelist[id].physical = iPhysical then
    begin
      result := true;
      break;
    end;
  end;
end;

procedure TVirtualDisk_Advanced.Preallocate(lba: int64);
begin
  exit;
  // self.BlockSize := blocksize;
  GrowIfNeededBlock(lba);
{$IFNDEF USE_STANDARD_STREAM}
//  FVATStream.Flush;
{$ENDIF}
  FileName := FileName;

end;

procedure TVirtualDisk_Advanced.PrefetchAllPayloads(pos: int64; bStopOnHit: boolean; bWriteMode: boolean);
var
  ps: TPayloadStream;
  t: ni;
  ubs: TUnbufferedFileStream;
  sf: queuestream.TSideFetchCommand;
  tm: ticker;
begin
  if not assigned(sfthread) then
    exit;
  back_prefetchposition := pos;
  back_allowed_prefetches := 32;
//  rs3.BeginTime;
  //figure out the next logical thing we might want to fetch
  if not bWriteMode then begin
    while (back_allowed_prefetches > 0) and HasRaid((back_prefetchposition and RAID_ALIGN_BYTE) shr BLOCKSHIFT) do
    begin
      inc(back_prefetchposition, RAID_STRIPE_SIZE_IN_BYTES);
      dec(back_allowed_prefetches);
    end;
  end;

  if back_allowed_prefetches = 0 then
    exit;

  //sync up the pointers on all the payloads to something close to what we're wanting next
  tm := GEtHighResTicker;
  repeat
    t := prefetch_payload_idx;



    ps := FPayloadStreams[t];
    if ps <> nil then begin
      {$IFNDEF PAYLOAD_IS_UNBUFFERED}
      ubs := ps.understream as TUnbufferedFileSTream;
      if bSTopOnHit then
        PrefetchPayload(t, self.back_prefetchposition, 1, ubs, bWriteMode)
      else
        PrefetchPayload(t, self.back_prefetchposition, 32, ubs, bWriteMode);
      {$ELSE}
      ubs := ps;
      if bSTopOnHit then
        PrefetchPayload(t, self.back_prefetchposition, 1, ubs, bWriteMode)
      else
        PrefetchPayload(t, self.back_prefetchposition, 32, ubs, bWriteMode);

      {$ENDIF}

    end;
    inc(t);
    if (t > HighestAssignedPayload) then
      t := 0;

    prefetch_payload_idx := t;
  until (t=0) or (GEtTimeSince(tm)>50);
//  rs3.endtime;
//  if rs3.NewBAtch then rs3.OptionDebug('Start Payload Prefetches');
  if not bStopOnHit then begin
    sfthread.runhot := true;
    sfthread.HasWork := true;
  end;

end;

function TVirtualDisk_Advanced.PRefetchPayload(const fileid: ni; const  startsearch_byte: int64; const maxsearch_bblocks: int64; const ubs: TUnbufferedFileStream; bWRiteMode: boolean): boolean;
var
  pvar: PVirtualAddressRecord;
  bbidx: ni;
  searchpos: int64;
  cx: ni;
  fp: PFilePhysical;
  prog: single;
  span: int64;
  bpos: int64;
begin
  searchpos := startsearch_byte;
  cx := maxsearch_bblocks;
  //search forward in VAT for next occurance of fileid
  repeat
    //determine BB of searchpos
    bbidx := searchpos shr BIG_BLOCK_BYTE_SHIFT;
    if bbidx < 0 then exit(false);
    if bbidx >= MAX_BLOCKS_IN_VAT then exit(false);

    //get the record
    pvar := @vat.table[bbidx];
    if pvar.filecount <=0 then exit(false);
    //if record has our fileid
    fp := pvar.GetFP(fileid);
    if fp <> nil then begin

      //set the prefetch position to the search position
      span := GetBigBlockSizeInBytes(pvar.FileCount);
      prog := (searchpos and not BIG_BLOCK_ALIGN_BYTE) / BIG_BLOCK_SIZE_IN_BYTES;

      bpos := fp.PhysicalAddr_afterheader+trunc(span*prog);
      ubs.prefetchwritemode := bWriteMode;
      ubs.prefetchbytepos := bpos;
      ubs.StartPrefetches;

      //return success
      exit(true);
    end
    //else
    else begin
      //decrement our search counter
      dec(cx);

      //increment search pointer to next big-block ALIGNED to start of big block
      inc(searchpos, BIG_BLOCK_SIZE_IN_BYTES);
      searchpos := searchpos and BIG_BLOCK_ALIGN_BYTE;

      //continue on to the next big block

    //endif
    end;
  until cx = 0;
  exit(false);
end;

function TVirtualDisk_Advanced.QueueFull: boolean;
begin
  result := queue.QueueFull;
end;

procedure TVirtualDisk_Advanced.QuickCheckVat;
var
  t: ni;
begin
  debug.Log('2629 is:'+vat.table[$2629].DebugString);
  debug.Log('308 is:'+vat.table[$308].DebugString);
  for t := low(vat.table) to high(vat.table) do
  begin
    vat.table[t].StartingBlock := t * (_BIG_BLOCK_SIZE_IN_BLOCKS);
  end;

end;

procedure TVirtualDisk_Advanced.QuickOnline;
var
  t: ni;
begin
  for t := 0 to MAX_BLOCKS_IN_VAT - 1 do
  begin
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
  iOldSize: int64;
  a: array [0 .. BLOCKSIZE - 1] of byte;
  l: TLock;
  phys: int64;
label
  rep;
begin

  result := false;
  if self.vat.maxdiskspan in [1,2] then exit;

  l := GetLock;
  try
    vart := vat.FindVARToRAIDUp;

    if vart <> nil then
    begin
      ZoneLog(vart.vatindex,'Needs to RAID-up: '+vart.debugstring);
      if not online_bigblocks[vart.vatindex] then begin
        Debug.Log('block to RAID-up was not online');
        exit(false);
      end;

      toS := GEtUnderQuotaStream(vart, toID, false);
      if toS <> nil then
      begin
        Status := 'RAID up @'+inttohex(vart.VatIndex,0)+' (fix broken RAID) to disk '+inttostr(toID);
        ZoneLog(vart.vatindex,Status);
        // allocate space in a new file
        iOldSize := toS.Size;
        phys := toS.Expand(GetBigBlockSizeInBytes(vart.FileCount, false),vart.StartingBlock * BLOCKSIZE, true);
                            //^ growth                                   ^ virtual address (to consume)^ ^consume
        if vart.HasFile(toID) then begin
          ZoneLog(vart.vatindex,'In RAIDUP, target already has fileid '+inttostr(toID));
          exit;
        end;
        vart.ChangeFileID(-1, toID, phys);
        vart.placedby := PLACED_BY_RAIDUP;
        self.vat.MarkTableEntryDirtyByPtr(vart);

//        FPayloadStreams[toID].gaps.Length := toS.Size;
//        FPayloadStreams[toID].gaps.ConsumeRegion(iOldSize, toS.Size - iOldSize);

        // read and write-back all the RAID blocks in the big block
        for t := 0 to _BIG_BLOCK_SIZE_IN_BLOCKS - 1 do
        begin
          Self.ReadBlock(vart.StartingBlock + t, @a[0]);
          Self.WriteBlock(vart.StartingBlock + t, @a[0]);
        end;
        result := true;
      end;
    end;

  finally
    UnlockLock(l);
  end;

end;

procedure TVirtualDisk_Advanced.ReadBlock(const lba: int64; const p: pbyte);
begin
  ReadBlocks(lba, 1, p);
end;

function TVirtualDisk_Advanced.ReadBlocks(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
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

function TVirtualDisk_Advanced.ReadBlocks_Buffered(const lba: int64;
  const cnt: nativeint; pCanBeNil: pbyte): nativeint;
var
  ioffset, iByteOffset: nativeint;
  iToRead: nativeint;
  cnt2: nativeint;
  l: TLock;
{$IFDEF RECORD_OPS}
  REC: TRecordedOp;
{$ENDIF}

begin
//  debug.log('readblocks_buffered('+inttostr(lba)+', cnt='+inttostr(cnt));
//  rs0.BeginTIme;
  l := getLock;
  try
    // sync up the buffer to the desired position
    SyncBuffer(lba, false);

    // determine which slot in the buffer we're using
    ioffset := lba and (int64(MAX_BUFFERED_BLOCKS - 1));

    // READ the data FROM the buffer
    iByteOffset := BLOCKSIZE * ioffset;

    cnt2 := cnt;
    cnt2 := lesserof(cnt2, MAX_BUFFERED_BLOCKS - ioffset);
    iToRead := lesserof(cnt2, MAX_BUFFERED_BLOCKS - ioffset);
    result := iToRead;
    iToRead := iToRead * BLOCKSIZE;

    if pCanBeNil <> nil then begin
      movemem32(pCanBeNil, primary_buffer.byteptr(iByteOffset), iToRead);
{$IFDEF RECORD_OPS}
      rec.op := 0;
      rec.block := lba;
      rec.blockcount := result;
      CalculateChecksum(pCanBeNil, result shl 9, rec.cs[SET_CS]);
      stream_GuaranteeWrite(oplog, @rec, sizeof(rec));
{$ENDIF}
    end;


    // mark dirty
    // buffer.dirty[iOffset] := true;
  finally
    UnlockLock(l);
  end;
//  rs0.endTime;
//  if rs0.NewBAtch then
//    rs0.OptionDebug('readblocks_buffered time');

end;
{$ENDIF}

function TVirtualDisk_Advanced.ReadBlocks_Direct(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
begin
  result := ReadBlocks_Direct_Single(lba, cnt, p);
end;

function TVirtualDisk_Advanced.ReadBlocks_Direct_Single(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
var
  actual_addr: TVirtualAddressRecord;
  end_actual_addr: TVirtualAddressRecord;
  bDifferentFiles: boolean;
  lcnt: nativeint;
  ps: TPayloadStream;
  l: TLock;
begin
  lcnt := cnt;
  result := cnt;

  l := getLock;
  try
    GrowIfNeededBlock(lba + (lcnt - 1));
    actual_addr := VirtualToPhysicalAddr(lba * BLOCKSIZE);
    end_actual_addr := VirtualToPhysicalAddr((lba + lcnt) * BLOCKSIZE);
{$IFDEF ALLOW_RAID}
    bDifferentFiles := actual_addr.FPs[0].FileID <> end_actual_addr.FPs
      [0].FileID;
{$ELSE}
    bDifferentFiles := actual_addr.FileID <> end_actual_addr.FileID;
{$ENDIF}

    // if the blocks are not all consecutive, we'll need to reduce the number of blocks we can write
{$IFNDEF ALLOW_RAID}
    if bDifferentFiles or
      ((end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <>
      (lcnt * BLOCKSIZE)) then
    begin
{$ELSE}
    if bDifferentFiles or
      ((end_actual_addr.FPs[0].PhysicalAddr_afterheader - actual_addr.FPs[0].PhysicalAddr_afterheader)
      <> (lcnt * BLOCKSIZE)) then
    begin
{$ENDIF}

      // change the end actual address
      begin
        // find the beginning of the actual_addr bigblock
{$IFDEF ALLOW_RAID}
        end_actual_addr.FPs[0] := actual_addr.FPs[0];
{$ELSE}
        end_actual_addr.FileID := actual_addr.FileID;
{$ENDIF}
        end_actual_addr := VirtualToPhysicalAddr
          (((lba shr BIG_BLOCK_BLOCK_SHIFT) shl BIG_BLOCK_BLOCK_SHIFT)
          shl BLOCKSHIFT);
{$IFNDEF ALLOW_RAID}
        end_actual_addr.PhysicalAddress := end_actual_addr.PhysicalAddress +
          (_BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT);
{$ELSE}
        end_actual_addr.FPs[0].PhysicalAddr_afterheader := end_actual_addr.FPs[0]
          .PhysicalAddr_afterheader + (_BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT);
{$ENDIF}
      end;

{$IFNDEF ALLOW_RAID}
      result := (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress)
        shr BLOCKSHIFT;
{$ELSE}
      result := (end_actual_addr.FPs[0].PhysicalAddr_afterheader - actual_addr.FPs[0]
        .PhysicalAddr_afterheader) shr BLOCKSHIFT;
{$ENDIF}
      lcnt := result;
{$IFNDEF ALLOW_RAID}
      if (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <>
        (lcnt shl BLOCKSHIFT) then
      begin
{$ELSE}
      if (end_actual_addr.FPs[0].PhysicalAddr_afterheader - actual_addr.FPs[0].PhysicalAddr_afterheader)
        <> (lcnt shl BLOCKSHIFT) then
      begin
{$ENDIF}
        raise ECritical.Create('Block segment fault failure.');
      end;
    end;
    // Debug.Log(inttohex(lba*BLOCKSIZE, 2)+'<-actual_addr(rb)->'+inttohex(actual_addr, 2));

    if (lba * BLOCKSIZE) >= (Size + BLOCKSIZE) then
      raise ECritical.Create('Address beyond the size of the disk! ' +
        inttohex(lba shl BLOCKSHIFT, 2));

{$IFDEF ALLOW_RAID}
    ps := FPayloadStreams[actual_addr.FPs[0].FileID];
{$IFDEF USESEEKLOCK} ps.SeekLock; {$ENDIF}
    try
      ps.Seek(actual_addr.FPs[0].PhysicalAddr_afterheader, 0);
      Stream_GuaranteeRead(ps, p, lcnt shl BLOCKSHIFT);
    finally
{$IFDEF USESEEKLOCK} ps.seekunlock; {$ENDIF}
    end;
{$ELSE}
    ps := FPayloadStreams[actual_addr.FileID];
{$IFDEF USESEEKLOCK} ps.SeekLock; {$ENDIF}
    try
      FPayloadStreams[actual_addr.FileID].Seek(actual_addr.PhysicalAddress, 0);
      Stream_GuaranteeRead(FPayloadStreams[actual_addr.FileID], p,
        BLOCKSIZE * lcnt);
    finally
{$IFDEF USESEEKLOCK} ps.seekunlock; {$ENDIF}
    end;

{$ENDIF}
  finally
    UnlockLock(l);
  end;

end;

function TVirtualDisk_Advanced.ReadBlocks_RaidCalc(const lba: int64;
  const cnt: nativeint; p: pbyte): nativeint;
var
  lba_off: int64;
  iCanRead: int64;
begin
  // determine offset into RAID space
  lba_off := lba - primary_buffer.StartingBlock;
  if (lba_off < 0) or (lba_off >= RAID_STRIPE_SIZE_IN_BLOCKS) then
  begin
    raise ECritical.Create
      ('Something is wrong and lba is not within RAID stripe.');
  end;

  iCanRead := lesserof(RAID_STRIPE_SIZE_IN_BLOCKS - lba_off, cnt);

  movemem32(p, primary_buffer.byteptr((lba_off * BLOCKSIZE)),
    iCanRead * BLOCKSIZE);

  result := iCanRead;

end;

procedure TVirtualDisk_Advanced.ReadData(const addr: int64;
  const cnt: nativeint; p: pbyte);
{$IFDEF VD_QUEUE}
var
  qi: TVDReadCommand;
{$ENDIF}
begin
{$IFNDEF VD_QUEUE}
  SyncReadData(addr, cnt, p);
{$ELSE}
  qi := TVDReadCommand.Create;
  qi.vd := self;
  qi.addr := addr;
  qi.count := cnt;
  qi.p := p;
  queue.AddItem(qi);
  queue.urgent := true;
  qi.WAitFor();
  IF NOT qi.wasexecuted then
    raise ECritical.create('wtf');
  qi.Free;
{$ENDIF}
end;

function TVirtualDisk_Advanced.ReadData_Begin(const addr: int64;
  const cnt: nativeint; p: pbyte): TVDReadCommand;
{$IFDEF VD_QUEUE}
var
  qi: TVDReadCommand;
{$ENDIF}
begin
{$IFNDEF VD_QUEUE}
  SyncReadData(addr, cnt, p);
  result := nil;
{$ELSE}
  qi := TVDReadCommand.Create;
  qi.vd := self;
  qi.addr := addr;
  qi.count := cnt;
  qi.p := p;
  queue.AddItem(qi);
  queue.urgent := true;
  result := qi;
{$ENDIF}
end;

procedure TVirtualDisk_Advanced.ReadData_End(o: TVDReadCommand);
begin
  if o = nil then
    exit;
  o.WaitFor();
  o.free();
end;


function TVirtualDisk_Advanced.ReadSingleByte(iAddr: int64): byte;
var
  block: array [0 .. BLOCKSIZE - 1] of byte;
  iOFF: int64;
begin
  Self.ReadBlock(iAddr shr BLOCKSHIFT, @block[0]);
  iOFF := iAddr shr blockshift;

  result := block[iOFF];
  // self.WriteBlock(iAddr shr BLOCKSHIFT, @block[0]);

end;

{$DEFINE REBUILD_IN_SINGLE_CONNECTION}
{$IFNDEF REBUILD_IN_SINGLE_CONNECTION}
procedure TVirtualDisk_Advanced.REbuildBigBlockFromSourceArchive(bbid: int64);
var
  daLocal: TDynByteArray;
  l: TLock;
  bbidx: int64;
  s: string;
  zidx_start, zidx_last, zidx: int64;
  c: Tcmd_SourceFetch;
  t: ni;
  bMakeNewcommands: boolean;
  doneflags: array[0..ARC_ZONES_PER_BIG_BLOCK-1] of boolean;
  tmStart: ticker;
  function AllDone: boolean;
  var
    yy: ni;
  begin
    result := true;
    for yy := 0 to ARC_ZONES_PER_BIG_BLOCK-1 do
      if doneflags[yy] = false then exit(false);
  end;

begin
  if workingfromsourcearchive then
    exit;
  if disableArcRestore then
    exit;

  l := GetLock;
  try
    WorkingFromSourceARchive := true;
    try
      shipper.pauseforfetch := true;
//      if bbid = $5A then
//        Debug.Log('here');

      zidx := bbid shl BIG_BLOCK_BLOCK_SHIFT;
      zidx := zidx shr ARC_ZONE_BLOCK_SHIFT;

      Debug.Log('----Rebuild from Source Archive '+inttohex(bbid, 16));
//      s := SourceARchiver[t].GetZoneStackReport(SourceArchive, bbid shl BIG_BLOCK_BLOCK_SHIFT);
//      Debug.Log(s);
//      Debug.Log('----Rebuild from Source Archive '+inttohex(bbid, 16));
      //SourceArchiver.GetLog(SourceArchive, SourceArchivePinID, bbid shl BIG_BLOCK_BLOCK_SHIFT, _BIG_BLOCK_SIZE_IN_BLOCKS, da);
      zidx_start := zidx;
      zidx_last := zidx + ARC_ZONES_PER_BIG_BLOCK-1;

      //IF THERE ARE SOURCE FETCH COMMANDS RUNNING
      if (FSourceFetchCommands[0] <> nil)
      //AND THE SOURCE FETCH COMMANDS DO NOT MATCH
      and (FSourceFetchCommands[0].zidx <> zidx_start) then begin
        Debug.Log('Prefetch miss!');
        //MISS! wait for unmatched commands
        for t:= 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
          try
            //Debug.Log('Miss Wait for ' +FSourceFetchCommands[0].zidx.tostring);
            c:= FSourceFetchCommands[t];
            FsourceFetchcommands[t] := nil;
            c.waitfor;
            c.free;
            c := nil;
          except
          end;
        end;
      end
      //else (if there are no commands or the commands don't match)
      else begin
        //Start NEW commands
        resurrector.seekopt := zidx+1;
        for zidx := zidx_start to zidx_last do begin
          Debug.Log('Begin getLog zidx='+inttohex(zidx, 1));
          c := Tcmd_SourceFetch.create;
          c.arc := SourceArchivers[zidx mod CONCURRENT_SOURCE_ARCHIVER_CONNECTIONS];
          c.zidx := zidx;
          c.pin := sourcearchivepinid;
          c.archivename := sourcearchive;
          c.pointer := daSourcePrefetchResults[zidx mod ARC_ZONES_PER_BIG_BLOCK];
          FsourceFetchCommands[zidx mod ARC_ZONES_PER_BIG_BLOCK] := c;
          c.resources.SetResourceUsage('SourceFetch', 0.25);
          c.resources.SetResourceUsage('SourceFetch'+inttostr(zidx mod CONCURRENT_SOURCE_ARCHIVER_CONNECTIONS), 1.0);
          c.start;
        end;

      end;

//      Debug.Log('Rebuild Flush');
      while not (FlushBuffers(true, 0)= bsNoBuffersToFlush) do sleep(1);


      //initialize the done flags
      for t := 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
        doneflags[t] := false;
      end;

      //when a command is complete, swap for a prefetch command
      tmStart := GEtTicker;
      while not AllDone do begin
//        Debug.Log('Waiting for Rebuild Commands. '+inttostr(getTimeSince(tmStart) div 1000));
        for t := 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
          c := FSourceFetchCommands[t];
          IF c = nil then
            doneflags[t] := true
          else begin
            c.waitfor(100);
            if c.IsComplete then begin
              try
                //finish command
                c.WaitFor;
                daSourcePrefetchResults[t] := c.pointer;
                c.free;
                c := nil;
                FSourceFetchCommands[t] := c;
                zidx := zidx_start+t;

                Debug.Log('GetLog_Response zidx='+inttohex(zidx, 1));

                if length(daLocal) <> ARC_ZONE_SIZE_IN_BYTES then
                  raise ECritical.create('Expected ARC Zone size Bytes, but size returned was '+inttostr(length(daLocal)));

                if daLocal <> nil then begin
        {$IFNDEF OLD_REBUILD_WRITES}
                  SyncWriteData((zidx shl ARC_ZONE_BLOCK_SHIFT) shl BLOCKSHIFT, ARC_ZONE_SIZE_IN_BLOCKS shl BLOCKSHIFT, @daLocal[0]);
        {$ELSE}
                  GuaranteeWriteBlocks(zidx shl ARC_ZONE_BLOCK_SHIFT, ARC_ZONE_SIZE_IN_BLOCKS, @daLocal[0]);
        {$ENDIF}
                  while not (FlushBuffers(true, 0)= bsNoBuffersToFlush) do sleep(1);
                end;


              finally
                doneflags[t] := true;
              end;
            end else
              FlushBuffers(false, 0);
          end;
        end;
      end;



    while not (FlushBuffers(true, 0)= bsNoBuffersToFlush) do sleep(0);
//      FlushBuffersInBlockOrder(zidx shl ARC_ZONE_BLOCK_SHIFT, false, false);


    finally
      shipper.pauseforfetch := false;
      WorkingFromSourceARchive := false;
    end;
  finally
    UnlockLock(l);
  end;
end;
{$ELSE}
procedure TVirtualDisk_Advanced.REbuildBigBlockFromSourceArchive(bbid: int64);
var
  daLocal: TDynByteArray;
  l: TLock;
  bbidx: int64;
  s: string;
  zidx_start, zidx_last, zidx: int64;
  t: ni;
  cli: TRDTPArchiveClient;
begin
  if workingfromsourcearchive then
    exit;
  if disableArcRestore then
    exit;

  l := GetLock;
  try

    WorkingFromSourceARchive := true;
    try
      shipper.pauseforfetch := true;
//      if bbid = $5A then
//        Debug.Log('here');

      cli := SourceArchivers[0];
      zidx := bbid shl BIG_BLOCK_BLOCK_SHIFT;
      zidx := zidx shr ARC_ZONE_BLOCK_SHIFT;

      Debug.Log('----Rebuild from Source Archive '+inttohex(bbid, 16));
      zidx_start := zidx;
      zidx_last := zidx + ARC_ZONES_PER_BIG_BLOCK-1;

      for t := 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
        cli.GetLog_Async(sourcearchive, sourcearchivepinid, (zidx+t) shl ARC_ZONE_BLOCK_SHIFT, ARC_ZONE_SIZE_IN_BLOCKS);
      end;
      for t := 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
        cli.GetLog_Response(daSourcePrefetchResults[0]);
        SyncWriteData(((zidx+t) shl ARC_ZONE_BLOCK_SHIFT) shl BLOCKSHIFT, ARC_ZONE_SIZE_IN_BLOCKS shl BLOCKSHIFT, @daSourcePrefetchResults[0]);
        FlushBuffersInBlockOrder((zidx+t) shl ARC_ZONE_BLOCK_SHIFT, false, false);
      end;

    finally
      shipper.pauseforfetch := false;
      WorkingFromSourceARchive := false;
    end;
  finally
    UnlockLock(l);
  end;
end;
{$ENDIF}

function TVirtualDisk_Advanced.REbuildBigBlockFromTargetArchive(bbid: int64): boolean;
var
  da: TDynByteArray;
  l: TLock;
begin
  result := false;
//  exit(false);
  if workingfromtargetarchive then
    exit;
//  if disableArcRestore then
//    exit;
  result := false;
//  exit;


  l := GetLock;
  try
    WorkingFromTargetARchive := true;
    try
//      if bbid = $5A then
//        Debug.Log('here');
      shipper.pauseforfetch := true;
      Debug.Log('Rebuild from Target Archive '+inttohex(bbid, 16));
      setlength(da, BIG_BLOCK_SIZE_IN_BYTES);
      shipper.FetchLog(bbid shl BIG_BLOCK_BLOCK_SHIFT, _BIG_BLOCK_SIZE_IN_BLOCKS, @da[0]);
      while not (FlushBuffers(true, 0)= bsNoBuffersToFlush) do sleep(1);
      if IsAllZeros(@da[0], BIG_BLOCK_SIZE_IN_BYTES) then begin
        if (vat.table[bbid].FileCount < 0) or (vat.table[bbid].IsDead) then
          vat.table[bbid].FileCount := 0;
        exit(true);
      end;


      if length(da) <> BIG_BLOCK_SIZE_IN_BYTES then
        raise ECritical.create('Expected Big Block Bytes, but size returned was '+inttostr(length(da)));

      if da <> nil then begin
{$IFNDEF OLD_REBUILD_WRITES}
        SyncWriteData((bbid shl BIG_BLOCK_BLOCK_SHIFT) shl BLOCKSHIFT, _BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT, @da[0]);
{$ELSE}
        GuaranteeWriteBlocks(bbid shl BIG_BLOCK_BLOCK_SHIFT, _BIG_BLOCK_SIZE_IN_BLOCKS, @da[0]);
{$ENDIF}
        FlushBuffersInBlockOrder(bbid shl BIG_BLOCK_BLOCK_SHIFT, false, false);
        //while not FlushBuffers(0) do sleep(0);
        exit(true);
      end;
    finally
      shipper.pauseforfetch := false;
      WorkingFromTargetARchive := false;
    end;
  finally
    UnlockLock(l);
  end;
end;


procedure TVirtualDisk_Advanced.RecheckDisk;
begin
  FBringOnlineIdx := MAX_BLOCKS_IN_VAT - 1;
end;

function TVirtualDisk_Advanced.Reconstitute
  (pvar: PVirtualAddressRecord = nil): boolean;
var
  varsave: TVirtualAddressRecord;
  p: pbyte;
  t: fi;
  l: TLock;
  fid: fi;
  idx: int64;
  bDone: boolean;
begin
//  if pvar = nil then
//    exit(false);
  result := false; // start with result false
  l := getLock; // lock the whole disk
  try
    if bInReconstitution then begin
      if pvar <> nil then
        ZoneLog(pvar.vatindex, 'Recursion in reconstitute is not allowed');
    // if already reconstituting then exit (recursion danger?)
      exit;
    end;
    result := false;
    bInReconstitution := true; // flag that we're reconstituting

    //turn off stats (so we don't overscore stuff)
    disablecallupstats := true;

    if pvar = nil then
      pvar := vat.FindVARToReconstitute;
    // look for something to reconstitute... (priority < 0)???

    //if nothing found then exit
    if pvar = nil then
      exit;

    if pvar.SelfCheckOk(pvar.VatIndex, self)<>scrOk then begin
      zl.Log(pvar.vatindex,'Reconst VAR entry that was previously failed self-check');

    end;



    //something is wrong if we have more than 16 files
    if pvar.FileCount > 16 then
    begin
      ZoneLog(pvar.vatindex, 'wtf!  filecount > 16');exit;
    end;

    if pvar <> nil then
    begin
      //figure out what index we're on
      idx := pvar.StartingBlock div _BIG_BLOCK_SIZE_IN_BLOCKS;
      ZoneLog(pvar.vatindex, 'Reconstitute ' +
        inttohex(idx, 1));
      if idx = 0 then begin
        Debug.Log('trap');
      end;
      ZoneLog(pvar.vatindex, 'vart is '+pvar.DebugString);

      // save the original record in case there's a problem
      varsave := pvar^;

      // flag all these files as potentially collapsable
      for t := 0 to varsave.FileCount - 1 do
      begin
        fid := varsave.FPs[t].FileID;
        if ValidPayloadID(fid) then
          FPayloadStreams[fid].Collapsable := true;
      end;
      try

        //read all the data from the big block (we don't need the headers)
        p := GetMemory(_BIG_BLOCK_SIZE_IN_BLOCKS * BLOCKSIZE);
        if p = nil then begin
          ZoneLog(pvar.vatindex, 'OUT OF MEMORY!');
          exit;
        end;
        try
{$IFDEF FLUSH_ON_RECONSTITUTE}
          FlushBuffers(0,false,false);
{$ENDIF}
          // read the entire big block
          GuaranteeReadBlocks(pvar.StartingBlock, _BIG_BLOCK_SIZE_IN_BLOCKS, p);
          // read the blocks from the stuff, as-if we're just reading the drive
          ZoneLog(pvar.vatindex, 'Read for Reconst got '+memorytohex(p, 64));
//          AlertMemoryPattern(@BAD_PATTERN[0], sizeof(BAD_PATTERN), pbyte(p), _BIG_BLOCK_SIZE_IN_BLOCKS shl BLOCKSHIFT);

          // ----------------

          repeat
            //start the vat entry over fresh
            pvar.InitVirgin(varsave.StartingBlock);
            pvar.priority_target := varsave.priority_target;
            pvar.placedby := PLACED_BY_RECONST;
            NewPhysicalBlockAddress(pvar.virtualAddress, pvar.priority_target,pvar.priority_target, PLACED_BY_RECONST);

            // rewrite the entire big block (should use only drives with positive priority)
            GuaranteeWriteBlocks(pvar.StartingBlock, _BIG_BLOCK_SIZE_IN_BLOCKS, p, true);

            ZoneLog(pvar.vatindex, 'write for Reconst '+memorytohex(p, 64));
            {$IFDEF FLUSH_ON_RECONSTITUTE}
            FlushBuffers(0,false,false);
            {$ENDIF}
            ZoneLog(pvar.vatindex, 'Reconst flush in block order starting at '+pvar.startingblock.tohexstring);
            FlushBuffersInBlockOrder(pvar.StartingBlock, false, false);
            result := true;
            ZoneLog(pvar.vatindex, 'after reconst vart is '+pvar.DebugString);
            if pvar.filecount <=0 then
              zl.log(pvar.vatindex,'FILECOUNT <=0!!!');
          until pvar.filecount > 0;


          // wipe out the vat entry from the gap trees
          RemoveVatFromGapTrees(varsave);

        finally
          Freememory(p);
        end;

        //mark entry dirty so vat gets saved
        vat.MarkTableEntryDirtyByPtr(pvar);
        SaveVat(false);

      except
        on e: exception do begin
          ZoneLog(pvar.vatindex, 'err in reconstitute: '+e.message);
          AlertAdmin('err in reconstitute: '+e.message);

          pvar^ := varsave; //don't do this because var was removed from gap trees and allocated space will be used
        end;
      end;

    end;
    ShrinkPayloads; // shrink the payloads
    // flag all these files as potentially collapsable
    for t := 0 to varsave.FileCount - 1 do
    begin
      if (varsave.FPs[t].FileID > 2047) then
      begin
        debug.Log('wtf');
      end;
      Collapse(varsave.FPs[t].FileID);//WE CAN'T DO THIS HERE BECAUSE
    end;

  finally
    UnlockLock(l);
    bInReconstitution := false;
    disablecallupstats := false;
  end;

  reconstituting := result;

end;

procedure TVirtualDisk_Advanced.RecordVarInGapTrees
  (const vart: TVirtualAddressRecord);
//refok
var
  t: fi;
  fp: PFilePhysical;
  fid: fi;
  start,len,iend:int64;
begin
  for t := 0 to vart.FileCount - 1 do
  begin
    fp := @vart.FPs[t];
    fid := fp.FileID;
    if validPAyloadID(fid) and (FPayloadStreams[fid] <> nil) then
    begin
      // FGapTrees[fid].Length := FPayloadStreams[fid].Size;
      start := (fp.PhysicalAddr_afterheader - sizeof(TVirtualDiskBigBlockHeader));
      len := GetBigBlockSizeInBytes(vart.FileCount, true);
      iend := start+len;
      if FPayloadStreams[fid].gaps.length < iend then
        Fpayloadstreams[fid].gaps.length := iend;
      FPayloadStreams[fid].gaps.ConsumeRegion(start,len);
    end;
  end;
end;

procedure TVirtualDisk_Advanced.ReFunctPayload(iPayloadID: ni;
  sNewSource: string);
var
  fs: TFileStream;
  l: TLock;
begin
  l := getLock;
  try
    SaveVatAndConfig;
    vat.PayloadConfig.filelist[iPayloadID].NameAsString := sNewSource;
    try
      if not fileexists(sNewSource) then
      begin
        fs := nil;
        try
          fs := TFileStream.Create(sNewSource, fmCreate);
        finally
          fs.free;
        end;
        fs := nil;
      end;
    except
      on e: EXception do begin
        AlertAdmin('err in refunctpayload: '+e.message);
      end;
    end;
    LoadPayloadStreams;

    vat.ChangeAllFiles(iPayloadID, -1);
    bRepaired := false;

    // raise ECritical.create('Refunct isn''t fully implemented, implement the refunct process.');
  finally
    UnlockLock(l);
  end;
end;

procedure TVirtualDisk_Advanced.RemoveVatFromGapTrees
  (const vart: TVirtualAddressRecord);
var
  t: fi;
  fp: PFilePhysical;
  ps: TPayloadStream;
begin
  for t := 0 to vart.FileCount - 1 do
  begin
    fp := @vart.FPs[t];
    if fp.fileid >=0 then begin
      ps := FPayloadStreams[fp.FileID];
      if ps <> nil then begin
        FPayloadStreams[fp.FileID].gaps.FreeRegion
          (fp.PhysicalAddr_afterheader - sizeof(TVirtualDiskBigBlockHeader),
          GetBigBlockSizeInBytes(vart.FileCount, true));
      end;
    end;
  end;
end;

function TVirtualDisk_Advanced.RepairArcZone(zoneidx: ni): boolean;
var
  l: Tlock;
  dba: TDynByteArray;
  blk, iSum1,iSum2, iXor1,iXor2: int64;

begin
  result := false;
  l := GEtlock;
  try
    setlength(dba, ARC_ZONE_SIZE_IN_BYTES);
    blk := zoneidx shl ARC_ZONE_BLOCK_SHIFT;
    self.GuaranteeReadBlocks(blk, ARC_ZONE_SIZE_IN_BLOCKS, @dba[0]);
    self.GuaranteeWriteBlocks(blk, ARC_ZONE_SIZE_IN_BLOCKS, @dba[0]);
    result := true;
  finally
    unlocklock(l);
  end;

end;


procedure TVirtualDisk_Advanced.RepairbigBlock(varr: PVirtualAddressRecord;
  ps: TPayloadStream; pbbh: TVirtualDiskBigBlockHeader);
begin
  if pbbh.FooterStart + sizeof(TVirtualDiskBigBlockHeader) >= ps.Size then
  begin
    ps.GrowFile(pbbh.FooterStart + sizeof(TVirtualDiskBigBlockHeader));
    LogRepair(pbbh.VirtualAddress shr BIG_BLOCK_BYTE_SHIFT,'BBH extends after EOF.  Extending big block, data loss might occur, but RAID5 will be responsible for repairs');
  end;

end;

procedure TVirtualDisk_Advanced.Repair_payload(ps: TPayloadStream; id: ni);
var
  pfh: TVirtualDiskPayloadFileHeader;
  bbh, bbf: TVirtualDiskBigBlockHeader;
  vad: int64;
begin
  exit;
  // if (ps.Size-sizeof(TVirtualDiskPayloadFileHEader)) mod (2*sizeof(bbf)+GetBigBlockSizeInBytes(1)) <> 0 then begin
  // DEbug.Log(ps.filename+' is invalid size.... truncating.');
  // ps.Size := (ps.Size-sizeof(TVirtualDiskPayloadFileHEader)) div (2*sizeof(bbf)+GetBigBlockSizeInBytes(1) * (2*sizeof(bbf)+GetBigBlockSizeInBytes(1) ))+sizeof(TVirtualDiskPayloadFileHEader);
  // brepaired := false;
  // end;

  pfh := ps.GetPFH;
  // move to the first payload
  ps.Seek(sizeof(TVirtualDiskPayloadFileHeader), soBeginning);

  while ps.Position < ps.Size do
  begin
    // get the header
    Stream_GuaranteeRead(ps, pbyte(@bbh), sizeof(bbh), true);
    ps.Seek(ps.Position + bbh.PayloadSize, soBeginning);
    // get the footer
    Stream_GuaranteeRead(ps, pbyte(@bbf), sizeof(bbh), true);

    if bbh.VirtualAddress = bbf.VirtualAddress then
    begin
      vad := bbh.VirtualAddress;
      if vad > (int64(MAX_BLOCKS_IN_VAT) * int64(_BIG_BLOCK_SIZE_IN_BLOCKS *
        BLOCKSIZE)) then
      begin
        LogRepair(bbh.virtualaddress shr BIG_BLOCK_BYTE_SHIFT, 'Problem ' + inttostr(vad) +
          ' is beyond the end of the VAT.');
        bRepaired := false;
      end;

      if vad < 0 then
      begin
        LogRepair(bbh.virtualaddress shr BIG_BLOCK_BYTE_SHIFT, 'Problem ' + inttostr(vad) + ' is < 0.');
        bRepaired := false;
      end;

      {
        if bbh.VirtualAddress = bbf.VirtualAddress then
        if self.vat.table[bbh.VirtualAddress div (BLOCKSIZE * _BIG_BLOCK_SIZE_IN_BLOCKS)].FPs[u].fileid = -1 then begin
        Debug.Log('Found orphaned payload, assigning');
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

procedure TVirtualDisk_Advanced.ReplayLog;
{$IFDEF PLAY_OPS}
var
  p: PByte;
  cx: ni;
  iJust: ni;
  op: TRecordedOp;
  cs: int64;
  iCnt: ni;
  tmStart: ticker;
  cst: ni;
{$ENDIF}

begin
{$IFDEF PLAY_OPS}
  cx := oplog.Size;
  iCnt := 0;
//  while not IsOnline do begin
//    Debug.Log('Wait for online');
//    sleep(8000);
//  end;
  tmStart := getticker;
  while cx > 0 do begin
    if (icnt mod $1000) = 0 then
      Debug.Log('Playback: '+inttoHEX(iCnt,1));
    op.Init;
    stream_GuaranteeRead(oplog, @op, sizeof(op));
    if (op.block >= ($0 shl BIG_BLOCK_BLOCK_SHIFT)) then begin
      p := GetMemory(op.blockcount shl 9);
      try
        self.GuaranteeReadBlocks(op.block, op.blockcount, p);
        Front_StartPrefetches;
//        if random(10000) > 9990 then
//          sleep(random(1000));
        CalculateChecksum(p, op.blockcount shl 9, cs);

        if cs <> op.cs[1] then begin
        {$IFDEF FIX_OP_CS}
          oplog.seek(0-sizeof(op), soCurrent);
          op.cs[SET_CS] := cs;
          stream_GuaranteeWrite(oplog, @op, sizeof(op));
        {$ENDIF}
          Debug.Log('DIFFERENCE! '+op.debugstring);
        end;

      finally
        FreeMemory(p);
        p := nil;
      end;
    end;
    dec(cx, sizeof(op));
    inc(iCnt);
  end;

  Debug.Log('Done with test in '+ inttostr(gettimesince(tmStart) div 1000) +' seconds.');
  {$ifdef  TERMINATE_AFTER_PLAYBACK}
    windows.postmessage(uMainGUI.frmTotalDebug1.handle, WM_CLOSE, 0,0);
  {$ENDIF}


{$ENDIF}
end;

procedure TVirtualDisk_Advanced.ResetRepair;
var
  l: TLock;
  t: ni;
begin
  l := GetLock;
  try
    while FlushBuffers(true, 0, false, false) <> bsNoBuffersToFlush do ;
    self.vat.InitVirgin;
    for t:= 0 to FPayloadStreams.Count-1 do begin
      if FPayloadStreams[t] = nil then
        continue;
      FPayloadStreams[t].Size := 0;
      FPAyloadStreams[t].gaps.Length := 0;
    end;
    for t:= 0 to high(vat.table) do begin
      vat.table[t].FileCount := -1;
      vat.MarkTableEntryDirty(t);
    end;
  finally
    Unlocklock(l);
  end;

end;

procedure TVirtualDisk_Advanced.ResetZone(zid: int64);
var
  l: TLock;
  ent: TArcMapEntry;
begin
  l := GetLock;
  try
//    zonerevs.Lock;
    try
//      ent := self.zonerevs.GetEntry(zid);
//      ent.logid := -1;
//      self.zonerevs.PutEntry(zid);
    finally
//      zonerevs.unlock;
    end;
    vat.table[zid].FileCount := -1;
    vat.MarkTableEntryDirty(zid);
    SaveVatAndConfig;
    //LoadPayloadStreams;
  finally
    UnlockLock(l);
  end;
end;

procedure TVirtualDisk_Advanced.ReSourcePayload(iPayloadID: ni;
  sNewSource: string);
var
  l: TLock;
begin
  l := GetLock;
  try
    vat.PayloadConfig.filelist[iPayloadID].NameAsString := sNewSource;
    SaveVatAndConfig;
    LoadPayloadStreams;
  finally
    UnlockLock(l);
  end;
end;

procedure TVirtualDisk_Advanced.REstartBringOnline;
begin
  fillmem(pointer(@online_bigblocks[0]), sizeof(online_bigblocks), 0);
  Fbringonlineidx := MAX_BLOCKS_IN_VAT-1;
end;

{$IFDEF use_vat}

procedure TVirtualDisk_Advanced.SanityCheckPayloads;
var
  t: ni;
  ps: TPayloadStream;
  ga: TGapAnalyzer;
  l: TLock;
begin
  l := GetLock;
  try
    for t := 0 to FPayloadStreams.count - 1 do
    begin
      ps := FPayloadStreams[t];
      if ps <> nil then
      begin
        if (ps.size > $21) and (ps.Size <> FPayloadStreams[t].gaps.Length) then
          raise ECritical.Create('length sanity check fail for file ' +
            inttostr(t) + ' ' + inttohex(ps.Size, 1) + ' <> ' +
            inttohex(FPayloadStreams[t].gaps.Length, 2));
      end;
    end;
  finally
    UnlockLock(l);
  end;
end;

procedure TVirtualDisk_Advanced.SaveVat(bAll: boolean);
begin
{$IFDEF DETAILED_DEBUGGING}
  Debug.Log('Saving Vat with config '+self.debugvatstructure);
{$ENDIF}
  if fVatStream = nil then begin
    Debug.Log(self, 'vat stream is nil!  Already destroyed?');
    exit;
  end;
  vat.FlushToStream(FVATStream, bAll);
  // SaveStringAsFile(FVatStream.FileName+'.debug.txt', vat.DebugVatSTructure);
end;

procedure TVirtualDisk_Advanced.SaveVatAndConfig(sTo: string = '');
var
  fs: TUnbufferedFileStream;
begin
  if prioritizer <> nil then
    prioritizer.prepped := false;
  if FVatStream = nil then begin
    Debug.Log(self, 'vat stream is nil!');
    exit;
  end;

  vat.FlushToStream(FVATStream, true);

  if sTo <> '' then
  begin
    try
      fs := nil;
      try
        fs := TUnbufferedFileStream.Create(sTo, fmCreate);

        FVATStream.Seek(0, soBeginning);
        Stream_GuaranteeCopy(FVATStream, fs, FVATStream.Size);

      finally
        fs.free;
      end;
    except
      on e: EXception do begin
        AlertAdmin('err in SaveVatAndConfig: '+e.message);
      end;
    end;
  end;



  // SaveStringAsFile(FVatStream.FileName+'.debug.txt', vat.DebugVatSTructure);
  // FVatStream.FInalizeBuffers;
end;
{$ENDIF}


procedure TVirtualDisk_Advanced.SaveVatStats;
var
  ubs: TUnbufferedFileStream;
  sFile: string;
  bExists: boolean;

begin
  sFile := Self.FileName+'.vatstats';
  bExists := fileexists(sFile);

  ubs := nil;
  try
    if bExists then begin
      ubs := TUnbufferedFileStream.create(sFile, fmOpenReadWrite+fmShareExclusive);
    end else begin
      ubs := TUnbufferedFileStream.create(sFile, fmCreate);
    end;
    ubs.Seek(0,soBeginning);
    stream_GuaranteeWrite(ubs, pbyte(@vatstats), sizeof(vatstats));
  finally
    ubs.free;
    ubs := nil;
  end;
end;



procedure TVirtualDisk_Advanced.SelfTest(testid: ni);
begin
  StopScrubber;
  StartScrubber;
end;

procedure TVirtualDisk_Advanced.SelfTestOld;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TVirtualDisk_Advanced.SetCachedSTripes(const Value: ni);
var
  rab: TRaidAssembledBuffer;
  itm: TRaidTreeitem_ByLastUsedTime;
  l: TLock;
begin
  l := GetLock;
  try
    while FRaidsByBlock.count < Value do
    begin
      rab := TRaidAssembledBuffer.Create;
      FRaidsByBlock.addRaid(rab);
      FRaidsByLastUsed.addRAid(rab, TObject(rab.treeItemLastUsed));
      FRaidsByDirtyTime.addRaid(rab, TObject(rab.treeItemDirtyTime));
      FRaidsByDirtyTime.remove(rab.treeItemDirtyTime,true);
    end;
    while FRaidsByLAstUsed.count > Value do
    begin


      itm := FRaidsByLAstUsed.FIRSTITEM;
      rab := itm.rab;
      if rab=primary_buffer then
        primary_buffer := nil;
      SyncAwayBuffer(rab);
      FRaidsByBlock.RemoveRaid(rab);
//      Debug.Log('----------------------');
//      FRaidsByLAstUsed. DebugItems_ByLastUsed;
//      Debug.Log('----------------------');
      FRaidsByLAstUsed.RemoveRaid(rab, TObject(rab.treeItemLastUsed));

//      Debug.Log('----------------------');
//      FRaidsByDirtyTime. DebugItems_ByDirtyTime;
//      Debug.Log('----------------------');
      FRaidsByDirtyTime.RemoveRaid(rab, TObject(rab.treeItemLastUsed));
//      Debug.Log('----------------------');
//      FRaidsByDirtyTime. DebugItems_ByDirtyTime;
//      Debug.Log('----------------------');

      rab.free;

      rab := nil;
      primary_buffer := FRaidsByLastUsed.LastItem.rab;
    end;
  finally
    UnlockLock(l);
  end;
end;

procedure TVirtualDisk_Advanced.SetCAcheSize(const Value: int64);
begin
  FCacheSize := Value;
{$IFNDEF USE_STANDARD_STREAM}
  // if assigned(FStream) then
  // FStream.BufferSize := FCacheSize;
{$ENDIF}
end;

procedure TVirtualDisk_Advanced.SetDefaultCacheParams(iSegmentSize,
  iSegmentCount: ni; bReadAhead: boolean);
var
  t: ni;
  ps: TPayloadStream;
  l: TLock;
begin
  exit;
  l := GetLock;
  try
    vat.DefaultBufferSegmentSize := iSegmentSize;
    vat.DefaultBufferSegmentCount := iSegmentCount;
    vat.DefaultBufferReadAhead := bReadAhead;
    vat.MarkDirtyByLength(pbyte(@vat.DefaultBufferSegmentSize)-pbyte(@vat),sizeof(vat.DefaultBufferSegmentSize));
    vat.MarkDirtyByLength(pbyte(@vat.DefaultBufferSegmentCount)-pbyte(@vat),sizeof(vat.DefaultBufferSegmentCount));
    vat.MarkDirtyByLength(pbyte(@vat.DefaultBufferReadAhead)-pbyte(@vat),sizeof(vat.DefaultBufferReadAhead));
    SaveVat(false);
    for t := 0 to FPayloadStreams.count - 1 do
    begin
      ps := FPayloadStreams[t];
      if ps = nil then
        continue;
{$IFDEF PAYLOAD_HAS_QUEUED_INTERFACE}
//      ps.BufferSize := vat.DefaultBufferSegmentSize *
//        vat.DefaultBufferSegmentCount;
//      ps.BufferSEgments := vat.DefaultBufferSegmentCount;
//      ps.DisableLookAhead := not bReadAhead;
{$ENDIF}
    end;
  finally
    UnlockLock(l);
  end;

end;

procedure TVirtualDisk_Advanced.SetFileNAme(const Value: string);
var
  c: byte;
  iSize, bc: int64;
  l: TLock;
{$IFDEF PLAY_OPS}
  playcmd: Tcmd_ReplayLog;
{$ENDIF}
begin
  Debug.log(self, 'opening virtual disk '+value);

{$IFDEF RECORD_OPS}
  Debug.log(self, 'creating op log');
  oplog := TRecordFileStream.Create(self.FileName+'.oplog', fmCreate);
{$ENDIF}
{$IFDEF PLAY_OPS}
  oplog := TRecordFileStream.Create(self.FileName+'.oplog', fmOpenReadWrite);
{$ENDIF}


  try
    Forcedirectories(extractfilepath(Value));
  except
    on e: EXception do begin
      AlertAdmin('err in SetFileName/Forcedirectories: '+e.message);
    end;
  end;

  Debug.log(self, 'dealing with lck file.  Confused as to how this works now.');
  if fileexists(GetfileName + '.lck') then
    DeleteFile(GetfileName + '.lck');

  bRepaired := not fileexists(Value + '.lck');
  if not bRepaired then
    DeleteFile(Value + '.lck');

  SaveStringAsFile(Value + '.lck', 'weeeEEEEeeee');

  l := GetLock;
  try
    Debug.log(self, 'loading repair log');
    if fileexists(filename+'.repairlog') then
      RepairLog.LoadFromFile(filename+'.repairlog');

    if assigned(FVATStream) then
    begin
      FVATStream.free;
      FVATStream := nil;
    end;

    Debug.log(self, 'opening vat');
    if fileexists(Value) then
      FVATStream := TVatStream.Create(Value, fmOpenReadWrite + fmShareExclusive)
    else
    begin
      FVATStream := TVatStream.Create(Value, fmCreate);
    end;

{$IFNDEF USE_STANDARD_STREAM}
    // FStream.MinimumPrefetchSize := 999999999;
//    FVATStream.AllowReadPastEOF := true;
//    FVATStream.BufferSize := 256000000;
//    FVATStream.BufferSEgments := 256;
//    FVATStream.DisableLookAhead := true;
//    FVATStream.BufferSize := FCacheSize;
{$ENDIF}
{$IFDEF use_vat}
    iSize := Size;
    bc := MAX_BLOCKS_IN_VAT;
    LoadVat;
    if MAX_BLOCKS_IN_VAT < bc then
      Size := iSize;

    Debug.log(self, 'setting open marker in vat');
    vat.SetOpenMarker;

    SaveVat(false);
{$ELSE}
    if FStream <> nil then
    begin
      if FStream.Size < Size then
      begin
        FStream.Seek(Size - 1, 0);
        c := $55;
        FStream.Write(c, 1);
        FStream.Flush;
      end;
    end;
{$ENDIF}
    Debug.log(self, 'loading payloads');
    LoadPayloadStreams;
    Debug.log(self, 'loading vatstats');
    LoadVatStats;

    shipper.RingfileName := FileName+'.tmp';
    zonerevs.FileName := FileName+'.arcmap';

    resurrector.Reset;
    resurrector.Start;

    // Debug.Log('Stream is CReated.  Size is '+inttostr(FStream.size));


{$IFDEF PLAY_OPS}
  playcmd := Tcmd_ReplayLog.Create;
  playcmd.vda := self;
  playcmd.FireForget := true;
  playcmd.start;

{$ENDIF}

  finally
    UnlockLock(l);
  end;

//  DumpBigBlock(0);

end;

procedure TVirtualDisk_Advanced.SetPayloadPhysical(iFileID: ni;
  physical: int64);
var

  l: TLock;

begin
  l := GetLock;
  try
    lastpayloadchange := getticker;
    vat.PayloadConfig.filelist[iFileID].physical := physical;
    SaveVatAndConfig;
//    FVATStream.FinalizeBuffers;

  finally
    UnlockLock(l);
  end;

end;

procedure TVirtualDisk_Advanced.SetPayloadPriority(iFileID: ni;
  priority: int64);
var
  l: TLock;
begin
  l := GetLock;
  try
    lastpayloadchange := getticker;
    vat.PayloadConfig.filelist[iFileID].priority := priority;
    SortPayloadPriorities;
    SaveVatAndConfig;
//    FVATStream.FinalizeBuffers;

  finally
    UnlockLock(l);
  end;

end;

procedure TVirtualDisk_Advanced.SetPayloadQuota(iFileID: ni; max_size: int64);
var
  l: TLock;
begin
  l := GetLock;
  try
    if max_size < 0 then
      max_size := -1;
    lastpayloadchange := getticker;
    vat.PayloadConfig.filelist[iFileID].size_limit := max_size;
    SaveVatAndConfig;
//    FVATStream.FinalizeBuffers;

  finally
    UnlockLock(l);
  end;
end;

procedure TVirtualDisk_Advanced.SetSize(const Value: int64);
var
  c: byte;
begin
  inherited;
  // vat.block_count := 1+ (value div (BIG_BLOCK_SIZE_IN_BYTES));
{$IFNDEF use_vat}
  if FStream <> nil then
  begin
    if FStream.Size < Size then
    begin
      FStream.Seek(Value - 1, 0);
      c := $55;
      FStream.Write(@c, 1);
      FStream.Flush;
    end;
  end;

{$ENDIF}
end;

{$IFDEF BUFFER_BLOCKS}
{$ENDIF}
procedure TVirtualDisk_Advanced.SyncAwayAllBuffers;
begin
  FRaidsByBlock.Iterate(
    procedure([unsafe] ABTreeItem: TBTreeItem)
    var
      tmp: TRaidTreeItem_byBlock;
    begin
      tmp := TRaidTreeItem_byBlock(ABTreeItem);
      SyncAwayBuffer(tmp.rab);
    end);
end;

function TVirtualDisk_Advanced.SyncAwayBuffer(buf: TRaidAssembledBuffer;
bOnlyIfALLDirty: boolean = false): boolean;
// returns TRUE if buffer was dirty
var
  iStart, iEnd: ni;
  iDone: ni;
  iWrote: ni;
  bFirst: boolean;
  bReadingToSame: boolean;
  iOldStart, iOldEnd: int64;
  l: TLock;
begin
  result := false;
  l := GetLock;
  try
    // flush buffer
    iStart := 0;
    iEnd := -1;
    // if any blocks are dirtys
    if buf.AnyDirty then
    begin
      inc(vatstats.big_blocks[buf.StartingBlock shr BIG_BLOCK_BLOCK_SHIFT].Writes);
      if not buf.AllDirty then
      begin

        if bOnlyIfALLDirty and (not shuttingdown) THEN
          exit;

        bFirst := true;
        // if the starting block is valid
        if buf.StartingBlock >= 0 then
          while iStart < MAX_BUFFERED_BLOCKS do
          begin
            // find the first dirty block
            iStart := buf.FirstDirty(iEnd);
            if iStart >= MAX_BUFFERED_BLOCKS then
              break;
            // find the block after the last dirty block
            iEnd := buf.FirstClean(iStart);

            if bFirst then
            begin
              // if this block was not completely written through then we need to read the old stuff from the disk
              if (iEnd = MAX_BUFFERED_BLOCKS) and (iStart = 0) then
              begin
                // skip the fetch
              end
              else
              begin
                if not buf.ReadFromDisk then
                  FetchAndMergeRaid(buf);

                buf.ReadFromDisk := true;
              end;
            end;

            // bFirst := false;

            // write the blocks to RAID calculator
            // iWRote := WriteBlocks_RaidCalc(buffer, buffer.StartingBlock+iStart, iEnd-iStart, buffer.byteptr(iStart*BLOCKSIZE));
            // iStart := iStart + iWrote;
            // iStart := iEnd;
          end;
      end
      else
      begin
        // iWRote := WriteBlocks_RaidCalc(buffer, buffer.StartingBlock, RAID_STRIPE_SIZE_IN_BLOCKS, buffer.byteptr(0));
      end;



      FlushRaid(buf);
      // this does the actual splitting into pieces based on VAT information
      result := true;

      FRaidsByDirtyTime.Remove(buf.treeItemDirtyTime, true);
      buf.ClearDirty;
    end;



  finally

    UnlockLock(l);
  end;

end;

procedure TVirtualDisk_Advanced.SyncBuffer(lba: int64; bForWriting: boolean);
var
  iStart, iEnd: ni;
  iDone: ni;
  iWrote: ni;
  bFirst: boolean;
  bReadingToSame: boolean;
  bLAtentRead: boolean;
  iOldStart, iOldEnd: int64;
  buffer: TRaidAssembledBuffer;
  oblock: int64;
  vidx: int64;
begin

  BringBigBlockOnline(lba shr BIG_BLOCK_BLOCK_SHIFT);
  vidx := lba shr BIG_BLOCK_BLOCK_SHIFT;
{$IFDEF ASYNC_REBUILD}
  if WaitForRebuildLBARegion(lba, RAID_STRIPE_SIZE_IN_BYTES) then begin
    buffer := NeedRaid(lba);
    movemem32(@buffer.single[0], @currentRebuildOperation.data[(lba shl blockshift) and BIG_BLOCK_BYTE_ALIGN_MASK], RAID_STRIPE_SIZE_IN_BYTES);
    FlushBuffersInBlockOrder(lba, false, false);
    exit;
  end;
{$ELSE}


  IF VIDX >= MAX_BLOCKS_IN_VAT then
    raise ECritical.create('looking up VAT index '+inttostr(vidx)+' would be beyond max vat index.');
  if not disablecallupstats then
    inc(vatstats.big_blocks[vidx].Callups);

  //debug.Log(ltAll, 'Direct Fetch Block:'+lba.tohexstring+' Stripe:'+(lba shr STRIPE_SHIFT).ToHexString);
  if (not WorkingFromTargetArchive) and (not WorkingFromSourceArchive) and (lba >=0) then begin
    if vat.table[vidx].IsDead then begin
      //check for this block in the source archive
      if (not shipper.enabled) or (not REbuildBigBlockFromTargetARchive(vidx)) then begin
        if SourceArchivePinID > 0 then begin
          RebuildBigBlockFromSourceArchive(vidx);

        end;
      end;
    end else begin
      if (self.SourceArchiveHost <> '') and (vat.table[vidx].FileCount <= 0) and (vat.table[vidx].FileCount <> FILE_COUNT_NO_FETCH_FROM_SOURCE_ARCHIVE) then begin
        RebuildBigBlockFromSourceArchive(vidx);
      end;
    end;
  end;
{$ENDIF}

//  if lba = 1758 then
//    Debug.Log('Trap 1758');
  oblock := lba;
  if lba > 0 then
    lba := lba and RAID_ALIGN_BLOCK;
//  if lba = 0 then
//    Debug.Log('Trap align 0 : oblock='+inttostr(oblock));


  if bForWriting then
  begin
    // if this is for writing then we just need to get a buffer, period
    buffer := NeedRaid(lba)
  end
  else
  begin
    // else we need to
    // 1. get a buffer
    buffer := NeedRaid(lba);
    primary_buffer := buffer;

{$IFDEF USE_VAT_HINTS}
    if buffer.StartingBlock >= 0 then
      mem_vathints[buffer.StartingBlock shr STRIPE_SHIFT] := 1;
{$ENDIF}



    // if this buffer was read from the disk
    if buffer.ReadFromDisk then
      exit
    else
    begin
      // if it has been written to
      if (buffer.AnyDirty) then
      begin
        // fetch but merge written blocks
        if not(buffer.AllDirty) then
          FetchAndMergeRaid(buffer);
      end
      else
        // fetch blocks
        FixFetchRaid(buffer);
    end;
  end;

end;

procedure TVirtualDisk_Advanced.SyncReadData(const addr: int64;
  const cnt: nativeint; p: pbyte);
var
  l: TLock;
  tmLockSTart, tmDif: ticker;
begin
  hint_requests_waiting := true;

  l := getLock;
  try
    GuaranteeReadBlocks(addr shr BLOCKSHIFT, cnt shr BLOCKSHIFT, p);

  finally
    hint_requests_waiting := false;
    UnlockLock(l);
  end;

end;

procedure TVirtualDisk_Advanced.SyncRebuildOperation(bbid: int64);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TVirtualDisk_Advanced.SyncWriteData(const addr: int64;
  const cnt: nativeint; p: pbyte);
var
  l: Tlock;
begin
  hint_requests_waiting := true;
  l := GetLock;
  try
//    if (cnt mod BLOCKSIZE) <> 0 then
//      raise ECritical.Create('Cnt is not a multiple of block size');

//    if (addr mod BLOCKSIZE) <> 0 then
//      raise ECritical.Create('addr is not a multiple of block size');

    GuaranteeWriteBlocks(addr shr BLOCKSHIFT, cnt shr BLOCKSHIFT, p);
  finally
    hint_requests_waiting := false;
    UnlockLock(l);
  end;
end;


procedure TVirtualDisk_Advanced.TryHintNExtBigBlock(bb: int64;
  notin: PVirtualAddressRecord);
var
  ps: TPayloadStream;
  t: ni;
  vatrec: PVirtualAddressRecord;
  fid: ni;
begin
  {$IFNDEF PAYLOAD_IS_UNBUFFERED}
{  if bb >= MAX_BLOCKS_IN_VAT then
    exit;

  vatrec := @vat.table[bb];

  for t:= 0 to vatrec.filecount-1 do begin
    fid := vatrec.FPs[t].FileID;
    if fid >=0 then
      ps := FPayloadStreams[fid]
    else
      continue;

    if not (notin.HasFile(fid)) then
      ps.SetPRefetchHints(vatrec.FPs[t].PhysicalAddr);

  end;}
  {$ENDIF}


end;

procedure TVirtualDisk_Advanced.TryHintNExtBigBlock(fileid: ni; bb: int64);
var
  ps: TPayloadStream;
  fp: PFilePhysical;
begin
  {$IFNDEF PAYLOAD_IS_UNBUFFERED}
  if fileid < 0 then
    exit;

  if bb >= MAX_BLOCKS_IN_VAT then
    exit;

  if fileid > FPayloadStreams.count then
    exit;

  ps := FPayloadStreams[FileId];
  if ps = nil then exit;


  fp := vat.table[bb].GetFP(fileid);
  if fp <> nil then
    ps.SEtPrefetchHints(fp.PhysicalAddr_AfterHeader);

  {$ENDIF}
end;

procedure TVirtualDisk_Advanced.UnpauseScrubber;
begin
  lastpayloadchange := 0;
end;



procedure TVirtualDisk_Advanced.vdqueue_onidle(sender: Tobject);
begin
//  if queue.TryLock then
//  try
//    VD_SmartSideFetch;
//  finally
//    Queue.Unlock;
//  end;
end;


function TVirtualDisk_Advanced.WaitForQueueSpace(itimeout: ni): boolean;
begin
  result := queue.evNotCongested.WaitFor(iTimeout);
end;


function TVirtualDisk_Advanced.WaitForRebuildLBARegion(lba, bytelength: int64) : boolean;
var
  bb: int64;
  bTryTarget: boolean;
  bRequiresRebuild: boolean;
begin
  result := false;
  CompleteRebuildOperation(false);

  //if the region requires source or target fetch then
  bb := lba shr BIG_BLOCK_BLOCK_SHIFT;
  bTryTarget := vat.table[bb].IsDead;
  bRequiresRebuild := (self.SourceArchiveHost <> '') and (vat.table[bb].FileCount <= 0) and (vat.table[bb].FileCount <> FILE_COUNT_NO_FETCH_FROM_SOURCE_ARCHIVE);

  //if the region to wait for is NOT part of the currently-rebuilding big block
  if bRequiresRebuild or bTryTarget then begin
    if (currentRebuildOperation = nil) or (currentRebuildoperation.bbid <> bb) then begin
      //wait for entire rebuild operation
      CompleteREbuildOperation(true);
      //start a new rebuild operation
      StartRebuildOperation(bb, bTryTarget);
    end;

    //finally wait for region rebuild
    while currentREbuildOperation.progress.step < ((lba shl BLOCKSHIFT)+bytelength) do
      sleep(10);

    //grab the data from the currentRebuildOperation and write it to the cache

  end;

end;

procedure TVirtualDisk_Advanced.WriteBlock(const lba: int64; const p: pbyte);
begin
  WriteBlocks(lba, 1, p);
end;

var
  hits: integer;

function TVirtualDisk_Advanced.WriteBlocks(const lba: int64;
const cnt: nativeint; p: pbyte; bDontArchive: boolean = false): nativeint;
begin
  if not bInReconstitution then
    writes := (writes + 1) and $7FFFFFFFFFFFFFFF;

{$IFDEF BUFFER_BLOCKS}
  result := WriteBlocks_Buffered(lba, cnt, p, bDontArchive);
{$ELSE}
  result := WriteBlocks_Direct(lba, cnt, p);
{$ENDIF}
end;

{$IFDEF BUFFER_BLOCKS}

function TVirtualDisk_Advanced.WriteBlocks_Buffered(const lba: int64;
const cnt: nativeint; p: pbyte; bDontArchive: boolean = false): nativeint;
var
  i, ioffset, iByteOffset: nativeint;
  iToWrite: nativeint;
  l: TLock;
  buffer: TRaidAssembledBuffer;
begin
//  debug.log('writeblocks_buffered('+inttostr(lba)+', cnt='+inttostr(cnt));
  l := GetLock;
  try
//    if lba = 1758 then begin
//      debug.log('write blocks buffered '+memorydebugstring(p, 512));
//    end;
//
//    if (lba < 1758) and ((lba+cnt) > 1758) then begin
//      debug.log('write spans 1758'+memorydebugstring(p, 512));
//    end;

    // sync up the buffer to the desired position
    SyncBuffer(lba, true);

    // determine which slot in the buffer we're using
    ioffset := lba and (int64(RAID_STRIPE_SIZE_IN_BLOCKS - 1));

    // write the data to the buffer
    iByteOffset := BLOCKSIZE * ioffset;
    iToWrite := lesserof(cnt, RAID_STRIPE_SIZE_IN_BLOCKS - ioffset);
    result := iToWrite;
    iToWrite := iToWrite * BLOCKSIZE;

    // move data buffer offset $200 * 0,1,2,or 3
    // data
    // size of data (count * blocksize)
    buffer := primary_buffer;

    movemem32(buffer.byteptr(iByteOffset), p, iToWrite);

    // mark dirty
    FRaidsByDirtyTime.Remove(buffer.treeItemDirtyTime, true);

    buffer.archive := true;
    for i := ioffset to (ioffset + result) - 1 do
    begin
      buffer.dirty[i] := true;//todo 1 - use byte pointer instead of array indexing
      // !FIXME! Set them all dirty?  Is that important?

    end;
    FRaidsByDirtyTime.add(buffer.treeItemDirtyTime);
//    while FRAidsByDirtyTime.count > MAX_DIRTY_BUFFERS do begin
      while FRAidsByDirtyTime.count > (MAX_DIRTY_BUFFERS) do begin
        if FlushBuffers(true, 1000, false, false) <> bsFlushedSome then
          break;
      end;
//    end;

  finally
    UnlockLock(l);
  end;

end;
{$ENDIF}

function TVirtualDisk_Advanced.WriteBlocks_Direct(const lba: int64;
const cnt: nativeint; p: pbyte): nativeint;
begin
  result := WriteBlocks_Direct_Single(lba, cnt, p);
end;

function TVirtualDisk_Advanced.WriteBlocks_Direct_Single(const lba: int64;
const cnt: nativeint; p: pbyte): nativeint;
var
  actual_addr: TVirtualAddressRecord;
  end_actual_addr: TVirtualAddressRecord;
  bDifferentFiles: boolean;
  totalsize: int64;
  startingbigblock: int64;
  startingaddress: int64;
  fs: TPayloadStream;
  lcnt: nativeint;
  l: Tlock;
begin
  lcnt := cnt;

  result := cnt;

  l := GetLock;
  try
    totalsize := lcnt * BLOCKSIZE;
    startingbigblock := (lba div _BIG_BLOCK_SIZE_IN_BLOCKS) *
      _BIG_BLOCK_SIZE_IN_BLOCKS;
    startingaddress := lba * BLOCKSIZE;
    if (startingaddress) >= (Size + BLOCKSIZE) then
      raise ECritical.Create('Address beyond the size of the disk! ' +
        inttohex(lba * BLOCKSIZE, 2));

    GrowIfNeededBlock(lba + (lcnt - 1));
    actual_addr := VirtualToPhysicalAddr(lba * BLOCKSIZE);
    end_actual_addr := VirtualToPhysicalAddr((lba + lcnt) * BLOCKSIZE);
{$IFDEF ALLOW_RAID}
    if not(actual_addr.StartingBlock = end_actual_addr.StartingBlock) then
    begin
{$ELSE}
    // if the blocks are not all consecutive, we'll need to reduce the number of blocks we can write
    bDifferentFiles := actual_addr.FileID <> end_actual_addr.FileID;
    if bDifferentFiles or
      ((end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <>
      (totalsize)) then
    begin
{$ENDIF}

      // change the end actual address
      begin
        // find the beginning of the actual_addr bigblock

        end_actual_addr := VirtualToPhysicalAddr(startingbigblock * BLOCKSIZE);
{$IFNDEF ALLOW_RAID}
        end_actual_addr.PhysicalAddress := end_actual_addr.PhysicalAddress +
          (_BIG_BLOCK_SIZE_IN_BLOCKS * BLOCKSIZE);
{$ELSE}
        end_actual_addr := vat.GetTableEntryForLBA(startingbigblock)^;
        // end_actual_addr := VirtualToPhysicalAddr(((startingbigblock) * _BIG_BLOCK_SIZE_IN_BLOCKS)*BLOCKSIZE);
        end_actual_addr.FPs[0].PhysicalAddr_afterheader := end_actual_addr.FPs[0]
          .PhysicalAddr_afterheader + GetBigBlockSizeInBytes(1, false);
{$ENDIF}
      end;

      // actual that CAN be written
{$IFNDEF ALLOW_RAID}
      result := (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress)
        shr BLOCKSHIFT;
{$ELSE}
      result := (end_actual_addr.FPs[0].PhysicalAddr_afterheader - actual_addr.FPs[0]
        .PhysicalAddr_afterheader) shr BLOCKSHIFT;
{$ENDIF}
      lcnt := result;
      totalsize := lcnt * BLOCKSIZE;
{$IFNDEF ALLOW_RAID}
      if (end_actual_addr.PhysicalAddress - actual_addr.PhysicalAddress) <>
        (totalsize) then
      begin
        raise ECritical.Create('Block segment fault failure.  ' +
          inttostr((end_actual_addr.PhysicalAddress -
          actual_addr.PhysicalAddress)) + ',' + inttostr(totalsize));
{$ELSE}
      if (end_actual_addr.FPs[0].PhysicalAddr_afterheader - actual_addr.FPs[0].PhysicalAddr_afterheader)
        <> (totalsize) then
      begin
        raise ECritical.Create('Block segment fault failure.  ');
{$ENDIF}
      end;
    end;
    // Debug.Log(inttohex(lba*BLOCKSIZE, 2)+'<-actual_addr(wb)->'+inttohex(actual_addr, 2));




    // Debug.Log('About to write to virtual disk at '+inttohex(actual_addr.Address,0)+' for lba '+inttohex(lba,0)+' lcnt:'+inttostr(lcnt));

{$IFDEF ALLOW_RAID}
    fs := FPayloadStreams[actual_addr.FPs[0].FileID];
{$IFDEF USESEEKLOCK} fs.SeekLock; {$ENDIF}
    try
      fs.Seek(actual_addr.FPs[0].PhysicalAddr_afterheader, 0);
{$ELSE}
    fs := FPayloadStreams[actual_addr.FileID];
{$IFDEF USESEEKLOCK} fs.SeekLock; {$ENDIF}
    try
      fs.Seek(actual_addr.PhysicalAddress, 0);
{$ENDIF}
      Stream_GuaranteeWrite(fs, p, BLOCKSIZE * lcnt)
    finally
{$IFDEF USESEEKLOCK} fs.seekunlock; {$ENDIF}
    end;

  finally
    UnlockLock(l);
  end;

end;

function TVirtualDisk_Advanced.WriteBlocks_RaidCalc
  (const rab: TRaidAssembledBuffer; const lba: int64; const cnt: nativeint;
p: pbyte): nativeint;
var
  lba_off: int64;
  iCanRead: int64;
  buffer: TRaidAssembledBuffer;
begin
  // determine offset into RAID space
  buffer := rab;
  lba_off := lba - buffer.StartingBlock;
  if (lba_off < 0) or (lba_off >= RAID_STRIPE_SIZE_IN_BLOCKS) then
  begin
    raise ECritical.Create
      ('Something is wrong and lba is not within RAID stripe.');
  end;

  iCanRead := lesserof(RAID_STRIPE_SIZE_IN_BLOCKS - lba_off, cnt);

  movemem32(buffer.byteptr(lba_off * BLOCKSIZE), p, iCanRead * BLOCKSIZE);

  result := iCanRead;

end;

{$IFDEF ALLOW_RAID}

procedure TVirtualDisk_Advanced.WriteData(const addr: int64;
const cnt: nativeint; p: pbyte);
{$IFDEF VD_QUEUE}
var
  qi: TVDWriteCommand;
{$ENDIF}
begin
{$IFNDEF VD_QUEUE}
  SyncWriteData(addr, cnt, p);
{$ELSE}
  qi := TVDWriteCommand.Create;
  qi.vd := self;
  qi.addr := addr;
  qi.count := cnt;
  if (cnt < 0) or (cnt > 100000000) then
    raise ECritical.create('invalid data count in writedata '+inttostr(cnt));
  qi.p := getmemory(cnt);
  movemem32(qi.p, p, cnt);
  qi.autodestroy := true;
//  Debug.Log('About to add '+qi.Getobjectdebug);
  queue.AddItem(qi);
//  qi.waitfor;
//  qi.Free;
{$ENDIF}
end;

procedure TVirtualDisk_Advanced.ZoneLog(idx: int64; sMessage: string);
begin
  zl.Log(idx, sMessage);
  //Debug.Log('ZL BB '+idx.tohexstring+': '+sMessage);
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
  FStream.Seek(Self.addr, 0);
  Stream_GuaranteeRead(FStream, data, Size);

end;

procedure TFileBlock.Write(FStream: TStream);
begin
  FStream.Seek(Self.addr, 0);
  Stream_GuaranteeWrite(FStream, data, Size);

end;

{ TBlockLevelCacheList }

procedure TBlockLevelCacheList.AddBlock(b: TFileBlock);
begin
  Self.FList.add(b);

end;

constructor TBlockLevelCacheList.Create;
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
  result := FList.IndexOf(addr);
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
  result := BLOCKSIZE * 64;
end;

constructor TAbstractVirtualDisk_Advanced.Create;
begin
  inherited;
  backgroundLock := TLockQueuedObject.create;
{$IFDEF use_vat}
  vat.InitVirgin;
{$ENDIF}
{$IFDEF STATIC_VAT_HINTS}
  mem_vathints := GEtmemory(MAX_RAID_STRIPES);
  //setlength(vathints, MAX_RAID_STRIPES);
{$ENDIF}
  zonerevs := TArcMap.create;

  EnableOptionalDebugging := true;
  shipper := TArcLogShipper.create;
  shipper.disk := self;



end;

destructor TAbstractVirtualDisk_Advanced.Destroy;
begin
  if assigned(shipper) then begin
    shipper.free;
    shipper := nil;
  end;
  zonerevs.free;
  inherited;
  freememory(mem_vathints);
  backgroundLock.free;
  backgroundLock := nil;
end;

procedure TAbstractVirtualDisk_Advanced.FlexRead(const addr: int64;
const cnt: nativeint; p: pbyte);
var
  pblock: pbyte;
  iCan, itot, ipos, iEnd, iOFF: int64;
begin
  pblock := GetMemory(BLOCKSIZE);
  try
    ipos := addr;
    iEnd := ipos + cnt;
    itot := 0;
    while ipos < (iEnd) do
    begin
      iOFF := ipos shr blockshift;//TODO 1 - Is this even right?
      iCan := lesserof(BLOCKSIZE - iOFF, iEnd - ipos);
      ReadBlock(addr shr BLOCKSHIFT, pblock);
      movemem32(@p[itot], @pblock[iOFF], iCan);
      inc(itot, iCan);
      inc(ipos, iCan);
    end;

  finally
    Freememory(pblock);
  end;

end;

procedure TAbstractVirtualDisk_Advanced.FlexWrite(const addr: int64;
const cnt: nativeint; p: pbyte);
var
  pblock: pbyte;
  iCan, itot, ipos, iEnd, iOFF: int64;
begin
  pblock := GetMemory(BLOCKSIZE);
  try
    ipos := addr;
    iEnd := ipos + cnt;
    itot := 0;
    while ipos < (iEnd) do
    begin
      iOFF := ipos shr blockshift;//TODO 1 - IS THIS EVEN RIGHT?
      iCan := lesserof(BLOCKSIZE - iOFF, iEnd - ipos);
      // we need to read the block before writing it if the offset is in the middle somewhere
      if (iOFF > 0) or ((iEnd - ipos) < 512) then
        ReadBlock(addr shr BLOCKSHIFT, pblock);
      movemem32(@pblock[iOFF], @p[itot], iCan);
      WriteBlock(addr shr BLOCKSHIFT, pblock);

      inc(itot, iCan);
      inc(ipos, iCan);
    end;

  finally
    Freememory(pblock);
  end;
end;

function TAbstractVirtualDisk_Advanced.GetIdentifier: string;
var
  l: TLock;
begin
//  l := GetLock;
  try
    result := FIdentifier;
  finally
//    UnlockLock(l);
  end;
end;

function TAbstractVirtualDisk_Advanced.GetStatus: string;
var
  l: TLock;
begin
  l := getLock();
  try
    result := FStatus;
  finally
    unlocklock(l);
  end;
end;

function TAbstractVirtualDisk_Advanced.GEtVerboseDebug(sToFile: string)
  : boolean;
var
  t: ni;
  sl: TStringlist;
  s: string;
begin
  result := true;

  sl := TStringlist.Create;
  try
    for t := 0 to MAX_BLOCKS_IN_VAT - 1 do
    begin
      if vat.table[t].FileCount > 0 then
      begin
        s := vat.DebugVatSTructure;
        if s <> '' then
          sl.add(s);
      end;
    end;
    if sToFile<>'' then
      sl.SaveToFile(sToFile);
  finally

    sl.free;
  end;

end;

function TAbstractVirtualDisk_Advanced.GEtVerboseDebug(iBigBlock: ni): string;
var
  t, u: ni;
  sl: TStringlist;
  a: array [0 .. BLOCKSIZE - 1] of byte;
  vart: PVirtualAddressRecord;
  l: TLock;
begin
  result := '';
  exit;
  vart := @Self.vat.table[iBigBlock];
  l := GetLock;
  try
    sl := TStringlist.Create;
    try
      sl.add('***********BIG BLOCK ' + inttohex(vart.StartingBlock *
        BLOCKSIZE, 0));
      for t := 0 to _BIG_BLOCK_SIZE_IN_BLOCKS - 1 do
      begin
        Self.ReadBlock(vart.StartingBlock + t, @a[0]);
        for u := 0 to BLOCKSIZE - 1 do
        begin
          if (u and $7F) <> 0 then
            continue;
          sl.add(memorydebugstring(@a[u], 63));
        end;
      end;
      result := sl.text;
    finally
      sl.free;
    end;

  finally
    UnlockLock(l);
  end;

end;

procedure TAbstractVirtualDisk_Advanced.GuaranteeReadBlocks(lba: int64;
cnt: nativeint; p: pbyte);
var
  iJustRead, iRead: nativeint;
  l: TLock;
begin
{$IFDEF LOCK_GUARANTEED_OPS}
  l := GetLock;
  try
{$ENDIF}
    // Debug.Log('GRB:'+inttostr(lba)+','+inttostr(cnt));
    iRead := 0;
    iJustRead := 0;
    while cnt > 0 do
    begin
      iJustRead := ReadBlocks(lba, cnt, @p[iRead * BLOCKSIZE]);
      inc(lba, iJustRead);
      dec(cnt, iJustRead);
      inc(iRead, iJustRead);
    end;
{$IFDEF LOCK_GUARANTEED_OPS}
  finally
    UnlockLock(l);
  end;
{$ENDIF}

end;

procedure TAbstractVirtualDisk_Advanced.GuaranteeWriteBlocks(lba: int64;
cnt: nativeint; p: pbyte; bDontArchive: boolean = false);
var
  iJustWrote, iWrote: nativeint;
  l: TLock;
begin
{$IFDEF LOCK_GUARANTEED_OPS}
  l := GetLock;
  try
{$ENDIF}
  // Debug.Log('GWB:'+inttostr(lba)+','+inttostr(cnt));
  iWrote := 0;
  while cnt > 0 do
  begin
    iJustWrote := WriteBlocks(lba, cnt, @p[iWrote * BLOCKSIZE], bDontArchive);
    inc(lba, iJustWrote);
    dec(cnt, iJustWrote);
    inc(iWrote, iJustWrote);
  end;
{$IFDEF LOCK_GUARANTEED_OPS}
  finally
    UnlockLock(l);
  end;
{$ENDIF}
end;

procedure TAbstractVirtualDisk_Advanced.optdebug(s: string);
begin
  if EnableOptionalDebugging then
    debug.Log(self,s);

end;

procedure TAbstractVirtualDisk_Advanced.PreShutdown;
begin
  shipper.Enabled := false;

end;

procedure TAbstractVirtualDisk_Advanced.SetIdentifier(const Value: string);
var

  l: TLock;
begin
  l := GetLock;
  try
    FIdentifier := 'iqn-2008-08.com.digitaltundrallc:' + systemx.GetComputerName
      + '-' + Value;
  finally
    UnlockLock(l);
  end;
end;

procedure TAbstractVirtualDisk_Advanced.SetSize(const Value: int64);
begin
  FRequestedSize := Value;
  FSize := (FRequestedSize shr BLOCKSHIFT) * BLOCKSIZE;

end;

procedure TAbstractVirtualDisk_Advanced.SetStatus(Value: string);
var
  l: TLock;
begin
  l := getLock();
  try
    FStatus := value;
  finally
    unlocklock(l);
  end;
end;

function TAbstractVirtualDisk_Advanced.TailStartAddr: int64;
begin
  result := Size - TailSize;
end;

function TAbstractVirtualDisk_Advanced.TailSize: int64;
begin
  result := 8192 * BLOCKSIZE;
end;

function TVirtualDisk_Advanced.VirtualToPhysicalAddr(addr: int64)
  : TVirtualAddressRecord;
{$IFNDEF use_vat}
begin
  result := addr;
end;
{$ELSE}

var
  r: PVirtualAddressRecord;
  tindex: nativeint;
  varr: TVirtualAddressRecord;
begin
  // debug.log('Input:'+inttostr(addr));

  // result.PhysicalAddress := addr;
  tindex := addr div (BLOCKSIZE * _BIG_BLOCK_SIZE_IN_BLOCKS);
  r := vat.GetTableEntryForLBA(addr shr BLOCKSHIFT);

  // debug.log('Initial translation:'+inttostr(r.Address));
{$IFDEF ALLOW_RAID}
  if r.FileCount = 0 then
  begin
{$ELSE}
  if not r.IsAssigned then
  begin
{$ENDIF}
    // {$MEssage Error 'r.Address can be 0 now... must initialize differently'}

    varr := NewPhysicalBlockAddress((addr shr BLOCKSHIFT) * BLOCKSIZE, varr.priority_target,varr.priority_target, PLACED_BY_VIRTUAL_TO_PHYSICAL);
    r := vat.GetTableEntryForLBA(addr shr BLOCKSHIFT);

{$IFNDEF ALLOW_RAID}
    r.PhysicalAddress := varr.PhysicalAddress;
{$ELSE}
    // r^ := varr;
{$ENDIF}
    // r.Marker := varr.Marker;
{$IFDEF ALLOW_RAID}
    // r.FPs[0] := varr.FPs[0];
{$ELSE}
    // r.FileID := varr.FileID;
{$ENDIF}
    // r.StartingBlock := tindex * _BIG_BLOCK_SIZE_IN_BLOCKS ;
    vat.MarkTableEntryDirtyByPtr(r);
    SaveVat(false);
  end;

{$IFDEF ALLOW_RAID}
  result := r^;

{$ELSE}
  result.FileID := r.FileID;
{$ENDIF}
  // Log('Prefinal:'+inttostr(r.Address));
{$IFDEF ALLOW_RAID}
  result.FPs[0].PhysicalAddr_afterheader := r.FPs[0].PhysicalAddr_afterheader +
    (addr shr BIG_BLOCK_BYTE_SHIFT);
{$ELSE}
  result.PhysicalAddress := r.PhysicalAddress +
    (addr shr BIG_BLOCK_BYTE_SHIFT);
{$ENDIF}
  // Log('Final:'+inttostr(result.Address));

{$IFDEF ALLOW_RAID}
{$ELSE}
  if result.PhysicalAddress < 0 then
    raise ECritical.Create('wtf!');
{$ENDIF}
end;
{$ENDIF}
{ TVirtualAddressTable }

{$IFDEF use_vat}

procedure TVirtualAddressTable.ChangeAllFiles(iFromID, iToID: ni);
var
  t, u: ni;
  vart: PVirtualAddressRecord;
  pf: PFilePhysical;
begin
  for t := 0 to high(table) do
  begin
    vart := @table[t];
    for u := 0 to vart.FileCount - 1 do
    begin
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
  for t := 0 to MAX_BLOCKS_IN_VAT - 1 do
  begin
    if table[t].FileCount > 0 then
      inc(result);
  end;

end;

function TVirtualAddressTable.DebugVatSTructure: string;
var
  t, u: ni;
  sl: TStringlist;
begin
  sl := TStringlist.Create;
  try
    t := 0;
    while t < ( high(Self.table)) do
    begin

{$IFDEF ALLOW_RAID}
      if Self.table[t].FPs[0].PhysicalAddr_afterheader >= 0 then
        sl.add(inttohex(t,1)+':'+Self.table[t].DebugString);
{$ELSE}
      if Self.table[t].FileID >= 0 then
        sl.add(inttohex(Self.table[t].StartingBlock, 8) + ' ' +
          inttohex(Self.table[t].FileID, 8) + ' ' +
          inttohex(Self.table[t].PhysicalAddress, 16));
{$ENDIF}
      inc(t);
    end;

    result := sl.text;
  finally
    sl.free;
  end;

end;

function TVirtualAddressTable.FindFilePhysical(iDISKID: ni; greaterthan: int64)
  : PFilePhysical;
var
  t, u: ni;
  vart: PVirtualAddressRecord;
  pf: PFilePhysical;
begin
  result := nil;
  for t := low(table) to high(table) do
  begin
    vart := @table[t];
    for u := 0 to vart.FileCount - 1 do
    begin
      pf := vart.GetFP(iDISKID);
      if pf <> nil then
      begin
        if pf.PhysicalAddr_afterheader > greaterthan then
        begin
          if (result = nil) or (result.PhysicalAddr_afterheader > pf.PhysicalAddr_afterheader) then
          begin
            result := pf;
          end;
        end;
      end;
    end;
  end;
end;

function TVirtualAddressTable.FindGapLocation_Deprecated(sz, FileID: fi;
out physical: int64; not_this_physical: int64): boolean;
var
  t: fi;
  vart: PVirtualAddressRecord;
  fp: PFilePhysical;
  gapsize: int64;
  varnext: PVirtualAddressRecord;
  fpnext: PFilePhysical;
  bbs: int64;
begin
  physical := $FFFFFFFFFFFFFFF;
  result := false;
  for t := low(table) to high(table) do
  begin
    vart := @table[t];
    if not vart.HasFile(FileID) then
      continue;
    // get the physical couple for this particular file in the VAT
    fp := vart.GetFP(FileID);

    // get the block that follows this one in the file
    varnext := FindNextBlockForFileIDInVat_Deprecated(FileID, fp.PhysicalAddr_afterheader);;
    // if no next block is found, continue
    if varnext = nil then
      continue;

    fpnext := varnext.GetFP(FileID);
    if fpnext = nil then
      continue;

    // calculate size of gap taking into account the size of current block
    bbs := GetBigBlockSizeInBytes(vart.FileCount, true);
    gapsize := (fpnext.PhysicalAddr_AfterHeader - fp.PhysicalAddr_AfterHeader) - bbs;

    // if the gap is big enough, then use it
    if (gapsize >= sz) and (fp.PhysicalAddr_AfterHeader <> not_this_physical) then
    begin
      Self.MarkTableEntryDirty(t);
      physical := fp.PhysicalAddr_AfterHeader + bbs;
      result := true;
      exit;
    end;

  end;
end;

function TVirtualAddressTable.FindNextBlockForFileIDInVat_Deprecated(FileID: fi;
beyond: int64): PVirtualAddressRecord;
var
  t: fi;
  vart: PVirtualAddressRecord;
  bHas: boolean;
  iMin: int64;
  fp: PFilePhysical;
begin
  iMin := $7FFFFFFFFFFFFFFF;

  result := nil;
  // return the minimum physical address that is greater than "beyond"
  for t := low(table) to high(table) do
  begin
    vart := @table[t];
    bHas := vart.HasFile(FileID);
    if not bHas then
      continue;
    fp := vart.GetFP(FileID);
    if (fp.PhysicalAddr_AfterHeader < iMin) and (fp.PhysicalAddr_AfterHeader > beyond) then
    begin
      iMin := fp.PhysicalAddr_AfterHeader;
      result := vart;
    end;
  end;

end;

function TVirtualAddressTable.FindVARToBuddyUp: PVirtualAddressRecord;
var
  t: fi;
begin
  result := nil;
  for t := low(table) to high(table) do
  begin
    if (table[t].FileCount = 1) and (table[t].FPs[0].FileID > -1) then
    begin
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
  for t := low(table) to high(table) do
  begin
    if table[t].HasFile(-1) then
    begin
      result := @table[t];
      exit;
    end;
  end;
end;

function TVirtualAddressTable.FindVARToReconstitute: PVirtualAddressRecord;
var
  t, u: fi;
  max_phys: int64;
  pf: PFilePhysical;

begin
  result := nil;
  max_phys := -1;
  // find the block with the highest physical address on a drive with a negative priority
  for t := low(table) to high(table) do
  begin
    for u := 0 to table[t].FileCount - 1 do
    begin
      pf := @table[t].FPs[u];
      if pf.FileID >= 0 then
      begin
        // IF any drives in the table are marked for reconstitution
        if PayloadConfig.filelist[pf.FileID].priority < 0 then
        begin
          // if the physical address for this block is highest then save it as the result
          if pf.PhysicalAddr_AfterHeader > max_phys then
          begin
            result := @table[t];
            if result.VatIndex <> t then begin
              zl.Log(t, ' in FindVARToReconstitue, record index '+result.vatindex.tohexstring+' did not match expected ('+t.tohexstring+').');
              result.VatIndex := t;
            end;
            max_phys := pf.PhysicalAddr_AfterHeader;
          end;
        end;
      end;
    end;
  end;
end;

function TVirtualAddressTable.FindVarWithHighestPhysicalForPayload
  (iPayloadID: ni): PVirtualAddressRecord;
var
  pf: PFilePhysical;
  t: ni;
  max: int64;
begin
  result := nil;
  max := -1;

  for t := 0 to high(table) do
  begin
    pf := table[t].GetFP(iPayloadID);
    if pf <> nil then
    begin
      if pf.PhysicalAddr_AfterHeader > max then
      begin
        max := pf.PhysicalAddr_AfterHeader;
        result := @table[t];
      end;
    end;
  end;

end;

procedure TVirtualAddressTable.FlushToStream(s: TStream; bForce: boolean);
var
  iStart, iEnd: int64;
  function CompareWithFileChangePointers(var iFileDirtyStart: int64; var iFileDirtyEnd: int64): boolean;
  var
  t: ni;
    a: array of byte;
    b,bb: byte;
    bp: pbyte;
    newstart, newEnd: int64;
  begin
    result := false;
    //RETURNS FALSE IF NO POINTERS CHANGED
    //RETURNS TRUE IF POINTERS CHANGED
    setlength(a, s.size);
    S.SEEK(0, soBeginning);
    stream_GuaranteeRead(s, @a[0], s.size);
    bp := PByte(@self);
    newstart := lesserof(int64(@end_of_persistence)-int64(@self), s.size);
    newend := -1;
    for t:= 0 to lesserof(int64(@end_of_persistence)-int64(@self), s.size)-1 do begin
      b := a[t];
      bb := bp[t];
      if b <> bb then begin
        if t < newstart then
          newstart := t;
        if t > newend then
          newend := t;
      end;

    end;

    if newend >-1 then begin
      if newend = -1 then begin
        newstart := -1;
        newend := 0;
      end;
      if newstart < iFileDirtyStart then begin
        Debug.Log('Dirty START did not match content check! Patching!  This is not ideal.');
        iFiledirtyStart := newstart;
        result := true;
      end;
      if newend > iFileDirtyEnd then begin
        Debug.Log('Dirty END did not match content check! Patching!  This is not ideal.');
        iFileDirtyEnd := newend;
        result := true;
      end;
    end;

  end;

begin
  if (not bForce) and ((dirtyStart = -1) and (dirtyEnd = 0)) then
    exit;
  // if (iStart < 0) and (iEnd < 0) then exit;
  // dirtyStart := -1;
  // dirtyEnd := -1;
  try

  finally

  end;
  //debug.Log('flush [' + inttostr(dirtyStart) + '-' + inttostr(dirtyEnd) + ']');
  iStart := dirtyStart;
  iEnd := dirtyEnd;

{$IFDEF COMPARE_ENTIRE_VAT_ON_SAVE}
  CompareWithFileChangePointers(iStart, iEnd);
{$ENDIF}


  if (iStart < 0) or bForce then
    iStart := 0;
  if (iEnd < 0) or bForce then
    iEnd := pbyte(Self.persistent_End) - pbyte(@Self);

  s.Seek(iStart, 0);
  // Debug.Log('Flush:'+NEWLINE+self.DebugVatSTructure);
  Stream_GuaranteeWrite(s, pbyte(@Self) + iStart, (iEnd - iStart) + 1);
  dirtyStart := -1;
  dirtyEnd := 0;
  if s is TMultiBufferMemoryFileStream then
    TMultiBufferMemoryFileStream(s).Flush;

end;

function TVirtualAddressTable.GetFileCount: cardinal;
var
  t: nativeint;
begin
  if reserved = 0 then
  begin
    t := 0;
    while t < high(Self.PayloadConfig.filelist) do
    begin
      if Self.PayloadConfig.filelist[t].name <> '' then
      begin
        inc(reserved);
      end;
      inc(t);
    end;
    markdirtybyLength(pbyte(@reserved)-pbyte(@self), sizeof(reserved));
  end;
  result := reserved;


end;

function TVirtualAddressTable.GetSafePhysicalAddr(iFileID: ni): int64;
var
  t, u: ni;
  vart: PVirtualAddressRecord;
  fpf: PFilePhysical;
begin

  vart := FindVarWithHighestPhysicalForPayload(iFileID);
  result := vart.GetFP(iFileID).PhysicalAddr_AfterHeader + GetBigBlockSizeInBytes
    (vart.FileCount, false) + sizeof(TVirtualDiskBigBlockHeader);

  if vart <> nil then
  begin
    result := vart.GetFP(iFileID).PhysicalAddr_AfterHeader + GetBigBlockSizeInBytes
      (vart.FileCount, false) + sizeof(TVirtualDiskBigBlockHeader);
  end;

end;

function TVirtualAddressTable.GetTableEntryForLBA(lba: int64)
  : PVirtualAddressRecord;
var
  t: nativeint;
begin
  lba := lba shr BIG_BLOCK_BLOCK_SHIFT;
//  if lba = $5a then
//    Debug.Log('here');
  result := @table[lba];

end;

function TVirtualAddressTable.GetTableEntryPhysicalAddress(iFileID: nativeint;
physical: int64): PVirtualAddressRecord;
var
  t: nativeint;
  pf: PFilePhysical;
begin
  result := nil;
  for t := low(table) to high(table) do
  begin
{$IFDEF ALLOW_RAID}
    pf := table[t].GetFP(iFileID, physical);
    if pf <> nil then
    begin
      result := @table[t];
      break;
    end;
{$ELSE}
    if (table[t].FileID = iFileID) and (table[t].PhysicalAddress = physical)
    then
    begin
      result := @table[t];
      break;
    end;
{$ENDIF}
  end;

  // if result = nil then
  // raise ECritical.create('Physical Address not found.  File is corrupt.');

end;

function TVirtualAddressTable.HasFile(iPayloadID: ni): boolean;
begin
  result := FindVarWithHighestPhysicalForPayload(iPayloadID) <> nil;
end;

procedure TVirtualAddressTable.InitVirgin;
var
  t: ni;
begin
  Marker := $55;
  for t := low(Self.table) to high(table) do
  begin
    table[t].InitVirgin(t * _BIG_BLOCK_SIZE_IN_BLOCKS);
    table[t].StartingBlock := t * _BIG_BLOCK_SIZE_IN_BLOCKS;
    MarkTableEntryDirty(t);
  end;

end;

function TVirtualAddressTable.LocalAddr(p: pointer): int64;
begin
  result := pbyte(p) - pbyte(@Self);

end;

procedure TVirtualAddressTable.MarkDirtyByLength(const iStart, iLEngth: ni);
begin
  needsbackup := true;
  MarkDirtyRange(iStart, iStart + iLEngth - 1);
end;

procedure TVirtualAddressTable.MarkDirtyRange(const iStart, iEnd: int64);
begin
  needsbackup := true;
  if (iStart < Self.dirtyStart) or (Self.dirtyStart < 0) then
  begin
    if iStart >= PersistentSize then
    begin
      debug.Log('wtf iStart=' + inttostr(iStart));
      dirtyStart := 0;
    end
    else
      dirtyStart := iStart;
  end;

  if (iEnd > Self.dirtyEnd) or (Self.dirtyEnd < 0) then
  begin
    dirtyEnd := iEnd;
  end;

end;

procedure TVirtualAddressTable.MarkDirtyByLength(ptr: pointer; iLEngth: ni);
var
  iStart:int64;
begin
  iStart := LocalAddr(ptr);
  MarkDirtyByLength(iStart, iLength);

end;

procedure TVirtualAddressTable.MarkTableEntryDirty(idx: ni);
var
  i1, i2, i3, iStart, iEnd: int64;
begin
  i1 := (idx * sizeof(TVirtualAddressRecord));
  i2 := int64(pbyte(@table[0]));
  i3 := int64(@Self);
  iStart := i1 + (i2 - i3);
  iEnd := iStart + sizeof(TVirtualAddressRecord) - 1;

  MarkDirtyByLength(iStart, sizeof(TVirtualAddressRecord));


end;

procedure TVirtualAddressTable.MarkTableEntryDirtyByPtr
  (ptr: PVirtualAddressRecord);
var
  iStart, iEnd: int64;
begin
  needsbackup := true;
  iStart := pbyte(ptr) - pbyte(@Self);
  if iStart < 0 then
    raise Ecritical.create('bad start address '+inttohex(iStart, 1));
  iEnd := iStart + sizeof(TVirtualAddressRecord) - 1;

  if (iStart < Self.dirtyStart) or (Self.dirtyStart < 0) then
  begin
    dirtyStart := iStart;
  end;

  if (iEnd > Self.dirtyEnd) or (Self.dirtyEnd < 0) then
  begin
    dirtyEnd := iEnd;
  end;

end;

function TVirtualAddressTable.persistent_End: pbyte;
begin
  result := pbyte(@end_of_persistence)
end;

function TVirtualAddressTable.PersistedSize: ni;
begin
  result := pbyte(Self.persistent_End) - pbyte(@Self);

end;

function TVirtualAddressTable.PersistentSize: int64;
begin
  result := pbyte(Self.persistent_End) - pbyte(@Self)
end;

procedure TVirtualAddressTable.ReadFromStream(s: TStream);
var
  sz: ni;
begin
  s.Seek(0, 0);
  sz := nativeint(pbyte(Self.persistent_End) - pbyte(@Self));
  Stream_GuaranteeRead(s, pbyte(@Self), sz);
  // Debug.Log('Read STructure:'+NEWLINE+self.DebugVatSTructure);
  reserved := 0;
  markdirtybyLength(pbyte(@reserved)-pbyte(@self), sizeof(reserved));

end;

procedure TVirtualAddressTable.SetCloseMarker;
begin
  CloseConfirm := OpenTime;
  MarkDirtyByLength(pbyte(@CloseConfirm)-pbyte(@self), sizeof(CloseConfirm));

end;

procedure TVirtualAddressTable.SetOpenMarker;
begin
  OpenTime := tickcount.getticker;
  MarkDirtyByLength(pbyte(@OpenTime)-pbyte(@self), sizeof(OpenTime));
end;

function TVirtualAddressTable.VerifyGaps(bThrowExceptions: boolean): boolean;
var
  t, cpus: ni;
  fpf: PFilePhysical;
  c: array of Tcmd_VerifyGapsPArt;
begin
  result := true;

  setlength(c, Length(PayloadConfig.filelist));

  cpus := GetEnabledCPUCount - 1;
  if cpus = 0 then
    cpus := 1;
  for t := 0 to cpus - 1 do
  begin
    c[t] := nil;
    // if payloadconfig.filelist[t].NameAsString <> '' then begin
    c[t] := Tcmd_VerifyGapsPArt.Create;
    c[t].selfie := @Self;
    c[t].divd := cpus;
    c[t].modd := t;
    // c[t].FileIndex := t;
    c[t].CPUExpense := 1.0;
    // c[t].MemoryExpense := 1.0;
    c[t].start;
    // end;
  end;

  for t := 0 to cpus - 1 do
  begin
    if c[t] <> nil then
    begin
      c[t].WAitFor;
      if c[t].error <> '' then
      begin
        if bThrowExceptions then
        begin
          raise ECritical.Create(c[t].error);
        end
        else
        begin
          result := false;
        end;
      end;
      c[t].free;
      c[t] := nil;
    end;
  end;
end;

function TVirtualAddressTable.VerifyGapSingle(idx: int64; out violations: array of int64): boolean;
{$IFDEF MQ}
var
  t, u: ni;
  vart: PVirtualAddressRecord;
  a: array [0 .. 15] of Tcmd_VerifyGapsSinglePart;
begin
  result := true;
  vart := @Self.table[idx];
  for t := 0 to vart.FileCount - 1 do
  begin
    a[t] := Tcmd_VerifyGapsSinglePart.Create;
    a[t].vat := @Self;
    a[t].u := t;
    a[t].block := idx;
    gmq.AddItem(a[t]);
  end;

  for t := 0 to vart.FileCount - 1 do
  begin
    a[t].WAitFor;
    a[t].free;
    result := result and a[t].result;
  end;
end;
{$ELSE}

var
  t, u: ni;
  vart: PVirtualAddressRecord;
  fp, fpf: PFilePhysical;
begin
//  setlength(violations, 0);
  result := true;
  t := idx;
  vart := @Self.table[t];
  for u := 0 to vart.FileCount - 1 do
  begin
    // u := self.FileIndex;// to self.fileindex do begin
    // find file physical that is next after this one
    fp := @vart.FPs[u];
    if fp <> nil then
    begin
      fpf := Self.FindFilePhysical(fp.FileID, fp.PhysicalAddr_AfterHeader);
      if fpf <> nil then
      begin
        if (fpf.PhysicalAddr_AfterHeader - vart.FPs[u].PhysicalAddr_AfterHeader) <
          GetBigBlockSizeInBytes(vart.FileCount, true) then
        begin
          debug.Log
            (inttohex((fpf.PhysicalAddr_AfterHeader - vart.FPs[u].PhysicalAddr_AfterHeader), 1) + ' < '
            + inttohex(GetBigBlockSizeInBytes(vart.FileCount, true), 1));
          vart.RemoveFile(vart.FPs[u].FileID);
//          setlength(violations, length(violations)+1);
//          violations[high(violations)] := u;
          result := false;
        end;
      end;
    end;
  end;
end;
{$ENDIF}
{$ENDIF}
{ TVirtualAddressRecord }

procedure TVirtualAddressRecord.ChangeFileID(ifrom, ito: ni;
newphysical: int64);
var
  t: ni;
begin
{$IFNDEF ALLOW_RAID}
  FileID := ito;
{$ELSE}
  for t := low(FPs) to low(FPs) + FileCount - 1 do
  begin
    if FPs[t].FileID = ifrom then
    begin
      FPs[t].FileID := ito;
      FPs[t].PhysicalAddr_AfterHeader := newphysical;
      exit;
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
  if i >= 0 then
  begin
    result := @FPs[i];
  end;
end;
{$ENDIF}
{$IFDEF ALLOW_RAID}

function TVirtualAddressRecord.IndexOf(iFileID, iPhysical: int64): ni;
var
  t: ni;
begin
  result := -1;
  for t := low(FPs) to (low(FPs) + FileCount - 1) do
  begin
    if (FPs[t].FileID = iFileID) and (FPs[t].PhysicalAddr_AfterHeader = iPhysical) then
    begin
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
  result := a.StartingBlock = b.StartingBlock;
end;

function TVirtualAddressRecord.GetFP(iFileID: ni): PFilePhysical;
var
  i: ni;
begin
  result := nil;
  i := IndexOf(iFileID);
  if i >= 0 then
  begin
    result := @FPs[i];
  end;
end;

function TVirtualAddressRecord.GetVatIndex: int64;
begin
  result := startingblock shr BIG_BLOCK_BLOCK_SHIFT;
end;

function TVirtualAddressRecord.GetVirtualAddress: int64;
begin
  result := FStartingBlock shl BLOCKSHIFT;
end;

function TVirtualAddressRecord.HasFile(iFileID: ni): boolean;
begin
  result := IndexOf(iFileID) >= 0;
end;

function TVirtualAddressRecord.HasFileCount(iFileID: ni): ni;
var
  t: ni;
begin
  result := 0;
  for t:= 0 to filecount-1 do begin
    if fps[t].fileid = iFileId then
      inc(result);
  end;
end;

function TVirtualAddressRecord.HasUndefinedLocations: boolean;
var
  t: ni;
begin
  result := false;
  for t := 0 to FileCount - 1 do
  begin
    if FPs[t].FileID < 0 then
    begin
      result := true;
      exit;
    end;
  end;

end;

{$ENDIF}
{$IFDEF ALLOW_RAID}

function TVirtualAddressRecord.IndexOf(iFileID: ni): ni;
var
  t: ni;
  pf: PFilePhysical;
  fc: ni;
begin
  pf := @FPs[0];
  t := 0;
  fc := FileCount;
  while t<fc do begin
    if pf.fileid = iFileId then
      exit(t);
    inc(t);
    inc(PByte(pf), sizeof(TFilePhysical));
  end;
  exit(-1);

//  for t := 0 to FileCount - 1 do
//  begin
//    if (FPs[t].FileID = iFileID) then
//    begin
//      result := t;
//      exit;
//    end;
//  end;

end;
{$ENDIF}

function TVirtualAddressRecord.DebugString: string;
var
  t: ni;
begin
{$IFDEF ALLOW_RAID}
  result := '[pb'+inttohex(self.placedby,2)+']';
  result := result + '[va:' + inttohex(Self.StartingBlock * BLOCKSIZE, 0) + ']';

  // +'[f:RAIDME:RAIDME]';
  for t := 0 to Self.FileCount - 1 do
  begin
    result := result + '[f' + inttostr(FPs[t].FileID) + '::' +
      inttohex(Self.FPs[t].PhysicalAddr_AfterHeader, 0) + ']';
  end;

{$ELSE}
  result := '[va:' + inttohex(Self.StartingBlock * BLOCKSIZE, 0) + ']' + '[f:' +
    inttostr(FileID) + '::' + inttohex(Self.PhysicalAddr_AfterHeaderess, 0) + ']';
{$ENDIF}
end;

procedure TVirtualAddressRecord.InitVirgin(iStartingBlock: int64);
var
  t: ni;
begin
//  if iStartingblock = 42112 then
//    debug.log('trappy');
{$IFDEF ALLOW_RAID}
  for t := low(FPs) to high(FPs) do
  begin
    FPs[t].InitVirgin;
  end;
  FileCount := 0;
{$ELSE}
  FileID := -1;
  PhysicalAddress := -1;
  junk := 1;
{$ENDIF}
  PLACEDBY := $55;
  StartingBlock := iStartingBlock;

end;

function TVirtualAddressRecord.InvalidCount: ni;
begin
  result := HasfileCount(-1);
end;

function TVirtualAddressRecord.IsAssigned: boolean;
begin
{$IFNDEF ALLOW_RAID}
  result := (FileID > -1) and (PhysicalAddress > -1);
{$ELSE}
  result := FileCount > 0;
{$ENDIF}
end;

function TVirtualAddressRecord.IsDead: boolean;
var
  deaad: ni;
begin
  if filecount = 0 then
    exit(false);
  deaad := HasfileCount(-1);//how many dead positons do we have?
{$IFDEF NEW_DEAD_RULES}
  result := (filecount > 0) and (deaad >= lesserof(2, filecount));
{$ELSE}
  result := (filecount <> FILE_COUNT_NO_FETCH_FROM_SOURCE_ARCHIVE)
            and ((filecount = -2) or (filecount = FILE_COUNT_VAT_FUXORED) or (deaad >= lesserof(2, filecount)));{$ENDIF}
  if result then
    Debug.Log('DEAD! '+DebugString);
end;

procedure TVirtualAddressRecord.RemoveFile(idx: ni);
var
  t: ni;
begin
  // if it is a mirror, then we can just remove it
  if FileCount = 2 then
  begin
    for t := idx to FileCount - 2 do
    begin
      FPs[t] := FPs[t + 1];
    end;
    dec(FileCount);
  end
  else
  begin
    // if this is 1-drive or RAID then we need to flag this as a missing chunk
    FPs[idx].PhysicalAddr_AfterHeader := -1;
    FPs[idx].FileID := -1;
  end;

end;

function TVirtualAddressRecord.SelfCheckOk(myidx: ni; vda: TVirtualDisk_Advanced): TSelfCheckResult;
var
  u,id: ni;
  ps: TPayloadSTream;
  {$IFDEF PAYLOAD_HAS_QUEUED_INTERFACE}
  hedsobj, futsobj: array[0..31] of TObject;
  {$ENDIF}
  heds, futs: array[0..31] of TVirtualDiskBigBlockHeader;
  hed, fut: TVirtualDiskBigBlockHeader;
  addr,sz: int64;
  cnt: int64;
begin
  result := scrOK;

  if StartingBlock <> (myidx * _BIG_BLOCK_SIZE_IN_BLOCKS) then begin
    StartingBlock := (myidx * _BIG_BLOCK_SIZE_IN_BLOCKS);
    zl.Log(myidx,'Self check is fixing Starting block for '+inttohex(myidx, 1));
    result := scrCritical;
  end;

  if FileCount > 16 then begin
    zl.Log(myidx,'Self check reports too many files in entry '+inttohex(myidx, 1)+', invalidating');
    self.InitVirgin(startingBlock);
    result := scrCritical;
  end;

  if FileCount > 16 then begin
    zl.Log(myidx,'Self check reports too many files in entry '+inttohex(myidx, 1)+', invalidating');
    self.InitVirgin(startingBlock);
    result := scrCritical;
  end;

  for u := 0 to FileCount-1 do begin
    id := self.FPs[u].FileID;
    if id >= vda.FPayloadStreams.count then begin
      zl.Log(myidx,'Self check reports that Payload id is invalid for entry '+inttohex(myidx, 1));
      self.FPs[u].FileID := -1;
      result := scrCritical;
    end else begin
      id := self.FPs[u].FileID;
      if id < 0 then continue;
      ps := vda.FPayloadStreams[id];
      if ps = nil then begin
        zl.Log(myidx,'Payload stream '+inttostr(id)+' is not active.');
        continue;
      end;

      addr := self.FPs[u].PhysicalAddr_AfterHeader;
      if addr >= ps.Size then begin
        zl.Log(myidx,'Self check reports that Physical Start is beyond the end of the file for entry '+inttohex(myidx, 1));
        self.FPs[u].FileID := -1;
        result := scrSomeBad;
        continue;//^^^^^^^^^^^^^^
      end;
      if addr+GetBigBlockSizeInBytes(self.FileCount, false)+sizeof(hed) > ps.Size then begin
        zl.Log(myidx,'Self check reports that Physical End is beyond the end of the file for entry '+inttohex(myidx, 1));
        self.FPs[u].FileID := -1;
        result := scrSomeBad;
        continue;//^^^^^^^^^^^^^^
      end;

      sz := GetBigBlockSizeInBytes(self.FileCount, false);//<<<<<<--------------
      {$IFNDEF PAYLOAD_IS_UNBUFFERED}
      hedsobj[u] := ps.GetBigBlockHeaderFromPhysicalStart_Begin(self.FPs[u].PhysicalAddr_AfterHeader, heds[u]);
      futsobj[u] := ps.GetBigBlockFooterFromPhysicalStart_Begin(self.FPs[u].PhysicalAddr_AfterHeader, GetBigBlockSizeInBytes(filecount), futs[u]);
      {$ENDIF}
    end;
  end;

  for u := 0 to FileCount-1 do begin
    id := self.FPs[u].FileID;
    ps := vda.FPayloadStreams[id];
    if ps = nil then begin
      zl.Log(myidx,'Payload stream '+inttostr(id)+' is not active.');
      continue;
    end;
    if id >= vda.FPayloadStreams.count then begin
      zl.Log(myidx,'Self check reports that Payload id is invalid for entry '+inttohex(myidx, 1));
      self.FPs[u].FileID := -1;
      result := scrCritical;
    end else begin
      id := self.FPs[u].FileID;
      if id < 0 then continue;
      ps := vda.FPayloadStreams[id];
      addr := self.FPs[u].PhysicalAddr_AfterHeader;
      sz := GetBigBlockSizeInBytes(self.FileCount, false);//<<<<<<--------------
      {$IFNDEF PAYLOAD_IS_UNBUFFERED}
      hed := ps.GetBigBlockHeaderFromPhysicalStart_End(hedsobj[u], heds[u]);
      fut := ps.GetBigBlockFooterFromPhysicalStart_End(futsobj[u], futs[u]);
      {$ELSE}
      hed := ps.GetBigBlockHeaderFromPhysicalStart(self.FPs[u].PhysicalAddr_AfterHeader);
      fut := ps.GetBigBlockFooterFromPhysicalStart(self.FPs[u].PhysicalAddr_AfterHeader,sz);
      {$ENDIF}

      if not hed.CheckAddressValidity(sz,@self,u) then begin
        result := scrSomeBad;
        self.FPs[u].FileID := -1;
        zl.Log(myidx,'Header self check failed, invalidating file '+inttostr(id)+' @'+inttohex(myidx, 1));
        continue;//^^^^^^^^^^^^
      end;
      if not fut.CheckAddressValidity(sz,@self,u) then begin
        zl.Log(myidx,'Header self check failed, invalidating file '+inttostr(id)+' @'+inttohex(myidx, 1));
        result := scrSomeBad;
        self.FPs[u].FileID := -1;
        zl.Log(myidx,'vart is now '+self.DebugString);
        continue;//^^^^^^^^^^^^
      end;
      sz := GetBigBlockSizeInBytes(self.FileCount, true);//<<<<<<--------------
      if hed.TotalSizeIncludingHeaders <> sz then begin
        zl.Log(myidx,'Header block size misreported, invalidating file '+inttostr(id)+' @'+inttohex(myidx, 1));
        result := scrSomeBad;
        self.FPs[u].FileID := -1;
        zl.Log(myidx,'vart is now '+self.DebugString);
        continue;//^^^^^^^^^^^^
      end;
      if fut.TotalSizeIncludingHeaders <> sz then begin
        zl.Log(myidx,'Footer block size misreported, invalidating file '+inttostr(id)+' @'+inttohex(myidx, 1));
        result := scrSomeBad;
        self.FPs[u].FileID := -1;
        zl.Log(myidx,'vart is now '+self.DebugString);
        continue;//^^^^^^^^^^^^
      end;
{$IFDEF CHECK_FOOTERS}
      if not CompareMem(@hed, @fut, sizeof(hed)) then begin
        zl.Log(myidx,'HEader and footer don''t match, invalidating file '+inttostr(id)+' @'+inttohex(myidx, 1));
        result := scrSomeBad;
        self.FPs[u].FileID := -1;
        zl.Log(myidx,'vart is now '+self.DebugString);
        continue;//^^^^^^^^^^^^
      end;
{$ENDIF}

      if HasFileCount(id) > 1 then begin
        zl.Log(myidx,'Duplicate FileIDs found, invaliding one file ('+inttostr(id)+') @'+inttohex(myidx, 1));
        result := scrSomeBad;
        self.FPs[u].FileID := -1;
        zl.Log(myidx,'vart is now '+self.DebugString);
        result := scrSomeBad;
      end;
    end;
    if gQuickFix then begin
      zl.Log(myidx,'Quick Fix needs SAVE');
      gQuickFixNeedsSave := true;
    end;

  end;

  cnt := 0;
  for u := 0 to FileCount-1 do begin
    id := self.fps[u].fileid;
    if id > 0 then begin
      if HasFileCount(id) > 1 then begin
        zl.Log(myidx,'Duplicate FileIDs found, invaliding @'+inttohex(myidx, 1));
        result := scrCRitical;
        self.InitVirgin(startingBlock);
        zl.Log(myidx,'vart is now '+self.DebugString);

      end;
    end;
  END;

  cnt := self.HasFileCount(-1);

  if cnt > 1 then begin
    zl.Log(myidx,'Too many invalid files, big block is useless @'+inttohex(myidx, 1));
    zl.Log(myidx,'vart is now '+self.DebugString);
    result := scrCritical;
    self.InitVirgin(startingBlock);
    self.FileCount := FILE_COUNT_VAT_FUXORED;
  end;







end;

procedure TVirtualAddressRecord.SetStartingblock(const Value: int64);
begin
//  if value = 42112 then
//    Debug.log('init trap');
  FStartingBlock := Value;
end;

procedure TVirtualAddressRecord.SetVatIndex(const Value: int64);
begin
  startingblock := value shl BIG_BLOCK_BLOCK_SHIFT;
end;

{ TVirtualDiskPayloadFileHeader }

procedure TVirtualDiskPayloadFileHeader.Init;
begin
  _junk_NextPhysicalAddress := sizeof(TVirtualDiskPayloadFileHeader);
end;

{ TVirtualDiskHub }

procedure TVirtualDiskHub.AddDiskFromConfiguration(ap: TAppParams; t: ni);
var
  vd: TVirtualDisk;
  prefix: string;
begin
  vd := TVirtualDisk.Create;
  prefix := 'LUN'+inttostr(t);
  vd.LoadFromConfiguration(ap, prefix);

  vdlist.add(vd);
end;

constructor TVirtualDiskHub.Create;
begin
  inherited;
  vdlist := TVirtualDiskList.Create;
  LoadConfiguration;

end;

destructor TVirtualDiskHub.Destroy;
var
  vd: TVirtualDisk;
begin
  while (vdlist.count > 0) do
  begin
    vd := vdlist[0];
    vdlist.delete(0);

    try
      vd.DetachAndFree;
    except
      on E:Exception do begin
        AlertAdmin('ERROR SHUTTING DOWN DISK: '+e.Message);
        Debug.Log('ERROR SHUTTING DOWN DISK! No choice but to silence and continue.  '+e.Message);
      end;
    end;
    vd := nil;
  end;

  vdlist.free;
  inherited;
end;

procedure TVirtualDiskHub.LoadConfiguration;
var
  t, iCount: nativeint;
  ap: TAppParams;
  vd: TVirtualDisk;
begin
  ap := NeedAppParams;
  try
    iCount := ap.GetItemEx('DiskCount', 0);
    for t := 0 to iCount - 1 do
    begin
      AddDiskFromConfiguration(ap, t);


    end;

  finally
    NoNeedAppParams(ap);
  end;
end;

procedure TVirtualDiskHub.NewDisk(di: TNewDiskParams);
var
  iCount: ni;
  ap: TAppParams;
begin
  ap := NeedAppParams;
  try
//    if vdlist.IndexOf(di.Name) >= 0 then
//      raise ECritical.create('A disk named '+di.name+' already exists on this server!');


    iCount := ap.GetItemEx('DiskCount', 0);

//      vd.Size := ap.GetItemEx('LUN' + inttostr(t) + ':Size', int64(256 * MEGA));
//      vd.FileName := ap.GetItemEx('LUN' + inttostr(t) + ':FileName', '');
//      vd.Identifier := ap.GetItemEx('LUN' + inttostr(t) + ':Identifier',
//        'LUN' + inttostr(t));
//      vd.CacheSize := ap.GetItemEx('LUN' + inttostr(t) + ':CacheSize',
//        int64(2048 + MEGA));
//      vd.CachedStripes := ap.GetItemEx('LUN'+inttostr(t)+':CachedSTripes', DEFAULT_CACHED_STRIPES);
//      vd.shipper.Client.Host := ap.GetItemEx('LUN'+inttostr(t)+':ArcTargetHost', '');
//      vd.shipper.Client.EndPoint := ap.GetItemEx('LUN'+inttostr(t)+':ArcTargetEndPoint', '420');
//      vd.shipper.Archive := ap.GetItemEx('LUN'+inttostr(t)+':ArcTargetArchiveName', '');
    ap.Items['LUN'+inttostr(iCount)+':Size'].AsInteger := di.DiskSize;
    ap.Items['LUN'+inttostr(iCount)+':MaxRaidSpan'].AsInteger := di.MaxRaidSpan;
    ap.Items['LUN'+inttostr(iCount)+':SourcePin'].AsInteger := di.SourceArchivePinId;
    ap.Items['LUN'+inttostr(iCount)+':ArcTargetHost'].Value := di.TargetArchiveHost;
    ap.Items['LUN'+inttostr(iCount)+':ArcTargetArchiveName'].Value := di.TargetArchive;
    ap.Items['LUN'+inttostr(iCount)+':ArcSourceHost'].Value := di.SourceArchiveHost;
    ap.Items['LUN'+inttostr(iCount)+':ArcSourceArchiveNAme'].Value := di.SourceArchive;
    ap.Items['LUN'+inttostr(iCount)+':LazyArchiveFetch'].AsBoolean := di.LazySourceFetch;
    ap.Items['LUN'+inttostr(iCount)+':LocalRedundancy'].AsBoolean := di.LocalRedundancy;
    ap.Items['DiskCount'].AsInteger := iCount+1;
    AddDiskFromConfiguration(ap, iCount-1);


  finally
    noNeedAppParams(ap);
  end;

end;

procedure TVirtualDiskHub.PreShutdown;
var
  t: ni;
begin
  lock;
  try
    for t:= 0 to vdlist.count-1 do begin
      vdlist[t].preshutdown;
    end;
  finally
    unlock;
  end;
end;

procedure CreateVirtualDiskHub;
begin
  if vdh = nil then
    vdh := TVirtualDiskHub.Create;

end;

procedure DestroyVirtualDiskHub;
begin
  if vdh <> nil then begin
    vdh.free;
    vdh := nil;
  end;
end;

procedure oinit;
var
  t: ni;
  tiny, rem: ni;
begin
  vdh := nil;
  gquickfix := false;
  gquickfixneedssave := false;
  // CreateVirtualDiskHub;

end;

procedure ofinal;
begin
  DestroyVirtualDiskHub;
end;

{ TVirtualDiskPayloadConfiguration }

procedure TVirtualDiskPayloadConfiguration.AddPayload(sFile: string;
max_size: int64; iPhysical: ni; iPriority: ni; iFlags: int64);
var
  i: ni;
begin
  i := FindUnusedSlot;
  if i < 0 then
    raise ECritical.Create
      ('Could not add payload.  Could not find unused payload slot.');

  if fileexists(sFile) then
    DeleteFile(sFile);
  filelist[i].NameAsString := sFile;
  filelist[i].size_limit := max_size;
  filelist[i].physical := iPhysical;
  filelist[i].priority := iPriority;
  filelist[i].Flags := iFlags;

end;

function TVirtualDiskPayloadConfiguration.FindUnusedSlot: nativeint;
var
  t: ni;
begin
  result := -1;
  for t := 0 to high(filelist) do
  begin
    if filelist[t].name = '' then
    begin
      result := t;
      exit;
    end;
  end;

end;

function TVirtualDiskPayloadConfiguration.GetMarhshalSize: nativeint;
var
  t: ni;
begin
  for t := high(filelist) downto low(filelist) do
  begin
    result := ni(@filelist[t]) + sizeof(filelist[t]) - ni(@Self);
    if filelist[t].name <> '' then
    begin
      exit;
    end;
  end;

end;


{ TPayloadFileInformation }

function TPayloadFileInformation.GetFlagMissing: boolean;
begin
  result := BitGet(pbyte(@Self.Flags), BIT_FLAG_MISSING);
end;

function TPayloadFileInformation.GetNameAsString: string;
begin
  result := name;
end;

procedure TPayloadFileInformation.SetFlagMissing(const Value: boolean);
begin
  BitSet(pbyte(@Self.Flags), BIT_FLAG_MISSING, Value);
end;

procedure TPayloadFileInformation.SetNameAsString(const Value: string);
begin
  fillmem(pbyte(@name[0]), sizeof(name), 0);
  movemem32(@name[0], @Value[STRZ], Length(Value) * sizeof(char));

end;

{ TVDBlockBuffer }

{$IFDEF BUFFER_BLOCKS}

function TVDBlockBuffer.AnyDirty: boolean;
var
  t: ni;
begin
  result := false;
  for t := low(dirty) to high(dirty) do
  begin
    result := dirty[t];
    if result then
      exit;
  end;

end;

function TVDBlockBuffer.CoversBlock(lba: int64): boolean;
begin
  if (StartingBlock < 0) or (lba < StartingBlock) or
    (lba >= StartingBlock + MAX_BUFFERED_BLOCKS) then
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
  fillmem(pbyte(@Self), sizeof(Self) - 2048, 0);
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

  raise Exception.Create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

function TPayloadStream.CanDropBlock(iAddr: int64;
out iBlockedBy: int64): boolean;
begin

  raise Exception.Create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TPayloadStream.CheckInit;
var
  pfh: TVirtualDiskPayloadFileHeader;
begin
  if Self.Size < sizeof(TVirtualDiskPayloadFileHeader) then
  begin
    pfh.Init;
    Self.SetPFH(pfh);
  end;
end;

constructor TPayloadStream.Create(const AFileName: string; Mode: cardinal;
Rights: cardinal; Flags: cardinal);
begin
  inherited Create(AFileName, Mode, Rights, Flags);
  CheckInit;
  stats := TRingStats.Create;
  FCollapsable := true;
  gaps := TGapAnalyzer.Create;

{$IFNDEF PAYLOAD_IS_UNBUFFERED}
  queue.name := extractfilename(aFilename)+' TPayloadStream';
{$ENDIF}

{$IFDEF DETAILED_DEBUGGING}
  queue.enableitemdebug := true;
{$ENDIF}
end;

destructor TPayloadStream.Destroy;
begin
  stats.free;
  stats := nil;
  gaps.free;
  gaps := nil;

  inherited;
end;

function TPayloadStream.DropBigBlock(out pfh: TVirtualDiskPayloadFileHeader;
out p: pbyte; out iSize: int64): boolean;
begin

  raise Exception.Create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

function TPayloadStream.Expand(iSizeBeyondCurrentExcludingHeaders: int64;
virtual_addr: int64; bConsume: boolean=false): int64;
var
  bbh: TVirtualDiskBigBlockHeader;
  addr: int64;
  pfh: TVirtualDiskPayloadFileHeader;
begin
//  debug.Log('At start of expand, ps.size = '+inttohex(self.Size,1));
//  debug.Log('At start of expand, ps.gap.length = '+inttohex(self.gaps.length,1));
  if self.gaps.length > self.size then begin
    debug.Log('sizes don''t match, reanalyze');
    //AnalyzeGaps;
    self.gaps.FreeRegion(self.size, self.gaps.length-self.Size);
    self.gaps.Length := self.Size;
  end;
  CheckInit;
  bbh.TotalSizeIncludingHeaders := iSizeBeyondCurrentExcludingHeaders +
    (sizeof(TVirtualDiskBigBlockHeader) * 2);
  addr := Self.GetNextBigBlockAddress(bbh.TotalSizeIncludingHeaders);
  bbh.HeaderStart := addr;
  bbh.PayloadStart := addr + sizeof(TVirtualDiskBigBlockHeader);
  result := bbh.PayloadStart;
  bbh.UpdateFooterStart;
  bbh.VirtualAddress := virtual_addr;
  self.gaps.length := greaterof(bbh.headerstart+bbh.totalsizeincludingheaders, self.size);
  if bConsume then
    self.gaps.ConsumeRegion(bbh.HeaderStart,bbh.TotalSizeIncludingHeaders);
  bbh.UpdateCheckSum;
  Self.Seek(addr, soBeginning);
  // Header
  Stream_GuaranteeWrite(Self, pbyte(@bbh), sizeof(bbh));
  // payload
  Stream_WriteZeros(Self, iSizeBeyondCurrentExcludingHeaders);
  // footer
  Stream_GuaranteeWrite(Self, pbyte(@bbh), sizeof(bbh));
  // result := self.Size;

  gaps.Length := Self.Size;

{$IFDEF REDUNDANT_PHYSICAL}
  pfh := Self.GetPFH;
  pfh._NextPhysicalAddress := Size;
  Self.SetPFH(pfh);
{$ENDIF}

end;

function TPayloadStream.GetBigBlockFooterFromPhysicalStart(
  const addr: int64; sz: int64): TVirtualDiskBigBlockHeader;
begin
  fillmem(pbyte(@result), sizeof(result), 0);

  if Size < addr then
    exit;
  Seek(addr + sz, soBeginning);
  Stream_GuaranteeRead(Self, pbyte(@result), sizeof(result));
end;

function TPayloadStream.GetBigBlockHeaderFromPhysicalStart(const addr: int64)
  : TVirtualDiskBigBlockHeader;
begin
  fillmem(pbyte(@result), sizeof(result), 0);

  if Size < addr then
    exit;

  Seek(addr - sizeof(TVirtualDiskBigBlockHeader), soBeginning);
  Stream_GuaranteeRead(Self, pbyte(@result), sizeof(result));
end;


{$IFNDEF PAYLOAD_IS_UNBUFFERED}
function TPayloadStream.GetBigBlockFooterFromPhysicalStart_Begin(
  const addr: int64; sz: int64; var bbh: TVirtualDiskBigBlockHEader): TObject;
begin
  fillmem(pbyte(@bbh), sizeof(bbh), 0);

  if Size < addr then
    exit(nil);
  Seek(addr + sz, soBeginning);
  result := Stream_GuaranteeRead_Begin(Self, pbyte(@bbh), sizeof(bbh));
end;

function TPayloadStream.GetBigBlockFooterFromPhysicalStart_End(const o: TObject; var bbh: TVirtualDiskBigBlockHeader)
  : TVirtualDiskBigBlockHeader;
begin
  Stream_GuaranteeRead_End(self, o);
  result := bbh;
end;


function TPayloadStream.GetBigBlockHeaderFromPhysicalStart_Begin(const addr: int64; var bbh: TVirtualDiskBigBlockHeader)
  : TObject;
begin
  fillmem(pbyte(@bbh), sizeof(bbh), 0);

  if Size < addr then
    exit(nil);

  Seek(addr - sizeof(TVirtualDiskBigBlockHeader), soBeginning);
  result := Stream_GuaranteeRead_Begin(Self, pbyte(@bbh), sizeof(bbh));
end;


function TPayloadStream.GetBigBlockHeaderFromPhysicalStart_End(const o: TObject; var bbh: TVirtualDiskBigBlockHeader)
  : TVirtualDiskBigBlockHeader;
begin
  Stream_GuaranteeRead_End(self, o);
  result := bbh;
end;
{$ENDIF}



function TPayloadStream.GetLastBigBlockFooterAddr: int64;
begin
  result := Size - sizeof(TVirtualDiskBigBlockHeader);
end;

function TPayloadStream.GetLastBigBlockHeader: TVirtualDiskBigBlockHeader;
var
  iLast: int64;
begin
  raise ECritical.Create
    ('I''m fairly certain that the way you''re using GetLastBigBlockHeader is probably useless and wrong');
  iLast := Self.GetLastBigBlockFooterAddr;
  // debug.Log('last:'+inttostr(iLast)+' size:'+inttostr(size));
  Self.Seek(iLast, soBeginning);
  try
    Stream_GuaranteeRead(Self, pbyte(@result), sizeof(result), true)
  except
    AlertAdmin('Critical 911');
    debug.Log
      ('Something is very wrong and we couldn''t read the last bigblock header... trying to recover.');
    debug.Log(self,'Something is very wrong and we couldn''t read the last bigblock header... trying to recover.');
{$IFNDEF PAYLOAD_IS_UNBUFFERED}
    Self.FinalizeBuffers;
{$ENDIF}
    iLast := Self.GetLastBigBlockFooterAddr;
    Stream_GuaranteeRead(Self, Pbyte(@result), sizeof(result), true)
  end;

end;

function TPayloadStream.GetLastBigBlockHeaderAddr: int64;
begin
  result := Size - GetLastBigBlockHeader.TotalSizeIncludingHeaders;
end;

function TPayloadStream.GetLastPayloadAddress: int64;
begin
  result := Self.GetLastBigBlockHeader.PayloadStart;
end;

function TPayloadStream.GetNextBigBlockAddress(iTotalSizeIncludingHeaders
  : int64): int64;
var
  r: TGapInfo;
begin
  r := gaps.FindGap(iTotalSizeIncludingHeaders, Self.Size);
  if r.start >= 0 then
    result := r.start
  else
    result := Size;
{$IFDEF REDUNDANT_PHYSICAL}
  result := GetPFH._NextPhysicalAddress;
  if result > Size then
    result := Size;
{$ENDIF}
end;

function TPayloadStream.GetPFH: TVirtualDiskPayloadFileHeader;
begin
  Self.Seek(0, soBeginning);
  Stream_GuaranteeRead(Self, Pbyte(@result), sizeof(result));
end;

function TPayloadStream.IsEmpty: boolean;
begin
  result := Size <= sizeof(TVirtualDiskPayloadFileHeader);
end;

procedure TPayloadStream.MoveBigBlock(fromAddr, toAddr: int64; sz: int64);
var
  p: pbyte;
  head: PVirtualDiskBigBlockHeader;
  foot: PVirtualDiskBigBlockHeader;
begin
  if (sz < 0) or (sz > BIG_BLOCK_SIZE_IN_BYTES+100000) then
    raise ECritical.create('invalid data count in MoveBigBlock '+inttostr(sz));

  p := GetMemory(sz);
  try
    //READ
    Self.Seek(fromAddr, soBeginning);
    Stream_GuaranteeRead(Self, p, sz, true);

    //HEADER
    head := PVirtualDiskBigBlockHeader(p);
    head.HeaderStart := toAddr;
    head.PayloadStart := toAddr + sizeof(TVirtualDiskBigBlockHeader);
    head.FooterStart := head.PayloadStart +
      (sz - (2 * sizeof(TVirtualDiskBigBlockHeader)));
    head.UpdateCheckSum;

    //FOOTER
    foot := PVirtualDiskBigBlockHeader(p+(sz - (2 * sizeof(TVirtualDiskBigBlockHeader))));
    foot^ := head^;

    //WRITE
    Self.Seek(toAddr, soBeginning);
    Stream_GuaranteeWrite(Self, p, sz);

  finally
    Freememory(p);
  end;

end;

procedure TPayloadStream.SetNextBigBlockAddress(addr: int64);
begin

  raise Exception.Create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TPayloadStream.SetPFH(pfh: TVirtualDiskPayloadFileHeader);
begin
  Self.Seek(0, soBeginning);
  Stream_GuaranteeWrite(Self, Pbyte(@pfh), sizeof(pfh));

end;

procedure TPayloadStream.SetSize(const Value: Int64);
begin
  inherited;
  gaps.Length := value;
end;

{ TVirtualDiskBigBlockHeader }

function TVirtualDiskBigBlockHeader.BeyondAddress: int64;
begin
  result := PayloadStart+payloadsize+SizeOf(self);
end;

function TVirtualDiskBigBlockHeader.CheckAddressValidity(payload_size: ni; pVar: pointer; fileidx: ni): boolean;
begin
  if CheckSum = ExpectedCheckSum then begin
    if VirtualAddress <> PVirtualAddressRecord(pvar).virtualaddress then begin
{$IFDEF ALLOW_QUICK_FIXES}
      Debug.Log('quick fix virtual address for file #'+inttostr(fileidx)+' of '+PVirtualAddressRecord(pvar).DebugString+' headerstart='+inttohex(HeaderStart+sizeof(TVirtualDiskBigBlockHeader),0)+' payloadstart='+inttohex(PayloadStart,0));
      Debug.Log('-was: '+inttohex(virtualaddress,16));
      virtualAddress := PVirtualAddressRecord(pvar).virtualaddress;
      Debug.Log('-now: '+inttohex(virtualaddress,16));
      UpdateCheckSum;
{$ELSE}
//      exit(false);
{$ENDIF}
    end;
  end;


  gQuickFix := false;
  result := (HeaderStart + sizeof(TVirtualDiskBigBlockHeader)) = PayloadStart;
  if not result then begin
    Debug.Log('payload start invalid file '+inttostr(fileidx)+' of '+PVirtualAddressRecord(pvar).DebugString+' headerstart='+inttohex(HeaderStart+sizeof(TVirtualDiskBigBlockHeader),0)+' payloadstart='+inttohex(PayloadStart,0));
    PayloadStart := (HeaderStart + sizeof(TVirtualDiskBigBlockHeader));
  end;
  if result then
    result := PayloadSize = payload_size;
  if not result then
    Debug.Log('payload size invalid '+inttostr(fileidx)+' of '+PVirtualAddressRecord(pvar).DebugString);
  if result then
    result := FooterStart = payloadstart + payload_size;
  if not result then begin
{$IFDEF ALLOW_QUICK_FIXES}
  {$DEFINE QUICK_FIX_FOOTER}
{$ENDIF}
{$IFDEF QUICK_FIX_FOOTER}
    debug.Log('footer start invalid '+inttostr(fileidx)+' of '+PVirtualAddressRecord(pvar).DebugString);
    Debug.Log('quick fix FOOTER address for file #'+inttostr(fileidx)+' of '+PVirtualAddressRecord(pvar).DebugString+' headerstart='+inttohex(HeaderStart+sizeof(TVirtualDiskBigBlockHeader),0)+' payloadstart='+inttohex(PayloadStart,0));
    Debug.Log('-was: '+inttohex(FooterStart,16));
    UpdateFooterStart;
    Debug.Log('-now: '+inttohex(FooterStart,16));
    UpdateCheckSum;
    result := true;
{$ENDIF}
{$IFDEF QUICK_FIX_FOOTER}
    debug.Log('QUICK FIX ENABLED');
    FooterStart := payloadstart + payload_size;
    gQuickFix := true;
    result := true;
{$ENDIF}
  end;

  if result = false then
    Debug.Log('Addresses for payload piece are not valid');

end;

function TVirtualDiskBigBlockHeader.EndOfFooter: int64;
begin
  result := HeaderStart + TotalSizeIncludingHeaders;
end;

function TVirtualDiskBigBlockHeader.ExpectedCheckSum: int64;
begin
  result := TotalSizeIncludingHeaders xor HeaderStart xor PayloadStart xor TotalSizeIncludingHeaders xor VirtualAddress;
end;

function TVirtualDiskBigBlockHeader.PayloadSize: int64;
begin
  result := TotalSizeIncludingHeaders - (sizeof(Self) * 2);
end;

procedure TVirtualDiskBigBlockHeader.UpdateCheckSum;
begin
  UpdatefooterStart;
  CheckSum := ExpectedChecksum;
end;

procedure TVirtualDiskBigBlockHeader.UpdateFooterStart;
begin
  FooterStart := PayloadStart+PayloadSize;
end;

function TVirtualDiskBigBlockHeader.VAlid(vaddr: int64): boolean;
begin
  result := (CheckSum = ExpectedChecksum) and (TotalSizeIncludingHeaders > (2 * sizeof(Self)))
  // and     (VirtualAddress = vaddr)
    and (PayloadStart = (HeaderStart + sizeof(Self)));
end;

{ TFilePhysical }

function TFilePhysical.CheckAndCorrect(filecount: ni;
  ps: TPayLoadStream): boolean;
  procedure Correct;
  begin
    fileid := -1;
    physicaladdr_afterheader := -1;
  end;
var
  a,b: int64;
begin
  if ps = nil then
    exit(false);

  if fileid < 0 then begin
    exit(false);//file is already marked bad
  end;
  if physicaladdr_afterheader < 0 then begin
    Debug.Log('Check and correct is marking some stuff invalid. <0');
    Correct;
    exit(false);
  end;

  a := ((physicaladdr_afterheader -sizeof(TvirtualDiskBigBlockHeader))+ GetBigBlockSizeInBytes(filecount, true));
  b := ps.Size;
  if a > b then begin
    Debug.Log('Check and correct is making some stuff invalid. >EOF by'+(a-b).tostring);
    Correct;
    exit(false);
  end;

  exit(true);





end;

procedure TFilePhysical.InitVirgin;
begin
//  Debug.Log('Init Virgin');
  FileID := -1;
  physicaladdr_afterheader := -1;
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
  comp2 := TVirtualDisk_SimpleNonLinear.Create;
end;

function TVirtualDisk_Comparative.DebugVatSTructure: string;
begin
  result := comp1.DebugVatSTructure;
end;

procedure TVirtualDisk_Comparative.DecommissionPayload(sFile: string);
begin
  comp1.DecommissionPayload(sFile);
end;

function TVirtualDisk_Advanced.DefragMentPayload(iFileID: fi): boolean;
begin
  result := false;
  // 1. Create a list of all the physical addresses

  // 2. Sort the physical Addresses

  // 3. Calculate Ideal File Positions for each of the physical addresses
  // we will not necessarily defrag the whole payload (that would take forever and kill performance)
  // instead we will devide the payload into 200MB zones which will be individually defraged.
  // 4.

end;

destructor TVirtualDisk_Comparative.Destroy;
var
  a: array [0 .. BLOCKSIZE - 1] of byte;
begin
  comp1.ReadBlock(0, @a[0]);
  SaveStringAsFile(comp1.FileName + '.comp1.0.txt',
    memorydebugstring(@a[0], BLOCKSIZE));
  comp2.ReadBlock(0, @a[0]);
  SaveStringAsFile(comp2.FileName + '.comp2.0.txt',
    memorydebugstring(@a[0], BLOCKSIZE));

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

function TVirtualDisk_Comparative.GetPayloadConfig
  : PVirtualDiskPayloadConfiguration;
begin
  result := comp1.GetPayloadConfig;
end;

function TVirtualDisk_Comparative.GetRepairLog: string;
begin
  result := comp1.GetRepairLog;
end;

procedure TVirtualDisk_Comparative.QuickOnline;
begin
  comp1.QuickOnline;
end;

procedure TVirtualDisk_Comparative.ReadBlock(const lba: int64; const p: pbyte);
var
  temp: array [0 .. BLOCKSIZE - 1] of byte;
begin
  inherited;
  comp2.ReadBlock(lba, p);
  comp1.ReadBlock(lba, @temp[0]);
  if not CompareMem(p, @temp[0], BLOCKSIZE) then
  begin
    raise ECritical.Create('Compare Fail');
  end;

end;

function TVirtualDisk_Comparative.ReadBlocks(const lba: int64;
const cnt: nativeint; p: pbyte): nativeint;
var
  pp: pbyte;
begin
  // if lba = $0 then
  // Debug.Log('Trap');
  result := comp2.ReadBlocks(lba, cnt, p);
  debug.Log(self,'ReadBlocks Read ' + inttostr(result) + ' @' + inttohex(lba, 0));
  pp := GetMemory(result * BLOCKSIZE);
  try

    comp1.GuaranteeReadBlocks(lba, result, pp);
    if not CompareMem(pp, p, result) then
    begin
      debug.Log(self,'Bad Read nl ' + memorydebugstring(p, result * BLOCKSIZE));
      debug.Log(self,'Bad Read adv ' + memorydebugstring(pp, result * BLOCKSIZE));
      comp1.GuaranteeWriteBlocks(lba, result, p);
      // halt;
      // raise ECritical.create('bad read');

    end;
  finally
    Freememory(pp);
  end;

end;

procedure TVirtualDisk_Comparative.ReadData(const addr: int64;
const cnt: nativeint; p: pbyte);
begin
  inherited;
//  if (cnt shr BLOCKSHIFT) <> 0 then
//    raise ECritical.Create('Cnt is not a multiple of block size');
//
//  if (addr shr BLOCKSHIFT) <> 0 then
//    raise ECritical.Create('addr is not a multiple of block size');

  GuaranteeReadBlocks(addr shr BLOCKSHIFT, cnt shr BLOCKSHIFT, p);

end;

procedure TVirtualDisk_Comparative.ReFunctPayload(id: ni; sFile: string);
begin
  comp1.ReFunctPayload(id, sFile);
end;

procedure TVirtualDisk_Comparative.ResetRepair;
begin
  //
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
  comp1.FileName := Value;
  comp2.FileName := Value + 'compare.vdnl';
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
  comp1.Size := Value;
  comp2.Size := Value;
end;


procedure TVirtualDisk_Comparative.UnpauseScrubber;
begin
  comp1.UnpauseScrubber;
end;

procedure TVirtualDisk_Advanced.VD_SmartSideFetch;
var
  start: int64;
  blk: int64;
  raid: int64;
  bNewBigBlock: boolean;
  tm1,tm2: ticker;
  ll,l: TLock;
  tmSinceSide: ticker;
  tmTimeUntilNextExpectedUsage: ticker;
  tmSinceIdle: ticker;
  avg: ticker;
begin
  if hint_requests_waiting then begin
    sleep(1);
    exit;
  end;
{$IFDEF DISABLE_VD_SIDEFETCH}
  sleep(500); exit;//<--------------------------------------
{$ENDIF}
{$IFDEF SINCE_IDLE_CHECKS}
  tmSinceside := greaterof(0,GetTimeSince(tmLastSideFetch));
  tmSinceIdle := greaterof(0,GetTimeSince(queue.idleTick));
  avg := round(queue.rsIdle.PeriodicAverage);
  tmTimeUntilNextExpectedUsage := greaterof(0,GetTicker-(GetTimeSince(tmLastSideFetch)+avg));
  if tmTimeUntilNextExpectedUsage < 0 then begin
    //BACK OFF A BIT
    sleep(lesserof(tmTimeUntilNextExpectedUsage,1000));
//    sfthread.HasWork := false;
//    sfthread.RunHot := false;
//    exit;
  end else begin
    //ELSE CRANK OUT PREFECTHES
//    sleep(avg);
  end;
{$ENDIF}

  tm1 := GEtHighResTicker;
  if queue.estimated_queue_size > 1 then begin
    sfthread.HasWork := false;
    sfthread.RunHot := false;
    exit;
  end;
  //if the last explicit op was a read
  //then fetch
  //if not lastopwaswrite then begin
    if front_allowed_prefetches > 0 then begin
        if not TryGetLock(ll) then begin//wrong form, should use TryGetLock
          //Dec(front_allowed_prefetches);
          sleep(1);
        end
        else
        try
          if front_allowed_prefetches > 0 then begin
//            rs4.BeginTime;
            bNewBigBlock := false;
            repeat
              l := GetLock;
              try
              if queue.estimated_queue_size > 1 then
                exit;
              start := self.front_prefetchposition;
              blk := start shr BLOCKSHIFT;

              if start >=0 then begin
                if blk < int64(MAX_POTENTIAL_DISK_SIZE) shr int64(BLOCKSHIFT) then begin
                  if not self.HasRaid(blk) then begin     //s
                    //debug.Log(ltAll, 'Side Fetch Block:'+blk.tohexstring+' Stripe:'+(blk shr STRIPE_SHIFT).ToHexString);
                    disablecallupstats := true;

                    self.SyncBuffer(blk, front_prefetchwritemode);
                    disablecallupstats := false;
                    inc(queue.sidefetches);
                    Dec(front_allowed_prefetches);
                    inc(front_prefetchposition, RAID_STRIPE_SIZE_IN_BYTES);

                  end else begin
                    //Dec(front_allowed_prefetches);
                    inc(front_prefetchposition, RAID_STRIPE_SIZE_IN_BYTES);
                  end;
                end;
              end;
              tm2 := GetHighResTicker;
              if (tm2-tm1) > 1000 {100us} then
                break;
              //STOP AT BIGBLOCK BOUNDARY
              if (front_prefetchposition shr BIG_BLOCK_BYTE_SHIFT) <> (start shr BIG_BLOCK_BYTE_SHIFT) then begin
                bNewBigBlock := true;
//                allowed_prefetches := 0;
              end;
            finally
              Unlocklock(l);
            end;

            until (front_allowed_prefetches <=0) or (bNewBigBlock);
          end;
        finally
//          rs4.EndTime;
//          rs4.optiondebug('VDSideFetch');
          queue.noworkruninterval := greaterof(1,rs4.periodicaverage/10000);
          unlockLock(ll);
        end;
    end;
    sfthread.HasWork := front_allowed_prefetches > 0;
    sfthread.RunHot := front_allowed_prefetches > 0;
    tmLastSideFetch := GetTicker;
end;

function TVirtualDisk_Advanced.VerifyAgainstArchive(zone: integer; out csa, csb,
  difstart: int64): boolean;
var
  l: Tlock;
  dba: TDynByteArray;
  blk, iSum1,iSum2, iXor1,iXor2: int64;
  bbi: int64;
begin
  result := false;
  repeat
    l := GEtlock;
    try
      //start calculating at the back-end
      if TECS(self.shipper.csQuickClient) then
      try
        bbi := (zone shl ARC_ZONE_BLOCK_SHIFT) shr (BIG_BLOCK_BLOCK_SHIFT);
        if vat.table[bbi].filecount <= 0 then begin
          exit(true);
        end;

        self.shipper.QuickClient.GetZoneChecksum_Async(self.shipper.Archive, zone);
        try
          setlength(dba, ARC_ZONE_SIZE_IN_BYTES);
          blk := zone shl ARC_ZONE_BLOCK_SHIFT;
          self.GuaranteeReadBlocks(blk, ARC_ZONE_SIZE_IN_BLOCKS, @dba[0]);
          CalculateChecksum(@dba[0], ARC_ZONE_SIZE_IN_BYTES, iSum1, iXor1);
        finally
          self.shipper.QuickClient.GetZoneChecksum_Response(iSum2, iXor2);
        end;

        csa := iSum1;
        csb := iSum2;
        result := (iSum1=iSum2) and (iXor1=iXor2);
        if not result then begin
          debug.Log('ARCHIVE MISMATCH! '+inttohex(iSum1,0)+'='+inttohex(iSum2,0)+' '+inttohex(iXor1,0)+'='+inttohex(iXor2,0));

          break;
        end;

      finally
        LCS(self.shipper.csQuickClient);
      end;

    finally
      unlocklock(l);
    end;
    if not result then
      sleep(1);
  until result;

end;

function TVirtualDisk_Advanced.VerifyArcZone(zoneidx: ni;
  bUseSourceArchive: boolean): boolean;
var
  l: Tlock;
  dba: TDynByteArray;
  blk, iSum1,iSum2, iXor1,iXor2: int64;
  bbi: int64;
begin
  result := false;
  repeat
    l := GEtlock;
    try
      //start calculating at the back-end
      if TECS(self.shipper.csQuickClient) then
      try
        bbi := (zoneidx shl ARC_ZONE_BLOCK_SHIFT) shr (BIG_BLOCK_BLOCK_SHIFT);
        if vat.table[bbi].filecount <= 0 then begin
          exit(true);
        end;

        self.shipper.QuickClient.GetZoneChecksum_Async(self.shipper.Archive, zoneidx);
        try
          setlength(dba, ARC_ZONE_SIZE_IN_BYTES);
          blk := zoneidx shl ARC_ZONE_BLOCK_SHIFT;
          self.GuaranteeReadBlocks(blk, ARC_ZONE_SIZE_IN_BLOCKS, @dba[0]);
          CalculateChecksum(@dba[0], ARC_ZONE_SIZE_IN_BYTES, iSum1, iXor1);
        finally
          self.shipper.QuickClient.GetZoneChecksum_Response(iSum2, iXor2);
        end;

        result := (iSum1=iSum2) and (iXor1=iXor2);
        if not result then begin
          debug.Log('ARCHIVE MISMATCH! zone=0x'+inttohex(zoneidx,0)+' '+inttohex(iSum1,0)+'='+inttohex(iSum2,0)+' '+inttohex(iXor1,0)+'='+inttohex(iXor2,0));
          break;
        end;

      finally
        LCS(self.shipper.csQuickClient);
      end;

    finally
      unlocklock(l);
    end;
    if not result then
      sleep(1);
  until result;

end;

procedure TVirtualDisk_Comparative.WriteBlock(const lba: int64; const p: pbyte);
begin
  inherited;
  comp1.WriteBlock(lba, p);
  comp2.WriteBlock(lba, p);

end;

function TVirtualDisk_Comparative.WriteBlocks(const lba: int64;
const cnt: nativeint; p: pbyte; bDontArchive: boolean = false): nativeint;
begin
  // if IsWithin(lba, cnt, $5e3688) then
  // Debug.Log('Trap');


  // if lba = $0 then
  // Debug.Log('Trap');

  result := comp2.WriteBlocks(lba, cnt, p);
  // Debug.Log('WriteBlocks Wrote '+inttostr(result)+' @'+inttohex(lba,0));
  comp1.GuaranteeWriteBlocks(lba, result, p, bDontArchive);
end;

procedure TVirtualDisk_Comparative.WriteData(const addr: int64;
const cnt: nativeint; p: pbyte);
begin
  inherited;
//  if (cnt mod BLOCKSIZE) <> 0 then
//    raise ECritical.Create('Cnt is not a multiple of block size');
//
//  if (addr mod BLOCKSIZE) <> 0 then
//    raise ECritical.Create('addr is not a multiple of block size');

  GuaranteeWriteBlocks(addr shr BLOCKSHIFT, cnt shr BLOCKSHIFT, p);

end;

{ TVirtualDisk_Fake }

procedure TVirtualDisk_Fake.AddPayload(sFile: string; size_limit: int64;
iPhysical, iPriority: ni; iFlags: int64);
begin
  //
end;

procedure TVirtualDisk_Fake.BeginRepair;
begin
  // comp1.beginrepair;
end;

procedure TVirtualDisk_Fake.DecommissionPayload(sFile: string);
begin
  //
end;

function TVirtualDisk_Fake.GetPayloadConfig: PVirtualDiskPayloadConfiguration;
begin
  raise ECritical.Create('not implemented');
end;

procedure TVirtualDisk_Fake.ResetRepair;
begin
  //
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
  t, u: ni;
  vart: PVirtualAddressRecord;
  fp, fpf: PFilePhysical;
begin
  inherited Cresult := true;

  StepCount := MAX_BLOCKS_IN_VAT;

  for t := 0 to MAX_BLOCKS_IN_VAT - 1 do
  begin
    if (t mod divd) <> modd then
      continue;
    Step := t;
    vart := @selfie.table[t];
    for u := vart.FileCount - 1 downto 0 do
    begin
      // u := self.FileIndex;// to self.fileindex do begin
      // find file physical that is next after this one
      fp := @vart.FPs[u];
      if fp <> nil then
      begin
        fpf := selfie.FindFilePhysical(fp.FileID, fp.PhysicalAddr_AfterHeader);
        if fpf <> nil then
        begin
          if (fpf.PhysicalAddr_AfterHeader - vart.FPs[u].PhysicalAddr_AfterHeader) <
            GetBigBlockSizeInBytes(vart.FileCount, true) then
          begin
            // vart.RemoveFile(vart.FPs[u].FileID);
            Cresult := false;
            error := 'GAP ERROR! in file #' + inttostr(vart.FPs[u].FileID) +
              ' 0x' + inttohex(fpf.PhysicalAddr_AfterHeader, 0) + '-0x' +
              inttohex(fpf.PhysicalAddr_AfterHeader, 0) + '=' +
              inttohex(fpf.PhysicalAddr_AfterHeader - vart.FPs[u].PhysicalAddr_AfterHeader, 0) +
              ' minimum is:' +
              inttohex(GetBigBlockSizeInBytes(vart.FileCount, true), 0);
          end;
        end;
      end;
    end;
  end;
end;


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
  // for u := 0 to vart.FileCount-1 do begin
  // u := self.FileIndex;// to self.fileindex do begin
  // find file physical that is next after this one
  fp := @vart.FPs[u];
  if fp <> nil then
  begin
    fpf := vat.FindFilePhysical(fp.FileID, fp.PhysicalAddr_AfterHeader);
    if fpf <> nil then
    begin
      if (fpf.PhysicalAddr_AfterHeader - vart.FPs[u].PhysicalAddr_AfterHeader) < GetBigBlockSizeInBytes
        (vart.FileCount, true) then
      begin
        // vart.RemoveFile(vart.FPs[u].FileID);
        result := false;
      end;
    end;
  end;
  // end;
end;

{ TVatStat }

procedure TVatStat.Discount;
begin
  dec(callups);
  dec(writes);
  callups := greaterof(0, callups);
  writes := greaterof(0, writes);
end;

procedure TVatStat.Init;
begin
  Callups := 0;
  Writes := 0;
end;

{ TVatStats }

procedure TVatStats.Discount;
var
  t: ni;
begin
  for t:= 0 to high(self.big_blocks) do begin
    big_blocks[t].Discount;
  end;

end;

procedure TVatStats.Init;
var
  t: ni;
begin
  for t:= 0 to high(big_blocks) do begin
    big_blocks[t].Init;
  end;
end;

{ Tcmd_BBHFetch }

procedure Tcmd_BBHFetch.DoExecute;

begin
  inherited;
//  ps.SeekLock;
  try
    try
      pbbh^ := ps.GetBigBlockHeaderFromPhysicalStart(phys);
    except
      on e:exception do begin
        Debug.Log('EXCEPTION IN '+self.ClassName+': '+e.message);
      end;
    end;
  finally
//    ps.SeekUnLock;
  end;

end;

procedure Tcmd_BBHFetch.Init;
begin
  inherited;
  Icon := @CMD_ICON_HEADER
end;

procedure Tcmd_BBHFetch.InitExpense;
begin
  inherited;
  cpuexpense := 0;
end;

{ TVDReadCommand }

constructor TVDReadCommand.create;
begin
  inherited;
{$IFDEF QUEUE_ITEM_DEBUG}Debug.Log('create '+getobjectdebug);{$ENDIF}
end;

destructor TVDReadCommand.destroy;
begin
{$IFDEF QUEUE_ITEM_DEBUG}Debug.Log('destroy '+getobjectdebug);{$ENDIF}
  inherited;
end;

procedure TVDReadCommand.DoExecute;
var
  sf: TVDSideFetch;
begin
  inherited;

  if addr = 877235765248 then
    Debug.Log('trap');
{$IFDEF QUEUE_ITEM_DEBUG}Debug.Log('execute '+getobjectdebug);{$ENDIF}
{$IFDEF VDREAD_LOG}
  Debug.Log('VDREAD['+addr.tostring+','+count.tostring+']');
{$ENDIF}
  vd.front_CancelPrefetches;
  vd.front_prefetchposition := addr;
  vd.front_prefetchwritemode := false;
  vd.EnableLegitTagging := true;
  vd.SYncReadData(addr, count, p);
  vd.EnableLegitTagging := false;
  if self.queue.estimated_queue_size < 2 then
    vd.Front_StartPrefetches;
  vd.front_prefetchposition := addr+count;
{$IFDEF ALLOW_SIDEFETCH_REQUEUE}
  sf := TVDSideFetch.create;
  sf.vd := self.vd;
  sf.AutoDestroy := true;
  Queue.SelfAdd(sf);
{$ENDIF}


end;

procedure TVDReadCommand.Init;
begin
  inherited;
{$IFDEF ALLOW_SYNCHRONOUS_READS}
  AllowSynchronous := true;
{$ENDIF}
end;

{ TVDWriteCommand }

constructor TVDWriteCommand.create;
begin
  inherited;
{$IFDEF QUEUE_ITEM_DEBUG}Debug.Log('create '+getobjectdebug);{$ENDIF}
end;

destructor TVDWriteCommand.destroy;
begin
{$IFDEF QUEUE_ITEM_DEBUG}Debug.Log('destroy '+getobjectdebug);{$ENDIF}
  inherited;
end;

procedure TVDWriteCommand.Detach;
begin
  if detached  then
    exit;
  FreeMemory(p);
  inherited;

end;

procedure TVDWriteCommand.DoExecute;
begin
  inherited;
{$IFDEF QUEUE_ITEM_DEBUG}Debug.Log('execute '+getobjectdebug);{$ENDIF}
  vd.Front_CancelPrefetches;
  vd.front_prefetchwritemode := true;
  vd.front_prefetchposition := addr;
  vd.enablelegittagging := true;
  vd.SyncWriteData(addr,count,p);
  vd.enablelegittagging := false;


  //continue prefetching empty writebuffers (flushes away dirty ones potentially)
  vd.front_prefetchwritemode := true;
  vd.front_prefetchposition := addr+count;
  if self.queue.estimated_queue_size < 2 then
    vd.Front_StartPrefetches;




end;

{ TVDQUeue }

{$IFDEF READ_BEFORE_WRITE_VD_QUEUE}
function TVDQUeue.GetNextItem: TQueueItem;
var
  t: ni;
  itm: TVDReadCommand;
  wi: TVDQueueItem;
  u: ni;
  bGood, bThisGood: boolean;
  a,b,c,d: int64;
begin
  result := nil;
  //inherited;/// <<--- do not call

  //stream queue order processing rules

  //read operations get priority,
  //...make a list of read operations
  itm := GetRead;

  bGood := false;
  if itm <> nil then begin
    //for each read operation, check that it does not overlap any write operations
    bGood := true;
    for u := 0 to FWorkingItems.count-1 do begin
      wi := TVDQueueItem(fWorkingItems[u]);
      if wi = itm then break;

      if wi is TVDWriteCommand then begin
        a := TVDWriteCommand(wi).Addr;
        b := TVDWriteCommand(wi).Count;
        c := itm.Addr;
        d := itm.Count;
        bThisGood := ((a+b) <= c) or (a >= (c+d));
        if not bThisGood then begin
          bgood := false;
          break;
        end;
      end;
    end;
  end;

  if not bGood then
    result := inherited
  else
    result := itm;

{$IFDEF LOG_ITEMS}
  Debug.Log(self,'got '+result.debugstring);
{$ENDIF}
end;
{$ENDIF}

function TVDQUeue.GetRead: TVDReadCommand;
var
  t: ni;
begin
  result := nil;
  for t:= 0 to FWorkingItems.count-1 do begin
    if FWorkingItems[t] is TVDReadCommand then begin
      exit(TVDReadCommand(FWorkingItems[t]));
    end;
  end;

end;


procedure TVDQUeue.Init;
begin
  inherited;
  MaxItemsInQueue := 8000;
end;

procedure TVDQUeue.InitFromPool;
begin
  inherited;
  MaxItemsInQueue := 8000;
end;

{ TVDSideFetch }

procedure TVDSideFetch.DoExecute;
begin
  inherited;
//  if queue.estimated_queue_size < 1 then
    vd.VD_SmartSideFetch;
end;

procedure TVDSideFetch.Init;
begin
  inherited;

end;

{ TVDSideFetchThread }

procedure TVDSideFetchThread.DoExecute;
begin
  inherited;
  loop := true;
  vd.VD_SmartSideFetch;
  StepCount := vd.front_RecommendedPrefetchAllowance;
  Step := vd.front_allowed_prefetches;
  IterationCOmplete;

end;

{ Tcmd_SourceFetch }

procedure Tcmd_SourceFetch.DoExecute;
var
  retries: ni;
begin
  inherited;
  retries := 0;
  while retries < 4 do
  try
    arc.GetLog(archivename, pin, zidx shl ARC_ZONE_BLOCK_SHIFT, ARC_ZONE_SIZE_IN_BLOCKS, pointer);
    break;
  except
    inc(retries);
  end;
end;

procedure Tcmd_SourceFetch.Init;
begin
  inherited;
  Icon := @CMD_ICON_REBUILD;
end;

procedure Tcmd_SourceFetch.Initexpense;
begin
  inherited;
  cpuexpense := 0;
end;

{ TResurrector }

function TResurrector.AllZeros: boolean;
var
  t: ni;
begin
  for t:= 0 to BIG_BLOCK_SIZE_IN_BYTES-1 do begin
    if scratch[t] <> 0 then
      exit(false);
  end;
  exit(true);

end;

procedure TResurrector.Connect;
begin
  if cli <> nil then begin
    if not cli.connected then
      Disconnect;
  end;

  if cli = nil then begin
    cli := TRDTPArchiveClient.create(archivehost, archiveEndpoint);
  end;

end;

procedure TResurrector.CopysourceTable;
begin
  movemem32(@tablecopy[0], @disk.vat.table[0], sizeof(disk.vat.table));
end;

procedure TResurrector.Detach;
begin
  Disconnect(true);
  inherited;

end;

procedure TResurrector.Disconnect(bHard: boolean = false);
begin
  if cli <> nil then begin
    if not bHard then
      cli.disconnect;
    cli.free;
    cli := nil;
  end;

end;

procedure TResurrector.DoExecute;
begin
  inherited;

  if self.ArchiveHost = '' then begin
    HasWork := false;
    exit;
  end;

  try
    if idx < MAX_BLOCKs_IN_VAT then
      Rebuild;
  except
    RepeatScan := true;
  end;
  FastForward;

end;

procedure TResurrector.FastForward;
begin
  Prep;
  while true do begin
    if seekopt >= 0 then begin
      repeatscan := true;
      idx := seekopt;
      forwardcount := 40;
      seekopt := -1;
    end;

    //if we're optimizing for the last known read-position
    if (forwardcount > 0) and (idx < (disk.BigBlocksInDisk)) then begin
      inc(idx);
      dec(forwardcount);
      if (tablecopy[idx].FileCount <= 0)
      and (tablecopy[idx].FileCount <> -4)
      then begin
        Debug.Log(inttohex(idx,1)+'  fwd  '+tablecopy[idx].FileCount.tostring);
        exit;//<-------STOP HERE
      end;
    end else
    //else if we're generally trying to resurrect the entire disk
    begin
      dec(idx);
      if idx>=0 then begin
//      if (idx=0)
      //if we're at the edge of a resurrected area
//      or ((tablecopy[idx-1].FileCount>0) and (tablecopy[idx-1].filecount <> -4))
//      then begin
        //if we haven't tried to resurrect this part of the disk
        if (tablecopy[idx].FileCount <> -4) and (tablecopy[idx].FileCount <= 0) then begin
          Debug.Log(inttohex(idx,1)+'  filecount  '+tablecopy[idx].FileCount.tostring+','+tablecopy[idx-1].FileCount.tostring);
          exit;//<---stop here
        end;
//      end;
      end;
    end;


    //REPEAT IF NECESSARY
    if idx < 0 then begin
      if repeatscan then begin
        idx := disk.BigBlocksInDisk-1;
        repeatscan := false;
        Prep;
      end else begin
        Status := 'Finished!';
        disk.queue.maxitemsInQueue := 32;
        Disconnect;
        finished := true;
        haswork := false;
        exit;
      end;
    end;
  end;
end;

function TResurrector.GetArchiveHost: string;
begin
  result := disk.sourcearchivehost;
end;

function TResurrector.GetArchiveName: string;
begin
  result := disk.sourcearchive;
end;

function TResurrector.GetArchivePort: string;
begin
  result := disk.sourcearchiveendpoint;
end;

function TResurrector.GetPin: TDateTime;
begin
  result := disk.SourceArchivePinID;
end;

procedure TResurrector.Give;
var
  l: TLock;
begin

  l := disk.GetLock;
  try
    if (disk.vat.table[idx].FileCount <= 0) then begin
{$DEFINE SKIP_ALL_ZERO_RESURRECT}
{$IFDEF SKIP_ALL_ZERO_RESURRECT}
      if allzeros then begin
        Debug.Log('resurrected all zeroes.');
        disk.vat.table[idx].filecount := -4;
        disk.vat.MarkTableEntryDirtyByPtr(@disk.vat.table[idx]);
        disk.SaveVat(false);
      end else begin
{$ENDIF}
        disk.GuaranteeWriteBlocks(idx shl BIG_BLOCK_BLOCK_SHIFT, BIG_BLOCK_SIZE_IN_BYTES shr BLOCKSHIFT, @scratch[0], false);
        repeatScan := true;
{$IFDEF SKIP_ALL_ZERO_RESURRECT}
      end;
{$ENDIF}
      repeatscan := true;
    end;
  finally
    disk.unlocklock(l);
  end;
end;

procedure TResurrector.InitFromPool;
begin
  disconnect;
end;

procedure TResurrector.OnFinish;
begin
  Disconnect(true);
  inherited;

end;

procedure TResurrector.Prep;
begin
  CopySourceTable;
end;

function TResurrector.Rebuild: boolean;
var
  l: TLock;
  c: Tcmd_SourceFetch;
  t: ni;
  dyn: TDynByteArray;
begin
  if (tablecopy[idx].FileCount <> -4) and (tablecopy[idx].FileCount <= 0) then begin
    Status := 'Resurrect '+inttohex(idx,0)+booltostrex(repeatscan,' Repeat', ' No Repeat');
    Step := disk.BigBlocksInDisk - idx;
    StepCount := MAX_BLOCKS_IN_VAT;
  //  repeatscan := true;//<---anytime we rebuild, we'll need to repeat the scan until there's nothing left to rebuild

    Connect;
    for t:= 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
      c := Tcmd_SourceFetch.create;
      try
        c.arc := cli;
        c.zidx := (idx * 4) + t;
        c.pin := pin;
        c.archivename := archivename;
        setlength(dyn,  ARC_ZONE_SIZE_IN_BYTES);
        c.pointer := dyn;
        c.resources.SetResourceUsage('SourceFetch', 0.25);
        c.start;
        while not c.WaitFor(4000) do
          if Terminated or StopRequested then begin
            c.cancel;
            exit(false);
          end;

        movemem32(@scratch[ARC_ZONE_SIZE_IN_BYTES * t], @dyn[0], ARC_ZONE_SIZE_IN_BYTES);
      finally
        if not c.IsCancelled then
          c.Free
        else begin
          c.WaitFor;
          GarbageCollect(c);
        end;

        c := nil;
      end;
    end;


    Give;
  end;



  result := true;


end;

procedure TResurrector.Reset;
begin
  seekopt := -1;
  idx := disk.BigBlocksInDisk;
  HasWork := true;
  ColdRunInterval := 500;
end;

{ TVDRecord }

function TRecordedOp.DebugString: string;
begin
  result := RecToDebugSTring(@self, typeinfo(TRecordedOp));
end;

function TRecordedOp.GetCS(idx: ni): int64;
var
  pcs: PInt64;
begin
  pcs := @fcs0;

  pcs := PInt64(PByte(pcs)+(idx*sizeof(int64)));
  result := pcs^;


end;

procedure TRecordedop.Init;
begin
  op := 0;
  block := 0;
  blockcount := 0;
  fcs0 := 0;
  fcs1 := 0;
  fcs2 := 0;
  fcs3 := 0;
  fcs4 := 0;

end;

procedure TRecordedOp.SetCS(idx: ni; const Value: int64);
var
  pcs: PInt64;
begin
  pcs := @fcs0;

  pcs := PInt64(PByte(pcs)+(idx*sizeof(int64)));
  pcs^ := value;

end;

{ Tcmd_ReplayLog }

procedure Tcmd_ReplayLog.DoExecute;
begin
  inherited;
  vda.replaylog;
end;

{ Tcmd_RebuildOperation }

procedure Tcmd_RebuildOperation.DoExecute;
begin
  inherited;

  if (not fromtarget) or  targetrebuild then
    sourcerebuild;
end;

function Tcmd_RebuildOperation.GetStartAddr: int64;
begin
  result := bbid shl BIG_BLOCK_BYTE_SHIFT;
end;

procedure Tcmd_RebuildOperation.InitExpense;
begin
  inherited;
  CPUExpense := 0;
end;

procedure Tcmd_RebuildOperation.SourceRebuild;
var
  l: TLock;
  bbidx: int64;
  s: string;
  zidx_start, zidx_last, zidx: int64;
  c: Tcmd_SourceFetch;
  t: ni;
  bMakeNewcommands: boolean;
  doneflags: array[0..ARC_ZONES_PER_BIG_BLOCK-1] of boolean;
  tmStart: ticker;
  function AllDone: boolean;
  var
    yy: ni;
  begin
    result := true;
    for yy := 0 to ARC_ZONES_PER_BIG_BLOCK-1 do
      if doneflags[yy] = false then exit(false);
  end;

begin
  step := 0;
  stepcount := BIG_BLOCK_SIZE_IN_BYTES;
  zidx := bbid shl BIG_BLOCK_BLOCK_SHIFT;
  zidx := zidx shr ARC_ZONE_BLOCK_SHIFT;

  Debug.Log('----Rebuild from Source Archive '+inttohex(bbid, 16));
  zidx_start := zidx;
  zidx_last := zidx + ARC_ZONES_PER_BIG_BLOCK-1;

      //IF THERE ARE SOURCE FETCH COMMANDS RUNNING
      if (FSourceFetchCommands[0] <> nil)
      //AND THE SOURCE FETCH COMMANDS DO NOT MATCH
      and (FSourceFetchCommands[0].zidx <> zidx_start) then begin
        Debug.Log('Prefetch miss!');
        //MISS! wait for unmatched commands
        for t:= 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
          try
            //Debug.Log('Miss Wait for ' +FSourceFetchCommands[0].zidx.tostring);
            c:= FSourceFetchCommands[t];
            FsourceFetchcommands[t] := nil;
            c.waitfor;
            c.free;
            c := nil;
          except
          end;
        end;
      end
      //else (if there are no commands or the commands don't match)
      else begin
        //Start NEW commands
        for zidx := zidx_start to zidx_last do begin
          Debug.Log('Begin getLog zidx='+inttohex(zidx, 1));
          c := Tcmd_SourceFetch.create;
          c.arc := d.SourceArchivers[zidx mod CONCURRENT_SOURCE_ARCHIVER_CONNECTIONS];
          c.zidx := zidx;
          c.pin := d.sourcearchivepinid;
          c.archivename := d.sourcearchive;
          //not needed, pointer is actually output array c.pointer := @self.data[zidx shl ARC_ZONE_BYTE_SHIFT and  ARC_ZONE_BYTE_ALIGN_MASK];
          FsourceFetchCommands[zidx mod ARC_ZONES_PER_BIG_BLOCK] := c;
          c.resources.SetResourceUsage('SourceFetch', 0.25);
          c.resources.SetResourceUsage('SourceFetch'+inttostr(zidx mod CONCURRENT_SOURCE_ARCHIVER_CONNECTIONS), 1.0);
          c.start;
        end;

      end;

      //initialize the done flags
      for t := 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
        doneflags[t] := false;
      end;

      //when a command is complete, swap for a prefetch command
      tmStart := GEtTicker;
      while not AllDone do begin
//        Debug.Log('Waiting for Rebuild Commands. '+inttostr(getTimeSince(tmStart) div 1000));
        for t := 0 to ARC_ZONES_PER_BIG_BLOCK-1 do begin
          c := FSourceFetchCommands[t];
          IF c = nil then
            doneflags[t] := true
          else begin
            c.waitfor(100);
            if c.IsComplete then begin
              try
                //finish command
                c.WaitFor;
                c.free;
                c := nil;
                step := (t+1) * ARC_ZONE_SIZE_IN_BYTES;

                FSourceFetchCommands[t] := c;
                zidx := zidx_start+t;
                Debug.Log('GetLog_Response zidx='+inttohex(zidx, 1));
              finally
                doneflags[t] := true;
              end;
            end else
              d.FlushBuffers(false, 0);
          end;
        end;
      end;



    while not (d.FlushBuffers(true, 0)= bsNoBuffersToFlush) do sleep(0);
//      FlushBuffersInBlockOrder(zidx shl ARC_ZONE_BLOCK_SHIFT, false, false);


end;

function Tcmd_RebuildOperation.TargetRebuild: boolean;
begin
  result := false;
end;

initialization

orderlyinit.Init.RegisterProcs('VirtualDisk_Advanced', oinit, ofinal,
  'ManagedThread,CommandProcessor,ApplicationParams,raid');
hits := 0;

end.
