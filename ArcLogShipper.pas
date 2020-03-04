unit ArcLogShipper;
{x$DEFINE DISABLE_SHIPPER}
{x$DEFINE USE_TCP}

interface

{x$DEFINE LOCAL_TEMP}
{$DEFINE SUCCESS_HOOK}
{$DEFINE COMBINE_RING_LOGS}
{x$DEFINE USE_RING_FILE}

{x$DEFINE USE_NEW_ARC_ZONE_CHECKSUMS}
{x$DEFINE POST_BY_COMMAND}
{x$DEFINE VERIFY}


//truth

//MODE - Real-time
//  Mainthread
//  -- If there's room in the ring put it.  Update local arcmap right away
//  --else switch modes, flag zone in arcmap with newer rev, but don't add anything to ring
// ShipperThread
// -- Get stuff from ring
// -- If the stuff is >= arcmap rev level the push it
// -- If the stuff is > arcmap rev level then update arcmap

//MODO - Pseudo-real-time
// Mainthread
// -- If there's room in the ring put it. Update Local arcmap right away
// -- flag zone in arcmap with newer rev, but don't add anything to ring
// -- if zoneidx < validationidx then flag that we need to make another pass
// ShipperThread
// -- Get stuff from ring
// --  If the stuff is >= arcmap rev level the push it
// --  If the stuff is > arcmap rev level then update arcmap
// -- CheckRev level At validationidx
// --  if arcmap rev > remote rev then
// --     fetch zone, push zone



//  1. LOCKED     - Gather DAta - localrev, remoterev
//  2. serialized - Get Data from Disk
//  2. UNLOCKED   - Push Data - remoterev is changed to remoterev + 1
//  3. LOCKED     - Commit - localrev is updated to


//Race #1
// 3 STATES to worry about - LocalRev (target) RemoteRev, RRC (RemoteRevCache)
//  ------ Start Local [0] RRRC [0] Remote[0] --------------------
// [THREAD 1 -----------] [THREAD 2 ----------]
// Gather [0][0][0] T=1
//                        Gather [0][0][0] T=1
// Push 0->1
//                        Push 0->1
// ^ rslt = TRUE           ^result = FALSE (backend rejected it under lock)
// State is [0][0][1]
// Commit OK! [1][1][1]
//                        Rollback Check/Verify that backend is [1] and force change to RRC
//                        Gather [1][1][1] T=2
//                        Push 1->2
//                         ^result = TRUE
//                        State is [1][1][2]
//                         Commit OK! [2][2][2]
//  ------ End  Local [2] RRC[2] Remote [2] --------------------

//Race #2
// 3 STATES to worry about - LocalRev (target) RemoteRev, RRC (RemoteRevCache)
//  ------ Start Local [0] RRC[0] Remote [0] --------------------
// [THREAD 1 -----------] [THREAD 2 ----------]
// Start [0][0][0]
// Gather [0][0][0]
// Push 0->1 (OK!)
// STate is [0][0][1]
//                        Gather[0][0][0] (since there's a change gather to reach backend, state might be [0][1][1])
// Commit OK! [1][1][1]
//
//                        Push 0->1 (FAIL!)
//                        Rollback Check/Verify that backend is [1]
//                        Gather[1] Target [2]
//                        Push 1->2
//                        ^result = TRUE
//                        Commit OK! [2][2][2]
//  ------ End Local [2] Remote [2] RRC[2] --------------------

//Race #2a gather takes state from backend
// 3 STATES to worry about - LocalRev (target) RemoteRev, RRC (RemoteRevCache)
//  ------ Start Local [0] RRC[0] Remote [0] --------------------
// [THREAD 1 -----------] [THREAD 2 ----------]
// Start [0][0][0]
// Gather [0][0][0]
// Push 0->1 (OK!)
// STate is [0][0][1]
//                        Gather[0][1][1] (alternate result fetched from back-end
// Commit OK! [1][1][1]
//                        Push 1->2
//                        ^result = TRUE
//                        Commit OK! [2][2][2]
//  ------ End Local [2] Remote [2] RRC[2] --------------------


//Race #3 (Validate/refresh vs. Ring Push)
// 3 STATES to worry about - LocalRev (target) RRC (RemoteRevCache) RemoteRev
//  ------ Start Local [0] RRC[0] Remote [0] --------------------
// [THREAD 1 (VR)---------] [THREAD 2 -------------] [THREAD 3 -------------]
// Start  [0][0][0]
// Gather [0][0][0] target [1]
// Push 0->1 (OK!)
// Commit OK! [1][1][1]
//                          L Examine localrev [1]
//                          L Add Data to Ring [1->2]
//                          L Update localrev [2][1][1]
//                                                    Get DAta From Ring [1->2]
//                                                    FALSE [1]>=[2] //if RRC >= localrev then data irrelevant
//                                                    Push [1->2] OK! [2][1][2]
//                                                    Commit! OK! [2][2][2]
//                          L Examine localrev [2]
//                          L Add Data to Ring [2->3]
//                          L Update localrev [3][2][2]
//                                                    Get DAta From Ring [2->3]
//                                                    FALSE [2]>=[3] //if RRC >= localrev then data irrelevant
//                                                    Push [2->3] OK! [3][2][3]
//                                                    Commit! OK! [3][3][3]


//Race #3A (Validate/refresh vs. Ring Push)
// 3 STATES to worry about - LocalRev (target) RRC (RemoteRevCache) RemoteRev
//  ------ Start Local [0] RRC[0] Remote [0] --------------------
// [THREAD 1 (VR)---------] [THREAD 2 -------------] [THREAD 3 -------------]
// Start  [0][0][0]
// Gather [0][0][0] target [1]
// Push 0->1 (OK!)
// Commit OK! [1][1][1]
//                          L Examine localrev [1]
//                          L Add Data to Ring [1->2]
//                          L Update localrev [2][1][1]
//                                                    Get DAta From Ring [1->2]
//                          L Examine localrev [2]
//                          L Add Data to Ring [2->3]
//                          L Update localrev [3][1][1]
//                                                    FALSE [1]>=[2] //if RRC >= localrev then data irrelevant
//                                                    Push [1->2] OK! [3][1][2]
//                                                    Commit! OK! [3][2][2]
//                                                                              //
//                                                    Get DAta From Ring [2->3]
//                                                    FALSE [2]>=[3] //if RRC >= localrev then data irrelevant
//                                                    Push [2->3] OK! [3][2][3]
//                                                    Commit! OK! [3][3][3]
// Final STate [3] [3] [3] OK!

//Race #3B (Validate/refresh vs. Ring Push)
// 3 STATES to worry about - LocalRev (target) RRC (RemoteRevCache) RemoteRev
//  ------ Start Local [0] RRC[0] Remote [0] --------------------
// [THREAD 1 (VR)---------] [THREAD 2 -------------] [THREAD 3 -------------]
// Start  [0][0][0]
// Gather [0][0][0] target [1]
// Push 0->1 (OK!)
// Commit OK! [1][1][1]
//                          L Examine localrev [1]
//                          L Add Data to Ring [1->2]
//                          L Update localrev [2][1][1]
//                                                    Get DAta From Ring [1->2]
//                                                    FALSE [1]>=[2] //if RRC >= localrev then data irrelevant
//                                                    Push [1->2] OK! [2][1][1->2]
//                          L Examine localrev [2]
//                          L Add Data to Ring [2->3]
//                          L Update localrev [2->3][1][2]
//                                                    Commit! OK! [3][1->2][2]
//                                                                              //
//                                                    Get DAta From Ring [2->3]
//                                                    FALSE [2]>=[3] //if RRC >= localrev then data irrelevant
//                                                    Push [2->3] OK! [3][2][2->3]
//                                                    Commit! OK! [3][2->3][3]
//   Final STate [3] [3] [3] OK!

//Race #3C (Validate/refresh vs. Ring Push)
// 3 STATES to worry about - LocalRev (target) RRC (RemoteRevCache) RemoteRev
//  ------ Start Local [0] RRC[0] Remote [0] --------------------
// [THREAD 1 (VR)---------] [THREAD 2 -------------] [THREAD 3 -------------]
// Start  [0][0][0]
// Gather [0][0][0] target [1]
// Push 0->1 (OK!)
// Commit OK! [1][1][1]
//                          L Examine localrev [1]
//                          L Add Data to Ring [1->2]
//                          L Update localrev [2][1][1]
//                                                    Get DAta From Ring [1->2]
//                                                    FALSE [1]>=[2] //if RRC >= localrev then data irrelevant
//                                                    Push [1->2] OK! [2][1][1->2]
//                          L Examine localrev [2]
//                          L Add Data to Ring [2->3]
//                          L Update localrev [2->3][1][2]
// Start  [3][1][2]
// Gather [3][1][2] target [2]
// Push [1->2] (FAIL! Backend is at 2!}
// Rollback! Update Local State to match backend [3][1->2][2]
// RETRY!
// Gather [3][2][2] target [3]
// Push [3][2][2->3] ok!
// Commit OK! [3][2->3][3]
//                                                    Commit! OK! [3][3(nc)][3]
//                                                                              //
//                                                    Get DAta From Ring [2->3]
//                                                    TRUE [3]>=[3] //if RRC >= localrev then data irrelevant
//                                                    No Push Since Equal!
//                                                    Commit! OK! [3][3(nc)][3]
//   Final STate [3] [3] [3] OK!


//Race #3C (Validate/refresh vs. Ring Push)
// 3 STATES to worry about - LocalRev (target) RRC (RemoteRevCache) RemoteRev
//  ------ Start Local [0] RRC[0] Remote [0] --------------------
// [THREAD 1 (VR)---------] [THREAD 2 -------------] [THREAD 3 -------------]
// Start  [0][0][0]
// Gather [0][0][0] target [1]
// Push 0->1 (OK!)
// Commit OK! [1][1][1]
//                          L Examine localrev [1]
//                          L Add Data to Ring [1->2]
//                          L Update localrev [2][1][1]
//                                                    Get DAta From Ring [1->2]
//                                                    FALSE [1]>=[2] //if RRC >= localrev then data irrelevant
//                                                    Push [1->2] OK! [2][1][1->2]
//                          L Examine localrev [2]
//                          L Add Data to Ring [2->3]
//                          L Update localrev [2->3][1][2]
// Start  [3][1][2]
// Gather [3][1][2] target [2]
// Push [1->2] (FAIL! Backend is at 2!}
// Rollback! Update Local State to match backend [3][1->2][2]
// RETRY!
// Gather [3][2][2] target [3]
// Push [3][2][2->3] ok!
//                                                    Commit! OK! [3][1->2)][3]
// Commit OK! [3][2->3][3]
//                                                                              //
//                                                    Get DAta From Ring [2->3]
//                                                    TRUE [3]>=[3] //if RRC >= localrev then data irrelevant
//                                                    No Push Since Equal!
//                                                    Commit! OK! [3][3(nc)][3]
//   Final STate [3] [3] [3] OK!


//Race #3C (Validate/refresh vs. Ring Push)
// 3 STATES to worry about - LocalRev (target) RRC (RemoteRevCache) RemoteRev
//  ------ Start Local [0] RRC[0] Remote [0] --------------------
// [THREAD 1 (VR)---------] [THREAD 2 -------------] [THREAD 3 -------------]
// Start  [0][0][0]
// Gather [0][0][0] target [1]
// Push 0->1 (OK!)
// Commit OK! [1][1][1]
//                          L Examine localrev [1]
//                          L Add Data to Ring [1->2]
//                          L Update localrev [2][1][1]
//                                                    Get DAta From Ring [1->2]
//                                                    FALSE [1]>=[2] //if RRC >= localrev then data irrelevant
// Start  [2][1][1]
// Gather [2][1][1] target [2]
// Push [2][1][1->2] OK!
//                                                    Push [1->2] FAIL! [2][1][!!! is already 2]
//                                                    Rollback refresh state [2][1->2][2]
//                                                    TRUE [2]>=[2] //if RRC >= localrev then data is irrelevant
//                                                    No push Since Equal!
//                          L Examine localrev [2]
//                          L Add Data to Ring [2->3]
//                          L Update localrev [2->3][1][2]
// Commit OK! [3][2(nc)][2]
//                                                    Commit! OK! [3][2nc)][2]
//                                                                              //
//                                                    Get DAta From Ring [2->3]
//                                                    FALSE [2]>=[3] //if RRC >= localrev then data irrelevant
//                                                    Push [3][2][2->3]
//                                                    Commit! OK! [3][2->3][3]
//   Final STate [3] [3] [3] OK!


//TODO 1: Make sure that on transaction commit, localrev only rolls FORWARD

uses
  numbers, typex, systemx, stringx, RDTPArchiveClient, LockQueue, managedthread, sysutils, ringfile, virtualdiskconstants, signals, arcmap, debug, tickcount, commandprocessor, simplequeue, ringbuffer, transaction, generics.collections, better_collections, commandicons;

const
  RESERVE_ID_COUNT = 1000000;
  MAX_UNIQUE_CLIENTS = 32;
  RESERVE_UNIQUE_CLIENTS = 24;
  ZONES_TO_BACKUP = { $200} MAX_ZONES;
  ARC_ZONE_CHECKSUM_SIZE = $10000;
type
{$IFDEF USE_RING_FILE}
  TLocalRing = TRingFile;
{$ELSE}
  TLocalRing = TDoubleRingBuffer;
{$ENDIF}
  TArcMode = (arcValidateRefresh, arcRealTime);
  TArcLogShipper = class;//forward
  Tcmd_ContinueValidateLogRefresh = class;//forward
  Ttrans_LogThis = class;//forward
  TArcTransactionalCommand = class;//forward
  TArcLogShippingthread = class(TManagedThread)
  public
    shipper: TArcLogShipper;
    procedure DoExecute;override;
  end;

  TLogShipHeader = packed record
  public
    logid: int64;
    blockstart: int64;
    blocklength: int64;
    bytelength: int64;
    headercheck: int64;
    function ComputeCheck: int64;inline;
    procedure SetCheck;inline;
    function IsCheckValid: boolean;inline;
    procedure Init;inline;
  end;

  TArcLogShipper = class(TFakeLockQueue)
  private
    FTargetArchive: string;
    FReadyForRealtime: boolean;
    FSourceArchive: string;
    FClient, FQUickClient: TRDTPArchiveClient;
    FActive: boolean;
    FArchive: string;
    ring: TLocalRing;
    FStatus: string;
    FThr: TArcLogShippingThread;
    [volatile] FMode: TArcMode;
    temp: TDynByteArray;
    function GetEndpoint: string;
    function GetHost: string;
    procedure SetEndPOint(value: string);
    procedure SetHost(value: string);
    procedure SetArchive(value: string);
    function GetRingFileName: string;
    procedure SetRingFileName(value: string);
    procedure SetMode(const Value: TArcMode);
    procedure CancelValBAtches;
    function NoneUsingClient(cli: TRDTPArchiveClient): boolean;
    procedure FinishValBatches(bAll: boolean; bCancel: boolean = false);
    procedure CollapseValbatches;
    function SearchValBatchForZone(zidx: int64; bCheckOnlyTransLogThis: boolean): TArcTransactionalCommand;
    function GetArcZonesInDisk: int64;
    function GetdiskSize: int64;
    function GEtQuickClient: TRDTPArchiveClient;
    protected
    function CanActivate: boolean;
    procedure Activate;
    procedure DestroyClientPool;
  public
    FClients: TSharedList<TRDTPArchiveClient>;
    disk: TObject;
//    arcmap: TArcMap;
    validateidx: int64;
    error_time: ticker;
    ValidateRefreshRepeat: boolean;
    Enabled: boolean;
    remote_block_start:int64;
    remote_rev_cache: TDynInt64Array;
    ids_reserved_to: int64;
    next_id: int64;
    bPosting: boolean;
    bJammed: boolean;
    pauseforfetch: boolean;
    arcmap_validated: boolean;
    FBackgroundCommands: TSimpleQueue;
    tmLastDiskBusyTime: ticker;
    csPrep, csClient, csQuickClient: TCLXCriticalSection;
{$IFNDEF DYNAMIC_VALBATCH}
    valbatchcount: ni;
{$ENDIF}
    valbatch: array[0..1023] of TArcTransactionalCommand;
    newbatch: array[0..1023] of TTrans_LogThis;
    unique: array[0..1023] of TRDTPArchiveClient;
//    scratchpad: array [0.._BIG_BLOCK_SIZE_IN_BLOCKS div ARC_ZONE_SIZE_IN_BLOCKS] of int64;
    function HasRemoveRevCached(idx: int64): boolean;
    procedure Detach;override;
    constructor Create;override;
    destructor Destroy;override;


    procedure StopThread;
    procedure StartThread;
    function NeedClient: TRDTPArchiveClient;
    procedure NoNeedClient(cli: TRDTPArchiveClient);
    function LogThis(const blockstart, blocklength: int64; const data: Pbyte): boolean;
    function LogThis_OLD(const blockstart, blocklength: int64; const data: Pbyte): boolean;
    function FetchLog(const blockstart, blocklength: int64; const outdata: PByte): boolean;

//    property TargetArchive: string read FTargetArchive write FTargetArchive;
//    property SourceArchive: string read FSourceArchive write FSourceArchive;
    property ReadyForRealTime: boolean read FReadyForRealtime;
    property Client: TRDTPArchiveClient read FClient;
    property QuickClient: TRDTPArchiveClient read GEtQuickClient;
    property Host: string read GetHost write SetHost;
    property Endpoint: string read GetEndpoint write SetEndPOint;
    property Archive: string read FArchive write SetArchive;
    property Active: boolean read FActive;
    property RingfileName: string read GetRingFileName write SetRingFileName;
    property Status: string read FStatus;
    procedure Alert(s: string);
    procedure DoThreadStuff;
    procedure ShipLogsToServer;
    procedure AddVAlBatch(c: TArcTransactionalCommand);
    procedure ContinueLogValidateRefresh;
    property Mode: TArcMode read FMode write SetMode;
    procedure ModeChanged;
    procedure Shovel;
    procedure SetRemoteLogRevCache(idx: int64; logid: int64);
    function GetREmoteLogRev(idx: int64): int64;
    procedure PutRemoteLogRevInCache(idx: int64;logid: int64);
    function GetNextLogId(zoneidx:int64): int64;
    function ArcMapValid: boolean;
    function ArcZoneValidateOrReset(zonebase: int64): boolean;
    procedure SaveARcmapChecksum;
    procedure LogSuccessX(const zoneidx, logid: int64);
    procedure FInishBackGroundCommand;
    function LogThis_Best(logid, startblock, blocklength: int64; data: TDynByteArray): boolean;
    function CountUniqueClients(bOnlyValRefresh: boolean): ni;
    property DiskSize: int64 read GetdiskSize;
    property ArcZonesInDisk: int64 read GetArcZonesInDisk;
    function Defined: boolean;
  end;


  TArcTransactionalCommand = class(TTransactionalCommand)
  public
    zidx: int64;
    client: TRDTPArchiveClient;
    shipper: TArcLogShipper;
  end;

  Ttrans_LogThis = class(TArcTransactionalCommand)
  private
    procedure SetStartBlock(const Value: int64);
  protected
    success: boolean;
    Fstartblock: int64;
    function LockState: Boolean; override;
    procedure UnlockState; override;
    procedure GatherStartSTate; override;
    function ReEvalAgainstStartState: TTransactionResult; override;
    procedure DoCommit; override;
    function Perform: TTransactionResult; override;
    procedure DoRollback; override;
  public
    known_fromid_at_creation: int64;
    fromid: int64;
    this_logid: int64;
    archive: string;
    data: TDynByteArray;
    blocklength: int64;
    procedure Init; override;
    property StartBlock: int64 read FStartBlock write SetStartBlock;
    procedure InitExpense; override;
    function CanCombine(h: TLogShipHeader;indata:TDynByteArray): boolean;
    function DoCombine(h: TLogShipHeader;indata: TDynByteArray): boolean;
    function TryCombine(h: TLogShipHeader; indata: TDynByteArray): boolean;

  end;

  Tcmd_ContinueVAlidateLogRefresh = class(TArcTransactionalCommand)
  protected
    original_local, fromid, this_logid: int64;
    entryvalidate: TArcMapEntry;
{$IFNDEF LOCAL_TEMP}
    temp: TDYnByteArray;
{$ENDIF}
    pass: boolean;
    checksumpass: boolean;
    function LockState: Boolean; override;
    procedure UnlockState; override;
    procedure GatherStartSTate; override;
    function ReEvalAgainstStartState: TTransactionResult; override;
    procedure DoCommit; override;
    procedure DoRollback;override;
    function Perform: TTransactionResult; override;

  public
    disk: TObject;
    success: boolean;
    archive: string;
    procedure InitExpense; override;
    procedure Init; override;

  end;


implementation

{ TArcLogShipper }

uses
  virtualdisk_advanced;


{ TArcLogShipper }

procedure TArcLogShipper.Activate;
begin
  FACtive := true;
end;

procedure TArcLogShipper.AddVAlBatch(c: TArcTransactionalCommand);
var
  t: ni;
begin
  for t:= 0 to high(valbatch) do begin
    if valbatch[t] = nil then begin
      valbatch[t] := c;
      inc(valbatchcount);
      exit;
    end;
  end;


{$IFDEF DYNAMIC_VALBATCH}
  setlength(valbatch, length(valbatch)+1);
  valbatch[high(valbatch)] := c;
{$ELSE}
  raise ECritical.create('no more slots');
{$ENDIF}

end;

procedure TArcLogShipper.Alert(s: string);
begin
  FStatus := s;
  Debug.Log(s);
end;

function TArcLogShipper.ArcMapValid: boolean;
var
  cs: int64;
  s: string;
  z: int64;
  csLocal: int64;
begin
  result := false;
  ECS(csClient);
  try
    try
      result := arcmap_validated;
      if not result then begin
        if (GetTimeSince(FClient.LastConnectionAttempt) > 30000) or FClient.Connected then
        begin
{$IFNDEF USE_NEW_ARC_ZONE_CHECKSUMS}
          FClient.GetStoredParam_Async(self.Archive, 'arcmap_checksum', '0');
          csLocal := TVirtualDisk_Advanced(self.disk).zonerevs.CalculateChecksum;
          s := FClient.GetStoredParam_Response;
          cs := 0;
          if s <> '' then begin
            cs := strtoint64(s);
          end;

          result := (s<>'') and (cs = csLocal);
{$ELSE}
          result := false;
{$ENDIF}
          if not result then begin
            z := 0;
            while z < self.ArcZonesInDisk do begin
              if assigned(FThr) then begin
                Fthr.Step := z;
                FThr.stepcount := self.ArcZonesInDisk;
              end;
              ArcZonevalidateOrReset(z);
              inc(z, ARC_ZONE_CHECKSUM_SIZE);
            end;
            result := true;
          end;

          arcmap_validated := true;

        end;
      end;
    except
      FClient.Disconnect;
    end;
  finally
    LCS(csclient);
  end;

end;

function TArcLogShipper.ArcZoneValidateOrReset(zonebase: int64): boolean;
var
  cs: int64;
  s: string;
  csLocal: int64;
begin
  ECS(csClient);
  try
  result := arcmap_validated;
  if not result then begin
    if (GetTimeSince(FClient.LastConnectionAttempt) > 30000) or FClient.Connected then
    begin
{$IFNDEF USE_NEW_ARC_ZONE_CHECKSUMS}
      FClient.GetStoredParam_ASYNC(self.Archive, 'arcmap_checksum_zone_'+inttohex(zonebase,8), '0');
      csLocal := TVirtualDisk_Advanced(self.disk).zonerevs.CalculateZoneChecksum(zonebase, ARC_ZONE_CHECKSUM_SIZE);
      s := FClient.GetStoredParam_Response;
      cs := 0;
      if s <> '' then begin
        cs := strtoint64(s);
      end;
{$ELSE}
      FClient.GetArcVatCheckSum_Async(self.Archive, zonebase, ARC_ZONE_CHECKSUM_SIZE);
      csLocal := arcmap.CalculateZoneChecksum2(zonebase, ARC_ZONE_CHECKSUM_SIZE);
      cs := FClient.GetArcVatCheckSum_Response;
{$ENDIF}


      result := (s<>'') and (cs = csLocal);
      if not result then begin
        TVirtualDisk_Advanced(self.disk).zonerevs.ResetZone(zonebase, ARC_ZONE_CHECKSUM_SIZE,true);
        result := true;
      end;


    end;
  end;
  finally
    LCS(csclient);
  end;
end;

function TArcLogShipper.CanActivate: boolean;
begin
  result := (Archive <> '') and (FCLient.Host <> '') and (FClient.Endpoint <> '');
//              and (FQuickCLient.Host <> '') and (FQuickCLient.Endpoint <> '');
end;

procedure TArcLogShipper.CancelValBAtches;
var
  t: ni;
  cmd:  TArcTransactionalCommand;
begin
  exit;
  for t := 0 to high(valbatch) do begin
    cmd := valbatch[t];
    if cmd <> nil then begin
      try
        cmd.Cancel;
      except
      end;
    end;
  end;
end;



function TArcLogShipper.CountUniqueClients(bOnlyValRefresh: boolean): ni;
var
  t: ni;
  uidx: ni;
  function Has(c: TRDTPArchiveClient): boolean;
  var
    t: ni;
  begin
    for t:= 0 to uidx-1 do begin
      if unique[t]=c then
        exit(true);
    end;
    exit(false);
  end;
begin
  uidx := 0;
  for t:= 0 to high(valbatch) do begin
    if valbatch[t] <> nil then begin
      if not has(valbatch[t].client) then begin
        unique[uidx] := valbatch[t].client;
        if (not bOnlyValRefresh) or (valbatch[t] is Tcmd_ContinueValidateLogRefresh) then
          inc(uidx);
      end;
    end;
  end;
  exit(uidx);
end;

procedure TArcLogShipper.FinishValBatches(bAll: boolean; bCancel: boolean = false);
var
  t,u: ni;
  cmd:  TArcTransactionalCommand;
  iFound: ni;

begin
  if bAll then begin
    for t := 0 to high(valbatch) do begin
      cmd := valbatch[t];
      if cmd <> nil then begin
        try
          cmd.Cancel;
          cmd.WaitFor;
        except
        end;
        valbatch[t] := nil;
        dec(valbatchcount);

        if NoneUsingClient(cmd.client) then
          NoNeedClient(cmd.client);
        cmd.SelfDestruct(2000);
//        cmd.free;
        cmd := nil;
      end;
    end;
  end else begin
    iFound := 0;
    for t := 0 to high(valbatch) do begin
      cmd := valbatch[t];
      if (cmd <> nil) and (cmd.IsComplete) then begin
        try
          cmd.WaitFor;
        except
        end;
        valbatch[t] := nil;
        if NoneUsingClient(cmd.client) then
          NoNeedClient(cmd.client);

        cmd.SelfDestruct(2000);
//        cmd.free;
        cmd := nil;


        for u := t to high(valbatch)-1 do begin
          valbatch[u] := valbatch[u+1];
        end;
        valbatch[high(valbatch)] := nil;
        dec(valbatchcount);


        inc(iFound);
      end;
    end;
    if iFound = 0 then
      sleep(10);
  end;
end;

procedure TArcLogShipper.CollapseValbatches;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TArcLogShipper.ContinueLogValidateRefresh;
var
  cmd: Tcmd_ContinueVAlidateLogRefresh;
  t: ni;
  vidx: ni;
  lastc: TArcTransactionalCommand;
  bFinish: boolean;
  bRestart: boolean;
  a,b: int64;
  bfound: boolean;
  tmStart: ticker;
begin
  tmStart := getTicker;
  FinishValBatches(false);
  bFinish := false;
  bRestart := false;

  //exit if we have too many clients opened for validate/refresh
  if CountUniqueClients(true) >= RESERVE_UNIQUE_CLIENTS then
     exit;

  //exit if we have too many clients opened for all purposes
  if CountUniqueClients(false) >= MAX_UNIQUE_CLIENTS then
     exit;

  //check if any valbatches are done
//  if valbatchcount > 0 then
  repeat
    bFound := false;
    for t:= low(valbatch) to high(valbatch) do begin
      if valbatch[t] = nil then begin
        vidx := validateidx;//current index...
        if vidx >= ZONES_To_BACKUP then
          break;

        ecs(csPrep);
        try

          //GET our local log rev
          b := TVirtualDisk_Advanced(self.disk).zonerevs.GetEntry(validateidx).logid;
          if b > 0 then begin
            //get remote rev from cache (should be equal or less than our upgraded target regardless of race condition)
            a := GetREmoteLogRev(validateidx);
            if (a <> b) {or (b=0)} then begin
              bFound := true;
              cmd := Tcmd_ContinueVAlidateLogRefresh.create;
              cmd.archive := self.Archive;
              cmd.shipper := self;
                cmd.zidx := vidx;
    //          FThr.Status := 'VR '+vidx.tohexstring;
              cmd.disk := self.disk;

              lastc := SearchValBatchForZone(vidx, false);
              if lastc <> nil then begin
                cmd.AddDependency(lastc);
                cmd.client := lastc.client;
              end else begin
                cmd.client := NeedClient;
              end;
              cmd.Start;
              valbatch[t] := cmd;
            end;
          end;

          validateidx := validateidx + 1;
          self.FThr.stepcount := ZONES_TO_BACKUP;
          self.FThr.step := validateidx;
          if validateidx = ZONES_To_BACKUP then
          begin
            validateidx := 0;
            if not ValidateRefreshRepeat then
            begin
              bFinish := true;
              ValidateRefreshRepeat := false;
              mode := arcRealTime;
              exit;
            end else begin
              ValidateRefreshRepeat := false;
            end;
          end;
        finally
          lcs(csPrep);
        end;

        if bFInish then
          FinishValBatches(true);
        if CountUniqueClients(false) >= MAX_UNIQUE_CLIENTS then
          break;

      end;
    end;
  until bFound or (GetTimeSince(tmStart) > 1000);
end;

constructor TArcLogShipper.Create;
var
  t: ni;
begin
  inherited;

  //create a command to handle the refresh
  for t:= low(valbatch) to high(valbatch) do begin
    valbatch[t] := nil;
  end;

  FClient := TRDTPARchiveClient.create('','');
  FClients := TSharedList<TRDTPARchiveClient>.create;
{$IFDEF USE_TCP}
  FClient.UseTCP := true;
{$ENDIF}

  FQuickclient := nil;

  ring := TLocalRing.create;

  setlength(temp, ARC_ZONE_SIZE_IN_BYTES);
  remote_block_start := -1;
  ICS(csClient);
  ICS(csPrep);
  ICS(csQuickClient);
  FBackgroundCommands := tpm.Needthread<TSimpleQueue>(nil);
  FBackGRoundCommands.start;




end;

function TArcLogShipper.Defined: boolean;
begin
  result := ARchive <> '';
end;

destructor TArcLogShipper.Destroy;
begin
  CAncelValBatches;
  StopThread;
  FBackGroundCOmmands.WAitForAll;
  FBackGroundCommands.Stop;
  FBackGroundCommands.SafeWaitFor;
  TPM.NoNeedThread(FBackGroundCommands);


  FClient.free;
  FClient := nil;
  FQuickClient.free;
  FQuickClient := nil;
  ring.free;
  ring := nil;
  DCS(csPrep);
  DCS(csClient);
  DCS(csQuickClient);
  FClients.free;

  inherited;
end;

procedure TArcLogShipper.DestroyClientPool;
var
  cli: TRDTPArchiveClient;
begin
  FClients.lock;
  try
    while FClients.count > 0 do begin
      cli := FClients[FClients.count-1];
      FClients.Delete(FClients.count-1);
      cli.free;
      cli := nil;
    end;
  finally
    FClients.unlock;
  end;
end;

procedure TArcLogShipper.Detach;
begin
  if detached then
    exit;
  inherited;
  CAncelValBatches;
  StopThread;
  FinishValBAtches(true, true);
  SaveARcmapChecksum;
  //TPM.NoNeedthread(FThr);already taken care of in stopthread
  DestroyClientPool;


end;

procedure TArcLogShipper.DoThreadStuff;
begin
  if Mode = arcValidateRefresh then begin
    self.ContinueLogValidateRefresh;
  end else begin
    self.ShipLogsToServer;
  end;

end;

function TArcLogShipper.FetchLog(const blockstart, blocklength: int64;
  const outdata: PByte): boolean;
var
  dba: TDynByteArray;
  iGot: int64;
begin
  try
  result := false;
  IF not TECS(csPrep) then
    exit;
  try
    result := false;

    if (not ring.WaitForEmptySignal(1))  then
    begin
      Alert('Possible loss of data because logs are not empty.');
//      exit;
    end;

    ring.Lock;
    try
      if ring.DataAvailable > 0 then
      begin
        Alert('Possible loss of data because logs are not empty.');
        //exit;
      end;

//      if active then
      begin
        FinishBackgroundCommand;
        ecs(csQuickClient);
        try
          QuickClient.timeout := 60000;
          try
            iGot := QuickClient.GetLog(Archive, -1, blockstart, blocklength, {out} dba);
          except
            FQuickClient.free;
            FQuickClient := nil;
            raise;
          end;
        finally
          lcs(csQuickClient);
        end;
        if iGot = 0 then
        begin
          exit;
        end else
        if (iGot <> blocklength) then
          raise ECritical.create('did not get blocklength expected '+inttostr(iGot)+'<>'+inttostr(blocklength))
        else
        begin
          movemem32(outdata, @dba[0], blocklength shl BLOCKSHIFT);//changed 5/14/2017
          result := true;
        end;
      end;
    finally
      ring.unlock;
    end;
  finally
    LCS(csPrep);
  end;
  except
    on e: exception do begin
      Debug.Log(self, 'Exception in fetchlog:'+e.message);
      result := false;
    end;
  end;

end;

procedure TArcLogShipper.FInishBackGroundCommand;
begin
  //FBackgroundCommands.WaitForAll;
end;

function TArcLogShipper.GetArcZonesInDisk: int64;
begin
  result := DiskSize div ARC_ZONE_SIZE_IN_BYTES;
  if (DiskSize mod ARC_ZONE_SIZE_IN_BYTES) > 0 then
    inc(result);
end;

function TArcLogShipper.GetdiskSize: int64;
begin
  result := (disk as TVirtualDisk).Size;
end;

function TArcLogShipper.GetEndpoint: string;
begin
  RESULT := FcLIENT.endPoint;
end;

function TArcLogShipper.GetHost: string;
begin
  result := FClient.Host;
end;

function TArcLogShipper.GetNextLogId(zoneidx:int64): int64;
var
  l: TLock;
  ent: TArcMapEntry;
  rem: int64;
begin
  result := -3;
//  l := GetLock;
  ecs(csPrep);
  try
    ent := TVirtualDisk_Advanced(disk).zonerevs.GetEntry(zoneidx);
    rem := GetREmoteLogRev(zoneidx);
    result := greaterof(ent.logid, greaterof(rem,0))+1;
    ent.logid := result;
    TVirtualDisk_Advanced(disk).zonerevs.PutEntry(zoneidx, ent,true);


  finally
    lcs(csPrep);
//    UnlockLock(l);
  end;

end;

function TArcLogShipper.GEtQuickClient: TRDTPArchiveClient;
begin
  if FQuickclient = nil then begin
    FQuickClient := TRDTPARchiveClient.create('','');
    FQuickClient.TimeOUt := 24000;
  {$IFDEF USE_TCP}
    FQuickClient.UseTCP := true;
  {$ENDIF}
    FQuickClient.Host := FClient.Host;
    FQuickClient.endPoint := FClient.Endpoint;
  end;
  result := FQuickClient;
end;

function TArcLogShipper.GetREmoteLogRev(idx: int64): int64;
var
  idx2: int64;
  l: Tlock;
begin
  l := GetLock;
  try
    ECS(csQuickCLient);
    try
      if not HasRemoveRevCached(idx) then begin
        remote_block_start := 0;//(idx shr 12) shl 12;
        try
          remote_rev_cache := Quickclient.GetLogRevs(archive, remote_block_start, MAX_ZONES{4096});
        except
          FQuickClient.free;
          FQuickClient := nil;
          raise;
        end;
      end;
      idx2 := idx-remote_block_start;
      result := -1;
      if idx2 < high(remote_rev_cache) then
        result := remote_rev_cache[idx-remote_block_start];
    finally
      LCS(csQuickClient);
    end;
  finally
    UnlockLock(l);
  end;
end;

function TArcLogShipper.GetRingFileName: string;
begin
  result := '';
  if ring = nil then
    exit;

{$IFDEF USE_RING_FILE}
  result := ring.filename;
{$ELSE}
  result := 'ring in memory';
{$ENDIF}



end;

function TArcLogShipper.HasRemoveRevCached(idx: int64): boolean;
begin
  if remote_block_start < 0 then
    exit(false);
  result := (idx >= remote_block_start) and (idx < (remote_block_start+length(remote_rev_cache)));
end;

procedure TArcLogShipper.LogSuccessX(const zoneidx, logid: int64);
var
  entry: TArcMapEntry;
begin
  ecs(csPrep);
  try
    entry.logid := logid;
    TVirtualDisk_Advanced(disk).zonerevs.PutEntry(validateidx, entry, false);
  finally
    lcs(csPrep);
  end;
end;

function TArcLogShipper.LogThis(const blockstart, blocklength: int64;
  const data: Pbyte): boolean;
var
  h: TLogShipHeader;
  ent: TArcMapEntry;
  t,idx1,idx2: int64;
  bUseRing: boolean;
  l: TLock;
  bSuccess: boolean;
  bNoCollision: boolean;
  rememberedlogid: int64;
  posttime: ticker;
  tmSTart: ticker;
begin
//  arcmap.Lock;
  try
    bSuccess := false;
    result := false;
    tmSTart := GetTicker;
    if gettimesince(error_time) < 60000 then begin
      self.FMode := arcValidateRefresh;
      exit(false);
    end;

    idx1 := blockstart shr ARC_ZONE_BLOCK_SHIFT;
    tmSTart := GetTicker;
    while not tecs(csPrep,1) do begin
      //sleep(0);
      if GEtTimeSince(tmStart) > 80 then begin
        self.FMode := arcValidateRefresh;
        error_time := getticker;
        exit(false);
      end;
    end;
    try
      try
        rememberedlogid := GetNExtLogID(idx1);
      except
        error_time := getticker;
        exit(false);
      end;
    finally
      lcs(csPrep)
    end;
    idx1 := blockstart and ARC_ZONE_BLOCK_BLOCK_ALIGN_MASK;
    idx2 := ((blockstart + blocklength)-1) and ARC_ZONE_BLOCK_BLOCK_ALIGN_MASK;
    idx1 := idx1 shr ARC_ZONE_BLOCK_SHIFT;
    idx2 := idx2 shr ARC_ZONE_BLOCK_SHIFT;

    posttime := Getticker;
    bNoCollision := false;
    repeat
      //Get the preparation lock, only one thread can use the prep state at a time
      if not TECS(csPrep) then begin
        sleep(1);
        continue;
      end;

      //wait for any posts of the current region to complete
      if (bPosting and ((validateidx >=idx1) and (validateidx <= idx2))) then
      begin
        bJammed := true;
        LCS(csPrep);
        sleep(1);
        if gettimesince(posttime) > 8000 then begin

          Fthr.status :='LOG SHIP FAIL!';
          error_time := getticker;
          enabled := false;
          exit;
        end;
      end else
        bNoCollision := true;
    until bNoCollision;

    bJammed := false;
    try
      //decide if we should use the ring buffer
      //we can use the ring buffer if the mode is real-time (will be switched out of real-time when full)
      //OR any time the available space is at least 1/4th the size - we just don't
      //   want to block the virtual disk activity by waiting for the ring to drain
      bUseRing := (Mode = arcRealTime) or (ring.SpaceAvailable > (ring.Size shr 2));

      //if in the appropriate modes (basicalyl any of them)
      if Mode in [arcValidateRefresh, arcRealTime] then begin
        //use the ID we got at the beginning of the function
        ent.logid := rememberedlogid;

        //go through the zone indexes affected by this string of blocks
        for t:= idx1 to idx2 do begin
          //repeat until successful
          while not bSuccess do begin
            //try to get an entry from the arcmap, it will return -3 if it can't lock it
            ent.logid := -3;
            repeat
              ent := TVirtualDisk_Advanced(disk).zonerevs.getentry(t);
              if ent.logid = -3 then
                sleep(100);
            until ent.logid <> -3;

            //try to get the preparation lock, only one thread can use the preparation state at a time
            if TECS(csPrep) then
            try
              //if not in proper mode, (see earlier assignment of bUseRing)
              //then we can still use the ring as long as our index is behind the current validateidx
              //(we've already made a pass)...
              if not bUseRing then begin
                bUseRing := t <  self.validateidx; //allow use of ring only if we've already made a pass at this index

                //...HOWEVER ... if the ring buffer is filling up or has filled up, we'll need to make another
                //validate-refresh round...
                if bUseRing and (ring.SpaceAvailable < (ring.Size shr 2)) then begin
                  ValidateRefreshRepeat := true;
                  bUseRing := false;
                end;
              end;

              ent.logid := rememberedlogid;
              if rememberedlogid > ent.logid then
                TVirtualDisk_Advanced(disk).zonerevs.PutEntry(t, ent, true);//put entry into the arcmap (NExtID), this flags the zone as needing to be pushed
                                        //we should do this regardless of whether or not we're deferring the post into the ring
                                        //either the ring will post it, or it will be pushed in the validate-refresh process
              bSuccess := true;
            finally
              LCS(csPrep);
            end;
          end;

        end;
      end;

      if bUseRing then
      begin
        h.init;
        h.blockstart := blockstart;
        h.blocklength := blocklength;
        h.bytelength := blocklength*blocksize;
        h.logid := rememberedlogid;
        h.SetCheck;//<--DO THIS LAST
        ring.Lock;
        try
          if ring.SpaceAvailable < (sizeof(h)+h.bytelength) then begin
            Alert('Ring buffer is full, wait until space available then try again.');
            mode := arcValidateRefresh;
            result := false;
            exit;
          end;
          ring.GuaranteePutData(pbyte(@h), sizeof(h));
          ring.GuaranteePutData(data, h.bytelength);
          result := true;
        finally
          ring.unlock;
        end;
      end;
    finally
      LCS(csPrep);
    end;
  finally
//    arcmap.unlock;
  end;

end;


function TArcLogShipper.LogThis_OLD(const blockstart, blocklength: int64;
  const data: Pbyte): boolean;
var
  h: TLogShipHeader;
  ent: TArcMapEntry;
  t,idx1,idx2: int64;
  bUseRing: boolean;
  l: TLock;
  bSuccess: boolean;
  bNoCollision: boolean;
  rememberedlogid: int64;
  posttime: ticker;
  tmSTart: ticker;
begin

  bSuccess := false;
  result := false;
  tmSTart := GetTicker;
  if gettimesince(error_time) < 300000 then
    exit(false);

  idx1 := blockstart shr ARC_ZONE_BLOCK_SHIFT;
  tmSTart := GetTicker;
  while not tecs(csPrep) do begin
    sleep(0);
    if GEtTimeSince(tmStart) > 6000 then begin
      error_time := getticker;
      exit(false);
    end;
  end;
  try
    try
      rememberedlogid := GetNExtLogID(idx1);
    except
      error_time := getticker;
      exit(false);
    end;
  finally
    lcs(csPrep)
  end;
  idx1 := blockstart and ARC_ZONE_BLOCK_BLOCK_ALIGN_MASK;
  idx2 := ((blockstart + blocklength)-1) and ARC_ZONE_BLOCK_BLOCK_ALIGN_MASK;
  idx1 := idx1 shr ARC_ZONE_BLOCK_SHIFT;
  idx2 := idx2 shr ARC_ZONE_BLOCK_SHIFT;

  posttime := Getticker;
  bNoCollision := false;
  repeat
    //Get the preparation lock, only one thread can use the prep state at a time
    if not TECS(csPrep) then begin
      sleep(1);
      continue;
    end;

    //wait for any posts of the current region to complete
    if (bPosting and ((validateidx >=idx1) and (validateidx <= idx2))) then
    begin
      bJammed := true;
      LCS(csPrep);
      sleep(1);
      if gettimesince(posttime) > 8000 then begin

        Fthr.status :='LOG SHIP FAIL!';
        error_time := getticker;
        enabled := false;
        exit;
      end;
    end else
      bNoCollision := true;
  until bNoCollision;

  bJammed := false;
  try
    //decide if we should use the ring buffer
    //we can use the ring buffer if the mode is real-time (will be switched out of real-time when full)
    //OR any time the available space is at least 1/4th the size - we just don't
    //   want to block the virtual disk activity by waiting for the ring to drain
    bUseRing := (Mode = arcRealTime) or (ring.SpaceAvailable > (ring.Size shr 2));

    //if in the appropriate modes (basicalyl any of them)
    if Mode in [arcValidateRefresh, arcRealTime] then begin
      //use the ID we got at the beginning of the function
      ent.logid := rememberedlogid;

      //go through the zone indexes affected by this string of blocks
      for t:= idx1 to idx2 do begin
        //repeat until successful
        while not bSuccess do begin
          //try to get an entry from the arcmap, it will return -3 if it can't lock it
          ent.logid := -3;
          repeat
            ent := TVirtualDisk_Advanced(disk).zonerevs.getentry(t);
            if ent.logid = -3 then
              sleep(100);
          until ent.logid <> -3;

          //try to get the preparation lock, only one thread can use the preparation state at a time
          if TECS(csPrep) then
          try
            //if not in proper mode, (see earlier assignment of bUseRing)
            //then we can still use the ring as long as our index is behind the current validateidx
            //(we've already made a pass)...
            if not bUseRing then begin
              bUseRing := t <  self.validateidx; //allow use of ring only if we've already made a pass at this index

              //...HOWEVER ... if the ring buffer is filling up or has filled up, we'll need to make another
              //validate-refresh round...
              if bUseRing and (ring.SpaceAvailable < (ring.Size shr 2)) then begin
                ValidateRefreshRepeat := true;
                bUseRing := false;
              end;
            end;

            ent.logid := rememberedlogid;
{            arcmap.PutEntry(t, ent);//put entry into the arcmap (NExtID), this flags the zone as needing to be pushed
                                    //we should do this regardless of whether or not we're deferring the post into the ring
                                    //either the ring will post it, or it will be pushed in the validate-refresh process}
            bSuccess := true;
          finally
            LCS(csPrep);
          end;
        end;

      end;
    end;

    if bUseRing then
    begin
      h.init;
      h.blockstart := blockstart;
      h.blocklength := blocklength;
      h.bytelength := blocklength*blocksize;
      h.logid := rememberedlogid;
      h.SetCheck;//<--DO THIS LAST
      ring.Lock;
      try
        if ring.SpaceAvailable < (sizeof(h)+h.bytelength) then begin
          Alert('Ring buffer is full, wait until space available then try again.');
          mode := arcValidateRefresh;
          result := false;
          exit;
        end;
        ring.GuaranteePutData(pbyte(@h), sizeof(h));
        ring.GuaranteePutData(@data[0], h.bytelength);
        result := true;
      finally
        ring.unlock;
      end;
    end;
  finally
    LCS(csPrep);
  end;

end;


function TArcLogShipper.LogThis_Best(logid, startblock, blocklength: int64;
  data: TDynByteArray): boolean;
var
  data2: TDynByteArray;
  fromid: int64;
begin
  {$IFDEF POST_BY_COMMAND}
  c := Tcmd_LogThis.Create;
  c.archive := self.Archive;
  c.LogId := logid;
  c.startblock := startblock;
  c.blocklength := blocklength;
  c.data := data;
  c.shipper := self;
  FBackGroundCommands.AddItem(c);
  result := false;//return false to indicate that this was deferred
  {$ELSE}
  ECS(csClient);
  try
    fromid := GetREmoteLogRev(startblock shr ARC_ZONE_BLOCK_SHIFT);
    //fromid := FClient.//FClient.GetNextLogID(archive, startblock shr ARC_ZONE_BLOCK_SHIFT)-1;
    FClient.NextZoneHint(archive, startblock shr ARC_ZONE_BLOCK_SHIFT);
    result := FCLient.LogThis(archive, fromid, fromid+1, startblock, blocklength, data);
    SetRemoteLogRevCache(validateidx, fromid+1);
    FClient.NextZoneHint(archive, (startblock shr ARC_ZONE_BLOCK_SHIFT)+1);
{$IFDEF VERIFY}
    Debug.Log(FClient.GetZoneStackReport(Archive, startblock shr ARC_ZONE_BLOCK_SHIFT));
    FClient.GetLog(archive, -1, 0, 1, data2);//force back to zero to flush
    FClient.GetLog(archive, logid, startblock, blocklength, data2);
    if not CompareMem(@data[0], @data2[0], lesserof(length(data2), length(data))) then
      Debug.Log('MEMORY MISMATCH!');


{$ENDIF}

  finally
    lcs(csCLient);
  end;
  {$ENDIF}
end;

procedure TArcLogShipper.ModeChanged;
begin
  if FMode = arcValidateRefresh then begin
    validateidx := 0;
  end;
end;


function TArcLogShipper.NeedClient: TRDTPArchiveClient;
begin
  FClients.Lock;
  try
    if FClients.Count = 0 then begin
      result := TRDTPArchiveClient.create('','');
{$IFDEF USE_TCP}
      result.UseTCP := true;
{$ENDIF}
      result.Host := host;
      result.endPoint := endpoint;
      result.timeout := 30000;
    end else begin
      result := FClients[FClients.count-1];
      FClients.Delete(FClients.Count-1);
    end;
  finally
    FClients.Unlock;
  end;
//  result.CheckConnected;
end;

procedure TArcLogShipper.NoNeedClient(cli: TRDTPArchiveClient);
begin
  IF CLI= nil then
    raise ECritical.create('trying to put nil client in pool!');
  if (not cli.Connected) or (cli.errors > 0) then begin
    cli.Free;
    cli := nil;
  end else
    if FClients.IndexOf(cli) < 0 then
      FClients.Add(cli);

end;

function TArcLogShipper.NoneUsingClient(cli: TRDTPArchiveClient): boolean;
var
  t: ni;
begin
  for t:= 0 to high(valbatch) do begin
    if valbatch[t] <> nil then begin
      if valbatch[t].client = cli then
        exit(false);
    end;
  end;
  exit(true);

end;

procedure TArcLogShipper.PutRemoteLogRevInCache(idx, logid: int64);
var
  idx2: int64;
  l: Tlock;
begin
  l := GetLock;
  try
    try
      if not HasRemoveRevCached(idx) then begin
        exit;
      end;
      idx2 := idx-remote_block_start;

      if idx2 < high(remote_rev_cache) then
        remote_rev_cache[idx-remote_block_start] := logid;
    finally
    end;
  finally
    UnlockLock(l);
  end;
end;

procedure TArcLogShipper.SaveARcmapChecksum;
var
  l: TLock;
  bGotLocks: boolean;
  t: int64;
  cs: int64;
  tmSTart: ticker;
begin
  if not defined then exit;
  tmStart := GetTicker;
  try
  bGotLocks := false;
  while not bGotLocks do begin
    l := GetLock;
    try
      if self.enabled then
      try
        if TECS(csClient) then
        try
          if FClient.connected then begin
            FClient.SetStoredParam(self.Archive, 'arcmap_checksum', inttostr(TVirtualDisk_Advanced(disk).zonerevs.CalculateChecksum));
          end;
          bGotLocks := true;//note that we don't want to

          FClient.GetStoredParam(self.ARchive, 'arcmap_checksum', '');

{$IFNDEF USE_NEW_ARC_ZONE_CHECKSUMS}
          t := 0;
          while t < self.ArcZonesInDisk do begin
            cs := TVirtualDisk_Advanced(self.disk).zonerevs.CalculateZoneChecksum(t, ARC_ZONE_CHECKSUM_SIZE);
            if cs <> 0 then
              FClient.SetStoredParam(self.Archive, 'arcmap_checksum_zone_'+inttohex(t, 8), inttostr(cs));
            inc(t, ARC_ZONE_CHECKSUM_SIZE);
          end;
{$ENDIF}

          //get the stored param at the end to make sure that we're doing okay.
          FClient.GetStoredParam(self.ARchive, 'arcmap_checksum', '');

        finally
          LCS(csClient);
        end;

      except
      end;
    finally
      unlocklock(l);
    end;

    if not bGotLocks then begin
      sleep(100);
      if gettimesince(tmStart) > 10000 then begin
        Debug.Log(self, 'Aborting Save of ARcmap checksum due to lock timeout');
        exit;
      end;
    end;

  end;
  except
  end;
end;

procedure TArcLogShipper.SetArchive(value: string);
begin
  FArchive := Value;
  if canactivate then
    Activate;

end;

procedure TArcLogShipper.SetEndPOint(value: string);
begin
  FClient.endpoint := value;
//  FQuickClient.endpoint := value;
  if canactivate then
    Activate;
end;

procedure TArcLogShipper.SetHost(value: string);
begin
  FClient.Host := value;
//  FQuickClient.Host := value;
  if canactivate then
    Activate;
end;

procedure TArcLogShipper.SetMode(const Value: TArcMode);
var
  l: TLock;
begin
  l := GetLock;
  try
    if value <> FMode then begin
      FMode := Value;
      if assigned(FThr) then
        FThr.Status := 'Mode set to '+inttostr(ord(value));
    end;
    ModeChanged;
  finally
    UnlockLock(l);
  end;

end;

procedure TArcLogShipper.SetRemoteLogRevCache(idx, logid: int64);
var
  idx2: int64;
  l: Tlock;
begin
  l := GetLock;
  try
    if HasRemoveRevCached(idx) then begin
      idx2 := idx-remote_block_start;
      if idx2 < high(remote_rev_cache) then
        remote_rev_cache[idx2] := logid;
    end;
  finally
    UnlockLock(l);
  end;
end;

procedure TArcLogShipper.SetRingFileName(value: string);
begin
  StopThread;
{$IFDEF USE_RING_FILE}
  ring.FileName := value;
{$ELSE}
  ring.Size := 4*BILLION;
{$ENDIF}
  arcmap_validated := false;
  StartThread;
end;


function TArcLogShipper.SearchValBatchForZone(zidx: int64; bCheckOnlyTransLogThis: boolean): TArcTransactionalCommand;
var
  tt: ni;
begin
  for tt := high(valbatch) downto 0 do begin
    if valbatch[tt] <> nil then begin
      if (valbatch[tt].zidx) = zidx then begin
        if valbatch[tt].WatcherCount = 0 then begin
          if not bCheckOnlyTransLogThis then
            exit(valbatch[tt])
          else
            if valbatch[tt] is Ttrans_LogThis then begin
              exit(valbatch[tt]);
            end;
        end;
      end;
    end;
  end;
  exit(nil);
end;

procedure TArcLogShipper.ShipLogsToServer;
var
  c: Ttrans_LogThis;
  lastc: TARcTransactionalCommand;
  dyn: TDynByteArray;
  h: TLogShipHEader;
  ent: TArcMapEntry;
  muffidx, idx2: ni;
  bCombined: boolean;
  iAvail: ni;
  t: ni;
begin
  FinishValBAtches(false);

  if CountUniqueClients(false) >= (MAX_UNIQUE_CLIENTS-RESERVE_UNIQUE_CLIENTS) then
     exit;


  //count how many slots are open
  iAvail := 0;
  for t := low(valbatch) to high(valbatch) do begin
    if valbatch[t] = nil then
        inc(iAVail);
  end;

  if iAvail = 0 then begin
    sleep(20);
    exit;
  end;



  if ring.WaitForDataSignal(5000) then begin
//    locli := NeedClient;
    try
      muffidx := 0;
      lastc := nil;
      try
        while ring.WaitForDataSignal(1) do begin

          if not ring.TryLock then begin
            tmLAstDiskBusyTime := getticker;
            exit;
          end;
          try
            if ring.GuaranteeGetData(pbyte(@h), sizeof(h)) = 0 then
              exit;
            if not h.IsCheckValid then
              raise ECritical.create('RingFile has invalid header checksum.');

            setlength(dyn, h.bytelength);
      //      Debug.ConsoleLog('Will get '+commaize(h.bytelength)+' from ring buffer');
            ring.GuaranteeGetData(@dyn[0], h.bytelength);
            if h.logid < GetREmoteLogRev(h.blockstart shr ARC_ZONE_BLOCK_SHIFT) then
              continue;
            lastc := SearchValBAtchForZone(h.blockstart shr ARC_ZONE_BLOCK_SHIFT, false);
            bCombined := false;
            if lastc <> nil then begin
{$IFDEF COMBINE_RING_LOGS}
              //We already have zone, try to combine
              if lastc is Ttrans_logthis then begin
                for t:= 0 to high(valbatch) do begin
                  if (valbatch[t] <> nil) and (valbatch[t] is Ttrans_LogThis) then begin
                    bCombined := TTrans_LogThis(valbatch[t]).TryCombine(h, dyn);
                    if bCombined then break;
                  end;
                end;
              end;
{$ELSE}
            bCombined := false;
{$ENDIF}
            end;
            //if the combine failed, we still need to share the client from the previous zone, but
            //submit as a separate command
            if not bCombined then begin
              c := Ttrans_LogThis.Create;
              c.this_logid := h.logid;
              c.blocklength := h.blocklength;
              c.startblock := h.blockstart;
              c.shipper := self;
              c.ARchive := self.archive;
              c.data := dyn;
              //c.start;<<----------DON'T START YET!
              newbatch[muffidx] := c;
              inc(muffidx);

              if lastc <> nil then begin
                c.AddDependency(lastc);
                c.client := lastc.client;
                c.known_fromid_at_creation := h.logid - 1;
                AddVAlBatch(c);//put in val batch.. will be started later
              end else begin
                c.known_fromid_at_creation := h.logid - 1;
                c.Client := NEedClient;
                AddVAlBatch(c);//put in val batch.. will be started later
                if CountUniqueClients(false) >= MAX_UNIQUE_CLIENTS then
                  break;
              end;


              if muffidx >= iAvail then
                break;


              lastc := c;
            end else begin
                //
            end;
            FThr.StepCount := ring.Size;
            FThr.Step := ring.SpaceAvailable;
          finally
            ring.Unlock;
          end;
        end;
      finally
        //start the new stuff
        for idx2 := 0 to muffidx-1 do begin
          if newbatch[idx2].client = nil then begin
            newbatch[idx2].client := NeedClient;
          end;
          newbatch[idx2].start;
        end;
      end;
    finally
    end;
  end;

end;

procedure TArcLogShipper.Shovel;
var
  tm: ticker;
  bt: ticker;
begin
  FThr.IterationComplete;
  if not enabled then
    exit;

  if bJammed then sleep(1000);

  if pauseForFetch then begin
    sleep(1000);
    exit;
  end;



  //make sure that we can get idx 0
  GetREmoteLogRev(0);

  if not ArcMapValid then
    exit;

  try
//    mode := arcRealTime;
    if enabled then begin
      case mode of
        arcValidateRefresh: begin
          bt := GEtTimeSInce(tmLastDiskBusyTime);
          if bt > 1000 then begin
            if not TAbstractVirtualDisk_Advanced(disk).hint_requests_waiting then begin
              FThr.status := 'VR '+validateidx.tohexstring+' prt '+Commaize(ring.DAtaAvailable)+' bytes behind. Since-Busy='+bt.tostring;
              ContinueLogValidateRefresh;
            end
            else begin
              tmLastDiskBusyTime := GetTicker;
            end;
          end else begin
            FThr.status := 'VR '+validateidx.tohexstring+' prt '+Commaize(ring.DAtaAvailable)+' bytes behind. Since-Busy='+bt.tostring;
//            sleep(500);
          end;
          if (bt > 1000) (*and (((GetTicker div 2000) mod 2)=0)*) then begin
            tm := GetTicker;
            while ring.DataAvailable > 0 do begin
              FThr.status := 'VR '+validateidx.tohexstring+' prt '+Commaize(ring.DAtaAvailable)+' bytes behind. Since-Busy='+bt.tostring;
              ShipLogsToServer;
              if gettimesince(tm) > 2000 then
                break;
            end;
          end else begin
            sleep(greaterof((500-bt)+1,1));
          end;
        end;
        arcRealTime: begin
          bt := GEtTimeSInce(tmLastDiskBusyTime);
          if bt > 500 then begin
            FThr.status := 'real-time '+Commaize(ring.DAtaAvailable)+' bytes behind.';
            ShipLogsToServer;
          end else begin
            sleep((500-bt)+1);
          end;
        end;
      end;
    end;
  except
    on E: Exception do begin
      FThr.status := e.message;
      FMode := arcValidateRefresh;
      FClient.Disconnect;
    end;
  end;


end;

procedure TArcLogShipper.StartThread;
begin
  StopThread;
  Fthr := TPM.NeedThread<TArcLogShippingthread>(nil);
  FThr.Loop := true;
  Fthr.ColdRunInterval := 5;
  FThr.shipper := self;
  FThr.RunHot := true;
  FThr.Start;
end;

procedure TArcLogShipper.StopThread;
begin
  if assigned(FThr) then begin
    Fthr.Stop;
    CancelValBatches;
    FThr.WaitForFinish;
    TPM.NoNeedThread(FThr);
  end;
  FThr := nil;

end;

{ TLogShipHeader }

function TLogShipHeader.ComputeCheck: int64;
begin
  result := blockstart xor bytelength xor blocklength xor int64($5555555555555555);
end;

procedure TLogShipHeader.Init;
begin
  //
end;

function TLogShipHeader.IsCheckValid: boolean;
begin
  result := (headercheck = ComputeCheck) and (bytelength = (blocklength * BLOCKSIZE));
end;

procedure TLogShipHeader.SetCheck;
begin
  headerCheck := computeCheck;
end;

{ TArcLogShippingthread }

procedure TArcLogShippingthread.DoExecute;
begin
  inherited;
  {$IFDEF DISABLE_SHIPPER}
  runhot := false;
  exit;
  {$ENDIF}
  try
    if shipper.enabled then begin
      runhot := true;
      shipper.shovel
    end
    else
      runhot := false;
  except
    on e: exception do begin
      status := e.message;
      sleep(1000);
    end;
  end;


end;
{ Tcmd_LogThis }


function Ttrans_LogThis.CanCombine(h: TLogShipHeader;
  indata: TDynByteArray): boolean;
begin
  if (started) then
    exit(false);
  if (h.blockstart shr ARC_ZONE_BLOCK_SHIFT) <> (startblock shr ARC_ZONE_BLOCK_SHIFT) then
    exit(false);

  if h.blockstart > (self.startblock + self.blocklength) then
    exit(false);

  if self.startblock > (h.blockstart+h.blocklength) then
    exit(false);



  exit(true);
end;

function Ttrans_LogThis.DoCombine(h: TLogShipHeader;
  indata: TDynByteArray): boolean;
var
  newdata: TDynByteArray;
  newstart, newend: int64;
  blockoff: int64;
  byteoff: int64;
  newblocklen: int64;
  newbytelen: int64;
begin

  self.this_logid := greaterof(this_logid, h.logid);
  newstart := lesserof(h.blockstart, self.Startblock);
  newend := greaterof(h.blockstart+h.blocklength, self.startblock+self.blocklength);

  newblocklen := newend-newstart;
  newbytelen := newblocklen shl BLOCKSHIFT;


  setlength(newdata, newbytelen);

  //Debug.Log('COMBINE: size='+newbytelen.tostring+' startblock='+newstart.tostring);

  //existing data into new array
  blockoff := self.startblock-newstart;{ok}
  byteoff := blockoff shl BLOCKSHIFT;{ok}
  movemem32(@newdata[byteoff], @data[0], length(data));{ok}

  //new data into new array
  blockoff := h.blockstart-newstart;{ok}
  byteoff := blockoff shl BLOCKSHIFT;{ok}
  movemem32(@newdata[byteoff], @indata[0], length(indata));{ok}

  self.startblock := newstart;
  self.blocklength := newblocklen;
  self.data := newdata;

  exit(true);
end;

procedure Ttrans_LogThis.DoCommit;
var
  zidx: int64;
begin
  inherited;
  //upgrade the local entry to reflect success
  zidx := Self.startblock shr ARC_ZONE_BLOCK_SHIFT;
  shipper.SetRemoteLogRevCache(zidx, this_logid);

end;

procedure Ttrans_LogThis.DoRollback;
var
  zidx: int64;
begin
  //make sure the local logid matches the remote one
  zidx := startblock shr ARC_ZONE_BLOCK_SHIFT;
  //make sure the local cache matches remote
  fromid := client.GetLogRev(archive, zidx);
  shipper.PutRemoteLogRevInCache(zidx, fromid);
end;

procedure Ttrans_LogThis.GatherStartSTate;
begin
  inherited;

  if tries = 0 then begin
    fromid := known_fromid_at_creation;
    //this_logid := shipper.GetNextLogId(startblock shr ARC_ZONE_BLOCK_SHIFT);
  end
  else
  begin
    fromid := shipper.GetREmoteLogRev(startblock shr ARC_ZONE_BLOCK_SHIFT);
    //this_logid := shipper.GetNextLogId(startblock shr ARC_ZONE_BLOCK_SHIFT);
  end;

end;

procedure Ttrans_LogThis.Init;
begin
  inherited;
  icon := @cmd_icon_quickship;
end;

procedure Ttrans_LogThis.InitExpense;
begin
  inherited;
  CPUExpense := 0;
end;

function Ttrans_LogThis.LockState: Boolean;
begin
  ecs(shipper.csPrep);
  exit(true);
end;



function Ttrans_LogThis.Perform;
begin
  inherited;
  StepCount := 4;
  Step := 1;
  TriesBeforeFail := 20;
  RetryOnPerformException := true;
  success := false;
  result := trCollision;
  if fromid < this_logid then begin //if we didn't already commit this one, somehow
    if IsCancelled then begin
      result := trFailed;
      exit;
    end;

    client.NExtZoneHint(archive, startblock shr ARC_ZONE_BLOCK_SHIFT);
    Step := 2;
    client.LogThis_Async(archive, fromid, this_logid, startblock, blocklength, data);
    Step := 3;
    success := client.LogThis_Response;
    Step := 4;
    //LOGTHIS RETURNS FALSE IF LOGID CHANGED!
    if success then
      result := trPerformed
    else
      result := trCollision;
  end else
    result := trPerformed;//<---it was already performed
end;

function Ttrans_LogThis.ReEvalAgainstStartState: TTransactionResult;
begin
  if success or (CResult = trPerformed) then
    exit(trNoCollision)
  else
    exit(trCollision);
end;

procedure Ttrans_LogThis.SetStartBlock(const Value: int64);
begin
  FStartBlock := Value;
  zidx := value shr ARC_ZONE_BLOCK_SHIFT;
end;

function Ttrans_LogThis.TryCombine(h: TLogShipHeader;
  indata: TDynByteArray): boolean;
begin
  Lock;
  try
    result := cancombine(h, indata);
    if result then
      docombine(h, indata);
  finally
    Unlock;
  end;

end;

procedure Ttrans_LogThis.UnlockState;
begin
  //inherited;
  Lcs(shipper.csPrep);
end;

{ Tcmd_ContinueVAlidateLogRefresh }

procedure Tcmd_ContinueVAlidateLogRefresh.DoCommit;
var
  res, rrc, rem, loc: int64;

begin
  inherited;

  if pass then//we don't need to do anything at all if everythign was already peachy
    exit;

{$IFDEF DONT_COMMIT_CHECKSUM_PASSES}
  if checksumpass then
    this_logid := fromid;
{$ENDIF}

  //upgrade the local entry to reflect success
  shipper.SetRemoteLogRevCache(zidx, this_logid);
  //status :='Commit ['+loc.tostring+']['+rrc.tostring+'->'+res.tostring+'][??]';
  //shipper.arcmap.PutEntry(zidx, local_entry);NO! You want to update the remove cache, but not change the local rev number... that should be changed on disk flush
end;

procedure Tcmd_ContinueVAlidateLogRefresh.DoRollback;
begin
  inherited;
  //make sure the local cache matches remote
  fromid := client.GetLogRev(archive, zidx);
  shipper.PutRemoteLogRevInCache(zidx, fromid);

end;

procedure Tcmd_ContinueVAlidateLogRefresh.GatherStartSTate;
begin
  inherited;
  original_local := TVirtualDisk_Advanced(shipper.disk).zonerevs.GetEntry(zidx).logid;
  fromid := shipper.GetREmoteLogRev(zidx);

end;

procedure Tcmd_ContinueVAlidateLogRefresh.Init;
begin
  inherited;
  icon := @cmd_icon_bigship;
end;

procedure Tcmd_ContinueVAlidateLogRefresh.InitExpense;
begin
  inherited;
  CPUExpense := 1;
  NetworkExpense := 1/32;
end;

function Tcmd_ContinueVAlidateLogRefresh.LockState: Boolean;
begin
  ecs(shipper.csPrep);
  exit(true);

end;

function Tcmd_ContinueVAlidateLogRefresh.Perform: TTransactionResult;
var
  ll,l: TLock;
  startblock: int64;
  startbyte: int64;
  disk: TVirtualDisk_Advanced;

  bGotDisk: boolean;
  bIsProvisioned: boolean;

  bPostSuccess: boolean;
  iSum1,iSum2,iXor1,iXor2: int64;
  bExitCS: boolean;
  s: string;
{$IFDEF LOCAL_TEMP}
    temp: TDYnByteArray;
{$ENDIF}
begin
//  Debug.Log(self, 'Perform');
  StepCount := 9;
  Step := 1;
  pass := false;
  TriesBeforeFail := 20;
  RetryOnPerformException := true;
  success := false;
  if ((fromid <> original_local) or (original_local=0)) then begin
//    this_logid := shipper.GetNextLogId(zidx);
    Step := 2;
    //fetch the data from the disk
    startblock := zidx * ARC_ZONE_SIZE_IN_BLOCKS;
//    if this_logid <= 0 then
//      status := 'refresh '+inttohex(zidx, 1)+' rev_local=0 (virgin)'
//    else begin
//      s := 'refresh zidx='+inttohex(zidx, 1)+' remote='+inttostr(fromid)+' < local='+inttostr(this_logid);
//      status := s;
//    end;

    //read the data from the disk
    setlength(temp, ARC_ZONE_SIZE_IN_BYTES);
//    setlength(temp, BIG_BLOCK_SIZE_IN_BYTES);
    disk := self.disk as TVirtualDisk_Advanced;
    bGotDisk := false;
    bIsProvisioned := false;
    while disk.hint_requests_waiting do begin
      status := 'Waiting because disk is active...';
      sleep(100*MAX_UNIQUE_CLIENTS);
    end;
    disk.backgroundlock.LockWrite;
    try
    if disk.hint_requests_waiting then begin
      exit(trCollision);
    end;
    if (disk.TryGetLock(ll,1000, 10)) then
    try
      if disk.hint_requests_waiting then begin
        exit(trCollision);
      end;
//      Debug.Log(self, 'Got Disk');
      bGotDisk := true;
      bIsProvisioned := disk.vat.table[startblock shr BIG_BLOCK_BLOCK_SHIFT].FileCount > 0;
      if bIsProvisioned then begin
        Debug.Log(self, 'Is Provisioned');
        disk.DisableArcRestore := true;
        try
          Step := 3;
          disk.disablecallupstats := true;
          Debug.Log(self, 'Reading for Push');
          disk.GuaranteeReadBlocks(startblock, ARC_ZONE_SIZE_IN_BLOCKS, @temp[0]);
          Debug.Log(self, 'Read');
          this_logid := disk.zonerevs.GetEntry(zidx).logid;
          disk.disablecallupstats := false;
          Step := 4;
        finally
          disk.DisableArcRestore := false;
        end;
      end;
    finally
      disk.UnlockLock(ll);
    end else
      debug.log(self, 'Did not get disk lock probably because '+inttostr(disk.lockowner)+' might have it.');
    finally
      disk.backgroundlock.UnLockWrite;
    end;

    result := trCollision;

    if bGotDisk then  //we get the data... WE ARE NO LONGER UNDER LOCK
    begin
      if bIsProvisioned then
      begin
        //log the data
        {ts}Step := 5;
        Debug.Log(self, 'Requesting Checksum');
        client.GetZoneChecksum_Async(archive, startblock shr ARC_ZONE_BLOCK_SHIFT);
        Debug.Log(self, 'Calculating local checksum');
        CalculateChecksum(@temp[0], ARC_ZONE_SIZE_IN_BYTES, iSum2, iXor2);
        Step := 6;
        Debug.Log(self, 'Waiting for Remote Checksum');
        client.GetZoneChecksum_Response(iSum1, iXor1);
        Step := 7;

        if (iSum1=iSum2) and (iXor1=iXor2) then begin
          Debug.Log(self, 'Pushing checksum pass');

          //give the back-end a little warning so it can prepare buffers while the data is uploading
          client.NextZoneHint(archive, startblock shr ARC_ZONE_BLOCK_SHIFT);
          //just update the ID by sending a 0-length revision
          status := 'Checksum Pass ' +zidx.tohexstring;
          {$IFDEF DONT_COMMIT_CHECKSUM_PASSES}
          if FromID = 0 then begin
          {$ENDIF}
            client.Logthis_Async(archive, fromid, this_logid, startblock, 0, nil);
            STep := 8;
            success := client.LogThis_Response;
          {$IFDEF DONT_COMMIT_CHECKSUM_PASSES}
          end else begin
            STep := 8;
            success := true;
            checksumPass := true;
          end;
          {$ENDIF}
        end else begin
          Debug.Log(self, 'Checksums differ');
          //give the back-end a little warning so it can prepare buffers while the data is uploading
          client.NextZoneHint(archive, startblock shr ARC_ZONE_BLOCK_SHIFT);
          //send revised data along with original logid
          status := 'PUSH  ' +zidx.tohexstring+' '+fromid.tostring+'->'+this_logid.tostring;
          Debug.Log(status);
          client.LogThis_ASync(archive, fromid, this_logid, startblock, ARC_ZONE_SIZE_IN_BLOCKS, temp);
          Step := 8;
          success := client.LogThis_Response;
        end;

        //if either path above sucessful then the operation was successful
        if success then begin
          Debug.Log(self, 'Success');
          result := trNoCollision;
          exit;
        end else begin
          Debug.Log(self, 'fail');
        end;
      end
      else
      begin
        //remember that since this isn't provisioned, to skip over it in the future (quickly)
        success := true;
        this_logid := -1;
        result := trNoCollision;//<---success
        status := 'NoProv ' +zidx.tohexstring;

      end;
    //ELSE IF not bGotDisk
    end
    else begin
      //couldn't get disk
      success := false;
      result := trCollision;//<---not successful, collision, will retry
      exit;
    end;

  end else
  //NOT --> if ((rev_remote <> rev_local) or (rev_local = 0)) then begin
  begin
    //This is the state of things when there's nothing to do
    pass := true;
    Step := StepCount;
    status := 'Pass '+zidx.tohexstring;
    success := true;
    result := trNoCollision;
    success := true;
  end;
end;


function Tcmd_ContinueVAlidateLogRefresh.ReEvalAgainstStartState: TTransactionResult;
begin
  if shipper.getremotelogrev(zidx) <> fromid then //<<--- this probably isn't necessary because commands are
    exit(trCollision);                            //      declared as dependent on each other in the command processor

  if success then
    exit(trNoCollision)
  else
    exit(trCollision);
end;

procedure Tcmd_ContinueVAlidateLogRefresh.UnlockState;
begin
  inherited;
  Lcs(shipper.csPrep);
end;

end.
