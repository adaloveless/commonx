unit ArcMap;

interface

uses
  typex, sharedobject, queuestream, helpers_stream, sysutils, classes, debug, virtualdiskconstants, betterobject;

type
  TArcMapEntry = packed record
  private
    _logid: int64;
    check: int64;
    procedure SetLogID(const Value: int64);
    function GetLogID: int64;
  public
    property logid:int64 read GetLogID write SetLogID;

  end;

  TlocalfileStream = TUnbufferedFileStream;

  TArcMap = class(TSharedObject)
  private
    FFileName: string;
    procedure SetFileName(value: string);
    function GetEntryAddress(idx: int64): int64;
  //arcvat is a list of the fileids and final blocks associated with each ARC zone.
  protected
    FCachedNextID: int64;
    fs: TUnbufferedFileStream;
  public
    shutdownban: boolean;
    procedure Init;override;//
    procedure Detach;override;
    function GetEntry(idx: int64): TArcMapEntry; //
    procedure PutEntry(idx: int64; ent: TArcMApEntry; bIKnowWhatImDoing: boolean);
    property FileName: string read FFileName write SetFileName;
    function CalculateChecksum: int64;
    function CalculateZoneChecksum(zonebase: int64; zonesize: int64): int64;
    function CalculateZoneChecksum2(zonebase: int64; zonesize: int64): int64;

    procedure Reset;
    procedure ResetZone(zonebase, zonelength: int64; bProvisioned: boolean);//
    procedure IncrementZoneRev(revidx: int64);
    procedure IncrementZoneRevForStartingBlock(block: int64);
  end;

implementation

{ TArcMap }

function TArcMap.CalculateChecksum: int64;
var
  l: int64;
begin
  lock;
  try
    result := Stream_CalculateChecksum(fs);

  finally
    unlock;
  end;

end;

function TArcMap.CalculateZoneChecksum(zonebase, zonesize: int64): int64;
var
  l: int64;
begin
  lock;
  try
    result := Stream_CalculateChecksum(fs, zonebase*sizeof(TArcMapEntry), zonesize*sizeof(TArcMapEntry));
    debug.log('zone checksum '+inttohex(zonebase,8)+'='+result.tohexstring);
  finally
    unlock;
  end;
end;

function TArcMap.CalculateZoneChecksum2(zonebase, zonesize: int64): int64;
var
  r: TArcMapEntry;
begin
  Lock;
  try
    result := 0;
    while zonesize > 0 do begin
      r := GetEntry(zonebase);
      result := result + r.logid;
      dec(zonesize);
      inc(zonebase);
    end;
  finally
    Unlock;
  end;
end;

procedure TArcMap.Detach;
begin
  inherited;
  fs.Free;
  fs := nil;

end;

function TArcMap.GetEntry(idx: int64): TArcMapEntry;
var
  addr: int64;
  bCreate: boolean;
  r: TArcMapEntry;
begin
  Lock;
  try
    addr := GetentryAddress(idx);
    bCreate := fs.Size < (addr+sizeof(result));
    if bCreate then begin
      stream_Grow(fs, (((addr+sizeof(result)) div MEGA)+1)*(MEGA));
    end;
    fs.Seek(addr, soBeginning);
    stream_GuaranteeRead(fs, pbyte(@r), sizeof(r));
//    if r.logid < -1 then begin
//      Debug.Log('Logid < -1 was stored in arcmap! forcing refresh');
//      bCreate := true;
//    end;
    result := r;
    if bCreate then begin
      result.logid := 0;
      PutEntry(idx, result, true);
    end;
  finally
    Unlock;
  end;

end;

function TArcMap.GetEntryAddress(idx: int64): int64;
begin
  result := sizeof(TArcMapEntry) * idx;
end;



procedure TArcMap.IncrementZoneRev(revidx: int64);
var
  ent: TArcMapEntry;
begin
  lock;
  try
    ent := GetEntry(revidx);
    ent.logid := ent.logid + 1;
    PutEntry(revidx, ent, true);
  finally
    unlock;
  end;
end;

procedure TArcMap.IncrementZoneRevForStartingBlock(block: int64);
begin
  IncrementZoneRev(block shr ARC_ZONE_BLOCK_SHIFT);
end;

procedure TArcMap.Init;
begin
  inherited;
  FCachedNextID := -1;

end;

procedure TArcMap.PutEntry(idx: int64; ent: TArcMApEntry; bIKnowWhatImDoing: boolean);
var
  addr: int64;
  oldent: TArcMapEntry;
begin
  if shutdownban then
    Debug.Log('shutdown! putEntry is banned!');
  if (idx >= MAX_ZONES) or (idx < 0) then
    raise ECritical.create('zone index not valid!'+idx.tostring+' '+idx.tohexstring);
  Lock;
  try
    oldEnt := GetEntry(idx);
    addr := GetEntryAddress(idx);
    stream_Grow(fs, addr);
    fs.Seek(addr, soBeginning);
    if (oldEnt.logid > ent.logid) and (ent.logid >=0) then
        Debug.Log('Cannot decrease log id!');
    if (ent.logid > oldent.logid) or (ent.logid < 0) then begin
      //Debug.Log('Idx '+idx.tohexstring+' changes '+oldent.logid.tostring+'->'+ent.logid.tostring);
      stream_GuaranteeWrite(fs, pbyte(@ent), sizeof(ent));
    end;
    //fs.CheckFlush;
  finally
    unlock;
  end;

end;

procedure TArcMap.Reset;
var
  sFile: string;
begin
  Lock;
  try
    sFile := fs.FileName;
    fs.free;
    fs := nil;
    DeleteFile(sFile);
    fs := TUnbufferedFileStream.Create(sFile, fmCreate, fmShareExclusive);

  finally
    Unlock;
  end;
end;

procedure TArcMap.ResetZone(zonebase: int64; zonelength: int64; bProvisioned: boolean);
var
  sFile: string;
  arcmap: TArcMapEntry;
begin
  Lock;
  try
    sFile := fs.FileName;
    fs.GrowFile(zonebase*SizeOf(TArcMapEntry));
    if bProvisioned then
      arcmap.SetLogID(1)
    else
      arcmap.SetLogID(0);
    fs.seek(zonebase*sizeof(TArcmapEntry), soBeginning);
    Stream_GuaranteeWrite(fs, @arcmap, sizeof(arcmap));


  finally
    Unlock;
  end;
end;

procedure TArcMap.SetFileName(value: string);
begin
  fFileName := value;
  if (value <> FFileName) or (fs=nil) then begin
    fs.free;
    fs := nil;
    if not fileexists(fFileName) then begin
      forcedirectories(extractfilepath(FFileName));
      fs := TLocalFileStream.create(FFileName, fmCreate,0,0);

    end else begin
      fs := TLocalFileStream.create(FFileName, fmOpenReadWRite+fmShareExclusive,0,0);
      if fs.size > sizeof(TArcMapEntry)*MAX_ZONES then
        fs.Size := sizeof(TArcMapEntry)*MAX_ZONES;
    end;
  end;
end;

{ TArcMapEntry }

function TArcMapEntry.GetLogID: int64;
begin
  if check = (int64(_logid) xor int64($FFFFFFFFFFFFFFFF)) then
    result := _logid
  else
    result := 0;
end;

procedure TArcMapEntry.SetLogID(const Value: int64);
begin
  _logid := value;
  check := value xor int64($FFFFFFFFFFFFFFFF);
end;

end.
