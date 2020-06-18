unit PerfMessageServer;

interface

uses
  betterobject, PerfMessage, typex, orderlyinit, PeriodicEvents, numbers,
  IdGlobal,IdBaseComponent, IdComponent, IdUDPBase, idudpserver, systemx,
  tickcount, idsockethandle, herro, debug, sysutils;

type
  TPerfSnapshot = record
    tick: ticker;
    nodecount: ni;
    data: array[0..65535] of TPerfNode;
  end;

  TDeltaResult = record
    elapsedtime: ticker;
    data: TArray<TPerfNode>;
    raw: TArray<TPerfNode>;
    desc: TArray<TPerfDescriptor>;
  end;

  TRealmData = record
    lastseen: ticker;
    origin: string;
    eport: ni;
    currentbuffer: ni;
    data : array[0..1] of TPerfSnapshot;
    desc: array[0..65535] of TPerfDescriptor;
    function GetDeltas: TDeltaResult;
    procedure ApplyNewData(Adata: TIdBytes);
  end;
  PRealmData = ^TRealmData;

  TAllDeltaResults = record
    realms: TArray<TRealmData>;
    deltas: Tarray<TDeltaResult>;
    function GetMergedDeltas: TDeltaResult;
  end;

  TPerfMessageServer = class(TSharedObject)
  private
    realms: TArray<TRealmData>;
    udpS: TIdUdpServer;
    function NewRealm: PRealmData;
    function FindRealm(sHost: string; ePort: ni): PRealmData;



    procedure InitServer;
    procedure CleanupServer;
    procedure ScrubRealms;
    procedure udps_OnUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
  public
    function GetDeltas: TAllDeltaResults;
    procedure Shutdown;
    constructor Create; override;
    procedure Detach; override;
  end;

implementation

{ TPerfMessageServer }

procedure TPerfMessageServer.CleanupServer;
begin
  udps.Active := false;
  udps.Free;
  udps := nil;
end;

constructor TPerfMessageServer.Create;
begin
  inherited;
 InitServer;
end;

procedure TPerfMessageServer.Detach;
begin
  if detached then exit;
  Shutdown;

  inherited;
end;

function TPerfMessageServer.FindRealm(sHost: string; ePort: ni): PRealmData;
begin
  Lock;
  try
    for var t:= 0 to High(realms) do begin
      if (realms[t].origin = sHost)
      and (realms[t].eport = ePort) then begin
        exit(@realms[t]);
      end;
    end;
    exit(nil);

  finally
    Unlock;
  end;

end;

function TPerfMessageServer.GetDeltas: TAllDeltaResults;
begin
  Lock;
  try
    result.realms := realms;
    setlength(result.deltas, length(realms));
    for var t := 0 to high(realms) do
      result.deltas[t] := realms[t].GetDeltas;
  finally
    Unlock;
  end;

end;


procedure TPerfMessageServer.InitServer;
begin
  if udps <> nil then exit;
  udps := TIdUDPServer.create(nil);
  udps.ThreadedEvent := true;
  udps.OnUDPRead := self.udps_onUDPRead;
  udps.DefaultPort := 144;
  udps.Active := true;
  herro.RegisterLocalSkill('PerfNodeViewer',1,'144','UDP');
end;

function TPerfMessageServer.NewRealm: PRealmData;
begin
  Lock;
  try
    setlength(realms, length(realms)+1);
    result := @realms[high(realms)];
  finally
    Unlock;
  end;
end;

procedure TPerfMessageServer.ScrubRealms;
begin
  lock;
  try
    for var t:= 0 to high(realms) do begin
      if gettimesince(realms[t].lastseen) > 8000 then begin
        for var x := t to high(realms)-1 do begin
          realms[x] := realms[x+1];
        end;
        setlength(realms, length(realms)-1);
        exit;//<<------------exit early because T range is now bad
      end;
    end;
  finally
    unlock;
  end;
end;

procedure TPerfMessageServer.Shutdown;
begin
  CleanupServer;
end;

procedure TPerfMessageServer.udps_OnUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
begin
  Lock;
  try
    //get the header
    var r := FindRealm(ABinding.PeerIP, ABinding.Peerport);
    if r = nil then begin
      r := NewRealm;
      r.origin := ABinding.PeerIP;
      r.eport := ABinding.peerport;
    end;
    r.lastseen := getticker;
    r.ApplyNewData(AData);
    ScrubRealms;
  finally
    Unlock;
  end;


end;


procedure oinit;
begin
//
end;

procedure ofinal;
begin
//
end;

{ TRealmData }

procedure TRealmData.ApplyNewData(Adata: TIdBytes);
var
  hed: TPerfMessageHeader;
begin
    if length(adata) < sizeof(hed) then
      exit;

    movemem32(@hed, @adata[0], sizeof(hed));
    if hed.startnode > high(desc) then exit;
    if hed.startnode <0 then exit;
    if hed.nodesinmessage= 0 then
      exit;

    case hed.mtyp of
      MT_PERFORMANCE_DATA: begin
        var writenode := (currentbuffer + 1) and 1;

        var szToCopy := hed.nodesinmessage * SizeOf(TPerfNode);
        var copystart := sizeof(TPerfMessageHEader);
        movemem32(@self.data[writenode].data[hed.startnode],@adata[copystart],szToCopy);
        self.data[writenode].nodecount := hed.nodesinmessage+hed.startnode;
        self.data[writenode].tick := hed.ticker;
        currentbuffer := writenode;

        MMQ.QuickBroadcast('NewStats');
      end;
      MT_DESCRIPTORS: begin
        if hed.startnode > high(desc) then exit;
        if hed.startnode <0 then exit;

        var tocopy := lesserof(length(desc)-hed.startnode, hed.nodesinmessage);
        var szToCopy :=tocopy * sizeof(TPerfDescriptor);
        var copystart := sizeof(TPerfMessageHEader);

        movemem32(@self.desc[hed.startnode],@adata[copystart],szToCopy);
      end;
    end;

end;

function TRealmData.GetDeltas: TDeltaResult;
begin
  var idxNow := currentbuffer;
  var idxThen := (currentbuffer + 1) and 1;

  result.elapsedtime := data[idxNow].tick-data[idxThen].tick;
  var len := data[idxNow].nodecount;
  setlength(result.data, len);
  setlength(result.desc, len);
  setlength(result.raw, len);

  for var t := 0 to lesserof(data[idxNow].nodecount, data[idxThen].nodecount)-1 do begin
    result.data[t].id := data[idxNow].data[t].id;
    result.data[t].r := data[idxNow].data[t].r - data[idxThen].data[t].r;
    result.data[t].w := data[idxNow].data[t].w - data[idxThen].data[t].w;
    result.data[t].typ := data[idxNow].data[t].typ;
    result.desc[t] := self.desc[t];
    result.data[t].busyR := data[idxNow].data[t].busyR;
    result.data[t].busyW := data[idxNow].data[t].busyW;

    result.raw[t].r := data[idxNow].data[t].r;
    result.raw[t].w := data[idxNow].data[t].w;
    result.desc[t] := self.desc[t];
    result.data[t].typ := data[idxNow].data[t].typ;
    result.data[t].busyR := data[idxNow].data[t].busyR;
    result.data[t].busyW := data[idxNow].data[t].busyW;


  end;


end;


{ TAllDeltaResults }

function TAllDeltaResults.GetMergedDeltas: TDeltaResult;
begin
  var len := 0;
  for var t:= 0 to high(deltas) do begin
    inc(len, lesserof(length(deltas[t].data), length(deltas[t].desc)));
  end;

  setlength(result.data, len);
  setlength(result.desc, len);
  setlength(result.raw, len);
  var realmbaseid := 0;
  var taridx:= 0;
  for var r := 0 to high(deltas) do begin
    result.elapsedtime := greaterof(deltas[r].elapsedtime, result.elapsedtime);
    var nuBaseId: ni := realmbaseid;
//    Debug.log(inttostr(r)+'='+inttostr(realmbaseid));

    for var t := 0 to lesserof(high(deltas[r].data), high(deltas[r].desc)) do begin
      result.desc[taridx] := deltas[r].desc[t];
      result.data[taridx] := deltas[r].data[t];
      result.raw[taridx] := deltas[r].raw[t];

      if result.desc[taridx].id >=0 then
        result.desc[taridx].id := result.desc[taridx].id+realmbaseid;
      if result.data[taridx].id >=0 then
        result.data[taridx].id := result.data[taridx].id+realmbaseid;
      if result.desc[taridx].left >=0 then
        result.desc[taridx].left := result.desc[taridx].left+realmbaseid;
      if result.desc[taridx].above >=0 then
        result.desc[taridx].above := result.desc[taridx].above+realmbaseid;
      if result.raw[taridx].id >=0 then
        result.raw[taridx].id := result.raw[taridx].id+realmbaseid;


      nubaseid := greaterof(result.data[taridx].id, nubaseid);
      inc(taridx);

    end;
    realmbaseid :=  nubaseid;
  end;
end;

initialization
  orderlyinit.init.RegisterProcs('PerfMessageServer', oinit, ofinal, 'herro');


end.
