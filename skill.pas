unit skill;

interface

uses
  systemx, betterobject, sysutils, sharedobject, better_collections, generics.collections, orderlyinit, stringx, classes, typex, tickcount;

type
  TSkillDef = class;//forward


  TSkillInfo = record
    name: string;
    version: cardinal;
    protocol: string;
    endpoint: string;
    greeterhost: string;
    hostid: string;
    greeterport: cardinal;
    local: boolean;
    function Host: string;

    procedure Init;
    procedure FromString(s: string);
    function ToString: string;
    class operator equal(a,b: TSkillInfo): boolean;
  end;

  TSkillArray = array of TSkillInfo;


  TSkills = class(TSharedobject)
  private
    FList: TBetterList<TSkillDef>;
  public
    procedure RegisterSkill(sCommaSeparatedData: string; peerip: string; peerport: ni);
    procedure RegisterLocalSkill(eport: ni; servicename: string; version: cardinal; endpoint: string = '420';protocol: string = 'RDTP/RUDP');
    function GetSkillList(localonly: boolean): IHolder<TStringList>;
    function GetValidSkillList(): IHolder<TStringList>;
    function GetSkillListToCheck(): TSkillArray;
    constructor Create; override;
    procedure Detach; override;
    function IndexOf(sk: TSkillDef; byIP: boolean): ni;overload;
    function IndexOf(sk: TSkillInfo; byIP: boolean): ni;overload;
    function IndexOf(s: ansistring; byIP: boolean): ni;overload;

    procedure Delete(idx: ni);
    function Has(sk: TSkillDef; byIP: boolean): boolean;overload;
    function Has(sk: TSkillInfo; byIP: boolean): boolean;overload;
    function Has(sk: ansistring; byIP: boolean): boolean;overload;
    function HasLocal(sk: ansistring; byIP: boolean): boolean;overload;
    function GetSkillsOfType(sType: string; protocol: string = 'RDTP/TCP'): TSkillArray;

    function Find(sName: string): TSkillDef;
    function FindAll(sName: string): TArray<TSkillInfo>;

    procedure ScrubSkills;
    procedure Remove(sk: ansistring; byIP: boolean);
    procedure MarkValid(sk: ansistring; peerip: string; peerport: ni; byIP: boolean);
  end;




  TSkillDef = class(TBetterObject)
  public
    info: TskillInfo;
    lastseen: TDateTime;
    lastValidationRequest: TDateTime;
    lastValidation: TDateTime;
    function IsValid: boolean;
    procedure init;override;
    function NeedsValidation: boolean;
  end;

  TSkill = class(TSharedobject)
  public

  end;

procedure RegisterLocalSkill(eport: ni;servicename: string; version: cardinal; endpoint: string = '420';protocol: string = 'RDTP/RUDP');


var
//  hostid: string;
  Skills: TSKills= nil;

implementation

uses
  networkx;

procedure RegisterLocalSkill(eport: ni;servicename: string; version: cardinal; endpoint: string = '420';protocol: string = 'RDTP/RUDP');
begin
  Skills.RegisterLocalSkill(eport, servicename, version, endpoint, protocol);
end;



procedure oinit;
begin
  Skills:= TSkills.create;
  Randomize;
//  hostid := networkx.gethostid;
end;

procedure ofinal;
begin
  Skills.free;
end;

class operator TSkillInfo.equal(a, b: TSkillInfo): boolean;
begin
  result :=(a.hostid = b.hostid) and (a.name = b.name) and (a.protocol = b.protocol);


end;

procedure TSkillInfo.FromString(s: string);
var
  h: IHolder<TStringList>;
begin
  h := ParseStringH(s, ',');
  if not h.o.count in [6,7,8] then
    raise ECritical.create('unrecognized count in skillinfo: '+inttostr(h.o.count));

  name := h.o[0];
  version := strtoint(h.o[1]);
  protocol := h.o[2];
  endpoint := h.o[3];
  greeterhost := h.o[4];
  hostid := h.o[5];
  if h.o.count > 6 then
    greeterport := strtoint(h.o[6])
  else
    greeterport := 1212;

  if h.o.count > 7 then
    local := h.o[7] = '1'
  else
    local := false;


end;

function TSkillInfo.Host: string;
var
  l,r: string;
begin
  SplitString(HostId, '@', l,r);
  SplitString(r,'/', result, r);
end;

procedure TSkillInfo.Init;
begin
  greeterhost := 'localhost';
end;

{ TSkills }

constructor TSkills.Create;
begin
  inherited;
  FList := TBetterList<TSkillDef>.create;
end;

procedure TSkills.Delete(idx: ni);
begin
  Lock;
  try
    FList[idx].free;
    FList.delete(idx);
  finally
    unlock;
  end;
end;

procedure TSkills.Detach;
begin
  if detached then exit;

  Flist.ClearandFree;
  FList.free;
  fList := nil;
  inherited;

end;

function TSkills.Find(sName: string): TSkillDef;
var
  t: ni;
begin
  result := nil;
  Lock;
  try
    for t:= 0 to Flist.count-1 do begin
      if comparetext(Flist[t].info.name,sName) = 0 then begin
        exit(FList[t]);
      end;
    end;
  finally
    Unlock;
  end;
end;

function TSkills.FindAll(sName: string): TArray<TSkillInfo>;
var
  t: ni;
begin
  setlength(result, 0);
  Lock;
  try
    for t:= 0 to Flist.count-1 do begin
      if comparetext(Flist[t].info.name,sName) = 0 then begin
        setlength(result, length(result)+1);
        result[high(result)] := FList[t].info;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TSkills.GetSkillList(localonly: boolean): IHolder<TStringList>;
var
  t: ni;
begin
  result := THolder<TSTringList>.create;
  result.o := TStringlist.create;
  Lock;
  try
    for t:= 0 to Flist.count-1 do begin
//      if FList[t].info.greeterport = 0 then
//        FList[t].info.greeterport := ephemeralport;
      if (not localonly) or FList[t].info.local then begin
        result.o.Add(FList[t].info.ToString);
      end;
    end;

  finally
    unlock;
  end;
end;

function TSkills.GetSkillListToCheck: TSkillArray;
var
  t: ni;
  idxout: ni;
begin
  Lock;
  try
    idxout := 0;
    setlength(result, FList.count);
    for t:= 0 to Flist.count-1 do begin
      if FList[t].NeedsValidation then begin
        result[idxout] := FList[t].info;
        inc(idxout);
      end;
      FList[t].lastValidationRequest := now;
    end;

    setlength(result, idxout);
  finally
    unlock;
  end;

end;

function TSkills.GetSkillsOfType(sType: string; protocol: string = 'RDTP/TCP'): TSkillArray;
begin
  setlength(result, 0);
  var l:= LockI;
  for var t:= 0 to FList.count-1 do begin
    if (comparetext(FList[t].info.name,sType)=0)
    and ((protocol='') or ((comparetext(Flist[t].info.protocol,protocol)=0)))
    then begin
      setlength(result,length(result)+1);
      result[high(result)] := Flist[t].info;
    end;
  end;
end;

function TSkills.GetValidSkillList: IHolder<TStringList>;
var
  t: ni;
begin
  result := THolder<TSTringList>.create;
  result.o := TStringlist.create;
  Lock;
  try
    for t:= 0 to Flist.count-1 do begin
      if FList[t].IsValid then begin
        result.o.Add(FList[t].info.ToString);
      end;
    end;

  finally
    unlock;
  end;
end;

function TSkills.Has(sk: TSkillInfo; byIP: boolean): boolean;
begin
  result := IndexOf(sk, byip) >=0;
end;

function TSkills.Has(sk: ansistring; byIP: boolean): boolean;
begin
  result := IndexOf(sk, byip) >=0;
end;

function TSkills.HasLocal(sk: ansistring; byIP: boolean): boolean;
var
  i: ni;
begin
  Lock;
  try
    i := IndexOf(sk, byip);
    if i < 0 then
      exit(false)
    else
      exit(FList[i].info.local);

  finally
    Unlock;
  end;

end;

function TSkills.Has(sk: TSkillDef; byIP: boolean): boolean;
begin
  result := IndexOf(sk, byip) >=0;
end;

function TSkills.IndexOf(sk: TSkillInfo; byIP: boolean): ni;
var
  t: ni;
begin
  result := -1;
  Lock;
  try
    for t:= 0 to fList.count-1 do begin
      if FList[t].info = sk then begin
        exit(t);
      end;
    end;
  finally
    unlock;
  end;

end;

function TSkills.IndexOf(s: ansistring; byIP: boolean): ni;
var
  t: ni;
  ss: string;
  skcheck: TSkillInfo;
  sk: TSkillInfo;
begin
  skcheck.FromString(s);

  s := ansistring(zcopy(s,0,length(s)-2));
  result := -1;
  Lock;
  try
    for t:= 0 to fList.count-1 do begin
      sk := Flist[t].info;
      if (zpos(lowercase(skcheck.host+'/'), lowercase(sk.hostid)) >=0)
      and (zpos(lowercase(skcheck.protocol), lowercase(sk.protocol)) >=0) then begin

        if skcheck.name = sk.name then
          exit(t);
      end;

    end;
  finally
    unlock;
  end;
end;

procedure TSkills.MarkValid(sk: ansistring; peerip: string; peerport: ni; byIP: boolean);
var
  i: ni;
begin
  Lock;
  try
    i :=  IndexOf(sk, byIP);
    if i < 0 then begin
      self.RegisterSkill(sk, peerip, peerport);
      i :=  IndexOf(sk, byIP);
      if i<0 then
        exit;
    end;

    fList[i].lastValidationREquest := now;
    fList[i].lastseen := now;
    fList[i].lastValidation := now;


  finally
    Unlock;
  end;
end;

function TSkills.IndexOf(sk: TSkillDef; byIP: boolean): ni;
var
  t: ni;
begin
  result := -1;
  Lock;
  try
    for t:= 0 to fList.count-1 do begin
      if FList[t].info = sk.info then begin
        exit(t);
      end;
    end;
  finally
    unlock;
  end;

end;

procedure TSkills.RegisterLocalSkill(eport: ni; servicename: string; version: cardinal;
  endpoint, protocol: string);
var
  sk: TSkillDef;
begin
  Lock;
  try
    sk := TSkillDef.create;
    sk.info.Init;
    sk.info.name := servicename;
    sk.info.version := version;
    sk.info.protocol := protocol;
    sk.info.endpoint := endpoint;
    sk.info.hostid := networkx.GetHostID(eport);
    sk.info.greeterport := eport;
    sk.info.local := true;
    if not has(sk, false) then
      FList.add(sk)
    else
      sk.free;
  finally
    unlock;
  end;

end;



procedure TSkills.RegisterSkill(sCommaSeparatedData: string; peerip: string; peerport: ni);
var
  sk: TSkillDef;
begin
  sk := TSkillDef.create;
  sk.info.FromString(sCommaSeparatedData);
  sk.info.local := false;
  sk.info.greeterhost := peerip;
  lock;
  try
    if not has(sk,false) then begin
      FList.add(sk);
      FList[Indexof(sk, false)].lastseen := now;
    end else begin
      sk.free;
    end;

  finally
    unlock;
  end;

end;

procedure TSkills.Remove(sk: ansistring; byIP: boolean);
var
  i: ni;
begin
  i := IndexOf(sk, byIP);
  if i >=0 then begin
    Delete(i);
  end;

end;

procedure TSkills.ScrubSkills;
var
  t: ni;
  sk: TSkillDef;
begin
  Lock;
  try
    for t:= FList.count-1 downto 0 do begin
      if (not FList[t].isvalid) (*and ((Now-FList[t].lastValidationRequest) > 4000)*) then begin
        sk := FList[t];
        Flist.delete(t);
        sk.free;
        sk := nil;
      end;
    end;
  finally
    unlock;
  end;
end;

function TSkillInfo.ToString: string;
begin
  result := name+','+version.tostring+','+protocol+','+endpoint+','+host+','+hostid+','+greeterport.tostring+','+booltoint(local).tostring;
end;

{ TSkillDef }

procedure TSkillDef.init;
begin
  inherited;
  lastseen := now;
end;

function TSkillDef.IsValid: boolean;
begin
  result := info.local or ((Now-lastValidation) < (30*(1/(60*24))));
end;

function TSkillDef.NeedsValidation: boolean;
begin
  result := (not info.local) and ((Now- lastValidationRequest) > (15*(1/(60*24))));
end;

initialization

orderlyinit.init.RegisterProcs('skill', oinit, ofinal, 'ManagedThread,CommandProcessor');

end.
