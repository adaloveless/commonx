unit skill;

interface

uses
  systemx, betterobject, sysutils, sharedobject, better_collections, generics.collections, orderlyinit, stringx, classes, typex, tickcount;

type
  TSkillDef = class;//forward

  TSkills = class(TSharedobject)
  private
    FList: TBetterList<TSkillDef>;
  public
    procedure RegisterSkill(sCommaSeparatedData: string; peerip: string; peerport: ni);
    procedure RegisterLocalSkill(servicename: string; version: cardinal; endpoint: string = '420';protocol: string = 'RDTP/RUDP');
    procedure RegisterRemoteSkill(servicename: string; version: cardinal; host: string; endpoint: string = '420'; protocol: string = 'RDTP/RUDP');
    function GetSkillList(localonly: boolean): IHolder<TStringList>;
    constructor Create; override;
    procedure Detach; override;
    function IndexOf(sk: TSkillDef; byIP: boolean): ni;
    procedure Delete(idx: ni);
    function Has(sk: TSkillDef; byIP: boolean): boolean;
    function Find(sName: string): TSkillDef;
    procedure ScrubSkills;
  end;


  TSkillInfo = record
    name: string;
    version: cardinal;
    protocol: string;
    endpoint: string;
    host: string;
    hostid: string;

    procedure Init;
    procedure FromString(s: string);
    function ToString: string;
    class operator equal(a,b: TSkillInfo): boolean;
  end;

  TSkillDef = class(TBetterObject)
  public
    info: TskillInfo;
    local: boolean;
    lastseen: TDateTime;
    function IsValid: boolean;
    procedure init;override;
  end;

  TSkill = class(TSharedobject)
  public

  end;

procedure RegisterLocalSkill(servicename: string; version: cardinal; endpoint: string = '420';protocol: string = 'RDTP/RUDP');
procedure RegisterRemoteSkill(servicename: string; version: cardinal; host: string; endpoint: string = '420'; protocol: string = 'RDTP/RUDP');


var
  hostid: string;
  Skills: TSKills= nil;

implementation

uses
  networkx;

procedure RegisterLocalSkill(servicename: string; version: cardinal; endpoint: string = '420';protocol: string = 'RDTP/RUDP');
begin
  Skills.RegisterLocalSkill(servicename, version, endpoint, protocol);

end;
procedure RegisterRemoteSkill(servicename: string; version: cardinal; host: string; endpoint: string = '420'; protocol: string = 'RDTP/RUDP');
begin
  Skills.RegisterRemoteSkill(servicename, version, endpoint, protocol);
end;



procedure oinit;
begin
  Skills:= TSkills.create;
  Randomize;
  hostid := networkx.gethostid;
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
  if h.o.count <> 6 then
    raise ECritical.create('unrecognized count in skillinfo: '+inttostr(h.o.count));

  name := h.o[0];
  version := strtoint(h.o[1]);
  protocol := h.o[2];
  endpoint := h.o[3];
  host := h.o[4];
  hostid := h.o[5];


end;

procedure TSkillInfo.Init;
begin
  host := 'localhost';
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

function TSkills.GetSkillList(localonly: boolean): IHolder<TStringList>;
var
  t: ni;
begin
  result := THolder<TSTringList>.create;
  result.o := TStringlist.create;
  Lock;
  try
    for t:= 0 to Flist.count-1 do begin
      if (not localonly) or FList[t].local then begin
        result.o.Add(FList[t].info.ToString);
      end;
    end;

  finally
    unlock;
  end;
end;

function TSkills.Has(sk: TSkillDef; byIP: boolean): boolean;
begin
  result := IndexOf(sk, byip) >=0;
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

procedure TSkills.RegisterLocalSkill(servicename: string; version: cardinal;
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
    sk.info.hostid := skill.hostid;
    sk.local := true;
    if not has(sk, false) then
      FList.add(sk)
    else
      sk.free;
  finally
    unlock;
  end;

end;

procedure TSkills.RegisterRemoteSkill(servicename: string; version: cardinal;
  host, endpoint, protocol: string);
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
    sk.info.host := host;
    if not has(sk, false) then
      FList.add(sk)
    else begin
      FList[Indexof(sk, false)].lastseen := now;
      sk.free;
    end;
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
  sk.info.host := peerip;
  lock;
  try
    if not has(sk,false) then
      FList.add(sk)
    else begin
      FList[Indexof(sk, false)].lastseen := now;
      sk.free;
    end;

  finally
    unlock;
  end;

end;

procedure TSkills.ScrubSkills;
var
  t: ni;
begin
  Lock;
  try
    for t:= FList.count-1 downto 0 do begin
      if not FList[t].isvalid then
        Flist.delete(t);
    end;
  finally
    unlock;
  end;
end;

function TSkillInfo.ToString: string;
begin
  result := name+','+version.tostring+','+protocol+','+endpoint+','+host+','+hostid;
end;

{ TSkillDef }

procedure TSkillDef.init;
begin
  inherited;
  lastseen := now;
end;

function TSkillDef.IsValid: boolean;
begin
  result := Now-lastseen < (1/(60*24));
end;

initialization

orderlyinit.init.RegisterProcs('skill', oinit, ofinal, 'ManagedThread,CommandProcessor');

end.
