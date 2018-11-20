unit ArcHub;

interface


uses
  archiver, orderlyinit, lockqueue, generics.collections.fixed, typex, sysutils, applicationparams;

type
  TArcHub = class(TLockQueue)
  private
    function GetArc(idx: ni): TArchiver;
  protected
    FArcs: TList<TArchiver>;
  public
    procedure Init;override;
    procedure BeforeDestruction;override;
    procedure LoadFromConfig;
    procedure DestroyArchivers;

    function IndexOf(sArcName: string): ni;
    function Find(sArcName: string): TArchiver;
    function Count: ni;
    property arcs[idx: ni]: TArchiver read GetArc;


  end;



var
  garchub: TArchub;


implementation






procedure oinit;
begin
  garchub := TArchub.Create;
  //garchub.LoadFromConfig;
end;

procedure ofinal;
begin
  garchub.free;
  garchub := nil;
end;


procedure oLATEfinal;
begin
//  garchub.free;
  garchub := nil;
end;

{ TArcHub }

procedure TArcHub.BeforeDestruction;
begin
  inherited;
  DestroyArchivers;
  FArcs.free;
  FArcs := nil;
end;

function TArcHub.Count: ni;
begin
  result := FARcs.count;
end;

procedure TArcHub.DestroyArchivers;
var
  a: TArchiver;
begin
  while FARcs.Count > 0 do begin
    a := FArcs[0];
    FARcs.Delete(0);
    a.Free;
    a := nil;
  end;
end;

function TArcHub.Find(sArcName: string): TArchiver;
var
  i: ni;
  l: TLock;
begin
  l := GetLock;
  try
    i := IndexOf(sArcName);
    if i < 0 then
      result := nil
    else
      result := FArcs[i];
  finally
    unlocklock(l);
  end;
end;

function TArcHub.GetArc(idx: ni): TArchiver;
begin
  result := FArcs[idx];
end;

function TArcHub.IndexOf(sArcName: string): ni;
var
  l: TLock;
  t: ni;
  s: string;
begin
  result := -1;
  l := GetLock;
  try
    s := lowercase(sArcName);
    for t:= 0 to FArcs.Count-1 do begin
      if CompareText(FArcs[t].Name, s) = 0 then begin
        result := t;
        exit;
      end;
    end;
  finally
    unlocklock(l);
  end;
end;

procedure TArcHub.Init;
begin
  inherited;
  FARcs := TList<TARchiver>.create;
  LoadFromConfig;
end;

procedure TArcHub.LoadFromConfig;
var
  ap: TAppParams;
  idx: ni;
  sName, sPath: string;
  a: TArchiver;
begin
  ap := NeedAppParams;
  try
    idx := 0;
    while true do begin
      sName := ap.GetItemEx('Archive'+inttostr(idx)+'Name', '');
      sPath := ap.GetItemEx('Archive'+inttostr(idx)+'VatPath', '');
      if sName = '' then break;
      if sPath = '' then break;
//      if (sPath) then begin
      a := TARchiver.Create;
      a.LoadConfig('Archive'+inttostr(idx));
      inc(idx);
      FArcs.add(a);

//      end;
    end;
  finally
    noneedappparams(ap);
  end;


end;

initialization
  init.RegisterProcs('ArcHub', oinit, nil, ofinal, oLATEfinal,  'PeriodicEvents,Debug');

end.
