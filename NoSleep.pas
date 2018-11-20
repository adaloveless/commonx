unit NoSleep;

interface

uses
  periodicEvents, QueueStream, systemx, classes, sysutils, tickcount, sharedobject, applicationparams, namevaluepair, orderlyinit, typex, dir, generics.collections;


type
  TNoSleepEvent = class(TPeriodicEvent)
  private
    FPath: string;
    procedure SetPath(const Value: string);
  public
    procedure Init;override;
    property Path: string read FPath write SetPath;
    procedure DoExecute; override;
  end;

  TNoSleeper = class(TSharedObject)
  strict protected
    FUserPaths: TSTringList;
    FAppPaths: TSTringList;
    FEVents: TList<TNoSleepEvent>;
    procedure LoadStuff(nvpl: TNAmeValuePairList; sl: TStringList;sName: string);overload;
    procedure StartEvents(sl: TStringList);overload;
  public
    procedure Detach;override;
    procedure Init;override;
    procedure LoadStuff;overload;
    procedure StopEvents;
    procedure StartEvents;overload;
  end;

var
  ns: TNoSleeper;

implementation

{ TNoSleepEvent }

procedure TNoSleepEvent.DoExecute;
var
  f: TUnbufferedFileStream;
  s: string;
  i: int64;
begin
  inherited;
  Frequency := (15000+(random(5000)));
  s := path;
  try
    if fileexists(s) then
      deletefile(s);

    f := nil;
    try
      f := TUnbufferedFileStream.Create(s, fmCReate);
      i := getticker;
      f.Write(f, sizeof(i));
    finally
      f.free;
      f := nil;
    end;

  except
  end;
end;

procedure TNoSleepEvent.Init;
begin
  inherited;
  Frequency := 1;

end;

procedure TNoSleepEvent.SetPath(const Value: string);
begin
  FPath := Value;
  UniqueString(FPath);
  try
    ForceDirectories(extractfilepath(FPath));
  except
  end;


end;

{ TNoSleeper }

procedure TNoSleeper.Detach;
begin
  if Detached then
    exit;

  StopEvents;

  FEvents.free;
  FUserPaths.free;
  FAppPaths.free;
  FEvents := nil;
  FUserPaths := nil;
  FAppPaths := nil;
  inherited;

end;

procedure TNoSleeper.Init;
begin
  inherited;
  FUserPAths := TStringlist.create;
  FAppPaths := TStringlist.create;
  FEvents := TList<TNoSleepEvent>.create;
  LoadStuff;

end;

procedure TNoSleeper.LoadStuff(nvpl: TNAmeValuePairList; sl : TStringList; sNAme: string);
var
  s: string;
  t: ni;
begin
  t := 0;
  while true do begin
    s := nvpl.GetItemEx('NoSleep'+inttostr(t), '');
    if s = '' then break;
    sl.Add(slash(s)+sNAme);
    inc(t);
  end;

end;

procedure TNoSleeper.LoadStuff;
var
  ap: TAppParams;
begin
  FUserPaths.Clear;
  FAppPaths.clear;
  StopEvents;
  ap := NeedAppParams;
  try
    LoadStuff(ap, FAppPaths, extractfilenamepart(dllname)+'.app.keepalive');
  finally
    NoNeedAppPArams(ap);
  end;

  ap := NeedUserParams;
  try
    LoadStuff(ap,FUserPaths, extractfilenamepart(dllname)+'.user.keepalive');
  finally
    NoNeedUSerPArams(ap);
  end;

  StartEvents;
end;

procedure TNoSleeper.StartEvents;
begin
  StartEvents(FUserPaths);
  STartEvents(FAppPaths);

end;

procedure TNoSleeper.StartEvents(sl: TStringList);
var
  nse: TNoSleepEvent;
  t: ni;
begin
  for t:= 0 to sl.count-1 do begin
    nse := TNoSleepEvent.Create;
    nse.Path := sl[t];
    FEvents.add(nse);
    PEA.Add(nse);
  end;
end;

procedure TNoSleeper.StopEvents;
var
  nse: TNoSleepEvent;
begin
  while FEVents.Count > 0 do begin
    nse := FEvents[0];
    FEvents.delete(0);
    PEA.Remove(nse);
    nse.Free;
    nse := nil;
  end;
end;

procedure oinit;
begin
  ns := TNoSleeper.create;
end;

procedure ofinal;
begin
  ns.free;
  ns := nil;
end;



initialization
  init.RegisterProcs('NoSleep', oinit, ofinal, 'PeriodicEvents');

end.
