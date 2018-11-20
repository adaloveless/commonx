unit AbstractLocker;

interface

uses
  betterobject, typex, systemx, sharedobject, generics.collections, tickcount, numbers, classes;

type
  TAbstractLock = class(TBetterObject)
    function CollidesWith(l: TAbstractLock): boolean;virtual;abstract;
    function ToPersistentString: string;virtual;
    procedure FromPersistentString(s: string);virtual;
  end;


  TAbstractLocker<_TYPE_: TAbstractLock, constructor> = class(TSharedObject)
  strict protected
    FLocks: TList<_TYPE_>;
  public
    constructor Create;override;
    destructor Destroy;override;
    function New: _TYPE_;
    function TryGet(lck: _TYPE_; iTimeout: ni = 0): boolean;overload;
    function TryGet(sHash: string; iTimeOut: ni = -1): _TYPE_;overload;
    procedure Release(lck: _TYPE_);

    procedure Get(lck: _TYPE_);
    procedure PersistToFile(sFilename: string);
    procedure LoadFromFile(sFileName: string);

    procedure ClearLocks;

  end;

implementation

{ TAbstractLocker<_TYPE_> }

procedure TAbstractLocker<_TYPE_>.ClearLocks;
var
  l: _TYPE_;
begin
  Lock;
  try
    while FLocks.count > 0 do begin
      l := FLocks[FLocks.count-1];
      FLocks.Delete(FLocks.count-1);
      l.free;
      l := nil;
    end;
  finally
    Unlock;
  end;
end;

constructor TAbstractLocker<_TYPE_>.Create;
begin
  inherited;
  FLocks := TList<_TYPE_>.create;

end;

destructor TAbstractLocker<_TYPE_>.Destroy;
begin
  if FLocks.count > 0 then
    raise ECritical.create('destroying a locker with active locks.');
  FLocks.free;
  FLocks := nil;
  inherited;
end;

procedure TAbstractLocker<_TYPE_>.Get(lck: _TYPE_);
begin
  while not TryGet(lck, -1) do sleep(1);


end;

procedure TAbstractLocker<_TYPE_>.LoadFromFile(sFileName: string);
var
  t: ni;
  sl: TStrings;
  al: _TYPE_;
begin
  sl := TStrings.create;
  try
    sl.LoadFromFile(sFileName);

    for t:= 0 to sl.count-1 do begin
      al := self.New;
      al.FromPersistentString(sl[t]);
      FLocks.add(al);
    end;


  finally
    sl.free;
  end;


end;

function TAbstractLocker<_TYPE_>.New: _TYPE_;
begin
  result := _TYPE_.create;

end;

procedure TAbstractLocker<_TYPE_>.PersistToFile(sFilename: string);
var
  t: ni;
  l: _TYPE_;
  sl: TStrings;
begin
  lock;
  try
    sl := TStrings.Create;
    try
      for t:= 0 to FLocks.count-1 do begin
        l := FLocks[t];
        sl.add(l.ToPersistentString);
      end;
    finally
      sl.free;
    end;
  finally
    unlock;
  end;

end;

procedure TAbstractLocker<_TYPE_>.Release(lck: _TYPE_);
var
  t: ni;
begin
  Lock;
  try
    FLocks.remove(lck);
    lck.free;
  finally
    unlock;
  end;


end;


function TAbstractLocker<_TYPE_>.TryGet(sHash: string; iTimeOut: ni = -1): _TYPE_;
begin
  Lock;
  try
    result := New;
    result.FromPersistentString(sHash);
    if not TryGet(result,iTimeOut) then begin
      result.free;
      result := nil;
    end;
  finally
    Unlock;
  end;
end;

function TAbstractLocker<_TYPE_>.TryGet(lck: _TYPE_; iTimeout: ni): boolean;
var
  t: ni;
  tm, tmNow, tmSince, tmLeft: ticker;
begin

  result := false;
  tm := GetTicker;
  repeat
    lock;
    try
      result := true;
      for t:= 0 to FLocks.count-1 do begin
        if lck.collideswith(Flocks[t]) then begin
          result := false;
          exit;
        end;
      end;
    finally
      unlock;
    end;

    tmNow := GetTicker;
    tmSince := GetTimeSince(tmNow, tm);
    tmLEft := greaterof(0,iTimeout) - tmSince;
    if (iTimeOut >= 0) and (tmSince > iTimeout) then
      exit;
    if not result then
      sleep(lesserof(tmLeft, 1*FLocks.count));//todo 5: put in some kind of subscription-to-change event
  until result;
end;

{ TAbstractLock }

procedure TAbstractLock.FromPersistentString(s: string);
begin
  raise ECritical.create('trying to persist TAbstractLock, not allowed without overriding ToPersistentString()');
end;

function TAbstractLock.ToPersistentString: string;
begin
  raise ECritical.create('trying to persist TAbstractLock, not allowed without overriding ToPersistentString()');
end;

end.
