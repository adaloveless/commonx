unit Signals;
{$I DelphiDefs.inc}
interface

uses
{$DEFINE DOUBLE_CHECK_SIG}
{$ifdef WINDOWS}
  windows,
{$endif}
  syncobjs, sharedobject, typex, sysutils, tickcount, numbers;

type
  TArrayOfEvent = array of TEvent;
  TSignal = class(TSharedobject)
  private
    FEvent: TEvent;
    [volatile] FState: boolean;
    function GetHandle: THandle;
  public
    procedure Init;override;
    procedure Detach;override;
    function IsSignaled: boolean;
    function WaitFor(iTimeout: nativeint): boolean;overload;virtual;
    procedure WaitFor;overload;
    procedure Signal(b: boolean);virtual;
    procedure ResetSignal;inline;
{$IFDEF windows}
    property Handle: THandle read GEtHandle;
{$endif}
  end;
  TBetterSignal = TSignal;

function IsSignaled(ev: TEvent): boolean;overload;
function IsSignaled(ev: TSignal): boolean;overload;
procedure Signal(ev: TEvent; bSignal: boolean = true);overload;
procedure Signal(ev: TSignal; bSignal: boolean = true);overload;
procedure ResetSignal(ev: TEVent);overload;
procedure ResetSignal(ev: TSignal);overload;
function WaitForSignal(ev: TEvent; iTimeout: nativeint = -1): boolean;overload;
function WaitForSignal(ev: TSignal; iTimeout: nativeint = -1): boolean;overload;
{$IFDEF MSWINDOWS}
function WaitForMultipleSignals(ar: THandleObjectArray; out signaled: THandleObject; iTimeOut: nativeint = -1; bAll: boolean = false): boolean;
{$ENDIF}
function WaitForAnyOfTwoSignals(sig1, sig2: TSignal; out signaled: TSignal; iTimeout: nativeint = -1): boolean;

implementation

uses
  systemx;

{ TSignal }

procedure TSignal.Detach;
begin
  if not detached then begin
    Lock;
    FEvent.free;
    inherited;
  end;

end;

function TSignal.GetHandle: THandle;
begin
{$IFDEF MSWINDOWS}
  Result := FEvent.Handle;
{$ELSE}
  raise ECritical.create('cannot get signal handle on non-windows platforms');
{$endif}
end;

procedure TSignal.Init;
begin
  FEvent := TEvent.create;
  Fevent.ResetEvent;
  FState := false;
  inherited;


end;

function TSignal.IsSignaled: boolean;
begin
  result := FState;
end;

procedure TSignal.ResetSignal;
begin
  Signal(false);
end;

procedure TSignal.Signal(b: boolean);
begin
  Lock;
  try
    if b then begin
      FState := b;
      signals.Signal(FEvent, b);
    end else begin
      FState := b;
      signals.Signal(FEvent, b);

    end;
  finally
    Unlock;
  end;
end;

procedure TSignal.WaitFor;
begin
  if ISSignaled then begin
    exit;
  end;

  WaitForSignal(FEvent, nativeint(INFINITE));
end;

function TSignal.WaitFor(iTimeout: nativeint): boolean;
var
  tmStart: ticker;
begin


  tmStart := GetTicker;
  repeat
    if ISSignaled then begin
      result := true;
      exit;
    end;

    result := WaitForSignal(FEvent, lesserof(iTimeout,100));

  until (iTimeout >= 0) and (GetTimeSince(tmStart) > iTimeout);
end;


function IsSignaled(ev: TEvent): boolean;
begin
  result := WaitForSignal(ev,0);
end;
procedure Signal(ev: TEvent; bSignal: boolean = true);
begin
  if bSignal then
    ev.SetEvent
  else
    ev.ResetEvent;
end;

procedure ResetSignal(ev: TEVent);
begin
  ev.ResetEvent;
end;
function WaitForSignal(ev: TEvent; iTimeout: nativeint = -1): boolean;
var
  wr: TWaitResult;
  iThisWait: ni;
  tmStart: ticker;
  tmSince: ticker;
  tmToGo: int64;
begin
  tmStart := GetTicker;
  WR := wrAbandoned;
  repeat
    tmSince := GEtTimeSince(tmStart);
    tmToGo := iTimeout-tmSince;
    if (iTimeOut < 0) then
      iThisWait := 1000
    else
      iThisWait := lesserof(1000, greaterof(tmToGo,0));
    wr := ev.WaitFor(iThisWait);
    if wr = wrSignaled then begin
      exit(true);
    end;
  until (GetTimeSince(tmStart) > iTimeout) and (iTimeout >=0);

  if wr = wrAbandoned then begin
    raise Exception.Create('Signal was abandoned');
  end;


  if wr = wrIOCompletion then
    raise Exception.Create('Signal was of unexpected type IOCOmpletion');

  if wr = wrError then
    raise Exception.Create('Signal error');

  result := wr = wrSIgnaled;

end;

{$IFDEF WINDOWS}
function WaitForMultipleSignals(ar: THandleObjectArray; out signaled: THandleObject; iTimeOut: nativeint = -1; bAll: boolean = false): boolean;
var
  wr: tWaitResult;
begin
  wr := ar[0].WaitForMultiple(ar, iTimeout, bAll, signaled, false, length(ar));
  result := wr = wrSignaled;
end;
{$ENDIF}


function WaitForAnyOfTwoSignals(sig1, sig2: TSignal; out signaled: TSignal; iTimeout: nativeint = -1): boolean;
{$IFDEF WINDOWS}
var
  ar: THandleObjectArray;
  outsig: THandleObject;
begin
  if sig1.IsSignaled then begin
    result := true;
    signaled := sig1;
    exit;
  end;
  if sig2.IsSignaled then begin
    result := true;
    signaled := sig2;
    exit;
  end;

  setlength(ar, 2);
  ar[0] := sig1.FEvent;
  ar[1] := sig2.FEvent;
  result := WaitForMultipleSignals(ar, outsig, iTimeout);
  if not result then begin
    signaled := nil
  end
  else begin
    if outsig = sig1.FEvent then
      signaled := sig1
    else
      signaled := sig2;
  end;

end;
{$ELSE}
var
  tm: ticker;
begin
  tm := GetTicker;
  while (not Issignaled(sig1) and (not Issignaled(sig2))) do begin
    sleep(1);
    if gettimesince(tm) > iTimeOut then begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;
{$ENDIF}


function IsSignaled(ev: TSignal): boolean;overload;
begin
  result := ev.IsSignaled;
end;

procedure Signal(ev: TSignal; bSignal: boolean = true);overload;
begin
  ev.Signal(bSignal);
end;

procedure ResetSignal(ev: TSignal);overload;
begin
  ev.ResetSignal;
end;

function WaitForSignal(ev: TSignal; iTimeout: nativeint = -1): boolean;overload;
var
  iThisWait: ni;
  tmStart: ticker;
  tmSince: ticker;
  tmToGo: int64;
begin
  tmStart := GetTicker;
  repeat
    tmSince := GEtTimeSince(tmStart);
    tmToGo := iTimeout-tmSince;
    if (iTimeOut < 0) then
      iThisWait := 1000
    else
      iThisWait := lesserof(1000, greaterof(tmToGo,0));
    if ev.WaitFor(iThisWait) then begin
{$IFDEF DOUBLE_CHECK_SIG}
      if not ev.IsSignaled then
        exit(false);
{$ENDIF}
      exit(true);
    end;
  until (GetTimeSince(tmStart) > iTimeout) and (iTimeout >=0);

  result := false;

end;





end.
