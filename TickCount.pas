unit TickCount;
{$MESSAGE '*******************COMPILING TickCount.pas'}
{$I DelphiDefs.inc}
interface

uses
{$IFDEF IOS}
  iosapi.foundation,
{$ELSE}
{$IFDEF ANDROID}
{$ELSE}
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
{$ENDIF}

{$ENDIF}
  classes,
  {$IFNDEF FPC}
    system.Diagnostics,
  {$ELSE}

  {$ENDIF}
  sysutils;

type
  ticker = int64;

  TThreadTimes = record
    kernel: int64;
    user: int64;
  end;

function GetThreadTime(thrid: NativeUint): TThreadTimes;
function GetTicker: ticker;
function GetHighResTicker: ticker;
function Get100NanoTicker: ticker;
function hr2us(t:ticker): ticker;inline;
function hr2ms(t:ticker): ticker;inline;

function GetTimeSInce(tmNow, tmSince: ticker): ticker;inline;overload;
function GetTimeSInce(tmSince: ticker): ticker;inline;overload;
function GetTimeSInceHR(tmSince: ticker): ticker;

procedure Sleep(tm: ticker);
procedure SleepEx(tm: ticker; bAlertable: boolean = true);

//type
//  TUT_TickCount = class(TUnitTest)
//  public
//    procedure Init;override;
//    procedure DoExecute;override;
//  end;

function FormatHRT(i: int64): string;
var
  GTC: TStopWatch;

implementation

uses
  orderlyinit,stringx;

function FormatHRT(i: int64): string;
var
  s, s1,s2,s3: string;
begin
  s := i.tostring;
  s1 := zcopy(s, 0, length(s)-7);
  s2 := zcopy(s, length(s)-7,3);
  s3 := zcopy(s, length(s)-4,4);
  result := s1+'.'+s2+'.'+s3;


end;

function GetTicker: ticker;
begin
  result := GTC.ElapsedMilliseconds;
end;


{$IFDEF WINDOWS}
function GetThreadTime(thrid: NativeUint): TThreadTimes;
var
  creation, exit, user, kernel: _FILETIME;
begin
  windows.GetThreadTimes(thrid, creation, exit, kernel, user);
  RESULT.user := (int64(user.dwHighDateTime) shl 32)+user.dwLowDateTime;
  RESULT.kernel := (int64(kernel.dwHighDateTime) shl 32)+kernel.dwLowDateTime;



end;
{$ELSE}
function GetThreadTime(thrid: NativeUint): TThreadTimes;
begin
  RESULT.user := 0;
  RESULT.kernel := 0;

end;
{$ENDIF}

function Get100NanoTicker: ticker;
begin
  result := GEtHighResTicker;
end;
{$IFDEF WINDOWS}
function GetHighResTicker: ticker;
var
  freq: Int64;
begin
  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(result);
  result := round((result / freq) * 10000000) ;
end;
{$ELSE}
function GetHighResTicker: ticker;
begin
  result := GetTicker * 1000;
end;

{$ENDIF}


function GetTimeSInce(tmSince: ticker): ticker;
begin
  result := GetTimeSince(GetTicker, tmSince);
end;

function GetTimeSInceHR(tmSince: ticker): ticker;
begin
  result := GetTimeSince(GetHighResTicker, tmSince);
end;

function GetTimeSince(tmNow, tmSince: ticker): ticker;
begin
  if tmNow >= tmSince then begin
    result:= tmNow-tmSince
  end else begin
    result := tmNow+(($FFFFFFFF - tmSince)+1);
  end;


end;


procedure oinit;
begin

  GTC := TStopWatch.Create;
  GTC.Start;
//  UTF.RegisterClass(TUT_TickCount);

end;

procedure ofinal;
begin
  //GTC.free;

end;

procedure Sleep(tm: ticker);
begin
{$IFNDEF WINDOWS}
  TThread.Sleep(tm);

{$ELSE}
  windows.Sleep(tm);
{$ENDIF}
end;

procedure SleepEx(tm: ticker; bAlertable: boolean = true);
begin
{$IFNDEF WINDOWS}
  TThread.Sleep(tm);

{$ELSE}
  windows.Sleepex(tm, bAlertable);
{$ENDIF}

end;

function hr2us(t:ticker): ticker;inline;
begin
  result := t div 10;
end;
function hr2ms(t:ticker): ticker;inline;
begin
  result := t div 10000000;
end;





{ TUT_TickCount }

//procedure TUT_TickCount.DoExecute;
//var
//  a,b: ticker;
//begin
//  inherited;
//  case variation of
//    1: begin
//      VariationNAme := 'sleep(1000)';
//      a := GetTicker;
//      sleep(1000);
//      b := GetTicker;
//      UTresult := inttostr((GetTimeSince(b,a) div 100) * 100);
//    end;
//  end;
//end;
//
//procedure TUT_TickCount.Init;
//begin
//  inherited;
//
//end;
//
initialization
  oinit;
  init.RegisterProcs('TickCount', oinit, ofinal);

finalization
  ofinal;


end.


