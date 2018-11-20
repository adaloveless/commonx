unit UT_ThreadingAdvanced;

interface

uses
  unittest, systemx, System.SyncObjs, managedthread, sysutils, tickcount, signals;

type
  TThread_WaitsForSignal= class(TManagedThread)
  protected
    waitingon: TEvent;
  public
    ut: TUnitTest;
    result : string;
    DelayAfterReceivingSignal: ticker;
    procedure DoExecute;override;
  end;



  TThread_SetsSignal = class(TManagedThread)
  public
    my_external_signal: TEvent;
    ut: TUnitTest;
    time_to_wait_before_set: nativeint;
    procedure Init;override;
    destructor Destroy;override;
    procedure DoExecute;override;
  end;

  TThread_SetsSignal2 = class(TThread_SetsSignal)
  public
    ping_count: integer;
    procedure DoExecute;override;
    procedure Ping;
  end;

  TThread_WaitsForSignal2 = class(TThread_WaitsForSignal)
  public
    ss: TThread_SetsSignal2;
    procedure DoExecute;override;
  end;


  TUT_ThreadingAdvanced = class(TUnitTest)
  private
  protected
    LTPM: TMasterThreadPool;
    procedure PoolTest0;
    procedure ThreadTerminationTest(bOneShot, bStartFromPool, bEndToPool,
      bFinishedAtEnd: boolean);
    procedure EventTest;
    procedure EventTestRepeated;
    procedure WaitForThreadPoolToShrink(tp: TThreadPoolBase);
    procedure WAitForThreadPoolToFullyInductThread;
    procedure PoolTest1;
    procedure PoolTest2;
    procedure PoolTest3;
    procedure PoolTest4;

  public
    procedure Init;override;
    procedure Detach;override;

  end;

  TUT_ThreadSubSet_Termination = class(TUT_ThreadingAdvanced)
    procedure DoExecute;override;
  end;

  TUT_ThreadSubSet_Other = class(TUT_ThreadingAdvanced)
    procedure DoExecute;override;
  end;


implementation

{ TUT_ThreadingAdvanced }

procedure TUT_ThreadingAdvanced.Detach;
begin
  inherited;
  LTPM.Free;
  LTPM := nil;


end;

procedure TUT_ThreadSubSet_Termination.DoExecute;
var
  tmStarT: ticker;
begin
  inherited;
  case Variation of
    1: begin
        PoolTest0;
        VariationNAme := 'Wait For Thread Pool To Shrink';
    end;
    10: begin
      VariationName := 'Pool Finished One-shot thread';
      ThreadTerminationTest(true, true, true, true);
    end;
    20: begin
      variationName := 'Pool Unfinished One-shot thread';
      ThreadTerminationTest(true, true, true, false);
    end;
    30: begin
      VariationName := 'Terminate Finished One-shot thread';
      ThreadTerminationTest(true,  false, false, true);
    end;
    40: begin
      variationName := 'Terminate Unfinished One-shot thread';
      ThreadTerminationTest(true,  false, false, false);
    end;
    50: begin
      variationName := 'Pool Unfinished Loop thread';
      ThreadTerminationTest(false, false, false, false);
    end;
    60: begin
      variationName := 'Pool Finished Loop thread';
      ThreadTerminationTest(false, false, false, true);
    end;
    70: begin
      variationName := 'Misuse a thread by pooling an unpooled thread (should work).';
      ThreadTerminationTest(true, false, true, true);
    end;
    80: begin
      variationName := 'Misuse a thread by terminating a thread created from the pool (should work and still deregister with pool, check pool counts)';
      ThreadTerminationTest(true, true, false, true);
    end;
  end;
end;

procedure TUT_ThreadingAdvanced.EventTest;
var
  ev: TEvent;
  t1: TThread_WaitsForSignal;
  t2: TThread_SetsSignal;
begin
  VariationName := 'Event Test';
  ev := TEvent.create;
  try
    t1 := LTPM.Needthread<TThread_WaitsForSignal>(nil);
    try
      t2 := LTPM.Needthread<TThread_SetsSignal>(nil);
      try
        t1.ut := self;
        t2.ut := self;
        t1.waitingon := ev;
        t2.my_external_signal := ev;
        t1.SafeResume;
        t2.SafeResume;
        t1.SafeWaitFor;
        t2.SafeWaitFor;
        utresult := t1.result;

      finally
        LTPM.NoNeedthread(t2);
      end;
    finally
      LTPM.NoNeedthread(t1);
    end;

  finally
    ev.free;
  end;


end;

procedure TUT_ThreadingAdvanced.EventTestRepeated;
var
  ev: TEvent;
  t1: TThread_WaitsForSignal2;
  t2: TThread_SetsSignal2;
begin
  VariationName := 'Event Test';
  ev := TEvent.create;
  try
    t1 := LTPM.Needthread<TThread_WaitsForSignal2>(nil);
    try
      t2 := LTPM.Needthread<TThread_SetsSignal2>(nil);
      try
        t1.ut := self;
        t2.ut := self;
        t1.waitingon := ev;
        t2.my_external_signal := ev;
        t1.ss := t2;
        t1.SafeResume;
        t2.SafeResume;
        t1.SafeWaitFor;
        t2.SafeWaitFor;
        utresult := t1.result;

      finally
        LTPM.NoNeedthread(t2);
      end;
    finally
      LTPM.NoNeedthread(t1);
    end;

  finally
    ev.free;
  end;


end;

procedure TUT_ThreadingAdvanced.Init;
begin
  inherited;
  LTPM := TMasterThreadPOol.create;
end;

procedure TUT_ThreadingAdvanced.PoolTest1;
var
  ev: TEvent;
  t2: TThread_SetsSignal;
  s: string;
begin
  VariationName := 'Run a Thread, check stats';
  s := LTPM.NeedThreadPool(TThread_SetsSignal).GetStatMessage;
  ev := TEvent.create;
  try
    t2 := LTPM.Needthread<TThread_SetsSignal>(nil);
    try
      t2.ut := self;
      t2.my_external_signal := ev;
      t2.SafeResume;
      utresult := LTPM.NeedThreadPool(TThread_SetsSignal).GetStatMessage+' }} Originally: '+s ;
      t2.SafeWaitFor;
    finally
      LTPM.NoNeedthread(t2);
    end;
  finally
    ev.free;
  end;
end;

procedure TUT_ThreadingAdvanced.PoolTest0;
begin

  self.WaitForThreadPoolToShrink(LTPM.NeedthreadPool(TThread_SetsSignal));
  self.WaitForThreadPoolToShrink(LTPM.NeedthreadPool(TThread_WAitsForSignal));
  VariationName := 'Wait For Thread Pool to Shrink (so stats are clean before running more thread tests)';

end;


procedure TUT_ThreadingAdvanced.PoolTest2;
var
  ev: TEvent;
  t1: TThread_WaitsForSignal;
  t2: TThread_SetsSignal;
begin
  VariationName := 'Run a Thread, send to pool, check stats';
  ev := TEvent.create;
  try
    t2 := LTPM.Needthread<TThread_SetsSignal>(nil);
    try
      t2.ut := self;
      t2.my_external_signal := ev;
      t2.SafeResume;
      t2.SafeWaitFor;
    finally
      LTPM.NoNeedthread(t2);
      utresult := LTPM.NeedThreadPool(TThread_SetsSignal).GetStatMessage;
    end;
  finally
    ev.free;
  end;
end;

procedure TUT_ThreadingAdvanced.PoolTest3;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TUT_ThreadingAdvanced.PoolTest4;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TUT_ThreadingAdvanced.ThreadTerminationTest(bOneShot, bStartFromPool,
  bEndToPool, bFinishedAtEnd: boolean);
var
  tp: TThreadPoolBase;
  thr: TThread_WaitsForSignal;
  ev: TEvent;
  tm: ticker;
begin
  tm := Getticker;

  WaitForThreadPoolToShrink(LTPM.NeedthreadPool(TThread_WaitsForSignal));
  if bStartFromPool then begin
    thr := LTPM.Needthread<TTHread_WaitsForSignal>(nil);
  end else begin
    thr := TPM.Needthread<TThread_WaitsForSignal>(nil);
  end;

  thr.Loop := not bOneShot;

  ev := TEvent.Create;
  try
    try
      thr.waitingon := ev;
      thr.ut := self;

      if not bFinishedAtEnd then
        thr.DelayAfterReceivingSignal := 1000;

      thr.Start;
      Signal(ev);

      if bFinishedAtEnd then begin
        sleep(1000);
      end;

    finally
      if bEndToPool then begin
        LTPM.NoNeedthread(thr);
      end else begin
        thr.Terminate;
        thr.SafeWaitFor;
        thr.Detach;
        thr.Free;
        thr := nil;
      end;
    end;
  finally
   ev.Free;
    ev := nil;
  end;

  utresult := LTPM.NeedthreadPool(TThread_WaitsForSignal).GetStatMessage+' }} Time='+inttostr(getTimeSince(tm));

end;

procedure TUT_ThreadingAdvanced.WAitForThreadPoolToFullyInductThread;
var
  tp: TThreadPoolBase;
  i1, i2 ,i3: nativeint;
  s: string;
begin
  VariationNAme := 'Wait For Thread Pool To Shrink';
  tp := LTPM.NeedthreadPool(TThread_SetsSignal);
  s := tp.GetStatMessage;
  repeat
    tp.GetStats(i1,i2,i3);
    utresult := inttostr(i2);
  until i1 = 0;

  utresult := tp.GetStatMessage+' }} '+s



end;

procedure TUT_ThreadingAdvanced.WaitForThreadPoolToShrink(tp: TThreadPoolBase);
var
  i1, i2 ,i3: nativeint;
  tm: ticker;
  s: string;
begin
  tp.ThreadPoolTimeout := 8000;
  s := tp.GetStatMessage;
  tm := tickcount.GetTicker;
  while tp.GetStats(i1, i2, i3) > 0 do begin
    sleep(1000);
    if GetTimeSince(tm) > 12000 then begin
      utresult := 'Timeout! '+tp.GetStatMessage;
      exit;
    end;
  end;

  utresult := tp.GetStatMessage+'}}Originally:'+s;




end;

{ TThread_SetsSignal }

destructor TThread_SetsSignal.Destroy;
begin
  inherited;

end;

procedure TThread_SetsSignal.DoExecute;
begin
  inherited;
  ut.UTLog('Waiting for some time before setting event');
  sleep(time_to_wait_before_set);
  ut.UTLog('Setting event as signalled');
  my_external_signal.SetEvent;

end;

procedure TThread_SetsSignal.Init;
begin
  inherited;
  time_to_wait_before_set := 3000;

end;

{ TThread_WaitsForSignal }

procedure TThread_WaitsForSignal.DoExecute;
var
  s: string;
begin
  inherited;
  while waitingon.WaitFor(1000) = wrTimeout do begin
    ut.UTLog('Still Waiting for signal');
  end;
  case waitingon.WaitFor(1) of
    wrSignaled:     s := 'Got Signal';
    wrTimeout: s := 'Timed out';
    wrAbandoned: s := 'Abandoned';
    wrIOCompletion: s := 'IOCompletion';
{$IFDEF WINDOWS}
    wrError: s := 'Error '+inttostr(waitingon.LastError);
{$ELSE}
    wrError: s := 'Error (android provides no error details)';
{$ENDIF}
  end;
  ut.utlog(s);
  sleep(100);/// only because on slow devices some tests will flood the logs.
  result := s;

end;

{ TThread_WaitsForSignal2 }

procedure TThread_WaitsForSignal2.DoExecute;
var
  i: nativeint;
  tmStart, tmEnd: ticker;
begin
  //inherited; <<<---- DONT
  i := 0;
  tmStart := tickcount.GetTicker;
  while ss.ping_count < 1000 do begin
    if (Self.waitingon.WaitFor(3000) = wrSignaled) then begin
      ss.ping;
      i := 1000;
    end;
  end;
  tmEnd := tickcount.GetTicker;

  self.result := 'Called '+inttostr(i)+' times }} in '+inttostr(GetTimeSince(tmEnd, tmStart))+' ms.';
  if DelayAfterReceivingSignal > 0 then
    Sleep(DelayAfterReceivingSignal);

end;

{ TThread_SetsSignal2 }

procedure TThread_SetsSignal2.DoExecute;
begin
  //inherited;<<--done
  while ping_count < 1000 do begin
    my_external_signal.SetEvent;
  end;

end;

procedure TThread_SetsSignal2.Ping;
begin
  inc(ping_count);
  self.my_external_signal.ResetEvent;
end;

{ TUT_ThreadSubSet_Other }

procedure TUT_ThreadSubSet_Other.DoExecute;
var
  tmStart: ticker;
begin
  inherited;
  case Variation of
    1: PoolTest0;
    2: begin
      tmSTart := GEtTicker;
      VariationName := 'Destroy Empty Thread Pool';
      LTPM.Free;
      LTPM := nil;
      utresult := 'COMPLETED }} Time='+inttostr(GEtTimeSince(tmStart));
    end;
    3: begin
      tmSTart := GEtTicker;
      VariationName := 'Recreate Destroyed Thread Pool';
      LTPM := TMAsterThreadPool.Create;
      utresult := 'COMPLETED }} Time='+inttostr(GEtTimeSince(tmStart));
    end;
    10: PoolTest1;
    11: begin
        WAitForThreadPoolToFullyInductThread;
        VariationName := 'Thread Pool should induct thread fully';
    end;
    20: begin
        PoolTest1;
        VariationName := 'Repeat #10, should have reused same thread, verify thread pool counts.'
    end;
    21: begin
        WaitForThreadPoolToShrink(LTPM.NeedthreadPool(TThread_WaitsForSignal));
        WaitForThreadPoolToShrink(LTPM.NeedthreadPool(TThread_SetsSignal));
        WaitForThreadPoolToShrink(LTPM.NeedthreadPool(TThread_WaitsForSignal2));
        WaitForThreadPoolToShrink(LTPM.NeedthreadPool(TThread_SetsSignal2));

        VariationName := 'Wait For Thread Pool to shrink completely';
    end;
    22: begin
      tmSTart := GEtTicker;
      VariationName := 'Destroy Empty Thread Pool after it has been used.';
      try
        LTPM.Free;
        LTPM := nil;
        utresult := 'COMPLETED }} Time='+inttostr(GEtTimeSince(tmStart));
      finally
        LTPM := TMAsterThreadPool.Create;
      end;
    end;
    30: begin
      tmSTart := GEtTicker;
      PoolTest1;
      VariationName := 'Destroy Thread POol Immediately after use.';
      try
        LTPM.Free;
        LTPM := nil;
        utresult := 'COMPLETED }} Time='+inttostr(GEtTimeSince(tmStart));
      finally
        LTPM := TMAsterThreadPool.Create;
      end;
    end;
    40: EventTest;
    50: EventTestRepeated;
  end;
end;

initialization
  UTF.RegisterClass(TUT_ThreadSubset_Termination);
  UTF.RegisterClass(TUT_ThreadSubSet_Other);

end.
