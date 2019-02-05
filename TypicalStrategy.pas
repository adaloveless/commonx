unit TypicalStrategy;

{$DEFINE MT_WATCH}

interface

uses
  problemdomain_lambot, tickcount, debug,
  commandprocessor, managedthread, typex, jsonhelpers, rec2json,
  systemx, stringx, numbers, bittrex, shapeshiftdb, sysutils;

const
  LOCAL_PICK_STRATID = 101;

type
  TTypicalStrategy = class(TStrategy)
  private
    thrWatching: TManagedThread;
    manageCOmmands: TCommandList<TManageStrategy>;
    pickCommands: TCommandList<TPickStrategy>;
    FPickTradeInterval: ticker;
    procedure SyncManageCommands;
    procedure SyncPickCommands;
    function IndexOfManageCommand(tradeId: int64): ni;
    function IndexOfpickCommand(coin: string): ni;
    function FindManageCommand(tradeid: int64): TManageStrategy;
    function FindPickCommand(coin: string): TPickStrategy;
    function HasManageCommand(tradeid: int64): boolean;
    function HasPickCommand(coin: string): boolean;

  public

    toSpend: double;
    availFunds: double;
    function CreatePick(strat: ni): TPickStrategy;
    function CreateManage(strat: ni): TManageStrategy;
    procedure PickTrades;override;
    function ManageTrades(Liquidate: boolean; thr: TManagedThread): boolean;override;
    function ManageTrade(var p: TOpenPosition; Liquidate: boolean): boolean;
    procedure BeginPickTrade(ms: TMarketScore; var pi: TPickInputs);
    procedure EndPickTrade(var ms: TMarketScore; var pi: TPickInputs);
    procedure AfterConstruction; override;
    procedure Init; override;
    procedure Detach; override;
    property PickTradeInterval: ticker read FPickTradeInterval write FPickTradeInterval;

  end;



implementation

{$IFNDEF NO_STRATS}
uses
// strategy_moonrise, strategy_allin, strategy_socialvelocity;
  strategy_enroyd, strategy_enroyd_tusd, strategy_tusd_usdt;
{$ENDIF}


{ TTypicalStrategy }

procedure TTypicalStrategy.AfterConstruction;
begin
  inherited;


end;

function TTypicalStrategy.CreateManage(strat: ni): TManageStrategy;
begin
  result := nil;
{$IFNDEF NO_STRATS}
  case strat of
    STRATID_ENROYD: result := TManage_Enroyd.create;
    STRATID_ENROYD_TUSD: result := TManage_Enroyd_TUSD.create;
    STRATID_TUSD_USDT: result := TManage_Enroyd_TUSD.create;
  else
    raise ECritical.create('StratID not found '+inttostr(strat));
  end;
{$ENDIF}

//  result.strat := self;
//  result.pdTrades := self.pdTrades;

end;

function TTypicalStrategy.CreatePick(strat: ni): TPickStrategy;
begin
  result := nil;
{$IFNDEF NO_STRATS}
  case strat of
    STRATID_ENROYD: result := TPick_Enroyd.create;
    STRATID_ENROYD_TUSD: result := TPick_Enroyd_TUSD.create;
  end;
{$ENDIF}
//  result.strat := self;
//  result.pdTrades := self.pdTrades;
end;

procedure TTypicalStrategy.Detach;
begin
  if detached then exit;

  manageCommands.WaitForAll;
  manageCommands.ClearAndDestroyCommands;
  manageCOmmands.Free;
  manageCommands := nil;

  pickCommands.WaitForAll;
  pickCommands.ClearAndDestroyCommands;
  pickCommands.free;
  pickCommands := nil;


  inherited;

end;

function TTypicalStrategy.FindManageCommand(tradeid: int64): TManageStrategy;
var
  t: ni;
begin
  t := IndexOfManageCommand(tradeid);
  if t<0 then
    exit(nil);
  result := manageCOmmands[t];

end;

function TTypicalStrategy.FindPickCommand(coin: string): TPickStrategy;
var
  t: ni;
begin
  t := IndexOfPickCommand(coin);
  if t<0 then
    exit(nil);
  result := pickCOmmands[t];

end;

function TTypicalStrategy.HasManageCommand(tradeid: int64): boolean;
begin
  result := IndexOfManageCommand(tradeid) >=0;
end;

function TTypicalStrategy.HasPickCommand(coin: string): boolean;
begin
  result := IndexOfPickCommand(coin) >=0;

end;

function TTypicalStrategy.IndexOfManageCommand(tradeId: int64): ni;
var
  t: ni;
begin
  for t:= 0 to manageCOmmands.count-1 do begin
    if managecommands[t].p.id = tradeid then
      exit(t);
  end;

  exit(-1);

end;

function TTypicalStrategy.IndexOfpickCommand(coin: string): ni;
var
  t: ni;
begin
  for t:= 0 to pickCommands.count-1 do begin
    if comparetext(pickcommands[t].ms.coin, coin) =0 then
      exit(t);
  end;

  exit(-1);

end;

procedure TTypicalStrategy.Init;
begin
  inherited;
  PickTradeInterval := 600000;
  pickCommands := TCommandList<TPickStrategy>.create;
  manageCommands := TCommandList<TManageStrategy>.create;

end;

function TTypicalStrategy.ManageTrade(var p: TOpenPosition;
  Liquidate: boolean): boolean;
var
  m: TManageStrategy;
begin
  result := false;
  m := nil;
  try
    SyncManageCommands;
{$IFDEF PERSIST_COMMANDS}
    m := FindCommand(p.id);
    if m <> nil then begin
      if not m.Started then begin
        m.p := p;
        m.RaiseExceptions := false;
        if (p.lastRefresh>0) and (gettimesince(p.lastRefresh) < p.refreshRate) then begin
        end else begin
          m.Start(ManageCOmmands);
        end;
      end;
      if m.IsComplete then begin
        p := m.p^;
        m.ForceCloseID := pdTrades.ForceCloseFlag;
        m.Liquidate := liquidate;
        m.Start(ManageCOmmands);
      end;
    end;
{$ELSE}
    {$IFDEF MT_WATCH}
    m := FindManageCommand(p.id);
    if m <> nil then begin
      if m.IsComplete then begin
        p := m.p;
        p.lastrefresh := getticker;
        m.Liquidate := liquidate;
        manageCOmmands.Remove(m);
        m.waitfor;
        m.free;
        m := nil;
        result := true;
        SyncManageCommands;
        m := FindManageCommand(p.id);
      end else
      if (m <> nil) and (not m.Started) then begin
        m.p := p;
        m.RaiseExceptions := false;
        if (p.lastRefresh>0) and (gettimesince(p.lastRefresh) < p.refreshRate) then begin
        end else begin
//          m.Start(ManageProcesssor);
          p.lastRefresh := getticker;
          m.p := p;
          if pdTrades.fundValue = 0 then
            pdTrades.UpdateFundValue;
          m.FundValue := self.pdTrades.fundValue;
          m.RaiseExceptions := false;
          m.Liquidate := liquidate;
          m.Start(ManageProcesssor);
//          result := true;
        end;
      end;
    end;
    {$ELSE}
    m := CreateManage;
    m.Liquidate := liquidate;
    m.RaiseExceptions := false;
    m.p := @p;
    m.TotalFundValue := pdTrades.fundValue;
    m.Start(ManageProcessor);
    m.WaitFor;
    p := m.p;
    {$ENDIF}
{$ENDIF}

  finally
{$IFNDEF PERSIST_COMMANDS}
    {$IFNDEF MT_WATCH}
      m.free;
    {$ENDIF}
{$ENDIF}
  end;
end;

function TTypicalStrategy.ManageTrades(Liquidate: boolean; thr: TManagedThread): boolean;
var
  p,pp: TOpenPosition;
  j: IJSONHolder;
  t: ni;
  id: int64;
  bIsDust: boolean;
  bCloseNow: boolean;
  v: variant;
  ob, ob2: TOrderBookAnalysis;
  pct: double;
  moondiv: double;
  ratio: double;
  avg: double;
  peak: double;
  ca: TChartAnalysis;
  slip1, slip2: TOrderBookAnalysis;
//  crv: TConvertedRateAndValue;
  cv,ttl: double;
begin
  result := false;
  thrWatching := thr;
  with pdTRades do begin
    if assigned(thrWatching) then thrWatching.Status := 'manage trades';

    if btcMoon then
      moonDiv := 2
    else
      moonDiv := 1;

    self.pdTrades.RollUpSignalBatch;

    moonDiv := 1+(1-climateAssessment);
    moonDiv := lesserof(2,greaterof(1,moonDiv));


    if not AllowTrades then
      exit(false);
    result := false;
    j := nil;
    //refresh DB of positions
    Fetch;
    //iterate through positions

    moons := 0;
    closed := 0;
    bags := 0;

    debug.log('Managing '+inttostr(length(trades))+' trades');
    for t:= 0 to high(trades) do begin
      debug.log('Managing ['+inttostr(t)+'] '+p.coin);

      pp := trades[t];
      p := pp;
      result := ManageTrade(p, liquidate);
      trades[t] := p;
      if result then begin
        ssdb.CoinStat('btc', p.coin, 'position', RecToJSON(@p, typeinfo(TOpenPosition)));
        ssdb.Saveposition(p);
        if result then
          break;
      end;
    end;
  end;

end;

procedure TTypicalStrategy.BeginPickTrade(ms: TMarketScore; var pi: TPickInputs);
var
  p: TPickStrategy;
begin
  p := nil;
  try
//    SyncPickCommands;
{$IFDEF PERSIST_COMMANDS}
{$ELSE}
    {$IFDEF MT_WATCH}
    p := CreatePick(LOCAL_PICK_STRATID);
    if p <> nil then begin
      pickcommands.add(p);
      p.ms := ms;
      p.RaiseExceptions := false;
      p.Start;
    end;
    {$ELSE}
    {$ENDIF}
{$ENDIF}

  finally
{$IFNDEF PERSIST_COMMANDS}
    {$IFNDEF MT_WATCH}
      m.free;
    {$ENDIF}
{$ENDIF}
  end;
end;


procedure TTypicalStrategy.EndPickTrade(var ms: TMarketScore; var pi: TPickInputs);
var
  p: TPickStrategy;
begin
  p := nil;
  try
{$IFDEF PERSIST_COMMANDS}
{$ELSE}
    {$IFDEF MT_WATCH}
    p := FindpickCommand(ms.coin);
    if p <> nil then begin
      while not p.waitfor(1000) do
        Debug.Log('waiting on pick '+ms.coin);
      p.WaitFor;
      ms := p.ms;
      pickCOmmands.Remove(p);
      //p.waitfor;
      p.free;
      p := nil;
    end;
    {$ELSE}
    {$ENDIF}
{$ENDIF}

  finally
{$IFNDEF PERSIST_COMMANDS}
    {$IFNDEF MT_WATCH}
      m.free;
    {$ENDIF}
{$ENDIF}
  end;
end;

procedure TTypicalStrategy.PickTrades;
var
  t: ni;
  btc: double;
  s: string;
  a: TArray<string>;
  f,ff: double;
  p: double;
  pms: PMarketScore;
  toSpend: double;
  ll,r,u: double;
  iToCreate: ni;
  bRefetch: boolean;
  ob, ob2: TorderBookAnalysis;
  slip1,slip2: TOrderBookAnalysis;
  y1,y2: double;
  anal: TChartAnalysis;
  v,vp,tp: double;
  rTemp: double;
  tt: ni;
  tS: double;
  strategy: double;
  TypicalSwing: double;
  bFoundSizing: boolean;
  spreadReq: double;
  prevms: PMarketScore;
  bCrossBump: boolean;
  usd: double;
  obusd: TOrderbookAnalysis;
  analusd: TChartAnalysis;
  availFunds: double;
  totalAvailFunds: double;
  tmSince, tmInterval: ticker;
  signal, signalNormal: double;
  center: double;
  coin: string;
  chartRecent: TChartAnalysis;
  chartLessRecent: TChartAnalysis;
  pi: TPickInputs;
BEGIN
  with pdTrades do begin
  if not allowtrades then
    exit;
  btx.RefreshFunds;
  btc := btx.AvailableFunds['btc'].Amount;
  lastTradepicktime := tickcount.getticker;

  t := 0;
  iToCreate := MAX_OPEN_TRADES - length(trades);
  if (length(watching) = 0) or (GetTimeSince(LastWatchPickTime) >  (1000*60*60*12)) then begin
    self.pickCommands.WaitForAll;
    self.pickCommands.ClearAndDestroyCommands;
    PickToScan;
  end;

  obusd := btx.GetOrderBookAnalysis('USDT', 'BTC', btIAmSelling, btx.totalFundsValue, false);
  analusd := btx.GetChartAnalysis('USDT', 'BTC', cp60);
  analusd.Calc(24*7);
  ll := greaterof(analusd.lo,0);
  u := analusd.hi;
  v := analusd.Volatility;
  r := obusd.crav.convertedRate;
  usd := r;
  if v = 0 then
    tp := 0
  else
    tp := (r-ll) / v;
  bitcoinClimate := greaterof(0.0,lesserof(1.0,tp));
//  bitCoinClimate := 0.0;

  btcMoon := (bitcoinClimate > 0.95);


  //UpdateClimateAssessment;
  //pdTrades.Fetch;

   try
      //if btc > (btx.totalFundsValue / MAX_OPEN_TRADES) then
      Fetch;

      totalAvailFunds := greaterof(btx.totalFundsValue(* * climateAssessment*), BTX_DUST*2);

      t := 0;
      if length(watching) > 0 then
      while true (*btc > (btx.totalFundsValue / MAX_OPEN_TRADES)*) do begin
        if t = length(watching) then
          break;

        if shutdown then break;

        if not (FindOpenPosition(watching[t].coin, 0) = nil) then begin
          watching[t].Msg := 'position already open';
        end else begin
          //prevms := FindPrevMarketScore(watching[t].coin);
          tmSince := gettimesince(watching[t].scantime);
          tmInterval := watching[t].RefreshRate;
          coin := watching[t].coin;
          if (watching[t].scantime=0) or ((tmSince > tmInterval)) then begin
            watching[t].scantime := Getticker;
            pi.toSpend := greaterof(totalAvailFunds / TYPICAL_OPEN_TRADES, BTX_DUST*2);
            BeginPickTrade(watching[t], pi);
          end;
        end;
        inc(t);

      end;

      t := 0;
      if length(watching) > 0 then
      while true (*btc > (btx.totalFundsValue / MAX_OPEN_TRADES)*) do begin
        EndPickTrade(watching[t], pi);
        inc(t);
        if t = length(watching) then
          break;

      end;

   finally
      previousmarketscores := marketscores;
   end;
  end;
end;

procedure TTypicalStrategy.SyncManageCommands;
var
  c: TManageStrategy;
  t: ni;
  p: POpenPosition;
begin
{$IFNDEF PERSIST_COMMANDS}
  {$IFNDEF MT_WATCH}
  exit;
  {$ENDIF}
{$ENDIF}
  //remove commands not in trades
  for t:= managecommands.count-1 downto 0 do begin
    c := managecommands[t];
    p := pdTrades.FindOpenPosition(c.p.id);

    if p = nil then begin
      if c.IsComplete then begin
        manageCommands.Remove(c);
        if assigned(thrWatching) then thrWatching.Status := 'removing '+c.Status;
        if c.Started or c.IsComplete then
          c.WaitFor;
        c.Free;
        c := nil;
      end;
    end;
  end;

  //add commands for new trades
  for t:= 0 to high(pdTrades.trades) do begin
    p := @pdTrades.trades[t];
    c := self.FindManageCommand(p.id);
    if c = nil then begin
      if assigned(thrWatching) then thrWatching.Status := 'adding '+p.tradename;
      c := CreateManage(p.strategy);
      c.p := p^;
      managecommands.add(c);
    end;
  end;


end;

procedure TTypicalStrategy.SyncPickCommands;
var
  c: TPickStrategy;
  t: ni;
  ms: PMarketScore;
begin
{$IFNDEF PERSIST_COMMANDS}
  {$IFNDEF MT_WATCH}
  exit;
  {$ENDIF}
{$ENDIF}
  //remove commands not in trades
(*  for t:= pickcommands.count-1 downto 0 do begin
    c := pickcommands[t];
    ms := pdTrades.FindMarketScore(c.ms.coin);

    if ms = nil then begin
      if c.IsComplete then begin
        pickCommands.Remove(c);
        c.WaitFor;
        c.Free;
        c := nil;
      end;
    end;
  end;*)

  //add commands for new trades
  for t:= 0 to high(pdTrades.marketscores) do begin
    ms := @pdTrades.marketscores[t];
    c := self.FindPickCommand(ms.coin);
    if c = nil then begin
      c := CreatePick(LOCAL_PICK_STRATID);
      c.ms := ms^;
      pickcommands.add(c);
    end;
  end;

end;

end.
