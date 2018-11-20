unit ShapeShiftDB;

{x$DEFINE USE_TOR}
interface

uses
  typex, systemx, classes, commandprocessor, numbers, variants, debug, tickcount, orderlyinit, better_collections, sharedobject, BitStampAPI, ShapeShiftAPI, rdtpdb, betterobject, abstractdb, storageenginetypes, sysutils, stringx, mysqlstoragestring, rdtpkeybotclient, applicationparams, jsonhelpers, rec2json, maths, geometry, twitter, data.db, fleximath;

const
  TARGET_DB_VERSION = 2.0;
  MAX_RDTP_CONNECTIONS = 5;
  BUCKET_COUNT = 20;
  ID_SESSIONID = 100;
  ID_TRANSACTION = 1001;
  ID_LEDGER=1002;
  POSITIONSTATE_WATCHING = 0;
  POSITIONSTATE_REBUY = 5;
  POSITIONSTATE_BUYING = 10;
  POSITIONSTATE_BOUGHT = 20;
  POSITIONSTATE_SELLING = 30;
  POSITIONSTATE_POSTSELL1 = 31;
  POSITIONSTATE_POSTSELL2 = 32;
  POSITIONSTATE_SOLD = 40;
  POSITIONSTATE_COMPLETED = 50;
  POSITIONSTATE_CANCEL = 60;
    POSITIONSTATE_ABANDONED_DUST = 70;
  BEST_REFRESH_TIME = 8000;

type
  Tcmd_UpgradeCoinStats = class(TCommand)
  protected
    db: TRDTPDB;
    procedure DoExecute; override;
  public
    coin: string;
    procedure InitExpense; override;

  end;

  Tcmd_UpgradeDatabase = class(TCommand)
  protected
    db: TRDTPDB;
    procedure UpgradeTo2;
    procedure DoExecute; override;
  public
    startVersion: single;
    procedure InitExpense; override;


  end;

  TChartRec = record
    BaseCoin, MarketCoin: string;
    ts: TDateTime;
    res: ni;
    o,h,l,c,v,bv: double;
  end;
  TTwitterFollow = record
    coin: string;
    twitter: string;
    procedure Init;
  end;
  TCoinStat = record
    basecoin: string;
    marketcoin: string;
    id: int64;
    ts: TDateTime;
    val: string;
    fval: double;

  end;
  TCoinStatReturn = record
    ts: TDateTime;
    id: int64;
  end;
  TTweet = record
    id: int64;
    created_on: TDateTime;
    coin: string;
    tweet: string;
    json: string;
    found_on: TDateTime;
    function DBValueString: string;
  end;
  TStatRec = record
    id: int64;
    msg: string;
    messageClass: string;
    timestamp: TDateTime;
    basecoin, marketcoin: string;
  end;
  TDirection = (dirFlat, dirUp, dirDown);
  TClimateReport = record
    timestamp: TDateTime;
    reportName: string;
    value: double;
    value2: double;
    stringValue: string;
    valid: boolean;
    function Age: TDateTime;
    procedure Init;
  end;

  TDogHouse = record
    coin: string;
    rate: double;
    tm: double;
  end;


  TPriceLine = record
    MinAsk: double;
    MaxAsk: double;
    AvgAsk: double;
    MinBid: double;
    MaxBid: double;
    AvgBid: double;
    EnterBid: double;
    EnterAsk: double;
    Samples: ni;
    procedure init;
    procedure ApplyPrices(bid, ask: double);

  end;

  TSignalTragectory = record
  private
    procedure SetAvgMaxTrajectory(const Value: double);
    function GetAvgMaxTrajectory: double;
  public
    basecoin: string;
    marketcoin: string;
    xSignalMin: double;
    SignalMax: double;
    MedianTrajectory: double;
    MeanTrajectory: double;
    MaxMaxTrajectory: double;
    MinMaxTrajectory: double;
    MaxMinTrajectory: double;
    MinMinTrajectory: double;
    FAvgMaxTrajectory: double;
    FAvgMinTrajectory: double;
    AdjustedTrajectory: double;
    Samples: ni;
    function ToJSON: IHolder<TJSON>;
    procedure FromJson(j: TJSON);
    procedure init(bucketindex: ni);
    procedure ApplyTraj(traj: TSignalTragectory);
    procedure ApplyPriceLine(pl: TPriceLine);

    function AvgMinTrajectory: double;
    procedure Synthesize(a,b: TSignalTragectory; rBlend: double);
    property AvgMaxTrajectory: double read GetAvgMaxTrajectory write SetAvgMaxTrajectory;

  end;
  TBuckets = record
  private
    fr: TFitResults;
    function GetLowestBucket: ni;
    function GetHighestBucket: ni;
    function CalcFit: TFitResults;
  public
    a: array of TSignalTragectory;
    function FindBucketIndex(sig: double): ni;
    function IsUsable: boolean;
    function IsSemiUsable: boolean;
    function GetUsableSignal(sigIn: double): double;
    function GetLowestAmp: double;
    function GetHighestAmp: double;
    function FitTrajFromSignal(sig: double): double;
  end;



  TPositionProfits = record
    coin: string;
    bought: double;
    sold: double;
    BTCProfits: double;
    function PercentProfits: double;
  end;

  TClosedPosition = record
  public
    id: int64;
    coin: string;
    strat: ni;
    boughtAt: double;
    soldAt: double;
    buytime: TDateTime;
    selltime: TDateTime;
  end;

  TOpenPosition = record
  private
    function GetProfit: double;
    function GetProfitPercent: double;
    function GetPercentAboveLowest: double;
    function GetPercentBelowHighest: double;
    function GetCurrentBaseValue: double;
    procedure SetCurrentRate(const Value: double);
    procedure SetCurrentValue(const Value: double);
    function GetPeakProfitPercent: double;
    procedure SetState(const Value: integer);
    function GetPeakPercentAboveLowest: double;
    procedure SetBuyRate(const Value: double);
    function GetStateXJSON: IHolder<TJSON>;
    procedure SetStateXJSON(const Value: IHolder<TJSON>);
    function getrefreshRate: ticker;
    procedure SetRefreshRate(const Value: ticker);
    function GetLastStateChange: double;

  public
    id: int64;
    coin: string;
    completed: boolean;
    buyCost: double;
    buyamount: double;
    fbuyrate: double;
    created: TDateTime;
    buytime: TDateTime;
    sellTime: TDateTime;

    sellrate: double;

    peakValue: double;
    FcurrentValue: double;
    lowestValue: double;

    peakRate: double;
    FcurrentRate: double;
    lowestRate: double;

    TypicalSwing: double;
    HistoricalBottom: double;
    UnitsToProcess: double;

    BuyUUID: string;
    SellUUID: string;
    Fstate: integer;
    expires: TDateTime;
    lastRefresh: ticker;
    direction: TDirection;
    priority: double;
    FlastStateChange: double;
    effectiveTakeLine: double;
    effectiveMinimizeLossLine: double;
    effectiveDropline: double;
    strategy: ni;
    signal: double;
    status: string;
    statex: string;
    because: string;
    FrefreshRate: ticker;
    property Profit: double read GetProfit;
    property ProfitPercent: double read GetProfitPercent;
    property PeakProfitPercent: double read GetPeakProfitPercent;
    property PeakPercentAboveLowest: double read GetPeakPercentAboveLowest;
    property PercentAboveLowest: double read GetPercentAboveLowest;
    property PercentBelowHighest: double read GetPercentBelowHighest;
    property CurrentBaseValue: double read GetCurrentBaseValue;
    property CurrentRate: double read FCurrentRate write SetCurrentRate;
    property CurrentValue: double read FCurrentValue write SetCurrentValue;
    function TimeUntilRefresh: ticker;
    property RefreshRate: ticker read GEtRefreshRate write SetRefreshRate;
    function TimeInState: ticker;
    property LastStatechange: double read GetLastStateChange write FLastStateChange;
    function BreakEvenPercent: double;
    function BreakEvenAboveLowest: double;
    property state: integer read Fstate write SetState;
    function CurrentPositionInVolatility: double;
    function CurrentPositionInProfitability: double;
    property buyrate: double read FBuyRate write SetBuyRate;
    property StateXJson: IHolder<TJSON> read GetStateXJSON write SetStateXJSON;
    procedure Init;
    function TradeName: string;

  end;

  POpenPosition = ^TopenPosition;





  TQuotesAndVolume = record
    quotes: IHolder<TSERowSet>;
    volume: IHolder<TSERowset>;
  end;
  TShapeShiftDB = class;//forward
  Tcmd_PushVolume = class(Tcommand)
  strict private
    db: TShapeShiftDB;
  public
    cc: string;
    iSampleIDMax: int64;
    procedure DoExecute; override;
    procedure InitExpense;override;
  end;

  TShapeShiftDB = class(TBetterObject)
  strict
  private
    FSimulated: boolean;
    function GettablePrefix: string;
  protected
  public
    accountid: int64;
//    kb: TKeyBotClient;
    function FindRecentPositionTime(sBase, sMarket: string; istrategy: ni): TDateTime;
    procedure ValidateAccount;
    function RollupIncompleteTrades: int64;
    procedure PushVolumeToDB(sampleidmax: int64; cc: string);overload;
    procedure PushTransactionsToDB(sampleid: int64);
    procedure PushVolumeToDB(isampleidmax: int64);overload;

    procedure connect;
    procedure disconnect;
    procedure Detach;override;

    procedure Saveposition(p: TopenPosition);

    function GetTransactions(sampleidmax: int64): TSERowset;
    function GetOpenPositions(sampleidmax: int64): TSERowset;


    function GetCurrencies(OnlyTradable: boolean=false; AILive: boolean=false; AISim: boolean=false): TSERowset;
    procedure PushQuotesToDB;

    function GetQuotes(cFrom, cto: string; iSampleIDLimit: int64 = -1;iSampleIDStart: int64 = 0): TQuotesAndVolume;
    function GetHoldings(maxsampleid: int64): TSERowSet;
    procedure RecordPosition(p: POpenPosition);
    procedure BeginClosePosition(original_txid, new_txid: int64);
    procedure BeginOpenPosition(original_txid, new_txid: int64);
    property Simulated: boolean read FSimulated write FSimulated;
    property TablePrefix: string read GettablePrefix;
    function GetValueInBTC(cc: string; amt: double; atSample: int64=-1): double;
    function GetValueInUSD(cc: string; amt: double; atSample: int64=-1): double;
    function GetValueInCC(ccIn: string; ccOut: string; amt: double; atSample: int64=-1): double;

    function GetHistoricalSpread(ccFrom, ccTo: string; atSample: int64=-1): double;
    function GetSingleQuote(ccFrom, ccTo: string; atSample: int64): double;
    procedure changepositionState(id: int64; state: nativeint);
    procedure RecordFundValue(v: double);
    function GetPositionProfits(p: TOpenPosition): TPositionProfits;
    procedure AddToDoghouse(sCoin: string; rate: double);
    function GetFromDogHouse(sCoin: string): TDogHouse;
    function GetFundHistory(): IHolder<TSERowset>;
    function Login(user, pass: string): int64;
    procedure ValidateIDs(db: TRDTPDB);
    function GetClimateReport(reportName: string): TClimateReport;
    procedure SaveClimateReport(report: TClimateReport);
    procedure CoinLog(sBase, sMarket, sMessage: string; openid: int64 = 0);
    function CoinStat(sBase, sMarket, sMessageClass, sMessage: string; ts: TDateTime = 0.0; reuseid: int64 = -1; openid: int64 = 0; bDelayed: boolean = true): TCoinStatReturn;
    function GetCoinStatValueInsert(sBase, sMarket, sMessageClass, sMessage: string; ts: TDateTime = 0.0; reuseid: int64 = -1; openid: int64 = 0): string;
    procedure ChartUpdate(a: TArray<TChartRec>; behind: boolean);
    function GetStatIDs(starttime: TDateTime; sBase, sMarket: string; sClasses: array of string): TArray<int64>;
    function GetStatsToRollUp(cointables: TArray<string>): TArray<TStatRec>;
    function GetStatsToRollUpAlltables: TArray<TStatRec>;
    function GetCoinStatTables(sCoinFilter: String = ''; sMessageClassfilter: string = ''): TArray<string>;
    function GetStats(starttime: TDateTime; sBase, sMarket: string; sClasses: array of string): TArray<TStatRec>;
    function GetStatByID(id: int64): TStatRec;
    procedure RollUpTrajectory(sBase, sMarketCoin: string);
    function LoadSignalBuckets(sBase, sMArketCoin: string): TBuckets;
    function GetBannedCoins: string;
    function AllowedToSell(sCoin: string): boolean;
    procedure ForceClose(id: ni);
    function IsForcedClose(id: ni): boolean;
    procedure SaveTweets(sCoin: string; j: IHolder<TJSON>);
    procedure LinkTwitter(sCoin: string; sTwitterhandle: string);
    function GetDistinctCoins: TArray<string>;
    function GetClosedPositions(iLimit: ni = 400): TArray<TClosedPosition>;
    function GetTopPerformersInMessageClass(sMessageClass: string; days_peak_range: single): IHolder<TSERowSet>;
    function GetCoinStatClasses(sCoin: string): IHolder<TSERowSet>;
    function GetCoinStatsRange(sCoin: string; sMessageClass: string; dtStart, dtEnd: TDateTime; bNewestFirst: boolean = true): IHolder<TSERowset>;overload;
    function GetCoinStatNewestF(sCoin: string; sMessageClass: string): double;
    function GetCoinStatsRangeA(sCoin: string; sMessageClass: string; dtStart, dtEnd: TDateTime; bNewestFirst: boolean = true): TArray<TCoinStat>;overload;
    function GetCoinStatsRangeS(sCoin: string; sMessageClass: string; dtStart, dtEnd: TDateTime; bNewestFirst: boolean = true): TCoinStat;overload;
    function GetCoinStatPeak(sCoin: string; sMessageClass: string; dtStart, dtEnd: TDateTime): TCoinStat;overload;
    function GetCoinStatDelta(sCoin: string; sMessageClass: string): TCoinStat;overload;
    function GetCoinLogRange(sCoin: string; dtStart, dtEnd: TDateTime): TSERowSet;
    function TXIDtoCoin(id: int64): string;
    function GetChartRange(base,market: string; period: ni; dtStart, dtEnd: TDateTime): TArray<TChartRec>;
    function GetSocialScore(sCoin: string; dtFirstAfter: TDatetime = 0.0): double;
    function GetSocialVelocity(sCoin: string; dtDistance: TDateTime = 1.0; ending: TDateTime = 0.0): double;
    function GetBestSocialVelocity(sCoin: string; dtDistance: TDateTime = 1.0): double;
    function ReadQuery(sQuery: string): IHolder<TSERowSet>;
    procedure WriteQuery(sQuery: string);
    procedure WriteQueryMultiTable(sQuery: string);
    function ReadQueryMultiTable(sQuery: string; postorder: string): IHolder<TSERowSet>;
    function CoinStatTable(sCoin: string; sMessageClass: string): string;
    function DatabaseNeedsUpgrade: boolean;
    function BeginUpgradeDatabase: TCommand;
    procedure EndUpgradeDAtabase(c: TCommand);
    function GetAnything(sAddr: string; tmStart: TDateTime; tmEnd: TDateTime): IHolder<TSERowSet>;

  end;


  TShapeShiftDBProvider = class(TSharedobject)
  private
    pool: TBetterList<TShapeShiftDB>;
  public
    procedure Init;override;
    procedure Detach;override;
    function Need(simulated: boolean): TshapeShiftDB;
    procedure NoNeed(db: TShapeShiftDB);
  end;

  TRDTPDBPRovider = class(TsharedObject)
  private
    pool: TBetterList<TRDTPDB>;
    issued: ni;
  public
    procedure Init;override;
    procedure Detach;override;
    function Need: TRDTPDB;
    procedure NoNeed(var db: TRDTPDB);
  end;


function FindSignalBucket(a: Tarray<TSignalTragectory>; sig: double): TSignalTragectory;

function NewDB: TRDTPDB;
function StatTableToCoin(sTable: string): string;

var
  prov: TShapeShiftDBProvider;
  dbprov: TRDTPDBProvider;
  GAccountid: int64 = -1;


implementation

function StatTableToCoin(sTable: string): string;
var
  l,r: string;
begin
  SplitString(sTable, '_', l, r);
  SplitString(r,'_', result, r);

end;


function LoadUserName: string;
begin
  result := applicationparams.APGet('DB_USER', '');
end;

function LoadPass: string;
begin
  result := applicationparams.APGet('DB_PASS', '');
end;




function NewDB: TRDTPDB;
begin
  result := TRDTPDB.create;
{$IFDEF USE_TCP}
  result.UseTcp := true;
  result.MiddleWarePort := '235';

  {$IFNDEF USE_TOR}
{$ifndef USE_INTRANET}
    result.MiddleWareAddress := '173.165.239.132';
{$ELSE}
    result.MiddleWareAddress := '172.16.0.193';
{$ENDIF}
  {$ELSE}
    result.UseTor := true;
    result.MiddleWareAddress := 'djvideos4lvw5meh.onion'
  {$ENDIF}
{$ELSE}
  {x$DEFINE DEV_SERVER}
  {$IFDEF DEV_SERVER}
    result.MiddleWareAddress := '127.0.0.1';
  {$ELSE}
{$ifndef USE_INTRANET}
    result.MWHost := '173.165.239.132';
{$ELSE}
    result.MWHost  := '172.16.0.193';
{$ENDIF}
  {$ENDIF}

  result.MWEndpoint := '235';
{$ENDIF}

{$IFDEF DEV_SERVER}
  result.Connect('172.16.0.193', 'shapeshift', 'root', '', '');
{$ELSE}
  result.Connect('127.0.0.1', 'shapeshift', 'root', '', '');
{$ENDIF}








end;



{ TShapeShiftDB }

procedure TShapeShiftDB.AddToDoghouse(sCoin: string; rate: double);
var
  db: TRDTPDB;
begin
  db := dbprov.Need();
  try
    db.WriteBehind('insert ignore into doghouse values ("'+sCoin+'", '+floattostr(now)+', '+floattostr(rate)+')');
    db.WriteBehind(' update doghouse set rate='+floattostr(rate)+', timestamp='+floattostr(now())+' where coin = "'+sCoin+'"');

  finally
    dbprov.noneed(db);
  end;
end;

function TShapeShiftDB.AllowedToSell(sCoin: string): boolean;
var
  db: TRDTPDB;
begin

  if accountid = 1 then
    exit(true)
  else begin
    db := dbprov.need;
    try
      result := db.functionquery('select count(*) from position p where marketcoin="'+sCoin+'" and state >= 20 and state <= 29 and p.accountid=1',0)=0;
    finally
      dbprov.noneed(db);
    end;

  end;
end;

procedure TShapeShiftDB.BeginClosePosition(original_txid, new_txid: int64);
var
  db: TRDTPDB;
begin
  ValidateAccount;
  db := dbprov.Need();
  try
//    db.WriteQuery(' update position set '+
//                  ' transactionid='+new_txid.tostring+', '+
//                  ' closeid='+new_txid.tostring+
//                  ' WHERE transactionid='+original_txid.tostring);
  finally
    dbprov.noneed(db);
  end;
end;

procedure TShapeShiftDB.BeginOpenPosition(original_txid, new_txid: int64);
var
  db: TRDTPDB;
begin
  ValidateAccount;
  db := dbprov.Need();
  try
//    db.WriteQuery(' update position set '+
//                  ' transactionid='+new_txid.tostring+', '+
//                  ' closeid='+new_txid.tostring+
//                  ' WHERE transactionid='+original_txid.tostring);
  finally
    dbprov.noneed(db);
  end;
end;

procedure TShapeShiftDB.changepositionState(id: int64; state: nativeint);
var
  db: TRDTPDB;
begin
  db := dbprov.need;
  try
    db.WriteQuery('update position set state='+state.tostring+' where transactionid='+id.tostring);
  finally
    dbprov.NoNeed(db);
  end;

end;



procedure TShapeShiftDB.ChartUpdate(a: TArray<TChartRec>; behind: boolean);
var
  db: TRDTPDB;
  id: int64;
  cr: TChartRec;
  sql: string;
  sqlRec: string;
  t: ni;
begin
  db := dbprov.Need;
  try
    sql := 'insert ignore into chart values';

    if length(a) = 0 then
      exit;




    for t:= 0 to high(a) do begin
      cr := a[t];
      sqlRec := '('+
                    GetStorageString(varString, cr.BaseCoin)+','+
                    GetStorageString(varString, cr.MarketCoin)+','+
                    GetStorageString(varDate,   cr.ts)+','+
                    GetStorageString(varinteger,cr.res)+','+
                    GetStorageString(varDouble, cr.o)+','+
                    GetStorageString(varDouble, cr.h)+','+
                    GetStorageString(varDouble, cr.l)+','+
                    GetStorageString(varDouble, cr.c)+','+
                    GetStorageString(varDouble, cr.v)+','+
                    GetStorageString(varDouble, cr.bv)+')';

      if t > 0 then
        sql := sql +','+sqlRec
      else
        sql := sql + sqlRec;
    end;



    if behind then
      db.writebehind(sql)
    else
      db.WriteQuery(sql);



  finally
    dbprov.NoNeed(db);
  end;
end;

procedure TShapeShiftDB.CoinLog(sBase, sMarket, sMessage: string; openid: int64 = 0);
var
  db: TRDTPDB;
  id: int64;
begin
  db := dbprov.Need;
  try
    id := db.kb.GetNextID_str('logid');
    db.Writebehind('insert into coinlog values ('+
                     GetStorageString(varInt64, id)+','+
                     GetStorageString(varString, sBase)+','+
                     GetStorageString(varString, sMarket)+','+
                     GetStorageString(varString, sMessage)+','+
                     GetStorageString(varDate, now())+','+
                     GetStorageString(varInt64, self.accountid)+','+
                     GetStorageString(varInt64, openid)+')'
                  );



  finally
    dbprov.NoNeed(db);
  end;
end;

function TShapeShiftDB.CoinStat(sBase, sMarket, sMessageClass, sMessage: string; ts: TDateTime = 0.0; reuseid: int64 = -1; openid: int64 = 0; bDelayed: boolean = true): TCoinStatReturn;
var
  db: TRDTPDB;
  id: int64;
  delayed: string;
begin
  if bDelayed then
    delayed := 'LOW_PRIORITY '
  else
    delayed := '';
  db := dbprov.Need;
  try
    if ts = 0.0 then
      ts := now;
    if reuseid < 0 then
      id := db.kb.GetNextID_str('coinstatid')
    else
      id := reuseID;

    result.id := id;
    result.ts := ts;
    if sMessageClass = 'manage' then
      Debug.log('MANAGE!');
    db.Writebehind('create table if not exists '+coinstattable(sMarket, sMessageClass)+' like coinstats_template');

    db.WriteBehind('insert '+DELAYED+'ignore into '+coinstattable(sMarket, sMessageClass)+' values ('+
                     GetStorageString(varInt64, id)+','+
                     GetStorageString(varString, sBase)+','+
                     GetStorageString(varString, sMarket)+','+
                     GetStorageString(varString, sMessage)+','+
                     GetStorageString(varDate, ts)+','+
                     GetStorageString(varInt64, self.accountid)+','+
                     GetStorageString(varInt64, openid)+','+
                     GetStorageString(varString, sMessageClass)+')'
                  );



  finally
    dbprov.NoNeed(db);
  end;
end;

function TShapeShiftDB.CoinSTatTable(sCoin: string; sMessageClass: string): string;
begin
  result := 'coinstats_'+sCoin+'_'+sMessageClass;
end;

procedure TShapeShiftDB.connect;
begin
//  if db <> nil then
//    exit;
//  db := NewDB;
end;


procedure TShapeShiftDB.Detach;
begin
  if detached then exit;

  disconnect;
  inherited;

end;

procedure TShapeShiftDB.disconnect;
begin
//  db.free;
//  db := nil;

end;

function TShapeShiftDB.FindRecentPositionTime(sBase, sMarket: string;
  istrategy: ni): TDateTime;
var
  db: TRDTPDB;
begin
  db := dbprov.need;
  try
    result := db.FunctionQuery('select created from position p where basecoin='+quote(sBase)+' and marketcoin='+quote(sMarket)+' and strategy='+inttostr(istrategy)+' and buytime>='+getstoragestring(varDate, 43049)+' and p.accountid='+inttostr(self.accountid)+' and state <= 59 order by buytime desc limit 1', 0.0);
  finally
    dbprov.noneed(db);
  end;
end;

procedure TShapeShiftDB.ForceClose(id: ni);
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
    db.Writebehind('insert ignore into forceclose values ('+inttostr(id)+')');
  finally
    dbprov.NoNeed(db);
  end;
end;

function TShapeShiftDB.GetAnything(sAddr: string; tmStart: TDateTime; tmEnd: TDateTime): IHolder<TSERowSet>;
var
  rs1: IHolder<TSERowSet>;
  sCoin: string;
  sMessageClass: string;
  sNode: string;
  sArray: string;
  sElement: string;
  t: ni;
  lm: string;
  expectjson: boolean;
  fld: string;
  fmh: IHolder<TFlexiMath>;
  nv: variant;
  fd: PSErowsetFieldDef;
begin
  SplitString(sAddr, '.', sCoin, sNode);
  SplitString(sNode, '.', sMessageClass, sNOde);
  if not SplitString(sNode, '[]', sArray, sElement) then
    sElement := sArray;

  sElement := trim(sElement);

  expectJson := sElement <> '';


  if expectJson then
    fld := 'logmessage, logmessage as lmstr'
  else
    fld := '(0+logmessage), logmessage as lmstr ';

  rs1 := ReadQuery('select '+fld+', ts from coinstats_'+sCoin+'_'+sMessageClass+' where ts >='+quote(DatetoMYSQLDate(tmStart))+' and ts <= '+quote(DateToMYSQLDate(tmEnd))+' order by ts');


  if sElement = '' then
    exit(rs1);//<------------------------

  result := THolder<TSERowSet>.create;
  result.o := TSeRowset.create;
  rs1.o.CopyFieldDefsTo(result.o);
//  fd := result.o.AddField;
//  fd.sname := 'result';
//  fd.vType := ftFloat;

  for t:= 0 to rs1.o.RowCount-1 do begin
    lm := rs1.o.Values[1,t];
//    Debug.log(lm);
    fmh := STrtofmh(lm);
    if fmh.o.Hasnode(sElement) then begin
      nv := fmh.o.GetNode(sElement).value;
      result.o.AddRow;
      result.o.Values[0,result.o.rowcount-1] := nv;
      result.o.Values[1,result.o.rowcount-1] := rs1.o.values[1,t];
      result.o.Values[2,result.o.rowcount-1] := rs1.o.values[2,t];

    end;

  end;



end;

function TShapeShiftDB.GetBannedCoins: string;
var
  db: TRDTPDB;
  rs: TSERowSet;
  sql: string;
  t: ni;
begin
  result := '';
  db := dbprov.Need;
  try
    rs := nil;
    try
      sql := 'select * from coinban';
      rs := db.ReadQuery(sql);
      for t:= 0 to rs.rowcount-1 do begin
        result := result + rs.values[0,t]+'.';

      end;


    finally
      rs.free;
      rs := nil;
    end;
  finally
    dbprov.noneed(db);
  end;
end;

function TShapeShiftDB.GetBestSocialVelocity(sCoin: string;
  dtDistance: TDateTime): double;
var
  cs: TCoinstat;
begin
  result := GetSocialVelocity(sCoin);
  CoinStat('btc', scoin, 'social_velocity', floatprecision(result,8));
  cs := GetCoinStatPeak(scoin, 'SocialVelocity', now-1.0, Now);
  if cs.val <> '' then
    result := greaterof(result, strtofloat(cs.val));


end;

function TShapeShiftDB.GetChartRange(base, market: string; period: ni; dtStart,
  dtEnd: TDateTime): TArray<TChartRec>;
var
  t: ni;
  db: TRDTPDB;
  sql: string;
  rS: TSERowSet;
begin
  db := dbprov.Need;
  try
    sql := 'select * from chart '+
           ' where basecoin='+GetStorageString(varString, base)+
           ' and marketcoin='+GetStorageString(varString, market)+
           ' and resolution='+inttostr(period)+
           ' and ts>='+GetStorageString(varDate, dtStart)+
           ' and ts<='+GetStorageString(varDate, dtEnd)+
           ' order by ts';

    rs := db.ReadQuery(sql);
    setlength(result, rs.RowCount);
    for t:= 0 to rs.RowCount-1 do begin
      rs.Cursor := t;
      result[t].BaseCoin := base;
      result[t].MarketCoin := market;
      result[t].res := period;
      result[t].ts := rs.CurRecordFields['ts'];
      result[t].h := rs.CurRecordFields['high'];
      result[t].l := rs.CurRecordFields['low'];
      result[t].o := rs.CurRecordFields['open'];
      result[t].c := rs.CurRecordFields['close'];
      result[t].v := rs.CurRecordFields['vol_market'];
      result[t].bv := rs.CurRecordFields['vol_base'];


    end;



  finally
    dbprov.noneed(db);
  end;


end;

function TShapeShiftDB.GetClimateReport(reportName: string): TClimateReport;
var
  db: TRDTPDB;
  rs: TSERowSet;
  sql: string;
begin
  db := dbprov.Need;
  try
    rs := nil;
    try
      sql := 'select * from climate where reportName="'+reportName+'" order by datetimeid desc limit 1';
      rs := db.ReadQuery(sql);

      result.valid := false;
      if rs.RowCount > 0 then begin
        rs.first;
        result.reportName :=  rs.CurRecordFields['reportName'];
        result.value := rs.CurRecordFields['value'];
        result.value2 := rs.CurRecordFields['value2'];
        result.stringValue := rs.CurRecordFields['stringValue'];
        result.timestamp := rs.CurRecordFields['datetimeid'];
        result.valid := true;
      end;
    finally
      rs.free;
      rs := nil;
    end;
  finally
    dbprov.noneed(db);
  end;


end;

function TShapeShiftDB.GetClosedPositions(iLimit: ni): TArray<TClosedPosition>;
var
  db: TRDTPDB;
  t: ni;
  rs: TSERowSet;
begin
  db := dbprov.need;
  try
    rs := db.ReadQuery(
      'select p.transactionid, '+
      ' basecoin, '+
      ' marketcoin, '+
      ' created, '+
      ' strategy, '+
      ' BuyRate, '+
      ' SellRate, '+
      ' BuyTime, '+
      ' SellTime, '+
      ' null from position p'+
      ' where selluuid<>"" '+
      ' order by selltime desc limit '+inttostr(iLimit));

    setlength(result, rs.rowcount);
    for t:= 0 to rs.rowcount-1 do begin
      rs.Cursor := t;
      result[t].id := rs.CurRecordFields['transactionid'];
      result[t].coin := rs.CurRecordFields['marketcoin'];
      result[t].strat := rs.CurRecordFields['strategy'];
      result[t].boughtAt := rs.CurRecordFields['BuyRate'];
      result[t].soldAt := rs.CurRecordFields['SellRate'];
      result[t].buytime := rs.CurRecordFields['BuyTime'];
      result[t].selltime := rs.CurRecordFields['SellTime'];


    end;
  finally
    dbprov.noneed(db);
  end;

end;

function TShapeShiftDB.GetCoinLogRange(sCoin: string; dtStart,
  dtEnd: TDateTime): TSERowSet;
var
  db: TRDTPDB;
  sql: string;
begin
  db := dbprov.Need;
  try
    sql := 'select * from coinlog '+
           ' where marketcoin='+GetStorageString(varString, sCoin)+
           ' and ts>='+GetStorageString(varDate, dtStart)+
           ' and ts<='+GetStorageString(varDate, dtEnd)+
           ' order by logid';
    result := db.ReadQuery(sql);
  finally
    dbprov.noneed(db);
  end;

end;

function TShapeShiftDB.GetCoinStatsRangeA(sCoin,
  sMessageClass: string; dtStart, dtEnd: TDateTime;
  bNewestFirst: boolean): TArray<TCoinStat>;
var
  rs: IHolder<TSERowSet>;
  t: ni;
begin
  rs := nil;
  try
    rs := GetCoinStatsRange(sCoin, sMessageClass, dtStart, dtEnd, bNewestfirst);
    rs.o.first;
    setlength(result, rs.o.RowCount);
    for t:= 0 to rs.o.rowcount-1 do begin
      result[t].basecoin := rs.o.CurRecordFields['basecoin'];
      result[t].marketcoin := rs.o.CurRecordFields['marketcoin'];
      result[t].id := rs.o.CurRecordFields['statid'];
      result[t].ts := rs.o.CurRecordFields['ts'];
      result[t].val := rs.o.CurRecordFields['logmessage'];
    end;
  finally
    rs.o.free;
    rs := nil;
  end;


end;

function TShapeShiftDB.GetCoinStatsRangeS(sCoin,
  sMessageClass: string; dtStart, dtEnd: TDateTime;
  bNewestFirst: boolean): TCoinStat;
var
  a: TArray<TCoinSTat>;
begin
  a := GetCoinStatsRangeA(scoin, sMessageClass, dtStart, dtEnd, bNewestFirst);
  result.ts := 0.0;
  result.id := 0;
  if length(a) > 0 then
    result := a[0];


end;

function TShapeShiftDB.GetCoinStatTables(sCoinFilter: String = ''; sMessageClassfilter: string = ''): TArray<string>;
var
  t: ni;
  rs: IHolder<TSERowSet>;
  sTable: string;
  sLeft: string;
  sRight: string;
  sLeftMustStartWith: string;
  sMustEndWith: string;
  idx: ni;
begin
//  rs := TSERowSet.Create;
  try
    rs := ReadQuery('show tables');
    idx := 0;
    setlength(result, rs.o.RowCount);
    rs.o.First;
    while not rs.o.EOF do begin
      try
        sTable := rs.o.CurRecordFieldsByIdx[0];
        sLeftMustStartWith := 'coinstats_'+sCoinFilter;
        sLeft := zcopy(sTable, 0, length(sLeftMustStartWith));
        if countchar(sTable, '_') >= 2 then begin
          if comparetext(sLeft, sLeftMustStartWith) = 0 then begin
            sMustEndWith := sMessageClassFilter;
            sRight := zcopy(sTable, length(sTable)-length(sMessageClassFilter), length(sMessageClassFilter));
            if comparetext(sRight, sMustEndWith) = 0 then begin
              result[idx] := sTable;
              inc(idx);
            end;
          end;
        end;
      finally
        rs.o.Next;
      end;
    end;
    setlength(result, idx);
  finally
//    rs.free;
  end;
end;

function TShapeShiftDB.GetCoinStatValueInsert(sBase, sMarket, sMessageClass,
  sMessage: string; ts: TDateTime; reuseid, openid: int64): string;
var
  db: TRDTPDB;
  id: int64;
  delayed: string;
begin
    if ts = 0.0 then
      ts := now;
    if reuseid < 0 then begin
      db := dbProv.need;
      try
        id := db.kb.GetNextID_str('coinstatid')
      finally
        dbprov.noNeed(db);
      end;
    end
    else
      id := reuseID;

    result := '('+
                     GetStorageString(varInt64, id)+','+
                     GetStorageString(varString, sBase)+','+
                     GetStorageString(varString, sMarket)+','+
                     GetStorageString(varString, sMessage)+','+
                     GetStorageString(varDate, ts)+','+
                     GetStorageString(varInt64, self.accountid)+','+
                     GetStorageString(varInt64, openid)+','+
                     GetStorageString(varString, sMessageClass)+')';

end;

function TShapeShiftDB.GetCoinStatClasses(sCoin: string): IHolder<TSERowSet>;
var
  db: TRDTPDB;
  sql: string;
  cl: string;
  sl: IHolder<TStringlist>;
  sDesc: string;
begin
  db := dbprov.Need;
  try
    cl := '';

    sql := 'select distinct messageclass from coinstats_eth_... '+
//           ' where marketcoin='+GetStorageString(varString, sCoin)+
           ' order by messageclass ';

//    result := THolder<TSERowSet>.create;
    result := db.ReadQueryMultiTable(sql, 'messageclass');

  finally
    dbprov.noneed(db);
  end;
end;

function TShapeShiftDB.GetCoinStatDelta(sCoin, sMessageClass: string): TCoinStat;
var
  db: TRDTPDB;
  sql: string;
  rs: TSERowSet;
  s1,s2: string;
  v1,v2: variant;
begin
  rs := nil;
  db := dbprov.Need;
  try



    sql := 'select logmessage, statid, ts from '+coinstattable(sCoin, sMessageClass)+
           ' where marketcoin='+GetStorageString(varString, sCoin)+
           ' and (messageclass = '+quote(sMessageClass)+') order by ts desc limit 2';

    rs := db.ReadQuery(sql);

    rs.first;
    if (rs.RowCount >= 2) and (vartype(rs.CurRecordFields['logmessage']) <> varNull) then begin
      result.basecoin := 'btc';
      result.marketcoin := sCoin;
      result.id := round(vartodoublenull(rs.CurRecordFields['statid'],0.0));
      result.ts := vartodoublenull(rs.CurRecordFields['ts'],0.0);
      s1 := rs.CurRecordFields['logmessage'];
      v1 := rs.CurRecordFields['logmessage'];
      rs.Next;
      s2 := rs.CurRecordFields['logmessage'];
      v2 := rs.CurRecordFields['logmessage'];
      if (s1 = '') or (s2 = '') then
        result.val := '0.0'
      else
        result.val := floattostr(vartodoublenull(v1,0.0)-vartodoublenull(v2,0.0));
        result.fval := vartodoublenull(v1,0.0)-vartodoublenull(v2,0.0);
    end else begin
      result.val := '0.0';
    end;

  finally
    rs.free;
    dbprov.noneed(db);
  end;
end;

function TShapeShiftDB.GetCoinStatNewestF(sCoin, sMessageClass: string): double;
var
  rs: IHolder<TSERowSet>;
begin
  result := 0.0;
  rs := self.GetCoinStatsRange(sCoin, sMessageClass, now-0.05, now, true);
  rs.o.first;
  if rs.o.RowCount > 0 then
    result := rs.o.CurRecordFields['LogMessage'];



end;

function TShapeShiftDB.GetCoinStatPeak(sCoin,
  sMessageClass: string; dtStart, dtEnd: TDateTime): TCoinStat;
var
  db: TRDTPDB;
  sql: string;
  cl: string;
  rs: TSERowSet;
  s: string;
begin
  rs := nil;
  db := dbprov.Need;
  try
    cl := '';





    if sMessageClass<> '' then
      cl := ' and (messageclass = '+quote(sMessageClass)+')';

    sql := 'select max(0+logmessage) as mx, statid, ts from '+coinstattable(sCoin, sMessageClass)+
           ' where marketcoin='+GetStorageString(varString, sCoin)+
           ' and ts>='+GetStorageString(varDate, dtStart)+
           ' and ts<='+GetStorageString(varDate, dtEnd) +
           cl;

    rs := db.ReadQuery(sql);

    rs.first;
    if (rs.RowCount > 0) and (vartype(rs.CurRecordFields['mx']) <> varNull) then begin
      result.basecoin := 'btc';
      result.marketcoin := sCoin;
      result.id := round(vartodoublenull(rs.CurRecordFields['statid'],0.0));
      result.ts := vartodoublenull(rs.CurRecordFields['ts'],0.0);
      s := rs.CurRecordFields['mx'];
      if s = '' then
        result.val := '0.0'
      else
        result.val := floattostr(vartodoublenull(rs.CurRecordFields['mx'],0.0));
    end else begin
      result.val := '0.0';
    end;

  finally
    rs.free;
    dbprov.noneed(db);
  end;
end;

function TShapeShiftDB.GetCoinStatsRange(sCoin: string; sMessageClass: string; dtStart,
  dtEnd: TDateTime; bNewestFirst: boolean): IHolder<TSERowSet>;
var
  db: TRDTPDB;
  sql: string;
  cl: string;
  sDesc: string;
begin
  db := dbprov.Need;
  try
    cl := '';

    sDesc := '';
    if bNewestFirst then
      sDesc := 'desc';

    if sMessageClass <> '' then
      cl := ' and (messageclass = '+quote(sMessageClass)+')'
    else
      cl := '';

    sql := 'select * from coinstats_'+sCoin+'_'+sMessageClass +
           ' where marketcoin='+GetStorageString(varString, sCoin)+
           ' and ts>='+GetStorageString(varDate, dtStart)+
           ' and ts<='+GetStorageString(varDate, dtEnd) +
           cl+
           ' order by statid '+sDesc;

    result := THolder<TSERowset>.create;
    result.o := db.ReadQuery(sql);

  finally
    dbprov.noneed(db);
  end;
end;

function TShapeShiftDB.GetCurrencies(OnlyTradable: boolean; AILive: boolean; AISim: boolean): TSERowset;
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
  if onlytradable then
    result := db.ReadQuery('select * from currency where tradable=true order by name ')
  else
  if AISim then
    result := db.ReadQuery('select * from currency where AISim=true order by name ')
  else
  if AISim then
    result := db.ReadQuery('select * from currency where AILive=true order by name ')
  else
    result := db.ReadQuery('select * from currency order by name');
  finally
    dbprov.noneed(db);
  end;

end;

function TShapeShiftDB.GetDistinctCoins: TArray<string>;
var
  db: Trdtpdb;
  rs: TSERowSet;
  t: ni;
begin
  db :=  dbprov.need;
  try
    rs := db.ReadQuery('select distinct marketcoin from coinlog order by marketcoin');
    rs.first;
    try
      setlength(result, rs.RowCount);
      for t:= 0 to rs.rowcount-1 do begin
        result[t] := rs.Values[0,t];
      end;
    finally
      rs.free;
    end;

  finally
    dbprov.noneed(db);
  end;

end;

function TShapeShiftDB.GetFromDogHouse(sCoin: string): TDogHouse;
var
  db: TRDTPDb;
  rs: TSERowset;
begin
  db := dbprov.Need;
  rs := nil;
  try
    result.coin := sCoin;
    result.rate := 0;
    result.tm := 0;

    rs := db.ReadQuery('select * from doghouse where coin="'+sCoin+'"');
    if rs.RowCount > 0 then begin
      result.rate := rs['rate'];
      result.tm := rs['timestamp'];
    end;

  finally
    rs.free;
    rs := nil;
    dbprov.noneed(db);
  end;
end;

function TShapeShiftDB.GetFundHistory: IHolder<TSERowset>;
var
  db: TRDTPDB;
  rs: TSERowSet;
begin
  db := dbprov.Need;
  if db <> nil then
  try

    result := THolder<TSErowset>.create;
    rs := db.ReadQuery('select * from fundvalue where accountid='+inttostr(accountid)+' order by datetimeid');
    result.o := rs;
  finally
    dbprov.noneed(db);
  end else
    raise ECritical.create('could not connect');

end;

function TShapeShiftDB.GetHistoricalSpread(ccFrom, ccTo: string;
  atSample: int64): double;
var
  samp: string;
  rate1: double;
  rate2: double;
  qty, qty2: double;
  db: TRDTPDB;
begin
  if atSample >=0 then
    samp := ' sampleid=(select max(sampleid) from quote)'
  else
    samp := ' sampleid='+inttostr(atsample);

  db := dbprov.Need;
  try
    rate1 := db.FunctionQuery_Cached('select rate from quote where '+samp+' and FromCoin='+quote(ccFrom)+' and ToCoin='+quote(ccTo),0.0);
    rate2 := db.FunctionQuery_Cached('select rate from quote where '+samp+' and FromCoin='+quote(ccTo)+' and ToCoin='+quote(ccFrom),0.0);
  finally
    dbprov.noneed(db);
  end;


  //if I have 50 dollars and they give me 45 dillars
  //what are those 45 dillars worth in dollars
  qty := rate1 * 50;//this tells me how much 50 dollars are worth in dillars
  qty2 := qty * rate2; //this tells me how much ?? dillars are worth in dollars


  //compare final qty to original qty
  if qty = 0 then
    exit(0.0)
  else
    result := abs(1-(qty2 / 50));


end;

function TShapeShiftDB.GetHoldings(maxsampleid: int64): TSERowSet;
var
  sWhere: string;
  ssim: string;
  db: TRDTPDB;
begin
  sSim := GettablePrefix;
//  if maxsampleid > 0 then
//    sWhere := 'where sampleid < '+maxsampleid.tostring;

  db := dbprov.Need;
  try
    result := db.ReadQuery('select sum(l.delta) as holding, c.cc as CurrencyCode, c.name as Name from '+sSim+'ledger l join currency c on (c.cc=l.CurrencyCode) '+sWhere+' group by currencyid');
  finally
    dbprov.noneed(db);
  end;

end;

function TShapeShiftDB.GetOpenPositions(sampleidmax: int64): TSERowset;
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
    result := db.ReadQuery('select * from position where state < 40 and accountid='+inttostr(accountid));
  finally
    dbprov.noneed(db);
  end;

end;

function TShapeShiftDB.GetPositionProfits(p: TOpenPosition): TPositionProfits;
var
  db: TRDTPDB;
  oid: int64;
  rs: TSERowSet;
begin
  db := dbprov.need;
  try
    result.coin := p.coin;
    result.bought := p.buyrate;
    result.sold := p.sellrate;
    result.BTCProfits := result.sold - result.bought;
  finally
    dbprov.NoNeed(db);
  end;
end;


function TShapeShiftDB.GetQuotes(cFrom, cto: string; iSampleIDLimit: int64 = -1;iSampleIDStart: int64 = 0): TQuotesAndVolume;
var
  sMaxSample: string;
  sMinSample: string;
  db: TRDTPDB;
begin
  if iSampleIDLimit > 0 then begin
    sMaxSample := ' and (sampleid <= '+iSampleIDLImit.tostring+')';
  end;
  sMinSample := ' and (sampleid >= '+iSampleIDStart.tostring+')';
//  result := db.ReadQuery('select * from quote where fromcoin="'+cFrom+'" and tocoin="'+cto+'"'+sMaxSample+sMinSample+' and (rate>0) order by sampleid');

  db := dbprov.need;
  try
    db.ReadQuery_Begin('select * from quote where fromcoin="'+cFrom+'" and tocoin="'+cTo+'"'+sMinSample+sMaxSample+' order by sampleid');
    db.ReadQuery_Begin('select * from volume where coin="'+cTo+'"'+sMinSample+sMaxSample+' order by sampleid');
    result.quotes.o := db.ReadQuery_End;
    result.volume.o := db.ReadQuery_End;
  finally
    dbprov.noneed(db);
  end;


end;

function TShapeShiftDB.GetSingleQuote(ccFrom, ccTo: string;
  atSample: int64): double;
var
  db: TRDTPDB;
begin
  db := dbprov.need;
  try
    result := db.FunctionQuery_Cached('select rate from quote where FromCoin="'+ccFrom+'" and ToCoin="'+ccTo+'" and sampleid<'+atsample.tostring+' order by sampleid desc limit 1', 0.0);
  finally
    dbprov.noneed(db);
  end;

end;

function TShapeShiftDB.GetSocialScore(sCoin: string;
  dtFirstAfter: TDatetime): double;
var
  cs: TCoinStat;
  searchwidth: single;
begin
  searchwidth := 0.1;

  while searchwidth < 2.0 do begin
    if dtFirstAFter = 0.0 then begin
      cs := GetCoinStatPeak(sCoin, 'social_points', Now-searchwidth, now);
    end else begin
      cs := GetCoinStatPeak(sCoin, 'social_points', dtFirstAfter-searchwidth, dtFirstAfter);
    end;
    if cs.val = '0.0' then
      searchwidth := searchwidth + 0.1
    else
      break;
  end;
  if cs.val = '' then
    result := 0.0
  else
    result := strtofloat(cs.val);
end;

function TShapeShiftDB.GetSocialVelocity(sCoin: string; dtDistance: TDateTime = 1.0; ending: TDateTime = 0.0): double;
var
  s,e: double;

begin
  if ending = 0.0 then
    ending := now;
  e := GetSocialScore(sCoin, ending);//get's NOW score
  s := GetSocialScore(sCoin, ending-1.0);//Get's yesterday's score
  if s = 0 then
    exit(0.0);
  result := e/s;
  if result < 0 then begin
    Debug.Log('Social Velocity should not be <0!');
    result := 0.0;
  end;
end;

function TShapeShiftDB.GetStatIDs(starttime: TDateTime; sBase, sMarket: string; sClasses: array of string): TArray<int64>;
var
  db: TRDTPDB;
  rs: TSERowSet;
  t: ni;
  claus: string;
begin
  Notimplemented;
  db := dbprov.Need;
  rs := nil;
  try
    claus := '(';
    for t := low(sClasses) to high(sClasses) do begin
      if t > 0 then
        claus := claus + ',';

      claus := claus + GetStorageString(varstring, sClasses[t]);

    end;
    claus := claus + ')';

//    rs := db.ReadQuery('select statid from coinstats where (messageClass="'+sVar+'") and (basecoin="'+sBase+'") and (marketcoin="'+sMarket+'") order by statid');
    rs := db.ReadQuery('select statid from coinstats where (messageClass in '+claus+') and (basecoin="'+sBase+'") and (marketcoin="'+sMarket+'") and (ts>='+GetStorageString(varDate, startTime)+') order by statid');
    setlength(result, rs.RowCount);
    rs.first;
    for t:= 0 to high(result) do begin

      result[t] := rs.Values[0, t];
    end;

  finally
    rs.free;
    rs := nil;
    dbprov.NoNeed(db);
  end;

end;

function TShapeShiftDB.GetStats(starttime: TDateTime; sBase, sMarket: string;
  sClasses: array of string): TArray<TStatRec>;
var
  db: TRDTPDB;
  rs: TSERowSet;
  t: ni;
  claus: string;
begin
  rs := nil;
  db := dbprov.Need;
  try
    claus := '(';
    for t := low(sClasses) to high(sClasses) do begin
      if t > 0 then
        claus := claus + ',';

      claus := claus + GetStorageString(varstring, sClasses[t]);

    end;
    claus := claus + ')';

//    rs := db.ReadQuery('select statid from coinstats where (messageClass="'+sVar+'") and (basecoin="'+sBase+'") and (marketcoin="'+sMarket+'") order by statid');
    rs := db.ReadQuery('select statid,ts,messageclass,logmessage,basecoin,marketcoin from coinstats where (messageClass in '+claus+') and (basecoin="'+sBase+'") and (marketcoin="'+sMarket+'") and (ts>='+GetStorageString(varDate, startTime)+') order by statid');
    setlength(result, rs.RowCount);
    for t:= 0 to high(result) do begin
      result[t].id := rs.Values[0, t];
      result[t].timestamp := rs.Values[1, t];
      result[t].messageClass := rs.Values[2, t];
      result[t].msg := rs.Values[3, t];
      result[t].basecoin := rs.Values[4, t];
      result[t].marketcoin := rs.Values[5, t];

    end;
  finally
    rs.free;
    rs := nil;
    dbprov.NoNeed(db);
  end;
end;


function TShapeShiftDB.GetStatsToRollUp(cointables: TArray<string>): TArray<TStatRec>;
var
  db: TRDTPDB;
  rs,rs2: TSERowSet;
  tt,t,z: ni;
  sTable: string;
  sCoin: string;
  idx: ni;
begin

  rs := nil;
  db := dbprov.Need;
  try
    idx := 0;
    for tt:= 0 to high(cointables) do begin
      sCoin := STatTableToCoin(cointables[tt]);
      db.ReadQuery_Begin('select statid,ts,messageclass,logmessage,basecoin,marketcoin from coinstats_'+sCoin+'_pick where messageclass in ("pick", "manage") and not statid in (select statid from coinstats_'+sCoin+'_chartcache where messageclass="ChartCache") limit 1');
      db.ReadQuery_Begin('select statid,ts,messageclass,logmessage,basecoin,marketcoin from coinstats_'+sCoin+'_pick where messageclass in ("pick", "manage") and not statid in (select statid from coinstats_'+sCoin+'_chartcache where messageclass="ChartCache") limit 1');
    end;

    for tt:= 0 to high(cointables) do begin
      for z := 0 to 1 do begin
        rs := db.ReadQuery_End;
        try
          sCoin := STatTableToCoin(cointables[tt]);
        setlength(result, length(result)+rs.RowCount);
          for t:= 0 to rs.RowCount-1 do begin
            result[idx].id := rs.Values[0, t];
            result[idx].timestamp := rs.Values[1, t];
            result[idx].messageClass := rs.Values[2, t];
            result[idx].msg := rs.Values[3, t];
            result[idx].basecoin := rs.Values[4, t];
            result[idx].marketcoin := rs.Values[5, t];
            inc(idx);
          end;
        finally
          rs.free;
        end;
      end;
    end;


  finally
//    rs.free;
//    rs := nil;
    dbprov.NoNeed(db);
  end;
end;


function TShapeShiftDB.GetStatsToRollUpAlltables: TArray<TStatRec>;
begin
  notimplemented;
end;

function TShapeShiftDB.GetStatByID(id: int64): TStatRec;
var
  db: TRDTPDB;
  rs: TSERowSet;
  t: ni;
begin
  db := dbprov.Need;
  rs := nil;
  try
    rs := db.ReadQuery('select statid, ts,messageclass,logmessage,basecoin,marketcoin from coinstats where (statid='+inttostr(id)+')');
    rs.First;
    if rs.RowCount = 0 then begin
      result.id := id;
      result.msg := '';
      result.timestamp := 0.0;
    end else begin
      result.id := id;
      result.timestamp := rs.Values[1, 0];
      result.messageClass := rs.Values[2, 0];
      result.msg := rs.Values[3, 0];
      result.basecoin := rs.Values[4, 0];
      result.marketcoin := rs.Values[5, 0];

    end;

  finally
    rs.free;
    rs := nil;
    dbprov.NoNeed(db);
  end;

end;

function TShapeShiftDB.GettablePrefix: string;
begin
  result := '';
  if simulated then
    result := 'sim_';
end;

function TShapeShiftDB.GetTopPerformersInMessageClass(
  sMessageClass: string; days_peak_range: single): IHolder<TSERowSet>;
var
  db: TRDTPDB;
begin
  db := dbprov.need;
  try
    result := db.ReadqueryMultiTable('select marketcoin, max(0+logmessage) as logmessage from coinstats...'+sMessageClass+' where messageclass="'+sMessageClass+'" and (ts > date_add(curdate(), interval -'+floattostr(days_peak_range*(60*60*24))+' second)) group by marketcoin order by logmessage desc ;', '-logmessage');
  finally
    dbprov.noneed(db);
  end;



end;

function TShapeShiftDB.GetTransactions(sampleidmax: int64): TSERowset;
var
  sWherE: string;
  db: TRDTPDB;
begin
  swhere := '';
  if sampleidmax >=0 then
    sWhere := 'where sampleid < '+sampleidmax.tostring;

  db := dbprov.need;
  try
    result := db.Readquery('select * from '+gettableprefix+'transaction '+sWhere+' order by sampleid');
  finally
    dbprov.noneed(db);
  end;

end;



function TShapeShiftDB.GetValueInBTC(cc: string; amt: double;
  atSample: int64 =-1): double;
begin
  result := GetValueInCC(cc, 'BTC', amt, atSample);
end;

function TShapeShiftDB.GetValueInCC(ccIn, ccOut: string; amt: double;
  atSample: int64): double;
var
  samp: string;
  s: string;
  db: TRDTPDB;
begin
  if (ccIn = ccOut) then begin
    result := amt;
    exit;
  end;

  result := 0;
  samp := '';
  if atSample < 0 then
    samp := '(sampleid<(select max(sampleid) from quote))'
  else
    samp := '(sampleid<'+atSample.tostring+')';

  s := 'select (rate) from quote where fromCoin="'+ccIn+'" and toCoin="'+ccOut+'" and '+samp+' order by sampleid desc limit 1';
  db := dbprov.need;
  try
    result := db.FunctionQuery_Cached(s, 0.0)*amt;
  finally
    dbprov.noneed(db);
  end;
end;

function TShapeShiftDB.GetValueInUSD(cc: string; amt: double;
  atSample: int64): double;
begin
  result := GetValueInCC(cc, 'BTC', amt, atSample);
  result := GetValueInCC('BTC', 'USD', result, atSample);
end;

function TShapeShiftDB.IsForcedClose(id: ni): boolean;
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
    result := db.FunctionQuery('select count(*) from forceclose where id='+inttostr(id), 0.0)>0.0;
  finally
    dbprov.NoNeed(db);
  end;
end;

procedure TShapeShiftDB.LinkTwitter(sCoin, sTwitterhandle: string);
var
  db: TRDTPDB;
begin
  db := dbprov.need;
  try
    db.Writebehind('insert into twitterfollow values ('+
          mysqlstoragestring.GetStorageString(varString, sCoin)+','+
          mysqlstoragestring.GetStorageString(varString, sTwitterHandle)+')');
  finally
    dbprov.NoNeed(db);
  end;

end;

function TShapeShiftDB.LoadSignalBuckets(sBase,
  sMArketCoin: string): TBuckets;
var
  db: TRDTPDB;
  s: string;
  jh: IHolder<TJSON>;
  t: ni;
begin
  db := dbprov.need;
  try
    setlength(result.a,0);
    s := db.Functionquery('select logmessage from coinstats where messageclass="Outcomes" and basecoin='+quote(sbase)+' and marketcoin='+quote(sMarketCoin)+'order by statid desc limit 1','');
    if s = '' then
      exit;

    //if we don't have a lot of data, use generalized data


    jh := strtojsonh(s);

//    try
//      if jh.o.HasNode('samples') then begin
//        if jh.o.GetNode('samples').Value < 25000 then begin
//          s := db.FunctionQuery('select logmessage from coinstats where messageClass="Outcomes" and basecoin='+quote(sbase)+' order by statid desc limit 1','');
//          jh := strtojsonh(s);
//        end;
//      end;
//    except
//    end;

    if not (jh.o.HasNode('buckets')) then exit;

    setlength(result.a, jh.o['buckets'].iCount);
    for t:= 0 to LESSEROF(BUCKET_COUNT-1, high(result.a)) do begin
      result.a[t].FromJson(jh.o['buckets'][t]);
    end;

  finally
    dbprov.noneed(db);
  end;


end;

function TShapeShiftDB.Login(user, pass: string): int64;
var
  db: TRDTPDB;
  i: int64;
begin
  db := dbprov.Need;
  try
    result := db.FunctionQuery('select accountid from account where name="'+user+'" and pass="'+pass+'"',-1);
    accountid := result;
    Gaccountid := accountid;

    Debug.Log('Login!');
//    i := db.FunctionQuery('select max(transactionid) from position',0);
//    db.kb.SetNextID_str('transactionid', i);
//    Debug.Log(inttostr(db.kb.GetNextID_str('transactionid')));
//    validateIDS(db);



  finally
    dbprov.noneed(db);
  end;


end;

procedure TShapeShiftDB.PushQuotesToDB;
var
  usd: BitStampAPI.TBTCtoUSD;
  mis: TMarketInfos;
  mi: TMarketInfo;
  txns: TMarketTransactions;
  qid, sampleid: int64;
  s,v: string;
  t: ni;
  nxt: ni;
  tmStart: ticker;
  db: TRDTPDB;
begin
  tmStart := GetTicker;
  while length(mis.a) = 0 do begin
    mis.GetFromAPI;
    if length(mis.a) = 0 then begin
      sleep(1000);
      Debug.Log('Got garbage from shapeshift.  Retry. elapsed='+gettimesince(tmSTart).tostring+'.ms');
    end;
  end;
  db := dbprov.need;
  try

    qid := db.FunctionQuery('select max(quoteid) from quote', 0);
    sampleid := db.functionquery('select max(sampleid) from quote', 0)+1;
    if qid < 0 then
      raise Ecritical.create('could not select max(quoteid)');

    s := 'insert into quote(timestamp,resolution,basecoin,marketcoin,open,close,minimum,maximum,volume)'+
         'values';

    nxt := 0;
    for t:= 0 to high(mis.a) do begin
      mi := mis.a[t];
      nxt := (qid+t+1);
      v := '('+inttostr(nxt)+','+
           GetStorageString(varDouble, mi.Rate)+','+
           GetStorageString(varDouble, mi.MinerFee)+','+
           GetStorageString(varDouble, mi.Min)+','+
           GetStorageString(varDouble, mi.Limit)+','+
           GetStorageString(varDate, now)+','+
           GetStorageString(varInt64, sampleid)+')';

      if t = 0 then
        s := s + v
      else
        s := s + ',' + v;

    end;

    //--------------------------------------------------
    //get BTC->USD quote from BitStamp

    usd.GetFromAPI;
    inc(nxt);
    s := s + ',('+inttostr(nxt)+','+
           GetStorageString(varString, 'BTC')+','+
           GetStorageString(varString, 'USD')+','+
           GetStorageString(varDouble, usd.usdPrice)+','+
           GetStorageString(varDouble, 0)+','+
           GetStorageString(varDouble, 0)+','+
           GetStorageString(varDouble, 0)+','+
           GetStorageString(varDate, now)+','+
           GetStorageString(varInt64, sampleid)+')';


    inc(nxt);
    s := s + ',('+inttostr(nxt)+','+
           GetStorageString(varString, 'USD')+','+
           GetStorageString(varString, 'BTC')+','+
           GetStorageString(varDouble, 1/usd.usdPrice)+','+
           GetStorageString(varDouble, 0)+','+
           GetStorageString(varDouble, 0)+','+
           GetStorageString(varDouble, 0)+','+
           GetStorageString(varDate, now)+','+
           GetStorageString(varInt64, sampleid)+')';



    db.WriteBehind(s);

    Debug.Log('Write behind elapsed='+gettimesince(tmSTart).tostring+'.ms');
  finally
    dbprov.noneed(db);
  end;




  PushTransactionsToDB(sampleid);
  Debug.Log('Volume collected '+gettimesince(tmSTart).tostring+'.ms');


end;

procedure TShapeShiftDB.PushTransactionsToDB(sampleid: int64);
var
  s: string;
  sLine: string;
  t: ni;
  mts: TMarketTransactions;
  mt: TMarketTransaction;
  db: TRDTPDB;
begin
  if not mts.GetFromAPI then
    exit;


  s := 'insert ignore into tx values';

  for t:= 0 to high(mts.a) do begin
    mt := mts.a[t];
    sLine := '('+mt.timestamp.tostring+','+quote(mt.ccFrom)+','+quote(mt.ccto)+','+mt.amount.tostring+',-1.0,'+sampleid.tostring+','+GetStorageString(varDate, now)+')';
    if t = 0 then
      s := s + sLine
    else
      s := s + ','+sLine;

  end;

  db := dbprov.need;
  try
    db.WriteQuery(s);
  finally
    dbprov.noneed(db);
  end;
//  RollupIncompleteTrades;



end;

procedure TShapeShiftDB.PushVolumeToDB(isampleidmax: int64);
var
  ccs: TSERowSet;
  cmds: array of Tcmd_PushVolume;
  db: TRDTPDB;
begin
  db := dbprov.need;
  try
    ccs := db.ReadQuery('Select cc from currency');
  finally
    dbprov.noneed(db);
  end;
  try
    setlength(cmds, ccs.RowCount);
    ccs.First;
    while not ccs.EOF do begin
//      pushVolumetoDB(iSampleIDMax, ccs['cc']);
      cmds[ccs.cursor] := Tcmd_PushVolume.create;
      cmds[ccs.cursor].cc := ccs['cc'];
      cmds[ccs.cursor].iSampleIDMax := iSampleIDMax;
      cmds[ccs.cursor].Start;
      ccs.Next;
    end;

    ccs.First;
    while not ccs.EOF do begin
      try
        cmds[ccs.cursor].WaitFor;
      finally
        cmds[ccs.cursor].free;
        ccs.Next;
      end;
    end;



  finally
    ccs.free;
  end;

end;

procedure TShapeShiftDB.PushVolumeToDB(sampleidmax: int64; cc: string);
var
  iMax: int64;
  txs: TSERowSet;
  tm: string;
  iThis: int64;
  iStarT: int64;
  t: ni;
  ts: double;
  tsStart: double;
  ssts: double;
  buy, sell, net: double;
  buys, sells, nets: array[0..4] of double;
  slLatentPush: TStringlist;
  db: TRDTPDB;
const
  dist: array[0..4] of integer = (60,30,15,1440,240);

begin
  db := dbprov.need;
  try
    Debug.Log('Push Volume '+cc);
(*
| volume | CREATE TABLE `volume` (
  `sampleid` bigint(20) NOT NULL,
  `Coin` varchar(25) NOT NULL DEFAULT '',
  `buyVolume60` double DEFAULT NULL,
  `sellVolume60` double DEFAULT NULL,
  `netVolume60` double DEFAULT NULL,
  `buyVolume30` double DEFAULT NULL,
  `sellVolume30` double DEFAULT NULL,
  `netVolume30` double DEFAULT NULL,
  `buyVolume15` double DEFAULT NULL,
  `sellVolume15` double DEFAULT NULL,
  `netVolume15` double DEFAULT NULL,
  `buyVolumeDay` double DEFAULT NULL,
  `sellVolumeDay` double DEFAULT NULL,
  `netVolumeDay` double DEFAULT NULL,
  PRIMARY KEY (`sampleid`,`Coin`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 |
*)
  slLatentPush := TStringlist.create;
  try
    iMAx := sampleidmax;
    iStart := db.FunctionQuery('select max(sampleid) from volume where coin='+quote(cc),db.FunctionQuery('select min(sampleid) from tx',0));

    Debug.Log('Stage 1 '+cc);
    iThis := iStart;
    while iThis <= iMax do begin
      db.FunctionQueryDouble_Begin('select max(timestamp) from tx where sampleid='+inttostr(iThis));
      inc(iThis);
    end;


    iThis := iStart;
    while iThis <= iMax do begin
//      Debug.Log('Stage 1-'+iThis.tostring);
      ts := db.FunctionQuery_end(0.0);

      for t:= 0 to high(dist) do begin
        db.FunctionQueryDouble_Begin('select sum(amountFrom) from tx where ccFrom='+quote(cc)+' and timestamp <= '+getstorageString(varDouble, ts)+' and timestamp > '+getstoragestring(varDouble, ts-(dist[t])));
        db.FunctionQueryDouble_Begin('select sum(amountTo) from tx where ccTo='+quote(cc)+'and timestamp <= '+getstorageString(varDouble, ts)+' and timestamp > '+getstoragestring(varDouble, ts-(dist[t])));
      end;
      inc(ithis);
    end;

    Debug.Log('Stage 2 '+cc);
    iThis := iStart;
    while iThis <= iMax do begin

      for t:= 0 to high(dist) do begin
        sell := db.FunctionQuery_End(0.0);
        buy := db.FunctionQuery_End(0.0);
        net := buy-sell;
        buys[t] := buy;
        sells[t] := sell;
        nets[t] := net;
      end;

      slLatentPush.add('insert ignore into volume values('+iThis.tostring+',"'+cc+'",'+
//                          buys[2].tostring+','+sells[2].tostring+','+nets[2].ToString+','+
//                          buys[1].tostring+','+sells[1].tostring+','+nets[1].ToString+','+
                          buys[0].tostring+','+sells[0].tostring+','+nets[0].ToString+','+
                          buys[1].tostring+','+sells[1].tostring+','+nets[1].ToString+','+
                          buys[2].tostring+','+sells[2].tostring+','+nets[2].ToString+','+
                          buys[3].tostring+','+sells[3].tostring+','+nets[3].ToString+','+
                          buys[4].tostring+','+sells[4].tostring+','+nets[4].ToString+')');

  //

      inc(iThis);
    end;

    Debug.Log('Stage 3 '+cc);
    for t:= 0 to slLatentPush.Count-1 do begin
      db.WriteBehind(slLatentPush[t]);
    end;


  finally
    slLatentPush.free;
  end;

  finally
    dbprov.noneed(db);
  end;



end;

function TShapeShiftDB.ReadQuery(sQuery: string): IHolder<TSERowSet>;
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
    result := THolder<TSERowSET>.create;
    result.o := db.ReadQuery(sQuery);
  finally
    dbprov.noneed(db);
  end;

end;

function TShapeShiftDB.ReadQueryMultiTable(sQuery: string; postorder: string): IHolder<TSERowSet>;
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
    result := db.ReadQueryMultiTable(sQuery, postorder);
  finally
    dbprov.noneed(db);
  end;

end;

procedure TShapeShiftDB.RecordFundValue(v: double);
var
  db: TRDTPDB;
begin
  ValidateAccount;
  db := dbprov.need;
  try
    db.Writebehind('insert into fundvalue values('+inttostr(accountid)+','+floattostr(now())+','+floattostr(v)+',0.0)');
  finally
    dbprov.NoNeed(db);
  end;
end;

procedure TShapeShiftDB.RecordPosition(p: POpenPosition);
var
  db: TRDTPDB;
  s: string;
  ss: string;
begin
  ValidateAccount;
  ss := p.currentRate.tostring;
  if ss = 'INF' then
    exit;

  ss := p.peakrate.tostring;
  if ss = 'INF' then
    exit;

  ss := p.lowestrate.tostring;
  if ss = 'INF' then
    exit;


  p.refreshRate := 0;
  db := dbprov.Need();
  try
    p.id :=  db.kb.GetNextID_str('transactionid');
    s := 'insert into position values ('+p.id.tostring+','+
      '"btc",'+
      GetStorageString(varString, p.coin)+','+
      'now(),'+
      '"'+DateToMySQLDate(p.expires)+'",'+
      '0,'+
      GetStorageString(varDouble, p.peakValue.tostring)+','+
      GetStorageString(varDouble, p.lowestValue.tostring)+','+
      GetStorageString(varDouble, p.currentValue.tostring)+','+
      GetStorageString(varDouble, p.lowestRate.tostring)+','+
      GetStorageString(varDouble, p.peakRate.tostring)+','+
      GetStorageString(varDouble, p.currentRate.tostring)+','+
      GetStorageString(varDouble, p.strategy.tostring)+','+
      GetStorageString(varDouble, p.HistoricalBottom.tostring)+','+
      GetStorageString(varInt64, Accountid)+','+
      GetStorageString(varDouble, p.buyrate)+','+
      GetStorageString(varDouble, p.sellrate)+','+
      GetStorageString(varString, '')+','+
      GetStorageString(varString, '')+','+
      GetStorageString(varDate, 0.0)+','+
      GetStorageString(varDate, 0.0)+','+
      '0.0,0.0,0.0,1,"",'+floatprecision(now(),8)+
    ')';


    if zpos('INF', s) >=0 then begin
      debug.log('trap! '+s);
    end;

    db.Writequery(s);

  finally
    dbprov.noneed(db);
  end;
end;

function TShapeShiftDB.RollupIncompleteTrades: int64;
var
  txs: TSERowSet;
  amt: double;
  s: string;
  amts: array of double;
  db: TRDTPDB;
begin
  db := dbprov.need;
  try
    result := 0;
    txs := db.ReadQuery('select * from tx where amountTo < 0');
    try
      setlength(amts, txs.rowcount);

  {$DEFINE BATCH}
  {$IFDEF BATCH}
      txs.First;
      while not txs.eof do begin
        amt := txs['amountFrom'];
        s := 'select (rate*'+amt.tostring+') from quote where fromCoin="'+txs.CurRecordFields['ccFrom']+'" and toCoin="'+txs.CurRecordFields['ccTo']+'" and '+vartostr(txs.CurRecordFields['sampleid'])+' order by sampleid desc limit 1';
        db.FunctionQueryDouble_Begin(s);
        txs.next;
      end;

      txs.First;
      while not txs.eof do begin
        Debug.Log('converting trade amounts: '+txs.cursor.tostring+' of '+txs.RowCount.tostring);
        amt := db.FunctionQuery_End(0.0);
        amts[txs.Cursor] := amt;
        txs.next;
      end;
  {$ELSE}
      txs.First;
      while not txs.eof do begin
        amt := txs['amountFrom'];
        s := 'select (rate*'+amt.tostring+') from quote where fromCoin="'+txs.CurRecordFields['ccFrom']+'" and toCoin="'+txs.CurRecordFields['ccTo']+'" and '+vartostr(txs.CurRecordFields['sampleid'])+' order by sampleid desc limit 1';
        db.FunctionQueryDouble_Begin(s);
        amt := db.FunctionQuery_End(0.0);
        Debug.Log(amt.tostring+' converting trade amounts: '+txs.cursor.tostring+' of '+txs.RowCount.tostring);
        amts[txs.Cursor] := amt;
        txs.next;
      end;

  {$ENDIF}


      result := 0;
      txs.First;
      while not txs.eof do begin
        Debug.Log('fixing incomplete trades: '+(txs.cursor+1).tostring+' of '+txs.RowCount.tostring);
        amt := amts[txs.cursor];
        db.WriteBehind('update tx set amountTo="'+getstoragestring(varDouble, amt)+'"'+
                    ' where ccFrom='+quote(txs['ccFrom'])+
                    ' and ccTo='+quote(txs['ccTo'])+
                    ' and amountFrom='+vartostr(txs['amountFrom'])+
                    ' and sampleid='+vartostr(txs['sampleid']));
        result := greaterof(result,txs['sampleid']);
        txs.next;
      end;

    finally
      txs.free;
    end;
  finally
    dbprov.NoNeed(db);
  end;

end;

procedure TShapeShiftDB.RollUpTrajectory(sBase, sMarketCoin: string);
var
  rsVars: TSERowSet;//ok
  jh: IHolder<TJSON>;
  jhOut: IHOlder<TJSON>;
  db: TRDTPDB;
  s: string;
  bix: ni;
  row: ni;
  sig, act, ask,bid: double;
  buckets: array of TSignalTragectory;
  pl: TPriceLine;

  function GetBucketIndex(d: double): ni;
  begin
    result := round(d * BUCKET_COUNT);
    result := LesserOf(BUCKET_COUNT,greaterof(0, result));
  end;

  function GetPriceLine(idx: ni): TPriceLine;
  var
    row2: ni;
    jhh: IHolder<TJSON>;
    x: ni;
    tsStart: TDateTime;
  begin
    rsVars.Cursor := idx;
    tsStart := rsVArs['ts'];
    result.init;

//    if sMarketcoin = 'eth' then
//      Debug.Log('-------PRICELINE-------');
    for x := idx to rsVars.RowCount-1 do begin
      rsVars.Cursor := x;
      jhh := StrToJSONh(rsVars.CurRecordFields['logmessage']);
      if rsVArs['ts'] > (tsStart + (10*(1/(24*60)))) then
        break;

//      if sMarketCoin = 'eth' then
//        Debug.Log(vartostr(jhh.o['Bid'].Value)+' ---- ' +vartostr(jhh.o['Ask'].value));
//      if x = idx then begin
//        if jhh.o['ActionSignal'].value > 0.95 then
          result.ApplyPrices(jhh.o['Bid'].Value, jhh.o['Ask'].value);
//      end;
    end;

  end;

  function FindNonEmptyBucket(above: ni): ni;
  var
    y: ni;
  begin
    for y := above to high(buckets) do
      if buckets[y].samples > 0 then
        exit(y);

    exit(-1);
  end;

  function FindNonEmptyBucketBelow(below: ni): ni;
  var
    y: ni;
  begin
    for y := below downto 0 do
      if buckets[y].samples > 0 then
        exit(y);

    exit(-1);
  end;

var
  ab, bl: ni;
  t: ni;
  sWhere: string;
  avg: double;
  cnt: double;
begin
  try
  Debug.Log(self, 'Roll up Trajectory '+sBase+'-'+sMarketCoin);
//  if sMarketcoin = 'grs' then
//    Debug.Log('trap')
//  else
//    exit;
  setlength(buckets, BUCKET_COUNT);
  for t := 0 to high(buckets) do
    buckets[t].init(t);
  //fetch all signals with ChartCache stat var
  jhOut := nil;
  db := dbprov.Need;
  try
    if sMarketCoin <> '' then
      sWhere := ' and marketcoin="'+sMarketCoin+'" ';
    rsVars := db.ReadQuery('select * from coinstats where messageclass="ChartCache" and basecoin="'+sBase+'" '+sWhere);
    rsVars.First;
    for t:= 0 to rsVars.RowCount-1 do begin
      rsVars.cursor := t;
      s := rsVars.CurRecordFields['logmessage'];
      jh := StrToJSONh(s);
      if not jh.o.HasNode('Signal') then
        continue;

      try
      sig := jh.o['Signal'].value;
      act := jh.o['ActionSignal'].value;
      bid := jh.o['Bid'].value;
      ask := jh.o['Ask'].value;
      bix := GetBucketIndex(sig);
      //determine price line from current point
      if act > 0.95 then begin
        pl := GetPriceLine(t);

        if pl.Samples > 2 then begin
          //apply price line to bucket
          if (sMarketcoin = 'eth') and (bix=5)then
            debug.log('Apply PriceLine To Bucket '+inttostr(bix));
          buckets[bix].ApplyPriceLine(pl);
        end;
      end;
      except
      end;
    end;

{$IFDEF SYNTHESIZE_MISSING_BUCKETS}
    //synthesize empty buckets
    for bix:= 0 to high(buckets) do begin
      if buckets[bix].samples = 0 then begin
        ab := FindNonEmptyBucket(bix);
        bl := FindNonEmptyBucketBelow(bix);
        if bl < 0 then bl := ab;
        if ab < 0 then ab := bl;

        if bl < 0 then
          exit;//NO BuCKETS WITH SAMPLES ANYWHERE!


        if ab = bl then
          buckets[bix].Synthesize(buckets[ab], buckets[ab], 1)
        else
          buckets[bix].Synthesize(buckets[bl], buckets[ab], (bix-bl)/(ab-bl));
        buckets[bix].xsignalMin := (bix/BUCKET_COUNT);
        buckets[bix].signalMax := ((bix+1)/BUCKET_COUNT);

      end;
    end;
{$ENDIF}


    //normalize buckets

    avg := 0;
    cnt := 0;
    for bix := 0 to high(buckets) do begin
      avg := avg + (buckets[bix].MeanTrajectory*buckets[bix].samples);
      cnt := cnt + buckets[bix].samples;
    end;
    if cnt > 0 then
      avg := avg / cnt
    else
      avg := 0;

    for bix:= 0 to high(buckets) do begin
      buckets[bix].AdjustedTrajectory := buckets[bix].MeanTrajectory - avg;
      if buckets[bix].AdjustedTrajectory < -0.9 then
        Debug.Log('trap');
    end;






    jhOut := THOLDER<TJSON>.create;
    jhOut.o := TJSON.create;
    jhOut.o.AddMemberPrimitive('samples', rsVars.rowCount);
    jhOut.o.AddMemberJSON('buckets', '[]');
    for t:= 0 to high(buckets) do
      jhOut.o['buckets'].AddIndexed(buckets[t].ToJSON.o);

    CoinStat(sBase, sMarketCoin, 'Outcomes', jhOut.o.ToJson, now);

  finally
    rsVars.free;
    rsVars := nil;
    dbprov.noneed(db);
  end;
  except
  end;


end;

procedure TShapeShiftDB.SaveClimateReport(report: TClimateReport);
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
    report.timeStamp := now;
    db.WriteBehind('insert ignore into climate values ('+
                   floattostr(report.timestamp)+','+
                   GEtStorageString(varString, report.reportName)+','+
                   floattostr(report.value)+','+
                   floattostr(report.value2)+','+
                   GEtStorageString(varString, report.stringValue)+')');

  finally
    dbprov.noneed(db);
  end;
end;

procedure TShapeShiftDB.Saveposition(p: TopenPosition);
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
    db.WriteQuery('update position set'+
                  ' peakValue='+p.peakValue.ToString+','+
                  ' lowestValue='+p.lowestValue.ToString+','+
                  ' currentValue='+p.currentValue.ToString+','+
                  ' peakRate='+p.peakRate.ToString+','+
                  ' ExpireTime='+GetStorageString(varDate, p.Expires)+','+
                  ' buyCost='+GetStorageString(varDouble, p.buyCost)+','+
                  ' buyAmount='+GetStorageString(varDouble, p.buyAmount)+','+
                  ' buyRate='+GetStorageString(varDouble, p.buyrate)+','+
                  ' sellRate='+GetStorageString(varDouble, p.sellrate)+','+
                  ' buyTime='+GetStorageString(varDAte, p.buytime)+','+
                  ' sellTime='+GetStorageString(varDate, p.selltime)+','+
                  ' lowestRate='+GetStorageString(varDouble, p.lowestRate.ToString)+','+
                  ' currentRate='+GetStorageString(varDouble, p.currentRate.ToString)+','+
                  ' state='+p.state.ToString+','+
                  ' strategy='+p.strategy.ToString+','+
//                  ' statex='+GetStorageString(varString, p.statex)+','+
                  ' refreshrate='+GetStorageString(varInt64, p.refreshrate)+','+
                  ' unitstoprocess='+GetStorageString(varDouble, p.unitstoprocess)+','+
                  ' because='+GetStorageString(varString, p.because)+','+
                  ' statechangetime='+GetStorageString(varDouble, p.lastStateChange)+','+
                  ' BuyUUID='+GetStorageString(varString, p.buyuuid)+','+
                  ' SellUUID='+GetStorageString(varString, p.selluuid)+
                  ' WHERE transactionid='+p.id.tostring);
    //db.Writebehind('update transaction set unitsToProcess='+p.UnitsToProcess.tostring+' where transactionid='+p.id.tostring);
  finally
    dbprov.noneed(db);
  end;
end;

procedure TShapeShiftDB.SaveTweets(sCoin: string; j: IHolder<TJSON>);
var
  db: Trdtpdb;
  s: string;
  tw: TTweet;
  t: ni;
  node: TJSON;
begin
  db := dbprov.Need;
  try
    tw.coin := sCoin;
    for t:= 0 to j.o['statuses'].iCount-1 do begin
      node := j.o['statuses'][t];
      tw.id := strtoint64(node['id_str'].value);
      tw.created_on := TwitterDateToDAteTimeGMT(node['created_at'].value);
      tw.tweet := node['full_text'].value;
      tw.found_on := now;
      tw.json := node.ToJson;
      s := 'insert ignore into tweet values '+tw.DBValueString;
      db.Writebehind(s);

    end;


  finally
    dbprov.NoNeed(db);
  end;

end;

function TShapeShiftDB.TXIDtoCoin(id: int64): string;
var
  db: TRDTPDB;
  sql: string;
begin
  db := dbprov.Need;
  try
    result := db.FunctionQuery('select marketcoin from position p transactionid='+inttostr(id),'');
  finally
    dbprov.noneed(db);
  end;
end;

procedure TShapeShiftDB.ValidateAccount;
begin
  if accountid < 1 then
    raise ECritical.create('not logged in');
end;

procedure TShapeShiftDB.ValidateIDs(db: TRDTPDB);
var
  id: int64;
  dbid: int64;
begin
  id := db.kb.GetNextID_str('ledger');
  dbid := db.FunctionQuery('select max(ledgerid) from ledger', 0);
  if id < dbid then
    db.kb.SetNextID_str('ledger',dbid+10);

  id := db.kb.GetNextID_str('transaction');
  dbid := db.FunctionQuery('select max(transactionid) from transaction', 0);
  if id < dbid then
    db.kb.SetNextID_str('transaction',dbid+10);



end;

procedure TShapeShiftDB.WriteQuery(sQuery: string);
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
    db.WriteQuery(sQuery);
  finally
    dbprov.noneed(db);
  end;
end;

procedure TShapeShiftDB.WriteQueryMultiTable(sQuery: string);
var
  db: TRDTPDB;
begin
  db := dbprov.Need;
  try
    db.WriteQueryMultiTable(sQuery);
  finally
    dbprov.noneed(db);
  end;
end;

{ TShapeShiftDBProvider }

procedure TShapeShiftDBProvider.Detach;
begin
  if detached then exit;

  while pool.count > 0 do begin
    pool[0].Free;
    pool.delete(0);
  end;
  pool.free;
  pool := nil;

  inherited;

end;

procedure TShapeShiftDBProvider.Init;
begin
  inherited;
  pool := TBetterList<TShapeShiftDB>.create;
end;

function TShapeShiftDBProvider.Need(simulated: boolean): TshapeShiftDB;
begin
  lock;
  try
    if pool.count > 0 then begin
      result := pool[pool.Count-1];
      pool.Delete(pool.Count-1);
    end else begin
      result := TshapeShiftDB.create;
      try
        result.connect;
        //result.GetStatByID(30105);
        result.accountid := GAccountid;
        if result.accountid = -1 then
          Result.Login(LoadUserName, LoadPass);
      except
      end;

    end;

    result.Simulated := simulated;

  finally
    Unlock;
  end;
end;

procedure TShapeShiftDBProvider.NoNeed(db: TShapeShiftDB);
begin
  lock;
  try
    pool.add(db);
  finally
    unlock;
  end;
end;

procedure oinit;
begin;
  dbprov := TRDTPDBPRovider.create;
  prov := TShapeShiftDBProvider.create;


end;

procedure ofinal;
begin
  prov.free;
  dbprov.free;

end;

{ Tcmd_PushVolume }

procedure Tcmd_PushVolume.DoExecute;
begin
  inherited;
  db := prov.Need(false);
  try
    db.PushVolumeToDB(iSampleIDMax, cc);
  finally
    prov.NoNeed(db);
  end;

end;

procedure Tcmd_PushVolume.InitExpense;
begin
  CPUExpense := systemx.GetnumberofProcessors/32;
  MemoryExpense := 0.25;
end;

{ TRDTPDBPRovider }

procedure TRDTPDBPRovider.Detach;
begin

  if detached then exit;

  while pool.count > 0 do begin
    pool[0].Free;
    pool.delete(0);
  end;
  pool.free;
  pool := nil;
  inherited;
end;

procedure TRDTPDBPRovider.Init;
begin
  inherited;
  pool := TBetterList<TRDTPDB>.create;
end;

function TRDTPDBPRovider.Need: TRDTPDB;
var
  bForce: boolean;
  tmstart: ticker;
begin
  bForce := false;
  tmStart := getTicker;
  result := nil;
  while result = nil do begin
    while (issued >= MAX_RDTP_CONNECTIONS) and (gettimesince(tmStart) < 16000) do
      sleep(100);
    lock;
    try
      if pool.count > 0 then begin
        result := pool[0];
        pool.Delete(0);
        inc(issued);
//        Debug.Log('Issued='+inttostr(issued));
      end else begin
        try
          if (issued < MAX_RDTP_CONNECTIONS) or (gettimesince(tmStart) >= 16000) then begin
            result := NewDB;
            inc(issued);
          end;
        except
          result := nil;
          exit;
        end;
      end;


    finally
      Unlock;
    end;
  end;
end;


function TShapeShiftDB.BeginUpgradeDatabase: TCommand;
begin
  result := Tcmd_UpgradeDatabase.Create;
  result.Start;

end;

function TOpenPosition.BreakEvenAboveLowest: double;
begin
  result := buyCost / lowestValue;

end;

function TOpenPosition.BreakEvenPercent: double;
begin
  if CurrentBaseValue <= 0.0 then
    exit(1.0);
  result := (buyCost-CurrentBaseValue) / CurrentBaseValue;
end;

function TOpenPosition.CurrentPositionInProfitability: double;
begin
  if currentrate < buyrate then
    exit(0.0);
  result := (currentrate-buyrate)/(peakrate-buyrate);
end;

function TOpenPosition.CurrentPositionInVolatility: double;
begin
  result := (currentrate-lowestrate)/(peakrate-lowestrate);
end;

function TShapeShiftDB.DatabaseNeedsUpgrade: boolean;
var
  db: TRDTPDB;
begin
  db := dbProv.Need;
  try
    result := db.FunctionQuery('Select number from version where id=0', 0.0) < TARGET_DB_VERSION;
  finally
    dbprov.noneed(db);
  end;
end;

procedure TShapeShiftDB.EndUpgradeDAtabase(c: TCommand);
begin
  c.waitfor;
  c.free;

end;

function TOpenPosition.GetCurrentBaseValue: double;
begin
  result := self.UnitsToProcess*self.currentrate;
end;

function TOpenPosition.GetLastStateChange: double;
begin
  Result := FLastStateChange;
  if result = 0.0 then begin
    FLastStateChange := now();
    result := now();
  end;
end;

function TOpenPosition.GetPeakPercentAboveLowest: double;
begin
  result := (peakrate-lowestrate) / lowestrate;
end;

function TOpenPosition.GetPeakProfitPercent: double;
begin
  result := (peakrate-buyrate) / buyrate;
end;

function TOpenPosition.GetPercentAboveLowest: double;
begin
  result := (currentrate-lowestrate) / lowestrate;

end;

function TOpenPosition.GetPercentBelowHighest: double;
begin
  result := (peakrate-currentrate) / peakrate;


end;


function TOpenPosition.GetProfit: double;
begin
  result := (currentrate-buyrate) * buyamount;

end;

function TOpenPosition.GetProfitPercent: double;
begin
  result := (currentrate-buyrate) / buyrate;

end;


function TOpenPosition.GetStateXJSON: IHolder<TJSON>;
begin
  result := StrToJSONh(statex);
end;

procedure TOpenPosition.Init;
begin
  CurrentRate := 0.0;
  peakRate := 0.0;
  LowestRate := 0.0;
  CurrentValue := 0.0;
  PeakValue := 0.0;
end;

function TOpenPosition.getrefreshRate: ticker;
var
  pp: double;
begin
  result := FRefreshRate;
  result := greaterof(BEST_REFRESH_TIME, result);


end;

procedure TOpenPosition.SetBuyRate(const Value: double);
begin
//  Debug.Log(self.coin+' buyrate='+floatprecision(value,8));
  FBuyRate := Value;
end;

procedure TOpenPosition.SetCurrentRate(const Value: double);
begin
  FCurrentRate := Value;
  if (peakRate = 0) or (value > peakRate) then peakRate := value;
  if (lowestRate = 0) or (value < lowestRate) then lowestRate := value;
end;

procedure TOpenPosition.SetCurrentValue(const Value: double);
begin
  FCurrentValue := Value;
  if (peakValue = 0) or (value > peakValue) then peakValue := value;
  if (lowestValue = 0) or (value < lowestValue) then lowestValue := value;
end;

procedure TOpenPosition.SetRefreshRate(const Value: ticker);
begin
  FRefreshRate := value;
end;

procedure TOpenPosition.SetState(const Value: integer);
begin
  if value <> FState then
    lastStateChange := now();
  Fstate := Value;

end;

procedure TOpenPosition.SetStateXJSON(const Value: IHolder<TJSON>);
begin
  statex := value.o.ToJson;
end;

function TOpenPosition.TimeInState: ticker;
begin
  if lastStatechange = 0 then begin
    lastStateChange := now;
    exit(0);
  end;
  result := round((now()-lastStateChange)* ( 24*60*60*1000));
end;

function TOpenPosition.TimeUntilRefresh: ticker;
begin
  result := GetTimeSince(lastRefresh+RefreshRate);


end;

function TOpenPosition.TradeName: string;
begin
  result := coin+':'+inttostr(strategy);
end;

procedure TRDTPDBPRovider.NoNeed(var db: TRDTPDB);
begin
  lock;
  try
    dec(issued);
    if (not db.connected) or (pool.count > MAX_RDTP_CONNECTIONS) then begin
      db.free;
      db := nil;
    end
    else begin
      pool.add(db);
      db := nil;
    end;
  finally
    unlock;
  end;
end;

{ TPositionProfits }

function TPositionProfits.PercentProfits: double;
begin
  if bought = 0 then
    exit(0);
  result := (sold-bought)/bought;
end;

{ TClimateReport }

function TClimateReport.Age: TDateTime;
begin
  result := now - timestamp;
end;

procedure TClimateReport.Init;
begin
  timestamp := 0.0;
  value := 0.0;
  value2 := 0.0;
  stringValue := '';
  valid := false;
  reportName := '';
end;

{ TSignalTragectory }



procedure TSignalTragectory.ApplyPriceLine(pl: TPriceLine);
var
  gain, loss: double;
  traj: TSignalTragectory;
begin
  if pl.EnterAsk = 0 then
    exit;
  gain := pl.MaxBid/pl.EnterAsk;
  loss := pl.MinBid/pl.EnterAsk;

  if gain > 1.5 then begin
    Debug.Log('unusual gain');
    exit;
  end;


  traj.init(0);
  traj.MaxMaxTrajectory := gain;
  traj.MinMaxTrajectory := gain;
  traj.FAvgMaxTrajectory := gain;
  traj.MaxMinTrajectory := loss;
  traj.MinMinTrajectory := loss;
  traj.FAvgMinTrajectory := loss;
  traj.MedianTrajectory := (traj.MaxMaxTrajectory + traj.MinMinTrajectory) / 2;
  traj.Samples := 1;
  traj.MeanTrajectory := (traj.AvgMaxTrajectory+traj.AvgMinTrajectory) / 2;

   self.ApplyTraj(traj);
//  Debug.Log('After priceLIne gain:'+floatprecision(gain, 4)+'Avg Max Traj:'+floatprecision(traj.AvgMaxTrajectory,4));

end;

procedure TSignalTragectory.ApplyTraj(traj: TSignalTragectory);
begin
  if samples = 0 then begin
    self.MinMinTrajectory := traj.MinMinTrajectory;
    self.MaxMinTrajectory := traj.MaxMinTrajectory;
    self.FAvgMinTrajectory :=traj.FAvgMinTrajectory;
    self.MinMaxTrajectory := traj.MinMaxTrajectory;
    self.MaxMaxTrajectory := traj.MaxMaxTrajectory;
    self.FAvgMaxTrajectory :=traj.FAvgMaxTrajectory;
  end else begin
    self.MinMaxTrajectory := lesserof(self.MinMaxTrajectory, traj.MinMaxTrajectory);
    self.MaxMaxTrajectory := greaterof(self.MaxMaxTrajectory, traj.MaxMaxTrajectory);
    self.MinMinTrajectory := lesserof(self.MinMinTrajectory, traj.MinMinTrajectory);
    self.MaxMinTrajectory := greaterof(self.MaxMinTrajectory, traj.MaxMinTrajectory);
    self.FAvgMaxTrajectory := self.FAvgMaxTrajectory + traj.FAvgMaxTrajectory;
    self.FAvgMinTrajectory := self.FAvgMinTrajectory + traj.FAvgMinTrajectory;

  end;

  self.MeanTrajectory := (self.AvgMinTrajectory + self.AvgMaxTrajectory) / 2;


  inc(samples);
end;

function TSignalTragectory.GetAvgMaxTrajectory: double;
begin
  if samples = 0 then
    result := 1.0
  else
    result := FAvgMaxTrajectory / samples;
end;

function TSignalTragectory.AvgMinTrajectory: double;
begin
  if samples = 0 then
    result := 1.0
  else
    result := FAvgMinTrajectory / samples;
end;

procedure TSignalTragectory.FromJson(j: TJSON);
begin
  self.basecoin := j['basecoin'].value;
  self.marketcoin := j['marketcoin'].value;
  self.xSignalMin := j['SignalMin'].value;
  self.SignalMax := j['SignalMax'].Value;
  self.MedianTrajectory := j['MedianTrajectory'].value;
  self.MaxMaxTrajectory := j['MaxMaxTrajectory'].value;
  self.MinMaxTrajectory := j['MinMaxTrajectory'].value;
  self.MaxMinTrajectory := j['MaxMinTrajectory'].value;
  self.MinMinTrajectory := j['MinMinTrajectory'].value;
  self.MeanTrajectory := j['MeanTrajectory'].value;
  self.FAvgMaxTrajectory := j['FAvgMaxTrajectory'].value;
  self.FAvgMinTrajectory := j['FAvgMinTrajectory'].value;
  self.Samples := j['Samples'].value;
  if j.HasNode('AdjustedTrajectory') then
    self.AdjustedTrajectory := j['AdjustedTrajectory'].value
  else
    self.AdjustedTrajectory := AvgMaxTrajectory - 1.0;
//  result.o.AddMember('AdjustedTrajectory',   self.AdjustedTrajectory);
end;

procedure TSignalTragectory.init(bucketindex: ni);
begin
  MaxMaxTrajectory := 0;
  MinMaxTrajectory := 0;
  FAvgMaxTrajectory := 0;
  MaxMinTrajectory := 0;
  MinMinTrajectory := 0;
  FAvgMinTrajectory := 0;

  basecoin := 'btc';
  marketcoin := '';
  xSignalMin := bucketindex / BUCKET_COUNT;
  SignalMax := (bucketindex / BUCKET_COUNT)+(1/BUCKET_COUNT);
  MedianTrajectory := 1.0;
  MeanTrajectory := 1.0;
  samples := 0;
end;

procedure TSignalTragectory.SetAvgMaxTrajectory(const Value: double);
begin
  FAvgMaxTrajectory := value * samples;
end;

procedure TSignalTragectory.Synthesize(a, b: TSignalTragectory; rBlend: double);
begin
  basecoin := a.basecoin;
  marketcoin := a.marketcoin;
  Samples := a.Samples;
  xSignalMin := interpolate(rBlend, a.xSignalMin, b.xSignalMin);
  SignalMax := interpolate(rBlend, a.SignalMax, b.SignalMax);
  MedianTrajectory := interpolate(rBlend, a.MedianTrajectory, b.MedianTrajectory);
  MeanTrajectory := interpolate(rBlend, a.MeanTrajectory, b.MeanTrajectory);
  MaxMaxTrajectory := interpolate(rBlend, a.MaxMaxTrajectory, b.MaxMaxTrajectory);
  MinMaxTrajectory := interpolate(rBlend, a.MinMaxTrajectory, b.MinMaxTrajectory);
  MaxMinTrajectory := interpolate(rBlend, a.MaxMinTrajectory, b.MaxMinTrajectory);
  MinMinTrajectory := interpolate(rBlend, a.MinMinTrajectory, b.MinMinTrajectory);
  FAvgMaxTrajectory := interpolate(rBlend, a.FAvgMaxTrajectory / a.Samples, b.FAvgMaxTrajectory/ b.Samples)*samples;
  FAvgMinTrajectory := interpolate(rBlend, a.FAvgMinTrajectory/ a.Samples, b.FAvgMinTrajectory/ b.Samples)*samples;

end;

function TSignalTragectory.ToJSON: IHolder<TJSON>;
begin
  result := THolder<TJSON>.create;
  result.o := TJSON.create;

  result.o.AddMemberPrimitive('basecoin',   self.basecoin);
  result.o.AddMemberPrimitive('marketcoin',   self.marketcoin);
  result.o.AddMemberPrimitive('SignalMin',   self.xSignalMin);
  result.o.AddMemberPrimitive('SignalMax',   self.SignalMax);
  result.o.AddMemberPrimitive('MedianTrajectory',   self.MedianTrajectory);
  result.o.AddMemberPrimitive('MaxMaxTrajectory',   self.MaxMaxTrajectory);
  result.o.AddMemberPrimitive('MinMaxTrajectory',   self.MinMaxTrajectory);
  result.o.AddMemberPrimitive('MaxMinTrajectory',   self.MaxMinTrajectory);
  result.o.AddMemberPrimitive('MinMinTrajectory',   self.MinMinTrajectory);
  result.o.AddMemberPrimitive('MeanTrajectory',   self.MeanTrajectory);
  result.o.AddMemberPrimitive('FAvgMaxTrajectory',   self.FAvgMaxTrajectory);
  result.o.AddMemberPrimitive('FAvgMinTrajectory',   self.FAvgMinTrajectory);
  result.o.AddMemberPrimitive('AdjustedTrajectory',   self.AdjustedTrajectory);
  result.o.AddMemberPrimitive('Samples',   self.Samples);

//  result := StrToJSONh(rec2json.RecToJSon(@self, TypeInfo(TSignalTragectory)));
end;

{ TPriceLine }

procedure TPriceLine.ApplyPrices(bid, ask: double);
begin
  if Samples = 0 then begin
    MinBid := bid;
    MinAsk := ask;
    MaxBid := bid;
    MaxAsk := ask;
    AvgBid := bid;
    AvgAsk := ask;
    EnterBid := bid;
    EnterAsk := ask;
  end else begin
    MinBid := lesserof(MinBid, bid);
    MinAsk := lesserof(MinAsk, ask);
    MaxBid := greaterof(MaxBid, bid);
    maxAsk := greaterof(maxask, ask);
    avgBid := avgBid + bid;
    avgAsk := avgAsk + ask;
  end;

  inc(samples);
  //Debug.Log('Samples:'+inttostr(samples)+' MaxBid:'+floatprecision(maxbid,8));


end;

function FindSignalBucket(a: Tarray<TSignalTragectory>; sig: double): TSignalTragectory;
var
  t: ni;
begin
  result.init(0);
  for t:= 0 to high(a) do
    if (a[t].xSignalMin <= sig) and (a[t].SignalMax >= sig) then begin
      exit(a[t]);
    end;

end;

procedure TPriceLine.init;
begin
  fillmem(pbyte(@self), sizeof(self), 0);
  samples := 0;
end;

{ TBuckets }

function TBuckets.FindBucketIndex(sig: double): ni;
//var
//  t: ni;
begin
  result := round(lesserof(high(a), sig * length(a)));
//  result := -1;
//  for t:= 0 to high(a) do
//    if (a[t].xSignalMin <= sig) and (a[t].SignalMax >= sig) then
//      exit(t);

end;

function TBuckets.FitTrajFromSignal(sig: double): double;
begin
  CalcFit;
  //stretch signal to bucket index (fractional allowed);
  sig := sig * Length(a);
  result := fr.GetPoint(sig);

end;

function TBuckets.CalcFit: TFitResults;
var
  dy: TArray<double>;
  dx: TArray<double>;
  t: ni;
begin
  setlength(dy, length(a));
  setlength(dx, length(a));
  for t:= 0 to high(a) do begin
    dy[t] := a[t].AdjustedTrajectory;
    dx[t] := t;
  end;




  result := maths.CalculateFit(dx, dy,false,4);
  self.fr := result;

end;

function TBuckets.GetHighestAmp: double;
begin
  result := a[GetHighestBucket].AvgMaxTrajectory;
end;

function TBuckets.GetHighestBucket: ni;
var
  t: ni;
begin
  result := 0;
  for t:= 0 to high(a) do
    if a[t].AvgMaxTrajectory > a[result].AvgMaxTrajectory then
      result := t;

end;

function TBuckets.GetLowestAmp: double;
begin
  result := a[GetLowestBucket].AvgMaxTrajectory;
end;

function TBuckets.GetLowestBucket: ni;
var
  t: ni;
begin
  result := high(a);
  for t:= 0 to high(a) do
    if a[t].AvgMaxTrajectory <= a[result].AvgMaxTrajectory then
      result := t;


end;

function TBuckets.GetUsableSignal(sigIn: double): double;
VAR
  mybucket: ni;
  lb, hb: ni;
begin
  myBucket := round(sigin*BUCKET_COUNT);
  if myBucket >= BUCKET_COUNT then myBucket := BUCKET_COUNT-1;
  lb := GetLowestBucket;
  hb := GetHighestBucket;
  result := interpolate(myBucket, 0.0, 1.0, lb, hb);


end;

function TBuckets.IsSemiUsable: boolean;
var
  hb:ni;
  lb:ni;
begin
  lb := GetLowestBucket;
  hb := GetHighestBucket;
  if lb >= hb then exit(false);
  if hb < 50 then exit(false);
  exit(true);
end;

function TBuckets.IsUsable: boolean;
var
  hb:ni;
  lb:ni;
  t: ni;
  above: ni;
  below: ni;
begin
  if length(fr.terms) = 0 then
    CalcFit;


  above := 0;
  below := 0;
  for t:= 0 to high(a) do begin
    if FittrajFromSignal(t/high(a)) > 0.0 then inc(above);
    if FittrajFromSignal(t/high(a)) < 0.0 then inc(below);
  end;

  result := (above>0) and (below>0);



end;

{ TTweet }

function TTweet.DBValueString: string;
begin
  result :=
    '('+GetStorageString(varInt64, id)+','+
    GetStorageString(varDate, created_on)+','+
    GetStorageString(varString, coin)+','+
    GetStorageString(varString, tweet)+','+
    GetStorageString(varString, json)+','+
    GetStorageString(varDate, found_on)+')';


end;

{ TTwitterFollow }

procedure TTwitterFollow.Init;
begin
  coin := '';
  twitter := '';
end;

{ Tcmd_UpgradeDatabase }

procedure Tcmd_UpgradeDatabase.DoExecute;
begin
  inherited;

  db := dbprov.need;
  try
    startversion := db.functionquery('select number from version where id=0',0.0);

    if startVersion < 2.0 then
      UpgradeTo2;



  finally
    db.free;
  end;


end;

procedure Tcmd_UpgradeDatabase.InitExpense;
begin
  inherited;
  cpuexpense := 0.0;
  Resources.SetResourceUsage('DB', 1/32);
end;

procedure Tcmd_UpgradeDatabase.UpgradeTo2;
var
  rsCoinsInStats: IHolder<TSERowset>;
  c: Tcmd_UpgradeCoinStats;
  cs: array of Tcmd_UpgradeCoinStats;
  t: ni;
begin
  Status := 'Upgrade to v2...';
  rsCoinsInStats := db.ReadQueryH('select distinct marketcoin from coinstats');
  setlength(cs, rsCoinsInStats.o.RowCount);
  rsCoinsInStats.o.first;
  while not rsCoinsInStats.o.eof do begin
    try
      c := Tcmd_UpgradeCoinStats.Create;
      c.coin := rsCoinsInStats.o.CurRecordFields['marketcoin'];
      c.start;
      cs[rsCoinsInStats.o.Cursor] := c;
    finally
      rsCoinsInStats.o.next;
    end;
  end;

  for t:= 0 to high(cs) do begin
    StepCount := high(cs);
    Step := t;
    cs[t].WaitFor;
    cs[t].free;
    cs[t] := nil;
  end;

  db.WriteQuery('update version set number = 2.0 where id =0');



end;

{ Tcmd_UpgradeCoinStats }

procedure Tcmd_UpgradeCoinStats.DoExecute;
var
  rsclasses: IHolder<TSERowSEt>;
  cls: string;
  newt: string;
  oldt: string;
begin
  inherited;
  db := dbprov.need;
  try
    rsClasses := db.ReadQueryH('select distinct messageclass from coinstats');
    rsClasses.o.first;
    StepCount := rsClasses.o.RowCount;
    while not rsClasses.o.EOF do begin
      try
        Step := rsClasses.o.Cursor;
        cls := rsClasses.o.CurRecordFields['messageclass'];
        newt := 'coinstats_'+coin+'_'+cls;
        oldt := 'coinstats_'+cls;
//        db.WriteQuery('drop table if exists '+newt);
        db.WriteQuery('create table if not exists '+newt+' like coinstats_template');
//        db.WriteQuery('insert ignore into '+newt+' select * from '+oldt+' where marketcoin='+quote(coin));

      finally
        rsClasses.o.Next;
      end;
    end;

    rsClasses.o.first;
    StepCount := rsClasses.o.RowCount;
    while not rsClasses.o.EOF do begin
      try
        Step := rsClasses.o.Cursor;
        cls := rsClasses.o.CurRecordFields['messageclass'];
        newt := 'coinstats_'+coin+'_'+cls;
        oldt := 'coinstats_'+cls;
//        db.WriteQuery('drop table if exists '+newt);
//        db.WriteQuery('create table if not exists '+newt+' like coinstats_template');
        db.WriteQuery('insert ignore into '+newt+' select * from '+oldt+' where marketcoin='+quote(coin));

      finally
        rsClasses.o.Next;
      end;
    end;

  finally
    db.free;
  end;
end;

procedure Tcmd_UpgradeCoinStats.InitExpense;
begin
  inherited;
  cpuexpense := 0.0;
  Resources.SetResourceUsage('DB', 1/32);
end;

initialization
  init.registerprocs('ShapeShiftDB', oinit, ofinal, 'SimpleReliableUDP');



end.
