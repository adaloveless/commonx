unit guitar_problem_solver;
{$DEFINE SAVE_ONLY_BAD_SNAPS}
{$DEFINE CACHE_GOOD_SNAPS_LOCALLY}
interface

uses
  classes, numbers, debug, typex, systemx, stringx, z2guitar, betterobject, commandprocessor, abstractdb, generics.collections.fixed,namevaluepair, simpledb, sysutils, sharedobject, rdtpdb;



type
  TLocalDBCursor = TSERowSetCursor;
  TLocalDB = TRDTPDB;
  TGuitarProblemSolver = class;//forward

  TGuitarPositionFail = record
    waspressed: boolean;
    expectedpressed: boolean;
    failed_state: ni;
    care: boolean;
    stringid, fretid: ni;
  end;
  PGuitarPositionFail = ^TGuitarPositionFail;

  TGuitarFailureAnal = record
    positions: array[0..5] of array [0..4] of TGuitarPositionFail;
    snap: TGuitarData;
  end;

  TZ2GuitarDataProvider = class(TSharedObject)
  public
    db: TLocalDB;
    gd: TGuitarData;
    er: TExpectedTestResults;
    constructor Create;override;
    destructor Destroy;override;
    function AreFilesAvailable(iTestingID: ni; iEvalID: ni): boolean;
    procedure BuildFilesFromDatabase(iTestingID, iEvalID: ni);
    procedure ProvideData(iTestingID, iEvalID: ni);
    procedure CleanDataFiles(iTestingID: ni);
    function CommitCacheToDatabase(iTestingID, iEvalID: ni): boolean;

  end;

  TZ2DataAnalysis = class(TBetterObject)
  public
    function NeedsAnal: boolean;virtual;
    procedure CaptureData(gps: TGuitarProblemSolver);
    procedure DoCaptureData(gps: TGuitarProblemSolver);virtual;
    procedure RunLogic(gps: TGuitarProblemSolver);
    procedure DoRunLogic(gps: TGuitarProblemSolver);virtual;
    procedure AssimilateRecommendations(gps: TGuitarProblemSolver);
    procedure DoAssimilateRecommendations(gps: TGuitarProblemSolver);virtual;
    procedure OnNewGuitar_CaptureData(gps: TGuitarProblemSolver; sDevice: string);virtual;
    procedure OnNewGuitar_AssimilateRecommendations(gps: TGuitarProblemSolver; sDevice: string);virtual;
    procedure OnNewGuitar_RunLogic(gps: TGuitarProblemSolver; sDevice: string);virtual;


  end;

  TAnalHandTweaked = class(TZ2DataAnalysis)
  private
    FBAseCalibration: TNameValuePairList;
    FCalFile: string;
  public
    constructor Create;override;
    destructor Destroy;override;
    property CalFile: string read FCalFile write FCalFile;
    procedure DoCaptureData(gps: TGuitarProblemSolver);override;
    procedure DoAssimilateRecommendations(gps: TGuitarProblemSolver);override;
  end;

  Trecommend = record
    Direct, Diverse: nativefloat;
    class operator add(a,b: TRecommend): TRecommend;
  end;

  TGuitarCAlibration = record
    Recommendations: array[0..5]of array [0..19] of Trecommend;
    procedure AddArray(var cal: TGuitarCalibration);
    procedure Init;
  end;
  PGuitarCalibration = ^TGuitarCalibration;

  TAnalAutoTweaker = class(TZ2DataAnalysis)
  private
    cal: TGuitarCAlibration;
    FLogicMode: ni;
  public
    function NeedsAnal: boolean;override;
    procedure Solveproblems(gps: TGuitarProblemSolver);
    procedure SolveFalseNegative(gps: TGuitarProblemSolver; t,u: ni; deet: PGuitarPositionFail);
    procedure SolveFalsePositive(gps: TGuitarProblemSolver; t,u: ni; deet: PGuitarPositionFail);

    procedure DoCaptureData(gps: TGuitarProblemSolver);override;
    procedure DoAssimilateRecommendations(gps: TGuitarProblemSolver);override;
    property LogicMode: ni read FLogicMode write FLogicMode;
  end;

  TAnalHoodTweaker = class(TZ2DataAnalysis)
  private
    cal: TGuitarCAlibration;
    FLinearOffset: boolean;
    FRatio: nativefloat;
  public
    procedure Init;override;
    function NeedsAnal: boolean;override;
    procedure OnNewGuitar_AssimilateRecommendations(gps: TGuitarProblemSolver; sDevice: string);override;
    procedure DoAssimilateRecommendations(gps: TGuitarProblemSolver);override;
    property LinearOffset: boolean read FLinearOffset write FLinearOffset;
    property Ratio: nativefloat read FRatio write FRatio;
  end;

  TCalObj = class(TBetterObject)
  public
    obj: TguitarCalibration;
  end;

  TCalibrationDictionary = class(TBetterObject)
  private
    function GetCount: ni;
  protected
    FList: TStringlist;
    function GetGuitarByIndex(idx: ni): PGuitarCalibration;
    function IndexOfMac(sMac: string): ni;
    function GetguitarByMac(sMac: string): PGuitarCalibration;
    function Add(mac: string): PGuitarCalibration;//x
  public
    procedure Clear;
    constructor Create;override;//x
    destructor Destroy;override;//x

    property Guitars[sMac: string]: PGuitarCalibration read GetGuitarByMac;
    property Count: ni read GetCount;


  end;

  TAnalAutoTweaker_PerGuitar = class(TZ2DataAnalysis)
  private
    cal: TCalibrationDictionary;
    FLogicMode: ni;
    function GetGuitar(sDevice: string): PGuitarCalibration;
  public
    function NeedsAnal: boolean;override;
    constructor Create;override;
    destructor Destroy;override;
    procedure Solveproblems(gps: TGuitarProblemSolver);
    procedure SolveFalseNegative(gps: TGuitarProblemSolver; t,u: ni; deet: PGuitarPositionFail);
    procedure SolveFalsePositive(gps: TGuitarProblemSolver; t,u: ni; deet: PGuitarPositionFail);

    procedure DoCaptureData(gps: TGuitarProblemSolver);override;
    procedure DoAssimilateRecommendations(gps: TGuitarProblemSolver);override;
    property LogicMode: ni read FLogicMode write FLogicMode;
    property Guitars[sDevice: string]: PGuitarCalibration read GetGuitar;
  end;

  TAnalCertainLightPlusEdge = class(TZ2DataAnalysis)
  public
    procedure DoRunLogic(gps: TGuitarProblemSolver);override;
  end;

  TAnalCertainLightPlusEdge_AKS = class(TZ2DataAnalysis)
  private
    FAKS: ni;
    FAKSE: ni;
    FLogicType: ni;
  public
    procedure Init;override;
    procedure DoRunLogic(gps: TGuitarProblemSolver);override;
    property AKS_Amount: ni read FAKS write FAKS;
    property AKS_Edge_Amount: ni read FAKSE write FAKSE;
    property LogicType: ni read FLogicType write FLogicType;
  end;

  TAnalCertainLightPlusEdge_RAR_AKS = class(TZ2DataAnalysis)
  private
    FAKS: ni;
  public
    procedure DoRunLogic(gps: TGuitarProblemSolver);override;
    property AKS_Amount: ni read FAKS write FAKS;
  end;



  TAnalRAR = class(TZ2DataAnalysis)
  public
    procedure DoRunLogic(gps: TGuitarProblemSolver);override;
  end;


  TAnalCertainLightPlusEdgeAndHalfMute = class(TZ2DataAnalysis)
  public
    procedure DoRunLogic(gps: TGuitarProblemSolver);override;
  end;


  TGuitarAccuracyResults = record
    snapsChecked: ni;
    snapsPassed: ni;
    fingerpositionsChecked:ni;
    fingerpositionsPassed: ni;
    procedure Init;
  end;


  TGuitarProblemSolver = class (TCommand)
  protected
    FAnals: TList<TZ2DataAnalysis>;
    FDevices: TNameValuePairList;
    procedure DestroyAnals;
  public
    sdb: TAbstractDB;
    gResult: TGuitarData;
    gCurrent: TGuitarData;
    gCurrentExp: TExpectedTestResults;
    finalsnap: TGuitarData;
    testingid, evalid: int64;
    mac: string;
    accuracy: TGuitarAccuracyResults;
    devicefilter: string;

    procedure RunAnals;
    procedure RunLogics;
    procedure AssimilateCalibrations;

    procedure NewGuitar_RunAnals;
    procedure NewGuitar_RunLogics;
    procedure NewGuitar_AssimilateCalibrations;


    procedure LoadTestingOLD(iTestingID: ni);
    procedure LoadExpectedOLD(iTestingID: ni);
    procedure TallyTestResults;
    procedure RecordMistake(istring, iFret: ni);
    procedure SaveSnap(bIsBad: boolean);


    procedure DoExecute;override;
    function AddAnal<T:TZ2DataAnalysis, constructor>: T;
    constructor Create;override;
    destructor Destroy;override;
    procedure LoadData(iTestingID: int64);
    function AnyNeedsAnal: boolean;
  end;

function guitarResultIsExpected(var gCurrent: TguitarDAta; var gCurrentExp: TExpectedTestResults; out gOutResultDeets: TGuitarFailureAnal): boolean;
function GuitFile(iTestingID, iEvalID: ni): string;
function ExpFile(iTestingID, iEvalID: ni): string;


var
  GDP: TZ2GuitarDataProvider;
  accumulated_recommendations: TGuitarCAlibration;


function GuitarDAtaPath: string;


implementation

{ TZ2DataAnalysis }

procedure TZ2DataAnalysis.AssimilateRecommendations(gps: TGuitarProblemSolver);
begin
  DoAssimilateRecommendations(gps);
end;

procedure TZ2DataAnalysis.CaptureData(gps: TGuitarProblemSolver);
begin
  DoCaptureData(gps);
end;


procedure TZ2DataAnalysis.DoAssimilateRecommendations(gps: TGuitarProblemSolver);
begin
//
end;

procedure TZ2DataAnalysis.DoCaptureData(gps: TGuitarProblemSolver);
begin
//
end;

procedure TZ2DataAnalysis.DoRunLogic(gps: TGuitarProblemSolver);
begin
//
end;

function TZ2DataAnalysis.NeedsAnal: boolean;
begin
  result := false;
end;

procedure TZ2DataAnalysis.OnNewGuitar_AssimilateRecommendations(
  gps: TGuitarProblemSolver; sDevice: string);
begin
///
end;

procedure TZ2DataAnalysis.OnNewGuitar_CaptureData(gps: TGuitarProblemSolver;
  sDevice: string);
begin
///
end;

procedure TZ2DataAnalysis.OnNewGuitar_RunLogic(gps: TGuitarProblemSolver;
  sDevice: string);
begin
///
end;

procedure TZ2DataAnalysis.RunLogic(gps: TGuitarProblemSolver);
begin
  DoRunLogic(gps);
end;

{ TGuitarProblemSolver }

function TGuitarProblemSolver.AddAnal<T>: T;
begin
  result := T.create;
  FAnals.add(result);

end;

function TGuitarProblemSolver.AnyNeedsAnal: boolean;
var
  t: ni;
begin
  result := false;
  for t:= 0 to Self.FAnals.count-1 do begin
    if FAnals[t].NeedsAnal then begin
      result := true;
      break;
    end;
  end;
end;

procedure TGuitarProblemSolver.AssimilateCalibrations;
var
  t: ni;
begin
  for t:= 0 to Self.FAnals.count-1 do begin
    FAnals[t].AssimilateRecommendations(self);
  end;
end;

constructor TGuitarProblemSolver.Create;
begin
  inherited;
  FAnals := TList<TZ2DataAnalysis>.create;
end;

destructor TGuitarProblemSolver.Destroy;
begin
  DestroyAnals;
  sdb.free;
  FDevices.free;
  FDevices := nil;
  inherited;
end;

procedure TGuitarProblemSolver.DestroyAnals;
var
  z: TZ2DataAnalysis;
begin
  while FAnals.count > 0 do begin
    z := Fanals[0];
    Fanals.delete(0);
    z.free;
    z := nil;
  end;
end;

procedure TGuitarProblemSolver.DoExecute;
var
  dbc: TLocalDBCursor;
  aht: TAnalHandTweaked;
  base: TAnalCertainLightPlusEdge;
  iRec: ni;
  sWhere: string;
  sDevice: string;
  old_mac: string;
begin
  inherited;
  mac := '';
  gResult.init;
  gCurrent.Init;

  if sdb = nil then
    raise ECritical.create('please set sdb in TGuitarProblemSolver');

  accuracy.init;
  evalid := sdb.FunctionQuery('select max(evalid) from eval',0)+1;
  sdb.writebehind('insert into eval values('+inttostr(evalid)+',0,0,0,0)');

  sWhere := 'where testdate >= "2014-05-27 00:00:00"';

  if devicefilter <> '' then begin
    sDevice := 'device = "'+DeviceFilter+'"';
    if sWhere = '' then
      sWhere := sWhere + ' where '+sDevice
    else
      sWhere := sWhere + ' and '+sDevice;
  end;

  //query database for all snapshots
  dbc := sdb.ReadQueryDBC ('select testingid from testing join test on (testing.testid=test.testid) '+sWhere+' order by device'
//                           +' limit 1000'
                          )as TLocalDBCursor;

  FDevices.free;
  FDevices := nil;
  FDevices := sdb.ReadQueryNVPL('select testingid, device from testing');

  try
    stepcount := dbc.RecordCount;
    if AnyNeedsAnal then begin
      dbc.first;

      //walk through snapshots
      iRec := 0;
      while not dbc.Eof do begin
        old_mac := mac;
        LoadData(dbc['testingid']);
        //load the snapshot
  //      LoadTestingOLD(dbc['testingid']);
        //load expected results
  //      LoadExpectedOLD(dbc['testingid']);

        //run logic
        if old_mac <> mac then
          NewGuitar_RunAnals;

        RunAnals;

        //--------------
        inc(iRec);
        Step := iRec;
        dbc.Next;
  //      if iRec = 1000 then break;
      end;
    end;

    //walk through snapshots AGAIN
    dbc.first;
    iRec := 0;
    while not dbc.Eof do begin
      old_mac := mac;
      LoadData(dbc['testingid']);
      //load the snapshot
//      LoadTesting(dbc['testingid']);
      //load expected results
//      LoadExpected(dbc['testingid']);
      //debug.consolelog(inttostr(gCurrent.strings[0].flatsensors[0].reading.ledon));

      if old_mac <> mac then
        NewGuitar_AssimilateCalibrations;
      //assimilate calibration
      AssimilateCalibrations;
      //run logic
      gCurrent.RemoteLogic := false;
      if old_mac <> mac then
        NewGuitar_RunLogics;
      RunLogics;

      //evaluate whether the logic results in expected results
      TallyTestResults;


      //--------------

      inc(iRec);
      Step := iRec;
      dbc.Next;
//      if iRec = 1000 then break;

    end;
  finally
    dbc.free;
  end;



  //iterate through the records of the database based on some kind of logical filter

    //load each record into a structure

    //


end;

procedure TGuitarProblemSolver.LoadData(iTestingID: int64);
begin
  gdp.Lock;
  try
    gdp.ProvideData(iTestingID,0);
    mac := FDevices.GetItemEx(inttostr(iTestingID), '');
    self.testingid := iTestingID;
    gCurrent.FromGuitar(gdp.gd);
    gCurrentExp := gdp.er;
  finally
    gdp.unlock;
  end;
end;

procedure TGuitarProblemSolver.LoadExpectedOLD(iTestingID: ni);
var
  iTestID: ni;
  dbc: TLocalDBCursor;
begin
  iTestID := sdb.FunctionQuery('select testid from testing where testingid='+inttostr(iTestingID),0);

//  dbc := sdb.ReadQueryDBC('select * from test_expected_results where testid='+sTestID) as TDBCursor;

  gCurrentExp.Init(iTestID, -1);
  gCurrentExp.LoadFromDB(sdb);



end;

procedure TGuitarProblemSolver.LoadTestingOLD(iTestingID: ni);
var
  dbc: TAbstractDBCursor;
begin
  dbc := sdb.ReadQueryDBC('select * from sensors s join frets f on (f.evalid=s.evalid and f.testingid=s.testingid and f.fretid=s.fretid and f.stringid=s.stringid) where s.testingid='+inttostr(iTestingID));
  try
    gCurrent.LoadStateFromDataSet(dbc);
    gCurrent.LoadFretStateFromDataSet(dbc);
    testingid := iTestingid;
  finally
    dbc.free;
  end;


end;


procedure TGuitarProblemSolver.NewGuitar_AssimilateCalibrations;
var
  t: ni;
begin
  for t:= 0 to Self.FAnals.count-1 do begin
    FAnals[t].OnNewGuitar_AssimilateRecommendations(self, mac);
  end;
end;
procedure TGuitarProblemSolver.NewGuitar_RunAnals;
var
  t: ni;
begin
  for t:= 0 to Self.FAnals.count-1 do begin
    FAnals[t].OnNewGuitar_CaptureData(self, mac);
  end;
end;

procedure TGuitarProblemSolver.NewGuitar_RunLogics;
var
  t: ni;
begin
  for t:= 0 to Self.FAnals.count-1 do begin
    FAnals[t].OnNewGuitar_RunLogic(self, mac);
  end;
end;
procedure TGuitarProblemSolver.RecordMistake(istring, iFret: ni);
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TGuitarProblemSolver.RunAnals;
var
  t: ni;
begin
  for t:= 0 to Self.FAnals.count-1 do begin
    FAnals[t].CaptureData(self);
  end;
end;

procedure TGuitarProblemSolver.RunLogics;
var
  t: ni;
begin
  gCurrent.remotelogic := (self.FAnals.Count = 0);

  for t:= 0 to Self.FAnals.count-1 do begin
    FAnals[t].RunLogic(self);
  end;
end;

procedure TGuitarProblemSolver.SaveSnap(bIsBad: boolean);
begin
  if Fanals.count = 0 then begin
    sdb.WriteQuery('update ignore testing set pass='+inttostr(booltoint(not bIsBad))+' where testingid='+inttostr(testingid));
  end else begin
{$IFDEF SAVE_ONLY_BAD_SNAPS}
    if bIsBad or (gCurrentExp.testid= 30) or (gCurrentExp.testid= 1059) or (gCurrentExp.testid=1060) then
{$ENDIF}
      gCurrent.SaveToDB(true, sdb, '_eval', testingid, evalid, bIsBad);

{$IFDEF CACHE_GOOD_SNAPS_LOCALLY}
    gCurrent.SavetoFile(guitfile(testingid, evalid));
{$ENDIF}
//    gCurrentExp.Savetobin(expfile(testingid, evalid));

  end;
end;

function guitarResultIsExpected(var gCurrent: TguitarDAta; var gCurrentExp: TExpectedTestResults; out gOutResultDeets: TGuitarFailureAnal): boolean;
var
  t, u,v: ni;
  a,b: PguitarSensor;
  bgoodPos: boolean;
  bGoodSnap: boolean;
  bCare: boolean;
  bHalfMute: boolean;
begin
  FillMem(@gOutResultdeets, sizeof(goutresultdeets), 0);

  bGoodSnap := true;
  for u := 0 to 5 do begin
    for t := 4 downto 0 do begin
      gOutResultDeets.positions[u][t].stringid := u;
      gOutResultDeets.positions[u][t].fretid := t;
    end;
  end;

  for u := 0 to 5 do begin
    for t := 4 downto 0 do begin


      bCare := true;
      bGoodPOs := true;
      bHalfMute :=    (0 <> (gCurrent.strings[u].frets[t].state and (FRET_STATE_SIMPLE_DIRECT_HALF or FRET_STATE_SIMPLE_DIVERSE_HALF)))
                  and (0 = (gCurrent.strings[u].frets[t].state and (FRET_STATE_SIMPLE_DIRECT or FRET_STATE_SIMPLE_DIVERSE)));

      IF NOT bHalfMute then
      case gCurrentExp.x[u][t] of
        0: bGoodPos := not gCurrent.strings[u].frets[t].IsPressed;
        1: bGoodPos := gCurrent.strings[u].frets[t].IsPressed;
        -1: begin bGoodPos := true;
          bCare := false;
        end;
      end;

      if bCare then begin
        gOutResultDeets.positions[u][t].care := true;
        if not bGoodPos then begin
          bGoodSnap := false;
          gOutResultDeets.positions[u][t].waspressed := gCurrent.strings[u].frets[t].IsPressed;
          gOutResultDeets.positions[u][t].expectedpressed := gCurrentExp.x[u][t] = 1;
          gOutResultDeets.positions[u][t].failed_state := gcurrent.strings[u].frets[t].final_state;
        end else begin
          gOutResultDeets.positions[u][t].waspressed := gCurrent.strings[u].frets[t].IsPressed;
          gOutResultDeets.positions[u][t].expectedpressed := gCurrentExp.x[u][t] = 1;
        end;

      end;



      //STOP evaluating this string at the highest expected pressed fret
      if gCurrent.strings[u].frets[t].IsPressed or bHalfMute then
        break;

    end;
  end;






  result := bGoodSnap;

end;

procedure TGuitarProblemSolver.TallyTestResults;
var
  t, u: ni;
  bgoodPos: boolean;
  bGoodSnap: boolean;
  bCare: boolean;
  bHalfMute: boolean;
begin
  bGoodSnap := true;
  for u := 0 to 5 do begin
    for t := 4 downto 0 do begin
      bCare := true;
      bGoodPOs := true;
      bHalfMute :=    (0 <> (gCurrent.strings[u].frets[t].state and (FRET_STATE_SIMPLE_DIRECT_HALF or FRET_STATE_SIMPLE_DIVERSE_HALF)))
                  and (0 = (gCurrent.strings[u].frets[t].state and (FRET_STATE_SIMPLE_DIRECT or FRET_STATE_SIMPLE_DIVERSE)));

      IF NOT bHalfMute then
      case gCurrentExp.x[u][t] of
        0: bGoodPos := not gCurrent.strings[u].frets[t].IsPressed;
        1: bGoodPos := gCurrent.strings[u].frets[t].IsPressed;
        -1: begin bGoodPos := true;
          bCare := false;
        end;
      end;

      if bCare then begin
        if not bGoodPos then begin
          RecordMistake(u,t);
          bGoodSnap := false;
        end else begin
          inc(accuracy.fingerpositionsPassed);
        end;
        inc(accuracy.fingerpositionsChecked);
      end;

      //STOP evaluating this string at the highest pressed fret
      if gCurrent.strings[u].frets[t].IsPressed or bHalfMute then
        break;

    end;
  end;

  if bGoodSnap then
    inc(accuracy.snapsPassed);
  inc(accuracy.snapsChecked);

  if (accuracy.snapsChecked <> 0) and (accuracy.fingerpositionsChecked <> 0) then begin
    Status := 'Snap: '+inttostr(accuracy.snapsPassed)+'/'+inttostr(accuracy.snapsChecked)+' Finger:'+inttostr(accuracy.fingerpositionsPassed)+'/'+inttostr(accuracy.fingerpositionsChecked);
  end;


  SaveSnap(not bGoodSnap);

end;

{ TAnalHandTweaked }

constructor TAnalHandTweaked.Create;
begin
  inherited;
  FBAseCalibration := TNameValuePairList.create;

end;

destructor TAnalHandTweaked.Destroy;
begin
  FBaseCalibration.free;
  inherited;
end;

procedure TAnalHandTweaked.DoAssimilateRecommendations(gps: TGuitarProblemSolver);
var
  nvpl: TNameValuePairList;
begin
  inherited;
//  finalsnap.LoadCalibrationFromNVPL(self.FBAseCalibration);
  nvpl := TNameValuePairList.create;
  try
    NVPL.LoadFromFile(CalFile);
    gps.gCurrent.LoadCalibrationFromNVPL(nvpl, true);
  finally
    nvpl.free;
  end;


end;

procedure TAnalHandTweaked.DoCaptureData(gps: TGuitarProblemSolver);
begin
  inherited;
  gps.gCurrent.LoadCalibrationFromFile(CalFile);
end;

{ TGuitarAccuracyResults }

procedure TGuitarAccuracyResults.Init;
begin
  fillmem(@self, sizeof(self), 0);
end;

{ TAnalCertainLightPlusEdge }

procedure TAnalCertainLightPlusEdge.DoRunLogic(gps: TGuitarProblemSolver);
begin
  inherited;
  gps.gCurrent.LogicType := 0;
  gps.gCurrent.UpdateState;
end;

{ TAnalCertainLightPlusEdgeAndHalfMute }

procedure TAnalCertainLightPlusEdgeAndHalfMute.DoRunLogic(
  gps: TGuitarProblemSolver);
begin
  inherited;
  gps.gCurrent.enablehalfmute := true;
  gps.gCurrent.UpdateState;

end;

{ TZ2GuitarDataProvider }

function TZ2GuitarDataProvider.AreFilesAvailable(iTestingID: ni; iEvalID: ni): boolean;
begin
//  Lock;
//  try
    result := FileExists(guitFile(iTestingID, iEvalID)) and (FileExists(expfile(iTestingID, iEvalID)));
//  finally
//    unlock;
//  end;
end;

procedure TZ2GuitarDataProvider.BuildFilesFromDatabase(iTestingID, iEvalID: ni);
var
  gd: TGuitarDAta;
  er: TExpectedTestResults;
  iTestID: ni;
begin
  Lock;
  try
    gd.init;
    gd.LoadFromDB(self.db, iTestingID, '', iEvalID);
    iTestID := db.FunctionQuery('select testid from testing where testingid='+inttostr(iTestingID)+' limit 1', 0);

    er.testid := iTestid;
    er.LoadFromDb(db);

    gd.SavetoFile(guitfile(iTestingID, iEvalID));
    er.Savetobin(expfile(iTestingID, iEvalID));
  finally
    Unlock;
  end;


end;

procedure TZ2GuitarDataProvider.CleanDataFiles(iTestingID: ni);
var
  t: ni;
begin
  for t:= 0 to 999 do begin
    if fileexists(guitfile(iTestingID,t)) then
      deletefile(guitfile(iTestingID,t));

    if fileexists(expfile(iTestingID,t)) then
      deletefile(expfile(iTestingid,t));
  end;
end;

function TZ2GuitarDataProvider.CommitCacheToDatabase(iTestingID, iEvalID: ni): boolean;
var
  gd: TGuitarDAta;
  sFile: string;
begin
  Lock;
  try
    gd.init;

    sFile := guitfile(iTestingID, iEvalID);
    result := false;
    if fileExists(sFile) then begin
      gd.LoadFromFile(sFile);
      gd.SaveToDB(true, self.db, '_eval', iTestingID, iEvalID);
      result := true;
    end;

  finally
    Unlock;
  end;


end;

constructor TZ2GuitarDataProvider.Create;
begin
  inherited;
  db := Trdtpdb.create;
{$IFNDEF LOCALMIDDLEWARE}
  db.MiddleWareAddress := 'sourcecontrol.zivix.net';
{$ENDIF}
  db.MiddleWarePort := '235';

  db.Connect('sourcecontrol.zivix.net', 'zivix', 'root','shad0ws');
end;

destructor TZ2GuitarDataProvider.Destroy;
begin

  inherited;
end;

function ExpFile(iTestingID, iEvalID: ni): string;
begin
  result := guitardatapath+inttostr(iTestingID)+'_'+inttostr(iEvalID)+'.exp'
end;

function GuitFile(iTestingID, iEvalID: ni): string;
begin
  result := guitardatapath+inttostr(iTestingID)+'_'+inttostr(iEvalID)+'.guit';
end;

procedure TZ2GuitarDataProvider.ProvideData(iTestingID,
  iEvalID: ni);
begin
  gd.Init;
  if not Self.AreFilesAvailable(iTestingID, iEvalID) then begin
    Lock;
    try
      if not Self.AreFilesAvailable(iTestingID, iEvalID) then begin
        BuildFilesFromDatabase(iTestingID, iEvalID);
      end;
    finally
      Unlock;
    end;
  end;
  if not self.arefilesavailable(iTestingID, iEvalID) then
    raise ECRitical.create('failed to build files from database');

  gd.LoadFromFile(guitfile(iTestingId, iEvalID));
//  if gd.strings[0].flatsensors[0].reading.reflection > 255 then
//    raise ECritical.create('wtf.');
  er.LoadFromBin(expfile(iTestingID, iEvalid));

end;


function GuitarDataPath: string;
begin
  result := slash(dllpath)+'GuitarDataCache\';
end;

{ TAnalAutoTweaker }

procedure TAnalAutoTweaker.DoAssimilateRecommendations(gps: TGuitarProblemSolver);
var
  t,u: ni;
begin
  inherited;
  for t:= 0 to 5 do begin
    for u := 0 to 19 do begin
      gps.gCurrent.strings[t].flatsensors[u].reading.CDirect :=
          gps.gCurrent.strings[t].flatsensors[u].reading.CDirect + round(cal.recommendations[t][u].Direct);
      gps.gCurrent.strings[t].flatsensors[u].reading.LocalCDir :=
          gps.gCurrent.strings[t].flatsensors[u].reading.CDirect + round(cal.recommendations[t][u].Direct);

      gps.gCurrent.strings[t].flatsensors[u].reading.CDiv :=
          gps.gCurrent.strings[t].flatsensors[u].reading.CDiv + round(cal.recommendations[t][u].Diverse);
      gps.gCurrent.strings[t].flatsensors[u].reading.LocalCDiv :=
          gps.gCurrent.strings[t].flatsensors[u].reading.CDiv + round(cal.recommendations[t][u].Diverse);


    end;
  end;


end;

procedure TAnalAutoTweaker.DoCaptureData(gps: TGuitarProblemSolver);
begin
  inherited;
  gps.gCurrent.LogicType := LogicMode;
  gps.gCurrent.RemoteLogic := false;
  gps.gcurrent.UpdateState;

  SolveProblems(gps);



end;

function TAnalAutoTweaker.NeedsAnal: boolean;
begin
  result := true;
end;

procedure TAnalAutoTweaker.SolveFalseNegative(gps: TGuitarProblemSolver; t,
  u: ni; deet: PGuitarPositionFail);
var
  deets: TGuitarFailureAnal;
  strong: ni;
  d,fret, str,sen: ni;
  tt: ni;
  cl, mx, mx_t: ni;
  bEdge: boolean;
  adjust_dir, adjust_h: ni;
begin

  fret := deet.fretid;
  str := deet.stringid;


  //if the position isn't contacted, give up...we can't help you
  if gps.gcurrent.strings[str].frets[fret].contact = 0 then
    exit;


  mx := 0;
  mx_t := 0;
  //determine strongest sensor
  for tt:= 0 to gps.gCurrent.strings[str].frets[fret].SensorCount-1 do begin
    cl := gps.gCurrent.strings[str].frets[fret].sensors[tt].reading.LocalCertainLight;
    if mx <  cl then begin
      mx := cl;
      mx_t := tt;
    end;
  end;
  sen := fret_sensor_base[fret]+mx_t;

  //is it an edge?  tweak the edge reading downwards until problem is solved... count number of decrements
  bEdge := (mx_t = 0) or (mx_t = gps.gCurrent.strings[str].frets[fret].sensorcount);



  if bEdge then begin
    adjust_h := 0;
    while not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) do begin
      //check the specific position and break, because there could be multiple bad ones
      if deets.positions[str][fret].waspressed = deets.positions[str][fret].expectedpressed then
        break;

      d := gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDiv;
      dec(d);
      gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDiv := d;
      gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.LocalCDiv := d;
      dec(adjust_h);
      if adjust_h < -80 then
        break;

      gps.gcurrent.UpdateState;
    end;
  end else begin
    adjust_h := 9999999;
  end;


  adjust_dir := 0;
  while not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) do begin
    //check the specific position and break, because there could be multiple bad ones
    if deets.positions[str][fret].waspressed = deets.positions[str][fret].expectedpressed then
      break;
    d := gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDirect;
    dec(d);
    gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDirect := d;
    gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.LocalCDir := d;
    dec(adjust_dir);
    if adjust_dir < -80 then
      break;

    gps.gcurrent.UpdateState;
  end;

  if abs(adjust_h) < abs(adjust_dir) then begin
    if adjust_h > -80 then
      Self.cal.Recommendations[str][sen].Diverse := lesserof(adjust_h,Self.cal.Recommendations[str][sen].Direct);
  end else begin
    if adjust_dir > -80 then
      Self.cal.Recommendations[str][sen].Direct := lesserof(adjust_dir,Self.cal.Recommendations[str][sen].Direct);
  end;

end;

procedure TAnalAutoTweaker.SolveFalsePositive(gps: TGuitarProblemSolver; t,
  u: ni; deet: PGuitarPositionFail);
var
  deets: TGuitarFailureAnal;
  strong: ni;
  d,fret, str,sen: ni;
  tt: ni;
  cl, mx, mx_t: ni;
  bEdge: boolean;
  adjust_dir, adjust_h: ni;
begin

  fret := deet.fretid;
  str := deet.stringid;
  //if the position isn't contacted, give up...we can't help you
  if gps.gcurrent.strings[str].frets[fret].contact = 0 then
    exit;


  mx := 0;
  mx_t := 0;
  //determine strongest sensor
  for tt:= 0 to gps.gCurrent.strings[str].frets[fret].SensorCount-1 do begin
    cl := gps.gCurrent.strings[str].frets[fret].sensors[tt].reading.LocalCertainLight;
    if mx <  cl then begin
      mx := cl;
      mx_t := tt;
    end;
  end;
  sen := fret_sensor_base[fret]+mx_t;

  //is it an edge?  tweak the edge reading downwards until problem is solved... count number of decrements
  bEdge := (mx_t = 0) or (mx_t = gps.gCurrent.strings[str].frets[fret].sensorcount);



  if bEdge then begin
    adjust_h := 0;
    while not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) do begin
      //check the specific position and break, because there could be multiple bad ones
      if deets.positions[str][fret].waspressed = deets.positions[str][fret].expectedpressed then
        break;
      d := gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDiv;
      inc(d);
      gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDiv := d;
      gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.LocalCDiv := d;
      inc(adjust_h);
      if adjust_h > 80 then
        break;

      gps.gcurrent.UpdateState;
    end;
  end else begin
    adjust_h := 9999999;
  end;



  adjust_dir := 0;
  while not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) do begin
    //check the specific position and break, because there could be multiple bad ones
    if deets.positions[str][fret].waspressed = deets.positions[str][fret].expectedpressed then
      break;
    d := gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDirect;
    inc(d);
    gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDirect := d;
    gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.LocalCDir := d;
    inc(adjust_dir);
    if adjust_dir > 80 then
      break;

    gps.gcurrent.UpdateState;
  end;

  if abs(adjust_h) < abs(adjust_dir) then begin
    if adjust_h < 80 then begin
      Self.cal.Recommendations[str][sen].Diverse := greaterof(adjust_h,Self.cal.recommendations[str][sen].Diverse);
    end;
  end else begin
    if adjust_dir < 80 then begin
      Self.cal.Recommendations[str][sen].Direct := greaterof(adjust_dir,Self.cal.recommendations[str][sen].Direct);
    end;
  end;

end;

procedure TAnalAutoTweaker.Solveproblems(gps: TGuitarProblemSolver);
var
  deets: TGuitarFailureAnal;
  t,u: ni;
begin
  gps.gCurrent.LogicType := LogicMode;
  if not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) then begin
    for t:= 0 to 5 do begin
      for u := 0 to 4 do begin
        if deets.positions[t][u].care then begin
          if (gps.testingid=28728) and (t = 1) and (u = 2) then
            debug.consolelog('here');

          if deets.positions[t][u].expectedpressed then begin
            SolveFalseNegative(gps, t,u, @deets.positions[t][u]);
          end else begin
            SolveFalsePositive(gps, t,u, @deets.positions[t][u]);
          end;
        end;
      end;
    end;
  end;

end;

{ TAnalRAR }

procedure TAnalRAR.DoRunLogic(gps: TGuitarProblemSolver);
begin
  inherited;
  gps.gCurrent.LogicType := 1;
  gps.gCurrent.UpdateState;


end;

{ TAnalCertainLightPlusEdge_AKS }

procedure TAnalCertainLightPlusEdge_AKS.DoRunLogic(gps: TGuitarProblemSolver);
begin
  inherited;
  if gps.testingid=19161 then
    debug.consolelog('trap');
  gps.gCurrent.LogicType := Logictype;
  gps.gcurrent.aks_direct_amount := AKS_Amount;
  gps.gcurrent.aks_edge_amount := AKS_Edge_Amount;

  gps.gCurrent.UpdateState;

end;

procedure TAnalCertainLightPlusEdge_AKS.Init;
begin
  inherited;
  LogicType := 2;
end;

{ TGuitarCAlibration }

procedure TGuitarCAlibration.AddArray(var cal: TGuitarCalibration);
var
  t,u: ni;
begin
  for u := 0 to 5 do begin
    for t:= 0 to 19 do begin
      self.Recommendations[t][u] := self.Recommendations[t][u] + cal.Recommendations[t][u];
    end;
  end;

end;

procedure TGuitarCAlibration.Init;
begin
  fillmem(@self, sizeof(self), 0);
end;

{ Trecommend }

class operator Trecommend.add(a, b: TRecommend): TRecommend;
begin
  result.Direct := a.Direct + b.direct;
  result.Diverse := a.Diverse + b.Diverse;
end;

{ TAnalCertainLightPlusEdge_RAR_AKS }

procedure TAnalCertainLightPlusEdge_RAR_AKS.DoRunLogic(
  gps: TGuitarProblemSolver);
begin
  inherited;
  if gps.testingid=24076 then
    debug.consolelog('trap');

  gps.gCurrent.LogicType := 3;
  gps.gcurrent.aks_direct_amount := AKS_Amount;
  gps.gcurrent.aks_edge_amount := AKS_Amount;
  gps.gCurrent.UpdateState;

end;

{ TAnalAutoTweaker_PerGuitar }

constructor TAnalAutoTweaker_PerGuitar.Create;
begin
  inherited;
  cal := TCalibrationDictionary.create;
end;

destructor TAnalAutoTweaker_PerGuitar.Destroy;
begin
  cal.free;
  inherited;
end;

procedure TAnalAutoTweaker_PerGuitar.DoAssimilateRecommendations(
  gps: TGuitarProblemSolver);
var
  t,u: ni;
begin
  inherited;
  for t:= 0 to 5 do begin
    for u := 0 to 19 do begin
      gps.gCurrent.strings[t].flatsensors[u].reading.CDirect :=
          gps.gCurrent.strings[t].flatsensors[u].reading.CDirect + round(cal.Guitars[gps.mac].Recommendations[t][u].Direct);
      gps.gCurrent.strings[t].flatsensors[u].reading.LocalCDir :=
          gps.gCurrent.strings[t].flatsensors[u].reading.CDirect + round(cal.Guitars[gps.mac].Recommendations[t][u].Direct);

      gps.gCurrent.strings[t].flatsensors[u].reading.CDiv :=
          gps.gCurrent.strings[t].flatsensors[u].reading.CDiv + round(cal.Guitars[gps.mac].Recommendations[t][u].Diverse);
      gps.gCurrent.strings[t].flatsensors[u].reading.LocalCDiv :=
          gps.gCurrent.strings[t].flatsensors[u].reading.CDiv + round(cal.Guitars[gps.mac].Recommendations[t][u].Diverse);


    end;
  end;


end;

procedure TAnalAutoTweaker_PerGuitar.DoCaptureData(gps: TGuitarProblemSolver);
begin
  inherited;
  gps.gCurrent.LogicType := LogicMode;
  gps.gCurrent.RemoteLogic := false;
  gps.gcurrent.UpdateState;

  SolveProblems(gps);

end;

function TAnalAutoTweaker_PerGuitar.GetGuitar(
  sDevice: string): PGuitarCalibration;
var
  c: TGuitarCAlibration;
begin
  //do we have in dictionary?
  result := cal.GetguitarByMac(sDevice);


end;

function TAnalAutoTweaker_PerGuitar.NeedsAnal: boolean;
begin
  result := false;
end;

procedure TAnalAutoTweaker_PerGuitar.SolveFalseNegative(
  gps: TGuitarProblemSolver; t, u: ni; deet: PGuitarPositionFail);
var
  deets: TGuitarFailureAnal;
  strong: ni;
  d,fret, str,sen: ni;
  tt: ni;
  cl, mx, mx_t: ni;
  bEdge: boolean;
  adjust_dir, adjust_h: ni;
begin

  fret := deet.fretid;
  str := deet.stringid;


  //if the position isn't contacted, give up...we can't help you
  if gps.gcurrent.strings[str].frets[fret].contact = 0 then
    exit;


  mx := 0;
  mx_t := 0;
  //determine strongest sensor
  for tt:= 0 to gps.gCurrent.strings[str].frets[fret].SensorCount-1 do begin
    cl := gps.gCurrent.strings[str].frets[fret].sensors[tt].reading.LocalCertainLight;
    if mx <  cl then begin
      mx := cl;
      mx_t := tt;
    end;
  end;
  sen := fret_sensor_base[fret]+mx_t;

  //is it an edge?  tweak the edge reading downwards until problem is solved... count number of decrements
  bEdge := (mx_t = 0) or (mx_t = gps.gCurrent.strings[str].frets[fret].sensorcount);



  if bEdge then begin
    adjust_h := 0;
    while not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) do begin
      //check the specific position and break, because there could be multiple bad ones
      if deets.positions[str][fret].waspressed = deets.positions[str][fret].expectedpressed then
        break;

      d := gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDiv;
      dec(d);
      gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDiv := d;
      gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.LocalCDiv := d;
      dec(adjust_h);
      if adjust_h < -80 then
        break;

      gps.gcurrent.UpdateState;
    end;
  end else begin
    adjust_h := 9999999;
  end;


  adjust_dir := 0;
  while not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) do begin
    //check the specific position and break, because there could be multiple bad ones
    if deets.positions[str][fret].waspressed = deets.positions[str][fret].expectedpressed then
      break;
    d := gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDirect;
    dec(d);
    gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDirect := d;
    gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.LocalCDir := d;
    dec(adjust_dir);
    if adjust_dir < -80 then
      break;

    gps.gcurrent.UpdateState;
  end;

  if abs(adjust_h) < abs(adjust_dir) then begin
    if adjust_h > -80 then
      Self.guitars[gps.mac].recommendations[str][sen].Diverse := lesserof(adjust_h,Self.guitars[gps.mac].Recommendations[str][sen].Direct);
  end else begin
    if adjust_dir > -80 then
      Self.guitars[gps.mac].Recommendations[str][sen].Direct := lesserof(adjust_dir,Self.guitars[gps.mac].Recommendations[str][sen].Direct);
  end;

end;

procedure TAnalAutoTweaker_PerGuitar.SolveFalsePositive(
  gps: TGuitarProblemSolver; t, u: ni; deet: PGuitarPositionFail);
var
  deets: TGuitarFailureAnal;
  strong: ni;
  d,fret, str,sen: ni;
  tt: ni;
  cl, mx, mx_t: ni;
  bEdge: boolean;
  adjust_dir, adjust_h: ni;
begin

  fret := deet.fretid;
  str := deet.stringid;
  //if the position isn't contacted, give up...we can't help you
  if gps.gcurrent.strings[str].frets[fret].contact = 0 then
    exit;


  mx := 0;
  mx_t := 0;
  //determine strongest sensor
  for tt:= 0 to gps.gCurrent.strings[str].frets[fret].SensorCount-1 do begin
    cl := gps.gCurrent.strings[str].frets[fret].sensors[tt].reading.LocalCertainLight;
    if mx <  cl then begin
      mx := cl;
      mx_t := tt;
    end;
  end;
  sen := fret_sensor_base[fret]+mx_t;

  //is it an edge?  tweak the edge reading downwards until problem is solved... count number of decrements
  bEdge := (mx_t = 0) or (mx_t = gps.gCurrent.strings[str].frets[fret].sensorcount);



  if bEdge then begin
    adjust_h := 0;
    while not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) do begin
      //check the specific position and break, because there could be multiple bad ones
      if deets.positions[str][fret].waspressed = deets.positions[str][fret].expectedpressed then
        break;
      d := gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDiv;
      inc(d);
      gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDiv := d;
      gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.LocalCDiv := d;
      inc(adjust_h);
      if adjust_h > 80 then
        break;

      gps.gcurrent.UpdateState;
    end;
  end else begin
    adjust_h := 9999999;
  end;



  adjust_dir := 0;
  while not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) do begin
    //check the specific position and break, because there could be multiple bad ones
    if deets.positions[str][fret].waspressed = deets.positions[str][fret].expectedpressed then
      break;
    d := gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDirect;
    inc(d);
    gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.CDirect := d;
    gps.gcurrent.strings[str].frets[fret].sensors[mx_t].reading.LocalCDir := d;
    inc(adjust_dir);
    if adjust_dir > 80 then
      break;

    gps.gcurrent.UpdateState;
  end;

  if abs(adjust_h) < abs(adjust_dir) then begin
    if adjust_h < 80 then begin
      Self.cal.Guitars[gps.mac].Recommendations[str][sen].Diverse := greaterof(adjust_h,Self.cal.Guitars[gps.mac].Recommendations[str][sen].Diverse);
    end;
  end else begin
    if adjust_dir < 80 then begin
      Self.cal.Guitars[gps.mac].Recommendations[str][sen].Direct := greaterof(adjust_dir,Self.cal.Guitars[gps.mac].Recommendations[str][sen].Direct);
    end;
  end;

end;

procedure TAnalAutoTweaker_PerGuitar.Solveproblems(gps: TGuitarProblemSolver);
var
  deets: TGuitarFailureAnal;
  t,u: ni;
begin
  gps.gCurrent.LogicType := LogicMode;
  if not guitarResultIsExpected(gps.gCurrent, gps.gCurrentExp, deets) then begin
    for t:= 0 to 5 do begin
      for u := 0 to 4 do begin
        if deets.positions[t][u].care then begin
          if (gps.testingid=28728) and (t = 1) and (u = 2) then
            debug.consolelog('here');

          if deets.positions[t][u].expectedpressed then begin
            SolveFalseNegative(gps, t,u, @deets.positions[t][u]);
          end else begin
            SolveFalsePositive(gps, t,u, @deets.positions[t][u]);
          end;
        end;
      end;
    end;
  end;

end;

{ TCalibrationDictionary }

function TCalibrationDictionary.Add(mac: string): PGuitarCalibration;
var
  obj: TCalObj;
begin
  obj := TCalObj.create;
  FList.addobject(mac, obj);
  result := @obj.obj;
end;

procedure TCalibrationDictionary.Clear;
var
  obj: tCalObj;
begin
  while fList.count > 0 do begin
    obj := TCalObj(FList.Objects[FList.count-1]);
    obj.free;
    FList.delete(FList.count-1);
  end;
end;

constructor TCalibrationDictionary.Create;
begin
  inherited;
  fList := tStringlist.create;
end;

destructor TCalibrationDictionary.Destroy;
begin
  Clear;
  FList.free;

  inherited;
end;

function TCalibrationDictionary.GetCount: ni;
begin
  result := FList.count;
end;

function TCalibrationDictionary.GetGuitarByIndex(idx: ni): PGuitarCalibration;
begin
  result := @TCalObj(FList.objects[idx]).obj;
end;

function TCalibrationDictionary.GetguitarByMac(
  sMac: string): PGuitarCalibration;
var
  idx: ni;
begin
  idx := IndexOfMac(sMac);

  if idx < 0 then
    result := Add(sMac)
  else
    result := GetGuitarByIndex(idx);

end;

function TCalibrationDictionary.IndexOfMac(sMac: string): ni;
begin
  result := FList.IndexOf(sMac);
end;

{ TAnalHoodTweaker }

procedure TAnalHoodTweaker.DoAssimilateRecommendations(
  gps: TGuitarProblemSolver);
var
  t,u: ni;
begin
  inherited;
  for t:= 0 to 5 do begin
    for u := 0 to 19 do begin
      if LinearOffset then begin
        gps.gCurrent.strings[t].flatsensors[u].reading.CDirect :=
            gps.gCurrent.strings[t].flatsensors[u].reading.CDirect + round(ratio * cal.recommendations[t][u].Direct);
        gps.gCurrent.strings[t].flatsensors[u].reading.LocalCDir :=
            gps.gCurrent.strings[t].flatsensors[u].reading.CDirect + round(ratio * cal.recommendations[t][u].Direct);

        gps.gCurrent.strings[t].flatsensors[u].reading.CDiv :=
            gps.gCurrent.strings[t].flatsensors[u].reading.CDiv + round(ratio * cal.recommendations[t][u].Diverse);
        gps.gCurrent.strings[t].flatsensors[u].reading.LocalCDiv :=
            gps.gCurrent.strings[t].flatsensors[u].reading.CDiv + round(ratio * cal.recommendations[t][u].Diverse);
      end else begin
        gps.gCurrent.strings[t].flatsensors[u].reading.CDirect :=
            gps.gCurrent.strings[t].flatsensors[u].reading.CDirect * round(cal.recommendations[t][u].Direct);
        gps.gCurrent.strings[t].flatsensors[u].reading.LocalCDir :=
            gps.gCurrent.strings[t].flatsensors[u].reading.CDirect * round(cal.recommendations[t][u].Direct);

        gps.gCurrent.strings[t].flatsensors[u].reading.CDiv :=
            gps.gCurrent.strings[t].flatsensors[u].reading.CDiv * round(cal.recommendations[t][u].Diverse);
        gps.gCurrent.strings[t].flatsensors[u].reading.LocalCDiv :=
            gps.gCurrent.strings[t].flatsensors[u].reading.CDiv * round(cal.recommendations[t][u].Diverse);
      end;


    end;
  end;


end;

procedure TAnalHoodTweaker.Init;
begin
  inherited;
  FRatio := 1;
end;

function TAnalHoodTweaker.NeedsAnal: boolean;
begin
  result := false;
end;

procedure TAnalHoodTweaker.OnNewGuitar_AssimilateRecommendations(
  gps: TGuitarProblemSolver; sDevice: string);
var
  f: single;
  t,u: ni;
  poa: single;
  dbc: TAbstractDBCursor;
begin
  f := gps.sdb.FunctionQuery('select avg(rawledon-(rawledoff+reflection)) as a from sensors s join testing tt on (s.testingid=tt.testingid) join test t on (t.testid=tt.testid) where tt.testid=1060',0.0);
  dbc := gps.sdb.ReadQueryDBC('select avg(rawledon-(rawledoff+reflection)) as a from sensors s join testing tt on (s.testingid=tt.testingid) join test t on (t.testid=tt.testid) where tt.testid=1060 and device="'+gps.mac+'" group by stringid,sensorid order by stringid, sensorid');
  try
  cal.init;
  dbc.First;
  if dbc.RecordCount = 20 * 6 then begin
    for t := 0 to 5 do begin
      for u := 0 to 19 do begin

        if LinearOffset then begin
          poa := dbc.Fields['a'] - f;
          cal.Recommendations[t][u].Direct := poa;
          cal.Recommendations[t][u].Diverse := poa;
        end else begin
          poa := dbc.Fields['a'] / f;
          cal.Recommendations[t][u].Direct := poa;
          cal.Recommendations[t][u].Diverse := poa;
        end;

        dbc.next;
      end;
    end;
  end;
  finally
    dbc.free;
  end;

end;

initialization
  gdp := TZ2GuitarDataProvider.create;
  accumulated_recommendations.init;
finalization
  gdp.free;
  gdp := nil;



end.
