unit UnitTest;
{$INCLUDE DELPHIDEfs.inc}
interface
uses
  typex, classfactory, betterobject, sharedobject, sysutils, Debug, classes, applicationparams, systemx, stringx, orderlyinit;


type
  EIMeantToDoThat = class(Exception);
  TUnitTest = class;//forward

  TOnTestComplete = procedure (test: TUnitTest) of object;

  TUnitTestMessage = procedure(sMessage: string) of object;


  TUnitTestClass = class of TUnitTest;


  TUnitTestThread = class(TThread)
  private
    FLogHook: TUnitTestMessage;
    FOnTestComplete: TOnTestComplete;
    FRunSingle: TUnitTestClass;
  public
    constructor Create; reintroduce; virtual;

    destructor Destroy;override;
    property Loghook: TUnitTestMessage read FLogHook write FLogHook;
    property GiveCompletedTestsTo: TOnTestComplete read FOnTestComplete write FOnTestComplete;
    property RunSingle: TUnitTestClass read FRunSingle write FRunSingle;
    procedure Execute;override;
  end;

  TUnitTest = class(TSharedObject)
  private
    FResult: string;
    FVariation: ni;
    FVariationName: string;
    FDebugLog: TStringlist;
    Fskipto: nativeint;
    function GetResult: string;
    function GetExpectedResult: string;

    function GetSuccess: boolean;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Init;override;
    property Variation: ni read FVariation write FVariation;
    property VariationName: string read FVariationName write FVariationName;
    procedure Execute;
    procedure DoExecute;virtual;
    property UTResult: string read GetResult write FResult;
    property ExpectedResult: string read GetExpectedResult;
    property Success: boolean read GetSuccess;
    function AppParamsKey: string;
    procedure AcceptResult;
    procedure InvalidateResult;
    procedure UTLog(s: string);
    property SkipTo: nativeint read Fskipto write FSkipTo;
  end;

  TUnitTestFactory = class(TClassFactory<TUnitTest>)
  public
  end;

  TUT_UnitTestSelfTest = class(TUnitTest)
  public
    procedure DoExecute;override;
  end;

  TUT_ApplicationParams = class(TUnitTest)
  public
    procedure DoExecute;override;
  end;





  TUnitTestEngine = class(TBetterObject)
  private
    FOnMessage: TUnitTestMessage;
    FOnTestComplete: TOnTestComplete;
  public
    procedure RunAll;
    procedure RunSingle(c: TClass; startvar: ni = -1; endvar: ni = -1);
    property OnMessage: TUnitTestMessage read FOnMessage write FOnMessage;
    property GiveCompletedTestsTo: TOnTestComplete read FOnTestComplete write FOnTestComplete;
  end;

var
  GUTF: TUnitTestFactory;

function UTF: TUnitTestFactory;


implementation

uses
  tickcount;

{ TUnitTest }

procedure TUnitTest.AcceptResult;
var
  ap: TAppParams;
begin
  ap := NeedAppParams;
  try
    ap.Items[self.AppParamsKey].Value := 'H'+stringtohex(UTresult);
  finally
    NoNeedAppParams(ap);
  end;
end;

function TUnitTest.AppParamsKey: string;
begin
  result := ClassName+'#'+inttostr(Variation);
end;

constructor TUnitTest.Create;
begin
  inherited;
  FDebugLog := TStringlist.create;
end;

destructor TUnitTest.Destroy;
begin
  FDebugLog.Free;
  inherited;
end;

procedure TUnitTest.DoExecute;
begin
  inherited;
end;

procedure TUnitTest.Execute;
begin
  try
    if Variation < SkipTo then
      UTresult := 'variation #'+inttostr(variation)+' was skipped.'
    else begin
      VariationName := 'var '+inttostr(Variation);
      UTresult := 'no execution';
      Debug.Log(self, 'Execute Unit Test '+VariationName);
      DoExecute;
    end;
  except
    on E: Exception do begin
      FResult := 'Raised: '+E.ClassName+' with message '+E.Message;
    end;
  end;
end;

function TUnitTest.GetExpectedResult: string;
var
  ap : TAppParams;
  s: string;
  bIsHex: boolean;
begin
  ap := NeedAppParams;
  try
    result := ap.GetItemEx(Self.AppParamsKey, '');
    s := result;
    bIsHex := false;
    if length(result) > 0 then begin
      bIsHex := result[strz] = 'H';
    end;
    try
      if bIsHex then
        result := HexToString(zcopy(result,1,length(result)-1));

    except
      result := s;
    end;
  finally
    NoNeedAppParams(ap);
  end;

end;

function TUnitTest.GetResult: string;
begin
  result := FResult;
end;


function TUnitTest.GetSuccess: boolean;
var
  s, s1,s2: string;
begin
  //ignore part of result after }}
  SplitString(self.UTResult, '}}', s1, s2);
  SplitString(self.ExpectedResult, '}}', s2, s);

  result := s1 = s2;
end;

procedure TUnitTest.Init;
begin
  inherited;
end;


procedure TUnitTest.InvalidateResult;
var
  ap: TAppParams;
begin
  ap := NeedAppParams;
  try
    ap.Items[self.AppParamsKey].Value := '';
  finally
    NoNeedAppParams(ap);
  end;
end;

procedure TUnitTest.UTLog(s: string);
begin
  Lock;
  try
    FDebugLog.Add(inttostr(GEtTicker)+': '+s);
    Debug.log(self, s);
  finally
    Unlock;
  end;
end;

function UTF: TUnitTestFactory;
begin
  if GUTF = nil then
    GUTF := TUnitTestFactory.create;

  result := GUTF;
end;

{ TUT_UnitTestSelfTest }

procedure TUT_UnitTestSelfTest.DoExecute;
begin
  inherited;
  case variation of
    1:  Fresult := 'Hello World: Unit Test Framework Online';
    2:  Fresult := 'This is Variation 2';
    3:  raise EIMeantToDoThat.create('Variation 3 intentionally throws an exception for testing purposes.');
  end;

end;


{ TUnitTestEngine }


procedure TUnitTestEngine.RunAll;
var
  t,u: ni;
  ut: TUnitTest;
begin
  for t:= 0 to UTF.count-1 do begin
    RunSingle(UTF.classes[t]);
  end;

end;

procedure TUnitTestEngine.RunSingle(c: TClass; startvar: ni = -1; endvar: ni = -1);
var
  ut: TUnitTest;
  u: nativeint;
begin
  try
    u := 1;
    if startvar >=0 then
      u := startvar;
    repeat
      ut := TUnitTest(c.Create);
      try
        if (endvar >=0) and (u>endvar) then
          break;
        ut.Variation := u;
        ut.Execute;

        if (ut.UTResult = 'no execution') then begin
          if ((u mod 10)=0) then
            break;
        end else begin
          if assigned(FOnMessage) then
            FOnMessage(ut.ClassName+'#'+inttostr(u)+' '+ut.VariationName+' = '+ut.UTresult);
          if Assigned(self.FOnTestComplete) then begin
            FOnTestComplete(ut);
            ut := nil;//set to nil because we don't want this one to free in finally
          end;
        end;
        inc(u);

      FINALLY
        if assigned(ut) then begin
          ut.free;
          ut := nil;
        end;


      end;
    until false;
  finally
//    ut.Free;
  end;

end;

{ TUnitTestThread }

constructor TUnitTestThread.Create;
begin
  inherited Create(true);
  self.FreeOnTerminate := false;
end;

destructor TUnitTestThread.Destroy;
begin

  inherited;
end;

procedure TUnitTestThread.Execute;
var
  ute: TUnitTestEngine;
begin
  inherited;

  ute := TUnitTestEngine.Create;
  try
    ute.OnMessage := Self.Loghook;
    ute.GiveCompletedTestsTo := self.FOnTestComplete;
    sleep(1000);
    if RunSingle <> nil then
      ute.RunSingle(RunSingle)
    else
      ute.RunAll;

  finally
    ute.DetachAndFree;
    ute := nil
  end;


end;

{ TUT_ApplicationParams }

procedure TUT_ApplicationParams.DoExecute;
var
  ap: TAppParams;
begin
  inherited;
  ap := NeedAppParams;
  try
    case variation of
      1:  begin
        VariationName := 'Get undefined default key with default value 1234';
        UTresult := ap.GetItemEx('undefined_key', '1234');
      end;
      2:  begin
        VariationName := 'Get undefined default key with default value (int) 1234';
        UTresult := inttostr(ap.GetItemEx('undefined_key', 1234));
      end;
      3:  begin
        VariationName := 'Get undefined default key with default value true';
        UTresult := booltostrex(ap.GetItemEx('undefined_key', true));
      end;
      4:  begin
        VariationName := 'Get undefined default key with default value 1234.5';
        UTresult := floattostr(ap.GetItemEx('undefined_key', 1234.5));
      end;
      5:  begin
        VariationName := 'Set value of test_key to ''stuff'' ';
        ap.Items['test_key'].Value := 'stuff';
        UTresult := ap.GetItemEx('test_key', 'stuff');
      end;
      6:  begin
        VariationName := 'Test key should still = ''stuff'' ';
        //ap.Items['test_key'].Value := 'stuff';
        UTresult := ap.GetItemEx('test_key', '');
      end;
      7:  begin
        VariationName := 'Set value of test_int to 1234';
        ap.Items['test_int'].AsInteger := 1234;
        UTresult := inttostr(ap.GetItemEx('test_int', 0));
      end;
      8:  begin
        VariationName := 'Test_int should still = 1234';
        UTresult := inttostr(ap.GetItemEx('test_int', 0));
      end;
      9:  begin
        VariationName := 'Set value of test_bool to true';
        ap.Items['test_bool'].Value := 'true';
        UTresult := booltostr(ap.GetItemEx('test_bool', false));
      end;
      10:  begin
        VariationName := 'TestBool should still = true';
        utresult := booltostr(ap.GetItemEx('test_bool', false));
      end;
    end;
  finally
    NoNeedAppParams(ap);
//    ap.free;
  end;

end;

procedure oinit;
begin
  UTF.RegisterClass(TUT_UnitTestSelfTest);
  UTF.RegisterClass(TUT_ApplicationParams);

end;

procedure ofinal;
begin
  GUTF.free;
end;


initialization

init.RegisterProcs('unittest', oinit, ofinal, 'betterobject');

finalization


end.
