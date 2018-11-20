unit Weka;
{$DEFINE USE_SEPARATE_BASELINE}
{$INLINE AUTO}
{x$DEFINE NOCV}
{x$DEFINE NOWEKA}
{x$DEFINE ADTREE}
{$DEFINE WRITECSV_IN_PLACE}
{$DEFINE WTF}
{x$DEFINE SINGLE_THREAD_EVAL}
{x$IFDEF ENABLE_WEKA}
interface

uses
  commandicons,templateclasses, svnclient,dir, dirfile,misccommands,console_tools, sysutils, classes, commandprocessor, stringutilities, simpletypes, miscroutines,exe, windows, multibuffermemoryfilestream, generics.collections.fixed, wekaicons;


function HasFinger(sString, sFret: string; iString, iFret: nativeint): boolean;
function Z2MassagedCSVFile(sDir, sPrefix: string; iFretNumber, iStringNumber: nativeint): string;


const
  SPLIT_SIZE = 1000;
  BATCH_SIZE = 1;
  HIGHEST_FRET = 4;
  HIGHEST_STRING = 5;
  fret_sensor_counts: array [0..4] of nativeint = (4,5,4,4,3);
type
  TFretSensorData = record
    d,a,r,u,l,ru,rl: nativeint;
    avg_unwanted: nativeint;
    function CertainLight_Standard: nativeint;
    function CertainLight2(rULRatio: nativefloat): nativeint;
    function CertainLight3: nativeint;
    function CertainLight: nativeint;
    function RAA: nativeint;
    procedure FillFromStringList(sl: TStringlist; iStartat: nativeint);
  end;
  TFretSensorDataArray = record
  strict private
    FAvgUnwanted: nativeint;
    procedure SetAvgUnwanted(const Value: nativeint);
  private
    procedure ComputeAVGUnwanted;
  public
    a: array[0..5] of TFretSensorData;
    Count: nativeint;
    function Min: nativeint;
    function Max: nativeint;
    procedure FillFromStringList(sl: tSTringlist; iStartAT: integer);
    property avg_unwanted: nativeint read FAvgUnwanted write SetAvgUnwanted;
  end;


  Tcmd_MasterSplitCSV = class;
  Tcmd_MasterProcess = class(TCommand)
  private
    FAppend: boolean;
    procedure z2_go;
    procedure go_z2_weka(c: Tcmd_MAsterSplitCSV);
    procedure SetAppend(const Value: boolean);
  public
    FileList: TStringList;
    constructor Create;override;
    destructor Destroy;override;
    procedure InitExpense;override;
    procedure DoExecute;override;
    property Append: boolean read FAppend write SetAppend;

  end;





  Tcmd_CSVWrite = class;//forward

  TOutputStats = record
    MaxFalseOverAmbient: nativeint;
    MinTrueOverAmbient: nativeint;
    Hits: nativeint;
    procedure ApplyValues(d,a,r,u,l,ru,rl: nativeint);
    procedure Init;
  end;


  TCSVOutputRecord = record
    BaseLine: TStringlist;
//    cp: TCommandProcessor;
    active_commands: TSharedList<Tcmd_CSVWrite>;
    slOutput: TStringlist;
    FileName: string;
    Header: string;
    FirstField: nativeint;
    LAstField: nativeint;
    StringNumber: nativeint;
    FretNumber: nativeint;
    FirstFieldName: string;
    AboveFieldName: string;
    BelowFieldName: string;

    AboveFirstField: nativeint;
    BelowFirstField: nativeint;
    FieldCount: nativeint;
    changed: boolean;
    sect: _RTL_CRITICAL_SECTION;
    stats: TOutputStats;
    procedure ComputeFirstLastField;
    procedure CleanupAndSave;
    procedure Init;
    procedure Lock;
    procedure Unlock;
    function SensorCount: nativeint;
  end;
  PCSVOutputRecord = ^TCSVOutputRecord;


  Tcmd_RollupResults = class(TCommand)
  private
    FOutputRecord: TCSVOutputRecord;
  public
    procedure DoExecute;override;
    property OutputRecord: TCSVOutputRecord read FOutputRecord write FOutputRecord;
  end;

  TEvaluator = function: boolean of object;
  Tcmd_EvaluateEquation = class(TCommand)
  private
    ss: TFretSensorDataArray;
    ssA: TFretSensorDataArray;
    ssB: TFretSensorDataArray;
    bExpected: boolean;
    FFretNumber: nativeint;
    FInputFile: string;
    FStringNumber: nativeint;
    input: TMBMemoryStringStream;
    slMiss: TStringlist;
    FEvaluator: TEvaluator;
    FTargetAccuracy: nativefloat;
    function VarianceSum: nativeint;
    function VarianceMax: nativeint;
    procedure Go;
    procedure DoExecuteOld;
    function BSFunc_SimpleDirect(test: int64): nativeint;
    function BSFunc_SimpleDirect_VarianceStage(test: int64): nativeint;
    procedure Setup;
    procedure DetermineQualityThresholds;
    function GetQualityThreshold(bAttr: boolean; rQuality: nativefloat): nativeint;
    procedure Finish;


  public
    TrueHits: nativeint;
    TrueMisses: nativeint;
    FalseHits: nativeint;
    FalseMisses: nativeint;
    DirectThreshold: nativeint;
    VArianceThreshold: nativeint;
    TrueQuality: nativeint;
    FalseQuality: nativeint;
    function TrueAccuracy: nativefloat;
    function FalseAccuracy: nativefloat;
    property InputFile: string read FInputFile write FInputFile;
    property STringNumber: nativeint read FStringNumber write FStringNumber;
    property FretNumber: nativeint read FFretNumber write FFretNumber;
    procedure DoExecute;override;//<<-----------------------------
    procedure InitExpense;override;
    function Compute: boolean;
    function Evaluate: boolean;
    function Evaluate_Simple: boolean;
    function Evaluate_Simple_PlusVariance: boolean;
    function Evaluate_Suppressed: boolean;
    function Evaluate_Sum: boolean;
    function SensorCount: nativeint;
    property Evaluator: TEvaluator read FEvaluator write FEvaluator;
    property TargetAccuracy: nativefloat read FTargetAccuracy write FTargetAccuracy;
    procedure REset;
  end;

  Tcmd_MasterEvaluateEquation = class(TCommand)
  private
    FOutput: string;
  public
    procedure Init;override;
    procedure InitExpense;override;
    procedure DoExecute;override;
    property Output: string read FOutput;
  end;


  Tcmd_CSVPArse = class(TCommand)
  private
    FFileName: string;
    FMaster: Tcmd_MasterSplitCSV;
    FFirstLine: nativeint;
    FLastLine: nativeint;
  public
    procedure Init;override;
    procedure DoExecute;override;
    procedure Parse;
    property FileName: string read FFileName write FFilename;
    property Master: Tcmd_MasterSplitCSV read FMaster write FMaster;
    property FirstLine: nativeint read FFirstLine write FFirstLine;
    property LastLine: nativeint read FLastLine write FLastLine;
  end;




  Tcmd_MasterSplitCSV = class(TCommand)
  private
    FInputdir: string;
    FMaster: Tcmd_MasterProcess;
    function GEtAppend: boolean;

  public

    outputs: array[0..HIGHEST_FRET]of array[0..HIGHEST_STRING] of TCSVOutputRecord;
    procedure InitExpense;override;
    procedure DoExecute;override;
    procedure Old;
    procedure PreSplit;

    property InputDir: string read FInputdir write Finputdir;
    procedure PArse;
    property Master: Tcmd_MasterProcess read FMaster write FMaster;
    property Append: boolean read GEtAppend;
    procedure RollUpStats;
  end;



  Tcmd_CSVWrite = class(TCommand)
  private
    FWriteTo: PCSVOutputRecord;
    FReadFrom: TStringlist;
    procedure SetWriteTo(const Value: PCSVOutputRecord);
    procedure SetReadFrom(const Value: TStringlist);
  public
    destructor Destroy;override;
    property WriteTo: PCSVOutputRecord read FWriteTo write SetWriteTo;
    property ReadFrom: TStringlist read FReadFrom write SetReadFrom;
    procedure DoExecute;override;
    procedure ExecuteInPlace;
    procedure InitExpense;override;
    procedure ReserveCSVResources;
  end;





  Tcmd_Weka = class(Tcmd_RunEXE)
  private
    FOutputFile: string;
    FInputFile: string;
  public
    procedure InitExpense;override;
    procedure BuildCommandLine;override;
    property InputFile: string read FInputFile write FInputFile;
    property OutputFile: string read FOutputFile write FOutputFile;
  end;


  Tcmd_MassageCSV_Z2 = class(TCommand)
  private
    FFileName: string;
    FFretNumber: nativeint;
    FStringNumber: nativeint;
    procedure SplitCSVFile(sFile: string; iStringNumber,
      iFretNumber: nativeint);
    procedure Massage_C(sFile: string; iStringNumber, iFretNumber: nativeint);
    //^Uses RAwOn-(RawOff+Refelction+(UpperIndirect-UpperReflection)+(LowerIndered-LowerReflection))

    procedure Massage_W(sFile: string; iStringNumber, iFretNumber: nativeint);
    //^ Uses Reading above ambient
  public
    procedure Init;override;
    procedure InitExpense;override;
    procedure DoExecute;override;
    property FileName: string read FFileName write fFileName;
    function MassagedFileName(sAlgorithm: string): string;
    property StringNumber: nativeint read FStringNumber write FStringNumber;
    property FretNumber: nativeint read FFretNumber write FFretNumber;

  end;


  Tcmd_CombineMassagedResults = class(TCommand)
  private
    FFileName: string;
  public
    property FileName: string read FFileName write fFileName;
    procedure DoExecute;override;
  end;




function Z2InputFile: string;
function CSVINputPath: string;
var
  max_injection: nativeint;
  avg_injection: nativeint;



implementation

//------------------------------------------------------------------------------
function Z2CodeGenInputPath: string;
begin
  result := '..\..\..\zivix_guitars\embedded_systems\Z2-32bit\Trunk\Z2\src\';
end;
function Z2InputFile: string;
begin
  result := Z2codegeninputPath+'decision_tree.c';
end;

//------------------------------------------------------------------------------
function GetZ2BaseLIneFile(sFingerDir: string): string;
var
  s1,s2,s3: string;
begin
  s1 := adjustpath(sFIngerDir);
  result := adjustpath(adjustpath(sFingerDir))+'baseline.csv';

end;

//------------------------------------------------------------------------------
function Z2DecisionPath: string;
begin
  result := DLLPath;
end;
//------------------------------------------------------------------------------
function DecisionFile(iFret, iString: integer): string;
begin
  result := z2decisionpath+'f'+inttostr(iFret)+'s'+inttostr(iString)+'.results';
end;
//------------------------------------------------------------------------------
function Z2DecisionFile(iFret, iString: integer): string;
begin
  result := z2decisionpath+'f'+inttostr(iFret)+'s'+inttostr(iString)+'.results';
end;
//------------------------------------------------------------------------------
function CSVINputPath: string;
begin
  result := '..\Z2_Debugger\';
end;
//------------------------------------------------------------------------------
function GetDecision(iFret, iString: integer): string;
var
  sLeft, sRight: string;
begin
  result := '';
  sLeft  := LoadStringFromFile(Z2DecisionFile(iFret, iString));
  if SplitString(sLEft, '------------------', sLeft, sRight) then begin
    if SplitString(sRight, 'Number of Leaves', sLeft, sRight) then begin
      result := sLEft;
    end;
  end;
end;

//------------------------------------------------------------------------------
function LoadDecisionAsComment(iFret, iString: integer): string;
var
  sl: TStringlist;
  t: integer;
begin
  result := '';
  sl := TStringlist.Create;
  try
    sl.LoadFromFile(Z2DecisionFile(iFret, iString));
    for t:= 0 to sl.Count-1 do begin
      sl[t] := '//-file:'+extractfilenamepart(Z2DecisionFile(iFret,iString))+':'+sl[t];
    end;

    result := sl.Text;
  finally
    sl.Free;
  end;


end;

//------------------------------------------------------------------------------
function GetTreeLevel(sLine: string): ni;
var
  t: ni;
begin
  result := 0;
  for t:= 1 to length(sLine) do begin
    if sLIne[t] = '|' then inc(result);
  end;
end;
//------------------------------------------------------------------------------
function SplitStringAtAny(sIn: string; slSplit: TStringList; var sLeft, sRight, sFound: string): boolean;
var
  t: integer;
begin
  result := false;
  sLeft := sIn;
  sRight := '';
  sFound := '';
  for t:= 0 to slSplit.Count-1 do begin
    result := SplitString(sIn, slSplit[t], sleft, sRight);
    sFound := slSplit[t];
    if result then exit;
  end;
end;
//------------------------------------------------------------------------------
function SplitHeading(sIn: string; var sLeft, sRight, sHeading: string): boolean;
var
  sl: TStringlist;
  t: nativeint;
begin
  sl := TStringList.Create;
  try
    for t := 0 to 31 do begin
      sl.Add('o'+inttostr(t)+' ');
      sl.Add('a'+inttostr(t)+' ');
      sl.Add('u'+inttostr(t)+' ');
      sl.Add('r'+inttostr(t)+' ');
      sl.Add('v'+inttostr(t)+' ');
      sl.Add('c'+inttostr(t)+' ');
      sl.Add('w'+inttostr(t)+' ');
      sl.Add('z'+inttostr(t)+' ');
      sl.Add('s'+inttostr(t)+' ');
      sl.Add('d'+inttostr(t)+' ');
      sl.Add('g'+inttostr(t)+' ');
    end;

    result := SplitStringAtAny(sIn, sl, sLeft, sRight, sHeading);

  finally
    sl.Free;
  end;

end;

function HeadingToCode(sHeading: string; iFret: nativeint): string;
var
  sFirstChar: string;
begin
  sFirstChar := copy(sHeading,1,1);

  result := 'IXN_'+uppercase(sFirstChar)+'('+inttostr(iFret)+','+trim(copy(sHeading, 2,99999))+')';


end;

function ParseDecisionLine(iFret, iString: ni; sLine: string; iLevel, iOldLevel: ni; var bLastWasLeaf: boolean): string;
var
  sLEft, sright, sIXN, sSign, sBoundary, sResult: string;
  bIsLeaf: boolean;
  t: ni;
begin
  iLevel := GEttreeLevel(sLine);
  ////Below is typical of the line we're parsing
  //|   |   |   |   |   intersection31 <= -15: 0 (7.0)
  //also this
  //|   |   |   |   intersection23 <= -12
  if SplitHeading(sLIne, sLeft, sRight, sIXN) then begin
//    if SplitString(sright, ' ', sIXN, sRight) then begin
      if SplitString(sRight, ' ', sSign, sright) then begin
        if SplitString(sRight, ': ', sBoundary, sRight) then begin
          SplitString(sRight, ' ', sResult, sRight);
          sResult := lowercase(sREsult);
        end else begin
          sResult := '';
        end;
      end;
//    end;
  end else begin
    result := #9'//end of stuff';
    exit;
  end;

  //now that we have the individual values, make a decision about what to do with them
  bIsLeaf := sResult <> '';
  if sSign = '=' then
    sSign := '==';
  result :=stringUtilities.PadString('', #9, 1+iLevel);
  if iLevel < iOldLevel then begin
    if iOldLevel - iLevel = 1 then
      result := result + '} else '
    else begin
      for t:= iLevel+1 to iOldLevel-booltoint(bLastWasLeaf) do begin
        result := PadString('', #9, 1+t)+'}'#13#10+result;
      end;
      result := result + '} else '

    end;
  end
  else if iLevel > iOldLevel then
    result := PadString('', #9, 1+iLevel-1)+'{'#13#10+result;
  result := result + 'if ('+HeadingToCode(sIXN, iFret)+sSign+sBoundary+')';
  if bIsLeaf then begin
    result := result +  ' return '+sResult+';';
  end else begin
    result := result;
  end;

  bLastWasLeaf := bIsLeaf;



end;
//------------------------------------------------------------------------------
function ParseDecision(sDecision: string; iFret, iString: integer ): string;
var
  sl, slResult: TStringlist;
  t: nativeint;
  sLine: string;
  iOldLevel, iLevel, uu: ni;
  bLastWasLeaf: boolean;
begin
  iOldLevel := 0;
  iLevel := 0;
  sl := TStringlist.Create;
  try
    slResult := TStringlist.Create;
    try
      sl.Text := sDecision;
      for t:= 0 to sl.Count-1 do begin
        sLine := sl[t];
        iOldLevel := iLevel;
        iLevel := GetTreeLevel(sLine);
        if trim(sLine) = '' then begin
          if iLevel < iOldLevel then begin
            for uu := iOldLevel-(booltoint(bLAstWasleaf)) downto iLevel do begin
              slResult.Add(PadString('',#9, 1+uu)+'}//closing');
            end;
          end else
            continue;
        end;

        slResult.Add(ParseDecisionLine(iFret, iString, sLine, iLevel, iOldLevel, bLastWasLeaf));
      end;
    finally
      result := slResult.text;
      slResult.Free;
    end;
  finally
    sl.Free;
  end;



end;
//------------------------------------------------------------------------------
function GetBeginMarker(iFret, iString: nativeint): string;
begin
  //START_DT,4,2

  result := '//START_DT,'+inttostr(iFret)+','+inttostr(iString);

end;
//------------------------------------------------------------------------------
function GetEndMarker(iFret, iString: nativeint): string;
begin
  result := '//END_DT';
end;
//------------------------------------------------------------------------------
function GetStringListRange(sl: Tstringlist; iFret, iString: nativeint; out iStartLine, iEndLine: nativeint): boolean;
var
  t,u: integer;
  sLine: string;
begin
  iStartLine := 0;
  iEndLine := 0;
  result := false;

  for t:= 0 to sl.Count-1 do begin
    if uppercase(trim(sl[t]))=GetBeginMarker(iFret, iString) then begin
      iStartLine := t+1;
      for u := t+1 to sl.count-1 do begin
        if uppercase(trim(sl[u]))=GetEndMarker(iFret, iString) then begin
          iEndLine := u-1;
          result := true;
          break;
        end
      end;
      break;
    end;
  end;

end;
//------------------------------------------------------------------------------
function GetInjection(iFret, iString: nativeint): string;
var
  sl: TStringlist;
  inj: string;
begin
  result := '';
  sl := TStringlist.Create;
  try
    sl.Add(LoadDecisionAsComment(iFret,iString));
    sl.Add('bool_t decision_IsPressed_'+inttostr(iFret)+'_'+inttostr(iString)+'(PFret_t self)');
    sl.Add('{');
    inj := ParseDecision(GetDecision(iFret,iString), iFret, iString);
    sl.Add(inj);
    sl.Add(#9'//final closing');
    sl.Add(#9'return false;//final result if tree is incomplete');
    sl.Add('}');
  finally
    result := sl.Text;


    //figure out how many lines the injection is
    sl.text := inj;
    if sl.count > max_injection then
      max_injection := sl.Count;
    avg_injection := avg_injection + sl.Count;

    sl.Free;
  end;

end;

//------------------------------------------------------------------------------
procedure Z2InjectDecision(sl: TStringlist; iFret, iString: nativeint);
var
  iStart, iEnd: nativeint;
  sInjection: string;
  t: integer;
begin
  if GetStringListRange(sl, iFret, iString, iStart, iEnd) then begin
    sInjection := GetInjection(iFret, iString);
    for t:=iStart to iEnd do begin
      sl[t] := '//OLD'+sl[t];
    end;
    sl.Insert(iEnd+1, sInjection);
    sl.Text := sl.Text;
    for t:= sl.Count-1 downto 0 do begin
      if copy(sl[t],1,7) = '//OLD//' then begin
        sl.delete(t);
      end;
    end;
  end;

end;




procedure Z2_GenCode();
var
  sl: TStringlist;
  t, u: nativeint;
begin
  sl := TStringlist.create;
  try
    sl.LoadFromFile(z2inputfile);

    max_injection := 0;
    avg_injection := 0;
    for t:= 0 to 4 do begin
      for u := 0 to 5 do begin
        Z2InjectDecision(sl, t,u);
      end;
    end;
    avg_injection := avg_injection div (5*6);


    sl.SaveToFile(z2inputfile);
  finally
    sl.free;
  end;
end;


{ Tcmd_Weka }

procedure Tcmd_Weka.BuildCommandLine;
var
  sCoreCommand: string;
  sCommand: string;
begin
  inherited;
  //sample command :   java -classpath ./weka.jar weka.classifiers.trees.J48 -t "e:\_\fingers0-0.csv" -c 1 >output.txt;

{$IFDEF NOWEKA}
  sCoreCommand := 'dir';
{$ELSE}
  {$IFDEF NOCV}
    {$IFDEF ADTREE}
      sCoreCommand := '"%%java%%" -classpath "%%jar%%" weka.classifiers.trees.ADTree -t "%%file%%" -c 1 -no-cv -i >%%output%%';
    {$ELSE}
      sCoreCommand := '"%%java%%" -classpath "%%jar%%" weka.classifiers.trees.J48 -t "%%file%%" -c 1 -no-cv -i >%%output%%';
    {$ENDIF}
  {$ELSE}
    {$IFDEF ADTREE}
      sCoreCommand := '"%%java%%" -classpath "%%jar%%" weka.classifiers.trees.ADTree -t "%%file%%" -c 1 -i >%%output%%';
    {$ELSE}
      sCoreCommand := '"%%java%%" -classpath "%%jar%%" weka.classifiers.trees.J48 -t "%%file%%" -c 1 -x 10 -i >%%output%%';
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  sCommand := sCoreCOmmand;
  BatchWrap := true;
  sCommand := StringReplace(sCommand, '%%java%%', 'java',[]);
  sCommand := StringReplace(sCommand, '%%jar%%',  'c:\program files\Weka-3-6\weka.jar',[]);
  sCommand :=   StringReplace(sCommand, '%%file%%', InputFile,[]);
  sCommand :=   StringReplace(sCommand, '%%output%%', OutputFile,[]);



  Prog := sCommand;
  ConsoleRedirect := false;
  




end;




procedure Tcmd_MasterProcess.z2_go();
var
  stream: TMBMemoryStringStream;
  c: Tcmd_MAsterSplitCSV;
begin
  //nclient.svn_update(InputPath);
  con.Write('Checking Index');

//  stream := TMBMemoryStringStream.Create(Z2CSVFile, fmOpenRead+fmShareDenyNone);
//  try
//    if not stream.Loadindex then begin
//      Status := ('Building Index');
//      stream.Count;
//      Status := ('Saving Index');
//      stream.SaveIndex;
//    end;
//  finally
//    stream.Free;
//  end;

  c := Tcmd_MAsterSplitCSV.Create;
  c.InputDir := CSVInputPath;
  c.Master := self;
  c.Start;

  self.WaitForAnotherCommand(c);



{$IFDEF ENABLE_WEKA}
  Status := ('Doing stuff...');
  if paramstr(1) <> '/noweka' then
    go_z2_weka(c);

  c.Free;


  Status := ('Generating Code...');
  z2_gencode();
{$ENDIF}

//  SAvesTringAsFile(CSVINputPath+'parse_time.dat', 'poop');
end;




procedure Tcmd_MasterProcess.go_z2_weka(c: Tcmd_MAsterSplitCSV);
var
  sl: TStringlist;
  t, u: nativeint;
  //c: Tcmd_Weka;
  cc: Tcmd_MassageCSV_z2;
  tmStart: cardinal;
  //a: array[0..4] of array [0..5] of Tcmd_Weka;
  aa: array[0..4] of array [0..5] of Tcmd_MassageCSV_Z2;
begin

  sl := TStringlist.create;
  try
//    sleep(10000);

    tmStart := GEtTickCount;
    //start commands
    StepCount := 30;
    for t:= 0 to HIGHEST_FRET do begin
      for u := 0 to HIGHEST_STRING do begin
        aa[t][u] := nil;
//        if c.outputs[t][u].changed then begin
          cc := Tcmd_MassageCSV_z2.create;
          cc.FileName := Z2MassagedCSVFile(CSVINputPath, '_Split_', t, u);
          cc.FretNumber := t;
          cc.StringNumber := u;
          aa[t][u] := cc;
          cc.Start;
//        end;

      end;
    end;

    //wait for commands
    StepCount := 30;
    for t:= 0 to HIGHEST_FRET do begin
      for u := 0 to HIGHEST_STRING do begin
        if aa[t][u] = nil then
          continue;

        Step := (t * 5)+u;
        cc := aa[t][u];
        self.WaitForAnotherCommand(cc, false);
        cc.WaitFor;
        //Status := (inttostr(round(((t*5)+u)/30*100))+'%');
        cc.Free;
        aa[t][u] := nil;
      end;
    end;



  finally
    sl.free;
  end;

end;


{ Tcmd_MassageCSV_Z2 }

procedure Tcmd_MassageCSV_Z2.DoExecute;
var
  sFile1, sFile2: string;
  c: Tcmd_Weka;
begin
  inherited;
//  SplitCSVFile(FileName, StringNumber, FretNumber);//<<--splits into individual-fingers
  //Z2_MassageCSVFile_Dual(FileName, StringNumber, FretNumber);
  Massage_C(Z2MassagedCSVFile(FileName, '_Split_', FretNumber, StringNumber), Stringnumber, FretNumber);
  Massage_W(Z2MassagedCSVFile(FileName, '_Split_', FretNumber, StringNumber), Stringnumber, FretNumber);


  if fileexists(pchar(MassagedFileName(''))) then
    deletefile(pchar(MassagedFileName('')));

  sFile1 := MassagedFileName('_Split_');
  sFile2 := MassagedFileName('');

//  exit;
  c := Tcmd_Weka.create;
  c.INputFile := Z2MassagedCSVFile(CSVInputPath,'_Split_', FretNumber, StringNumber);
  c.OutputFile := Z2DecisionFile(FretNumber,StringNumber);
  c.Start;
  //c.WaitFor;
  WaitForAnothercommand(c);
  c.Free;


  c := Tcmd_Weka.create;
  c.INputFile := Z2MassagedCSVFile(CSVInputPath,'_C_', FretNumber, StringNumber);
  c.OutputFile := Z2DecisionFile(FretNumber,StringNumber);
  c.Start;
  WaitForAnothercommand(c);
  c.WaitFor;
  copyfile(pchar(c.OutputFile), pchar(Z2DecisionFile(FretNumber,StringNumber)), false);
  c.Free;




end;

procedure Tcmd_MassageCSV_Z2.Init;
begin
  inherited;
  Icon := @CMD_ICON_CRUNCH;
end;

procedure Tcmd_MassageCSV_Z2.InitExpense;
begin
  inherited;
  CPuExpense := 0.9;
  MemoryExpense :=0.0;
end;

function Tcmd_MassageCSV_Z2.MassagedFileName(sAlgorithm: string): string;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


procedure Tcmd_Weka.InitExpense;
begin
  inherited;
  CPUExpense := 1;
  MemoryExpense := 0;
end;

{ Tcmd_MassageCSV }


{ Tcmd_CombineMassagedResults }

procedure Tcmd_CombineMassagedResults.DoExecute;
var
  slOut, slIn, parse: TStringlist;
  t,u,v: nativeint;
  sThisFile: string;
begin
  inherited;
  parse := TStringlist.Create;
  try

    slIn := tStringlist.Create;
    try
      slOut := TStringlist.Create;
      slOut.LoadFromFile(FileName);
      try
        for u := 0 to 5 do begin
          for t := 0 to 4 do begin
            sThisFile := '';//IntermediateCSVFile(t,u);
            if not fileexists(sThisFile) then
              continue;

            if sThisFile=FileNAme then
              continue;//<<---skip TRUE file it was laded into slOUT to begin with
            slIn.LoadFromFile(sThisFile);
            stringutilities.ParseString(slIn[t], ',',parse);
            for v := 0 to slIn.Count-1 do begin
              if lowercase(parse[0]) = 'true' then begin
                parse[0] := 'false';//<--change true to false
              end;
            end;
            slOut.Add(UnparseString(',',parse));
          end;
        end;
      finally
        slOut.Free;
      end;
    finally
      slIn.Free;
    end;
  finally
    parse.Free;
  end;

end;





procedure Tcmd_MassageCSV_Z2.SplitCSVFile(sFile: string; iStringNumber: nativeint; iFretNumber: nativeint);
var
  stream: TMBMemoryStringStream;
  header: TStringlist;
  parsed: TStringList;
  x,t: integer;
  s,sZ: string;
  c,d,a,r,u,l,ru,rl: nativeint;
  slBaseLines: TSTringlist;
  slOutput: TStringlist;
  sHeader: string;
  sFieldPRefix: string;
  sRecord: string;
  slRecord: TStringlist;
  slFingers, slFrets: TStringlist;
  iFieldIndex: nativeint;
  sen_idxs:  array[0..5] of nativeint;
  sOutputFile: string;
CONST
  OIDX=1+0;
  AIDX=1+32;
  RIDX=1+64;
  UIDX=1+(32*3);
  VIDX=1+(32*4);
begin
  header := nil;
  soutputFile := Z2MassagedCSVFile(sFile, '_Split_', iFretNumber, iStringNumber);

  slBaseLines := TStringlist.Create;
  slOutput := tStringlist.Create;
  slOutput.sorted := true;
  slFingers := tStringlist.Create;
  slFrets := tStringlist.Create;
  slRecord := TStringlist.create;
  try
    stream := TMBMemoryStringStream.Create(sFile, fmOpenRead+fmShareDenyNone);
    stream.Loadindex;
    try
      header := TStringlist.Create;
      try
        parsed := TStringlist.Create;
        try
          slbaseLines.Clear;
          s := LoadStringFromFile(GEtz2BaseLineFile(sFile));
          parseString(s, ',', slBaseLines);


          //create header
          sHeader := 'attr,';
          for t:= 0 to fret_sensor_counts[iFretNumber]-1 do begin
            sHeader := sHeader+
                        'd'+inttostr(t)+','+
                        'a'+inttostr(t)+','+
                        'r'+inttostr(t)+','+
                        'u'+inttostr(t)+','+
                        'l'+inttostr(t)+','+
                        'ru'+inttostr(t)+','+
                        'rl'+inttostr(t);

            if t< fret_sensor_counts[iFretNumber]-1 then
              sHeader := sHeader+',';
          end;


          //get the field names into the header stringlist
          parsestring(Stream.Lines[0], ',', header);

          for x := 0 to fret_sensor_counts[iFretNumber]-1 do begin
             sfieldPrefix := 'str'+inttostr(iStringNumber)+'frt'+inttostr(iFretNumber)+'sen'+inttostr(x)+'_';
             sen_idxs[x] := GetFieldIndex(header, sFieldPrefix+'d');
          end;
          //for each line
          StepCount := stream.Count;
          for t:= 1 to stream.Count-1 do begin
            if (t mod 100) = 0 then begin
              status := (extractfilename(sOutputFile)+' @'+inttostr(t));
              step := t;
            end;

            //get the record
            ParseString(stream.lines[t], ',', parsed);
            if parsed.count < 842 then begin
              continue;
            end;
            sZ := '';

            slRecord.Clear;
            if HasFinger(parsed[0], parsed[1], iStringNumber, iFretNumber) then
              slRecord.Add('true')///<--FIX!
            else
              slRecord.Add('false');///<--FIX!


            for x := 0 to fret_sensor_counts[iFretNumber]-1 do begin

              d := strtoint(parsed[sen_idxs[x]+0]);
              a := strtoint(parsed[sen_idxs[x]+1]);
              u := strtoint(parsed[sen_idxs[x]+3]);
              l := strtoint(parsed[sen_idxs[x]+4]);
    {$IFDEF USE_SEPARATE_BASELINE}
              r := greaterof(0, strtoint(slbaselines[sen_idxs[x]+0])-strtoint(slbaselines[sen_idxs[x]+1]));
              ru := greaterof(0, strtoint(slbaselines[sen_idxs[x]+3])-strtoint(slbaselines[sen_idxs[x]+1]));
              rl := greaterof(0, strtoint(slbaselines[sen_idxs[x]+4])-strtoint(slbaselines[sen_idxs[x]+1]));
    {$ELSE}
              r := strtoint(parsed[sen_idxs[x]+2]);
              ru := strtoint(parsed[sen_idxs[x]+5]);
              rl := strtoint(parsed[sen_idxs[x]+6]);
    {$ENDIF}

              //todo 1 put out true only if finger matches
              //c := d - (a+r+(u-ru)+(l-rl));

              slRecord.add(inttostr(d));
              slRecord.add(inttostr(a));
              slRecord.add(inttostr(r));
              slRecord.add(inttostr(u));
              slRecord.add(inttostr(l));
              slRecord.add(inttostr(ru));
              slRecord.add(inttostr(rl));
              //slRecord.add(inttostr(c));


            end;
            sRecord := UnParseString(',', slRecord);
            if slOutput.indexof(sRecord) < 0 then begin
              //Debug('cnt='+inttostr(slOutput.count));
              slOutput.Add(sRecord);
              //Debug('cnt='+inttostr(slOutput.count));
            end;
          end;

          sFile := soutputFile;

          slOutput.Sorted := false;

          slOutput.Insert(0, sHeader);
          slOutput.SaveToFile(sFile);

        finally
          parsed.free;
        end;
      finally
        stream.free;
        slBaseLInes.Free;
      end;
    finally
      header.Free;
    end;
  finally
    slOutput.Free;
    slFingers.free;
    slFrets.free;
    slREcord.free;
  end;

end;


procedure Tcmd_MassageCSV_Z2.Massage_C(sFile: string; iStringNumber: nativeint; iFretNumber: nativeint);
var
  stream: TMBMemoryStringStream;
  header: TStringlist;
  parsed: TStringList;
  x,t: integer;
  s,sZ: string;
  c,d,a,r,u,l,ru,rl: nativeint;
  slBaseLines: TSTringlist;
  slOutput: TStringlist;
  sHeader: string;
  sFieldPRefix: string;
  sRecord: string;
  slRecord: TStringlist;
  slFingers, slFrets: TStringlist;
  iFieldIndex: nativeint;
  sOutputFile: string;
  cc, noize, noize_iterations: nativeint;
CONST
  OIDX=1+0;
  AIDX=1+32;
  RIDX=1+64;
  UIDX=1+(32*3);
  VIDX=1+(32*4);
  NOISE_MARGIN = 6;
  NOISE_ITERATIONS = 1;
begin
  header := nil;
  soutputFile := Z2MassagedCSVFile(FileName, '_C_', iFretNumber, iStringNumber);

  slBaseLines := TStringlist.Create;
  slOutput := tStringlist.Create;
  slOutput.sorted := true;
  slFingers := tStringlist.Create;
  slFrets := tStringlist.Create;
  slRecord := TStringlist.create;
  try
    stream := TMBMemoryStringStream.Create(sFile, fmOpenRead+fmShareDenyNone);
    stream.Loadindex;
    try
      header := TStringlist.Create;
      try
        parsed := TStringlist.Create;
        try
          slbaseLines.Clear;
          s := LoadStringFromFile(GEtz2BaseLineFile(sFile));
          parseString(s, ',', slBaseLines);


          //create header
          sHeader := 'attr,';
          for t:= 0 to fret_sensor_counts[iFretNumber]-1 do begin
            sHeader := sHeader+
//                        'd'+inttostr(t)+','+
//                        'a'+inttostr(t)+','+
//                        'r'+inttostr(t)+','+
//                        'u'+inttostr(t)+','+
//                        'l'+inttostr(t)+','+
//                        'ru'+inttostr(t)+','+
//                        'rl'+inttostr(t)+','+
                        'c'+inttostr(t);
            if t< fret_sensor_counts[iFretNumber]-1 then
              sHeader := sHeader+',';
          end;


          //get the field names into the header stringlist
          parsestring(Stream.Lines[0], ',', header);

          //for each line
          StepCount := stream.Count;
          for t:= 1 to stream.Count-1 do begin
//            if (t mod 100) = 0 then begin
              status := (extractfilename(sOutputFile)+' @'+inttostr(t));
              step := t;
//            end;

            //get the record
            ParseString(stream.lines[t], ',', parsed);
            sZ := '';

            for noize := 1 to NOISE_ITERATIONS do begin
              slRecord.Clear;
              slRecord.Add(parsed[0]);

              for x := 0 to fret_sensor_counts[iFretNumber]-1 do begin

                d := strtoint(parsed[1+(x*7)+0]);
                a := strtoint(parsed[1+(x*7)+1]);
                r := strtoint(parsed[1+(x*7)+2]);
                u := strtoint(parsed[1+(x*7)+3]);
                l := strtoint(parsed[1+(x*7)+4]);
                ru := strtoint(parsed[1+(x*7)+5]);
                rl := strtoint(parsed[1+(x*7)+6]);


                //todo 1 put out true only if finger matches
                c := d - (a+r+(u-ru)+(l-rl));
                //c := d - a;
                //c := d - (a+r{+(u-ru)+(l-rl)});

  //              slRecord.add(inttostr(d));
  //              slRecord.add(inttostr(a));                         1000
  //              slRecord.add(inttostr(r));
  //              slRecord.add(inttostr(u));
  //              slRecord.add(inttostr(l));
  //              slRecord.add(inttostr(ru));
  //              slRecord.add(inttostr(rl));
                cc := c + random(NOISE_MARGIN*2)-NOISE_MARGIN;
                slRecord.add(inttostr(cc));



              end;
              sRecord := UnParseString(',', slRecord);
              if slOutput.indexof(sRecord) < 0 then begin
                //Debug('cnt='+inttostr(slOutput.count));
                slOutput.Add(sRecord);
                //Debug('cnt='+inttostr(slOutput.count));
              end;
            end;
          end;

          sFile := soutputFile;

          slOutput.Sorted := false;
          slOutput.Insert(0, sHeader);
          slOutput.SaveToFile(sFile);

        finally
          parsed.free;
        end;
      finally
        stream.free;
        slBaseLInes.Free;
      end;
    finally
      header.Free;
    end;
  finally
    slOutput.Free;
    slFingers.free;
    slFrets.free;
    slREcord.free;
  end;

end;

procedure Tcmd_MassageCSV_Z2.Massage_W(sFile: string; iStringNumber: nativeint; iFretNumber: nativeint);
var
  stream: TMBMemoryStringStream;
  header: TStringlist;
  parsed: TStringList;
  x,t: integer;
  s,sZ: string;
  c,d,a,r,u,l,ru,rl: nativeint;
  slBaseLines: TSTringlist;
  slOutput: TStringlist;
  sHeader: string;
  sFieldPRefix: string;
  sRecord: string;
  slRecord: TStringlist;
  slFingers, slFrets: TStringlist;
  iFieldIndex: nativeint;
  sOutputFile: string;
  cc, noize, noize_iterations: nativeint;
CONST
  OIDX=1+0;
  AIDX=1+32;
  RIDX=1+64;
  UIDX=1+(32*3);
  VIDX=1+(32*4);
  NOISE_MARGIN = 6;
  NOISE_ITERATIONS = 1;
begin
  header := nil;
  soutputFile := Z2MassagedCSVFile(FileName, '_W_', iFretNumber, iStringNumber);

  slBaseLines := TStringlist.Create;
  slOutput := tStringlist.Create;
  slOutput.sorted := true;
  slFingers := tStringlist.Create;
  slFrets := tStringlist.Create;
  slRecord := TStringlist.create;
  try
    stream := TMBMemoryStringStream.Create(sFile, fmOpenRead+fmShareDenyNone);
    stream.Loadindex;
    try
      header := TStringlist.Create;
      try
        parsed := TStringlist.Create;
        try
          slbaseLines.Clear;
          s := LoadStringFromFile(GEtz2BaseLineFile(sFile));
          parseString(s, ',', slBaseLines);


          //create header
          sHeader := 'attr,';
          for t:= 0 to fret_sensor_counts[iFretNumber]-1 do begin
            sHeader := sHeader+
//                        'd'+inttostr(t)+','+
//                        'a'+inttostr(t)+','+
//                        'r'+inttostr(t)+','+
//                        'u'+inttostr(t)+','+
//                        'l'+inttostr(t)+','+
//                        'ru'+inttostr(t)+','+
//                        'rl'+inttostr(t)+','+
                        'w'+inttostr(t);
            if t< fret_sensor_counts[iFretNumber]-1 then
              sHeader := sHeader+',';
          end;


          //get the field names into the header stringlist
          parsestring(Stream.Lines[0], ',', header);

          //for each line
          StepCount := stream.Count;
          for t:= 1 to stream.Count-1 do begin
//            if (t mod 100) = 0 then begin
              status := (extractfilename(sOutputFile)+' @'+inttostr(t));
              step := t;
//            end;

            //get the record
            ParseString(stream.lines[t], ',', parsed);
            sZ := '';

            for noize := 1 to NOISE_ITERATIONS do begin
              slRecord.Clear;
              slRecord.Add(parsed[0]);

              for x := 0 to fret_sensor_counts[iFretNumber]-1 do begin

                d := strtoint(parsed[1+(x*7)+0]);
                a := strtoint(parsed[1+(x*7)+1]);
                r := strtoint(parsed[1+(x*7)+2]);
                u := strtoint(parsed[1+(x*7)+3]);
                l := strtoint(parsed[1+(x*7)+4]);
                ru := strtoint(parsed[1+(x*7)+5]);
                rl := strtoint(parsed[1+(x*7)+6]);


                //todo 1 put out true only if finger matches
                //c := d - (a+r+(u-ru)+(l-rl));
                c := d - a;
                //c := d - (a+r{+(u-ru)+(l-rl)});

  //              slRecord.add(inttostr(d));
  //              slRecord.add(inttostr(a));                         1000
  //              slRecord.add(inttostr(r));
  //              slRecord.add(inttostr(u));
  //              slRecord.add(inttostr(l));
  //              slRecord.add(inttostr(ru));
  //              slRecord.add(inttostr(rl));
                cc := c + random(NOISE_MARGIN*2)-NOISE_MARGIN;
                slRecord.add(inttostr(cc));



              end;
              sRecord := UnParseString(',', slRecord);
              if slOutput.indexof(sRecord) < 0 then begin
                //Debug('cnt='+inttostr(slOutput.count));
                slOutput.Add(sRecord);
                //Debug('cnt='+inttostr(slOutput.count));
              end;
            end;
          end;

          sFile := soutputFile;

          slOutput.Sorted := false;
          slOutput.Insert(0, sHeader);
          slOutput.SaveToFile(sFile);

        finally
          parsed.free;
        end;
      finally
        stream.free;
        slBaseLInes.Free;
      end;
    finally
      header.Free;
    end;
  finally
    slOutput.Free;
    slFingers.free;
    slFrets.free;
    slREcord.free;
  end;

end;









function HasFinger(sString, sFret: string; iString, iFret: nativeint): boolean;
var
  slSTring,slFret: TSTringlist;
  t: nativeint;
begin
  result := false;
  slSTring := TStringlist.Create;
  try
    slFret := TStringlist.Create;
    try
      sString := unquote(sString);
      sFret := unquote(sFret);
      if pos(',', sString) > 0 then
        Debug(sString);
      if pos(',', sFret) > 0 then
        Debug(sFret);
      sString := StringReplace(sString, '"', '', [rfReplaceAll]);
      sFret := StringReplace(sFret, '"', '', [rfReplaceAll]);
      sString := StringReplace(sString, '''', '', [rfReplaceAll]);
      sFret := StringReplace(sFret, '''', '', [rfReplaceAll]);

      PArseString(sString, '|', slString);
      PArseString(sFret, '|', slFret);
      for t:= slSTring.Count-1 downto 0 do begin
        slString[t] := StringReplace(slString[t], '|', '', [rfReplaceAll]);
        if trim(slSTring[t]) = '' then slString.delete(t);
      end;
      for t:= slFret.count-1 downto 0  do begin
        slFret[t] := StringReplace(slFret[t], '|', '', [rfReplaceAll]);
        if trim(slFret[t]) = '' then slString.delete(t);
      end;
      for t:= 0 to lesserof(slSTring.count, slFret.Count)-1 do begin
      if (strtoint(slString[t])=iString) and (strtoint(slFret[t])=iFret) then begin
          result := true;
        end;
      end;

    finally
      slFret.free;
    end;
  finally
    slSTring.free;
  end;

end;

function Z2MassagedCSVFile(sDir, sPrefix: string; iFretNumber, iStringNumber: nativeint): string;
begin
  result := adjustpath(extractfilepath(sDir))+sPrefix+'massaged_fingers_'+inttostr(iFretNumber)+'_'+inttostr(iStringNumber)+'.csv';
end;


{ Tcmd_MasterProcess }

constructor Tcmd_MasterProcess.Create;
begin
  inherited;
  FileList := TStringlist.Create;
end;

destructor Tcmd_MasterProcess.Destroy;
begin
  FileList.Free;
  inherited;
end;

procedure Tcmd_MasterProcess.DoExecute;
begin
  inherited;
  z2_go;
end;

procedure Tcmd_MasterProcess.InitExpense;
begin
  inherited;
  CPUExpense := 0;
end;

procedure Tcmd_MasterProcess.SetAppend(const Value: boolean);
begin
  FAppend := Value;
end;


{ Tcmd_CSVWrite }

destructor Tcmd_CSVWrite.Destroy;
begin
  FReadFrom.Free;
  FReadFrom := nil;
  inherited;
end;

procedure Tcmd_CSVWrite.DoExecute;
var
  slREcord: TSTringlist;
  t: integer;
  iCount: nativeint;
begin
  inherited;
  slRecord := TStringlist.Create;
  try
    if self.ReadFrom.Count < 9 then
      exit;

    slRecord.Capacity := (WriteTo^.LAstField-WriteTo^.FirstField)+2+1;

    slRecord.Add(booltostr(HasFinger(self.ReadFrom[0], self.ReadFrom[1], WriteTo^.StringNumber, WriteTo^.FretNumber)));

//    slRecord.Add(Self.ReadFrom[0]);
//    slRecord.Add(Self.ReadFrom[1]);
{$IFDEF USE_SEPARATE_BASELINE}
    for t:= WriteTo^.FirstField to WriteTo^.LAstField do begin
      case ((t-WriteTo^.FirstField) mod 7) of {d,a,r,u,l,ru,rl}
        2: slRecord.Add(inttostr(strtoint(WriteTo^.BaseLine[t-2])-strtoint(WriteTo^.BaseLine[t-1])));
        5: slRecord.Add(inttostr(strtoint(WriteTo^.BaseLine[t-2])-strtoint(WriteTo^.BaseLine[t-4])));
        6: slRecord.Add(inttostr(strtoint(WriteTo^.BaseLine[t-2])-strtoint(WriteTo^.BaseLine[t-5])));
      else
        slRecord.Add(self.ReadFrom[t]);
      end;
    end;

    if WriteTo^.AboveFirstField >= 0 then begin
      for t:= WriteTo^.AboveFirstField to WriteTo^.AboveFirstField+WriteTo^.FieldCount-1 do begin
        case ((t-WriteTo^.AboveFirstField) mod 7) of {d,a,r,u,l,ru,rl}
          2: slRecord.Add(inttostr(strtoint(WriteTo^.BaseLine[t-2])-strtoint(WriteTo^.BaseLine[t-1])));
          5: slRecord.Add(inttostr(strtoint(WriteTo^.BaseLine[t-2])-strtoint(WriteTo^.BaseLine[t-4])));
          6: slRecord.Add(inttostr(strtoint(WriteTo^.BaseLine[t-2])-strtoint(WriteTo^.BaseLine[t-5])));
        else
          slRecord.Add(self.ReadFrom[t]);
        end;
      end;
    end;

    if WriteTo^.BelowFirstField >= 0 then begin
      for t:= WriteTo^.BelowFirstField to WriteTo^.BelowFirstField+WriteTo^.FieldCount-1 do begin
        case ((t-WriteTo^.BelowFirstField) mod 7) of {d,a,r,u,l,ru,rl}
          2: slRecord.Add(inttostr(strtoint(WriteTo^.BaseLine[t-2])-strtoint(WriteTo^.BaseLine[t-1])));
          5: slRecord.Add(inttostr(strtoint(WriteTo^.BaseLine[t-2])-strtoint(WriteTo^.BaseLine[t-4])));
          6: slRecord.Add(inttostr(strtoint(WriteTo^.BaseLine[t-2])-strtoint(WriteTo^.BaseLine[t-5])));
        else
          slRecord.Add(self.ReadFrom[t]);
        end;
      end;
    end;

{$ELSE}
    for t:= WriteTo^.FirstField to WriteTo^.LAstField do begin
      slRecord.Add(self.ReadFrom[t]);
    end;
{$ENDIF}

//    ReserveCSVResources;
//    WaitForResources;
    WriteTo^.Lock;
    try
      iCount := WriteTo^.slOutput.Count;
      WriteTo^.slOutput.Add(UnParseString(',',slRecord));
      if writeTo^.slOutput.Count <> iCount then
        WriteTo^.changed := true;
    finally
      WriteTo^.Unlock;
    end;
//    InitExpense;

  finally
    slRecord.Free;
  end;



end;

procedure Tcmd_CSVWrite.ExecuteInPlace;
begin
  DoExecute;
end;

procedure Tcmd_CSVWrite.InitExpense;
begin
  inherited;
  CPuExpense := 0.2;
  Resources.Clear;
end;


procedure Tcmd_CSVWrite.ReserveCSVResources;
begin
  if assigned(WriteTo) then begin
    Resources.Clear;
    resources.SetResourceUsage(extractfilenamepart(writeto^.FileName), 1.0);
  end;
end;

procedure Tcmd_CSVWrite.SetReadFrom(const Value: TStringlist);
begin
  if assigned(FReadFrom) then
    FReadFrom.Free;

  FReadFrom := CopyStringList(value)

//  FReadFrom := Value;
end;

procedure Tcmd_CSVWrite.SetWriteTo(const Value: PCSVOutputRecord);
begin
  FWriteTo := Value;
  InitExpense;
end;

{ TCSVOutputRecord }

procedure TCSVOutputRecord.CleanupAndSave;
begin
  slOutput.Sorted := false;
//  if Changed then begin
    if slOutput.count > 0 then
      if copy(slOutput[0],1,4) <> 'attr' then begin
        slOutput.Insert(0, Header);
      end;


    slOutput.SaveToFile(self.FileName);
//  end;
  slOutput.Free;
  active_commands.free;
  BaseLIne.Free;
//  cp.free;
  DeleteCRiticalSection(sect);
end;

procedure TCSVOutputRecord.ComputeFirstLastField;
var
  slHeader: TStringList;
begin
  slHeader := TStringlist.Create;
  try
    ParseString(LoadStringFromFile(CSVInputPath+'header.csv'), ',', slHeader);
    FirstField := slheader.IndexOf(FirstFieldName);
    AboveFirstField := slheader.IndexOf(AboveFieldName);
    BelowFirstField := slheader.IndexOf(BelowFieldName);

    LAstField := FirstField+fieldCount-1;
  finally
    slHeader.Free;
  end;

end;

procedure TCSVOutputRecord.Init;
begin
  InitializeCriticalSection(sect);
  Changed := false;
  stats.Init;
end;

procedure TCSVOutputRecord.Lock;
begin
  EnterCriticalSection(sect);
end;

function TCSVOutputRecord.SensorCount: nativeint;
begin
  result := fret_sensor_counts[fretnumber];
end;

procedure TCSVOutputRecord.Unlock;
begin
  LEaveCriticalSection(sect);
end;

{ Tcmd_MasterSplitCSV }

procedure Tcmd_MasterSplitCSV.DoExecute;
var

  fil: TFileInformation;
begin
  inherited;


  PreSplit;
  Old;

end;

function Tcmd_MasterSplitCSV.GEtAppend: boolean;
begin
  result := FMaster.Append;
end;

procedure Tcmd_MasterSplitCSV.InitExpense;
begin
  inherited;
  CPUExpense := 0;
end;

procedure Tcmd_MasterSplitCSV.Old;
var
  uu,tt,t,u,v,lin,lll: nativeint;

  c: Tcmd_CSvWrite;
//  cp: TCommandProcessor;
  batch: nativeint;
  sHeader: string;
  tmStart, tmNow: cardinal;
  rPercentComplete, rMin: nativefloat;
begin
  inherited;
  //setup output
  for u := 0 to HIGHEST_FRET do begin
    for t := 0 to HIGHEST_STRING do begin
      outputs[u][t].Init;
      outputs[u][t].BaseLIne := Tstringlist.create;
      ParseString(LoadStringFromFile(GetZ2BaseLineFile(InputDir)), ',', outputs[u][t].BaseLine);
      outputs[u][t].active_commands := TSharedList<Tcmd_CSVWrite>.create;
//          outputs[u][t].cp := TCommandProcessor.create(nil, 'splitter');
      outputs[u][t].slOutput := TStringList.Create;
      outputs[u][t].slOutput.Sorted := true;
      outputs[u][t].FileName := Z2MassagedCSVFile(InputDir, '_Split_', u, t);
      if Append and fileexists(outputs[u][t].FileName) then
        outputs[u][t].slOutput.LoadFromFile(outputs[u][t].FileName);
      outputs[u][t].FirstFieldName := 'str'+inttostr(t)+'frt'+inttostr(u)+'sen0_d';
      outputs[u][t].BelowFieldName := 'str'+inttostr(t+1)+'frt'+inttostr(u)+'sen0_d';
      outputs[u][t].AboveFieldName := 'str'+inttostr(t-1)+'frt'+inttostr(u)+'sen0_d';
      outputs[u][t].FieldCount := fret_sensor_counts[u]*7;
//          outputs[u][t].FirstField := header.IndexOf('str'+inttostr(t)+'frt'+inttostr(u)+'sen0_d');
//          outputs[u][t].LAstField := outputs[u][t].FirstField+((fret_sensor_counts[u]*7)-1);
      outputs[u][t].StringNumber := t;
      outputs[u][t].FretNumber := u;
      //create header for output files
      sHeader := 'attr,';
      for v:= 0 to fret_sensor_counts[u]-1 do begin
        sHeader := sHeader+
                    'd'+inttostr(v)+','+
                    'a'+inttostr(v)+','+
                    'r'+inttostr(v)+','+
                    'u'+inttostr(v)+','+
                    'l'+inttostr(v)+','+
                    'ru'+inttostr(v)+','+
                    'rl'+inttostr(v);

          sHeader := sHeader+',';
      end;

      if t > 0 then begin
        for v:= 0 to fret_sensor_counts[u]-1 do begin
          sHeader := sHeader+
                      'A_d'+inttostr(v)+','+
                      'A_a'+inttostr(v)+','+
                      'A_r'+inttostr(v)+','+
                      'A_u'+inttostr(v)+','+
                      'A_l'+inttostr(v)+','+
                      'A_ru'+inttostr(v)+','+
                      'A_rl'+inttostr(v);

            sHeader := sHeader+',';
        end;
      end;

      if t < 5 then begin
        for v:= 0 to fret_sensor_counts[u]-1 do begin
          sHeader := sHeader+
                      'B_d'+inttostr(v)+','+
                      'B_a'+inttostr(v)+','+
                      'B_r'+inttostr(v)+','+
                      'B_u'+inttostr(v)+','+
                      'B_l'+inttostr(v)+','+
                      'B_ru'+inttostr(v)+','+
                      'B_rl'+inttostr(v);

          if v< fret_sensor_counts[u]-1 then
            sHeader := sHeader+',';
        end;
      end;


      outputs[u][t].Header := sHeader;

      outputs[u][t].ComputeFirstLastField;
    end;
  end;
//  try

    Parse;

//  finally
    //cleanup outputs
{$IFDEF WTF}
    outputs[0][0].CleanupAndSave;
    outputs[1][0].CleanupAndSave;
    outputs[2][0].CleanupAndSave;
    outputs[3][0].CleanupAndSave;
    outputs[4][0].CleanupAndSave;
    outputs[0][1].CleanupAndSave;
    outputs[1][1].CleanupAndSave;
    outputs[2][1].CleanupAndSave;
    outputs[3][1].CleanupAndSave;
    outputs[4][1].CleanupAndSave;
    outputs[0][2].CleanupAndSave;
    outputs[1][2].CleanupAndSave;
    outputs[2][2].CleanupAndSave;
    outputs[3][2].CleanupAndSave;
    outputs[4][2].CleanupAndSave;
    outputs[0][3].CleanupAndSave;
    outputs[1][3].CleanupAndSave;
    outputs[2][3].CleanupAndSave;
    outputs[3][3].CleanupAndSave;
    outputs[4][3].CleanupAndSave;
    outputs[0][4].CleanupAndSave;
    outputs[1][4].CleanupAndSave;
    outputs[2][4].CleanupAndSave;
    outputs[3][4].CleanupAndSave;
    outputs[4][4].CleanupAndSave;
    outputs[0][5].CleanupAndSave;
    outputs[1][5].CleanupAndSave;
    outputs[2][5].CleanupAndSave;
    outputs[3][5].CleanupAndSave;
    outputs[4][5].CleanupAndSave;
{$ELSE}
    for uu := 0 to 4 do begin
      for tt := 0 to 5 do begin
        outputs[uu][tt].CleanupAndSave;
      end;
    end;
{$ENDIF}
//  end;

end;

procedure Tcmd_MasterSplitCSV.PArse;
var
  dir: TDirectory;
  fil: TFileInformation;
  fileparses: TCommandList<Tcmd_CSVPArse>;
  c: Tcmd_CSVParse;
  cc: Tcmd_CSVWrite;
  tt,uu,v: nativeint;
  active_commands: TSharedList<Tcmd_CSVWrite>;
  str: TMBMEmoryStringStream;
  fcomp: TFileInformation;
begin
  fileparses := TCommandList<Tcmd_CSVParse>.create;
  try
    dir := TDirectory.Create(InputDir, 'fingers.*.csv', 0,0, false, false);
    try
      fcomp := TFileInformation.Create;
//      if fileexists(CSVInputPath+'parse_time.dat') then
//        fcomp.LoadFromFile(CSVInputPath+'parse_time.dat');
      while dir.GetNextFile(fil) do begin

//        if fileexists(CSVInputPath+'parse_time.dat') and (fil.Date < fcomp.Date) then
//          continue;

        if self.Master.FileList.IndexOf(fil.Name) < 0 then
          continue;


        str := TMBMEmoryStringStream.create(fil.fullname, fmOpenReadWrite+fmShareDenyNone);
        try
          if not str.Loadindex then begin
            str.Count;
            str.SaveIndex;
          end;

          for tt := 0 to str.Count div SPLIT_SIZE do begin
            c := Tcmd_CSVPArse.Create;
            c.Master := self;
            c.FileNAme := fil.FullName;

            c.FirstLIne := greaterof(1,tt*SPLIT_SIZE);
            c.LastLIne := lesserof(str.Count-1, ((tt+1)*SPLIT_SIZE)-1);
            fileparses.Add(c);
            c.start;
          end;
        finally
          str.Free;
        end;
      end;


      while not fileparses.IsComplete do begin
        StepCount := 100;
        Step := round(fileparses.PercentComplete * 100);
        c := fileparses.FirstIncomplete;
        if c <> nil then begin
          Status := c.Status;
          SubStep := c.Step;
          SubStepCount := c.StepCount;
        end;

 //          Status := 'Intermediate cleanup';
          //cleanup any completed commands
          for uu := 0 to HIGHEST_FRET do begin
            for tt := 0 to HIGHEST_STRING do begin
              active_commands := outputs[uu][tt].active_commands;

              if active_commands.trylock then
              try
              //while active_commands.Count > 8 do begin
                for v:= active_commands.Count-1 downto 0 do begin
                  cc := active_commands[v];
                  if cc.IsComplete then begin
                    cc.Free;
                    active_commands.delete(v);
                  end;
                  if active_commands.count < 100 then break;
                end;
              finally
                active_commands.unlock;
              end;
              //end;
            end;
          end;
        sleep(1);
      end;

      while fileparses.Count > 0 do begin
        fileparses[0].WaitFor;
        fileparses[0].Free;
        fileparses.Delete(0);
      end;



    finally
      dir.Free;
    end;
  finally
    fileparses.Free;
  end;

end;

procedure Tcmd_MasterSplitCSV.PreSplit;
var
  dir: TDirectory;
  fil: TFileInformation;
  fileparses: TCommandList<Tcmd_CSVPArse>;
  c: Tcmd_CSVParse;
  fs: TMBMEmoryStringStream;
  slOutput: TStringlist;
  t: nativeint;
  b: boolean;
begin
  exit;
  slOutput := TStringlist.Create;
  fileparses := TCommandList<Tcmd_CSVParse>.create;
  try
    dir := TDirectory.Create(InputDir, 'fingers.*.csv', 0,0, false, false);
    while dir.GetNextFile(fil) do begin
      fs := TMBMemoryStringStream.Create(fil.FullName, fmOpenReadWrite+fmShareDenyNone);
      try
        if not fs.Loadindex then begin
          fs.Count;
          fs.SaveIndex;
        end;

        b := (fs.Count > 50001) and (pos('@', fil.Name) < 1);

        if b then begin
          Status := 'Found large file '+fil.Name+' ('+inttostr(fs.Count)+' lines)... splitting';
          slOutput.clear;
          slOutput.Add(fs.lines[0]);
          Stepcount := fs.Count;
          Step := 0;
          for t:= 1 to fs.Count-1 do begin
            slOutput.add(fs.lines[t]);
            if t mod 50000 = 0 then begin
              if slOutput.Count > 50001 then
                raise exception.create('wtf');
              slOutput.SaveToFile(fil.Path+'@'+fil.NamePart+'@'+inttostr(t)+fil.Extension);
              slOutput.Clear;
              slOutput.Add(fs.lines[0]);
              Step := t;
            end;
          end;



          if slOutput.Count > 1 then begin
            if slOutput.Count > 50001 then
              raise exception.create('wtf');
            slOutput.SaveToFile(fil.Path+'@'+fil.NamePart+'@end'+fil.Extension);
          end;
        end else begin
          copyfile(pchar(fil.FullName), pchar(fil.Path+'@'+fil.NamePart+fil.Extension), false);
        end;


      finally
        fs.Free;
//        if b then
//          RenameFile(fil.FullName, fil.Path++'.original');
      end;


    end;
  finally
    dir.Free;
    slOutput.Free;
  end;



end;

procedure Tcmd_MasterSplitCSV.RollUpStats;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

{ Tcmd_CSVPArse }

procedure Tcmd_CSVPArse.DoExecute;
begin
  inherited;
  if FirstLIne < 1 then FirstLine := 1;
  Parse;
end;

procedure Tcmd_CSVPArse.Init;
begin
  inherited;
  Icon := @CMD_ICON_SPLIT;
end;

procedure Tcmd_CSVPArse.Parse;
var
  active_commands: TSharedList<Tcmd_CSVWrite>;
  uu,tt,t,u,v,lin,lll: nativeint;
  sLine: string;
  input: TMBMemoryStringStream;
  header: TStringlist;
  sHeader: string;
  c: Tcmd_CSvWrite;
  c_Parses: TList<Tcmd_ParseString>;
  c_Parse: Tcmd_ParseString;
//  cp: TCommandProcessor;
  batch: nativeint;
  tmStart, tmNow: cardinal;
  rPercentComplete, rMin: nativefloat;
  line: TSTringlist;
begin
  inherited;

  //setup misc string lists
//  cp := TCommandProcessor.Create(nil, 'Parse');
  c_Parses := TList<Tcmd_ParseString>.create;
  header := TStringlist.Create;
  line := Tstringlist.Create;
  try

    //setup input
    input := TMBMemoryStringStream.Create(FileName, fmOpenRead+fmShareDenyNone);
    if not input.Loadindex then begin
      Status := 'Building index';
      input.Count;
      input.SaveIndex;
    end;

    ParseString(input.Lines[0], ',', header);
    try
      try
        StepCount := LAstLIne-firstLine;
        //for each line in the file
        Status := 'Splitting files';
        tmStart := GEtTickCount;
        for lll := FirstLIne to LastLine do begin
//          garbagecollect(line);
//          line := TStringlist.create;

//          rPErcentComplete := lll/input.Count;

//          tmNow := GetTickCount;
//          if rPercentComplete <> 1 then
//            rMin := ((GetTimeSInce(tmNow, tmStart)/1000)/(1-rPercentComplete))/60.0
//          else
//            rMin := 0;

          lin := lll;
          sLIne := input.lines[lin];
//          Status := 'Splitting files @'+inttostr(lin)+' ETC:'+floatprecision(rMin,2)+' minutes. ';
          Status := 'Splitting files @'+inttostr(lin);//'+' ETC:'+floatprecision(rMin,2)+' minutes. ';

          if (lin mod 16) = 0 then
            Step := lin-FirstLIne;

{$IFNDEF BATCH_PARSE}
            PArseString(sLIne, ',', line);
            //distribute to various
            for u := 0 to HIGHEST_FRET do begin
               for t := 0 to HIGHEST_STRING do begin
                c := Tcmd_CSVWrite.Create;
                c.WriteTo := @Master.outputs[u][t];
                c.ReadFrom := line;
{$IFDEF WRITECSV_IN_PLACE}
                  c.ExecuteInPlace;
                  c.Free;
{$ELSE}
                  c.Start();
                  c.WriteTo^.active_commands.Add(c);
{$ENDIF}
              end;
            end;
{$ELSE}
//          Status := 'Creating Batch';
          for batch := 0 to BATCH_SIZE-1 do begin
            if lin+batch >= input.Count then continue;
            sLine := input.Lines[lin+batch];
            //parse the line
            //PArseString(sLIne, ',', line);
            c_Parse := Tcmd_ParseString.Create;
            c_Parse.Input := sLine;
            c_Parse.Delimiter := ',';
//            c_Parse.CPUExpense := 0.05;
            c_Parse.Start();
            c_Parses.Add(c_Parse);
          end;

//          Status := 'Wait for batch '+inttostr(GEtCurrentThreadID);
          while c_Parses.Count > 0 do begin
            v := 0;
            for batch := c_Parses.Count-1 downto 0 do begin
              c_PArse := c_Parses[batch];
              if c_Parse = nil then begin
                inc(v);
                continue;
              end;
              if not c_Parse.IsComplete then
                continue;
              //distribute to various
              for u := 0 to HIGHEST_FRET do begin
                for t := 0 to HIGHEST_STRING do begin
                  c := Tcmd_CSVWrite.Create;
                  c.WriteTo := @Master.outputs[u][t];
                  c.ReadFrom := c_Parse.Output;
{$IFDEF WRITECSV_IN_PLACE}
                  c.ExecuteInPlace;
                  c.Free;
{$ELSE}
                  c.Start();
                  c.WriteTo^.active_commands.Add(c);
{$ENDIF}
                end;
              end;
              c_Parse.Free;
              c_Parses[batch] := nil;//.Delete(batch);
              c_Parses.Delete(batch);

            end;
            if v = c_Parses.Count then
              c_Parses.Clear;
          end;
{$ENDIF}
        end;

        //wait for all commands

        for uu := 0 to HIGHEST_FRET do begin
          for tt := 0 to HIGHEST_STRING do begin
            active_commands := MAster.outputs[uu][tt].active_commands;
            while active_commands.Count > 0 do begin
              for v:= active_commands.Count-1 downto 0 do begin
                c := active_commands[v];
                if c.IsComplete then begin
                  c.Free;
                  active_commands.Remove(c);
                end;
              end;
            end;
          end;
        end;


      finally
      end;

    finally
      input.Free;
    end;
  finally
    header.Free;
    line.Free;
    //active_commands.Free;
//    cp.Free;
    c_parses.Free;
  end;
end;

{ TParseStats }

procedure TOutputStats.ApplyValues(d, a, r, u, l, ru, rl: nativeint);
begin
  if (d-a) > MinTrueOverAmbient then
    MinTrueOverAmbient := d-a;

  if (d-a) > MaxFalseOverAmbient then
    MaxFalseOverAmbient := d-a;

end;

procedure TOutputStats.Init;
begin
  MinTrueOverAmbient := 9999999;
  MAxFAlseOverAmbient := -1;
end;

{ Tcmd_RollupResults }

procedure Tcmd_RollupResults.DoExecute;
var
  t,s,d,a,r,u,l,ru,rl: nativeint;
  sHeader: string;
  slHeader: TStringlist;
  sLine: string;
  slLine: TStringlist;
begin
  inherited;

  sHeader := FOutputRecord.slOutput[0];
  slHeader := ParseString(sHeader, ',');
  try
    for t := 0 to FOutputRecord.slOutput.Count-1 do begin
      sLine := FOutputRecord.slOutput[t];
      slLine := ParseString(sLine, ',');
      try
        for s := 0 to fret_sensor_counts[Foutputrecord.FretNumber]-1 do begin
          d := strtoint(GetFieldValue(slHeader, slLIne, 'd'+inttostr(s)));
          a := strtoint(GetFieldValue(slHeader, slLIne, 'a'+inttostr(s)));
          r := strtoint(GetFieldValue(slHeader, slLIne, 'r'+inttostr(s)));
          u := strtoint(GetFieldValue(slHeader, slLIne, 'u'+inttostr(s)));
          l := strtoint(GetFieldValue(slHeader, slLIne, 'l'+inttostr(s)));
          ru := strtoint(GetFieldValue(slHeader, slLIne, 'ru'+inttostr(s)));
          rl := strtoint(GetFieldValue(slHeader, slLIne, 'rl'+inttostr(s)));
          FOutputRecord.stats.ApplyValues(d,a,r,u,l,ru,rl);

        end;
      finally
        slLine.free;
      end;
    end;

  finally
    slHeader.free;
  end;


end;

{ Tcmd_EvaluateEquation }


function Tcmd_EvaluateEquation.BSFunc_SimpleDirect(test: int64): nativeint;
var
  a1, a2: nativefloat;
begin
  if test > 512 then begin
    result := 1;
    exit;
  end;




  DirectThreshold := (test-256)-1;
  Reset;
  Go;
  a1 := FalseAccuracy;
  DirectThreshold := test - 256;
  Reset;
  Go;
  a2 := FalseAccuracy;

  status := inttostr(fretnumber)+'_'+inttostr(stringnumber)+'D: '+inttostr(DirectThreshold)+' V:'+inttostr(VArianceThreshold)+' '+inttostr(round(TrueAccuracy*100))+'%/'+inttostr(round(FalseAccuracy*100))+'%';


  if (a1 >= TargetAccuracy) and (a2 >= TargetAccuracy) then
    result := 1
  else
  if (a1 < TargetAccuracy) and (a2 >= TargetAccuracy) then
    result := 0
  else
    result := -1;


end;


function Tcmd_EvaluateEquation.BSFunc_SimpleDirect_VarianceStage(test: int64): nativeint;
var
  a1, a2: nativefloat;
begin
  if test > 512 then begin
    result := 1;
    exit;
  end;




  VarianceThreshold := test-1;
  Reset;
  Go;
  a1 := FalseAccuracy;
  VarianceThreshold := test;
  Reset;
  Go;
  a2 := FalseAccuracy;

  status := 'D: '+inttostr(DirectThreshold)+' V:'+inttostr(VArianceThreshold)+' '+inttostr(round(TrueAccuracy*100))+'%/'+inttostr(round(FalseAccuracy*100))+'%';

  if (a2 >= TargetAccuracy) and (a1 >= TargetAccuracy) then
    result := 1
  else
  if (a1 < TargetAccuracy) and (a2 >= TargetAccuracy) then
    result := 0
  else
    result := -1;


end;


function Tcmd_EvaluateEquation.Compute: boolean;
begin
  if Evaluator = bExpected then begin
    if (bExpected) then
      inc(TrueHits)
    else
      inc(FalseHits);
    result := true;
  end else begin
    if (bExpected) then
      inc(TrueMisses)
    else
      inc(FalseMisses);
    result := false;
  end;
end;

procedure Tcmd_EvaluateEquation.DetermineQualityThresholds;
var
  t: integer;
begin
  TrueQuality := GEtQualityThreshold(true, 0.20);
  FAlseQuality := GEtQualityThreshold(false, 0.80);


end;

procedure Tcmd_EvaluateEquation.DoExecute;
var
  acc: nativefloat;
begin
  Setup;
  try

    Evaluator := Evaluate_Suppressed;
    acc := 1.0;
    repeat
      VarianceThreshold := 1024;
      TargetAccuracy := acc;
      BinarySearch(BSFunc_SimpleDirect);
      TargetAccuracy := acc;
      BinarySearch(BSFunc_SimpleDirect_VarianceStage);
      acc := acc - 0.01;
      StepCount := 100;
      if FalseAccuracy > 0 then
        Step := round((TrueAccuracy / FalseAccuracy) * 100);

    until TrueAccuracy >=acc;
  finally
    FInish;
  end;



end;

procedure Tcmd_EvaluateEquation.DoExecuteOld;
const
  INTERMEDIATE_TARGET_ACCURACY = 0.9999;
  TARGET_ACCURACY = 0.99;
var
  ii: nativeint;
  LAst: nativeint;
begin
  FalseMisses := 99999;
  DirectThreshold := -255;
  VarianceThreshold := 512;
  ii := 256;
  while abs(ii) > 1 do begin
    Last := DirectThreshold;

    Reset;
    DirectThresHold := DirectThreshold + ii;
    status := 'D: '+inttostr(DirectThreshold)+' V:'+inttostr(VArianceThreshold);
    Go;

    if (ii > 0) and (FalseAccuracy > INTERMEDIATE_TARGET_ACCURACY) then begin
      ii := 0-(ii div 3);
    end else
    if (ii < 0) and (FalseAccuracy <= INTERMEDIATE_TARGET_ACCURACY) then begin
      ii := 0-(ii div 3);
    end;

    if FalseAccuracy <= INTERMEDIATE_TARGET_ACCURACY then
      ii := abs(ii);

    if FalseAccuracy >= INTERMEDIATE_TARGET_ACCURACY then
      ii := 0-abs(ii);

  end;


  repeat
    Last := DirectThreshold;
    Reset;
    DirectThresHold := DirectThreshold -1;
    status := 'D: '+inttostr(DirectThreshold)+' V:'+inttostr(VArianceThreshold);
    Go;
  until FalseAccuracy <= INTERMEDIATE_TARGET_ACCURACY;

  repeat
    Last := DirectThreshold;
    Reset;
    DirectThresHold := DirectThreshold + 1;
    status := 'D: '+inttostr(DirectThreshold)+' V:'+inttostr(VArianceThreshold);
    Go;
  until FalseAccuracy > INTERMEDIATE_TARGET_ACCURACY;

  //DirectThreshold := Last;
  Reset;
  Go;

  ii := -256;

  while abs(ii) > 1 do begin
    Last := VarianceThreshold;

    Reset;

    status := 'D: '+inttostr(DirectThreshold)+' V:'+inttostr(VArianceThreshold);
    Go;

    if (ii < 0) then begin
      if (TrueAccuracy >= TARGET_ACCURACY) then begin
        ii := abs(ii div 2);//flip to pos
      end else begin
        ii := 0-abs(ii);
      end;



      VarianceThreshold := VarianceThreshold + ii;


      if variancethreshold <=0 then begin
        varianceThreshold := 0;
        ii := 0-(ii div 2);
      end;


    end else begin
      if (TrueAccuracy < TARGET_ACCURACY) then begin
        ii := 0-abs(ii div 2);//flip to neg
      end else begin
        ii := abs(ii);
      end;
      VarianceThreshold := VarianceThreshold + ii;

      if variancethreshold >512 then begin
        variancethreshold := 512;
        ii := 0-(ii div 2);
      end;


    end;
  end;

  repeat
    Last := VarianceThreshold;
    Reset;
    VarianceThreshold := VarianceThreshold + 1;
    status := 'D: '+inttostr(DirectThreshold)+' V:'+inttostr(VArianceThreshold);
    Go;

  until TrueAccuracy < TARGET_ACCURACY;

  repeat
    Last := VarianceThreshold;
    Reset;
    VarianceThreshold := VarianceThreshold - 1;
    status := 'D: '+inttostr(DirectThreshold)+' V:'+inttostr(VArianceThreshold);
    Go;

  until TrueAccuracy >= TARGET_ACCURACY;

  Variancethreshold := last;
  Reset;
  Go;


end;

procedure Tcmd_EvaluateEquation.Setup;
begin
  InputFile := Z2MassagedCSVFile(CSVInputPath, '_Split_', fretNumber,STringNumber);
  slMiss := TStringlist.create;
  input := TMBMemoryStringStream.Create(INputFile, fmOpenRead+fmShareDenyNone);
  input.BuildAndSaveIndex;
  ssA.Count := SensorCount;
  ssB.Count := SensorCount;
  ss.Count := SensorCount;


  DetermineQualityThresholds;

end;

procedure Tcmd_EvaluateEquation.Finish;
begin
  slMiss.free;
  input.Free;
end;

function Tcmd_EvaluateEquation.GetQualityThreshold(
  bAttr: boolean; rQuality: nativefloat): nativeint;
var
  a: array of nativeint;
  t,tt: integer;
  slHeader: TSTringlist;
  slLine: TStringlist;
  idx: integer;
  f: TFretSensorDataArray;
  fa: TFretSensorDataArray;

begin
  setlength(a, input.Count-1);
  slHeader := ParseString(input[0], ',');
  idx := slHeader.indexof('d0');

  Status := 'Determining Quality Thresholds';
  tt := 0;
  for t:= 1 to input.Count-1 do begin
    slLine := ParseString(input.Lines[t], ',');
    try
      if strtobool(slLIne[0]) = bAttr then begin
      f.count := SensorCount;
      f.FillFromStringList(slLine, idx);
      f.ComputeAVGUnwanted;

        if battr then
          a[tt] := f.Min
        else
          a[tt] := f.Max;
        inc(tt);
      end;
    finally
      slLIne.Free;
    end;
  end;

  setlength(a, tt);

  SortArray(a);

  result := a[round(tt * rQuality)];

end;

procedure Tcmd_EvaluateEquation.Go;
var
  t,s: nativeint;
  sHeader: string;
  slHeader: TStringlist;
  sLine: string;
  slLine: TStringlist;

  iAvg: nativeint;
  idx, idx_, idx_A, idx_B: nativeint;
begin
  Reset;
  slMiss.clear;
    try
      sHeader := input.Lines[0];
      slMiss.Add(sHeader);
      slHeader := ParseString(sHeader, ',');
      try
        idx_A := slHeader.IndexOf('A_d0');
        idx_B := slHeader.IndexOf('B_d0');
        idx_ := slHeader.IndexOf('d0');
        for t := 1 to input.count-1 do begin
          sLine := input.lines[t];
          slLine := ParseString(sLine, ',');
          try
            iAvg := 0;
            for s := 0 to fret_sensor_counts[FretNumber]-1 do begin
              idx := idx_ + (s*7);
              ss.a[s].FillFromStringList(slLIne, idx);
              iAvg := iAvg + (ss.a[s].u - ss.a[s].ru);
              iAvg := iAvg + (ss.a[s].l - ss.a[s].rl);
            end;

            iAvg := iAvg div fret_sensor_counts[FretNumber];
            ss.avg_unwanted := iAvg;


            if StringNumber > 0 then begin
              iAvg := 0;
              for s := 0 to fret_sensor_counts[FretNumber]-1 do begin
                idx := idx_A + (s*7);
                ssA.a[s].FillFromStringList(slLIne, idx);
                iAvg := iAvg + (ssA.a[s].u - ssA.a[s].ru);
                iAvg := iAvg + (ssA.a[s].l - ssA.a[s].rl);
              end;

              iAvg := iAvg div fret_sensor_counts[FretNumber];
              for s := 0 to fret_sensor_counts[FretNumber]-1 do begin
                ssA.a[s].avg_unwanted := iAvg;
              end;
            end;

            if strtobool(slLine[0]) then begin
              if ss.Min < TrueQuality then continue;
            end else begin
              if ss.Max > FAlseQuality then continue;
            end;





            if StringNumber < 5 then begin
              iAvg := 0;
              for s := 0 to fret_sensor_counts[FretNumber]-1 do begin
                idx := idx_B + (s*7);
                ssB.a[s].FillFromStringList(slLIne, idx);
                iAvg := iAvg + (ssB.a[s].u - ssB.a[s].ru);
                iAvg := iAvg + (ssB.a[s].l - ssB.a[s].rl);
              end;

              iAvg := iAvg div fret_sensor_counts[FretNumber];
              for s := 0 to fret_sensor_counts[FretNumber]-1 do begin
                ssB.a[s].avg_unwanted := iAvg;
              end;
            end;






            bExpected := strtobool(slLine[0]);

            //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if not Compute then
              slMiss.Add(sLine);
            //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          finally
            slLine.free;
          end;
        end;

        //slMiss.SaveToFile(Z2MassagedCSVFile(CSvInputPath, '_MISS_', FretNumber, StringNumber));
      finally
        slHeader.free;
      end;
    finally
    end;
end;

procedure Tcmd_EvaluateEquation.InitExpense;
begin
  inherited;
{$IFDEF SINGLE_THREAD_EVAL}
  MemoryExpense := 1;
{$ENDIF}
end;

procedure Tcmd_EvaluateEquation.REset;
begin
  FalseHits := 0;
  FalseMisses := 0;
  TrueHits := 0;
  TrueMisses := 0;
end;

function Tcmd_EvaluateEquation.Evaluate: boolean;
begin
  result := FEValuator;
end;

function Tcmd_EvaluateEquation.Evaluate_Simple: boolean;
var
  t: nativeint;
begin
  result := false;
  for t:= 0 to SensorCount-1 do begin
    if ss.a[t].CertainLight > DirectThreshold then begin
      result := true;
    end;
  end;

end;

function Tcmd_EvaluateEquation.VarianceMax: nativeint;
var
  tt: integer;
  cLAst: nativeint;
  delta: nativeint;
begin
  result := 0;
  cLAst := ss.a[0].CertainLight;
  for tt := 1 to SensorCount-1 do begin
    delta := abs(ss.a[tt].CertainLight-cLast);
    if delta > result then result := delta;
    cLAst:= ss.a[tt].CertainLight;
  end;
end;

function Tcmd_EvaluateEquation.VarianceSum: nativeint;
var
  tt: integer;
  cLAst: nativeint;
begin
  result := 0;
  cLAst := ss.a[0].CertainLight;
  for tt := 1 to SensorCount-1 do begin
    inc(result, abs(ss.a[tt].CertainLight-cLast));
    cLAst:= ss.a[tt].CertainLight;
  end;
end;
function Tcmd_EvaluateEquation.Evaluate_Simple_PlusVariance: boolean;
var
  t: nativeint;
begin
  result := false;
  for t:= 0 to SensorCount-1 do begin
    if ss.a[t].CertainLight > DirectThreshold then begin
      result := true;
    end else begin
      if VarianceMax > VArianceThreshold then
        result := true;
    end;
  end;
end;

function Tcmd_EvaluateEquation.Evaluate_Sum: boolean;
var
  t: nativeint;
  iSum: nativeint;
begin
  result := false;
  iSum := 0;
  for t:= 0 to SensorCount-1 do begin
    inc(iSum, ss.a[t].CertainLight);
  end;

  result := iSum >=80;


end;

function Tcmd_EvaluateEquation.Evaluate_Suppressed: boolean;
var
  t: nativeint;
  iLight: nativeint;
begin
  result := false;
  for t:= 0 to SensorCount-1 do begin
    if StringNumber > 0 then begin
      iLight := ss.a[t].CertainLight - (ssA.a[t].CertainLight div 5);
    end else begin
      iLight := ss.a[t].CertainLight;
    end;
    if iLight > DirectThreshold then begin
      result := true;
    end else begin
      if VarianceMax > VArianceThreshold then
        result := true;
    end;
  end;
end;

function Tcmd_EvaluateEquation.FalseAccuracy: nativefloat;
begin
  result := 0;
  try
    result := FalseHits/(Falsemisses+FAlseHits);
  except
  end;
end;


function Tcmd_EvaluateEquation.SensorCount: nativeint;
begin
  result := fret_sensor_counts[fretnumber];
end;

function Tcmd_EvaluateEquation.TrueAccuracy: nativefloat;
begin
  result := 0;
  try
    result := TrueHits/(truemisses+trueHits);
  except
  end;

end;

{ TFretSensorData }

function TFretSensorData.CertainLight_Standard: nativeint;
begin
  result := d - (a+r+((u-ru) div 1)+((l-rl) div 1));
end;


procedure TFretSensorData.FillFromStringList(sl: TStringlist;
  iStartat: nativeint);
begin
  d := strtoint(sl[iStartAt+0]);
  a := strtoint(sl[iStartAt+1]);
  r := strtoint(sl[iStartAt+2]);
  u := strtoint(sl[iStartAt+3]);
  l := strtoint(sl[iStartAt+4]);
  ru := strtoint(sl[iStartAt+5]);
  rl := strtoint(sl[iStartAt+6]);

end;

function TFretSensorData.RAA: nativeint;
begin
  result := d-a;
end;

function TFretSensorData.CertainLight: nativeint;
begin
//  result := CertainLight2(0.7);
  result := CertainLight3;
end;

function TFretSensorData.CertainLight2(rULRatio: nativefloat): nativeint;
begin
  result := d - (a+r+round((u-ru) * rULRatio)+round((l-rl) * rULRatio));
end;

function TFretSensorData.CertainLight3: nativeint;
begin
  result := round(d - (a+r+((u-ru) div 2)+((l-rl) * 0.7)));
  result := result - round((avg_unwanted * 0.5));


end;

{ Tcmd_MasterEvaluateEquation }

procedure Tcmd_MasterEvaluateEquation.DoExecute;
var
  cl: TCommandList<Tcmd_EvaluateEquation>;
  f,s,t: nativeint;
  c: TCmd_EvaluateEquation;

begin
  inherited;

  cl := TCommandList<Tcmd_EValuateEquation>.create;
  try
    for f := 0 to 4 do begin
      for s := 0 to 5 do begin
        c := Tcmd_EvaluateEquation.Create;
        c.FretNumber := f;
        c.StringNumber := s;
        c.Start;
        cl.Add(c);
      end;
    end;

    try
      while not cl.IsComplete do begin
        self.StepCount := 100;
        Self.Step := round(cl.PercentComplete * 100);
        self.Status := cl.FirstIncomplete.Status;
        sleepex(100, true);
      end;


      FOutput := 'fret,string,D,V,truehits,truemisses,falsehits,falsemisses,trueaccuracy,falseaccuracy'#13#10;

      cl.WaitForAll;

      for t:= 0 to cl.Count-1 do begin
        FOutput := FOutput+inttostr(cl[t].FretNumber)+','+
                          inttostr(cl[t].STringNumber)+','+
                          inttostr(cl[t].DirectThreshold)+','+
                          inttostr(cl[t].VarianceThreshold)+','+
                          inttostr(cl[t].TrueHits)+','+
                          inttostr(cl[t].TrueMisses)+','+
                          inttostr(cl[t].FalseHits)+','+
                          inttostr(cl[t].FalseMisses)+','+
                          floattostr(cl[t].TrueHits/(cl[t].TrueHits+cl[t].TrueMisses))+','+
                          floattostr(cl[t].FalseHits/(cl[t].FalseHits+cl[t].FalseMisses))+#13#10;
      end;


      SaveStringAsFile(CSVInputPath+'eval_results.csv', FOutput);


    finally
      cl.ClearAndDestroyCommands;
    end;






  finally
    cl.Free;
  end;

end;

procedure Tcmd_MasterEvaluateEquation.Init;
begin
  inherited;
  CPUexpense := 0;
end;

procedure Tcmd_MasterEvaluateEquation.InitExpense;
begin
  inherited;

end;

{ TFretSensorDataArray }

procedure TFretSensorDataArray.ComputeAVGUnwanted();
var
  t: nativeint;
  iAvg: nativeint;
begin
  iAVg := 0;
  for t:= 0 to count-1 do begin
    iAvg := iAvg + ((a[t].u-a[t].ru)+(a[t].l-a[t].rl));
  end;
  iAvg := iAVg div count;
  self.avg_unwanted := iAVg;

end;

procedure TFretSensorDataArray.FillFromStringList(sl: tSTringlist;
  iStartAT: integer);
var
  t: integer;
begin
  for t:= 0 to Count-1 do begin
    a[t].FillFromStringList(sl, iStartAT+(t*7));
  end;

end;

function TFretSensorDataArray.Max: nativeint;
var
  t: integer;
begin
  result := 0;
  for t:= 0 to Count-1 do begin
    if result < Self.a[t].CertainLight then
      result := a[t].CertainLight;
  end;
end;

function TFretSensorDataArray.Min: nativeint;
var
  t: integer;
begin
  result := 99999999;
  for t:= 0 to Count-1 do begin
    if result > Self.a[t].CertainLight then
      result := a[t].CertainLight;

  end;

end;

procedure TFretSensorDataArray.SetAvgUnwanted(const Value: nativeint);
var
  s: nativeint;
begin
  FAvgUnwanted := Value;
  for s := 0 to count-1 do begin
    a[s].avg_unwanted := FAvgUnwanted;
  end;


end;

end.
