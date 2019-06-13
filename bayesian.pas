unit bayesian;
{$INLINE AUTO}
interface

uses
  numbers, tickcount, dirfile, windows, memoryfilestream, debug, simplemail, backgroundcommandprocessor, generics.collections, dir, beeper,httpclient,commandprocessor, systemx, sharedobject, classes, sysutils, stringx, orderlyinit, betterobject;

const
  MINIMUM_TOKEN_INSTANCES = 500;
  FULL_GOOGLE_WEIGHT = 1800000000;
  ZERO_GOOGLE_WEIGHT = 3470000000;
type
  Tcmd_GetGoogleWordStats = class;

  TBayesianDictionary = class;//forward

  TWordStat = record
    count: integer;
    spam: integer;
    google: int64;
    timestamp: TdateTime;
    cmd: Tcmd_GetGoogleWordStats;
    wordlength: integer;
    localcommonality: double;
    localtimestamp: TDateTime;
    procedure Init;
    function Ratio: double;
    function CertaintyRating: real;
    function GoogleWeight: double;
    function Weight: double;
    function WeightedRatio: double;
    procedure SaveToStream(s: TStream);
    procedure LoadFromStream(s: TStream);
    function Score: double;
    function PossibleScore: double;
    function NeedsREfresh: boolean;
    function Debug: string;
  private
    procedure SetGoogleFromWeight(w: double);
    function GetWeight(bIncludeLocal: boolean): double;
  public
    procedure ForceGoogleRefresh;
  end;

  TWordStatEx = record
    word: string;
    ws: TWordStat;
  end;
  PWordStat = ^TWordSTat;


  TWordStore = class (TFakeLockQueuedObject)
  private
    FWords: TStringList;
    Fstats: array of TWordStat;
    FIdx: string;
    FDict: TBayesianDictionary;
    FDataLoaded: boolean;
    FChanged: boolean;
    function GEtDataPAth: string;
    function GetWordStat(sWord: string): PWordStat;
    procedure SetWordStat(sWord: string; bSpam: boolean; rMultiplier: double);
    procedure ForceWordStat(sWord: string; iSpamCount, itotalCount: integer);
  public
    constructor create;override;
    destructor destroy;override;
    procedure LoadData;
    procedure SaveData;
    property DataPath: string read GEtDataPAth;
    property Dict: TBayesianDictionary read FDict write FDict;

    property Idx: string read FIdx write FIdx;
    property DataLoaded: boolean read FDataLoaded write FDataLoaded;
    function OverallWeightedRAtio: double;
    function OverallWeightedPossible: double;
    function OverallWeightedSpam: double;
    function MostCommonWordCount: int64;
    procedure CheckDataLoaded;
    procedure SetWordCommonality(sWord: string; icommonality: int64);
    function Incrementaloptimize: boolean;
  end;

  TBayesianResults = class(TFakeLockQueuedObject)
  protected
    FWords: array of TWordStatEx;
    FRef: integer;
  public
    procedure AddWord(sWord:string; pws: PWordSTat);
    function Ratio: double;
    procedure AddRef;
    procedure ReleaseRef;
  end;

  TwordSummaryData = record
    totalwords: int64;
    totaldocuments: int64;
  end;
  TBayesianDictionary = class (TFakeLockQueuedObject)
  private
    FLoaded: boolean;
    FLogCounter: array[0..1] of integer;
    function GetStore(idx: char): TWordStore;
    function GetStoreByRefString(refString: string): TWordStore;
    procedure LoadFromLog1(bSpam: boolean; dir: TDirectory);
    function RefreshGoogleStats(sWord: string; CE: TCommandProcessor;
      bBanned: boolean): boolean;

  protected
    bantime: TDateTime;
    FStores: TList<TWordStore>;
    FsummaryData: TWordSummaryData;
    loadingfromlog: boolean;
    FHamSampleRatio: real;
    CE: TCommandProcessor;
    function DataPath: string;
    property DataLoaded: boolean read FLoaded write FLoaded;

    function OverallRatio: double;

  public
    FLastSaveTime: cardinal;
    constructor Create;override;
    destructor Destroy;override;

    function GetWordStat(sWord: string; bNoRefresh: boolean = false): PWordStat;
    function GetWordLocalCommonality(sWord: string): double;
    function GetWordCompositeCommonality(sWord: string): double;
    procedure SetWordStat(sWord: string; bSpam: boolean; rMultiplier: double);


    function OverallWeightedRatio: double;
    function OverallWeightedSpam: double;
    function OverallWeightedPOssible: double;
    function MostCommonWordCount: int64;

    procedure SaveData(bForce: boolean = false);
    function GetTextSpamRatio(sText: string; out sLog:string): double;
    function GetTextSpamProbability(sText: string): double;
    procedure CommitText(sText: string; bSpam: boolean; rMultiplier: double);
    procedure AsyncCommitText(sText: string; bSpam: boolean; iMultiplier: double);
    procedure CReateStores;
    procedure FreeStores;
    property Stores[idx: char]: TWordStore read GetStore;
    property StoresByRefString[refString: string]: TWordStore read GetStoreByRefString;
    function ShouldLoadFromLog: boolean;
    procedure LoadFromLog;
    function BannedFromGoogle: boolean;
    function LoadLogCounter(bSpam: boolean): integer;
    procedure IncLogCounter(bSpam: boolean);
    procedure SetWordCommonality(sWord: string; iCommonality: int64);
    procedure forceWordStat(sWord: string; iSpamCount, iTotal: integer);
  end;

  Tcmd_CommitText = class(TCommand)
  private
    FSpam: boolean;
    FText: string;
    Fmult: double;
  public
    procedure InitExpense;override;
    procedure DoExecute;override;
    property Spam: boolean read FSpam write FSpam;
    property Text: string read FText write FText;
    property Multiplier: double read Fmult write Fmult;
  end;

  Tcmd_GetGoogleWordStats = class(TCommand)
  private
    FWord: string;
    FResult: int64;
    fpb: boolean;
    FGoogban: boolean;
    function TryGoogle: boolean;
    function TryLive: boolean;
  public
    procedure InitExpense;override;
    procedure DoExecute;override;
    property Word: string read FWord write FWord;
    property Result: int64 read FResult write FResult;
    property PreBanned: boolean read fpb write fpb;
    property BannedFromGoogle: boolean read FGoogban write FGoogban;
  end;

function PrepareText(const sText: string): string;

var
  baydict: TBayesianDictionary;




implementation

{ TBayesianDictionary }

procedure TBayesianDictionary.AsyncCommitText(sText: string; bSpam: boolean; iMultiplier: double);
var
  c: Tcmd_CommitText;
begin
  if ShouldLoadFromLog then LoadFromLog;

  c := Tcmd_CommitText.create;
  c.FireForget := true;
  c.Spam := bSpam;
  c.Text := sText;
  c.Multiplier := iMultiplier;
  c.Start();
end;



function TBayesianDictionary.BannedFromGoogle: boolean;
begin
  result := (now-BanTime) < 1/12;
end;

procedure TBayesianDictionary.CommitText(sText: string; bSpam: boolean; rMultiplier: double);

var
  sl: TStringlist;
  sPrior, s1,s2: string;
  t: integer;
  dir : TDirectory;
  sFileSpec: string;

  procedure BeginSaveTExt;
  begin
    //
  end;
  procedure EndSaveText;
  var
    s: string;
  begin
    s := inttostr(LoadLogCounter(bSpam));
    if bSpam then
      sFileSpec := '.spam.log'
    else
      sFileSpec := '.ham.log';



    SaveStringAsFile(DataPath+'Log\'+s+sFileSpec, floattostr(rMultiplier)+#13#10+sText);

  end;

begin
  if bSpam then begin
    //SayNatural('Calculating bayesian statistics for spam.');
  end else begin
    //SayNatural('Calculating bayesian statistics for non-spam.');
  end;

  sl := nil;
  try
    if length(sTExt) > 64000 then
      exit;
    sText := PrepareText(sText);

    s2 := sText;

    sPrior := '';
    if s2 = '' then exit;

    sl := SplitStringIntoStringList(s2);
    for t:= 0 to sl.count-1 do begin
      s1 := sl[t];
      if s1 = '' then continue;
//      LockRead;
      try
        if (sPrior <> '') and (s1 <> '') then begin
          SetWordStat(PrepareText(sPrior+' '+s1), bSpam, rMultiplier);
        end;
        if s1 <> '' then begin
          SetWordStat(PrepareText(s1), bSpam, rMultiplier);
        end;
      finally
//        UnLockRead;
      end;

      sPrior := s1;
    end;

    SaveData;
    LockWRite;
    try
      try
        if not loadingfromlog then EndSaveText;
      except
        on E: Exception do begin
          Debug.Log(e.message);
        end;
      end;


    finally
      UnlockWrite;
    end;


  finally
    sl.free;
  end;



end;


constructor TBayesianDictionary.Create;
begin
  inherited;
  FLogCounter[0] := -1;
  FLogCounter[1] := -1;
  FLastSavetime := GetTicker;
  CreateStores;
  CE := TCommandProcessor.create(nil,'Google Words');
  CE.Throttle := 500;


end;

procedure TBayesianDictionary.CReateStores;
var
  t: integer;
  c: char;
  ws: TWordStore;
begin
  FStores := Tlist<TWordStore>.create;
  for t:= 0 to 26 do begin
    ws := TWordStore.create;
    if (t < 26) then
      ws.Idx := chr(ord('a')+t)
    else
      ws.idx := 'high';

    ws.Dict := self;
    FStores.add(ws);
    ws.LoadData;
  end;
end;

function TBayesianDictionary.DataPath: string;
begin
  result := DLLPath+'Data\'
end;

destructor TBayesianDictionary.Destroy;
begin
  FReeStores;
  CE.free;
  inherited;
end;




procedure TBayesianDictionary.FreeStores;
var
  t: integer;
begin
  for t:= 0 to FStores.count-1 do begin
    FStores[t].free;
  end;
  FStores.free;
  FStores := nil;

end;

function TBayesianDictionary.GetStore(idx: char): TWordStore;

var
  i:integer;
begin
  LockWrite;
  try
    if ord(idx) > 255 then begin
      i := 26;
    end else begin
      i := ord(idx)-ord(ansichar('a'));
    end;
    if (i > 25) or (i < 0) then begin
      i := 25;

    end;
    result := Fstores[i];
  finally
    UnlockWrite;
  end;

end;

function TBayesianDictionary.GetStoreByRefString(refString: string): TWordStore;
var
  astr: string;
  ac: char;
begin
  astr := lowercase(refString);
  if length(astr) = 0 then
    raise exception.create('no word');

  ac := astr[1];

  result := stores[ac];

end;

function TBayesianDictionary.GetTextSpamProbability(sText: string): double;
var
  r1,r2: double;
  slSummary: TStringlist;
  sLog: string;
begin
//  if WordCount < 100000 then begin
//    SayNatural('Not enough spam collected to determine spam.');
//    result := 0; exit;
//  end;

  if OverallRatio = 0 then
    result := 0
  else begin
    r1 := GetTextSpamRatio(sText, sLog);
    //result := r1;
    //exit;
    r2 := OverallRatio;

    result := (r1/(r2))/2;

    sLog := '[LOG]'#13#10+sLog+'-----------'#13#10+floattostr(r1)+'/'+floattostr(r2)+'/2='+floattostr(result);

    if pos('[LOG]', sText) < 1 then begin
      SaveStringAsFile(DLLPath+'AnalLog\'+floattostr(now)+'.csv', sLog);
//      SimpleMail.SendMail('poop@digitaltundra.com','jason@maudlinmusic.com', 'Analysis', sLog);
    end;

    if result > 1 then result := 1;
  end;

//  if result > 0.5 then
//    SayNatural('Message is likely spam')
//  else
//    SayNatural('Message is probably okay');


end;

function TBayesianDictionary.GetTextSpamRatio(sText: string; out sLog: string): double;
type
  TLastMatch = (mtPhrase, mtLeft, mtRight, mtNone);
var
  t: integer;
  sPrior, s1,s2,ss: string;
  ws,wsp,wst: PWordStat;
  rwsp,rwst,rws: double;
  spamhits, totalpossible: double;
  iRefreshed: integer;
  lm: TLastMatch;
  sLogLine: string;
  slLog: TStringlist;
begin
  result := 0;
  sText := PrepareText(sText);
  spamhits := 0;
  totalpossible := 0;
  iRefreshed := 0;
  lm := mtNone;
//  LockRead;
  slLog := Tstringlist.create;
  try
    s2 := sText;
    sPrior := '';
    while SplitString(s2, ' ',s1,s2) do begin
      if s1 <> '' then begin

        ss := sPrior+' '+s1;
        wsp := GetWordStat(sPrior);
        wst := GetWordStat(s1);
        ws := GetWordStat(ss);

        if wsp <> nil then rwsp := wsp.CertaintyRating else rwsp := -1;
        if wst <> nil then rwst := wst.CertaintyRating else rwst := -1;
        if ws <> nil then rws := ws.CertaintyRating else rws := -1;



//        if rws >= rwst then begin
//          if rws >= rwsp then begin
            if (ws <> nil) and (rws > -1) then begin
              spamhits := spamhits + ws.Score;
              totalpossible := totalpossible + ws.PossibleScore;
              slLog.add(ss+','+ws.Debug);
              lm := mtPhrase;
            end;
//          end;
//        end;

//        if rwsp > rws then begin
//          if rwsp > rwst then begin
            if (wsp <> nil) and (rwsp > -1) then begin
              if lm <> mtRight then begin
                spamhits := spamhits + wsp.Score;
                totalpossible := totalpossible + wsp.PossibleScore;
                slLog.add(sPrior+','+ws.Debug);
              end;
              lm := mtLeft;
            end;
//          end;
//        end;

//        if rwst > rws then begin
//          if rwst > rwsp then begin
            if (wst <> nil) and (rwst > -1) then begin
              spamhits := spamhits + wst.Score;
              totalpossible := totalpossible + wst.PossibleScore;
              slLog.add(s1+','+ws.Debug);
              lm := mtRight;
            end;
//          end;
//        end;

        if (TotalPossible = 0) then begin
          result := 0;
        end else begin
          result := spamhits/totalpossible;
        end;
        sPrior := s1;
//        if ws.Ratio > result then
//          result := ws.Ratio;
      end;
    end;

    //if totalpossible < round(WordCount * 0.0002) then begin
//      SayNatural('words are too rare to determine spam');
    //  result := 0;
    //end;
  finally
    sLog := slLog.text;
    slLog.free;
//    UnlockRead;
  end;

end;

function TWordStore.GetWordStat(sWord: string): PWordStat;
var
  i: integer;
begin
  LockRead;
  try
    CheckDataLoaded;
    i := FWords.indexof(lowercase(sWord));
    if (i>=0) then
      result := @FStats[i]
    else begin
      result := nil;
  //    result.Count :=0;
  //    result.spam := 0;
    end;
  finally
    UnlockRead;
  end;

end;

function TWordStore.Incrementaloptimize: boolean;
var
  t: integer;
  ws: TWordStat;
  s: string;
begin
  result := false;
  for t := 0 to FWords.Count - 2 do begin
    if FStats[t].count < FStats[t+1].count  then begin
      ws := FStats[t];
      FStats[t] := Fstats[t+1];
      Fstats[t+1] := ws;

      s := FWords[t];
      FWords[t] := FWords[t+1];
      FWords[t+1] := s;

      result := true;
      exit;
    end;
  end;



end;

function TBayesianDictionary.GetWordCompositeCommonality(sWord: string): double;
var
  ws: PWordStat;
begin
  result := 0;
  ws := GetWordStat(sWord);

  if ws <> nil then begin
    result := GetWordLocalCommonality(sWord)*ws.Weight;
  end;
end;

function TBayesianDictionary.GetWordLocalCommonality(sWord: string): double;
var
  iCount, iMax: int64;
  ws: PWordStat;
begin
  ws := GetwordStat(sWord, true);
  result := 0;

  if ws <> nil then begin
    result := ws.count/MostCommonWordcount;
    result := result * 4;
    if result > 1 then result := 1;

    result := 1-result;

    ws.localcommonality := result;
    ws.localtimestamp := now;


  end;

end;

function TBayesianDictionary.GetWordStat(sWord: string; bNoRefresh: boolean = false): PWordStat;
var
  store: TWordStore;
begin

  result := nil;
  if sWord = '' then exit;
//  Lockread;
  try
    store := storesbyrefstring[sWord];
    result := store.GetWordStat(sWord);

    if result = nil then exit;
    //if now-bantime < 1/24 then exit;
    if (not bNoRefresh) then begin
      GetWordLocalCommonality(sword);
      if result.needsrefresh then begin
        store.LockWrite;
        try
          if result.needsrefresh then begin
            if not RefreshGoogleStats(sWord, CE, BannedFromGoogle) then begin
              bantime := now;
            end;
          end;
        finally
          store.UnlockWrite;
        end;
      end;
    end;
  finally
//    UnlockRead;
  end;
end;


procedure TBayesianDictionary.IncLogCounter(bSpam: boolean);
begin
  if bSpam then
    FLogCounter[1] := FLogCounter[1] + 1
  else
    FLogCounter[0] := FLogCounter[0] + 1;
end;

procedure TBayesianDictionary.LoadFromLog1(bSpam: boolean; dir: TDirectory);
var
  s,s1,s2: string;
  i: integer;
  fil: TFileInformation;
  t: integer;
  L: TList;
  iTarget: integer;
  iSeed: integer;
begin
  l := TList.create;


  iSeed := 5000;
  if bSpam then begin
    iSeed := trunc(dir.Filecount * FHamSampleRatio);
  end else begin
    if (dir.filecount = 0) or (dir.fileCount < iSeed) then
      FHamSampleRatio := 1
    else
      FHamSampleRatio := iSeed/dir.filecount;
  end;


  iTarget := LesserOf(dir.filecount, iSeed);


  for t:= 0 to dir.Filecount - 1 do begin
    L.Add(dir.Files[t]);
  end;


  while L.count > iTarget do begin
    i := trunc(Random * L.count-1);
    L.delete(i);
  end;


  try
    for t := 0 to L.count - 1 do begin
      s := LoadSTringFromFile(TfileInformation(L[t]).fullname);
      SplitSTring(s, #13#10, s1,s2);
      i := strtoint(s1);
      if (i=0) then
        i := 1;
      self.ASyncCommitText(s2, bSpam, i);
    end;
  finally
    l.free;
  end;

end;
function TBayesianDictionary.LoadLogCounter(bSpam: boolean): integer;
var
  dir: TDirectory;
  idx: integer;
  sFileSpec: string;
begin
  LockWrite;
  try
    if bSpam then idx := 1 else idx := 0;

    result := FLogCOunter[idx];

    if result > -1 then begin
      IncLogCounter(bSpam);
      exit;
    end;


    if bSpam then
      sFileSpec := '*.spam.log'
    else
      sFileSpec := '*.ham.log';

    dir := TDirectory.create(DataPath+'Log\', '*'+sFileSpec, 0,0, true);

    try
      if dir.filecount = 0 then
        FLogCounter[idx] := 0
      else begin
        FLogCounter[idx] := dir.filecount;
      end;
    finally
      dir.free;
    end;
  finally
    UnlockWrite;
  end;


end;

function TBayesianDictionary.MostCommonWordCount: int64;
var
  t: integer;
begin
//  LockRead;
  try
    result := 1;
    for t:= 0 to FStores.count-1 do begin
      if FStores[t].MostCommonWordCount > result then
        result := FStores[t].MostCommonWordCount;
    end;
  finally
//    UnlockRead;
  end;

end;

procedure TBayesianDictionary.LoadFromLog;
var
  dir1, dir2: TDirectory;
begin
  loadingfromlog := true;
  dir1 := TDirectory.create(datapath+'log\', '*.ham.log', 0,0,true, false);
  dir2 := TDirectory.create(datapath+'log\', '*.spam.log', 0,0,true, false);
  LoadFromLog1(false, dir1);
  LoadFromLog1(true,dir2);
  dir1.free;
  dir2.free;
  BGCmd.WaitForAll;
  loadingfromlog := false;

end;

function TBayesianDictionary.OverallRatio: double;
begin
  result := OverallWeightedRatio;
end;

function TBayesianDictionary.OverallWeightedPOssible: double;
var
  t: integer;
begin
//  LockRead;
  try
    result := 0;
    for t:= 0 to FStores.count-1 do begin
      result := result + FStores[t].OverallWeightedPossible;

    end;
    result := result;
  finally
//    UnlockRead;
  end;

end;


function TBayesianDictionary.OverallWeightedRatio: double;
var
  t: integer;
  a,b: double;
begin
//  LockRead;
  try
    result := 0;
    a := 0;
    b := 0;

    for t:= 0 to FStores.count-1 do begin
      a := A + FStores[t].OverallWeightedspam;
      b := b + FStores[t].OverallWeightedPossible;

    end;
    if b<=0 then
      result:= 0
    else
      result := a/b;
  finally
//    UnlockRead;
  end;

end;


function TBayesianDictionary.OverallWeightedSpam: double;
var
  t: integer;
begin
//  LockRead;
  try
    result := 0;
    for t:= 0 to FStores.count-1 do begin
      result := result + FStores[t].OverallWeightedSpam;

    end;
  finally
//    UnlockRead;
  end;

end;

type
  TChartype = (ctnone, ctUpperCase, ctLowerCase, ctnumber, ctsymbol, ctSpace);

function GetCharType(c: char):TChartype;
begin
  if charInSet(c, [' ']) then begin
    result := ctSpace;
  end else
  if CharInSet(c, ['A'..'Z']) then begin
    result := ctUpperCase;
  end else
  if CharInSet(c, ['a'..'z']) then begin
    result := ctLowercase;
  end else
  if charInSet(c, ['0'..'9']) then begin
    result := ctNumber;
  end else
  if (c= #0) then begin
    result := ctNone;
  end else
    result := ctSymbol;

end;

function PrepareText(const sText: string): string;
var
  t: integer;
  prev, next, wordstart: char;

begin
    prev := #0;
    next := #0;
    wordstart := #0;
    result := sText;

    t := 1;
    while t < length(result) do begin
      if t > 1 then prev := result[t-1] else prev := #0;
      if t < length(result) then next := result[t+1] else next := #0;


      //figure out word start
      if GetCharType(prev) in [ctNone, ctSpace, ctSymbol] then begin
        wordstart := result[t];
      end else begin
        //if not at the start of a word

        //if uppercase in the middle of a word, then add a space (camelback camouflage)
        if (GetCharType(prev) in [ctLowercase]) and (GetChartype(result[t]) in [ctUppercase]) then begin
          result := copy(result, 1, t-1)+' '+copy(result, t, length(result)-(t-1));
          continue;
        end;

        //if a symbol or number in the middle of a word, convert the symbol into something that kinda makes sense
        if GetCharType(wordstart) in [ctUppercase, ctLowercase] then
        if GetChartype(result[t]) in [ctSymbol, ctNumber] then begin
          case result[t] of
            '1','|': result[t] := 'i';
            '0': result[t] := 'o';
            '4': result[t] := 'a';
            '5','$': result[t] := 's';
            '3': result[t] := 'e';
            '6': result[t] := 'g';
          end;
        end;

      end;




      if not (CharInSet(result[t], ['a'..'z', 'A'..'Z', '@'{, widechar(256)..widechar(65535)}])) then begin
        result[t] := ' ';
      end;

      inc(t);
    end;

    result := TRim(RemoveDoubles(result, ' '));
    //while pos('  ',sText) > 0 do begin
      //sText := StringReplace(sText, '  ', ' ', []);
    //end;
  //result := sText;

end;


procedure TBayesianDictionary.SaveData(bForce: boolean = false);
var
  t: integer;
  tm: cardinal;
begin
  tm := GetTicker;
  if (not bforce) and (GetTimeSince(tm, FLastSaveTime) < 60000) then begin
    exit;
  end;


  for t:= 0 to FStores.count-1 do begin
    FStores[t].SaveData;
  end;

  FLastSaveTime := tm;

end;

procedure TBayesianDictionary.SetWordCommonality(sWord: string;
  iCommonality: int64);
var
  ws: TWordStore;
  ps: PWordStat;
begin
  ws := StoresByRefString[sWord];
  ws.SetWordCommonality(sWord, iCommonality);


end;

procedure TBayesianDictionary.forceWordStat(sWord: string; iSpamCount: integer; iTotal: integer);
var
  ws: TWordStore;
  ps: PWordStat;
begin
  ws := StoresByRefString[sWord];
  ws.ForceWordStat(sWord, iSpamCount, iTotal);

end;


procedure TBayesianDictionary.SetWordStat(sWord: string; bSpam: boolean;
  rMultiplier: double);
begin
  storesbyrefstring[sWord].SetWordStat(sWord, bSpam, rMultiplier);

end;

function TBayesianDictionary.ShouldLoadFromLog: boolean;
var
  dir: TDirectory;
begin
  if loadingfromlog then begin
    result := false;
    exit;
  end;
  dir := TDirectory.create(DAtaPath, '*.*', 0,0, false);

  result := dir.filecount = 0;
  dir.free;

end;

procedure TWordStore.SetWordCommonality(sWord: string; icommonality: int64);
var
  ws: PWordStat;
begin
  lOCKWRITE;
  try
    ws := GetwordStat(sWord);
    if ws <> nil then begin
      ws.Google := iCommonality;
    end;
  FINALLY
    unlockwrite;
  end;
end;

procedure TWordStore.SetWordStat(sWord: string; bSpam: boolean; rMultiplier: double);
var
  i: integer;
  s: string;
begin
  LockWrite;
  try
    if rMultiplier > 1 then rMultiplier := 1;

    CheckDataLoaded;
    i := FWords.indexof(lowercase(sWord));
    if (i >=0) then begin

      //if a negative multiplier then subtract previous instances before adding
      if rMultiplier < 0 then begin
        dec(FStats[i].Count, round(abs(rMultiplier)));

        //if this time it is NOT spam, then that must mean that the
        //instance we're replacing WAS marked as spam before, so decrement some shit
        if not bSpam then
          dec(FStats[i].Spam, (round(abs(rMultiplier))));

      end;

      inc(FStats[i].Count, round(abs(rMultiplier)));

      if bSpam then
        inc(FStats[i].Spam, round(abs(rMultiplier)));

  //    if sWord = 'medications' then begin
  //      SayNatural('Medications '+inttostr(FStats[i].Spam)+' in '+inttostr(FStats[i].Count));
  //    end;

      if (i/FWords.Count+1) > 0.5 then
        IncrementalOptimize;


    end else begin
      FWords.add(lowercase(sWord));
      setlength(FStats, length(FStats)+1);
      FStats[length(FStats)-1].Init();
      FStats[length(FStats)-1].Count := round(rMultiplier);
      if (bSpam) then
        FStats[length(Fstats)-1].Spam := round(rMultiplier)
      else
        Fstats[length(FStats)-1].Spam := 0;
      FStats[length(FStats)-1].wordlength := length(sWord);
    end;
    FChanged := true;
  finally
    UnlockWrite;
  end;





end;

procedure TWordStore.ForceWordStat(sWord: string; iSpamCount, itotalCount:integer);
var
  i: integer;
begin
  LockWrite;
  try

    CheckDataLoaded;
    i := FWords.indexof(lowercase(sWord));
    if (i >=0) then begin

      FStats[i].Count := iTotalCount;

      Fstats[i].Spam := iSpamcount;
    end else begin
      FWords.add(lowercase(sWord));
      setlength(FStats, length(FStats)+1);
      FStats[length(FStats)-1].Init();
      FStats[length(FStats)-1].Count := iTotalCount;
      FStats[length(Fstats)-1].Spam := iSpamCount;
      FStats[length(FStats)-1].wordlength := length(sWord);
    end;
    FChanged := true;
  finally
    UnlockWrite;
  end;





end;





{ TWordStat }

procedure TWordStat.LoadFromStream(s: TStream);
begin
  s.Read(self, sizeof(self));
end;

function TWordStat.PossibleScore: double;
begin
  result := Count*Weight;
end;

function TWordStat.Ratio: double;
begin
//  if Count < MINIMUM_TOKEN_INSTANCES then begin //must see at least 20 of this word
//    result := 0.0;
//    exit;
//  end;
  if PossibleScore = 0 then
    result := 0
  else
    result := Score/PossibleScore;
end;


function TWordStat.NeedsREfresh: boolean;
begin
  result :=  (now-timestamp >= 0.01);
end;


function TBayesiandictionary.RefreshGoogleStats(sWord: string; CE: TCommandProcessor; bBanned: boolean): boolean;
var
  c: Tcmd_GetGoogleWordStats;
  ws,ws1,ws2: PWordStat;
  s1,s2: string;
begin
  result := false;

  ws := GetWordStat(sWord, true);
  if ws = nil then exit;

  if pos(' ', sWord) > 0 then begin
    SplitString(sWord, ' ', s1,s2);
    ws1 := GetWordStat(s1);
    ws2 := GetWordStat(s2);
    ws.SetgoogleFromWeight((ws1.googleweight+ws2.googleweight) / 2);
    result := true;
    exit;
  end;


  result := true;

  if not ws.needsrefresh then
    exit;

  c := Tcmd_GetGoogleWordStats.create;
  c.Word := sword;
  c.Start(CE);
  c.Prebanned := bBanned;
  try
    c.WaitFor;

    ws.google := c.result;
    result := not c.bannedfromgoogle;
    if result then
      ws.TimeSTamp := now;
  finally
    c.free;
  end;
end;

procedure TWordStat.SaveToStream(s: TStream);
begin
  s.Write(self, sizeof(self));
end;

function TWordStat.Score: double;
begin
  result := Spam*Weight;
end;

{ Tcmd_CommitText }

procedure Tcmd_CommitText.DoExecute;
var
  sl: TStringlist;
  s1,ss,s2: string;
  t: integer;
  sText: string;
begin

  sl := TStringlist.create;
  try
    sText := PrepareText(FText);

    s2 := sText;

    //split up into chunks
    while true do begin
      if SplitString(s2, ' the ', s1,s2) then begin
        sl.add(s1+' the ');
      end else begin
        sl.add(s1);
        break;
      end;
    end;



//    SayNatural('Committing');
    for t:= sl.count-1 downto 0 do begin
      if sl[t] = '' then
        sl.delete(t);
    end;

    for t:= 0 to sl.count-1 do begin
      s1 := sl[t];
      StepCount := sl.count;
      Step := t;
      if sl.count = 1 then
        baydict.CommitText(s1, Spam, Multiplier)
      else
        baydict.AsyncCommitText(s1, Spam, Multiplier)

    end;


  finally
    sl.free;
  end;




end;

procedure Tcmd_CommitText.InitExpense;
begin
  inherited;
  CPuExpense := 0.66;
end;


{ Tcmd_GetGoogleWordStats }

function Tcmd_GetGoogleWordStats.TryGoogle: boolean;
var
  cli : THTTPClient;
  s,s1,s2: string;
begin
  self.result := 0;
  result := false;
  cli := THTTPClient.create;
  try
    try
      Status := 'Google: '+Word;
      cli.Get('http://www.google.com/search?hl=en&q=%22'+Word+'%22&btnG=Google+Search&aq=f&oq=','http://www.google.com');
      s := cli.InBody;
      if Pos('we''re sorry...', lowercase(s)) > 0 then begin
        self.result := -1;
        result := false;
        BannedFromGoogle := true;
      end;

      if SplitString(s, 'of about <b>', s1,s2) then begin
        if SplitString(s2, '</b>', s1,s2) then begin
          s1 := StringReplace(s1, ',', '', [rfReplaceAll]);
          s1 := STringREplace(s1, ' ', '', [rfReplaceAll]);
          self.Result := strtoint64(s1);
          result := true;
        end;
      end;

    except
      result := false;
      self.Result := -1;
    end;

  finally
    cli.free;
  end;


end;

function Tcmd_GetGoogleWordStats.TryLive: boolean;
var
  cli : THTTPClient;
  s,s1,s2: string;
begin
  self.result := 0;
  result := false;
  cli := THTTPClient.create;
  try
    try
      Status := 'Live: '+Word;

      //http://www.bing.com/search?q=customer&go=&form=QBLH&qs=n
      cli.Get('http://www.bing.com/search?q='+Word+'&go=&form=QBLH&qs=n','http://www.bing.com');
      s := lowercase(cli.InBody);

      if SplitString(s, '<span class="sb_count" id="count">', s1,s2) then begin
        if SplitString(s2, ' of ', s1,s2) then begin
          if SplitString(s2, ' results', s1,s2) then begin
            s1 := StringReplace(s1, ',', '', [rfReplaceAll]);
            s1 := STringREplace(s1, ' ', '', [rfReplaceAll]);
            self.Result := strtoint64(s1);
            result := true;
          end;
        end;
      end;

    except
      result := false;
      self.Result := -1;
    end;

  finally
    cli.free;
  end;
end;

procedure Tcmd_GetGoogleWordStats.DoExecute;
begin
  inherited;
  Prebanned := true;
  if PreBanned or (not TryGoogle) then
    TryLive;

end;

procedure Tcmd_GetGoogleWordStats.InitExpense;
begin
  inherited;
  NEtworkExpense := 0.1;
  CPUExpense := 0.01;

end;

{ TWordWeight }


function TWordStat.CertaintyRating: real;
var
  r, rMinSeenProgress: real;
begin
  rMinSeenProgress := PossibleScore/100;
  if rMinSeenProgress > 1 then rMinSeenProgress := 1;

  r := RAtio;
  r := 0.5-r;
  r := abs(r);
  r := r*rMinSeenProgress;
  result := r*(1-Weight);

//  r := LocalCommonality;

end;

function TWordStat.Debug: string;
begin
  try
    result := 'Spam/Cnt,'+inttostr(spam)+','+inttostr(count)+',Weight,'+floattostr(weight)+',Score/Possible,'+floattostr(score)+','+floattostr(possiblescore);
  except
    on e: exception do begin
        result := e.message;
    end;
  end;
end;

procedure TWordStat.ForceGoogleRefresh;
begin
  timestamp := 0;
end;

procedure TWordStat.Init;
begin
  FillMem(@self, sizeof(self), 0);
end;

function TWordStat.GetWeight(bIncludeLocal: boolean): double;
var
  workingval: double;
  baseval: double;
begin
  if (Google <= 0) or (wordLength=1) then
    result := 0
  else begin
    workingval := Google;
    workingval := workingval - FULL_GOOGLE_WEIGHT;
    if workingval <= 0 then workingval := 1;
    baseval := ZERO_GOOGLE_WEIGHT - FULL_GOOGLE_WEIGHT;
    if workingval = 0 then begin
      result := 0;
    end
    else begin
      result := 1-(workingval/ZERO_GOOGLE_WEIGHT);
      if result > 1 then
        result := 1;
      if result < 0 then
        result := 0;
      if bIncludelocal then begin
        result := result * LocalCommonality;
      end else begin
        result := result;
      end;
    end;
  end;
end;

function TWordStat.Weight: double;
begin
  result := GetWeight(true);
end;

function TWordStat.Googleweight: double;
begin
  result := getWeight(false);
end;


procedure TwordStat.SetGoogleFromWeight(w: double);
begin
  self.google := trunc(FULL_GOOGLE_WEIGHT + (w * (ZERO_GOOGLE_WEIGHT - FULL_GOOGLE_WEIGHT)));
end;

function TWordStat.WeightedRatio: double;
begin
  if PossibleScore = 0 then
    result := 0
  else
    result := Score/PossibleScore;
end;

{ TWordStore }

procedure TWordStore.CheckDataLoaded;
begin
  if not dataloaded then
    loaddata;
end;

constructor TWordStore.create;
begin
  inherited;
  FWords := TStringlist.create;
end;

destructor TWordStore.destroy;
begin
  FWords.free;
  inherited;
end;

function TWordStore.GEtDataPAth: string;
begin
  result := Dict.DataPath;

end;

procedure TWordStore.LoadData;
var
  sl: TStringList;
  fs: TMemoryFileStream;
  t: integer;
begin

  sl := TStringList.create;
  LockWrite;
  try
    if DataLoaded then
      exit;
    sl.clear;
    if not fileexists(DataPath+'bayesian.'+Idx+'.words.txt') then begin
      DataLoaded := true;
      exit;
    end;

    FWords.LoadFromFile(datapath+'bayesian.'+Idx+'.words.txt');
    for t:= 0 to FWords.count-1 do begin
      FWords[t] := trim(FWords[t]);

    end;


    fs := TMemoryFileStream.create(datapath+'bayesian.'+Idx+'.words.dat', fmOpenRead+fmShareDenyNone);
    try
      setlength(FStats, fs.size div sizeof(TWordStat));
      for t := 0 to length(FStats)-1 do begin
        if t >=FWords.count then
            continue; //prevents data corruption

        fs.Read(FStats[t],sizeof(FStats[t]));
        if FStats[t].weight = 0 then begin
          FStats[t].timestamp := 0.0;
        end;
      end;
    finally
      fs.free;
    end;

    DataLoaded := true;

  finally
    UnlockWrite;
    sl.free;
  end;
end;

function TWordStore.MostCommonWordCount: int64;
var
  t: integer;
  a: int64;
begin
  CheckDataLoaded;
  LockRead;
  try
    a := 0;
    for t:= low(FStats) to high(FStats) do begin
      if FStats[t].Count > a then
        a := FStats[t].count;

    end;

    result := a;
  finally
    UnlockRead;
  end;
end;


function TWordStore.OverallWeightedPossible: double;
var
  t: integer;
  a,b: double;
begin
  CheckDataLoaded;
  LockRead;
  try
    a := 0;
    b := a;
    for t:= low(FStats) to high(FStats) do begin
      b := b+FStats[t].PossibleScore;


    end;
    result := b;
  finally
    UnlockRead;
  end;

end;


function TWordStore.OverallWeightedSpam: double;
var
  t: integer;
  a,b: double;
begin
  CheckDataLoaded;
  LockRead;
  try
    a := 0;
    b := a;
    for t:= low(FStats) to high(FStats) do begin
      a := a+FStats[t].Score;


    end;
    result := a;
  finally
    UnlockRead;
  end;

end;

function TWordStore.OverallWeightedRAtio: double;
var
  t: integer;
  a,b: double;
begin
  CheckDataLoaded;
  LockRead;
  try
    a := 0;
    b := a;
    for t:= low(FStats) to high(FStats) do begin
      a := a+FStats[t].Score;
      b := b+FStats[t].PossibleScore;


    end;
    if b = 0 then
      result := 0
    else
      result := a/b;
  finally
    UnlockRead;
  end;
end;

procedure TWordStore.SaveData;
var
  sl: TStringList;
  fs: TMemoryFileStream;
  t: integer;
begin
  if not DataLoaded then exit;
  sl := TStringList.create;

  if not FChanged then exit;

  LockWrite;
  try

    ForceDirectories(datapath);

    fs := TMemoryFileStream.create(datapath+'bayesian.'+Idx+'.words.dat', fmCreate);
    try

      for t := 0 to length(FStats)-1 do begin
        fs.Write(FStats[t],sizeof(FStats[t]));
      end;
    finally
      fs.free;
    end;

    FWords.SaveToFile(datapath+'bayesian.'+Idx+'.words.txt');


//    fs := TFileStream.create(datapath+'bayesian.'+Idx+'.words.dat', fmCreate);
//    try
//
//      for t := 0 to length(FStats)-1 do begin
//        fs.Write(FStats[t],sizeof(FStats[t]));
//      end;
//    finally
//      fs.free;
//    end;



    {$IFDEF DEBUG_WORD_FILES}
    sl.clear;
    for t := 0 to length(FStats)-1 do begin
      //fs.Write(FStats[t],sizeof(FStats[t]));
      try
        sl.add(FWords[t]+' Spam:'+inttostr(Fstats[t].Spam)+'/'+inttostr(FStats[t].Count)+' Google:'+inttostr(FStats[t].Google));
      except
        on E: Exception do begin
          sl.add(e.message);
        end;
      end;
    end;


    sl.SaveToFile(DAtaPath+'bayesian.'+Idx+'.words.debug.txt');
    {$ENDIF}

  finally
    FChanged := false;
    UnlockWrite;
    sl.free;

  end;
end;

{ TBayesianResults }

procedure TBayesianResults.AddRef;
begin
  lockwrite;
  try
    inc(Fref);
  finally
    unlockwrite;
  end;
end;

procedure TBayesianResults.AddWord(sWord: string; pws: PWordSTat);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TBayesianResults.Ratio: double;
begin
  RESULT := 0;
end;

procedure TBayesianResults.ReleaseRef;
begin
  lockwrite;
  try
    dec(Fref);
  finally
    unlockwrite;
  end;
end;

procedure oinit;
begin
//  baydict := TBayesianDictionary.create;
end;

procedure ofinal;
begin
//  baydict.free;

end;

initialization
  init.RegisterProcs('bayesian', oinit, ofinal);


finalization



end.
