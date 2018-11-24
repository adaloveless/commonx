unit sentiment;

interface

uses
  debug, classes,tools, betterobject, sharedobject, systemx, typex, stringx, BetterFileStream, MemoryFileStream, MultiBufferMemoryFileStream, sysutils, spider, https, webstring;

type
  TWordScore = record
    term: string;
    posscore: double;
    negscore: double;
    function ObjScore: double;
    function Sentiment: double;
    class operator Add(a,b: TWordScore): TWordScore;
    procedure Init;
    function ToString: string;
  end;

  TSentiWordNet = class(TSharedObject)
  strict protected
    a: array of TWordScore;
    procedure Load;
    function IndexOfword(s: string): ni;
  public
    constructor Create; override;
    function Analyze(s: string): TWordScore;
    function AnalyzeWord(s: string): TWordScore;

    function AnalyzeURL(sURL: string; iLevels: ni = -1): TWordScore;
    function AnalyzeWebSearch(sTerms: string): TWordScore;

  end;



implementation




{ TWordScore }

class operator TWordScore.Add(a, b: TWordScore): TWordScore;
begin
  result.posscore := a.posscore + b.posscore;
  result.negscore := a.negscore + b.negscore;

end;

procedure TWordScore.Init;
begin
  posscore := 0;
  negscore := 0;
  term := '';
end;

function TWordScore.ObjScore: double;
begin
  result := (1-(PosScore+NegScore));
end;

function TWordScore.Sentiment: double;
begin
  result := Posscore-Negscore;
end;

function TWordScore.ToString: string;
begin
  result := 'Sentiment: '+floattostr(Self.sentiment);
  result := result + ' ObjScore: '+floattostr(Self.ObjScore);

end;

{ TSentiWordNet }

function TSentiWordNet.Analyze(s: string): TWordScore;
var
  sl: TStringlist;
  t: ni;
begin
  s := stringreplace(s, '/', ' ', [rfReplaceAll]);
  sl := TStringlist.create;
  try
    result.init;
    ParseString(s, ' ', sl);
    for t:= 0 to sl.Count-1 do begin
      result := result + AnalyzeWord(sl[t]);
    end;
  finally
    sl.free;
  end;

end;

function TSentiWordNet.AnalyzeURL(sURL: string; iLevels: ni = -1): TWordScore;
var
  spi: TSpider;
  c: Tcmd_HTTPs;
  ca: array of Tcmd_HTTPS;
  shtml: string;
  t: ni;
  s: string;
begin
  inherited;
  result.init;

  if iLevels > 0 then begin
    spi := TSpider.create;
    try
  //  spi.URL := 'http://ip1.digitaltundra.com:89/video_library.ms?folder=\\192.168.33.3\b$\media\_tv\Rick%20and%20Morty\Rick%20and%20Morty%20Season%202';
    spi.URL := sURL;
  //  spi.URL := 'http://www.google.com';
    spi.MaxLevel := iLevels;
    spi.Start;
    spi.WaitFor;

    result.init;
    setlength(ca, spi.URLCount);
    for t:= 0 to high(ca) do begin
      c := Tcmd_HTTPS.create;
      c.url := spi.URLs[t];
      c.Start;
      ca[t] := c;
    end;

    for t:= 0 to high(ca) do begin
      c := ca[t];
      c.waitFor;
      result := result + self.Analyze(XMLStringToPlainText(c.Results.Body));
    end;
    finally
      spi.free;
    end;
  end;

  QuickHTTPSGet(sURL, s);
  s := XMLStringToPlainText(s);
  result := result + self.Analyze(s);



end;

function TSentiWordNet.AnalyzeWebSearch(sTerms: string): TWordScore;
begin

  result := AnalyzeURL('https://www.google.com/search?q='+encodewebstring(sTerms));
//  result := AnalyzeURL('https://www.metacrawler.com/search/news?q='+encodewebstring(sTerms));
end;

function TSentiWordNet.AnalyzeWord(s: string): TWordScore;
var
  i: ni;
begin
  i := IndexOfword(s);
  result.init;
  if i < 0 then
    exit;
  result := a[i];

end;

constructor TSentiWordNet.Create;
begin
  inherited;
  Load;
end;

function TSentiWordNet.IndexOfword(s: string): ni;
var
  t: ni;
begin
  result := -1;
  if length(s) = 0 then
    exit;
  if s[STRZ] = '''' then
    exit;
  if s[high(s)] = '''' then
    exit;
  if s[STRZ] = '"' then
    exit;
  if s[high(s)] = '"' then
    exit;


  s := lowercase(s);
  s := stringreplace(s, '!', '', [rfReplaceAll]);
  s := stringreplace(s, '.', '', [rfReplaceAll]);
  s := stringreplace(s, ':', '', [rfReplaceAll]);
  s := stringreplace(s, '=', '', [rfReplaceAll]);
  s := stringreplace(s, ',', '', [rfReplaceAll]);
  s := stringreplace(s, '/', '', [rfReplaceAll]);


  for t:= 0 to high(a) do begin
    if a[t].term = s then
      exit(t);
  end;
  exit(-1);

end;

procedure TSentiWordNet.Load;
var
  ss: TStringlist;
  t,u: ni;
  sl,sl2: TStringlist;
  sLine: string;
  sterms: string;
  iWordCount: ni;
  iwordIndex: ni;
  sfile: string;
  s1,s2: string;
begin
  iWordCount := 0;
  ss := TStringList.create;
  try
    sLine := LoadFileAsString(FindMedia('SentiWordNet_3.0.0_20130122.txt'));
    //sLine := StringReplace(sLine, #13, #10, [rfReplaceAll]);
    ss.Text := sLine;
    sl2 := tstringlist.create;
    try

      sl := TStringlist.create;
      try
        for t:= 0 to ss.Count-1 do begin
          sLine := ss[t];
          if length(sLine) = 0 then
            continue;
          if sLine[STRZ] = '#' then
            continue;
          if sLine[STRZ] = #9 then
            continue;
          ParseString(sLine, #9, sl);
          sTerms := sl[4];
          ParseString(sTerms, ' ', sl2);
          inc(iWordcount, sl2.count);

        end;

        setlength(a, iWordCount);
        iWordIndex := 0;
        for t:= 0 to ss.count-1 do begin
          sLine := ss[t];
//          Debug.Log(sLine);
          if length(sLine) = 0 then
            continue;
          if sLine[STRZ] = '#' then
            continue;
          if sLine[STRZ] = #9 then
            continue;
          ParseString(sLine, #9, sl);
          sTerms := sl[4];
          ParseString(sTerms, ' ', sl2);

          for u := 0 to sl2.count-1 do begin
            splitstring(sl2[u], '#', s1,s2);
            a[iWordIndex].term := s1;
            a[iwordIndex].posscore := strtofloat(sl[2]);
            a[iwordIndex].negscore := strtofloat(sl[3]);
            inc(iWordIndex);
          end;
        end;
      finally
        sl.free;
      end;
    finally
      sl2.free;
    end;

  finally
    ss.free;
  end;

end;

end.
