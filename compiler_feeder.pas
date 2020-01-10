unit compiler_feeder;

interface
{$INLINE AUTO}
uses
  sysutils, stringx, classes;

type
  TCompilerFeeder = class
  private
    FLastOperator: ansistring;
    function GEtBOF: boolean;
    function GEtOpScore(sOP: ansistring): integer;

  protected
    FFeed: ansistring;
    FPosition: integer;
    FResult: ansistring;
    FQuote: ansistring;
    FOPPres: TStringlist;
    symbols: set of AnsiChar;
    identifiers: set of AnsiChar;
    whitespaces: set of AnsiChar;
    operators: set of AnsiChar;
    operatorwords: array of ansistring;//NOTE! LOWERCASE ONLY


    procedure SetFeed(const Value: ansistring);
    function GEtEOF: boolean;
  public
    constructor create;reintroduce;virtual;
    destructor destroy;override;
    property Feed: ansistring read FFeed write SetFeed;
    property Position: integer read FPosition write FPosition;
    procedure Move(iinc: integer);
    function GetSurrounding(iPlusOrMinusChars: integer): ansistring;
    function GetSurroundingWord: ansistring;
    property EOF: boolean read GEtEOF;
    property BOF: boolean read GEtBOF;
    property Result: ansistring read FResult;
    procedure AddToResult(s: ansistring);
    function GetCurrentWord(bMove: boolean = true; bSearchToBeginning: boolean=false): ansistring;
    function GetSpaceRight: ansistring;
    function GetOperatorsToRight(bTrim: boolean = true; bIgnoreSymbols: boolean = false): ansistring;
    function MoveThroughWhitespace: boolean;
    function MoveThroughSymbols: boolean;
    procedure MoveThroughWord;
    procedure MoveThroughOperator;
    function GetRestofFeed: ansistring;
    function GetCurrentChar: AnsiChar;
    property RestOfFeed: ansistring read GetRestOfFeed;
    function IsStringConstant(sWord: ansistring): boolean;virtual;
    function IsNumericConstant(sWord: ansistring): boolean;virtual;
    function IsOperatorWord(sWord: ansistring): boolean;
    procedure DefineOperatorWord(sWord: ansistring);
    procedure BackSpace(iCount: integer = 1);
    procedure DefineOperatorPrecidence(sOP: ansistring; score: integer);
    property OpScore[sOP: ansistring]: integer read GEtOpScore;
    procedure ClearOpPres;
    property LastOperator: ansistring read FLastOperator write FLastOperator;
    function StartsWithWord(sMaster, sWord: ansistring): boolean;
    function IsFunctionStructure(sMaster, sWord: ansistring): boolean;

  end;


implementation

{ TCompilerFeeder }

procedure TCompilerFeeder.AddToResult(s: ansistring);
begin
  FResult := FResult + s;
end;

procedure TCompilerFeeder.BackSpace(iCount: integer = 1);
begin
  move(0-iCount);
  if FPosition < 1 then FPosition := 1; 
  Fresult := copy(FResult, 1, length(Fresult)-iCount);
end;

procedure TCompilerFeeder.ClearOpPres;
begin
  FOpPres.clear;
end;

constructor TCompilerFeeder.create;
begin
  inherited;
  FOPPres := TStringlist.create;
  FQuote := '';
  symbols := ['+','-','/','\','<','>','(',')','*','%','=',' ',',',#13, #10];
  operators := ['+','-','/','\','<','>','*','%','=','!'];
  identifiers := ['a'..'z','A'..'Z','0'..'9','_','.'];
  whitespaces := [' ',#13, #10, #9];
  position := 1;

  ClearOpPres;
  self.DefineOperatorPrecidence('*',   200);
  self.DefineOperatorPrecidence('/',   200);
  self.DefineOperatorPrecidence('+',   100);
  self.DefineOperatorPrecidence('-',   100);
  self.DefineOperatorPrecidence('=',    75);
  self.DefineOperatorPrecidence('<',    75);
  self.DefineOperatorPrecidence('>',    75);
  self.DefineOperatorPrecidence('>=',    75);
  self.DefineOperatorPrecidence('=>',    75);
  self.DefineOperatorPrecidence('<>',    75);
  self.DefineOperatorPrecidence('!=',    75);
  self.DefineOperatorPrecidence('not',  60);
  self.DefineOperatorPrecidence('and ', 50);
  self.DefineOperatorPrecidence('xor ', 50);
  self.DefineOperatorPrecidence('or',   40);



end;

procedure TCompilerFeeder.DefineOperatorPrecidence(sOP: ansistring; score: integer);
begin
  FOpPres.addObject(lowercase(sOP), pointer(score));

end;

procedure TCompilerFeeder.DefineOperatorWord(sWord: ansistring);
var
  l: integer;
begin
  l := length(operatorwords);
  setlength(operatorwords, l+1);
  operatorwords[l] := lowercase(sWord);

end;

destructor TCompilerFeeder.destroy;
begin
  FOpPres.free;
  inherited;
end;

function TCompilerFeeder.GEtBOF: boolean;
begin
  result := position = 1;
end;

function TCompilerFeeder.GetCurrentChar: AnsiChar;
var
  s: ansistring;
begin
  s := copy(FFeed, FPosition,1);
  if length(s) = 0 then
    result := #0
  else
    result := s[1];


end;

function TCompilerFeeder.GetCurrentWord(bMove: boolean = true; bSearchToBeginning: boolean=false): ansistring;
var
  t: integer;
  iCount: integer;
  bQuotes: boolean;
  ii,iii: integer;
begin
  iii := 0;
  if bSearchToBeginning then begin
    iii := position;
    while (GetCurrentChar in identifiers) and (not bof) do
      move(-1); //TODO 5: Support " words here on backward search? -- not all that important only used when looking for operators

    move(1);

  end else begin
    MoveThroughWhitespace();
  end;

  iCount := 0;
  t := Position;

  //if starts with quote
  bQuotes := GetSurrounding(0) = '"';

  if not bQuotes then begin
    while (FFeed[t] <> ' ') and (((t<=length(FFeed)) and (not (FFeed[t] in (symbols)))) or ((t<length(FFeed)) and ((position  <> t) and (FFeed[Position]='-') and ((FFeed[t] in ['0'..'9','.']))) or ((position  = t) and (FFeed[Position]='-') and ((FFeed[t+1] in ['0'..'9','.']))))) do begin
      inc(t);
      inc(iCount);
    end;
  end else begin
    inc(t);
//    move(1);
    while (t<=length(FFeed)) and (not (FFeed[t] ='"')) do begin
      inc(t);
      inc(iCount);
    end;
    inc(iCount,2);
  end;

  if iCount = 0 then
    result := ''
  else
    result := GetSurrounding(iCount-1);

  if bMove then
    move(iCount);
//  if bQuotes then
//    move(1);

  if bSearchToBeginning then
    position := iii;

end;

function TCompilerFeeder.GEtEOF: boolean;
begin
  result := (Position > length(feed));
end;

function TCompilerFeeder.GetRestofFeed: ansistring;
begin
  result := copy(self.FFeed, self.Position, length(FFeed));
end;

function TCompilerFeeder.GetSpaceRight: ansistring;
var
  sWord: ansistring;
  sSpace: ansistring;
  t: integer;
begin
  t := self.Position;
  if length(Feed) = 0 then begin
    result := '';
    EXIT;
  END;
  //move through word
  while (t < length(Feed)) and (not (self.Feed[t] in symbols)) do begin
    inc(t);
  end;

  //move through whitespace
  while (t < length(Feed)) and ((self.Feed[t] in symbols)) do begin
    sSpace := sSpace + feed[t];
  end;




end;

function TCompilerFeeder.GetSurrounding(iPLusOrMinusChars: integer): ansistring;
begin
  if iPLusOrMinusChars < 0 then begin
    result := Copy(feed, position+iPLusOrMinusChars, (0-iPLusOrMinusChars)+1);
  end else begin
    result := Copy(feed, position, iPLusOrMinusChars+1);
  end;

end;

function TCompilerFeeder.GetSurroundingWord: ansistring;
begin
  raise exception.create('not implemmented');
end;

function TCompilerFeeder.GetOperatorsToRight(bTrim: boolean; bIgnoreSymbols: boolean): ansistring;
var
  sWord: ansistring;
  sSpace: ansistring;
  t,origpos: integer;
begin
  result := '';
  origpos := self.Position;
  try
    if length(Feed) = 0 then begin
      result := '';
      EXIT;
    END;
    sWord := GetCurrentWord(false);
    if IsOperatorWord(sWord) then begin
      result := sWord;
      exit;
    end;

    t := position;
    //move through word
    if not bIgnoreSymbols then begin
      while (t < length(Feed)) and (not ((self.Feed[t] in (operators+symbols)) or IsOperatorWord(GEtCurrentWord(false)))) do begin

        move(1);
        inc(t);
      end;
    end else begin
      while (t < length(Feed)) and (not ((self.Feed[t] in (operators+symbols)) or IsOperatorWord(GEtCurrentWord(false)))) do begin

        move(1);
        inc(t);
      end;
    end;

    //move through whitespace
    if IsOperatorWord(GetCurrentWord(false)) then begin
      sSpace := GetCurrentWord(false);
    end else
    while (t < length(Feed)) and ((self.Feed[t] in operators)) do begin
      if (length(sspace) > 0) and (feed[t] = '-') then begin //TODO 5: This is a cludge that makes compiler feeded not compatible with C++ =-
        break;
      end else begin

        sSpace := sSpace + feed[t];
        inc(t);
      end;
    end;

    result := sspace;

    if bTrim then
      result := Trimstr(result);

    if result = '' then begin
      t := position;
      try
        MoveThroughWord;
        MoveThroughWhitespace;
        if IsOperatorWord(GetCurrentWord) then begin
          result := lowercase(GetCurrentWord);
        end;
      finally
        position := t;
      end;

    end;
  finally
    position := origpos;
  end;




end;

function TCompilerFeeder.GEtOpScore(sOP: ansistring): integer;
var
  i: integer;
begin
  SOP := lowercase(sop);
  i := FOPPres.indexof(sOP);

  if i < 0 then
    result := 0
  else
    result := integer(FOpPres.Objects[i]);
end;

function TCompilerFeeder.IsFunctionStructure(sMaster, sWord: ansistring): boolean;
var
  c: AnsiChar;
  op: integer;
begin
  op := position;
  try
    result := false;
    if StartsWith(sMaster, sWord) then begin

      MoveThroughWord;
      MoveThroughWhiteSpace;
      c := GetCurrentChar;

      result := (c in ['(']);
    end;
  finally
    position := op;
  end;
end;

function TCompilerFeeder.IsNumericConstant(sWord: ansistring): boolean;
begin
  if sWord = '' then
    result := false
  else
    result := sWord[1] in ['0'..'9','-'];

end;

function TCompilerFeeder.IsOperatorWord(sWord: ansistring): boolean;
var
  t: integer;
begin
  result := false;
  for t:= low(operatorwords) to high(operatorwords) do begin
    if lowercase(sWord) = operatorwords[t] then begin
      result := true;
      break;
    end;
  end;

end;

function TCompilerFeeder.IsStringConstant(sWord: ansistring): boolean;
begin
  if sWord = '' then
    result := false
  else
    result := sWord[1] in ['"',''''];

end;

procedure TCompilerFeeder.Move(iinc: integer);
begin
  inc(FPosition, iinc);

end;

procedure TCompilerFeeder.MoveThroughOperator;
var
  bFound: boolean;
begin
  bFound := false;
  if IsOperatorWord(GetCurrentWord(false)) then
    MoveThroughWord
  else begin
    while self.GetCurrentChar in  operators do begin
      if (GetCurrentChar = '-') and bFound then
        break
      else
      if GetCurrentChar = '=' then
        bFound := true;

      move(1);
    end;
  end;
end;

function TCompilerFeeder.MoveThroughSymbols: boolean;
begin
  result := false;
  while (not eof) and (GetCurrentChar in symbols) do begin
    move(1);
    result := true;
  end;
end;

function TCompilerFeeder.MoveThroughWhitespace: boolean;
//returns TRUE if cursor was moved
begin
  result := false;
  while (not eof) and (GetCurrentChar in [' ',#13, #10, #9]) do begin
    move(1);
    result := true;
  end;
end;

procedure TCompilerFeeder.MoveThroughWord;
var
  s: ansistring;
begin
  s := GetCurrentWord(false);
  move(length(s));
end;




procedure TCompilerFeeder.SetFeed(const Value: ansistring);
begin
  FFeed := Value;
  FPosition := 1;
end;

function TCompilerFeeder.StartsWithWord(sMaster, sWord: ansistring): boolean;
var
  s: ansistring;
begin
  result := false;
  if StartsWith(sMaster, sWord) then begin
    s := copy(sMaster, length(sWord)+1, 1);
    if length(s) > 0 then begin
      result := (s[1] in WhiteSpaces) or (s[1] in symbols);

    end else
      result := true;

  end;


end;

end.
