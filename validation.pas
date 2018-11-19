unit Validation;

interface
uses
{$I stringx.inc}
  sysutils, classes, stringx, typex;

function RestricttoHex(s: string): string;

function ReplaceAbbreviation(var s: string; sAbb: string; sCorrection: string): boolean;
function ContainsLetters(s: string): boolean;
function IsCardinal(s: string): boolean;
function IsInteger(const s: string): boolean;
function ContainsAbbreviation(s: string; sAbb: string; var iPos: integer): boolean;
function FixCockpitErrors(s: string): string;

function CountSpaces(s: string; iUntilPos: integer=0): integer;


implementation


function ReplaceAbbreviation(var s: string; sAbb: string; sCorrection: string): boolean;
var
  iPos: integer;
begin

  result := false;
  while ContainsAbbreviation(s, sAbb, iPos) do begin
    s := zcopy(s, 1, iPos-1)+sCorrection+copy(s, iPos+length(sAbb));
    result := true;
  end;
end;

function ContainsAbbreviation(s: string; sAbb: string; var iPos: integer): boolean;
begin
  s := lowercase(s);
  sAbb := lowercase(sAbb);

  iPos := (zpos(' '+sAbb+' ', s));

  if iPos < 0 then begin
    if zcopy(s, length(s)-(length(' '+sAbb)-1), length(' '+sAbb)) = ' '+sAbb then
      iPos := length(s)-(length(sAbb)-1);

    if zcopy(s, 1, length(sAbb+' ')) = sAbb+' ' then
      iPos := 1;

  end;

  result := iPos > 0;

end;

function ContainsLetters(s: string): boolean;
var
  t: cardinal;
begin
  result := false;
  for t:= STRZ to (length(s)-1)+strz do begin
    if sysutils.CharInSet(s[t], ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'])
    then
    begin
    //if s[t] in ['a'..'z', 'A'..'Z'] then begin
      result := true;
      exit;
    end;
  end;
end;


function IsCardinal(s: string): boolean;
var
  t: cardinal;
begin
  if s = '' then begin
    result := false;
    exit;
  end;

  result := true;
  for t:= 1 to length(s) do begin

    if not CharInSet(s[t], ['0'..'9']) then begin
      result := false;
      exit;
    end;
  end;
end;

function IsInteger(const s: string): boolean;
var
  t: nativeint;
begin
  if length(s) = 0 then begin
    result := false;
    exit;
  end;

  result := true;
  for t:= 1 to length(s) do begin
    if not CharInSet(s[t], ['-','0'..'9']) then begin
      result := false;
      exit;
    end;
  end;

end;

function FixCockpitErrors(s: string): string;
begin

  ReplaceAbbreviation(s, 'St', 'St.');
  ReplaceAbbreviation(s, 'ln', 'Ln.');
  ReplaceAbbreviation(s, 'Rd', 'Rd.');
  ReplaceAbbreviation(s, 'apt', 'Apt.');
  ReplaceAbbreviation(s, 'mr', 'Mr.');
  ReplaceAbbreviation(s, 'ms', 'Ms.');
  ReplaceAbbreviation(s, 'mrs', 'Mrs.');

  result := s;
end;

function CountSpaces(s: string; iUntilPos: integer=0): integer;
var
  t: integer;
begin
  result := 0;
  if iUntilPos = 0 then
    iUntilPos := length(s)+1;
  for t:=STRZ to (length(s)-1)+STRZ do begin
    if t >= iuntilPos then
      break;

    if s[t]=' ' then
      inc(result);
  end;



end;

function RestricttoHex(s: string): string;
var
  t: nativeint;
begin
  s := uppercase(s);
  for t:= 1 to length(s) do begin
    if charinset(s[t], ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']) then
      result := result + s[t];
  end;
end;

end.
