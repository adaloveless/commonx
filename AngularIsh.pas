unit AngularIsh;

interface

uses
  stringx, sysutils, debug;

type
  EValueExpected = class(Exception);

function ReplaceToken(var sXML: string; const sToken, sVal: string; bAll: boolean = true; bAllowBlanks: boolean = false): boolean;
function CutTokens(var sXML: string; const sToken: string; bCut: boolean): boolean;



implementation

function CutTokens(var sXML: string; const sToken: string; bCut: boolean): boolean;
var
  p1,p2: nativeint;
  s1,s2: string;
  t1,t2: string;
begin
  result := false;
  t1 := '[[[+'+sToken+']]]';
  t2 := '[[[-'+sToken+']]]';
  p1 := zpos(t1, sXML);
  p2 := zpos(t2, sXML);
  if bCut then begin
    if p2 > p1 then begin
      if (p2 >=0) and (p1 >=0)then begin
        s1 := ZCopy(sXML, 0, p1);
        s2 := ZCopy(sXML, p2+length(t2), length(sXML));
      end else begin
        raise Exception.create('Found one but not the other +- '+sToken);
      end;
//      Debug.Consolelog(sXML);
      sXML := s1+s2;
//      Debug.Consolelog(sXML);
      result := true;
    end;
  end
  else begin
    sXML := StringReplace(sXML, t1, '', [rfReplaceAll]);
    sXML := StringReplace(sXML, t2, '', [rfReplaceAll]);
  end;


end;

function ReplaceToken(var sXML: string; const sToken, sVal: string; bAll: boolean = true; bAllowBlanks: boolean = false): boolean;
var
  sToken2: string;
  sOld: string;
begin
  if not CutTokens(sXML, sToken, sVAl='') then begin
    if not bAllowBlanks then
      if sVal = '' then
        raise EValueExpected.Create('Blank value found. Expecting a value for '+sToken+'.  The value is likely not set in the calling class.');
  end;

  sToken2 := '[[['+sToken+']]]';

  sOld := sXML;
  if bAll then begin
    sXML := StringReplace(sXML, sToken2, sVal, [rfReplaceAll, rfIgnoreCase]);
    result := true;
  end else begin
    sXML := StringReplace(sXML, sToken2, sVal, [rfIgnoreCase]);
    result := sOLD <> sXML;
  end;





end;

end.
