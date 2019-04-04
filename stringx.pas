unit stringx;
{$I 'DelphiDefs.inc'}
{$MESSAGE '*******************COMPILING stringutilities.pas'}

interface


uses
  Classes, variants, math, commonconstants, systemx, sort, numbers, typex, betterobject;

const
  CRLF = #13#10;
type
  TSplitSet = array of string;



  TDynStringArray = array of string;

  TPosDetails = record
    search: string;
    position: ni;
    leftof: string;
    rightof_including: string;
    rightof_excluding: string;
  end;

  TDumbRamString = record
    size: integer;
  end;

  TStringListFixer = class helper for TStringlist
  private
    function GetTrustyText: string;
    procedure SetTrustyText(const Value: string);
  public
    property TrustyText: string read GetTrustyText write SetTrustyText;
  end;

  TStringsFixer = class helper for TStrings
  private
    function GetTrustyText: string;
    procedure SetTrustyText(const Value: string);
  public
    property TrustyText: string read GetTrustyText write SetTrustyText;
  end;

  PDumbRamString = ^TDumbRamString;
function paddr(o: TObject): string;inline;
function its(i: int64): string; inline;overload;
function its(i: integer): string; inline;overload;

function OFirstPos(s: string; searchfor: array of string; bIgnoreCase: boolean = false; iOStartAt: ni = 1): TPosDetails;
function ZFirstPos(s: string; searchfor: array of string; bIgnoreCase: boolean = false; iZStartAt: ni = 0): TPosDetails;

procedure RemovePrefixFromStringList(sPrefix: string; sl: TStrings);
function StringListFromNullTerminatedStrings(p: PByte; sz: nativeint): TStringlist;
function StringsFromMemory(p: PByte; sz: nativeint): string;
function Commaize(i: int64): string;
function IsInteger(s: string): boolean;
function IsHex(s: string): boolean;
function StrToBool(s: string): boolean;
function BoolToStrEx(b: boolean; sTrue: string ='TRUE'; sFalse:string = 'FALSE') : string;
function BoolToStr(b: boolean; sTrue: string ='TRUE'; sFalse:string = 'FALSE') : string;
function BoolToString(b: boolean; sTrue: string ='TRUE'; sFalse:string = 'FALSE') : string;
function GetFieldValue(slFields: TStringlist; slRecord: TStringlist; sFieldName: string): string;
function  GetFieldIndex(slFields: TStringlist; sFieldName: string): nativeint;
function IsolateDelimited(sLine: string; rangestart, rangeEnd: nativeint; sDelimiter: string = ','): string;
function RemoveDuplicatesFromStringList(sl: TStringlist): boolean;
function BtxDateToDAteTime(sBTX: string): TDateTime;
function HTMLToPlainText(s: string): string;
function SplitStringIntoStringList(s: string; cDelimiter: char = ' '; bRespectQuote: char = #0): TStringList;
function ExtractLinksFromPlainText(s: string): string;
function RemoveDoubles(s: string; c: char): string;
function StrSize(s: string): integer;
function HextoString(sHexString: string): string;
function StringTohex(s: string): string;
function DelimitIfNotEmpty(sInput: string; sDelimiter: string): string;
function StringRepeat(s: string; iCount: ni): string;
procedure AppendTextToFile(sFile: string; sText: string);
function IPToBytes(sIP: string): TDynByteArray;
function CountChar(s: string; c: char): ni;
function StrIsBinary(s: string): boolean;



function Unquote(s: string): string;
function StartsWith(sMaster: string; sStartsWith: string): boolean;
function StartsWithThenSkip(var sMaster: string; sStartsWith: string): boolean;
function Stringlist_ValuesBlank(sl: TStringlist): boolean;

function PairsToParams(sPairs: string): string;
//function FloatPrecision(r: real; iDigits: integer): string;
function FloatPrecision(r: nativefloat; iDecPLaces: integer; bCollapseIntegers: boolean = false): string;
procedure TrimStringList(sl: tStringlist);

function IntToRank(i: integer): string;
function MatchCase(sSource, sTarget: string): string;
function PosFirst(sSource: string; search1, search2: string): integer;overload;
function PosFirst(sSource: string; search1, search2, search3: string): integer;overload;
function ZSubCopy(sSource: string; iPOs: nativeint; iLength: nativeint): string;
function zpos(sSubString: string; sString: string;iSTartAt:ni=0): nativeint;overload;
function zpos_ignorecase(sSubString: string; sString: string;iSTartAt:ni=0): nativeint;
function zpos(sSubString: string; sString: string;bIgnoreCase: boolean; iSTartAt:ni=0): nativeint;overload;
function StrToFloatEx(s: string; nanresult: double = 0): double;

function ZFindSplitMidPOint(sPattern: string; sString: string;bIgnoreCase: boolean): nativeint;

function zsubisatpos(sSub: string; sString: string; idx: nativeint): boolean;
function zsubisatpos_ignorecase(sSub: string; sString: string; idx: nativeint): boolean;
function zlastpos(sSubString: string; sString: string;iSTartAt:ni=0): nativeint;
function trimcharsfromfront(sString: string; char: string): string;
function ocopy(sString: string; iStartOneBased: nativeint; iLength: nativeint): string;
function zcopy(sString: string; iStartZeroBased: nativeint; iLength: nativeint): string;
function zExtractDelimitedString(sSource: string; sDelimiter: string; var iStartAtZeroBased: ni; out sResult: string): boolean;
function MemoryToString(p: pointer; l: ni): string;overload;
function MemoryToString(m: TDynByteArray): string;overload;
function StringToMemory(s: string): TDynByteArray;





//function SplitString(sSource: string; const sSplitter: string; var sLeft, sRight: string; bStartAtRight: boolean = false): boolean; overload;
function SplitString(sSource: string; const sSplitter: string; var sLeft, sRight: string; bStartAtRight: boolean = false;iZStartAt: ni=0): boolean; overload;
//function SplitString(sSource: string; const sSplitter: string; var sLeft, sRight: string; bStartAtRight: boolean = false): boolean; overload;
//function SplitString(sSource: string; sSplitters: array of string; var sLeft, sRight: string; sDelimiterUsed: string; bStartAtRight: boolean = false): boolean; overload;
function SplitStringNoCase(sRealSource, sSplitter: string; var sLeft, sRight: string; bStartAtRight: boolean = false): boolean; overload;


function SplitStringNoCaseEx(sRealSource, sSplitter: string; var sLeft, sRight: string; notinLeft, notInRight: string; bStartAtRight: boolean = false): boolean; overload;
function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: string; var sLeft, sRight: string; notinLeft, notInRight: string; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;
function SplitStringNoCaseEx(sRealSource, sSplitter: string; var sLeft, sRight: string; notin: TSplitSet; bStartAtRight: boolean = false): boolean; overload;
function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: string; var sLeft, sRight: string; notin: TSplitSet; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;



function PcharToString(Pchar: Pchar; length: integer): string;
function ExpandMacAddress(sMac: string): string;
procedure ReplicateStringListArea(sl: TStringList; iStartRow, iEndRow, iRepeatCount: integer; sDelimiters: string = '');
function StripPhone(sPhone: string): string;
function stringProgress(rPercent: real; length: integer; cap1: string = '['; cap2: string = ']'; bar: string = '|'; space: string = ' '): string;
function CountLinesInTextFile(sFile: string): integer;
function stringToDumbRAM(s: string): pointer;
function  DumbRamToString(p: pointer): string;
procedure FreeDumbRamString(var p: PDumbRamString);
function BoolString(bCondition: boolean; sTrue: string=''; sFalse: string = ''): string;
function VarToMYSQLStorage(v: variant): string;
function VArtoJSONStorage(v: variant): string;
function SQLEscape(s: string): string;
procedure SaveStringAsFile(sFile: string; data: string);

function LoadFileAsString(sFile: string; iLimitLength: integer = 0): string;
function LoadStreamAsString(s: TStream): string;
function LoadStringFromFile(sFile: string; iLimitLength: integer = 0): string;
function SortStringCompare(s1,s2: string): integer;
function CleanString(s: string): string;
function CleanInjection(s: string): string;
function UnCleanInjection(s: string): string;
function FriendlySizeName(i: int64): string;



function MakeThreadSafe(s: string): string;inline;
function HexEncode(s: string): string;
function  stringToStringList(s: string): TStringList;
function stringToStringListH(s: string): IHolder<TStringList>;
//procedure SplitString(sSource, sDelimiter: string; var sLeft, sRight: string);
function TrimStr(s: string): string;
procedure CrackDelimitedString(const sLine : string; cDelimiter : char; slList : TStringList);
function ExtractStr(src: string; what: string): string;
function LeftStr(s: string; n: integer): string;
function RightStr(s: string; n: integer): string;
function MidStr(s: string; p: integer; n: integer): string;

function StrExtract(src: string; what: string): string;
function UnParseString(sDelimiter: string; sl: TStringList; bEvenOnly: boolean = false): string;
procedure ParseString(src: string; sDelimiter: string; slList: TStringList);overload;
function ParseString_Quick(src: string; sDelimiter: string):TStringList;overload;
procedure ParseString(src: string; sDelimiter: char; slList: TStringList);overload;
function ParseString(src: string; sDelimiter: char): TStringlist;overload;
function ParseStringNotIn(src: string; sDelimiter: char; NotIn: char): TStringlist;
function ParseStringNotInH(src: string; sDelimiter: char; NotIn: char): IHolder<TStringlist>;
function ParseStringH(src: string; sDelimiter: char): IHolder<TStringlist>;overload;
function ParseString(src: string; sDelimiter: string): TStringlist;overload;
procedure ParseString2(src: string; sDelimiter: string; slList: TStringList);
procedure ParseNumericStringWithRanges(src: string; sDelimiter: string; slList: TStringList);
procedure ParseNumericRange(src: string; sDelimiter: string; slList: TStringList);
function Quote(s: string): string;overload;
function Quote(sl: TStringlist): string;overload;
function CommaQuote(s: string): string;
function AdjustFolderPath(sPath: string): string;
function AdjustFileName(sName, sExt: string): string;
function CleanupParentsInPath(sPath: string): string;
function StripLineOfChar(src, s: string): string;
function LastPos(sub,s : string):integer;overload;

function SubStringExistsInString(sSrc, sWhat: string; var iPos: integer): boolean;
function EditStringWithNewSubString(iPos: integer; sOld, sNew: string; var sSrc: string): boolean;
function PadString(sSource: string; sPadChar: char; iWidth: integer): string;
function SplitQuote(sOrig: string; sQuoteDelimiter: string; var sLeft, sRight: string; out sInQuote: string): boolean;
function StringIsAt(sMain, sSub: string; iPosToSearch: ni): boolean;
function SplitString(sSource: string; sSplitters: array of string; var sLeft, sRight: string; out sDelimiterUsed: string; NotIn: array of string; bStartAtRight: boolean = false): boolean; overload;
function CropStringLR(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;
function CropStringRL(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;
function CropString(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;
function CopyStringList(sl: TStringList): TStringList;

function PosEx(sSubString, sMain: string; NotIn: array of string): integer;
function LastPosEx(sSubString, sMain: string; NotIn: array of string): integer;


function BestPos(SubStrings: array of string; sMain: string; out sMatch: string; out position: integer; NotIn: array of string; bStartAtRight: boolean = false): boolean;

procedure mergeStringLists(slmaster: tStringList; sInsert: string; iStartRow, iStartCol, iEndrow, iEndcol: integer);

procedure DeleteStringListArea(sl: TStringList; iStartCol, iStartRow, iEndCol, iEndRow: integer; sReplace: string = '');

function DatetoMYSQLDate(dt: TDateTime): string;
function MYSQLDateTimeToDateTime(s: string): TDateTime;
function Abbreviate(s: string): string;
function Scramble(s: string): string;
function Unscramble(s: string): string;
{$IFDEF MSWINDOWS}
function ordEX(c: ansichar): nativeint;overload;
function ordEX(c: widechar): nativeint;overload;
{$ENDIF}
{##############################################################################}
implementation
uses
  helpers.stream, SysUtils, unittest;

type
  TUT_StringX = class(TUnitTest)
  public
    procedure DoExecute;override;
  end;








{------------------------------------------------------------------------------}
function TrimStrOld(s: string): string;  {trim off left and right spaces}
//p: s: The string to trim.
//A custom function to TRIM off extra spaces on both sides of a string.
var
  t: string;
  p, n, c, b, e: integer;
  z: array[0..256] of char;
begin
  p := pos(' ',s);
  if (p > 0) then
    begin
      n := Length(s);
      StrPcopy(z,s);
      b := -1;
      e := -1;
      for c := 0 to n-1 do
        begin
          if (z[c] <> char(32)) then
            begin
              b := c+1;
              break;
            end;
        end;
      for c := n-1 downto 0 do
        begin
          if (z[c] <> char(32)) then
            begin
              e := c+1;
              break;
            end;
        end;
      t := '';
      if (e >= b) and (e >=0) then
        begin
          n := e-b+1;
          t := copy(s,b,n);
        end;
      Result := t;
    end
  else
    Result := s;
end;
//------------------------------------------------------------------------------
function TrimStr(s: string): string;  {trim off left and right spaces}
//p: s: The string to trim.
//A custom function to TRIM off extra spaces on both sides of a string.
begin
  while (length(s)>0) and CharInSet(s[1], [' ',#13, #10, #9]) do begin
    s := copy(s, 2,length(s));
  end;

  while (length(s)>0) and charInSet(s[length(s)], [' ',#13, #10, #9]) do begin
    s := copy(s, 1,length(s)-1);
  end;

  result := s;


end;

{------------------------------------------------------------------------------}
procedure CrackDelimitedString(const sLine : string; cDelimiter : char; slList : TStringList);
//p: sLine: The string you are cracking.
//p: cDelimiter: The delimiter -- single character supported only.
//p: slList: A TStringList that will hold the results.  Be sure to create it before hand.
//Splits sLine into multiple strings at cDelimiter and returns the strings in
//a given TStringList class instance.
var
  sIn  : string;
  nLen : integer;
begin
  if Length(sLine) = 0 then
    exit;

  sIn := sLine;
  while Pos(cDelimiter, sIn) <> 0 do begin
    nLen := Pos(cDelimiter, sIn) - 1;
    slList.Add(Copy(sIn, 1, nLen));
    Delete(sIn, 1, nLen + 1);
  end;

  slList.Add(sIn);
end;
function ExtractStr(src: string; what: string): string;
//a: Author Unknown
//returns string to left of what
var
  p: integer;
begin
  p := pos(what,src);
  if (p > 0) then
    Result := LeftStr(src,p-1)
  else
    Result := src;
end;
{----------------------------------------------------------------------}
function LeftStr(s: string; n: integer): string;
//a: Author Unknown
//Returns the Left n characters in the string.
//var
//  z: array[0..256] of char;
begin
  result := copy(s, 1, n);
//  StrPcopy(z,s);
//  if (n <= Length(s)) then
//    z[n] := char(0);
//  Result := StrPas(z);
end;
{----------------------------------------------------------------------}
function RightStr(s: string; n: integer): string;
begin
  result := copy(s, length(s)-(n-1), n);
end;
//a: Author Unknown
//Returns the right n characters in the string.
//var
//  x, z: array[0..256] of char;
//  c, l, p: integer;
//begin
//  l := Length(s);
//  StrPcopy(z,s);
//  if (n < l) then
//    begin
//      p := l-1;
//      for c := 0 to n-1 do
//        begin
//          x[n-c-1] := z[p];
//          p := p-1;
//        end;
//      x[n] := char(0);
//      Result := StrPas(x);
//    end
//  else
//    begin
//      Result := s;
//    end;
//end;
{----------------------------------------------------------------------}
function MidStr(s: string; p: integer; n: integer): string;
//a: Author Unknown
//p: s: string, the source string
//p: p: The start position
//p: n: The number of characters to read starting at p
//Returns the characters in the middle of the string.
var
  x, z: array[0..256] of char;
  c, l, b: integer;
begin
  l := Length(s);
  StrPcopy(z,s);
  if (p+n < l+1) then
    begin
      b:=0;
      for c := p-1 to p+n-2 do
        begin
          x[b] := z[c];
          b:=b+1;
        end;
      x[b] := char(0);
      Result := StrPas(x);
    end
  else
    begin
      Result := '';
    end;
end;
{------------------------------------------------------------------------------}
function StrExtract(src: string; what: string): string;
//a: Author Unknown
{returns string to right of what}
var
  l, p: integer;
begin
  l := Length(src);
  p := pos(what,src);
  if (p > 0) then
    Result := RightStr(src,l-p-Length(what)+1)
  else
    Result := src;
end;
{------------------------------------------------------------------------------}
function UnParseString(sDelimiter: string; sl: TStringList; bEvenOnly: boolean = false): string;
var
  t,u,tt: nativeint;
  iCount: nativeint;
  sTemp: string;
begin
{x$DEFINE SLOW_UNPARSESTRING}
{$IFDEF SLOW_UNPARSESTRING}
  result := '';
  for t:= 0 to sl.count-1 do begin
    if t< sl.count-1 then begin
      result := result + sl[t]+sDelimiter;
    end else begin
      result := result + sl[t];
    end;
  end;
{$ELSE}
  iCount := 0;
  for t:= 0 to sl.count-1 do begin
    inc(iCount, length(sl[t]));
  end;
  inc(iCount, length(sDelimiter)*(sl.Count-1));


  setlength(result, iCount);
  tt := 1;
  for t := 0 to sl.Count-1 do begin
    if bEvenOnly then if (t and 1) = 1 then continue;
    sTemp := sl[t];
    for u := 1 to length(sTemp) do begin
      result[tt] := sTemp[u];
      inc(tt);
    end;

    if t < (sl.Count-1) then begin
      for u := 1 to length(sDelimiter) do begin
        result[tt] := sDelimiter[u];
        inc(tt);
      end;
    end;
  end;
{$ENDIF}
end;
//------------------------------------------------------------------------------
function  GetFieldIndex(slFields: TStringlist; sFieldName: string): nativeint;
begin
  result := slFields.indexof(sFieldName);
  if result < 0 then
    raise Exception.Create('field '+sfieldName+' not found.');

end;
//------------------------------------------------------------------------------
function GetFieldValue(slFields: TStringlist; slRecord: TStringlist; sFieldName: string): string;
begin
  result := slREcord[GetFieldIndex(slFields, sFieldName)];
end;
//------------------------------------------------------------------------------
function ParseString(src: string; sDelimiter: char): TStringlist;
begin
  result := TStringList.Create;
  ParseString(src, sDelimiter, result);
end;

function ParseStringNotIn(src: string; sDelimiter: char; NotIn: char): TStringlist;
var
  sLeft, sRight: string;
begin
  result := TStringlist.create;
  sRight := src;
  while SplitStringNoCAseEx(sRight, sDelimiter, sLeft, sRight, NotIn, Notin, false) do begin
    result.Add(sLeft);
  end;

  result.Add(sLEft);

end;

function ParseStringNotInH(src: string; sDelimiter: char; NotIn: char): IHolder<TStringlist>;
begin
  result := THolder<TStringList>.create;
  result.o := ParseStringNotIn(src, sDelimiter, NotIn);
end;


function ParseStringH(src: string; sDelimiter: char): IHolder<TStringlist>;overload;
begin
  result := THolder<TStringList>.create;
  result.o := ParseString(src, sDelimiter);
end;
  //------------------------------------------------------------------------------
function ParseString(src: string; sDelimiter: string): TStringlist;
begin
  result := TStringList.Create;
  ParseString(src, sDelimiter, result);
end;
//------------------------------------------------------------------------------
procedure ParseString(src: string; sDelimiter: char; slList: TStringList);
var
  t,l,s,ld,f: nativeint;
  ss: string;
  c: char;
  slTemp: TStrings;
begin
  slTemp := nil;
//  ParseString2(src, sDelimiter, slList);
//  exit;
  slList.clear;

  t := 0;
  l := length(src);
  ld := 1;

  s := 0;
  while t < l do begin
    c := src[t+STRZ];
    if (c=sDelimiter) then begin
      f := t;
      slList.add(zcopy(src, s, f-s));
      if slList.count > 300 then begin
        if slTemp = nil then
          slTemp := TStringlist.create;
        slTemp.AddStrings(slList);
        slList.clear;
      end;
      s := f + 1;
      t := s;
    end else
      inc(t);
  end;


  if (t>s) then begin
    f := t;
    slList.add(zcopy(src, s, f-s));
  end;

  if slTemp <> nil then begin
    slTemp.AddStrings(slList);
    slList.assign(slTemp);
    slTEmp.free;
  end;

end;
//------------------------------------------------------------------------------
procedure ParseString(src: string; sDelimiter: string; slList: TStringList);
var
  t,l,s,ld,f: nativeint;
  sLeft, sRight: string;
begin
//  ParseString2(src, sDelimiter, slList);
//  exit;
  slList.clear;
  if length(sDelimiter) = 1 then begin
    ParseString(src, sDelimiter[STRZ], slList);
    exit;
  end;
                             sRight := src;
  while SplitString(sRight, sDelimiter, sLeft, sRight) do begin
    slList.add(sLeft);

  end;
  slList.add(sLeft);
end;
procedure ParseString2(src: string; sDelimiter: string; slList: TStringList);
//a: Author Unknown
//This function is a duplicate of CrackDelimitedString so it seems.
//  This function takes a string and a delimiter characer (e.g. a comma or semi-
//   colon) and splits the line at each delimiter, putting each piece (not
//   including the delimiter itself) as an entry in the TStringList, simply added
//   in the order encountered, processing from left to right.
var
  sTemp: string;
  sNew: string;
  sOld: string;
begin
  slList.Clear;
  if Length(src) = 0 then
    exit;
  sNew := src;
  while (Pos(sDelimiter, sNew) <> 0) do begin
    sTemp := ExtractStr(sNew, sDelimiter);
    sTemp := Trim(sTemp);
    if sTemp <> '' then
      slList.Add(sTemp);
    sOld := sNew;
    sNew := StrExtract(sOld, sDelimiter);
  end;
  if sNew <> '' then slList.Add(Trim(sNew));{add the last delimited value}
end;
{------------------------------------------------------------------------------}
procedure ParseNumericStringWithRanges(src: string; sDelimiter: string;
                                             slList: TStringList);
//a: Author Unknown
//  This function takes a string and a delimiter characer (e.g. a comma or semi-
//  colon) and splits the line at each delimiter, putting each piece (not
//   including the delimiter itself) as an entry in the TStringList, simply added
//   in the order encountered, processing from left to right.  Within each
//   delimited entry, the value may be a range that is further seperated by a
//   dash.  Within this range entry, we determine the lower and upper values
//   and starting with the lower value, we add every integer until we get to
//   the upper value.}
var
  sTemp: string;
  sNew: string;
  sOld: string;
begin
  if Length(src) = 0 then
    exit;
  sNew := src;
  while (Pos(sDelimiter, sNew) <> 0) do begin
    sTemp := ExtractStr(sNew, sDelimiter);
    if (Pos('-', sTemp) = 0) then begin
      sTemp := Trim(sTemp);
      if sTemp <> '' then
        slList.Add(sTemp);
    end else
      ParseNumericRange(sTemp, '-', slList);
    sOld := sNew;
    sNew := StrExtract(sOld, sDelimiter);
  end;
  if sNew <> '' then begin {add the last delimited value}
    if (Pos('-', sNew) = 0) then begin
      sTemp := Trim(sNew);
      if sTemp <> '' then
        slList.Add(sTemp);
    end else
      ParseNumericRange(sNew, '-', slList);
  end;
end;
{------------------------------------------------------------------------------}
procedure ParseNumericRange(src: string; sDelimiter: string;
                                             slList: TStringList);
//a: Unknown
//  This function takes a numeric range in the form of a string where the
//   numeric values are separated by a dash (example 203-211) and loads the
//   starting value, the ending value, and all values inbetween, into the
//   stringlist
var
  sStart: string;
  sEnd: string;
  iStart: LongInt;
  iEnd: LongInt;
  x: LongInt;
begin
  try
    sStart := Trim(ExtractStr(src, '-'));
    sEnd := Trim(StrExtract(src, '-'));
    iStart := StrToInt(sStart);
    iEnd := StrToInt(sEnd);
    for x := iStart to iEnd do
      slList.Add(IntToStr(x));
  finally
  end;
end;
{------------------------------------------------------------------------------}
function AdjustFolderPath(sPath: string): string;
//a: Author Unknown

var
  sTemp: string;
begin
  sTemp := Trim(sPath);
  Result := sTemp;
  if sTemp[length(sTemp)] <> '\' then
    Result := sTemp + '\';
end;
{------------------------------------------------------------------------------}
function AdjustFileName(sName, sExt: string): string;
//a: Author Unknown
var
  sTemp: string;
begin
  sTemp := Trim(sName);
  Result := sTemp;
  if Pos('.', sTemp) = 0 then
    Result := sTemp + '.' + sExt
  else
    Result := ExtractStr(sTemp, '.') + '.' + sExt;
end;
{------------------------------------------------------------------------------}
function StripLineOfChar(src, s: string): string;
//a: Author Unknown
//This function strips all occurances of 's' from the src string.  For this
// function to work, 's' must be a single character
var
  x: integer;
  a, b: string;
begin
  Result := '';
  a := '';
  if Length(src) = 0 then
    exit;
  for x := 1 to Length(src)do begin
    b := src[x];
    if b <> s then
      a := a + b;
  end;
  Result := a;
end;
{------------------------------------------------------------------------------}
(*procedure SplitString(sSource, sDelimiter: string; var sLeft, sRight: string);
var
  iFirstInstance: integer;
begin
  iFirstInstance := pos(sDelimiter, sSource);

  if iFirstInstance > 0 then begin
    sLeft := copy(sSource, 1, iFirstInstance-1);
    sRight := copy(sSource, iFirstInstance+length(sDelimiter), length(sSource) - iFirstInstance);
  end else begin
    sLeft := sSource;
    sRight := '';
  end;
end;*)
//------------------------------------------------------------------------------


function LastPos(sub,s : string):integer;
//a: Jason nelson
//Retrurns the last position of sub in s.
var
  p: integer;
  t: integer;
begin
  p:=0;

  for t:=((length(s))-length(sub))+1 downto 0 do begin
    if copy(s, t, length(sub))=sub then begin
      p:=t;
      break;
    end;
  end;

 result:=p;
end;
//------------------------------------------------------------------------------
function SubStringExistsInString(sSrc, sWhat: string; var iPos: integer): boolean;
//a: Author Unknown
// This function searches for a string in another string.  It returns TRUE
//   if sWhat is found in sSrc.  iPos is the position of the first character
//   on sWhat in sSrc.

//IMPORTANT: I have NOT tested the condition of sWhat being at the
//   very end of sSrc.
var
  x: integer;
  iWhatLen: integer;
  iSrcLen: integer;
  sTemp: string;
begin
  Result := FALSE;
  iPos := 0;
  iWhatLen := Length(sWhat);
  iSrcLen := Length(sSrc);
  if iWhatLen > iSrcLen then
    Exit;
  for x := 0 to (iSrcLen - iWhatLen) do begin
    sTemp := Copy(sSrc, x, iWhatLen);
    if sTemp = sWhat then begin
      iPos := x;
      Result := TRUE;
      exit;
    end;
  end;
end;
//------------------------------------------------------------------------------
function EditStringWithNewSubString(iPos: integer; sOld, sNew: string;
                                    var sSrc: string): boolean;
//a: Author Unknown
// This routine replaces one string with another string inside of a
//   larger string.  iPos is the position of the first character of
//   the string that is to be removed.  sOld tells us how many characters
//   are to be removed.
var
  x: integer;
  sTemp: string;
begin
  Result := TRUE;

  sTemp := '';
  try
    sTemp := Copy(sSrc, 1, iPos - 1) + sNew;
    x := (iPos - 1) + Length(sOld);
    sTemp := sTemp + Copy(sSrc, x + 1, (Length(sSrc) - x) + 1);
  finally
    sSrc := sTemp;
  end;
end;
//------------------------------------------------------------------------------
function PadString(sSource: string; sPadChar: char; iWidth: integer): string;
//a: Author Unknown
//p: sSource: The original string to be padded.
//p: sPadchar: The character to use for padding.
//p: iWidth: The number of characters to pad.
//Pads characters on the front of a string.
begin
  result := sSource;
  while length(result) < iWidth do
    result := sPadChar+result;
end;
//------------------------------------------------------------------------------
function stringToStringListH(s: string): IHolder<TStringList>;
//p: s: The string you want to convert to a TStringList;
//Takes a string, creates a TStringList class, and returns the stringlist class
//as the result with the string set in TStringList.text.  It is useful for
//quickly breaking apart a large document into a bunch of lines.
//Note that you should manaually free the TStringList class yourself.
var
  s1,s2: string;
begin
  result := THolder<TStringList>.create;
  result.o := TStringList.create;
{$IFDEF FMX}
  result.o.TrustyText := s;
{$ELSE}
  result.o.text := s;
{$ENDIF}

end;
//------------------------------------------------------------------------------
function stringToStringList(s: string): TStringList;
//p: s: The string you want to convert to a TStringList;
//Takes a string, creates a TStringList class, and returns the stringlist class
//as the result with the string set in TStringList.text.  It is useful for
//quickly breaking apart a large document into a bunch of lines.
//Note that you should manaually free the TStringList class yourself.
var
  s1,s2: string;
begin
  result := TStringList.create;
{$IFDEF FMX}
  result.TrustyText := s;
{$ELSE}
  result.text := s;
{$ENDIF}

end;
//------------------------------------------------------------------------------
function HexEncode(s: string): string;
//a: Jason Nelson
//This is FOR DEBUGGING PURPOSES ONLY, it dumps binary out in a readable hex format.
//Are you looking for stringToHex?
var
  s1, s2: string;
  c: char;
  t: integer;
begin
  s1 := '';
  s2 := '';
  result :='';

  for t:= 1 to length(s) do begin
    //get the character
    c := s[t];
    //if the character is visible display it on line 1
    if (ord(c)>31) and (ord(c)<127) then begin
      s1 := s1 + (c+'  ');
    end
    //else put XX into line 12
    else begin
      s1 := s1 + '__ ';
    end;
    //line 2
    //put hex value into line 2
    s2 := s2+(inttohex(ord(c), 2)+' ');

    //on end of line... add crap to result
    if c= #10 then begin
      result := result +s1+#13#10+s2+#13#10;
      s1 := '';
      s2 := '';
    end;

  end;
  //add end crap to end
  result := result +s1+#13#10+s2+#13#10;
  s1 := '';
  s2 := '';


end;
function MakeThreadSafe(s: string): string;
//a: Jason Nelson
//Makes a copy of the string so that it can be safely passsed to another thread.
//p: s: The string you want to make thread safe, note that you should have your own critical section around the variable to prevent access while the string is copied.
//p: result: A new string with a reference count of 1.
begin
  //result := Pchar(s);
  result := s;
  {$IFDEF ENABLE_UNIQUESTRING}
  UniqueString(result);
  {$ENDIF}
end;



function SplitString(sSource: string; const sSplitter: string; var sLeft, sRight: string; bStartAtRight: boolean = false;iZStartAt: ni=0): boolean; overload;
//Splits a string into sleft and sRight at sSplitter.  If sSplitter is not found
//then the entire string is in sLeft and sRight is blank, unless bStartAtRight is TRUE.
//returns true if split.... else false
//p: sSource: The master string
//p: sSplitter: The pattern that triggers the split
//p: var sLeft: Reference to the string that will contain the LEFT half of the split
//p: var sRight: Reference to the string that will contain the RIGHT half of the split
//p: bStartAtRight: Default is FALSE, boolean indication whether to start at the right half of the string instead of the left
var
  iPos : nativeint;
begin
  //find position of splitter
  if bStartAtRight then
    iPos:= zlastpos(sSplitter, sSource, iZStartAT)
  else
    iPos := zpos(sSplitter, sSource, iZStartAT);

  //if splitter not found
  if iPos<0 then begin
    //entire source goes to left
    if not bStartAtRight then begin
      sLeft := sSource;
      sRight := '';
    end else begin
      sRight := sSource;
      sLeft := '';
    end;

    result := false;
  end
  else begin
    //otherwise Slice and dice at the splitter
    sLeft := zcopy(sSource, 0, iPos);
    sRight := zcopy(sSource, iPos+length(sSplitter), length(sSource));
    result := true;
  end;
end;



//Splits a string into sleft and sRight at sSplitter.  If sSplitter is not found
//then the entire string is in sLeft and sRight is blank, unless bStartAtRight is TRUE.
//returns true if split.... else false
//p: sSource: The master string
//p: sSplitter: The pattern that triggers the split
//p: var sLeft: Reference to the string that will contain the LEFT half of the split
//p: var sRight: Reference to the string that will contain the RIGHT half of the split
//p: bStartAtRight: Default is FALSE, boolean indication whether to start at the right half of the string instead of the left


(*function SplitString(sSource: string; const sSplitter: string; var sLeft, sRight: string; bStartAtRight: boolean = false): boolean; overload;
//Splits a string into sleft and sRight at sSplitter.  If sSplitter is not found
//then the entire string is in sLeft and sRight is blank, unless bStartAtRight is TRUE.
//returns true if split.... else false
//p: sSource: The master string
//p: sSplitter: The pattern that triggers the split
//p: var sLeft: Reference to the string that will contain the LEFT half of the split
//p: var sRight: Reference to the string that will contain the RIGHT half of the split
//p: bStartAtRight: Default is FALSE, boolean indication whether to start at the right half of the string instead of the left
var
  iPos : integer;
begin
  //find position of splitter
  if bStartAtRight then
    iPos:= lastpos(sSplitter, sSource)
  else
    iPos := pos(sSplitter, sSource);

  //if splitter not found
  if iPos<1 then begin
    //entire source goes to left
    if not bStartAtRight then begin
      sLeft := sSource;
      sRight := '';
    end else begin
      sRight := sSource;
      sLeft := '';
    end;

    result := false;
  end
  else begin
    //otherwise Slice and dice at the splitter
    sLeft := copy(sSource, 1, iPos-1);
    sRight := copy(sSource, iPos+length(sSplitter), length(sSource));
    result := true;
  end;
end;*)



function SplitStringNoCaseEx(sRealSource, sSplitter: string; var sLeft, sRight: string; notinLeft, notInRight: string; bStartAtRight: boolean = false): boolean; overload;
//Splits a string into sleft and sRight at sSplitter.  If sSplitter is not found
//then the entire string is in sLeft and sRight is blank, unless bStartAtRight is TRUE.
//returns true if split.... else false
//p: sSource: The master string
//p: sSplitter: The pattern that triggers the split
//p: var sLeft: Reference to the string that will contain the LEFT half of the split
//p: var sRight: Reference to the string that will contain the RIGHT half of the split
//p: bStartAtRight: Default is FALSE, boolean indication whether to start at the right half of the string instead of the left
var
  iPos : integer;
  sSource: string;
begin

  sSource := lowercase(sRealSource);
  sSplitter := lowercase(sSplitter);

  //find position of splitter
  if bStartAtRight then
    iPos:= lastposex(sSplitter, sSource, [notinleft, notinright])
  else
    iPos := posex(sSplitter, sSource, [notinleft, notinright]);

  //if splitter not found
  if iPos<1 then begin
    //entire source goes to left
    if not bStartAtRight then begin
      sLeft := sRealSource;
      sRight := '';
    end else begin
      sRight := sRealSource;
      sLeft := '';
    end;

    result := false;
  end
  else begin
    //otherwise Slice and dice at the splitter
    sLeft := copy(sRealSource, 1, iPos-1);
    sRight := copy(sRealSource, iPos+length(sSplitter), length(sRealSource));
    result := true;
  end;
end;



function SortStringCompare(s1,s2: string): integer;
//Compares two strings. This is commonly used for the quicksort of a list of obj's
begin
  if s1 > s2 then begin
    result:=1;
  end else if s1 < s2 then begin
    result:=-1;
  end else begin
    result:=0;
  end;

end;


function LoadFileAsString(sFile: string; iLimitLength: integer = 0): string;
begin
  result := LoadStringFromfile(sfile, iLimitLength);
end;


function LoadStreamAsString(s: TStream): string;
var
  sl: TStringlist;
begin
  sl := TStringlist.create;
  try
    sl.LoadFromStream(s);
    result := sl.text;
  finally
    sl.Free;
  end;

end;
function LoadStringFromFile(sFile: string; iLimitLength: integer = 0): string;
var
  sl : TStringList;
  fs: TfileStream;
  sTemp: array[0..8000] of char;
{$IFDEF NO_LEGACY_STRINGS}
  u8: TBytes;
{$ELSE}
  u8: UTF8String;
{$ENDIF}
  iRead: integer;
  te: TEncoding;
begin
  result := '';
  sl := nil;
  try
    if iLimitLength = 0 then begin
      sl := TStringLIst.create;
      if FileExists(sFile) then
      try
        sl.LoadFromFile(sFile);
        result := sl.text;
      except
      end;
    end else begin
      if iLimitLength > 8000 then iLimitLength := 8000;
      fs := TfileStream.create(sFile, fmopenRead+fmShareDenynone);
      try
        iRead := fs.Read(sTemp, iLimitLength);
{$IFDEF NO_LEGACY_STRINGS}
        SetLength(u8, iRead);
        movemem32(@u8[0], @sTemp[0], iRead);
        te := TEncoding.UTF8;
        result := te.GetString(u8, 0, length(u8));

{$ELSE}
        SetLength(u8, iRead);
        movemem32(@u8[strzero], @sTemp[0], iRead);
//        setlength(u8, iRead);
        result := u8;
{$ENDIF}

      finally
        fs.free;
      end;
    end;
  finally
    sl.free;
  end;

end;

function BestPos(SubStrings: array of string; sMain: string; out sMatch: string; out position: integer; NotIn: array of string; bStartAtRight: boolean = false): boolean;
var
  t: integer;
  sCurrentSub: string;
  i: integer;
begin
  position := -1;
  sMAtch := '';
  for t:= Low(SubSTrings) to High(SubStrings) do begin
    sCurrentSub := SubStrings[t];

    if bStartAtright then begin
      i := LastPosEx(sCurrentSub, sMain, NotIn);
      if i > -1 then begin
        if i> position then begin
          position := i;
          sMatch := sCurrentSub;
        end;
      end;

    end else begin
      i := posex(sCurrentSub, sMain, NotIn);
      if i > -1 then begin
        if ((i< position) and (i > -1)) or ((position = -1) and (i > -1))then begin
          position := i;
          sMatch := sCurrentSub;
        end;
      end;
    end;
  end;

  result := position > -1;



end;

function SplitString(sSource: string; sSplitters: array of string; var sLeft, sRight: string; out sDelimiterUsed: string; NotIn: array of string; bStartAtRight: boolean = false): boolean; overload;
var
  iAT: integer;
begin
  result := false;

  if BestPos(sSPlitters, sSource,sDelimiterUsed, iAT, NotIn) then begin
    result := SplitString(sSource, sDelimiterUsed, sLEft, sRight);

  end;


end;

function StringIsAt(sMain, sSub: string; iPosToSearch: ni): boolean;
var
  t: ni;
  tpos: ni;
begin
  result := true;
  tpos := iPosToSearch;
  for t:= low(sSub) to high(sSub) do begin
    if tpos > high(sMain) then begin
      result := false;
      exit;
    end;

    if sMAin[tpos] <> sSub[t] then begin
      result := false;
      exit;
    end;
  end;

end;

function PosEx(sSubString, sMain: string; NotIn: array of string): integer;
//Searches for a substring in a string that is not inside certain set delimiters

var
  sTemp: string;
  sDel: string;
  t,u,i: integer;
  iDelLenL, iDelLenR: integer;
  bHasDelimiter: boolean;
  bChanged: boolean;
  aRefs: array of integer;
  function bInDelimiter: boolean;
  var  tttt: integer;
  begin
    result :=false;
    for tttt := low(aRefs) to high(aRefs) do begin
      if aRefs[tttt] > 0 then begin
        result := true;
        break;
      end;
    end;
  end;
begin

  setlength(aRefs, length(notin));
  for t:= low(aRefs) to high(aRefs) do begin
    aRefs[t] := 0
  end;

  result := -1;
  i := length(sSubString);


  bHasDelimiter := (High(notIn)-Low(NotIn)+1) > 1;

//  iDelLenL := length(NotIn[Low(NotIn)]);
//  iDelLenR := length(NotIn[High(NotIn)]);
  iDelLenR := 0;

  if i < 1 then
    exit;


  for t:= 1 to length(sMain)-i+1 do begin
      //check if the current character is a delimiter passed in the array
      bChanged := false;
      for u := Low(NotIn) to High(NotIn) do begin
        //delimiters are handled in pairs, if this is the odd pair, then skip this
        //loop
        if ((u - Low(NotIn)) mod 2) <> 1 then
          continue;
        //determine the length of the delmiter passed
        iDelLenL := Length(NotIn[u]);

        if NotIn[U] <> '' then begin
          //extract a string from the main string that is the same
          //length as the delimiter in question
//          sDel := copy(sMain, T, iDelLenL);
          //if the extracted string mathes the delimiter then
//          if sDel = NotIn[u] then begin
          if stringisat(sMAin, NotIn[u], t) then begin
            //remember which delimiter we are inside
            if aRefs[u] = aRefs[u-1] then
              arefs[u-1] := arefs[u-1] xor 1
            else
              aRefs[u-1] := aRefs[u-1]-1;
            bChanged := true;
            //remember the length of the ending delimiter
            iDelLenR := Length(NotIn[u]);
            //break out of this loop
            break;
          end;//IF sDEL
        end;//IF NOTIN[u]
      end;//IF


    //if no in the delimiter
    //if a delimiter was supplied
    if bHasDelimiter and (not bchanged) then begin
      //check if the current character is a delimiter passed in the array
      for u := Low(NotIn) to High(NotIn) do begin
        //delimiters are handled in pairs, if this is the odd pair, then skip this
        //loop
        if ((u - Low(NotIn)) mod 2) <> 0 then
          continue;
        //determine the length of the delmiter passed
        iDelLenL := Length(NotIn[u]);

        if NotIn[U] <> '' then begin
          //extract a string from the main string that is the same
          //length as the delimiter in question
//          sDel := copy(sMain, T, iDelLenL);
          //if the extracted string mathes the delimiter then
//          if sDel = NotIn[u] then begin
          if stringisat(sMAin, NotIn[u], t) then begin
            //remember which delimiter we are inside
            if aRefs[u] = aRefs[u+1] then
              arefs[u] := arefs[u] xor 1
            else
              aRefs[u] := aRefs[u]+1;
            //remember the length of the ending delimiter
            iDelLenR := Length(NotIn[u+1]);
            //break out of this loop
            break;
          end;//IF sDEL
        end;//IF NOTIN[u]
      end;//IF


      //if in the delimiter then continue
      if bInDelimiter then
        continue;

      sTemp := copy(sMain, t, length(sSubString));
      //if not in the dleimiter then
      //if the substring is found
      if sTemp = sSubString then begin
        //return a result
        result := t;
        //break out of main loop
        break;
      end;
    end
  end;

end;


function LastPosEx(sSubString, sMain: string; NotIn: array of string): integer;
var
  sTemp: string;
  sDel: string;
  t, i: integer;
  iDelLenL, iDelLenR: integer;
  bInDelimiter: boolean;
  bHasDelimiter: boolean;
begin
  bInDelimiter := false;
  result := -1;
  i := length(sSubString);


  bHasDelimiter := (High(notIn)-Low(NotIn)+1) > 1;

  if bHasDelimiter then
    bHasDelimiter := NotIn[Low(notIn)] <> '';

  iDelLenL := length(NotIn[Low(NotIn)]);
  iDelLenR := length(NotIn[High(NotIn)]);

  if i < 1 then
    exit;

  for t:= length(sMain)-i+1 downto 1 do begin
    if not bInDelimiter then begin
      //check against delmiter for strings/comments and whatnot
      if bHasDelimiter then begin
        sDel := copy(sMain, t, iDelLenL);
        if sDel = NotIn[Low(NotIn)] then begin
          bInDelimiter := true;
          continue;
        end;
      end;

      //check for the substring if not in string state
      sTemp := copy(sMAin, t, i);
      if sTemp = sSubString then begin
        result := t;
        break;
      end;
    end else begin
      //check for end of delimiter/string
      sDel := copy(sMain, t, iDelLenR);
      if sDel = NotIn[High(NotIn)] then begin
        bInDelimiter := true;
        continue;
      end;
    end;
  end;
end;

function stringToDumbRAM(s: string): pointer;
var
  p: PDumbRamString;
  i: integer;
  pc: Pchar;
begin
//  if s = '' then begin
//    result := nil;
//    break;
//  end;

  i := length(s);
  GetMem(p, i+SizeOf(TDumbRamString));
  p.Size := i;

  pc := Pchar(p)+4;

  if i > 0 then
    MoveMem32(pc, @s[1], i);


  result := p;
end;

procedure FreeDumbRamString(var p: PDumbRamString);
begin
  if p <> nil then begin
    freeMem(p);
    p := nil;
  end;



end;

function DumbRamToString(p: pointer): string;
var
  ps: PDumbRamString;
  pc: Pchar;
begin
  if p = nil then begin
    result := '';
    exit;
  end;


  ps := PDumbRamString(p);
  pc := Pchar(p)+SizeOf(TDumbRamString);

  SetLength(result, ps.size);

  if ps.size > 0 then
    MoveMem32(@result[1], @pc[0],  ps.size);




end;

procedure SaveStringAsFile(sFile: string; data: string);
{$IFDEF NO_LEGACY_STRINGS}
var
  fs: TFileStream;
  s: string;
begin
  s := data;
//  outputDebugString(pchar(sFile));


  if fileexists(sFile) then begin
    fs := TFileStream.create(sFile, fmOpenReadWrite+fmShareExclusive);
    try
      fs.Size := 0;
      fs.Seek(0,soBeginning);
      fs.Write(s[1], length(s));
      fs.Size := length(s);
    finally
      fs.free;
    end;
  end else begin
    fs := TFileStream.create(sFile, fmCreate);
    try
      fs.Seek(0,soBeginning);
      Stream_GuaranteeWrite(fs, @s[strzero], length(s)*sizeof(char));
      fs.Write(s[1], length(s));
      fs.Size := length(s);
    finally
      fs.free;
    end;
  end;
end;
{$ELSE}
var
  fs: TFileStream;
  s: UTF8String;
begin
  s := data;
//  outputDebugString(pchar(sFile));


  if fileexists(sFile) then begin
    fs := TFileStream.create(sFile, fmOpenReadWrite+fmShareExclusive);
    try
      fs.Size := 0;
      fs.Seek(0,0);
      fs.Write(s[1], length(s));
      fs.Size := length(s);
    finally
      fs.free;
    end;
  end else begin
    fs := TFileStream.create(sFile, fmCreate);
    try
      fs.Seek(0,0);
      fs.Write(s[1], length(s));
      fs.Size := length(s);
    finally
      fs.free;
    end;
  end;
end;
{$ENDIF}

function CountLinesInTextFile(sFile: string): integer;
var
  sl: TStringList;
begin
  result := 0;
  if not fileexists(sFile) then begin
    exit;
  end;

  sl := TStringlist.Create;
  try
    sl.LoadFromFile(sFile);
    result := sl.Count;

  finally
    sl.Free;
  end;



end;


function stringProgress(rPercent: real; length: integer; cap1: string = '['; cap2: string = ']'; bar: string = '|'; space: string = ' '): string;
var
  t,barcount, spacecount: integer;
begin
  barcount := round(rPercent * length);
  spacecount := round((1-rPercent) * length);

  result := cap1;
  for t:= 1 to barcount do begin
    result :=result + bar;
  end;

  for t:= 1 to spacecount do begin
    result :=result + space;
  end;


  result := result + ']';



end;

function StripPhone(sPhone: string): string;
begin
  result := sPhone;
  result := stringreplace(result, '-','',[rfReplaceAll]);
  result := stringreplace(result, '(','',[rfReplaceAll]);
  result := stringreplace(result, ')','',[rfReplaceAll]);
  result := stringreplace(result, ' ','',[rfReplaceAll]);

end;

function StripPhone2(sPhone: string): string;
var
  t: integer;
  s: string;
begin
  s := sphone;

  for t:= 1 to length(sPhone) do begin
    if (s[t] < '0') or (s[t] > '9') then begin
      s[t] := ' ';
    end;
  end;

  result := stringReplace(s, ' ', '', [rfReplaceAll]);

end;

procedure mergeStringLists(slmaster: tStringList; sInsert: string; iStartRow, iStartCol, iEndrow, iEndcol: integer);
var
  slTemp: TStringList;
  t: integer;
  sLeft: string;
  sRight: string;
  sTemp: string;
begin
  slTemp := TStringList.create;
  try
    slTemp.text := sInsert;
    //delete the area to be affected

    //first line


    sLeft := Copy(slMaster[iStartRow], 1, iStartCol-1);
    sRight := copy(slmaster[iEndRow], iEndcol, 999999999);

    DeleteStringListArea(slMaster, 0, iStartRow, length(slMaster[iEndRow])+1, iEndrow);


    for t:= slTemp.count-1 downto 0 do begin
      sTemp := slTemp[t];

      if t = 0 then
        stemp := sLeft+sTemp;

      if t = slTemp.count-1 then begin
        sTemp := stemp+sRight;
      end;

      slMaster.Insert(iStartRow+1, sTemp);

    end;

  finally
    slTemp.free;
  end;






end;


//------------------------------------------------------------------------------
procedure ReplicateStringListArea(sl: TStringList; iStartRow, iEndRow, iRepeatCount: integer; sDelimiters: string = '');
var
//  bAtEnd: boolean;
  t, u: integer;
  slDelimiters: TStringList;
  slTemp: TStringLIst;
//  sTemp: string;
//  bFlag: boolean;
  iTemp: integer;
begin
  slDelimiters := TStringList.create;
  slTemp := TStringList.create;

  try
    slDelimiters.text := sDelimiters;
//    bAtEnd := (iEndRow = sl.count-1);

    //if the repeat count is 0 then delete the section... because it shouldn't exist
    if iRepeatCount= 0 then begin
      for t:= iEndrow downto iStartRow do begin
        sl.delete(t);
      end;

    end
    else begin
      for u:= 1 to iRepeatCount do begin
        for t:= iStartRow to iEndRow do begin
          slTemp.add(sl[t]);
        end;
        if sDelimiters <> '' then begin
          if slDelimiters.count = 1 then
            slTemp.Add(sDelimiters)
          else
            slTemp.Add(slDelimiters[u-1])
          ;
        end;
        //showmessage(slTemp.text);
      end;

      for t := iEndRow+1 to sl.count-1 do begin
        slTemp.add(sl[t]);
      end;

      for t:= 0  to slTemp.count-1 do begin
        iTemp := iStartRow+t;
        if iTemp <= sl.count-1 then begin
          sl[iTemp] := slTemp[t]
        end else begin
          sl.add(slTemp[t]);
        end;
      end;


      //sTemp := slTemp.text;
    end;
  finally
    slDelimiters.free;
    slTemp.free;
  end;

end;


procedure DeleteStringListArea(sl: TStringList; iStartCol, iStartRow, iEndCol, iEndRow: integer; sReplace: string = '');
begin
  //Setup the end result
  sl[iStartRow] := copy(sl[iStartRow], 1, iStartCol-1)+sReplace+
                   copy(sl[iEndRow], iEndCol, length(sl[iEndRow]));

  if not (iEndRow = iStartRow) then begin
    //Delete the rows starting after the startrow upto the endrow
    ReplicateStringListArea(sl, iStartrow+1, iEndRow, 0);
  end;


end;
function DatetoMYSQLDate(dt: TDateTime): string;
begin
  result := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
end;

function MYSQLDateTimeToDateTime(s: string): TDateTime;
var
  s1,s2,s3,s4,s5,s6: string;
  half: string;
begin
  SplitString(s, '-', s1,s2);
  SplitString(s2,'-', s2,s3);
  SplitString(s3,' ', s3,s4);
  SplitString(s4,':', s4,s5);
  splitString(s5,':', s5,s6);

  half := 'AM';
  if strtoint(s4) >= 12 then
    half := 'PM';

  case strtoint(s4) of
    0: s4 := '12';
    13..23: s4 := inttostr(strtoint(s4)-12);
  end;

  s := s2+'/'+s3+'/'+s1+' '+s4+':'+s5+':'+s6+half;
  result := strtodatetime(s);


end;

function IntToRank(i: integer): string;
var
  s: string;
begin
  s := inttostr(i);


  if (length(s) < 2) or (s[length(s)-1] <> '1') then begin
    case s[length(s)] of
      '0': if length(s)>1 then result := s+'th' else result := '--';
      '1': result := s+'st';
      '2': result := s+'nd';
      '3': result := s+'rd';
      '4','5','6','7','8','9': result := s+'th';
    end;
  end else begin
    result := s+'th';
  end;

end;

function PairsToParams(sPairs: string): string;
begin
  result := stringReplace(sPairs, '&', 'and', [rfReplaceAll]);
  result := stringReplace(result , #10, '', [rfReplaceAll]);
  result := stringReplace(result , #13, '&', [rfReplaceAll]);

end;

function ExpandMacAddress(sMac: string): string;
begin
  result := '';

  if length(sMac) = 11 then
    sMac := '0'+sMac;


  sMac := uppercase(sMac);

  sMac := stringReplace(sMac, '-', '', [rfReplaceAll]);


  result :=
    copy(sMac, 1, 2)+'-'+
    copy(sMac, 3, 2)+'-'+
    copy(sMac, 5, 2)+'-'+
    copy(sMac, 7, 2)+'-'+
    copy(sMac, 9, 2)+'-'+
    copy(sMac, 11, 2);

end;

function CleanString(s: string): string;
begin
  result := s;
  result := stringReplace(result, #13, '', [rfReplaceAll]);
  result := stringReplace(result, #10, '', [rfReplaceAll]);
end;

function PcharToString(Pchar: Pchar; length: integer): string;
begin
  result := '';
  SetLength(result, length);

  MoveMem32(@result[1], Pchar, length);




end;

function CropStringRL(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;
var
  sL, sR: string;

begin
  result := false;
  if SplitStringNoCase(sSource, sRight, sL, sR) then begin
    if SplitStringNoCase(sL, sLeft, sL, sR, true) then begin
      sResult := sR;
      result := true;
    end;
  end;
end;

function CropStringLR(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;
var
  sL, sR: string;

begin
  result := false;
  if SplitStringNoCase(sSource, sLeft, sL, sR) then begin
    if SplitStringNoCase(sR, sRight, sL, sR) then begin
      sResult := sL;
      result := true;
    end;
  end;
end;


function PosFirst(sSource: string; search1, search2: string): integer;
//Note: returns 0 if none found
var
  i1,i2: integer;
begin
  result := 0;
  i1 := pos(search1, sSource);
  i2 := pos(search2, sSource);

  if i2 > 0 then
    result := 2;

  if (i1 < i2) and (i1 > 0) then
    result := 1;

end;


function PosFirst(sSource: string; search1, search2, search3: string): integer;
//Note: returns 0 if none found
var
  i1,i2,i3, iFinal: integer;
begin
  result := 0;
  i1 := pos(search1, sSource);
  i2 := pos(search2, sSource);
  i3 := pos(search3, sSource);

  iFinal := i1;
  if i1 > 0 then
    result := 1;

  if (iFinal > i2) and (iFinal > 0) then begin
    iFinal := i2;
    result := 2;
  end;

  if (iFinal > i3) and (iFinal > 0) then begin
    result := 3;
  end;
end;

function Quote(s: string): string;
begin
  if s <> '' then begin
    if s[1] = '"' then begin
      result := s;
      exit;
    end;
  end;
  result := '"'+s+'"';
end;

function Quote(sl: TStringlist): string;overload;
var
  t: ni;
begin
  for t:= 0 to sl.count-1 do begin
    sl[t] := Quote(sl[t]);
  end;

end;

function CommaQuote(s: string): string;
begin
  result := ',"'+s+'"';
end;

function BoolString(bCondition: boolean; sTrue: string=''; sFalse: string = ''): string;
begin
  if (sTrue = '') and (sFalse = '') then begin
    strue := 'true';
    sfalse := 'false';
  end;

  if bCondition then
    result := sTRUE
  else
    result := sFALSE;
end;


function CropString(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;
var
  r1,r2: string;
begin
  CropStringLR(sSource, sLeft, sRIght, r1);
  CropStringRL(sSource, sLeft, sRIght, r2);
  result := true;

  if length(r1) > length(r2) then
    sresult := r1
  else
    sresult := r2;


end;
function SplitStringNoCase(sRealSource, sSplitter: string; var sLeft, sRight: string; bStartAtRight: boolean = false): boolean; overload;
begin
  result := SplitStringNoCaseEx(sRealSource, sSplitter, sLeft, sRight, '','', bStartAtRight);
end;



function Abbreviate(s: string): string;
const
  vowels = ['a','e','i','o','u','y','A','E','I','O','U','Y'];
var
  t: integer;
  b: boolean;
begin
  s := StringReplace(s, 'and', '&', [rfReplaceAll, rfIgnoreCase]);


  if s = '' then begin
    result := '';
    exit;
  end;

  result := s[1];
  for t:= 2 to length(s) do begin
    b := true;
    if (lowercase(s[t-1])=lowercase(s[t])) then
      b := false;

    if CharInSet(s[t], vowels) then
      b := false;

    if b then
      result := result+s[t];

  end;
end;

function MatchCase(sSource, sTarget: string): string;
var
  iSourceLength: integer;
  t, idx: integer;
begin

  result := sTarget;

  iSourceLength := length(sSource);
  if iSourceLength = 0 then
    raise Exception.create('Error matching case: source string is 0 length');

  for t:= 1 to length(sTarget) do begin
    if t>iSourceLength then
      idx := iSourceLength
    else
      idx := t;

    if CharInSet(sSource[idx], ['a'..'z']) and charInSet(sTarget[t], ['A'..'Z']) then begin
      result[t] := char(lowercase(sTarget[t])[1]);
    end else
    if CharInSet(sSource[idx], ['A'..'Z']) and charInSet(sTarget[t], ['a'..'z']) then begin
      result[t] := char(uppercase(sTarget[t])[1]);
    end;
  end;
end;

function IsInteger(s: string): boolean;
var
  t: integer;
  c: char;
begin
  result := length(s) > 0;
  for t:= 1 to length(s) do begin
    c := s[t];
    if not charinSet(c, ['0'..'9']) then begin
      result := false;
      break;
    end;

  end;

end;

function SplitQuote(sOrig: string; sQuoteDelimiter: string; var sLeft, sRight: string; out sInQuote: string): boolean;
begin

  result := SplitStringNoCase(sOrig, sQuoteDelimiter, sLeft, sInQuote);

  if Result then begin
    result := SplitStringNoCase(sInQuote, sQuoteDelimiter, sInQuote, sRight);
  end;

end;

function SplitStringNoCaseEx(sRealSource, sSplitter: string; var sLeft, sRight: string; notin: TSplitSet; bStartAtRight: boolean = false): boolean; overload;
var
  iPos : integer;
  sSource: string;
begin

  sSource := lowercase(sRealSource);
  sSplitter := lowercase(sSplitter);

  //find position of splitter
  if bStartAtRight then
    iPos:= lastposex(sSplitter, sSource, notin)
  else
    iPos := posex(sSplitter, sSource, notin);

  //if splitter not found
  if iPos<1 then begin
    //entire source goes to left
    if not bStartAtRight then begin
      sLeft := sRealSource;
      sRight := '';
    end else begin
      sRight := sRealSource;
      sLeft := '';
    end;

    result := false;
  end
  else begin
    //otherwise Slice and dice at the splitter
    sLeft := copy(sRealSource, 1, iPos-1);
    sRight := copy(sRealSource, iPos+length(sSplitter), length(sRealSource));
    result := true;
  end;
end;



function SQLEscape(s: string): string;
begin
  result := s;
  result := stringreplace(result, '\', '\\', [rfReplaceAll]);
  result := stringreplace(result, '"', '\"', [rfReplaceAll]);
  result := stringreplace(result, '''', '\''', [rfReplaceAll]);


end;

function VArtoJSONStorage(v: variant): string;
var
  vt: cardinal;
begin
  vt := vartype(v);
  if vt=varNull then begin
    result := 'NULL';
  end else
  if (vt and varDate)=varDate then begin
    result := '"'+JStr(v)+'"';
  end else
  if (vt and varString)=varString then begin
    result := '"'+JStr(vartostr(v))+'"';
  end
  //--------------------------------
  else if (vt and varBoolean)=varBoolean then begin
    if v then
      result := '1'
    else
      result := '0';
  end
  //--------------------------------
  else begin
    result := vartoSTr(v);
  end;
end;

function VarToMYSQLStorage(v: variant): string;
var
  vt: cardinal;
begin
  vt := vartype(v);
  if vt=varNull then begin
    result := 'NULL';
  end else
  if (vt and varDate)=varDate then begin
    result := '"'+datetomysqldate(v)+'"';
  end else
  if (vt and varString)=varString then begin
    result := '"'+SQLEscape(vartostr(v))+'"';
  end
  //--------------------------------
  else if (vt and varBoolean)=varBoolean then begin
    if v then
      result := '1'
    else
      result := '0';
  end
  //--------------------------------
  else begin
    result := vartoSTr(v);
  end;
end;

function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: string; var sLeft, sRight: string; notinLeft, notInRight: string; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;
begin
  result := SplitStringNoCaseEx(sRealSource, sSplitter, sLEft, sRight, notinLeft, notinRight, bStartAtRight);
  if result then begin
    if bPutDelimiterInRight then
      sRight := sSplitter+sRight
    else
      sLEft := sLeft + sSplitter;
  end;


end;



function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: string; var sLeft, sRight: string; notin: TSplitSet; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;
begin
  result := SplitStringNoCaseEx(sRealSource, sSplitter, sLEft, sRight, notin,bStartAtRight);
  if result then begin
    if bPutDelimiterInRight then
      sRight := sSplitter+sRight
    else
      sLEft := sLeft + sSplitter;
  end;


end;



function StartsWith(sMaster: string; sStartsWith: string): boolean;
begin
  sStartswith := lowercase(sStartsWith);
  result := lowercase(copy(sMaster, 1,length(sStartsWith))) = sStartsWith;

end;

function StartsWithThenSkip(var sMaster: string; sStartsWith: string): boolean;
begin
  sMaster := Trimstr(sMaster);
  result := StartsWith(sMaster, sStartsWith);
  if result then begin
    sMaster := copy(sMaster, length(sStartsWith)+1, length(sMaster));
  end;
end;

function Unquote(s: string): string;
begin
  if StartsWith(s, '"') then begin
    result := copy(s, 2, length(s)-2); //TODO 5: This should be improved... TRIM spaces
  end else
    result := s;




end;

function Scramble(s: string): string;
begin
  result:= s;
end;
function Unscramble(s: string): string;
begin
  result := s;
end;

function StringTohex(s: string): string;
var
  t: integer;
begin
  result := '';
  for t:= 1 to length(s) do begin
    result := result + inttohex(ord(s[t]), 2);
  end;

end;

function HextoString(sHexString: string): string;
var
  t: integer;
begin
  result := '';
  t:= 1;
  while t < length(sHexString) do begin
    result := result + chr(strtoint('$'+copy(sHexString, t, 2)));
    inc(t,2);
  end;
end;

function CleanInjection(s: string): string;
begin
  result := s;
  result := stringReplace(result, '[[[','`[``[``[`', [rfReplaceAll]);
  result := stringReplace(result, '"','%'+inttohex(ord('"'),2), [rfReplaceAll]);

end;

function UncleanInjection(s: string): string;
begin
  result := s;
  result := stringReplace(result, '`[`', '[', [rfReplaceAll]);
  result := stringReplace(result, '%'+inttohex(ord('"'),2),'"', [rfReplaceAll]);

end;


function StrSize(s: string): integer;
begin
  result := length(s)*sizeof(char);
end;

function DelimitIfNotEmpty(sInput: string; sDelimiter: string): string;
begin
  if (sInput <> '') then result := sInput+sDelimiter else result := sInput;
end;


function RemoveDoubles(s: string; c: char): string;
var
  t,u: integer;
  cPrev: char;
begin
  result := '';
  setlength(result, length(s));
  u := 2;
  if s = '' then
    exit;

  result[1] := s[1];
  for t:= 2 to length(s) do begin
    if s[t] = c then begin
      if s[t-1] <> c then begin
        result[u] := c;
        inc(u);
      end else begin
      end;

    end else begin
      result[u] := s[t];
      inc(u);
    end;

  end;
  if (result[u-1] = ' ') then begin
    setlength(result,u-1);
  end else begin
    setlength(result,u-1);
  end;

end;


function SplitStringIntoStringList(s: string; cDelimiter: char = ' '; bRespectQuote: char = #0): TStringList;
var
  slocal: string;
  t: integer;
  iBegin: integer;
  iEnd: integer;
  bQuote:boolean;
  sSub: string;
begin
  bQuote := false;
  result := TStringlist.create;
  iBegin := STRZ;
  for t:= STRZ to high(s) do begin


    if (bRespectQuote <> #0) and (s[t] = bRespectQuote) then
      bQuote := not bQuote;

    if not bQuote then begin
      if s[t] = cDelimiter then begin
        sSub := zcopy(s, iBegin, ((t-1)-iBegin));
        result.add(sSub);
        if bRespectQuote <> #0 then begin
          sSub := zcopy(sSub, 0, length(sSub)-1);
          result[result.count-1] := sSub;
        end;
        iBegin := t+1;
      end;
    end;
  end;
  sSub := trim(zcopy(s, iBegin, (length(s)-iBegin)));
  if bRespectQuote <> #0 then begin
    sSub := zcopy(sSub, 0, length(sSub)-1);
  end;
  result.add(sSub);



end;

function ExtractLinksFromPlainText(s: string): string;
var
  sl: TStringlist;
  t: integer;
begin
  sl := SplitSTringIntoStringList(s);
  try
    for t:= sl.count-1 downto 0 do begin
      s := sl[t];
      if not (zcopy(lowercase(s), 0, length('http://')) = 'http://') then begin
        sl.delete(t);
      end;
    end;
  finally
    result := sl.text;
    sl.free;
  end;
end;

function AtPattern(iPOs: integer; sPattern, sString: string): boolean;
begin
  result := copy(sString, iPos, length(sPattern)) = sPattern;
end;

function QuickStyleRemoval(s: string): string;
var
  s1,s2,sbefore, safter: string;
begin
  result := s;
  while SplitSTringNoCase(result, '<style', sBefore,s2) do begin
    splitstring(s2, '/style>', s1,sAfter);
    result := sBefore+' '+sAfter;
  end;
end;

function HTMLToPlainText(s: string): string;
var
  igt: integer;
  bquot: boolean;
  bcomment: boolean;
  iMoveCredit: integer;
  t,u: integer;

begin
  u := 0;
  setlength(result, length(s));
  iMoveCredit := 0;
  igt := 0;
  bquot := false;
  bComment := false;
  s := QuickStyleRemoval(s);
  for t:= 1 to length(s) do begin
    if iMoveCredit > 0 then begin
      dec(iMoveCredit);
      continue;
    end;

    if bComment then begin
      if AtPattern(t,'-->', s) then begin
        bComment := false;
        iMoveCredit := 3;
        continue;
      end;
    end;

    if bQuot then begin
      if s[t] = '"' then begin
        bQuot := false;
        continue;
      end;
    end;

    if igt > 0 then begin
      if s[t] = '>' then begin
        dec(igt);
        continue;
      end;
    end;

    if (igt = 0) then begin
      if s[t] = '<' then begin
        inc(igt);
        continue;
      end else begin
        inc(u);
        result[u] := s[t];
      end
    end;

  end;

  setlength(result, u);

end;
function RemoveDuplicatesFromStringList(sl: TStringlist): boolean;
var
  s: string;
  t,u: integer;
begin
  result := false;
  for t:= sl.count-1 downto 0 do begin
    if t>=sl.count then continue;
    s := sl[t];
    for u := sl.count-1 downto 0 do begin
      if u>=sl.count then continue;
      if (t<>u) and (s=sl[u]) then begin
        sl.delete(u);
        result := true;
      end;
    end;
  end;
end;

procedure TrimStringList(sl: tStringlist);
var
  t: integer;
begin
  for t:= 0 to sl.count-1 do begin
    sl[t] := trim(sl[t]);
  end;

end;

function FloatPrecision(r: nativefloat; iDecPLaces: integer; bCollapseIntegers: boolean = false): string;
var
  iTemp: nativeint;
  iPow: nativeint;
  rTemp: nativefloat;
  iTemp2: nativeint;
  s1,s2: string;
begin
  iTemp := iDecPlaces;
  iPOw := round(power(10, iTemp));
  rTemp := r*iPow;
  iTemp2 := round(rTemp);
  rTemp := iTemp2 / iPow;

  result := FormatFloat('0.########', rTemp);


  if bCollapseIntegers and (rTemp = round(rTemp)) then begin
    result := inttostr(round(rTemp));

  end else begin
    if iTemp > 0 then begin
      if pos('.', result) = 0 then
        result := result+'.';

      splitstring(result, '.', s1,s2);
      s2 := copy(s2, 1,iDecPlaces);
      result := s1+'.'+s2;

      while (length(result) - pos('.', result) ) < iTemp do
        result := result + '0';

    end;
  end;
end;


function IsolateDelimited(sLine: string; rangestart, rangeEnd: nativeint; sDelimiter: string = ','): string;
var
  parsed: TStringlist;
  t: integer;
begin
  Order(rangeStart, RangeEnd);
  parsed := TStringlist.Create;
  try
    parseString(sLine, sDelimiter, parsed);
    for t:= parsed.Count-1 downto rangeEnd+1 do begin
      parsed.Delete(t);
    end;
    for t:= 0 to rangeStart-1 do begin
      parsed.Delete(0);
    end;
    result := UnParseString(sDelimiter, parsed);
  finally
    parsed.Free;
  end;
end;


function Commaize(i: int64): string;
//todo 5: describe this
var
  iToEnd: ni;
  s: string;
  idx: ni;
begin
  s := inttostr(i);
  idx := 1;
  result := '';
  while idx <= length(s) do begin
    iToEnd := (length(s)-idx)+1;

    if ((iToEnd mod 3) = 0) and (idx > 1) then begin
      result := result + ',';
      result := result + s[idx];
      inc(idx);

    end else begin
      result := result + s[idx];
      inc(idx);
    end;



  end;
end;

function IsHex(s: string): boolean;
var
  t: integer;
begin
  result := true;
  if length(s) = 0 then begin
    result := false;
    exit;
  end;

  if length(s) > 16 then begin
    result := false;
    exit;
  end;



  for t := 1 to length(s) do begin
    if not (charinset(s[t] , ['0'..'9', 'a'..'f', 'A'..'F']) or ((t = 1) and (s[t] = '-'))) then begin
      result := false;
      break;
    end;
  end;


end;

function StrToBool(s: string): boolean;
begin
  if IsInteger(s) then begin
    if strtoint(s) = 0 then begin
      result := false;
    end else begin
      result := true;
    end;
  end else begin
    s := lowercase(s);
    if s = '' then
      result := false
    else
    if s = 'false' then
      result := false
    else
    if s = 'true' then
      result := true
    else
    if s = 'no' then
      result := false
    else
    if s = '0' then
      result := false
    else
    if s = '-1' then
      result := true
    else
    if s = 'yes' then
      result := true
    else
      raise EConvertError.create('Unable to converts string "'+s+'" to boolean');

  end;

end;

function BoolToStrEx(b: boolean; sTrue: string ='TRUE'; sFalse:string = 'FALSE') : string;
begin
  result := BoolToStr(b, sTrue, sFalse);
end;

function BoolToStr(b: boolean; sTrue: string ='TRUE'; sFalse:string = 'FALSE') : string;
begin
  if b then result := sTrue else result := sFalse;
end;

function BoolToString(b: boolean; sTrue: string ='TRUE'; sFalse:string = 'FALSE') : string;
begin
  result := booltostr(b, strue, sfalse);
end;

function ZSubCopy(sSource: string; iPOs: nativeint; iLength: nativeint): string;
var
  t: nativeint;
begin
  result := '';
  SetLength(result, LesserOf(length(sSource)-(iPos), iLength));
  for t := 0 to length(result)-1 do begin
    result[t+low(result)] := sSource[(t+low(result))+iPos];
  end;
end;


function zsubisatpos_ignorecase(sSub: string; sString: string; idx: nativeint): boolean;
var
  t,i: nativeint;
  c1,c2: char;
begin
  result := true;
  for t:= 0 to length(sSub)-1 do begin
    i := idx+t;
    //if out of bounds, return false
    if i >= length(sString) then begin
      result := false;
      exit;
    end;

    //if chars don't match... return false
    c1 := sSub[t+strz];
    c2 := sString[i+STRZ];

    if c1 <> c2 then begin
      if charinset(c1, ['a'..'z','A'..'Z']) then begin
        if lowercase(c1) <> lowercase(c2) then begin
          exit(false);
        end;
      end else
        exit(false);
    end;

  end;
end;
function zsubisatpos(sSub: string; sString: string; idx: nativeint): boolean;
var
  t,i: nativeint;

begin
  result := true;
  for t:= 0 to length(sSub)-1 do begin
    i := idx+t;
    //if out of bounds, return false
    if i >= length(sString) then begin
      result := false;
      exit;
    end;

    //if chars don't match... return false
    if sSub[t+strz] <> sString[i+STRZ] then begin
      result := false;
      exit;
    end;


  end;
end;
function zpos(sSubString: string; sString: string;iSTartAt:ni=0): nativeint;
var
  t: nativeint;
begin
  result := -1;
  if sSubString = '' then
    result := -1;

  for t:= iSTartAt to length(sString)-length(sSubString) do begin
    if zsubisatpos(sSubString, sString, t) then begin
      result := t;
      exit;
    end;
  end;
end;

function zpos_ignorecase(sSubString: string; sString: string;iSTartAt:ni=0): nativeint;
var
  t: nativeint;
begin
  result := -1;
  if sSubString = '' then
    result := -1;

  for t:= iSTartAt to length(sString)-length(sSubString) do begin
    if zsubisatpos_ignorecase(sSubString, sString, t) then begin
      result := t;
      exit;
    end;
  end;
end;
function zlastpos(sSubString: string; sString: string;iSTartAt:ni=0): nativeint;
var
  t: nativeint;
begin
  result := -1;
  if sSubString = '' then begin
    result := -1;
    exit;
  end;

  for t:= lesserof(length(sString)-length(sSubString),length(sString)-iStartAT) downto 0 do begin
    if zsubisatpos(sSubString, sString, t) then begin
      result := t;
      exit;
    end;
  end;
end;


function zcopy(sString: string; iStartZeroBased: nativeint; iLength: nativeint): string;
begin
  result := '';
  if length(sString) = 0 then
    exit;
  setlength(result, lesserof(iLength, length(sString)-iStartZerobased));
  movemem32(@result[strz], @sString[(strz+iStartZeroBased)], length(result)*sizeof(char));
end;

function ocopy(sString: string; iStartOneBased: nativeint; iLength: nativeint): string;
begin
  result := zcopy(sString, iStartOneBased-1, iLength);
end;

function trimcharsfromfront(sString: string; char: string): string;
begin
  result := sString;
  if char = '' then
    raise ECritical.create('cannnot call this with blank char');
  while zcopy(result, 0, length(char)) = char do begin
    result := zcopy(result, length(char), length(result));
  end;
end;






{ TUT_StringX }

procedure TUT_StringX.DoExecute;
var
  sl1,sl2: TStringlist;
  sTemp: string;
  ioff: nativeint;
  sin, sLeft, sRight: string;
begin
  inherited;
  sl1 := TStringlist.create;
  sl2 := TStringlist.create;
  try
    case Variation of
      10: begin
        VariationName := 'function Commaize(1234567890): string;';
        utresult := Commaize(1234567890);
      end;
      20: begin
        VariationName := 'function IsInteger(... 0, -1, 5000000000, -5000000000, x, 5.1, 5.0 ...)';
        utresult := booltostr(IsInteger('0'))+', '+
                  booltostr(IsInteger('-1'))+', '+
                  booltostr(IsInteger('5000000000'))+', '+
                  booltostr(IsInteger('-5000000000'))+', '+
                  booltostr(IsInteger('x'))+', '+
                  booltostr(IsInteger('5.1'))+', '+
                  booltostr(IsInteger('5.0'));
      end;
      30: begin
        VariationName := 'function IsHex(... 0 -1 0x1 abcdef ABCDEF GHI ghi 0.1 45 FFFFFFFF FFFFFFFFF): boolean;';
        utresult := booltostr(IsHex('0'))+', '+
                  booltostr(IsHex('-1'))+', '+
                  booltostr(IsHex('0x1'))+', '+
                  booltostr(IsHex('abcdef'))+', '+
                  booltostr(IsHex('ABCDEF'))+', '+
                  booltostr(IsHex('GHI'))+', '+
                  booltostr(IsHex('ghi'))+', '+
                  booltostr(IsHex('0.1'))+', '+
                  booltostr(IsHex('45'))+', '+
                  booltostr(IsHex('FFFFFFFF'))+', '+
                  booltostr(IsHex('FFFFFFFFF'));

      end;
      40: begin
        VariationName := 'result := booltostr(StrToBool(''0''));';
        utresult := booltostr(StrToBool('0'));
      end;
      41: begin
        VariationName := 'result := booltostr(StrToBool(''1''));';
        utresult := booltostr(StrToBool('1'));
      end;
      42: begin
        VariationName := 'result := booltostr(StrToBool(''yes''));';
        utresult := booltostr(StrToBool('yes'));
      end;
      43: begin
        VariationName := 'result := booltostr(StrToBool(''no''));';
        utresult := booltostr(StrToBool('no'));
      end;
      44: begin
        VariationName := 'result := booltostr(StrToBool(''-1''));';
        utresult := booltostr(StrToBool('-1'));
      end;
      45: begin
        VariationName := 'result := booltostr(StrToBool(''true''));';
        utresult := booltostr(StrToBool('true'));
      end;
      46: begin
        VariationName := 'result := booltostr(StrToBool(''false''));';
        utresult := booltostr(StrToBool('false'));
      end;
      47: begin
        VariationName := 'result := booltostr(StrToBool(''TRUE''));';
        utresult := booltostr(StrToBool('TRUE'));
      end;
      48: begin
        VariationName := 'result := booltostr(StrToBool(''FALSE''));';
        utresult := booltostr(StrToBool('FALSE'));
      end;
      49: begin
        VariationName := 'result := booltostr(StrToBool(''YES''));';
        utresult := booltostr(StrToBool('YES'));
      end;
      50: begin
        VariationName := 'result := booltostr(StrToBool(''NO''));';
        utresult := booltostr(StrToBool('NO'));
      end;
      51: begin
        VariationName := 'result := booltostr(StrToBool(''affirmative''));';
        utresult := booltostr(StrToBool('affirmative'));
      end;
      60: begin
        VariationName := 'function BoolToStr() expecting: TRUE FALSE YES NO ';
        //function BoolToStr(b: boolean; sTrue: string ='TRUE'; sFalse:string = 'FALSE') : string;
        utresult := booltostr(true)+' '+booltostr(false)+' '+booltostr(true, 'YES', 'NO')+' '+booltostr(false, 'YES', 'NO');
      end;
      61: begin
        VariationName := 'function BoolToString(**variant function name for namespace issues) expecting: TRUE FALSE YES NO';
        utresult := booltostring(true)+' '+booltostring(false)+' '+booltostring(true, 'YES', 'NO')+' '+booltostring(false, 'YES', 'NO');
      end;
      70: begin
        VariationName := 'function GetFieldValue(expecting 1 1 3 2): string;';
        sl1.add('piss');sl2.Add('1');
        sl1.add('shit');sl2.Add('2');
        sl1.add('poop');sl2.Add('3');

        utresult := GetFieldValue(sl1,sl2, 'piss');
        utresult := utresult + ' '+ GetFieldValue(sl1,sl2, 'Piss');
        utresult := utresult + ' '+ GetFieldValue(sl1,sl2, 'poop');
        utresult := utresult + ' '+ GetFieldValue(sl1,sl2, 'shit');
        //function GetFieldValue(slFields: TStringlist; slRecord: TStringlist; sFieldName: string): string;
      end;
      80: begin
        VariationName := 'function  GetFieldIndex() expecting 0 0 2 1';
        sl1.add('piss');sl2.Add('1');
        sl1.add('shit');sl2.Add('2');
        sl1.add('poop');sl2.Add('3');

        utresult := inttostr(GetFieldIndex(sl1,'piss'));
        utresult := utresult + ' '+ inttostr(GetFieldIndex(sl1, 'Piss'));
        utresult := utresult + ' '+ inttostr(GetFieldIndex(sl1, 'poop'));
        utresult := utresult + ' '+ inttostr(GetFieldIndex(sl1, 'shit'));


        //function  GetFieldIndex(slFields: TStringlist; sFieldName: string): nativeint;
      end;
      90: begin
        VariationName := 'function IsolateDelimited(''a,b,c,dd,e,fgh'', 4, length(utresult)-1): string;';
        utresult := IsolateDelimited('a,b,c,dd,e,fgh', 4, length(utresult)-1);
      end;
      100: begin
        VariationName := 'function RemoveDuplicatesFromStringList(sl: TStringlist): boolean;';
        sl1.Add('apples');
        sl1.Add('Apples');
        sl1.Add('apples');
        sl1.Add('Oranges');
        sl1.Add('Oranges');
        sl1.Add('Grapes');
        utresult := booltostr(RemoveDuplicatesFromStringList(sl1));

        sTemp := sl1.text;
        sTemp := StringReplace(sTemp, #13, #10, [rfReplaceAll]);
        sTemp := StringReplace(sTemp, #10#10, #10, [rfReplaceAll]);
        sTemp := StringReplace(sTemp, #10#10, #10, [rfReplaceAll]);
        sTemp := StringReplace(sTemp, #10, ' ', [rfReplaceAll]);

        utresult := utresult + ':'+sTemp;
        //function RemoveDuplicatesFromStringList(sl: TStringlist): boolean;
      end;
      110: begin
        VariationName := 'function HTMLToPlainText(s: string): string;';
        utresult := HTMLToPlainText('<HTML><TITLE>Poop</Title><BODY>Your Body is Hot</BODY></HTML>');
        //function HTMLToPlainText(s: string): string;
      end;
      120: begin
        VariationName := 'function SplitStringIntoStringList(s: string; cDelimiter: char = '' ''; bRespectQuote: char = #0): TStringList;';
        sl1.free;
        sl1 := nil;
        sl1 := SplitStringIntoStringList('What the hell is going on "you jerk"', ' ', '"');

        sTemp := sl1.text;
        sTemp := StringReplace(sTemp, #13, #10, [rfReplaceAll]);
        sTemp := StringReplace(sTemp, #10#10, #10, [rfReplaceAll]);
        sTemp := StringReplace(sTemp, #10#10, #10, [rfReplaceAll]);
        sTemp := StringReplace(sTemp, #10, '[cr]', [rfReplaceAll]);

        utresult := sTemp;

        //function SplitStringIntoStringList(s: string; cDelimiter: char = ' '; bRespectQuote: char = #0): TStringList;
      end;
      130: begin
        VariationName := 'function ExtractLinksFromPlainText(s: string): string;';
        //function ExtractLinksFromPlainText(s: string): string;
        utresult := ExtractLinksFromPlainText('There is a link http://www.google.com/?wtf=isgoingon#123 somewhere in here.');

      end;
      140: begin
        VariationName := 'function RemoveDoubles(from aaabcaabbccaa where char is a): expecting abcabbcca';
        utresult := RemoveDoubles('aaabcaabbccaa', 'a');
        //function RemoveDoubles(s: string; c: char): string;
      end;
      150: begin
        VariationName := 'function StrSize(''hello''): integer;';
        utresult := inttostr(StrSize('hello'));
        //function StrSize(s: string): integer;
      end;
      160: begin
        VariationName := 'function StringTohex(abcdefg): expecting 61626364656667;';
        //function StringTohex(s: string): string;
        utresult := StringToHex('abcdefg');
      end;
      170: begin
        VariationName := 'function HextoString(61626364656667): expecting abcdefg;';
        utresult := HexToString(StringToHex('abcdefg'));

      end;
      180: begin
        VariationName := 'function DelimitIfNotEmpty(sInput: string; sDelimiter: string): string;';
        utresult := DelimitifnotEmpty('comma should follow', ',');

      end;
      190: begin
        VariationName := 'function Unquote("whatever"): string;';
        utresult :=  unquote('"whatever"');
        //function Unquote(s: string): string;
      end;
      200: begin
        VariationName := 'function StartsWith(sMaster: string; sStartsWith: string): boolean;';
        utresult := '"Hello world" starts with "hello" = '+booltostr(StartsWith('Hello World', 'Hello'));

        //function StartsWith(sMaster: string; sStartsWith: string): boolean;
      end;
      210: begin
        VariationName := 'function StartsWithThenSkip(var sMaster: string; sStartsWith: string): boolean;';
        sTemp := 'Hello World';
        if not StartsWithThenSkip(sTemp, 'Hello') then begin
          utresult := 'returned false';
        end else begin
          utresult:= sTemp;
        end;

      end;

      220: begin
        VariationName := 'function PairsToParams(): string;';
        sl1.Add('Param1=1234');
        sl1.add('Param2=45 67');
        sl1.add('Param3=890');
        utresult := PairsToParams(sl1.text);

        //function PairsToParams(sPairs: string): string;
      end;
      230: begin
        VariationName := 'function FloatPrecision(2.45, 4): string;';
        utresult := FloatPrecision(2.45, 4);
      end;
      231: begin
        VariationName := 'function FloatPrecision(2.45, 1): string;';
        utresult := FloatPrecision(2.45, 1);
      end;
      232: begin
        VariationName := 'function FloatPrecision(2.45, 0): string;';
        utresult := FloatPrecision(2.45, 0);
      end;
      233: begin
        VariationName := 'function FloatPrecision(2.45, 1): string;';
        utresult := FloatPrecision(-2.45, 1);
      end;
      234: begin
        VariationName := 'function FloatPrecision(2.45, 4): string;';
        utresult := FloatPrecision(-2.45, 4);
      end;
      235: begin
        VariationName := 'function FloatPrecision(2.45, 20): string;';
        utresult := FloatPrecision(2.45, 20);
      end;

      240: begin
        VariationName := 'procedure TrimStringList(); expecting [cr]a[cr][cr]b[cr]c[cr]';
        sl1.Add('');
        sl1.Add('a');
        sl1.Add('');
        sl1.Add('b');
        sl1.Add('c');
        sl1.Add('');
        sl1.Add('');
        TrimStringList(sl1);
        sTemp := sl1.text;
        sTemp := StringReplace(sTemp, #13, #10, [rfReplaceAll]);
        sTemp := StringReplace(sTemp, #10#10, #10, [rfReplaceAll]);
        sTemp := StringReplace(sTemp, #10#10, #10, [rfReplaceAll]);
        sTemp := StringReplace(sTemp, #10, '[cr]', [rfReplaceAll]);
        utresult := sTemp;
      end;
      250: begin
        VariationName := 'function IntToRank(1): string;';
        utresult := InttoRank(1);
        //function IntToRank(i: integer): string;
      end;
      251: begin
        VariationName := 'function IntToRank(0): string;';
        utresult := InttoRank(0);
        //function IntToRank(i: integer): string;
      end;
      252: begin
        VariationName := 'function IntToRank(2): string;';
        utresult := InttoRank(2);
        //function IntToRank(i: integer): string;
      end;
      253: begin
        VariationName := 'function IntToRank(3): string;';
        utresult := InttoRank(3);
        //function IntToRank(i: integer): string;
      end;
      254: begin
        VariationName := 'function IntToRank(4): string;';
        utresult := InttoRank(4);
        //function IntToRank(i: integer): string;
      end;
      255: begin
        VariationName := 'function IntToRank(5): string;';
        utresult := InttoRank(5);
        //function IntToRank(i: integer): string;
      end;
      256: begin
        VariationName := 'function IntToRank(6): string;';
        utresult := InttoRank(6);
        //function IntToRank(i: integer): string;
      end;
      257: begin
        VariationName := 'function IntToRank(7): string;';
        utresult := InttoRank(7);
        //function IntToRank(i: integer): string;
      end;
      258: begin
        VariationName := 'function IntToRank(8): string;';
        utresult := InttoRank(8);
        //function IntToRank(i: integer): string;
      end;
      259: begin
        VariationName := 'function IntToRank(9): string;';
        utresult := InttoRank(9);
        //function IntToRank(i: integer): string;
      end;
      260: begin
        VariationName := 'function IntToRank(999): string;';
        utresult := InttoRank(999);
        //function IntToRank(i: integer): string;
      end;
      261: begin
        VariationName := 'function IntToRank(222): string;';
        utresult := InttoRank(222);
        //function IntToRank(i: integer): string;
      end;
      262: begin
        VariationName := 'function IntToRank(-222): string;';
        utresult := InttoRank(-222);
        //function IntToRank(i: integer): string;
      end;
      270: begin
        VariationName := 'function MatchCase(Apple, apple): string;';
        utresult := MatchCase('Apple', 'apple');
        //function MatchCase(sSource, sTarget: string): string;
      end;
      271: begin
        VariationName := 'function MatchCase(ApPle, apple): string;';
        utresult := MatchCase('ApPle', 'apple');
        //function MatchCase(sSource, sTarget: string): string;
      end;
      272: begin
        VariationName := 'function MatchCase(Apple, aPPle): string;';
        utresult := MatchCase('Apple', 'aPPle');
        //function MatchCase(sSource, sTarget: string): string;
      end;
      273: begin
        VariationName := 'function MatchCase(blank, aPPle): string;';
        utresult := MatchCase('', 'aPPle');
        //function MatchCase(sSource, sTarget: string): string;
      end;
      274: begin
        VariationName := 'function MatchCase(Apple, blank): string;';
        utresult := MatchCase('Apple', '');
        //function MatchCase(sSource, sTarget: string): string;
      end;
      280: begin
        VariationName := 'utresult := PosFirst(''qwer<tyuiop>asdfg'',''<'',''>'')';
        utresult := inttostr(PosFirst('qwer<tyuiop>asdfg','<','>'));
      end;
      281: begin
        VariationName := 'utresult := PosFirst(''qwer>tyuiop<asdfg'',''<'',''>'')';
        utresult := inttostr(PosFirst('qwer>tyuiop<asdfg','<','>'));
      end;
      290: begin
        VariationName := 'utresult := PosFirst(''qwer<tyuiop>asdfg'',''<'',''>'',''|'')';
        utresult := inttostr(PosFirst('|qwer<tyuiop>asdfg','<','>'));
      end;
      291: begin
        VariationName := 'utresult := PosFirst(''qwer>tyuiop<asdfg'',''<'',''>'',''|'')';
        utresult := inttostr(PosFirst('qwer>tyu|iop<asdfg','<','>'));
      end;
      292: begin
        VariationName := 'utresult := PosFirst(''qwer>tyuiop<asdfg'',''<'',''>'',''|'')';
        utresult := inttostr(PosFirst('qwer>tyuiop<asdfg|','<','>'));
      end;
      293: begin
        VariationName := 'utresult := PosFirst(''qwer>tyuiop<asdfg'',''<'',''>'',''qwer'')';
        utresult := inttostr(PosFirst('qwer>tyuiop<asdfg|','<','>'));
      end;
      294: begin
        VariationName := 'utresult := PosFirst(''qwer>tyuiop<asdfg'',''tyu'',''>'',''|'')';
        utresult := inttostr(PosFirst('qwer>tyuiop<asdfg|','<','>'));
      end;

      300: begin
        VariationName := 'zsubcopy(''hello'', 0,3)';
        utresult := zsubcopy('hello', 0,3);
      end;
      301: begin
        VariationName := 'zsubcopy(''hello'', 1,3)';
        utresult := zsubcopy('hello', 1,3);
      end;
      302: begin
        VariationName := 'zsubcopy(''hello'', 4,1)';
        utresult := zsubcopy('hello', 4,1);
      end;
      303: begin
        VariationName := 'zsubcopy(''hello'', 0,10)';
        utresult := zsubcopy('hello', 0,10);
      end;
      304: begin
        VariationName := 'zsubcopy(''hello'', 5,1)';
        utresult := zsubcopy('hello', 5,1);
      end;

      310: begin
        VariationName := 'function zpos(''hell'', ''hello''): nativeint;';
        utresult := inttostr(zpos('hell', 'hello'));
        //function zpos(sSubString: string; sString: string): nativeint;
      end;
      311: begin
        VariationName := 'function zpos(''ell'', ''hello''): nativeint;';
        utresult := inttostr(zpos('ell', 'hello'));
        //function zpos(sSubString: string; sString: string): nativeint;
      end;
      312: begin
        VariationName := 'function zpos(''o'', ''hello''): nativeint;';
        utresult := inttostr(zpos('o', 'hello'));
        //function zpos(sSubString: string; sString: string): nativeint;
      end;
      313: begin
        VariationName := 'function zpos('''', ''hello''): nativeint;';
        utresult := inttostr(zpos('', 'hello'));
        //function zpos(sSubString: string; sString: string): nativeint;
      end;
      314: begin
        VariationName := 'function zpos(''nothere'', ''hello''): nativeint;';
        utresult := inttostr(zpos('nothere', 'hello'));
        //function zpos(sSubString: string; sString: string): nativeint;
      end;
      315: begin
        VariationName := 'function zpos(''no'', ''hello''): nativeint;';
        utresult := inttostr(zpos('no', 'hello'));
        //function zpos(sSubString: string; sString: string): nativeint;
      end;
      316: begin
        VariationName := 'function zpos(''hellos this is too big'', ''hello''): nativeint;';
        utresult := inttostr(zpos('hellos this is too big', 'hello'));
        //function zpos(sSubString: string; sString: string): nativeint;
      end;
      320: begin
        VariationName := 'function zlastpos(''hi'', ''hihi''): nativeint;';
        utresult := inttostr(zlastpos('hi','hihi'));
      end;
      321: begin
        VariationName := 'function zlastpos(''no'', ''hihi''): nativeint;';
        utresult := inttostr(zlastpos('no','hihi'));
      end;
      322: begin
        VariationName := 'function zlastpos(''i'', ''hihi''): nativeint;';
        utresult := inttostr(zlastpos('i','hihi'));
      end;
      323: begin
        VariationName := 'function zlastpos(''hihihi too big'', ''hihi''): nativeint;';
        utresult := inttostr(zlastpos('hi too big','hihi'));
      end;
      330: begin
        VariationName := 'function zcopy(''hello'', 2, 3): string;';
        utresult := zcopy('hello', 2,3);
      end;
      331: begin
        VariationName := 'function zcopy(''hello'', 2, 9): string;';
        utresult := zcopy('hello', 2,9);
      end;
      332: begin
        VariationName := 'function zcopy(''hello'', 0, 3): string;';
        utresult := zcopy('hello', 0,3);
      end;
      333: begin
        VariationName := 'function zcopy(''hello'', 7, 3): string;';
        utresult := zcopy('hello', 7,3);
      end;
      334: begin
        VariationName := 'function zcopy(''hello'', 4, 1): string;';
        utresult := zcopy('hello', 4,1);
      end;
      340..349: begin
        iOFF := Variation - 340;
        if iOFF < 5 then begin
          VAriationName := 'zsubisat(''i'', ''hihi'', '+inttostr(iOFF)+')';
          utresult := booltostr(zsubisatpos('i', 'hihi', iOFF));
        end;

      end;
      350..359: begin
        iOFF := Variation - 350;
        if iOFF < 5 then begin
          VAriationName := 'zsubisat(''hi'', ''hihi'', '+inttostr(iOFF)+')';
          utresult := booltostr(zsubisatpos('hi', 'hihi', iOFF));
        end;
      end;

      360..369: begin
        iOFF := Variation - 360;
        if iOFF < 5 then begin
          VAriationName := 'zsubisat(''hihihi''(too big), ''hihi'', '+inttostr(iOFF)+')';
          utresult := booltostr(zsubisatpos('hihihi', 'hihi', iOFF));
        end;
      end;
      370: begin
        VariationName := 'SplitString("stuff and things and stuff", " and ", sLeft, sRight, false)';
        if not SplitString('stuff and things and stuff', ' and ', sLeft, sRight) then begin
          utresult := 'returned false';
        end else begin
          utresult := 'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end;
      end;
      371: begin
        VariationName := 'SplitString("stuff and things and stuff", " and ", sLeft, sRight, true)';
        if not SplitString('stuff and things and stuff', ' and ', sLeft, sRight, true) then begin
          utresult := 'returned false';
        end else begin
          utresult := 'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end;
      end;
      372: begin
        VariationName := '2X SplitString("stuff and things and stuff", " and ", sLeft, sRight, false) (params 2 and 3 are same var in 2nd iter) sLeft, " and ", sLeft, sRight) ';
        if not SplitString('stuff and things and stuff', ' and ', sLeft, sRight) then begin
          utresult := 'returned false';
        end else begin
          if not SplitString(sRight, ' and ' , sLeft, sRight) then begin
            utresult := 'returned false on second iteration';
          end else begin
            utresult := 'After second iteration Left: "'+sLeft+'" Right1: "'+sRight+'"';
          end;
        end;
      end;
      373: begin
        VariationName := '2X SplitString("stuff and things and stuff", " and ", sLeft, sRight, false) (params2 and 3 are same var in 2nd iter) sLeft, " and ", sLeft, sRight) ';
        if not SplitString('stuff and things and stuff', ' and ', sLeft, sRight, true) then begin
          utresult := 'returned false';
        end else begin
          if not SplitString(sLeft, ' and ' , sLeft, sRight, true) then begin
            utresult := 'returned false on second iteration';
          end else begin
            utresult := 'After second iteration Left: "'+sLeft+'" Right1: "'+sRight+'"';
          end;
        end;
      end;
      374: begin
        VariationName := 'SplitString (not found, from left) ("stuff and things and stuff", " not found ", sLeft, sRight, false)';
        if SplitString('stuff and things and stuff', ' not found ', sLeft, sRight) then begin
          utresult := 'returned true, this was unexpected '+
                    'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end else begin
          utresult := 'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end;
      end;
      375: begin
        VariationName := 'SplitString (not found, from right) ("stuff and things and stuff", " not found ", sLeft, sRight, true)';
        if SplitString('stuff and things and stuff', ' not found ', sLeft, sRight, true) then begin
          utresult := 'returned true, this was unexpected '+
                    'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end else begin
          utresult := 'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end;
      end;
      380: begin
        VariationName := 'SplitStringNoCase("stuff and things and stuff", " AND ", sLeft, sRight, false)';
        if not SplitStringNoCase('stuff and things and stuff', ' AND ', sLeft, sRight) then begin
          utresult := 'returned false';
        end else begin
          utresult := 'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end;
      end;
      381: begin
        VariationName := 'SplitStringNoCase("stuff and things and stuff", " AND ", sLeft, sRight, true)';
        if not SplitStringNoCase('stuff and things and stuff', ' AND ', sLeft, sRight, true) then begin
          utresult := 'returned false';
        end else begin
          utresult := 'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end;
      end;
      382: begin
        VariationName := '2X SplitStringNoCase("stuff and things and stuff", " AND ", sLeft, sRight, false) (params 2 and 3 are same var in 2nd iter) sLeft, " and ", sLeft, sRight) ';
        if not SplitStringNoCase('stuff and things and stuff', ' AND ', sLeft, sRight) then begin
          utresult := 'returned false';
        end else begin
          if not SplitStringNoCase(sRight, ' AND ' , sLeft, sRight) then begin
            utresult := 'returned false on second iteration';
          end else begin
            utresult := 'After second iteration Left: "'+sLeft+'" Right1: "'+sRight+'"';
          end;
        end;
      end;
      383: begin
        VariationName := '2X SplitStringNoCase("stuff and things and stuff", " AND ", sLeft, sRight, false) (params2 and 3 are same var in 2nd iter) sLeft, " and ", sLeft, sRight) ';
        if not SplitStringNoCase('stuff and things and stuff', ' AND ', sLeft, sRight, true) then begin
          utresult := 'returned false';
        end else begin
          if not SplitStringNoCase(sLeft, ' AND ' , sLeft, sRight, true) then begin
            utresult := 'returned false on second iteration';
          end else begin
            utresult := 'After second iteration Left: "'+sLeft+'" Right1: "'+sRight+'"';
          end;
        end;
      end;
      384: begin
        VariationName := 'SplitStringNoCase (not found, from left) ("stuff and things and stuff", " not found ", sLeft, sRight, false)';
        if SplitStringNoCase('stuff and things and stuff', ' not found ', sLeft, sRight) then begin
          utresult := 'returned true, this was unexpected '+
                    'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end else begin
          utresult := 'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end;
      end;
      385: begin
        VariationName := 'SplitStringNoCase (not found, from right) ("stuff and things and stuff", " not found ", sLeft, sRight, true)';
        if SplitStringNoCase('stuff and things and stuff', ' not found ', sLeft, sRight, true) then begin
          utresult := 'returned true, this was unexpected '+
                    'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end else begin
          utresult := 'Left: "'+sLeft+'" Right: "'+sRight+'"';
        end;
      end;




{
      2: begin
        VariationName := 'function SplitStringNoCase(sRealSource, sSplitter: string; var sLeft, sRight: string; bStartAtRight: boolean = false): boolean; overload;';
        //function SplitStringNoCase(sRealSource, sSplitter: string; var sLeft, sRight: string; bStartAtRight: boolean = false): boolean; overload;
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function SplitStringNoCaseEx(sRealSource, sSplitter: string; var sLeft, sRight: string; notinLeft, notInRight: string; bStartAtRight: boolean = false): boolean; overload;';
        //function SplitStringNoCaseEx(sRealSource, sSplitter: string; var sLeft, sRight: string; notinLeft, notInRight: string; bStartAtRight: boolean = false): boolean; overload;
      end;
      2: begin
        VariationName := 'function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: string; var sLeft, sRight: string; notinLeft, notInRight: string; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;';
        //function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: string; var sLeft, sRight: string; notinLeft, notInRight: string; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;
      end;
      2: begin
        VariationName := 'function SplitStringNoCaseEx(sRealSource, sSplitter: string; var sLeft, sRight: string; notin: TSplitSet; bStartAtRight: boolean = false): boolean; overload;';
        //function SplitStringNoCaseEx(sRealSource, sSplitter: string; var sLeft, sRight: string; notin: TSplitSet; bStartAtRight: boolean = false): boolean; overload;
      end;
      2: begin
        VariationName := 'function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: string; var sLeft, sRight: string; notin: TSplitSet; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;';
        //function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: string; var sLeft, sRight: string; notin: TSplitSet; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function PcharToString(Pchar: Pchar; length: integer): string;';
        //function PcharToString(Pchar: Pchar; length: integer): string;
      end;
      2: begin
        VariationName := 'function ExpandMacAddress(sMac: string): string;';
        //function ExpandMacAddress(sMac: string): string;
      end;
      2: begin
        VariationName := 'procedure ReplicateStringListArea(sl: TStringList; iStartRow, iEndRow, iRepeatCount: integer; sDelimiters: string = '');';
        //procedure ReplicateStringListArea(sl: TStringList; iStartRow, iEndRow, iRepeatCount: integer; sDelimiters: string = '');
      end;
      2: begin
        VariationName := 'function StripPhone(sPhone: string): string;';
        //function StripPhone(sPhone: string): string;
      end;
      2: begin
        VariationName := 'function stringProgress(rPercent: real; length: integer; cap1: string = '['; cap2: string = ']'; bar: string = '|'; space: string = ' '): string;';
        //function stringProgress(rPercent: real; length: integer; cap1: string = '['; cap2: string = ']'; bar: string = '|'; space: string = ' '): string;
      end;
      2: begin
        VariationName := 'function CountLinesInTextFile(sFile: string): integer;';
        //function CountLinesInTextFile(sFile: string): integer;
      end;
      2: begin
        VariationName := 'function stringToDumbRAM(s: string): pointer;';
        //function stringToDumbRAM(s: string): pointer;
      end;
      2: begin
        VariationName := 'function  DumbRamToString(p: pointer): string;';
        //function  DumbRamToString(p: pointer): string;
      end;
      2: begin
        VariationName := 'procedure FreeDumbRamString(var p: PDumbRamString);';
        //procedure FreeDumbRamString(var p: PDumbRamString);
      end;
      2: begin
        VariationName := 'function BoolString(bCondition: boolean; sTrue: string=''; sFalse: string = ''): string;';
        //function BoolString(bCondition: boolean; sTrue: string=''; sFalse: string = ''): string;
      end;
      2: begin
        VariationName := 'function VarToMYSQLStorage(v: variant): string;';
        //function VarToMYSQLStorage(v: variant): string;
      end;
      2: begin
        VariationName := 'procedure SaveStringAsFile(sFile: string; data: string);';
        //procedure SaveStringAsFile(sFile: string; data: string);
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function LoadFileAsString(sFile: string; iLimitLength: integer = 0): string;';
        //function LoadFileAsString(sFile: string; iLimitLength: integer = 0): string;
      end;
      2: begin
        VariationName := 'function LoadStringFromFile(sFile: string; iLimitLength: integer = 0): string;';
        //function LoadStringFromFile(sFile: string; iLimitLength: integer = 0): string;
      end;
      2: begin
        VariationName := 'function SortStringCompare(s1,s2: string): integer;';
        //function SortStringCompare(s1,s2: string): integer;
      end;
      2: begin
        VariationName := 'function CleanString(s: string): string;';
        //function CleanString(s: string): string;
      end;
      2: begin
        VariationName := 'function CleanInjection(s: string): string;';
        //function CleanInjection(s: string): string;
      end;
      2: begin
        VariationName := 'function UnCleanInjection(s: string): string;';
        //function UnCleanInjection(s: string): string;
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function MakeThreadSafe(s: string): string;inline;';
        //function MakeThreadSafe(s: string): string;inline;
      end;
      2: begin
        VariationName := 'function HexEncode(s: string): string;';
        //function HexEncode(s: string): string;
      end;
      2: begin
        VariationName := 'function  stringToStringList(s: string): TStringList;';
        //function  stringToStringList(s: string): TStringList;
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function TrimStr(s: string): string;';
        //function TrimStr(s: string): string;
      end;
      2: begin
        VariationName := 'procedure CrackDelimitedString(const sLine : string; cDelimiter : char; slList : TStringList);';
        //procedure CrackDelimitedString(const sLine : string; cDelimiter : char; slList : TStringList);
      end;
      2: begin
        VariationName := 'function ExtractStr(src: string; what: string): string;';
        //function ExtractStr(src: string; what: string): string;
      end;
      2: begin
        VariationName := 'function LeftStr(s: string; n: integer): string;';
        //function LeftStr(s: string; n: integer): string;
      end;
      2: begin
        VariationName := 'function RightStr(s: string; n: integer): string;';
        //function RightStr(s: string; n: integer): string;
      end;
      2: begin
        VariationName := 'function MidStr(s: string; p: integer; n: integer): string;';
        //function MidStr(s: string; p: integer; n: integer): string;
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function StrExtract(src: string; what: string): string;';
        //function StrExtract(src: string; what: string): string;
      end;
      2: begin
        VariationName := 'function UnParseString(sDelimiter: string; sl: TStringList): string;';
        //function UnParseString(sDelimiter: string; sl: TStringList): string;
      end;
      2: begin
        VariationName := 'procedure ParseString(src: string; sDelimiter: string; slList: TStringList);overload;';
        //procedure ParseString(src: string; sDelimiter: string; slList: TStringList);overload;
      end;
      2: begin
        VariationName := 'procedure ParseString(src: string; sDelimiter: char; slList: TStringList);overload;';
        //procedure ParseString(src: string; sDelimiter: char; slList: TStringList);overload;
      end;
      2: begin
        VariationName := 'function ParseString(src: string; sDelimiter: char): TStringlist;overload;';
        //function ParseString(src: string; sDelimiter: char): TStringlist;overload;
      end;
      2: begin
        VariationName := 'function ParseString(src: string; sDelimiter: string): TStringlist;overload;';
        //function ParseString(src: string; sDelimiter: string): TStringlist;overload;
      end;
      2: begin
        VariationName := 'procedure ParseString2(src: string; sDelimiter: string; slList: TStringList);';
        //procedure ParseString2(src: string; sDelimiter: string; slList: TStringList);
      end;
      2: begin
        VariationName := 'procedure ParseNumericStringWithRanges(src: string; sDelimiter: string; slList: TStringList);';
        //procedure ParseNumericStringWithRanges(src: string; sDelimiter: string; slList: TStringList);
      end;
      2: begin
        VariationName := 'procedure ParseNumericRange(src: string; sDelimiter: string; slList: TStringList);';
        //procedure ParseNumericRange(src: string; sDelimiter: string; slList: TStringList);
      end;
      2: begin
        VariationName := 'function Quote(s: string): string;';
        //function Quote(s: string): string;
      end;
      2: begin
        VariationName := 'function CommaQuote(s: string): string;';
        //function CommaQuote(s: string): string;
      end;
      2: begin
        VariationName := 'function AdjustFolderPath(sPath: string): string;';
        //function AdjustFolderPath(sPath: string): string;
      end;
      2: begin
        VariationName := 'function AdjustFileName(sName, sExt: string): string;';
        //function AdjustFileName(sName, sExt: string): string;
      end;
      2: begin
        VariationName := 'function StripLineOfChar(src, s: string): string;';
        //function StripLineOfChar(src, s: string): string;
      end;
      2: begin
        VariationName := 'function LastPos(sub,s : string):integer;overload;';
        //function LastPos(sub,s : string):integer;overload;
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function SubStringExistsInString(sSrc, sWhat: string; var iPos: integer): boolean;';
        //function SubStringExistsInString(sSrc, sWhat: string; var iPos: integer): boolean;
      end;
      2: begin
        VariationName := 'function EditStringWithNewSubString(iPos: integer; sOld, sNew: string; var sSrc: string): boolean;';
        //function EditStringWithNewSubString(iPos: integer; sOld, sNew: string; var sSrc: string): boolean;
      end;
      2: begin
        VariationName := 'function PadString(sSource: string; sPadChar: char; iWidth: integer): string;';
        //function PadString(sSource: string; sPadChar: char; iWidth: integer): string;
      end;
      2: begin
        VariationName := 'function SplitQuote(sOrig: string; sQuoteDelimiter: string; var sLeft, sRight: string; out sInQuote: string): boolean;';
        //function SplitQuote(sOrig: string; sQuoteDelimiter: string; var sLeft, sRight: string; out sInQuote: string): boolean;
      end;
      2: begin
        VariationName := 'function SplitString(sSource: string; sSplitters: array of string; var sLeft, sRight: string; out sDelimiterUsed: string; NotIn: array of string; bStartAtRight: boolean = false): boolean; overload;';
        //function SplitString(sSource: string; sSplitters: array of string; var sLeft, sRight: string; out sDelimiterUsed: string; NotIn: array of string; bStartAtRight: boolean = false): boolean; overload;
      end;
      2: begin
        VariationName := 'function CropStringLR(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;';
        //function CropStringLR(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;
      end;
      2: begin
        VariationName := 'function CropStringRL(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;';
        //function CropStringRL(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;
      end;
      2: begin
        VariationName := 'function CropString(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;';
        //function CropString(sSource: string; sLeft: string; sRight: string; var sResult: string): boolean;
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function PosEx(sSubString, sMain: string; NotIn: array of string): integer;';
        //function PosEx(sSubString, sMain: string; NotIn: array of string): integer;
      end;
      2: begin
        VariationName := 'function LastPosEx(sSubString, sMain: string; NotIn: array of string): integer;';
        //function LastPosEx(sSubString, sMain: string; NotIn: array of string): integer;
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function BestPos(SubStrings: array of string; sMain: string; out sMatch: string; out position: integer; NotIn: array of string; bStartAtRight: boolean = false): boolean;';
        //function BestPos(SubStrings: array of string; sMain: string; out sMatch: string; out position: integer; NotIn: array of string; bStartAtRight: boolean = false): boolean;
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'procedure mergeStringLists(slmaster: tStringList; sInsert: string; iStartRow, iStartCol, iEndrow, iEndcol: integer);';
        //procedure mergeStringLists(slmaster: tStringList; sInsert: string; iStartRow, iStartCol, iEndrow, iEndcol: integer);
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'procedure DeleteStringListArea(sl: TStringList; iStartCol, iStartRow, iEndCol, iEndRow: integer; sReplace: string = '');';
        //procedure DeleteStringListArea(sl: TStringList; iStartCol, iStartRow, iEndCol, iEndRow: integer; sReplace: string = '');
      end;
      2: begin
        VariationName := '';
        //
      end;
      2: begin
        VariationName := 'function DatetoMYSQLDate(dt: TDateTime): string;';
        //function DatetoMYSQLDate(dt: TDateTime): string;
      end;
      2: begin
        VariationName := 'function Abbreviate(s: string): string;';
        //function Abbreviate(s: string): string;
      end;
      2: begin
        VariationName := 'function Scramble(s: string): string;';
        //function Scramble(s: string): string;
      end;
      2: begin
        VariationName := 'function Unscramble(s: string): string;';
        //function Unscramble(s: string): string;
      end;
}
    end;

  finally
    sl1.free;
    sl2.free;
  end;
end;



function CopyStringList(sl: TStringList): TStringList;
var
  t: integer;
  s: string;
begin
  result := TStringList.Create;
  result.Capacity := sl.Count;
  for t:= 0 to sl.Count-1 do begin
    s := sl[t];
//    UniqueString(s);
    result.Add(s);
  end;
end;

function StringsFromMemory(p: PByte; sz: nativeint): string;
var
  sl: TStringList;
begin
  sl := StringListFromNullTerminatedStrings(p, sz);
  try
    result := sl.Text;
  finally
    sl.Free;
  end;

end;


function StringListFromNullTerminatedStrings(p: PByte; sz: nativeint): TStringlist;
var
  sl: TStringlist;
  s: ansistring;
  t,st: integer;
begin
  sl := TStringlist.Create;
  result := sl;
  try
    st := STRZ();


{$IFDEF NEED_FAKE_ANSISTRING}
    s.SetLength(sz);
{$ELSE}
    setlength(s, sz);
{$ENDIF}

    for t:= 0 to sz-1 do begin
      if p[t] = 0 then begin
{$IFDEF NEED_FAKE_ANSISTRING}
        s.bytes[st] := p[t];
{$ELSE}
        s[st] := ansichar(p[t]);
{$ENDIF}
{$IFDEF NEED_FAKE_ANSISTRING}
        s.SetLength(st-1);
{$ELSE}
        setlength(s,st-1);
{$ENDIF}
        if s <> '' then
          sl.Add(s);
{$IFDEF NEED_FAKE_ANSISTRING}
        s.SetLength(st-1);
{$ELSE}
        setlength(s, sz);
{$ENDIF}
        st := STRZ();
      end else begin
{$IFDEF NEED_FAKE_ANSISTRING}
        s.bytes[st] := p[t];
{$ELSE}
        s[st] := ansichar(p[t]);
{$ENDIF}
        inc(st);
      end;
    end;


  finally


    //sl.Free;
  end;

end;

function StringRepeat(s: string; iCount: ni): string;
var
  t: ni;
begin
  result := '';
  setlength(result, length(s) * iCount);
  for t := 0 to iCount-1 do begin
    movemem32(@result[STRZ+(t*length(s))], @s[STRZ], length(s)*sizeof(char));
  end;


end;

function FriendlySizeName(i: int64): string;
var
  bNeg: boolean;
begin
  //PB EB ZB
  bNeg := i < 0;
  if bNeg then
    i := 0-i;

  result := inttostr(i)+' Bytes';
  if i >= (QUADRILLION) then begin
    result := FloatPrecision(i / (QUADRILLION), 3)+' PB';
  end else
  if i >= (TRILLION) then begin
    result := FloatPrecision(i / TRILLION, 3)+' TB';
  end else
  if i >= (BILLION) then begin
    result := FloatPrecision(i / BILLION, 3)+' GB';
  end else
  if i >= (MILLION) then begin
    result := FloatPrecision(i / MILLION, 3)+' MB';
    exit;
  end else
  if i >= (1000) then begin
    result := FloatPrecision(i / 1000, 3)+' KB';
  end;



  if bNeg then
    i := 0-i;







end;

function CleanupParentsInPath(sPath: string): string;
var
  sl: TStringlist;
  bOK: boolean;
  t: ni;
begin
  sl := TStringlist.create;
  try
    ParseString(sPath, '\', sl);
    bOK := false;
    while not bOK do begin
      bOK := true;
      for t := 0 to sl.count-1 do begin
        if sl[t] = '..' then begin
          sl.delete(t);
          if t>0 then
            sl.delete(t-1);
          bOk := false;
          break;
        end;
      end;
    end;
    result := UnParseString('\', sl);
  finally
    sl.free;
  end;
end;


procedure RemovePrefixFromStringList(sPrefix: string; sl: TStrings);
var
  t: ni;
begin
  for t:= sl.count-1 downto 0 do begin
    if lowercase(sl[t]).StartsWith(lowercase(sPrefix)) then begin
      sl.Delete(t);
    end;
  end;

end;

function ParseString_Quick(src: string; sDelimiter: string): TStringlist;overload;
var
  slTemp, slTemp2, sl: TStringlist;
  bDone: boolean;
  t: ni;
  s, s1,s2: string;
begin
  result := nil;

  slTemp := TStringlist.create;
  try
    slTemp2 := TStringlist.create;
    try
      slTemp.add(src);
      bDone := false;
      while not bDone do begin
        bDone := true;
        slTemp2.Clear;
        for t := 0 to slTemp.Count-1 do begin
          s := slTemp[t];
          if SplitString(s, sDelimiter, s1,s2, false, length(s) div 2) then begin
            slTemp2.add(s1);
            slTemp2.add(s2);
            bDone := false;
          end else begin
            slTemp2.Add(s1);            
          end;
        end;
        sl := slTemp2;
        slTemp2 := slTEmp;
        slTemp := sl;
      end;
      bDone := false;
      while not bDone do begin
        bDone := true;
        slTemp2.Clear;
        for t := 0 to slTemp.Count-1 do begin
          s := slTemp[t];
          if SplitString(s, sDelimiter, s1,s2) then begin
            slTemp2.add(s1);
            slTemp2.add(s2);
            bDone := false;
          end else begin
            slTemp2.Add(s1);            
          end;
        end;
        sl := slTemp2;
        slTemp2 := slTEmp;
        slTemp := sl;

        result := slTEmp;        

      end;
      

    finally
      slTemp2.free;
    end;


  finally
//    slTEmp.free;
  end;

end;

function its(i: int64): string; inline;overload;
begin
  result := inttostr(i);
end;
function its(i: integer): string; inline;overload;
begin
  result := inttostr(i);
end;


function zExtractDelimitedString(sSource: string; sDelimiter: string; var iStartAtZeroBased: ni; out sResult: string): boolean;
var
  i: ni;
begin
  if iStartAtZeroBased >= length(sSource) then begin
    result := false;
    exit;
  end;
  i := zpos(sDelimiter, sSource, iStartAtZeroBased);
  result := true;
  if not result then begin
    i := length(sSource);
    sResult := zcopy(sSource, iStartAtZeroBased, i - iStartAtZeroBased);;
    iStartAtZeroBased := length(sDelimiter);
  end else begin
    sResult := zcopy(sSource, iStartAtZeroBased, i - iStartAtZeroBased);
    iStartAtZeroBased := i + length(sDelimiter);
  end;

end;


function MemoryToString(p: pointer; l: ni): string;overload;
var
  s,ss: string;
  t,tt: ni;
begin
  setlength(s, (l*2));

  t := 0;
  tt := STRZ;
  for t:= 0 to l-1 do begin
    ss := inttohex(pbyte(p)[t], 2);

    s[tt] := ss[STRZ];
    inc(tt);
    s[tt] := ss[STRZ+1];
    inc(tt);

    if (t < (l-1)) then begin
//      s[tt] := '.';
//      inc(tt);
    end;


  end;

  result := s;

end;
function MemoryToString(m: TDynByteArray): string;overload;
begin
  result := MemoryToString(@m[0], length(m));
end;
function StringToMemory(s: string): TDynByteArray;
var
  ss: string;
  t,tt: ni;
begin
  t := STRZ;
  tt := 0;
  setlength(result, length(s) shr 1);
  if (length(s) and 1)=1 then
    raise ECritical.create('StringToMemory requires an even number of bytes');
  while t < (length(s)-STRZ) do
  begin
    result[tt] := strtoint('$'+s[t]+s[t+1]);
    inc(t,2);
    inc(tt);
  end;


end;



function paddr(o: TObject): string;inline;
begin
  result := '@'+inttohex(ni(pointer(o)),1);
end;

{ TStringListFixer }

function TStringListFixer.GetTrustyText: string;
begin
  result := text;
end;

procedure TStringListFixer.SetTrustyText(const Value: string);
var
  s,s1,s2: string;
begin
  s := value;
  s := StringReplace(s, #13#10, #10, [rfReplaceAll]);
  s := StringReplace(s, #13, #10, [rfReplaceAll]);
  s2 := s;
  clear;
  while SplitString(s2, #10, s1,s2) do begin
    add(s1);
  end;

  if s1 <> '' then
    add(s1);

end;

{ TStringsFixer }

function TStringsFixer.GetTrustyText: string;
begin
  result := text;
end;

procedure TStringsFixer.SetTrustyText(const Value: string);
var
  s,s1,s2: string;
begin
  s := value;
  s := StringReplace(s, #13#10, #10, [rfReplaceAll]);
  s := StringReplace(s, #13, #10, [rfReplaceAll]);
  s2 := s;
  clear;
  while SplitString(s2, #10, s1,s2) do begin
    add(s1);
  end;

  if s1 <> '' then
    add(s1);

end;

function ZFindSplitMidPOint(sPattern: string; sString: string;bIgnoreCase: boolean): nativeint;
var
  iMid: ni;
begin
  iMid := length(sString) shr 1;

  result := zpos(sPattern, sString, bIgnoreCase, iMid);

  //special case... if the result came back as being RIGHT ON the mid point, then
  // we might have split the string in the middle in cases where the pattern
  // we're looking for is repeating like ;; or ABAB or something...
  // therefore we'll recheck the result recursively until this is not the case
  while (result = iMid) and (iMid > 0) do begin
    dec(iMid, length(sPattern));
    iMid := greaterof(0,iMid);
    result := zpos(sPattern,sString, bIgnoreCase, iMid);
  end;


end;

function ZFindSplitMidPOint_SLOW(sPattern: string; sString: string;bIgnoreCase: boolean): nativeint;
var
  iMid: ni;
  iMinDelta: ni;
  iDelta: ni;
  iSearch: ni;
  iResult: ni;
  iLastResult: ni;
  iSearchPoint: ni;
  iLastSearchPOint: ni;
  delimiter_size: ni;
begin
  iMid := length(sSTring) shr 1;
  iMinDelta := length(sString);

  result := -1;
  iSearchPoint := 0;
  iLAstResult := -1;
  iLAstSearchPoint := -1;
  delimiter_size := Length(sPattern);
  while true do begin
    iResult := zpos(sPattern, sString, bIgnoreCase,iSearchPoint);
    //if nothing found then the result is the previous search
    if iResult < 0 then
      exit(iLastResult);
    //calc delta from center
    iDelta := abs(iMid-iResult);
    //if the delta increases, then we're past the mid point, so result was previous search
    if iDelta > iMinDelta then
      exit(iLastResult);

    //else move forward
    iLastResult := iResult;
    iLastSearchPOint := iSearchPOint;
    iMinDelta := lesserof(iDelta, iMinDelta);
    iSearchPoint := iResult + delimiter_size;


  end;







end;

procedure AppendTextToFile(sFile: string; sText: string);
var
  fs: TFileStream;
begin
  fs := nil;
  try
    if not fileexists(sFile) then
      fs := TFileStream.create(sFile, fmCReate)
    else
      fs := TFileStream.create(sFile, fmOpenReadWrite+fmShareExclusive);


    fs.Seek(0, soEnd);

    Stream_GuaranteeWrite(fs, @sText[strz], length(sText) * sizeof(char));



  finally
    fs.free;
    fs := nil;
  end;




end;

function IPToBytes(sIP: string): TDynByteArray;
var
  s1,s2: string;
begin
  s2 := sIP;
  setlength(result, 4);
  SplitString(s2, '.', s1,s2);
  result[0] := strtoint(s1);
  SplitString(s2, '.', s1,s2);
  result[1] := strtoint(s1);
  SplitString(s2, '.', s1,s2);
  result[2] := strtoint(s1);
  result[3] := strtoint(s2);
end;

function zpos(sSubString: string; sString: string;bIgnoreCase: boolean; iSTartAt:ni=0): nativeint;overload;
begin
  if bIgnoreCase then
    result := zpos_ignorecase(sSubString, sString, iStartAt)
  else
    result := zpos(sSubString, sString, iStartAt);

end;


function StrToFloatEx(s: string; nanresult: double = 0): double;
begin
  if lowercase(s)= 'nan' then
    result := nanresult
  else
    result := strtofloat(s);
end;


function BtxDateToDAteTime(sBTX: string): TDateTime;
var
  s: string;
  s1, s2: string;
  sYear,sMonth,sDay, shour, sMin, sSec: string;
  fs: TFormatSettings;
begin
  s := lowercase(sBTX);
  SplitString(s, 't', s1,s2);
  splitString(s1,'-', sYear, s1);
  splitString(s1,'-', sMonth, sDay);

  splitString(s2,':', sHour, s2);
  splitString(s2,':', sMin, sSec);


  result := StrToDate(sMonth+'/'+sDay+'/'+sYear);
  result := result + ((strtofloat(sSec)+(strtofloat(sMin)*60)+(strtofloat(sHour)*(60*60)))/(60*60*24))


end;

function OFirstPos(s: string; searchfor: array of string; bIgnoreCase: boolean = false; iOStartAt: ni = 1): TPosDetails;
begin
  result := ZFirstPos(s, searchfor, bIgnoreCase, iOStartAt-1);
  inc(result.position);
end;

function ZFirstPos(s: string; searchfor: array of string; bIgnoreCase: boolean = false; iZStartAt: ni = 0): TPosDetails;
var
  t: ni;
  thispos: TPosDetails;
begin
  result.search := '';
  result.position := -1;
  for t:= 0 to high(searchfor) do begin
    thispos.search := searchfor[t];
    thispos.position := zpos(thispos.search, s, bIgnoreCase, iZStartAT);
    if ((thispos.position >= 0) and ((thispos.position < result.position) or (result.position<0))) then begin
      result := thispos;
    end;
  end;


  if length(result.search) = 0 then begin
    result.leftof := s;
    result.rightof_including := s;
    result.rightof_excluding := s;
  end else begin
    result.leftof := zcopy(s, 0, result.position);
    result.rightof_including := zcopy(s, result.position, length(s));
    result.rightof_excluding := zcopy(s, result.position+length(result.search), length(s));
  end;

end;

function CountChar(s: string; c: char): ni;
var
  cc: char;
begin
  result := 0;
  for cc in s do
    if cc=c then inc(result);
end;

function StrIsBinary(s: string): boolean;
var
  sum, oob, ratio: single;
  t: ni;
begin
  sum := 0;
  oob := 0;
  for t:= STRZ to high(s) do begin
    if charinset(s[t], [#0,#1,#2,#3,#4,#5,#6,#7,#8]) then begin
      oob := oob + 1;
    end;
    sum := sum + 1;
  end;

  if sum = 0 then
    exit(false);
  ratio := oob/sum;
  result := ratio > 0.01;


end;

{$IFDEF MSWINDOWS}
function ordEX(c: ansichar): nativeint;overload;
begin
  result := ord(c);
end;
function ordEX(c: widechar): nativeint;overload;
begin
  result := ord(c);
end;
{$ENDIF}


function Stringlist_ValuesBlank(sl: TStringlist): boolean;
var
  t: ni;
begin
  result := true;
  for t:= 0 to sl.count-1 do begin
    if sl[t] <> '' then
      exit(false);
  end;

end;


initialization
  UTF.RegisterClass(TUT_StringX);

end.

