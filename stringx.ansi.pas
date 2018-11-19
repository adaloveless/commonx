unit stringx.ansi;
{$I 'DelphiDefs.inc'}
interface

uses
{$IFDEF NEED_FAKE_ANSISTRING}
  ios.stringx.iosansi,
{$ENDIF}
  classes, sysutils, typex, numbers;

type
  TSplitSet = array of string;
{$IFDEF NEED_FAKE_ANSISTRING}
  ansistring = ios.stringx.iosansi.ansistring;
  ansichar = TIOSAnsiChar;
  PAnsiChar = Pbyte;
{$ENDIF}

{$IFNDEF WINDOWS}
procedure SetLength(s: ansistring; len: nativeint);
{$ENDIF}
function StringListFromNullTerminatedStrings(p: PByte; sz: nativeint): TStringlist;
function LastPos(sub,s : ansistring):integer;overload;
function SplitStringNoCaseEx(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; notinLeft, notInRight: ansistring; bStartAtRight: boolean = false): boolean; overload;
function SplitString(const sSource: ansistring; const sSplitter: ansistring; var sLeft, sRight: ansistring; bStartAtRight: boolean = false): boolean; overload;
function SplitStringNoCase(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; bStartAtRight: boolean = false): boolean; overload;
function SplitStringNoCaseEx(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; notin: TSplitSet; bStartAtRight: boolean = false): boolean; overload;
function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; notinLeft, notInRight: ansistring; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;
function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; notin: TSplitSet; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;
function StringsFromMemory(p: PByte; sz: nativeint): string;
function SplitStringNoCaseEx(sRealSource, sSplitter: string; var sLeft, sRight: string; notin: TSplitSet; bStartAtRight: boolean = false): boolean; overload;
function ZSubCopy(sSource: ansistring; iPOs: nativeint; iLength: nativeint): ansistring;


implementation

uses
  stringx;

function LastPos(sub,s : ansistring):integer;overload;
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


function SplitString(const sSource: ansistring; const sSplitter: ansistring; var sLeft, sRight: ansistring; bStartAtRight: boolean = false): boolean; overload;
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
end;


function SplitStringNoCaseEx(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; notinLeft, notInRight: ansistring; bStartAtRight: boolean = false): boolean; overload;
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
  sSource: ansistring;
begin

  sSource := ansistring(lowercase(sRealSource));
  sSplitter := ansistring(lowercase(sSplitter));

  //find position of splitter
  if bStartAtRight then
    iPos:= lastposex(string(sSplitter), string(sSource), [notinleft, notinright])
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

function SplitStringNoCase(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; bStartAtRight: boolean = false): boolean; overload;
begin
  result := SplitStringNoCaseEx(sRealSource, sSplitter, sLeft, sRight, '','', bStartAtRight);
end;


function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; notinLeft, notInRight: ansistring; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;
begin
  result := SplitStringNoCaseEx(sRealSource, sSplitter, sLEft, sRight, notinLeft, notinRight, bStartAtRight);
  if result then begin
    if bPutDelimiterInRight then
      sRight := sSplitter+sRight
    else
      sLEft := sLeft + sSplitter;
  end;


end;

function SplitStringNoCaseExKeepSplit(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; notin: TSplitSet; bStartAtRight: boolean = false; bPutDelimiterInRight: boolean = true): boolean; overload;
begin
  result := SplitStringNoCaseEx(sRealSource, sSplitter, sLEft, sRight, notin,bStartAtRight);
  if result then begin
    if bPutDelimiterInRight then
      sRight := sSplitter+sRight
    else
      sLEft := sLeft + sSplitter;
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

function SplitStringNoCaseEx(sRealSource, sSplitter: ansistring; var sLeft, sRight: ansistring; notin: TSplitSet; bStartAtRight: boolean = false): boolean; overload;
var
  iPos : integer;
  sSource: ansistring;
begin

  sSource := ansistring(lowercase(sRealSource));
  sSplitter := ansistring(lowercase(sSplitter));

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
{$IFNDEF WINDOWS}
procedure SetLength(s: ansistring; len: nativeint);
begin
  s.SetLength(len);
end;
{$ENDIF}

function ZSubCopy(sSource: ansistring; iPOs: nativeint; iLength: nativeint): ansistring;
var
  t: nativeint;
begin
  SetLength(result, LesserOf(length(sSource)-(iPos), iLength));
  for t := 0 to length(result)-1 do begin
{$IFDEF NEED_FAKE_ANSISTRING}
    result.chars[t+STRZ] := sSource.chars[(t+STRZ)+iPos];
{$ELSE}
    result[t+low(result)] := sSource[(t+low(result))+iPos];
{$ENDIF}
  end;
end;


end.
