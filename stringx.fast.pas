unit stringx.fast;
{$INLINE AUTO}
{$O+}
interface


uses
  typex, faststrings, numbers, systemx;

function SplitString(sSource: faststring; sSplitter: faststring; var sLeft, sRight: faststring; bStartAtRight: boolean = false): boolean; overload;
function ZSubCopy(sSource: faststring; iPOs: nativeint; iLength: nativeint): faststring;
function zpos(sSubString: faststring; sString: faststring): nativeint;
function zsubisatpos(sSub: faststring; sString: faststring; idx: nativeint): boolean;
function zlastpos(sSubString: faststring; sString: faststring): nativeint;
function zcopy(sString: faststring; iStartZeroBased: nativeint; iLength: nativeint): faststring;

implementation

function SplitString(sSource: faststring; sSplitter: faststring; var sLeft, sRight: faststring; bStartAtRight: boolean = false): boolean; overload;
var
  iPos : nativeint;
begin
//  sLeft := '';
//  sRight := '';

  //find position of splitter
  if bStartAtRight then
    iPos:= zlastpos(sSplitter, sSource)
  else
    iPos := zpos(sSplitter, sSource);

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


function ZSubCopy(sSource: faststring; iPOs: nativeint; iLength: nativeint): faststring;
var
  t: nativeint;
begin
//  result := '';
  result.SetLength(LesserOf(length(sSource)-(iPos), iLength));
  for t := 0 to length(result)-1 do begin
    result[t+STRZ] := sSource[(t+STRZ)+iPos];
  end;
end;


function zsubisatpos(sSub: faststring; sString: faststring; idx: nativeint): boolean;
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
function zpos(sSubString: faststring; sString: faststring): nativeint;
var
  t: nativeint;
begin
  result := -1;
  if sSubString = '' then
    result := -1;

  for t:= 0 to length(sString)-length(sSubString) do begin
    if zsubisatpos(sSubString, sString, t) then begin
      result := t;
      exit;
    end;
  end;

end;
function zlastpos(sSubString: faststring; sString: faststring): nativeint;
var
  t: nativeint;
begin
  result := -1;
  if sSubString = '' then begin
    result := -1;
    exit;
  end;

  for t:= length(sString)-length(sSubString) downto 0 do begin
    if zsubisatpos(sSubString, sString, t) then begin
      result := t;
      exit;
    end;
  end;
end;

function zcopy(sString: faststring; iStartZeroBased: nativeint; iLength: nativeint): faststring;
var
  t,u: nativeint;
begin
  //result := '';

  result.setlength(lesserof(iLength, length(sString)-iStartZerobased));

  u := iStartZeroBased+STRZ;
//  for t:= STRZ to (result.length-1)+STRZ do begin
//    result.chars[t] := sString.chars[u];
//    inc(u);
//  end;


  movemem32(result.charaddr[strz], sString.charaddr[(strz+iStartZeroBased)], length(result)*sizeof(char));
end;



end.
