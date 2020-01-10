unit commandline;


interface

uses
  sysutils, typex, stringx, classes, betterobject;

type
  TNameValue = record
    name, value: string;
  end;
  TCommandLine = record
  private
    FIgnoreCase: boolean;
    FNamedParams: array of TNameValue;
    FUnNamedParams: array of string;
    procedure AddUnnamed(sValue: string);
    procedure AddNamed(sName, sValue: string);
  public
    function GetNamedParamByIdx(idx: ni): TNameValue;
    procedure ParseCommandLine(sCmdLine: string = '');
        property IgnoreCase: boolean read FIgnoreCase write FIgnoreCase;


    //vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    function HasFlag(sFlag: string): boolean;
    function HasFlagEx(sShortFlag, sLongFlag: string): boolean;
    function GetUnnamedParameter(idx: ni; sDefault: string = ''): string;
    function GetNamedParameterEx(sShortFlag, sLongFlag: string; sDefault: string = ''): string;
    //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    function UnnamedCount: ni;
    function NamedCount: ni;

  end;




implementation

{ TCommandLine }

procedure TCommandLine.AddNamed(sName, sValue: string);
begin
  SetLength(FNamedParams, length(FnamedParams)+1);
  FNamedParams[high(FNamedParams)].value := sValue;
  FNamedParams[high(FNamedParams)].name := sName;
end;

procedure TCommandLine.AddUnnamed(sValue: string);
begin
  SetLength(FUnNamedParams, length(FUnnamedParams)+1);
  FUnNamedParams[high(FUnNamedParams)] := sValue;
end;

function TCommandLine.GetNamedParameterEx(sShortFlag, sLongFlag,
  sDefault: string): string;
begin
  result := sDefault;
  for var t := 0 to high(FNamedParams) do begin
    IF IgnoreCase THEN begin
      if CompareText(FNamedParams[t].name, sShortFlag)=0 then
        exit(FNamedParams[t].value);
      if CompareText(FNamedParams[t].name, sLongFlag)=0 then
        exit(FNamedParams[t].value);
    end else begin
      if CompareStr(FNamedParams[t].name, sShortFlag)=0 then
        exit(FNamedParams[t].value);
      if CompareStr(FNamedParams[t].name, sLongFlag)=0 then
        exit(FNamedParams[t].value);
    end;
  end;
end;

function TCommandLine.GetNamedParamByIdx(idx: ni): TNameValue;
begin
  result := FNamedParams[idx];
end;

function TCommandLine.GetUnnamedParameter(idx: ni; sDefault: string): string;
begin
  if idx < 0 then
    exit(sDefault);

  if idx > high(FUnnamedParams) then
    exit(sDefault);

  result := FUnnamedParams[idx];

end;

function TCommandLine.HasFlag(sFlag: string): boolean;
begin
  result := false;
  for var t := 0 to high(FNamedParams) do begin
    IF IgnoreCase THEN begin
      if CompareText(FNamedParams[t].name, sFlag)=0 then
        exit(true);
    end else begin
      if CompareStr(FNamedParams[t].name, sFlag)=0 then
        exit(true);
    end;
  end;

end;

function TCommandLine.HasFlagEx(sShortFlag, sLongFlag: string): boolean;
begin
  result := false;
  if HasFlag(sShortFlag) or HasFlag(sLongFlag) then
    exit(true);

end;

function TCommandLine.NamedCount: ni;
begin
  result:= length(FNamedParams);
end;

procedure TCommandLine.ParseCommandLine(sCmdLine: string);
var
  h: IHolder<TStringList>;
begin
{$IFDEF MSWINDOWS}
  if sCmdLine = '' then
    sCmdLine := CmdLine;
{$ENDIF}

  h := ParseStringNotInH(sCmdLine, ' ', '"');

  for var t:= 1 to h.o.count-1 do begin
    var sParam := h.o[t];
    if trim(sParam) = '' then
      continue;
    if copy(trim(sParam), STRZ, 1) = '-' then begin
      var l,r: string;
      SplitString(sParam,'=', l,r);
      AddNamed(trim(l), unquote(trim(r)));
    end else begin
      AddUnnamed(unquote(trim(sParam)));
    end;
  end;
end;

function TCommandLine.UnnamedCount: ni;
begin
  result := Length(FUnnamedParams);


end;

end.
