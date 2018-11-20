unit ComponentDataHelper;

interface

uses
  stdctrls, comctrls, classes, sysutils, glasscontrols;

function GetValueFromComponent(c: TComponent): ansistring;
procedure SetValueToComponent(c: TComponent; sValue: ansistring);

implementation

//------------------------------------------------------------------------------
function GetValueFromComponent(c: TComponent): ansistring;
begin
  if c is TEdit then with c as TEdit do begin
    result := text;
  end else
  if c is TGlassLabel then with c as TGlassLabel do begin
    result := caption;
  end else
  if c is TLabel then with c as TLabel do begin
    result := caption;
  end else
  if c is TCheckBox then with c as TCheckBox do begin
    if checked then
      result := '1'
    else
      result := '0';
  end else

  begin
    raise exception.create('Component type not handled in ComponentDataHelper');
  end;
end;

//------------------------------------------------------------------------------
procedure SetValueToComponent(c: TComponent; sValue: ansistring);
begin
  if c is TEdit then with c as TEdit do begin
    text := sValue;
  end else
  if c is TGlassLabel then with c as TGlassLabel do begin
    caption := sValue;
  end else
  if c is TLabel then with c as TLabel do begin
    caption := sValue;
  end else
  if c is TCheckBox then with c as TCheckBox do begin
    checked := strtobool(sValue);
  end else
  begin
    raise exception.create('Component type not handled in ComponentDataHelper');
  end;
end;



end.
