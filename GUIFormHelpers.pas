unit GUIFormHelpers;

interface
uses forms, comctrls,stdctrls, extctrls;

procedure EnableTimers(frm: TForm; bEnabled: boolean);

implementation

procedure EnableTimers(frm: TForm; bEnabled: boolean);
var
  t: integer;
begin
  for t := 0 to frm.componentcount-1 do begin
    if frm.Components[t] is TTimer then
      TTimer(frm.Components[t]).Enabled := bEnabled;
  end;

end;

end.
