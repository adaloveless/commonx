unit FlowGrowLayout;

interface

uses
  fmx.types, FMX.Layouts, fmx.controls, numbers, debug, stringx, fmx.forms, fmx.objects, guihelpers_fmx;

type
  TFlowGrowLayout = class(TFlowLayout)
  public
    procedure AutoSizeVertical;

  end;


implementation

{ TFlowGrowLayout }

procedure TFlowGrowLayout.AutoSizeVertical;
begin
  if self.parent = nil then
    exit;
  var bottom :single := 0.0;
  for var t := 0 to Childrencount-1 do begin
    var c := children[t];
    if c is TControl then begin
      var cc := c as TControl;
      bottom := greaterof(bottom, cc.position.y + cc.Height);
    end;
  end;

  self.width := Control_GetWidth(self.parent);
  self.position.X := 0;
  self.Position.y := 0;
  self.Height := bottom;


end;

end.
