unit BetterPaintBox;
//wanted to add mouse wheel to paintbox, but I don't think it can be done
//windows messages cannot be handled here?!?
interface

uses
  controls, classes, extctrls,typex,types;


type
  TBetterPaintBox = class(TPaintBox)
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;override;
  end;

implementation

{ TBetterPaintBox }

procedure TBetterPaintBox.CMMouseWheel(var Message: TCMMouseWheel);
begin
//
end;

function TBetterPaintBox.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TBetterPaintBox.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

end.
