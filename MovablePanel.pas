unit MovablePanel;

interface

uses
  classes, controls, comctrls, extctrls, types;

type
  TMoveState = (msNotMoving, msMoving, msResizeLeft, msResizeRight, msResizeTop, msResizeBottom, msResizeTopLeft, msResizeTopRight, msResizeBottomLeft, msResizeBottomRight);

  TMovablePanel = class(TPanel)
  protected
    mousedownat: TPoint;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;

  public


  end;


implementation

end.
