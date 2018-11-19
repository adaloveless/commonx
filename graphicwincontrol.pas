unit graphicwincontrol;

interface

uses
  debug, controls, stdctrls, comctrls, classes, typex, windows, messages, graphics;

type
  TGraphicWinControl = class(TCustomControl)
  private
    FOnFocus: TNotifyEvent;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);override;
  public
    CONSTRUCTOR create(aowner: TComponent);override;
    procedure Init;virtual;
    procedure SetFocus;override;
    property OnFocus: TNotifyEvent read FOnFocus write FonFocus;


  end;
  TUT_GWC = class(TGraphicWinControl)
  public
    procedure Paint;override;
  end;
implementation

{ TUT_GWC }

procedure TUT_GWC.Paint;
begin
  inherited;
  self.Canvas.Brush.color := clBlack;
  self.canvas.Rectangle(0,0,width,height);
  consolelog('paint gwc');
end;


constructor TGraphicWinControl.create(aowner: TComponent);
begin
  inherited;
  Init;
end;

procedure TGraphicWinControl.Init;
begin
//  raise Exception.create('unimplemented');

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TGraphicWinControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if not (csDesigning in ComponentState) and CanFocus then
    SetFocus;
end;

procedure TGraphicWinControl.SetFocus;
begin
  inherited;
  if assigned(FOnFocus) then
    FOnFocus(self);
//  invalidate;
end;

end.
