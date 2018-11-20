unit GraphicButton;

interface

uses
  controls, glasscontrols, graphics, sysutils, classes;


type
  TGraphicButtonState = (gbsRegular, gbsOver, gbsDown);

  TGraphicButton = class(TGlassImage)
  private
    FPictureOver: TPicture;
    FPictureDown: TPicture;
    FRegular: TPicture;
    FSTickDown: boolean;
    FOver: boolean;
    FDown: boolean;
    FDoOver: boolean;
    FinMouse: boolean;
    //property OnMouseEnter;
    //property OnMouseLeave;
    procedure LocalMouseEnter(sender: TObject);
    procedure LocalMouseLeave(sender: TObject);

    procedure SetPictureDown(const Value: TPicture);
    procedure SetPictureOver(const Value: TPicture);
    procedure SetState(const Value: TGraphicButtonState);
    procedure SetOVer(const Value: boolean);
    function GetState: TGraphicButtonState;
    procedure SetDown(const Value: boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click;override;

    procedure SetEnabled(value: boolean);override;
    procedure RefreshState;
  published
    constructor create(AOwner: TComponent);override;
    destructor Destroy;override;
    property PictureDown: TPicture read FPictureDown write SetPictureDown;
    property PictureOver: TPicture read FPictureOver write SetPictureOver;
    property State: TGraphicButtonState read GetState write SetSTate;
    property StickDown: boolean read FSTickDown write FStickDown;
    property Over: boolean read FOver write SetOVer;
    property Down: boolean read FDown write SetDown;
    property DoOver: boolean read FDoOver write FdoOver;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Digital Tundra', [TGraphicButton]);
end;



{ TGraphicButton }

procedure TGraphicButton.Click;
begin
  if stickdown then begin
    Down := not down;
  end else
    inherited;

end;

constructor TGraphicButton.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMouseEnter := self.LocalMouseEnter;
  OnMouseLeave := self.LocalMouseLeave;
  FPictureOver := TPicture.create;
  FPictureDown := TPicture.create;
  FRegular := TPicture.create;
  DoOver := true;
end;

destructor TGraphicButton.Destroy;
begin
  FPictureOver.free;
  FPictureDown.free;
  FRegular.free;

  inherited;
end;

function TGraphicButton.GetState: TGraphicButtonState;
begin
  if Down then begin
    result := gbsDown;
  end else
  if Over then begin
    result := gbsOver;
  end else
    result := gbsRegular;
end;

procedure TGraphicButton.LocalMouseEnter(sender: TObject);
begin
  if Doover and assigned(pictureOver.graphic) then
    Over := true;

end;

procedure TGraphicButton.LocalMouseLeave(sender: TObject);
begin
  if Doover and assigned(pictureOver.graphic) then
    Over := false;
end;

procedure TGraphicButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if StickDown then exit;
//  GLOG.Debug(self.Name+' in mouse down');

  FInMouse := true;
  try

    if StickDown then
      Down := not down
    else
      Down := true;
  finally
    FinMouse := false;
  end;
end;

procedure TGraphicButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if StickDown then exit;
//  GLOG.Debug(self.Name+' in mouse up');
  FInMouse := true;
  try


    if not StickDown then
      Down := false;

  finally
    FinMouse := false;
  end;


end;

procedure TGraphicButton.RefreshState;
begin
//  if FStickDown then
//    FState := gbsDown;
  if Enabled then begin
    case State of
      gbsRegular:
      begin
        if assigned(Picture.Graphic) then begin
          PictureToDraw := Picture;
          invalidate;
        end;
      end;
      gbsOver:
      begin
        if assigned(PictureOver.Graphic) and DoOver then begin
          PictureToDraw := PictureOver;
          invalidate;
        end;
      end;
      gbsDown:
      begin
        if assigned(PictureDown.Graphic) then begin
          PictureToDraw := PictureDown;
          invalidate;
        end;
      end;
    end;
  end else begin
    if assigned(PictureDown.Graphic) then
      PictureToDraw := PictureDown;
          invalidate;
  end;

end;

procedure TGraphicButton.SetDown(const Value: boolean);
begin
  if FDown = value then exit;

  FDown := Value;

  RefreshSTate;

//  if (value) then
//    GLOG.Debug(self.name+' Down')
//  else
//    GLOG.Debug(self.name+' Up');

  if (FDown or StickDown) and Assigned(Onclick) then begin
    if not FInMouse then begin
      OnClick(self);
    end;
  end;

end;

procedure TGraphicButton.SetEnabled(value: boolean);
begin
  inherited;
  RefreshState;
end;

procedure TGraphicButton.SetOVer(const Value: boolean);
begin
  FOver := Value;
  RefreshSTate;
end;

procedure TGraphicButton.SetPictureDown(const Value: TPicture);
begin
  FPictureDown.assign(value);
end;

procedure TGraphicButton.SetPictureOver(const Value: TPicture);
begin
  FPictureOver.assign(value);
end;

procedure TGraphicButton.SetState(const Value: TGraphicButtonState);
begin
  exit;//deprecated

//  if value <> FState then begin
//    FState := value;
//    RefreshState;
//  end;



end;

end.
