unit FMX.Better.Forms;

interface

uses FMX.Forms, fmx.types, fmx.controls, system.types, system.classes, system.UITypes, sysutils, fmx.graphics, fmx.platform;


type
  TCustomPopupForm = class (TCustomForm)
  private type
    TAniState =  (asNone, asShow, asClose);
  private
    FPlacement: TPlacement;
    [weak]FPlacementTarget: TControl;
    FOffset: TPointF;
    FSize: TSizeF;
    FPlacementRectangle: TBounds;
    FScreenPlacementRect: TRectF;
    FPlacementChanged: Boolean;
    FTimer: TTimer;
    FAniState: TAniState;
    FAniDuration: Single;
    FMaxAniPosition: Single;
    FAniPosition: Single;
    FShowTime: TDateTime;
    FCloseTime: TDateTime;
    FOnAniTimer: TNotifyEvent;
    FFirstShow: Boolean;
    FDragWithParent: Boolean;
    FBeforeClose: TNotifyEvent;
    FBeforeShow: TNotifyEvent;
    FScreenContentRect: TRectF;
    FContentPadding: TBounds;
    procedure SetOffset(const Value: TPointF);
    procedure SetSize(const Value: TSizeF);
    procedure SetPlacementRectangle(const Value: TBounds);
    procedure SetPlacement(const Value: TPlacement);
    procedure TimerProc(Sender: TObject);
    procedure SetPlacementTarget(const Value: TControl);
    procedure SetDragWithParent(const Value: Boolean);
    procedure SetContentPadding(const Value: TBounds);
  protected
    procedure DoBeforeShow; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoPaddingChanged; override;
    procedure DoApplyPlacement; virtual;
    procedure Loaded; override;
    procedure Updated; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoAniTimer; virtual;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: NativeInt = 0); override;
    constructor Create(AOwner: TComponent; AStyleBook: TStyleBook = nil; APlacementTarget: TControl = nil); reintroduce;
    destructor Destroy; override;
    procedure ApplyPlacement;
    function CanShow: Boolean; override;
    function CloseQuery: Boolean; override;
    property Placement: TPlacement read FPlacement write SetPlacement;
    property PlacementTarget: TControl read FPlacementTarget write SetPlacementTarget;
    property PlacementRectangle: TBounds read FPlacementRectangle write SetPlacementRectangle;
    property Offset: TPointF read FOffset write SetOffset;
    property ContentPadding: TBounds read FContentPadding write SetContentPadding;
    property ScreenContentRect: TRectF read FScreenContentRect;
    property ScreenPlacementRect: TRectF read FScreenPlacementRect;
    property DragWithParent: Boolean read FDragWithParent write SetDragWithParent;
    property Size: TSizeF read FSize write SetSize;
    property AniDuration: Single read FAniDuration write FAniDuration;
    property AniPosition: Single read FAniPosition;
    property OnAniTimer: TNotifyEvent read FOnAniTimer write FOnAniTimer;
    property BeforeShow: TNotifyEvent read FBeforeShow write FBeforeShow;
    property BeforeClose: TNotifyEvent read FBeforeClose write FBeforeClose;
  end;


implementation

type
  TBoundsPopupForm = class(TBounds)
  private
    FPopupForm: TCustomPopupForm;
  protected
    procedure DoChange; override;
  public
    constructor Create(APopupForm: TCustomPopupForm); reintroduce;
  end;




constructor TCustomPopupForm.Create(AOwner: TComponent; AStyleBook: TStyleBook;
  APlacementTarget: TControl);
var
  NewStyleBook: TStyleBook;
begin
  CreateNew(AOwner);
  BeginUpdate;
  try
    if Assigned(APlacementTarget) then
      FPlacementTarget := APlacementTarget
    else if (Owner is TControl) and (Assigned(TControl(Owner).ParentControl)) then
      FPlacementTarget := TControl(Owner).ParentControl;
    Parent := FPlacementTarget;
    if Assigned(FPlacementTarget) then
      TComponent(FPlacementTarget).FreeNotification(Self);
    if (not Assigned(AStyleBook)) and Assigned(PlacementTarget) and Assigned(PlacementTarget.Scene) then
      NewStyleBook := PlacementTarget.Scene.GetStyleBook
    else
      NewStyleBook := AStyleBook;
    SetStyleBookWithoutUpdate(NewStyleBook);
  finally
    EndUpdate;
  end;
end;


constructor TCustomPopupForm.CreateNew(AOwner: TComponent; Dummy: NativeInt);
  function FindUniqueFormName(const Name: string): string;
  var
    I: Integer;
  begin
    I := 0;
    Result := Name;
    while (FindGlobalComponent(Result) <> nil) or
          (Assigned(AOwner) and (AOwner.FindComponent(Result) <> nil)) do
    begin
      Inc(I);
      Result := Format('%s_%d', [Name, I]);
    end;
  end;
begin
  Name := FindUniqueFormName('CustomPopupForm');
  inherited;
  FDragWithParent := True;
  FPlacementRectangle := TBoundsPopupForm.Create(Self);
  FContentPadding := TBoundsPopupForm.Create(Self);
  FSize := TSizeF.Create(320, 200);
  FPlacement := TPlacement.plBottom;
  Visible := False;
  FormStyle := TFormStyle.fsPopup;
  Position := TFormPosition.poDesigned;
  BorderStyle := TFmxFormBorderStyle.bsNone;
  Fill.Kind := TBrushKind.bkNone;
  Transparency := True;
  Padding.DefaultValue := TRectF.Create(8, 8, 8, 8);
  Padding.Rect := Padding.DefaultValue;
end;

destructor TCustomPopupForm.Destroy;
begin
  FreeAndNil(FTimer);
  if Assigned(FPlacementTarget) then
  begin
    TComponent(FPlacementTarget).RemoveFreeNotification(Self);
    FPlacementTarget := nil;
  end;
  FreeAndNil(FContentPadding);
  FreeAndNil(FPlacementRectangle);
  inherited;
end;

procedure TCustomPopupForm.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction := TCloseAction.caFree;
  inherited;
  if CloseAction <> TCloseAction.caNone then
  begin
    if Assigned(FTimer) then
      FTimer.Enabled := False;
    SetStyleBookWithoutUpdate(nil);
  end;
end;

procedure TCustomPopupForm.DoPaddingChanged;
begin
  inherited;
  ApplyPlacement;
end;

procedure TCustomPopupForm.TimerProc(Sender: TObject);
var
  T, LAniDuration: Double;
begin
  if not Released then
  begin
    LAniDuration := AniDuration;
    if FAniState = TAniState.asShow then
    begin
      if LAniDuration <= 0 then
        FAniPosition := FMaxAniPosition
      else
      begin
        T := (Now - FShowTime) * SecsPerDay;
        if (T >= LAniDuration) then
          FAniPosition := FMaxAniPosition
        else
          FAniPosition := FMaxAniPosition * (T / LAniDuration);
      end;
      if FAniPosition >= FMaxAniPosition then
        FAniState := TAniState.asNone;
      if AniDuration >= 0 then
        DoAniTimer;
    end;
    if FAniState = TAniState.asClose then
    begin
      T := (Now - FCloseTime) * SecsPerDay;
      LAniDuration := LAniDuration * FMaxAniPosition;
      if (LAniDuration > 0) and (T < LAniDuration) then
        FAniPosition := FMaxAniPosition * (1 - (T / LAniDuration))
      else
        FAniPosition := 0;
      if AniDuration >= 0 then
        DoAniTimer;
      if FAniPosition <= 0 then
        Close;
    end;
  end;
  if (Visible or (FAniState = TAniState.asClose)) and (not Released) and (FFirstShow or FDragWithParent) then
    ApplyPlacement;
  if Visible and (not (FAniState = TAniState.asClose)) and (not Released) and (FFirstShow) then
  begin
    FFirstShow := False;
  end;
end;

function TCustomPopupForm.CanShow: Boolean;
begin
  Result := inherited CanShow;
  if Result then
  begin
    FFirstShow := True;
    FMaxAniPosition := 1;
    if AniDuration <= 0 then
      FAniPosition := FMaxAniPosition
    else
      FAniPosition := 0;
    FCloseTime := 0;
    FShowTime := Now;
    if (not (csDestroying in ComponentState)) and (not Released) then
      DoBeforeShow;
    if AniDuration >= 0 then
    begin
      FAniState := TAniState.asShow;
      DoAniTimer;
    end
    else
      FAniState := TAniState.asNone;
    if not Assigned(FTimer) then
      FTimer := TTimer.Create(Self);
    FTimer.Interval := 20;
    FTimer.OnTimer := TimerProc;
    FTimer.Enabled := True;
  end;
end;

function TCustomPopupForm.CloseQuery;
begin
  FCloseTime := Now;
  FMaxAniPosition := FAniPosition;
  if (AniDuration <= 0) or (FAniState = TAniState.asClose) then
  begin
    FAniPosition := 0;
    if AniDuration = 0 then
    begin
      FAniState := TAniState.asClose;
      DoAniTimer;
    end;
    if FAniState = TAniState.asClose then
      Result := True
    else
    begin
      Result := inherited CloseQuery;
      if Result and (not (csDestroying in ComponentState)) and (not Released) then
        DoBeforeClose;
    end;
    if not Result then
    begin
      FAniPosition := 1;
      FMaxAniPosition := FAniPosition;
      if AniDuration = 0 then
      begin
        FAniState := TAniState.asNone;
        DoAniTimer;
      end;
    end;
  end
  else
  begin
    if inherited CloseQuery then
    begin
      if (not (csDestroying in ComponentState)) and (not Released) then
        DoBeforeClose;
      FAniState := TAniState.asClose;
    end;
    Result := False;
  end;
end;

procedure TCustomPopupForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FPlacementTarget then
    begin
      FPlacementTarget := nil;
      ApplyPlacement;
    end;
  end;
end;

procedure TCustomPopupForm.SetDragWithParent(const Value: Boolean);
begin
  FDragWithParent := Value;
end;

procedure TCustomPopupForm.SetContentPadding(const Value: TBounds);
begin
  FContentPadding.Assign(Value);
end;

procedure TCustomPopupForm.SetOffset(const Value: TPointF);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    ApplyPlacement;
  end;
end;

procedure TCustomPopupForm.SetPlacement(const Value: TPlacement);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    ApplyPlacement;
  end;
end;

procedure TCustomPopupForm.SetPlacementRectangle(const Value: TBounds);
begin
  FPlacementRectangle.Assign(Value);
end;

procedure TCustomPopupForm.SetPlacementTarget(const Value: TControl);
begin
  if FPlacementTarget <> Value then
  begin
    if Assigned(FPlacementTarget) then
      TComponent(FPlacementTarget).RemoveFreeNotification(self);
    FPlacementTarget := Value;
    if Assigned(FPlacementTarget) then
      TComponent(FPlacementTarget).FreeNotification(Self);
  end;
end;

procedure TCustomPopupForm.SetSize(const Value: TSizeF);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    ApplyPlacement;
  end;
end;

procedure TCustomPopupForm.Loaded;
begin
  inherited;
  if FPlacementChanged then
    ApplyPlacement;
end;

procedure TCustomPopupForm.Updated;
begin
  inherited;
  if FPlacementChanged then
    ApplyPlacement;
end;

procedure TCustomPopupForm.DoAniTimer;
begin
  if Assigned(OnAniTimer) then
    OnAniTimer(self);
end;

procedure TCustomPopupForm.DoApplyPlacement;
var
  AbsolutePos: TPointF;
  Pos: TPointF;
  MouseSvc: IFMXMouseService;
  LRect: TRectF;
  LPlacement: TPlacement;
  LOffset: TPointF;
  LStep: Byte;
  LSoGood: Boolean;
  function UpdateRectByScreen(var R: TRectF): Boolean;
  var
    LScreenSize: TSize;
    W, H: Single;
  begin
    Result := True;
    R.Left := Round(R.Left);
    R.Top := Round(R.Top);
    R.Width := Round(R.Width);
    R.Height := Round(R.Height);
    R := Padding.PaddingRect(R);
    LScreenSize := Screen.Size;
    if (not(csDesigning in ComponentState)) and (LScreenSize.cx > 0) and (LScreenSize.cy > 0) then
    begin
      LScreenSize.cx := Round(LScreenSize.cx + ContentPadding.Right);
      LScreenSize.cy := Round(LScreenSize.cy + ContentPadding.Bottom);
      W := R.Width;
      H := R.Height;
//      R.Left := Max(R.Left, -ContentPadding.Left);
//      R.Top := Max(R.Top, -ContentPadding.Top);
      if R.Left < LScreenSize.cx then
      begin
        if R.Left > LScreenSize.cx - W then
        begin
          R.Left := LScreenSize.cx - W;
          if LPlacement = TPlacement.plRight then
          begin
            LPlacement := TPlacement.plLeft;
            Result := False;
          end;
        end;
      end;
      if R.Top < LScreenSize.cy then
      begin
        if R.Top > LScreenSize.cy - H then
        begin
          R.Top := LScreenSize.cy - H;
          if LPlacement = TPlacement.plBottom then
          begin
            LPlacement := TPlacement.plTop;
            Result := False;
          end;
        end;
      end;
      R.Width := W;
      R.Height := H;
    end;
    R := Padding.MarginRect(R);
  end;
begin
  FPlacementChanged := False;
  LOffset := Offset;
  LPlacement := Placement;
  LStep := 0;
  repeat
    LRect := FPlacementRectangle.Rect;
    if Assigned(PlacementTarget) and (FPlacementRectangle.Empty) then
      LRect := TRectF.Create(0, 0, PlacementTarget.Width, PlacementTarget.Height);
    if (not Assigned(PlacementTarget)) and
       (PlacementRectangle.Empty) and
       (not(LPlacement in [TPlacement.plAbsolute, TPlacement.plMouse, TPlacement.plMouseCenter])) then
    begin
      if not TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, IInterface(MouseSvc)) then
        LPlacement := TPlacement.plAbsolute
      else
        LPlacement := TPlacement.plMouse;
    end;
    FScreenPlacementRect := LRect;
    // Vertical Offset
    if LPlacement in [TPlacement.plTop, TPlacement.plTopCenter] then
      LRect.Offset(0, ContentPadding.Bottom - LOffset.Y)
    else
      LRect.Offset(0, LOffset.Y - ContentPadding.Top);
    // Horizontal Offset
    if LPlacement in [TPlacement.plLeft, TPlacement.plLeftCenter] then
      LRect.Offset(ContentPadding.Right - LOffset.X, 0)
    else
      LRect.Offset(LOffset.X - ContentPadding.Left, 0);
    // Offset by rect
    case LPlacement of
      TPlacement.plBottom:
        LRect.Offset(0, LRect.Height);
      TPlacement.plTop:
        LRect.Offset(0, -Size.cy);
      TPlacement.plLeft:
        LRect.Offset(-Size.cx, 0);
      TPlacement.plRight:
        LRect.Offset(LRect.Width, 0);
      TPlacement.plCenter:
        LRect.Offset((LRect.Width - Size.cx) / 2, (LRect.Height - Size.cy) / 2);
      TPlacement.plBottomCenter:
        LRect.Offset((LRect.Width - Size.cx) / 2, LRect.Height);
      TPlacement.plTopCenter:
        LRect.Offset((LRect.Width - Size.cx) / 2, -Size.cy);
      TPlacement.plLeftCenter:
        LRect.Offset(-Size.cx, (LRect.Height - Size.cy) / 2);
      TPlacement.plRightCenter:
        LRect.Offset(LRect.Width, (LRect.Height - Size.cy) / 2);
      TPlacement.plAbsolute:
        begin
          if FPlacementRectangle.Empty then
            LRect := TRectF.Create(FPlacementRectangle.Rect.TopLeft, Size.cx, Size.cy)
          else
            LRect := FPlacementRectangle.Rect;
        end;
      TPlacement.plMouse, TPlacement.plMouseCenter:
        begin
          Pos := Screen.MousePos;
          LRect := TRectF.Create(Pos, Size.cx, Size.cy);
          if LPlacement = TPlacement.plMouseCenter then
            LRect.Offset(-Size.cx / 2, -Size.cy / 2);
        end;
    end;
    // use border
    LRect := Padding.MarginRect(LRect);
    if (not(LPlacement in [TPlacement.plAbsolute, TPlacement.plMouse, TPlacement.plMouseCenter])) then
    begin
      AbsolutePos := LRect.TopLeft;
      AbsolutePos.Offset(Padding.Left, Padding.Top);
      if Assigned(PlacementTarget) then
      begin
        AbsolutePos := PlacementTarget.LocalToAbsolute(AbsolutePos);
        FScreenPlacementRect.TopLeft := PlacementTarget.LocalToAbsolute(FScreenPlacementRect.TopLeft);
        FScreenPlacementRect.BottomRight := PlacementTarget.LocalToAbsolute(FScreenPlacementRect.BottomRight);
        if Assigned(PlacementTarget.Scene) then
        begin
          AbsolutePos := PlacementTarget.Scene.LocalToScreen(AbsolutePos);
          FScreenPlacementRect.TopLeft := PlacementTarget.Scene.LocalToScreen(FScreenPlacementRect.TopLeft);
          FScreenPlacementRect.BottomRight := PlacementTarget.Scene.LocalToScreen(FScreenPlacementRect.BottomRight);
        end;
      end;
      LRect := TRectF.Create(AbsolutePos, Size.cx, Size.cy);
      LRect := Padding.MarginRect(LRect);
    end;
    LSoGood := UpdateRectByScreen(LRect);
    Inc(LStep);
  until LSoGood or (LStep > 1);
  FScreenContentRect := LRect;
  FScreenContentRect := inherited Padding.PaddingRect(FScreenContentRect);
  FScreenContentRect := FContentPadding.PaddingRect(FScreenContentRect);
  SetBounds(Round(LRect.Left), Round(LRect.Top), Round(LRect.Width), Round(LRect.Height));
end;

procedure TCustomPopupForm.DoBeforeClose;
begin
  if Assigned(BeforeClose) then
    BeforeClose(self);
end;

procedure TCustomPopupForm.DoBeforeShow;
begin
  if Assigned(BeforeShow) then
    BeforeShow(self);
end;

procedure TCustomPopupForm.ApplyPlacement;
begin
  if (([csLoading, csUpdating, csDestroying] * ComponentState) = []) and (not Released) then
    DoApplyPlacement
  else
    FPlacementChanged := True;
end;


  { TBoundsPopupForm }

constructor TBoundsPopupForm.Create(APopupForm: TCustomPopupForm);
begin
  inherited Create(TRectF.Create(0, 0, 0, 0));
  FPopupForm := APopupForm;
end;

procedure TBoundsPopupForm.DoChange;
begin
  inherited;
  if Assigned(FPopupForm) then
    FPopupForm.ApplyPlacement;
end;


end.
