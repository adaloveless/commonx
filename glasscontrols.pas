unit GlassControls;

interface

uses winapi.Messages, winapi.Windows, SysUtils, Classes, Contnrs, vcl.imaging.pngimage, easyimage,
  Controls, Forms, Menus, Graphics, StdCtrls, GraphUtil, winapi.ShellApi, extctrls, beeper, gdipobj, gdipapi, graphicwincontrol;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TGlassImage = class (TGraphicWinControl)
  private
    FPicture: TPicture;
    FOnProgress: TProgressEvent;
    FStretch: Boolean;
    FCenter: Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FProportional: Boolean;
    FPicturetoDraw: TPicture;
    function GetCanvas: TCanvas;
    procedure PictureChanged(Sender: TObject);
    procedure SetCenter(Value: Boolean);
    procedure SetPicture(Value: TPicture);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    procedure SetPictureToDraw(const Value: TPicture);
  protected
    FDrawing: Boolean;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DestRect: TRect;
    function DoPaletteChange: Boolean;
    function GetPalette: HPALETTE; override;
    procedure Paint;override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    property PictureToDraw: TPicture read FPicturetoDraw write SetPictureToDraw;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
  published

    property Align;
    property Anchors;
    property AutoSize;
    property Center: Boolean read FCenter write SetCenter default False;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay default False;
    property ParentShowHint;
    property Picture: TPicture read FPicture write SetPicture;
    property PopupMenu;
    property Proportional: Boolean read FProportional write SetProportional default false;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnStartDock;
    property OnStartDrag;
  end;

  TCustomGlassLabel = class(TGraphicControl)
  private
    FFocusControl: TWinControl;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FLayout: TTextLayout;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    FTransparentSet: Boolean;
    FEllipsisPosition: TEllipsisPosition;
    function GetTransparent: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetEllipsisPosition(Value: TEllipsisPosition);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetLayout(Value: TTextLayout);
    procedure SetWordWrap(Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure AdjustBounds; dynamic;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); dynamic;
    function GetLabelText: string; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); override;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property EllipsisPosition: TEllipsisPosition read FEllipsisPosition write SetEllipsisPosition default epNone;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property Transparent: Boolean read GetTransparent write SetTransparent stored FTransparentSet;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  public
    constructor Create(AOwner: TComponent); override;
    property Caption;
    property Canvas;
  end;

  TGlassLabel = class(TCustomGlassLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EllipsisPosition;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;


implementation

uses Consts, Dialogs, Themes, Math, winapi.UxTheme, winapi.DwmApi;


{ TGlassImage }

procedure FillGlassRect(Canvas: TCanvas; Rect: TRect);
var
  MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
  try
    FillRect(MemDC, Rect, Canvas.Brush.Handle);
    BufferedPaintMakeOpaque(PaintBuffer, @Rect);
  finally
    EndBufferedPaint(PaintBuffer, True);
  end;
end;


function TGlassImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and
    (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Picture.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Picture.Height;
  end;
end;

constructor TGlassImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csPannable];
  FPicture := TPicture.Create;
  PIctureToDraw := FPicture;
  FPicture.OnChange := PictureChanged;
  FPicture.OnProgress := Progress;
  Height := 105;
  Width := 105;
end;

function TGlassImage.DestRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := Picture.Width;
  h := Picture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
    if Proportional and (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;

destructor TGlassImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TGlassImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := Picture.Graphic;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and
    (Tmp.PaletteModified) then
  begin
    if (Tmp.Palette = 0) then
      Tmp.PaletteModified := False
    else
    begin
      ParentForm := GetParentForm(Self);
      if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
      begin
        if FDrawing then
          ParentForm.Perform(wm_QueryNewPalette, 0, 0)
        else
          PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
        Result := True;
        Tmp.PaletteModified := False;
      end;
    end;
  end;
end;

function TGlassImage.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if Picture.Graphic = nil then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      Picture.Graphic := Bitmap;
    finally
      Bitmap.Free;
    end;
  end;
  if Picture.Graphic is TBitmap then
    Result := TBitmap(Picture.Graphic).Canvas
  else
    raise EInvalidOperation.Create(SImageCanvasNeedsBitmap);
end;

function TGlassImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
    Result := FPicture.Graphic.Palette;
end;


procedure TGlassImage.Paint;
  procedure DoBufferedPaint(Canvas: TCanvas);
  var
    MemDC: HDC;
    Rect: TRect;
    GPREct: TGPRect;
    PaintBuffer: HPAINTBUFFER;
    gg: TGPGraphics;
    bm: Winapi.TGPBitmap;
    oldDC: HDC;
  begin
    Rect := DestRect;


//      BufferedPaintMakeOpaque(PaintBuffer, @Rect);
//      BufferedPaintSetAlpha(PaintBuffer, @Rect, 0);
//      BufferedPaintSetAlpha(PaintBuffer, @Rect, 0);
     if PictureToDraw.graphic is TPNGImage then begin
        oldDC := canvas.handle;
        GPREct.X := 0;//rect.Left;
        GPREct.Y := 0;//rect.Top;
        GPRect.Width := rect.Right-rect.left;
        GPRect.HEight := rect.Bottom-rect.Top;

        bm := nil;
        gg := nil;
        gg := TGPGraphics.Create(oldDC);
        try
          //bm:= TPNGimage(PictureToDraw.graphic).Get;
          bm := PNGtoGPBitmap(TPNgImage(picturetodraw.Graphic));


          //GdipSetCompositingMode(gg, CompositingModeSourceCopy);
          GdipSetCompositingMode(gg, CompositingModeSourceOver);
          gg.DrawImage(bm, GPRect);



        finally
          bm.Free;
          gg.Free;
        end;
      end else begin
        PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect, BPBF_COMPOSITED, nil, MemDC);
        try
          oldDC := canvas.handle;
          Canvas.Handle := MemDC;

            Canvas.StretchDraw(DestRect, PictureToDraw.Graphic);
        finally
          EndBufferedPaint(PaintBuffer, True);
        end;

      end;
end;

var
  Save: Boolean;
  LForm: TCustomForm;
  PaintOnGlass: Boolean;
begin
  inherited;
  if csDesigning in ComponentState then
    with inherited Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
  Save := FDrawing;
  FDrawing := True;
  try
    PaintOnGlass := DwmCompositionEnabled and not (csDesigning in ComponentState);
    if PaintOnGlass then
    begin
      LForm := GetParentForm(Self);
      PaintOnGlass := (LForm <> nil) and LForm.GlassFrame.FrameExtended and
        LForm.GlassFrame.IntersectsControl(Self);
    end;

    if PaintOnGlass then
      DoBufferedPaint(inherited Canvas)
    else
      with inherited Canvas do
        StretchDraw(DestRect, PictureToDraw.Graphic);
  finally
    FDrawing := Save;
  end;

end;

procedure TGlassImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
  D : TRect;
begin
  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
	SetBounds(Left, Top, Picture.Width, Picture.Height);
  G := Picture.Graphic;
  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := FTransparent;
    D := DestRect;
    if (not G.Transparent) and (D.Left <= 0) and (D.Top <= 0) and
       (D.Right >= Width) and (D.Bottom >= Height) then
      ControlStyle := ControlStyle + [csOpaque]
    else  // picture might not cover entire clientrect
      ControlStyle := ControlStyle - [csOpaque];
    if DoPaletteChange and FDrawing then Update;
  end
  else ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing then Invalidate;
end;

procedure TGlassImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if FIncrementalDisplay and RedrawNow then
  begin
    if DoPaletteChange then Update
    else Paint;
  end;
  if Assigned(FOnProgress) then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TGlassImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    PictureChanged(Self);
  end;

end;

procedure TGlassImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TGlassImage.SetPictureToDraw(const Value: TPicture);
begin
  if FPictureToDraw <> value then begin
    FPicturetoDraw := Value;
    invalidate;
  end;

end;

procedure TGlassImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    PictureChanged(Self);
  end;

end;

procedure TGlassImage.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    PictureChanged(Self);
  end;

end;

procedure TGlassImage.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    PictureChanged(Self);
  end;

end;

procedure TCustomGlassLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Rect, CalcRect: TRect;
  DrawStyle: Longint;
  LForm: TCustomForm;
  LGlassEnabled: Boolean;
begin
  LGlassEnabled := ThemeServices.ThemesEnabled and DwmCompositionEnabled and
    not (csDesigning in ComponentState);
  if LGlassEnabled then
  begin
    LForm := GetParentForm(Self);
    LGlassEnabled := (LForm <> nil) and LForm.GlassFrame.FrameExtended and
      LForm.GlassFrame.IntersectsControl(Self);
  end;

  with Canvas do
  begin
    Rect := ClientRect;
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      if not LGlassEnabled then
        FillRect(Rect)
      else
        FillGlassRect(Canvas, Rect);
    end
    else
//      if LGlassEnabled then
//        Windows.FillRect(Handle, Rect, GetStockObject(BLACK_BRUSH));
    Brush.Style := bsClear;
    { DoDrawText takes care of BiDi alignments }
    DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
    { Calculate vertical layout }
    if FLayout <> tlTop then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if FLayout = tlBottom then OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawText(Rect, DrawStyle);
  end;
end;



{ TCustomGlassLabel }

constructor TCustomGlassLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
  { The "default" value for the Transparent property depends on
    if you have Themes available and enabled or not. If you have
    ever explicitly set it, that will override the default value. }
  if ThemeServices.ThemesEnabled then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
end;

function TCustomGlassLabel.GetLabelText: string;
begin
  Result := Caption;
end;

procedure TCustomGlassLabel.DoDrawText(var Rect: TRect; Flags: Longint);

  procedure DoDrawThemeTextEx(DC: HDC; const Text: string; TextLen: Integer;
    var TextRect: TRect; TextFlags: Cardinal);
  var
    Options: TDTTOpts;
    rr: TRect;
    x,y: integer;
  begin
    FillChar(Options, SizeOf(Options), 0);
    Options.dwSize := SizeOf(Options);
    Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED;
    if TextFlags and DT_CALCRECT = DT_CALCRECT then
      Options.dwFlags := Options.dwFlags or DTT_CALCRECT;

    Options.crText := ColorToRGB(not Canvas.Font.Color);

(*    for x := -1 to 1 do begin
      for y := -1 to 1 do begin
        rr := TextRect;
        rr.Left := TextRect.Left-x;
        rr.Top := TextRect.Top-y;
        rr.Bottom := TextRect.Bottom-y;
        rr.Right := TextRect.Right-x;

        with ThemeServices.GetElementDetails(teEditTextNormal) do
          DrawThemeTextEx(ThemeServices.Theme[teEdit], DC, Part, State,
            PWideChar(WideString(Text)), TextLen, TextFlags, @rr, Options);
      end;
    end;
*)


    Options.dwFlags := Options.dwFlags or DTT_TEXTCOLOR or DTT_CALCRECT or DTT_SHADOWCOLOR or DTT_BORDERCOLOR or DTT_SHADOWTYPE or DTT_APPLYOVERLAY or DTT_COMPOSITED or DTT_GLOWSIZE;
    Options.iTextShadowType := TST_CONTINUOUS;
    Options.iGlowSize := 10;
    Options.crText := ColorToRGB(Canvas.Font.Color);
    Options.crBorder := ColorToRGB(not Canvas.Font.Color);
    Options.crShadow := ColorToRGB(clYellow);




    with ThemeServices.GetElementDetails(teEditTextNormal) do
      DrawThemeTextEx(ThemeServices.Theme[teEdit], DC, Part, State,
        PWideChar(WideString(Text)), TextLen, TextFlags, @TextRect, Options);


  end;

  procedure DrawText(DC: HDC; const Text: string; TextLen: Integer;
    var TextRect: TRect; TextFlags: Cardinal);
  var
    LForm: TCustomForm;
    PaintOnGlass: Boolean;
  begin
    PaintOnGlass := ThemeServices.ThemesEnabled and DwmCompositionEnabled and
      not (csDesigning in ComponentState);
    if PaintOnGlass then
    begin
      LForm := GetParentForm(Self);
      PaintOnGlass := (LForm <> nil) and LForm.GlassFrame.FrameExtended and
        LForm.GlassFrame.IntersectsControl(Self);
    end;
    if PaintOnGlass then
      DoDrawThemeTextEx(DC, Text, TextLen, TextRect, TextFlags)
    else
     winapi.Windows.DrawText(DC, PChar(Text), TextLen, TextRect, TextFlags);
  end;

const
  EllipsisStr = '...';
  Ellipsis: array[TEllipsisPosition] of Longint = (0, DT_PATH_ELLIPSIS,
    DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);
var
  Text, DText: string;
  NewRect: TRect;
  Height, Delim: Integer;
begin
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not FShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  if (FEllipsisPosition <> epNone) and not FAutoSize then
  begin
    DText := Text;
    Flags := Flags and not DT_EXPANDTABS;
    Flags := Flags or Ellipsis[FEllipsisPosition];
    if FWordWrap and (FEllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
    begin
      repeat
        NewRect := Rect;
        Dec(NewRect.Right, Canvas.TextWidth(EllipsisStr));
        winapi.Windows.DrawText(Canvas.Handle, PChar(DText), Length(DText), NewRect, Flags or DT_CALCRECT);
        Height := NewRect.Bottom - NewRect.Top;
        if (Height > ClientHeight) and (Height > Canvas.Font.Height) then
        begin
          Delim := LastDelimiter(' '#9, Text);
          if Delim = 0 then
            Delim := Length(Text);
          Dec(Delim);
          if ByteType(Text, Delim) = mbLeadByte then
            Dec(Delim);
          Text := Copy(Text, 1, Delim);
          DText := Text + EllipsisStr;
          if Text = '' then
            Break;
        end else
          Break;
      until False;
    end;
    if Text <> '' then
      Text := DText;
  end;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, Text, Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, Text, Length(Text), Rect, Flags);
  end
  else
    DrawText(Canvas.Handle, Text, Length(Text), Rect, Flags);
end;



procedure TCustomGlassLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TCustomGlassLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  AAlignment: TAlignment;
begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TCustomGlassLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomGlassLabel.SetEllipsisPosition(Value: TEllipsisPosition);
begin
  if FEllipsisPosition <> Value then
  begin
    FEllipsisPosition := Value;
    FAutoSize := False;
    Invalidate;
  end;
end;

procedure TCustomGlassLabel.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    FEllipsisPosition := epNone;
    AdjustBounds;
  end;
end;

function TCustomGlassLabel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TCustomGlassLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TCustomGlassLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TCustomGlassLabel.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
  FTransparentSet := True;
end;

procedure TCustomGlassLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TCustomGlassLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomGlassLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TCustomGlassLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;

procedure TCustomGlassLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TCustomGlassLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;


procedure Register;
begin
  RegisterComponents('Digital Tundra', [TGlassImage, TGlassLabel]);

end;

{ TThemedText }


end.
