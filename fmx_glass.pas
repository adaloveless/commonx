unit fmx_glass;

interface

uses
  fmx.controls, fmx.effects, fmx.graphics, classes, fmx.forms, system.math, fmx.types, types;

type
  TGlass = class(TControl)
  private
    FBlur: TBlurEffect;
    FParentScreenshotBitmap: TBitmap;
    FSoftNess: double;
    function GetSoftness: double;
    procedure SetSoftness(Value: double);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property Softness: double read GetSoftness write SetSoftness;
  end;


implementation

{ TGlass }

constructor TGlass.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  // Create parent background
  FParentScreenshotBitmap := TBitmap.Create(0, 0);

  // Create blur
  FBlur := TBlurEffect.Create(nil);
  FBlur.Softness := 0.6;

end;

destructor TGlass.Destroy;
begin

  FBlur.Free;
  FParentScreenshotBitmap.Free;

  inherited Destroy;

end;

function TGlass.GetSoftness: double;
begin
  Result := FSoftNess;

end;

procedure TGlass.SetSoftness(Value: double);
begin

  FSoftNess := value;
  FBlur.Softness := Value;
  InvalidateRect(self.BoundsRect);

end;

procedure TGlass.Paint;
var
  ParentWidth: Single;
  ParentHeight: Single;

  procedure DefineParentSize;
  begin
    ParentWidth := 0;
    ParentHeight := 0;
    if Parent is TCustomForm then
    begin
      ParentWidth := (Parent as TCustomForm).ClientWidth;
      ParentHeight := (Parent as TCustomForm).ClientHeight;
    end;
    if Parent is TControl then
    begin
      ParentWidth := (Parent as TControl).Width;
      ParentHeight := (Parent as TControl).Height;
    end;
  end;

  function IsBitmapSizeChanged(ABitmap: TBitmap; const ANewWidth, ANewHeight: Single): Boolean;
  begin
    Result := not SameValue(ANewWidth * ABitmap.BitmapScale, ABitmap.Width) or
              not SameValue(ANewHeight * ABitmap.BitmapScale, ABitmap.Height);
  end;

  procedure MakeParentScreenshot;
  var
    Form: TCommonCustomForm;
    Child: TFmxObject;
    ParentControl: TControl;
  begin
    if parent = nil then
      exit;
    if FParentScreenshotBitmap.Canvas.BeginScene then
      try
        FDisablePaint := True;
        if Parent is TCommonCustomForm then
        begin
          Form := Parent as TCommonCustomForm;
          for Child in Form.Children do
            if (Child is TControl) and (Child as TControl).Visible then
            begin
              ParentControl := Child as TControl;
              ParentControl.PaintTo(FParentScreenshotBitmap.Canvas, ParentControl.ParentedRect);
            end;
        end
        else
          (Parent as TControl).PaintTo(FParentScreenshotBitmap.Canvas, RectF(0, 0, ParentWidth, ParentHeight));
      finally
        FDisablePaint := False;
        FParentScreenshotBitmap.Canvas.EndScene;
      end;
  end;

begin

  // Make screenshot of Parent control
  DefineParentSize;
  if IsBitmapSizeChanged(FParentScreenshotBitmap, ParentWidth, ParentHeight) then
    FParentScreenshotBitmap.SetSize(Round(ParentWidth), Round(ParentHeight));
  MakeParentScreenshot;

  // Apply glass effect
  Canvas.BeginScene;
  try
    FBlur.ProcessEffect(Canvas, FParentScreenshotBitmap, FBlur.Softness);
    Canvas.DrawBitmap(FParentScreenshotBitmap, ParentedRect, LocalRect, 1, TRUE);
  finally
    Canvas.EndScene;
  end;

end;

end.
