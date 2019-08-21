unit ScaledLayoutProportional;

interface


uses
  Debug, stringx, guihelpers_fmx,
  System.Classes, System.Types, System.UITypes, System.SysUtils, System.Math.Vectors, FMX.Types, FMX.StdCtrls,
  FMX.Platform, FMX.Controls, FMX.InertialMovement, fmx.layouts;

type
  TAnotherFuckingHackedDelphiControl = class(TControl)
  public
    property Updating: integer read Fupdating;
    property Scale;
  end;

  TScaledLayoutProportional = class(TControl)
  private
    last_proportions: single;
    FOriginalWidth: Single;
    FOriginalHeight: Single;
    FfirstWidth: single;
    FFirstheight: single;
    FHasPainted: boolean;
    procedure SetOriginalWidth(const Value: Single);
    procedure SetOriginalHeight(const Value: Single);
  protected
    procedure DoRealign; override;
    function GetChildrenMatrix(var Matrix: TMatrix; var Simple: Boolean): Boolean; override;
    procedure SetHeight(const Value: Single); override;
    procedure SetWidth(const Value: Single); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    property HitTest default False;
    property Padding;
    property Opacity;
    property OriginalWidth: Single read FOriginalWidth write SetOriginalWidth;
    property OriginalHeight: Single read FOriginalHeight write SetOriginalHeight;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    procedure ForceRealign;
  end;




function UpgradeLayoutToProportional(lay: TScaledLayout): TScaledLayoutProportional;


implementation

uses
  System.Generics.Collections, System.Math, System.RTLConsts, System.TypInfo, FMX.Styles, FMX.Consts, FMX.Effects,
  FMX.Ani, FMX.Graphics;


function UpgradeLayoutToProportional(lay: TScaledLayout): TScaledLayoutProportional;
begin
  if lay.parent = nil then
    exit;


  result := TScaledlayoutProportional.create(lay.owner);
  result.Width := lay.width;
  result.height := lay.height;
  result.OriginalWidth := lay.width;
  result.OriginalHeight := lay.height;

  var par := lay.parent;


  for var t:= lay.childrencount-1 downto 0  do begin
    lay.children[t].parent := result;
  end;
  result.parent := lay.parent;
  result.align := lay.align;

  lay.parent := nil;
  result.ForceRealign;



end;

{ TScaledLayoutProportional }

constructor TScaledLayoutProportional.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FOriginalWidth := Width;
  FOriginalHeight := Height;
end;

destructor TScaledLayoutProportional.Destroy;
begin
  inherited;
end;

procedure TScaledLayoutProportional.DoRealign;
begin
  if (Parent <> nil) and (Parent is TCustomScrollBox) and (TAnotherFuckingHackedDelphiControl(Parent).Updating > 0) then
    Exit;


  if csDesigning in ComponentState then
    inherited DoRealign
  else
  begin
    if FNeedAlign then
      AlignObjects(Self, Padding, FOriginalWidth, FOriginalHeight, FLastWidth, FLastHeight, FDisableAlign);

    if (FOriginalheight <> 0) and (Height <> 0) then begin
      var w := Control_GetWidth(parent);
      var h := Control_GetHeight(parent);

      if FFirstHeight = 0 then
        FFirstHeight := h;
      if FFirstWidth = 0 then
        FFirstWidth := w;



      if (w>0) and (h>0) then begin
        var original_proportions := FFirstWidth / FFirstHeight;
        var this_proportions := w/h;
        var sx,sy: single;
        if (this_proportions / original_proportions) >= 1.0 then begin
          sx := original_proportions / this_proportions;
          sy := 1.0;
        end else begin
          sx := 1;
          sy := this_proportions / original_proportions;
        end;

        Debug.Log('props : '+floatprecision(original_proportions,2)+','+floatprecision(this_proportions,2));
        Debug.Log('dimensions : '+floatprecision(w,2)+','+floatprecision(h,2));
        last_proportions := this_proportions;
        for var t := 0 to self.ChildrenCount-1 do begin
          if children[t] is TControl then begin
            TAnotherFuckingHackedDelphiControl(children[t]).Scale.x := sx;
            TAnotherFuckingHackedDelphiControl(children[t]).Scale.y := sy;
          end;
        end;
      end;

    end;


    RecalcAbsolute;
    FRecalcUpdateRect := True;
  end;
end;

procedure TScaledLayoutProportional.ForceRealign;
begin
  self.Realign;
end;

function TScaledLayoutProportional.GetChildrenMatrix(var Matrix: TMatrix; var Simple: Boolean): Boolean;
begin
  Result := True;
  if csDesigning in ComponentState then
  begin
    OriginalHeight := Height;
    OriginalWidth := Width;
  end;
  Matrix := TMatrix.Identity;
  Matrix.m11 := Width / FOriginalWidth;
  Matrix.m22 := Height / FOriginalHeight;
  Simple := SameValue(Matrix.m11, 1, TEpsilon.Matrix) and SameValue(Matrix.m22, 1, TEpsilon.Matrix);
end;

procedure TScaledLayoutProportional.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
  if not FHasPainted then begin
    ForceRealign;
    FHasPainted := true;
  end;
end;

procedure TScaledLayoutProportional.SetOriginalHeight(const Value: Single);
begin
  if FOriginalHeight <> Value then
  begin
    FOriginalHeight := Value;
    if FOriginalHeight < 1 then
      FOriginalHeight := 1;
    RecalcAbsolute;
  end;
end;

procedure TScaledLayoutProportional.SetOriginalWidth(const Value: Single);
begin
  if FOriginalWidth <> Value then
  begin
    FOriginalWidth := Value;
    if FOriginalWidth < 1 then
      FOriginalWidth := 1;
    RecalcAbsolute;
  end;
end;

procedure TScaledLayoutProportional.SetHeight(const Value: Single);
begin
  inherited;
  if csDesigning in ComponentState then
    OriginalHeight := value
  else
    RecalcAbsolute;

  ReAlign;
end;

procedure TScaledLayoutProportional.SetWidth(const Value: Single);
begin
  inherited;
  if csDesigning in ComponentState then
    OriginalWidth := value
  else
    RecalcAbsolute;

  ReAlign;
end;

end.
