unit ColorButton;
{
Article:

TColorButton - button with Color properties

http://delphi.about.com/library/weekly/aa061104a.htm

Full source code of the TColorButton Delphi component,
an extension to the standard TButton control,
with font color, background color and mouse over color properties.

}

interface

uses
  colorblending, Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Buttons, ExtCtrls, touchcontrols_vcl;

type
  TColorButton = class(TTouchButton)
  private
    FBackBeforeHoverColor: TColor;
  private
    FCanvas: TCanvas;
    IsFocused: Boolean;
    FBackColor: TColor;
    FForeColor: TColor;
    FHoverColor: TColor;
    procedure SetBackColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetHoverColor(const Value: TColor);

    property BackBeforeHoverColor : TColor read FBackBeforeHoverColor write FBackBeforeHoverColor;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message : TMessage); override;

    procedure SetButtonStyle(Value: Boolean); override;
    procedure DrawButton(Rect: TRect; State: UINT);

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property BackColor: TColor read FBackColor write SetBackColor default clBtnFace;
    property ForeColor: TColor read FForeColor write SetForeColor default clBtnText;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clBtnFace;
  end;

procedure Register;

implementation


constructor TColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  BackColor := clBtnFace;
  ForeColor := clBtnText;
  HoverColor := clBtnFace;
end; (*Create*)

destructor TColorButton.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end; (*Destroy*)

procedure TColorButton.WndProc(var Message : TMessage);
begin
  if (Message.Msg = CM_MOUSELEAVE) then
  begin
    BackColor := BackBeforeHoverColor;
    invalidate;
  end;
  if (Message.Msg = CM_MOUSEENTER) then
  begin
    BackBeforeHoverColor := BackColor;
    BackColor := HoverColor;
    invalidate;
  end;

  inherited;
end; (*WndProc*)

procedure TColorButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end; (*CreateParams*)



procedure TColorButton.SetButtonStyle(Value: Boolean);
begin
  if Value <> IsFocused then
  begin
    IsFocused := Value;
    Invalidate;
  end;
end; (*SetButtonStyle*)

procedure TColorButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth  := Width; 
    itemHeight := Height; 
  end; 
end; (*CNMeasureItem*)

procedure TColorButton.CNDrawItem(var Message: TWMDrawItem);
var 
  SaveIndex: Integer;
begin 
  with Message.DrawItemStruct^ do 
  begin 
    SaveIndex := SaveDC(hDC); 
    FCanvas.Lock; 
    try 
      FCanvas.Handle := hDC; 
      FCanvas.Font := Font; 
      FCanvas.Brush := Brush; 
      DrawButton(rcItem, itemState);
    finally
      FCanvas.Handle := 0;
      FCanvas.Unlock; 
      RestoreDC(hDC, SaveIndex); 
    end;
  end; 
  Message.Result := 1; 
end; (*CNDrawItem*)

procedure TColorButton.CMEnabledChanged(var Message: TMessage);
begin 
  inherited; 
  Invalidate;
end; (*CMEnabledChanged*)

procedure TColorButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end; (*CMFontChanged*)


procedure TColorButton.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then begin
    FBackColor:= Value;
    HoverColor := colorblend(FBackColor, clWhite, 0.5);
    Invalidate;
  end;
end; (*SetButtonColor*)

procedure TColorButton.SetForeColor(const Value: TColor);
begin
  if FForeColor <> Value then begin
    FForeColor:= Value;
    Invalidate;
  end;
end; (*SetForeColor*)

procedure TColorButton.SetHoverColor(const Value: TColor);
begin
  if FHoverColor <> Value then begin
    FHoverColor:= Value;
    Invalidate;
  end;
end; (*SetHoverColor*)

procedure TColorButton.DrawButton(Rect: TRect; State: UINT);
var
  Flags, OldMode: Longint;
  IsDown, IsDefault, IsDisabled: Boolean;
  OldColor: TColor;
  OrgRect: TRect;
begin
  OrgRect := Rect;
  Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
  IsDown := State and ODS_SELECTED <> 0;
  IsDefault := State and ODS_FOCUS <> 0;
  IsDisabled := State and ODS_DISABLED <> 0;

  if IsDown then Flags := Flags or DFCS_PUSHED;
  if IsDisabled then Flags := Flags or DFCS_INACTIVE;

  if IsFocused or IsDefault then 
  begin 
    FCanvas.Pen.Color := clWindowFrame; 
    FCanvas.Pen.Width := 1; 
    FCanvas.Brush.Style := bsClear; 
    FCanvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom); 
    InflateRect(Rect, - 1, - 1); 
  end; 

  if IsDown then 
  begin
    FCanvas.Pen.Color := clBtnShadow;
    FCanvas.Pen.Width := 1; 
    FCanvas.Brush.Color := clBtnFace; 
    FCanvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom); 
    InflateRect(Rect, - 1, - 1); 
  end 
  else 
    DrawFrameControl(FCanvas.Handle, Rect, DFC_BUTTON, Flags); 

  if IsDown then OffsetRect(Rect, 1, 1); 

  OldColor := FCanvas.Brush.Color;
  FCanvas.Brush.Color := BackColor;
  FCanvas.FillRect(Rect); 
  FCanvas.Brush.Color := OldColor;
  OldMode := SetBkMode(FCanvas.Handle, TRANSPARENT); 
  FCanvas.Font.Color := ForeColor;
  if IsDisabled then
    DrawState(FCanvas.Handle, FCanvas.Brush.Handle, nil, nativeint(Caption), 0,
    ((Rect.Right - Rect.Left) - FCanvas.TextWidth(Caption)) div 2, 
    ((Rect.Bottom - Rect.Top) - FCanvas.TextHeight(Caption)) div 2, 
      0, 0, DST_TEXT or DSS_DISABLED) 
  else 
    DrawText(FCanvas.Handle, PChar(Caption), - 1, Rect, 
      DT_SINGLELINE or DT_CENTER or DT_VCENTER); 
  SetBkMode(FCanvas.Handle, OldMode); 

  if IsFocused and IsDefault then
  begin
    Rect := OrgRect;
    InflateRect(Rect, - 4, - 4);
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Brush.Color := clBtnFace;
    DrawFocusRect(FCanvas.Handle, Rect);
  end;
end; (*DrawButton*)

procedure Register;
begin
  RegisterComponents('Digital Tundra', [TColorButton]);
end;




end.

