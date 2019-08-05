unit ResizeableLabel;

TResizeableLabel = class(TLabel)
  protected
    FTextHeight, FTextWidth : integer;
    function GetCaption : TCaption;
    procedure SetCaption(ACaption : TCaption);
    function GetFont : TFont;
    procedure SetFont(AFont : TFont);
  public
    procedure Resize; override;
    property Caption : TCaption read GetCaption write SetCaption;
    property Font : TFont read GetFont write SetFont;
end;

implementation

procedure TResizeableLabel.Resize;
var
  num : double;
begin
  inherited;
  if AutoSize then
    begin
      if (FTextHeight = 0) or (FTextWidth = 0) then
        begin
            //lazy evaluation, we re evaluate every time the caption or font changes
            FTextWidth := Utills.GetTextWidth(Caption, Font);
            FTextHeight := Utills.GetTextHeight(Caption,Font);
        end;

      //TODO: there is still one bug here, set alCenter and make the last word long enough so it cant always wrapped to the line before, even though there is globally enough space
      num := (  Height / FTextHeight) - (FTextWidth /Width );
      //if num is greater then 1 it means we need an extra line, if it is lower then zero it means there is an extra line
      if (num > 1) or (num < 0) then
        begin
          //just doing this all the time will cause it to really resize and will break alTop matching the whole space
          AutoSize := false;
          AutoSize := true;
        end;
    end;
end;

function TResizeableLabel.GetCaption : TCaption;
begin
  Result := inherited Caption;
end;
procedure TResizeableLabel.SetCaption(ACaption : TCaption);
begin
  FTextWidth := Utills.GetTextWidth(ACaption, Self.Font);
  FTextHeight := Utills.GetTextHeight(ACaption,Self.Font);
  inherited Caption := ACaption;
end;

function TResizeableLabel.GetFont : TFont;
begin
  Result := inherited Font;
end;
procedure TResizeableLabel.SetFont(AFont : TFont);
begin
  FTextWidth := Utills.GetTextWidth(Caption, AFont);
  FTextHeight := Utills.GetTextHeight(Caption,AFont);
  inherited Font := AFont;
end;


class function Utills.GetTextHeight(const Text:String; Font:TFont) : Integer;
var
  bitmap: TBitmap;
begin
  bitmap := TBitmap.Create;
  try
   bitmap.Canvas.Font := Font;
   Result := bitmap.Canvas.TextHeight(Text);
  finally
   bitmap.Free;
  end;
end;

class function Utills.GetTextWidth(const Text:String; Font:TFont) : Integer;
var
  bitmap: TBitmap;
begin
  bitmap := TBitmap.Create;
  try
   bitmap.Canvas.Font := Font;
   Result := bitmap.Canvas.TextWidth(Text);
  finally
   bitmap.Free;
  end;
end;


end.
