unit dmx_color_visualizer;

interface

uses
  extctrls, windows, dmx_objects;

type
  TDMXColorVisualizer = class(TShape)
  private
    FICOLOR: IDMXCOlor;
    Fmarked: boolean;
    FEnabled: boolean;
  protected
  public
    property ICOLOR: IDMXCOlor read FICOLOR write FICOLOR;
    procedure RefreshColor;
    property Marked: boolean read Fmarked write FMarked;
    property Enabled: boolean read FEnabled write FEnabled;
  published
  end;


implementation

{ TDMXColorVisualizer }

procedure TDMXColorVisualizer.RefreshColor;
begin
  Pen.Color := ICOLOR.Color;
  Brush.Color := ICOLOR.Color;
  invalidate;
  refresh;

end;

end.
