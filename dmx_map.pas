unit dmx_map;

interface

uses
  easyimage, advancedgraphics, typex, systemx, dmx_objects, graphics, fastbitmap;

function GetDMXMap(dmx: TDMXUniverse): TFastBitmap;


implementation



function GetDMXMap(dmx: TDMXUniverse): TFastBitmap;
var
  x,y: ni;
const
  C_IN_USE = clRed;
  C_AVAILABLE = clLIme;
begin
  result := TFastBitmap.create;
  result.width := 256;
  result.height := 256;

  result.New;

  for y := 0 to 15 do begin
    for x := 0 to 15 do begin

    end;
  end;

end;



end.
