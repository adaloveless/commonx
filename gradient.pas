unit gradient;

interface

uses
  easyimage, typex, systemx, math, geometry, graphics, soundtools, advancedgraphics, fastbitmap, synth_functions, colorblending;

function GenerateRWB: TFAstBitmap;




implementation

function GenerateRWB: TFAstBitmap;
const
  PI = 3.1415;
  RCENTER = 0;
  WCENTER = PI/2;
  BCENTER = PI;
var
  cpi,cpii: nativefloat;
  x,y: ni;
  c1,c2: TColor;
  blend: nativefloat;
begin
  result := TFastBitmap.create;
  result.Width := 256;
  result.Height := 256;
  result.new;


  for y:= 0 to result.Height -1 do begin
    for x:= 0 to result.Width -1 do begin
      cpi := (x/(result.Width-1))*(PI);
      cpii := cpi;
      while cpii > PI do cpii := cpii-pi;
      blend := CosSynth(x, 1, 1, result.Width);
      blend := blend * ((y/result.height)/0.05);
      if blend > 1.0 then blend := 1.0;
      if blend < -1.0 then blend := -1.0;
      blend := (blend + 1)/2;

      if cpii <= WCENTER then begin
        c1 := clRed;
        c2 := clWhite;
      end else
      if cpii <= BCENTER then begin
        c1 := clBlue;
        c2 := clWhite;
      end;

      result.Canvas.Pixels[x,y] := colorblend(c1,c2, 1-blend);



    end;
  end;

end;

end.
