unit huestogram;

interface

uses
  typex, easyimage, graphics, betterobject, fastbitmap, classes, sysutils, colorconversion, colorblending, helpers.stream;

type

  THueFloat = double;

  THuestogramRecord = packed record
    comparator: TColor;
    count: Thuefloat;
  end;

  THuestogram = class(TBetterObject)
  public
    hues: array[0..8] of THuestogramRecord;
    sum: Thuefloat;
    primarycolor: TColor;
    secondarycolor: TColor;
    procedure FromFastBitMap(fb: TFastBitmap);
    procedure SaveToStream(s: TStream);
    procedure LoadFromStream(s: TStream);
    procedure CalcPrimes;
    procedure Init;override;
    function Debug: string;

  end;


implementation



{ THuestogram }

procedure THuestogram.CalcPrimes;
var
  t: integer;
  i1, i2: integer;
  fSum: nativefloat;
begin
  i1 := 8;

  fSum := 0;
  for t:= 0 to 8 do begin
    fSum := fSum + hues[t].count;
    if hues[t].count > 0 then begin
      i1 := t;
    end;
  end;

  sum := fSum;

  for t:= 0 to 7 do begin
    if hues[t].count > hues[i1].count then begin
      i1 := t;
    end;
  end;

  i2 := -1;
  for t:= 0 to 8 do begin
    if (t <> i1) and ((i2 < 0) or (hues[t].count > hues[i2].count)) then begin
      i2 := t;
    end;
  end;

  primarycolor := primary_hues[i1];
  secondarycolor := primary_hues[i2];


end;

function THuestogram.Debug: string;
var
  t: integer;
begin
  result := '';
  for t:= 0 to 8 do begin
    result := result + inttohex(round(hues[t].count), 4);
  end;

end;

procedure THuestogram.FromFastBitMap(fb: TFastBitmap);
var
  hsl_img, hsl_primary: THSLnativefloatColor;
  rr,x,y,t: integer;
  a: array[0..8] of TColorFloat;
  iMax1, iMax2: integer;
  hsl_primarys: array[0..8] of THSLnativefloatColor;
begin
  init;
  for t:= 0 to 8 do begin
    hsl_primarys[t] := RGBtoHSL(ColorTonativefloatRGB(primary_hues[t]));
  end;

  for y := 0 to fb.Height-1 do begin
    for x := 0 to fb.Width-1 do begin
//      if ((x+y) mod 256) <> 0 then begin
//        continue;
//      end;
      hsl_img := fb.Canvas.HSLPixels[x,y];

      //go through colors and determine error (distance from target hue)
      for t := 0 to 8 do begin
        hsl_primary := hsl_primarys[t];
        if hsl_img.s < 0.1 then
          a[t] := 99999999
        else
          a[t] := abs(hsl_primary.h-hsl_img.h)/(hsl_img.s);
      end;

      iMax1 := 0;
      for t := 1 to 8 do begin
        if a[t] < a[iMax1] then
          iMax1 := t;
      end;

      //init iMax2 but make sure its not the same as iMax1
      if iMax1 = 0 then
        iMax2 := 1
      else
        iMax2 := 0;

      //find the second highest hue in the list
      for t := 1 to 8 do begin
        if (a[t] < a[iMax2]) and (t <> iMax2) then
          iMax2 := t;
      end;

      if (hsl_img.s < 0.1) or (hsl_img.l < 0.1) then
        iMax1 := 8;

      hues[iMax1].count := hues[iMax1].count + (hsl_img.s*hsl_img.l);



    end;
  end;
end;

procedure THuestogram.Init;
var
  t: integer;
begin
  for t := 0 to  8 do begin
    hues[t].comparator := primary_hues[t];
    hues[t].count := 0;
  end;



  sum  := 0;
end;

procedure THuestogram.LoadFromStream(s: TStream);
var
  t: integer;
  sum: THueFloat;
begin
  sum := 0;
  for t:= 0 to 8 do begin
    try
      Stream_guaranteeread(s, Pbyte(@hues[t]), sizeof(hues[t]));
    except
    end;
  end;
  try
    Stream_guaranteeread(s,Pbyte(@sum), sizeof(sum));
  except
  end;
  self.sum := sum;

  CalcPrimes;

end;

procedure THuestogram.SaveToStream(s: TStream);
var
  t: integer;
  sum: Thuefloat;
begin
  sum := 0;
  for t:= 0 to 8 do begin
    Stream_GuaranteeWrite(s,Pbyte(@hues[t]), sizeof(hues[t]));
    sum := sum + hues[t].count;

  end;
  Stream_GuaranteeWrite(s,Pbyte(@sum), sizeof(sum));

end;


end.
