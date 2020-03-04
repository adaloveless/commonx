unit fmx_bitmaps;

interface

uses
  betterobject,fastbitmap, typex, systemx, fmx.graphics, uitypes, types;


function ColorblendAlphaColors(a,b: TAlphaColor): TAlphaColor;
function ColorAddAlphaColors(a,b: TAlphaColor): TAlphaColor;



function fmxBitmapToFastBitmap(bm: TBitmap): TFastBitmap;overload;
function fmxBitmapToFastBitmapH(bm: TBitmap): IHolder<TFastBitmap>;overload;

function fmxBitmapToFastBitmap(var bm: TBitmapData): TFastBitmap;overload;
function FastBitmapToFmxBitmap(bm: TFAstBitmap): TBitmap;overload;
procedure FastBitmapToFmxBitmap(fb: TFastBitmap; var bm: TBitmapData);overload;

procedure bitmapdata_DrawRect(var bm: TBitmapData; r: TRect; ac: TAlphaColor);

procedure SaveBitmapAsProprietary(bm: TBitmap; sFile: string);overload;
procedure SaveBitmapAsProprietary(var bmd: TBitmapData; sFile: string);overload;
procedure LoadBitmapAsProprietary(bm: TBitmap; sFile: string);overload;
procedure LoadBitmapAsProprietary(var bmd: TBitmapData; sFile: string);overload;





implementation

uses
  colorblending, colorconversion;

procedure FastBitmapToFmxBitmap(fb: TFastBitmap; var bm: TBitmapData);overload;
var
  x,y: ni;
begin
  for y:= 0 to fb.Height-1 do begin
    for x := 0 to fb.width-1 do begin
      with fb.canvas do
      begin
        bm.SetPixel(x,y,AlphaPixels[x,y]);
      end;
    end;
  end;
end;

function fmxBitmapToFastBitmapH(bm: TBitmap): IHolder<TFastBitmap>;
begin
  result := THolder<TFastBitmap>.create;
  result.o := fmxBitmaptoFastBitmap(bm);
end;



function fmxBitmapToFastBitmap(var bm: TBitmapData): TFastBitmap;overload;
var
  x,y: ni;
begin
  result := TFastBitmap.create;
  result.Width := bm.Width;
  result.Height := bm.Height;
  result.New;
  for y:= 0 to result.Height-1 do begin
    for x := 0 to result.width-1 do begin
      with result.canvas do begin
//        var c := colorconversion.ColorFormat(bm.GetPixel(x,y), 'rgba', 'bgra');
        AlphaPixels[x,y] := bm.GetPixel(x,y);
      end;
    end;
  end;

end;

function fmxBitmapToFastBitmap(bm: TBitmap): TFastBitmap;
var
  bmd: TBitmapData;
begin
  bm.Map(TMapAccess.ReadWrite, bmd);
  try
    result := fmxBitmapToFastBitmap(bmd);

  finally
    bm.Unmap(bmd);
  end;
end;

function FastBitmapToFmxBitmap(bm: TFastBitmap): TBitmap;overload;
var
  bmd: TBitmapData;
begin
  result := TBitmap.create(bm.width, bm.Height);
  result.Map(TMapAccess.ReadWrite, bmd);
  try
    FastBitmapToFmxBitmap(bm, bmd);
  finally
    result.Unmap(bmd);
  end;
end;

function ColorblendAlphaColors(a,b: TAlphaColor): TAlphaColor;
var
  c,d,r: TGiantColor;
  oppa: single;
  outa: single;
begin

  c.FromAlphaColor(a);
  d.FromAlphaColor(b);
  oppa := 1-d.a;


  outa := d.a+(c.a*((1-d.a)));
  if outa = 0 then begin
    result := 0;
    exit;
  end;

  r.r := ((c.r*oppa)+(d.r*d.a)/outa);
  r.g := ((c.g*oppa)+(d.g*d.a)/outa);
  r.b := ((c.b*oppa)+(d.b*d.a)/outa);
  r.a := outa;
  result := r.ToAlphaColor;


end;


function ColorAddAlphaColors(a,b: TAlphaColor): TAlphaColor;
var
  c,d,r: TGiantColor;
begin

  c.FromAlphaColor(a);
  d.FromAlphaColor(b);
  d.r := d.r * d.a;
  d.g := d.g * d.a;
  d.b := d.b * d.a;

  r.r := ((c.r)+(d.r));
  r.g := ((c.g)+(d.g));
  r.b := ((c.b)+(d.b));
  r.a := ((c.a)+(d.a));
  result := r.ToAlphaColor;


end;


procedure bitmapdata_DrawRect(var bm: TBitmapData; r: TRect; ac: TAlphaColor);
var
  x,y: ni;
begin
  for y := r.Top to r.Bottom do begin
    for x := r.Left to r.Right do begin
      bm.SetPixel(x,y,ac);
    end;
  end;
end;


procedure SaveBitmapAsProprietary(bm: TBitmap; sFile: string);
var
  bmd: TBitmapData;
begin
  bm.Map(TMapAccess.ReadWrite, bmd);
  try
    SaveBitmapAsProprietary(bmd, sFile);
  finally
    bm.Unmap(bmd);
  end;
end;

procedure LoadBitmapAsProprietary(bm: TBitmap; sFile: string);
var
  bmd: TBitmapData;
begin
  bm.Map(TMapAccess.ReadWrite, bmd);
  try
    LoadBitmapAsProprietary(bmd, sFile);
  finally
    bm.Unmap(bmd);
  end;
end;

procedure SaveBitmapAsProprietary(var bmd: TBitmapData; sFile: string);overload;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure LoadBitmapAsProprietary(var bmd: TBitmapData; sFile: string);overload;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;




end.
