unit PixelShader;

interface

uses
  typex, stringx, fastbitmap, geometry, colorblending, betterobject, pxl.canvas, pxl.types, system.uitypes, numbers;

type
  TPS_IN = record
    InCol: TVector4;
    InTex: TPOint2;
    InPos: TVector4;
    InRef: TPOint2;
  end;

  TPixelShader = class(TBetterObject)
  strict private
    function CONST_LUMINENCE: single; inline;//#define CONST_LUMINENCE materialconsts0.a
    function CONST_DIFFUSE: single;inline;//#define CONST_DIFFUSE materialconsts0.b
    function CONST_AMBIENCE: single;inline;//#define CONST_AMBIENCE materialconsts1.r
    function CONST_SPECULAR: single;inline;//#define CONST_SPECULAR materialconsts1.g
    function CONST_MINREF: single;inline;//#define CONST_MINREF materialconsts0.g
    function CONST_MAXREF: single;inline;//#define CONST_MAXREF materialconsts0.r
    function CONST_CENTERX: single;inline;//#define CONST_CENTERX screen_data.r
    function CONST_CENTERY: single;inline;//#define CONST_CENTERY screen_data.g
    function CONST_UNITYDEPTH: single;inline;//#define CONST_UNITYDEPTH screen_data.b
    function MaterialConsts0: TVector4;inline;
    function MaterialConsts1: TVector4;inline;
  private
    function GetLightInfluence(input: TPS_IN): TVector4;

  public
    Texture, RefMap, RefTex: TFastBitmap;
    LIghts: array[0..15] of TPXLLight;
    Consts: array[0..3] of TVector4;
    light_params: TVector4;
    screen_data: TVector4;
    function Main(input: TPS_IN): TVector4;

  end;

function ACtoV4(ac: TAlphaColor): TVEctor4;




implementation

{ TPixelShader }


//cbuffer VS_CONSTANT_BUFFER : register(b0)
//{
//  float4 materialconsts0; //r=maxref, g=minref, b=diffuse, a=luminence
//  float4 light_params;
//  float4 screen_data;//r=centerx, g=centery, b=unitydepth
//  float4 materialconsts1; //r=ambience g=specular b=future a=future
//};

function ACtoV4(ac: TAlphaColor): TVEctor4;
begin
  result.x := (ac and $FF) / 255;
  result.y := ((ac shr 8)and $FF) / 255;
  result.z := ((ac shr 16)and $FF) / 255;
  result.w := ((ac shr 24)and $FF) / 255;

end;

function V4toAC(v4: TVector4): TAlphaColor;
var
  r,g,b,a: ni;
begin
  r := lesserof(round(v4.r*255),255);
  g := lesserof(round(v4.g*255),255);
  b := lesserof(round(v4.b*255),255);
  a := lesserof(round(v4.a*255),255);

  result := r + (g shl 8)+(b shl 16)+(a shl 24);

end;


function TPixelShader.CONST_AMBIENCE: single;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPixelShader.CONST_CENTERX: single;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPixelShader.CONST_CENTERY: single;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPixelShader.CONST_DIFFUSE: single;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPixelShader.CONST_LUMINENCE: single;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPixelShader.CONST_MAXREF: single;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPixelShader.CONST_MINREF: single;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPixelShader.CONST_SPECULAR: single;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPixelShader.CONST_UNITYDEPTH: single;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPIxelShader.GetLightInfluence(input: TPS_IN): TVector4;//float4 getLightInfluence(PS_IN input)
var
  light_count: ni;
  t: ni;
  lc, lp,light_color: TVector4;
  i, i1, l1, light_result: TVector4;
  depthcenter: TPOint2;
  len: single;
begin
  light_count := round(light_params.r);

  light_color := Vector4(0,0,0,0);

  for t := 0 to 15 do begin
    //get light position and color
    lp := lights[t].pos;//    float4 lp = lights[t].pos;
    lc := lights[t].color;//  //float4 lc = lights[t].color;

    //correct light's position in space in case it is behind the camera
    if (lp.z < 0) then begin
      lp := lp * vector4(-1,-1,1,1);
    end;

    light_result := lc;

    //determine light's position relative to object
    i1 := input.InPOs;
    l1 := lp;
    depthcenter := screen_data.xy;
    i1.xy := i1.xy - depthcenter;
    l1.xy := i1.xy - depthcenter;
    i1.xy := i1.xy*abs(i1.z/100);
    l1.xy := l1.xy*abs(l1.z/100);
    i1.xy := i1.xy + depthcenter;
    l1.xy := l1.xy +  depthcenter;
    i := i1-l1;

    if (i.z >= 0) then
    begin
        i.w := 0;

        len := sqrt((i.x*i.x)+(i.y*i.y)+(i.z*i.z)+(i.w*i.w));
        if (len<1) then
            len := 1;

        len := abs(1/len);

        //light influence decreases with distance
        light_result := light_result * len;

        light_color := light_color + (light_result * Vector4(lights[t].param.r, lights[t].param.r, lights[t].param.r, 1));
    end;

    //add ambience from lights
    light_color := light_color + (lights[t].color * (Vector4(lights[t].param.g* CONST_AMBIENCE, lights[t].param.g* CONST_AMBIENCE, lights[t].param.g* CONST_AMBIENCE, 1)));

  end;

  result := light_color;
end;


function TPixelShader.Main(input: TPS_IN): TVector4;
var
  diflum, lum, main_sample, map, res, mul, reflection, light_color: TVector4;
  tcRef, refOffset: TPoint2;
  resA: single;
  mul1, mul2: TVector4;
begin
  main_sample :=actov4(self.Texture.canvas.alphapixels[round(input.InTex.x), round(input.InTex.y)]) * input.InCol;

  map := actov4(self.RefMap.Canvas.alphapixels[round(input.InTex.x), round(input.InTex.Y)]);
  lum := vector4(0,0,0,0);
  if map.A = 1.0 then begin
    lum.rgb := map.rgb;
    lum.a := 0;
  end;
  map.a := map.a * (CONST_MAXREF-CONST_MINREF);
  map.a := map.a + CONST_MINREF;

  resA := main_sample.a;
  refOffset := map.xy;
  reflection := actov4(Self.RefTex.canvas.alphapixels[round((input.InRef+refOffset).x),round((input.InRef+refOffset).y)]);;

  res := main_sample;

  mul2 := vector4(map.a, map.a, map.a, map.a);
  mul1 := vector4(1-map.a, 1-map.a, 1-map.A, 1-map.a);

  light_color := getLightInfluence(input);
  diflum := res * CONST_LUMINENCE;//amount of diffuse that is self illuminating
  res.rgb := res.rgb * light_color.rgb;//affect of diffuse relating to light
  main_sample := res;
  //reflection last
  reflection := reflection * mul2;
  res.rgb := main_sample.rgb + reflection.rgb;
  res.a := main_sample.a + map.a;

  res := res + lum;//lum is the color from the luminance texture (part of the reflection map)
  res := res + diflum;

  //crunch
//  res.rgb = round(res.rgb*10)/10;
//  res.r = round(res.r*10)/10;
//  res.g = round(res.g*10)/10;
//  res.b = round(res.b*10)/10;

  //res.rgb = res.rgb * light_color.rgb;

  result := res;

end;

function TPixelShader.MaterialConsts0: TVector4;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPixelShader.MaterialConsts1: TVector4;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

end.

