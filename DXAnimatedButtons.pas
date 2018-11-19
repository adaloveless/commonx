unit DXAnimatedButtons;

interface

uses
  tickcount, typex, systemx, colorconversion, advancedgraphics_dx, stringx, geometry, pxl.types, math, maths, numbers, graphics;


const
  MAX_BALLS = 8;
type
  TParticle = record
    v: TVector4;
    pos: TVector4;
    sz: single;
  end;
  TDXBounceButton = class(TDXButton)
  strict protected
    animstarttime: ticker;
    lastwidth, lastheight: single;
    balls: array[0..MAX_BALLS] of TParticle;
    procedure InternalAnimation(rDeltaTime: Cardinal); override;
    procedure OnStartAnimation; override;
    procedure OnStopAnimiation; override;
  public
    procedure DoDrawAnimation; override;
    procedure PlaceRandomStuff;



  end;

implementation

{ TDXBounceButton }

procedure TDXBounceButton.DoDrawAnimation;
var
  t: ni;
  x1,y1,x2,y2: single;
  p: TParticle;
begin
  inherited;

  try
    if abs(width-lastwidth) > 1.0 then
      exit;
    if abs(height-lastheight) > 1.0 then
      exit;

    for t:= 0 to High(balls) do begin
      p := balls[t];
      x1 := p.pos.x+self.Left-p.sz;
      y1 := p.pos.y+self.Top-p.sz;
      x2 := p.pos.x+self.Left+p.sz;
      y2 := p.pos.y+self.Top+p.sz;
      x1 := Self.DX.ScreenToGlobalX(x1);
      y1 := Self.DX.ScreenToGlobalX(y1);
      x2 := Self.DX.ScreenToGlobalX(x2);
      y2 := Self.DX.ScreenToGlobalX(y2);
      self.dx.Rectangle_Fill(x1,y1,x2,y2,clWhite, lesserof(0.25,gettimesince(animstarttime)/4000));

    end;
  finally
    lastWidth := width;
    lastHeight := height;
  end;

end;

procedure TDXBounceButton.InternalAnimation(rDeltaTime: Cardinal);
VAR
  t: ni;
  speed: single;
  p: Tparticle;
begin
  inherited;

  speed := rDeltaTime/4000;
  for t := 0 to high(Self.balls) do begin
    p := balls[t];
    p.pos := p.pos + ((p.v * speed) * p.sz);
    if p.pos.x >= width then p.v.x := 0-p.v.x;
    if p.pos.y >= height then p.v.y := 0-p.v.y;
    if p.pos.x < 0 then p.v.x := 0-p.v.x;
    if p.pos.y < 0 then p.v.y := 0-p.v.y;

    balls[t] := p;
  end;
end;

procedure TDXBounceButton.OnStartAnimation;
begin
  inherited;
  animstarttime := getticker;
  placeRandomStuff;

end;

procedure TDXBounceButton.OnStopAnimiation;
begin
  inherited;
  //
end;

procedure TDXBounceButton.PlaceRandomStuff;
var
  t: ni;
  p: TParticle;
begin
  for t := 0 to high(Self.balls) do begin
    p.v.Init;
    p.v.x := Random(10000)/10000;
    p.v.y := Random(10000)/10000;
    p.pos.Init;
    p.pos.x := random(round(width));
    p.pos.y := random(round(height));
    p.sz := random(round(greaterof(width, height) / 2));
    balls[t] := p;
  end;

end;

end.
