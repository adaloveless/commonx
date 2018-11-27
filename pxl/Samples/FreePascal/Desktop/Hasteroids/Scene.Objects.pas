unit Scene.Objects;
{
  This file is part of Asphyre Framework, also known as Pascal eXtended Library (PXL).
  Copyright (c) 2000 - 2015  Yuriy Kotsarenko

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.
}
interface

{$INCLUDE PXL.Config.inc}

{ Special note: this code was ported multiple times from earliest framework releases predating Asphyre. }

uses
  PXL.TypeDef, PXL.Types, PXL.Canvas, Engine.Particles, Engine.Objects;

{$INCLUDE Scene.Objects.Ships.inc}

type
  { A simple particle which is exactly 1 pixel big which positions itself randomly on the screen and then simply
    fades after some amount of time. }
  TStar = class(TParticleEx)
  protected
    procedure ExRender(const Pt: TPoint2px); override;
  public
    constructor Create(const AOwner: TParticles); reintroduce;
  end;

  // A particle which shows text aligned horizontally for a short period of time.
  TText = class(TParticleEx)
  private
    FText: StdString;
    FFontIndex: Integer;
    FSize: Integer;
  protected
    procedure ExRender(const Pt: TPoint2px); override;
  public
    constructor Create(const AOwner: TParticles; const Text: StdString; const FontIndex, X, Y, Size: Integer;
      const Color: TIntColor); reintroduce;
  end;

  { A helper class which provides movement control for all space objects.
    (if an object leaves the screen, it appears from the opposite direction) }
  TSpaceObject = class(TBaseObject)
  private
    FAngle: VectorFloat;
    FAngleVel: VectorFloat;
    FImageIndex: Integer;
    FSize: Integer;
    FPattern: Integer;
    FEffect: TBlendingEffect;
    FDiffuse: Cardinal;

    procedure SetAngle(const Value: VectorFloat);
    procedure SetAngleVel(const Value: VectorFloat);
    procedure SetSize(const Value: Integer);
  protected
    procedure UpdateAngle; virtual;
    procedure UpdateSize; virtual;
  public
    constructor Create(const AOwner: TBaseObjects);

    procedure Move; override;
    procedure Render(const Tag: TObject); override;

    property Angle: VectorFloat read FAngle write SetAngle;
    property AngleVel: VectorFloat read FAngleVel write SetAngleVel;

    // Image info
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Pattern: Integer read FPattern write FPattern;
    property Size: Integer read FSize write SetSize;
    property Effect: TBlendingEffect read FEffect write FEffect;
    property Diffuse: Cardinal read FDiffuse write FDiffuse;
  end;

  // A class which implements user-controlled ship.
  TShip = class(TSpaceObject)
  private
    EngineSmoke: Integer;
    WeaponCharge: Integer;
    MaxWeaponCharge: Integer;
    FScore: Integer;
    FWeaponIndex: Integer;
    FLife: Integer;
    FArmour: Integer;

    procedure SetScore(const Value: Integer);
    procedure SetWeaponIndex(const Value: Integer);
    procedure SetLife(const Value: Integer);
  protected
    procedure UpdateAngle; override;
    procedure CollideCheck(const DestObj: TBaseObject; const Distance: Integer; var Accept: Boolean); override;
    procedure ObjectCollide(const DestObj: TBaseObject); override;
  public
    constructor Create(const AOwner: TBaseObjects);

    procedure TurnLeft;
    procedure TurnRight;
    procedure Accelerate;
    procedure Brake;
    function Shoot: Boolean;

    procedure Move; override;
    procedure Render(const Tag: TObject); override;

    property Score: Integer read FScore write SetScore;
    property WeaponIndex: Integer read FWeaponIndex write SetWeaponIndex;
    property Life: Integer read FLife write SetLife;
    property Armour: Integer read FArmour write FArmour;
  end;

  // A class implementing Asteroid behaviour.
  TAsteroid = class(TSpaceObject)
  private
    Anim, AnimDelta: Integer;
    FScore: Integer;
  protected
    procedure CollideCheck(const DestObj: TBaseObject; const Distance: Integer; var Accept: Boolean); override;
    procedure UpdateSize; override;
    procedure ObjectCollide(const DestObj: TBaseObject); override;
  public
    constructor Create(const AOwner: TBaseObjects);

    procedure Move; override;

    property Score: Integer read FScore write FScore;
  end;

  // Bullet that is fired from the ship.
  TBullet = class(TSpaceObject)
  private
    Anim: Integer;
    FRange: Integer;
  protected
    procedure CollideCheck(const DestObj: TBaseObject; const Distance: Integer; var Accept: Boolean); override;
    procedure ObjectCollide(const DestObj: TBaseObject); override;
  public
    constructor Create(const AOwner: TBaseObjects);

    procedure Move; override;

    property Range: Integer read FRange write FRange;
  end;

var
  ShipID: Integer = -1; // that ID represents the player's ship Globally

  PEngine1: TParticles = nil;
  OEngine1: TBaseObjects = nil;
  PEngine2: TParticles = nil;

implementation

uses
  SysUtils, Math, PXL.Images, PXL.Fonts, Engine.Globals, Sound.Globals;

const
  StarOrderIndex = $100;
  TextOrderIndex = $200;
  StarColors: array[0..3] of TIntColor = ($FFFFFFFF, $FFFF7F00, $FF3F7FFF, $FFFFE000);
  ScreenWidth = 640;
  ScreenHeight = 480;
  AccelFactor = 0.1;
  ResistFactor = 0.97;
  BrakeFactor = 0.9;
  WeaponSpeed = 24;
  BulletSpeed = 5.0;
  BulletRange = 56;
  TurnSpeed = Pi * 4 / 128;

constructor TStar.Create(const AOwner: TParticles);
begin
  inherited Create(AOwner, StarOrderIndex);

  // set random position
  IntPos := Point2px(Random(ScreenWidth), Random(ScreenHeight));

  // one-pixel dimensions
  RenderSize := Point2px(1, 1);

  // maximum star duration
  MaxRange := 32 + (32 * Random(8));

  // star color
  Diffuse := StarColors[Random(4)];
end;

procedure TStar.ExRender(const Pt: TPoint2px);
var
  Alpha: Integer;
begin
  // 1. fade the pixel when cycle reaches its end
  Alpha := 255 - ((CurRange * 255) div MaxRange);

  // 2. render the pixel
  EngineCanvas.PutPixel(Pt, IntColor(Diffuse, Alpha));
end;

constructor TText.Create(const AOwner: TParticles; const Text: StdString; const FontIndex, X, Y, Size: Integer;
  const Color: TIntColor);
var
  Font: TBitmapFont;
begin
  inherited Create(AOwner, TextOrderIndex);

  // assign text attributes
  Diffuse := Color;
  FText := Text;
  FSize := Size;
  FFontIndex := FontIndex;

  // retreive a specific font
  Font := EngineFonts[FontIndex];

  // determine text dimensions
  if Font <> nil then
  begin
    Font.Scale := FSize / 256.0;
    RenderSize := Font.TextExtentInt(FText);
  end;

  // centered position
  IntPos := Point2px(X - (RenderSize.X div 2), Y - (RenderSize.Y div 2));

  // text duration
  MaxRange := 128 + (Random(16) * 8);
end;

procedure TText.ExRender(const Pt: TPoint2px);
var
  Alpha: Integer;
  Font: TBitmapFont;
begin
  // fade the text when cycle reaches its end
  Alpha := 255;

  if CurRange >= MaxRange div 2 then
    Alpha := Min((255 - ((CurRange * 255) div MaxRange)) * 2, 255);

  if CurRange < MaxRange div 4 then
    Alpha := Min(((CurRange * 255) div MaxRange) * 4, 255);

  // retreive the required font
  Font := EngineFonts[FFontIndex];

  // render text
  if Font <> nil then
  begin
    Font.Scale := FSize / 256;
    Font.DrawText(Pt, FText, Diffuse, Alpha / 255.0);
  end;
end;

constructor TSpaceObject.Create(const AOwner: TBaseObjects);
begin
  inherited;

  IntPos := Point2px(Random(ScreenWidth), Random(ScreenHeight));
  Angle := Random * Pi * 2;
  FImageIndex := -1;
  FSize := 256;
  FDiffuse := $FFFFFFFF;
  FEffect := TBlendingEffect.Normal;
end;

procedure TSpaceObject.Move;
var
  Image: TAtlasImage;
  VisibleSize: TPoint2px;
  LSize, LX, LY: VectorFloat;
begin
  Image := EngineImages[ImageIndex];
  if Image <> nil then
  begin
    LX := Position.X;
    LY := Position.Y;

    VisibleSize := GetImageVisibleSize(Image);
    LSize := Max(VisibleSize.X, VisibleSize.Y) * FSize / 256;

    if LX < -(LSize / 2.0) then
      LX := LX + ScreenWidth + LSize;

    if LX > ScreenWidth + (LSize / 2.0) then
      LX := LX - ScreenWidth - LSize;

    if LY < -(LSize / 2.0) then
      LY := LY + ScreenHeight + LSize;

    if LY > ScreenHeight + (LSize / 2.0) then
      LY := LY - ScreenHeight - LSize;

    Position := Point2(LX, LY);
  end;

  inherited Move;
end;

procedure TSpaceObject.SetAngle(const Value: VectorFloat);
begin
  FAngle := Value;

  while FAngle < 0.0 do
    FAngle := FAngle + (2 * Pi);

  while FAngle > 2 * Pi do
    FAngle := FAngle - (2 * Pi);

  UpdateAngle;
end;

procedure TSpaceObject.SetAngleVel(const Value: VectorFloat);
begin
  FAngleVel := Value;
  UpdateAngle;
end;

procedure TSpaceObject.UpdateAngle;
begin
end;

procedure TSpaceObject.Render(const Tag: TObject);
var
  Image: TAtlasImage;
  VisibleSize: TPoint2px;
  LWidth, LHeight: Integer;
begin
  Image := EngineImages[ImageIndex];
  if Image = nil then
    Exit;

  VisibleSize := GetImageVisibleSize(Image);

  LWidth := (VisibleSize.X * Size) div 256;
  LHeight := (VisibleSize.Y * Size) div 256;

  EngineCanvas.UseImageRegion(Image, Pattern);
  EngineCanvas.TexQuad(FloatRect4R(IntPos, Point2(LWidth, LHeight), Point2(LWidth * 0.5, LHeight * 0.5), Angle),
    FDiffuse, FEffect);
end;

procedure TSpaceObject.SetSize(const Value: Integer);
begin
  FSize := Value;
  UpdateSize;
end;

procedure TSpaceObject.UpdateSize;
begin
end;

constructor TShip.Create(const AOwner: TBaseObjects);
begin
  inherited;

  IntPos := Point2px(ScreenWidth div 2, ScreenHeight div 2);
  ImageIndex := ImageShip;
  EngineSmoke := 3;
  CollideRadius := 24;
  MaxWeaponCharge := WeaponSpeed;
  FArmour := 7;
  FLife := 3;
end;

procedure TShip.SetScore(const Value: Integer);
begin
  FScore := Max(Value, 0);
end;

procedure TShip.UpdateAngle;
begin
  Pattern := 16 - Trunc((Angle * 16.0) / Pi);

  while Pattern < 0 do
    Pattern := Pattern + 32;

  while Pattern > 31 do
    Pattern := Pattern - 32;
end;

procedure TShip.TurnLeft;
begin
  Angle := Angle - TurnSpeed;
end;

procedure TShip.TurnRight;
begin
  Angle := Angle + TurnSpeed;
end;

procedure TShip.Accelerate;
var
  Alpha: VectorFloat;
  SmokeX, SmokeY: Integer;
begin
  // convert from 32-based angle (stored in Pattern) to radian system
  Alpha := ((8 - Pattern) * Pi) / 16;

  // accelerate
  Velocity := Point2(Velocity.X + (Cos(Alpha) * AccelFactor), Velocity.Y + (Sin(Alpha) * AccelFactor));

  // certain brake - to limit the speed
  Velocity := Velocity * ResistFactor;

  if EngineSmoke > 6 then
  begin
    SmokeX := IntPos.X + ShipCoords[Pattern, 2].X - 32;
    SmokeY := IntPos.Y + ShipCoords[Pattern, 2].Y - 32;

    PEngine2.CreateParticleEx(ImageCombust, SmokeX, SmokeY, 64).CurRange := 4;
    EngineSmoke := Random(4);

    PlaySample(EffectSamples[2], 33);
  end;
end;

procedure TShip.Brake;
begin
  Velocity := Velocity * BrakeFactor;
end;

procedure TShip.Move;
begin
  Inc(EngineSmoke);
  Inc(WeaponCharge);

  inherited Move;
end;

procedure TShip.Render(const Tag: TObject);
var
  Image: TAtlasImage;
  VisibleSize: TPoint2px;
  Left, Top, Width, Height: Integer;
begin
  // retreive the Image
  Image := EngineImages[ImageIndex];
  if Image = nil then
    Exit;

  VisibleSize := GetImageVisibleSize(Image);

  // calculate Image size
  Width := (VisibleSize.X * Size) div 256;
  Height := (VisibleSize.Y * Size) div 256;
  Left := IntPos.X - (Width div 2);
  Top := IntPos.Y - (Height div 2);

  // render the Image
  EngineCanvas.UseImageRegion(Image, Pattern);
  EngineCanvas.TexQuad(FloatRect4(Left, Top, Width, Height), IntColorWhite4);
end;

procedure TShip.SetWeaponIndex(const Value: Integer);
begin
  FWeaponIndex := Value;

  case FWeaponIndex of
    0: MaxWeaponCharge := Max(WeaponSpeed, 2);
    1: MaxWeaponCharge := Max(Trunc(WeaponSpeed * 0.75), 2);
    2: MaxWeaponCharge := Max(Trunc(WeaponSpeed * 1.05), 2);
    3: MaxWeaponCharge := Max(Trunc(WeaponSpeed * 0.95), 2);
    4: MaxWeaponCharge := Max(Trunc(WeaponSpeed * 0.5), 2);
    5: MaxWeaponCharge := Max(Trunc(WeaponSpeed * 0.7), 2);
    6: MaxWeaponCharge := Max(Trunc(WeaponSpeed * 0.6), 2);
    7: MaxWeaponCharge := Max(Trunc(WeaponSpeed * 0.5), 2);
  end;
end;

function TShip.Shoot: Boolean;
const
  Colors: array[0..3] of TIntColor = ($9FFF0000, $9FFF00FF, $9F00FFFF, $9FFFFFFF);
var
  Alpha, Beta, Theta, BetaInc: VectorFloat;
  I, J, BulletCount, WeaponRange: Integer;
  BulletColor: TIntColor;
  Bullet: TBullet;
begin
  Result := False;
  if WeaponCharge < MaxWeaponCharge then
    Exit;

  Alpha := ((8 - Pattern) * Pi) / 16;
  Beta := Pi / 32;

  for I := 0 to 1 do
    case WeaponIndex of
      0:
      begin
        Bullet := TBullet.Create(Owner);
        Bullet.Range := BulletRange;
        Bullet.Position := Point2(Position.X + ShipCoords[Pattern, I].X - 32, Position.Y +
          ShipCoords[Pattern, I].Y - 32);
        Bullet.Velocity := Point2(Velocity.X + (Cos(Alpha) * BulletSpeed), Velocity.Y + (Sin(Alpha) * BulletSpeed));
        Bullet.Diffuse := $9FFF0000;
      end;
      1:
      begin
        Bullet := TBullet.Create(Owner);
        Bullet.Range := Trunc(BulletRange * 1.2);
        Bullet.Position := Point2(Position.X + ShipCoords[Pattern, I].X - 32, Position.Y +
          ShipCoords[Pattern, I].Y - 32);
        Bullet.Velocity := Point2(Velocity.X + (Cos(Alpha) * BulletSpeed * 1.1), Velocity.Y +
          (Sin(Alpha) * BulletSpeed * 1.1));
        Bullet.Diffuse := $9FFF3F7F;
      end;
      2:
      begin
        Bullet := TBullet.Create(Owner);
        Bullet.Range := Trunc(BulletRange * 0.75);
        Bullet.Position := Point2(Position.X + ShipCoords[Pattern, I].X - 32, Position.Y +
          ShipCoords[Pattern, I].Y - 32);
        Bullet.Velocity := Point2(Velocity.X + (Cos(Alpha - Beta) * BulletSpeed * 0.8), Velocity.Y +
          (Sin(Alpha - Beta) * BulletSpeed * 0.8));
        Bullet.Diffuse := $9F00FF00;

        Bullet := TBullet.Create(Owner);
        Bullet.Range := Trunc(BulletRange * 0.75);
        Bullet.Position := Point2(Position.X + ShipCoords[Pattern, I].X - 32, Position.Y +
          ShipCoords[Pattern, I].Y - 32);
        Bullet.Velocity := Point2(Velocity.X + (Cos(Alpha + Beta) * BulletSpeed * 0.8), Velocity.Y +
          (Sin(Alpha + Beta) * BulletSpeed * 0.8));
        Bullet.Diffuse := $9F00FF00;
      end;
      3:
      begin
        Beta := Pi / 24;

        Theta := 1.1;
        if I mod 2 = 0 then
          Theta := 0.9;

        BulletColor := $9F3FFF7F;
        if I mod 2 = 0 then
          BulletColor := $9F3F7FFF;

        Bullet := TBullet.Create(Owner);
        Bullet.Range := Trunc(BulletRange * Theta);
        Bullet.Position := Point2(Position.X + ShipCoords[Pattern, I].X - 32, Position.Y +
          ShipCoords[Pattern, I].Y - 32);
        Bullet.Velocity := Point2(Velocity.X + (Cos(Alpha - Beta) * BulletSpeed * Theta), Velocity.Y +
          (Sin(Alpha - Beta) * BulletSpeed * Theta));
        Bullet.Diffuse := BulletColor;

        Theta := 0.9;
        if I mod 2 = 0 then
          Theta := 1.1;

        BulletColor := $9F3F7FFF;
        if I mod 2 = 0 then
          BulletColor := $9F3FFF7F;

        Bullet := TBullet.Create(Owner);
        Bullet.Range := Trunc(BulletRange * Theta);
        Bullet.Position := Point2(Position.X + ShipCoords[Pattern, I].X - 32, Position.Y +
          ShipCoords[Pattern, I].Y - 32);
        Bullet.Velocity := Point2(Velocity.X + (Cos(Alpha + Beta) * BulletSpeed * Theta), Velocity.Y +
          (Sin(Alpha + Beta) * BulletSpeed * Theta));
        Bullet.Diffuse := BulletColor;
      end;
      4, 5, 6, 7:
      begin
        BulletCount := 4 + (WeaponIndex - 4);
        WeaponRange := 12 - ((WeaponIndex - 4) * 2);
        Beta := -Pi / BulletRange;
        BetaInc := (Pi / (WeaponRange / 2)) / BulletCount;

        if WeaponIndex = 4 then
        begin
          if I = 1 then
            Break;

          Beta := Beta - (Pi / 12);
          BulletCount := BulletCount + 1;
        end;

        for J := 0 to BulletCount - 1 do
        begin
          Theta := 1.0 + ((WeaponIndex - 4) * 0.4);
          BulletColor := Colors[WeaponIndex - 4];

          Bullet := TBullet.Create(Owner);
          Bullet.Range := Trunc(BulletRange * (1 / Theta));
          Bullet.Position := Point2(Position.X + ShipCoords[Pattern, I].X - 32, Position.Y +
            ShipCoords[Pattern, I].Y - 32);
          Bullet.Velocity := Point2(Velocity.X + (Cos(Alpha - Beta) * BulletSpeed * Theta), Velocity.Y +
            (Sin(Alpha - Beta) * BulletSpeed * Theta));
          Bullet.Diffuse := BulletColor;
          Beta := Beta + BetaInc;
        end;
      end;
    end;

  WeaponCharge := 0;
  Result := True;
end;

procedure TShip.CollideCheck(const DestObj: TBaseObject; const Distance: Integer; var Accept: Boolean);
begin
  Accept := (DestObj is TAsteroid);
end;

procedure TShip.ObjectCollide(const DestObj: TBaseObject);
begin
  Velocity := Point2(Velocity.X + DestObj.Velocity.X, Velocity.Y + DestObj.Velocity.Y);
  Brake;

  Dec(FArmour);
  Score := Score - 5;
{ if (FArmour < 1) then
  begin
   Dec(FLife);
   FArmour:= 7;
  end;}
end;

procedure TShip.SetLife(const Value: Integer);
begin
  FLife := Value;
end;

constructor TAsteroid.Create(const AOwner: TBaseObjects);
begin
  inherited;

  ImageIndex := ImageRock;
  Velocity := Point2((Random * 8) - 4.0, (Random * 8) - 4.0);

  AnimDelta := 1;
  if Random(2) = 0 then
    AnimDelta := -1;

  AngleVel := (Random - 0.5) * Pi / 4;
end;

procedure TAsteroid.UpdateSize;
begin
  CollideRadius := (48 * FSize) div 256;
end;

procedure TAsteroid.Move;
begin
  Inc(Anim, AnimDelta);
  if Anim < 0 then
    Anim := High(Integer);

  Pattern := (Anim div 3) mod 32;

  inherited Move;
end;

procedure TAsteroid.CollideCheck(const DestObj: TBaseObject; const Distance: Integer; var Accept: Boolean);
begin
  Accept := (DestObj is TBullet) or (DestObj is TShip);
end;

procedure TAsteroid.ObjectCollide(const DestObj: TBaseObject);
var
  Particle: TParticleEx;
  LSize, I, PieceCount: Integer;
  Piece: TAsteroid;
  Text: TText;
  TextContent: StdString;
begin
  Dying := True;

  LSize := (FSize * 192) div 256;

  Particle := PEngine2.CreateParticleEx(ImageExplode, IntPos.X, IntPos.Y, 64, TBlendingEffect.Add);
  Particle.RenderSize := Point2px(LSize, LSize);
  Particle.RotMiddle := Point2px(LSize div 2, LSize div 2);

  if (FSize > 64) and (FScore > 1) then
  begin
    PieceCount := 1;
    if Random(3) = 0 then
      Inc(PieceCount);

    for I := 0 to PieceCount do
    begin
      Piece := TAsteroid.Create(Owner);
      Piece.Size := (FSize * 2) div 3;
      Piece.Position := Point2(Position.X, Position.Y);
      Piece.Score := Score - 1;
    end;
  end;

  TextContent := IntToStr(FScore) + ' point';

  if FScore > 1 then
    TextContent := TextContent + 's';

  TextContent := TextContent + '!';

  Text := TText.Create(PEngine2, TextContent, 1, IntPos.X, IntPos.Y + 24, 256, $FFFFE000);
  Text.MaxRange := 48;
  Text.Velocity := Point2(Random - 0.5, Random - 1.5);

  TShip(Owner.Objects[ShipID]).Score := TShip(Owner.Objects[ShipID]).Score + FScore;
  PlaySample(EffectSamples[1], 50);
end;

constructor TBullet.Create(const AOwner: TBaseObjects);
begin
  inherited;

  ImageIndex := ImageTorpedo;
  FRange := 1;
  Effect := TBlendingEffect.Add;
  Diffuse := $7FFFFFFF;
  CollideRadius := 16;
end;

procedure TBullet.Move;
var
  Alpha: Integer;
begin
  inherited;

  Dec(FRange);
  if FRange < 1 then
    Dying := True;

  Inc(Anim);
  Pattern := Anim mod 32;

  if FRange < BulletRange div 2 then
  begin
    Alpha := (FRange * $9F) div (BulletRange div 2);
    Diffuse := IntColor(FDiffuse or $FF000000, Alpha);
  end;
end;

procedure TBullet.CollideCheck(const DestObj: TBaseObject; const Distance: Integer; var Accept: Boolean);
begin
  Accept := DestObj is TAsteroid;
end;

procedure TBullet.ObjectCollide(const DestObj: TBaseObject);
begin
  Dying := True;
end;

end.
