unit Engine.Particles;
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
  PXL.TypeDef, PXL.Types, PXL.Canvas, PXL.Images;

type
  TParticles = class;

  TParticle = class
  private
    FOwner: TParticles;
    FPrev: TParticle;
    FNext: TParticle;
    FOrderIndex: Integer;
    FAccel: TPoint2;
    FPosition: TPoint2;
    FVelocity: TPoint2;
    FMaxRange: VectorInt;
    FCurRange: VectorInt;

    procedure SetOwner(const Value: TParticles);
    procedure SetPrev(const Value: TParticle);
    procedure SetNext(const Value: TParticle);
    procedure SetOrderIndex(const Value: Integer);
    procedure SetPosition(const Value: TPoint2);
    procedure SetVelocity(const Value: TPoint2);
    procedure SetAccel(const Value: TPoint2);
    function GetIntPos: TPoint2px; inline;
    procedure SetIntPos(const Value: TPoint2px); inline;
    procedure SetCurRange(const Value: VectorInt);
    procedure SetMaxRange(const Value: VectorInt);
  protected
    // links previous and next objects leaving this object unconnected
    procedure Unlink;
    // + called AFTER position has been changed
    procedure UpdatedPosition; virtual;
    // + called AFTER velocity has been changed
    procedure UpdatedVelocity; virtual;
    // + called AFTER velocity has been changed
    procedure UpdatedAccel; virtual;
    // + called AFTER any of range variables have been updated
    procedure UpdatedRange; virtual;
  public
    constructor Create(const AOwner: TParticles; const AOrderIndex: Integer); virtual;
    destructor Destroy; override;

    // returns False when particle needs to be destroyed
    function Move: Boolean; virtual;

    procedure Render(const Tag: TObject); virtual; abstract;

    property Owner: TParticles read FOwner write SetOwner;
    property Prev: TParticle read FPrev write SetPrev;
    property Next: TParticle read FNext write SetNext;
    // all particles are sorted by their order for cached rendering
    property OrderIndex: Integer read FOrderIndex write SetOrderIndex;

    // particle position vector
    property Position: TPoint2 read FPosition write SetPosition;
    property Velocity: TPoint2 read FVelocity write SetVelocity;
    // particle acceleration
    property Accel: TPoint2 read FAccel write SetAccel;

    // integer position
    property IntPos: TPoint2px read GetIntPos write SetIntPos;

    // current range the particle has travelled
    property CurRange: VectorInt read FCurRange write SetCurRange;
    // maximum range for the particle
    property MaxRange: VectorInt read FMaxRange write SetMaxRange;
  end;

  TParticleEx = class(TParticle)
  private
    FEffect: TBlendingEffect;
    FDiffuse4: TIntColor4;
    FImageIndex: Integer;
    FRenderSize: TPoint2px;

    FAngle: VectorFloat;
    FAngleVel: VectorFloat;
    FRotMiddle: TPoint2px;

    procedure SetImageIndex(const Value: Integer);
    procedure SetRenderSize(const Value: TPoint2px);
    procedure SetEffect(const Value: TBlendingEffect);
    procedure SetDiffuse4(const Value: TIntColor4);
    function GetDiffuse: TIntColor; inline;
    procedure SetDiffuse(const Value: TIntColor); inline;
    procedure SetRotMiddle(const Value: TPoint2px);
    procedure SetAngle(const Value: VectorFloat);
    procedure SetAngleVel(const Value: VectorFloat);
  protected
    // + called AFTER image index or size has been changed
    procedure UpdatedImage; virtual;
    // + called AFTER image effect or diffuse color have been changed
    procedure UpdatedEffect; virtual;
    // + called AFTER particle rotation parameters have been changed or
    // the particle has been rotated
    procedure UpdatedRotation; virtual;
    // * called to render the particle
    //   "Pt" represents the middle Point2px of the particle on screen
    procedure ExRender(const Pt: TPoint2px); virtual;
  public
    constructor Create(const AOwner: TParticles; const AOrderIndex: Integer); override;

    function Move: Boolean; override;
    procedure Render(const Tag: TObject); override;

    // visible index
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    // rendering size
    property RenderSize: TPoint2px read FRenderSize write SetRenderSize;
    // rendering info
    property Effect: TBlendingEffect read FEffect write SetEffect;
    property Diffuse4: TIntColor4 read FDiffuse4 write SetDiffuse4;
    property Diffuse: TIntColor read GetDiffuse write SetDiffuse;

    // particle rotation info
    property RotMiddle: TPoint2px read FRotMiddle write SetRotMiddle;
    // angle and rotation speed (in radians)
    property Angle: VectorFloat read FAngle write SetAngle;
    property AngleVel: VectorFloat read FAngleVel write SetAngleVel;
  end;

  TParticles = class
  private
    function GetCount: Integer;
  protected
    FListHead: TParticle;
    FListTail: TParticle;

    function Linked(const ANode: TParticle): Boolean;
    procedure Insert(const ANode: TParticle); virtual;
    procedure UnlinkObj(const ANode: TParticle); virtual;
  public
    constructor Create;

    // removes all particles from list
    procedure Clear;

    // moves and updates all particles
    procedure Update;

    // Renders all particles on the screen ("Tag" is simply passed to individual
    // object).
    // NOTE: Particles that derive TParticleEx can take advantage from Tag, if
    // you pass TVScreen as its value.
    procedure Render(const Tag: TObject);

    // adds new TParticleExplosion and returns pointer to it
    function CreateParticleEx(const ImageNum, Xpos, Ypos, Cycle: Integer;
      const Effect: TBlendingEffect = TBlendingEffect.Normal): TParticleEx; overload;

    property Count: Integer read GetCount;
  end;

function GetImageVisibleSize(const Image: TAtlasImage): TPoint2px;

implementation

uses
  Engine.Globals;

{$REGION 'Global Functions'}

function GetImageVisibleSize(const Image: TAtlasImage): TPoint2px;
begin
  if Image.Regions.Count > 0 then
    Result := Image.Regions[0].Rect.Size
  else if Image.TextureCount > 0 then
    Result := Image.Texture[0].Size
  else
    Result := ZeroPoint2px;
end;

{$ENDREGION}
{$REGION 'TParticle'}

constructor TParticle.Create(const AOwner: TParticles; const AOrderIndex: Integer);
begin
  inherited Create;

  FOrderIndex := AOrderIndex;
  FOwner := AOwner;
  FMaxRange := 1;

  if FOwner <> nil then
    FOwner.Insert(Self);
end;

destructor TParticle.Destroy;
begin
  Unlink;

  inherited;
end;

procedure TParticle.SetPrev(const Value: TParticle);
var
  LPrev: TParticle;
begin
  // determine previous forward link
  LPrev := nil;
  if (FPrev <> nil) and (FPrev.Next = Self) then
    LPrev := FPrev;

  // update link
  FPrev := Value;

  // remove previous forward link
  if LPrev <> nil then
    LPrev.Next := nil;

  // insert forward link
  if (FPrev <> nil) and (FPrev.Next <> Self) then
    FPrev.Next := Self;
end;

procedure TParticle.SetNext(const Value: TParticle);
var
  LNext: TParticle;
begin
  // determine previous backward link
  LNext := nil;
  if (FNext <> nil) and (FNext.Prev = Self) then
    LNext := FNext;

  // update link
  FNext := Value;

  // remove previous backward link
  if LNext <> nil then
    LNext.Prev := nil;

  // insert backward link
  if (FNext <> nil) and (FNext.Prev <> Self) then
    FNext.Prev := Self;
end;

procedure TParticle.Unlink;
var
  LPrev, LNext: TParticle;
begin
  // unlink the object from its owner
  if FOwner <> nil then
    FOwner.UnlinkObj(Self);

  // unlink previous node
  LPrev := FPrev;
  LNext := FNext;
  FPrev := nil;
  FNext := nil;

  if LPrev = nil then
  begin
    if LNext <> nil then
      LNext.Prev := nil;
  end
  else
    LPrev.Next := LNext;
end;

procedure TParticle.SetOwner(const Value: TParticles);
begin
  // unlink the node
  Unlink;

  // switch owner
  FOwner := Value;

  // re-insert the node
  if FOwner <> nil then
    FOwner.Insert(Self);
end;

procedure TParticle.SetOrderIndex(const Value: Integer);
begin
  // unlink the node
  Unlink;

  // update order index
  FOrderIndex := Value;

  // re-insert the particle
  if FOwner <> nil then
    FOwner.Insert(Self);
end;

procedure TParticle.SetPosition(const Value: TPoint2);
begin
  FPosition := Value;
  UpdatedPosition;
end;

procedure TParticle.SetVelocity(const Value: TPoint2);
begin
  FVelocity := Value;
  UpdatedVelocity;
end;

procedure TParticle.SetAccel(const Value: TPoint2);
begin
  FAccel := Value;
  UpdatedAccel;
end;

function TParticle.GetIntPos: TPoint2px;
begin
  Result := Point2ToPx(FPosition);
end;

procedure TParticle.SetIntPos(const Value: TPoint2px);
begin
  Position := Value;
end;

procedure TParticle.SetCurRange(const Value: VectorInt);
begin
  FCurRange := Value;
  UpdatedRange;
end;

procedure TParticle.SetMaxRange(const Value: VectorInt);
begin
  FMaxRange := Value;
  UpdatedRange;
end;

procedure TParticle.UpdatedPosition;
begin
end;

procedure TParticle.UpdatedVelocity;
begin
end;

procedure TParticle.UpdatedAccel;
begin
end;

procedure TParticle.UpdatedRange;
begin
end;

function TParticle.Move: Boolean;
begin
  // accelerate
  FVelocity := FVelocity + FAccel;
  UpdatedVelocity;

  // move
  FPosition := FPosition + FVelocity;
  UpdatedPosition;

  // update particle's range
  Inc(FCurRange);
  UpdatedRange;

  Result := FCurRange < FMaxRange;
end;

{$ENDREGION}
{$REGION 'TParticleEx'}

constructor TParticleEx.Create(const AOwner: TParticles; const AOrderIndex: Integer);
begin
  inherited;

  FDiffuse4 := IntColorWhite4;
  FEffect := TBlendingEffect.Normal;
  FImageIndex := -1;
  FAngle := Random * Pi * 2;
end;

procedure TParticleEx.SetAngle(const Value: VectorFloat);
begin
  FAngle := Value;
  UpdatedRotation;
end;

procedure TParticleEx.SetAngleVel(const Value: VectorFloat);
begin
  FAngleVel := Value;
  UpdatedRotation;
end;

procedure TParticleEx.SetDiffuse(const Value: TIntColor);
begin
  Diffuse4 := Value;
end;

function TParticleEx.GetDiffuse: TIntColor;
begin
  Result := AverageFourPixels(FDiffuse4.TopLeft, FDiffuse4.TopRight, FDiffuse4.BottomRight, FDiffuse4.BottomLeft);
end;

procedure TParticleEx.SetEffect(const Value: TBlendingEffect);
begin
  FEffect := Value;
  UpdatedEffect;
end;

procedure TParticleEx.SetImageIndex(const Value: Integer);
var
  Image: TAtlasImage;
begin
  // attempt to retreive image parameters
  if (FImageIndex = -1) and (Value >= 0) then
  begin
    Image := EngineImages[Value];
    if Image <> nil then
    begin
      FRenderSize := GetImageVisibleSize(Image);
      FRotMiddle := Point2ToPx(TPoint2(FRenderSize) * 0.5);
    end;
  end;

  FImageIndex := Value;
  UpdatedImage;
end;

procedure TParticleEx.SetRenderSize(const Value: TPoint2px);
begin
  FRenderSize := Value;
  UpdatedImage;
end;

procedure TParticleEx.SetRotMiddle(const Value: TPoint2px);
begin
  FRotMiddle := Value;
  UpdatedImage;
end;

procedure TParticleEx.SetDiffuse4(const Value: TIntColor4);
begin
  FDiffuse4 := Value;
  UpdatedEffect;
end;

procedure TParticleEx.UpdatedImage;
begin
end;

procedure TParticleEx.UpdatedEffect;
begin
end;

procedure TParticleEx.UpdatedRotation;
begin
end;

function TParticleEx.Move: Boolean;
begin
  // update angle
  FAngle := FAngle + FAngleVel;

  while FAngle > Pi * 2 do
    FAngle := FAngle - (Pi * 2);

  UpdatedRotation;

  // move the particle
  Result := inherited Move;
end;

procedure TParticleEx.Render(const Tag: TObject);
var
  Pt: TPoint2px;
begin
  Pt := IntPos;

  if not OverlapRect(IntRect(Pt.X - RotMiddle.X, Pt.Y - RotMiddle.Y, RenderSize.X, RenderSize.Y),
    IntRect(0, 0, DisplaySize.X, DisplaySize.Y)) then
    Exit;

  ExRender(Pt);
end;

procedure TParticleEx.ExRender(const Pt: TPoint2px);
var
  Image: TAtlasImage;
  Pattern: Integer;
begin
  Image := EngineImages[ImageIndex];
  if Image = nil then
    Exit;

  Pattern := 0;

  if MaxRange > 0 then
    Pattern := (CurRange * Image.Regions.Count) div MaxRange;

  EngineCanvas.UseImageRegion(Image, Pattern);
  EngineCanvas.TexQuad(FloatRect4R(Pt, RenderSize, RotMiddle, Angle), Diffuse4, Effect);
end;

{$ENDREGION}
{$REGION 'TParticles'}

constructor TParticles.Create;
begin
  inherited;

  FListHead := nil;
  FListTail := nil;
end;

procedure TParticles.Clear;
var
  LNode, LPrev: TParticle;
begin
  LNode := FListTail;

  while LNode <> nil do
  begin
    LPrev := LNode.Prev;
    LNode.Free;
    LNode := LPrev;
  end;
end;

function TParticles.GetCount: Integer;
var
  LNode: TParticle;
begin
  Result := 0;
  LNode := FListHead;

  while LNode <> nil do
  begin
    Inc(Result);
    LNode := LNode.Next;
  end;
end;

function TParticles.Linked(const ANode: TParticle): Boolean;
var
  LNode1, LNode2: TParticle;
begin
  // validate initial object
  Result := False;
  if ANode = nil then
    Exit;

  // start from opposite ends
  LNode1 := FListHead;
  LNode2 := FListTail;

  // do bi-directional search
  while (LNode1 <> nil) or (LNode2 <> nil) do
  begin
    // compare the objects
    if (LNode1 = ANode) or (LNode2 = ANode) then
      Exit(True);

    // advance in the list
    if LNode1 <> nil then
      LNode1 := LNode1.Next;

    if LNode2 <> nil then
      LNode2 := LNode2.Prev;
  end;
end;

procedure TParticles.Insert(const ANode: TParticle);
var
  LOrderIndex: Integer;
  LNode: TParticle;
begin
  // do not accept NULL objects
  if ANode = nil then
    Exit;

  // retreive order index
  LOrderIndex := ANode.OrderIndex;

  // check if the particle is already linked into the list
  if Linked(ANode) then
    Exit;

  // if no items available - create a first element
  if FListHead = nil then
  begin
    ANode.Prev := nil;
    ANode.Next := nil;
    FListHead := ANode;
    FListTail := FListHead;
    Exit;
  end;

  // insert BEFORE first element
  if LOrderIndex <= FListHead.OrderIndex then
  begin
    ANode.Prev := nil;
    ANode.Next := FListHead;
    FListHead := ANode;
    Exit;
  end;

  // insert AFTER first element
  if LOrderIndex >= FListTail.OrderIndex then
  begin
    ANode.Next := nil;
    FListTail.Next := ANode;
    FListTail := ANode;
    Exit;
  end;

  // search using either fordward or backward method
  if Abs(Int64(FListHead.OrderIndex) - LOrderIndex) < Abs(Int64(FListTail.OrderIndex) - LOrderIndex) then
  begin
    // forward search
    LNode := FListHead;

    while LNode.Next.OrderIndex < LOrderIndex do
      LNode := LNode.Next;

    // update links
    ANode.Next := LNode.Next;
    ANode.Prev := LNode;
  end
  else
  begin
    // backward search
    LNode := FListTail;

    while LNode.Prev.OrderIndex > LOrderIndex do
      LNode := LNode.Prev;

    // update links
    ANode.Prev := LNode.Prev;
    ANode.Next := LNode;
  end;
end;

procedure TParticles.UnlinkObj(const ANode: TParticle);
begin
  if FListTail = ANode then
    FListTail := FListTail.Prev;

  if FListHead = ANode then
  begin
    FListHead := nil;

    if ANode.Next <> nil then
      FListHead := ANode.Next;
  end;
end;

procedure TParticles.Update;
var
  Aux, pNext: TParticle;
  PForward: Boolean;
begin
  // decide random direction for processing
  PForward := Random(2) = 0;

  Aux := FListHead;
  if not PForward then
    Aux := FListTail;

  // update all particles
  while Aux <> nil do
  begin
    // determine next particle
    pNext := Aux.Next;
    if not PForward then
      pNext := Aux.Prev;

    // move current particle
    if not Aux.Move then
      Aux.Free();

    // advance in the list
    Aux := pNext;
  end;
end;

function TParticles.CreateParticleEx(const ImageNum, Xpos, Ypos, Cycle: Integer;
  const Effect: TBlendingEffect): TParticleEx;
var
  Image: TAtlasImage;
begin
  Result := nil;

  Image := EngineImages[ImageNum];
  if Image = nil then
    Exit;

  Result := TParticleEx.Create(Self, ImageNum);

  Result.IntPos := Point2px(Xpos, Ypos);
  Result.RenderSize := GetImageVisibleSize(Image);
  Result.Effect := Effect;
  Result.ImageIndex := ImageNum;
  Result.Angle := Random(256);
  Result.MaxRange := Cycle;
end;

procedure TParticles.Render(const Tag: TObject);
var
  LNode: TParticle;
begin
  LNode := FListHead;

  while (LNode <> nil) do
  begin
    LNode.Render(Tag);
    LNode := LNode.Next;
  end;
end;

{$ENDREGION}

end.
