unit PXL.Canvas.SRT;
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

uses
  PXL.Types, PXL.Surfaces, PXL.Devices, PXL.Textures, PXL.Canvas;

type
  TSRTCanvas = class(TCustomCanvas)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FSurface: TConceptualPixelSurface;
    FClipRect: TIntRect;

    function NeedsInitialization: Boolean; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    function GetClipRect: TIntRect; override;
    procedure SetClipRect(const Value: TIntRect); override;
  public
    procedure PutPixel(const Point: TPoint2; const Color: TIntColor); override;
    procedure Line(const SrcPoint, DestPoint: TPoint2; const Color: TIntColor2); override;

    procedure DrawIndexedTriangles(const Vertices: PPoint2; const Colors: PIntColor; const Indices: PLongInt;
      const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure Flush; override;
  end;

implementation

uses
  PXL.Types.SRT, PXL.Rasterizer.SRT, PXL.Textures.SRT;

function TSRTCanvas.NeedsInitialization: Boolean;
begin
  Result := False;
end;

function TSRTCanvas.BeginDraw: Boolean;
var
  Context: TSRTDeviceContext;
begin
  if (Device = nil) or (not (Device.Context is TSRTDeviceContext)) then
    Exit(False);

  Context := TSRTDeviceContext(Device.Context);

  FSurface := Context.Surface;
  if FSurface = nil then
    Exit(False);

  FClipRect := IntRect(ZeroPoint2px, Context.SurfaceSize);

  Result := True;
end;

procedure TSRTCanvas.EndDraw;
begin
  FSurface := nil;
end;

function TSRTCanvas.GetClipRect: TIntRect;
begin
  Result := FClipRect;
end;

procedure TSRTCanvas.SetClipRect(const Value: TIntRect);
begin
  FClipRect := Value;
end;

procedure TSRTCanvas.Flush;
begin
end;

procedure TSRTCanvas.PutPixel(const Point: TPoint2; const Color: TIntColor);
var
  IntPoint: TPoint2px;
begin
  if FSurface <> nil then
  begin
    IntPoint := Point2ToPx(Point);

    if PointInRect(IntPoint, FClipRect) then
      FSurface.DrawPixelUnsafe(IntPoint, Color);
  end;
end;

procedure TSRTCanvas.Line(const SrcPoint, DestPoint: TPoint2; const Color: TIntColor2);
var
  SrcPt, DestPt, Delta, DrawPos: TPoint2px;
  FixedPos, FixedDelta, InitialPos, I, AlphaPos, AlphaDelta: Integer;
begin
  SrcPt := Point2ToPx(SrcPoint);
  DestPt := Point2ToPx(DestPoint);
  Delta.X := Abs(DestPt.X - SrcPt.X);
  Delta.Y := Abs(DestPt.Y - SrcPt.Y);

  if (Delta.X < 1) and (Delta.Y < 1) then
  begin
    if PointInRect(Point2ToPx((SrcPoint + DestPoint) * 0.5), FClipRect) then
      FSurface.DrawPixelUnsafe(SrcPt, AveragePixels(Color.First, Color.Second));

    Exit;
  end;

  if Delta.Y > Delta.X then
  begin
    InitialPos := SrcPt.Y;
    FixedDelta := Round((DestPoint.X - SrcPoint.X) * 65536.0) div Delta.Y;
    AlphaDelta := $FFFF div Delta.Y;

    if DestPt.Y < InitialPos then
    begin
      InitialPos := DestPt.Y;

      FixedPos :=  Round(DestPoint.X * 65536.0);
      FixedDelta := -FixedDelta;

      AlphaPos := $FFFF;
      AlphaDelta := -AlphaDelta;
    end
    else
    begin
      FixedPos := Round(SrcPoint.X * 65536.0);
      AlphaPos := 0;
    end;

    for I := 0 to Delta.Y - 1 do
    begin
      DrawPos := Point2px(FixedPos div 65536, InitialPos + I);

      if PointInRect(DrawPos, FClipRect) then
        FSurface.DrawPixelUnsafe(DrawPos, BlendPixels(Color.First, Color.Second, AlphaPos div 256));

      Inc(FixedPos, FixedDelta);
      Inc(AlphaPos, AlphaDelta);
    end;
  end
  else
  begin
    InitialPos := SrcPt.X;
    FixedDelta := Round((DestPoint.Y - SrcPoint.Y) * 65536.0) div Delta.X;
    AlphaDelta := $FFFF div Delta.X;

    if DestPt.X < InitialPos then
    begin
      InitialPos := DestPt.X;

      FixedPos :=  Round(DestPoint.Y * 65536.0);
      FixedDelta := -FixedDelta;

      AlphaPos := $FFFF;
      AlphaDelta := -AlphaDelta;
    end
    else
    begin
      FixedPos := Round(SrcPoint.Y * 65536.0);
      AlphaPos := 0;
    end;

    for I := 0 to Delta.X - 1 do
    begin
      DrawPos := Point2px(InitialPos + I, FixedPos div 65536);

      if PointInRect(DrawPos, FClipRect) then
        FSurface.DrawPixelUnsafe(DrawPos, BlendPixels(Color.First, Color.Second, AlphaPos div 256));

      Inc(FixedPos, FixedDelta);
      Inc(AlphaPos, AlphaDelta);
    end;
  end;
end;

procedure TSRTCanvas.DrawIndexedTriangles(const Vertices: PPoint2; const Colors: PIntColor; const Indices: PLongInt;
  const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect);
var
  I: Integer;
  Index1, Index2, Index3: PLongInt;
  Vertex1, Vertex2, Vertex3: PPoint2;
  Color1, Color2, Color3: PIntColor;
  Det: Single;
begin
  if (TriangleCount < 1) or (VertexCount < 3) or (FSurface = nil) then
    Exit;

  Index1 := Indices;
  Index2 := Pointer(PtrInt(Indices) + SizeOf(LongInt));
  Index3 := Pointer(PtrInt(Indices) + 2 * SizeOf(LongInt));

  for I := 0 to TriangleCount - 1 do
  begin
    Vertex1 := Pointer(PtrInt(Vertices) + Index1^ * SizeOf(TPoint2));
    Vertex2 := Pointer(PtrInt(Vertices) + Index2^ * SizeOf(TPoint2));
    Vertex3 := Pointer(PtrInt(Vertices) + Index3^ * SizeOf(TPoint2));

    Color1 := Pointer(PtrInt(Colors) + Index1^ * SizeOf(TIntColor));
    Color2 := Pointer(PtrInt(Colors) + Index2^ * SizeOf(TIntColor));
    Color3 := Pointer(PtrInt(Colors) + Index3^ * SizeOf(TIntColor));

    Det := (Vertex1.X - Vertex3.X) * (Vertex2.Y - Vertex3.Y) - (Vertex2.X - Vertex3.X) * (Vertex1.Y - Vertex3.Y);
    if Det > 0 then
      DrawTriangle(FSurface, nil, Vertex3^, Vertex2^, Vertex1^, ZeroPoint2, ZeroPoint2, ZeroPoint2, Color3^, Color2^,
        Color1^, FClipRect, BlendingEffect = TBlendingEffect.Add)
    else
      DrawTriangle(FSurface, nil, Vertex1^, Vertex2^, Vertex3^, ZeroPoint2, ZeroPoint2, ZeroPoint2, Color1^, Color2^,
        Color3^, FClipRect, BlendingEffect = TBlendingEffect.Add);

    Index1 := Pointer(PtrInt(Index1) + 3 * SizeOf(LongInt));
    Index2 := Pointer(PtrInt(Index2) + 3 * SizeOf(LongInt));
    Index3 := Pointer(PtrInt(Index3) + 3 * SizeOf(LongInt));
  end;
end;

procedure TSRTCanvas.DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2;
  const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
  const BlendingEffect: TBlendingEffect);
var
  I: Integer;
  Index1, Index2, Index3: PLongInt;
  Vertex1, Vertex2, Vertex3, TexCoord1, TexCoord2, TexCoord3: PPoint2;
  Color1, Color2, Color3: PIntColor;
begin
  if (TriangleCount < 1) or (VertexCount < 3) or (not (Texture is TSRTLockableTexture)) or (FSurface = nil) then
    Exit;

  Index1 := Indices;
  Index2 := Pointer(PtrInt(Indices) + SizeOf(LongInt));
  Index3 := Pointer(PtrInt(Indices) + 2 * SizeOf(LongInt));

  for I := 0 to TriangleCount - 1 do
  begin
    Vertex1 := Pointer(PtrInt(Vertices) + Index1^ * SizeOf(TPoint2));
    Vertex2 := Pointer(PtrInt(Vertices) + Index2^ * SizeOf(TPoint2));
    Vertex3 := Pointer(PtrInt(Vertices) + Index3^ * SizeOf(TPoint2));

    TexCoord1 := Pointer(PtrInt(TexCoords) + Index1^ * SizeOf(TPoint2));
    TexCoord2 := Pointer(PtrInt(TexCoords) + Index2^ * SizeOf(TPoint2));
    TexCoord3 := Pointer(PtrInt(TexCoords) + Index3^ * SizeOf(TPoint2));

    Color1 := Pointer(PtrInt(Colors) + Index1^ * SizeOf(TIntColor));
    Color2 := Pointer(PtrInt(Colors) + Index2^ * SizeOf(TIntColor));
    Color3 := Pointer(PtrInt(Colors) + Index3^ * SizeOf(TIntColor));

    DrawTriangle(FSurface, TSRTLockableTexture(Texture).Surface, Vertex3^, Vertex2^, Vertex1^, TexCoord3^, TexCoord2^,
      TexCoord1^, Color3^, Color2^, Color1^, FClipRect, BlendingEffect = TBlendingEffect.Add);

    Index1 := Pointer(PtrInt(Index1) + 3 * SizeOf(LongInt));
    Index2 := Pointer(PtrInt(Index2) + 3 * SizeOf(LongInt));
    Index3 := Pointer(PtrInt(Index3) + 3 * SizeOf(LongInt));
  end;
end;

end.
