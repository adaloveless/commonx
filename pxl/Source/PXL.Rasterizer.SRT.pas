unit PXL.Rasterizer.SRT;
{
  This file is part of Asphyre Framework, also known as Pascal eXtended Library (PXL).
  Copyright (c) 2000 - 2015  Yuriy Kotsarenko

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  This triangle rasterization code is based on C/C++ affine texture mapping code, that was originally published long
  time ago in "fatmap2.zip" package by Mats Byggmastar, 1997.

  Initial Pascal translation and adaptation was made by Yuriy Kotsarenko in November 2007.

  The code has been rewritten to use vertex indices and avoid pointer math as much as possible, in addition to
  interpolating colors in August, 2015 by Yuriy Kotsarenko. "Inner loop" code is made with preference on stability
  rather the performance, guarding against possible access violations and overflows.
}
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.TypeDef, PXL.Types, PXL.Surfaces;

// Renders colored and/or textured triangle on destination surface.
// Note that the vertices should be specified in anti-clockwise order.
procedure DrawTriangle(const Surface: TConceptualPixelSurface; const Texture: TPixelSurface;
  const Pos1, Pos2, Pos3: TPoint2; TexPos1, TexPos2, TexPos3: TPoint2; const Color1, Color2, Color3: TIntColor;
  const ClipRect: TIntRect; const BlendAdd: Boolean);

implementation

type
  TRasterInt = Integer;

  TRasterPoint = record
    X, Y: TRasterInt;
  end;

  TRasterColor = record
    R, G, B, A: TRasterInt;
  end;

  TRightSection = record
    VertexIndex: TRasterInt;
    X: TRasterInt;       // right edge X position
    Delta: TRasterInt;   // right edge X velocity (dx/dy)
    Height: TRasterInt;  // right section vertical height
  end;

  TLeftSection = record
    VertexIndex: TRasterInt;
    X: TRasterInt;       // left edge X position
    Delta: TRasterInt;   // left edge X velocity (dx/dy)
    Height: TRasterInt;  // left section vertical height
    TexCoord: TRasterPoint;
    TexCoordDelta: TRasterPoint; // du/dy, dv/dy
    Color: TRasterColor;
    ColorDelta: TRasterColor;
  end;

  TVertexPoint = record
    Position: TRasterPoint;
    TexCoord: TRasterPoint;
    Color: TRasterColor;
  end;

  TRasterSettings = record
    Textured: Boolean;
    Colored: Boolean;
    BlendAdd: Boolean;
    ClipRect: TIntRect;
  end;

  TVertices = array[0..2] of TVertexPoint;

function FixedCeil16(const Value: TRasterInt): TRasterInt; inline;
begin
  Result := (Value + 65535) div 65536;
end;

function FixedMultiply14(const Value1, Value2: TRasterInt): TRasterInt; inline;
begin
  Result := (Int64(Value1) * Value2) div 16384;
end;

function FixedMultiply16(const Value1, Value2: TRasterInt): TRasterInt; inline;
begin
  Result := (Int64(Value1) * Value2) div 65536;
end;

function FixedDivide16(const Value1, Value2: TRasterInt): TRasterInt; inline;
begin
  Result := (Int64(Value1) * 65536) div Value2;
end;

function FloatToFixed(const Value: Single): TRasterInt; inline; overload;
begin
  Result := Round(Value * 65536.0);
end;

function FloatToFixed(const Point: TPoint2): TRasterPoint; inline; overload;
begin
  Result.X := FloatToFixed(Point.X);
  Result.Y := FloatToFixed(Point.Y);
end;

function FloatToFixed(const Color: TIntColor): TRasterColor; inline; overload;
begin
  Result.R := TRasterInt(TIntColorRec(Color).Red) * 65536;
  Result.G := TRasterInt(TIntColorRec(Color).Green) * 65536;
  Result.B := TRasterInt(TIntColorRec(Color).Blue) * 65536;
  Result.A := TRasterInt(TIntColorRec(Color).Alpha) * 65536;
end;

function FloatToFixedHalfShift(const Point: TPoint2): TRasterPoint; inline;
begin
  Result.X := FloatToFixed(Point.X - 0.5);
  Result.Y := FloatToFixed(Point.Y - 0.5);
end;

procedure RenderScanlineTexturedColored(const Surface: TConceptualPixelSurface; const Texture: TPixelSurface;
  const LineWidth: TRasterInt; const DestPos, TexCoord, TexCoordDelta: TRasterPoint;
  const Color, ColorDelta: TRasterColor; const ClipRect: TIntRect; const BlendAdd: Boolean);
var
  SourceColor, ModulateColor: TIntColor;
  CurPos: TRasterPoint;
  CurColor: TRasterColor;
  I, DestX: Integer;
begin
  CurPos := TexCoord;
  CurColor := Color;

  for I := 0 to LineWidth - 1 do
  begin
    DestX := DestPos.X + I;

    if (DestX >= ClipRect.Left) and (DestX < ClipRect.Right) then
    begin
      ModulateColor := IntColorRGB(
        Saturate(CurColor.R div 65536, 0, 255),
        Saturate(CurColor.G div 65536, 0, 255),
        Saturate(CurColor.B div 65536, 0, 255),
        Saturate(CurColor.A div 65536, 0, 255));

      SourceColor := MultiplyPixels(Texture.Pixels[CurPos.X div 65536, CurPos.Y div 65536], ModulateColor);

      if TIntColorRec(SourceColor).Alpha > 0 then
        if TIntColorRec(SourceColor).Alpha < 255 then
          if not BlendAdd then
            Surface.PixelsUnsafe[DestX, DestPos.Y] := BlendPixels(Surface.PixelsUnsafe[DestX, DestPos.Y], SourceColor,
              TIntColorRec(SourceColor).Alpha)
          else
            Surface.PixelsUnsafe[DestX, DestPos.Y] := AddPixels(Surface.PixelsUnsafe[DestX, DestPos.Y],
              PremultiplyAlpha(SourceColor))
        else if not BlendAdd then
          Surface.PixelsUnsafe[DestX, DestPos.Y] := SourceColor
        else
          Surface.PixelsUnsafe[DestX, DestPos.Y] := AddPixels(Surface.PixelsUnsafe[DestX, DestPos.Y], SourceColor);
    end;

    Inc(CurPos.X, TexCoordDelta.X);
    Inc(CurPos.Y, TexCoordDelta.Y);

    Inc(CurColor.R, ColorDelta.R);
    Inc(CurColor.G, ColorDelta.G);
    Inc(CurColor.B, ColorDelta.B);
    Inc(CurColor.A, ColorDelta.A);
  end;
end;

procedure RenderScanlineTextured(const Surface: TConceptualPixelSurface; const Texture: TPixelSurface;
  const LineWidth: TRasterInt; const DestPos, TexCoord, TexCoordDelta: TRasterPoint; const ClipRect: TIntRect;
  const BlendAdd: Boolean);
var
  SourceColor: TIntColor;
  CurPos: TRasterPoint;
  I, DestX: Integer;
begin
  CurPos := TexCoord;

  for I := 0 to LineWidth - 1 do
  begin
    DestX := DestPos.X + I;

    if (DestX >= ClipRect.Left) and (DestX < ClipRect.Right) then
    begin
      SourceColor := Texture.Pixels[CurPos.X div 65536, CurPos.Y div 65536];

      if TIntColorRec(SourceColor).Alpha > 0 then
        if TIntColorRec(SourceColor).Alpha < 255 then
          if not BlendAdd then
            Surface.PixelsUnsafe[DestX, DestPos.Y] := BlendPixels(Surface.PixelsUnsafe[DestX, DestPos.Y], SourceColor,
              TIntColorRec(SourceColor).Alpha)
          else
            Surface.PixelsUnsafe[DestX, DestPos.Y] := AddPixels(Surface.PixelsUnsafe[DestX, DestPos.Y],
              PremultiplyAlpha(SourceColor))
        else if not BlendAdd then
          Surface.PixelsUnsafe[DestX, DestPos.Y] := SourceColor
        else
          Surface.PixelsUnsafe[DestX, DestPos.Y] := AddPixels(Surface.PixelsUnsafe[DestX, DestPos.Y], SourceColor);
    end;

    Inc(CurPos.X, TexCoordDelta.X);
    Inc(CurPos.Y, TexCoordDelta.Y);
  end;
end;

procedure RenderScanlineColored(const Surface: TConceptualPixelSurface; const LineWidth: TRasterInt;
  const DestPos: TRasterPoint; const Color, ColorDelta: TRasterColor; const ClipRect: TIntRect;
  const BlendAdd: Boolean);
var
  SourceColor: TIntColor;
  CurColor: TRasterColor;
  I, DestX: Integer;
begin
  CurColor := Color;

  for I := 0 to LineWidth - 1 do
  begin
    DestX := DestPos.X + I;

    if (DestX >= ClipRect.Left) and (DestX < ClipRect.Right) then
    begin
      SourceColor := IntColorRGB(
        Saturate(CurColor.R div 65536, 0, 255),
        Saturate(CurColor.G div 65536, 0, 255),
        Saturate(CurColor.B div 65536, 0, 255),
        Saturate(CurColor.A div 65536, 0, 255));

      if TIntColorRec(SourceColor).Alpha > 0 then
        if TIntColorRec(SourceColor).Alpha < 255 then
          if not BlendAdd then
            Surface.PixelsUnsafe[DestX, DestPos.Y] := BlendPixels(Surface.PixelsUnsafe[DestX, DestPos.Y], SourceColor,
              TIntColorRec(SourceColor).Alpha)
          else
            Surface.PixelsUnsafe[DestX, DestPos.Y] := AddPixels(Surface.PixelsUnsafe[DestX, DestPos.Y],
              PremultiplyAlpha(SourceColor))
        else if not BlendAdd then
          Surface.PixelsUnsafe[DestX, DestPos.Y] := SourceColor
        else
          Surface.PixelsUnsafe[DestX, DestPos.Y] := AddPixels(Surface.PixelsUnsafe[DestX, DestPos.Y], SourceColor);
    end;

    Inc(CurColor.R, ColorDelta.R);
    Inc(CurColor.G, ColorDelta.G);
    Inc(CurColor.B, ColorDelta.B);
    Inc(CurColor.A, ColorDelta.A);
  end;
end;

procedure UpdateRightSection(const Vertices: TVertices; var Section: TRightSection);
var
  PrevIndex, FixedHeight, FixedInvHeight, Prestep: TRasterInt;
begin
  // Walk backwards trough the vertex array
  PrevIndex := Section.VertexIndex;

  if Section.VertexIndex > 0 then
    Section.VertexIndex := Section.VertexIndex - 1
  else
    Section.VertexIndex := 2;

  // Calculate number of scanlines in this section
  Section.Height := FixedCeil16(Vertices[Section.VertexIndex].Position.Y) -
    FixedCeil16(Vertices[PrevIndex].Position.Y);

  if Section.Height > 0 then
  begin
    // Guard against possible div overflows
    FixedHeight := Vertices[Section.VertexIndex].Position.Y - Vertices[PrevIndex].Position.Y;

    if Section.Height > 1 then
      // OK, no worries, we have a section that is at least one pixel high. Calculate slope as usual.
      Section.Delta := FixedDivide16(Vertices[Section.VertexIndex].Position.X - Vertices[PrevIndex].Position.X,
        FixedHeight)
    else
    begin
      // FixedHeight is less or equal to one pixel.
      // Calculate slope = width * 1/Fixedheight using 18:14 bit precision to avoid overflows.
      FixedInvHeight := ($10000 shl 14) div FixedHeight;

      Section.Delta := FixedMultiply14(Vertices[Section.VertexIndex].Position.X - Vertices[PrevIndex].Position.X,
        FixedInvHeight);
    end;

    // Prestep initial values
    Prestep := (FixedCeil16(Vertices[PrevIndex].Position.Y) shl 16) - Vertices[PrevIndex].Position.Y;
    Section.X := Vertices[PrevIndex].Position.X + FixedMultiply16(Prestep, Section.Delta);
  end;
end;

procedure UpdateLeftSection(const Vertices: TVertices; var Section: TLeftSection; const Settings: TRasterSettings);
var
  PrevIndex, FixedHeight, FixedInvHeight, Prestep: TRasterInt;
begin
  // Walk forward trough the vertex array
  PrevIndex := Section.VertexIndex;

  if Section.VertexIndex < 2 then
    Section.VertexIndex := Section.VertexIndex + 1
  else
    Section.VertexIndex := 0;

  // Calculate number of scanlines in this section
  Section.Height := FixedCeil16(Vertices[Section.VertexIndex].Position.Y) -
    FixedCeil16(Vertices[PrevIndex].Position.Y);

  if Section.Height > 0 then
  begin
    // Guard against possible div overflows
    FixedHeight := Vertices[Section.VertexIndex].Position.Y - Vertices[PrevIndex].Position.Y;

    if Section.Height > 1 then
    begin
      // OK, no worries, we have a section that is at least one pixel high. Calculate slope as usual.
      Section.Delta := FixedDivide16(Vertices[Section.VertexIndex].Position.X - Vertices[PrevIndex].Position.X,
        FixedHeight);

      if Settings.Textured then
      begin
        Section.TexCoordDelta.X := FixedDivide16(Vertices[Section.VertexIndex].TexCoord.X -
          Vertices[PrevIndex].TexCoord.X, FixedHeight);

        Section.TexCoordDelta.Y := FixedDivide16(Vertices[Section.VertexIndex].TexCoord.Y -
          Vertices[PrevIndex].TexCoord.Y, FixedHeight);
      end;

      if Settings.Colored then
      begin
        Section.ColorDelta.R := FixedDivide16(Vertices[Section.VertexIndex].Color.R -
          Vertices[PrevIndex].Color.R, FixedHeight);

        Section.ColorDelta.G := FixedDivide16(Vertices[Section.VertexIndex].Color.G -
          Vertices[PrevIndex].Color.G, FixedHeight);

        Section.ColorDelta.B := FixedDivide16(Vertices[Section.VertexIndex].Color.B -
          Vertices[PrevIndex].Color.B, FixedHeight);

        Section.ColorDelta.A := FixedDivide16(Vertices[Section.VertexIndex].Color.A -
          Vertices[PrevIndex].Color.A, FixedHeight);
      end;
    end
    else
    begin
      // FixedHeight is less or equal to one pixel.
      // Calculate slope = width * 1/FixedHeight using 18:14 bit precision to avoid overflows.
      FixedInvHeight := ($10000 shl 14) div FixedHeight;

      Section.Delta := FixedMultiply14(Vertices[Section.VertexIndex].Position.X - Vertices[PrevIndex].Position.X,
        FixedInvHeight);

      if Settings.Textured then
      begin
        Section.TexCoordDelta.X := FixedMultiply14(Vertices[Section.VertexIndex].TexCoord.X -
          Vertices[PrevIndex].TexCoord.X, FixedInvHeight);

        Section.TexCoordDelta.Y := FixedMultiply14(Vertices[Section.VertexIndex].TexCoord.Y -
          Vertices[PrevIndex].TexCoord.Y, FixedInvHeight);
      end;

      if Settings.Colored then
      begin
        Section.ColorDelta.R := FixedMultiply14(Vertices[Section.VertexIndex].Color.R -
          Vertices[PrevIndex].Color.R, FixedInvHeight);

        Section.ColorDelta.G := FixedMultiply14(Vertices[Section.VertexIndex].Color.G -
          Vertices[PrevIndex].Color.G, FixedInvHeight);

        Section.ColorDelta.B := FixedMultiply14(Vertices[Section.VertexIndex].Color.B -
          Vertices[PrevIndex].Color.B, FixedInvHeight);

        Section.ColorDelta.A := FixedMultiply14(Vertices[Section.VertexIndex].Color.A -
          Vertices[PrevIndex].Color.A, FixedInvHeight);
      end;
    end;

    // Prestep initial values
    Prestep := (FixedCeil16(Vertices[PrevIndex].Position.Y) shl 16) - Vertices[PrevIndex].Position.Y;

    Section.X := Vertices[PrevIndex].Position.X + FixedMultiply16(Prestep, Section.Delta);

    if Settings.Textured then
    begin
      Section.TexCoord.X := Vertices[PrevIndex].TexCoord.X + FixedMultiply16(Prestep, Section.TexCoordDelta.X);
      Section.TexCoord.Y := Vertices[PrevIndex].TexCoord.Y + FixedMultiply16(Prestep, Section.TexCoordDelta.Y);
    end;

    if Settings.Colored then
    begin
      Section.Color.R := Vertices[PrevIndex].Color.R + FixedMultiply16(Prestep, Section.ColorDelta.R);
      Section.Color.G := Vertices[PrevIndex].Color.G + FixedMultiply16(Prestep, Section.ColorDelta.G);
      Section.Color.B := Vertices[PrevIndex].Color.B + FixedMultiply16(Prestep, Section.ColorDelta.B);
      Section.Color.A := Vertices[PrevIndex].Color.A + FixedMultiply16(Prestep, Section.ColorDelta.A);
    end;
  end;
end;

procedure ComputePolyMargins(const Vertices: TVertices; out PolyTop, PolyBottom, MinIndex, MaxIndex: TRasterInt);
var
  I: TRasterInt;
begin
  PolyTop := Vertices[0].Position.Y;
  PolyBottom := Vertices[0].Position.Y;
  MinIndex := 0;
  MaxIndex := 0;

  for I := 1 to 2 do
  begin
    if Vertices[I].Position.Y < PolyTop then
    begin
      PolyTop := Vertices[I].Position.Y;
      MinIndex := I;
    end;

    if Vertices[I].Position.Y > PolyBottom then
    begin
      PolyBottom := Vertices[I].Position.Y;
      MaxIndex := I;
    end;
  end;
end;

procedure RasterizeTriangle(const Surface: TConceptualPixelSurface; const Texture: TPixelSurface;
  const Vertices: TVertices; const TexCoordDelta: TRasterPoint; const ColorDelta: TRasterColor;
  const Settings: TRasterSettings);
var
  PolyTop, PolyBottom, MinIndex, MaxIndex, LineWidth, Prestep: TRasterInt;
  DestPos, TexCoord: TRasterPoint;
  Color: TRasterColor;
  RightSection: TRightSection;
  LeftSection: TLeftSection;
begin
  ComputePolyMargins(Vertices, PolyTop, PolyBottom, MinIndex, MaxIndex);

  RightSection.VertexIndex := MinIndex;
  LeftSection.VertexIndex := MinIndex;

  // Search for the first usable right section
  repeat
    if RightSection.VertexIndex = MaxIndex then
      Exit;
    UpdateRightSection(Vertices, RightSection);
  until RightSection.Height > 0;

  // Search for the first usable left section
  repeat
    if LeftSection.VertexIndex = MaxIndex then
      Exit;
    UpdateLeftSection(Vertices, LeftSection, Settings);
  until LeftSection.Height > 0;

  DestPos.Y := FixedCeil16(PolyTop);

  while True do
  begin
    DestPos.X := FixedCeil16(LeftSection.X);
    LineWidth := FixedCeil16(RightSection.X) - DestPos.X;

    if (LineWidth > 0) and (DestPos.Y >= Settings.ClipRect.Top) and (DestPos.Y < Settings.ClipRect.Bottom) then
    begin
      // Prestep initial texture u,v
      Prestep := DestPos.X * 65536 - LeftSection.X;

      if Settings.Colored then
      begin
        Color.R := LeftSection.Color.R + FixedMultiply16(Prestep, ColorDelta.R);
        Color.G := LeftSection.Color.G + FixedMultiply16(Prestep, ColorDelta.G);
        Color.B := LeftSection.Color.B + FixedMultiply16(Prestep, ColorDelta.B);
        Color.A := LeftSection.Color.A + FixedMultiply16(Prestep, ColorDelta.A);
      end;

      if Settings.Textured then
      begin
        TexCoord.X := LeftSection.TexCoord.X + FixedMultiply16(Prestep, TexCoordDelta.X);
        TexCoord.Y := LeftSection.TexCoord.Y + FixedMultiply16(Prestep, TexCoordDelta.Y);

        if Settings.Colored then
          RenderScanlineTexturedColored(Surface, Texture, LineWidth, DestPos, TexCoord, TexCoordDelta, Color,
            ColorDelta, Settings.ClipRect, Settings.BlendAdd)
        else
          RenderScanlineTextured(Surface, Texture, LineWidth, DestPos, TexCoord, TexCoordDelta, Settings.ClipRect,
            Settings.BlendAdd);
      end
      else
        RenderScanlineColored(Surface, LineWidth, DestPos, Color, ColorDelta, Settings.ClipRect, Settings.BlendAdd);
    end;

    Inc(DestPos.Y);

    // Scan the right side
    Dec(RightSection.Height);
    if RightSection.Height <= 0 then // End of this section?
    begin
      repeat
        if RightSection.VertexIndex = MaxIndex then
          Exit;
        UpdateRightSection(Vertices, RightSection);
      until RightSection.Height > 0;
    end
    else
      Inc(RightSection.X, RightSection.Delta);

    // Scan the left side
    Dec(LeftSection.Height);
    if LeftSection.Height <= 0 then // End of this section?
    begin
      repeat
        if LeftSection.VertexIndex = MaxIndex then
          Exit;
        UpdateLeftSection(Vertices, LeftSection, Settings);
      until LeftSection.Height > 0;
    end
    else
    begin
      Inc(LeftSection.X, LeftSection.Delta);

      if Settings.Textured then
      begin
        Inc(LeftSection.TexCoord.X, LeftSection.TexCoordDelta.X);
        Inc(LeftSection.TexCoord.Y, LeftSection.TexCoordDelta.Y);
      end;

      if Settings.Colored then
      begin
        Inc(LeftSection.Color.R, LeftSection.ColorDelta.R);
        Inc(LeftSection.Color.G, LeftSection.ColorDelta.G);
        Inc(LeftSection.Color.B, LeftSection.ColorDelta.B);
        Inc(LeftSection.Color.A, LeftSection.ColorDelta.A);
      end;
    end;
  end;
end;

procedure DrawTriangle(const Surface: TConceptualPixelSurface; const Texture: TPixelSurface;
  const Pos1, Pos2, Pos3: TPoint2; TexPos1, TexPos2, TexPos3: TPoint2; const Color1, Color2, Color3: TIntColor;
  const ClipRect: TIntRect; const BlendAdd: Boolean);

  function CalculateDelta(const Value1, Value2, Value3, Pos1, Pos2, Pos3, InvDenom: Single): Integer;
  begin
    Result := Round(((Value1 - Value3) * (Pos2 - Pos3) - (Value2 - Value3) * (Pos1 - Pos3)) * InvDenom);
  end;

var
  Denom, InvDenom: Single;
  TexCoordDelta: TRasterPoint;
  ColorDelta: TRasterColor;
  Vertices: TVertices;
  Settings: TRasterSettings;
begin
  if Texture <> nil then
  begin
    TexPos1 := TexPos1 * TPoint2(Texture.Size);
    TexPos2 := TexPos2 * TPoint2(Texture.Size);
    TexPos3 := TexPos3 * TPoint2(Texture.Size);
  end;

  Denom := (Pos1.X - Pos3.X) * (Pos2.Y - Pos3.Y) - (Pos2.X - Pos3.X) * (Pos1.Y - Pos3.Y);
  if Abs(Denom) <= VectorEpsilon then
    Exit;

  Settings.ClipRect := ClipRect;
  Settings.Textured := Texture <> nil;
  Settings.BlendAdd := BlendAdd;

  if (Color1 = Color2) and (Color2 = Color3) and (Color1 = IntColorWhite) and (Texture <> nil) then
    Settings.Colored := False
  else
    Settings.Colored := True;

  Vertices[0].Position := FloatToFixedHalfShift(Pos1);
  Vertices[1].Position := FloatToFixedHalfShift(Pos2);
  Vertices[2].Position := FloatToFixedHalfShift(Pos3);

  InvDenom := 1.0 / Denom * 65536.0;

  if Settings.Textured then
  begin
    Vertices[0].TexCoord := FloatToFixed(TexPos1);
    Vertices[1].TexCoord := FloatToFixed(TexPos2);
    Vertices[2].TexCoord := FloatToFixed(TexPos3);

    // Calculate du/dx, dv/dy.
    TexCoordDelta.X := CalculateDelta(TexPos1.X, TexPos2.X, TexPos3.X, Pos1.Y, Pos2.Y, Pos3.Y, InvDenom);
    TexCoordDelta.Y := CalculateDelta(TexPos1.Y, TexPos2.Y, TexPos3.Y, Pos1.Y, Pos2.Y, Pos3.Y, InvDenom);
  end;

  if Settings.Colored then
  begin
    Vertices[0].Color := FloatToFixed(Color1);
    Vertices[1].Color := FloatToFixed(Color2);
    Vertices[2].Color := FloatToFixed(Color3);

    ColorDelta.R := CalculateDelta(TIntColorRec(Color1).Red, TIntColorRec(Color2).Red, TIntColorRec(Color3).Red,
      Pos1.Y, Pos2.Y, Pos3.Y, InvDenom);

    ColorDelta.G := CalculateDelta(TIntColorRec(Color1).Green, TIntColorRec(Color2).Green, TIntColorRec(Color3).Green,
      Pos1.Y, Pos2.Y, Pos3.Y, InvDenom);

    ColorDelta.B := CalculateDelta(TIntColorRec(Color1).Blue, TIntColorRec(Color2).Blue, TIntColorRec(Color3).Blue,
      Pos1.Y, Pos2.Y, Pos3.Y, InvDenom);

    ColorDelta.A := CalculateDelta(TIntColorRec(Color1).Alpha, TIntColorRec(Color2).Alpha, TIntColorRec(Color3).Alpha,
      Pos1.Y, Pos2.Y, Pos3.Y, InvDenom);
  end;

  RasterizeTriangle(Surface, Texture, Vertices, TexCoordDelta, ColorDelta, Settings);
end;

end.
