unit PXL.Canvas.DX7;
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
  PXL.Windows.D3D7, PXL.Types, PXL.Textures, PXL.Canvas, PXL.Types.DX7;

type
  TDX7Canvas = class(TCustomCanvas)
  private const
    VertexFVFType = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1;

    { The following parameters roughly affect the rendering performance. The higher values means that more primitives
      will fit in cache, but it will also occupy more bandwidth, even when few primitives are rendered.

      These parameters can be fine-tuned in a finished product to improve the overall performance. }
    MaxCachedVertices = 4096;
    MaxCachedIndices = 4096;
  private type
    TTopology = (Unknown, Points, Lines, Triangles);

    PVertexRecord = ^TVertexRecord;
    TVertexRecord = packed record
      Vertex: D3DVECTOR;
      Rhw: Single;
      Color: LongWord;
      U: Single;
      V: Single;
    end;

    TVertexIndex = Word;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX7DeviceContext;

    FVertexArray: packed array[0..MaxCachedVertices - 1] of TVertexRecord;
    FIndexArray: packed array[0..MaxCachedIndices - 1] of TVertexIndex;

    FCurrentVertexCount: Integer;
    FCurrentIndexCount: Integer;

    FActiveTopology: TTopology;
    FActiveTexture: TCustomBaseTexture;
    FActiveBlendingEffect: TBlendingEffect;
    FActivePremultipliedAlpha: Boolean;

    procedure PrepareVertexArray;
    function DrawBuffers: Boolean;

    procedure SetEffectStates(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean);

    function NextVertexEntry: PVertexRecord;
    procedure AddIndexEntry(const Index: Integer);

    function RequestCache(const Topology: TTopology; const Vertices, Indices: Integer;
      const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture): Boolean;
  protected
    function InitCanvas: Boolean; override;
    procedure DoneCanvas; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    procedure UpdateAttributes; override;
    function GetClipRect: TIntRect; override;
    procedure SetClipRect(const Value: TIntRect); override;
  public
    procedure PutPixel(const Point: TPoint2; const Color: TIntColor); override;
    procedure Line(const Point1, Point2: TPoint2; const Color: TIntColor2); override;

    procedure DrawIndexedTriangles(const Vertices: PPoint2; const Colors: PIntColor; const Indices: PLongInt;
      const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure Flush; override;
    procedure Reset; override;

    property Context: TDX7DeviceContext read FContext;
  end;

implementation

uses
  Windows;

procedure TDX7Canvas.PrepareVertexArray;
var
  I: Integer;
begin
  FillChar(FVertexArray, SizeOf(FVertexArray), 0);

  for I := 0 to MaxCachedVertices - 1 do
  begin
    FVertexArray[I].Vertex.z := 0.0;
    FVertexArray[I].rhw := 1.0;
  end;
end;

function TDX7Canvas.DrawBuffers: Boolean;
begin
  if FContext.D3D7Device = nil then
    Exit(False);

  with FContext.D3D7Device do
  begin
    case FActiveTopology of
      TTopology.Points:
        Result := Succeeded(DrawPrimitive(D3DPT_POINTLIST, VertexFVFType, @FVertexArray[0], FCurrentVertexCount, 0));

      TTopology.Lines:
        Result := Succeeded(DrawPrimitive(D3DPT_LINELIST, VertexFVFType, @FVertexArray[0], FCurrentVertexCount, 0));

      TTopology.Triangles:
        Result := Succeeded(DrawIndexedPrimitive(D3DPT_TRIANGLELIST, VertexFVFType, @FVertexArray[0],
          FCurrentVertexCount, @FIndexArray[0], FCurrentIndexCount, 0));

    else
      Result := False;
    end;
  end;

  NextDrawCall;
end;

procedure TDX7Canvas.Reset;
begin
  inherited;

  FCurrentVertexCount := 0;
  FCurrentIndexCount := 0;

  FActiveTopology := TTopology.Unknown;
  FActiveBlendingEffect := TBlendingEffect.Unknown;
  FActiveTexture := nil;
  FActivePremultipliedAlpha := False;

  if FContext.D3D7Device = nil then
    Exit;

  with FContext.D3D7Device do
  begin
    // Disable unnecessary device states.
    SetRenderState(D3DRENDERSTATE_LIGHTING, Ord(False));
    SetRenderState(D3DRENDERSTATE_CULLMODE, Ord(D3DCULL_NONE));
    SetRenderState(D3DRENDERSTATE_ZENABLE, Ord(D3DZB_FALSE));
    SetRenderState(D3DRENDERSTATE_FOGENABLE, Ord(False));

    // Enable Alpha-testing.
    SetRenderState(D3DRENDERSTATE_ALPHATESTENABLE, Ord(True));
    SetRenderState(D3DRENDERSTATE_ALPHAFUNC, Ord(D3DCMP_GREATEREQUAL));
    SetRenderState(D3DRENDERSTATE_ALPHAREF, $00000001);

    // Default alpha-blending behavior
    SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, Ord(True));

    SetTextureStageState(0, D3DTSS_COLOROP, Ord(D3DTOP_MODULATE));
    SetTextureStageState(0, D3DTSS_ALPHAOP, Ord(D3DTOP_MODULATE));

    SetTextureStageState(0, D3DTSS_MAGFILTER, Ord(D3DTFG_LINEAR));
    SetTextureStageState(0, D3DTSS_MINFILTER, Ord(D3DTFG_LINEAR));
    SetTextureStageState(0, D3DTSS_MIPFILTER, Ord(D3DTFP_NONE));
  end;
end;

procedure TDX7Canvas.Flush;
begin
  if FCurrentVertexCount > 0 then
    DrawBuffers;

  FCurrentVertexCount := 0;
  FCurrentIndexCount := 0;
  FActiveTopology := TTopology.Unknown;
  FActiveBlendingEffect := TBlendingEffect.Unknown;
  FActivePremultipliedAlpha := False;

  if FContext.D3D7Device <> nil then
    FContext.D3D7Device.SetTexture(0, nil);

  FActiveTexture := nil;
end;

procedure TDX7Canvas.SetEffectStates(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean);
begin
  if FContext.D3D7Device = nil then
    Exit;

  case BlendingEffect of
    TBlendingEffect.Normal:
      with FContext.D3D7Device do
      begin
        if not PremultipliedAlpha then
          SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_SRCALPHA))
        else
          SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_ONE));

        SetRenderState(D3DRENDERSTATE_DESTBLEND, Ord(D3DBLEND_INVSRCALPHA));
        SetTextureStageState(0, D3DTSS_COLOROP, Ord(D3DTOP_MODULATE));
        SetTextureStageState(0, D3DTSS_ALPHAOP, Ord(D3DTOP_MODULATE));
      end;

    TBlendingEffect.Shadow:
      with FContext.D3D7Device do
      begin
        SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_ZERO));
        SetRenderState(D3DRENDERSTATE_DESTBLEND, Ord(D3DBLEND_INVSRCALPHA));
        SetTextureStageState(0, D3DTSS_COLOROP, Ord(D3DTOP_MODULATE));
        SetTextureStageState(0, D3DTSS_ALPHAOP, Ord(D3DTOP_MODULATE));
      end;

    TBlendingEffect.Add:
      with FContext.D3D7Device do
      begin
        if not PremultipliedAlpha then
          SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_SRCALPHA))
        else
          SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_ONE));

        SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_SRCALPHA));
        SetRenderState(D3DRENDERSTATE_DESTBLEND, Ord(D3DBLEND_ONE));
        SetTextureStageState(0, D3DTSS_COLOROP, Ord(D3DTOP_MODULATE));
        SetTextureStageState(0, D3DTSS_ALPHAOP, Ord(D3DTOP_MODULATE));
      end;

    TBlendingEffect.Multiply:
      with FContext.D3D7Device do
      begin
        SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_ZERO));
        SetRenderState(D3DRENDERSTATE_DESTBLEND, Ord(D3DBLEND_SRCCOLOR));
        SetTextureStageState(0, D3DTSS_COLOROP, Ord(D3DTOP_MODULATE));
        SetTextureStageState(0, D3DTSS_ALPHAOP, Ord(D3DTOP_MODULATE));
      end;

    TBlendingEffect.InverseMultiply:
      with FContext.D3D7Device do
      begin
        SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_ZERO));
        SetRenderState(D3DRENDERSTATE_DESTBLEND, Ord(D3DBLEND_INVSRCCOLOR));
        SetTextureStageState(0, D3DTSS_COLOROP, Ord(D3DTOP_MODULATE));
        SetTextureStageState(0, D3DTSS_ALPHAOP, Ord(D3DTOP_MODULATE));
      end;

    TBlendingEffect.SourceColor:
      with FContext.D3D7Device do
      begin
        SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_SRCCOLOR));
        SetRenderState(D3DRENDERSTATE_DESTBLEND, Ord(D3DBLEND_INVSRCCOLOR));
        SetTextureStageState(0, D3DTSS_COLOROP, Ord(D3DTOP_MODULATE));
        SetTextureStageState(0, D3DTSS_ALPHAOP, Ord(D3DTOP_MODULATE));
      end;

    TBlendingEffect.SourceColorAdd:
      with FContext.D3D7Device do
      begin
        SetRenderState(D3DRENDERSTATE_SRCBLEND, Ord(D3DBLEND_SRCCOLOR));
        SetRenderState(D3DRENDERSTATE_DESTBLEND, Ord(D3DBLEND_ONE));
        SetTextureStageState(0, D3DTSS_COLOROP, Ord(D3DTOP_MODULATE));
        SetTextureStageState(0, D3DTSS_ALPHAOP, Ord(D3DTOP_MODULATE));
      end;
  end;
end;

function TDX7Canvas.RequestCache(const Topology: TTopology; const Vertices, Indices: Integer;
  const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture): Boolean;
var
  PremultipliedAlpha: Boolean;
begin
  if (Vertices > MaxCachedVertices) or (Indices > MaxCachedIndices) then
    Exit(False);

  if (FCurrentVertexCount + Vertices > MaxCachedVertices) or (FCurrentIndexCount + Indices > MaxCachedIndices) or
    (FActiveTopology = TTopology.Unknown) or (FActiveTopology <> Topology) or (FActiveTexture <> Texture) or
    (FActiveBlendingEffect = TBlendingEffect.Unknown) or (FActiveBlendingEffect <> BlendingEffect) then
  begin
    Flush;

    PremultipliedAlpha := False;

    if Texture <> nil then
      PremultipliedAlpha := Texture.PremultipliedAlpha;

    if (FActiveBlendingEffect = TBlendingEffect.Unknown) or (FActiveBlendingEffect <> BlendingEffect) or
      (FActivePremultipliedAlpha <> PremultipliedAlpha) then
      SetEffectStates(BlendingEffect, PremultipliedAlpha);

    if (FContext.D3D7Device <> nil) and ((FActiveBlendingEffect = TBlendingEffect.Unknown) or
      (FActiveTexture <> Texture)) then
    begin
      if Texture <> nil then
        Texture.Bind(0)
      else
        FContext.D3D7Device.SetTexture(0, nil);
    end;

    FActiveTopology := Topology;
    FActiveBlendingEffect := BlendingEffect;
    FActiveTexture := Texture;
    FActivePremultipliedAlpha := PremultipliedAlpha;
  end;

  Result := True;
end;

function TDX7Canvas.NextVertexEntry: PVertexRecord;
begin
  Result := @FVertexArray[FCurrentVertexCount];
end;

procedure TDX7Canvas.AddIndexEntry(const Index: Integer);
begin
  FIndexArray[FCurrentIndexCount] := Index;
  Inc(FCurrentIndexCount);
end;

procedure TDX7Canvas.PutPixel(const Point: TPoint2; const Color: TIntColor);
var
  VertexEntry: PVertexRecord;
begin
  if not RequestCache(TTopology.Points, 1, 0, TBlendingEffect.Normal, nil) then
    Exit;

  VertexEntry := NextVertexEntry;
  VertexEntry.Vertex.X := Point.X;
  VertexEntry.Vertex.Y := Point.Y;
  VertexEntry.Color := Color;

  Inc(FCurrentVertexCount);
end;

procedure TDX7Canvas.Line(const Point1, Point2: TPoint2; const Color: TIntColor2);
var
  VertexEntry: PVertexRecord;
begin
  if not RequestCache(TTopology.Lines, 2, 0, TBlendingEffect.Normal, nil) then
    Exit;

  VertexEntry := NextVertexEntry;
  VertexEntry.Vertex.X := Point1.X;
  VertexEntry.Vertex.Y := Point1.Y;
  VertexEntry.Color := Color.First;
  Inc(FCurrentVertexCount);

  VertexEntry := NextVertexEntry;
  VertexEntry.Vertex.X := Point2.X;
  VertexEntry.Vertex.Y := Point2.Y;
  VertexEntry.Color := Color.Second;
  Inc(FCurrentVertexCount);
end;

procedure TDX7Canvas.DrawIndexedTriangles(const Vertices: PPoint2; const Colors: PIntColor; const Indices: PLongInt;
  const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect);
var
  VertexEntry: PVertexRecord;
  SourceIndex: PLongInt;
  SourceVertex: PPoint2;
  SourceColor: PIntColor;
  I: Integer;
begin
  if not RequestCache(TTopology.Triangles, VertexCount, TriangleCount * 3, BlendingEffect, nil) then
    Exit;

  SourceIndex := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    AddIndexEntry(FCurrentVertexCount + SourceIndex^);
    Inc(SourceIndex);
  end;

  SourceVertex := Vertices;
  SourceColor := Colors;

  for i := 0 to VertexCount - 1 do
  begin
    VertexEntry := NextVertexEntry;
    VertexEntry.Vertex.X := SourceVertex.X - 0.5;
    VertexEntry.Vertex.Y := SourceVertex.Y - 0.5;
    VertexEntry.Color := SourceColor^;

    Inc(FCurrentVertexCount);
    Inc(SourceVertex);
    Inc(SourceColor);
  end;
end;

procedure TDX7Canvas.DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2;
  const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
  const BlendingEffect: TBlendingEffect);
var
  VertexEntry: PVertexRecord;
  SourceIndex: PLongInt;
  SourceVertex: PPoint2;
  SourceTexCoord: PPoint2;
  SourceColor: PIntColor;
  I: Integer;
begin
  if not RequestCache(TTopology.Triangles, VertexCount, TriangleCount * 3, BlendingEffect, Texture) then
    Exit;

  SourceIndex := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    AddIndexEntry(FCurrentVertexCount + SourceIndex^);
    Inc(SourceIndex);
  end;

  SourceVertex := Vertices;
  SourceTexCoord := TexCoords;
  SourceColor := Colors;

  for I := 0 to VertexCount - 1 do
  begin
    VertexEntry := NextVertexEntry;
    VertexEntry.Vertex.X := SourceVertex.X - 0.5;
    VertexEntry.Vertex.Y := SourceVertex.Y - 0.5;

    if not FActivePremultipliedAlpha then
      VertexEntry.Color := SourceColor^
    else
      VertexEntry.Color := PremultiplyAlpha(SourceColor^);

    VertexEntry.U := SourceTexCoord.X;
    VertexEntry.V := SourceTexCoord.Y;

    Inc(FCurrentVertexCount);
    Inc(SourceVertex);
    Inc(SourceTexCoord);
    Inc(SourceColor);
  end;
end;

function TDX7Canvas.InitCanvas: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX7DeviceContext)) then
    Exit(False);

  FContext := TDX7DeviceContext(Device.Context);

  PrepareVertexArray;
  Result := True;
end;

procedure TDX7Canvas.DoneCanvas;
begin
  FContext := nil;
end;

function TDX7Canvas.BeginDraw: Boolean;
begin
  Reset;
  Result := True;
end;

procedure TDX7Canvas.EndDraw;
begin
  Flush;
end;

procedure TDX7Canvas.UpdateAttributes;
begin
  inherited;

  if FContext.D3D7Device = nil then
    Exit;

  Flush;

  with FContext.D3D7Device do
  begin
    if TCanvasAttribute.Antialias in Attributes then
    begin
      SetTextureStageState(0, D3DTSS_MAGFILTER, Ord(D3DTFG_LINEAR));
      SetTextureStageState(0, D3DTSS_MINFILTER, Ord(D3DTFN_LINEAR));
    end
    else
    begin
      SetTextureStageState(0, D3DTSS_MAGFILTER, Ord(D3DTFG_POINT));
      SetTextureStageState(0, D3DTSS_MINFILTER, Ord(D3DTFN_POINT));
    end;

    if TCanvasAttribute.MipMapping in Attributes then
      SetTextureStageState(0, D3DTSS_MIPFILTER, Ord(D3DTFP_LINEAR))
    else
      FContext.D3D7Device.SetTextureStageState(0, D3DTSS_MIPFILTER, Ord(D3DTFP_NONE));
  end;
end;

function TDX7Canvas.GetClipRect: TIntRect;
var
  Viewport: D3DVIEWPORT7;
begin
  if FContext.D3D7Device = nil then
    Exit(IntRect(0, 0, 0, 0));

  FillChar(Viewport, SizeOf(D3DVIEWPORT7), 0);

  if Failed(FContext.D3D7Device.GetViewport(Viewport)) then
    Exit(IntRect(0, 0, 0, 0));

  Result.Left := Viewport.dwX;
  Result.Top := Viewport.dwY;
  Result.Right := Viewport.dwX + Viewport.dwWidth;
  Result.Bottom := Viewport.dwY + Viewport.dwHeight;
end;

procedure TDX7Canvas.SetClipRect(const Value: TIntRect);
var
  NewViewport, PrevViewport: D3DVIEWPORT7;
begin
  if FContext.D3D7Device = nil then
    Exit;

  FillChar(PrevViewport, SizeOf(D3DVIEWPORT7), 0);

  if Failed(FContext.D3D7Device.GetViewport(PrevViewport)) then
    Exit;

  NewViewport.dwX := Value.Left;
  NewViewport.dwY := Value.Top;
  NewViewport.dwWidth := Value.Width;
  NewViewport.dwHeight := Value.Height;
  NewViewport.dvMinZ := PrevViewport.dvMinZ;
  NewViewport.dvMaxZ := PrevViewport.dvMaxZ;

  if (PrevViewport.dwX <> NewViewport.dwX) or (PrevViewport.dwY <> NewViewport.dwY) or
    (PrevViewport.dwWidth <> NewViewport.dwWidth) or (PrevViewport.dwHeight <> NewViewport.dwHeight) then
    begin
      Flush;
      FContext.D3D7Device.SetViewport(NewViewport);
    end;
end;

end.
