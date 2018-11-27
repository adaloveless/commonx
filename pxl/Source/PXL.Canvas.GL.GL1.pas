unit PXL.Canvas.GL.GL1;
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
  PXL.TypeDef, PXL.Types, PXL.Textures, PXL.Canvas, PXL.Types.GL;

type
  TGLCanvas = class(TCustomCanvas)
  private type
    TTopology = (Unknown, Points, Lines, Triangles);
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TGLDeviceContext;

    FActiveTopology: TTopology;
    FActiveTexture: TCustomBaseTexture;
    FActiveBlendingEffect: TBlendingEffect;
    FActiveAttributes: TCanvasAttributes;
    FActivePremultipliedAlpha: Boolean;

    FViewNormSize: TPoint2;

    procedure ResetStates;
    procedure ResetScene;

    procedure UpdateBlendingEffect(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean);
    procedure UpdateTexture(const Texture: TCustomBaseTexture);
    function RequestCache(const Topology: TTopology; const BlendingEffect: TBlendingEffect;
      const Texture: TCustomBaseTexture): Boolean;

    procedure InsertVertex(const Position: TPoint2; const Color: TIntColor);
  protected
    function InitCanvas: Boolean; override;
    procedure DoneCanvas; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    function GetClipRect: TIntRect; override;
    procedure SetClipRect(const Value: TIntRect); override;
  public
    procedure PutPixel(const Point: TPoint2; const Color: TIntColor); override;
    procedure Line(const Point1, Point2: TPoint2; const Color: TIntColor2); override;

    procedure DrawIndexedTriangles(const Vertices: PPoint2; const Colors: PIntColor; const Indices: PLongInt;
      const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect); override;

    procedure DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect); override;

    procedure Flush; override;
    procedure Reset; override;

    property Context: TGLDeviceContext read FContext;
  end;

implementation

uses
{$IFDEF FPC}
  gl, glext;
{$ELSE}
  {$IFDEF MSWINDOWS}
    Winapi.OpenGL, Winapi.OpenGLext;
  {$ENDIF}

  {$IFDEF MACOS}
    Macapi.CocoaTypes, Macapi.OpenGL;
  {$ENDIF}
{$ENDIF}

function TGLCanvas.BeginDraw: Boolean;
begin
  ResetStates;
  Result := True;
end;

procedure TGLCanvas.EndDraw;
begin
  Flush;
end;

function TGLCanvas.DeviceRestore: Boolean;
begin
  Result := True;
end;

procedure TGLCanvas.DeviceRelease;
begin
end;

function TGLCanvas.GetClipRect: TIntRect;
var
  ScissorValues: array[0..3] of GLint;
begin
  glGetIntegerv(GL_SCISSOR_BOX, @ScissorValues[0]);

  Result.Left := ScissorValues[0];
  Result.Top := Round(FViewNormSize.Y * 2) - ScissorValues[1];
  Result.Right := Result.Left + ScissorValues[2];
  Result.Bottom := Result.Top + ScissorValues[3];
end;

procedure TGLCanvas.SetClipRect(const Value: TIntRect);
begin
  glScissor(Value.Left, Round(FViewNormSize.Y * 2) - Value.Top, Value.Width, Value.Height);
end;

procedure TGLCanvas.ResetStates;
var
  Viewport: array[0..3] of GLint;
begin
  FActiveTopology := TTopology.Unknown;
  FActiveBlendingEffect := TBlendingEffect.Unknown;
  FActiveTexture := nil;
  FActivePremultipliedAlpha := False;

  glGetIntegerv(GL_VIEWPORT, @Viewport[0]);

  FViewNormSize.X := Viewport[2] / 2;
  FViewNormSize.Y := Viewport[3] / 2;

  glDisable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  glDisable(GL_STENCIL_TEST);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glActiveTexture(GL_TEXTURE0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  glDisable(GL_DEPTH_TEST);

  glDisable(GL_TEXTURE_1D);
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_LINE_SMOOTH);
  glUseProgram(0);

  if (FContext <> nil) and (FContext.Extensions.EXT_separate_specular_color or
    FContext.Extensions.VersionCheck(1, 2)) then
    glDisable(GL_COLOR_SUM_EXT);

  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER, 1 / 255);

  glScissor(Viewport[0], Viewport[1], Viewport[2], Viewport[3]);
  glEnable(GL_SCISSOR_TEST);
end;

procedure TGLCanvas.ResetScene;
begin
  if FActiveTopology <> TTopology.Unknown then
  begin
    glEnd;
    NextDrawCall;

    FActiveTopology := TTopology.Unknown;
    FActivePremultipliedAlpha := False;
  end;
end;

procedure TGLCanvas.UpdateBlendingEffect(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean);
begin
  if (FActiveBlendingEffect = BlendingEffect) and (FActivePremultipliedAlpha = PremultipliedAlpha) then
    Exit;

  if BlendingEffect = TBlendingEffect.Unknown then
  begin
    glBlendFunc(GL_ONE, GL_ZERO);
    glDisable(GL_BLEND);
  end
  else
    glEnable(GL_BLEND);

  case BlendingEffect of
    TBlendingEffect.Normal:
      if not PremultipliedAlpha then
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      else
        glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);

    TBlendingEffect.Shadow:
      glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_ALPHA);

    TBlendingEffect.Add:
      if not PremultipliedAlpha then
        glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      else
        glBlendFunc(GL_ONE, GL_ONE);

    TBlendingEffect.Multiply:
      glBlendFunc(GL_ZERO, GL_SRC_COLOR);

    TBlendingEffect.InverseMultiply:
       glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_COLOR);

    TBlendingEffect.SourceColor:
      glBlendFunc(GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);

    TBlendingEffect.SourceColorAdd:
      glBlendFunc(GL_SRC_COLOR, GL_ONE);
  end;

  FActiveBlendingEffect := BlendingEffect;
  FActivePremultipliedAlpha := PremultipliedAlpha;
end;

procedure TGLCanvas.UpdateTexture(const Texture: TCustomBaseTexture);
begin
  if (FContext = nil) or ((FActiveTexture = Texture) and (FActiveAttributes = Attributes)) then
    Exit;

  if Texture <> nil then
  begin
    Texture.Bind(0);

    if TCanvasAttribute.Antialias in Attributes then
    begin
      if Texture.MipMapping and (TCanvasAttribute.MipMapping in Attributes) and
        FContext.Extensions.VersionCheck(1, 4) then
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
      else
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end
    else
    begin
      if Texture.MipMapping and (TCanvasAttribute.MipMapping in Attributes) and
        FContext.Extensions.VersionCheck(1, 4) then
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST)
      else
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    end;

    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glEnable(GL_TEXTURE_2D);
  end
  else
  begin
    glBindTexture(GL_TEXTURE_2D, 0);
    glDisable(GL_TEXTURE_2D);
  end;

  FActiveTexture := Texture;

  if FActiveTexture <> nil then
    FActiveAttributes := Attributes
  else
    FActiveAttributes := [];
end;

function TGLCanvas.RequestCache(const Topology: TTopology; const BlendingEffect: TBlendingEffect;
  const Texture: TCustomBaseTexture): Boolean;
var
  PremultipliedAlpha: Boolean;
begin
  if (FActiveTopology <> Topology) or (FActiveBlendingEffect <> BlendingEffect) or (FActiveTexture <> Texture) or
    (FActiveAttributes <> Attributes) then
    ResetScene;

  PremultipliedAlpha := False;

  if Texture <> nil then
    PremultipliedAlpha := Texture.PremultipliedAlpha;

  UpdateBlendingEffect(BlendingEffect, PremultipliedAlpha);
  UpdateTexture(Texture);

  if FActiveTopology <> Topology then
  begin
    FActiveTopology := Topology;

    case FActiveTopology of
      TTopology.Points:
        glBegin(GL_POINTS);

      TTopology.Lines:
        glBegin(GL_LINES);

      TTopology.Triangles:
        glBegin(GL_TRIANGLES);
    end;
  end;

  Result := True;
end;

procedure TGLCanvas.InsertVertex(const Position: TPoint2; const Color: TIntColor);
begin
  glColor4f(TIntColorRec(Color).Red / 255.0, TIntColorRec(Color).Green / 255.0, TIntColorRec(Color).Blue / 255.0,
    TIntColorRec(Color).Alpha / 255.0);

  if FContext.FrameBufferLevel <= 0 then
    glVertex2f((Position.X - FViewNormSize.X) / FViewNormSize.X, (FViewNormSize.Y - Position.Y) / FViewNormSize.Y)
  else
    glVertex2f((Position.X - FViewNormSize.X) / FViewNormSize.X, (Position.Y - FViewNormSize.Y) / FViewNormSize.Y);
end;

procedure TGLCanvas.PutPixel(const Point: TPoint2; const Color: TIntColor);
begin
  if not RequestCache(TTopology.Points, TBlendingEffect.Normal, nil) then
    Exit;

  InsertVertex(Point + Point2(0.5, 0.5), Color);
end;

procedure TGLCanvas.Line(const Point1, Point2: TPoint2; const Color: TIntColor2);
begin
  if not RequestCache(TTopology.Lines, TBlendingEffect.Normal, nil) then
    Exit;

  InsertVertex(Point1 + PXL.Types.Point2(0.5, 0.5), Color.First);
  InsertVertex(Point2 + PXL.Types.Point2(0.5, 0.5), Color.Second);
end;

procedure TGLCanvas.DrawIndexedTriangles(const Vertices: PPoint2; const Colors: PIntColor; const Indices: PLongInt;
  const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect);
var
  SourceIndex: PLongInt;
  I: Integer;
begin
  if not RequestCache(TTopology.Triangles, BlendingEffect, nil) then
    Exit;

  SourceIndex := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    InsertVertex(PPoint2(PtrInt(Vertices) + SourceIndex^ * SizeOf(TPoint2))^, PIntColor(PtrInt(Colors) +
      SourceIndex^ * SizeOf(TIntColor))^);

    Inc(SourceIndex);
  end;
end;

procedure TGLCanvas.DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2;
  const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
  const BlendingEffect: TBlendingEffect);
var
  SourceIndex: PLongInt;
  InpTexCoord: PPoint2;
  I: Integer;
begin
  if not RequestCache(TTopology.Triangles, BlendingEffect, Texture) then
    Exit;

  SourceIndex := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    InpTexCoord := Pointer(PtrInt(TexCoords) + SourceIndex^ * SizeOf(TPoint2));
    glTexCoord2f(InpTexCoord.X, InpTexCoord.Y);

    InsertVertex(PPoint2(PtrInt(Vertices) + SourceIndex^ * SizeOf(TPoint2))^, PIntColor(PtrInt(Colors) +
      SourceIndex^ * SizeOf(TIntColor))^);

    Inc(SourceIndex);
  end;
end;

procedure TGLCanvas.Flush;
begin
  ResetScene;
end;

procedure TGLCanvas.Reset;
begin
  RequestCache(TTopology.Unknown, TBlendingEffect.Unknown, nil);
end;

function TGLCanvas.InitCanvas: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TGLDeviceContext)) then
    Exit(False);

  FContext := TGLDeviceContext(Device.Context);
  Result := True;
end;

procedure TGLCanvas.DoneCanvas;
begin
  FContext := nil;
end;

end.
