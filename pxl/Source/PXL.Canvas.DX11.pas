unit PXL.Canvas.DX11;
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

//WHAT YOU FORGOT, JASON
//-- CanvasVertexLayout is where you go to describe a change in the vertex layout
interface

{$INCLUDE PXL.Config.inc}
{x$DEFINE USE_ZW}
{x$DEFINE USE_NEW_CLIPPING}
uses
  PXL.Windows.D3D11, PXL.TypeDef, PXL.Types, PXL.Textures, PXL.Canvas, PXL.Types.DX11, PXL.Shaders.DX11, typex, systemx, helpers.stream, debug;

const
  CONSTANT_BUFFER_COUNT = 4;
  VERTEX_CONSTANT_BUFFER_COUNT = 9;
type
  TDX11Canvas = class(TCustomCanvas)
  protected const
    { The following parameters roughly affect the rendering performance. The higher values means that more primitives
      will fit in cache, but it will also occupy more bandwidth, even when few primitives are rendered.
      These parameters can be fine-tuned in a finished product to improve the  overall performance. }
    MaxCachedIndices = 8192;
    MaxCachedVertices = 8192;
    MAX_LIGHTS = 16;
  protected type
    TTopology = (Unknown, Points, Lines, Triangles);
    TProgram = (Unknown, Solid, Textured, TexturedL, TexturedLA, TexturedA, TexturedI);

    PVertexEntry = ^TVertexEntry;
    TVertexEntry = record
      X, Y: Single;
{$IFDEF USE_ZW}
      Z, W: Single;
{$ENDIF}
      U, V: Single;
      u2,v2:single;
      Color: LongWord;
{$IFDEF PASS_VIEW_MATRIX_AS_PARAM}
      ViewMatrix: TMatrix4;
{$ENDIF}
{$IFDEF USE_NEW_CLIPPING}
      ClipLeft, ClipRight, ClipNear, ClipFar: single;
{$ENDIF}
    end;


    TIndexEntry = Word;
  private
    FLights: array[0..MAX_LIGHTS-1] of TPXLLIght;
    FPSConstants: array[0..CONSTANT_BUFFER_COUNT-1] of TVector4;
    FVSConstants: array[0..VERTEX_CONSTANT_BUFFER_COUNT-1] of TVector4;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX11DeviceContext;

    light_idx: ni;
    dyn: TDynByteArray;
    dynA: TDynByteArray;
    dynVS: TDynByteArray;



    FEffectSolid: TDX11ShaderEffect;
    FEffectTextured: TDX11ShaderEffect;
    FEffectTexturedL: TDX11ShaderEffect;
    FEffectTexturedLA: TDX11ShaderEffect;
    FEffectTexturedA: TDX11ShaderEffect;
    FEffectTexturedI: TDX11ShaderEffect;


    FRasterState: ID3D11RasterizerState;
    FDepthStencilState: ID3D11DepthStencilState;

    FPointSampler: ID3D11SamplerState;
    FLinearSampler: ID3D11SamplerState;
    FMipMapSampler: ID3D11SamplerState;

    FVertexBuffer: ID3D11Buffer;
    FIndexBuffer: ID3D11Buffer;

    FBlendingStates: array[TBlendingEffect, Boolean] of ID3D11BlendState;

    FVertexArray: Pointer;
    FIndexArray: Pointer;

    FCurrentVertexCount: Integer;
    FCurrentIndexCount: Integer;

    FActiveTopology: TTopology;
    FActiveTexture: TCustomBaseTexture;
    FActiveRefMap: TCustomBaseTexture;
    FActiveProgram: TProgram;
    FActiveShaderEffect: TDX11ShaderEffect;
    FActiveBlendingEffect: TBlendingEffect;
    FActivePremultipliedAlpha: Boolean;


    FPaletteTexture: TCustomLockableTexture;

    FNormalSize: TPoint2;
    FScissorRect: TIntRect;
    FViewport: D3D11_VIEWPORT;
    FOverrideCanvasSize: boolean;
    FPSConstantBuffer: ID3D11Buffer;//ID3D11Buffer
    FVSConstantBuffer: ID3D11Buffer;//ID3D11Buffer
    FLIghtBuffer: ID3D11Buffer;
    FViewMatrix: TMatrix4;//ID3D11Buffer
    FProjMatrix: TMatrix4;//ID3D11Buffer

    function InitializeEffects: Boolean;
    procedure FinalizeEffects;

    procedure CreateStaticObjects;
    procedure DestroyStaticObjects;

    function CreateDynamicBuffers: Boolean;
    procedure DestroyDynamicBuffers;

    function CreateSamplerStates: Boolean;
    procedure DestroySamplerStates;

    function CreateDeviceStates: Boolean;
    procedure DestroyDeviceStates;

    procedure ReleaseBlendingStates;

    function CreateDynamicObjects: Boolean;
    procedure DestroyDynamicObjects;

    function RetrieveViewport: Boolean;

    procedure ResetRasterState;
    procedure ResetDepthStencilState;
    procedure ResetShaderViews;
    procedure RemoveShaderBinding(idx: integer);override;
    procedure ResetBlendingState;

    procedure UpdateSamplerState(const NewProgram: TProgram);
    procedure ResetSamplerState;

    function UploadVertexBuffer: Boolean;
    function UploadIndexBuffer: Boolean;
    procedure SetBuffersAndTopology;
    procedure DrawPrimitives;

    procedure GetBlendingParams(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean;
      out SrcColor, DestColor, SrcAlpha, DestAlpha: D3D11_BLEND);
    function SetEffectStates(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean): Boolean;

    function RequestCache(const ATopology: TTopology; AProgram: TProgram; const Vertices,
      Indices: Integer; const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture; const RefMap: TCustomBaseTExture): Boolean;

    function NextVertexEntry: Pointer;
    procedure AddVertexEntry(const Position, TexCoord: TPoint2; const Color: Cardinal);overload;
    procedure AddVertexEntry(const Position, TexCoord: TPoint2; const Color: Cardinal; const refCoord: TPOint2);overload;
    procedure AddVertexEntry(const Position: TVector4; TexCoord: TPoint2; const Color: Cardinal; const refCoord: TPOint2);overload;
    procedure AddIndexEntry(const Index: Integer);
    procedure SetNormalSize(const Value: TPoint2);
    function CReateConstantBuffers: Boolean;
    function CReatePSConstantBuffer: Boolean;
    function CReateVSConstantBuffer: Boolean;
    procedure SendPixelShaderConstants;override;

    function CreateLightBuffer: boolean;
  protected

    function InitCanvas: Boolean; override;
    procedure DoneCanvas; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    function GetClipRect: TIntRect; override;
    procedure SetClipRect(const Value: TIntRect); override;
    procedure UpdateAttributes; override;
    procedure SendVertexShaderConstants;override;
    procedure SendShaderConstants;override;
  public
    procedure CreateEffects;
    procedure DestroyEffects;

    procedure SendLights;override;
    destructor Destroy; override;

    procedure PutPixel(const Point: TPoint2; const Color: TIntColor); override;
    procedure Line(const SrcPoint, DestPoint: TPoint2; const Color: TIntColor2); override;

    procedure DrawIndexedTriangles(const Vertices: PPoint2; const Colors: PIntColor; const Indices: PLongInt;
      const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure DrawTexturedTriangles(const Texture: TCustomBaseTexture;  const RefMap: TCustomBaseTExture; const Vertices, TexCoords: PPoint2;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure DrawTexturedTriangles3D(const Texture: TCustomBaseTexture;  const RefMap: TCustomBaseTExture; const Vertices: PVector4; TexCoords: PPoint2;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer; RefCoords: PPoint2;
      const MAt: TMaterialProperties); override;


    procedure Flush; override;
    procedure Reset; override;

    function SetPalette(const Palette: TIntColorPalette): Boolean; override;
    procedure ResetPalette; override;
    procedure SelectLights(lights: PPXLLIght; count: Integer); override;

    property Context: TDX11DeviceContext read FContext;
    property OverrideCanvasSize: boolean read FOverrideCanvasSize write FOverrideCanvasSize;
    property NormalSize: TPoint2 read FNormalSize write SetNormalSize;
    procedure ResetLights;override;
    function AddLIght(l: TPXLLight): boolean;override;



  end;

//var
//  dyn: TDynByteArray;
//  dynA: TDynByteArray;
//  dynVS: TDynByteArray;

implementation

uses
  Windows, SysUtils, PXL.Windows.D3DCommon, PXL.Windows.DXGI, PXL.Consts, PXL.Logs, PXL.Providers;

{$REGION 'Global Types and Functions'}

{$INCLUDE PXL.Canvas.DX11.Shaders.inc}

const
  CanvasVertexLayout: array[0..3] of D3D11_INPUT_ELEMENT_DESC =
    ((SemanticName: 'POSITION';
    SemanticIndex: 0;
    Format: DXGI_FORMAT_R32G32B32A32_FLOAT;
    InputSlot: 0;
    AlignedByteOffset: 0;
    InputSlotClass: D3D11_INPUT_PER_VERTEX_DATA;
    InstanceDataStepRate: 0),

    (SemanticName: 'TEXCOORD';
    SemanticIndex: 0;
    Format: DXGI_FORMAT_R32G32_FLOAT;
    InputSlot: 0;
    AlignedByteOffset: 8+8;
    InputSlotClass: D3D11_INPUT_PER_VERTEX_DATA;
    InstanceDataStepRate: 0),

    (SemanticName: 'TEXCOORD';
    SemanticIndex: 1;
    Format: DXGI_FORMAT_R32G32_FLOAT;
    InputSlot: 0;
    AlignedByteOffset: 16+8;
    InputSlotClass: D3D11_INPUT_PER_VERTEX_DATA;
    InstanceDataStepRate: 0),

    (SemanticName: 'COLOR';
    SemanticIndex: 0;
    Format: DXGI_FORMAT_R8G8B8A8_UNORM;
    InputSlot: 0;
    AlignedByteOffset: 24+8;
    InputSlotClass: D3D11_INPUT_PER_VERTEX_DATA;
    InstanceDataStepRate: 0)
    );



{$ENDREGION}
{$REGION 'TDX11Canvas'}




destructor TDX11Canvas.Destroy;
begin
  FreeAndNil(FPaletteTexture);

  inherited;
end;

procedure TDX11Canvas.CreateEffects;
var
  vertexShader: pointer;
  vertexShaderLength: ni;

begin
  if fileexists(dllpath+'CanvasVertex.fxc') then begin
    dynVS := LoadFileAsByteArray(dllpath+'CanvasVertex.fxc');
    vertexShader := @dynVS[0];
    vertexShaderlength := length(dynvs);
  end else begin
    vertexShader := @canvasvertex[0];
    vertexshaderlength := high(canvasvertex)+1;
  end;






  // Solid (non-textured)
  FEffectSolid := TDX11ShaderEffect.Create(FContext);

  FEffectSolid.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectSolid.SetShaderCodes(vertexShader, vertexShaderLength, @CanvasSolid[0], High(CanvasSolid) + 1);

  // Textured (color / texture)
{x$DEFINE USE_STANDARD_TEXTURE}
{$IFDEF USE_STANDARD_TEXTURE}
  FEffectTextured := TDX11ShaderEffect.Create(FContext);
  FEffectTextured.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectTextured.SetShaderCodes(vertexShader, vertexShaderLength, @CanvasTextured[0],
    High(CanvasTextured) + 1);
{$ELSE}
  FEffectTextured := TDX11ShaderEffect.Create(FContext);

  FEffectTextured.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  if fileexists(dllpath+'CanvasTextured.fxc') then begin
    dyn := LoadFileAsByteArray(dllpath+'CanvasTextured.fxc');
    FEffectTextured.SetShaderCodes(vertexShader, vertexShaderLength, @dyn[0], High(dyn) + 1);
  end else begin
    FEffectTextured.SetShaderCodes(vertexShader, vertexShaderLength, @CanvasTextured[0],
      High(CanvasTextured) + 1);
  end;
{$ENDIF}



  // Luminance Textured (color / texture having only luminance channel)
  FEffectTexturedL := TDX11ShaderEffect.Create(FContext);
  FEffectTexturedL.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
    FEffectTexturedL.SetShaderCodes(vertexShader, vertexShaderLength, @CanvasTexturedL[0],
      High(CanvasTexturedL) + 1);


  // Luminance-Alpha Textured (color / texture having luminance and alpha channels)
  FEffectTexturedLA := TDX11ShaderEffect.Create(FContext);
  FEffectTexturedLA.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectTexturedLA.SetShaderCodes(vertexShader, vertexShaderLength, @CanvasTexturedLA[0],
    High(CanvasTexturedLA) + 1);



  // Alpha Textured (color / texture having only alpha channel)
  FEffectTexturedA := TDX11ShaderEffect.Create(FContext);

  FEffectTexturedA.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  if fileexists(dllpath+'CanvasTexturedA.fxc') then begin
    dynA := LoadFileAsByteArray(dllpath+'CanvasTexturedA.fxc');
    FEffectTexturedA.SetShaderCodes(vertexShader, vertexShaderLength, @dynA[0], High(dyna) + 1);
  end else begin
    FEffectTexturedA.SetShaderCodes(vertexShader, vertexShaderLength, @CanvasTexturedA[0],
      High(CanvasTexturedA) + 1);
  end;


  // Indexed Textured (color / texture using 256-color palette in a separate texture)
  FEffectTexturedI := TDX11ShaderEffect.Create(FContext);

  FEffectTexturedI.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectTexturedI.SetShaderCodes(vertexShader, vertexShaderLength, @CanvasTexturedI[0],
    High(CanvasTexturedI) + 1);
end;

function TDX11Canvas.CreateLightBuffer: boolean;
var
  Desc: D3D11_BUFFER_DESC;
  InitData: D3D11_SUBRESOURCE_DATA;
  hr: HResult;
  t: ni;
begin
  FillChar(Desc, SizeOf(D3D11_BUFFER_DESC), 0);

  desc.ByteWidth := SizeOf(FLights);
  desc.Usage :=  D3D11_USAGE_DYNAMIC;
  desc.BindFlags := D3D11_BIND_CONSTANT_BUFFER;
  desc.CPUAccessFlags := D3D11_CPU_ACCESS_WRITE;
  desc.MiscFlags := 0;
  desc.StructureByteStride := 0;


  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  InitData.SysMem :=@FLights[0];
  InitData.SysMemPitch := 0;
  InitData.SysMemSlicePitch := 0;

  PushClearFPUState;
  try

//    if Failed(FContext.Device.CreateBuffer(Desc, @InitData, @FConstantBuffer)) then
//      Exit(False);

    hr := FContext.Device.CreateBuffer(Desc, @InitData, @FLightBuffer);
    if Failed(hr) then
      Exit(False);

  finally
    PopFPUState;
  end;
  exit(true);
end;

procedure TDX11Canvas.DestroyEffects;
begin
  FEffectTexturedI.Free;
  FEffectTexturedA.Free;
  FEffectTexturedLA.Free;
  FEffectTexturedL.Free;
  FEffectTextured.Free;
  FEffectSolid.Free;
end;

function TDX11Canvas.InitializeEffects: Boolean;
begin
  CreateEffects;

  if not FEffectSolid.Initialize then
    Exit(False);

  Result := FEffectTexturedL.Initialize;
  if not Result then
  begin
    FEffectTextured.Finalize;
    FEffectSolid.Finalize;
  end;

  if not FEffectTextured.Initialize then
  begin
    FEffectSolid.Finalize;
    Exit(False);
  end;



  Result := FEffectTexturedLA.Initialize;
  if not Result then
  begin
    FEffectTexturedL.Finalize;
    FEffectTextured.Finalize;
    FEffectSolid.Finalize;
  end;

  Result := FEffectTexturedA.Initialize;
  if not Result then
  begin
    FEffectTexturedLA.Finalize;
    FEffectTexturedL.Finalize;
    FEffectTextured.Finalize;
    FEffectSolid.Finalize;
  end;

  Result := FEffectTexturedI.Initialize;
  if not Result then
  begin
    FEffectTexturedA.Finalize;
    FEffectTexturedLA.Finalize;
    FEffectTexturedL.Finalize;
    FEffectTextured.Finalize;
    FEffectSolid.Finalize;
  end;
end;

procedure TDX11Canvas.FinalizeEffects;
begin
  if FEffectTexturedLA <> nil then
    FEffectTexturedLA.Finalize;

  if FEffectTextured <> nil then
    FEffectTextured.Finalize;

  if FEffectSolid <> nil then
    FEffectSolid.Finalize;

  DestroyEffects;
end;

procedure TDX11Canvas.CreateStaticObjects;
begin
  FVertexArray := AllocMem(MaxCachedVertices * SizeOf(TVertexEntry));
  FIndexArray := AllocMem(MaxCachedIndices * SizeOf(TIndexEntry));
end;

function TDX11Canvas.CReateVSConstantBuffer: Boolean;
var
  Desc: D3D11_BUFFER_DESC;
  InitData: D3D11_SUBRESOURCE_DATA;
  hr: HResult;
  t: ni;
begin
  FillChar(Desc, SizeOf(D3D11_BUFFER_DESC), 0);

  desc.ByteWidth := SizeOf(FVSConstants);
  desc.Usage :=  D3D11_USAGE_DYNAMIC;
  desc.BindFlags := D3D11_BIND_CONSTANT_BUFFER;
  desc.CPUAccessFlags := D3D11_CPU_ACCESS_WRITE;
  desc.MiscFlags := 0;
  desc.StructureByteStride := 0;


  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  InitData.SysMem :=@FVSConstants[0];
  InitData.SysMemPitch := 0;
  InitData.SysMemSlicePitch := 0;

  PushClearFPUState;
  try

//    if Failed(FContext.Device.CreateBuffer(Desc, @InitData, @FConstantBuffer)) then
//      Exit(False);

    hr := FContext.Device.CreateBuffer(Desc, @InitData, @FVSConstantBuffer);
    if Failed(hr) then
      Exit(False);


  finally
    PopFPUState;
  end;

  exit(true);
end;

procedure TDX11Canvas.DestroyStaticObjects;
begin
  FreeMemAndNil(FIndexArray);
  FreeMemAndNil(FVertexArray);
end;

function TDX11Canvas.CreateDynamicBuffers: Boolean;
var
  Desc: D3D11_BUFFER_DESC;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  // Create Vertex Buffer.
  FillChar(Desc, SizeOf(D3D11_BUFFER_DESC), 0);

  Desc.ByteWidth := SizeOf(TVertexEntry) * MaxCachedVertices;
  Desc.Usage := D3D11_USAGE_DYNAMIC;
  Desc.BindFlags := Ord(D3D11_BIND_VERTEX_BUFFER);
  Desc.MiscFlags := 0;
  Desc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_WRITE);

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateBuffer(Desc, nil, @FVertexBuffer)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  // Create Index Buffer.
  FillChar(Desc, SizeOf(D3D11_BUFFER_DESC), 0);

  Desc.ByteWidth := SizeOf(TIndexEntry) * MaxCachedIndices;
  Desc.Usage := D3D11_USAGE_DYNAMIC;
  Desc.BindFlags := Ord(D3D11_BIND_INDEX_BUFFER);
  Desc.MiscFlags := 0;
  Desc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_WRITE);

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateBuffer(Desc, nil, @FIndexBuffer)) then
    begin
      FVertexBuffer:= nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  Result := True;
end;

procedure TDX11Canvas.DestroyDynamicBuffers;
begin
  FIndexBuffer := nil;
  FVertexBuffer := nil;
end;

function TDX11Canvas.CReatePSConstantBuffer: Boolean;
var
  Desc: D3D11_BUFFER_DESC;
  InitData: D3D11_SUBRESOURCE_DATA;
  hr: HResult;
  t: ni;
begin
  FillChar(Desc, SizeOf(D3D11_BUFFER_DESC), 0);

  desc.ByteWidth := SizeOf(FPSConstants);
  desc.Usage :=  D3D11_USAGE_DYNAMIC;
  desc.BindFlags := D3D11_BIND_CONSTANT_BUFFER;
  desc.CPUAccessFlags := D3D11_CPU_ACCESS_WRITE;
  desc.MiscFlags := 0;
  desc.StructureByteStride := 0;


  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  InitData.SysMem :=@FPSConstants[0];
  InitData.SysMemPitch := 0;
  InitData.SysMemSlicePitch := 0;

  PushClearFPUState;
  try

//    if Failed(FContext.Device.CreateBuffer(Desc, @InitData, @FConstantBuffer)) then
//      Exit(False);

    hr := FContext.Device.CreateBuffer(Desc, @InitData, @FPSConstantBuffer);
    if Failed(hr) then
      Exit(False);


  finally
    PopFPUState;
  end;
  exit(true);
end;

function TDX11Canvas.CreateSamplerStates: Boolean;
var
  Desc: D3D11_SAMPLER_DESC;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  FillChar(Desc, SizeOf(D3D11_SAMPLER_DESC), 0);

  // Create Point Sampler.
  Desc.Filter := D3D11_FILTER_MIN_MAG_MIP_POINT;
  Desc.AddressU := D3D11_TEXTURE_ADDRESS_WRAP;
  Desc.AddressV := D3D11_TEXTURE_ADDRESS_WRAP;
  Desc.AddressW := D3D11_TEXTURE_ADDRESS_WRAP;
  Desc.MaxAnisotropy := 1;
  Desc.ComparisonFunc := D3D11_COMPARISON_NEVER;
  Desc.BorderColor[0] := 1.0;
  Desc.BorderColor[1] := 1.0;
  Desc.BorderColor[2] := 1.0;
  Desc.BorderColor[3] := 1.0;
  Desc.BorderColor[0] := 1.0;

  if FContext.FeatureLevel < D3D_FEATURE_LEVEL_10_0 then
  begin
    Desc.MinLOD := -D3D11_FLOAT32_MAX;
    Desc.MaxLOD := D3D11_FLOAT32_MAX;
  end;

  PushClearFPUState;
  try

    if Failed(FContext.Device.CreateSamplerState(Desc, @FPointSampler)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  // Create Linear Sampler.
{$IFDEF LOFI}
  Desc.Filter := D3D11_FILTER_MIN_MAG_LINEAR_MIP_POINT;
  Desc.Filter := D3D11_FILTER_MIN_MAG_MIP_POINT;
{$ELSE}
  Desc.Filter := D3D11_FILTER_MIN_MAG_MIP_LINEAR;
  Desc.Filter := D3D11_FILTER_MIN_MAG_MIP_LINEAR;
{$ENDIF}

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateSamplerState(Desc, @FLinearSampler)) then
    begin
      FPointSampler := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  // Create Mipmap Sampler.
  Desc.Filter := D3D11_FILTER_MIN_MAG_MIP_LINEAR;
  Desc.MinLOD := -D3D11_FLOAT32_MAX;
  Desc.MaxLOD := D3D11_FLOAT32_MAX;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateSamplerState(Desc, @FMipMapSampler)) then
    begin
      FLinearSampler := nil;
      FPointSampler := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  Result := True;
end;

procedure TDX11Canvas.DestroySamplerStates;
begin
  FMipMapSampler := nil;
  FLinearSampler := nil;
  FPointSampler := nil;
end;

function TDX11Canvas.CReateConstantBuffers: Boolean;
begin
  result := CReatePSConstantBuffer and CReateVSConstantBuffer;
end;

function TDX11Canvas.CreateDeviceStates: Boolean;
var
  RasterDesc: D3D11_RASTERIZER_DESC;
  DepthStencilDesc: D3D11_DEPTH_STENCIL_DESC;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  // Create Raster state.
  FillChar(RasterDesc, SizeOf(D3D11_RASTERIZER_DESC), 0);

  RasterDesc.CullMode := D3D11_CULL_NONE;
  RasterDesc.FillMode := D3D11_FILL_SOLID;

  RasterDesc.DepthClipEnable := false;
  RasterDesc.ScissorEnable := True;

  RasterDesc.MultisampleEnable := False;
  RasterDesc.AntialiasedLineEnable := False;
  RasterDesc.DepthBias := 1;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateRasterizerState(RasterDesc, @FRasterState)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  // Create Depth/Stencil state.
  FillChar(DepthStencilDesc, SizeOf(D3D11_DEPTH_STENCIL_DESC), 0);

  DepthStencilDesc.DepthEnable := False;
  DepthStencilDesc.StencilEnable := False;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateDepthStencilState(DepthStencilDesc, @FDepthStencilState)) then
    begin
      FRasterState := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  Result := True;
end;

procedure TDX11Canvas.DestroyDeviceStates;
begin
  FDepthStencilState := nil;
  FRasterState := nil;
end;

procedure TDX11Canvas.ReleaseBlendingStates;
var
  State: TBlendingEffect;
begin
  for State := High(TBlendingEffect) downto Low(TBlendingEffect) do
  begin
    FBlendingStates[State, False] := nil;
    FBlendingStates[State, True] := nil;
  end;
end;

procedure TDX11Canvas.RemoveShaderBinding(idx: integer);
var
  NullView: ID3D11ShaderResourceView;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  NullView := nil;

  PushClearFPUState;
  try
    FContext.Context.PSSetShaderResources(idx, 1, @NullView);
  finally
    PopFPUState;
  end;

  SendShaderConstants;
end;

function TDX11Canvas.CreateDynamicObjects: Boolean;
begin
  if not InitializeEffects then
    Exit(False);

  if not CreateDynamicBuffers then
  begin
    FinalizeEffects;
    Exit(False);
  end;

  if not CreateSamplerStates then
  begin
    DestroyDynamicBuffers;
    FinalizeEffects;
    Exit(False);
  end;

  if not CReateConstantBuffers then
  begin
    DestroyDynamicBuffers;
    FinalizeEffects;
    Exit(False);
  end;

  if not CReateLightBuffer then
  begin
    DestroyDynamicBuffers;
    FinalizeEffects;
    Exit(False);
  end;



  if not CreateDeviceStates then
  begin
    DestroySamplerStates;
    DestroyDynamicBuffers;
    FinalizeEffects;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX11Canvas.DestroyDynamicObjects;
begin
  ReleaseBlendingStates;
  DestroyDeviceStates;
  DestroyDynamicBuffers;
  DestroySamplerStates;
  FinalizeEffects;
end;

function TDX11Canvas.RetrieveViewport: Boolean;
var
  NumViewports: LongWord;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit(False);

  NumViewports := 1;

  PushClearFPUState;
  try
    FContext.Context.RSGetViewports(NumViewports, @FViewport);
  finally
    PopFPUState;
  end;

  if NumViewports < 1 then
    FillChar(FViewport, SizeOf(D3D11_VIEWPORT), 0);

  Result := NumViewports > 0;
end;

procedure TDX11Canvas.ResetRasterState;
var
  TempRect: D3D11_RECT;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FRasterState = nil) then
    Exit;


  FScissorRect := IntRect(Round(FViewport.TopLeftX), Round(FViewport.TopLeftY), Round(FViewport.Width),
    Round(FViewport.Height));

  TempRect.Left := FScissorRect.Left;
  TempRect.Top := FScissorRect.Top;
  TempRect.Right := FScissorRect.Right;
  TempRect.Bottom := FScissorRect.Bottom;

  PushClearFPUState;
  try
    FContext.Context.RSSetState(FRasterState);
    FContext.Context.RSSetScissorRects(1, @TempRect);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.ResetDepthStencilState;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FDepthStencilState = nil) then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.OMSetDepthStencilState(FDepthStencilState, 0);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.ResetLights;
begin
  light_idx := 0;
  FillMem(@FLights[0], sizeof(FLights), 0);
end;

procedure TDX11Canvas.ResetShaderViews;
var
  NullView: ID3D11ShaderResourceView;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  NullView := nil;

  PushClearFPUState;
  try
    FContext.Context.PSSetShaderResources(0, 1, @NullView);
    FContext.Context.PSSetShaderResources(1, 1, @NullView);
    FContext.Context.PSSetShaderResources(2, 1, @NullView);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.ResetBlendingState;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.OMSetBlendState(nil, nil, $FFFFFFFF);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.UpdateSamplerState(const NewProgram: TProgram);
var
  NullSampler: ID3D11SamplerState;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  PushClearFPUState;
  try



    if NewProgram <> TProgram.TexturedI then
    begin
      if (TCanvasAttribute.Antialias in Attributes) and (TCanvasAttribute.MipMapping in Attributes) then
        FContext.Context.PSSetSamplers(0, 1, @FMipMapSampler)
      else if (TCanvasAttribute.Antialias in Attributes) and (not (TCanvasAttribute.MipMapping in Attributes)) then
        FContext.Context.PSSetSamplers(0, 1, @FLinearSampler)
      else
        FContext.Context.PSSetSamplers(0, 1, @FPointSampler);

      NullSampler := nil;
      FContext.Context.PSSetSamplers(1, 1, @NullSampler);
    end
    else
    begin
      FContext.Context.PSSetSamplers(0, 1, @FPointSampler);
      FContext.Context.PSSetSamplers(1, 1, @FPointSampler);
    end;
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.ResetSamplerState;
var
  NullSampler: ID3D11SamplerState;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  NullSampler := nil;

  PushClearFPUState;
  try
    FContext.Context.PSSetSamplers(0, 1, @NullSampler);
    FContext.Context.PSSetSamplers(1, 1, @NullSampler);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.Reset;

begin
  inherited;
  ViewMatrix := IdentityMtx4;
  ProjMatrix := IdentityMtx4;

  FCurrentVertexCount := 0;
  FCurrentIndexCount := 0;

  FActiveTopology := TTopology.Unknown;
  FActiveTexture := nil;
  FActiveProgram := TProgram.Unknown;
  FActiveShaderEffect := nil;
  FActiveBlendingEffect := TBlendingEffect.Unknown;
  FActivePremultipliedAlpha := False;

  if RetrieveViewport then
  begin
    if not OverrideCanvasSize then begin
      FNormalSize.X := FViewport.Width * 0.5;
      FNormalSize.Y := FViewport.Height * 0.5;
    end else begin

    end;


  end
  else
    FNormalSize := UnityPoint2;

  ResetRasterState;
  ResetDepthStencilState;
  ResetShaderViews;
  ResetSamplerState;
  ResetBlendingState;
end;

function TDX11Canvas.UploadVertexBuffer: Boolean;
var
  Mapped: D3D11_MAPPED_SUBRESOURCE;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FVertexBuffer = nil) then
    Exit(False);

  if Failed(FContext.Context.Map(FVertexBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, Mapped)) then
    Exit(False);
  try
    Move(FVertexArray^, Mapped.Data^, FCurrentVertexCount * SizeOf(TVertexEntry));
  finally
    FContext.Context.Unmap(FVertexBuffer, 0);
  end;

  Result := True;
end;

function TDX11Canvas.UploadIndexBuffer: Boolean;
var
  Mapped: D3D11_MAPPED_SUBRESOURCE;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FIndexBuffer = nil) then
    Exit(False);

  if Failed(FContext.Context.Map(FIndexBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, Mapped)) then
    Exit(False);
  try
    Move(FIndexArray^, Mapped.Data^, FCurrentIndexCount * SizeOf(TIndexEntry));
  finally
    FContext.Context.Unmap(FIndexBuffer, 0);
  end;

  Result := True;
end;

procedure TDX11Canvas.SetBuffersAndTopology;
var
  VertexStride, VertexOffset: LongWord;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  VertexStride := SizeOf(TVertexEntry);
  VertexOffset := 0;

  FContext.Context.IASetVertexBuffers(0, 1, @FVertexBuffer, @VertexStride, @VertexOffset);

  case FActiveTopology of
    TTopology.Points:
      FContext.Context.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_POINTLIST);

    TTopology.Lines:
      FContext.Context.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_LINELIST);

    TTopology.Triangles:
    begin
      FContext.Context.IASetIndexBuffer(FIndexBuffer, DXGI_FORMAT_R16_UINT, 0);
      FContext.Context.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    end;
  end;
end;

procedure TDX11Canvas.DrawPrimitives;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  case FActiveTopology of
    TTopology.Points,
    TTopology.Lines:
      FContext.Context.Draw(FCurrentVertexCount, 0);

    TTopology.Triangles:
      FContext.Context.DrawIndexed(FCurrentIndexCount, 0, 0);
  end;
end;

procedure TDX11Canvas.Flush;
begin
  if FCurrentVertexCount > 0 then
  begin
    PushClearFPUState;
    try
      if UploadVertexBuffer and UploadIndexBuffer then
      begin
        SetBuffersAndTopology;
        DrawPrimitives;
      end;
    finally
      PopFPUState;
    end;

    NextDrawCall;
  end;

  ResetShaderViews;
  ResetSamplerState;

  if FActiveShaderEffect <> nil then
  begin
    FActiveShaderEffect.Deactivate;
    FActiveShaderEffect := nil;
  end;

  FCurrentVertexCount := 0;
  FCurrentIndexCount := 0;
  FActiveTopology := TTopology.Unknown;
  FActiveProgram := TProgram.Unknown;
  FActiveBlendingEffect := TBlendingEffect.Unknown;

  FActiveTexture := nil;
end;

procedure TDX11Canvas.GetBlendingParams(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean;
  out SrcColor, DestColor, SrcAlpha, DestAlpha: D3D11_BLEND);
begin
  case BlendingEffect of
    TBlendingEffect.None:
      begin
        SrcColor := D3D11_BLEND_ONE;
        DestColor := D3D11_BLEND_ZERO;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ZERO;
      end;

    TBlendingEffect.Normal:
      if not PremultipliedAlpha then
      begin
        SrcColor := D3D11_BLEND_SRC_ALPHA;
        DestColor := D3D11_BLEND_INV_SRC_ALPHA;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ONE;
      end
      else
      begin
        SrcColor := D3D11_BLEND_ONE;
        DestColor := D3D11_BLEND_INV_SRC_ALPHA;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ONE;
      end;

    TBlendingEffect.Shadow:
      begin
        SrcColor := D3D11_BLEND_ZERO;
        DestColor := D3D11_BLEND_INV_SRC_ALPHA;
        SrcAlpha := D3D11_BLEND_ZERO;
        DestAlpha := D3D11_BLEND_INV_SRC_ALPHA;
      end;

    TBlendingEffect.Add:
      if not PremultipliedAlpha then
      begin
        SrcColor := D3D11_BLEND_SRC_ALPHA;
        DestColor := D3D11_BLEND_ONE;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ONE;
      end
      else
      begin
        SrcColor := D3D11_BLEND_ONE;
        DestColor := D3D11_BLEND_ONE;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ONE;
      end;

    TBlendingEffect.Multiply:
      begin
        SrcColor := D3D11_BLEND_ZERO;
        DestColor := D3D11_BLEND_SRC_COLOR;
        SrcAlpha := D3D11_BLEND_ZERO;
        DestAlpha := D3D11_BLEND_SRC_ALPHA;
      end;

    TBlendingEffect.InverseMultiply:
      begin
        SrcColor := D3D11_BLEND_ZERO;
        DestColor := D3D11_BLEND_INV_SRC_COLOR;
        SrcAlpha := D3D11_BLEND_ZERO;
        DestAlpha := D3D11_BLEND_INV_SRC_ALPHA;
      end;

    TBlendingEffect.SourceColor:
      begin
        SrcColor := D3D11_BLEND_SRC_COLOR;
        DestColor := D3D11_BLEND_INV_SRC_COLOR;
        SrcAlpha := D3D11_BLEND_SRC_ALPHA;
        DestAlpha := D3D11_BLEND_INV_SRC_ALPHA;
      end;

    TBlendingEffect.SourceColorAdd:
      begin
        SrcColor := D3D11_BLEND_SRC_COLOR;
        DestColor := D3D11_BLEND_ONE;
        SrcAlpha := D3D11_BLEND_SRC_ALPHA;
        DestAlpha := D3D11_BLEND_ONE;
      end;
  end;
end;

function TDX11Canvas.SetEffectStates(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean): Boolean;
var
  BlendDesc: D3D11_BLEND_DESC;
begin
  if FContext = nil then
    Exit(False);

  if FBlendingStates[BlendingEffect, PremultipliedAlpha] = nil then
  begin
    if FContext.Device = nil then
      Exit(False);

    FillChar(BlendDesc, SizeOf(D3D11_BLEND_DESC), 0);

    BlendDesc.RenderTarget[0].BlendEnable := true;
    BlendDesc.RenderTarget[0].BlendOp := D3D11_BLEND_OP_ADD;
    BlendDesc.RenderTarget[0].BlendOpAlpha := D3D11_BLEND_OP_ADD;
    BlendDesc.RenderTarget[0].RenderTargetWriteMask := Ord(D3D11_COLOR_WRITE_ENABLE_ALL);

    GetBlendingParams(BlendingEffect, PremultipliedAlpha, BlendDesc.RenderTarget[0].SrcBlend,
      BlendDesc.RenderTarget[0].DestBlend, BlendDesc.RenderTarget[0].SrcBlendAlpha,
      BlendDesc.RenderTarget[0].DestBlendAlpha);

    PushClearFPUState;
    try
      if Failed(FContext.Device.CreateBlendState(BlendDesc, @FBlendingStates[BlendingEffect, PremultipliedAlpha])) then
        Exit(False);
    finally
      PopFPUState;
    end;
  end;

  if FContext.Context = nil then
    Exit(False);

  PushClearFPUState;
  try
    FContext.Context.OMSetBlendState(FBlendingStates[BlendingEffect, PremultipliedAlpha], nil, $FFFFFFFF);
  finally
    PopFPUState;
  end;

  Result := True;
end;

procedure TDX11Canvas.SetNormalSize(const Value: TPoint2);
begin
  FNormalSize := Value;
end;

function TDX11Canvas.RequestCache(const ATopology: TTopology; AProgram: TProgram; const Vertices, Indices: Integer;
  const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture; const RefMap: TCustomBaseTExture): Boolean;
var
  PremultipliedAlpha: Boolean;
begin
  if (Vertices > MaxCachedVertices) or (Indices > MaxCachedIndices) then
  begin
    LogText(SCanvasGeometryTooBig, TLogType.Error);
    Exit(False);
  end;

  Result := True;

  if (Texture <> nil) and (AProgram = TProgram.Textured) then
  begin
    if Texture.PixelFormat in [TPixelFormat.L8, TPixelFormat.L16] then
      AProgram := TProgram.TexturedL
    else if Texture.PixelFormat in [TPixelFormat.A4L4, TPixelFormat.A8L8] then
      AProgram := TProgram.TexturedLA
    else if Texture.PixelFormat = TPixelFormat.A8 then
      AProgram := TProgram.TexturedA
    else if Texture.PixelFormat = TPixelFormat.I8 then
      AProgram := TProgram.TexturedI;
  end;

  if (FCurrentVertexCount + Vertices > MaxCachedVertices) or (FCurrentIndexCount + Indices > MaxCachedIndices) or
    (FActiveTopology = TTopology.Unknown) or (FActiveTopology <> ATopology) or
    (FActiveProgram = TProgram.Unknown) or (FActiveProgram <> AProgram) or
    (FActiveBlendingEffect = TBlendingEffect.Unknown) or (FActiveBlendingEffect <> BlendingEffect) or
    (FActiveTexture <> Texture) or
    (FActiveRefMap <> RefMap) then
  begin
    Flush;

    PremultipliedAlpha := False;

    if Texture <> nil then
      PremultipliedAlpha := Texture.PremultipliedAlpha;

    if (FActiveBlendingEffect = TBlendingEffect.Unknown) or (FActiveBlendingEffect <> BlendingEffect) or
      (FActivePremultipliedAlpha <> PremultipliedAlpha) then
      SetEffectStates(BlendingEffect, PremultipliedAlpha);

    if (FActiveTexture <> Texture) or (FActiveProgram <> AProgram) or (RefMap <> FCurrentReflectionMAp) then
    begin
      if Texture <> nil then
      begin
        Texture.Bind(0);

        if (FPaletteTexture <> nil) and (AProgram = TProgram.TexturedI) then
          FPaletteTexture.Bind(1)
        else
          Self.RemoveShaderBinding(1);


        if FCurrentReflection <> nil then
          FCurrentReflection.bind(1)
        else
          Self.RemoveShaderBinding(1);

        if FCurrentReflectionMAp <> nil then
          FCurrentReflectionMAp.bind(2)
        else
          Self.RemoveShaderBinding(2);


        UpdateSamplerState(AProgram);
        SendShaderConstants;

      end
      else
      begin
        Self.RemoveShaderBinding(0);
//        ResetShaderViews;
//        ResetSamplerState;
      end;
    end;



    if (FActiveProgram = TProgram.Unknown) or (FActiveProgram <> AProgram) then
    begin
      case AProgram of
        TProgram.Solid:
          FActiveShaderEffect := FEffectSolid;

        TProgram.Textured:
          FActiveShaderEffect := FEffectTextured;

        TProgram.TexturedL:
          FActiveShaderEffect := FEffectTexturedL;

        TProgram.TexturedLA:
          FActiveShaderEffect := FEffectTexturedLA;

        TProgram.TexturedA:
          FActiveShaderEffect := FEffectTexturedA;

        TProgram.TexturedI:
          FActiveShaderEffect := FEffectTexturedI;

        else
          FActiveShaderEffect := nil;
      end;

      if (FActiveShaderEffect <> nil) and (not FActiveShaderEffect.Activate) then
      begin
        LogText(SCouldNotActivateShaderEffect, TLogType.Error);
        Result := False;
      end;
    end;

    FActiveTopology := ATopology;
    FActiveTexture := Texture;
    FActiveProgram := AProgram;
    FActiveBlendingEffect := BlendingEffect;
    FActivePremultipliedAlpha := PremultipliedAlpha;


  end;
end;

procedure TDX11Canvas.SelectLights(lights: PPXLLIght; count: Integer);
var
  cnt: ni;
begin
  inherited;
  cnt := count;
  if cnt > Length(FLights) then
    cnt := length(FLIghts);

  movemem32(@Flights[0], lights, count*sizeof(TPXLLIght));

  SendLights;


end;

procedure TDX11Canvas.SendLights;
var
  lmap: D3d11_MAPPED_SUBRESOURCE;
begin
  Flush;

  FContext.Context.PSSetConstantBuffers(1, 1, @FLightBuffer);
  SendShaderConstants;

//  FLights[0].param.x := random(100)/100;
//  FLights[0].color.y := random(100)/100;
//  FLights[0].color.z := random(100)/100;

  if Failed(FContext.Context.Map(FLightBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, lmap)) then
    Exit;
  try
    Move((@FLights[0])^, lmap.Data^, sizeof(FLIghts){sizeof(TPXLLight)*light_idx});
  finally
    FContext.Context.Unmap(FLightBuffer, 0);
  end;

  FContext.Context.PSSetConstantBuffers(1, 1, @FLightBuffer);



end;

procedure TDX11Canvas.SendPixelShaderConstants;
var
  lmap: D3d11_MAPPED_SUBRESOURCE;
  m: TMAterialProperties;
begin


  m := CurrentMaterial;
  if (FCurrentReflectionMap = nil) or (FCurrentReflection=nil) then begin
    m.minreflection := 0;
    m.maxreflection := 0;
  end;

//  if m.luminence <> 0 then
//    debug.consolelog('here');

  move( m.maxreflection, FPSConstants[0],sizeof(TVEctor4));//quick copy first 4 singles from record
  move( m.ambience, FPSConstants[3], sizeof(TVEctor4));//quick copy next 4 singles

  FPSConstants[2] := VEctor4(ScreenCEnter.x, ScreenCenter.y, UnityDepth,0);//make sure shader knows where the center is

  FContext.Context.PSSetConstantBuffers(0, 1, @FPSConstantBuffer);

  if Failed(FContext.Context.Map(FPSConstantBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, lmap)) then
    Exit;
  try
    Move((@FPSConstants[0])^, lmap.Data^, sizeof(FPSConstants));
  finally
    FContext.Context.Unmap(FPSConstantBuffer, 0);
  end;

end;

procedure TDX11Canvas.SendShaderConstants;
begin
  SendVertexShaderConstants;
  SendPixelShaderConstants;
end;

procedure TDX11Canvas.SendVertexShaderConstants;
var
  lmap: D3d11_MAPPED_SUBRESOURCE;
  m: TMatrix4;
begin
//  if m.luminence <> 0 then
//    debug.consolelog('here');
  m := transposemtx4(ViewMatrix);
  movemem32(@FVSConstants[0], @m.data[0], sizeof(ViewMatrix));
  m := transposemtx4(ProjMatrix);
  movemem32(@FVSConstants[4], @m.data[0], sizeof(ProjMatrix));
  FContext.Context.VSSetConstantBuffers(0, 1, @FVSConstantBuffer);

  if Failed(FContext.Context.Map(FVSConstantBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, lmap)) then
    Exit;
  try
    Move((@FVSConstants[0])^, lmap.Data^, sizeof(FVSConstants));
  finally
    FContext.Context.Unmap(FVSConstantBuffer, 0);
  end;
end;

function TDX11Canvas.NextVertexEntry: Pointer;
begin
  Result := Pointer(PtrInt(FVertexArray) + FCurrentVertexCount * SizeOf(TVertexEntry));
end;

procedure TDX11Canvas.AddIndexEntry(const Index: Integer);
var
  Entry: PWord;
begin
  Entry := Pointer(PtrInt(FIndexArray) + FCurrentIndexCount * SizeOf(TIndexEntry));
  Entry^ := Index;

  Inc(FCurrentIndexCount);
end;

procedure TDX11Canvas.AddVertexEntry(const Position, TexCoord: TPoint2; const Color: Cardinal; const refCoord: TPOint2);
var
  Entry: PVertexEntry;
begin
  Entry := NextVertexEntry;
  Entry.X := POsition.x;//(Position.X - FNormalSize.X) / FNormalSize.X;
  Entry.Y := POsition.y;//(FNormalSize.Y - Position.Y) / FNormalSize.Y;
{$IFDEF USE_ZW}
  Entry.Z := 0;
  Entry.W := 1;
{$ENDIF}
  Entry.Color := DisplaceRB(Color);
  Entry.U := TexCoord.X;
  Entry.V := TexCoord.Y;

  Inc(FCurrentVertexCount);
end;

procedure TDX11Canvas.PutPixel(const Point: TPoint2; const Color: TIntColor);
begin
  RequestCache(TTopology.Points, TProgram.Solid, 1, 0, TBlendingEffect.Normal, nil, nil);

  AddVertexEntry(Point + Point2(0.5, 0.5), ZeroPoint2, Color, Point);
end;

procedure TDX11Canvas.Line(const SrcPoint, DestPoint: TPoint2; const Color: TIntColor2);
begin
  RequestCache(TTopology.Lines, TProgram.Solid, 2, 0, TBlendingEffect.Normal, nil, nil);

  AddVertexEntry(SrcPoint + Point2(0.5, 0.5), ZeroPoint2, Color.First, SrcPoint);
  AddVertexEntry(DestPoint + Point2(0.5, 0.5), ZeroPoint2, Color.Second, DestPoint);
end;

procedure TDX11Canvas.DrawIndexedTriangles(const Vertices: PPoint2; const Colors: PIntColor; const Indices: PLongInt;
  const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect);
var
  Index: PLongInt;
  Vertex: PPoint2;
  RefCoords: TPOint2;
  Color: PIntColor;
  I: Integer;
begin
  RequestCache(TTopology.Triangles, TProgram.Solid, VertexCount, TriangleCount * 3, BlendingEffect, nil, nil);

  Index := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    AddIndexEntry(FCurrentVertexCount + Index^);
    Inc(Index);
  end;

  Vertex := Vertices;
  Color := Colors;

  REfCoords := point2(0,0);
  for I := 0 to VertexCount - 1 do
  begin

    AddVertexEntry(Vertex^, ZeroPoint2, Color^, refcoords);

    Inc(Vertex);
    Inc(Color);
  end;
end;

procedure TDX11Canvas.DrawTexturedTriangles(const Texture: TCustomBaseTexture;  const RefMap: TCustomBaseTExture; const Vertices, TexCoords: PPoint2;
  const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
  const BlendingEffect: TBlendingEffect);
var
  Index: PLongInt;
  Vertex, TexCoord: PPoint2;
  Color: PIntColor;
  I: Integer;
begin
  RequestCache(TTopology.Triangles, TProgram.Textured, VertexCount, TriangleCount * 3, BlendingEffect, Texture, RefMap);

  Index := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    AddIndexEntry(FCurrentVertexCount + Index^);
    Inc(Index);
  end;

  Vertex := Vertices;
  TexCoord := TexCoords;
  Color := Colors;

  for I := 0 to VertexCount - 1 do
  begin
    AddVertexEntry(Vertex^, TexCoord^, Color^);

    Inc(Vertex);
    Inc(TexCoord);
    Inc(Color);
  end;
end;

procedure TDX11Canvas.DrawTexturedTriangles3D(
  const Texture: TCustomBaseTexture;
  const RefMap: TCustomBaseTexture;
  const Vertices: PVector4; TexCoords: PPoint2; const Colors: PIntColor;
  const Indices: PLongInt; const VertexCount, TriangleCount: Integer; RefCoords: PPoint2;
  const MAt: TMaterialProperties);
var
  Index: PLongInt;
  Vertex: PVector4;
  RefCoord: PPoint2;
  TexCoord: PPoint2;
  Color: PIntColor;
  I: Integer;
begin
  self.CurrentMaterial := Mat;
  if CurrentMaterialChanged then begin
    Flush;
    SendShaderConstants;
  end;
  RequestCache(TTopology.Triangles, TProgram.Textured, VertexCount, TriangleCount * 3, mat.blendingeffect, Texture, RefMap);

  Index := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    AddIndexEntry(FCurrentVertexCount + Index^);
    Inc(Index);
  end;

  RefCoord := RefCoords;
  Vertex := Vertices;
  TexCoord := TexCoords;
  Color := Colors;

  for I := 0 to VertexCount - 1 do
  begin
    AddVertexEntry(Vertex^, TexCoord^, Color^, REfCoord^);
    inc(RefCoord);
    Inc(Vertex);
    Inc(TexCoord);
    Inc(Color);
  end;
end;

function TDX11Canvas.InitCanvas: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX11DeviceContext)) then
    Exit(False);

  FContext := TDX11DeviceContext(Device.Context);

  CreateStaticObjects;

  if not CreateDynamicObjects then
  begin
    DestroyStaticObjects;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX11Canvas.DoneCanvas;
begin
  DestroyDynamicObjects;
  DestroyStaticObjects;
  FContext := nil;
end;

function TDX11Canvas.AddLIght(l: TPXLLight): boolean;
begin
  result := false;
  if light_idx <= high(FLights) then begin
    FLights[light_idx] := l;
    inc(light_idx);
    result := true;
  end;

end;

procedure TDX11Canvas.AddVertexEntry(const Position: TVector4;
  TexCoord: TPoint2; const Color: Cardinal; const refCoord: TPOint2);
var
  Entry: PVertexEntry;
begin
  Entry := NextVertexEntry;
  Entry.X := POsition.x;//;(Position.X - FNormalSize.X) / FNormalSize.X;
  Entry.Y := POsition.y;//(FNormalSize.Y - Position.Y) / FNormalSize.Y;
{$IFDEF USE_ZW}
  Entry.Z := Position.Z;
  entry.W := FarPlane;
{$IFDEF USE_NEW_CLIPPING}
  entry.clipfar := FarPLane;
  entry.clipnear := 0.0001;
  entry.clipleft := FarPLane;
  entry.clipright := FarPLane;
{$ENDIF}
{$ENDIF}
  Entry.Color := DisplaceRB(Color);
  Entry.U := TexCoord.X;
  Entry.V := TexCoord.Y;
  Entry.U2 := RefCoord.x;
  Entry.V2 := RefCoord.y;


  Inc(FCurrentVertexCount);
end;

procedure TDX11Canvas.AddVertexEntry(const Position, TexCoord: TPoint2;  const Color: Cardinal);
begin
  AddVertexEntry(position, texcoord, color, point2(0,0));
end;

function TDX11Canvas.BeginDraw: Boolean;
begin
  Reset;
  Result := True;
end;

procedure TDX11Canvas.EndDraw;
begin
  Flush;
end;

function TDX11Canvas.DeviceRestore: Boolean;
begin
  Result := CreateDynamicObjects;
end;

procedure TDX11Canvas.DeviceRelease;
begin
  DestroyDynamicObjects;
end;

procedure TDX11Canvas.UpdateAttributes;
begin
  if FCurrentVertexCount > 0 then
    Flush;
end;

function TDX11Canvas.GetClipRect: TIntRect;
begin
  Result := FScissorRect;
end;

procedure TDX11Canvas.SetClipRect(const Value: TIntRect);
var
  TempRect: D3D11_RECT;
begin
  if FScissorRect <> Value then
  begin
    FScissorRect := Value;

    if (FContext <> nil) and (FContext.Device <> nil) then
    begin
      if FCurrentVertexCount > 0 then
        Flush;

      TempRect.Left := FScissorRect.Left;
      TempRect.Top := FScissorRect.Top;
      TempRect.Right := FScissorRect.Right;
      TempRect.Bottom := FScissorRect.Bottom;

      PushClearFPUState;
      try
        FContext.Context.RSSetScissorRects(1, @TempRect);
      finally
        PopFPUState;
      end;
    end;
  end;
end;

function TDX11Canvas.SetPalette(const Palette: TIntColorPalette): Boolean;
var
  LockedPixels: TLockedPixels;
  I: Integer;
begin
  if FPaletteTexture = nil then
  begin
    if (Device <> nil) and (not (Device.Provider is TGraphicsDeviceProvider)) then
      Exit(False);

    FPaletteTexture := TGraphicsDeviceProvider(Device.Provider).CreateLockableTexture(Device);
    if FPaletteTexture = nil then
      Exit(False);

    FPaletteTexture.Width := 256;
    FPaletteTexture.Height := 1;
    FPaletteTexture.PixelFormat := TPixelFormat.A8R8G8B8;
    FPaletteTexture.DynamicTexture := True;

    if not FPaletteTexture.Initialize then
    begin
      FreeAndNil(FPaletteTexture);
      Exit(False);
    end;
  end;

  Flush;

  if not FPaletteTexture.Lock(LockedPixels) then
    Exit(False);
  try
    for I := 0 to 255 do
      LockedPixels.Pixels[I, 0] := Palette[I];
  finally
    Result := FPaletteTexture.Unlock;
  end;
end;


procedure TDX11Canvas.ResetPalette;
begin
  Flush;
  FreeAndNil(FPaletteTexture);
end;

{$ENDREGION}

end.
