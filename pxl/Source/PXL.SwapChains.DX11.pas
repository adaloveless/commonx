unit PXL.SwapChains.DX11;
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
  PXL.Windows.DXGI, PXL.Windows.D3D11, PXL.TypeDef, PXL.Types, PXL.SwapChains, PXL.Types.DX11;

type
  TDX11SwapChain = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX11DeviceContext;
    FInitialized: Boolean;

    FDXGISwapChain: IDXGISwapChain;
    FSwapChainDesc: DXGI_SWAP_CHAIN_DESC;

    FRenderTargetView: ID3D11RenderTargetView;
    FDepthStencilTex: ID3D11Texture2D;
    FDepthStencilView: ID3D11DepthStencilView;

    FSavedTargetView: ID3D11RenderTargetView;
    FSavedStencilView: ID3D11DepthStencilView;

    FVSyncEnabled: Boolean;
    FIdleState: Boolean;

    function FindSwapChainFormat(Format: TPixelFormat): DXGI_FORMAT;
    function CreateSwapChain(const SwapChainInfo: PSwapChainInfo): Boolean;
    procedure DestroySwapChain;

    function CreateRenderTargetView: Boolean;
    procedure DestroyRenderTargetView;

    function CreateDepthStencil(const SwapChainInfo: PSwapChainInfo): Boolean;
    procedure DestroyDepthStencil;

    procedure PreserveRenderTargets;
    procedure RestoreRenderTargets;
  public
    constructor Create(const AContext: TDX11DeviceContext);
    destructor Destroy; override;

    function Initialize(const SwapChainInfo: PSwapChainInfo): Boolean;
    procedure Finalize;

    function Resize(const SwapChainInfo: PSwapChainInfo; const UpdateFormat: Boolean = False): Boolean;

    function SetRenderTargets: Boolean;
    function SetDefaultViewport: Boolean;

    procedure ResetRenderTargets;

    function Present: HResult;
    function PresentTest: HResult;

    property Context: TDX11DeviceContext read FContext;
    property Initialized: Boolean read FInitialized;

    property DXGISwapChain: IDXGISwapChain read FDXGISwapChain;
    property SwapChainDesc: DXGI_SWAP_CHAIN_DESC read FSwapChainDesc;

    property RenderTargetView: ID3D11RenderTargetView read FRenderTargetView;
    property DepthStencilTex: ID3D11Texture2D read FDepthStencilTex;
    property DepthStencilView: ID3D11DepthStencilView read FDepthStencilView;

    property IdleState: Boolean read FIdleState write FIdleState;
  end;

  TDX11SwapChains = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX11DeviceContext;
    FSwapChains: array of TDX11SwapChain;

    function GetCount: Integer;
    function GetItem(const Index: Integer): TDX11SwapChain;
  public
    constructor Create(const AContext: TDX11DeviceContext);
    destructor Destroy; override;

    function Add(const SwapChainInfo: PSwapChainInfo): Integer;
    procedure Clear;

    function Recreate(const UserChains: TSwapChains): Boolean;

    property Context: TDX11DeviceContext read FContext;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TDX11SwapChain read GetItem; default;
  end;

implementation

uses
  Windows, SysUtils;

{$REGION 'TDX11SwapChain'}

constructor TDX11SwapChain.Create(const AContext: TDX11DeviceContext);
begin
  inherited Create;

  FContext := AContext;
  Increment_PXL_ClassInstances;
end;

destructor TDX11SwapChain.Destroy;
begin
  try
    if FInitialized then
      Finalize;

    FContext := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TDX11SwapChain.FindSwapChainFormat(Format: TPixelFormat): DXGI_FORMAT;
var
  NewFormat: TPixelFormat;
begin
  if Format = TPixelFormat.Unknown then
    Format := TPixelFormat.A8R8G8B8;

  NewFormat := FContext.FindDisplayFormat(Format);

  Result := DXGI_FORMAT_UNKNOWN;
  if NewFormat <> TPixelFormat.Unknown then
    Result := TDX11DeviceContext.NativeToFormat(NewFormat);

  if Result = DXGI_FORMAT_UNKNOWN then
    Result := DXGI_FORMAT_R8G8B8A8_UNORM;
end;

function TDX11SwapChain.CreateSwapChain(const SwapChainInfo: PSwapChainInfo): Boolean;
var
  SwapDesc: DXGI_SWAP_CHAIN_DESC;
  SampleCount, QualityLevel: Integer;
  NewFormat: TPixelFormat;
begin
  if (FContext.Device = nil) or (FContext.Factory = nil) or (SwapChainInfo.Width < 1) or (SwapChainInfo.Height < 0) or
    (SwapChainInfo.WindowHandle = 0) then
    Exit(False);

  // Prepare swap chain declaration.
  FillChar(SwapDesc, SizeOf(DXGI_SWAP_CHAIN_DESC), 0);

  SwapDesc.BufferCount := 1;

  SwapDesc.BufferDesc.Width := SwapChainInfo.Width;
  SwapDesc.BufferDesc.Height := SwapChainInfo.Height;
  SwapDesc.BufferDesc.Format := FindSwapChainFormat(SwapChainInfo.Format);

  SwapDesc.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
  SwapDesc.OutputWindow := SwapChainInfo.WindowHandle;

  FContext.FindBestMultisampleType(SwapDesc.BufferDesc.Format, SwapChainInfo.Multisamples, SampleCount, QualityLevel);

  SwapDesc.SampleDesc.Count := SampleCount;
  SwapDesc.SampleDesc.Quality := QualityLevel;

  SwapDesc.Windowed := True;

  // Create swap chain.
  PushClearFPUState;
  try
    if Failed(FContext.Factory.CreateSwapChain(FContext.Device, SwapDesc, FDXGISwapChain)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  // Retrieve the updated description of swap chain.
  FillChar(FSwapChainDesc, SizeOf(DXGI_SWAP_CHAIN_DESC), 0);

  PushClearFPUState;
  try
    if Failed(FDXGISwapChain.GetDesc(FSwapChainDesc)) then
    begin
      FDXGISwapChain := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  // Update user swap chain parameters.
  FVSyncEnabled := SwapChainInfo.VSync;

  SwapChainInfo.Multisamples := FSwapChainDesc.SampleDesc.Count;

  NewFormat := TDX11DeviceContext.FormatToNative(FSwapChainDesc.BufferDesc.Format);
  if NewFormat <> TPixelFormat.Unknown then
    SwapChainInfo.Format := NewFormat;

  Result := True;
end;

procedure TDX11SwapChain.DestroySwapChain;
begin
  FDXGISwapChain := nil;

  FillChar(FSwapChainDesc, SizeOf(DXGI_SWAP_CHAIN_DESC), 0);
  FVSyncEnabled := False;
end;

function TDX11SwapChain.CreateRenderTargetView: Boolean;
var
  BackBuffer: ID3D11Texture2D;
begin
  PushClearFPUState;
  try
    if Failed(FDXGISwapChain.GetBuffer(0, ID3D11Texture2D, BackBuffer)) then
      Exit(False);

    Result := Succeeded(FContext.Device.CreateRenderTargetView(BackBuffer, nil, @FRenderTargetView));
  finally
    PopFPUState;
  end;
end;

procedure TDX11SwapChain.DestroyRenderTargetView;
begin
  FRenderTargetView := nil;
end;

function TDX11SwapChain.CreateDepthStencil(const SwapChainInfo: PSwapChainInfo): Boolean;
var
  Format: DXGI_FORMAT;
  Desc: D3D11_TEXTURE2D_DESC;
begin
  if SwapChainInfo.DepthStencil <= TDepthStencil.None then
    Exit(True);

  Format := FContext.FindDepthStencilFormat(SwapChainInfo.DepthStencil);
  if Format = DXGI_FORMAT_UNKNOWN then
    Exit(False);

  FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);

  Desc.Format := Format;
  Desc.Width := FSwapChainDesc.BufferDesc.Width;
  Desc.Height := FSwapChainDesc.BufferDesc.Height;

  Desc.MipLevels := 1;
  Desc.ArraySize := 1;

  Desc.SampleDesc.Count := FSwapChainDesc.SampleDesc.Count;
  Desc.SampleDesc.Quality := FSwapChainDesc.SampleDesc.Quality;

  Desc.Usage := D3D11_USAGE_DEFAULT;
  Desc.BindFlags := D3D11_BIND_DEPTH_STENCIL;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateTexture2D(Desc, nil, @FDepthStencilTex)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  Result := Succeeded(FContext.Device.CreateDepthStencilView(FDepthStencilTex, nil, @FDepthStencilView));
  if not Result then
    FDepthStencilTex := nil;
end;

procedure TDX11SwapChain.DestroyDepthStencil;
begin
  FDepthStencilView := nil;
  FDepthStencilTex := nil;
end;

function TDX11SwapChain.Initialize(const SwapChainInfo: PSwapChainInfo): Boolean;
begin
  if FInitialized or (SwapChainInfo = nil) or (FContext = nil) then
    Exit(False);

  if not CreateSwapChain(SwapChainInfo) then
    Exit(False);

  if not CreateRenderTargetView then
  begin
    DestroySwapChain;
    Exit(False);
  end;

  if not CreateDepthStencil(SwapChainInfo) then
  begin
    DestroyRenderTargetView;
    DestroySwapChain;
    Exit(False);
  end;

  FInitialized := True;
  FIdleState := False;
  Result := True;
end;

procedure TDX11SwapChain.Finalize;
begin
  if not FInitialized then
    Exit;

  RestoreRenderTargets;

  DestroyDepthStencil;
  DestroyRenderTargetView;
  DestroySwapChain;

  FInitialized := False;
end;

function TDX11SwapChain.Resize(const SwapChainInfo: PSwapChainInfo; const UpdateFormat: Boolean): Boolean;
var
  SampleCount, QualityLevel: Integer;
begin
  if (not FInitialized) or (SwapChainInfo = nil) or (FDXGISwapChain = nil) or (FContext = nil) then
    Exit(False);

  // Destroy depth-stencil and render target views because they will be of different size.
  DestroyDepthStencil;
  DestroyRenderTargetView;

  // If the swap chain format needs to be updated, recalculate it.
  if UpdateFormat then
  begin
    FSwapChainDesc.BufferDesc.Format := FindSwapChainFormat(SwapChainInfo.Format);

    FContext.FindBestMultisampleType(FSwapChainDesc.BufferDesc.Format, SwapChainInfo.Multisamples, SampleCount,
      QualityLevel);

    FSwapChainDesc.SampleDesc.Count := SampleCount;
    FSwapChainDesc.SampleDesc.Quality := QualityLevel;
  end;

  // Resize the swap chain itself.
  PushClearFPUState;
  try
    if Failed(FDXGISwapChain.ResizeBuffers(1, SwapChainInfo.Width, SwapChainInfo.Height,
      FSwapChainDesc.BufferDesc.Format, 0)) then
      begin
        DestroySwapChain;
        Exit(False);
      end;
  finally
    PopFPUState;
  end;

  // Retrieve the updated description of swap chain.
  FillChar(FSwapChainDesc, SizeOf(DXGI_SWAP_CHAIN_DESC), 0);

  PushClearFPUState;
  try
    if Failed(FDXGISwapChain.GetDesc(FSwapChainDesc)) then
      begin
        DestroySwapChain;
        Exit(False);
      end;
  finally
    PopFPUState;
  end;

  // Create render target view with the new size.
  if not CreateRenderTargetView then
  begin
    DestroySwapChain;
    Exit(False);
  end;

  // Create depth stencil with the new size.
  if not CreateDepthStencil(SwapChainInfo) then
  begin
    DestroyRenderTargetView;
    DestroySwapChain;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX11SwapChain.PreserveRenderTargets;
begin
  if FContext.Context = nil then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.OMGetRenderTargets(1, @FSavedTargetView, @FSavedStencilView);
  finally
    PopFPUState;
  end;
end;

procedure TDX11SwapChain.RestoreRenderTargets;
begin
  if (FContext.Context <> nil) and (FSavedTargetView <> nil) then
  begin
    PushClearFPUState;
    try
      FContext.Context.OMSetRenderTargets(1, @FSavedTargetView, FSavedStencilView);
    finally
      PopFPUState;
    end;
  end;

  FSavedStencilView := nil;
  FSavedTargetView := nil;
end;

function TDX11SwapChain.SetRenderTargets: Boolean;
begin
  if (FContext.Context = nil) or (FRenderTargetView = nil) then
    Exit(False);

  PreserveRenderTargets;

  PushClearFPUState;
  try
    FContext.Context.OMSetRenderTargets(1, @FRenderTargetView, FDepthStencilView);
  finally
    PopFPUState;
  end;

  Result := True;
end;

procedure TDX11SwapChain.ResetRenderTargets;
begin
  RestoreRenderTargets;
end;

function TDX11SwapChain.SetDefaultViewport: Boolean;
var
  Viewport: D3D11_VIEWPORT;
begin
  if (FContext.Device = nil) or (FSwapChainDesc.BufferDesc.Width < 1) or (FSwapChainDesc.BufferDesc.Height < 1) then
    Exit(False);

  FillChar(Viewport, SizeOf(D3D11_VIEWPORT), 0);

  Viewport.Width := FSwapChainDesc.BufferDesc.Width;
  Viewport.Height := FSwapChainDesc.BufferDesc.Height;
  Viewport.MinDepth := 0.0;
  Viewport.MaxDepth := 1.0;

  PushClearFPUState;
  try
    FContext.Context.RSSetViewports(1, @Viewport);
  finally
    PopFPUState;
  end;

  Result := True;
end;

function TDX11SwapChain.Present: HResult;
var
  Interval: Cardinal;
begin
  if FDXGISwapChain = nil then
    Exit(DXGI_ERROR_INVALID_CALL);

  Interval := 0;
  if FVSyncEnabled then
    Interval := 1;

  PushClearFPUState;
  try
    Result := DXGISwapChain.Present(Interval, 0);
  finally
    PopFPUState;
  end;
end;

function TDX11SwapChain.PresentTest: HResult;
begin
  if FDXGISwapChain = nil then
    Exit(DXGI_ERROR_INVALID_CALL);

  PushClearFPUState;
  try
    Result := DXGISwapChain.Present(0, DXGI_PRESENT_TEST);
  finally
    PopFPUState;
  end;
end;

{$ENDREGION}
{$REGION 'TDX11SwapChains'}

constructor TDX11SwapChains.Create(const AContext: TDX11DeviceContext);
begin
  inherited Create;

  FContext := AContext;
  Increment_PXL_ClassInstances;
end;

destructor TDX11SwapChains.Destroy;
begin
  try
    Clear;
    FContext := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TDX11SwapChains.GetCount: Integer;
begin
  Result := Length(FSwapChains);
end;

function TDX11SwapChains.GetItem(const Index: Integer): TDX11SwapChain;
begin
  if (Index >= 0) and (Index < Length(FSwapChains)) then
    Result := FSwapChains[Index]
  else
    Result := nil;
end;

procedure TDX11SwapChains.Clear;
var
  I: Integer;
begin
  for I := Length(FSwapChains) - 1 downto 0 do
    FSwapChains[I].Free;

  SetLength(FSwapChains, 0);
end;

function TDX11SwapChains.Add(const SwapChainInfo: PSwapChainInfo): Integer;
var
  NewItem: TDX11SwapChain;
begin
  NewItem := TDX11SwapChain.Create(FContext);

  if not NewItem.Initialize(SwapChainInfo) then
  begin
    NewItem.Free;
    Exit(-1);
  end;

  Result := Length(FSwapChains);
  SetLength(FSwapChains, Result + 1);

  FSwapChains[Result] := NewItem;
end;

function TDX11SwapChains.Recreate(const UserChains: TSwapChains): Boolean;
var
  I, Index: Integer;
  SwapChainInfo: PSwapChainInfo;
begin
  if UserChains = nil then
    Exit(False);

  if Length(FSwapChains) > 0 then
    Clear;

  Result := False;

  for I := 0 to UserChains.Count - 1 do
  begin
    SwapChainInfo := UserChains[I];
    if SwapChainInfo = nil then
    begin
      Result := False;
      Break;
    end;

    Index := Add(SwapChainInfo);

    Result := Index <> -1;
    if not Result then
      Break;
  end;

  if (not Result) and (Length(FSwapChains) > 0) then
    Clear;
end;

{$ENDREGION}

end.
