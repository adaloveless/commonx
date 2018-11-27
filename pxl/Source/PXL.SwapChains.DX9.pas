unit PXL.SwapChains.DX9;
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
  Jedi.Direct3D9, PXL.TypeDef, PXL.SwapChains, PXL.Types.DX9;

type
  TDX9SwapChain = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX9DeviceContext;
    FInitialized: Boolean;

    FD3DSwapChain: IDirect3DSwapChain9;
    FPresentParams: D3DPRESENT_PARAMETERS;
    FDepthStencil: IDirect3DSurface9;
    FDepthStencilFormat: D3DFORMAT;

    FSavedBackBuffer: IDirect3DSurface9;
    FSavedDepthStencil: IDirect3DSurface9;

    procedure UpdatePresentParams(const SwapChainInfo: TSwapChainInfo);

    function CreateSwapChain(var SwapChainInfo: TSwapChainInfo): Boolean;
    procedure DestroySwapChain;

    function CreateDepthStencil(const SwapChainInfo: TSwapChainInfo): Boolean;
    procedure DestroyDepthStencil;
  public
    constructor Create(const AContext: TDX9DeviceContext);
    destructor Destroy; override;

    function Initialize(const SwapChainInfo: PSwapChainInfo): Boolean;
    procedure Finalize;

    function SetRenderBuffers: Boolean;
    function SetDefaultViewport: Boolean;
    function RestoreRenderBuffers: Boolean;

    function Present: Boolean;

    property Context: TDX9DeviceContext read FContext;
    property Initialized: Boolean read FInitialized;

    property D3DSwapChain: IDirect3DSwapChain9 read FD3DSwapChain;
    property PresentParams: D3DPRESENT_PARAMETERS read FPresentParams;
    property DepthStencil: IDirect3DSurface9 read FDepthStencil;
  end;

  TDX9SwapChains = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX9DeviceContext;
    FSwapChains: array of TDX9SwapChain;

    function GetCount: Integer;
    function GetItem(const Index: Integer): TDX9SwapChain;
  public
    constructor Create(const AContext: TDX9DeviceContext);
    destructor Destroy; override;

    function Add(const SwapChainInfo: PSwapChainInfo): Integer;
    procedure Clear;

    function Recreate(const UserChains: TSwapChains): Boolean;

    property Context: TDX9DeviceContext read FContext;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TDX9SwapChain read GetItem; default;
  end;

implementation

uses
  Windows, SysUtils, PXL.Types;

{$REGION 'TDX9SwapChain'}

constructor TDX9SwapChain.Create(const AContext: TDX9DeviceContext);
begin
  inherited Create;

  FContext := AContext;
  Increment_PXL_ClassInstances;
end;

destructor TDX9SwapChain.Destroy;
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

procedure TDX9SwapChain.UpdatePresentParams(const SwapChainInfo: TSwapChainInfo);
begin
  with FPresentParams do
  begin
    BackBufferWidth := SwapChainInfo.Width;
    BackBufferHeight := SwapChainInfo.Height;

    Windowed := True;
    SwapEffect := D3DSWAPEFFECT_DISCARD;

    hDeviceWindow := SwapChainInfo.WindowHandle;

    PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
    if SwapChainInfo.VSync then
      PresentationInterval := D3DPRESENT_INTERVAL_ONE;

    BackBufferFormat := FContext.FindBackBufferFormat(SwapChainInfo.Format);
  end;

  FDepthStencilFormat := FContext.FindDepthStencilFormat(SwapChainInfo.DepthStencil);

  FContext.FindBestMultisampleType(FPresentParams.BackBufferFormat, FDepthStencilFormat, SwapChainInfo.Multisamples,
    FPresentParams.MultiSampleType, FPresentParams.MultiSampleQuality);
end;

function TDX9SwapChain.CreateSwapChain(var SwapChainInfo: TSwapChainInfo): Boolean;
begin
  if (FContext.Direct3DDevice = nil) or (FPresentParams.BackBufferWidth < 1) or
    (FPresentParams.BackBufferHeight < 1) then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.CreateAdditionalSwapChain(FPresentParams, FD3DSwapChain));
  if Result then
  begin
    SwapChainInfo.Format := TDX9DeviceContext.FormatToNative(FPresentParams.BackBufferFormat);
    SwapChainInfo.Multisamples := Ord(FPresentParams.MultiSampleType);
  end;
end;

procedure TDX9SwapChain.DestroySwapChain;
begin
  FD3DSwapChain := nil;
  FDepthStencilFormat := D3DFMT_UNKNOWN;
  FillChar(FPresentParams, SizeOf(D3DPRESENT_PARAMETERS), 0);
end;

function TDX9SwapChain.CreateDepthStencil(const SwapChainInfo: TSwapChainInfo): Boolean;
begin
  if SwapChainInfo.DepthStencil = TDepthStencil.None then
    Exit(True);

  if (FContext.Direct3DDevice = nil) or (FDepthStencilFormat = D3DFMT_UNKNOWN) then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.CreateDepthStencilSurface(SwapChainInfo.Width, SwapChainInfo.Height,
    FDepthStencilFormat, FPresentParams.MultiSampleType, FPresentParams.MultiSampleQuality, True, FDepthStencil, nil));
end;

procedure TDX9SwapChain.DestroyDepthStencil;
begin
  FDepthStencil := nil;
end;

function TDX9SwapChain.Initialize(const SwapChainInfo: PSwapChainInfo): Boolean;
begin
  if FInitialized or (SwapChainInfo = nil) or (FContext = nil) then
    Exit(False);

  UpdatePresentParams(SwapChainInfo^);

  if not CreateSwapChain(SwapChainInfo^) then
    Exit(False);

  if not CreateDepthStencil(SwapChainInfo^) then
  begin
    DestroySwapChain;
    Exit(False);
  end;

  FInitialized := True;
  Result := True;
end;

procedure TDX9SwapChain.Finalize;
begin
  if not FInitialized then
    Exit;

  RestoreRenderBuffers;
  DestroyDepthStencil;
  DestroySwapChain;

  FInitialized := False;
end;

function TDX9SwapChain.SetRenderBuffers: Boolean;
var
  BackBuffer: IDirect3DSurface9;
  Res: HResult;
begin
  if (FContext.Direct3DDevice = nil) or (FD3DSwapChain = nil) then
    Exit(False);

  if Failed(FContext.Direct3DDevice.GetRenderTarget(0, FSavedBackBuffer)) then
    Exit(False);

  Res := FContext.Direct3DDevice.GetDepthStencilSurface(FSavedDepthStencil);
  if Res = D3DERR_NOTFOUND then
    FSavedDepthStencil := nil
  else if Failed(Res) then
    Exit(False);

  if Failed(FD3DSwapChain.GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, BackBuffer)) then
  begin
    FSavedDepthStencil := nil;
    FSavedBackBuffer := nil;
    Exit(False);
  end;

  if Failed(FContext.Direct3DDevice.SetRenderTarget(0, BackBuffer)) then
  begin
    FSavedDepthStencil := nil;
    FSavedBackBuffer := nil;
    Exit(False);
  end;

  if Failed(FContext.Direct3DDevice.SetDepthStencilSurface(FDepthStencil)) then
  begin
    FSavedDepthStencil := nil;
    FSavedBackBuffer := nil;
    Exit(False);
  end;

  Result := True;
end;

function TDX9SwapChain.SetDefaultViewport: Boolean;
var
  Viewport: TD3DViewport9;
begin
  if (FContext.Direct3DDevice = nil) or (FPresentParams.BackBufferWidth < 1) or
    (FPresentParams.BackBufferHeight < 1) then
    Exit(False);

  Viewport.X := 0;
  Viewport.Y := 0;
  Viewport.Width := FPresentParams.BackBufferWidth;
  Viewport.Height := FPresentParams.BackBufferHeight;
  Viewport.MinZ := 0.0;
  Viewport.MaxZ := 1.0;

  Result := Succeeded(FContext.Direct3DDevice.SetViewport(Viewport));
end;

function TDX9SwapChain.RestoreRenderBuffers: Boolean;
begin
  try
    if (FContext.Direct3DDevice <> nil) and (FSavedBackBuffer <> nil) then
    begin
      if Failed(FContext.Direct3DDevice.SetDepthStencilSurface(FSavedDepthStencil)) then
        Exit(False);

      Result := Succeeded(FContext.Direct3DDevice.SetRenderTarget(0, FSavedBackBuffer));
    end
    else
      Result := False;
  finally
    FSavedDepthStencil := nil;
    FSavedBackBuffer := nil;
  end;
end;

function TDX9SwapChain.Present: Boolean;
begin
  if FD3DSwapChain = nil then
    Exit(False);

  Result := Succeeded(FD3DSwapChain.Present(nil, nil, 0, nil, 0));
end;

{$ENDREGION}
{$REGION 'TDX9SwapChains'}

constructor TDX9SwapChains.Create(const AContext: TDX9DeviceContext);
begin
  inherited Create;

  FContext := AContext;
  Increment_PXL_ClassInstances;
end;

destructor TDX9SwapChains.Destroy;
begin
  try
    Clear;
    FContext := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TDX9SwapChains.GetCount: Integer;
begin
  Result := Length(FSwapChains);
end;

function TDX9SwapChains.GetItem(const Index: Integer): TDX9SwapChain;
begin
  if (Index >= 0) and (Index < Length(FSwapChains)) then
    Result := FSwapChains[Index]
  else
    Result := nil;
end;

procedure TDX9SwapChains.Clear;
var
  I: Integer;
begin
  for I := Length(FSwapChains) - 1 downto 0 do
    FreeAndNil(FSwapChains[I]);

  SetLength(FSwapChains, 0);
end;

function TDX9SwapChains.Add(const SwapChainInfo: PSwapChainInfo): Integer;
var
  NewItem: TDX9SwapChain;
begin
  NewItem := TDX9SwapChain.Create(FContext);

  if not NewItem.Initialize(SwapChainInfo) then
  begin
    NewItem.Free;
    Exit(-1);
  end;

  Result := Length(FSwapChains);
  SetLength(FSwapChains, Result + 1);

  FSwapChains[Result] := NewItem;
end;

function TDX9SwapChains.Recreate(const UserChains: TSwapChains): Boolean;
var
  I, Index: Integer;
  SwapChainInfo: PSwapChainInfo;
begin
  if UserChains = nil then
    Exit(False);

  Clear;
  Result := True;

  // The first chain is skipped as it is created and handled by Direct3D device.
  for I := 1 to UserChains.Count - 1 do
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

  if not Result then
    Clear;
end;

{$ENDREGION}

end.
