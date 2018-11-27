unit PXL.SwapChains;
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
{< Devices with extra handling for windows, buffers and swap-chains. }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.TypeDef, PXL.Types, PXL.Devices;

type
  { Pointer to @link(TSwapChainInfo) structure typically used to pass it by reference. }
  PSwapChainInfo = ^TSwapChainInfo;

  { General description of rendering swap chain. }
  TSwapChainInfo = record
    { The width of rendering surface. }
    Width: Integer;

    { The height of rendering surface. }
    Height: Integer;

    { This parameter determines whether to wait for vertical retrace to provide flicker-free animations. }
    VSync: Boolean;

    { The desired pixel format to be used in the rendering target. This is a suggestion and different format may be
      chosen by the provider depending on hardware support. If this parameter is set to @code(TPixelFormat.Unknown)
      (by default), the best possible pixel format will be used. }
    Format: TPixelFormat;

    { The handle to the application's main window or a control where the rendering should be made (can be another
      window or even a panel). }
    WindowHandle: TUntypedHandle;

    { Number of samples to use for antialiasing. This is a suggestion and different value may actually be used by the
      provider depending on hardware support; values of zero and one are treated as no multisampling. }
    Multisamples: Integer;

    { The type of depth-stencil buffer to be used with the swap chain. }
    DepthStencil: TDepthStencil;
  end;

  { List of all rendering swap chains that are to be used with Asphyre device. This class describes all swap chains
    that should be created and used with the device; if the device is already initialized, modifying swap chains is
    not allowed. }
  TSwapChains = class
  private
    Data: array of TSwapChainInfo;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;

    function GetCount: Integer;
    function GetItem(const Index: Integer): PSwapChainInfo;
  public
    { @exclude } constructor Create(const ADevice: TCustomDevice);
    { @exclude } destructor Destroy; override;

    { Inserts a new swap chain to the end of list and returns its index. }
    function Insert: Integer;

    { Adds a new rendering swap chain with the specified parameters to the end of list and returns its index. }
    function Add(const WindowHandle: TUntypedHandle; const Size: TPoint2px; const Multisamples: Integer = 0;
      const VSync: Boolean = False; const Format: TPixelFormat = TPixelFormat.Unknown;
      const DepthStencil: TDepthStencil = TDepthStencil.None): Integer; overload;

    { Adds a new rendering swap chain specified in the given structure to the end of list and returns its index. }
    function Add(const Desc: TSwapChainInfo): Integer; overload;

    { Removes the swap at the specified index from the list, shifting all elements by one. The index should be in
      range of [0..(Count - 1)] range; if it is outside of valid range, this function does nothing. }
    procedure Remove(const Index: Integer);

    { Removes all rendering swap chains from the list. }
    procedure Clear;

    { The pointer to a valid graphics device which owns this list of rendering swap chains. }
    property Device: TCustomDevice read FDevice;

    { Number of swap chains in the list. }
    property Count: Integer read GetCount;

    { Provides access to each of the rendering swap chains in the list by index, which should be in range of
      [0..(Count - 1)] range. If the index is outside of valid range, @nil is returned. }
    property Items[const Index: Integer]: PSwapChainInfo read GetItem; default;
  end;

  { Hardware device wrapper that supports rendering to multiple swap chains, which can be located on one or several
    windows. }
  TCustomSwapChainDevice = class(TCustomStateDevice)
  private
    FSwapChains: TSwapChains;
  protected
    { Updates swap chain implementation-specific resources and resizes any related surfaces. }
    function ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean; virtual;

    { Returns @True when device is ready to render on the given swap chain and @False otherwise. }
    function MayRender(const SwapChainIndex: Integer): Boolean; virtual;
  public
    { @exclude } constructor Create(const AProvider: TCustomDeviceProvider);
    { @exclude } destructor Destroy; override;

    { Clears all textures, shaders and states currently bound to the device. This method works only on some modern
      providers. }
    procedure Reset; virtual;

    { Changes size of the back-buffer tied to swap chain identified by the given index. The first swap chain has index
      of zero. If the index is outside of valid range or the swap chain cannot be resized, the returned value
      is @False and the size of swap chain remains unchanged. If this method succeeds, the swap chain will have its
      size updated and @True will be returned. In some providers this may cause device to be reset and some resources
      to be recreated, so any resources that are not handled internally should be released before calling this; the
      best way to handle this scenario is to subscribe in @code(TCustomDevice.OnRelease) and
      @code(TCustomDevice.OnRestore) events. }
    function Resize(const SwapChainIndex: Integer; const NewSize: TPoint2px): Boolean;

    { Activates the specified swap chain and begins rendering to it. The rendering should end with call
      to @link(EndScene). Note that multiple cascade calls to @code(BeginScene) and @code(EndScene) are not allowed,
      so a next call to @code(BeginScene) without corresponding @code(EndScene) will fail. This function returns @True
      if the rendering started successfully and @False otherwise. If returned value is @False, there is no need to
      call @link(EndScene). }
    function BeginScene(const SwapChainIndex: Integer = 0): Boolean; virtual; abstract;

    { Finishes rendering to the swap chain that was started with @link(BeginScene). If the swap chain is located on a
      window, then this will present the contents. }
    function EndScene: Boolean; virtual; abstract;

    { The list of swap chains that will be used for rendering into. In a typical scenario at least one swap chain must
      be added to this list for device initialization to succeed. In FireMonkey applications the swap chains are not
      used and will be ignored by the device. }
    property SwapChains: TSwapChains read FSwapChains;
  end;

implementation

{$REGION 'TSwapChains'}

constructor TSwapChains.Create(const ADevice: TCustomDevice);
begin
  inherited Create;

  FDevice := ADevice;

  Inc(PXL_ClassInstances);
end;

destructor TSwapChains.Destroy;
begin
  Dec(PXL_ClassInstances);

  inherited;
end;

function TSwapChains.GetCount: Integer;
begin
  Result := Length(Data);
end;

function TSwapChains.GetItem(const Index: Integer): PSwapChainInfo;
begin
  if (Index >= 0) and (Index < Length(Data)) then
    Result := @Data[Index]
  else
    Result := nil;
end;

function TSwapChains.Insert: Integer;
begin
  Result := Length(Data);
  SetLength(Data, Result + 1);

  FillChar(Data[Result], SizeOf(TSwapChainInfo), 0);
end;

function TSwapChains.Add(const WindowHandle: TUntypedHandle; const Size: TPoint2px; const Multisamples: Integer;
  const VSync: Boolean; const Format: TPixelFormat; const DepthStencil: TDepthStencil): Integer;
begin
  Result := Insert;
  if Result <> -1 then
  begin
    Data[Result].WindowHandle := WindowHandle;

    Data[Result].Width := Size.x;
    Data[Result].Height := Size.y;
    Data[Result].Format := Format;
    Data[Result].VSync := VSync;

    Data[Result].Multisamples := Multisamples;
    Data[Result].DepthStencil := DepthStencil;
  end;
end;

function TSwapChains.Add(const Desc: TSwapChainInfo): Integer;
begin
  Result := Insert;

  if Result <> -1 then
    Move(Desc, Data[Result], SizeOf(TSwapChainInfo));
end;

procedure TSwapChains.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(Data)) then
    Exit;

  for I := Index to Length(Data) - 2 do
    Data[I] := Data[I + 1];

  SetLength(Data, Length(Data) - 1);
end;

procedure TSwapChains.Clear;
begin
  SetLength(Data, 0);
end;

{$ENDREGION}
{$REGION 'TCustomSwapChainDevice'}

constructor TCustomSwapChainDevice.Create;
begin
  inherited;

  FSwapChains := TSwapChains.Create(Self);
end;

destructor TCustomSwapChainDevice.Destroy;
begin
  FSwapChains.Free;

  inherited;
end;

procedure TCustomSwapChainDevice.Reset;
begin
end;

function TCustomSwapChainDevice.ResizeSwapChain(const SwapChainIndex: Integer;
  const NewSwapChainInfo: PSwapChainInfo): Boolean;
begin
  Result := True;
end;

function TCustomSwapChainDevice.MayRender(const SwapChainIndex: Integer): Boolean;
begin
  Result := True;
end;

function TCustomSwapChainDevice.Resize(const SwapChainIndex: Integer; const NewSize: TPoint2px): Boolean;
var
  SwapChainInfo: PSwapChainInfo;
  PrevSize: TPoint2px;
begin
  SwapChainInfo := FSwapChains[SwapChainIndex];
  if SwapChainInfo = nil then
    Exit(False);

  if (SwapChainInfo.Width = NewSize.X) and (SwapChainInfo.Height = NewSize.Y) then
    Exit(True);

  Reset;

  PrevSize.X := SwapChainInfo.Width;
  PrevSize.Y := SwapChainInfo.Height;

  SwapChainInfo.Width := NewSize.X;
  SwapChainInfo.Height := NewSize.Y;

  if Initialized then
  begin
    Result := ResizeSwapChain(SwapChainIndex, SwapChainInfo);
    if not Result then
    begin
      SwapChainInfo.Width := PrevSize.X;
      SwapChainInfo.Height := PrevSize.Y;
    end;
  end
  else
    Result:= True;
end;

{$ENDREGION}

end.
