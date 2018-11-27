unit PXL.Devices.GL.Win;
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
  Windows, PXL.TypeDef, PXL.Types, PXL.Devices, PXL.SwapChains, PXL.Types.GL;

type
  TGLDevice = class(TCustomSwapChainDevice)
  private type
    PGLContext = ^TGLContext;
    TGLContext = record
      Handle: TUntypedHandle;
      WinDC: HDC;
      Context: HGLRC;
    end;

    TGLContexts = class
    private
      FContexts: array of TGLContext;

      FSearchList: array of Integer;
      FSearchDirty: Boolean;

      function GetCount: Integer;
      function GetItem(const Index: Integer): PGLContext;
      procedure Release(const Index: Integer);

      procedure InitSearchList;
      procedure SearchListSwap(const Index1, Index2: Integer);
      function SearchListCompare(const Index1, Index2: Integer): Integer;
      function SearchListSplit(const Start, Stop: Integer): Integer;
      procedure SearchListSort(const Start, Stop: Integer);
      procedure UpdateSearchList;

      function ChooseAndSetPixelFormat(const WindowDC: HDC): Boolean;
      function InitWindow(const Index: Integer; const MainHandle: TUntypedHandle): Boolean;
    public
      destructor Destroy; override;

      function Insert(const Handle: TUntypedHandle): Integer;
      function IndexOf(const Handle: TUntypedHandle): Integer;

      procedure Remove(const Index: Integer);
      procedure Clear;

      function Activate(const SubHandle, MainHandle: TUntypedHandle): Boolean;
      function Flip(const WinHandle: TUntypedHandle): Boolean;

      property Count: Integer read GetCount;
      property Items[const Index: Integer]: PGLContext read GetItem; default;
    end;
  private
    FDeviceContext: TGLDeviceContext;

    FContexts: TGLContexts;
    FSwapChainInfo: PSwapChainInfo;

    procedure UpdateSwapInterval(const Value: Boolean);
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;

    function BeginScene(const SwapChainIndex: Integer = 0): Boolean; override;
    function EndScene: Boolean; override;

    property Contexts: TGLContexts read FContexts;
  end;

implementation

uses
{$IFDEF FPC}
  gl, glext;
{$ELSE}
  Winapi.OpenGL, Winapi.OpenGLext;
{$ENDIF}

{$REGION 'Global Functions'}

var
  wglSwapIntervalEXT: function(interval: GLint): Boolean; stdcall;
  wglGetSwapIntervalEXT: function: GLint; stdcall;

{$IFDEF FPC}
  ExtensionsLoaded: Boolean = False;
{$ENDIF}

{$ENDREGION}
{$REGION 'TGLDevice.TGLContexts'}

destructor TGLDevice.TGLContexts.Destroy;
begin
  Clear;
  inherited;
end;

function TGLDevice.TGLContexts.GetCount: Integer;
begin
  Result := Length(FContexts);
end;

function TGLDevice.TGLContexts.GetItem(const Index: Integer): PGLContext;
begin
  if (Index >= 0) and (Index < Length(FContexts)) then
    Result := @FContexts[Index]
  else
    Result := nil;
end;

procedure TGLDevice.TGLContexts.Release(const Index: Integer);
begin
  if (Index < 0) or (Index >= Length(FContexts)) then
    Exit;

  if FContexts[Index].Context <> 0 then
  begin
    if wglGetCurrentContext = FContexts[Index].Context then
      wglMakeCurrent(0, 0);

    wglDeleteContext(FContexts[Index].Context);
    FContexts[Index].Context := 0;
  end;

  if FContexts[Index].WinDC <> 0 then
  begin
    ReleaseDC(FContexts[Index].Handle, FContexts[Index].WinDC);
    FContexts[Index].WinDC := 0;
  end;
end;

procedure TGLDevice.TGLContexts.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FContexts)) then
    Exit;

  Release(Index);

  for I := Index to Length(FContexts) - 2 do
    FContexts[I] := FContexts[I + 1];

  SetLength(FContexts, Length(FContexts) - 1);
  FSearchDirty := True;
end;

procedure TGLDevice.TGLContexts.Clear;
var
  I: Integer;
begin
  for I := Length(FContexts) - 1 downto 0 do
    Release(I);

  SetLength(FContexts, 0);
  FSearchDirty := True;
end;

function TGLDevice.TGLContexts.Insert(const Handle: TUntypedHandle): Integer;
begin
  Result := Length(FContexts);
  SetLength(FContexts, Result + 1);

  FContexts[Result].Handle := Handle;
  FContexts[Result].WinDC := 0;
  FContexts[Result].Context := 0;

  FSearchDirty := True;
end;

procedure TGLDevice.TGLContexts.InitSearchList;
var
  I: Integer;
begin
  if Length(FSearchList) <> Length(FContexts) then
    SetLength(FSearchList, Length(FContexts));

  for I := 0 to Length(FSearchList) - 1 do
    FSearchList[I] := I;
end;

procedure TGLDevice.TGLContexts.SearchListSwap(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FSearchList[Index1];
  FSearchList[Index1] := FSearchList[Index2];
  FSearchList[Index2] := TempValue;
end;

function TGLDevice.TGLContexts.SearchListCompare(const Index1, Index2: Integer): Integer;
begin
  Result := 0;

  if FContexts[Index1].Handle < FContexts[Index2].Handle then
    Result := -1
  else if FContexts[Index1].Handle > FContexts[Index2].Handle then
    Result := 1;
end;

function TGLDevice.TGLContexts.SearchListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FSearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (SearchListCompare(FSearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (SearchListCompare(FSearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      SearchListSwap(Left, Right);
  end;

  SearchListSwap(Start, Right);
  Result := Right;
end;

procedure TGLDevice.TGLContexts.SearchListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := SearchListSplit(Start, Stop);
    SearchListSort(Start, SplitPt - 1);
    SearchListSort(SplitPt + 1, Stop);
  end;
end;

procedure TGLDevice.TGLContexts.UpdateSearchList;
begin
  InitSearchList;

  if Length(FSearchList) > 1 then
    SearchListSort(0, Length(FSearchList) - 1);

  FSearchDirty := False;
end;

function TGLDevice.TGLContexts.IndexOf(const Handle: TUntypedHandle): Integer;
var
  Left, Right, Pivot: Integer;
begin
  if FSearchDirty then
    UpdateSearchList;

  Left := 0;
  Right := Length(FSearchList) - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;

    if FContexts[FSearchList[Pivot]].Handle = Handle then
      Exit(FSearchList[Pivot]);

    if FContexts[FSearchList[Pivot]].Handle > Handle then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

function TGLDevice.TGLContexts.ChooseAndSetPixelFormat(const WindowDC: HDC): Boolean;
var
  Index: Integer;
  Desc: TPixelFormatDescriptor;
begin
  FillChar(Desc, SizeOf(TPixelFormatDescriptor), 0);

  Desc.nSize := SizeOf(TPixelFormatDescriptor);
  Desc.nVersion := 1;
  Desc.iPixelType := PFD_TYPE_RGBA;
  Desc.cColorBits := 32;
  Desc.cDepthBits := 24;
  Desc.cStencilBits := 8;
  Desc.iLayerType := PFD_MAIN_PLANE;
  Desc.dwFlags := PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or PFD_DOUBLEBUFFER;

  Index := ChoosePixelFormat(WindowDC, @Desc);
  if Index = 0 then
    Exit(False);

  Result := SetPixelFormat(WindowDC, Index, nil);
end;

function TGLDevice.TGLContexts.InitWindow(const Index: Integer; const MainHandle: TUntypedHandle): Boolean;
var
  MainIndex: Integer;
begin
  FContexts[Index].WinDC := GetDC(FContexts[Index].Handle);
  if FContexts[Index].WinDC = 0 then
    Exit(False);

  if not ChooseAndSetPixelFormat(FContexts[Index].WinDC) then
    Exit(False);

  FContexts[Index].Context := wglCreateContext(FContexts[Index].WinDC);
  if  FContexts[Index].Context = 0 then
    Exit(False);

  if (MainHandle <> 0) and (MainHandle <> FContexts[Index].Handle) then
  begin
    MainIndex := IndexOf(MainHandle);
    if MainIndex <> -1 then
      Result := wglShareLists(FContexts[MainIndex].Context, FContexts[Index].Context)
    else
      Result := True;
  end
  else
    Result := True;
end;

function TGLDevice.TGLContexts.Activate(const SubHandle, MainHandle: TUntypedHandle): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(SubHandle);
  if Index = -1 then
  begin
    Index := Insert(SubHandle);

    if not InitWindow(Index, MainHandle) then
    begin
      Remove(Index);
      Exit(False);
    end;
  end;

  Result := wglMakeCurrent(FContexts[Index].WinDC, FContexts[Index].Context);
end;

function TGLDevice.TGLContexts.Flip(const WinHandle: TUntypedHandle): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(WinHandle);

  if Index <> -1 then
    Result := SwapBuffers(FContexts[Index].WinDC)
  else
    Result := False;
end;

{$ENDREGION}
{$REGION 'TGLDevice'}

constructor TGLDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FDeviceContext := TGLDeviceContext.Create(Self);

  FContexts := TGLContexts.Create;
  FTechnology := TDeviceTechnology.OpenGL;
end;

destructor TGLDevice.Destroy;
begin
  inherited;

  FContexts.Free;
  FDeviceContext.Free;
end;

function TGLDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FDeviceContext;
end;

function TGLDevice.InitDevice: Boolean;
var
  SwapChainInfo: PSwapChainInfo;
begin
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

  if not FContexts.Activate(SwapChainInfo.WindowHandle, 0) then
    Exit(False);

{$IFDEF FPC}
  if not ExtensionsLoaded then
  begin
    Result := False;

    if FDeviceContext.Extensions.MajorVersion >= 4 then
      Result := Load_GL_version_4_0
    else if FDeviceContext.Extensions.MajorVersion >= 3 then
    begin
      if FDeviceContext.Extensions.MinorVersion >= 3 then
        Result := Load_GL_version_3_3
      else if FDeviceContext.Extensions.MinorVersion >= 2 then
        Result := Load_GL_version_3_2
      else if FDeviceContext.Extensions.MinorVersion >= 1 then
        Result := Load_GL_version_3_1
      else
        Result := Load_GL_version_3_0;
    end
    else if FDeviceContext.Extensions.MajorVersion >= 2 then
    begin
      if FDeviceContext.Extensions.MinorVersion >= 1 then
        Result := Load_GL_version_2_1
      else
        Result := Load_GL_version_2_0;
    end
    else if FDeviceContext.Extensions.MajorVersion >= 1 then
    begin
      if FDeviceContext.Extensions.MinorVersion >= 5 then
        Result := Load_GL_version_1_5;
    end;

    if not Result then
      Exit;

    if FDeviceContext.Extensions.EXT_framebuffer_object and (not Load_GL_EXT_framebuffer_object) then
      Exit(False);

    ExtensionsLoaded := True;
  end;

{$ELSE}
  InitOpenGLext;
{$ENDIF}

  if FDeviceContext.Extensions.EXT_swap_control then
  begin
    if @wglSwapIntervalEXT = nil then
      wglSwapIntervalEXT := wglGetProcAddress('wglSwapIntervalEXT');

    if @wglGetSwapIntervalEXT = nil then
      wglGetSwapIntervalEXT := wglGetProcAddress('wglGetSwapIntervalEXT');
  end;

  FTechVersion := Cardinal(FDeviceContext.Extensions.MajorVersion) shl 8;

  FTechFeatureVersion :=
    (Cardinal(FDeviceContext.Extensions.MajorVersion) shl 8) or
    (Cardinal(FDeviceContext.Extensions.MinorVersion and $0F) shl 4);

  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  Result := glGetError = GL_NO_ERROR;
end;

procedure TGLDevice.DoneDevice;
begin
  FContexts.Clear;
end;

function TGLDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
  const StencilValue: Cardinal): Boolean;
var
  Flags: Cardinal;
begin
  if ClearTypes = [] then
    Exit(False);

  Flags := 0;

  if TClearType.Color in ClearTypes then
  begin
    glClearColor(TIntColorRec(ColorValue).Red / 255.0, TIntColorRec(ColorValue).Green / 255.0,
      TIntColorRec(ColorValue).Blue / 255.0, TIntColorRec(ColorValue).Alpha / 255.0);
    Flags := Flags or GL_COLOR_BUFFER_BIT;
  end;

  if TClearType.Depth in ClearTypes then
  begin
    glClearDepth(DepthValue);
    Flags := Flags or GL_DEPTH_BUFFER_BIT;
  end;

  if TClearType.Stencil in ClearTypes then
  begin
    glClearStencil(StencilValue);
    Flags := Flags or GL_STENCIL_BUFFER_BIT;
  end;

  glClear(Flags);

  Result := glGetError = GL_NO_ERROR;
end;

procedure TGLDevice.UpdateSwapInterval(const Value: Boolean);
var
  Interval: Integer;
begin
  if FDeviceContext.Extensions.EXT_swap_control then
  begin
    Interval := wglGetSwapIntervalEXT;

    if Value and (Interval <> 1) then
      wglSwapIntervalEXT(1)
    else if (not Value) and (Interval <> 0) then
      wglSwapIntervalEXT(0);
  end;
end;

function TGLDevice.BeginScene(const SwapChainIndex: Integer): Boolean;
var
  MainSwapChainInfo: PSwapChainInfo;
begin
  FSwapChainInfo := SwapChains[SwapChainIndex];
  if FSwapChainInfo = nil then
    Exit(False);

  MainSwapChainInfo := nil;

  if SwapChainIndex <> 0 then
    MainSwapChainInfo := SwapChains[0];

  if MainSwapChainInfo <> nil then
    Result := FContexts.Activate(FSwapChainInfo.WindowHandle, MainSwapChainInfo.WindowHandle)
  else
    Result := FContexts.Activate(FSwapChainInfo.WindowHandle, 0);

  if not Result then
    Exit(False);

  UpdateSwapInterval(FSwapChainInfo.VSync);

  glViewport(0, 0, FSwapChainInfo.Width, FSwapChainInfo.Height);
  glDisable(GL_SCISSOR_TEST);

  Result := glGetError = GL_NO_ERROR;
end;

function TGLDevice.EndScene: Boolean;
begin
  if FSwapChainInfo = nil then
    Exit(False);

  Result := FContexts.Flip(FSwapChainInfo.WindowHandle);
  FSwapChainInfo := nil;
end;

{$ENDREGION}

end.
