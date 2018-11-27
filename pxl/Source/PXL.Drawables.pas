unit PXL.Drawables;
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
{< Container classes that facilitate storage, usage and handling of drawable textures. }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.TypeDef, PXL.Types, PXL.Devices, PXL.Textures;

type
  { List of drawable textures that can be rendered into but also can be used as normal textures. }
  TDrawableTextures = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;
    FTextures: array of TCustomDrawableTexture;

    DeviceRestoreHandle: Cardinal;
    DeviceReleaseHandle: Cardinal;

    function GetCount: Integer;
    function GetTexture(const Index: Integer): TCustomDrawableTexture;

    procedure OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
    procedure OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);
  public
    { @exclude } constructor Create(const ADevice: TCustomDevice);
    { @exclude } destructor Destroy; override;

    { Inserts a new drawable texture to the list without initializing it. }
    function Insert: Integer;

    { Returns the index of existing drawable texture in the list. If the given texture is not found in the list,
      the returned value is -1. }
    function IndexOf(const Texture: TCustomDrawableTexture): Integer; overload;

    { Removes texture at the given index from the list, shifting all further textures by one. Index must be specified
      in range of [0..(Count - 1)]. If the specified index is outside of valid range, this method does nothing. }
    procedure Remove(const Index: Integer);

    { Adds one or more drawable textures to the end of the list and initializes them. If the method succeeds, the
      index to first added element is returned; if the method fails, -1 is returned.
        @param(AddCount The number of drawable textures to add.)
        @param(Width The width of added drawable textures.)
        @param(Height The height of added drawable textures.)
        @param(PixelFormat The pixel format to be used in added drawable textures.)
        @param(DepthStencil Determines whether to create depth-stencil buffer in the added drawable textures.)
        @param(Mipmapping Determines whether to use Mip-Mapping in added drawable textures.)
        @param(Multisamples Indicates the number of samples to use for antialiasing in drawable textures.
        This parameter is only supported in latest DX10+ providers.) }
    function Add(const AddCount, Width, Height: Integer; const PixelFormat: TPixelFormat;
      const DepthStencil: TDepthStencil = TDepthStencil.None; const MipMapping: Boolean = False;
      const Multisamples: Integer = 0): Integer; overload;

    { Adds one or more drawable textures to the end of the list and initializes them. If the method succeeds, the
      index to first added element is returned; if the method fails, -1 is returned.
        @param(AddCount The number of drawable textures to add.)
        @param(Size The size of added drawable textures.)
        @param(PixelFormat The pixel format to be used in added drawable textures.)
        @param(DepthStencil Determines whether to create depth-stencil buffer in the added drawable textures.)
        @param(Mipmapping Determines whether to use Mip-Mapping in added drawable textures.)
        @param(Multisamples Indicates the number of samples to use for antialiasing in drawable textures.
        This parameter is only supported in latest DX10+ providers.) }
    function Add(const AddCount: Integer; const Size: TPoint2px; const PixelFormat: TPixelFormat;
      const DepthStencil: TDepthStencil = TDepthStencil.None; const MipMapping: Boolean = False;
      const Multisamples: Integer = 0): Integer; overload;

    { Removes all existing drawable textures from the list. }
    procedure Clear;

    { Reference to parent device. }
    property Device: TCustomDevice read FDevice;

    { The number of drawable textures in the list. }
    property Count: Integer read GetCount;

    { Provides access to individual drawable textures in the list by using the index in range of [0..(Count - 1)].
      If the specified index is outside of valid range, @nil is returned. }
    property Texture[const Index: Integer]: TCustomDrawableTexture read GetTexture; default;
  end;

implementation

uses
  PXL.Consts, PXL.Providers, PXL.Logs;

constructor TDrawableTextures.Create(const ADevice: TCustomDevice);
begin
  inherited Create;

  Inc(PXL_ClassInstances);

  FDevice := ADevice;

  if FDevice <> nil then
  begin
    DeviceRestoreHandle := FDevice.OnRestore.Subscribe(OnDeviceRestore);
    DeviceReleaseHandle := FDevice.OnRelease.Subscribe(OnDeviceRelease);
  end
  else
  begin
    DeviceRestoreHandle := 0;
    DeviceReleaseHandle := 0;
  end;
end;

destructor TDrawableTextures.Destroy;
begin
  try
    if FDevice <> nil then
    begin
      if DeviceReleaseHandle <> 0 then
        FDevice.OnRelease.Unsubscribe(DeviceReleaseHandle);

      if DeviceRestoreHandle <> 0 then
        FDevice.OnRestore.Unsubscribe(DeviceRestoreHandle);
    end
    else
    begin
      DeviceReleaseHandle := 0;
      DeviceRestoreHandle := 0;
    end;

    Clear;
    FDevice := nil;
  finally
    Dec(PXL_ClassInstances);
  end;

  inherited;
end;

function TDrawableTextures.GetCount: Integer;
begin
  Result := Length(FTextures);
end;

function TDrawableTextures.GetTexture(const Index: Integer): TCustomDrawableTexture;
begin
  if (Index >= 0) and (Index < Length(FTextures)) then
    Result := FTextures[Index]
  else
    Result := nil;
end;

function TDrawableTextures.Insert: Integer;
var
  Texture: TCustomDrawableTexture;
  Index: Integer;
begin
  if (FDevice = nil) or (not (FDevice.Provider is TGraphicsDeviceProvider)) then
    Exit(-1);

  Texture := TGraphicsDeviceProvider(FDevice.Provider).CreateDrawableTexture(FDevice, False);
  if Texture = nil then
    Exit(-1);

  Index := Length(FTextures);
  SetLength(FTextures, Index + 1);

  FTextures[Index] := Texture;
  Result := Index;
end;

function TDrawableTextures.IndexOf(const Texture: TCustomDrawableTexture): Integer;
var
  I: Integer;
begin
  if Texture = nil then
    Exit(-1);

  for I := 0 to Length(FTextures) - 1 do
    if FTextures[I] = Texture then
      Exit(I);

  Result := -1;
end;

procedure TDrawableTextures.Clear;
var
  I: Integer;
begin
  for I := 0 to Length(FTextures) - 1 do
    FTextures[I].Free;

  SetLength(FTextures, 0);
end;

procedure TDrawableTextures.OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
var
  I: Integer;
  FailedToRestore: Boolean;
begin
  FailedToRestore := False;

  for I := 0 to Length(FTextures) - 1 do
    if (FTextures[I] <> nil) and (not FTextures[I].DeviceRestore) then
      FailedToRestore := True;

  if FailedToRestore then
    LogText(SCannotRestoreTextures, TLogType.Error);
end;

procedure TDrawableTextures.OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);
var
  I: Integer;
begin
  for I := 0 to Length(FTextures) - 1 do
    if FTextures[I] <> nil then
      FTextures[I].DeviceRelease;
end;

procedure TDrawableTextures.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FTextures)) then
    Exit;

  FTextures[Index].Free;

  for I := Index to Length(FTextures) - 2 do
    FTextures[I] := FTextures[I + 1];

  SetLength(FTextures, Length(FTextures) - 1);
end;

function TDrawableTextures.Add(const AddCount, Width, Height: Integer; const PixelFormat: TPixelFormat;
  const DepthStencil: TDepthStencil; const MipMapping: Boolean; const Multisamples: Integer): Integer;
var
  I, Index, FirstIndex: Integer;
  CreationFailed: Boolean;
begin
  FirstIndex := -1;
  CreationFailed := False;

  for I := 0 to AddCount - 1 do
  begin
    Index := Insert;
    if Index = -1 then
    begin
      CreationFailed := True;
      Break;
    end;

    if FirstIndex = - 1 then
      FirstIndex := Index;

    FTextures[Index].Width := Width;
    FTextures[Index].Height := Height;
    FTextures[Index].PixelFormat := PixelFormat;
    FTextures[Index].DepthStencil := DepthStencil;
    FTextures[Index].Mipmapping := MipMapping;
    FTextures[Index].Multisamples := Multisamples;

    if not FTextures[Index].Initialize then
    begin
      CreationFailed := True;
      Break;
    end;
  end;

  if CreationFailed then
  begin
    if FirstIndex <> -1 then
      for I := Length(FTextures) - 1 downto FirstIndex do
        Remove(I);

    Exit(-1);
  end;

  Result := FirstIndex;
end;

function TDrawableTextures.Add(const AddCount: Integer; const Size: TPoint2px; const PixelFormat: TPixelFormat;
  const DepthStencil: TDepthStencil; const MipMapping: Boolean; const Multisamples: Integer): Integer;
begin
  Result := Add(AddCount, Size.X, Size.Y, PixelFormat, DepthStencil, MipMapping, Multisamples);
end;

end.
