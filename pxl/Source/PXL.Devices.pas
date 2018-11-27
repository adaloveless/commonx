unit PXL.Devices;
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
{< Hardware device specification that handles creation of rendering buffers, different technologies such as Direct3D
   and OpenGL along with other administrative tasks. }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.TypeDef, PXL.Types, PXL.Events, PXL.ImageFormats;

type
  TCustomDevice = class;
  TCustomDeviceContext = class;

  { Type of graphics technology used in device. }
  TDeviceTechnology = (
    { The technology has not yet been established. }
    Unknown,

    { Microsoft Direct3D technology. }
    Direct3D,

    { OpenGL by Khronos Group. }
    OpenGL,

    { OpenGL ES by Khronos Group. }
    OpenGL_ES,

    { Software rasterizer. }
    Software,

    { Private proprietary technology. }
    Proprietary);

  { Type of graphics technology features provided by device. }
  TTechnologyFeature = (
    { Hardware-accelerated rendering. }
    Hardware,

    { Software-rasterized rendering. }
    Software);

  { Set of different graphics technology features provided by device. }
  TTechnologyFeatures = set of TTechnologyFeature;

  { Type of surface should be cleared. }
  TClearType = (
    { Color buffer. }
    Color,

    { Depth buffer. }
    Depth,

    { Stencil buffer. }
    Stencil);

  { Set of flags that define what type of surfaces should be cleared. }
  TClearTypes = set of TClearType;

  { Abstract device provider class that is responsible of creating resources that are specific to one particular
    technology and/or API. }
  TCustomDeviceProvider = class abstract
  private
    FImageFormatManager: TCustomImageFormatManager;
  public
    { @exclude } constructor Create(const AImageFormatManager: TCustomImageFormatManager);

    { Creates new instance of device. }
    function CreateDevice: TCustomDevice; virtual; abstract;

    { Provides a reference to image format manager responsible of loading and saving different formats in the
      rendering ecosystem. }
    property ImageFormatManager: TCustomImageFormatManager read FImageFormatManager;
  end;

  { Abstract writer class that is given by device context upon creation and allows writing to context's internal
    values. }
  TCustomDeviceContextWriter = class abstract
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TCustomDeviceContext;
  public
    { @exclude } constructor Create(const AContext: TCustomDeviceContext); virtual;

    { Reference to device context instance that created this writer. }
    property Context: TCustomDeviceContext read FContext;
  end;

  { Abstract device context class that contains important device specific references. }
  TCustomDeviceContext = class abstract
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;
  public
    { @exclude } constructor Create(const ADevice: TCustomDevice);

    { Reference to device instance that created this context. }
    property Device: TCustomDevice read FDevice;
  end;

  { Hardware device wrapper that handles communication between application and the video card. The device must be
    created from the factory and is one of the first objects that needs to be initialized before working with any
    other components. }
  TCustomDevice = class abstract
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FProvider: TCustomDeviceProvider;
    FOnRestore: TEventNotifier;
    FOnRelease: TEventNotifier;

    function GetImageFormatManager: TCustomImageFormatManager;
  protected
    { Type of technology that is currently being used. }
    FTechnology: TDeviceTechnology;

    { The version of current technology that is currently being used. }
    FTechVersion: Integer;

    { The feature level version of current technology that is currently being used. }
    FTechFeatureVersion: Integer;

    { Technology features are currently being provided by the device. }
    FTechFeatures: TTechnologyFeatures;

    { This method should be implemented by derived classes to provide important device specific references. }
    function GetDeviceContext: TCustomDeviceContext; virtual;
  public
    { @exclude } constructor Create(const AProvider: TCustomDeviceProvider);
    { @exclude } destructor Destroy; override;

    { Clears the currently active rendering surface. }
    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; virtual; abstract;

    { Parent provider object that created this device instance. }
    property Provider: TCustomDeviceProvider read FProvider;

    { Device context that contains important device specific references. }
    property Context: TCustomDeviceContext read GetDeviceContext;

    { Indicates the type of technology that is currently being used. }
    property Technology: TDeviceTechnology read FTechnology;

    { Indicates the version of current technology that is currently being used. The values are specified in hexadecimal
      format. That is, a value of $100 indicates version 1.0, while a value of $247 would indicate version 2.4.7. This
      value is used in combination with @link(Technology), so if @code(Technology) is set to
      @italic(TDeviceTechnology.Direct3D) and this value is set to $A10, it means that @italic(Direct3D 10.1) is
      being used.  }
    property TechVersion: Integer read FTechVersion;

    { Indicates the feature level version of current technology that is currently being used. The difference between
      this parameter and @link(TechVersion) is that the second parameter indicates type of technology being used (for
      example, DirectX 3D), while this one indicates the level of features available (for example, Direct3D 9.0c).
      The values here are specified in hexadecimal format. That is, a value of $213 would indicate version 2.1.3. }
    property TechFeatureVersion: Integer read FTechFeatureVersion;

    { Indicates what technology features are currently being provided by the device. }
    property TechFeatures: TTechnologyFeatures read FTechFeatures;

    { Event notifier that signals when the device has been put into "restored" state, where all volatile resources that
      had to be freed previously during "release" state can now be recreated and restored. }
    property OnRestore: TEventNotifier read FOnRestore;

    { Event notifier that signals when the device has been put into "released" state and all volatile resources are no
      longer valid and should be freed at earliest convenience. }
    property OnRelease: TEventNotifier read FOnRelease;

    { A shortcut to image format manager associated with this device's provider. }
    property ImageFormatManager: TCustomImageFormatManager read GetImageFormatManager;
  end;

  { A more elaborated hardware device wrapper that can have working state so it needs to be initialized before any
    rendering can take place. }
  TCustomStateDevice = class(TCustomDevice)
  protected
    { Current device initialization status. }
    FInitialized: Boolean;

    { This method should be implemented by derived classes to initialize implementation specific resources. }
    function InitDevice: Boolean; virtual;

    { This method should be implemented by derived classes to release implementation specific resources. }
    procedure DoneDevice; virtual;
  public
    { @exclude } constructor Create(const AProvider: TCustomDeviceProvider);
    { @exclude } destructor Destroy; override;

    { Initializes the device and puts it into working state.  }
    function Initialize: Boolean; virtual;

    { Finalizes the device, releasing all its resources and handles. }
    procedure Finalize; virtual;

    { Indicates whether the device has been initialized and is now in working state. }
    property Initialized: Boolean read FInitialized;
  end;

{ Returns a readable text string with the name of the specified device technology. }
function DeviceTechnologyToString(const Technology: TDeviceTechnology): StdString;

{ Converts device version value originally specified in hexadecimal format (e.g. $324) into a readable text string
  describing that version (e.g. "3.2.4"). If @italic(CompactForm) form parameter is set to @True, the version text is
  reduced for trailing zeros, so a text like "3.0" becomes just "3". }
function DeviceVersionToString(const Value: Integer; const CompactForm: Boolean = False): StdString;

{ Returns a readable text string that describes the current device's technology, technology version and feature level
  version. This information can be used for informative purposes. }
function GetFullDeviceTechString(const Device: TCustomDevice): StdString;

implementation

uses
  SysUtils;

{$REGION 'Global Functions'}

function DeviceTechnologyToString(const Technology: TDeviceTechnology): StdString;
begin
  case Technology of
    TDeviceTechnology.Direct3D:
      Result := 'Direct3D';

    TDeviceTechnology.OpenGL:
      Result := 'OpenGL';

    TDeviceTechnology.OpenGL_ES:
      Result := 'OpenGL ES';

    TDeviceTechnology.Software:
      Result := 'Software';

    TDeviceTechnology.Proprietary:
      Result := 'Proprietary';

    else
      Result := 'Unknown';
  end;
end;

function DeviceVersionToString(const Value: Integer; const CompactForm: Boolean = False): StdString;
var
  LeastSet: Boolean;
begin
  if Value <= 0 then
    Exit('0.0');

  Result := '';

  if Value and $00F > 0 then
  begin
    Result := '.' + IntToStr(Value and $00F);
    LeastSet := True;
  end
  else
    LeastSet := False;

  if ((not CompactForm) or LeastSet) or (Value and $0F0 > 0) then
    Result := '.' + IntToStr((Value and $0F0) shr 4) + Result;

  Result := IntToStr(Value shr 8) + Result;
end;

function GetFullDeviceTechString(const Device: TCustomDevice): StdString;
begin
  if (Device = nil) or (Device.Technology = TDeviceTechnology.Unknown) then
    Exit('Unidentified device technology.');

  Result := DeviceTechnologyToString(Device.Technology);

  if (Device.TechVersion > 0) and (Device.TechVersion <> $100) then
    Result := Result + #32 + DeviceVersionToString(Device.TechVersion, True);

  if (Device.Technology = TDeviceTechnology.Direct3D) and (Device.TechVersion = $900) then
  begin // DirectX 9 specific.
    if Device.TechFeatureVersion = $901 then
      Result := Result + ' Ex (Vista)'
    else
      Result := Result + ' (XP compatibility)';
  end
  else
  begin // General feature levels.
    if Device.TechFeatureVersion > 0 then
      Result := Result + ' (feature level ' + DeviceVersionToString(Device.TechFeatureVersion) + ')';
  end;

  if TTechnologyFeature.Software in Device.TechFeatures then
    Result := Result + ' [SW]';

  if TTechnologyFeature.Hardware in Device.TechFeatures then
    Result := Result + ' [HW]';
end;

{$ENDREGION}
{$REGION 'TCustomDeviceProvider'}

constructor TCustomDeviceProvider.Create(const AImageFormatManager: TCustomImageFormatManager);
begin
  inherited Create;

  FImageFormatManager := AImageFormatManager;
end;

{$ENDREGION}
{$REGION 'TCustomDeviceContextWriter'}

constructor TCustomDeviceContextWriter.Create(const AContext: TCustomDeviceContext);
begin
  inherited Create;

  FContext := AContext;
end;

{$ENDREGION}
{$REGION 'TCustomDeviceContext'}

constructor TCustomDeviceContext.Create(const ADevice: TCustomDevice);
begin
  inherited Create;

  FDevice := ADevice;
end;

{$ENDREGION}
{$REGION 'TCustomDevice'}

constructor TCustomDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited Create;

  FProvider := AProvider;

  FOnRestore := TEventNotifier.Create;
  FOnRelease := TEventNotifier.Create;

  Increment_PXL_ClassInstances;
end;

destructor TCustomDevice.Destroy;
begin
  try
    FOnRelease.Free;
    FOnRestore.Free;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TCustomDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := nil;
end;

function TCustomDevice.GetImageFormatManager: TCustomImageFormatManager;
begin
  if FProvider <> nil then
    Result := FProvider.ImageFormatManager
  else
    Result := nil;
end;

{$ENDREGION}
{$REGION 'TCustomStateDevice'}

constructor TCustomStateDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FInitialized := False;
end;

destructor TCustomStateDevice.Destroy;
begin
  if FInitialized then
    Finalize;

  inherited;
end;

function TCustomStateDevice.InitDevice: Boolean;
begin
  Result := True;
end;

procedure TCustomStateDevice.DoneDevice;
begin
end;

function TCustomStateDevice.Initialize: Boolean;
begin
  if FInitialized then
    Exit(False);

  Result := InitDevice;

  if Result then
    FInitialized := True;
end;

procedure TCustomStateDevice.Finalize;
begin
  if FInitialized then
  begin
    DoneDevice;
    FInitialized := False;
  end;
end;

{$ENDREGION}

end.
