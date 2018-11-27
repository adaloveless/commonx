unit PXL.Displays.Types;
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

{ This directive controls the inclusion of SRT canvas for rendering on the display. If enabled, certain implementations
  of TDisplayProvider, TDisplayDevice and TDisplayCanvas are declared that use software renderer for drawing primitives
  on the display. }
{$DEFINE ImplementSRTComponents}

{ This directive controls whether TCustomDisplay owns and creates SRT components by itself. If enabled, it will create
  instances of commonly used components such as Provider, Device and Canvas. In addition, it will also create Images
  and Fonts on demand. }
{$DEFINE CreateSRTComponents}

{ Comment this out to prevent the component from releasing the pins in destructor. }
{$DEFINE DISPLAY_RESET_PINS_AFTER_DONE}

uses
{$IFDEF ImplementSRTComponents}
  PXL.ImageFormats, PXL.Devices, PXL.Textures, PXL.Canvas, PXL.Providers, PXL.Images, PXL.Fonts, PXL.Types.SRT,
  PXL.Canvas.SRT,
{$ENDIF}

  PXL.TypeDef, PXL.Types, PXL.Events, PXL.Surfaces, PXL.Boards.Types;

const
  PinNumberUnused = -1;

type
{$IFDEF ImplementSRTComponents}
  TCustomDisplay = class;

  TDisplayProvider = class(TGraphicsDeviceProvider)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDisplay: TCustomDisplay;
  public
    constructor Create(const AImageFormatManager: TCustomImageFormatManager; const ADisplay: TCustomDisplay);

    function CreateDevice: TCustomDevice; override;
    function CreateCanvas(const Device: TCustomDevice): TCustomCanvas; override;
    function CreateLockableTexture(const Device: TCustomDevice;
      const AutoSubscribe: Boolean): TCustomLockableTexture; override;
    function CreateDrawableTexture(const Device: TCustomDevice;
      const AutoSubscribe: Boolean): TCustomDrawableTexture; override;

    property Display: TCustomDisplay read FDisplay;
  end;

  TDisplayDevice = class(TCustomDevice)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDisplay: TCustomDisplay;
    FContext: TSRTDeviceContext;
    FContextWriter: TSRTDeviceContextWriter;
    FOrientationChangedHandle: Cardinal;

    procedure OnOrientationChanged(const Sender: TObject; const EventData, UserData: Pointer);
  protected
    function GetDeviceContext: TCustomDeviceContext; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider; const ADisplay: TCustomDisplay);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
      const StencilValue: Cardinal): Boolean; override;

    property Display: TCustomDisplay read FDisplay;
  end;

  TDisplayCanvas = class(TSRTCanvas)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDisplay: TCustomDisplay;
    FOrientationChangedHandle: Cardinal;

    procedure OnOrientationChanged(const Sender: TObject; const EventData, UserData: Pointer);
  protected
    function BeginDraw: Boolean; override;
    procedure EndDraw; override;
  public
    constructor Create(const ADevice: TCustomDevice; const ADisplay: TCustomDisplay);
    destructor Destroy; override;

    property Display: TCustomDisplay read FDisplay;
  end;

  {$IFDEF CreateSRTComponents}
    {$DEFINE DisplaySRTComponents}
  {$ENDIF}
{$ENDIF}

  TCustomDisplay = class(TConceptualPixelSurface)
  public type
    TOrientation = (Landscape, Portrait, InverseLandscape, InversePortrait);
  private const
    SDisplayCommandWrite = 'Failed to write display command <0x%x>.';
    SDisplayCommandBytesWrite = 'Failed to write <%d> bytes of display commands.';
    SDisplayDataWrite = 'Failed to write display data <0x%x>.';
    SDisplayDataBytesWrite = 'Failed to write <%d> bytes of display data.';
  private
    FOnOrientationChanged: TEventNotifier;
    FAdjustedOrientation: TOrientation;

  {$IFDEF DisplaySRTComponents}
    FImageFormatManager: TImageFormatManager;
    FImageFormatHandler: TCustomImageFormatHandler;
    FProvider: TDisplayProvider;
    FDevice: TDisplayDevice;
    FCanvas: TDisplayCanvas;
    FImages: TAtlasImages;
    FFonts: TBitmapFonts;

    function GetImages: TAtlasImages;
    function GetFonts: TBitmapFonts;
  {$ENDIF}

    procedure SetLogicalOrientation(const Value: TOrientation);
    procedure AdjustPosition(var X, Y: Integer); inline;
  protected
    FPhysicalOrientation: TOrientation;
    FPhysicalSize: TPoint2px;

    FLogicalOrientation: TOrientation;
    FLogicalSize: TPoint2px;

    FScreenBufferSize: Integer;
    FScreenBuffer: Pointer;

    function GetPixel(X, Y: Integer): TIntColor; override;
    procedure SetPixel(X, Y: Integer; const Color: TIntColor); override;

    function GetPixelUnsafe(X, Y: Integer): TIntColor; override;
    procedure SetPixelUnsafe(X, Y: Integer; const Color: TIntColor); override;

    procedure Reset; virtual; abstract;
    procedure InitSequence; virtual; abstract;
    procedure PresentBuffer(const Rect: TIntRect); virtual; abstract;

    function ReadPixel(const X, Y: Integer): TIntColor; virtual; abstract;
    procedure WritePixel(const X, Y: Integer; const Color: TIntColor); virtual; abstract;

    function GetScanline(const Index: Integer): Pointer; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize; virtual;
    procedure Present(const Rect: TIntRect); overload;
    procedure Present; overload;
    procedure Clear; virtual;

    property PhysicalOrientation: TOrientation read FPhysicalOrientation;
    property PhysicalSize: TPoint2px read FPhysicalSize;

    property LogicalOrientation: TOrientation read FLogicalOrientation write SetLogicalOrientation;
    property LogicalSize: TPoint2px read FLogicalSize;

    property ScreenBuffer: Pointer read FScreenBuffer;
    property ScreenBufferSize: Integer read FScreenBufferSize;

    property Scanline[const Index: Integer]: Pointer read GetScanline;

    property OnOrientationChanged: TEventNotifier read FOnOrientationChanged;

  {$IFDEF DisplaySRTComponents}
    property Provider: TDisplayProvider read FProvider;
    property Device: TDisplayDevice read FDevice;
    property Canvas: TDisplayCanvas read FCanvas;
    property Images: TAtlasImages read GetImages;
    property Fonts: TBitmapFonts read GetFonts;
  {$ENDIF}
  end;

  TCustomDrivenDisplay = class(TCustomDisplay)
  protected
    FGPIO: TCustomGPIO;
    FDataPort: TCustomDataPort;
    FPinDC: Integer;
    FPinRST: Integer;
  protected
    procedure Reset; override;

    procedure WriteCommand(const Value: Byte); overload; virtual;
    procedure WriteCommand(const Values: array of Byte); overload; virtual;
    procedure WriteData(const Value: Byte); overload; virtual;
    procedure WriteData(const Values: array of Byte); overload; virtual;
  public
    constructor Create(const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort;
      const APinDC: Integer; const APinRST: Integer = PinNumberUnused);
    destructor Destroy; override;

    property GPIO: TCustomGPIO read FGPIO;
    property PinDC: Integer read FPinDC;
    property PinRST: Integer read FPinRST;
  end;

  TCustomDrivenDualDisplay = class(TCustomDrivenDisplay)
  protected const
    { I2C codes that differentiate command and data values. }
    DisplayCommandID = $00;
    DisplayDataID = $40;
  private type
    TI2CBlock = packed record
      Control: Byte;
      Data: Byte;
    end;
  protected
    FAddress: Integer;

    procedure WriteCommand(const Value: Byte); override;
    procedure WriteCommand(const Values: array of Byte); override;
    procedure WriteData(const Value: Byte); override;
    procedure WriteData(const Values: array of Byte); override;
  public
    constructor Create(const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort;
      const APinDC: Integer; const APinRST: Integer = PinNumberUnused; const AAddress: Integer = PinNumberUnused);

    property Address: Integer read FAddress;
  end;

implementation

uses
{$IFNDEF DISPLAY_SILENT}
  PXL.Logs,
{$ENDIF}

{$IFDEF ImplementSRTComponents}
  PXL.Textures.SRT, PXL.ImageFormats.FCL,
{$ENDIF}

  SysUtils;

{$IFDEF ImplementSRTComponents}
{$REGION 'TDisplayProvider'}

constructor TDisplayProvider.Create(const AImageFormatManager: TCustomImageFormatManager;
  const ADisplay: TCustomDisplay);
begin
  inherited Create(AImageFormatManager);
  FDisplay := ADisplay;
end;

function TDisplayProvider.CreateDevice: TCustomDevice;
begin
  Result := TDisplayDevice.Create(Self, FDisplay);
end;

function TDisplayProvider.CreateCanvas(const Device: TCustomDevice): TCustomCanvas;
begin
  Result := TSRTCanvas.Create(Device);
end;

function TDisplayProvider.CreateLockableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomLockableTexture;
begin
  Result := TSRTLockableTexture.Create(Device, AutoSubscribe);
end;

function TDisplayProvider.CreateDrawableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomDrawableTexture;
begin
  Result := nil;
end;

{$ENDREGION}
{$REGION 'TDisplayDevice'}

constructor TDisplayDevice.Create(const AProvider: TCustomDeviceProvider; const ADisplay: TCustomDisplay);
begin
  inherited Create(AProvider);

  FDisplay := ADisplay;
  FContext := TSRTDeviceContext.Create(Self, FContextWriter);

  FTechnology := TDeviceTechnology.Software;
  FTechVersion := $100;

  FContextWriter.Surface := FDisplay;

  if FDisplay <> nil then
  begin
    FContextWriter.SurfaceSize := FDisplay.LogicalSize;
    FOrientationChangedHandle := FDisplay.OnOrientationChanged.Subscribe(OnOrientationChanged);
  end;
end;

destructor TDisplayDevice.Destroy;
begin
  if (FDisplay <> nil) and (FOrientationChangedHandle <> 0) then
    FDisplay.OnOrientationChanged.Unsubscribe(FOrientationChangedHandle);

  FContextWriter.Free;
  FContext.Free;

  inherited;
end;

function TDisplayDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result:= FContext;
end;

function TDisplayDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor;
  const DepthValue: Single; const StencilValue: Cardinal): Boolean;
begin
  // This function is unsupported.
  Result := False;
end;

procedure TDisplayDevice.OnOrientationChanged(const Sender: TObject; const EventData, UserData: Pointer);
begin
  if Sender is TCustomDisplay then
    FContextWriter.SurfaceSize := TCustomDisplay(Sender).LogicalSize;
end;

{$ENDREGION}
{$REGION 'TDisplayCanvas'}

constructor TDisplayCanvas.Create(const ADevice: TCustomDevice; const ADisplay: TCustomDisplay);
begin
  inherited Create(ADevice);

  FDisplay := ADisplay;
  FSurface := FDisplay;

  if FDisplay <> nil then
  begin
    FClipRect := IntRect(ZeroPoint2px, FDisplay.LogicalSize);
    FOrientationChangedHandle := FDisplay.OnOrientationChanged.Subscribe(OnOrientationChanged);
  end;
end;

destructor TDisplayCanvas.Destroy;
begin
  if (FDisplay <> nil) and (FOrientationChangedHandle <> 0) then
    FDisplay.OnOrientationChanged.Unsubscribe(FOrientationChangedHandle);

  inherited;
end;

function TDisplayCanvas.BeginDraw: Boolean;
begin
  Result := True;
end;

procedure TDisplayCanvas.EndDraw;
begin
end;

procedure TDisplayCanvas.OnOrientationChanged(const Sender: TObject; const EventData, UserData: Pointer);
begin
  if Sender is TCustomDisplay then
    FClipRect := IntRect(ZeroPoint2px, TCustomDisplay(Sender).LogicalSize);
end;

{$ENDREGION}
{$ENDIF}
{$REGION 'TCustomDisplay'}

constructor TCustomDisplay.Create;
begin
  inherited;

  FOnOrientationChanged := TEventNotifier.Create;

  FLogicalOrientation := FPhysicalOrientation;
  FLogicalSize := FPhysicalSize;

{$IFDEF DisplaySRTComponents}
  FImageFormatManager := TImageFormatManager.Create;
  FImageFormatHandler := TFCLImageFormatHandler.Create(FImageFormatManager);

  FProvider := TDisplayProvider.Create(FImageFormatManager, Self);
  FDevice := TDisplayDevice.Create(FProvider, Self);
  FCanvas := TDisplayCanvas.Create(FDevice, Self);
{$ENDIF}
end;

destructor TCustomDisplay.Destroy;
begin
{$IFDEF DisplaySRTComponents}
  FreeAndNil(FCanvas);
  FreeAndNil(FDevice);
  FreeAndNil(FProvider);
  FreeAndNil(FImageFormatHandler);
  FreeAndNil(FImageFormatManager);
{$ENDIF}

  FreeMemAndNil(FScreenBuffer);
  FreeAndNil(FOnOrientationChanged);

  inherited;
end;

{$IFDEF DisplaySRTComponents}
function TCustomDisplay.GetImages: TAtlasImages;
begin
  if FImages = nil then
    FImages := TAtlasImages.Create(FDevice);

  Result := FImages;
end;

function TCustomDisplay.GetFonts: TBitmapFonts;
begin
  if FFonts = nil then
  begin
    FFonts := TBitmapFonts.Create(FDevice);
    FFonts.Canvas := FCanvas;
  end;

  Result := FFonts;
end;
{$ENDIF}

procedure TCustomDisplay.SetLogicalOrientation(const Value: TOrientation);
var
  AdjustedValue: Integer;
begin
  if FLogicalOrientation <> Value then
  begin
    FLogicalOrientation := Value;

    AdjustedValue := Ord(FLogicalOrientation) - Ord(FPhysicalOrientation);

    if AdjustedValue < 0 then
      Inc(AdjustedValue, 4)
    else
      AdjustedValue := AdjustedValue mod 4;

    FAdjustedOrientation := TOrientation(AdjustedValue);

    if FAdjustedOrientation in [TOrientation.Portrait, TOrientation.InversePortrait] then
      FLogicalSize := Point2px(FPhysicalSize.Y, FPhysicalSize.X)
    else
      FLogicalSize := FPhysicalSize;

    FOnOrientationChanged.Notify(Self);
  end;
end;

procedure TCustomDisplay.Initialize;
begin
  Reset;
  InitSequence;
end;

procedure TCustomDisplay.Present(const Rect: TIntRect);

  procedure Exchange(var Value1, Value2: Integer);
  var
    TempValue: Integer;
  begin
    TempValue := Value1;
    Value1 := Value2;
    Value2 := TempValue;
  end;

var
  Left, Top, Right, Bottom: Integer;
begin
  Left := Rect.Left;
  Top := Rect.Top;
  Right := Rect.Right - 1;
  Bottom := Rect.Bottom - 1;

  AdjustPosition(Left, Top);
  AdjustPosition(Right, Bottom);

  if Left > Right then
    Exchange(Left, Right);

  if Top > Bottom then
    Exchange(Top, Bottom);

  Left := Saturate(Left, 0, FPhysicalSize.X);
  Right := Saturate(Right + 1, 0, FPhysicalSize.X);
  Top := Saturate(Top, 0, FPhysicalSize.Y);
  Bottom := Saturate(Bottom + 1, 0, FPhysicalSize.Y);

  if (Left < Right) and (Top < Bottom) then
    PresentBuffer(IntRectBDS(Left, Top, Right, Bottom));
end;

procedure TCustomDisplay.Present;
begin
  Present(IntRect(ZeroPoint2px, FLogicalSize));
end;

procedure TCustomDisplay.Clear;
begin
  FillChar(FScreenBuffer^, FScreenBufferSize, 0);
end;

procedure TCustomDisplay.AdjustPosition(var X, Y: Integer);
var
  TempValue: Integer;
begin
  case FAdjustedOrientation of
    TOrientation.Portrait:
    begin
      TempValue := Y;
      Y := X;
      X := (FPhysicalSize.X - 1) - TempValue;
    end;

    TOrientation.InverseLandscape:
    begin
      X := (FPhysicalSize.X - 1) - X;
      Y := (FPhysicalSize.Y - 1) - Y;
    end;

    TOrientation.InversePortrait:
    begin
      TempValue := Y;
      Y := (FPhysicalSize.Y - 1) - X;
      X := TempValue;
    end;
  end;
end;

function TCustomDisplay.GetPixel(X, Y: Integer): TIntColor;
begin
  if (X >= 0) and (Y >= 0) and (X < FLogicalSize.X) and (Y < FLogicalSize.Y) then
  begin
    AdjustPosition(X, Y);
    Result := ReadPixel(X, Y);
  end
  else
    Result := IntColorTranslucentBlack;
end;

procedure TCustomDisplay.SetPixel(X, Y: Integer; const Color: TIntColor);
begin
  if (X >= 0) and (Y >= 0) and (X < FLogicalSize.X) and (Y < FLogicalSize.Y) then
  begin
    AdjustPosition(X, Y);
    WritePixel(X, Y, Color);
  end
end;

function TCustomDisplay.GetPixelUnsafe(X, Y: Integer): TIntColor;
begin
  AdjustPosition(X, Y);
  Result := ReadPixel(X, Y);
end;

procedure TCustomDisplay.SetPixelUnsafe(X, Y: Integer; const Color: TIntColor);
begin
  AdjustPosition(X, Y);
  WritePixel(X, Y, Color);
end;

{$ENDREGION}
{$REGION 'TCustomDrivenDisplay'}

constructor TCustomDrivenDisplay.Create(const AGPIO: TCustomGPIO;
  const ADataPort: TCustomDataPort; const APinDC: Integer;
  const APinRST: Integer);
begin
  FGPIO := AGPIO;
  FDataPort := ADataPort;
  FPinDC := APinDC;
  FPinRST := APinRST;

  inherited Create;

  if FPinRST <> -1 then
    FGPIO.PinMode[FPinRST] := TPinMode.Output;

  if (FPinDC <> -1) and (FDataPort is TCustomPortSPI) then
    FGPIO.PinMode[FPinDC] := TPinMode.Output;
end;

destructor TCustomDrivenDisplay.Destroy;
begin
{$IFDEF DISPLAY_RESET_PINS_AFTER_DONE}
  if (FPinDC <> -1) and (FDataPort is TCustomPortSPI) then
    FGPIO.PinMode[FPinDC] := TPinMode.Input;

  if FPinRST <> -1 then
    FGPIO.PinMode[FPinRST] := TPinMode.Input;
{$ENDIF}

  inherited;
end;

procedure TCustomDrivenDisplay.Reset;
begin
  if FPinRST <> -1 then
  begin
    FGPIO.PinValue[FPinRST] := TPinValue.High;
    Sleep(5);
    FGPIO.PinValue[FPinRST] := TPinValue.Low;
    Sleep(10);
    FGPIO.PinValue[FPinRST] := TPinValue.High;
  end;
end;

procedure TCustomDrivenDisplay.WriteCommand(const Value: Byte);
begin
  FGPIO.PinValue[FPinDC] := TPinValue.Low;

{$IFDEF DISPLAY_SILENT}
  FDataPort.Write(@Value, 1);
{$ELSE}
  if FDataPort.Write(@Value, 1) <> 1 then
    LogText(Format(SDisplayCommandWrite, [Value]));
{$ENDIF}
end;

procedure TCustomDrivenDisplay.WriteCommand(const Values: array of Byte);
begin
  if Length(Values) > 0 then
  begin
    FGPIO.PinValue[FPinDC] := TPinValue.Low;

  {$IFDEF DISPLAY_SILENT}
    FDataPort.Write(@Values[0], Length(Values));
  {$ELSE}
    if FDataPort.Write(@Values[0], Length(Values)) <> Length(Values) then
      LogText(Format(SDisplayCommandBytesWrite, [Length(Values)]));
  {$ENDIF}
  end;
end;

procedure TCustomDrivenDisplay.WriteData(const Value: Byte);
begin
  FGPIO.PinValue[FPinDC] := TPinValue.High;

{$IFDEF DISPLAY_SILENT}
  FDataPort.Write(@Value, 1);
{$ELSE}
  if FDataPort.Write(@Value, 1) <> 1 then
    LogText(Format(SDisplayDataWrite, [Value]));
{$ENDIF}
end;

procedure TCustomDrivenDisplay.WriteData(const Values: array of Byte);
begin
  if Length(Values) > 0 then
  begin
    FGPIO.PinValue[FPinDC] := TPinValue.High;

  {$IFDEF DISPLAY_SILENT}
    FDataPort.Write(@Values[0], Length(Values));
  {$ELSE}
    if FDataPort.Write(@Values[0], Length(Values)) <> Length(Values) then
      LogText(Format(SDisplayDataBytesWrite, [Length(Values)]));
  {$ENDIF}
  end;
end;

{$ENDREGION}
{$REGION 'TCustomDrivenDisplay'}

constructor TCustomDrivenDualDisplay.Create(const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort; const APinDC,
  APinRST, AAddress: Integer);
begin
  if (AAddress <> -1) and (ADataPort is TCustomPortI2C) then
    FAddress := AAddress
  else
    FAddress := -1;

  inherited Create(AGPIO, ADataPort, APinDC, APinRST);
end;

procedure TCustomDrivenDualDisplay.WriteCommand(const Value: Byte);
begin
  if FAddress <> -1 then
  begin
    TCustomPortI2C(FDataPort).SetAddress(FAddress);
    TCustomPortI2C(FDataPort).WriteByteData(DisplayCommandID, Value);
  end
  else
    inherited;
end;

procedure TCustomDrivenDualDisplay.WriteCommand(const Values: array of Byte);
var
  I: Integer;
begin
  if FAddress <> -1 then
  begin
    for I := 0 to Length(Values) - 1 do
      WriteCommand(Values[I]);
  end
  else
    inherited;
end;

procedure TCustomDrivenDualDisplay.WriteData(const Value: Byte);
begin
  if FAddress <> -1 then
  begin
    TCustomPortI2C(FDataPort).SetAddress(FAddress);
    TCustomPortI2C(FDataPort).WriteByteData(DisplayDataID, Value);
  end
  else
    inherited;
end;

procedure TCustomDrivenDualDisplay.WriteData(const Values: array of Byte);
var
  I: Integer;
begin
  if FAddress <> -1 then
  begin
    for I := 0 to Length(Values) - 1 do
      WriteData(Values[I]);
  end
  else
    inherited;
end;

{$ENDREGION}

end.
