program DisplayI2C;
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
{
  This example illustrates usage of I2C protocol and grayscale drawing on monochrome OLED display with SSD1306 driver.

  Attention! Please follow these instructions before running the sample:

   1. When creating TSysfsGPIO and TSysfsI2C classed, make sure that the corresponding Linux paths are correct.
      For instance, on BeagleBoneBlack they are "/sys/class/gpio" and "/dev/i2c-1" accordingly.

   2. PinRST constant should contain actual Linux GPIO number corresponding to physical pin.

   3. On some devices, before using pins for GPIO, it is necessary to set one or more multiplexers. Refer to the
      manual of your specific device for the information on how to do this. In many cases, you can use the same GPIO
      class to do it, e.g. "GPIO.SetMux(24, TPinValue.High)".

   4. After compiling and uploading this sample, change its attributes to executable. It is also recommended to
      execute this application with administrative privileges. Something like this:
        chmod +x DisplayI2C
        sudo ./DisplayI2C

   5. Remember to upload accompanying file "tahoma8.font" to your device as well.

   6. Check the accompanying diagram and photo to see an example on how this can be connected on BeagleBone Black.
}
uses
  Crt, SysUtils, PXL.TypeDef, PXL.Types, PXL.Fonts, PXL.Boards.Types, PXL.Sysfs.GPIO, PXL.Sysfs.I2C,
  PXL.Displays.Types, PXL.Displays.SSD1306;

const
  DisplayAddressI2C = $3C;

  // Please make sure to specify the following RST pin according to Linux GPIO numbering scheme on your device.
  PinRST = 60;

type
  TApplication = class
  private
    FGPIO: TCustomGPIO;
    FDataPort: TCustomDataPort;
    FDisplay: TCustomDisplay;

    FDisplaySize: TPoint2px;

    FFontSystem: Integer;
    FFontTahoma: Integer;

    procedure LoadFonts;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
  end;

constructor TApplication.Create;
begin
  FGPIO := TSysfsGPIO.Create;
  FDataPort := TSysfsI2C.Create('/dev/i2c-1');

  FDisplay := TDisplay.Create(TDisplay.OLED128x32, FGPIO, FDataPort, -1, PinRST, DisplayAddressI2C);

  FDisplaySize := (FDisplay as TDisplay).ScreenSize;

  FDisplay.Initialize;
  FDisplay.LogicalOrientation := TCustomDisplay.TOrientation.InverseLandscape;
  FDisplay.Clear;

  LoadFonts;
end;

destructor TApplication.Destroy;
begin
  FDataPort.Free;
  FGPIO.Free;

  inherited;
end;

procedure TApplication.LoadFonts;
const
  TahomaFontName: StdString = 'tahoma8.font';
begin
  FFontTahoma := FDisplay.Fonts.AddFromBinaryFile(TahomaFontName);
  if FFontTahoma = -1 then
    raise Exception.CreateFmt('Could not load %s.', [TahomaFontName]);

  FFontSystem := FDisplay.Fonts.AddSystemFont(TSystemFontImage.Font9x8);
  if FFontSystem = -1 then
    raise Exception.Create('Could not load system font.');
end;

procedure TApplication.Execute;
var
  Ticks: Integer = 0;
  Omega, Kappa: Single;
begin
  WriteLn('Showing animation on display, press any key to exit...');

  while not KeyPressed do
  begin
    Inc(Ticks);
    FDisplay.Clear;

    // Draw an animated ribbon with some sort of grayscale gradient.
    Omega := Ticks * 0.02231;
    Kappa := 1.25 * Pi + Sin(Ticks * 0.024751) * 0.5 * Pi;

    FDisplay.Canvas.FillRibbon(
      Point2(FDisplaySize.X * 0.8, FDisplaySize.Y * 0.5),
      Point2(7.0, 3.0),
      Point2(14.0, 16.0),
      Omega, Omega + Kappa, 16,
      IntColor4($FF000000, $FF404040, $FFFFFFFF, $FF808080));

    // Draw some text.
    FDisplay.Fonts[FFontTahoma].DrawText(Point2(0.0, 1.0),
      'Tahoma 8 font.', IntColorWhite2);

    FDisplay.Fonts[FFontSystem].DrawText(Point2(0.0, 20.0),
      'System Font.', IntColorWhite2);

    // Send picture to the display.
    FDisplay.Present;
  end;

  ReadKey;
end;

var
  Application: TApplication = nil;

begin
  Application := TApplication.Create;
  try
    Application.Execute;
  finally
    Application.Free;
  end;
end.

