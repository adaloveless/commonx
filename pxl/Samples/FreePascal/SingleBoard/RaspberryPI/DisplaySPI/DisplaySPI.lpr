program DisplaySPI;
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
  This example illustrates usage of SPI protocol and real-time drawing on color OLED display with SSD1351 driver.

  Attention! Please follow these instructions before running the sample:

   1. Since this application uses native Raspberry PI implementations of GPIO and SPI, make sure to disable SPI module
      in raspi-config. This module would be required for similar sample in "Generic" folder, which uses Sysfs.

   2. PinRST and PinDC constants should contain the corresponding pin numbers to which these lines are connected.

   3. After compiling and uploading this sample, change its attributes to executable. It is also required to execute
      this application with administrative privileges. Something like this:
        chmod +x DisplaySPI
        sudo ./DisplaySPI

   5. Remember to upload accompanying files "corbel.font", "consolas.font" and "lenna.png" to your device as well.

   6. Check the accompanying diagram and photo to see an example on how this can be connected on Raspberry PI.
}
uses
  Crt, SysUtils, PXL.TypeDef, PXL.Types, PXL.Fonts, PXL.Boards.Types, PXL.Boards.RPi, PXL.Displays.Types,
  PXL.Displays.SSD1351;

const
  // Please make sure to specify the following pins according to Raspberry PI pin numbering scheme.
  PinRST = 11;
  PinDC = 13;

type
  TApplication = class
  private
    FSystemCore: TFastSystemCore;
    FGPIO: TFastGPIO;
    FDataPort: TCustomDataPort;
    FDisplay: TCustomDisplay;

    FDisplaySize: TPoint2px;

    FFontCorbel: Integer;
    FFontConsolas: Integer;
    FImageLenna: Integer;

    procedure LoadGraphics;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
  end;

constructor TApplication.Create;
begin
  FSystemCore := TFastSystemCore.Create;
  FGPIO := TFastGPIO.Create(FSystemCore);
  FDataPort := TFastSPI.Create(FGPIO);

  FDisplay := TDisplay.Create(TDisplay.OLED128x128, FGPIO, FDataPort, PinDC, PinRST);

  FDisplaySize := (FDisplay as TDisplay).ScreenSize;

  FDisplay.Initialize;
  FDisplay.Clear;

  LoadGraphics;
end;

destructor TApplication.Destroy;
begin
  FDataPort.Free;
  FGPIO.Free;
  FSystemCore.Free;

  inherited;
end;

procedure TApplication.LoadGraphics;
const
  CorbelFontName: StdString = 'corbel.font';
  ConsolasFontName: StdString = 'consolas.font';
  LennaFileName: StdString = 'lenna.png';
begin
  FFontCorbel := FDisplay.Fonts.AddFromBinaryFile(CorbelFontName);
  if FFontCorbel = -1 then
    raise Exception.CreateFmt('Could not load %s.', [CorbelFontName]);

  FFontConsolas := FDisplay.Fonts.AddFromBinaryFile(ConsolasFontName);
  if FFontConsolas = -1 then
    raise Exception.CreateFmt('Could not load %s.', [ConsolasFontName]);

  FImageLenna := FDisplay.Images.AddFromFile(LennaFileName);
  if FImageLenna = -1 then
    raise Exception.CreateFmt('Could not image %s.', [LennaFileName]);
end;

procedure TApplication.Execute;
var
  J, I: Integer;
  Ticks: Integer = 0;
  Omega, Kappa: Single;
begin
  WriteLn('Showing animation on display, press any key to exit...');

  while not KeyPressed do
  begin
    Inc(Ticks);
    FDisplay.Clear;

    // Draw some background.
    for J := 0 to FDisplaySize.Y div 16 do
      for I := 0 to FDisplaySize.X div 16 do
        FDisplay.Canvas.FillQuad(
          FloatRect4(I * 16, J * 16, 16, 16),
          IntColor4($FF101010, $FF303030, $FF585858, $FF303030));

    // Draw an animated Arc.
    Omega := Ticks * 0.0274;
    Kappa := 1.25 * Pi + Sin(Ticks * 0.01854) * 0.5 * Pi;

    FDisplay.Canvas.FillArc(
      Point2(FDisplaySize.X * 0.2, FDisplaySize.Y * 0.5),
      Point2(16.0, 15.0),
      Omega, Omega + Kappa, 16,
      IntColor4($FFA4E581, $FFFF9C00, $FF7728FF, $FFFFFFFF));

    // Draw an animated Ribbon.
    Omega := Ticks * 0.02231;
    Kappa := 1.25 * Pi + Sin(Ticks * 0.024751) * 0.5 * Pi;

    FDisplay.Canvas.FillRibbon(
      Point2(FDisplaySize.X * 0.8, FDisplaySize.Y * 0.5),
      Point2(7.0, 3.0),
      Point2(14.0, 16.0),
      Omega, Omega + Kappa, 16,
      IntColor4($FFFF244F, $FFACFF0D, $FF2B98FF, $FF7B42FF));

    // Draw the image of famous Lenna.
    FDisplay.Canvas.UseImage(FDisplay.Images[FImageLenna]);
    FDisplay.Canvas.TexQuad(FloatRect4RC(
      Point2(FDisplaySize.X * 0.5, FDisplaySize.Y * 0.5),
      Point2(64.0, 64.0),
      Ticks * 0.01),
      IntColorAlpha(128));

    // Draw some text.
    FDisplay.Fonts[FFontCorbel].DrawTextAligned(
      Point2(FDisplaySize.X * 0.5, 2.0),
      'Hello World.',
      IntColor2($FFFFE000, $FFFF0000),
      TTextAlignment.Middle, TTextAlignment.Start);

    FDisplay.Fonts[FFontConsolas].DrawText(
      Point2(1.0, FDisplaySize.Y - 14.0),
      'Frame #: ' + IntToStr(Ticks),
      IntColor2($FFD6F5FC, $FF3E0DDC));

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

