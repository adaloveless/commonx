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
  This example illustrates usage of SPI protocol and real-time drawing on color OLED display with ILI9340 driver.

  Attention! Please follow these instructions before running the sample:

   1. Check the accompanying diagram and photo to see an example on how this can be connected on Intel Galileo.

   2. After compiling and uploading this sample, change its attributes to executable. Something like this:
        chmod +x DisplaySPI
        ./DisplaySPI

   3. Remember to upload accompanying files "kristen.font" and "lenna.png" to your device as well.
}
uses
  Crt, SysUtils, PXL.TypeDef, PXL.Types, PXL.Fonts, PXL.Boards.Types, PXL.Boards.Galileo, PXL.Displays.Types,
  PXL.Displays.ILI9340;

const
  // Please make sure to specify the following pins according to their physical number on Galileo.
  PinRST = 2;
  PinDC = 4;

type
  TApplication = class
  private
    FGPIO: TCustomGPIO;
    FDataPort: TCustomDataPort;
    FDisplay: TCustomDisplay;

    FDisplaySize: TPoint2px;

    FFontKristen: Integer;
    FImageLenna: Integer;

    procedure LoadGraphics;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
  end;

constructor TApplication.Create;
begin
  FGPIO := TGalileoGPIO.Create;
  FDataPort := TGalileoSPI.Create(FGPIO as TGalileoGPIO);

  FDisplay := TDisplay.Create(FGPIO, FDataPort, PinDC, PinRST);
  FDisplay.LogicalOrientation := TDisplay.TOrientation.InverseLandscape;
  FDisplaySize := FDisplay.LogicalSize;

  FDisplay.Initialize;
  FDisplay.Clear;

  LoadGraphics;
end;

destructor TApplication.Destroy;
begin
  FDataPort.Free;
  FGPIO.Free;

  inherited;
end;

procedure TApplication.LoadGraphics;
const
  KristenFontName: StdString = 'kristen.font';
  LennaFileName: StdString = 'lenna.png';
begin
  FFontKristen := FDisplay.Fonts.AddFromBinaryFile(KristenFontName);
  if FFontKristen = -1 then
    raise Exception.CreateFmt('Could not load %s.', [KristenFontName]);

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
    for J := 0 to FDisplaySize.Y div 32 do
      for I := 0 to FDisplaySize.X div 32 do
        FDisplay.Canvas.FillQuad(
          FloatRect4(I * 32, J * 32, 32, 32),
          IntColor4($FF101010, $FF303030, $FF585858, $FF303030));

    // Draw an animated Arc.
    Omega := Ticks * 0.0274;
    Kappa := 1.25 * Pi + Sin(Ticks * 0.01854) * 0.5 * Pi;

    FDisplay.Canvas.FillArc(
      Point2(FDisplaySize.X * 0.2, FDisplaySize.Y * 0.5),
      Point2(36.0, 32.0),
      Omega, Omega + Kappa, 16,
      IntColor4($FFA4E581, $FFFF9C00, $FF7728FF, $FFFFFFFF));

    // Draw an animated Ribbon.
    Omega := Ticks * 0.02231;
    Kappa := 1.25 * Pi + Sin(Ticks * 0.024751) * 0.5 * Pi;

    FDisplay.Canvas.FillRibbon(
      Point2(FDisplaySize.X * 0.8, FDisplaySize.Y * 0.5),
      Point2(16.0, 18.0),
      Point2(40.0, 34.0),
      Omega, Omega + Kappa, 16,
      IntColor4($FFFF244F, $FFACFF0D, $FF2B98FF, $FF7B42FF));

    // Draw the image of famous Lenna.
    FDisplay.Canvas.UseImage(FDisplay.Images[FImageLenna]);
    FDisplay.Canvas.TexQuad(FloatRect4RC(
      Point2(FDisplaySize.X * 0.5, FDisplaySize.Y * 0.5),
      Point2(128.0, 128.0),
      Ticks * 0.01),
      IntColorAlpha(128));

    // Draw some text.
    FDisplay.Fonts[FFontKristen].DrawTextAligned(
      Point2(FDisplaySize.X * 0.5, 2.0),
      'Hello World.',
      IntColor2($FFFFE000, $FFFF0000),
      TTextAlignment.Middle, TTextAlignment.Start);

    FDisplay.Fonts[FFontKristen].DrawText(
      Point2(1.0, FDisplaySize.Y - 22.0),
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

