program DisplaySPIAndGPU;
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
  This example illustrates usage of Raspberry PI integrated GPU for rendering off-screen scene, which can then be
  uploaded to an external HX8357-driven display using SPI protocol.

  Attention! Please follow these instructions before running the sample:

   1. Since this application uses native Raspberry PI implementations of GPIO and SPI, make sure to disable SPI module
      in raspi-config. This module would be required for similar sample in "Generic" folder, which uses Sysfs.

   2. PinRST and PinDC constants should contain the corresponding pin numbers to which these lines are connected.

   3. After compiling and uploading this sample, change its attributes to executable. It is also required to execute
      this application with administrative privileges. Something like this:
        chmod +x DisplaySPIAndGPU
        sudo ./DisplaySPIAndGPU

   5. Remember to upload accompanying files "Lenna.png" and "Tahoma9b.font" to your device as well.

   6. Check the accompanying diagram and photo to see an example on how this can be connected on Raspberry PI.
}
uses
  Crt, SysUtils, PXL.TypeDef, PXL.Types, PXL.ImageFormats, PXL.ImageFormats.FCL, PXL.Boards.Types, PXL.Boards.RPi,
  PXL.Displays.Types, PXL.Displays.HX8357, PXL.Bitmaps, PXL.Devices, PXL.Canvas, PXL.SwapChains, PXL.Images, PXL.Fonts,
  PXL.Providers, PXL.Providers.GLES, PXL.Devices.GLES.RPi;

const
  PinDC = 13;
  PinRST = 11;

type
  TApplication = class
  private
    FImageFormatManager: TImageFormatManager;
    FImageFormatHandler: TCustomImageFormatHandler;

    FAcceleratedProvider: TGraphicsDeviceProvider;

    FAcceleratedDevice: TGLESDevice;
    FAcceleratedImages: TAtlasImages;
    FAcceleratedFonts: TBitmapFonts;

    FCurrentFrame: Integer;

    FImageLenna: Integer;
    FFontTahoma: Integer;

    FHardwareBitmap: TBitmap;
    FSoftwareBitmap: TBitmap;

    FDisplay: TCustomDisplay;
    FSystemCore: TFastSystemCore;
    FGPIO: TFastGPIO;
    FDataPort: TCustomDataPort;

    procedure RenderSceneGPU;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
  end;

constructor TApplication.Create;
begin
  inherited;

  FSystemCore := TFastSystemCore.Create;
  FGPIO := TFastGPIO.Create(FSystemCore);
  FDataPort := TFastSPI.Create(FGPIO, TFastSPI.DefaultChipSelect, 8000000);

  // Create and configure embedded display.
  FDisplay := TDisplay.Create(FGPIO, FDataPort, PinDC, PinRST);
  FDisplay.LogicalOrientation := TDisplay.TOrientation.Landscape;

  // Create infrastructure for loading and saving different image formats.
  FImageFormatManager := TImageFormatManager.Create;
  FImageFormatHandler := TFCLImageFormatHandler.Create(FImageFormatManager);

  // Create and configure GPU rendering provider.
  FAcceleratedProvider := TGLESProvider.Create(FIMageFormatManager);

  FAcceleratedDevice := FAcceleratedProvider.CreateDevice as TGLESDevice;
  if not FAcceleratedDevice.Initialize then
    raise Exception.Create('Failed to initialize PXL Device.');

  // Create and configure GPU images and fonts.
  FAcceleratedImages := TAtlasImages.Create(FAcceleratedDevice);

  FImageLenna := FAcceleratedImages.AddFromFile('Lenna.png');
  if FImageLenna = -1 then
    raise Exception.Create('Could not load Lenna image.');

  FAcceleratedFonts := TBitmapFonts.Create(FAcceleratedDevice);

  FFontTahoma := FAcceleratedFonts.AddFromBinaryFile('Tahoma9b.font');
  if FFontTahoma = -1 then
    raise Exception.Create('Could not load Tahoma font.');

  // GPU hardware-accelerated bitmap.
  FHardwareBitmap := TBitmap.Create(FAcceleratedDevice);
  FHardwareBitmap.SetSize(FDisplay.LogicalSize);

  // Display-bound bitmap that uses software rendering.
  FSoftwareBitmap := TBitmap.Create(FDisplay.Device);
end;

destructor TApplication.Destroy;
begin
  FSoftwareBitmap.Free;
  FHardwareBitmap.Free;
  FAcceleratedFonts.Free;
  FAcceleratedImages.Free;
  FAcceleratedDevice.Free;
  FAcceleratedProvider.Free;
  FImageFormatHandler.Free;
  FImageFormatManager.Free;
  FDisplay.Free;
  FDataPort.Free;
  FGPIO.Free;
  FSystemCore.Free;

  inherited;
end;

procedure TApplication.RenderSceneGPU;
var
  J, I: Integer;
  Omega, Kappa: Single;
begin
  FHardwareBitmap.Canvas.BeginScene;
  try
    // Draw gray background.
    for J := 0 to FHardwareBitmap.Height div 40 do
      for I := 0 to FHardwareBitmap.Width div 40 do
        FHardwareBitmap.Canvas.FillQuad(
          FloatRect4(I * 40, J * 40, 40, 40),
          IntColor4($FF585858, $FF505050, $FF484848, $FF404040));

    for I := 0 to FHardwareBitmap.Width div 40 do
      FHardwareBitmap.Canvas.Line(
        Point2(I * 40.0, 0.0),
        Point2(I * 40.0, FHardwareBitmap.Size.Y),
        $FF555555);

    for J := 0 to FHardwareBitmap.Height div 40 do
      FHardwareBitmap.Canvas.Line(
        Point2(0.0, J * 40.0),
        Point2(FHardwareBitmap.Size.X, J * 40.0),
        $FF555555);

    // Draw an animated hole.
    FHardwareBitmap.Canvas.QuadHole(
      Point2(0.0, 0.0),
      FHardwareBitmap.Size,
      Point2(
        FHardwareBitmap.Width * 0.5 + Cos(FCurrentFrame * 0.073) * FHardwareBitmap.Width * 0.25,
        FHardwareBitmap.Height * 0.5 + Sin(FCurrentFrame * 0.0312) * FHardwareBitmap.Height * 0.25),
      Point2(80.0, 100.0),
      $20FFFFFF, $80955BFF, 16);

    // Draw the image of famous Lenna.
    FHardwareBitmap.Canvas.UseImage(FAcceleratedImages[FImageLenna]);
    FHardwareBitmap.Canvas.TexQuad(FloatRect4RC(
      TPoint2(FHardwareBitmap.Size) * 0.5,
      Point2(300.0, 300.0),
      FCurrentFrame * 0.1),
      IntColorAlpha(128));

    // Draw an animated Arc.
    Omega := FCurrentFrame * 0.274;
    Kappa := 1.25 * Pi + Sin(FCurrentFrame * 0.1854) * 0.5 * Pi;

    FHardwareBitmap.Canvas.FillArc(
      Point2(FHardwareBitmap.Width * 0.15, FHardwareBitmap.Height * 0.8),
      Point2(75.0, 50.0),
      Omega, Omega + Kappa, 32,
      IntColor4($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

    // Draw an animated Ribbon.
    Omega := FCurrentFrame * 0.2231;
    Kappa := 1.25 * Pi + Sin(FCurrentFrame * 0.24751) * 0.5 * Pi;

    FHardwareBitmap.Canvas.FillRibbon(
      Point2(FHardwareBitmap.Width * 0.75, FHardwareBitmap.Height * 0.8),
      Point2(25.0, 20.0),
      Point2(70.0, 80.0),
      Omega, Omega + Kappa, 32,
      IntColor4($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

    // Draw some text.
    FAcceleratedFonts.Canvas := FHardwareBitmap.Canvas;

    FAcceleratedFonts[FFontTahoma].DrawText(
      Point2(4.0, 4.0),
      'GPU rendering on SPI display demo application.',
      IntColor2($FFFFE887, $FFFF0000));

    FAcceleratedFonts[FFontTahoma].DrawText(
      Point2(4.0, 24.0),
      'Technology: ' + GetFullDeviceTechString(FAcceleratedDevice),
      IntColor2($FFE8FFAA, $FF12C312));
  finally
    FHardwareBitmap.Canvas.EndScene;
  end;
end;

procedure TApplication.Execute;
var
  Key: StdChar;
begin
  WriteLn('Initializing display...');
  FDisplay.Initialize;

  WriteLn('Drawing continuously on display, press ESC to stop...');

  while True do
  begin
    if KeyPressed then
    begin
      Key := ReadKey;
      if Key = #27 then
        Break;
    end;

    // Render scene using GPU.
    RenderSceneGPU;

    { Copy from hardware-accelerated bitmap to software bitmap. Note that software bitmap is bound to the same
      provider as the display, so it can be used for drawing there. Trying to draw hardware-accelerated bitmap on the
      display directly would not work as they are not bound to the same provider. }
    FSoftwareBitmap.CopyFrom(FHardwareBitmap);

    // Draw software bitmap on display.
    FDisplay.Canvas.UseImage(FSoftwareBitmap);
    FDisplay.Canvas.TexQuad(FloatRect4(0.0, 0.0, FSoftwareBitmap.Width, FSoftwareBitmap.Height), IntColorWhite4);

    // Present picture on the display.
    FDisplay.Present;

    Inc(FCurrentFrame);
  end;
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

