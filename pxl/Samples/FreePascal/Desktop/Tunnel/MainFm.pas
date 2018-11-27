unit MainFm;
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

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, PXL.TypeDef, PXL.Types, PXL.Timing, PXL.Devices,
  PXL.ImageFormats, PXL.Canvas, PXL.SwapChains, PXL.Fonts, PXL.Providers, PXL.Bitmaps;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    ImageFormatManager: TImageFormatManager;
    ImageFormatHandler: TCustomImageFormatHandler;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    DisplaySize: TPoint2px;
    EngineTicks: Integer;

    FontTempusSans: Integer;

    BitmapMain: TBitmap;
    BitmapCopy: TBitmap;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure PrepareBitmaps;

    procedure RenderWindow;
    procedure RenderScene;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation
{$R *.lfm}

uses
  PXL.Classes, PXL.Providers.Auto, PXL.ImageFormats.Auto;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ImageFormatManager := TImageFormatManager.Create;
  ImageFormatHandler := CreateDefaultImageFormatHandler(ImageFormatManager);

  DeviceProvider := CreateDefaultProvider(ImageFormatManager);
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  DisplaySize := Point2px(ClientWidth, ClientHeight);
  EngineDevice.SwapChains.Add(Handle, DisplaySize, 0, True);

  if not EngineDevice.Initialize then
  begin
    MessageDlg('Failed to initialize PXL Device.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineCanvas := DeviceProvider.CreateCanvas(EngineDevice);
  if not EngineCanvas.Initialize then
  begin
    MessageDlg('Failed to initialize PXL Canvas.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  FontTempusSans := EngineFonts.AddFromBinaryFile(CrossFixFileName('..\..\..\Media\TempusSans.font'));
  if FontTempusSans = -1 then
  begin
    MessageDlg('Could not load TempusSans font.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  Application.OnIdle := ApplicationIdle;
  EngineTicks := 0;

  BitmapMain := TBitmap.Create(EngineDevice);
  BitmapMain.SetSize(512, 512);

  BitmapCopy := TBitmap.Create(EngineDevice);
  BitmapCopy.Storage := TBitmapStorage.Drawable;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  BitmapCopy.Free;
  BitmapMain.Free;
  EngineTimer.Free;
  EngineFonts.Free;
  EngineCanvas.Free;
  EngineDevice.Free;
  DeviceProvider.Free;
  ImageFormatHandler.Free;
  ImageFormatManager.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  DisplaySize := Point2px(ClientWidth, ClientHeight);

  if (EngineDevice <> nil) and (EngineTimer <> nil) and EngineDevice.Initialized then
  begin
    EngineDevice.Resize(0, DisplaySize);
    EngineTimer.Reset;
  end;
end;

procedure TMainForm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  EngineTimer.NotifyTick;
  Done := False;
end;

procedure TMainForm.EngineTiming(const Sender: TObject);
begin
  PrepareBitmaps;

  RenderWindow;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(EngineTicks);
end;

procedure TMainForm.PrepareBitmaps;
const
  SourceMapping: TFloatRect4 = (
    TopLeft    : (X:   0 + 4; Y: 0 + 4);
    TopRight   : (X: 511 - 1; Y: 0 + 3);
    BottomRight: (X: 511 - 3; Y: 511 - 4);
    BottomLeft : (X:   0 + 1; Y: 511 - 8));
var
  Theta, RibbonLength: Single;
begin
  if BitmapMain.Canvas.BeginScene then
  try
    // Copy previous scene, englarged and slightly rotated.
    BitmapMain.Canvas.UseImagePx(BitmapCopy, SourceMapping);
    BitmapMain.Canvas.TexQuad(FloatRect4(0.0, 0.0, 512.0, 512.0), IntColorWhite4);

    // Darken the area slightly, to avoid color mess :)
    // Replace color parameter to $FFF0F0F0 to reduce the effect.
    BitmapMain.Canvas.FillRect(0, 0, 512, 512, $FFFCFCFC, TBlendingEffect.Multiply);

    // Add the "motion scene" to the working surface.
    Theta := (EngineTicks mod 200) * Pi / 100;
    RibbonLength := (1.0 + Sin(EngineTicks / 50.0)) * Pi * 2 / 3 + (Pi / 3);

    BitmapMain.Canvas.FillRibbon(
      Point2(256, 256 - 32), Point2(32.0, 48.0), Point2(96.0, 64.0),
      Theta, Theta + RibbonLength, 64,
      $7F7E00FF, $7F75D3FF, $7FD1FF75, $7FFFC042, $7F00FF00, $7FFF0000);
  finally
    BitmapMain.Canvas.EndScene;
  end;

  // Copy newly created scene to auxiliary bitmap.
  BitmapCopy.CopyFrom(BitmapMain);
end;

procedure TMainForm.RenderWindow;
begin
  if EngineDevice.BeginScene then
  try
    EngineDevice.Clear([TClearType.Color], $FF404040);

    if EngineCanvas.BeginScene then
    try
      RenderScene;
    finally
      EngineCanvas.EndScene;
    end;

    { Invoke "EngineProcess" event (60 times per second, independently of rendering speed) to do processing and calculations
      while GPU is busy rendering the scene. }
    EngineTimer.Process;
  finally
    EngineDevice.EndScene;
  end;
end;

procedure TMainForm.RenderScene;
begin
  // Simply draw the bitmap on the screen.
  EngineCanvas.UseImage(BitmapMain);
  EngineCanvas.TexQuad(
    FloatRect4(
      (DisplaySize.X - BitmapMain.Width) * 0.5, (DisplaySize.Y - BitmapMain.Height) * 0.5,
      BitmapMain.Width, BitmapMain.Height), IntColorWhite4, TBlendingEffect.Add);

  // Display the information text.
  EngineFonts[FontTempusSans].DrawText(
    Point2(4.0, 4.0),
    'Frame Rate: ' + IntToStr(EngineTimer.FrameRate),
    IntColor2($FFEDF8FF, $FFA097FF));

  EngineFonts[FontTempusSans].DrawText(
    Point2(4.0, 24.0),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    IntColor2($FFE8FFAA, $FF12C312));
end;

end.

