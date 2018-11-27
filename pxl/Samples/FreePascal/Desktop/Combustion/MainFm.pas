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

{
  An old demo that originally appeared in Asphyre's predecessor library called "PowerDraw" version 2, published
  on Delphi news site "Turbo" somewhere around 2002. The example has been reworked to use PXL's new "script object"
  mechanism.

  This sample also illustrates one of PXL features - support loading images and fonts from older archives.
  PXL supports older Asphyre archives such VTDb, ASDb and ASVF in addition to its own format, PXLA. Note that
  earlier archive formats such as VTD2 from very first Asphyre releases are not supported.
}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, PXL.Types, PXL.Timing, PXL.Devices, PXL.Canvas,
  PXL.SwapChains, PXL.Images, PXL.Fonts, PXL.Providers, PXL.Archives, PXL.Scripts;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    EngineTicks: Integer;

    EngineArchive: TArchive;

    ImagePowerDraw: Integer;
    ImageScanline: Integer;
    FontVerdana: Integer;

    EngineParticles: TScriptObject;

    BackColor1: TIntColor;
    BackColor2: TIntColor;
    BackAlpha: Integer;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

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
  PXL.Providers.Auto, PXL.Archives.Loading;

const
  // Defines the overall size of the combustion particle.
  ParticleSize = 128;

var
  // The following variables are made global so they can be accessed by particle objects.
  DisplaySize: TPoint2px;

  EngineCanvas: TCustomCanvas;
  EngineImages: TAtlasImages;

  ImageCombustion: Integer = -1;

type
  // This is a simplest particle object that moves with constant acceleration and has fixed life.
  TParticle = class(TScriptObject)
  private
    FPosition: TPoint2;
    FVelocity: TPoint2;
    FAcceleration: TPoint2;
    FCurrentLife: Integer;
    FMaximumLife: Integer;
    FAngle: Single;
    FScale: Single;
  protected
    procedure DoUpdate; override;
    procedure DoDraw; override;
  public
    constructor Create(const AOwner: TScriptObject);

    property Position: TPoint2 read FPosition;
    property Velocity: TPoint2 read FVelocity;
    property Acceleration: TPoint2 read FAcceleration;

    property CurrentLife: Integer read FCurrentLife;
    property MaximumLife: Integer read FMaximumLife;

    property Angle: Single read FAngle;
    property Scale: Single read FScale;
  end;

constructor TParticle.Create(const AOwner: TScriptObject);
begin
  inherited Create(AOwner);

  FPosition := Point2(Random(DisplaySize.X + 1), DisplaySize.Y);
  FVelocity := Point2((Random(10) - 5) / 20.0, -(Random(20) / 5.0));
  FAcceleration := Point2(0.0, -(0.001 + (Random(15) / 100)));
  FMaximumLife := 56 + Random(32);
  FAngle := Random * 2.0 * Pi;
  FScale := 0.75 + Random * 0.5;
end;

procedure TParticle.DoUpdate;
begin
  FVelocity := FVelocity + FAcceleration;
  FPosition := FPosition + FVelocity;

  Inc(FCurrentLife);
  if FCurrentLife >= FMaximumLife then
    ObjDispose;
end;

procedure TParticle.DoDraw;
var
  Image: TAtlasImage;
  Region: Integer;
begin
  Image := EngineImages[ImageCombustion];
  Region := (FCurrentLife * Image.Regions.Count) div FMaximumLife;

  EngineCanvas.UseImageRegion(Image, Region);
  EngineCanvas.TexQuad(
    FloatRect4RC(FPosition, Point2(ParticleSize, ParticleSize), FAngle, FScale),
    IntColorAlpha(0.75));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;

  DeviceProvider := CreateDefaultProvider;
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

  EngineArchive := TArchive.Create;
  EngineArchive.OpenMode := TArchive.TOpenMode.ReadOnly;

  if not EngineArchive.OpenFile('media.vtdb') then
  begin
    MessageDlg('Failed to open media archive.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineImages := TAtlasImages.Create(EngineDevice);

  ImagePowerDraw := LoadImageFromArchive('PowerDraw logo', EngineImages, EngineArchive);
  if ImagePowerDraw = -1 then
  begin
    MessageDlg('Could not load PowerDraw logo image.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  ImageScanline := LoadImageFromArchive('Scanline Tex', EngineImages, EngineArchive);
  if ImageScanline = -1 then
  begin
    MessageDlg('Could not load Scanline Texture image.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  ImageCombustion := LoadImageFromArchive('Fire Combustion', EngineImages, EngineArchive);
  if ImageCombustion = -1 then
  begin
    MessageDlg('Could not load Combustion image.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  FontVerdana := LoadFontFromArchive('Verdana.font', EngineFonts, EngineArchive);
  if FontVerdana = -1 then
  begin
    MessageDlg('Could not load Verdana font from archive.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  Application.OnIdle := ApplicationIdle;
  EngineTicks := 0;

  EngineParticles := TScriptObject.Create;

  BackColor1 := IntColorBlack;
  BackColor2 := IntColorRGB(Random(256), Random(256), Random(256));
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  EngineParticles.Free;
  EngineTimer.Free;
  EngineFonts.Free;
  EngineImages.Free;
  EngineArchive.Free;
  EngineCanvas.Free;
  EngineDevice.Free;
  DeviceProvider.Free;
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
  RenderWindow;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
var
  Particle: TParticle;
  I: Integer;
begin
  Inc(EngineTicks);

  for I := 0 to 18 do
    if Random(2) = 0 then
    begin
      Particle := TParticle.Create(EngineParticles);
      Particle.DrawOrder := -EngineTicks;
    end;

  EngineParticles.Update;

  Inc(BackAlpha);
  if BackAlpha >= 255 then
  begin
    BackColor1 := BackColor2;
    BackColor2 := IntColorRGB(Random(256), Random(256), Random(256));
    BackAlpha := 0;
  end;
end;

procedure TMainForm.RenderWindow;
begin
  if EngineDevice.BeginScene then
  try
    EngineDevice.Clear([TClearType.Color], BlendPixels(BackColor1, BackColor2, BackAlpha));

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
const
  LogoSize: TPoint2px = (X: 480; Y: 128);
  StatusSize: TPoint2px = (X: 400; Y: 32);
var
 DrawAt: TPoint2px;
 I, J: Integer;
begin
  // Draw particles according to their order.
  EngineParticles.DrawSorted;

  // Show "PowerDraw" logo on top of the particles.
  DrawAt.X := (DisplaySize.X - LogoSize.X) div 2;
  DrawAt.Y := (DisplaySize.Y - LogoSize.Y) div 2;

  EngineCanvas.UseImageRegion(EngineImages[ImagePowerDraw], 0);
  EngineCanvas.TexQuad(FloatRect4(DrawAt.X, DrawAt.Y, LogoSize.X div 2, LogoSize.Y), IntColorWhite4);

  EngineCanvas.UseImageRegion(EngineImages[ImagePowerDraw], 1);
  EngineCanvas.TexQuad(FloatRect4(DrawAt.X + (LogoSize.X div 2), DrawAt.Y, LogoSize.X div 2, LogoSize.Y),
    IntColorWhite4);

  // Apply "Scanline" effect to the whole scene.
  for J := 0 to DisplaySize.Y div 64 do
    for I := 0 to DisplaySize.X div 64 do
    begin
      EngineCanvas.UseImage(EngineImages[ImageScanline]);
      EngineCanvas.TexQuad(FloatRect4(I * 64, J * 64, 64, 64), IntColorWhite4, TBlendingEffect.Multiply);
    end;

  // Draw some fill for status background.
  DrawAt.X := (DisplaySize.X - StatusSize.X) div 2;
  DrawAt.Y := LogoSize.Y + ((DisplaySize.Y - StatusSize.Y) div 2);

  EngineCanvas.FillRect(DrawAt.X, DrawAt.Y, StatusSize.X, StatusSize.Y, $FF5F5F5F, TBlendingEffect.Multiply);
  EngineCanvas.FillRect(DrawAt.X, DrawAt.Y, StatusSize.X, StatusSize.Y, $FF1F1F1F, TBlendingEffect.Add);
  EngineCanvas.FrameRect(FloatRect(DrawAt.X, DrawAt.Y, StatusSize.X, StatusSize.Y), $FF3F3F3F, TBlendingEffect.Add);

  // Show current status.
  EngineFonts[FontVerdana].DrawText(
    Point2(DrawAt.X + 2, DrawAt.Y),
    'Frame Rate: ' + IntToStr(EngineTimer.FrameRate) + ', Particle Count: ' + IntToStr(EngineParticles.ComputeTotalNodeCount),
    IntColor2($FF00FF00, $FFFFFFFF));

  EngineFonts[FontVerdana].DrawText(
    Point2(DrawAt.X + 2, DrawAt.Y + 14),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    IntColor2($FFFF00FF, $FFFFFFFF));
end;

end.

