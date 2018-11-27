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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, PXL.Types, PXL.Timing, PXL.Devices, PXL.Canvas,
  PXL.SwapChains, PXL.Fonts, PXL.Providers;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    procedure PositionForms;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure RenderWindows;
    procedure RenderPrimary;
    procedure RenderSecondary;
  public
    { public declarations }
    PrimarySize: TPoint2px;
    SecondarySize: TPoint2px;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    EngineTicks: Integer;

    FontBookAntiqua: Integer;
  end;

var
  MainForm: TMainForm;

implementation
{$R *.lfm}

uses
  LCLType, PXL.Classes, PXL.Providers.Auto, SecondFm;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PositionForms;

  DeviceProvider := CreateDefaultProvider;
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  PrimarySize := Point2px(ClientWidth, ClientHeight);

  EngineDevice.SwapChains.Add(Handle, PrimarySize);

  if SecondForm <> nil then
    EngineDevice.SwapChains.Add(SecondForm.Handle, SecondarySize);

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

  FontBookAntiqua := EngineFonts.AddFromBinaryFile(CrossFixFileName('..\..\..\Media\BookAntiqua24.font'));
  if FontBookAntiqua = -1 then
  begin
    MessageDlg('Could not load Book Antiqua font.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  Application.OnIdle := ApplicationIdle;
  EngineTicks := 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  EngineTimer.Free;
  EngineFonts.Free;
  EngineCanvas.Free;
  EngineDevice.Free;
  DeviceProvider.Free;
end;

procedure TMainForm.PositionForms;
begin
  if Screen.MonitorCount > 0 then
  begin
    BorderStyle := bsNone;

    Left := Screen.Monitors[0].Left;
    Top := Screen.Monitors[0].Top;

    Width := Screen.Monitors[0].Width;
    Height := Screen.Monitors[0].Height;
  end;

  // If more than one monitor is present, create the second form and place it on the second monitor.
  if (SecondForm = nil) and (Screen.MonitorCount > 1) then
  begin
    SecondForm := TSecondForm.Create(Self);
    SecondForm.Show;

    SecondForm.BorderStyle := bsNone;
    SecondForm.Left := Screen.Monitors[1].Left;
    SecondForm.Top := Screen.Monitors[1].Top;

    SecondForm.Width := Screen.Monitors[1].Width;
    SecondForm.Height := Screen.Monitors[1].Height;
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  PrimarySize := Point2px(ClientWidth, ClientHeight);

  if (EngineDevice <> nil) and (EngineTimer <> nil) and EngineDevice.Initialized then
  begin
    EngineDevice.Resize(0, PrimarySize);
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
  RenderWindows;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(EngineTicks);
end;

procedure TMainForm.RenderWindows;
begin
  // Render on second window.
  if (SecondForm <> nil) and EngineDevice.BeginScene(1) then
  try
    EngineDevice.Clear([TClearType.Color], $FF404040);

    if EngineCanvas.BeginScene then
    try
      RenderSecondary;
    finally
      EngineCanvas.EndScene;
    end;
  finally
    EngineDevice.EndScene;
  end;

  // Render on first window.
  if EngineDevice.BeginScene(0) then
  try
    EngineDevice.Clear([TClearType.Color], $FF000040);

    if EngineCanvas.BeginScene then
    try
      RenderPrimary;
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

procedure TMainForm.RenderPrimary;
begin
  EngineFonts[FontBookAntiqua].DrawText(
    Point2(4.0, 4.0),
    'This text should appear on first monitor.',
    IntColor2($FFE8F9FF, $FFAEE2FF));

  EngineFonts[FontBookAntiqua].DrawText(
    Point2(4.0, 44.0),
    'Frame Rate: ' + IntToStr(EngineTimer.FrameRate),
    IntColor2($FFEED1FF, $FFA1A0FF));

  EngineFonts[FontBookAntiqua].DrawText(
    Point2(4.0, 84.0),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    IntColor2($FFE8FFAA, $FF12C312));
end;

procedure TMainForm.RenderSecondary;
begin
  EngineFonts[FontBookAntiqua].DrawText(
    Point2(4.0, 4.0),
    'This text should appear on second monitor.',
    IntColor2($FFFFD27B, $FFFF0000));

  EngineFonts[FontBookAntiqua].DrawText(
    Point2(4.0, 44.0),
    'FPS: ' + IntToStr(EngineTimer.FrameRate),
    IntColor2($FFE4FFA5, $FF00E000));
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.

