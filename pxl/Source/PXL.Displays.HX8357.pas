unit PXL.Displays.HX8357;
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

uses
  PXL.Types, PXL.Boards.Types, PXL.Displays.Types;

type
  TDisplay = class(TCustomDrivenDisplay)
  private type
    PScreenColor = ^TScreenColor;
    TScreenColor = packed record
      Red: Byte;
      Green: Byte;
      Blue: Byte;
    end;
  private
    procedure SetWriteWindow(const WriteRect: TIntRect);
  protected
    procedure InitSequence; override;
    procedure PresentBuffer(const Rect: TIntRect); override;

    function ReadPixel(const X, Y: Integer): TIntColor; override;
    procedure WritePixel(const X, Y: Integer; const Color: TIntColor); override;

    function GetScanline(const Index: Integer): Pointer; override;
  public
    constructor Create(const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort;
      const APinDC: Integer; const APinRST: Integer = -1);
  end;

implementation

uses
  SysUtils, Math;

const
  CMD_COLUMN_ADDR_SET = $2A;
  CMD_DISPLAY_ON = $29;
  CMD_ENABLE_EXT = $B9;
  CMD_MEM_WRITE = $2C;
  CMD_PAGE_ADDR_SET = $2B;
  CMD_SET_ACCESS_CTRL = $36;
  CMD_SET_DISPLAY_CYCLE = $B4;
  CMD_SET_GAMMA_CURVE = $E0;
  CMD_SET_INT_OSC = $B0;
  CMD_SET_PANEL_CHR = $CC;
  CMD_SET_PIXEL_FORMAT = $3A;
  CMD_SET_POWER_CTRL = $B1;
  CMD_SET_RGB_INTFC = $B3;
  CMD_SET_STBA = $C0;
  CMD_SET_TEARING_FX = $35;
  CMD_SET_TEAR_SCANLINE = $44;
  CMD_SET_VCOM_VOLTAGE = $B6;
  CMD_SLEEP_OUT = $11;
  CMD_SW_RESET = $01;

constructor TDisplay.Create(const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort; const APinDC,
  APinRST: Integer);
begin
  FPhysicalOrientation := TOrientation.InversePortrait;
  FPhysicalSize := Point2px(320, 480);

  FScreenBufferSize := (FPhysicalSize.X * FPhysicalSize.Y) * SizeOf(TScreenColor);
  FScreenBuffer := AllocMem(FScreenBufferSize);

  inherited Create(AGPIO, ADataPort, APinDC, APinRST);
end;

procedure TDisplay.InitSequence;
begin
  WriteCommand(CMD_SW_RESET);
  Sleep(5);

  WriteCommand(CMD_ENABLE_EXT);
  WriteData([$FF, $83, $57]);

  WriteCommand(CMD_SET_RGB_INTFC);
  WriteData([$80, $00, $06, $06]);

  WriteCommand(CMD_SET_VCOM_VOLTAGE);
  WriteData($25);

  WriteCommand(CMD_SET_INT_OSC);
  WriteData($68);

  WriteCommand(CMD_SET_PANEL_CHR);
  WriteData($05);

  WriteCommand(CMD_SET_POWER_CTRL);
  WriteData([$00, $15, $1C, $1C, $83, $AA]);

  WriteCommand(CMD_SET_STBA);
  WriteData([$50, $50, $01, $3C, $1E, $08]);

  WriteCommand(CMD_SET_DISPLAY_CYCLE);
  WriteData([$02, $40, $00, $2A, $2A, $0D, $78]);

  WriteCommand(CMD_SET_GAMMA_CURVE);
  WriteData([$02, $0A, $11, $1D, $23, $35, $41, $4B, $4B, $42, $3A, $27, $1B, $08, $09, $03, $02, $0A, $11, $1D, $23,
    $35, $41, $4B, $4B, $42, $3A, $27, $1B, $08, $09, $03, $00, $01]);

  WriteCommand(CMD_SET_PIXEL_FORMAT);
  WriteData($FF);  // $55 for 16-bit

  WriteCommand(CMD_SET_ACCESS_CTRL);
  WriteData($88); // defines RGB/BGR and horizontal/vertical memory direction.

  WriteCommand(CMD_SET_TEARING_FX);
  WriteData($00);

  WriteCommand(CMD_SET_TEAR_SCANLINE);
  WriteData([$00, $02]);

  WriteCommand(CMD_SLEEP_OUT);
  Sleep(5);

  WriteCommand(CMD_DISPLAY_ON);
end;

procedure TDisplay.SetWriteWindow(const WriteRect: TIntRect);
begin
  WriteCommand(CMD_COLUMN_ADDR_SET);
  WriteData([Cardinal(WriteRect.Left) shr 8, Cardinal(WriteRect.Left) and $FF, Cardinal(WriteRect.Right - 1) shr 8,
    Cardinal(WriteRect.Right - 1) and $FF]);

  WriteCommand(CMD_PAGE_ADDR_SET);
  WriteData([Cardinal(WriteRect.Top) shr 8, Cardinal(WriteRect.Top) and $FF, Cardinal(WriteRect.Bottom - 1) shr 8,
    Cardinal(WriteRect.Bottom - 1) and $FF]);

  WriteCommand(CMD_MEM_WRITE);

  FGPIO.PinValue[FPinDC] := TPinValue.High;
end;

procedure TDisplay.PresentBuffer(const Rect: TIntRect);
var
  I, StartPos, BytesToCopy, BytesTotal: Integer;
begin
  SetWriteWindow(Rect);

  BytesTotal := Rect.Width * Rect.Height * SizeOf(TScreenColor);
  StartPos := (Rect.Top * FPhysicalSize.X + Rect.Left) * SizeOf(TScreenColor);

  if (Rect.Left = 0) and (Rect.Top = 0) and (Rect.Right = FPhysicalSize.X) and (Rect.Bottom = FPhysicalSize.Y) then
    for I := 0 to FScreenBufferSize div MaxSPITransferSize do
    begin // Full burst copy.
      StartPos := I * MaxSPITransferSize;
      BytesToCopy := Min(FScreenBufferSize - StartPos, MaxSPITransferSize);

      if BytesToCopy > 0 then
        FDataPort.Write(Pointer(PtrUInt(FScreenBuffer) + Cardinal(StartPos)), BytesToCopy)
      else
        Break;
    end
  else
    for I := 0 to Rect.Height - 1 do
    begin // Copy one scanline at a time.
      StartPos := ((Rect.Top + I) * FPhysicalSize.X + Rect.Left) * SizeOf(TScreenColor);
      BytesToCopy := Rect.Width * SizeOf(TScreenColor);

      FDataPort.Write(Pointer(PtrUInt(FScreenBuffer) + Cardinal(StartPos)), BytesToCopy);
    end;
end;

function TDisplay.ReadPixel(const X, Y: Integer): TIntColor;
var
  SrcColor: PScreenColor;
begin
  SrcColor := Pointer(PtrUInt(FScreenBuffer) + (Cardinal(Y) * Cardinal(FPhysicalSize.X) + Cardinal(X)) *
    SizeOf(TScreenColor));

  TIntColorRec(Result).Red := SrcColor.Red;
  TIntColorRec(Result).Green := SrcColor.Green;
  TIntColorRec(Result).Blue := SrcColor.Blue;
  TIntColorRec(Result).Alpha := 255;
end;

procedure TDisplay.WritePixel(const X, Y: Integer; const Color: TIntColor);
var
  DestColor: PScreenColor;
begin
  DestColor := Pointer(PtrUInt(FScreenBuffer) + (Cardinal(Y) * Cardinal(FPhysicalSize.X) + Cardinal(X)) *
    SizeOf(TScreenColor));

  DestColor.Red := TIntColorRec(Color).Red;
  DestColor.Green := TIntColorRec(Color).Green;
  DestColor.Blue := TIntColorRec(Color).Blue;
end;

function TDisplay.GetScanline(const Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FPhysicalSize.Y) then
    Result := Pointer(PtrUInt(FScreenBuffer) + Cardinal(Index) * Cardinal(FPhysicalSize.X) * SizeOf(TScreenColor))
  else
    Result := nil;
end;

end.
