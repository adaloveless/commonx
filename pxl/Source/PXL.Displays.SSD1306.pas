unit PXL.Displays.SSD1306;
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
  TDisplay = class(TCustomDrivenDualDisplay)
  public const
    OLED128x64: TPoint2px = (X: 128; Y: 64);
    OLED128x32: TPoint2px = (X: 128; Y: 32);
    OLED96x16: TPoint2px = (X: 96; Y: 16);
    OLED64x48: TPoint2px = (X: 64; Y: 48);
  private
    FScreenSize: TPoint2px;
    FInternalVCC: Boolean;

    procedure SetWriteWindow(const WriteRect: TIntRect);
  protected
    procedure InitSequence; override;
    procedure PresentBuffer(const Rect: TIntRect); override;

    function ReadPixel(const X, Y: Integer): TIntColor; override;
    procedure WritePixel(const X, Y: Integer; const Color: TIntColor); override;

    function GetScanline(const Index: Integer): Pointer; override;
  public
    constructor Create(const AScreenSize: TPoint2px; const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort;
      const APinDC: Integer; const APinRST: Integer = PinNumberUnused; const AAddress: Integer = PinNumberUnused;
      const AInternalVCC: Boolean = True);

    property ScreenSize: TPoint2px read FScreenSize;
  end;

implementation

uses
  SysUtils, Math;

const
  CMD_CHARGE_PUMP = $8D;
  CMD_COLUMN_ADDRESS = $21;
  CMD_COM_SCAN_DEC = $C8;
  CMD_DISPLAY_ALL_ON_RESUME = $A4;
  CMD_DISPLAY_OFF = $AE;
  CMD_DISPLAY_ON = $AF;
  CMD_MEMORY_MODE = $20;
  CMD_NORMAL_DISPLAY = $A6;
  CMD_PAGE_ADDRESS   = $22;
  CMD_SEGMENT_REMAP = $A0;
  CMD_SET_COM_PINS = $DA;
  CMD_SET_CONTRAST = $81;
  CMD_SET_DISPLAY_CLOCK_DIV = $D5;
  CMD_SET_DISPLAY_OFFSET = $D3;
  CMD_SET_MULTIPLEX = $A8;
  CMD_SET_PRECHARGE = $D9;
  CMD_SET_START_LINE = $40;
  CMD_SET_VCOM_DETECT = $DB;

  OrderedDitherMatrix: array[0..63] of Integer = (0, 32, 8, 40, 2, 34, 10, 42, 48, 16, 56, 24, 50, 18, 58, 26, 12, 44,
    4, 36, 14, 46, 6, 38, 60, 28, 52, 20, 62, 30, 54, 22, 3, 35, 11, 43, 1, 33, 9, 41, 51, 19, 59, 27, 49, 17, 57, 25,
    15, 47, 7, 39, 13, 45, 5, 37, 63, 31, 55, 23, 61, 29, 53, 21);

constructor TDisplay.Create(const AScreenSize: TPoint2px; const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort;
  const APinDC, APinRST, AAddress: Integer; const AInternalVCC: Boolean);
begin
  FScreenSize := AScreenSize;

  FPhysicalOrientation := TOrientation.Landscape;
  FPhysicalSize := FScreenSize;

  FScreenBufferSize := (FPhysicalSize.X * FPhysicalSize.Y) div 8;
  FScreenBuffer := AllocMem(FScreenBufferSize);

  FInternalVCC := AInternalVCC;

  inherited Create(AGPIO, ADataPort, APinDC, APinRST, AAddress);
end;

procedure TDisplay.InitSequence;
begin
  WriteCommand(CMD_DISPLAY_OFF);
  Sleep(5);

  WriteCommand([CMD_SET_DISPLAY_CLOCK_DIV, $80, CMD_SET_MULTIPLEX]);

  if FScreenSize = OLED128x32 then
    WriteCommand($1F)
  else if FScreenSize = OLED96x16 then
    WriteCommand($0F)
  else if FScreenSize = OLED64x48 then
    WriteCommand($2F)
  else
    WriteCommand($3F);

  WriteCommand([CMD_SET_DISPLAY_OFFSET, $00, CMD_SET_START_LINE or $00, CMD_CHARGE_PUMP]);

  if FInternalVCC then
    WriteCommand($14)
  else
    WriteCommand($10);

  WriteCommand([CMD_MEMORY_MODE, $00, CMD_SEGMENT_REMAP or $01, CMD_COM_SCAN_DEC, CMD_SET_COM_PINS]);

  if (FScreenSize = OLED128x32) or (FScreenSize = OLED96x16) then
    WriteCommand($02)
  else
    WriteCommand($12);

  WriteCommand(CMD_SET_CONTRAST);

  if (FScreenSize = OLED128x32) or (FScreenSize = OLED64x48) then
    WriteCommand($8F)
  else if FScreenSize = OLED96x16 then
  begin
    if FInternalVCC then
      WriteCommand($AF)
    else
      WriteCommand($10);
  end
  else
  begin
    if FInternalVCC then
      WriteCommand($CF)
    else
      WriteCommand($9F);
  end;

  WriteCommand(CMD_SET_PRECHARGE);

  if FInternalVCC then
    WriteCommand($F1)
  else
    WriteCommand($22);

  WriteCommand([CMD_SET_VCOM_DETECT, $40, CMD_DISPLAY_ALL_ON_RESUME, CMD_NORMAL_DISPLAY]);
  Sleep(5);
  WriteCommand(CMD_DISPLAY_ON);
end;

procedure TDisplay.SetWriteWindow(const WriteRect: TIntRect);
const
  VirtualWidth = 128;
var
  InitOffset: Integer;
begin
  InitOffset := (VirtualWidth - FScreenSize.X) div 2;

  WriteCommand([CMD_COLUMN_ADDRESS, InitOffset, InitOffset + FScreenSize.X - 1, CMD_PAGE_ADDRESS, 0,
    (FPhysicalSize.Y div 8) - 1]);

  if (FPinDC <> -1) and (FDataPort is TCustomPortSPI) then
    FGPIO.PinValue[FPinDC] := TPinValue.High;
end;

procedure TDisplay.PresentBuffer(const Rect: TIntRect);
var
  I, StartPos, BytesToCopy, Page: Integer;
begin
  SetWriteWindow(Rect);

  if FAddress <> -1 then
    for I := 0 to FScreenBufferSize div MaxI2CTransferSize do
    begin // I2C
      StartPos := I * MaxI2CTransferSize;
      BytesToCopy := Min(FScreenBufferSize - StartPos, MaxI2CTransferSize);

      if BytesToCopy > 0 then
        TCustomPortI2C(FDataPort).WriteBlockData(DisplayDataID, Pointer(PtrUInt(FScreenBuffer) + Cardinal(StartPos)),
          BytesToCopy)
      else
        Break;
    end
  else
    for I := 0 to FScreenBufferSize div MaxSPITransferSize do
    begin // SPI
      StartPos := I * MaxSPITransferSize;
      BytesToCopy := Min(FScreenBufferSize - StartPos, MaxSPITransferSize);

      if BytesToCopy > 0 then
        FDataPort.Write(Pointer(PtrUInt(FScreenBuffer) + Cardinal(StartPos)), BytesToCopy)
      else
        Break;
    end;
end;

function TDisplay.ReadPixel(const X, Y: Integer): TIntColor;
var
  Location: PByte;
begin
  Location := Pointer(PtrUInt(FScreenBuffer) + (Cardinal(Y) div 8) * Cardinal(FPhysicalSize.X) + Cardinal(X));

  if Location^ and (1 shl (Y mod 8)) > 0 then
    Result := IntColorWhite
  else
    Result := IntColorTranslucentBlack;
end;

procedure TDisplay.WritePixel(const X, Y: Integer; const Color: TIntColor);
var
  Location: PByte;
begin
  Location := Pointer(PtrUInt(FScreenBuffer) + (Cardinal(Y) div 8) * Cardinal(FPhysicalSize.X) + Cardinal(X));

  if (PixelToGray(Color) div 4 >= OrderedDitherMatrix[((Y and $07) shl 3) + (X and $07)]) then
    Location^ := Location^ or (1 shl (Y mod 8))
  else
    Location^ := Location^ and ($FF xor (1 shl (Y mod 8)));
end;

function TDisplay.GetScanline(const Index: Integer): Pointer;
begin
  Result := Pointer(PtrUInt(FScreenBuffer) + (Cardinal(Index) div 8) * Cardinal(FPhysicalSize.X));
end;

end.
