unit PXL.Displays.PCB8544;
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
  Note: Nokia 5110 LCD display driver can only operate at SPI frequencies of 4 mHz or lower (default 8 mHz won't work).
}
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.Types, PXL.Boards.Types, PXL.Displays.Types;

type
  TDisplay = class(TCustomDrivenDisplay)
  public const
    Nokia84x48: TPoint2px = (X: 84; Y: 48);
  private
    FScreenSize: TPoint2px;
    FContrast: Integer;

    procedure SetContrast(const Value: Integer);
  protected
    procedure InitSequence; override;
    procedure PresentBuffer(const Rect: TIntRect); override;

    function ReadPixel(const X, Y: Integer): TIntColor; override;
    procedure WritePixel(const X, Y: Integer; const Color: TIntColor); override;

    function GetScanline(const Index: Integer): Pointer; override;
  public
    constructor Create(const AScreenSize: TPoint2px; const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort;
      const APinDC: Integer; const APinRST: Integer = -1);

    property ScreenSize: TPoint2px read FScreenSize;
    property Contrast: Integer read FContrast write SetContrast;
  end;

implementation

uses
  Math;

const
  CMD_DISPLAY_CONTROL = $08;
  CMD_FUNCTION_SET = $20;
  CMD_SET_BIAS = $10;
  CMD_SET_COL_ADDRESS = $40;
  CMD_SET_CONTRAST = $80;
  CMD_SET_ROW_ADDRESS = $80;

  MASK_DISPLAY_NORMAL = $4;
  MASK_EXTENDED_FUNCTION = $01;

constructor TDisplay.Create(const AScreenSize: TPoint2px; const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort;
  const APinDC, APinRST: Integer);
begin
  FScreenSize := AScreenSize;

  FPhysicalOrientation := TOrientation.Landscape;
  FPhysicalSize := FScreenSize;

  FScreenBufferSize := (FPhysicalSize.X * FPhysicalSize.Y) div 8;
  FScreenBuffer := AllocMem(FScreenBufferSize);

  FContrast := 60;

  inherited Create(AGPIO, ADataPort, APinDC, APinRST);
end;

procedure TDisplay.SetContrast(const Value: Integer);
var
  NewValue: Integer;
begin
  NewValue := Saturate(Value, 0, $7F);
  if FContrast <> NewValue then
  begin
    FContrast := NewValue;
    WriteCommand(CMD_SET_CONTRAST or FContrast);
  end;
end;

procedure TDisplay.InitSequence;
const
  DefaultBias = $04;
begin
  WriteCommand(CMD_FUNCTION_SET or MASK_EXTENDED_FUNCTION);
  WriteCommand(CMD_SET_BIAS or DefaultBias);
  WriteCommand(CMD_SET_CONTRAST or FContrast);

  WriteCommand(CMD_FUNCTION_SET);
  WriteCommand(CMD_DISPLAY_CONTROL or MASK_DISPLAY_NORMAL);
end;

procedure TDisplay.PresentBuffer(const Rect: TIntRect);
var
  I, StartPos, BytesToCopy: Integer;
begin
  if (Rect.Left = 0) and (Rect.Top = 0) and (Rect.Right = FPhysicalSize.X) and (Rect.Bottom = FPhysicalSize.Y) then
  begin // Full burst copy.
    WriteCommand(CMD_SET_COL_ADDRESS or 0);
    WriteCommand(CMD_SET_ROW_ADDRESS or 0);

    if FPinDC <> -1 then
      FGPIO.PinValue[FPinDC] := TPinValue.High;

    for I := 0 to FScreenBufferSize div MaxSPITransferSize do
    begin
      StartPos := I * MaxSPITransferSize;
      BytesToCopy := Min(FScreenBufferSize - StartPos, MaxSPITransferSize);

      if BytesToCopy > 0 then
        FDataPort.Write(Pointer(PtrUInt(FScreenBuffer) + Cardinal(StartPos)), BytesToCopy)
      else
        Break;
    end;
  end
  else
    for I := 0 to Rect.Height - 1 do
    begin // Copy one scanline at a time.
      WriteCommand(CMD_SET_COL_ADDRESS or ((Rect.Top + I) div 8));
      WriteCommand(CMD_SET_ROW_ADDRESS or Rect.Left);

      if FPinDC <> -1 then
        FGPIO.PinValue[FPinDC] := TPinValue.High;

      StartPos := ((Rect.Top + I) div 8) * FPhysicalSize.X + Rect.Left;
      BytesToCopy := Rect.Width;

      FDataPort.Write(Pointer(PtrUInt(FScreenBuffer) + Cardinal(StartPos)), BytesToCopy);
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

  if PixelToGray(Color) > 127 then
    Location^ := Location^ or (1 shl (Y mod 8))
  else
    Location^ := Location^ and ($FF xor (1 shl (Y mod 8)));
end;

function TDisplay.GetScanline(const Index: Integer): Pointer;
begin
  Result := Pointer(PtrUInt(FScreenBuffer) + (Cardinal(Index) div 8) * Cardinal(FPhysicalSize.X));
end;

end.
