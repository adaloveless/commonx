unit PXL.Displays.ILI9340;
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
  CMD_DISPLAY_FUNCTION_CTRL = $B6;
  CMD_DISPLAY_ON = $29;
  CMD_FRAME_RATE_CTRL = $B1;
  CMD_GAMMA_SET = $26;
  CMD_MEM_ACCESS_CTRL = $36;
  CMD_MEM_WRITE = $2C;
  CMD_NEG_GAMMA_CORRECT = $E1;
  CMD_PAGE_ADDR_SET = $2B;
  CMD_POS_GAMMA_CORRECT = $E0;
  CMD_POWER_CTRL_1 = $C0;
  CMD_POWER_CTRL_2 = $C1;
  CMD_SET_PIXEL_FORMAT = $3A;
  CMD_SLEEP_OUT = $11;
  CMD_SW_RESET = $01;
  CMD_VCOM_CTRL_1 = $C5;
  CMD_VCOM_CTRL_2 = $C7;

constructor TDisplay.Create(const AGPIO: TCustomGPIO;
  const ADataPort: TCustomDataPort; const APinDC: Integer;
  const APinRST: Integer);
begin
  FPhysicalOrientation := TOrientation.InversePortrait;
  FPhysicalSize := Point2px(240, 320);

  FScreenBufferSize := (FPhysicalSize.X * FPhysicalSize.Y) * SizeOf(TScreenColor);
  FScreenBuffer := AllocMem(FScreenBufferSize);

  inherited Create(AGPIO, ADataPort, APinDC, APinRST);
end;

procedure TDisplay.InitSequence;
begin
  WriteCommand(CMD_SW_RESET);
  Sleep(5);

  WriteCommand($EF);
  WriteData([$03, $80, $02]);

  WriteCommand($CF);
  WriteData([$00, $C1, $30]);

  WriteCommand($ED);
  WriteData([$64, $03, $12, $81]);

  WriteCommand($E8);
  WriteData([$85, $00, $78]);

  WriteCommand($CB);
  WriteData([$39, $2C, $00, $34, $02]);

  WriteCommand($F7);
  WriteData($20);

  WriteCommand($EA);
  WriteData([$00, $00]);

  WriteCommand(CMD_POWER_CTRL_1);
  WriteData($23);

  WriteCommand(CMD_POWER_CTRL_2);
  WriteData($10);

  WriteCommand(CMD_VCOM_CTRL_1);
  WriteData($3E);
  WriteData($28);

  WriteCommand(CMD_VCOM_CTRL_2);
  WriteData($86);

  WriteCommand(CMD_MEM_ACCESS_CTRL);
  WriteData($48);

  WriteCommand(CMD_SET_PIXEL_FORMAT);
  WriteData($66);

  WriteCommand(CMD_FRAME_RATE_CTRL);
  WriteData([$00, $18]);

  WriteCommand(CMD_DISPLAY_FUNCTION_CTRL);
  WriteData([$08, $82, $27]);

  WriteCommand($F2);
  WriteData($00);

  WriteCommand(CMD_GAMMA_SET);
  WriteData($01);

  WriteCommand(CMD_POS_GAMMA_CORRECT);
  WriteData([$0F, $31, $2B, $0C, $0E, $08, $4E, $F1, $37, $07, $10, $03, $0E, $09, $00]);

  WriteCommand(CMD_NEG_GAMMA_CORRECT);
  WriteData([$00, $0E, $14, $03, $11, $07, $31, $C1, $48, $08, $0F, $0C, $31, $36, $0F]);

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
