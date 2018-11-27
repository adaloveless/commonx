unit PXL.Displays.SSD1351;
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
  public const
    OLED128x128: TPoint2px = (X: 128; Y: 128);
    OLED128x96: TPoint2px = (X: 128; Y: 96);
  private
    FScreenSize: TPoint2px;

    procedure SetWriteWindow(const WriteRect: TIntRect);
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
  end;

implementation

uses
  SysUtils, Math;

const
  CMD_SET_COLUMN = $15;
  CMD_SET_ROW = $75;
  CMD_WRITE_RAM = $5C;
  CMD_SET_REMAP = $A0;
  CMD_START_LINE = $A1;
  CMD_DISPLAY_OFFSET = $A2;
  CMD_NORMAL_DISPLAY = $A6;
  CMD_FUNCTIONSELECT = $AB;
  CMD_DISPLAY_OFF = $AE;
  CMD_DISPLAY_ON = $AF;
  CMD_PRECHARGE = $B1;
  CMD_CLOCK_DIV = $B3;
  CMD_SET_VSL = $B4;
  CMD_SET_GPIO = $B5;
  CMD_PRECHARGE2 = $B6;
  CMD_VCOMH = $BE;
  CMD_CONTRAST_ABC = $C1;
  CMD_CONTRAST_MASTER = $C7;
  CMD_MUX_RATIO = $CA;
  CMD_COMMAND_LOCK = $FD;

constructor TDisplay.Create(const AScreenSize: TPoint2px; const AGPIO: TCustomGPIO; const ADataPort: TCustomDataPort;
  const APinDC: Integer; const APinRST: Integer);
begin
  FScreenSize := AScreenSize;

  FPhysicalOrientation := TOrientation.Landscape;
  FPhysicalSize := FScreenSize;

  FScreenBufferSize := (FPhysicalSize.X * FPhysicalSize.Y) * SizeOf(Word);
  FScreenBuffer := AllocMem(FScreenBufferSize);

  inherited Create(AGPIO, ADataPort, APinDC, APinRST);
end;

procedure TDisplay.InitSequence;
begin
  WriteCommand(CMD_COMMAND_LOCK);
  WriteData($12);

  WriteCommand(CMD_COMMAND_LOCK);
  WriteData($B1);

  WriteCommand(CMD_DISPLAY_OFF);

  WriteCommand(CMD_CLOCK_DIV);
  WriteCommand($F1);

  WriteCommand(CMD_MUX_RATIO);
  WriteData(127);

  WriteCommand(CMD_SET_REMAP);
  WriteData($74);

  WriteCommand(CMD_SET_COLUMN);
  WriteData([$00, $7F]);

  WriteCommand(CMD_SET_ROW);
  WriteData([$00, $7F]);

  WriteCommand(CMD_START_LINE);

  if FScreenSize.Y = 96 then
    WriteData(96)
  else
    WriteData(0);

  WriteCommand(CMD_DISPLAY_OFFSET);
  WriteData($00);

  WriteCommand(CMD_SET_GPIO);
  WriteData($00);

  WriteCommand(CMD_FUNCTIONSELECT);
  WriteData($01);

  WriteCommand(CMD_PRECHARGE);
  WriteCommand($32);

  WriteCommand(CMD_VCOMH);
  WriteCommand($05);

  WriteCommand(CMD_NORMAL_DISPLAY);

  WriteCommand(CMD_CONTRAST_ABC);
  WriteData([$C8, $80, $C8]);

  WriteCommand(CMD_CONTRAST_MASTER);
  WriteData($0F);

  WriteCommand(CMD_SET_VSL);
  WriteData([$A0, $B5, $55]);

  WriteCommand(CMD_PRECHARGE2);
  WriteData($01);

  WriteCommand(CMD_DISPLAY_ON);
end;

procedure TDisplay.SetWriteWindow(const WriteRect: TIntRect);
begin
  WriteCommand(CMD_SET_COLUMN);
  WriteData([WriteRect.Left, WriteRect.Right - 1]);

  WriteCommand(CMD_SET_ROW);
  WriteData([WriteRect.Top, WriteRect.Bottom - 1]);

  WriteCommand(CMD_WRITE_RAM);

  FGPIO.PinValue[FPinDC] := TPinValue.High;
end;

procedure TDisplay.PresentBuffer(const Rect: TIntRect);
var
  I, StartPos, BytesToCopy, BytesTotal: Integer;
begin
  SetWriteWindow(Rect);

  BytesTotal := Rect.Width * Rect.Height * SizeOf(Word);
  StartPos := (Rect.Top * FPhysicalSize.X + Rect.Left) * SizeOf(Word);

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
      StartPos := ((Rect.Top + I) * FPhysicalSize.X + Rect.Left) * SizeOf(Word);
      BytesToCopy := Rect.Width * SizeOf(Word);

      FDataPort.Write(Pointer(PtrUInt(FScreenBuffer) + Cardinal(StartPos)), BytesToCopy);
    end;
end;

function TDisplay.ReadPixel(const X, Y: Integer): TIntColor;
var
  Value: PWord;
  Color: Word;
begin
  Value := PWord(PtrUInt(FScreenBuffer) + (Cardinal(Y) * Cardinal(FPhysicalSize.X) + Cardinal(X)) * SizeOf(Word));
  Color := ((Value^ and $FF) shl 8) or (Value^ shr 8);

  Result :=
    ((Cardinal(Color and $1F) * 255) div 31) or
    (((Cardinal((Color shr 5) and $3F) * 255) div 63) shl 8) or
    (((Cardinal((Color shr 11) and $1F) * 255) div 31) shl 16) or
    $FF000000;
end;

procedure TDisplay.WritePixel(const X, Y: Integer; const Color: TIntColor);
var
  Value: Word;
begin
  Value := ((Color shr 3) and $1F) or (((Color shr 10) and $3F) shl 5) or (((Color shr 19) and $1F) shl 11);

  PWord(PtrUInt(FScreenBuffer) + (Cardinal(Y) * Cardinal(FPhysicalSize.X) + Cardinal(X)) * SizeOf(Word))^ :=
    ((Value and $FF) shl 8) or (Value shr 8);
end;

function TDisplay.GetScanline(const Index: Integer): Pointer;
begin
  Result := Pointer(PtrUInt(FScreenBuffer) + Cardinal(Index) * Cardinal(FPhysicalSize.X) * SizeOf(Word));
end;

end.
