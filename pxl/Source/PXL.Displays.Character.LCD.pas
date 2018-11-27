unit PXL.Displays.Character.LCD;
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

{ Comment this out to prevent the component from releasing the pins in destructor. }
{$DEFINE LCD_RESET_PINS_AFTER_DONE}

uses
  PXL.TypeDef, PXL.Types, PXL.Boards.Types;

type
  TCharacterMask = array[0..7] of Byte;

  TCharacterLCD = class
  public type
    TDataPins = record
    case Integer of
      0: (D0: Integer;
          D1: Integer;
          D2: Integer;
          D3: Integer;
          D4: Integer;
          D5: Integer;
          D6: Integer;
          D7: Integer);
      1: (Pins: array[0..7] of Integer);
    end;
  private
    FGPIO: TCustomGPIO;
    FSystemCore: TCustomSystemCore;

    FPinRS: Integer;
    FPinRW: Integer;
    FPinEnable: Integer;

    FDataPins: TDataPins;

    FScreenSize: TPoint2px;
    FCursorPos: TPoint2px;

    FDisplayFunction: Integer;
    FDisplayControl: Integer;
    FDisplayMode: Integer;

    procedure PrepareDataPins;
    procedure PrepareDisplay(const AScreenSize: TPoint2px);

    procedure PulsePinEnable;
    procedure WriteRawBits4(const Value: Integer);
    procedure WriteRawBits8(const Value: Integer);
    procedure WriteCustomValue(const Value: Integer; const PinValueRS: TPinValue);

    procedure WriteCommand(const Value: Integer); inline;
    procedure WriteData(const Value: Integer); inline;

    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetCursorVisible: Boolean;
    procedure SetCursorVisible(const Value: Boolean);
    function GetCursorBlinking: Boolean;
    procedure SetCursorBlinking(const Value: Boolean);
    procedure SetCursorPos(const Value: TPoint2px);
  protected
    procedure Delay(const Microseconds: Integer); inline;
  public
    { Creates instance of TCharacterLCD in 8-bit data mode. Note that "RW" pin is not really necessary and can be
      connected to Ground on the board, while specifying "-1" for APinRW here. }
    constructor Create(const ASystemCore: TCustomSystemCore; const AScreenSize: TPoint2px; const AGPIO: TCustomGPIO;
      const APinRS, APinRW, APinEnable, APinD0, APinD1, APinD2, APinD3, APinD4, APinD5, APinD6,
      APinD7: Integer); overload;

    { Creates instance of TCharacterLCD in 4-bit data mode using as few cables as possible. Note that required data
      pins are actually last (and not the first ones!) four pins from D5 to D7. }
    constructor Create(const ASystemCore: TCustomSystemCore; const AScreenSize: TPoint2px; const AGPIO: TCustomGPIO;
      const APinRS, APinEnable, APinD4, APinD5, APinD6, APinD7: Integer); overload;
    destructor Destroy; override;

    procedure Print(const Text: StdString);

    procedure Clear;
    procedure ResetCursor;

    procedure SetCharacterMask(const Location: Integer; const Mask: TCharacterMask);

    property GPIO: TCustomGPIO read FGPIO;
    property SystemCore: TCustomSystemCore read FSystemCore;

    property PinRS: Integer read FPinRS;
    property PinRW: Integer read FPinRW;

    property PinEnable: Integer read FPinEnable;

    property PinD0: Integer read FDataPins.D0;
    property PinD1: Integer read FDataPins.D1;
    property PinD2: Integer read FDataPins.D2;
    property PinD3: Integer read FDataPins.D3;
    property PinD4: Integer read FDataPins.D4;
    property PinD5: Integer read FDataPins.D5;
    property PinD6: Integer read FDataPins.D6;
    property PinD7: Integer read FDataPins.D7;

    property ScreenSize: TPoint2px read FScreenSize;
    property Enabled: Boolean read GetEnabled write SetEnabled;

    property CursorVisible: Boolean read GetCursorVisible write SetCursorVisible;
    property CursorBlinking: Boolean read GetCursorBlinking write SetCursorBlinking;
    property CursorPos: TPoint2px read FCursorPos write SetCursorPos;
  end;

function CharacterMask(const Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8: Byte): TCharacterMask; inline;

implementation

uses
  SysUtils;

const
  // General Commands
  CMD_DISPLAY_CLEAR = $01;
  CMD_CURSOR_RESET = $02;
  CMD_DISPLAY_ENTRY_MODE = $04;
  CMD_DISPLAY_CONTROL = $08;
  CMD_DISPLAY_FUNCTION = $20;
  CMD_CURSOR_MASK = $40;
  CMD_CURSOR_SET = $80;

  // Display Entry Mode Flags
  CTRL_ENTRY_LEFT_TO_RIGHT = $02;

  // Display Control Flags
  CTRL_CURSOR_BLINKING = $01;
  CTRL_CURSOR_VISIBLE = $02;
  CTRL_DISPLAY_ENABLED = $04;

  // Display Function Flags
//  CTRL_DISPLAY_BIGFONT = $04;
  CTRL_DISPLAY_MULTILINE = $08;
  CTRL_INTERFACE_8BITS = $10;

function CharacterMask(const Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8: Byte): TCharacterMask; inline;
begin
  Result[0] := Row1;
  Result[1] := Row2;
  Result[2] := Row3;
  Result[3] := Row4;
  Result[4] := Row5;
  Result[5] := Row6;
  Result[6] := Row7;
  Result[7] := Row8;
end;

constructor TCharacterLCD.Create(const ASystemCore: TCustomSystemCore; const AScreenSize: TPoint2px;
  const AGPIO: TCustomGPIO; const APinRS, APinRW, APinEnable, APinD0, APinD1, APinD2, APinD3, APinD4, APinD5, APinD6,
  APinD7: Integer);
begin
  inherited Create;

  FGPIO := AGPIO;
  FSystemCore := ASystemCore;

  FPinRS := APinRS;
  FPinRW := APinRW;

  FPinEnable := APinEnable;

  FDataPins.D0 := APinD0;
  FDataPins.D1 := APinD1;
  FDataPins.D2 := APinD2;
  FDataPins.D3 := APinD3;
  FDataPins.D4 := APinD4;
  FDataPins.D5 := APinD5;
  FDataPins.D6 := APinD6;
  FDataPins.D7 := APinD7;

  FGPIO.PinMode[FPinRS] := TPinMode.Output;

  if FPinRW <> -1 then
    FGPIO.PinMode[FPinRW] := TPinMode.Output;

  FGPIO.PinMode[FPinEnable] := TPinMode.Output;

  PrepareDataPins;

  FDisplayFunction := 0;

  if (FDataPins.D4 <> -1) and (FDataPins.D5 <> -1) and (FDataPins.D6 <> -1) and (FDataPins.D7 <> -1) then
    FDisplayFunction := FDisplayFunction or CTRL_INTERFACE_8BITS;

  PrepareDisplay(AScreenSize);
end;

constructor TCharacterLCD.Create(const ASystemCore: TCustomSystemCore; const AScreenSize: TPoint2px;
  const AGPIO: TCustomGPIO; const APinRS, APinEnable, APinD4, APinD5, APinD6, APinD7: Integer);
begin
  Create(ASystemCore, AScreenSize, AGPIO, APinRS, -1, APinEnable, APinD4, APinD5, APinD6, APinD7, -1, -1, -1, -1);
end;

destructor TCharacterLCD.Destroy;
{$IFDEF LCD_RESET_PINS_AFTER_DONE}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF LCD_RESET_PINS_AFTER_DONE}
  for I := 0 to 7 do
    if FDataPins.Pins[I] <> -1 then
      FGPIO.PinMode[FDataPins.Pins[I]] := TPinMode.Input;

  FGPIO.PinMode[FPinEnable] := TPinMode.Input;

  if FPinRW <> -1 then
    FGPIO.PinMode[FPinRW] := TPinMode.Input;

  FGPIO.PinMode[FPinRS] := TPinMode.Input;
{$ENDIF}

  inherited;
end;

procedure TCharacterLCD.Delay(const Microseconds: Integer);
begin
  if FSystemCore <> nil then
    FSystemCore.Delay(4500);
end;

procedure TCharacterLCD.PrepareDataPins;
var
  I: Integer;
begin
  for I := 0 to 7 do
    if FDataPins.Pins[I] <> -1 then
      FGPIO.PinMode[FDataPins.Pins[I]] := TPinMode.Output;
end;

procedure TCharacterLCD.PrepareDisplay(const AScreenSize: TPoint2px);
begin
  FScreenSize := AScreenSize;
  FCursorPos := ZeroPoint2px;

  if FScreenSize.Y > 1 then
    FDisplayFunction := FDisplayFunction or CTRL_DISPLAY_MULTILINE;

  FGPIO.PinValue[FPinRS] := TPinValue.Low;
  FGPIO.PinValue[FPinEnable] := TPinValue.Low;

  if FDisplayFunction and CTRL_INTERFACE_8BITS > 0 then
  begin
    WriteCommand(CMD_DISPLAY_FUNCTION or FDisplayFunction);
    Delay(4500);

    WriteCommand(CMD_DISPLAY_FUNCTION or FDisplayFunction);
    Delay(150);

    WriteCommand(CMD_DISPLAY_FUNCTION or FDisplayFunction);
  end
  else
  begin
    WriteRawBits4($03);
    Delay(4500);

    WriteRawBits4($03);
    Delay(4500);

    WriteRawBits4($03);
    Delay(150);

    WriteRawBits4($02);
  end;

  WriteCommand(CMD_DISPLAY_FUNCTION or FDisplayFunction);

  FDisplayControl := CTRL_DISPLAY_ENABLED or CTRL_CURSOR_VISIBLE or CTRL_CURSOR_BLINKING;
  WriteCommand(CMD_DISPLAY_CONTROL or FDisplayControl);

  Clear;

  FDisplayMode := CTRL_ENTRY_LEFT_TO_RIGHT;
  WriteCommand(CMD_DISPLAY_ENTRY_MODE or FDisplayMode);
end;

procedure TCharacterLCD.PulsePinEnable;
begin
  FGPIO.PinValue[FPinEnable] := TPinValue.Low;
  Delay(1);

  FGPIO.PinValue[FPinEnable] := TPinValue.High;
  Delay(1);

  FGPIO.PinValue[FPinEnable] := TPinValue.Low;
  Delay(100);
end;

procedure TCharacterLCD.WriteRawBits4(const Value: Integer);
var
  I: Integer;
begin
  for I := 0 to 3 do
    FGPIO.PinValue[FDataPins.Pins[I]] := TPinValue((Value shr I) and $01);

  PulsePinEnable;
end;

procedure TCharacterLCD.WriteRawBits8(const Value: Integer);
var
  I: Integer;
begin
  for I := 0 to 7 do
    FGPIO.PinValue[FDataPins.Pins[I]] := TPinValue((Value shr I) and $01);

  PulsePinEnable;
end;

procedure TCharacterLCD.WriteCustomValue(const Value: Integer; const PinValueRS: TPinValue);
begin
  FGPIO.PinValue[FPinRS] := PinValueRS;

  if FPinRW <> -1 then
    FGPIO.PinValue[FPinRW] := TPinValue.Low;

  if FDisplayFunction and CTRL_INTERFACE_8BITS > 0 then
    WriteRawBits8(Value)
  else
  begin
    WriteRawBits4(Value shr 4);
    WriteRawBits4(Value);
  end;
end;

procedure TCharacterLCD.WriteCommand(const Value: Integer);
begin
  WriteCustomValue(Value, TPinValue.Low);
end;

procedure TCharacterLCD.WriteData(const Value: Integer);
begin
  WriteCustomValue(Value, TPinValue.High);
end;

function TCharacterLCD.GetEnabled: Boolean;
begin
  Result := FDisplayControl and CTRL_DISPLAY_ENABLED > 0;
end;

procedure TCharacterLCD.SetEnabled(const Value: Boolean);
begin
  if GetEnabled <> Value then
  begin
    if Value then
      FDisplayControl := FDisplayControl or CTRL_DISPLAY_ENABLED
    else
      FDisplayControl := FDisplayControl and (not CTRL_DISPLAY_ENABLED);

    WriteCommand(CMD_DISPLAY_CONTROL or FDisplayControl);
  end;
end;

function TCharacterLCD.GetCursorVisible: Boolean;
begin
  Result := FDisplayControl and CTRL_CURSOR_VISIBLE > 0;
end;

procedure TCharacterLCD.SetCursorVisible(const Value: Boolean);
begin
  if GetCursorVisible <> Value then
  begin
    if Value then
      FDisplayControl := FDisplayControl or CTRL_CURSOR_VISIBLE
    else
      FDisplayControl := FDisplayControl and (not CTRL_CURSOR_VISIBLE);

    WriteCommand(CMD_DISPLAY_CONTROL or FDisplayControl);
  end;
end;

function TCharacterLCD.GetCursorBlinking: Boolean;
begin
  Result := FDisplayControl and CTRL_CURSOR_BLINKING > 0;
end;

procedure TCharacterLCD.SetCursorBlinking(const Value: Boolean);
begin
  if GetCursorBlinking <> Value then
  begin
    if Value then
      FDisplayControl := FDisplayControl or CTRL_CURSOR_BLINKING
    else
      FDisplayControl := FDisplayControl and (not CTRL_CURSOR_BLINKING);

    WriteCommand(CMD_DISPLAY_CONTROL or FDisplayControl);
  end;
end;

procedure TCharacterLCD.SetCursorPos(const Value: TPoint2px);
const
  RowOffsets: array[0..3] of Integer = ($00, $40, $14, $54);
begin
  if FCursorPos <> Value then
  begin
    FCursorPos.X := Saturate(Value.X, 0, FScreenSize.X - 1);
    FCursorPos.Y := Saturate(Value.Y, 0, FScreenSize.Y - 1);

    WriteCommand(CMD_CURSOR_SET or (FCursorPos.X + RowOffsets[FCursorPos.Y]));
  end;
end;

procedure TCharacterLCD.Clear;
begin
  WriteCommand(CMD_DISPLAY_CLEAR);
  Delay(2000);
end;

procedure TCharacterLCD.ResetCursor;
begin
  FCursorPos := ZeroPoint2px;
  WriteCommand(CMD_CURSOR_RESET);
  Delay(2000);
end;

procedure TCharacterLCD.SetCharacterMask(const Location: Integer; const Mask: TCharacterMask);
var
  I: Integer;
begin
  WriteCommand(CMD_CURSOR_MASK or ((Location and $07) shl 3));

  for I := 0 to 7 do
    WriteData(Mask[I]);
end;

procedure TCharacterLCD.Print(const Text: StdString);
var
  I: Integer;
begin
  for I := 1 to Length(Text) do
    WriteData(Ord(Text[I]));
end;

end.
