unit PXL.Sensors.BMP180;
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
  SysUtils, PXL.Boards.Types, PXL.Sensors.Types;

type
  TSensorBMP180 = class(TCustomSensor)
  private type
    TCalibration = (AC1, AC2, AC3, AC4, AC5, AC6, B1, B2, MB, MC, MD);
    TCalibrationData = record
    case Integer of
      0: (Values: array[TCalibration] of Word);
      1: (AC1, AC2, AC3: SmallInt;
          AC4, AC5, AC6: Word;
          B1, B2, MB, MC, MD: SmallInt);
    end;
  public type
    TMode = (UltraLowPower, Standard, HighResolution, UltraHighResolution);
  public const
    DefaultAddress = $77;
  private
    FSystemCore: TCustomSystemCore;
    FDataPort: TCustomPortI2C;
    FAddress: Integer;
    FMode: TMode;
    FCalibrationData: TCalibrationData;

    function ReadWordValue(const Command: Byte): Word; inline;

    procedure TestChipID;
    procedure ReadCalibrationData;
    function ReadUncompensatedTemperature: Integer;
    function ReadUncompensatedPressure: Integer;

    function CalculateB5(const UT: Integer): Integer;
    function GetTemperature: Single; inline;
    function GetPressure: Single; inline;
    function GetAltitude: Single;
  public
    constructor Create(const ASystemCore: TCustomSystemCore; const ADataPort: TCustomPortI2C;
      const AAddress: Integer = DefaultAddress; const AMode: TMode = TMode.HighResolution);

    function GetTemperatureRaw: Integer;
    function GetPressureRaw: Integer;

    property DataPort: TCustomPortI2C read FDataPort;
    property Address: Integer read FAddress;
    property Mode: TMode read FMode;

    { Current temperature in "Celcius" units. }
    property Temperature: Single read GetTemperature;

    { Current atmospheric pressure in "kPa" units. }
    property Pressure: Single read GetPressure;

    { Current calculated altitude in meters. }
    property Altitude: Single read GetAltitude;
  end;

  ESensorCalibrationInvalid = class(ESensorGeneric);

resourcestring
  SSensorCalibrationInvalid = 'Sensor calibration data appears to be invalid.';

implementation

uses
  Math;

constructor TSensorBMP180.Create(const ASystemCore: TCustomSystemCore; const ADataPort: TCustomPortI2C;
  const AAddress: Integer; const AMode: TMode);
begin
  inherited Create;

  FSystemCore := ASystemCore;
  if FSystemCore = nil then
    raise ESensorNoSystemCore.Create(SSensorNoSystemCore);

  FDataPort := ADataPort;
  if FDataPort = nil then
    raise ESensorNoDataPort.Create(SSensorNoDataPort);

  FAddress := AAddress;
  if (FAddress < 0) or (FAddress > $7F) then
    raise ESensorInvalidAddress.Create(Format(SSensorInvalidAddress, [FAddress]));

  FMode := AMode;

  TestChipID;
  ReadCalibrationData;
end;

function TSensorBMP180.ReadWordValue(const Command: Byte): Word;
var
  TempValue: Word;
begin
  if not FDataPort.ReadWordData(Command, TempValue) then
    raise ESensorDataRead.Create(Format(SSensorDataRead, [SizeOf(Word)]));

  Result := (TempValue shr 8) or ((TempValue and $FF) shl 8);
end;

procedure TSensorBMP180.TestChipID;
const
  ExpectedID = $55;
var
  ChipID: Byte;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.ReadByteData($D0, ChipID) then
    raise ESensorDataRead.Create(Format(SSensorDataRead, [SizeOf(ChipID)]));

  if ChipID <> ExpectedID then
    raise ESensorInvalidChipID.Create(Format(SSensorInvalidChipID, [ExpectedID, ChipID]));
end;

procedure TSensorBMP180.ReadCalibrationData;
var
  I: TCalibration;
  Command: Byte;
begin
  FDataPort.SetAddress(FAddress);

  Command := $AA;

  for I := Low(TCalibration) to High(TCalibration) do
  begin
    FCalibrationData.Values[I] := ReadWordValue(Command);

    if (FCalibrationData.Values[I] = $0000) or (FCalibrationData.Values[I] = $FFFF) then
      raise ESensorCalibrationInvalid.Create(SSensorCalibrationInvalid);

    Inc(Command, 2);
  end;
end;

function TSensorBMP180.ReadUncompensatedTemperature: Integer;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.WriteByteData($F4, $2E) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [2]));

  FSystemCore.Delay(4500);

  Result := ReadWordValue($F6);
end;

function TSensorBMP180.ReadUncompensatedPressure: Integer;
var
  TempValue1: Word;
  TempValue2: Byte;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.WriteByteData($F4, $34 or (Ord(FMode) shl 6)) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [2]));

  case FMode of
    TMode.Standard:
      FSystemCore.Delay(7500);

    TMode.HighResolution:
      FSystemCore.Delay(13500);

    TMode.UltraHighResolution:
      FSystemCore.Delay(25500);
  else
    FSystemCore.Delay(4500);
  end;

  TempValue1 := ReadWordValue($F6);

  if not FDataPort.ReadByteData($F8, TempValue2) then
    raise ESensorDataRead.Create(Format(SSensorDataRead, [1]));

  Result := ((Cardinal(TempValue1) shl 8) or TempValue1) shr (8 - Ord(FMode));
end;

function TSensorBMP180.CalculateB5(const UT: Integer): Integer;
var
  X1, X2: Integer;
begin
  X1 := ((Int64(UT) - FCalibrationData.AC6) * FCalibrationData.AC5) div 32768;
  X2 := (Int64(FCalibrationData.MC) * 2048) div (Int64(X1) + FCalibrationData.MD);
  Result := X1 + X2;
end;

function TSensorBMP180.GetTemperatureRaw: Integer;
var
  UT, B5: Integer;
begin
  UT := ReadUncompensatedTemperature;
  B5 := CalculateB5(UT);
  Result := (B5 + 8) div 16;
end;

function TSensorBMP180.GetTemperature: Single;
begin
  Result := GetTemperatureRaw * 0.1;
end;

function TSensorBMP180.GetPressureRaw: Integer;
var
  UT, UP, B5, B6, X1, X2, X3, B3, P: Integer;
  B4, B7: Cardinal;
begin
  UT := ReadUncompensatedTemperature;
  UP := ReadUncompensatedPressure;
  B5 := CalculateB5(UT);
  B6 := B5 - 4000;
  X1 := (Int64(FCalibrationData.B2) * (Sqr(Int64(B6)) div 4096)) div 2048;
  X2 := (Int64(FCalibrationData.AC2) * B6) div 2048;
  X3 := X1 + X2;
  B3 := (((Int64(FCalibrationData.AC1) * 4 + X3) shl Ord(FMode)) + 2) div 4;
  X1 := (Int64(FCalibrationData.AC3) * B6) div 8192;
  X2 := (Int64(FCalibrationData.B1) * (Sqr(Int64(B6)) div 4096)) div 65536;
  X3 := ((Int64(X1) + X2) + 2) div 4;
  B4 := (Int64(FCalibrationData.AC4) * (Int64(X3) + 32768)) div 32768;
  B7 := (Int64(UP) - B3) * (50000 shr Ord(FMode));

  if B7 < $80000000 then
    P := (Int64(B7) * 2) div B4
  else
    P := (B7 div B4) * 2;

  X1 := Sqr(Int64(P) div 256);
  X1 := (Int64(X1) * 3038) div 65536;
  X2 := (-7357 * Int64(P)) div 65536;

  Result := P + ((X1 + X2 + 3791) div 16);
end;

function TSensorBMP180.GetPressure: Single;
begin
  Result := GetPressureRaw * 0.001;
end;

function TSensorBMP180.GetAltitude: Single;
const
  PressureAtSeaLevel = 101.325;
begin
  Result := 44330.0 * (1.0 - Power(GetPressure / PressureAtSeaLevel, 0.190294957));
end;

end.
