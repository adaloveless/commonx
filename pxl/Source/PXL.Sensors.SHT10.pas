unit PXL.Sensors.SHT10;
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
  TSensorSHT10 = class(TCustomSensor)
  private const
    SensorStartupTime = 11; // ms
    MeasurementTimeout = 500 * 1000; // us

    CommandMeasureTemperature = $03;
    CommandMeasureHumidity = $05;
  private
    FSystemCore: TCustomSystemCore;
    FGPIO: TCustomGPIO;
    FClockPin: Integer;
    FDataPin: Integer;

    function BusReadByte(const DataAck: Boolean): Integer;
    procedure BusWriteByte(const Value: Integer);
    procedure BeginTransmission;
    procedure WaitForMeasurement;
    function ReadMeasurement(const MeasurementCommand: Integer): Integer;
  public
    constructor Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const AClockPin,
      ADataPin: Integer);
    destructor Destroy; override;

    function ReadTemperatureRaw: Integer; inline;
    function ReadHumidityRaw: Integer; inline;

    procedure ReadTemperatureAndHumidity(out Temperature, Humidity: Single);

    property SystemCore: TCustomSystemCore read FSystemCore;
    property GPIO: TCustomGPIO read FGPIO;

    property ClockPin: Integer read FClockPin;
    property DataPin: Integer read FDataPin;
  end;

  ESensorFailedHighAck = class(ESensorDataRead);
  ESensorFailedLowAck = class(ESensorDataRead);
  ESensorMeasurementTimeout = class(ESensorGeneric);
  ESensorChecksumFailure = class(ESensorGeneric);

resourcestring
  SSensorFailedHighAck = 'Sensor did not acknowledge data high.';
  SSensorFailedLowAck = 'Sensor did not acknowledge data low.';
  SSensorMeasurementTimedOut = 'Sensor measurement timed out.';
  SSensorChecksumFailure = 'Received data from sensor failed checksum verification: received 0x%x, expected 0x%x.';

implementation

const
  CRCLookup: array[0..255] of Byte = (0, 49, 98, 83, 196, 245, 166, 151, 185, 136, 219, 234, 125, 76, 31, 46, 67, 114,
    33, 16, 135, 182, 229, 212, 250, 203, 152, 169, 62, 15, 92, 109, 134, 183, 228, 213, 66, 115, 32, 17, 63, 14, 93,
    108, 251, 202, 153, 168, 197, 244, 167, 150, 1, 48, 99, 82, 124, 77, 30, 47, 184, 137, 218, 235, 61, 12, 95, 110,
    249, 200, 155, 170, 132, 181, 230, 215, 64, 113, 34, 19, 126, 79, 28, 45, 186, 139, 216, 233, 199, 246, 165, 148,
    3, 50, 97, 80, 187, 138, 217, 232, 127, 78, 29, 44, 2, 51, 96, 81, 198, 247, 164, 149, 248, 201, 154, 171, 60, 13,
    94, 111, 65, 112, 35, 18, 133, 180, 231, 214, 122, 75, 24, 41, 190, 143, 220, 237, 195, 242, 161, 144, 7, 54, 101,
    84, 57, 8, 91, 106, 253, 204, 159, 174, 128, 177, 226, 211, 68, 117, 38, 23, 252, 205, 158, 175, 56, 9, 90, 107,
    69, 116, 39, 22, 129, 176, 227, 210, 191, 142, 221, 236, 123, 74, 25, 40, 6, 55, 100, 85, 194, 243, 160, 145, 71,
    118, 37, 20, 131, 178, 225, 208, 254, 207, 156, 173, 58, 11, 88, 105, 4, 53, 102, 87, 192, 241, 162, 147, 189, 140,
    223, 238, 121, 72, 27, 42, 193, 240, 163, 146, 5, 52, 103, 86, 120, 73, 26, 43, 188, 141, 222, 239, 130, 179, 224,
    209, 70, 119, 36, 21, 59, 10, 89, 104, 255, 206, 157, 172);

function CalculateCRC(const CurCRC, Value: Integer): Integer; inline;
begin
  Result := CRCLookup[Value xor CurCRC];
end;

function InvertCRC(const Value: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to 7 do
  begin
    Result := Result shl 1;

    if Value and (1 shl I) > 0 then
      Result := Result or 1;
  end;
end;

constructor TSensorSHT10.Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const AClockPin,
  ADataPin: Integer);
begin
  inherited Create;

  FSystemCore := ASystemCore;
  if FSystemCore = nil then
    raise ESensorNoSystemCore.Create(SSensorNoSystemCore);

  FGPIO := AGPIO;
  if FGPIO = nil then
    raise ESensorNoGPIO.Create(SSensorNoGPIO);

  FClockPin := AClockPin;
  FDataPin := ADataPin;

  FGPIO.PinMode[FClockPin] := TPinMode.Output;
  FGPIO.PinValue[FClockPin] := TPinValue.Low;

  FGPIO.PinMode[FDataPin] := TPinMode.Output;
  FGPIO.PinValue[FDataPin] := TPinValue.High;

  FSystemCore.Delay(SensorStartupTime * 1000);
end;

destructor TSensorSHT10.Destroy;
begin
  FGPIO.PinMode[FDataPin] := TPinMode.Input;
  FGPIO.PinMode[FClockPin] := TPinMode.Input;

  inherited;
end;

function TSensorSHT10.BusReadByte(const DataAck: Boolean): Integer;
var
  I: Integer;
begin
  FGPIO.PinMode[FDataPin] := TPinMode.Input;

  Result := 0;

  for I := 0 to 7 do
  begin
    FGPIO.PinValue[FClockPin] := TPinValue.High;

    Result := Result shl 1;

    if FGPIO.PinValue[FDataPin] = TPinValue.High then
      Result := Result or 1;

    FGPIO.PinValue[FClockPin] := TPinValue.Low;
  end;

  if DataAck then
  begin
    FGPIO.PinMode[FDataPin] := TPinMode.Output;
    FGPIO.PinValue[FDataPin] := TPinValue.Low;
  end;

  FGPIO.PinValue[FClockPin] := TPinValue.High;
  FSystemCore.Delay(5);
  FGPIO.PinValue[FClockPin] := TPinValue.Low;

  FGPIO.PinMode[FDataPin] := TPinMode.Input;
end;

procedure TSensorSHT10.BusWriteByte(const Value: Integer);
var
  I, TempValue: Integer;
begin
  FGPIO.PinMode[FDataPin] := TPinMode.Output;

  TempValue := Value;

  for I := 0 to 7 do
  begin
    if TempValue and $80 > 0 then
      FGPIO.PinValue[FDataPin] := TPinValue.High
    else
    begin
      FGPIO.PinValue[FDataPin] := TPinValue.Low;
    end;

    TempValue := TempValue shl 1;

    FGPIO.PinValue[FClockPin] := TPinValue.High;
    FGPIO.PinValue[FClockPin] := TPinValue.Low;
  end;

  FGPIO.PinMode[FDataPin] := TPinMode.Input;
  FGPIO.PinValue[FClockPin] := TPinValue.High;

  if FGPIO.PinValue[FDataPin] <> TPinValue.Low then
    raise ESensorFailedLowAck.Create(SSensorFailedLowAck);

  FGPIO.PinValue[FClockPin] := TPinValue.Low;

  FSystemCore.Delay(5);

  if FGPIO.PinValue[FDataPin] <> TPinValue.High then
    raise ESensorFailedHighAck.Create(SSensorFailedHighAck);
end;

procedure TSensorSHT10.BeginTransmission;
begin
  FGPIO.PinMode[FClockPin] := TPinMode.Output;
  FGPIO.PinValue[FClockPin] := TPinValue.High;

  FGPIO.PinMode[FDataPin] := TPinMode.Output;
  FGPIO.PinValue[FDataPin] := TPinValue.High;

  FGPIO.PinValue[FDataPin] := TPinValue.Low;
  FGPIO.PinValue[FClockPin] := TPinValue.Low;

  FGPIO.PinValue[FClockPin] := TPinValue.High;
  FGPIO.PinValue[FDataPin] := TPinValue.High;

  FGPIO.PinValue[FClockPin] := TPinValue.Low;
end;

procedure TSensorSHT10.WaitForMeasurement;
var
  StartTime: UInt64;
begin
  FGPIO.PinMode[FDataPin] := TPinMode.Input;

  StartTime := FSystemCore.GetTickCount;

  while FGPIO.PinValue[FDataPin] = TPinValue.High do
    if FSystemCore.TicksInBetween(StartTime, FSystemCore.GetTickCount) >= MeasurementTimeout then
      raise ESensorMeasurementTimeout.Create(SSensorMeasurementTimedOut);
end;

function TSensorSHT10.ReadMeasurement(const MeasurementCommand: Integer): Integer;
var
  SensorCRC, CurrentCRC: Integer;
begin
  BeginTransmission;
  BusWriteByte(MeasurementCommand);
  WaitForMeasurement;

  Result := (BusReadByte(True) shl 8) or BusReadByte(True);

  SensorCRC := BusReadByte(False);

  CurrentCRC := CalculateCRC(0, MeasurementCommand);
  CurrentCRC := CalculateCRC(CurrentCRC, Result shr 8);
  CurrentCRC := InvertCRC(CalculateCRC(CurrentCRC, Result and $FF));

  if SensorCRC <> CurrentCRC then
    raise ESensorChecksumFailure.CreateFmt(SSensorChecksumFailure, [SensorCRC, CurrentCRC]);
end;

function TSensorSHT10.ReadTemperatureRaw: Integer;
begin
  Result := ReadMeasurement(CommandMeasureTemperature);
end;

function TSensorSHT10.ReadHumidityRaw: Integer;
begin
  Result := ReadMeasurement(CommandMeasureHumidity);
end;

procedure TSensorSHT10.ReadTemperatureAndHumidity(out Temperature, Humidity: Single);
const
  D1 = -39.66;
  D2 = 0.01;
  C1 = -2.0468;
  C2 = 0.0367;
  C3 = 1.5955E-6;
  T1 = 0.01;
  T2 = 0.00008;
var
  RH, RHLinear: Single;
begin
  Temperature := (ReadTemperatureRaw * D2) + D1;

  RH := ReadHumidityRaw;
  RHLinear := C1 + C2 * RH + C3 * Sqr(RH);

  Humidity := (Temperature - 25.0) * (T1 + T2 * RH) + RHLinear;
end;

end.
