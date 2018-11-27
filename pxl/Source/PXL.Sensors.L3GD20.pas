unit PXL.Sensors.L3GD20;
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
  PXL.Types, PXL.Boards.Types, PXL.Sensors.Types;

type
  TSensorL3GD20 = class(TCustomSensor)
  public type
    TSensitivity = (Scale245, Scale500, Scale2000);
  public const
    DefaultAddress = $6B;
  private
    FDataPort: TCustomPortI2C;
    FAddress: Integer;
    FSensitivity: TSensitivity;
    FGyroCoefficient: Single;

    procedure TestChipID;
    procedure Configure;

    function GetGyroscope: TVector3;
  public
    constructor Create(const ADataPort: TCustomPortI2C;  const AAddress: Integer = DefaultAddress;
      const ASensitivity: TSensitivity = TSensitivity.Scale245);

    function GetGyroscopeRaw: TVector3px;
    function GetTemperatureRaw: Integer;

    property DataPort: TCustomPortI2C read FDataPort;
    property Address: Integer read FAddress;
    property Sensitivity: TSensitivity read FSensitivity;

    property Gyroscope: TVector3 read GetGyroscope;
    property Temperature: Integer read GetTemperatureRaw;
  end;

implementation

uses
  SysUtils, Math;

constructor TSensorL3GD20.Create(const ADataPort: TCustomPortI2C; const AAddress: Integer;
  const ASensitivity: TSensitivity);
begin
  inherited Create;

  FDataPort := ADataPort;
  if FDataPort = nil then
    raise ESensorNoDataPort.Create(SSensorNoDataPort);

  FAddress := AAddress;
  if (FAddress < 0) or (FAddress > $7F) then
    raise ESensorInvalidAddress.Create(Format(SSensorInvalidAddress, [FAddress]));

  FSensitivity := ASensitivity;

  TestChipID;
  Configure;
end;

procedure TSensorL3GD20.TestChipID;
const
  ExpectedID1 = $D4;
  ExpectedID2 = $D7;
var
  ChipID: Byte;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.ReadByteData($0F, ChipID) then
    raise ESensorDataRead.Create(Format(SSensorDataRead, [SizeOf(ChipID)]));

  if not (ChipID in [ExpectedID1, ExpectedID2]) then
    raise ESensorInvalidChipID.Create(Format(SSensorInvalidChipID, [ExpectedID2, ChipID]));
end;

procedure TSensorL3GD20.Configure;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.WriteByteData($20, $0F) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [2]));

  if not FDataPort.WriteByteData($23, Ord(FSensitivity) shl 4) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [2]));

  case FSensitivity of
    TSensitivity.Scale500:
      FGyroCoefficient := 1.0 / 500.0;

    TSensitivity.Scale2000:
      FGyroCoefficient := 1.0 / 2000.0;
  else
    FGyroCoefficient := 1.0 / 250.0;
  end;
end;

function TSensorL3GD20.GetGyroscopeRaw: TVector3px;
var
  Values: array[0..5] of Byte;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.WriteByte($28 or $80) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [SizeOf(Byte)]));

  if FDataPort.Read(@Values[0], SizeOf(Values)) <> SizeOf(Values) then
    raise ESensorDataRead.Create(Format(SSensorDataRead, [SizeOf(Values)]));

  Result.X := SmallInt(Word(Values[0]) or (Word(Values[1]) shl 8));
  Result.Y := SmallInt(Word(Values[2]) or (Word(Values[3]) shl 8));
  Result.Z := SmallInt(Word(Values[4]) or (Word(Values[5]) shl 8));
end;

function TSensorL3GD20.GetTemperatureRaw: Integer;
var
  Value: Byte;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.ReadByteData($26, Value) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [SizeOf(Byte)]));

  Result := Value;
end;

function TSensorL3GD20.GetGyroscope: TVector3;
var
  ValueRaw: TVector3px;
begin
  ValueRaw := GetGyroscopeRaw;

  Result.X := DegToRad(ValueRaw.X * FGyroCoefficient);
  Result.Y := DegToRad(ValueRaw.Y * FGyroCoefficient);
  Result.Z := DegToRad(ValueRaw.Z * FGyroCoefficient);
end;

end.
