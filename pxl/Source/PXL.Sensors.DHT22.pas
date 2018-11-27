unit PXL.Sensors.DHT22;
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

{ Enable the following option to "restart" the sensor each time, which seems to be the only way to somewhat reliably
  read the sensor each time. Otherwise, the sensor would give data a couple of times and then continue to fail. }
{$DEFINE DHT22_WORKAROUND}

uses
  SysUtils, PXL.Types, PXL.Boards.Types, PXL.Sensors.Types;

type
  TSensorDHT22 = class(TCustomSensor)
  private const
    WaitSignalTimeout = 250; // microseconds
  protected const
    SensorUpdateInterval = 2000; // milliseconds
    SensorStartupTime = 250; // milliseconds
    SensorWakeupTime = 20; // milliseconds
  private
    FSystemCore: TCustomSystemCore;
    FGPIO: TCustomGPIO;
    FPin: Integer;
    FLastTemperature: Integer;
    FLastHumidity: Integer;
    FLastReadTicks: UInt64;

    function WaitForLowSignal(out WaitedTicks: UInt64): Boolean; overload; inline;
    function WaitForHighSignal(out WaitedTicks: UInt64): Boolean; inline;

    function GetTemperature: Single;
    function GetHumidity: Single;
  public
    constructor Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const APin: Integer);
    destructor Destroy; override;

    function ReadRawValues(out Temperature, Humidity: Integer): Boolean;

    { Retrieves "compact" values of temperature (multiplied by two, e.g. value of 90 means 45 C) and relative
      humidity (0 = 0%, 255 = 100%), clamped to fit within their respective ranges. Returns True if successful and
      False if there were communication errors. }
    function ReadValuesCompact(out Temperature: ShortInt; out Humidity: Byte): Boolean;

    property SystemCore: TCustomSystemCore read FSystemCore;
    property GPIO: TCustomGPIO read FGPIO;
    property Pin: Integer read FPin;

    property Temperature: Single read GetTemperature;
    property Humidity: Single read GetHumidity;
  end;

  ESensorReadValues = class(ESensorGeneric);

resourcestring
  SSensorReadValues = 'Error trying to read values from the sensor.';

implementation

constructor TSensorDHT22.Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const APin: Integer);
begin
  inherited Create;

  FSystemCore := ASystemCore;
  if FSystemCore = nil then
    raise ESensorNoSystemCore.Create(SSensorNoSystemCore);

  FGPIO := AGPIO;
  if FGPIO = nil then
    raise ESensorNoGPIO.Create(SSensorNoGPIO);

  FPin := APin;

{$IFNDEF DHT22_WORKAROUND}
  FGPIO.PinMode[FPin] := TPinMode.Output;
  FGPIO.PinValue[FPin] := TPinValue.High;
  FSystemCore.Delay(SensorStartupTime * 1000);
{$ENDIF}

  FLastTemperature := -1;
  FLastHumidity := -1;
end;

destructor TSensorDHT22.Destroy;
begin
  FGPIO.PinMode[FPin] := TPinMode.Input;

  inherited;
end;

function TSensorDHT22.WaitForLowSignal(out WaitedTicks: UInt64): Boolean;
var
  StartTime: UInt64;
begin
  StartTime := FSystemCore.GetTickCount;

  while FGPIO.PinValue[FPin] = TPinValue.High do
  begin
    if FSystemCore.TicksInBetween(StartTime, FSystemCore.GetTickCount) >= WaitSignalTimeout then
      Exit(False);
  end;

  WaitedTicks := FSystemCore.TicksInBetween(StartTime, FSystemCore.GetTickCount);
  Result := True;
end;

function TSensorDHT22.WaitForHighSignal(out WaitedTicks: UInt64): Boolean;
var
  StartTime: UInt64;
begin
  StartTime := FSystemCore.GetTickCount;

  while FGPIO.PinValue[FPin] = TPinValue.Low do
  begin
    if FSystemCore.TicksInBetween(StartTime, FSystemCore.GetTickCount) >= WaitSignalTimeout then
      Exit(False);
  end;

  WaitedTicks := FSystemCore.TicksInBetween(StartTime, FSystemCore.GetTickCount);
  Result := True;
end;

function TSensorDHT22.ReadRawValues(out Temperature, Humidity: Integer): Boolean;
const
  DataReceiveWaitTime = 40;
  MinTimeToConsiderOne = 40;
var
  Cycle, Mask, Index: Integer;
  Data: array[0..5] of Byte;
  WaitedTicks: UInt64;
begin
  if (FLastTemperature <> -1) and (FLastHumidity <> -1) and (FSystemCore.TicksInBetween(FLastReadTicks,
    FSystemCore.GetTickCount) < SensorUpdateInterval * 1000) then
  begin
    Temperature := FLastTemperature;
    Humidity := FLastHumidity;
    Exit(True);
  end;

  FillChar(Data, SizeOf(Data), 0);

{$IFDEF DHT22_WORKAROUND}
  FGPIO.PinMode[FPin] := TPinMode.Output;
  FGPIO.PinValue[FPin] := TPinValue.High;
  FSystemCore.Delay(SensorStartupTime * 1000);
{$ENDIF}

  FGPIO.PinValue[FPin] := TPinValue.Low;
  FSystemCore.Delay(SensorWakeupTime * 1000);

  FGPIO.PinValue[FPin] := TPinValue.High;
  FSystemCore.Delay(DataReceiveWaitTime);
  FGPIO.PinMode[FPin] := TPinMode.Input;

  if not WaitForHighSignal(WaitedTicks) then
    Exit(False);

  if not WaitForLowSignal(WaitedTicks) then
    Exit(False);

  Mask := 128;
  Index := 0;

  for Cycle := 0 to 39 do
  begin
    if not WaitForHighSignal(WaitedTicks) then
      Exit(False);

    if not WaitForLowSignal(WaitedTicks) then
      Exit(False);

    if WaitedTicks > MinTimeToConsiderOne then
      Data[Index] := Data[Index] or Mask;

    Mask := Mask shr 1;
    if Mask = 0 then
    begin
      Mask := 128;
      Inc(Index);
    end;
  end;

{$IFDEF DHT22_WORKAROUND}
  FGPIO.PinMode[FPin] := TPinMode.Input;
{$ELSE}
  FGPIO.PinMode[FPin] := TPinMode.Output;
  FGPIO.PinValue[FPin] := TPinValue.High;
{$ENDIF}

  if (Data[0] + Data[1] + Data[2] + Data[3]) and $FF <> Data[4] then
    Exit(False);

  Temperature := (Word(Data[2] and $7F) shl 8) or Word(Data[3]);
  if Data[2] and $80 > 0 then
    Temperature := -Temperature;

  Humidity := (Word(Data[0]) shl 8) or Word(Data[1]);

  FLastTemperature := Temperature;
  FLastHumidity := Humidity;
  FLastReadTicks := FSystemCore.GetTickCount;

  Result := True;
end;

function TSensorDHT22.GetTemperature: Single;
var
  RawTemperature, RawHumidity: Integer;
begin
  if not ReadRawValues(RawTemperature, RawHumidity) then
    raise ESensorReadValues.Create(SSensorReadValues);

  Result := RawTemperature * 0.1;
end;

function TSensorDHT22.GetHumidity: Single;
var
  RawTemperature, RawHumidity: Integer;
begin
  if not ReadRawValues(RawTemperature, RawHumidity) then
    raise ESensorReadValues.Create(SSensorReadValues);

  Result := RawHumidity * 0.1;
end;

function TSensorDHT22.ReadValuesCompact(out Temperature: ShortInt; out Humidity: Byte): Boolean;
var
  RawTemperature, RawHumidity: Integer;
begin
  if not ReadRawValues(RawTemperature, RawHumidity) then
    Exit(False);

  Temperature := Saturate(RawTemperature div 5, Low(ShortInt), High(ShortInt));
  Humidity := Saturate((RawHumidity * 255) div 10, Low(Byte), High(Byte));

  Result := True;
end;

end.
