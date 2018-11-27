unit PXL.Clocks.DS1307;
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
  SysUtils, PXL.Boards.Types;

type
  TClockRTC = class(TCustomClockRTC)
  public const
    DefaultAddress = $68;
  private
    FDataPort: TCustomPortI2C;
    FAddress: Integer;

    class function ValueToBCD(const Value: Integer): Integer; static; inline;
    class function BCDToValue(const Value: Integer): Integer; static; inline;

    function GetSquarePinMode: Integer;
    procedure SetSquarePinMode(const Value: Integer);
  protected
    function GetValue: TDateTime; override;
    procedure SetValue(const Value: TDateTime); override;
  public
    constructor Create(const ADataPort: TCustomPortI2C; const AAddress: Integer = DefaultAddress);

    procedure ReadNVRAM(const DataAddress: Integer; const Buffer: Pointer; const BufferSize: Integer);
    procedure WriteNVRAM(const DataAddress: Integer; const Buffer: Pointer; const BufferSize: Integer);

    property DataPort: TCustomPortI2C read FDataPort;
    property Address: Integer read FAddress;
    property SquarePinMode: Integer read GetSquarePinMode write SetSquarePinMode;
  end;

  EClockGeneric = class(Exception);

  EClockDataRead = class(EClockGeneric);
  EClockDataWrite = class(EClockGeneric);

  EClockNoDataPort = class(EClockGeneric);
  EClockInvalidAddress = class(EClockGeneric);

  EClockNVRAMError = class(EClockGeneric);
  EClockNVRAMInvalidAddress = class(EClockNVRAMError);
  EClockNVRAMDataTooBig = class(EClockNVRAMError);
  EClockNVRAMDataInvalid = class(EClockNVRAMError);

resourcestring
  SClockDataRead = 'Unable to read <%d> bytes from RTC clock.';
  SClockDataWrite = 'Unable to write <%d> bytes to RTC clock.';
  SClockNoDataPort = 'A valid data port is required for RTC clock.';
  SClockInvalidAddress = 'The specified RTC clock address <%x> is invalid.';
  SClockNVRAMInvalidAddress = 'The specified RTC clock NVRAM address <%x> is invalid.';
  SClockNVRAMDataTooBig = 'RTC clock <%d> data bytes starting at <%x> address is too big to fit in NVRAM.';
  SClockNVRAMDataInvalid = 'The specified data buffer and/or size are invalid.';

implementation

uses
  DateUtils;

class function TClockRTC.ValueToBCD(const Value: Integer): Integer;
begin
  Result := Value + 6 * (Value div 10);
end;

class function TClockRTC.BCDToValue(const Value: Integer): Integer;
begin
  Result := Value - 6 * (Value shr 4);
end;

constructor TClockRTC.Create(const ADataPort: TCustomPortI2C; const AAddress: Integer);
begin
  inherited Create;

  FDataPort := ADataPort;
  if FDataPort = nil then
    raise EClockNoDataPort.Create(SClockNoDataPort);

  FAddress := AAddress;
  if (FAddress < 0) or (FAddress > $7F) then
    raise EClockInvalidAddress.Create(Format(SClockInvalidAddress, [FAddress]));
end;

function TClockRTC.GetValue: TDateTime;
var
  Values: array[0..6] of Byte;
begin
  FDataPort.SetAddress(FAddress);
  FDataPort.WriteByte(0);

  if FDataPort.Read(@Values[0], SizeOf(Values)) <> SizeOf(Values) then
    raise EClockDataRead.Create(Format(SClockDataRead, [SizeOf(Values)]));

  Result := EncodeDateTime(BCDToValue(Values[6]) + 2000, BCDToValue(Values[5]), BCDToValue(Values[4]),
    BCDToValue(Values[2]), BCDToValue(Values[1]), BCDToValue(Values[0] and $7F), 0);
end;

procedure TClockRTC.SetValue(const Value: TDateTime);
var
  Values: array[0..9] of Byte;
  LYears, LMonths, LDays, LHours, LMinutes, LSeconds, LMilliseconds: Word;
begin
  DecodeDateTime(Value, LYears, LMonths, LDays, LHours, LMinutes, LSeconds, LMilliseconds);

  Values[0] := 0;
  Values[1] := ValueToBCD(LSeconds);
  Values[2] := ValueToBCD(LMinutes);
  Values[3] := ValueToBCD(LHours);

  Values[4] := 0;
  Values[5] := ValueToBCD(LDays);
  Values[6] := ValueToBCD(LMonths);
  Values[7] := ValueToBCD(LYears - 2000);
  Values[8] := 0;

  FDataPort.SetAddress(FAddress);

  if FDataPort.Write(@Values[0], SizeOf(Values)) <> SizeOf(Values) then
    raise EClockDataWrite.Create(Format(SClockDataWrite, [SizeOf(Values)]));
end;

function TClockRTC.GetSquarePinMode: Integer;
var
  Value: Byte;
begin
  FDataPort.SetAddress(FAddress);
  FDataPort.WriteByte($07);

  if not FDataPort.ReadByte(Value) then
    raise EClockDataRead.Create(Format(SClockDataRead, [SizeOf(Byte)]));

  Result := Value and $93;
end;

procedure TClockRTC.SetSquarePinMode(const Value: Integer);
var
  Values: array[0..1] of Byte;
begin
  Values[0] := $07;
  Values[1] := Value;

  FDataPort.SetAddress(FAddress);

  if FDataPort.Write(@Values[0], SizeOf(Values)) <> SizeOf(Values) then
    raise EClockDataWrite.Create(Format(SClockDataWrite, [SizeOf(Values)]));
end;

procedure TClockRTC.ReadNVRAM(const DataAddress: Integer; const Buffer: Pointer; const BufferSize: Integer);
var
  Values: array of Byte;
begin
  if (DataAddress < 0) or (DataAddress >= 56) then
    raise EClockNVRAMInvalidAddress.Create(Format(SClockNVRAMInvalidAddress, [DataAddress]));

  if DataAddress + BufferSize > 56 then
    raise EClockNVRAMDataTooBig.Create(Format(SClockNVRAMDataTooBig, [BufferSize, DataAddress]));

  if (Buffer = nil) or (BufferSize < 1) then
    raise EClockNVRAMDataInvalid.Create(SClockNVRAMDataInvalid);

  FDataPort.SetAddress(FAddress);
  FDataPort.WriteByte($08 + DataAddress);

  if FDataPort.Read(Buffer, BufferSize) <> BufferSize then
    raise EClockDataRead.Create(Format(SClockDataRead, [BufferSize]));
end;

procedure TClockRTC.WriteNVRAM(const DataAddress: Integer; const Buffer: Pointer; const BufferSize: Integer);
var
  Values: array of Byte;
begin
  if (DataAddress < 0) or (DataAddress >= 56) then
    raise EClockNVRAMInvalidAddress.Create(Format(SClockNVRAMInvalidAddress, [DataAddress]));

  if DataAddress + BufferSize > 56 then
    raise EClockNVRAMDataTooBig.Create(Format(SClockNVRAMDataTooBig, [BufferSize, DataAddress]));

  if (Buffer = nil) or (BufferSize < 1) then
    raise EClockNVRAMDataInvalid.Create(SClockNVRAMDataInvalid);

  SetLength(Values, BufferSize + 1);
  Values[0] := $08 + DataAddress;

  Move(Buffer^, Values[1], BufferSize);

  FDataPort.SetAddress(FAddress);
  if FDataPort.Write(@Values[0], BufferSize + 1) <> BufferSize + 1 then
    raise EClockDataWrite.Create(Format(SClockDataWrite, [BufferSize + 1]));
end;

end.
