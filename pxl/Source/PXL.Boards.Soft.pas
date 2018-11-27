unit PXL.Boards.Soft;
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
  TSoftSPI = class(TCustomPortSPI)
  public const
    DefaultFrequency = 0;
    DefaultMode = 0;
  private
    FGPIO: TCustomGPIO;
    FSystemCore: TCustomSystemCore;

    FPinSCLK: Integer;
    FPinMOSI: Integer;
    FPinMISO: Integer;
    FPinCS: Integer;

    FFrequency: Integer;
    FMode: Integer;
    FChipSelectAttributes: TChipSelectAttributes;
    FDelayInterval: Integer;

    function GetInitialValueSCLK: Integer; inline;
    function GetInitialValueCS: Integer; inline;
    procedure SetChipSelect(const ValueCS: Integer); inline;
    procedure FlipClock(var ValueSCLK: Integer); inline;
  protected
    function GetMode: Integer; override;
    procedure SetMode(const Value: Integer); override;
    function GetBitsPerWord: Integer; override;
    procedure SetBitsPerWord(const Value: Integer); override;
    function GetFrequency: Integer; override;
    procedure SetFrequency(const Value: Integer); override;
    function GetChipSelectAttributes: TChipSelectAttributes; override;
    procedure SetChipSelectAttributes(const Value: TChipSelectAttributes); override;
  public
    constructor Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const APinSCLK, APinMOSI,
      APinMISO, APinCS: Integer; const AFrequency: Integer = DefaultFrequency; const AMode: Integer = DefaultMode);
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Integer): Integer; override;

    property GPIO: TCustomGPIO read FGPIO;
    property SystemCore: TCustomSystemCore read FSystemCore;

    property PinSCLK: Integer read FPinSCLK;
    property PinMOSI: Integer read FPinMOSI;
    property PinMISO: Integer read FPinMISO;
    property PinCS: Integer read FPinCS;

    property Mode: Integer read FMode write SetMode;
    property Frequency: Integer read FFrequency write SetFrequency;
    property ChipSelectAttributes: TChipSelectAttributes read FChipSelectAttributes write FChipSelectAttributes;
  end;

  TSoftUART = class(TCustomPortUART)
  private const
    DefaultReadTimeout = 100; // ms
  private
    FGPIO: TCustomGPIO;
    FSystemCore: TCustomSystemCore;
    FPinTX: Integer;
    FPinRX: Integer;
    FBaudRate: Integer;

    function CalculatePeriod(const ElapsedTime: UInt64): Cardinal; inline;
    procedure WaitForPeriod(const StartTime: UInt64; const Period: Cardinal); inline;
  protected
    function GetBaudRate: Integer; override;
    procedure SetBaudRate(const Value: Integer); override;
    function GetBitsPerWord: Integer; override;
    procedure SetBitsPerWord(const Value: Integer); override;
    function GetParity: TParity; override;
    procedure SetParity(const Value: TParity); override;
    function GetStopBits: TStopBits; override;
    procedure SetStopBits(const Value: TStopBits); override;
  public
    constructor Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const APinTX, APinRX: Integer);
    destructor Destroy; override;

    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;

    function ReadBuffer(const Buffer: Pointer; const BufferSize, Timeout: Integer): Integer; override;
    function WriteBuffer(const Buffer: Pointer; const BufferSize, Timeout: Integer): Integer; override;

    procedure Flush; override;

    property BaudRate: Integer read FBaudRate write SetBaudRate;
  end;

  ESoftDataPortGeneric = class(Exception);
  EGPIORefRequired = class(ESoftDataPortGeneric);

  ESoftSPIGeneric = class(ESoftDataPortGeneric);

  ESoftSPIUnsupported = class(ESoftSPIGeneric);
  ESoftSPIUnsupportedMode = class(ESoftSPIUnsupported);
  ESoftSPIUnsupportedBitsPerWord = class(ESoftSPIUnsupported);
  ESoftSPIUnsupportedFrequency = class(ESoftSPIUnsupported);

  ESoftSPIInvalidParameters = class(ESoftSPIGeneric);

resourcestring
  SGPIOReferenceNotProvided = 'Reference to GPIO has not been provided.';
  SSoftSPIUnsupportedMode = 'The specified SPI mode <%d> is not supported.';
  SSoftSPIUnsupportedBitsPerWord = 'The specified SPI bits per word <%d> are not supported.';
  SSoftSPIUnsupportedFrequency = 'The specified SPI frequency <%d> is not supported.';
  SSoftSPIInvalidParameters = 'The specified function parameters are invalid.';

implementation

{$REGION 'TSoftSPI'}

constructor TSoftSPI.Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const APinSCLK, APinMOSI,
  APinMISO, APinCS, AFrequency, AMode: Integer);
begin
  inherited Create;

  FSystemCore := ASystemCore;

  FGPIO := AGPIO;
  if FGPIO = nil then
    raise EGPIORefRequired.Create(ClassName + ExceptionClassNameSeparator + SGPIOReferenceNotProvided);

  FPinSCLK := APinSCLK;
  FPinMOSI := APinMOSI;
  FPinMISO := APinMISO;
  FPinCS := APinCS;

  FMode := -1;
  SetMode(AMode);

  FFrequency := -1;
  SetFrequency(AFrequency);

  FGPIO.PinMode[FPinSCLK] := TPinMode.Output;

  if FPinMOSI <> -1 then
    FGPIO.PinMode[FPinMOSI] := TPinMode.Output;

  if FPinMISO <> -1 then
    FGPIO.PinMode[FPinMISO] := TPinMode.Output;

  if FPinCS <> -1 then
  begin
    FGPIO.PinMode[FPinCS] := TPinMode.Output;
    FGPIO.PinValue[FPinCS] := TPinValue.High;
  end;
end;

destructor TSoftSPI.Destroy;
begin
  if FPinCS <> -1 then
    FGPIO.PinMode[FPinCS] := TPinMode.Input;

  if FPinMISO <> -1 then
    FGPIO.PinMode[FPinMISO] := TPinMode.Input;

  if FPinMOSI <> -1 then
    FGPIO.PinMode[FPinMOSI] := TPinMode.Input;

  FGPIO.PinMode[FPinSCLK] := TPinMode.Input;

  inherited;
end;

function TSoftSPI.GetMode: Integer;
begin
  Result := FMode;
end;

procedure TSoftSPI.SetMode(const Value: Integer);
begin
  if (Value < 0) or (Value > 3) then
    raise ESoftSPIUnsupportedMode.Create(Format(SSoftSPIUnsupportedMode, [Value]));

  FMode := Value;
end;

function TSoftSPI.GetBitsPerWord: Integer;
begin
  Result := 8;
end;

procedure TSoftSPI.SetBitsPerWord(const Value: Integer);
begin
  if Value <> 8 then
    raise ESoftSPIUnsupportedBitsPerWord.Create(Format(SSoftSPIUnsupportedBitsPerWord, [Value]));
end;

function TSoftSPI.GetFrequency: Integer;
begin
  Result := FFrequency;
end;

procedure TSoftSPI.SetFrequency(const Value: Integer);
begin
  if (Value < 0) or (Value > 1000000) then
    raise ESoftSPIUnsupportedFrequency.Create(Format(SSoftSPIUnsupportedFrequency, [Value]));

  FFrequency := Value;

  if (FFrequency <> 0) and (FSystemCore <> nil) then
    FDelayInterval := 1000000 div FFrequency
  else
    FDelayInterval := 0;
end;

function TSoftSPI.GetChipSelectAttributes: TChipSelectAttributes;
begin
  Result := FChipSelectAttributes;
end;

procedure TSoftSPI.SetChipSelectAttributes(const Value: TChipSelectAttributes);
begin
  FChipSelectAttributes := Value;
end;

function TSoftSPI.GetInitialValueSCLK: Integer;
begin
  if FMode and 2 > 0 then
    Result := 0
  else
    Result := 1;
end;

function TSoftSPI.GetInitialValueCS: Integer;
begin
  if TChipSelectAttribute.ActiveHigh in FChipSelectAttributes then
    Result := 1
  else
    Result := 0;
end;

procedure TSoftSPI.SetChipSelect(const ValueCS: Integer);
begin
  if FPinCS <> -1 then
    FGPIO.PinValue[FPinCS] := TPinValue(ValueCS);
end;

procedure TSoftSPI.FlipClock(var ValueSCLK: Integer);
begin
  ValueSCLK := ValueSCLK xor 1;
  FGPIO.PinValue[FPinSCLK] := TPinValue(ValueSCLK);
end;

function TSoftSPI.Read(const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  I, BitIndex, ValueSCLK, ValueCS: Integer;
  CycleStartTime: UInt64;
  ReadValue: Cardinal;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise ESoftSPIInvalidParameters.Create(SSoftSPIInvalidParameters);

  ValueSCLK := GetInitialValueSCLK;
  ValueCS := GetInitialValueCS;

  FGPIO.PinValue[FPinSCLK] := TPinValue(ValueSCLK);
  SetChipSelect(ValueCS);
  try
    for I := 0 to BufferSize - 1 do
    begin
      ReadValue := 0;

      for BitIndex := 0 to 7 do
      begin
        if FDelayInterval <> 0 then
          CycleStartTime := FSystemCore.GetTickCount;

        if FPinMISO <> -1 then
        begin
          if FGPIO.PinValue[FPinMISO] = TPinValue.High then
            ReadValue := ReadValue or 1;

          ReadValue := ReadValue shl 1;
        end;

        FlipClock(ValueSCLK);

        if FDelayInterval <> 0 then
          while FSystemCore.TicksInBetween(CycleStartTime, FSystemCore.GetTickCount) < FDelayInterval do ;

        FlipClock(ValueSCLK);
      end;

      PByte(PtrUInt(Buffer) + Cardinal(I))^ := ReadValue;
    end;
  finally
    SetChipSelect(ValueCS xor 1);
  end;

  Result := BufferSize;
end;

function TSoftSPI.Write(const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  I, BitIndex, ValueSCLK, ValueCS: Integer;
  CycleStartTime: UInt64;
  WriteValue: Cardinal;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise ESoftSPIInvalidParameters.Create(SSoftSPIInvalidParameters);

  ValueSCLK := GetInitialValueSCLK;
  ValueCS := GetInitialValueCS;

  FGPIO.PinValue[FPinSCLK] := TPinValue(ValueSCLK);
  SetChipSelect(ValueCS);
  try
    for I := 0 to BufferSize - 1 do
    begin
      WriteValue := PByte(PtrUInt(Buffer) + Cardinal(I))^;

      for BitIndex := 0 to 7 do
      begin
        if FDelayInterval <> 0 then
          CycleStartTime := FSystemCore.GetTickCount;

        if FPinMOSI <> -1 then
        begin
          if WriteValue and $80 > 0 then
            FGPIO.PinValue[FPinMOSI] := TPinValue.High
          else
            FGPIO.PinValue[FPinMOSI] := TPinValue.Low;

          WriteValue := WriteValue shl 1;
        end;

        FlipClock(ValueSCLK);

        if FDelayInterval <> 0 then
          while FSystemCore.TicksInBetween(CycleStartTime, FSystemCore.GetTickCount) < FDelayInterval do ;

        FlipClock(ValueSCLK);
      end;
    end;
  finally
    SetChipSelect(ValueCS xor 1);
  end;

  Result := BufferSize;
end;

function TSoftSPI.Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Integer): Integer;
var
  I, BitIndex, ValueSCLK, ValueCS: Integer;
  CycleStartTime: UInt64;
  WriteValue, ReadValue: Cardinal;
begin
  if ((ReadBuffer = nil) and (WriteBuffer = nil)) or (BufferSize <= 0) then
    raise ESoftSPIInvalidParameters.Create(SSoftSPIInvalidParameters);

  ValueSCLK := GetInitialValueSCLK;
  ValueCS := GetInitialValueCS;

  FGPIO.PinValue[FPinSCLK] := TPinValue(ValueSCLK);
  SetChipSelect(ValueCS);
  try
    WriteValue := 0;

    for I := 0 to BufferSize - 1 do
    begin
      ReadValue := 0;

      if WriteBuffer <> nil then
        WriteValue := PByte(PtrUInt(WriteBuffer) + Cardinal(I))^;

      for BitIndex := 0 to 7 do
      begin
        if FDelayInterval <> 0 then
          CycleStartTime := FSystemCore.GetTickCount;

        if FPinMOSI <> -1 then
        begin
          if WriteValue and $80 > 0 then
            FGPIO.PinValue[FPinMOSI] := TPinValue.High
          else
            FGPIO.PinValue[FPinMOSI] := TPinValue.Low;

          WriteValue := WriteValue shl 1;
        end;

        if FPinMISO <> -1 then
        begin
          if FGPIO.PinValue[FPinMISO] = TPinValue.High then
            ReadValue := ReadValue or 1;

          ReadValue := ReadValue shl 1;
        end;

        FlipClock(ValueSCLK);

        if FDelayInterval <> 0 then
          while FSystemCore.TicksInBetween(CycleStartTime, FSystemCore.GetTickCount) < FDelayInterval do ;

        FlipClock(ValueSCLK);
      end;

      if ReadBuffer <> nil then
        PByte(PtrUInt(ReadBuffer) + Cardinal(I))^ := ReadValue;
    end;
  finally
    SetChipSelect(ValueCS xor 1);
  end;

  Result := BufferSize;
end;

{$ENDREGION}
{$REGION 'TSoftUART'}

constructor TSoftUART.Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const APinTX,
  APinRX: Integer);
begin
  inherited Create;

  FSystemCore := ASystemCore;

  FGPIO := AGPIO;
  if FGPIO = nil then
    raise EGPIORefRequired.Create(ClassName + ExceptionClassNameSeparator + SGPIOReferenceNotProvided);

  FPinTX := APinTX;
  FPinRX := APinRX;

  FGPIO.PinMode[FPinTX] := TPinMode.Output;
  FGPIO.PinValue[FPinTX] := TPinValue.High;
  FGPIO.PinMode[FPinRX] := TPinMode.Input;
end;

destructor TSoftUART.Destroy;
begin
  FGPIO.PinMode[FPinTX] := TPinMode.Input;

  inherited;
end;

function TSoftUART.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

procedure TSoftUART.SetBaudRate(const Value: Integer);
begin
  FBaudRate := Value;
end;

function TSoftUART.GetBitsPerWord: Integer;
begin
  Result := 8;
end;

procedure TSoftUART.SetBitsPerWord(const Value: Integer);
begin
end;

function TSoftUART.GetParity: TParity;
begin
  Result := TParity.None;
end;

procedure TSoftUART.SetParity(const Value: TParity);
begin
end;

function TSoftUART.GetStopBits: TStopBits;
begin
  Result := TStopBits.One;
end;

procedure TSoftUART.SetStopBits(const Value: TStopBits);
begin
end;

function TSoftUART.CalculatePeriod(const ElapsedTime: UInt64): Cardinal;
begin
  Result := (UInt64(FBaudRate) * ElapsedTime) div 1000000;
end;

procedure TSoftUART.WaitForPeriod(const StartTime: UInt64; const Period: Cardinal);
var
  Current: Cardinal;
begin
  repeat
    Current := CalculatePeriod(FSystemCore.TicksInBetween(StartTime, FSystemCore.GetTickCount));
  until Current >= Period;
end;

function TSoftUART.Write(const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  StartTime: UInt64;
  Period: Cardinal;
  I: Integer;
  BitMask, Value: Cardinal;
begin
  for I := 0 to BufferSize - 1 do
  begin
    FGPIO.PinValue[FPinTX] := TPinValue.Low;

    // Start sending data a bit earlier to ensure that receiver will sample data correctly.
    FSystemCore.Delay(UInt64(1000000 * 3) div (UInt64(FBaudRate) * 5));

    Period := 0;
    StartTime := FSystemCore.GetTickCount;

    Value := PByte(PtrUInt(Buffer) + Cardinal(I))^;
    BitMask := 1;

    while BitMask <> 256 do
    begin
      if Value and BitMask > 0 then
        FGPIO.PinValue[FPinTX] := TPinValue.High
      else
        FGPIO.PinValue[FPinTX] := TPinValue.Low;

      BitMask := BitMask shl 1;

      Inc(Period);
      WaitForPeriod(StartTime, Period);
    end;

    FGPIO.PinValue[FPinTX] := TPinValue.High;

    Inc(Period);
    WaitForPeriod(StartTime, Period);
  end;

  Result := BufferSize;
end;

function TSoftUART.Read(const Buffer: Pointer; const BufferSize: Integer): Integer;
begin
  Result := ReadBuffer(Buffer, BufferSize, DefaultReadTimeout);
end;

function TSoftUART.ReadBuffer(const Buffer: Pointer; const BufferSize, Timeout: Integer): Integer;
var
  StartTime, TimeoutStart, TimeoutMicroSec: UInt64;
  Period, BitMask, Value: Cardinal;
  BytesReceived: Integer;
begin
  BytesReceived := 0;

  if Timeout > 0 then
    TimeoutMicrosec := UInt64(Timeout) * 1000
  else
    TimeoutMicrosec := DefaultReadTimeout * 1000;

  TimeoutStart := FSystemCore.GetTickCount;

  // Wait for RX line to settle on high value.
  repeat
    if FSystemCore.TicksInBetween(TimeoutStart, FSystemCore.GetTickCount) > TimeoutMicrosec then
      Exit(0);
  until FGPIO.PinValue[FPinRX] = TPinValue.High;

  while BytesReceived < BufferSize do
  begin
    // Wait until RX line goes low for the "Start" bit.
    repeat
      if FSystemCore.TicksInBetween(TimeoutStart, FSystemCore.GetTickCount) > TimeoutMicrosec then
        Exit(BytesReceived);
    until FGPIO.PinValue[FPinRX] = TPinValue.Low;

    // Once start bit is received, wait for another 1/3rd of baud to sort of center next samples.
    FSystemCore.Delay(UInt64(1000000) div (UInt64(FBaudRate) * UInt64(4)));

    // Start receiving next byte.
    BitMask := 1;
    Value := 0;

    Period := 0;
    StartTime := FSystemCore.GetTickCount;

    // Skip the remaining of "Start" bit.
    Inc(Period);
    WaitForPeriod(StartTime, Period);

    while BitMask <> 256 do
    begin
      if FGPIO.PinValue[FPinRX] = TPinValue.High then
        Value := Value or BitMask;

      BitMask := BitMask shl 1;

      Inc(Period);
      WaitForPeriod(StartTime, Period);
    end;

    PByte(PtrUInt(Buffer) + Cardinal(BytesReceived))^ := Value;
    Inc(BytesReceived);
  end;

  Result := BytesReceived;
end;

function TSoftUART.WriteBuffer(const Buffer: Pointer; const BufferSize, Timeout: Integer): Integer;
begin
  Result := Write(Buffer, BufferSize);
end;

procedure TSoftUART.Flush;
begin
end;

{$ENDREGION}

end.

