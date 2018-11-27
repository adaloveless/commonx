unit PXL.Boards.Types;
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
{< Basic types and components commonly used on compact singleboard devices. }
interface

{$INCLUDE PXL.Config.inc}

uses
  SysUtils, PXL.TypeDef;

type
  { System core of the board, which provides high-performance utility functions for accurate timing and delays. }
  TCustomSystemCore = class abstract
  public
    { Returns the current value of system timer as 64-bit unsigned integer, in microseconds. }
    function GetTickCount: UInt64; virtual;

    { Calculates the difference between two system timer values with proper handling of overflows. }
    function TicksInBetween(const InitTicks, EndTicks: UInt64): UInt64; inline;

    { Waits the specified amount of microseconds accurately by continuously polling the timer.
      This is useful for accurate timing but may result in high CPU usage. }
    procedure BusyWait(const Microseconds: Cardinal);

    { Waits for the specified amount of microseconds, calling NanoSleep if waiting time is long enough for the most
      portion of wait time, while the remaining part doing a busy wait to provide greater accuracy. }
    procedure Delay(const Microseconds: Cardinal); virtual;
  end;

  { I/O mode typically used in GPIO pins. }
  TPinMode = (
    { Pin set for input / high impedance }
    Input,

    { Pin set for output }
    Output);

  { Digital value of the pin. }
  TPinValue = (
    { Low (0) or zero voltage. }
    Low,

    { High (1) or full voltage. }
    High);

  { Abstract GPIO (General Purpose Input / Output) manager. }
  TCustomGPIO = class abstract
  protected
    { Returns current mode for the specified pin number. }
    function GetPinMode(const Pin: Integer): TPinMode; virtual; abstract;

    { Changes mode for the specified pin number, as long as new mode is different than the current one. }
    procedure SetPinMode(const Pin: Integer; const Mode: TPinMode); virtual; abstract;

    { Returns current value for the specified pin number, both for input and output modes. }
    function GetPinValue(const Pin: Integer): TPinValue; virtual; abstract;

    { Changes value for the specified pin number, as long as new value is different than the current one. }
    procedure SetPinValue(const Pin: Integer; const Value: TPinValue); virtual; abstract;
  public
    { Configures the specified pin for output and with the specified value, typically used for configuring
      multiplexers. }
    procedure SetMux(const Pin: Integer; const Value: TPinValue); inline;

    { Currently set mode for the specified pin number. }
    property PinMode[const Pin: Integer]: TPinMode read GetPinMode write SetPinMode;

    { Currently set signal value for the specified pin number. }
    property PinValue[const Pin: Integer]: TPinValue read GetPinValue write SetPinValue;
  end;

  { Abstract PWM (Pulse-Width Modulation) manager. }
  TCustomPWM = class abstract
  protected
    { Returns @True when the specified pin is configured for PWM output and @False otherwise. }
    function GetEnabled(const Pin: Integer): Boolean; virtual; abstract;

    { Changes status of PWM output on the specified pin number. }
    procedure SetEnabled(const Pin: Integer; const Value: Boolean); virtual; abstract;

    { Returns current period (nanoseconds) set for the specified pin number. }
    function GetPeriod(const Pin: Integer): Integer; virtual; abstract;

    { Changes period (nanoseconds) for the specified pin number. }
    procedure SetPeriod(const Pin, Value: Integer); virtual; abstract;

    { Returns current duty cycle (nanoseconds, in relation to period) set for the specified pin number. }
    function GetDutyCycle(const Pin: Integer): Integer; virtual; abstract;

    { Changes duty cycle (nanoseconds, in relation to period) for the specified pin number. }
    procedure SetDutyCycle(const Pin, Value: Integer); virtual; abstract;
  public
    { Starts PWM on specified Pin with the desired frequency (in Hz) and duty cycle (0.5 = 50%). }
    procedure Start(const Pin: Integer; const Frequency: Integer; const DutyCycle: Single); virtual;

    { Stops PWM on the specified Pin. }
    procedure Stop(const Pin: Integer); virtual;

    { Determines whether the specified Pin is configured for PWM output. }
    property Enabled[const Pin: Integer]: Boolean read GetEnabled write SetEnabled;

    { Determines PWM period in nanoseconds (e.g. 1000000 ns period would be 1 ms or 100 hz). }
    property Period[const Pin: Integer]: Integer read GetPeriod write SetPeriod;

    { Determines PWM duty cycle in nanoseconds in respect to period (e.g. 500000 ns for period of 1000000 ns would
      define a 50% of duty cycle). }
    property DutyCycle[const Pin: Integer]: Integer read GetDutyCycle write SetDutyCycle;
  end;

  { Abstract ADC (Analog-to-Digital Converter) manager. }
  TCustomADC = class abstract
  protected
    { Returns raw digital value for the given channel number. }
    function GetRawValue(const Channel: Integer): Integer; virtual; abstract;
  public
    { Raw value on the specified analog input channel that depends on particular device's resolution. }
    property RawValue[const Channel: Integer]: Integer read GetRawValue;
  end;

  { Abstract communication manager can be used for reading and writing data. }
  TCustomDataPort = class abstract
  public
    { Reads specified number of bytes to buffer and returns actual number of bytes read. }
    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; virtual; abstract;

    { Writes specified number of bytes from buffer and returns actual number of bytes written. }
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; virtual; abstract;
  end;

  { Abstract I2C (Inter-Integrated Circuit) communication manager. }
  TCustomPortI2C = class abstract(TCustomDataPort)
  public
    { Specifies new device address to which the communication will be made. }
    procedure SetAddress(const Address: Integer); virtual; abstract;

    { Reads a single byte from current address. Returns @True when the operation was successful and @False otherwise. }
    function ReadByte(out Value: Byte): Boolean; virtual;

    { Write a single byte to current address. Returns @True when the operation was successful and @False otherwise. }
    function WriteByte(const Value: Byte): Boolean; virtual;

    { Write one or more bytes to current address. Returns @True when the operation was successful and @False otherwise. }
    function WriteBytes(const Values: array of Byte): Boolean;

    { Writes command to current address and reads a single byte from it. Although this varies depending on
      implementation, but typically stop bit is given at the end of the whole transmission (so there is no stop bit
      between command and read operation). Returns @True when the operation was successful and @False otherwise. }
    function ReadByteData(const Command: Byte; out Value: Byte): Boolean; virtual;

    { Writes command and a single byte of data to current address. Returns @True when the operation was successful and
      @False otherwise. }
    function WriteByteData(const Command, Value: Byte): Boolean; virtual;

    { Writes command to current address and reads a word (16-bit unsigned) from it. Although this varies depending on
      implementation, but typically stop bit is given at the end of the whole transmission (so there is no stop bit
      between command and read operation). Returns @True when the operation was successful and @False otherwise. }
    function ReadWordData(const Command: Byte; out Value: Word): Boolean; virtual;

    { Writes command and a word (16-bit unsigned) of data to current address. Returns @True when the operation was
      successful and @False otherwise. }
    function WriteWordData(const Command: Byte; const Value: Word): Boolean; virtual;

    { Writes command to current address and reads specified block of data from it. Although this varies depending on
      implementation, but typically stop bit is given at the end of the whole transmission (so there is no stop bit
      between command and read operation). Returns @True when the operation was successful and @False otherwise. }
    function ReadBlockData(const Command: Byte; const Buffer: Pointer;
      const BufferSize: Integer): Integer; virtual; abstract;

    { Writes command and specified block of data to current address. Returns @True when the operation was
      successful and @False otherwise. }
    function WriteBlockData(const Command: Byte; const Buffer: Pointer;
      const BufferSize: Integer): Integer; virtual; abstract;
  end;

  { Chip Select handling attribute. }
  TChipSelectAttribute = (
    { Chip Select should be held active high instead of active low. }
    ActiveHigh,

    { Chip Select should be disabled. }
    Disable);

  { Set of optional Chip Select handling attributes. }
  TChipSelectAttributes = set of TChipSelectAttribute;

  { Abstract SPI (Serial Peripheral Interface) communication manager. }
  TCustomPortSPI = class abstract(TCustomDataPort)
  protected
    { Returns currently active SPI mode. }
    function GetMode: Integer; virtual; abstract;

    { Changes current SPI mode to the specified value. }
    procedure SetMode(const Value: Integer); virtual; abstract;

    { Returns current number of bits that each word occupies. }
    function GetBitsPerWord: Integer; virtual; abstract;

    { Changes current number of bits each word occupies. }
    procedure SetBitsPerWord(const Value: Integer); virtual; abstract;

    { Returns current operating frequency. }
    function GetFrequency: Integer; virtual; abstract;

    { Changes current operating frequency. }
    procedure SetFrequency(const Value: Integer); virtual; abstract;

    { Returns current Chip Select handling attributes. }
    function GetChipSelectAttributes: TChipSelectAttributes; virtual; abstract;

    { Changes current Chip Select handling attributes. }
    procedure SetChipSelectAttributes(const Value: TChipSelectAttributes); virtual; abstract;
  public
    { Transfers data through SPI port asynchronously - that is, reading and writing at the same time.
        @param(ReadBuffer Pointer to data buffer where the data will be read from. If this parameter is set to @nil,
          then no reading will be done.)
        @param(WriteBuffer Pointer to data buffer where the data will be written to. If this parameter is set to @nil,
          then no writing will be done.)
        @param(BufferSize The size of read and write buffers in bytes.)
        @returns(Number of bytes that were actually transferred.) }
    function Transfer(const ReadBuffer, WriteBuffer: Pointer;
      const BufferSize: Integer): Integer; overload; virtual; abstract;

    { Transfers data through SPI port asynchronously - that is, reading and writing at the same time.
        @param(Buffer Pointer to data buffer where the data will be read from and at the same time written to,
          overwriting its contents.)
        @param(BufferSize The size of buffer in bytes.)
        @returns(Number of bytes that were actually transferred.) }
    function Transfer(const Buffer: Pointer; const BufferSize: Integer): Integer; overload; inline;

    { Mode of SPI operation, including Clock Polarity (CPOL) and Clock Edge (CPCHA). The actual meaning of this
      parameter depends on implementation and should be consulted from corresponding documentation. }
    property Mode: Integer read GetMode write SetMode;

    { Number of bits each word occupies, typically either 8 or 16 depending on hardware and support. }
    property BitsPerWord: Integer read GetBitsPerWord write SetBitsPerWord;

    { SPI operating frequency in Hz. }
    property Frequency: Integer read GetFrequency write SetFrequency;

    { Chip Select handling attributes. }
    property ChipSelectAttributes: TChipSelectAttributes read GetChipSelectAttributes write SetChipSelectAttributes;
  end;

  { Parity bit type used for transceiving binary strings. }
  TParity = (
    { No parity bit. }
    None,

    { Odd parity bit. }
    Odd,

    { Even parity bit. }
    Even);

  { Number of stop bits used for transceiving binary strings. }
  TStopBits = (
    { One stop bit. }
    One,

    { One and "half" stop bits. }
    OneDotFive,

    { Two stop bits. }
    Two);

  { Abstract UART (Universal Asynchronous Receiver / Transmitter) communication manager. }
  TCustomPortUART = class abstract(TCustomDataPort)
  protected const
    { Default buffer size used when reading and writing strings. }
    StringBufferSize = 32; // characters

    { Default sleep time while waiting during transmission between multiple attempts. }
    InterimSleepTime = 10; // ms
  protected
    { Returns currently set baud rate. }
    function GetBaudRate: Integer; virtual; abstract;

    { Sets new baud rate. }
    procedure SetBaudRate(const Value: Integer); virtual; abstract;

    { Returns current number of bits per word. }
    function GetBitsPerWord: Integer; virtual; abstract;

    { Sets new number of bits per word. }
    procedure SetBitsPerWord(const Value: Integer); virtual; abstract;

    { Returns current parity check type. }
    function GetParity: TParity; virtual; abstract;

    { Sets new parity check type. }
    procedure SetParity(const Value: TParity); virtual; abstract;

    { Returns current number of stop bits. }
    function GetStopBits: TStopBits; virtual; abstract;

    { Sets new number of stop bits. }
    procedure SetStopBits(const Value: TStopBits); virtual; abstract;
  public
    { Flushes UART read and write buffers. The actual meaning of this may depend on implementation - it could mean that
      the buffers are simply emptied or any buffers that are waiting due to packetization are sent immediately. }
    procedure Flush; virtual; abstract;

    { Reads buffer from UART.
        @param(Buffer Pointer to data buffer where the data will be written to.)
        @param(BufferSize Number of bytes to read.)
        @param(Timeout Maximum time (in milliseconds) to wait while attempting to read the buffer. If this parameter is
          set to zero, then the function will read only as much data as fits in readable FIFO buffers (or none when
          such buffers are not supported).)
        @returns(Number of bytes that were actually read.) }
    function ReadBuffer(const Buffer: Pointer; const BufferSize, Timeout: Integer): Integer; virtual;

    { Writes buffer to UART.
        @param(Buffer Pointer to data buffer where the data will be read from.)
        @param(BufferSize Number of bytes to write.)
        @param(Timeout Maximum time (in milliseconds) to wait while attempting to write the buffer. If this parameter
          is set to zero, then the function will write only as much data as fits in writable FIFO buffers (or none
          when such buffers are not supported).)
        @returns(Number of bytes that were actually written.) }
    function WriteBuffer(const Buffer: Pointer; const BufferSize, Timeout: Integer): Integer; virtual;

    { Attempts to read a byte from UART. @code(Timeout) defines maximum time (in milliseconds) to wait while attempting
      to do so; if this parameter is set to zero, then the function will read only what's in readable FIFO buffers or
      fail when such buffers are unavailable. @True is returned when the operation was successful and @False when the
      byte could not be read. }
    function ReadByte(out Value: Byte; const Timeout: Integer = 0): Boolean; inline;

    { Attempts to write a byte to UART. @code(Timeout) defines maximum time (in milliseconds) to wait while attempting
      to do so; if this parameter is set to zero, then the function will write only what fits in writable FIFO buffers
      or fail when such buffers are unavailable. @True is returned when the operation was successful and @False when
      the byte could not be written. }
    function WriteByte(const Value: Byte; const Timeout: Integer = 0): Boolean; inline;

    { Attempts to write multiple bytes to UART. @code(Timeout) defines maximum time (in milliseconds) to wait while
      attempting to do so; if this parameter is set to zero, then the function will write only what fits in writable
      FIFO buffers or fail when such buffers are unavailable. @True is returned when the operation was successful and
      @False when not all bytes could be written. }
    function WriteBytes(const Values: array of Byte; const Timeout: Integer = 0): Boolean;

    { Reads string from UART.
        @param(Text String that will hold the incoming data.)
        @param(MaxCharacters Maximum number of characters to read. Once this number of characters has been read, the
          function immediately returns, even if there is more data to read. When this parameter is set to zero, then
          the function will continue to read the data, depending on value of @code(Timeout).)
        @param(Timeout Maximum time (in milliseconds) to wait while attempting to read the buffer. If this parameter
          is set to zero, then the function will read only as much data as fits in readable FIFO buffers (or fail when
          such buffers are not supported).)
        @returns(Number of bytes that were actually read.) }
    function ReadString(out Text: StdString; const MaxCharacters: Integer = 0;
      const Timeout: Integer = 0): Boolean;

    { Writes string to UART.
        @param(Text String that should be sent.)
        @param(Timeout Maximum time (in milliseconds) to wait while attempting to write the buffer. If this parameter
          is set to zero, then the function will write only what fits in writable FIFO buffers (or fail when such
          buffers are not supported).)
        @returns(Number of bytes that were actually read.) }
    function WriteString(const Text: StdString; const Timeout: Integer = 0): Boolean;

    { Currently used baud rate in terms of bits per second. Note that to calculate the actual speed of transmission,
      it is necessary to take into account start and stop bits among other things; for typical situations, the actual
      transmission speed may be something like BaudRate / 10 bytes per second or less. }
    property BaudRate: Integer read GetBaudRate write SetBaudRate;

    { Number of bits per word used in the transmission. }
    property BitsPerWord: Integer read GetBitsPerWord write SetBitsPerWord;

    { Number of parity bits used in the transmission. }
    property Parity: TParity read GetParity write SetParity;

    { Number of stop bits used in the transmission. }
    property StopBits: TStopBits read GetStopBits write SetStopBits;
  end;

  { Abstract RTC (Real-Time Clock) manager. }
  TCustomClockRTC = class
  protected
    { Returns current clock value. }
    function GetValue: TDateTime; virtual; abstract;

    { Sets new clock value. }
    procedure SetValue(const Value: TDateTime); virtual; abstract;
  public
    { Current clock value. }
    property Value: TDateTime read GetValue write SetValue;
  end;

const
  { Maximum number of bytes that can be reliably sent through SPI protocol in each read/write call. }
  MaxSPITransferSize = 4096;

  { Maximum number of bytes that can be reliably sent through I2C protocol in each read/write call. }
  MaxI2CTransferSize = 32;

  { @exclude } ExceptionClassNameSeparator = ': ';

implementation

uses
  Math, PXL.Timing;

{$REGION 'TCustomSystemCore'}

function TCustomSystemCore.GetTickCount: UInt64;
begin
  Result := GetSystemTimerValue;
end;

function TCustomSystemCore.TicksInBetween(const InitTicks, EndTicks: UInt64): UInt64;
begin
  Result := EndTicks - InitTicks;
  if High(UInt64) - Result < Result then
    Result := High(UInt64) - Result;
end;

procedure TCustomSystemCore.BusyWait(const Microseconds: Cardinal);
var
  StartTicks: UInt64;
begin
  StartTicks := GetTickCount;
  while TicksInBetween(StartTicks, GetTickCount) < Microseconds do ;
end;

procedure TCustomSystemCore.Delay(const Microseconds: Cardinal);
begin
  MicroSleep(Microseconds);
end;

{$ENDREGION}
{$REGION 'TCustomGPIO'}

procedure TCustomGPIO.SetMux(const Pin: Integer; const Value: TPinValue);
begin
  SetPinMode(Pin, TPinMode.Output);
  SetPinValue(Pin, Value);
end;

{$ENDREGION}
{$REGION 'TCustomPWM'}

procedure TCustomPWM.Start(const Pin: Integer; const Frequency: Integer; const DutyCycle: Single);
var
  DesiredPeriod: Integer;
begin
  SetEnabled(Pin, False);

  DesiredPeriod := Int64(1000000000) div Frequency;

  SetPeriod(Pin, DesiredPeriod);
  SetDutyCycle(Pin, Round(DesiredPeriod * DutyCycle));

  SetEnabled(Pin, True);
end;

procedure TCustomPWM.Stop(const Pin: Integer);
begin
  SetEnabled(Pin, False);
end;

{$ENDREGION}
{$REGION 'TCustomPortI2C'}

function TCustomPortI2C.ReadByte(out Value: Byte): Boolean;
begin
  Result := Read(@Value, SizeOf(Byte)) = SizeOf(Byte);
end;

function TCustomPortI2C.WriteByte(const Value: Byte): Boolean;
begin
  Result := Write(@Value, SizeOf(Byte)) = SizeOf(Byte);
end;

function TCustomPortI2C.WriteBytes(const Values: array of Byte): Boolean;
begin
  if Length(Values) > 0 then
    Result := Write(@Values[0], Length(Values)) = Length(Values)
  else
    Result := False;
end;

function TCustomPortI2C.ReadByteData(const Command: Byte; out Value: Byte): Boolean;
begin
  Result := ReadBlockData(Command, @Value, SizeOf(Byte)) = SizeOf(Byte);
end;

function TCustomPortI2C.WriteByteData(const Command, Value: Byte): Boolean;
begin
  Result := WriteBlockData(Command, @Value, SizeOf(Byte)) = SizeOf(Byte);
end;

function TCustomPortI2C.ReadWordData(const Command: Byte; out Value: Word): Boolean;
begin
  Result := ReadBlockData(Command, @Value, SizeOf(Word)) = SizeOf(Word);
end;

function TCustomPortI2C.WriteWordData(const Command: Byte; const Value: Word): Boolean;
begin
  Result := WriteBlockData(Command, @Value, SizeOf(Word)) = SizeOf(Word);
end;

{$ENDREGION}
{$REGION 'TCustomPortUART'}

function TCustomPortUART.ReadBuffer(const Buffer: Pointer; const BufferSize, Timeout: Integer): Integer;
var
  StartupTicks: Cardinal;
  BytesRead, BytesProcessed: Integer;
begin
  if Timeout > 0 then
  begin
    StartupTicks := GetSystemTickCount;
    BytesProcessed := 0;

    while BytesProcessed < BufferSize do
    begin
      BytesRead := Read(Pointer(PtrUInt(Buffer) + Cardinal(BytesProcessed)), BufferSize - BytesProcessed);
      if BytesRead <= 0 then
      begin
        if TickCountInBetween(StartupTicks, GetSystemTickCount) >= Cardinal(Timeout) then
          Break;

        Sleep(InterimSleepTime);
        Continue;
      end;

      Inc(BytesProcessed, BytesRead);
    end;

    Result := BytesProcessed;
  end
  else
    Result := Read(Buffer, BufferSize);
end;

function TCustomPortUART.WriteBuffer(const Buffer: Pointer; const BufferSize, Timeout: Integer): Integer;
var
  StartupTicks: Cardinal;
  BytesWritten, BytesProcessed: Integer;
begin
  if Timeout > 0 then
  begin
    StartupTicks := GetSystemTickCount;
    BytesProcessed := 0;

    while BytesProcessed < BufferSize do
    begin
      BytesWritten := Write(Pointer(PtrUInt(Buffer) + Cardinal(BytesProcessed)), BufferSize - BytesProcessed);
      if BytesWritten <= 0 then
      begin
        if (Timeout > 0) and (TickCountInBetween(StartupTicks, GetSystemTickCount) >= Cardinal(Timeout)) then
          Break;

        Sleep(InterimSleepTime);
        Continue;
      end;

      Inc(BytesProcessed, BytesWritten);
    end;

    Result := BytesProcessed;
  end
  else
    Result := Write(Buffer, BufferSize);
end;

function TCustomPortUART.ReadByte(out Value: Byte; const Timeout: Integer): Boolean;
begin
  Result := ReadBuffer(@Value, SizeOf(Byte), Timeout) = SizeOf(Byte);
end;

function TCustomPortUART.WriteByte(const Value: Byte; const Timeout: Integer): Boolean;
begin
  Result := WriteBuffer(@Value, SizeOf(Byte), Timeout) = SizeOf(Byte);
end;

function TCustomPortUART.WriteBytes(const Values: array of Byte; const Timeout: Integer): Boolean;
begin
  if Length(Values) > 0 then
    Result := WriteBuffer(@Values[0], Length(Values), Timeout) = Length(Values)
  else
    Result := False;
end;

function TCustomPortUART.ReadString(out Text: StdString; const MaxCharacters, Timeout: Integer): Boolean;
const
  PercentualLengthDiv = 4;
var
  StartupTicks: Cardinal;
  BytesRead, BytesToRead, I, TextLength, NewTextLength: Integer;
  Buffer: array[0..StringBufferSize - 1] of Byte;
begin
  // Define initial string length.
  NewTextLength := StringBufferSize;

  if (MaxCharacters > 0) and (NewTextLength > MaxCharacters) then
    NewTextLength := MaxCharacters;

  SetLength(Text, NewTextLength);

  // Start time measurement.
  StartupTicks := GetSystemTickCount;
  TextLength := 0;

  while ((MaxCharacters <= 0) or (TextLength < MaxCharacters)) and ((Timeout <= 0) or (TickCountInBetween(StartupTicks,
    GetSystemTickCount) < Cardinal(Timeout))) do
  begin
    // Determine number of bytes that still need to be read.
    BytesToRead := StringBufferSize;

    if MaxCharacters > 0 then
      BytesToRead := Min(BytesToRead, MaxCharacters - TextLength);

    // Read bytes from serial port.
    BytesRead := Read(@Buffer[0], BytesToRead);
    if BytesRead <= 0 then
    begin
      if Timeout <= 0 then
        Break;

      Sleep(InterimSleepTime);
      Continue;
    end;

    // Increase the length of string as necessary.
    if Length(Text) < TextLength + BytesRead then
    begin
      NewTextLength := Length(Text) + StringBufferSize + (Length(Text) div PercentualLengthDiv);

      if MaxCharacters > 0 then
        NewTextLength := Min(NewTextLength, MaxCharacters);

      SetLength(Text, NewTextLength);
    end;

    // Copy read bytes into the string.
    for I := 0 to BytesRead - 1 do
      Text[1 + TextLength + I] := Chr(Buffer[I]);

    Inc(TextLength, BytesRead);
  end;

  // Adjust the length of string to match what was actually read.
  SetLength(Text, TextLength);
  Result := Length(Text) > 0;
end;

function TCustomPortUART.WriteString(const Text: StdString; const Timeout: Integer): Boolean;
{$IF SIZEOF(StdChar) <> 1}
var
  StartupTicks: Cardinal;
  I, WrittenLength, BytesToWrite, BytesWritten: Integer;
  Buffer: array[0..StringBufferSize - 1] of Byte;
{$ENDIF}
begin
  if Length(Text) > 0 then
  begin
{$IF SIZEOF(StdChar) = 1}
    Result := WriteBuffer(@Text[1], Length(Text), Timeout) = Length(Text);
{$ELSE}
    StartupTicks := GetSystemTickCount;
    WrittenLength := 0;

    while (WrittenLength < Length(Text)) and ((Timeout <= 0) or
      (TickCountInBetween(StartupTicks, GetSystemTickCount) < Cardinal(Timeout))) do
    begin
      BytesToWrite := Min(StringBufferSize, Length(Text) - WrittenLength);

      for I := 0 to BytesToWrite - 1 do
        Buffer[I] := Min(Ord(Text[1 + WrittenLength + I]), 255);

      BytesWritten := Write(@Buffer[0], BytesToWrite);
      if BytesWritten <= 0 then
      begin
        if Timeout <= 0 then
          Break;

        Sleep(InterimSleepTime);
        Continue;
      end;

      Inc(WrittenLength, BytesWritten);
    end;

    Result := WrittenLength = Length(Text);
{$ENDIF}
  end
  else
    Result := False;
end;

function TCustomPortSPI.Transfer(const Buffer: Pointer; const BufferSize: Integer): Integer;
begin
  Result := Transfer(Buffer, Buffer, BufferSize);
end;

{$ENDREGION}

end.

