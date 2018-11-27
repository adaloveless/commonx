unit PXL.Boards.RPi;
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
{
  Acknowledgments:

  For development of this support library, one of richest source of information was:
    "C library for Broadcom BCM 2835 as used in Raspberry Pi" written by Mike McCauley.
      http://www.airspayce.com/mikem/bcm2835/
  That library provided great deal of information and ideas on how to interact with BCM2835 registers, handling
  barriers, accessing SPI and I2C. Many thanks for that library author for such excellent work!

  An invaluable source of information was "BCM2835 ARM Peripherals" PDF file:
    http://www.raspberrypi.org/wp-content/uploads/2012/02/BCM2835-ARM-Peripherals.pdf
  This document provided important information about chip registers and the meaning of different bits / constants.

  Other sources of motivation were the following projects:

    "Hardware abstraction library for the Raspberry Pi" by Stefan Fischer.
    http://shop.basis.biz/shop/Raspberry-PI/piggy-back-board/

    "BCM2835 GPIO Registry Driver" by Gabor Szollosi.
    http://wiki.freepascal.org/Lazarus_on_Raspberry_Pi
}
interface

{$INCLUDE PXL.Config.inc}

{ Enable this option to reset SPI and I2C pins back to "input" mode after using. By default and after reboot, these
  pins are usually set to Alt0 mode, so they are ready to use by Sysfs SPI/I2C. Therefore, resetting them to "input"
  would prevent native Linux SPI / I2C from working until next reboot, or until the function of these pins is adjusted. }
{.$DEFINE DATAPORTS_PINS_RESET_AFTER_DONE}

// The following option controls whether the code is optimized for Raspberry PI 2.
{.$DEFINE RPi2}

uses
  SysUtils, PXL.TypeDef, PXL.Boards.Types, PXL.Sysfs.UART;

type
  TChipOffset = PtrUInt;

  TFastSystemCore = class(TCustomSystemCore)
  public const
    BaseClock = 250000000; // 250 mHz
    PageSize = 4096;
    ChipOffsetST = $3000;
  strict private const
    ChipMapPath = '/dev/mem';
    DeviceTreeRangesPath = '/proc/device-tree/soc/ranges';

    OffsetTimerLower = $0004; // System Timer Counter Lower 32 bits
    OffsetTimerUpper = $0008; // System Timer Counter Upper 32 bits
  strict private
    FChipOffsetBase: TChipOffset;

    FHandle: TUntypedHandle;
    FMemory: Pointer;

    function GetChipOffsetST: TChipOffset; inline;
    function UpdateIOValuesFromKernel: Boolean;
  protected
    function GetChipOffsetBase: TChipOffset; inline;
    function GetOffsetPointer(const Offset: Cardinal): Pointer; inline;

    property Handle: TUntypedHandle read FHandle;
  public
    constructor Create;
    destructor Destroy; override;

    { Returns the current value of system timer as 64-bit unsigned integer, in microseconds. }
    function GetTickCount: UInt64; override;

    { Waits for the specified amount of microseconds, calling NanoSleep if waiting time is long enough for the most
      portion of wait time, while the remaining part doing a busy wait to provide greater accuracy. }
    procedure Delay(const MicroSeconds: Cardinal); override;
  end;

  TNumberingScheme = (Printed, BCM);

  TPinModeEx = (Input = $00, Output = $01, Alt5 = $02, Alt4 = $03, Alt0 = $04, Alt1 = $05, Alt2 = $06, Alt3 = $07);

  TFastGPIO = class(TCustomGPIO)
  strict private
    FSystemCore: TFastSystemCore;
    FMemory: Pointer;
    FNumberingScheme: TNumberingScheme;

    function GetChipOffsetGPIO: TChipOffset; inline;
    function GetPinModeEx(const Pin: Integer): TPinModeEx;
    procedure SetPinModeEx(const Pin: Integer; const Value: TPinModeEx);
  protected
    function GetOffsetPointer(const Offset: Cardinal): Pointer; inline;
    function ProcessPinNumber(const Pin: Integer): Integer;

    procedure SetPinModeBCM(const PinBCM: Integer; const Mode: TPinModeEx);

    function GetPinMode(const Pin: Integer): TPinMode; override;
    procedure SetPinMode(const Pin: Integer; const Mode: TPinMode); override;

    function GetPinValue(const Pin: Integer): TPinValue; override;
    procedure SetPinValue(const Pin: Integer; const Value: TPinValue); override;

    property Memory: Pointer read FMemory;
  public
    constructor Create(const ASystemCore: TFastSystemCore;
      const ANumberingScheme: TNumberingScheme = TNumberingScheme.Printed);
    destructor Destroy; override;

    { Quickly changes specified pin value (assuming it is set for output). Note that the pin must be specified using
      native BCM numbering scheme. }
    procedure SetFastValue(const PinBCM: Integer; const Value: TPinValue);

    { Reference to @link(TFastSystemCore), which provides high performance timing and delay utilities. }
    property SystemCore: TFastSystemCore read FSystemCore;

    { Defines what pin numbering scheme should be used for all functions that receive "Pin" as parameter.
      Default is "Printed" scheme, which means pins are specified as they are numbered on PCB. Alternatively, it can
      be changed to "BCM" numbering scheme, which use native GPIO numbers. }
    property NumberingScheme: TNumberingScheme read FNumberingScheme write FNumberingScheme;

    { Provides control and feedback of currently selected mode for the given pin, including alternative functions as
      supported by BCM2835 chip. }
    property PinModeEx[const Pin: Integer]: TPinModeEx read GetPinModeEx write SetPinModeEx;
  end;

  TFastSPI = class(TCustomPortSPI)
  public const
    DefaultChipSelect = 0;
    DefaultFrequency = 8000000;
    DefaultMode = 0;
  strict private const
    OffsetControlStatus = $0000;
    OffsetDataBuffer = $0004;
    OffsetClockDivider = $0008;

    MaskControlStatusChipSelect = $00000003;
    MaskControlStatusClockPhase = $00000004;
    MaskControlStatusClockPolarity = $00000008;
    MaskControlStatusClearBuffer = $00000030;
    MaskControlStatusTransfer = $00000080;
    MaskControlStatusDone = $00010000;
    MaskControlStatusRXD = $00020000;
    MaskControlStatusTXD = $00040000;

    TransferBlockCounterStart = 1024; // # of ticks when to capture initial time
    TransferBlockCounterMax = 8192; // # of ticks after which to start comparing time
    TransferBlockTimeout = 1000000; // in microseconds
  private
    FFastGPIO: TFastGPIO;
    FMemory: Pointer;

    FMode: Integer;
    FFrequency: Integer;
    FChipSelectAttributes: TChipSelectAttributes;
    FChipSelect: Integer;

    function GetChipOffsetSPI: TChipOffset; inline;
    procedure SetChipSelect(const Value: Integer);
  protected
    function GetOffsetPointer(const Offset: Cardinal): Pointer; inline;

    function GetMode: Integer; override;
    procedure SetMode(const Value: Integer); override;
    function GetBitsPerWord: Integer; override;
    procedure SetBitsPerWord(const Value: Integer); override;
    function GetFrequency: Integer; override;
    procedure SetFrequency(const Value: Integer); override;
    function GetChipSelectAttributes: TChipSelectAttributes; override;
    procedure SetChipSelectAttributes(const Value: TChipSelectAttributes); override;

    property FastGPIO: TFastGPIO read FFastGPIO;
    property Memory: Pointer read FMemory;
  public
    constructor Create(const AFastGPIO: TFastGPIO; const AChipSelect: Integer = DefaultChipSelect;
      const AFrequency: Integer = DefaultFrequency; const AMode: Integer = DefaultMode);
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Integer): Integer; override;

    { Defines clock polarity and phase for SPI operation. }
    property Mode: Integer read FMode write SetMode;

    { Controls the operating frequency of SPI bus in Hz, with supported values between ~3.8 kHz and 125 mHz.
      Typically, default value for SPI is 8 mHz. }
    property Frequency: Integer read FFrequency write SetFrequency;

    { Defines what Chip Select line to enable during transfers. Supported values are 0 = CE0 and 1 = CE1. }
    property ChipSelect: Integer read FChipSelect write SetChipSelect;

    { Determines how Chip Select line is handled by protocol. }
    property ChipSelectAttributes: TChipSelectAttributes read FChipSelectAttributes write SetChipSelectAttributes;
  end;

  TFastI2C = class(TCustomPortI2C)
  strict private const
    OffsetControl = $0000;
    OffsetStatus = $0004;
    OffsetDataLength = $0008;
    OffsetSlaveAddress = $000C;
    OffsetDataBuffer = $0010;
    OffsetClockDivider = $0014;

    MaskControlRead = $00000001;
    MaskControlClearBuffers = $00000020;
    MaskControlStart = $00000080;
    MaskControlEnabled = $00008000;

    MaskStatusTransfer = $00000001;
    MaskStatusDone = $00000002;
    MaskStatusTXD = $00000010;
    MaskStatusRXD = $00000020;
    MaskStatusNoACK = $00000100;
    MaskStatusTimeout = $00000200;

    MaxInternalBufferSize = 16;

    TransferCounterStart = 1024; // # of ticks when to capture initial time
    TransferCounterMax = 8192; // # of ticks after which to start comparing time
    TransferTimeout = 1000000; // in microseconds
  strict private
    FFastGPIO: TFastGPIO;
    FMemory: Pointer;
    FFrequency: Integer;
    FTimePerByte: Cardinal;

    function GetChipOffsetI2C: TChipOffset; inline;
    procedure UpdateTimePerByte(const ClockDivider: Cardinal);
    procedure SetFrequency(const Value: Integer);
    function ProcessBlockCounter(var BlockCounter: Integer; var BlockTimeoutStart: UInt64): Boolean; inline;
  protected
    function GetOffsetPointer(const Offset: Cardinal): Pointer; inline;

    property FastGPIO: TFastGPIO read FFastGPIO;
    property Memory: Pointer read FMemory;
  public
    constructor Create(const AFastGPIO: TFastGPIO);
    destructor Destroy; override;

    procedure SetAddress(const Address: Integer); override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;

    function ReadBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function WriteBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Integer): Integer; override;

    { Controls the operating frequency of I2C bus in Hz. }
    property Frequency: Integer read FFrequency write SetFrequency;
  end;

  TDefaultUART = class(TSysfsUART)
  public const
    DefaultSystemPath = '/dev/ttyAMA0';
  private
    FFastGPIO: TFastGPIO;
  protected
    property FastGPIO: TFastGPIO read FFastGPIO;
  public
    constructor Create(const AFastGPIO: TFastGPIO; const ASystemPath: StdString = DefaultSystemPath);
    destructor Destroy; override;
  end;

  ERPiGeneric = class(Exception);
  ERPiOpenFile = class(ERPiGeneric);
  ERPiMemoryMap = class(ERPiGeneric);

  EGPIOGeneric = class(ERPiGeneric);
  EGPIOMemoryMap = class(EGPIOGeneric);
  EGPIOUnsupported = class(EGPIOGeneric);

  EGPIOInvalidPin = class(EGPIOGeneric);
  EGPIOInvalidBCMPin = class(EGPIOInvalidPin);
  EGPIOInvalidPrintedPin = class(EGPIOInvalidPin);
  EGPIOAlternateFunctionPin = class(EGPIOGeneric);

  ESPIGeneric = class(ERPiGeneric);
  ESPIMemoryMap = class(ESPIGeneric);
  ESPIUnsupportedGeneric = class(ESPIGeneric);
  ESPIUnsupportedBitsPerWord = class(ESPIUnsupportedGeneric);
  ESPIUnsupportedFrequency = class(ESPIUnsupportedGeneric);
  ESPIUnsupportedChipSelect = class(ESPIUnsupportedGeneric);

  EI2CGeneric = class(ERPiGeneric);
  EI2CUnsupportedGeneric = class(EI2CGeneric);
  EI2CUnsupportedFrequency = class(EI2CUnsupportedGeneric);

  ESystemCoreRefRequired = class(ERPiGeneric);
  EGPIORefRequired = class(ERPiGeneric);

resourcestring
  SCannotMapRegistersPortion = 'Cannot map <%s> portion of BCM2835 registers to memory.';
  SCannotOpenFileToMap = 'Cannot not open file <%s> for memory mapping.';

  SGPIOSpecifiedBCMPinInvalid = 'The specified GPIO pin <%d> (BCM) is invalid.';
  SGPIOSpecifiedPrintedPinInvalid = 'The specified GPIO pin <%d> (Printed) is invalid.';
  SGPIOSpecifiedBCMPinAlternativeMode = 'The specified GPIO pin <%d> has non-basic alternative mode.';

  SGPIOUnsupported = 'The requested feature is unsupported.';

  SSPIUnsupportedBitsPerWord = 'Specified SPI number of bits per word <%d> is not supported.';
  SSPIUnsupportedFrequency = 'Specified SPI frequency <%d> is not supported.';
  SSPIUnsupportedChipSelect = 'Specified SPI chip select line <%d> is not supported.';

  SI2CUnsupportedFrequency = 'Specified I2C frequency <%d> is not supported.';

  SSystemCoreRefNotProvided = 'Reference to TFastSystemCore has not been provided.';
  SGPIORefNotProvided = 'Reference to TFastGPIO has not been provided.';

implementation

uses
  BaseUnix, Math;

{$REGION 'Global Types and Functions'}

const
  PinmapPrintedToBCM: array[1..40] of Integer = (
    { 01 } -1,  // 3.3V
    { 02 } -1,  // 5V
    { 03 }  2,  // SDA1 (Alt0)
    { 04 } -1,  // 5V
    { 05 }  3,  // SCL1 (Alt0)
    { 06 } -1,  // Ground
    { 07 }  4,  // GPCLK0
    { 08 } 14,  // TXD0 (Alt0)
    { 09 } -1,  // Ground
    { 10 } 15,  // RXD0 (Alt0)
    { 11 } 17,
    { 12 } 18,  // PCM_CLK / PWM0 (Alt0)
    { 13 } 27,  // PCM_DOUT
    { 14 } -1,  // Ground
    { 15 } 22,
    { 16 } 23,
    { 17 } -1,  // 3.3V
    { 18 } 24,
    { 19 } 10,  // SPI0_MOSI (Alt0)
    { 20 } -1,  // Ground
    { 21 }  9,  // SPI0_MISO (Alt0)
    { 22 } 25,
    { 23 } 11,  // SPI0_SCLK (Alt0)
    { 24 }  8,  // SPI0_CE0 (Alt0)
    { 25 } -1,  // Ground
    { 26 }  7,  // SPI0_CE1 (Alt0)

    { 27 } -1,  // EEPROM ID Data
    { 28 } -1,  // EEPROM ID Clock
    { 29 }  5,
    { 30 } -1,  // Ground
    { 31 }  6,
    { 32 } 12,  // PWM0 (Alt0)
    { 33 } 13,  // PWM1 (Alt0)
    { 34 } -1,  // Ground
    { 35 } 19,  // PCM Frame Sync (Alt0), SPI1_MISO (Alt4), PWM1 (Alt5)
    { 36 } 16,
    { 37 } 26,
    { 38 } 20,  // PCM Data In (Alt0), SPI1_MOSI (Alt4)
    { 39 } -1,  // Ground
    { 40 } 21   // PCM Data Out (Alt0), SPI_SCLK (Alt4)
  );

{ The following "safe" commands do the actual reading and/or writing twice to make sure the data arrives in the
  appropriate order. Apparently, when switching access between different peripheral devices, next read or write
  operation may not arrive in the appropriate order so some values to be read may be lost, while others that are
  written may result in data corruption. Therefore, the actual data is read and/or written twice, where the second
  operation is guaranteed to succeed.

  On Raspberry PI 2 (ARMv7) there is an data memory barrier instruction, which resolves the above. }

procedure WriteMemSafe(const Address: Pointer; const Value: Cardinal);
asm
{$IFDEF RPi2}
  dmb
  str	r1, [r0]
  dmb
{$ELSE}
  str	r1, [r0]
  str	r1, [r0]
{$ENDIF}
end;

function ReadMemSafe(const Address: Pointer): Cardinal;
asm
{$IFDEF RPi2}
  dmb
  ldr r0, [r0]
  dmb
{$ELSE}
  ldr r1, [r0]
  ldr r0, [r0]
{$ENDIF}
end;

procedure ChangeBitsSafe(const Address: Pointer; const Value, Mask: Cardinal); inline;
begin
  WriteMemSafe(Address, (ReadMemSafe(Address) and (not Mask)) or (Value and Mask));
end;

procedure WriteMemFast(const Address: Pointer; const Value: Cardinal); inline;
begin
  PCardinal(Address)^ := Value;
end;

function ReadMemFast(const Address: Pointer): Cardinal; inline;
begin
  Result := PCardinal(Address)^;
end;

function PortionMap(const RegName: StdString; const Handle: TUntypedHandle; const Offset, PageSize: Cardinal): Pointer;
begin
  Result := Fpmmap(nil, PageSize, PROT_READ or PROT_WRITE, MAP_SHARED, Handle, Offset);
  if (Result = nil) or (Result = MAP_FAILED) then
    raise ERPiMemoryMap.Create(Format(SCannotMapRegistersPortion, [RegName]));
end;

procedure PortionUnmap(var MemAddr: Pointer; const PageSize: Cardinal);
begin
  if MemAddr <> nil then
  begin
    Fpmunmap(MemAddr, PageSize);
    MemAddr := nil;
  end;
end;

{$ENDREGION}
{$REGION 'TFastSystemCore'}

constructor TFastSystemCore.Create;
begin
  inherited;

  FChipOffsetBase := {$IFDEF RPi2} $3F000 {$ELSE} $20000 {$ENDIF}; // in pages (x 4096 bytes)
  UpdateIOValuesFromKernel;

  FHandle := FpOpen(ChipMapPath, O_RDWR or O_SYNC);
  if FHandle < 0 then
  begin
    FHandle := 0;
    raise ERPiOpenFile.Create(Format(SCannotOpenFileToMap, [ChipMapPath]));
  end;

  FMemory := PortionMap('ST', FHandle, GetChipOffsetST, PageSize);
end;

destructor TFastSystemCore.Destroy;
begin
  PortionUnmap(FMemory, PageSize);

  if FHandle <> 0 then
  begin
    FpClose(FHandle);
    FHandle := 0;
  end;

  inherited;
end;

function TFastSystemCore.UpdateIOValuesFromKernel: Boolean;
const
  OffsetForIOStart = 4;
var
  LHandle: TUntypedHandle;
  DataValue: Cardinal;
begin
  LHandle := FpOpen(DeviceTreeRangesPath, O_RDONLY);
  if LHandle < 0 then
    Exit(False);
  try
    if FpLseek(LHandle, OffsetForIOStart, Seek_Set) <> OffsetForIOStart then
      Exit(False);

    if FpRead(LHandle, DataValue, SizeOf(DataValue)) <> SizeOf(DataValue) then
      Exit(False);

    FChipOffsetBase := BEtoN(DataValue) div PageSize;
  finally
    FpClose(LHandle);
  end;

  Result := True;
end;

function TFastSystemCore.GetChipOffsetBase: TChipOffset;
begin
  Result := FChipOffsetBase;
end;

function TFastSystemCore.GetChipOffsetST: TChipOffset;
begin
  Result := GetChipOffsetBase + $3;
end;

function TFastSystemCore.GetOffsetPointer(const Offset: Cardinal): Pointer;
begin
  Result := Pointer(PtrUInt(FMemory) + Offset);
end;

function TFastSystemCore.GetTickCount: UInt64;
var
  UpperBits, LowerBits: Cardinal;
begin
  UpperBits := ReadMemSafe(GetOffsetPointer(OffsetTimerUpper));
  LowerBits := ReadMemSafe(GetOffsetPointer(OffsetTimerLower));

  Result := ReadMemSafe(GetOffsetPointer(OffsetTimerUpper));

  if Result <> UpperBits then
    Result := (Result shl 32) or ReadMemSafe(GetOffsetPointer(OffsetTimerLower))
  else
    Result := (Result shl 32) or LowerBits;
end;

procedure TFastSystemCore.Delay(const MicroSeconds: Cardinal);
var
  StartTicks: UInt64;
  NanoSpec: timespec;
begin
  StartTicks := GetTickCount;

  { This whole approach was taken from "bcm2835" library by Mike McCauley, where the author explained that "nanosleep"
    call by itself takes around 100 - 200 microseconds, so in this case it is called when waiting time is big enough
    for a time that is 200 microseconds shorter; the remaining time is spent doing busy wait. }
  if Microseconds > 300 then
  begin
  	NanoSpec.tv_nsec := (Microseconds - 200) * 1000;
  	NanoSpec.tv_sec := 0;

    FpNanoSleep(@NanoSpec, nil);
  end;

  while TicksInBetween(StartTicks, GetTickCount) < Microseconds do ;
end;

{$ENDREGION}
{$REGION 'TFastGPIO'}

constructor TFastGPIO.Create(const ASystemCore: TFastSystemCore; const ANumberingScheme: TNumberingScheme);
begin
  inherited Create;

  FSystemCore := ASystemCore;
  if FSystemCore = nil then
    raise ESystemCoreRefRequired.Create(SSystemCoreRefNotProvided);

  FMemory := PortionMap('GPIO', FSystemCore.Handle, GetChipOffsetGPIO, TFastSystemCore.PageSize);

  FNumberingScheme := ANumberingScheme;
end;

destructor TFastGPIO.Destroy;
begin
  PortionUnmap(FMemory, TFastSystemCore.PageSize);

  inherited;
end;

function TFastGPIO.GetChipOffsetGPIO: TChipOffset;
begin
  Result := FSystemCore.GetChipOffsetBase + $200;
end;

function TFastGPIO.GetOffsetPointer(const Offset: Cardinal): Pointer;
begin
  Result := Pointer(PtrUInt(FMemory) + Offset);
end;

function TFastGPIO.ProcessPinNumber(const Pin: Integer): Integer;
begin
  if FNumberingScheme = TNumberingScheme.Printed then
  begin
    if (Pin < Low(PinmapPrintedToBCM)) or (Pin > High(PinmapPrintedToBCM)) then
      raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedBCMPinInvalid, [Pin]));

    Result := PinmapPrintedToBCM[Pin];
  end
  else
  begin
    if (Pin < 0) or (Pin > 53) then
      raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPrintedPinInvalid, [Pin]));

    Result := Pin;
  end;
end;

procedure TFastGPIO.SetPinModeBCM(const PinBCM: Integer; const Mode: TPinModeEx);
var
  Shift: Integer;
begin
  Shift := (PinBCM mod 10) * 3;
  ChangeBitsSafe(GetOffsetPointer((Cardinal(PinBCM) div 10) * 4), Ord(Mode) shl Shift, $07 shl Shift);
end;

function TFastGPIO.GetPinMode(const Pin: Integer): TPinMode;
begin
  case GetPinModeEx(Pin) of
    TPinModeEx.Input:
      Result := TPinMode.Input;
    TPinModeEx.Output:
      Result := TPinMode.Output;
  else
    raise EGPIOAlternateFunctionPin.Create(Format(SGPIOSpecifiedBCMPinAlternativeMode, [Pin]));
  end;
end;

procedure TFastGPIO.SetPinMode(const Pin: Integer; const Mode: TPinMode);
begin
  if Mode = TPinMode.Output then
    SetPinModeEx(Pin, TPinModeEx.Output)
  else
    SetPinModeEx(Pin, TPinModeEx.Input);
end;

function TFastGPIO.GetPinValue(const Pin: Integer): TPinValue;
var
  PinBCM: Integer;
begin
  PinBCM := ProcessPinNumber(Pin);

  if ReadMemSafe(GetOffsetPointer($34 + (Cardinal(PinBCM) div 32) * 4)) and (1 shl (PinBCM mod 32)) > 0 then
    Result := TPinValue.High
  else
    Result := TPinValue.Low;
end;

procedure TFastGPIO.SetPinValue(const Pin: Integer; const Value: TPinValue);
var
  PinBCM: Integer;
  DestPtr: Pointer;
begin
  PinBCM := ProcessPinNumber(Pin);

  if Value = TPinValue.Low then
    DestPtr := GetOffsetPointer($28 + (Cardinal(PinBCM) div 32) * 4)
  else
    DestPtr := GetOffsetPointer($1C + (Cardinal(PinBCM) div 32) * 4);

  WriteMemSafe(DestPtr, 1 shl (PinBCM mod 32));
end;

function TFastGPIO.GetPinModeEx(const Pin: Integer): TPinModeEx;
var
  Address: Pointer;
  PinBCM: Integer;
begin
  PinBCM := ProcessPinNumber(Pin);
  Address := GetOffsetPointer((Cardinal(PinBCM) div 10) * 4);

  Result := TPinModeEx((ReadMemSafe(Address) shr ((PinBCM mod 10) * 3)) and $07);
end;

procedure TFastGPIO.SetPinModeEx(const Pin: Integer; const Value: TPinModeEx);
begin
  SetPinModeBCM(ProcessPinNumber(Pin), Value);
end;

procedure TFastGPIO.SetFastValue(const PinBCM: Integer; const Value: TPinValue);
var
  DestValue: PLongWord;
begin
  if Value = TPinValue.Low then
    DestValue := GetOffsetPointer($0028 + (Cardinal(PinBCM) shr 5) shl 2)
  else
    DestValue := GetOffsetPointer($001C + (Cardinal(PinBCM) shr 5) shl 2);

  DestValue^ := 1 shl (PinBCM and $1F);
end;

{$ENDREGION}
{$REGION 'TFastSPI'}

constructor TFastSPI.Create(const AFastGPIO: TFastGPIO;
  const AChipSelect: Integer; const AFrequency: Integer; const AMode: Integer);
var
  DestPtr: Pointer;
begin
  inherited Create;

  FFastGPIO := AFastGPIO;
  if FFastGPIO = nil then
    raise EGPIORefRequired.Create(ClassName + ExceptionClassNameSeparator + SGPIORefNotProvided);

  FMemory := PortionMap('SPI', FFastGPIO.SystemCore.Handle, GetChipOffsetSPI, TFastSystemCore.PageSize);

  FFastGPIO.SetPinModeBCM(7, TPinModeEx.Alt0);  // CE1
  FFastGPIO.SetPinModeBCM(8, TPinModeEx.Alt0);  // CE0
  FFastGPIO.SetPinModeBCM(9, TPinModeEx.Alt0);  // MISO
  FFastGPIO.SetPinModeBCM(10, TPinModeEx.Alt0); // MOSI
  FFastGPIO.SetPinModeBCM(11, TPinModeEx.Alt0); // SCLK

  DestPtr := GetOffsetPointer(OffsetControlStatus);
  WriteMemSafe(DestPtr, 0);
  WriteMemFast(DestPtr, MaskControlStatusClearBuffer);

  FChipSelect := -1;
  FMode := -1;
  FFrequency := -1;

  SetChipSelect(AChipSelect);
  SetMode(AMode);
  SetFrequency(AFrequency);
end;

destructor TFastSPI.Destroy;
begin
{$IFDEF DATAPORTS_PINS_RESET_AFTER_DONE}
  FFastGPIO.SetPinModeBCM(11, TPinModeEx.Input);
  FFastGPIO.SetPinModeBCM(10, TPinModeEx.Input);
  FFastGPIO.SetPinModeBCM(9, TPinModeEx.Input);
  FFastGPIO.SetPinModeBCM(8, TPinModeEx.Input);
  FFastGPIO.SetPinModeBCM(7, TPinModeEx.Input);
{$ENDIF}

  PortionUnmap(FMemory, TFastSystemCore.PageSize);

  inherited;
end;

function TFastSPI.GetChipOffsetSPI: TChipOffset;
begin
  Result := FFastGPIO.SystemCore.GetChipOffsetBase + $204;
end;

function TFastSPI.GetOffsetPointer(const Offset: Cardinal): Pointer;
begin
  Result := Pointer(PtrUInt(FMemory) + Offset);
end;

function TFastSPI.GetMode: Integer;
begin
  Result := FMode;
end;

procedure TFastSPI.SetMode(const Value: Integer);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    ChangeBitsSafe(GetOffsetPointer(OffsetControlStatus), FMode shl 2, MaskControlStatusClockPolarity or
      MaskControlStatusClockPhase);
  end;
end;

function TFastSPI.GetBitsPerWord: Integer;
begin
  Result := 8;
end;

procedure TFastSPI.SetBitsPerWord(const Value: Integer);
begin
  if Value <> 8 then
    raise ESPIUnsupportedBitsPerWord.Create(Format(SSPIUnsupportedBitsPerWord, [Value]));
end;

function TFastSPI.GetFrequency: Integer;
begin
  Result := FFrequency;
end;

procedure TFastSPI.SetFrequency(const Value: Integer);
var
  RealDivider: Single;
  ActualDivider: Integer;
begin
  if Value < 1 then
    raise ESPIUnsupportedFrequency.Create(Format(SSPIUnsupportedFrequency, [Value]));

  if FFrequency <> Value then
  begin
    RealDivider := TFastSystemCore.BaseClock / Value;

    ActualDivider := Round(Power(2, Round(Log2(RealDivider))));

    if (ActualDivider < 1) or (ActualDivider > 65536) then
      raise ESPIUnsupportedFrequency.Create(Format(SSPIUnsupportedFrequency, [Value]));

    if ActualDivider = 65536 then
      ActualDivider := 0;

    WriteMemSafe(GetOffsetPointer(OffsetClockDivider), ActualDivider);

    FFrequency := Value;
  end;
end;

function TFastSPI.GetChipSelectAttributes: TChipSelectAttributes;
begin
  Result := FChipSelectAttributes;
end;

procedure TFastSPI.SetChipSelectAttributes(const Value: TChipSelectAttributes);
var
  ActiveValue: Cardinal;
begin
  if FChipSelectAttributes <> Value then
  begin
    FChipSelectAttributes := Value;

    if TChipSelectAttribute.Disable in FChipSelectAttributes then
      ChangeBitsSafe(GetOffsetPointer(OffsetControlStatus), 3, MaskControlStatusChipSelect)
    else
    begin
      if TChipSelectAttribute.ActiveHigh in FChipSelectAttributes then
        ActiveValue := 1
      else
        ActiveValue := 0;

      ChangeBitsSafe(GetOffsetPointer(OffsetControlStatus), Ord(ActiveValue) shl 21, 1 shl 21);
      ChangeBitsSafe(GetOffsetPointer(OffsetControlStatus), Ord(ActiveValue) shl 22, 1 shl 22);
    end;
  end;
end;

procedure TFastSPI.SetChipSelect(const Value: Integer);
begin
  if (Value < 0) or (Value > 1) or (TChipSelectAttribute.Disable in FChipSelectAttributes) then
    raise ESPIUnsupportedChipSelect.Create(Format(SSPIUnsupportedChipSelect, [Value]));

  if FChipSelect <> Value then
  begin
    FChipSelect := Value;
    ChangeBitsSafe(GetOffsetPointer(OffsetControlStatus), FChipSelect, MaskControlStatusChipSelect);
  end;
end;

function TFastSPI.Read(const Buffer: Pointer; const BufferSize: Integer): Integer;
begin
  Result := Transfer(Buffer, nil, BufferSize);
end;

function TFastSPI.Write(const Buffer: Pointer; const BufferSize: Integer): Integer;
begin
  Result := Transfer(nil, Buffer, BufferSize);
end;

function TFastSPI.Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Integer): Integer;
var
  ControlStatusPtr, DataBufferPtr: Pointer;
  BytesRead, BytesWritten, BlockCounter: Integer;
  BlockTimeoutStart: UInt64;
begin
  ControlStatusPtr := GetOffsetPointer(OffsetControlStatus);
  DataBufferPtr := GetOffsetPointer(OffsetDataBuffer);

  // Clear FIFO buffers.
  ChangeBitsSafe(ControlStatusPtr, MaskControlStatusClearBuffer, MaskControlStatusClearBuffer);

  // Begin transfer (TA = 1)
  ChangeBitsSafe(ControlStatusPtr, MaskControlStatusTransfer, MaskControlStatusTransfer);
  try
    BytesRead := 0;
    BytesWritten := 0;
    BlockCounter := -1;

    while (BytesRead < BufferSize) or (BytesWritten < BufferSize) do
    begin
      // Send bytes.
      while (ReadMemSafe(ControlStatusPtr) and MaskControlStatusTXD <> 0) and (BytesWritten < BufferSize) do
      begin
        if WriteBuffer <> nil then
          WriteMemFast(DataBufferPtr, PByte(PtrUInt(WriteBuffer) + Cardinal(BytesWritten))^)
        else
          WriteMemFast(DataBufferPtr, 0);

        Inc(BytesWritten);
        BlockCounter := -1;
      end;

      // Receive bytes.
      while (ReadMemSafe(ControlStatusPtr) and MaskControlStatusRXD <> 0) and (BytesRead < BufferSize) do
      begin
        if ReadBuffer <> nil then
          PByte(PtrUInt(ReadBuffer) + Cardinal(BytesRead))^ := ReadMemFast(DataBufferPtr)
        else
          ReadMemFast(DataBufferPtr);

        Inc(BytesRead);
        BlockCounter := -1;
      end;

      { Apply a timeout to prevent hung up by adjusting ticks first and after certain fixed interval calculating the
        actual waiting time. }
      if BlockCounter = -1 then
        BlockCounter := 0
      else
      begin
        if BlockCounter = TransferBlockCounterStart then
          BlockTimeoutStart := FFastGPIO.SystemCore.GetTickCount;

        if BlockCounter >= TransferBlockCounterMax then
        begin
          if FFastGPIO.SystemCore.TicksInBetween(BlockTimeoutStart, FFastGPIO.SystemCore.GetTickCount) >
            TransferBlockTimeout then
            Exit(-1);
        end
        else
          Inc(BlockCounter);
      end
    end;

    // Wait for DONE flag to be set with a timeout to prevent hunging.
    BlockTimeoutStart := FFastGPIO.SystemCore.GetTickCount;

    while ReadMemFast(ControlStatusPtr) and MaskControlStatusDone = 0 do
      if FFastGPIO.SystemCore.TicksInBetween(BlockTimeoutStart, FFastGPIO.SystemCore.GetTickCount) >
        TransferBlockTimeout then
        Exit(-1);
  finally
    // End transfer (TA = 0)
    ChangeBitsSafe(ControlStatusPtr, 0, MaskControlStatusTransfer);
  end;

  Result := BufferSize;
end;

{$ENDREGION}
{$REGION 'TFastI2C'}

constructor TFastI2C.Create(const AFastGPIO: TFastGPIO);
begin
  inherited Create;

  FFastGPIO := AFastGPIO;
  if FFastGPIO = nil then
    raise EGPIORefRequired.Create(ClassName + ExceptionClassNameSeparator + SGPIORefNotProvided);

  FMemory := PortionMap('I2C', FFastGPIO.SystemCore.Handle, GetChipOffsetI2C, TFastSystemCore.PageSize);

  FFastGPIO.SetPinModeBCM(2, TPinModeEx.Alt0); // SDA
  FFastGPIO.SetPinModeBCM(3, TPinModeEx.Alt0); // SCL

  UpdateTimePerByte(ReadMemSafe(GetOffsetPointer(OffsetClockDivider)));
end;

destructor TFastI2C.Destroy;
begin
{$IFDEF DATAPORTS_PINS_RESET_AFTER_DONE}
  FFastGPIO.SetPinModeBCM(3, TPinModeEx.Input);
  FFastGPIO.SetPinModeBCM(2, TPinModeEx.Input);
{$ENDIF}

  PortionUnmap(FMemory, TFastSystemCore.PageSize);

  inherited;
end;

function TFastI2C.GetChipOffsetI2C: TChipOffset;
begin
  Result := FFastGPIO.SystemCore.GetChipOffsetBase + $804;
end;

procedure TFastI2C.UpdateTimePerByte(const ClockDivider: Cardinal);
const
  BitsPerByte = 9;
begin
  FTimePerByte := (UInt64(ClockDivider) * BitsPerByte * 1000000) div TFastSystemCore.BaseClock;
end;

procedure TFastI2C.SetFrequency(const Value: Integer);
var
  ClockDivider: Cardinal;
begin
  if Value < 1 then
    raise EI2CUnsupportedFrequency.Create(Format(SI2CUnsupportedFrequency, [Value]));

  if FFrequency <> Value then
  begin
    ClockDivider := TFastSystemCore.BaseClock div Cardinal(Value);

    if (ClockDivider < 1) or (ClockDivider > 65536) then
      raise EI2CUnsupportedFrequency.Create(Format(SI2CUnsupportedFrequency, [Value]));

    if ClockDivider = 65536 then
      ClockDivider := 0;

    WriteMemSafe(GetOffsetPointer(OffsetClockDivider), ClockDivider);

    FFrequency := Value;
    UpdateTimePerByte(ClockDivider);
  end;
end;

function TFastI2C.GetOffsetPointer(const Offset: Cardinal): Pointer;
begin
  Result := Pointer(PtrUInt(FMemory) + Offset);
end;

procedure TFastI2C.SetAddress(const Address: Integer);
begin
  WriteMemSafe(GetOffsetPointer(OffsetSlaveAddress), Address);
end;

function TFastI2C.ProcessBlockCounter(var BlockCounter: Integer; var BlockTimeoutStart: UInt64): Boolean;
begin
  if BlockCounter = -1 then
    BlockCounter := 0
  else
  begin
    if BlockCounter = TransferCounterStart then
      BlockTimeoutStart := FFastGPIO.SystemCore.GetTickCount;

    if BlockCounter >= TransferCounterMax then
    begin
      if FFastGPIO.SystemCore.TicksInBetween(BlockTimeoutStart, FFastGPIO.SystemCore.GetTickCount) > TransferTimeout then
        Exit(False);
    end
    else
      Inc(BlockCounter);
  end;

  Result := True;
end;

function TFastI2C.Read(const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  ControlPtr, StatusPtr, DataBufferPtr: Pointer;
  BytesRead, BlockCounter: Integer;
  BlockTimeoutStart: UInt64;
begin
  ControlPtr := GetOffsetPointer(OffsetControl);
  StatusPtr := GetOffsetPointer(OffsetStatus);
  DataBufferPtr := GetOffsetPointer(OffsetDataBuffer);

  // Clear FIFO and status flags.
  ChangeBitsSafe(ControlPtr, MaskControlClearBuffers, MaskControlClearBuffers);
  WriteMemFast(StatusPtr, MaskStatusTimeout or MaskStatusNoACK or MaskStatusDone);

  // Specify data buffer size.
  WriteMemFast(GetOffsetPointer(OffsetDataLength), BufferSize);

  // Begin reading operation and send "START" signal.
  WriteMemFast(ControlPtr, MaskControlEnabled or MaskControlStart or MaskControlRead);

  // Receive bytes.
  BytesRead := 0;
  BlockCounter := -1;

  while ReadMemFast(StatusPtr) and MaskStatusDone = 0 do
  begin
    while (ReadMemFast(StatusPtr) and MaskStatusRXD > 0) and (BytesRead < BufferSize) do
    begin
      PByte(PtrUInt(Buffer) + Cardinal(BytesRead))^ := ReadMemFast(DataBufferPtr);
      Inc(BytesRead);
      BlockCounter := -1;
    end;

    if not ProcessBlockCounter(BlockCounter, BlockTimeoutStart) then
      Exit(-1);
  end;

  // Retrieve any remaining bytes from FIFO buffer.
  while (ReadMemFast(StatusPtr) and MaskStatusRXD > 0) and (BytesRead < BufferSize) do
  begin
    PByte(PtrUInt(Buffer) + Cardinal(BytesRead))^ := ReadMemFast(DataBufferPtr);
    Inc(BytesRead);
  end;

  Result := BytesRead;

  // Check status flags for any issues during read.
  if ReadMemSafe(StatusPtr) and (MaskStatusNoACK or MaskStatusTimeout) > 0 then
    Result := -1;
end;

function TFastI2C.Write(const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  ControlPtr, StatusPtr, DataBufferPtr: Pointer;
  BytesWritten, BlockCounter: Integer;
  BlockTimeoutStart: UInt64;
begin
  ControlPtr := GetOffsetPointer(OffsetControl);
  StatusPtr := GetOffsetPointer(OffsetStatus);
  DataBufferPtr := GetOffsetPointer(OffsetDataBuffer);

  // Clear FIFO and status flags.
  ChangeBitsSafe(ControlPtr, MaskControlClearBuffers, MaskControlClearBuffers);
  WriteMemFast(StatusPtr, MaskStatusTimeout or MaskStatusNoACK or MaskStatusDone);

  // Specify data buffer size.
  WriteMemFast(GetOffsetPointer(OffsetDataLength), BufferSize);

  // Fill FIFO buffers as much as possible before starting transfer.
  BytesWritten := 0;

  while (BytesWritten < BufferSize) and (BytesWritten < MaxInternalBufferSize) do
  begin
    WriteMemFast(DataBufferPtr, PByte(PtrUInt(Buffer) + Cardinal(BytesWritten))^);
    Inc(BytesWritten);
  end;

  // Begin transfer and send "START" signal.
  WriteMemFast(ControlPtr, MaskControlEnabled or MaskControlStart);

  // Send bytes.
  BlockCounter := -1;

  while ReadMemFast(StatusPtr) and MaskStatusDone = 0 do
  begin
    while (ReadMemFast(StatusPtr) and MaskStatusTXD > 0) and (BytesWritten < BufferSize) do
    begin
      WriteMemFast(DataBufferPtr, PByte(PtrUInt(Buffer) + Cardinal(BytesWritten))^);
      Inc(BytesWritten);
      BlockCounter := -1;
    end;

    if not ProcessBlockCounter(BlockCounter, BlockTimeoutStart) then
      Exit(-1);
  end;

  Result := BytesWritten;

  // Check status flags for any issues during write.
  if ReadMemSafe(StatusPtr) and (MaskStatusNoACK or MaskStatusTimeout) > 0 then
    Result := -1;
end;

function TFastI2C.ReadBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  ControlPtr, StatusPtr, DataBufferPtr: Pointer;
  BytesRead, BlockCounter: Integer;
  BlockTimeoutStart: UInt64;
begin
  ControlPtr := GetOffsetPointer(OffsetControl);
  StatusPtr := GetOffsetPointer(OffsetStatus);
  DataBufferPtr := GetOffsetPointer(OffsetDataBuffer);

  // Clear FIFO and status flags.
  ChangeBitsSafe(ControlPtr, MaskControlClearBuffers, MaskControlClearBuffers);
  WriteMemFast(StatusPtr, MaskStatusTimeout or MaskStatusNoACK or MaskStatusDone);

  // Specify data length and write command to FIFO buffer.
  WriteMemFast(GetOffsetPointer(OffsetDataLength), 1);
  WriteMemFast(DataBufferPtr, Command);

  // Begin transfer and send "START" signal.
  WriteMemFast(ControlPtr, MaskControlEnabled or MaskControlStart);

  // Wait until the transfer has started (with timeout).
  BlockTimeoutStart := FFastGPIO.SystemCore.GetTickCount;

  while ReadMemFast(StatusPtr) and (MaskStatusTransfer or MaskStatusDone) = 0 do
    if FFastGPIO.SystemCore.TicksInBetween(BlockTimeoutStart, FFastGPIO.SystemCore.GetTickCount) > TransferTimeout then
      Exit(-1);

  // Specify data length for reading and send "REPEATED START" signal.
  WriteMemFast(GetOffsetPointer(OffsetDataLength), BufferSize);
  WriteMemFast(ControlPtr, MaskControlEnabled or MaskControlStart or MaskControlRead);

  // Wait until the command is sent and one byte is received.
  FFastGPIO.SystemCore.Delay(FTimePerByte * 2 * SizeOf(Byte));

  // Receive bytes.
  BytesRead := 0;
  BlockCounter := -1;

  while ReadMemFast(StatusPtr) and MaskStatusDone = 0 do
  begin
    while (ReadMemFast(StatusPtr) and MaskStatusRXD > 0) and (BytesRead < BufferSize) do
    begin
      PByte(PtrUInt(Buffer) + Cardinal(BytesRead))^ := ReadMemFast(DataBufferPtr);
      Inc(BytesRead);
      BlockCounter := -1;
    end;

    if not ProcessBlockCounter(BlockCounter, BlockTimeoutStart) then
      Exit(-1);
  end;

  // Retrieve any remaining bytes from FIFO buffer.
  while (ReadMemFast(StatusPtr) and MaskStatusRXD > 0) and (BytesRead < BufferSize) do
  begin
    PByte(PtrUInt(Buffer) + Cardinal(BytesRead))^ := ReadMemFast(DataBufferPtr);
    Inc(BytesRead);
  end;

  Result := BytesRead;

  // Check status flags for any issues during read.
  if ReadMemSafe(StatusPtr) and (MaskStatusNoACK or MaskStatusTimeout) > 0 then
    Result := -1;
end;

function TFastI2C.WriteBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  ControlPtr, StatusPtr, DataBufferPtr: Pointer;
  BytesWritten, BlockCounter: Integer;
  BlockTimeoutStart: UInt64;
begin
  ControlPtr := GetOffsetPointer(OffsetControl);
  StatusPtr := GetOffsetPointer(OffsetStatus);
  DataBufferPtr := GetOffsetPointer(OffsetDataBuffer);

  // Clear FIFO and status flags.
  ChangeBitsSafe(ControlPtr, MaskControlClearBuffers, MaskControlClearBuffers);
  WriteMemFast(StatusPtr, MaskStatusTimeout or MaskStatusNoACK or MaskStatusDone);

  // Specify data buffer size.
  WriteMemFast(GetOffsetPointer(OffsetDataLength), BufferSize + 1);

  // Fill FIFO buffers with the actual command and as much data as possible before starting transfer.
  WriteMemFast(DataBufferPtr, Command);
  BytesWritten := 0;

  while (BytesWritten < BufferSize) and (BytesWritten < MaxInternalBufferSize - 1) do
  begin
    WriteMemFast(DataBufferPtr, PByte(PtrUInt(Buffer) + Cardinal(BytesWritten))^);
    Inc(BytesWritten);
  end;

  // Begin transfer and send "START" signal.
  WriteMemFast(ControlPtr, MaskControlEnabled or MaskControlStart);

  // Send bytes.
  BlockCounter := -1;

  while ReadMemFast(StatusPtr) and MaskStatusDone = 0 do
  begin
    while (ReadMemFast(StatusPtr) and MaskStatusTXD > 0) and (BytesWritten < BufferSize) do
    begin
      WriteMemFast(DataBufferPtr, PByte(PtrUInt(Buffer) + Cardinal(BytesWritten))^);
      Inc(BytesWritten);
      BlockCounter := -1;
    end;

    if not ProcessBlockCounter(BlockCounter, BlockTimeoutStart) then
      Exit(-1);
  end;

  Result := BytesWritten;

  // Check status flags for any issues during write.
  if ReadMemSafe(StatusPtr) and (MaskStatusNoACK or MaskStatusTimeout) > 0 then
    Result := -1;
end;

{$ENDREGION}
{$REGION 'TDefaultUART'}

constructor TDefaultUART.Create(const AFastGPIO: TFastGPIO; const ASystemPath: StdString);
begin
  FFastGPIO := AFastGPIO;
  if (FFastGPIO <> nil) and (ASystemPath = DefaultSystemPath) then
  begin
    FFastGPIO.SetPinModeBCM(14, TPinModeEx.Alt0); // UART0_TXD
    FFastGPIO.SetPinModeBCM(15, TPinModeEx.Alt0); // UART0_RXD
  end;

  inherited Create(ASystemPath);
end;

destructor TDefaultUART.Destroy;
begin
{$IFDEF DATAPORTS_PINS_RESET_AFTER_DONE}
  if (FFastGPIO <> nil) and (SystemPath = DefaultSystemPath) then
  begin
    FFastGPIO.SetPinModeBCM(15, TPinModeEx.Input);
    FFastGPIO.SetPinModeBCM(14, TPinModeEx.Input);
  end;
{$ENDIF}

  inherited;
end;

{$ENDREGION}

end.

