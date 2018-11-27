unit PXL.Boards.Galileo;
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

  Thanks to Sergey Kiselev for creating a thorough analysis of Galileo Gen1 GPIO, including an indispensable diagram of
  pin multiplexing on his blog:
      http://www.malinov.com/Home/sergey-s-blog/intelgalileo-programminggpiofromlinux

  Thanks for Emutex Labs for providing a comprehensive diagram of Galileo Gen2 GPIO and multiplexing on their web site:
      http://www.emutexlabs.com/project/203-getting-started-with-intel-galileo-gen-2

  Additional information regarding GPIO pins, including multiplexing on Intel Edison, ideas about the data structure
  and finally some meaningful information on Fast I/O were found in "libmraa - Low Level Skeleton Library for
  Communication on GNU/Linux platforms" written by Brendan Le Foll and Thomas Ingleby:
      https://github.com/intel-iot-devkit/mraa
}

{
  Please remember that to use SPI on Intel Galileo Gen2 it is necessary to manually enable chip-select line, which is
  disabled by default for compatibility with Arduino. This can be done by editing "grub.conf" file:
      /media/mmcblk0p1/boot/grub/grub.conf

  Open that file, look for second "kernel" command line, which looks something like:
      "kernel /bzImage root=/dev/ram0 console=ttyS1,115200n8 ..."

  Append " intel_qrk_plat_galileo_gen2.gpio_cs=1" to that line, so it will end something like:
      "...debugshell=5 rootimage=image-full-galileo-clanton.ext3 intel_qrk_plat_galileo_gen2.gpio_cs=1"

  After doing this, reboot. It can be verified that SPI chip-select is working by trying to export gpio10, which
  should now produce an error:
      "echo 10 > /sys/class/gpio/export"
}
interface

{$INCLUDE PXL.Config.inc}

{ Enable the following option to automatically enable Fast I/O on pins that are capable of it, when setting their
  mode to "Output". }
{$DEFINE GalileoAutoFastIO}

uses
  PXL.TypeDef, PXL.Boards.Types, PXL.Sysfs.Types, PXL.Sysfs.GPIO, PXL.Sysfs.PWM, PXL.Sysfs.ADC, PXL.Sysfs.SPI,
  PXL.Sysfs.I2C, PXL.Sysfs.UART;

type
  TGalileoBoard = (Gen1, Gen2, Edison, Other);

  TGalileoGPIO = class(TCustomGPIO)
  protected type
    TPinOpMode = (Unknown, GPIO, FastIO, PWM, ADC, UART, SPI, I2C);
    TPinOpModes = set of TPinOpMode;

    TPinMux = record
      Pin: Integer;
      Value: TPinValue;
    end;

    TPinMuxes = array of TPinMux;

    TPinOpSetup = record
      Pin: Integer;
      ChipId: Integer;
      Muxes: TPinMuxes;
      OutputEnable: Integer;
      PullupEnable: Integer;
      MemBitPos: Integer;

      procedure Reset;
    end;

    TPinEntry = record
      Name: StdString;
      Modes: TPinOpModes;
      GPIO: TPinOpSetup;
      FastIO: TPinOpSetup;
      PWM: TPinOpSetup;
      ADC: TPinOpSetup;
      UART: TPinOpSetup;
      SPI: TPinOpSetup;
      I2C: TPinOpSetup;

      procedure Reset;
    end;

    TPinEntries = array of TPinEntry;

    TBusEntry = record
      Path: StdString;
      Pins: array of Integer;
    end;

    TBoardInfo = record
      Entries: TPinEntries;
      GPIO: TBusEntry;
      FastIO: TBusEntry;
      PWM: TBusEntry;
      ADC: TBusEntry;
      UART: TBusEntry;
      SPI: TBusEntry;
      I2C: TBusEntry;
      InternalLED: Integer;
    end;
  public type
    TFastIO = class
    private const
      DefaultPageSize = $1000;
    private
      FFileName: StdString;
      FHandle: TUntypedHandle;
      FMemory: Pointer;
    public
      constructor Create(const AFileName: StdString);
      destructor Destroy; override;

      property FileName: StdString read FFileName;
      property Handle: TUntypedHandle read FHandle;
      property Memory: Pointer read FMemory;
    end;
  private
    FBoard: TGalileoBoard;
    FSysfsGPIO: TSysfsGPIO;
    FFastIO: TFastIO;
    FPinModes: array of TPinOpMode;

    procedure InitPinModes;
    procedure SetupMuxes(const PinMuxes: TPinMuxes);
    function GetFastIO: TFastIO;
    function PinToGPIO(const Pin: Integer): Integer; inline;
    function GetInternalLEDPin: Integer; inline;
    function GetPinDrive(const Pin: Integer): TPinDrive;
    procedure SetPinDrive(const Pin: Integer; const Value: TPinDrive);
  protected
    FBoardInfo: TBoardInfo;

    function GetPinMode(const Pin: Integer): TPinMode; override;
    procedure SetPinMode(const Pin: Integer; const Mode: TPinMode); override;

    function GetPinValue(const Pin: Integer): TPinValue; override;
    procedure SetPinValue(const Pin: Integer; const Value: TPinValue); override;

    class procedure InitGalileo1Info(out Info: TBoardInfo);
    class procedure InitGalileo2Info(out Info: TBoardInfo);
    class procedure InitEdisonInfo(out Info: TBoardInfo);

    property FastIO: TFastIO read GetFastIO;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetFastOutput(const Pin: Integer);
    procedure SetFastValue(const Pin: Integer; const Value: TPinValue);

    property Board: TGalileoBoard read FBoard;
    property SysfsGPIO: TSysfsGPIO read FSysfsGPIO;
    property InternalLEDPin: Integer read GetInternalLEDPin;

    property PinDrive[const Pin: Integer]: TPinDrive read GetPinDrive write SetPinDrive;
  end;

  TGalileoPWM = class(TCustomPWM)
  private
    FGPIO: TGalileoGPIO;
    FSysfsPWM: TSysfsPWM;

    function PinToPWM(const Pin: Integer): Integer; inline;
  protected
    function GetEnabled(const Pin: Integer): Boolean; override;
    procedure SetEnabled(const Pin: Integer; const Value: Boolean); override;
    function GetPeriod(const Pin: Integer): Integer; override;
    procedure SetPeriod(const Pin, Value: Integer); override;
    function GetDutyCycle(const Pin: Integer): Integer; override;
    procedure SetDutyCycle(const Pin, Value: Integer); override;
  public
    constructor Create(const AGPIO: TGalileoGPIO);
    destructor Destroy; override;

    property GPIO: TGalileoGPIO read FGPIO;
    property SysfsPWM: TSysfsPWM read FSysfsPWM;
  end;

  TGalileoADC = class(TCustomADC)
  private
    FGPIO: TGalileoGPIO;
    FSysfsADC: TSysfsADC;
  protected
    function GetRawValue(const Channel: Integer): Integer; override;
  public
    constructor Create(const AGPIO: TGalileoGPIO);
    destructor Destroy; override;

    property GPIO: TGalileoGPIO read FGPIO;
    property SysfsADC: TSysfsADC read FSysfsADC;
  end;

  TGalileoSPI = class(TSysfsSPI)
  private
    FGPIO: TGalileoGPIO;
  public
    constructor Create(const AGPIO: TGalileoGPIO; const AFrequency: Integer = TSysfsSPI.DefaultFrequency;
      const AMode: Integer = TSysfsSPI.DefaultMode; const ABitsPerWord: Integer = TSysfsSPI.DefaultBitsPerWord);

    property GPIO: TGalileoGPIO read FGPIO;
  end;

  TGalileoI2C = class(TSysfsI2C)
  private
    FGPIO: TGalileoGPIO;
  public
    constructor Create(const AGPIO: TGalileoGPIO);

    property GPIO: TGalileoGPIO read FGPIO;
  end;

  TGalileoUART = class(TSysfsUART)
  private
    FGPIO: TGalileoGPIO;
  public
    constructor Create(const AGPIO: TGalileoGPIO);

    property GPIO: TGalileoGPIO read FGPIO;
  end;

  EPinNotConfiguredGPIO = class(EGPIOInvalidPin);
  EPinNotConfiguredPWM = class(EGPIOInvalidPin);
  EPinNotSupportFastIO = class(EGPIOInvalidPin);
  EGPIOReferenceInvalid = class(ESysfsGeneric);

resourcestring
  SPinNotConfiguredGPIO = 'The specified pin <%d> is not properly configured for GPIO.';
  SPinNotConfiguredPWM = 'The specified pin <%d> is not properly configured for PWM.';
  SPinNotSupportFastIO = 'The specified pin <%d> does not support fast I/O.';
  SGPIOReferenceInvalid = 'The specified GPIO reference is not valid.';

implementation

uses
  BaseUnix, SysUtils;

{$REGION 'TGalileoGPIO.TPinOpSetup'}

procedure TGalileoGPIO.TPinOpSetup.Reset;
begin
  Pin := -1;
  ChipId := 0;
  SetLength(Muxes, 0);
  OutputEnable := -1;
  PullupEnable := -1;
  MemBitPos := -1;
end;

{$ENDREGION}
{$REGION 'TGalileoGPIO.TPinEntry'}

procedure TGalileoGPIO.TPinEntry.Reset;
begin
  Name := '';
  Modes := [];
  GPIO.Reset;
  FastIO.Reset;
  PWM.Reset;
  ADC.Reset;
  UART.Reset;
  SPI.Reset;
  I2C.Reset;
end;

{$ENDREGION}
{$REGION 'TGalileoGPIO.TFastIO'}

constructor TGalileoGPIO.TFastIO.Create(const AFileName: StdString);
begin
  inherited Create;

  FFileName := AFileName;

  FHandle := fpopen(FFileName, O_RDWR or O_SYNC);
  if FHandle < 0 then
  begin
    FHandle := 0;
    raise ESysfsFileOpenReadWrite.Create(Format(SCannotOpenFileForReadingWriting, [FFileName]));
  end;

  FMemory := fpmmap(nil, DefaultPageSize, PROT_READ or PROT_WRITE, MAP_SHARED, FHandle, 0);
  if (FMemory = nil) or (FMemory = MAP_FAILED) then
  begin
    FMemory := nil;
    raise ESysfsFileMemoryMap.Create(Format(SCannotMemoryMapFile, [FFileName]));
  end;
end;

destructor TGalileoGPIO.TFastIO.Destroy;
begin
  if FMemory <> nil then
  begin
    fpmunmap(FMemory, DefaultPageSize);
    FMemory := nil;
  end;

  if FHandle <> 0 then
  begin
    fpclose(FHandle);
    FHandle := 0;
  end;

  inherited;
end;

{$ENDREGION}
{$REGION 'TGalileoGPIO'}

constructor TGalileoGPIO.Create;
var
  BoardName: StdString;
begin
  inherited Create;

  BoardName := Trim(ReadTextFromFile('/sys/devices/virtual/dmi/id/board_name'));

  if SameText(BoardName, 'GalileoGen2') then
    FBoard := TGalileoBoard.Gen2
  else if SameText(BoardName, 'BODEGA BAY') or SameText(BoardName, 'SALT BAY') then
    FBoard := TGalileoBoard.Edison
  else if SameText(BoardName, 'DE3815') or SameText(BoardName, 'NOTEBOOK') or
    SameText(BoardName, 'MinnowBoard MAX') then
    FBoard := TGalileoBoard.Other
  else
    FBoard := TGalileoBoard.Gen1;

  case FBoard of
    TGalileoBoard.Gen1:
      InitGalileo1Info(FBoardInfo);

    TGalileoBoard.Gen2:
      InitGalileo2Info(FBoardInfo);

    TGalileoBoard.Edison:
      InitEdisonInfo(FBoardInfo);
  end;

  InitPinModes;
  FSysfsGPIO := TSysfsGPIO.Create(FBoardInfo.GPIO.Path);
end;

destructor TGalileoGPIO.Destroy;
begin
  FreeAndNil(FFastIO);
  FSysfsGPIO.Free;

  inherited;
end;

procedure TGalileoGPIO.InitPinModes;
var
  I: Integer;
begin
  SetLength(FPinModes, Length(FBoardInfo.Entries));

  for I := 0 to Length(FPinModes) - 1 do
    FPinModes[I] := TPinOpMode.Unknown;
end;

procedure TGalileoGPIO.SetupMuxes(const PinMuxes: TPinMuxes);
var
  I: Integer;
begin
  for I := 0 to Length(PinMuxes) - 1 do
  begin
    FSysfsGPIO.TrySetPinMode(PinMuxes[I].Pin, TPinMode.Output);
    FSysfsGPIO.PinValue[PinMuxes[I].Pin] := PinMuxes[I].Value;
  end;
end;

function TGalileoGPIO.GetFastIO: TFastIO;
begin
  Result := FFastIO;
end;

function TGalileoGPIO.PinToGPIO(const Pin: Integer): Integer;
begin
  Result := FBoardInfo.Entries[Pin].GPIO.Pin;
end;

function TGalileoGPIO.GetInternalLEDPin: Integer;
begin
  Result := FBoardInfo.InternalLED;
end;

function TGalileoGPIO.GetPinMode(const Pin: Integer): TPinMode;
begin
  if (Pin < 0) or (Pin > Length(FPinModes)) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if FPinModes[Pin] <> TPinOpMode.GPIO then
    raise EPinNotConfiguredGPIO.Create(Format(SPinNotConfiguredGPIO, [Pin]));

  Result := FSysfsGPIO.PinMode[PinToGPIO(Pin)];
end;

procedure TGalileoGPIO.SetPinMode(const Pin: Integer; const Mode: TPinMode);
begin
  if (Pin < 0) or (Pin > Length(FPinModes)) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if FPinModes[Pin] <> TPinOpMode.GPIO then
  begin
    if FBoardInfo.Entries[Pin].GPIO.OutputEnable <> -1 then
    begin
      if Mode = TPinMode.Output then
        FSysfsGPIO.SetMux(FBoardInfo.Entries[Pin].GPIO.OutputEnable, TPinValue.Low)
      else
        FSysfsGPIO.SetMux(FBoardInfo.Entries[Pin].GPIO.OutputEnable, TPinValue.High);
    end;

    if FBoardInfo.Entries[Pin].GPIO.PullupEnable <> -1 then
      FSysfsGPIO.PinMode[FBoardInfo.Entries[Pin].GPIO.PullupEnable] := TPinMode.Input;

    SetupMuxes(FBoardInfo.Entries[Pin].GPIO.Muxes);
    FPinModes[Pin] := TPinOpMode.GPIO;
  end;

{$IFDEF GalileoAutoFastIO}
  if (Mode = TPinMode.Output) and (TPinOpMode.FastIO in FBoardInfo.Entries[Pin].Modes) then
    SetFastOutput(Pin)
  else
    FSysfsGPIO.PinMode[PinToGPIO(Pin)] := Mode;
{$ELSE}
  FSysfsGPIO.PinMode[PinToGPIO(Pin)] := Mode;
{$ENDIF}
end;

function TGalileoGPIO.GetPinValue(const Pin: Integer): TPinValue;
begin
  if (Pin < 0) or (Pin > Length(FPinModes)) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if FPinModes[Pin] <> TPinOpMode.GPIO then
    raise EPinNotConfiguredGPIO.Create(Format(SPinNotConfiguredGPIO, [Pin]));

  Result := FSysfsGPIO.PinValue[PinToGPIO(Pin)];
end;

procedure TGalileoGPIO.SetPinValue(const Pin: Integer; const Value: TPinValue);
begin
  if (Pin < 0) or (Pin > Length(FPinModes)) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if FPinModes[Pin] = TPinOpMode.FastIO then
    SetFastValue(Pin, Value)
  else if FPinModes[Pin] = TPinOpMode.GPIO then
    FSysfsGPIO.PinValue[PinToGPIO(Pin)] := Value
  else
    raise EPinNotConfiguredGPIO.Create(Format(SPinNotConfiguredGPIO, [Pin]));
end;

function TGalileoGPIO.GetPinDrive(const Pin: Integer): TPinDrive;
begin
  if (Pin < 0) or (Pin > Length(FPinModes)) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if FPinModes[Pin] <> TPinOpMode.GPIO then
    raise EPinNotConfiguredGPIO.Create(Format(SPinNotConfiguredGPIO, [Pin]));

  Result := FSysfsGPIO.PinDrive[PinToGPIO(Pin)];
end;

procedure TGalileoGPIO.SetPinDrive(const Pin: Integer; const Value: TPinDrive);
begin
  if (Pin < 0) or (Pin > Length(FPinModes)) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if FPinModes[Pin] <> TPinOpMode.GPIO then
    raise EPinNotConfiguredGPIO.Create(Format(SPinNotConfiguredGPIO, [Pin]));

  FSysfsGPIO.PinDrive[PinToGPIO(Pin)] := Value;
end;

procedure TGalileoGPIO.SetFastOutput(const Pin: Integer);
begin
  if (Pin < 0) or (Pin > Length(FPinModes)) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if not (TPinOpMode.FastIO in FBoardInfo.Entries[Pin].Modes) then
    raise EPinNotSupportFastIO.Create(Format(SPinNotSupportFastIO, [Pin]));

  if FPinModes[Pin] <> TPinOpMode.FastIO then
  begin
    SetupMuxes(FBoardInfo.Entries[Pin].FastIO.Muxes);
    FPinModes[Pin] := TPinOpMode.FastIO;
  end;

  if FFastIO = nil then
    FFastIO := TFastIO.Create(FBoardInfo.FastIO.Path);
end;

procedure TGalileoGPIO.SetFastValue(const Pin: Integer; const Value: TPinValue);
begin
  if Value = TPinValue.High then
    PLongWord(FFastIO.Memory)^ := PLongWord(FFastIO.Memory)^ or
      (LongWord(1) shl FBoardInfo.Entries[Pin].FastIO.MemBitPos)
  else
    PLongWord(FFastIO.Memory)^ := PLongWord(FFastIO.Memory)^ and
      (not (LongWord(1) shl FBoardInfo.Entries[Pin].FastIO.MemBitPos));
end;

class procedure TGalileoGPIO.InitGalileo1Info(out Info: TBoardInfo);
var
  I: Integer;
begin
  with Info do
  begin
    SetLength(Entries, 21);

    with Entries[0] do
    begin
      Reset;
      Name := 'IO0';
      Modes := [TPinOpMode.GPIO, TPinOpMode.UART];
      with GPIO do
      begin
        Pin := 50;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 40;
        Muxes[0].Value := TPinValue.High;
      end;
      with UART do
      begin
        Pin := 0;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 40;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[1] do
    begin
      Reset;
      Name := 'IO1';
      Modes := [TPinOpMode.GPIO, TPinOpMode.UART];
      with GPIO do
      begin
        Pin := 51;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 41;
        Muxes[0].Value := TPinValue.High;
      end;
      with UART do
      begin
        Pin := 0;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 41;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[2] do
    begin
      Reset;
      Name := 'IO2';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO];
      with GPIO do
      begin
        Pin := 32;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 31;
        Muxes[0].Value := TPinValue.High;
      end;
      with FastIO do
      begin
        Pin := 14;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 31;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 14;
        Muxes[1].Value := TPinValue.Low;
        MemBitPos := 6;
      end;
    end;

    with Entries[3] do
    begin
      Reset;
      Name := 'IO3';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.PWM];
      with GPIO do
      begin
        Pin := 18;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 30;
        Muxes[0].Value := TPinValue.High;
      end;
      with FastIO do
      begin
        Pin := 15;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 30;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 15;
        Muxes[1].Value := TPinValue.Low;
        MemBitPos := 7;
      end;
      with PWM do
      begin
        Pin := 3;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 30;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[4] do
    begin
      Reset;
      Name := 'IO4';
      Modes := [TPinOpMode.GPIO];
      GPIO.Pin := 28;
    end;

    with Entries[5] do
    begin
      Reset;
      Name := 'IO5';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM];
      GPIO.Pin := 17;
      PWM.Pin := 5;
    end;

    with Entries[6] do
    begin
      Reset;
      Name := 'IO6';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM];
      GPIO.Pin := 24;
      PWM.Pin := 6;
    end;

    with Entries[7] do
    begin
      Reset;
      Name := 'IO7';
      Modes := [TPinOpMode.GPIO];
      GPIO.Pin := 27;
    end;

    with Entries[8] do
    begin
      Reset;
      Name := 'IO8';
      Modes := [TPinOpMode.GPIO];
      GPIO.Pin := 26;
    end;

    with Entries[9] do
    begin
      Reset;
      Name := 'IO9';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM];
      GPIO.Pin := 19;
      PWM.Pin := 1;
    end;

    with Entries[10] do
    begin
      Reset;
      Name := 'IO10';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 16;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 42;
        Muxes[0].Value := TPinValue.High;
      end;
      with PWM do
      begin
        Pin := 7;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 42;
        Muxes[0].Value := TPinValue.High;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 42;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[11] do
    begin
      Reset;
      Name := 'IO11';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 25;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 43;
        Muxes[0].Value := TPinValue.High;
      end;
      with PWM do
      begin
        Pin := 4;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 43;
        Muxes[0].Value := TPinValue.High;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 43;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[12] do
    begin
      Reset;
      Name := 'IO12';
      Modes := [TPinOpMode.GPIO, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 38;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 54;
        Muxes[0].Value := TPinValue.High;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 54;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[13] do
    begin
      Reset;
      Name := 'IO13';
      Modes := [TPinOpMode.GPIO, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 39;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 55;
        Muxes[0].Value := TPinValue.High;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 55;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[14] do
    begin
      Reset;
      Name := 'A0';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 44;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 37;
        Muxes[0].Value := TPinValue.High;
      end;
      with ADC do
      begin
        Pin := 0;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 37;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[15] do
    begin
      Reset;
      Name := 'A1';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 45;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 36;
        Muxes[0].Value := TPinValue.High;
      end;
      with ADC do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 36;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[16] do
    begin
      Reset;
      Name := 'A2';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 46;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 23;
        Muxes[0].Value := TPinValue.High;
      end;
      with ADC do
      begin
        Pin := 2;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 23;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[17] do
    begin
      Reset;
      Name := 'A3';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 47;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 22;
        Muxes[0].Value := TPinValue.High;
      end;
      with ADC do
      begin
        Pin := 3;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 22;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[18] do
    begin
      Reset;
      Name := 'A4';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC, TPinOpMode.I2C];
      with GPIO do
      begin
        Pin := 48;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 29;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 21;
        Muxes[1].Value := TPinValue.High;
      end;
      with ADC do
      begin
        Pin := 4;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 29;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 21;
        Muxes[1].Value := TPinValue.Low;
      end;
      with I2C do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 29;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[19] do
    begin
      Reset;
      Name := 'A5';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC, TPinOpMode.I2C];
      with GPIO do
      begin
        Pin := 49;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 29;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 20;
        Muxes[1].Value := TPinValue.High;
      end;
      with ADC do
      begin
        Pin := 5;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 29;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 20;
        Muxes[1].Value := TPinValue.Low;
      end;
      with I2C do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 29;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[20] do
    begin
      Reset;
      Name := 'IL';
      Modes := [TPinOpMode.GPIO];
      GPIO.Pin := 13;
    end;

    with GPIO do
    begin
      Path := '/sys/class/gpio';
      SetLength(Pins, 20);
      for I := 0 to 19 do
        Pins[I] := I;
    end;

    with FastIO do
    begin
      Path := '/dev/uio1';
      SetLength(Pins, 2);
      Pins[0] := 2;
      Pins[1] := 3;
    end;

    with PWM do
    begin
      Path := '/sys/class/pwm/pwmchip0';
      SetLength(Pins, 6);
      Pins[0] := 3;
      Pins[1] := 5;
      Pins[2] := 6;
      Pins[3] := 9;
      Pins[4] := 10;
      Pins[5] := 11;
    end;

    with ADC do
    begin
      Path := '/sys/bus/iio/devices/iio:device0';
      SetLength(Pins, 6);
      Pins[0] := 14;
      Pins[1] := 15;
      Pins[2] := 16;
      Pins[3] := 17;
      Pins[4] := 18;
      Pins[5] := 19;
    end;

    with UART do
    begin
      Path := '/dev/ttyS0';
      SetLength(Pins, 2);
      Pins[0] := 0;
      Pins[1] := 1;
    end;

    with SPI do
    begin
      Path := '/dev/spidev1.0';
      SetLength(Pins, 4);
      Pins[0] := 10;
      Pins[1] := 11;
      Pins[2] := 12;
      Pins[3] := 13;
    end;

    with I2C do
    begin
      Path := '/dev/i2c-0';
      SetLength(Pins, 2);
      Pins[0] := 18;
      Pins[1] := 19;
    end;

    InternalLED := 20;
  end;
end;

class procedure TGalileoGPIO.InitGalileo2Info(out Info: TBoardInfo);
var
  I: Integer;
begin
  with Info do
  begin
    SetLength(Entries, 21);

    with Entries[0] do
    begin
      Reset;
      Name := 'IO0';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.UART];
      with GPIO do
      begin
        Pin := 11;
        OutputEnable := 32;
        PullupEnable := 33;
      end;
      with FastIO do
      begin
        Pin := 11;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 32;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 11;
        Muxes[1].Value := TPinValue.Low;
        MemBitPos := 3;
      end;
      with UART do
      begin
        Pin := 0;
      end;
    end;

    with Entries[1] do
    begin
      Reset;
      Name := 'IO1';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.UART];
      with GPIO do
      begin
        Pin := 12;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 45;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 28;
        PullupEnable := 29;
      end;
      with FastIO do
      begin
        Pin := 12;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 45;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 28;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 12;
        Muxes[2].Value := TPinValue.Low;
        MemBitPos := 4;
      end;
      with UART do
      begin
        Pin := 0;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 45;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[2] do
    begin
      Reset;
      Name := 'IO2';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO];
      with GPIO do
      begin
        Pin := 13;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 77;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 34;
        PullupEnable := 35;
      end;
      with FastIO do
      begin
        Pin := 13;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 77;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 34;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 13;
        Muxes[2].Value := TPinValue.Low;
        MemBitPos := 5;
      end;
    end;

    with Entries[3] do
    begin
      Reset;
      Name := 'IO3';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.PWM];
      with GPIO do
      begin
        Pin := 14;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 76;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 64;
        Muxes[1].Value := TPinValue.Low;
        OutputEnable := 16;
        PullupEnable := 17;
      end;
      with FastIO do
      begin
        Pin := 14;
        SetLength(Muxes, 4);
        Muxes[0].Pin := 76;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 64;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 16;
        Muxes[2].Value := TPinValue.Low;
        Muxes[3].Pin := 14;
        Muxes[3].Value := TPinValue.Low;
        MemBitPos := 6;
      end;
      with PWM do
      begin
        Pin := 1;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 76;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 64;
        Muxes[1].Value := TPinValue.High;
        Muxes[2].Pin := 16;
        Muxes[2].Value := TPinValue.Low;
      end;
    end;

    with Entries[4] do
    begin
      Reset;
      Name := 'IO4';
      Modes := [TPinOpMode.GPIO];
      with GPIO do
      begin
        Pin := 6;
        OutputEnable := 36;
        PullupEnable := 37;
      end;
    end;

    with Entries[5] do
    begin
      Reset;
      Name := 'IO5';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM];
      with GPIO do
      begin
        Pin := 0;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 66;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 18;
        PullupEnable := 19;
      end;
      with PWM do
      begin
        Pin := 3;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 66;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 18;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[6] do
    begin
      Reset;
      Name := 'IO6';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM];
      with GPIO do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 68;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 20;
        PullupEnable := 21;
      end;
      with PWM do
      begin
        Pin := 5;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 68;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 20;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[7] do
    begin
      Reset;
      Name := 'IO7';
      Modes := [TPinOpMode.GPIO];
      with GPIO do
      begin
        Pin := 38;
        PullupEnable := 39;
      end;
    end;

    with Entries[8] do
    begin
      Reset;
      Name := 'IO8';
      Modes := [TPinOpMode.GPIO];
      with GPIO do
      begin
        Pin := 40;
        PullupEnable := 41;
      end;
    end;

    with Entries[9] do
    begin
      Reset;
      Name := 'IO9';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM];
      with GPIO do
      begin
        Pin := 4;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 70;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 22;
        PullupEnable := 23;
      end;
      with PWM do
      begin
        Pin := 7;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 70;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 22;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[10] do
    begin
      Reset;
      Name := 'IO10';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.PWM, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 10;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 74;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 26;
        PullupEnable := 27;
      end;
      with FastIO do
      begin
        Pin := 10;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 74;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 26;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 10;
        Muxes[2].Value := TPinValue.Low;
        MemBitPos := 2;
      end;
      with PWM do
      begin
        Pin := 11;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 74;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 26;
        Muxes[1].Value := TPinValue.Low;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 74;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 26;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[11] do
    begin
      Reset;
      Name := 'IO11';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 5;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 72;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 44;
        Muxes[1].Value := TPinValue.Low;
        OutputEnable := 24;
        PullupEnable := 25;
      end;
      with PWM do
      begin
        Pin := 9;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 72;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 44;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 24;
        Muxes[2].Value := TPinValue.Low;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 72;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 44;
        Muxes[1].Value := TPinValue.High;
        Muxes[2].Pin := 24;
        Muxes[2].Value := TPinValue.Low;
      end;
    end;

    with Entries[12] do
    begin
      Reset;
      Name := 'IO12';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 15;
        OutputEnable := 42;
        PullupEnable := 43;
      end;
      with FastIO do
      begin
        Pin := 15;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 42;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 15;
        Muxes[1].Value := TPinValue.Low;
        MemBitPos := 7;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 42;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[13] do
    begin
      Reset;
      Name := 'IO13';
      Modes := [TPinOpMode.GPIO, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 7;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 46;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 30;
        PullupEnable := 31;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 46;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 30;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[14] do
    begin
      Reset;
      Name := 'A0';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 48;
        PullupEnable := 49;
      end;
      with ADC do
      begin
        Pin := 0;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 49;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[15] do
    begin
      Reset;
      Name := 'A1';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 50;
        PullupEnable := 51;
      end;
      with ADC do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 51;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[16] do
    begin
      Reset;
      Name := 'A2';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 52;
        PullupEnable := 53;
      end;
      with ADC do
      begin
        Pin := 2;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 53;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[17] do
    begin
      Reset;
      Name := 'A3';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 54;
        PullupEnable := 55;
      end;
      with ADC do
      begin
        Pin := 3;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 55;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[18] do
    begin
      Reset;
      Name := 'A4';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC, TPinOpMode.I2C];
      with GPIO do
      begin
        Pin := 56;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 78;
        Muxes[1].Value := TPinValue.High;
        PullupEnable := 57;
      end;
      with ADC do
      begin
        Pin := 4;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 78;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 57;
        Muxes[2].Value := TPinValue.High;
      end;
      with I2C do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[19] do
    begin
      Reset;
      Name := 'A5';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC, TPinOpMode.I2C];
      with GPIO do
      begin
        Pin := 58;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 79;
        Muxes[1].Value := TPinValue.High;
        PullupEnable := 59;
      end;
      with ADC do
      begin
        Pin := 5;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 79;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 59;
        Muxes[2].Value := TPinValue.High;
      end;
      with I2C do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[20] do
    begin
      Reset;
      Name := 'IL';
      Modes := [TPinOpMode.GPIO];
      GPIO.Pin := 3;
    end;

    with GPIO do
    begin
      Path := '/sys/class/gpio';
      SetLength(Pins, 20);
      for I := 0 to 19 do
        Pins[I] := I;
    end;

    with FastIO do
    begin
      Path := '/dev/uio1';
      SetLength(Pins, 6);
      Pins[0] := 0;
      Pins[1] := 1;
      Pins[2] := 2;
      Pins[3] := 3;
      Pins[4] := 10;
      Pins[5] := 12;
    end;

    with PWM do
    begin
      Path := '/sys/class/pwm/pwmchip0';
      SetLength(Pins, 6);
      Pins[0] := 3;
      Pins[1] := 5;
      Pins[2] := 6;
      Pins[3] := 9;
      Pins[4] := 10;
      Pins[5] := 11;
    end;

    with ADC do
    begin
      Path := '/sys/bus/iio/devices/iio:device0';
      SetLength(Pins, 6);
      Pins[0] := 14;
      Pins[1] := 15;
      Pins[2] := 16;
      Pins[3] := 17;
      Pins[4] := 18;
      Pins[5] := 19;
    end;

    with UART do
    begin
      Path := '/dev/ttyS0';
      SetLength(Pins, 2);
      Pins[0] := 0;
      Pins[1] := 1;
    end;

    with SPI do
    begin
      Path := '/dev/spidev1.0';
      SetLength(Pins, 4);
      Pins[0] := 10;
      Pins[1] := 11;
      Pins[2] := 12;
      Pins[3] := 13;
    end;

    with I2C do
    begin
      Path := '/dev/i2c-0';
      SetLength(Pins, 2);
      Pins[0] := 18;
      Pins[1] := 19;
    end;

    InternalLED := 13;
  end;
end;

class procedure TGalileoGPIO.InitEdisonInfo(out Info: TBoardInfo);
var
  I: Integer;
begin
  with Info do
  begin
    SetLength(Entries, 21);

    with Entries[0] do
    begin
      Reset;
      Name := 'IO0';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.UART];
      with GPIO do
      begin
        Pin := 11;
        OutputEnable := 32;
        PullupEnable := 33;
      end;
      with FastIO do
      begin
        Pin := 11;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 32;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 11;
        Muxes[1].Value := TPinValue.Low;
        MemBitPos := 3;
      end;
      with UART do
      begin
        Pin := 0;
      end;
    end;

    with Entries[1] do
    begin
      Reset;
      Name := 'IO1';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.UART];
      with GPIO do
      begin
        Pin := 12;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 45;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 28;
        PullupEnable := 29;
      end;
      with FastIO do
      begin
        Pin := 12;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 45;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 28;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 12;
        Muxes[2].Value := TPinValue.Low;
        MemBitPos := 4;
      end;
      with UART do
      begin
        Pin := 0;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 45;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[2] do
    begin
      Reset;
      Name := 'IO2';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO];
      with GPIO do
      begin
        Pin := 13;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 77;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 34;
        PullupEnable := 35;
      end;
      with FastIO do
      begin
        Pin := 13;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 77;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 34;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 13;
        Muxes[2].Value := TPinValue.Low;
        MemBitPos := 5;
      end;
    end;

    with Entries[3] do
    begin
      Reset;
      Name := 'IO3';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.PWM];
      with GPIO do
      begin
        Pin := 14;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 76;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 64;
        Muxes[1].Value := TPinValue.Low;
        OutputEnable := 16;
        PullupEnable := 17;
      end;
      with FastIO do
      begin
        Pin := 14;
        SetLength(Muxes, 4);
        Muxes[0].Pin := 76;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 64;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 16;
        Muxes[2].Value := TPinValue.Low;
        Muxes[3].Pin := 14;
        Muxes[3].Value := TPinValue.Low;
        MemBitPos := 6;
      end;
      with PWM do
      begin
        Pin := 1;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 76;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 64;
        Muxes[1].Value := TPinValue.High;
        Muxes[2].Pin := 16;
        Muxes[2].Value := TPinValue.Low;
      end;
    end;

    with Entries[4] do
    begin
      Reset;
      Name := 'IO4';
      Modes := [TPinOpMode.GPIO];
      with GPIO do
      begin
        Pin := 6;
        OutputEnable := 36;
        PullupEnable := 37;
      end;
    end;

    with Entries[5] do
    begin
      Reset;
      Name := 'IO5';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM];
      with GPIO do
      begin
        Pin := 0;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 66;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 18;
        PullupEnable := 19;
      end;
      with PWM do
      begin
        Pin := 3;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 66;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 18;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[6] do
    begin
      Reset;
      Name := 'IO6';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM];
      with GPIO do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 68;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 20;
        PullupEnable := 21;
      end;
      with PWM do
      begin
        Pin := 5;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 68;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 20;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[7] do
    begin
      Reset;
      Name := 'IO7';
      Modes := [TPinOpMode.GPIO];
      with GPIO do
      begin
        Pin := 38;
        PullupEnable := 39;
      end;
    end;

    with Entries[8] do
    begin
      Reset;
      Name := 'IO8';
      Modes := [TPinOpMode.GPIO];
      with GPIO do
      begin
        Pin := 40;
        PullupEnable := 41;
      end;
    end;

    with Entries[9] do
    begin
      Reset;
      Name := 'IO9';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM];
      with GPIO do
      begin
        Pin := 4;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 70;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 22;
        PullupEnable := 23;
      end;
      with PWM do
      begin
        Pin := 7;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 70;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 22;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[10] do
    begin
      Reset;
      Name := 'IO10';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.PWM, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 10;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 74;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 26;
        PullupEnable := 27;
      end;
      with FastIO do
      begin
        Pin := 10;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 74;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 26;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 10;
        Muxes[2].Value := TPinValue.Low;
        MemBitPos := 2;
      end;
      with PWM do
      begin
        Pin := 11;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 74;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 26;
        Muxes[1].Value := TPinValue.Low;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 74;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 26;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[11] do
    begin
      Reset;
      Name := 'IO11';
      Modes := [TPinOpMode.GPIO, TPinOpMode.PWM, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 5;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 72;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 44;
        Muxes[1].Value := TPinValue.Low;
        OutputEnable := 24;
        PullupEnable := 25;
      end;
      with PWM do
      begin
        Pin := 9;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 72;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 44;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 24;
        Muxes[2].Value := TPinValue.Low;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 72;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 44;
        Muxes[1].Value := TPinValue.High;
        Muxes[2].Pin := 24;
        Muxes[2].Value := TPinValue.Low;
      end;
    end;

    with Entries[12] do
    begin
      Reset;
      Name := 'IO12';
      Modes := [TPinOpMode.GPIO, TPinOpMode.FastIO, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 15;
        OutputEnable := 42;
        PullupEnable := 43;
      end;
      with FastIO do
      begin
        Pin := 15;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 42;
        Muxes[0].Value := TPinValue.Low;
        Muxes[1].Pin := 15;
        Muxes[1].Value := TPinValue.Low;
        MemBitPos := 7;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 42;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[13] do
    begin
      Reset;
      Name := 'IO13';
      Modes := [TPinOpMode.GPIO, TPinOpMode.SPI];
      with GPIO do
      begin
        Pin := 7;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 46;
        Muxes[0].Value := TPinValue.Low;
        OutputEnable := 30;
        PullupEnable := 31;
      end;
      with SPI do
      begin
        Pin := 1;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 46;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 30;
        Muxes[1].Value := TPinValue.Low;
      end;
    end;

    with Entries[14] do
    begin
      Reset;
      Name := 'A0';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 48;
        PullupEnable := 49;
      end;
      with ADC do
      begin
        Pin := 0;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 49;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[15] do
    begin
      Reset;
      Name := 'A1';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 50;
        PullupEnable := 51;
      end;
      with ADC do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 51;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[16] do
    begin
      Reset;
      Name := 'A2';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 52;
        PullupEnable := 53;
      end;
      with ADC do
      begin
        Pin := 2;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 53;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[17] do
    begin
      Reset;
      Name := 'A3';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC];
      with GPIO do
      begin
        Pin := 54;
        PullupEnable := 55;
      end;
      with ADC do
      begin
        Pin := 3;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 55;
        Muxes[0].Value := TPinValue.High;
      end;
    end;

    with Entries[18] do
    begin
      Reset;
      Name := 'A4';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC, TPinOpMode.I2C];
      with GPIO do
      begin
        Pin := 56;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 78;
        Muxes[1].Value := TPinValue.High;
        PullupEnable := 57;
      end;
      with ADC do
      begin
        Pin := 4;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 78;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 57;
        Muxes[2].Value := TPinValue.High;
      end;
      with I2C do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[19] do
    begin
      Reset;
      Name := 'A5';
      Modes := [TPinOpMode.GPIO, TPinOpMode.ADC, TPinOpMode.I2C];
      with GPIO do
      begin
        Pin := 58;
        SetLength(Muxes, 2);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 79;
        Muxes[1].Value := TPinValue.High;
        PullupEnable := 59;
      end;
      with ADC do
      begin
        Pin := 5;
        SetLength(Muxes, 3);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.High;
        Muxes[1].Pin := 79;
        Muxes[1].Value := TPinValue.Low;
        Muxes[2].Pin := 59;
        Muxes[2].Value := TPinValue.High;
      end;
      with I2C do
      begin
        Pin := 1;
        SetLength(Muxes, 1);
        Muxes[0].Pin := 60;
        Muxes[0].Value := TPinValue.Low;
      end;
    end;

    with Entries[20] do
    begin
      Reset;
      Name := 'IL';
      Modes := [TPinOpMode.GPIO];
      GPIO.Pin := 3;
    end;

    with GPIO do
    begin
      Path := '/sys/class/gpio';
      SetLength(Pins, 20);
      for I := 0 to 19 do
        Pins[I] := I;
    end;

    with FastIO do
    begin
      Path := '/dev/uio1';
      SetLength(Pins, 6);
      Pins[0] := 0;
      Pins[1] := 1;
      Pins[2] := 2;
      Pins[3] := 3;
      Pins[4] := 10;
      Pins[5] := 12;
    end;

    with PWM do
    begin
      Path := '/sys/class/pwm/pwmchip0';
      SetLength(Pins, 6);
      Pins[0] := 3;
      Pins[1] := 5;
      Pins[2] := 6;
      Pins[3] := 9;
      Pins[4] := 10;
      Pins[5] := 11;
    end;

    with ADC do
    begin
      Path := '/sys/bus/iio/devices/iio:device0';
      SetLength(Pins, 6);
      Pins[0] := 14;
      Pins[1] := 15;
      Pins[2] := 16;
      Pins[3] := 17;
      Pins[4] := 18;
      Pins[5] := 19;
    end;

    with UART do
    begin
      Path := '/dev/ttyS0';
      SetLength(Pins, 2);
      Pins[0] := 0;
      Pins[1] := 1;
    end;

    with SPI do
    begin
      Path := '/dev/spidev1.0';
      SetLength(Pins, 4);
      Pins[0] := 10;
      Pins[1] := 11;
      Pins[2] := 12;
      Pins[3] := 13;
    end;

    with I2C do
    begin
      Path := '/dev/i2c-0';
      SetLength(Pins, 2);
      Pins[0] := 18;
      Pins[1] := 19;
    end;

    InternalLED := 13;
  end;
end;

{$ENDREGION}
{$REGION 'TGalileoPWM'}

constructor TGalileoPWM.Create(const AGPIO: TGalileoGPIO);
begin
  inherited Create;

  FGPIO := AGPIO;
  if FGPIO = nil then
    raise EGPIOReferenceInvalid.Create(ClassName + ExceptionClassNameSeparator + SGPIOReferenceInvalid);

  FSysfsPWM := TSysfsPWM.Create(FGPIO.FBoardInfo.PWM.Path);
end;

destructor TGalileoPWM.Destroy;
begin
  FSysfsPWM.Free;

  inherited;
end;

function TGalileoPWM.PinToPWM(const Pin: Integer): Integer;
begin
  Result := FGPIO.FBoardInfo.Entries[Pin].PWM.Pin;
end;

function TGalileoPWM.GetEnabled(const Pin: Integer): Boolean;
begin
  with FGPIO do
  begin
    if (Pin < 0) or (Pin > Length(FPinModes)) then
      raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

    if FPinModes[Pin] <> TPinOpMode.PWM then
      raise EPinNotConfiguredPWM.Create(Format(SPinNotConfiguredPWM, [Pin]));

    Result := FSysfsPWM.Enabled[PinToPWM(Pin)];
  end;
end;

procedure TGalileoPWM.SetEnabled(const Pin: Integer; const Value: Boolean);
begin
  with FGPIO do
  begin
    if (Pin < 0) or (Pin > Length(FPinModes)) then
      raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

    if FPinModes[Pin] <> TPinOpMode.PWM then
    begin
      SetupMuxes(FBoardInfo.Entries[Pin].PWM.Muxes);
      FPinModes[Pin] := TPinOpMode.PWM;
    end;

    if not Value then
      FGPIO.FSysfsGPIO.SetMux(FBoardInfo.Entries[Pin].GPIO.Pin, TPinValue.Low);

    FSysfsPWM.Enabled[PinToPWM(Pin)] := Value;

    if Value then
      FGPIO.FSysfsGPIO.SetMux(FBoardInfo.Entries[Pin].GPIO.Pin, TPinValue.High);
  end;
end;

function TGalileoPWM.GetPeriod(const Pin: Integer): Integer;
begin
  with FGPIO do
  begin
    if (Pin < 0) or (Pin > Length(FPinModes)) then
      raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

    if FPinModes[Pin] <> TPinOpMode.PWM then
      raise EPinNotConfiguredPWM.Create(Format(SPinNotConfiguredPWM, [Pin]));

    Result := FSysfsPWM.Period[PinToPWM(Pin)];
  end;
end;

procedure TGalileoPWM.SetPeriod(const Pin, Value: Integer);
begin
  with FGPIO do
  begin
    if (Pin < 0) or (Pin > Length(FPinModes)) then
      raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

    if FPinModes[Pin] <> TPinOpMode.PWM then
    begin
      SetupMuxes(FBoardInfo.Entries[Pin].PWM.Muxes);
      FPinModes[Pin] := TPinOpMode.PWM;
    end;

    FSysfsPWM.Period[PinToPWM(Pin)] := Value;
  end;
end;

function TGalileoPWM.GetDutyCycle(const Pin: Integer): Integer;
begin
  with FGPIO do
  begin
    if (Pin < 0) or (Pin > Length(FPinModes)) then
      raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

    if FPinModes[Pin] <> TPinOpMode.PWM then
      raise EPinNotConfiguredPWM.Create(Format(SPinNotConfiguredPWM, [Pin]));

    Result := FSysfsPWM.DutyCycle[PinToPWM(Pin)];
  end;
end;

procedure TGalileoPWM.SetDutyCycle(const Pin, Value: Integer);
begin
  with FGPIO do
  begin
    if (Pin < 0) or (Pin > Length(FPinModes)) then
      raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

    if FPinModes[Pin] <> TPinOpMode.PWM then
    begin
      SetupMuxes(FBoardInfo.Entries[Pin].PWM.Muxes);
      FPinModes[Pin] := TPinOpMode.PWM;
    end;

    FSysfsPWM.DutyCycle[PinToPWM(Pin)] := Value;
  end;
end;

{$ENDREGION}
{$REGION 'TGalileoADC'}

constructor TGalileoADC.Create(const AGPIO: TGalileoGPIO);
begin
  inherited Create;

  FGPIO := AGPIO;
  if FGPIO = nil then
    raise EGPIOReferenceInvalid.Create(ClassName + ExceptionClassNameSeparator + SGPIOReferenceInvalid);

  FSysfsADC := TSysfsADC.Create(FGPIO.FBoardInfo.ADC.Path);
end;

destructor TGalileoADC.Destroy;
begin
  FSysfsADC.Free;
  inherited;
end;

function TGalileoADC.GetRawValue(const Channel: Integer): Integer;
var
  Pin: Integer;
begin
  with FGPIO do
  begin
    if (Channel < 0) or (Channel > Length(FBoardInfo.PWM.Pins)) then
      raise EADCInvalidChannel.Create(Format(SADCSpecifiedChannelInvalid, [Channel]));

    Pin := FBoardInfo.ADC.Pins[Channel];

    if FPinModes[Pin] <> TPinOpMode.ADC then
    begin
      SetupMuxes(FBoardInfo.Entries[Pin].ADC.Muxes);
      FPinModes[Pin] := TPinOpMode.ADC;
    end;

    Result := FSysfsADC.RawValue[FBoardInfo.Entries[Pin].ADC.Pin];
  end;
end;

{$ENDREGION}
{$REGION 'TGalileoSPI'}

constructor TGalileoSPI.Create(const AGPIO: TGalileoGPIO; const AFrequency, AMode, ABitsPerWord: Integer);
var
  I, Pin: Integer;
begin
  FGPIO := AGPIO;
  if FGPIO = nil then
    raise EGPIOReferenceInvalid.Create(ClassName + ExceptionClassNameSeparator + SGPIOReferenceInvalid);

  with FGPIO do
    for I := 0 to Length(FBoardInfo.SPI.Pins) - 1 do
    begin
      Pin := FBoardInfo.SPI.Pins[I];
      if FPinModes[Pin] <> TPinOpMode.SPI then
        SetupMuxes(FBoardInfo.Entries[Pin].SPI.Muxes);
    end;

  inherited Create(FGPIO.FBoardInfo.SPI.Path, AFrequency, AMode, ABitsPerWord);
end;

{$ENDREGION}
{$REGION 'TGalileoI2C'}

constructor TGalileoI2C.Create(const AGPIO: TGalileoGPIO);
var
  I, Pin: Integer;
begin
  FGPIO := AGPIO;
  if FGPIO = nil then
    raise EGPIOReferenceInvalid.Create(ClassName + ExceptionClassNameSeparator + SGPIOReferenceInvalid);

  with FGPIO do
    for I := 0 to Length(FBoardInfo.I2C.Pins) - 1 do
    begin
      Pin := FBoardInfo.I2C.Pins[I];
      if FPinModes[Pin] <> TPinOpMode.I2C then
        SetupMuxes(FBoardInfo.Entries[Pin].I2C.Muxes);
    end;

  inherited Create(FGPIO.FBoardInfo.I2C.Path);
end;

{$ENDREGION}
{$REGION 'TGalileoUART'}

constructor TGalileoUART.Create(const AGPIO: TGalileoGPIO);
var
  I, Pin: Integer;
begin
  FGPIO := AGPIO;
  if FGPIO = nil then
    raise EGPIOReferenceInvalid.Create(ClassName + ExceptionClassNameSeparator + SGPIOReferenceInvalid);

  with FGPIO do
    for I := 0 to Length(FBoardInfo.UART.Pins) - 1 do
    begin
      Pin := FBoardInfo.UART.Pins[I];
      if FPinModes[Pin] <> TPinOpMode.UART then
        SetupMuxes(FBoardInfo.Entries[Pin].UART.Muxes);
    end;

  inherited Create(FGPIO.FBoardInfo.UART.Path);
end;

{$ENDREGION}

end.
