unit PXL.Boards.SC16IS7x0;
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
  TUARTBridge = class(TCustomPortUART)
  public const
    DefaultAddress = $4D;
    DefaultBaudRate = 115200;
  private const
    CrystalFrequency = 14745600;
    DefaultPrescaler = 1;
  public type
    TGPIO = class(TCustomGPIO)
    strict private
      FParent: TUARTBridge;
      FPinModes: Cardinal;

      procedure SetPinModes(const Value: Cardinal); inline;
      function GetPinValues: Cardinal; inline;
      procedure SetPinValues(const Value: Cardinal); inline;
    protected
      function GetPinMode(const Pin: Integer): TPinMode; override;
      procedure SetPinMode(const Pin: Integer; const Mode: TPinMode); override;
      function GetPinValue(const Pin: Integer): TPinValue; override;
      procedure SetPinValue(const Pin: Integer; const Value: TPinValue); override;
    public
      constructor Create(const AParent: TUARTBridge);

      property Parent: TUARTBridge read FParent;

      property PinModes: Cardinal read FPinModes write SetPinModes;
      property PinValues: Cardinal read GetPinValues write SetPinValues;
    end;
  strict private
    FDataPort: TCustomDataPort;
    FGPIO: TGPIO;
    FAddress: Integer;
    FBaudRate: Integer;
    FBitsPerWord: Integer;
    FParity: TParity;
    FStopBits: TStopBits;

    procedure UpdateParameters;
    procedure SelfTest;
    function GetGPIO: TGPIO;
  protected
    procedure UpdateAddress; inline;
    procedure WriteReg(const RegAddr, Value: Byte); inline;
    function ReadReg(const RegAddr: Byte): Byte; inline;

    function GetBaudRate: Integer; override;
    procedure SetBaudRate(const Value: Integer); override;
    function GetBitsPerWord: Integer; override;
    procedure SetBitsPerWord(const Value: Integer); override;
    function GetParity: TParity; override;
    procedure SetParity(const Value: TParity); override;
    function GetStopBits: TStopBits; override;
    procedure SetStopBits(const Value: TStopBits); override;
  public
    constructor Create(const ADataPort: TCustomDataPort; const AAddress: Integer = DefaultAddress);
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    procedure Flush; override;

    property DataPort: TCustomDataPort read FDataPort;
    property Address: Integer read FAddress;

    property BaudRate: Integer read FBaudRate write SetBaudRate;
    property BitsPerWord: Integer read FBitsPerWord write SetBitsPerWord;
    property Parity: TParity read FParity write SetParity;
    property StopBits: TStopBits read FStopBits write SetStopBits;

    property GPIO: TGPIO read GetGPIO;
  end;

  EUARTBridgeGeneric = class(Exception);
  EUARTBridgeTransfer = class(EUARTBridgeGeneric);
  EUARTBridgeWrite = class(EUARTBridgeTransfer);
  EUARTBridgeExchange = class(EUARTBridgeTransfer);

  EUARTBridgeNoDataPort = class(EUARTBridgeGeneric);
  EUARTBridgeAddressInvalid = class(EUARTBridgeGeneric);
  EUARTBridgeBaudRateInvalid = class(EUARTBridgeGeneric);
  EUARTBridgeBitsPerWordInvalid = class(EUARTBridgeGeneric);
  EUARTBridgeSelfTestFailed = class(EUARTBridgeGeneric);

  EUARTBridgeGPIOGeneric = class(EUARTBridgeGeneric);
  EUARTBridgeGPIOPinInvalid = class(EUARTBridgeGeneric);

resourcestring
  SUARTBridgeWrite = 'Error writing <%d> byte(s) to Serial-UART Bridge.';
  SUARTBridgeExchange = 'Error exchanging <%d> byte(s) with Serial-UART Bridge.';
  SUARTBridgeNoDataPort = 'A valid data port is required for Serial-UART Bridge.';
  SUARTBridgeAddressInvalid = 'The specified Serial-UART Bridge address <%x> is invalid.';
  SUARTBridgeBaudRateInvalid = 'The specified Serial-UART Bridge baud rate <%d> is invalid.';
  SUARTBridgeBitsPerWordInvalid = 'The specified Serial-UART Bridge bits per word <%d> are invalid.';
  SUARTBridgeSelfTestFailed = 'Serial-UART Bridge self-test failed: expected <%x>, got <%x>.';
  SUARTBridgeGPIOPinInvalid = 'The specified Serial-UART Bridge I/O pin <%d> is invalid.';

implementation

const
  REG_THR = $00;
  REG_RHR = $00;
  REG_FCR = $02;
  REG_LCR = $03;
  REG_MCR = $04;
  REG_LSR = $05;
//  REG_MSR = $06;
  REG_SPR = $07;
  REG_TXLVL = $08;
  REG_RXLVL = $09;
  REG_IODIR = $0A;
  REG_IOSTATE = $0B;
  REG_IOCTRL = $0E;
  REG_DLL = $00;
  REG_DLM = $01;

{$REGION 'TUARTBridge.TGPIO'}

constructor TUARTBridge.TGPIO.Create(const AParent: TUARTBridge);
begin
  inherited Create;

  FParent := AParent;

  FParent.UpdateAddress;
  FPinModes := FParent.ReadReg(REG_IODIR);
end;

procedure TUARTBridge.TGPIO.SetPinModes(const Value: Cardinal);
begin
  if FPinModes <> Value then
  begin
    FPinModes := Value and $FF;
    FParent.WriteReg(REG_IODIR, FPinModes);
  end;
end;

function TUARTBridge.TGPIO.GetPinValues: Cardinal;
begin
  Result := FParent.ReadReg(REG_IOSTATE);
end;

procedure TUARTBridge.TGPIO.SetPinValues(const Value: Cardinal);
begin
  FParent.WriteReg(REG_IOSTATE, Value and $FF);
end;

function TUARTBridge.TGPIO.GetPinMode(const Pin: Integer): TPinMode;
begin
  if (Pin < 0) or (Pin > 7) then
    raise EUARTBridgeGPIOPinInvalid.Create(Format(SUARTBridgeGPIOPinInvalid, [Pin]));

  if FPinModes and (Cardinal(1) shl Pin) > 0 then
    Result := TPinMode.Output
  else
    Result := TPinMode.Input;
end;

procedure TUARTBridge.TGPIO.SetPinMode(const Pin: Integer; const Mode: TPinMode);
begin
  if (Pin < 0) or (Pin > 7) then
    raise EUARTBridgeGPIOPinInvalid.Create(Format(SUARTBridgeGPIOPinInvalid, [Pin]));

  if Mode = TPinMode.Output then
    SetPinModes(FPinModes or (Cardinal(1) shl Pin))
  else
    SetPinModes(FPinModes and (not (Cardinal(1) shl Pin)));
end;

function TUARTBridge.TGPIO.GetPinValue(const Pin: Integer): TPinValue;
var
  PinValues: Byte;
begin
  if (Pin < 0) or (Pin > 7) then
    raise EUARTBridgeGPIOPinInvalid.Create(Format(SUARTBridgeGPIOPinInvalid, [Pin]));

  PinValues := GetPinValues;
  if PinValues and (Cardinal(1) shl Pin) > 0 then
    Result := TPinValue.High
  else
    Result := TPinValue.Low;
end;

procedure TUARTBridge.TGPIO.SetPinValue(const Pin: Integer; const Value: TPinValue);
begin
  if (Pin < 0) or (Pin > 7) then
    raise EUARTBridgeGPIOPinInvalid.Create(Format(SUARTBridgeGPIOPinInvalid, [Pin]));

  PinValues := GetPinValues;

  if Value = TPinValue.High then
    SetPinValues(PinValues or (Cardinal(1) shl Pin))
  else
    SetPinValues(PinValues and (not (Cardinal(1) shl Pin)));
end;

{$ENDREGION}
{$REGION 'TUARTBridge'}

constructor TUARTBridge.Create(const ADataPort: TCustomDataPort; const AAddress: Integer);
begin
  inherited Create;

  FDataPort := ADataPort;
  if FDataPort = nil then
    raise EUARTBridgeNoDataPort.Create(SUARTBridgeNoDataPort);

  if FDataPort is TCustomPortI2C then
  begin
    FAddress := AAddress;
    if (FAddress < 0) or (FAddress > $7F) then
      raise EUARTBridgeAddressInvalid.Create(Format(SUARTBridgeAddressInvalid, [FAddress]));
  end
  else
    FAddress := -1;

  FBaudRate := DefaultBaudRate;
  FBitsPerWord := 8;

  UpdateParameters;
  SelfTest;
end;

destructor TUARTBridge.Destroy;
begin
  FGPIO.Free;

  inherited;
end;

procedure TUARTBridge.UpdateAddress;
begin
  if FAddress <> -1 then
    TCustomPortI2C(FDataPort).SetAddress(FAddress);
end;

procedure TUARTBridge.WriteReg(const RegAddr, Value: Byte);
var
  Values: array[0..1] of Byte;
begin
  Values[0] := RegAddr shl 3;
  Values[1] := Value;

  if FDataPort.Write(@Values[0], SizeOf(Values)) <> SizeOf(Values) then
    raise EUARTBridgeWrite.Create(Format(SUARTBridgeWrite, [SizeOf(Values)]));
end;

function TUARTBridge.ReadReg(const RegAddr: Byte): Byte;
var
  Values: array[0..1] of Byte;
begin
  if FAddress <> -1 then
  begin
    if not TCustomPortI2C(FDataPort).ReadByteData(RegAddr shl 3, Result) then
      raise EUARTBridgeExchange.Create(Format(SUARTBridgeExchange, [SizeOf(Byte)]));
  end
  else
  begin
    Values[0] := (RegAddr shl 3) or $80;
    Values[1] := 0;

    if TCustomPortSPI(FDataPort).Transfer(@Values[0], @Values[0], SizeOf(Values)) <> SizeOf(Values) then
      raise EUARTBridgeExchange.Create(Format(SUARTBridgeExchange, [SizeOf(Byte)]));

    Result := Values[1];
  end;
end;

procedure TUARTBridge.UpdateParameters;
var
  ControlValue, ControlDivisor: Cardinal;
begin
  UpdateAddress;

//  WriteReg(REG_IOCTRL, 1 shl 3);

  ControlValue := Cardinal(FBitsPerWord) - 5;

  if FStopBits > TStopBits.One then
    ControlValue := ControlValue or $04;

  if FParity > TParity.None then
    ControlValue := ControlValue or $08 or (Cardinal(FParity) shl 4);

  ControlDivisor := (CrystalFrequency div DefaultPrescaler) div (Cardinal(FBaudRate) * 16);
  if (ControlDivisor = 0) or (ControlDivisor > High(Word)) then
    raise EUARTBridgeBaudRateInvalid.Create(Format(SUARTBridgeBaudRateInvalid, [FBaudRate]));

  WriteLn('Using divisor: ', ControlDivisor);

  // Specify parameters and enable writing to DLx registers.
  WriteReg(REG_LCR, ControlValue or $80);
  WriteReg(REG_DLL, ControlDivisor and $FF);
  WriteReg(REG_DLM, ControlDivisor shr 8);

  // Disable writing to DLx registers but keep same parameters.
  WriteReg(REG_LCR, ControlValue);

  // Disable unnecessary features and enable normal operation.
  WriteReg(REG_MCR, $00);

  // Enable FIFO and reset buffers.
  WriteReg(REG_FCR, $07);
end;

procedure TUARTBridge.SelfTest;
const
  MagicNumber = $AA;
var
  Value: Byte;
begin
  UpdateAddress;

  WriteReg(REG_SPR, MagicNumber);

  Value := ReadReg(REG_SPR);
  if Value <> MagicNumber then
    raise EUARTBridgeSelfTestFailed.Create(Format(SUARTBridgeSelfTestFailed, [MagicNumber, Value]));
end;

function TUARTBridge.GetGPIO: TGPIO;
begin
  if FGPIO = nil then
    FGPIO := TGPIO.Create(Self);

  Result := FGPIO;
end;

function TUARTBridge.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

procedure TUARTBridge.SetBaudRate(const Value: Integer);
begin
  if Value <= 0 then
    raise EUARTBridgeBaudRateInvalid.Create(Format(SUARTBridgeBaudRateInvalid, [Value]));

  if FBaudRate <> Value then
  begin
    FBaudRate := Value;
    UpdateParameters;
  end;
end;

function TUARTBridge.GetBitsPerWord: Integer;
begin
  Result := FBitsPerWord;
end;

procedure TUARTBridge.SetBitsPerWord(const Value: Integer);
begin
  if (Value < 5) or (Value > 8) then
    raise EUARTBridgeBitsPerWordInvalid.Create(Format(SUARTBridgeBitsPerWordInvalid, [FBitsPerWord]));

  if FBitsPerWord <> Value then
  begin
    FBitsPerWord := Value;
    UpdateParameters;
  end;
end;

function TUARTBridge.GetParity: TParity;
begin
  Result := FParity;
end;

procedure TUARTBridge.SetParity(const Value: TParity);
begin
  if FParity <> Value then
  begin
    FParity := Value;
    UpdateParameters;
  end;
end;

function TUARTBridge.GetStopBits: TStopBits;
begin
  Result := FStopBits;
end;

procedure TUARTBridge.SetStopBits(const Value: TStopBits);
begin
  if FStopBits <> Value then
  begin
    FStopBits := Value;
    UpdateParameters;
  end;
end;

function TUARTBridge.Read(const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  I, BytesAvailable, Res: Integer;
begin
  UpdateAddress;

  BytesAvailable := ReadReg(REG_RXLVL);

  Result := BytesAvailable;
  if Result > BufferSize then
    Result := BufferSize;

  for I := 0 to Result - 1 do
    PByte(PtrUInt(Buffer) + Cardinal(I))^ := ReadReg(REG_RHR);

  Res := ReadReg(REG_LSR);
  if Res and $80 > 0 then
  begin
    WriteLn('Read error: 0x', IntToHex(Res, 2));
//    Result := 0;
  end;

{  WriteLn('RX bytes unread: ', ReadReg(REG_RXLVL));
  WriteLn('TX spaces left: ', ReadReg(REG_TXLVL));}
end;

function TUARTBridge.Write(const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  I, SpacesAvailable, Res: Integer;
begin
  UpdateAddress;

  SpacesAvailable := ReadReg(REG_TXLVL);

  Result := SpacesAvailable;
  if Result > BufferSize then
    Result := BufferSize;

  for I := 0 to Result - 1 do
    WriteReg(REG_THR, PByte(PtrUInt(Buffer) + Cardinal(I))^);

  Res := ReadReg(REG_LSR);
  if Res and $80 > 0 then
  begin
    WriteLn('Write error: 0x', IntToHex(Res, 2));
//    Result := 0;
  end;

{  WriteLn('RX bytes unread: ', ReadReg(REG_RXLVL));
  WriteLn('TX spaces left: ', ReadReg(REG_TXLVL));}
end;

procedure TUARTBridge.Flush;
begin
  UpdateAddress;
  WriteReg(REG_FCR, $07);
end;

{$ENDREGION}

end.

