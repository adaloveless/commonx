unit PXL.Sysfs.GPIO;
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
  PXL.TypeDef, PXL.Sysfs.Types, PXL.Boards.Types;

type
  { Drive mode that is used in GPIO pins. }
  TPinDrive = (
    { Strong low and high. }
    Strong,

    { Resistive high, strong low. }
    PullUp,

    { Resistive low, strong high. }
    PullDown,

    { High Z state }
    HighZ);

  TSysfsGPIO = class(TCustomGPIO)
  public const
    DefaultSystemPath = '/sys/class/gpio';
  protected const
    MaximumSupportedPins = 256;

    ExportedBitmask = $80;
    DirectionDefinedBitmask = $40;
    DirectionBitmask = $20;
    DriveBitmask = $18;
    ValueDefinedBitmask = $02;
    ValueBitmask = $01;
  private
    FSystemPath: StdString;
    FExportFileName: StdString;
    FUnexportFileName: StdString;
    FAccessFileName: StdString;

    FPins: packed array[0..MaximumSupportedPins - 1] of Byte;

    procedure SetPinBit(const Pin, Mask: Integer); inline;
    procedure ClearPinBit(const Pin, Mask: Integer); inline;
    function IsPinBitSet(const Pin, Mask: Integer): Boolean; inline;

    function IsPinExported(const Pin: Integer): Boolean; inline;
    function HasPinDirection(const Pin: Integer): Boolean; inline;
    function HasPinValue(const Pin: Integer): Boolean; inline;

    procedure ExportPin(const Pin: Integer);
    procedure UnexportPin(const Pin: Integer);
    procedure UnexportAllPins;

    function GetPinDrive(const Pin: Integer): TPinDrive;
    procedure SetPinDrive(const Pin: Integer; const Value: TPinDrive);
  protected
    function GetPinMode(const Pin: Integer): TPinMode; override;
    procedure SetPinMode(const Pin: Integer; const Mode: TPinMode); override;

    function GetPinValue(const Pin: Integer): TPinValue; override;
    procedure SetPinValue(const Pin: Integer; const Value: TPinValue); override;
  public
    constructor Create(const ASystemPath: StdString = DefaultSystemPath);
    destructor Destroy; override;

    function TrySetPinMode(const Pin: Integer; const Mode: TPinMode): Boolean;

    property PinDrive[const Pin: Integer]: TPinDrive read GetPinDrive write SetPinDrive;
  end;

  EGPIOGeneric = class(ESysfsGeneric);
  EGPIOInvalidPin = class(EGPIOGeneric);
  EGPIOUndefinedPin = class(EGPIOGeneric);
  EGPIOIncorrectPinDirection = class(EGPIOGeneric);

resourcestring
  SGPIOSpecifiedPinInvalid = 'The specified GPIO pin <%d> is invalid.';
  SGPIOSpecifiedPinUndefined = 'The specified GPIO pin <%d> is undefined.';
  SGPIOPinHasIncorrectDirection = 'The specified GPIO pin <%d> has incorrect direction.';

implementation

uses
  SysUtils;

constructor TSysfsGPIO.Create(const ASystemPath: StdString);
begin
  inherited Create;

  FSystemPath := ASystemPath;
  FExportFileName := FSystemPath + '/export';
  FUnexportFileName := FSystemPath + '/unexport';
  FAccessFileName := FSystemPath + '/gpio';
end;

destructor TSysfsGPIO.Destroy;
begin
  UnexportAllPins;

  inherited;
end;

procedure TSysfsGPIO.SetPinBit(const Pin, Mask: Integer);
begin
  FPins[Pin] := FPins[Pin] or Mask;
end;

procedure TSysfsGPIO.ClearPinBit(const Pin, Mask: Integer);
begin
  FPins[Pin] := FPins[Pin] and ($FF xor Mask);
end;

function TSysfsGPIO.IsPinBitSet(const Pin, Mask: Integer): Boolean;
begin
  Result := FPins[Pin] and Mask > 0;
end;

function TSysfsGPIO.IsPinExported(const Pin: Integer): Boolean;
begin
  Result := IsPinBitSet(Pin, ExportedBitmask);
end;

function TSysfsGPIO.HasPinDirection(const Pin: Integer): Boolean;
begin
  Result := IsPinBitSet(Pin, DirectionDefinedBitmask);
end;

function TSysfsGPIO.HasPinValue(const Pin: Integer): Boolean;
begin
  Result := IsPinBitSet(Pin, ValueDefinedBitmask);
end;

procedure TSysfsGPIO.ExportPin(const Pin: Integer);
begin
  TryWriteTextToFile(FExportFileName, IntToStr(Pin));
  SetPinBit(Pin, ExportedBitmask);
end;

procedure TSysfsGPIO.UnexportPin(const Pin: Integer);
begin
  TryWriteTextToFile(FUnexportFileName, IntToStr(Pin));
  ClearPinBit(Pin, ExportedBitmask);
end;

procedure TSysfsGPIO.UnexportAllPins;
var
  I: Integer;
begin
  for I := Low(FPins) to High(FPins) do
    if IsPinExported(I) then
      UnexportPin(I);
end;

function TSysfsGPIO.GetPinMode(const Pin: Integer): TPinMode;
begin
  if (Pin < 0) or (Pin > MaximumSupportedPins) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  if IsPinBitSet(Pin, DirectionBitmask) then
    Result := TPinMode.Output
  else
    Result := TPinMode.Input;
end;

procedure TSysfsGPIO.SetPinMode(const Pin: Integer; const Mode: TPinMode);
begin
  if (Pin < 0) or (Pin > MaximumSupportedPins) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if not IsPinExported(Pin) then
    ExportPin(Pin);

  if Mode = TPinMode.Input then
  begin
    WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/direction', 'in');
    ClearPinBit(Pin, DirectionBitmask);
  end
  else
  begin
    WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/direction', 'out');
    SetPinBit(Pin, DirectionBitmask);
  end;

  SetPinBit(Pin, DirectionDefinedBitmask);
end;

function TSysfsGPIO.TrySetPinMode(const Pin: Integer; const Mode: TPinMode): Boolean;
begin
  if (Pin < 0) or (Pin > MaximumSupportedPins) then
    Exit(False);

  if not IsPinExported(Pin) then
    ExportPin(Pin);

  if Mode = TPinMode.Input then
  begin
    Result := TryWriteTextToFile(FAccessFileName + IntToStr(Pin) + '/direction', 'in');
    ClearPinBit(Pin, DirectionBitmask);
  end
  else
  begin
    Result := TryWriteTextToFile(FAccessFileName + IntToStr(Pin) + '/direction', 'out');
    SetPinBit(Pin, DirectionBitmask);
  end;

  SetPinBit(Pin, DirectionDefinedBitmask);
end;

function TSysfsGPIO.GetPinValue(const Pin: Integer): TPinValue;
begin
  if (Pin < 0) or (Pin > MaximumSupportedPins) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  if IsPinBitSet(Pin, DirectionBitmask) and HasPinValue(Pin) then
  begin // Pin with direction set to OUTPUT and VALUE defined can be retrieved directly.
    if IsPinBitSet(Pin, ValueBitmask) then
      Result := TPinValue.High
    else
      Result := TPinValue.Low;
  end
  else
  begin // Pin needs to be read from GPIO.
    if ReadCharFromFile(FAccessFileName + IntToStr(Pin) + '/value') = '1' then
      Result := TPinValue.High
    else
      Result := TPinValue.Low;
  end;
end;

procedure TSysfsGPIO.SetPinValue(const Pin: Integer; const Value: TPinValue);
var
  CurValue: TPinValue;
begin
  if (Pin < 0) or (Pin > MaximumSupportedPins) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  if not IsPinBitSet(Pin, DirectionBitmask) then
    raise EGPIOIncorrectPinDirection.Create(Format(SGPIOPinHasIncorrectDirection, [Pin]));

  if HasPinValue(Pin) then
  begin
    if IsPinBitSet(Pin, ValueBitmask) then
      CurValue := TPinValue.High
    else
      CurValue := TPinValue.Low;

    // Do not write value to the pin if it is already set.
    if CurValue = Value then
      Exit;
  end;

  if Value = TPinValue.Low then
  begin
    WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/value', '0');
    ClearPinBit(Pin, ValueBitmask);
  end
  else
  begin
    WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/value', '1');
    SetPinBit(Pin, ValueBitmask);
  end;
end;

function TSysfsGPIO.GetPinDrive(const Pin: Integer): TPinDrive;
begin
  if (Pin < 0) or (Pin > MaximumSupportedPins) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  Result := TPinDrive((FPins[Pin] and DriveBitmask) shr 3);
end;

procedure TSysfsGPIO.SetPinDrive(const Pin: Integer; const Value: TPinDrive);
var
  DriveText: StdString;
begin
  if (Pin < 0) or (Pin > MaximumSupportedPins) then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  if IsPinBitSet(Pin, DirectionBitmask) then
    raise EGPIOIncorrectPinDirection.Create(Format(SGPIOPinHasIncorrectDirection, [Pin]));

  case Value of
    TPinDrive.PullUp:
      DriveText := 'pullup';

    TPinDrive.PullDown:
      DriveText := 'pulldown';

    TPinDrive.HighZ:
      DriveText := 'hiz';
  else
    DriveText := 'strong';
  end;

  WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/drive', DriveText);

  ClearPinBit(Pin, DriveBitmask);
  SetPinBit(Pin, Ord(Value) shl 3);
end;

end.

