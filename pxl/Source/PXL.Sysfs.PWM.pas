unit PXL.Sysfs.PWM;
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
  TSysfsPWM = class(TCustomPWM)
  private const
    MaximumSupportedChannels = 64;

    ExportedBitmask = $80;
    EnabledDefinedBitmask = $40;
    EnabledBitmask = $20;
    PeriodDefinedBitmask = $10;
    DutyCycleDefinedBitmask = $08;
  private
    FSystemPath: StdString;
    FExportFileName: StdString;
    FUnexportFileName: StdString;
    FAccessFileName: StdString;

    FChannels: array[0..MaximumSupportedChannels - 1] of Byte;
    FPeriods: array[0..MaximumSupportedChannels - 1] of Integer;
    FDutyCycles: array[0..MaximumSupportedChannels - 1] of Integer;

    procedure SetChannelBit(const Channel, Mask: Integer); inline;
    procedure ClearChannelBit(const Channel, Mask: Integer); inline;
    function IsChannelBitSet(const Channel, Mask: Integer): Boolean; inline;
    function IsChannelExported(const Channel: Integer): Boolean; inline;
    function HasChannelAbility(const Channel: Integer): Boolean; inline;

    procedure ExportChannel(const Channel: Integer);
    procedure UnexportChannel(const Channel: Integer);
    procedure UnexportAllChannels;
  protected
    function GetEnabled(const Channel: Integer): Boolean; override;
    procedure SetEnabled(const Channel: Integer; const Value: Boolean); override;
    function GetPeriod(const Channel: Integer): Integer; override;
    procedure SetPeriod(const Channel, Value: Integer); override;
    function GetDutyCycle(const Channel: Integer): Integer; override;
    procedure SetDutyCycle(const Channel, Value: Integer); override;
  public
    constructor Create(const ASystemPath: StdString);
    destructor Destroy; override;
  end;

  EPWMGeneric = class(ESysfsGeneric);
  EPWMInvalidChannel = class(EPWMGeneric);
  EPWMUndefinedChannel = class(EPWMGeneric);

resourcestring
  SPWMSpecifiedChannelInvalid = 'The specified PWM channel <%d> is invalid.';
  SPWMSpecifiedChannelUndefined = 'The specified PWM channel <%d> is undefined.';

implementation

uses
  SysUtils;

constructor TSysfsPWM.Create(const ASystemPath: StdString);
begin
  inherited Create;

  FSystemPath := ASystemPath;
  FExportFileName := FSystemPath + '/export';
  FUnexportFileName := FSystemPath + '/unexport';
  FAccessFileName := FSystemPath + '/pwm';
end;

destructor TSysfsPWM.Destroy;
begin
  UnexportAllChannels;

  inherited;
end;

procedure TSysfsPWM.SetChannelBit(const Channel, Mask: Integer);
begin
  FChannels[Channel] := FChannels[Channel] or Mask;
end;

procedure TSysfsPWM.ClearChannelBit(const Channel, Mask: Integer);
begin
  FChannels[Channel] := FChannels[Channel] and ($FF xor Mask);
end;

function TSysfsPWM.IsChannelBitSet(const Channel, Mask: Integer): Boolean;
begin
  Result := FChannels[Channel] and Mask > 0;
end;

function TSysfsPWM.IsChannelExported(const Channel: Integer): Boolean;
begin
  Result := IsChannelBitSet(Channel, ExportedBitmask);
end;

function TSysfsPWM.HasChannelAbility(const Channel: Integer): Boolean;
begin
  Result := IsChannelBitSet(Channel, EnabledDefinedBitmask);
end;

procedure TSysfsPWM.ExportChannel(const Channel: Integer);
begin
  TryWriteTextToFile(FExportFileName, IntToStr(Channel));
  SetChannelBit(Channel, ExportedBitmask);
end;

procedure TSysfsPWM.UnexportChannel(const Channel: Integer);
begin
  TryWriteTextToFile(FUnexportFileName, IntToStr(Channel));
  ClearChannelBit(Channel, ExportedBitmask);
end;

procedure TSysfsPWM.UnexportAllChannels;
var
  I: Integer;
begin
  for I := Low(FChannels) to High(FChannels) do
    if IsChannelExported(I) then
      UnexportChannel(I);
end;

function TSysfsPWM.GetEnabled(const Channel: Integer): Boolean;
begin
  if (Channel < 0) or (Channel > MaximumSupportedChannels) then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  Result := IsChannelBitSet(Channel, EnabledBitmask);
end;

procedure TSysfsPWM.SetEnabled(const Channel: Integer; const Value: Boolean);
var
  NeedModify: Boolean;
begin
  if (Channel < 0) or (Channel > MaximumSupportedChannels) then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if not IsChannelExported(Channel) then
    ExportChannel(Channel);

  if IsChannelBitSet(Channel, EnabledDefinedBitmask) then
    if IsChannelBitSet(Channel, EnabledBitmask) then
      NeedModify := not Value
    else
      NeedModify := Value
  else
    NeedModify := True;

  if NeedModify then
  begin
    if Value then
    begin
      WriteTextToFile(FAccessFileName + IntToStr(Channel) + '/enable', '1');
      SetChannelBit(Channel, EnabledBitmask);
    end
    else
    begin
      WriteTextToFile(FAccessFileName + IntToStr(Channel) + '/enable', '0');
      ClearChannelBit(Channel, EnabledBitmask);
    end;

    SetChannelBit(Channel, EnabledDefinedBitmask);
  end;
end;

function TSysfsPWM.GetPeriod(const Channel: Integer): Integer;
begin
  if (Channel < 0) or (Channel > MaximumSupportedChannels) then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) or
    (not IsChannelBitSet(Channel, PeriodDefinedBitmask)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  Result := FPeriods[Channel];
end;

procedure TSysfsPWM.SetPeriod(const Channel, Value: Integer);
begin
  if (Channel < 0) or (Channel > MaximumSupportedChannels) then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  WriteTextToFile(FAccessFileName + IntToStr(Channel) + '/period', IntToStr(Value));
  FPeriods[Channel] := Value;

  SetChannelBit(Channel, PeriodDefinedBitmask);
end;

function TSysfsPWM.GetDutyCycle(const Channel: Integer): Integer;
begin
  if (Channel < 0) or (Channel > MaximumSupportedChannels) then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) or
    (not IsChannelBitSet(Channel, DutyCycleDefinedBitmask)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  Result := FDutyCycles[Channel];
end;

procedure TSysfsPWM.SetDutyCycle(const Channel, Value: Integer);
begin
  if (Channel < 0) or (Channel > MaximumSupportedChannels) then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  WriteTextToFile(FAccessFileName + IntToStr(Channel) + '/duty_cycle', IntToStr(Value));
  FDutyCycles[Channel] := Value;

  SetChannelBit(Channel, DutyCycleDefinedBitmask);
end;

end.

