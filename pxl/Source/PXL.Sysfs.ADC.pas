unit PXL.Sysfs.ADC;
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
  TSysfsADC = class(TCustomADC)
  private const
    MaximumSupportedChannels = 64;
  private
    FSystemPath: StdString;
  protected
    function GetRawValue(const Channel: Integer): Integer; override;
  public
    constructor Create(const ASystemPath: StdString);
  end;

  EADCGeneric = class(ESysfsGeneric);
  EADCInvalidChannel = class(EADCGeneric);

resourcestring
  SADCSpecifiedChannelInvalid = 'The specified ADC channel <%d> is invalid.';

implementation

uses
  SysUtils;

constructor TSysfsADC.Create(const ASystemPath: StdString);
begin
  inherited Create;

  FSystemPath := ASystemPath;
end;

function TSysfsADC.GetRawValue(const Channel: Integer): Integer;
begin
  if (Channel < 0) or (Channel > MaximumSupportedChannels) then
    raise EADCInvalidChannel.Create(Format(SADCSpecifiedChannelInvalid, [Channel]));

  Result := StrToInt(Trim(ReadTextFromFile(FSystemPath + '/in_voltage' + IntToStr(Channel) + '_raw')));
end;

end.

