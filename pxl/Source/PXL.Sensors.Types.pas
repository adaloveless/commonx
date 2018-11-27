unit PXL.Sensors.Types;
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
  TCustomSensor = class
  end;

  ESensorGeneric = class(Exception);
  ESensorDataRead = class(ESensorGeneric);
  ESensorDataWrite = class(ESensorGeneric);
  ESensorNoDataPort = class(ESensorGeneric);
  ESensorInvalidAddress = class(ESensorGeneric);
  ESensorInvalidChipID = class(ESensorGeneric);
  ESensorNoSystemCore = class(ESensorGeneric);
  ESensorNoGPIO = class(ESensorGeneric);

resourcestring
  SSensorDataRead = 'Unable to read <%d> bytes from sensor.';
  SSensorDataWrite = 'Unable to write <%d> bytes to sensor.';
  SSensorNoDataPort = 'A valid data port is required for the sensor.';
  SSensorInvalidAddress = 'The specified sensor address <%x> is invalid.';
  SSensorInvalidChipID = 'Invalid sensor Chip ID, expected <%x>, found <%x>.';
  SSensorNoSystemCore = 'A valid system core is required for the sensor.';
  SSensorNoGPIO = 'A valid GPIO interface is required for the sensor.';

implementation

end.
