program Blinky;
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
  This example illustrates how to blink a LED on Intel Galileo.

  Attention! Please follow these instructions before running the sample:

   1. PinLED constant should contain pin number to which positive LED terminal is connected to.

   2. After compiling and uploading this sample, change its attributes to executable. Something like this:
        chmod +x Blinky
        ./Blinky

   3. Check the accompanying diagram and photo to see an example on how the wiring should be made.
}
uses
  Crt, PXL.Timing, PXL.Boards.Types, PXL.Boards.Galileo;

const
  PinLED = 7;

var
  GPIO: TCustomGPIO = nil;
  TurnedOn: Boolean = False;
begin
  GPIO := TGalileoGPIO.Create;
  try
    // Switch LED pin for output.
    GPIO.PinMode[PinLED] := TPinMode.Output;

    WriteLn('Blinking LED, press any key to exit...');

    while not KeyPressed do
    begin
      TurnedOn := not TurnedOn;

      if TurnedOn then
        GPIO.PinValue[PinLED] := TPinValue.High
      else
        GPIO.PinValue[PinLED] := TPinValue.Low;

      MicroSleep(500000); // wait for 500 ms
    end;

    // Eat the key pressed so it won't go to terminal after we exit.
    ReadKey;

    // Turn the LED off after we are done and switch it to "Input" just to be safe.
    GPIO.PinMode[PinLED] := TPinMode.Input;
  finally
    GPIO.Free;
  end;
end.

