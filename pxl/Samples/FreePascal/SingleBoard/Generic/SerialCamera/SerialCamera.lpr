program SerialCamera;
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
  This example illustrates how to take a snapshot from serial camera.

  Attention! Please follow these instructions before running the sample:

   1. Make sure that the serial camera is connected properly to UART pins on your device. Pay special attention to
      voltage levels as the majority devices, including Raspberry PI and BeagleBone Black tolerate only up to 3.3V.
      If that is the case, then make sure to apply voltage divider on UART's RX pin.

   2. Specify the path to the corresponding UART port on the device. This varies between devices but is usually
      located in "/dev/tty*". For BeagleBone Black, it is usually "/dev/ttyON" (where N is number of UART, usually 1).
      For Intel Galileo, it is usually "/dev/ttyS0", for Raspberry PI, it is "/dev/ttyAMA0".

   3. Note that on some devices such as Raspberry PI, the UART by default is used as debug console. Therefore, it is
      necessary to disable such feature (e.g. in raspi-config) before using that UART port.

   4. After compiling and uploading this sample, change its attributes to executable. It is also recommended to
      execute this application with administrative privileges. Something like this:
        chmod +x SerialCamera
        sudo ./SerialCamera

   5. Serial cameras such as VC0706 or LSY201 transfer images through UART. This is quite slow and delicate process,
      and many things can go wrong. It is recommended to use only hardware UART ports connected directly to the camera
      (but again, pay attention on voltage levels) for this purpose, without any helpers such as SC16IS7x0 chip or
      intermediaries such as XBee.
}
uses
  // The following line can be replaced to "PXL.Cameras.LSY201" depending on camera's brand.
  PXL.Cameras.VC0706,
//  PXL.Cameras.LSY201,

  SysUtils, Classes, PXL.Types, PXL.Boards.Types, PXL.Sysfs.UART;

const
// Please make sure that this path points to the appropriate UART where serial camera is connected to.
  PathToUART = '/dev/ttyO1';

procedure SaveBufferToDisk(const Buffer: Pointer; const BufferSize: Integer);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create('snapshot.jpg', fmCreate or fmShareExclusive);
  try
    Stream.WriteBuffer(Buffer^, BufferSize);
  finally
    Stream.Free;
  end;
end;

procedure RetrievePicture(const PortUART: TCustomPortUART);
var
  Camera: TCamera = nil;
  Buffer: Pointer = nil;
  BufferSize: Integer = 0;
begin
  Camera := TCamera.Create(PortUART);
  try
    PortUART.BaudRate := Camera.DefaultBaudRate;

    Write('Resetting camera: ');

    if not Camera.Reset then
    begin
      WriteLn('ERROR.');
      Exit;
    end;

    WriteLn('OK.');

    WriteLn('Received camera response:');
    WriteLn('................................');
    WriteLn(TCamera(Camera).BootText);
    WriteLn('................................');

    Sleep(200);

    Write('Setting image size: ');

    if not Camera.SetImageSize(640, 480) then
    begin
      WriteLn('ERROR.');
      Exit;
    end;

    WriteLn('OK.');

    Sleep(250);

    Write('Taking snapshot: ');

    if not Camera.TakeSnapshot then
    begin
      WriteLn('ERROR.');
      Exit;
    end;

    WriteLn('OK.');

    Sleep(500);

    Write('Retrieving picture: ');

    if not Camera.GetPicture(Buffer, BufferSize) then
    begin
      WriteLn('ERROR.');
      Exit;
    end;

    WriteLn('OK.');
  finally
    Camera.Free;
  end;

  try
    SaveBufferToDisk(Buffer, BufferSize);
  finally
    FreeMem(Buffer);
  end;

  WriteLn('Snapshot saved to disk.');
end;

var
  PortUART: TCustomPortUART = nil;

begin
  PortUART := TSysfsUART.Create(PathToUART);
  try
    RetrievePicture(PortUART);
  finally
    PortUART.Free;
  end;
end.

