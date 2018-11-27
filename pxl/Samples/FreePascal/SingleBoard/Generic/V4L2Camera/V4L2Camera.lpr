program V4L2Camera;
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
  This example illustrates how to take a snapshot from V4L2-compatible webcam.

  Attention! Please follow these instructions before running the sample:

   1. Make sure your V4L2-compliant camera is connected and working properly.
      You can verify that your camera is present and available by using v4l2-ctl tool, e.g.:
        v4l2-ctl --list-devices
        v4l2-ctl --list-formats-ext

   2. When creating TV4L2Camera class, make sure that Linux path (by default /dev/video0) is correct.

   3. After compiling and uploading this sample, change its attributes to executable. It is also recommended to
      execute this application with administrative privileges. Something like this:
        chmod +x V4L2Camera
        sudo ./V4L2Camera
}
uses
  cthreads, SysUtils, PXL.Types, PXL.Surfaces, PXL.ImageFormats, PXL.ImageFormats.FCL, PXL.Timing, PXL.Cameras.V4L2;

var
  ImageFormatManager: TImageFormatManager = nil;
  ImageFormatHandler: TCustomImageFormatHandler = nil;

  Camera: TV4L2Camera = nil;
  Surface: TPixelSurface = nil;
begin
  ImageFormatManager := TImageFormatManager.Create;
  ImageFormatHandler := TFCLImageFormatHandler.Create(ImageFormatManager);
  Surface := TPixelSurface.Create;
  try
    WriteLn('[1] Connecting to camera...');

    Camera := TV4L2Camera.Create('/dev/video0');
    try
      { If you are having problems with the camera, try uncommenting the following line, which would reduce video size
        to see if it's a bandwidth issue related to USB. Depending on singleboard device and/or camera, not all sizes
        reported by V4L2 driver are realistically supported, resulting in all kind of weird issues. }
      // Camera.Size := Point2px(160, 120);

      WriteLn('[2] Starting capture...');

      Camera.StartCapture;
      try
        WriteLn('[3] Waiting until some frames are captured...');

        // Wait for camera to stabilize to take a balanced shot.
        while Camera.FramesCaptured <= 4 do ;

        Write('[4] Taking snapshot...');

        Camera.TakeSnapshot(Surface);

        WriteLn('Done.');
      finally
        Write('[5] Trying to stop capture...');

        Camera.StopCapture;

        WriteLn('Done.');
      end;

      WriteLn('[6] Converting pixel format...');

      Surface.ConvertPixelFormat(TPixelFormat.A8R8G8B8);

      Write('[7] Saving to disk...');

      ImageFormatManager.SaveToFile('snapshot.jpg', Surface, Pointer(80));

      WriteLn('Done.');
    finally
      Camera.Free;
    end;
  finally
    Surface.Free;
    ImageFormatHandler.Free;
    ImageFormatManager.Free;
  end;
end.

