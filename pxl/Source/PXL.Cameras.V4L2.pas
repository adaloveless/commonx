unit PXL.Cameras.V4L2;
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

{ V4L2 video capture can be quite troublesome depending on drivers and hardware, especially when using big video size
  on devices such as Raspberry PI. The following option enables debug text logging to standard output, which could
  help figuring out what is going wrong. }
{.$DEFINE V4L2_CAMERA_DEBUG}

uses
  SysUtils, Classes, SyncObjs, PXL.Linux.videodev2, PXL.TypeDef, PXL.Types, PXL.Surfaces;

type
  TV4L2Camera = class
  private type
    TCaptureThread = class(TThread)
    private type
      TBufferType = record
        Memory: Pointer;
        Size: Cardinal;
      end;
    private
      FCamera: TV4L2Camera;
      FHandle: TUntypedHandle;
      FBufferCount: Integer;
      FBuffers: array of TBufferType;

      FImageBufferSection: TCriticalSection;
      FImageBuffer: Pointer;
      FImageBufferSize: Cardinal;

      procedure CreateBuffers;
      procedure ReleaseBuffers;
    protected
      procedure Execute; override;
    public
      constructor Create(const ACamera: TV4L2Camera; const AHandle: TUntypedHandle; const ABufferCount: Integer);
      destructor Destroy; override;

      property Camera: TV4L2Camera read FCamera;

      property BufferCount: Integer read FBufferCount;

      property ImageBufferSection: TCriticalSection read FImageBufferSection;

      property ImageBuffer: Pointer read FImageBuffer;
      property ImageBufferSize: Cardinal read FImageBufferSize;
    end;
  public type
    TCaptureNotifyEvent = procedure(const Sender: TObject; const Buffer: Pointer;
      const BufferSize: Cardinal) of object;
  public const
    DefaultSystemPath = '/dev/video0';
    DefaultBufferCount = 2;
    DefaultVideoSize: TPoint2px = (X: 640; Y: 480);
  private
    FSystemPath: StdString;
    FHandle: TUntypedHandle;
    FSize: TPoint2px;
    FPixelFormat: Cardinal;
    FCaptureStream: TCaptureThread;
    FOnCapture: TCaptureNotifyEvent;
    FFramesCaptured: Integer;

    function FormatToString(const AVideoFormat: Cardinal): StdString;
    function GetCapturing: Boolean;
    function TryVideoFormat(const AVideoFormat: Cardinal): Boolean;

    procedure SetSize(const Value: TPoint2px);
    procedure ConvertImageYUYV(const Source: Pointer; const Surface: TPixelSurface);
  public
    constructor Create(const ASystemPath: StdString = DefaultSystemPath);
    destructor Destroy; override;

    { Create a new video capturing thread with the specified number of buffers to start recording.
      The number of buffers should be at least 2. Higher number of buffers could reduce issues and/or prevent camera
      stalling, at the expense of higher memory footprint. }
    procedure StartCapture(const ABufferCount: Integer = DefaultBufferCount);

    { Tries to stop camera capture and release the working thread. }
    procedure StopCapture;

    procedure TakeSnapshot(const DestSurface: TPixelSurface);

    // Currently used path to V4L2 video device.
    property SystemPath: StdString read FSystemPath;

    // The size of video image captured from camera.
    property Size: TPoint2px read FSize write SetSize;

    // Pixel format of video image captured from camera according to V4L2 "FOURCC" codes.
    property PixelFormat: Cardinal read FPixelFormat;

    // Indicates whether the camera is currently capturing.
    property Capturing: Boolean read GetCapturing;

    // Indicates how many frames were captured so far.
    property FramesCaptured: Integer read FFramesCaptured;

    { This event is called from a different thread whenever a buffer has been captured. The pointer to that buffer and
      its size are returned. The actual video feed will have size and format according to "Size" and "PixelFormat".
      The direct access to video buffers is provided and the code inside this event should execute as fast as possible
      to prevent camera from stalling, which depending on drivers could be unrecoverable. If this event is assigned,
      the recording thread won't copy its buffers to a secondary location and "TakeSnapshot" won't work. }
    property OnCapture: TCaptureNotifyEvent read FOnCapture write FOnCapture;
  end;

  EV4L2Generic = class(Exception);
  EV4L2FileOpen = class(EV4L2Generic);

  EV4L2Format = class(EV4L2Generic);
  EV4L2GetFormat = class(EV4L2Format);
  EV4L2SetFormat = class(EV4L2Format);
  EV4L2SetPixelFormat = class(EV4L2SetFormat);
  EV4L2SetVideoSize = class(EV4L2SetFormat);
  EV4L2UnsupportedFormat = class(EV4L2Format);

  EV4L2Buffers = class(EV4L2Generic);
  EV4L2RequestBuffers = class(EV4L2Buffers);
  EV4L2AssociateBuffer = class(EV4L2Buffers);
  EV4L2MemoryMapBuffer = class(EV4L2Buffers);
  EV4L2QueueBuffer = class(EV4L2Buffers);
  EV4L2ReceiveBuffers = class(EV4L2Buffers);

  EV4L2Stream = class(EV4L2Generic);
  EV4L2StreamStart = class(EV4L2Stream);
  EV4L2StreamWait = class(EV4L2Stream);
  EV4L2NotStreaming = class(EV4L2Stream);
  EV4L2AlreadyStreaming = class(EV4L2Stream);

resourcestring
  SCannotOpenCameraDeviceFile = 'Cannot open camera device file <%s> for reading and writing.';
  SCannotObtainCameraFormat = 'Cannot obtain camera device format.';
  SCannotChangeCameraFormat = 'Cannot change camera device format.';
  SUnsupportedCameraFormat = 'Unsupported camera device format (%s).';
  SCannotSetCameraPixelFormat = 'Cannot set camera pixel format format.';
  SCannotSetCameraVideoSize = 'Cannot set new video size (%d by %d).';
  SCannotObtainCameraBuffers = 'Cannot obtaim %d camera device buffers.';
  SCannotAssociateCameraBuffer = 'Cannot associate buffer %d with camera device.';
  SCannotMemoryMapCameraBuffer = 'Cannot map camera device buffer %d to memory.';
  SCannotQueueCameraBuffer = 'Cannot queue camera device buffer %d.';
  SCannotReceiveCameraBuffers = 'Cannot receive camera device buffers.';
  SCannotStartCameraStreaming = 'Cannot start camera device streaming.';
  SFailedWaitingForCameraStream = 'Failed waiting for camera device stream.';
  SCameraIsCurrentlyNotStreaming = 'Camera is currently not capturing.';
  SCameraIsAlreadyStreaming = 'Camera is already capturing.';

implementation

uses
  BaseUnix, PXL.Formats;

{$REGION 'TV4L2Camera.TCaptureThread'}

constructor TV4L2Camera.TCaptureThread.Create(const ACamera: TV4L2Camera; const AHandle: TUntypedHandle;
  const ABufferCount: Integer);
begin
{$IFDEF V4L2_CAMERA_DEBUG}
  WriteLn('[CT] Created.');
{$ENDIF}

  FCamera := ACamera;
  FHandle := AHandle;

  FBufferCount := ABufferCount;
  if FBufferCount < 2 then
    FBufferCount := 2;

  FImageBufferSection := TCriticalSection.Create;
  CreateBuffers;

  inherited Create(False);
end;

destructor TV4L2Camera.TCaptureThread.Destroy;
begin
  inherited;

  ReleaseBuffers;
  FreeAndNil(FImageBufferSection);

{$IFDEF V4L2_CAMERA_DEBUG}
  WriteLn('[CT] Destroyed.');
{$ENDIF}
end;

procedure TV4L2Camera.TCaptureThread.CreateBuffers;
const
  PageSize = {$IFDEF FPC_MMAP2}4096{$ELSE}1{$ENDIF};
var
  BufferRequest: v4l2_requestbuffers;
  BufferDecl: v4l2_buffer;
  I: Integer;
begin
{$IFDEF V4L2_CAMERA_DEBUG}
  Write('[CT] Requesting buffers...');
{$ENDIF}

  FillChar(BufferRequest, SizeOf(v4l2_requestbuffers), 0);

  BufferRequest.count := FBufferCount;
  BufferRequest.&type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  BufferRequest.memory := V4L2_MEMORY_MMAP;

  if FpIOCtl(FHandle, VIDIOC_REQBUFS, @BufferRequest) < 0 then
    raise EV4L2RequestBuffers.Create(Format(SCannotObtainCameraBuffers, [FBufferCount]));

{$IFDEF V4L2_CAMERA_DEBUG}
  WriteLn('OK.');
{$ENDIF}

  SetLength(FBuffers, FBufferCount);

  for I := 0 to Length(FBuffers) - 1 do
  begin
    FBuffers[I].Memory := nil;
    FBuffers[I].Size := 0;
  end;

  FImageBufferSize := 0;

  for I := 0 to Length(FBuffers) - 1 do
  begin
  {$IFDEF V4L2_CAMERA_DEBUG}
    Write('[CT] Querying buffer #', I, '...');
  {$ENDIF}

    FillChar(BufferDecl, SizeOf(v4l2_buffer), 0);

    BufferDecl.index := I;
    BufferDecl.&type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
    BufferDecl.memory := V4L2_MEMORY_MMAP;

    if FpIOCtl(FHandle, VIDIOC_QUERYBUF, @BufferDecl) < 0 then
      raise EV4L2AssociateBuffer.Create(Format(SCannotAssociateCameraBuffer, [I]));

  {$IFDEF V4L2_CAMERA_DEBUG}
    WriteLn('OK.');
    Write('[CT] Mapping buffer #', I, '...');
  {$ENDIF}

    FBuffers[I].Size := BufferDecl.length;
    FBuffers[I].Memory := Fpmmap(nil, FBuffers[I].Size, PROT_READ, MAP_SHARED, FHandle, BufferDecl.offset div PageSize);

    if (FBuffers[I].Memory = nil) or (FBuffers[I].Memory = MAP_FAILED) then
    begin
      FBuffers[I].Memory := nil;
      FBuffers[I].Size := 0;

      raise EV4L2MemoryMapBuffer.Create(Format(SCannotMemoryMapCameraBuffer, [I]));
    end;

  {$IFDEF V4L2_CAMERA_DEBUG}
    WriteLn('OK.');
  {$ENDIF}

    if FImageBufferSize < BufferDecl.length then
      FImageBufferSize := BufferDecl.length;
  end;

  if (FCamera = nil) or (not Assigned(FCamera.FOnCapture)) then
    FImageBuffer := AllocMem(FImageBufferSize);
end;

procedure TV4L2Camera.TCaptureThread.ReleaseBuffers;
var
  I: Integer;
begin
  FreeMemAndNil(FImageBuffer);
  FImageBufferSize := 0;

{$IFDEF V4L2_CAMERA_DEBUG}
  Write('[CS] Releasing buffers...');
{$ENDIF}

  for I := Length(FBuffers) - 1 downto 0 do
    if FBuffers[I].Memory <> nil then
    begin
      Fpmunmap(FBuffers[I].Memory, FBuffers[I].Size);

      FBuffers[I].Memory := nil;
      FBuffers[I].Size := 0;
    end;

  SetLength(FBuffers, 0);

{$IFDEF V4L2_CAMERA_DEBUG}
  WriteLn('OK.');
{$ENDIF}
end;

procedure TV4L2Camera.TCaptureThread.Execute;
var
  I, Status, Res: Integer;
  FDStatus: TFDSet;
  Time: timeval;
  Buffer: v4l2_buffer;
begin
  for I := 0 to FBufferCount - 1 do
  begin
  {$IFDEF V4L2_CAMERA_DEBUG}
    Write('[CT] Queing buffer ', I, '...');
  {$ENDIF}

    FillChar(Buffer, SizeOf(v4l2_buffer), 0);

    Buffer.index := I;
    Buffer.&type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
    Buffer.memory := V4L2_MEMORY_MMAP;
    if FpIOCtl(FHandle, VIDIOC_QBUF, @Buffer) < 0 then
      raise EV4L2QueueBuffer.Create(Format(SCannotQueueCameraBuffer, [I]));

  {$IFDEF V4L2_CAMERA_DEBUG}
    WriteLn('OK.');
  {$ENDIF}
  end;

{$IFDEF V4L2_CAMERA_DEBUG}
  Write('[CS] Starting streaming...');
{$ENDIF}

  Status := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  if FpIOCtl(FHandle, VIDIOC_STREAMON, @Status) < 0 then
    raise EV4L2StreamStart.Create(SCannotStartCameraStreaming);
  try
  {$IFDEF V4L2_CAMERA_DEBUG}
    WriteLn('OK.');
  {$ENDIF}

    while not Terminated do
    begin
    {$IFDEF V4L2_CAMERA_DEBUG}
      Write('[CS] Waiting...');
    {$ENDIF}

      repeat
        fpFD_ZERO(FDStatus);
        fpFD_SET(FHandle, FDStatus);

        Time.tv_sec := 10;
        Time.tv_usec := 0;

        Res := fpSelect(FHandle + 1, @FDStatus, nil, nil, @Time);
      until (Res <> -1) or (errno <> ESysEINTR);

      if Res = -1 then
        raise EV4L2StreamWait.Create(SFailedWaitingForCameraStream);

    {$IFDEF V4L2_CAMERA_DEBUG}
      WriteLn('DONE.');
      Write('[CS] Dequeuing buffer...');
    {$ENDIF}

      FillChar(Buffer, SizeOf(v4l2_buffer), 0);
      Buffer.&type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
      Buffer.memory := V4L2_MEMORY_MMAP;
      if FpIOCtl(FHandle, VIDIOC_DQBUF, @Buffer) < 0 then
        raise EV4L2ReceiveBuffers.Create(SCannotReceiveCameraBuffers);

    {$IFDEF V4L2_CAMERA_DEBUG}
      WriteLn('got #', Buffer.index, '.');
    {$ENDIF}

      if (FCamera <> nil) and Assigned(FCamera.FOnCapture) then
      begin
        FCamera.FOnCapture(FCamera, FBuffers[Buffer.index].Memory, Buffer.length);

      {$IFDEF V4L2_CAMERA_DEBUG}
        WriteLn('[CS] Notified OnCapture event about ', Buffer.length, ' bytes.');
      {$ENDIF}
      end
      else if (FImageBuffer <> nil) and (FImageBufferSize >= Buffer.length) then
      begin
        FImageBufferSection.Enter;
        try
          Move(FBuffers[Buffer.index].Memory^, FImageBuffer^, Buffer.length);
        finally
          FImageBufferSection.Leave;
        end;

      {$IFDEF V4L2_CAMERA_DEBUG}
        WriteLn('[CS] Copied ', Buffer.length, ' bytes to front buffer.');
      {$ENDIF}
      end;

      if FCamera <> nil then
        InterLockedIncrement(FCamera.FFramesCaptured);

    {$IFDEF V4L2_CAMERA_DEBUG}
      Write('[CS] Queuing buffer #', Buffer.index, '...');
    {$ENDIF}

      if FpIOCtl(FHandle, VIDIOC_QBUF, @Buffer) < 0 then
        raise EV4L2QueueBuffer.Create(Format(SCannotQueueCameraBuffer, [I]));

    {$IFDEF V4L2_CAMERA_DEBUG}
      WriteLn('OK.');
    {$ENDIF}
    end;
  finally
    Status := V4L2_BUF_TYPE_VIDEO_CAPTURE;
    FpIOCtl(FHandle, VIDIOC_STREAMOFF, @Status);
  end;

{$IFDEF V4L2_CAMERA_DEBUG}
  WriteLn('[CS] Streaming Stopped.');
{$ENDIF}
end;

{$ENDREGION}
{$REGION 'TV4L2Camera'}

constructor TV4L2Camera.Create(const ASystemPath: StdString);
begin
  inherited Create;

  FSystemPath := ASystemPath;

  FHandle := FpOpen(FSystemPath, O_RDWR or O_NONBLOCK);
  if FHandle < 0 then
  begin
    FHandle := 0;
    raise EV4L2FileOpen.Create(Format(SCannotOpenCameraDeviceFile, [FSystemPath]));
  end;

{$IFDEF V4L2_CAMERA_DEBUG}
  Write('[V4L2] Setting video format...');
{$ENDIF}

  if not TryVideoFormat(V4L2_PIX_FMT_BGR32) then
    if not TryVideoFormat(V4L2_PIX_FMT_YUYV) then
      raise EV4L2UnsupportedFormat.Create(Format(SUnsupportedCameraFormat, [FormatToString(FPixelFormat)]));

{$IFDEF V4L2_CAMERA_DEBUG}
  WriteLn('OK.');
  Write('Setting video size...');
{$ENDIF}

  SetSize(DefaultVideoSize);

{$IFDEF V4L2_CAMERA_DEBUG}
  WriteLn('OK.');
{$ENDIF}
end;

destructor TV4L2Camera.Destroy;
begin
  if FCaptureStream <> nil then
    StopCapture;

  if FHandle <> 0 then
  begin
    FpClose(FHandle);
    FHandle := 0;
  end;

  inherited;
end;

function TV4L2Camera.FormatToString(const AVideoFormat: Cardinal): StdString;
var
  Chars: array[0..3] of AnsiChar absolute AVideoFormat;
begin
  Result := Chars[0] + Chars[1] + Chars[2] + Chars[3];
end;

function TV4L2Camera.TryVideoFormat(const AVideoFormat: Cardinal): Boolean;
var
  LFormat: v4l2_format;
begin
  FillChar(LFormat, SizeOf(v4l2_format), 0);
  LFormat.&type := V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if FpIOCtl(FHandle, VIDIOC_G_FMT, @LFormat) < 0 then
    raise EV4L2GetFormat.Create(SCannotObtainCameraFormat);

//  Format.pix.field := V4L2_FIELD_INTERLACED;  // - this is not necessary
  LFormat.pix.pixelformat := AVideoFormat;

  if FpIOCtl(FHandle, VIDIOC_S_FMT, @LFormat) < 0 then
    raise EV4L2SetPixelFormat.Create(SCannotSetCameraPixelFormat);

// Uncomment to show some interesting information about current camera format.
{ WriteLn('Width: ', LFormat.pix.width);
  WriteLn('Height: ', LFormat.pix.height);
  WriteLn('Bytes per Line: ', LFormat.pix.bytesperline);
  WriteLn('Size Image: ', LFormat.pix.sizeimage);
  WriteLn('Pixel Format: ', FormatToString(LFormat.pix.pixelformat)); }

  FPixelFormat := LFormat.pix.pixelformat;
  Result := LFormat.pix.pixelformat = AVideoFormat;
end;

procedure TV4L2Camera.SetSize(const Value: TPoint2px);
var
  WasCapturing: Boolean;
  LFormat: v4l2_format;
begin
  if FSize <> Value then
  begin
    WasCapturing := GetCapturing;
    if WasCapturing then
      StopCapture;

    FSize := Value;

    FillChar(LFormat, SizeOf(v4l2_format), 0);
    LFormat.&type := V4L2_BUF_TYPE_VIDEO_CAPTURE;

    if FpIOCtl(FHandle, VIDIOC_G_FMT, @LFormat) < 0 then
      raise EV4L2GetFormat.Create(SCannotObtainCameraFormat);

    LFormat.pix.width := FSize.X;
    LFormat.pix.height := FSize.Y;

    if FpIOCtl(FHandle, VIDIOC_S_FMT, @LFormat) < 0 then
      raise EV4L2SetVideoSize.Create(Format(SCannotSetCameraVideoSize, [FSize.X, FSize.Y]));

    FSize.X := LFormat.pix.width;
    FSize.Y := LFormat.pix.height;

    if WasCapturing then
      StartCapture;
  end;
end;

function TV4L2Camera.GetCapturing: Boolean;
begin
  Result := FCaptureStream <> nil;
end;

procedure TV4L2Camera.StartCapture(const ABufferCount: Integer);
begin
  if FCaptureStream <> nil then
    raise EV4L2AlreadyStreaming.Create(SCameraIsAlreadyStreaming);

  FFramesCaptured := 0;
  FCaptureStream := TCaptureThread.Create(Self, FHandle, ABufferCount);
end;

procedure TV4L2Camera.StopCapture;
begin
  if FCaptureStream = nil then
    raise EV4L2NotStreaming.Create(SCameraIsCurrentlyNotStreaming);

  FCaptureStream.Terminate;
  FCaptureStream.WaitFor;

  FreeAndNil(FCaptureStream);
end;

procedure TV4L2Camera.ConvertImageYUYV(const Source: Pointer; const Surface: TPixelSurface);
type
  PPixelYUYV = ^TPixelYUYV;
  TPixelYUYV = record
    Y1, U, Y2, V: Byte;
  end;

  procedure PixelToColors(SrcPixel: PPixelYUYV; var DestPixels: PIntColor); inline;
  var
    YValue: Integer;
  begin
    YValue := (Integer(SrcPixel.Y1) - 16) * 298;

    PIntColorRec(DestPixels).Red := Saturate((YValue + 409 * (Integer(SrcPixel.V) - 128) + 128) div 256, 0, 255);
    PIntColorRec(DestPixels).Green := Saturate((YValue - 100 * (Integer(SrcPixel.U) - 128) - 208 *
      (Integer(SrcPixel.V) - 128) + 128) div 256, 0, 255);
    PIntColorRec(DestPixels).Blue := Saturate((YValue + 516 * (Integer(SrcPixel.U) - 128) + 128) div 256, 0, 255);
    PIntColorRec(DestPixels).Alpha := 255;

    Inc(DestPixels);

    YValue := (Integer(SrcPixel.Y2) - 16) * 298;

    PIntColorRec(DestPixels).Red := Saturate((YValue + 409 * (Integer(SrcPixel.V) - 128) + 128) div 256, 0, 255);
    PIntColorRec(DestPixels).Green := Saturate((YValue - 100 * (Integer(SrcPixel.U) - 128) - 208 *
      (Integer(SrcPixel.V) - 128) + 128) div 256, 0, 255);
    PIntColorRec(DestPixels).Blue := Saturate((YValue + 516 * (Integer(SrcPixel.U) - 128) + 128) div 256, 0, 255);
    PIntColorRec(DestPixels).Alpha := 255;

    Inc(DestPixels);
  end;

var
  I, J: Integer;
  SrcPixel: PPixelYUYV;
  DestPixel: PIntColor;
begin
  SrcPixel := Source;

  for J := 0 to Surface.Height - 1 do
  begin
    DestPixel := Surface.Scanline[J];

    for I := 0 to (Surface.Width div 2) - 1 do
    begin
      PixelToColors(SrcPixel, DestPixel);
      Inc(SrcPixel);
    end;
  end;
end;

procedure TV4L2Camera.TakeSnapshot(const DestSurface: TPixelSurface);
var
  Buffer: Pointer;
  I: Integer;
begin
  if (FCaptureStream = nil) or (FCaptureStream.ImageBufferSection = nil) then
    raise EV4L2NotStreaming.Create(SCameraIsCurrentlyNotStreaming);

  GetMem(Buffer, FCaptureStream.ImageBufferSize);
  try
    FCaptureStream.ImageBufferSection.Enter;
    try
      Move(FCaptureStream.ImageBuffer^, Buffer^, FCaptureStream.ImageBufferSize);
    finally
      FCaptureStream.ImageBufferSection.Leave;
    end;

    if FPixelFormat = V4L2_PIX_FMT_BGR32 then
    begin
      if (DestSurface.Size <> FSize) or (DestSurface.PixelFormat <> TPixelFormat.X8B8G8R8) then
        DestSurface.SetSize(FSize, TPixelFormat.X8B8G8R8);

      for I := 0 to FSize.Y - 1 do
        Move(Pointer(PtrUInt(Buffer) + (I * FSize.X * SizeOf(TIntColor)))^, DestSurface.Scanline[I]^,
          FSize.X * SizeOf(TIntColor));
    end
    else
    begin
      if (DestSurface.Size <> FSize) or (DestSurface.PixelFormat <> TPixelFormat.X8R8G8B8) then
        DestSurface.SetSize(FSize, TPixelFormat.X8R8G8B8);

      ConvertImageYUYV(Buffer, DestSurface);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

{$ENDREGION}

end.
