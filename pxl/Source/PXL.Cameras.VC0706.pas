unit PXL.Cameras.VC0706;
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
{.$DEFINE CAMERA_DEBUG}

uses
  PXL.TypeDef, PXL.Cameras.Types;

type
  TCamera = class(TCustomCamera)
  protected const
    ProtocolCommandGetVersion = $11;
  private
    FBootText: StdString;
    FVersionText: StdString;

    function ImageSizeToCode(const Width, Height: Integer): Byte;
  protected
    function GetDefaultBaudRate: Integer; override;

    function ChangeCaptureStatus(const Enabled: Boolean): Boolean;
  public
    function Reset: Boolean; override;
    function GetVersion: Boolean;

    function SetImageSize(const Width, Height: Integer): Boolean; override;
    function TakeSnapshot: Boolean; override;

    function GetPictureSize: Integer; override;
    function GetPicture(out Buffer: Pointer; out BufferSize: Integer): Boolean; override;

    function ResumeCapture: Boolean;
    function StopCapture: Boolean;

    property BootText: StdString read FBootText;
    property VersionText: StdString read FVersionText;
  end;

implementation

uses
{$IFDEF CAMERA_DEBUG}
  PXL.Logs,
{$ENDIF}

  SysUtils;

function TCamera.GetDefaultBaudRate: Integer;
begin
  Result := 38400;
end;

function TCamera.ImageSizeToCode(const Width, Height: Integer): Byte;
begin
  if (Width = 160) and (Height = 120) then
    Result := $22
  else if (Width = 320) and (Height = 240) then
    Result := $11
  else if (Width = 640) and (Height = 480) then
    Result := $00
  else
    Result := $FF;
end;

function TCamera.Reset: Boolean;
begin
  if not SendCommand(ProtocolCommandReset) then
    Exit(False);

  if not ReceiveAck(ProtocolCommandReset) then
    Exit(False);

  FBootText := ReceiveText;
  Result := Length(FBootText) > 0;
end;

function TCamera.SetImageSize(const Width, Height: Integer): Boolean;
var
  Code: Byte;
begin
  Code := ImageSizeToCode(Width, Height);
  if Code = $FF then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.SedImageSize: Unsupported image size.');
  {$ENDIF}
    Exit(False);
  end;

  if not SendCommand(ProtocolCommandSetImageSize, [Code]) then
    Exit(False);

  Result := ReceiveAck(ProtocolCommandSetImageSize);
end;

function TCamera.GetVersion: Boolean;
begin
  if not SendCommand(ProtocolCommandGetVersion) then
    Exit(False);

  Result := ReceiveAckString(ProtocolCommandGetVersion, FVersionText);
end;

function TCamera.ChangeCaptureStatus(const Enabled: Boolean): Boolean;
var
  ControlFlag: Byte;
begin
  if Enabled then
    ControlFlag := $02
  else
    ControlFlag := $00;

  if not SendCommand(ProtocolCommandBufferControl, [ControlFlag]) then
    Exit(False);

  Result := ReceiveAck(ProtocolCommandBufferControl);
end;

function TCamera.TakeSnapshot: Boolean;
begin
  Result := ChangeCaptureStatus(False);
end;

function TCamera.GetPictureSize: Integer;
begin
  if not SendCommand(ProtocolCommandGetBufferSize, [$00]) then
    Exit(0);

  if not ReceiveAckInt32(ProtocolCommandGetBufferSize, Result) then
    Result := 0;
end;

function TCamera.GetPicture(out Buffer: Pointer; out BufferSize: Integer): Boolean;
const
  DataRetrieveDelay = 40; // ms
  DataReactTimeout = 500; // ms
var
  BytesRead: Integer;
begin
  Buffer := nil;

  BufferSize := GetPictureSize;
  if BufferSize <= 0 then
    Exit(False);

  if not SendCommand(ProtocolCommandGetBufferData, [$00, $0A, $00, $00, $00, $00, Cardinal(BufferSize) shr 24,
    (Cardinal(BufferSize) shr 16) and $FF, (Cardinal(BufferSize) shr 8) and $FF, Cardinal(BufferSize) and $FF,
    $02, $00]) then
    Exit(False);

  if not ReceiveAck(ProtocolCommandGetBufferData) then
    Exit(False);

  Sleep(DataRetrieveDelay);

  Buffer := AllocMem(BufferSize);

  BytesRead := DataPort.ReadBuffer(Buffer, BufferSize, DataReactTimeout + ComputeBaudTimeout(BufferSize));
  if BytesRead <> BufferSize then
  begin
    FreeMemAndNil(Buffer);
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.GetPicture: Failed reading data, obtained ' + IntToStr(BytesRead) + ' out of ' +
      IntToStr(BufferSize) + ' bytes.');
  {$ENDIF}
    Exit(False);
  end;

  Result := ReceiveAck(ProtocolCommandGetBufferData);
end;

function TCamera.ResumeCapture: Boolean;
begin
  Result := ChangeCaptureStatus(True);
end;

function TCamera.StopCapture: Boolean;
begin
  Result := ChangeCaptureStatus(False);
end;

end.
