unit PXL.Cameras.LSY201;
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
    ProtocolCommandSetIRCut = $AA;

    SnapshotReactTimeout = 4000;
  private
    FBootText: StdString;

    function BaudRateToCode(const Value: Integer): Word;
    function ImageSizeToCode(const Width, Height: Integer): Byte;
  protected
    function GetDefaultBaudRate: Integer; override;
  public
    function Reset: Boolean; override;

    function SetImageSize(const Width, Height: Integer): Boolean; override;
    function TakeSnapshot: Boolean; override;

    function GetPictureSize: Integer; override;
    function GetPicture(out Buffer: Pointer; out BufferSize: Integer): Boolean; override;

    function SetBaudRate(const BaudRate: Integer): Boolean;
    function SetIRCut(const DayMode: Boolean): Boolean;

    property BootText: StdString read FBootText;
  end;

implementation

uses
{$IFDEF CAMERA_DEBUG}
  PXL.Logs,
{$ENDIF}

  SysUtils;

function TCamera.GetDefaultBaudRate: Integer;
begin
  Result := 115200;
end;

function TCamera.BaudRateToCode(const Value: Integer): Word;
begin
  if Value = 9600 then
    Result := $AEC8
  else if Value = 19200 then
    Result := $56E4
  else if Value = 38400 then
    Result := $2AF2
  else if Value = 57600 then
    Result := $1C4C
  else if Value = 115200 then
    Result := $0DA6
  else
    Result := $0000;
end;

function TCamera.ImageSizeToCode(const Width, Height: Integer): Byte;
begin
  if (Width = 160) and (Height = 120) then
    Result := $22
  else if (Width = 320) and (Height = 240) then
    Result := $11
  else if (Width = 640) and (Height = 480) then
    Result := $00
  else if (Width = 800) and (Height = 600) then
    Result := $1D
  else if (Width = 1024) and (Height = 768) then
    Result := $1C
  else if (Width = 1280) and (Height = 960) then
    Result := $1B
  else if (Width = 1600) and (Height = 1200) then
    Result := $21
  else
    Result := $FF;
end;

function TCamera.Reset: Boolean;
begin
  if not SendCommand(ProtocolCommandReset) then
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

function TCamera.TakeSnapshot: Boolean;
begin
  if not SendCommand(ProtocolCommandBufferControl, [$00]) then
    Exit(False);

  Result := ReceiveAck(ProtocolCommandBufferControl, SnapshotReactTimeout);
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

function TCamera.SetBaudRate(const BaudRate: Integer): Boolean;
var
  Code: Word;
begin
  Code := BaudRateToCode(BaudRate);
  if Code = 0 then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.SedBaudRate: Unsupported baud rate value.');
  {$ENDIF}
    Exit(False);
  end;

  if not SendCommand(ProtocolCommandSetBaudRate, [$01, Code shr 8, Code and $FF]) then
    Exit(False);

  Result := ReceiveAck(ProtocolCommandSetBaudRate);
end;

function TCamera.SetIRCut(const DayMode: Boolean): Boolean;
var
  Code: Byte;
//  BytesWritten: Integer;
//  Values: array[0..4] of Byte;
begin
  if DayMode then
    Code := $00
  else
    Code := $01;

(*  Values[0] := ProtocolSendID;
  Values[1] := ProtocolSerialNo;
  Values[2] := ProtocolCommandSetIRCut;
  Values[4] := $00;

  if DayMode then
    Values[3] := $00
  else
    Values[3] := $01;

  BytesWritten := SerialPort.WriteBuffer(@Values[0], SizeOf(Values), ComputeBaudTimeout(SizeOf(Values)));
  if BytesWritten <> SizeOf(Values) then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.SetIRCut failed, sending ' + IntToStr(BytesWritten) + ' out of ' +
      IntToStr(SizeOf(Values)) + ' bytes:');
    LogDumpBytes(@Values[0], SizeOf(Values));
  {$ENDIF}
    Exit(False);
  end;*)

  if not SendCommand(ProtocolCommandSetIRCut, [Code]) then
    Exit(False);

  Result := ReceiveAck(ProtocolCommandSetIRCut);
end;

end.
