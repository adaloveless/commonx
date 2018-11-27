unit PXL.Cameras.Types;
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
  PXL.TypeDef, PXL.Boards.Types;

type
  TCustomCamera = class
  protected const
    ProtocolSendID = $56;
    ProtocolReceiveID = $76;
    ProtocolSerialNo = $00;
    ProtocolDataEmpty = $00;
    ProtocolStatusOK = $00;

    ProtocolCommandReset = $26;
    ProtocolCommandSetBaudRate = $24;
    ProtocolCommandSetImageSize = $54;
    ProtocolCommandBufferControl = $36;
    ProtocolCommandGetBufferSize = $34;
    ProtocolCommandGetBufferData = $32;

    DefaultReactTimeout = 50;

    DefaultTextCharacterLimit = 256;
    DefaultTextTimeout = 500;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDataPort: TCustomPortUART;
  protected
  {$IFDEF CAMERA_DEBUG}
    procedure LogDumpBytes(const Bytes: PByte; const ByteCount: Integer);
  {$ENDIF}

    function ComputeBaudTimeout(const ByteCount: Integer): Integer; virtual;

    function SendCommand(const Command: Integer; const Data: array of Byte): Boolean; overload;
    function SendCommand(const Command: Integer): Boolean; overload;
    function ReceiveAck(const Command: Integer; const ReactTimeout: Integer = DefaultReactTimeout): Boolean;
    function ReceiveAckInt32(const Command: Integer; out Value: Integer;
      const ReactTimeout: Integer = DefaultReactTimeout): Boolean;
    function ReceiveAckString(const Command: Integer; out Text: StdString;
      const StringReactTimeout: Integer = DefaultReactTimeout;
      const ReactTimeout: Integer = DefaultReactTimeout): Boolean;

    function ReceiveText(const MaxCharacters: Integer = DefaultTextCharacterLimit;
      const Timeout: Integer = DefaultTextTimeout): StdString;

    function GetDefaultBaudRate: Integer; virtual;
  public
    constructor Create(const ADataPort: TCustomPortUART);
    destructor Destroy; override;

    function Reset: Boolean; virtual; abstract;
    function SetImageSize(const Width, Height: Integer): Boolean; virtual; abstract;

    function TakeSnapshot: Boolean; virtual; abstract;

    function GetPictureSize: Integer; virtual; abstract;
    function GetPicture(out Buffer: Pointer; out BufferSize: Integer): Boolean; virtual; abstract;

    property DataPort: TCustomPortUART read FDataPort;
    property DefaultBaudRate: Integer read GetDefaultBaudRate;
  end;

implementation

uses
{$IFDEF CAMERA_DEBUG}
  PXL.Logs, Math,
{$ENDIF}

  SysUtils;

constructor TCustomCamera.Create(const ADataPort: TCustomPortUART);
begin
  inherited Create;

  FDataPort := ADataPort;
end;

destructor TCustomCamera.Destroy;
begin
  inherited;
end;

{$IFDEF CAMERA_DEBUG}
procedure TCustomCamera.LogDumpBytes(const Bytes: PByte; const ByteCount: Integer);
var
  SrcByte: PByte;
  I: Integer;
begin
  SrcByte := Bytes;
  for I := 0 to ByteCount - 1 do
  begin
    LogText('  [byte ' + IntToStr(I) + '] = ' + IntToHex(SrcByte^, 2) + 'h');
    Inc(SrcByte);
  end;
end;
{$ENDIF}

function TCustomCamera.ComputeBaudTimeout(const ByteCount: Integer): Integer;
const
  BitsPerByte = 12; // assume generous extra 4 bits wasted in case of losses or other delays
var
  BytesPerMSec: Int64;
  TimeNeeded: Integer;
begin
  BytesPerMSec := Int64(1000) * Int64(FDataPort.BaudRate) div BitsPerByte;
  if BytesPerMSec <= 0 then
    Exit(0);

  TimeNeeded := (Int64(1000000) * Int64(ByteCount)) div BytesPerMSec;
  Result := TimeNeeded + (10 - (TimeNeeded mod 10))
end;

function TCustomCamera.SendCommand(const Command: Integer; const Data: array of Byte): Boolean;
var
  Values: array of Byte;
  BytesWritten, I: Integer;
begin
  SetLength(Values, 4 + Length(Data));

  Values[0] := ProtocolSendID;
  Values[1] := ProtocolSerialNo;
  Values[2] := Command;
  Values[3] := Length(Data);

  for I := 0 to Length(Data) - 1 do
    Values[4 + I] := Data[I];

  BytesWritten := FDataPort.WriteBuffer(@Values[0], Length(Values), ComputeBaudTimeout(Length(Values)));
  Result := BytesWritten = Length(Values);

{$IFDEF CAMERA_DEBUG}
  if not Result then
  begin
    LogText(ClassName + '.SendCommand (extended) failed, sending ' + IntToStr(BytesWritten) + ' out of ' +
      IntToStr(Length(Values)) + ' bytes:');
    LogDumpBytes(@Values[0], Length(Values));
  end;
{$ENDIF}
end;

function TCustomCamera.SendCommand(const Command: Integer): Boolean;
var
  Values: array[0..3] of Byte;
  BytesWritten: Integer;
begin
  Values[0] := ProtocolSendID;
  Values[1] := ProtocolSerialNo;
  Values[2] := Command;
  Values[3] := ProtocolDataEmpty;

  BytesWritten := FDataPort.WriteBuffer(@Values[0], SizeOf(Values), ComputeBaudTimeout(SizeOf(Values)));
  Result := BytesWritten = SizeOf(Values);

{$IFDEF CAMERA_DEBUG}
  if not Result then
  begin
    LogText(ClassName + '.SendCommand failed, sending ' + IntToStr(BytesWritten) + ' out of ' +
      IntToStr(Length(Values)) + ' bytes:');
    LogDumpBytes(@Values[0], SizeOf(Values));
  end;
{$ENDIF}
end;

function TCustomCamera.ReceiveAck(const Command: Integer;
  const ReactTimeout: Integer): Boolean;
var
  Values: array[0..4] of Byte;
  BytesRead: Integer;
begin
  BytesRead := FDataPort.ReadBuffer(@Values[0], SizeOf(Values), ComputeBaudTimeout(SizeOf(Values)) + ReactTimeout);
  if BytesRead <> SizeOf(Values) then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.ReceiveAck failed, reading ' + IntToStr(BytesRead) + ' out of ' + IntToStr(SizeOf(Values)) +
      ' bytes:');
    LogDumpBytes(@Values[0], Min(BytesRead, SizeOf(Values)));
  {$ENDIF}
    Exit(False);
  end;

  if (Values[0] <> ProtocolReceiveID) or (Values[1] <> ProtocolSerialNo) or (Values[2] <> Command) or
    (Values[3] <> ProtocolStatusOK) or (Values[4] <> ProtocolDataEmpty) then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.ReceiveAck failed, due to unexpected response.');
    LogText('  Expected: ' + IntToHex(ProtocolReceiveID, 2) + 'h, ' + IntToHex(ProtocolSerialNo, 2) + 'h, ' +
      IntToHex(Command, 2) + 'h, ' + IntToHex(ProtocolStatusOK, 2) + 'h, ' + IntToHex(ProtocolDataEmpty, 2) + 'h.');
    LogDumpBytes(@Values[0], SizeOf(Values));
  {$ENDIF}
    Exit(False);
  end;

  Result := True;
end;

function TCustomCamera.ReceiveAckInt32(const Command: Integer; out Value: Integer;
  const ReactTimeout: Integer): Boolean;
const
  ByteCount = 9;
var
  Values: array[0..ByteCount - 1] of Byte;
  BytesRead: Integer;
begin
  BytesRead := FDataPort.ReadBuffer(@Values[0], ByteCount, ComputeBaudTimeout(ByteCount) + ReactTimeout);
  if BytesRead <> ByteCount then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.ReceiveAckInt32 failed, reading ' + IntToStr(BytesRead) + ' out of ' + IntToStr(ByteCount) +
      ' bytes:');
    LogDumpBytes(@Values[0], Min(BytesRead, ByteCount));
  {$ENDIF}
    Exit(False);
  end;

  if (Values[0] <> ProtocolReceiveID) or (Values[1] <> ProtocolSerialNo) or (Values[2] <> Command) or
    (Values[3] <> ProtocolStatusOK) or (Values[4] <> 4) then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.ReceiveAckInt32 failed, due to unexpected response.');
    LogText('  Expected: ' + IntToHex(ProtocolReceiveID, 2) + 'h, ' + IntToHex(ProtocolSerialNo, 2) + 'h, ' +
      IntToHex(Command, 2) + 'h, ' + IntToHex(ProtocolStatusOK, 2) + 'h, 04h.');
    LogDumpBytes(@Values[0], SizeOf(Values));
  {$ENDIF}
    Exit(False);
  end;

  Value := Integer(Cardinal(Values[5]) shl 24) or Integer(Cardinal(Values[6]) shl 16) or
    Integer(Cardinal(Values[7]) shl 8) or Integer(Cardinal(Values[8]));

  Result := True;
end;

function TCustomCamera.ReceiveAckString(const Command: Integer; out
  Text: StdString; const StringReactTimeout: Integer;
  const ReactTimeout: Integer): Boolean;
const
  ByteCount = 5;
var
  Values: array[0..ByteCount - 1] of Byte;
  BytesRead, ExpectedStringSize: Integer;
begin
  BytesRead := FDataPort.ReadBuffer(@Values[0], ByteCount, ComputeBaudTimeout(ByteCount) + ReactTimeout);
  if BytesRead <> ByteCount then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.ReceiveAckString failed, reading ' + IntToStr(BytesRead) + ' out of ' +
      IntToStr(ByteCount) + ' bytes:');
    LogDumpBytes(@Values[0], Min(BytesRead, ByteCount));
  {$ENDIF}
    Exit(False);
  end;

  if (Values[0] <> ProtocolReceiveID) or (Values[1] <> ProtocolSerialNo) or (Values[2] <> Command) or
    (Values[3] <> ProtocolStatusOK) or (Values[4] <= 0) then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.ReceiveAckString failed, due to unexpected response.');
    LogText('  Expected: ' + IntToHex(ProtocolReceiveID, 2) + 'h, ' + IntToHex(ProtocolSerialNo, 2) + 'h, ' +
      IntToHex(Command, 2) + 'h, ' + IntToHex(ProtocolStatusOK, 2) + 'h, XXh.');
    LogDumpBytes(@Values[0], SizeOf(Values));
  {$ENDIF}
    Exit(False);
  end;

  ExpectedStringSize := Values[4];

  Result := FDataPort.ReadString(Text, ExpectedStringSize, ComputeBaudTimeout(ExpectedStringSize) +
    StringReactTimeout);
  if Length(Text) <> ExpectedStringSize then
  begin
  {$IFDEF CAMERA_DEBUG}
    LogText(ClassName + '.ReceiveAckString: Expected ' + IntToStr(ExpectedStringSize) + ' string length, but got ' +
      IntToStr(Length(Text)) + ' bytes.');
    if Length(Text) > 0 then
      LogText('  Received string is: ' + Text);
  {$ENDIF}
    Result := False;
  end;
end;

function TCustomCamera.ReceiveText(const MaxCharacters, Timeout: Integer): StdString;
begin
  if not FDataPort.ReadString(Result, MaxCharacters, Timeout) then
  begin
  {$IFDEF CAMERA_DEBUG}
    if Length(Result) <= 0 then
      LogText(ClassName + '.ReceiveText: Failed reading text.');
  {$ENDIF}
    Exit('');
  end;

{$IFDEF CAMERA_DEBUG}
  if Length(Result) <= 0 then
    LogText(ClassName + '.ReceiveAckString: Expected text, but got nothing.');
{$ENDIF}
end;

function TCustomCamera.GetDefaultBaudRate: Integer;
begin
  Result := 38400;
end;

end.

