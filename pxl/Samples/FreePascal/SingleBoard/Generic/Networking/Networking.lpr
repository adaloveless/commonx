program Networking;
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
  This example illustrates communication between applications on different devices through UDP protocol.

  The communication is compliant and should work along with desktop-grade "Networking" sample. That is, this and other
  sample can communicate to each other. This application can also communicate with itself running on a different
  device.

  Since in this application the terminal is used for reading and writing messages, it is not very comfortable to use,
  but it is meant to illustrate the usage of NetCom on singleboard devices, which is the same as on desktop.
}
uses
  Crt, Classes, SysUtils, PXL.TypeDef, PXL.Classes, PXL.Timing, PXL.NetComs;

type
  TApplication = class
  private const
    DefaultPort = 7500;
    KeySpace = #32;
    KeyEscape = #27;
  private
    FNetCom: TNetCom;

    procedure OnReceiveData(const Sender: TObject; const Host: StdString; const Port: Integer; const Data: Pointer;
      const Size: Integer);

    procedure SeparateHostAndPort(const Text: StdString; out Host: StdString; out Port: Integer);
    procedure SendTextMessage(const DestHost: StdString; const DestPort: Integer; const MsgText: StdString);

    function WaitForValidKey: StdChar;
    procedure AskForMessageToSend;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
  end;

constructor TApplication.Create;
begin
  inherited;

  FNetCom := TNetCom.Create;

  // Assign event, which will be invoked when some message arrives.
  FNetCom.OnReceive := OnReceiveData;

  // Try to initialize NetCom with default port.
  FNetCom.LocalPort := DefaultPort;

  if not FNetCom.Initialize then
  begin
    // Default port seems to be used, try any available port (it will be choosen automatically).
    FNetCom.LocalPort := 0;

    if not FNetCom.Initialize then
      raise Exception.Create('Could not initialize networking component.');
  end;

  WriteLn('Listening at port: ', FNetCom.LocalPort);
end;

destructor TApplication.Destroy;
begin
  FNetCom.Free;

  inherited;
end;

procedure TApplication.OnReceiveData(const Sender: TObject; const Host: StdString; const Port: Integer;
  const Data: Pointer; const Size: Integer);
var
  Stream: TMemoryStream;
  InpText: StdString;
begin
  Stream := TMemoryStream.Create;
  try
    // Put the chunk of binary data into the stream.
    Stream.WriteBuffer(Data^, Size);

    // Move current stream position back to the beginning.
    Stream.Position := 0;

    // Try to read a readable string from the stream.
    InpText := Stream.GetShortString;

    // Additional data can be read here from the stream using reverse order in which it was embedded previously.
  finally
    Stream.Free;
  end;

  WriteLn('Received "', InpText, '" from ', Host, ':', IntToStr(Port) + '.');
end;

procedure TApplication.SeparateHostAndPort(const Text: StdString; out Host: StdString; out Port: Integer);
var
  ColonPos: Integer;
begin
  ColonPos := Pos(':', Text);
  if ColonPos = 0 then
  begin
    Host := Text;
    Port := DefaultPort;
  end
  else
  begin
    Host := Trim(Copy(Text, 1, ColonPos - 1));
    Port := StrToIntDef(Trim(Copy(Text, ColonPos + 1, Length(Text) - ColonPos)), DefaultPort);
  end;
end;

procedure TApplication.SendTextMessage(const DestHost: StdString; const DestPort: Integer; const MsgText: StdString);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    // Put the message into the stream.
    Stream.PutShortString(MsgText);

    // Additional data can be written and embedded here to the stream.

    // Send the stream as a chunk of binary data.
    FNetCom.Send(DestHost, DestPort, Stream.Memory, Stream.Size);
  finally
    Stream.Free;
  end;
end;

function TApplication.WaitForValidKey: StdChar;
begin
  Result := #0;

  repeat
    if KeyPressed then
    begin
      Result := ReadKey;

      // Accept only SPACE or ESC.
      if (Result <> KeySpace) and (Result <> KeyEscape) then
        Result := #0;
    end;

    // This tells NetCom to check for any incoming messages and if such arrive, invoke OnDataReceive event.
    FNetCom.Update;

    Sleep(100); // wait for 100 ms
  until Result <> #0;
end;

procedure TApplication.AskForMessageToSend;
var
  DestText, DestHost: StdString;
  DestPort: Integer;
begin
  WriteLn('Please type destination address and port separated by ":", something like: "192.168.0.2:7500".');
  Write('> ');

  ReadLn(DestText);
  if Length(DestText) <= 0 then
    Exit;

  SeparateHostAndPort(DestText, DestHost, DestPort);
  WriteLn('Type message for host "', DestHost, '" and port "', DestPort, '":');
  Write('> ');

  ReadLn(DestText);
  if Length(DestText) <= 0 then
    Exit;

  SendTextMessage(DestHost, DestPort, DestText);
  WriteLn('Message sent.');
end;

procedure TApplication.Execute;
var
  Key: StdChar;
begin
  repeat
    WriteLn('Waiting for incoming messages, press ESC to exit or SPACE to send a message...');

    Key := WaitForValidKey;

    if Key = KeySpace then
      AskForMessageToSend;
  until Key = KeyEscape;
end;

var
  Application: TApplication = nil;

begin
  Application := TApplication.Create;
  try
    Application.Execute;
  finally
    Application.Free;
  end;
end.

