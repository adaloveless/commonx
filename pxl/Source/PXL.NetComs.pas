unit PXL.NetComs;
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
{< Provides communication and multiplayer capabilities by using simple message system based on UDP communication
  protocol. }
interface

{$INCLUDE PXL.Config.inc}
{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  {$IFDEF FPC}
    WinSock2,
  {$ELSE}
    WinSock,
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF UNIX}
    termio, BaseUnix,
  {$ENDIF}

  Sockets,
{$ELSE}
  {$IFDEF POSIX}
    {$DEFINE DELPHI_POSIX}
    {$WARN UNIT_PLATFORM OFF}
    Posix.Errno, Posix.NetinetIn, Posix.Unistd, Posix.SysSocket, Posix.Fcntl,
  {$ENDIF}
{$ENDIF}

  PXL.TypeDef;

type
  { A simple communication component that can transmit and receive messages through UDP protocol over local network
    and/or Internet. }
  TNetCom = class
  private const
    MaximumPacketSize = 8166;
  private type
  {$IFDEF DELPHI_POSIX}
    TSocket = Integer;
    TInAddr = record
      S_addr: Cardinal;
    end;

    TSockAddr = record
      sin_family: Word;
      sin_port: Word;
      sin_addr: TInAddr;
      sin_zero: array[0..7] of Byte;
    end;
  {$ENDIF}

    TPacketMessage = array[0..MaximumPacketSize - 1] of Byte;
  private const
    InvalidSocket = TSocket($FFFFFFFF);
    UnknownIP = '0.0.0.0';
  public type
    { Data reception event. In this event the incoming message should be interpreted and properly handled. After this
      event executes, memory referenced by provided pointers is lost; therefore, to preserve the message it is
      necessary to copy it somewhere within this event. Source host and port can be used to identify the receiver and
      for sending replies.
     @param(Sender Reference to the class that received the message, usually @link(TNetCom).)
     @param(Host Source host that sent the message.)
     @param(Port Source port through which the message was sent.)
     @param(Data Pointer to the beginning of message block.)
     @param(Size Size of the message block.) }
    TReceiveEvent = procedure(const Sender: TObject; const Host: StdString; const Port: Integer; const Data: Pointer;
      const Size: Integer) of object;
  private
    FSocket: TSocket;
    FInitialized: Boolean;
    FLocalPort: Integer;
    FBroadcast: Boolean;
    FOnReceive: TReceiveEvent;
    FPacketMessage: TPacketMessage;

    FUpdateRefreshTime: Integer;
    FBytesReceived: Integer;
    FBytesSent: Integer;
    FSentPackets: Integer;
    FReceivedPackets: Integer;
    FBytesPerSec: Integer;
    FBytesTransferred: Integer;
    FLastTickCount: LongWord;

    class var FSessions: Integer;
{$IFDEF MSWINDOWS}
    class var FStringBuf: array[0..511] of AnsiChar;
    class var FSession: TWSAdata;
{$ENDIF}

    procedure IncrementSessions;
    procedure DecrementSessions;

    procedure SetLocalPort(const Value: Integer);
    procedure SetBroadcast(const Value: Boolean);
    procedure SetUpdateRefreshTime(const Value: Integer);

    function GetLocalIP: StdString;
    function CreateSocket(const Broadcast: Boolean): TSocket;
    procedure DestroySocket(var Handle: TSocket);
    function BindSocket(const Handle: TSocket; const LocalPort: Integer): Boolean;
    function GetSocketPort(const Handle: TSocket): Integer;
    function SetSocketToNonBlock(const Handle: TSocket): Boolean;
    function SocketSendData(const Handle: TSocket; const Data: Pointer; const DataSize: Integer;
      const DestHost: StdString; const DestPort: Integer): Boolean;
    function SocketReceiveData(const Handle: TSocket; const Data: Pointer; const MaxDataSize: Integer;
      out SrcHost: StdString; out SrcPort: Integer): Integer;

    function HostToInt(const Host: StdString): LongWord;
    function IntToHost(const Value: LongWord): StdString;
    function IP4AddrToInt(const Text: StdString): LongWord;

    function InitializeSocket: Boolean;
    procedure FinalizeSocket;
    procedure SocketReceive;
  public
    { @exclude } constructor Create;
    { @exclude } destructor Destroy; override;

    { Initializes the component and begins listening to the given port for incoming messages. @link(LocalPort) should
      be set before calling this function to set a specific listening port. If @link(LocalPort) remains zero before
      this call, the listening port will be selected by the system from among available ones and @link(LocalPort) will
      be updated to reflect this. @True is returned when the operation was successful and @False otherwise. }
    function Initialize: Boolean;

    { Finalizes the component and closes the communication link. }
    procedure Finalize;

    { Converts text containing host address into the corresponding IP address. }
    function ResolveHost(const Host: StdString): StdString;

    { Converts text containing IP address into the corresponding host string. }
    function ResolveIP(const IPAddress: StdString): StdString;

    { Sends the specified message data block to the destination.
        @param(Host Destination host or address where the message should be sent. Multicast and broadcast addresses are
          accepted, although should be used with care to not saturate the local network.)
        @param(Port Destination port where the receiver is currently listening at.)
        @param(Data Pointer to the message data block. The method copies the data to its internal structures, so it's
          not necessary to maintain the buffer after this call exits.)
        @param(Size Size of the message data block.)
        @returns(@True when the packet was sent successfully and @False when there were errors. It is important to note
          that since messages are sent through UDP protocol, @True return value doesn't necessarily mean that the
          packet was actually received.) }
    function Send(const Host: StdString; const Port: Integer; const Data: Pointer; const Size: Integer): Boolean;

    { Handles internal communication and receives incoming messages; in addition, internal structures and bandwidth
      usage are also updated. This method should be called as fast as possible and no less than once per second.
      During the call to this method, @link(OnReceive) event may occur to notify the reception of messages. }
    procedure Update;

    { Resets all statistic parameters related to the current session such as number of packets transmitted, bytes per
      second among others. }
    procedure ResetStatistics;

    { Returns IP address of current machine. If several IP addresses are present, the last address in the list is
      returned. }
    property LocalIP: StdString read GetLocalIP;

    { Indicates whether the component has been properly initialized. }
    property Initialized: Boolean read FInitialized;

    { Determines whether the communication should support broadcast and multicast messages. This can be written to only
      before the component is initialized, but can be read from at any time.}
    property Broadcast: Boolean read FBroadcast write SetBroadcast;

    { Local port that is used for listening and transmitting packets. This can be written to only before the component
      is initialized, but can be read from at any time. }
    property LocalPort: Integer read FLocalPort write SetLocalPort;

    { This event occurs when the data has been received. It should always be assigned to interpret any incoming
      messages. }
    property OnReceive: TReceiveEvent read FOnReceive write FOnReceive;

    { Time interval (in milliseconds) to consider for "BytesPerSec" calculation. }
    property UpdateRefreshTime: Integer read FUpdateRefreshTime write SetUpdateRefreshTime;

    { Indicates how many bytes were received during the entire session. }
    property BytesReceived: Integer read FBytesReceived;

    { Indicates how many bytes were sent during the entire session. }
    property BytesSent: Integer read FBytesSent;

    { Indicates how many packets were sent during the entire session. }
    property SentPackets: Integer read FSentPackets;

    { Indicates how many packets in total were received during the entire session. }
    property ReceivedPackets: Integer read FReceivedPackets;

    { Indicates the current bandwidth usage in bytes per second. In order for this variable to have meaningful values,
      it is necessary to call @link(Update) method at least once per second. }
    property BytesPerSec: Integer read FBytesPerSec;
  end;

implementation

uses
{$IFDEF FPC}
  StrUtils,
{$ENDIF}

  SysUtils, PXL.Timing;

const
  DefaultUpdateRefreshTime = 1000;

  CodeSocketGeneralError = -1;
  CodeSocketWouldBlock =
    {$IFDEF FPC}
      EsockEWOULDBLOCK
    {$ELSE}
      {$IFDEF MSWINDOWS}
        WSAEWOULDBLOCK
      {$ELSE}
        EWOULDBLOCK
      {$ENDIF}
    {$ENDIF};

constructor TNetCom.Create;
begin
  inherited;

  IncrementSessions;

  FBroadcast := False;

  FSocket := InvalidSocket;
  FUpdateRefreshTime := DefaultUpdateRefreshTime;
end;

destructor TNetCom.Destroy;
begin
  if FInitialized then
    Finalize;

  DecrementSessions;

  inherited;
end;

procedure TNetCom.IncrementSessions;
begin
{$IFDEF MSWINDOWS}
  if FSessions <= 0 then
  begin
    if WSAStartup($101, FSession) = 0 then
      Inc(FSessions);

    Exit;
  end;
{$ENDIF}

  Inc(FSessions);
end;

procedure TNetCom.DecrementSessions;
begin
{$IFDEF MSWINDOWS}
  if FSessions = 1 then
  begin
    WSACleanup;
    FillChar(FSession, SizeOf(TWSAdata), 0);
  end;
{$ENDIF}

  if FSessions > 0 then
    Dec(FSessions);
end;

procedure TNetCom.SetLocalPort(const Value: Integer);
begin
  if not FInitialized then
  begin
    FLocalPort := Value;

    if FLocalPort < 0 then
      FLocalPort := 0
    else if FLocalPort > 65535 then
      FLocalPort := 65535;
  end;
end;

procedure TNetCom.SetBroadcast(const Value: Boolean);
begin
  if not FInitialized then
    FBroadcast := Value;
end;

procedure TNetCom.SetUpdateRefreshTime(const Value: Integer);
begin
  FUpdateRefreshTime := Value;

  if FUpdateRefreshTime <= 0 then
    FUpdateRefreshTime := 1;
end;

function TNetCom.GetLocalIP: StdString;
const
  DefaultIP = '127.0.0.1';
{$IFDEF MSWINDOWS}
type
  PInAddrs = ^TInAddrs;
  TInAddrs = array [Word] of PInAddr;
var
  HostEnt: PHostEnt;
  InAddp: PInAddrs;
{$ENDIF}
begin
  if FSessions <= 0 then
    Exit(DefaultIP);

{$IFDEF MSWINDOWS}
  GetHostName(FStringBuf, SizeOf(FStringBuf));

  HostEnt := GetHostByName(FStringBuf);
  if HostEnt = nil then
    Exit;

  InAddp := Pointer(HostEnt.h_addr_list);

  if InAddp[0] <> nil then
    Result := IntToHost(InAddp[0].S_addr);
{$ELSE}
  Result := DefaultIP;
{$ENDIF}
end;

function TNetCom.CreateSocket(const Broadcast: Boolean): TSocket;
var
  SocketOption: LongWord;
begin
{$IFDEF FPC}
  Result := fpSocket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
{$ELSE}
  Result := Socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
{$ENDIF}

  if (Result <> InvalidSocket) and Broadcast then
  begin
    SocketOption := Ord(True);

{$IFDEF FPC}
    fpSetSockOpt(Result, SOL_SOCKET, SO_BROADCAST, @SocketOption, SizeOf(SocketOption));
{$ELSE}
  {$IFDEF DELPHI_POSIX}
    SetSockOpt(Result, SOL_SOCKET, SO_BROADCAST, SocketOption, SizeOf(SocketOption));
  {$ELSE}
    SetSockOpt(Result, SOL_SOCKET, SO_BROADCAST, @SocketOption, SizeOf(SocketOption));
  {$ENDIF}
{$ENDIF}
  end;
end;

procedure TNetCom.DestroySocket(var Handle: TSocket);
begin
  if Handle <> InvalidSocket then
  begin
{$IFDEF DELPHI_POSIX}
    __close(Handle);
{$ELSE}
    CloseSocket(Handle);
{$ENDIF}
    Handle := InvalidSocket;
  end;
end;

function TNetCom.BindSocket(const Handle: TSocket; const LocalPort: Integer): Boolean;
var
  TempAddr: TSockAddr;
begin
  FillChar(TempAddr, SizeOf(TSockAddr), 0);

  TempAddr.sin_port := LocalPort;
  TempAddr.sin_family := AF_INET;

{$IFDEF FPC}
  Result := fpBind(Handle, @TempAddr, SizeOf(TSockAddr)) = 0;
{$ELSE}
  {$IFDEF DELPHI_POSIX}
    Result := Bind(Handle, sockaddr(TempAddr), SizeOf(TSockAddr)) = 0;
  {$ELSE}
    Result := Bind(Handle, TempAddr, SizeOf(TSockAddr)) = 0;
  {$ENDIF}
{$ENDIF}
end;

function TNetCom.GetSocketPort(const Handle: TSocket): Integer;
var
  TempAddr: TSockAddr;
  SocketOption: {$IFDEF DELPHI_POSIX}Cardinal{$ELSE}LongWord{$ENDIF};
begin
  FillChar(TempAddr, SizeOf(TSockAddr), 0);
  SocketOption := SizeOf(TempAddr);

{$IFDEF FPC}
  fpGetSockName(Handle, @TempAddr, @SocketOption);
{$ELSE}
  {$IFDEF DELPHI_POSIX}
    GetSockName(Handle, sockaddr(TempAddr), SocketOption);
  {$ELSE}
    GetSockName(Handle, TempAddr, Integer(SocketOption));
  {$ENDIF}
{$ENDIF}

  Result := TempAddr.sin_port;
end;

function TNetCom.SetSocketToNonBlock(const Handle: TSocket): Boolean;
{$IFNDEF DELPHI_POSIX}
var
  SocketOption: LongWord;
{$ENDIF}
begin
{$IFNDEF DELPHI_POSIX}
  SocketOption := Cardinal(True);
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$IFDEF FPC}
    Result := ioctlsocket(Handle, FIONBIO, @SocketOption) = 0;
  {$ELSE}
    Result := ioctlsocket(Handle, FIONBIO, Integer(SocketOption)) = 0;
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF UNIX}
    Result := fpioctl(Handle, FIONBIO, @SocketOption) = 0;
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI_POSIX}
  Result := fcntl(Handle, F_SETFL, O_NONBLOCK) <> -1;
{$ENDIF}
end;

function TNetCom.SocketSendData(const Handle: TSocket; const Data: Pointer; const DataSize: Integer;
  const DestHost: StdString; const DestPort: Integer): Boolean;
var
  TempAddr: TSockAddr;
  Res: Integer;
begin
  FillChar(TempAddr, SizeOf(TSockAddr), 0);

  TempAddr.sin_addr.S_addr := HostToInt(DestHost);
  if Integer(TempAddr.sin_addr.S_addr) = 0 then
    Exit(False);

  TempAddr.sin_family := AF_INET;
  TempAddr.sin_port := DestPort;

{$IFDEF FPC}
  Res := fpSendTo(Handle, Data, DataSize, 0, @TempAddr, SizeOf(TSockAddr));
{$ELSE}
  {$IFDEF DELPHI_POSIX}
    Res := SendTo(Handle, Data^, DataSize, 0, sockaddr(TempAddr), SizeOf(TSockAddr));
  {$ELSE}
    Res := SendTo(Handle, Data^, DataSize, 0, TempAddr, SizeOf(TSockAddr));
  {$ENDIF}
{$ENDIF}

  Result := (Res > 0) and (Res = DataSize);
end;

function TNetCom.SocketReceiveData(const Handle: TSocket; const Data: Pointer; const MaxDataSize: Integer;
  out SrcHost: StdString; out SrcPort: Integer): Integer;
var
  SocketOption: {$IFDEF DELPHI_POSIX}Cardinal{$ELSE}LongWord{$ENDIF};
  TempAddr: TSockAddr;
begin
  SocketOption := SizeOf(TSockAddr);
  FillChar(TempAddr, SizeOf(TSockAddr), 0);

{$IFDEF FPC}
  Result := fpRecvFrom(Handle, Data, MaxDataSize, 0, @TempAddr, @SocketOption);
{$ELSE}
  {$IFDEF DELPHI_POSIX}
    Result := RecvFrom(Handle, Data^, MaxDataSize, 0, sockaddr(TempAddr), SocketOption);
  {$ELSE}
    Result := RecvFrom(Handle, Data^, MaxDataSize, 0, TempAddr, Integer(SocketOption));
  {$ENDIF}
{$ENDIF}

  if (Result = CodeSocketGeneralError) or (Result = CodeSocketWouldBlock) or (Result <= 0) then
    Exit(0);

  SrcPort := TempAddr.sin_port;
  SrcHost := IntToHost(TempAddr.sin_addr.S_addr);
end;

function TNetCom.IntToHost(const Value: LongWord): StdString;
begin
  Result := IntToStr(PByte(@Value)^) + '.' + IntToStr(PByte(PtrUInt(@Value) + 1)^) + '.' +
    IntToStr(PByte(PtrUInt(@Value) + 2)^) + '.' + IntToStr(PByte(PtrUInt(@Value) + 3)^);
end;

function TNetCom.IP4AddrToInt(const Text: StdString): LongWord;
var
  I, DotAt, NextStartAt, Value: Integer;
  NumText: StdString;
begin
  Result := 0;
  NextStartAt := 1;

  for I := 0 to 3 do
  begin
    if I < 3 then
    begin
    {$IFDEF FPC}
      DotAt := PosEx('.', Text, NextStartAt);
    {$ELSE}
      DotAt := Pos('.', Text, NextStartAt);
    {$ENDIF}
      if DotAt = 0 then
        Exit;

      NumText := Copy(Text, NextStartAt, DotAt - NextStartAt);
      NextStartAt := DotAt + 1;
    end
    else
      NumText := Copy(Text, NextStartAt, (Length(Text) - NextStartAt) + 1);

    Value := StrToIntDef(NumText, -1);
    if (Value < 0) or (Value > 255) then
      Exit;

    PByte(PtrUInt(@Result) + Cardinal(I))^ := Value;
  end;
end;

function TNetCom.HostToInt(const Host: StdString): LongWord;
{$IFDEF MSWINDOWS}
var
  HostEnt: PHostEnt;
{$ENDIF}
begin
  Result := IP4AddrToInt(Host);

{$IFDEF MSWINDOWS}
  if Result = 0 then
  begin
    StrPCopy(@FStringBuf, Host);

    HostEnt := GetHostByName(FStringBuf);
    if HostEnt = nil then
      Exit;

    Result := PLongWord(HostEnt.h_addr_list^)^;
  end;
{$ENDIF}
end;

function TNetCom.ResolveHost(const Host: StdString): StdString;
var
  Address: LongWord;
begin
  if FSessions <= 0 then
    Exit(UnknownIP);

  Address := HostToInt(Host);
  Result := IntToHost(Address);
end;

function TNetCom.ResolveIP(const IPAddress: StdString): StdString;
{$IFDEF MSWINDOWS}
var
  HostEnt: PHostEnt;
  Address: LongWord;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if FSessions <= 0 then
    Exit(UnknownIP);

  Address := HostToInt(IPAddress);
  HostEnt := GetHostByAddr(@Address, 4, AF_INET);

  if HostEnt <> nil then
    Result := StdString(HostEnt.h_name)
  else
    Result := UnknownIP;
{$ELSE}
  Result := UnknownIP;
{$ENDIF}
end;

function TNetCom.InitializeSocket: Boolean;
begin
  FSocket := CreateSocket(FBroadcast);
  if FSocket = InvalidSocket then
    Exit(False);

  if not BindSocket(FSocket, FLocalPort) then
  begin
    DestroySocket(FSocket);
    Exit(False);
  end;

  FLocalPort := GetSocketPort(FSocket);

  if not SetSocketToNonBlock(FSocket) then
  begin
    DestroySocket(FSocket);
    Exit(False);
  end;

  Result := True;
end;

procedure TNetCom.FinalizeSocket;
begin
  DestroySocket(FSocket);
end;

procedure TNetCom.ResetStatistics;
begin
  FBytesReceived := 0;
  FBytesSent := 0;
  FSentPackets := 0;
  FReceivedPackets := 0;
  FBytesPerSec := 0;
  FBytesTransferred := 0;
end;

function TNetCom.Initialize: Boolean;
begin
  if FSessions <= 0 then
    Exit(False);

  if FInitialized then
    Exit(False);

  if not InitializeSocket then
    Exit(False);

  ResetStatistics;

  FInitialized := True;
  FLastTickCount := GetSystemTickCount;

  Result := True;
end;

procedure TNetCom.Finalize;
begin
  if FInitialized then
  begin
    FinalizeSocket;
    FInitialized := False;
  end;
end;

function TNetCom.Send(const Host: StdString; const Port: Integer; const Data: Pointer; const Size: Integer): Boolean;
begin
  if (not FInitialized) or (Length(Host) <= 0) or (Data = nil) or (Size <= 0) then
    Exit(False);

  Result := SocketSendData(FSocket, Data, Size, Host, Port);
  if Result then
  begin
    Inc(FSentPackets);
    Inc(FBytesSent, Size);
    Inc(FBytesTransferred, Size);
  end;
end;

procedure TNetCom.SocketReceive;
var
  ReceivedBytes, FromPort: Integer;
  FromHost: StdString;
begin
  ReceivedBytes := SocketReceiveData(FSocket, @FPacketMessage[0], MaximumPacketSize, FromHost, FromPort);
  if ReceivedBytes < 1 then
    Exit;

  Inc(FReceivedPackets);
  Inc(FBytesReceived, ReceivedBytes);
  Inc(FBytesTransferred, ReceivedBytes);

  if Assigned(FOnReceive) then
    FOnReceive(Self, FromHost, FromPort, @FPacketMessage, ReceivedBytes);
end;

procedure TNetCom.Update;
var
  NowTickCount, ElapsedTime: LongWord;
begin
  NowTickCount := GetSystemTickCount;
  ElapsedTime := Abs(NowTickCount - FLastTickCount);

  if ElapsedTime > Cardinal(FUpdateRefreshTime) then
  begin
    FLastTickCount := NowTickCount;

    FBytesPerSec := (Int64(FBytesTransferred) * 1000) div ElapsedTime;
    FBytesTransferred := 0;
  end;

  if FInitialized then
    SocketReceive;
end;

initialization
  TNetCom.FSessions := 0;

end.
