{ *************************************************************************** }
{                                                                             }
{ Delphi and Kylix Cross-Platform Visual Component Library                    }
{ Internet Application Runtime                                                }
{                                                                             }
{ Copyright (C) 2000, 2001 Borland Software Corporation                       }
{                                                                             }
{ Licensees holding a valid Borland No-Nonsense License for this Software may }
{ use this file in accordance with such license, which appears in the file    }
{ license.txt that came with this Software.                                   }
{                                                                             }
{ *************************************************************************** }

unit Better_Sockets2;
//{$IFNDEF VER150}{$INLINE AUTO}{$ENDIF}
interface

{$IFDEF MSWINDOWS}
uses
  systemx, system.rtlconsts,//for inline optimization
  stringx, debug, typex, Windows, WinSock, SysUtils, Classes, orderlyinit, managedthread, generics.collections.fixed, threadmanager, tickcount, ringbuffer;

type
  ESocketCritical = class(Exception);
  TSocketDomain = (pfUnspec, pfUnix, pfInet,
                pfImpLink, pfPup, pfChaos,
                pfIpx, pfNs, pfIso,
                pfOsi, pfEcma, pfDataKit,
                pfCcitt, pfSna, pfDecNet,
                pfDli, pfLat, pfHylink,
                pfAppleTalk, pfVoiceView, pfFireFox,
                pfUnknown1, pfBan, pfMax);
{$ENDIF}
{$IFDEF LINUX}
uses Libc, SysUtils, Classes;

type
  TSocketDomain = (pfUnspec, pfLocal, pfUnix,
                pfFile, pfInet, pfAx25,
                pfIpx, pfAppleTalk, pfNetRom,
                pfBridge, pfAtmPvc, pfX25,
                pfInet6, pfRose, pfDecNet,
                pfNetbeui, pfSecurity, pfKey,
                pfNetLink, pfRoute, pfPacket,
                pfAsh, pfEcoNet, pfAtmSvc,
                pfSna, pfIrda, pfMax);
{$ENDIF}

Const
  CRLF = #13#10;

type

{ TBetterBaseSocket }



  TSocketProtocol = Word;
  TServerSocketBlockMode = (bmBlocking, bmNonBlocking, bmThreadBlocking);
  TSocketBlockMode = bmBlocking..bmNonBlocking;
  TSocketType = (stStream, stDgram, stRaw, stRdm, stSeqPacket);
  TSocketNotifyEvent = procedure (Sender: TObject) of object;
  TSocketDataEvent = procedure (Sender: TObject; Buf: PAnsiChar; var DataLen: Integer) of object;
  TSocketErrorEvent = procedure (Sender: TObject; SocketError: Integer) of object;
  ESocketError = class(Exception);

  TBetterBaseSocket = class(TComponent)
  private
    FActive: Boolean;
    FBlockMode: TSocketBlockMode;
    FBytesReceived: Cardinal;
    FBytesSent: Cardinal;
    FDomain: TSocketDomain;
    FProtocol: TSocketProtocol;
    FSocket: TSocket;
    FSockType: TSocketType;
    FOnCreateHandle: TSocketNotifyEvent;
    FOnDestroyHandle: TSocketNotifyEvent;
    FOnError: TSocketErrorEvent;
    FOnReceive: TSocketDataEvent;
    FOnSend: TSocketDataEvent;
    FKill: boolean;
    FtimeOfLastData: ticker;


    procedure SetActive(Value: Boolean);
    procedure SetBlockMode(Value: TSocketBlockMode);
    procedure SetDomain(Value: TSocketDomain);
    procedure SetProtocol(Value: TSocketProtocol);
    procedure SetSockType(Value: TSocketType);

  protected
    procedure DoCreateHandle; dynamic;
    procedure DoDestroyHandle; dynamic;
    procedure DoHandleError; dynamic;
    procedure DoReceive(Buf: PAnsiChar; var DataLen: Integer); virtual;
    procedure DoSend(Buf: PAnsiChar; var DataLen: Integer); virtual;
    function ErrorCheck(rc: Integer): Integer; virtual;
    procedure Loaded; override;
    procedure SetBytesReceived(Value: Cardinal);
    procedure SetBytesSent(Value: Cardinal);

  public
    procedure CheckConnected;{$IFDEF IL}inline;{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; virtual;
    procedure Close; virtual;
    function MapDomain(sd: TSocketDomain): Integer;
    function MapSockType(st: TSocketType): Integer;
    function PeekBuf(var Buf; BufSize: Integer): Integer;{$IFDEF IL}inline;{$ENDIF}
    function ReceiveBuf(var Buf; BufSize: Integer; Flags: Integer = 0): Integer;{$IFDEF IL}inline;{$ENDIF}
    function Receiveln(const eol: ansistring = CRLF): ansistring;
    function Select(ReadReady, WriteReady, ExceptFlag: PBoolean; TimeOut: Integer = 0): Boolean;{$IFDEF IL}inline;{$ENDIF}
    function SendBuf(var Buf; BufSize: Integer; Flags: Integer = 0): Integer;{$IFDEF IL}inline;{$ENDIF}
    function Sendln(s: ansistring; const eol: ansistring = CRLF): Integer;
    function SendStream(AStream: TStream): Integer;
    function WaitForData(TimeOut: Integer = 0): Boolean;{$IFDEF IL}inline;{$ENDIF}
    procedure SendEmpty;{$IFDEF IL}inline;{$ENDIF}

    property Active: Boolean read FActive write SetActive default False;
    property BlockMode: TSocketBlockMode read FBlockMode write SetBlockMode default bmBlocking;
    property BytesReceived: Cardinal read FBytesReceived;
    property BytesSent: Cardinal read FBytesSent;
    property Domain: TSocketDomain read FDomain write SetDomain default pfUnspec;
    property Handle: TSocket read FSocket;
    property Protocol: TSocketProtocol read FProtocol write SetProtocol;
    property SockType: TSocketType read FSockType write SetSockType default stStream;
    property OnCreateHandle: TSocketNotifyEvent read FOnCreateHandle write FOnCreateHandle;
    property OnDestroyHandle: TSocketNotifyEvent read FOnDestroyHandle write FOnDestroyHandle;
    property OnError: TSocketErrorEvent read FOnError write FOnError;
    property OnReceive: TSocketDataEvent read FOnReceive write FOnReceive;
    property OnSend: TSocketDataEvent read FOnSend write FOnSend;
    procedure Kill;
    property KillMe: boolean read FKill;
  end;

  TSocketRingBuffer = class(TRingBuffer)
  private
    FSocket: TBetterBaseSocket;
    procedure eFSocket(const Value: TBetterBaseSocket);
  public
    property Socket: TBetterBaseSocket read FSocket write eFSocket;
    function IsConnected: boolean;
  end;

{ TBetterIPSocket }

  TIPHeader = packed record
    iph_verlen: byte;           // Version and length
    iph_tos: byte;              // Type of service
    iph_length: word;           // Total datagram length
    iph_id: word;               // Identification
    iph_offset: word;           // Flags, fragment offset
    iph_ttl: byte;              // Time to live
    iph_protocol: byte;         // Protocol
    iph_xsum: word;             // Header checksum
    iph_src: longword;          // Source address
    iph_dest: longword;         // Destination address
  end;

  TSocketHost = type ansistring;
  TSocketPort = type ansistring;

{$IFDEF LINUX}
(*$HPPEMIT '#include <sys/socket.h>'*)
{$ENDIF}

  TBetterIPSocket = class(TBetterBaseSocket)
  private
    FLocalHost: TSocketHost;
    FLocalPort: TSocketPort;
    FRemoteHost: TSocketHost;
    FRemotePort: TSocketPort;

    procedure SetLocalHost(Value: TSocketHost);
    procedure SetLocalPort(Value: TSocketPort);
    procedure SetRemoteHost(Value: TSocketHost);
    procedure SetRemotePort(Value: TSocketPort);

  protected
    function Bind: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    function GetSocketAddr(h: TSocketHost; p: TSocketPort): TSockAddr;
    function LookupHostName(const ipaddr: ansistring): TSocketHost;
    function LookupHostAddr(const hn: ansistring): TSocketHost;
    function LookupPort(const sn: ansistring; pn: PAnsiChar = nil): word;
    function LookupProtocol(const pn: ansistring): TSocketProtocol;
    function LocalDomainName: ansistring;
    function LocalHostName: TSocketHost;
    function LocalHostAddr: TSocketHost;

    function ReceiveFrom(var buf; bufsize: Integer; ToAddr: TSockAddr; var len: Integer; flags: Integer = 0): Integer;
    function SendTo(var buf; bufsize: Integer; ToAddr: TSockAddr; flags: Integer = 0): Integer;

    property LocalHost: TSocketHost read FLocalHost write SetLocalHost;
    property LocalPort: TSocketPort read FLocalPort write SetLocalPort;
    property RemoteHost: TSocketHost read FRemoteHost write SetRemoteHost;
    property RemotePort: TSocketPort read FRemotePort write SetRemotePort;
    property Domain default pfInet;
  end;

{ TBetterCustomIPClient }

  TBetterClientSocketThread = class;

  TBetterCustomIPClient = class(TBetterIPSocket)
  private
    FConnected: Boolean;
    FOnConnect: TSocketNotifyEvent;
    FOnDisconnect: TSocketNotifyEvent;
    FNoDelay: boolean;
    procedure SetNoDelay(const Value: boolean);
  protected
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Open; override;
    procedure Close; override;

    function Connect: Boolean;
    procedure Disconnect;
    function GetThreadObject: TBetterClientSocketThread;

    property Connected: Boolean read FConnected;
    property OnConnect: TSocketNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TSocketNotifyEvent read FOnDisconnect write FOnDisConnect;
    procedure Kill;
    property NoDelay: boolean read FNoDelay write SetNoDelay;
  end;

{ TBetterRawSocket }

  TBetterRawSocket = class(TBetterIPSocket)
  public
    constructor Create(AOwner: TComponent); override;
    property SockType default stRaw;

  end;

{ TBetterUDPSocket }

  TBetterUDPSocket = class(TBetterCustomIPClient)
  public
    constructor Create(AOwner: TComponent); override;

  published
    property Active;
    property BlockMode;
    property LocalHost;
    property LocalPort;
    property RemoteHost;
    property RemotePort;
    property OnCreateHandle;
    property OnDestroyHandle;
    property OnConnect;
    property OnDisconnect;
    property OnReceive;
    property OnSend;
    property OnError;
  end;

{ TBetterTCPClient }

  TBetterTCPClient = class(TBetterCustomIPClient)
  published
    property Active;
    property BlockMode;
    property Connected;
    property RemoteHost;
    property RemotePort;
    property OnCreateHandle;
    property OnDestroyHandle;
    property OnConnect;
    property OnDisconnect;
    property OnReceive;
    property OnSend;
    property OnError;
  end;

{ TBetterClientSocketThread }

  TBetterServerSocketThread = class;

  TBetterClientSocketThread = class(TMAnagedThread)
  private
    FClientSocket: TBetterCustomIPClient;
    FServerSocketThread: TBetterServerSocketThread;
    procedure SetServerSocketThraed(const Value: TBetterServerSocketThread);
    procedure SetCLientSocket(const Value: TBetterCustomIPClient);

  protected
    procedure SyncProc; virtual;

  public
    destructor Destroy; override;
    procedure DoExecute; override;
    procedure KillClient;
    procedure ExecuteSyncProc;
    procedure BuildMenu; override;
    procedure MenuAction(idx: NativeInt); override;
    property ClientSocket: TBetterCustomIPClient read FClientSocket write SetCLientSocket;
    property ServerSocketThread: TBetterServerSocketThread read FServerSocketThread write SetServerSocketThraed;


  end;

{ TBetterServerSocketThread }

  TBetterCustomTCPServer = class;
  TGetThreadEvent = procedure (Sender: TObject; var ClientSocketThread: TBetterClientSocketThread) of object;

  TBetterServerSocketThread = class(TManagedThread)
  private
    FPoolIndex: Integer;
    FThreads: TList<TBetterCLientSocketThread>;
    FServerSocket: TBetterCustomTCPServer;
    FThreadCacheSize: Integer;
    FOnGetThread: TGetThreadEvent;
    FReady: boolean;
    FRing: boolean;
    procedure SetThreadCacheSize(Value: Integer);

  protected
    function CreateThread(clientsocket: TBetterCustomIPClient): TBetterClientSocketThread; virtual;

  public
    constructor Create(Owner: TObject; Manager: TThreadManager; pool: TThreadpoolBase); override;

    procedure OnFinish;override;
    destructor Destroy; override;
    procedure WaitForChildThreads;
    procedure DoExecute; override;
    procedure KillAllClients;
    property ServerSocket: TBetterCustomTCPServer read FServerSocket write FServerSocket;
    property OnGetThread: TGetThreadEvent read FOnGetThread write FOnGetThread;
    property Ready: boolean read FReady write FReady;
    property Ring: boolean read FRing write FRing;
    procedure RegisterThread(t: TBetterClientSocketThread);
    procedure UnRegisterThread(t: TBetterClientSocketThread);
  end;

{ TBetterCustomTCPServer }

  TSocketAcceptEvent = procedure (Sender: TObject; ClientSocket: TBetterCustomIPClient) of object;

  TBetterCustomTCPServer = class(TBetterIPSocket)
  private
    FServerBlockMode: TServerSocketBlockMode;
    FListening: Boolean;
    FServerSocketThread: TBetterServerSocketThread;
{$IFDEF LINUX}
    FThreadLock: TRTLCriticalSection;
{$ENDIF}
    FOnAccept: TSocketAcceptEvent;
    FOnGetThread: TGetThreadEvent;
    FOnListening: TNotifyEvent;

    procedure GetThread(Sender: TObject; var ClientSocketThread: TBetterClientSocketThread);
    function GeTBetterServerSocketThread: TBetterServerSocketThread;
    procedure SeTBetterServerSocketThread(Value: TBetterServerSocketThread);
    procedure SetServerBlockMode(Value: TServerSocketBlockMode);

  protected
    procedure DoAccept(ClientSocket: TBetterCustomIPClient); virtual;
    function Listen(backlog: Integer = SOMAXCONN): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;
    function Accept: Boolean; overload;
    function Accept(var ClientSocket: TBetterCustomIPClient): Boolean; overload;
    function Answer(var ClientSocket: TBetterCustomIPClient): Boolean;
    procedure Process(var ClientSocket: TBetterCustomIPCLient);

    function WaitForConnection: Boolean;

    property BlockMode: TServerSocketBlockMode read FServerBlockMode write SetServerBlockMode default bmThreadBlocking;
    property Listening: Boolean read FListening;
    property ServerSocketThread: TBetterServerSocketThread read GeTBetterServerSocketThread write SeTBetterServerSocketThread;
    property OnAccept: TSocketAcceptEvent read FOnAccept write FOnAccept;
    property OnGetThread: TGetThreadEvent read FOnGetThread write FOnGetThread;
    property OnListening: TNotifyEvent read FOnListening write FOnListening;
  end;

  TBetterTCPServer = class(TBetterCustomTCPServer)
  published
    property Active;
    property BlockMode;
    property LocalHost;
    property LocalPort;
    property OnAccept;
    property OnGetThread;
    property OnListening;
    property OnCreateHandle;
    property OnDestroyHandle;
  end;

procedure Socket_GuaranteeRead(s: TBetterCustomIPClient; p: pbyte; iLength: ni);{$IFDEF IL}inline;{$ENDIF}
function Socket_Read(s: TBetterCustomIPClient; p: pbyte; iLength: ni; iWaitTime: ni): ni;{$IFDEF IL}inline;{$ENDIF}
procedure Socket_GuaranteeWrite(const s: TBetterCustomIPClient; const p: pbyte; const iLength: ni);{$IFDEF IL}inline;{$ENDIF}

procedure Register;

implementation

threadvar
  ThreadObject: TBetterClientSocketThread;

{$IFDEF MSWINDOWS}
const
  Xlat_Domain: array[TSocketDomain] of Integer
                = (PF_UNSPEC, PF_UNIX, PF_INET,
                   PF_IMPLINK, PF_PUP, PF_CHAOS,
                   PF_IPX, PF_NS, PF_ISO,
                   PF_OSI, PF_ECMA, PF_DATAKIT,
                   PF_CCITT, PF_SNA, PF_DECnet,
                   PF_DLI, PF_LAT, PF_HYLINK,
                   PF_APPLETALK, PF_VOICEVIEW, PF_FIREFOX,
                   PF_UNKNOWN1, PF_BAN, PF_MAX);
type
  __socket_type = Integer;
{$ENDIF}

{$IFDEF LINUX}
const
  Xlat_Domain: array[TSocketDomain] of Integer
  		= (PF_UNSPEC, PF_LOCAL, PF_UNIX,
                   PF_FILE, PF_INET, PF_AX25,
                   PF_IPX, PF_APPLETALK, PF_NETROM,
                   PF_BRIDGE, PF_ATMPVC, PF_X25,
                   PF_INET6, PF_ROSE, PF_DECnet,
                   PF_NETBEUI, PF_SECURITY, PF_KEY,
                   PF_NETLINK, PF_ROUTE, PF_PACKET,
                   PF_ASH, PF_ECONET, PF_ATMSVC,
                   PF_SNA, PF_IRDA, PF_MAX);
{$ENDIF}

const
  Xlat_SocketType: array[TSocketType] of __socket_type
                = (SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_RDM, SOCK_SEQPACKET);

{ TBetterBaseSocket }

constructor TBetterBaseSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive  := False;
  FBlockMode := bmBlocking;
  FBytesReceived := 0;
  FBytesSent := 0;
  FDomain := pfUnspec;
  FProtocol := IPPROTO_IP;
  FSocket := INVALID_SOCKET;
  FSockType := stStream;
  FOnCreateHandle := nil;
  FOnDestroyHandle := nil;
  FOnError := nil;
  FOnReceive := nil;
  FOnSend := nil;
  FtimeOfLastDAta := GEtTicker;
{$IFDEF MSWINDOWS}
  RPR;
{$ENDIF}
{$IFDEF LINUX}
  RPR;
{$ENDIF}
end;

destructor TBetterBaseSocket.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TBetterBaseSocket.Open;
{$IFDEF MSWINDOWS}
var
  NonBlock: Integer;
{$ENDIF}
begin
  if not FActive then
  begin
    FSocket := ErrorCheck(socket(Integer(Xlat_Domain[FDomain]), Integer(Xlat_SocketType[FSockType]), FProtocol));
    FActive := FSocket <> INVALID_SOCKET;
    if FActive then
    begin
      if FBlockMode = bmNonBlocking then
      begin
  {$IFDEF MSWINDOWS}
        NonBlock := 1;
        ErrorCheck(ioctlsocket(FSocket, FIONBIO, NonBlock));
  {$ENDIF}
  {$IFDEF LINUX}
        ErrorCheck(fcntl(FSocket, F_SETFL, O_NONBLOCK));
  {$ENDIF}
      end;
      FBytesReceived := 0;
      FBytesSent := 0;
      DoCreateHandle;
    end;
  end;
end;

procedure TBetterBaseSocket.CheckConnected;
begin
  if FKill then
    raise EAbort.create('Socket aborted');
end;

procedure TBetterBaseSocket.Close;
begin
  if FActive then
  begin
{$IFDEF MSWINDOWS}
    ErrorCheck(closesocket(FSocket));
{$ENDIF}
{$IFDEF LINUX}
    ErrorCheck(Libc.__close(FSocket));
{$ENDIF}
    FSocket := INVALID_SOCKET;
    FActive := False;
    DoDestroyHandle;
  end;
end;

function TBetterBaseSocket.MapDomain(sd: TSocketDomain): Integer;
begin
  Result := Integer(Xlat_Domain[sd]);
end;

function TBetterBaseSocket.MapSockType(st: TSocketType): Integer;
begin
  Result := Integer(Xlat_SocketType[st]);
end;

function TBetterBaseSocket.PeekBuf(var Buf; BufSize: Integer): Integer;
begin
  CheckConnected;
  Result := ErrorCheck(recv(FSocket, buf, bufsize, MSG_PEEK));
end;

function TBetterBaseSocket.ReceiveBuf(var Buf; BufSize: Integer; Flags: Integer): Integer;
begin
  CheckConnected;
  Result := ErrorCheck(recv(FSocket, Buf, BufSize, Flags));
  if Result <> SOCKET_ERROR then
    DoReceive(PAnsiChar(@Buf), Result);
end;

function TBetterBaseSocket.Receiveln(const eol: ansistring): ansistring;
var
  len: Integer;
  buf: array[0..511] of AnsiChar;
  eolptr: PAnsiChar;
begin
  Result := '';
  eolptr := nil;
  repeat
    len := PeekBuf(buf, sizeof(buf) - 1);
    if len > 0 then
    begin
      buf[len] := #0;
      eolptr := strpos(buf, PAnsiChar(eol));
      if eolptr <> nil then
        len := eolptr - buf + length(eol);
      ReceiveBuf(buf, len);
      if eolptr <> nil then
        len := len - length(eol);
      buf[len] := #0;
      Result := Result + buf;
    end;
  until (len < 1) or (eolptr <> nil);
end;

function TBetterBaseSocket.Select(ReadReady, WriteReady, ExceptFlag: PBoolean; TimeOut: Integer): Boolean;
var
  ReadFds: TFDset;
  ReadFdsptr: PFDset;
  WriteFds: TFDset;
  WriteFdsptr: PFDset;
  ExceptFds: TFDset;
  ExceptFdsptr: PFDset;
  tv: timeval;
  Timeptr: PTimeval;
begin
  Result := False;
  if Active then
  begin
    if Assigned(ReadReady) then
    begin
      ReadFdsptr := @ReadFds;
      FD_ZERO(ReadFds);
      FD_SET(FSocket, ReadFds);
    end
    else
      ReadFdsptr := nil;
    if Assigned(WriteReady) then
    begin
      WriteFdsptr := @WriteFds;
      FD_ZERO(WriteFds);
      FD_SET(FSocket, WriteFds);
    end
    else
      WriteFdsptr := nil;
    if Assigned(ExceptFlag) then
    begin
      ExceptFdsptr := @ExceptFds;
      FD_ZERO(ExceptFds);
      FD_SET(FSocket, ExceptFds);
    end
    else
      ExceptFdsptr := nil;
    if TimeOut >= 0 then
    begin
      tv.tv_sec := TimeOut div 1000;
      tv.tv_usec :=  1000 * (TimeOut mod 1000);
      Timeptr := @tv;
    end
    else
      Timeptr := nil;
    Try
{$IFDEF MSWINDOWS}
      Result := ErrorCheck(WinSock.select(FSocket + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr)) > 0;
{$ENDIF}
{$IFDEF LINUX}
      Result := ErrorCheck(Libc.select(FSocket + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr)) > 0;
{$ENDIF}
    except
      Result := False;
    end;
    if Assigned(ReadReady) then
      ReadReady^ := FD_ISSET(FSocket, ReadFds);
    if Assigned(WriteReady) then
      WriteReady^ := FD_ISSET(FSocket, WriteFds);
    if Assigned(ExceptFlag) then
      ExceptFlag^ := FD_ISSET(FSocket, ExceptFds);
  end;
end;

function TBetterBaseSocket.SendBuf(var Buf; BufSize: Integer; Flags: Integer): Integer;
begin
  CheckConnected;
  DoSend(PAnsiChar(@Buf), BufSize);
  Result := ErrorCheck(Send(FSocket, Buf, BufSize, Flags));
  if Result <> SOCKET_ERROR then
    inc(FBytesSent, Result);
end;

procedure TBetterBaseSocket.SendEmpty;
var
  c: char;
begin
  self.SendBuf(c, 0, MSG_OOB);
  FtimeOfLastData := getticker;
end;

function TBetterBaseSocket.Sendln(s: ansistring; const eol: ansistring): Integer;
begin
  s := s + eol;
  Result := SendBuf(PAnsiChar(s)^, length(s), 0);
end;

function TBetterBaseSocket.SendStream(AStream: TStream): Integer;
var
  BufLen: Integer;
  Buffer: array[0..511] of Byte;
begin
  Result := 0;
  if Assigned(AStream) then
  begin
    repeat
      BufLen := AStream.Read(Buffer, SizeOf(Buffer));
    until (BufLen = 0) or (SendBuf(Buffer, BufLen) = SOCKET_ERROR);
  end;
end;

function TBetterBaseSocket.WaitForData(TimeOut: Integer): Boolean;
var
  ReadReady, ExceptFlag: Boolean;
  c: AnsiChar;
begin
  Result := False;
  // Select also returns True when connection is broken.
  if Select(@ReadReady, nil, @ExceptFlag, TimeOut) then begin
    Result := ReadReady and not ExceptFlag;
    if result and (PeekBuf(c, sizeof(c)) <= 0) then
      raise ESocketError.Create('Connection drop detected.');

    if result then
      FTimeOfLastData := GEtTickCount;
  end else begin
    if gettimesince(FTimeOfLastDAta) > 20000 then begin
      SendEmpty;
    end;
  end;


end;

procedure TBetterBaseSocket.DoHandleError;
var
  SocketError: Integer;
begin
{$IFDEF MSWINDOWS}
  SocketError := WSAGetLastError;
{$ENDIF}
{$IFDEF LINUX}
  SocketError := errno;
{$ENDIF}
  if Assigned(FOnError) then
    OnError(Self, SocketError);
end;

procedure TBetterBaseSocket.DoCreateHandle;
begin
  if FActive and Assigned(FOnCreateHandle) then
    OnCreateHandle(self);
end;

procedure TBetterBaseSocket.DoDestroyHandle;
begin
  if Assigned(FOnDestroyHandle) then
    OnDestroyHandle(self);
end;

procedure TBetterBaseSocket.DoReceive(Buf: PAnsiChar; var DataLen: Integer);
begin
  if Assigned(FOnReceive) then
    OnReceive(Self, Buf, DataLen);
  inc(FBytesReceived, DataLen);
end;

procedure TBetterBaseSocket.DoSend(Buf: PAnsiChar; var DataLen: Integer);
begin
  if Assigned(FOnSend) then
    OnSend(Self, Buf, DataLen);
end;

function TBetterBaseSocket.ErrorCheck(rc: Integer): Integer;
begin
  Result := rc;
  if rc = SOCKET_ERROR then
    DoHandleError;
end;

procedure TBetterBaseSocket.Kill;
begin
  FKill := true;
end;

procedure TBetterBaseSocket.Loaded;
begin
  inherited Loaded;
  if FActive and not (csDesigning in ComponentState) then
  begin
    FActive := False;
    Open;
  end;
end;

procedure TBetterBaseSocket.SetBytesReceived(Value: Cardinal);
begin
  FBytesReceived := Value;
end;

procedure TBetterBaseSocket.SetBytesSent(Value: Cardinal);
begin
  FBytesSent := Value
end;

procedure TBetterBaseSocket.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    Debug.Log(self, 'Changing to '+booltostrex(value, 'Active', 'Inactive'));
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      if Value then Open
      else Close
    else FActive := Value;
  end;
end;

procedure TBetterBaseSocket.SetDomain(Value: TSocketDomain);
begin
  if Value <> FDomain then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FDomain := Value;
  end;
end;

procedure TBetterBaseSocket.SetSockType(Value: TSocketType);
begin
  if Value <> FSockType then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      Close;
    FSockType := Value;
  end;
end;

procedure TBetterBaseSocket.SetProtocol(Value: TSocketProtocol);
begin
  if Value <> FProtocol then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      Close;
    FProtocol := Value;
  end;
end;

procedure TBetterBaseSocket.SetBlockMode(Value: TSocketBlockMode);
begin
  if Value <> FBlockMode then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      Close;
    FBlockMode := Value;
  end;
end;

{ TBetterIPSocket }

constructor TBetterIPSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDomain := pfInet;
  FProtocol := IPPROTO_IP;
  FLocalHost := '';
  FLocalPort := '';
  FRemoteHost := '';
  FRemotePort := '';
end;

function TBetterIPSocket.GetSocketAddr(h: TSocketHost; p: TSocketPort): TSockAddr;
begin
  Result.sin_family := AF_INET;
  Result.sin_addr.s_addr := inet_addr(PAnsiChar(LookupHostAddr(h)));
  Result.sin_port := htons(LookupPort(p));
end;

function TBetterIPSocket.LookupHostName(const ipaddr: ansistring): TSocketHost;
var
  h: PHostEnt;
  addr: TSockAddr;
begin
  Result := '';
  addr.sin_addr.s_addr := inet_addr(PAnsiChar(ipaddr));
  if addr.sin_addr.s_addr <> INADDR_NONE then
  begin
    h := gethostbyaddr(@addr.sin_addr.s_addr, sizeof(addr), AF_INET);
    if h <> nil then
      Result := h^.h_name;
  end;
end;

function TBetterIPSocket.LookupHostAddr(const hn: ansistring): TSocketHost;
var
  h: PHostEnt;
begin
  Result := '';
  if hn <> '' then
  begin
    if hn[1] in ['0'..'9'] then
    begin
      if inet_addr(PAnsiChar(hn)) <> INADDR_NONE then
        Result := hn;
    end
    else
    begin
      h := gethostbyname(PAnsiChar(hn));
      if h <> nil then
        with h^ do
        Result := format('%d.%d.%d.%d', [ord(h_addr^[0]), ord(h_addr^[1]),
      		  ord(h_addr^[2]), ord(h_addr^[3])]);
    end;
  end
  else Result := '0.0.0.0';
end;

function TBetterIPSocket.LookupPort(const sn: ansistring; pn: PAnsiChar): word;
var
  se: PServent;
begin
  Result := 0;
  if sn <> '' then
  begin
    se := getservbyname(PAnsiChar(sn), PAnsiChar(pn));
    if se <> nil then
      Result := ntohs(se^.s_port)
    else
      Result := StrToInt(sn);
  end;
end;

function TBetterIPSocket.LookupProtocol(const pn: ansistring): TSocketProtocol;
var
  pe: PProtoent;
begin
  Result := 0;
  pe := getprotobyname(PAnsiChar(pn));
  if pe <> nil then
    Result := pe^.p_proto;
end;

function TBetterIPSocket.LocalDomainName: ansistring;
var
{$IFDEF MSWINDOWS}
  dname: PAnsiChar;
{$ENDIF}
{$IFDEF LINUX}
  dname: array[0..255] of AnsiChar;
{$ENDIF}
begin
  Result := '';
{$IFDEF MSWINDOWS}
  dname := strpos(PAnsiChar(LookupHostName(LocalHostAddr)), '.');
  if dname <> nil then
    Result := dname + 1;
{$ENDIF}
{$IFDEF LINUX}
  if ErrorCheck(getdomainname(dname, sizeof(dname))) = 0 then
    Result := dname;
{$ENDIF}
end;

function TBetterIPSocket.LocalHostName: TSocketHost;
var
  name: array[0..255] of AnsiChar;
begin
  Result := '';
  if ErrorCheck(gethostname(name, sizeof(name))) = 0 then
    Result := name;
end;

function TBetterIPSocket.LocalHostAddr: TSocketHost;
begin
  Result := LookupHostAddr(LocalHostName);
end;

function TBetterIPSocket.Bind: Boolean;
var
  addr: TSockAddr;
begin
  Result := False;
  if Active then
  begin
    addr := GetSocketAddr(FLocalHost, FLocalPort);
{$IFDEF MSWINDOWS}
    Result := ErrorCheck(WinSock.bind(FSocket, addr, sizeof(addr))) = 0;
{$ENDIF}
{$IFDEF LINUX}
    Result := ErrorCheck(Libc.bind(FSocket, addr, sizeof(addr))) = 0;
{$ENDIF}
  end;
end;

function TBetterIPSocket.ReceiveFrom(var buf; bufsize: Integer; ToAddr: TSockAddr; var len: Integer; flags: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := ErrorCheck(WinSock.recvfrom(FSocket, buf, bufsize, flags, ToAddr, len));
{$ENDIF}
{$IFDEF LINUX}
  Result := ErrorCheck(Libc.recvfrom(FSocket, buf, bufsize, flags, @ToAddr, @len));
{$ENDIF}
  if Result <> SOCKET_ERROR then
    DoReceive(PAnsiChar(@Buf), Result);
end;

function TBetterIPSocket.SendTo(var buf; bufsize: Integer; ToAddr: TSockAddr; flags: Integer): Integer;
begin
  DoSend(PAnsiChar(@Buf), BufSize);
{$IFDEF MSWINDOWS}
  Result := ErrorCheck(WinSock.sendto(FSocket, buf, bufsize, flags, ToAddr, sizeof(ToAddr)));
{$ENDIF}
{$IFDEF LINUX}
  Result := ErrorCheck(Libc.sendto(FSocket, buf, bufsize, flags, ToAddr, sizeof(ToAddr)));
{$ENDIF}
  if Result <> SOCKET_ERROR then
    SetBytesSent(BytesSent + Cardinal(Result));
end;

procedure TBetterIPSocket.SetLocalHost(Value : TSocketHost);
begin
  if Value <> FLocalHost then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FLocalHost := Value;
  end;
end;

procedure TBetterIPSocket.SetLocalPort(Value: TSocketPort);
begin
  if Value <> FLocalPort then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FLocalPort := Value;
  end;
end;

procedure TBetterIPSocket.SetRemoteHost(Value : TSocketHost);
begin
  if Value <> FRemoteHost then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FRemoteHost := Value;
  end;
end;

procedure TBetterIPSocket.SetRemotePort(Value: TSocketPort);
begin
  if Value <> FRemotePort then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FRemotePort := Value;
  end;
end;

{ TBetterCustomIPClient }

constructor TBetterCustomIPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnected := False;
  FOnConnect := nil;
  FOnDisconnect := nil;
end;

procedure TBetterCustomIPClient.Open;
var
  addr: TSockAddr;
begin
  inherited Open;
  if Active and not FConnected then
  begin
    addr := GetSocketAddr(FRemoteHost, FRemotePort);
{$IFDEF MSWINDOWS}
    FConnected := ErrorCheck(WinSock.connect(FSocket, addr, sizeof(addr))) = 0;
{$ENDIF}
{$IFDEF LINUX}
    FConnected := ErrorCheck(Libc.connect(FSocket, addr, sizeof(addr))) = 0;
    if not FConnected then   // Workaround on bug in Red Hat 6.2
      Close;
{$ENDIF}
    if FConnected then
      DoConnect;
  end;
end;

procedure TBetterCustomIPClient.SetNoDelay(const Value: boolean);
begin
  if value <> FNoDelay then begin
    setsockopt(self.Handle, IPPROTO_TCP, TCP_NODELAY, pansichar(@value), sizeof(value));
  end;
  FNoDelay := Value;

end;

procedure TBetterCustomIPClient.Close;
begin
  if FConnected then
  begin
{$IFDEF MSWINDOWS}
    ErrorCheck(shutdown(FSocket, SD_BOTH));
{$ENDIF}
{$IFDEF LINUX}
    ErrorCheck(shutdown(FSocket, SHUT_RDWR));
{$ENDIF}
    FConnected := False;
    DoDisconnect;
  end;
  inherited Close;
end;

function TBetterCustomIPClient.Connect: Boolean;
begin
  Open;
  Result := FConnected;
end;

procedure TBetterCustomIPClient.Disconnect;
begin
  Close;
end;

function TBetterCustomIPClient.GetThreadObject: TBetterClientSocketThread;
begin
  Result := ThreadObject;
end;

procedure TBetterCustomIPClient.Kill;
begin
  FKill := true;
end;

procedure TBetterCustomIPClient.DoConnect;
begin
  if Assigned(FOnConnect) then
    OnConnect(self);
end;

procedure TBetterCustomIPClient.DoDisconnect;
begin
  if Assigned(FOnDisconnect) then
    OnDisconnect(self);
end;

{ TBetterRawSocket }

constructor TBetterRawSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSockType := stRaw;
  FProtocol := IPPROTO_RAW;
end;

{ TBetterUDPSocket }

constructor TBetterUDPSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SockType := stDgram;
  Protocol := IPPROTO_UDP;
end;

{ TBetterClientSocketThread }


procedure TBetterClientSocketThread.BuildMenu;
begin
  inherited;
  AddMenuItem('Kill');
end;

destructor TBetterClientSocketThread.Destroy;
begin
  FClientSocket.free;
  inherited Destroy;
end;

procedure TBetterClientSocketThread.DoExecute;
var
  bc : TBetterCustomIPCLient;
begin
  ThreadObject := Self;
//  while Started do begin
    try
      if Assigned(FServerSocketThread) and not FServerSocketThread.Terminated and
         Assigned(FServerSocketThread.ServerSocket) then
      begin
          try
            try
              self.Status := 'Assigned to: '+FClientsocket.RemoteHost+':'+FClientSocket.RemotePOrt;
              FServerSocketThread.ServerSocket.Process(FClientSocket);
              self.Status := 'Completed : '+FClientsocket.RemoteHost+':'+FClientSocket.RemotePOrt;
            except
              on E: Exception do begin
                self.Status := 'DoExecute caught '+e.classname+' '+e.message;
                TRY
                  self.Status := '(1)'+Self.Status;
                  //FServerSocketThread.ServerSocket.Close;
                finally
                end;
              end;
            end;
          finally
            self.Status := '(2)'+Self.Status;
            FClientSocket.Free;
            self.Status := '(3)'+Self.Status;
            FClientSocket := nil;
            self.Status := '(4)'+Self.Status;
            ServerSocketThread.UnRegisterThread(self);
            self.Status := '(5)'+Self.Status;
            TPM.NoNeedThread(self);
            self.Status := '(6)'+Self.Status;
            self.RunHot := false;
          end;

      end;
    except
      on E: Exception do begin
         self.Status := '(E)DoExecute caught '+e.classname+' '+e.message;
         self.RunHot := false;
      end;
    end;
//  end;

end;

procedure TBetterClientSocketThread.ExecuteSyncProc;
begin
  Synchronize(SyncProc);
end;

procedure TBetterClientSocketThread.KillClient;
begin
  try
    WAitBEforeAbortTime := 8000;
    self.ClientSocket.Kill;
  except
  end;
end;

procedure TBetterClientSocketThread.MenuAction(idx: NativeInt);
begin
  inherited;
  case idx of
    0: begin
      Lock;
      try
        if assigned(FClientSocket) then
          FClientSocket.kill;
      finally
        Unlock;
      end;
    end;
  end;
end;

procedure TBetterClientSocketThread.SetCLientSocket(
  const Value: TBetterCustomIPClient);
begin
  if FClientSocket <> nil then
    FClientSocket.free;

  FClientSocket := Value;
end;

procedure TBetterClientSocketThread.SetServerSocketThraed(
  const Value: TBetterServerSocketThread);
begin
  if FServerSocketThread<>nil then
    FServerSocketThread.UnRegisterThread(self);

  FServerSocketThread := Value;
  if FServerSocketThread<>nil then
    FServerSocketThread.RegisterThread(self);
end;

procedure TBetterClientSocketThread.SyncProc;
begin
  // override this method and put code there
  // to be executed in the main clx thread,
  // then call ExecuteSyncProc.
end;

{ TBetterServerSocketThread }


destructor TBetterServerSocketThread.Destroy;
begin

  inherited Destroy;
  FThreads.Free;
  FThreads := nil;
end;



procedure TBetterServerSocketThread.DoExecute;
var
  T: TBetterClientSocketThread;
  cs:   TBetterCustomIPClient;
begin
//  SayNatural('Server socket is accepting connections', true);
  Status := 'Listening on: '+serversocket.LocalHost+':'+serversocket.LocalPort;

  Ready := true;
  //while not Terminated (*and Assigned(FServerSocket) and FServerSocket.Listening*) do
  begin
     if ServerSocket.WaitForConnection then begin
//      SayNatural('Incoming!',true);
      if not Terminated then
      begin
        //set ring
        Ring := true;


        Lock;
        try
          T := CreateThread(TBetterCustomIPClient.Create(nil));

          self.ServerSocket.Answer(t.FClientSocket);
          T.Start;

          //wait for thread to accept the ring
          while ring do begin
            sleep(10);
          end;


        finally
          Unlock;
        end;

      end;
    end else begin
    end;
  end;



end;
procedure TBetterServerSocketThread.KillAllClients;
var
  t: ni;
  i: ni;
begin
  Lock;
  try
    for t:= 0 to FThreads.count-1 do begin
      FThreads[t].KillClient;
    end;
  finally
    Unlock;
  end;
  for t:= 0 to FThreads.count-1 do begin
    Lock;
    try
      if t< FThreads.count then begin
        FThreads[t].KillClient;
        FThreads[t].BeginStop;
      end;
    finally
      Unlock;
    end;
  end;

  while FThreads.count > 0 do begin
      Lock;
      try
        if FThreads.count > 0 then begin
          i := Fthreads.count;
          FThreads[0].KillClient;
          FThreads[0].BeginStop;
          Debug.Log('Waiting for #'+FThreads[0].realthread.handle.tostring+' to stop.');
        end;
//        for t:= 0 to FThreads.count-1 do begin
//          FThreads[t].KillClient;
//        end;
      finally
        Unlock;
      end;

      sleep(1000);
  end;
//  for t:= 0 to FThreads.count-1 do begin
//    if TryLock then
//    try
//      if t< FThreads.count then begin
//        FThreads[t].Stopped;
//      end;
//    finally
//      Unlock;
//    end else begin
//      Debug.Log('Waiting for client socket threads to stop.');
//      sleep(1000);
//    end;
//  end;

  while true do begin
    Lock;
    try
//      if FThreads.count > 0 then
//        FThreads[0].EndStop;  //we shouldn't wait on EndStop, the thread will unregister itself whem done.

      if FThreads.count = 0 then
        break;
    finally
      Unlock;
    end;
    sleep(100);
  end;


end;

procedure TBetterServerSocketThread.OnFinish;
begin
  inherited;

  //FServerSocket.Active := false;
  self.KillAllClients;
  WaitForChildThreads;
end;

procedure TBetterServerSocketThread.SetThreadCacheSize(Value: Integer);
begin
  FThreadCacheSize := Value;
end;



constructor TBetterServerSocketThread.Create(Owner: TObject;
  Manager: TThreadManager; pool: TThreadpoolBase);
begin
  inherited;
  self.BetterPriority := bpHighest;
  FThreads := TList<TBetterClientSocketThread>.create;
  Loop := true;
  WAitBEforeAbortTime := 8000;
end;

function TBetterServerSocketThread.CreateThread(clientsocket: TBetterCustomIPClient): TBetterClientSocketThread;
begin
  if FServerSocket.Active = false then
    raise Exception.create('server is shutting down, no new threads allowed.');
  Result := TPM.NeedThread<TBetterClientSocketThread>(nil);
  result.ServerSocketThread := self;
  result.ClientSocket := clientsocket;
  result.betterpriority := bpHighest;

end;



procedure TBetterServerSocketThread.RegisterThread(
  t: TBetterClientSocketThread);
begin
  Lock;
  try
    if FThreads.indexof(t)>=0 then
      raise ECritical.create('thread is already registered!');
    FThreads.add(t);
    Debug.Log(self, 'Socket thread registered. '+inttostr(FThreads.count)+' remain.');
  finally
    uNLOCK;
  end;
end;



procedure TBetterServerSocketThread.UnRegisterThread(
  t: TBetterClientSocketThread);
var
  b: boolean;
begin
  b := false;
  while not b do begin
    if TryLock then
    try
      FThreads.Remove(t);
      Debug.Log(self, 'Socket thread unregistered. '+inttostr(FThreads.count)+' active.');
      b := true;
    finally
      Unlock;
    end else begin
      Debug.Consolelog(t.classname+' can not lock '+self.classname);
      sleep(1000);
    end;
  end;
end;

procedure TBetterServerSocketThread.WaitForChildThreads;
begin
  while FThreads.count> 0 do begin
    debug.ConsoleLog('waiting for '+self.ClassName+'. '+inttostr(self.FThreads.count)+' threads still active.');
    sleep(1000);
  end;

end;

{ TBetterCustomTCPServer }

constructor TBetterCustomTCPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListening := False;
  FServerSocketThread := nil;
{$IFDEF LINUX}
  InitializeCriticalSection(FThreadLock);
{$ENDIF}
  FOnAccept := nil;
  FOnGetThread := nil;
  FOnListening := nil;
  BlockMode := bmThreadBlocking;
end;

destructor TBetterCustomTCPServer.Destroy;
begin
  Close;

{$IFDEF LINUX}
  DeleteCriticalSection(FThreadLock);
{$ENDIF}
  inherited Destroy;
end;

procedure TBetterCustomTCPServer.Open;
begin
  inherited Open;

  if Bind then
    if Listen then
      if BlockMode = bmThreadBlocking then
      begin
        GeTBetterServerSocketThread;
        if Assigned(FServerSocketThread) then begin

          FServerSocketThread.Start;
        end;
      end;
end;

procedure TBetterCustomTCPServer.Close;
begin
  Debug.Log(self,'Close');
  FListening := False;
  OnAccept := nil;
//  Active := false;
  if (BlockMode = bmThreadBlocking) and Assigned(FServerSocketThread) then
  begin
    Debug.Log(self,'KillAllClients');
    FServerSocketThread.KillAllClients;
    Debug.Log(self,'Stopping Server Socket Thread');
    FServerSocketThread.Stop;
    Debug.Log(self,'Waiting for Server Socket Thread to finish');
    FServerSocketThread.SafeWaitFor;
    Debug.Log(self,'Pooling Server Socket Thread');
    TPM.NoNeedThread(FServerSocketThread);

    FServerSocketThread := nil;
{$IFDEF LINUX}
    EnterCriticalSection(FThreadLock);
    try
    finally
      LeaveCriticalSection(FThreadLock);
    end;
{$ENDIF}
  end;

  inherited Close;
end;

function TBetterCustomTCPServer.Accept: Boolean;
var
  ClientSocket: TBetterCustomIPClient;
begin
  ClientSocket := TBetterCustomIPClient.Create(nil);
  try
    Result := Accept(ClientSocket);
  finally
    ClientSocket.Free;
  end;
end;

function TBetterCustomTCPServer.Accept(var ClientSocket: TBetterCustomIPClient): Boolean;
var
  sock: TSocket;
  addr: TSockAddr;
  len: Integer;
begin
  Result := False;
  len := sizeof(addr);
  Fillchar(addr, sizeof(addr), 0);
  try
{$IFDEF MSWINDOWS}
    Sock := ErrorCheck(WinSock.accept(FSocket, @addr, @len));
{$ENDIF}
{$IFDEF LINUX}
    Sock := ErrorCheck(Libc.accept(FSocket, @addr, @len));
{$ENDIF}
  except
    Sock := INVALID_SOCKET;
  end;
  if Sock <> INVALID_SOCKET then
  begin
    Result := True;
    ClientSocket.FActive := True;
    ClientSocket.FConnected := True;
    ClientSocket.FSocket := Sock;
    ClientSocket.FDomain := FDomain;
    ClientSocket.SockType := FSockType;
    ClientSocket.FProtocol := FProtocol;
    ClientSocket.FBlockMode := FBlockMode;
    ClientSocket.FRemoteHost := inet_ntoa(addr.sin_addr);
    ClientSocket.FRemotePort := IntToStr(ntohs(addr.sin_port));
    DoAccept(ClientSocket);
    ClientSocket.Disconnect;
  end;
end;

procedure TBetterCustomTCPServer.GetThread(Sender: TObject; var ClientSocketThread: TBetterClientSocketThread);
begin
  if Assigned(FOnGetThread) then
    FOnGetThread(Self, ClientSocketThread);
end;

function TBetterCustomTCPServer.GeTBetterServerSocketThread: TBetterServerSocketThread;
begin
  if not Assigned(FServerSocketThread) then
    FServerSocketThread := TPM.NeedThread<TBetterServerSocketThread>(nil);
  if Assigned(FServerSocketThread) then
    FServerSocketThread.OnGetThread := GetThread;
  Result := FServerSocketThread;
  result.ServerSocket := self;

end;

procedure TBetterCustomTCPServer.SeTBetterServerSocketThread(Value: TBetterServerSocketThread);
begin
  if Assigned(FServerSocketThread) then
  begin
    FServerSocketThread.Stop;
    FServerSocketThread.WaitForFinish;
    TPM.NoNeedThread(FServerSocketThread);
    FServerSocketThread := nil;
    Close;
  end;
  FServerSocketThread := Value;
end;

procedure TBetterCustomTCPServer.DoAccept(ClientSocket: TBetterCustomIPClient);
begin
  if Assigned(FOnAccept) then
    FOnAccept(Self, ClientSocket);
end;

function TBetterCustomTCPServer.Listen(backlog: Integer): Boolean;
begin
  if Active and not FListening then
  begin
{$IFDEF MSWINDOWS}
    FListening := ErrorCheck(WinSock.listen(FSocket, backlog)) = 0;
{$ENDIF}
{$IFDEF LINUX}
    FListening := ErrorCheck(Libc.listen(FSocket, backlog)) = 0;
{$ENDIF}
  end;
  Result := FListening;
end;

procedure TBetterCustomTCPServer.SetServerBlockMode(Value: TServerSocketBlockMode);
begin
  if Value <> FServerBlockMode then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FServerBlockMode := Value;
    if Value = bmThreadBlocking then
      inherited BlockMode := bmBlocking
    else
      inherited BlockMode := Value;
  end;
end;

function TBetterCustomTCPServer.WaitForConnection: Boolean;
var
  ReadReady, ExceptFlag: Boolean;
begin
  Result := False;
{$IFDEF LINUX}
  if BlockMode = bmThreadBlocking then
  begin
    // Hack to avoid server thread block forever in linux
    EnterCriticalSection(FThreadLock);
    try
      if Select(@ReadReady, nil, @ExceptFlag, 1000) then
        Result := ReadReady and not ExceptFlag;
    finally
      LeaveCriticalSection(FThreadLock);
    end;
  end
  else
{$ENDIF}
    if Select(@ReadReady, nil, @ExceptFlag, 4000) then
      Result := ReadReady and not ExceptFlag;
end;

{$IFDEF MSWINDOWS}
var
  WSAData: TWSAData;

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise ESocketError.Create('WSAStartup');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise ESocketError.Create('WSACleanup');
end;
{$ENDIF}



function TBetterCustomTCPServer.Answer(
  var ClientSocket: TBetterCustomIPClient): Boolean;
var
  sock: TSocket;
  addr: TSockAddr;
  len: Integer;
begin
  Result := False;
  len := sizeof(addr);
  Fillchar(addr, sizeof(addr), 0);
  try
{$IFDEF MSWINDOWS}
    Sock := ErrorCheck(WinSock.accept(FSocket, @addr, @len));
{$ENDIF}
{$IFDEF LINUX}
    Sock := ErrorCheck(Libc.accept(FSocket, @addr, @len));
{$ENDIF}
  except
    Sock := INVALID_SOCKET;
  end;
  if Sock <> INVALID_SOCKET then
  begin
    Result := True;
    ClientSocket.FActive := True;
    ClientSocket.FConnected := True;
    ClientSocket.FSocket := Sock;
    ClientSocket.FDomain := FDomain;
    ClientSocket.SockType := FSockType;
    ClientSocket.FProtocol := FProtocol;
    ClientSocket.FBlockMode := FBlockMode;
    ClientSocket.FRemoteHost := inet_ntoa(addr.sin_addr);
    ClientSocket.FRemotePort := IntToStr(ntohs(addr.sin_port));
    self.ServerSocketThread.Ring := false;
  end;
end;

procedure TBetterCustomTCPServer.Process(
  var ClientSocket: TBetterCustomIPCLient);
begin
  DoAccept(ClientSocket);
  ClientSocket.Disconnect;

end;

function Socket_Read(s: TBetterCustomIPClient; p: pbyte; iLength: ni; iWaitTime: ni): ni;
var
  iJustRead: ni;
begin
  iJustRead := 0;
  s.CheckConnected;
  if s.WaitForData(iWaitTime) then begin
    iJustRead := s.ReceiveBuf(p[0], iLength);
    if ijustRead = 0 then
      raise ENetworkError.create('connection dropped');
  end;

  result := iJustRead;


end;

//------------------------------------------------------------------------------
procedure Socket_GuaranteeRead(s: TBetterCustomIPClient; p: pbyte; iLength: ni);
var
  iREad, iJustRead: ni;
begin
  s.CheckConnected;
  iRead := 0;
  iJustRead := 0;
  while iRead < iLength do begin
    if s.WaitForData(1000) then begin
      iJustRead := s.ReceiveBuf(p[iRead], iLength-iRead);
      if ijustRead = 0 then
        raise ENetworkError.create('connection dropped');
    end else
      if s.killme then
        raise ESocketCritical.create('socket killed');
    inc(iRead, iJustRead);
  end;


end;
//------------------------------------------------------------------------------
procedure Socket_GuaranteeWrite(const s: TBetterCustomIPClient; const p: pbyte; const iLength: ni);
var
  iToWrite, iWrote, iJustWrote, iLastWrote: ni;
  useless: nativeint;
  f: ni;
  bErr: boolean;
begin
  useless := 0;
  s.CheckConnected;
  iWrote := 0;
  iLastWrote := 9000;
  f := 0;
  bErr := false;
  while iWrote < iLength do begin
    iToWrite := iLength-iWrote;

    s.NoDelay := iToWrite < iLastWrote;
    iJustWrote := s.SendBuf(p[iWrote], iToWrite,f);
    if iJustWrote < 0 then begin
      bErr := true;
      break;
    end;

    inc(iWrote, iJustWrote);
  end;

  if bErr then
    raise ESocketCritical.create('TCP Socket dropped during expected write.');
end;
//------------------------------------------------------------------------------


procedure Register;
begin
  RegisterComponents('Digital Tundra', [TBetterTCPServer, TBetterTCPClient]);
end;




procedure oinit;
begin
  Startup;
end;

procedure ofinal;
begin
  Cleanup;


end;

{ TMultiThreadedCommandProcessor }

{ TSocketRingBuffer }

procedure TSocketRingBuffer.eFSocket(const Value: TBetterBaseSocket);
begin
  FSocket := Value;
end;

function TSocketRingBuffer.IsConnected: boolean;
begin
  result := false;
  self.Lock;
  try
    if assigned(self.Socket) then begin
      result := self.Socket.Active;
    end;
  finally
    self.unlock;
  end;
end;

initialization
  init.RegisterProcs('Better_Sockets2', oinit, ofinal,'ManagedThread');



end.

