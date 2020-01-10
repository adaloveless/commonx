unit SimpleWinsock;
{$IFNDEF VER150}{$INLINE AUTO}{$ENDIF}
interface
//TODO: Flush buffer on close
//TODO: Flush Buffer on read

uses Winapi.Winsock, DtNetConst, Classes, sysutils, SimpleAbstractConnection, winapi. windows, debug, typex, numbers, endian;

type
  ESocketsDisabled = class(Exception);
  ESocketError = class(Exception);

  TSocksHeader = packed record
  private
    function GetPortNumber: smallint;
    procedure SetPortNumber(const Value: smallint);
  public
    version: byte;
    command: byte;
    portnumber_revendian:smallint;
    invalidip:array[0..3]of byte;
    procedure Init;
    property portnumber: smallint read GetPortNumber write SetPortNumber;

  end;
  TSocksReply = packed record
    nullbyte: byte;
    status:byte;
    portnumber_endianswap: smallint;
    ip: array[0..3] of byte;
  end;

  TSimpleWinsockconnection = class(TSimpleAbstractConnection)
  private
    hSocket: Winapi.Winsock.TSocket;
    Addr:     TSockAddrIn;
    wsaData:  TWSAData;
    Close:    Boolean;

    bError: boolean;
    bclosed: boolean;
    FUseSocks: boolean;
    function ConnectDirect: boolean;
    function ConnectWithSocks: boolean;
  protected
    function GetConnected: boolean;override;
    function GetIPFromHost(const HostName: ansistring): ansistring;
  strict protected
    function DoWaitForData(timeout: cardinal): boolean;override;
    function DoCheckForData: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function DoConnect: boolean; override;
    procedure DoDisconnect;override;

    function DoSendData(buffer: pbyte; length: integer): integer;override;
    function DoReadData(buffer: pbyte; length: integer): integer;override;
    function CheckConnected: boolean;override;
    procedure CheckSocketError(iJustSent: nativeint);


    procedure Flush;override;
    property UseSocks: boolean read FUseSocks write FUseSocks;

  end;




function recvp(s: TSocket; Buf: pbyte; len, flags: Integer): Integer; stdcall;external 'winsock32.dll' name 'recv';

var
  SocketsAllowed: boolean;


implementation

uses systemx, helpers_stream;





{ TSimpleWinsockconnection }

function TSimpleWinsockconnection.CheckConnected: boolean;
begin
  //no implementation required.... check to make sure if connection is dead but thinks
  //it is still active
  result := Connected;
end;

function TSimpleWinsockconnection.DoConnect: boolean;
begin
  if not UseSocks then
    result := ConnectDirect
  else
    result := ConnectWithSocks;
end;


function TSimpleWinsockconnection.ConnectDirect: boolean;
var
//  Buffer: Array[0..8192] Of Char;
  iRecv: Integer;
Begin
  result := false;
  if (WSAStartup($0202, wsaData) <> 0) then
  begin

    Debug.Log(self,'Unable to start winsock, unable to continue');
    exit;
  end;


  //Prepare a listen socket to be used for our connection
  //and declare its host/port information
  hSocket := Socket(AF_INET, SOCK_STREAM, 0);
  Addr.sin_family   := AF_INET;
  Addr.sin_port     := htons(smallint(strtoint(EndPoint)));
  Addr.sin_addr.S_addr := INET_ADDR(PAnsiChar(GetIPFromHost(ansistring(HostName))));


  Debug.Log(self,'Connecting to ' + string(GetIPFromHost(ansistring(HostName)))+':'+EndPoint);

  //Attempt the connection using our socket
  if (Winapi.Winsock.Connect(hSocket, Addr, SizeOf(Addr)) = 0) then
  begin

    Debug.Log(self,'Connected to host');
    bClosed := false;
    bError := false;
    result := true;
  end;
end;

function TSimpleWinsockconnection.ConnectWithSocks: boolean;
var
//  Buffer: Array[0..8192] Of Char;
  iRecv: Integer;
  shead: TSocksHeader;
  user: ansistring;
  domain: ansistring;
  sreply: TSocksReply;
  zero: byte;
Begin
  zero := 0;
  result := false;
  if (WSAStartup($0202, wsaData) <> 0) then
  begin

    Debug.Log(self,'Unable to start winsock, unable to continue');
    exit;
  end;


  //Prepare a listen socket to be used for our connection
  //and declare its host/port information
  hSocket := Socket(AF_INET, SOCK_STREAM, 0);
  Addr.sin_family   := AF_INET;
  Addr.sin_port     := htons(smallint(9050));
  Addr.sin_addr.S_addr := INET_ADDR(PAnsiChar('127.0.0.1'));


  //Attempt the connection using our socket
  if (Winapi.Winsock.Connect(hSocket, Addr, SizeOf(Addr)) = 0) then
  begin

    Debug.Log(self,'Connected to host');
    bClosed := false;
    result := true;
  end;

  //Do the SOCKS stuff
  if result then begin
    shead.Init;
    shead.portnumber := strtoint(Endpoint);
    SendData(pbyte(@shead), sizeof(shead), true);
    user := '';
    domain := ansistring(hostname);
    //SendData(@user[strz], length(user)+1, true);
    SendData(@zero, 1, true);
    SendData(@domain[strz], length(domain)+1, true);
  end;

  if self.WaitForData(8000) then begin
    GuaranteeReadData(pbyte(@sreply), sizeof(sreply), 8000);
    result := (sreply.status = $5a) and (sreply.nullbyte=0);

  end else
    result := false;

  if not result then
    Disconnect;

end;

constructor TSimpleWinsockconnection.Create;
begin
  inherited;
  hSocket := INVALID_SOCKET;
  bClosed := true;

end;

destructor TSimpleWinsockconnection.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TSimpleWinsockconnection.DoDisconnect;
begin
  inherited;
  if hSocket <> INVALID_SOCKET then
    CloseSocket(hSocket);

  hSocket := INVALID_SOCKET;
   bClosed := true;

end;

function TSimpleWinsockconnection.DoCheckForData: Boolean;
begin
  raise ECritical.create(self.ClassName+' is not intended for data polling.');
end;

function TSimpleWinsockconnection.DoReadData(buffer: pbyte; length: integer): integer;
var
  iJustRead: nativeint;
begin
  iJustRead :=  Recv(hSocket, Buffer[0], lesserof(length, 8000), 0);
  result:= iJustRead;
  CheckSocketError(result);

end;


procedure TSimpleWinsockconnection.Flush;
begin
  inherited;

end;

function TSimpleWinsockconnection.GetConnected: boolean;
begin
  result := (hSocket <> INVALID_SOCKET) and (not bClosed) and not (bError);
end;

function TSimpleWinsockconnection.GetIPFromHost(
  const HostName: ansistring): ansistring;
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  i: Integer;
begin
  Result := '';
  phe := GetHostByName(PAnsiChar(HostName));
  if phe = nil then Exit;
  pPtr := PaPInAddr(phe^.h_addr_list);
  i := 0;
  while pPtr^[i] <> nil do
  begin
    Result := inet_ntoa(pptr^[i]^);
    Inc(i);
  end;
end;

procedure TSimpleWinsockconnection.CheckSocketError(iJustSent: nativeint);
begin
  if iJustSent < 0 then begin
    if iJustSent  = WSAEINPROGRESS then exit;
    if iJustSent  = WSAEWOULDBLOCK then exit;
    if iJustSent  = WSAETIMEDOUT then exit;

    bError := true;
//    Disconnect;
    raise ESocketError.Create('Socket error :'+iJustSent.tostring);
  end;

end;

function TSimpleWinsockconnection.DoSendData(buffer: pbyte; length: integer): integer;
var
  iToSend, iJustSent, iSent: integer;
begin
  iSent := 0;
  iToSend := 0;
  iJustSent := 0;
  RESULT := 0;

  while iSent < length do begin
    iToSend := lesserOf(length-iSent, 8000);
    iJustSent := Send(hSocket, buffer[iSent], iToSend, 0);
    if iJustSent > 0 then
      inc(iSent, iJustSent)
    else begin
      CheckSocketError(iJustSent);
      exit(iSent);
    end;
  end;

  result := iSent;

end;



function TSimpleWinsockconnection.DoWaitForData(timeout: cardinal): boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
begin
  TimeVal.tv_sec := Timeout div 1000;//todo 3: remove this divide
  TimeVal.tv_usec := (Timeout mod 1000) * 1000;
  FD_ZERO(FDSet);
  FD_SET(hSocket, FDSet);
  if timeout = 0 then
    result := (select(0, @FDSet, nil, nil, nil) >= 0)
  else
    Result := (select(0, @FDSet, nil, nil, @TimeVal) >= 0);
end;

{ TSocksHeader }

function TSocksHeader.GetPortNumber: smallint;
begin
  result := portnumber_revendian;
  EndianSwap(@result, sizeof(result));

end;

procedure TSocksHeader.Init;
begin
  version := 4;
  command := 1;
  portnumber := 0;
  invalidip[0] := 0;
  invalidip[1] := 0;
  invalidip[2] := 0;
  invalidip[3] := 69;


end;

procedure TSocksHeader.SetPortNumber(const Value: smallint);
begin
  portnumber_revendian := value;
  EndianSwap(@portnumber_revendian, sizeof(portnumber_revendian));
end;

initialization
  SocketsAllowed := true;

end.
