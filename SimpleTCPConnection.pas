unit SimpleTCPConnection;
{$IFNDEF VER150}{$INLINE AUTO}{$ENDIF}
interface
//TODO: Flush buffer on close
//TODO: Flush Buffer on read

uses IdSocketHandle, idglobal, IdTCPClient, IdTCPConnection,
     DtNetConst, Classes, sysutils, SimpleAbstractConnection,
     debug, typex, numbers, endian;

type
  ESocketsDisabled = class(Exception);

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

  TSimpleTCPConnection = class(TSimpleAbstractConnection)
  private
    Close:    Boolean;
    bError: boolean;
    bclosed: boolean;
    FUseSocks: boolean;
    FTCP: TIDTCPClient;

    function ConnectDirect: boolean;
    function ConnectWithSocks: boolean;
  protected
    function GetConnected: boolean;override;
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

    procedure Flush;override;
    property UseSocks: boolean read FUseSocks write FUseSocks;

  end;




//function recvp(s: TSocket; Buf: pbyte; len, flags: Integer): Integer; stdcall;external 'winsock32.dll' name 'recv';

var
  SocketsAllowed: boolean;


implementation

uses systemx, helpers_stream;





{ TSimpleTCPconnection }

function TSimpleTCPconnection.CheckConnected: boolean;
begin
  //no implementation required.... check to make sure if connection is dead but thinks
  //it is still active
  result := Connected;
end;

function TSimpleTCPconnection.DoConnect: boolean;
begin
  if not UseSocks then
    result := ConnectDirect
  else
    result := ConnectWithSocks;
end;

function TSimpleTCPconnection.ConnectDirect: boolean;
Begin
  try
    FTCP.Host := self.HostName;
    FTCP.Port := strtoint(self.EndPoint);
    FTCP.Connect;
    result := true;
  except
    on E: Exception do begin
      result := false;
      Debug.Log(classname+' could not connect to '+FTCP.Host+':'+FTCP.Port.tostring);

    end;
  end;
end;

function TSimpleTCPconnection.ConnectWithSocks: boolean;
(*var
//  Buffer: Array[0..8192] Of Char;
  iRecv: Integer;
  shead: TSocksHeader;
  user: ansistring;
  domain: ansistring;
  sreply: TSocksReply;
  zero: byte;*)
Begin
  raise ECritical.Create('connectWithSocks() is not implemented in '+classname);
(*
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
*)

end;

constructor TSimpleTCPconnection.Create;
begin
  inherited;
  FTCP := TIdTCPClient.Create(nil);

  bClosed := true;

end;

destructor TSimpleTCPconnection.Destroy;
begin
  Disconnect;
  inherited;
  FTCP.free;
  FTCP := nil;
end;

procedure TSimpleTCPconnection.DoDisconnect;
begin
  inherited;
  FTCP.Disconnect;
end;

function TSimpleTCPconnection.DoCheckForData: Boolean;
begin
  raise ECritical.create(self.ClassName+' is not intended for data polling.');
end;

function TSimpleTCPconnection.DoReadData(buffer: pbyte; length: integer): integer;
var
  bbuf: TIDBytes;
  iToRead: ni;
begin
  if FTCP.iohandler = nil then
    exit(0);

  iToRead := lesserof(length, FTCP.iohandler.InputBuffer.size);
  if iToRead = 0 then
    iToRead := 1;
  FTCP.IoHandler.readBytes(bbuf, iToRead);
  result := system.length(bbuf);
  movemem32(buffer, @bbuf[0], result);



end;


procedure TSimpleTCPconnection.Flush;
begin
  inherited;
  FTCP.Socket.WriteBufferFlush;
end;

function TSimpleTCPconnection.GetConnected: boolean;
begin
  result := FTCP.Connected;
end;

function TSimpleTCPconnection.DoSendData(buffer: pbyte; length: integer): integer;
var
  bbuf: TIDBytes;
begin
  system.setlength(bbuf, length);
  movemem32(@bbuf[0], buffer, length);
  FTCP.IOHandler.Write(bbuf);
//  FTCP.Socket.Write(bbuf);
  result := length;

end;



function TSimpleTCPconnection.DoWaitForData(timeout: cardinal): boolean;
begin

  if FTCP.iohandler = nil then
    exit(true);

  if not FTCP.IOHandler.InputBufferIsEmpty then
    exit(true);
  result := FTCP.Socket.Readable(timeout);
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
