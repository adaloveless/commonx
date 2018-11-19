unit SimpleWinSock2;

interface

uses scktComp, Forms, DtNetConst, Classes, SimpleAbstractConnection, windows, sysutils;
,orderlyinit
type
  ESocketsDisabled = class(Exception);

  TSimpleWinSockConnection = class(TSimpleAbstractConnection)
  //Implements the methods of SimpleAbstractConnection as specific for
  //WinSock communication.  This class is actually a wrapper around the
  //TClientSocket class, and ADAPTS the use of the TClientSocket and
  //TWinSocketStream classes to fit the SimpleAbstractConnection interface.
  private
    bDisconnected : boolean;
    FWinsock: TCLientSocket;
    FAddress: ansistring;
    FTimeOut: integer;
    FLastDAtaTime: cardinal;
    function GetLocalHost: ansistring;
    function GetConnected: boolean;override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property LocalHost: ansistring read GetLocalHost;
    property Address: ansistring read FAddress write FAddress;
    function Connect: boolean; override;
    procedure Disconnect;override;

    function WaitForData(timeout: cardinal): boolean;override;
    procedure SendData(buffer: PAnsiChar; length: integer);override;
    function ReadData(buffer: PAnsiChar; length: integer; bReadAll: boolean = false): integer;override;
    procedure OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    function IsConnected: boolean;
    property TimeOut: integer read FTimeOut write FtimeOut;
    procedure CheckTimeout;
    procedure ResetTimeout;

  end;

const
  sErrTransportSend = 'Could not send request: connection not active.';
  WINSOCK_TIMEOUT = 300000; //Maximum length of time the transport will
  //wait without receiving any data from the server in Milliseconds.

var
  sect: _RTL_CRITICAL_SECTION;
  bKillSockets: boolean;
  G_Default_Winsock_TimeOut: integer;

function SocketsAllowed: boolean;
procedure KillSockets;
procedure ReviveSockets;
procedure SetDefaultWinsockTimeout(value: integer);
function  GetDefaultWinsockTimeout: integer;
procedure RaiseSocketsDisabled;
procedure CheckSocketsDisabled;

implementation

//------------------------------------------------------------------------------
procedure SetDefaultWinsockTimeout(value: integer);
begin
  EnterCriticalSection(sect);
  try
    G_Default_Winsock_Timeout := value;
  finally
    LeaveCriticalSection(sect);
  end;

end;

//------------------------------------------------------------------------------
function  GetDefaultWinsockTimeout: integer;
begin
  EnterCriticalSection(sect);
  try
    result := G_Default_Winsock_Timeout;
  finally
    LeaveCriticalSection(sect);
  end;

end;

//-------------------------------------------------------------------------------
destructor TSimpleWinSockconnection.destroy;
begin
  FwinSock.free;
  FWinsock := nil;
  inherited;
end;
//-------------------------------------------------------------------------------
constructor TSimpleWinSockconnection.create;
begin
  inherited;
  //Create the TClientSocket class which actually does all the work.
  FWinSock := TClientSocket.create(nil);
  FWinSock.ClientType := ctBlocking;
  FAddress := '';
  FWinsock.OnDisconnect := OnDisconnect;
  FTimeout := WINSOCK_TIMEOUT;
  FLastDAtaTime := GetTicker;

end;
procedure TSimpleWinSockConnection.CheckTimeout;
var
  tmnow: cardinal;
begin
  tmnow := GetTicker;

  if (tmnow < FLastDatatime) or (FLastDataTime = 0) then begin
    FLastDataTime := tmNow;
  end;

  if (tmNow - FLastDataTime) > cardinal(FTimeout) then
    raise Exception.create('Socket timeout');


  CheckSocketsDisabled;


end;

procedure TSimpleWinSockConnection.ResetTimeout;
begin
  FLastDatatime := GetTicker;
end;

function TSimpleWinSockConnection.Connect: boolean;
//Connects to the socket server.  Return true if successful, otherwise false.
label
  tryConnection;
var
  iRetryCount: integer;
  tmStartTime, tmCurrentTime: cardinal;
begin
  result := false;

  FwinSock.Host := HostName;
  FwinSock.Address := Address;
  FwinSock.Port := strtoint(EndPoint);

  //Setup aggregate with proper params

  iRetryCount :=0;

  tmStartTime := GetTicker;
  tmCurrentTime := tmStartTime;

  while (iRetryCount < 4) and ((tmCurrentTime-tmStartTime) < WINSOCK_TIMEOUT) do begin
    try
      if not SocketsAllowed then
        raise ETransportError.create('Socket terminated because the application is shutting down.');
      try
        FWinSock.Open;

        result := true;

        break;

      except
        on E: Exception do begin
          result := false;
          tmCurrentTime := GetTicker;
          self.Error := E.Message;
          FWinSock.Free;
          FWinsock := TClientSocket.Create(nil);
          FwinSock.Host := HostName;
          FwinSock.Address := Address;
          FwinSock.Port := strtoint(EndPoint);
          FWinSock.ClientType := ctBlocking;
        end;
      end;
    finally
      inc(iRetryCount);
      bDisconnected := not result;
    end;
  end;

end;
//-------------------------------------------------------------------------------
procedure TSimpleWinSockConnection.Disconnect;
//Disconnects from the socket server.
begin
  bDisconnected := true;
  FWinSock.Socket.Close;
  FWinSock.Close;

end;
//-------------------------------------------------------------------------------
function TSimpleWinSockConnection.WaitForData(timeout: cardinal): boolean;
//Waits for data for TIMEOUT milleseconds.  Returns true if data was received,
//false if timed out.
var
  tm1, tm2: cardinal;
begin
  result := false;

  //result := Fstream.WaitForData(timeout);

  tm1 := GetTicker;
  tm2 := GetTicker;

  while tm2-tm1 < timeout do begin

    result := FWinSock.Socket.ReceiveLength > 0;
    if result then break else sleep(1);

    if not SocketsAllowed then
      raise ETransportError.create('Socket terminated because the application is shutting down.');


    //sleep(100);
    //update timer
    tm2 := GetTicker;
    //rollover protection
    if tm2<tm1 then tm1 := tm2;

    if (tm2-tm1)>timeout then break;
  end;
end;
//-------------------------------------------------------------------------------
procedure TSimpleWinSockConnection.SendData(buffer: PAnsiChar;length: integer);
//Sends data to the server.
begin

  if not SocketsAllowed then
    raise ETransportError.create('Socket terminated because the application is shutting down.');


  FWinsock.Socket.SendBuf(buffer[0], length);

end;
//-------------------------------------------------------------------------------
function TSimpleWinSockConnection.ReadData(buffer: PAnsiChar; length: integer; bReadAll: boolean = false): integer;
//Reads data into BUFFER up-to LENGTH bytes.  The read operation MAY NOT read
//the number of bytes requested.  The RESULT pis the number of bytes ACTUALLY
//read.
var
  iJustRead: integer;
  iRead: integer;
begin
  if not SocketsAllowed then
    raise ETransportError.create('Socket terminated because the application is shutting down.');

//  if not FStream.WaitForData(WINSOCK_TIMEOUT) then begin
//    self.Disconnect;
//    result := 0;
//    exit;
//  end else begin
    iREad := 0;
    if self.IsConnected then begin

      if not self.WaitForData(TimeOut) then
        raise ETransportError.create('Socket timed out.');

        repeat
          if WaitForData(1) then begin
            iJustRead := self.FWinsock.socket.ReceiveBuf(buffer[0], length);

            if iJustRead >0 then begin
              ResetTimeOut
            end else begin
              DisConnect;
            end;

            inc(iRead, iJustRead);
            result := iRead;
          end;
        until (not bReadAll) or (iRead = length);

      if assigned(self.OnData) then
        OnData;

    end else
      result := 0;
    //if the waitfordata function returns success, but zero bytes is read...
    //then it means that the client has recieved a close request on the socket
//  end;

end;

//-------------------------------------------------------------------------------
function TSimpleWinSockConnection.IsConnected: boolean;
begin
//  result := FWinsock.Socket <> INVALID_SOCKET;

  if bDisconnected then
    result := false
  else
    result := FWinsock.Socket.Connected;
end;
//-------------------------------------------------------------------------------
function TSimpleWinSockConnection.GetConnected: boolean;
begin
  result := FWinSock.Socket.connected;
end;

function TSimpleWinSockConnection.GetLocalHost: ansistring;
begin
  result := FWinsock.Socket.LocalHost;
end;
//------------------------------------------------------------------------------
procedure TSimpleWinSockConnection.OnDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  bDisconnected := true;

end;

//------------------------------------------------------------------------------
function SocketsAllowed: boolean;
begin
  EnterCriticalSection(sect);
  try
    result := not bKillSockets;
  finally
    LeaveCriticalSection(sect);
  end;
end;

//------------------------------------------------------------------------------
procedure ReviveSockets;
begin
  EnterCriticalSection(sect);
  try
    bKillSockets := false;
  finally
    LeaveCriticalSection(sect);
  end;

end;
//------------------------------------------------------------------------------
procedure KillSockets;
begin
  EnterCriticalSection(sect);
  try
    bKillSockets := true;
  finally
    LeaveCriticalSection(sect);
  end;

end;

//------------------------------------------------------------------------------
procedure CheckSocketsDisabled;
begin
  if not SocketsAllowed then
    RaiseSocketsDisabled;

end;
//------------------------------------------------------------------------------

procedure RaiseSocketsDisabled;
begin
   raise ESocketsDisabled.create('Socket terminated because the application is shutting down.');
end;

//------------------------------------------------------------------------------
procedure oinit;
begin
  bKillSockets := false;
  InitializeCriticalSection(sect);

end;

procedure ofinal;
begin
  DeleteCriticalSection(sect);

end;

initialization
  init.RegisterProcs('SimpleWinSock2', oinit, ofinal);

finalization



end.
