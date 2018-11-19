unit SimpleWinsock_Legacy;
{$IFNDEF VER150}{$INLINE AUTO}{$ENDIF}
interface
//TODO: Flush buffer on close
//TODO: Flush Buffer on read

uses System.Win.ScktComp, Forms, DtNetConst, Classes, sysutils, SimpleAbstractConnection, winapi. windows, shareddebuglog, crossplatformtypes;
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
    FWinSock: TClientSocket;
    FStream: TWinSocketStream;
    FAddress: string;
    FTimeOut: integer;
    FLastDataTime: cardinal;
    FoutBuffer: pbyte;
    FoutBufferSize: integer;
    FBufferIndex: integer;
    function GetLocalHost: string;

  protected
    function GetConnected: boolean;override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property LocalHost: string read GetLocalHost;
    property Address: string read FAddress write FAddress;
    function Connect: boolean; override;
    procedure Disconnect;override;
    function WaitForData(timeout: cardinal): boolean;override;
    procedure SendData(buffer: pbyte; length: integer);override;
    procedure SendDataOld(buffer: pbyte; length: integer);
    function BufferSend: boolean;
    function DoReadData(buffer: pbyte; length: integer; bReadAll: boolean = false): integer;override;
    function ReadDataAsString(length: integer; bReadAll: boolean): ansistring;
    function ReadDataAsStringUntilClose(iTimeOutMS: integer = 30000): ansistring;
    procedure OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    function IsConnected: boolean;
    property TimeOut: integer read FTimeOut write FtimeOut;
    procedure CheckTimeout;
    procedure ResetTimeout;
    procedure Flush;override;

  end;

procedure SetDefaultWinsockTimeout(value: integer);
function  GetDefaultWinsockTimeout: integer;
function LesserOf(a,b: integer): integer;
function GreaterOf(a,b: integer): integer;


const
  sErrTransportSend = 'Could not send request: connection not active.';

var
  sect: _RTL_CRITICAL_SECTION;
  bKillSockets: boolean;
  G_Default_Winsock_TimeOut: integer;

function SocketsAllowed: boolean;
procedure KillSockets;
procedure ReviveSockets;
procedure RaiseSocketsDisabled;
procedure CheckSocketsDisabled;

implementation

uses dialogs, MiscRoutines;
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
  self.Disconnect;
  FreeMem(FOutBuffer);
  FwinSock.free;
  FWinsock := nil;
  FStream.free;
  FStream:=nil;
  inherited;
end;
//-------------------------------------------------------------------------------
constructor TSimpleWinSockconnection.create;
begin
  inherited;
  //Create the TClientSocket class which actually does all the work.
  FWinSock := TClientSocket.create(nil);
  FWinSock.ClientType := ctBlocking;
  FStream := nil;
  FAddress := '';
  FWinsock.OnDisconnect := OnDisconnect;
  FTimeout := GetDefaultWinsockTimeout;

end;
//-------------------------------------------------------------------------------
function TSimpleWinSockConnection.Connect: boolean;
//Connects to the socket server.  Return true if successful, otherwise false.
label
  tryConnection;
var
  iRetryCount: integer;
  tmStartTime, tmCurrentTime: cardinal;
begin
  result := false;
  GetMem(FOutBuffer, 1);
  FOutBufferSize := 0;
  FBufferIndex := 0;


  FWinSock.ClientType := ctBlocking;
  FwinSock.Host := HostName;
  FwinSock.Address := Address;
  FwinSock.Port := strtoint(EndPoint);
//  FWinsock.Socket.LocalPort := 10000 + GetCurrentThreadID;

  //Setup aggregate with proper params

  iRetryCount :=0;

  tmStartTime := GetTicker;
  tmCurrentTime := tmStartTime;

  while (iRetryCount < 2) and ((tmCurrentTime-tmStartTime) < cardinal(FTimeout)) do begin
    try
      CheckSocketsDisabled;
      try
        FWinSock.Open;

        if Fstream = nil then
          Fstream := TWinSocketStream.Create(FWinSock.socket, FTimeOut);

        result := true;

        break;

      except
        on E: ESocketsDisabled do begin
          raise;
        end;
        on E: Exception do begin
          result := false;
          tmCurrentTime := GetTicker;
          self.Error := e.message;
//          FWinSock.Free;
//          FWinsock := TClientSocket.Create(nil);
//          FWinSock.ClientType := ctBlocking;
//          FwinSock.Host := HostName;
//          FwinSock.Address := Address;
//          FwinSock.Port := strtoint(EndPoint);

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
  while BufferSend do CheckTimeout;

  bDisconnected := true;
  if assigned(FWinsock) then begin
    FWinSock.Socket.Close;
    FWinSock.Close;
    fWinsock.free;
    FWinsock := nil;
  end;
  if assigned(FStream) then begin
    Fstream.free;
    Fstream := nil;
  end;
  GLOG.Debug('Someone called disconnect', 'forwarder');

end;
procedure TSimpleWinSockConnection.Flush;
begin
  repeat until not BufferSend;

end;

//-------------------------------------------------------------------------------
function TSimpleWinSockConnection.WaitForData(timeout: cardinal): boolean;
//Waits for data for TIMEOUT milleseconds.  Returns true if data was received,
//false if timed out.
var
  tm1, tm2: cardinal;
  bFirst: boolean;
begin
  result := false;

  if Fstream = nil then
    raise ETransportError.create('Winsock stream dropped unexpectedly');

  //result := Fstream.WaitForData(timeout);

  tm1 := GetTicker;
  tm2 := GetTicker;


  bFirst := true;
  while (tm2-tm1 < timeout) or bFirst do begin
    bFirst := false;
    while BufferSend do CheckTimeout;

    result := Fstream.WaitForData(lesserof(timeout, 5000));
    if result then break;

//    GLOG.Debug('TSimpleWinsock is Waiting for response...');

    CheckSocketsDisabled;

    //sleep(100);
    //update timer
    tm2 := GetTicker;
    //rollover protection
    if tm2<tm1 then tm1 := tm2;

    if (tm2-tm1)>timeout then break;


  end;
end;
//-------------------------------------------------------------------------------
procedure TSimpleWinSockConnection.SendData(buffer: pbyte; length: integer);
var
  idx: integer;
begin
  inherited;
  //position to buffer is at the end of the current buffer
  idx := FOutBufferSize;
  //make the buffer bigger, + length of what's begin added
  inc(FOutBufferSize, length);
  //reallocate the buffer
  reallocmem(FOutBuffer, FOutBufferSize);

  //MOVE THE new shit onto the end of the buffer

 //glog.Debug('ADDING '+inttostr(length)+' TO BUFFER AT:'+inttostr(idx));

  MoveMem32(pbyte(FOutBuffer)+idx, buffer, length);



  if (FOutBufferSize-FBufferIndex) > 128 then
    BufferSend;

end;

procedure TSimpleWinSockConnection.SendDataOld(buffer: pbyte;length: integer);
//Sends data to the server.
var
  idx: integer;
  i: integer;
begin

  self.FWinSock.socket.Lock;
  try
    CheckSocketsDisabled;

    if Fstream = nil then
      raise ETransportError.createFMT(sErrTransportSend, []);
    idx := 0;
    while (idx < length) do begin
      i := self.FWinSock.Socket.SendBuf(buffer[idx], lesserof(length-idx,1000));

//Fstream.write(buffer[idx], lesserof(Length, 1000));
      if i < 0 then
        raise exception.create('what the fuck');
      if i = 0 then
        sleep(1);

      inc(idx, i);
    end;
  finally
    self.FWinsock.socket.Unlock;
  end;

end;
//-------------------------------------------------------------------------------
function TSimpleWinSockConnection.DoReadData(buffer: pbyte; length: integer; bReadAll: boolean = false): integer;
//Reads data into BUFFER up-to LENGTH bytes.  The read operation MAY NOT read
//the number of bytes requested.  The RESULT pis the number of bytes ACTUALLY
//read.
var
  iRead, iJustRead: integer;
begin
  result := 0;
  self.FWinSock.Socket.Lock;
  try
    while BufferSend do CheckTimeout;

    if (Fstream = nil) or (not IsConnected) then
      raise ETransportError.createFMT('Could not read data: stream dropped', []);

  //  if not FStream.WaitForData(WINSOCK_TIMEOUT) then begin
  //    self.Disconnect;
  //    result := 0;
  //    exit;
  //  end else begin

      if self.IsConnected then begin

        if not self.WaitForData(TimeOut) then
          raise ETransportError.create('Socket timed out.');


        iRead := 0;
        repeat
          if WaitForData(1) then begin
            iJustread := Fstream.Read(buffer[iRead],length);

            if iJustRead >0 then begin
              ResetTimeOut
            end else begin
              DisConnect;
              exit;
            end;

            inc(iRead, iJustRead);
            result := iRead;
          end;
        until (not bReadAll) or (iRead = length);

        if result = 0 then
          self.Disconnect;

        if assigned(self.OnData) then
          OnData;
      end else
        result := 0;
      //if the waitfordata function returns success, but zero bytes is read...
      //then it means that the client has recieved a close request on the socket
  //  end;
  finally
    self.FWinsock.socket.Unlock;
  end;

end;
//-------------------------------------------------------------------------------
//-------------------------------------------------------------------------------
function TSimpleWinSockConnection.GetLocalHost: string;
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

//  if result then result := not IsThreadTerminated;
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



//------------------------------------------------------------------------------
function TSimpleWinSockConnection.GetConnected: boolean;
begin
  result := FWinSock.Socket.connected;
  if result then begin
    CheckConnected;
  end;
end;

function LesserOf(a,b: integer): integer;
begin
  result :=a;
  if b< a then
    result := b;

end;
function GreaterOf(a,b: integer): integer;
begin
  result :=a;
  if b> a then
    result := b;

end;

function TSimpleWinSockConnection.BufferSend: boolean;
var
  iSent: integer;
  iToSend: integer;
begin
  result := false;
  if FOutBuffersize = 0 then
    exit;

  //send some shit
  CheckSocketsDisabled;

  iToSend := lesserof(FOutBufferSize-FBufferIndex, 4000);
  if iToSend = 0  then
    exit;
  iSent := self.FWinSock.Socket.SendBuf(pbyte(pbyte(FOutBuffer)+fbufferIndex)[0], iToSend);


  if iSent > 0 then begin
   //glog.Debug('TRANSMITTED '+inttostr(iSent)+' bytes');
    //reset timeout
    ResetTimeout;
    result := true;
    //move the buffer index forward
    FBufferIndex := FBUfferINdex + iSent;
   //glog.Debug('Buffer Index is now '+inttostr(FBufferINDEX));

    //if the buffer is kinda big then

    if FBufferIndex > 1000 then begin
      //move the end of the buffer onto the beginning
     //glog.Debug('**SHRINKING BUFFER**');
     //glog.Debug('MOving '+inttostr(FOutBufferSize-FBufferIndex)+' bytes of memory ');
      MoveMem32(FOutBuffer, pbyte(FOutBuffer)+FBufferIndex, FOutBufferSize-FBufferIndex);
      dec(FOutBufferSize, FBufferIndex);
      FBufferIndex := 0;
     //glog.Debug('NEw Buffer Index is '+inttostr(FBufferIndex));
      reallocmem(FOutBuffer, GreaterOf(FOutBufferSize,1));
    end;
  end else
    checkTimeout;



end;

function TSimpleWinSockConnection.CheckConnected: boolean;
begin
  result := self.FWinSock.socket.connected;
{$IFDEF BETTER_CONNNECTION_CHECKES}
  if result then begin
    if self.WaitForData(1) then begin
      result := FWinsock.Socket.ReceiveLength > 0;
    end;
  end;
{$ENDIF}

  if not result then
    FWinsock.Socket.Close;

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

function TSimpleWinSockConnection.ReadDataAsString(length: integer; bReadAll: boolean): ansistring;
var
  pc: pbyte;
  iRead: integer;
begin
  result := '';

  GEtMem(pc, Length);
  //pc := pbyte(GetMEm(Length));
  try
    iRead := ReadData(pc, length, bReadAll);
    SetLength(result, iRead);
    MoveMem32(@result[1], pc,iRead);


  finally
    Freemem(pc);
  end;








end;

function TSimpleWinSockConnection.ReadDataAsStringUntilClose(
  iTimeOutMS: integer): ansistring;
var
  s1: ansistring;
  idx: integer;
begin
  s1 := '';
  result := '';
  while waitfordata(iTimeoutMS) do begin
    s1 := ReadDataAsString(1000, false);
    idx := length(result);
    setlength(result, idx+length(s1));
    MoveMem32(@result[idx], @s1[1], length(s1));
  end;
end;

procedure CheckSocketsDisabled;
begin
  if not SocketsAllowed then
    RaiseSocketsDisabled;

end;


procedure RaiseSocketsDisabled;
begin
   raise ESocketsDisabled.create('Socket terminated because the application is shutting down.');
end;

procedure oinit;
begin
  bKillSockets := false;
  G_Default_Winsock_Timeout := 300000;
  InitializeCriticalSection(sect);

end;

procedure ofinal;
begin
  DeleteCriticalSection(sect);


end;

initialization
  init.RegisterProcs('SimpleWinsock_Legacy', oinit, ofinal);

finalization





end.
