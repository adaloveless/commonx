unit Transport;
//This unit has the astract connection class and the Transport class
interface
uses SysUtils, Windows, NetworkBuffer, typex, Classes, DtNetConst, tickcount,
  Packet, SimpleAbstractConnection, ErrorHandler;

const
  NETWORK_TIMEOUT = 3000000; //Maximum length of time the transport will
  //wait without receiving any data from the server in Milliseconds.

type
  TNetworkInterface = (niUndefined,niWinsock,niNamedPipes);
  //Pick Winsock (internet/intranet), NamedPipes(LAN), COM (Standalone),



TTransport =class
  //The Transport class controls the PROCESS of sending and receiving a packet,
  //as defined specifically for the Pathways-RDTP protocol. The class relies on
  //the data being send/read being a TRDTPPacket class.  It will NOT transfer
  //data from other sources.
  protected
    FNetworkInterface: TNetworkInterface;
    FHostName: widestring;//Identifies the computer to connected to
    FEndpoint: widestring;//Identifies the PORT/SOCKET/PIPE or other kind of listener
    FBandWidthLimit: integer; //Maximum bits/sec throughput allowed.
    FDisconnectimmediately: boolean;
    Fconnection: TsimpleAbstractConnection;
    function Connect: boolean;
    function InitConnectionClass: TSimpleAbstractConnection;
  public
    constructor Create(bDisconnectImmediately: boolean = false);reintroduce;virtual;
    destructor destroy; override;

    //Packet Transaction
    function Transact(inPacket: TRDTPPacket; out outPacket: TRDTPPacket; sender: TObject; iTimeOutMS: integer = 300000; bForget: boolean=false):boolean;

    //Protocol SETUP
    property NetworkInterface: TNetworkInterface
      read FNetworkInterface  write FNetworkInterface;
    property HostName: widestring read FHostName write FHostName;
    property EndPoint: widestring read FEndPoint write FEndpoint;

    //Low-Bandwidth Simulation
    property BandWidthLimit: integer read FBandWidthLimit write FBandWidthlimit;
    property Connection: TSimpleAbstractConnection read FConnection;

end;

implementation

//##############################################################################

uses scktComp, ServerInterface, ExceptionsX, SimpleWinSock,
  SimpleNamedPipe, DataObjectServices;




//###########################################################################
// TTransport
//###########################################################################
constructor TTransport.create(bDisconnectImmediately: boolean);
begin
  inherited create;
  self.FDisconnectimmediately := bDisconnectImmediately;

  FNetworkInterface := niUndefined;
  FBandWidthLimit := 0;
  FHostName := '';
  FEndPoint := '';


end;
//---------------------------------------------------------------------------
destructor TTransport.destroy;
begin
  FConnection.Free;
  FConnection := nil;
  inherited;
end;
//---------------------------------------------------------------------------
function TTransport.Connect: boolean;
//Connects to the server, ensuring that the connection class is initialized
//Returns boolean result indicating success and an OUT parameter that is the
//instantiated and connected CONNECTION class of TSimpleAbstractConnection.
//The OUT parameter is need for allowing thread safe operation and allowing
//multiple connections to at once.
begin
  //Disable message processing for forms so we can call "processMessages"
  //without allowing the user to click on buttons.
  try
    if Fconnection <> nil then begin
      result := FConnection.Connected;
      if not result then
        result := Fconnection.connect;
      exit;
    end;

    //Ensure that the connection class is initialized;
    FConnection := InitConnectionClass;
    //Connect
    result := FConnection.Connect;

  finally
    //Re-enable message processing
  end;

end;

function TTransport.Transact(inPacket: TRDTPPacket; out outPacket: TRDTPPacket; sender: TObject; iTimeOutMS: integer = 300000; bForget: boolean  = false):boolean;
//Sends the passed buffer and length and swaps with a reply
//Pass it a PByte buffer, and the length of the thing to be sent...
//I wish this function wasn't so difficult.  TPowersock isn't as robust as it should be
//**************************************************************************
//IMPORTANT NOTE:  The buffer is not automatically destroyed by the function
//Must call strDispose externally. <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
var
  buffer : PByte;
  length : integer;
  pcHeader: PByte;
  iBytesRead: integer;
  iLength, t: integer;
  bufTemp: PByte;
  iBytesToRead: integer;
  tmBegin: cardinal;
  iBytesPerSec: cardinal;
  iBytesThisRead: cardinal;
begin

  {$IFDEF BEEPy} beeper.beep(100, 25);{$ENDIF}
  //initializevariables
  result := false;
  buffer := nil;
  length := 0;



  if not (sender is TServerInterface) then begin
    raise EArchitecturalInconsistency.CreateFmt('Transport can only be used by TServerInterface',[]);
    exit;
  end;

  //disable message processing on forms
    try
    //Connect

    if not Connect then begin
//      SetLastServerError(999, 'Unable to connect to server '''+self.HostName+''' endpoint '''+self.endpoint+''': '+GetLastServerErrorMessage);
            //raise ETransPortError.create('Unable to connect to Pathways server '''+self.HostName+''' endpoint '''+self.endpoint+''': '+GetLastServerErrorMessage);
      result := false;
      exit;
    end;

    //Define buffer to be sent
    buffer := inPacket.EncryptedBuffer.RawBuffer;
    length := inpacket.packedlength;

    if FConnection = nil then
      raise ETransportError.create('Connection dropped unexpectedly');


    //Send
    FConnection.SendData(buffer, length);

    if FConnection = nil then
      raise ETransportError.create('Connection dropped unexpectedly');

    if bForget then
      exit;

    //Wait
    if not FConnection.WaitForData(iTimeOutMS) then
      raise ETransportError.create('Server did not respond in time');

    //READ

    //allocate memory for header
    getmem(pcHeader, 18);
    //Read Header
    iBytesRead := 0;
    while iBytesRead < 18 do begin
      if FConnection = nil then
        raise ETransportError.create('Connection dropped unexpectedly');

      if not FConnection.WaitForData(iTimeOutMS) then
        raise exception.create('Server connection dropped');

      if FConnection = nil then
        raise ETransportError.create('Connection dropped unexpectedly');

      iBytesThisREad := FConnection.ReadData(pcHeader+iBytesRead, 18-iBytesRead);

      if iBytesThisRead = 0 then
        raise ESocketError.create('client disconnected');

      inc(iBytesRead, iBytesThisREAd);
    end;

    //Get info on packet
    //Extract length from header
    iLength := ord(pcHeader[4])+(ord(pcHeader[5])*256)+(ord(pcHeader[6])*65536)+(cardinal(ord(pcHeader[7]))*(256*65536));

    //Allocate buffer based on size reported in header
    //note: old buffer must be referenced and destroyed externally
    reallocmem(pcHeader, iLength);
    buffer := pcHeader;

    //read rest of stream
    iBytesRead := 18;
    while iBytesRead < iLength do begin

      //determine number of bytes to read based on throttling settings (bandwidthlimit)
      iBytesPerSec := (FBandWidthLimit div 8);

      //determine maximum single read based on bandwidthlimit
      if FBandWidthLimit >0 then begin
        //read X bytes per second
        iBytesToRead := iBytesPerSec;

        //if at end of packet then re-adjust number of bytes to read
        if (iLength-iBytesRead)<iBytesToRead then
          iBytesToRead := iLength-iBytesRead;
      end else
        iBytesToRead := iLength-iBytesRead;

      if iBytesToRead>8000 then iBytesToRead := 8000;
      //Incremental read
      iBytesThisREad := 0;
      bufTemp := buffer+iBytesRead;
      if Fconnection.WaitForData(1) then begin
        iBytesThisRead := FConnection.ReadData(bufTemp, iBytesToRead);
        if iBytesThisREad = 0 then begin
          raise ESocketError.create('Client disconnected');
        end;
        inc(iBytesRead, iBytesThisRead);

      end;

      //Wait
      if iBytesPerSec <> 0 then begin
        tmBegin := GetTicker;
        While GetTicker < (tmBegin+(iBytesThisRead * 1000) div iBytesPerSec) do ;
      end;
    end;

    //Disconnect
    if FDisconnectImmediately then begin
      FConnection.disconnect;
      FConnection.free;
      FConnection := nil;
    end;

    //Reinitialize packet
    outPacket.Initialize;

    //assign the new buffer to the packet
    outpacket.Origin := poServer;
    outpacket.EncryptedBuffer.AssignBuffer(buffer, length);

    result := true;

  finally
  end;
  {$IFDEF BEEP} beeper.beep(50, 25);{$ENDIF}
end;
//---------------------------------------------------------------------------

function TTransport.InitConnectionClass: TSimpleAbstractConnection;
//Creates a connection class based on the values of the HostName, Endpoint,
//and NetworkInterface properties.
begin

  //Create the appropriate connection
  case NetworkInterface of
    niWinsock: result := TSimpleWinSockConnection.create;
    niNamedPipes: result := TSimpleNamedPipeConnection.create;
  else
    begin
            raise Exception.create('Network interface was not defined');
      exit;
    end;
  end;

  //Setup the connection
  result.HostName := HostName;
  result.EndPoint := EndPoint;

end;

end.
