unit SimpleNamedPipe;
//Contains the NamedPipe Simple connection class: TSimpleNamedPipeConnection.

interface
uses scktComp, DtNetConst, Classes, SimpleAbstractConnection, tickcount;

const
  PIPE_TIMEOUT = 120000; //Length of timeout for pipe connection.
  CMAX_PIPE_READ = 32000; //Maximum pipe read length in a single pass.
  CMAX_PIPE_WRITE = 32000; //Maximum pipe write in a single pass.

type
  TSimpleNamedPipeConnection = class(TSimpleAbstractConnection)
  strict protected
    function DoWaitForData(timeout: Cardinal): Boolean; override;
  //Implements the abstract methods of its base class, as they specifically
  //relate to Named Pipe communication.  Named pipe communication is actually
  //built into the Windows file system API.
  protected
    hPipe : cardinal; //FileHandle of the pipe connected to
    sPipeName : ansistring; //Full path to the pipe, e.g. \\computer\pipe\Pathways
    function DoSendData(buffer: pbyte; length: integer): integer;override;//<---------------------------------------------
    function DoReadData(buffer: pbyte; length: integer): integer;override;
    function GetConnected: Boolean; override;//<<------------------------------

  public
    constructor Create; override;
    destructor Destroy; override;
    function DoConnect: boolean; override;
    procedure DoDisconnect;override;

  end;


implementation

uses sysutils, ExceptionsX, Windows;

destructor TSimpleNamedPipeConnection.destroy;
begin
  inherited;
end;

constructor TSimpleNamedPipeConnection.create;
begin
  inherited;
  hPipe := cardinal(INVALID_HANDLE_VALUE);
  sPipeName := '';
end;
//-------------------------------------------------------------------------------
function TSimpleNamedPipeConnection.DoConnect: boolean;
//Connects to the Named pipe.  Returns TRUE if successful, else FALSE.
var
  bTimeout: boolean;
  tm1, tm2: integer;
  iLastError: integer;
begin
  inherited;
  sPipeName := '\\'+FHostName+'\pipe\'+FEndPoint;
  result := false;
  bTimeOut := false;

  //Start timer
  tm1 := GetTicker;

  //Attempt and re-attempt connection until TIMEOUT or CONNECTED.
  while (not result) and (not  bTimeout) do begin
//    IF WaitNamedPipe(PChar(sPipeName),  1000) THEN BEGIN
      hPipe := CreateFile(
        PChar(sPipeName),
        GENERIC_READ+GENERIC_WRITE,
        FILE_SHARE_READ+FILE_SHARE_WRITE,
        nil,
        OPEN_EXISTING,
        FILE_FLAG_NO_BUFFERING,
        INVALID_HANDLE_VALUE);

      result := not (hpipe=INVALID_HANDLE_VALUE);

      if not result then begin
        iLastError := GetLastError;
        if not (iLastError = 231) then
          raise ETransportError.createFMT('Could not connect to pipe.  Error code %d', [GetLastError]);
      end;

//    END;

    //Check for timeout condition
    tm2 := GetTicker;
    if (tm2>=tm1+PIPE_TIMEOUT) then begin
      raise ETransportError.createFMT('Pipe Connection Timed Out',[]);
    end;
  end;

end;
//-------------------------------------------------------------------------------
procedure TSimpleNamedPipeConnection.DoDisconnect;
//Closes the pipe
begin
  inherited;
  CloseHandle(hPipe);
end;
//-------------------------------------------------------------------------------
function TSimpleNamedPipeConnection.DoWaitForData(timeout: cardinal): boolean;
//Waits for any number of bytes to be available in the pipe.
var
  iAvail, iRead, iLeft: integer;
  tmStart, tmEnd: cardinal;
begin
  iAvail := 0;
  result := false;

  tmStart := GetTicker;

  //wait for bytes available to be greater than zero
  while not result do begin

      if not PeekNamedpipe(hPipe,nil,0,@iRead,@iAvail,@iLeft) then
        raise ETransPortError.createFMT('Peek named pipe failed: Error code - %d', [GetLastError]);

      result := (iAvail>0);

    //timeout control
    tmEnd := GetTicker;
    if (((tmStart+timeout)<tmEnd) or (tmEnd<tmStart)) then exit;
  end;

end;
function TSimpleNamedPipeConnection.GetConnected: Boolean;
begin
  result := hpipe <> INVALID_HANDLE_VALUE;
end;

//-------------------------------------------------------------------------------
function TSimpleNamedPipeConnection.DoSendData(buffer: PByte;length: integer): integer;
//Sends data over the pipe connection.
var
  b: boolean;
  iWritten: cardinal;
begin
  iWritten := 0;

  b := WriteFile(hPipe, buffer[0], length, iWritten, nil);
  if not B then
    raise ETransportError.createFMT('Error writing pipe %s', [sPipename]);


  exit(iWritten);

end;
//-------------------------------------------------------------------------------
function TSimpleNamedPipeConnection.DoReadData(buffer: PByte; length: integer): integer;
//Reads up-to LENGTH bytes into BUFFER from pipe.  Result is the number of bytes
//ACTUALLY read.
var
  b: boolean;
  c: cardinal;
  iAvail, iRead, iLeft: integer;
begin
  //due to bugs/limitations of the PIPE protocol... each read is limited to 32K
  if length>CMAX_PIPE_READ then
    length := CMAX_PIPE_READ;

  //ALSO due to bugs/limitations of the PIPE protocol...
  //the length to read cannot be greater than the bytes available
  //We therefore must peek the pipe and make sure that we are not reading
  //more bytes than are available in the pipe.
  if not PeekNamedpipe(hPipe,nil,0,@iRead,@iAvail,@iLeft) then
    raise ETransPortError.createFMT('Peek named pipe failed: Error code - %d', [GetLastError]);
  if iAvail<length then
    length := iAvail;

  //Finally do the actually read.
  b := ReadFile(hPipe, buffer[0], length, c, nil);
  if not B then
    raise ETransportError.createFMT('Error reading pipe %s %d', [sPipename, GetLastError]);

  result := c;
end;


end.
