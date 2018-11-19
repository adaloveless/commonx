unit SimpleCommPort;
//Contains the NamedPipe Simple connection class: TSimpleNamedPipeConnection.
{$DEFINE SOUND_DEBUG}

interface
uses
  Forms, DtNetConst, Classes, SimpleAbstractConnection, windows, debug, systemx,typex,numbers, tickcount, betterobject;

const
  PIPE_TIMEOUT = 120000; //Length of timeout for pipe connection.
  CMAX_PIPE_READ = 32000; //Maximum pipe read length in a single pass.
  CMAX_PIPE_WRITE = 32000; //Maximum pipe write in a single pass.



type
  _COMMCONFIG = packed record
    dwSize: DWORD;
    wVersion: Word;
    wReserved: Word;
    dcb: TDCB;
    dwProviderSubType: DWORD;
    dwProviderOffset: DWORD;
    dwProviderSize: DWORD;
    wcProviderData: array[0..0] of WCHAR;
    junk: smallint;
  end;

  TCommConfig = _COMMCONFIG;

  PCommconfig = ^TCommConfig;


  TSimpleCommPortConnection = class(TSimpleAbstractConnection)
  //Implements the abstract methods of its base class, as they specifically
  //relate to Named Pipe communication.  Named pipe communi-cation is actually
  //built into the Windows file system API.
  private
    FEscapeMode: boolean;
    function GetBaudRate: integer;override;
    procedure SetBaudRate(const Value: integer);override;
  strict protected
    function DoCheckForData: Boolean; override;
  protected
    hPipe : THandle; //FileHandle of the pipe connected to
    sPipeName : ansistring;
    bConnected: boolean;
    bLastWasEscape: boolean;
    function GetConnected: boolean;override;
    procedure UpdateBaudRAte_LL;
    procedure SetEndPOint(const Value: string); override;
    function GetISDataAvailable: boolean;override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function DoConnect: boolean; override;
    procedure DoDisconnect;override;
    procedure BeforeDestruction;override;
    function DoWaitForData(timeout: cardinal): boolean;override;
    function DoSendData(buffer: PByte; length: integer): integer;override;
    procedure SendString(buffer: string);
    function DoReadData(buffer: pbyte; length: integer): integer;override;
    function DoReadData_RAW(buffer: pbyte; length: integer): integer;
    function DoReadData_ESC(buffer: pbyte; length: integer): integer;
    property EscapeMode: boolean read FEscapeMode write FEscapeMode;
  end;

function EnumComPorts: IHolder<TStringList>;
function ComPortExists(sPort: string): boolean;


function GetCommConfig(hCommDev: THandle; var lpCC: TCommConfig; var lpdwSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetCommConfig}
function GetCommConfig; external kernel32 name 'GetCommConfig';

function SetCommConfig(hCommDev: THandle; const lpCC: TCommConfig; dwSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetCommConfig}
function SetCommConfig; external kernel32 name 'SetCommConfig';



implementation

uses sysutils, dialogs, exceptions;

function LesserOf(i1,i2: integer): integer;
begin
  result := i1;
  if i2 < i1 then
    result := i2;
end;


destructor TSimpleCommPortConnection.destroy;
begin

  inherited;
  Debug.Log('Destroy '+FEndpoint);
  DisConnect;
end;

constructor TSimpleCommPortConnection.create;
begin
  inherited;
  hPipe := INVALID_HANDLE_VALUE;
  sPipeName := '';
  FEndPoint := 'COM1';
  rbReadAhead.size := 4096;

  RequiresPolling := true;
  rbReadAhead.Size := 65536*8;


end;
//-------------------------------------------------------------------------------
procedure TSimpleCommPortConnection.BeforeDestruction;
begin
  Disconnect;
  inherited;

end;

function TSimpleCommPortConnection.DoConnect: boolean;
//Connects to the Named pipe.  Returns TRUE if successful, else FALSE.
var
  bTimeout: boolean;
  tm1, tm2: integer;
  iLastError: integer;
begin
  result := false;
  if bConnected then exit;

  if (length(FEndPoint) > 4) and (FendPoint[1] <> '\') then begin
    FEndPoint := '\\.\'+FendPoint;
  end;

  Debug.Log('Opening Endpoint: '+FEndpoint);

  sPipeName := FEndPoint;
  result := false;
  bTimeOut := false;

  //Start timer
  tm1 := GetTicker;

  //Attempt and re-attempt connection until TIMEOUT or CONNECTED.
  while (not result) and (not  bTimeout) do begin
//    IF WaitNamedPipe(PAnsiChar(sPipeName),  1000) THEN BEGIN
      hPipe := CreateFileA(
        PAnsiChar(sPipeName),
        GENERIC_READ+GENERIC_WRITE,
        (*FILE_SHARE_READ+FILE_SHARE_WRITE*)0,
        nil,
        OPEN_EXISTING,

//       FILE_FLAG_NO_BUFFERING,

//        FILE_FLAG_OVERLAPPED,
        0,

        0);

      result := not (hpipe=INVALID_HANDLE_VALUE);

      if not result then begin
        iLastError := GetLastError;
        if not (iLastError = 231) then
          raise ETransportError.createFMT('Could not connect to pipe '+sPipeName+'.  Error code %d', [GetLastError]);
      end;

//    END;

    //Check for timeout condition
    tm2 := GetTicker;
    if (tm2>=tm1+PIPE_TIMEOUT) then begin
      raise ETransportError.createFMT('Pipe Connection Timed Out',[]);
    end else begin
      UpdateBaudRAte_LL;
      bConnected := true;
    end;
  end;

  result := bConnected;

end;
//-------------------------------------------------------------------------------
procedure TSimpleCommPortConnection.DoDisconnect;
//Closes the pipe
begin
  //if not bConnected then exit;

  try
    if hPipe <> INVALID_HANDLE_value then begin
      CloseHandle(hPipe);
      hPipe := INVALID_HANDLE_VALUE;
      bConnected := false;
    end;
  except
  end;
end;
//-------------------------------------------------------------------------------
function TSimpleCommPortConnection.DoWaitForData(timeout: cardinal): boolean;
//Waits for any number of bytes to be available in the pipe.
var
  iAvail, iRead, iLeft: integer;
  tmStart, tmEnd: cardinal;
  i: byte;
  iError: cardinal;
  cs: _COMSTAT;
  bMaskSet: boolean;
  ev: dword;
  cc: TCommTimeouts;
  toRead: ni;
  justRead: cardinal;
  a: array of byte;
begin
  if HasLeftovers then begin
    result := true;
    exit;
  end;

  iAvail := 0;
  result := false;

  tmStart := GetTicker;


(*
  We don't need this because HasLeftOvers will cause exit above
  if not HasLeftOverSpace then begin
    sleep(1);
    exit(true);
  end;
*)


  //get current timeout state
  if not GetCommTimeouts(hpipe, cc) then
    exit(false);

  //change it
  cc.ReadTotalTimeoutConstant := timeout;

  //reset timeout states
  if not SetCommTimeouts(hPipe, cc) then
      exit(false);

  //set event mask
  bMaskSet :=  SetCommMask(hPipe, EV_RXCHAR);
  if not bMaskSet then
    exit(false);

  toRead := lesserof(16384,rbReadAhead.SpaceAvailable);
  if not ClearCommError(hpipe, iError, @cs) then begin
    exit(true);
  end;
  toRead := lesserof(toRead, cs.cbInQue);
  toRead := greaterof(1, toRead);
  SetLength(a, toREad);
  if ReadFile(hpipe, a[0], toRead, justRead, nil) then begin
    if justRead < 1 then
      exit(false);
    rbReadAhead.BufferData(@a[0], justRead);
    UpdateBufferStatus;
    exit(true);
  end else
    exit(false);

(*


  if not WaitCommEvent(hPipe, ev,  0) then
    exit(false);

  if (ev and EV_RXCHAR)<>0 then
    exit(true)
  else
    exit(false);


  //wait for bytes available to be greater than zero
  while not result do begin

      iError := 0;

      ClearCommError(hpipe, iError, @cs);

      result := false;
      if cs.cbInQue > 0 then begin
        result := cs.cbInQue > 0;
      end else begin
        //Debug.Consolelog('Nothing to read');
      end;



//      if not PeekNamedpipe(hPipe,@i,sizeof(i),@iRead,@iAvail,@iLeft) then begin
//        raise ETransPortError.createFMT('Peek named pipe failed: Error code - %d', [GetLastError]);
////        iRead := 0;
////        iAvail := 0;
////        iLeft := 0;
//      end else begin
//        sleep(1);
//      end;
//
//      result := (iAvail>0);

    //timeout control
    tmEnd := GetTicker;
    if (((tmStart+timeout)<tmEnd) or (tmEnd<tmStart)) then exit;
    if not result then
      sleep(1);
  end;

*)

end;
//-------------------------------------------------------------------------------
function TSimpleCommPortConnection.DoSendData(buffer: Pbyte;length: integer): integer;
//Sends data over the pipe connection.
var
  b: boolean;
  iWritten: cardinal;
  iPos: integer;
begin
  result := 0;
  iPos := 0;
  b := true;
//  while iPos < length do begin
    b := WriteFile(hPipe, buffer[iPos],
            LesserOf(length-iPos, 64),
//            1,
            iWritten, nil);
    inc(iPos, iWritten);
//    sleep(1);
//  end;


  result := iWritten;

  FlushFileBuffers(self.hPipe);

  if not B then
    raise ETransportError.createFMT('Error writing pipe %s - Error %d', [sPipename, GetLastError]);

end;
//-------------------------------------------------------------------------------
function TSimpleCommPortConnection.DoReadData_RAW(buffer: PByte; length: integer): integer;
//Reads up-to LENGTH bytes into BUFFER from pipe.  Result is the number of bytes
//ACTUALLY read.
var
  b: boolean;
  c: integer;
  cs: _COMSTAT;
  iError: cardinal;
  iAvail, iRead, iLeft: integer;
begin
  //due to bugs/limitations of the PIPE protocol... each read is limited to 32K
  if length>CMAX_PIPE_READ then
    length := CMAX_PIPE_READ;

  //ALSO due to bugs/limitations of the PIPE protocol...
  //the length to read cannot be greater than the bytes available
  //We therefore must peek the pipe and make sure that we are not reading
  //more bytes than are available in the pipe.
//  if not PeekNamedpipe(hPipe,nil,0,@iRead,@iAvail,@iLeft) then begin
//      raise ETransPortError.createFMT('Peek named pipe failed: Error code - %d', [GetLastError]);
//    iRead := 0;
//    iAvail := 0;
//    iLeft := 0;
//  end;
//  if iAvail<length then
//    length := iAvail;

  //Finally do the actually read.
  iError := 0;
  if not ClearCommError(hpipe, iError, @cs) then begin
//    debug.log('Clear fail');
  end;
  if cardinal(length) > cs.cbInQue then begin
    length := cs.cbInQue;
  end;
//  b := ReadFile(hPipe, buffer[0], length, c, nil);
  if length = 0 then
    c := 0
  else
    c := FileRead(hPipe, buffer[0], length);
//  debug.log('Read '+inttostr(length)+' bytes '+inttohex(buffer[0],2));
  if c < 0 then
    raise ETransportError.createFMT('Error reading pipe %s %d', [sPipename, GetLastError]);

  result := c;

end;


function TSimpleCommPortConnection.DoReadData_ESC(buffer: pbyte;
  length: integer): integer;
var
  p: pointer;
  bTemp: pbyte;
  t,u: nativeint;
begin
  GetMem(p, length);
  try
    bTemp := p;
    result := DoReadData_RAW(buffer, length);
    MoveMem32(bTemp, buffer, length);         //move what we read into temp buffer
    //transform the temp buffer into the final result
    u := 0;
    t := 0;
    while t < result do begin
      if ((t=0) and (bLastWasEscape)) or (bTemp[t] = 27) then begin
        if not bLastWasEscape then
          inc(t);
        if t >= result then begin
          bLastWasEscape := true;
        end else begin
          buffer[u] := bTemp[t]-1;
          bLAstWasEscape := false;
        end;
      end else begin
        buffer[u] := bTemp[t];
        bLAstWasEscape := false;
      end;
      inc(t);
      inc(u);

    end;
    result := u;



  finally
    FreeMem(p);
  end;


end;

function TSimpleCommPortConnection.DoCheckForData: Boolean;
begin
  raise ECritical.create(self.ClassName+' is not intended for polled operation');
end;

function TSimpleCommPortConnection.DoReadData(buffer: pbyte;
  length: integer): integer;
begin
  if EscapeMode then begin
    result := DoReadData_ESC(buffer, length);
  end else begin
    result := DoReadData_RAW(buffer, length);
  end;



end;

function TSimpleCommPortConnection.GetBaudRate: integer;
var
  buffer: PCommConfig;
  size: cardinal;
begin
  {Allocate the CommConfig structure}
  GetMem(Buffer, size);
  GetCommConfig(self.hPipe, Buffer^, size);

 {Change the baud rate}
  result :=   Buffer^.dcb.BaudRate;


  FreeMem(buffer);


end;

function TSimpleCommPortConnection.GetConnected: boolean;
begin
  result := bConnected;
end;

function TSimpleCommPortConnection.GetISDataAvailable: boolean;
var
  iAvail, iRead, iLeft: integer;
  tmStart, tmEnd: cardinal;
  i: byte;
  iError: cardinal;
  cs: _COMSTAT;
begin
  if HasLeftovers then begin
    result := true;
    exit;
  end;

  iAvail := 0;
  result := false;

  tmStart := GetTicker;

  //wait for bytes available to be greater than zero

  iError := 0;

  ClearCommError(hpipe, iError, @cs);

  result := false;
  if cs.cbInQue > 0 then begin
    result := cs.cbInQue > 0;
  end else begin
    //Debug.Consolelog('Nothing to read');
  end;
end;

procedure TSimpleCommPortConnection.SetBaudRate(const Value: integer);
begin
  inherited;
  IF Connected then
    UpdateBaudRate_LL;
end;

procedure TSimpleCommPortConnection.SetEndPOint(const Value: string);
begin
  inherited;
  if value = 'COM21' then
    Debug.Log('EndPoint set to ' +value);

end;

procedure TSimpleCommPortConnection.UpdateBaudRAte_LL;
type
  PCommConfig = ^TCommConfig;
var
  buffer: TCommConfig;
  size: cardinal;
  pbuffer: PCommConfig;
begin
  {Allocate the CommConfig structure}
  size := sizeof(_CommConfig);
  if not GetCommConfig(self.HPipe, Buffer, size) then
    raise exception.create('Unable to get baud rate');

 {Change the baud rate}
  IF FBaudRate = 0 then
    exit;
  Buffer.dcb.BaudRate := FBaudRate;
  Buffer.dcb.ByteSize := 8;
  Buffer.dcb.StopBits := ONESTOPBIT;
  Buffer.dcb.Flags := 1;

  //Buffer.dcb.Flags := 4113;


  pbuffer := @buffer;

 {Set the comm port to the new configuration}
  if not SetCommConfig(self.HPipe, buffer, size) then
    raise Exception.Create('Code:'+inttostr(GetLastError));
//    raise exception.create('Unable to set baud rate');

//  FreeMem(buffer);
end;

procedure TSimpleCommPortConnection.SendString(buffer: string);
var
  s: UTF8String;
begin
  inherited;
  s := buffer;
  SendDAta(PByte(PAnsiChar(s)), length(buffer));
end;

function EnumComPorts: IHolder<TStringList>;
var
 MaxPorts   : integer;
 hPort       : THandle;
 PortNumber : integer;
 PortName   : string;
begin
  result := Tholder<TStringList>.create;
  result.o := TStringlist.create;

  MaxPorts := 9;
  case Win32PlatForm of
   VER_PLATFORM_WIN32_NT      : MaxPorts := 256;
   VER_PLATFORM_WIN32_WINDOWS : MaxPorts := 9;
  end;
  for PortNumber := 1 to MaxPorts do
  begin
   if PortNumber > 9 then
    PortName := '\\.\COM' + IntToStr ( PortNumber )  // ask Microsoft why...
   else
    PortName := 'COM' + IntToStr ( PortNumber );
   hPort := CreateFile (PChar ( PortName ),
                        GENERIC_READ or GENERIC_WRITE,
                        0,
                        NIL,
                        OPEN_EXISTING,
                        0,
                        0);
   // note that ports already in use by other apps
   // will *NOT* be detected here
   if not  ( hPort = INVALID_HANDLE_VALUE) then
        result.o.Add  ('COM'+PortNumber.tostring);
   CloseHandle ( hPort );
  end;
end;


function ComPortExists(sPort: string): boolean;
var
  h: IHolder<TStringlist>;
  t: ni;
begin
  h := EnumComPorts;
  result := false;
  for t:= 0 to h.o.count-1 do begin
    result := comparetext(h.o[t],sPOrt) = 0;
    if result then exit;
  end;

end;
end.
