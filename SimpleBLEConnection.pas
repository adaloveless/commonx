unit SimpleBLEConnection;
//NOTE!:  THIS IS FOR DONGLED CONNECTIONS

{x$DEFINE FORCE_FAILURE}
{x$DEFINE DETAILED_BLE_DEBUGGING}
{$DEFINE SEND_3E}

interface

uses
  debug, classes, SimpleAbstractconnection, SimpleMIDIBase, ringbuffer, SimpleCommPOrt, systemx, typex, stringx.ansi, sysutils, stringx, tickcount, numbers, signals, beeper;


type
  EBLECommunicationError = class(Exception);
  TSimpleBLEMIDIConnection = class(TSimpleMIDIBase)
  private
    Fport: string;
    FincomingLine: ansistring;
    datain: TRingbuffer;
    Fconnected: boolean;
    FEnableSelfLog: boolean;
    FHoldOutput: boolean;
    function GEtPort: string;
    procedure SetPort(const Value: string);
    function LLSendData(buffer: pbyte; length: integer): integer;
  strict protected
    function DoWaitForData(timeout: cardinal): boolean;override;
  protected
    flag_ok: boolean;
    flag_err: boolean;
    outputRing: TRingBuffer;
    underclass: TSimpleCommPortConnection;
    procedure Init;override;
    procedure ResetFlags;

    procedure TX(sString: string);
    destructor Destroy;override;
    function CheckForNewData: boolean;
    function HandleNewData: boolean;

    function GetConnected: boolean;override;//<---------------------------------------------
    function DoReadData(buffer: pbyte; length: integer): integer;override;//<<------------------------------
    function DoSendData(buffer: pbyte; length: integer): integer;override;//<---------------------------------------------

    function Pair(sMac: string): boolean;//
    procedure Handle_Message(sMessage: string; sParams: string);//
    procedure Handle_Connect(sCmd: string; sParams: string);//
    procedure Handle_Err();//
    procedure Handle_Ok();//
    procedure Handle_Dis();
    procedure Handle_ReadHandle(sDataBytes: string);
    procedure Handle_DataIn(sData: string);
    function DoCheckForData: boolean;OVERRIDE;

  public

    noresetonConnect: boolean;
    tmBeginConnectTime: ticker;
    diagbyte: byte;
    gyroquality: byte;
    SelfLog: TStringlist;
    LastTXTime: ticker;
    function WaitForErr(iTimeout: ni = 1000): boolean;
    function WaitForOk(iTimeout: ni = 1000): boolean;

    function CommandReset: boolean;//
    function GetThrottledBytesAllowed: ni;
    procedure Idle;
    function GetDiagByte(out b: byte;out q: byte): boolean;
    PROCEDURE beginREset;
    procedure BeginConnect;
    function IsOk: boolean;
    function DoConnect: boolean; override;//<---------------------------------------------
    procedure DoDisconnect; override;
    constructor Create; override;//<---------------------------------------------
    property Port: string read GEtPort write SetPort;
    property EnableSelfLog: boolean read FEnableSelfLog write FEnableSelfLog;
    procedure Scan;
    property HoldOutput: boolean read FHoldOutput write FHoldOutput;

  end;


implementation

{ TSimpleBLEMIDIConnection }

procedure TSimpleBLEMIDIConnection.BeginConnect;
begin
  flag_ok := false;
  tmBeginConnectTime := GetTicker;
  Pair(Endpoint);
end;

procedure TSimpleBLEMIDIConnection.beginREset;
begin
  flag_ok := false;
{$IFDEF SEND_3E}
    ResetFlags;
    TX('>'#13);
    WaitForErr;
{$ENDIF}

  ResetFlags;
  TX(#13);
  WaitForErr;

  ResetFlags;
  TX('RESET'#13);
  WaitForOK;
end;

function TSimpleBLEMIDIConnection.CheckForNewData: boolean;
var
  buf: array[0..1024] of byte;
  iRead: ni;
begin
  result := false;
  Lock;
  try
    if underclass.WaitForData(1) then begin
      fillmem(@buf[0], sizeof(buf), 0);
      iREad := underclass.ReadData(@buf[0], 1024-1, false);
      if  iRead > 0 then begin
        FIncomingLine := FIncomingLine + pansichar(@buf[0]);
        LastTXTime := getticker;
        Signal(evData,true);
        result := true;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TSimpleBLEMIDIConnection.CommandReset: boolean;
var
  tmStart, tmLast: ticker;
begin
  Lock;
  try
    FConnected := false;
    result := false;
    flag_ok := false;

{$IFDEF SEND_3E}
    ResetFlags;
    TX('>'#13);
    WaitForErr;
{$ENDIF}

    ResetFlags;
    TX(#13);
    WAitForErr;

    ResetFlags;
    TX('RESET'#13);
    WaitForOK;

    tmStart := GetTicker;
    tmLast := tmStart;
    while not flag_ok do begin
      CheckForNewData;
      HandleNewData;
      if getTimeSince(tmlast)> 9000 then begin
        ResetFlags;
        TX(#13);
        WAitForErr;
        ResetFlags;
        TX('RESET'#13);
        WaitForOK;

        tmLast := getticker;
      end;
      if gettimesince(tmStart) > 19000 then begin
        raise EBLECommunicationError.Create('Failed to RESET');
      end;
    end;
    result := true;
  finally
    Unlock;
  end;

  //Sleep(4000);
end;
function TSimpleBLEMIDIConnection.DoConnect: boolean;
var
  tm: ticker;
  tmSince: ticker;
begin
  SelfLog.clear;
  result := false;
  if not noresetonConnect then
    CommandReset;
  beginconnect;
  tm := GetTicker;
  tmSince := GEtTicker;
  while not Fconnected do begin
    if gettimesince(tmSince) > 6000 then begin
      Pair(Endpoint);
      tmSince := GetTicker;
    end;
    if gettimesince(tm) > 12000 then begin
      result := false;
      exit;
    end;

    Lock;
    try
      CheckForNewDAta;
      HandleNewData;
    finally
      Unlock;
    end;
  end;
  result := true;


end;

constructor TSimpleBLEMIDIConnection.Create;
begin
  inherited;
  Debug.Log(CLRE+'Created '+self.ClassName);
end;

destructor TSimpleBLEMIDIConnection.Destroy;
begin
  Debug.Log(CLRE+'Destroying '+self.classname);
  SelfLog.free;
  dataIn.free;
  datain := nil;
  underclass.Free;
  underclass := nil;
  inherited;
end;

procedure TSimpleBLEMIDIConnection.DoDisconnect;
begin
  inherited;
  CommandReset;
end;

function TSimpleBLEMIDIConnection.DoCheckForData: boolean;
begin
  result := CheckForNewData;
end;

function TSimpleBLEMIDIConnection.DoReadData(buffer: pbyte;
  length: integer): integer;
begin
  CheckForData;
  HandleNewData;
//vvvvvvvvvvvvvvvvvvv if you use these "HINTS" for performance purposes, evData might not get unsignaled
//.... I THINK evData must be unsignaled under lock
//  result := lesserof(datain.AvailableDataSize, length);
//  if result > 0 then begin
    datain.Lock;
    try
      result := datain.GetAvailableChunk(buffer, length);
      if (datain.AvailableDataSize=0) and (FincomingLine = '') then
        signal(evData, false);
    finally
      datain.Unlock;
    end;
//  end;

end;


function TSimpleBLEMIDIConnection.LLSendData(buffer: pbyte;
  length: integer): integer;
var
  s: ansistring;
  digit: ansistring;
  t: ni;
  iAllowed: ni;
  iTotalSent: ni;
  iJustSent: ni;
begin
  iAllowed := 0;
  while iAllowed < 4 do begin
    iAllowed := GetThrottledBytesAllowed;
    if iAllowed < 4 then
      sleep(1);
  end;

  iAllowed := lesserof(iAllowed, length);

  setlength(s, iAllowed*2);

  for t:= 0 to iAllowed-1 do begin
    digit := inttohex(buffer[t], 2);
    s[STRZ+(t*2)] := digit[STRZ];
    s[STRZ+(t*2+1)] := digit[STRZ+1];
  end;
  TX('<'+s+'>');
  LastTXTime := getticker;
  result := iAllowed;
end;

function TSimpleBLEMIDIConnection.DoSendData(buffer: pbyte;
  length: integer): integer;
var
  a: array[0..19] of byte;
  iGot: ni;
begin
  result := outputRing.BufferData(buffer, length);
  if (not HoldOutput) or (outputRing.AvailableDataSize >= 12) then begin
    repeat
      iGot := outputRing.GetAvailableChunk(@a[0], lesserof(outputRing.AvailableDataSize, 12));
      LLSendData(@a[0], iGot);
    until (outputRing.AvailableDataSize < 12);
  end;

end;

function TSimpleBLEMIDIConnection.DoWaitForData(timeout: cardinal): boolean;
var
  tm: ticker;
  bNew: boolean;
begin
  if HasLeftovers then begin
    result := true;
  end;
  tm := GetTicker;
  result := false;
  while result = false do begin
    CheckForNewData;
    bNEw := HandleNewData;

    result := datain.AvailableDataSize > 0;
    if timeout > 0 then
      if getTimeSince(tm)> timeout then
        exit;

    if not bNew then
      sleep(3);

  end;
end;



function TSimpleBLEMIDIConnection.GetConnected: boolean;
begin
  result := FConnected;
end;


function TSimpleBLEMIDIConnection.GetDiagByte(out b: byte; out q: byte): boolean;
var
  tmStart: ticker;
begin
  result := false;
  flag_ok := false;
  debug.Log(self, '@ReadHandle');
  TX('READHANDLE[45]'#13);
  debug.Log(self, 'ReadHandle');

  tmStart := getTicker;
  while not flag_ok do begin
    CheckForNewData;
    HandleNewData;
    if not flag_ok then begin
      if gettimesince(tmStart) > 1000 then begin
        raise EBLECommunicationError.Create('Failed to get Diagnostic byte');
      end;
    end;
  end;
  Debug.Log(self,'ReadOK!');

  diagByte := self.diagbyte;
  b := diagByte;
  q := gyroquality;
  result := true;
end;

function TSimpleBLEMIDIConnection.GEtPort: string;
begin
  result := Fport;
end;

function TSimpleBLEMIDIConnection.GetThrottledBytesAllowed: ni;
begin
{$IFNDEF BLE_SLOW}
  result := lesserof(round(GetTimeSince(LAstTxTime) * (3.125)*4), 16);
{$ELSE}
  result := lesserof(round(GetTimeSince(LAstTxTime) * (3.125/4)), 16);
{$ENDIF}
end;

function TSimpleBLEMIDIConnection.HandleNewData: boolean;
var
  sLeft, sRight: ansistring;
  iPos, iPos2: ni;
const
  REALLY_BIG_NUMBER = $7FFFFFFF;
begin
  Lock;
  try
  result := false;
  if FIncomingline = '' then exit;
//  Debug.log(self, 'SLACK = '+FIncomingLine);


  iPos := Zpos(#13#10, FINcomingLine);
  iPos2 := ZPos('>', FIncomingLIne);

  if (iPos < 0) then iPos := REALLY_BIG_NUMBER;
  if (iPos2 < 0) then iPos2 := REALLY_BIG_NUMBER;


//  Debug.Log('FIncomingLine = '+FIncomingLIne+' iPos='+iPos.tostring+' iPos2='+iPos2.ToString);
  if (iPos < iPos2) and (iPos <> REALLY_BIG_NUMBER) then begin
    result := SplitString(FIncomingLine, #13#10, sLeft, sRight);
  end else
  if (iPos2 < iPos) and (iPos2 <> REALLY_BIG_NUMBER) then begin
    result := SplitString(FIncomingLine, '>', sLeft, sRight);
    sLeft := sLeft + '>';
  end;

//  dEBUG.Log('LR='+sLEft + '<>><><<>'+sRight);
  if result then begin
    if enableselflog then begin
      SelfLog.Add('<<'+sLeft);
{$IFDEF DETAILED_BLE_DEBUGGING}
      debug.log(self, '<<'+sLeft);
{$ENDIF}
    end;
    FincomingLine := sRight;
{$IFDEF DETAILED_BLE_DEBUGGING}
    Debug.log(self, sLeft);
{$ENDIF}
    SplitString(sLEft, ' ', sLeft, sRight);
    Handle_Message(sLeft, sRight);
  end;
  finally
    Unlock;
  end;
end;


procedure TSimpleBLEMIDIConnection.Handle_Connect(sCmd, sParams: string);
begin
  Debug.Log(self,'Connect!');
  FConnected := true;
end;

procedure TSimpleBLEMIDIConnection.Handle_DataIn(sData: string);
var
  d: array[0..255] of byte;
  sHex: string;
  t,len: ni;
  c1,c2: byte;
begin
  len := (length(sData) shr 1);
  for t:= 0 to len-1 do begin
    sHex :='$'+sdata[STRZ+(t*2)]+sData[STRZ+((t*2)+1)];
    if charinset(sHex[strz+1],['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'])
    AND charinset(sHex[strz+2],['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']) THEN BEGIN
      d[t] := strtoint(sHex);
    end;
  end;
  datain.BufferData(@d[0], len);

end;

procedure TSimpleBLEMIDIConnection.Handle_Dis;
begin
  FConnected := false;
end;


procedure TSimpleBLEMIDIConnection.Handle_Message(sMessage: string; sParams: string);
var
  sLeft, sright: string;
begin
{$IFDEF DETAILED_BLE_DEBUGGING}
  debug.log(self,'BLE message:'+sMessage);
{$ENDIF}
  if sMEssage = '' then exit;
  if sMessage = 'DIS' then
    Handle_Dis()
  else
  if sMEssage = 'READ' then BEGIN
    handle_ReadHandle(sParams);
  end else
  if sMEssage[STRZ] = '<' then begin
    SplitString(sMEssage, '<', sLeft, sRight);
    splitString(sright, '>', sLeft, sRight);
    Handle_DataIn(sLeft);
  end else
  if sMessage = 'ERR' then
    Handle_Err()
  else
  if sMessage = 'OK' then
    Handle_OK()
  else begin
    SplitString(sMEssage, '[', sLeft, sRight);
    sMEssage := sLeft;
    splitString(sright, ']', sLeft, sRight);
    if sMessage = 'CON' then begin
      Handle_Connect('CON', sLeft);
    end;
  end;

end;

procedure TSimpleBLEMIDIConnection.Handle_Ok;
begin
  flag_ok := true;
end;

procedure TSimpleBLEMIDIConnection.Handle_Err;
begin
  flag_err := true;
end;


procedure TSimpleBLEMIDIConnection.Handle_ReadHandle(sDataBytes: string);
var
  s: string;
begin
  diagByte := $FF;

  s := sDataBytes;

  sDataBytes := zcopy(s,0,2);
  if length(s) < 2 then
    exit;

  diagByte := strtoint('$'+sDataBytes);

  if length(s) < (16*3) then
    exit;

  sDataBytes := zcopy(s,(15*3),2);
  gyroQuality := strtoint('$'+sDataBytes);

  flag_ok := true;

end;

procedure TSimpleBLEMIDIConnection.Idle;
begin
  CheckForNewData;
  HandleNEwData;
end;

procedure TSimpleBLEMIDIConnection.Init;
begin
  inherited;
  datain := TRingBuffer.create;
  underclass := TSimpleCommPortConnection.Create;
  selflog := TStringlist.Create;
  EnableSelfLog := false;
  outputRing := TRingBuffer.create;
  outputRing.Size := 32768;
  PolledByReader := true;
end;

function TSimpleBLEMIDIConnection.IsOk: boolean;
begin
  result := flag_ok;
end;

function TSimpleBLEMIDIConnection.Pair(sMac: string): boolean;
var
  tmStart: ticker;
  sMac2: string;
begin
{$IFDEF FORCE_FAILURE}
  sMac := '000000000000';
{$ENDIF}
  result := false;
  flag_ok := false;
  sMac2 := trim(sMac);
  if zpos(':', sMac2) < 0 then begin
    sMac := zcopy(sMac2, 0, 2)+':'+
            zcopy(sMac2, 2, 2)+':'+
            zcopy(sMac2, 4, 2)+':'+
            zcopy(sMac2, 6, 2)+':'+
            zcopy(sMac2, 8, 2)+':'+
            zcopy(sMac2, 10, 2);

  end;
  TX('PAIR['+sMAc+']'#13);
  tmStart := GetTicker;
  Debug.Log(self,'Pairing');
  while not flag_ok do begin
    CheckForNewData;
    HandleNewData;
    if gettimesince(tmStart) > 24000 then begin
      raise EBLECommunicationError.Create('Failed to pair to '+sMac);
    end;
  end;
  Debug.Log(self,'Pair Started');
  result := true;

end;

procedure TSimpleBLEMIDIConnection.ResetFlags;
begin
  flag_err := false;
  flag_ok := false;
end;

procedure TSimpleBLEMIDIConnection.Scan;
begin
  ResetFlags;
  TX('>'#13);
  WaitForErr;
  ResetFlags;
  TX(#13);
  WaitForErr;
  ResetFlags;
  TX('SCAN'#13);

end;

procedure TSimpleBLEMIDIConnection.SetPort(const Value: string);
begin
  FPort := value;
  underclass.Disconnect;
  underclass.EndPoint := value;
  underclass.Connect;
  underclass.BaudRate := 921600;


end;

procedure TSimpleBLEMIDIConnection.TX(sString: string);
var
  s: ansistring;
  iJustSent, iSent: ni;
  iToSend: ni;
begin
  if not underclass.connected then
    underclass.connect;

  s := ansistring(sString);
{$IFDEF DETAILED_BLE_DEBUGGING}
  debug.log(self,'BLE message TX:'+sString);
{$ENDIF}

  if EnableSelfLog then begin
    SelfLog.add('>>'+sString);
{$IFDEF DETAILED_BLE_DEBUGGING}
    debug.log(self, '>>'+sString);
{$ENDIF}
  end;

  iSent := 0;
  repeat
    iToSend := length(s)-iSent;
    iJustSent := UNDERCLASS.SendData(@s[STRZ+iSent], iToSend, true);
    iSent := iSent + iJustSent;
  until iSent >= length(s);
  LastTXTime := getticker;


end;

function TSimpleBLEMIDIConnection.WaitForErr(iTimeout: ni): boolean;
var
  tm: ticker;
begin
  tm := GEtTicker;
  while not flag_err do begin
    sleep(10);
    CheckForNewDAta;
    HandleNewDAta;
    if gettimesince(tm) > iTimeout then begin
//      beeper.beeparray([900,900],[50,50]);
      exit(false);
    end;
  end;
  exit(true);
end;

function TSimpleBLEMIDIConnection.WaitForOk(iTimeout: ni): boolean;
var
  tm: ticker;
begin
  tm := GEtTicker;
  while not flag_ok do begin
    sleep(10);
    CheckForNewDAta;
    HandleNewDAta;
    if gettimesince(tm) > iTimeout then begin
//      beeper.beeparray([900,900],[50,50]);
      exit(false);
    end;
  end;
  exit(true);
end;

end.
