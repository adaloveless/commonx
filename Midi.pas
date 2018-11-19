//****************************************************************************/
//* MIDI device classes by Adrian Meyer
//****************************************************************************/
//* V1.1 Delphi 6 Windows 2000
//****************************************************************************/
//* V1.0 First release with simple MIDI Input/Output
//* V1.1 SysEx Input Event added, refactured error handling
//* V1.2 SysEx Output procedure added, changes sysex input for multiple ports
//****************************************************************************/
//* Homepage: http://www.midimountain.com
//****************************************************************************/
//* If you get a hold of this source you may use it upon your own risk. Please
//* let me know if you have any questions: adrian.meyer@rocketmail.com.
//****************************************************************************/
unit Midi;

{$DEFINE ENABLE_GLOBALS}

interface

uses
  typex, debug, classes, SysUtils, mmsystem, Math, Windows, Contnrs, systemx,orderlyinit, stringx, sharedobject, generics.collections.fixed;

const
  // size of system exclusive buffer
  cSysExBufferSize = 2048;

type
  TSysExBuffer = array[0..cSysExBufferSize] of ansichar;

  TSysExData = class
  private
    fSysExStream: TMemoryStream;
  public
    SysExHeader: TMidiHdr;
    SysExData: TSysExBuffer;
    constructor Create;
    destructor Destroy; override;
    property SysExStream: TMemoryStream read fSysExStream;
  end;


  // event if data is received
  TOnMidiInData = procedure (const aDeviceIndex: integer; const aStatus, aData1, aData2: byte) of object;
  // event of system exclusive data is received
  TOnSysExData = procedure (const aDeviceIndex: integer; const aStream: TMemoryStream) of object;

  EMidiDevices = class(Exception);
  EMidi = class(Exception);


  TMidiDevice = class(TSharedObject)
  private
    FStopped: boolean;
    FMidiResult: MMResult;
    procedure SetMidiResult(const Value: MMResult);
  protected
    procedure RaiseError(sMessage: string);
  public
    handle: THandle;
    deviceIndex: integer;
    sysexdata: TSysexData;
    name: string;
    INSysex: boolean;
    IsInCallback: boolean;
    constructor Create;override;
    destructor Destroy;override;
    procedure DoOpen;virtual;abstract;
    procedure Open;
    procedure Close;
    procedure DoClose;virtual;abstract;
    property Stopped: boolean read FStopped write FStopped;
    property MIDIResult: MMResult read FMIDIResult write SetMidiResult;
  end;

  // base class for MIDI devices
  TMidiDevices<T: TMidiDevice, constructor> = class(TSharedobject)
  private
    FDeviceNames: TStringList;
    FDevices: TList<T>;
    fMidiResult: MMResult;
    FError: boolean;
    FLastError: string;
    FEnableExceptions: boolean;
  protected
  public
    // create the MIDI devices
    constructor Create; override;
    // whack the devices
    destructor Destroy; override;
    // close all devices
    procedure CloseAll;
    // THE devices
    property Devices: TList<T> read fDevices;
    property LastError: string read FLastError write FLastError;
    property Error: boolean read FError write FError;
    property EnableExceptions: boolean read FEnableExceptions write FEnableExceptions;

    procedure ClearDeviceNAmes;
    procedure ScanDevices;virtual;

    function GetInstance(aHandle: integer): T;overload;
    function GetInstance(sName: string; inst: ni):T;overload;
    property DeviceNames: TStringlist read FDeviceNames;

    function Open(idx: ni): T;
  end;



  TMidiInput = class(TMidiDevice)
  private
    fOnSysExData: TOnSysExData;
    fOnMidiData: TOnMidiInData;
  protected
    rsStatus, rsData1, rsData2, rsExpected: byte;
    procedure DoMidiInData(const aDeviceIndex, aMidiData, aTimeSTamp: integer);
    procedure DoSysExData(const aDeviceIndex: integer);
  public
    procedure DoOpen;override;
    procedure DoClose;override;

    property OnMidiData: TOnMidiInData read fOnMidiData write fOnMidiData;
    property OnSysExData: TOnSysExData read fOnSysExData write fOnSysExData;
  end;

  TMidiOutput = class(TMidiDevice)
  protected
    rsStatus, rsData1, rsData2, rsExpected: byte;
  public
    procedure DoOpen;override;
    procedure DoClose;override;

    // send some midi data to the indexed device
    procedure Send(const aDeviceINdex: integer; const aStatus, aData1, aData2: byte);overload;
    procedure Send(const aDeviceINdex: integer; const aData: byte);overload;
    procedure Send(const aDeviceINdex: integer; const aData1, aData2 : byte);overload;

    // send system exclusive data to a device
    procedure SendSysEx(const aDeviceIndex: integer; const aStream: TMemoryStream); overload;
    procedure SendSysEx(const aDeviceIndex: integer; const aString: string); overload;
    procedure SendBuf(const aDeviceINDEX: integer; ptr: PByte; len: nativeint);
  end;


  // MIDI input devices
  TMidiInputs = class(TMidiDevices<TMIDIInput>)
  private
  protected
  public
    // create an input device
    constructor Create; override;
    // what the input devices
    destructor Destroy; override;

    procedure ScanDevices;override;
  end;

  // MIDI output devices
  TMidiOutputs = class(TMidiDevices<TMIDIOutput>)
  private

  protected

  public
    constructor Create; override;
    // open a specific input device

    procedure ScanDevices;override;
  end;

  // convert the stream into xx xx xx xx string
  function SysExStreamToStr(const aStream: TMemoryStream): string;
  // fill the string in a xx xx xx xx into the stream
  procedure StrToSysExStream(const aString: string; const aStream: TMemoryStream);

{$IFDEF ENABLE_GLOBALS}
  // MIDI input devices
  function MidiInputs: TMidiInputs;
  // MIDI output Devices
  function MidiOutputs: TMidiOutputs;
{$ENDIF}

function FindDevice(sLeftMatch: string; device_list: TStringList;instance: integer=0): integer;





var
  midilock: TCLXCriticalSection;
{$IFDEF ENABLE_GLOBALS}
  gMidiInputs: TMidiInputs;
  gMidiOutputs: TMidiOutputs;
{$ENDIF}


implementation



constructor TMidiDevices<T>.Create;
begin
  Inherited Create;
  fDevices := TList<T>.create;
  FDeviceNames:= TStringlist.create;
end;

destructor TMidiDevices<T>.Destroy;
begin
  fDevices.Free;
  fDevices := nil;
  FDeviceNames.free;
  inherited;
end;

function TMidiDevices<T>.GetInstance(aHandle: integer): T;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TMidiDevices<T>.GetInstance(sName: string; inst: ni): T;
var
  t: ni;
  x: ni;
begin
  result := nil;
  x := 0;
  for t:= 0 to FDevices.Count-1 do begin
    result := FDevices[t];
    if CompareText(sName, result.name) = 0 then begin
      if inst=x then begin
        exit;
      end else begin
        result := nil;
        inc(x);
      end;
    end;

  end;

end;

function TMidiDevices<T>.Open(idx: ni): T;
var
  inst: T;
begin
  result := nil;
  try
    inst := T.create;
    inst.deviceIndex := idx;
    inst.name := FDeviceNames[idx];
    inst.Open;
    result := inst;
  except
    on E: Exception do begin
      inst.Free;
      inst := nil;
      raise;
    end;

  end;


end;

{$IFDEF ENABLE_GLOBALS}
function MidiInputs: TMidiInputs;
begin

  if not assigned(gMidiInputs) then
    gMidiInputs := TMidiInputs.Create;
  Result := gMidiInputs;
end;

function MidiOutputs: TMidiOutputs;
begin
  if not assigned(gMidiOutputs) then
    gMidiOutputs := TMidiOutputs.Create;
  Result := gMidiOutputs;
end;
{$ENDIF}

{ TMidiInputs }

procedure midiInCallback(aMidiInHandle: PHMIDIIN; aMsg: UInt; dwInstance: nativeuint; dwPAram1,dwParam2: nativeuint); stdcall;
var
  mi: TMidiInput;
begin
//    Debug.ConsoleLog('dwInstance is '+inttohex(dwInstance, sizeof(dwInstance)*2));
  mi := TMidiinput(pointer(dwInstance));//MidiInputs.GetInstance(aMidiInHandle^);
  mi.IsIncallback := true;
  try
    case aMsg of
      MIM_OPEN:
        begin
          Debug.ConsoleLog('callback for open');
        end;
      MIM_DATA:
        begin
          mi.DoMidiInData(dwInstance, dwParam1, dwParam2);
//          MidiInputs.DoMidiInData(aData, aMidiDAta, aTimeSTamp);
        end;

      MIM_LONGDATA:
        mi.DoSysExData(dwInstance);
    end;
  finally
    mi.IsInCallback := false;
  end;
end;

procedure TMidiDevices<T>.ClearDeviceNames;
begin
  while fDeviceNAmes.Count > 0 do begin
//    TSysExData(FSysExData[0]).free;
    FDeviceNAmes.Delete(0);
  end;

end;



procedure TMidiDevices<T>.CloseAll;
var
  i: integer;
begin
  for i:=0 to fDevices.Count - 1 do
    FDevices[i].Close;
end;

constructor TMidiInputs.Create;
begin
  inherited Create;
  ScanDevices;
end;

constructor TMidiDevice.Create;
begin
  inherited;
  sysexdata := TSysExData.Create;

end;

destructor TMidiDevice.Destroy;
begin
  sysexdata.Free;
  inherited;
end;

procedure TMidiDevice.Open;
var
  lHandle: THandle;
  lSysExData: TSysExData;
  MidiResult: MMResult;
begin
  ECS(midilock);
  try

    DoOpen;
    Stopped := false;

  finally
    LCS(midilock);
  end;
end;


procedure TMidiDevice.RaiseError(sMessage: string);
begin
  raise EMidi.Create(sMEssage);
end;

procedure TMidiInputs.ScanDevices;
var
  i: integer;
  lInCaps: TMidiInCaps;
begin
  inherited;
  for i:=0 to midiInGetNumDevs - 1 do
  begin
    midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));
    FDeviceNames.Add(StrPas(lInCaps.szPname));
  end;
end;


destructor TMidiInputs.Destroy;
begin
  inherited;
end;

{ TMidiOutputs }

procedure TMidiDevices<T>.ScanDevices;
begin
  ClearDeviceNames;
end;

constructor TMidiOutputs.Create;
begin
  inherited Create;
  SCanDevices;
end;


procedure TMidiOutput.Send(const aDeviceINdex: integer; const aStatus,
  aData1, aData2: byte);
var
  lMsg: cardinal;
begin
  // open the device is not open
  if handle = INVALID_HANDLE_VALUE then
    Open;

  lMsg := aStatus + (aData1 * $100) + (aData2 * $10000);
  midiOutShortMsg(handle, lMSG);
end;

procedure TMidiDevice.SetMidiResult(const Value: MMResult);
var
  lError: array[0..MAXERRORLENGTH] of char;
begin
  fMidiResult := Value;
  if fMidiResult <> MMSYSERR_NOERROR then
    if midiInGetErrorText(fMidiResult, @lError, MAXERRORLENGTH) = MMSYSERR_NOERROR then
      RaiseError(StrPas(lError));
end;

procedure TMidiOutput.Send(const aDeviceINdex: integer; const aData: byte);
var
  h: MIDIHDR;
begin
  if aData > 127 then begin
    rsStatus := aData;
    if (aData and $F0) in [$80, $C0] then
      rsExpected := 1
    else
      rsExpected := 2;
    rsData1 := 0;
    rsData2 := 0;
    exit;


  end;

  dec(rsExpected);
  if rsExpected = 1 then
    rsData1 := aData
  else
    rsData2 := aData;


  if rsExpected = 0 then begin
    Send(aDeviceIndex, rsStatus, rsData1, rsData2);
  end;

end;


procedure TMidiOutputs.ScanDevices;
var
  i: integer;
  lOutCaps: TMidiOutCaps;
begin
  inherited;
  for i:=0 to midiOutGetNumDevs - 1 do
  begin
    midiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps));
    fDeviceNAmes.Add(strpas(lOutCaps.szPname));
  end;
end;

procedure TMidiOutput.DoClose;
var
  MidiResult: MMResult;
  bDone: boolean;
begin
  Stopped := true;
  bDone := false;
  while not bDone do begin
    Lock;
    try
      if IsInCallback then begin
        debug.ConsoleLog('in callback');
        continue;
      end;

      if handle = INVALID_HANDLE_VALUE then
        exit;
    	MidiResult := midiOutReset(handle);
  //  	MidiResult := midiOutStop(handle);
  //  	MidiResult := midiOUtReset(handle);
  //  	MidiResult := midiOutUnprepareHeader(handle, @sysexdata.SysExHeader, SizeOf(TMidiHdr));
      MidiResult := midiOutClose(handle);
      debug.ConsoleLog('closed '+self.ClassName+' with handle '+inttostr(handle));
      handle := INVALID_HANDLE_VALUE;
      bDone := true;
    finally
      Unlock;
      if not bDone then
        sleep(100);
    end;
  end;

end;

procedure TMidiOutput.DoOpen;
begin
  Handle := 0;
  MidiResult := midiOutOpen(@Handle, DeviceIndex, 0, 0, CALLBACK_NULL);
  Debug.ConsoleLog('Midi Output opened with handle:'+inttostr(handle));



end;

procedure TMidiOutput.Send(const aDeviceINdex: integer; const aData1,
  aData2: byte);
var
  lMsg: cardinal;
begin
  // open the device is not open
  if handle = INVALID_HANDLE_VALUE then
    Open;

  lMsg := aData1 + (aData2 shl 8);
  MidiResult := midiOutShortMsg(handle, lMSG);

end;

procedure TMidiOutput.SendBuf(const aDeviceINDEX: integer; ptr: PByte;
  len: nativeint);
var
  hdr: TMidiHdr;
  r: nativeint;
begin

  hdr.lpData := PAnsiChar(ptr);
  hdr.dwBufferLength := len;
  hdr.dwflags := 0;

  r := midiOutPrepareHeader(handle, @hdr, sizeof(hdr));
  if r <> 0 then
    RaiseError('MIDI error: '+inttostr(r))
  else begin
    r := midiOutLongMsg(handle, @hdr, sizeof(hdr));
    if r<>MMSYSERR_NOERROR then
      RaiseError('MIDI error: '+inttostr(r));
  end;
  //midiOutUnprepareHeader(GetHandle(aDeviceIndex), @hdr, sizeof(hdr));
end;

procedure TMidiOutput.SendSysEx(const aDeviceIndex: integer;
  const aString: string);
var
  lStream: TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    StrToSysExStream(aString, lStream);
    SendSysEx(aDeviceIndex, lStream);
  finally
    lStream.Free;
    lStream := nil;
  end;
end;

procedure TMidiOutput.SendSysEx(const aDeviceIndex: integer;
  const aStream: TMemoryStream);
var
  lSysExHeader: TMidiHdr;
begin
  aStream.Position := 0;
  lSysExHeader.dwBufferLength := aStream.Size;
  lSysExHeader.lpData := aStream.Memory;
  lSysExHeader.dwFlags := 0;

	MidiResult := midiOutPrepareHeader(handle, @lSysExHeader, SizeOf(TMidiHdr));
  MidiResult := midiOutLongMsg( handle, @lSysExHeader, SizeOf(TMidiHdr));
	MidiResult := midiOutUnprepareHeader(handle, @lSysExHeader, SizeOf(TMidiHdr));
end;

{ TSysExData }

constructor TSysExData.Create;
begin
  SysExHeader.dwBufferLength := cSysExBufferSize;
  SysExHeader.lpData := SysExData;
  fSysExStream := TMemoryStream.Create;
end;

destructor TSysExData.Destroy;
begin
  fSysExStream.Free;
  fSysExStream := nil;
end;

function SysExStreamToStr(const aStream: TMemoryStream): string;
var
  i: integer;
begin
  Result := '';
  aStream.Position := 0;
  for i:=0 to aStream.Size - 1 do
    Result := Result + Format('%.2x ', [ byte(pchar(aStream.Memory)[i]) ]);
end;

procedure StrToSysExStream(const aString: string; const aStream: TMemoryStream);
const
  cHex = '123456789ABCDEF';
var
  i: integer;
  lStr: string;
begin
  lStr := StringReplace(AnsiUpperCase(aString), ' ', '', [rfReplaceAll]);
  aStream.Size := Length(lStr) div 2 - 1;
  aStream.Position := 0;

  for i:=1 to aStream.Size do
    pchar(aStream.Memory)[i-1] :=
      char(AnsiPos(lStr[ i*2 - 1], cHex) shl 4 + AnsiPos(lStr[i*2], cHex));
end;

function FindDevice(sLeftMatch: string; device_list: TStringList;instance: integer=0): integer;
//You can use this to find the index of a MIDI device
//For example if you use search for
// 'JS' you might find JS03020404
// if there are multiple JS devices use can specify JS#0, JS#1 etc// and it will find you the first or second device that starts with 'JS'

var
  t: integer;
  ss: string;
  inst: integer;
begin
  result := -1;
  sLeftMatch := lowercase(sLeftMatch);
  inst := 0;
  ECS(midilock);
  try
    for t:= 0 to device_list.Count-1 do begin
      ss := LowerCase(device_list[t]);
      if zpos(sLeftMatch, ss) = 0 then begin
        if inst=instance then begin
          result := t;
        end else begin
          inc(inst);
        end;
      end;
    end;
  finally
    LCS(midilock);
  end;

end;


procedure oinit;
begin
{$IFDEF ENABLE_GLOBALS}
  gMidiInputs := nil;
  gMidiOutputs := nil;
{$ENDIF}
  ICS(midilock);

end;

procedure ofinal;
begin
  //gMidiInput.Free;
  //gMidiOutput.Free;
  DCS(midilock);


end;

{ TMidiInput }


procedure TMidiInput.DoClose;
var
  MidiResult: MMResult;
begin
  Stopped := true;
  Lock;
  try
    if IsInCallback then begin
      debug.ConsoleLog('in callback');
      exit;
    end;
    if handle = INVALID_HANDLE_VALUE then
      exit;
  	MidiResult := midiInReset(handle);
  	MidiResult := midiInStop(handle);
  	MidiResult := midiInReset(handle);
  	MidiResult := midiInUnprepareHeader(handle, @sysexdata.SysExHeader, SizeOf(TMidiHdr));
    MidiResult := midiInClose(handle);
    debug.ConsoleLog('closed '+self.ClassName+' with handle '+inttostr(handle));
    handle := INVALID_HANDLE_VALUE;

  finally
    Unlock;
  end;

end;

procedure TMidiInput.DoMidiInData(const aDeviceIndex, aMidiData,
  aTimeSTamp: integer);
begin
//  Lock;
  try
    if Stopped then exit;
    if assigned(OnMidiData) then
      OnMidiData(DeviceIndex, aMidiData and $000000FF,
        (aMidiData and $0000FF00) shr 8, (aMidiData and $00FF0000) shr 16);
  finally
//    Unlock;
  end;


end;

procedure TMidiInput.DoSysExData(const aDeviceIndex: integer);
var
  lSysExData: TSysExData;
  t: integer;
  bConcluded: boolean;
begin
//  Debug.ConsoleLog('SysEx!');
  if Stopped then exit;


  lSysExData := sysexdata;
  if lSysExData.SysExHeader.dwBytesRecorded = 0 then Exit;
  bConcluded := false;

  InSysex := true;
  for t:= 0 to lsysexdata.SysExHeader.dwBytesRecorded do begin
    if lsysexdata.sysexdata[t] = ansichar($f7) then
      bConcluded := true;
  end;
  lSysExData.SysExStream.Write(lSysExData.SysExData, lSysExData.SysExHeader.dwBytesRecorded);
  //debug.consolelog('adding SYSEX midi buffer bytes:'+inttostr(lSysExData.SysExHeader.dwBytesRecorded));
  if lSysExData.SysExHeader.dwFlags and MHDR_DONE = MHDR_DONE then
  begin
    lSysExData.SysExStream.Position := 0;
    if assigned(fOnSysExData) then fOnSysExData(aDeviceIndex, lSysExData.SysExStream);
    lSysExData.SysExStream.Clear;
  end;

  lSysExData.SysExHeader.dwBytesRecorded := 0;
  MidiResult := midiInPrepareHeader(handle, @lSysExData.SysExHeader, SizeOf(TMidiHdr));

  MidiResult := midiInAddBuffer(handle, @lSysExData.SysExHeader, SizeOf(TMidiHdr));

  if bConcluded then
    InSysex := false;

end;


procedure TMidiDevice.Close;
var
  MidiResult: MMResult;
begin
  Stopped := true;
  DoClose;


end;

procedure TMidiInput.DoOpen;
begin
  handle := INVALID_HANDLE_VALUE;
  MidiResult := midiinopen(@handle, DeviceIndex, NativeUInt(@midiInCallback), NativeUInt(pointer(self)), CALLBACK_FUNCTION);
  Debug.ConsoleLog('Midi INput @'+inttohex(nativeint(self),sizeof(nativeint)*2)+' opened with handle:'+inttostr(handle));


  SysexData.SysExHeader.dwFlags := 0;
	MidiResult := midiInReset(handle);
  MidiResult := midiInPrepareHeader(handle, @SysexData.SysExHeader, SizeOf(TMidiHdr));
  MidiResult := midiInAddBuffer(handle, @SysexData.SysExHeader, SizeOf(TMidiHdr));
	MidiResult := midiInStart(handle);




end;



initialization
  init.RegisterProcs('Midi', oinit, ofinal);

finalization

end.
