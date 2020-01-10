unit SimpleMIDIPort;

interface

uses
  numbers, helpers_stream, systemx, debug, tickcount, Midi, ringbuffer, SimpleAbstractConnection, classes, windows, stringx, sysutils, consolelock, typex, simplebufferedconnection, signals, SimpleMIDIBase;


type

  TSimpleMIDIPort = class(TSimpleMIDIBase)
  strict protected
    function DoCheckForData: Boolean; override;
  protected
    rbIn: TRingBuffer;
    m: TMidiInput;
    mo: TMidiOutput;
    bConnected: boolean;
    iOutputDev: integer;
    iInputDev: integer;

    rem_write: nativeint;
    rem_read: nativeint;

    sect: _RTL_CRITICAL_SECTION;
    function GetConnected: boolean;override;
    function DoReadData(buffer: pbyte; length: integer): integer;override;
    function DoWaitForData(timeout: cardinal): boolean;override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CloseMIDI;
    procedure OnMidiInData(const aDeviceIndex: integer; const aStatus, aData1, aData2: byte);
    procedure OnSysExData(const aDeviceIndex: integer; const aStream: TMemoryStream);
    function DoConnect: boolean; OVERRIDE;
    procedure DoDisconnect;override;

    function DoSendData(buffer: pbyte; length: integer): integer;override;
    procedure Flush;override;
    procedure Lock;override;
    procedure Unlock;override;

    procedure MIDI_PitchWheel(iChannel: nativeint; iValue: nativeint);overload;
    procedure MIDI_PitchWheel(iChannel: nativeint; rValue: nativefloat);overload;
    procedure MIDI_SendMessage(data: PByte; iLen: nativeint);
    procedure MIDI_StandardMessage(iChannel: nativeint; iType: nativeint; iData1, iData2: nativeint);
    procedure MIDI_ShortMessage(iChannel: nativeint; iType: nativeint; iData1: nativeint);
    procedure MIDI_NoteOn(iChannel: nativeint; iNote: nativeint; iVel: nativeint);
    procedure MIDI_NoteOff(iChannel: nativeint; iNote: nativeint; iVel: nativeint);
    procedure ProcessIncoming;override;  //does nothing
    procedure EvalDataSignal;
  end;

  TSimpleBufferedMidiConnection = class(TSimpleAbstractConnection)
  public

  end;

function MIDI_HasDevice(sDevice: string): boolean;

implementation

{ TSimpleMIDIPort }


procedure TSimpleMIDIPort.CloseMIDI;
begin
  try
    IF ASSIGNED(m) then
      m.Close;
  except
  end;
  try
    if assigned(mo) then
      mo.close;
  except
  end;
  try
    m.Free;
  except
  end;
  try
    mo.Free;
  except
  end;
  m := nil;
  mo := nil;
end;

function MIDI_HasDevice(sDevice: string): boolean;
var
  inst: integer;
  s1,s2: string;
  iInputDev, iOutputDev: ni;
begin
  ecs(midilock);
  try
    Midi.MidiInputs.ClearDeviceNames;
    Midi.MidiOutputs.ClearDeviceNames;
    Midi.MidiInputs.ScanDevices;
    Midi.MidiOutputs.ScanDevices;
    inst := 0;
    if splitstring(sDevice, '#', s1,s2) then begin
      inst := strtoint(s2);
    end;
    iInputDev := FindDevice(s1, Midi.MidiInputs.DeviceNames,inst);
    iOutputDev := FindDevice(s1, Midi.MidiOutputs.DeviceNames,inst);

    result := (iInputDev>=0) or (iOutputDev>=0);

  finally
    lcs(midilock);
  end;
end;

function TSimpleMIDIPort.DoConnect: boolean;
var
  inst: integer;
  s1,s2: string;
begin
  if Connected then
    exit(true);
  ecs(midilock);
  try
    Midi.MidiInputs.ClearDeviceNames;
    Midi.MidiOutputs.ClearDeviceNames;
    Midi.MidiInputs.ScanDevices;
    Midi.MidiOutputs.ScanDevices;
    inst := 0;
    if splitstring(endpoint, '#', s1,s2) then begin
      inst := strtoint(s2);
    end;
    iInputDev := FindDevice(s1, Midi.MidiInputs.DeviceNames,inst);
    iOutputDev := FindDevice(s1, Midi.MidiOutputs.DeviceNames,inst);

    if iInputDev>=0 then
      m := Midi.MidiInputs.Open(iInputDev);
    if iOutputDev>=0 then
      mo := Midi.MidiOutputs.Open(iOutputDev);


    if (m=nil) or (mo=nil) then begin
      result := false;
      exit;
    end;

    Debug.ConsoleLog('m='+inttohex(nativeuint(pointer(m)),sizeof(pointer)*2));

    m.OnMidiData := self.OnMidiInData;
    m.OnSysExData := self.OnSysExData;

  finally
    lcs(midilock);
  end;
  bConnected := true;
  result := true;
end;

constructor TSimpleMIDIPort.Create;
begin
  inherited;
  InitializeCriticalSection(sect);
  rbIn := TRingBuffer.create;
  rbIn.LogOnAdd := true;



end;

destructor TSimpleMIDIPort.Destroy;
begin
  //m.Free;

  CloseMIDI;
  DeleteCriticalSection(sect);
  rbIn.Free;
  inherited;
end;


procedure TSimpleMIDIPort.DoDisconnect;
begin
  inherited;
  bConnected := false;
  CloseMIDI;

end;

function TSimpleMIDIPort.DoCheckForData: Boolean;
begin
  raise ECritical.create(self.Classname+' is not to be polled.');
end;

function TSimpleMIDIPort.DoReadData(buffer: pbyte; length: integer): integer;
var
  iToRead: integer;
begin
  Lock;
  try
    result := rbIn.GetAvailableChunk(buffer, length);
    if result> 0 then begin
      //debug.consolelog(memorydebugstring(buffer, result));
    end;

    EvalDataSignal;

  finally
    Unlock;
  end;

end;

procedure TSimpleMIDIPort.Flush;
begin
  //implement upon implementing outputs

end;

function TSimpleMIDIPort.GetConnected: boolean;
begin
  result := bConnected;
end;



procedure TSimpleMIDIPort.Lock;
begin
  EnterCriticalSection(sect);
end;

procedure TSimpleMIDIPort.MIDI_NoteOff(iChannel, iNote, iVel: nativeint);
begin
  MIDI_StandardMessage(iChannel, $80, iNote, iVel);
//TODO -cunimplemented: unimplemented block
end;

procedure TSimpleMIDIPort.MIDI_NoteOn(iChannel, iNote, iVel: nativeint);
begin
  MIDI_StandardMessage(iChannel, $90, iNote, iVel);


end;

procedure TSimpleMIDIPort.MIDI_PitchWheel(iChannel: nativeint;
  rValue: nativefloat);
begin
  MIDI_PitchWheel(iChannel, nativeint(round(rValue * $2fff)));
end;

procedure TSimpleMIDIPort.MIDI_SendMessage(data: PByte; iLen: nativeint);
begin
  if data[0] = RunningStatusOut then begin
    SendData(@data[1], iLen-1, true);
  end else begin
    SendData(@data[0], iLen, true);
    RunningStatusOut := data[0];
  end;
end;

procedure TSimpleMIDIPort.MIDI_ShortMessage(iChannel, iType, iData1: nativeint);
var
  a: array[0..1] of byte;
begin
  a[0] := iChannel or iType;
  a[1] := iData1;

  MIDI_SendMessage(@a[0], 2);


end;

procedure TSimpleMIDIPort.MIDI_StandardMessage(iChannel, iType, iData1,
  iData2: nativeint);
var
  a: array[0..2] of byte;
begin
  a[0] := iChannel or iType;
  a[1] := iData1;
  a[2] := iData2;


  MIDI_SendMessage(@a[0], 3);

end;

procedure TSimpleMIDIPort.MIDI_PitchWheel(iChannel, iValue: nativeint);
var
  a: array[0..2] of byte;
begin
  a[0] := $E0 or iChannel;
  a[2] := (iValue shr 7) and 127;
  a[1] := iValue and 127;

  MIDI_SendMessage(@a[0], 3);


end;

procedure TSimpleMIDIPort.OnMidiInData(const aDeviceIndex: integer;
  const aStatus, aData1, aData2: byte);
begin
  Lock;
  try

    rbIn.BufferChar(aStatus);
    rbIn.BufferChar(aData1);
    rbIn.BufferChar(aData2);
    evalDataSignal;
  finally
    Unlock;
  end;

end;


procedure TSimpleMIDIPort.OnSysExData(const aDeviceIndex: integer;
  const aStream: TMemoryStream);
var
  t: integer;
  aa: array [0..2048] of byte;
begin

  Lock;
  try
    //rbIn.BufferChar($F0);
    aStream.Seek(0,0);
    if aStream.Size > 2048 then
      raise ECritical.Create('cannot accept sysex stream-chunks from API > 2048 in size');
    stream_guaranteeread(aStream, @aa[0], lesserof(aStream.Size, 2048));
//    Debug.consoleLog('new sysex stream size:'+inttostr(aStream.size));
//    Debug.consoleLog('sysex stream data:'+memorydebugstring(@aa[0], aStream.size));

    rbIn.Lock;
    try
      for t:= 0 to aStream.Size -1 do begin
        rbIn.BufferChar(aa[t]);
      end;
    finally
      rbIn.Unlock;
    end;

    evalDataSignal;


    //rbIn.BufferChar($FF);
  finally
    Unlock;
  end;
end;




procedure TSimpleMIDIPort.ProcessIncoming;
begin
  inherited;
  //
end;

function TSimpleMIDIPort.DoSendData(buffer: pbyte; length: integer): integer;
var
  ms: TMemoryStream;
begin
  inherited;
  result := 0;
  lock;
  try
    if not connected then
      exit;
//    try
      mo.SendBuf(iOutputDev, buffer, length);
      result := length;
//    except
//      result := 0;
//    end;
  finally
    Unlock;
  end;

//  ms := TMemoryStream.Create;
//  try

//    Stream_GuaranteeWrite(ms, buffer, length);
//    ms.Seek(0, soBeginning);
//    try
//      mo.Send(iOutputDev, buffer[0]);
//    except
//    end;
//    mo.SendSysEx(iOutputDev, ms);
//    result := length;
//  finally
//    ms.Free;
//  end;
end;

procedure TSimpleMIDIPort.Unlock;
begin
  LeaveCriticalSection(sect);
end;

function TSimpleMIDIPort.DoWaitForData(timeout: cardinal): boolean;
var
  tmStart, tmNow: Cardinal;
begin
  tmStart := GetTicker;
  repeat
    result := rbin.IsDataAvailable or HasLeftovers;
    if result then begin
      //Bufferincoming;
      break;
    end;

    result := rbIn.WaitForData(timeout);
    tmNow := GetTicker;
    if GetTimeSince(tmNow, tmStart) > timeout then begin
      result := false;
      break;
    end;
    if Disconnecting then exit(true);


  until false;
  //result := rbIn.WaitForData(timeout);

end;

procedure TSimpleMIDIPort.EvalDataSignal;
begin
  Signal(evdata, rbIn.IsDataAvailable or HasLeftOvers);
end;

end.

