unit MidiConsts;

interface


const
  MIDI_NOTE_ON = $90;
  MIDI_NOTE_OFF = $80;
  MIDI_AFTERTOUCH = $a0;
  MIDI_CONTROLLER = $b0;
  MIDI_PATCH = $C0;
  MIDI_CHANNEL_PRESSURE = $d0;
  MIDI_PITCH_BEND = $e0;
  MIDI_SYSTEM = $F0;


function MidiIs3byte(b : byte): boolean;inline;
function MidiIs2Byte(b: byte): boolean;inline;
function MidiIs1Byte(b: byte): boolean;inline;
function MidiIsRealtime(b: byte): boolean;inline;
function MidiIsSysex(b: byte): boolean;inline;
function MidiMessageSize(bStatus: byte): nativeint; inline;
function MidiIsStatus(b: byte): boolean; inline;

implementation


function MidiMessageSize(bStatus: byte): nativeint; inline;
begin
  if MidiIs3byte(bStatus) then exit(3);
  if MidiIs2byte(bStatus) then exit(2);
  if MidiIs1byte(bStatus) then exit(1);
  exit(0);
end;

function MidiIsStatus(b: byte): boolean; inline;
begin
  result := (b and $80) <> 0;
end;

function MidiIs3byte(b : byte): boolean;
begin
  result := (b and $f0) in [$80, $90, $a0, $b0, $e0];
end;

function MidiIs2Byte(b: byte): boolean;
begin
  result := (b and $f0) in [$C0, $D0];
end;

function MidiIs1Byte(b: byte): boolean;
begin
  result := (b in [$f6, $f8, $fa, $fb, $fc, $fe, $ff]);
end;

function MidiIsRealtime(b: byte): boolean;
begin
  result := MidiIs1Byte(b);
end;

function MidiIsSysex(b: byte): boolean;
begin
  result := (b in [$f0, $ff]);
end;


end.
