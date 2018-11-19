unit synth;

interface

uses
  soundtools, soundinterfaces, typex, systemx, linked_list, math, betterobject, soundsample;

type
  TNote = (
            nC,nCSharp,
            nD,nDSharp,
            nE,
            nF,nFSharp,
            nG,nGSharp,
            nA,nASharp,
            nB
          );

  TMultiOsc = class (TOscillatorObject)
  strict private
    Notes: TDirectlyLinkedList<TOscillatorObject>;//linked list
  public

    constructor Create;override;
    destructor Destroy;override;
    procedure Add(note: TOscillatorObject);
    procedure Remove(note: TOscillatorObject; bFree: boolean = true);
    function FindByTag(tag: cardinal): ToscillatorObject;

    procedure o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64); override;
    function HasOscillators: boolean;
    procedure Clear;
    function CheckReleaseRemove: boolean;



  end;




  TSynthOscProvider = class(TBetterObject)
  public
    function Provide: TSynthOscillatorObject;virtual;
  end;

  TSineOscProvider = class(TSynthOscProvider)
  public
    function Provide: TSynthOscillatorObject;override;
  end;

  TSynth = class(TMultiOsc)
  private
    FProvider: TSynthOscProvider;
    procedure SetProvider(const Value: TSynthOscProvider);
  public
    constructor Create;override;
    class function GetFrequencyForNote(octave: ni; note: TNote): Single;
    procedure NoteOn(octave: ni; note: TNote; vel: nativefloat);
    procedure NoteOff(octave: ni; note: TNote);

    procedure NoteOnMidi(notenumber, vel: ni);
    procedure NoteOffMidi(notenumber, vel: ni);
    property Provider: TSynthOscProvider read FProvider write SetProvider;
  end;







implementation

{ TMultiOsc }

procedure TMultiOsc.Add(note: TOscillatorObject);
begin
  notes.addfirst(note);
end;

function TMultiOsc.CheckReleaseRemove: boolean;
var
  note: TSynthOscillatorObject;
begin
  result := false;
  note := notes.First as TSynthOscillatorObject;
  if note = nil then exit;
  repeat
    if note.ReleaseComplete then begin
      remove(note);
      exit(true)
    end;
    note := note.Next as TSynthOscillatorObject;
  until note = nil;
end;

procedure TMultiOsc.Clear;
begin
  while notes.First<> nil do begin
    Remove(TOscillatorObject(notes.First),true);
  end;
end;

constructor TMultiOsc.Create;
begin
  inherited;
  Notes := TDirectlyLinkedList<TOscillatorObject>.create;
end;

destructor TMultiOsc.Destroy;
begin
  Notes.Clear;
  Notes.free;
  Notes := nil;
  inherited;
end;

function TMultiOsc.FindByTag(tag: cardinal): ToscillatorObject;
var
  note: TSynthOscillatorObject;
begin
  result := nil;
  note := notes.First as TSynthOscillatorObject;
  if note = nil then exit;
  repeat
    if (note.Tag = tag) and (not note.needsrelease) then begin
      result := note;
      exit;
    end;
    note := note.Next as TSynthOscillatorObject;
  until note = nil;
end;

function TMultiOsc.HasOscillators: boolean;
begin
  result := Notes.First <> nil;
end;

procedure TMultiOsc.o(mt: ToscMessageType; out ss: TStereoSoundSample;
  iSampletime: int64);
var
  note: TOscillatorObject;
  s,t: TStereoSoundSample;


begin
  inherited;
  s.init;
  note := TOscillatorObject(notes.First);
  if note = nil then exit;
  repeat
    note.o(mt, t, iSampleTime);
    s := s +t;
    s := s + t;

    note := TOscillatorObject(note.next);
  until note = nil;

  ss := s;
  ss := s;




end;

procedure TMultiOsc.Remove(note: TOscillatorObject; bFree: boolean);
begin
  notes.Remove(note);
  if bFree then
    note.free;

end;

{ TSynth }

constructor TSynth.Create;
begin
  inherited;
  FProvider := TSineOscProvider.create;
end;

class function TSynth.GetFrequencyForNote(octave: ni; note: TNote): Single;
const
  root_octave = 3;
var
  oct: ni;
begin
  result := 65.4 * power(2, ord(note) / 12); // sLog := sLog + '1:'+floattostr(frequencies[1])+#13#10;

  oct := root_octave;
  while oct > octave do begin
    result := result / 2;
    dec(oct);
  end;

  while oct < octave do begin
    result := result * 2;
    inc(oct);
  end;

end;

procedure TSynth.NoteOff(octave: ni; note: TNote);
var
  t: cardinal;
  o: TSynthoscillatorObject;
begin
  t := (octave * 12)+ord(note);
  o := FindByTag(t) as TSynthOscillatorObject;
  IF O<>NIL THEN
    o.NeedsRelease := true;



end;

procedure TSynth.NoteOffMidi(notenumber, vel: ni);
begin
  noteOff((notenumber div 12), TNote(notenumber mod 12));
  while CheckReleaseRemove do ;
end;

procedure TSynth.NoteOn(octave: ni; note: TNote; vel: nativefloat);
var
  s: TSynthOscillatorObject;
begin
  s := Provider.Provide;
  s.masterstream := self.masterstream;
  s.Freq := GetFrequencyForNote(octave, note);
  s.Amp := vel / 3;
  s.tag := (octave * 12)+ord(note);
  Add(s);
end;

procedure TSynth.NoteOnMidi(notenumber, vel: ni);
begin
  while CheckReleaseRemove do ;
  noteOn((notenumber div 12), TNote(notenumber mod 12), vel/127);

end;

procedure TSynth.SetProvider(const Value: TSynthOscProvider);
begin
  FProvider := Value;
end;

{ TSynthOscProvider }

function TSynthOscProvider.Provide: TSynthOscillatorObject;
begin
  result := nil;
end;

{ TSineOscProvider }

function TSineOscProvider.Provide: TSynthOscillatorObject;
begin
  result := TSineWaveOscillator.Create;
end;

end.
