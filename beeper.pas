unit beeper;
{$Message '********************************Compiling beeper'}
{x$DEFINE DISABLE_SOUND}

interface
uses

{$IFDEF MSWINDOWS}
  winapi.windows,
  soundtools_easywindows,
  soundinterfaces,
  {$DEFINE PLATFORM_ALLOWED}
{$ENDIF}
  typex, systemx, soundtools, synth_functions, soundsample;


procedure Beep(freq: integer; duration: integer; attack: integer=10; release: integer=1000);overload;

procedure BeepArray(freq: array of integer; duration: array of integer; attack: integer=10; release: integer=1000);overload;

procedure BeepChord(freq: array of integer; duration: integer; attack: integer=10; release: integer=1000);overload;
procedure BeepArray(var buf: TRawSoundArrayPointerMono; freq: array of integer; duration: array of integer; attack: integer=10; release: integer=1000);overload;
procedure Beep2(var buf: TRawSoundArrayPointerMono; freq: integer; duration: integer; attack: integer=10; release: integer=1000);
procedure BeepChord(var buf: TRawSoundArrayPointerMono; freq: array of integer; duration: integer; attack: integer=10; release: integer=1000);overload;

function GetIntervalFrequency(intv: integer; iRootFrequency: integer = 440): integer;


implementation


function GetIntervalFrequency(intv: integer; iRootFrequency: integer = 440): integer;
var
  rGain: NativeFloat;
  rOct: integer;
  rem: integer;
const
  mults: array [0..11] of NativeFloat = (1, 466.16/440, 493.88/440, 523.25/440, 554.37/440,587.33/440,622.25/440,659.26/440,698.46/440,739.99/440,783.99/440,830.61/440);
begin
  while intv < 0 do begin
    iRootFrequency := iRootFrequency div 2;
    intv := intv + 12;
  end;

  while intv > 12 do begin
    iRootFrequency := iRootFrequency * 2;
    intv := intv - 12;
  end;

  if intv >= 0 then begin
    iRootFrequency := iRootFrequency + (iRootFrequency*(intv div 12));

    rem := intv mod 12;

    result := round((mults[(intv mod 12)]*irootFrequency));
  end else begin
    result := iRootFrequency;
  end;

//  result := round(((intv/12)+1)*iRootFrequency);


end;
procedure Beep2(var buf: TRawSoundArrayPointerMono; freq: integer; duration: integer; attack: integer=10; release: integer=1000);
var
  buflen: integer;
  xt,t: integer;
  amp: NativeFloat;
  ampamp: smallint;
  lo, hi: byte;
  olen: integer;
begin
{$IFNDEF DISABLE_SOUND}
  //calc buffer length
  buflen := round((duration/1000) * 44100);

  olen := length(buf);
  setlength(buf, olen+buflen);


  release := round((release / 1000) * 44100);
  attack := round((attack / 1000) * 44100);


  if attack > buflen then
    attack := buflen div 2;


  if release > buflen then
    release := buflen - attack;



//  (t/YDIMS)*6.28
  for xt:= low(buf)+olen to high(buf) do begin
    t := xt-olen;
    amp := SquareSynth(t, freq, 0.2);

    if t < attack then
      amp := amp * (t/attack);

    if t > (buflen-release) then begin
      amp := amp * ((buflen-t)/release);
    end;

    ampamp := (round(amp * 32767));

    buf[xt] := ampamp
  end;
{$ENDIF}
end;


procedure BeepChord(var buf: TRawSoundArrayPointerMono; freq: array of integer; duration: integer; attack: integer=10; release: integer=1000);overload;
var
  buflen: integer;
  ct,xt,t: integer;

  amp: NativeFloat;
  amp_combined: NativeFloat;
  ampamp: smallint;
  lo, hi: byte;
  olen, len: integer;
  att: NativeFloat;
begin
{$IFNDEF DISABLE_SOUND}
  //calc buffer length
  buflen := round((duration/1000) * 44100);

  olen := length(buf);
  setlength(buf, olen+buflen);
  len := length(buf);

  release := round((release / 1000) * 44100);
  attack := round((attack / 1000) * 44100);


  if attack > buflen then
    attack := buflen div 2;


  if release > buflen then
    release := buflen - attack;



//  (t/YDIMS)*6.28
  for xt:= low(buf)+olen to high(buf) do begin
    amp_combined := 0;
    for ct := low(freq) to high(freq) do begin
      t := xt-olen;

      att := (freq[ct]-100)/1200;
      if att > 0.95 then
        att := 0.95;

      amp := SineSynth(t, freq[ct], 0.15-(0.006*att))+SawSynth(t, freq[ct], 0.075-(0.012*att));

      if t < attack then
        amp := amp * (t/attack);

      if t > (buflen-release) then begin
        amp := amp * ((buflen-t)/release);
      end;

      amp_combined := amp_combined+amp;


    end;
    ampamp := (round(amp_combined * 32767));
    buf[xt] := ampamp
  end;
{$ENDIF}
end;


procedure BeepArray(freq: array of integer; duration: array of integer; attack: integer=10; release: integer=1000);
begin
{$IFDEF PLATFORM_ALLOWED}
  Tcmd_Beep.BeepArray(freq, duration, attack, release);
{$ENDIF}
end;

procedure BeepArray(var buf: TRawSoundArrayPointerMono;  freq: array of integer; duration: array of integer; attack: integer=10; release: integer=1000);
var
  t: integer;
  dur: integer;
  pbuf: soundsample.TRawSoundArrayPOinterMono;
  ilen: integer;
begin
  setlength(buf,0);
  for t:= low(freq) to high(freq) do begin
    if length(freq) <> length(duration) then begin
      dur := duration[0];
    end else begin
      dur := duration[t];
    end;
    pbuf := buf;
    iLen := length(buf);
    Beep2(pbuf, freq[t], dur, attack, release);
    buf := pbuf;


  end;



{$IFNDEF DISABLE_SOUND}
//  soundtools.PlayRawSound44Mono(buf, length(buf)*2);
{$ENDIF}


end;





procedure Beep(freq: integer; duration: integer; attack: integer=10; release: integer=1000);
begin
  {$IFNDEF DISABLE_SOUND}
{$IFDEF PLATFORM_ALLOWED}
  Tcmd_Beep.BeepWait(freq, duration, attack, release, 1);
{$ENDIF}
  {$ENDIF}

end;

procedure BeepChord(freq: array of integer; duration: integer; attack: integer=10; release: integer=1000);
begin
{$IFDEF PLATFORM_ALLOWED}
  Tcmd_Beep.BeepChord(freq, duration, attack, release);
{$ENDIF}


end;

{$Message '********************************Finished Compiling beeper'}
end.
