unit synth_functions;

interface

uses
  typex;

function SineSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;inline;
function CosSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
function SawSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
function SquareSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
function TriangleSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;




implementation



function SineSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
begin
  result := sIn((6.28 * (t / samplerate)) * freq) * amp;
end;

function CosSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
begin
  result := cos((6.28 * (t / samplerate)) * freq) * amp;
end;


function SawSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
begin
  result := (amp * (-1 + (2 * ((t * round(freq) mod samplerate) / samplerate)))
    );
end;

function SquareSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
var
  b: boolean;
begin
  if (round(t * freq) mod samplerate) > samplerate div 2 then
    result := amp
  else
    result := 0 - amp;

end;

function TriangleSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
var
  rWaveLength: integer;
  q1, q2, q3, q4: integer;
  offset: integer;
begin
  rWaveLength := round(samplerate / freq);

  offset := t mod rWaveLength;
  q1 := 0;
  q2 := q1 + (rWaveLength div 4);
  q3 := q1 + (rWaveLength div 2);
  q4 := q3 + (rWaveLength div 4);

  if offset > q4 then begin
    result := -1 + (offset - q4) / (rWaveLength / 4);
  end
  else if offset > q2 then begin
    result := 1 - (offset - q2) / (rWaveLength / 4);
  end
  else
    result := 0 + (offset) / (rWaveLength / 4);

  result := result * amp;

end;


end.
