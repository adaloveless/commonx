unit SoundDevice_NULL;

interface

uses
  debug, tickcount, soundtools, typex, soundinterfaces, sysutils, classes, signals, ringbuffer, systemx, windows, orderlyinit, portaudio, synth_functions;


type
  TSoundDevice_NULL = class(TAbstractSoundDevice, ISoundOscillatorRenderer)
  public
    procedure AudioLoop; override;
    procedure SetupWave; override;
    procedure CleanupWave; override;
  end;



implementation

{ TSoundDevice_PortAudio }

procedure TSoundDevice_NULL.AudioLoop;
begin
  inherited;
  //
end;

procedure TSoundDevice_NULL.CleanupWave;
begin
  inherited;
  //
end;

procedure TSoundDevice_NULL.SetupWave;
begin
  inherited;
  //
end;

end.
