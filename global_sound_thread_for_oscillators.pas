unit global_sound_thread_for_oscillators;

interface

uses
  soundtools, sounddevice_portaudio, managedthread;

var
  GSnd : TsoundDevice_portaudio;

procedure StartSound;
procedure StopSound;

implementation

procedure StartSound;
begin
  GSnd := TPM.Needthread<TsoundDevice_portaudio>(nil);
  Gsnd.Start;
end;

procedure StopSound;
begin
  GSnd.free;
  Gsnd := nil;
end;


initialization
  Gsnd := nil;



end.
