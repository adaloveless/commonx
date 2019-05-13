unit SoundDevice_Provider;

interface

uses
{$IFDEF MSWINDOWS}
  sounddevice_portaudio,
{$ELSE}
  sounddevice_android,
{$ENDIF}
  orderlyinit, managedthread,
  soundtools;


function GetPlatFormAudioDevice(bStartAutomatically: boolean = true): TAbstractSoundDevice;
procedure FreePLatformAudioDevice(dev: TAbstractSoundDevice);



implementation





function GetPlatFormAudioDevice(bStartAutomatically: boolean = true): TAbstractSoundDevice;
begin
{$IFDEF MSWINDOWS}
  result := TPM.Needthread<TSoundDevice_PortAudio>(nil);
  if bSTartAutomatically then
    result.Start;
{$ELSE}
  result := TPM.Needthread<TSoundDevice_Android>(nil);
  if bSTartAutomatically then
    result.Start;
{$ENDIF}
end;


procedure FreePLatformAudioDevice(dev: TAbstractSoundDevice);
begin
  TPM.NoNeedThread(dev);
end;

procedure oinit;
begin
  //
end;

procedure ofinal;
begin
  //
end;


initialization

orderlyinit.init.RegisterProcs('SoundDevice_Provider', oinit, ofinal, 'ManagedThread');


end.
