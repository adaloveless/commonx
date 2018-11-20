unit dmx_sound_engine;

interface

uses betterobject, windows, sysutils, soundtools, sharedobject;

type
  TDMXSoundEngine = class (TsharedObject)
  protected

  public
    constructor Create;override;
    destructor Destroy;override;


  end;

implementation



{ TDMXSoundEngine }

constructor TDMXSoundEngine.Create;
begin
  inherited;

end;

destructor TDMXSoundEngine.Destroy;
begin

  inherited;
end;

end.
