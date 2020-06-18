unit DMXComposerBusiness;

interface

uses
  betterobject;


type
  TDMXComposerBusiness = class(TBetterObject)
  public

    //Load some waves
    //playback some waves
    //display some waves

    procedure LoadSoundStream(sFile: string);


    property ActiveSoundStread read FActiveSoundStream write FactiveSoundStream;





  end;

implementation

end.
