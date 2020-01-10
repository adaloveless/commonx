unit GameComponentsAudio;

interface

uses
  gameobject, typex, soundinterfaces, soundtools, classes, helpers_stream;


type
  TGCAudioListener = class(TGameComponent)
  public
  end;

  TGCAudioBase = class(TGameComponent)
  private
    FFileName: string;
    o: TSoundStreamOscillator;
    data: TMemoryStream;
    FLoop: boolean;
    FAutoDestroy: boolean;
    Fvol: single;
    procedure SetFileNAme(const Value: string);
    procedure SetLoop(const Value: boolean);
    procedure SetVol(const Value: single);
  public

    procedure Update(deltatime: Double); override;
    procedure Init; override;
    property FileName: string read FFileName write SetFileNAme;
    property Loop: boolean read FLoop write SetLoop;
    procedure OnAwake;override;
    procedure Detach;override;

    property AutoDestroy: boolean read FAutoDestroy write FAutoDestroy;
    property Volume: single read FVol write SetVol;


  end;

  TGCAudioSource = class(TGCAudioBase)
  public
  end;

  TGCAudioTrack = class(TGCAudioBase)
  public
  end;


implementation

{ TGCAudioBase }

procedure TGCAudioBase.Detach;
begin
  if not detached then begin
    if data <> nil then begin
      data.free;
      data := nil;
    end;
    if o <> nil then begin
      o.detachandfree;
      o := nil;
    end;
  end;
  inherited;



end;

procedure TGCAudioBase.Init;
begin
  inherited;
  Fvol := 1.0;
end;

procedure TGCAudioBase.OnAwake;
var
  ss:  TSoundStream;
  s: TStream;
begin
  inherited;
  o := TSoundStreamOscillator.Create;
  s := GameObject.game.EnginePackages.GEtAssetStream(FileName);
  ss := TSoundStream.create(s, '.boog');
  o.GiveStream(ss);
  o.Start;
  o.Volume := self.Volume;
  gameobject.game.EngineAudio.AddOscillator(o);
end;

procedure TGCAudioBase.SetFileNAme(const Value: string);
var
  temps: TStream;
begin
  FFileName := Value;

  temps := gameobject.game.EnginePackages.GEtAssetStream(value);
  try
    data := TMemoryStream.create;
    stream_GuaranteeCopy(temps, data, temps.size);
  finally
    temps.free;
  end;

end;

procedure TGCAudioBase.SetLoop(const Value: boolean);
begin
  FLoop := Value;
end;

procedure TGCAudioBase.SetVol(const Value: single);
begin
  FVol := Value;
  if Assigned(o) then begin
    o.Volume := FVol;
  end;
end;

procedure TGCAudioBase.Update(deltatime: Double);
begin
  inherited;
  if o <> nil then begin
    if not o.started then
      killme := true;
  end;

end;

{ TGCAudioTrack }



end.
