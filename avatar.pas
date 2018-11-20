unit avatar;

interface


uses
  systemx, typex, spritefile, gameobject, gamecomponentscommon;

const
  MAX_POSES = 128;
type

  TAVatarDefinition = class(TPreFab)
  public
    procedure Init;override;

  end;

  TAvatarPose = record
    id: ni;
    def: PPose;
  end;
  PAvatarPose = ^TAvatarPOse;

  TAvatar = class(TSprite)
  strict private

    poses: array[0..MAX_POSES] of TAvatarPose;
    FActivePose: ni;
    procedure SetActivePose(const Value: ni);
  strict protected
    procedure Animate();virtual;abstract;
    procedure ProcessAI();virtual;
  public
    posechangetime: TGameTime;
    poseholdtime: TGameTime;
    procedure Init;override;
    procedure AddPose(id: ni; sName: string);
    property ActivePose: ni read FActivePose write SetActivePose;
    procedure FixedUpdate(deltatime: TGameTime);override;


  end;





implementation

{ TAVatarDefinition }

procedure TAVatarDefinition.Init;
begin
  inherited;

end;

{ TAvatar }

procedure TAvatar.AddPose(id: ni; sName: string);
begin
  poses[id].id := id;
  poses[id].def := self.game.SpriteLibrary.Find(sNAme);
end;

procedure TAvatar.FixedUpdate(deltatime: TGameTime);
var
  tm, exp: TGametime;
begin
  inherited;

  tm := game.timepiece.fixedupdatetime;
  exp := posechangetime+poseholdtime;

  ProcessAI();

  if (tm > exp) then begin
    Animate();
  end;
end;

procedure TAvatar.Init;
begin
  inherited;
  FActivePose := -1;
end;

procedure TAvatar.ProcessAI;
begin
  //optional, override to handle artificial stupidity
end;

procedure TAvatar.SetActivePose(const Value: ni);
begin
  if FActivePose = value then
    exit;
  FActivePose := Value;

  Self.def := poses[value].def;

  posechangetime := game.timepiece.fixedupdatetime;




end;


end.
