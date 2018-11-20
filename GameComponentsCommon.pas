unit GameComponentsCommon;

interface

uses
  typex, gameobject, pxl.types, geometry, sysutils, classes, stringx, numbers, debug;


type
  TMoveMode = (mmLinear, mmSin);

  TGCCameraJoyMove = class(TGameComponent)
  public
    AllowX: boolean;
    AllowY: boolean;
    AllowZ: boolean;
    moveby: single;
    procedure FixedUpdate(deltatime: TGameTime);override;
    procedure Init;override;
  end;

  TGCDoom = class(TGameComponent)
  public
    IsDoomed: boolean;
    DoomTime: TGameTime;
    procedure OnAwake;override;
    procedure FixedUpdate(deltatime: TGameTime);override;
    procedure Doom(timeFromNow: TGameTime);
  end;

  TGCVelocity = class(TGameComponent)
  public
    vec: TVector3;
    procedure FixedUpdate(deltatime: TGameTime);override;
  end;


  TGCCameraSmoothFollow = class(TGameComponent)
  private
    FTarget: TGameObject;
    vel: TGCVelocity;
    FDAmpening: single;
  public
    weights: TVector3;
    followoffset: TVector3;
    property DampeningWidth: single read FDAmpening write FDampening;
    property Target: TGameObject read FTarget write FTarget;
    procedure FixedUpdate(deltatime: TGameTime);override;
    procedure Init;override;
    procedure Snap;
  end;

  TGCGravity = class(TGameComponent)
  strict protected
    velocity: TGCVelocity;
    procedure ResolveVelocityComponent;
  public
    accellerationVector: TVector3;
    procedure FixedUpdate(deltatime: TGameTime);override;
    procedure OnAwake;override;

  end;

  TGCSceneSkipper = class(TGameComponent)
  public
    procedure FixedUpdate(deltatime: TGAmeTime);override;
  end;

  TGCAttraction = class(TGameComponent)
  public
    AttractTo: TGAmeObject;
    procedure FixedUpdate(deltatime: TGameTime);override;
  end;

  TWayPOint = record
    Pos: TVEctor3;
    Rot: TVector3;
    traveltime: Tgametime;
    movemode: TMoveMode;
    procedure FromString(s: string);
  end;

  TGCWAypointFollower = class(TGameComponent)
  public
    waypoints: array[0..255] of TWayPoint;
    fromwaypoint: TWayPOint;
    waypointcount: ni;
    currentwaypoint: ni;
    movestart: Tgametime;
    procedure OnAwake;override;
    procedure Detach;override;
    procedure FixedUpdate(deltatime: TGameTime);override;
    procedure AddWaypointFromString(s: string);
    procedure ClearWaypoints;
    procedure AddSCriptCommand(s: string);override;
    procedure ClearScripts;override;
  end;


  TMultiObjectScripter = class(TGameObject)
  private
    FScriptFile: string;
  public
    procedure Construct;override;
    procedure OnAwake;override;
    procedure Detach;override;
    procedure DisseminateScripts(sScript: string);
    property ScriptFile: string read FScriptFile write FScriptFile;
  end;

  TGCScripter = class(TGameCOmponent)
  public
    procedure NewScripts(sComp: string);
    procedure AddToScript(sLIne: string; sComponent: string);
  end;

  TPoseChange = record
    def: PPose;
    timetohold: TGameTime;
  end;

  TGCPoseChanger = class(TGameComponent)
  public
    poses: array[0..255] of TPoseChange;
    poseidx: ni;
    posecount: ni;
    posestarttime: TGameTime;
    procedure FixedUpdate(deltatime: TGameTime);override;
    procedure AddSCriptCommand(s: string);override;
    procedure ClearScripts;override;
  end;

  TSpriteCollider = class(TGameComponent)
  protected
  public
  end;

  TGCLightBlink = class(TGameComponent)
  public
    speed: single;
    phase: single;
    procedure Update(dt: TGameTime);override;
    procedure Init;override;
  end;


  TGCReparent = class(TGameComponent)
  public
    waittime: TGameTime;
    toParent: string;
    procedure AddScriptCommand(s: string); override;
    procedure ClearScripts; override;
    procedure FixedUpdate(deltatime: Double); override;
  end;


  TDialogEvent = record
    text: string;
    portrait: string;
    speaker: string;
    delaytime: TGameTime;
    procedure FromString(s: string);
  end;

  TGCDialog = class(TGAmeComponent)
  private
    FDelaytime: TGameTime;
    FPortrait: string;
    FText: string;
    FDialogEvents: array[0..31] of TDialogEvent;
    eventcount: ni;
    eventidx: ni;
    FSpeaker: string;
  public
    procedure AddScriptCommand(s: string); override;
    procedure ClearScripts; override;
    procedure Detach;override;
    procedure OnAwake; override;
    procedure Update(deltatime: Double); override;
    property Text: string read FText write FText;
    property Speaker: string read FSpeaker write FSpeaker;
    property Portrait: string read FPortrait write FPortrait;
    property DelayTime: TGameTime read FDElayTime write FDelaytime;
    function NextScriptEvent: boolean;
    procedure Init;override;
  end;



function MoveMOdeToString(mm: TMoveMOde): string;
function StringToMoveMode(s: string): TMovemode;

implementation

function MoveMOdeToString(mm: TMoveMOde): string;
begin
  case mm of
    mmLInear: exit('L');
    mmSin: exit('S');
  else
    raise ECritical.create('Unknown movemode in MoveModeToSTring');
  end;
end;
function StringToMoveMode(s: string): TMovemode;
begin
  s := uppercase(s);
  if s = 'L' then
    exit(mmLInear);
  if s = 'S' then
    exit(mmSin);

  raise ECritical.create('Unknown move mode "'+s+'"');

end;

{ TGCCameraJoyMove }

procedure TGCCameraJoyMove.FixedUpdate(deltatime: TGameTime);
var
  pos: Tvector4;
  xy: TVector4;
  rot: TVector4;
begin

  inherited;

  pos := gameobject.transform.position;
  if allowx or allowy then begin
    xy.init;
    xy.xy := gameobject.game.input.joy[0].xy;
    if allowx and allowy then begin
      pos.xy := pos.xy + (xy.xy * point2(moveby,moveby));
    end else
    if allowx then begin
      pos.xy := pos.xy + (xy.xy * point2(moveby,0));
    end
    else
    if allowy then begin
      pos.xy := pos.xy + (xy.xy * point2(0,moveby));
    end;
  end;
  if allowz then begin
    pos.z := pos.z + (gameobject.game.input.joy[1].xy.y * moveby);
  end;


  rot := gameobject.transform.RotationInRots;
  rot.y := rot.y + gameobject.game.input.joy[1].xy.x * moveby;

  gameobject.transform.position := pos;
  gameobject.TransForm.RotationInRots := rot;



end;

procedure TGCCameraJoyMove.Init;
begin
  inherited;
  moveby := 1/4;
end;

{ TGCDoom }

procedure TGCDoom.OnAwake;
begin
  inherited;

end;



procedure TGCDoom.Doom(timeFromNow: TGameTime);
begin
  DoomTime := gameobject.game.scene.timepiece.fixedupdatetime + timeFromNow;
  IsDoomed := true;
end;

procedure TGCDoom.FixedUpdate(deltatime: TGameTime);
begin
  inherited;
  if isDoomed then begin
    if gameobject.game.Scene.timepiece.fixedupdatetime > DoomTime then
      gameobject.killme := true;
  end;

end;

{ TGCVelocity }

procedure TGCVelocity.FixedUpdate(deltatime: TGameTime);
var
  v4: TVector4;

begin
  inherited;
  v4 := gameobject.TransForm.position;
  v4 := v4 + (self.vec*(deltatime*100));
  v4.w := 1;
  gameobject.TransForm.position := v4;
end;

{ TGCWAypointFollower }

procedure TGCWAypointFollower.AddSCriptCommand(s: string);
begin
  inherited;
  AddWaypointFromString(s);
end;

procedure TGCWAypointFollower.AddWaypointFromString(s: string);
begin
  waypoints[waypointcount].FromString(s);
  inc(waypointcount);
  if waypointcount = 1 then begin
    fromwaypoint.Pos := gameobject.transform.position.getxyz;
    fromwaypoint.Rot := gameobject.transform.rotationinrots.getxyz;
  end;
end;

procedure TGCWAypointFollower.OnAwake;
begin
  inherited;
//
end;

procedure TGCWAypointFollower.ClearScripts;
begin
  inherited;
  ClearWaypoints;
end;

procedure TGCWAypointFollower.ClearWaypoints;
begin
  waypointcount := 0;
  currentwaypoint := 0;
end;

procedure TGCWAypointFollower.Detach;
begin
  inherited;
  //
end;

procedure TGCWAypointFollower.FixedUpdate;
var
  v1,v2: TVector3;
  r: single;
  f,t: TWAyPOint;
label neednew;
begin
neednew:
  if currentwaypoint >= waypointcount then
    exit;

  f := fromwaypoint;
  t := waypoints[currentwaypoint];
  r := gameobject.game.scene.timepiece.fixedupdatetime-movestart;
  r := r / t.traveltime;

  if r >= 1.0 then begin
    r := 1.0;
    if (currentwaypoint < waypointcount) then begin
      inc(currentwaypoint);
      movestart := gameobject.game.scene.timepiece.fixedupdatetime;
      fromwaypoint := t;
      goto neednew;
    end else
      killme := true;
  end;

  if t.movemode = mmSin then begin
    r := Interpolate_SIN(r, 0, 1, 0,1);
  end;



  f.Pos := f.Pos * VEctor3(1-r,1-r,1-r);
  t.Pos := t.Pos * Vector3(r,r,r);

  self.gameobject.transform.position := f.pos + t.Pos;



end;

{ TWayPOint }

procedure TWayPOint.FromString(s: string);
var
  s1, s2, sPOs, sRot, sMode, sTime: string;
begin
  s2 := s;
  SplitString(s2,';', sPOs, s2);
  SplitString(s2,';', sRot, s2);
  SplitString(s2,';', sTime, s2);
  SplitString(s2,';', sMode, s2);

  pos.FromString(sPos);
  rot.FromString(sRot);
  Self.traveltime := strtofloat(sTime);
  movemode := StringToMoveMode(sMode);

end;

{ TMultiObjectScripter }


procedure TMultiObjectScripter.OnAwake;
var
  s: string;
  ss: TStream;
begin
  inherited;

  s := '';
  ss := game.EnginePackages.GEtAssetStream(SCriptFile);
  try
    s := LoadStreamAsString(ss);

  finally
    ss.free;
    ss := nil;
  end;

  DisseminateScripts(s);
//
end;

procedure TMultiObjectScripter.Construct;
begin
  inherited;

end;

procedure TMultiObjectScripter.Detach;
begin
  inherited;
//
end;

procedure TMultiObjectScripter.DisseminateScripts(sScript: string);
var
  s, sComp,ss: string;
  go: TGameObject;
  so: TGCScripter;
  script: TStringList;
var
  t: ni;
begin
  script := TStringList.create;
  try
    script.text := sScript;
    for t := 0 to script.count-1 do begin
      s := script[t];
      SplitString(s, ';', s,ss);
      SplitString(ss, ';', sComp,ss);
      if CompareTExt(s, 'scene') = 0 then
        go := game.scene
      else
        go := game.scene.findobjectbyname(s);
      if go = nil then
        raise ECritical.create('Game Object Named "'+s+'" not found');

      so := go.FindComponent<TGCSCripter>();
      if so = nil then begin
        so := TGCScripter.create(go);
//        raise ECritical.create('Component not found of object "'+s+'"');
      end;

      so.NewScripts(sComp);
    end;

    for t := 0 to script.count-1 do begin
      s := script[t];
      SplitString(s, ';', s,ss);
      SplitString(ss, ';', sComp,ss);

      if CompareText(s,'scene')= 0 then
        go := game.Scene
      else
        go := game.scene.findobjectbyname(s);
      if go = nil then
        raise ECritical.create('Game Object Named "'+s+'" not found');

      so := go.FindComponent<TGCSCripter>();
      if so = nil then
        raise ECritical.create('Component not found of object "'+s+'"');

      if ss <> '' then
        so.AddToScript(ss, sComp);
    end;
  finally
    script.Free;
  end;

end;

{ TGCScripter }

procedure TGCScripter.AddToScript(sLIne: string; sComponent: string);
var
  wpf: TGAmeComponent;
begin
  wpf := gameobject.findcomponent(sComponent);
  if wpf = nil then begin
    wpf := TGameComponent(compfact.CreateClassByName(sCOmponent));
    wpf.GameObject := self.gameobject;
  end;
  wpf.AddScriptCommand(sLIne);





end;

procedure TGCScripter.NewScripts(sComp: string);
var
  c: TGameCOmponent;
//  cls: TGameCOmponentClass;
begin
  //clear any waypoint followers
//  cls := TGameComponent(compfact.CreateClassByName(sComp));

  c := gameobject.FindComponent(sComp);
  if c <> nil then begin
    c.ClearScripts;
  end;

end;

{ TGCPoseChanger }


procedure TGCPoseChanger.AddSCriptCommand(s: string);
var
  pc: TPoseChange;
  s1,s2: string;
begin
  inherited;

  //this is disseminated from the multi-object scripter, typically
  //add a pose change to the list in the future
  SplitString(s, ';', s1,s2);
  pc.def := GameObject.game.SpriteLibrary.Find(s1);
  pc.timetohold := strtofloat(s2);
  poses[posecount] := pc;
  inc(posecount);


end;

procedure TGCPoseChanger.ClearScripts;
begin
  inherited;
  poseidx := 0;
  posecount := 0;
end;

procedure TGCPoseChanger.FixedUpdate(deltatime: TGameTime);
var
  pc: TPoseChange;
  gt, delt: TGameTime;
  spr: TSprite;
  ws: TWorldSprite;
begin
  inherited;
  pc := poses[Self.poseidx];
  gt := gameobject.game.scene.timepiece.fixedupdatetime;
  delt := gt - posestarttime;
  if delt > pc.timetohold then begin
    posestarttime := posestarttime + pc.timetohold;
    inc(poseidx);
    if poseidx < posecount then begin
      if GameObject is TWorldSprite then begin
        ws := TWorldSprite(gameobject);
        ws.defname := poses[self.poseidx].def.Name;
        //spr.ResolveDefinition;
      end else
      if GameObject is TSprite then begin
        spr := TSprite(gameobject);
        spr.def := poses[self.poseidx].def;
        spr.ResolveDefinition;
      end;
    end;
  end;


end;


{ TGCSceneSkipper }

procedure TGCSceneSkipper.FixedUpdate(deltatime: TGAmeTime);
begin
  inherited;
  if gameobject.game.Input.skip.Pressed then begin
    TGameScene(gameobject).SceneFinish;
  end;
end;

{ TGCGravity }

procedure TGCGravity.OnAwake;
begin
  inherited;
  accellerationVector := Vector3(0,0.03,0);
end;

procedure TGCGravity.FixedUpdate(deltatime: TGameTime);
var
  v4: TVector4;
begin
  inherited;
  ResolveVelocityComponent;
  velocity.vec := velocity.vec + accellerationVector;

end;

procedure TGCGravity.ResolveVelocityComponent;
begin
  if velocity = nil then begin
    velocity := self.GameObject.FindComponent<TGCVelocity>();
    if velocity = nil then begin
      velocity := TGCVelocity.create(self.gameobject);
//      raise ECritical.create('Could not resolve velocity component.');
    end;
  end;
end;

{ TBoxCollider }


{ TGCAttraction }

procedure TGCAttraction.FixedUpdate;
var
  v,vMe: TVEctor4;
  velchange: TVector3;
  gcv: TGCVelocity;
begin
  inherited;
  v := AttractTo.TransForm.WorldPosition;
  vMe := gameobject.Transform.worldposition;

  velchange := Vector3(0,0,0);

  if (vMe.x < v.x) then
    velChange.x := 1;
  if (vMe.x > v.x) then
    velChange.x := -1;
  if (vMe.y < v.y) then
    velChange.y := 1;
  if (vMe.y > v.y) then
    velChange.y := -1;
  if (vMe.z < v.z) then
    velChange.z := 1;
  if (vMe.z > v.z) then
    velChange.z := -1;


  gcv := gameobject.FindCOmponent<TGCVelocity>();

  if gcv = nil then
    gcv := TGCVelocity.create(gameobject);


  velchange := velchange * vector3(DeltaTime,DeltaTime,DeltaTime);
  gcv.vec := gcv.vec + velChange;





end;

{ TGCCameraSmoothFollow }

procedure TGCCameraSmoothFollow.FixedUpdate(deltatime: TGameTime);
var
  v1,v2,v3,vi: TVector4;
  acc: TVector3;
begin
  inherited;


  v1 := self.GameObject.TransForm.WorldPosition;
  v2 := self.Target.transform.WorldPosition;
  v2  := v2 + followoffset;

  vi := vector3(1,1,1)-weights;
//  v3 := (v1*weights)+(v2*vi);
  v3 := (v1*0.99)+(v2*0.01);

  gameobject.TransForm.worldposition := v3;


end;

procedure TGCCameraSmoothFollow.Init;
begin
  inherited;
  Weights := Vector3(0.1,0.1,0.1);
  DampeningWidth := 5;
  followoffset := vector3(0,-32,-100);
end;

procedure TGCCameraSmoothFollow.Snap;
begin
  self.GameObject.transform.WorldPosition := self.Target.TransForm.worldposition + followoffset;
end;

{ TGCLightBlink }

procedure TGCLightBlink.Init;
begin
  inherited;
  phase := random(6230)/1000;
end;

procedure TGCLightBlink.Update(dt: TGameTime);
begin
  if gameobject is TLight then begin
    with gameobject as TLight do begin
      color := vector4((sin((phase+gameobject.game.timepiece.frametime)*speed)+1)*0.5,0,0,1);
    end;
  end;
end;

{ TGCReparent }

procedure TGCReparent.AddScriptCommand(s: string);
var
  sl: TStringlist;
begin
  inherited;
  sl := TStringlist.create;
  try
    ParseString(s, ';', sl);
    waittime := strtofloat(sl[1]);
    toPArent := sl[0];

  finally
    sl.free;
  end;

end;

procedure TGCReparent.ClearScripts;
begin
  inherited;

end;

procedure TGCReparent.FixedUpdate(deltatime: Double);
begin
  inherited;
  if waittime < 0 then
    exit;
  waittime := waittime - deltatime;

  if waittime <= 0 then begin
    Self.GameObject.PArent := self.gameobject.game.Scene.FindObjectByName(toParent);
  end;

end;

{ TGCDialog }

procedure TGCDialog.AddScriptCommand(s: string);
var
  de: TDialogEvent;
begin
  inherited;
  //format for this script: Name;Component;Time;Portrait;Text
  de.FromString(s);
  FDialogEvents[eventcount] := de;
  inc(eventcount);

end;

procedure TGCDialog.ClearScripts;
begin
  inherited;
  eventcount := 0;

end;

procedure TGCDialog.Detach;
var
  t: ni;
  go: TGameObject;
begin
  if not detached then begin
    for t:= gameobject.children.count-1 downto 0  do begin
      go := gameobject.children[0];
      if go is TDialog then begin
        go.killme := true;
      end;
    end;
    inherited;
  end;

end;

procedure TGCDialog.Init;
begin
  inherited;
  eventidx := -1;
end;

function TGCDialog.NextScriptEvent: boolean;
begin
  result := true;
  inc(eventidx);
  if eventidx >= eventcount then begin
    killme := true;
    exit(false);
  end else begin
    text := self.FDialogEvents[eventidx].text;
    Debug.Consolelog(text);
    portrait := self.FDialogEvents[eventidx].portrait;
    delaytime := self.FDialogEvents[eventidx].delaytime;
    speaker := self.FDialogEvents[eventidx].speaker;
  end;

end;

procedure TGCDialog.OnAwake;
begin
  inherited;
//  eventidx := -1;


end;

procedure TGCDialog.Update(deltatime: Double);
var
  dlg: TDialog;
begin
  inherited;

  //handle delays in-between dialogs
  //if there's no dialog then count-down delay time
  if gameobject.game.dialog = nil then begin
    delaytime := delaytime - deltatime;
    if delaytime < 0 then begin
      if NextScriptEvent then begin
        if portrait <> '' then begin
          dlg := TDIalog.Create(gameobject.game);
          dlg.transform.position := vector4(0,0,5);
          dlg.parent := gameobject.game.scene;
          dlg.Portrait := self.Portrait;
          dlg.Text := self.text;
          dlg.Speaker := self.Speaker;
          gameobject.game.dialog := dlg;
        end;
      end;
    end;
  end else
  //if there IS dialog then
  //
  begin
    //do nothing




  end;







end;

{ TDialogEvent }

procedure TDialogEvent.FromString(s: string);
var
  s1,s2: string;
begin
  if SplitString(s, ';', s1,s2) then begin
    delaytime := strtofloat(s1);
    SplitString(s2,';', s1,s2);
    portrait := s1;
    SplitString(s2,';', s1,s2);
    speaker := s1;
    text := s2;
  end;


end;

initialization

CompFact.RegisterClass(pointer(TGCWaypointFollower), TGCWaypointFollower);
CompFact.RegisterClass(pointer(TGCPoseChanger), TGCPoseChanger);
CompFact.RegisterClass(pointer(TGCSceneSkipper), TGCSceneSkipper);
CompFact.RegisterClass(pointer(TGCReparent), TGCReparent);
CompFact.RegisterClass(pointer(TGCDialog), TGCDialog);

end.
