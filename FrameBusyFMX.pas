unit FrameBusyFMX;

interface
{$DEFINE FANCY}

uses
  debug, betterobject, numbers, systemx, typex, pxl.types, tickcount, FramBaseFMX,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, guihelpers_fmx;

const
  PROJECTILE_COUNT = 100;

const
  GRAV_POINT_TEST_CONST : TVector4 = (FX: 0; Fy: 0; Fz: 0; Fw: 0);
  GRAV_POINTS : array [0..7] of TVector4 = (
      (FX: 0;       Fy: -1;     Fz: 0; Fw: 1),
      (FX: 0.666;   Fy: -0.666; Fz: 0; Fw: 1),
      (FX: 1;       Fy: 0;      Fz: 0; Fw: 1),
      (FX: 0.666;   Fy: 0.666;  Fz: 0; Fw: 1),
      (FX: 0;       Fy: 1;      Fz: 0; Fw: 1),
      (FX: -0.666;  Fy: 0.666;  Fz: 0; Fw: 1),
      (FX: -1;      Fy: 0;      Fz: 0; Fw: 1),
      (FX: -0.666;  Fy: -0.666; Fz: 0; Fw: 1));


type
  TGravityPoint = record
    pos: TVector4;
    oldpos: TVector4;
    deltaFrameTimeInSeconds: single;
    function VacuumForce: TVector4;

    procedure Init;
    procedure Draw();
  end;

  TProjectile = record
  public
    pos: TVector4;
    Velocity: TVector4;
    belowground: boolean;
    c: TColor;
    sz: single;
    tracking: TFMXObject;
    procedure Draw;
    procedure init;
  end;

  TAnimStage = (asStop, asIn, asPlay, asOut);

  TframBusyFMX = class(TframeBaseFMX)
    RectBusyAnim: TRectangle;
    tmAnim: TTimer;
    procedure tmAnimTimer(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { Private declarations }
    animstage: TAnimStage;
  public
    { Public declarations }
    starttime: ticker;
    keyframetime: ticker;
    circles: array[0..PROJECTILE_COUNT-1] of TCircle;
    physicstime: single;
    grav : array[0..7] of TGravityPoint;
    proj: array[0..PROJECTILE_COUNT-1] of TProjectile;
    master_fader: single;
    peak_fader: single;

    procedure DoDraw;
    procedure UpdatePhysics(deltaTime: ticker);
    procedure Init;
    constructor Create(AOwner: TComponent); override;
    procedure AnimateFrame(interval: ni);
    procedure AnimateTransitionIn;
    procedure AnimateTransitionOut;
  end;

var
  framBusyFMX: TframBusyFMX;

implementation

{$R *.fmx}

{ TValorDraw }

procedure TframBusyFMX.AnimateFrame(interval: ni);
begin
  if animstage = asSTop then
    exit;
  bringtofront;
  UpdatePhysics(interval);
  DoDraw;
end;

procedure TframBusyFMX.AnimateTransitionIn;
begin
  if not (AnimStage in [asIn, asPlay]) then begin
    peak_fader := 0;
    bringtofront;
    starttime := getticker;
    keyframetime := starttime;
    Visible := true;
    RectBusyAnim.HitTest := true;
    AnimStage := asIn;
  end;
//  debug.log('DO STUFF HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
end;

procedure TframBusyFMX.AnimateTransitionOut;
begin
//    Debug.Log('why are you going to transtion-out from stop state?');
  if not (AnimStage in [asOut, asStop]) then begin
    keyframetime := getticker;
    AnimStage := asOut;
  end;
end;

constructor TframBusyFMX.Create(AOwner: TComponent);
begin
  inherited;
  Init;
end;

procedure TframBusyFMX.DoDraw;
const
  MAX_BG_ALPHA = 240;
var
  x: integer;
  x1,y1,x2,y2: single;
  c: Tcolor;
  t: integer;
begin
  inherited;

  case animstage of
    asStop: REctBusyAnim.Visible := false;
    asIn, asPlay: begin

            RectBusyAnim.visible := true;
            master_fader := lesserof(1.0,gettimesince(keyframetime) / 250);
            if peak_fader < master_fader then
              peak_fader := master_fader;

            var alpha := lesserof(MAX_BG_ALPHA, round(master_fader*255));
            RectBusyAnim.Fill.Color := alpha shl 24;
            if master_fader = 1.0 then
              animStage := asPlay;
            self.Opacity := master_fader;


          end;
    asOut: begin
            RectBusyAnim.visible := true;
            master_fader := peak_fader-lesserof(1.0,gettimesince(keyframetime) / 250);
            master_fader := greaterof(0.0, master_fader);

            var alpha := lesserof(MAX_BG_ALPHA, round(master_fader*255));
            RectBusyAnim.Fill.Color := alpha shl 24;
            if master_fader = 0.0 then begin
              animStage := asStop;
              visible := false;
              REctBusyAnim.Visible := false;
            end;
            self.Opacity := master_fader;
          end;
  end;
//  for t := 0 to high(grav) do begin
//    grav[t].Draw(circles[t]);
//  end;

{$IFDEF FANCY}
  for t := 0 to PROJECTILE_COUNT-1 do begin
    proj[t].Draw;
  end;
{$ENDIF}


end;

procedure TframBusyFMX.FrameResize(Sender: TObject);
begin
  inherited;
  Init;
end;

procedure TframBusyFMX.Init;
var
  t: integer;
begin
  inherited;
  starttime := getticker;
  for t := 0 to PROJECTILE_COUNT-1 do begin
    if circles[t] = nil then
      circles[t] := TCircle.create(self);
    circles[t].parent := RectBusyAnim;
    circles[t].Width := 3;
    circles[t].Height := 3;
    circles[t].Fill.Color := $1F000000+random($FFFFFF);
    circles[t].Stroke.Kind := TBrushKind.None;
    proj[t].init;
    proj[t].pos.Init;
    proj[t].Velocity.init;
    proj[t].tracking := circles[t];
   // projectile[t].vx := random(300);
    proj[t].pos.x := random(1920);
    proj[t].pos.y := random(1080);
    proj[t].c := random($FFFFFFFF);
  end;
end;


procedure TframBusyFMX.tmAnimTimer(Sender: TObject);
begin
  inherited;
  AnimateFrame(tmAnim.Interval);

end;

procedure TframBusyFMX.UpdatePhysics(deltatime: ticker);
var
  t: integer;
const
  TIME_SCALE = 0.1;
begin
  inherited;
  if parent = nil then
    exit;

  self.width :=  Control_GetWidth(self);
  self.height := Control_GetHeight(self);
  {$IFNDEF FANCY}
  exit;
  {$ENDIF}
  var deltatimeinseconds: single := DeltaTime / 1000;
  physicstime := physicstime + (deltatimeinseconds * TIME_SCALE);
  var center: pxl.types.TVector4;
  center.Init;
  center.w := 1;
  center.x := ((Self.width)/2);
  center.y := ((Self.height)/2);
  var radius := 25;
  var speed := 8;
  var size := 100* sin(getticker*0.0005);

  //GRAV_POINTS are constant
  //we need a translation matrix to move them to the center
  var trans := TranslateMtx4(center);

  //we need a rotate matrix to rotate
  var rotate := RotateMtx4(Vector3(0,0,1), speed*physicstime);

  //we need a scale matrix to make it bigger or smaller
  var scale := ScaleMtx4(Vector3(size, size, size));

  //build the composite matrix, first scale, then rotate, then translate
  var composite := (scale * rotate) * trans;

  //ram all the points through the composite matrix
  for t:= 0 to high(GRAV_POINTS) do begin
    grav[t].oldpos := grav[t].pos;
    grav[t].pos := GRAV_POINTS[t] * composite;
    if grav[t].deltaFrameTimeInSeconds = 0 then begin
      grav[t].oldpos := grav[t].pos;
    end;
    grav[t].deltaFrameTimeInSeconds := deltatimeinseconds*TIME_SCALE;

//    grav[t].pos := grav[t].pos * trans;
  end;

  for t := 0 to projectile_COUNT -1 do begin
    var p := proj[t];
    for var u := 0 to High(grav) do begin
      var g := grav[u];
      //each projectile is influenced by each gravity point

      //calculate distance from p->g
      var gravVector: TVector4;
      gravVector := g.pos-p.pos;
      var dist := gravVector.Length;
      gravVector.normalize;
//      if ((t=0) and (u=0)) then Debug.Log(diff.ToString);
      var attraction : single := (1/(dist))*80000*(greaterof(0.05,(lesserof(1.0,0.6+(sin(getticker*0.0005))))));;
      var VacuumVector :=  ((g.VacuumForce*10.0));
      attraction := attraction * master_fader;
      p.Velocity := p.Velocity + (gravVector * attraction);
      p.Velocity := p.Velocity + (VacuumVector * attraction);
      var range: integer := 64;
      var minus: integer := 32;
      var rnd: TVector4 := Vector4(random(range)-minus, random(range)-minus, 0.0, 0.0);
      p.velocity := p.velocity + rnd;


    end;
    var terminal: single := 1000.0;
    if p.velocity.Length > terminal then
        p.velocity := p.Velocity / (p.velocity.Length / terminal);

    if p.velocity.Length < 0-terminal then
        p.velocity := p.Velocity / (p.velocity.Length / (0-terminal));


    p.pos := p.pos + (p.Velocity * deltatimeinSeconds*TIME_SCALE);
    proj[t] := p;
  end;

end;

{ TProjectile }

procedure TProjectile.Draw;
const
  circle_size = 64;
begin
  if self.tracking is TCircle then begin
    TCircle(self.tracking).Position.X := self.pos.x-(circle_size shr 1);
    TCircle(self.tracking).Position.y := self.pos.y-(circle_size shr 1);
    TCircle(self.tracking).size.Width := circle_size;
    TCircle(self.tracking).size.height := circle_size;


  end;

end;


procedure TProjectile.init;
begin
  pos.init;
  Velocity.init;
  sz := random(64);
end;

{ TGravityPoint }

procedure TGravityPoint.Draw();
begin
  exit;
end;

procedure TGravityPoint.Init;
begin
  pos.Init;
end;

function TGravityPoint.VacuumForce: TVector4;
begin
  result := (pos-oldpos) *Self.deltaFrameTimeInSeconds;
  result.w := 0.0;



end;

end.
