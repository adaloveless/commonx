unit FormFMXBaseAdvanced;

interface
{x$DEFINE USE_ANON_THREAD}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FrameBusyFMX,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, typex,systemx, guihelpers_fmx, FramBaseFMX,
  FMX.Objects, commandprocessor, FMX.Controls.Presentation, FMX.StdCtrls, AnonCommand, formfmxbase;

type
{$IFNDEF USE_ANON_THREAD}
  TLocalBackground = TAnonymousCommand<boolean>;
{$ELSE}
  TLocalBackground = TAnonymousThread<boolean>;
{$ENDIF}

  TOnCommandFinished = procedure (c: TCommand) of object;
  TOnCommandFinishedThen = reference to procedure (c: Tcommand);


  TfrmFMXBaseAdvanced = class(TfrmFMXBase, IUnknown)
    BusyCircle: TCircle;
    Busy: TArc;
    BusyTimer: TTimer;
    BusyRect: TRectangle;
    procedure BusyTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    bSelfRef: boolean;
    FrefSect: TCLXCriticalsection;
    FRefCount: ni;
    FFreeWithReferences: boolean;

    function _AddRef: Integer;
    function _RefCount: Integer;
    function _Release: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult;
  protected
{$IFDEF USE_ANON_THREAD}
    FWaitingOn: TThread;
    FBackgroundOp: TAnonymousThread<boolean>;
{$ELSE}
    FWaitingOn: Tcommand;
    FBackgroundOp: TAnonymousCommand<boolean>;
{$ENDIF}
    FTakeOwnershipOfbackgroundCommand: boolean;
    InBGOp: boolean;
    FonCommandFinish: TOnCommandFinished;
    FonCommandFinishedThen: TOnCommandFinishedThen;
    procedure ShowFancy(show: boolean);
    procedure ToggleBusy(working: Boolean);
    procedure BeforeDestruction;override;
    { Private declarations }
    function BackgroundOp(AThreadFunc: TFunc<boolean>;
      AOnFinishedProc: TProc<boolean>; AOnErrorProc: TProc<Exception>;
      bAutoDestroy: boolean=true): TLocalBackground;
  protected
    procedure DoUpdateState;virtual;
  public
    detached: boolean;
    fancyAnim: TFramBusyFMX;
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property FreeWithReferences: boolean read FFreeWithReferences write FFreeWithReferences;
    destructor Destroy;override;
    procedure Detach;virtual;
    procedure Kill;virtual;

    //NEW Services Offered by Base Form
    function GetControl<T: TControl>(parent: TControl): T;
{$IFDEF USE_ANON_THREAD}
    procedure WaitForCommand(c: TThread; bTakeOwnership: boolean);overload;
    procedure WaitForCommand(c: TThread; bTakeOwnership: boolean; p: TOnCommandFinished);overload;
{$ELSE}
    procedure WaitForCommand(c: TCommand; bTakeOwnership: boolean);overload;
    procedure WaitForCommand(c: TCommand; bTakeOwnership: boolean; p: TOnCommandFinished);overload;
    procedure WaitForCommand(c: TCommand; bTakeOwnership: boolean; p: TOnCommandFinishedThen);overload;
{$ENDIF}

    property ActiveCommand: TCommand read FWaitingOn;
    procedure UpdateState;


  end;

var
  frmFMXBaseAdvanced: TfrmFMXBaseAdvanced;

implementation

{$R *.fmx}

{ TfrmBase }

function TfrmFMXBaseAdvanced.BackgroundOp(AThreadFunc: TFunc<boolean>;
  AOnFinishedProc: TProc<boolean>; AOnErrorProc: TProc<Exception>;
  bAutoDestroy: boolean): TLocalBackground;
begin
  InBGOp := true;
  FBackgroundOp := TLocalBackground.Create(AthreadFunc,
    procedure (Aresult: boolean)
    begin
      AonFinishedProc(AResult);
      InBGOp := false;
    end,
    procedure (E: Exception)
    begin
      AOnErrorProc(E);
      InBGOp := false;
    end,
    false, false);


  result := nil;
  if not bAutoDestroy then
    result := FBackgroundOp;//<--don't return an autodestroy thread,
                            //it might be dead before this function exists
  WaitForCommand(FBackGroundOp, bAutoDestroy);

end;

procedure TfrmFMXBaseAdvanced.BeforeDestruction;
begin

  Detach;
  if _RefCount > 1 then begin
    FreeWithReferences := true;
    raise EAbort.create('Trying to free '+self.ClassName+' with more than 1 reference');

  end;

  inherited;

end;

procedure TfrmFMXBaseAdvanced.BusyTimerTimer(Sender: TObject);
begin
  Busy.StartAngle:=Busy.StartAngle+10;
  if Busy.StartAngle>359 then
    Busy.StartAngle:=0;

  if FWaitingOn <> nil then begin
{$IFDEF USE_ANON_THREAD}
    if FWaitingOn.Finished then begin
{$ELSE}
   if FWaitingOn.IsComplete then begin
{$ENDIF}
      try
        if assigned(FonCommandFinish) then begin
          FOnCommandFinish(FWaitingOn);
          FonCommandFinish := nil;
        end;
        if assigned(FonCommandFinishedthen) then begin
          FOnCommandFinishedThen(FWaitingOn);
          FonCommandFinishedThen := nil;
        end;
        if FBackgroundOp = FWaitingon then
          FBackgroundOp := nil;

        if FTakeOwnershipOfBackGroundcommand then
          FWaitingOn.free;

        FWaitingOn := nil;

      finally
        ToggleBusy(false);
      end;
    end;
  end;

end;

constructor TfrmFMXBaseAdvanced.Create(AOwner: TComponent);
begin
  ics(Frefsect);
  inherited;

end;

destructor TfrmFMXBaseAdvanced.Destroy;
begin
  if assigned(FWaitingOn) then begin
    if not InBGOp then
      FWaitingOn.WaitFor;
    FWaitingOn.Free;
    FWaitingOn := nil;
  end;

//  BGCmd.WaitForAll;

  dcs(FRefSect);
  inherited;
end;

procedure TfrmFMXBaseAdvanced.Detach;
begin
  detached := true;
end;

procedure TfrmFMXBaseAdvanced.DoUpdateState;
begin
  //no implementation required
end;

procedure TfrmFMXBaseAdvanced.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//  Action := TCloseAction.caFree;
//  YOU DONT WANT THIS, because each form might have a wild pointer pointing to
//  it and you will therefore have to save frmWhatever := nil in each formclose ANYWAY.
//  it is less confusing like this

end;

function TfrmFMXBaseAdvanced.GetControl<T>(parent: TControl): T;
begin
  result := TGuiHelper.control_Getcontrol<T>(parent);
end;

procedure TfrmFMXBaseAdvanced.Kill;
begin
  if bSelfRef then begin
    _Release;
  end else begin
    Destroy;
  end;
end;

function TfrmFMXBaseAdvanced.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TfrmFMXBaseAdvanced.ShowFancy(show: boolean);
begin
  if show then begin
    if fancyAnim <> nil then exit;
    fancyAnim := TframBusyFMX.Create(self);
    fancyAnim.Parent := BusyRect;
    fancyAnim.Align := TAlignLayout.Client;

  end else begin
    fancyAnim.DisposeOf;
    fancyAnim := nil;
  end;

end;

function TfrmFMXBaseAdvanced._AddRef: Integer;
begin
  ecs(FRefSect);
  inc(FrefCount);
  Result := FRefCount;
{$IFDEF MSWINDOWS}
  //if this is the first reference but we haven't given ourselves a reference
  //then, on windows, we need a fake reference to represent the non-interface
  //pointers
  if not bSelfRef then begin
    bSelfRef := true;
    _AddRef;
  end;
{$ENDIF}

  lcs(FRefSect);


end;

function TfrmFMXBaseAdvanced._RefCount: Integer;
begin
  ecs(FRefSect);
  result := FRefCount;
  lcs(FRefSect);

end;

function TfrmFMXBaseAdvanced._Release: Integer;
begin

  ecs(FRefSect);
  dec(FRefCount);
  Result := FRefCount;
  lcs(FRefSect);

  if (Result = 0) and FreeWithReferences then begin
{$IFDEF WINDOWS}
    Destroy;//<--- android has ARC, don't destroy on other platforms
{$ENDIF}
  end;

end;


procedure TfrmFMXBaseAdvanced.ToggleBusy(working: Boolean);
begin
  if working=true then
  begin
    BusyTimer.Enabled:=true;
    BusyCircle.Visible:=true;
    BusyRect.Width := clientwidth;
    BusyRect.height := clientheight;
    BusyRect.Position.x := 0;
    BusyRect.position.y := 0;
    BusyRect.Visible := true;
    BusyRect.bringtofront;
    CenterControl(BusyRect);
    CenterControl(BusyCircle);
    BusyRect.Align := TAlignLayout.client;
    ShowFancy(working);
  end else
  begin
    BusyRect.Visible := false;
    BusyTimer.Enabled:=false;
    BusyCircle.Visible:=false;
    ShowFancy(working);
  end;
  self.Cursor := crDefault;
  UpdateMouseCursor;
  UpdateState;
  self.Invalidate;

end;




procedure TfrmFMXBaseAdvanced.UpdateState;
begin
  DoUpdateState;
end;

procedure TfrmFMXBaseAdvanced.WaitForCommand(c: TCommand; bTakeOwnership: boolean;
  p: TOnCommandFinished);
begin
  FonCommandFinish := p;
  WaitForCommand(c, bTakeOwnerShip);
end;

procedure TfrmFMXBaseAdvanced.WaitForCommand(c: TCommand; bTakeOwnership: boolean; p: TOnCommandFinishedThen);
begin
  FonCommandFinishedThen := p;
  WaitForCommand(c, bTakeOwnerShip);
end;


{$IFDEF USE_ANON_THREAD}
procedure TfrmBase.WaitForCommand(c: TThread; bTakeOwnership: boolean);
begin
  FTakeOwnershipOfbackgroundCommand := btakeOwnership;
  FWaitingOn := c;
  ToggleBusy(true);
end;
{$ELSE}
procedure TfrmFMXBaseAdvanced.WaitForCommand(c: TCommand; bTakeOwnership: boolean);
begin
  FTakeOwnershipOfbackgroundCommand := btakeOwnership;
  FWaitingOn := c;
  ToggleBusy(true);
end;
{$ENDIF}

end.

unit ValorDraw;

interface


uses
  debug, betterobject, advancedgraphics_dx, graphics, numbers, systemx, typex, better_colors, colorconversion, sysutils, D3DX9, pxl.types, tickcount;


const
  PROJECTILE_COUNT = 4000;

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
    procedure Draw(canvas: TDX2D);
  end;

  TProjectile = class(TBetterobject)
  public
    pos: TVector4;
    Velocity: TVector4;
    canvas: TDX2D;
    belowground: boolean;
    c: TColor;
    sz: single;
    procedure Draw;
    procedure init;override;

  end;


  TValorDraw = class(TDX2d)
  private
    procedure Update;
  public
    physicstime: single;
    grav : array[0..7] of TGravityPoint;
    proj: array[0..PROJECTILE_COUNT-1] of TProjectile;
    procedure DoDraw; override;
    procedure LoadTextures; override;
    procedure Init; override;
    procedure UpdatePhysics(rDeltaTime: Cardinal); override;
  end;






implementation



{ TValorDraw }

procedure TValorDraw.DoDraw;
var
  x: integer;
  x1,y1,x2,y2: single;
  c: Tcolor;
  t: integer;
begin
  inherited;
  ClearScreen(0);

  SetIdentityBounds;

  for t := 0 to high(grav) do begin
    grav[t].Draw(self);
  end;

  for t := 0 to PROJECTILE_COUNT-1 do begin
    proj[t].Draw;
  end;




end;

procedure TValorDraw.Init;
var
  t: integer;
begin
  inherited;
  for t := 0 to PROJECTILE_COUNT-1 do begin
    proj[t] := TProjectile.Create;
    proj[t].pos.Init;
    proj[t].Velocity.init;
    proj[t].canvas := self;
   // projectile[t].vx := random(300);
    proj[t].pos.x := random(1920);
    proj[t].pos.y := random(1080);
    proj[t].c := random($FFFFFFFF);
  end;
end;

procedure TValorDraw.LoadTextures;
begin
  inherited;
  LoadTexture('graphics\spark2.png');
  LoadFont('graphics\font.png',2,2);//0


end;

procedure TValorDraw.Update;
begin
  inherited;

end;

procedure TValorDraw.UpdatePhysics(rDeltaTime: Cardinal);
var
  t: integer;
begin
  inherited;
  var deltatimeinseconds: single := rDeltaTime / 1000;
  physicstime := physicstime + deltatimeinseconds;
  var center: pxl.types.TVector4;
  center.Init;
  center.w := 1;
  center.x := ((Self.BoundX2-Self.BoundX1)/2)+Self.BoundX1;
  center.y := ((Self.Boundy2-Self.Boundy1)/2)+Self.Boundy1;
  var radius := 25;
  var speed := 8;
  var size := 800* sin(getticker*0.0005);

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
    grav[t].deltaFrameTimeInSeconds := deltatimeinseconds;

//    grav[t].pos := grav[t].pos * trans;
  end;

  for t := 0 to projectile_COUNT -1 do begin
    var p := proj[t];
    for var u := 0 to High(grav) do begin
      var g := grav[u];
      //each projectile is influenced by each gravity point

      //calculate distance from p->g
      var gravVector := g.pos-p.pos;
      var dist := gravVector.Length;
      gravVector.normalize;
//      if ((t=0) and (u=0)) then Debug.Log(diff.ToString);
      var attraction : single := (1/(dist))*80000*(greaterof(0.0,(lesserof(1.0,0.6+(sin(getticker*0.0005))))));;
      var VacuumVector :=  ((g.VacuumForce*10.0));
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


    p.pos := p.pos + (p.Velocity * deltatimeinSeconds);

  end;

end;

{ TProjectile }

procedure TProjectile.Draw;
begin
  canvas.settexture(0);
  canvas.AlphaOp := aoAdd;
  canvas.Sprite(pos.x,pos.y, c, 0.2, sz);

//  canvas.ResetText;
//  canvas.SetFont(0);
//  canvas.TextColor := c;
//  canvas.TextPosition.X := 0;
//  canvas.TextPosition.y := 0;
//  canvas.TextOffset := D3DXVector3(pos.x,pos.y,0);
//
//  canvas.canvas_Text('o');

end;


procedure TProjectile.init;
begin
  pos.init;
  Velocity.init;
  sz := random(64);
end;

{ TGravityPoint }

procedure TGravityPoint.Draw(canvas: TDX2D);
begin
  exit;
  canvas.ResetText;
  canvas.SetFont(0);
  canvas.TextColor := clWhite;
  canvas.TextPosition.X := 0;
  canvas.TextPosition.y := 0;
  canvas.TextOffset := D3DXVector3(pos.x,pos.y,0);
  canvas.canvas_Text('x');
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
