unit GenericTimeLine;

interface

uses
  tickcount, typex,windows, beeper,dialogs,messages, sysutils, classes, advancedgraphics_DX, advancedgraphics, graphics, forms,
  generics.Collections.fixed, betterobject, ColorConversion, easyimage, numbers, colorblending,
  stringx, systemx, controls, types, direct3d9_jedi, generics.defaults, geometry, debug;


const
  TIMELINE_BAR_TOP = 0.40;
  TIMELINE_BAR_BOTTOM = 0.60;
  POINT_OUTER_Y_RADIUS = 0.4;
  POINT_OUTER_Y_MARGIN = 0.1;
  POINT_INNER_Y_MARGIN = 0.15;
  POINT_OUTER_TOP = 0 + POINT_OUTER_Y_MARGIN;
  POINT_INNER_TOP = 0 + POINT_INNER_Y_MARGIN;
  POINT_OUTER_BOTTOM = 1 - POINT_OUTER_Y_MARGIN;
  POINT_INNER_BOTTOM = 1 - POINT_INNER_Y_MARGIN;
  POINT_OUTER_X_RADIUS = 12;
  POINT_INNER_X_RADIUS = 8;
  POINT_GLOW_SCALE = 20;
  POINT_GLOW_YSCALE = 6;
  BUTTON_COLORS : array [0..4] of TColor = (clLime,clRed,clYellow,clBlue, clorange);

type
  TmouseOpmode = (momNone, momDragScroll, momPoint, momDragSelect,
    momMovePoint);

  TTimelineCommand = (tlcStartloop, tlcStopLoop, tlcBankSetChange, tlcPoint, tlcOther);

  TTimeLine = class; // forward
  TTimeLineLine = class;//forward

  TPerformanceLine = record
    Time: NativeFloat;
    Command: string;
    Param1: NativeFloat;
    Param2: NativeFloat;
  end;


  TTimeLineElement = class(TBetterObject)
  private
    FParent: TTimeLine;
    FYoffset: NativeFloat;
  protected
  public
    procedure Draw1(var yOffset: NativeFloat); virtual;
    procedure Draw2; virtual;
    procedure Draw3; virtual;
    procedure Draw4; virtual;
    procedure Draw5; virtual;

    property Parent: TTimeLine read FParent write FParent;
    property yOffset: NativeFloat read FYoffset write FYoffset;
  end;

  TTimeLineDivider = class(TTimeLineElement)
  private
    FParent: TTimeLine;
    FBGColor: TColor;
  protected
  public
    procedure Draw1(var yOffset: NativeFloat); override;

    property BGColor: TColor read FBGColor write FBGColor;
  end;


  TTimeLinePoint = class(TBetterObject, IComparable)
  strict private
    FTime: NativeFloat;
    FOriginalTime: NativeFloat;
    FParam2: NativeFloat;
    FParam1: NativeFloat;
    FCommand: string;

    Fboolval: tribool;
    FSelected: boolean;
    FLine: TTimelineline;
    procedure SetSelected(const Value: boolean);
    procedure SetCommand(const Value: string);
  private
    FeCommand: TTimeLineCommand;
    FPriority: boolean;
  public
    GlowNumber: integer;
    Data: string;
    property Line: TTimelineline  read FLine write FLine;
    property Time: NativeFloat read FTime write FTime;
    property Command: string read FCommand write SetCommand;
    property eCommand: TTimeLineCommand read FeCommand write FeCommand;
    property BoolVal: tribool read Fboolval write Fboolval;
    property Param1: NativeFloat read FParam1 write FParam1;
    property Param2: NativeFloat read FParam2 write FParam2;
    procedure CopyFromPerformanceLine(var pl: TPerformanceLine);
    property Selected: boolean read FSelected write SetSelected;
    procedure MoveRelative(rBy: NativeFloat; bCommit: boolean);
    procedure ApplySnapping;
    function CompareTo(Obj: TObject): Integer;
    function ExporttoString: string;
    property Priority: boolean read FPriority write FPriority;
  end;

  TTimelineParticle = class(TbetterObject)
  public
    Gravity: NativeFloat;
    VX, VY, VS, Alpha: NativeFloat;
    X,Y,S: NativeFloat;
    Created: cardinal;
    LastUpdateTime: cardinal;
    LifeTime: cardinal;
    Particlenumber: integer;

    procedure Update(time: cardinal);


  end;
  TTimeLineSplosion = class(TTimeLIneElement)
  private
    ScreenX, ScreenY: NativeFloat;
    const FLASHPOT_BASE_RADIUS = 128;
    const PARTICLE_BASE_RADIUS = 32;
    const PARTICLE_COUNT = 30;
  public

    Fparticles:  TList<TTimelineParticle>;
    Rate: NativeFloat;
    Created: cardinal;
    GlobalX, GlobalY: NativeFloat;
    LifeTime: cardinal;
    FlashPotScale: NativeFloat;
    particlecolor: TColor;

    constructor Create;override;
    destructor Destroy;override;

    procedure Draw1(var yOffset: NativeFloat); override;
    procedure Draw2; override;
    procedure Draw3; override;
    procedure Draw4; override;
    procedure Draw5; override;

    procedure InitParticles;
    function UpdateParticles: boolean;
    procedure DrawParticles;



  end;

  TTimeLineLine = class(TTimeLineElement)
  private
    FColor: TColor;
    FBGColor: TColor;
    FGroupID: integer;
    FGroupOF: integer;
    Fpoints: TList<TTimeLinePoint>;
    FTimeLine: TTimeLine;
    cached_point_search_boundx1: NativeFloat;
    cached_point_search_idx: integer;
    FTag: integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    property TimeLine: TTimeLine read FTimeLine write FTimeLine;
    property color: TColor read FColor write FColor;
    property BGColor: TColor read FBGColor write FBGColor;
    procedure Draw1(var yOffset: NativeFloat); override;
    procedure Draw2; override;
    procedure Draw3; override;

    property GroupOf: integer read FGroupOF write FGroupOF;
    property GroupID: integer read FGroupID write FGroupID;

    procedure AddForeign(p: TTimelinePOint);
    function AddPoint: TTimeLinePoint;
    procedure ClearPoints;
    procedure Clearforeign;
    function PointCount: integer;
    procedure SortPoints;
    function GetFirstPointInWindow(out idx: integer): boolean;
    function GetboolState(rPosition: NativeFloat; var iEffectiveIndex: integer)
      : boolean;

    procedure ClearSelectedPoints;
    procedure SelectPointsInRange(rStart: NativeFloat; rEnd: NativeFloat; bSelected: boolean);
    procedure MoveSelectedPoints(rBy: NativeFloat; bCommit: boolean);
    property Tag: integer read FTag write FTag;
    function FindOrphanedPoints: NativeFloat;

    function FindFirstPointOftype(sCommandType: string): TTimeLinePOint;
    procedure ClearPointsOfType(sCommandType: string);
    procedure Quantize;

    function GetMostRecentevent(rTime: NativeFloat; sType: string): TTimeLinePoint;


  end;

  TTimeLinePointcomparer = class(Tcomparer<TTimeLinePoint>)
  public
    function Compare(const Left, Right: TTimeLinePoint): Integer; override;
  end;

  TTimeLine = class(TDX2D)
  private
    FLInes: TList<TTimeLineLine>;
    Felements: TList<TTimeLineElement>;
    FPosition: NativeFloat;
    slHeader: TStringList;
    slfooter: TStringList;
    originalbound1: TNativeFloatPOint;
    originalbound2: TNativeFloatPOint;
    FTexturesLoaded: boolean;
    FSnap: NativeFloat;
    FGlobalEvents: TTimelineline;
    FFileName: string;
    FTempo: NativeFloat;
    FlockUI: boolean;
    procedure SetPosition(const Value: NativeFloat);
    function GetTimeLineLIne(idx: integer): TTimeLineLine;
    procedure StartDrag(var DragObject: TDragObject);
  public
    mouse_down_at: TPoint;
    mouse_last_pos: TPoint;
    mouse_op_mode: TmouseOpmode;
    mouse_last_buttons: integer;

    procedure LoadTextures;override;
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    procedure DoDraw; override;
    property Lines[idx: integer]: TTimeLineLine read GetTimeLineLIne;
    function LineCount: integer;
    function AddLIne: TTimeLineLine;
    function AddDivider: TTimeLineDivider;
    procedure ClearElements;

    property Position: NativeFloat read FPosition write SetPosition;
    procedure LoadFromfile(sFile: string);
    procedure SaveToFile(sFile: string);
    procedure AddFromPerformanceLine(var pl: TPerformanceLine);
    procedure ClearData;
    procedure ClearLines;

    procedure Click(); override;
    procedure CMMouseLeave(var AMsg: TMessage); message CM_MOUSELEAVE;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;

    procedure MouseUp(SButton: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;

    function MouseMovedX: integer;
    function MouseMovedY: integer;
    procedure Resize; override;

    property TexturesLoaded: boolean read FTexturesLoaded;

    procedure ClearSelectedPoints;
    function GetTimeLineLineAtScreenPoint(xScreen, yScreen: integer): TTimeLIneLine;
    function GetTimeLinePointOnScreen(xScreen, yScreen: integer)
      : TTimeLinePoint;
    procedure CommitDragSelect;
    procedure MoveSelectedPoints(rBy: NativeFloat; bCommit: boolean);
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;  MousePos: TPoint): Boolean; override;

    property Snap: NativeFloat read FSnap write FSnap;
    function CanFocus: boolean;override;
    procedure setfocus;override;

    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;

    procedure DblClick;override;
    procedure KeyDown(var Key: Word; Shift: TShiftState);override;
    procedure CreateExplosion(xScreen, yScreen: integer; c:Tcolor; bCreateParticles: boolean = true; rFlashpotScale: NativeFloat = 1.0);
    procedure DeleteSelectedPoints;
    procedure FindOrphanedPoints;
    procedure ScrollIntoview(rtime: NativeFloat);
    procedure CenterViewOnTime(rtime: NativeFloat);

    function GetFinalPoint: TTimelinePoint;
    procedure Conclude;
    function AddPoint(rTime: NativeFloat; iLine: integer; Command: TTimeLIneCommand): TTimeLinePOint;

    procedure SetFadeOutTime(rPosition: NativeFloat);
    procedure InsertBanksetChange(iBankSet: integer; rPosition: NativeFloat);
    property FileName: string read FFileName write FFileName;
    procedure Quantize;
    function GetEffectiveBankset(rTime: NativeFloat): integer;
    procedure DeletemostRecentBanksetChange;
    function SamplesToBeats(i: int64): NativeFloat;
    function BeatsToSamples(i: NativeFloat): int64;
    property Tempo: NativeFloat read FTempo write FTempo;
    property LockUI: boolean read FlockUI write FLockUI;
    function BeatsToSeconds(r: NativeFloat): NativeFloat;
    function SecondsToBeats(r: NativeFloat): NativeFloat;
  end;

function ParsePerformanceLine(s: string): TPerformanceLine;

implementation

{ TTimeLine }

function TTimeLine.AddDivider: TTimeLineDivider;
var
  l: TTimeLineDivider;
begin
  l := TTimeLineDivider.Create;
  Felements.add(l);
  result := l;
  result.Parent := self;
end;

procedure TTimeLine.AddFromPerformanceLine(var pl: TPerformanceLine);
var
  p: TTimeLinePoint;
begin
  if pl.command = 'StrumDown' then
    exit;

  if (pl.Command = 'StartLoop') or (pl.Command = 'StopLoop') then begin
    p := self.Lines[((round(pl.Param1) * 5) mod 25) + round(pl.Param2)]
      .AddPoint;
    p.CopyFromPerformanceLine(pl);
    p.Param1 := round(p.Param1) mod 5; //<--make sure bank number is 0-4.. we'll fix apply bankset changes on save

  end else begin
    p := FGlobalEvents.AddPoint;

    p.CopyFromPerformanceLIne(pl);

  end;

end;

function TTimeLine.AddLIne: TTimeLineLine;
var
  l: TTimeLineLine;
begin
  l := TTimeLineLine.Create;

  l.Tag  := FLInes.add(l);
  Felements.add(l);
  result := l;
  result.TimeLine := self;
  result.Parent := self;
  self.TargetBoundY2 := self.FElements.count;


end;

function TTimeLine.AddPoint(rTime: NativeFloat; iLine: integer; Command: TTimeLineCommand): TTimeLinePoint;
var
  p: tTimeLInePoint;
  tll: TTimelineline;
begin
  result := nil;
  if LockUI then exit;

  tll := FLines[iLine];
  p := tll.AddPoint;
  p.eCommand := command;

  p.Time := rTime;

  p.Param1 := tll.Tag div 5;
  p.Param2 := tll.Tag mod 5;
  p.applysnapping;

  tll.SortPoints;

  CreateExplosion(round(globaltoscreenx(p.time)), round(globaltoscreeny(tll.yoffset+0.5)), tll.color);
  result := p;

end;

function TTimeLine.CanFocus: boolean;
begin
  result := Inherited CanFocus;
end;

procedure TTimeLine.CenterViewOnTime(rtime: NativeFloat);
var
  xdim: NativeFloat;
begin
  xdim := TargetBoundX2 - TargetBoundX1;

  TargetBoundX1 := rTime- (xdim/2);
  TargetBoundX2 := TargetBoundX1 + (xdim);

end;

procedure TTimeLine.ClearData;
var
  t: integer;
begin
  FglobalEvents.ClearPoints;

  for t := 0 to LineCount - 1 do begin
    self.Lines[t].ClearPoints;
  end;

  dirty := true;

end;

procedure TTimeLine.ClearElements;
begin
  while Felements.Count > 0 do begin
    Felements[0].Free;
    Felements.Delete(0);
  end;

  FLines.clear;

  FGlobalEvents.ClearPoints;

end;

procedure TTimeLine.ClearLines;
begin
  ClearElements;


end;

procedure TTimeLine.ClearSelectedPoints;
var
  t: integer;
begin
  for t := 0 to FLInes.Count - 1 do begin
    FLInes[t].ClearSelectedPoints;
  end;

end;

procedure TTimeLine.Click;
begin
  inherited;

end;

procedure TTimeLine.CMMouseLeave(var AMsg: TMessage);
begin
  if mouse_op_mode <> momNone then begin
    mouseup(mbLeft, [], -1,-1);
    mouseup(mbRight, [], -1,-1);
    mouseup(mbMiddle, [], -1,-1);
  end;
end;

procedure TTimeLine.CMMouseWheel(var Message: TCMMouseWheel);
var
  pt: TPoint;
begin
  with Message do
  begin
    Result := 0;
    pt.x := pos.x;
    pt.y := pos.y;
    if DoMouseWheel(ShiftState, WheelDelta, pt) then
      Message.Result := 1
    else if Parent <> nil then
{$IF DEFINED(CLR)}
      with UnwrapMessage(Message) do
{$ELSE}
      with TMessage(Message) do
{$IFEND}
        Result := Parent.Perform(CM_MOUSEWHEEL, WParam, LParam);
  end;

end;

procedure TTimeLine.CommitDragSelect;
var
  ystart, yFinish, xstart, xend: Tdxfloat;
  t, u: integer;
begin
  ystart := mouse_down_at.Y;
  yFinish := mouse_last_pos.Y;
  order(ystart, yFinish);

  ystart := trunc(ScreenToGlobalY(ystart));
  yFinish := trunc(ScreenToGlobalY(yFinish));

  for u := 0 to FLInes.Count - 1 do begin
    if FLInes[u].yOffset < ystart then
      continue;

    if FLInes[u].yOffset > yFinish then
      break;

    xstart := screentoglobalx(mouse_down_at.X);
    xend := screentoglobalX(mouse_last_pos.X);
    order(xstart, xend);
    FLInes[u].SelectPointsInRange(xstart,
      xend, true);
  end;

// for each timeline in range
end;

procedure TTimeLine.Conclude;
var
  tll: TTimelineline;
  tlp,ltlp: TTimeLinePoint;
  u: integer;
begin
  tlp := GetFinalPoint;

  ScrollIntoView(tlp.time);

  if tlp = nil then exit;

  for u := 0 to fLInes.count-1 do begin
    if FLInes[u].FPoints.count = 0 then continue;

    tll := FLines[u];

    tll.SortPoints;

    ltlp := tll.fpoints[tll.fpoints.count-1];

    if ltlp.BoolVal = tbTrue then begin
      AddPoint(tlp.Time+1, FLines.IndexOf(tll), tlcStartloop);

      ltlp := tll.fpoints[tll.fpoints.count-1];
      if ltlp.BoolVal = tbTrue then
        raise exception.create('wtf');
      ApplyInterpolatedBoundsChanges;
//      Draw;
//      Flip;

      tll.Sortpoints;
    end;

  end;





end;

constructor TTimeLine.Create(Aowner: TComponent);
begin
  inherited;
  FLInes := TList<TTimeLineLine>.Create;
  Felements := TList<TTimeLineElement>.Create;
  ConstrainProportions := false;
  FSnap := 0.25;
  self.TabStop := true;
  FGlobalEvents := TTimeLineline.create;
  FglobalEvents.TimeLine := self;
  slHeader := TStringList.create;
  slFooter := TStringList.create;

end;

procedure TTimeLine.CreateExplosion(xScreen, yScreen: integer; c:Tcolor; bCreateParticles: boolean = true; rFlashpotScale: NativeFloat = 1.0);
var
  asplode: TTimeLineSplosion;
begin
  asplode := TTimelineSplosion.create;
  asplode.parent := self;
  FElements.Add(asplode);
  asplode.created := GetTicker;
  asplode.LifeTime := 1000;
  asplode.FlashpotScale := rflashpotscale;
  asplode.GlobalX := ScreenToGlobalX(xScreen);
  asplode.GlobalY := ScreenToGlobalY(yScreen);
  if bCreateParticles then
    asplode.InitParticles;
  asplode.particlecolor := ColorBlend(c, clWhite, 0.5);
end;

procedure TTimeLine.DblClick;
var
  tll: TTimelineline;
  p: TTimelinepoint;
  asplode: TTimeLIneSplosion;
begin
  inherited;
  tll := GetTimeLineLineAtScreenPoint(mouse_last_pos.x,mouse_last_pos.y);

  if tll = nil then exit;

  AddPoint(ScreenToGlobalX(mouse_last_pos.x), tll.tag,  tlcStartLoop);

//  p := tll.AddPoint;
//  p.FCommand := 'StartLoop';
//
//  p.Time := ScreenToGlobalX(mouse_last_pos.x);
//
//  p.BoolVal := tbTrue;
//  p.Param1 := tll.Tag div 5;
//  p.Param2 := tll.Tag mod 5;
//
//  tll.SortPoints;
//
//
//  CreateExplosion(mouse_last_pos.x, mouse_last_pos.y, tll.color);


end;

procedure TTimeLine.DeletemostRecentBanksetChange;
var
  tlp: TTimeLInePOint;
begin
  if LockUI then exit;

  tlp := FGlobalEvents.GetMostRecentevent(position, 'BankSetChange');
  if tlp <> nil then begin
    FGlobalEvents.FPoints.remove(tlp);
  end;

end;

procedure TTimeLine.DeleteSelectedPoints;
var
  t,u: integer;
  tll: TTimelineline;
  p: TTimeLinePoint;
  xx,yy: integer;
  fxx,fyy: integer;
  iDelCount: integer;
begin
  if LockUI then exit;

  iDelCount := 0;
  for u := 0 to FLines.count-1 do begin
    tll := FLines[u];

    for t:= tll.Fpoints.count-1 downto 0 do begin
      p := tll.fpoints[t];
      if p.selected then begin
        xx := round(self.GlobalToScreenX(p.time));
        yy := round(globaltoScreeny(tll.yoffset+0.5));
        fxx := round(self.GlobalToFormX(p.time));
        fyy := round(globaltoFormy(tll.yoffset+0.5));
        p.free;
        tll.fpoints.delete(t);
        inc(iDelCount);


        if fxx< 0 then continue;
        if fyy< 0 then continue;
        if fxx > self.clientwidth then continue;
        if fyy > self.clientwidth then continue;

        CreateExplosion(xx, yy,tll.color);

        if (iDelCount mod 3) = 0 then begin
          Draw;
          Flip;
        end;

      end;
    end;


  end;
end;

destructor TTimeLine.Destroy;
begin
  ClearElements;
  FLInes.Free;
  FglobalEvents.free;
  slHeader.free;
  slFooter.free;

  inherited;
end;

procedure TTimeLine.DoDraw;
var
  t: integer;
  yOffset: NativeFloat;
  fp: TTimelinePOint;
begin
  //inherited;


  //if not TexturesLoaded then begin
  //  LoadTextures;

  //end;

// debug := true;
  //self.Prepare;
  self.ClearScreen(clBlack);

// self.DrawTo.brush.color := clRed;
// self.DrawTo.pen.color := clBlue;
// self.DrawTo.Rectangle(0,0,width,height);
// self.BoundX1 := 0;
// self.BoundX2 := 100;
  self.BoundY1 := 0;
  self.Boundy2 := 15;
// self.Clear(clBlack);

  // draw the actual lines
  yOffset := 0;
  for t := 0 to Felements.Count - 1 do begin
    if t>= FElements.count then break;  //<--some things, such as particle systems may delete themselves during draw
    Felements[t].Draw1(yOffset);
  end;

  fp := FglobalEvents.FindFirstPointOftype('FadeAllSounds');
  if fp <> nil then begin
    Alphaop := aoAdd;
    canvas_Rectangle_Fill(GlobalToScreenX(fp.time), 0, GlobalToScreenX(fp.time+4), clientheight, clBlack, clGray, clBlack,clGray);
    canvas_Rectangle_Fill(GlobalToScreenX(fp.time+4), 0, clientwidth, clientheight, clGray, clGray, clGray,clGray);
  end;

  for t := 0 to Felements.Count - 1 do begin
    if t>= FElements.count then break;//<--some things, such as particle systems may delete themselves during draw
    Felements[t].Draw2;
  end;
  for t := 0 to Felements.Count - 1 do begin
    if t>= FElements.count then break;//<--some things, such as particle systems may delete themselves during draw
    Felements[t].Draw3;
  end;
  for t := 0 to Felements.Count - 1 do begin
    if t>= FElements.count then break;//<--some things, such as particle systems may delete themselves during draw
    Felements[t].Draw4;
  end;
  for t := 0 to Felements.Count - 1 do begin
    if t>= FElements.count then break;//<--some things, such as particle systems may delete themselves during draw
    Felements[t].Draw5;
  end;

// draw the scrubber
  Alphaop := aoAdd;
  self.Rectangle(Position - 0.15, BoundY1, Position + 0.15, Boundy2,
    colorblend(clYellow, clBlack, 0.80), true, false);
  self.Rectangle(Position - 0.25, BoundY1, Position + 0.25, Boundy2,
    colorblend(clYellow, clBlack, 0.80), true, false);
  Alphaop := aoNone;

  // draw the selector

  if mouse_op_mode = momDragSelect then begin
    Alphaop := aoAdd;
    self.canvas_Rectangle_Empty(mouse_down_at.X, mouse_down_at.Y,
      mouse_last_pos.X, mouse_last_pos.Y, clAqua, clAqua, clAqua,
      clAqua, 1.0);
    self.canvas_Rectangle_Fill(mouse_down_at.X, mouse_down_at.Y,
      mouse_last_pos.X, mouse_last_pos.Y, clAqua, clAqua, clAqua, clAqua, 0.2);
    Alphaop := aoNone;
  end;



// SetTexture(0);
// Alphaop := aoAdd;
// canvas_Rectangle_Empty(0, 0, 320, 200, clWhite, clWhite, clWhite, clWhite);
// Alphaop := aoNone;
// SetTexture(-1);

  // self.Flip;
end;

function TTimeLine.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if wheeldelta > 0 then
    result := DoMouseWheelUp(shift, mousepos)
  else
    result := DoMouseWheelDown(shift, mousepos);
end;

function TTimeLine.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  w: NativeFloat;
  x1,x2: NativeFloat;
  c,cPercent: NativeFloat;
begin
  inherited;
  w := (TargetBoundX2-TargetBoundX1);
  cPercent := (ScreenToGlobalX(mouse_last_pos.x) - TargetBoundX1) / w;
  c := ScreenToGlobalX(mouse_last_pos.x);

  w := w * 1.07;
  x1 := c - (w*cPercent);
  x2 := c + (w*(1-cPercent));
  TargetboundX1 := x1;
  TargetboundX2 := x2;

  result := true;

end;

function TTimeLine.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var
  w: NativeFloat;
  x1,x2: NativeFloat;
  c,cPercent: NativeFloat;
begin
  inherited;
  w := (TargetBoundX2-TargetBoundX1);
  cPercent := (ScreenToGlobalX(mouse_last_pos.x) - TargetBoundX1) / w;
  c := ScreenToGlobalX(mouse_last_pos.x);

  w := w / 1.05;
  x1 := c - (w*cPercent);
  x2 := c + (w*(1-cPercent));

  TargetboundX1 := x1;
  TargetboundX2 := x2;

  result := true;

end;

procedure TTimeLine.FindOrphanedPoints;
var
  t: integer;
  firstTime, min: NativeFloat;
begin
  ClearSelectedPoints;
  min := -1;
  for t:= 0 to FLines.count-1 do begin

    firsttime := FLines[t].FindOrphanedPoints;

    if (t = 0) or ((firsttime >= 0 ) and (firstTime < min)) then
      min := FirstTime;
  end;

  ScrollIntoView(min);

end;

function TTimeLine.GetEffectiveBankset(rTime: NativeFloat): integer;
var
  tlp: TTimeLinePoint;
begin
  result := 0;
  tlp := FGlobalEvents.GetMostRecentevent(rTime, 'BankSetChange');
  if assigned(tlp) then begin
    result := round(tlp.Param1);

  end;

end;

function TTimeLine.GetFinalPoint: TTimelinePoint;
var
  t: integer;
  tlp: TTimeLinePoint;
begin
  result := nil;

  for t:= 0 to FLines.count-1 do begin

    if FLines[t].fPoints.count = 0 then
      continue;

    tlp := FLines[t].Fpoints[FLines[t].fPoints.count-1];

    if (result = nil) or (tlp.Time > result.time) then begin
      result := tlp;
    end;

  end;

end;


function TTimeLine.GetTimeLineLIne(idx: integer): TTimeLineLine;
begin
  result := FLInes[idx];
end;

function TTimeLine.GetTimeLineLineAtScreenPoint(xScreen,
  yScreen: integer): TTimeLIneLine;
var
  t, u, yy: integer;
begin
  result := nil;

  yy := trunc(ScreenToGlobalY(yScreen));
  for u := 0 to FLInes.Count - 1 do begin
    if FLInes[u].yOffset = yy then begin
      result := FLines[u];
      break;
    end;
  end;
end;

function TTimeLine.GetTimeLinePointOnScreen(xScreen, yScreen: integer)
  : TTimeLinePoint;
var
  t, u, yy: integer;
begin
  result := nil;

  yy := trunc(ScreenToGlobalY(yScreen));
  for u := 0 to FLInes.Count - 1 do begin
    if FLInes[u].yOffset <> yy then
      continue;

    for t := 0 to FLInes[u].PointCount - 1 do begin
      if abs(FLInes[u].Fpoints[t].Time - ScreentoGlobalx(xScreen))
        < ScaleScreenXtoGlobal(POINT_OUTER_X_RADIUS) then begin
        result := FLInes[u].Fpoints[t];
        break;
      end;

    end;
  end;

end;

procedure TTimeLine.InsertBanksetChange(iBankSet: integer; rPosition: NativeFloat);
var
  p: TTimelinePoint;
begin
  p := FGlobalEvents.AddPoint;
  p.Command := 'BankSetChange';
  p.time := rPosition;
  p.ApplySnapping;
  p.Param1 := iBankset;
  Fglobalevents.sortpoints;
end;

procedure TTimeLine.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_DELETE then begin
    DeleteSelectedPoints;
  end;
end;

function TTimeLine.LineCount: integer;
begin
  result := FLInes.Count;
end;

procedure TTimeLine.LoadFromfile(sFile: string);
var
  sl: TStringlist;
  t: integer;
  pl: TPerformanceLine;
begin
  sl := TStringlist.Create;
  try
    FileName := sFile;
    sl.LoadFromfile(sFile);
    ClearData;
    slHeader.clear;
    slFooter.clear;
    for t := 0 to 3 do begin
      slHeader.Add(sl[t]);
    end;
    slFooter.add(sl[sl.count-1]);

    for t := 4 to sl.Count - 2 do begin
      pl := ParsePerformanceLine(sl[t]);
      pl.Time := secondstobeats(pl.time);
      self.AddFromPerformanceLine(pl);
    end;
  finally
    sl.Free;
  end;

end;

procedure TTimeLine.LoadTextures;
begin
  FTexturesLoaded := true;

  LoadTexture('Graphics\Spark1.png');
  LoadTexture('Graphics\Spark2.png');
  LoadTexture('Graphics\Spark3.png');
  LoadTexture('Graphics\Spark4.png');
  LoadTexture('Graphics\Spark5.png');
  LoadTexture('Graphics\brick.jpg');

end;

procedure TTimeLine.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  tlp: TTimeLinePoint;
begin
  inherited;
  if not Focused then begin
    SetFocus;
    //exit;
  end;
  x := round(globaltoscreenx(formtoglobalx(X)));
  y := round(globaltoscreeny(formtoglobaly(y)));
  mouse_down_at.X := x;
  mouse_down_at.Y := y;


  if (Button = mbMiddle) then begin
    mouse_op_mode := momDragScroll;

    originalbound1.X := BoundX1;
    originalbound1.Y := BoundY1;
    originalbound2.X := boundx2;
    originalbound2.Y := Boundy2;

  end else
  if (Button = mbRight) then begin
    tlp := GetTimeLinePointOnScreen(X, Y);
    if tlp = nil then begin
      self.Position := ScreenToGlobalX(mouse_last_pos.x);

    end else begin
      if tlp.Command = 'StartLoop' then begin
        tlp.Command := 'StopLoop';
      end else begin
        tlp.Command := 'StartLoop';
      end;
    end;

  end
  else begin
    // if no point found... default to drag select
    tlp := GetTimeLinePointOnScreen(X, Y);

    if tlp = nil then begin
      mouse_op_mode := momDragSelect;

    end
    else begin
      mouse_op_mode := momMovePoint;
      if not(ssShift in Shift) then begin
        // if we're recliking a selected point, then we will assume we're at the beginning
        // of a drag-move operation
        if tlp.Selected then begin
          mouse_op_mode := momMovePoint;
        end
        else begin
          ClearSelectedPoints;
          tlp.Selected := true;
        end;
      end
      else begin
        tlp.Selected := not tlp.Selected;
      end;
    end;
  end;

end;


procedure TTimeLine.MouseMove(Shift: TShiftState; X, Y: integer);
var
  NativeFloat_move_by: NativeFloat;
begin
  inherited;

  mouse_last_pos.X :=  round(globaltoscreenx(formtoglobalx(x)));
  mouse_last_pos.Y := round(globaltoscreeny(formtoglobaly(y)));
  //debug.consolelog(inttostr(mouse_last_pos.X));
  x := mouse_last_pos.x;
  y := mouse_last_pos.y;

  case mouse_op_mode of
    momDragScroll: begin
        TargetBoundX1 := originalbound1.X - ScaleScreenXtoGlobal(MouseMovedX,
          true);
        Targetboundx2 := originalbound2.X - ScaleScreenXtoGlobal(MouseMovedX,
          true);
        ApplyInterpolatedBoundsChanges;
//        Draw;
        self.font.color := clWhite;
// self.text(0,0,ScaleScreenXtoglobal(320,true),ScaleScreenXtoglobal(15,true), 'poop', tjLeft, trhorizontal);
//        Flip;
      end;
    momDragSelect: begin
//        Draw;
//        Flip;
      end;
    momMovePoint: begin
        NativeFloat_move_by := self.ScaleScreenXtoGlobal(X - mouse_down_at.X, true);
        MoveSelectedPoints(NativeFloat_move_by,false);

//        Draw;
//        Flip;
      end;
  end;

end;

function TTimeLine.MouseMovedX: integer;
begin
  result := mouse_last_pos.X - mouse_down_at.X;
end;

function TTimeLine.MouseMovedY: integer;
begin
  result := mouse_last_pos.Y - mouse_down_at.Y;

end;

procedure TTimeLine.MouseUp(SButton: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;

  if (x>=0) and (y>=0) then begin
    mouse_last_pos.X :=  round(globaltoscreenx(formtoglobalx(x)));
    mouse_last_pos.Y := round(globaltoscreeny(formtoglobaly(y)));

  end;

  if mouse_op_mode = momMovePoint then begin
    MoveSelectedPoints(self.ScaleScreenXtoGlobal(mouse_last_pos.x - mouse_down_at.X, true),true);
  end;
  if mouse_op_mode = momDragSelect then begin
    if not(ssShift in Shift) then
      ClearSelectedPoints;
    CommitDragSelect;
  end;

  mouse_op_mode := momNone;

end;


procedure TTimeLine.MoveSelectedPoints(rBy: NativeFloat; bCommit: boolean);
var
  t: integer;
begin
  for t := 0 to FLInes.Count - 1 do begin
    FLInes[t].MoveSelectedPoints(rBy, bCommit);
  end;
end;

procedure TTimeLine.Quantize;
var
  t: integer;
begin
  for t := 0 to Flines.count-1 do begin
    FLines[t].Quantize;
    ApplyInterpolatedBoundsChanges;
//    draw;
//    Flip;
  end;

  FGlobalEvents.Quantize;

end;

procedure TTimeLine.Resize;
begin
  inherited;
  dirty := true;
end;

function TTimeLine.SamplesToBeats(i: int64): NativeFloat;
begin
  result := i/((60*44100)/Tempo);
end;

function TTimeLine.BeatsToSamples(i: NativeFloat): int64;
begin
  result := round(i*((60*44100)/Tempo));
end;

function TTimeLine.BeatsToSeconds(r: NativeFloat): NativeFloat;
begin
  result := r / (Tempo/60);
end;

procedure TTimeLine.SaveToFile(sFile: string);
var
  FMergedEvents: TTimelineline;
  t,u: integer;
  slSave: TStringlist;
  bs: integer;
begin
  FileName := sFile;
  fMergedEvents := TTimelineline.create;
  try
    for u := 0 to FLInes.count-1 do begin
      for t:= 0 to FLInes[u].FPoints.count-1 do begin
        FMergedEvents.AddForeign(FLines[u].FPoints[t]);
      end;
    end;

    for t:= 0 to FGlobalEvents.FPoints.count-1 do begin
    FGlobalEvents.FPoints[t].Priority := true;
      FMergedEvents.AddForeign(FGlobalEvents.FPoints[t]);
    end;

    FMergedEvents.SortPoints;

    //change Start/Stop loop events based on final banksetswitches
    bs := 0;
    for t:=0 to FMergedEvents.fpoints.count-1 do begin
      if FMergedEvents.fpoints[t].eCommand in [tlcBankSetChange] then begin
        bs := round(FMergedEvents.fpoints[t].Param1);
      end;
      if FMergedEvents.fpoints[t].eCommand in [tlcStartloop,tlcStopLoop] then begin
         FMergedEvents.fpoints[t].Param1 := (5*bs)+ (round(FMergedEvents.fpoints[t].Param1) mod 5);
      end;
    end;

    slSave := TstringList.create;
    try
      slSave.Add(slHEader.text);

      for t:= 0 to FMergedEvents.FPoints.count-1 do begin
        slSave.Add(FMergedEvents.FPoints[t].ExporttoString);
      end;

      slSave.Add(slFooter.text);

      slSave.SaveToFile(sfile);
    finally
      slSave.free;
    end;

    for t:=0 to FMergedEvents.fpoints.count-1 do begin
      if FMergedEvents.fpoints[t].eCommand in [tlcStartloop,tlcStopLoop] then begin
         FMergedEvents.fpoints[t].Param1 := (round(FMergedEvents.fpoints[t].Param1) mod 5);
      end;
    end;


  finally
    FMergedEvents.Clearforeign;
    FmergedEvents.free;
  end;

end;

procedure TTimeLine.ScrollIntoview(rtime: NativeFloat);
var
  xdim: NativeFloat;
begin
  xdim := TargetBoundX2 - TargetBoundX1;

  if rTime < (TargetBoundx1+(xdim*0.2)) then begin
    TargetBoundX1 := rTime- (xdim/2);
    TargetBoundX2 := TargetBoundX1 + (xdim);
  end;

  if rTime > (TargetBoundx1+(xdim*0.8)) then begin
    TargetBoundX1 := rTime- (xdim/2);
    TargetBoundX2 := TargetBoundX1 + (xdim);
  end;
end;

function TTimeLine.SecondsToBeats(r: NativeFloat): NativeFloat;
begin
  result := r * (Tempo/60);
end;

procedure TTimeLine.SetFadeOutTime(rPosition: NativeFloat);
var
  p: TTimelinePoint;
begin
  FGlobalEvents.ClearPointsOfType('FadeAllSounds');
  FGlobalEvents.ClearPointsOfType('StopSession');

  p := FGlobalEvents.AddPOint;
  p.Command := 'FadeAllSounds';
  p.Time := rPosition;
  p.Param1 := 0.05;
  p.Param2 := 0;

  p := FGlobalEvents.AddPOint;
  p.Command := 'StopSession';
  p.Time := rPosition;
  p.Param1 := rPosition;
  p.Param2 := 0;


end;

procedure TTimeLine.setfocus;
begin
  inherited;
//  beeper.beep(400,400);
end;

procedure TTimeLine.SetPosition(const Value: NativeFloat);
begin
  FPosition := Value;
  dirty := true;
end;

procedure TTimeLine.StartDrag(var DragObject: TDragObject);
begin
  inherited;

end;

{ TTimeLineLine }

procedure TTimeLineLine.AddForeign(p: TTimelinePOint);
begin
  FPOints.add(p);
end;

function TTimeLineLine.AddPoint: TTimeLinePoint;
begin
  result := TTimeLinePoint.Create;
  result.Line := self;
  Fpoints.add(result);

end;

procedure TTimeLineLine.Clearforeign;
begin
  FPoints.clear;
end;

procedure TTimeLineLine.ClearPoints;
begin
  while Fpoints.Count > 0 do begin
    Fpoints[0].Free;
    Fpoints.Delete(0);
  end;
end;

procedure TTimeLineLine.ClearPointsOfType(sCommandType: string);
var
  tlp: TTimeLinePoint;
begin
  while true do begin
    tlp := FindFirstPointOftype(sCommandType);
    if tlp = nil then break;

    fpoints.remove(tlp);
  end;
end;

procedure TTimeLineLine.ClearSelectedPoints;

var
  t: integer;
begin
  for t := 0 to Fpoints.Count - 1 do begin
    Fpoints[t].Selected := false;
  end;

end;

constructor TTimeLineLine.Create;
begin
  inherited;

  FColor := clWhite;
  Fpoints := TList<TTimeLinePoint>.Create;
  FGroupOf := 1;


end;

destructor TTimeLineLine.Destroy;
begin
  ClearPoints;
  Fpoints.Free;

  inherited;
end;

procedure TTimeLineLine.Draw2;
var
  tll: TTimeLineLine;
  c1, c2: TColor;
  rBeat: NativeFloat;
  t,u: integer;
  X, Y: integer;

begin
  inherited;

  // draw glows behind points
  // draw points
  //PArent.CommitBatch;
  Parent.Alphaop := aoAdd;
  Parent.SetTexture(3);
  parent.BeginVertexBatch(D3DPT_TRIANGLELIST);
  if GetFirstPointInWindow(t) then begin
    while true do begin
      if t >= Fpoints.Count then
        break;
      if Fpoints[t].Time > Parent.boundx2 then
        break;
      if Fpoints[t].Time < Parent.BoundX1 then
        break;

      if (Fpoints[t].Selected) then begin
        FPoints[t].GlowNumber := random(4);

        // glow points

        Parent.Rectangle(Fpoints[t].Time - Parent.ScaleScreenXtoGlobal
            (POINT_OUTER_X_RADIUS * POINT_GLOW_SCALE),
          yOffset + 0.5 - (POINT_OUTER_Y_RADIUS * POINT_GLOW_YSCALE),
          Fpoints[t].Time + Parent.ScaleScreenXtoGlobal
            (POINT_OUTER_X_RADIUS * POINT_GLOW_SCALE),
          yOffset + 0.5 + (POINT_OUTER_Y_RADIUS * POINT_GLOW_YSCALE), clWhite,
          true);
      end;
      inc(t);
    end;

// parent.FatPoint(fPoints[t].Time, yOffset + 0.5, Color, BGColor, 4);
  end;
  parent.EndVertexBatch;
{$DEFINE FLICKER}
{$IFDEF FLICKER}
  for u := 0 to 4 do begin
    Parent.SetTexture(u);
    parent.BeginVertexBatch(D3DPT_TRIANGLELIST);
    if GetFirstPointInWindow(t) then begin
      while true do begin
        if t >= Fpoints.Count then
          break;
        if Fpoints[t].Time > Parent.boundx2 then
          break;
        if Fpoints[t].Time < Parent.BoundX1 then
          break;

        if (Fpoints[t].Selected) and (Fpoints[t].GlowNumber = u) then begin

          // glow points
          Parent.Rectangle(Fpoints[t].Time - Parent.ScaleScreenXtoGlobal
              (POINT_OUTER_X_RADIUS * POINT_GLOW_SCALE),
            yOffset + 0.5 - (POINT_OUTER_Y_RADIUS * POINT_GLOW_YSCALE),
            Fpoints[t].Time + Parent.ScaleScreenXtoGlobal
              (POINT_OUTER_X_RADIUS * POINT_GLOW_SCALE),
            yOffset + 0.5 + (POINT_OUTER_Y_RADIUS * POINT_GLOW_YSCALE), $5F5F,
            true);

        end;
        inc(t);
      end;
      parent.EndVertexBatch;
  // parent.FatPoint(fPoints[t].Time, yOffset + 0.5, Color, BGColor, 4);
    end;
  end;
{$ENDIF}
  Parent.Alphaop := aoNone;
  Parent.SetTexture(-1);


end;

procedure TTimeLineLine.Draw1(var yOffset: NativeFloat);
var
  tll: TTimeLineLine;
  c1, c2: TColor;
  rBeat: NativeFloat;
  t: integer;
  X, xx, Y: integer;
  bOldB, bCurrentB: boolean;
  bX: integer;
  iEffectiveIndex: integer;

begin
  inherited;

  bCurrentB := false;
  tll := self;

// background gradient
  parent.AlphaOp := aoStandard;

  c2 := colorblend(colorblend(tll.BGColor, clBlack, 0.75),
    colorblend(tll.BGColor, clBlack, 0.0), 1 - (GroupID / GroupOf));
  c1 := colorblend(colorblend(tll.BGColor, clBlack, 0.75),
    colorblend(tll.BGColor, clBlack, 0.0), 1 - ((GroupID + 1) / GroupOf));
  Parent.Rectangle(Parent.BoundX1, yOffset, Parent.boundx2, yOffset + 1, c1,
    true, true, c2);

  // time bars
  Parent.Rectangle(Parent.BoundX1, yOffset + 0.47, Parent.boundx2,
    yOffset + 0.53, colorblend(tll.color, c1, 0.75), true);

  bOldB := false;
  bX := 0;
  iEffectiveIndex := 0;

  X := round(Parent.GlobaltoScreenX(Parent.BoundX1));

  while X <= round(Parent.GlobaltoScreenX(Parent.boundx2)) do begin

    bCurrentB := GetboolState(Parent.ScreentoGlobalx(X), iEffectiveIndex);

    if bCurrentB <> bOldB then begin
      if bOldB then begin
        Parent.canvas_Rectangle_Fill(bX,
          Parent.GlobaltoScreenY(yOffset + TIMELINE_BAR_TOP), X,
          Parent.GlobaltoScreenY(yOffset + TIMELINE_BAR_BOTTOM), tll.color,
          tll.color, tll.color, tll.color, 1.0);
      end;

      bX := X;
    end;

// if iEffectiveIndex < 0 then break;

    inc(iEffectiveIndex);

    if iEffectiveIndex >= Fpoints.Count then
      break;

    xx := round(Parent.GlobaltoScreenX(Fpoints[iEffectiveIndex].Time));

    if xx <= X then
      X := xx + 1
    else
      X := xx;

// if x > parent.clientwidth then
// x := parent.clientwidth;

    bOldB := bCurrentB;

  end;

  if bCurrentB then begin
    Parent.canvas_Rectangle_Fill(bX,
      Parent.GlobaltoScreenY(yOffset + TIMELINE_BAR_TOP), Parent.width - 1,
      Parent.GlobaltoScreenY(yOffset + TIMELINE_BAR_BOTTOM), tll.color,
      tll.color, tll.color, tll.color, 1.0);
  end;


// for y:= round(parent.GlobaltoScreenY(yOffset+TIMELINE_BAR_TOP)) to round(parent.GlobaltoScreenY(yOffset+TIMELINE_BAR_BOTTOM)) do begin
// for x:= round(parent.GlobaltoScreenX(parent.boundx1)) to round(parent.GlobaltoScreenX(parent.boundx2)) do begin
// if GetBoolState(parent.screentoglobalx(x)) then parent.DrawTo_pixels[x,y] := tll.color;
//
//
// end;
// end;

// metrinome marks
  Parent.Alphaop := aoAdd;
  rBeat := round(Parent.BoundX1);
  if rBeat < 0 then
    rBeat := 0;
  Parent.BeginVertexBatch(D3DPT_TRIANGLELIST);
  try
    while rBeat <= Parent.boundx2 do begin
      if (round(rBeat) mod 4) = 0 then begin
        Parent.Rectangle_Fill(rBeat - (Parent.ScaleScreenXtoGlobal(0.5)),
          yOffset, rBeat + (Parent.ScaleScreenXtoGlobal(0.5)), yOffset + 1,
          clWhite, 0.5);
      end
      else begin
        Parent.Rectangle_Fill(rBeat - (Parent.ScaleScreenXtoGlobal(0.5)),
          yOffset, rBeat + (Parent.ScaleScreenXtoGlobal(0.5)), yOffset + 1,
          clBlue, 0.5);
      end;
      rBeat := rBeat + 1;
    end;
  finally
    Parent.EndVertexBatch;
    Parent.Alphaop := aoNone;

  end;

  yOffset := yOffset + 1;
end;

procedure TTimeLineLine.Draw3;
var
  tll: TTimeLineLine;
  c1, c2: TColor;
  rBeat: NativeFloat;
  t: integer;
  X, Y: integer;

begin
  inherited;

  Parent.SetTexture(-1);
  t := 0;
  Parent.Alphaop := aoNone;

  // draw points
  parent.commitbatch;
  parent.BeginVertexBatch(D3DPT_TRIANGLELIST);
  if GetFirstPointInWindow(t) then begin
    while true do begin
      if t >= Fpoints.Count then
        break;
      if Fpoints[t].Time > Parent.boundx2 then
        break;
      if Fpoints[t].Time < Parent.BoundX1 then
        break;

      // start points
      if Fpoints[t].eCommand = tlcStartLoop then begin
        Parent.Rectangle(Fpoints[t].Time - Parent.ScaleScreenXtoGlobal
            (POINT_OUTER_X_RADIUS), yOffset + POINT_OUTER_TOP,
          Fpoints[t].Time + Parent.ScaleScreenXtoGlobal
            (POINT_OUTER_X_RADIUS),
          yOffset + POINT_OUTER_BOTTOM, clBlack, true);
        Parent.Rectangle(Fpoints[t].Time - Parent.ScaleScreenXtoGlobal
            (POINT_INNER_X_RADIUS), yOffset + POINT_INNER_TOP,
          Fpoints[t].Time + Parent.ScaleScreenXtoGlobal
            (POINT_INNER_X_RADIUS),
          yOffset + POINT_INNER_BOTTOM, color, true);
      end
      // end points
      else begin
        Parent.Rectangle(Fpoints[t].Time - Parent.ScaleScreenXtoGlobal
            (POINT_OUTER_X_RADIUS), yOffset + POINT_OUTER_TOP,
          Fpoints[t].Time + Parent.ScaleScreenXtoGlobal
            (POINT_OUTER_X_RADIUS),
          yOffset + POINT_OUTER_BOTTOM, clGray, true);

      end;
      inc(t);
    end;

  end;

  parent.EndVertexBatch;



  // scrub bar

end;

function TTimeLineLine.FindFirstPointOftype(
  sCommandType: string): TTimeLinePOint;
var
  t: integer;
begin
  result := nil;

  for t:= 0 to fpoints.count-1 do begin
    if fpoints[t].Command = sCommandType then begin
      result := FPoints[t];
      exit;
    end;
  end;

end;

function TTimeLineLine.FindOrphanedPoints: NativeFloat;
var
  bExpectedOn: boolean;
  t: integer;
  p: TTimeLinePoint;
begin
  bExpectedOn := true;

  result := -1;

  for t:= 0 to FPoints.count-1 do begin
    p := FPoints[t];
    if bExpectedOn then begin
      if p.BoolVal = tbFalse then
        p.Selected := true;

      if result = -1 then
        result := p.time;
    end;

    if not bExpectedOn then begin
      if p.BoolVal = tbTrue then
        p.Selected := true;

      if result = -1 then
        result := p.time;
    end;

    bExpectedOn := not TriBoolToBool(p.Boolval);

  end;

end;

function TTimeLineLine.GetboolState(rPosition: NativeFloat;
  var iEffectiveIndex: integer): boolean;
var
  p1, p2: TTimeLinePoint;
  i: integer;
begin
  result := false;
  // scan through points until point.time > rPosition
  i := iEffectiveIndex;
  if i < 0 then
    i := 0;
  repeat
    if i >= Fpoints.Count then
      exit;
    if (Fpoints[i].Time > rPosition) and (Fpoints[i].BoolVal <> tbNull) then
      break;

    inc(i);

  until false;

  // backup one
  dec(i);
  iEffectiveIndex := i;

  if i < 0 then
    exit;

  p1 := Fpoints[i];
  if p1.BoolVal = tbTrue then begin
    result := true;
  end;

end;

function TTimeLineLine.GetFirstPointInWindow(out idx: integer): boolean;
begin
// if parent.boundx1 >= cached_point_search_boundx1 then
// idx := cached_point_search_idx
// else
  idx := 0;
  result := false;
  while true do begin
    if idx >= Fpoints.Count then begin
      break;
    end;

    if (Fpoints[idx].Time >= TimeLine.BoundX1) and
      (Fpoints[idx].Time <= TimeLine.boundx2) then begin
      cached_point_search_idx := idx;
      cached_point_search_boundx1 := Parent.BoundX1;
      result := true;
      exit;
    end;

    inc(idx);

  end;

  cached_point_search_idx := idx;
  cached_point_search_boundx1 := Parent.BoundX1;

end;

function TTimeLineLine.GetMostRecentevent(rTime: NativeFloat;
  sType: string): TTimeLinePoint;
var
  t: integer;
  p: TTimeLinePoint;
begin
  result := nil;
  t := 0;
  while t < fpoints.count-1 do begin
    p := FPoints[t];
    if p.Time > rTime then
      break;

    if (p.Command = sType) or (sType = '') then begin
      result := p;
    end;
    inc(t);
  end;
end;
procedure TTimeLineLine.MoveSelectedPoints(rBy: NativeFloat; bCommit: boolean);
var
  t: integer;
begin
  for t := 0 to Fpoints.Count - 1 do begin
    IF Fpoints[t].Selected then
      Fpoints[t].MoveRelative(rBy, bCommit);
  end;

  sortpoints;
end;

function TTimeLineLine.PointCount: integer;
begin
  result := Fpoints.Count;
end;

procedure TTimeLineLine.Quantize;
var
  u: integer;
begin
  u := FPoints.count-1;
  while u >=0 do begin
    FPoints[u].applysnapping;
    dec(u);

  end;

  //important DONT SORT DURING APPLYSNAPPING!

  for u:= FPOints.count-1 downto 1 do begin
    if u >= FPoints.count then continue;
    if FPoints[u].Time = FPOints[u-1].Time then begin
      FPoints.delete(u-1);
    end;
  end;

end;

procedure TTimeLineLine.SelectPointsInRange(rStart, rEnd: NativeFloat;
  bSelected: boolean);
var
  t: integer;
begin
  for t := 0 to Fpoints.Count - 1 do begin
    if Fpoints[t].Time < rStart then
      continue;

    if Fpoints[t].Time > rEnd then
      break;

    if not FPoints[t].Selected then begin
      timeline.CreateExplosion(round(timeline.globaltoscreenx(FPoints[t].time)), round(timeline.globaltoscreeny(round(yOffset))), clGray, false, 0.4);
    end;
    Fpoints[t].Selected := true;


  end;

end;




procedure TTimeLineLine.SortPoints;
var
  c: TComparer<TTimeLinePoint>;
begin
  c := TTimeLinePointcomparer.create;
  try
    Fpoints.Sort(c);
  FINALLY
    c.free;
  end;

end;

{ TTimeLineElement }

procedure TTimeLineElement.Draw1(var yOffset: NativeFloat);
begin
  //dni
  self.yOffset := yoffset;
end;

procedure TTimeLineElement.Draw2;
begin
// no implementation required
end;

procedure TTimeLineElement.Draw3;
begin
// no implementation required
end;

procedure TTimeLineElement.Draw4;
begin
// no implementation required
end;

procedure TTimeLineElement.Draw5;
begin
// no implementation required
end;

{ TTimeLineDivider }

procedure TTimeLineDivider.Draw1(var yOffset: NativeFloat);
var
  t: integer;
  p: TTimeLinePOint;
begin
  inherited;
  if not parent.Focused then
    BGColor := $3f3f3f
  else
    BGColor := clBlack;

  Parent.Rectangle(Parent.BoundX1, yOffset, Parent.boundx2, yOffset + 1,
    colorblend(BGColor, clBlack, 0.1), true, true,
    colorblend(BGColor, clBlack, 0.0));

  parent.AlphaOp := aoNone;
  parent.BeginVertexBatch(D3DPT_TRIANGLELIST);
  try
    parent.SetTexture(5);
    for  t:= truncate(parent.boundx1, true)-4 to (truncate(parent.boundx2,true)+1) do begin
//      parent.BatchMode := true;
      if (t mod 4) <> 0 then continue;
      parent.Rectangle_Fill((t),yOffset, (t)+4, yOffset+1, clGray, 1);
    end;

  finally
    parent.EndVertexBatch;
  end;
  parent.SetTexture(-1);
  parent.AlphaOp := aoAdd;
  parent.BeginVertexBatch(D3DPT_TRIANGLELIST);
  try
    for t:= 0 to parent.FGlobalEvents.Fpoints.count-1 do begin
      p := parent.FGlobalevents.Fpoints[t];
      if p.eCommand = tlcBanksetchange then begin
        parent.canvas_batch_Rectangle_Fill(parent.GlobalToScreenX(p.time), parent.GlobalToScreenY(yOffset), parent.GlobalToScreenX(p.time+4), parent.GlobalToScreenY(yOffset+1), BUTTON_COLORS[round(p.param1)],clBlack, BUTTON_COLORS[round(p.param1)],clBlack);
      end;
    end;
  finally
    parent.EndVertexBatch;
  end;
  parent.AlphaOp := aoNone;

  yOffset := yOffset + 1;
end;

{ TTimeLinePoint }
function ParsePerformanceLine(s: string): TPerformanceLine;
var
  sl: TStringlist;
begin
  sl := TStringlist.Create;
  try
    stringx.ParseString(s, #9, sl);

    if sl.Count = 4 then begin
      result.Time := strtofloat(sl[0]);
      result.Command := sl[1];
      result.Param1 := strtofloat(sl[2]);
      result.Param2 := strtofloat(sl[3]);
    end;
  finally
    sl.Free;
  end;
end;

procedure TTimeLinePoint.MoveRelative(rBy: NativeFloat; bCommit: boolean);
begin
  FTime := FOriginalTime + rBy;
  if bCommit then
    FOriginalTime := FTime;

  ApplySnapping;

end;

procedure TTimeLinePoint.SetCommand(const Value: string);
begin
  FCommand := Value;

  Fboolval := tbNull;
  FeCommand := tlcOther;


  if Fcommand = 'StartLoop' then begin
    FboolVal := tbTrue;
    FeCommand := tlcStartloop;
  end;

  if FCommand = 'StopLoop' then begin
    FBoolVal := tbFalse;
    FeCommand := tlcStopLoop;
  end;

  if fcommand = 'BankSetChange' then begin
    FeCommand := tlcBankSetChange;
  end;


end;

procedure TTimeLinePoint.SetSelected(const Value: boolean);
begin
  FSelected := Value;
  // record the time that the point was selected (for drag-move operations)
  FOriginalTime := FTime;

end;

{ TTimeLinePoint }

procedure TTimeLinePoint.ApplySnapping;
begin
  if FLine.TimeLine.Snap >0 then begin
    FTime := round(Ftime / FLine.TimeLine.Snap) * FLine.TimeLine.Snap;
  end;
end;

function TTimeLinePoint.CompareTo(Obj: TObject): Integer;
var
  c: TTimeLInePOint;
begin
  c := obj as TTimeLInePoint;

  result := 0;

  if c.Time < time then
    result := -1;

  if c.Time > time then
    result := 1;





end;

procedure TTimeLinePoint.CopyFromPerformanceLine(var pl: TPerformanceLine);
begin
  Time := pl.Time;
  Param1 := pl.Param1;
  Param2 := pl.Param2;
  Command := pl.Command;
  if Command = 'StartLoop' then begin
    self.BoolVal := tbTrue;
  end
  else if Command = 'StopLoop' then begin
    self.BoolVal := tbFalse;
  end
  else begin
    self.BoolVal := tbNull;
  end;

end;

function TTimeLinePoint.ExporttoString: string;
begin
  result := FloatPrecision(self.Line.timeLine.BeatsToSeconds(FTime),3)+#9+FCommand+#9+FloatPrecision(Param1,3, true)+#9+FloatPrecision(Param2,3, true);
end;

{ TTimeLinePointcomparer }

function TTimeLinePointcomparer.Compare(const Left,
  Right: TTimeLinePoint): Integer;
begin
  result := 0;
  if (Left.Time < Right.Time) then
    result := -1
  else if (Left.Time > Right.Time) then
    result := 1
  else begin
    if Left.Priority = Right.Priority then
      result := 0
    else
    if booltoint(Left.Priority) > booltoint(Right.Priority) then
      result := 1
    else
      result := -1;
  end;
end;

{ TTimeLineSplosion }


constructor TTimeLineSplosion.Create;
begin
  inherited;
  Fparticles := TList<TTimeLIneparticle>.create;
  FlashPotScale := 1.0;
end;

destructor TTimeLineSplosion.Destroy;
begin
  while FParticles.count > 0 do begin
    FParticles[FParticles.count-1].free;
    FParticles.delete(FParticles.count-1)
  end;
  FParticles.free;
  inherited;
end;

procedure TTimeLineSplosion.Draw1(var yOffset: NativeFloat);
begin
  inherited;
  ScreenX := parent.GlobalToScreenX(GlobalX);
  ScreenY := parent.GlobalToScreenY(GlobalY);

end;

procedure TTimeLineSplosion.Draw2;
begin
  inherited;
  //
end;

procedure TTimeLineSplosion.Draw3;
begin
  inherited;
  //

end;

procedure TTimeLineSplosion.Draw4;
begin
  inherited;
  //
end;

procedure TTimeLineSplosion.Draw5;
var
  a: NativeFloat;
  c: TColor;
  flashpotsize: NativeFloat;
begin
  inherited;
  //
  try
    UpdateParticles;
    a := GetTimeSince(created) / Lifetime;
    a := 1-a;
    if a < 0 then
      a := 0;

    parent.AlphaOp := aoAdd;
    flashpotsize := FLASHPOT_BASE_RADIUS * FlashPotScale;

    if a > 0 then begin
      c := ColorBlend(clBlack, clWhite, a);
      parent.Settexture(random(4));
      parent.canvas_Rectangle_Fill(ScreenX - flashpotsize, ScreenY - flashpotsize, ScreenX + flashpotsize, ScreenY + flashpotsize, c,c,c,c, 1.0);
      parent.Settexture(random(4));
      parent.canvas_Rectangle_Fill(ScreenX - flashpotsize, ScreenY - flashpotsize, ScreenX + flashpotsize, ScreenY + flashpotsize, c,c,c,c, 1.0);

    END ELSE begin
      if not UpdateParticles then begin
        parent.Felements.Remove(self);
        self.free;
        exit;
      end;
    end;

    SELF.DrawParticles;
  finally
    parent.Settexture(-1);
    parent.AlphaOp := aonone;
  end;

end;

procedure TTimeLineSplosion.DrawParticles;
var
  t,u: integer;
  p : TTimelineParticle;
  px,py,pr: NativeFloat;
  c: TColor;


begin
  for u := 4 downto 0 do begin
    parent.Settexture(random(u));
    parent.BeginVertexBatch(d3dpt_trianglelist);
    for t:= Fparticles.count-1 downto 0 do begin
      p := FParticles[t];
      c := ColorBlend(clBlack, particlecolor, p.alpha);
      if p.alpha > 0 then begin
        px := ScreenX - p.X;
        py := ScreenY - p.y;
        pr := p.s * PARTICLE_BASE_RADIUS;

        if p.Particlenumber = u then begin
          parent.canvas_Rectangle_Fill(px - pr, py - pr, px + pr, py + pr, c,c,c,c, 1.0);
        end;

        //always draw a 4
        if u = 4 then begin
          parent.canvas_Rectangle_Fill(px - pr, py - pr, px + pr, py + pr, c,c,c,c, 1.0);
        end;
      end;
    end;
    parent.EndVertexBatch;

  end;

end;


procedure TTimeLineSplosion.InitParticles;
var
  t: integer;
  p: TTimelineParticle;
begin
  for t:= 0 to PARTICLE_COUNT-1 do begin
    p := TTimeLineParticle.create;
    p.x := 0;
    p.y := 0;
    p.created := self.Created;
    p.LastUpdateTime := p.created;
    p.Gravity := -9.8;
    p.LifeTime := 4000;
    p.vx := (random(40) - 20);
    p.vy := (random(200));
    p.vs := (random(50) - 50)/500;
    p.s := 1.0;

    Fparticles.add(p);

  end;

end;

function TTimeLineSplosion.UpdateParticles: boolean;
var
  t: integer;
  tm: Cardinal;
begin
  tm := GetTicker;
  for t:= FParticles.count-1 downto 0 do begin
    FParticles[t].Update(tm);

    if FParticles[t].Alpha <= 0 then begin
      fParticles[t].free;
      FParticles.delete(t);
    end;
  end;
  result := FParticles.count > 0;
end;

{ TTimelineParticle }

procedure TTimelineParticle.Update(time: cardinal);
var
  deltatime: NativeFloat;
begin
  deltaTime := (time - LastUpdateTime) / 100;
  vy := vy + (Gravity * deltatime);

  x := x + (vx * deltatime);
  y := y + (vy * deltatime);
  s := s * (1 + (vs * deltatime));
  Alpha := 1 - GetTimeSince(Created) / LifeTime;

  if Alpha < 0 then
    Alpha := 0;

  LastUpdateTime := time;
  ParticleNumber := random(5);
  if ParticleNumber = 5 then
    ParticleNumber := 4;


end;

end.
