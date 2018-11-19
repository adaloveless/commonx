unit velocitypanel;

interface

uses
  tickcount, systemx, comctrls, windows, math, extctrls, controls, classes, debug, stringx;


type
  TVelocityPanel = class(TPanel)
  private
    FRealTop: real;
    FRealheight: real;
    FRealLeft: real;
    FTargetTop: integer;
    FTargetHeight: integer;
    FTargetLeft: integer;
    FREalWidth: real;
    FTargetWidth: integer;
    FVelTop: real;
    FVelheight: real;
    FVelLeft: real;
    FVelWidth: real;
    FDecelrate: real;
    FAccelRate: real;
    FLAstTick, FThisTick: cardinal;
    FInMOtion: boolean;
    FApplyingPhysics: boolean;
    FPreserveAspect: boolean;
    procedure SetInMotion(const Value: boolean);
  public
    constructor create(aowner: TComponent);override;
    property TargetLeft: integer read FTargetLeft write FTargetLeft;
    property TargetWidth: integer read FTargetWidth write FTargetWidth;
    property TargetTop: integer read FTargetTop write FTargetTop;
    property TargetHeight: integer read FTargetHeight write FTargetHeight;
    property RealLeft: real read FRealLeft write FRealLeft;
    property RealTop: real read FRealTop write FRealTop;
    property REalWidth: real read FREalWidth write fREalWidth;
    property Realheight: real read FRealheight write FRealheight;
    property VelLeft: real read FVelLeft write FVelLeft;
    property VelTop: real read FVelTop write FVelTop;
    property VelWidth: real read FVelWidth write fVelWidth;
    property Velheight: real read FVelheight write FVelheight;

    property AccelRate: real read FAccelRate write FAccelRate;
    property DecelRate: real read FDecelrate write FDecelRate;
//    property SequenceStartTime: cardinal read FSeqStartTime write FseqStartTime;
//    property SequenceLength: cardinal read FSeqLength write FSeqLength;

    procedure UpdateSequence;
    procedure ApplyPhysics(var rpos, rvel: real; var rtarget: integer);
    procedure Resize;override;
    procedure StartSequence;
    property InMotion: boolean read FInMOtion write SetInMotion;
    function TimeSpan: cardinal;
    property ApplyingPhysics: boolean read FApplyingPhysics write FApplyingPhysics;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property PreserveAspect: boolean read FPreserveAspect write FPreserveAspect;



  end;




implementation

{ TVelocityPanel }

procedure TVelocityPanel.ApplyPhysics(var rpos, rvel: real;
  var rtarget: integer);
var
  dist: real;
  rchange: real;
begin
  dist := abs(rtarget-rpos);
  rchange := 0;
  if rpos < rtarget then begin
    rchange := (AccelRate*(TimeSpan/1000));;

    if rtarget-rpos < rvel*rvel then
        rvel := rvel * DecelRate;



    if rchange > dist then
      rchange := dist;

    rvel := rvel+rchange;
    if rVel > rtarget-rpos then
      rVel := rtarget-rpos;
  end;

  if rpos > rtarget then begin

    rchange := 0-(AccelRate*(TimeSpan/1000));;

    if rpos-rtarget < rvel*rvel then
        rvel := rvel *DecelRate;


    if rchange < 0-dist then
      rchange := 0-dist;

    rvel := rvel+rchange;

    if rVel < rtarget-RPOS then
      rVel := rtarget-RPOS;
  end;


//  if rvel < 1 then rVel := 1;
  if abs(rTarget-rPOs) <= 1 then
    rPOs := round(rTarget);


  rpos := rpos+rvel;
  if rpos = rtarget then
    rvel := 0;



end;



constructor TVelocityPanel.create(aowner: TComponent);
begin
  inherited;
  FAccelRate := 3000;
  FDecelRate := 0.1;
end;

procedure TVelocityPanel.Resize;
begin
  inherited;
  Debug.Log('Resize');
end;

procedure TVelocityPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not ApplyingPhysics then begin
    FRealLeft := ALeft;
    FRealTop := ATop;
    FRealWidth := AWidth;
    FRealHeight := AHeight;
  end;
end;

procedure TVelocityPanel.SetInMotion(const Value: boolean);
begin
  FInMOtion := Value;
  Debug.Log(self,'InMotion = '+booltostr(value));
end;

procedure TVelocityPanel.StartSequence;
begin
  if FInMotion then exit;

  FLastTick := GetTicker;
  InMotion := true;
end;

function TVelocityPanel.TimeSpan: cardinal;
begin
  RESULT := GetTimeSince(FThisTick, FLastTick);
end;

procedure TVelocityPanel.UpdateSequence;
var
  l,t,w,h: integer;
begin
  if not Inmotion then exit;
  FThisTick := GetTicker;
  if FThisTick = FLastTick then exit;
  ApplyingPhysics := true;
  try

    applyphysics(FREalLeft, FVelLeft, FTargetLeft);
    applyphysics(FREalTop, FVelTop, FTargetTop);
    applyphysics(FREalWidth, FVelWidth, FTargetWidth);
    if not PreserveAspect then begin
      applyphysics(FREalHeight, FVelHeight, FTargetHeight);
    end else begin
      FRealHEight := FRealWidth*(FTargetHeight/FTargetWidth);
    end;

    l := round(FRealLeft);
    t := round(FRealTop);
    w := round(FRealWidth);
    h := round(FRealHeight);
    Setbounds(l, t, w, h);

    if (l=FtargetLeft) and (t=fTargetTop) and (w=FTargetWidth) and (h=ftargetHeight) then begin
      Resize;
      InMotion := false;
    end;

  finally
    ApplyingPhysics := false;
  end;
end;

end.
