unit FormFMXBase;
{$DEFINE FADE_OPEN}
interface

uses
{$IFDEF MSWINDOWS}
  windows, Winapi.Messages, Winapi.IpTypes, fmx.platform.win, fmx.platform,
{$ENDIF}
  guihelpers_fmx, SCALEDlayoutproportional, commandprocessor, systemx, typex, tickcount,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, fmx_messages,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, system.messaging,
  FMX.Objects, anoncommand;

const
  CURTAIN_MIN_SPEED_CLOSE = 2.0;
  CURTAIN_IIR_CURRENT_CLOSE = 0.96;
  CURTAIN_IIR_NEW_CLOSE = 0.04;
  CURTAIN_MIN_SPEED_OPEN = 2.0;
  CURTAIN_IIR_CURRENT_OPEN = 0.98;
  CURTAIN_IIR_NEW_OPEN = 0.02;

  CURTAIN_COUNT = 100;
  CM_CURSORCHANGED = $B00F;
  DEFAULT_CURTAIN_SPEED_MULT =4;
type
  TCurtainState = (csOpen, csClosing, csClosed, csOpening);
  TCurtainPoint = record
    targetpoint: TPointF;
    obj: TRectangle;
  end;

  TCurtains = record
  private
    procedure SetState(const Value: TCurtainState);
  public
    Fstate: TCurtainState;
    last_state_change_time: ticker;
    transition_proc: TProc;
    transition_object_index: ni;
    property state: TCurtainState read FState write SetState;
  end;


  TfrmFMXBase = class(TForm)
  private
    FTransplanted: boolean;
    procedure Transition(proc: TProc);
    { Private declarations }

  protected
    watch_last_active: boolean;
    default_curtains: array[0..CURTAIN_COUNT-1] of TCurtainPoint;
    curtainsdata : TCurtains;

    ActiveCommands: TCommandList<TCommand>;
    {$IFDEF MSWINDOWS}
    FMsgSys: TMessagingSystem;
    {$ENDIF}

    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoHide; override;
    procedure WatchCommand(c: TCommand; bTakeOwnership: boolean);
    procedure BringDefaultCurtainsToFront;
  public
    mock: TForm;
    WorkingHard: boolean;
    function WatchCommands: boolean;virtual;
    { Public declarations }

    function GetControl<T: TControl>(parent: TControl): T;

    procedure ActivateOrTransplant;virtual;
    procedure ActivateByPush;virtual;
    procedure ActivateByPop;virtual;
    procedure UnregisterWithMockMobile;
    procedure UpdateMouseCursor;
    constructor Create(AOwner: TComponent); override;

    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    destructor Destroy; override;
    property Transplanted: boolean read FTransplanted write FTransplanted;
    procedure DoBoundsSet;virtual;
    procedure UpdateFromModel; virtual;
    procedure UpdateState;
    procedure DoUpdatestate;virtual;
    procedure ToggleBusy(busy: boolean);virtual;
    procedure Watch(bTakeOwnership: boolean; c: TCommand);
    procedure Curtains(proc: TProc);
    procedure Curtains_Frame(interval: nativeint);virtual;
    procedure Curtains_Frame_Fixed(interval: nativeint);virtual;
    procedure Curtains_Open_SetStartingState_Slide;
    procedure Curtains_Open_SetStartingState_Fade;
    procedure Curtains_Open_SetStartingState;virtual;
    procedure Curtains_Close_SetStartingState;virtual;
    function Curtains_Close(intv: nativeint): boolean;virtual;//return TRUE when finished
    function Curtains_Open_Slide(intv: nativeint): boolean;
    function Curtains_Open_Fade(intv: nativeint): boolean;
    function Curtains_Open(intv: nativeint): boolean;virtual;//return TRUE when finished
    function InitCurtains: boolean;virtual;
    procedure WorkError(sMessage: string);virtual;
    procedure HardWork(proc: TProc);overload;
    procedure HardWork(proc: TProc; guiSuccess: TProc);overload;
    procedure BusyWork(proc: TProc);overload;
    procedure BusyWork(proc: TProc; guiSuccess: TProc);overload;
    procedure ShowMessage(sMessage: string);virtual;




  end;

var
  frmFMXBase: TfrmFMXBase;

implementation

uses
  FormMockMobile, debug;

{$R *.fmx}

{ TfrmFMXBase }

procedure TfrmFMXBase.ActivateByPop;
begin
  UpdateState;
//this happens when the form is activated after another form is popped off the form stack
end;

procedure TfrmFMXBase.ActivateByPush;
begin
  UpdateState;
//this happens when the form is pushed to the form stack
end;

procedure TfrmFMXBase.ActivateOrTransplant;
begin
  DoShow;
  if assigned(onActivate) then
    OnActivate(self);
  if mock <> nil then
    Tmm(mock).SetBounds(Tmm(mock).GetBounds)
  else
    self.setbounds(self.getbounds);
end;

procedure TfrmFMXBase.BringDefaultCurtainsToFront;
var
  t: ni;
begin
  for t:= 0 to high(default_curtains) do begin
    default_curtains[t].obj.bringtofront;
  end;

end;

procedure TfrmFMXBase.BusyWork(proc, guiSuccess: TProc);
begin
  HardWork(proc, guiSuccess);
end;

procedure TfrmFMXBase.BusyWork(proc: TProc);
begin
  HardWork(proc);
end;

constructor TfrmFMXBase.Create(AOwner: TComponent);
begin
  Debug.Log('Creating '+classname);
  inherited;
{$IFDEF MSWINDOWS}
  FMsgSys := TMessagingSystem.create(self);
{$ENDIF}
//  FMsgSys.RegisterMessageHandler(  <<---- you can use this to subscribe to windows messages if NEEDED
  ActiveCommands := TCommandList<TCommand>.create;
  ActiveCommands.RestrictedtoThreadID := Tthread.Currentthread.threadid;



end;

procedure TfrmFMXBase.Curtains(proc: TProc);
begin
  //defer to mock-mobile form if assigned, this form will not handle curtains
  if mock <> nil then begin
    Tmm(mock).Curtains(proc);
    exit;
  end;

  InitCurtains;
  Curtains_Close_SetStartingState;

  //set curtain start state
  curtainsdata.state := csClosing;
  curtainsdata.last_state_change_time := getticker;
  //set instance variable for proc to use when curtains are closed
  curtainsdata.transition_proc := proc;

end;


function TfrmFMXBase.Curtains_Close(intv: nativeint): boolean;
var
  pnew, pcurrent, ptarget, pdif: TPointF;
  iGoodCount: ni;
begin
  iGoodCount := 0;
  result := false;
  for var t := 0 to high(default_curtains) do begin
    var c := default_curtains[t];
    pcurrent := c.obj.Position.Point;
    ptarget := c.targetpoint;
    pdif := ptarget - pcurrent;
    if pdif.Length < CURTAIN_MIN_SPEED_CLOSE then begin
      c.obj.Position.point := ptarget;
      inc(iGoodCount);
    end else begin
      pnew := (pcurrent*CURTAIN_IIR_CURRENT_CLOSE) + (ptarget * CURTAIN_IIR_NEW_CLOSE);
      if (pnew-pcurrent).length < CURTAIN_MIN_SPEED_CLOSE then
        pnew := pcurrent+((pnew-pcurrent).normalize*CURTAIN_MIN_SPEED_CLOSE);
      c.obj.Position.point := pnew;
    end;
  end;


  result := iGoodCount = length(default_curtains);

end;

function TfrmFMXBase.Curtains_Open_Fade(intv: nativeint): boolean;
const
  FORWARD_COUNT = 5;
begin
  if curtainsdata.transition_object_index > high(default_curtains) then
    exit(true);
  for var t:= curtainsdata.transition_object_index to curtainsdata.transition_object_index+FORWARD_COUNT do begin
    if t > high(default_curtains) then
      continue;

    var obj := self.default_curtains[t].obj;
    var alpha := obj.opacity;
    alpha := alpha - (intv / 100);
    if alpha < 0 then
      alpha := 0;

    obj.opacity := alpha;

    if alpha = 0 then begin
      obj.visible := false;
      inc(curtainsdata.transition_object_index);
    end;

    result := false;
  end;



end;

procedure TfrmFMXBase.Curtains_Open_SetStartingState;
begin
{$IFDEF FADE_OPEN}
  Curtains_Open_SetStartingState_Fade;
{$ELSE}
  Curtains_Open_SetStartingState_Slide;
{$ENDIF}
end;

procedure TfrmFMXBase.Curtains_Open_SetStartingState_Fade;
begin
  BringDefaultCurtainsToFront;
  curtainsdata.transition_object_index := 0;
end;

procedure TfrmFMXBase.Curtains_Open_SetStartingState_Slide;
  function IsOutside(p: TPointF; o: TControl): boolean;
  begin
    if (p.x<0-width) then exit(true);
    if (p.y<0-height) then  exit(true);
    if (p.x>width) then exit(true);
    if (p.y>height) then exit(true);
    exit(false);
  end;
begin
  for var t := 0 to high(default_curtains) do begin
    var c := default_curtains[t];
    repeat
      c.targetpoint.x := random(width*6)-(width*3);
      c.targetpoint.y := random(height*6)-(height*3);


//      if t = 0 then
//        c.obj.Fill.Color := (cardinal(255 shl 24)+random($FFFFFF))
//      else
//        c.obj.Fill.Color := (cardinal(random(128) shl 24)+random($FFFFFF));
      default_curtains[t] := c;
      c.obj.bringtofront;
      c.obj.Opacity := 1.0;
      c.obj.Visible := true;
    until IsOutSide(c.targetpoint, c.obj);
  end;

end;

function TfrmFMXBase.Curtains_Open_Slide(intv: nativeint): boolean;
var
  pnew, pcurrent, ptarget, pdif: TPointF;
  iGoodCount: ni;
begin
  iGoodCount := 0;
  result := false;
  for var t := 0 to high(default_curtains) do begin
    var c := default_curtains[t];
    pcurrent := c.obj.Position.Point;
    ptarget := c.targetpoint;
    pdif := ptarget - pcurrent;


    if pdif.Length < CURTAIN_MIN_SPEED_OPEN then begin
      c.obj.Position.point := ptarget;
      inc(iGoodCount);
    end else begin
      pnew := (pcurrent*CURTAIN_IIR_CURRENT_OPEN) + (ptarget * CURTAIN_IIR_NEW_OPEN);
      if (pnew-pcurrent).length < CURTAIN_MIN_SPEED_OPEN then
        pnew := pcurrent+((pnew-pcurrent).normalize*CURTAIN_MIN_SPEED_OPEN);
      c.obj.Position.point := pnew;

    end;
  end;

  result := iGoodCount = length(default_curtains);

end;

procedure TfrmFMXBase.Curtains_Frame(interval: nativeint);
begin
  repeat
    Curtains_Frame_Fixed(15);
    dec(interval, 15);
  until interval < 0;
end;


procedure TfrmFMXBase.Curtains_Frame_Fixed(interval: nativeint);
begin
  initcurtains;
  case curtainsdata.state of
    csClosing: begin
      if Curtains_Close(interval) then begin
        curtainsdata.state := csClosed;
      end;
    end;
    csClosed: begin
      curtainsdata.transition_proc();
      curtainsdata.state := csOpening;
      Curtains_Open_SetStartingState();
      if Curtains_Open(interval) then begin
        curtainsdata.state := csOpen;
      end;
    end;
    csopening: begin
      if Curtains_Open(interval) then begin
        curtainsdata.state := csOpen;
      end;
    end;
  end;
end;

function TfrmFMXBase.Curtains_Open(intv: nativeint): boolean;
begin
{$IFDEF FADE_OPEN}
  exit(Curtains_Open_Fade(intv));
{$ELSE}
  exit(Curtains_Open_Slide(intv));
{$ENDIF}
end;

procedure TfrmFMXBase.Curtains_Close_SetStartingState;
  function IsOutside(p: TPointF; o: TControl): boolean;
  begin
    if (p.x<0-width) then exit(true);
    if (p.y<0-height) then  exit(true);
    if (p.x>width) then exit(true);
    if (p.y>height) then exit(true);
    exit(false);
  end;
begin
  for var t := 0 to high(default_curtains) do begin
    var c := default_curtains[t];
    repeat
      c.obj.Position.x := random(width*6)-(width*3);
      c.obj.Position.y := random(height*6)-(height*3);
      if t = 0 then begin
        c.targetpoint := TPointF.create(0,0)
      end else begin
        c.targetpoint := TPointF.create(random(round(width*1.5))-(width*0.75),random(round(height*1.5))-(height*0.75))
      end;
      if t = 0 then begin
        c.obj.width := self.width;
        c.obj.height := self.height;
      end else begin
        c.obj.width := random(round(self.width));
        c.obj.height := random(round(self.height));
      end;

      if t = 0 then
        c.obj.Fill.Color := (cardinal(255 shl 24)+random($FFFFFF))
      else
        c.obj.Fill.Color := (cardinal(random(128) shl 24)+random($FFFFFF));
      default_curtains[t] := c;
      c.obj.bringtofront;
      c.obj.Opacity := 1.0;
      c.obj.Visible := true;
    until IsOutSide(c.obj.position.point, c.obj);
  end;
end;

procedure TCurtains.SetState(const Value: TCurtainState);
begin
  if value <> Self.Fstate then
    last_state_change_time := GetTicker;
  FState := Value;


end;


destructor TfrmFMXBase.Destroy;
begin
  ActiveCommands.WaitForAll;
  ActiveCommands.ClearAndDestroyCommands;

  inherited;
  ActiveCommands.free;

end;

procedure TfrmFMXBase.DoBoundsSet;
begin
  Control_IterateChildren(self, procedure (c: TFMXobject; var stop: boolean)
    begin
      stop := false;
      if c is TScaledLayoutProportional then
        TScaledLayoutProportional(C).ForceRealign;
    end);



end;

procedure TfrmFMXBase.DoClose(var CloseAction: TCloseAction);
begin
  inherited;
  UnregisterWithMockMobile;

end;

procedure TfrmFMXBase.DoHide;
begin
  inherited;
  UnregisterWithMockMobile;
end;

procedure TfrmFMXBase.DoUpdatestate;
begin
  //
end;

function TfrmFMXBase.GetControl<T>(parent: TControl): T;
begin
  result := TGuiHelper.control_Getcontrol<T>(parent);
end;

procedure TfrmFMXBase.HardWork(proc, guiSuccess: TProc);
var
  cl: TCommandLIst<TCommand>;
begin
  WorkingHard := true;
  try
    cl := ActiveCommands;
    if mock <> nil then
      cl := Tmm(mock).ActiveCommands;

    cl.Add(InlineProcWithGuiEx(proc, procedure begin guisuccess() end, procedure(s: string) begin WorkError(s) end));
    WatchCommands;
  finally
    WorkingHard := false;
  end;

end;

procedure TfrmFMXBase.HardWork(proc: TProc);
var
  cl: TCommandLIst<TCommand>;
begin
  WorkingHard := true;
  try
    cl := ActiveCommands;
    if mock <> nil then
      cl := Tmm(mock).ActiveCommands;

    cl.Add(InlineProcWithGuiEx(proc, procedure begin end, procedure(s: string) begin WorkError(s) end));
    WatchCommands;
  finally
    WorkingHard := false;
  end;

end;

function TfrmFMXBase.InitCurtains: boolean;
begin
  result := false;
  //creates curtain objects

  //this is the default curtain
  for var t := 0 to high(default_curtains) do begin
    var default_curtain := default_curtains[t].obj;
    if default_curtain = nil then begin
      default_curtain := TRectangle.create(self);
      default_curtain.parent := self;
      default_curtain.Width := 1;
      default_curtain.height := 1;
      default_curtain.position.x := -10;
      default_curtain.position.y := -10;
      default_curtain.Fill.Color := (cardinal(random(128) shl 24)+random($FFFFFF));
      default_curtain.Stroke.Kind := TBrushKind.bkNone;

      default_curtains[t].obj := default_curtain;
      result := true;
    end;
  end;

end;

procedure TfrmFMXBase.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  DoBoundsSet;

end;

procedure TfrmFMXBase.ShowMessage(sMessage: string);
begin
  fmx.Dialogs.showmessage(sMessage);
end;

procedure TfrmFMXBase.ToggleBusy(busy: boolean);
begin
  //no implementation requried
end;

procedure TfrmFMXBase.Transition(proc: TProc);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TfrmFMXBase.UnregisterWithMockMobile;
begin
  if mock = nil then
    exit;

  if mock is Tmm then
    Tmm(mock).RemoveForm(self);

  mock := nil;
end;

procedure TfrmFMXBase.UpdateMouseCursor;
begin
{$IFDEF MSWINDOWS}
  (TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService).SetCursor(self.cursor);
//  var h := WindowHandleToPlatform(self.Handle);
//  Windows.SetCursor( self.cursor);
//  PostMessage(h.Wnd, WM_PAINT, 0,0);
//  PostMessage(h.Wnd, CM_CURSORCHANGED, 0,0);

//  FMsgSys.PostMessage(CM_CURSORCHANGED, 0,0);

//  PostMessage(s, CM_CURSORCHANGED, WParam, LParam);
//  Perform( $B00F, 0, 0 );

{$ENDIF}
end;

procedure TfrmFMXBase.UpdateState;
begin
  updateFromModel;
  DoupdateState;
end;

procedure TfrmFMXBase.UpdateFromModel;
begin
  //
end;

procedure TfrmFMXBase.WatchCommand(c: TCommand; bTakeOwnership: boolean);
begin
  if not ActiveCommands.Has(c) then ActiveCommands.Add(c);
  if c.OwnedByProcessor then
    raise ECritical.create('cannot wait on a free-on-complete command');

end;

procedure TfrmFMXBase.Watch(bTakeOwnership: boolean; c: TCommand);
begin
  if mock <> nil then
    Tmm(mock).Watch(bTakeOwnerShip, c)
  else begin
    ToggleBusy(true);
    WAtchCommand(c, bTakeOwnership);
  end;
end;

function TfrmFMXBase.WatchCommands: boolean;
begin

  if mock <> nil then begin
    result := Tmm(mock).WatchCommands;
    if result then
      exit;
  end;

  var wererunning := ActiveCommands.count = 0;
  if ActiveCommands.count > 0 then begin
    ToggleBusy(true);
    if activecommands[0].IsComplete then begin
      var c := activecommands[0];

      activecommands.delete(0);
      c.free;
      c := nil;
    end;
  end;
  result := ActiveCommands.count = 0;
  if result and (result <> wererunning) then begin
//    Debug.Log(CLR_F+'*********************************************************');
    Debug.Log(CLR_F+'********* ALL COMMANDS watched by '+self.classname+' ARE COMPLETE');
//    Debug.Log(CLR_F+'*********************************************************');
  end;
  ToggleBusy(not result);

end;



procedure TfrmFMXBase.WorkError(sMessage: string);
begin
  showmessage(sMessage);

end;

end.
