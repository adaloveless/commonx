unit FormFMXBaseAdvanced;
{$I DelphiDefs.inc}
interface
{x$DEFINE USE_ANON_THREAD}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FrameBusyFMX, tickcount, numbers,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, typex,systemx, guihelpers_fmx, FrameBaseFMX,
  FMX.Objects, commandprocessor, FMX.Controls.Presentation, FMX.StdCtrls, AnonCommand, formfmxbase, FormFMXOverlay;

type
{$IFNDEF USE_ANON_THREAD}
  TLocalBackground = TAnonymousCommand<boolean>;
{$ELSE}
  TLocalBackground = TAnonymousThread<boolean>;
{$ENDIF}

  TOnCommandFinished = procedure (c: TCommand) of object;
  TOnCommandFinishedThen = reference to procedure (c: Tcommand);

  Tcmd_TestSleep=class(TCommand)
  public
    ms: ni;
    procedure DoExecute;override;
  end;

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
    procedure ToggleBusy(working: Boolean);override;
    procedure BeforeDestruction;override;
    { Private declarations }
    function BackgroundOp(AThreadFunc: TFunc<boolean>;
      AOnFinishedProc: TProc<boolean>; AOnErrorProc: TProc<Exception>;
      bAutoDestroy: boolean=true): TLocalBackground;
  protected
    procedure DoUpdateState;virtual;
    procedure DoUpdateCommandProgress(status: string; prog: TProgress);
      override;

  public
    detached: boolean;
    fancyAnim: TFramBusyFMX;
    overlay: TfrmFMXOverlay;
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

    function UseOverlay: boolean;

    procedure TestSleep(ms: ni);
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

  WatchCommands;
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

procedure TfrmFMXBaseAdvanced.DoUpdateCommandProgress(status: string;
  prog: TProgress);
begin
  inherited;
  if overlay <> nil then begin
    overlay.UpdateCommandProgress(status, prog);
  end;
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
    if UseOverlay then begin
      Self.disableAllControls;
      if overlay = nil then begin
        Self.overlay := FormFMXOverlay.TfrmFMXOverlay.create(self);
        if fancyAnim <> nil then exit;
        fancyAnim := TframBusyFMX.Create(self);
        fancyAnim.Parent := overlay;
        fancyAnim.Align := TAlignLayout.Contents;
        fancyAnim.bringtofront;
        fancyAnim.AnimateTransitionIn;
        overlay.Parent := self;
//        overlay.Width := self.Width;
//        overlay.Height := self.Height div 3;
//        overlay.Top := self.top + (self.Height div 3);
//        overlay.Left := self.left;
        overlay.Show;
        var prog: TProgress;
        prog.step := 0;
        prog.stepcount := 100;
        overlay.UpdateCommandProgress('...',prog);
      end;
    end else begin

      if fancyAnim <> nil then exit;
      fancyAnim := TframBusyFMX.Create(self);
      fancyAnim.Parent := BusyRect;
      fancyAnim.Align := TAlignLayout.Client;
    end;


  end else begin
    self.ReenableDisabledControls;
    fancyAnim.DisposeOf;
    fancyAnim := nil;
    overlay.DisposeOf;
    overlay := nil;
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


procedure TfrmFMXBaseAdvanced.TestSleep(ms: ni);
begin
  var c := Tcmd_TestSleep.Create;
  c.ms := ms;
  c.Start;
  self.WatchCommand(c,true);


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

function TfrmFMXBaseAdvanced.UseOverlay: boolean;
begin
{$IFDEF DESKTOP}
  exit(true);
{$ELSE}
  exit(false);
{$ENDIF}

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

{ Tcmd_TestSleep }

procedure Tcmd_TestSleep.DoExecute;
begin
  inherited;

  var tmStart := getticker;
  Status := 'Sleeping...';
  Step := 0;
  StepCount := ms;
  repeat
    sleep(lesserof(ms, 100));
    step := gettimesince(tmStart);
    if gettimesince(tmStart) >= ms then
      break;

  until false;
end;

end.

