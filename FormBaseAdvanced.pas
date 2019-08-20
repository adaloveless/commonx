unit FormBaseAdvanced;

interface
{x$DEFINE USE_ANON_THREAD}
{$DEFINE NEW_COMMAND_FLOW}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FrameBusyFMX,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, typex,systemx, guihelpers_fmx, FramBaseFMX,
  FMX.Objects, commandprocessor, FMX.Controls.Presentation, FMX.StdCtrls, AnonCommand,
  FormFMXBase, tickcount, betterobject;

type
{$IFNDEF USE_ANON_THREAD}
  TLocalBackground = TAnonymousCommand<boolean>;
{$ELSE}
  TLocalBackground = TAnonymousThread<boolean>;
{$ENDIF}

  TOnCommandFinished = procedure (c: TCommand) of object;
  TOnCommandFinishedThen = reference to procedure (c: Tcommand);


  TfrmBaseAdvanced = class(TfrmFMXBase, IUnknown)
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
    ActiveCommands: TCommandList<TCommand>;
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
    procedure WaitforCommandsToComplete;
    function WatchCommands: boolean;

    property ActiveCommand: TCommand read FWaitingOn;
    procedure UpdateState;
    procedure ShowMessage(m: string);


  end;

var
  frmBaseAdvanced: TfrmBaseAdvanced;

implementation

uses
  FormMockMobile;

{$R *.fmx}

{ TfrmBase }

function TfrmBaseAdvanced.BackgroundOp(AThreadFunc: TFunc<boolean>;
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

procedure TfrmBaseAdvanced.BeforeDestruction;
begin

  Detach;
  if _RefCount > 1 then begin
    FreeWithReferences := true;
    raise EAbort.create('Trying to free '+self.ClassName+' with more than 1 reference');

  end;

  inherited;

end;

procedure TfrmBaseAdvanced.BusyTimerTimer(Sender: TObject);
begin

  Busy.StartAngle:=Busy.StartAngle+10;
  if Busy.StartAngle>359 then
    Busy.StartAngle:=0;

{$IFDEF NEW_COMMAND_FLOW}
  WatchCommands;

{$ELSE}
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
{$ENDIF}

end;

constructor TfrmBaseAdvanced.Create(AOwner: TComponent);
begin
  ics(Frefsect);
  ActiveCommands := TCommandList<TCommand>.create;
  ActiveCommands.RestrictedtoThreadID := Tthread.Currentthread.threadid;
  inherited;

end;

destructor TfrmBaseAdvanced.Destroy;
begin

  if assigned(FWaitingOn) then begin
    if not InBGOp then
      FWaitingOn.WaitFor;
    FWaitingOn.Free;
    FWaitingOn := nil;
  end;

  ActiveCommands.WaitForAll;
  ActiveCommands.ClearAndDestroyCommands;

//  BGCmd.WaitForAll;

  dcs(FRefSect);
  inherited;

  Activecommands.free;
end;

procedure TfrmBaseAdvanced.Detach;
begin
  detached := true;
end;

procedure TfrmBaseAdvanced.DoUpdateState;
begin
  //no implementation required
end;

procedure TfrmBaseAdvanced.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//  Action := TCloseAction.caFree;
//  YOU DONT WANT THIS, because each form might have a wild pointer pointing to
//  it and you will therefore have to save frmWhatever := nil in each formclose ANYWAY.
//  it is less confusing like this

end;

function TfrmBaseAdvanced.GetControl<T>(parent: TControl): T;
begin
  result := TGuiHelper.control_Getcontrol<T>(parent);
end;

procedure TfrmBaseAdvanced.Kill;
begin
  if bSelfRef then begin
    _Release;
  end else begin
    Destroy;
  end;
end;

function TfrmBaseAdvanced.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TfrmBaseAdvanced.ShowFancy(show: boolean);
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

procedure TfrmBaseAdvanced.ShowMessage(m: string);
begin
  mm.TemporaryMessage(m);
end;

function TfrmBaseAdvanced._AddRef: Integer;
begin
  EnterCriticalSection(FRefSect);
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

  LeaveCriticalSection(FRefSect);


end;

function TfrmBaseAdvanced._RefCount: Integer;
begin
  EnterCriticalSection(FRefSect);
  result := FRefCount;
  LeaveCriticalSection(FRefSect);

end;

function TfrmBaseAdvanced._Release: Integer;
begin

  EnterCriticalSection(FRefSect);
  dec(FRefCount);
  Result := FRefCount;
  LeaveCriticalSection(FRefSect);

  if (Result = 0) and FreeWithReferences then begin
{$IFDEF WINDOWS}
    Destroy;//<--- android has ARC, don't destroy on other platforms
{$ENDIF}
  end;

end;






procedure TfrmBaseAdvanced.ToggleBusy(working: Boolean);
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
    BusyRect.Align := TAlignLayout.contents;
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




procedure TfrmBaseAdvanced.UpdateState;
begin
  DoUpdateState;
end;

procedure TfrmBaseAdvanced.WaitForCommand(c: TCommand; bTakeOwnership: boolean;
  p: TOnCommandFinished);
begin
  FonCommandFinish := p;
  WaitForCommand(c, bTakeOwnerShip);
end;

procedure TfrmBaseAdvanced.WaitForCommand(c: TCommand; bTakeOwnership: boolean; p: TOnCommandFinishedThen);
begin
  FonCommandFinishedThen := p;
  WaitForCommand(c, bTakeOwnerShip);
end;


procedure TfrmBaseAdvanced.WaitforCommandsToComplete;
begin
  while ActiveCommands.count > 0 do begin
    WaitForCommand(activecommands[0], false);
    var c := activecommands[0];
    activecommands.delete(0);
    c.free;
    c := nil;
  end;
  ToggleBusy(false);

end;

function TfrmBaseAdvanced.WatchCommands: boolean;
begin
  if ActiveCommands.count > 0 then begin
    if activecommands[0].IsComplete then begin
      WaitForCommand(activecommands[0], false);
      var c := activecommands[0];
      activecommands.delete(0);
      if assigned(FonCommandFinishedThen) then
        FonCommandFinishedThen(c);
      if assigned(FonCommandFinish) then
        FonCommandFinish(c);
      c.free;
      c := nil;
    end;
  end;
  result := not (ActiveCommands.count > 0);
  ToggleBusy(not result);

end;

{$IFDEF USE_ANON_THREAD}
procedure TfrmBase.WaitForCommand(c: TThread; bTakeOwnership: boolean);
begin
  FTakeOwnershipOfbackgroundCommand := btakeOwnership;
  FWaitingOn := c;
  ToggleBusy(true);
end;
{$ELSE}
procedure TfrmBaseAdvanced.WaitForCommand(c: TCommand; bTakeOwnership: boolean);
begin
  if not ActiveCommands.Has(c) then ActiveCommands.Add(c);
  if c.OwnedByProcessor then
    raise ECritical.create('cannot wait on a free-on-complete command');
  FTakeOwnershipOfbackgroundCommand := btakeOwnership;
  FWaitingOn := c;
  ToggleBusy(true);
end;
{$ENDIF}



end.


