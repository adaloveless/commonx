unit FormBaseAdvanced;

interface
{x$DEFINE USE_ANON_THREAD}
{$DEFINE NEW_COMMAND_FLOW}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FrameBusyFMX,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, typex,systemx, guihelpers_fmx, FrameBaseFMX,
  FMX.Objects, commandprocessor, FMX.Controls.Presentation, FMX.StdCtrls, AnonCommand,
  FormFMXBase, tickcount, betterobject, stringx;

type
{$IFNDEF USE_ANON_THREAD}
  TLocalBackground = TAnonymousCommand<boolean>;
{$ELSE}
  TLocalBackground = TAnonymousThread<boolean>;
{$ENDIF}



  TfrmBaseAdvanced = class(TfrmFMXBase, IUnknown)
    BusyTimer: TTimer;
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
{$ELSE}
{$ENDIF}
    FTakeOwnershipOfbackgroundCommand: boolean;
    InBGOp: boolean;
    procedure ShowFancy(show: boolean);
    procedure BeforeDestruction;override;
    { Private declarations }
    function BackgroundOp(AThreadFunc: TFunc<boolean>;
      AOnFinishedProc: TProc<boolean>; AOnErrorProc: TProc<Exception>;
      bAutoDestroy: boolean=true): TLocalBackground;
  private
  protected
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

    procedure ShowMessage(m: string);override;
    procedure ToggleBusy(working: Boolean);override;

  end;

var
  frmBaseAdvanced: TfrmBaseAdvanced;

implementation

uses
  FormMockMobile, debug;

{$R *.fmx}

{ TfrmBase }

function TfrmBaseAdvanced.BackgroundOp(AThreadFunc: TFunc<boolean>;
  AOnFinishedProc: TProc<boolean>; AOnErrorProc: TProc<Exception>;
  bAutoDestroy: boolean): TLocalBackground;
begin
  InBGOp := true;
  var bgop := TLocalBackground.Create(AthreadFunc,
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
    result := bgop;//<--don't return an autodestroy thread,
                            //it might be dead before this function exists
  Watch(bAutoDestroy, bgop);
//  WaitForCommand(FBackGroundOp, bAutoDestroy);

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
{$IFDEF OLD_BUSY}
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
{$ENDIF}

end;

constructor TfrmBaseAdvanced.Create(AOwner: TComponent);
begin
  ics(Frefsect);
  inherited;

end;

destructor TfrmBaseAdvanced.Destroy;
begin
  Debug.Log(CLR_F+'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  Debug.Log(CLR_F+'Destroying form: '+classname);
  Debug.Log(CLR_F+'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');

//  BGCmd.WaitForAll;

  dcs(FRefSect);
  inherited;
end;

procedure TfrmBaseAdvanced.Detach;
begin
  detached := true;
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
    Tmm(mock).ShowFancy;


  end else begin
    fancyAnim.DisposeOf;
    fancyAnim := nil;
  end;

end;

procedure TfrmBaseAdvanced.ShowMessage(m: string);
begin
  mm.TemporaryMessage(CRLF+m+CRLF);
end;

function TfrmBaseAdvanced._AddRef: Integer;
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

function TfrmBaseAdvanced._RefCount: Integer;
begin
  ecs(FRefSect);
  result := FRefCount;
  lcs(FRefSect);

end;

function TfrmBaseAdvanced._Release: Integer;
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






procedure TfrmBaseAdvanced.ToggleBusy(working: Boolean);
begin

  if working=true then
  begin
{$IFDEF OLDBUSY}
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
{$ENDIF}
    if assigned(mock) then
      Tmm(mock).ShowFancy;
  end else
  begin
    if assigned(mock) then
      Tmm(mock).HideFancy;
{$IFDEF OLDBUSY}
    BusyRect.Visible := false;
    BusyTimer.Enabled:=false;
    BusyCircle.Visible:=false;
    ShowFancy(working);
{$ENDIF}
  end;
  self.Cursor := crDefault;
  UpdateMouseCursor;
  UpdateState;
  self.Invalidate;

end;
















end.


