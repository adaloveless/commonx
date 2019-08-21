unit FormFMXBase;

interface

uses
{$IFDEF MSWINDOWS}
  windows, Winapi.Messages, Winapi.IpTypes, fmx.platform.win, fmx.platform,
{$ENDIF}
  guihelpers_fmx, SCALEDlayoutproportional, commandprocessor, systemx, typex,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, fmx_messages,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, system.messaging;

const
  CM_CURSORCHANGED = $B00F;
type
  TfrmFMXBase = class(TForm)
  private
    FTransplanted: boolean;
    { Private declarations }

  protected
    ActiveCommands: TCommandList<TCommand>;
    {$IFDEF MSWINDOWS}
    FMsgSys: TMessagingSystem;
    {$ENDIF}

    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoHide; override;
    procedure WatchCommand(c: TCommand; bTakeOwnership: boolean);
  public
    mock: TForm;
    function WatchCommands: boolean;
    { Public declarations }

    function GetControl<T: TControl>(parent: TControl): T;

    procedure ActivateOrTransplant;virtual;
    procedure ActivateByPush;virtual;
    procedure ActivateByPop;virtual;
    procedure UnregisterWithMockMobile;
    procedure UpdateMouseCursor;
    constructor Create(AOwner: TComponent); override;

    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    destructor Destroy; override;
    property Transplanted: boolean read FTransplanted write FTransplanted;
    procedure DoBoundsSet;virtual;
    procedure UpdateFromModel; virtual;
    procedure UpdateState;
    procedure DoUpdatestate;virtual;
    procedure ToggleBusy(busy: boolean);virtual;
    procedure Watch(bTakeOwnership: boolean; c: TCommand);


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

procedure TfrmFMXBase.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  DoBoundsSet;

end;

procedure TfrmFMXBase.ToggleBusy(busy: boolean);
begin
  //no implementation requried
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

  if ActiveCommands.count > 0 then begin
    ToggleBusy(true);
    if activecommands[0].IsComplete then begin
      var c := activecommands[0];

      activecommands.delete(0);
      c.free;
      c := nil;
    end;
  end;
  result := not (ActiveCommands.count > 0);
  ToggleBusy(not result);

end;



end.
