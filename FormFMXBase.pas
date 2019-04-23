unit FormFMXBase;

interface

uses
{$IFDEF MSWINDOWS}
  windows, Winapi.Messages, Winapi.IpTypes, fmx.platform.win, fmx.platform,
{$ENDIF}
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
    {$IFDEF MSWINDOWS}
    FMsgSys: TMessagingSystem;
    {$ENDIF}

    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoHide; override;

  public
    mock: TForm;
    { Public declarations }

    procedure ActivateOrTransplant;virtual;
    procedure UnregisterWithMockMobile;
    procedure UpdateMouseCursor;
    constructor Create(AOwner: TComponent); override;
    property Transplanted: boolean read FTransplanted write FTransplanted;
  end;

var
  frmFMXBase: TfrmFMXBase;

implementation

uses
  FormMockMobile;

{$R *.fmx}

{ TfrmFMXBase }

procedure TfrmFMXBase.ActivateOrTransplant;
begin
  DoShow;
  if assigned(onActivate) then
    OnActivate(self);
end;

constructor TfrmFMXBase.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF MSWINDOWS}
  FMsgSys := TMessagingSystem.create(self);
{$ENDIF}
//  FMsgSys.RegisterMessageHandler(  <<---- you can use this to subscribe to windows messages if NEEDED



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

procedure TfrmFMXBase.UnregisterWithMockMobile;
begin
  if mock = nil then
    exit;

  if mock is TfrmMockMobile then
    TfrmMockMobile(mock).RemoveForm(self);

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

end.
