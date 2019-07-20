unit FormFMXBase;

interface

uses
{$IFDEF MSWINDOWS}
  windows, Winapi.Messages, Winapi.IpTypes, fmx.platform.win, fmx.platform,
{$ENDIF}
  guihelpers_fmx, SCALEDlayoutproportional,
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

    function GetControl<T: TControl>(parent: TControl): T;

    procedure ActivateOrTransplant;virtual;
    procedure UnregisterWithMockMobile;
    procedure UpdateMouseCursor;
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    property Transplanted: boolean read FTransplanted write FTransplanted;
    procedure DoBoundsSet;virtual;


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

function TfrmFMXBase.GetControl<T>(parent: TControl): T;
begin
  result := TGuiHelper.control_Getcontrol<T>(parent);
end;

procedure TfrmFMXBase.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  DoBoundsSet;

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
