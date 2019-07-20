unit FrameBaseVCL;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, debug, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TfrmFrameBase = class(TFrame)
  private
    FUpdatingState: boolean;
    FOnupdateState: TNotifyEvent;
    { Private declarations }
  public
    constructor Create(aowner: TComponent);override;
    property UpdatingState: boolean read FUpdatingState write FUpdatingState;
    procedure UpdateState;
    procedure ForceResize(source: TWinControl);
    procedure Init;virtual;
    { Public declarations }
  published
    property OnupdateState: TNotifyEvent read FOnupdateState write FOnUpdateState;
    procedure DisableAllControls(bDisabled: boolean);
    procedure WndProc(var Message: TMessage);override;
  end;

implementation

{$R *.dfm}

{ TfrmFrameBase }

constructor TfrmFrameBase.Create(aowner: TComponent);
begin
  inherited;
  Init;
end;

procedure TfrmFrameBase.DisableAllControls(bDisabled: boolean);
var
  t: integer;
begin
  for t:= 0 to ComponentCount-1 do begin
    if components[t] is TControl then begin
      TControl(components[t]).Enabled := not bDisabled;
    end;
  end;

end;

procedure TfrmFrameBase.ForceResize(source: TWinControl);
begin
  exit;
  if source = self then begin
    width := parent.width;
    height := parent.height;
  end;
  resizing(wsNormal);

  source.doublebuffered := false;
  if source.parent is TWincontrol then begin
    ForceResize(source.parent as TwinControl);
//    TWinControl(Parent).doublebuffered := false;

  end;


end;

procedure TfrmFrameBase.Init;
begin
  //nop
end;

procedure TfrmFrameBase.UpdateState;
var
  bOld: boolean;
begin
  bold := UpdatingState;
  UpdatingState := true;
  try
    if Assigned(OnUpdateState) then
      OnUpdateState(self);
  finally
    updatingState := bOld;
  end;


end;

procedure TfrmFrameBase.WndProc(var Message: TMessage);
begin
  //GLOG.Debug('msg:'+inttostr(message.Msg));
  inherited;

end;

end.

