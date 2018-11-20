unit FormPropertyDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormBase, ExtCtrls, StdCtrls, DB, SqlExpr, FrameHostPanel;

type
  TfrmPropertyDialog = class(TfrmBase)
    btnOK: TButton;
    btnCancel: TButton;
    panBody: TFrameHostPanel;
    procedure frmBaseCreate(Sender: TObject);
  private
    FOnConnectToDAta: TNotifyEvent;
    FNewMode: boolean;
    { Private declarations }
  public
    { Public declarations }
    function ShowForNew: TModalResult;virtual;
    function ShowForEdit: TModalResult;virtual;
    procedure DoConnectToData;virtual;
  published
    property NewMode: boolean read FNewMode write FNewMode;
    property OnConnectToData: TNotifyEvent read FOnConnectToDAta write FOnConnectToDAta;
  end;

implementation

uses ProgressForm;

{$R *.dfm}

procedure TfrmPropertyDialog.DoConnectToDAta;
begin
  BeginProgress;
  ShowProgress('Please wait...', 0, 1,0);
  try
    if Assigned(OnConnectToDAta) then
      OnConnectToDAta(self);
  finally
    EndProgress;
  end;
end;

procedure TfrmPropertyDialog.frmBaseCreate(Sender: TObject);
begin
  inherited;
  panBody.caption := '';

end;

function TfrmPropertyDialog.ShowForEdit: TModalResult;
begin
  NewMode := false;
  DoConnectToData;
  result := ShowModal;
end;

function TfrmPropertyDialog.ShowForNew: TModalResult;
begin
  NewMode := false;
  DOConnectToDAta;
  result := ShowModal;
end;

end.
