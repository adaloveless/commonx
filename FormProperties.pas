unit FormProperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormDataAware, ExtCtrls, StdCtrls, FMTBcd, DB, SqlExpr, FormBase;

type
  TfrmProperties = class(TfrmDataAware)
    btnOK: TButton;
    btnCancel: TButton;
    panBody: TPanel;
    procedure frmBaseCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
  public

  end;

var
  frmProperties: TfrmProperties;

implementation

{$R *.dfm}


uses ProgressForm;

procedure TfrmProperties.btnCancelClick(Sender: TObject);
begin
  inherited;
  DoCancelData;
end;

procedure TfrmProperties.btnOKClick(Sender: TObject);
begin
  inherited;
  try
    DoSaveData;
  except
    modalresult := mrNone;
    raise;
  end;

end;

procedure TfrmProperties.frmBaseCreate(Sender: TObject);
begin
  inherited;
  panBody.caption := '';

end;


end.
