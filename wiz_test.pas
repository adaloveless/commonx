unit wiz_test;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormWizard, Vcl.StdCtrls;

type
  TfrmWizard1 = class(TfrmWiz)
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWizard1: TfrmWizard1;

implementation

{$R *.dfm}

end.
