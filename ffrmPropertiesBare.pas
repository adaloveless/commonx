unit ffrmPropertiesBare;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormBase, StdCtrls, ExtCtrls;

type
  TfrmPropertiesBare = class(TfrmBase)
    pnlBack: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPropertiesBare: TfrmPropertiesBare;

implementation

{$R *.dfm}

end.
