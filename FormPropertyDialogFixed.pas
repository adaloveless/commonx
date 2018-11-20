unit FormPropertyDialogFixed;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormBase, Vcl.StdCtrls, Vcl.ExtCtrls,
  FrameHostPanel;

type
  TfrmPropertyDialogFixed = class(TfrmBase)
    btnOK: TButton;
    btnCancel: TButton;
    panBody: TFrameHostPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPropertyDialogFixed: TfrmPropertyDialogFixed;

implementation

{$R *.dfm}

end.
