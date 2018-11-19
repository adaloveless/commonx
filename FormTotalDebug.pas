unit FormTotalDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormBase, FormBGThreadWatcher, Vcl.ComCtrls, FrameTotalDebug;

type
  TfrmTotalDebug = class(TfrmBase)
    procedure frmBaseCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
//    framDbg: TframTotalDebug;
    fram: TfrmBGThreadWatcher;
  end;

var
  frmTotalDebug: TfrmTotalDebug;

implementation

{$R *.dfm}

procedure TfrmTotalDebug.frmBaseCreate(Sender: TObject);
begin
  inherited;
  fram := TfrmBGThreadWatcher.create(self);
  fram.parent := self;
//  framDbg := TframTotalDebug.create(self);
//  framDbg.parent := self;
//  framDbg.align := alClient;
end;

end.
