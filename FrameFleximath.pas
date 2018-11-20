unit FrameFleximath;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FrameBase, Vcl.ComCtrls, jsonhelpers, guihelpers, fleximath;

type
  TFramFlexiMath = class(TfrmFrameBase)
    tv: TTreeView;
  private
    { Private declarations }
  public
    { Public declarations }
//    fmv: TFramFleximath;
    procedure SyncJSON(json: TJSON);
    procedure SyncFlexiMath(fm: TFlexiMath);
  end;

implementation

{$R *.dfm}

{ TFramFlexiMath }

procedure TFramFlexiMath.SyncFlexiMath(fm: TFlexiMath);
begin
  FlexiMathtoTreeView(fm, tv);
end;

procedure TFramFlexiMath.SyncJSON(json: TJSON);
begin
  JSONtoTreeView(json, tv);

end;

end.
