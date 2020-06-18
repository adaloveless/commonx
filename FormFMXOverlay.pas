unit FormFMXOverlay;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, commandprocessor, typex, FMX.Ani, FMX.Objects;

type
  TfrmFMXOverlay = class(TForm)
    lblStatus: TLabel;
    ProgressBar: TProgressBar;
    Rectangle1: TRectangle;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateCommandProgress(status:string; prog: TProgress);
    { Public declarations }
  end;

var
  frmFMXOverlay: TfrmFMXOverlay;

implementation

{$R *.fmx}

{ TfrmFMXOverlay }

procedure TfrmFMXOverlay.FormCreate(Sender: TObject);
begin
  lblStatus.Text := '';
end;

procedure TfrmFMXOverlay.UpdateCommandProgress(status: string; prog: TProgress);
begin
  ProgressBar.Max := prog.stepcount;
  ProgressBar.Value := prog.step;
  lblStatus.Text := status;
  if self.Owner is TForm then begin
    var f := self.Owner as TForm;

    var pf := PointF(f.Left, f.top+(f.height div 3));
    self.Left := round(pf.x+32);
    self.Width := f.width-64;
    self.top := round(pf.y);
    self.Height := (f.Height div 3);

  end;

end;

end.
