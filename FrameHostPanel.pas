unit FrameHostPanel;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, forms, typex;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TFrameHostPanel = class(TPanel)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Paint;override;
  published
    procedure TakeFrame(frm: TFrame);
    procedure TakeFrame2(frm: TFrame);
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Digital Tundra', [TFrameHostPanel]);
end;

{ TFrameHostPanel }

procedure TFrameHostPanel.Paint;
begin
//  inherited;

end;

procedure TFrameHostPanel.TakeFrame(frm: TFrame);
var
  al: TAlign;
  anc: TAnchors;
  t,l,w,h: ni;
begin
  al := self.Align;
  anc := self.Anchors;
  t := self.top;
  l := self.left;
  w := self.Width;
  h := self.height;
  try
    self.Align := alNone;
    self.Anchors := [akLeft, akTop];
    self.width := frm.Width;
    self.Height := frm.Height;
    frm.Parent := self;
    frm.Align := alClient;
  finally
    self.Anchors := anc;
    self.Align := al;
    self.width := w;
    self.height := h;
    self.top := top;
    self.Left := left;
  end;









end;

procedure TFrameHostPanel.TakeFrame2(frm: TFrame);
begin
  frm.Left := 0;
  frm.Top := 0;

  frm.width := self.ClientWidth;
  frm.Height := self.clientheight;
  frm.Parent := self;
  frm.Anchors := [akLeft, akTop, akBottom, akRight];
  frm.Align := alClient;
  //frm.Resize;
end;

end.
