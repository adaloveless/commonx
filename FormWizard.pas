unit FormWizard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormPropertyDialog, Vcl.ExtCtrls, typex, systemx,
  Vcl.StdCtrls, FrameHostPanel, FrameWizardPage, generics.collections;

type
  TfrmWizard = class(TfrmPropertyDialog)
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure frmBaseUpdateState(Sender: TObject);
    procedure frmBaseCreate(Sender: TObject);
    procedure frmBaseDestroy(Sender: TObject);
  private
    FPageIndex: ni;
    procedure SetPageIndex(const Value: ni);
    { Private declarations }
  protected
    FFrames: TList<TfrmWizardPage>;
  public
    { Public declarations }
    procedure AddFrame(frm: TfrmWizardPage);overload;
    procedure AddFrame(frmC: TWizardFrameClass);overload;
    procedure DestroyFrames;
    property PageIndex: ni read FPageIndex write SetPageIndex;
    procedure UpdatePageVisibility;

  end;

var
  frmWizard: TfrmWizard;

implementation

{$R *.dfm}

{ TfrmWizard }

procedure TfrmWizard.AddFrame(frm: TfrmWizardPage);
begin
  FFrames.add(frm);
  panBody.TakeFrame(frm);
  UpdatePageVisibility;
end;

procedure TfrmWizard.AddFrame(frmC: TWizardFrameClass);
begin
  AddFrame(frmC.create(self));
end;

procedure TfrmWizard.btnNextClick(Sender: TObject);
begin
  inherited;
  PageIndex := PageIndex + 1;

end;

procedure TfrmWizard.btnPreviousClick(Sender: TObject);
begin
  inherited;
  PageIndex := PageIndex - 1;

end;

procedure TfrmWizard.DestroyFrames;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TfrmWizard.frmBaseCreate(Sender: TObject);
begin
  inherited;
  FFrames := TList<TfrmWizardPage>.create;

end;

procedure TfrmWizard.frmBaseDestroy(Sender: TObject);
begin
  inherited;
  FFrames.free;
  fFrames := nil;
end;

procedure TfrmWizard.frmBaseUpdateState(Sender: TObject);
begin
  inherited;
  UpdatePageVisibility;
end;

procedure TfrmWizard.SetPageIndex(const Value: ni);
begin
  if value < 0 then exit;
  if value >= FFrames.count then exit;

  FPageIndex := Value;
  UpdatePageVisibility;
end;

procedure TfrmWizard.UpdatePageVisibility;
var
  t: ni;
begin
  for t:= 0 to FFrames.count-1 do begin
    FFrames[t].Visible := (t=PAgeIndex);
  end;
end;

end.
