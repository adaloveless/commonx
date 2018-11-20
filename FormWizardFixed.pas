unit FormWizardFixed;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormPropertyDialogFixed, Vcl.StdCtrls,
  Vcl.ExtCtrls, FrameHostPanel, typex, generics.collections, FrameWizardPage,
  Vcl.ComCtrls;

type
  TfrmWizardFixed = class(TfrmPropertyDialogFixed)
    btnNext: TButton;
    btnPrevious: TButton;
    pcWizard: TPageControl;
    procedure frmBaseCreate(Sender: TObject);
    procedure frmBaseDestroy(Sender: TObject);
    procedure pcWizardChange(Sender: TObject);
    procedure frmBaseUpdateState(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
  private
    FPageIndex: ni;
    procedure SetPageIndex(const Value: ni);
    { Private declarations }
  protected
    FFrames: TList<TfrmWizardPage>;
    properPage: ni;
  public
    { Public declarations }
    procedure AddFrame(frm: TfrmWizardPage);overload;
    procedure AddFrame(frmC: TWizardFrameClass);overload;
    procedure DestroyFrames;
    property PageIndex: ni read FPageIndex write SetPageIndex;
    procedure UpdatePageVisibility;
    procedure CreatePages;virtual;
    procedure PageTurn(direction: ni = 1);
    procedure OnLeavingPage(fromPAge: TTabSheet; var can: boolean);virtual;
    procedure OnPageTurn(fromPage: ni; direction: ni; var Handled: boolean);virtual;
  end;

var
  frmWizardFixed: TfrmWizardFixed;

implementation

{$R *.dfm}

{ TfrmWizardFixed }

procedure TfrmWizardFixed.AddFrame(frm: TfrmWizardPage);
begin
  FFrames.add(frm);
  panBody.TakeFrame(frm);
  UpdatePageVisibility;

end;

procedure TfrmWizardFixed.AddFrame(frmC: TWizardFrameClass);
begin
  FFrames.add(frmC.create(self));
end;

procedure TfrmWizardFixed.btnNextClick(Sender: TObject);
begin
  inherited;
  PageTurn(1);
end;

procedure TfrmWizardFixed.btnPreviousClick(Sender: TObject);
begin
  inherited;
  PageTurn(-1);
  UpdateState;
end;

procedure TfrmWizardFixed.CreatePages;
begin
  //S
end;

procedure TfrmWizardFixed.DestroyFrames;
begin
  raise ECritical.create('not implemented');
  //
end;

procedure TfrmWizardFixed.frmBaseCreate(Sender: TObject);
begin
  inherited;
  FFrames := TList<TfrmWizardPage>.create;
  CreatePages;
  pcWizard.ActivePageIndex := 0;
  Updatestate;

end;

procedure TfrmWizardFixed.frmBaseDestroy(Sender: TObject);
begin
  inherited;
  FFrames.free;
  fFrames := nil;
end;

procedure TfrmWizardFixed.frmBaseUpdateState(Sender: TObject);
var
  bIsLastPage: boolean;
  bIsFirstPage: boolean;
begin
  inherited;
  if pcWizard.ActivePageIndex <> properpage then
    pcWizard.ActivePageIndex := properpage;

  bIsLastPage := pcWizard.ActivePageIndex = (pcWizard.PageCount-1);
  bIsFirstPage := pcWizard.ActivePageIndex = 0;
  btnOK.enabled := bIsLastPage;
  btnNext.Enabled := not bIsLastPage;
  btnNext.Visible := pcWizard.pagecount > 1;
  btnPrevious.Visible := pcWizard.pagecount > 1;
  btnPrevious.enabled := not bIsFirstPage;






end;

procedure TfrmWizardFixed.OnLeavingPage(fromPAge: TTabSheet; var can: boolean);
begin
  //
end;

procedure TfrmWizardFixed.OnPageTurn(fromPage, direction: ni;
  var Handled: boolean);
begin
  //

end;

procedure TfrmWizardFixed.PageTurn(direction: ni);
var
  handled: boolean;
  can: boolean;
begin
  can := true;
  handled := false;
  OnLeavingPage(pcWizard.ActivePage, can);

  if not can then
    exit;


  OnPageTurn(pcWizard.ActivePageIndex, direction, handled);

  if not handled then begin
    properPage := pcWizard.ActivePageIndex + direction;
    pcWizard.ActivePageIndex := pcWizard.ActivePageIndex + direction;

  end;

  Updatestate;

end;

procedure TfrmWizardFixed.pcWizardChange(Sender: TObject);
begin
  inherited;
  UpdateState;
end;

procedure TfrmWizardFixed.SetPageIndex(const Value: ni);
begin
  if value < 0 then exit;
  if value >= FFrames.count then exit;

  FPageIndex := Value;
  UpdatePageVisibility;
end;

procedure TfrmWizardFixed.UpdatePageVisibility;
var
  t: ni;
begin
  for t:= 0 to FFrames.count-1 do begin
    FFrames[t].Visible := (t=PAgeIndex);
  end;
  btnNext.visible := PageIndex < (FFrames.count-1);
  btnPrevious.visible := PageIndex > 0;
  btnOK.visible := not btnNext.Visible;
end;

end.
