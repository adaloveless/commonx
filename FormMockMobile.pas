unit FormMockMobile;

//this unit is intended to allow windows apps to
//behave similarly to mobile apps.

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, typex,
  formFMXBase, FMX.Objects, FMX.Controls.Presentation, mBASEFORM, better_collections;

type
  TfrmMockMobile = class(TfrmFMXBase)
    PanelParent: TPanel;
  private
    function GetShowingForm: TfrmFMXBase;
  protected
    formstack: TBetterList<TfrmFMXBase>;
    procedure DoShow; override;
    { Private declarations }
    procedure PushForm(f: TfrmFMXBase);
    function PopForm: TfrmFMXBase;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    { Public declarations }
    property showingform: TfrmFMXBase read GetShowingForm;
    procedure TakeControls;
    procedure GiveBackControls;
    procedure ShowForm(f:TfrmFMXBase);
    constructor Create(AOwner: TComponent); override;
    procedure RemoveForm(f: TfrmFMXBase);
  end;

type
  TFormClass = class of TfrmFMXBase;
var
  frmDefault: TFormClass;
  frmMockMobile: TfrmMockMobile;

procedure MM_ShowForm(frm: TfrmFMXBase);
procedure MM_CloseForm(frm: TfrmFMXBase);



implementation

uses debug;



procedure MM_ShowForm(frm: TfrmFMXBase);
begin
  //Transfer controls from form to this one
  if frm = nil then
    raise ECritical.create('trying to show a nil form');
  frmMockMobile.ShowForm(frm);



end;

{$R *.fmx}

{ TfrmMockMobile }

procedure TfrmMockMobile.AfterConstruction;
begin
  inherited;

end;

procedure TfrmMockMobile.BeforeDestruction;
begin
  inherited;
  for var t := 0 to formstack.count-1 do begin
    var f := formstack.Items[t];
    f.mock := nil;
  end;
  formstack.free;

end;

constructor TfrmMockMobile.Create(AOwner: TComponent);
begin
  inherited;
  formstack := TBetterList<TfrmFMXBase>.create;
  Debug.Log(self.ClassName+' created.');
end;

procedure TfrmMockMobile.DoShow;
begin
  inherited;
  if showingform = nil then begin
    showform(frmdefault.create(application));
  end;

end;

function TfrmMockMobile.GetShowingForm: TfrmFMXBase;
begin
  if formstack.count = 0 then
    exit(nil);
  result := formstack.Last;
end;

procedure TfrmMockMobile.GiveBackControls;
begin
  if showingform = nil then
    exit;
  showingform.transplanted := false;
  var hitend := false;
  repeat
    hitend := true;
    for var t:= 0 to self.ChildrenCount-1 do begin
      var c := self.Children[t];
      if c.owner = showingform then begin
        Debug.Log('giving back control: '+c.name);
        hitend := false;
        c.Parent := showingform;
        break;
      end;
    end;
  until hitend;
end;

function TfrmMockMobile.PopForm: TfrmFMXBase;
begin
  result := showingform;
  formstack.remove(result);
end;

procedure TfrmMockMobile.PushForm(f: TfrmFMXBase);
begin
  formstack.add(f);
end;

procedure TfrmMockMobile.RemoveForm(f: TfrmFMXBase);
begin
  if f = showingform then
    GiveBackControls;
  formstack.remove(f);

  if showingform <> nil then
    TakeControls;
end;

procedure TfrmMockMobile.ShowForm(f: TfrmFMXBase);
begin
  if showingform <> nil then begin
    showingform.Parent := nil;
    giveBackControls;
//    showingform.Hide;
  end;


  f.mock := self;
  PushForm(f);
  if showingform <> nil then begin
    showingform.parent := self;
    showingform.left := 0;
    showingform.width := width;
    showingform.top := 0;
    showingform.height := height;
    TakeControls;
    showingform.ActivateOrTransplant;
//    showingform.show;
  end;

  show;

end;

procedure TfrmMockMobile.TakeControls;
begin
  if showingform = nil then
    exit;
  var hitend := false;
  repeat
    hitend := true;
    for var t:= 0 to showingform.ChildrenCount-1 do begin
      var c := showingform.Children[t];
      Debug.Log('taking control: '+c.name);
      hitend := false;
      c.Parent := self;
      break;
    end;
  until hitend;

  showingform.transplanted := true;
end;

procedure MM_CloseForm(frm: TfrmFMXBase);
begin
  frm.UnregisterWithMockMobile;

end;

end.
