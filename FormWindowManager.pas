unit FormWindowManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, stdctrls,
  Dialogs, FormBase, ButtonGroup, ExtCtrls, Generics.collections.fixed, betterobject, velocitypanel,
  GlassControls, FrameHostPanel, Menus, typex, guihelpers, geometry;


type
  TLOCAL_CHILD_WINDOW_CLASS = TfrmBase;

  TfrmWindowManager = class;//forward

  TWindowInfo = class(TBetterObject)
  private
    FIndex: integer;
    FActive: boolean;
    FManager: TfrmWindowManager;
    function GetParent: TWinControl;
    procedure SetParent(const Value: TWinControl);
    procedure SetIndex(const Value: integer);
    procedure SetACtive(const Value: boolean);
  public
    Form: TLOCAL_CHILD_WINDOW_CLASS;
    Widget: TVelocityPanel;
    Button: TButton;
    constructor Create;override;
    destructor Destroy;override;
    property Parent: TWinControl read GetParent write SetParent;
    property Index: integer read FIndex write SetIndex;

    property Active: boolean read FActive write SetACtive;

    procedure OnWidgetclick(sender: TObject);

    property Manager: TfrmWindowManager read FManager write FManager;
    procedure Rig;
  end;

  TfrmWindowManager = class(TfrmBase)
    tmRearrange: TTimer;
    panWindows: TFrameHostPanel;
    menuWinMan: TMainMenu;
    Monitor1: TMenuItem;
    Monitor2: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    N51: TMenuItem;
    procedure frmBaseCreate(Sender: TObject);
    procedure frmBaseDestroy(Sender: TObject);
    procedure tmRearrangeTimer(Sender: TObject);
    procedure Monitor2Click(Sender: TObject);
    procedure N51Click(Sender: TObject);
  private
    { Private declarations }
    FWindows: TList<TWindowInfo>;
    FActiveForm: TfrmBase;
    function GetWindowCount: integer;
    function GetWindows(idx: integer): TWindowInfo;
    procedure SetActiveForm(const Value: TfrmBase);
  public
    { Public declarations }
    procedure ChildForm_Activated(frm: TLOCAL_CHILD_WINDOW_CLASS);
    procedure ChildForm_DeActivated(frm: TLOCAL_CHILD_WINDOW_CLASS);
    procedure ChildForm_Minimized(frm: TLOCAL_CHILD_WINDOW_CLASS);
    procedure ChildForm_Maximized(frm: TLOCAL_CHILD_WINDOW_CLASS);
    procedure ChildForm_Hidden(frm: TLOCAL_CHILD_WINDOW_CLASS);
    procedure ChildForm_Shown(frm: TLOCAL_CHILD_WINDOW_CLASS);
    procedure ChildForm_Moved(frm: TLOCAL_CHILD_WINDOW_CLASS);
    procedure ChildForm_Resized(frm: TLOCAL_CHILD_WINDOW_CLASS);


    property Windows[idx: integer]: TWindowInfo read GetWindows;
    property WindowCount: integer read GetWindowCount;
    function IndexOfForm(frm: TLOCAL_CHILD_WINDOW_CLASS): integer;

    procedure DetachWindows;
    procedure RegisterWindow(frm: TLOCAL_CHILD_WINDOW_CLASS);
    procedure UnRegisterWindow(frm: TLOCAL_CHILD_WINDOW_CLASS);
    procedure StartRearrange;
    procedure ResyncButtons;

    procedure _WM_GETMINMAXINFO(var mmInfo : TWMGETMINMAXINFO ); message wm_GetMinMaxInfo;

    property ActiveForm: TfrmBase read FActiveForm write SetActiveForm;

    function CreateSingletonForm(c: TfrmBaseClass): TfrmBAse;
    function FindformByToken(tok: string): TfrmBAse;
    function HasFormToken(tok: string): boolean;
  end;

var
  frmWindowManager: TfrmWindowManager;

implementation

{$R *.dfm}

{ TfrmWindowManager }

procedure TfrmWindowManager.ChildForm_Activated(frm: TLOCAL_CHILD_WINDOW_CLASS);
begin

  activeForm := frm;

end;

procedure TfrmWindowManager.ChildForm_DeActivated(
  frm: TLOCAL_CHILD_WINDOW_CLASS);
begin
  if activeform = frm then
    Activeform := nil;
end;

procedure TfrmWindowManager.ChildForm_Hidden(frm: TLOCAL_CHILD_WINDOW_CLASS);
begin
  //no implementation required
end;

procedure TfrmWindowManager.ChildForm_Maximized(frm: TLOCAL_CHILD_WINDOW_CLASS);
begin
  //no implementation required
end;

procedure TfrmWindowManager.ChildForm_Minimized(frm: TLOCAL_CHILD_WINDOW_CLASS);
begin
  //no implementation required
end;

procedure TfrmWindowManager.ChildForm_Moved(frm: TLOCAL_CHILD_WINDOW_CLASS);
begin
  //no implementation required
end;

procedure TfrmWindowManager.ChildForm_Resized(frm: TLOCAL_CHILD_WINDOW_CLASS);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TfrmWindowManager.ChildForm_Shown(frm: TLOCAL_CHILD_WINDOW_CLASS);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TfrmWindowManager.CreateSingletonForm(c: TfrmBaseClass): TfrmBAse;
begin
  result := TfrmBase(c.create(self));
  result.Manager := self;

end;

procedure TfrmWindowManager.DetachWindows;
var
  t: integer;
begin
  while windowcount > 0 do begin
    Windows[0].form.Detach;
  end;

end;

function TfrmWindowManager.FindformByToken(tok: string): TfrmBAse;
var
  t: ni;
begin
  for t:= 0 to windowcount-1 do begin
    if windows[t].Form.Token = tok then
      exit(windows[t].Form);
  end;
  exit(nil);

end;

procedure TfrmWindowManager.frmBaseCreate(Sender: TObject);
begin
  inherited FormCreate(sender);
  FWindows := Tlist<TWindowInfo>.create;
end;

procedure TfrmWindowManager.frmBaseDestroy(Sender: TObject);
begin
  DetachWindows;
  inherited;
  FWindows.free;
end;

function TfrmWindowManager.GetWindowCount: integer;
begin
  result := FWindows.count;
end;

function TfrmWindowManager.GetWindows(idx: integer): TWindowInfo;
begin
  result := FWindows[idx];
end;

function TfrmWindowManager.HasFormToken(tok: string): boolean;
begin
  result := FindFormByToken(tok) <> nil;
end;

function TfrmWindowManager.IndexOfForm(frm: TLOCAL_CHILD_WINDOW_CLASS): integer;
var
  t: integer;
begin
  result := -1;
  for t:= 0 to FWindows.count-1 do begin
    if FWindows[t].form = frm then begin
      result := t;
      break;
    end;
  end;

end;

procedure TfrmWindowManager.Monitor2Click(Sender: TObject);
begin
  n11.enabled := true;
  n21.enabled := screen.MonitorCount > 1;
  n31.enabled := screen.MonitorCount > 2;
  n41.enabled := screen.MonitorCount > 3;
  n51.enabled := screen.MonitorCount > 4;

end;

procedure TfrmWindowManager.N51Click(Sender: TObject);
var
  rc, scr,dsk: TRect;
  tbWidthSubtract, tbHeightSubtract:  ni;
begin
  inherited;
  rc := guihelpers.GetTaskBardimensions;

  scr.Left := screen.Monitors[TControl(sender).Tag].Left;
  scr.Right := scr.Left+screen.Monitors[TControl(sender).Tag].width-1;
  scr.Top := screen.Monitors[TControl(sender).Tag].Top;
  scr.Bottom := scr.Top + screen.Monitors[TControl(sender).Tag].height-1;
  dsk := RectMinusRectEdge(scr, rc);

  self.align := alNone;
  self.Left := dsk.Left;
  self.Top := dsk.Top;
  self.height := dsk.Height;
  self.width:= dsk.width;
//  self.align := alLeft;
end;

procedure TfrmWindowManager.RegisterWindow(frm: TLOCAL_CHILD_WINDOW_CLASS);
var
  wi: TWindowInfo;
begin
  wi := TWindowInfo.create;
  wi.Parent := panWindows;
  wi.form := frm;
  wi.Manager := self;

  FWindows.add(wi);

  ResyncButtons;
  StartRearrange;
end;

procedure TfrmWindowManager.ResyncButtons;
var
  t: integer;
begin
  for t:= 0 to WindowCount-1 do begin
    Windows[t].Index := t;
    Windows[t].Rig;
  end;

end;

procedure TfrmWindowManager.SetActiveForm(const Value: TfrmBase);
var
  i: integer;
begin
  i := IndexOfForm(FActiveForm);
  if (i>=0) then begin
    Windows[i].Active := false;
  end;

  FActiveForm := value;


  //no implementation required
  i := IndexOfForm(value);

  if i < 0 then exit;


  Windows[i].Active := true;

//  IF aCTIVEfORM <> nil then begin
//    glassimage1.picture.bitmap.assign(activeform.icon);
//  end;

end;

procedure TfrmWindowManager.StartRearrange;
begin
  ResyncButtons;
  tmRearrange.enabled := true;
end;

procedure TfrmWindowManager.tmRearrangeTimer(Sender: TObject);
var
  t: integer;
  b: boolean;
begin
  inherited;
  for t:= 0 to WindowCount-1 do begin
    Windows[t].widget.UpdateSequence;
  end;


  b  := false;
  for t:= 0 to WindowCount-1 do begin
    if Windows[t].widget.InMotion then begin
      b := true;
      break;
    end;
  end;

  tmRearrange.enabled := b;


end;

procedure TfrmWindowManager.UnRegisterWindow(frm: TLOCAL_CHILD_WINDOW_CLASS);
var
  wi: TWindowInfo;
  i: integer;
begin
  i := IndexOfForm(frm);
  if i < 0 then
    exit;

  wi := FWindows[i];
  FWindows.delete(i);
  wi.free;


  StartRearrange;
end;

{ TWindowInfo }

constructor TWindowInfo.Create;
begin
  inherited;
  Widget := TVelocityPanel.create(nil);
  Widget.Height := 35;
  Widget.Onclick := self.OnWidgetClick;
  Widget.ParentColor := false;
  Widget.BevelWidth := 3;
  Widget.AccelRate := 0.001;
  Widget.DecelRate := 0.001;
  Button := TButton.create(widget);
  Button.Parent := widget;
  Button.Align := alCLient;
  Button.Onclick := self.OnWidgetClick;


end;

destructor TWindowInfo.Destroy;
begin
  Widget.free;
  inherited;
end;


function TWindowInfo.GetParent: TWinControl;
begin
  result := Widget.Parent;
  Widget.Width := Parent.Width;
end;

procedure TWindowInfo.OnWidgetclick(sender: TObject);
begin
  Manager.Windows[Index].Form.WindowState := wsMinimized;
  Manager.Windows[Index].Form.WindowState :=   Manager.Windows[Index].Form.PreviousWindowState;

  Manager.Windows[Index].Form.Show;
  Manager.Windows[Index].Form.SetFocus;

end;

procedure TWindowInfo.Rig;
begin
  Widget.Caption := Form.Caption;
  Button.Caption := Form.Caption;

end;

procedure TWindowInfo.SetACtive(const Value: boolean);
begin
  FActive := Value;
  Widget.ParentColor := false;
  if FActive then begin
    Widget.BevelOuter := bvLowered;
    Widget.Color := clNavy;
    Button.Font.Style := [fsBold];

  end else begin
    Widget.BevelOuter := bvRaised;
    Widget.Color := clWindow;
    Button.Font.Style := [];
  end;

end;

procedure TWindowInfo.SetIndex(const Value: integer);
begin
  FIndex := Value;
  widget.TargetLeft := 0;
  widget.TargetTop := (value * (widget.height+3));
  widget.InMotion := true;
  widget.TargetWidth := widget.parent.width;
  widget.TargetHeight := 32;

end;

procedure TWindowInfo.SetParent(const Value: TWinControl);
begin
  Widget.Parent := value;
  Widget.Width := Widget.Parent.ClientWidth;
end;


procedure TfrmWindowManager._WM_GETMINMAXINFO(var mmInfo: TWMGETMINMAXINFO);
var
  i: integer;
begin
//  tagMINMAXINFO = packed record
//    ptReserved: TPoint;
//    ptMaxSize: TPoint;
//    ptMaxPosition: TPoint;
//    ptMinTrackSize: TPoint;
//    ptMaxTrackSize: TPoint;
//  end;

  exit;
  if Manager = nil then exit;




  if GetMyScreen = TfrmBase(Manager).GetMyScreen then begin
    i := GetMyScreen;

    mmInfo.MinMaxinfo.ptMaxPosition.x := (screen.monitors[i].Left+manager.Width);
    mmInfo.MinMaxinfo.ptMaxPosition.y := screen.monitors[i].Top;
    mmInfo.MinMaxinfo.ptMaxSize.x := screen.monitors[i].Width-manager.Width;
//    mmInfo.MinMaxinfo.ptMaxSize.y := screen.monitors[i].Height;


  end;





end;


end.
