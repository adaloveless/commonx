unit dmx_pantilt_frame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus,
  Vcl.StdCtrls,
  typex, dmx_objects, dmx_light_definitions, dmx_winforms, graphicwincontrol,
  Vcl.ColorGrd, Vcl.ComCtrls;

type
  TDMXPanTiltFrame = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    popCoordSelect: TPopupMenu;
    Delete1: TMenuItem;
    Defalt1: TMenuItem;
    Timer1: TTimer;
    panstuff: TPanel;
    lbpoints: TListBox;
    lblights: TListBox;
    procedure Defalt1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure lbpointsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbpointsKeyPress(Sender: TObject; var Key: Char);
    procedure lbpointsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lblightsClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    { Public declarations }

    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure xlbpointsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure xlbpointsKeyPress(Sender: TObject; var Key: Char);
    procedure xlbpointsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);


  published
    spantilt: TSimplePantiltcontrol;
    pantilt: TPanTiltControl;
    stage: TDMXStage;
    plane: TDMXPLane;
    procedure pantilt_onfocus(sender: tObject);
    procedure spantilt_change(sender: TDMXFixtureControl; x,y: nativefloat);
    procedure PositionMe;

  end;

implementation

{$R *.dfm}

constructor TDMXPanTiltFrame.Create(AOwner: TComponent);
var
  ul: TUniverseList<TDMXTiltPan>;
  l: TDMXTiltPan;
  ut: TUT_GWC;
  sl: TUniverseList<TDMXChannelCluster>;
  t: ni;
begin
  inherited;
  spantilt := TSimplePanTiltControl.create(self);
  spantilt.parent := panstuff;
  spantilt.Left := 700;
  spantilt.top := 0;
  spantilt.OnChange := self.Spantilt_Change;
  spantilt.Visible := false;


  pantilt := TPanTiltControl.create(self);
  pantilt.Parent := self;
  pantilt.Left := 0;
  pantilt.Top := 0;
  pantilt.OnFocus := self.pantilt_onfocus;
  pantilt.spantilt := spantilt;



  stage := Multiverse.Stage;

  stage.LoadData;
  plane := stage.planes[0];

  ul := Multiverse.Groups.ByName['spot'].GetList<TDMXTiltPan>();
  sl := Multiverse.Groups.ByName['spot'].GetList<TDMXChannelCluster>();



//  pantilt.lights.add(l);
  pantilt.plane := plane;
  pantilt.light := ul[0];
  pantilt.lights := ul;

  for t:= 0 to sl.Count-1 do begin

    if sl[t] is TDMXQSpot14 then begin
      TDMXQSpot14(sl[t]).speed := 1.0;
      TDMXQSpot14(sl[t]).Color.Color := clWhite;
      TDMXQSpot14(sl[t]).Intensity := 1.0;
    end;

    if sl[t] is TDMXAccuSpot250 then begin
      TDMXAccuSpot250(sl[t]).speed := 1.0;
      TDMXAccuSpot250(sl[t]).Color.Color := clWhite;
      TDMXAccuSpot250(sl[t]).Intensity := 1.0;
    end;

  end;

  sl.free;

  //  ul.free;

  pantilt.LightLIst := lblights;
  pantilt.POintLIst := lbpoints;

end;

procedure TDMXPanTiltFrame.Defalt1Click(Sender: TObject);
begin
  pantilt.DefaultPoints;
end;

procedure TDMXPanTiltFrame.Delete1Click(Sender: TObject);
begin
  pantilt.DeletePoint(lbpoints.ItemIndex);
end;

destructor TDMXPanTiltFrame.Destroy;
begin
  spantilt.free;
  spantilt := nil;
  pantilt.free;
  pantilt := nil;
  inherited;
end;

procedure TDMXPanTiltFrame.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  pantilt.KeyDown(key, shift);
end;

procedure TDMXPanTiltFrame.lblightsClick(Sender: TObject);
begin
  pantilt.onselectlight(self);
//  pantilt.invalidate;
end;

procedure TDMXPanTiltFrame.lbpointsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  xlbpointsKeyDown(sender, key, shift);
end;

procedure TDMXPanTiltFrame.lbpointsKeyPress(Sender: TObject; var Key: Char);
begin
  xlbpointskeypress(sender, key);
end;

procedure TDMXPanTiltFrame.lbpointsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  xlbpointskeyup(sender, key, shift);
end;

procedure TDMXPanTiltFrame.xlbpointsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  pantilt.KeyDown(key, shift);
  key := 0;

end;

procedure TDMXPanTiltFrame.xlbpointsKeyPress(Sender: TObject; var Key: Char);
begin
  pantilt.KeyPress(key);
  key := #0;

end;

procedure TDMXPanTiltFrame.xlbpointsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  pantilt.KeyUp(key, shift);
  key := 0;

end;

procedure TDMXPanTiltFrame.pantilt_onfocus(sender: tObject);
begin
  pantilt.RefreshLights;

end;

procedure TDMXPanTiltFrame.PositionMe;
begin
  pantilt.width := pantilt.height;
  pantilt.height := parent.height;
  pantilt.width := parent.height;

  panstuff.left := pantilt.width;
  panstuff.width := parent.width - panstuff.left;
  panstuff.height := parent.height;

end;

procedure TDMXPanTiltFrame.SetParent(Aparent: TWinControl);
begin
  inherited;
  if aparent <> nil then
    positionme;

end;

procedure TDMXPanTiltFrame.spantilt_change(sender: TDMXFixtureControl; x,
  y: nativefloat);
begin
  pantilt.X := x;
  pantilt.Y := y;

end;

end.
