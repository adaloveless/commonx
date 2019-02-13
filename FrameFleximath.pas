unit FrameFleximath;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FrameBase, Vcl.ComCtrls, jsonhelpers, guihelpers, fleximath,
  Vcl.ExtCtrls, FormBase;

type
  TFramFlexiMath = class;//forward

  TMovingItem = record
    item: TControl;
    offset: TPoint;
  end;

  TfmDisplayMode = (fmScalar, fmTable, fmTree);
  TFramFlexiMath = class(TfrmFrameBase)
    tv: TTreeView;
    panTitle: TPanel;
    procedure panTitleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure panTitleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure panTitleMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FDisplayMode: TfmDisplayMode;
    FAutoDisplaymode: boolean;
    FMoving: boolean;
    FNode: string;
    procedure SetDisplayMode(const Value: TfmDisplayMode);
    procedure SetAutoDisplayMode(const Value: boolean);
    function GetIdentifier: string;
    procedure SetIdentifier(const Value: string);
    function GetNode: string;
    procedure SetNode(const Value: string);
    { Private declarations }
  public
    { Public declarations }
//    fmv: TFramFleximath;
    scene: TfrmBase;
    procedure SyncJSON(json: TJSON);
    procedure SyncFlexiMath(fm: TFlexiMath);
    property DisplayMode: TfmDisplayMode read FDisplayMode write SetDisplayMode;
    property AutoDisplaymode: boolean read FAutoDisplaymode write SetAutoDisplayMode;
    property Moving: boolean read FMoving;
    property Identifier: string read GetIdentifier write SetIdentifier;
    property Node: string read GetNode write SetNode;
    procedure RefreshData;
    procedure DisplayEditor;virtual;

  end;

implementation

uses
  uConcept;

{$R *.dfm}

{ TFramFlexiMath }

procedure TFramFlexiMath.DisplayEditor;
begin

end;

function TFramFlexiMath.GetIdentifier: string;
begin
  result := panTitle.Caption;
end;

function TFramFlexiMath.GetNode: string;
begin
  result := FNode;
end;


procedure TFramFlexiMath.panTitleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if button = mbLeft then begin
    TfrmConcept(scene).BeginItemMove(self,point(x,y));
    FMoving := true;
  end;
end;

procedure TFramFlexiMath.panTitleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if moving then begin
    TfrmConcept(scene).ContinueItemMove(point(x,y), panTitle);
  end;
end;

procedure TFramFlexiMath.panTitleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if button = mbLeft then begin
    TfrmConcept(scene).EndItemMove();
    FMoving := false;
  end;
  if button = mbRight then begin
    DisplayEditor;
  end;
end;

procedure TFramFlexiMath.RefreshData;
begin
  if (scene <> nil) and (Node <> '') then begin
  end;
end;

procedure TFramFlexiMath.SetAutoDisplayMode(const Value: boolean);
begin
  FAutoDisplaymode := Value;
end;

procedure TFramFlexiMath.SetDisplayMode(const Value: TfmDisplayMode);
begin
  FDisplayMode := Value;
end;

procedure TFramFlexiMath.SetIdentifier(const Value: string);
begin
  panTitle.caption := value;
end;

procedure TFramFlexiMath.SetNode(const Value: string);
begin
  FNode := value;
  RefreshData;
end;


procedure TFramFlexiMath.SyncFlexiMath(fm: TFlexiMath);
begin
  FlexiMathtoTreeView(fm, tv);
end;

procedure TFramFlexiMath.SyncJSON(json: TJSON);
begin
  JSONtoTreeView(json, tv);

end;

end.
