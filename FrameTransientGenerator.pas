unit FrameTransientGenerator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, tickcount,
  Dialogs, FrameInsertable, StdCtrls, ComCtrls, ExtCtrls, SoundEditorFrame, systemx;

type
  TfrmTransientGenerator = class(TfrmInsertableFrame)
    Panel3: TPanel;
    btnGo: TButton;
    panstuff: TPanel;
    procedure btnGoClick(Sender: TObject);
  private
    FAxis: integer;
    FEditor: TfrmSoundEditor;
    FLastDisplayUpdate: cardinal;
    function GetEditingCurve: TCurveList;
    function GEtEditingMarks: TSoundMarkerList;
    { Private declarations }
  public
    { Public declarations }
    property Editor: TfrmSoundEditor read FEditor write FEditor;
    property EditingCurve: TCurveList read GetEditingCurve;
    property EditingMarks: TSoundMarkerList read GEtEditingMarks;
    procedure Wipe;virtual;
    procedure UpdateDisplay(bOptional: boolean);virtual;

    procedure Generate;virtual;
  end;

  TransientGeneratorClass = class of TfrmTransientGenerator;

var
  frmTransientGenerator: TfrmTransientGenerator;

implementation

{$R *.dfm}

procedure TfrmTransientGenerator.btnGoClick(Sender: TObject);
begin
  inherited;
  if Editor.editingchannel < 0 then exit;
  if Editor.editingAxis < 0 then exit;
  Wipe;
  Generate;
  UpdateDisplay(false);
  Editor.DirtyDisplay;
end;

procedure TfrmTransientGenerator.Generate;
begin

//TODO -cunimplemented: unimplemented block
end;

function TfrmTransientGenerator.GetEditingCurve: TCurveList;
begin
  result := Editor.ParamChannels[Editor.EditingChannel][Editor.EditingAxis];
end;

function TfrmTransientGenerator.GEtEditingMarks: TSoundMarkerList;
begin
  result := Editor.ParamChannels[Editor.EditingChannel].Markers;
end;

procedure TfrmTransientGenerator.UpdateDisplay(bOptional: boolean);
var
  tm: cardinal;
begin
  if bOptional then begin
    tm := GEtTickCount;
    if GetTimeSince(tm, FLastDisplayUpdate) > 400 then begin
      Editor.PanelPaint;
      fLastDisplayUpdate := tm;
    end;
  end else begin
    Editor.PanelPaint;
  end;


end;

procedure TfrmTransientGenerator.Wipe;
begin
  EditingCurve.clear;
end;

end.
