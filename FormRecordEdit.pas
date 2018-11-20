unit FormRecordEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormPropertyDialogFixed, Vcl.ExtCtrls,
  FrameHostPanel, Vcl.StdCtrls, system.rtti, systemx, typex;

type
  TfrmRecordEdit = class(TfrmPropertyDialogFixed)
  private
    { Private declarations }
  public
    { Public declarations }
    containers: array of TPanel;
    edits: array of TControl;

    procedure LoadFromRecord(inst: pointer; typeinfoOfrec: pointer);
    procedure SavetoRecord(inst: pointer; typeinfoOfrec: pointer);


  end;

var
  frmRecordEdit: TfrmRecordEdit;

implementation

{$R *.dfm}

{ TfrmRecordEdit }

procedure TfrmRecordEdit.LoadFromRecord(inst, typeinfoOfrec: pointer);
var
  FContext: TRTTIContext;
  typ: TRTTIType;
  rt: TRTTIRecordType;
  fld: TRttiField;
  data: TValue;
  Value: TValue;
  t: ni;
  a: TArray<TRTTIField>;
  pnl: TPanel;
  ed: TLabeledEdit;
begin
  typ := FContext.GetType(TypeInfoOfRec);
  rt := typ.AsRecord;
  a := rt.GetFields;

  FContext := TRTTIContext.create;
  try
    setlength(containers, length(a));
    setlength(edits, length(a));
    for t:= 0 to high(a) do begin
      fld := a[t];
      Data := fld.GetValue(inst);
      pnl := TPanel.create(self);
      pnl.Parent := self.panBody;
      ed := TLabeledEdit.create(self);
      ed.parent := pnl;
      ed.Top := 0;
      ed.Left := 150;
      ed.LabelPosition := lpLeft;
      ed.EditLabel.Caption := fld.Name;
      ed.Text := Data.ToString;
      edits[t] := ed;
      pnl.Align := alTop;
      pnl.BorderStyle := bsNone;
    end;

  finally
    FContext.free;
  end;

end;


procedure TfrmRecordEdit.SavetoRecord(inst, typeinfoOfrec: pointer);
var
  FContext: TRTTIContext;
  typ: TRTTIType;
  rt: TRTTIRecordType;
  fld: TRttiField;
  data: TValue;
  Value: TValue;
  t: ni;
  a: TArray<TRTTIField>;
  pnl: TPanel;
  ed: TLabeledEdit;
begin
  typ := FContext.GetType(TypeInfoOfRec);
  rt := typ.AsRecord;
  a := rt.GetFields;

  FContext := TRTTIContext.create;
  try
    for t:= 0 to high(a) do begin
      ed := edits[t] as TLabeledEdit;
      fld := a[t];
      Data := fld.GetValue(inst);
      pnl := TPanel.create(self);

      data := data.FromVariant(ed.Text);
      fld.SetValue(inst, data);
    end;

  finally
    FContext.free;
  end
end;

end.
