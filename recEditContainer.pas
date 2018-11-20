unit RecEditContainer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FormPropertyDialogFixed, Vcl.ExtCtrls,
  FrameHostPanel, Vcl.StdCtrls, system.rtti, systemx, typex, guihelpers;

type
  TpanRecordEdit = class(TPanel)
  private
    { Private declarations }
  public
    { Public declarations }
    containers: array of TPanel;
    edits: array of TControl;

    procedure LoadFromRecord(inst: pointer; typeinfoOfrec: pointer);
    procedure SavetoRecord(inst: pointer; typeinfoOfrec: pointer);
    procedure Clear;
  end;


implementation

{ TpanRecordEdit }

procedure TpanRecordEdit.Clear;
begin
  guihelpers.ClearChildren(self, self);
end;

procedure TpanRecordEdit.LoadFromRecord(inst, typeinfoOfrec: pointer);
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
  Clear;

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
      pnl.Parent := self;
      ed := TLabeledEdit.create(self);
      ed.parent := pnl;
      ed.Top := 0;
      ed.Left := 150;
      ed.LabelPosition := lpLeft;
      ed.EditLabel.Caption := fld.Name;
      ed.Text := Data.ToString;
      ed.Tag := vartype(data.AsVariant);
      edits[t] := ed;
      pnl.Align := alTop;
      pnl.BorderStyle := bsNone;
    end;

  finally
    FContext.free;
  end;

end;


procedure TpanRecordEdit.SavetoRecord(inst, typeinfoOfrec: pointer);
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
  v: variant;
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

      v := VarAsType(ed.Text, ed.Tag);
      data := data.FromVariant(v);
      fld.SetValue(inst, data);
    end;

  finally
    FContext.free;
  end
end;

end.
