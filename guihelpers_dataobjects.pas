unit guihelpers_dataobjects;

interface

uses
  debug,dataobject, typex, systemx, sysutils,
  stringx, classes, controls,
  storageenginetypes, extctrls, guihelpers,
  stdctrls, variants, vcl.comctrls;


procedure SyncDataObjectTocomboBox(d: TDataObject; cb: TComboBox; sField: string);
procedure SyncDataObjectToListView(d: TDataObject; lv: TListView);
procedure SyncDataObjectToListViewByFieldName(d: TDataObject; lv: TListView);


implementation


procedure SyncDataObjectTocomboBox(d: TDataObject; cb: TComboBox; sField: string);
var
  t: ni;
begin
  cb.Items.Clear;
  for t:= 0 to d.objectcount-1 do begin
    cb.Items.Add(vartostr(d.fld[sfield].AsString));
  end;

end;






procedure SyncDataObjectToListView(d: TDataObject; lv: TListView);
var
  objidx, t: ni;
  v: string;
  fc: ni;
  oSub: TDataObject;
begin
  if d = nil then begin
    lv.clear;
    exit;
  end;

  if d.ObjectCount = 0 then
    fc := 0
  else
    fc := d.obj[0].FieldCount;
  SyncListView(lv, d.objectcount, fc);
  for objidx := 0 to d.ObjectCount-1 do begin
    oSub := d.obj[objidx];
    lv.items[objidx].Caption := inttostr(objidx);
    for t:= 0 to fc-1 do begin
      v := oSub.FieldByIndex[t].AsString;
      lv.Items[objidx].SubItems[t] := vartostrex(v);
      if objidx = 0 then
        lv.columns[t+1].Caption := oSub.fieldbyIndex[t].Name;
    end;
  end;
end;

procedure SyncDataObjectToListViewByfieldName(d: TDataObject; lv: TListView);
var
  objidx, t: ni;
  v: string;
  fc: ni;
  oSub: TDataObject;
  sColumnTitle: string;
begin
  Debug.Log('sync:'+lv.Name);
  if d = nil then begin
    lv.clear;
    exit;
  end;

  if d.ObjectCount = 0 then
    fc := 0
  else
    fc := d.obj[0].FieldCount;
  SyncListView(lv, d.objectcount, lv.Columns.count-1);
  for objidx := 0 to d.ObjectCount-1 do begin
    oSub := d.obj[objidx];
    sColumnTitle := lv.Columns[0].Caption;
    lv.items[objidx].Caption := osub[sColumnTitle].AsString;
    for t:= 1 to lv.Columns.count-1 do begin
      sColumnTitle := lv.Columns[t].Caption;
      v := oSub[sColumnTitle].AsString;
      lv.Items[objidx].SubItems[t-1] := v;
    end;
  end;
end;


end.
