unit GuiHelpers_DB;

interface

uses
  typex, systemx, stringx, classes, controls, storageenginetypes, extctrls, stdctrls, variants, vcl.comctrls;

procedure SyncRowSetTocomboBox(rs: TSERowSet; cb: TComboBox; sField: string);
procedure SyncRowSetToListView(rs: TSERowSet; lv: TListView);
procedure SyncRowSetToListViewFilterColumns(rs: TSERowSet; lv: TListView);



implementation

uses
  guihelpers, sysutils;


procedure SyncRowSetTocomboBox(rs: TSERowSet; cb: TComboBox; sField: string);
begin
  cb.Items.BeginUpdate;
  try
    cb.Items.Clear;
    rs.First;
    while not rs.EOF do begin
      cb.Items.Add(vartostr(rs[sfield]));
      rs.Next;
    end;
  finally
    cb.Items.EndUpdate;
  end;
end;




procedure SyncRowSetToListViewFilterColumns(rs: TSERowSet; lv: TListView);
var
  t: ni;
  cx: ni;
  idx: ni;
  v: variant;
  ci: ni;
begin
  cx := rs.rowcount;
  idx := 0;

  lv.items.BeginUpdate;
  try
    SyncListView(lv, rs.rowcount, lv.Columns.count);
    while cx > 0 do begin
      lv.items[idx].Caption := inttostr(idx);
      for t:= 0 to rs.fieldcount-1 do begin
        ci := IndexOfListColumn(lv, rs.FieldDefs[t].sName)-1;
        if ci >=0 then begin
          v := rs.Values[t,idx];
          lv.Items[idx].SubItems[ci] := vartostrex(v);
        end;
      end;
      dec(cx);
      inc(idx);
    end;
  finally
    lv.items.EndUpdate;
  end;
end;

procedure SyncRowSetToListView(rs: TSERowSet; lv: TListView);
var
  t: ni;
  cx: ni;
  idx: ni;
  v: variant;
begin
  cx := rs.rowcount;
  idx := 0;

  lv.items.BeginUpdate;
  try
    SyncListView(lv, rs.rowcount, rs.FieldCount);
    while cx > 0 do begin
      lv.items[idx].Caption := inttostr(idx);
      for t:= 0 to rs.fieldcount-1 do begin
        v := rs.Values[t,idx];
        lv.Items[idx].SubItems[t] := vartostrex(v);
        if idx = 0 then
          lv.columns[t+1].Caption := rs.fields[t].sName;
      end;
      dec(cx);
      inc(idx);
    end;
  finally
    lv.items.EndUpdate;
  end;
end;

end.
