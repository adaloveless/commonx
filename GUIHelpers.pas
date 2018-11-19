unit GUIHelpers;

interface

uses
  system.rtti, debug, typex, classes, dbGrids, stdctrls, extctrls, comctrls, systemx, windows, vcl.forms, controls, numbers, json, jsonhelpers, sysutils, variants, fleximath, stringx, vcl.samples.spin, winapi.shellapi, richedit;


function Treenode_GetChildCount(tv: TTreeView; self: TTreenode): integer;
function TreeNode_GetChild(tv: TTreeView; self: TTreenode; n: ni): TTreenode;
function TreeView_FindRoot(tv: TTreeView; idx: integer): TTreeNode;
procedure TreeView_ExpandAll(tv: TTreeView);
function TreeView_FindRootIndex(tv: TTreeView; idx: integer): TTreeNode;deprecated;

procedure SyncTreeNode(tv: TTreeView; tn: TTreeNode; iCount: integer);overload;
procedure SyncTreeNode(tv: TTreeView; tn: TTreeNode; tns: TTreenodes; iCount: integer);overload;

procedure SyncListView(lv: TListView; iRows, iSub: integer);overload;
procedure SyncListViewToVolatileProgression(lv: TlistView; prog: PVolatileProgression; subCount: integer);
procedure SyncListView(lv: TlistView; prog: PVolatileProgression; subCount: integer);overload;

procedure SyncComboBox(cb: TComboBox; itemcount: ni);overload;
procedure SyncComboBox(cb: TComboBox; a: TArray<string>);overload;
function IndexOfListColumn(lv: TListview; sTitle: string): ni;



procedure SyncListBox(lb: TListBox; iCount: nativeint);
procedure StringListToListBox(lb: TListBox; sl: TStringlist);

procedure ClearChildren(aowner: TComponent; parent: TControl);

function SumDBGridColumnWidths(dbg: TDBGrid): integer;
procedure SetDynamicColumnWidth(dbg: TDbGrid; col: TColumn; iMin: integer; iMax: integer);
function GetPrimaryMonitor: Vcl.forms.TMonitor;

procedure SaveListViewAsTSV(lv: TListView; sFileName: string);

function IsValidScreenCoordinate(x,y: ni): boolean;
function FormFromControl(c: TControl): TForm;
procedure JSONToControl(json: TJSON; c: TControl);
procedure ControlToJSON(json: TJSON; c: TControl);
procedure JSONtoListView(json: TJSON; lv: TListView);
procedure JSONtoListViewColumns(json: TJSON; lv: TListView);
procedure JSONtoTreeView(json: TJSON; tv: TTreeView);
procedure JSONtoTreeNode(json: TJSON; tv: TTreeView; tn: TTreeNode);
procedure FlexiMathtoTreeView(fm: TFlexiMath; tv: TTreeView);
function ListView_SelectByCaption(lv: TListView; scap: string): boolean;
function Control_IsShowing(c: TControl): boolean;
procedure RecsToListView(lv: TListView; firstinst: pointer; stride: ni; count: ni; TypeInfoOfRec: pointer);
function GetTaskBardimensions: TRect;
procedure RichEdit_SetBGColor(re: TRichEdit; c: cardinal);

type
  TControlHelper = class helper for TControl
  private
    function GetBottom: ni;
    function GetRight: ni;
    procedure SetBottom(const Value: ni);
    procedure SetRight(const Value: ni);
  public
    property Bottom: ni read GetBottom write SetBottom;
    property Right: ni read GetRight write SetRight;
    procedure PositionBelow(cAbove: TControl; gap: ni = 0; match_width: boolean = true);
    procedure PositionAbove(cBelow: TControl; gap: ni = 0; match_width: boolean = true);
    procedure PositionLeft(cRight: TControl; gap: ni = 0; match_height: boolean = true);
    procedure PositionRight(cLeft: TControl; gap: ni = 0; match_height: boolean = true);
  end;



implementation

function SumDBGridColumnWidths(dbg: TDBGrid): integer;
var
  t: integer;
begin
  result := 0;
  for t:= 0 to dbg.columns.count-1 do begin
    inc(result, dbg.columns[t].width);
  end;

end;

procedure SetDynamicColumnWidth(dbg: TDbGrid; col: TColumn; iMin: integer; iMax: integer);
var
  i: integer;
begin
  i := GuiHelpers.SumDBGridColumnWidths(dbg);
  i := i - col.width;
  col.width := (dbg.clientwidth-i)-40;

end;

procedure SyncListView(lv: TListView; iRows, iSub: integer);
var
  itm: TListItem;
  t: integer;
begin
  lv.items.BeginUpdate;
  try
  iRows := abs(iRows);

  while lv.Columns.Count < (iSub+1) do begin
    lv.columns.Add.Width := 100;
  end;

  while lv.Columns.Count > (iSub+1) do begin
    lv.Columns.Delete(lv.columns.count-1);
  end;

  while (lv.items.count > iRows) do begin
    lv.items.delete(lv.items.count-1);
  end;

  while (lv.items.count < iRows) do begin
    itm := lv.items.add;
    while itm.subitems.count < iSub do begin
      itm.subitems.add('---');
    end;
  end;


  for t:= 0 to lv.items.count-1 do begin
    itm := lv.items[t];
    while itm.subitems.count < iSub do begin
      itm.subitems.add('---');
    end;
  end;
  finally
    lv.Items.EndUpdate;
  end;

end;

procedure SyncListViewToVolatileProgression(lv: TlistView; prog: PVolatileProgression; subCount: integer);
var
  itm: TListItem;
  iRows: integer;
begin
  lv.Items.BeginUpdate;
  try
    while not prog.Complete do begin
      iRows := prog.StepsCompleted;
  //    while (lv.items.count > iRows) do begin
  //      lv.items.delete(lv.items.count-1);
  //    end;

      while (lv.items.count < iRows) do begin
        itm := lv.items.add;
        while itm.subitems.count < subCount do begin
          itm.subitems.add('---');
        end;
      end;
      lv.Refresh();
      sleep(1);
    end;
    SyncListView(lv, prog.StepsCompleted, subcount);
  finally
    lv.Items.EndUpdate;
  end;

end;
procedure SyncListView(lv: TlistView; prog: PVolatileProgression; subCount: integer);
begin
  SyncListViewToVolatileprogression(lv, prog, subCount);
end;

procedure SyncTreeNode(tv: TTreeView; tn: TTreeNode; tns: TTreenodes; iCount: integer);
begin
  if tn <> nil then begin
    while (tns.Count < iCount) do begin
      tv.Items.AddChild(tn, '--');
    end;

    while (tns.Count > iCount) do begin
      tv.Items[tns.Count-1].Delete;

    end;
  end else begin
    while (tv.items.Count < iCount) do begin
      tv.Items.AddChild(tn, '--');
    end;

    while (tv.items.Count > iCount) do begin
      tv.Items[tn.Count-1].Delete;
    end;
  end;

end;

function Treenode_GetChildCount(tv: TTreeView; self: TTreenode): integer;
var
  t: ni;
begin
  //
  if self = nil then begin
    result := 0;
    for t:= 0 to tv.items.Count-1 do begin
      if tv.items[t].parent = nil then
        inc(result);
    end;
  end else begin
    result := 0;
    for t:= 0 to tv.items.Count-1 do begin
      if tv.items[t].parent = self then
        inc(result);
    end;
  end;
end;

function TreeView_FindRoot(tv: TTreeView; idx: integer): TTreeNode;
var
  t: integer;
  tt: Integer;
  vv: integer;
begin
  tt := 0;
  vv := -1;
  for t:= 0 to tv.Items.Count-1 do begin
    if tv.Items[t].Parent = nil then
      inc(tt);

    if (tt-1) = idx then begin
      vv := t;
      break;
    end;

  end;

  if (vv<0) then
    result := nil
  else
    result := tv.Items[vv];

end;

function TreeView_FindRootIndex(tv: TTreeView; idx: integer): TTreeNode;deprecated;
var
  t: integer;
  tt: Integer;
  vv: integer;
begin
  tt := 0;
  vv := -1;
  for t:= 0 to tv.Items.Count-1 do begin
    if tv.Items[t].Parent = nil then
      inc(tt);

    if (tt-1) = idx then begin
      vv := t;
      break;
    end;

  end;

  if (vv<0) then
    result := nil
  else
    result := tv.Items[vv];

end;



procedure SyncTreeNode(tv: TTreeView; tn: TTreeNode; iCount: integer);
var
  tnTemp: TTreeNode;
  cnt: ni;
begin
//  if tn = nil then
//    Debug.Log('node (nil) will have '+iCount.toString+' nodes.')
//  else
//    Debug.Log('node '+tn.text+' will have '+iCount.toString+' nodes.');
  if tn <> nil then begin
    while (Treenode_GetChildCount(tv, tn) < iCount) do begin
      tv.Items.AddChild(tn, '--');
    end;



    while (Treenode_GetChildCount(tv, tn) > iCount) do begin
//      Debug.Log('Count: '+ Treenode_GetChildCount(tv, tn).tostring);
      tnTemp := TreeNode_GetChild(tv,tn, Treenode_GetChildCount(tv, tn)-1);

//      Debug.Log('Deleting: '+ tnTemp.text);
      tnTemp.free;
//      tv.Items[tn.Count-1].Delete;
    end;

  end else begin
    while (Treenode_GetChildCount(tv,nil) < iCount) do begin
      tv.Items.Add(nil, '--');
    end;

    while (Treenode_GetChildCount(tv,nil) > iCount) do begin
      tv.Items[tv.items.Count-1].Delete;
    end;
  end;


end;

function GetPrimaryMonitor: TMonitor;
var
  t: integer;
begin

  result := nil;
  for t := 0 to screen.MonitorCount-1 do begin
    if screen.monitors[t].Primary then begin
      result := screen.monitors[t];
      break;
    end;
  end;


end;

procedure SyncListBox(lb: TListBox; iCount: nativeint);
begin
  if icount < 0 then exit;

  while lb.Items.Count < iCount do
    lb.Items.Add('');

  while lb.Items.Count > iCount do
    lb.Items.Delete(lb.Items.Count-1);


end;





{ TControlHelper }

function TControlHelper.GetBottom: ni;
begin
  result := Top + height;
end;

function TControlHelper.GetRight: ni;
begin
  result := Left + Width;
end;

procedure TControlHelper.PositionAbove(cBelow: TControl; gap: ni;
  match_width: boolean);
begin
  Top := cBelow.Top - (height+gap);
  if match_width then
    Width := cBelow.Width;
end;

procedure TControlHelper.PositionBelow(cAbove: TControl; gap: ni;
  match_width: boolean);
begin
  Top := cAbove.Bottom+gap;
  if match_width then
    Width := cAbove.Width;

end;

procedure TControlHelper.PositionLeft(cRight: TControl; gap: ni;
  match_height: boolean);
begin
  Left := cRight.Left - (Width+gap);
  if match_height then
    Height := cRight.Height;
end;

procedure TControlHelper.PositionRight(cLeft: TControl; gap: ni;
  match_height: boolean);
begin
  Left := cLeft.Right + gap;
  if match_height then
    Height := cLeft.Height;
end;

procedure TControlHelper.SetBottom(const Value: ni);
begin
  Top := GreaterOf(0, value-top);
end;

procedure TControlHelper.SetRight(const Value: ni);
begin
  Width := GreaterOf(0, value-left);
end;

procedure StringListToListBox(lb: TListBox; sl: TStringlist);
var
  t: ni;
begin
  SyncListBox(lb, sl.count);
  for t:= 0 to sl.count-1 do begin
    lb.Items[t] := sl[t];
  end;

end;

procedure SaveListViewAsTSV(lv: TListView; sFileName: string);
var
  sl: TStringlist;
  t,u: ni;
  li: TListItem;
  sLine: string;
begin
  sl := TStringlist.create;
  try
    for u := 0 to lv.items.count-1 do begin
      li := lv.items[u];
      sLine := li.caption;
      for t:= 0 to li.subitems.count-1 do begin
        sLine := sLine+#9+li.subitems[t];
      end;

      sl.add(sLine);
    end;

    sl.SavetoFile(sFilenAme);
  finally
    sl.free;
  end;

end;


function IsValidOnScreen(x,y: ni; screennum: ni): boolean;
begin
  if (x <   Screen.Monitors[screennum].Left) then
    exit(false);

  if (x > (Screen.Monitors[screennum].Left+Screen.Monitors[screennum].width-1)) then
    exit(false);

  if (y < screen.monitors[screennum].Top) then
    exit(false);

  if (y > (screen.monitors[screennum].Top+screen.monitors[screennum].Height-1)) then
    exit(false);

  exit(true);

end;

function IsValidScreenCoordinate(x,y: ni): boolean;
var
  t: ni;
begin
  for t:= 0 to screen.monitorcount-1 do begin
//    result := Screen.MonitorFromPoint(point(x,y), mdNull) <> nil;
      if IsVAlidOnScreen(x,y,t) then
        exit(true);
  end;

  result := false;

end;
function FormFromControl(c: TControl): TForm;
begin
  if c = nil then
    exit(nil);

  if c is TForm then
    exit(c as TForm)
  else
    exit(FormFromControl(c.Parent));
end;


procedure JSONtoListView(json: TJSON; lv: TListView);
var
  t,u: ni;
  s: string;
  j: TJSON;
begin

  if json.iCount < 1 then begin
    SyncListView(lv, 0,0);
    exit;
  end;

  SyncListView(lv, json.iCount, json.indexed[0].named.count);

  JSONtoListViewColumns(json.indexed[0], lv);
  for t:= 0 to lv.Items.Count-1 do begin
    j := json.indexed[t];
    lv.items[t].Caption := inttostr(t);
    for u := 0 to lv.items[t].subitems.count-1 do begin
      lv.items[t].SubItems[u] := json.indexed[t].named.ItemsByIndex[u].asstring;
    end;
  end;

end;

procedure JSONtoListViewColumns(json: TJSON; lv: TListView);
var
  c: TListcolumn;
  s: string;
  t: ni;
begin
  lv.Columns.Clear;
  c := lv.columns.add;
  c.caption := 'Row';
  for t:= 0 to json.ncount-1 do begin
    s := json.named.Keys[t];
    c := lv.Columns.Add;
    c.Width := 75;
    c.Caption := s;
  end;

end;


procedure JSONToControl(json: TJSON; c: TControl);
begin
  if c is TSpinEdit then begin
    with c as TSPinEdit do begin
      value := json.value;
    end;
  end;
end;

procedure ControlToJSON(json: TJSON; c: TControl);
begin
  if c is TSpinEdit then begin
    with c as TSPinEdit do begin
      json.value := value;
    end;
  end;
end;

procedure JSONtoTreeNode(json: TJSON; tv: TTreeView; tn: TTreeNode);
var
  t: ni;
  idx, iValueindex, iMemberIndex, iArrayBaseIndex: ni;
  s, s1,s2: string;
//  nSub: TTreenode;
  nRootSub: TTreenode;
  nArraySub: TTreeNode;
  nMemberSub: TTreeNode;
begin
  idx := 0;
  iMemberIndex := -1;
  iArrayBaseIndex := -1;
  iValueIndex := -1;

  if json.nCount > 0 then begin
    iMemberIndex := idx;
    inc(idx);
  end else
  if json.iCount > 0 then begin
    iArrayBaseIndex := idx;
    inc(idx);
  end else
  if tn = nil then begin
    iValueIndex := idx;
    inc(idx);
  end;

  //ROOTS of tree are handled differently than branches
  if tn = nil then begin
    //THIS IS A TREE ROOT
    SyncTreeNode(tv, nil, idx);

    if iValueIndex >=0 then begin
      nRootSub := TreeView_FindRoot(tv, iValueIndex);
      nRootSub.Text := {'Value: '+}vartostr(json.value);
    end;

    if iMemberIndex >=0 then begin
//      nSub := TreeView_FindRoot(tv, iMemberIndex);
//      nSub.Text := 'Members: ';
      nRootSub := TreeView_FindRoot(tv, iMemberIndex);
      nRootSub.Text := '{..}';
      SyncTreeNode(tv, nRootSub, json.ncount);

      s := '{';
      for t:= 0 to json.nCount-1 do begin
        nRootSub.item[t].Text := json.named.keys[t]+': ';
        JSONToTreenode(json[json.named.keys[t]], tv, nRootSub.item[t]);
        if t> 0 then
          s := s + ',';
        s := s + nRootSub.item[t].text;
      end;
      s := s + '}';

      nRootSub.text := s;


    end;

    if iArrayBaseIndex >=0 then begin
      nArraySub := TreeView_FindRoot(tv, iArrayBaseIndex);
      nArraySub.Text := '['+inttostr(json.icount)+']';
      SyncTreeNode(tv, nArraySub, json.icount);
      for t:= 0 to json.iCount-1 do begin
        nArraySub.item[t].text := '['+inttostr(t)+']: ';
        JSONToTreenode(json[t], tv, nArraySub.item[t]);
      end;
    end;


  end else begin
    //THIS IS A TREE BRANCH
    s := tn.text;
    if SplitString(s, ':', s1,s2) then begin
      tn.Text := s1+': ';
    end;

    tn.Text := tn.Text+vartostr(json.value);
    tn.Data := json;

//    Debug.Log(tn, 'subtext: '+tn.text);


    if iMemberIndex >=0 then begin
{$IFDEF SUB_MEMBERS}
      SyncTreeNode(tv, tn, idx);
      nSub := tn.Item[iMemberIndex];
      nsub.Text := 'Members: ';
{$ELSE}
      nMemberSub:= tn;
      SyncTreeNode(tv, nMemberSub, json.icount);
{$ENDIF}
      SyncTreeNode(tv, nMemberSub, json.ncount);
      for t:= 0 to json.nCount-1 do begin
        nMemberSub.item[t].Text := json.named.keys[t]+': ';
        JSONToTreenode(json[json.named.keys[t]], tv, nMemberSub.item[t]);
      end;
    end;
    if iArrayBaseIndex >=0 then begin
      SyncTreeNode(tv, tn, json.icount);
      tn.Text := tn.Text+'['+inttostr(json.icount)+']';
{$IFDEF SUB_ARRAYS}
      nSub := tn.Item[iArrayBaseIndex];
      nsub.Text := '['+inttostr(json.icount)+']';
      SyncTreeNode(tv, nsub, json.icount);
{$ELSE}
      nArraySub := tn;
      SyncTreeNode(tv, nArraySub, json.icount);
{$ENDIF}

      for t:= 0 to json.iCount-1 do begin
        nArraySub.item[t].text := '['+inttostr(t)+']: ';
        JSONToTreenode(json[t], tv, nArraySub.item[t]);
      end;
    end;

  end;
end;

procedure JSONtoTreeView(json: TJSON; tv: TTreeView);
var
  rn: TTreeNode;
  t: ni;
begin
  JSONtoTreeNode(json, tv, nil);




end;

procedure FlexiMathtoTreeView(fm: TFlexiMath; tv: TTreeView);
var
  t: ni;
  js: TJSON;
  n: TTreeNode;
  a: TArray<String>;
begin
  tv.Items.Clear;
  SyncTreeNode(tv, nil, fm.Count);
  a := fm.Keys.ToArray;
  for t:= 0 to Treenode_GetChildCount(tv, nil)-1 do begin
    n := TreeView_FindRoot(tv, t);
    n.Text := a[t]+' ';
    JSONToTreenode(fm.Values.ToArray[t].o, tv, n);

  end;

end;

function ListView_SelectByCaption(lv: TListView; scap: string): boolean;
var
  t: ni;
begin
  for t:= 0 to lv.Items.count-1 do begin
    if lv.Items[t].Caption = sCap then begin
      lv.Selected := lv.Items[t];
      exit(true);
    end;
  end;


  exit(false);

end;

function Control_IsShowing(c: TControl): boolean;
begin
  if c.Parent = nil then begin
    exit(c.visible);
  end else begin
    if c.Visible = false then
      exit(false)
    else
      exit(Control_IsShowing(c.parent));
  end;


end;


procedure RecsToListView(lv: TListView; firstinst: pointer; stride: ni; count: ni; TypeInfoOfRec: pointer);
var
  FContext: TRTTIContext;
  typ: TRTTIType;
  rt: TRTTIRecordType;
  fld: TRttiField;
  data: TValue;
  Value: TValue;
  t: ni;
  a: TArray<TRTTIField>;
  inst: PByte;
  cx: ni;
  idx: ni;
begin

  inst := PByte(firstinst);
  cx := count;
  idx := 0;
  typ := FContext.GetType(TypeInfoOfRec);
  rt := typ.AsRecord;
  a := rt.GetFields;

  SyncListView(lv, count, length(a));
  while cx > 0 do begin
    FContext := TRTTIContext.create;
    try

      lv.items[idx].Caption := inttostr(idx);
      for t:= 0 to high(a) do begin
        fld := a[t];
        Data := fld.GetValue(inst) ;
        lv.Items[idx].SubItems[t] := data.ToString;
        if idx = 0 then
          lv.columns[t+1].Caption := fld.Name;
      end;

    finally
      FContext.free;
    end;
    inst := inst + stride;
    dec(cx);
    inc(idx);
  end;
end;

procedure ClearChildren(aowner: TComponent; parent: TControl);
var
  t: ni;
  c: TComponent;
  ctl: TControl;
begin
  for t:= aowner.ComponentCount-1 downto 0 do begin
    c := aowner.Components[t];
    if c is TControl then begin
      ctl := c as TControl;
      ctl.Free;
      ctl := nil;
    end;
  end;

end;

procedure TreeView_ExpandAll(tv: TTreeView);
var
  t: ni;
begin
  for t:= 0 to tv.Items.Count-1 do begin
    tv.Items[t].Expand(true);
  end;
end;


function TreeNode_GetChild(tv: TTreeView; self: TTreenode; n: ni): TTreenode;
var
  t: ni;
  idx: ni;
begin
  //
  result := nil;
  if self = nil then begin
    idx := 0;
    for t:= 0 to tv.items.Count-1 do begin
      if tv.items[t].parent = nil then begin
        if (idx = n) then
          exit(tv.Items[t]);

        inc(idx);
      end;
    end;
  end else begin
    idx := 0;
    for t:= 0 to tv.items.Count-1 do begin
      if tv.items[t].parent = self then begin
        if (idx = n) then
          exit(tv.Items[t]);
        inc(idx);
      end;
    end;
  end;
end;



procedure SyncComboBox(cb: TComboBox; itemcount: ni);overload;
begin
  while cb.Items.count > itemcount do
    cb.Items.delete(cb.items.count-1);

  while cb.items.Count < itemcount do
    cb.items.Add('--');
end;
procedure SyncComboBox(cb: TComboBox; a: TArray<string>);overload;
var
  t: ni;
begin
  SyncCombobox(cb, length(a));
  for t := 0 to high(a) do
    cb.Items[t] := a[t];


end;

function IndexOfListColumn(lv: TListview; sTitle: string): ni;
var
  t: ni;
begin
  result := -1;
  for t:= 0 to lv.Columns.Count-1 do begin
    if comparetext(lv.columns[t].Caption, sTitle)=0 then
      exit(t);
  end;

end;

function GetTaskBardimensions: TRect;
var
  Data: TAppBarData;
begin
  result := Rect(0,0,0,0);
  Data.hWnd := FindWindow('Shell_TrayWnd', nil);
  Data.cbSize := SizeOf(TAppBarData);
  if SHAppBarMessage(ABM_GETTASKBARPOS, Data) = 1 then begin
    result := Data.rc;
    result.right := result.right -1;
    result.bottom := result.bottom -1;

  end;
end;

procedure RichEdit_SetBGColor(re: TRichEdit; c: cardinal);
var
  cf: TCharFormat2;
begin
  fillchar(cf, sizeof(cf), 0);
  cf.cbSize := sizeof( cf );
  cf.dwMask := CFM_BACKCOLOR;
  cf.crBackColor := c;
  re.Perform( EM_SETCHARFORMAT, SCF_SELECTION, lparam(@cf));
end;


end.
