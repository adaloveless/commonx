unit guihelpers_fmx;

interface


uses
  FMX.StdCtrls, FMX.ListView, FMX.ListView.Appearances, FMX.ListView.Types, typex, fmx.objects, fmx.controls, FMX.VirtualKeyboard, FMX.Platform, FMX.Types, classes, fmx.forms, debug, FMXTee.Chart, FMXTee.Series, FMX.ListBox, fmx.layouts, types, stringx;

type
  TChildIterator = reference to procedure (c: TFMXobject; var stop: boolean);

function listview_GetSelectedAppearance(lv: TListView): TlistViewItem;

procedure ToggleVirtualKeyboard(bShow: boolean; sender: TFMXObject);
procedure InFormAlert(form: TForm; sMessage: string);
function Chart_ExtractYValues(cChart: TChart; series: ni): TArray<variant>;overload;
function Chart_ExtractYValuesA(cChart: TChart): TArray<TArray<variant>>;overload;
function Chart_ExtractXValues(cChart: TChart; series: ni): TArray<variant>;overload;
function Chart_ExtractXValuesA(cChart: TChart): TArray<TArray<variant>>;overload;
procedure Chart_ClearData(cChart: TChart);
procedure Chart_AddValues(c: Tchart; yvalues, xvalues: TArray<Tarray<variant>>);
procedure ListViewUpdateEmptyMessage(lv: TListView; sMessage: string = '');
procedure ListBoxUpdateEmptyMessage(lb: TListBox; sMessageIfEmpty: string);
procedure ControlUpdateEmptyMessage(c: TControl; sMessage: string = '');
procedure CenterControl(c: TControl);
procedure Control_DestroyChildren(c: TFMXobject);
function Control_GetWidth(c: TFMXObject): single;
function Control_GetHeight(c: TFMXObject): single;
function Control_GetPosition(c: TFMXObject): TPointF;

function Control_IterateChildren(c: TFMXObject; p: TChildIterator): boolean;

type
  TGuiHelper = class
    class function control_GetControl<TC: TControl>(parent: fmx.controls.TControl; bRecurse: boolean = true): TC;overload;
    class function control_GetControl<TC: TControl>(parent: TForm; bRecurse: boolean = true): TC;overload;
    class function control_GetParentOfType<TC: TFMXObject>(startingcontrol: TFMXObject): TC;
    class procedure DestroySubControls(parent: fmx.controls.TControl);
    class procedure DestroySubComponents(owner: TComponent; cc: TComponentClass);
  end;

implementation

procedure Chart_ClearData(cChart: TChart);
var
  t: ni;
begin
  for t:= 0 to cChart.SeriesList.count-1 do
    cChart.series[t].Clear;


end;
function listview_GetSelectedAppearance(lv: TListView): TlistViewItem;
begin
  result := lv.selected as TListViewItem;

end;

{ TGuiHelper }

class function TGuiHelper.control_GetControl<TC>(parent: fmx.controls.TControl; bRecurse: boolean = true): TC;
var
  t: ni;
  c: TControl;
begin
  result := nil;
  for t := 0 to parent.controls.Count-1 do begin
    c := parent.controls[t];
    if c is TC then
      exit(c as TC);
  end;

  if bRecurse then begin
    for t := 0 to parent.controls.Count-1 do begin
      c := parent.controls[t];
      result := control_GetControl<TC>(c);
      if result <> nil then
        exit;
    end;
  end;

end;

procedure ToggleVirtualKeyboard(bShow: boolean; sender: TFMXObject);
var
  FService: IFMXVirtualKeyboardService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FService));
  if (FService <> nil) then
    begin
      if bShow then
        fService.ShowVirtualKeyboard(sender)
      else
        FService.HideVirtualKeyboard;
    end;
end;

class function TGuiHelper.control_GetControl<TC>(parent: TForm;
  bRecurse: boolean): TC;
var
  t: ni;
  c: TFMXObject;
begin
  result := nil;
  for t := 0 to parent.Children.Count-1 do begin
    c := parent.children[t];
    if not (c is TControl) then
      continue;

    var cc := c as TControl;

    if cc is TC then
      exit(cc as TC);
  end;

  if bRecurse then begin
    for t := 0 to parent.Children.Count-1 do begin
      c := parent.children[t];
      if not (c is TControl) then
        continue;

      var cc := c as TControl;

      result := control_GetControl<TC>(cc);
      if result <> nil then
        exit;
    end;
  end;

end;

class function TGuiHelper.control_GetParentOfType<TC>(
  startingcontrol: TFMXObject): TC;
begin
  if startingcontrol = nil then
    exit(nil);

  if startingcontrol is TC then
    exit(TC(startingcontrol))
  else
    exit(control_GetParentOfType<TC>(startingcontrol.parent));


end;

class procedure TGuiHelper.DestroySubComponents(owner: TComponent; cc: TComponentClass);
var
  c: TComponent;
  t: ni;
begin
  t := 0;
  while t < owner.componentcount do begin
    c := owner.components[t];
    if c is cc then begin
      owner.RemoveComponent(c);
      if c is TControl then
        TControl(c).parent := nil;
      c.free;
    end else begin
      inc(t);
    end;
    c := nil;
  end;

end;

class procedure TGuiHelper.DestroySubControls(parent: fmx.controls.TControl);
var
  c: TControl;
begin
  Debug.Log('Destroying sub controls of '+parent.name);
  while parent.Controls.count > 0 do begin
    c := parent.controls[0];
    Debug.Log(' -'+c.classname);
    parent.Controls.delete(0);
    c.free;
    c := nil;
  end;


end;

procedure InFormAlert(form: TForm; sMessage: string);
var
  panBG, pan: TPanel;
  lbl : TLabel;
begin
  panBG := TPanel.Create(form);
  panBG.Parent := form;
  panBG.Align := TAlignLayout.Client;
  pan := TPanel.create(form);
  pan.Height := form.clientheight div 2;
  pan.Position.Y := form.clientheight div 3;
  pan.position.x := 0;
  pan.Width := form.clientwidth;

  lbl := TLabel.create(form);
  lbl.parent := pan;
  lbl.Text := sMessage;
  lbl.Align := TAlignLayout.Client;
  lbl.VertTextAlign := TTextAlign.Center;






end;

function Chart_ExtractYValuesA(cChart: TChart): TArray<TArray<variant>>;
var
  t: ni;
begin
  setlength(result, cChart.serieslist.count);
  for t:= 0 to high(result) do
    result[t] := Chart_ExtractYValues(cChart, t);

end;

function Chart_ExtractYValues(cChart: TChart; series: ni): TArray<variant>;
var
  t: ni;
begin
  setlength(result, cChart.Series[series].YValues.Count);
  for t:= 0 to high(result) do begin
    result[t] := cChart.series[series].YValues[t];
  end;

end;




function Chart_ExtractXValuesA(cChart: TChart): TArray<TArray<variant>>;
var
  t: ni;
begin
  setlength(result, cChart.serieslist.count);
  for t:= 0 to high(result) do
    result[t] := Chart_ExtractXValues(cChart, t);

end;

function Chart_ExtractXValues(cChart: TChart; series: ni): TArray<variant>;
var
  t: ni;
begin
  setlength(result, cChart.Series[series].YValues.Count);
  for t:= 0 to high(result) do begin
    result[t] := cChart.series[series].XValues[t];
  end;

end;

procedure Chart_AddValues(c: Tchart; yvalues, xvalues: TArray<Tarray<variant>>);
var
  s, t: ni;
begin
  for s := 0 to c.SeriesList.count-1 do begin
    for t:= 0 to high(yvalues[s]) do
      c.Series[s].Add(yvalues[s][t], xvalues[s][t]);
  end;

end;

procedure ListBoxUpdateEmptyMessage(lb: TListBox; sMessageIfEmpty: string);
//Doing arbitrary children of a listbox is not advised, therefore
//this function searches the PARENT of the listbox for a label to update.
//If the listbox is empty, it displays the label.  Else it hides the label.
//To use this, you simply need to have a TLabel on your form at the same
//level in the hierarchy as the TListBox.
{$DEFINE GOOD_CODE}
{$IFDEF GOOD_CODE}
var
  child: TLabel;
begin
{x$DEFINE FMX_BUG}
{$IFDEF FMX_BUG}
  repeat
    child := TGuiHelper.control_GetControl<TLabel>(lb, false);
    if child <> nil then begin
      child.Parent := nil;
      child.owner.RemoveComponent(child);
      //child.Free;
      child := nil;
    end;
  until child = nil;
{$ELSE}
  child := TGuiHelper.control_GetControl<TLabel>(lb.parent as TControl, false);
  if child <> nil then begin
    if lb.Items.Count = 0 then begin
      child.visible := true;
      if sMessageIfEmpty <> '' then
        child.Text := sMessageIfEmpty;
    end else begin
      child.visible := false;
    end;
  end else begin
    raise ECritical.create('no TLabel found as child of '+lb.Name);
  end;
{$ENDIF}
end;
{$ELSE}
var
  child: TLabel;
begin
  if lb.Items.Count = 0 then begin
    child := TGuiHelper.control_GetControl<TLabel>(lb, false);
    if child = nil then begin
      child := TLabel.create(lb);
      child.parent := lb;
    end;

    child.Visible := true;
    child.Align := TAlignLayout.Client;
    child.TextAlign := TTextAlign.Center;
    child.StyleLookup := 'label';

    child.text := sMessageIfEmpty;
  end else begin
    repeat
      child := TGuiHelper.control_GetControl<TLabel>(lb, false);
      if child <> nil then begin
        child.Parent := nil;
        child.owner.RemoveComponent(child);
        //child.Free;
        child := nil;
      end;
    until child = nil;
  end;
end;
{$ENDIF}

procedure ControlUpdateEmptyMessage(c: TControl; sMessage: string = '');
var
  child: TLabel;
begin
  child := TGuiHelper.control_GetControl<TLabel>(c, false);
  if child <> nil then begin

    //IF THERE is only one child (the label)
    if c.ChildrenCount = 1 then begin
      //then this is empty
      child.visible := true;
      if sMessage <> '' then
        child.Text := sMessage;
    end else begin
      child.visible := false;
    end;
  end else begin
    raise ECritical.create('no TLabel found as child of '+c.Name);
  end;
end;


procedure ListViewUpdateEmptyMessage(lv: TListView; sMessage: string = '');
var
  child: TLabel;
begin
  child := TGuiHelper.control_GetControl<TLabel>(lv, false);
  if child <> nil then begin
    if lv.Items.Count = 0 then begin
      child.visible := true;
      if sMessage <> '' then
        child.Text := sMessage;
    end else begin
      child.visible := false;
    end;
  end else begin
    raise ECritical.create('no TLabel found as child of '+lv.Name);
  end;
end;

procedure CenterControl(c: TControl);
var
  w,h,pw,ph: single;
begin
  if c = nil then
    exit;
  if c.parent = nil then
    exit;
  w := c.width;
  h := c.height;
  if c.parent is Tcontrol then begin
    pw := (c.parent as Tcontrol).width;
    ph := (c.parent as TControl).height;
  end else
  if c.parent is TForm then begin
    pw := (c.parent as TForm).width;
    ph := (c.parent as TForm).height;
  end else
    exit;

  c.Position.x := (pw / 2)-(w / 2);
  c.position.y := (ph / 2) - (h / 2);

end;

procedure Control_DestroyChildren(c: TFMXobject);
var
  cc: TFMXobject;
begin
  while c.ChildrenCount > 0 do begin
    cc := c.Children[c.ChildrenCount-1];
    cc.parent := nil;
{$IFDEF MSWINDOWS}
    cc.free;
{$ELSE}
    cc.DisposeOf;
{$ENDIF}
    cc := nil;

  end;

end;
function Control_GetPosition(c: TFMXObject): TPointF;
begin
  result.x := 0.0;
  result.y := 0.0;
  if c = nil then
    exit;
  if c is TControl then
    exit(TControl(c).Position.Point);

  if c is TForm then begin
    result.x := TForm(c).Left;
    result.y := TForm(c).Top;
  end;

end;

function Control_GetWidth(c: TFMXObject): single;
begin
  if c = nil then
    exit(0);
  if c is TControl then
    exit(TControl(c).width);

  if c is TForm then
    exit(TForm(c).Width);

  exit(0);
end;

function Control_GetHeight(c: TFMXObject): single;
begin
  if c = nil then
    exit(0);
  if c is TControl then
    exit(TControl(c).Height);

  if c is TForm then
    exit(TForm(c).Height);

  exit(0);
end;

function Control_IterateChildren(c: TFMXObject; p: TChildIterator): boolean;
var
  stop: boolean;
begin
  result := true;
  stop := false;
  for var t:= 0 to c.childrencount-1 do begin
    var cc := c.children[t];
    p(cc, stop);
    if stop then
      exit(false);
    Control_IterateChildren(cc,p);

  end;
end;






end.

