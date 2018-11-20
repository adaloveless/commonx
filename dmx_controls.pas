unit dmx_controls;
//when you dblclick on effect, add to DMX

interface

uses
  observableobject, betterobject, controls, classes, sysutils, windows, dmx_objects, comctrls, stdctrls, extctrls, guihelpers, generics.collections.fixed;

type
  TDMXEffectList = class(TPanel)
  //lists allt effects in the library
  private
    lb: TListBox;
  private
    FDMX: TDMxUniverse;
  public
    constructor Create(AOwner: TComponent);reintroduce;virtual;
    destructor Destroy;override;
    procedure RefreshData;
  published
  end;

  TDMXGroupList = class(TPanel)
  //lists all groups registered with the dmx controller
  strict private
    lb: TListBox;
  private
    FDMX: TDMxUniverse;
  public
    constructor Create(AOwner: TComponent);reintroduce;virtual;
    destructor Destroy;override;
    procedure RefreshData;
  end;

  TDMXEffectSTack = class(TPanel)
  //lists all effects active
  strict private
    lb: TListBox;
  private
    FDMX: TDMxUniverse;
  public
    constructor Create(AOwner: TComponent);reintroduce;virtual;
    destructor Destroy;override;
    property DMX: TDMxUniverse read FDMX write FDMX;
    procedure RefreshData;
  end;

  TDMXEffectSlider = class(TPanel)
  private
    FDMX: TDMxUniverse;
  //handles changing parameters
  public
    constructor Create(AOwner: TComponent);reintroduce;virtual;
    destructor Destroy;override;
    property DMX: TDMxUniverse read FDMX write FDMX;
  end;

  TDMXEffectParameters = class(TPanel, ISimpleObserver, IUnknown)
  //handles showing parameter sliders
  private
    FDMX: TDMxUniverse;
    FList: TList<TDMXEffectSlider>;
    FEffect: TDMXEffect;
    pif: TBetterObject;
    procedure SetEffect(const Value: TDMXEffect);
  public
    constructor Create(AOwner: TComponent);reintroduce;virtual;
    destructor Destroy;override;
    property DMX: TDMxUniverse read FDMX write FDMX;
    property Effect: TDMXEffect read FEffect write SetEffect;
    property iunk: TBetterObject read pif implements IUnknown;
    procedure ObserverDetach(obj: TObservableObject);
    procedure RefreshData;
  end;

  TDMXEffectControllerX = class(TPanel)
  private
  protected
    procedure SetParent(AParent: TWinControl);override;
    procedure Resize;override;
  public
    gl: TDMXGroupList;
    el: TDMXEffectList;
    es: TDMXEffectStack;
    ep: TDMxEffectParameters;

    constructor Create(AOwner: TComponent);reintroduce;virtual;
    destructor Destroy;override;

    procedure CreateControls;
    procedure SizeControls;

    procedure RefreshData;

    procedure LInkHooks;

    procedure EffectList_OnDblClick(sender: TObject);

  end;


implementation

{ TDMXEffectList }

constructor TDMXEffectList.Create(AOwner: TComponent);
begin
  inherited CReate(AOwner);
  lb := TListBox.Create(self);
  lb.Parent := self;
  lb.Align := alClient;
end;

destructor TDMXEffectList.Destroy;
begin

  inherited;
end;

procedure TDMXEffectList.RefreshData;
var
  t: nativeint;
begin
  SyncListBox(lb,Multiverse.EffectLibrary.Count);
  for t:= 0 to Multiverse.EffectLibrary.Count-1 do begin
    lb.Items[t] := Multiverse.EffectLibrary.Classes[t].ClassName;
  end;

end;

{ TDMXGroupList }

constructor TDMXGroupList.Create(AOwner: TComponent);
begin
  inherited CReate(AOwner);
  lb := TListBox.Create(self);
  lb.Parent := self;
  lb.Align := alClient;
end;

destructor TDMXGroupList.Destroy;
begin

  inherited;
end;

procedure TDMXGroupList.RefreshData;
var
  t: nativeint;
begin
  SyncListBox(lb,Multiverse.Groups.Count);
  for t:= 0 to Multiverse.Groups.Count-1 do begin
    lb.Items[t] := Multiverse.Groups.Groups[t].Name;
  end;
end;

{ TDMXEffectSTack }

constructor TDMXEffectSTack.Create(AOwner: TComponent);
begin
  inherited CReate(AOwner);
  lb := TListBox.Create(self);
  lb.Parent := self;
  lb.Align := alClient;
end;

destructor TDMXEffectSTack.Destroy;
begin

  inherited;
end;

procedure TDMXEffectSTack.RefreshData;
var
  t: nativeint;
begin
  SyncListBox(lb,Multiverse.Effects.Count);
  for t:= 0 to Multiverse.Effects.Count-1 do begin
    lb.Items[t] := Multiverse.Effects[t].Name;
  end;
end;

{ TDMXEffectSlider }

constructor TDMXEffectSlider.Create(AOwner: TComponent);
begin
  inherited CReate(AOwner);
end;

destructor TDMXEffectSlider.Destroy;
begin

  inherited;
end;

{ TDMXEffectParameters }

constructor TDMXEffectParameters.Create(AOwner: TComponent);
begin
  inherited CReate(AOwner);
end;

destructor TDMXEffectParameters.Destroy;
begin

  inherited;
end;

procedure TDMXEffectParameters.ObserverDetach(obj: TObservableObject);
begin
  FEffect := nil;

end;

procedure TDMXEffectParameters.RefreshData;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TDMXEffectParameters.SetEffect(const Value: TDMXEffect);
begin
  FEffect := Value;
  FEffect.AddObserver(self);
end;

{ TDMXEffectControllerX }

constructor TDMXEffectControllerX.Create(AOwner: TComponent);
begin
  inherited CReate(aowner);
  CReateControls;

end;

procedure TDMXEffectControllerX.CreateControls;
begin
  gl := TDMXGroupList.Create(self);
  gl.Parent := self;

  es := TDMXEffectSTack.Create(self);
  es.Parent := self;

  el := TDMXEffectList.Create(self);
  el.Parent := self;
  el.Left := 0;
  gl.Left := gl.Left + gl.Width;

  ep:= TDMXEffectParameters.create(self);
  ep.Parent := self;
  ep.top := 0;

  es.Left := gl.Left+gl.Width;
  ep.Left := es.Left+es.Width;

  LinkHooks;
end;

destructor TDMXEffectControllerX.Destroy;
begin

  inherited;
end;

procedure TDMXEffectControllerX.EffectList_OnDblClick(sender: TObject);
var
  sNAme: string;
  i: integer;
begin
  i := el.lb.ItemIndex;
  if i < 0 then exit;

  sName := el.lb.Items[i];

  Multiverse.Effects.Add(sName,0);
  REfreshData;

end;

procedure TDMXEffectControllerX.LInkHooks;
begin
  el.lb.OnDblClick := self.EffectList_OnDblClick;

end;

procedure TDMXEffectControllerX.RefreshData;
begin
  gl.RefreshData;
  es.refreshdata;
  el.RefreshData;
  gl.RefreshData;



end;

procedure TDMXEffectControllerX.Resize;
begin
  inherited;
  Sizecontrols;
end;



procedure TDMXEffectControllerX.SetParent(AParent: TWinControl);
begin
  inherited;
  Sizecontrols;
end;


procedure TDMXEffectControllerX.SizeControls;
begin
  el.Left := 0;//effect list
  es.Top := 0; //effect stack
  gl.Top := 0; //group list
  ep.top := 0; //effect parameters

  es.Left := el.Left + el.Width;
  gl.Left := es.Left+es.Width;
  ep.Left := gl.Left+gl.Width;

  gl.Height := clientheight;
  es.Height := clientheight;
  el.Height := clientheight;
  ep.height := clientheight;

end;

end.
