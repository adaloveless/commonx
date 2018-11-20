unit dmx_effect_controller;

interface

uses
  tickcount, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dmx_controls, Vcl.ExtCtrls, validation, debug,
  Vcl.StdCtrls, guihelpers, dmx_objects, comctrls, generics.collections.fixed, typex, BCF2000, systemx;

type
  TSliderChangeEvent = procedure (iChannel: nativeint; iValue: nativefloat) of object;

  TDMXEffectController = class;//forward

  TScriptParamDirtyEvent = procedure(iLayerNumber: nativeint; const sClassName: string; const sParamName: string) of object;


  IDMXSCriptHooker = interface
    procedure AddToScript(sCmd: string);
  end;


  TDMXSliderControl = class(TPanel)
  strict private
    lbl: TLabel;
    tb: TTrackBar;
    ed: TEdit;
  private
    FParamName: string;
    FOnCHange: TSliderChangeEvent;
    FParamDefinition: TDMXEffectParameterDefinition;
    FChannel: nativeint;
    procedure SetParamName(const Value: string);
    procedure SetParamDefinition(const Value: TDMXEffectParameterDefinition);
    function GetFloatposition: nativefloat;
    procedure SetFloatPosition(const Value: nativefloat);
    function GetFloatSlider: nativefloat;
    procedure SEtFloatSlider(const Value: nativefloat);
  public
    Controller: TDMXEffectController;
    procedure RefreshControl;

    constructor Create(aowner: TComponent);override;
    property ParamDefinition: TDMXEffectParameterDefinition read FParamDefinition write SetParamDefinition;
    property OnChange: TSliderChangeEvent read FOnCHange write FOnChange;
    procedure Slider_OnChange(sender: TObject);
    property Channel: nativeint read FChannel write FChannel;
    property FloatPosition: nativefloat read GetFloatposition write SetFloatPosition;
    property FloatSlider: nativefloat read GetFloatSlider write SEtFloatSlider;
    procedure Edit_OnChange(sender: TObject);

    procedure Edit_OnKeyDown(sender: TObject; var Key: Word; Shift: TShiftState);

  end;

  TDMXEffectController = class(TFrame)
    lbEffectList: TListBox;
    lbEffectStack: TListBox;
    lbGroupList: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    tmControllerCheck: TTimer;
    procedure lbEffectListDblClick(Sender: TObject);
    procedure lbEffectStackClick(Sender: TObject);
    procedure lbGroupListClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure tmControllerCheckTimer(Sender: TObject);

  strict private
    FSliders: TList<TDMXSliderControl>;
    FCurrentEffect: TDMXEffect;

    FBCF: TBCF2000;
    FHooker: IDMXSCriptHooker;
    needs_refresh: boolean;
    procedure SetCurrentEffect(const Value: TDMXEffect);
    { Private declarations }
    procedure RemoveGroupsHL;
    procedure AddGroupsHL;
    procedure AddGroupHL(const sName: string);
    procedure RemoveGroupHL(const sName: string);
    procedure MSG_Refresh_DAta(var msg: TMEssage);
    procedure RefreshData;
  private
    RefreshingData: nativeint;
  public
    { Public declarations }
    BCFDataSource: TBCF2000DataSource;
    constructor Create(aowner: TComponent);override;
    destructor Destroy;override;
    procedure PostRefreshData;
    procedure RefreshDataSynchronous;

    procedure SyncSliders;
    procedure SEtCurrentEffectLayer(iLayer: nativeint; sClass: string);
    property CurrentEffect: TDMXEffect read FCurrentEffect write SetCurrentEffect;
    function GetParamDefintion(sName: string): TDMXEffectParameterDefinition;

    procedure SyncGroupsToEffect;
    procedure SyncEffectToGroups;

    procedure Slider_OnChange(iChannel: nativeint; iValue: nativefloat);
    property BCF: TBCF2000 read FBCF write FBCF;
    procedure BCfButton(bcf: TBCF2000; ds: TBCF2000DAtaSource; iButton: nativeint; bDown: boolean; bChanged: boolean; var handled: boolean);
    procedure BCFSlider(bcf: TBCF2000; ds: TBCF2000DAtaSource;iChannel: nativeint; rLevel: nativefloat; var handled: boolean );
    procedure BCFRotor(bcf: TBCF2000; ds: TBCF2000DAtaSource; iChannel, iTicks: nativeint; var handled: boolean);

    procedure DoEnter;override;
    property Hooker: IDMXSCriptHooker read FHooker write FHooker;

    procedure ParamDirty(iLayer: nativeint; const sEffectClass: string; const sParamName: string);
  end;

implementation

{$R *.dfm}

{ TDMXEffectController }

procedure TDMXEffectController.SetCurrentEffect(const Value: TDMXEffect);
begin
  FCurrentEffect := Value;
  PostReFreshData;
end;

procedure TDMXEffectController.SEtCurrentEffectLayer(iLayer: nativeint;
  sClass: string);
begin
  if iLayer > Multiverse.Effects.Count-1 then
    exit;

  Inc(RefreshingData);
  try

    self.lbEffectList.ItemIndex := iLayer;

    if Multiverse.Effects[iLayer].Name = sClass then
      CurrentEffect := Multiverse.Effects[iLayer]
    else
      CurrentEffect := nil;


    //RefreshData;  SetCurrentEffect triggers this
  finally
    dec(RefreshingData);
  end;

end;



procedure TDMXEffectController.tmControllerCheckTimer(Sender: TObject);
begin
  if needs_refresh then begin
    RefreshData;
    needs_Refresh := false;
  end;
end;

procedure TDMXEffectController.Slider_OnChange(iChannel: nativeint; iValue: nativefloat);
var
  r: nativefloat;
  pd: TDMXEffectParameterDefinition;
begin
  if currenteffect = nil then
    exit;

  if RefreshingDAta>0 then exit;

  pd := FSliders[iChannel].ParamDefinition;


  currenteffect.SetParam(pd.Name, iVAlue);
  BCFDataSource.SliderOut(iChannel, FSliders[iChannel].FloatSlider, true);


end;

procedure TDMXEffectController.SyncEffectToGroups;
var
  t: integer;
begin
  if CurrentEffect = nil then begin
    lbGroupList.Enabled := false;
    exit;
  end;


  lbGroupList.Enabled := true;

  for t:= 0 to lbGroupList.Items.Count-1 do begin
    lbGroupList.Selected[t] := CurrentEffect.HasGroup(lbGrouplist.Items[t]);
  end;



end;

procedure TDMXEffectController.SyncGroupsToEffect;
var
  t: integer;
  tmStart: cardinal;
begin
  if CurrentEffect = nil then
    exit;


  tmStart := GetTicker;
  RemoveGroupsHL;
  AddGroupsHL;
  Debug.Log('SyncGroupsToEffect took '+inttostr(getTimeSInce(tmStart))+'ms.');
//  CurrentEffect.ClearLights;
//  for t:= 0 to lbGroupList.Items.Count-1 do begin
//    if lbGroupList.Selected[t] then
//       CurrentEffect.AddGroup(lbGrouplist.Items[t]);
//
//  end;


end;

procedure TDMXEffectController.SyncSliders;
var
  iCount: nativeint;
  sl: TDMXSliderControl;
  t: nativeint;
  iFreq: nativeint;
begin


  if CurrentEffect = nil then
    iCount := 0
  else
    iCount := CurrentEffect.ParamCount;

  while FSliders.Count > icount do begin
    FSliders[FSliders.Count-1].Free;
    FSliders.Delete(FSliders.Count-1);
  end;

  while FSliders.Count < iCount do begin
    sl := TDMXSliderControl.Create(self);
    sl.Parent := self;
    sl.Height := 32;
    sl.Top := FSliders.Count * sl.Height;
    sl.Left := lbGroupList.Left + lbGroupList.width;
    sl.Width := clientwidth - sl.left;
    FSliders.add(sl);
  end;

  if CurrentEffect <> nil then begin
    for t:= 0 to FSliders.Count-1 do begin

      FSliders[t].Controller := self;
      FSliders[t].Channel := t;
      FSliders[t].ParamDefinition := CurrentEffect.ParamDefinitions[t];
      FSliders[t].OnChange := self.Slider_OnChange;


    end;
    for t := FSliders.Count to 7 do begin
      BCFDataSource.SliderOut(t, 0, true);
    end;
  end;
end;

procedure TDMXEffectController.AddGroupHL(const sName: string);
begin
  if assigned(Hooker) then begin
    Hooker.AddToScript('fx_add_group,'+sName);
  end else begin
    if assigned(CurrentEffect) then
      CurrentEffect.AddGroup(sNAme);
  end;
end;

procedure TDMXEffectController.AddGroupsHL;
var
  t: nativeint;
begin
  if currenteffect = nil then exit;
  //high level add/remove groups
  for t:= 0 to lbGroupList.Count-1 do begin
    if (lbGroupList.Selected[t]) and (not currenteffect.HAsGroup(lbGroupList.Items[t])) then begin
      AddGroupHL(lbGroupList.Items[t]);
    end;
  end;

end;

procedure TDMXEffectController.BCfButton(bcf: TBCF2000; ds: TBCF2000DAtaSource;
  iButton: nativeint; bDown: boolean; bChanged: boolean; var handled: boolean);
begin
//
end;

procedure TDMXEffectController.BCFRotor(bcf: TBCF2000; ds: TBCF2000DAtaSource;
  iChannel, iTicks: nativeint; var handled: boolean);
begin
//
end;

procedure TDMXEffectController.BCFSlider(bcf: TBCF2000; ds: TBCF2000DAtaSource;
  iChannel: nativeint; rLevel: nativefloat; var handled: boolean);
begin
//
  if iChannel < FSliders.count then
    FSliders[iChannel].FloatSlider := rLevel;

end;

procedure TDMXEffectController.Button1Click(Sender: TObject);
begin
  REFreshData;
end;

constructor TDMXEffectController.Create(aowner: TComponent);
begin
  inherited;
  FSliders := TList<TDMXSliderControl>.create;
  BCFDAtaSource := TBCF2000DAtaSource.create;
  BCFDataSource.OnSliderIn := self.BCFSlider;
  BCFDataSource.OnRotorIn := self.BCFRotor;
  BCFDataSource.OnButtonCodeIn := self.BCfButton;
end;

destructor TDMXEffectController.Destroy;
begin
  BCFDAtaSource.free;
  CurrentEffect := nil;//should delete all sliders
  FSliders.Free;
  FSliders := nil;

  inherited;
end;

procedure TDMXEffectController.DoEnter;
begin
  inherited;
  if assigned(BCF) then
    BCF.DataSource := self.BCFDataSource;


end;


function TDMXEffectController.GetParamDefintion(
  sName: string): TDMXEffectParameterDefinition;
var
  t: integer;
begin
  result := nil;

end;

procedure TDMXEffectController.lbEffectListDblClick(Sender: TObject);
begin
  if lbEffectList.itemindex < 0 then
    exit;

  if assigned(Hooker) then begin
    hooker.AddToScript('fx_create,'+lbEffectList.Items[lbEffectList.ItemIndex]);
  end else begin
    Multiverse.effects.add(lbEffectList.Items[lbEffectList.ItemIndex], 0);
    RefreshData;
  end;


end;

procedure TDMXEffectController.lbEffectStackClick(Sender: TObject);
begin
  if RefreshingData > 0 then exit;

  if assigned(hooker) then begin
    if (lbEffectStack.ItemIndex >= 0) and (lbEffectStack.itemindex <Multiverse.Effects.count) then begin
      hooker.AddToScript('fx_set_layer,'+inttostr(lbEffectStack.ItemIndex)+','+Multiverse.Effects[lbEffectStack.ItemIndex].Name);
    end;
  end else begin
    if lbEffectStack.ItemIndex < 0 then
      CurrentEffect := nil
    else
      CurrentEffect := Multiverse.Effects[lbEffectStack.ItemIndex];
  end;
end;

procedure TDMXEffectController.lbGroupListClick(Sender: TObject);
begin
  if RefreshingData > 0  then exit;

  SyncGroupsToEffect;

end;

procedure TDMXEffectController.MSG_Refresh_DAta(var msg: TMEssage);
begin
  needs_refresh := true;
end;

procedure TDMXEffectController.ParamDirty(iLayer: nativeint; const sEffectClass,
  sParamName: string);
begin
  inc(RefreshingDAta);
  try
    if CurrentEffect = nil then
      exit;

    if iLayer <> Self.lbEffectStack.ItemIndex then exit;
    if lowercase(CurrentEffect.Name) <> lowercase(sEffectClass) then exit;

    SyncSliders;
  finally
    dec(RefreshingDAta);
  end;





end;

procedure TDMXEffectController.PostRefreshData;
begin
  needs_refresh := true;
end;

procedure TDMXEffectController.RefreshDataSynchronous;
var
  t: nativeint;
begin
  Debug.ConsoleLog('REFRESH!');
  inc(RefreshingDAta);
  try
    if assigned(BCF) then
      BCF.DataSource := self.BCFDataSource;

    SyncListBox(lbEffectList,Multiverse.EffectLibrary.Count);
    for t:= 0 to Multiverse.EffectLibrary.Count-1 do begin
      lbEffectList.Items[t] := Multiverse.EffectLibrary.Classes[t].ClassName;
    end;

    SyncListBox(lbGroupList,Multiverse.Groups.Count);
    for t:= 0 to Multiverse.Groups.Count-1 do begin
      lbGroupList.Items[t] := Multiverse.Groups.Groups[t].Name;
    end;

    SyncListBox(lbEffectStack,Multiverse.Effects.Count);
    for t:= 0 to Multiverse.Effects.Count-1 do begin
      lbEffectStack.Items[t] := Multiverse.Effects[t].Name;
    end;

    SyncSliders;
    SyncEffectToGroups;
  finally
    dec(RefreshingDAta);
  end;


end;

procedure TDMXEffectController.RefreshData;
begin
  RefreshDataSYnchronous;

end;

procedure TDMXEffectController.RemoveGroupHL(const sName: string);
begin
  if assigned(Hooker) then begin
    Hooker.AddToScript('fx_remove_group,'+sName);
  end else begin
    if assigned(CurrentEffect) then
      CurrentEffect.RemoveGroup(sNAme);
  end;

end;

procedure TDMXEffectController.RemoveGroupsHL;
var
  t: nativeint;
begin
  if currenteffect = nil then exit;
  //high level add/remove groups
  for t:= 0 to lbGroupList.Count-1 do begin
    if (not lbGroupList.Selected[t]) and (currenteffect.HAsGroup(lbGroupList.Items[t])) then begin
      RemoveGroupHL(lbGroupList.Items[t]);
    end;
  end;
end;

{ TDMXSliderControl }

constructor TDMXSliderControl.CReate(aowner: TComponent);
begin
  inherited;
  tb := TTrackBar.Create(self);
  lbl := TLabel.Create(self);
  lbl.Parent := self;
  lbl.Align := alTop;
  tb.parent := self;
  tb.Align := alClient;
  tb.OnChange := self.Slider_OnChange;
  ed := TEdit.Create(self);
  ed.parent := self;
  ed.Align := alClient;
  ed.Visible := false;
  ed.OnChange := self.Edit_OnChange;
  ed.OnKeyDown := self.Edit_OnKeyDown;

end;

procedure TDMXSliderControl.Edit_OnChange(sender: TObject);
begin

  if Controller.RefreshingData >0 then
    exit;

  if ParamDefinition = nil then exit;


  if ParamDefinition.entryType = eetHex then begin
    ed.Text := restricttohex(ed.text);
    //DO NOTHING! IMPLEMENTED ON KEYDOWN


  end;


end;

procedure TDMXSliderControl.Edit_OnKeyDown(sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if key = VK_RETURN then begin
    if Assigned(Controller.Hooker) then begin
      PAramDefinition.sValue := ed.Text;
      if controller.refreshingdata = 0 then
        Controller.Hooker.AddToScript('fx_param,'+ParamDefinition.Name+','+ParamDefinition.sValue);

    end else begin
      ParamDefinition.sValue := ed.Text;
    end;
    key := 0;
  end;

end;

function TDMXSliderControl.GetFloatposition: nativefloat;
begin
  result := ((tb.position / tb.max) * (ParamDefinition.Max-ParamDefinition.Min)) + ParamDefinition.Min;
end;

function TDMXSliderControl.GetFloatSlider: nativefloat;
begin
  if tb.max <> 0 then
    result := tb.Position / tb.Max
  else
    result := 0;

end;

procedure TDMXSliderControl.RefreshControl;
var
  iFreq: nativeint;
  r: nativefloat;
begin
  if paramdefinition = nil then exit;

  caption := '';
  if ParamDefinition.entryType = eetSlider then begin
    ed.Visible := false;
    tb.Visible := true;

    tb.Min := 0;
    tb.MAx := ParamDefinition.Steps - 1;
    controller.CurrentEffect.GetParam(paramdefinition.name, r);
    Floatposition := r;
    lbl.Caption := ParamDefinition.name+' '+floattostr(r);


    iFreq := round(ParamDefinition.MAx - ParamDefinition.Min);
    if iFreq = 0 then iFreq := 1;
    if iFreq < ParamDefinition.Steps then
      iFreq := ParamDefinition.Steps div iFreq;
    tb.Frequency := iFReq;

  end else begin
    tb.Visible := false;
    ed.Visible := true;
    ed.Align := alClient;
    lbl.Caption := ParamDefinition.name;
    ed.Text := ParamDefinition.sValue;
  end;

end;


procedure TDMXSliderControl.SetFloatPosition(const Value: nativefloat);
var
  i: nativeint;
begin
  if self.FParamDefinition.imptyp = eptFloat then begin

    i := round(tb.Max * ((value-ParamDefinition.Min) / (ParamDefinition.Max-ParamDefinition.Min)));
    if i < 0 then i := 0;
    if i > tb.Max then
      i := tb.Max;

    tb.Position := i;
  end;
  //result := ((tb.position / tm.max) * (pd.Max-pd.Min)) + pd.Min;
end;

procedure TDMXSliderControl.SEtFloatSlider(const Value: nativefloat);
begin
  tb.Position := round(value * tb.Max);
end;

procedure TDMXSliderControl.SetParamDefinition(
  const Value: TDMXEffectParameterDefinition);
begin
  FParamDefinition := Value;
  REfreshControl;
  Controller.BCFDataSource.SliderOut(Channel, FloatSlider, true);
end;

procedure TDMXSliderControl.SetParamName(const Value: string);
begin
  FParamName := Value;
  lbl.Caption := value;
end;

procedure TDMXSliderControl.Slider_OnChange(sender: TObject);
begin
  if ParamDefinition = nil then exit;

  if assigned(FOnCHange) then
    FOnChange(Channel, FloatPosition);

  if Assigned(Controller.Hooker) then begin
    if Controller.RefreshingData = 0 then
      Controller.Hooker.AddToScript('fx_param,'+ParamDefinition.Name+','+floattostr(ParamDefinition.rValue));
  end else begin
//  ParamDefinition.rValue := FloatPosition;
  end;
end;

end.
