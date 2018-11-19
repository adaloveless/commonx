unit z3_config_controls;
//todo 2: implement description in scheme in embedded system
//todo 1: implement setting of values
//todo 2: implement ROM table in embedded system


interface

uses
  windows, messages, graphics, sysutils, comctrls, extctrls, systemx, typex, forms, better_JSON, debug, classes, generics.collections.fixed, controls, stdctrls, z3_sysex_config, stringx, tickcount, helpers.JSON, guihelpers, zbass_data;

const
  SPACING = 22;
  CM_THREADED_UPDATE = WM_USER+0;


type
  TNotifyCCCodeChanged = procedure (CCCode: string; rec: TSysexConfigRecord; newval: int64; bUserchange: boolean) of object;
  TNotifyCCCodeRefresh = procedure (CCCode: string) of object;
  EJSONSucks = class(Exception);


  TZ3ConfigItem = class(TPanel)
  private
    FRemoteupdate: boolean;
    FFlags: ni;
    function GEtCatCode: string;
    function GEtCCCode: string;
    procedure SetCatCode(const Value: string);
    procedure SetCCCode(const Value: string);
    function GetIntValue: int64;
    procedure SetIntValue(const Value: int64);
    function GetRec: TSysexConfigRecord;
    procedure SetRec(const Value: TSysexConfigRecord);
    procedure SetTreatmentType(tt: TConfigValueTreatmentType);
    function GEtDesc: string;
    procedure SetDesc(const Value: string);
    procedure btnRefresh_Onclick(sender: TObject);
    procedure btnHide_Onclick(sender: TObject);
    procedure SetFlags(const Value: ni);
  protected
    fFilter: string;
    Ftt: TConfigValueTreatmentType;
    procedure SetParent(AParent: TWinControl); override;
    procedure Resize;override;
    procedure UpdateVisibility;
  public
    retries: ni;
    threadsafe_value: int64;
    Frec: TSysexConfigRecord;
    lblCCCode: TLabel;
    lblCatCode: TLAbel;
    lblReadOnly: TLAbel;
    lblDesc: TLabel;
    lblFlags: TLabel;
    ed: Tedit;
    cb: TCheckBox;
    btnCommit: TButton;
    btnRefresh: TButton;
    btnHide: TButton;
    FVAlue: int64;
    refreshRequestTime: ticker;
    steadyhandle: THandle;
    random: ni;
    procedure ed_onenter(sender: TObject);
    constructor Create(aowner: TComponent); override;
    destructor destroy;override;
    property CCCode: string read GEtCCCode write SetCCCode;
    property CatCode: string read GEtCatCode write SetCatCode;
    property Desc: string read GEtDesc write SetDesc;
    property IntValue: int64 read GetIntValue write SetIntValue;
    property RemoteUpdate: boolean read FRemoteupdate write FRemoteUpdate;
    procedure CommitValue;
    procedure RefreshData;
    property rec: TSysexConfigRecord read GetRec write SetRec;
    procedure btnCommmit_Onclick(sender: TObject);
    procedure PositionStuff;
    property Flags: ni read FFlags write SetFlags;
    procedure CheckRetries;
    function RefreshPending: boolean;
    procedure PushPArentRefresh;
  published
    function MatchesFilter(s: string): boolean;
    procedure MSg_ThreadedUpdate(var msg: TMessage); message CM_THREADED_UPDATE;

    procedure DefineFromIndividualSchema(s: string);
  end;

  TZ3Config = class(TScrollBox)
  private
    FOnChanged: TNotifyCCCodeChanged;
    FOnRefresh: TNotifyCCCodeRefresh;
    FDisableConfigValueFetch: boolean;
    function GetItems(idx: ni): TZ3configItem;
    function GetCount: ni;
  protected
    csConfigControl: TCLXCriticalSection;
    fFilter: string;
    procedure Resize;override;
    function GetFilter: string;
    procedure SetFilter(const Value: string);

  public
    FList: TList<TZ3ConfigItem>;
    constructor Create(aowner: TComponent); override;
    destructor destroy;override;

    procedure SyncJSON(json: TJsonObject);
    procedure SyncItemCount(iCount: ni);
    procedure Clear;
    property Items[idx: ni]: TZ3configItem read GetItems;
    procedure UpdateValueFromRemote(cccode: string; val: int64);
    procedure NotifyCCcodeChanged(cccode: string; rec: TSysexConfigRecord; intvalue: int64; bUserChange: boolean);
    procedure NotifyRefreshRequested(cccode: string);
    property OnCCCodeChanged: TNotifyCCCodeChanged read FOnChanged write FOnChanged;
    property OnRefreshRequested: TNotifyCCCodeRefresh read FOnRefresh write FOnRefresh;
    function FindConfigRec(ccCode: string): TSysexConfigRecord;
    procedure positionChildren;
    procedure UpdateFilter;
    procedure CheckRetries;
    property Count: ni read GetCount;
    function AnyPendingRefreshes: boolean;
  published
    property Filter: string read GetFilter write SetFilter;
    property DisableConfigValueFetch: boolean read FDisableConfigValueFetch write FDisableConfigValueFetch;
    procedure SyncFromGuitarData(g: TZBassData);
  end;

var
  lastGLobalRetryTime: ticker;
  GlobalRetryThrottle: ticker;
  RefreshBlockSize: int64;
  RefreshBlockAllowance: int64;


implementation

{ TZ3Config }

function TZ3Config.AnyPendingRefreshes: boolean;
var
  t: ni;
begin
  result := false;
  for t := 0 to count-1 do begin
    if items[t].RefreshPending then begin
      exit(true);
    end;
  end;

end;

procedure TZ3Config.CheckRetries;
var
  t: ni;
begin

  try
    if TECS(csConfigControl) then
    try
      for t:= 0 to FList.count-1 do begin
        FList[t].CheckRetries;
      end;
    finally
      LCS(csConfigControl);
    end;
  finally
  end;
end;

procedure TZ3Config.Clear;
begin
  syncitemcount(0);
end;

constructor TZ3Config.Create(aowner: TComponent);
begin
  inherited;
  FList := TList<TZ3ConfigItem>.create;
  ICS(csConfigControl);

end;

destructor TZ3Config.destroy;
begin
  SyncItemCount(0);
  FList.free;
  DCS(csConfigControl);
  inherited;
end;

function TZ3Config.FindConfigRec(ccCode: string): TSysexConfigRecord;
var
  t: ni;
  itm: TZ3ConfigItem;
begin
  ECS(csConfigControl);
  try
    for t:= 0 to FList.count-1 do begin
      itm := FList[t];
      if itm.CCCode = ccCode then begin
        result := itm.rec;
        break;
      end;
    end;
  finally
    LCS(csCONFIGControl);
  end;

end;

function TZ3Config.GetItems(idx: ni): TZ3configItem;
begin
  result := FList[idx];
end;


procedure TZ3Config.NotifyCCcodeChanged(cccode: string; rec: TSysexConfigRecord; intvalue: int64; bUserChange: boolean);
begin
  if assigned(FOnChanged) then
    FOnChanged(cccode, rec, intvalue, bUserChange);
end;

procedure TZ3Config.NotifyRefreshRequested(cccode: string);
begin
  if assigned(FOnRefresh) then
    FonRefresh(cccode);

end;

procedure TZ3Config.positionChildren;
var
  t: ni;
  itm: TZ3ConfigItem;
  y: ni;
begin
  y := 0;
//  autoscroll := false;
  for t:= 0 to FList.count-1 do begin
    itm := FList[t];
    itm.Width := self.Width;
    itm.PositionStuff;
    if itm.visible then begin
      itm.top := y;
      inc(y,SPACING);
    end;
  end;
//  autoscroll := true;
end;

procedure TZ3Config.Resize;
begin
  inherited;
  POsitionChildren;
end;

procedure TZ3Config.SyncFromGuitarData;
var
  t: ni;
  c: TconfigMember;
begin
  SyncItemCount(g.ConfigCount);
  self.positionChildren;
  for t:= 0 to g.configcount-1 do begin
    c := g.Configs[t];
    try
      self.Items[t].DefineFromIndividualSchema(c.Schema);
      self.positionChildren;
    except
      c.Schema := '';
    end;
  end;
  updatefilter;
  self.positionChildren;



end;

procedure TZ3Config.SyncItemCount(iCount: ni);
var
  t: ni;
  itm: TZ3ConfigItem;

begin
  while FList.count > 0 do begin
    itm := FList[Flist.count-1];
    FList.Delete(FList.Count-1);
    itm.free;
  end;

  if assigned(self.parent) then
    self.width := parent.clientwidth;

  while FList.count < iCount do begin
    itm := TZ3Configitem.create(self);
    itm.visible := false;
    itm.Parent := self;
    itm.Top := FList.count * SPACING;
    itm.Height := SPACING;
    itm.Width := self.Width;
    itm.Left := 0;

    FList.add(itm);
  end;




end;

procedure TZ3Config.SyncJSON(json: TJsonObject);
var
  LSchema, LElements, LItem : TJSONValue;
  t: ni;
  itm: TZ3ConfigItem;
  rec: TSysexConfigRecord;
  ary: TJSONArray;
  s: string;
  i: integer;
  jp: TJSONPair;
begin
  ECS(csConfigControl);
  try
    jp := json.Get('Schema');
    if jp = nil then
      raise EJSONSucks.create('JSON Schema was not found/malformed');
    LSchema:=jp.JsonValue;
    if LSchema is TJsonArray then begin
      ary := LSchema as TJsonArray;
      self.SyncItemCount(ary.count);
      for t:= 0 to ary.count-1 do
      begin
        LItem := ary.Get(t);
        if LItem = nil then
          exit;
        itm := self.Items[t];
        try
          if not TJSONHelper.TryGetValue<string>(LItem,'CC',s) then
            exit;
          if not TJSONHelper.TryGetValue<string>(Litem,'Cat',s) then
            exit;
          if not TJSONHelper.TryGetValue<string>(Litem,'Desc',s) then
            exit;
          if not TJSONHelper.TryGetValue<integer>(Litem,'TT',i) then
            exit;
          if not TJSONHelper.TryGetValue<integer>(Litem,'Typ',i) then
            exit;
          if not TJSONHelper.TryGetValue<integer>(Litem,'F',i) then
            exit;
          rec.cccode := LItem.getvalue<string>('CC');
          rec.catcode := LItem.getvalue<string>('Cat');
          rec.desc := LItem.getvalue<string>('Desc');
          rec.treatment_type := TConfigValueTreatMentType(LItem.getvalue<integer>('TT'));
          rec.transfer_type := TConfigValueTransferType(LItem.getvalue<integer>('Typ'));
          rec.flags := LItem.getvalue<integer>('F');

          itm.rec := rec;
          if not DisableConfigValueFetch then
            itm.RefreshData;

        except
          on E: Exception do begin
            itm.Desc := e.Message;
            itm.Color := clRed;
          end;

        end;



      end;
      updatefilter;
      positionChildren;
    end;

  finally
    LCS(csConfigControl);
//     LJsonObj.Free;
  end;
end;


procedure TZ3Config.UpdateFilter;
var
  t: ni;
  itm: TZ3ConfigItem;
  y: ni;
begin
  y := 0;
//  autoscroll := false;
  for t:= 0 to FList.count-1 do begin
    itm := FList[t];
    itm.visible := itm.matchesfilter(filter);
  end;
//  autoscroll := true;
end;

procedure TZ3Config.UpdateValueFromRemote(cccode: string; val: int64);
var
  t: ni;
  itm: TZ3configitem;
  rec: TSysexConfigRecord;
begin
  ECS(csConfigControl);
  try
    for t:= 0 to FList.Count-1 do begin
      itm := FList[t];
      if itm.rec.CCCode = cccode then begin
        itm.threadsafe_value := val;
        itm.refreshRequestTime := 0;
        windows.PostMessage(itm.steadyHandle, CM_THREADED_UPDATE, 0,0);
        //windows.PostMessage(FormFromControl(itm).Handle, CM_THREADED_UPDATE, 0,0);
        if itm <> nil then
          rec := itm.rec
        else
          rec.CCCode := cccode;
        if Assigned(self.OnCCCodeChanged) then
          OnCCCodeChanged(cccode, rec, val, false);

      end;
    end;
  finally
    LCS(csConfigControl);
  end;


end;

{ TZ3ConfigItem }

procedure TZ3ConfigItem.btnCommmit_Onclick(sender: TObject);
begin
  commitvalue;
end;

procedure TZ3ConfigItem.btnHide_Onclick(sender: TObject);
begin
  TZ3ConfigItem(TControl(sender).parent).visible := false;
  PushParentRefresh;

end;

procedure TZ3ConfigItem.btnRefresh_Onclick(sender: TObject);
begin
  refreshData;
end;


procedure TZ3ConfigItem.CheckRetries;
begin
  if (refreshRequestTime <> 0) and (GetTimeSince(refreshRequestTime) > 1000) then begin
    RefreshData;

  end;
end;

procedure TZ3ConfigItem.CommitValue;
var
  i: int64;
begin
  try
    i := 0;
    if (self.rec.treatment_type = ttManualEntryDecimal)
    or (self.rec.treatment_type = ttSpin) then begin
      i := strtoint64(self.ed.Text);
    end else
    begin
      i := booltoint(cb.Checked);
    end;

    if parent is TZ3Config then with parent as TZ3Config do begin
      NotifyCCCodeChanged(self.CCCode, self.rec, i, true);
    end;
  except
  end;
end;

constructor TZ3ConfigItem.Create(aowner: TComponent);
begin
  inherited;
  random := system.random(1000);
  width := 800;
  lblCCCode := Tlabel.create(self);
  lblCatCode := TLabel.create(self);
  lblDesc := TLabel.create(self);
  lblFlags := TLabel.CREATE(self);

  cb := TCheckbox.create(self);
  btnCommit := Tbutton.create(self);
  btnRefresh := Tbutton.create(self);
  btnHide := TButton.create(self);

  ed := TEdit.create(self);

  ed.text := '';

  lblCCCode.top := 0;
  lblCAtCode.top := 0;
  ed.top := 0;
  lblDesc.top := 0;
  cb.Top := 0;
  btnCommit.top := 0;
  cb.onEnter := self.ed_onenter;
  btnRefresh.top := 0;



  lblCCCode.left := 0;
  lblCatCode.left := 100;
  lblDesc.Left := 200;

//  ed.Left := 200;
//  ed.Width := 80;


  ed.OnEnter := self.ed_OnEnter;
  lblCCCode.parent := self;
  lblFlags.parent := self;
  lblFlags.top := 0;
  lblCatCode.parent := self;
  lblDesc.parent := self;
  ed.parent := self;
  cb.Parent := self;
  cb.Top := 0;
  cb.Anchors := [akRight, akTop, akBottom];
  ed.Top := 0;
  ed.Anchors := [akRight, akTop, akBottom];
  btnCommit.parent := self;
  btnCommit.height := SPACING;
  btnCommit.caption := 'Commit';
  btnCommit.left := ed.left + ed.width;
  btnCommit.Onclick := self.btnCommmit_Onclick;
  btnCommit.Anchors := [akRight, akTop, akBottom];

  btnRefresh.parent := self;
  btnRefresh.height := SPACING;
  btnRefresh.caption := 'Ref';
  btnRefresh.left := ed.left + ed.width+btnCommit.left;
  btnRefresh.Onclick := self.btnRefresh_Onclick;
  btnRefresh.Anchors := [akRight, akTop, akBottom];


  btnHide.parent := self;
  btnHide.height := SPACING;
  btnHide.caption := 'Hide';
  btnHide.left := ed.left + ed.width+btnCommit.left+btnRefresh.width;
  btnHide.Onclick := self.btnHide_Onclick;
  btnHide.Anchors := [akRight, akTop, akBottom];


end;

procedure TZ3ConfigItem.DefineFromIndividualSchema(s: string);
var
  ansi: ansistring;
  itm: TZ3ConfigItem;
  rec: TSysexConfigRecord;
  i: integer;
  s1, s2, s3: string;
  sCC, sDesc, sTyp, sCat, sTT, sF: string;
begin
  try
    Debug.Log('Defining '+s);
    s2 := s;
    SplitString(s2, '"CC":"', s1, sCC);
    SplitString(sCC, '",', sCC, s2);

    SplitString(s2, '"Desc":"', s1, sDesc);
    SplitString(sDesc, '",', sDesc, s2);

    SplitString(s2, '"Typ":', s1, sTyp);
    SplitString(sTyp, ',', sTyp, s2);

    SplitString(s2, '"TT":', s1, sTT);
    SplitString(sTT, ',', sTT, s2);

    SplitString(s2, '"Cat":"', s1, sCat);
    SplitString(sCat, '",', sCat, s2);

    SplitString(s2, '"F":', s1, sF);
    SplitString(sF, '}', sF, s2);

    itm := self;
    rec.cccode := sCC;
    rec.catcode := sCat;
    rec.desc := sDesc;
    rec.treatment_type := TConfigValueTreatmentType(strtoint(sTT));
    rec.transfer_type := TconfigValueTransferType(strtoint(sTyp));
    rec.flags := strtoint(sF);
    self.rec := rec;

    itm.RefreshData;
  finally
//     LJsonObj.Free;
  end;
end;

destructor TZ3ConfigItem.destroy;
begin

  inherited;
end;

procedure TZ3ConfigItem.ed_onenter(sender: TObject);
begin
  refreshData;
end;

function TZ3ConfigItem.GEtCatCode: string;
begin
  result := lblCatCode.caption;
end;

function TZ3ConfigItem.GEtCCCode: string;
begin
  result := lblCcCode.caption;
end;

function TZ3ConfigItem.GEtDesc: string;
begin
  result := lblDesc.caption;
end;

function TZ3Config.GetCount: ni;
begin
  result := FList.Count;
end;

function TZ3Config.GetFilter: string;
begin
  result := FFilter;
end;

function TZ3ConfigItem.GetIntValue: int64;
begin
  result := FVAlue;
end;

function TZ3ConfigItem.GetRec: TSysexConfigRecord;
begin
  result := FRec;
end;

function TZ3ConfigItem.MatchesFilter(s: string): boolean;
begin
  s := lowercase(s);

  result := (zpos(s, lowercase(Self.Frec.CCCode))>=0)
         or (zpos(s, lowercase(Self.Frec.Desc))>=0)
         or (zpos(s, lowercase(Self.Frec.CatCode))>=0);



end;

procedure TZ3ConfigItem.MSg_ThreadedUpdate(var msg: TMessage);
begin
  RemoteUpdate := true;
  IntValue := threadsafe_value;
  retries := 0;
  RemoteUpdate := false;
  btnRefresh.enabled := true;
end;

procedure TZ3ConfigItem.PositionStuff;
begin
  Updatevisibility;

  lblCCCode.left := 0;
  lblCatCode.left := 75;
  lblFlags.left := 150;
  lblDesc.Left := 225;

//  ed.Left := 200;
//  ed.Width := 80;

  cb.Left := width - 300;
  cb.Top := 0;
  cb.height := spacing;

  ed.Top := 0;
  ed.Left := cb.Left;
  ed.height := spacing;

  btnCommit.height := SPACING;
  btnCommit.left := ed.left + ed.width;
  btnRefresh.height := SPACING;
  btnRefresh.left := btnCommit.left+btnCommit.width;
  btnRefresh.width := 45;
  btnHIde.height := SPACING;
  btnHide.left := btnCommit.left+btnCommit.width+btnRefresh.width;
  btnHide.width := 45;

end;

procedure TZ3ConfigItem.PushPArentRefresh;
begin
  TZ3Config(parent).POsitionChildren;
end;

procedure TZ3ConfigItem.RefreshData;
var
  bTime: boolean;
begin
  try
    if refreshRequestTime = 0 then
      refreshRequestTime := getticker;

    bTime := (gettimesince(lastglobalretrytime) < (200+((self.random*4) * retries)));
    if bTime and  (refreshBlockAllowance <=0) then
      exit;

    btnRefresh.enabled := false;

    inc(retries);

    if bTime then
      dec(refreshBlockAllowance)
    else
      refreshBlockAllowance := RefreshBlockSize;


    lastGlobalRetryTime := getticker;

    refreshRequestTime := getticker;

    if parent is TZ3Config then with parent as TZ3Config do begin
      NotifyRefreshRequested(self.CCCode);
    end;
  except
  end;
end;

function TZ3ConfigItem.RefreshPending: boolean;
begin
  Result := refreshRequestTime <> 0;
end;

procedure TZ3ConfigItem.Resize;
begin
  inherited;
  POsitionStuff;
end;

procedure TZ3ConfigItem.SetCatCode(const Value: string);
begin
  lblCatCode.caption := value;
end;

procedure TZ3ConfigItem.SetCCCode(const Value: string);
begin
  lblCCCode.caption  := value;
end;

procedure TZ3ConfigItem.SetDesc(const Value: string);
begin
  lblDesc.caption := value;
end;


procedure TZ3ConfigItem.SetFlags(const Value: ni);
begin
  FFlags := Value;
  lblFlags.caption := Frec.FlagDebug;
end;

procedure TZ3Config.SetFilter(const Value: string);
begin
  fFilter := value;
  UpdateFilter;
  positionChildren;
end;

procedure TZ3ConfigItem.SetIntValue(const Value: int64);
begin
  FValue := value;
  if FRec.treatment_type = ttReadOnlyHEx then begin
    if FRec.transfer_type = stt7bitByte then
      ed.Text := inttohex(value, 2);
    if FRec.transfer_type = stt8bitByte then
      ed.Text := inttohex(value, 2);
    if FRec.transfer_type = sttUInt16 then
      ed.Text := inttohex(value, 4);
    if FRec.transfer_type = sttUInt32 then
      ed.Text := inttohex(value, 8);
    if FRec.transfer_type = sttUInt64 then
      ed.Text := inttohex(value, 16);
  end else
    ed.text := inttostr(FValue);

  if Frec.treatment_type = ttCheckbox then begin
    cb.Checked := value >0;
  end;


end;

procedure TZ3ConfigItem.SetParent(AParent: TWinControl);
begin
  inherited;
  IF AParent <> nil then begin
    PositionStuff;
    steadyHandle := Handle;
  end else
    steadyHandle := INVALID_HANDLE_VALUE;
end;

procedure TZ3ConfigItem.SetRec(const Value: TSysexConfigRecord);
begin
  FRec := value;
  CCcode := FRec.cccode;
  CatCode := FRec.catCode;
  Desc := FRec.desc;
  Flags := FRec.flags;
  SetTreatmentType(rec.treatment_type);
end;

procedure TZ3ConfigItem.SetTreatmentType(tt: TConfigValueTreatmentType);
begin
  FTT := tt;
  updateVisibility;
end;

procedure TZ3ConfigItem.UpdateVisibility;
begin
  ed.Visible := not (FTT = ttCheckbox);
  cb.Visible := FTT = ttCheckbox;
  ed.ReadOnly := (FTT = ttReadOnlyHex) or (FTT = ttReadOnlyDecimal);

end;

initialization
   GlobalRetryThrottle := 15;
   RefreshBlockSize := 1;

end.
