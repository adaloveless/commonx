unit FormBase;

interface
{$DEFINE DISABLE_GLASS}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, generics.collections,
  Dialogs, GDIPOBJ, betterobject, easyimage, menus, systemx, StdCtrls, ApplicationParams,commandprocessor,
  ComCtrls, ExtCtrls, GUIHelpers, GlassControls, screenscrape, typex, numbers, geometry ,gdiplus, guiproclist;

const
  CM_UPDATE_STATE = WM_USER+100;
type
  TfrmBase = class(TForm)
    tmAfterFirstActivation: TTimer;
    tmDelayedFormSave: TTimer;
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmAfterFirstActivationTimer(Sender: TObject);
    procedure tmDelayedFormSaveTimer(Sender: TObject);
  private
    FLateLoaded: boolean;
    FDisabledtimers: TList<TTimer>;
    FCreatingThreadID: THandle;
    FSectReady: boolean;
    FOnPaintPLus: TGPGraphicEvent;
    FCanvasPLus: TCanvasPLus;
    FOriginallySheetOfGlass: boolean;
    FFakeSheetOfGlass: boolean;
    FOnFirstActivation: TNotifyEvent;
    FActivated: boolean;
    FUpdatingState: boolean;
    FOnUpdateState: TNotifyEvent;
    FCursorStack: array of TCursor;
    FManager: TForm;
    FPreviousWindowState: TWindowState;
    sect: _RTL_Critical_Section;
    FonMove: TNotifyEvent;
    function GetCAnvasPLus: TCanvasPlus;
    procedure SetManager(const Value: TForm);
    function Getbottom: ni;
    function GetRight: ni;
    procedure SetBottom(const Value: ni);
    procedure SetRight(const Value: ni);
    procedure SetToken(const Value: string);
    { Private declarations }

  protected
    FToken: string;
    statuspanel: TPanel;
    procedure SaveState;virtual;
    procedure LoadState;virtual;
    procedure LoadLateState;virtual;
    procedure DoUpdateState;virtual;
  public
    destructor Destroy;override;
    { Public declarations }
    class function GetUniqueHash: string; virtual;
    class function GetExistingForm(manager: TForm): TfrmBase;virtual;
    class function GetExistingOrCreate(manager: TForm): TfrmBase;virtual;
    procedure EnableGlass;
    procedure AfterConstruction;override;
    property OriginallySheetOfGlass: boolean read FOriginallySheetOfGlass write FOriginallySheetOfGlass;
    procedure AdjustGlass;
    function HasMenu: boolean;
    procedure RecenterWindow;
    procedure Activate;override;
    property Activated: boolean read FActivated;
    property UpdatingState: boolean read FUpdatingState write FUpdatingState;
    procedure MSG_UpdateState(var msg: TMessage);message CM_UPDATE_STATE;
    procedure UpdateState;

    procedure ResizeFrames;
    procedure Detach;virtual;
    property Manager: TForm read FManager write SetManager;

    function GetMyScreen: integer;
    procedure _WM_GETMINMAXINFO(var mmInfo : TWMGETMINMAXINFO ); message wm_GetMinMaxInfo;
    procedure WMSize(var M : TWMSIZE); message WM_Size;
    property PreviousWindowState: TWindowState read FPreviousWindowState write FPreviousWindowState;
    procedure Lock;
    procedure Unlock;
    function TryLock: boolean;
    procedure SeekAndSaveColumns;
    procedure SeekAndLoadColumns;
    procedure SaveColumns(lv: TListView);
    procedure LoadColumns(lv: TListView);
    procedure FirstActivation;virtual;
    property CanvasPlus: TCanvasPlus read GetCanvasPlus;
    property CreatingThreadID: THandle read FCreatingThreadID;
    procedure Move(var Msg: TWMMove);message WM_MOVE;
    procedure DoMove;virtual;
  published
    property OnPaintPlus: TGPGraphicEvent read FOnPaintPLus write FOnPaintPLus;
    property OnFirstActivation: TNotifyEvent read FOnFirstActivation write FOnFirstACtivation;
    property OnUpdateState: TNotifyEvent read FOnUpdateState write FonupdateState;

    procedure EnforceFormThread;
    property Right: ni read GetRight write SetRight;
    property Bottom: ni read Getbottom write SetBottom;
    procedure DisableActiveTimers;
    procedure RestoreDisabledTimers;
    function AsInterface<T:IUnknown>(guid: TGUID):T;
    function IsInterface(guid: TGUID):boolean;
    procedure ShowStatus(sMEssage: string);
    procedure HideStatus;

    function WindowCenter: TPoint;
    function GetToken: string;virtual;
    property Token: string read GetToken write SetToken;
    procedure WaitForSinglecommand(c: TCommand);
    property OnMove: TNotifyEvent read FonMove write FOnMove;
    procedure SaveComponentStates;
    procedure DelaySaveState;
    procedure PushCursor(cr: TCursor);
    procedure PopCursor;
  end;


type
  TfrmBaseClass = class of TfrmBase;

implementation

uses FrameBase, FormWindowManager, debug;

{$R *.dfm}

procedure TfrmBase.Activate;
begin
  inherited;
  if not FActivated then begin
    FActivated := true;
    FirstActivation;
  end;

  if assigned(Manager) then
    TfrmWindowManager(Manager).ActiveForm := self;

end;

procedure TfrmBase.AdjustGlass;
var
  t: integer;
  wc: TWinControl;
begin
  if csDestroying in componentstate then exit;

{$IFNDEF DISABLE_GLASS}
  if OriginallySheetOfGlass and HasMenu then begin
    self.GlassFrame.Bottom := clientheight+3;
  end;

  for t:= 0 to ControlCount-1 do begin
    if controls[t] is TWinControl then begin
      wc := controls[t] as TWinControl;
      wc.Repaint;
    end else
      continue;


  end;
{$ENDIF}

end;

procedure TfrmBase.AfterConstruction;
begin
  inherited;
  {$IFDEF DISABLE_GLASS}
  GlassFrame.Enabled := false;
  {$ELSE}
  if GlassFrame.Enabled then begin
    self.DoubleBuffered := true;
    OriginallySheetOfGlass := GlassFrame.SheetOfGlass;
    self.EnableGlass;
  end;
  {$ENDIF}

end;


procedure TfrmBase.EnableGlass;
var
  t: integer;
  c: TComponent;
  wc: TWinControl;
begin
  if csDestroying in componentstate then exit;
{$IFNDEF DISABLE_GLASS}
  OriginallySheetOfGlass := GlassFrame.SheetOfGlass;
  if HasMenu then begin
    GlassFrame.SheetOfGlass := false;
    AdjustGlass;
  end;


  self.DoubleBuffered := true;
  self.GlassFrame.Enabled := true;
  for t:= 0 to self.ComponentCount -1 do begin
    c := self.components[t ];
    if c is TWinControl then begin
      wc := c as TWincontrol;
      //wc.DoubleBuffered := true;
    end;
  end;
{$ENDIF}

end;

procedure TfrmBase.EnforceFormThread;
begin
  if GetcurrentThreadID <> CreatingThreadID then
    raise exception.Create('A function was called that requires threadid '+inttostr(CreatingThreadID)+' but was actually called from '+inttostr(GetCurrentThreadID));

end;

procedure TfrmBase.FirstActivation;
begin
  if csDesigning in componentstate then
    exit;
  LoadState;
  SeekAndLoadColumns;
  if assigned(OnFirstActivation) then
    OnFirstActivation(self);
  tmAfterFirstActivation.enabled := true;

end;

procedure TfrmBase.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveState;
  if Manager <> nil then
    Action := caFree;
end;

procedure TfrmBase.FormCreate(Sender: TObject);
begin
  inherited;
  FToken := name;
  FCreatingThreadID := GetCurrentThreadId;
  InitializeCriticalSEction(sect);
  FSectReady:= true;
  FDisabledtimers := TList<TTimer>.create;


end;

procedure TfrmBase.FormDestroy(Sender: TObject);
begin
  Detach;
  FDisabledTimers.free;

end;

procedure TfrmBase.FormPaint(Sender: TObject);
begin
  inherited;
//  canvas.brush.color := 0;
//  canvas.Pen.Color := 0;
//  canvas.TextOut(0,0,'1');
end;


procedure TfrmBase.FormResize(Sender: TObject);
var
  mon: TMonitor;
begin
  inherited;
  if csDesigning in componentstate then
    exit;
  self.tmDelayedFormSave.enabled := false;
  self.tmDelayedFormSave.enabled := true;

  if csDestroying in componentstate then exit;
  if visible then
    adjustglass;

  if (WindowState = wsNormal) then begin
    if not IsValidScreenCoordinate(left, top) then begin
      mon := Screen.MonitorFromRect(rect(left, top, left+width-1, right+height-1));
      if mon <> nil then begin
        left := mon.left;
        top := mon.top;
      end;
    end;
  end;

end;

function TfrmBase.Getbottom: ni;
begin
  result := (top+height)-1;
end;

function TfrmBase.GetCanvasPLus: TCanvasPlus;
begin

  if FCAnvasPLus = nil then
    FCAnvasPLus := TCanvasPLus.Create(self.canvas.Handle);

  result := FCAnvasPLus;

end;



class function TfrmBase.GetExistingForm(manager: TForm): TfrmBase;
var
  t: integer;
  wm: TfrmWindowManager;
  s1, s2: string;
begin
  wm := manager as TfrmWindowManager;
  result := nil;
  for t := 0 to wm.windowcount-1 do begin

    if (TfrmBaseClass(wm.windows[t].Form.ClassType).GetUniqueHash) = GetUniqueHash then begin
      result := wm.Windows[t].form;
    end;
  end;

end;

class function TfrmBase.GetExistingOrCreate(manager: TForm): TfrmBase;

begin
  result := GetExistingForm(manager);
  if result = nil then begin
    result := create(manager);
    result.manager := TfrmWindowManager(manager);
  end;
end;

function TfrmBase.GetMyScreen: integer;
var
  t: integer;
  a: array of nativefloat;
  p1,p2: TPoint;
  m: TMonitor;
  max: nativefloat;
  maxt: ni;
begin
  result := 0;


  setlength(a, screen.monitorcount);
  //calculate area on screen

  p1.x := 0;
  p1.Y := 0;


  max := -1;
  maxt := 0;
  for t:= 0 to screen.monitorcount-1 do begin
    m := screen.Monitors[t];

    a[t] := GetIntersectedArea(self.Left, self.Top, self.Right, self.Bottom,
                               m.left, m.Top, m.Left+m.Width-1, m.top+m.Height-1);

    if a[t] > max then begin
      max := a[t];
      maxt := t;
    end;

  end;

  result := maxt;

end;

function TfrmBase.GetRight: ni;
begin
  result := left+width-1;
end;

function TfrmBase.GetToken: string;
begin
  result := FToken;
end;

class function TfrmBase.GetUniqueHash: string;
begin
  result := classname;
end;

function TfrmBase.HasMenu: boolean;
var
  t: integer;
begin
  result := false;
  for t:= 0 to componentcount-1 do begin
    if components[t] is TMainMenu then begin
      result := true;
      break;
    end;
  end;

end;


procedure TfrmBase.HideStatus;
begin
  statuspanel.free;
  statuspanel := nil;
end;

procedure TfrmBase.LoadColumns(lv: TListView);
var
  ap: TAppParams;
  sKey: string;
  t: ni;
  newwid: ni;
begin
  ap := NeedUserParams;
  try
    for t:= 0 to lv.Columns.Count-1 do begin
      sKey := 'STATE_'+self.Token+'->'+lv.Name+'['+inttostr(t)+'].width';
      newwid := ap.GetItemEx(sKey, lv.Columns[t].Width);

      lv.Columns[t].Width := newwid;
    end;
  finally
    NoNeedUserParams(ap);
  end;

end;

procedure TfrmBase.LoadLateState;
var
  x,y: int64;
  bDoMaximize: boolean;
begin
  if csDesigning in componentstate then
    exit;
  UpdatingState := true;
  try

    bDoMaximize := UPGet('STATE_'+token+'_maximize', false);

    SeekAndLoadColumns;

    x := UPGet('STATE_'+token+'_left', -1);
    y := UPGet('STATE_'+token+'_top', -1);
    if bDoMaximize or IsValidScreenCoordinate(x,y) then begin
      self.Left := x;
      self.Top := y;

      x := UPGet('STATE_'+token+'_width', -1);
      if x > 0 then
        self.Width := x;

      x := UPGet('STATE_'+token+'_height', -1);
      if x > 0 then
        self.height := x;

    end;

    if bDoMaximize then begin
      self.WindowState := wsMaximized
    end
    else begin
      self.WindowState := wsNormal;
    end;


  finally
    FLateLoaded := true;
    UpdatingState := false;

  end;
end;

procedure TfrmBase.LoadState;
begin
  //
end;

procedure TfrmBase.Lock;
begin
  EnterCriticalSection(sect);
end;

procedure TfrmBase.Move(var Msg: TWMMove);
begin
  if assigned(tmDelayedFormSave) then begin
    self.tmDelayedFormSave.enabled := false;
    self.tmDelayedFormSave.enabled := true;
  end;

  if csDesigning in componentstate then
    exit;
  DoMove;


  if assigned(OnMove) then
    FonMove(self);
end;

procedure TfrmBase.MSG_UpdateState(var msg: TMessage);
begin
  UpdateState;
end;



procedure TfrmBase.PopCursor;
begin
  cursor := FCursorStack[high(FCursorStack)];
  setlength(FCursorStack, length(FCursorStack)-1);
  invalidate;
  refresh;
end;

procedure TfrmBase.PushCursor(cr: TCursor);
begin
  setlength(FCursorStack, length(FCursorStack)+1);
  FCursorStack[high(FCursorStack)] := cursor;
  cursor := cr;
  invalidate;
  refresh;
end;

procedure TfrmBase.RecenterWindow;
var
  pm: TMonitor;
begin
  //centering calculation
  pm := GetPrimaryMonitor;
  if pm <> nil then begin
    Top := ((pm.Height div 2) - (self.height div 2))+pm.top;
    Left := ((pm.Width div 2) - (self.Width div 2))+pm.left;
  end;

end;



procedure TfrmBase.ResizeFrames;
VAR
  t: integer;
  f: TfrmFrameBase;
begin
  for t:= 0 to Componentcount-1 do begin
    if components[t] is TfrmFrameBase then begin
      f := components[t] as TfrmFrameBase;
      f.ForceResize(f);
    end;
  end;

end;

procedure TfrmBase.RestoreDisabledTimers;
begin
  if csDesigning in componentstate then
    exit;
  while fdisabledTimers.count > 0 do begin
    Fdisabledtimers[FDisabledTimers.count-1].Enabled := true;
    FdisabledTimers.delete(FDisabledTimers.count-1);
  end;

end;

procedure TfrmBase.Unlock;
begin
  LeaveCriticalSection(sect);
end;

procedure TfrmBase.UpdateState;
var
  bOld: boolean;
begin
  if GetCurrentThreadID <> CreatingThreadID then
    Debug.Log('THREAD VIOLATION in UpdateState');
  bOld := UpdatingState;
  UpdatingState := true;
  try
    DoUpdateState;
    if Assigned(OnUpdateState) then begin
      OnupdateState(self);
    end;
  finally
    updatingState := bOld;
  end;
end;

procedure TfrmBase._WM_GETMINMAXINFO(var mmInfo: TWMGETMINMAXINFO);
var
  i: integer;
begin
//  tagMINMAXINFO = packed record
//    ptReserved: TPoint;
//    ptMaxSize: TPoint;
//    ptMaxPosition: TPoint;
//    ptMinTrackSize: TPoint;
//    ptMaxTrackSize: TPoint;
//  end;

  if Manager = nil then exit;

  if GetMyScreen = TfrmBase(Manager).GetMyScreen then begin
    i := GetMyScreen;

    mmInfo.MinMaxinfo.ptMaxPosition.x := (screen.monitors[i].Left+manager.Width);
    mmInfo.MinMaxinfo.ptMaxPosition.y := screen.monitors[i].Top;
    mmInfo.MinMaxinfo.ptMaxSize.x := screen.monitors[i].Width-manager.Width;
    mmInfo.MinMaxinfo.ptMaxSize.y := manager.height;


  end;





end;


procedure TfrmBase.WaitForSinglecommand(c: TCommand);
begin
  self.showstatus(c.Status);
  try
    while not c.IsComplete do begin
      sleep(100);
      self.ShowStatus(c.Status);
    end;
    try
      c.WaitFor;
    except
    end;
  finally
    self.HideStatus;
  end;

end;

function TfrmBase.WindowCenter: TPoint;
begin
  result := point((left+width) div 2, (top+height) div 2);
end;

procedure TfrmBase.WMSize(var M : TWMSIZE) ;
begin
  if Application.MainForm = self then begin
    inherited;
    exit;
  end;



  if m.SizeType <> Size_Minimized then begin
    case m.SizeType of
      SIZE_RESTORED: PreviousWindowState := wsNormal;
//      SIZE_MINIMIZED: PreviousWindowState := wsMinimized;
      SIZE_MAXIMIZED: PreviousWindowState := wsMaximized;
      SIZE_MAXSHOW: PreviousWindowState := wsMaximized;
      SIZE_MAXHIDE: PreviousWindowState := wsMaximized;
    end;
  end;

  if M.SizeType = Size_Minimized then
  begin
//    PreviousWindowState := WindowState;
    ShowWindow(Handle,Sw_Hide) ;
    M.Result := 0;
  end
  else
    inherited;//DefaultHandler(m);
end;


procedure TfrmBase.DelaySaveState;
begin
  tmDelayedFormSave.Enabled := false;
  tmDelayedFormSave.Enabled := true;

end;

destructor TfrmBase.Destroy;
begin
  inherited;
  DeleteCriticalSection(sect);
end;

procedure TfrmBase.Detach;
begin
  Manager := nil;
end;


procedure TfrmBase.DisableActiveTimers;
var
  tm: TTimer;
  t: ni;
begin
  for t:= 0 to componentcount-1 do begin
    if components[t] is TTimer then begin
      tm := components[t] as TTimer;
      if tm.enabled then begin
        FDisabledtimers.add(tm);
        tm.enabled := false;
      end;
    end;
  end;
end;

procedure TfrmBase.DoMove;
begin
  if not Activated then
    exit;

  tmDelayedFormSave.enabled := false;
  tmDelayedFormSave.enabled := true;
//  FormResize(self);
end;

procedure TfrmBase.DoUpdateState;
begin
  if csDesigning in componentstate then
    exit;
  //no implementation required

end;

procedure TfrmBase.SaveColumns(lv: TListView);
var
  ap: TAppParams;
  sKey: string;
  t: ni;
begin
  UPBegin;
  try
    for t:= 0 to lv.Columns.Count-1 do begin
      sKey := 'STATE_'+self.Token+'->'+lv.Name+'['+inttostr(t)+'].width';
      UPPut(sKey, inttostr(lv.Columns[t].Width));

    end;

  finally
    UPEnd;
  end;

end;

procedure TfrmBase.SaveComponentStates;
begin
  if csDesigning in componentstate then
    exit;
  if not UpdatingState then begin
    SaveState;
  end;
end;

procedure TfrmBase.SaveState;
var
  x: int64;
begin
  if csDesigning in componentstate then
    exit;
  if not FLateLoaded then
    exit;
  SeekAndSaveColumns;
  if not (windowstate=wsMaximized) then begin
    UPPut('STATE_'+token+'_width', width);
    UPPut('STATE_'+token+'_height', height);
    UPPut('STATE_'+token+'_left', left);
    UPPut('STATE_'+token+'_top', top);
  end;
  UPPut('STATE_'+token+'_maximize', WindowState=wsMaximized);

end;

procedure TfrmBase.SeekAndLoadColumns;
var
  lv: TlistView;
  t: ni;
begin
  if csDesigning in componentstate then
    exit;
  for t:= 0 to componentcount-1 do begin
    if components[t] is TListView then begin
      lv := components[t] as TListView;
      LoadColumns(lv);
    end;
  end;
end;

procedure TfrmBase.SeekAndSaveColumns;
var
  lv: TlistView;
  t: ni;
begin
  if csDesigning in componentstate then
    exit;
  for t:= 0 to componentcount-1 do begin
    if components[t] is TListView then begin
      lv := components[t] as TListView;
      SaveColumns(lv);
    end;
  end;
end;


procedure TfrmBase.SetBottom(const Value: ni);
var
  i: ni;
begin
  i := value-top;
  if i > 0 then begin
    height := i;
  end;
end;

procedure TfrmBase.SetManager(const Value: TForm);
begin
  if assigned(FManager) then begin
    TfrmWindowManager(FManager).UnregisterWindow(self);
  end;


  FManager := Value;


  if assigned(FManager) then begin
    TfrmWindowManager(FManager).registerWindow(self);
  end;



end;

procedure TfrmBase.SetRight(const Value: ni);
var
  i: ni;
begin
  i := value-left;
  if i > 0 then begin
    width := i;
  end;
end;

procedure TfrmBase.SetToken(const Value: string);
begin
  FToken := value;
end;

procedure TfrmBase.ShowStatus(sMEssage: string);
begin
  if csDesigning in componentstate then
    exit;
  if statuspanel = nil then begin
    statuspanel := TPanel.create(self);
  end;

  statuspanel.Font.Size := 18;
  statuspanel.parent := self;
  statuspanel.Width := clientwidth;
  statuspanel.Height := clientheight div 8;
  statuspanel.Left := 0;
  statuspanel.top := (clientheight div 16) * 7;
  statuspanel.BringToFront;
  statuspanel.caption := sMessage;

  refresh;

end;

procedure TfrmBase.tmAfterFirstActivationTimer(Sender: TObject);
begin
  if csDesigning in componentstate then
    exit;
  tmAfterFirstActivation.enabled := false;
  LoadLateState;

end;

procedure TfrmBase.tmDelayedFormSaveTimer(Sender: TObject);
begin
  if csDesigning in componentstate then
    exit;
  SaveComponentStates;
  tmDelayedFormSave.enabled := false;
end;

function TfrmBase.TryLock: boolean;
begin
  result := TryEnterCriticalSection(sect);
end;

function TfrmBase.AsInterface<T>(guid: TGUID): T;
begin
  if IsInterface(guid) then begin
    //Supports(self, T, result);
    self.QueryInterface(guid,result);
  end;

end;

function TfrmBase.IsInterface(guid: TGUID): boolean;
var
  cout:IUnknown;
begin
  result := self.QueryInterface(guid,cout)= 0;
end;



initialization




end.

