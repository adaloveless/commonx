unit FormMockMobile;

//this unit is intended to allow windows apps to
//behave similarly to mobile apps.

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, ScaledLayoutProportional,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, typex, numbers,
  formFMXBase, FMX.Objects, FMX.Controls.Presentation, better_collections, guihelpers_fmx,
  FMX.Gestures, tickcount, betterobject, geometry, stringx, FMX.Effects;

type
  TSetupProc = reference to procedure (frm: TfrmFMXBAse);

  TMQInfo = record
    startTime: ticker;
    msg: string;
    visual: TRectangle;
    function Opacity: single;
    function IsFinished: boolean;
    function Enabled: boolean;
  end;
  PMQinfo = ^TMQInfo;

  Tmm = class(TfrmFMXBase)
    q: TPanel;
    TempMessageTimer: TTimer;
    BusyTimer: TTimer;
    procedure btnGestureTestGesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure TempMessageTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BusyTimerTimer(Sender: TObject);
  private
    function GetShowingForm: TfrmFMXBase;
  protected
    lastbusytimertime: ticker;
    FTemporaryMessageQueue: IHolder<TStringList>;
    FTempMsgInfo: TMQInfo;
    formstack: TBetterList<TfrmFMXBase>;
    procedure DoShow; override;
    { Private declarations }
    procedure PushForm(f: TfrmFMXBase);
    function PopForm: TfrmFMXBase;
  public
    FancyFrame: TFrame;
    OnShowFancy: TProc;
    OnHideFancy: TProc;
    OnFancyUpdate: TProc<nativeint>;
    OnFancyMessage: TProc<string>;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure ToggleBusy(busy: Boolean); override;
    function WatchCommands: Boolean; override;

    { Public declarations }
    property showingform: TfrmFMXBase read GetShowingForm;
    procedure TakeControls;
    procedure MoveControls(owner, cfrm, cto: TFMXObject);
    procedure GiveBackControls;
    procedure ShowForm(f:TfrmFMXBase);
    constructor Create(AOwner: TComponent); override;
    procedure RemoveForm(f: TfrmFMXBase);
    procedure ShowFormClass<T: TfrmFMXBase>(var f: T);
    procedure ShowFormClassAndSetup<T: TfrmFMXBase>(var f: T; p: TProc);
    procedure TemporaryMessage(s: string);
    procedure TempMessageEngine;

    procedure ShowFancy;
    procedure HideFancy;
    procedure Fancyupdate(intv: ni);
    procedure FancyMessage(s: string);
  end;

type
  TFormClass = class of TfrmFMXBase;
var
  frmDefault: TFormClass;
  OnConfigureMM: Tproc;
  mm: Tmm;

procedure MM_ShowForm(frm: TfrmFMXBase);
procedure MM_CloseForm(frm: TfrmFMXBase);


procedure SetMMConfig(proc: TProc);
procedure ConfigureMM();


type
  TAlignSet = set of TAlignLayout;
  TChildFindProc = reference to function (alignset: TAlignset; parent: TFMXObject): TFMXObject;


implementation

uses debug;


procedure Tmm.ShowFormClass<T>(var f: T);
begin
  if f = nil then begin
    Debug.Log(CLR_UI+'***Creating form: '+ T.classname);
    f := T.create(application);
    Debug.Log(CLR_UI+'***Created form: '+ T.classname);
  end;
  Debug.Log(CLR_UI+'***Showing form: '+ f.classname);
  MM_ShowForm(f);
end;

procedure Tmm.ShowFormClassAndSetup<T>(var f: T; p: TProc);
begin
  if f = nil then begin
    Debug.Log(CLR_UI+'***Creating form: '+ f.classname);
    f := T.create(application);
    Debug.Log(CLR_UI+'***Created form: '+ f.classname);
  end;

  Debug.Log(CLR_UI+'***running lambda setup proc()');
  p();

  Debug.Log(CLR_UI+'***Showing form: '+ f.classname);
  MM_ShowForm(f);
end;

procedure MM_ShowForm(frm: TfrmFMXBase);
begin
  //Transfer controls from form to this one
  if frm = nil then
    raise ECritical.create('trying to show a nil form');
  mm.ShowForm(frm);



end;

{$R *.fmx}

{ TfrmMockMobile }

procedure Tmm.AfterConstruction;
begin
  inherited;
  formMockmobile.configureMM;
end;

procedure Tmm.BeforeDestruction;
begin
  inherited;
  for var t := 0 to formstack.count-1 do begin
    var f := formstack.Items[t];
    f.mock := nil;
  end;
  formstack.free;

end;

procedure Tmm.btnGestureTestGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  inherited;
//  btnGestureTest.text := 'gesture!'+getticker.tostring;
end;

procedure Tmm.BusyTimerTimer(Sender: TObject);
begin
  inherited;
  var tmSince := gettimesince(lastbusytimertime);
  lastbusytimertime := getticker;
  WatchCommands;
  if Assigned(OnFancyUpdate) then begin
    OnFancyUpdate(tmSince);
  end;
  Curtains_Frame(tmSince);
end;

constructor Tmm.Create(AOwner: TComponent);
begin
  inherited;
  formstack := TBetterList<TfrmFMXBase>.create;
  Debug.Log(self.ClassName+' created.');
end;

procedure Tmm.DoShow;
begin
  inherited;
  if showingform = nil then begin
    Curtains(procedure begin
      var f := frmDefault.create(application);
      showform(f);
    end);
  end;

end;

procedure Tmm.FancyMessage(s: string);
begin
  //
end;

procedure Tmm.Fancyupdate(intv: ni);
begin
  if assigned(Onfancyupdate) then
    OnFancyUpdate(intv);
end;

procedure Tmm.FormCreate(Sender: TObject);
begin
  inherited;
  //
end;

function Tmm.GetShowingForm: TfrmFMXBase;
begin
  if formstack.count = 0 then
    exit(nil);
  result := formstack.Last;
end;

procedure Tmm.GiveBackControls;
begin
  MoveControls(showingform, self, showingform);
end;


procedure Tmm.HideFancy;
begin
  if assigned(OnHideFancy) then
    OnHideFancy;

end;

function Tmm.PopForm: TfrmFMXBase;
begin
  result := showingform;
  formstack.remove(result);
end;

procedure Tmm.PushForm(f: TfrmFMXBase);
begin
  formstack.add(f);
end;

procedure Tmm.RemoveForm(f: TfrmFMXBase);
begin
  if f = showingform then
    GiveBackControls;
  formstack.remove(f);

  if showingform <> nil then begin
    TakeControls;
    showingform.ActivateOrTransplant;
    showingform.ActivateByPop;
  end;
end;

procedure Tmm.ShowFancy;
begin
  if assigned(OnShowFancy) then begin
    OnShowFancy();
  end;

end;

procedure Tmm.ShowForm(f: TfrmFMXBase);
begin

  if showingform <> nil then begin
      showingform.Parent := nil;
    showingform.DeactivateOrTransplant;
    giveBackControls;
//    showingform.Hide;
  end;


  f.mock := self;
  PushForm(f);
  if showingform <> nil then begin
    Debug.Log(CLR_F+'*********************************************************');
    Debug.Log(CLR_F+'Form: '+showingform.classname);
    Debug.Log(CLR_F+'*********************************************************');
    showingform.parent := self;
    showingform.left := 0;
    showingform.width := width;
    showingform.top := 0;
    showingform.height := height;
    TakeControls;
    showingform.ActivateOrTransplant;
    showingform.ActivateByPush;
//    showingform.show;
  end;

  show;

end;

procedure Tmm.TakeControls;
begin
  MoveControls(showingform, showingform, self);
end;

procedure Tmm.MoveControls(owner, cfrm, cto: TFMXObject);
{$IFNDEF NEW_MOVE}
begin
  if cfrm = nil then
    exit;
  var hitend := false;
  repeat
    hitend := true;
    for var t:= 0 to cfrm.ChildrenCount-1 do begin
      var c := cfrm.Children[t];
      if c.owner = owner then begin
        if c.parent <> cto then begin
//          Debug.Log('taking control: '+c.name+' from '+cfrm.name+' to '+cto.name);
          hitend := false;
          c.Parent := cto;
//          Debug.Log('control now belongs to '+c.parent.name);
          break;
        end;
      end;
    end;
  until hitend;
end;
{$ELSE}
begin
  var LR: TChildFindProc := function (alignset: TAlignSet; parent: TFMXObject): TFMXObject
      begin
        var best: single := 0.0;
        result := nil;
        for var t:= 0 to parent.ChildrenCount-1 do begin
          var o := parent.Children[t];
          if o is TControl then begin
            var c := parent.Children[t] as TControl;
            if c.Align in alignset then begin
              if t = 0 then best := guihelpers_fmx.control_GetPosition(c).x
              else begin
                best := lesserof(best,guihelpers_fmx.control_GetPosition(c).x);
                result := c;
              end;
            end;
          end;
        end;
      end;

  var RL: TChildFindProc := function (alignset: TAlignSet; parent: TFMXObject): TFMXObject
      begin
        var best: single := 0.0;
        result := nil;
        for var t:= 0 to parent.ChildrenCount-1 do begin
          var o := parent.Children[t];
          if o is TControl then begin
            var c := parent.Children[t] as TControl;
            if c.Align in alignset then begin
              if t = 0 then best := guihelpers_fmx.control_GetPosition(c).x
              else begin
                best := greaterof(best,guihelpers_fmx.control_GetPosition(c).x);
                result := c;
              end;
            end;
          end;
        end;
      end;

  var UD: TChildFindProc := function (alignset: TAlignSet; parent: TFMXObject): TFMXObject
      begin
        var best: single := 0.0;
        result := nil;
        for var t:= 0 to parent.ChildrenCount-1 do begin
          var o := parent.Children[t];
          if o is TControl then begin
            var c := parent.Children[t] as TControl;
            if c.Align in alignset then begin
              if t = 0 then best := guihelpers_fmx.control_GetPosition(c).y
              else begin
                best := lesserof(best,guihelpers_fmx.control_GetPosition(c).y);
                result := c;
              end;
            end;
          end;
        end;
      end;

  var DU: TChildFindProc := function (alignset: TAlignSet; parent: TFMXObject): TFMXObject
      begin
        var best: single := 0.0;
        result := nil;
        for var t:= 0 to parent.ChildrenCount-1 do begin
          var o := parent.Children[t];
          if o is TControl then begin
            var c := parent.Children[t] as TControl;
            if c.Align in alignset then begin
              if t = 0 then best := guihelpers_fmx.control_GetPosition(c).y
              else begin
                best := greaterof(best,guihelpers_fmx.control_GetPosition(c).y);
                result := c;
              end;
            end;
          end;
        end;
      end;

  var mov: TProc<TAlignSet, TChildFindProc> := procedure (alignset: TAlignset; proc: TChildFindProc) begin
    var cc: TFMXObject := nil;
    repeat
      cc := proc(alignset, cfrm);
      if assigned(cc) then begin
//        Debug.Log('taking control: '+cc.name);
        cc.parent := cto;
        if cc is TScaledLayoutProportional then
          TScaledLayoutProportional(cc).ForceRealign;
      end;
    until cc = nil;
  end;

  mov([
        TAlignLayout.Client,
        TAlignLayout.Contents,
        TAlignLayout.Center,
        TAlignLayout.VertCenter,
        TAlignLayout.HorzCenter,
        TAlignLayout.Vertical,
        TAlignLayout.Scale,
        TAlignLayout.Fit], UD);
  mov([TAlignLayout.Left, TAlignLayout.MostLeft, TAlignLayout.FitLeft], LR);
  mov([TAlignLayout.Right, TAlignLayout.MostRight, TAlignLayout.FitRight], RL);
  mov([TAlignLayout.Top, TAlignLayout.MostTop], UD);
  mov([TAlignLayout.Bottom, TAlignLayout.MostBottom], DU);







  showingform.transplanted := cto = showingform;
end;
{$ENDIF}

procedure MM_CloseForm(frm: TfrmFMXBase);
begin
  frm.UnregisterWithMockMobile;

end;

{ TMQInfo }

function TMQInfo.Enabled: boolean;
begin
  result := starttime <> 0;
end;

function TMQInfo.IsFinished: boolean;
begin
  result := GetTimeSince(starttime) > 3000;
end;

function TMQInfo.Opacity: single;
begin
  var nao := GetTicker;
  var dif := gettimesince(nao, starttime);
  if dif < 500 then
    exit(dif / 500)
  else if dif > 3000 then
    exit(0.0)
  else if dif > 2500 then
    exit((3000-dif)/500)
  else
    exit(1.0);


end;

procedure Tmm.TempMessageEngine;
begin
  var mq : PMQinfo := @FTempMsginfo;
  if (not mq.enabled) and (FTemporaryMessageQueue=nil) then
    exit;
  if (not mq.enabled) and (FTemporaryMessageQueue.o.count = 0) then
    exit;

  //start if not enabled
  if not mq.Enabled then begin
    mq.msg := FTemporaryMessageQueue.o[0];
    FTemporaryMessageQueue.o.delete(0);
    mq.startTime := getticker;
    mq.visual := TRectangle.Create(self);
    mq.visual.XRadius := 10;
    mq.visual.YRadius := 10;
    mq.visual.Fill.color := $EF000000;
    mq.visual.width := self.clientwidth;
    var txt: TLabel := TLabel.create(mq.visual);
    txt.parent := mq.visual;
    txt.AutoSize := true;
    txt.WordWrap := true;
    txt.Text := mq.msg;
    txt.width := mq.visual.Width;
//    txt.FontColor := $FFFFFFFF;
    txt.TextSettings.FontColor := $FFFFFFFF;
    txt.StyledSettings := [];
    //txt.align := TAlignLayout.Contents;
    txt.TextAlign := TTextAlign.Center;
    mq.visual.parent := self;

    Debug.Log('vis = '+mq.visual.width.tostring+', form = '+self.clientwidth.tostring);
    mq.visual.position.x := 0;
    mq.visual.position.y := 0;
    mq.visual.height := 1000;
//  mq.visual.align := TAlignLayout.top;
    mq.visual.BringToFront;

    mq.visual.Height := txt.height;
//    mq.visual.Height := mq.visual.Height * (1+    stringx.CountChar(txt.Text, #13));





  end;
  //continue if enabled
  if mq.enabled then begin
    mq.visual.Height := mq.visual.Controls[0].height;
    mq.visual.Opacity := mq.Opacity;

//    mq.visual.position.y := Interpolate(gettimesince(mq.startTime)/3000, 32,0);
//    debug.Log(mq.visual.opacity.tostring);
  end;
  //finish
  if mq.IsFinished then begin
    mq.startTime := 0;
    mq.visual.parent := nil;
    mq.visual.DisposeOf;
    mq.visual := nil;
  end;

  if ((not mq.enabled) and (FTemporaryMessageQueue.o.count=0)) then
    TempMessageTimer.enabled := false;

end;

procedure Tmm.TempMessageTimerTimer(Sender: TObject);
begin
  inherited;
  TempMessageEngine;
end;

procedure Tmm.TemporaryMessage(s: string);
begin
  if FTemporaryMessageQueue = nil then begin
    FTemporaryMessageQueue := Tholder<TSTringList>.create;
    FTemporaryMessageQueue.o := TStringlist.create;
  end;

  FTemporaryMessageQueue.o.add(s);
  TempMessageTimer.Enabled := true;


end;

procedure Tmm.ToggleBusy(busy: Boolean);
begin
  inherited;
  if busy then begin
    ShowFancy;
  end else begin
    HideFancy;
  end;
end;

function Tmm.WatchCommands: boolean;
begin
  inherited;
  if not BusyTimer.Enabled then
    BusyTimer.Enabled := true;
end;

procedure ConfigureMM();
begin
  if assigned(OnConfigureMM) then
    OnConfigureMM();
end;

procedure SetMMConfig(proc: TProc);
begin
  OnconfigureMM := proc;
end;

end.
