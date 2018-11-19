unit ProgressForm;

{xDEFINE DISABLE_MODAL_COMMAND_WATCHING}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, exceptions,orderlyinit,
  Dialogs, StdCtrls, ComCtrls, FormBase, GlassControls,exe, commandprocessor,
  ExtCtrls, generics.collections.fixed, debug, FrameHostPanel, FormBGThreadWatcher,
  Vcl.WinXCtrls;

type
  TfrmProgress = class(TfrmBase)
    PB: TProgressBar;
    TimerWatchCommand: TTimer;
    TimerWatchQueue: TTimer;
    panBG: TFrameHostPanel;
    Timer1: TTimer;
    Timer2: TTimer;
    lbl: TLabel;
    procedure frmBaseCreate(Sender: TObject);
    procedure frmBaseClose(Sender: TObject; var Action: TCloseAction);
    procedure frmBaseDestroy(Sender: TObject);
    procedure TimerWatchCommandTimer(Sender: TObject);
    procedure TimerWatchQueueTimer(Sender: TObject);
    procedure frmBaseDblClick(Sender: TObject);
    procedure frmBaseFirstActivation(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure frmBaseActivate(Sender: TObject);
  private
    FcmdWatching: TCommand;
    FQueueWatching: TCommandProcessor;
    fReferences: integer;
    FPBs: TList<TProgressBar>;
    FOriginalHeight: integer;
    procedure AddPB;
    procedure RemovePB;
    function GetCmdWatching: TCommand;
    procedure SetCmdWatching(const Value: TCommand);
    procedure AdjustFormHeight;


    { Private declarations }
  public
    bg: TfrmBGThreadWatcher;
    constructor Create(aowner: TComponent);reintroduce;virtual;

    { Public declarations }
    procedure ExeConsole(ss: TConsolecaptureStatus; sStatus: string);
    procedure WatchSingleCommand(cmd: TCommand);
    procedure WatchSingleCommand_NonModal(cmd: TCommand);
    procedure WatchCommandQueue(q: TCommandProcessor);
    procedure EnableCommandtimer(var msg: TMessage);message WM_USER+1;
    procedure EnableCommandQueuetimer(var msg: TMessage);message WM_USER+2;
    procedure beginProgress();
    procedure EndProgress();
    function CurrentPB: TProgressBar;
    property CmdWatching: TCommand read GetCmdWatching write SetCmdWatching;
    procedure RefreshAllBars;

  end;

var
  frmProgress: TfrmProgress;

procedure ShowProgress(sStatus: string; min,max,pos: integer);overload;
procedure ShowProgress(pos,max: integer);overload;
//procedure HideProgress;
procedure BeginProgress();
procedure EndProgress();



implementation

{$R *.dfm}

procedure BeginProgress();
begin
  if frmProgress = nil then
    frmProgress := TfrmProgress.create(nil);

  if not frmProgress.visible then begin
    frmProgress.show;

  end;
  frmProgress.SetFocus;
  frmProgress.BeginProgress();


end;
procedure EndProgress();
begin
  frmProgress.EndProgress();

end;


procedure ShowProgress(pos,max: integer);
begin
  ShowProgress('--', 0, max, pos);
end;

procedure ShowProgress(sStatus: string; min,max,pos: integer);
begin
  if not assigned(frmProgress) then
    application.createform(TfrmProgress, frmProgress);

  if not frmProgress.visible then begin
    frmProgress.show;

  end;
  frmProgress.SetFocus;
  if sStatus <> '--' then
    frmProgress.lbl.caption := sStatus;
  frmProgress.currentpb.min := min;
  frmProgress.currentpb.max := max;
  frmProgress.currentpb.position := pos;
  frmProgress.lbl.refresh;
  frmProgress.refreshallbars;

  frmProgress.refresh;
  if (max = 1) and (pos=0) then begin
    frmProgress.currentpb.Style  := pbstMarquee;
  end else begin
    frmProgress.currentpb.style := pbstnormal;
  end;

end;

procedure HideProgress;
begin
  postmessage(frmProgress.Handle, WM_CLOSE, 0,0);
//  frmProgress.visible := false;
end;


procedure TfrmProgress.AdjustFormHeight;
var
  pb: TProgressbar;
  pan_height: nativeint;
  pbheight: nativeint;

begin
  if not showing then
    exit;
  if panBG.visible = false then
    pan_height := 0
  else
    pan_height := panBG.Height;

  if fpbs.count < 1 then begin
    self.clientheight := 150;
    exit;
  end;

  pbheight := (535-431) + ((5+(fpbs[0].height div 2)) * Fpbs.count);
  if panBG.Visible then begin
    if clientheight < (pbheight + 400) then begin
      clientheight := pbheight+pan_height;
    end else begin
      panBG.Height := clientheight - pbHeight;
    end;
  end else begin
    if clientheight > pbheight then
      clientheight := pbheight
    else if clientheight < pbheight then
      clientheight := pbheight;
  end;

//  clientheight := PAN_HEIGHT;


end;


procedure TfrmProgress.AddPB;
var
  pb: TProgressbar;
  pan_height: nativeint;
  pbheight: nativeint;
begin
  if panBG.visible = false then
    pan_height := 0
  else
    pan_height := panBG.Height;

  pb := TProgressbar.create(self);
  pb.parent := self;
  pb.width := fpbs[0].width;
  pb.height := fpbs[0].height div 2;
  pb.anchors := fpbs[0].anchors;
  pb.left := fpbs[0].left;
  pb.top := fpbs[0].top + ((5+(fpbs[0].height div 2)) * (Fpbs.count+1));

  AdjustFormHeight;



  Fpbs.add(pb);
  refresh;

end;

procedure TfrmProgress.beginProgress;
begin
  inc(FReferences);
  if FReferences > 1 then begin
    AddPb();
  end;
end;

constructor TfrmProgress.Create(aowner: TComponent);
begin
  inherited Create(aowner);
end;

function TfrmProgress.CurrentPB: TProgressBar;
begin
  result := Fpbs[Fpbs.count-1];
end;

procedure TfrmProgress.EnableCommandQueuetimer(var msg: TMessage);
begin
  TimerWatchQueueTimer(self);

end;

procedure TfrmProgress.EnableCommandtimer(var msg: TMessage);
begin
  TimerWatchCommandTimer(self);
end;

procedure TfrmProgress.EndProgress;
begin
  dec(FReferences);
  if FReferences < 0 then begin
    debug.log(self,'Progress form references less than ZERO!!!!', 'Error');
    FReferences := 0;
  end;

  if FReferences > 0 then
    RemovePB;
  if FReferences = 0 then
    Hide();



end;

procedure TfrmProgress.ExeConsole(ss: TConsolecaptureStatus; sStatus: string);
var
  sl: TStringlist;
begin
  sl := tStringlist.create;
  try
    case ss of
      ccStart: if not Showing then Show;
      ccProgress: begin
        if sStatus <> '' then begin
          sl.text := sStatus;
          lbl.caption := sl[sl.count-1];
        end;
      end;
    end;
  finally
    sl.free;
  end;
 end;

procedure TfrmProgress.frmBaseActivate(Sender: TObject);
begin
  inherited;
  AdjustFormHeight;
end;

procedure TfrmProgress.frmBaseClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caHide;
  timer1.Enabled := false;
  timer2.Enabled := false;
  TimerWatchCommand.enabled := false;
  TimerWatchQueue.enabled := false;

end;

procedure TfrmProgress.frmBaseCreate(Sender: TObject);
begin
  inherited FormCReate(sender);
  inherited;
  //stuff
  FReferences := 0;
  Fpbs := TList<TProgressBar>.create;

  pb.Height := 8;
  Fpbs.add(pb);

  FOriginalHeight := self.height;

end;

procedure TfrmProgress.frmBaseDblClick(Sender: TObject);
begin
  panBG.Visible := not panBG.Visible;

  if not panBG.Visible then
    height := height - panBG.Height
  else
    height := height + panBG.Height;

  if panBG.Visible then begin
    bg := TfrmBGThreadWatcher.Create(self);
    bg.Parent := panBG;
  end else begin
    bg.Free;
    bg := nil;
  end;

end;

procedure TfrmProgress.frmBaseDestroy(Sender: TObject);
begin
  timer1.Enabled := false;
  timer2.Enabled := false;
  inherited FormDestroy(sender);
  inherited;
  frmProgress := nil;
  FPbs.free;


end;

procedure TfrmProgress.frmBaseFirstActivation(Sender: TObject);
begin
  self.panBG.Visible := false;
  self.clientHeight := 72;
  self.RecenterWindow;
  timer1.Enabled := true;


end;

function TfrmProgress.GetCmdWatching: TCommand;
begin
  Lock;
  try
    result:= FcmdWatching;
  finally
    Unlock;
  end;
end;

procedure TfrmProgress.RefreshAllBars;
var
  t: integer;
begin
  for t:= 0 to componentcount-1 do begin
    if components[t] is TProgressBar then begin
//      TProgressBar(components[t]).invalidate;
      TProgressBar(components[t]).Repaint;
    end;
  end;

end;

procedure TfrmProgress.RemovePB;
begin
  FPbs[fpbs.count-1].free;
  fpbs.delete(fpbs.count-1);
end;

procedure TfrmProgress.SetCmdWatching(const Value: TCommand);
begin
  Lock;
  try
    FcmdWatching := value;
  finally
    unlock;
  end;
end;

procedure TfrmProgress.Timer1Timer(Sender: TObject);
begin

  if not panBG.Visible then
    height := height - panBG.Height
  else
    height := height + panBG.Height;

  timer1.Enabled := false;
end;

procedure TfrmProgress.Timer2Timer(Sender: TObject);
begin
  ADjustFormHeight;
end;

procedure TfrmProgress.TimerWatchCommandTimer(Sender: TObject);
begin

  TimerWatchCommand.enabled := false;
  Lock;
  try
    if cmdWatching = nil then
      exit;

    if cmdWatching.IsComplete  then begin
      cmdWatching := nil;
      ModalResult := mrOK;
    end else begin
      try
        //Debug.ConsoleLog('currenpb='+inttostr(currentpb.top)+','+inttostr(currentpb.Left));
        if not currentpb.Visible then
          currentpb.visible := true;

        currentpb.Min := 0;
        currentpb.max := cmdWatching.StepCount;
        currentpb.Position := cmdWatching.Step;
        lbl.caption := cmdWatching.Status;
        if cmdWatching.StepCount = 1 then begin
          currentpb.Style  := pbstMarquee;
        end else begin
          currentpb.style := pbstnormal;
        end;

      finally
        TimerWatchCommand.enabled := true;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TfrmProgress.TimerWatchQueueTimer(Sender: TObject);
begin

  TimerWatchQueue.enabled := false;
  Lock;
  try
    if currentpb = nil then exit;

    if FqueueWatching = nil then
      exit;

    if FqueueWatching.IsComplete then begin
      FQueueWatching := nil;
      ModalResult := mrOK;
    end else begin
      try
        currentpb.Min := 0;
        currentpb.max := FqueueWatching.commandcount;
        currentpb.Position := FqueueWatching.completecount;
        if (currentpb.max = 1) and (currentpb.position=0) then begin
          frmProgress.currentpb.Style  := pbstMarquee;
        end else begin
          frmProgress.currentpb.style := pbstnormal;
        end;

        lbl.caption := 'Waiting for commands...';

      finally
        TimerWatchQueue.enabled := true;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TfrmProgress.WatchCommandQueue(q: TCommandProcessor);
begin
  Lock;
  BeginProgress();
  try
    FqueueWatching := q;

    PostMessage(self.handle, WM_USER+2, 0,0);
    //TimerWatchCommand.Enabled := true;
    IF showing then hide;
    showmodal;

  finally
    FqueueWatching := nil;
    EndProgress();
    Unlock;
  end;
end;

procedure TfrmProgress.WatchSingleCommand(cmd: TCommand);
begin
{$IFDEF DISABLE_MODAL_COMMAND_WATCHING}
  WatchSingleCommand_nonModal(cmd);
  exit;
{$ENDIF}


  Lock;
  BeginProgress();
  try
    if cmd = nil then
      exit;

    if cmd.fireforget then
      raise Exception.create('You cannot watch a fireforget command');

    cmdWatching := cmd;
    if cmd.Fireforget then
      raise EClassException.create('You tried to watch a fire-forget command, this is BAD!');

    PostMessage(self.handle, WM_USER+1, 0,0);
    //TimerWatchCommand.Enabled := true;
    IF showing then hide;
    showmodal;

    cmd.waitfor;
  finally
    EndProgress();
    Unlock;
  end;

end;

procedure TfrmProgress.WatchSingleCommand_NonModal(cmd: TCommand);
begin
  if cmd.Fireforget then
    raise EClassException.create('You tried to watch a fire-forget command, this is BAD!');

  bringtofront;
  if not showing then show;
  while not cmd.IsComplete do begin
    pb.Min := 0;
    pb.max := cmd.StepCount;
    pb.Position := cmd.Step;
    lbl.caption := cmd.Status;
    refresh;
    sleep(300);
  end;



end;

procedure oinit;
begin
  frmProgress := nil;
{$IFNDEF DESIGN_TIME_PACKAGE}
//  showmessage('progress form is being created');
//  frmProgress := TfrmProgress.create(nil);
{$ENDIF}
end;

procedure ofinal;
begin
{$IFNDEF DESIGN_TIME_PACKAGE}
//  showmessage('progress form is being destroyed');
//  frmProgress.free;
{$ENDIF}


end;

initialization
  init.RegisterProcs('ProgressForm', oinit, ofinal);


finalization

end.
