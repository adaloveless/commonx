unit FormSplash;
{$DEFINE SYNC_SPLASH}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, tickcount,
  Dialogs, easyimage, systemx, glasscontrols, pngimage, FormBase, ComCtrls, commandprocessor,
  graphicwincontrol, numbers, Vcl.ExtCtrls, fastbitmap, typex, managedthread;
const
  WM_HIDEPROG = WM_USER+1;
type
  TfrmSplash = class(TfrmBase)
    pb: TProgressBar;
    gi: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure frmBaseClose(Sender: TObject; var Action: TCloseAction);
  private
    FImageFile: ansistring;
//    gi: TGlassImage;
    pngi: TPNgImage;
    fbm: TFAstBitmap;
    frame: ni;
    FProcessMessagesOnWatch: boolean;
    FWatch: boolean;
    procedure FocusIn_BAD;
    procedure FocusOut_bad;
    procedure SetWatch(const Value: boolean);
    { Private declarations }
  public
    { Public declarations }
    property ImageFile: ansistring read FImageFile write FImageFile;

    procedure FocusIn;
    procedure FocusOut;

    procedure WatchCommandQueue(queue: TCommandProcessor; sCaption: ansistring = 'Please Wait...');

    procedure ShowProgress;
    procedure HideProgress;
    procedure WaitProc(sender: TObject);
    procedure MSG_HideProg(var msg: TMessage);message WM_HIDEPROG;
    property ProcessMessagesOnWatch: boolean read FProcessMessagesOnWatch write FProcessMessagesOnWatch;
    property Watch: boolean read FWatch write SetWatch;
  end;

  TSplashThread = class(TManagedThread)
  protected
    procedure DoExecute; override;
  public

  end;


procedure ShowSplash;
procedure HideSplash;
procedure ShowSplash_Sync;
procedure HideSplash_Sync;
procedure WatchCommands;



implementation

uses BackGroundCommandProcessor;

var
  Application: TApplication;
  splashThread: TSplashThread;
  frmSplash: TfrmSplash = nil;



procedure ShowSplash;
begin
{$IFDEF SYNC_SPLASH}
  showSplash_sync;
{$ELSE}
  splashThread := TPM.Needthread<TSplashThread>(nil);
  splashThread.start;
{$ENDIF}
end;
procedure HideSplash;
begin
{$IFDEF SYNC_SPLASH}
  hidesplash_sync;
{$ELSE}
{$ENDIF}


end;




procedure WatchCommands;
begin
  frmSplash.WatchCommandQueue(CommandProcessor.BGCmd)
end;

procedure ShowSplash_Sync;
begin
{$IFNDEF SYNC_SPLASH}
  formSplash.Application := Tapplication.create(nil);
  formSplash.application.name := 'splashapp';
  formSplash.Application.MainFormOnTaskbar := false;
  formSplash.application.initialize;
  formSplash.Application.MainFormOnTaskbar := false;


{$ENDIF}
//  formSplash.Application.CreateForm(TfrmSplash, frmSplash);
  frmSplash := TfrmSplash.create(nil);
  frmSplash.Focusin;
  frmSplash.Show;
  frmSplash.BringToFront;
  frmSplash.refresh;

{$IFNDEF SYNC_SPLASH}
      FormSplash.Application.ProcessMessages;
{$ENDIF}


end;

procedure HideSplash_Sync;
begin
  frmSplash.FocusOut;
  frmsplash.free;
  frmSplash := nil;
{$IFNDEF SYNC_SPLASH}
  FormSplash.Application.Terminate;
  FormSplash.Application.free;
  FormSplash.Application := nil;
{$ENDIF}
end;



{$R *.dfm}

procedure TfrmSplash.FocusIn;
var
  fil: string;
  tm,tmtm: ticker;
begin
  self.DoubleBuffered := true;
  frame := 0;
  while true do begin
    fil := DLLPath+'graphics\splash'+inttostr(frame)+'.png';
    if not fileexists(fil) then
      break;
    inc(frame);
  end;

  dec(frame);
  while true do begin
    tm := getticker;
    fil := DLLPath+'graphics\splash'+inttostr(frame)+'.png';
    if not fileexists(fil) then
      break;

    gi.Picture.LoadFromFile(fil);
    if not showing then
      show;
    gi.refresh;
    {$IFNDEF SYNC_SPLASH}FormSplash.Application.ProcessMessages;{$ENDIF}

    dec(frame);
    tmtm := gettimesince(tm);
    if tmtm < 100 then
      sleep(100-tmtm);
  end;

  dec(frame);




end;

procedure TfrmSplash.FocusIn_BAD;
var
  r: real;
  tmNow, tmStart, tmSince: cardinal;
  tmp: TFastBitmap;
const
  TIME_TO_COMPLETE = 40000;
begin
  if visible then exit;

  r := 0.01;
  tmStart := GEtTickCount;
  self.DoubleBuffered := true;
  fbm.FromPNG(pngi);
  while (r < 1.0) do begin
    tmNow := GetTickCount;
    tmSince := GEtTimeSince(tmNow, tmStart);
    if tmSince = 0 then
      continue;

    r := tmSince / TIME_TO_COMPLETE;
    r := r* r;
    if r = 0.0 then
      continue;
    if r > 1.0 then
      r := 1.0;

    tmp := TFastBitmap.create;
    try
      tmp.FromFastBitmap(fbm);
      ResizeImage(tmp, greaterof(1,round(tmp.Width * r)), greaterof(1,round(tmp.Height * r)));
      ResizeImage(tmp, pngi.Width, pngi.Height);
      if (tmp.width <> pngi.width)
      or (tmp.height <> pngi.height) then
        raise Exception.create('wtf!');
      //TMP.SaveToFile('d:\temp.png');
      tmp.AssignToPicture(gi.picture);
      gi.stretch := false;
      gi.proportional := false;
      if not visible then
        Show;
      gi.Refresh;

    finally
      tmp.free;
    end;






  end;


end;

procedure TfrmSplash.FocusOut;
var
  fil: string;
  tm,tmtm: ticker;
begin
  self.DoubleBuffered := true;
  frame := 0;
  while true do begin
    tm := getticker;
    fil := DLLPath+'graphics\splash'+inttostr(frame)+'.png';
    if not fileexists(fil) then
      break;

    gi.Picture.LoadFromFile(fil);
    if not showing then
      show;
    gi.refresh;

    inc(frame);
    tmtm := gettimesince(tm);
    if tmtm < 100 then
      sleep(100-tmtm);
  end;
end;

procedure TfrmSplash.FocusOut_bad;
var
  r: real;
  tmNow, tmStart, tmSince: cardinal;
const
  TIME_TO_COMPLETE = 1000;
begin
  if not visible then exit;
  visible := false;
  exit;

  self.pb.position := pb.max;

  Refresh;
  r := 0.01;
  tmStart := GEtTickCount;
  self.DoubleBuffered := true;
  while (r > 0.0) do begin
    tmNow := GetTickCount;
    tmSince := GEtTimeSince(tmNow, tmStart);
    if tmSince = 0 then
      continue;

    r := tmSince / TIME_TO_COMPLETE;

    if r > 1.0 then
      r := 1.0;

    r := 1.0-r;
    r := r*r;
    if r > 1.0 then
      r := 1.0;





    gi.picture.assign(pngi);
    if FocusPNG(TPNGImage(gi.picture.graphic), r,r) then begin
      if not visible then
        Show;
      gi.Refresh;
    end;
  end;
  Visible := false;
  Refresh;    

end;

procedure TfrmSplash.FormCreate(Sender: TObject);
var
  sPath: ansistring;
  sFile: ansistring;
  pm: TMonitor;
begin
  inherited;
  //full res image
  try
    //early component setup
//    gi := TGlassImage.create(self);
//    gi.parent := self;
    fbm := TFastBitmap.create;

    sPath := dllpath;
    sFile := sPath+'graphics\splash.png';
    pngi := TPNGImage.create;
    try
      //pngi.assign(gi.picture.bitmap);
      pngi.loadfromfile(sFile);
      gi.picture.assign(pngi);

      self.ClientHeight := pngi.height;
      self.clientwidth := pngi.width;

      gi.Align := alClient;
      gi.Align := alNone;
      gi.Width := clientwidth;
      gi.height := clientheight;
      gi.Top := 0;
      gi.Left := 0;
      gi.Transparent := false;

    except
      exit;
    end;

  finally
    RecenterWindow;
  end;

end;

procedure TfrmSplash.FormDestroy(Sender: TObject);
begin
  pngi.free;
  fbm.Free;
  inherited;
end;

procedure TfrmSplash.frmBaseClose(Sender: TObject; var Action: TCloseAction);
begin
//  Action := caFree;
//  Application.Free;
//  application := nil;
end;

procedure TfrmSplash.HideProgress;
begin
  pb.visible := false;
  ClientHeight := gi.Height;
end;

procedure TfrmSplash.MSG_HideProg(var msg: TMessage);
begin
  HideProgress;
end;

procedure TfrmSplash.SetWatch(const Value: boolean);
begin
  Lock;
  try
    FWatch := Value;
  finally
    Unlock;
  end;
end;

procedure TfrmSplash.ShowProgress;
begin
  if pb.visible then
    exit;
  pb.Position := 0;
  ClientHeight := ClientHeight+pb.height;
  pb.Visible := true;
  refresh;

end;

procedure TfrmSplash.WaitProc(sender: TObject);
var
  bWasVisible: boolean;
begin
  bWasVisible := self.Visible;

  Visible := true;
  try
    self.RecenterWindow;
    WAtchCommandQueue(BGCmd);
  finally
    Visible := bWasVisible;
  end;
end;

procedure TfrmSplash.WatchCommandQueue(queue: TCommandProcessor;
  sCaption: ansistring);
var
  b: boolean;
  r: real;
  c: TCommand;
  iTemp: integer;
  t: integer;
begin

  Caption := sCaption;
  b := visible;
  if not visible then FocusIn;

  r := 0;
  ShowProgress;
  try
    while not queue.IsComplete do begin
      queue.lock;
      try
        BringToFront;
        pb.Max := 1000;

        iTemp := round(queue.PercentComplete*1000);

        if iTemp > pb.max then
          iTemp := pb.max;

        pb.Position := iTemp;


      finally
        queue.Unlock;
      end;
      refresh;
      if ProcessMessagesOnWatch then begin
        for t:= 0 to 100 do begin
          application.processmessages;
          sleep(1);
        end;

      end else begin
        refresh;
        sleep(100);
      end;

    end;
    pb.position := pb.Max;
      if ProcessMessagesOnWatch then begin
        for t:= 0 to 100 do begin
          application.processmessages;
          sleep(1);
        end;

      end else begin
        refresh;
      end;

  finally
//    PostMessage(self.Handle, WM_HIDEPROG, 0,0);
    HideProgress;
  end;

  if not b then
    FocusOut;

end;

{ TSplashThread }

procedure TSplashThread.DoExecute;
begin
  inherited;
  ShowSplash_Sync;
  Application.Run;
  HideSplash_Sync;


end;

end.
