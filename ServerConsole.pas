unit ServerConsole;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ComCtrls, StdCtrls, ScktComp, ExtCtrls, stringx, VclTee.TeeGDIPlus,
  Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.Imaging.jpeg, VCLTee.Series,
  VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, systemx, backgroundThreads, managedthread,
  requestManager, better_Sockets, webprocessor,
  newserversocketthread, advancedgraphics, easyimage, pngimage, formbase,
  consolelock, System.ImageList, colorblending;

const
  poop = CM_CURSORCHANGED;
type
  TfrmWebConsole = class(TfrmBase)
    txtActiveConnections: TLabel;
    lblHits: TLabel;
    ImageList1: TImageList;
    Timer1: TTimer;
    lblBackgroundThreads: TLabel;
    ImageList2: TImageList;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    brnKill: TButton;
    Splitter2: TSplitter;
    Label1: TLabel;
    lblDOs2: TLabel;
    lblRQs: TLabel;
    lblDOs: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Chart1: TChart;
    Series1: TPieSeries;
    lvObjects: TListView;
    TabSheet3: TTabSheet;
    Splitter3: TSplitter;
    Chart2: TChart;
    PieSeries1: TPieSeries;
    lvAgents: TListView;
    cbNoDetails: TCheckBox;
    TabSheet4: TTabSheet;
    lbRQs: TListBox;
    Button3: TButton;
    TabSheet5: TTabSheet;
    memMarsh: TMemo;
    Button4: TButton;
    TabSheet6: TTabSheet;
    memMem: TMemo;
    lblLWP: TLabel;
    TabSheet8: TTabSheet;
    lvBackground: TListView;
    Timer2: TTimer;
    Timer3: TTimer;
    lblThreadpool: TLabel;
    ActionList1: TActionList;
    CheckBox1: TCheckBox;
    cMemory: TChart;
    PieSeries2: TPieSeries;
    cObjects: TChart;
    Series2: TBarSeries;
    Panel2: TPanel;
    Panel3: TPanel;
    lvStatus: TListView;
    memGLOG: TMemo;
    Splitter1: TSplitter;
    Edit1: TEdit;
    Button5: TButton;
    Panel4: TPanel;
    imgRcv: TImage;
    imghttp: TImage;
    imgTrans: TImage;
    lblHTTP: TLabel;
    Label2: TLabel;
    popThreads: TPopupMenu;
    btnResetHits: TButton;
    lblAccepts: TLabel;
    Label4: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RefreshHitsMessage(var Msg: TMessage); message WM_USER+1;
    procedure MarshallDebug(var Msg: TMessage); message WM_USER+5;

   procedure RefreshHits; overload;
    procedure Button2Click(Sender: TObject);
    procedure ServerSocket1ThreadEnd(Sender: TObject;
      Thread: TServerClientThread);
    procedure Button3Click(Sender: TObject);
    procedure ForceItemCount(lv: TlistView; iQuantity: integer);
    procedure ForceSubItems(li: TListItem; iCount: integer);
    procedure FormDestroy(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure brnKillClick(Sender: TObject);
    procedure ServerSocket1Accept(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocket1ClientConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure tcpAccept(Sender: TObject; ClientSocket: TBetterCustomIpClient);
    procedure tcpListening(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure popThreadsPopup(Sender: TObject);
    procedure btnResetHitsClick(Sender: TObject);
  private
    { Private declarations }
    bRefreshing: boolean;
    bAccepting: boolean;
    iHitCount: integer;
    tmLAstRefresh: cardinal;
    FLWPConnections: integer;
    doPeak: integer;

    FBGThr: TManagedThread;
    procedure Listen(Sender: TObject);
    procedure StopListening(Sender: TObject);
    function GetLWPConnections: integer;
    procedure SetLWPConnection(const Value: integer);
  public
    { Public declarations }
    FMarshallDebug : TStringlist;
    procedure RefreshHits(sender: TObject);overload;
    procedure ThreadMenuClick(sender: TObject);
    procedure UnmarshallDebugger(sText: ansistring);
     property LWPConnections: integer read GetLWPConnections write SetLWPConnection;
    procedure IncLWP;
    procedure DecLWP;
    property LastSelectedThread: TManagedThread read FBGThr write FBGThr;
  end;

  TKillthread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  frmWebConsole: TfrmWebConsole;

implementation

uses {$IFNDEF DELPHI7}WebThreadManager,WebThread,{$ENDIF}
  DataObjectServices,
  MothershipWebServer, ThreadManager,  RequestInfo,Dataobject,
  debug,
  SimpleWinSock, HTTPClient, MotherShip_DM;

{$R *.DFM}

//------------------------------------------------------------------------------
procedure TfrmWebConsole.FormCreate(Sender: TObject);
var
  sKeyName : ansistring;
  h: THandle;
  t: integer;
begin
  inherited FormCReate(sender);
  EnableGlass;

  bRefreshing := false;
  //dmMothership.listen;


  debug.DebugLog.Filter := 'myspace';
  cObjects.SeriesList[0].Add(0.0, 'Current', clYellow);
  cObjects.SeriesList[0].Add(0.0, 'Peak', clRed);

  for t:= 0 to DOSVPool.count-1 do begin
//    dosvpool[t].Server.Debugger := UnmarshallDebugger;

  end;

  //DOOB.OnUpdate := self.RefreshHits;
//  DOSV := TDataObjectServices.create;
//  DOSV.InitializeServer('iPway');

  bAccepting := false;
  iHitCount := 0;

  h := CreateEvent(nil, true, false, 'Farty');
  CloseHandle(h);

  debug.debuglog.filter := edit1.Text;


end;

//------------------------------------------------------------------------------
procedure TfrmWebConsole.ThreadMenuClick(sender: TObject);
var
  idx: integer;
begin
  idx := popThreads.Items.IndexOf(sender as TMenuItem);

  if LastSElectedThread <> nil then begin
    LastSelectedThread.MenuAction(idx);
  end;

end;

procedure TfrmWebConsole.Timer1Timer(Sender: TObject);
var
  s: ansistring;
begin
  s := debug.debuglog.DrainLog;
  if s <> '' then
    memGLOG.lines.add(s);

//  if memGlog.Lines.count > 0 then begin
//    if memGlog.lines[memGlog.lines.count-1] = '' then
//      memGlog.lines.delete(memglog.lines.count-1);
//  end;

  if memGLOG.lines.count > 25000 then
    memGLOG.lines.clear;




{$IFDEF DO_MEM_CHART}
  MemChart(cMemory);
{$ENDIF}

  if not (WebServer.State = wssRunning) then
    exit;


  lblLWP.caption := 'Lightwieght:'+inttostr(LWPConnections);

//  memMem.lines.clear;
//  memMem.lines.add('Count:'+inttostr(GetManagerCount));
//  memMem.lines.add('Capacity:'+inttostr(GetManagerCapacity));
  try
    //if not Timer1.enabled then
    //  exit;
    //exit;
    Timer1.Enabled := false;
    try
      inherited;
      try
      RefreshHits;
    //    PostMessage(self.handle, WM_USER+1, 0,0);

    //    WebThreads.ExpireThreads;

      except
//        LogWebMessage('Exception at Timer Event');
//        LogWebMessage('Exception: '+exception(exceptobject).Message);
      end;
    finally
      Timer1.Enabled := true;
    end;
  except               
    on E:Exception do begin
      raise exception.create('In Timer:'+e.message);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmWebConsole.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Timer1.enabled := false;
end;
//------------------------------------------------------------------------------
procedure TfrmWebConsole.RefreshHits;
var
  t: integer;
  thr2: TManagedThread;
  thr3: TThread;
  item : TListItem;
  tmNow: cardinal;
  sTemp: ansistring;
  cTemp1, cTemp2: cardinal;
  iTemp1, iTEmp2: integer;
  rqTemp: TRequestInfo;
  v: variant;
  iRes: integer;
  fTemp: real;
  s: ansistring;
  c,l,i: integer;
  ACtive, Dead, PendingSuspend: integer;
begin
  if not assigned(WebServer) then
    exit;

  if bRefreshing then
    exit;

//  Pool := 0;
//  Dead := 0;

  ACtive := 0;
  Dead := 0;
  Pendingsuspend := 0;

//  dmMothership.tcp.ServerSocketThread.GEtSTats(ACtive,Dead,PendingSuspend);
  lblThreadPool.Caption := inttostr(ACtive)+'/'+inttostr(Dead)+'/'+inttostr(PendingSuspend);
//  lblThreadPool.Caption := lblThreadPool.caption + '  ' +inttostr(Pool)+'/'+inttostr(Dead);

  sTemp := Doob.LastAudit;
  if sTemp <> '' then
    lbRQs.Items.Add(sTemp);

  lbRQs.ItemIndex := lbRQs.Items.count-1;

  bRefreshing := true;
  try
//  exit;
  tmNow := GetTickCount;
  t:= doob.count div 10;
  if t<100 then t:= 100;
  if (tmNow>tmLAstRefresh) and ((tmNow-tmLastRefresh) < t) and (not self.cbNoDetails.checked) then begin
    application.processmessages;
    exit;
  end;

  tmLastRefresh := tmNow;

  try
    lblHits.Caption := inttostr(webserver.HitCount);
    lblAccepts.Caption := inttostr(webserver.HitAcceptCount);
    //lblHits.Caption := inttostr(iHitCount);
(*    WebThreadMan.LockRead;
    LockConsole;
    ForceItemCount(lvStatus, WebThreadMan.ThreadCount);
    lvStatus.Columns[0].Caption := 'Requests ('+inttostr(webThreadMan.Threadcount)+')';
    try
      iTemp1 := WebThreadMan.ThreadCount-1;
      for t:=  0 to iTEmp1 do begin
        thr := WebThreadMan.threads[t];
        item := lvStatus.Items[iTemp1-t];
        item.ImageIndex := 2;
        if item.caption <> thr.StateString then
          item.caption := thr.StateString;

      end;
    finally
      WebThreadMan.UnLockRead;
      UnLockConsole;
    end;*)
    RqMan.LockRead;
    LockConsole;
    ForceItemCount(lvStatus, RqMan.Count);
    lvStatus.Columns[0].Caption := 'Requests ('+inttostr(rqMan.count)+')';
    try
      iTemp1 := rqMan.Count-1;
      for t:= 0 to iTemp1 do begin
        if not socketsallowed then
          exit;
        item := lvStatus.Items[t];
        rqTemp := rqMan.RealRequests[t];

        item.ImageIndex := round(rqTemp.PercentComplete * 32)+8;
        if (rqTemp.PercentComplete = 1) then begin
          item.ImageIndex := (GEtTickCount div 100) mod 8;
        end;

        s := rqMan.Requests[t];
//        s := rqMan.RealRequests[t].Request.Document;
        if item.caption <> s then
          item.caption := inttostr(rqMan.RealRequests[t].ThreadID)+':'+s;

        if item.SubItems.Count < 1 then
          item.SubItems.Add(floattostrF(rqMan.Ages[t]/1000, ffFixed, 20, 3))
        else
          item.SubItems[0] := (floattostrF(rqMan.Ages[t]/1000, ffFixed, 20, 3))

      end;
    finally
      RqMan.UnLockRead;
      UnLockConsole;
    end;


    //ServerSocket1.
    //background threads
    BackgroundthreadMan.LockRead;
    try
      ForceItemCount(lvBackground, BackgroundthreadMan.count);
      lblBackgroundthreads.caption := WebServer.StateString;

      for t:= 0 to BackgroundthreadMan.count-1 do begin
        thr2 := BackgroundthreadMan.threads[t];
        item := lvBackground.Items[t];
        ForceSubItems(item, 7);
        if thr2.Spin then begin
          if not thr2.AutoSpin then
            item.ImageIndex := thr2.Step mod 8
          else
            item.imageindex := (item.imageindex+1) mod 8;
        end else begin
          if thr2.trylock(100) then
          try
            c := thr2.Step;
            l := thr2.StepCount;
            if l > 0 then
              i := round((c / l)*32)
            else
              i := 0;

            if i> 32 then i:=32;
            if i <0 then i:= 0;
            item.ImageIndex := 8+i;
          finally
            thr2.Unlock;
          end;


        end;
        if thr2.trylock(100) then
        try
          if (item.caption <> inttostr(thr2.threadid)) then
            item.caption := inttostr(thr2.threadid);

          if item.subitems[0] <> thr2.Name then
            item.subitems[0] := thr2.Name;

          //read last tick
          if item.SubItems[6] <> '' then
            cTemp1 := cardinal(strtoint(item.Subitems[6]))
          else
            cTemp1 := 0;

          //read last work value
          if item.SubItems[3] <> '' then
            iTemp1 := strtoint(item.Subitems[4])
          else
            iTemp1 := 0;


          //get current tick
          cTemp2 := GetTickCount;

          item.SubItems[6] := inttostr(integer(cTemp2));

          //calc tick difference
          cTEmp2 := cTemp2-cTemp1;

          //read current work value
          iTemp2 := thr2.Step;

          //calculate work throughput
          if cTemp2 >0 then
            fTemp := (iTemp2-iTemp1)/(cTemp2/1000)
          else
            fTemp := 0.0;

          //write out values
          item.SubItems[1] := thr2.Status;
          item.SubItems[2] := inttostr(thr2.Step);
          item.SubItems[3] := inttostr(thr2.StepCount);
          item.SubItems[4] := inttostr(thr2.Iterations);
          item.SubItems[5] := floattostrF(fTemp,ffFixed, 18,3);

        finally
          thr2.unlock;
        end;
      end;
    finally
      BackgroundthreadMan.UnLockRead;
    end;


    DOOB.LockRead;
    try
      lvObjects.Columns[0].Caption := 'Objects ('+inttostr(DOOB.count)+')';
      if doPeak < DOOB.count then
        doPeak := DOOB.count;

      cObjects.SeriesList[0].clear;
      cObjects.SeriesList[0].Add(doob.count, 'cur', clYellow);
      cObjects.SeriesList[0].Add(doPeak, 'peak', clRed);



      if not cbNoDetails.checked then begin
        ForceItemCount(lvObjects, DOOB.count);

        for t:= 0 to DOOB.count-1 do begin
          item := lvObjects.Items[t];

          item.Caption := doob.Objects[t];
          item.ImageIndex := 2;
        end;
      end;

      try
        //Chart1.SeriesList.Series[0].XValue[0] := 50;
        //Chart1.SeriesList.Series[0].XValue[1] := 50;
          lblRQs.Caption := inttostr(DOOB.TotalRequests);
          lblDOs.Caption := inttostr(DOOB.TotalObjects);      except
      On E: Exception do
        showmessage(e.message);
      end;

    finally
      DOOB.UnLockRead;
    end;



    chart1.SeriesList[0].Clear;
    //Chart1.SeriesList.Series[0].Add(doob.totalobjects, 'DOs', clRed);
    chart1.SeriesList[0].Add(WebServer.HitCount, 'Hits', clBlue);
    chart1.SeriesList[0].Add(doob.totalrequests, 'RQs', clSilver);

              
    self.refresh;

    RqMan.UserAgents.lock;
    try
      ForceItemCount(lvAgents, RqMan.UserAgents.count);

      chart2.SeriesList[0].Clear;
      for t:= 0 to RqMan.UserAgents.count -1 do begin
        lvAgents.Items[t].Caption := RqMan.UserAgents.Agents[t];
        if lvAgents.Items[t].SubItems.count < 1 then
          lvAgents.items[t].SubItems.add('__');;

        lvAgents.Items[t].SubItems[0] := inttostr(RqMan.UserAgents.AgentStats[t]);

      chart2.SeriesList[0].Add(RqMan.UserAgents.AgentStats[t], RqMan.UserAgents.Agents[t]);



      end;
    finally
      RqMan.UserAgents.Unlock;
    end;

    httpStatus.lock;
    try
      imgHTTP.Visible := httpstatus.InTransaction > 0;
      imgTrans.Visible := httpstatus.InTransmit > 0;
      imgRcv.Visible := httpstatus.InReceive > 0;
      lblHTTP.caption := 'D:'+inttostr(httpstatus.InReceive)+' T:'+inttostr(httpstatus.InTransaction)+' U:'+inttostr(httpstatus.InTransmit);
    finally
      httpStatus.unlock;
    end;

  except
    on E: Exception do
      lblBackGroundThreads.caption := E.message;
  end;
  finally
    bRefreshing := false;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmWebConsole.Button2Click(Sender: TObject);
begin
  inherited;
  dmMothership.StopListening;
  try
    WebServer.Stop;
  except
    on E:Exception do begin
      raise exception.create('In Stop method:'+e.message);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmWebConsole.ServerSocket1ThreadEnd(Sender: TObject;
  Thread: TServerClientThread);
begin
//  thread.freeonTerminate := true;
//  thread.terminate;

end;
//------------------------------------------------------------------------------
procedure TfrmWebConsole.Button3Click(Sender: TObject);
begin
  lbRQs.items.clear;
end;
//------------------------------------------------------------------------------
procedure TfrmWebConsole.ForceItemCount(lv: TlistView; iQuantity: integer);
var
  temp: TListItem;
begin
  LockConsole;
  try
    while lv.Items.count < iQuantity do begin
      temp := lv.items.add;
      temp.caption := '....';
    end;

    while lv.Items.count > iQuantity do begin
      lv.Items.Delete(lv.items.count-1);
    end;
  finally
    UnLockConsole;
  end;

end;

//------------------------------------------------------------------------------
procedure TfrmWebConsole.FormDestroy(Sender: TObject);
begin
  if WebServer<> nil then begin
    WebServer.Stop;
    FMarshallDebug.free;
    WebServer.free;
  end;
  WebServer := nil;
  inherited;

end;

//------------------------------------------------------------------------------
procedure TfrmWebConsole.btnGoClick(Sender: TObject);
begin

//TODO -cunimplemented: unimplemented block
end;
procedure TfrmWebConsole.btnResetHitsClick(Sender: TObject);
begin
  webserver.ClearHits;
end;

//------------------------------------------------------------------------------
procedure TfrmWebConsole.ForceSubItems(li: TListItem;
  iCount: integer);
begin
  while li.SubItems.Count < iCount do begin
    li.SubItems.Add('');
  end;

  while li.SubItems.Count > iCount do begin
    li.SubItems.Delete(li.SubItems.count-1);
  end;

end;
//------------------------------------------------------------------------------
procedure TfrmWebConsole.RefreshHitsMessage(var Msg: TMessage);
begin

  if bRefreshing then
    exit;

  RefreshHits;
end;
//------------------------------------------------------------------------------
procedure TfrmWebConsole.Listen(Sender: TObject);
begin
  {$IFDEF REPORTSERVER}
    ServerSocket1.Port := 3282;
  {$ENDIF}


  dmMotherShip.Listen;

//  tcp.Active := true;

  Timer1.Enabled := true;


//  caption := 'Listening on port :'+ tcp.LocalPort;



end;
//------------------------------------------------------------------------------
procedure TfrmWebConsole.StopListening(Sender: TObject);
var
  t: integer;
begin
  Timer1.Enabled := false;
  for t:= 0 to 25 do begin
    //application.processmessages;
    sleep(100);
    refreshHits;
  end;



  //ServerSocket1.free;
  //ServerSocket1.Socket.Lock;
(*  try
    tcp.Active := false;
  finally
    //ServerSocket1.Socket.UnLock;
  end;

  tcp.Active := false;*)

//  ServerSocket1.Close;
//  ServerSocket2.Close;


end;

//------------------------------------------------------------------------------
procedure TfrmWebConsole.Button1Click(Sender: TObject);
begin
  dmMothership.Listen;
  WebServer.Start;

end;

procedure TfrmWebConsole.brnKillClick(Sender: TObject);
begin
  if WebServer<> nil then begin
    WebServer.Stop;
    FMarshallDebug.free;
    WebServer.free;
  end;
  WebServer := nil;
  close;
end;
{ TKillthread }

procedure TKillthread.Execute;
begin
  LockConsole;
  try

    WebServer.free;
    WebServer := nil;
  finally
    UnlockConsole;
    terminate;
  end;
end;

procedure TfrmWebConsole.ServerSocket1Accept(Sender: TObject;
  Socket: TCustomWinSocket);
begin

  //sleep(500);
(*  Socket.Lock;
  try
    if not (WebServer.State = wssRunning) then
       Socket.close;
  finally
    Socket.Unlock;
  end;*)
end;

procedure TfrmWebConsole.ServerSocket1ClientConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
//  if not (WebServer.State = wssRunning) then
//    Socket.Close;

end;

procedure TfrmWebConsole.RefreshHits(sender: TObject);
begin
  PostMessage(self.handle, WM_USER+1, 0,0);
end;

procedure TfrmWebConsole.UnmarshallDebugger(sText: ansistring);
var
  item: TListItem;
begin
  LockConsole;
  try

    FMarshallDebug.add(MakeThreadSafe(sText));
    //postmessage(self.handle, WM_USER+5, 0,0);
    //memMarsh.lines.add(sText);

  finally
    UnlockConsole;
  end;
end;

procedure TfrmWebConsole.Button4Click(Sender: TObject);
begin
  memMarsh.clear;
  //postmessage(self.handle, WM_USER+5, 0,0);
end;

procedure TfrmWebConsole.MarshallDebug(var Msg: TMessage);
begin
  LockConsole;
  try
    if FMArshallDebug.text <> '' then begin
//      windows.beep(100,10);
      memMarsh.lines.add(FMArshallDebug.text);
      FMarshallDebug.clear;
    end;
  finally
    UnlockConsole;
  end;

end;

procedure TfrmWebConsole.popThreadsPopup(Sender: TObject);
var
  man: TThreadManager;
  t: integer;
  thr: TManagedThread;
  itm: TMenuItem;
begin
  man := backgroundthreadman;
  man.Lock;
  try
    popThreads.Items.Clear;
    LAstSElectedThread := nil;
    t := lvBackGround.ItemIndex;
    if t < 0 then exit;
    if t >= man.count then exit;

    thr := man.Threads[t];
    LastSelectedThread := thr;
    if thr.trylock(0) then
    try
      for t:= 0 to thr.MenuCount-1 do begin
        itm := TMenuItem.create(self);
        itm.Caption := thr.menu[t];
        popThreads.Items.Add(itm);
        itm.OnClick := self.ThreadMenuClick;
      end;
    finally
      thr.unlock;
    end;






  finally
    man.unlock;
  end;

end;

procedure TfrmWebConsole.Button5Click(Sender: TObject);
var
  t: integer;
begin
  debug.debuglog.filter := edit1.Text;
end;

procedure TfrmWebConsole.tcpAccept(Sender: TObject;
  ClientSocket: TBetterCustomIpClient);
var
  proc: TWebProcessor;
begin
  proc := TWebProcessor.create;
  try
    proc.ClientSocketProxy := clientsocket;
//    proc.rqInfo := TRequestInfo.create;
    try
      proc.Process;
    except
    end;

  finally
    proc.rqInfo.Free;
    proc.free;
    ClientSocket.Close;
  end;

end;

procedure TfrmWebConsole.tcpListening(Sender: TObject);
begin
  self.color := clNavy;
end;

function TfrmWebConsole.GetLWPConnections:   integer;
begin
  LockConsole;
  try
     result := FLWPConnections;
  finally
    UnlockConsole;
  end;
end;

procedure TfrmWebConsole.SetLWPConnection(const Value: integer);
begin
  LockConsole;
  try
    FLWPconnections := value;
  finally
    UnlockConsole;
  end;

end;

procedure TfrmWebConsole.DecLWP;
begin
  LockConsole;
  try
    dec(FLWPConnections);
  finally
    UnlockConsole;
  end;

end;

procedure TfrmWebConsole.IncLWP;
begin
  LockConsole;
  try
    inc(FLWPConnections);
  finally
    UnlockConsole;
  end;

end;

procedure TfrmWebConsole.Timer2Timer(Sender: TObject);
begin
  TIMER2.Enabled := FALSE;

  button1click(nil);
end;

procedure TfrmWebConsole.Timer3Timer(Sender: TObject);
var
  t: integer;
begin
  if not (WebServer.State = wssRunning) then
    exit;

  rqMan.lockwrite;
  try
    for t:= 0 to rqMan.Count-1 do begin
      rqMan.sync(t);


    end;
  finally
    rqMan.UnlockWrite;
  end;
end;


procedure TfrmWebConsole.FormPaint(Sender: TObject);
var
  t: integer;
  c1,c2,c3: TColor;
begin
  exit;
  for t := 0 to clientwidth-1 do begin
    c1 := ColorBlend(clWhite, clAqua, t/clientwidth);
    canvas.Brush.color := c1;
    canvas.pen.Color := c1;
    canvas.FillRect(Rect(t,0,t+1, clientheight));
  end;
end;

end.
