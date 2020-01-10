unit ServerConsole;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ComCtrls, StdCtrls, ScktComp, ExtCtrls, stringx, VclTee.TeeGDIPlus,
  Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.Imaging.jpeg, VCLTee.Series,
  VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, systemx, backgroundThreads, managedthread,
  requestManager, better_Sockets, webprocessor,
  newserversocketthread, advancedgraphics, easyimage, pngimage, formbase,
  consolelock, System.ImageList, colorblending, FrameHostPanel, FormBGThreadWatcher;

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
    cbNoDetails: TCheckBox;
    lblLWP: TLabel;
    Timer2: TTimer;
    Timer3: TTimer;
    lblThreadpool: TLabel;
    ActionList1: TActionList;
    CheckBox1: TCheckBox;
    Panel2: TPanel;
    Edit1: TEdit;
    Button5: TButton;
    Label2: TLabel;
    btnResetHits: TButton;
    lblAccepts: TLabel;
    Label4: TLabel;
    Panel3: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    lvObjects: TListView;
    TabSheet2: TTabSheet;
    Chart1: TChart;
    Series1: TPieSeries;
    TabSheet3: TTabSheet;
    Splitter3: TSplitter;
    Chart2: TChart;
    PieSeries1: TPieSeries;
    lvAgents: TListView;
    TabSheet4: TTabSheet;
    lbRQs: TListBox;
    Button3: TButton;
    TabSheet5: TTabSheet;
    memMarsh: TMemo;
    Button4: TButton;
    TabSheet6: TTabSheet;
    memMem: TMemo;
    TabSheet8: TTabSheet;
    FrameHostBackground: TFrameHostPanel;
    Splitter1: TSplitter;
    Panel5: TPanel;
    lvStatus: TListView;
    cObjects: TChart;
    Series2: TBarSeries;

    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RefreshHitsMessage(var Msg: TMessage); message WM_USER+1;
    procedure MarshallDebug(var Msg: TMessage); message WM_USER+5;

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
    procedure UnmarshallDebugger(sText: ansistring);
    procedure FirstActivation; override;
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


procedure TfrmWebConsole.Timer1Timer(Sender: TObject);
var
  s: ansistring;
begin



{$IFDEF DO_MEM_CHART}
  MemChart(cMemory);
{$ENDIF}

  if not (WebServer.State = wssRunning) then
    exit;

  try
    //if not Timer1.enabled then
    //  exit;
    //exit;
    Timer1.Enabled := false;
    try
      inherited;
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
procedure TfrmWebConsole.FirstActivation;
begin
  inherited;
  var debugframe := TfrmBGThreadWatcher.Create(self);
  debugframe.parent := FrameHostBackground;
  debugframe.align := alClient;

end;

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
    FMarshallDebug.free;
  end;
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
  //
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

    FMarshallDebug.add(MakeThreadSafe(string(sText)));
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


procedure TfrmWebConsole.Button5Click(Sender: TObject);
var
  t: integer;
begin
  debug.debuglog.filter := edit1.Text;
end;

procedure TfrmWebConsole.tcpAccept(Sender: TObject;
  ClientSocket: TBetterCustomIpClient);
var
  proc: Tcmd_ProcessWebRequests;
begin
  proc := Tcmd_ProcessWebRequests.create;
  try
    proc.Start;
    proc.waitfor;
  finally
    proc.free;
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
