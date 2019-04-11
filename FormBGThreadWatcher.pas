unit FormBGThreadWatcher;
{$I DelphiDefs.inc}
interface

uses
  tickcount, betterobject,guihelpers,colorconversion, commandprocessor, advancedgraphics, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, BackgroundThreads, ManagedThread, ExtCtrls, betterobjectregistry, systemx, numbers,
  Vcl.StdCtrls,easyimage, typex, ringstats,
{$DEFINE TRYLOCKRUDP}
{$IFDEF RUDP2}
  RUDPMonitor2,
{$ELSE}
  RUDPMonitor,
{$EndIF}
  colorblending, fastbitmap, helpers.chart,
{$IFDEF DO_MEM_CHART}
  brainscanultra,
{$ENDIF}
  stringx, VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs,
  VCLTee.Chart, Vcl.Menus, System.ImageList, simplereliableudp;

type
  TThreadState = record
    pooled: boolean;
    expireprog: single;
  end;

  PThreadState = ^TThreadState;

  TCommandBitmapThread = class(TFireWaitThread)
  private
    procedure ResizeDB;
    procedure SetHeight(const Value: ni);
    procedure SetWidth(const Value: ni);
  public
    cprr: IHolder<TCommandProcessorDebugState>;
    volState: TArray<TProgressEx>;
    db: TFastBitmap;
    Fwidth,Fheight: ni;
    function GetIdealSquareSize(): integer;
    procedure DrawCommandBitmap();
    procedure DoExecute;override;
    property Width: ni read FWidth write SetWidth;
    property height: ni read FHeight write SetHeight;
  end;

  TfrmBGThreadWatcher = class(TFrame)
    ImageList2: TImageList;
    ImageList1: TImageList;
    Timer1: TTimer;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    lvBackground: TListView;
    Splitter1: TSplitter;
    pnlBitmap: TPanel;
    lvClasses: TListView;
    Timer2: TTimer;
    Panel1: TPanel;
    rgUpdateSpeed: TRadioGroup;
    TabSheet3: TTabSheet;
    memDisk: TMemo;
    TabSheet4: TTabSheet;
    lvMemory: TListView;
    TabSheet5: TTabSheet;
    Chart1: TChart;
    Series1: TPieSeries;
    popThread: TPopupMenu;
    TabSheet6: TTabSheet;
    memRingStats: TMemo;
    Timer3: TTimer;
    TabSheet7: TTabSheet;
    lvRUDP: TListView;
    TabSheet8: TTabSheet;
    memLog: TMemo;
    StaticText1: TStaticText;
    Splitter2: TSplitter;
    cRUDP: TChart;
    Series2: TAreaSeries;
    Series3: TAreaSeries;
    Series4: TLineSeries;
    Series5: TLineSeries;
    procedure Timer1Timer(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure rgUpdateSpeedClick(Sender: TObject);
    procedure lvBackgroundChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure Timer3Timer(Sender: TObject);
    procedure lvBackgroundCustomDraw(Sender: TCustomListView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure lvBackgroundCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure lvBackgroundDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure lvBackgroundAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
  private
    { Private declarations }
    tm1,tm2: cardinal;
    lastNanoTick: int64;
    lastUDPCheck: ticker;
    threadstates: array of TThreadState;
    thrDraw: TCommandBitmapThread;
    dbr: TFastBackBufferedControl;
    rudp_reads, rudp_writes, rudp_reads_oh, rudp_writes_oh: array[0..59] of int64;
    rudp_chartidx: ni;
    procedure SyncPopup;
    procedure POpupMenuClick(sender: TObject);
    function REfreshHits: boolean;
    procedure UpdateInfos(infos: TArray<TThreadInfo>);
    procedure ForceItemCount(lv: TlistView; iQuantity: integer);
    procedure ForceSubItems(li: TListItem; iCount: integer);
    procedure DrawDiskUsage(cp: TCommandProcessor);
    procedure RefreshClasses;
    function GetSelectedThread: TManagedThread;
  public
    destructor Destroy; override;

    { Public declarations }

  end;


function GetCommandIconColor(c, bg: cardinal): cardinal;



implementation

{$R *.dfm}

uses debug;


procedure TfrmBGThreadWatcher.RefreshClasses;
var
  sl: Tlist;
  t: nativeint;
  sOut: string;
  refOuter: TClass;
  sTag: string;
begin
  sl := Tlist.Create;
  try
    bor.Lock;
    try
      for t:= 0 to bor.Count-1 do begin
        sl.Add(bor.Objects[t]);
      end;
    finally
      bor.Unlock;
    end;

    SyncListView(lvClasses, sl.Count, 2);
    for t:= 0 to sl.Count-1 do begin
      lvClasses.items[t].caption := TBORENTRY(sl[t]).ClassRef.ClassName;
      lvClasses.items[t].SubItems[0] := TBORENTRY(sl[t]).Count.ToString;

      sTag := TBORENTRY(sl[t]).outerclassref;
      lvClasses.items[t].SubItems[1] := sTag;



    end;

  finally
    sl.Free;
  end;
end;

function TfrmBGThreadWatcher.RefreshHits: boolean;
var
  infos: Tarray<TThreadInfo>;
begin
  result := false;
    //ServerSocket1.
    //background threads
    if BackgroundthreadMan.TryLockRead then
    try
      infos := BackgroundThreadMan.GetInfoList;
      result := true;
    finally
      BackgroundThreadMan.UnlockRead;
    end;

    UpdateInfos(infos);



end;


procedure TfrmBGThreadWatcher.rgUpdateSpeedClick(Sender: TObject);
begin
  case rgUpdateSpeed.itemindex of
    0: timer1.Interval := 10;
    1: timer1.Interval := 1000;
    2: timer1.Interval := 333;
    3: timer1.Interval := 100;
    4: timer1.Interval := 10;
  end;
end;

procedure TfrmBGThreadWatcher.SyncPopup;
var
  idx: ni;
  thr: TManagedThread;
  itm: TMEnuItem;
begin
  backgroundthreadman.lock;
  try
    popThread.items.Clear;
    thr := GetSelectedThread;
    for idx := popthread.Items.count-1 downto 0 do begin
      itm := popThread.Items[idx];
      popThread.Items.Delete(idx);
      itm.free;
    end;
    thr.BuildMenu;
    for idx := 0 to thr.MenuCount-1 do begin
      itm := TMenuitem.create(self);
      itm.OnClick := self.POpupMenuClick;
      itm.Caption := thr.Menu[idx];
      popThread.Items.Add(itm);
    end;

  finally
    backgroundthreadman.unlock;
  end;

end;

procedure TfrmBGThreadWatcher.Timer1Timer(Sender: TObject);
var
  thr: TCommandProcessorMainthread;
  s: string;
  slTemp: TStringlist;
  t: ni;
begin
{$IFDEF DO_MEM_CHART}
  brainscanultra.memchart(chart1);
{$ENDIF}

  s := Debug.DebugLog.DrainLog;
  memLog.Lines.BeginUpdate;
  try
    if s <> '' then begin
      slTemp := StringtoStringList(s);
      try
        if slTemp.Count > 80 then begin
          memLog.Lines.Clear;
          for t := slTemp.Count-80 to slTemp.Count-1 do
            memLog.lines.add(slTemp[t]);

        end else
          memLog.lines.add(s);

      finally
        slTemp.Free;
      end;

      while memLog.lines.count > 80 do begin
        for t := 0 to 79 do
          memlog.Lines[t] := memLog.Lines[(memlog.Lines.Count-1)-t];

        while memLog.Lines.Count>80 do
          memLog.lines.delete(memLog.Lines.Count-1);
      end;
    end;

  thr := nil;
    if refreshhits then begin
      //brainscanwindowsgui.MemList(lvMemory, lvBackground);

      if BackGroundthreadMan.TryLockread then
      try
        tm1 := GetTicker;
        if lvBackGround.itemIndex > -1 then begin
          if backgroundthreadman.count > lvBackGround.itemindex then begin
            if backgroundthreadman[lvBackGround.itemindex] is TCommandProcessorMainThread then begin
              thr := backgroundthreadman[lvBackGround.itemindex] as TCommandProcessorMainthread;
              if thr.tryLock then
              try
                if thrDraw = nil then begin
                  thrDraw := TPM.Needthread<TCommandBitMapThread>(nil);
                  thrDraw.HasWork := false;
                  thrDraw.Start;
                end;

                if (thrDraw <> nil) and thrDraw.started and thrDraw.IsIdle then begin
                  var pic: TPicture;
                  pic := dbr.picture;
                  thrDraw.db.AssignToPicture(pic);
                  if assigned(thr.cp) then begin
                    thrDraw.cprr := thr.CP.volState;
                    thrDraw.height := dbr.height;
                    thrDraw.width := dbr.width;
                    thrDraw.Fire;
                    DrawDiskUsage(thr.CP);
                  end;

                end;

              finally
                thr.Unlock;
              end;

            end;
          end;
        end;

        tm2 := GetTicker;
        if rgUpdateSpeed.ItemIndex = 0 then begin
          timer1.Interval := lesserof(greaterof(gettimesince(tm2,tm1)*8, 75),4000);
        end;
      finally
        BackGroundThreadMan.unlockread;
      end;
    end;
  finally
    memLog.Lines.EndUpdate;
  end;

end;

procedure TfrmBGThreadWatcher.Timer2Timer(Sender: TObject);
begin
  RefreshClasses;
end;

procedure TfrmBGThreadWatcher.Timer3Timer(Sender: TObject);
var
  ep: TReliableUDPEndpoint;
  t: ni;
  li: TListItem;
  iTempCAlc, iTEmp, iOldTemp: int64;
  tmDif, tmOld, tmNow: ticker;
  iTotalWrite, iTotalRead, iTotalReadOH, iTotalWriteOH: int64;
  s: string;
begin
  //exit;
  //RING STATS DO NOT WORK ON MuLTI-MODULE architectures!!!!
  //memRingStats.lines.text := rsMon.GetDebugString;
//  memo1.text := RUDPMon.GetDebugInfoString;

  SyncListView(lvrudp, RUDPMon.Count, 24);//hint
  //rsACkTime.periodicAverage
  //FlexREtransTime
  //Ping
  //Retrans
  //BadOrder



  RUDPMon.Lock;
  try
    SyncListView(lvrudp, RUDPMon.Count, 24);

    iTotalWrite := 0;
    iTotalRead := 0;
    iTotalReadOH := 0;
    iTotalWriteOH := 0;
    for t:= 0 to RUDPMon.count-1 do begin
      ep := RUDPMon.items[t];
{$IFDEF TRYLOCKRUDP}
      if ep.TryLock then
{$ELSE}
      ep.Lock;
{$ENDIF}
      try
        li := lvrudp.items[t];
        s := li.subitems[19];
        if (s <> '') and (s[STRZ]<>'-') then
          tmOld := strtoint(s)
        else
          tmOld := 0;
        tmNow := GetTicker;
        tmDif := greaterof(GetTimeSince(tmOld),1);

        li.Caption := ep.Remotehost+':'+inttostr(ep.RemotePort);
        li.SubItems[0] := ep.LocalDebugInfo.ToString;

        //TXRate
        {$IFDEF RUDP2}
        iTEmp := ep.outputchannel.totalTXData;
        {$ELSE}
        iTEmp := ep.totalTXData;
        {$ENdIF}
        iOldTemp := strtointex(li.SubItems[3]);
        iTempCalc := round(((iTemp-iOldTemp) / (tmDif/1000)));
        inc(iTotalWrite, greaterof(0,iTempCalc));
        li.SubItems[1] := friendlysizename(iTEmpCalc)+'/sec';
        li.SubItems[3] := inttostr(iTemp);

        //RXRate
        iTEmp := ep.TotalRXData;
        iOldTemp := strtointex(li.SubItems[4]);
        iTempCalc := round(((iTemp-iOldTemp) / (tmDif/1000)));
        inc(iTotalRead, greaterof(0,iTempCalc));
        li.SubItems[2] := friendlysizename(iTEmpCalc)+'/sec';
        li.SubItems[4] := inttostr(iTemp);

        //RXOverhead
        iTEmp := ep.TotalRX;
        iOldTemp := strtointex(li.SubItems[21]);
        iTempCalc := round(((iTemp-iOldTemp) / (tmDif/1000)));
        inc(iTotalReadOH, greaterof(0,iTempCalc));
        li.SubItems[20] := friendlysizename(iTEmpCalc)+'/sec';
        li.SubItems[21] := inttostr(iTemp);
        //TXOverhead
        {$IFDEF RUDP2}
        iTEmp := ep.outputchannel.TotalTX;
        {$ELSE}
        iTEmp := ep.TotalTX;
        {$ENdIF}
        iOldTemp := strtointex(li.SubItems[23]);
        iTempCalc := round(((iTemp-iOldTemp) / (tmDif/1000)));
        inc(iTotalWriteOH, greaterof(0,iTempCalc));
        li.SubItems[22] := friendlysizename(iTEmpCalc)+'/sec';
        li.SubItems[23] := inttostr(iTemp);
//        Debug.log('######TXO:'+li.subitems[22]);



        {$IFDEF RUDP2}
        li.SubItems[5] := inttostr(ep.outputchannel.FlexMTU);
        {$ELSE}
        li.SubItems[5] := inttostr(ep.FlexMTU);
        {$ENDIF}
        {$IFDEF RUDP2}
        li.SubItems[6] := inttostr(ep.outputchannel.WAlkingAverage);
        {$ELSE}
        li.SubItems[6] := inttostr(ep.WAlkingAverage);
        {$ENDIF}
        {$IFDEF RUDP2}
        li.SubItems[7] := inttostr(round(ep.outputchannel.rsAckTime.PeriodicAverage));
        {$ELSE}
        li.SubItems[7] := inttostr(round(ep.rsAckTime.PeriodicAverage));
        {$ENDIF}
        {$IFDEF RUDP2}
        li.SubItems[8] := inttostr(ep.outputchannel.FlexREtransTime);
        {$ELSE}
        li.SubItems[8] := inttostr(ep.FlexREtransTime);
        {$ENDIF}
        li.SubItems[9] := inttostr(ep.bestAckTime);
        {$IFDEF RUDP2}
        li.SubItems[10] := inttostr(ep.outputchannel.Retransmissions);
        {$ELSE}
        li.SubItems[10] := inttostr(ep.Retransmissions);
        {$ENDIF}
        li.SubItems[11] := inttostr(ep.OutOfOrder);
        {$IFDEF RUDP2}
        li.SubItems[12] := inttostr(round(ep.outputchannel.rsAckTime.Rate));
        {$ELSE}
        li.SubItems[12] := inttostr(round(ep.rsAckTime.Rate));
        {$ENDIF}
        {$IFDEF RUDP2}
        li.SubItems[13] := inttostr(ep.outputchannel.transmissions);
        {$ELSE}
        li.SubItems[13] := inttostr(ep.transmissions);
        {$ENDIF}
        {$IFDEF RUDP2}
        li.SubItems[14] := 'n/a';
        {$ELSE}
        li.SubItems[14] := floatprecision(ep.queue_out.ticks_per_byte,4);
        {$ENDIF}
        {$IFDEF RUDP2}
        li.SubItems[15] := inttostr(ep.outputchannel.count);
        {$ELSE}
        li.SubItems[15] := inttostr(ep.txLog.count);
        {$ENDIF}
        li.SubItems[16] := inttostr(ep.rxLog.count);
        li.SubItems[17] := inttostr(ep.rxQueue.count);
        li.SubItems[18] := inttostr(ep.rxDataLog.count);
        li.SubItems[19] := tmNow.tostring;


      finally
        ep.unlock;
      end
{$IFDEF TRYLOCKRUDP}
      else begin
        li := lvrudp.items[t];
        li.Caption := 'X'+li.Caption;
      end;
{$ENDIF}
    end;
  finally
    RUDPMon.Unlock;
  end;



  rudp_reads_oh[rudp_chartidx] := iTotalREadOH;
  rudp_writes_oh[rudp_chartidx] := iTotalWriteOH;
  rudp_reads[rudp_chartidx] := iTotalREad;
  rudp_writes[rudp_chartidx] := iTotalWrite;
  inc(rudp_chartidx);
  if rudp_chartidx >= high(rudp_reads) then
    rudp_chartidx := 0;
  helpers.chart.chart_ClearData(cRUDP);
  for t:= rudp_chartidx to high(rudp_reads) do begin
    cRUDP.Series[3].Add(rudp_writes_oh[t]);
    cRUDP.Series[2].Add(rudp_reads_oh[t]);
    cRUDP.Series[1].Add(rudp_reads[t]);
    cRUDP.Series[0].Add(rudp_writes[t]);
  end;
  for t:= 0 to rudp_chartidx-1 do begin
    cRUDP.Series[3].Add(rudp_writes_oh[t]);
    cRUDP.Series[2].Add(rudp_reads_oh[t]);
    cRUDP.Series[1].Add(rudp_reads[t]);
    cRUDP.Series[0].Add(rudp_writes[t]);
  end;



  lastUDPCheck := GEtTicker;





//  debug.log(self, memo1.text);
end;



procedure TfrmBGThreadWatcher.UpdateInfos(infos: TArray<TThreadInfo>);
var
  t,t2: integer;
  thr2: TThreadInfo;
  item : TListItem;
  tmNow: cardinal;
  sTemp: string;
  cTemp1, cTemp2: cardinal;
  iTemp1, iTEmp2: integer;
  v: variant;
  fTemp: real;
  temp64, temptime, deltatime, tempfreq: int64;
  c,l,i: int64;
  tt: TThreadTimes;

  rTemp: single;
  totalCpuTicks: int64;
  totalUsedTicks: int64;
  ts:  PThreadState;
begin
  totalCPUTicks := 0;
  temptime := GetHighResTicker;
  deltaTime := (temptime-Self.LastNanotick);
  ForceItemCount(lvBackground, length(infos));
  SetLength(threadstates, length(infos));
  lvBackground.Columns[2].Caption := 'Threads ('+inttostr(length(Infos))+')';
  for t:= 0 to high(infos) do begin
    thr2 := infos[t];
    item := lvBackground.Items[t];

    ForceSubItems(item, 14);
    ts := @threadstates[t];
    ts.pooled := false;
    ts.expireprog := 0;

    //ts.pooled := thr2.pooled;
    ts.expireprog := thr2.Age / 60000;
    try
      ts.pooled := thr2.Pooled;

      if thr2.Blocked then begin
        item.ImageIndex := 41;
      end else
      if thr2.Spin then begin
        if not thr2.AutoSpin then
          item.ImageIndex := thr2.Step mod 8
        else
          item.imageindex := (item.imageindex+1) mod 8;
      end else begin
        c := thr2.Step;
        l := thr2.StepCount;
        i := c;
        if l > 0 then
          i := round((c / l)*32)
        else
          i := 0;

        if i> 32 then i:=32;
        if i <0 then i:= 0;
        item.ImageIndex := 8+i;
      end;
      try
        if (item.caption <> inttostr(thr2.threadid)) then
          item.caption := inttostr(thr2.threadid);

        if item.subitems[1] <> thr2.Name then
          item.subitems[1] := thr2.SignalDebug+thr2.Name;

        //read last tick
        if item.SubItems[9] <> '' then
          cTemp1 := strtoint64(item.Subitems[9])
        else
          cTemp1 := 0;

        //read last work value
        if item.SubItems[4] <> '' then
          iTemp1 := strtoint64(item.Subitems[5])
        else
          iTemp1 := 0;


        //get current tick
        cTemp2 := GetTicker;

        item.SubItems[8] := inttostr(integer(cTemp2));

        //calc tick difference
        cTEmp2 := cTemp2-cTemp1;

        //read current work value
        iTemp2 := thr2.Iterations;

        //calculate work throughput
        if cTemp2 >0 then
          fTemp := (iTemp2-iTemp1)/(cTemp2/1000)
        else
          fTemp := 0.0;

        //write out values
        item.SubItems[2] := thr2.Status;
        item.SubItems[3] := inttostr(thr2.StepCount);
        item.SubItems[4] := inttostr(thr2.Step);
        item.SubItems[5] := inttostr(thr2.Iterations);
        item.SubItems[6] := thr2.ColdRunInterval.tostring;
        item.SubItems[9] := inttostr(thr2.age);
        item.SubItems[7] := 'deprecated';


        //-----------THREAD TIMES
        if deltaTime = 0 then deltaTime := 1;

        QueryPerformanceFrequency(tempfreq);



        //- - - - - - - - - - - - - - - - - - - -
        if (IsInteger(item.subitems[12])) then begin
          temp64 := strtoint64(item.subitems[12]);
        end else
          temp64 := 0;

//              temp64 := 0;


        temp64 := tt.user - temp64;
//              IF tt.kernel > 0 then
//                debug.consolelog('weee');

        item.subitems[12] := tt.user.tostring;
        totalCPUTicks := 0;
        if deltaTime > 0 then begin
          rTemp := (((temp64/tempfreq)/(deltaTime/tempfreq)));
          totalCPUTicks := temp64;
          rTemp := lesserof(1, rTemp);
          sTemp := floatprecision(rTemp*100,2)+'%';
          item.SubItems[10] := sTemp;
        end;

        //- - - - - - - - - - - - - - - - - - - -
        if (IsInteger(item.subitems[13])) then begin
          temp64 := strtoint64(item.subitems[13]);
        end else
          temp64 := 0;

//              temp64 := 0;

        temp64 := tt.kernel - temp64;
        item.subitems[13] := tt.kernel.tostring;

        if deltaTime > 0 then begin
          rTemp := (((temp64/tempfreq)/(deltaTime/tempfreq)));
          rTemp := lesserof(1, rTemp);
          sTemp := floatprecision(rTemp*100,2)+'%';
          item.subitems[11] := sTemp;
          inc(totalCPUTicks, temp64);
          rTemp := (((totalCPUTicks/tempfreq)/(deltaTime/tempfreq)));
          rTemp := lesserof(1, rTemp);
          rTemp := rTemp * 10;
          sTemp := '..........';
          for t2:= 1 to (round(rTemp)) do begin
            sTemp[t2-(1-STRZ)] := '|';
          end;

          item.SubItems[0] := sTemp;

        end;


      finally
      end;
    finally
    end;


  end;
  self.LastNanoTick := temptime;
end;

function TCommandBitmapThread.GetIdealSquareSize(): integer;
var
  h: integer;
  d: nativeint;
  cpCount: nativeint;

begin
  h := db.height;
  result := 1;

  if h = 0 then begin
    result := 0;
    exit;
  end;


  cpCount := length(volState);
  repeat
    d := (db.width div h);
    if d = 0 then begin
      result := 0;
      exit;
    end;

    if ((cpCount div d) + 1) < (db.height div h) then
      break;

    h := h -1;
  until h <=1;

  if h<1 then h := 0;
  result := h;
end;

procedure TCommandBitmapThread.ResizeDB;
begin
  if db = nil then begin
    db := TFastBitmap.Create;
  end;

  if (db.Width = width) and (db.Height=height) then
    exit;


  db.Width := width;
  db.Height := height;


end;

procedure TCommandBitmapThread.SetHeight(const Value: ni);
begin
  FHeight := Value;
end;

procedure TCommandBitmapThread.SetWidth(const Value: ni);
begin
  FWidth := Value;
end;

function TfrmBGThreadWatcher.GetSelectedThread: TManagedThread;
begin
  result := nil;
  if lvBackground.ItemIndex < 0 then
    exit;

  BackgroundThreadMan.Lock;
  try
    result := TManagedThread(BackgroundThreadMan.Threads[lvBackground.ItemIndex]);

  finally
    BackgroundThreadMan.UnLock;
  end;
end;

procedure TfrmBGThreadWatcher.lvBackgroundAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  idx: ni;
begin
  idx := sender.items.indexof(item);
  sender.Canvas.Brush.Color := clWhite;
  if idx >=0 then begin
    if threadstates[idx].pooled then begin
      sender.canvas.font.color := colorblend(clGray, clWhite, threadstates[idx].expireprog);
    end else
      sender.canvas.font.color := clBlack;
  end;
//  DefaultDraw := not DefaultDraw;
end;

procedure TfrmBGThreadWatcher.lvBackgroundChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if (change = TItemChange.ctState) and (Item.Selected) then begin
    popThread.Items.Clear;
    BackgroundThreadMan.Lock;
    try
      syncpopup;
    finally
      backgroundthreadman.unlock;
    end;

  end;
end;

procedure TfrmBGThreadWatcher.lvBackgroundCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := true;
end;

procedure TfrmBGThreadWatcher.lvBackgroundCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  DefaultDraw := true;
end;

procedure TfrmBGThreadWatcher.lvBackgroundDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  idx: ni;
begin
  idx := sender.items.indexof(item);
  if idx >=0 then begin
    if threadstates[idx].pooled then
      sender.canvas.Brush.Color := clSilver;
  end;
end;

procedure TfrmBGThreadWatcher.POpupMenuClick(sender: TObject);
var
  thr: TManagedThread;
begin
  BackgroundThreadMan.Lock;
  try
    thr := GetSelectedThread;
    THR.MenuAction(TMenuItem(sender).Tag);

  finally
    backgroundthreadman.Unlock;
  end;
end;

procedure TCommandBitmapThread.DoExecute;
begin
  inherited;
  cprr.o.lock;
  try
    volState := cprr.o.volState;
  finally
    cprr.o.unlock;
  end;
  Status := 'Draw';
  resizedb;
  DrawCommandBitmap();
  Status := 'Done';

end;

procedure TCommandBitmapThread.DrawCommandBitmap();
var
  t: integer;
  c: TProgressEx;
  cc,ccMatte, ccNoise,cccc,cicon: tColor;

  xx,yy: integer;
  tt,uu: integer;
  h: integer;
  tm: cardinal;
  p: nativefloat;
  f1, f2: nativefloat;
  bDoNoise: boolean;
  cNoise: TColor;
  b: byte;
  rPBFade: nativefloat;
          function GetXY(iPos: integer; out x,y: integer): boolean;
          begin
            x := iPos mod (db.width div h);
            y := iPos div (db.width div h);
            result := (y < db.height div h);
          end;

begin


  db.new;
  db.Canvas.Clear(clBlack);

  try
    tm := GetTicker;
    f1 := (tm mod 750)/7500;
    f1 := f1 * 6.23;
    f1 := sin(f1);
    f1 := f1 + 1;
    f1 := f1 / 2;
    f2 := (tm mod 500)/500;
    f2 := f2 * 6.23;
    f2 := sin(f2);
    f2 := f2 + 1;
    f2 := f2 / 2;

    cNoise := clBlack;
    h := GetIdealSquareSize();
    for t:= 0 to high(volState) do begin
      cc := $007f7f;
      bDoNoise := true;
      cNoise := clBlack;
      if (not GetXY(t,xx,yy)) then
        break;
      c := volstate[t];
      p := c.progress.PercentComplete;
      if c.isComplete then
        cc := clLime
      else
      if c.isRunning then begin
        cc := clYellow;

        if c.IsExecutingNow then
          cNoise := clRed
        else
          cNoise := clWhite;

      end
      else if c.isBlockedByDependsOn then begin
        cc := colorblend(clREd, $FF00FF, f1);
      end
      else if c.isBlocked then begin
        cc := colorblend(clWhite, $FFFF00, f2);
      end else
        cc := clred;

      if c.isWaitingForResources then
        cc := colorblend(cc, clBlue, f1);



      for uu := 0 to h-1 do begin
        for tt := 0 to h-1 do begin
          ccMatte := Colorblend(cc, ColorBlend(cc, clBlack, 0.5), (sqrt((uu*uu)+(tt*tt)))/(sqrt((h*h)+(h*h))));;
          if bDoNoise then begin
            cccc := ColorBlend(Colorblend(ccMatte, cNoise,(random(trunc((h/96)*255))/255)*(1-(p/2))),
                               Colorblend(ccMatte, clBlack,(random(trunc((h/128)*255))/255)*(1-(p/2))),(uu mod 2));

          end else begin
            cccc := ccMatte;
          end;
          rPBFade := (1-(uu/h));
          if rPBFade < 0 then
            rPBFade := 0;

          if (1-(uu / h)) < p then
            cccc := ColorBlend(cccc,clWhite, rPBFAde);


          if h > c.Icon.Width then begin
            if (tt < c.Icon.Width div 1) and (uu < c.Icon.Width div 1) then begin
              cIcon := c.Icon.data[(uu)+0][(tt)+0];
              if (c.IsExecutingNow) then begin
                cccc := colorblend_ForegroundSourceAlpha(cccc,cIcon,1);
              end else begin
                cccc := colorblend_ForegroundSourceAlpha(cccc,cIcon,0.8);
              end;
            end;
          end else
          if h > c.Icon.Width div 2 then begin
            if (tt < c.Icon.Width div 2) and (uu < c.Icon.Height div 2) then begin
              cIcon := c.Icon.data[(uu*2)+0][(tt*2)+0];
              cIcon := colorblendRGBA(cIcon, c.Icon.data[(uu*2)+1][(tt*2)+0], 0.5);
              cIcon := colorblendRGBA(cIcon, c.Icon.data[(uu*2)+0][(tt*2)+1], 0.333);
              cIcon := colorblendRGBA(cIcon, c.Icon.data[(uu*2)+1][(tt*2)+1], 0.25);
              cccc := cccc + (255 shl 24);
              if (c.IsExecutingNow) then begin
                cccc := colorblend_ForegroundSourceAlpha(cccc,cIcon,1);
              end else begin
                cccc := colorblend_ForegroundSourceAlpha(cccc,cIcon,0.75);
              end;
            end;
          end else begin
            //icon overlay
//            if (uu < c.Icon.Height) and (tt < c.Icon.Width) then begin
//              if (c.IsExecutingNow) then begin
//                cccc := colorblend(cccc,GEtCommandIconColor(c.Icon.data[uu][tt], cccc),0.98);
//              end else begin
//                cccc := colorblend(cccc,GEtCommandIconColor(c.Icon.data[uu][tt], cccc),0.2);
//              end;
//            end;
          end;



          db.Canvas.pixels[(xx*h)+TT,(yy*h)+uu] := cccc;


        end;
      end;

    end;

//    db.Flip;
  finally
//    cp.unlock;
  end;


end;

destructor TfrmBGThreadWatcher.Destroy;
begin
  if assigned(thrDraw) then begin
    thrDraw.Stop;
    thrDraw.waitfor;
  end;
  TPM.NoNeedThread(thrDraw);
  thrDraw := nil;

  inherited;
end;

procedure TfrmBGThreadWatcher.DrawDiskUsage(cp: TCommandProcessor);
var
  t: integer;
begin
  if cp.trylock then
  try
    cp.ResourceStats.Lock;
    try
      memDisk.Clear;
      for t:= 0 to cp.ResourceStats.StatCount-1 do begin
        memDisk.Lines.Add(cp.ResourceStats.Stats[t].Resource+':'#9+floattostr(cp.ResourceStats.Stats[t].MaxLarge)+#9+floattostr(cp.ResourceStats.Stats[t].MaxSmall));
      end;
    finally
      cp.ResourceStats.Unlock;
    end;
  finally
    cp.unlock;
  end;
end;

procedure TfrmBGThreadWatcher.ForceItemCount(lv: TlistView; iQuantity: integer);
var
  temp: TListItem;
begin
  try
    while lv.Items.count < iQuantity do begin
      temp := lv.items.add;
      temp.caption := '....';
    end;

    while lv.Items.count > iQuantity do begin
      lv.Items.Delete(lv.items.count-1);
    end;
    setlength(threadstates, iQuantity);
  finally
  end;

end;

procedure TfrmBGThreadWatcher.ForceSubItems(li: TListItem;
  iCount: integer);
begin
  while li.SubItems.Count < iCount do begin
    li.SubItems.Add('');
  end;

  while li.SubItems.Count > iCount do begin
    li.SubItems.Delete(li.SubItems.count-1);
  end;

end;



procedure TfrmBGThreadWatcher.FrameResize(Sender: TObject);
begin
  if assigned(dbr) then dbr.free;
  dbr := TFastBackBufferedControl.create(self);
  dbr.parent := pnlBitmap;
  dbr.Align := alClient;
//
end;

function GetCommandIconColor(c, bg: cardinal): cardinal;
begin
  result := c;
  result := colorblend(cardinal(bg) and $FFFFFF, cardinal(result) and $FFFFFF, ((cardinal(c) and $FF000000) shr 24)/255);



end;



end.
