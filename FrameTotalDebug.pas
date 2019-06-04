unit FrameTotalDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, debug,  FormBGThreadWatcher,
  VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.TeEngine,
  VclTee.TeeGDIPlus, GUIHelpers, sharedobject, typex, betterobject;

type
  TframTotalDebug = class(TFrame)
    tabCommands: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    Chart1: TChart;
    Series1: TPieSeries;
    TabSheet4: TTabSheet;
    ListView1: TListView;
    tmStartupHack: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure tmStartupHackTimer(Sender: TObject);
  private
    { Private declarations }
    bg: TfrmBGThreadWatcher;
  public
    { Public declarations }
    constructor Create(aowner: TComponent);override;
    destructor Destroy;override;
    procedure UpdateMasterMonitor;
    procedure PutIn(parnt: TWinControl);
    procedure CReateWatcher;
  end;

implementation

{$IFDEF DO_MEM_CHART}
uses
  brainscanultra;
{$ENDIF}

{$R *.dfm}

{ TframTotalDebug }


constructor TframTotalDebug.Create(aowner: TComponent);
begin
  inherited;
//  slBackLog := Tstringlist.create;
end;

procedure TframTotalDebug.CReateWatcher;
begin
  if not assigned(bg) then begin
    bg := TfrmBGThreadWatcher.Create(self);
    bg.Parent := TabSheet2;
  end;

end;

destructor TframTotalDebug.Destroy;
begin

  inherited;
//  slBackLog.free;
end;

procedure TframTotalDebug.PutIn(parnt: TWinControl);
begin
  self.width := parnt.ClientWidth;
  self.Height := parnt.clientheight;
  self.Parent := parnt;
  self.Resize;

end;


procedure TframTotalDebug.TabSheet2Show(Sender: TObject);
begin
  createwatcher;

end;

procedure TframTotalDebug.Timer1Timer(Sender: TObject);
begin

{$IFDEF DO_MEM_CHART}
  BrainScanUltra.MemChart(chart1);
{$ENDIF}
  CreateWatcher;

  UpdateMasterMonitor;


  if not assigned(bg) then begin
    bg := TfrmBGThreadWatcher.Create(self);
    bg.Parent := TabSheet2;
    tabsheet2.refresh;
  end;








end;

procedure TframTotalDebug.tmStartupHackTimer(Sender: TObject);
begin
  //I have a love-hate relationship with Delphi
  //as it has a ton of bugs... like this one...
  //my frames will not show unless I put a timer
  //in the app and cycle through each of the tabs.

  //cycle through next page
  tabCommands.ActivePageIndex := (tabCommands.activepageIndex + 1) mod tabCommands.PageCount;

  //stop timer once all pages have been loaded
  tmStartupHack.enabled := tabCommands.ActivePageIndex > 0;


end;

procedure TframTotalDebug.UpdateMasterMonitor;
var
  mm: TstandardMonitor;
  t: ni;
begin

  mm := MasterMonitor;
  mm.Lock;
  try
    SyncListView(self.ListView1,mm.count, 2);
    for t:= 0 to mm.count-1 do begin
      listview1.items[t].caption := inttohex(nativeint(pointer(mm.Monitored[t])), sizeof(nativeint));
      listview1.items[t].SubItems[0] := mm.Monitored[t].classname;
      listview1.items[t].SubItems[1] := mm.Monitored[t].status;

    end;
  finally
    mm.unlock;
  end;


end;

end.
