unit FrameTotalDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, debug,  FormBGThreadWatcher,
  VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.TeEngine,
  VclTee.TeeGDIPlus, GUIHelpers, sharedobject, typex;

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
    procedure Timer1Timer(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
  private
    { Private declarations }
    bg: TfrmBGThreadWatcher;
  public
    { Public declarations }
    constructor Create(aowner: TComponent);override;
    destructor Destroy;override;
    procedure UpdateMasterMonitor;
    procedure PutIn(parnt: TWinControl);
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
  if not assigned(bg) then begin
    bg := TfrmBGThreadWatcher.Create(self);
    bg.Parent := TabSheet2;
  end;

end;

procedure TframTotalDebug.Timer1Timer(Sender: TObject);
begin

{$IFDEF DO_MEM_CHART}
  BrainScanUltra.MemChart(chart1);
{$ENDIF}

  UpdateMasterMonitor;

  if not assigned(bg) then begin
    bg := TfrmBGThreadWatcher.Create(self);
    bg.Parent := TabSheet2;
    tabsheet2.refresh;
  end;






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
