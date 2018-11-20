unit FormDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormBase, FormBGThreadWatcher, ExtCtrls, StdCtrls, ComCtrls, debug,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart,
  VclTee.TeeGDIPlus;


type
  TfrmDebug = class(TfrmBase)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    tsthreads: TTabSheet;
    Memo1: TMemo;
    tm: TTimer;
    TabSheet2: TTabSheet;
    Chart1: TChart;
    Series1: TPieSeries;
    procedure frmBaseCreate(Sender: TObject);
    procedure tmTimer(Sender: TObject);
    procedure frmBaseClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    dbg: TfrmBGThreadWatcher;
  end;

var
  frmDebug: TfrmDebug;

implementation

{$R *.dfm}

procedure TfrmDebug.frmBaseClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;

procedure TfrmDebug.frmBaseCreate(Sender: TObject);
begin
  inherited;
  dbg := TfrmBGThreadWatcher.create(self);
  dbg.parent := tsThreads;
  PageControl1.ACtivePageIndex := 0;
end;

procedure TfrmDebug.tmTimer(Sender: TObject);
var
  s: string;
begin
  inherited;

  s := debug.DebugLog.DrainLog();
  if s <> '' then begin
    memo1.lines.add(s);
    if memo1.lines.count > 100 then
      memo1.lines.clear;
    while memo1.lines.count > 50 do begin
      memo1.lines.delete(0);
    end;
  end;

end;

initialization
  frmDebug := nil;

end.
