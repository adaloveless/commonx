unit FmxFrameDebug;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FmxFrameBase, FMX.ListView.Types, Data.Bind.GenData,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.ListView, FMX.Grid,
  FMX.Layouts, backgroundthreads, managedthread, FMX.Grid.Style,
  FMX.Controls.Presentation, FMX.ScrollBox;

type
  TFrameDebug = class(TFrameBase)
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    Timer1: TTimer;
    StringColumn3: TStringColumn;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(aowner: TComponent);override;
    procedure SyncStringGrid(iRows: nativeint);
  end;

var
  FrameDebug: TFrameDebug;

implementation

{$R *.fmx}

constructor TFrameDebug.Create(aowner: TComponent);
begin
  inherited;

end;

procedure TFrameDebug.SyncStringGrid(iRows: nativeint);
begin
  while stringgrid1.RowCount > iRows do begin
    stringgrid1.RowCount := iRows;
  end;
  while stringgrid1.RowCount < iRows do begin
    stringgrid1.RowCount := iRows;
  end;


end;

procedure TFrameDebug.Timer1Timer(Sender: TObject);
var
  x: nativeint;
  thr: TThread;
begin
  inherited;



  backgroundthreads.BackgroundThreadMan.Lock;
  try
    SyncStringGrid(BackgroundThreadMan.Count);
    for x:= 0 to backgroundthreadman.Count-1 do begin
      thr := backgroundthreadman.Threads[x];
      if thr is TManagedThread then begin
        stringgrid1.Cells[2,x] := TManagedThread(thr).GetSignalDebug+TManagedThread(thr).STatus;
        stringgrid1.Cells[1,x] := TManagedThread(thr).Name;
        stringgrid1.cells[0,x] := inttostr(TManagedThread(thr).ThreadID);
      end else begin
        stringgrid1.Cells[1,x] := 'unmanaged thread';
        stringgrid1.cells[0,x] := inttostr(TManagedThread(thr).ThreadID);
      end;
    end;
  finally
    backgroundthreads.BackgroundThreadMan.Unlock;
  end;


end;

end.
