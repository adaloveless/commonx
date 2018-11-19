unit RUDPMonitor;

interface

uses
  simplereliableudp, sharedobject, typex, systemx, generics.collections.fixed, classes, orderlyinit, sysutils;


type
  TRUDPMonitor = class(TSharedObject)
  private
    function GetItems(idx: ni): TReliableUDPEndpoint;
    function GEtCount: ni;
  protected
    FList: TList<TReliableUDPEndPOint>;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure RegisterEndPoint(FEndPoint: TReliableUDPEndPoint);
    procedure UnRegisterEndPoint(fEndPoint: TReliableUDPEndPOint);
    function GetDebugInfoString: string;
    property Items[idx: ni]: TReliableUDPEndpoint read GetItems;
    property Count: ni read GEtCount;
  end;

var
  RUDPMon: TRUDPmonitor;

implementation


{ TRUDPMonitor }

constructor TRUDPMonitor.Create;
begin
  inherited;
  FList := TList<TReliableUDPEndpoint>.create;
end;

destructor TRUDPMonitor.Destroy;
begin
  FList.free;
  FList := nil;
  inherited;
end;

function TRUDPMonitor.GEtCount: ni;
begin
  Lock;
  try
    result := FList.count;
  finally
    Unlock;
  end;
end;

function TRUDPMonitor.GetDebugInfoString: string;
var
  lines: Tstringlist;
  t: ni;
  tr: ni;
  bGot: boolean;
begin
  lines := nil;
  Lock;
  try
    lines := Tstringlist.create;
    result:= '';

    for t:= 0 to FList.count-1 do begin
      tr := 0;
      bGot := false;
      repeat
        bGot := FList[t].TryLock;
        inc(tr);
      until bGot or (tr > 100);
      if bGot then
      try
        lines.add(FList[t].DebugSummaryMessage);
      finally
        FList[t].unlock;
      end else
        lines.add('unable to lock index ['+inttostr(t)+']');

    end;
    result := lines.text;
  finally
    Unlock;
  end;
end;

function TRUDPMonitor.GetItems(idx: ni): TReliableUDPEndpoint;
begin
  Lock;
  try
    result := FList[idx];
  finally
    Unlock;
  end;
end;

procedure TRUDPMonitor.RegisterEndPoint(FEndPoint: TReliableUDPEndPoint);
begin
  Lock;
  try
    FList.add(fendpoint);
  finally
    Unlock;
  end;
end;

procedure TRUDPMonitor.UnRegisterEndPoint(fEndPoint: TReliableUDPEndPOint);
begin
  Lock;
  try
    FList.remove(fendpoint);
  finally
    Unlock;
  end;
end;


procedure oinit;
begin
  RUDPmon := TRUDPMonitor.create;
end;

procedure ofinal;
begin
  RUDPMon.free;
  RUDPMon := nil;
end;


initialization
  init.RegisterProcs('RUDPMonitor', oinit, ofinal);




end.
