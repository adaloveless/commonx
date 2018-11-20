unit WebThreadManager;

interface

uses
  classes, windows, webthread;

type
  TWebThreadManager = class(TObject)
  protected
    FThreads: TList;
    sect: _RTL_CRITICAL_SECTION;
    function GetThreadCount: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RegisterThread(thread: TThread);
    procedure DeRegisterThread(thread: TThread);
    procedure WaitForThreads;

    procedure ExpireThreads;

    property ThreadCount: integer read GetThreadCount;

    procedure Lock;
    procedure UnLock;
  end;

var
  WebThreads: TWebThreadManager;
implementation

{ TWebThreadManager }
//------------------------------------------------------------------------------
constructor TWebThreadManager.Create;
begin
  inherited;
  InitializeCriticalSection(sect);
  FThreads:= Tlist.create;
end;
//------------------------------------------------------------------------------
procedure TWebThreadManager.DeRegisterThread(thread: TThread);
begin
  EnterCriticalSection(sect);
  try
    FThreads.remove(thread);
  finally
    LeaveCriticalSection(sect);
  end;

end;
//------------------------------------------------------------------------------
destructor TWebThreadManager.Destroy;
begin
  FThreads.free;
  DeleteCriticalSection(sect);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TWebThreadManager.ExpireThreads;
begin
//TODO -cunimplemented: unimplemented block
end;
(*var
  t: integer;
  thread: TWebThread;
begin
  Lock;
  try
    for t:= FThreads.count-1 downto 0 do begin
      thread := TWebThread(FThreads[t]);
    end;
  finally
    UnLock;
  end;
end;*)

function TWebThreadManager.GetThreadCount: integer;
begin
  EnterCriticalSection(sect);
  try
    result := FThreads.count;
  finally
    LeaveCriticalSection(sect);
  end;

end;
//------------------------------------------------------------------------------
procedure TWebThreadManager.Lock;
begin
  EnterCriticalSection(sect);
end;

procedure TWebThreadManager.RegisterThread(thread: TThread);
begin
  EnterCriticalSection(sect);
  try
    FThreads.add(thread);
  finally
    LeaveCriticalSection(sect);
  end;
end;
//------------------------------------------------------------------------------
procedure TWebThreadManager.UnLock;
begin
  LeaveCriticalSection(sect);
end;

procedure TWebThreadManager.WaitForThreads;
begin
  sleep(1000);

end;

initialization

webThreads := TWebThreadManager.create;


finalization

webThreads.free;

end.
