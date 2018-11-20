unit DataObjectCacheJanitor;
interface
{$Message Hint 'This code using OLD, primitive threading techniques'}

uses
  Classes, sysutils, threadmanager, managedthread;

type
  TDataObjectCacheJanitor = class(TManagedThread)
  private
    procedure SetCacheManager(const Value: TObject);
    { Private declarations }
  protected

    procedure DoExecute; override;
  public
    FCacheManager: TObject;
    property CAcheManager: TObject read FCacheManager write SetCacheManager;
    procedure InitFromPool;override;
    procedure Detach;override;
    destructor Destroy; override;
  end;

implementation

uses
  DataObjectCacheManager, DataObjectServices, Dataobjectcache, Dataobject;

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------

destructor TDataObjectCacheJanitor.Destroy;
begin
  inherited;
end;
procedure TDataObjectCacheJanitor.Detach;
begin
  if detached then exit;

  if assigned(FCacheManager) then
    TDataObjectCacheManager(FCacheManager).DeRegisterJanitor(self);

  FCacheManager := nil;

  inherited;

end;

//------------------------------------------------------------------------------
procedure TDataObjectCacheJanitor.DoExecute;
const
  THREAD_SLEEP = 300;
var
  i, iCount: integer;
begin
  inherited;
  Spin := true;
  self.status := 'Spinning up [.       ]';
  sleep(1000);
  self.status := 'Spinning up [..      ]';
  sleep(1000);
  self.status := 'Spinning up [...     ]';
  sleep(1000);
  self.status := 'Spinning up [....    ]';
  sleep(1000);
  self.status := 'Spinning up [.....   ]';
  sleep(1000);
  self.status := 'Spinning up [......  ]';
  sleep(1000);
  self.status := 'Spinning up [....... ]';                       
  sleep(1000);
  self.status := 'Spinning up [........]';
  sleep(1000);
  self.status := 'Ready';

  { Place thread code here }
  //FreeOnTerminate := true;
  while not safeTerminated do begin
    sleep(THREAD_SLEEP);
//    windows.beep(75,10);
//  windows.beep(100,10);
    try
      repeat
        StepCount := TDataObjectCacheManager(FCacheManager).DecommisionedCacheCount;//+TDataObjectCacheManager(FCacheManager).DeadCacheCount;
//        Load := TDataObjectCacheManager(FCacheManager).DecommisionedCacheCount+TDataObjectCacheManager(FCacheManager).DeadCacheCount;
        //auditlog('About to enter expireCaches Load = '+inttostr(Load),'bg');
        i := TDataObjectCacheManager(FCacheManager).ExpireCaches;

        iCount := DOOB.Count;
        (*if iCount < 1000 then begin
          self.Priority := tpLower;
          self.Status := 'Low Priority';
        end else
        if iCount >= 10000 then begin
          self.Priority := tpNormal;
          self.Status := 'High Priority';
        end else
        if iCount >= 1000 then begin
          self.Priority := tpHigher;
          self.Status := 'Normal Priority';
        end;*)


        //IterationComplete(i);




      until (i = 0) or safeTerminated;




      //increment cycle count
      //CycleComplete;
    except
      on E: Exception do begin
//        auditlog('****'+e.message,'bg');
        self.status := E.Message;
      end;
    end;
  end;
  //windows.beep(50,100);
  //FreeOnTerminate := true;
end;


procedure TDataObjectCacheJanitor.InitFromPool;
begin
  inherited;
  FCacheManager := CacheManager;
  self.name := 'Garbage Man';

end;

procedure TDataObjectCacheJanitor.SetCacheManager(const Value: TObject);
begin
  FCacheManager := Value;

  TDataObjectCacheManager(FCacheManager).RegisterJanitor(self);

end;

end.

