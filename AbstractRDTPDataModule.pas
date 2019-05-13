unit AbstractRDTPDataModule;


interface

uses
  typex, orderlyinit, tickcount, managedthread, SysUtils, Classes, SqlExpr, better_Sockets, systemx,inifiles, commandprocessor,
  sockfix, classfactory, sharedobject, betterobject, windows, stringx, genericRDTPClient, storageenginetypes, helpers.list;

const
  DB_CONNECTION_TIMEOUT: cardinal = 300000;
  MAX_DM_AGE = 300000;

type
  TSQLChannel = (sqlRead, sqlWrite, sqlSystem);
  TAbstractRDTPDataModule = class;
  TRDTPDataModuleClass= class of TAbstractRDTPDataModule;
  TRDTPExecutionTHread = class;

  TDataClass = class of TObject;
  TAbstractRDTPDataModuleClass = class of TAbstractRDTPDataModule;//forward

  TDataPool = class(TSharedObject)
  private
    function GetClassToCreate: TRDTPDataMOduleClass;
    procedure SetClassToCreate(const Value: TRDTPDataMOduleClass);
    function GetCount: integer;
    function GetData(idx: integer): TAbstractRDTPDAtaModule;
    function GetActiveCount: integer;
  protected
    FRDTPDataModuleClass: TRDTPDataModuleClass;
    FData: TList;
    Fet: TRDTPExecutionThread;
    FActiveCount: integer;
    FCreationTime: cardinal;
    property Data[idx: integer]: TAbstractRDTPDAtaModule read GetData;
  public
    factory: TClassClassFactory;
    constructor create;override;
    destructor destroy;override;
    function NeedData(cClass: TDataClass; sContext: string): TAbstractRDTPDataModule;
    procedure NoNeedData(dm: TAbstractRDTPDataModule);
    property DefaultProduct: TRDTPDataMOduleClass read GetClassToCreate write SetClassToCreate;
    property Count: integer read GetCount;
{$IFDEF ALLOW_CONTROLS}
    procedure SyncToListView(lv: TListView);
{$ENDIF}
    property ActiveCount: integer read GetActiveCount;

  end;



  Tcmd_DBWriteBehind = class(TCommand)
  public
    dm: TAbstractRDTPDataModule;
    query: string;
    procedure InitExpense;override;
    procedure DoExecute;override;
  end;

  TAbstractRDTPDataModule = class(TBetterObject)
  private
    FLastUsed: cardinal;
    FCreationTime: cardinal;
    procedure SetConfigFromLocalFile(const Value: boolean);
  protected
    cmd: TCommand;
    Fet: TRDTpExecutionThread;
    FProg: TRDTPProgressEvent;
    FContext: string;
    FConfigFromLocalFile: boolean;
    procedure WaitForCommands;
    function GetExecutionThread: TRDTPExecutionThread;
    procedure DataModuleCreate(Sender: TObject);
    function TryGetNextID(iKey: integer; out res: int64): boolean;
    function TrySetNextID(iKey: integer; value: int64): boolean;
    procedure Execute(sQuery:string; connection: TSQLConnection; ds: TCustomSQLDataset);
    procedure ExecuteDirect(sQuery: string; connection: TSQLConnection);
    procedure SetProgress(const Value: TRDTPProgressEvent);virtual;
    procedure SetContext(value: string);
    function GetContext: string;
  public
    constructor create;overload;override;
    constructor create2(sContext: string);overload;virtual;
    destructor destroy;override;
    procedure BeginTransaction;virtual;
    procedure Commit;virtual;
    procedure Rollback;virtual;

    function GetNextID(iKey: integer): int64;virtual;
    function SetNextID(iKey: integer; iValue: int64): int64;virtual;




    //RETURNS TSEROWSET
    function ExecuteSystem(sQuery: string; out dataset: TSERowSet): integer;overload;virtual;
    function ExecuteWrite(sQuery: string; out dataset: TSERowSet): integer;overload;virtual;
    procedure ExecuteRead(sQuery: string; out dataset: TSERowSet);virtual;

    //RETURNS NOTHING
    function ExecuteSystem(sQuery: string): integer;overload;virtual;
    function ExecuteWrite(sQuery: string): integer;overload;virtual;
    function ExecuteWriteRaw(sQuery: string): integer;overload;virtual;
    procedure ExecuteWriteBehind(sQuery: string);

    procedure ConnectRead;virtual;
    procedure ConnectWrite;virtual;
    procedure ConnectSystem;virtual;
    procedure ConfigureFromContext;virtual;

    property OnConnectionProgress: TRDTPProgressEvent read FProg write SetProgress;
    function CreateExecutionThread: TRDTPExecutionThread; virtual;
    property ExecutionThread: TRDTPExecutionThread read GetExecutionThread;
    property Context: string read GetContext write SetContext;
    procedure CheckContextSet;
    function GetConfigFile: string;virtual;
    property ConfigFromLocalFile: boolean read FConfigFromLocalFile write SetConfigFromLocalFile;
    property LastUsed: cardinal read FLastUsed write FLastUsed;
    function IsExpired: boolean;


  end;


  TRDTPExecutionThread = class(TProcessorThread)
  private
    FDSResult: TSERowSet;
    function GetMod: TAbstractRDTPDAtaModule;
    procedure SetMod(const Value: TAbstractRDTPDAtaModule);
    function GEtDSResult: TSERowSet;
  protected
    FDS: TSERowset;
    FQuery: string;
    FMod: TAbstractRDTPDAtaModule;
    procedure DoExecute; override;
  public
    destructor Destroy;override;
    property DSResult: TSERowSet read GEtDSResult;
    property Module:TAbstractRDTPDAtaModule read GetMod write SetMod;
    procedure ExecuteSystem(sQuery: string; bWAitWithKeepAlive: boolean = true);
  end;


var
  datapool: TDataPool;


implementation

uses DatabaseDictionary, Exceptions, beeper;


{ TAbstractRDTPDataModule }

procedure TAbstractRDTPDataModule.BeginTransaction;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TAbstractRDTPDataModule.CheckContextSet;
begin
  if FContext = '' then
    raise EClassException.create('Server context not set in '+self.ClassName);
end;

procedure TAbstractRDTPDataModule.Commit;
begin

//TODO -cunimplemented: unimplemented block
end;




procedure TAbstractRDTPDataModule.ConfigureFromContext;
begin
  //no implementation needed

end;

procedure TAbstractRDTPDataModule.ConnectRead;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TAbstractRDTPDataModule.ConnectSystem;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TAbstractRDTPDataModule.ConnectWrite;
begin

//TODO -cunimplemented: unimplemented block
end;

constructor TAbstractRDTPDataModule.create;
begin
  inherited;
  FCreationtime := GetTicker;
  FContext := '';
end;

constructor TAbstractRDTPDataModule.create2(sContext: string);
begin
  CReate;
//  beeper.beeparray([200,300],[50]);

  FContext := sContext;
end;

function TAbstractRDTPDataModule.CreateExecutionThread: TRDTPExecutionThread;
begin
  result := TPM.Needthread<TRDTPExecutionThread>(nil);
end;

procedure TAbstractRDTPDataModule.DataModuleCreate(Sender: TObject);
begin

//TODO -cunimplemented: unimplemented block
end;

destructor TAbstractRDTPDataModule.destroy;
begin
  WaitForCommands;
//  beeper.beeparray([300,200],[50]);

  Fet.free;
  inherited;
end;

procedure TAbstractRDTPDataModule.Execute(sQuery: string;
  connection: TSQLConnection; ds: TCustomSQLDataset);
begin
  CheckContextSet;
//TODO -cunimplemented: unimplemented block
end;

procedure TAbstractRDTPDataModule.ExecuteDirect(sQuery: string;
  connection: TSQLConnection);
begin
  CheckContextSet;
//TODO -cunimplemented: unimplemented block
end;



procedure TAbstractRDTPDataModule.ExecuteRead(sQuery: string;
  out dataset: TSERowSet);
begin
  raise ECritical.create('not implemented. implement ExecuteRead in '+self.classname);
end;



function TAbstractRDTPDataModule.ExecuteSystem(sQuery: string;
  out dataset: TSERowSet): integer;
begin
  raise ECritical.create('not implemented');
end;

function TAbstractRDTPDataModule.ExecuteSystem(sQuery: string): integer;
begin
  raise ECritical.create('not implemented');
end;



function TAbstractRDTPDataModule.ExecuteWrite(sQuery: string): integer;
begin
  WaitForCommands;
  result := ExecuteWriteRaw(sQuery);
end;


function TAbstractRDTPDataModule.ExecuteWrite(sQuery: string;
  out dataset: TSERowSet): integer;
begin
  result := 0;
  WaitForCommands;
  //
end;

procedure TAbstractRDTPDataModule.ExecuteWriteBehind(sQuery: string);
var
  c: Tcmd_DBWriteBehind;
begin
  ExecuteWrite(sQuery);
//  WaitForCommands;
//
//  c := Tcmd_DBWriteBehind.create;
//  c.dm := self;
//  c.Query := sQuery;
//  c.Start;
//  cmd := c;

end;

function TAbstractRDTPDataModule.ExecuteWriteRaw(sQuery: string): integer;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;



function TAbstractRDTPDataModule.GetContext: string;
begin
  result := FContext;
end;

function TAbstractRDTPDataModule.GetExecutionThread: TRDTPExecutionThread;
begin
  if Fet = nil then begin
    Fet := CreateExecutionThread;
    Fet.Module := self;
  end;
  result := Fet;
end;

function TAbstractRDTPDataModule.GetNextID(iKey: integer): int64;
begin
  result := 0;
end;


function TAbstractRDTPDataModule.IsExpired: boolean;
begin
  result := (GetTimeSince(GetTicker, FLastUsed) > DB_CONNECTION_TIMEOUT) or (GEtTimeSince(FCreationTime) > MAX_DM_AGE);


end;

procedure TAbstractRDTPDataModule.Rollback;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TAbstractRDTPDataModule.SetConfigFromLocalFile(const Value: boolean);
begin
  FConfigFromLocalFile := Value;
end;

procedure TAbstractRDTPDataModule.SetContext(value: string);
begin
  FContext := value;
  ConfigureFromContext;
end;

function TAbstractRDTPDataModule.SetNextID(iKey: integer; iValue: int64): int64;
begin
  result := 0;
end;

procedure TAbstractRDTPDataModule.SetProgress(const Value: TRDTPProgressEvent);
begin
  FProg := Value;
end;

function TAbstractRDTPDataModule.TryGetNextID(iKey: integer;
  out res: int64): boolean;
begin

//TODO -cunimplemented: unimplemented block
  raise Exception.create('not implemented');
end;

function TAbstractRDTPDataModule.TrySetNextID(iKey: integer;
  value: int64): boolean;
begin

//TODO -cunimplemented: unimplemented block
  raise Exception.create('not implemented');
end;

procedure TAbstractRDTPDataModule.WaitForCommands;
begin
  if assigned(cmd) then begin
    cmd.waitfor;
    cmd.free;
  end;
  cmd := nil;
end;

{ TDataPool }

constructor TDataPool.create;
begin
  inherited;
  FData := TList.create;
  FRDTPDataModuleClass := TAbstractRDTPDataModule;
  factory :=classfactory.TClassClassFactory.create();
end;

destructor TDataPool.destroy;
begin
  FACTORY.Free;
  factory := nil;
  FreeListContents(FData);
  Fdata.free;


  inherited;
end;

function TDataPool.GetActiveCount: integer;
begin
  Lock;
  try
    result := FActiveCount;
  finally
    Unlock;
  end;
end;

function TDataPool.GetClassToCreate: TRDTPDataMOduleClass;
begin
  Lock;
  try
    result := FRDTPDAtaModuleClass;
  finally
    Unlock;
  end;
end;

function TDataPool.GetCount: integer;
begin
  Lock;
  try
    result := FData.count;
  finally
    Unlock;
  end;
end;


function TDataPool.GetData(idx: integer): TAbstractRDTPDAtaModule;
begin
  Lock;
  try
    result := TAbstractRDTPDAtaModule(FData[idx]);
  finally
    Unlock;
  end;
end;

{$IFDEF ALLOW_CONTROLS}
procedure TDataPool.SyncToListView(lv: TListView);
var
  t: integer;
  iCount: integer;
begin
  Lock;
  try
    iCount := count;

    with lv do begin
      while items.count > iCount do begin
        items.Delete(items.count-1);
      end;

      while items.count < iCount do begin
        items.Add;
      end;


      for t:= 0 to iCount-1 do begin
        items[t].caption := data[t].Context;
      end;
    end;
  finally
    Unlock;
  end;
end;
{$ENDIF}

function TDataPool.NeedData(cClass: TDataClass; sContext: string): TAbstractRDTPDataModule;
var
  t: integer;
  d: TAbstractRDTPDataModule;
begin
//  if sContext = '' then
//    raise EClassException.create('Blank context in NeedData');

  result := nil;
  Lock;
  try
    for t:= FData.count-1 downto 0 do begin
      d := FData[t];

      if d = nil then
        raise EClassException.create('nil data in Data Array found in TDataPool.NeedData');

      if d.IsExpired then begin
        d.free;
        FData.delete(t);
        d := nil;

        continue;

      end;

      if not assigned(result) then begin
        if (d.Context = sContext) and (d.classtype = cClass) then begin
          result := d;
          FData.delete(t);
          break;
        end;
      end;
    end;
    if result = nil then begin
      d := TAbstractRDTPDataModule(factory.CreateClass(TClass(cClass)));
      d.Context := sContext;
      result := d;

    end;

    result.LastUsed := GetTicker;


    inc(FActiveCount);
  finally
    Unlock;
  end;
end;

procedure TDataPool.NoNeedData(dm: TAbstractRDTPDataModule);
begin
  if dm= nil then
    exit;
  Lock;
  try
    dm.LastUsed := GetTicker;
    FData.add(dm);
    dec(FActiveCount);
  finally
    Unlock;
  end;

end;

procedure TDataPool.SetClassToCreate(const Value: TRDTPDataMOduleClass);
begin
  Lock;
  try
    FRDTPDAtaModuleClass := value;
  finally
    Unlock;
  end;

end;

destructor TRDTPExecutionThread.Destroy;
begin
  FDS.free;
  FDS := nil;
  inherited;
end;

procedure TRDTPExecutionThread.DoExecute;
begin
  inherited;

  try
    //make sure if dataset was not taken in last run
    //that it is freed
    if assigned(Fds) then begin
      FDs.free;
      FDs := nil;
    end;

    //execute
    FMod.ExecuteSystem(FQuery);

  except
    on E: Exception do begin
      Error := e.message;
    end;
  end;
end;
procedure TRDTPExecutionThread.ExecuteSystem(sQuery: string; bWaitWithKeepAlive: boolean = true);
var
  t: integer;
begin
  //TODO 5: TRDTPExecutionThread might want to support queries other than system queries, including queries that return datasets
  FQuery := sQuery;
  while not ready do sleep(1);
  Start;

  if bWaitWithKeepAlive then begin
    t := 0;
    while not Ready do begin
      //send back keep-alive every 4 seconds
      inc(t);
      t := t mod 400;
      if (t=0) then begin
        if assigned(Module.OnConnectionProgress) then begin
          try
            self.Module.OnConnectionProgress('','',-99,-99);
          except
          end;
        end;
      end;
      sleep(10);
    end;
  end;
end;


function TRDTPExecutionThread.GEtDSResult: TSERowSet;
begin
  Lock;
  try
    result := FDSREsult;
    FDSResult := nil; //<<-- called takes ownership
  finally
    Unlock;
  end;
end;

function TRDTPExecutionThread.GetMod: TabstractRDTPDataModule;
begin
  Lock;
  try
    result := FMod;
  finally
    Unlock;
  end;
end;

procedure TRDTPExecutionThread.SetMod(const Value: TabstractRDTPDataModule);
begin
  Lock;
  try
    FMod := value;
  finally
    Unlock;
  end;
end;

function TAbstractRDTPDataModule.GetConfigFile: string;
begin
  if copy(context, 1,1) = '[' then
    result := Context
  else
    result := LoadStringFromFile(slash(extractfilepath(DLLName))+'MWContext_'+Context+'.ini')
end;


{ TDataPoolViewer }




procedure oinit;
begin
//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure ofinal;
begin
//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

{ Tcmd_DBWriteBehind }

procedure Tcmd_DBWriteBehind.DoExecute;
begin
  inherited;
  dm.ExecuteWriteRaw(query);
end;

procedure Tcmd_DBWriteBehind.InitExpense;
begin
  inherited;
  CPuExpense := 0.0;
end;

initialization
init.RegisterProcs('AbstractRDTPDataModule', oinit, ofinal);

datapool := TDataPool.create;

finalization
datapool.free;

end.

