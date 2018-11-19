unit RDTPServerList;
{$include 'DelphiDefs.inc'}

interface


uses
  rdtpprocessor, sharedobject, betterobject, numbers, systemx, typex, classes, sysutils, debug, commandprocessor,orderlyinit;

type
  ERDTPMultiplexerException = exception;


  TRDTPServerList = class(TLockQueuedObject)
  strict private
    function GetCount: nativeint;
    function GEtServer(idx: nativeint): TRDTPProcessorClass;
    function GEtServiceName(idx: nativeint): string;
  strict protected
    FList: TStringlist;
    FClasses: array of TRDTPProcessorClass;

    function FindRDTPProcessor(sServiceName: ansistring): TRDTPProcessorClass;
  public
    [volatile]
    Upgrading: boolean;

    constructor create;override;
    destructor Destroy;override;
    procedure Clear;
    procedure RegisterRDTPProcessor(sServiceName: ansistring; p: TRDTPProcessorClass);
    property Count: nativeint read GetCount;
    property ServiceNames[idx: nativeint]: string read GEtServiceName;
    property Servers[idx: nativeint]: TRDTPProcessorClass read GetServer;
    function NeedRDTPProcessor(sServiceNAme: string): TRDTPProcessorCLass;
    procedure NoNeedRDTPProcessor(sServiceName: string);

  end;

  TRegisterModulesProc = function (mainmod: TRDTPServerList): boolean;

function RegisterModules(mainmod: TRDTPServerList): boolean;export;

exports RegisterModules;


var
  external_rdtp_server: boolean;
  RDTPServers: TRDTPServerList;


implementation


{ TRDTPServerList }

procedure TRDTPServerList.Clear;
begin
  inherited;
  LockWrite;
  try
    FList.Clear;
  finally
    UnlockWrite;
  end;
end;

constructor TRDTPServerList.Create;
begin
  inherited;
  FList := TStringList.create;
end;

destructor TRDTPServerList.Destroy;
begin
  FList.free;
  inherited;
end;

function TRDTPServerList.FindRDTPProcessor(
  sServiceName: ansistring): TRDTPProcessorClass;
var
  idx: integer;
begin
  result := nil;
  sServiceName := Lowercase(sServiceName);
  LockRead;
  try
    idx := FList.IndexOf(sServiceName);
    if idx < 0 then
      raise ERDTPMultiplexerException.create('service was not registered ('+sServiceName+')');

    result := TRDTPProcessorClass(FClasses[idx]);

  finally
    UnlockRead;
  end;

end;

function TRDTPServerList.GetCount: nativeint;
begin
  LockRead;
  try
    result := FList.Count;
  finally
    UnlockRead;
  end;
end;

function TRDTPServerList.GEtServer(idx: nativeint): TRDTPProcessorClass;
begin
  LockREad;
  try
    result := TRDTPProcessorClass(FList.objects[idx]);
  finally
    UnlockREad;
  end;
end;

function TRDTPServerList.GEtServiceName(idx: nativeint): string;
begin
  LockRead;
  try
    result := FList[idx];
  finally
    UnlockRead;
  end;
end;

function TRDTPServerList.NeedRDTPProcessor(
  sServiceNAme: string): TRDTPProcessorCLass;
//gets RDTP processor and tallys a read long on the list so that
//upgrades will not occur until the service is no longer needed
begin
  result := FindRDTPProcessor(sServiceName);
  LockRead;
end;

procedure TRDTPServerList.NoNeedRDTPProcessor(sServiceName: string);
begin
  UnLockRead;
end;

procedure TRDTPServerList.RegisterRDTPProcessor(sServiceName: ansistring;
  p: TRDTPProcessorClass);
begin
  LockWrite;
  try
    FList.addObject(lowercase(sServiceName), TObject(pointer(p)));
    SetLength(FClasses, FList.Count);
    FClasses[FList.count-1] := p;
    Debug.Log(self,DLLName+' Registered service '+sServiceName);
  finally
    Unlockwrite;
  end;
end;



function RegisterModules(mainmod: TRDTPServerList): boolean;export;
var
  t: nativeint;
begin
  result := false;
  try
    MainMod.LockWrite;
    try
      Debug.Log(nil,'Registering '+inttostr(RDTPServers.count)+' RDTP modules from DLL');
      for t:= 0 to RDTPServers.Count-1 do begin
        mainmod.RegisterRDTPProcessor(RDTPServers.ServiceNames[t], RDTPServers.Servers[t]);
      end;
      RDTPServers.Free;
      external_rdtp_server := true;
      RDTPServers := mainmod;
    finally
      MainMod.UnlockWrite;
    end;

  except
    result := false;
  end;

end;


{$IFDEF CPUX86}
  {$IFNDEF ALLOW_ON_x86}
    {$Message Error 'For safety reasons, 32-bit compilation no longer supported for RDTP Servers unless ALLOW_ON_X86 is defined'}
  {$ENDIF}
{$ENDIF}



{ Tcmd_RDTPServerListUpgrade }


procedure oinit;
begin
RDTPServers := TRDTPServerList.create;
end;

procedure ofinal;
begin
if not external_rdtp_server then
  RDTPServers.free;

end;

initialization
  init.RegisterProcs('RDTPServerList', oinit, ofinal);


finalization


end.
