unit RDTP_FindModules;

{$IFNDEF ENABLE_RDTP_FINDMODULES}
  {$Message Error 'Use of this unit is dangerous,  define ENABLE_RDTP_FINDMODULES in project symbols to confirm usage'}
{$ENDIF}

interface

uses
  betterobject, typex, classes, managedthread, tickcount, commandprocessor, dir, dirfile, rdtpserverlist, sharedobject, stringx, commonconstants, sysutils, windows, generics.collections.fixed, systemx, orderlyinit, debug;


type
  TRDTPMOduleList = class;//forward

  Tthr_IdleWatch = class(TThread)
  private
    FModlist: TRDTPMOduleList;
  public
    procedure Execute;override;
    property modlist: TRDTPMOduleList read FModlist write FModlist;
  end;


  Tcmd_RDTPFindModulesUpgrade = class(TCommand)
  protected
    procedure DoExecute;override;
  public


  end;


  TRDTPModule = class(TSharedObject)
  private
    FHandle: THandle;
    FFileName: string;
  public
    constructor Create(owner: TRDTPModuleList);reintroduce;virtual;
    property Handle: THandle read FHandle write FHandle;
    function Load(sFile: string): boolean;
    procedure Unload;
    property FileName: string read FFileName;

  end;


  TRDTPModuleList = class(TSharedObject)
  protected
    FModules: TList<TRDTPModule>;
    FLAstVersionCheck: cardinal;
    FVersion: string;
    cmd: Tcmd_RDTPFindModulesUpgrade;
    thr: Tthr_IdleWatch;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure FindAndAggregateModules();overload;
    procedure FindAndAggregateModules(sDir: string);overload;
    procedure UnloadAllModules;
    function AlreadyLoaded(sModuleFileName: string): boolean;
    function IndexOfModule(sFullName: string): nativeint;
    procedure Upgrade;
    procedure BeginUpgrade;
    procedure ExtExecute(mt: TExternalEventThread);
    procedure MultiplexerIdleEvent;
    function LoadVersionFile: string;
    procedure StartIdleThread;
    procedure StopIdleThread;
  end;



var
  rdtp_modlist: TRDTPModuleList;

implementation

{ TRDTPModule }

constructor TRDTPModule.Create(owner: TRDTPModuleList);
begin
  inherited Create;

end;

function TRDTPModule.Load(sFile: string): boolean;
var
  p: pointer;
  regmod: TRegisterModulesProc;
begin
  result := false;
  FFileName := sFile;
  //load the library
  Debug.Log('******************************************************************');
  Debug.Log('******************************************************************');
  Debug.Log('******************************************************************');
  Debug.Log('******************************************************************');
  Debug.Log('******************************************************************');
  Debug.Log('Loading: '+sFile);
  Debug.Log('******************************************************************');
  Debug.Log('******************************************************************');
  Debug.Log('******************************************************************');
  Debug.Log('******************************************************************');

  //windows.Beep(600, 100);





  FHandle := LoadLibrary(pchar(sFile));

  p := GetProcAddress(FHandle, pchar('RegisterModules'));
  if p = nil then begin
    Unload;
    exit;
  end;

  regmod := p;
  regmod(RDTPServers);

  result := true;
  Debug.Log('^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^');
  Debug.Log('^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^');
  Debug.Log('Finished Loading:'+sFile);
  Debug.Log('^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^');
  Debug.Log('^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^');



end;

procedure TRDTPModule.Unload;
begin
  try
    FreeLibrary(FHandle);
  except
  end;
  FHandle := INVALID_HANDLE_VALUE;
end;





{ TRDTPModuleList }

function TRDTPModuleList.AlreadyLoaded(sModuleFileName: string): boolean;
begin
  result := IndexOfModule(sModuleFileName) >=0;
end;

procedure TRDTPModuleList.BeginUpgrade;
begin
  if assigned(cmd) then begin
    cmd.WaitFor;
    cmd.Free;
    cmd := nil;
  end;
  cmd := Tcmd_RDTPFindModulesUpgrade.Create;
  cmd.Start;

end;

constructor TRDTPModuleList.Create;
begin
  inherited;
  FModules := TList<TRDTPModule>.create;

  FLAstVersionCheck := GetTicker;
  FVersion := LoadVersionFile;
  StartIdleThread;
end;

destructor TRDTPModuleList.Destroy;
begin
  StopIdleThread;
  if assigned(cmd) then begin
    cmd.WaitFor;
    cmd.Free;
    cmd := nil;
  end;

  UnloadAllModules;
  FModules.Free;
  FModules := nil;
  inherited;
end;

procedure TRDTPModuleList.ExtExecute(mt: TExternalEventThread);
begin
  MultiplexerIdleEvent;
end;

procedure TRDTPModuleList.FindAndAggregateModules;
begin
  RDTP_MODLIST.FindAndAggregateModules(dllPath);
  RDTP_MODLIST.FindAndAggregateModules(dllPath+'..\');
end;

procedure TRDTPModuleList.FindAndAggregateModules(sDir: string);
var
  dir: TDirectory;
  fil: TFileInformation;
  module: TRDTPModule;
begin
  dir := TDirectory.Create(sDir, '*.dll', 0,0,true, false, false);
  try
    while dir.GetNextFile(fil) do begin
      if lowercase(copy(fil.Name, STRZ, 5)) = 'rdtpm' then begin
        if AlreadyLoaded(fil.fullname) then
          continue;

        module := TRDTPModule.Create(self);
        if module.Load(fil.FullName) then
          FModules.Add(module);
      end;
    end;

    WHILE dir.GetNextFolder(fil) do begin
      if lowercase(copy(fil.Name, STRZ, 5)) = 'rdtpm' then begin
        FindAndAggregateModules(fil.FullName);
      end;
    end;


  finally
    dir.Free;
  end;

end;

function TRDTPModuleList.IndexOfModule(sFullName: string): nativeint;
var
  t: integer;
begin
  result := -1;
  Lock;
  try
    for t := 0 to FModules.Count-1 do begin
      if (lowercase(FModules[t].FileName) = lowercase(sFullName)) then begin
        result := t;
        break;
      end;
    end;
  finally
    Unlock;
  end;

end;

function TRDTPModuleList.LoadVersionFile: string;
var
  sFile: string;
begin
  result := '';
  sFile := DLLPath+'Config\version_touch_me.txt';

  if fileexists(sFile) then
    result := LoadFileAsString(sFile);
end;

procedure TRDTPModuleList.MultiplexerIdleEvent;
var
  s: string;
begin
  inherited;

  if GetTimeSince(FLastVersionCheck) > 30000 then begin
    s := LoadVersionFile;
    if FVersion <> s then begin
      FVersion := s;
      BeginUpgrade;
    end;
    FLAstVersionCheck := GetTicker;
  end;




end;

procedure TRDTPModuleList.StartIdleThread;
begin
//  thr := TExternalEventThread.Create(self, nil, true);
//  thr.OnExecute := self.ExtExecute;
//  thr.Loop := true;
//  thr.Resume;

  thr := Tthr_IdleWatch.Create(true);
  thr.modlist := self;
  thr.FreeOnTerminate := false;
  thr.Start;




end;

procedure TRDTPModuleList.StopIdleThread;
begin
  if assigned(thr) then begin
    thr.Terminate;
    thr.WaitFor;
    thr.Free;
  end;
  thr := nil;

end;

procedure TRDTPModuleList.UnloadAllModules;
begin
  RDTPServers.Clear;
  Lock;
  try
    while FModules.Count > 0 do begin
      FModules[FModules.Count-1].Unload;
      FModules.Delete(FModules.Count-1);
    end;
  finally
    Unlock;
  end;
end;

procedure TRDTPModuleList.Upgrade;
begin
  //signal that we're upgrading
  RDTPServers.Upgrading  := true;
  try

    //The multiplexer processor code should begin release all its read-locks
    //on the list

    //get an exclusive lock
    Lock;
    try
      RDTPServers.Lockwrite;
      try
        //----------------------------------------
        //another thread triggered the upgrade?  Depends on how I
        //end up using it.
        //regardless we'll recheck the upgrading flag... it is possiple that
        //since it is set to FALSE inside the writelock
        //if it is FALSE now, then that reliably would indicate that
        //another thread just upgraded everyting and we can cancel this
        //operation
        if RDTPSErvers.Upgrading = false then
          exit;
        //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        self.UnloadAllModules;
        self.FindAndAggregateModules;
      finally
        RDTPServers.Upgrading := false;
        RDTPSErvers.Unlockwrite;

      end;
    finally
      Unlock;
    end;
    //processing should be free to resume.
  except
    RDTPServers.Upgrading := false;
    raise;
  end;


end;

{ Tcmd_RDTPFindModulesUpgrade }

procedure Tcmd_RDTPFindModulesUpgrade.DoExecute;
begin
  inherited;
  rdtp_modlist.Upgrade;
end;


{ Tthr_IdleWatch }


procedure Tthr_IdleWatch.Execute;
begin
  inherited;
  Debug.Log('Idle thread id:'+inttostr(getcurrentthreadid()));
  while not terminated do begin
    sleep(1000);
    if modlist <> nil then
      modlist.MultiplexerIdleEvent;
  end;
  Debug.Log('Idle thread terminated');
end;


procedure oinit;
begin
  rdtp_Modlist := TRDTPModuleList.Create;
  RDTP_MODLIST.FindAndAggregateModules();

end;

procedure ofinal;
begin
  rdtp_Modlist.Free;
  rdtp_Modlist := nil;


end;

initialization
  init.RegisterProcs('RDTP_FindModules', oinit, ofinal, 'dir,managedthread');

finalization


end.
