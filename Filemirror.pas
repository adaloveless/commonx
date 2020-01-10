unit Filemirror;

interface
uses ioutils, debug, classes, filectrl, managedthread, threadmanager, commandprocessor, commands_file, systemx, typex, dir, dirfile, fileex, filefind, tickcount;


const
  MAX_FILES = 10000;
type

{$DEFINE ENABLE_THREADED_DIRS}

  TMirrorMode = (mmCollect, mmreplicate, mmMerge);

  TFileReplicatorState = (frWait, frRun, frDone);

  
  TFileReplicator = class(TCommand)
  private
    FFileSpec: string;
    FTrashDir: string;
    FTarget: string;
    FSource: string;
    FStatus: string;
    FMessages: string;
    FMultithread: boolean;
    FonProgress: TNotifyevent;

    FExcludeList: TStringlist;
    FMirrorMode: Tmirrormode;
    FNoRecursion: boolean;
    Fstate: TFileReplicatorState;
    FCP: TcommandProcessor;
    FAI: boolean;
    FLatency: cardinal;
    FCommand: TCommand;
    function GetMirrorMode: Tmirrormode;
    function GetMultithread: boolean;
    function GetNoRecursion: boolean;
    function GetOnProgress: TNotifyevent;
    function GetSource: string;
    function GetTarget: string;
    function GetTrashDir: string;
    procedure SetMirrorMode(const Value: Tmirrormode);
    procedure SetMultithread(const Value: boolean);
    procedure SetNoRecursion(const Value: boolean);
    procedure SetOnProgress(const Value: TNotifyevent);
    procedure SetSource(const Value: string);

    procedure SetTarget(const Value: string);
    procedure SetTreashDir(const Value: string);
    function GetState: TFileReplicatorState;
    procedure SetState(const Value: TFileReplicatorState);
    function GetFileSpec: string;
    procedure SetFileSpec(const Value: string);
    function GetMessages: string;
    procedure CopyFile2(sSource, sTarget: string; iSize: int64);
    procedure MoveFile2(sSource, sTarget: string; iSize: int64);
    function slashXX(sPath: string): string;
  protected

    procedure TrashFolder(sdir: string; sTrashDir: string);
    procedure TrashFile(sFullName: string; sTrashDir: string);
    function AdjustPathXX(sPath: string): string;
    procedure MoveFiles(sSourceDir, sTargetDir: string; bRoot: boolean);

    procedure Replicate(sSourceBaseDir, sTargetBaseDir, sDir1, sDir2: string; sTrashDir: string);
    procedure Merge(sSourceBaseDir, sTargetBaseDir, sDir1, sDir2: string);
    //procedure Replicate(sTargetBaseDir, sDir1, sDir2: string; sTrashDir: string); overload;
    procedure UpdateStatus(s: string);
    procedure MoveFile(sSource, sDest: string; bOverwrite : boolean = true);
  public
    destructor Destroy;override;

    procedure Start(sFileSpec: string = '*.*'; bWait:boolean = true);reintroduce;virtual;
    //procedure Start(sFileSpec: string = '*.*'); overload;
    procedure Collect(sDir1, sdir2: string; sSourceBaseDir, sTargetBaseDir: string; sFileSpec: string = '*.*');

    procedure DoExecute; override;


    property FileSpec: string read GetFileSpec write SetFileSpec;
    property State: TFileReplicatorState read GetState write SetState;
    property SourceDir: string read GetSource write SetSource;
    property TargetDir: string read GetTarget write SetTarget;
    property TrashDir: string read GetTrashDir write SetTreashDir;

    property Status: string read GetStatus write SetStatus;
    property MultiThread: boolean read GetMultithread write SetMultithread;
    property OnProgress: TNotifyevent read GetOnProgress write SetOnProgress;

    procedure ClearExcludes;
    procedure Exclude(sExt: string);
    function IsExcluded(sExt: string): boolean;

    property MirrorMode :Tmirrormode read GetMirrorMode write SetMirrorMode;
    property NoRecursion: boolean read GetNoRecursion write SetNoRecursion;

    property Messages: string read GetMessages;
    procedure AddMessage(s: string);

    property CP: TcommandProcessor read FCP;

    property AI: boolean read FAI write FAI;

    procedure TestLatency;
    property Latency: cardinal read FLatency write FLatency;
    property Command: TCommand read FCommand write Fcommand;

    procedure Init;override;
  end;


//procedure CopyFile2(sSource, sTarget: string);




implementation

uses windows, sysutils;

procedure TFileReplicator.MoveFiles(sSourceDir, sTargetDir: string; bRoot: boolean);
var
  dir: TDirectory;
  t: integer;
  fa: TFileAttributes;
begin
  dir := TDirectory.create(sSourceDir, '*.*', 0,0,multithread);
  try
    for t:= 0 to dir.Filecount-1 do begin
      if IsCancelled then exit;
      if (TFileAttribute.faReadOnly in (dir.files[t].attributes)) then
        fa := dir.files[t].Attributes;
        system.Exclude(fa, TFileAttribute.faReadOnly);
        dir.files[t].Attributes := fa;
      self.MoveFile2(dir.Files[t].FullName, slash(sTargetDir)+dir.Files[t].Name,dir.Files[t].Size);
      if (not bRoot) and ((GetFolderSize(sSourceDir, true)) = 0) then
        DeleteFolder(sSourceDir);

    end;
  finally

  end;
end;

procedure TFileReplicator.Replicate(sSourceBaseDir, sTargetBaseDir, sDir1, sDir2: string; sTrashDir: string);
var
  dir1: TDirectory;
  dir2: TDirectory;
  dir3tmp: TDirectory;
  fi1, fi2: TFileInformation;
  t: integer;
  sLastStatus: string;
  sNewStatus: string;
  bISRoot: boolean;
begin
  sSourceBaseDir := slash(sSourceBaseDir);
  sTargetBaseDir := slash(sTargetBaseDir);
  sdir1 := slash(sdir1);
  sdir2 := slash(sdir2);
  if (sDir1 = sSourceBaseDir) then begin
    self.TestLatency;
    self.TestLatency;
  end;

  while filecommands.Commandcount > MAX_FILES do begin
    if assigned(COMMAND) then Command.WaitingForResources := true;
    SleepEx(400,false);
  end;

  if assigned(COMMAND) then Command.WaitingForResources := false;



  bISRoot := sSourceBaseDir = sDir1;
  sLastStatus := '';
  try
    ForceDirectories(sDir1);
  except
    on E: Exception do begin
      self.AddMessage(E.MEssage);
    end;
  end;
  try
    ForceDirectories(sDir2);
  except
    on E: Exception do begin
      self.AddMessage(E.MEssage);
    end;
  end;

  updateStatus('Scanning 0% '+sDir1+' -> '+sDir2);

  dir1 := TDirectory.create(sDir1,FileSpec,0,0, multithread);
  try
    dir2:= TDirectory.create(sDir2,FileSpec,0,0, multithread);
    try
      //cycle through all files in dir1
      for t:= 0 to dir1.Filecount-1 do begin
        if IsCancelled then exit;

        if bIsRoot then begin
          self.volatile_progress.stepcount := dir1.Filecount * dir1.foldercount;
          self.volatile_progress.step := t;
        end;
        sNewStatus := inttostr(Round((t/dir1.FileCount*100)/2)+0)+'%';
        if sNewStatus <> sLastStatus then begin
          updateStatus('Scanning '+sNewStatus+' '+sDir1+' -> '+sDir2);
          sLastStatus := sNewStatus;
        end;

        if IsExcluded(dir1.Files[t].Extension) then
          continue;

        //if dir2 has the file then compare the dates
        fi1 := dir1.Files[t];
        if dir2.HasFile(fi1.Name) then begin

          fi2 := dir2.Files[dir2.IndexOfByname(fi1.Name)];

          //if 1<>2 then copy 1->2
          try
            fi1.date;
          except
            on E: Exception do begin
              self.AddMessage(E.MEssage);
            end;

          end;
          try
            fi2.date;
          except
            on E: Exception do begin
              self.AddMessage(E.MEssage);
            end;

          end;

          if fi1.Date <> fi2.Date then begin
            try
              UpdateStatus('Refreshing '+fi1.FullName+' -> '+fi2.FullName);
              AddMessage('Refreshing '+fi1.FullName+' -> '+fi2.FullName);
              Copyfile2(Pchar(fi1.FullName), Pchar(fi2.FullName), fi1.Size);
            except
              on E: Exception do begin
                self.AddMessage(E.MEssage);
              end;
            end;
          end;

        end else begin
          //create file in 2
            try
              UpdateStatus('Creating '+fi1.FullName+' -> '+slash(sdir2)+fi1.Name);
              AddMessage('Creating '+fi1.FullName+' -> '+slash(sdir2)+fi1.Name);
              Copyfile2(Pchar(fi1.FullName), Pchar(slash(sdir2)+fi1.Name), fi1.Size);
            except
              on E: Exception do begin
                self.AddMessage(E.MEssage);
              end;
            end;
        end;
      end;

      //swap pointers and do again
      dir3tmp := dir1;
      dir1  := dir2;
      dir2 := dir3tmp;

      for t:= 0 to dir1.Filecount-1 do begin
        if IsCancelled then exit;
        if IsExcluded(dir1.Files[t].Extension) then
          continue;

        sNewStatus := inttostr(Round((t/dir1.FileCount*100)/2)+50)+'%';
        if sNewStatus <> sLastStatus then begin
          updateStatus('Scanning '+sNewStatus+' '+sDir1+' -> '+sDir2);
          sLastStatus := sNewStatus;
        end;

        //if dir2 has the file then compare the dates
        fi1 := dir1.Files[t];
        if NOT dir2.HasFile(fi1.Name) then begin
          //delete file in 2
          try
            UpdateStatus('Deleting '+fi1.FullName+' -> '+sTrashDir);
            AddMessage('Deleting '+fi1.FullName+' -> '+sTrashDir);
            Trashfile(fi1.FullName, sTrashdir);
          except
            on E: Exception do begin
              self.AddMessage(E.MEssage);
            end;
          end;
        end;
      end;

      //swap pointers and do again
      dir3tmp := dir1;
      dir1  := dir2;
      dir2 := dir3tmp;

      //for every folder in dir 1 replicate
      sSourceBaseDir := slash(sSourceBaseDir);
      for t:= 0 to dir1.FolderCount-1 do begin
        if lowercase(dir1.Folders[t].name) = 'dfsrprivate' then
          continue;
        if IsCancelled then exit;
        if bIsRoot then begin
          self.volatile_progress.Stepcount := dir1.Filecount * dir1.foldercount;
          self.volatile_progress.Step := (t+1) * dir1.Filecount;
        end;
        Replicate(sSourceBaseDir, sTargetBaseDir, slash(sDir1)+dir1.folders[t].Name, slash(slash(sTargetBaseDir)+copy(sDir1, length(sSourceBaseDir)+1, length(sDir1)))+dir1.folders[t].Name, sTrashdir);
      end;

      //for every folder in dir 2 that is NOT in Dir 1 -- trash
      sSourceBaseDir := slash(sSourceBaseDir);
      for t:= 0 to dir2.FolderCount-1 do begin
        if lowercase(dir2.Folders[t].name) = 'dfsrprivate' then
          continue;
        if not dir1.HasFolder(dir2.Folders[t].Name) then begin
          UpdateStatus('Trashing Folder '+dir2.Folders[t].fullName);
          AddMessage('Trashing Folder '+dir2.Folders[t].fullName);
          TrashFolder(dir2.Folders[t].fullName, sTrashDir);
        end;
      end;
    finally
      dir2.free;
    end;
  finally
    dir1.Free;

    if (sSourceBaseDir = sdir1) and (sTargetBaseDir = sDir2) then begin

      while not CP.IsComplete do begin
        self.Status := 'Waiting for final copies...'+inttostr(round(CP.PercentComplete*100))+'%';
        self.volatile_progress.stepcount := 100;
        self.volatile_progress.step := round(CP.PercentComplete*100);
        sleep(cp.RecommendedSleepTime);

        cp.RollupCompletedCommands;
      end;
      CP.WaitForAll;
      CP.Clear;
    end;

    //call this same function for every sub folder
//    windows.Beep(50,50)


  end;
end;


procedure TFileReplicator.TestLatency;
var
  tmStart, tmSource, tmTarget: cardinal;
  tmDif: int64;
  scope: TfindScope;
  rec: TSearchRec;
  t: integer;
begin
  try

    Debug.Log('Begin Latency Test: '+SourceDir+'->'+TargetDir, 'filecopy');
    //source
    ResetEasyfind(scope);
    EasyFindFile(scope, SourceDir+'*.*', 0,0, rec);
    FindClose(rec);


    tmStart := GetTickCount;
    for t:= 0 to 1000 do begin
      ResetEasyfind(scope);
      EasyFindFile(scope, SourceDir+'*.*', 0,0, rec);
      FindClose(rec);
    end;
    tmDif :=  GetTimeSince(tmstart);


    tmSource := tmDif;

    ResetEasyfind(scope);
    EasyFindFile(scope, TargetDir+'*.*', 0,0, rec);
    FindClose(rec);

    tmStart := GetTickCount;
    for t:= 0 to 1000 do begin
      ResetEasyfind(scope);
      EasyFindFile(scope, TargetDir+'*.*', 0,0, rec);
      FindClose(rec);
    end;
    tmDif :=  GetTimeSince(tmstart);


    tmTarget := tmDif;

    tmDif := tmTarget+tmSource;
    Debug.Log('End Latency Test: '+floattostr(tmDif), 'filecopy');
    tmDif := tmDif - 32;
    if tmdif < 0 then tmDif := 0;
    tmDif := tmDif div 10;
    Debug.Log('Local Compare - 32: '+floattostr(tmDif), 'filecopy');

    if (tmDif) <> Latency then begin
      Latency := tmDif;
      Debug.Log('Latency Set '+floattostr(Latency), 'filecopy');
    end


  except
    Latency := 0;
  end;



end;

procedure TFileReplicator.TrashFile(sFullName: string; sTrashDir: string);
var
  sName: string;
begin
  try
    //rip off stupid shit
    if copy(sFullName, 1, 2) = '\\' then
      sName := copy(sFullName, 3,length(sFullName))
    else
    if copy(sFullName, 2, 2) = ':\' then
      sName := copy(sFullName, 4,length(sFullName));

    sName := slash(sTrashDir)+sName;


    ForceDirectories(ExtractFilePath(sName));
    DeleteFile(sName);
    MoveFile(sFullName, sName);
    DeleteFile(sFullName);
  except
    on E: Exception do begin
      self.AddMessage(E.MEssage);
    end;
  end;
end;

procedure TFileReplicator.TrashFolder(sdir: string; sTrashDir: string);
var
  sName: string;
  dir: TDirectory;
  t: integer;
begin
  try
    //rip off stupid shit
    if copy(sdir, 1, 2) = '\\' then
      sName := copy(sdir, 3,length(sdir))
    else
    if copy(sdir, 2, 2) = ':\' then
      sName := copy(sdir, 4,length(sdir));

    sName := slash(sTrashDir)+StringReplace(sName, '$', '_dollar_', [rfREplaceAll]);

    //force the trash folder to exist
    ForceDirectories(sName);

    //move files
    MoveFiles(sdir, sName, true);

    //trash all subfolders
    dir := TDirectory.create(sDir, '*.*', 0,0,multithread);
    try
      for t:= 0 to dir.foldercount-1 do begin
        TrashFolder(dir.Folders[t].FullName, sTrashdir);
      end;
    finally
      dir.free;
    end;

    RemoveDirectory(PChar(sdir));

  except
    on E: Exception do begin
      self.AddMessage(E.MEssage);
    end;
  end;

end;

function TFileReplicator.slashXX(sPath: string): string;
begin
  result := sPath;

  if sPath = '' then begin
    exit;
  end;

  if copy(sPath, length(sPath), 1) <> '\' then
    result := sPath+'\';


end;



procedure TfileReplicator.Start(sFileSpec: string = '*.*'; bWait:boolean = true);
begin
  FileSpec := sFileSpec;
  self.State := frRun;
  inherited Start();
  if bWait then begin
    while self.State = frRun do begin
        sleep(cp.RecommendedSleepTime);
    end;
  end;
end;

procedure TFileReplicator.UpdateStatus(s: string);
begin
  status := s;
  if Assigned(OnProgress) then begin
    Lock;
    try
      FOnProgress(self);
    finally
      Unlock;
    end;
  end;
end;

procedure TFileReplicator.Collect(sDir1, sdir2: string; sSourceBaseDir, sTargetBaseDir: string; sFileSpec: string = '*.*');
var
  dir1: TDirectory;
  dir2: TDirectory;
  fi1: TFileInformation;
  t: integer;
  sLastStatus: string;
  sNewStatus: string;
  fa: TFileAttributes;
  tmpSrc, tmpDst: string;
begin

  while filecommands.Commandcount > MAX_FILES do begin
    if assigned(COMMAND) then Command.WaitingForResources := true;
    SleepEx(4000,false);
  end;
  if assigned(COMMAND) then Command.WaitingForResources := false;



  sSourceBaseDir := slash(sSourceBaseDir);
  sTargetBaseDir := slash(sTargetBaseDir);
  sdir1 := slash(sdir1);
  sdir2 := slash(sdir2);
  if lowercase(sdir2) = lowercase(sTargetBaseDir) then begin
    self.TestLatency;
    self.TestLatency;
  end;

  try
    sLastStatus := '';
    try
      ForceDirectories(sDir1);


    except;
      self.Status := 'Source May be unreachable';
      exit;
    end;
    try
      ForceDirectories(sDir2);
    except
      self.Status := 'Destination May be unreachable';
      exit;
    end;

    if not DirectoryExists(sDir1) then begin
      self.Status := 'Source May be unreachable';
      exit;
    end;

    if not DirectoryExists(sDir2) then begin
      self.Status := 'Target May be unreachable';
      exit;
    end;



    updateStatus('Scanning 0% '+sDir1+' -> '+sDir2);

    dir1 := TDirectory.create(sDir1,sFileSpec,0,0, multithread);
    try
      dir2:= TDirectory.create(sDir2,sFileSpec,0,0, multithread);
      try
        AddMessage('scan for collect '+sDir1+' '+sFileSpec+'->'+sDir2);
        //cycle through all files in dir1
        for t:= 0 to dir1.Filecount-1 do begin
          fi1 := dir1.files[t];


          sNewStatus := inttostr(Round((t/dir1.FileCount*100)/2)+0)+'%';
          if sNewStatus <> sLastStatus then begin
            updateStatus('Scanning '+sNewStatus+' '+sDir1+' -> '+sDir2);
            sLastStatus := sNewStatus;
          end;

          //create file in 2
          try
            if not IsExcluded(fi1.Extension) then begin
              UpdateStatus('Collecting '+fi1.FullName+' -> '+slash(sdir2)+fi1.Name);
              AddMessage('Collecting '+fi1.FullName+' -> '+slash(sdir2)+fi1.Name);
              if (TFileAttribute.faReadonly in fi1.attributes) then begin
                fa := fi1.Attributes;
                system.exclude(fa, TFileAttribute.faReadonly);
                fi1.Attributes := fa;
              end;
              Movefile2(fi1.FullName, slash(sdir2)+fi1.Name, fi1.Size);

            end;
          except
            on E:Exception do begin
              UpdateStatus(e.Message);
            end;
          end;
        end;

        //for every folder in dir 1 replicate
        sSourceBaseDir := slash(sSourceBaseDir);
        if not NoRecursion then begin
          for t:= 0 to dir1.FolderCount-1 do begin
            tmpSrc := slash(sDir1)+dir1.folders[t].Name;
            tmpDst := slash(slash(sTargetBaseDir)+copy(sDir1, length(sSourceBaseDir)+1, length(sDir1)))+dir1.folders[t].Name;
            Collect(tmpSrc, tmpDst, sSourceBaseDir, sTargetBaseDir);
            if (GetFolderSize(tmpSrc) = 0) then begin
              DeleteFolder(tmpSrc);
            end;
          end;
        end;



      finally
        dir2.free;
      end;
    finally
      dir1.Free;
    end;
  except
    on E:Exception do begin
      UpdateStatus(e.Message);
    end;
  end;

end;

function TFileReplicator.AdjustPathXX(sPath: string): string;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TFileReplicator.ClearExcludes;
begin
  FExcludeList.clear;

end;

procedure TFileReplicator.Exclude(sExt: string);
begin
  FExcludeList.Add('.'+sExt);
end;

procedure TFileReplicator.Init;
begin
  inherited;
  fState := FRWait;
  FTrashDir := '';
  FTarget := '';
  FSource := '';
  volatile_progress.StepCount := 100;
  FStatus := '';
  Multithread  := true;
  FOnProgress := nil;
  FExcludeList := TStringList.create;
  FmirrorMode := mmreplicate;
  fnORECURsion := false;
  FCP := TCommandProcessor.create(nil, 'FileReplicatorCommands');
  FAI := true;

end;

function TFileReplicator.IsExcluded(sExt: string): boolean;
begin
  FexcludeList.CaseSensitive := false;
  result := FExcludeList.IndexOf(sExt)>=0;
end;

destructor TFileReplicator.destroy;
begin
  FCP.free;
  FExcludelist.free;
  inherited;
end;

procedure TFileReplicator.MoveFile(sSource, sDest: string;
  bOverwrite: boolean);
begin
  if fileExists(sDest) then begin
    FileSetAttr(Pchar(sDest), FileGetAttr(Pchar(sDest)) and not faReadOnly)
  end;

  windows.DeleteFile(PChar(sDest));

  if not windows.MoveFile(Pchar(sSource) , Pchar(sDest)) then
    raise Exception.create('Error when moving file code:'+inttostr(GetLastError));


end;

procedure TFileReplicator.DoExecute;
begin
  inherited;

  State := frWait;

  //do the stuff
  case FMirrorMode of
    mmMerge:
      self.Merge(Sourcedir, TargetDir, Sourcedir, targetdir);
    mmreplicate:
      begin
        self.Replicate(Sourcedir, TargetDir, Sourcedir, targetdir, trashdir);
      end;
    mmCollect:
      self.Collect(SourceDir, Targetdir, SourceDir, TargetDir, FileSpec);
  end;

  //done
  self.volatile_progress.Step := self.volatile_progress.Stepcount;
  self.Status := 'done';
  State := frDone;






end;

function TFileReplicator.GetMirrorMode: Tmirrormode;
begin
  Lock;
  try
    result := FMirrorMode;
  finally
    Unlock;
  end;
end;

function TFileReplicator.GetMultithread: boolean;
begin
  Lock;
  try
    result := FMultithread;
  finally
    Unlock;
  end;
end;

function TFileReplicator.GetNoRecursion: boolean;
begin
  Lock;
  try
    result := FNoRecursion;
  finally
    Unlock;
  end;
end;

function TFileReplicator.GetOnProgress: TNotifyevent;
begin
  Lock;
  try
    result := FOnProgress;
  finally
    Unlock;
  end;
end;


function TFileReplicator.GetSource: string;
begin
  Lock;
  try
    result := FSource;
    UniqueString(result);
  finally
    Unlock;
  end;
end;


function TFileReplicator.GetTarget: string;
begin
  Lock;
  try
    result := FTarget;
    UniqueString(result);
  finally
    Unlock;
  end;
end;

function TFileReplicator.GetTrashDir: string;
begin
  Lock;
  try
    result := FTrashDir;
    UniqueString(result);
  finally
    Unlock;
  end;
end;

procedure TFileReplicator.SetMirrorMode(const Value: Tmirrormode);
begin
  Lock;
  try
    FMirrorMode := Value;
  finally
    Unlock;
  end;
end;

procedure TFileReplicator.SetMultithread(const Value: boolean);
begin
  Lock;
  try
{$IFDEF ENABLE_THREADED_DIRS}
    FMultithread := value;
{$ELSE}
    FMultithread := false;
{$ENDIF}
  finally
    Unlock;
  end;
end;

procedure TFileReplicator.SetNoRecursion(const Value: boolean);
begin
  Lock;
  try
    FNoRecursion := value;
  finally
    Unlock;
  end;
end;

procedure TFileReplicator.SetOnProgress(const Value: TNotifyevent);
begin
  FOnProgress := value;
end;


procedure TFileReplicator.SetSource(const Value: string);
begin
  Lock;
  try
    FSource := value;
    UniqueString(FSource);
  finally
    Unlock;
  end;
end;


procedure TFileReplicator.SetTarget(const Value: string);
begin
  Lock;
  try
    Ftarget := value;
    UniqueString(Ftarget);
  finally
    Unlock;
  end;
end;

procedure TFileReplicator.SetTreashDir(const Value: string);
begin
  Lock;
  try
    FTrashDir := value;
    UniqueString(FTrashDir);
  finally
    Unlock;
  end;
end;

function TFileReplicator.GetState: TFileReplicatorState;
begin
  Lock;
  try
    result := Fstate;
  finally
    Unlock;
  end;
end;

procedure TFileReplicator.SetState(const Value: TFileReplicatorState);
begin
  Lock;
  try
    Fstate := value;
  finally
    Unlock;
  end;
end;

function TFileReplicator.GetFileSpec: string;
begin
  Lock;
  try
    result := FFileSpec;
    UniqueString(result);
  finally
    Unlock;
  end;

end;

procedure TFileReplicator.SetFileSpec(const Value: string);
begin
  Lock;
  try
    FFileSpec := value;
    UniqueString(FFileSpec);
  finally
    Unlock;
  end;

end;

procedure TFileReplicator.AddMessage(s: string);
begin
  lock;
  try
    Debug.Log(s,'filemirror');
  finally
    unlock;
  end;
end;

function TFileReplicator.GetMessages: string;
begin
  lock;
  try
    result := FMessages;
    UniqueString(result);
    FMessages := '';
  finally
    unlock
  end;
end;

procedure TFileReplicator.Merge(sSourceBaseDir, sTargetBaseDir, sDir1,
  sDir2: string);
var
  dir1: TDirectory;
  dir2: TDirectory;
//  dir3tmp: TDirectory;
  fi1, fi2: TFileInformation;
  t: integer;
  sLastStatus: string;
  sNewStatus: string;
  dt1, dt2: TDateTime;
  bISRoot: boolean;
  bChange: boolean;
  fa: TFileAttributes;
begin
  Status := 'Merging';
  sSourceBaseDir := slash(sSourceBaseDir);
  sTargetBaseDir := slash(sTargetBaseDir);
  sdir1 := slash(sdir1);
  sdir2 := slash(sdir2);
  if (sSourceBasedir=sDir1) then begin
    Status := 'Latency Test 1';
    self.TestLatency;
    Status := 'Latency Test 2';
    self.TestLatency;
  end;

  while filecommands.Commandcount > MAX_FILES do begin
    if assigned(COMMAND) then Command.WaitingForResources := true;
    SleepEx(4000,false);
  end;
  if assigned(COMMAND) then Command.WaitingForResources := false;


  bISRoot := sSourceBaseDir = sDir1;
  try
    sLastStatus := '';
    try
      ForceDirectories(sDir1);
    except
      on E: Exception do begin
        self.AddMessage(E.MEssage);
      end;
    end;
    try
      ForceDirectories(sDir2);
    except
      on E: Exception do begin
        self.AddMessage(E.MEssage);
      end;
    end;

    updateStatus('Scanning 0% '+sDir1+' -> '+sDir2);

    dir1 := TDirectory.create(sDir1,FileSpec,0,0, multithread);
    try
      dir2:= TDirectory.create(sDir2,FileSpec,0,0, multithread);
      try
        //cycle through all files in dir1
        //AddMessage('At start found: '+inttostr(dir1.Filecount)+' and '+inttostr(dir2.Filecount)+' files.');
        for t:= 0 to dir1.Filecount-1 do begin
          if IsCancelled then exit;
          if bIsRoot then begin
            self.volatile_progress.StepCount := dir1.Filecount * dir1.foldercount;
            self.volatile_progress.Step := t;
          end;
          sNewStatus := inttostr(Round((t/dir1.FileCount*100)/2)+0)+'%';
          if sNewStatus <> sLastStatus then begin
            updateStatus('Scanning '+sNewStatus+' '+sDir1+' -> '+sDir2);
            sLastStatus := sNewStatus;
          end;


          if IsExcluded(dir1.Files[t].Extension) then
            continue;


          //if dir2 has the file then compare the dates
          fi1 := dir1.Files[t];
          if dir2.HasFile(fi1.Name) then begin
            fi2 := dir2.Files[dir2.IndexOfByname(fi1.Name)];


            //get dates
            try
              fi1.date;
            except
              on E: Exception do begin
                self.AddMessage(E.MEssage);
                now;
              end;

            end;
            try
              fi2.date;
            except
              on E: Exception do begin
                self.AddMessage(E.MEssage);
                now;
              end;

            end;

            //if 1>2 then copy 1->2
            if fi1.Date > fi2.Date then begin
              try
                if lowercase(fi1.Extension) <> 'dcu' then begin
                  UpdateStatus('Pushing '+fi1.FullName+' -> '+fi2.FullName);
                  AddMessage('Pushing '+fi1.FullName+' -> '+fi2.FullName);
                end;
                if (TFileAttribute.faReadOnly in fi2.attributes) then begin
                  fa := fi2.Attributes;
                  system.exclude(fa, TFileAttribute.faReadonly);
                  fi2.Attributes := fa;
                end;
                Copyfile2(Pchar(fi1.FullName), Pchar(fi2.FullName), fi1.Size);
                fi2.Attributes := fi1.Attributes;
              except
                on E: Exception do begin
                  self.AddMessage(E.MEssage);
                end;
              end;
            end;

            //if 1<2 then copy 1->2
            if fi1.Date < fi2.Date then begin
              try
                if lowercase(fi1.Extension) <> 'dcu' then begin
                  UpdateStatus('Pulling '+fi1.FullName+' <- '+fi2.FullName);
                    AddMessage('Pulling '+fi1.FullName+' <- '+fi2.FullName);
                end;

                bChange := faLSE;
                if (TFileAttribute.faReadOnly in fi1.attributes) then begin
                  fa := fi1.Attributes;
                  system.exclude(fa, TFileAttribute.faReadonly);
                  fi1.Attributes := fa;
                  bChange := true;
                end;
                Copyfile2(Pchar(fi2.FullName), Pchar(fi1.FullName), fi2.Size);

                fi1.Attributes := fi2.Attributes;
              except
                on E: Exception do begin
                  self.AddMessage(E.MEssage);
                end;
              end;
            end;

          end else begin
            //create file in 2
              try
                if (lowercase(fi1.Extension) <> 'dcu') then begin
                  UpdateStatus('Pushing '+fi1.FullName+' -> '+slash(sdir2)+fi1.Name);
                  AddMessage('Pushing '+fi1.FullName+' -> '+slash(sdir2)+fi1.Name);
                end;
                Copyfile2(Pchar(fi1.FullName), Pchar(slash(sdir2)+fi1.Name), fi1.Size);
              except
                on E: Exception do begin
                  self.AddMessage(E.MEssage);
                end;
              end;
          end;
        end;

        for t:= 0 to dir2.Filecount-1 do begin
          if IsCancelled then exit;
          if dir1.FileCount > 0 then begin
            sNewStatus := inttostr(Round((t/dir1.FileCount*100)/2)+50)+'%';
          end else
            sNewStatus := '0%';
          if sNewStatus <> sLastStatus then begin
            updateStatus('Scanning '+sNewStatus+' '+sDir1+' -> '+sDir2);
            sLastStatus := sNewStatus;
          end;

          if IsExcluded(dir2.Files[t].Extension) then
            continue;


          fi2 := dir2.Files[t];
          if NOT dir1.HasFile(fi2.Name) then begin
            //create file in 2
            try
              if lowercase(fi2.Extension) <> 'dcu' then begin
                UpdateStatus('Pulling '+slash(sdir1)+fi2.Name+' <- '+fi2.FullName);
                AddMessage('Pulling '+slash(sdir1)+fi2.Name+' <- '+fi2.FullName);
              end;
              Copyfile2(Pchar(fi2.FullName), Pchar(slash(sdir1)+fi2.Name), fi2.Size);
            except
              on E: Exception do begin
                self.AddMessage(E.MEssage);
              end;
            end;
          end;
        end;


        //for every folder in dir 2 that is NOT in Dir replicate
        sSourceBaseDir := slash(sSourceBaseDir);
        for t:= 0 to dir2.FolderCount-1 do begin
          if lowercase(dir2.Folders[t].name) = 'dfsrprivate' then
            continue;
          if IsCancelled then exit;
          if bIsRoot then begin
            self.volatile_progress.StepCount := dir2.Filecount * dir2.foldercount;
            self.volatile_progress.Step := ((t+1) * dir2.Filecount div 2);
          end;
          if not dir1.HasFolder(dir2.Folders[t].Name) then begin
            if AI then begin
              //if copy(dir2.folders[t].name,1, 1) = '.'  then continue;
              //if copy(dir2.folders[t].name,1, 1) = '_' then continue;
              if lowercase(dir2.folders[t].name) = 'modelsupport' then continue;
              if lowercase(dir2.folders[t].name) = '.svn' then continue;
            end;
            Merge(sSourceBaseDir, sTargetBaseDir, slash(sDir1)+dir2.folders[t].Name, slash(slash(sTargetBaseDir)+copy(sDir1, length(sSourceBaseDir)+1, length(sDir1)))+dir2.folders[t].Name);
          end;
        end;

        //dir1.Refresh;
        //dir2.Refresh;
        //AddMessage('At end found: '+inttostr(dir1.Filecount)+' and '+inttostr(dir2.Filecount)+' files.');

        //for every folder in dir 1 replicate
        sSourceBaseDir := slash(sSourceBaseDir);
        for t:= 0 to dir1.FolderCount-1 do begin
          if lowercase(dir1.Folders[t].name) = 'dfsrprivate' then
            continue;
          if IsCancelled then exit;
          if bIsRoot then begin
            self.volatile_progress.StepCount := dir1.Filecount * dir1.foldercount;
            self.volatile_progress.Step := ((t+1) * dir1.Filecount div 2)+(self.volatile_progress.StepCount div 2);
          end;
          if AI then begin
//            if copy(dir1.folders[t].name,1, 1) = '.' then continue;
//            if copy(dir1.folders[t].name,1, 1) = '_' then continue;
            if lowercase(copy(dir1.folders[t].name,1, length('modelsupport'))) = 'modelsupport' then continue;
            if lowercase(dir1.folders[t].name) = '.svn' then continue;
          end;

          Merge(sSourceBaseDir, sTargetBaseDir, slash(sDir1)+dir1.folders[t].Name, slash(slash(sTargetBaseDir)+copy(sDir1, length(sSourceBaseDir)+1, length(sDir1)))+dir1.folders[t].Name);
        end;


      finally
        dir2.free;
      end;
    finally
      dir1.Free;

      if (sSourceBaseDir = sdir1) and (sTargetBaseDir = sDir2) then begin
        while not CP.IsComplete do begin
          self.Status := 'Waiting for final copies...'+inttostr(round(CP.PercentComplete*100))+'% still:'+cp.Commands[0].classname;
          self.volatile_progress.StepCount := 100;
          self.volatile_progress.Step := round(CP.PercentComplete*100);
          sleep(cp.RecommendedSleepTime);
          CP.RollupCompletedcommands;
        end;

        CP.WaitForAll;
        CP.Clear;
      end;

      //call this same function for every sub folder
  //    windows.Beep(50,50)     dd
    end;


  except
    on E: Exception do begin
      self.status := e.message;
      Debug.Log(e.message);
    end;
  end;
end;


procedure TFileReplicator.CopyFile2(sSource, sTarget: string; iSize: int64);
var
//  b: boolean;
  c: TFileCopycommand;
begin

  c := TfileCopycommand.create;
  c.Size := iSize;
  c.Source := sSource;
  c.Destination := sTarget;
  c.FireForget := true;
  c.Latency := Latency;
  c.Compress := true;

  filecommands.AddCommand(c);



//  if not B then
//    raise Exception.create('Exception copying file code:'+inttostr(GetLastError));

end;



procedure TFileReplicator.MoveFile2(sSource, sTarget: string; iSize: int64);
var
//  b: boolean;
  c: TfileMovecommand;
begin

  c := TfileMovecommand.create;
  c.Source := sSource;
  c.Destination := sTarget;
  c.FireForget := true;
  c.Size := iSize;
  c.Latency := Latency;
  c.Compress := true;

  filecommands.AddCommand(c);



//  if not B then
//    raise Exception.create('Exception copying file code:'+inttostr(GetLastError));

end;



{ TFileReplicateCommand }




end.
