unit Dir;
{$INCLUDE DelphiDefs.inc}
{$DEFINE DISABLE_ASYNC}
interface
uses
  tickcount, orderlyinit, systemx, Debug, BackGroundThreads, commandprocessor, Classes, numbers,
{$IFDEF WINDOWS}
  winapi.windows,
{$ENDIF}
  ioutils, generics.collections, better_collections, generics.defaults, typex,
  sysutils, DirFile, sharedobject, betterobject, commandicons, stringx, filefind, commands_system, fileex;

const
  dir_sleep = 1;
  {$IFDEF CLX}
  ALL_FILES = '*';
  {$ELSE}
  ALL_FILES = '*.*';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FILE_WILDCARD_ANY = '*.*';
  {$ELSE}
  FILE_WILDCARD_ANY = '*';
  {$ENDIF}


type
  TDirCommand = class;
  TFileInfoRec = record
    name: string;
    date: TDateTime;
    function DebugString: string;
  end;


  TFileTransferReference = class(TBetterObject)
  private
    FBuffer: PByte;
    FLength: int64;
    FFileName: string;
    FFileDate: TDateTime;
    FStartBlock: int64;
    FcontainsData: boolean;
    FEOF: boolean;
    FHandle: THandle;
  public
    property FileName: string read FFileName write FFileName;
    property StartBlock: int64 read FStartBlock write FStartBlock;
    property Length: int64 read FLength write FLength;
    property Buffer: PByte read FBuffer write FBuffer;
    property FileDate: TDateTime read FFileDate write FFileDate;
    property ContainsData: boolean read FcontainsData write FContainsData;
    property EOF: boolean read FEOF write FEOF;
    property Handle: THandle read FHandle write FHandle;
    procedure FreeBuffer;
  end;

  TDirectory = class(TSharedObject)
  private
    FbFilterfolders: boolean;
    FAttributes: integer;
    FFiles: TBetterList<TFileInformation>;
    FFolders: TBetterList<TFileInformation>;
    Fsubdirs: TBetterList<TDirectory>;
    FAsynchronous: boolean;
    Frefreshing: boolean;
    FFileSpec, FPath: string;
    Fthr: TDirCommand;
    FIndex, FFolderIndex: integer;
    iLastIndexOF: integer;
    FError: boolean;
    FSort: boolean;
    FRecurse: boolean;
    function GetFile(index: integer): TfileInformation;
    function GetFolder(index: integer): TfileInformation;
    procedure FillList(prog: PVolatileProgression; strdirectory: string; Attr, AttrMask: Integer; List: TBetterList<TFileInformation>);
    procedure EMptyList(list: TBetterList<TFileInformation>);
    function GetAttributes: integer;
    function GetFilterfolders: boolean;
    procedure SetAttributes(i: integer);
    procedure SetFilterfolders(b: boolean);
    function GetFileCount: integer;
    function Getfoldercount: integer;
    function Getrefreshing: boolean;
    function GetAsync: boolean;
    procedure SetAsync(const Value: boolean);
    procedure SetRefreshing(const Value: boolean);
    function GetThr: TDirCommand;
    procedure SetThr(const Value: TDirCommand);
    function GetFileSpec: string;
    function GEtPath: string;
    procedure SetFileSpec(const Value: string);
    procedure SetPath(const Value: string);
    function GetSubdir(index: integer): TDirectory;
    function GetParentFolder: string;


  public
    FileProgression: TVolatileProgression;//read this to get pseudo-threadsafe status when refreshing
    FolderProgression: TVolatileProgression;
    AttributeMask: integer;
    procedure RefreshFiles;
    procedure RefreshFolders;
    procedure syncSubdirs;
    procedure ClearSubdirs;

    constructor Create(strpath, strFilespec: string; AttrResult, AttrMask: integer; bAsynchronous: boolean = false; bSort: boolean = true; brecurse: boolean = false); reintroduce; virtual;
    class function CreateH(strpath, strFilespec: string; AttrResult, AttrMask: integer; bAsynchronous: boolean = false; bSort: boolean = true; brecurse: boolean = false): IHolder<TDirectory>; reintroduce; virtual;

    destructor Destroy;override;
    procedure Refresh;
    function HasFile(sName: string): boolean;
    function IndexOfByName(sName: string): integer;

    function HasFolder(sName: string): boolean;
    function IndexOfFolderByName(sName: string): integer;
    procedure DetachThread;


    property Files[index: integer]: TfileInformation read GetFile; default;
    {A list of files in the directory}
    property Folders[index: integer]: TfileInformation read GetFolder;
    {A list of subfolders in the directory}
    property SubDirs[index: integer]: TDirectory read GetSubdir;


    property Attributes: integer read GetAttributes write SetAttributes;
    {Attributes of the current folder}
    property FilterFolders: boolean read GetFilterFolders write SetfilterFolders;
    {}
    property Filecount: integer read Getfilecount;
    {The number of files in the directory}
    property Foldercount: integer read Getfoldercount;
    {The number of folders in the list}

    property Path: string read GEtPath write SetPath;
    property FileSpec: string read GetFileSpec write SetFileSpec;
    property Refreshing: boolean read Getrefreshing write SetRefreshing;
    property Asynhronous: boolean read GetAsync write SetAsync;
    property thr: TDirCommand read GetThr write SetThr;

    procedure ResetIndex;
    function GetNextFile(out fil: TfileInformation): boolean;
    function GetNextFolder(out fil: TfileInformation): boolean;
    procedure SortByDate;
    property Error: boolean read FError write FError;
    property Sort: boolean read FSort write fSort;
    property Recurse: boolean read FRecurse write Frecurse;
    property ParentFolder: string read GetParentFolder;
  end;




  //----------------------------------------------------------------------------
  TdirCommand = class(Tcommand)
  private
    { Private declarations }
    FDir: TDirectory;
  protected
    procedure DoExecute; override;
  public
    constructor Create(dir: TDirectory);reintroduce;virtual;
    destructor Destroy; override;
  end;

  Tcmd_CopyFolder = class(Tcommand)
  private
    FSource: string;
    FDest: string;
    fFilespec: string;
  public
    procedure InitExpense;override;
    procedure DoExecute; override;
    property Source: string read FSource write FSource;
    property Dest: string read FDest write FDest;
    property FileSpec: string read fFilespec write FFileSpec;
  end;

  Tcmd_BeginCopyFolder = class (TCommand)
  private
    FFileSpec: string;
    FSource: string;
    FDest: string;
    FP: TCommandProcessor;
    procedure SetP(const Value: TCommandProcessor);
  public
    property Source: string read FSource write FSource;
    property Dest: string read FDest write FDest;
    property P: TCommandProcessor read FP write SetP;
    property FileSpec: string read FFileSpec write FFileSpec;
    procedure DoExecute;override;
  end;

function GetFileDate(FullName: string): TDateTime;
function GetFolderSize(sFullPath: string; bRecurse: boolean = true):int64;
function GetFileSize(sFileName: string): int64;
procedure DeleteFolder(sFolder: string);
procedure Deletefilespec(sFolder: string; sFileSpec: string);
function GetFileCount(sdir: string; sFileSpec: string = '*.*'): integer;
function CompareThenCopyFile(sSource: string; sDest: string): boolean;
function CopyFile(sSource: string; sDest: string): boolean;
procedure CopyFolder(sSource: string; sDest: string; sFileSpec: string);
function BeginCopyFolder(sSource: string; sDest: string; sFileSpec: string = '*.*'): Tcmd_CopyFolder;overload;
function BeginCopyFolder(p: TCommandProcessor; sSource: string; sDest: string; sFileSpec: string = '*.*'): boolean;overload;
function BeginBeginCopyFolder(p: TCommandProcessor; sSource: string; sDest: string; sFileSpec: string = '*.*'): Tcmd_BeginCopyFolder;overload;
procedure EndCopyFolder(c: Tcmd_CopyFolder);overload;
procedure EndCopyFolder(p: TCommandProcessor);overload;
function GetFileCheckSum(sFile: string): TAdvancedfileChecksum;
procedure EmptyPath(sPath: string);
function FindFileRecursive(sBaseDir: string; var sFile: string): boolean;
function IsAllHex(l: TBetterList<TFileInformation>): boolean;
function GetNewestFile(sdir: string; bRecurse: boolean = true): TFileInfoRec;
function GetFileList(dir: string; filespec: string; attrres, attrmask: ni; bRecurse: boolean): TStringlist;
function GetFileListH(dir: string; filespec: string; attrres, attrmask: ni; bRecurse: boolean): IHolder<TStringlist>;
function GetLastSequentialFileName(sdir: string; sExt: string): string;
function GetNextSequentialFileName(sdir: string; sExt: string): string;

var
  DirCommands: TCommandProcessor;
  filenamecomparison: TComparison<TFileInformation>;
  filenamehexcomparison: TComparison<TFileInformation>;
  filenamedatecomparison: TComparison<TFileInformation>;


implementation

uses
  commands_file;

function GetFileListH(dir: string; filespec: string; attrres, attrmask: ni; bRecurse: boolean): IHolder<TStringlist>;
begin
  result := THolder<TStringList>.create;
  result.o := GetfileList(dir, filespec, attrres, attrmask, bRecurse);
end;
function GetFileList(dir: string; filespec: string; attrres, attrmask: ni; bRecurse: boolean): TStringlist;
var
  fil: TFileInformation;
  oDir: TDirectory;
  slSub: TStringlist;
begin
  result := TStringlist.create;
  odir := nil;
  try
    odir := TDirectory.create(dir, filespec, attrres, attrmask, false, true, false);
    while odir.getnextfile(fil) do begin
      result.Add(fil.FullName);
    end;

    if bRecurse then
    while odir.GetNextFolder(fil) do begin
      slSub := GetFileList(fil.FullName, filespec, attrres, attrmask, bRecurse);
      try
        result.AddStrings(slSub);
      finally
        slSub.free;
      end;
    end;
  finally
    odir.free;
  end;
end;

function IsAllHex(l: TBetterList<TFileInformation>): boolean;
var
  fi: tFileInformation;
  t: integer;
begin
  result := true;
  for t:= 0 to l.Count-1 do begin
    fi := l[t];
    if not IsHex(fi.namepart) then begin
      result := false;
      break;
    end;
  end;

end;

function BeginCopyFolder(sSource: string; sDest: string; sFileSpec: string = '*.*'): Tcmd_CopyFolder;
begin
  result := Tcmd_CopyFolder.create;
  result.Source := sSource;
  result.Dest := sDest;
  result.FileSpec := SFileSpec;
  result.start;

end;

function BeginCopyFolder(p: TCommandProcessor; sSource: string; sDest: string; sFileSpec: string = '*.*'): boolean;overload;
var
  dir: TDirectory;
  c: TFileCopyCommand;
  t: integer;
begin
  result := true;
  if not directoryexists(sSource) then begin
    result := false;
    exit;
  end;

  dir := Tdirectory.create(sSource, sFileSpec, 0, 0, false, false);
  try
    for t:= 0 to dir.Filecount-1 do begin
      c := TFileCopyCommand.create;
      c.Source := dir.Files[t].FullName;
      c.Destination := systemx.Slash(sDest)+dir.Files[t].name;
      c.OwnedByProcessor := true;
      c.FireForget := false;
      c.Start(p);
    end;

    for t:=0 to dir.Foldercount-1 do begin
      forcedirectories(sDest);
      BegincopyFolder(p, dir.Folders[t].FullName, slash(sDest)+dir.Folders[t].Name, sfileSpec);
    end;

  finally
    dir.Free;
  end;
end;

procedure EndCopyFolder(p: TCommandProcessor);
begin
  p.WaitForAll;
  p.Clear;

end;


procedure EndCopyFolder(c: Tcmd_CopyFolder);
begin
  try
    c.WaitFor;
  finally
    c.free;
  end;
end;


procedure Deletefilespec(sFolder: string; sFileSpec: string);
var
  dir: TDirectory;
  fil: TFileinformation;
  t: integer;
begin
  dir := TDirectory.create(sFolder,sfileSpec,0,0,false, true);
  try
    while dir.GetNextFile(fil) do begin
      DeleteFile(fil.fullname);
    end;
  finally
    dir.free;
  end;


end;


function GetFileCount(sdir: string; sFileSpec: string = '*.*'): integer;
var
  dir: TDirectory;
begin
  dir := TDirectory.create(sDir, sFileSpec, 0,0, false, false);
  try
    result := dir.filecount+dir.foldercount;
  finally
    dir.free;
  end;

end;

function GetFileSize(sFileName: string): int64;
var
  fi: TFileInformation;
begin
  fi := TFileInformation.Create;
  try
    fi.LoadFromFile(sFileName);
    result := fi.size;
  finally
    fi.Free;
  end;


end;
{var
  h: THandle;
  fi: TByHandleFileInformation;
begin
  result := 0;
  h := fileOpen(sfileName, fmopenRead+fmShareDenyNone);
  try
    if (windows.GetFileInformationByHandle(h, fi)) then
      result := int64(fi.nFileSizeHigh) * 65536 + fi.nFileSizeLow


  finally
    fileClose(h);
  end;

end;}



procedure DeleteFolder(sFolder: string);
var
  dir: TDirectory;
  fil: TFileinformation;
  t: integer;
begin
  dir := TDirectory.create(sFolder,'*.*',0,0,false);
  try
    while dir.GetNextFile(fil) do begin
      DeleteFile(fil.fullname);
    end;

    for t:= 0 to dir.foldercount-1 do begin
      DeleteFolder(dir.folders[t].fullname);
    end;


  finally
    dir.free;
  end;

  REmoveDir(sFolder);
  //RemoveDirectory(PChar(sFolder));

end;

procedure DeleteFilesInFolder(sFolder: string);
var
  dir: TDirectory;
  fil: TFileinformation;
  t: integer;
begin
  dir := TDirectory.create(sFolder,'*.*',0,0,false);
  try
    while dir.GetNextFile(fil) do begin
      DeleteFile(fil.fullname);
    end;

    for t:= 0 to dir.foldercount-1 do begin
      DeleteFolder(dir.folders[t].fullname);
    end;

    REmoveDir(sFolder);
  finally
    dir.free;
  end;

end;


{------------------------------------------------------------------------------}
function GetFolderSize(sFullPath: string; bRecurse: boolean = true): int64;
var
  dir: TDirectory;
  t: integer;
begin
  result := 0;
  dir := TDirectory.create(sFullPAth, '*.*', 0,0);
  try
    if bRecurse then begin
      for t:= 0 to dir.FolderCount-1 do begin
        inc(result, GetFolderSize(dir.Folders[t].FullName, true));
      end;
    end;

    for t:= 0 to dir.FileCount-1 do begin
      inc(result, dir.Files[t].Size);
    end;
  finally
    dir.free;
  end;
end;


procedure TDirectory.ClearSubdirs;
var
  t: integer;
begin
  if FSubdirs = nil then
    exit;

  for t:= fSubdirs.count-1 downto 0 do begin
    TObject(FSubdirs[t]).free;
    FSubdirs[t] := nil;
  end;
  fsubdirs.Clear;



end;

constructor TDirectory.Create(strpath, strFilespec: string; AttrResult, AttrMask: integer; bAsynchronous: boolean; bSort: boolean; bRecurse: boolean);
begin
  inherited Create;

  FRefreshing := false;

  iLastIndexOF := 0;

  FbFilterFolders := false;
  FFiles := TBetterList<TFileInformation>.create;
  FFolders := TBetterList<TFileInformation>.create;
  path:=strPath;
  filespec:=strFilespec;
  attributes:= AttrResult;
  attributeMask:= AttrMask;
{$IFNDEF DISABLE_ASYNC}
  FAsynchronous := bAsynchronous;
{$ENDIF}
  Sort := bSort;
  Recurse := brecurse;
  try
    Refresh;
  except
    Error := true;
  end;

end;
class function TDirectory.CreateH(strpath, strFilespec: string; AttrResult,
  AttrMask: integer; bAsynchronous, bSort,
  brecurse: boolean): IHolder<TDirectory>;
begin
  result := THolder<TDirectory>.create;
  result.o := TDirectory.Create(strpath, strfilespec, attrresult, attrmask, bAsynchronous, bSort, bRecurse);

end;

{------------------------------------------------------------------------------}
destructor TDirectory.destroy;
var
  fi: TFileInformation;
begin
  WHILE refreshing do
    sleepex(dir_sleep,true);

  //while thr <> nil do
    //sleepex(50);


  DetachThread;
  EmptyList(FFolders);
  EmptyList(FFiles);
  Ffolders.free;
  Ffiles.free;

  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TDirectory.Refresh;
begin
  FileProgression.reset;
  FolderProgression.reset;
  if NOT self.Asynhronous then begin
    RefreshFiles;
    RefreshFolders;
  end else begin
    DetachThread;
    Refreshing := true;
    thr := TDirCommand.create(self);
    thr.start(DirCommands);

    //while (not thr.IsComplete) do
    //  sleepex(50);

    //DetachThread;

  end;
end;
{------------------------------------------------------------------------------}
function FileInfoCompare(p1,p2: TFileInformation): integer;
var
  f1,f2: TFileInformation;
begin
  f1 := TFIleINformation(p1);
  f2 := TFIleINformation(p2);

  result := 0;
  if lowercase(f1.Name) > lowercase(f2.Name) then result := 1;
  if lowercase(f1.Name) < lowercase(f2.Name) then result := -1;
  if lowercase(f1.Name) = lowercase(f2.Name) then result := 0;

end;

function FileInfoCompareHex(p1,p2: TFileInformation): integer;
var
  f1,f2: TFileInformation;
begin
  f1 := TFIleINformation(p1);
  f2 := TFIleINformation(p2);

  result := 0;
  if strtoint64('$'+f1.NamePart) > strtoint64('$'+f2.NamePart) then result := 1;
  if strtoint64('$'+f1.NamePart) < strtoint64('$'+f2.NamePart) then result := -1;
  if strtoint64('$'+f1.NamePart) = strtoint64('$'+f2.NamePart) then result := 0;

end;

procedure TDirectory.FillList(prog: PVolatileProgression; strdirectory: string; Attr, AttrMask: Integer; List: TBetterList<TFileInformation>);
var
  sRec: TSearchRec;
  FileInf: TfileInformation;
  accept: boolean;
  scope: Tfindscope;
  List2: TBetterList<TFileInformation>;
  List3: TBetterList<TFileInformation>;
  tmLastMerge: cardinal;
const
  NULL_DATE:double =  0.0;
begin
  //Lock;
  tmlastMerge := GetTicker;
  List2 := TBetterList<TFileInformation>.create;
  List3 := TBetterList<TFileInformation>.create;
  try
    EmptyList(list);
    ResetEasyFind(scope);
    try
      while EasyFindFile(scope, strdirectory, Attr, AttrMask, sRec) do begin
        fileinf:= TfileInformation.create;
        fileinf.fullname:=Extractfilepath(strdirectory)+srec.Name;
        fileinf.Size := sRec.Size;
        try
          fileinf.Date := FileDateToDateTime(sRec.Time);
        except
          fileinf.Date := TDateTime(NULL_DATE);
        end;


        accept := true;

        if fileinf.name = '.' then
          accept := false;
        if fileinf.name = '..' then
          accept := false;

        if accept then begin
          List2.add(fileinf);
          if (List2.Count > 8000) or ((GetTimeSince(tmLAstMerge) > 100)) then begin
//            List2.Sort(FileInfoCompare);
//            List3.Assign(List2, laCopy);
            Lock;
            try
              List.AddList(list2);
              tmLastMerge := GetTicker;
            finally
              Unlock;
            end;
//            List.Sort(FileInfoCompare);
            List2.Clear;

          end;
          prog.StepsCompleted := list2.count+list.count;
        end
        else begin
          fileinf.free;
        end;
        {fileinf need not be destroyed, because Tdirectory destroys it}
      end;
    finally
      prog.Complete := true;
      FindClose(srec);
    end;
  finally
    Lock;
    try
      List.AddList(list2);
      if FSort then begin
        if IsAllHex(list) then begin
          List.Sort(TComparer<TFileInformation>.Construct(filenamehexcomparison));
        end else begin
//          List.Sort(TComparer<TFileInformation>.Construct(filenamecomparison));
          List.Sort(TDelegatedComparer<TFileInformation>.Create(filenamecomparison));
        end;
      end;
    finally
      Unlock;
    end;
    //unlock;
    List2.Free;
    List3.free;
  end;

end;
{------------------------------------------------------------------------------}
procedure TDirectory.RefreshFolders;
begin
  if not FilterFolders then
    FillList(@FolderProgression, slash(path)+FILE_WILDCARD_ANY, fadirectory, faDirectory , FFolders)
  else
    FillList(@FolderProgression, slash(path)+FileSpec, Attributes or faDirectory, AttributeMask or faDirectory,  FFolders);

  if Recurse then begin
    SyncSubdirs;
  end;
end;
{------------------------------------------------------------------------------}
procedure TDirectory.RefreshFiles;
begin
  FillList(@FileProgression, slash(path)+filespec, Attributes and (not faDirectory), AttributeMask or faDirectory, FFiles);
end;

{------------------------------------------------------------------------------}
function TDirectory.GetAttributes: integer;

begin
  lock;
  try
    result:=FAttributes;
  finally
    Unlock;
  end;
end;
{------------------------------------------------------------------------------}
function TDirectory.GetFilterFolders: boolean;

begin
//  lock;
//  try
    result:=Fbfilterfolders;
//  finally
//    Unlock;
//  end;
end;
{------------------------------------------------------------------------------}
procedure TDirectory.SetAttributes(i: integer);
begin
  lock;
  try
    FAttributes:=i;
  finally
    Unlock;
  end;
end;
{------------------------------------------------------------------------------}
procedure TDirectory.SetFilterfolders(b: boolean);

begin
  while refreshing do
    sleepex(dir_sleep,true);

  Lock;
  try
    FbFilterFolders:=b;
    RefreshFolders;
  finally
    unlock;
  end;
end;
{------------------------------------------------------------------------------}
function TDirectory.Getfile(index: integer):Tfileinformation;
begin
  lock;
  try
    result:=TfileInformation(FFiles.items[index]);
  finally
    Unlock;
  end;

end;
{------------------------------------------------------------------------------}
function TDirectory.GetFolder(index: integer):TfileInformation;
begin
  lock;
  try
    result:=Tfileinformation(FFolders.items[index]);
  finally
    Unlock;
  end;
end;
{------------------------------------------------------------------------------}
function TDirectory.GetFileCount: integer;

begin
  while Refreshing do
    sleepex(dir_sleep,true);

  lock;
  try
    result:=FFiles.count;
  finally
    unlock;
  end;
end;
{------------------------------------------------------------------------------}
function TDirectory.Getfoldercount: integer;

begin
  while Refreshing do
    sleepex(dir_sleep,true);
  lock;
  try
    result:=Ffolders.count;
  finally
    unlock;
  end;
end;
{------------------------------------------------------------------------------}
function Tdirectory.IndexOfByName(sName: string): integer;


var
  t: integer;
begin
  while self.Refreshing do
    sleepex(dir_sleep,true);

  lock;
  try

    result := -1;

    sName := uppercase(sName);
    for t:= iLastIndexOF to FileCount -1 do begin
      if StrIComp(Pchar(files[t].Name), Pchar(sName))= 0 then begin
      //if uppercase(Files[t].Name) = uppercase(sName) then begin
        iLastIndexOF := t;
        result := t;
        break;
      end;
    end;

    if result = -1 then
    for t:= 0 to iLastIndexOf -1 do begin
      if StrIComp(Pchar(files[t].Name), Pchar(sName))= 0 then begin
      //if uppercase(Files[t].Name) = uppercase(sName) then begin
        result := t;
        iLastIndexOF := t;
        break;
      end;
    end;

  finally
    Unlock;
  end;

end;
{------------------------------------------------------------------------------}
function Tdirectory.HasFile(sName: string): boolean;


begin
  while self.Refreshing do
    sleepex(dir_sleep,true);

  lock;
  try
    result := not (IndexOfByName(sName) = -1);
  finally
    Unlock;
  end;

end;
{------------------------------------------------------------------------------}
function TDirectory.HasFolder(sName: string): boolean;


begin
  while self.Refreshing do
    sleepex(dir_sleep,true);

  result := not (IndexOfFolderByName(sName) = -1);
end;
{------------------------------------------------------------------------------}
function TDirectory.IndexOfFolderByName(sName: string): integer;


var
  t: integer;
begin
  while Refreshing do
    sleepex(dir_sleep,true);

  Lock;
  try
    result := -1;
    for t:= 0 to FolderCount -1 do begin
      if uppercase(Folders[t].Name) = uppercase(sName) then begin
        result := t;
        break;
      end;
    end;
  finally
    UnLock;
  end;

end;

{------------------------------------------------------------------------------}
function TDirectory.Getrefreshing: boolean;


begin
//  Lock;
//  try
{$IFDEF SAFE_REFRESHING}
    if not FAsynchronous then
{$ENDIF}
      result := FRefreshing
{$IFDEF SAFE_REFRESHING}
    else
      if assigned(thr) then
        result := (not thr.IsComplete)
      else
        result := FRefreshing;
{$ENDIF}

    //result :=(FAsynchronous and (FRefreshing);
//  finally
//    Unlock;
//  end;
end;

function TDirectory.GetSubdir(index: integer): TDirectory;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

{ TDirCommand }

constructor TDirCommand.Create(dir: TDirectory);
begin
  inherited Create;
  //self.Processor := DirCommands;
  FDir := dir;
//  DiskExpenseByString[GetDrive(dir.path)] := 0.1;
//  NetworkExpense := 0.05;
  CPUExpense := 0.00;
  Icon := @CMD_ICON_FOLDER;
end;

destructor TDirCommand.destroy;

begin

  inherited;
end;

procedure TDirCommand.DoExecute;

begin
      if assigned(fDir) then begin
        //while FDir.Refreshing do sleepex(50);

        Fdir.Refreshing := true;
        //GLOG.Debug('Dir '+FDIr.Path, 'filecopy');
        try
          Status := FDir.Path;
          StepCount := 2;
          STep := 0;
          Fdir.RefreshFiles;//<--- call individuall because they don't check asynchronous
          Step := 1;
          Fdir.RefreshFolders;//<--- call individuall because they don't check asynchronous
          Step := 2;
        finally
          Fdir.Refreshing := false;
          //GLOG.Debug('Dir Complete '+Fdir.Path, 'filecopy');
        end;
        //sleepex(fdir.filecount);
        //Fdir.DetachThread;

        //sleepex(10000);
      end;
end;

function TDirectory.GetAsync: boolean;


begin
  Lock;
  try
    result := FAsynchronous;
  finally
    Unlock;
  end;
end;

procedure TDirectory.SetAsync(const Value: boolean);


begin
  Lock;
  try
    FAsynchronous := value;
  finally
    Unlock;
  end;
end;

procedure TDirectory.SetRefreshing(const Value: boolean);
begin
  Lock;
  try
    FRefreshing := value;
  finally
    Unlock;
  end;

end;

procedure TDirectory.DetachThread;
var
  c: Tcmd_Garbage;
begin
  Lock;
  try

    if assigned(thr) then begin
      c := Tcmd_Garbage.Create;
      c.CommandObject := thr;
      c.Start;
    end;
    thr := nil;

  finally
    Unlock;
  end;
end;

procedure TDirectory.EMptyList(list: TBetterList<TFileInformation>);
var
  fi: TFileInformation;
begin
  while list.count > 0 do begin
    fi := list[list.count-1];
    list.delete(list.count-1);
    fi.free;
    fi := nil;
  end;

end;

function TDirectory.GetThr: TDirCommand;

begin
  Lock;
  try
    result := Fthr;
  finally
    UnLock;
  end;
end;

procedure TDirectory.SetThr(const Value: TDirCommand);

begin
  Lock;
  try
    Fthr := value;
  finally
    Unlock;
  end;

end;

function DateCompare(Item1, Item2: Pointer): Integer;
var
  p1,p2: TFileInformation;
begin
  p1 := item1;
  p2 := item2;
  if (p1.Date < p2.Date) then
    result := -1
  else
  if (p1.Date > p2.Date) then
    result := 1
  else
    result := 0;


end;

procedure TDirectory.SortByDate;
var
  t: integer;
begin
  FFiles.Sort(TComparer<TFileInformation>.Construct(filenamedatecomparison));


end;

procedure TDirectory.syncSubdirs;
var
  t: integer;
begin
  ClearSubdirs;
  for t:= 0 to FolderCount-1 do begin
    Fsubdirs.Add(Tdirectory.Create(folders[t].Path, self.FileSpec, self.Attributes, self.AttributeMask, self.Asynhronous, self.Sort, self.Recurse));
  end;

end;

function TDirectory.GetFileSpec: string;


begin
  Lock;
  try
    result := FFileSpec;
    UniqueString(result);
  finally
    UnLock;
  end;

end;

function TDirectory.GEtPath: string;


begin
  Lock;
  try
    result := slash(FPath);
    UniqueString(result);
  finally
    UnLock;
  end;

end;

procedure TDirectory.SetFileSpec(const Value: string);
begin
  Lock;
  try
    FFileSpec := value;
    UniqueString(FFileSpec);
  finally
    UnLock;
  end;
end;

procedure TDirectory.SetPath(const Value: string);


begin
  Lock;
  try
    FPath := value;
    UniqueString(FPath);
  finally
    UnLock;
  end;
end;

procedure TDirectory.ResetIndex;
begin
  FIndex := 0;
  FFolderIndex := 0;
end;

//------------------------------------------------------------------------------
function TDirectory.GetNextFile(out fil: TfileInformation): boolean;
begin
  result := false;
  fil := nil;

  while Refreshing and Sort do begin
    sleepex(1,true);
  end;

  while Refreshing and (FIndex >= FFiles.Count) do begin
    sleepex(1,true);
  end;
  if not Refreshing then begin
    if FIndex > FFiles.Count-1 then begin
      exit;
    end;
  end;

  fil := TFileInformation(FFiles[FIndex]);
  result := true;
  inc(FIndex);

end;
//------------------------------------------------------------------------------
function TDirectory.GetNextFolder(out fil: TfileInformation): boolean;
begin
  result := false;
  fil := nil;

  while Refreshing and Sort do begin
    sleepex(1,true);
  end;

  while Refreshing and (FFolderIndex > FFolders.Count-1) do begin
    sleepex(1,true);
  end;
  if not Refreshing then begin
    if FFolderIndex > FFolders.Count-1 then begin
      exit;
    end
  end;

 fil := TFileInformation(FFolders[FFolderIndex]);
 result := true;
 inc(FFolderIndex);
end;


function TDirectory.GetParentFolder: string;
var
  s1,s2,s3: string;
begin
  s1 := slash(Path);
  if splitstring(s1, '\', s2, s3, true) then begin
    if splitstring(s2, '\', s2, s3, true) then begin
      result := slash(s2);
    end else begin
      result := '';
    end;
  end else begin
    result := '';
  end;

end;

function GetFileDate(FullName: string): TDateTime;
var
  iA: integer;
begin
  iA := FileAge(FullName);
  if iA < 0 then iA:= 0;
  try
    result := FileDateToDateTime(iA);
  except
    Debug.Log(nil,'!!!!!!!!!!!!!GetFileDate failed on file '+Fullname);
    result := 0;
  end;
end;


{ TFileTransferReference }

procedure TFileTransferReference.FreeBuffer;
begin
  if assigned(FBuffer) then begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
end;

function CopyFile(sSource: string; sDest: string): boolean;
begin
  result := true;

  TFile.Copy(sSource, sDest, true);

end;

function CompareThenCopyFile(sSource: string; sDest: string): boolean;
begin
  if (not fileExists(sDest)) then begin
    result := true;
  end;
  if (GetFileDate(sSource) = GetfileDate(sDest))
  and ((Getfilesize(sSource) = GetFileSize(sDest))) then begin
    result := false;
    exit;
  end else
    result := (not CompareFiles(sSource, sDest));

  if result then begin
    CopyFile(sSource, sDest);
  end;

end;


function GetFileCheckSum(sFile: string): TAdvancedfileChecksum;
begin
  result.Calculate(sFile);
end;


{ Tcmd_CopyFolder }

procedure Tcmd_CopyFolder.DoExecute;
var
  dir: TDirectory;
  t: integer;
begin
  inherited;
  try
    ForceDirectories(slash(dest));
  except
  end;
  dir := Tdirectory.create(Source, FileSpec, 0,0, false);
  try
    StepCount := dir.filecount-1;
    for t:= 0 to dir.filecount-1 do begin
      Step := t;
      Status := 'Copying '+source;
      Copyfile(slash(Source)+dir.files[t].name, slash(Dest)+dir.files[t].name);
    end;
  finally
    dir.free;
  end;

end;

procedure Tcmd_CopyFolder.InitExpense;
begin
  inherited;
  CPUExpense := 0;
  Ffilespec := '*.*';
end;

{ Tcmd_BeginCopyFolder }

procedure Tcmd_BeginCopyFolder.DoExecute;
begin
  inherited;
  BeginCopyFolder(P, Source, Dest, FileSpec);
end;

procedure Tcmd_BeginCopyFolder.SetP(const Value: TCommandProcessor);
begin
  FP := Value;
end;

function BeginBeginCopyFolder(p: TCommandProcessor; sSource: string; sDest: string; sFileSpec: string = '*.*'): Tcmd_BeginCopyFolder;overload;
begin
  result := Tcmd_BeginCopyFolder.create;
  result.P := p;
  result.Source := sSource;
  result.Dest := sDest;
  result.FileSpec := sFileSpec;
  result.Start(p);

end;

procedure oinit;
begin
  DirCommands := TCommandProcessor.create(BackgroundThreadMan, 'DirCommands');
end;
procedure oafterinit;
begin
  //
end;

procedure ofinal;
begin
  DIrCommands.free;

end;

procedure EmptyPath(sPath: string);
var
  dir: TDirectory;
  fil: TFileInformation;
begin
  dir := TDirectory.create(sPath, '*.*', 0,0, true, false, false);
  try
    while dir.GetNextFile(fil) do begin
      deletefile(fil.FullName);
    end;
  finally
    dir.free;
  end;
end;


procedure CopyFolder(sSource: string; sDest: string; sFileSpec: string);
begin
  EndCopyFolder(BeginCopyFolder(sSource, sDest, sFileSpec));
end;


function FindFileRecursive(sBaseDir: string; var sFile: string): boolean;
var
  sTemp: string;
  d: TDirectory;
  fi: TFileinformation;
begin
  result := false;
  d := Tdirectory.create(sBaseDir, '*.*', 0,0,false,false,false);
  try
    while d.GetNextFile(fi) do begin
      if comparetext(fi.Name, sFile) = 0 then begin
        sFile := slash(sBaseDir)+sFile;
        exit(true);
      end;
    end;

    while d.GetNextFolder(fi) do begin
      result := FindFileRecursive(fi.FullName, sFile);
      if result then exit;
    end;
  finally
    d.Free;
  end;


end;

{ TFileNameComparer }

function GetNewestFile(sdir: string; bRecurse: boolean = true): TFileInfoRec;
var
  d: TDirectory;
  fil:  TFileinformation;
  fr: TfileInfoRec;
begin
  result.Date := 0.0;
  result.Name := '';
  d := TDirectory.create(sDir, '*.*', 0, 0, false, false, false);
  while d.GetNextFile(fil) do begin
    if fil.date > result.date then begin
      result.Date := fil.date;
      result.name := fil.FullName;
    end;
  end;

  while d.GetNextFolder(fil) do begin
    fr := GetNewestFile(fil.fullname);
    if fr.date > Result.date then begin
      result := fr;
    end;

  end;
end;


{ TFileInfoRec }

function TFileInfoRec.DebugString: string;
begin
  result := name+' date='+datetimetostr(date);
end;

function GetLastSequentialFileName(sdir: string; sExt: string): string;
var
  t: ni;
begin
  t := 0;
  while fileexists(slash(sdir)+PadString(inttostr(t),'0', 20)+sExt) do begin
    inc(t);
  end;

  dec(t);
  if t > 0 then
    result := slash(sdir)+PadString(inttostr(t),'0', 20)+sExt
  else
    result := '';

end;

function GetNextSequentialFileName(sdir: string; sExt: string): string;
var
  t: ni;
begin
  t := 0;
  while fileexists(slash(sdir)+PadString(inttostr(t),'0', 20)+sExt) do begin
    inc(t);
  end;

  result := slash(sdir)+PadString(inttostr(t),'0', 20)+sExt;
end;


initialization
  init.RegisterProcs('Dir', oinit, oafterinit, nil, ofinal, nil, 'managedthread');

  //NOTE!: These things do NOT like being members of an object
  filenamecomparison := function(const Left, Right: TFileInformation): Integer
                        begin
                          //debug.Consolelog('Stuff');
                          if right = nil then exit(1);
                          if Left = nil then exit(-1);
                          Result := comparetext(left.Name, right.name);
                        end;
  filenamehexcomparison := function(const Left, Right: TFileInformation): Integer
                         begin
                            if right = nil then exit(1);
                            if Left = nil then exit(-1);
                           Result := strtoint64('$'+left.namepart)-strtoint64('$'+right.namepart)
                         end;


  filenamedatecomparison :=   function(const Left, Right: TFileInformation): Integer
                          var f: double;
                          begin
                            if right = nil then exit(1);
                            if Left = nil then exit(-1);
                             f := left.Date-right.Date;
                             if (f<0) then exit(-1);
                             if (f>0) then exit(1);
                             exit(0);
                          end;


finalization







end.


