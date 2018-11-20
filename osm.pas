unit osm;
dont use me
interface

uses
  tickcount, commandicons, numbers, math, sharedobject, typex, systemx, https, stringx, sysutils, easyimage, dir, dirfile, osm_xml_parser, commandprocessor, abstractdb, rdtpdb, classes, xmltools, osm_to_sql, osmdb;

const
  FETCH_TILE_SIZE = 0.001;
  FETCH_TILE_RADIUS = FETCH_TILE_SIZE / 2;
type
  TOSM_Getter = class(TSharedObject)
  private
    FCacheDir: string;
    fAcquiredResources: string;
    function GetXMLData(west, south, east,north: double; cWAtch: TCommand): string;
    function GetBinFileName(west, south, east,north: double): string;
    function GetFileName(west, south, east,north: double): string;
    function GetFileName_Old(west, south, east,north: double): string;
    function GetBinaryData(west, south, east, north: double; cWatch: TCommand): TXMLDrawWorkingData;
    function GetBinaryDAta_FRomDB(west, south, east, north: double; cWatch: TCommand): TXMLDrawWorkingDAta;
    procedure SEtCacheDir(const Value: string);
    function GetCAcheDir: string;
  protected
    procedure Init;override;
    procedure AcquireDownloadResources(c: TCommand);
    procedure ReleaseDownloadResources(c: TCommand);
    procedure AcquireResources(c: TCommand; sRes: string);
    procedure ReleaseResources(c: TCommand);
    function DownloadChangeSet(iCid: int64): string;
    function DownloadAndConvertChangeSet(iCID: int64): string;

  public
    constructor Create;override;

    property CacheDir: string read GetCAcheDir write SEtCacheDir;
    function GetSnapBinaryDAta(west, south,east,north: double; cWatch: TCommand): TXMLDrawWorkingData;
    function GetChangeSetCacheFile(iCID: int64): string;
    function GetChangeSetSQLCacheFile(iCID: int64): string;
    function GetChangeSet(iCid: int64; iSizeLimit: ni = -1): string;
    function GetChangeSetSQL(iCid: int64): string;
    procedure PutChangeSetSQL(iCid: int64; sSQL: string);
    procedure PreGetChangeSETSQL(iCID: int64);
    procedure CleanFiles(iCID: int64);
  end;

  Tcmd_OSMgetSQL = class(TCommand)
  public
    cid: int64;
    result: string;
    procedure InitExpense;override;
    procedure DoExecute;override;
  end;

var
  GOSM: TOSM_Getter;


implementation

{ TOSM_Getter }

procedure TOSM_Getter.AcquireDownloadResources(c: TCommand);
begin
//  c.resources.setresourceusage('osmhttp', 1/8);
//  c.waitforresources;
end;

procedure TOSM_Getter.AcquireResources(c: TCommand; sRes: string);
begin
  if assigned(c) then begin
    c.Resources.SetResourceUsage(sRes, 1.0);
    FAcquiredResources := sRes;
    c.waitforresources;
  end;

end;

procedure TOSM_Getter.CleanFiles(iCID: int64);
begin
  if fileexists(GetChangeSetCacheFile(iCID)) then
    deletefile(GetChangeSetCacheFile(iCID));
  if fileexists(GetChangeSetSQLCacheFile(iCID)) then
    deletefile(GetChangeSetSQLCacheFile(iCID));
end;

constructor TOSM_Getter.create;
begin
  inherited;
  CAcheDir := 'h:\terraindata\osm\';

end;

function TOSM_Getter.DownloadAndConvertChangeSet(iCID: int64): string;
var
  c: Tcmd_InjectChangeSet;
  p: Tcmd_PArseXML;
  bRetry: boolean;
begin
  bRetry := false;
  repeat
  p := Tcmd_ParseXML.create;
  try
    bRetry := false;
    p.value := GetChangeSet(iCID);
    p.MemoryExpense := lesserof(1.0, length(p.value)/2*million);
//    SetCommandStatus('waiting for xml parse');
    p.Start;
//    WAitForCommandBlind(p);
    p.WaitFor;

    c := Tcmd_InjectChangeSet.create;
    try
      c.xData := p.result;
      p.free;
      p := nil;
      c.Start;
//      WaitForCOmmandBlind(c);
      c.RaiseExceptions := false;
      c.waitfor;
      if c.error then begin
        DeleteFile(self.GetChangeSetCacheFile(iCID));
        deletefile(self.GetChangeSetSQLCacheFile(iCID));
        bRetry := true;
      end;

      PutChangeSetSQL(iCID, c.result);
      result := c.result;
    finally
      c.free;
    end;
  finally
    p.free;
    p := nil;
  end;
  until not bRetry;

end;

function TOSM_Getter.DownloadChangeSet(iCid: int64): string;
var
//  htp: THTTPClient;
  sURL: string;
  sFile: string;
  bGood: boolean;
  sOut: string;
  tmStart: ticker;
begin
  bGood := false;
  tmStart := GEtTicker;
  while not bGood do begin
    try

//      htp := THTTPClient.create;
      try
        surl := 'http://www.openstreetmap.org/api/0.6/changeset/'+inttostr(iCID)+'/download';
        sFile := GetChangeSetCacheFile(iCid);

        if QuickHTTPSGet(sUrl, sOut) then begin
          SaveSTringASFile(sFile,sOut);
          bGood := true;
        end else
          if GetTimeSince(tmStart) > (1000*60*15) then begin
            SaveStringAsFile(sFile, '<timeout></timeout>');
            bGood := true;
          end;

//        if htp.Get(surl) then begin
//          htp.SaveToFile(sFile);
//          bGood := true;
//        end;
        if bGood then
          result := loadfileasstring(sFile);

      finally
//        htp.free;
      end;
    except
      bGood := false;
      sleep(4000);
    end;
  end;

end;

function TOSM_Getter.GetBinaryData(west, south, east,
  north: double; cWatch: TCommand): TXMLDrawWorkingData;
var
  sBin: string;
  bRetry: boolean;
  db: Trdtpdb;
begin
  try
    sBin := getbinfilename(west,south,east,north);
//    if assigned(cWatch) then
//      cWatch.Resources.SetResourceUsage('DB', 1.0);

    repeat
      bRetry := false;
      if fileexists(sBin) then begin
        try
          result.FRomBinFile(sBin);
          if fileexists(GetFileName(west, south, east, north)) then begin
            AcquireResources(cWatch, sBin);
            deletefile(GetFileName(west, south, east, north));
          end;
        except
          bRetry := true;
          AcquireResources(cWatch, sBin);
          deletefile(sBin);
        end;
      end
      else begin
        AcquireResources(cWatch, sBin);
        if fileexists(sBin) then begin
          bRetry := true;
          continue;
        end;

        db := needosmdb;
        try
          result.FromDataBAse(db, west, south,east,north);
          result.ToBinFile(sBin);
        finally
          db.Free;
        end;
      end;
    until not bRetry;
  finally
    ReleaseResources(cWatch);
  end;

end;

function TOSM_Getter.GetBinaryDAta_FRomDB(west, south, east, north: double;
  cWatch: TCommand): TXMLDrawWorkingDAta;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TOSM_Getter.GetBinFileName(west, south, east, north: double): string;
begin
  result := GetFileName(west,south, east,north)+'.bin';
end;

function TOSM_Getter.GetCAcheDir: string;
begin
  Lock;
  try
    result := FCacheDir;
  finally
    unlock;
  end;
end;

function TOSM_Getter.GetChangeSet(iCid: int64; iSizeLimit: ni = -1): string;
var
  sFile: string;
begin
  sFile := GetChangeSetCacheFile(iCID);
  if fileexists(sFile) then begin
    if (iSizeLimit > -1) and (GetFileSize(sFile) > iSizeLimit) then
      result := 'over limit'
    else
      result := LoadFileAsString(sFile)
  end
  else
    result := downloadchangeset(iCid);
end;

function TOSM_Getter.GetChangeSetCacheFile(iCID: int64): string;
var
  sPath: string;
begin
  sPath := slash(CAchedir)+'changesets\'+inttostr(icid div 10000)+'\';
  forcedirectories(sPath);
  result := sPath+inttostr(icid)+'.xml';

end;

function TOSM_Getter.GetChangeSetSQL(iCid: int64): string;
var
  sFile: string;
begin
  sFile := GetChangeSetSqlCacheFile(iCID);
  if fileexists(sFile) then begin
    result := LoadFileAsString(sFile)
  end
  else
    result := downloadandconvertchangeset(iCid);
end;

function TOSM_Getter.GetChangeSetSQLCacheFile(iCID: int64): string;
var
  sPath: string;
begin
  sPath := slash(CAchedir)+'changesets_sql\'+inttostr(icid div 10000)+'\';
  forcedirectories(sPath);
  result := sPath+inttostr(icid)+'.sql';

end;

function TOSM_Getter.GetFileName(west, south, east, north: double): string;
var
  sDir, sFile: string;
begin
  sDir := CacheDir+inttostr(round(floor(west)))+'\'+inttostr(round(floor(south)))+'\';
  sFile := 'bbox_'+floatprecision(west,5)+'_'+floatprecision(south,5)+'_'+floatprecision(east,5)+'_'+floatprecision(north,5)+'.xml';
  try
    lock;
    try
      forcedirectories(sDir);
    finally
      unlock;
    end;
  except
  end;
  result := sDir + sFile;
end;

function TOSM_Getter.GetFileName_Old(west, south, east, north: double): string;
var
  sDir, sFile: string;
begin
  sDir := CacheDir+inttostr(round(floor(west)))+'\'+inttostr(round(floor(south)))+'\';
  sFile := 'bbox_'+floattostr(west)+'_'+floattostr(south)+'_'+floattostr(east)+'_'+floattostr(north)+'.xml';
  try
    lock;
    try
      forcedirectories(sDir);
    finally
      unlock;
    end;
  except
  end;
  result := sDir + sFile;
end;

function TOSM_Getter.GetSnapBinaryDAta(west, south, east, north: double;
  cWatch: TCommand): TXMLDrawWorkingData;
begin
  west := snaptoless(west, FETCH_TILE_SIZE);
  east := snaptomore(east,FETCH_TILE_SIZE);
  north := snaptomore(north,FETCH_TILE_SIZE);
  south := snaptoless(south,FETCH_TILE_SIZE);
  result := GetBinaryData(west, south, east,north, cWatch);
end;

function TOSM_Getter.GetXMLData(west, south, east, north: double; cWAtch: TCommand): string;
var
//  htp: THTTPCLient;
  sFile, sFile2: string;
  bReTry: boolean;
  sUrl: string;
  sOut: string;
begin
  sFile := GEtFileNAme(west,south,east,north);
  sFile2 := GEtFileNAme_OLD(west,south,east,north);
  if fileexists(sFile) and (GetFileSize(sFile) > 0) then begin
    result := LoadFileAsString(sFile);
    sFile := GEtFileNAme_old(west,south,east,north);
    if fileexists(sFile) then begin
      try
      deletefile(sfile);
      except end;
    end;
  end else begin

    bRetry := false;
    repeat
//      htp := nil;
      try
        if fileexists(sFile2) and (GetFileSize(sFile2) > 0) then begin
          result := LoadFileAsString(sFile2);

        end else begin

          bRetry := false;
//          htp := THTTPCLient.create;
          try
            AcquireDownloadResources(cwatch);
            try
              sURl := 'http://www.openstreetmap.org/api/0.6/map?bbox='+floattostr(west)+','+floattostr(south)+','+floattostr(east)+','+floattostr(north);
              QuickHTTPSGet(sURL,result);
            finally
              ReleaseDownloadResources(cWatch);
            end;
          except
            sleep(2000);
            bRetry := true;
          end;



//          result := htp.InBody;
        end;
        if length(result) > 0 then begin
          SaveStringAsFile(sFile, result);
          if fileexists(sFile2) then begin
          try
            deletefile(sfile2);
            except end;
          end;

        end
        else
          bRetry := true;
      finally
//        if assigned(htp) then
//          htp.Free;
//        htp := nil;
     end;
    until bRetry = false;
  end;

end;

procedure TOSM_Getter.Init;
begin
  inherited;

end;

procedure TOSM_Getter.PreGetChangeSETSQL(iCID: int64);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TOSM_Getter.PutChangeSetSQL(iCid: int64; sSQL: string);
var
  sFile: string;
begin
  sFile := GetChangeSetSQLCacheFile(icid);
  SaveStringAsFile(sFile, sSQL);

end;

procedure TOSM_Getter.ReleaseDownloadResources(c: TCommand);
begin
//  c.resources.setresourceusage('osmhttp', 0.0);
end;

procedure TOSM_Getter.ReleaseResources(c: TCommand);
begin
  if fAcquiredResources <> '' then
    c.Resources.SetResourceUsage(FAcquiredResources, 0.0);

  FAcquiredResources := '';
end;

procedure TOSM_Getter.SEtCacheDir(const Value: string);
begin
  Lock;
  try
    FCacheDir := slash(Value);
    forcedirectories(FCAcheDir);
    forcedirectories(FCacheDir+'changesets\');
  finally
    unlock;
  end;
end;

{ Tcmd_OSMgetSQL }

procedure Tcmd_OSMgetSQL.DoExecute;
begin
  inherited;
  resources.SetResourceUsage('osm' + inttostr(cid), 1.0);
  Icon := @CMD_ICON_FILE_COPY;
  result := GOSM.GetChangeSetSQL(cid);
end;

procedure Tcmd_OSMgetSQL.InitExpense;
begin
  inherited;
  //Status := 'Getting resources ' + inttostr(cid);
  resources.Clear;
  resources.SetResourceUsage('osmDL', 1/5);
  resources.SetResourceUsage('osm' + inttostr(cid), 1.0);
  Icon := @CMD_ICON_FILE_COPY;
  cpuexpense := 0;

//  memoryexpense := ;
end;

end.
