unit ApplicationParams;
{$I DelphiDefs.inc}
interface
{$DEFINE CACHEPARAMS}

uses
  NameValuePair, systemx,  classes,
  {$IFNDEF FPC}
  System.IOUtils,
  {$ELSE}

  {$ENDIF}
{$IFDEF WINDOWS}
  windows,
{$ENDIF}
  tickcount,
  sysutils, orderlyinit;

type
  TAppParams = class(TNameValuePairList)
  private
  protected
  public
  end;


function NeedAppParams: TAppParams;
procedure NoNeedAppParams(nvpl: TAppParams);
function NeedUserParams: TAppParams;
procedure NoNeedUserParams(nvpl: TAppParams);

function GetApplicationParamsfileName: string;
function GetUserParamsFileName: string;

function APGet(const key: string; defaultvalue: string; bMore: boolean = false): string;overload;
function APGet(const key: string; defaultvalue: boolean; bMore: boolean = false): boolean;overload;
function APGet(const key: string; defaultvalue: real; bMore: boolean = false): real;overload;
function APGet(const key: string; defaultvalue: int64; bMore: boolean = false): int64;overload;
function APGetIntegerArray(const key: string; bMore: boolean = false): TArray<int64>;overload;

procedure APPut(const key: string; value: string; bMore: boolean = false);
procedure APPutIntegerArray(const key: string; a: TArray<int64>; bMore: boolean = false);


function UPGet(const key: string; defaultvalue: string; bMore: boolean = false): string;overload;
function UPGet(const key: string; defaultvalue: int64; bMore: boolean = false): int64;overload;
function UPGet(const key: string; defaultvalue: boolean; bMore: boolean = false): boolean;overload;
function UPGet(const key: string; defaultvalue: double; bMore: boolean = false): double;overload;

procedure UPPut(const key: string; value: string; bMore: boolean = false);overload;
procedure UPPut(const key: string; value: int64; bMore: boolean = false);overload;
procedure UPPut(const key: string; value: boolean; bMore: boolean = false);overload;
procedure UPPut(const key: string; value: double; bMore: boolean = false);overload;

procedure APBegin;
procedure APEnd;
procedure UPBegin;
procedure UPEnd;



implementation

var
  cachedGAP: TAppParams = nil;
  cachedGUP: TAppParams = nil;
  cachedGAPTime, cachedGUPtime: ticker;
  applock: TCLXCriticalSection;
  gap, gup: TAppParams;
  apcount, upcount: nativeint;

procedure APBegin;
begin
  ecs(applock);
  if gap = nil then
    gap := NeedAppParams;

  inc(apcount);
end;
procedure APEnd;
begin
  dec(apcount);
  if apcount = 0 then begin
    NoNeedAppParams(gap);
    gap := nil;

  end;
  lcs(applock);

end;
procedure UPBegin;
begin
  ecs(applock);
  if gup = nil then
    gup := NeedUserParams;
  inc(upcount);
end;
procedure UPEnd;
begin
  dec(upcount);
  if upcount = 0 then begin
    NoNeedUSerParams(gup);
    gup := nil;
  end;
  lcs(applock);
end;



function APGet(const key: string; defaultvalue: string; bMore: boolean = false): string;
begin
  apbegin;
  try
    result := gap.GEtItemEx(key, defaultvalue);
  finally
    apend;
  end;
end;

function APGet(const key: string; defaultvalue: real; bMore: boolean = false): real;overload;
begin
  apbegin;
  try
    result := gap.GEtItemEx(key, defaultvalue);
  finally
    apend;
  end;
end;

function APGet(const key: string; defaultvalue: int64; bMore: boolean = false): int64;overload;
begin
  apbegin;
  try
    result := gap.GEtItemEx(key, defaultvalue);
  finally
    apend;
  end;
end;

function APGetIntegerArray(const key: string; bMore: boolean = false): TArray<int64>;overload;
begin
  apbegin;
  try
    result := gap.GetIntegerParameterArray(key);
  finally
    apend;
  end;
end;

function APGet(const key: string; defaultvalue: boolean; bMore: boolean = false): boolean;
begin
  result := strtobool(APGet(key, booltostr(defaultvalue), bMore));
end;

function UPGet(const key: string; defaultvalue: string; bMore: boolean = false): string;
begin
  upbegin;
  try
    result := gup.GEtItemEx(key, defaultvalue);
  finally
    upend;
  end;
end;

function UPGet(const key: string; defaultvalue: int64; bMore: boolean = false): int64;
begin
  upbegin;
  try
    result := gup.GEtItemEx(key, defaultvalue);
  finally
    upend;
  end;
end;

function UPGet(const key: string; defaultvalue: boolean; bMore: boolean = false): boolean;overload;
begin
  upbegin;
  try
    result := gup.GEtItemEx(key, defaultvalue);
  finally
    upend;
  end;

end;

function UPGet(const key: string; defaultvalue: double; bMore: boolean = false): double;overload;
begin
  upbegin;
  try
    result := gup.GEtItemEx(key, defaultvalue);
  finally
    upend;
  end;

end;

procedure APPut(const key: string; value: string; bMore: boolean = false);
begin
  apbegin;
  try
    gap.items[key].value := value;
  finally
    apend;
  end;
end;

procedure APPutIntegerArray(const key: string; a: TArray<int64>; bMore: boolean = false);
begin
  apbegin;
  try
    gap.SetIntegerParameterArray(key, a);
  finally
    apend;
  end;
end;


procedure UPPut(const key: string; value: string; bMore: boolean = false);
begin
  upbegin;
  try
    gup.items[key].value := value;
  finally
    upend;
  end;
end;


function GetApplicationParamsfileName: string;
begin
{$IFDEF WINDOWS}
  result := changefileext(DLLName,'.params');
  if not fileexists(result) then
    result := changefileext(DLLName,'.ini');
  {$IFDEF USE_PARAMS_FROM_WORKING_DIRECTORY}
    result := ExtractFileName(result);
  {$ENDIF}
{$ELSE}
//  result := System.IOUtils.TPath.GetHomePath;
  result := System.IOUtils.TPath.GetDocumentsPath+'app.params';
//  result := GetTempPath+'app.params';
{$ENDIF}
end;

function GetUserPAramsFileName: string;
begin
{$IFDEF DESKTOP}
  result := DLLNAme;
  result := ChangeFilePath(result, slash(TPath.GetHomePath))+'.app.params';
{$ELSE}
//  result := System.IOUtils.TPath.GetHomePath;
  result := slash(System.IOUtils.TPath.GetDocumentsPath)+'app.params';
//  result := GetTempPath+'app.params';
{$ENDIF}
end;


procedure ExpireCachedGap(bForce: boolean);
begin
  if bForce or (gettimesince(cachedGapTime) > 30000) then begin
    if cachedGap <> nil then begin
      cachedGAP.SaveToFile;
      cachedGAp.Free;
      cachedGap := nil;
    end;
  end;
end;
function NeedAppParams: TAppParams;
begin

  result := TAppParams.Create;
  result.AutoAdd := true;
  ecs(applock);
  ExpireCachedGAP(false);
  if cachedGAP <> nil then
    exit(cachedGAP);


  //showmessage(GetApplicationParamsfileName);
  if fileexists(GetApplicationParamsfileName) then
    result.LoadFromFile(GetApplicationParamsfileName)
  else
    result.SaveTofile(GetApplicationParamsfileName);

  //result.SaveOnFree := true;

end;

function NeedUserParams: TAppParams;
begin

  result := TAppParams.Create;
  result.AutoAdd := true;
  ecs(applock);
  //showmessage(GetApplicationParamsfileName);
  if fileexists(GetUserPAramsFileName) then
    result.LoadFromFile(GetUserPAramsFileName)
  else
    result.SaveTofile(GetUserPAramsFileName);

  //result.SaveOnFree := true;

end;


procedure NoNeedAppParams(nvpl: TAppParams);
begin
  nvpl.SaveTofile(GetApplicationParamsFileName);
{$IFDEF CACHEPARAMS}
  cachedGap := nvpl;
  cachedGapTime := GetTicker;
{$ELSE}
  nvpl.free;
  nvpl := nil;
{$ENDIF}
  lcs(applock);

end;

procedure NoNeedUserParams(nvpl: TAppParams);
begin
  nvpl.SaveTofile(GetUserPAramsFileName);
  nvpl.free;
  nvpl := nil;
  lcs(applock);

end;


procedure oinit;
begin
  ics(applock);
  apcount := 0;
  upcount := 0;

end;

procedure ofinal;
begin
  ExpireCachedGap(true);
  dcs(applock);
end;


procedure UPPut(const key: string; value: int64; bMore: boolean = false);overload;
begin
  UPPUt(key, inttostr(value), bMore);

end;
procedure UPPut(const key: string; value: boolean; bMore: boolean = false);overload;
begin
  UPPut(key, booltostr(value), bMore);
end;

procedure UPPut(const key: string; value: double; bMore: boolean = false);overload;
begin
  UPPut(key, floattostr(value), bMore);
end;




initialization
orderlyinit.init.RegisterProcs('ApplicationParams', oinit, ofinal);


{ TAppParams }


end.
