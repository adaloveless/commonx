unit Spider;

interface

uses SharedObject, ManagedThread, ThreadManager, classes, comctrls, stdctrls, ExtCtrls, windows, commandprocessor, generics.collections, https;

const
  THREADS_MAX = 70;

type
  TMultiProgressCallback= procedure(iProgCount: integer; iBar: integer; iMin, iMax, iPos: integer) of object;

type
  TSpider = class;
  TSpiderSubCommand = class;



  TSpider = class(TCommand)
  private
    sl: TStringLIst;
    slAddr: TStringList;
    slCache: TStringList;
    FURL: string;
    FMAxLevel: integer;
    FSubThreads: TList<TSpiderSubCommand>;
    iStartedIndex: integer;
    iFinishedIndex: integer;
    FGetURLCount: integer;
    FNewLock: TlockQueuedObject;
    function GetMaxLevel: integer;
    function GetURL: string;
    procedure SetMaxLevel(const Value: integer);
    procedure SetURL(const Value: string);
    function GetURLs(idx: integer): string;
    procedure SetGetURLCount(const Value: integer);
    function GetURLCount: integer;
    function GetLevels(idx: integer): integer;
    function GetAllURLs: string;
    function GetAllAddr: string;

  public
    procedure DoExecute; override;
    procedure Init;override;
    destructor Destroy; override;

    property DoSubThreads: TList<TSpiderSubCommand> read FSubThreads;

    property URL: string read GetURL write SetURL;
    property MaxLevel: integer read GetMaxLevel write SetMaxLevel;
    function FoundURL(s: string; iLevel: integer): integer;
    function HasURL(s: string): boolean;
    procedure CacheSearch(sSearch: string; bResult: boolean);
    procedure CleanCache;
    procedure NewThread(sBaseURL, sURL: string; iLevels, iMaxLevels: integer);
    property URLs[idx: integer]: string read GetURLs;
    property Levels[idx: integer]: integer read GetLevels;
    property URLCount: integer read GetURLCount;
    property AllURLS: string read GetAllURLs;

    procedure FoundAddr(sAddr: string);
    property AllAddr: string read GetAllAddr;

    function TryLock(iTimeoutMS: integer): boolean; override;
    procedure Lock; override;
    procedure Unlock; override;

  end;

  TSpiderSubCommand = class(TCommand)
  private
    FURL, FBaseURL: string;
    FLevel, FMaxLevel: integer;
    Fspider: Tspider;
    slCache: TstringLIst;
    function GetBaseURL: string;
    function GetLevel: integer;
    function GetMaxLevel: integer;
    function GetSpider: TSpider;
    function GetURL: string;
    procedure SetBaseURL(const Value: string);
    procedure SetLevel(const Value: integer);
    procedure SetMaxLEvel(const Value: integer);
    procedure SetSpider(const Value: TSpider);
    procedure SetURL(const Value: string);
    procedure ExtractLinks(sURL: string);
  public
    constructor Create;override;
    destructor Destroy; override;

    procedure DoExecute; override;
    property Level: integer read GetLevel write SetLevel;
    property MaxLevel: integer read GetMaxLevel write SetMaxLEvel;
    property BaseURL: string read GetBaseURL write SetBaseURL;
    property URL: string read GetURL write SetURL;
    property Spider: TSpider read GetSpider write SetSpider;

    function HasURLCached(sURL: string): boolean;
    procedure CacheSearch(sSearch: string; bResult: boolean);
    procedure CleanCache;

  end;

implementation

uses HttpClient, WebString, stringx, sysutils;



procedure TSpider.CacheSearch(sSearch: string; bResult: boolean);
var
  i: integer;
begin
//  i := slCache.indexOf(sSearch);
//  if i >=0 then begin
//    slCache.delete(i);
//  end;

  slCache.AddObject(sSearch, pointer(bResult));


end;

procedure TSpider.CleanCache;
begin
  while slCache.count > 100 do begin
    slCache.delete(0);
  end;
end;

destructor TSpider.Destroy;
begin

  slCache.free;
  sl.free;
  slAddr.free;
  inherited;
  FNewLock.free;
end;

procedure TSpider.DoExecute;
var
  thr: TSpiderSubCommand;
begin
  inherited;
  thr := TSpiderSubCommand.create;
  try
    thr.spider := self;
    thr.BaseURL := url;
    thr.Level := 0;
    thr.MaxLevel := 3;
    thr.URL := URL;
    thr.start;
    thr.WaitFor;

  finally
    thr.free;
    thr := nil;
  end;


end;


procedure TSpider.FoundAddr(sAddr: string);
begin
  Lock;
  try
    if slAddr.IndexOf(sAddr) < 0 then
      slAddr.add(sAddr);
  finally
    Unlock;
  end;
end;

function TSpider.FoundURL(s: string; iLevel: integer): integer;
begin
  Lock;
  try
    if not self.HasURL(s) then begin
      UniqueString(s);
      sl.addObject(s, pointer(iLevel));
    end;

    result := sl.count;
  finally
    Unlock;
  end;
end;

function TSpider.GetAllAddr: string;
begin
  Lock;
  try
    result := slAddr.text;
    UniqueString(result);

  finally
    Unlock;
  end;

end;

function TSpider.GetAllURLs: string;
begin
  Lock;
  try
    result := sl.text;
    uniquestring(result);
  finally
    Unlock;
  end;
end;

function TSpider.GetLevels(idx: integer): integer;
begin
  Lock;
  try
    result := integer(sl.Objects[idx]);
  finally
    Unlock;
  end;
end;

function TSpider.GetMaxLevel: integer;
begin
  Lock;
  try
    result := FMaxLevel;
  finally
    Unlock;
  end;
end;

function TSpider.GetURL: string;
begin
  Lock;
  try
    result := FURL;
    uniquestring(result);
  finally
    Unlock;
  end;
end;

function TSpider.GetURLCount: integer;
begin
  Lock;
  try
    result := sl.count;
  finally
    Unlock;
  end;
end;

function TSpider.GetURLs(idx: integer): string;
begin
  Lock;
  try
    result := sl[idx];
    UniqueString(result);
  finally
    Unlock;
  end;
end;

function TSpider.HasURL(s: string): boolean;
var
  iCached: integer;
begin
  Lock;
  try
    iCached := slCache.IndexOf(s);
    if iCached >= 0 then
      result := integer(slCAche.objects[iCached])<>0
    else
      result := sl.IndexOf(s) >= 0;

    CacheSearch(s, true);
    CleanCache;



  finally
    Unlock;
  end;
end;

procedure TSpider.Init;
begin
  inherited;
  FNewLock := TLockQueuedobject.create;
  slCache := TStringlist.create;
  sl := TStringlist.create;
  slAddr := TStringLIst.create;

end;

procedure TSpider.Lock;
begin
  FNewLock.LockWrite;

end;

procedure TSpider.NewThread(sBaseURL, sURL: string; iLevels,
  iMaxLevels: integer);
var
  thr: TSpiderSubCommand;
begin

  if thr = nil then
    thr := TSpiderSubCommand.create;

  thr.BaseURL := sBaseURL;
  thr.URL := sURL;
  thr.Level := iLevels;
  thr.MaxLevel := iMaxLevels;
  thr.Spider := self;
  thr.Start;


end;

procedure TSpider.SetGetURLCount(const Value: integer);
begin
  FGetURLCount := Value;
end;

procedure TSpider.SetMaxLevel(const Value: integer);
begin
  Lock;
  try
    FMaxLEvel := value;
  finally
    Unlock;
  end;
end;

procedure TSpider.SetURL(const Value: string);
begin
  Lock;
  try
    FURL := value;
    Uniquestring(FURL);
  finally
    Unlock;
  end;
end;

{ TSpiderSubCommand }

procedure TSpiderSubCommand.DoExecute;
begin
  inherited;

  self.ExtractLinks(URL);

end;

function TSpiderSubCommand.GetBaseURL: string;
begin
  Lock;
  try
    result := FBaseURL;
    UniqueString(result);
  finally
    Unlock;
  end;
end;

function TSpiderSubCommand.GetLevel: integer;
begin
  Lock;
  try
    result := FLevel;
  finally
    Unlock;
  end;
end;

function TSpiderSubCommand.GetMaxLevel: integer;
begin
  Lock;
  try
    result := FMaxLevel;
  finally
    Unlock;
  end;
end;

function TSpiderSubCommand.GetSpider: TSpider;
begin
  Lock;
  try
    result := FSPider;
  finally
    Unlock;
  end;
end;

function TSpiderSubCommand.GetURL: string;
begin
  Lock;
  try
    result := FURL;
    uniquestring(result);

  finally
    Unlock;
  end;
end;

procedure TSpiderSubCommand.SetBaseURL(const Value: string);
begin
  Lock;
  try
    FBaseURL := value;
    UniqueString(FBaseURL);
  finally
    Unlock;
  end;
end;

procedure TSpiderSubCommand.SetLevel(const Value: integer);
begin
  Lock;
  try
    FLevel := value;
  finally
    Unlock;
  end;
end;

procedure TSpiderSubCommand.SetMaxLEvel(const Value: integer);
begin
  Lock;
  try
    FMaxLEvel := value;
  finally
    Unlock;
  end;
end;

procedure TSpiderSubCommand.SetSpider(const Value: TSpider);
begin
  Lock;
  try
    FSpider := value;
  finally
    Unlock;
  end;
end;

procedure TSpiderSubCommand.SetURL(const Value: string);
begin
  Lock;
  try
    FURL := value;
    UniqueString(FURL);
  finally
    Unlock;
  end;
end;

procedure TSpiderSubCommand.ExtractLinks(sURL: string);
var
{$IFDEF USE_OLD_HTTP}
  htp2 : THTTPClient;
{$ENDIF}
  f: textfile;
  sLeft, sRight: string;
  sDoc, sFoundURL: string;
  sBasePath: string;
  sTemp: string;
  iTemp: integer;
  iStarT: integer;
  iend: integer;
  sAddr: string;
  iCount: integer;
  t: integer;
  slLocalURLS: TStringList;
begin
  sBasePAth := ExtractURLPath(BaseURL);
  slLocalURLs := TStringlist.create;
  try
    spider.Lock;
    try
      if not (spider.HasURL(sURL)) then
        spider.FoundURL(sURL, Level);
    finally
      spider.unlock;
    end;


{$IFDEF USE_OLD_HTTP}
    htp2 := THTTPClient.create;
{$ENDIF}
    try
      try
{$IFDEF CHECK_HEAD}
        if htp2.HEad(sURL, '') then begin
          sTemp := lowercase(GetHeaderPAram(htp2.InHeader, 'Content-type'));
          if (sTemp <> 'text/html') and (sTemp <> '') then
            exit;
        end;
{$ENDIF}



{$IFDEF USE_OLD_HTTP}
        if htp2.Get(sUrl, '') then begin
          sDoc := htp2.InBody;
{$ELSE}

        if QuickHTTPsGet(sURL, sdoc) then begin
{$ENDIF}



{$IFDEF USE_OLD_HTTP}
          if (lowercase(GetHeaderPAram(htp2.InHeader, 'Content-type')) = 'text/html') then begin
{$ENDIF}

            sRight := sdoc;

            while pos('@', sRight) > 0 do begin
              iTEmp := pos('@', sRight);
        //      frmProgress.UpdateProgress(0, length(http.inbody), length(http.inbody) - length(sRight));

              //step backward
              while sRight[iTemp] in ['@','.','0'..'9', 'a'..'z', 'A'..'Z'] do begin
                dec(iTemp);
              end;
              inc(iTemp);
              iStart := iTemp;

              while sRight[iTemp] in ['@','.','0'..'9', 'a'..'z', 'A'..'Z'] do begin
                inc(iTemp);
              end;
              dec(iTemp);
              iEnd := iTemp;
              sAddr := copy(sRight, iStart, (iEnd-iStart)+1);
        //      frmProgress.StatusMessage('Scanning...'+sAddr);
              spider.FoundAddr(sAddr);
              SplitString(sRight, '@', sLeft, sRight);
            end;
{$IFDEF USE_OLD_HTTP}
          end;
{$ENDIF}


          if Level >  MaxLevel then
            exit;



          while splitstring(sDoc, 'href=', sLEft, sRight) do begin
            if (pos('"', sRight) > pos('.', sRight)) or (pos('"', sRight) < 1) then begin
              splitstring(sRight, '>', sFoundURL, sDoc);
              splitstring(sFoundURL, ' ', sFoundURL, sRight);
            end else begin
              splitString(sRight, '"', sLeft, sFoundURL);
              splitstring(sFoundURL, '"', sFoundURL, sDoc);
            end;

            if Zcopy(sFoundURL,0, length('javascript:')) <> 'javascript:' then
            begin
              if  (zpos('http://', lowercase(sFoundURL)) < 0)
              and (zpos('https://', lowercase(sFoundURL)) < 0) then
                sFoundURL := sBasePAth+sFoundURL;

              if IsvalidUrl(sFoundURL) then
                slLocalURLs.add(sFoundURL);
            end;

          end;


          //spider.lock;
          //try
            iCount := 0;
            for t:=slLocalURLs.count-1 downto 0 do begin
              if (HasURLCached(slLocalURLS[t])) then begin
                //self.Status := 'Extracting links: level '+inttostr(LEvel)+' -- found';
                //self.Load := length(htp2.inbody)-length(sDoc);
                //self.cycles := length(htp2.inbody)+1;
                slLocalURLS.Delete(t);

              end;
            end;

            //spider.lock;
            try
              for t:=0 to slLocalURLs.count-1 do begin
                //self.Status := 'Extracting links: level '+inttostr(LEvel)+' -- found';
                //self.Load := length(htp2.inbody)-length(sDoc);
                //self.cycles := length(htp2.inbody)+1;
                iCount := self.Spider.FoundURL(slLocalURLS[t], Level+1);
              end;

              //ExtractLinks(sFoundURL,Level, iLEvelLImit,'', sl);
              if ((slCache.count ) < (iCount div 2)) and (slCache.count < 4000) then begin
                slCache.text := spider.AllURls;
              end;
            finally
              //spider.Unlock;
            end;


          //finally
          //  spider.Unlock;
          //end;

        end;

      except
      end;
    finally
{$IFDEF USE_OLD_HTTP}
      htp2.free;
{$ENdIF}
    end;
  finally
    slLocalURLS.free;
  end;


end;


constructor TSpiderSubCommand.Create;
begin
  inherited;
  slCache := TStringlist.create;
end;

destructor TSpiderSubCommand.Destroy;
begin
  slCache.free;
  inherited;
end;

procedure TSpiderSubCommand.CacheSearch(sSearch: string; bResult: boolean);
var
  i: integer;
begin
  i := slCache.indexOf(sSearch);
  if i >=0 then begin
    slCache.delete(i);
  end;

  slCache.AddObject(sSearch, pointer(bResult));


end;

procedure TSpiderSubCommand.CleanCache;
begin
  while slCache.count > 5000 do begin
    slCache.delete(0);
  end;
end;

function TSpiderSubCommand.HasURLCached(sURL: string): boolean;
var
  iCached: integer;
begin
  iCached := slCache.IndexOf(sURL);
  if iCached >= 0 then
    result := integer(slCAche.objects[iCached])<>0
  else
    result := false;

  CacheSearch(sURL, true);
  CleanCache;
end;

function TSpider.TryLock(iTimeoutMS: integer): boolean;
begin
  Lock;
end;

procedure TSpider.Unlock;
begin
  FNewLock.UnlockWrite;
end;

{ TSpiderControl }




end.
