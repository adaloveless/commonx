unit VersionInfo;

interface

uses requestInfo, inifile, stringx, systemx;


//server version
function GetServerVersion: string;
//tests destination version
function GetVersionStringHTTP(sDestName : string) : string;
//convenience wrappers
function GetUDSVersion: string;
function GetIPCD3ControllerVersion: string;
function GetFlashControllerVersion: string;
function GetUmbrellaVersion: string;



implementation

uses
  Dataobjectservices, webconfig, webfunctions, HTTPCLient, sysutils, classes, webstring;

const
  VERSION_INFO = '0.5';
//------------------------------------------------------------------------------
function GetUmbrellaVersion: string;
var
  sCrap: string;
begin
  result := LoadStringFromFile(slash(WebServerConfig.ExternalResourcePath)+'version_umbrella.txt');
  SplitString(result, #13#10, result, sCrap);
end;

function GetSingleLineFile(sFileName: string; iLIne: integer): string;
var
  sl: TStringList;
begin
  sl := nil;
  try
    sl := TStringList.create;
    sl.loadFromFile(sFileName);
    //return the requested line from the file
    if iLine < sl.count then begin
      result := sl[iLine];
    end;
  finally
    sl.free;
  end;

end;

//-----------------------------------------------------------------------------
function GetServerVersion: string;
begin
  result := VERSION_INFO;
end;


function GetVersionStringHTTP(sDestName : string) : string;
var
  dest : TNetConnection;
  http : THTTPClient;
begin
  try
    http := THTTPClient.Create;
    try
      dest := webserverconfig.Conn[sDestName];
      if NOT http.Get(AdjustWebPath(dest.URL)+'version.Digital Tundra', '') then begin
        result := 'No Response';
        exit;
      end;

      if http.ResultCode = '200' then
        result := http.InBody
      else
        result := 'Error : '+http.ResultCode;
    finally
      http.free;
    end;
  except
    result := Exception(ExceptObject).Message;
  end;
end;

//------------------------------------------------------------------------------

function GetUDSVersion: string;
begin
  result := GetVersionStringHTTP('UserDataServer');
end;

//------------------------------------------------------------------------------

function GetIPCD3ControllerVersion: string;
begin
  result := GetVersionStringHTTP('IPCD3Controller');
end;

//------------------------------------------------------------------------------

function GetFlashControllerVersion: string;
begin
  result := GetVersionStringHTTP('FlashController');
end;

initialization

finalization

end.
