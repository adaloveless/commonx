unit WebScriptFunctions;

interface

uses classes, requestinfo, math, variantlist;

function DispatchScriptCommand(sTagBody,  sCommand: ansistring; params: TVariantList; rqInfo: TRequestInfo; var iStartRow, iStartCol: integer; out bHandled: boolean): ansistring;
function LoadStringFromFile(sFile: ansistring): ansistring;
function GeTMotherShipLib: ansistring;

const

  MonthName: array[1..12] of ansistring = ('January', 'February', 'March', 'April',
            'May', 'June', 'July', 'August', 'September', 'October', 'November',
            'December');

implementation

uses  sysutils, webstring, kiddienames, Dataobject, windows,
  webfunctions, webScriptMTDTInterface, ExceptionsX, webresource, errorresource, stringx,
  webscript, MTDTInterface, VersionInfo, WebConfig, systemx, rights, ExtendedScriptFunctions,
  CoreWebScriptFunctions;

function DispatchScriptCommand(sTagBody,  sCommand: ansistring; params: TVariantList; rqInfo: TRequestInfo; var iStartRow, iStartCol: integer; out bHandled: boolean): ansistring;
var
  iEndRow, iEndCol: integer;
  TTemp: TDateTime;
  sTemp: ansistring;
  sTemp2: ansistring;
  sLeft, sRight: ansistring;
  t: integer;
  bTemp: boolean;
  iTemp, iTemp1, iTemp2: integer;
  response: TMotherShipWebResponse;
  bench1, bench2: int64;
  bDispatched: boolean;
  doTemp: TDataobject;
begin
  response := rqInfo.Response;

  result := CoreWebScriptFunctions.DispatchScriptCommand(sTagBody, sCommand, params, rqInfo, iStartRow, iStartCol, bHandled, bDispatched);

  if bDispatched then
    exit;
end;


function LoadStringFromFile(sFile: ansistring): ansistring;
begin
  result := stringx.LoadStringFromFile(slash(webserverconfig.ExternalResourcePath)+sFile);
end;

function GeTMotherShipLib: ansistring;
begin
    result := '<script language="Javascript" src="Digital Tundralib.js?date='+FormatDateTime('YYYYMMDD', now)+'"></script>'
end;


end.
