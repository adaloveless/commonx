unit ExtendedProxyDispatch;


interface

uses RequestInfo, WebConfig, HTTPCLient, sysutils, webstring, stringx, stringx.ansi, webfunctions, dialogs, ExceptionsX, filecache;

function DispatchProxyRequest(rqInfo: TRequestInfo): boolean;


implementation

function DispatchProxyRequest(rqInfo: TRequestInfo): boolean;
begin

  result := false;
end;





end.
