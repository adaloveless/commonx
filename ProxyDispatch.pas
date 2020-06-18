unit ProxyDispatch;


interface

uses RequestInfo, WebConfig, HTTPCLient, sysutils, webstring, stringx, stringx.ansi, webfunctions, dialogs, ExceptionsX, filecache, ExtendedProxyDispatch;

function DispatchProxyRequest(rqInfo: TRequestInfo): boolean;


implementation

function DispatchProxyRequest(rqInfo: TRequestInfo): boolean;
var
  sDoc: ansistring;
  sTemp, sTemp2: ansistring;
begin
  sTemp := rqInfo.request.document;
  sTemp2 := sTemp;
  sTemp := stringReplace(stemp, 'http://ds2network.com/mother.dll', '', [rfIgnorecase]);
  if sTemp2 <> sTemp then begin
    rqInfo.request.document := sTemp;
  end;


  sTemp := stringReplace(stemp, 'http://www.ds2network.com/mother.dll', '', [rfIgnorecase]);
  if sTemp2 <> sTemp then begin
    rqInfo.request.document := sTemp;
  end;


  result := ExtendedProxyDispatch.dispatchProxyRequest(rqInfo);

  //host header templates
  if pos('studios.digitaltundra.com', lowercase(rqINfo.request['host'])) > 0  then begin
    if rqInfo.sessionid = 0 then
      rqINfo.request.addParam('template', 'studios', pcHeader);
    if rqINfo.request.document = '/' then
      rqINfo.request.document := '/dts_default.ms';
//    if rqINfo.request.document = '' then
//      rqINfo.request.document := '/default.ms';

  end;

  if pos('maudlinmusic.com', lowercase(rqINfo.request['host'])) > 0  then begin
    if rqInfo.sessionid = 0 then
      rqINfo.request.addParam('template', 'maudlin', pcHeader);
    if rqINfo.request.document = '/' then
      rqINfo.request.document := '/default.ms';
//    if rqINfo.request.document = '' then
//      rqINfo.request.document := '/default.ms';

  end;

  sDoc := lowercase(rqInfo.request.document);
  rqINfo.request.default('template', '');
  if copy(sDoc, 1, length('/sales/')) = '/sales/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/sales/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'sales', pcHeader);
  end;

  if copy(sDoc, 1, length('/support/')) = '/support/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/support/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'support', pcHeader);
  end;


  if copy(sDoc, 1, length('/shop/')) = '/shop/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/shop/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'shop', pcHeader);
  end;

  if copy(sDoc, 1, length('/maudlin/')) = '/maudlin/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/maudlin/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'maudlin', pcHeader);

  end;

  if copy(sDoc, 1, length('/studios/')) = '/studios/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/studios/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'studios', pcHeader);

  end;



  if copy(sDoc, 1, length('/out/')) = '/out/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/out/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'out', pcHeader);
  end;

  if copy(sDoc, 1, length('/none/')) = '/none/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/none/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'none', pcHeader);
  end;


  if copy(sDoc, 1, length('/popup/')) = '/popup/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/popup/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'popup', pcHeader);
  end;

  if copy(sDoc, 1, length('/market/')) = '/market/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/market/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'market', pcHeader);
  end;

  if copy(sDoc, 1, length('/phone/')) = '/phone/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/phone/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'phone', pcHeader);
    rqINfo.request.addParam('skin', '_phone', pcHeader);
  end;

  if copy(sDoc, 1, length('/wml/')) = '/wml/' then begin
    rqInfo.request.document := stringReplace(rqINfo.Request.Document, '/wml/', '/', [rfReplaceAll, rfIgnoreCase]);
    rqINfo.request.addParam('template', 'wml', pcHeader);
    rqINfo.request.addParam('skin', '_wml', pcHeader);
  end;



  rqInfo.SetupDefaultVarPool;





end;





end.
