unit CommonRequests;


interface

uses numbers, RequestInfo, WebResource, WebString, HTTPClient, ErrorResource, systemx, xmltools, graphics, extctrls, jpeg, variants, sysutils, exceptions;


procedure WRQ_ServerTestGraphic(rqInfo: TRequestInfo);
procedure WRQ_Sleep(rqInfo: TRequestInfo);
procedure WRQ_StressException(rqInfo: TRequestInfo);
procedure WRQ_StressDTConn(rqInfo: TRequestInfo);
procedure WRQ_StressCPU(rqInfo: TRequestInfo);
procedure WRQ_StressHeap(rqInfo: TRequestInfo);
procedure WRQ_StressMemory(rqInfo: TRequestInfo);
procedure WRQ_StreamRemoteImage(rQInfo: TRequestInfo);
procedure WRQ_MailingListSubscribe(rqInfo: TRequestInfo);

procedure WRQ_Recurse82(rqInfo: TRequestInfo);

procedure WRQ_JavaCircle(rqInfo: TRequestInfo);
procedure WRQ_Circle(rqInfo: TRequestInfo);
procedure WRQ_BrowserTest(rqInfo: TRequestInfo);
procedure WRQ_ServerStatus(rqInfo: TRequestInfo);
procedure WRQ_FileTest(rqInfo: TRequestInfo);
procedure WRQ_SpeedTest(rqInfo: TRequestInfo);
procedure WRQ_TextEcho(rqInfo: TRequestInfo);
procedure WRQ_StringTest(rqInfo: TRequestInfo);
procedure WRQ_PrimeCalc(rqInfo: TRequestInfo);
procedure WRQ_GET_GetUDS(rqInfo: TRequestInfo);
procedure WRQ_HeapStatus(rqInfo: TRequestInfo);
procedure WRQ_NotFound(rqInfo: TRequestInfo);
procedure WRQ_ParameterEcho(rqInfo: TRequestInfo);
procedure WRQ_DevHalt(rqInfo: TRequestInfo);
//procedure WebServerError(rqInfo: TRequestInfo; sMessage: ansistring;bException: boolean); overload;
//procedure WebServerError(rqInfo: TRequestInfo; sMessage: ansistring);overload;
procedure WebServerError(rqInfo: TRequestInfo; e: exception);
//procedure ShowLastServerError(rqInfo: TRequestInfo; bFramed: boolean; iErrorCode: integer; sFriendlyHeading, sTechDetails: ansistring); overload;
//procedure ShowLastServerError(rqInfo: TRequestInfo; sFriendlyHeading: ansistring); overload;
//procedure ShowLastServerError(rqInfo: TRequestInfo); overload;
procedure WRQ_JavaRedirect(rqInfo: TRequestInfo);
procedure WRQ_UpdateProxy(rqInfo: TRequestInfo);
procedure SendJavaRedirect(rqInfo: TRequestInfo; sUrl: ansistring; breakframes: boolean = true; bWait: boolean = false);overload;
procedure SendJavaRedirect(rqInfo: TRequestInfo; sUrl: ansistring; altMessage: ansistring; breakframes: boolean = true; bWait: boolean = false);overload;
procedure SendAJJavaRedirect(rqInfo: TRequestInfo; sUrl: ansistring; altMessage: ansistring = ''; breakframes: boolean = true; bWait: boolean = false);overload;
procedure CloseWindow(rqInfo: TRequestInfo; ifName: ansistring; sURL: ansistring);
function WRQ_DoTest(Rqinfo: TRequestInfo): boolean;

procedure WRQ_ShowServerConfig(rqInfo: TRequestInfo);
procedure WRQ_ReloadServerConfig(rqInfo: TRequestInfo);
procedure ConfigTableRecord(rqInfo: TRequestInfo; sLeftValue, sRightvalue: ansistring);
procedure ConfigTableRecord2(rqInfo: TRequestInfo; sLeftValue, sRightvalue: ansistring);
procedure ConfigTableRecord3(rqInfo: TRequestInfo; sLeftValue, sRightvalue: ansistring);
procedure ConfigTableRecord5(rqInfo: TRequestInfo; sName, sType, sURL, sStatus: ansistring);
procedure WRQ_ShowVersionInfo(rqInfo: TRequestInfo);

procedure WRQ_GET_SuperSession(rqInfo: TRequestInfo);
procedure WRQ_POST_SuperSession(rqInfo: TRequestInfo);
procedure WRQ_GET_ClearSuperSession(rqInfo: TRequestInfo);

function ExternalResourceStatus(sPath: ansistring): ansistring;
function HttpLinkStatus(sURL: ansistring): ansistring;
function RDTPLinkStatus(sLinkName: ansistring): ansistring;


implementation

uses
  WebFunctions, Classes, scktcomp, DataObjectServices, ErrorHandler, 
  Webconfig, FileCtrl, DataObject, MTDTInterface, VersionInfo, windows,
  SimpleAbstractConnection, SimpleMail, better_sockets,
  RequestDispatcher;
//------------------------------------------------------------------------------
procedure WRQ_NotFound(rqInfo: TRequestInfo);
//Generates a 404 Not Found error message in HTML.
//This page is displayed if a request passes through the dispatcher without being handled.
begin
  rqInfo.Response.ResultCode := 404;
  rqInfo.response.contenttype := 'text/html';
  //rqInfo.response.content.clear;
  with rqinfo.Response.Content do begin
    add('<HTML>');
    add(WebTitle('404'));
    if copy(rqInfo.request.document, 1, 6) ='/help_' then
      add('<font size="4" face="Helvetica, Arial">Sorry, help is not available for this topic.<br></font>')
    else
      add('<H1>The page you are looking for cannot be found (404)</H1>');
    add('<BODY>');
    add('The document:<B>'+rqInfo.Request.document+ '</B><BR> was not found. Method:"<B>'+rqInfo.request.command+'</B>".<BR><BR>');

    add('</BODY>');
    add('</HTML>');
  end;

end;
//------------------------------------------------------------------------------
procedure WRQ_DevHalt(rqInfo: TRequestInfo);
//Halts the server, active only in debug mode.
begin
  exit;
  with rqInfo.Response.Content do begin
    add('<HTML>');
    add(WebTitle('Digital Tundra Web Learner Server 4.0 (ALPHA)'));
    add('<H1>Please don''t leave.... we''ll miss you!</H1>');
    add('<BODY>');

    add('</BODY>');
    add('</HTML>');
  end;
end;

//------------------------------------------------------------------------------
procedure WRQ_HeapStatus(rqInfo: TRequestInfo);
//w: heap_status.Digital Tundra
//Displays a web page showing the status of the memory heap.  Should return
//a bunch of ZEROs if the multi-threaded memory manager is in place.
begin
  with rqInfo.Response.Content do begin
    add('<HTML>');
    add(WebTitle('Mothership web server'));
    add(WebHeapStatus);
    add('<br>X='+rQInfo.request['x']);
    add('</HTML>');
  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_ParameterEcho(rqInfo: TRequestInfo);
//For debuging, generates a webpage that simply echos all parameters passed to it.  Used to test whether or not parameters are being parsed by the engine propertly or not.
var
  t: integer;
begin
  rqInfo.NoHit := true;
  with rqInfo.response.content do begin
    add('<HTML>');
    add(WebTitle('Mothership WebServer'));
    add('<H2>Mothership</H2><BR>');
    add('The following lines should have responded back all parameters passed to the Web Server.  Please Ensure that they all make sense.<BR><BR>');
  end;

  for t:= 0 to rqInfo.Request.ParamCount-1 do begin
    rqInfo.Response.Content.add(
      '<STRONG>'+
      rqInfo.Request.ParamNames[t]+' = </STRONG>'+
      rqInfo.Request.Params[rqInfo.Request.ParamNames[t]]+'<BR>')
  end;
  rqInfo.Response.Content.add('</HTML>');
end;
//------------------------------------------------------------------------------
//procedure WebServerError(rqInfo: TRequestInfo; sMessage: ansistring);
//begin
//  WebServerError(rqInfo, sMessage, false);
//end;
//------------------------------------------------------------------------------
procedure WebServerError(rqInfo: TRequestInfo; e: exception);
//Displays the graphical server error page
var
  slError: TSTringList;
  slBody:TStringList;
  sPath: ansistring;
  bFatal: boolean;
  sOldBody: ansistring;
begin
  sOldBody := rqINfo.response.content.text;
  sOldBody := stringReplace(sOldBody, '[[[', '[_[', [rfreplaceall]);

  rqInfo.response.varpool['oldbody'] := sOldBody;

  rqInfo.response.Default('usermessage', '');
  rqInfo.response.Default('techmessage', '');
  rqInfo.response.Default('errornumber', '-1');

  if e is EUserError then begin
    rqInfo.response.default('user_message', EUserError(e).UserMessage);
    rqInfo.response.default('error_message', '');
    bFatal := false;
  end else
  if e is ENewException then begin
    rqInfo.response.default('user_message', EUserError(e).UserMessage);
    rqInfo.response.default('error_message', EUserError(e).Message);
    bFatal := true
  end else begin
    rqInfo.response.default('user_message', 'An error has occurred');
    rqInfo.response.default('error_message', e.message);
    bFatal := true;
  end;

  if pos('session not found', lowercase(rQInfo.response.varpool['error_message'])) > 0 then begin
    rqInfo.response.location := 'out/login.ms';
    exit;
  end;

  if pos(': session', lowercase(rQInfo.response.varpool['error_message'])) > 0 then begin
    rqInfo.response.location := 'out/login.ms';
    exit;
  end;


  rqInfo.response.ResultCode := 200;

  if bFatal then
    rqInfo.response.varpool['fatal'] := 'true'
  else
    rqInfo.response.varpool['fatal'] := 'false';


//  rqInfo.response.content.add(sMessage);
//  exit;


  //if a sessionID was not provided`then default to 0
  if not rqInfo.Request.HasParam('sessionid') then
    rqInfo.Response.VarPool['sessionid'] := '0';

  //Erase any webpage that was being built
  rqInfo.response.Content.clear;

  //Get the page from the document -- for display purposes
  sPath := ExtractFilePath(rqInfo.Request.Document);
  sPath := lowercase(sPath);




  slBody := TStringList.Create;
  slError := TStringList.create;
  try
    //load the error template
    LoadWebResource(rqInfo,  'error_framed.html');

    //setup Dynamic Variables for error template
    with rqInfo.Response do begin
      VarPool['page_title'] := WEB_APPLICATION_NAME+' - Error';
      VarPool['section_title'] := 'Error';
    end;

    //Throw in the error message
//    if bFatal then begin
//      slBody.add('<H2>Server Error</H2><BR>');
//      slBody.add('There was an internal server error processing your request.<BR>');
//    end;
//    slBody.add(sMessage+'<BR>');

    //LoadWebResourceBody(rqInfo, 'error.html');
    //Put the error message into the error template
    MergeAtToken(slError, 'body', slBody);
    //Put the error template into the response
    MergeAtToken(rqInfo.Response.content, 'body', slError);

    //Reforce processing of dynamic variables
    rqInfo.Response.ProcessDynamicVariables;

    //Setup default error code
    rqInfo.Response.ResultCode := 500;
  finally
    slBody.Free;
    slError.free;
  end;
end;
//------------------------------------------------------------------------------
//procedure ShowLastServerError(rqInfo: TRequestInfo); overload;
//begin
//  if uppercase(rqInfo.request.command) = 'GET' then
//    ShowLastServerError(rqInfo, 'Unable to Display Page')
//  else
//    ShowLastServerError(rqInfo, 'There was a problem submitting your information');
//
//end;
//------------------------------------------------------------------------------
//procedure ShowLastServerError(rqInfo: TRequestInfo; bFramed: boolean; iErrorCode: integer; sFriendlyHeading, sTechDetails: ansistring);
//var
//  sMessage: ansistring;
//  sResourceMessage: ansistring;
//  sDel: ansistring;
//  sLastError: ansistring;
//begin
//  try
//    rqInfo.response.varpool['error_message'] := '';
//    sResourceMessage := '';
//    sLastError := GetLastServerErrorMessage;
//
//    //get a stock message from the error resource
//    if iErrorCode<2000 then
//      sResourceMessage := ErrorRes.ErrorMessages[iErrorCode];
//
//
//    if sResourceMessage <> '' then
//      sDel := '<BR>';
//    if (sResourceMessage <> sFriendlyHeading) and (sFriendlyHeading <> '') then
//      sFriendlyHeading := sFriendlyHeading+sDel+sResourceMessage;
//    if (sFriendlyHeading = '') and (iErrorCode<>0) then
//      sFriendlyHeading := sResourceMessage;
//
//    //if the error code is betweeen 1000 and 1500 display the server error message
//    //as the friendly heading
//
//    if (iErrorcode >1000) and (iErrorCode<=1500) then
//      sFriendlyHeading := sLastError;
//
//    rqInfo.response.varpool['usermessage'] := sFriendlyHeading;
//    rqInfo.response.varpool['techmessage'] := sTechDetails;
//    rqInfo.response.varpool['errorcode'] := inttostr(iErrorCode);
//    rqInfo.response.varpool['page_title'] := '';
//    rqInfo.response.varpool['section_title'] := '';
//    rqInfo.response.varpool['page_message'] := '';
//
//    //default response to `body`
//    rqInfo.response.content.text := '`body`';
//
//    if (not bFramed) and(2=1) then begin
//      rqInfo.response.varpool['error_true'] := '0000';
//      LoadWebResource(rqInfo, rqInfo.response.content, 'main_template_passive.html');
//      LoadwebResourceBody(rqInfo, 'new_error.ps2');
//    end else begin
//      try
//        LoadWebResource(rqInfo, rqInfo.response.content, 'error_framed.html');
//      except
//        rqInfo.response.content.text := '[[[error_message]]]';
//      end;
//    end;
//
//
//    //special error processing
////    case iErrorCode of
//(*      933: LoadWebResourceBody(rqInfo, 'error_busy.html');
//      932: LoadWebResourceBody(rqInfo, 'error_busy.html');
//      925: LoadWebResourceBody(rqInfo, 'error_account.html');
//      924: rqInfo.response.location := 'change_account.ms';
//      916: LoadWebResourceBody(rqInfo, 'already_running_courseware.html');
//      107: begin
//             LoadMainShell(rqinfo, rqInfo.response.content, 0, '', 'Already Logged In', 'Already Logged In');
//             LoadWebResourceBody(rqInfo, 'already_logged_in.html');
//           end;
//      114: LoadWebResourceBody(rqInfo, 'license_limit_exceeded.html');
//      115: LoadWebResourceBody(rqInfo, 'service_not_activated.html');
//      117: LoadWebResourceBody(rqInfo, 'session_expired.html');
//      118: LoadWebResourceBody(rqInfo, 'feature_unavailable_nointernet.html');*)
////    else
//      //generic errors
//      LoadWebResourceBody(rqInfo, 'generic_error.html');
////      LoadwebResource('generic_error.html', rqInfo.response.content);
////    end;
//
//    rqInfo.response.varpool['error_message'] := sLastError;
//
//  //  rqInfo.Response.content.Add(rqInfo.Response.VarPoolStatus);
//
//    rqInfo.Response.ResultCode := 500;
//
//    rqInfo.response.contenttype := 'text/html';
//    //rQInfo.response.NeedsProcessing := false;
//    rqInfo.Response.ProcessDynamicVariables;
//  except
//    on e: Exception do begin
//      rqInfo.response.content.text := 'Error displaying error page: '+e.Message;
//    end;
//  end;
//end;
//
procedure WRQ_JavaRedirect(rqInfo: TRequestInfo);
//a: Jason Nelson
//w: Java_redirect.Digital Tundra
//Sends a redirect to the browser, using javascript.  Useful for forwarding the user
//to a page in cases where you don't want the user to be able to click BACK to go back.
begin
  //if the result was falsified (which occurs on launch), force the session state
  //back to 1.
  rqInfo.request.Default('altmessage', 'You are being redirected to another page.');
  rqINfo.SetupDefaultVarPool;


  //get/default the breakframes parameter
  if not rqInfo.response.hasvar('breakframes') then
    rqInfo.response.varpool['breakframes'] := 'true';
  rqInfo.response.varpool['breakframes'] := lowercase(rqInfo.response.varpool['breakframes']);

  with rqInfo.Response.content do begin
    add('<HTML>');
    add('<HEAD>');
    add('<TITLE>Redirect</TITLE>');

    //redirecting
    add('<SCRIPT Language="Javascript" DEFER>');
    add('<!--');
    //include this java if breakframes is TRUE
    add('function Goto() {');
      add('  window.location = "'+MorphURL(rqinfo.Request['return']+'"'));
    add('};');

    add('function onPageLoad(){');
    if rqInfo.response.varpool['breakframes'] = 'true' then begin
      add('if (window != top) top.location.href = "'+MorphURL(rqinfo.Request['return']+'"'));
      add('else window.location = "'+MorphURL(rqinfo.Request['return']+'"'));
    end else begin
      if rqInfo.request.hasparam('wait') then begin
        add(' setTimeout(''Goto();'', '+inttostr(Random(8000)+5000)+');')
      end else begin
        add('  window.location = "'+MorphURL(rqinfo.Request['return']+'"'));
      end;
    end;


    add('}');
    add('//-->');
    add('</SCRIPT>');
//    add('<meta content="blendTrans(duration=0.5)" http-equiv="Page-Enter">');
//    add('<meta content="blendTrans(God...duration=0.5)" http-equiv="Page-Exit">');
    add('</Head>');
    add('<body bgcolor="#FFFFFF" onLoad="onPageLoad()">');
    add('<table width="400" border="0" cellspacing="0" cellpadding="0" align="center">');
    add('  <tr>');
    add('    <td><center>');
    add('<p><b><font face="Helvetica, Arial" size="4"><BR><BR><BR>Please Wait...<br> </font></b>');
    add('<font size="2" face="arial, helvetica">[[[altmessage]]]</font><BR>');
    add('<font size="2" face="arial, helvetica"><a href="[[[return]]]">Click here</a> if you do not see the next page in 10 seconds.</font><BR>');
    add('  <BR><img src="images/working.gif" width="447" height="25"></p>');
    add('</center></td></tr></table>');
    add('</body>');
    add('</html>');
//    add('</HTML>');
  end;

end;

procedure WRQ_AJJavaRedirect(rqInfo: TRequestInfo);
//a: Jason Nelson
//w: Java_redirect.Digital Tundra
//Sends a redirect to the browser, using javascript.  Useful for forwarding the user
//to a page in cases where you don't want the user to be able to click BACK to go back.
begin
  //if the result was falsified (which occurs on launch), force the session state
  //back to 1.
  rqInfo.request.Default('altmessage', 'You are being redirected to another page.');
  rqINfo.SetupDefaultVarPool;


  //get/default the breakframes parameter
  if not rqInfo.response.hasvar('breakframes') then
    rqInfo.response.varpool['breakframes'] := 'true';
  rqInfo.response.varpool['breakframes'] := lowercase(rqInfo.response.varpool['breakframes']);

  with rqInfo.Response.content do begin
    //redirecting
    add('<SCRIPT Language="Javascript" DEFER>');
    add('<!--');
    //include this java if breakframes is TRUE
    add('function Goto() {');
      add('  window.location = "'+MorphURL(rqinfo.Request['return']+'"'));
    add('};');

    add('function onPageLoad(){');
    if rqInfo.response.varpool['breakframes'] = 'true' then begin
      add('if (window != top) top.location.href = "'+MorphURL(rqinfo.Request['return']+'"'));
      add('else window.location = "'+MorphURL(rqinfo.Request['return']+'"'));
    end else begin
      if rqInfo.request.hasparam('wait') then begin
        add(' setTimeout(''Goto();'', '+inttostr(Random(8000)+5000)+');')
      end else begin
        add('  window.location = "'+MorphURL(rqinfo.Request['return']+'"'));
      end;
    end;


    add('}');
    add('//-->');
    add('</SCRIPT>');
    add('<table width="400" border="0" cellspacing="0" cellpadding="0" align="center">');
    add('  <tr>');
    add('    <td><center>');
    add('<p><b><font face="Helvetica, Arial" size="4"><BR><BR><BR>Please Wait...<br> </font></b>');
    add('<font size="2" face="arial, helvetica">[[[altmessage]]]</font><BR>');
    add('<font size="2" face="arial, helvetica"><a href="[[[return]]]">Click here</a> if you do not see the next page in 10 seconds.</font><BR>');
    add('  <BR><img src="images/working.gif" width="447" height="25"></p>');
    add('</center></td></tr></table>');
    add('<SCRIPT language="Javascript" DEFER>');
    add('<!--');
    add(' onPageLoad();');
    add('');
    add('//-->');
    add('</SCRIPT>');



//    add('</HTML>');
  end;

end;

//------------------------------------------------------------------------------
procedure SendJavaRedirect(rqInfo: TRequestInfo; sUrl: ansistring; breakframes: boolean = true; bWait: boolean = false);
begin
  SendJavaRedirect(rqInfo, sUrl, '', breakframes, bWait);
end;

procedure SendAJJavaRedirect(rqInfo: TRequestInfo; sUrl: ansistring; altMessage: ansistring = ''; breakframes: boolean = true; bWait: boolean = false);overload;
begin
  rqInfo.response.content.clear;
  rqInfo.Request.AddParam('return', sUrl, pcContent);
  if breakframes then
    rqInfo.Request.AddParam('breakframes', 'true', pcContent)
  else
    rqInfo.Request.AddParam('breakframes', 'false', pcContent);

  if bWait then
    rqInfo.Request.AddParam('wait', '4000', pcContent);

  if altmessage = '' then
    altMessage := 'You are being redirected to another web page';
  rqInfo.Request.AddParam('altmessage', altMessage, pcContent);
  rqInfo.Request.AddParam('url', sURL, pcContent);


  rqInfo.SetupDefaultVarpool;

  LoadWebResource(rqInfo, 'aj_java_redirect.html')
//  WRQ_JavaRedirect(rqInfo);
end;

procedure SendJavaRedirect(rqInfo: TRequestInfo; sUrl: ansistring; altMessage: ansistring; breakframes: boolean = true; bWait: boolean = false);
//a: Jason Nelson
//Sends a redirect to the browser, using javascript.  Useful for forwarding the user
//to a page in cases where you don't want the user to be able to click BACK to go back.
//p: BreakFrames: Indicates that when redirecting, the browser should redirect the parent frame.
//p: bWait: Indicates that the page should wait 4 seconds before redirecting.
begin
  rqInfo.response.content.clear;
  rqInfo.Request.AddParam('return', sUrl, pcContent);
  if breakframes then
    rqInfo.Request.AddParam('breakframes', 'true', pcContent)
  else
    rqInfo.Request.AddParam('breakframes', 'false', pcContent);

  if bWait then
    rqInfo.Request.AddParam('wait', '4000', pcContent);

  if altmessage = '' then
    altMessage := 'You are being redirected to another web page';
  rqInfo.Request.AddParam('altmessage', altMessage, pcContent);
  rqInfo.Request.AddParam('url', sURL, pcContent);


  rqInfo.SetupDefaultVarpool;

  LoadWebResource(rqInfo, 'java_redirect.html')
//  WRQ_JavaRedirect(rqInfo);
end;
//------------------------------------------------------------------------------
procedure WRQ_ServerTestGraphic(rqInfo: TRequestInfo);
  function TestHTTPLInk(sDest: ansistring): boolean;
  var
    http: THTTPClient;
  begin
    http := THTTPClient.create;
    try
      result := http.get(adjustwebpath(WebServerconfig.conn[sDest].URL)+WebServerconfig.conn[sDest].TestURL,'');
      
    finally
      http.free;
    end;

  end;
var
  img: TImage;
  jpg: TJpegImage;
  sDest: ansistring;
  bGood: boolean;
  bUnknown: boolean;
begin
  bGood := false;
  bUnknown := true;
  img := Timage.create(nil);
  try
    img.width := 640;
    img.height := 20;
    img.canvas.fillrect(Rect(0,0,img.width, img.height));
    img.canvas.lock;
    jpg := TJpegImage.create;
    try
      try
        sDest := rqINfo.request['Dest'];

        if WebServerconfig.conn[sDest].NoTest then
          bUnknown := true
        else
        if WebServerConfig.conn[sDest].Middleware then begin
//          bGood := DOSVPool.ServersByName[WebServerConfig.conn[sDest].Name].Server.Alive;
          bGood := false;
          bUnknown := false;
        end else begin
          bGood := TestHTTPLInk(sDest);
          bUnknown := false;
        end;


        if bUnknown then begin
          jpg.LoadFromFile(slash(WebServerConfig.ExternalResourcepath)+'images\normal.jpg');
//          img.canvas.draw(0,0,jpg);
//          img.canvas.textOut(18,2, 'Unknown/Untested');
        end else
        if bGood then begin
          jpg.LoadFromFile(slash(WebServerConfig.ExternalResourcepath)+'images\smiley.jpg');
//          img.canvas.draw(0,0,jpg);
          img.canvas.textOut(18,2, 'OK!');
        end else begin
          jpg.LoadFromFile(slash(WebServerConfig.ExternalResourcepath)+'images\frowny.jpg');
//          img.canvas.draw(0,0,jpg);
//          img.canvas.textOut(18,2, 'Down');
        end;
      except
        on E: exception do begin
          jpg.LoadFromFile(slash(WebServerConfig.ExternalResourcepath)+'images\frowny.jpg');
//          img.canvas.draw(0,0,jpg);
//          img.canvas.textOut(18,2, 'Exception:'+E.Message);
        end;
      end;

//      StreamImageAsJpeg(rqINfo, img);

    finally
      img.canvas.unlock;
      jpg.free;
    end;


  finally
    img.free;
  end;

end;
procedure WRQ_ShowServerConfig(rqInfo: TRequestInfo);
//w: show_server_config.Digital Tundra
//a: Jason Nelson
//Shows the configuration of the Tier.  Tests all connections to all external tiers.
const
  STATUS = '<BR><B><font size="2">Status: </font></B>';
var
  t: integer;
  dest : TNetConnection;
begin
  try
    with rqInfo.response.content do begin
      add('<HTML><BODY>');
      add('<TABLE WIDTH="100%" BORDER="0">');
      add('<TR><TD COLSPAN=2 BGCOLOR="F7C7C7">');
      add('<B>Web Server Configuration Information</B><BR>');
      add('</TD></TR>');
      //Main Info

      ConfigTableRecord(rqInfo, 'configFile', WebServerConfig.ConfigFile+' (<a href="reload_server_config.Digital Tundra">reload</a>)');
      //EchoPageParameters
      if WebServerConfig.EchoPageParameters then
        ConfigTableRecord(rqInfo, 'EchoPageParameters', 'On')
      else
        ConfigTableRecord(rqInfo, 'EchoPageParameters', 'Off');
      //ResourcePath
      rqInfo.response.SendChunk;
      ConfigTableRecord(rqInfo, 'ExternalResourcePath', WebServerConfig.ExternalResourcePath+STATUS+ExternalResourceStatus(WebServerConfig.ExternalResourcePath));
      rqInfo.response.SendChunk;
      ConfigTableRecord(rqInfo, 'FarmRouter', WebServerConfig.FarmRouter+STATUS+HttpLinkStatus(WebServerConfig.FarmRouter));
      add('</Table>');
      rqInfo.response.SendChunk;


      //Destination Information
      add('<B>Destinations ('+inttostr(WebServerConfig.ConnectionCount)+')</B><BR>');

      add('<TABLE WIDTH="100%" BORDER="0" BGCOLOR="#BBBBBB">');
      for t := 0 to WebServerConfig.ConnectionCount-1 do begin

        dest :=WebServerConfig.DestByIndex[t];
        if dest.MiddleWare then begin
          ConfigTableRecord5(rqINfo, dest.Name, 'RDTP', dest.Host+':'+dest.Endpoint, '<img src="server_test_graphic.jpg?dest='+EncodeWEbString(dest.Name)+'">');
        end
        else begin
          if dest.Citrix then begin
           ConfigTableRecord5(rqINfo, dest.Name, 'CITRIX', dest.Host+':'+dest.Endpoint, '<img src="server_test_graphic.jpg?dest='+EncodeWEbString(dest.Name)+'">');
          end
          else begin
           ConfigTableRecord5(rqINfo, dest.Name, 'HTTP', dest.URL, '<img src="server_test_graphic.jpg?dest='+EncodeWEbString(dest.Name)+'">');
          end;
        end;
        rqInfo.response.SendChunk;
      end;
      add('</table>');
      add('</BODY></HTML>');
    end;
    rqInfo.response.SendFooter;
  except
    rqInfo.response.content.add('Exception: '+Exception(ExceptObject).Message);
  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_ServerStatus(rqInfo: TRequestInfo);
//a: Jason Nelson
//w: server_status.Digital Tundra
//Shows the status of all threads in the web server.  Includes page names and instantiated Data objects if
//extrainfo=true is passed as a parameter to the page.
var
  t: integer;
  dest : TNetConnection;
  http: THTTPClient;
  result : boolean;
begin
  result := true;
  rqInfo.response.NoDebug := true;
  rqInfo.response.contenttype := 'text/plain';
  try
    http := nil;
    try
      http := THTTPClient.create;
      with rqInfo.response.content do begin
        FOR t := 0 to WebServerConfig.ConnectionCount-1 DO BEGIN
          dest :=WebServerConfig.DestByIndex[t];
          try
            if dest.Middleware then begin
              result := true;
            end
            else begin
              IF dest.Citrix THEN BEGIN
                //cannot test
              END
              ELSE BEGIN
                if not http.Get(adjustwebpath(dest.url)+'load.Digital Tundra', '') then begin
                  add(Dest.name+' down');
                  result := false;
                end;
              END;
            end;
          except
            add(Dest.name+' down');
            result := false;
          end;
        END;//for
        if result then
          add('OK');

      end;//with
    finally


      http.free;
    end;
  except
    rqInfo.response.resultcode := 500;
    rqInfo.response.Content.clear;
    rqInfo.response.content.add('Exception: '+Exception(ExceptObject).Message);
  end;
end;
//------------------------------------------------------------------------------
procedure ConfigTableRecord(rqInfo: TRequestInfo; sLeftValue, sRightvalue: ansistring);
//a: Jason Nelson
//Helper function for show_server_config.Digital Tundra
begin
  with rqInfo.response.content do begin
    add('<TR><TD WIDTH="200" BGCOLOR="#F7F7F7">');
    Add(sLeftValue);
    add('</TD>');
    add('<TD WIDTH="99%" BGCOLOR="#F7F7F7"><font face="Helvetica, Arial" size=2">');
    if sRightValue='' then
      add('<font color="#FF0000">undefined</font>');
    Add(sRightValue);
    add('</font></TD></TR>');
  end;
end;
//------------------------------------------------------------------------------
procedure ConfigTableRecord2(rqInfo: TRequestInfo; sLeftValue, sRightvalue: ansistring);
//a: Jason Nelson
//Helper function for show_server_config.Digital Tundra
begin
  with rqInfo.response.content do begin
    add('<TR><TD WIDTH="20%" BGCOLOR="#c7c7F7">');
    Add(sLeftValue);
    add('</TD>');
    add('<TD WIDTH="80%" BGCOLOR="#c7c7F7">');
    if sRightValue='' then
      add('<font color="#FF0000">undefined</font>');
    Add(sRightValue);
    add('</TD></TR>');
  end;
end;

procedure ConfigTableRecord5(rqInfo: TRequestInfo; sName, sType, sURL, sStatus: ansistring);
//a: Jason Nelson
//Helper function for show_server_config.Digital Tundra
begin
  with rqInfo.response.content do begin
    add('<TR><TD WIDTH="10%" BGCOLOR="#FFFFFF">');
    Add(sName);
    add('</TD>');

    add('<TD WIDTH="40" BGCOLOR="#FFFFFF">');
    Add(sType);
    add('</TD>');

    add('<TD WIDTH="50%" BGCOLOR="#FFFFFF">');
    Add(sURL);
    add('</TD>');

    add('<TD WIDTH="1" BGCOLOR="#FFFFFF">');
    Add(sStatus);
    add('</TD></TR>');

  end;
end;

//------------------------------------------------------------------------------
procedure ConfigTableRecord3(rqInfo: TRequestInfo; sLeftValue, sRightvalue: ansistring);
//a: Jason Nelson
//Helper function for show_server_config.Digital Tundra
begin
  with rqInfo.response.content do begin
    add('<TR><TD WIDTH="35%" BGCOLOR="#F7F7F7">');
    Add(sLeftValue);
    add('</TD>');
    add('<TD  BGCOLOR="#F7F7F7"><font face="Helvetica, Arial" size=2">');
    if sRightValue='' then
      add('<font color="#FF0000">undefined</font>');
    Add(sRightValue);
    add('</font></TD></TR>');
  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_ReloadServerConfig(rqInfo: TRequestInfo);
//a: Jason Nelson
//w: reload_server_config.Digital Tundra
//This page is accessed via a link on the show_server_config.Digital Tundra page.  It currently
//does not work because of Windows INI caching.
begin
  WebServerConfig.LoadConfig;
  rqInfo.Response.location := 'show_server_config.Digital Tundra';

end;
//------------------------------------------------------------------------------
procedure WRQ_GET_GetUDS(rqInfo: TRequestInfo);
//a: Jason Nelson
//w: Get_UDS.Digital Tundra
//Returns a plain text response with two lines.  The first is the internal URL to the User Data Server.  The second is the external URL.
var
  sTemp: ansistring;
begin
  try
    rqInfo.response.contentType := 'text/plain';
//    sTemp := WebServerConfig.Dest['UserDataServer'].URL;
    sTemp := adjustwebpath(WebServerConfig.FarmRouter)+'proxy.UserDataServer.';
    rqInfo.response.content.add(sTemp);
    rqInfo.response.content.add(sTemp);
  except
    rqInfo.response.resultcode := 203;
  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_GET_SuperSession(rqInfo: TRequestInfo);
//w: super_session.Digital Tundra
//a: Jason Nelson
//Helper page for load testing.  Requires special setup of accounts.  Do not use.  Obsolete.
begin
  with rqInfo.response.content do begin
    add('<form method="POST" action="super_session.Digital Tundra">');
    add('Enter your super session id here: <input type="text" name="supersession" value="0">');
    add('<input type="submit">');
    add('</form>');
  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_POST_SuperSession(rqInfo: TRequestInfo);
//w: super_session.Digital Tundra
//a: Jason Nelson
//Helper page for load testing.  Requires special setup of accounts.  Do not use.  Obsolete.
begin
(*  rqInfo.response.CookieName := 'supersession';
  rqInfo.response.CookieValue := inttohash(strtoint(rqInfo.request['supersession']), rqInfo.request.clientIP);

  with rqInfo.response.content do begin
    add('<HTML><BODY><CENTER>');
    add('<B><H1>Super session has been set!</H1>');
    add('<BR>Note: please make sure you return to the <a href="utility.Digital Tundra">utility page</a> and clear the super session.<BR>');
    add('<BR><a href="admin_home.Digital Tundra?sessionid='+inttohash(0, rqInfo.request.clientIP)+'">Continue to the admin home page</a>');
    add('<BR>-or-<BR><a href="learner_home.Digital Tundra?sessionid='+inttohash(0, rqInfo.request.clientIP)+'">Continue to the learner home page</a>');
    add('</B></CENTER></BODY></HTML>');
  end;
*)
end;
//------------------------------------------------------------------------------
procedure WRQ_GET_ClearSuperSession(rqInfo: TRequestInfo);
begin
  rqInfo.response.AddCookie('supersession', '');

  with rqInfo.response.content do begin
    add('<HTML><BODY>');
    add('<B>Super session has been cleared</B><BR>');
    add('<a href="javascript:history.back(1)">Back</a>');
    add('</BODY></HTML>');
  end;

end;
//------------------------------------------------------------------------------
function ExternalResourceStatus(sPath: ansistring): ansistring;
//a: Jason Nelson
//Checks the existance of a path, and returns an HTML ansistring response.  Helper function for
//show_server_status.Digital Tundra.
begin
  if DirectoryExists(sPath) then
    result := '<font color="#007F00" size="2">OK!</font>'
  else
    result := '<font color="#7F0000" size="2">Incorrect!</font>';

end;
//------------------------------------------------------------------------------
function HttpLinkStatus(sURL: ansistring): ansistring;
//a: Jason Nelson
//Checks the existance of a URL, and returns an HTML ansistring response.  Helper function for
//show_server_status.Digital Tundra.
var
  http: THTTPClient;
begin
  http := THTTPClient.create;
  try
    try
      if NOT http.Get(AdjustWebPath(sURL)+'load.Digital Tundra', '') then begin
        result := '<font color="#7F0000" size="2">No Response!</font>';
        exit;
      end;

      if http.ResultCode = '200' then
        result := '<font color="#007f00" size="2">200 OK!</font>'
      else
        result := '<font color="#7F7F00" size="2">'+http.ResultCode+' ???</font>';
    except
      result := '<font color="#AF0000" size="2">Exception:'+Exception(ExceptObject).Message+'</font>';
    end;

  finally
    http.free;
  end;
end;
//------------------------------------------------------------------------------
function RDTPLinkStatus(sLinkName: ansistring): ansistring;
//a: Jason Nelson
//Checks the existance of a Data-Tier, and returns an HTML ansistring response.  Helper function for
//show_server_status.Digital Tundra.

var
  DOSV: TDataObjectServices;
begin
  try
    DOSV := DOSVPool.ServersByName[sLinkName];
    if not (DOSV = nil) then begin
      result := '<font color="#7F0000" size="2">Data Object Services not initialized</font>'
    end;

  except
    result := '<font color="#AF0000" size="2">'+Exception(ExceptObject).Message+'</font>';
  end;

end;

procedure WRQ_PrimeCalc(rqInfo: TRequestInfo);
//w: Prime_calc.Digital Tundra
//This function was created to contrast the scalability of the memory manager in the Middle tier.  It simply calculates primenumbers, a CPU-intensive, but not heap intensive task.
var
  t, u: integer;
  bIsPrime: boolean;
begin
  for t:=600000 to 600100 do begin
    bIsPrime := true;
    for u:=2 to t div 2 do begin
      if (t mod u) = 0 then begin
        bIsPrime := false;
      end;
    end;
    if bIsPrime then
      rqInfo.response.content.add(inttostr(t));
  end;
end;

procedure WRQ_StringTest(rqInfo: TRequestInfo);
//w: Prime_calc.Digital Tundra
//This function was created to stress the memory manager in the middle-tier, it puts
//stress on memory allocation by concatinating a bunch of ansistrings.  We can then monitor performance and scalability.
var
  t: integer;
  s1, s2: ansistring;
  t1, t2: cardinal;
  build: ansistring;
begin

  t1 := GetTickCount;
  for t:= 0 to 5000000 do begin
    s1 := 'hi';
    s2 := s1+'!';
    s1 := s1[1];
    build := build + s1;
  end;
  t2 := GetTickCount;
  rqInfo.response.content.add('done '+floattostr(500000/((t2-t1)/1000))+' ops per second.');

end;

procedure WRQ_UpdateProxy(rqInfo: TRequestInfo);
//a: Jason Nelson
//w: Update_proxy.Digital Tundra
//This page is displayed wile performance data is being updated.  If it is not updated
//within a certain amount of time, and error is displayed stating they are already running courseware.
begin
(*  rqInfo.request.params['breakframes'] := 'false';
  rqInfo.request.params['return'] := rqInfo.request.params['url'];
  WRQ_JavaRedirect(rqInfo);
  exit;*)


  with rqInfo.response.content do begin
    loadfromfile(slash(WebServerConfig.ExternalResourcePath)+'updating.html');
//    rqinfo.response.sendchunk;
//    rqInfo.Response.SendFooter

    //rqInfo.response.location := rqinfo.Request['url'];

//    if (not rqInfo.request.hasparam('referer')) or (pos('update_proxy', rqInfo.request['referer']) >0) then begin
    if 1=2 then begin
      add('<HTML>');
      add('<font face="Helvetica, Arial" size="2">The action was cancelled because the previous action did not complete.<BR>');
      add('<a href="javascript:history.back(2)>Click here to go back</a>');
      add('</FONT></HTML>');
    end
    else begin
      insert(0, '<SCRIPT Language="Javascript">');
      insert(1, '<!--');
      insert(2, 'function onPageLoad(){');
      insert(3, '  window.location ='''+MorphURL(DecodeWebString(rqinfo.Request['url'])+''''));
      insert(4, '}');
      insert(5, '//-->');
      insert(6, '</SCRIPT>');
      insert(7, '</Head>');
      insert(8, '</HTML>');
//      rqInfo.response.location := ChangeURLSession(rqInfo, DecodeWebString(rqinfo.Request['url']));

    end;
  end;
end;

procedure WRQ_TextEcho(rqInfo: TRequestInfo);
//a: Jason Nelson
//w: text_echo.Digital Tundra
//Hello-world test for HDML (wireless).
begin
  rqInfo.response.contenttype := 'text/x-hdml';
  rqInfo.Response.NoDebug := true;
  rqInfo.response.content.Clear;
  rqInfo.response.content.add('<hdml version=3.0 public=true markable=true TTL=0>');
  rqInfo.response.content.add('<ce dest=#s1b2 task=gosub label=OK vars="src=adbk">Hello World');

end;

//------------------------------------------------------------------------------
procedure WRQ_SpeedTest2(rqInfo: TRequestInfo);
begin
//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
procedure WRQ_SpeedTest(rqInfo: TRequestInfo);
begin
(*  DOSV.Server.Alive

  rqInfo.response.content.add('<SCRIPT Language="Javascript">');
  rqInfo.response.content.add('<!--');
  rqInfo.response.content.add('  var StartDate = new Date(0);');
  for t:=0 to 1000 do begin
    rqInfo.response.content.add('//This is a dummy file for testing the speed of your internet connection');
  end;

  rqInfo.response.NoDebug := true;
  rqInfo.response.content.add('Your connection has been tested');


  rqInfo.response.content.add('//-->');
  rqInfo.response.content.add('</SCRIPT>');*)

end;
procedure WRQ_FileTest(rqInfo: TRequestInfo);
var
  f: textfile;
begin
  AssignFile(f, 'd:\bin.txt');
  rewrite(f);
  writeln(f, copy(rqInfo.request.content, 1, strtoint(rqInfo.request['content-length'])));
  closefile(f);
end;

//------------------------------------------------------------------------------
procedure WRQ_ShowVersionInfo(rqInfo: TRequestInfo);
// Returns a page with the version numbers for the middle tier, data tier,
// user data server, ipcd3 controller, and flash controller.
var
  t: integer;
begin
  try
    with rqInfo.response.content do begin
      add('<HTML><BODY>');
      add('<TABLE WIDTH="100%" BORDER="0">');
      add('<TR><TD COLSPAN=2 BGCOLOR="F7C7C7">');
      add('<B>Mothership Version Information</B><BR>');
      add('</TD></TR>');
      // Middle tier
      ConfigTableRecord3(rqInfo, 'Web Tier Server', GetServerVersion);
      // UDS
      ConfigTableRecord3(rqInfo, 'User Data Server', GetUDSVersion);
      // ipcd3 controller
      ConfigTableRecord3(rqInfo, 'IPCD3 Controller', GetIPCD3ControllerVersion);
      // flash controller
      //ConfigTableRecord(rqInfo, 'Flash Controller', GetFlashControllerVersion);
      add('</table>');
      add('<p><center><a href="javascript:close()">Click here to close this window</a></center> </p>');
      add('</BODY></HTML>');
    end;
  except
    rqInfo.response.content.add('Exception: '+Exception(ExceptObject).Message);
  end;
end;

procedure WRQ_BrowserTest(rqInfo: TRequestInfo);
//This is a highly specialized page that adds browser version information to the query ansistring
//of a target URL.  This includes service packs installed on the browser client.  It was created to
//allow us to check whether or not service-pack 2 was installed of IE 5.5.
begin
  rqinfo.request.AddParam('url', DecodeWebString(rqInfo.request['url'], '*'), pcInline);
  rqInfo.SetupDefaultVarPool;
  LoadWebResource(rqInfo,rqInfo.response.content, 'version_test.html');
end;

procedure WRQ_MoveToParent(rqInfo: TRequestInfo);
//a: Jason Nelson
//w: move_to_parent.Digital Tundra
//Breaks frames and loads the target page in the parent frame.  The target page is passed as a querystring parameter.  Be sure to URL-encode the URL passed in the query ansistring.  <BR>
//e.g. move_to_parent.Digital Tundra?url=some_url.Digital Tundra%2d.....
begin
  LoadWebResource(rqInfo, 'move_to_parent.html', rqInfo.response.content);
end;


procedure WRQ_Circle(rqInfo: TRequestInfo);
var
  iLoop: integer;
begin
  rqInfo.response.content.add(rqInfo.request['loop']);
  iLoop := strtoint(rqInfo.request['loop']);
  if iLoop > 0 then begin
    dec(iLoop);
    rqInfo.response.location := 'circle.Digital Tundra?loop='+inttostr(iLoop)+'&random='+inttostr(random(2000000000));
  end;

end;
//------------------------------------------------------------------------------
procedure WRQ_JavaCircle(rqInfo: TRequestInfo);
var
  iLoop: integer;
begin
  rqInfo.response.content.add(rqInfo.request['loop']);
  iLoop := strtoint(rqInfo.request['loop']);
  if iLoop > 0 then begin
    dec(iLoop);
    SendJavaRedirect(rqInfo, 'java_circle.Digital Tundra?loop='+inttostr(iLoop)+'&random='+inttostr(random(2000000000)));
  end;

end;

procedure WRQ_StressException(rqInfo: TRequestInfo);
//Stresses the server error infrastructure.
var
  t: integer;
begin
  rqInfo.response.contenttype := 'text/plain';
  for t:= 0 to 1000 do begin
//    SetLastServerError(899, 'A message '+inttostr(t));
    Sleep(100);
//    if not (GetLastServerErrorMessage = 'A message '+inttostr(t)) then
//      raise Exception.create('out of sync');
  end;
end;

procedure WRQ_StressDTConn(rqInfo: TRequestInfo);
//Calls a bad DT request 1000 times.
var
  t: integer;
  i: integer;
begin
  rqInfo.response.contenttype := 'text/plain';
end;
//------------------------------------------------------------------------------
procedure WRQ_StressCPU(rqInfo: TRequestInfo);
begin
  WRQ_PrimeCalc(rqInfo);
end;
//------------------------------------------------------------------------------
procedure WRQ_StressHeap(rqInfo: TRequestInfo);
begin
  WRQ_StringTest(rqInfo);
end;
//------------------------------------------------------------------------------
procedure WRQ_Sleep(rqInfo: TRequestInfo);
var
  t: integer;
  i: integer;
begin
  rqInfo.response.contenttype := 'text/plain';
  rQInfo.response.content.add('OK');

  sleep(60000*10);


end;
//------------------------------------------------------------------------------
procedure WRQ_StressMemory(rqInfo: TRequestInfo);
var
  doTemp,doTemp1: TDataObject;
  t: integer;
  tm1, tm2: cardinal;
  x: integer;
begin
  doTemp1 := nil;
  tm1 := GetTickCount;
  rqinfo.request.default('x', '4000');

  for t:= 1 to strtoint(rqINfo.request['x']) do begin
    rqInfo.DTID := 0;
    doTemp := Ghost(rqInfo, 'TdoSession', t);
    if not (doTemp1 = nil) then
      doTemp1.addObject(doTemp);

    doTemp1 := doTemp;
  end;

  tm2 := GetTickCount;

  rqINfo.response.content.add(floattostr(4000/((tm2-tm1)/1000)));

end;

procedure CloseWindow(rqInfo: TRequestInfo; ifName: ansistring; sURL: ansistring);
begin
  rqInfo.response.varpool['url'] := sUrl;
  rqInfo.response.Content.add('<SCRIPT LANGUAGE="Javascript">');
  rqInfo.response.Content.add('<!--');
  //rqInfo.response.Content.add('alert(window.name);');
  rqInfo.response.Content.add('if (window.name=='''+ifName+''') {window.close()} else {window.location=''[[[url]]]''};');
  rqInfo.response.Content.add('//-->');
  rqInfo.response.Content.add('</script>');
end;

function WRQ_DoTest(Rqinfo: TRequestInfo): boolean;
    procedure OUtputObject(xmlb: TXMLBuilder; obj: TDataObject);
    var
      t: integer;
    begin
      xmlb.OpenElement('Object', VarArrayOf(['Name', obj.Name, 'ConstName', obj.token.constname]));

        for t:= 0 to obj.fieldCount-1 do begin
          xmlb.OpenElement('Field', VarArrayOf(['Name', obj.fieldbyIndex[t].Name, 'Value', obj.fieldbyIndex[t].AsString]),true);
        end;

        for t:= 0 to obj.objectcount-1 do begin
          OutputObject(xmlb, obj.obj[t]);
        end;

      xmlb.CloseElement('Object');

    end;
var
  obj: TDataObject;
  sType, sParam: ansistring;
  vParam: variant;
  xmlb: TXMLBuilder;
  t: integer;
begin

  sType  := rqInfo.request['typename'];
  vParam  := rqInfo.request['param0'];
  sParam  := rqInfo.request['paramtype0'];
  if sParam = 'integer' then
    vParam := strtoint(vPAram);

  obj := nil;
  xmlb := TXMLBuilder.create;
  try
    OutputObject(xmlb, obj);

    rqINfo.response.content.text := xmlb.xml;

  finally
    xmlb.free;
  end;

  rqInfo.response.contenttype := 'text/xml';
  result := true;
end;

procedure WRQ_Recurse82(rqInfo: TRequestInfo);
var
  t: integer;
  i: integer;
  a: array[0..999] of TREquestInfo;
var
  http: THTTPCLient;
begin
  rqInfo.NoHit := true;
//  rqINfo.response.needsprocessing := false;

(*  for t:= 0 to 99 do begin
    a[t] := TRequestINfo.create;
    a[t].request.document := 'fart #'+inttostr(t);
    a[t].Response.VarPool['x'] := 'pee';
    a[t].Response.ProcessDynamicVariables(true);

  end;

  for t:= 0 to 99 do begin
    a[t].free;
  end;*)



//  for t:= 0 to 1000 do begin
//    rqInfo.Server.GetNextID(random(24));
//    rqInfo.CloseServer;
//  end;

  if rqINfo.request['x']='1' then begin
    rQInfo.response.content.add('OK<BK>');
    exit;
  end;

  http := THTTPCLient.create;
  try
    try
      if http.Get('http://localhost:82/recurse82.ms?x='+inttostr(strtoint(rqINfo.request['x'])-1),'') then begin
        rqInfo.response.content.add(http.inbody);
        rqInfo.response.content.add('good [[[x]]]<BR>');
      end
      else begin
        rqInfo.response.content.add('bad [[[x]]]<BR>');
      end;
    except
      on E:Exception do begin
        rqInfo.response.content.add('[[[x]]] exception '+e.Message+'<BR>');
      end;
    end;


  finally
    http.free;
  end;

  //sleep(60000*10);
end;

procedure WRQ_StreamRemoteImage(rQInfo: TRequestInfo);
var
  htp: THTTPCLient;
  ms: TMemoryStream;
  iStart, iend: int64;
  sRange: string;
begin
  htp := THTTPClient.create;
  ms := TMemoryStream.create;
  try
    htp.Get(rqInfo.request['url'],'');
    rQInfo.response.contentType := GetHeaderParam(htp.InHeader, 'Content-type');
    if rqInfo.response.contenttype <> 'text/html' then begin

      rQInfo.response.contentlength := strtoint(GetHeaderParam(htp.InHeader, 'Content-length'));
      htp.SaveToStream(ms);
      rQInfo.Response.ContentStream := ms;



    end;



  finally
    htp.free;
//    ms.Free;
  end;

end;

procedure WRQ_MailingListSubscribe(rqInfo: TRequestInfo);
var
  sName: ansistring;
  sCity: ansistring;
  sEmail: ansistring;
  sState: ansistring;
begin
  //create a mail
  sName := rqINfo.Request['name'];
  sCity := rqInfo.Request['city'];
  sState := rqInfo.request['state'];
  sEMail := rqInfo.Request['email'];

  if sName = '' then
    sName := sEmail;



  SimpleMail.SendMail(rqInfo.Request['host'], systemx.GetComputerName+'@mothership.digitaltundra.com', rqInfo.Request['listemail'],rqInfo.Request['listemail'],'SUBSCRIBE', 'web_subscribe:'+sName+'/'+sEmail+','+sCity+','+sState, '','','yuna.digitaltundra.com');

  rqInfo.Response.location := 'common_mailing_list_thanks.html';

end;



end.
