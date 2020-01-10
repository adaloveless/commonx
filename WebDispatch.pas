unit WebDispatch;

interface

uses RequestDispatcher, RequestInfo, Exceptions, RequestManager, Dataobjectcachemanager, managedthread, backgroundthreads, Dataobject, variants, graphics, webstring, sysutils, tsc;

function IgnoreError(s: string): boolean;

procedure LogError(sPage, s: string; code: integer);
procedure HTTPProxy(rqInfo: TRequestInfo);
procedure MailError(rqInfo: trequestinfo; e: exception);
procedure WRQ_MaxConnections(rqInfo: TRequestInfo);
procedure WRQ_ShowErrorLog(rqInfo: TRequestINfo);
function DispatchWebRequest(rqInfo: TRequestInfo): boolean;
function ForceBackSlash(s: string): string;
procedure WRQ_ViewLog(rqInfo: TRequestInfo);
procedure WRQ_ResetTierStatistics(rqInfo: TRequestInfo);
procedure WRQ_TierStatistics(rqInfo: TRequestInfo);
procedure WRQ_Utility(rqInfo:TRequestInfo);
procedure EchoParametersAsComments(rqInfo: TRequestInfo);
procedure WRQ_ClearLog(rqInfo: TRequestInfo);
procedure WRQ_CGIGraphicTest(rqInfo: TrequestInfo);
procedure WRQ_ChunkTest(rqInfo: TRequestInfo);
procedure WRQ_ServerState(rqInfo: TRequestInfo);
procedure WRQ_ServerStatus(rqInfo: TRequestInfo);
procedure WRQ_XMLServerStatus(rqInfo: TRequestInfo);
procedure WRQ_EchoHeader(rqInfo: TrequestInfo);
procedure WRQ_GetHitCount(rqInfo: TRequestInfo);



implementation

uses
  MainServerDispatch, dialogs, ThreadSessionVar, Windows, FileCache,
  ErrorHandler, DataObjectServices, classes, Webfunctions,
  CommonRequests, WebStats, WebConfig, extctrls, MTDTInterface,
  WebResource, WebScript, ProxyDispatch, Thumbnail, mothershipwebserver, XMLTools,
  stringx, stringx.ansi, advancedgraphics, EasyImage, TundraCharts,
  SimpleMail, systemx, HTTPClient, debug,
  CommonAsyncPages;

procedure LogError(sPage, s: string; code: integer);
begin
  Debug.Log(sPage+' Error: '+s);
end;

//------------------------------------------------------------------------------
function DispatchWebRequest(rqInfo: TRequestInfo): boolean;
var
  tm1, tm2: cardinal;
  sMethod,s1,s2: string;
  sDoc: string;
  sFile: string;
begin

  result := false;
  rqINfo.request.default('pdf', 'false');
  rqINfo.request.default('radioorientation', 'portrait');
  try
  {$IFDEF ALLOW_PROXY}
    //if begins with HTTP then HTTPproxy
    if lowercase(copy(rqInfo.request.document, 1,5)) = 'http:' then begin
      //HTTPProxy(rqInfo);
      result := true;
      exit;
    end;
  {$ELSE}
    if pos('http://', lowercase(rqInfo.request.document))>0 then begin
      if SplitStringNoCase(rqINfo.request.document, '/mother.dll', s1,s2) then begin
        rQInfo.request.document := s2;
      end;
    end;
  {$ENDIF}


    //If a multipart message
    if IsMultiPart(rqInfo) then
      DecodeMultiPart(rqInfo);

//    AuditLog('Initializing Engine:'+rqInfo.request.document);
    //setup stuff in rqinfo
    rqInfo.InitializeEngine;
//    AuditLog('Engine Initialized');

    //Repair Missing Method if bad header is sent
    if (rqInfo.request.command = '') then begin
      if rqInfo.request.HasParam('Content-Length') and (rqInfo.request['Content-Length'] <> '') then begin
        if strtoint(rqInfo.request['content-length']) > 0 then begin
          rqInfo.request.command := 'POST';
          LogError(rqInfo.request.Document, 'Repairing missing content length -- POST', 700);
        end else begin
          rqinfo.request.command := 'GET';
          LogError(rqInfo.request.Document, 'Repairing missing content length -- GET', 700);
        end;
      end else begin
        rqInfo.request.AddParam('Content-Length', '0', pcHeader);
      end;
    end;


  sMethod := rqInfo.request.Command;
  if SplitString(sMethod, '~', s1,s2) then  begin
    rqInfo.request.command := s2;
//    SendMail('lwp_change', '6515925867@tmomail.net', 'dirka', '6515924867@tmomail.net', 'aud', '"'+s2+'"');

  end;


    result := false;
    tm1 := GetTickCount;
    try
      try
        {$IFDEF FORCEECHO}
          WRQ_ParameterEcho(rqInfo);
          exit;
        {$ENDIF}

        //Setup default auto-parameters
        rqInfo.SetupDefaultVarPool;

        //leave if shutting down


        //--------------------------------------PROXY DISPATCH
        //for dispatching requests starting with
        if NOT result then
          result := DispatchProxyRequest(rqInfo);

        if not result then
          result := DispatchMainServerRequest(rqInfo);


        if not result then
        //--------------------------------------HELP DISPATCH
(*        if copy(rqInfo.request.Document, 1, length('/help_')) = '/help_' then begin

          try
            LoadWebResource(rqInfo.response.content, WebServer.Help.LookupHelpPage(rqInfo));
          except
            rqInfo.response.content.text := '<font size="4" face="helvetica, arial">Sorry no help is available for this topic</font>';
          end;

          result := true;
        end else*)
        //-------------------------------------AUTOMATIC DISPATCH
        //if it has a ps2 extrension... base the content EXCLUSIVELY on Digital Tundrascript
        if (lowercase(rqInfo.request.DocumentExt) = '.ps2') then begin
          rqInfo.Response.Framed := true; //different exception handling style
          if rqInfo.Request.Command = 'GET' then begin
            result:= true;
            LoadWebResource(rqInfo, copy(rqInfo.request.document, 2, length(rqInfo.request.document)), rqInfo.response.content);
          end
          else begin
            result := true;
            ProcessPostScript(rqInfo);
          end;
        end;

        sDoc := lowercase(Rqinfo.request.document);

        //-------------------------------------MAIN DISPATCH
        if NOT result then begin
          //HEAP STATUS
          if sDoc = '/load.ms' then begin
            WRQ_HeapStatus(rqInfo);
            result := true;
          end else
          if sDoc = '/max_connections.ms' then begin
            WRQ_MaxConnections(rqInfo);
            result := true;
          end else
          if sDoc = '/heap_status.ms' then begin
            WRQ_HeapStatus(rqInfo);
            result := true;
          end else
          if sDoc = '/cgi_graphic_test.jpg' then begin
            WRQ_CGIGraphicTest(rqInfo);
            result := true;
          end else
          //Tier_Statistics
          if sDoc = '/tier_statistics.ms' then begin
            WRQ_TierStatistics(rqInfo);
            result := true;
          end else
          //Tier_Statistics
          if sDoc = '/common_mailing_list_subscribe.ms' then begin
            WRQ_MailingLIstSubscribe(rQInfo);
            result := true;
          end else
          if sDoc = '/reset_tier_statistics.ms' then begin
            WRQ_ResetTierStatistics(rqInfo);
            result := true;
          end else
          if sDoc = '/stream_remote_image.ms' then begin
            WRQ_StreamRemoteImage(rqInfo);
            result := true;
          end else
          if sDoc = '/utility.ms' then begin
            WRQ_utility(rqInfo);
            result := true;
          end else
          if sDoc = '/heap_test.ms' then begin
            result := true;
          end else
          if sDoc = '/hit_count.ms' then begin
            WRQ_GetHitCount(rqInfo);
            result := true;
          end else
          if sDoc = '/file_test.ms' then begin
            WRQ_FileTest(rqInfo);
            result := true;
          end else
          if sDoc = '/server_status.ms' then begin
            WRQ_ServerStatus(rqInfo);
            result := true;
          end else
          if sDoc = '/xml_server_status.ms' then begin
            WRQ_XMLServerStatus(rqInfo);
            result := true;
          end else
          if sDoc = '/show_error_log.ms' then begin
            WRQ_ShowErrorLog(rqInfo);
            result := true;
          end else
          if sDoc = '/speed_test.ms' then begin
            WRQ_SpeedTest(rqInfo);
            result := true;
          end else
          if sDoc = '/resize_image' then begin
            WRQ_ResizeImage(rqInfo);
            result := true;
          end else
          if sDoc = '/recurse82.ms' then begin
            WRQ_Recurse82(rqInfo);
            result := true;
          end else
          if sDoc = '/resize_image.jpg' then begin
            WRQ_ResizeImage(rqInfo);
            result := true;
          end else
          if sDoc = '/resize_image.png' then begin
            WRQ_ResizeImage(rqInfo);
            result := true;
          end else
          if sDoc = '/resize_image.gif' then begin
            WRQ_ResizeImage(rqInfo);
            result := true;
          end else
          if sDoc = '/thumbnails' then begin
            WRQ_thumbnails(rqInfo);
            result := true;
          end else
          if sDoc = '/stream_image' then begin
            WRQ_StreamImage(rqInfo);
            result := true;
          end else
          if sDoc = '/chunk_test.ms' then begin
            WRQ_ChunkTest(rqInfo);
            result := true;
          end else
          if sDoc = '/view_log.ms' then begin
            WRQ_ViewLog(rqInfo);
            result := true;
          end else
          if sDoc = '/clear_log.ms' then begin
            WRQ_ClearLog(rqInfo);
            result := true;
          end else
          //PARAMETER ECHO
          if sDoc = '/parameter_echo.ms' then begin
            WRQ_ParameterEcho(rqInfo);
            result := true;
           end else
          //SUPER SESSION
          if (sDoc = '/super_session.ms') and (rqInfo.request.Command = 'GET') then begin
            WRQ_GET_SuperSession(rqInfo);
            result := true;
           end else
          if (sDoc = '/super_session.ms') and (rqInfo.request.Command = 'POST') then begin
            WRQ_POST_SuperSession(rqInfo);
            result := true;
          end else
          if sDoc = '/clear_super_session.ms'then begin
            WRQ_GET_ClearSuperSession(rqInfo);
            result := true;
          end else
          //BOMB TEST
          if sDoc = '/bomb.ms' then begin
            raise Exception.create('Called bomb.ms.  An exception was raised for the purpose of testing exception handling.');
          end else
          //DEV_HALT
          if sDoc = '/fatal.ms' then begin
//            rqInfo.response.content.text := GLastFatalError;
            result := true;
          end else
          if sDoc = '/dev_halt.ms' then begin
            WRQ_Devhalt(rqInfo);
            result := true;
          end else

          if sDoc = '/serverstate.ms' then begin
            WRQ_ServerState(rqInfo);
            result := true;
          end else
          if sDoc = '/echo_header.ms' then begin
            WRQ_EchoHeader(rqInfo);
            result := true;
          end else
          if sDoc = '/status.ms' then begin
            WRQ_ServerStatus(rqInfo);
            result := true;
          end else
          if sDoc = '/stress_heap.ms' then begin
            WRQ_StressHeap(rqInfo);
            result := true;
          end else
          if sDoc = '/show_server_config.ms' then begin
            WRQ_showServerConfig(rqInfo);
            result := true;
          end else
          if sDoc = '/server_test_graphic.jpg' then begin
            WRQ_ServerTestGraphic (rqInfo);
            result := true;
          end else
          if sDoc = '/show_version.ms' then begin
            WRQ_ShowVersionInfo(rqInfo);
            result := true;
          end else
          if sDoc = '/reload_server_config.ms' then begin
            WRQ_showServerConfig(rqInfo);
            result := true;
          end;
        end;



        //-------------------------------------file streaming
        if NOT result then begin
          if not ((rqInfo.request.DocumentExt = '.ms'))
          then begin
            if comparetext(rqInfo.request.DocumentExt, '.js')=0 then begin
              sFile := GetResourceFile(rqInfo, zcopy(rqInfo.Request.document, 1, length(rqInfo.Request.document)-1));
              tsc.AutoCompile(sFile);
            end;
            result := StreamFileCached(rqInfo);
          end;
        end;



        //if nothing handled the request then
        //load 404-Not found page.
        if not result then
          WRQ_NotFound(rqInfo);

        //Ensure that dynamic varables were processed
        if rqInfo.response.NeedsProcessing then begin
          Debug.Log(rqInfo.request.document+' ProcessDynamicVariables');
          rqInfo.Response.ProcessDynamicVariables;
//          rqInfo.SaveVars;
        end;

        rqInfo.Commit;
        rqINfo.PercentComplete := 1;



        if rqINfo.request['pdf'] = 'true' then begin
          CommonAsyncPages.WRQ_GeneratePDFFormat(rqInfo);
        end;

      except
        on E: ENewException do begin
          debug.log('ENewException caught in WebDispatch: '+e.message, 'error');
          if pos('Charlotte', rqInfo.Request.UserAgent)>0 then begin
            rqInfo.response.content.text := 's=0&err='+e.message;
          end else
          if pos('jumpt', rqInfo.Request.UserAgent)>0 then begin
            rqInfo.response.content.text := 's=0&err='+e.message;
          end else begin
            LogError(rqInfo.request.document, e.Message, EnewException(ExceptObject).ErrorCode);
//            SetLastServerError(899, E.Message);
//            ShowLastServerError(rqInfo, rqInfo.response.Framed, E.ErrorCode, E.UserMessage, E.Message);
            WebServerError(rqInfo, e);
            rqInfo.response.message := E.Message;
          end;
          if Not IgnoreError(e.message) then
          try
            //MailError(rQInfo, e);
//            Simplemail.SendMail(GetThisComputerName, 'jason@digitaltundra.com','Er', rqInfo.Request.document+':'+e.Message+#13#10+rQInfo.Response.DebugLog.text);
//            SayNatural(e.Message);
          except
          end;
          try
            rqInfo.Rollback;
          except
          end;

          result := true; //<--must set result to true ELSE misleading 404
        end;
        on E: Exception do begin
          debug.log('Exception caught in WebDispatch: '+e.message, 'error');
          if pos('Charlotte', rqInfo.Request.UserAgent)>0 then begin
            rqInfo.response.content.text := 's=0&err='+e.message;
          end else
          if pos('jumpt', rqInfo.Request.UserAgent)>0 then begin
            rqInfo.response.content.text := 's=0&err='+e.message;
          end else begin
            LogError(rqInfo.request.document, E.Message, rqInfo.response.resultcode);
//            SetLastServerError(899, E.Message);
//            ShowLastServerError(rqInfo, rqInfo.response.Framed, 899, 'General Error', E.Message);
            WebServerError(rqInfo, e);
            rqInfo.response.message := E.Message;
            result := true; //<--must set result to true ELSE misleading 404
          end;
          if Not IgnoreError(e.message) then
          try
            //MailError(rQInfo, e);
          except
          end;
          try
            rqInfo.Rollback;
          except
          end;


        end;
      end;
    finally
      //IMPORTANT -- MUST NIL OUT ANY THREAD VARIABLES BEFORE THREAD TERMINATES!

      rqInfo.Commit;
      tm2 := GetTickCount;

      if pos('wasatchuds', lowercase(rqINfo.Request.Document)) < 1 then
        WebServerStats.AddStat(ExtractFileExt(rqInfo.request.Document)+' '+rqInfo.request.Document, 'DispatchTime', tm2-tm1);

//    GetThreadTimes(GetCurrentThread, ftCreation, ftExit, ftKernel, ftUser);
//    WebServerStats.AddStat(ExtractFileExt(rqInfo.Document)+' '+rqInfo.Document, 'KernelTime', int64(ftKernel));
//    WebServerStats.AddStat(ExtractFileExt(rqInfo.Document)+' '+rqInfo.Document, 'UserTime', int64(ftUser));

      (*if rqInfo.response.contenttype = 'text/html' then
        rqInfo.response.content.text := stringReplace(rqInfo.response.content.text, 'Project Styles.css', 'Project_Styles.css', [rfReplaceAll]);*)
      //If echoing parameters... add comments at top of page
      EchoParametersAsComments(rqInfo);
      {$IFDEF BLEND}
      rqInfo.request.addParam('blend', '0.25', pcHeader);
      {$ENDIF}



    end;
  except
    On E:Exception do begin
      debug.log('Exception caught in WebDispatch (outer): '+e.message, 'error');
      rqInfo.response.content.clear;
      rqInfo.response.content.add('<HTML><FONT face="Helvetica, Arial" size = "2">There was a problem dispatching your request:<BR>');
      rqInfo.response.content.add('<B>'+E.Message+'</B></FONT></HTML>');
      rqInfo.response.contentlength := length(rqInfo.response.content.text);
    end;
  end;

(*  sdocument := rqInfo.request.document;
  sdocument := copy(sDocument, 2, length(sDocument));
  if rqInfo.Response.HasCache and (pos('/', sDocument)<1) then
    AuditLog(rqInfo.response.DOCache.RQs, '.'+ExtractFileName(sDocument)+'.', true);*)

//  AuditLog(inttostr(rqInfo.response.resultcode)+' '+rqInfo.request.document);
end;

//------------------------------------------------------------------------------

function ForceBackSlash(s: string): string;
//This function takes a path that has forward slashes ("/") in it, and changes
//them all to backslashes ("\").
//It is most useful for translating URL file names into disk filenames
var
  t: integer;
  iLen: integer;
begin
  //Temp var used for optomization (length of "s" should not change);
  iLen := length(s);

  result := s;

  //Cycle and replace '/' with '\'
  for t:= 1 to iLen do
    if result[t] ='/' then
      result[t] :='\';
    //end
  //end
end;
procedure WRQ_TierStatistics(rqInfo: TRequestInfo);
var
  t: integer;
  stat: TWebStat;
  sLastCat : string;
begin
  sLastCat := '';
  //Set content type to normal boring text (NOT HTML);
  rqInfo.response.ContentType := 'text/plain';

  WebServerStats.LockRead;
  try
    for t:= 0 to WebServerStats.StatCount-1 do begin
      Stat := WebServerStats.Stats[t];
      if sLastCat <> Stat.Category then
        rqInfo.Response.content.add(Stat.Category+' --- Weight = '+inttostr(Stat.Weight));

      rqInfo.Response.content.add('  '+Stat.StatName+' = '+inttostr(Stat.Value));
      rqInfo.Response.content.add('    Average = '+floattostr(Stat.Average));
      sLastCat := Stat.Category;
    end;
  finally
    WebServerstats.UnLockRead;
  end;
end;
//------------------------------------------------------------------------------
procedure EchoParametersAsComments(rqInfo: TRequestInfo);
var
  sLine1: string;
  slParams: TStringList;
  t: integer;
begin
  exit;

  //a tier-to-tier request may specifically request that this feature be disabled
  //as it causes pasing problems on the other end.   
  if rqInfo.response.NoDebug then
    exit;

  //if not setup to echo parameters then exit
//  if NOT WebServerConfig.EchoPageParameters then
//    exit;

  //add params ONLY if HTML
  if NOT (rqInfo.response.contentType = 'text/html') then
    exit;

  rqInfo.response.contentlength := -1;

  //if chunked encoding then exit (cannot echo when using chunked encoding)
  if lowercase(rqInfo.response.TransferEncoding)='chunked' then
    exit;

  slParams := TStringList.create;
  try
    //Read first line from response
    if rqInfo.Response.content.count>0 then
      sLine1 := rqInfo.Response.content[0]
    else
      sLine1 := '';

    slPArams.Add('<!--//Debug Log//-->');
    //rqInfo.response.MassageDebugStats;
    for t:= 0 to rqInfo.Response.DebugLog.count-1 do
      slParams.add('<!--//'+rqINfo.response.DebugLog[t]+'//-->');

    slPArams.Add('<!--//-------//-->');

    for t:= 0 to rqInfo.request.paramcount-1 do
      slParams.add('<!--//'+rqInfo.request.ParamNames[t]+'='+rqInfo.request[rqInfo.request.ParamNames[t]]+'//-->');

    slParams.add('<!--//Sessionid='+inttostr(rqInfo.Sessionid)+'//-->');




    sLine1 := slParams.text+sLine1;

    //Update the first line of the response
    if rqInfo.Response.content.count>0 then
      rqInfo.Response.content[0] := sLine1
    else
      rqInfo.Response.content.Add(sLine1);

  finally
    slParams.free;
  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_Utility(rqInfo:TRequestInfo);
begin
  with rqInfo.Response.content do begin
(*    add('<a href="heap_status.ms">Heap Status</a><BR>');
    add('<BR>');
    add('<a href="tier_statistics.ms">View tier Statistics</a><BR>');
    add('<a href="reset_tier_statistics.ms">Reset tier Statistics</a><BR>');
    add('<BR>');
    add('<a href="view_log.ms">View System Log</a><BR>');
    add('<a href="clear_log.ms">Clear System Log</a><BR>');
    add('<BR>');
    add('<a href="show_server_config.ms">Show Server Configuration</a><BR>');
    add('<a href="server_status.ms">Show Server Status</a><BR>');
    add('<a href="echo_header.ms">Echo Browser Header</a><BR>');
    add('<a href="show_error_log.ms">Show Error Log</a><BR>');
    add('<BR>');
//    add('<a href="dev_halt.ms">Halt Server **do not use this on IIS**</a><BR>');
//    add('<BR>');
//    add('<B>Super Session Stuff for Load Testing</b><BR>');
//    add('<a href="super_session.ms">Setup super session </a><BR>');
//    add('<a href="clear_super_session.ms">Clear Super Session </a><BR>');
//    add('<BR>');*)
    add('<B>Stream Test</b> -- you should see a gradient graphic below if JPEG CGI streaming is working.<BR>');
    add('<img src="cgi_graphic_test.jpg?a=1">');
    add('<img src="cgi_graphic_test.jpg?a=2">');
    add('<img src="cgi_graphic_test.jpg?a=3">');
  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_ResetTierStatistics(rqInfo: TRequestInfo);
begin
  WebServerStats.Disable;
  WebServerStats.Clear;
  WebServerStats.Enable;
  with rqInfo.Response.content do begin
    add('<B>Statistics Reset</B><BR>');
    add('<a href="utility.ms">Back to Utility Page</a><BR>');
  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_ClearLog(rqInfo: TRequestInfo);
begin
//  ClearWebLog;
//  with rqInfo.Response.content do begin
//    add('<B>Log has been cleared</B><BR>');
//    add('<a href="utility.ms">Back to Utility Page</a><BR>');
//  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_ViewLog(rqInfo: TRequestInfo);
var
  sl : TStringList;
begin
  sl := TStringList.create;
  try
//    GetWebLog(sl);
    rqInfo.Response.content.add('<a href="utility.ms">Back to Utility Page</a><BR>');
    rqInfo.response.content.Add(sl.text);
  finally
    sl.free;
  end;
end;
//------------------------------------------------------------------------------

procedure WRQ_CGIGraphicTest3(rqInfo: TRequestInfo);
var
  ag: TDrawingBoard;
CONST
  margin = 0.5;
begin
//  exit;

  ag := TDrawingBoard.create(nil);
  try

    ag.Width := 640;
    ag.Height := 480;
    ag.BoundX1 := 0;
    ag.BoundX2 := 9;
    ag.BoundY1 := 0;
    ag.BoundY2 := 9;
    ag.Draw;
//    ag.DrawTo := ag.canvas;

    ag.LineColor := clRed;
    ag.FillColor := clBlack;
    ag.Rectangle(ag.BoundX1+margin,ag.boundy1+margin, ag.boundx2-margin, ag.boundy2-margin,clRed, true);



    ag.Linecolor := clYellow;
    ag.fillcolor := clWhite;
    ag.canvas.Font.Color := clBlack;
//    ag.Canvas.TextOut(0,0,'Test');

    ag.Text(0,0,ag.boundx2-ag.boundx1, 0.5, rqInfo.request['a'], false);

    ag.Flip;

    ResizeImage(ag.picture.bitmap, ag.width div 2, ag.height div 2);
    StreamImageAsPNG(rQInfo, ag);






  finally
    ag.free;
  end;



end;

procedure WRQ_CGIGraphicTest(rqInfo: TRequestInfo);
var
  ag: TLinechart;
  t: integer;
  s: TSeries;
  rr: real;
CONST
  margin = 0.5;

begin
//  exit;

  ag := TLineChart.create(nil);
  try
    ag.Width := 640;
    ag.Height := 480;
    ag.AntiAliasing := 3;

    s:= ag.AddSeries;
    s.Color := clRed;
    rr := 1;
    for t:= 0 to 10 do begin
      rr:= rr*(1+( random(10) / 10) );
      s.AddValue(t, rr);
    end;


    s:= ag.AddSeries;
    s.Color := clBlue;
    rr := 1;
    for t:= 0 to 10 do begin
      rr:= rr*(1+( random(10) / 10) );
      s.AddValue(t, rr);
    end;

    ag.Draw;



//
    (*ag.BoundX2 := 400;
    ag.BoundY2 := 300;
    for t := 0 to 100 do begin
      ag.fatline(random(round(ag.boundx2)),random(round(ag.boundy2)),random(round(ag.boundx2)),random(round(ag.boundy2)), random(5), random(16000000), true);
    end;*)

    //ag.Linecolor := clYellow;
    //ag.fillcolor := clWhite;
//    ag.canvas.Font.Color := clBlack;
//    ag.Canvas.TextOut(0,0,'Test');

//  ag.Text(0,0,ag.boundx2-ag.boundx1, 1, rqInfo.request['a'], false);

//    ag.Flip;

//    ag.canvas.lock;

//    ResizeImage(ag.BackBitmap, ag.width div 2, ag.height div 2);
    ag.flip;
    StreamImageAsPNG(rQInfo, ag);

//    ag.Canvas.Unlock;






  finally
    ag.free;
  end;



end;


procedure WRQ_CGIGraphicTest2(rqInfo: TRequestInfo);
var
  image1: TImage;
  x,y: integer;
begin
  image1 := Timage.create(nil);
  image1.width := 320;
  image1.height := 240;
  try
    //Quickie little CGI Jpeg Image
    with image1.canvas do begin
      Lock;
      try
        rect(0,0, 320, 200);
        image1.picture.bitmap.pixelformat := pf32bit;
        for x:= 0 to 319 do begin
          for y:= 0 to 240 do begin
            pixels[x,y] := (((320*x) div 256)*256)+((240*y) div 256);
          end;
        end;
      finally
        Unlock;
      end;
    end;

    //Send the result
    StreamImageAsJpeg(rqInfo, image1);
  finally
    image1.free;
  end;

end;

procedure WRQ_ChunkTest(rqInfo: TRequestInfo);
begin
  with rqInfo.response do begin
    //content.Add('4');
    content.Add('1234');
    content.Add('1234');
//    content.Add('1234');
    SendChunk;

    //content.Add('5');
    content.Add('12345');
//    content.Add('12345');
    SendChunk;

    //content.Add('6');
    content.Add('ABCDEF');
//    content.Add('ABCDEF');

    SendChunk;

    SendFooter;
//    content.Add('0');
//    content.Add('');
  end;

end;

//------------------------------------------------------------------------------
procedure WRQ_ServerState(rqInfo: TRequestInfo);
begin

//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
procedure WRQ_ShowErrorLog(rqInfo: TRequestInfo);
begin
  rqInfo.Response.Content.text := 'not implemented anymore';
//  GetErrorLogHTML(rqInfo.response.content);

end;

procedure WRQ_ServerStatus(rqInfo: TRequestInfo);
var
  t: integer;
  thr2: TManagedThread;
  sTemp, sLEft, sRight: string;
const
  LOCK_TIME = 30000;
begin
  with rqInfo.response.content do begin

    add('<B>Request Manager:</B><BR>');

//    windows.beep(100,100);


    if rqInfo.request.hasparam('newvals') then begin
      add('hits:'+ inttostr(WebServer.HitCount)+'<BR>');
      add('RQs:'+ inttostr(DOOB.TotalRequests)+'<BR>');
      add('DOs:'+ inttostr(DOOB.TotalObjects)+'<BR>');
    end;
    add('<BR>');



    add('<table borderwidth="0" columns="2">');
    add('<BR>DT Connections:<BR>');
    for t := 0 to dosvpool.count-1 do begin
      add('<TR><TD width="320" valign="TOP">');
      add('<!--name-->');
      add(dosvpool[t].Name+'<BR></TD>');
      add('<TD><font size = "2">');
      add('<!-- -->');
//      add('Active:'+inttostr(dosvpool[t].Server.ActiveTransactions)+'<BR>');
//      add('Aborted:'+inttostr(dosvpool[t].Server.AbortedTransactions)+'<BR>');
//      add('Retrying:'+inttostr(dosvpool[t].Server.RetryingTransactions)+'<BR>');
//      add('Total:'+inttostr(dosvpool[t].Server.TotalTransactions)+'<BR>');
      add('<!-- -->');
      add('</font></TD></TR>');
    end;
    add('</TABLE>');



    add('<B>Cache information:</B><BR>');
    if docm.TryLock(LOCK_TIME) then try
      add('Active:'+inttostr(DOCM.ActiveCacheCount)+'<BR>');
      add('Dead:'+inttostr(DOCM.DeadCacheCount)+'<BR>');
      add('Decommisioned:'+inttostr(DOCM.DeCommisionedCacheCount)+'<BR>');
    finally
      docm.Unlock;
    end;
    add('<BR>');
    add('<B>Background:</B><BR>');

    if BackgroundthreadMan.TryLockRead(LOCK_TIME) then
    try
      for t:= 0 to BackgroundthreadMan.count-1 do begin
        thr2 := BackgroundthreadMan.threads[t];
        if thr2.TryLock(LOCK_TIME) then try
          add('<b>'+inttostr(t)+': '+thr2.Name+'</b> Status:<i>'+thr2.Status+'</i> Step:<i>'+inttostr(thr2.Step)+'</i> StepCount:<i>'+inttostr(thr2.StepCount)+'</i> Iterations:<i>'+inttostr(thr2.Iterations)+'</i><BR>');
        finally
          thr2.Unlock;
        end;
      end;
    finally
      BackgroundthreadMan.UnLockRead;
    end
    else begin
      add('*: Unable to get lock on background thread manager');
    end;
    //windows.beep(900,10);


    add('<BR>');
    if doob.TryLockRead(LOCK_TIME) then try
      add('active Data objects:'+ inttostr(doob.count)+'<BR>');
    finally
      doob.UnlockRead;
    end;

    //windows.beep(1000,10);

    if rqInfo.request.hasParam('DOs') then begin
      add('<font size="1" face="helvetica, arial">');
      doob.LockRead;
      try
        for t:= 0 to doob.count-1 do begin
          add(doob.Objects[t]+'<BR>');
        end;
      finally
        add('</font>');
        doob.unlockRead;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure WRQ_EchoHeader(rqInfo: TrequestInfo);
begin
  rqInfo.response.contenttype :='text/plain';
  rqINfo.response.content.add(rqInfo.request.Raw);

end;
//------------------------------------------------------------------------------
procedure WRQ_GetHitCount(rqInfo: TRequestInfo);
//w: /hit_count.ms
//Returns the number of hits since the server was started
begin
  rqInfo.response.contenttype := 'text/plain';
  rqInfo.response.content.text := inttostr(webserver.hitcount);
end;


procedure WRQ_MaxConnections(rqInfo: TRequestInfo);
begin
  if rqInfo.request.hasparam('limit') then
    WebServerConfig.ConnectionLimit := strtoint(rqInfo.request['limit']);

  rqInfo.response.content.text := inttostr(WebServerconfig.connectionLImit);


end;

procedure WRQ_XMLServerStatus(rqInfo: TRequestInfo);
var
  thr: TManagedThread;
  t,u: integer;
  xmlb : TXMLBuilder;
  stat: TWebStat;
  doQueues: TDataObject;
  sTemp: string;
  sLeft, sRight: string;
begin
  try
    rqInfo.response.contenttype := 'text/xml';
    xmlb := TXMLBuilder.create;
    try
      xmlb.OpenSimpleElement('ServerStatus');
        {$IFNDEF OLDMEMMAN}
        xmlb.OpenSimpleElement('Memory');
{$IFDEF DO_MEM_CHART}
        ManMan.Lock;
        try
          for t:= 0 to ManMan.ManagerCount-1 do begin
            ManMan.Managers[t].Lock;
            try
              xmlb.OpenElement('Manager',VarArrayOf(['blocks', ManMan.Managers[t].BlockCount, 'Overhead', ManMan.Managers[t].BlockHeaderOverhead, 'Used', ManMan.Managers[t].UsedBytes, 'Waste', ManMan.Managers[t].WasteBytes, 'Free', ManMan.Managers[t].FreeBytes]),true);
            finally
              ManMan.Managers[t].Unlock;
            end;
          end;


        finally
          ManMan.Unlock;
          xmlb.Close('Memory');
        end;
{$ENDIF}
        {$ENDIF}

        xmlb.OpenElement('Hits', VarArrayOf(['Value', inttostr(WebServer.HitCount)]),true);
        doob.LockRead;
        try
          xmlb.OpenElement('DOs', VarArrayOf(['Value', DOOB.TotalRequests]),true);
          xmlb.OpenElement('RQs', VarArrayOf(['Value', DOOB.TotalObjects]),true);
          xmlb.OpenElement('ActiveDOs', VarArrayOf(['Value', DOOB.Count]),true);
        finally
          doob.UnlockRead;
        end;

        xmlb.Open('DTs');
        try
          for t:=0 to DOSVPool.count-1 do begin
            xmlb.openElement('DT', VarArrayOf(['Name', DOSVPool[t].NAme]));
            //dosvpool[t].Server.LockRead;
            try
              xmlb.openElement('ActiveUsers', VarArrayOf(['Value', DOSVPool[t].ActiveUsers]),true);
//              xmlb.openElement('ActiveConnections', VarArrayOf(['Value', DOSVPool[t].Server.ActiveTransactions]),true);
//              xmlb.openElement('RetryingConnections', VarArrayOf(['Value', DOSVPool[t].Server.RetryingTransactions]),true);
//              xmlb.openElement('AbortedConnections', VarArrayOf(['Value', DOSVPool[t].Server.AbortedTransactions]),true);
//              xmlb.openElement('TotalConnections', VarArrayOf(['Value', DOSVPool[t].Server.TotalTransactions]),true);



            finally
              //dosvpool[t].Server.UnLockRead;
            end;
            xmlb.closeElement('DT');
          end;
        finally
          xmlb.Close('DTs');
        end;
        xmlb.Open('CacheManager');
        DOCM.LockRead;
        try
          xmlb.OpenElement('Active', VarArrayOf(['Value', DOCM.ActiveCacheCount]),true);
          xmlb.OpenElement('Dead', VarArrayOf(['Value', DOCM.DeadCacheCount]),true);
          xmlb.OpenElement('Decommisioned', VarArrayOf(['Value', DOCM.DeCommisionedCacheCount]),true);
        finally
          DOCM.UnlockRead;
        end;
        xmlb.Close('CacheManager');

        xmlb.Open('BackgroundThreads');
        BackGroundThreadMan.LockRead;
        try
          for t:= 0 to BackgroundThreadMan.count-1 do begin
            thr := BackGroundThreadMan.Threads[t] as TManagedThread;
            xmlb.OpenElement('Thread', VarArrayOf([
              'NAme', thr.NAme,
              'Step', thr.Step,
              'StepCount',thr.StepCount,
              'Iterations', thr.Iterations
              ]), true
            );
          end;
        finally
          BackGroundThreadMan.UnlockRead;
        end;
        xmlb.Close('BackgroundThreads');

        xmlb.open('stats');
        WebServerStats.LockRead;
        try
          for t:= 0 to WebServerStats.StatCount-1 do begin
            Stat := WebServerStats.Stats[t];
            sTemp := stat.category;
            sTemp := stringReplace(sTemp, '"', EncodeWebSTring('"'), [rfReplaceALL]);
            xmlb.OpenElement('PageStats', VarArrayOf(['url', sTemp,'Hits',stat.weight,'LastTime',stat.value,'AverageTime',stat.Average]),true);
          end;
        finally
          WebServerstats.UnLockRead;
        end;

        xmlb.Close('stats');

      xmlb.CloseElement('ServerStatus');
    finally
      rqINfo.response.content.text := xmlb.xml;
      xmlb.free;
    end;
  except
    on E: Exception do begin
      debug.log('Exception caught in xml_server_status: '+e.message, 'error');
      rqInfo.response.contenttype := 'text/plain';
      rqInfo.response.content.clear;
      rqInfo.response.content.add(e.Message);
    end;

  end;
end;



procedure WRQ_StatMemory(rqInfo: TRequestInfo);
var
  t: integer;
begin
  rqInfo.NoHit := true;
  rqInfo.response.contenttype := 'text/plain';

{$IFDEF DO_MEM_CHART}
  ManMan.Lock;
  try


    for t:= 0 to ManMan.ManagerCount-1 do begin
      ManMan.Managers[t].Lock;
//      if ManMan.Managers[t].TryLock then
      try
        ManMan.Managers[t].IsFree;
        rqInfo.response.content.Add(inttostr(t)+':'+inttostr(ManMan.Managers[t].BlockHeaderOverhead)+':'+inttostr(ManMan.Managers[t].UsedBytes)+':'+inttostr(ManMan.Managers[t].WasteBytes)+':'+inttostr(ManMan.Managers[t].FreeBytes))
      finally
        ManMan.Managers[t].Unlock;
      end;
    end;

  finally
    ManMan.Unlock;
  end;
{$ENDIF}
end;



procedure WRQ_StatGeneral(rqInfo: TRequestInfo);
(*var
  t: integer;*)
begin
(*  rqInfo.NoHit := true;

  for t:= 0 to 1000000 do begin
    rqINfo.server.GetNextID(1);
  end;*)

end;

function IgnoreError(s: string): boolean;
var
  sl : TStringlist;
  t: integer;
  sFile: string;
begin
  result := false;
  sl := tstringlist.create;
  try
    sFile := slash(webconfig.WebServerConfig.ExternalResourcePath)+'ignored_errors.html';
    if fileexists(sFile) then
      sl.loadfromfile(sFile);


    for t:= 0 to sl.count-1 do begin
      if (sl[t] <> '') and (pos(lowercase(sl[t]), lowercase(s)) > 0) then begin
        result := true;
        break;
      end;
    end;

  finally
    sl.free;
  end;



end;

procedure MailError(rqInfo: trequestinfo; e: exception);
var
  sl: TStringlist;
  t: integer;

begin
  sl := Tstringlist.create;
  try
    sl.add(rQInfo.request.document);
    sl.add(e.message);
    sl.add('Parameters');
    sl.add('----------');
    for t:= 0 to rqInfo.request.paramcount-1 do begin
      sl.add(rqInfo.request.paramnames[t]+'='+rQInfo.request.paramsbyindex[t]);
    end;

    sl.add('Vars');
    sl.add('----------');
    for t:= 0 to rqInfo.response.varcount-1 do begin
      sl.add(rqInfo.response.varnames[t]+'='+rQInfo.response.varbyindex[t]);
    end;

    Simplemail.SendMail(systemx.GetComputerName, 'jason@digitaltundra.com','Er', sl.text);

  finally
    sl.free;
  end;


end;

procedure HTTPProxy(rqInfo: TRequestInfo);
var
  htp: THTTPClient;
  ms: TMemoryStream;
  s: string;
  sLeft, sRight: string;
  t: integer;
begin

  htp := rqinfo.HTTPClient;
  try
    htp.AutoLog := true;
    htp.AutoRedirect := false;
    htp.Request := rQInfo.Request.Raw;
//    htp.EnableGZIP := false;
    htp.Transact('www.ds2network.com', 80);

    rqInfo.Response.ContentType := GetHeaderParam(htp.InHeader, 'content-type');
    rqInfo.response.ContentEncoding := GetHeaderParam(htp.InHeader, 'content-encoding');
    s := GetHeaderParam(htp.Inheader, 'content-length');
    if s <> '' then
      rQinfo.Response.ContentLength := strtoint(s)
    else
      rQInfo.response.contentlength := 0;


    s := GetHeaderParam(htp.Inheader, 'location');
    if s <> '' then
      rQInfo.response.location := s;

    t := 0;
    While GetHeaderParam(htp.InHeader, 'Set-Cookie', t)<>'' do begin
      s := GetHeaderParam(htp.InHeader, 'Set-Cookie', t);
      SplitString(s, ';', sLeft, sRight);
      SplitString(sLeft, '=', sLeft, sRight);
      rQInfo.Response.AddCookie(sLeft, sRight);
      inc(t);
    end;


    rqInfo.response.ResultCode := strtoint(htp.ResultCode);


    rqInfo.response.RawHeader := htp.InHeader;

    ms := TMemoryStream.create;
    htp.SaveToStream(ms);
    rqInfo.Response.ContentStream := ms;
//    rqInfo.response.content.text := htp.InBody;
    rQInfo.response.NeedsProcessing := false;

  finally
    //htp.free;
  end;


end;
initialization
RQD.AddRequest('/memory.stat', 'GET', WRQ_StatMemory);
RQD.AddRequest('/general.stat', 'GET', WRQ_StatGeneral);



end.
