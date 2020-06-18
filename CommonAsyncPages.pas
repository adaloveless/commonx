unit CommonAsyncPages;


interface

uses
  managedthread, memoryfilestream, asyncclasses, backgroundthreads, exe, windows, webconfig, systemx, ExceptionsX, variants, webresource,WebString, stringx, stringx.ansi, CommonRequests, WebFunctions, Sysutils, RequestDispatcher, RequestInfo, classes, dataobject, serverinterface, MTDTInterface;


procedure AWRQ_AsyncTest(rqInfo: TRequestInfo; thread: TAsyncProcess);
procedure WRQ_AsyncTest(rqInfo: TrequestInfo);
procedure WRQ_GeneratePDFFormat(rqInfo: TRequestInfo);
procedure WRQ_AsyncStatus(rqInfo: TRequestInfo);

procedure WRQ_AsyncWait(rqInfo: TRequestInfo);
procedure WRQ_AsyncResult(rqINfo: TRequestINfo);
procedure WRQ_AsyncPDFResult(rqINfo: TRequestINfo);



implementation

function UniqueTempFilename(sTempFileLocation: ansistring): ansistring;
var
  sPrefix, sTempFile: ansistring;
  chrArray: array[0..250] of AnsiChar;
begin
  // create a uniquely named temp file for the pdf file to avoid conflicts
  // on multiple requests
  sPrefix := 'tmp';
  sTempFile := '';
  GetTempFileName(
    PChar(sTempFileLocation),	// address of directory name for temporary file
    PChar(sPrefix),	// address of filename prefix
    0,	// number used to create temporary filename
    @chrArray[0] 	// address of buffer that receives the new filename
  );

  sTempFile := chrArray;

  result := sTempFile;
end;


procedure WRQ_AsyncPDFResult(rqINfo: TRequestINfo);
var
  doResult: TDataObject;
begin
  raise exception.Create('unimplmented');


end;


procedure WRQ_AsyncResult(rqINfo: TRequestINfo);
var
  doResult: TDataObject;
begin
  Notimplemented;
{
  doResult := QueryMap(rqInfo, 'SELECT * from AsyncData where processid='+rQInfo.request['processid'], 'TdoAsyncProcessData', 'processid');

  rqINfo.response.content.text := DecodewebString(doResult['ProcessData'].AsString);
  rqInfo.response.NeedsProcessing := false;
}

end;

procedure WRQ_AsyncWait(rqInfo: TRequestInfo);
begin
  rqINfo.request.default('pdf', 'false');
  rqINfo.response.varpool['var_url'] := 'async_status.ms?sessionid='+rqInfo.sessionhash+'&processid='+rqInfo.request['processid'];
  if lowercase(rqINfo.request['pdf']) = 'true' then
    rqINfo.response.varpool['return_url'] := 'async_result.pdf?sessionid='+rqInfo.sessionhash+'&processid='+rqInfo.request['processid']+'&radioorientation='+rqInfo.request['orientation']
  else
    rqINfo.response.varpool['return_url'] := 'async_result.ms?sessionid='+rqInfo.sessionhash+'&processid='+rqInfo.request['processid'];

  LoadWebResource(rqInfo, 'async_response.html');

end;
procedure AWRQ_AsyncTest(rqInfo: TRequestInfo; thread: TAsyncProcess);
var
  t: integer;
begin
  for t:= 0 to 30 do begin
    rqInfo.response.content.add(inttostr(t)+'<BR>');
    Sleep(300);
    thread.UpdateStatus(t, 30, 'Counting... '+inttostr(t));
  end;
end;

procedure WRQ_AsyncStatus(rqInfo: TRequestInfo);
var
  status: TDataObject;
  sResult: ansistring;
begin
  NotImplemented;
{
  try


    status := QueryMap(rqInfo, 'SELECT * from async where processid='+rqInfo.Request['processid'], 'TdoAsyncProcess', vararrayof(['processid']));

    sResult := '';

    sResult := sResult + 'display_text=' + status['Name'].AsWebString;
    sResult := sResult + '&progress_pos=' + status['StepNumber'].AsWebString;
    sResult := sResult + '&progress_max=' + status['TotalSteps'].AsWebString;
    if status['State'].AsVariant < 1 then
      sResult := sResult + '&complete=1'
    else
      sResult := sResult + '&complete=0';

    sResult := sResult + '&error=' + '';
    sResult := sResult + '&loaded=1&nocrlfhere=';

    rqInfo.response.contenttype := 'text/plain';
    rqInfo.response.content.text := sResult;
  except
    On E:Exception do begin
      sResult := 'loaded=1&junk=1234&complete=0&error='+E.Message;
      rqInfo.response.content.text := sResult;
    end;
  end;

 }
end;

procedure WRQ_GeneratePDFFormat(rqInfo: TRequestInfo);
var
  objFileStream: TMemoryFileStream;
  sProgram, sWorkingDirectory, sParameters, sDocument, sTempFile: ansistring;
  sOrientation, sPageSize, sEncryption, sPassword: ansistring;
begin
  rQInfo.request.default('radioorientation', 'portrait');
  sOrientation := ' --' + rqInfo.Request.Params['radioOrientation'];


  sPageSize := 'Letter';
  if rqInfo.Request.HasParam('selectPageSize') and (rqInfo.Request.Params['selectPageSize']<>'Letter') then
    sPageSize := rqInfo.Request.Params['selectPageSize'];

  sEncryption := '';
  sPassword := '';
  if rqInfo.Request.HasParam('checkEncryption') then begin
    sEncryption := ' --encryption ';
    // require a password if encryption is checked
    if rqInfo.Request.HasParam('textPassword') then begin
      if rqInfo.Request.Params['textPassword'] <> '' then
        sPassword := '--owner-password ' + rqInfo.Request.Params['textPassword'] + ' --user-password ' + rqInfo.Request.Params['textPassword']
      else
        raise ENewException.create(907,'Password cannot be blank','');
    end else begin
      raise ENewException.create(907,'Password cannot be blank','');
    end;
  end;

  sProgram := slash(WebServerconfig.HTMLDocPath) + 'htmldoc.exe';
  sWorkingDirectory := slash(WebServerConfig.ReportPDFTempFileLocation);
  ForceDirectories(sWorkingDirectory);

  sDocument := rqInfo.Request.Document;
  system.Delete(sDocument, 1, 1);

  sTempFile := UniqueTempFilename(sWorkingDirectory);

  rqINfo.response.content.savetofile(sTempFile+'.html');
  rqInfo.response.DeleteFileOnDestroy := sTempFile+'.html';

  sParameters := '--webpage --compression=9 ' + sEncryption + sPassword + ' --browserwidth 900 --embedfonts --footer D./ --format pdf14 --outfile '
                 + sTempFile + ' --quiet ' + sOrientation + ' --size ' + sPageSize + ' "' + sTempFile+'.html'
                  + '" --permissions copy,print';

  if RunProgramAndWait(sProgram, sParameters, sWorkingDirectory,true) then begin
    rqInfo.Response.DeleteFileOnDestroy := sTempFile;
    objFileStream := TMemoryFileStream.Create(sTempFile, fmOpenRead);
    objFileStream.Seek(0,0);
    rqInfo.response.ContentLength := objFileStream.size;

    if IsIE(rqInfo) and IsPC(rqInfo) then
//      rqInfo.Response.ContentType := 'application/x-pdf'
      rqInfo.Response.ContentType := 'application/pdf'
    else
      rqInfo.response.contenttype := 'application/pdf';

    rqInfo.Response.ContentStream := objFileStream;
  end;

end;

procedure WRQ_AsyncTest(rqInfo: TRequestInfo);
var
  ap: TAsyncProcess;
begin
  ap := TPM.NeedThread<TAsyncProcess>(nil);
  ap.rqInfo := rqInfo;
  ap.CallbackProcedure := AWRQ_AsyncTest;
  sendAsyncResponse(rqInfo, ap.ProcessID);
  ap.Start;




end;



initialization

RQD.AddRequest('/async_test.ms', 'GET', wrq_asynctest);
RQD.AddRequest('/Async_Status.ms', 'GET', wrq_asyncstatus);
RQD.AddRequest('/Async_Wait.ms', 'GET', wrq_asyncWait);
RQD.AddRequest('/Async_Result.ms', 'GET', wrq_asyncResult);
RQD.AddRequest('/Async_Result.pdf', 'GET', wrq_AsyncPDFResult);


end.
