unit FileCache;
//This unit is essentially a singleton class that handles file caching.
//Files that are streamed from the middle-tier are held it memory so that
//they don't have to be loaded from disk more than once.

interface
uses
  RequestInfo, classes, windows, dialogs, sysutils,orderlyinit;

const
  MAX_FILE_COUNT = 5000;
  MAX_CACHEABLE_SIZE = 0;
type
  PCachedFile = ^CachedFile;



  CachedFile = record
    //Represents a cached file in the cache.
    Document: ansistring;
    DocumentExt: ansistring;
    ContentType: ansistring;
    Content: ansistring;
    Size: cardinal;
  end;


  procedure LockFileCAche;
  procedure UnlockFileCache;

  function GetCachedFile(rqInfo: TRequestInfo): PCachedFile;
  function StreamFileCached(rqInfo: TRequestInfo): boolean;
  function GetContentTypeForExt(sExt: ansistring): ansistring;
  procedure LoadStreamIntoResult(rqInfo: TrequestInfo);

var
  slFileIndex: TStringList;
  sectFileCache: _RTL_CRITICAL_SECTION;


implementation

uses WebFunctions, WebConfig, systemx, WebResource, EasyImage;

//------------------------------------------------------------------------------
function Streamfile(rqInfo: TRequestInfo): boolean;
//Streams a file from the TemplateRoot on demand.  Files that are stored in the directory
//defined in the "ExternalResourcePath" parameter in the Middle-Tier INI file are dispatched
//automatically through the employment of this function.
var
  fs: TfileStream;
  sFileName, sFile: ansistring;
  rec: PCachedFile;
  ifileSize: integer;
  bCacheable: boolean;
begin
  bCacheable := true;

  //intialize result
  result := false;

  //Process the file name
  sFileName := GetResourceFile(rqInfo, copy(rqInfo.Request.document, 2, length(rqInfo.Request.document)-1));
(*  if rqInfo.Request.hasparam('alt_template') then begin
    sFileName := WebServerConfig.ExternalResourcePath+rqInfo.request['alt_template']+'/'+copy(rqInfo.Request.document, 2, length(rqInfo.Request.document)-1);
    if not FileExists(sFileName) then begin
      sFileName := WebServerConfig.ExternalResourcePath+copy(rqInfo.Request.document, 2, length(rqInfo.Request.document)-1)
    end;
  end else
    sFileName := WebServerConfig.ExternalResourcePath+copy(rqInfo.Request.document, 2, length(rqInfo.Request.document)-1);*)
  if lowercase(ExtractFileExt(sFileName)) = '.png' then begin

  end;


  if lowercase(ExtractFileExt(sFileName)) = '.gif' then begin
    //if there is no GIF but there is a PNG then:
    if (NOT FileExists(sFileName)) then begin
      QueueConvertPNGtoGIF(ChangeFileExt(sFileName, '.png'));
    end else try
      if (FileAge(sFileName) < FileAge(ChangeFileExt(sFileName, '.png')))
      then begin
        QueueConvertPNGtoGIF(ChangeFileExt(sFileName, '.png'));
      end;
    except
    end;
  end;
  //If file exists then...
  if FileExists(sFileName) then begin
    //create a file stream object on the existing file
    fs := TFileStream.create(sFileName, fmOpenRead+fmShareDenyWrite);
    fs.Seek(0,0);
    try
      bCacheable := fs.Size < MAX_CACHEABLE_SIZE;
      if not bCacheable then begin
        rqInfo.response.ContentStream := fs;
        rqInfo.response.ContentLength := fs.Size;
        rqInfo.response.contentType := GetContentTypeForExt(rqInfo.request.documentext);
        if lowercase(copy(rqInfo.response.contentType,1,4)) = 'text' then begin
          LoadStreamIntoResult(rqInfo);
        end;
        result := true;

      end
      else begin
        //Force the length of the ansistring buffer that will hold the file
        iFileSize := fs.Size;
        SetLength(sFile, iFileSize);
        //read the file into the buffer
        fs.Read(sFile[1], fs.size);
        //Move the reference counted ansistring into the response
        //(this only copies a pointer to the ansistring unless something changes
        //the ansistring... which is not the case)
        //rqInfo.Response.content.Add(sfile);

        //Add the file to the cache
        result := false;
        New(rec);

        if rqInfo.Request.hasparam('alt_template') then
          rec.Document := rqInfo.request.Document+'?alt_template='+rqInfo.request['alt_template']
        else
          rec.Document := rqInfo.request.Document;

        rec.DocumentExt := rqInfo.request.DocumentExt;
        rec.contentType := GetContentTypeForExt(rqInfo.request.DocumentExt);
        rec.Content := sFile; //again this only copies a pointer to the ansistring
        rec.Size := iFileSize;


        LockFileCache;
        try
          //Add index of document to ansistring list
          slFileIndex.addObject(lowercase(rec.document), TObject(rec));
        finally
          UnlockFileCache;
        end;

      end;
      result :=true;
    finally
      //free the stream if added to the cache... else the calling methods free it
      if bCacheable then
        fs.free;
    end;
  end else begin
  end;
end;
//------------------------------------------------------------------------------
function StreamfileCached(rqInfo: TRequestInfo): boolean;
//Streams a file from the TemplateRoot on demand, but checks to see if its in the memory
//cache first.  If it is in the cache, then it uses the cached copy, else it
//calls StreamFile to stream the file.
var
  recFile: PCachedFile;

begin
  //Default value of result
  result := false;
  try

    //Try to find file in cache
    recFile := GetCachedFile(rqInfo);

    //If file NOT in cache then PUT the file in the cache
    if (recFile = nil) then begin
      result := StreamFile(rqInfo);
      if result then
        exit;

      if not result then begin
        recFile := GetCachedFile(rqInfo);
        if recFile = nil then
          result := false
      end;
    end;
    //if stream was assigned then exit... this is for large non-cacheable files
    if assigned(rqInfo.Response.ContentStream) then
      exit;

    //check again to see if in cache

    if recFile <> nil then begin
      rqInfo.Response.content.add(recFile.Content);
      rqInfo.response.ContentType := recFile.ContentType;
      rqInfo.Response.ContentLength := recFile.Size;
      result := true;
    end;

  except
//    AuditLog('error streaming file: '+rqInfo.request.document);
//    AuditLog('Exception: '+exception(exceptobject).Message);
  end;

end;


procedure LockFileCache;
//Locks the file cache so that other threads cannot write to it.  Uses a critical section to lock.
begin
  EnterCriticalSection(sectFileCache);
end;

procedure UnlockFileCache;
//Unlocks the cache so other threads have access. Uses a critical section to lock.
begin
  LeaveCriticalSection(sectFileCache);
end;


function GetCachedFile(rqInfo: TRequestInfo): PCachedFile;
//Returns a pointer to the cached file record if the file was cached
//else returns NIL
var
  iStringListIndex: integer;
  sDocument: ansistring;
begin
  LockFileCache;
  try
    if rqInfo.Request.hasparam('alt_template') then
      sDocument := rqInfo.request.Document+'?alt_template='+rqInfo.request['alt_template']
    else
      sDocument := rqInfo.request.Document;

    iStringListIndex := slFileIndex.IndexOf(lowercase(sDocument));

    //If file not found in cached document index then exit
    if iStringListIndex = -1 then begin
      result := nil;
      exit;
    end;

    //Else result is the pointer attached to the ansistring in the ansistringlist
    result := PCachedFile(pointer(slFileIndex.Objects[iStringListIndex]));
  finally
    UnLockFileCache;
  end;

end;

function GetContentTypeForExt(sExt: ansistring): ansistring;
//Returns an appropriate mime-type for a given file extension.  For example
//".jpg" returns "image/jpeg".  For inclusion in HTTP headers.   Currently the
//following are defined.<BR>
//.jpg: image/jpeg<BR>
//.swf: application/x-shockwave-flash<BR>
//.gif: image/gif<BR>
//.pdf: application/pdf<BR>
//.html: text/html<BR>
//.htm: text/html<BR>
//.mp3: audio/x-mpeg<BR>
//.lrm: application/encarta<BR>
//.exe: application/x-octet-stream<BR>
//.hqx: application/mac-binhex40<BR>
//.doc: application/msword<BR>
//.ica: application/x-ica<BR>
//<BR>
//Add new content types as necessary.

begin
  //Get the document extension
  sExt := lowercase(sExt);
  //Translate the document extension into the MIME type that corresponds
  //to it
  if sExt = '.svg' then begin
    result := 'image/svg+xml';
  end else
  if sExt = '.jpg' then begin
    result := 'image/jpeg';
  end else
  if sExt = '.png' then begin
    result := 'image/png';
  end else
  if sExt = '.swf' then begin
    result := 'application/x-shockwave-flash';
  end else
  if sExt = '.gif' then begin
    result := 'image/gif';
  end else
  if sExt = '.pdf' then begin
    result := 'application/pdf';
  end else
  if sExt = '.html' then begin
    result := 'text/html';
  end else
  if sExt = '.ms' then begin
    result := 'text/html';
  end else
  if sExt = '.js' then begin
    result := 'text/javascript';
  end else
  if sExt = '.htm' then begin
    result := 'text/html';
  end else
  if sExt = '.htc' then begin
    result := 'text/x-component';
  end else
  if sExt = '.ts' then begin
    result := 'text/typescript';
  end else
  if sExt = '.mp3' then begin
    result := 'audio/x-mpeg';
  end else
  if (sExt = '.lrm') then begin
    result := 'application/encarta';
  end else
  if (sExt = '.exe') then begin
    result := 'application/octet-stream';
  end else
  if (sExt = '.css') then begin
    result := 'text/css';
  end else
  if (sExt = '.cab') then begin
    result := 'application/octet-stream';
  end else
  if (sExt = '.hqx') then begin
    result := 'application/mac-binhex40';
  end else
  if (sExt = '.doc') then begin
    result := 'application/msword';
  end else
  if (sExt = '.wml') then begin
    result := 'text/vnd.wap.wml';
  end else
  if (sExt = '.mp4') then begin
    result := 'video/mp4';
  end else
  if sExt = '.ica' then begin
    result := 'application/x-ica';
 end;
end;

procedure LoadStreamIntoResult(rqInfo: TrequestInfo);
var
  s: TStream;
  buff: PAnsiChar;
  iSize : integer;
  sContent: ansistring;

begin

  iSize := rqInfo.response.ContentStream.Size;

  GetMem(buff, iSize);

  s:=rqInfo.response.ContentStream;

  s.Seek(0,0);

  s.ReadBuffer(buff[0], iSize);

  setLength(sContent, iSize);

  MoveMem32(@sContent[1], @buff[0], iSize);

  rqInfo.response.content.text := sContent;

  rqInfo.response.contentStream.free;
  rqInfo.response.contentStream := nil;

  FreeMem(buff);



end;

procedure oinit;
begin
  InitializeCriticalSection(sectFileCache);
  slFileIndex := TStringList.create;
  slFileIndex.Sorted := true;

end;

procedure ofinal;
begin

  DeleteCriticalSection(sectFileCache);
  slFileIndex.free;
  slFileIndex := nil;


end;

initialization
  init.RegisterProcs('FileCache', oinit, ofinal);

finalization

end.
