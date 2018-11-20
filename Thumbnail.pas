unit Thumbnail;
//Implements some image helper web-pages.

interface

uses systemx, RequestInfo, dir, WebConfig, stringx, stringx.ansi, graphics, extctrls, sysutils, webfunctions, jpeg, dialogs, classes, requestmanager, pngimage, debug;

procedure WRQ_QuickResizeImage(rqInfo: TRequestInfo);
procedure WRQ_ResizeImage(rqInfo: TRequestInfo);
function WRQ_Thumbnails(rqInfo: TRequestInfo): ansistring;
function WRQ_StreamImage(rqInfo: TRequestInfo): ansistring;

function GetThumbnailFileName(rqInfo: TRequestInfo; sFile: ansistring; sWidth: ansistring; sHeight: ansistring): ansistring;
function GetImageViews(rqInfo: TrequestInfo; sFile: ansistring): integer;
function GetViewsFile(rqInfo: TrequestInfo; sFile: ansistring): ansistring;
function IncImageViews(rqInfo: TrequestInfo; sFile: ansistring): integer;


implementation


uses WebString, EasyImage, simpleasync, MothershipWebserver;

function GetViewsFile(rqInfo: TrequestInfo; sFile: ansistring): ansistring;
begin
  result := ExtractFilePath(sFile)+'\Temp\'+ExtractFilename(sfile)+'.views.txt';
end;

function IncImageViews(rqInfo: TrequestInfo; sFile: ansistring): integer;
var
  sViewsFile: ansistring;
  sl: TStringList;
begin
  try
    sl := TStringlist.create;
    try
      sViewsFile := GetViewsFile(rqInfo, sFile);
      if fileexists(sViewsFile) then
        sl.loadfromfile(sViewsFile);

      try
        if sl.count > 0 then
          result := strtoint(sl[0])
        else
          result := 0;

      except
        result := 0;
      end;

      inc(result);
      sl.text := inttostr(result);
      sl.SaveToFile(sViewsFile);
    finally
      sl.free;
    end;
  except
    result := 0;
  end;
end;
function GetImageViews(rqInfo: TrequestInfo; sFile: ansistring): integer;
var
  sViewsFile: ansistring;
  sl: TStringList;
begin
  try
    sl := TStringlist.create;
    try
      sViewsFile := GetViewsFile(rqInfo, sFile);
      if fileexists(sViewsFile) then
        sl.loadfromfile(sViewsFile);


      result := strtoint(sl[0]);

    finally
      sl.free;
    end;
  except
    result := 0;
  end;
end;

function GetThumbnailFileName(rqInfo: TRequestInfo; sFile: ansistring; sWidth: ansistring; sHeight: ansistring): ansistring;
begin
  result := ExtractFilePath(sFile)+'\Temp\'+ExtractFilename(sfile)+'.'+swidth+'.'+sheight+ExtractFileExt(sfile);


end;


//------------------------------------------------------------------------------
procedure WRQ_ResizeImageJpeg(rqInfo: TRequestInfo);
//Implmements a simple web page request that performs a high-quailty resize of
//a larger jpeg image.  Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.
type
  TGiantColor = record
    r,g,b: int64;
  end;
  TFilterInfo = record
    color: TGiantColor;
    pixcount: integer;
  end;
var
  image1: TImage;
  imgOriginal: TImage;
  x,y: integer;
  r64,g64,b64: int64;
  pix: array of array of TFilterInfo;
  newx, newy: integer;
  jpeg: TJpegImage;
  sl: PAnsiChar;
  t: integer;
  dir: Tdirectory;
  iOHeight: integer;
  sFile: ansistring;
  sPath: ansistring;
  sName: ansistring;
  sThumb: ansistring;
  width,height: integer;
  fs: TfileStream;
begin
  jpeg := TJpegimage.create;
  try
    try
      if rqInfo.request.HasParam('quick') then begin
        WRQ_QuickResizeImage(rqInfo);
        exit;
      end;


      if rQInfo.request.hasparam('height') then begin
        height := strtoint(rqInfo.request['height']);
      end else begin
        height := 0;
      end;

      if rQInfo.request.hasparam('width') then begin
        width := strtoint(rqInfo.request['width']);
      end else begin
        width := 0;
      end;




      if rqInfo.request.hasparam('file') then begin
        sFile := rqInfo.request['file'];
      end else begin
        sFile := rqInfo.request['filename'];
      end;

      //if file is relative, convert to absolute path
      if (pos(':', sfile) = 0) and (pos('\\', sfile)=0) then begin
        sFile := slash(WebServerconfig.ExternalResourcePath)+sFile;
      end;

      sPath := ExtractFilePath(sFile);
      sPath := stringReplace(sPath, '..', '', [rfReplaceAll]);


      sThumb := slash(sPath)+'Temp\'+ExtractFilename(sFile)+'.'+inttostr(width)+'.'+inttostr(height)+ExtractFileExt(sFile);


      if NOT FileExists(sThumb)
      or (FileAge(sThumb) < FileAge(sFile))
      then begin
          jpeg.loadfromfile(sFile);
          if height = 0 then
            height := round(width * (jpeg.height/jpeg.width));

          if width = 0 then
            width := round(height * (jpeg.width/jpeg.height));


          easyimage.ResizeImage(jpeg, width,height);
          ForceDirectories(extractfilePath(sThumb));
          jpeg.SaveToFile(sThumb);
      end;


      fs := TFileStream.create(sThumb, fmOpenRead+fmShareDenyNone);
      fs.seek(0,0);

      rqInfo.response.contenttype := 'image/jpeg';
      rqInfo.response.contentlength := fs.Size;
      RQiNFO.response.needsprocessing := false;
      rqInfo.response.ContentStream := fs;

      if rQInfo.request.HasParamWithValue('tally') then begin
        IncImageViews(rqInfo, rqInfo.request['filename']);
      end;

    finally
      jpeg.free;
    end;
  except
  end;
end;


//------------------------------------------------------------------------------
procedure WRQ_ResizeImagePNG(rqInfo: TRequestInfo);
//Implmements a simple web page request that performs a high-quailty resize of
//a larger jpeg image.  Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.
type
  TGiantColor = record
    r,g,b: int64;
  end;
  TFilterInfo = record
    color: TGiantColor;
    pixcount: integer;
  end;
var
  image1: TImage;
  imgOriginal: TImage;
  x,y: integer;
  r64,g64,b64: int64;
  pix: array of array of TFilterInfo;
  newx, newy: integer;
  jpeg: TPNGImage;
  sl: PAnsiChar;
  t: integer;
  dir: Tdirectory;
  iOHeight: integer;
  sFile: ansistring;
  sPath: ansistring;
  sName: ansistring;
  sThumb: ansistring;
  width,height: integer;
  fs: TfileStream;
begin
  jpeg := TPNGImage.create;
  try
    try
      if rqInfo.request.HasParam('quick') then begin
        WRQ_QuickResizeImage(rqInfo);
        exit;
      end;


      if rQInfo.request.hasparam('height') then begin
        height := strtoint(rqInfo.request['height']);
      end else begin
        height := 0;
      end;

      if rQInfo.request.hasparam('width') then begin
        width := strtoint(rqInfo.request['width']);
      end else begin
        width := 0;
      end;




      if rqInfo.request.hasparam('file') then begin
        sFile := rqInfo.request['file'];
      end else begin
        sFile := rqInfo.request['filename'];
      end;
      sFile := StringReplace(sFile, '/','\',[rfReplaceAll]);

      //if file is relative, convert to absolute path
      if (pos(':', sfile) = 0) and (pos('\\', sfile)=0) then begin
        sFile := slash(WebServerconfig.ExternalResourcePath)+sFile;
      end;





      sPath := ExtractFilePath(sFile);
      sPath := stringReplace(sPath, '..', '', [rfReplaceAll]);


      sThumb := slash(sPath)+'Temp\'+ExtractFilename(sFile)+'.'+inttostr(width)+'.'+inttostr(height)+ExtractFileExt(sFile);


      if NOT FileExists(sThumb)
      or (FileAge(sThumb) < FileAge(sFile))
      then begin
          jpeg.loadfromfile(sFile);
          if height = 0 then
            height := round(width * (jpeg.height/jpeg.width));

          if width = 0 then
            width := round(height * (jpeg.width/jpeg.height));


          easyimage.ResizeImage(jpeg, width,height);
          ForceDirectories(extractfilePath(sThumb));
          jpeg.SaveToFile(sThumb);
      end;


      fs := TFileStream.create(sThumb, fmOpenRead+fmShareDenyNone);
      fs.seek(0,0);

      rqInfo.response.contenttype := 'image/png';
      rqInfo.response.contentlength := fs.Size;
      RQiNFO.response.needsprocessing := false;
      rqInfo.response.ContentStream := fs;

      if rQInfo.request.HasParamWithValue('tally') then begin
        IncImageViews(rqInfo, rqInfo.request['filename']);
      end;

    finally
      jpeg.free;
    end;
  except
    on E: Exception do begin
      debug.log('PNG RESIZE EXCEPTION:'+e.message, 'error');
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure WRQ_ResizeImageGIF(rqInfo: TRequestInfo);
//Implmements a simple web page request that performs a high-quailty resize of
//a larger jpeg image.  Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.
type
  TGiantColor = record
    r,g,b: int64;
  end;
  TFilterInfo = record
    color: TGiantColor;
    pixcount: integer;
  end;
var
  image1: TImage;
  imgOriginal: TImage;
  x,y: integer;
  r64,g64,b64: int64;
  pix: array of array of TFilterInfo;
  newx, newy: integer;
  jpeg: TPNGImage;
  sl: PAnsiChar;
  t: integer;
  dir: Tdirectory;
  iOHeight: integer;
  sFile: ansistring;
  sPath: ansistring;
  sName: ansistring;
  sThumb: ansistring;
  sThumbGif: ansistring;
  width,height: integer;
  fs: TfileStream;
begin
  jpeg := TPNGImage.create;
  try
    try
      if rqInfo.request.HasParam('quick') then begin
        WRQ_QuickResizeImage(rqInfo);
        exit;
      end;


      if rQInfo.request.hasparam('height') then begin
        height := strtoint(rqInfo.request['height']);
      end else begin
        height := 0;
      end;

      if rQInfo.request.hasparam('width') then begin
        width := strtoint(rqInfo.request['width']);
      end else begin
        width := 0;
      end;




      if rqInfo.request.hasparam('file') then begin
        sFile := stringReplace(rqInfo.request['file'], '/','\', [rfReplaceAll]);
      end else begin
        sFile := rqInfo.request['filename'];
      end;

      //if file is relative, convert to absolute path
      if (pos(':', sfile) = 0) and (pos('\\', sfile)=0) then begin
        sFile := slash(WebServerconfig.ExternalResourcePath)+sFile;
      end;

      sPath := ExtractFilePath(sFile);
      sPath := stringReplace(sPath, '..', '', [rfReplaceAll]);


      sThumb := slash(sPath)+'Temp\'+ExtractFilename(sFile)+'.'+inttostr(width)+'.'+inttostr(height)+ExtractFileExt(sFile);
      sThumbGif := ChangeFileExt(sThumb, '.gif');


      if NOT FileExists(sThumbGif)
      or (FileAge(sThumbGif) < FileAge(sFile))
      then begin
          jpeg.loadfromfile(sFile);
          if height = 0 then
            height := round(width * (jpeg.height/jpeg.width));

          if width = 0 then
            width := round(height * (jpeg.width/jpeg.height));


          easyimage.ResizeImage(jpeg, width,height);
          ForceDirectories(extractfilePath(sThumb));
          jpeg.SaveToFile(sThumb);
          ConvertPNGtoGIF(sThumb);
      end;


      fs := TFileStream.create(sThumbGif, fmOpenRead+fmShareDenyNone);
      fs.seek(0,0);

      rqInfo.response.contenttype := 'image/gif';
      rqInfo.response.contentlength := fs.Size;
      RQiNFO.response.needsprocessing := false;
      rqInfo.response.ContentStream := fs;

      if rQInfo.request.HasParamWithValue('tally') then begin
        IncImageViews(rqInfo, rqInfo.request['filename']);
      end;

    finally
      jpeg.free;
    end;
  except
    on E: Exception do begin
      debug.log('GIF RESIZE EXCEPTION:'+e.message, 'error');
    end;
  end;
end;



//------------------------------------------------------------------------------
procedure WRQ_ResizeImage(rqInfo: TRequestInfo);
var
  sFile: ansistring;
begin
  if rqInfo.request.hasparam('file') then begin
    sFile := rqInfo.request['file'];
  end else begin
    sFile := rqInfo.request['filename'];
  end;

  if lowercase(extractfileext(sFile))='.png' then begin
    if lowercase(rqInfo.request.documentext)='.gif' then begin
      WRQ_ResizeImageGIF(rqInfo);
    end else
      WRQ_ResizeImagePNG(rqInfo);
  end else begin
    WRQ_ResizeImagejpeg(rqInfo);
  end;




end;


//------------------------------------------------------------------------------
procedure WRQ_QuickResizeImage(rqInfo: TRequestInfo);
//Implmements a simple web page request that performs a high-quailty resize of
//a larger jpeg image.  Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.
var
  image1: TImage;
  imgOriginal: TImage;
  x,y: integer;
  r64,g64,b64: int64;
  newx, newy: integer;
  jpeg: TJpegImage;
  sl: PAnsiChar;
  iOWidth: integer;
begin
  jpeg := TJpegImage.create;
  jpeg.LoadFromFile(rqInfo.request['filename']);

  imgOriginal := Timage.create(nil);
  imgOriginal.autosize := true;
  imgOriginal.Picture.Bitmap.Assign(jpeg);
  image1 := Timage.create(nil);
  image1.width := strtoint(rqInfo.request['width']);
  image1.height := strtoint(rqInfo.request['height']);


  iOWidth := trunc((imgOriginal.width / imgOriginal.height) * image1.height);
  image1.picture.bitmap.pixelformat := pf32bit;
  image1.picture.bitmap.Width := image1.width;
  image1.picture.bitmap.height := image1.height;
  image1.picture.bitmap.canvas.lock;
  try
    image1.picture.Bitmap.Canvas.Rectangle(0,0,image1.width, image1.height);
    image1.picture.bitmap.Canvas.StretchDraw(Rect(0,0,iOWidth, image1.Height), imgOriginal.picture.bitmap);
    StreamImageAsJpeg(rqInfo, image1);


  finally
    image1.picture.bitmap.canvas.unlock;
  end;




end;


function WRQ_StreamImage(rqInfo: TRequestInfo): ansistring;
var
  s: TFileStream;
begin
  try
    s := TFileStream.create(rQInfo.request['filename'], fmOpenRead+fmShareDenyNone);
    rqInfo.response.contentstream := s;
    rqInfo.Response.ContentLength := s.Size;
    rqInfo.response.contenttype := 'image/jpeg';
    if rQInfo.request.HasParamWithValue('tally') then begin
      IncImageViews(rqInfo, rqInfo.request['filename']);
    end;

  except
  end;

end;

//------------------------------------------------------------------------------
function WRQ_Thumbnails(rqInfo: TRequestInfo): ansistring;
//Generates a webpage which has links to images in a folder and dynamically generated thumbnails for each image.
//Not used or dispatched, too CPU intensive currently.
//An experiment for use in the future potentially.
var
  t: integer;
  dir : TDirectory;
  sQuick: ansistring;
  sIMGURL: ansistring;
  sThumb: ansistring;
begin
  sQuick := '';
  if rqInfo.request.hasparam('quick') then
    sQuick := '&quick=true';
  result := '';
  dir := TDirectory.create(rqInfo.Request['dir'], '*.jpg', 0,0);
  try
    result := result + '<style type="text/css">'#13#10;
    result := result + '<!--'#13#10;
    result := result + '.picturecaption {'#13#10+
                      '	font-family: Arial, Helvetica, sans-serif;'#13#10+
                      '	font-size: 9px;'#13#10+
                      '}'#13#10;
    result := result + '-->'#13#10;
    result := result + '</style>';
    result := result + '<table border=0 width=640>'#13#10;
    rqInfo.request.Default('height', '0');
    for t:=0 to dir.filecount-1 do begin
      sImgURL := 'resize_image?width='+rqInfo.request['width']+'&height=0&lockaspect=true&filename='+encodewebstring(dir.files[t].fullname);
      sThumb := Getthumbnailfilename(rqInfo, dir.files[t].fullname, rqInfo.request['width'], '0');
      if (squick = '') and not fileexists(sThumb) then
        if not requestmanager.rqman.hasrequest('/'+sImgURL) then
          simpleasync.Asyncdispatch('/'+sImgURL)
      else
      if FileAge(sThumb) < FileAge(dir.Files[t].FullName) then begin
        if not rqman.hasrequest('/'+sImgURL) then
          simpleasync.Asyncdispatch('/'+sImgURL);
      end;


      if ((t) mod 4) = 0 then
        result := result+'<tr>'#13#10;
      result := result+'<td>';
      result := result+'';
      result := result+'<a href="stream_image?tally=true&filename='+encodewebstring(dir.files[t].fullname)+'">';
      result := result+'<img border=0 src="'+sImgURL+sQuick+'" width="[[[width]]]"/></a>';
      rQInfo.response.content.add(result);
      result := '';

      //Name and views
      result := result+'<table border=0 cellpad=0 callspace=0><tr>';
      result := result+'<td rowspan=2><p class="picturecaption">'+dir.files[t].namepart+'<BR><I>('+inttostr(GetImageViews(rqInfo, dir.files[t].fullname))+' views)</I></p></td>';
      //--
      sImgURL := 'resize_image?tally=true&width=800&height=0&lockaspect=true&filename='+encodewebstring(dir.files[t].fullname);
      result := result+'<td><div class="picturecaption"><a href="'+sImgURL+'">small</a></div></td>';
      sImgURL := 'resize_image?tally=true&width=1024&height=0&lockaspect=true&filename='+encodewebstring(dir.files[t].fullname);
      result := result+'<td><div class="picturecaption"><a href="'+sImgURL+'">regular</a></div></td>';
      sImgURL := 'resize_image?tally=true&width=1280&height=0&lockaspect=true&filename='+encodewebstring(dir.files[t].fullname);
      result :=result+'</tr><tr>';
      result := result+'<td><div class="picturecaption"><a href="'+sImgURL+'">large</a></div></td>';
      sImgURL := 'stream_image?tally=true&filename='+encodewebstring(dir.files[t].fullname);
      result := result+'<td><div class="picturecaption"><a href="'+sImgURL+'">full size</a></div></td>';
      result :=result+'</tr></table></td>';

      if ((t) mod 4) = 3 then
        result := result+'</tr>'#13#10;

      rQInfo.response.content.add(result);
      result := '';


    end;
    result := result+'</table>';
  finally
    dir.free;
  end;
  rqInfo.response.content.add(result);

end;



end.
