unit mothership_html_compiler;

interface

uses
  html_feeder_compiler, requestInfo, webstring, webfunctions, debug, pngimage, sysutils, stringx, stringx.ansi, systemx, namevaluepair, typex;

type

  TMothershipHTMLPromoter = class(THTMLPromoter)
  private
    FPNGtoGIF: tribool;
    FQuarterSize: tribool;
    FPNGtoSPAN: tribool;

    FrqINfo: TRequestInfo;
  public
    property rqInfo: TRequestInfo read FrqINfo write FRqInfo;
    procedure OnPromoteTag(var bHandled: boolean); override;
    function PromoteImageTag: boolean;
    procedure ChangeImageSourceToResized;
    function ShouldChangePNGToSpan: boolean;
    function ShouldChangePNGToGif: boolean;
    function ShouldQuarterSizeImages: boolean;

  end;

implementation

uses webconfig;

{ TMothershipHTMLPromoter }


procedure TMothershipHTMLPromoter.ChangeImageSourceToResized;
var
  src: string;
  sExt: string;
begin
  if (LastTag.Attributes.Has('width') or LastTag.Attributes.Has('height')) and (not LastTag.Attributes.Has('noautosize')) then begin
    src := LastTAg.Attributes.Items['src'].Value;
    if pos('?', src) > 0 then
      exit;

    sExt := ExtractFileExt(src);
    src := stringReplace(src, '/','\',[rfReplaceAll]);
    if LastTag.Attributes.Has('width') and LastTag.Attributes.Has('height') then begin
      src := 'resize_image'+sExt+'?file='+src+'&width='+lasttag.attributes.items['width'].Value+'&height='+lasttag.attributes.items['height'].Value;
    end else
    if LastTag.Attributes.Has('height') then begin
      src := 'resize_image'+sExt+'?file='+src+'&height='+lasttag.attributes.items['height'].Value;
    end else
    if LastTag.Attributes.Has('width') then begin
      src := 'resize_image'+sExt+'?file='+src+'&width='+lasttag.attributes.items['width'].Value;
    end;

    LastTag.Attributes.items['src'].Value := src;

  end;

end;

procedure TMothershipHTMLPromoter.OnPromoteTag(var bHandled: boolean);
begin
  bHandled := false;
  inherited;

//  if not IsIE(rqInfo) then
//    exit;


    if lowercase(LastTag.name) = 'img' then begin
      bHandled := PromoteImageTag;
    end;



end;

function TMothershipHTMLPromoter.PromoteImageTag: boolean;
//note: return true if handled... else false if you want the caller to rebuild the tag
//after return
var
  sResult: string;
  sWidth: string;
  sHeight: string;
  sSrc: string;
  sLeft, sRight: string;
  sExt: string;
  png: TPNGImage;
  sFile: string;
  nvp: TNameValuePairList;
  sResizeWidth: string;
  sResizeHeight: string;
  bAcceptPNG: boolean;
  sIEVersion: string;
  bHadQuestion: boolean;
begin

  nvp := nil;
  try


    sIEVErsion := GetIEVersion(rQInfo);

    ChangeImageSourceToResized; //if applicable

    if (sIEVersion <> '') and (strtofloat(sIEVersion) < 7.0) then begin
      result := false;
      bHadQuestion := SplitString(LastTag.Attributes.GetItemEx('src',''), '?', sLeft, sRight);
      if lowercase(ExtractFileExt(sLeft)) <> '.png' then
        exit;



      sLeft := changefileext(sLeft, '.gif');
      if bHadQuestion then
        sLeft := sLeft+'?'+sRight;

      LastTag.Attributes.Items['src'].Value := sLeft;


      exit;

    end;


//    if strtofloat(sIEVersion) >= 7.0 then begin
//      result := false;
//      exit;
//    end;


    sWidth := LastTag.Attributes.GetItemEx('width','0');
    sHeight := LastTag.Attributes.GetItemEx('height','0');
    sSRc := LastTag.Attributes.GetItemEx('src','');

    //nvp will = namevaluepair list of inline params
    nvp := DecodeURLParams(sSrc,sFile);

    //check if PNG, if not then exit;
    if SplitString(sFile, '.', sLeft, sright, true) then begin
      sExt := sRight;
      if lowercase(sExt)<>'png' then begin
        result := false;
        exit;
      end;

    end;

    //redefile file if "resize_image"
    if lowercase(sLeft) = 'resize_image' then begin
      sFile := nvp.GetItemEx('file','');
      sResizeWidth := nvp.GetItemEx('width','');
      sResizeHeight := nvp.GetItemEx('height','');

    end;

    exit;
    //load image to determine width and height
    sFile := slash(webconfig.WebServerConfig.ExternalResourcePath)+sFile;
    sFile := stringReplace(sFile, '/', '\', [rfReplaceAll]);
    if fileexists(sFile) then begin
      png := TPNGImage.create;
      try
        png.LoadFromFile(sFile);
        sWidth := inttostr(png.Width);
        sHeight := inttostr(png.Height);

        if lowercase(sLeft) = 'resize_image' then begin
          if sResizeWidth='' then begin
            sHEight := sResizeHeight;
            sWidth := inttostr(round((png.width/png.height)*strtoint(sResizeHeight)));
          end else begin
            sHeight := inttostr(round((png.height/png.width)*strtoint(sResizeWidth)));
            sWidth := sResizeWidth;
          end;

        end;

      finally
        png.free;
      end;
    end else begin
    end;

    sResult :=
      '<span title="'+LastTag.Attributes.GetItemEx('alt','')+'" '+
      'style="'+
      'width:'+sWidth+'px;'+
      'height:'+sHeight+'px;'+
      'display:inline-block;filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(src='''+LastTag.Attributes.GetItemEx('src','')+''', sizingMethod=''scale'');"></span>';

    self.AddToResult(sResult);
    result := true;

  finally
    nvp.free;
  end;


end;

function TMothershipHTMLPromoter.ShouldChangePNGToGif: boolean;
begin
  result := ((IsIE(rQInfo) and (strtofloat(GetIEVersion(rqINfo)) < 7.0)));
end;

function TMothershipHTMLPromoter.ShouldChangePNGToSpan: boolean;
begin
  result := false;
  exit;
  if FPNGtoGif = tbNull then begin
    result := NOT ShouldChangePNGToGif and (IsIE(rQInfo) and (strtofloat(GetIEVersion(rqINfo)) < 7.0));
    FPNGToGIF := BoolToTriBool(result);
  end else
    result := TriBoolToBool(FPNGtoGIF);

end;


function TMothershipHTMLPromoter.ShouldQuarterSizeImages: boolean;
begin
  result := pos('windows ce', rqInfo.request['user-agent']) > 0;
end;


end.
