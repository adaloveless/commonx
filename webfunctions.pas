unit WebFunctions;



interface
uses
  Variants, Classes, RequestInfo, DataObject, extCtrls, jpeg, WebResource,
  webstring, versioninfo, dialogs, glasscontrols, numbers;

type
  TShellButton = (sbBack, sbLogout, sbGroupTools, sbHome,
                  sbReport, sbHelp, sbAbout);
  TShellButtons = set of TShellButton;

  TScriptState = record
    slMaster: TStringList;
    sTag: string;
    iStartRow: integer;
    iStartCol: integer;
  end;

  TDataTierIDS = array of integer;

const
  WEB_APPLICATION_NAME = 'Mothership';
  EMAIL_COMMENTS = 'mailto:general@digitaltundra.com';
  INVALID_DATATIER = 255;
  ERROR_DATATIER = 254;
  MonthName: array[1..12] of string = ('January', 'February', 'March', 'April',
            'May', 'June', 'July', 'August', 'September', 'October', 'November',
            'December');

function FindTagPos(slStringList: TStringList;
  sBeginTag, sEndTag:string;
  var iStartCol, iStartRow: integer;
  out iEndCol, iEndRow: integer; out sCompleteTag, sTagBody: string ): boolean;
function GetStringListArea(sl: TStringList; istartcol, iendcol: integer; iStartRow, iEndRow: integer): string;
function AddURLParam(sURL, sParam: string): string;
//------------------------------------------------------------------------------
function GetMonth(dt: TDateTime): string;
function GetDay(dt: TDateTime): string;
function GetYear(dt: TDateTime): string;
//------------------------------------------------------------------------------
function ReadStringByte(sString: string; iStartPos: integer): integer;
function ReadStringByteDec32(sString: string; iStartPos: integer): integer;
function IsIE6(rqInfo: TRequestInfo): boolean;
function IsSpan(rqInfo: TRequestInfo): boolean;
function IsDiv(rqInfo: TRequestInfo): boolean;
function IsLayer(rqInfo: TRequestInfo): boolean;
function GetIEVersion(rqInfo: TRequestInfo): string;




function EncodePSTPassword(sPassword: string): string;
function TruncateHistory(iLevel: integer; sHistory: string): string;
function GetHistoryMinusOne(sHistory: string): string;
function GetLastHistory(sHistory: string): string;
function GetHistoryIndex(iIndex: integer; sHistory: string): string;
function GetHistoryCount(sHistory: string): integer;
function IsInHistory(sHistory: string; iID: integer): boolean;
function InsertHistory(sHistory: string; iInsertThisID, iAfterThisID: integer):string; overload;
function InsertHistory(sHistory: string; sInsertThisID, sAfterThisID: string):string; overload;

function IsMultiPart(rqInfo: TrequestInfo): boolean;
function GetMultiPartBoundary(rqInfo: TRequestInfo): string;
procedure DecodeBoundary(rqInfo: TRequestInfo; sBoundary: string; sBoundaryTag: string);
procedure DecodeMultiPart(rqInfo: TRequestInfo);


function Is9598(rqInfo: TRequestInfo): boolean; overload;
function IsNT(rqInfo: TRequestInfo): boolean;overload;
function IsPC(rqInfo: TRequestInfo): boolean;overload;
function IsMAC(rqInfo: TRequestInfo): boolean;overload;
function IsOSX(rqInfo: TrequestInfo): boolean;overload;
function IsIE(rqInfo: TRequestInfo): boolean;overload;
function IsNetscape(rqInfo: TRequestInfo): boolean;overload;
function IsSafari(rqInfo: TRequestInfo): boolean;overload;
function IsNetscape6(rqInfo: TRequestInfo): boolean;overload;
function Is9598(sUA: string): boolean;overload;
function IsNT(sUA: string): boolean;overload;
function IsPC(sUA: string): boolean;overload;overload;
function IsMAC(sUA: string): boolean;overload;
function IsOSX(sUA: string): boolean;overload;
function IsIE(sUA: string): boolean;overload;
function IsNetscape(sUA: string): boolean;overload;
function IsNetscape6(sUA: string): boolean;overload;

function GetSystemType(rqInfo: TRequestInfo): cardinal;
function MorphURL(sURL: string):string;

function SplitCSV(sString: string; sDelimiter: string = ','): TStringLIst;


function IntToHash(DataTierIDs: array of integer; xorByte: byte; iNumber: integer; bJunk: boolean=true): string;overload;
function IntToHash(DataTierIDs: array of integer; iNumber: integer): string;overload;
function HashToDataTierID(sHash: string; idxDataTier: integer): integer;
function HashToDataTierIDs(sHash: string): TDataTierIDs;
function GetDataTierCount(sHash: string): integer;
function HashToInt(sHashIn: string): integer;
function MorphHash(sHash: string): string;
function GetBoundary(rqInfo: TRequestInfo; sBoundaryTag: string; iStartingLine: integer; slAlternate: TStringList = nil): string;

procedure InfuseObjectPoolDebug(rqInfo: TRequestInfo);

//==============================================================================
function ImageToJpeg(imageComponent: TImage): TJpegImage;overload;//todo 1: move and lock in easy image
function ImageToJpeg(imageComponent: TGlassImage): TJpegImage;overload;//todo 1: move and lock in easy image
procedure StreamImageAsJpeg(rqInfo: TREquestInfo; image: TImage);overload;
procedure StreamImageAsJpeg(rqInfo: TREquestInfo; image: TGlassImage);overload;
procedure StreamImageAsPNG(rqInfo: TREquestInfo; image: TGlassImage);

procedure EncodeURLForInline(var sURL: string);

function MergeAtToken(slMaster: TStringList; sTokenWithoutDelimiters: string;
  sDetail: string): boolean; overload;
  //Merges two strings together ... replacing given token

function MergeAtToken(slMaster: TStringList; sTokenWithoutDelimiters: string;
  slDetail: TstringList): boolean; overload;
  //Merges two stringlists together ... replacing given token

function MergeAtToken(slMaster: TStringList; sDelimiters,
  sTokenWithoutDelimiters: string; sDetail: string; bRemoveDelimiters: boolean;
  var iStartCol, iStartRow: integer): boolean; overload;

function ReplaceTagParam(slMaster: TStringList;
  sParamWithoutQuotes: string; sReplacement: string): boolean;
  //Replaces a paramater (the kind inside tags) based on the VALUE of the param
  //written mainly to remove hyperlink references to graphics... for example
  //replacing: "/someurl.html" with "#"

//Returns the URL to files... JPGs GIFs... etc
function MergeAtToken(slMaster: TStringList; sDelimiters,
                      sTokenWithoutDelimiters: string; sDetail: string;
                      bRemoveDelimiters: boolean): boolean; overload;
function MergeAtToken(slMaster: TStringList;
                      sDelimiters, sTokenWithoutDelimiters: string;
                      slDetail: TStringList; bRemoveDelimiters: boolean;
                      var iStartCol, iStartRow: integer): boolean; overload;


function WebIncludeJavaScriptFunctions: string;
//Retunrs java script need to run web courseware

function WebHeapStatus: string;
//Shows memory usage of server


function WebTitle(sTitle: string): string;
//Macro for defining the title of the webpage

function EOL: string;
//Returns CR/LF

function WebImage(url: string): string;
//macro for defining a basic Image
function WebPictureCaption(sImageURL, sCaption, sTextAlign: string): string;



function ReplaceChunk(var sOrig: string; sBeginningOfTag, sEndOfTag, sReplacement : string) : boolean; overload;
//Used for cutting out or replacing tags  - sBeginingOfTag could be something
//like '<A' and sEndOfTag should be something like '>' to replace a tag
//like '<A HREF = "blah\blah.blah" OnMouseOver="whatever" OnMouseOut="something>
function ReplaceChunk(sl : TStringList; sBeginingOfTag, sEndOfTag, sReplacement : string) : boolean; overload;

procedure DisableButton(sl : TStringList; sLine, sImage : string);
procedure DisableAdminButton(sl: TStringList; sLine : string);
procedure DisableMainButton(sl: TStringList; sLine : string);
procedure DisableSmallAdminButton(sl: TStringList; sLine : string);
procedure DisableAndTurnAdminButton(sl: TStringList; sLine : string);
procedure EnableGToolMenu(sl : TStringList);
procedure EnableYellowBox(rqInfo: TRequestInfo);
procedure EnableRedline(rqInfo: TRequestInfo);

function  GetDateFromRequest(rqInfo: TRequestInfo; sYearVariable, sMonthVariable, sDayVariable :string): TDateTime;
function  Rpt_GetDate(sYear ,sMonth, sDay :string): TDateTime;
function  Rpt_SecondsToHhMmStr(iSeconds: integer): string;
{##############################################################################}
implementation

uses Sysutils, ClientConstants, DataObjectServices, WebConfig,
  CommonRequests, stringx, Exceptions, ErrorHandler,
  typex, systemx;



//------------------------------------------------------------------------------
function WebTitle(sTitle: string): string;
begin
  Result := '<title>'+sTitle+'</title>';
end;

//------------------------------------------------------------------------------
function WebHeapStatus: string;
var
  heap: THeapStatus;
begin
  result := '';
  result := result + '<H2>Mothership Framework Server</H2><BR>';
  result := result + '<STRONG>If you can read this.  The Mothership is communicating properly.</STRONG><BR>';

  heap := GetHeapStatus;
  result := result + '<P>';
  result := result + '<STRONG><FONT Color=#00007F>Server Heap Status (unoptimized)</FONT></STRONG><BR>';
  result := result + '<STRONG>Total Memory Allocated: </STRONG>'+inttostr(heap.TotalAllocated)+'<BR>';
  result := result + '<STRONG>Total Free Memory: </STRONG>'+inttostr(heap.TotalFree)+'<BR>';
  result := result + '<STRONG>Total Committed Memory: </STRONG>'+inttostr(heap.TotalCommitted)+'<BR>';
  result := result + '</P>';
end;
//------------------------------------------------------------------------------
procedure InfuseObjectPoolDebug(rqInfo: TRequestInfo);
begin
  MergeAtToken(rqInfo.response.content, 'body', rqInfo.response.VarPoolStatus+'`body`');
end;

//------------------------------------------------------------------------------
function WebBeginHTML: string;
begin
  result := '';
  result := result + '<HTML>';
end;

//------------------------------------------------------------------------------
function EOL: string;
begin
  result := #13#10;
end;

//------------------------------------------------------------------------------
function WebIncludeJavaScriptFunctions: string;
begin
  result := result + '<script language="JavaScript">'+#13#10;
  result := result + '<!--'+#13#10;
  result := result + 'function MM_openBrWindow(theURL,winName,features) { //v2.0'+#13#10;
  result := result + '  window.open(theURL,winName,features);'+#13#10;
  result := result + '}'+#13#10;
  result := result + '//-->}'+#13#10;
  result := result + '</script>'+#13#10;
end;

//------------------------------------------------------------------------------
function WebImage(url: string): string;
begin
  result := '<img src="'+url+'" border="0"  alt="">';
end;

//------------------------------------------------------------------------------
function WebPictureCaption(sImageURL, sCaption, sTextAlign: string): string;
begin
  result := '';
  result := '<TABLE BORDER = 0><TR><TD WIDTH="1%" VALIGN = "TOP">'+
    '<IMG SRC = "'+sImageUrL+'"></TD><TD VALIGN="'+sTextAlign+'">'+sCaption+'</TD></TR></TABLE>';
end;

//------------------------------------------------------------------------------
function ReadTag(slStringList: TStringList; sBeginTag, sEndTag:string;
    var iStartCol, iStartRow: integer): string;
begin
  //Find first occurance of start tag
end;

//------------------------------------------------------------------------------
function FindTagPos(slStringList: TStringList;
  sBeginTag, sEndTag:string;
  var iStartCol, iStartRow: integer;
  out iEndCol, iEndRow: integer; out sCompleteTag, sTagBody: string ): boolean;
var
  iCurrentCol, iCurrentEndCol, iCurrentRow: integer;
  sTemp: string;
  sLowerCaseBeginTag, sLowerCaseEndTag: string;
    //lowercase versions of requested begin and end tags
    //stored in local variable for REUSE SPEED

  iBeginTagLength, iEndTagLength: integer;
    //LENGTH of tags stored in local variable for
    //optimal speed
//  iTempItemCount: integer; //for optomization
    //LENGTH of tags stored in local variable for
    //optimal speed
  bBeginTagFound, bEndTagFound: boolean;
  sCurrentRow: string;
  iTemp: integer;
begin
  result := false;

  if slStringList.count = 0 then
    exit;

  //initialize search vars
  iCurrentCol:= iStartCol;
  iCurrentRow:= iStartRow;

  iBeginTagLength:= length(sBeginTag);
  iEndTagLength:= length(sEndTag);
  sLowerCaseBeginTag := lowercase(sBeginTag);
  sLowerCaseEndTag := lowercase(sEndTag);
  bBeginTagFound := false;

  if iBeginTagLength < 1 then
    raise Exception.create('Zero-length tag passed to FindTagPos');

  //find begin tag
  WHILE NOT bBeginTagFound DO BEGIN
    //scan strings
    sCurrentRow := lowercase(slStringList[iCurrentRow]);
    iTemp := pos(sLowerCaseBeginTag, sCurrentRow);
    if iTemp >0 then begin
      iCurrentCol := iTemp;
      iStartCol := iTemp;
      iStartRow := iCurrentRow;
      bBeginTagFound := true;
    end
    else begin
      inc(iCurrentRow);
      //if at end of document.. return false
      if iCurrentRow > (slStringList.count-1) then begin
        result := false;
        exit;
      end;
    end;
  END;

(*  //find begin tag
  WHILE NOT bBeginTagFound DO BEGIN
    //scan strings
    sCurrentRow := slStringList[iCurrentRow];
    sTemp := copy(sCurrentRow, iCurrentCol, iBeginTagLength);
    if sTemp = sLowerCaseBeginTag then begin
      iStartCol := iCurrentCol;
      iStartRow := iCurrentRow;
      bBeginTagFound := true;
    end
    else begin
      //if not found then
      inc(iCurrentCol);
      //if at end of line.. move to next line
      IF iCurrentCol>length(sCurrentRow) THEN BEGIN
        iCurrentCol := 0;
        inc(iCurrentRow);
        //if at end of document.. return false
        if iCurrentRow > (slStringList.count-1) then begin
          result := false;
          exit;
        end;
      END;
    end;
  END;*)

  //start searching for END tag after the BEGIN tag
  iCurrentEndCol := iCurrentCol+iBeginTagLength;
  bEndTagFound := false;
  while not bEndTagFound do begin
    sTemp := copy(slStringList[iCurrentRow], iCurrentEndCol, iEndTagLength);

    //if substring at current position matches current string
    //OR at the end of line... mark the end of the tag HERE
    if (lowercase(sTemp) = sLowerCaseEndTag)
    or (iCurrentEndCol >= length(slStringList[iCurrentRow])) then begin
      iEndCol := iCurrentEndCol;
      iEndRow := iCurrentRow;
      bEndTagFound := true;


      iTEmp := length(sEndTag);
      sCompleteTag := copy(slSTringList[iStartRow], iCurrentCol, (iEndCol - iStartCol)+iTemp);
      sTagBody := copy(slSTringList[iStartRow], iCurrentCol+iBeginTagLength, ((iEndCol - iStartCol)-iEndTagLength));
      result := true;
    end else begin;
      //increment the end column position if not found
      inc(iCurrentEndCol);
    end;
  end;
end;

//------------------------------------------------------------------------------
function MergeAtToken(slMaster: TStringList; sTokenWithoutDelimiters: string;
  slDetail: TstringList): boolean;
var
  iStartCol, iStartRow: integer;
begin
  iStartcol := 1;
  iStartRow := 0;
  result := MergeAtToken(slMaster, '`', sTokenWithoutDelimiters, slDetail, true, iStartCol, iStartRow);
end;

//------------------------------------------------------------------------------
function ReplaceTagParam(slMaster: TStringList;
  sParamWithoutQuotes: string; sReplacement: string): boolean;
//used to swap hyperlink references and graphic references... and otherwise
//anything typically enclosed in double-quotes ("").  Will NOT replace anything
//NOT enclose in double-quotes.
//slMaster is the TARGET string list
//sTokenWithoutDelimiters is the text inside the quotes that will be replaced.
//sDetail is the string that will replace the tag
//RETURNS: TRUE if something was replaced... else FALSE
//  Return value is useful when doing multiple-replacements of the same tag
//  as you can easily use it in a WHILE loop.
var
  iStartRow : integer;
  iStartCol : integer;
begin
  iStartRow := 0;
  iStartCol := 1;
  while MergeAtToken(slMaster, '"', sParamWithoutQuotes, sReplacement, false, iStartCol, iStartRow) do ;
  while MergeAtToken(slMaster, '''', sParamWithoutQuotes, sReplacement, false, iStartCol, iStartRow) do ;
  result := true;
end;

//------------------------------------------------------------------------------
function MergeAtToken(slMaster: TStringList; sTokenWithoutDelimiters: string; sDetail: string): boolean;
begin
  result := MergeAtToken(slMaster, '`', sTokenWithoutDelimiters, sDetail, true);

end;

//------------------------------------------------------------------------------
function MergeAtToken(slMaster: TStringList; sDelimiters, sTokenWithoutDelimiters: string; sDetail: string; bRemoveDelimiters: boolean): boolean; overload;
var
  iStartRow : integer;
  iStartCol : integer;
begin
  iStartRow := 0;
  iStartCol := 1;
  result := MergeAtToken(slMaster, sDelimiters, sTokenWithoutDelimiters, sDetail, bRemoveDelimiters, iStartCol, iStartRow);

end;

//------------------------------------------------------------------------------
function MergeAtToken(slMaster: TStringList; sDelimiters, sTokenWithoutDelimiters: string; sDetail: string; bRemoveDelimiters: boolean; var iStartCol, iStartRow: integer): boolean; overload;
//Replaces text enclosed in ` characters with sDetail.
//slMaster is the TARGET string list
//sTokenWithoutDelimiters is the text inside the ` chars that will be replaced.
//sDetail is the string that will replace the tag
//e.g.    MergeAtToken(slTarget, 'hello', 'hello world'); will replace
// '`hello`' in the target list with 'hello world'
//RETURNS: TRUE if something was replaced... else FALSE
//  Return value is useful when doing multiple-replacements of the same tag
//  as you can easily use it in a WHILE loop.
var
  iEndRow, iEndCol: integer;
  sCompleteTag: string;
  sTagBody: string;
  iLeftStart, iRightStart: integer;
  iLeftLength, iRightLength: integer;
  iPad: integer;
begin
  if bRemoveDelimiters then
    iPad := 0
  else
    iPad := 1;

  result := FindTagPos(slMaster, sDelimiters+sTokenWithoutDelimiters, sDelimiters,
    iStartCol, iStartRow, iEndCol, iEndRow, sCompleteTag, sTagBody);

  if result then begin
    //calc the starting position of the string that is on the left of the tag (1 or 0)
    iLeftStart := 1;
    iLeftLength := iStartCol-1;
    iRightStart :=  iEndCol+1;
    iRightLength := length(slMaster[iStartRow])-(iEndCol);

    //Concatinate the row
    slMaster[iStartRow] := copy(slMaster[iStartRow],iLeftStart, iLeftLength+iPad) +
                           sDetail +
                           copy(slMaster[iStartRow],iRightStart-iPad, iRightLength+iPad);
  end;
end;
//------------------------------------------------------------------------------
function MergeAtToken(slMaster: TStringList; sDelimiters, sTokenWithoutDelimiters: string; slDetail: TStringList; bRemoveDelimiters: boolean; var iStartCol, iStartRow: integer): boolean; overload;
//Replaces text enclosed in ` characters with sDetail.
//slMaster is the TARGET string list
//sTokenWithoutDelimiters is the text inside the ` chars that will be replaced.
//sDetail is the string that will replace the tag
//e.g.    MergeAtToken(slTarget, 'hello', 'hello world'); will replace
// '`hello`' in the target list with 'hello world'
//RETURNS: TRUE if something was replaced... else FALSE
//  Return value is useful when doing multiple-replacements of the same tag
//  as you can easily use it in a WHILE loop.
var
  iEndRow, iEndCol: integer;
  sCompleteTag: string;
  sTagBody: string;
  iLeftStart, iRightStart: integer;
  iLeftLength, iRightLength: integer;
  iPad: integer;
  t: integer;
  sStartRowLeft, sStartRowRight: string;
begin
  if bRemoveDelimiters then
    iPad := 0
  else
    iPad := 1;

  result := FindTagPos(slMaster, sDelimiters+sTokenWithoutDelimiters, sDelimiters,
    iStartCol, iStartRow, iEndCol, iEndRow, sCompleteTag, sTagBody);

  if result then begin
    //calc the starting position of the string that is on the left of the tag (1 or 0)
    iLeftStart := 1;
    iLeftLength := iStartCol-1;
    iRightStart :=  iEndCol+1;
    iRightLength := length(slMaster[iStartRow])-(iEndCol);

    //Concatinate the row
    sStartRowLeft := copy(slMaster[iStartRow],iLeftStart, iLeftLength+iPad);
    sStartRowRight := copy(slMaster[iStartRow],iRightStart-iPad, iRightLength+iPad);

    case slDetail.count of
      0: slMaster[iStartRow] := sStartRowLeft+sStartRowRight;
      1: slMaster[iStartRow] := sStartRowLeft+slDetail[0]+sStartRowRight;
      else begin
        slMaster[iStartRow] := sStartRowLeft+slDetail[0];
        for t:= 1 to slDetail.count-2 do begin
          slMaster.Insert(iStartrow+t, slDetail[t]);
        end;
        slMaster.Insert(iStartrow+slDetail.count-1, slDetail[slDetail.count-1]+sStartRowRight);
      end;
    end;//case
  end;
end;

//------------------------------------------------------------------------------
(*procedure ReplaceEscSequences(response: TMotherShipWebResponse; slMaster: TStringList);
begin
  //scan sl master for 1st and second [[[ tags found
  while Scan(slMaster, iStartX, iStartY, iEndX, iEndY, sTagBody) do begin

  end;
end;*)
//------------------------------------------------------------------------------
(*function Scan(slMaster: TStringList, var iStartX, iStartY:integer; out iEndX, iEndY: integer): boolean;
type
  TStage = (stagenone, stage1, stage2, stagematch);
var
  Stage: TStage;
  b1, b2 ,b3, bMatch: boolean;
  X, Y: integer;
  iLength: integer;
  iListSize: integer;
  sTemp: string;
  iResultX: integer;
begin
  //Set initial values
  try
    x:= iStartX;
    y:= iStartY;
    iLength := length(slMaster[y]);
    iListSize := slMaster.count;
    iResultX := 1;
  except
    result := false;
    exit;
  end;


//******************************************************************************
  //stage 1 find begin tag
  //while dual ]]] is not found
  while (not bMatch) and (not bEOF) do begin
    //check current char
    case slMaster[y][x] of
      '%': stage := stage1;
      '0': if stage = stage1 then stage = stage2;
      '1': if stage = stage2 then stage = stagematch;
    else begin
      stage := stageNone;
    end;

    if stage = stagematch then
      break;

    //next char
    inc(x);

    //check X against length of string
    if X > iLength then begin
      //setup next line
      x := 1;
      inc(y);

      //check if EOF
      if y> iListSize then begin
        result := false;
        exit;
      end;
      //set line length
      iLength := length(slMaster[y]);
    end;
  end;

//******************************************************************************
  //stage 2 find end tag and build the body
  //stage 1 find begin tag
  //while dual [[[ is not found
  while (not bMatch) and (not bEOF) do begin
    //check current char
    case slMaster[y][x] of
      '%': stage := stage1;
      '0': if stage = stage1 then stage = stage2;
      '1': if stage = stage2 then stage = stagematch;
    else begin
      //if previously in a stage but bombed... add to result tag
      if stage = stage1 then

      stage := stageNone;
    end;




    if stage = stagematch then
      break;

    //next char
    inc(x);

    //check X against length of string
    if X > iLength then begin
      //setup next line
      x := 1;
      inc(y);

      //check if EOF
      if y> iListSize then begin
        result := false;
        exit;
      end;
      //set line length
      iLength := length(slMaster[y]);
    end;
  end;

  //while dual ]]] is not found
  while (not bMatch) and (not bEOF) do begin




    //if % then b1
    if
  end;
end;*)


























































//------------------------------------------------------------------------------
function WebGraphicForActivity(doActivity: TDataObject): string;
var
  iSpecies: integer;
begin
  iSpecies := doActivity['Species'].AsVariant;

  case iSpecies of
    0: result := 'images/sm_yellow_folder_closed.gif';
  else
    result := 'images/sm_assignment.gif';
  end;
end;

(*//------------------------------------------------------------------------------
procedure LoadErrorPopUp(sl: TStringList; sessionID: integer; backURL, errorURL: string);
begin
  LoadWebResource(sl, 'ErrorRedirect.html');
  ReplaceTagParam(sl, 'back_url', backURL);
  ReplaceTagParam(sl, 'error_url', errorURL);
end;*)

(*//------------------------------------------------------------------------------
procedure LoadConfirmationShell(rqInfo: TRequestInfo; sl: TStringList; sessionID: integer);
begin
  //load the shell
  LoadWebResource(sl, 'confirmation.html');
end;*)





//------------------------------------------------------------------------------
procedure EncodeURLForInline(var sURL: string);
//This function encodes a URL so that it can be passed as a string parameter
//to another page... in-line.

var
  iPos : integer;
begin
  //change ?'s and &'s to hex ordinals
  while pos('?', sURL)>0 do begin
    iPos := pos('?', sURL);
    sURL := copy(sURL, 1, iPos-1)+'%'+inttohex(ord('?'), 2)+copy(sURL, iPos+1, length(sURL));
  end;

  while pos('&', sURL)>0 do begin
    iPos := pos('&', sURL);
    sURL := copy(sURL, 1, iPos-1)+'%'+inttohex(ord('&'), 2)+copy(sURL, iPos+1, length(sURL));
  end;

end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function ReplaceChunk(var sOrig: string; sBeginningOfTag, sEndOfTag, sReplacement: string) : boolean;
//Used for cutting out or replacing tags  - sBeginingOfTag could be something
//like '<A' and sEndOfTag should be something like '>' to replace a tag
//like '<A HREF = "blah\blah.blah" OnMouseOver="whatever" OnMouseOut="something>
var
  sNewString, sTempOrig, sTempBegin, sTempEnd, sTemp : string;
  iStart, iEnd, iTemp : integer;
begin

  //Get past case sensitivity problems...
  sTempOrig := LowerCase(sOrig);
  sTempBegin := LowerCase(sBeginningOfTag);
  sTempEnd := LowerCase(sEndOfTag);

  //Search for start and end tags
  iStart := pos(sTempBegin, sTempOrig);
  iEnd := pos(sTempEnd, sTempOrig);

  if iEnd > 0 then begin
    while iEnd < iStart do begin
      sTemp := copy(sOrig, iEnd + 1, Length(sOrig));
      iTemp := pos(sTempEnd, sTemp);
      iEnd := iEnd + iTemp;
    end;
  end;

  if iEnd > 0 then
    iEnd := iEnd + Length(sEndOfTag) - 1;
  if (iStart = 0) or (iEnd = 0) then
    result := false
  else begin
    sNewString := copy(sOrig, 1, iStart - 1) + sReplacement + copy(sOrig, iEnd + 1, Length(sOrig));
    sOrig := sNewString;
    result := true;
  end;
end;

//------------------------------------------------------------------------------
function ReplaceChunk(sl : TStringList; sBeginingOfTag, sEndOfTag, sReplacement : string) : boolean; overload;
// probably needs to be written better...
var
  sTemp : string;
begin
  sTemp := sl.text;
  result := ReplaceChunk(sTemp, sBeginingOfTag, sEndOfTag, sReplacement);
  sl.Clear;
  sl.Text := sTemp;
end;

//------------------------------------------------------------------------------
procedure DisableButton(sl : TStringList; sLine, sImage : string);
begin
  //OBSOLETE DO NOT CALL
  exit;
end;

//------------------------------------------------------------------------------
procedure DisableAdminButton(sl: TStringList; sLine : string);
// Disables an admin button
begin
  DisableButton(sl, sLine, 'images/admin_tri.gif');
end;


//------------------------------------------------------------------------------
procedure DisableMainButton(sl: TStringList; sLine : string);
// Disables a button from the main shell (Login screen, help...)
begin
  DisableButton(sl, sLine, 'images/login_tri.gif');
end;

//------------------------------------------------------------------------------
procedure DisableSmallAdminButton(sl: TStringList; sLine : string);
// Disables a small button like from the small sub menu under group tools
begin
  DisableButton(sl, sLine, 'images/admin_tri2.gif');
end;

//------------------------------------------------------------------------------
procedure DisableAndTurnAdminButton(sl: TStringList; sLine : string);
// Will turn an admin button and disable it - like what happens to the group
// tools button when the small sub menu comes up
begin
  DisableButton(sl, sLine, 'images/admin_tri3.gif');
end;

//------------------------------------------------------------------------------
procedure EnableGToolMenu(sl : TStringList);
// This function will enable the group tools sub menu
begin
  //OBSOLETE DO NOT CALL
end;
//------------------------------------------------------------------------------
procedure EnableYellowBox(rqInfo: TRequestInfo);
// This function enables the yellow box on the login screen.  Cheesy, I know.
// Perhaps in the future, we might want to control the contents of this
// happy little yellow box?
begin
//  ReplaceChunk(sl, '<!--yellow_', 'box', '');
//  ReplaceChunk(sl, 'yellow_box', '-->', '');
  rqINfo.response.varpool['enable_yellow_box'] := 'true';
end;

//------------------------------------------------------------------------------
procedure EnableRedline(rqInfo: TRequestInfo);
// This function enables the red line used in the main shell for the login
// and self enroll pages.  Also, cheesy, I know.
(*var
  x, y, xEnd, yEnd : integer;
  sLineWithComments, sLineWOComments : string;*)
begin
  rqInfo.response.varpool['enable_red_line'] := 'true';
(*  x := 1;
  y := 0;
  FindTagPos(sl, '<!--red_line-->', '<!--end-->', x, y, xEnd, yEnd, sLineWithComments, sLineWOComments);
  ReplaceChunk(sLineWithComments, '<!--expose_', 'red_line', '');
  ReplaceChunk(sLineWithComments, 'expose_red_line', '-->', '');

  if sl.count > 0 then
    sl[y] := sLineWithComments
  else
    sl.add(sLineWithComments);*)

end;

//------------------------------------------------------------------------------
function ImageToJpeg(imageComponent: TImage): TJpegImage;
begin
  imageComponent.Canvas.Lock;
  try
    result := TJpegImage.create;
    result.COmpressionQuality := 100;
    result.PixelFormat := jf24Bit;
    result.assign(imageComponent.picture.bitmap);

  finally
    imageComponent.Canvas.unLock;
  end;
end;

function ImageToJpeg(imageComponent: TGlassImage): TJpegImage;
begin
  imageComponent.Canvas.Lock;
  try
    result := TJpegImage.create;
    result.COmpressionQuality := 100;
    result.PixelFormat := jf24Bit;
    result.assign(imageComponent.picture.bitmap);

  finally
    imageComponent.Canvas.unLock;
  end;
end;


//------------------------------------------------------------------------------
function QImageToJpeg(imageComponent: TImage): TJpegImage;
begin
  imageComponent.Canvas.Lock;
  try
    result := TJpegImage.create;
    result.COmpressionQuality := 100;
    result.PixelFormat := jf24Bit;
    result.assign(imageComponent.picture.bitmap);

  finally
    imageComponent.Canvas.unLock;
  end;
end;


//------------------------------------------------------------------------------
procedure StreamImageAsJpeg(rqInfo: TREquestInfo; image: TImage);
var
  ms: TMemoryStream;
  jpeg : TJPEGImage;
begin
  ms := TmemoryStream.create;
  rqInfo.Response.ContentType := 'image/jpeg';
  jpeg := ImageToJpeg(image);
  try
    ms.seek(0,0);
    ms.SetSize(jpeg.InstanceSize);
    jpeg.SaveToStream(ms);
    rqInfo.response.ContentStream := ms;
    rqInfo.REsponse.ContentLength := ms.Size;
  finally
    jpeg.free;
    //note.... memory stream is freed by upper level TRequestInfo handler;
  end;
end;

//------------------------------------------------------------------------------
procedure StreamImageAsJpeg(rqInfo: TREquestInfo; image: TGlassImage);
var
  ms: TMemoryStream;
  jpeg : TJPEGImage;
begin
  ms := TmemoryStream.create;
  rqInfo.Response.ContentType := 'image/jpeg';
  jpeg := ImageToJpeg(image);
  try
    ms.seek(0,0);
    ms.SetSize(jpeg.InstanceSize);
    jpeg.SaveToStream(ms);
    rqInfo.response.ContentStream := ms;
    rqInfo.REsponse.ContentLength := ms.Size;
  finally
    jpeg.free;
    //note.... memory stream is freed by upper level TRequestInfo handler;
  end;
end;



//------------------------------------------------------------------------------
procedure StreamImageAsPNG(rqInfo: TREquestInfo; image: TGlassImage);
var
  ms: TMemoryStream;
begin
  ms := TmemoryStream.create;
  rqInfo.Response.ContentType := 'image/png';
  try
    ms.seek(0,0);
    image.Picture.Graphic.SaveToStream(ms);
    rqInfo.response.ContentStream := ms;
    rqInfo.REsponse.ContentLength := ms.Size;
  finally
    //note.... memory stream is freed by upper level TRequestInfo handler;
  end;
end;

//------------------------------------------------------------------------------
function GetStringListArea(sl: TStringList; istartcol, iendcol: integer; iStartRow, iEndRow: integer): string;
var
  t: integer;
begin
  result := '';

  for t:= iStartRow to iEndRow do begin
    if iStartRow = iEndRow then begin
      result := copy(sl[t], iStartCol, (iEndCol-iStartCol));
    end else
    if t = iStartRow then begin
      result := result + copy(sl[t], iStartRow, length(sl[t]));
    end else
    if t = iEndRow then begin
      result := result + copy(sl[t], 1, iEndCol-1);
    end else begin
      result := result + sl[t];
    end;
  end;



end;

//------------------------------------------------------------------------------
procedure ReplicateStringListAreaOld(sl: TStringList; iStartRow, iEndRow, iRepeatCount: integer; sDelimiters: string = '');
var
  bAtEnd: boolean;
  t, u, v: integer;
  slDelimiters: TStringList;
  slTemp: TStringLIst;
//  sTemp: string;
  bFlag: boolean;
begin
  slDelimiters := TStringList.create;
  slTemp := TStringList.create;

  try
    slDelimiters.text := sDelimiters;
    bAtEnd := (iEndRow = sl.count-1);

    //if the repeat count is 0 then delete the section... because it shouldn't exist
    if iRepeatCount= 0 then begin
      for t:= iEndrow downto iStartRow do begin
        sl.delete(t);
      end;


    end
    else begin
      bFlag := true;
      for u:= 1 to iRepeatCount-1 do
      for t:= iStartRow to iEndRow do begin
(*        if pos(':repeatforobject', lowercase(sl[t])) > 0 then begin
          bFlag := true;
        end;*)

        if (1=1) or (bFlag) then
          slTemp.add(sl[t])
        else begin
          slTemp[slTemp.Count-1] := slTemp[slTemp.count-1] + #13#10+sl[t];
        end;
        bFlag := false;
(*        if pos(':repeatforobject', lowercase(sl[t])) > 0 then begin
          bFlag := true;
        end;*)
      end;
      //sTemp := slTemp.text;


      for u:= 1 to iRepeatCount-1 do begin
        //if the end is at the end of the string list
        if bAtEnd then begin
          //then add the replications to the end of the stringlist
          for v := 0 to slTemp.count-1 do begin
            sl.add(slTemp[v]);
          end;

          if sDelimiters <> '' then begin
            if slDelimiters.count = 1 then
              sl.Add(sDelimiters)
            else
              sl.Add(slDelimiters[u]);
          end;
        end
        //else if the end of the repeated section is in the middle of the stringlist
        else begin
          //insert the repeated section after the last line of the repeated section


          for v := slTemp.count-1 downto 0 do begin
            sl.insert(iEndRow+1, slTemp[v]);
          end;


          if sDelimiters <> '' then begin
            if slDelimiters.count = 1 then
              sl.insert(iEndRow+1, sDelimiters)
            else
              sl.insert(iEndRow+1, slDelimiters[iRepeatCount-u-1]);
          end;

        end;
      end;
    end;
  finally
    slDelimiters.free;
    slTemp.free;
  end;

end;


//------------------------------------------------------------------------------
function  GetDateFromRequest(rqInfo: TRequestInfo; sYearVariable, sMonthVariable, sDayVariable :string): TDateTime;
begin
  result := Rpt_GetDate(rqInfo.request[sYearVariable], rqInfo.request[sMonthVariable], rqInfo.request[sDayVariable]);
end;
//------------------------------------------------------------------------------
function Rpt_GetDate(sYear ,sMonth, sDay :string): TDateTime;
{convert selection dates into TDateTime}
var
  dd, mm, yy :integer;
  dtTemp: TDateTime;
begin
  yy:=StrToIntDef(sYear, 0);
  mm:=StrToIntDef(sMonth, 0);
  dd:=StrToIntDef(sDay, 0);

  try
    dtTemp := EncodeDate(yy, mm, dd);
  except
    on E: Exception do begin
      raise ENewException.create(0, 'The date ' + MonthName[strtoint(sMonth)] + ', ' + sDay + ' ' + sYear+
      ' is not a real calendar date.' + '<br>' + 'Please go back and enter a valid date.', '');
    end;
  end;

  result:=dtTemp;

end;
//------------------------------------------------------------------------------
function GetMonth(dt: TDateTime): string;
var
  wMonth, wDay, wYear: word;
begin
  DecodeDate(dt, wYear, wMonth, wDay);
  result := inttostr(wMonth);
end;
//------------------------------------------------------------------------------
function GetDay(dt: TDateTime): string;
var
  wMonth, wDay, wYear: word;
begin
  DecodeDate(dt, wYear, wMonth, wDay);
  result := inttostr(wDay);
end;
//------------------------------------------------------------------------------
function GetYear(dt: TDateTime): string;
var
  wMonth, wDay, wYear: word;
begin
  DecodeDate(dt, wYear, wMonth, wDay);
  result := inttostr(wYear);
end;

//------------------------------------------------------------------------------
function Rpt_SecondsToHhMmStr(iSeconds: integer): string;
{Used to get Time On Task as a string formatted as hh:mm given input of seconds.}
var
  iH, iM :integer;
begin
  Result:='0:00';
  if iSeconds>29 then begin  {at least 1 min}
    iH:=iSeconds div 3600;
    iM:=Round((iSeconds-iH*3600)/60);
    if iM<10 then
      result:=IntToStr(iH)+':0'+IntToStr(iM)  {add leading 0 to mm.}
    else
      result:=IntToStr(iH)+':'+IntToStr(iM);
  end;
end;
//------------------------------------------------------------------------------

function IntToHash(DataTierIDs: array of integer; xorByte: byte; iNumber: integer; bJunk: boolean=true): string;overload;
var
  sHexTemp: string;
  t: integer;
  sPrefix: string;
const
  KEY = 'H@Ppìë%!';
begin
  if Length(DataTierIDs) < 2 then
    raise Exception.create('Hash must have at least two databases');

  //How the hash works -- simple 8-bit encryption...
  //with the KEY in the hash itself (so that it can morph from page-to-page)
  //futher scrambled with static key to make encoding scheme less obvious.
  //KKFFFFFFFF == XB is the XOR byte for

  //***BUILD THE PREFIX -- the UNENCRYPTED part which contains INFORMATION

  //put the COUNT of datatiers in the front
  sPrefix := copy(InttoDec32(Length(DataTierIDs),2), 1,2);

  //add each DATATIER to the PREFIX
  for t := Low(DataTierIDs) to High(DatatierIDs) do begin
    sPrefix := sPrefix + copy(inttodec32(DataTierIDs[t], 2), 1,2);
  end;

  //MAIN ID
  //put the XOR BYTE in the main result
  result := inttodec32(xorByte, 2);

  sHexTemp := inttodec32(iNumber, 8);

  //do the encryption
  for t:= 1 to 8 do begin
    //xor the key byte into the string
    sHexTemp[t] := char( ord(sHexTemp[t]) xor xorByte );
    //xor the static key into the string
    sHexTemp[t] := char( ord(sHexTemp[t]) xor ord(KEY[t]));
    //xor the clientIP string in reverse order
(*    sHexTEmp[t] := chr( ord(sHexTemp[t]) xor ord(sClientIP[(length(sClientIP)-t)+1]));*)
  end;

  //Expand sHexTemp into
  for t:= 1 to 4 do begin
    result := result + inttodec32(ord(sHexTemp[t]), 2);
  end;

  result := result;

  for t:= 5 to 8 do begin
    result := result + inttodec32(ord(sHexTemp[t]), 2);
  end;

  //tag the PREFIX on the FRONT of the RESULT

  result := sPrefix+result;
  if bJunk then begin
    result := result+chr(random(26)+ord('A'))+chr(random(26)+ord('A'))+chr(random(26)+ord('A'))+chr(random(26)+ord('A'))
  end;

end;
//------------------------------------------------------------------------------
function HashToDataTierID(sHash: string; idxDataTier: integer): integer;
var
  sTemp: string;
begin
    if idxDataTier > (GetDataTierCount(sHash)) then

    raise Exception.create('Session has '+inttostr(GetDataTierCount(sHash))+' databases defined.  Expecting '+inttostr(idxDataTier));
  try
    result := INVALID_DATATIER;
    sTemp := copy(sHash, 3+(idxDataTier*2), 2);
    if sTemp <> '' then begin
      result := Dec32ToInt(sTemp);
    end;
  except
    raise Exception.create('Could not extract valid destination from session ID.  SessionID is corrupt.');
  end;

end;
//------------------------------------------------------------------------------
function IntToHash(DataTierIDs: array of integer; iNumber: integer): string;overload;
begin
  result := IntToHash(DataTierIDs, random(255), iNumber);
end;
//------------------------------------------------------------------------------
function MorphHash(sHash: string): string;
var
  ids: TDataTierIDs;
begin
  SetLength(ids,0);
  result := sHash;
  while (result = sHash) do begin
    ids := HashToDataTierIDs(sHash);
    result := IntToHash(ids, HashToInt(sHash));
  end;
end;
//------------------------------------------------------------------------------
function HashToInt(sHashIn: string): integer;
var
  iXorByte: integer;
  sContent: string;
  iByte: integer;
  sHexTemp: string;
  t: integer;
  iDTCount: integer;
  sHash: string;
const
  KEY = 'H@Ppìë%!';
begin
  sHash := sHashIn;
  try
    //How the hash works -- simple 8-bit encryption...
    //with the KEY in the hash itself (so that it can morph from page-to-page)
    //futher scrambled with static key to make encoding scheme less obvious.
    //Map
    //------
    //DCD1..DnXBFFFFFFFF
    //Legend: DC=Data tier count (NOT encrypted)
    //        D1=First DataTier ID (NOT encrypted)
    //        Dn=Nth DataTier ID (0= PWLN 1=ISRN 2=Reserved for future)
    //        XB=XOR Byte
    //  FFFFFFFF=ENCRYPTED SessionID


    //get rid of the PREFIX on the front
    iDTcount := ReadStringByte(sHash, 1); //Read first two chars as hex pair
    sHash := copy(sHash, 3+(iDTCount*2), length(sHash)); //copy subtext

    //Get XOR Byte
    iXorByte := Dec32ToInt(copy(sHash, 1, 2));//strtoint('$'+copy(sHash, 1, 2));
                                                                               //Get the numbers
    sHexTemp := copy(sHash, 3,16);

    for t:= 1 to 8 do begin
      iByte := Dec32ToInt(copy(sHexTemp, (t*2)-1, 2));
      sContent := sContent+chr(iByte);
    end;

    //do the encryption
    for t:= 1 to 8 do begin
      //xor the key byte into the string
      sContent[t] := char( ord(sContent[t]) xor iXorByte);
      //xor the static key into the string
      sContent[t] := char( ord(sContent[t]) xor ord(KEY[t]));
      //xor the clientIP string in reverse order
(*      sContent[t] := chr( ord(sContent[t]) xor ord(sClientIP[(length(sClientIP)-t)+1]));*)
    end;

    result := Dec32ToInt(sContent);
  except
    raise exception.create(sHashIn + ' is not a valid ID');
  end;

end;


//------------------------------------------------------------------------------
function MorphURL(sURL: string):string;
//Auto-morphs the sessionid variable in a given URL
var
  sBefore, sAfter: string;
  iPos: integer;
  sSessionID: string;
  sLowercaseURL: string;
  sTemp: string;
begin
  //Convert the string to lowercase (preserve the original string)
  sLowerCaseURL := Lowercase(sURL);


  //find position of sessionid=
  iPos := pos('sessionid=', sLowerCaseURL);

  //leave if no session id
  if iPos < 1 then begin
    result := sURL;
    exit;
  end;

  inc(iPos, length('sessionid='));

  //copy out the BEFORE string
  sBefore := copy(sURL, 1, ipos-1);

  //copy out the AFTER string
  sAfter := copy(sURL, iPos, length(sURL));

  //Remove the sessionid parameter from the string by searching for the
  //first occurance on &
  iPos := pos('&', sAfter);
  sTemp := sAfter;
  if iPos>0 then begin
    sAfter := copy(sTemp, iPos, length(sTemp));
    sSessionID := copy(sTemp, 1, iPos-1);
  end
  else begin
    //Remove any bookmarks that might be on the end
    iPos := pos('#', sAfter);
    if iPos>0 then begin
      sSessionID := copy(sAfter, 1, iPos-1);
      sAfter := copy(sAfter, iPos, length(sURL));
    end else begin
      sSessionID := sAfter;
      sAfter := '';
    end;
  end;

  //Recalculate the sessionid
  sSessionID := MorphHash(sSessionID);

  result := sBefore+sSessionID+sAfter;

end;

function IsMac(rqInfo: TRequestInfo): boolean;
begin
  result :=  pos('mac', lowercase(rqInfo.request.UserAgent))>0;
end;

function Is9598(rqInfo: TRequestInfo): boolean;
begin
 result := (pos('98', lowercase(rqInfo.request.UserAgent))>0) or
    (pos('95', lowercase(rqInfo.request.UserAgent))>0) or
    (pos('Me', lowercase(rqInfo.request.UserAgent))>0) or
    (pos('Millenium', lowercase(rqInfo.request.UserAgent))>0);
end;

function IsNT(rqInfo: TRequestInfo): boolean;
begin
  result :=  pos('nt', lowercase(rqInfo.request.UserAgent))>0;
end;

function IsPC(rqInfo: TRequestInfo): boolean;
begin
  result :=  pos('win', lowercase(rqInfo.request.UserAgent))>0;
end;

function IsIE(rqInfo: TRequestInfo): boolean;
begin
  result :=  pos('msie', lowercase(rqInfo.request.UserAgent))>0;
end;

function IsNetscape(rqInfo: TRequestInfo): boolean;
begin
  result :=  pos('msie', lowercase(rqInfo.request.UserAgent))=0;

  if result then
    result := Not IsSafari(rqInfo);
end;

function IsSafari(rqInfo: TRequestInfo): boolean;overload;
begin
  result := pos('safari', lowercase(rqInfo.request.useragent))>0;

end;

function IsNetscape6(rqInfo: TRequestInfo): boolean;
begin
  result :=  pos('netscape', lowercase(rqInfo.request.UserAgent))>0;
end;


function GetSystemType(rqInfo: TRequestInfo): cardinal;
begin
  result := 0;

  if IsIE(rqInfo) then
    result := result or BT_IE;

  if IsNetscape(rqInfo) then
    result := result or BT_NETSCAPE;

  if IsMAC(rqInfo) then
    result := result or ST_MAC;

  if IsPC(rqInfo) then
    result := result or ST_PC;



end;

function GetHistoryMinusOne(sHistory: string): string;
var
  iIndex: integer;
begin

  iIndex := LastPos('_', sHistory);

  if iIndex > 0 then begin
    result := copy(sHistory, 1, iIndex-1);
  end
  else
    result := '';
end;
//------------------------------------------------------------------------------
function GetLastHistory(sHistory: string): string;
// returns the last entry in the history parameter
var
  iIndex: integer;
begin
  iIndex := LastPos('_', sHistory);

  if iIndex > 0 then begin
    result := copy(sHistory, iIndex+1, length(sHistory));
  end
  else
    result := sHistory;

end;

function GetHistoryIndex(iIndex: integer; sHistory: string): string;
var
  sLeft: string;
  t: integer;
begin
  //dec(iIndex);

  //split string iIndex times
  for t:= 0 to iIndex do begin
    //split the string
    SplitString(sHistory, '_', sLeft, sHistory);

    if t=iIndex then
      result := sLeft;
  end;

end;
//------------------------------------------------------------------------------
function TruncateHistory(iLevel: integer; sHistory: string): string;
var
  sLeft: string;
  t: integer;
begin
  result := '';
  for t:= 0 to iLevel do begin
    //split into left and right
    SplitString(sHistory, '_', sLeft, sHistory);

    //build result history
    if result = '' then
      result := sLeft
    else if sLeft <> '' then
      result := result+'_'+sLeft;
  end;

end;
//------------------------------------------------------------------------------
function GetHistoryCount(sHistory: string): integer;
var
  t: integer;
begin
  if length(sHistory) = 0 then begin
    result := 0;
    exit;
  end;

  result := 1;

  //count the number of '-'s found in the string to determine the length
  for t:= 1 to length(sHistory) do begin
    if sHistory[t] = '_' then begin
      inc(result);
    end;
  end;

end;

function IsInHistory(sHistory: string; iID: integer): boolean;
var
  t: integer;
begin
  result := false;
  for t:= 0 to GetHistoryCount(sHistory)-1 do begin
    if iID = strtoint(GetHistoryIndex(t, sHistory)) then begin
      result := true;
      break;
    end;
  end;


end;

function InsertHistory(sHistory: string; iInsertThisID, iAfterThisID: integer):string; overload;
begin
  result := InsertHistory(sHistory, inttostr(iInsertThisID), inttostr(iAfterThisID))
end;
function InsertHistory(sHistory: string; sInsertThisID, sAfterThisID: string):string;
var
  sID: string;
  iPos: integer;
begin
  sID := sAfterThisID;

  // if the FIRST thing in the history list
  if pos(sID, sHistory) = 1 then begin

    iPos := length(sID);
  end
  else begin
  //else
    sID := '_'+sID;
    //find position of parent
    iPos := pos(sID, sHistory);

    //bomb if parent not found
    if iPos<1 then begin
      result := sHistory;
      exit;
    end;

    inc(iPos, length(sID));
  end;

  //build the result
  result := copy(sHistory, 1, ipos-1)+'_'+sInsertThisID+copy(sHistory, iPos, length(sHistory));
end;

function EncodePSTPassword(sPassword: string): string;
var
  t: integer;
begin
  result := '';
  for t:= 1 to length(sPassword) do begin
    result := result + inttohex(ord(sPassword[t]), 2);
  end;

end;

//------------------------------------------------------------------------------
function GetIEVersion(rqInfo: TRequestInfo): string;
var
  iPos: integer;
  sTemp: string;
begin
  sTemp := lowercase(rqInfo.request.useragent);
  iPos := zpos('msie', sTemp);

  if iPos < 0 then
    exit('');

  //hack off the front
  sTemp := zcopy(sTemp, iPos, length(sTemp));

  //find ';' -- strip end off string
  iPos := pos(';', sTemp);
  if iPos >= 0 then begin
    sTemp := zcopy(sTemp, 1, iPos-1);
  end;

  //cut out the 'msie part'
  sTemp := zcopy(sTemp, 6, length(sTemp));

  result := sTemp;

end;
//------------------------------------------------------------------------------
function IsIESP2(rqInfo: TRequestInfo): boolean;
var
  sTemp : string;
begin
  if rqInfo.Request.HasParam('browser_version_minor') then begin
    sTemp := rqInfo.request['browser_version_minor'];
    sTemp := lowercase(sTemp);
  end else
    //default to sp2 if no browser version passed (fixing SABA issue with detection)
    sTemp := 'sp2';

  result := pos('sp2', sTemp)>0;

end;
//------------------------------------------------------------------------------
function IsIE6(rqInfo: TRequestInfo): boolean;
begin
  if not IsIE(rqInfo) then begin
    result := false;   //Netscape / Other browsers
  end else begin
    if GetIEVersion(rqInfo)<'5.5' then begin
      result := false; //Earlier IE
    end else
    if GetIEVersion(rqInfo)='5.5' then begin
      if IsIESP2(rqInfo) then
        result := true //IE5.5 Service Pack 2
      else
        result := false; //IE5.5 Service Pack 1 <
    end else
      result := true; //IE6
  end;
end;

function HashToDataTierIDs(sHash: string): TDataTierIDs;
var
  i,t: integer;
begin
    //Set the length of the result array to the first two digits

  i := GetDataTierCount(sHash);
  SetLength(result, i);
  //Read each data-tier ID and put in array
  for t:= 0 to i-1 do begin
    result[Low(result)+t] := ReadStringByteDec32(sHash, ((t+1)*2)+1)
  end;

end;

//------------------------------------------------------------------------------
function GetDataTierCount(sHash: string): integer;
begin
  result := ReadStringByte(sHash, 1);
end;

//------------------------------------------------------------------------------
function ReadStringByte(sString: string; iStartPos: integer): integer;
//Reads a HEXADECMAL ENCODED BYTE from a LITERAL string.
begin
  result := StrToInt('$'+copy(sString, iStartPos, 2));
end;

function ReadStringByteDec32(sString: string; iStartPos: integer): integer;
begin
  result := Dec32ToInt(copy(sString, iStartPos, 2))

end;

function AddURLParam(sURL, sParam: string): string;
//Adds a web page parameter to a URL appropriately choosing '&' or '?' as a delimiter.
var
  iPos: integer;
begin
  iPos := pos('?', sUrl);

  //use ? if no other params
  if iPos < 1 then begin
    result := sURL+'?'+sParam
  end
  //else use '&'
  else begin
    result := sURL+'&'+sParam;

  end;

end;
//------------------------------------------------------------------------------
function IsOSX(rqInfo: TrequestInfo): boolean;
var
  temp: boolean;
begin
  result :=  isMac(rqInfo);
  if result then begin
    result := ((pos('os x' , lowercase(rqInfo.Request['user-agent']))>0)
      or (rqInfo.request.hasparam('ua-os') and (pos('mac' , lowercase(rqInfo.request['ua-os']))>0)));
  end;

end;

function IsSpan(rqInfo: TRequestInfo): boolean;
begin
  result := IsSafari(rqINfo) or IsNetscape6(rqInfo) or (IsIE(rqInfo) and (NOT IsPc(rqInfo)));

end;
function IsDiv(rqInfo: TRequestInfo): boolean;
begin
  result := IsIE(rqInfo) and IsPC(rqInfo);

end;

function IsLayer(rqInfo: TRequestInfo): boolean;
begin
  result := (Not IsNetScape6(rqInfo)) and (not IsIE(rqInfo)) and (Not IsSafari(rqInfo));

end;

function IsMultiPart(rqInfo: TrequestInfo): boolean;
//<P>Returns whether the given request is encoded in Multi-Part form.
//Multi-part requests typically come in when the browser is uploading a file, such as an XML file, or a CSV Batch Enroll file.
begin
  if not rqInfo.request.hasParam('content-type') then begin
    result := false;
    exit;
  end
  else begin
    result := copy(lowercase(rqInfo.request['content-type']), 1, 9) = 'multipart';
  end;
end;

function GetMultiPartBoundary(rqInfo: TRequestInfo): string;
//<P>Scans the content type of the current request for a multipart boundary. This will raise an exception if the request is not a multi-part request.
var
  sTemp: string;
  iPos: integer;
begin
  result := '';

  sTemp := lowercase(rqInfo.request['content-type']);

  iPos := pos('boundary=', sTemp);

  if iPos < 1 then
    raise Exception.create('boundary not found in multipart message')
  else begin
    sTemp := copy(sTemp, iPos+length('boundary='), length(sTemp));

    //Make sure that sTemp doesn't end in a #13 or #10
    while (copy(sTemp, length(sTemp), 1) = #10) or (copy(sTemp, length(sTemp), 1) = #13) do begin
      sTemp := copy(sTemp, 1, length(sTemp)-1);
    end;

    result := '--'+sTemp;
  end;
end;

function GetBoundaryName(sBoundaryParams: string): string;
//Returns the "name" attribute from a multipart boundary header.
var
  iPos: integer;
  sTemp: string;
  sLeft, sRight: string;
begin
  sTemp := lowercase(sBoundaryParams);

  iPos := pos('name=', sTemp);

  if iPos= 0 then
    raise Exception.create('Boundary does not have a name.');

  sRight := copy(sBoundaryParams, iPos, length(sBoundaryParams));

  iPos := pos(';', sRight);

  if iPos>0 then begin
    sRight := copy(sRight, 1, iPos-1);
  end;

  //take the right half of the name=value
  SplitString(sRight, '=', sLeft, sRight);

  //take everything to the right of "
  SplitString(sRight, '"', sLeft, sRight);
  //split again, but now work with left
  SplitString(sRight, '"', sLeft, sRight);

  result := sLeft;


end;
procedure DecodeMultiPart(rqInfo: TRequestInfo);
//This decodes a message (typically a Form POST) that is sent with multi-part encoding.
//To decode a multipart message:
//<li>Extract the boundary identifier from the content-type parameter</li>
//<li>Start reading on the line following the boundary</li>
//<li>Extract information about the content disposition of the current part</li>
//<li>If the content disposition is form-data then extract the name of the form-data parameter and after dual CRLF include the content as the form variable value.</li>
//<li>If the content type is anything else, then extract it as the REAL content of the request</li>
var
  sBoundary: string;
  iLine: integer;
  sLine: string;
  sBoundaryParams: string;
  sl: TStringList;
begin
  sBoundary := GetMultipartBoundary(rQInfo);
  iLIne := 0;

  sl := TstringList.create;
  try
    sl.text := rqInfo.request.content;
    //sl.SaveToFile('d:\fixme.txt');
    repeat
      sLine := sl[iLIne];

      //if at the boundary then get the boundary params from the next line
      if sLine = sBoundary then begin
        Decodeboundary(rqInfo, GetBoundary(rqInfo, sBoundary, iLine), sBoundary);
      end;

      inc(iLIne);

    until
      (sLine = sBoundary+'--') or (iLine = sl.count-1);
  finally
    sl.free;
  end;
end;

function GetBoundary(rqInfo: TRequestInfo; sBoundaryTag: string; iStartingLine: integer; slAlternate: TStringList = nil): string;
//Returns a single part from a multipart request.  Typically needed for uploading files through web browsers.
var
  t: integer;
  sl: TStringList;
begin
  sl := TStringList.create;
  try

    if slAlternate=nil then
      sl.text := rqInfo.request.content
    else
      sl.text := slAlternate.text;

    t:= iStartingLine;
    result := sl[t];
    inc(t);
    while (t<sl.count) and (NOT (copy(sl[t], 1, length(sBoundaryTag)) = sBoundaryTag))  do begin
      result := result+#13#10+sl[t];
      inc(t);
    end;

  finally
    sl.free;
  end;
end;

procedure DecodeBoundary(rqInfo: TRequestInfo; sBoundary: string; sBoundaryTag: string);
var
  sl: TStringList;
  iLine: integer;
  sBoundaryPArams, sBoundaryName: string;
begin
  iLIne := 1;
  sl := TStringList.create;
  try
    sl.text := sBoundary;
    sBoundaryParams := sl[1];

    //Get the name parameter from the boundary params.
    sBoundaryName := GetBoundaryName(sBoundaryParams);

    //search for the end of the boundary header
    repeat
      inc(iLine);
      if iLine>=sl.count then
        raise Exception.create('Illegal boundary format!');
    until
      (sl[iLIne] = '');

    //if there is content, then add the content as a parameter
    if iLIne <sl.count-1 then begin
      rqInfo.request.addParam(sboundaryName, GetBoundary(rQInfo, sBoundaryTag, iLine+1, sl), pcContent);
    end;

 finally
    sl.free;
  end;

end;

function GetBoundaryDisposition(sBoundaryLIne: string): string;
var
  sLeft, sRight: string;
begin
  raise Exception.create('unimplmemented');


end;

function SplitCSV(sString: string; sDelimiter: string = ','): TStringLIst;
var
  t,u: integer;
  bInQuotes: boolean;
  s: string;
begin
  s := sString;
  result := TStringList.create;
  u := 0;
  bInQuotes := false;
  t:= 1;
  result.add('');

  while t<=length(sString) do begin
    //if a delimiter, then new line in stringlist
    if (NOT bInQuotes) and (copy(sString, t, length(sDelimiter)) = sDelimiter) then begin
      result.Add('');
      inc(u);
      inc(t, length(sDelimiter));
    end
    //if we hit a " then toggle whether or not we're in quotes
    //quotes are NOT outputted to the stringlist
    else if s[t] = '"' then begin
      bInquotes := not bInQuotes;
      inc(t);
    end
    //else just output to the string list
    else begin
      result[u] := result[u]+s[t];
      inc(t);
    end;
  end;
end;

function IsMac(sUA: string): boolean;
begin
  result :=  pos('mac', lowercase(sUA))>0;
end;

function Is9598(sUA: string): boolean;
begin
 result := (pos('98', sUA)>0) or
    (pos('95', lowercase(sUA))>0) or
    (pos('Me', lowercase(sUA))>0) or
    (pos('Millenium', lowercase(sUA))>0);
end;

function IsNT(sUA: string): boolean;
begin
  result :=  pos('nt', lowercase(sUA))>0;
end;

function IsPC(sUA: string): boolean;
begin
  result :=  pos('win', lowercase(sUA))>0;
end;

function IsIE(sUA: string): boolean;
begin
  result :=  pos('msie', lowercase(sUA))>0;
end;

function IsNetscape(sUA: string): boolean;
begin
  result :=  ((pos('msie', lowercase(sUA))=0)
         and (pos('mozilla', lowercase(sUA))>0));
end;

function IsNetscape6(sUA: string): boolean;
begin
  result :=  pos('netscape', lowercase(sUA))>0;
end;

function IsOSX(sUA: string): boolean;
var
  temp: boolean;
begin
  result :=  isMac(sUA);
  if result then begin
    result := (pos('os x' , lowercase(sUA))>0);
//      or (rqInfo.request.hasparam('ua-os') and (pos('mac' , lowercase(rqInfo.request['ua-os']))>0)));
  end;

end;




(*function GetStringListArea(sl: TStringList; iStartCol, iEndCol: integer; iStartRow, iEndRow: integer): string;
var
  t: integer;
  begin
  result := '';

  for t:= iStartRow to iEndRow do begin
    if iStartRow = iEndRow then begin
      result := copy(sl[t], iStartCol, (iEndCol-iStartCol));
    end else
    if t = iStartRow then begin
      result := result + copy(sl[t], 1, iEndCol-1);
    end else
    if t = iEndRow then begin
      result := result+copy(sl[t],1,iEndCol-1);
    end else begin
      result := result + sl[t];
    end;
  end;
end;


function IsSafari(rqInfo: TRequestINfo): boolean; overload;
begin
  result := pos('safari', lowercase(rqInfo.request.useragent))>0;

end;*)




end.
