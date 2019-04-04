unit WebString;
// functions for parsing ANY HTTPString that claims to be an HTTP request or response
// this functions aren't terribly efficient, making use of brute-force
// HTTPString-parsing techniques, but they'll do for about any purpose, and
// THEY WORK... which should take priority over working fast.

interface

uses classes, systemx, sysutils, namevaluepair, stringx.ansi, debug;

type
  HTTPException = class(Exception);
{$ifdef MSWINDOWS}
  HTTPString = AnsiString;
  HTTPChar = AnsiChar;
  PHTTPChar = ^AnsiChar;
  HTTPContentTextString = UTF8String;
  HTTPContentTextChar = AnsiChar;
  PHTTPContentTextChar = PAnsiChar;
{$ELSE}
  HTTPContentTextString = string;
  HTTPContentTextChar = Char;
  PHTTPContentTextChar = Char;
  HTTPString = String;
  HTTPChar = Char;
  PHTTPChar = ^Char;
{$ENDIF}



function DecodeURLParams(sURL: string; out sDocument: string): TNameValuePairList;
{$IFDEF WINDOWS}
function DecodeURLParams(sURL: HTTPString; out sDocument: string): TNameValuePairList;overload;
{$ENDIF}
function ApplyRelativeURL(sBaseURL: HTTPString; sURL: HTTPString): HTTPString;
function RemoveComments(sHTML: HTTPString): HTTPString;
function RepairLinuxHTTPString(sLinuxHTTPString: HTTPString): HTTPString;
function ExtractURLPAth(sURL: HTTPString): HTTPString;
function ExtractURLFileNAme(sURL: string): string;
function ExtractLinks(sBasePath: HTTPString; sHTML: HTTPString): HTTPString;
function AmpEncode(s: HTTPString): HTTPString;
procedure SplitAccount(sAccount: HTTPString; out iDataCenter: integer; out sAccountID: HTTPString);
function RandomHTTPString(iLength: integer): HTTPString;
function Flashify(sl: TStringList) : HTTPString;
function AdjustWebPath(sPath: HTTPString): HTTPString;
function AdjustURLForParams(sPath: HTTPString): HTTPString;

function StaticHashToInt(sHash: HTTPString): integer;
function IntToStaticHash(iNumber: integer): HTTPString; overload;
function CRToBR(s: HTTPString): HTTPString;
function SimpleEncrypt(s: HTTPString): HTTPString;
function SimpleDecrypt(s: HTTPString): HTTPString;


function SlashDot(sHTTPStringWithSlashes: HTTPString): HTTPString;
function Simplify(i: integer): HTTPString;
  //input number "44,321"... output 44.3K
  //input number "1,234,567... output 1.23M
  //input number "1,234,567,890"... ouptut 1.23G
procedure BlankString(var sHTTPString: HTTPString; sBlankChar: HTTPChar; iStartAt, iEndAt: integer);
function HackContentLength(sHeader: HTTPString): integer;
function DecodeInlineURL(sURL: HTTPString): HTTPString;
function EncodeInlineURL(sURL: HTTPString): HTTPString;
function StrToBool(s: HTTPString): boolean;
function BoolToSTr(b: boolean): HTTPString;
function ClipLeft(s, sBeforeAndIncluding: HTTPString): HTTPString;
function ClipRight(s, sStartingWith: HTTPString): HTTPString;
function GetHeaderParam(sHead, sParamName: HTTPString; iInstance: integer = 1): HTTPString;
function ChangeHeaderParam(sHead, sParamName, sNewValue: HTTPString): HTTPString;
function AddHeaderParam(sHead, sParamName, sValue: HTTPString ): HTTPString;
procedure MergeHeaderAndFooter(var sHeader: HTTPString; sFooter: HTTPString);
function GetCommand(sHead: HTTPString): HTTPString;
function GetDocument(sHead: HTTPString): HTTPString;

function GetResultMessage(sHead: HTTPString): HTTPString;

function GetResultCode(sHead: HTTPString): HTTPString;
function ChangeDocument(sHead, sNewDocument: HTTPString): HTTPString;
function IsHeaderComplete(sFullRequest: HTTPString): boolean;
function IsFooterComplete(sFullRequest: HTTPString): boolean;
function ExtractHeader(sFullRequest: HTTPString): HTTPString;
function ExtractContent(sFullRequest: HTTPString): HTTPString;
function EncodeXMLString(s: HTTPString; sEscapeChar:HTTPchar = '%'): HTTPString; overload;
function IsValidURL(sURL: string): boolean;
function EncodeTAGString(s: HTTPString; sEscapeChar:HTTPChar = '%'): HTTPString; overload;
function EncodeWebString(s: HTTPString; sEscapeChar:HTTPChar = '%'): HTTPString; overload;
function DecodeWebString(sOriginal: HTTPString; sEscapeChar: HTTPString = '%'): HTTPString; overload;
function XMLStringToPlainText(sOriginal:  HTTPString; sEscapeChar:HTTPChar = '%'): HTTPString;

procedure Decodeurl(sUrl: HTTPString; out sHostAndPort, sDocument: HTTPString);
function IsChunkComplete(sChunkedContent: HTTPString): boolean;
function GetChunkLength(sChunkedContentAtChunkStart: HTTPString): integer;
procedure DecodeChunk(var sChunkToChunkBody: HTTPString; out sRemainder: HTTPString);
function DigitToInteger(c: HTTPChar):integer;
function HexToInt(sHexHTTPString: HTTPString): integer;
//function SplitString(sSource, sSplitter: HTTPString; var sLeft, sRight: HTTPString; bStartAtRight: boolean = false): boolean; overload;
function ExtractWebPath(sPath: HTTPString): HTTPString;

function MakeSecureURL(sURL: HTTPString): HTTPString;
function FindLinkInHTML(sBaseURL: HTTPString; sHTML: HTTPString; sLinkText: HTTPString; iInstance: integer=0): HTTPString;
function FindIFrameInHTML(sBaseURL: HTTPString; sHTML: HTTPString; iInstanceOneBased: integer; out sSrc: HTTPString): boolean;
function FindTagAttributeInHTML(sBaseURL: HTTPString; sHTML: HTTPString; sTag: HTTPString; sAttribute: HTTPString; iInstanceOneBased: integer; out sSrc: HTTPString): boolean;
function StringListToHTML(sl: TStringList): string;



implementation

uses math, stringx;
//------------------------------------------------------------------------------
function GetHeaderParam(sHead, sParamName: HTTPString; iInstance: integer = 1): HTTPString;
var
  sJunk, sLeft, sRight: HTTPString;
  t: integer;
begin
  sParamName := sParamName + ':';
  result := '';
  if SplitStringNoCase(sHead, sParamName, sLeft, sRight) then begin
    for t:= 2 to iInstance do begin
      SplitString(sRight, sParamName, sLeft, sRight)
    end;

    if Not SplitString(sRight, #13#10, result, sJunk) then
      if Not SplitString(sRight, #13, result, sJunk) then
        SplitString(sRight, #10, result, sJunk);

    if copy(result, 1,1) = ' ' then
      result := copy(result, 2,length(result));

//    result := TrimStr(result);
//      result := TrimHTTPString(result);



  end;

end;

function GetHeaderParamXX(sHead, sParamName: HTTPString; iInstance: integer = 1): HTTPString;
//Extracts a parameter from the header of an http request/response.
//Use a pre-separated header (no content attached).
var
  sCUT: HTTPString;
  iPos, iPos2: integer;
  sOriginalHead: HTTPString;
begin
  //Convert params to uppercase to ensure case insensitivity
  sParamName := uppercase(sParamName);
  sOriginalHead := sHead;
  sHead := uppercase(sHead);

  //Find position of paramname after linefeed (ensure left-most column)
  iPos := pos(#10+sParamName+':', sHead);
  //If found then...
  if iPos > 0 then begin
    //Caluculate the starting position of the param's VALUE
    iPos := iPos + 1 {#10} + length(sParamName)+ 1 {:};
    //Remove everything BEFORE the value
    sCUT := copy(sOriginalHead, iPos, length(sHead));
    //Search to the next CRLF following the value
    iPos2 := pos(#13#10, sCUT);
    //Remove everything after and INCLUDING the CRLF at the end of the value
    sCUT := copy(sCUT, 1, iPos2-1);
    result := sCut;
    //Make sure there are no spaces on either side of the result
    TrimStr(result);
  end else begin
    //If param with given name was not found in header... return blank
    result := '';
  end;

end;
//------------------------------------------------------------------------------
function ChangeHeaderParam(sHead, sParamName, sNewValue: HTTPString): HTTPString;

// sHead = the entire HTTP header (no content)
// sParamName = the name of the parameter (do not include colon (:))
// sNewVAlue = the NewValue
// result = the changed header;
var
  sCUT: HTTPString;
  iPos, iPos2: integer;
  sOriginalHead: HTTPString;
begin
  //Save original header (so that CaSe is restored at end of function)
  sOriginalHead:= sHead;
  //Convert params to uppercase to ensure case insensitivity
  sParamName := uppercase(sParamName);
  sHead := uppercase(sHead);

  //Find position of paramname after linefeed (ensure left-most column)
  iPos := pos(#10+sParamName+':', sHead);
  //If found then...

  if iPos > 0 then begin
    //Caluculate the starting position of the param's VALUE
   iPos := iPos + 1 {#10} + length(sParamName)+ 1 {:};
    //Remove everything BEFORE the value int TEMPORARY HTTPString
    //this is needed so that we can search for the ending CRLF
    sCUT := copy(sHead, iPos, length(sHead));
    //Search to the next CRLF following the value
    iPos2 := pos(#13#10, sCUT);

    //Rebuild the header with the new value in place
    result := copy(sOriginalHead, 1, iPos-1)+' '+
              sNewValue+
              copy(sOriginalHead, iPos+iPos2-1, length(sOriginalHead));
  end else begin
    //If param with given name was not found in header... add the parameter
    result := sOriginalHead;
  end;


end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function AddHeaderParam(sHead, sParamName, sValue: HTTPString ): HTTPString;
//Adds a header parameter to an HTTP header
//  sHead = the header
//  sParamName = the name of the parameter
//  sValue = the value of the parameter
//Note: this function does not check to see if the parameter already exists
//use changeHeaderparam if you are unsure if the parameter exists... as it will
//add the parameter if it doesn't exist
var
  iPos: integer;
begin
  //Find CRLF+CRLF
  iPos := pos(#13#10#13#10, sHead);
  //Add header parameter inbetween the two CRLF's
  if iPos > 0 then begin
    result := copy(sHead, 1, iPos+1)+
              sParamName+': '+sValue+#13#10#13#10;
  end else begin
    raise HTTPException.create('Invalid HTTP Header in AddHeaderParam: '+sHead);
  end;

end;
//------------------------------------------------------------------------------
function GetCommand(sHead: HTTPString): HTTPString;
//Returns the command/action/method from an HTML header e.g. GET, POST, PUT
var
  iPos: integer;
begin
  //find first space in header
  iPos := pos(' ', sHead);
  //extract everything up to the first space
  result := copy(sHead, 1, iPos-1);

end;

//------------------------------------------------------------------------------
function GetDocument(sHead: HTTPString): HTTPString;
//Returns the document being requested from the first line of the HTTP header
var
  iPos: integer;
begin
  //find first space in header
  iPos := pos(' ', sHead);
  //extract everything AFTER the first space
  sHead := copy(sHead, iPos+1, length(sHead));
  //find SECOND space in header
  iPos := pos(' ', sHead);
  //extract everything up to the SECOND space
  result := copy(sHead, 1, iPos-1);


end;
//------------------------------------------------------------------------------
function ChangeDocument(sHead, sNewDocument: HTTPString): HTTPString;
//Changes the document being requested in the HTTP header (useful for proxies)
var
  iPos, iPos2: integer;
  sOriginalHead : HTTPString;
begin
  sOriginalHead := sHead;

  //find first space in header
  iPos := pos(' ', sHead);
  //extract everything AFTER the first space
  sHead := copy(sHead, iPos+1, length(sHead));
  //find SECOND space in header
  iPos2 := pos(' ', sHead);
  //rebuild header with new document
  result := copy(sOriginalHead, 1, iPos-1)+' '+
            sNewDocument+' '+
            copy(sOriginalHead, iPos2+iPos, length(sHead)+length(sNewdocument));
end;
//------------------------------------------------------------------------------
function ExtractHeader(sFullRequest: HTTPString): HTTPString;
//Pulls the Header part of an HTTP request or response from the entire
//Request/reponse
var
  iPos: integer;
  iPos2: integer;
  iDelimiterLength: integer;
begin
  //find first CRLF+CRLF  in header
  iPos := pos(#13#10#13#10, sFullRequest);
  iPos2 := pos(#10#10, sFullRequest);
  if (iPos2<iPos) and (iPos2>0) then
    iPos := iPos2;

  if (iPos2>0) and (iPos = 0) then
    iPos := iPos2;

  //account for different delimiter lengths
  if iPos = iPos2 then
    iDelimiterLength := 2
  else
    iDelimiterLength := 4;


  if iPos > 0 then begin
    //extract everything up to the first occurance
    result := copy(sFullRequest, 1, iPos+iDelimiterLength-1); //<<--Note: Takes 2 of 2 CRLFs
  end else
    result := '';
end;
function ExtractContent(sFullRequest: HTTPString): HTTPString;
//Pulls the content from the entire HTTP Request/Response
var
  iPos: integer;
  iPos2: integer;
  iDelimiterLength: integer;
begin
  //find first CRLF+CRLF  in header
  iPos := pos(#13#10#13#10, sFullRequest);
  iPos2 := pos(#10#10, sFullRequest);
  if (iPos2<iPos) and (iPos2>0) then
    iPos := iPos2;

  if (iPos2>0) and (iPos = 0) then
    iPos := iPos2;

  //account for different delimiter lengths
  if iPos = iPos2 then
    iDelimiterLength := 2
  else
    iDelimiterLength := 4;



  if iPos > 0 then begin
    //extract everything AFTER the first occurance
    result := copy(sFullRequest, iPos+iDelimiterLength, length(sFullRequest));
  end else
    result := '';

end;
//------------------------------------------------------------------------------
function IsChunkComplete(sChunkedContent: HTTPString): boolean;
begin
  result := (pos(#13#10, sChunkedContent)>0);
end;

function IsHeaderComplete(sFullRequest: HTTPString): boolean;
//Determines a partially-recieved request has returned the entire
//header or not
begin
  result := (pos(#13#10#13#10, sFullRequest)>0) or (pos(#10#10, sFullRequest)>0);



end;                    function IsFooterComplete(sFullRequest: HTTPString): boolean;
//Determines a partially-recieved request has returned the entire
//header or not
begin
  result := (pos(#13#10#13#10, sFullRequest)>0);


end;
//------------------------------------------------------------------------------
function GetResultCode(sHead: HTTPString): HTTPString;
//Returns the result code returned from an HTTP response header
var
  iPos: integer;
begin
  //find first space in header
  iPos := pos(' ', sHead);
  //extract everything AFTER the first space
  sHead := copy(sHead, iPos+1, length(sHead));
  //find the end of the first line
  iPos := pos(#13, sHead);
  result := copy(sHead, 1, iPos-1);

  //find SECOND space in header
  iPos := pos(' ', result);
  //extract everything up to the SECOND space
  if iPos > 0 then
    result := copy(result, 1, iPos-1);

end;

function GetResultMessage(sHead: HTTPString): HTTPString;
//Returns the result code returned from an HTTP response header
var
  iPos: integer;
begin
  //find first space in header
  iPos := pos(' ', sHead);
  //extract everything AFTER the first space
  sHead := copy(sHead, iPos+1, length(sHead));
  //find the end of the first line
  iPos := pos(#13, sHead);
  result := copy(sHead, 1, iPos-1);

  //find SECOND space in header
  iPos := pos(' ', result);
  //extract everything after the second space
  if iPos > 0 then
    result := copy(result, iPos+1, length(result));


end;



//------------------------------------------------------------------------------
function AmpEncode(s: HTTPString): HTTPString;
//a: Jason Nelson
//Ampersand Encodes a HTTPString.  This is most notably needed if you wish
//to display characters such as "<" and ">" in an HTML page.
var
  sEscapeChar: HTTPString;
  t: integer;
  u: integer;
  sNum: HTTPString;
begin
  u := 1;
//  sEscapeChar := '&#';
//  s := StringReplace(s, '[[[', '&#091;&#091;&#091;', [rfReplaceAll]);

  setlength(result, length(s)*3+1);



  //cycle through the HTTPString
  for t:= 1 to length(s) do begin
    //if  the character is anything other than a standard letter or number
    if (s[t]='[') or (ord(s[t])<48) or (ord(s[t])>122) or ((ord(s[t])>57) and (ord(s[t])<65))then begin
      if (NOT (charinset(s[t], [' ', '?', '[',']','!','.',',',';','(',')', '%'])) or (charinset(s[t],['[']) and (t<length(s)) and charinset(s[t+1], ['[']))) then begin
        //encode the character as a special hex job
        if ord(s[t]) <10 then
          sNum := '00'+inttostr(ord(s[t]))
        else if ord(s[t]) <100 then
          sNum := '0'+inttostr(ord(s[t]))
        else
          sNum := inttostr(ord(s[t]));

        result[u] := '&';
        result[u+1] := '#';
        result[u+2] := sNum[1];
        result[u+3] := sNum[2];
        result[u+4] := sNum[3];
        result[u+5] := ';';
        inc(u, 6);

      end else begin
        result[u] := s[t];
        inc(u);
      end;
    end else begin
      result[u] := s[t];
      inc(u);
    end;
  end;
  //tag a 0 on the end

  result := copy(result, 1, u-1);


end;


//------------------------------------------------------------------------------
function EncodeWebString(s: HTTPString; sEscapeChar: HTTPChar): HTTPString;
//Encodes a HTTPString so that special characters in it do not disrupt
//HTTP headers.  Also known as "URL" Encoding or "escape" encoding.
var
  t: integer;
  u: integer;
  sHex: HTTPString;
begin
  u := 1;
  setlength(result, length(s)*3+1);

  //cycle through the HTTPString
  for t:= 1 to length(s) do begin
    //if  the character is anything other than a standard letter or number
    if ((ord(s[t])<48) or (ord(s[t])>122) or ((ord(s[t])>57) and (ord(s[t])<65)))
    and (not charinset(s[t],['-','.','_','~']))
    then begin
      //encode the character as a special hex job
      sHex := inttohex(ord(s[t]), 2);
      result[u] := sEscapeChar;
      result[u+1] := sHex[1];
      result[u+2] := sHex[2];
      inc(u, 3);
    end else begin
      result[u] := s[t];
      inc(u);
    end;
  end;
  //tag a 0 on the end

  result := ocopy(result, 1, u-1);

  if (result = '') and not (sEscapeChar='%') then
    result := sEscapeChar+'02';


end;
//------------------------------------------------------------------------------
function DecodeWebString(sOriginal: HTTPString; sEscapeChar: HTTPString): HTTPString;
//Decodes a HTTPString that was URL encoded.  This function is the opposite of EncodeWebString.
// parses a HTTPString and replaces '+' with ' ' and '%nn' with the
// appropriate ASCII value

var
  sTemp: HTTPString;
  nTemp: integer;
  c : HTTPChar;
  t: integer;
begin
  if (sOriginal = (sEscapeChar+'02')) and not (sEscapeChar='%') then begin

    result := '';
    exit;
  end;

  // Get rid of the '+'s...
  sOriginal := StringReplace(sOriginal, '+', ' ', [rfReplaceAll]);

  result := '';
  t:= 1;
  while t <= length(sOriginal) do begin
    c := sOriginal [t];

    IF not (c = sEscapeChar) then BEGIN
      result := result + c;
      inc(t);
    END
    ELSE BEGIN
      //get the next two characters in the HTTPString
      sTemp := '$' + copy(sOriginal, t+1, 2);

      //if not two characters
      if length(sTemp)<3 then
        break;

      if (charinset(sTemp[2],['0'..'9', 'A'..'F', 'a'..'f']) and
          charinset(sTemp[3],['0'..'9', 'A'..'F', 'a'..'f'])) then begin

        //Get the ordinal value represented by the HTTPString
        nTemp := strtoint(sTemp);

        //Add the resulting character to the result
        result := result + HTTPChar(nTemp);

        inc(t,3);
      end
      else begin
        break;
      end;

    END;
  end;
end;
//------------------------------------------------------------------------------
procedure Decodeurl(sUrl: HTTPString; out sHostAndPort, sDocument: HTTPString);
//Pulls a URL apart into Host and port and Document portions.
//automatically sets port to 8080 if https is specified
//should accept null documents as well as missing http:// in url
var
  sUCaseURL: HTTPString;
  sPort: HTTPString;
  iHostStart: integer;
  iPos: integer;
  bSecure: boolean;
begin
  sUCaseURL := lowercase(sURL);

  //if it doesn't start with http then slap http:// on it
  if (copy(sUCaseURL, 1, 7) <> 'http://') and (copy(sUCaseURL, 1, 8) <> 'https://') then begin
    sUCASEURL := 'http://'+sUCaseURL;
    sURL := 'http://'+sURL;
  end;


  //if it starts with https:// then default port to 8080 else 80
  if copy(sUCaseURL, 1, 8) = 'https://' then begin
    bSecure := true;
    sPort := '8080';
    iHostStart := 9;
  end
  else begin
    bSecure := false;
    sPort := '80';
    iHostStart := 8;
  end;

  //mirror
  sUCaseURL := copy(sUCaseURL, iHostStart, length(sUCaseURL));
  sURL := copy(sURL, iHostStart, length(sURL));

  iPos := pos('/', sURL);
  //if there is no slash then
  if iPos <1 then begin
    //host and port are just the entire URL
    sHostAndPort := sURL;
    sDocument := '/';
  end
  else begin
    sHostAndPort := copy(sURL, 1, iPos-1);
    sDocument := copy(sURL, iPos, length(sURL));

  end;

  //if secure with no port specified... tag 8080 on the end
  if bSecure and (pos(':', sHostAndPort) < 1) then begin
    sHostAndPort := sHostAndPort+':8080';
  end;

end;
//------------------------------------------------------------------------------
function GetChunkLength(sChunkedContentAtChunkStart: HTTPString): integer;
//Returns the length of the chunk in an HTTP reponse with Chunked transfer-encoding.
//Be sure to pass only content in at the start of the current chunk in the content.
var
  iPos: integer;
  sTemp: HTTPString;
  sHex: HTTPString;
  sMsg: HTTPString;
begin
  //look for crlf
  iPos:= pos(#13#10, sChunkedContentAtChunkStart);

  //if crlf not found...  we have a serious problem
  if iPos < 1 then
    raise HTTPException.create('Error decoding chunked transfer-encoded response');

  //hack out the line from the Chunk
  sTemp := copy(sChunkedContentAtChunkStart, 1, iPos-1);

  //now look for a semicolon
  iPos := pos(';', sTemp);

  //if semicolon was found then remove everything after the semicolon
  if iPos > 0 then
    sTemp := copy(sTemp, 1, iPos-1);

  //Result is the hexadecimal value of the HTTPString
  try
    result := Hextoint(sTemp);

  except
    sMsg := exception(ExceptObject).Message;
    raise HTTPException.create('illegal chunk lenght value was: '+sHex+' -- '+sMsg)
  end;
end;
//------------------------------------------------------------------------------
procedure DecodeChunk(var sChunkToChunkBody: HTTPString; out sRemainder: HTTPString);
//Handles a chunk in a chunk-encoded body.
var
  iLength: integer;
  iPos: integer;
  sChunkedContent: HTTPString;
begin
  sChunkedContent := sChunkToChunkBody;
  //Get the length of the chunked content
  iLength := GetChunkLength(sChunkedContent);

  //Hack out the chunked header
  iPos := pos(#13#10, sChunkedContent);
  if iPos < 1 then
    raise HTTPException.create('Invalid encoding found while decoding chunked transfer-encoded response')
  else begin
    sChunkedContent := copy(sChunkedContent,iPos+2, length(sChunkedContent));
  end;
  if iLength > 0 then begin
    //Separate the Chunk body from the remainder
    sChunkToChunkBody := copy(sChunkedContent, 1, iLength);
    sRemainder := copy(sChunkedContent, iLength+3, length(sChunkedContent));
  end else begin
    //if the chunk length is zero.... the result is the footer (which may be
    //partial) so everything is treated as the remainder;
    //the downloading algorythm will know this and change to look for the end
    //of the file differently
    sChunkToChunkBody := ''; //this chunk should not be part of the content!
    sRemainDer := sChunkedContent;
  end;

end;
//------------------------------------------------------------------------------
procedure MergeHeaderAndFooter(var sHeader: HTTPString; sFooter: HTTPString);
//Merges a header with a footer (from a chunked transfer-encoded content)
begin
  sHeader := copy(sHeader, 1, length(sHeader)-2)+sFooter;
end;
//------------------------------------------------------------------------------
function HexToInt(sHexHTTPString: HTTPString): integer;
//Converts a HTTPString that represents a hexadecimal number into the actual number.
var
  iNumDigits: integer;
  cDigit: HTTPChar;
  t : integer;
begin
  TrimStr(sHexHTTPString);
  result := 0;
  iNumdigits := length(sHexHTTPString);

  for t:=1 to iNumDigits do begin
    cDigit := sHexHTTPString[t];
    result := result + ((DigitToInteger(cDigit) shl ((iNumdigits-t) * 4)));
  end;

end;
//------------------------------------------------------------------------------
function DigitToInteger(c: HTTPChar):integer;
//Converts a single hexadecimal digit into its integer value.
var
  s: HTTPString;
begin
  s := lowercase(c);
  c := s[1];

    case c of
    '0': result := 0;
    '1': result := 1;
    '2': result := 2;
    '3': result := 3;
    '4': result := 4;
    '5': result := 5;
    '6': result := 6;
    '7': result := 7;
    '8': result := 8;
    '9': result := 9;
    'a': result := 10;
    'b': result := 11;
    'c': result := 12;
    'd': result := 13;
    'e': result := 14;
    'f': result := 15;
  else
    raise HTTPException.create(c+' is not a valid haxadecimal digit');
  end;

end;
//------------------------------------------------------------------------------
function ClipLeft(s, sBeforeAndIncluding: HTTPString): HTTPString;
//Returns the text AFTER the pattern given in sBeforreAndIncluding parameter.
//p: s: The master HTTPString
//p: sBeforeAndIncluding: the pattern to find in s
var
  iPos: integer;
begin
  iPos := pos(s, sBeforeAndIncluding);
  if iPos= 0 then begin
    result := '';
    exit;
  end;

  result := copy(s, iPos+length(sBeforeAndIncluding), length(s));

end;
//------------------------------------------------------------------------------
function ClipRight(s, sStartingWith: HTTPString): HTTPString;
//Returns text from 's' up to 'sStartingWith'.
//p: s: the Master HTTPString.
//p: sStartingWith: the pattern which will be clipped out (along with everything to the right of it)

var
  iPos: integer;
begin
  iPos := pos(s, sStartingWith);
  if iPos= 0 then begin
    result := '';
    exit;
  end;

  result := copy(s, 1, iPos-1);
end;
//------------------------------------------------------------------------------
function LastPos(sub,s : HTTPString):integer;
//Returns the LAST position of a subHTTPString in a HTTPString
//p: sub: the subHTTPString
//p: s: the master HTTPString
label 1;
var
  p: integer;
  t: integer;
begin
  p:=0;

  for t:=((length(s))-length(sub))+1 downto 0 do begin
    if copy(s, t, length(sub))=sub then begin
      p:=t;
      goto 1;
    end;
  end;

1: result:=p;
end;
//------------------------------------------------------------------------------
(*function SplitString(sSource, sSplitter: HTTPString; var sLeft, sRight: HTTPString; bStartAtRight: boolean): boolean; overload;
//Splits a HTTPString into sleft and sRight at sSplitter.  If sSplitter is not found
//then the entire HTTPString is in sLeft and sRight is blank, unless bStartAtRight is TRUE.
//returns true if split.... else false
//p: sSource: The master HTTPString
//p: sSplitter: The pattern that triggers the split
//p: var sLeft: Reference to the HTTPString that will contain the LEFT half of the split
//p: var sRight: Reference to the HTTPString that will contain the RIGHT half of the split
//p: bStartAtRight: Default is FALSE, boolean indication whether to start at the right half of the HTTPString instead of the left
var
  iPos : integer;
begin
  //find position of splitter
  if bStartAtRight then
    iPos:= lastpos(sSplitter, sSource)
  else
    iPos := pos(sSplitter, sSource);

  //if splitter not found
  if iPos<1 then begin
    //entire source goes to left
    sLeft := sSource;
    sRight := '';
    result := false;
  end
  else begin
    //otherwise Slice and dice at the splitter
    sLeft := copy(sSource, 1, iPos-1);
    sRight := copy(sSource, iPos+length(sSplitter), length(sSource));
    result := true;
  end;
end;*)

function StrToBool(s: HTTPString): boolean;
//Converts a HTTPString to a boolean
begin
  s := lowercase(s);

  if (s = 'true') or (s='yes') or (s='on') then
    result := true
  else
    result := false;
end;


function BoolToSTr(b: boolean): HTTPString;
begin
  if b then
    result := 'TRUE'
  else
    result := 'FALSE'
end;

//------------------------------------------------------------------------------
function EncodeInlineURL(sURL: HTTPString): HTTPString;
//Do not use this. Experimental URL encoding.
var
  t: integer;
begin
  SetLength(result, length(sURL));
  //replace ? with ^
  //replace & with *
  for t:=0 to length(sURL) do begin
    case sURL[t] of
      '?': result[t] := '^';
      '&': result[t] := '*';
    else
      result[t] := sURL[t];
    end;
  end;
end;

//------------------------------------------------------------------------------
function DecodeInlineURL(sURL: HTTPString): HTTPString;
//Do not use this.  Experimental URL encoding.
var
  t: integer;
begin
  SetLength(result, length(sURL));
  //replace ? with ^
  for t:=1 to length(sURL) do begin
    case sURL[t] of
      '^': result[t] := '?';
      '*': result[t] := '&';
    else
      result[t] := sURL[t];
    end;
  end;
end;

//------------------------------------------------------------------------------
function HackContentLength(sHeader: HTTPString): integer;
//Given a raw chunk of HTTP header data, attempts to determine the content-length from the HTTP header.
//This is needed because the HTTP protocol is poorly designed.
var
  sl: TStringList;
  t: integer;
  iLineLength: integer;
  sTemp: HTTPString;
const
  s = 'Content-Length:';
begin
  sl := TStringList.create;
  result := 0;
  try
    //assign the input to a HTTPString list for easy delimiting of CRLF
    sl.Text := sHeader;

    //search each line until we find a "Content-Length:"
    for t:= 0 to sl.count-1 do begin
      iLineLength := length(sl[t]);
      if lowercase(copy(sl[t], 1, length(s))) = lowercase(s) then begin
        sTemp := copy(sl[t], length(s)+1, iLineLength);
        result := strtoInt(sTemp);
      end;
    end;
  finally
    sl.free;
  end;
end;
//------------------------------------------------------------------------------
function Simplify(i: integer): HTTPString;
//returns a digestible HTTPString for a large number.
//e.g. 12435 = 12.4K
//e.g. 456243100 = 456M
//Supports Upto Terra,.. then everything is terra from there...
//although integers won't allow that high on intel processors.
//An int64 version is an option if really needed.
var
  s: HTTPString;
  iDigits: integer;
  iGrade, iGradeMod: integer;
  sl, sr: HTTPString;
  e: Extended;
begin
  //convert integer to HTTPString
  s := inttostr(i);

  //If length is less than 5 than result is just the HTTPString conversion
  if length(s)<5 then begin
    result := s;
    exit;
  end;

  //Find number of digits
  e:= log10(i);
  iDigits := trunc(int(e))+1;

  //Find highest number grade qualified (millions, billions, trillions)
  //Grades will be 4-7-10-13...etc
  iGrade := (((iDigits-1) div 3) * 3)+1;

  //top out grade at Terra TTT,GGG,MMM,KKK,000
  if iGrade>13 then
    iGrade:= 13;

  //GradeMod == modulus of previous operation
  //Tells how many digits above the grade the number is
  //Subtract grade base from digit base
  iGradeMod := iDigits-iGrade+1;

  //if more than 3 digits then return Terra with iGradeMod in front
  if iGradeMod>3 then begin
    result := copy(s, 1, iGradeMod)+'T';
  end
  else begin
    //copy out 3 Digits
    s := copy(s, 1, 3);

    //if less than three... then insert a decimal place
    if iGradeMod<3 then begin
      sl := copy(s, 1, iGradeMod);
      sr := copy(s, iGradeMod+1, 3);
      s := sl+'.'+sr;
    end;

    //Add the kilo, mega, giga, terra qualifier
    case iGrade of
      4: s:= s+'K';
      7: s:= s+'M';
      10: s:= s+'G';
      13: s:= s+'T';
    end;

    result := s;

  end;

end;

procedure BlankString(var sHTTPString: HTTPString; sBlankChar: HTTPChar; iStartAt, iEndAt: integer);
var
  t: integer;

begin
  for t := iStartAt to iEndAt do begin
    sHTTPString[t] := sblankChar;
  end;


end;
function SlashDot(sHTTPStringWithSlashes: HTTPString): HTTPString;
// turns any slashes in a HTTPString into dots

var
  t: integer;
begin
  result := sHTTPStringWithSlashes;
  for t:=1 to length(sHTTPStringWithSlashes) do begin
    if result[t] = '/' then
      result[t] := '.';
  end;

end;

//------------------------------------------------------------------------------
function SimpleEncrypt(s: HTTPString): HTTPString;
//A simple HTTPString mangling function.  Use to make HTTPStrings less-readable in network protocols.
var
  t: integer;
begin
  result := s;

  for t:= 1 to length(s) do begin
    result[t] := HTTPChar(ord(s[t]) xor 1);
  end;

end;
//------------------------------------------------------------------------------
function SimpleDecrypt(s: HTTPString): HTTPString;
//See also SimpleEncrypt.
//A simple HTTPString  demangling function.  Use to make HTTPStrings less-readable in network protocols.

begin
  result := SimpleEncrypt(s);

end;

//------------------------------------------------------------------------------
function CRToBR(s: HTTPString): HTTPString;
//Accepts a HTTPString, and returns a HTTPString with Carriage returns converted into '<BR>' tags for HTML display.
var
  sl: TStringList;
  t: integer;
begin
  result := '';
  sl := TStringList.create;
  try
    sl.text := s;
    for t:= 0 to sl.count-1 do begin
      result := result+sl[t]+'<BR>';
    end;
  finally
    sl.free;
  end;
end;



//------------------------------------------------------------------------------
function IntToStaticHash(iNumber: integer): HTTPString;overload;
//Converts an integer into a static hexadecimal hash that is difficult to read.
//Convert back with StaticHashToInt.
var
  sHexTemp: HTTPString;
  t: integer;
const
  KEY = 'H@Ppìë%!##@%FJKQ9812';
begin
  //How the hash works -- simple 8-bit encryption...
  //with the KEY in the hash itself (so that it can morph from page-to-page)
  //futher scrambled with static key to make encoding scheme less obvious.
  //KK-FFFF-FFFF == XB is the XOR byte for

  result := '';

  sHexTemp := inttohex(iNumber, 8);

  //do the encryption
  for t:= 1 to 8 do begin
    //xor the static key into the HTTPString
    sHexTemp[t] := HTTPChar( ord(sHexTemp[t]) xor ord(KEY[t]));
  end;

  //Expand sHexTemp
  for t:= 1 to 2 do begin
    result := result + inttohex(ord(sHexTemp[t]), 2);
  end;

  result := result + '-';

  for t:= 3 to 4 do begin
    result := result + inttohex(ord(sHexTemp[t]), 2);
  end;

  result := result + '-';

  for t:= 5 to 6 do begin
    result := result + inttohex(ord(sHexTemp[t]), 2);
  end;

  result := result + '-';

  for t:= 7 to 8 do begin
    result := result + inttohex(ord(sHexTemp[t]), 2);
  end;

end;
//------------------------------------------------------------------------------
function StaticHashToInt(sHash: HTTPString): integer;
//Decodes an integer that was encoded with IntToStaticHash.
var
  sContent: HTTPString;
  iByte: integer;
  sHexTemp: HTTPString;
  t: integer;
const
  KEY = 'H@Ppìë%!##@%FJKQ9812';
begin
  try
    //How the hash works -- simple 8-bit encryption...
    //with the KEY in the hash itself (so that it can morph from page-to-page)
    //futher scrambled with static key to make encoding scheme less obvious.
    //FFFF-FFFF-FFFF-FFFF == XB is the XOR byte for

    //copy the parts minus the dashes into a hex thing
    sHexTemp := copy(sHash, 1,4)+copy(sHash, 6,4)+copy(sHash, 11,4)+copy(sHash, 16, 4);

    //convert the individual HTTPString bytes into ASCII characters
    for t:= 1 to 8 do begin
      iByte := strtoint('$'+copy(sHexTemp, (t*2)-1, 2));
      sContent := sContent+HTTPChar(iByte);
    end;

    //apply XOR mask to ASCII characters
    for t:= 1 to 8 do begin
      //xor the static key into the HTTPString
      sContent[t] := HTTPChar( ord(sContent[t]) xor ord(KEY[t]));

      //xor the clientIP HTTPString in reverse order
      //sContent[t] := HTTPChar( ord(sContent[t]) xor ord(sClientIP[(length(sClientIP)-t)+1]));
    end;

    result := strtoint('$'+sContent);
  except
    raise HTTPException.create(sHash + ' is not a valid ID');
  end;

end;
function ExtractWebRootPath(sPath: HTTPString): HTTPString;
//Returns the base virtual directory of a given URL.
var
  a,b: HTTPString;
begin
  DecodeURL(sPath, a, b);
  result := 'http://'+adjustWebPath(b);

end;

function ExtractWebPath(sPath: HTTPString): HTTPString;
var
  a,b: HTTPString;
begin
  SplitString(sPath, '#', a, b);
  SplitString(a, '?', a, b);
  SplitString(a, '/', a, b, true);

  result := a;

end;

//------------------------------------------------------------------------------
function AdjustWebPath(sPath: HTTPString): HTTPString;
//Adds a slash on the end of the HTTPString if there isn't one.
var
  i: integer;
begin
  i := length(sPath);

  if i=0 then
    result := sPath
  else
    if NOT (sPath[i] = '/') then
      result := sPath+'/'
    else
      result := sPath;

end;

function Flashify(sl: TStringList) : HTTPString;
// translates launchdata-type requests into flash compatible form (single line)

var
  t: integer;
  a, b: HTTPString;
begin
  result := '';
  for t:= 0 to sl.count-1 do begin
    if t>0 then
      result := result+'&';

    SplitString(sl[t], '=', a, b);

    result := result + EncodeWebString(a)+'='+EncodeWebString(b);
  end;

  result := result + '&loaded=1&somevaraiable=1';


end;

function RandomHTTPString(iLength: integer): HTTPString;
var
  t: integer;
begin
  result := '';
  for t:= 1 to iLength do begin
    result := result + HTTPChar(random(26)+ord('a'))
  end;

end;

//------------------------------------------------------------------------------
procedure SplitAccount(sAccount: HTTPString; out iDataCenter: integer; out sAccountID: HTTPString);
//Splits an datacenter-account id value in to respective datacenter and accountid parameters individually.
//<li>sAccount: contains the input HTTPString</li>
//<li>iDataCenter: is the output variable for the Data Center ID</li>
//<li>sAccountID: is the output variable for the Account ID</li>
var
  sDataCenter: HTTPString;
begin
  SplitString(sAccount, '-', sDataCenter, sAccountID, true);
  if sAccountID = '' then begin
    sAccountID := sDataCenter;
    sDataCenter := '';
  end;
  if sDataCenter = '' then
    iDataCenter := 0
  else
    iDataCenter := strtoint(sDataCenter);
end;

//------------------------------------------------------------------------------
function RepairLinuxHTTPString(sLinuxHTTPString: HTTPString): HTTPString;
//This function is used to convert a HTTPString that uses only LineFeeds instead of
//carriage returns followed by line feeds, into a HTTPString that always has carriage returns before the linefeeds.
//If called on a HTTPString that already has carriage returns before the linefeeds, it does nothing.
var
  t: integer;
begin
  result := '';
  for t:= 1 to length(sLinuxHTTPString) do begin
    //if we find a linefeed
    if (sLinuxHTTPString[t] = #10) then begin
      //if the first character then just concatinate CRLF
      if t=1 then begin
        result := result + #13#10;
      end
      else begin
        //else check to see if the previous character was NOT C
        if sLinuxHTTPString[t-1] <> #13 then begin
          //put CRLF if previous character was not CR
          result := result + #13#10;
        end else
          //else just put a LF because CR was already put
          result := result + #10;
      end;
    end
    //Else just build the HTTPString from the contents of the original
    else begin
      result := result + sLInuxHTTPString[t];
    end;
  end;


end;

function MakeSecureURL(sURL: HTTPString): HTTPString;
begin
  result := sURL;
  if lowercase(copy(sURL, 1, 5)) = 'http:' then begin
    result := 'https:'+copy(sURL, 6, length(sURL));
  end;
end;


function EncodeTAGString(s: HTTPString; sEscapeChar: HTTPChar): HTTPString;
//Encodes a HTTPString so that special characters in it do not disrupt
//HTTP headers.  Also known as "URL" Encoding or "escape" encoding.
var
  t: integer;
  u: integer;
  sHex: HTTPString;
begin
  u := 1;
  setlength(result, length(s)*3+1);

  //cycle through the HTTPString
  for t:= 1 to length(s) do begin
    //if  the character is anything other than a standard letter or number
    if (ord(s[t])<48) or (ord(s[t])>122) or ((ord(s[t])>57) and (ord(s[t])<65))then begin
      if charinset(s[t],[' ','!','@','#','$','^','&','*','(',')','-','=','_','[',']','{','}','\','|',':',';','/','.',',','?']) then begin
        result[u] := s[t];
        inc(u);
        continue;
      end;
      //encode the character as a special hex job
      sHex := inttohex(ord(s[t]), 2);
      result[u] := sEscapeChar;
      result[u+1] := sHex[1];
      result[u+2] := sHex[2];
      inc(u, 3);
    end else begin
      result[u] := s[t];
      inc(u);
    end;
  end;
  //tag a 0 on the end

  result := copy(result, 1, u-1);

  if (result = '') and not (sEscapeChar='%') then
    result := sEscapeChar+'02';


end;

function EncodeXMLString(s: HTTPString; sEscapeChar: HTTPChar): HTTPString;
//Encodes a HTTPString so that special characters in it do not disrupt
//HTTP headers.  Also known as "URL" Encoding or "escape" encoding.
var
  t,x: integer;
  u: integer;
  sReplace: HTTPString;
begin
  u := 1;
  setlength(result, length(s)*3+1);

  //cycle through the HTTPString
  for t:= 1 to length(s) do begin
    //if  the character is anything other than a standard letter or number
    if (ord(s[t])<48) or (ord(s[t])>122) or ((ord(s[t])>57) and (ord(s[t])<65))then begin
      if charinset(s[t],[' ','!','@','#','$','^','*','(',')','-','=','_','[',']','{','}','\','|',':',';','/','.',',','?','"','''',#13,#10,#9]) then begin
        result[u] := s[t];
        inc(u);
        continue;
      end;
      case s[t] of
        '<': sReplace := '&lt;';
        '>': sReplace := '&gt;';
        '&': sReplace := '&amp;';
      else
        sReplace := sEscapeChar+inttohex(ord(s[t]), 2);
      end;
      //encode the character as a special hex job
      for x:= 1 to length(sReplace) do begin
        result[(u+x)-1] := sReplace[x];
      end;
      inc(u, length(sReplace));
    end else begin
      result[u] := s[t];
      inc(u);
    end;
  end;
  //tag a 0 on the end

  result := copy(result, 1, u-1);

  if (result = '') and not (sEscapeChar='%') then
    result := sEscapeChar+'02';


end;

function IsValidURL(sURL: string): boolean;
begin
  result := true;
  if zpos(#13, sURL) >= 0 then exit(false);
  if zpos(#10, sURL) >= 0 then exit(false);
  if zpos('(', sURL) >= 0 then exit(false);
  if zpos('''', sURL) >= 0 then exit(false);
  if zpos('{', sURL) >= 0 then exit(false);
  if zpos('<', sURL) >= 0 then exit(false);
  if zpos('javascript:', sURL) >= 0 then exit(false);

end;

function XMLStringToPlainText(sOriginal: HTTPString; sEscapeChar:HTTPChar = '%'): HTTPString;
//Decodes a HTTPString that was URL encoded.  This function is the opposite of EncodeWebString.
// parses a HTTPString and replaces '+' with ' ' and '%nn' with the
// appropriate ASCII value
var
  t,u: integer;
  sTemp: HTTPString;
  bfound: boolean;
begin
  result := '';
  t := 1;
  repeat
    if sOriginal[t] = '&' then begin
      bFound := false;
      for u:= t to t+5 do begin
        if u> length(sOriginal) then
          break;

        if sOriginal[u] = ';' then begin
          sTemp := copy(sOriginal, t+1, (u-t)-1);
          if sTemp = 'lt' then sTemp :='<' else
          if sTemp = 'rt' then sTemp := '>' else
          if sTemp = 'amp' then sTemp := '&' else
          try
            stemp := zcopy(sTemp, 1, 3);
            if IsInteger(sTemp) then
              sTemp := HTTPChar(strtoint(sTemp))
            else
              sTemp := '';
          except
            sTemp := '';
          end;
          inc(t, (u-t)+1);
          bFound := true;
        end;
      end;
      if not bFound then
        inc(t);
    end
    else begin
      sTemp := sOriginal[t];
      inc(t);
    end;

    result := result + sTemp;
  until t = length(sOriginal)+1;


(*  sOriginal := DecodeWebString(sOriginal);

  sOriginal := StringReplace(sOriginal, '&lt;', '<', [rfReplaceAll]);
  sOriginal := StringReplace(sOriginal, '&gt;', '>', [rfReplaceAll]);
  result  := StringReplace(sOriginal, '&amp;', '&', [rfReplaceAll]);*)




end;
function ExtractURLPAth(sURL: HTTPString): HTTPString;
var
  i: integer;
begin
  i := lastpos('/', sURL);
  if i < 1 then
    result := sURL
  else begin
    result := copy(sURL, 1, i);
  end;
end;


function AdjustURLForParams(sPath: HTTPString): HTTPString;
var
  i: integer;
  bNoQuestion: boolean;
  cEnd: HTTPChar;
begin
  i := length(sPath);

  bNoQuestion := pos('?', sPath) < 1;
  if bNoQuestion then
    cEnd := '?'
  else
    cEnd := '&';



  if i=0 then
    result := sPath
  else
    if NOT (sPath[i] = cEnd) then
      result := sPath+cEnd
    else
      result := sPath;

end;


function FindLinkInHTML(sBaseURL: HTTPString; sHTML: HTTPString; sLinkText: HTTPString; iInstance: integer=0): HTTPString;
var
  t: integer;
  sLeft, sRight: HTTPString;
begin
  Debug.Log(nil,'Find link in HTML:"'+sLInkText+'" instance '+inttostr(iInstance));
  result := '';

  //split at text, the <a> tag will be in the left
  sRight := sHTML;
  for t:= 0 to iInstance do begin
    if not SplitStringNoCaseEx(sRight, sLinkText, sLeft, sRight, '<','>') then
      exit;
  end;
  //split at last <a tag
  SplitStringNoCase(sLeft, '<a', sLeft, sright, true);
  //link body is in the right

  //find href=
  SplitStringNoCase(sRight, 'href', sLeft, sRight);

  SplitStringNoCase(sRight, '=', sLeft, sRight);

  case PosFirst(sRight, ' ', '"') of
    0: result := '';
    2: begin
        SplitString(sRight, '"', sLeft, sRight);
        SplitString(sRight, '"', result, sRight);
       end;
    1: SplitString(sRight, ' ', result, sRight);
    3: SplitString(sRight, '>', result, sRight);
  end;


  if not (copy(lowercase(result),1,4)='http') then begin
    if result <> '' then
      result := AdjustWebPath(sBaseURL)+result;
  end;
end;

function ExtractLinks(sBasePath: HTTPString; sHTML: HTTPString): HTTPString;
var
  sLeft, sRight: HTTPString;
  sFoundURL: HTTPString;
  slResults: TStringList;
begin
  slResults := TStringlist.create;
  slResults.Duplicates := dupIgnore;
  try
    while SplitString(sHTML, 'href=', sLEft, sRight) do begin
      if (pos('"', sRight) > pos('.', sRight)) or (pos('"', sRight) < 1) then begin
        SplitString(sRight, '>', sFoundURL, sHTML);
        SplitString(sFoundURL, ' ', sFoundURL, sRight);
      end else begin
        SplitString(sRight, '"', sLeft, sFoundURL);
        SplitString(sFoundURL, '"', sFoundURL, sHTML);
      end;

      if pos('http://', lowercase(sFoundURL)) < 1 then
        sFoundURL := sBasePAth+sFoundURL;

      slResults.add(sFoundURL);



    end;
  finally
    result := slResults.text;
    slResults.free;
  end;
end;




function FindIFrameInHTML(sBaseURL: HTTPString; sHTML: HTTPString; iInstanceOneBased: integer; out sSrc: HTTPString): boolean;
var
  sLeft, sRight: HTTPString;
  t: integer;
begin
  sSrc := '';
  result := false;

  sRight := sHTML;
  //split out other instances
  for t:= 1 to iInstanceOneBased-1 do begin
    SplitStringNoCase(sRight,'iframe', sLeft, sRight);
  end;

  //split this instance (leave if nothing found)
  if not SplitStringNoCase(sright, 'iframe', sLeft, sRight) then
    exit;

  //src= should be on the right
  if not SplitStringNoCase(sright, 'src', sLeft, sRight) then
    exit;

  if not SplitStringNoCase(sright, '=', sLeft, sRight) then
    exit;

  case PosFirst(sRight, ' ', '"','>') of
    0: result := false;
    2: begin
        SplitString(sRight, '"', sLeft, sRight);
        SplitString(sRight, '"', sSrc, sRight);
        result := true;
       end;
    1: begin
        SplitString(sRight, ' ', sSrc, sRight);
        result := true;
       end;
    3: begin
        SplitString(sRight, '>', sSrc, sRight);
        result := true;
       end;
  end;


end;


function FindTagAttributeInHTML(sBaseURL: HTTPString; sHTML: HTTPString; sTag: HTTPString; sAttribute: HTTPString; iInstanceOneBased: integer; out sSrc: HTTPString): boolean;
var
  sLeft, sRight: HTTPString;
  t: integer;
begin
  sSrc := '';
  result := false;

  sRight := sHTML;
  //split out other instances
  for t:= 1 to iInstanceOneBased-1 do begin
    SplitStringNoCase(sRight,'<'+stag, sLeft, sRight);
  end;

  //split this instance (leave if nothing found)
  if not SplitStringNoCase(sright, '<'+stag, sLeft, sRight) then
    exit;

  //src= should be on the right
  if not SplitStringNoCase(sright, sAttribute, sLeft, sRight) then
    exit;

  if not SplitStringNoCase(sright, '=', sLeft, sRight) then
    exit;

  case PosFirst(sRight, ' ', '"','>') of
    0: result := false;
    2: begin
        SplitString(sRight, '"', sLeft, sRight);
        SplitString(sRight, '"', sSrc, sRight);
        result := true;
       end;
    1: begin
        SplitString(sRight, ' ', sSrc, sRight);
        result := true;
       end;
    3: begin
        SplitString(sRight, '>', sSrc, sRight);
        result := true;
       end;
  end;


end;

function RemoveComments(sHTML: HTTPString): HTTPString;
//TODO: This is an imcomplete solution
var
  t: integer;
  xon: boolean;
begin
 sHTML := StringReplace(sHTML,'<!', #1, [rfReplaceAll]);
  xon := true;
  for t := 1 to length(sHTML) do begin
    if sHTML[t] = #1 then xon := false;

    if xon then
      result := result + sHTML[t];

    if sHTML[t] = '>' then xon := true;

    result := result + sHTML[t];
  end;



end;
function ApplyRelativeURL(sBaseURL: HTTPString; sURL: HTTPString): HTTPString;
var
  sHead, sHOST, sPath: HTTPString;
begin
  if copy(lowercase(sURL), 1,7) = 'http://' then begin
    result := sURL
  end else begin
    if copy(sURL, 1,1) = '/' then begin
      SplitString(sBASEURL, '//', sHead, sHost);
      SplitString(sHost, '/', shost, sPath);
      result := sHead +'//'+sHost+'/'+sURL;

    end else begin
      sBaseURL := ExtractURLPath(sBaseURL);
      result := sBaseURL+sURL;
    end;
  end;

end;

function ExtractURLFileNAme(sURL: string): string;
var
  nvpl: TNameValuePairList;
  sLeft, sRight: string;
begin
  nvpl := DecodeURLParams(sUrl, result);
  SplitString(result, '/', sLeft, sRight, true);
  result := sRight;
  nvpl.free;
  nvpl := nil;
end;
function DecodeURLParams(sURL: string; out sDocument: string): TNameValuePairList;
var
  sLeft, sRight, sName, sValue: string;
begin

  sURL := StringReplace(sURl, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  result := TNameValuePairList.create;
  sDocument := '';
  sRight := '';

  SplitString(sURL, '#', sLeft, sRight);

  if not SplitString(sLeft, '?', sDocument, sRight) then begin
//    exit;
  end;

  while SplitString(sRight, '&', sLeft, sRight) do begin
    SplitString(sLeft, '=', sName, sValue);
    result.Add(sName, sValue);
  end;

  if SplitString(sLeft, '=', sName,sVAlue) then begin
    result.Add(sName, sValue);
  end;



end;


{$IFDEF WINDOWS}
function DecodeURLParams(sURL: HTTPString; out sDocument: string): TNameValuePairList;
var
  outS: HTTPString;
begin
  result := DecodeURLParams(sURL, outS);
  sDocument := outS;

end;
{$ENDIF}

function StringListToHTML(sl: TStringList): string;
begin
  for var t := 0 to sl.count-1 do begin
    var s := sl[t];
    result := result + s+'<br/>';
  end;
end;




end.
