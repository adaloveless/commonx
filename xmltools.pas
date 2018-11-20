unit XMLTools;
//This unit has some handy classes for manipulating basic XML.
{$DEFINE QUICKXML}
//some important info about ReadValue and ReadIntegerValue etc.
//some examples
//rootnode:subnode:multinode*   (reads count of muiltinodes under subnode under rootnode
//rootnode:subnode:multinode[0] (reads first multinode under subnode
//rootnode:subnode:multinode[Attrib=Value] reads multinode with given value (useful for keys/IDs)
//rootnode:subnode.attrib reads attribute of subnode
//rootnode:subnode:multinode[2].id reads attribute of multinode


interface

uses debug, betterobject, generics.collections.fixed, Classes, SysUtils, windows, dialogs, namevaluepair, variants, FileCtrl, ComObj, activex, MSXML2_TLB, commandprocessor, commandicons;

type
  XMLPArseException = class(Exception);
  TXMLArray = array of variant;
  TXMLDocument = class;//forward
  TXMLAttributes = class(TNameValuePairList)
  public
    Constructor Create;override;
    Destructor Destroy;override;
  end;


  Tcmd_PArseXML = class(TCommand)
  private
    FValue: string;
    FResult: TXMLDocument;
  public
    property result: TXMLDocument read FResult write FResult;
    property value: string read FValue write FValue;
    procedure InitExpense;override;
    procedure DoExecute;override;
  end;


  TXMLElement = class(TBetterObject)
    //r: Represents an XML Element in an XML document.
    //r: Provides a means to access sub-elements of the XML Element.
    //r: Parses the XML in its own scope within the XML document to build a list of sub-elements.
    //This class represents an XML element, which may be a subelement of another XML element, and/or contain other XML Elements.
  private
    FParent: TXMLElement;
    FElementName: string;
    FValue: string;
    FElements: TList<TXMLElement>;
    FAttributes: TXMLAttributes;
    FParsed: boolean;
    FUseNativeParser: boolean;
    function GetAttributes: TXMLAttributes;
    function GetElementsByIndex(idx: integer): TXMLElement;//
    procedure SetValue(const sValue: string);
    function GetElements(sName: string; iInstance: integer): TXMLElement;
    function GetElementcount: integer;
    function ExtractFirstTag(sDocument: string; var iStartAt: integer; out sTagName, sTagContents: string;out attributes: TXMLArray): boolean;
    function GetValue: string;
    function GetIntegerValue: integer;
    procedure SetIntegerValue(const Value: integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ParseOld; virtual;
    procedure Parse; virtual;
    function ParseTagContents(sTagContents: string; subelement: TXMLElement): string;

    property Attributes: TXMLAttributes read GetAttributes write FAttributes;
    //Points to the TXMLAttributes class which can be used to determine attributes specified within the XML brackets.
    property ElementName: string read FElementName write FElementName;
    //Returns the tag name of the Element
    property Value: string read GetValue write SetValue;
    property IntegerValue: integer read GetIntegerValue write SetIntegerValue;
    property Parsed: boolean read FParsed write FParsed;
    //Returns all XML within the scope of its tag in RAW form. Can potentially return text containing other sub-tags.
    property Parent: TXMLElement read FParent write FParent;
    //Points to the XML Element or Document that contains this element.
    property ElementsByIndex[idx: integer]: TXMLElement read GetElementsByIndex;
    //Retrieves an individual TXMLElement class by index that is CONTAINED within this element. Read ElementCount to determine how many there are.
    property Elements[sName :string; iInstance: integer]: TXMLElement read GetElements;
    //Retrieves an individual TXMLElement class by NAME that is CONTAINED
    //within this element. There can be multiple elements with the same name
    //underneath the current element, so this Array property is 2-dimensional.
    //The first dimension is the name, the second is the instance of the name.
    //Pass an instance index of 0 if you expect there to be only one element of
    //given name underneath this element.
    property Elementcount: integer read GetElementcount;
    //Returns the number of XML sub-elements under this element in the document.
    function GetElementNameCount(sName: string): integer;
    //Returns the number of XML sub-elements under this element that match a specific name.
    procedure ClearElements;
    //Removes all sub-elements from this element.
    function AddChild: TXMLElement;
    function HasElement(sName: string): boolean;overload;
    function FindElement(sName: string; attributematch: variant): TXMLElement;overload;
    function ReadValue(sValueAddr: string): string;
    procedure WriteValue(sValueAddr: string; sValue:string);
    function ReadNumberValue(rDEfault: real; sValueAddr: string): real;
    function ReadIntegerValue(iDEfault: integer; sValueAddr: string): integer;
    property UseNativeParser: boolean read FUseNativeParser write FUseNativeParser;

    function ExportValue: string;
    function ExportChildren: string;


  end;

  TXMLDocument = class(TXMLElement)
  private
    FIgnoreErrors: boolean;
    //r: Represents an XML Document in memory.
    //This class is created to represent the XMLDocument as a whole.  It is
    //inherited from TXMLElement, because it essentially supports all the properties
    //of TXMLElement (although the Parent property is irrellevant).  It currenty
    //doesn't implement anything new over TXMLElement, but may in the future.
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromFile(sFile: string);
    procedure SaveToFile(sFile: string);
    procedure ParseSub(doc: IXMLDOMDocument2; elem: IXMLDOMElement; myElem: TXMLElement);
    procedure Parse; override;
    property IgnoreErrors: boolean read FIgnoreErrors write FIgnoreErrors;
  end;

  TXMLBuilder = class(TObject)
  //r: Proviedes an interface useful for generating an XML document.
  //This class is is somewhat useful for building XML documents as responses for webpages.
  //It serves to provide a cleaner interface for building XML other than brute-force
  //string concatination, and does some of the tedious formatting work for you.
  private
      FXML: string;
  public
    constructor Create; reintroduce; virtual;
    procedure OpenSimpleElement(sElementName: string; bClose: boolean=false);
    procedure OpenElement(sElementName: string; attributes: variant; bClose: boolean=false);
    procedure Open(sElementName: string; bClose: boolean=false); overload;
    procedure Open(sElementName: string; attributes: array of variant; bClose: boolean=false);overload;

    procedure AddBody(sText: string);
    procedure CloseElement(sElementName: string);
    procedure Close(sElementName: string);
    property XML: string read FXML write FXML;
    //Read to get the XML document in Raw text form after you have added all
    //the information to it.
    procedure SaveToFile(sFile: string; bForceDirectory: boolean = false);
  end;

  function SuperPOS(sChar: char; iStartAt: integer; sFullString: string; sNotInside: char = #0):integer;

  function RemoveOuterTag(sXML: string): string;

var
  GelementCount: integer;

implementation

uses WebString, stringx;


function TXMLElement.ExportChildren: string;
var
  t: integer;
begin
  result := '';
  for t:= 0 to elementCount-1 do begin
    result := result + elementsbyindex[t].ExportValue;
  end;

end;

function TXMLElement.ExportValue: string;
var
  bHasBody: boolean;
  t: integer;
begin
  bHasbody := (Value <> '') or (ElementCount > 0);
  result := '<'+elementName+' ';
  if assigned(Fattributes) then
  for t:= 0 to attribUTES.COUNT-1 do begin
    result := result + attributes.itemsbyindex[t].name+'="'+attributes.itemsbyindex[t].value+'"'+#13#10;
  end;

  if not bHasBody then
    result := result + ' />'
  else begin
    result := result + '>'+ExportChildren+'</'+elementName+'>';
  end;



end;

function TXMLElement.ExtractFirstTag(sDocument: string; var iStartAt: integer; out sTagName, sTagContents: string;out attributes: TXMLArray): boolean;
//p: sDocument: The XML document as a string.
//p: iStartAT: The postiion in the XML document to start the search.
//p: out sTagName: returns the name of the next tag found in the XML document.
//p: out sTagContents: returns the contents of the tag... up to the closing of the XML tag.
//p: out attributes: Returns an array containing the parameters contained within the tag definition itself.
///This is the brute that parses XML for TXMLelement.parse.  It extracts the next tag
//available in the XML document and returns it to the caller for handling.
label
  recheck, skipcontents;
var
  i1, i2: integer;
  iEndTag: integer;
  sTemp: HTTPstring;
  sEndTag: HTTPstring;
  element: TXMLElement;
  bClosed: boolean;
begin
  sTagName := ''; sTagContents := '';
  result := false;
  //SetLength(attributes, 0);
recheck:


  //Find first occurance of '<';
  i1 := superpos('<', iStartAt, sDocument, '"');

  if i1<1 then
    exit;

  //Find first occurance of '>';
  i2 := superpos('>', iStartAt, sDocument, '"');

  if i2<i1 then
    exit;

  //If > is less than < then raise exception... illegal XML format
  if i2< i1 then begin
    raise exception.create('Illegal XML format.  Start of tag at '+inttostr(i1)+'; end of tag at '+inttostr(i2));
  end;

  //Extract the name of the tag from the string
  sTagName := copy(sDocument, i1+1, (i2-i1)-1);

  //ingore comments
  if copy(sTagName, 1, 3) = '!--' then begin
    iStartAT := i2+1;
    goto recheck;
  end;

  //ignore ? tags
  if copy(sTagName, 1, 1) = '?' then begin
    iStartAT := i2+1;
    goto recheck;
  end;

  //ignore / tags
  if copy(sTagName, length(sTagName), 1) = '/' then begin
    iStartAT := i2+1;
    sTagContents := '';
    sTagName := copy(sTagName, 1, length(sTagName)-1);
    bClosed := true;
  end else
    bClosed := false;

  element := self.AddChild;
  sTagName := ParseTagContents(sTagName, element);

  if not bClosed then begin
    //Find the start of the end tag
    sTemp := lowercase(sDocument);
    sEndTag := '</'+lowercase(sTagName)+'>';
    BlankString(sTemp, ' ', 1, iStartAT-1);
    iEndTag := pos(sEndTag, sTemp);

    if iEndTag < i2 then begin
      raise Exception.create('Illegal XML Format.  End tag not found for '+sTagName+', '+FValue)
    end;


    //Extract everything between the start and end tags
    sTagContents := copy(sDocument, i2+1, iEndTag-(i2+1));

    //Reset StartAT position to be right after end tag
    iStartAT := iEndTag+length(sEndTag);
  end;

skipcontents:
  element.ElementName := sTagName;
  element.Value := sTagContents;
  //Done
  result := true;

end;


{ TXMLElement }
//------------------------------------------------------------------------------
function TXMLElement.AddChild: TXMLElement;
//Creates an XML Element, adds it as a child of this element, and returns a
//reference to the created element.  Hence, you should not free the element EVER, because
//this object retains ownership of it.
var
  element: TXMLElement;
begin
  element := TXMLElement.create;
  element.parent := self;
  element.UseNativeParser := self.UseNativeParser;

  FElements.add(element);
  result := element;


end;

//------------------------------------------------------------------------------
procedure TXMLElement.ClearElements;
//Removes and frees all sub-elements of this element.
var
  t: integer;
begin
  for t:= FElements.count-1 downto 0 do begin
    TXMLElement(FElements[t]).free;
    FElements.delete(t);
  end;
end;

constructor TXMLElement.Create;
//A standard constructor.
begin
  inherited Create;
 
  FElements := TList<TXMLELement>.create;
  FAttributes := nil;
  FParsed := false;
end;
//------------------------------------------------------------------------------
destructor TXMLElement.Destroy;
//A standard destructor.
var
  t: integer;
begin
//  consolelog('destroying : '+self.ElementName);
  //Free up elements
  ClearElements;
//  for t:= FElements.count-1 downto 0 do begin
//    TXMLElement(FElements[t]).free;
//  end;
  FAttributes.free;
  FElements.clear;
  FElements.free;

  inherited;
end;
//------------------------------------------------------------------------------
function TXMLElement.GetAttributes: TXMLAttributes;
//Getter for the Attributes property.
//Returns a TXMLAttributes class that maintains a list of the attributes of the Element tag.
begin
  if NOT assigned(FAttributes) then
    FAttributes := TXMLAttributes.create;

  result := FAttributes;

end;

function TXMLElement.GetElementcount: integer;
//Getter for the Elementcount property.
begin
  result := 0;
  if FElements = nil then
    exit;
  if not Parsed then
    parse;

  result := FElements.count;
end;

function TXMLElement.GetElementNameCount(sName: string): integer;
//Getter for the ElementName count property.  Returns the number of sub-elements
//with the given name.
var
  elem: TXMLElement;
  t: integer;
begin
  if not Parsed then
    parse;

  result := 0;
  for t:=0 to self.ElementCount-1 do begin
    elem := ElementsByIndex[t];
    if lowercase(elem.ElementName) = lowercase(sName) then
      inc(result);
  end;
end;

function TXMLElement.GetElements(sName: string; iInstance: integer): TXMLElement;
//p: sName: the Name of the Element you are looking for.
//p: iInstance: the instance of the element named sName under this element.
//Getter for the Elements array property.
//Returns an XML element by name.  There can be multiple elements with the same name
//so it also requires an instance index.  An exception will be raised if the element cannot be found.
var
  t: integer;
  element: TXMLElement;
  iCount : integer;
begin
  if not Parsed then
    parse;

  iCount := 0;
  result := nil;
  //cycle sub elements
  for t:= 0 to ElementCount-1 do begin
    element := FElements[t];

    //if element at index.elementname = name passed in then set result and exit
    if lowercase(element.ElementName) = lowercase(sName) then begin
      if iCount =iInstance then begin
        result := element;
        break;
      end else
        inc(iCount);
    end;
  end;

  //if result is still nil then exception... cannot get an element
  //that doesn't exist
  if result=nil then
    raise Exception.create(sName+' is not an XML sub-element of '+self.ElementName);

end;
//------------------------------------------------------------------------------
function TXMLElement.GetElementsByIndex(idx: integer): TXMLElement;
//Getter for the ElementsByIndex property.
begin
  if not Parsed then
    parse;

  result := FElements[idx];

end;
//------------------------------------------------------------------------------
function TXMLElement.HasElement(sName: string): boolean;
//p: sName: the Tag name of the element you are looking for.
//Returns whether or not this element has a child element of given name.
var
  t: integer;
begin
  if not Parsed then
    parse;

  sName := lowercase(sName);
  result := false;
  for t:= 0 to ElementCount-1 do begin
    if lowercase(ElementsByIndex[t].ElementName)=sName then
      result := true;
  end;

end;

procedure TXMLElement.Parse;
begin
  if UseNativeParser then
    ParseOld;

  FParsed := true;
end;

procedure TXMLElement.ParseOld;
var
  iStartAt: integer;
  sTagName: string;
  sTagContents: string;
  ary: TXMLArray;
begin
  iStartAt := 1;

  while ExtractFirstTag(FValue, iStartAt, sTagName, sTagContents, ary) do begin
  end;

  Parsed := true;

end;
//------------------------------------------------------------------------------
function TXMLElement.ParseTagContents(sTagContents: string; subelement: TXMLElement): string;
//Parses the contents of sTagContent for given element 'subelement'.
//p: sTagContents: The text you are parsing.
//p: subelement: The element to which sub-elements will be added.
var
  sName, sValue, sRemaining: string;
  ipos: integer;
begin
  result := '';
  //if tag with no attributes then exit;
  if not SplitString(sTagContents, ' ', result, sRemaining) then begin
    result := sTagContents;
    exit;
  end;


  while SplitString(sRemaining, '=', sName, sRemaining) do begin
    sName := trim(sName);
    //remove first quote
    ipos := pos('"', sRemaining);
    sRemaining := copy(sRemaining, ipos+1, length(sRemaining));

    //split at second quote

    if not SplitString(sRemaining, '"', sValue, sRemaining) then
      raise Exception.create('unterminated string parsing '+sTagContents)
    else begin
      subelement.elementname := result;
      subelement.Attributes.Add(sName, sValue);
    end;
  end;


end;

procedure TXMLElement.SetValue(const sValue: string);
//p: Value: the Raw XML text that is relevant in the scope of this element.
//Setter for the VAlue property which stores the RAW XML for the sub element.
//Only the XML relevant to this element's scope should be stored.  After the XML
//is stored, the XML is parsed and individual instances of TXMLElement are added
//as childred on this instance.
begin
  self.ClearElements;

  FValue := '';
  {$IFNDEF QUICKXML}
  for t:= 1 to length(sValue) do begin
    if not (
      (sValue[t] = #10) or
      (sValue[t] = #13) or
      (sValue[t] = #9)
    ) then begin
      FValue := FValue + sValue[t];
    end;
  end;
  {$ELSE}
    FValue := sValue;
  {$ENDIF}
  FValue := Trim(FValue);
  FParsed := false;
//  Parse;


end;
procedure TXMLElement.WriteValue(sValueAddr, sValue: string);
var
  sLEft, sRight, sElementName, sAttributeName: string;
  elem: TXMlElement;
  i, i1, i2,i3: integer;
  sAttributeMatch: string;
  sAttributeValue: string;
begin
  if copy(sValueAddr, 1, 1) = ':' then
    sValueAddr := copy(sValueAddr, 2, length(sValueAddr));
  (*if copy(sValueAddr, 1, 1) = '.' then
    sValueAddr := copy(sValueAddr, 2, length(sValueAddr));*)

  i1 := pos(':', sValueAddr);
  i2 := pos('.', sValueAddr);
  i3 := pos('[', sValueAddr);

  //substitute '[' for '.' for attribute matches
  if (i3<i2) or (i2=0) then begin
    if i3>0 then
      i2 := i3;

  end;

  //determine if to use ':' or '.' or '['
  if (i1<i2) and (i1>0) then begin
    i := i1
  end else begin
    if i2>0 then
      i := i2
    else
      i := i1;

  end;

  //if no ':' or '.' return value
  if (i1 = i2) and (i1 < 1) then begin
    if copy(sValueAddr, length(sValueAddr),1) = '*' then begin
      raise Exception.create('* is not allowed when writing to XML');
    end else begin
      if self.HasElement(sValueAddr) then
        self.Elements[sValueAddr,0].value := sValue;
    end;
  end
  else
  //if separated by ':' then get sub element
  if i=i1 then begin
    SplitString(sValueAddr, ':', sLeft, sRight);

    if self.haselement(sLeft) then begin
      self.elements[sLeft,0].WriteValue(sRight, svalue);
    end;
  end else
  //if separated by '.' then read attribute
  if (i=i2) and (i2<>i3)then begin
    SplitString(sValueAddr, '.', sLeft, sRight);

    if (sLEft = '') then begin
      if self.Attributes.Has(sRight) then
        self.Attributes[sRight].Value := sValue;
    end else
    if self.HasElement(sLEft) and (self.Elements[sLeft,0].attributes.has(sRight)) then begin
      self.Elements[sLeft,0].attributes[sRight].Value := sValue;
    end;
  end else
  //advanced array lookup
  begin
    SplitString(sValueAddr, '[', sElementName, sRight);
    SplitString(sRight, ']', sAttributeMatch, sRight);
    //if a attribute match query is specified then find it
    if SplitString(sAttributeMatch,'=', sAttributeNAme, sAttributeValue) then begin
      elem := FindElement(sElementName, VarArrayOf([sAttributeNAme, sAttributeValue]));
      if elem <> nil then begin
        elem.WriteValue(sRight, sValue);
      end;
    end
    //else assume the node is being referenced by numeric index
    else begin
//      try
        self.elements[sElementNAme, strtoint(sAttributeMatch)].WriteValue(sRight, sValue);
//      except
//        on E:Exception do begin

//        end;
//        raise Exception.create('Non numeric attribute value passed to ReadValue: '''+sAttributeMatch+'''');
//        result := '';
//      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function SuperPOS(sChar: char; iStartAt: integer; sFullString: string; sNotInside: char = #0):integer;
//p: sChar: The character you're looking for.
//p: iStartAT: The position in the string to begin the search.
//p: sFullString: The full string that you are searching.
//Returns the position sChar in sFullstring, beginning the search at iStartAT.
//Returns <1 if not found.
var
  t: integer;
  bIgnore: boolean;
begin
  result := 0;
  bIgnore := false;

  for t:=iStartAt to length(sFullstring) do begin
    if (sNotInside<> #0) and (sFullString[t] = sNotInside) then begin
      bIgnore := not bIgnore;
    end;
    if (not bIgnore) and (sFullString[t] = sChar) then begin
      result := t;
      exit;
    end;
  end;

end;


{ TXMLDocument }

constructor TXMLDocument.Create;
//A standard constructor.
begin
  inherited;
  self.ElementName := 'XMLDocument';

end;


destructor TXMLDocument.Destroy;
begin

  inherited;

end;

procedure TXMLDocument.LoadFromFile(sFile: string);
var
  sl : tStringlist;
  s: string;
begin
  sl := TStringLIst.create;
  try
    if fileexists(sFile) then
      sl.LoadFromFile(sFile);
    s := sl.text;
    sl.clear;
    if s <> '' then
      self.Value := s
    else
      self.value := '<empty/>';
  finally
    sl.free;
  end;

end;

procedure TXMLDocument.Parse;
var
  doc: IXMLDOMDocument3;
  elem: IXMLDOMElement;
  myElem: TXMLElement;

begin
  if UseNativeParser then begin
    ParseOld;
    FParsed := true;
  end else begin
//    CoInitialize(nil);

    try

      doc := MSXML2_TLB.CoDOMDocument60.Create;
//      doc := CoDomDocument.Create;
      try

        doc.async := false;

        if not doc.loadXML(FValue) then begin
          if not IgnoreErrors then begin
//            SaveStringAsFile('h:\poop.xml', FValue);
            raise XMLPArseException.create('XML load failed '+doc.parseError.reason)
          end else
            exit;
        end;

        elem := doc.documentElement;
        myElem := self.AddChild;
        myElem.ElementName := elem.tagName;

        ParseSub(doc, elem, myElem);
      finally

        doc := nil;
      end;


    finally
      FParsed := true;
//      CoUninitialize;
    end;
  end;

end;

procedure TXMLDocument.ParseSub(doc: IXMLDOMDocument2;
  elem: IXMLDOMElement; myElem: TXMLElement);
var
  t: integer;
  att: IXMLDOMNamedNodeMap;
  node: IXMLDOMNode;
  newelem: IXMLDOMElement;
  NewMyElem: TXMLElement;
  j: integer;
begin
  inherited;

  myElem.ElementName := elem.tagName;
//  myElem.Value := elem.text;

  myElem.Value := RemoveOuterTag(elem.xml);


  //att := nil;
  att := elem.attributes;

  if att <> nil then
  for t:= 0 to att.length-1 do begin
    myElem.Attributes.Add(att.item[t].nodeName, att.item[t].nodeValue);
  end;
  att := nil;

  for t:= 0 to elem.childNodes.length-1 do begin

    node := elem.childNodes.item[t];
    if not assigned(node) then
      continue;
    if node.QueryInterface(IXMLDOMElement, newelem) = S_OK then begin
      NewMyElem := myElem.AddChild;
      self.PArseSub(doc, newelem, NewmyElem);
    end;
  end;

end;


procedure TXMLDocument.SaveToFile(sFile: string);
var
  sl: TstringList;
begin
  sl := TStringLIst.create;
  try

    sl.text := self.ExportChildren;
    sl.SaveToFile(sFile);
  finally
    sl.free;
  end;

end;

{ TXMLBuilder }

procedure TXMLBuilder.AddBody(sText: string);
//p: sText: The string to add to the output.
//Adds the given text to the XML already stored by the builder.  Basically a simple concatination.
begin
  FXML := FXML+sText;
end;

procedure TXMLBuilder.Close(sElementName: string);
begin
  CloseElement(sElementName);
end;

procedure TXMLBuilder.CloseElement(sElementName: string);
//p: sElementName: The name of the element to close.
//Generates a CLOSED XML tag of given name.
begin
  FXML := FXML+'</'+sElementName+'>'+#13#10;

end;

constructor TXMLBuilder.Create;
//A standard constructor.
begin
  inherited Create;
  FXML := '';
end;
//------------------------------------------------------------------------------
procedure TXMLBuilder.Open(sElementName: string; bClose: boolean);
begin
  OpenSimpleElement(sElementName, bClose);
end;

procedure TXMLBuilder.Open(sElementName: string;
  attributes: array of variant; bClose: boolean);
//p: sElementName: The tag name fo the element to open.
//p: attributes: A variant array containing the names and values of the attributes of the tag.
//p: bClose: boolean indicating whether or not this tag should be immediately closed (generates a single tag for open and close all-in-one)
//Generates a standard XML tag element with attributes. If bClose=true then
//the tag will have a / before the closing brackets indicating that the element
//has no subelements and no body.
var
  sAdd: string;
  t: integer;
begin

  if length(attributes) mod 2 <> 0 then
    raise Exception.create(self.classname+'.OpenElement requires an even number of entries passed in the ''attributes'' parameter array');

  sAdd := '<'+sElementName;

  for t:=0 to length(attributes)-1 do begin
    if (t mod 2) <> 0 then
      continue;
    sAdd := sAdd+' '+VarToStr(Attributes[t])+'="'+VarToStr(Attributes[t+1])+'"';
  end;

  if bClose then
    sAdd := sAdd+'/>'
  else
    sAdd := sAdd+'>';

  FXML := FXML+sAdd;

end;

procedure TXMLBuilder.OpenElement(sElementName: string;
  attributes: variant; bClose: boolean = false);
//p: sElementName: The tag name fo the element to open.
//p: attributes: A variant array containing the names and values of the attributes of the tag.
//p: bClose: boolean indicating whether or not this tag should be immediately closed (generates a single tag for open and close all-in-one)
//Generates a standard XML tag element with attributes. If bClose=true then
//the tag will have a / before the closing brackets indicating that the element
//has no subelements and no body.
var
  sAdd: string;
  t: integer;
begin
  //showmessage(inttostr(High(Attributes)-Low(Attributes)));
  if VarArrayDimCount(attributes) = 0 then
    raise Exception.create('No variant Array passed to Open Element');

  if (VarArrayHighBound(Attributes,1)-VarArrayLowBound(Attributes,1)+1) mod 2 <> 0 then
    raise Exception.create(self.classname+'.OpenElement requires an even number of entries passed in the ''attributes'' parameter array');

  sAdd := '<'+sElementName;

  for t:=VarArrayLowBound(Attributes,1) to VarArrayHighBound(Attributes,1) do begin
    if (t-VarArrayLowBound(Attributes,1)) mod 2 <> 0 then
      continue;
    sAdd := sAdd+' '+VarToStr(Attributes[t])+'="'+VarToStr(Attributes[t+1])+'"';
  end;

  if bClose then
    sAdd := sAdd+'/>'
  else
    sAdd := sAdd+'>';

  FXML := FXML+sAdd;

end;
//------------------------------------------------------------------------------
procedure TXMLBuilder.OpenSimpleElement(sElementName: string; bClose: boolean);
//p: sElementName: The tag name fo the element to open.
//p: bClose: boolean indicating whether or not this tag should be immediately closed (generates a single tag for open and close all-in-one)
//Generates a simple XML tag element with NO attributes.  The syntax for this function
//is easier to use than TXMLBuilder.openElement but it does not support attributes of the tag.
var
  sAdd: string;
begin

  sAdd := '<'+sElementName;

  if bClose then
    sAdd := sAdd+'/>'
  else
    sAdd := sAdd+'>';

  FXML := FXML+sAdd;

end;
//------------------------------------------------------------------------------
function TXMLElement.FindElement(sName: string;
  attributematch: variant): TXMLElement;
var
  t: integer;
  u: integer;
  bMatchAll: boolean;
  sTemp1, sTemp2: string;
  sSearchFor: string;
begin
  if not Parsed then
    parse;

  sName := lowercase(sName);
  result := nil;
  for t:= 0 to ElementCount-1 do begin
    if lowercase(ElementsByIndex[t].ElementName)=sName then begin
      if VArType(attributematch) = varnull then
        result := ElementsByIndex[t]
      else begin
        bMatchAll := true;
        for u := VarArrayLowBound(attributematch, 1) to VarArrayLowBound(attributematch, 1) do begin
          if ((u-VarArrayLowBound(attributematch, 1)) mod 2) = 0 then begin
            //if attribute match then all is good
            if ElementsByIndex[t].Attributes.Has(varToStr(attributematch[u])) then begin
              sSearchFor := attributematch[u];
              sTemp1 := lowercase(ElementsByIndex[t].Attributes.Items[sSearchFor].Value);
              sTemp2 := lowercase(varToStr(attributematch[u+1]));
              if sTemp1 = sTemp2 then begin
                //do nothing
              end else begin
                bMatchAll := false;
                break;
              end;
            end;
          end;
        end;

        //if we get here and bMatchALl is still true set the result
        if bMatchAll then begin
          result := ElementsByIndex[t];
          break;
        end;
      end;
    end;
  end;

end;



function TXMLElement.ReadValue(sValueAddr: string): string;
var
  sLEft, sRight, sElementName, sAttributeName: string;
  elem: TXMlElement;
  i, i1, i2,i3: integer;
  sAttributeMatch: string;
  sAttributeValue: string;
begin
  result := '';
  if copy(sValueAddr, 1, 1) = ':' then
    sValueAddr := copy(sValueAddr, 2, length(sValueAddr));
  (*if copy(sValueAddr, 1, 1) = '.' then
    sValueAddr := copy(sValueAddr, 2, length(sValueAddr));*)

  result := '';
  i1 := pos(':', sValueAddr);
  i2 := pos('.', sValueAddr);
  i3 := pos('[', sValueAddr);

  //substitute '[' for '.' for attribute matches
  if (i3<i2) or (i2=0) then begin
    if i3>0 then
      i2 := i3;

  end;

  //determine if to use ':' or '.' or '['
  if (i1<i2) and (i1>0) then begin
    i := i1
  end else begin
    if i2>0 then
      i := i2
    else
      i := i1;

  end;

  //if no ':' or '.' return value
  if (i1 = i2) and (i1 < 1) then begin
    if copy(sValueAddr, length(sValueAddr),1) = '*' then begin
      result := inttostr(self.GetElementNameCount(copy(sValueAddr, 1, length(sValueAddr)-1)));
    end else begin
      result := '';
      if self.HasElement(sValueAddr) then
        result := self.Elements[sValueAddr,0].value
    end;
  end
  else
  //if separated by ':' then get sub element
  if i=i1 then begin
    SplitString(sValueAddr, ':', sLeft, sRight);

    if self.haselement(sLeft) then begin
      result := self.elements[sLeft,0].ReadValue(sRight);
    end;
  end else
  //if separated by '.' then read attribute
  if (i=i2) and (i2<>i3)then begin
    SplitString(sValueAddr, '.', sLeft, sRight);

    if (sLEft = '') then begin
      if self.Attributes.Has(sRight) then
        result := self.Attributes[sRight].Value;
    end else
    if self.HasElement(sLEft) and (self.Elements[sLeft,0].attributes.has(sRight)) then begin
      result := self.Elements[sLeft,0].attributes[sRight].Value
    end;
  end else
  //advanced array lookup
  begin
    SplitString(sValueAddr, '[', sElementName, sRight);
    SplitString(sRight, ']', sAttributeMatch, sRight);
    //if a attribute match query is specified then find it
    if SplitString(sAttributeMatch,'=', sAttributeNAme, sAttributeValue) then begin
      elem := FindElement(sElementName, VarArrayOf([sAttributeNAme, sAttributeValue]));
      if elem <> nil then begin
        result := elem.ReadValue(sRight);
      end;
    end
    //else assume the node is being referenced by numeric index
    else begin
//      try
          result := self.elements[sElementNAme, strtoint(sAttributeMatch)].ReadValue(sRight);
//      except
//        on E:Exception do begin

//        end;
//        raise Exception.create('Non numeric attribute value passed to ReadValue: '''+sAttributeMatch+'''');
//        result := '';
//      end;
    end;
  end;



end;

function TXMLElement.ReadNumberValue(rDEfault: real;
  sValueAddr: string): real;
var
  sTemp: string;
begin
  try
    sTemp := readValue(sValueAddr);
    result := strtofloat(sTEmp);
  except
    result := rDefault;
  end;
end;

function TXMLElement.ReadIntegerValue(iDEfault: integer;
  sValueAddr: string): integer;
var
  sTemp: string;
begin
  try
    sTemp := readValue(sValueAddr);
    result := strtoint(sTEmp);
  except
    result := iDefault;
  end;
end;

procedure TXMLBuilder.SaveToFile(sFile: string; bForceDirectory: boolean);
var
  sl: TStringLIst;
begin
  sl := TStringList.create;
  try
    sl.text := self.xml;

    if bForceDirectory then
      ForceDirectories(ExtractFilePath(sFile));

    sl.SaveToFile(sFile);
  finally
    sl.free;
  end;

end;

function TXMLElement.GetValue: string;
var
  t: integer;
begin
  if UseNativePArser then begin
    if not Parsed then
      result := FValue
    else begin
      result := '';
      for t:= 0 to ElementCount-1 do begin
        result := result + self.ElementsByIndex[t].Value;
      end;
    end;
  end else begin

    result := FValue;
  end;
end;

function RemoveOuterTag(sXML: string): string;
var
  t: integer;
  iStart, iEnd: integer;
  bInQuotes: boolean;
begin

  iStart := 1;
  iend := 0;
  bInQuotes := false;

  for t:= 1 to length(sXML) do begin
    case sXML[t] of
      '"': bInQuotes := not bInQuotes;
      '>': begin
        iStart := t+1;
        break;
      end;
    end;
  end;

  for t:= length(sXML) downto 1 do begin
    case sXML[t] of
      '"': bInQuotes := not bInQuotes;
      '<': begin
        iEnd := t-1;
        break;
      end;
    end;
  end;

  result := copy(sXML, iStart, (iEnd-iStart)+1);

end;

{ TXMLAttributes }

constructor TXMLAttributes.Create;
begin
  inherited;

end;

destructor TXMLAttributes.Destroy;
begin
  inherited;
end;

function TXMLElement.GetIntegerValue: integer;
begin
  try
    result := strtoint(Value);
  except
    raise exception.create('Error reading element '''+self.ElementName+''' value could not be converted to an integer '''+Value+'''');
  end;

end;

procedure TXMLElement.SetIntegerValue(const Value: integer);
begin
  //Value := inttostr(value);
end;

{ Tcmd_PArseXML }

procedure Tcmd_PArseXML.DoExecute;
begin
  inherited;
  result := TXMLDocument.create;
  result.IgnoreErrors := true;
  result.value := value;
  result.parse;




end;

procedure Tcmd_PArseXML.InitExpense;
begin
  inherited;
  icon := @cmd_icon_xml;
end;

initialization
  GElementCount := 0;

end.

