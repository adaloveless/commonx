unit docgen;

//where you left off:
//bump
// You just figured out how to bring Type parameters of funcitons <T> into the function
//signature on HTML output
// you also figured out how to parse generic classes.. at least through the header
//you also just built a namespace hierarchy that follows the context stack
// with functions like GetFullyQualifiedName ... etc.


//what you probably still haven't done
//- namespace hierarchy's will probably affect your ability to resolve symbols...
//  .. I'm pretty sure we can't resolve symbols as a result, but that could be fixed
//- those generic <T> functions are probably not very parsable in the implementation section... who knows
//- parameterized types are not resolvable (will they ever be?)
//- It appears that the parameterized types don't show up in the class title



//define documentation for all symbol objects
//define class members for class definitions
//TODO: Define functions as symbols.
//TODO 1: Make it so that simple types don't show up as classes





interface

uses
  diagram, classes, sysutils, dialogs, variants, systemx, stringx, numbers, debug, webstring;


type
  TDocSection = class(TStringList)
  end;

  TDocument = class(TStringList)
  private
    Ftop, fbottom: TdocSection;
    function GetSectionCount: integer;
    function GetSections(idx: integer): TDocSection;
    procedure SetSections(idx: integer; const Value: TDocSection);
    function GetTOP: TDocSection;
    function GetBottom: TDocSection;
  public
    constructor create; reintroduce; virtual;
    destructor destroy;override;
    property TOP: TDocSection read GetTOP;
    property Bottom: TDocSection read GetBottom;
    function AddSection(sTitle: ansistring): TDocSection;
    property Sections[idx: integer]: TDocSection read GetSections write SetSections;
    property SectionCount: integer read GetSectionCount;
  end;

function ProjectToHTML(sOutputDir: ansistring; oProject: TUMLproject): ansistring;
function GetUnitXML(oProject: TUMLProject; unitDef: TUnitDefinition): ansistring;
function CompileXMLIndex(sDir: ansistring): ansistring;
function GetUnitIndex(oProject: TUMLProject): ansistring;
function GetClassIndex(oProject: TUMLProject): ansistring;
function GetClassTree(oProject: TUMLProject): ansistring;
function GetSymbolIndex(oProject: TUMLProject): ansistring;
function GetUnit(oProject: TUMLProject; unitDef: TUnitDefinition): ansistring;
function GetClass(oProject: TUMLProject; classdef: TMatureConstruct): ansistring;
function AdjustPath(s: ansistring): ansistring;
procedure DocHead(sl: TStringList; sTitle: ansistring);
procedure DocFoot(sl: TStringList);


implementation

uses XMLTools, DirFile, dir;

function AdjustPath(s: ansistring): ansistring;
begin
  result := s;
end;

//------------------------------------------------------------------------------
function ProjectToHTML(sOutputDir: ansistring; oProject: TUMLproject): ansistring;
var
  sl : TStringList;
  u: integer;
  sym: TSymbol;
  unitDef: TUnitDefinition;
begin
  sl := TStringList.create;
  try
    //SORT EVERYTHING
    oProject.SYmbols.sort;
    for u := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[u];
      if sym is TStructure then
        TStructure(sym).Sort;

      if sym is TUnitDefinition then begin
        unitDef := sym as TUnitDefinition;
        unitDef.sort;
      end;
    end;


    sl.text := GetUnitIndex(oProject);
    sl.SaveToFile(adjustpath(sOutputDir)+'\unitindex.html');

    sl.text := GetClassIndex(oProject);
    sl.SaveToFile(adjustpath(sOutputDir)+'\classindex.html');

    sl.text := GetClassTree(oProject);
    sl.SaveToFile(adjustpath(sOutputDir)+'\classtree.html');


    //3. Generate Individual Units
    Debug.Log('Project has '+inttostr(oProject.Symbols.Count)+' symbols');
    for u := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[u];
      sym.Children.Sort;

      if sym is TStructure then
        TStructure(sym).Sort;

      if sym is TUnitDefinition then begin
        unitDef := sym as TUnitDefinition;
        sl.text := GetUnit(oProject, unitDef);
        sl.SaveToFile(adjustpath(sOutputDir)+'\'+unitdef.Name+'.html');

        sl.text := unitDef.Debughtml;
        sl.SaveToFile(adjustpath(sOutputDir)+'\'+unitdef.Name+'.debug.html');
      end;
    end;



  finally
    result := sl.text;
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
function GetUnitIndex(oProject: TUMLProject): ansistring;
var
  sl : TStringList;
  t: integer;
  sym: TSymbol;
begin
  sl := TStringList.create;
  try
    //--------------------------------------------------------------------------
    //1. Generate UnitIndex.html
    DocHead(sl, 'Unit Index');
    sl.add('This is a list of units in the program.  Click on a unit to get details.<BR>');
    sl.add('Or <a href="ClassIndex.html">View the Index of Classes</a><br><BR>');

    for t := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[t];
      if sym is TUnitDefinition then begin
        sl.add('<a href="GetUnit.ms?symbol='+sym.Name+'">'+sym.Name+'</a>');
        sl.add(' (<a href="'+sym.Name+'.debug.html">'+'Debug'+'</a>)<BR>');
      end;
    end;
    Docfoot(sl);
  finally
    result := sl.text;
    sl.free;
  end;
end;
//------------------------------------------------------------------------------
function GetClassIndex(oProject: TUMLProject): ansistring;
var
  sl : TStringList;
  t: integer;
  sym: Tsymbol;
begin
  sl := TStringList.create;
  try
    DocHead(sl, 'Class Index ');
    sl.add('This is a list of classes in the program.  Click on a class to get details, or click on a unit to open the containing unit.<BR><BR>');
    for t := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[t];
      if (sym is TMatureConstruct) then begin
        if sym.Parent = nil then begin
          //showmessage('symbol ' +sym.name+' has no parent unit');
          continue;
        end;
        sl.add('<a href="'+sym.Parent.Name+'.html#'+sym.Name+'">'+sym.Name+'</a> in Unit <a href="'+sym.Parent.Name+'.html">'+sym.Parent.Name+'</a><BR>');
        sl.add(sym.Documentation.text+'<BR>');
      end;
    end;
    Docfoot(sl);
  finally
    result := sl.text;
    sl.free;
  end;
end;


function GetSymbolIndex(oProject: TUMLProject): ansistring;
var
  sl : TStringList;
  t: integer;
  sym: Tsymbol;
begin
  sl := TStringList.create;
  try
    DocHead(sl, 'Class Index ');
    sl.add('This is a list of classes in the program.  Click on a class to get details, or click on a unit to open the containing unit.<BR><BR>');
    for t := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[t];
//      if (sym is TMatureConstruct) then begin
        if sym.Parent = nil then begin
          sl.add('<a href="'+sym.Parent.Name+'.html#'+sym.Name+'">'+sym.Name+'</a> in Unit (no parent)<BR>');
          sl.add(sym.Documentation.text+'<BR>');
        end else begin
          sl.add('<a href="'+sym.Parent.Name+'.html#'+sym.Name+'">'+sym.Name+'</a> in Unit <a href="'+sym.Parent.Name+'.html">'+sym.Parent.Name+'</a><BR>');
          sl.add(sym.Documentation.text+'<BR>');

        end;
      end;
//    end;
    Docfoot(sl);
  finally
    result := sl.text;
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
function GetUnit(oProject: TUMLProject; unitDef: TUnitDefinition): ansistring;
var
  t,u: integer;
  sl : TStringList;
  sym: TSymbol;
begin
  sl := TStringList.create;
  try
    DocHead(sl, unitdef.Name);
    sl.Add('<blockquote>');
    sl.Add(unitdef.Documentation.text);
    sl.Add('</blockquote>');

    //table of contents
    for t := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[t];
      if (sym is TClassDefinition) and (sym.Parent=unitdef)then begin
        sl.add('<b>'+copy(sym.classname,2,9999)+'</b><a href="'+sym.Parent.Name+'.html#'+sym.Name+'">'+sym.Name+'</a><BR>');
      end;
    end;

    sl.add('<BR><BR><BR>');
    sl.add('<hr>');
    //Unit functions
    if unitdef.FunctionCount > 0 then begin
      sl.Add('<table class="main">');
      sl.Add('<TR><TD colspan=2 class="subheading"><B>Unit Functions (In Unit Namespace)</B></TD></TR>');
      for t := 0 to unitdef.FunctionCount -1 do begin
        sl.Add('<TR>');
        sl.Add('<!function link/><TD colspan=1 class="plain" width="10" align=TOP valign=TOP><a href="'+unitdef.name+'.html#'+unitdef.Functions[t].Name+'">'+unitdef.functions[t].Name+'</a></TD>');
        sl.Add('<!signature/><TD class="plain"><I><span class="code">'+ampencode(unitdef.Functions[t].Signature)+'</span></I><BR></TD>');
        sl.Add('</TR>');
      end;
      sl.add('</table>');
    end;

    //Individual classes

    //for every symbol:
    for t := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[t];
      //if the symbol is a class definition and the parent is the current unit:
      if (sym is TMatureConstruct) and (sym.Parent=unitdef)then begin
        //document
        sl.add(GetClass(oProject, sym as TMatureConstruct));
      end;
    end;

    //Unit function Details
    if unitdef.FunctionCount > 0 then begin
      for t := 0 to unitdef.FunctionCount -1 do begin
        if unitdef.functions[t].Documentation.count>0 then begin
          sl.Add('<hr>');
          sl.Add('<span class="classtitle"><a name="'+unitDef.Functions[t].Name+'"></a></span>');
          //sl.Add('<!function link/><TD colspan=1 class="main" width="10" align=TOP valign=TOP><a href="'+unitdef.name+'.html#'+unitdef.Functions[t].Name+'">'+unitdef.functions[t].Name+'</TD>');
          sl.Add('<table class="main">');
          sl.Add('<TR><TD colspan=1 class="classtitle"><B><span class="plain">'+unitDef.Name+'.'+unitDef.Functions[t].Name+'</span><BR><font size="-2">Signature:<BR></font></B><I><span class="PLAIN"><B><I>'+ampencode(unitdef.Functions[t].Signature)+'</I></B></span></I></TD></TR>');
          if unitdef.functions[t].Author<>'' then
            sl.Add('<TR><TD colspan=1 class="plain"><B>Author:</b>'+unitdef.functions[t].Author+'</TD></TR>');
          sl.Add('<TR><TD class="main"><BR>');
          if unitdef.functions[t].Parameters.Count > 0 then begin
            sl.add('<table bgcolor="#EFEFEF"><TR><TD colspan=1 width="200"><B>Parameter</I></B></TD><TD><B>Description<B></TD></TR>');
            for u := 0 to unitdef.functions[t].parameters.count-1 do begin
              sl.Add('<TR>');
              sl.add('<TD><I>');
              sl.Add(unitdef.functions[t].Parameters.ItemsByIndex[u].Name);
              sl.add('</I></TD>');
              sl.add('<TD>');
              sl.Add(unitdef.functions[t].Parameters.ItemsByIndex[u].value);
              sl.add('</TD>');
              sl.Add('</TR>');
            end;
            sl.add('</table>');
          end;
          sl.Add('<blockquote>');
          sl.Add(unitdef.functions[t].Documentation.text);
          sl.Add('</blockquote>');
          sl.Add('</TD></TR>');
          sl.add('</table>');
        end;
      end;

    end;
    Docfoot(sl);

  finally
    result := sl.text;
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
function GetUnitXML(oProject: TUMLProject; unitDef: TUnitDefinition): ansistring;
var
  t,u: integer;
  sym: TSymbol;
  xmlb: TXMLBuilder;
begin
  xmlb := TXMLBuilder.create;
  try
    xmlb.OpenElement('Unit', VarArrayof(['Name',unitdef.name]));

      //for all symbols that are class definitions and list this unit as the parent
      for t := 0 to oProject.Symbols.Count-1 do begin
        sym := oProject.Symbols[t];
        if (sym is TClassDefinition) and (sym.Parent=unitdef)then begin
          xmlb.OpenElement('Class', VarArrayOf(['name',sym.Name]), true);
        end;
      end;

      //for all functions in the unit
      for t := 0 to unitdef.FunctionCount -1 do begin
        xmlb.OpenElement('Function', VarArrayOf([
                          'Name', unitdef.Functions[t].Name,
                          'Author', unitdef.Functions[t].Author,
                          'WebPage', unitdef.Functions[t].WebPage,
                          'InType', unitdef.Functions[t].InType,
                          'OutType', unitdef.Functions[t].OutType,
                          'Visibility', unitdef.Functions[t].Visibility,
                          'Integration', unitdef.Functions[t].IntegrationPackage
                          ]));

          for u:= 0 to unitdef.Functions[t].Parameters.Count-1 do begin
            xmlb.OpenElement('Parameter', VarArrayOf(['Name', unitdef.Functions[t].Parameters.ItemsByIndex[u].Name, 'Value', unitdef.Functions[t].Parameters.ItemsByIndex[u].Value]), true)
          end;

        xmlb.Close('Function');

      end;


    xmlb.CloseElement('Unit');
  finally
    result := xmlb.xml;
    xmlb.free;

  end;
end;

//------------------------------------------------------------------------------
function GetClass(oProject: TUMLProject; classdef: TMatureConstruct): ansistring;
var
  t,u: integer;
  sl : TStringList;
begin
  sl := TStringList.create;
  try
    sl.add('<hr>');
    sl.Add('<a name="'+classDef.Name+'"></a>');
    sl.Add('<table class="main">');
    sl.Add('<TR><TD colspan=2 class="classtitle"><B>'+classDef.Name+ampencode(classdef.typeparameters)+'('+classDef.AncestorList+')</B></TD></TR>');
    if classdef.author <> '' then begin
       sl.Add('<TR><TD colspan=2 class="plain"><B>Author:</b>'+classdef.Author+'</TD></TR>');
    end;
    if classdef.Responsibilities.count > 0 then begin
      sl.Add('<TR><TD colspan=2 class="subheading"><B>Responsibilities</B></TD></TR>');
      for t := 0 to classdef.Responsibilities.count -1 do begin
        sl.Add('<TR><TD colspan=1 class="main" width="10">'+inttostr(t+1)+'.</TD><TD class="main">'+classdef.Responsibilities[t]+'</TD></TR>');
      end;
    end;
    sl.Add('</table>');

    //if classDef.Documentation.text = '' then
    //  classDef.documentation.text := '<font color="FF0000">Undocumented</font>';

    if classDef.documentation.text <> '' then begin
      sl.Add('<table class="main">');
      sl.Add('<TR><TD colspan=2 class="subheading"><B>Notes</B></TD></TR>');
      sl.Add('<TR><TD colspan=2 class="main"><blockquote>'+classDef.documentation.text+'</blockquote></TD></TR>');
      sl.Add('</table>');
    end;

    //Properties
    if classdef.PropertyCount > 0 then begin
      sl.Add('<table class="main">');
      sl.Add('<TR><TD colspan=2 class="subheading"><B>Properties</B></TD></TR>');
      for t := 0 to classdef.PropertyCount -1 do begin
        sl.Add('<TR>');
        sl.add('<!property link><TD  colspan=1 class="main" width="10" align=TOP valign=TOP><a href="'+classdef.parent.name+'.html#'+classdef.name+'.'+classdef.properties[t].Name+'">'+classdef.Properties[t].Name+'</TD>');
        sl.add('<!signature><TD class="main" bgcolor="#DFDFDF"><I>'+ampencode(classdef.Properties[t].Signature)+'</I></TD>');
        if classdef.properties[t].Documentation.text = '' then
          classdef.properties[t].Documentation.text := '<font color="FF0000">Undocumented</font>';

        sl.add('<TR><TD></TD><TD>'+classdef.properties[t].Documentation.text+'</TD>');
        sl.add('</TR>');
      end;
      sl.add('</table>');
    end;


    //Functions
    if classdef.FunctionCount > 0 then begin
      sl.Add('<table class="main">');
      sl.Add('<TR><TD colspan=2 class="subheading"><B>Member Functions</B></TD></TR>');
      for t := 0 to classdef.FunctionCount -1 do begin
        sl.Add('<TR>');
        sl.Add('<!function link/><TD colspan=1 class="main" width="10" align=TOP valign=TOP><a href="'+classdef.parent.name+'.html#'+classdef.name+'.'+classdef.Functions[t].Name+'">'+classdef.functions[t].Name+'</a></TD>');
        sl.Add('<!signature/><TD class="main"><I>'+ampencode(classdef.Functions[t].Signature)+'</I><BR></TD>');
        sl.Add('</TR>');
      end;
      sl.add('</table>');
    end;

    //function Details
    if classdef.FunctionCount > 0 then begin
      for t := 0 to classdef.FunctionCount -1 do begin
        if classdef.functions[t].Documentation.count>0 then begin
          sl.Add('<hr>');
          sl.Add('<a name="'+classDef.Name+'.'+classDef.Functions[t].Name+'"></a>');
          sl.Add('<table class="main">');
          sl.Add('<TR><TD colspan=1 class="functiontitle"><B>'+classDef.Name+'.'+classDef.Functions[t].Name+'<BR>Signature:<BR></B><I><span class="plain"><B><I>'+classdef.Functions[t].Signature+'</I></B></span></I></TD></TR>');

          if classdef.functions[t].Author<>'' then
            sl.Add('<TR><TD colspan=1 class="plain"><B>Author:</b>'+classdef.functions[t].Author+'</TD></TR>');

          sl.Add('<TR><TD class="main"><BR>');
          if classdef.functions[t].Parameters.Count > 0 then begin
            sl.add('<table bgcolor="#EFEFEF"><TR><TD colspan=1 width="200"><B>Parameter</I></B></TD><TD><B>Description<B></TD></TR>');
            for u := 0 to classdef.functions[t].parameters.count-1 do begin
              sl.Add('<TR>');
              sl.add('<TD><I>');
              sl.Add(classdef.functions[t].Parameters.ItemsByIndex[u].Name);
              sl.add('</I></TD>');
              sl.add('<TD>');
              sl.Add(classdef.functions[t].Parameters.ItemsByIndex[u].value);
              sl.add('</TD>');
              sl.Add('</TR>');
            end;
            sl.add('</table>');
          end;

          sl.Add('<blockquote>');
          if classdef.functions[t].Documentation.text ='' then
            classdef.functions[t].Documentation.text := '<font color="FF0000">Undocumented</font>';

          sl.Add(classdef.functions[t].Documentation.text);
          sl.Add('</blockquote>');
          sl.Add('</TD></TR>');

          sl.add('</table>');
        end;
      end;
    end;
  finally
    result := sl.text;
    sl.free;
  end;
end;

procedure DocHead(sl: TStringList; sTitle: ansistring);
begin
  sl.add('<html>');
  sl.add('<head>');
  sl.add('<title>'+sTitle+'</title>');
  sl.add('<meta http-equiv="Content-Type" content="text/html; AnsiCharset=iso-8859-1">');
  sl.add('<style type="text/css">');
  sl.add('<!--');
  sl.add('.functiontitle {  font-family: helvetica, Arial; font-size: 14pt; font-style: normal; border:1; valign:top; bgcolor:#F0F0F0}');
  sl.add('.main {  font-family: Times; font-size: 12pt; font-style: normal; border:1; valign:top; bgcolor:#F0F0F0}');
  sl.add('.plain {  font-family: Times; font-size: 12pt; font-style: normal; border:1; valign:top; bgcolor:#F0F0F0}');
  sl.add('.subheading {  font-family: helvetica, Arial; font-size: 14pt; font-style: bold; border:0;width:80%}');
  sl.add('.responsibility {  font-family: helvetica, Arial; font-size: 12pt; font-style: bold; border:0;width:100}');
  sl.add('.unittitle {  font-family: helvetica, Arial; font-size: 25pt; font-style: normal; border:0;width:80%}');
  sl.add('.classtitle {  font-family: helvetica, Arial; font-size: 17pt; font-style: bold; border:0;width:80%}');
  sl.add('.code {  font-family: courier; font-size: 7pt; font-style: normal; border:0;width:80%}');
  sl.add('-->');
  sl.add('</style>');
  sl.add('</head>');
  sl.add('<body class="unittitle">'+sTitle+'<BR>');
  sl.add('<table width = 640 class="plain"><tr><td>');

end;
//------------------------------------------------------------------------------
procedure DocFoot(sl: TStringList);
begin
  sl.add('</td></tr></table>');
  sl.add('</body>');
  sl.add('</html>');
end;



{ TDocument }

function TDocument.AddSection(sTitle: ansistring): TDocSection;
begin

//TODO -cunimplemented: unimplemented block
end;

constructor TDocument.create;
begin

//TODO -cunimplemented: unimplemented block
end;

destructor TDocument.destroy;
begin

  inherited;
end;

function TDocument.GetBottom: TDocSection;
begin

//TODO -cunimplemented: unimplemented block
end;

function TDocument.GetSectionCount: integer;
begin

//TODO -cunimplemented: unimplemented block
end;

function TDocument.GetSections(idx: integer): TDocSection;
begin

//TODO -cunimplemented: unimplemented block
end;

function TDocument.GetTOP: TDocSection;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TDocument.SetSections(idx: integer; const Value: TDocSection);
begin

//TODO -cunimplemented: unimplemented block
end;

function CompileXMLIndex(sDir: ansistring): ansistring;
//Transforms a directory full of WebPage XML documents into a single XML index of the
//documents... indexed by web page.
  function Compare1(item1,item2: pointer): integer;
  var
    d1,d2: TXMLDocument;
    s1,s2: ansistring;

  begin
    d1 := TXMLdocument(item1);
    d2 := TXMLdocument(item2);
//    try
      s1 := lowercase(d1.ReadValue('message:category')+d1.ReadValue('message:package')+d1.ReadValue('message:Responsibility')+d1.ReadValue('message:title'));
//    except
//    end;
//    try
      s2 := lowercase(d2.ReadValue('message:category')+d2.ReadValue('message:package')+d1.ReadValue('message:Responsibility')+d2.ReadValue('message:title'));
//    except
//      on E: Exception do begin
//        raise Exception.create(
//      end;
//    end;

    if  s1 > s2 then
      result := 1;

    if  s1 < s2 then
      result := -1;

    if  s1 = s2 then
      result := 0;

  end;
var
  xmld: TXMLDocument;
  dir: TDirectory;
  t: integer;
  fil: TFileInformation;
  l: TLIst;
  xmlb: TXMLBuilder;
begin


  xmld := nil;
  dir := nil;
  l := nil;
  xmlb := nil;
  try

    dir := TDirectory.Create(sDir, '*.xml', 0,0, false);
    xmlb := TXMLBuilder.create;
    l := TList.create;

    while dir.GetNextFile(fil) do begin
      xmld := TXMLDocument.create;
      xmld.LoadFromFile(fil.fullname);
      l.add(xmld);
    end;

    l.Sort(@Compare1);

    xmlb.OpenSimpleElement('stuff');
    for t:= 0 to l.Count-1 do begin
      xmld := TXMLDocument(l[t]);
      xmlb.OpenElement('message', VarArrayOf([
        'Title',    xmld.ReadValue('message:Title'),
        'Page',     xmld.ReadValue('message:Page'),
        'Function', xmld.ReadValue('message:Function'),
        'Category', xmld.ReadValue('message:Category'),
        'Responsibility', xmld.ReadValue('message:Responsibility'),
        'Package', xmld.ReadValue('message:Package')]),
         true);
    end;
    xmlb.close('stuff');


    result := xmlb.xml;
  finally

    FreeLIstContents(l);
    l.free;
    dir.free;
    xmlb.free;
  end;



end;


function GetClassTree(oProject: TUMLProject): ansistring;
var
  sl : TStringList;
  t: integer;
  sym: Tsymbol;
begin
  sl := TStringList.create;
  try
    DocHead(sl, 'Class Tree ');
    sl.add('This is a list of symbols in the program.  Click on a class to get details, or click on a unit to open the containing unit.<BR><BR>');
    for t := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[t];
//      if (sym is TMatureConstruct) then begin
        if sym.Parent = nil then begin
          //showmessage('symbol ' +sym.name+' has no parent unit');
          sl.add('<a href="'+sym.Name+'">'+sym.Name+'</a><br>');
          continue;
        end;
        sl.add('<a href="'+sym.Parent.Name+'.html#'+sym.Name+'">'+sym.Name+'</a> in Parent <a href="'+sym.Parent.Name+'.html">'+sym.Parent.Name+'</a><BR>');
        sl.add(sym.Documentation.text+'<BR>');
//      end;
    end;
    Docfoot(sl);
  finally
    result := sl.text;
    sl.free;
  end;
end;


end.

