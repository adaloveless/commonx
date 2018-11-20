unit docgen2;
//define documentation for all symbol objects
//define class members for class definitions
//TODO: Define functions as symbols.


interface

uses
  diagram, classes, sysutils;


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
procedure DocHead(sec: TdocSection; sTitle: ansistring);
procedure DocFoot(sec: TDocSection);
procedure DocumentUnit(doc: TDocument; oProject: TUMLProject; unitdef: TUnitDefinition);
procedure DocumentClass(doc: Tdocument; oProject: TUMLProject; classdef: TClassDefinition);


implementation

uses stringutilities, MiscRoutines, webstring;

//------------------------------------------------------------------------------
function ProjectToHTML(sOutputDir: ansistring; oProject: TUMLproject): ansistring;
var
  t, u: integer;
  sl: TStringList;
  sym: TSymbol;
  unitdef: TUnitDefinition;

begin
  ForceDirectories(sOutputDir);

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
        sl.add('<a href="'+sym.Name+'.html">'+sym.Name+'</a><BR>');
      end;
    end;
    Docfoot(sl);

    sl.SaveToFile(adjustpath(sOutputDir)+'UnitIndex.html');
    sl.clear;

    //--------------------------------------------------------------------------

    //2. Generate ClassIndex.html
    DocHead(sl, 'Class Index ');
    sl.add('This is a list of classes in the program.  Click on a class to get details, or click on a unit to open the containing unit.<BR><BR>');
    for t := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[t];
      if sym is TClassDefinition then begin
        if sym.Parent = nil then
          raise Exception.create('symbol ' +sym.name+' has no parent unit');
        sl.add('<a href="'+sym.Parent.Name+'.html#'+sym.Name+'">'+sym.Name+'</a> in Unit <a href="'+sym.Parent.Name+'.html">'+sym.Parent.Name+'</a><BR>');
        sl.add(sym.Documentation.text+'<BR>');
      end;
    end;
    Docfoot(sl);

    sl.SaveToFile(adjustpath(sOutputDir)+'ClassIndex.html');
    sl.clear;

    //3. Generate Individual Units
    for u := 0 to oProject.Symbols.Count-1 do begin
      sym := oProject.Symbols[u];
      if sym is TUnitDefinition then begin
        unitDef := sym as TUnitDefinition;
        documentUnit(sl, oProject, unitDef);
        sl.SaveToFile(adjustpath(sOutputDir)+unitdef.Name+'.html');
        sl.clear;

      end;
    end;

  finally
    result := sl.text;
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
procedure DocumentUnit(sl: TStringList; oProject: TUMLProject; unitdef: TUnitDefinition);
var
  t,u: integer;
  sym: TSymbol;
begin
  DocHead(sl, unitdef.Name);
  sl.Add('<blockquote>');
  sl.Add(unitdef.Documentation.text);
  sl.Add('</blockquote>');

  //table of contents
  for t := 0 to oProject.Symbols.Count-1 do begin
    sym := oProject.Symbols[t];
    if (sym is TClassDefinition) and (sym.Parent=unitdef)then begin
      sl.add('<b>Class:</b><a href="'+sym.Parent.Name+'.html#'+sym.Name+'">'+sym.Name+'</a><BR>');
    end;
  end;

  sl.add('<BR><BR><BR>');
  sl.add('<hr>');
  //Individual classes

  //for every symbol:
  for t := 0 to oProject.Symbols.Count-1 do begin
    sym := oProject.Symbols[t];
    //if the symbol is a class definition and the parent is the current unit:
    if (sym is TClassDefinition) and (sym.Parent=unitdef)then begin
      //document
      documentClass(sl, oProject, sym as TClassDefinition);
    end;
  end;


  //Unit functions
  if unitdef.FunctionCount > 0 then begin
    sl.Add('<table class="main">');
    sl.Add('<TR><TD colspan=2 class="subheading"><B>Unit Functions (In Unit Namespace)</B></TD></TR>');
    for t := 0 to unitdef.FunctionCount -1 do begin
      sl.Add('<TR>');
      sl.Add('<!function link/><TD colspan=1 class="plain" width="10" align=TOP valign=TOP><a href="'+unitdef.name+'.html#'+unitdef.Functions[t].Name+'">'+unitdef.functions[t].Name+'</TD>');
      sl.Add('<!signature/><TD class="plain"><I><span class="code">'+unitdef.Functions[t].Signature+'</span></I><BR></TD>');
      sl.Add('</TR>');
    end;
    sl.add('</table>');
  end;

  //Unit function Details
  if unitdef.FunctionCount > 0 then begin
    for t := 0 to unitdef.FunctionCount -1 do begin
      if unitdef.functions[t].Documentation.count>0 then begin
        sl.Add('<hr>');
        sl.Add('<a name="'+unitDef.Functions[t].Name+'"></a>');
        sl.Add('<table class="main">');
        sl.Add('<TR><TD colspan=1 class="subheading"><B><span class="plain">'+unitDef.Name+'.'+unitDef.Functions[t].Name+'</span><BR><font size="-2">Signature:<BR></font></B><I><span class="PLAIN"><B><I>'+unitdef.Functions[t].Signature+'</I></B></span></I></TD></TR>');
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

end;
//------------------------------------------------------------------------------
procedure DocumentClass(sl: TStringList; oProject: TUMLProject; classdef: TClassDefinition);
var
  t,u: integer;
begin
  sl.add('<hr>');
  sl.Add('<a name="'+classDef.Name+'"></a>');
  sl.Add('<table class="main">');
  sl.Add('<TR><TD colspan=2 class="classtitle">Class: '+classDef.Name+'</TD></TR>');
  if classdef.Responsibilities.count > 0 then begin
    sl.Add('<TR><TD colspan=2 class="subheading"><B>Responsibilities</B></TD></TR>');
    for t := 0 to classdef.Responsibilities.count -1 do begin
      sl.Add('<TR><TD colspan=1 class="main" width="10">'+inttostr(t+1)+'.</TD><TD class="main">'+classdef.Responsibilities[t]+'</TD></TR>');
    end;
  end;
  sl.Add('</table>');

  sl.Add('<table class="main">');
  sl.Add('<TR><TD colspan=2 class="subheading"><B>Notes</B></TD></TR>');
  sl.Add('<TR><TD colspan=2 class="main"><blockquote>'+classDef.documentation.text+'</blockquote></TD></TR>');
  sl.Add('</table>');

  //Properties
  sl.Add('<table class="main">');
  if classdef.PropertyCount > 0 then begin
    sl.Add('<TR><TD colspan=2 class="subheading"><B>Properties</B></TD></TR>');
    for t := 0 to classdef.PropertyCount -1 do begin
      sl.Add('<TR>');
      sl.add('<!property link><TD colspan=1 class="main" width="10" align=TOP valign=TOP><a href="'+classdef.parent.name+'.html#'+classdef.name+'.'+classdef.properties[t].Name+'">'+classdef.Properties[t].Name+'</TD>');
      sl.add('<!signature><TD class="main"><B><I>'+classdef.Properties[t].Signature+'</I></B><BR>');
      sl.add(classdef.properties[t].Documentation.text+'</TD>');
      sl.add('</TR>');
    end;
    sl.add('</table>');
  end;

  //Functions
  sl.Add('<table class="main">');
  if classdef.FunctionCount > 0 then begin
    sl.Add('<TR><TD colspan=2 class="subheading"><B>Member Functions</B></TD></TR>');
    for t := 0 to classdef.FunctionCount -1 do begin
      sl.Add('<TR>');
      sl.Add('<!function link/><TD colspan=1 class="main" width="10" align=TOP valign=TOP><a href="'+classdef.parent.name+'.html#'+classdef.name+'.'+classdef.Functions[t].Name+'">'+classdef.functions[t].Name+'</TD>');
      sl.Add('<!signature/><TD class="main"><I>'+classdef.Functions[t].Signature+'</I><BR></TD>');
      sl.Add('</TR>');
    end;
  end;
  sl.add('</table>');

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
        sl.Add(classdef.functions[t].Documentation.text);
        sl.Add('</blockquote>');
        sl.Add('</TD></TR>');

        sl.add('</table>');
      end;

    end;

  end;

end;

{ TDocument }

function TDocument.AddSection(sTitle: ansistring): TDocSection;
begin
  result := Tdocsection.create;
  self.AddObject(sTitle, result)
end;

constructor TDocument.create;
begin
  inherited create;
  FTOP := TDocsection.create;
  FBottom := TDocsection.create;


end;

destructor TDocument.destroy;
begin
  while sectioncount > 0 do begin
    sections[0].free;
    self.Delete(0);
  end;
  inherited;
end;

function TDocument.GetBottom: TDocSection;
begin
  reuslt := FBottom;
end;

function TDocument.GetSectionCount: integer;
begin
  result := self.count;
end;

function TDocument.GetSections(idx: integer): TDocSection;
begin
  result := TDocSection(self.objects[idx]);
end;

function TDocument.GetTOP: TDocSection;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TDocument.SetSections(idx: integer; const Value: TDocSection);
begin
  self.Objects[idx] := value;
end;

end.
