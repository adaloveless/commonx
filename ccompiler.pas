unit ccompiler;
//This unit contains classes for parsing object pascal source code. See individual units for details.

interface

uses exceptions, debug, contnrs, namevaluepair, sysutils, classes, d2c_project, stringx, stringx.ansi, d2c_pascalcompiler;

type
  TStringType = (stNull, stArray);



  //----------------------------------------------------------------------------
  TCCompiler = class(Tcompiler)
  //the Pascal compiler class is a compiler for the object Pascal language
  //this class is implemented the work with the irrational project only
  private
    FCurrentSymbol: Tsymbol;
    FCurrentUnit: TunitDefinition;
    FLastTypeName: ansistring;
    FCurrentSignature: ansistring;
    FCurrentProperty: ansistring;
    FCurrentFunctionDef: TFunctionDefinition;
    FCurrentScope: TScope;
    FStringType: TStringType;
    FStatic: boolean;
    fExtern: boolean;
    FTypeBuffer: ansistring;
    Fmovingthroughword: boolean;
    FmovingthroughSpace: boolean;
    FDocumentingSymbol: TSymbol;
    FCurrentStorageSymbol: ansistring;
    FCommentBuffer: ansistring;


    function GetCurrentClass: TMatureConstruct;
    procedure SetCurrentUnit(const Value: TunitDefinition);
    procedure SetCurrentClass(const Value: TMatureConstruct);
    procedure DispatchDefine;
    procedure DispatchFunctionParam;


  public
    constructor create; override;
    //--------------------------------------------------------------------------
    procedure DispatchContext;override;
    //--------------------------------------------------------------------------
    procedure DispatchRoot;
    procedure DispatchComment;
    procedure DispatchCodeComment;
    procedure DispatchLineComment;
    procedure DispatchUnit;
    procedure DispatchInterface;
    procedure DispatchImplementation;

    procedure DispatchConst;
    procedure DispatchTypeDefinition;
    procedure DispatchClassDefinitionHeader;
    procedure DispatchClassDefinition;
    procedure DispatchParens;
    procedure DispatchBrackets;
    procedure DispatchFunctionDeclaration;
    procedure DispatchPropertyDeclaration;
    procedure DispatchFunction;
    procedure DispatchCodeBlock;
    procedure DisPatchFunctionImpName;
    procedure DispatchVarConst;
    procedure DispatchString;
    procedure DispatchStorageSymbol;
    procedure DispatchDecorations;
    procedure DispatchStorageType;
    procedure DispatchDirective;
    procedure DispatchEnum;
    procedure DispatchRecordDefinition;
    procedure DispatchUses;
    procedure DispatchType;
    procedure DispatchStruct;
    procedure DispatchStructname;
    procedure DispatchStructDoc;


    //remote control functions
    procedure Doc(sString: ansistring);
    procedure DocNewLine;
    procedure FuncDoc(sString: ansistring);
    procedure FuncDocNewLine;
    procedure PropDoc(sString: ansistring);
    procedure PropDocNewLine;


    procedure RecognizeType(sName: ansistring);
    procedure CommitType(sName: ansistring);
    procedure CommitClass(sName: ansistring);
    procedure CommitFunction;
    procedure CommitProperty;
    procedure RecommitFunction;

    property CurrentProperty: ansistring read FCurrentProperty write FCurrentProperty;
    procedure FindFunction(sName: ansistring);

    function Compile(sFile: ansistring): boolean; override;

    property CurrentUnit: TunitDefinition read FCurrentunit write SetCurrentUnit;
    property DocumentingSymbol: TSymbol read FDocumentingSymbol write FDocumentingSymbol;
    property CurrentSymbol: Tsymbol read FCurrentSymbol write FCurrentSymbol;
    property CurrentClass: TMatureConstruct read GetCurrentClass write SetCurrentClass;
    property CurrentStruct: TMatureConstruct read GetCurrentClass write SetCurrentClass;
    property LastTypeName: ansistring read FLastTypeName write FLastTypeName;
    property CurrentSignature: ansistring read FCurrentSignature write FCurrentSignature;
    procedure DefineUnit(sName: ansistring);
    property CurrentScope: TScope read FCurrentScope write FCurrentScope;
    property ansistringType: TStringType read FStringType write FStringType;
    property CurrentStorageSymbol: ansistring read FCurrentStorageSymbol write FCurrentStorageSymbol;
    procedure ClearSymbol;
    procedure RootState;
    property flag_extern: boolean read fExtern write FExtern;
    property flag_static: boolean read FStatic write FStatic;

    function IsFlag: boolean;
    function IsKeyWord: boolean;
    property Typebuffer: ansistring read FTypeBuffer write FTypeBuffer;
    function GetBestParent: TSymbol;
    procedure MoveThroughWord;
    procedure MoveThroughSpace;
    property MovingThroughWord: boolean read Fmovingthroughword write FmovingThroughword;
    property MovingThroughSpace: boolean read FmovingthroughSpace write FmovingthroughSpace;
    property CommentBuffer: ansistring read FCommentBuffer write FCommentBuffer;
  end;

  //TCCompiler2 = class(Tcompiler)


const
  validnames = ['0'..'9','a'..'z','A'..'Z','_'];
  wordbreaks = [' ', '(',')','*','[',']','''','"','=','+','-','/','\','<','>','{','}',';',',','.',#10,#13,#0,#9];
  //oprerators = ['+', '-', '=', '=='];


implementation


{ TContextStack }


//------------------------------------------------------------------------------
procedure Register;
begin
//  RegisterComponents('Booger', [TCCompiler]);
end;
//------------------------------------------------------------------------------
constructor TCCompiler.create;
begin
  inherited;

  FcurrentUnit := nil;
  FCurrentSymbol := nil;
  FcurrentFunction := '';
  fCurrentUnit := nil;
  FCurrentSignature := '';
  self.FLastCommittedFunction := nil;
  self.FLastCommittedProperty := nil;

end;
//------------------------------------------------------------------------------
procedure TCCompiler.ClearSymbol;
begin
  CurrentSymbol := nil;
end;

procedure TCCompiler.CommitClass(sName: ansistring);
var
  sym: TMatureConstruct;
begin
  if sName ='' then
    exit;


  write('&lt;***TYPE COMMITED:'+sName+'***&gt;');

  if lowercase(sName) = lowercase(self.CurrentUnit.ClassName) then
    raise Exception.create('Class Name cannot match unit name: '+sName);


  sym := TMatureConstruct.create(Trim(sName), project, self.CurrentUnit);

  project.Symbols.Add(sym);

  ClearBuffer;
  self.LastTypeName := '';
  self.Currentsymbol := sym;

end;
//------------------------------------------------------------------------------
procedure TCCompiler.RecognizeType(sName: ansistring);
var
  sym: TSymbol;
begin
  if sName ='' then
    exit;

  if sName <> self.LastTypeName then begin
    write('<font color="#00007F">&lt;***TYPE RECOGNIZED:'+sName+'***&gt;</font>');
    self.LastTypeName := sName;
  end;
  ClearBuffer;

end;
//------------------------------------------------------------------------------
procedure TCCompiler.CommitType(sName: ansistring);
var
  sym: TSymbol;
begin
  if sName ='' then
    exit;

  write('&lt;***TYPE COMMITED:'+sName+'***&gt;');

  if not assigned(FCurrentUnit) then
    raise Exception.Create('No Current Unit defined, parsing: '+sName);

  if (lowercase(sName) = lowercase(self.FCurrentUnit.Name)) then
    raise Exception.create('Type Name cannot match unit name: '+sName);

  sym := TMatureConstruct.create(trim(sName), project, self.CurrentUnit);

  project.Symbols.Add(sym);
  ClearBuffer;
  self.LastTypeName := '';

  self.Currentsymbol := sym;

end;

//------------------------------------------------------------------------------
procedure TCCompiler.DefineUnit(sName: ansistring);
var
  sym: TUnitDefinition;
begin

  if Project = nil then
    raise EClassException.create('compiler has nil project');
  if self.Project.Symbols.HasSymbol(sName) then
    raise EClassException.create('Unit name '+sName+' already exists in project');

  sym := TunitDefinition.create(sNAme, project, nil);


  CurrentUnit := sym;


  self.Project.Symbols.Add(sym);

  ClearBuffer;

  DebugEvent('created unit named '+sName);


  self.CurrentSymbol := sym;

end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchClassDefinition;
begin
  if curchar = '(' then
    stack.push(pcParens);

  if AtEndOfWord('public') then begin
    self.CurrentScope := scPublic;
  end;

  if AtEndOfWord('published') then begin
    self.CurrentScope := scPublished;
  end;

  if AtEndOfWord('private') then begin
    self.CurrentScope := scPrivate;
  end;

  if AtEndOfWord('protected') then begin
    self.CurrentScope := scProtected;
  end;




  if AtEndOfWord('procedure')
  or AtEndOfWord('function') then begin
    //if currentSignature <> '' then
    //  commitfunction;
    currentSignature := previousword+' ';
    currentFunction := '';
    stack.push(pcFunctionDeclaration);
    stack.push(pcFunctionDeclarationName);
  end;

  if AtEndOfWord('property') then begin
    //if currentSignature <> '' then
    //  commitfunction;
    currentSignature := previousword+' ';
    currentFunction := '';
    currentProperty := '';
    lastCommittedFunction := nil;
    stack.push(pcPropertyDeclarationName);
  end;


  //these things can appear that affect the last committed function
  if AtWord('overload') then
  if assigned(LastCommittedFunction) then
    self.LastCommittedFunction.Flags := self.LastCommittedFunction.Flags + [ffOverload];

  if AtWord('override') then
  if assigned(LastCommittedFunction) then
    self.LastCommittedFunction.Flags := self.LastCommittedFunction.Flags + [ffoverride];

  if AtWord('reintroduce') then
  if assigned(LastCommittedFunction) then
    self.LastCommittedFunction.Flags := self.LastCommittedFunction.Flags + [ffreintroduce];

  if AtWord('virtual') then
  if assigned(LastCommittedFunction) then
    self.LastCommittedFunction.Flags := self.LastCommittedFunction.Flags + [ffvirtual];

  if AtWord('dynamic') then
  if assigned(LastCommittedFunction) then
    self.LastCommittedFunction.Flags := self.LastCommittedFunction.Flags + [ffdynamic];

  if AtWord('stdcall') then
  if assigned(LastCommittedFunction) then
    self.LastCommittedFunction.Flags := self.LastCommittedFunction.Flags + [ffstdcall];

  if AtWord('register') then
  if assigned(LastCommittedFunction) then
    self.LastCommittedFunction.Flags := self.LastCommittedFunction.Flags + [ffregister];

  if AtWord('abstract') then
  if assigned(LastCommittedFunction) then
    self.LastCommittedFunction.Flags := self.LastCommittedFunction.Flags + [ffabstract];

  if AtEndofWord('end') then
    stack.pop;
end;

procedure TCCompiler.DispatchRecordDefinition;
begin
  if AtEndofWord('end') then
    stack.pop;

end;

//------------------------------------------------------------------------------
procedure TCCompiler.DispatchCodeComment;
begin
  if AtEndOfPattern('*/') then begin
    stack.Pop;
    blockAdvance;
  end;


end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchComment;
begin
  if AtEndOfPattern('}') then begin
    stack.Pop;
    blockAdvance;
  end;

end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchContext;
begin
  if assigned(OnProgress) then begin
    if position mod 1000 = 0 then begin
      OnProgress(position, length(input));
    end;
  end;
  //if anything but ansistring, determine comment states
  if not (stack.current in [pcString, pcComment, pcLineComment, pcCodeComment]) then begin


    if AtBeginningOfPattern('//') then begin
      stack.Push(pcLineComment);
      movethroughpattern('//');
    end;

    if AtBeginningOfPattern('/*') then begin
      stack.Push(pcCodeComment);
      movethroughpattern('/*');
    end;

  end;

  if not (stack.current in [pcString, pcComment, pcLineComment, pcCodeComment]) then begin
    if AtEndofpattern('''') then begin
      ansistringType := stArray;
      stack.Push(pcString);
      exit;
    end;

    if AtEndofpattern('"') then begin
      ansistringType := stNull;
      stack.Push(pcString);
      exit;
    end;

  end;

  if Movingthroughword then begin
    if curchar in validnames then begin
      exit;
    end else begin
      MovingThroughWord := false;
    end;
  end;

  if MovingThroughSpace then begin
    if curchar in [' ',#13,#10] then begin
      exit;
    end else begin
      MovingThroughSpace := false;
    end;
  end;


  //dispatch contexts
  case stack.Current of
    pcNone: DispatchRoot;
    pcComment: DispatchComment;
    pcLineComment: DispatchLineComment;
    pcCodeComment: DispatchCodecomment;
    pcUnit, pcUnitName, pcLibrary, pcProgram: DispatchUnit;
    pcInterface: DispatchInterface;
    pcImplementation: DispatchImplementation;
    pcType: DispatchType;
    pcConst: DispatchConst;
    pcTypeDefinition: DispatchTypeDefinition;
    pcUses: DispatchUses;
    pcClassDefinition: DispatchClassDefinition;
    pcRecordDefinition: DispatchRecordDefinition;
    pcClassDefinitionHeader: DispatchClassDefinitionHeader;
    pcParens: DispatchParens;
    pcFunction, pcProcedure: DispatchFunction;
    pcFunctionDeclaration, pcFunctionResult, pcFunctionDeclarationName: DispatchFunctionDeclaration;
    pcPropertyDeclaration,
    pcPropertyDeclarationName,
    pcPropertyDeclarationIndex,
    pcPropertyDeclarationResult,
    pcPropertyDeclarationWrite,
    pcPropertyDeclarationRead: DispatchPropertyDeclaration;
    pcBrackets: DispatchBrackets;
    pcCodeBlock: DispatchCodeBlock;
    pcFunctionImpName: DispatchFunctionImpName;
    pcVar: DispatchVarConst;
    pcSTring: DispatchString;
    pcDirective: DispatchDirective;
    pcDefine: DispatchDefine;
    pcEnum: DispatchEnum;
    pcStructName: DispatchStructName;
    pcStruct: DispatchStruct;
    pcStructDoc: DispatchStructDoc;
    pcStorageSymbol: DispatchStorageSymbol;
    pcDecorations: DispatchDecorations;
    pcFunctionParam: DispatchFunctionParam;
    pcUnitDocumentation: DispatchLIneComment;
  else
    raise Exception.create('Context not handled '+inttostr(ord(stack.current)));
  end;
end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchDirective;
begin
  //end of directive at CR

  if (self.AtBeginningOfPattern(#13)) or (self.AtBeginningOfPattern('//')) then begin
    self.ClearBuffer;
    stack.Pop;
    BlockAdvance;
  end;

  if CurChar in validnames then
    self.BufferWrite(self.CurChar)
  else begin
    //check the directive name to see if its something we need to handle

    if lowercase(self.Buffer) = 'define' then begin
      self.ClearBuffer;
      ClearSymbol;
      stack.CurrentContext := pcDefine;
    end;


    //#if
    //#ifdef
    //#define
    //#elif
    //#endif
    //#if

  end;


end;
procedure TCCompiler.DispatchEnum;
begin
  if curchar in ['}'] then begin
    stack.pop;

  end;
end;

//------------------------------------------------------------------------------
procedure TCCompiler.DispatchDefine;
begin
  if curchar = #13 then begin
    stack.pop;
    if currentsymbol <> nil then begin
      With TMacro(CurrentSymbol) do begin
        code := buffer;
        clearbuffer;
      end;
    end;

  end;

  //if we haven't defined the symbol yet, fetch the name
  if currentsymbol = nil then begin
    //if a valid name caharacter
    if CurChar in validnames then begin
      self.BufferWrite(curchar);

    end
    //else define current symbol as macro
    else begin
      CurrentSymbol := TMacro.create(buffer, self.Project, nil);
      clearbuffer;
    end;
  end
  //else buffer into macro code
  else begin
    bufferchar;
  end;


end;


//------------------------------------------------------------------------------
procedure TCCompiler.DispatchImplementation;
begin
  self.CurrentScope := scImplementation;

  if AtWord('end') then
    stack.pop;

  if AtEndOfWord('procedure')
  or AtEndOfWord('function')
  or AtEndOfWord('constructor')
  or AtEndOfWord('destructor')
  then begin
    stack.push(pcFunction);
    currentSignature := previousword+' ';
    currentFunction := '';
    self.BlockAdvance;
  end;


  if AtWord('var') then
    stack.push(pcVar);

  if AtWord('const') then
    stack.push(pcConst);

  if AtWord('uses') then
    stack.push(pcUses);

  if AtWord('type') then
    stack.push(pcType);


end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchInterface;
begin
  self.currentscope := scInterface;

  //expect type, const, uses, var
  if AtEndofWord('type', false) then
    stack.Push(pcType);

  if AtEndofWord('uses', false) then
    stack.Push(pcUses);

  if AtEndofWord('const', false) then
    stack.Push(pcConst);

  if AtEndofWord('var', false) then
    stack.Push(pcVar);

  if AtWord('implementation') then
    stack.pop;

  if AtEndOfWord('implementation') then
    stack.pop;

  if AtEndOfWord('function') or AtEndOfWord('procedure') then begin
    stack.push(pcFunctionDeclaration);
    stack.push(pcFunctionDeclarationName);
    currentSignature := previousword+' ';
    currentFunction := '';
    currentSymbol := CurrentUnit;
  end;



  if AtEndOfWord('implementation') then
    stack.push(pcImplementation);




end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchLineComment;
begin
  if AtBeginningOfPattern('//') then begin
    stack.Push(pcUnitDocumentation);
    movethroughpattern('//');
  end;


  if (stack.Parent in [pcClassDefinitionHeader, pcUnit])
  or (stack.current in [pcUnitDocumentation])
  then begin
    documentingsymbol := currentunit;
    self.Doc(self.CurChar);

    if CurChar = #13 then begin
      self.DocNewLine;
    end;
  end
  else if stack.Parent in [pcClassDefinition, pcFunction, pcProcedure] then begin
    if self.LastCommittedFunction <> nil then begin
      documentingsymbol := self.LastCommittedFunction;
      self.FuncDoc(self.CurChar);

      if Curchar = #13 then begin
        self.FuncDocNewLine;
      end;
    end else
    if self.LastCommittedProperty <> nil then begin
      self.PropDoc(self.CurChar);

      if Curchar = #13 then begin
        self.PropDocNewLine;
      end;
    end;
  end
  else if stack.Parent in [pcStructDoc] then begin
    if self.LastCommittedFunction <> nil then begin
      self.FuncDoc(self.CurChar);

      if Curchar = #13 then begin
        self.FuncDocNewLine;
      end;
    end else
    if self.LastCommittedProperty <> nil then begin
      self.PropDoc(self.CurChar);

    end else
    //documentation of structure
    begin

      documentingsymbol := currentstruct;
      self.DebugEvent('doc to : 0x'+inttohex(cardinal(pointer(documentingsymbol)),8));
      self.Doc(self.CurChar);
      if Curchar = #13 then begin
        self.DocNewLine;
      end;

    end;

  end;



  if curchar = #13 then begin
    stack.Pop;
    BlockAdvance;
  end;


end;
//------------------------------------------------------------------------------
function TCCompiler.IsFlag: boolean;
begin
  if lowercase(buffer) = 'extern' then begin
    flag_extern := true;
    result := true;
  end else
  if lowercase(buffer) = 'unsigned' then begin
    flag_extern := true;
    result := true;
  end else
  if lowercase(buffer) = 'static' then begin
    flag_static := true;
    result := true;
  end else begin
    result := false;
  end;

end;
function TCCompiler.IsKeyWord: boolean;
begin
  if lowercase(buffer) = 'typedef' then begin
    flag_extern := true;
    result := true;
  end else
  if lowercase(buffer) = 'class' then begin
    flag_static := true;
    result := true;
  end else begin
    result := false;
  end;
end;

procedure TCCompiler.MoveThroughSpace;
begin
  MovingthroughSpace := true;
end;

procedure TCCompiler.MoveThroughWord;
begin
  Movingthroughword := true;
end;

procedure TCCompiler.DispatchRoot;
begin
  //when in root context, expect keyword, directive, or type

  if AtWord('struct') then begin
    CurrentStruct := TStructure.create('', project, currentunit);
    project.symbols.add(CurrentStruct);
    stack.push(pcStruct);
    stack.push(pcStructName);
    Clearbuffer;
    moveThroughWord;
    MovethroughSpace;

    //Advance;
    exit;
  end;

  if AtWord('typedef') then begin
    stack.push(pcType);
    Clearbuffer;
    moveThroughWord;
    MovethroughSpace;
    //Advance;
    exit;
  end;


  //symbols #

  //goes to directive
  if AtBeginningOfPattern('#') then begin
    clearbuffer;
    clearSymbol;
    stack.push(pcDirective);
    exit;
  end;

  if curchar in [';'] then begin
    clearbuffer;
    exit;
  end;


  //flags
  //todo 1: figure out when to clear flags
  //todo 1: handle some basic macros

  if curchar in validnames then begin
    bufferchar;
  end else begin
    if (buffer <> '') and (not (Isflag or IsKeyWord)) then begin
      //expect a storage type symbol
      TypeBuffer := buffer;
      ClearBuffer;

      stack.push(pcStorageSymbol);
      self.MoveThroughSpace;


    end;

  end;

  //todo 1: static flag

  //todo 1: extern flag
  //


end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchType;
begin

  //enum
  //struct



  if AtWord('enum') then begin
    stack.push(pcEnum);
    movethroughword;
    movethroughspace;
//    advance;
    exit;
  end;

  if AtWord('struct') then begin
    CurrentStruct := TStructure.create('', project, currentunit);
    stack.push(pcStruct);
    clearbuffer;
    stack.push(pcStructDoc);
    lasttypename := '';
    movethroughword;
    movethroughspace;
//    advance;
    exit;
  end;

  if Curchar in Validnames then
    BufferWrite(curchar);

  if curchar in wordbreaks then begin
    RecognizeType(Buffer);

  end;

  if Curchar = '=' then begin
    stack.push(pcTypeDefinition);
  end;

  if curchar in [';'] then begin
    stack.pop;
    CurrentStruct := nil;
    CurrentClass := nil;

  end;


end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchTypeDefinition;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

//------------------------------------------------------------------------------
procedure TCCompiler.DispatchUnit;
begin
  //expect uses, implementation

  //if in the unitname part of the unit then:
  if self.Stack.CurrentContext = pcUnitName then begin
    //if name AnsiCharacters
    if (Curchar in Validnames) then
      BufferWrite(curchar);

    //if wordbreak AnsiCharacterrs
    if Curchar in WordBreaks then begin
      //define the unit from the buffered text
      defineUnit(self.Buffer);
      //pop off to the previous context (program, unit, library)
      stack.Pop;
    end;
  end;


  if AtEndOfWord('uses') then
    stack.push(pcUses);

  if AtEndOfWord('interface') then begin
    self.currentsymbol := nil;
    stack.push(pcInterface);
  end;

  if AtEndOfWord('implementation') then
    stack.push(pcImplementation);

end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchUses;
begin
  if curchar in ValidNAmes then
    BufferWrite(curchar);


  if curchar = ',' then begin
    //project.QueueCompile(FBuffer);
    self.ClearBuffer;
  end;

  if curchar = ';' then begin
    //project.QueueCompile(FBuffer);
    self.ClearBuffer;
    stack.pop;
  end;
end;
//------------------------------------------------------------------------------
function TCCompiler.GetBestParent: TSymbol;
begin
  result := nil;
//  result := CurrentFunction;//todo 1: resolve function objets
  if result=nil then
    result := CurrentClass;
  if result=nil then
    result := CurrentUnit;
//  if result=nil then
//    result := project;
end;

function TCCompiler.GetCurrentClass: TMatureConstruct;
begin
  result := TMatureConstruct(Fcurrentsymbol);

end;

procedure TCCompiler.DispatchConst;
begin
  if AtWord('var') then begin
    stack.pop;
    exit;
  end;

  if AtWord('type') then begin
    stack.pop;
    exit;
  end;

  if AtWord('implementation') then begin
    stack.pop;
    exit;
  end;

  if AtWord('procedure') then begin
    stack.pop;
    exit;
  end;

  if AtWord('function') then begin
    stack.pop;
    exit;
  end;

end;

procedure TCCompiler.Doc(sString: ansistring);
begin
  if self.DocumentingSymbol <> nil then begin
    currentsymbol.Documentation.text :=     copy(DocumentingSymbol.Documentation.text, 1, length(DocumentingSymbol.documentation.Text)-2)+ sString;
  end;
end;

procedure TCCompiler.DocNewLine;
var
  sTemp: ansistring;
begin
  if self.CurrentSymbol <> nil then begin


    //if the line starts with "R:" then convert to class responsibility
    sTemp := currentsymbol.documentation[currentsymbol.documentation.count-1];
    if lowercase(copy(sTemp, 1, 2)) = 'r:' then begin
      currentsymbol.responsibilities.add(copy(sTemp, 3, length(sTemp)));
      currentsymbol.documentation.delete(currentsymbol.documentation.count-1);
    end
    else if lowercase(copy(sTemp, 1, 2)) = 'a:' then begin
      currentsymbol.Author := Trim(copy(sTemp, 3, length(sTemp)));
      currentsymbol.documentation.delete(currentsymbol.documentation.count-1);
    end
    else begin
      currentsymbol.Documentation.text := currentsymbol.Documentation.text + #13#10;
    end;


  end;

end;

procedure TCCompiler.DispatchClassDefinitionHeader;
begin
  //forward declaration
  if AtWord('public')
  or AtWord('private')
  or AtWord('protected')
  or AtWord('published')
  then begin
    stack.pop;
    stack.push(pcClassDefinition);
  end;

  if curchar in validNames then begin
    stack.pop;
    stack.push(pcClassDefinition);
  end;

  IF curchar = ';' then
    stack.pop;

end;
//------------------------------------------------------------------------------
procedure TCCompiler.SetCurrentUnit(const Value: TunitDefinition);
begin
  FCurrentunit := Value;
  self.Currentsymbol := fCurrentunit;
end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchParens;
begin
  if stack.Parent in [pcFunctionDeclaration, pcFunctionDeclarationName] then
    currentsignature := currentsignature + curchar;

  if curchar = ')' then
    stack.pop;
end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchFunctionDeclaration;
begin
  CurrentSignature := CurrentSignature+curChar;


  if (stack.current = pcFunctionDeclarationName) and (curchar in validnames) then
    self.currentfunction := self.currentfunction + curchar;

  if curChar = ':' then begin
    //CommitFunction;
    stack.Pop;
    stack.push(pcFunctionResult);
  end;
  if curChar = ';' then begin
    if stack.current = pcFunctionResult then
      stack.pop;

    Commitfunction;
    stack.pop;

    if stack.current = pcFunctionDeclaration then
      stack.pop;
  end;

  //TODO: Enhance to parse through types in parens
  if curchar = '(' then begin
    if stack.current = pcFunctionDeclarationName then begin
      stack.pop;
      stack.push(pcFunctionDeclaration);
    end;
    stack.Push(pcparens);
  end;

end;
//------------------------------------------------------------------------------
procedure TCCompiler.CommitFunction;
var
  func: TFunctionDefinition;
begin
  if CurrentFunction = '' then exit;

  write('<font color="#007F00">$commit "'+currentclass.name+'.'+currentFunction+'"$</font>');
  //frmDebug.ShowMessage(currentSignature);

  func := self.CurrentClass.AddFunction(currentFunction);
  func.Signature := currentSignature;

  self.LastCommittedFunction := func;



end;
//------------------------------------------------------------------------------
procedure TCCompiler.RecommitFunction;
var
  i: integer;
  func: TFunctionDefinition;
begin
  i := self.currentclass.ItemCount;

  if i>0 then begin
    func := self.CurrentClass.Functions[i-1];
    //func.free;
    self.CurrentClass.DeleteItem(i-1);
  end;

  CommitFunction;

end;
procedure TCCompiler.RootState;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

//------------------------------------------------------------------------------
procedure TCCompiler.FuncDoc(sString: ansistring);
begin
  if self.CurrentSymbol <> nil then begin
    lastCommittedFunction.Documentation.text :=     copy(lastCommittedFunction.Documentation.text, 1, length(lastCommittedFunction.Documentation.text)-2)+ sString;

  end;
end;
//------------------------------------------------------------------------------
procedure TCCompiler.FuncDocNewLine;
var
  sTemp: ansistring;
  sLeft, sRight: ansistring;
begin
  if self.LastCommittedFunction <> nil then begin

    //if the line starts with "R:" then convert to class responsibility
    if lastCommittedFunction.documentation.count = 0 then
      exit;

    sTemp := lastCommittedFunction.documentation[lastCommittedFunction.documentation.count-1];
    if lowercase(copy(sTemp, 1, 2)) = 'a:' then begin
      lastCommittedFunction.Author := Trim(copy(sTemp, 3, length(sTemp)));
      lastCommittedFunction.documentation.delete(lastCommittedFunction.documentation.count-1);
    end
    else if lowercase(copy(sTemp, 1, 7)) = 'author:' then begin
      lastCommittedFunction.Author := Trim(copy(sTemp, 8, length(sTemp)));
      lastCommittedFunction.documentation.delete(lastCommittedFunction.documentation.count-1);
    end
    else if lowercase(copy(sTemp, 1, 4)) = 'web:' then begin
      lastCommittedFunction.WebPage := Trim(copy(sTemp, 5, length(sTemp)));
      lastCommittedFunction.documentation.delete(lastCommittedFunction.documentation.count-1);
    end
    else if lowercase(copy(sTemp, 1, 2)) = 'w:' then begin
      lastCommittedFunction.WebPage := Trim(copy(sTemp, 3, length(sTemp)));
      lastCommittedFunction.documentation.delete(lastCommittedFunction.documentation.count-1);
    end
    else if lowercase(copy(sTemp, 1, 12)) = 'integration:' then begin
      lastCommittedFunction.IntegrationPackage := Trim(copy(sTemp, 13, length(sTemp)));
      lastCommittedFunction.documentation.delete(lastCommittedFunction.documentation.count-1);
    end
    else if lowercase(copy(sTemp, 1, 7)) = 'intype:' then begin
      lastCommittedFunction.InType := Trim(copy(sTemp, 8, length(sTemp)));
      lastCommittedFunction.documentation.delete(lastCommittedFunction.documentation.count-1);
    end
    else if lowercase(copy(sTemp, 1, 8)) = 'outtype:' then begin
      lastCommittedFunction.Outtype := Trim(copy(sTemp, 9, length(sTemp)));
      lastCommittedFunction.documentation.delete(lastCommittedFunction.documentation.count-1);
    end
    else if lowercase(copy(sTemp, 1, 11)) = 'visibility:' then begin
      lastCommittedFunction.Visibility := Trim(copy(sTemp, 12, length(sTemp)));
      lastCommittedFunction.documentation.delete(lastCommittedFunction.documentation.count-1);
    end
    else if lowercase(copy(sTemp, 1, 2)) = 'p:' then begin
      splitstring(Trim(copy(sTemp, 3, length(sTemp))), ':', sLeft, sRight);
      lastCommittedFunction.Parameters.Add(sLeft, sRight);
      lastCommittedFunction.documentation.delete(lastCommittedFunction.documentation.count-1);
    end
    else begin
      //lastCommittedFunction.Documentation.text := lastCommittedFunction.Documentation.text + #13#10;
    end;
    lastCommittedFunction.Documentation.text := lastCommittedFunction.Documentation.text + #13#10;
  end;


end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchPropertyDeclaration;
begin
  CurrentSignature := CurrentSignature+curChar;


  if (stack.current = pcPropertyDeclarationName) and (curchar in validnames) then
    self.currentproperty := self.currentproperty + curchar;

  if curChar = ';' then begin
    CommitProperty;
    stack.pop;
  end;

  //TODO: Enhance
  if curchar = ':' then begin
    stack.pop;
    stack.push(pcPropertyDeclarationResult);
  end;
  if curchar = '[' then begin
    if stack.current = pcPropertyDeclarationName then begin
      stack.pop;
      stack.push(pcPropertyDeclaration);
    end;
    stack.Push(pcBrackets);
  end;
end;

procedure TCCompiler.DispatchBrackets;
begin
  if stack.Parent in [pcPropertyDeclaration, pcPropertyDeclarationName, pcPropertyDeclarationIndex] then
    currentsignature := currentsignature + curchar;

  if curchar = ']' then
    stack.pop;

end;
//------------------------------------------------------------------------------
procedure TCCompiler.DispatchCodeBlock;
begin
  if AtWord('begin') or AtWord('case') or AtWord('try') then begin
    self.Write('$newblock$');
    stack.push(pcCodeBlock);
  end;

  if AtEndOfWord('end') then begin
    self.Write('$endblock$');
    stack.pop;
  end;


end;
//------------------------------------------------------------------------------
procedure TCCompiler.CommitProperty;
var
  prop: TPropertyDefinition;
begin
  write('$commit$');
  //frmDebug.ShowMessage(currentSignature);

  prop := self.CurrentClass.AddProperty(currentProperty);
  prop.Signature := currentSignature;

  self.LastCommittedProperty := prop;



end;

procedure TCCompiler.PropDoc(sString: ansistring);
begin
  if self.CurrentSymbol <> nil then begin
    lastCommittedProperty.Documentation.text :=     copy(lastCommittedProperty.Documentation.text, 1, length(lastCommittedProperty.Documentation.text)-2)+ sString;
  end;
end;

procedure TCCompiler.PropDocNewLine;
var
  sTemp: ansistring;
begin
  if self.LastCommittedProperty <> nil then begin

    //if the line starts with "R:" then convert to class responsibility
    sTemp := '';
    if lowercase(copy(sTemp, 1, 2)) = 'r:' then begin
      //currentsymbol.responsibilities.add(copy(sTemp, 3, length(sTemp)));
      //currentsymbol.documentation.delete(currentsymbol.documentation.count-1);
    end else begin
      lastCommittedProperty.Documentation.text := lastCommittedProperty.Documentation.text + #13#10;
    end;
  end;

end;

procedure TCCompiler.DispatchFunction;
begin
  if AtEndOfWord('function') or AtEndOfWord('procedure') or AtEndOfWord('destructor') or AtEndofWord('constructor') then begin
    self.CurrentFunction := '';
    stack.Push(pcFunctionImpName);
  end else
  if curchar = '(' then begin
    stack.push(pcParens);
  end else
  if AtWord('begin') then begin
      self.Write('$newblock$');
      stack.pop;
      stack.push(pcCodeBlock);
      LastcommittedFunction := nil;
      LastcommittedProperty := nil;
  end;


end;
//------------------------------------------------------------------------------
procedure TCCompiler.DisPatchFunctionImpName;
begin
  if (self.CurChar in ValidNames) or (self.curchar = '.') then
    self.CurrentFunction := self.CurrentFunction + self.curchar
  else
  if (self.curchar in wordbreaks) and (not (curchar=' '))then begin
    self.FindFunction(self.currentfunction);
    stack.pop;

  end;

end;
//------------------------------------------------------------------------------
procedure TCCompiler.FindFunction(sName: ansistring);
var
  iPos: integer;
  sClassName: ansistring;
  sFunctionName: ansistring;
begin
  //set t
  iPos := pos('.', sName);
  if iPos>0 then begin
    sClassName := copy(sName, 1, iPos-1);
    sFunctionName := copy(sName, iPos+1, length(sName));
    self.CurrentSymbol := project.symbols.findSymbol(sClassName);
    if assigned(currentclass) then
      self.LastCommittedFunction := currentclass.FindFunction(sFunctionName);
  end else begin
    //set the current class to the current unit
    self.CurrentSymbol := self.CurrentUnit;
    self.LastCommittedFunction := currentclass.FindFunction(sName);
    self.LAstCommittedProperty := nil;
  end;

end;

procedure TCCompiler.SetCurrentClass(const Value: TMatureConstruct);
begin
  FCurrentSymbol := value;
end;

procedure TCCompiler.DispatchVarConst;
begin
  if AtWord('procedure')
  or AtWord('function')
  or AtWord('constructor')
  or AtWord('destructor')
  or AtWord('initialization')
  or AtWord('finalization')
  or AtWord('implementation')
  or AtWord('uses')
  or AtWord('const')
  or AtWord('var') then begin
    stack.pop;
    blockAdvance;
  end;
end;


procedure TCCompiler.DispatchStorageSymbol;
var
  v: TVariableSymbol;
begin
  if curchar in validnames then begin
    bufferchar;
  end else begin
    if curchar in [';'] then begin
      v := TVariableSymbol.create(buffer, project, GetBestParent);
      v.TypeName := typebuffer;
      DebugEvent('created TVariableSymbol '+v.name+' of type '+v.typename);

      stack.pop;
      blockadvance;
    end else begin
      CurrentStorageSymbol := buffer;
      clearbuffer;
      stack.pop;
      Stack.Push(pcDecorations);
      blockadvance;
    end;
  end;
end;

procedure TCCompiler.DispatchStorageType;
begin
  //todo 1: finish this
  //if find word break then expect variable names

end;

procedure TCCompiler.DispatchString;
begin
  case ansistringtype of
    stNull: if AtEndofpattern('"') then stack.pop;
    stArray: if AtEndofpattern('''') then stack.pop;
  end;

end;


function TCCompiler.Compile(sFile: ansistring): boolean;
var
  sym: TSymbol;
begin
  DefineUnit(extractfilename(sfile));

  result := inherited Compile(sFile);

  if assigned(FCurrentUnit) then
    FCurrentUnit.Debug := output;

end;

procedure TCCompiler.DispatchStruct;
begin
  if CurrentStruct = nil then begin
    CurrentStruct := TStructure.create('', project, currentunit);

  end;
  if curchar in ['}'] then begin
    if stack.IsWithin(pcType) then begin
      MoveThroughPattern('}');
      BlockAdvance;
      stack.pop;
      MoveThroughSpace;
      stack.Push(pcStructName);

    end else begin
      MoveThroughPattern('}');
      BlockAdvance;
      ClearBuffer;
      stack.pop;
      if not stack.IsWithin(pcType) then begin
        currentclass := nil;
        currentstruct := nil;
      end;
    end;
  end;
end;

procedure TCCompiler.DispatchStructname;
begin

  if curchar in validnames then begin
    bufferchar;
  end else
  begin
    lasttypename := buffer;
    CurrentStruct.Name := lasttypename;
    //CurrentClass := TStructure.create(lasttypename, project, self.currentunit);

    DebugEvent('Named Structure: '+lasttypename+'[0x'+inttohex(cardinal(pointer(CurrentStruct)),8)+']');
    project.symbols.add(CurrentStruct);
    DebugEvent('symbolcount = '+inttostr(project.symbols.count));


    if stack.IsWithin(pctype) then begin
      if curchar = ';' then begin
        stack.pop;
        blockadvance;
      end else
        currentstruct.name := lasttypename;


    end else begin
      if curchar = '{' then
        stack.current := pcStruct
      else
        stack.current := pcStructDoc;

    end;

  end;


//  if curchar in [';'] then begin
//    stack.pop;
//  end;
end;


procedure TCCompiler.DispatchStructDoc;
begin
  if curchar = '{' then begin
    stack.pop;
//    stack.current := pcStruct;
  end;
end;




procedure TCCompiler.DispatchDecorations;
var
  f: TFunctionDefinition;
begin
  if not (curchar in [';']) then begin
    //if we find '(' then push into param parsing
    //also as a result, we have decided that this is a function
    //so create the function defintiion
    if curchar in ['('] then begin
      //create function definition
      f := TFunctionDefinition.create(CurrentStorageSymbol, project, GEtBestParent);
      f.ResultType := typebuffer;
      DebugEvent('Created a function definition '+f.name+' that returns '+f.resulttype);
      stack.push(pcFunctionParam);
    end;

  end
  //else if it is some other kind of symbol
  //we'll assume that we've ended our declaration and it is a variable???
  //todo 1: the above isn't entirely true... expand the logic
  else begin
      CurrentStorageSymbol := buffer;
      clearbuffer;
      stack.pop;
//      Stack.Push(pcDecorations);
//      movethroughspace;
      blockadvance;
  end;


end;

procedure TCCompiler.DispatchFunctionParam;
begin
  if curchar in [')'] then begin
    stack.pop;
  end;
end;

end.
