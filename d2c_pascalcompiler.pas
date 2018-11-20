unit d2c_pascalcompiler;
//This unit contains classes for parsing object pascal source code. See individual units for details.

interface

uses debug, contnrs, namevaluepair, sysutils, classes, d2c_project, stringx, sharedobject, stringx.ansi;

type
  ESyntax = class(Exception);
  TStackChangedEvent = procedure of object;
  TStackPushEvent = procedure of object;
  TStackPopEvent = procedure of object;

  TCompiler = class;//forward
  //----------------------------------------------------------------------------
  TCompilerClass = class of TCompiler;


  //Enumerated Types
  TParseContext =
  //this type describes the current context the compiler is working in.
  (     pcNone =0,
{01}    pcProgram, //the compiler is in the root of a unit that begins in "program"
{02}    pcProgramBody, //the compiler is in the body of a unit that begins with "program"
{03}    pcUnit, //the compiler used in the body of a unit that begins with "unit"
{04}    pcUnitName,//the compiler is defining the unit name
{05}    pcUses, //the compiler is analyzing unit dependencies
{06}    pcVar, //the compiler is in a var section
{07}    pcConst, //compiler using a constant declaration
{08}    pcType, //the compiler is in a type declaration
{09}    pcTypeDefinition, //the compiler is in a type definition
{10}    pcInterface, //the compiler using the interface action of a unit
{11}    pcImplementation, //compiler is in the implementation section of a unit
{12}    pcLibrary, //be compiler is in the root of a unit that begins with "library"
{13}    pcFunctionDeclaration, //be compiler used in the declaration of a function
{14}    pcFunctionDeclarationName, //be compiler used in the declaration of a function
{15}    pcFunctionName, //the compiler is parsing the name of a function
{16}    pcFunctionParam, //the compiler is parsing a parameter of the function
{17}    pcFunctionResult, //the compiler is parsing the result type of a function
{18}    pcFunctionBody, //the compiler is parsing the body of a function
{19}    pcInitialization, //the compiler is in the initialization section of the unit
{20}    pcClassDefinition, //the compiler is in a class definition
{21}    pcClassDefinitionHeader, //the compiler is in a class that definition but has not reached the body
{22}    pcRecordDefinition, //compiler is in a record definition
{23}    pcFinalization, //the compiler is in the finalization section of the unit
{24}    pcComment, //the compiler is in a standard comment
{25}    pcLineComment, //the compiler is a end-of-line line comment
{26}    pcCodeComment, //the compiler is in a comment that begins with (*
{27}    pcString, //the compiler is parsing a ansistring
{28}    pcParens, //the compiler is parsing someting in parens
{29}    pcPropertyDeclaration,
{30}    pcPropertyDeclarationName,
{31}    pcPropertyDeclarationIndex,
{32}    pcPropertyDeclarationResult,
{33}    pcPropertyDeclarationWrite,
{34}    pcPropertyDeclarationRead,
{35}    pcBrackets,
{36}    pcFunction,
{37}    pcProcedure,
{38}    pcFunctionImpName,
{39}    pcDirective,      //not implemented for pascal
{30}    pcMacro,          //not implemented for pascal
{41}    pcDefine,         //not implemented for pascal
{42}    pcPreIF,          //not implemented for pascal
{43}    pcPreIFDEF,       //not implemented for pascal
{44}    pcStruct,         //not implemented for pascal
{45}    pcStructDoc,       //not implemented for pascal
{46}    pcEnum,           //not implemented for pascal
{47}    pcStorageSymbol,  //not implemented for pascal
{48}    pcCodeBlock,
{49}    pcStructName,
{50}    pcUnitDocumentation, //not implemented for pascal
{51}    pcDecorations     //not implemeneted for pascal, represents the context in which the system tries to determine if it is a variable, function being declared
);

  //----------------------------------------------------------------------------
  TContextStack = class(TObject)
  //the context stack class works similar to the stack in a CPU.
  //it maintains a hierarchy of contexts that the compiler has parsed
  //use Push to push a context on the stack
  //use pop to remove a context from the stack
  //used to get current context read the current context
  private
    Fcontexts: TList;
    FSymbolContexts: TList;
    FonStackChange: TStackChangedEvent;
    FPreviousContext: TParsecontext;
    FonStackPop: TStackPopEvent;
    FonStackPush: TStackPushEvent;
    function GetCurrentcontext: TParseContext;
    procedure SetCurrentContext(const Value: TParseContext);
    function GetParentcontext: TParseContext;
    function GetPreviousContext: TParseContext;
    function GetCurrentSymbolContext: TSymbol;
    procedure SetCurrentSymbolContext(const Value: TSymbol);
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure Push(context: TParseContext; symbolcontext: TSymbol = nil);
    function Pop: TParseContext;
    procedure Clear;
    property CurrentContext: TParseContext read GetCurrentcontext write SetCurrentContext;
    property CurrentSymbolContext: TSymbol read GetCurrentSymbolContext write SetCurrentSymbolContext;
    property Current: TParseContext read GetCurrentcontext write SetCurrentContext;
    property Parent: TParseContext read GetParentcontext;
    property OnStackChange: TStackChangedEvent read FonStackChange write FOnStackChange;
    property OnStackPop: TStackPopEvent read FonStackPop write FOnStackPop;
    property OnStackPush: TStackPushEvent read FonStackPush write FOnStackPush;

    property PreviousContext: TParseContext read GetPreviousContext;
    procedure Changed;
    procedure Pushed;
    procedure Popped;
    function IsWithin(c: TParseContext): boolean;
  end;

  //----------------------------------------------------------------------------
  TCompiler = class(TLockQueuedObject)
  //the compiler class is a generic compiler
  //it does not have any specific parsing abilities or any specific language
  //inherit a new class from Tcompiler to implement compilation for a specific language
  //its primary properties include:
  //an input property for inputting the source to be compiled
  //an output property for generating results
  //a compilation loop that can be overridden to trigger defense in sub-classes
  private
    FOnProgress: TCompilerProgressEvent;
    FFileName: ansistring;
    FpreviousPosition: integer;
    function GetAdvanceWasBlocked: boolean;
  protected
    column: nativeint;
    linenumber: nativeint;
    position: nativeint;
    FStack: TcontextStack;
    FBlockAdvance: boolean;

    FLastCommittedFunction: TFunctionDefinition;
    FLastCommittedProperty: TPropertyDefinition;
    FcurrentFunction: ansistring;
    FBuffer: ansistring;
    FInput: ansistring;
    FOutput: ansistring;
    FOnDebug: TNotifyEvent;
    FOnContextChange: TNotifyEvent;
    FProject: TD2CProject;
    procedure SetInput(sValue: ansistring);
    function GetInput: ansistring;
    function GetOutput: ansistring;
    procedure Comment;
    function GetCurChar: AnsiChar;
    function AtWord(sWord: ansistring;
      bCaseSensitive: boolean = false): boolean;
    function GetCurrentWord: ansistring;
    function GetPreviousWord: ansistring;

    procedure DispatchContext; virtual; abstract;


    procedure Write(sString: ansistring);
    procedure BufferWrite(sString: ansistring);
    procedure ClearBuffer;

    property CurChar: AnsiChar read GetCurChar;
    function AtWordBreak: boolean;
    function AtEndOfPattern(sPattern: ansistring; bCaseSensitive: boolean = false): boolean;
    function AtEndOfWord(sWord: ansistring; bCaseSensitive: boolean = false): boolean;
    function AtBeginningOfPattern(sPattern: ansistring; bCaseSensitive: boolean = false): boolean;
    procedure Debug;
    procedure BufferChar;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure BeforeCompile;virtual;
    function Compile(sFile: ansistring): boolean; virtual;
    property Stack: TContextStack read FStack;
  published
    property Project: TD2CProject read FProject write FProject;
    property LastCommittedFunction: TFunctionDefinition read FLastCommittedFunction write FLastCommittedFunction;
    property LastCommittedProperty: TPropertyDefinition read FLastCommittedProperty write FLastCommittedProperty;
    procedure BlockAdvance;
    procedure Advance(iBy: nativeint);
    property CurrentFunction: ansistring read FCurrentFunction write FCurrentFunction;
    property Buffer: ansistring read FBuffer;
    property Input: ansistring read GetInput write SetInput;
    property Output: ansistring read GetOutput;
    property OnDebug: TNotifyEvent read FOnDebug write FOnDebug;
    property OnContextChange: TNotifyEvent read FOnContextChange write FOnContextChange;
    property CurrentWord: ansistring read GetCurrentWord;
    property PreviousWord: ansistring read GetPreviousWord;
    procedure OpenFont(scolor: ansistring);
    procedure CloseFont;

    procedure HandleOnStackChange;
    procedure HandleOnStackPush;
    procedure HandleOnStackPop;
    property filename: ansistring read FFileName write FFileName;
    property OnProgress: TCompilerProgressEvent read FOnProgress write FOnProgress;
    procedure DebugEvent(sMessage: ansistring; sColor: ansistring = '#0000FF');
    property AdvanceWasBlocked: boolean read GetAdvanceWasBlocked;
    property LastPosition: integer read FpreviousPosition write FPreviousPOsition;
    procedure MOveThroughPattern(sPattern: ansistring);
  end;

  //----------------------------------------------------------------------------
  TPascalCompiler = class(Tcompiler)
  private
    FCurrentSymbol: Tsymbol;
    FCurrentUnit: TunitDefinition;
    FLastTypeName: ansistring;
    FCurrentSignature: ansistring;
    FCurrentProperty: ansistring;
    FCurrentFunctionDef: TFunctionDefinition;
    FCurrentScope: TScope;
    FCCode: TStringlist;
    function GetCurrentClass: TMatureConstruct;
    procedure SetCurrentUnit(const Value: TunitDefinition);
    procedure SetCurrentClass(const Value: TMatureConstruct);
  public
    constructor create; override;
    destructor destroy;override;

    procedure DispatchContext;override;
    procedure DispatchRoot;
    procedure DispatchComment;
    procedure DispatchCodeComment;
    procedure DispatchLineComment;
    procedure DispatchUnit;
    procedure DispatchInterface;
    procedure DispatchImplementation;
    procedure DispatchType;
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
    procedure DispatchStruct;
    procedure DispatchStructName;

    procedure DispatchRecordDefinition;
    procedure DispatchUses;

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

    procedure BeforeCompile;override;
    function Compile(sFile: ansistring): boolean; override;
    procedure SetupCCode;

    property CurrentUnit: TunitDefinition read FCurrentunit write SetCurrentUnit;
    property CurrentSymbol: Tsymbol read FCurrentSymbol write FCurrentSymbol;
    property CurrentClass: TMatureConstruct read GetCurrentClass write SetCurrentClass;
    property LastTypeName: ansistring read FLastTypeName write FLastTypeName;
    property CurrentSignature: ansistring read FCurrentSignature write FCurrentSignature;
    procedure DefineUnit(sName: ansistring);
    property CurrentScope: TScope read FCurrentScope write FCurrentScope;
    property CCode: TStringlist read FCCode;
    procedure WriteC(s: string; start_new_line: boolean = true);
    procedure WriteCProgramHeader();
    procedure WriteCLibraryHeader();

  end;

  //TPascalCompiler2 = class(Tcompiler)


const
  validnames = ['0'..'9','a'..'z','A'..'Z','_'];
  wordbreaks = [' ', '(',')','*','[',']','''','"','=','+','-','/','\','<','>','{','}',';',',','.',#10,#13,#0,#9];
  //oprerators = ['+', '-', '=', '=='];

function ContextToString(pc: TParseContext): ansistring;

implementation


//------------------------------------------------------------------------------
procedure Tcompiler.SetInput(sValue: ansistring);
begin
  FInput := sValue;
  FblockAdvance := false;
  FBuffer := '';
  FOutput := '';
  position := 0;

end;
//------------------------------------------------------------------------------
function Tcompiler.GetInput: ansistring;
begin
  result := FInput;
end;
//------------------------------------------------------------------------------
function Tcompiler.GetOutput: ansistring;
begin
  result := FOutput;

end;
//------------------------------------------------------------------------------
function Tcompiler.Compile(sFile: ansistring): boolean;
var
  iLen: integer;
  prevcontext: TParseContext;
  sym: TSymbol;
  sPrevFunction: ansistring;
begin
  BeforeCompile;
  self.Stack.clear;
  FileName := sfile;
  Input := LoadStringFromFile(sFile);

  result := false;
  try
    LAstPosition := 0;
    position := 1;
    iLen := length(FInput);
    //for each AnsiCharacter process context changes
    if iLen = 0 then exit;
    if iLen > 500000 then exit;

    repeat
      prevContext := stack.current;

      DispatchContext;
      Debug;

      if prevcontext <> stack.current then begin
        if stack.current = pcLineComment then begin
          if LastCommittedFunction <> nil then
            Write('<font color="#FFFF00">&lt;'+LastCommittedFunction.Name+'&gt;</font>');
          if LastCommittedProperty <> nil then
            Write('<font color="#FFFF00">&lt;'+LastCommittedProperty.Name+'&gt;</font>');

        end;

      end;


      if (sPrevFunction <> currentFunction) and (curchar<>'.') and (curchar in WordBreaks) then begin
        Write('<font color="#FF00FF">'+CurrentFunction+'</font>');
        sPrevFunction := currentFunction;
      end;




      if not FBlockAdvance then begin
        if curchar = '<' then begin
          WRite('&lt;');
        end else
        if curchar = ' ' then begin
          WRite('&nbsp;');
        end else
        if curchar = '>' then begin
          WRite('&gt;');
        end else
        Write(curchar);


        if curchar = #10 then begin
          WRite('<BR>');
        end;


        ADvance(1);
      end;


      LastPOsition := position;
      FBlockAdvance := false;
    until position > iLen;

    result := true;

    DebugEvent('Ended in context '+ContextToString(self.Stack.Current));

  except
     On E: Exception do begin
      result := false;
      write('*BOMB*'+e.Message);
      log(self,e.message, 'error');
//      writeln('*BOMB*'+e.Message);
      //raise Exception.create(e.message);
    end;
  end;


end;
//------------------------------------------------------------------------------
constructor Tcompiler.Create;
begin
  inherited;
  FblockAdvance := false;
  FOnDebug := nil;
  FBuffer := '';
  FOutput := '';
  FInput := '';
  Fstack := TContextStack.create;
  FStack.OnStackChange := self.HandleOnStackChange;
  FStack.OnStackPush := self.HandleOnStackPush;
  FStack.OnStackPop := self.HandleOnStackPop;

end;

{ TTagInfo }



{ TContextStack }

//------------------------------------------------------------------------------
procedure TContextStack.Changed;
begin
  if assigned(OnStackChange) then
    OnStackChange;
end;

procedure TContextStack.Clear;
begin
  FContexts.Clear;
  FSYmbolContexts.Clear;
  Push(pcNone);
end;

constructor TContextStack.Create;
begin
  FContexts := TList.create;
  FSYmbolContexts := TList.create;
  Push(pcNone);
end;
//------------------------------------------------------------------------------
destructor TContextStack.Destroy;
begin
  FContexts.free;
  FSYmbolContexts.free;
  inherited;
end;
//------------------------------------------------------------------------------
function TContextStack.GetCurrentcontext: TParseContext;
begin
  result := TParseContext(Fcontexts[Fcontexts.count-1]);
end;
function TContextStack.GetCurrentSymbolContext: TSymbol;
var
  t: integer;
begin
  result := nil;
  for t:= FSymbolContexts.count-1 downto 0 do begin
    result := FSymbolContexts[t];
    if result <> nil then
      break;
  end;

end;

//------------------------------------------------------------------------------
function TContextStack.GetParentcontext: TParseContext;
begin
  if Fcontexts.count > 1 then
    result := TParseContext(Fcontexts[Fcontexts.count-2])
  else
    result := pcnone;;

end;

function TContextStack.GetPreviousContext: TParseContext;
begin
  result := FPreviousContext;
end;

function TContextStack.IsWithin(c: TParseContext): boolean;
var
  t: integer;
  cc: TParseContext;
begin
  result := false;
  for t := 0 to Fcontexts.count-1 do begin
    cc := TParseContext(self.Fcontexts[t]);
    if c = cc then begin
      result := true;
      break;
    end;
  end;
end;

function TContextStack.Pop: TParseContext;
begin
  FPreviouscontext := GetCurrentcontext;
  Fcontexts.Delete(Fcontexts.count-1);
  FSymbolcontexts.Delete(FSymbolcontexts.count-1);
  result := GetCurrentContext;
  Popped;
end;
procedure TContextStack.Popped;
begin
  if assigned(OnStackPop) then
    OnStackPop;

end;

//------------------------------------------------------------------------------
procedure TContextStack.Push(context: TParseContext; symbolcontext: TSymbol = nil);
begin
  FPreviousContext := context;
  Fcontexts.Add(pointer(context));
  FSymbolContexts.add(symbolcontext);
  Pushed;
end;
procedure TContextStack.Pushed;
begin
  if assigned(OnStackPush) then
    OnStackPush;

end;

//------------------------------------------------------------------------------
destructor Tcompiler.Destroy;
begin
  Fstack.free;
  inherited;
end;
//------------------------------------------------------------------------------
procedure Tcompiler.Comment;
begin

//TODO -cunimplemented: unimplemented block
end;

//------------------------------------------------------------------------------
procedure Tcompiler.Write(sString: ansistring);
begin
  FOutput := FOutput + sString;
end;
//------------------------------------------------------------------------------
function Tcompiler.AtEndOfPattern(sPattern: ansistring;
  bCaseSensitive: boolean): boolean;
var
  sScope: ansistring;
begin
  sScope := copy(FInput, position - length(sPattern), length(sPattern));

  if bCaseSensitive then begin
    sScope := lowercase(sScope);
    sPattern := lowercase(sPattern);
  end;

  result := sScope = sPattern;

end;

//------------------------------------------------------------------------------
function TCompiler.GetAdvanceWasBlocked: boolean;
begin
  result :=  LastPosition = POsition;
end;

function Tcompiler.GetCurChar: AnsiChar;
begin
  result := FInput[position];
end;
//------------------------------------------------------------------------------
procedure Tcompiler.BufferWrite(sString: ansistring);
begin
  FBuffer := Fbuffer + sString;
end;
//------------------------------------------------------------------------------
function Tcompiler.AtBeginningOfPattern(sPattern: ansistring;
  bCaseSensitive: boolean): boolean;
var
  sScope: ansistring;
begin
  sScope := copy(FInput, position, length(sPattern));

  if bCaseSensitive then begin
    sScope := lowercase(sScope);
    sPattern := lowercase(sPattern);
  end;

  result := sScope = sPattern;
end;
//------------------------------------------------------------------------------
procedure TContextStack.SetCurrentContext(const Value: TParseContext);
begin
  FPreviousContext := GetCurrentcontext; 
  Fcontexts[FContexts.count-1] := pointer(value);
  Changed;
end;
//------------------------------------------------------------------------------
procedure TContextStack.SetCurrentSymbolContext(const Value: TSymbol);
begin
  FSymbolContexts[FSymbolContexts.count-1] := value;
  Changed;
end;
//------------------------------------------------------------------------------
procedure Tcompiler.Debug;
begin
  If Assigned(OnDebug) then
    OnDebug(self);

end;
procedure TCompiler.DebugEvent(sMessage, sColor: ansistring);
begin
  Write('<font color="'+sColor+'">&lt;debug msg="'+sMessage+'"/&gt;</font>');

end;

//------------------------------------------------------------------------------
function ContextToString(pc: TParseContext): ansistring;
begin
  case pc of
   pcNone: result :='pcNone';
    pcProgram: result :='pcProgram';
    pcProgramBody: result :='pcProgramBody';
    pcUnit: result :='pcUnit';
    pcUnitName: result :='pcUnitName';
    pcUses: result :='pcUses';
    pcVar: result :='pcVar';
    pcConst: result :='pcConst';
    pcType: result :='pcType';
    pcTypeDefinition: result :='pcTypeDefinition';
    pcInterface: result :='pcInterface';
    pcImplementation: result :='pcImplementation';
    pcFunctionImpName: result :='pcFunctionImpName';
    pcCodeBlock: result :='pcCodeBlock';
    pcLibrary: result :='pcLibrary';
    pcFunction: result := 'pcFunction';
    pcProcedure: result := 'pcProcedure';
    pcFunctionDeclaration: result :='pcFunctionDeclaration';
    pcFunctionName: result :='pcFunctionName';
    pcFunctionParam: result :='pcFunctionParam';
    pcFunctionResult: result :='pcFunctionResult';
    pcFunctionBody: result :='pcFunctionBody';
    pcFunctionDeclarationName: result :='pcFunctionDeclarationName';
    pcInitialization: result :='pcInitialization';
    pcClassDefinition: result :='pcClassDefinition';
    pcRecordDefinition: result :='pcRecordDefinition';
    pcClassDefinitionHeader: result :='pcClassDefinitionHeader';
    pcFinalization: result :='pcFinalization';
    pcComment: result :='pcComment';
    pcLineComment: result :='pcLineComment';
    pcCodeComment: result :='pcCodeComment';
    pcString: result :='pcString';
    pcParens: result := 'pcParens';
    pcPropertyDeclaration: result := 'pcPropertyDeclaration';
    pcPropertyDeclarationName: result := 'pcPropertyDeclarationName';
    pcPropertyDeclarationIndex: result := 'pcPropertyDeclarationIndex';
    pcPropertyDeclarationResult: result := 'pcPropertyDeclarationResult';
    pcPropertyDeclarationWrite: result := 'pcPropertyDeclarationWrite';
    pcPropertyDeclarationRead: result := 'pcPropertyDeclarationRead';
    pcBrackets: result := 'pcBrackets';
    pcDefine: result := 'pcDefine';
    pcDirective: result := 'pcDirective';
    pcStructName: result := 'pcStructName';
    pcStruct: result := 'pcStruct';
    pcStructDoc: result := 'pcStructDoc';
    pcDecorations: result := 'pcDecorations';
    pcStorageSymbol: result := 'pcStorageSymbol';
    pcUnitDocumentation: result := 'pcUnitDocumentation';
    

  else
    result := 'unknown '+inttostr(ord(pc)) ;
  end;
end;
//------------------------------------------------------------------------------
procedure Register;
begin
//  RegisterComponents('Booger', [TPascalCompiler]);
end;
//------------------------------------------------------------------------------
constructor TPascalCompiler.create;
begin
  inherited;
  FProject := nil;
  FcurrentUnit := nil;
  FCurrentSymbol := nil;
  FcurrentFunction := '';
  fCurrentUnit := nil;
  FCurrentSignature := '';
  self.FLastCommittedFunction := nil;
  self.FLastCommittedProperty := nil;
  FCCode := TStringList.create;

end;
//------------------------------------------------------------------------------
procedure TCompiler.BufferChar;
begin
  bufferwrite(curchar);
end;

procedure TPascalCompiler.BeforeCompile;
begin
  inherited;
  SetupCCode;
end;

procedure TPascalCompiler.CommitClass(sName: ansistring);
var
  sym: TClassDefinition;
begin
  if sName ='' then
    exit;


  write('&lt;***TYPE COMMITED:'+sName+'***&gt;');

  if lowercase(sName) = lowercase(self.CurrentUnit.ClassName) then
    raise Exception.create('Class Name cannot match unit name: '+sName);


  sym := TClassDefinition.create(Trim(sName), project, self.CurrentUnit);

  project.Symbols.Add(sym);

  ClearBuffer;
  self.LastTypeName := '';
  self.Currentsymbol := sym;

end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.RecognizeType(sName: ansistring);
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
procedure TPascalCompiler.CommitType(sName: ansistring);
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

  sym := TClassDefinition.create(trim(sName), project, self.CurrentUnit);

  project.Symbols.Add(sym);
  ClearBuffer;
  self.LastTypeName := '';

  self.Currentsymbol := sym;

end;

//------------------------------------------------------------------------------
procedure TPascalCompiler.DefineUnit(sName: ansistring);
var
  sym: TUnitDefinition;
begin


  if self.Project.Symbols.HasSymbol(sName) then
    raise Exception.create('Unit name '+sName+' already exists in project');

  sym := TunitDefinition.create(sNAme, project, nil);

  CurrentUnit := sym;


  self.Project.Symbols.Add(sym);

  ClearBuffer;

  self.CurrentSymbol := sym;

end;
destructor TPascalCompiler.destroy;
begin
  fCCode.free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchClassDefinition;
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

procedure TPascalCompiler.DispatchRecordDefinition;
begin
  if AtEndofWord('end') then
    stack.pop;

end;

//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchCodeComment;
begin
  if AtEndOfPattern('*)') then begin
    stack.Pop;
    blockAdvance;
  end;


end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchComment;
begin
  if AtEndOfPattern('}') then begin
    stack.Pop;
    blockAdvance;
  end;

end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchContext;
begin
//  GLOG.Debug('ln:'+inttostr(linenumber)+' col:'+inttostr(column)+' Ctx: '+ContextToString(stack.Current));
  //if anything but ansistring, determine comment states
  if not (stack.current in [pcString, pcComment, pcLineComment, pcCodeComment]) then begin
    if AtEndofpattern('{') then
      stack.Push(pcComment);

    if AtEndofpattern('//') then
      stack.Push(pcLineComment);

    if AtEndofpattern('(*') then
      stack.Push(pcCodeComment);
  end;

  if not (stack.current in [pcString, pcComment, pcLineComment, pcCodeComment]) then begin
    if AtEndofpattern('''') then begin
      stack.Push(pcString);
      exit;
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
    pcStruct: DispatchStruct;


  else
    raise Exception.create('Context not handled');
  end;
end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchImplementation;
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
procedure TPascalCompiler.DispatchInterface;
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
procedure TPascalCompiler.DispatchLineComment;
begin
  if stack.Parent in [pcClassDefinitionHeader, pcUnit] then begin

    self.Doc(self.CurChar);

    if CurChar in [#13,#10] then begin
      self.DocNewLine;
    end;
  end
  else if stack.Parent in [pcClassDefinition, pcFunction, pcProcedure] then begin
    if self.LastCommittedFunction <> nil then begin
      self.FuncDoc(self.CurChar);

      if Curchar in [#13,#10] then begin
        self.FuncDocNewLine;
      end;
    end else
    if self.LastCommittedProperty <> nil then begin
      self.PropDoc(self.CurChar);

      if Curchar in [#13,#10] then begin
        self.PropDocNewLine;
      end;
    end;
  end;


  if curchar in [#13,#10] then begin
    stack.Pop;
    //BlockAdvance;
  end;


end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchRoot;
begin
  //look for either "project" or "unit" or "library"
  if AtEndofWord('unit', false) then begin
    stack.Push(pcUnit);
    stack.Push(pcunitName);
  end;

  if AtEndofWord('program', false) then begin
    WriteCProgramHeader();
    stack.Push(pcProgram);
    stack.Push(pcunitName);
  end;

  if AtEndofWord('library', false) then begin
    WriteCLibraryHeader();
    stack.Push(pcProgram);
    stack.Push(pcunitName);
  end;


end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchType;
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
    currentsignature := '';
    ClearBuffer;
    stack.pop;
    blockAdvance;
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





end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchTypeDefinition;

begin
  if AtEndOfWord('class') and not AtEndOfWord('class of') then begin
    stack.pop;
    stack.Push(pcClassDefinitionHeader);
    CommitType(LastTypeName);

    if curchar = '(' then
      stack.push(pcParens);


  end else
  if AtWord('record') then begin
    stack.pop;
    stack.Push(pcRecordDefinition);
    //CommitType(LastTypeName);
  end else
  if curchar = ';' then begin
    stack.pop;
    //CommitType(LastTypeName);
  end else
  if curchar = '(' then
    stack.push(pcParens);


end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchUnit;
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


  if AtEndOfWord('var') then begin
    stack.Push(pcVar);
  end;

  if AtEndOfWord('begin') then begin
    if stack.CurrentContext = pcProgram then begin
      stack.Push(pcCodeBlock);
      WriteC('int main(void)',true);
    end else begin
      raise ESyntax.create('code block without function, expecting function, var or other keyword');
    end;

    WriteC('{', true);
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
function TCompiler.AtEndOfWord(sWord: ansistring;
  bCaseSensitive: boolean): boolean;
var
  sScope: ansistring;
  sPrev: ansistring;
begin
  //prevent error at beginning of file
//  if position = 1 then begin
//    result := false;
//    exit;
//  end;

  //copy X AnsiCharacters out of code to check against pattern
  //copy up to PreVIOUS AnsiCharacter
  sScope := copy(FInput, (position) - length(sWord), length(sWord));

  if position -length(sWord)<=1 then
    sPrev := #0
  else
    sPrev := copy(FInput, (position-1) - length(sWord), 1);

  if sPrev = '' then
    sPrev := #0;

  //convert if case-insensitivity required
  if NOT bCaseSensitive then begin
    sScope := lowercase(sScope);
    sWord := lowercase(sWord);
  end;

  //result = true if pattern match and word break found
  result := (sScope = sWord) and AtWordBreak and (sPrev[1] in wordbreaks);




end;
//------------------------------------------------------------------------------
function TCompiler.AtWord(sWord: ansistring;
  bCaseSensitive: boolean): boolean;
var
  sScope: ansistring;
  cAfter: AnsiChar;
  sAfter: ansistring;
begin
  //prevent error at beginning of file
//  if position = 1 then begin
//    result := false;
//    exit;
//  end;

  //copy X AnsiCharacters out of code to check against pattern
  sScope := copy(FInput, position, length(sWord));

  //convert if case-insensitivity required
  if not bCaseSensitive then begin
    sScope := lowercase(sScope);
    sWord := lowercase(sWord);
  end;

  //result = true if pattern match and word break found
  sAfter := copy(FInput, position+length(sWord), 1);
  if sAfter <> '' then
    cAfter := sAfter[1]
  else
    cAfter := #0;

  result := (sScope = sWord) and (cAfter in wordbreaks);

  //one last check
  //if still good, check to make sure the AnsiCharacter before the word is a wordbreak.
  if (self.position = 1) or (self.Input[position-1] in WordBreaks) then
    result := result
  else
    result := false;


end;
//------------------------------------------------------------------------------
function TCompiler.AtWordBreak: boolean;
begin
  result := curchar in wordbreaks;
end;
//------------------------------------------------------------------------------
procedure TCompiler.ClearBuffer;
begin
  FBuffer := '';
end;
procedure TCompiler.CloseFont;
begin
  Write('</font>');
end;

//------------------------------------------------------------------------------
function TCompiler.GetCurrentWord: ansistring;
var
  t: integer;
begin
  t := position;
  while (t < length(fInput)) and not (Finput[t] in wordbreaks) do begin
    inc(t);
  end;

  result := copy(FInput, position, t-position);



end;
//------------------------------------------------------------------------------
procedure TCompiler.Advance(iBy: nativeint);
var
  t: nativeint;
begin


  if not FBlockAdvance then begin
    for t:= 1 to iBy do begin
      inc(position);
      inc(column);
      if CurChar = #10 then begin
        inc(linenumber);
        column := 0;
      end;
    end;
  end;


  FBlockAdvance := false;
end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchUses;
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
function TPascalCompiler.GetCurrentClass: TMatureConstruct;
begin
  result := TMatureConstruct(Fcurrentsymbol);

end;

procedure TPascalCompiler.DispatchConst;
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

procedure TPascalCompiler.Doc(sString: ansistring);
begin
  if self.CurrentSymbol <> nil then begin
    currentsymbol.Documentation.text :=     copy(currentsymbol.Documentation.text, 1, length(currentsymbol.documentation.Text)-2)+ sString;
  end;
end;

procedure TPascalCompiler.DocNewLine;
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

procedure TPascalCompiler.DispatchClassDefinitionHeader;
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
procedure TPascalCompiler.SetCurrentUnit(const Value: TunitDefinition);
begin
  FCurrentunit := Value;
  self.Currentsymbol := fCurrentunit;
end;
procedure TPascalCompiler.SetupCCode;
begin
  WriteC('#include <iostream>');
//  WriteC('#include "d2cbuiltin.h"');
end;

procedure TPascalCompiler.WriteC(s: string; start_new_line: boolean = true);
begin
  if (start_new_line) then
    FCCode.add(s)
  else begin
    if FCCode.count > 0 then
      FCcode[FCCode.count-1] := FCcode[FCCode.count-1]+s
    else
      FCCode.add(s);
  end;
end;

procedure TPascalCompiler.WriteCLibraryHeader;
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TPascalCompiler.WriteCProgramHeader;
begin
  WriteC('#include builtin.cpp');
end;

//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchParens;
begin
  if stack.Parent in [pcFunctionDeclaration, pcFunctionDeclarationName] then
    currentsignature := currentsignature + curchar;

  if curchar = ')' then
    stack.pop;
end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchFunctionDeclaration;
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
procedure TPascalCompiler.CommitFunction;
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
procedure TPascalCompiler.RecommitFunction;
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
//------------------------------------------------------------------------------
function TCompiler.GetPreviousWord: ansistring;
var
  t: integer;
begin
  if position = 0 then begin
    result := '';
    exit;
  end;

  t := position-1;
  while (t < length(fInput)) and not (Finput[t] in wordbreaks) do begin
    dec(t);
  end;

  result := copy(FInput, t, position-t);

end;
procedure TCompiler.HandleOnStackChange;
begin
  Write('<font color="#FF0000">&lt;/'+contextToString(stack.previouscontext)+'&gt;</font>');
  Write('<font color="#FF0000">&lt;'+contextToString(stack.current)+'&gt;</font>');
end;

procedure TCompiler.HandleOnStackPop;
begin
  Write('<font color="#FF0000">&lt;/'+contextToString(stack.previouscontext)+'&gt;</font>');
//  Write('<font color="#FF0000">&lt;'+contextToString(stack.current)+'&gt;</font>');

end;

procedure TCompiler.HandleOnStackPush;
begin
  Write('<font color="#FF0000">&lt;'+contextToString(stack.current)+'&gt;</font>');

end;

procedure TCompiler.MOveThroughPattern(sPattern: ansistring);
begin
  write(sPattern);
  advance(length(sPattern));

  
end;

procedure TCompiler.OpenFont(scolor: ansistring);
begin
  Write('<font color="'+sColor+'">');
end;

//------------------------------------------------------------------------------
procedure TPascalCompiler.FuncDoc(sString: ansistring);
begin
  if self.CurrentSymbol <> nil then begin
    lastCommittedFunction.Documentation.text :=     copy(lastCommittedFunction.Documentation.text, 1, length(lastCommittedFunction.Documentation.text)-2)+ sString;

  end;
end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.FuncDocNewLine;
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
procedure TPascalCompiler.DispatchPropertyDeclaration;
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
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchBrackets;
begin
  if stack.Parent in [pcPropertyDeclaration, pcPropertyDeclarationName, pcPropertyDeclarationIndex] then
    currentsignature := currentsignature + curchar;

  if curchar = ']' then
    stack.pop;

end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.DispatchCodeBlock;
var
  sym: TSymbol;
begin
  if AtWord('begin') or AtWord('case') or AtWord('try') then begin
    self.Write('$newblock$');
    stack.push(pcCodeBlock);
    self.WriteC('{',true);
  end;

  if AtWord('end') then begin
    self.Write('$endblock$');
    stack.pop;
    self.WriteC('}',true);
    self.Advance(3);
  end;

  if AtEndOfWord('end') then begin
    self.Write('$endblock$');
    stack.pop;
    self.WriteC('}',true);
  end;

  //else to PARSE CODE
  //- we expect to see a variable to store something in.  This variable must be
  //- resolvable... if we cannot resolve this variable then we throw an exception
  //OR
  //- we expect to see the name of a procedure.
  //BASICALLY we are looking for a SYMBOL or KEYWORD
  //- a symbol will be
  //    - a type (for class procedures)
  //    - a procedure/function
  //    - a variable, instance of a type
  //- flow keywords will be
  //    - for
  //    - case
  //    - repeat
  //    - while
  //    - try
  //    - goto


  if CurrentWord <> '' then begin
    ConsoleLog('Current word:'+CurrentWord);
    sym := self.Project.Symbols.FindSymbol(CurrentWord);
    if sym = nil then
      raise EComponentError.Create('Unknown symbol:'+ CurrentWord);

    if sym is TFunctionDefinition then begin
    end;

  end;






end;
//------------------------------------------------------------------------------
procedure TPascalCompiler.CommitProperty;
var
  prop: TPropertyDefinition;
begin
  write('$commit$');
  //frmDebug.ShowMessage(currentSignature);

  if self.currentclass = nil then begin
    log(self,'WTF. CurrentClass is NIL on CommitProperty');
    exit;
  end;

  if currentproperty = '' then begin
    log(self,'WTF. CurrentProperty is NIL on CommitProperty');
    exit;
  end;

  IF SELF= nil then
    raise Exception.create('wtf, SELF is nil!');


  IF SELF.CurrentClass = nil then
    raise Exception.create('wtf, SELF.currentclass is nil!');

  prop := self.CurrentClass.AddProperty(currentProperty);
  prop.Signature := currentSignature;

  self.LastCommittedProperty := prop;



end;

procedure TPascalCompiler.PropDoc(sString: ansistring);
begin
  if self.CurrentSymbol <> nil then begin
    lastCommittedProperty.Documentation.text :=     copy(lastCommittedProperty.Documentation.text, 1, length(lastCommittedProperty.Documentation.text)-2)+ sString;
  end;
end;

procedure TPascalCompiler.PropDocNewLine;
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

procedure TPascalCompiler.DispatchFunction;
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
procedure TPascalCompiler.DisPatchFunctionImpName;
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
procedure TPascalCompiler.FindFunction(sName: ansistring);
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

procedure TPascalCompiler.SetCurrentClass(const Value: TMatureConstruct);
begin
  FCurrentSymbol := value;
end;

procedure TPascalCompiler.DispatchVarConst;
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

procedure TCompiler.BeforeCompile;
begin
  //
end;

procedure TCompiler.BlockAdvance;
begin
  FBlockAdvance := true;
end;


procedure TPascalCompiler.DispatchString;
begin
  if AtEndofpattern('''') then
    stack.pop;

end;


procedure TPascalCompiler.DispatchStruct;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TPascalCompiler.DispatchStructName;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TPascalCompiler.Compile(sFile: ansistring): boolean;
var
  sym: TSymbol;
begin
  result := inherited Compile(sfile);

  if assigned(FCurrentUnit) then
    FCurrentUnit.Debug := output;


end;

end.
