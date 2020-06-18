unit RDTPGen_client_compiler;

{$INLINE AUTO}

interface

uses
  debug, betterobject, classes, stringx, sysutils, dir, systemx, typex;

const
  RQFILE_LOCATION_RELATIVE_TO_CSHARP = '..\..\..\..\..\Pascal\64\RDTP_DEFINITIONS\';
  RQFILE_LOCATION_RELATIVE_TO_DELPHI = '..\RDTP_DEFINITIONS\';
type
  TParamType = (ptNormal, ptOut, ptVar, ptConstPointer);
  TRDTPClientGenerator = class(TBetterObject)
  private
    procedure AddPublicMember(sLine: string);
    procedure AddPrivateMember(sLine: string);
    procedure AddProtectedMember(sLine: string);
  protected
    FInput: string;
    FOutput: string;
    FTemplate: string;
    FFileName: string;
    FDIRECTIVES: TStringList;
    FInterfaceBlock: TStringlist;
    FImplementationBlock: TStringlist;
    FImplementationBlock2: TStringlist;
    FCSharpClass: TStringlist;
    FCSharpCallbackDispatch: TStringList;
    FTYPE: string;
    FCLASS: string;
    FAncestor: string;
    FTemplateFileName: string;
    FImpLib: string;
    FServiceName: string;
    FImplmentationBlock2: TStringLIst;
    FPrivatemembers: TStringlist;
    FProtectedMembers: TStringList;
    FPublicMembers: TStringlist;
    FVAR: TStringList;
    FUses: TStringList;
    FUses_backend: TStringlist;
    FEndNames: TStringList;
  public
    constructor create;override;
    destructor destroy;override;

    procedure process(sFileName: string);
    procedure processTodo(sFileName: string);
    procedure processMacros(sFileName: string);

    procedure Process_Class(sParams: string);
    procedure Process_Type(sParams: string);
    procedure Process_RQ(sParams: string);
    procedure Process_CB(sParams: string);
    procedure Process_RQFILE(sParams: string; bCSharp: boolean);

    //client-to-server requests
    procedure Process_Client_RQ(sConstant, sFunctionName, sResultType: string; sParams: string);
    procedure Process_Async_Client_RQ(sConstant, sFunctionName, sResultType: string; sParams: string);
    procedure Process_Async_Client_RSP(sConstant, sFunctionName, sResultType: string; sParams: string);
    procedure Process_Server_RQ(sConstant, sFunctionName, sResultType: string; sParams: string);
    procedure Process_ImpLib_RQ(sConstant, sFunctionName, sResultType: string; sParams: string);

    //server-to-client requests
    procedure Process_Client_CB(sConstant, sFunctionName, sResultType: string; sParams: string);//
    procedure Process_Server_CB(sConstant, sFunctionName, sResultType: string; sParams: string);//
    procedure Process_Async_Server_CB(sConstant, sFunctionName, sResultType: string; sParams: string);
    procedure Process_Async_Server_CB_RSP(sConstant, sFunctionName, sResultType: string; sParams: string);
    procedure Process_Client_ImpLib_CB(sConstant, sFunctionName, sResultType: string; sParams: string);

    procedure Process_Ancestor(sParams: string);
    procedure Process_Template(sParams: string; bCSharp: boolean);
    function HasOverloadAlready(sFunctionName: string; list: TStringlist): boolean;


    //delphi
    procedure AddToInterface(sLine: string);
    procedure AddToImplementation(sLine: string);
    procedure AddToImplementation2(sLine: string);
    procedure AddToVar(sLine: string);

    //csharp *******C SHARP ***********************************************
    procedure AddToCBDispatch(sLine: string);
    procedure AddToClass(sline: string);

    procedure processCSharp(sFileName: string);
    procedure Process_RQ_CSharp(sParams: string);
    procedure Process_CB_CSharp(sParams: string);


    //client-to-server requests *******C SHARP ******
    procedure Process_Client_RQ_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);
    procedure Process_Server_RQ_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);//implement

    (*not needed*)procedure Process_ImpLib_RQ_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);//implement

    //server-to-client requests *******C SHARP ******
    procedure Process_Client_CB_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);//
    procedure Process_Server_CB_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);//

    //C# Async client->server
    procedure Process_Async_Client_RQ_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);
    procedure Process_Async_Client_RSP_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);

    //C# Async server->client
    procedure Process_Async_Server_CB_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);
    procedure Process_Async_Server_CB_RSP_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);

    (*not needed*)procedure Process_Client_ImpLib_CB_Csharp(sConstant, sFunctionName, sResultType: string; sParams: string);


  end;

procedure ProcessUnit(sFileName: string);
procedure ProcessDir(sDir: string);
function GetParamType(var sName: string): TParamType;
function GetParamModifier(pt: TParamType): string;

function GetConstantValue(sConstant: string): string;

function GetNumberOfReturns(sResultType: string; sParamLIst: string): integer;

implementation

function GetConstantValue(sConstant: string): string;
begin
  result := inttostr(strtoint(sConstant));

end;
function GetParamModifier(pt: TParamType): string;
begin
  case pt of
    ptVar: result := 'var ';
    ptOut: result := 'out ';
  else
    result := '';
  end;

end;

function GetNumberOfReturns(sResultType: string; sParamLIst: string): integer;
var
  sRemainder: string;
  sLeft, sRight: string;
begin
  result := 0;

  if lowercase(sResultTYpe) <> 'void' then
    inc(result);

  sRemainder := sParamList;
  while SplitString(sRemainder, ':', sLeft, sRight) do begin
    SplitString(sRight, ' ', sRight, sRemainder);
    if GetParamType(sLeft) <> ptNormal then
      inc(result);
  end;

end;


function GetParamType(var sName: string): TParamType;
var
  sFirstChar: string;
begin
  result := ptNormal;
  sFirstChar := zsubcopy(sName,0,1);//ok

  if sFirstChar = '&' then begin
    result := ptVAr;
    sName := zsubcopy(sName, 1,99999);//ok
  end;

  if sFirstChar = '^' then begin
    result := ptOut;
    sName := zsubcopy(sName, 1,99999);//ok
  end;

  if sFirstChar = '*' then begin
    result := ptConstPointer;
    sName := zsubcopy(sName, 1,99999);//ok
  end;




end;

procedure ProcessUnit(sFileName: string);
var
  gen: TRDTPClientGenerator;
begin
  gen := TRDTPClientGenerator.create;
  try
    gen.process(sFileName);
    gen.processTodo(sFileName);
    gen.processMacros(sFileName);

  finally
    gen.free;
  end;

end;

{ TRDTPClientGenerator }

procedure TRDTPClientGenerator.AddToCBDispatch(sLine: string);
begin
  self.FCSharpCallbackDispatch.add(sLine);
end;

procedure TRDTPClientGenerator.AddToClass(sline: string);
begin
  self.FImplementationBLock.Add(sLine);
end;

procedure TRDTPClientGenerator.AddToImplementation(sLine: string);
begin
  FImplementationBlock.add(sLine);
end;

procedure TRDTPClientGenerator.AddToImplementation2(sLine: string);
begin
  FImplementationBlock2.add(sLine);
end;

procedure TRDTPClientGenerator.AddPublicMember(sLine: string);
begin
  fpublicmembers.add(sLine);
end;

procedure TRDTPClientGenerator.AddProtectedMember(sLine: string);
begin
  FProtectedMembers.add(sLine);
end;

procedure TRDTPClientGenerator.AddPrivateMember(sLine: string);
begin
  FPrivatemembers.add(sLine);
end;



procedure TRDTPClientGenerator.AddToInterface(sLine: string);
begin
  FInterfaceBlock.add(sLIne);
end;

procedure TRDTPClientGenerator.AddToVar(sLine: string);
begin
  FVAR.add(sLine);
end;

constructor TRDTPClientGenerator.create;
begin
  inherited;
  FDirectives := TStringlist.create;
  FInterfaceBlock := TStringlist.create;
  FImplementationblock := Tstringlist.create;
  FImplementationblock2 := TStringList.create;
  FPrivatemembers := TStringList.create;
  FPublicMembers := TStringList.create;
  FProtectedMembers := TStringList.create;

  self.FCSharpClass := TStringlist.create;
  self.FCSharpCallbackDispatch := TStringlist.create;
  FEndNames := TStringList.create;


  FVAr := TStringLIst.create;
  FUSEs := TStringList.create;
  FUSES_BACKEND := TStringList.Create;
end;

destructor TRDTPClientGenerator.destroy;
begin
  FInterfaceblock.free;
  FImplementationBlock.free;
  FImplementationblock2.free;
  FPrivatemembers.Free;
  FPublicMembers.Free;
  FProtectedMembers.Free;
  self.FCSharpClass.free;
  self.FCSharpCallbackDispatch.free;
  FEndNames.free;

  FDirectives.free;
  FVar.free;
  FUses.free;
  FUses_BACKEND.free;
  inherited;
end;

function TRDTPClientGenerator.HasOverloadAlready(sFunctionName: string;
  list: TStringlist): boolean;
var
  s: string;
begin
  s := lowercase(list.text);

  result := pos(lowercase(sFunctionName)+'(', s) > 0;


end;

procedure TRDTPClientGenerator.processCSharp(sFileName: string);
var
  sLine, sLeft, sRight, sParams, sJunk: string;
  t: integer;
  sVal: string;
begin
  Debug.Log('Processing...'+sFileName);
  FFileName := sFileName;
  FInput := LoadStringFromFile(sFileName);
  FEndNames.clear;


  //first split out the unit's directives
  SplitString(FInput, '{END}', sLeft, sRight);
  if not SplitString(sLeft, '{GEN}', sLeft, sVAl) then begin
//    Debug.ConsoleLog('Skipped: '+sFileName);
    exit;
  end;

  Debug.ConsoleLog('Process: '+sFileName);

  FDirectives.text := sVal;

  //now go through all the directives
  t:= 0;
  while t <= FDirectives.count-1 do begin
    sLine := FDirectives[t];
    if sLIne = '' then begin
      inc(t);
      continue;
    end;


    SplitString(sLine, '{', sLeft, sRight);
    SplitString(sRight, ' ', sLeft, sRight);
    SplitString(sRight, '}', sParams, sRight);
    if (zsubcopy(sLeft, 0,2) <> '//') and (sLeft <> '') then//ok
    if uppercase(sLeft) = 'TYPE' then begin
      Process_Type(sParams);
    end else
    if uppercase(sLEft) = 'CLASS' then begin
      Process_Class(sParams);
    end else
    if uppercase(sLEft) = 'TEMPLATE' then begin
      Process_Template(sParams,true);
    end else
    if uppercase(sLEft) = 'ANCESTOR' then begin
      Process_Ancestor(sParams);
    end else
    if uppercase(sLEft) = 'IMPLIB' then begin
      FIMPLIB := sParams;
    end else
    if uppercase(sLEft) = 'SERVICENAME' then begin
      FSERVICENAME := sParams;
    end else
    if uppercase(sLEft) = 'RQFILE' then begin
      Process_RQFILE(sParams,true);
    end else
    if uppercase(sLEft) = 'USES' then begin
    end else
    if uppercase(sLEft) = 'USES_BACKEND' then begin
//      FUSES_BACKEND.add(sParams);
    end else
    if uppercase(sLEft) = 'USING' then begin
      FUSES.add('using '+sParams+';');
    end else
    if uppercase(sLEft) = 'RQ' then begin
      Process_RQ_CSharp(stringreplace(sParams,';','',[rfReplaceAll]));
    end else
    if uppercase(sLEft) = 'CB' then begin
      Process_CB_CSharp(stringreplace(sParams,';','',[rfReplaceAll]));
    end else
      raise ECritical.create('command not understood:'+sLeft);

    inc(t);
  end;


  //now merge with template
  if uppercase(FType) = 'IMPLIB' then begin
    SplitStringNoCAse(FINPUT, '/*INTERFACE_START*/', sLeft, sRight);
    SplitStringNoCAse(sRight, '/*INTERFACE_END*/', sJunk, sRight);
    FOUTPUT := sLeft + '/*INTERFACE_START}'#13#10+FINTERFACEBLOCK.text+#13#10+'/*INTERFACE_END*/'+sRight;

  end else
  if uppercase(FType) = 'CLIENT_IMPLIB' then begin
    SplitStringNoCAse(FINPUT, '/*INTERFACE_START*/', sLeft, sRight);
    SplitStringNoCAse(sRight, '/*INTERFACE_END*/', sJunk, sRight);
    FOUTPUT := sLeft + '/*INTERFACE_START*/'#13#10+FINTERFACEBLOCK.text+#13#10+'/*INTERFACE_END*/'+sRight;

  end else
  begin
    SplitString(FInput, '*/', FOutput, sRight);
    SplitString(FTemplate, '*/', sLeft, FTemplate);
    FOUTPUT := FOUTPUT+'*/'#13#10+FTemplate;

    if FIMPLIb <> '' then
      FIMPLIB := FIMPLIB + ', ';
    FOUTPUT := stringreplace(FOUTPUT, 'T__RDTP_CLIENT_CLASS', FCLASS, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, 'T__RDTP_SERVER_CLASS', FCLASS, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '/*CLASS_INTERFACE*/', #13#10+FINTERFACEBLOCK.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '/*CLASS_IMPLEMENTATION*/', #13#10+self.FImplementationblock.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '/*IMPLEMENTATION2*/', #13#10+self.FImplementationblock2.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '/*ANCESTOR*/', '('+FAncestor+')', [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '/*SERVICENAME*/', FServiceName, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '/*SERVICE_NAME*/', FServiceName, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '/*IMPLIB*/', FIMPLIB, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '/*VAR*/', FVAR.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '/*USES*/', FUSes.text, [rfReplaceAll, rfIgnoreCase]);


  end;


  if FINPUT <> FOUTPUT then begin
    //if (not (uppercase(FTYpe)='IMPLIB')) and (not (uppercase(FTYpe)='CLIENT_IMPLIB')) then
    FileSetAttr(FFileName, faArchive);
    SaveStringAsFile(FFileName, FOutput);
    if not (uppercase(FTYpe)='IMPLIB') and (not (uppercase(FTYpe)='CLIENT_IMPLIB')) then
      FileSetAttr(FFileName, faARchive+faReadOnly);
    FFileName := changefileext(FFileName, '.obj');
    if fileexists(FFileName) then
      deletefile(FFileName);

  end;

end;

procedure TRDTPClientGenerator.processMacros(sFileName: string);
var
  sl: TStringlist;
  t: integer;
  bIN: boolean;
begin
  exit;
  FFileName := sFileName;
  if (lowercase(extractfileext(FFileName)) = '.cs') then begin
    exit;
  end;
  FInput := LoadStringFromFile(sFileName);
  sl :=TStringlist.create;
  try
    sl.text := FInput;

    bIN := false;
    t := 0;
    //e.g.

    {
      {% LOCK():
        Lock;
        try
        finally
          Unlock;
        end;
      %}
//    }



    while t<sl.count do begin
    end;

    FOUTPUT := sl.text;

  finally
    sl.free;
  end;

  if FINPUT <> FOUTPUT then begin
    try
      SaveStringAsFile(FFileName, FOutput);
    except
    end;
    FFileName := changefileext(FFileName, '.dcu');
    if fileexists(FFileName) then
      deletefile(FFileName);

  end;


end;

procedure TRDTPClientGenerator.process(sFileName: string);
var
  sLine, sLeft, sRight, sParams, sJunk: string;
  t: integer;
  sVal: string;
begin
  FFileName := sFileName;
  if (ExtractfileExt(sfileName)='.cs') then begin
    ProcessCSharp(sFileName);
    exit;
  end;
  FInput := LoadStringFromFile(sFileName);


  //first split out the unit's directives
  SplitString(FInput, '{END}', sLeft, sRight);
  if not SplitString(sLeft, '{GEN}', sLeft, sVAl) then begin
//    Debug.ConsoleLog('Skipped: '+sFileName);
    exit;
  end;
  Debug.ConsoleLog('Process: '+sFileName);

  FDirectives.text := sVal;

  //now go through all the directives
  t:= 0;
  while t <= FDirectives.count-1 do begin
    sLine := FDirectives[t];
    if sLIne = '' then begin
      inc(t);
      continue;
    end;

    SplitString(sLine, '{', sLeft, sRight);
    SplitString(sRight, ' ', sLeft, sRight);
    SplitString(sRight, '}', sParams, sRight);
    if (zsubcopy(sLeft, 0,2) <> '//') and (sLeft <> '') then//ok
    if uppercase(sLeft) = 'TYPE' then begin
      Process_Type(sParams);
    end else
    if uppercase(sLEft) = 'CLASS' then begin
      Process_Class(sParams);
    end else
    if uppercase(sLEft) = 'TEMPLATE' then begin
      Process_Template(sParams, false);
    end else
    if uppercase(sLEft) = 'ANCESTOR' then begin
      Process_Ancestor(sParams);
    end else
    if uppercase(sLEft) = 'IMPLIB' then begin
      FIMPLIB := sParams;
    end else
    if uppercase(sLEft) = 'SERVICENAME' then begin
      FSERVICENAME := sParams;
    end else
    if uppercase(sLEft) = 'RQFILE' then begin
      Process_RQFILE(sParams,false);
    end else
    if uppercase(sLEft) = 'USES' then begin
      FUSES.add(sParams);
    end else
    if uppercase(sLEft) = 'USES_BACKEND' then begin
      FUSES_BACKEND.add(sParams);
    end else
    if uppercase(sLEft) = 'USING' then begin
    end else
    if uppercase(sLEft) = 'RQ' then begin
      Process_RQ(stringreplace(sParams,';','',[rfReplaceAll]));
    end else
    if uppercase(sLEft) = 'CB' then begin
      Process_CB(stringreplace(sParams,';','',[rfReplaceAll]));
    end else
      raise ECritical.create('command not understood:'+sLeft);

    inc(t);
  end;


  //now merge with template
  if uppercase(FType) = 'IMPLIB' then begin
    SplitStringNoCAse(FINPUT, '{INTERFACE_START}', sLeft, sRight);
    SplitStringNoCAse(sRight, '{INTERFACE_END}', sJunk, sRight);
    sLeft := StringReplace(sLeft, '{ANCESTOR}', '(T'+FSERVICENAME+'ServerBase)', [rfReplaceAll]);
    FOUTPUT := sLeft + '{INTERFACE_START}'#13#10+FINTERFACEBLOCK.text+#13#10+'{INTERFACE_END}'+sRight;

  end else
  if uppercase(FType) = 'CLIENT_IMPLIB' then begin
    SplitStringNoCAse(FINPUT, '{INTERFACE_START}', sLeft, sRight);
    SplitStringNoCAse(sRight, '{INTERFACE_END}', sJunk, sRight);
    FOUTPUT := sLeft + '{INTERFACE_START}'#13#10+FINTERFACEBLOCK.text+#13#10+'{INTERFACE_END}'+sRight;

  end else
  begin
    SplitString(FInput, 'interface', FOutput, sRight);
    SplitString(FTemplate, 'interface', sLeft, FTemplate);
    FOUTPUT := FOUTPUT+'interface'#13#10+FTemplate;

    if FIMPLIb <> '' then
      FIMPLIB := FIMPLIB + ', ';
    FOUTPUT := stringreplace(FOUTPUT, 'T__RDTP_CLIENT_CLASS', FCLASS, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, 'T__RDTP_SERVER_CLASS', FCLASS, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{CLASS_INTERFACE}', #13#10+FINTERFACEBLOCK.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{PRIVATE_MEMBERS}', #13#10+FPRIVATEMEMBERS.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{PUBLIC_MEMBERS}', #13#10+FPUBLICMEMBERS.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{PROTECTED_MEMBERS}', #13#10+FPROTECTEDMEMBERS.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{CLASS_IMPLEMENTATION}', FIMPLEMENTATIONBLOCK.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{IMPLEMENTATION2}', FIMPLEMENTATIONBLOCK2.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{ANCESTOR}', '('+FAncestor+')', [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{SERVICENAME}', FServiceName, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{IMPLIB}', FIMPLIB, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{VAR}', FVAR.text, [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{USES}', stringreplace(FUSES.text,#13#10,', ', [rfReplaceAll]), [rfIgnoreCase, rfReplaceAll]);
    FOUTPUT := stringreplace(FOUTPUT, '{USES_BACKEND}', stringreplace(FUSES_BACKEND.text,#13#10,', ', [rfReplaceAll]), [rfIgnoreCase, rfReplaceAll]);


  end;


  if FINPUT <> FOUTPUT then begin
    if (not (uppercase(FTYpe)='IMPLIB')) and (not (uppercase(FTYpe)='CLIENT_IMPLIB')) then
      FileSetAttr(FFileName, faArchive);
    SaveStringAsFile(FFileName, FOutput);
    if not (uppercase(FTYpe)='IMPLIB') and (not (uppercase(FTYpe)='CLIENT_IMPLIB')) then
      FileSetAttr(FFileName, faARchive+faReadOnly);
    FFileName := changefileext(FFileName, '.dcu');
    if fileexists(FFileName) then
      deletefile(FFileName);

  end;

end;

procedure TRDTPClientGenerator.processTodo(sFileName: string);
var
  sl: TStringlist;
  t: integer;
  bIN: boolean;
begin
  FFileName := sFileName;
  if (lowercase(extractfileext(FFileName)) = '.cs') then begin
    exit;
  end;
  FInput := LoadStringFromFile(sFileName);
  sl :=TStringlist.create;
  try
    sl.text := FInput;

    bIN := false;
    t := 0;
    while t<sl.count do begin
      if TrimStr(sl[t])='begin' then
        bIN := true
      else
      if (TrimStr(sl[t])='end;') and bIn then begin
        sl.Insert(t,'//TODO -cunimplemented: unimplemented block');
        sl.Insert(t,'  raise ECritical.create(''unimplemented'');');
        bIN := false;
      end else
      if (TrimStr(sl[t])<>'') then
        bIn := false;

      inc(t);
    end;

    FOUTPUT := sl.text;

  finally
    sl.free;
  end;

//  FOutput := StringReplace(FOutput, 'GetTicker(', 'GetTicker(', [rfReplaceAll, rfIgnoreCase]);
//  FOutput := StringReplace(FOutput, 'GetTicker,', 'GetTicker,', [rfReplaceAll, rfIgnoreCase]);
//  FOutput := StringReplace(FOutput, 'GetTicker;', 'GetTicker;', [rfReplaceAll, rfIgnoreCase]);
//  FOutput := StringReplace(FOutput, 'GetTicker)', 'GetTicker)', [rfReplaceAll, rfIgnoreCase]);
//  FOutput := StringReplace(FOutput, 'GetTicker ', 'GetTicker ', [rfReplaceAll, rfIgnoreCase]);
//  FOutput := StringReplace(FOutput, 'GetTicker-', 'GetTicker-', [rfReplaceAll, rfIgnoreCase]);
//  FOutput := StringReplace(FOutput, 'GetTicker+', 'GetTicker+', [rfReplaceAll, rfIgnoreCase]);



  if FINPUT <> FOUTPUT then begin
    try
      SaveStringAsFile(FFileName, FOutput);
    except
    end;
    FFileName := changefileext(FFileName, '.dcu');
    if fileexists(FFileName) then
      deletefile(FFileName);

  end;


end;

procedure TRDTPClientGenerator.Process_Ancestor(sParams: string);
begin
  FAncestor := sParams;
end;

procedure TRDTPClientGenerator.Process_Async_Client_RQ(sConstant, sFunctionName,
  sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin
  sParamList := '';


  //build param list for function definition
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';

    pt := GetParamType(sName);
    sVar := GetParamModifier(pt);

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    //do not include OUT parameters in list
    if not (pt = ptOUT) then begin
      if sPAramList <> '' then
        sParamList := sParamList+'; ';
      sParamLIst := sParamList+sVar+sName+':'+sType;
    end;
  end;

  //build interface
  sLine:= '    procedure '+sFunctionName+'_Async('+sParamList+');overload;virtual;';
  AddToInterface(sLine);


  //build implementation

  //-start with header

  AddToImplementation('//------------------------------------------------------------------------------');
  sLine := 'procedure '+FClass+'.'+sFunctionName+'_Async('+sParamList+');';
  AddToImplementation(sLine);
  AddToImplementation('var');
  AddToImplementation('  packet,outpacket: TRDTPPacket;');
  AddToImplementation('begin');
  AddToImplementation('  if not connect then');
  AddToImplementation('     raise ETransportError.create(''Failed to connect'');');
  AddToImplementation('  packet := NeedPacket;');
  AddToImplementation('  try');
  AddToImplementation('    packet.AddVariant('+sConstant+');');
  AddToImplementation('    packet.AddVariant(0);');
  AddToImplementation('    packet.AddString('''+FServiceName+''');');

  sRemainder := sParams;
  sReverse := '';
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+sName+':'+sType;

    if sTypeL = 'special' then begin
    end else begin
      //marshall variant-compatible type
      if pt <> ptOut then begin
//        sReverse := '    packet.AddVariant('+sName+');'#13#10+sReverse;
        AddToImplementation('    Write'+sType+'ToPacket(packet, '+sName+');');
      end;
    end;

  end;

//  AddToImplementation(sReverse);

//  if GetNumberOfReturns(sResultType, sParams) = 0 then
//    AddToImplementation('    if not TransactionThread.Transact(packet, true) then raise ECritical.create(''transaction failure'');')
//  else
//    AddToImplementation('    if not TransactionThread.Transact(packet) then raise ECritical.create(''transaction failure'');');

//  AddToImplementation('     InitConnectionClass;');

  if GetNumberOfReturns(sResultType, sParams) = 0 then
    AddToImplementation('    BeginTransact2(packet, outpacket,nil, true);')
  else
    AddToImplementation('    BeginTransact2(packet, outpacket,nil, false);');



  AddToImplementation('  except');
  AddToImplementation('    on E:Exception do begin');
  AddToImplementation('      e.message := ''RDTP Call Failed:''+e.message;');
  AddToImplementation('      raise;');
  AddToImplementation('    end;');
  AddToImplementation('  end;');
  AddToImplementation('end;');


end;

procedure TRDTPClientGenerator.Process_Async_Client_RQ_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin
  sParamList := '';

  //build param list
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';
    if sName[1] = '&' then begin //var parameter
      sVar := 'ref ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    if sName[1] = '^' then begin//out parameter
      sVar := 'out ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    //don't add out parameters for BeginFunction
    if (sVar <> 'out ') then begin

      if sPAramList <> '' then
        sParamList := sParamList+', ';

      sParamLIst := sParamList+sVar+sType+' '+sName;
    end;
  end;

  //build interface
  AddToClass(#9#9+'///<summary>');
  AddToClass(#9#9+'///Generated from '+sConstant+' '+sResultType+' '+sFunctionName+' '+sParams);
  AddToClass(#9#9+'///</summary>');
  sLine :=  #9#9+'public void Begin'+sFunctionName+'('+sParamList+')';
  AddToClass(sLine);
  AddToClass(#9#9+'{');

  AddToClass(#9#9#9+'MiscLib.Log.log.Debug("Begin Client Request: '+sFunctionName+'");');
  AddToClass(#9#9#9+'RDTPPacket packet;');
  AddToClass(#9#9#9+'packet = NeedPacket();');
  AddToClass(#9#9#9+'packet.AddShort('+sConstant+');');
  AddToClass(#9#9#9+'packet.AddShort(0);');
  AddToClass(#9#9#9'packet.AddString("'+FServiceName+'");');


  sRemainder := sParams;
  sReverse := '';
  while SplitString(sRemainder, ':', sName, sType) do begin
    //parameter type
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    //build param list
    sParamLIst := sParamList+sType+' '+sName;

    if sTypeL = 'special' then begin
    end else begin
      //marshall variant-compatible type
      if pt <> ptOut then begin
        //write the parameter that was passed to the function to the packet
        AddToClass(#9#9#9+'RDTP'+sType+'Helper.WriteToPacket(packet, '+sName+');');
      end;
    end;
  end;

//  AddToImplementation(sReverse);

  if GetNumberOfReturns(sResultType, sParams) = 0 then begin
    AddToClass(#9#9#9'RDTPPacket pout = null;');
    AddToClass(#9#9#9'if (!BeginTransact(packet, true)) throw new Exception("transaction failure: "+LastError);');
    AddToClass(#9#9#9'packet = pout;');

    //AddToClass(#9#9#9'if (!Transact(ref packet, true)) throw new Exception("transaction failure");')
  end else begin
    AddToClass(#9#9#9'RDTPPacket pout = null;');
    AddToClass(#9#9#9'if (!BeginTransact(packet, false)) throw new Exception("transaction failure: "+LastError);');
    AddToClass(#9#9#9'packet = pout;');

  end;



  AddToClass(#9#9#9+'MiscLib.Log.log.Debug("End Client Request: '+sFunctionName+'");');
  if lowercase(sREsultType)  <> 'void' then begin
    AddToClass(#9#9#9'return;');
  end;

  AddToClass(#9#9'}');

end;

procedure TRDTPClientGenerator.Process_Async_Client_RSP(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin
  if HasOverloadAlready(sFunctionname+'_Response', self.FInterfaceBlock) then begin
    exit;
  end;

  if GetNumberOfReturns(sResultType, sParams) = 0 then
    exit;

  sParamList := '';


  //build param list for function definition
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';

    pt := GetParamType(sName);
    sVar := GetParamModifier(pt);

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    //include ONLY VAR and OUT Parameters!
    if (pt in [ptOUT, ptVAr, ptConstPointer]) then begin
      if sPAramList <> '' then
        sParamList := sParamList+'; ';

      sParamLIst := sParamList+sVar+sName+':'+sType;
    end;
  end;

  //build interface
  sLine:= '    function '+sFunctionName+'_Response('+sParamList+'):'+sResultType+';';
  AddToInterface(sLine);


  //build implementation

  //-start with header

  AddToImplementation('//------------------------------------------------------------------------------');
  sLine := 'function '+FClass+'.'+sFunctionName+'_Response('+sParamList+'):'+sResultType+';';
  AddToImplementation(sLine);
  AddToImplementation('var');
  AddToImplementation('  packet: TRDTPPacket;');
  AddToImplementation('begin');
  AddToImplementation('  packet := nil;');
  AddToImplementation('  try');
  AddToImplementation('    if not EndTransact2(packet, packet,nil, false) then raise ECritical.create(''Transaction Failure'');');
  AddToImplementation('    if not packet.result then raise ECritical.create(''server error: ''+packet.message);');//todo Make more robust
  AddToImplementation('    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);');
  AddToImplementation('    //packet.SeqRead;//read off the service name and forget it (it is already known)');
  if lowercase(sResultType) <> 'void' then begin
    AddToImplementation('    Get'+sResultType+'FromPacket(packet, result);');
  end;

  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+sName+':'+sType;

    //marshall variant-compatible type
    if pt <> ptNormal then begin
      if lowercase(sType) <> 'void' then begin
        AddToImplementation('    Get'+sType+'FromPacket(packet, '+sName+');');
      end;
    end;
  end;

  AddToImplementation('  finally');
  AddToImplementation('    packet.free;');
  AddToImplementation('  end;');
  AddToImplementation('end;');

end;


procedure TRDTPClientGenerator.Process_Async_Client_RSP_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sTemp: string;
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin
  sParamList := '';

  //build param list
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';
    if sName[1] = '&' then begin //var parameter
      sVar := 'ref ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    if sName[1] = '^' then begin//out parameter
      sVar := 'out ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    if sVar <> '' then begin
      if sPAramList <> '' then
        sParamList := sParamList+', ';

      sParamLIst := sParamList+sVar+sType+' '+sName;
    end;
  end;

  //build interface
  AddToClass(#9#9+'///<summary>');
  AddToClass(#9#9+'///Generated from '+sConstant+' '+sResultType+' '+sFunctionName+' '+sParams);
  AddToClass(#9#9+'///</summary>');

  sTemp := 'End'+sFunctionName;
  if (FEndNames.IndexOf(lowercase(sTemp)) >=0) then
    sTemp := sTemp +'_'+sConstant;

  FEndNames.Add(lowercase(sTEmp));
  sLine :=  #9#9+'public '+sResultType+' '+sTemp+'('+sParamList+')';
  AddToClass(sLine);
  AddToClass(#9#9+'{');

  if GetNumberOfReturns(sResultType, sParams) = 0 then begin

    //AddToClass(#9#9#9'if (!Transact(ref packet, true)) throw new Exception("transaction failure");')
  end else begin
    AddToClass(#9#9#9+'RDTPPacket packet;');
    AddToClass(#9#9#9'RDTPPacket pout = null;');
    AddToClass(#9#9#9'if (!EndTransact(ref pout)) throw new Exception("transaction failure: "+LastError);');
    AddToClass(#9#9#9'packet = pout;');

    AddToClass(#9#9#9'if (!packet.result)   throw new Exception("server error: "+packet.message.ToString());');//todo Make more robust

    AddToClass(#9#9#9'packet.DataCursor = (RDTPPacket.PACKET_INDEX_RESULT_DETAILS);');

    if lowercase(sResultType) = 'special' then begin
      //todo special code here
    end else begin
      //variant-compatible result
      if lowercase(sREsultType)  <> 'void' then begin
        AddToClass(#9#9#9+sResultType+' result;');
        AddToClass(#9#9#9'RDTP'+sResultType+'Helper.ReadFromPacket(packet, out result);');
      end;
    end;
  end;


  //extract by reference parameters
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin

    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sType+' '+sParamList+sName;

    //marshall variant-compatible type
    if pt <> ptNormal then begin
      if lowercase(sType) <> 'void' then begin
        AddToClass(#9#9#9'RDTP'+sType+'Helper.ReadFromPacket(packet, out '+sName+');');
      end;
    end;
  end;

   AddToClass(#9#9#9+'MiscLib.Log.log.Debug("End Client Request: '+sFunctionName+'");');
  if lowercase(sREsultType)  <> 'void' then begin
    AddToClass(#9#9#9'return result;');
  end;

  AddToClass(#9#9'}');

end;

procedure TRDTPClientGenerator.Process_Async_Server_CB(sConstant, sFunctionName,
  sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin
  sParamList := '';


  //build param list for function definition
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';

    pt := GetParamType(sName);
    sVar := GetParamModifier(pt);

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    //do not include OUT parameters in list
    if not (pt = ptOUT) then begin
      if sPAramList <> '' then
        sParamList := sParamList+'; ';
      sParamLIst := sParamList+sVar+sName+':'+sType;
    end;
  end;

  //build interface
  sLine:= '    procedure '+sFunctionName+'_Async('+sParamList+');overload;virtual;';
  AddToInterface(sLine);


  //build implementation

  //-start with header

  AddToImplementation2('//------------------------------------------------------------------------------');
  sLine := 'procedure '+FClass+'Base.'+sFunctionName+'_Async('+sParamList+');';
  AddToImplementation2(sLine);
  AddToImplementation2('var');
  AddToImplementation2('  packet: TRDTPPacket;');
  AddToImplementation2('begin');
  AddToImplementation('  if not connect then');
  AddToImplementation('     raise ETransportError.create(''Failed to connect'');');
  AddToImplementation2('  packet := NeedPacket;');
  AddToImplementation2('  try');
  AddToImplementation2('    packet.AddVariant('+sConstant+');');
  AddToImplementation2('    packet.AddVariant(0);');
  AddToImplementation2('    packet.AddString('''+FServiceName+''');');

  sRemainder := sParams;
  sReverse := '';
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+sName+':'+sType;

    if sTypeL = 'special' then begin
    end else begin
      //marshall variant-compatible type
      if pt <> ptOut then begin
//        sReverse := '    packet.AddVariant('+sName+');'#13#10+sReverse;
        AddToImplementation2('    Write'+sType+'ToPacket(packet, '+sName+');');
      end;
    end;

  end;

//  AddToImplementation(sReverse);

//  if GetNumberOfReturns(sResultType, sParams) = 0 then
//    AddToImplementation2('    if not TransactionThread.Transact(packet, true) then raise ECritical.create(''transaction failure'');')
//  else
//    AddToImplementation2('    if not TransactionThread.Transact(packet) then raise ECritical.create(''transaction failure'');');

  if GetNumberOfReturns(sResultType, sParams) = 0 then
    AddToImplementation2('    //TransactionThread.Transact(packet, true);')
  else
    AddToImplementation2('    //TransactionThread.Transact(packet, false);');



  AddToImplementation2('  finally');
  AddToImplementation2('  end;');
  AddToImplementation2('end;');


end;

procedure TRDTPClientGenerator.Process_Async_Server_CB_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
begin
  //todo implement
end;

procedure TRDTPClientGenerator.Process_Async_Server_CB_RSP(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin

  if GetNumberOfReturns(sResultType, sParams) = 0 then
    exit;

  sParamList := '';


  //build param list for function definition
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';

    pt := GetParamType(sName);
    sVar := GetParamModifier(pt);

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    //include ONLY VAR and OUT Parameters!
    if (pt in [ptOUT, ptVAr, ptConstPointer]) then begin
      if sPAramList <> '' then
        sParamList := sParamList+'; ';

      sParamLIst := sParamList+sVar+sName+':'+sType;
    end;
  end;

  //build interface
  sLine:= '    function '+sFunctionName+'_Response('+sParamList+'):'+sResultType+';';
  AddToInterface(sLine);


  //build implementation

  //-start with header

  AddToImplementation2('//------------------------------------------------------------------------------');
  sLine := 'function '+FClass+'Base.'+sFunctionName+'_Response('+sParamList+'):'+sResultType+';';
  AddToImplementation2(sLine);
  AddToImplementation2('var');
  AddToImplementation2('  packet: TRDTPPacket;');
  AddToImplementation2('begin');
  AddToImplementation2('  packet := nil;');
  AddToImplementation2('  try');
  AddToImplementation2('    //packet := TransactionThread.GetResult;');
  AddToImplementation2('    //if not packet.result then raise ECritical.create(''server error: ''+packet.message);');//todo Make more robust
  AddToImplementation2('    //packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);');
  if lowercase(sResultType) <> 'void' then begin
    AddToImplementation2('    Get'+sResultType+'FromPacket(packet, result);');
  end;

  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+sName+':'+sType;

    //marshall variant-compatible type
    if pt <> ptNormal then begin
      if lowercase(sType) <> 'void' then begin
        AddToImplementation2('    Get'+sType+'FromPacket(packet, '+sName+');');
      end;
    end;
  end;

  AddToImplementation2('  finally');
  AddToImplementation2('    packet.free;');
  AddToImplementation2('  end;');
  AddToImplementation2('end;');

end;

procedure TRDTPClientGenerator.Process_Async_Server_CB_RSP_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
begin
  //todo implement
end;

procedure TRDTPClientGenerator.Process_CB(sParams: string);
var
  sLeft, sRight, sConstant, sFunction, sResultType: string;
begin
  //get constant
  SplitString(sParams, ' ', sConstant, sRight);
  //get function name
  SplitString(sRight, ' ', sFunction, sRight);
  //get result type
  SplitString(sRight, ' ', sResultType, sParams);

  if uppercase(FType) = 'CLIENT' then begin
    Process_Client_CB('$'+sConstant, sFunction, sResultType, sParams);
  end
  else if uppercase(FType) = 'IMPLIB' then
    exit
  else if uppercase(FType) = 'CLIENT_IMPLIB' then
    Process_Client_ImpLib_CB('$'+sConstant, sFunction, sResultType, sParams)
  else begin
    Process_Server_CB('$'+sConstant, sFunction, sResultType, sParams);
    Process_Async_SERVER_CB('$'+sConstant, sFunction, sResultType, sParams);
    Process_Async_SERVER_CB_RSP('$'+sConstant, sFunction, sResultType, sParams);
  end;

end;

procedure TRDTPClientGenerator.Process_CB_CSharp(sParams: string);
var
  sLeft, sRight, sConstant, sFunction, sResultType: string;
begin
  //get constant
  SplitString(sParams, ' ', sConstant, sRight);
  //get function name
  SplitString(sRight, ' ', sFunction, sRight);
  //get result type
  SplitString(sRight, ' ', sResultType, sParams);

  if uppercase(FType) = 'CLIENT' then begin
    Process_Client_CB_Csharp('0x'+sConstant, sFunction, sResultType, sParams);
  end
  else if uppercase(FType) = 'IMPLIB' then
    exit
  else if uppercase(FType) = 'CLIENT_IMPLIB' then
    Process_Client_ImpLib_CB_CSharp('$'+sConstant, sFunction, sResultType, sParams)
  else begin
    Process_Server_CB_CSharp('0x'+sConstant, sFunction, sResultType, sParams);
    Process_Async_SERVER_CB_CSharp('0x'+sConstant, sFunction, sResultType, sParams);
    Process_Async_SERVER_CB_RSP_CSharp('0x'+sConstant, sFunction, sResultType, sParams);
  end;

end;


procedure TRDTPClientGenerator.Process_Class(sParams: string);
begin
  FCLASS := sParams;
end;

procedure TRDTPClientGenerator.Process_Client_CB(sConstant, sFunctionName,
  sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sReverseParamList, sReverseCallingParamList, sCallingParamList: string;
  sLine: string;
  iPos: integer;
  pt: TParamType;
  sSetupLines: string;
  sOutLines: string;
  iREturns: integer;
  sExtendedSymbolList: string;
begin
  iReturns := 0;
  sSetupLines := '';
  sOutLines := '';
  //build reverse function param list
  sReverseParamList := '';
  sRemainder := sParams;
  sExtendedSymbolList := '';
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sReverseParamList <> '' then
      sReverseParamList := '; '+sReverseParamList;

    sExtendedSymbolList := sExtendedSymbolList + '_'+sType;

    sReverseParamList := GetParamModifier(pt)+sName+':'+sType+sReverseParamList;

  end;

  //build REVERSE CALLING param list
  sReverseCallingParamList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);


    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    sReverseCallingParamList := sReverseCallingParamList+', ';
    if (pt=ptNormal) and false then begin

      sReverseCallingParamList := 'Get'+sType+'FromPacket(callback.request)'+sReverseCallingParamList;
    end else begin
      //add a var for OUT and VAR parameters;
      AddToVar('  '+sName+':'+sType+';');
      sSetupLines := sSetupLines+'        '+sName+' := Get'+sType+'FromPacket(callback.request);';
      sOutLines := sOutLInes+'        RDTP'+sType+'Helper.WriteToPacket(callback.response, '+sName+GetConstantValue(sConstant)+')';
      sReverseCallingParamList := sReverseCallingParamList+sName+sConstant;

    end;

  end;

  //build forward calling param list
  sCallingParamList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sCallingParamList <> '' then
      sCallingParamList := sCallingParamList+', ';

    sCallingParamList := sCallingParamList+sName;

  end;

  AddToImplementation2('');
  AddToImplementation2('    //'+sFunctionName);
  AddToImplementation2('    '+sConstant+':');
  AddToImplementation2('      begin');
  AddToImplementation2('{$IFDEF RDTP_LOGGING}');
  AddToImplementation2('        LocalDebug(''Begin Client Callback handling of '+sFunctionname+''',''RDTPCALLBACKS'');');
  AddToImplementation2('{$ENDIF}');
  AddToImplementation2('        CB_HANDLE_'+sFunctionName+sExtendedSYmbolList+'(self);');
  AddToImplementation2('{$IFDEF RDTP_LOGGING}');
  AddToImplementation2('        LocalDebug(''End Client Callback handling of '+sFunctionname+''',''RDTPCALLBACKS'');');
  AddToImplementation2('{$ENDIF}');
  AddToImplementation2('        result := true;');
  AddToImplementation2('      end;');

  AddToImplementation('//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-');
  AddToImplementation('procedure CB_HANDLE_'+sFunctionName+sExtendedSYmbolList+'(cli: '+FClass+');');
  if (lowercase(sResultType) <> 'void') or (sParams <> '') then
    AddToImplementation('var');
  if lowercase(sResultType) <> 'void' then
    AddToImplementation('  res: '+sResultType+';');
  //VARS
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    AddToImplementation('  '+sName+':'+sType+';');
  end;
  AddToImplementation('begin');

  //VAR FETCH
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    if pt <> ptOut then
      AddToImplementation('  Get'+sType+'FromPacket(cli.callback.request, '+sName+');');
  end;

  if sCallingParamList <> '' then
    sCallingParamList := ', '+sCallingParamList;
  if lowercase(sResultType) <> 'void' then begin
    AddToImplementation('  res := CB_'+sFunctionName+'(cli'+sCallingParamList+');');
    AddToImplementation('  Write'+sResultType+'ToPacket(cli.callback.response, res);');
    inc(iReturns);
  end else begin
    AddToImplementation('  CB_'+sFunctionName+'(cli'+sCallingParamList+');');
  end;

  //OUT/VAR RESULT POST
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    if pt <> ptNormal then begin
      AddToImplementation('  Write'+sType+'ToPacket(cli.callback.response, '+sName+');');
      inc(iReturns);
    end;
  end;

  if iReturns = 0 then
    AddToImplementation('  proc.ForgetResult := true');
  AddToImplementation('end;');


end;
procedure TRDTPClientGenerator.Process_Client_CB_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sReverseParamList, sReverseCallingParamList, sCallingParamList: string;
  sLine: string;
  iPos: integer;
  pt: TParamType;
  sSetupLines: string;
  sOutLines: string;
  iREturns: integer;
  sExtendedSymbolList: string;
begin
  iReturns := 0;
  sSetupLines := '';
  sOutLines := '';
  //build reverse function param list
  sReverseParamList := '';
  sExtendedSymbolList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sReverseParamList <> '' then
      sReverseParamList := sReverseParamList+', ';

    sExtendedSymbolList := sExtendedSymbolList+'_';
    sExtendedSymbolList := sExtendedSymbolList+sType;

    sReverseParamList := sReverseParamList+(GetParamModifier(pt)+sType+' '+sName);

  end;

  //build REVERSE CALLING param list
  sReverseCallingParamList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);


    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    sReverseCallingParamList := sReverseCallingParamList+', ';
    if (pt=ptNormal) and false then begin

      sReverseCallingParamList := 'RDTP'+sType+'Helper.ReadFromPacket(request)'+sReverseCallingParamList;
    end else begin
      //add a var for OUT and VAR parameters;
      AddToVar('  '+sName+':'+sType+';');
      sSetupLines := sSetupLines+'        '+sName+' = RDTP'+sType+'Helper.ReadFromPacket(request);';
      sOutLines := sOutLInes+'        RDTP'+sType+'Helper.WriteToPacket(response, '+sName+GetConstantValue(sConstant)+')';
      sReverseCallingParamList := sReverseCallingParamList+sName+sConstant;

    end;

  end;

  //build forward calling param list
  sCallingParamList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sCallingParamList <> '' then
      sCallingParamList := sCallingParamList+', ';

    sCallingParamList := sCallingParamList+GetParamModifier(pt)+sName;

  end;

  //dispatcher
  AddToImplementation2(#9#9#9#9);
  AddToImplementation2(#9#9#9#9'//'+sFunctionName);
  AddToImplementation2(#9#9#9#9'case '+sConstant+':');
  AddToImplementation2(#9#9#9#9'{');
  AddToImplementation2(#9#9#9#9#9'MiscLib.Log.log.Debug("Begin Client Callback Handling of '+sFunctionname+'","RDTP");');
  AddToImplementation2(#9#9#9#9#9'CB_HANDLE_'+sFunctionName+sExtendedSymbolList+'();');
  AddToImplementation2(#9#9#9#9#9'MiscLib.Log.log.Debug("End Client Callback Handling of '+sFunctionname+'","RDTP");');
  AddToImplementation2(#9#9#9#9#9'return true;');
  AddToImplementation2(#9#9#9#9'}');

  //implementor
  AddToImplementation(#9#9'//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-');
  AddToImplementation(#9#9'protected abstract '+sREsultType+' CB_'+sFunctionName+'('+sReverseParamList+');');
  AddToImplementation(#9#9'void CB_HANDLE_'+sFunctionName+sExtendedSymbolList+'()');
  AddToImplementation(#9#9'{');
  //result var
  if lowercase(sResultType) <> 'void' then
    AddToImplementation(#9#9#9+sResultType+' res;');
  //VARS
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    AddToImplementation(#9#9#9+sType+' '+sName+';');
  end;
  //VAR FETCH
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    if pt <> ptOut then
      AddToImplementation(#9#9#9'RDTP'+sType+'Helper.ReadFromPacket(Callback.Request, out '+sName+');');
  end;

//  if sCallingParamList <> '' then
//    sCallingParamList := ', '+sCallingParamList;
  if lowercase(sResultType) <> 'void' then begin
    AddToImplementation(#9#9#9'res = CB_'+sFunctionName+'('+sCallingParamList+');');
    AddToImplementation(#9#9#9'RDTP'+sResultType+'Helper.WriteToPacket(Callback.Response, res);');
    inc(iReturns);
  end else begin
    AddToImplementation(#9#9#9'CB_'+sFunctionName+'('+sCallingParamList+');');
  end;

  //OUT/VAR RESULT POST
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    if pt <> ptNormal then begin
      AddToImplementation(#9#9#9'RDTP'+sType+'Helper.WriteToPacket(Callback.Response, '+sName+');');
      inc(iReturns);
    end;
  end;

  if iReturns = 0 then
    AddToImplementation(#9#9#9'Forget = true;');
  AddToImplementation(#9#9'}');


end;

procedure TRDTPClientGenerator.Process_Client_ImpLib_CB(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  pt: TParamType;
  ssResultTYpe, ssParamList: string;
  sFunctionKeyword: string;
begin
  sParamList := '';

  if FANCESTOR = '' then
    FANCESTOR := 'TRDTPProcessor';


  //build param list
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+'; ';

    sParamLIst := sParamList+GetParamModifier(pt)+sName+':'+sType;
  end;

  //build interface
  if lowercase(sResultType)<>'void' then begin
    ssREsultTYpe := ':'+sREsultType+';';
    sFunctionKeyword := 'function';
  end else begin
    ssResultType := ';';
    sFunctionKeyword := 'procedure';
  end;

  if TrimSTr(sParamList) <> '' then
    ssParamList := '; '+sParamList
  else
    ssParamLIst := '';

  sLine:= sFunctionKeyword+' CB_'+sFunctionName+'(cli:'+FClass+ssParamList+')'+ssResultType+'overload;';
  AddToInterface(sLine);


end;

procedure TRDTPClientGenerator.Process_Client_ImpLib_CB_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
begin
  //todo implement
end;

procedure TRDTPClientGenerator.Process_Client_RQ(sConstant, sFunctionName, sResultType: string; sPArams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin
  sParamList := '';


  //build param list
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';
    if sName[1] = '&' then begin //var parameter
      sVar := 'var ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    if sName[1] = '^' then begin//out parameter
      sVar := 'out ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    if sPAramList <> '' then
      sParamList := sParamList+'; ';

    sParamLIst := sParamList+sVar+sName+':'+sType;
  end;

  //build interface
  if lowercase(sResultType) = 'void' then begin
    sLine:= '    procedure '+sFunctionName+'('+sParamList+');overload;virtual;';
  end else begin
    sLine:= '    function '+sFunctionName+'('+sParamList+'):'+sResultType+';overload;virtual;';
  end;
  AddToInterface(sLine);


  //build implementation

  //-start with header

  AddToImplementation('//------------------------------------------------------------------------------');
  if lowercase(sResultType) = 'void' then begin
    sLine := 'procedure '+FClass+'.'+sFunctionName+'('+sParamList+');';
  end else begin
    sLine := 'function '+FClass+'.'+sFunctionName+'('+sParamList+'):'+sResultType+';';
  end;
  AddToImplementation(sLine);
  AddToImplementation('var');
  AddToImplementation('  packet: TRDTPPacket;');
  AddToImplementation('begin');
  AddToImplementation('  if not connect then');
  AddToImplementation('     raise ETransportError.create(''Failed to connect'');');
  AddToImplementation('  packet := NeedPacket;');
  AddToImplementation('  try try');
  AddToImplementation('    packet.AddVariant('+sConstant+');');
  AddToImplementation('    packet.AddVariant(0);');
  AddToImplementation('    packet.AddString('''+FServiceName+''');');
  AddToImplementation('{$IFDEF RDTP_CLIENT_LOGGING}Debug.Log(''RDTP'+sConstant+':'+FClass+'.'+sFunctionName+''');{$ENDIF}');


  sRemainder := sParams;
  sReverse := '';
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+sName+':'+sType;

    if sTypeL = 'special' then begin
    end else begin
      //marshall variant-compatible type
      if pt <> ptOut then begin
//        sReverse := '    packet.AddVariant('+sName+');'#13#10+sReverse;
        AddToImplementation('    Write'+sType+'ToPacket(packet, '+sName+');');
      end;
    end;

  end;

//  AddToImplementation(sReverse);

//  AddToImplementation('     WaitForTransactionThread;');
  if GetNumberOfReturns(sResultType, sParams) = 0 then
    AddToImplementation('    if not Transact(packet, true) then raise ECritical.create(''transaction failure'');')
  else begin
    AddToImplementation('    if not Transact(packet) then raise ECritical.create(''transaction failure'');');
    AddToImplementation('    if not packet.result then raise ECritical.create(''server error: ''+packet.message);');//todo Make more robust

    AddToImplementation('    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);');

    if lowercase(sResultType) = 'special' then begin
      //todo special code here
    end else begin
      //variant-compatible result

      if lowercase(sREsultType)  <> 'void' then begin
        AddToImplementation('    Get'+sResultType+'FromPacket(packet, result);');
      end;
    end;
  end;


  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+sName+':'+sType;

    //marshall variant-compatible type
    if pt <> ptNormal then begin
      if lowercase(sType) <> 'void' then begin
        AddToImplementation('    Get'+sType+'FromPacket(packet, '+sName+');');
      end;
    end;
  end;




  AddToImplementation('  except');
  AddToImplementation('    on E:Exception do begin');
  AddToImplementation('      e.message := ''RDTP Call Failed:''+e.message;');
  AddToImplementation('      raise;');
  AddToImplementation('    end;');
  AddToImplementation('  end;');
  AddToImplementation('  finally');
  AddToImplementation('    packet.free;');
  AddToImplementation('  end;');
  AddToImplementation('end;');





end;



//--------------------------------------------------------------------------------
procedure TRDTPClientGenerator.Process_Client_RQ_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin
  sParamList := '';

  //build param list
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';
    if sName[1] = '&' then begin //var parameter
      sVar := 'ref ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    if sName[1] = '^' then begin//out parameter
      sVar := 'out ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+sVar+sType+' '+sName;
  end;

  //build interface
  AddToClass(#9#9+'///<summary>');
  AddToClass(#9#9+'///Generated from '+sConstant+' '+sResultType+' '+sFunctionName+' '+sParams);
  AddToClass(#9#9+'///</summary>');
  sLine :=  #9#9+'public '+sResultType+' '+sFunctionName+'('+sParamList+')';
  AddToClass(sLine);
  AddToClass(#9#9+'{');

  AddToClass(#9#9#9+'MiscLib.Log.log.Debug("Begin Client Request: '+sFunctionName+'");');
  AddToClass(#9#9#9+'RDTPPacket packet;');
  AddToClass(#9#9#9+'packet = NeedPacket();');
  AddToClass(#9#9#9+'packet.AddShort('+sConstant+');');
  AddToClass(#9#9#9+'packet.AddShort(0);');
  AddToClass(#9#9#9'packet.AddString("'+FServiceName+'");');


  sRemainder := sParams;
  sReverse := '';
  while SplitString(sRemainder, ':', sName, sType) do begin
    //parameter type
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    //build param list
    sParamLIst := sParamList+sType+' '+sName;

    if sTypeL = 'special' then begin
    end else begin
      //marshall variant-compatible type
      if pt <> ptOut then begin
        //write the parameter that was passed to the function to the packet
        AddToClass(#9#9#9+'RDTP'+sType+'Helper.WriteToPacket(packet, '+sName+');');
      end;
    end;
  end;

//  AddToImplementation(sReverse);

  if GetNumberOfReturns(sResultType, sParams) = 0 then begin
    AddToClass(#9#9#9'RDTPPacket pout = null;');
    AddToClass(#9#9#9'if (!Transact(packet, ref pout, true)) throw new Exception("transaction failure: "+LastError);');
    AddToClass(#9#9#9'packet = pout;');

    //AddToClass(#9#9#9'if (!Transact(ref packet, true)) throw new Exception("transaction failure");')
  end else begin
    AddToClass(#9#9#9'RDTPPacket pout = null;');
    AddToClass(#9#9#9'if (!Transact(packet, ref pout, false)) throw new Exception("transaction failure: "+LastError);');
    AddToClass(#9#9#9'packet = pout;');

    AddToClass(#9#9#9'if (!packet.result)   throw new Exception("server error: "+packet.message.ToString());');//todo Make more robust

    AddToClass(#9#9#9'packet.DataCursor = (RDTPPacket.PACKET_INDEX_RESULT_DETAILS);');

    if lowercase(sResultType) = 'special' then begin
      //todo special code here
    end else begin
      //variant-compatible result
      if lowercase(sREsultType)  <> 'void' then begin
        AddToClass(#9#9#9+sResultType+' result;');
        AddToClass(#9#9#9'RDTP'+sResultType+'Helper.ReadFromPacket(packet, out result);');
      end;
    end;
  end;


  //extract by reference parameters
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin

    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sType+' '+sParamList+sName;

    //marshall variant-compatible type
    if pt <> ptNormal then begin
      if lowercase(sType) <> 'void' then begin
        AddToClass(#9#9#9'RDTP'+sType+'Helper.ReadFromPacket(packet, out '+sName+');');
      end;
    end;
  end;

   AddToClass(#9#9#9+'MiscLib.Log.log.Debug("End Client Request: '+sFunctionName+'");');
  if lowercase(sREsultType)  <> 'void' then begin
    AddToClass(#9#9#9'return result;');
  end;

  AddToClass(#9#9'}');

end;

procedure TRDTPClientGenerator.Process_ImpLib_RQ(sConstant, sFunctionName,
  sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  pt: TParamType;
  ssResultTYpe, ssParamList: string;
  sFunctionKeyword: string;
begin
  sParamList := '';

  if FClass = '' then
    FClass := 'TRDTPProcessor';


  //build param list
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+'; ';

    sParamLIst := sParamList+GetParamModifier(pt)+sName+':'+sType;
  end;

  //build interface
  if lowercase(sResultType)<>'void' then begin
    ssREsultTYpe := ':'+sREsultType+';';
    sFunctionKeyword := 'function';
  end else begin
    ssResultType := ';';
    sFunctionKeyword := 'procedure';
  end;

  if TrimSTr(sParamList) <> '' then
    ssParamList := sParamList
  else
    ssParamLIst := '';

  sLine:= '    '+sFunctionKeyword+' RQ_'+sFunctionName+'('+ssParamList+')'+ssResultType+'overload;override;';
  AddToInterface(sLine);


end;

procedure TRDTPClientGenerator.Process_ImpLib_RQ_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  pt: TParamType;
  ssResultTYpe, ssParamList: string;
  sFunctionKeyword: string;
begin
  sParamList := '';

  if FClass = '' then
    FClass := 'BaseRDTPProcessor';


  //build param list
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+'; ';

    sParamLIst := sParamList+GetParamModifier(pt)+sType+' '+sName;
  end;


  if TrimSTr(sParamList) <> '' then
    ssParamList := '; '+sParamList
  else
    ssParamLIst := '';

  sLine:= 'protected '+ssResultType+' RQ_'+sFunctionName+'('+FClass+' proc, '+ssParamList+');';
  AddToInterface(sLine);


end;

procedure TRDTPClientGenerator.Process_Server_CB(sConstant, sFunctionName,
  sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin
  sParamList := '';


  //build param list
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';
    if sName[1] = '&' then begin //var parameter
      sVar := 'var ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    if sName[1] = '^' then begin//out parameter
      sVar := 'out ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    if sPAramList <> '' then
      sParamList := sParamList+'; ';

    sParamLIst := sParamList+sVar+sName+':'+sType;
  end;

  //build interface
  if lowercase(sResultType) = 'void' then begin
    sLine:= '    procedure '+sFunctionName+'('+sParamList+');overload;virtual;';
  end else begin
    sLine:= '    function '+sFunctionName+'('+sParamList+'):'+sResultType+';overload;virtual;';
  end;
  AddToInterface(sLine);


  //build implementation

  //-start with header

  AddToImplementation2('//---wtfwtf-----------------------------------------------------------------------');
  if lowercase(sResultType) = 'void' then begin
    sLine := 'procedure '+FClass+'Base.'+sFunctionName+'('+sParamList+');';
  end else begin
    sLine := 'function '+FClass+'Base.'+sFunctionName+'('+sParamList+'):'+sResultType+';';
  end;
  AddToImplementation2(sLine);
  AddToImplementation2('var');
  AddToImplementation2('  packet: TRDTPPacket;');
  AddToImplementation2('begin');
  AddToImplementation('  if not connect then');
  AddToImplementation('     raise ETransportError.create(''Failed to connect'');');
  AddToImplementation2('  packet := NeedPAcket;');
  AddToImplementation2('  try');
  AddToImplementation2('    packet.AddVariant('+sConstant+');');
  AddToImplementation2('    packet.AddVariant(0);');
  AddToImplementation2('    packet.AddString('''+FServiceName+''');');

  sRemainder := sParams;
  sReverse := '';
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+GetParamModifier(pt)+sName+':'+sType;

    if sTypeL = 'special' then begin
    end else begin
      //marshall variant-compatible type
      if pt <> ptOut then begin
//        sReverse := '    packet.AddVariant('+sName+');'#13#10+sReverse;
        AddToImplementation2('    Write'+sType+'ToPacket(packet, '+sName+');');
      end;
    end;

  end;

//  AddToImplementation(sReverse);

  if GetNumberOfReturns(sResultType, sParams) = 0 then
    AddToImplementation2('    if not Transact(packet, true) then raise ECritical.create(''transaction failure'');')
  else begin
    AddToImplementation2('    if not Transact(packet) then raise ECritical.create(''transaction failure'');');
    AddToImplementation2('    if not packet.result then raise ECritical.create(''server error: ''+packet.message);');//todo Make more robust

    AddToImplementation2('    packet.SeqSeek(PACKET_INDEX_RESULT_DETAILS);');

    if lowercase(sResultType) = 'special' then begin
      //todo special code here
    end else begin
      //variant-compatible result

      if lowercase(sREsultType)  <> 'void' then begin
        AddToImplementation2('    Get'+sResultType+'FromPacket(packet, result);');
      end;
    end;
  end;


  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+sName+':'+sType;

    //marshall variant-compatible type
    if pt <> ptNormal then begin
      if lowercase(sType) <> 'void' then begin
        AddToImplementation2('    Get'+sType+'FromPacket(packet, '+sName+');');
      end;
    end;
  end;




  AddToImplementation2('  finally');
  AddToImplementation2('    packet.free;');
  AddToImplementation2('  end;');
  AddToImplementation2('end;');





end;

procedure TRDTPClientGenerator.Process_Server_CB_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sParamList: string;
  sLine: string;
  sReverse: string;
  sVar: string;
  pt: TParamType;
begin
  sParamList := '';

  //build param list
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    sVar := '';
    if sName[1] = '&' then begin //var parameter
      sVar := 'ref ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    if sName[1] = '^' then begin//out parameter
      sVar := 'out ';
      sName := zsubcopy(sName, 1, 99999);//ok
    end;

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sParamList+sVar+sType+' '+sName;
  end;

  //build interface
  AddToImplementation2(#9#9+'///<summary>');
  AddToImplementation2(#9#9+'///Generated from '+sConstant+' '+sResultType+' '+sFunctionName+' '+sParams);
  AddToImplementation2(#9#9+'///</summary>');
  sLine :=  #9#9+'public '+sResultType+' '+sFunctionName+'('+sParamList+')';
  AddToImplementation2(sLine);
  AddToImplementation2(#9#9+'{');

  AddToImplementation2(#9#9#9+'RDTPPacket packet;');
  AddToImplementation2(#9#9#9+'packet = NeedPacket();');
  AddToImplementation2(#9#9#9'packet.PacketType=70;//sets to callback type');
  AddToImplementation2(#9#9#9+'packet.AddShort('+sConstant+');');
  AddToImplementation2(#9#9#9+'packet.AddShort(0);');
  AddToImplementation2(#9#9#9'packet.AddString("'+FServiceName+'");');



  sRemainder := sParams;
  sReverse := '';
  while SplitString(sRemainder, ':', sName, sType) do begin
    //parameter type
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    //build param list
    sParamLIst := sParamList+sType+' '+sName;

    if sTypeL = 'special' then begin
    end else begin
      //marshall variant-compatible type
      if pt <> ptOut then begin
        //write the parameter that was passed to the function to the packet
        AddToImplementation2(#9#9#9+'RDTP'+sType+'Helper.WriteToPacket(packet, '+sName+');');
      end;
    end;
  end;

//  AddToImplementation(sReverse);

  if GetNumberOfReturns(sResultType, sParams) = 0 then
    AddToImplementation2(#9#9#9'if (!Transact(ref packet, true)) throw new Exception("transaction failure");')
  else begin
    AddToImplementation2(#9#9#9'if (!Transact(ref packet))throw new Exception("transaction failure");');
    AddToImplementation2(#9#9#9'if (!packet.result)   throw new Exception("server error: "+packet.message.ToString());');//todo Make more robust

    AddToImplementation2(#9#9#9'packet.DataCursor = (RDTPPacket.PACKET_INDEX_RESULT_DETAILS);');

    if lowercase(sResultType) = 'special' then begin
      //todo special code here
    end else begin
      //variant-compatible result
      if lowercase(sREsultType)  <> 'void' then begin
        AddToImplementation2(#9#9#9+sResultType+' result;');
        AddToImplementation2(#9#9#9'RDTP'+sResultType+'Helper.ReadFromPacket(packet, out result);');
      end;
    end;
  end;


  //extract by reference parameters
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin

    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+', ';

    sParamLIst := sType+' '+sParamList+sName;

    //marshall variant-compatible type
    if pt <> ptNormal then begin
      if lowercase(sType) <> 'void' then begin
        AddToImplementation2(#9#9#9'RDTP'+sType+'Helper.ReadFromPacket(packet, out '+sName+');');
      end;
    end;
  end;

  if lowercase(sREsultType)  <> 'void' then begin
    AddToImplementation2(#9#9#9'return result;');
  end;

  AddToImplementation2(#9#9'}');

end;
//-----------------------------------------------------------------------------
procedure TRDTPClientGenerator.Process_Server_RQ(sConstant, sFunctionName, sResultType: string; sPArams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sReverseParamList, sReverseCallingParamList, sCallingParamList: string;
  sLine: string;
  iPos: integer;
  pt: TParamType;
  sSetupLines: string;
  sOutLines: string;
  iREturns: integer;
  sParamList: string;
  sExtendedSymbolList: string;
begin
  iReturns := 0;
  sSetupLines := '';
  sOutLines := '';
  //build reverse function param list
  sReverseParamList := '';
  sExtendedSymbolList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sReverseParamList <> '' then
      sReverseParamList := '; '+sReverseParamList;

    sExtendedSymbolList := sExtendedSymbolList+'_';
    sExtendedSymbolList := sExtendedSymbolList+sType; 

    sReverseParamList := GetParamModifier(pt)+sName+':'+sType+sReverseParamList;

  end;

  //build param list (with types)
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sPAramList <> '' then
      sParamList := sParamList+'; ';

    sParamLIst := sParamList+GetParamModifier(pt)+sName+':'+sType;
  end;

  //build REVERSE CALLING param list
  sReverseCallingParamList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);


    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    sReverseCallingParamList := sReverseCallingParamList+', ';
    if (pt=ptNormal) and false then begin

      sReverseCallingParamList := 'Get'+sType+'FromPacket(request)'+sReverseCallingParamList;
    end else begin
      //add a var for OUT and VAR parameters;
      AddToVar('  '+sName+':'+sType+';');
      sSetupLines := sSetupLines+'        '+sName+' := Get'+sType+'FromPacket(request);';
      sOutLines := sOutLInes+'        Write'+sType+'ToPacket(response, '+sName+GetConstantValue(sConstant)+')';
      sReverseCallingParamList := sReverseCallingParamList+sName+sConstant;

    end;

  end;

  //build forward calling param list
  sCallingParamList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sCallingParamList <> '' then
      sCallingParamList := sCallingParamList+', ';

    sCallingParamList := sCallingParamList+sName;

  end;


  AddPrivateMember('    procedure RQ_HANDLE_'+sFunctionName+sExtendedSymbolList+'(proc: '+FAncestor+');');

  if lowercase(sResultType) = 'void' then
    AddPublicMember('    procedure RQ_'+sFunctionName+'('+sParamList+');overload;virtual;abstract;')
  else
    AddPublicMember('    function RQ_'+sFunctionName+'('+sParamList+'):'+sResultType+';overload;virtual;abstract;');

  AddToImplementation('');
  AddToImplementation('    //'+sFunctionName);
  AddToImplementation('    '+sConstant+':');
  AddToImplementation('      begin');
  AddToImplementation('{$IFDEF RDTP_LOGGING}');
  AddToImplementation('        LocalDebug(''Begin Server Handling of '+sFunctionname+''',''RDTPCALLS'');');
  AddToImplementation('{$ENDIF}');
  AddToImplementation('        result := true;//set to true BEFORE calling in case of exception');
  AddToImplementation('        RQ_HANDLE_'+sFunctionName+sExtendedSymbolList+'(self);');
  AddToImplementation('{$IFDEF RDTP_LOGGING}');
  AddToImplementation('        LocalDebug(''End Server Handling of '+sFunctionname+''',''RDTPCALLS'');');
  AddToImplementation('{$ENDIF}');
  AddToImplementation('      end;');

  AddToImplementation2('//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-');
  AddToImplementation2('procedure '+FClass+'Base.RQ_HANDLE_'+sFunctionName+sExtendedSymbolList+'(proc: '+FAncestor+');');
  if (lowercase(sResultType) <> 'void') or (sParams <> '') then
    AddToImplementation2('var');
  if lowercase(sResultType) <> 'void' then
    AddToImplementation2('  res: '+sResultType+';');
  //VARS
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    AddToImplementation2('  '+sName+':'+sType+';');
  end;
  AddToImplementation2('begin');

  //VAR FETCH
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    if pt <> ptOut then
      AddToImplementation2('  Get'+sType+'FromPacket(proc.request, '+sName+');');
  end;

//??  if sCallingParamList <> '' then
//    sCallingParamList := ', '+sCallingParamList;
  if lowercase(sResultType) <> 'void' then begin
    AddToImplementation2('  res := RQ_'+sFunctionName+'('+sCallingParamList+');');
    AddToImplementation2('  Write'+sResultType+'ToPacket(proc.response, res);');
    inc(iReturns);
  end else begin
    AddToImplementation2('  RQ_'+sFunctionName+'('+sCallingParamList+');');
  end;

  //OUT/VAR RESULT POST
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    if pt <> ptNormal then begin
      AddToImplementation2('  Write'+sType+'ToPacket(proc.response, '+sName+');');
      inc(iReturns);
    end;
  end;

  if iReturns = 0 then
    AddToImplementation2('  proc.ForgetResult := true');
  AddToImplementation2('end;');


end;

//-----------------------------------------------------------------------------
procedure TRDTPClientGenerator.Process_Server_RQ_Csharp(sConstant,
  sFunctionName, sResultType, sParams: string);
var
  sName, sType, sRemainder, sTypeL: string;
  sInterfaceLine, sImplementationLine: string;
  sReverseParamList, sReverseCallingParamList, sCallingParamList: string;
  sLine: string;
  iPos: integer;
  pt: TParamType;
  sSetupLines: string;
  sOutLines: string;
  iREturns: integer;
  sExtendedSymbolList: string;
begin
  iReturns := 0;
  sSetupLines := '';
  sOutLines := '';
  //build reverse function param list
  sReverseParamList := '';
  sExtendedSymbolList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);

    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sReverseParamList <> '' then
      sReverseParamList := sReverseParamList+', ';

    sExtendedSymbolList := sExtendedSymbolList+'_';
    sExtendedSymbolList := sExtendedSymbolList+sType;

    sReverseParamList := sReverseParamList+GetParamModifier(pt)+sType+' '+sName;

  end;

  //build REVERSE CALLING param list
  sReverseCallingParamList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);


    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);

    sReverseCallingParamList := sReverseCallingParamList+', ';
    if (pt=ptNormal) and false then begin

      sReverseCallingParamList := 'RDTP'+sType+'Helper.ReadFromPacket(request)'+sReverseCallingParamList;
    end else begin
      //add a var for OUT and VAR parameters;
      AddToVar('  '+sName+':'+sType+';');
      sSetupLines := sSetupLines+'        '+sName+' = RDTP'+sType+'Helper.ReadFromPacket(request);';
      sOutLines := sOutLInes+'        RDTP'+sType+'Helper.WriteToPacket(response, '+sName+GetConstantValue(sConstant)+')';
      sReverseCallingParamList := sReverseCallingParamList+sName+sConstant;

    end;

  end;

  //build forward calling param list
  sCallingParamList := '';
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    sTypeL := lowercase(sType);
    if sCallingParamList <> '' then
      sCallingParamList := sCallingParamList+', ';

    sCallingParamList := sCallingParamList+GEtParamModifier(pt)+sName;

  end;

  //dispatcher
  AddToImplementation(#9#9#9#9);
  AddToImplementation(#9#9#9#9'//'+sFunctionName);
  AddToImplementation(#9#9#9#9'case '+sConstant+':');
  AddToImplementation(#9#9#9#9'{');
  AddToImplementation(#9#9#9#9#9'MiscLib.Log.log.Debug("Begin Server Handling of '+sFunctionname+'","RDTP");');
  AddToImplementation(#9#9#9#9#9'RQ_HANDLE_'+sFunctionName+sExtendedSymbolList+'();');
  AddToImplementation(#9#9#9#9#9'MiscLib.Log.log.Debug("End Server Handling of '+sFunctionname+'","RDTP");');
  AddToImplementation(#9#9#9#9#9'return true;');
  AddToImplementation(#9#9#9#9'}');

  //implementor
  AddToImplementation2(#9#9'//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-');
  AddToImplementation2(#9#9'protected abstract '+sREsultType+' RQ_'+sFunctionName+'('+sReverseParamList+');');
  AddToImplementation2(#9#9'void RQ_HANDLE_'+sFunctionName+sExtendedSymbolList+'()');
  AddToImplementation2(#9#9'{');
  //result var
  if lowercase(sResultType) <> 'void' then
    AddToImplementation2(#9#9#9+sResultType+' res;');
  //VARS
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    AddToImplementation2(#9#9#9+sType+' '+sName+';');
  end;
  //VAR FETCH
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    if pt <> ptOut then
      AddToImplementation2(#9#9#9'RDTP'+sType+'Helper.ReadFromPacket(Call.Request, out '+sName+');');
  end;

//  if sCallingParamList <> '' then
//    sCallingParamList := ', '+sCallingParamList;
  if lowercase(sResultType) <> 'void' then begin
    AddToImplementation2(#9#9#9'res = RQ_'+sFunctionName+'('+sCallingParamList+');');
    AddToImplementation2(#9#9#9'RDTP'+sResultType+'Helper.WriteToPacket(Call.Response, res);');
    inc(iReturns);
  end else begin
    AddToImplementation2(#9#9#9'RQ_'+sFunctionName+'('+sCallingParamList+');');
  end;

  //OUT/VAR RESULT POST
  sRemainder := sParams;
  while SplitString(sRemainder, ':', sName, sType) do begin
    pt := GetParamType(sName);
    SplitString(sType, ' ', sType, sRemainder);
    if pt <> ptNormal then begin
      AddToImplementation2(#9#9#9'RDTP'+sType+'Helper.WriteToPacket(Call.Response, '+sName+');');
      inc(iReturns);
    end;
  end;

  if iReturns = 0 then
    AddToImplementation2(#9#9#9'Forget = true');
  AddToImplementation2(#9#9'}');


end;
//-----------------------------------------------------------------------------
procedure TRDTPClientGenerator.Process_RQ(sParams: string);
var
  sLeft, sRight, sConstant, sFunction, sResultType: string;
begin
  //get constant
  SplitString(sParams, ' ', sConstant, sRight);
  //get function name
  SplitString(sRight, ' ', sFunction, sRight);
  //get result type
  SplitString(sRight, ' ', sResultType, sParams);

  if uppercase(FType) = 'CLIENT' then begin
    Process_Client_RQ('$'+sConstant, sFunction, sResultType, sParams);
    Process_Async_Client_RQ('$'+sConstant, sFunction, sResultType, sParams);
    Process_Async_Client_RSP('$'+sConstant, sFunction, sResultType, sParams);

  end
  else if uppercase(FType) = 'CLIENT_IMPLIB' then
    exit
  else if uppercase(FType) = 'IMPLIB' then
    Process_ImpLib_RQ('$'+sConstant, sFunction, sResultType, sParams)
  else
    Process_Server_RQ('$'+sConstant, sFunction, sResultType, sParams);

end;
//-----------------------------------------------------------------------------
procedure TRDTPClientGenerator.Process_RQ_CSharp(sParams: string);
var
  sLeft, sRight, sConstant, sFunction, sResultType: string;
begin
  //get constant
  SplitString(sParams, ' ', sConstant, sRight);
  //get function name
  SplitString(sRight, ' ', sFunction, sRight);
  //get result type
  SplitString(sRight, ' ', sResultType, sParams);

  if uppercase(FType) = 'CLIENT' then begin
    Process_Client_RQ_CSharp('0x'+sConstant, sFunction, sResultType, sParams);
    Process_Async_Client_RQ_CSharp('0x'+sConstant, sFunction, sResultType, sParams);
    Process_Async_Client_RSP_CSharp('0x'+sConstant, sFunction, sResultType, sParams);

  end
  else if uppercase(FType) = 'CLIENT_IMPLIB' then
    exit
  else if uppercase(FType) = 'IMPLIB' then
    Process_ImpLib_RQ_CSharp('0x'+sConstant, sFunction, sResultType, sParams)
  else
    Process_Server_RQ_CSharp('0x'+sConstant, sFunction, sResultType, sParams);

end;





procedure TRDTPClientGenerator.Process_RQFILE(sParams: string; bCSharp: boolean);
var
  s: string;
CONST
  CONSTANT_RQ_FILE_LOCATION = '..\RDTP_DEFINITIONS\';
begin
  if bCSharp then begin
    s := dllpath+CONSTANT_RQ_FILE_LOCATION+extractfilename(sParams);
    FDirectives.add(LoadStringFromFile(s));
  end else begin
    s := dllpath+CONSTANT_RQ_FILE_LOCATION+extractfilename(sParams);
    FDirectives.add(LoadStringFromFile(s));
  end;
  FDirectives.text := FDirectives.text;
end;

procedure TRDTPClientGenerator.Process_Template(sParams: string; bCSharp: boolean);
var
  sExt: string;
begin
  if bCSharp then
    sExt := '.cs'
  else
    sExt := '.pas';

  FTemplateFileName := slash(extractfilepath(FFileName))+sParams;
  FTemplate := LoadStringFromFile(changefileext(FTemplateFileName, sExt));

end;

procedure TRDTPClientGenerator.Process_Type(sParams: string);
begin
  FType := sParams;
end;

procedure ProcessDir(sDir: string);
var
  t: integer;
  dir: TDirectory;
begin

  dir := TDirectory.create(sDir, '*.pas', 0,0);
  try
    for t:= 0 to dir.filecount-1 do begin
      ProcessUnit(dir.files[t].FullName);

    end;
  finally
    dir.free;
  end;

end;

end.

