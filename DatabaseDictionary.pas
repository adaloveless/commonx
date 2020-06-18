unit DatabaseDictionary;

//todo: Wipe Dictionary on reload
//todo: add field types in report
//TODO 5: Revisit Table PARTS in dictionary
//DONE 1: Allow a join record to look at master table
interface
{xDEFINE FREEDICT}
uses
  betterobject, SharedObject, sysutils, typex, systemx, classes, db, stringx, inifiles,
  inifile, debug, webstring, ExceptionsX, miscinterfaces, storageEngineTypes, helpers.list;

const
  SE_KEYS = 1;
  SE_FIELDS = 0;

type
  TPlexType = (ptStripe, ptArchive, ptMirror);

  TTableDefinition = class;
  TCompatibleQueryCallback = procedure (sQuery: string; out res: TSERowSet) of object;

  TFieldDefinition = class(TinterfacedLockQueuedObject, ICheckSum)
  private
    FDBType: string;
    FName: string;
    FFieldType: TFieldType;
    FTable: TTableDefinition;
    function GetTable: TTableDefinition;
    procedure SetTable(const Value: TTableDefinition);

    procedure SetFieldType(const Value: TFieldType);
    procedure SetName(const Value: string);
    function GetDBTYpe: string;
    function GetName: string;
    function GetFieldType: TFieldType;
  public
    constructor Create(sTable: TTableDefinition);reintroduce;virtual;
    destructor Destroy;override;
    property Name: string read GetName write SetName;
    property DBType: string read GetDBTYpe;
    property FieldType: TFieldType read GetFieldType write SetFieldType;

    property Table: TTableDefinition read GetTable write SetTable;
    function GetCheckSum: integer;
    property CheckSum: integer read GetCheckSum;
  end;


  TTablePartInfo = class(TinterfacedLockQueuedObject, ICheckSum)
  private
    FPlexField: string;
    FPlexOffset: int64;
    FPlexWindow: int64;
    FHost: integer;
    function GetHostID: integer;
    procedure SetHostID(const Value: integer);
    function GetTable: TTableDefinition;
    procedure SetTable(const Value: TTableDefinition);
    procedure SetPlexField(const Value: string);

    function GEtPlexField: string;
    function GetPlexOffset: int64;
    function GetPlexWindow: int64;
    procedure SetPlexOffset(const Value: int64);

    procedure SetPlexWindow(const Value: int64);
  public
    property HostID: integer read GetHostID write SetHostID;
    property PlexField: string read GEtPlexField write SetPlexField;
    property PlexOffset: int64 read GetPlexOffset write SetPlexOffset;
    property PlexWindow: int64 read GetPlexWindow write SetPlexWindow;

    property Table: TTableDefinition read GetTable write SetTable;

    function GetCheckSum: integer;
    property CheckSum: integer read GetCheckSum;

  end;

  TTableRelation = class(TinterfacedLockQueuedObject, ICheckSum)
  private
    FMasterTable: string;
    FDetailTable: string;
    FMasterField: string;
    FDetailField: string;
    FJoinName: string;
    FPublish: boolean;
    function GetPublish: boolean;
    procedure SetPublish(const Value: boolean);
    function GetMasterTable: string;
    procedure SetMasterTable(const Value: string);
    function GetDetailField: string;
    function GetDetailTAble: string;
    function GetJoinName: string;
    function GetMasterField: string;
    procedure SetDetailField(const Value: string);
    procedure SetDetailTable(const Value: string);
    procedure SetJoinName(const Value: string);
    procedure SetMasterField(const Value: string);
  protected
    function GetCheckSum: integer;
  public
    property MasterTable: string read GetMasterTable write SetMasterTable;
    property DetailTable: string read GetDetailTAble write SetDetailTable;
    property MasterField: string read GetMasterField write SetMasterField;
    property DetailField: string read GetDetailField write SetDetailField;
    property JoinName: string read GetJoinName write SetJoinName;
    property Publish: boolean read GetPublish write SetPublish;
    property CheckSum: integer read GetCheckSum;
  end;


  TTableDefinition = class(TinterfacedLockQueuedObject, ICheckSum)
  private
    FPlexType: TPlexType;
    FParts: TList;
    FRelations: TStringList;
    FName: string;
    FPLexField: string;
    FFields: TList;
    FPlexWindow: int64;
    FPrimaryKeys: integer;
    FFullyPromoted: boolean;
    FNative: boolean;
    procedure SetFullyPromoted(const Value: boolean);
    function GetFullyPromoted: boolean;
    function GetRelationByName(sToTable, sName: string): TTableRelation;
    function GetRelation(idx: integer): TTableRelation;

    function GetUserFieldCount: integer;
    function GetUserFields(idx: integer): TFieldDefinition;
    function GetPrimaryKeys: integer;
    procedure SetPrimaryKEys(const Value: integer);

    function GetFieldCount: integer;
    function GetFields(idx: integer): TFieldDefinition;
    function GetFieldByName(sName: string): TFieldDefinition;
    function GetName: string;
    function GetPartCount: integer;
    function GetMultiplexed: boolean;
    function GetTablePartInfo(idx: integer): TTablePartInfo;
    procedure SetName(const Value: string);
    function GetCheckSum: integer;
    function GetNative: boolean;
    procedure SetNative(const value: boolean);
    function GetIsTemporary: boolean;

  public
    constructor create;override;
    destructor destroy;override;

    //General Properties
    property Name: string read GetName write SetName;
    property FullyPromoted: boolean read GetFullyPromoted write SetFullyPromoted;


    //LOADING operations
    procedure LoadFromShowCreateTable(sShowTableReturn: string);
    procedure LoadFromBlankQuery(ds: TSERowset);

    //PARTS
    function AddPart: TTablePartInfo;
    property Parts[idx: integer]: TTablePartInfo read GetTablePartInfo;
    property MultiPlexed: boolean read GetMultiplexed;
    property PartCount: integer read GetPartCount;
    procedure MockUpBlankPart;

    //KEYS
    property PrimaryKeys: integer read GetPrimaryKeys write SetPrimaryKEys;
    function KeysSeparatedByComma: string;

    //FIELDS
    function AddField: TFieldDefinition;overload;
    procedure AddField(fd: TFieldDefinition);overload;
    property Fields[idx: integer]: TFieldDefinition read GetFields;
    property Field[s: string]: TFieldDefinition read GetFieldByName;
    property UserFields[idx: integer]: TFieldDefinition read GetUserFields;
    property FieldCount: integer read GetFieldCount;
    property UserFieldCount: integer read GetUserFieldCount;
    function IndexOfField(sName: string): integer;
    function IndexOfUserField(sName: string): integer;
    function HasField(sName: string): boolean;
    function AllFieldsSeparatedByComma: string;


    //todo 2:read and write packethelpers with primary keys


    //RELATIONS
    function AddRelation(sToTable, sName: string): TTableRelation;overload;
    procedure AddRelation(tr: TTableRelation);overload;
    property Relations[idx: integer]: TTableRelation read GetRelation;
    property Relation[sToTable, sName: string]:TTableRelation read GetRelationByName;
    function RelationCount: integer;
    function IndexOfRelation(sName: string): integer;

    property Native: boolean read GetNative write SetNative;
    property CheckSum: integer read GetCheckSum;

    property IsTemporary: boolean read GetIsTemporary;

  end;


  TStorageEngineInfo = class(TinterfacedLockQueuedObject, ICheckSum)
  private
    FDB: string;
    FSE: string;
    FID: integer;
    FEndpoint: string;
    FSystempower: real;
    FContext: string;
    procedure SetSE(const Value: string);
    procedure SetDB(const Value: string);
    procedure SetEndpoint(const Value: string);
    function GetID: integer;
    procedure SetID(const Value: integer);

    function GEtEndpoint: string;
    function GetSystemPOwer: real;
    procedure SetSystemPower(const Value: real);
    function GEtDB: string;
    function GetSE: string;
    function GEtContext: string;
    procedure SetContext(const Value: string);
  public
    property ID: integer read GetID write SetID;
    property SE: string read GetSE write SetSE;
    property Endpoint: string read GEtEndpoint write SetEndpoint;
    property DB: string read GEtDB write SetDB;
    property SystemPower: real read GetSystemPOwer write SetSystemPower;
    property Context: string read GEtContext write SetContext;
    function GetCheckSum: integer;
    property CheckSum: integer read GEtCheckSum;
    procedure LoadConfig(sINI: string; sIniSection: string);

  end;

  TDatabaseDictionary = class(TinterfacedLockQueuedObject, ICheckSum)
  private
    FTables: TList;
    FHosts: TList;
    FOldDictionary: TDAtabaseDictionary;
    FFetched: boolean;
    FContext: string;
    FLoaded: boolean;
    FCheckSum: integer;
    FConfig: string;
    function GetFetched: boolean;
    procedure SetFetched(const Value: boolean);
    function GetOld: TDatabaseDictionary;
    procedure SetOld(const Value: TDatabaseDictionary);
    function GetTableDefinitionByName(sName: string): TTableDefinition;
    //function GetTableLocation(idx: integer): string;
    function GetHostCount: integer;
    function GetTableCount: integer;
    function GetTableDefinition(idx: integer): TTableDefinition;
    function GetHost(idx: integer): TStorageEngineInfo;
    function GetContext: string;
    procedure SetContext(const Value: string);
    function GetLoaded: boolean;
    procedure SetLoaded(const Value: boolean);


  public
    constructor Create(sContext: string; sConfig: string);reintroduce;virtual;
    destructor Destroy;override;
    function LoadFromDatabase(bNativeMode: boolean; cb: TCompatibleQueryCallback):boolean;
    procedure AssumeNaturalRelations;
    property Hosts[idx: integer]:TStorageEngineInfo read GetHost;
    property HostCount: integer read GEtHostCount;
    property Tables[idx: integer]: TTableDefinition read GetTableDefinition;
    property Table[sName: string]: TTableDefinition read GetTableDefinitionByName;
    property TableCount: integer read GetTableCount;

    function AddTable:TTableDefinition;overload;
    function AddHost:TStorageEngineInfo;overload;
    procedure AddTable(td: TTableDefinition);overload;
    procedure AddHost(h: TStorageEngineInfo);overload;



    function AddTableFromBlankQuery(sName: string; ds: TSERowset): TTableDefinition;
    function AddTableFromShowCreateTable(sShowTableReturn: string): TTableDefinition;
    function GetStructureReport: string;

    procedure Clear;

    property OldDictionary: TDatabaseDictionary read GetOld write SetOld;

    procedure LoadHostsFromConfig;

    property Fetched: boolean read GetFetched write SetFetched;
    property Loaded: boolean read GetLoaded write SetLoaded;

    function GenerateDataModelCode(sUnitName: string): string;
    property Context: string read GetContext write SetContext;
    procedure ClearHosts;

    function GEtCheckSum: integer;
    procedure CalcCheckSum;
    property CheckSum: integer read GEtCheckSum;
    property Config: string read FConfig write FConfig;


  end;

  TDatabaseDictionaryManager = class(TinterfacedLockQueuedObject)
  private
    FHeads: TStringList;
    function IndexOfContext(sContext: string): integer;
    function GetDict(sContext: string): TDatabasedictionary;
    procedure SetDict(sContext: string; const Value: TDatabasedictionary);
  public
    constructor create;override;
    destructor Destroy;override;
    property Head[sContext: string]: TDatabasedictionary read GetDict write SetDict;
    procedure Invalidate(sContext:string);
  end;

function FieldTypeToString(ft: TFieldType): string;


var
  DMAN_Client: TDatabaseDictionaryManager;
  DMAN_Coord: TDatabaseDictionaryManager;
  DMAN_SE: TDatabaseDictionaryManager;

//todo: make multiple copies of dict to reduce risk of deadlock

implementation




{ TTableDefinition }




function FieldTypeToString(ft: TFieldType): string;
begin
  case ft of
    ftUnknown: result := 'variant';
    ftString: result := 'varchar(255)';
    ftSmallint: result := 'Smallint';
    ftInteger: result := 'Integer';
    ftWord: result := 'Word';
    ftBoolean: result := 'Boolean';
    ftFloat: result := 'Float';
    ftCurrency: result := 'Currency';
    ftBCD: result := 'BCD';
    ftDate: result := 'DateTime';
    ftTime: result := 'DateTime';
    ftDateTime: result := 'DateTime';
    ftBytes: result := 'variant';
    ftVarBytes: result := 'variant';
    ftAutoInc: result := 'integer';
    ftBlob: result := 'variant';
    ftMemo: result := 'variant';
    ftGraphic: result := 'variant';
    ftFmtMemo: result := 'variant';
    ftParadoxOle: result := 'variant';
    ftDBaseOle: result := 'variant';
    ftTypedBinary: result := 'variant';
    ftCursor: result := 'variant';
    ftFixedChar: result := 'string';
    ftWideString: result := 'widestring';
    ftLargeint: result := 'int64';
    ftADT: result := 'variant';
    ftArray: result := 'variant';
    ftReference: result := 'variant';
    ftDataSet: result := 'variant';
    ftOraBlob: result := 'variant';
    ftOraClob: result := 'variant';
    ftVariant: result := 'variant';
    ftInterface: result := 'variant';
    ftIDispatch: result := 'variant';
    ftGuid: result := 'variant';
    ftTimeStamp: result := 'variant';
    ftFMTBcd: result := 'variant';
    ftFixedWideChar: result := 'widestring';
    ftWideMemo: result := 'widestring';
    ftOraTimeStamp: result := 'datetime';
    ftOraInterval: result := 'datetime';
  end;
end;

function TTableDefinition.AddField: TFieldDefinition;
begin
  //TODO 3: Check that shit is locked in this ENTIRE unit.
  self.LockWrite;
  try
    result := TFieldDefinition.create(self);
    self.FFields.Add(result);
  finally
    self.UnlockWrite;
  end;
end;

procedure TTableDefinition.LoadFromShowCreateTable(sShowTableReturn: string);
var
  sLeft, sright, sQuote: string;
  td: TTableDefinition;
  fd: TFieldDefinition;

begin
  td := self;
  LockWrite;
  try
    sRight := sShowTableREturn;


      if SplitQuote(sRight, '`', sLeft, sRight, sQuote) then
      td.Name := sQuote;

      while SplitQuote(sRight, '`', sLeft, sRight, sQuote) do begin
        //found a field
        fd := td.AddField;
        fd.Name := sQuote;
        SplitString(sRight, ',', sLeft, sRight);
        fd.FieldType := storageEngineTYpes.MysqlTypeToFieldType(sLeft);

        if (copy(TrimStr(sRight), 1, length('KEY')) = 'KEY') then
          exit;

        if (copy(TrimStr(sRight), 1, length('PRIMARY')) = 'PRIMARY') then
          break;

      end;

    //DONE: record keys
    FPrimaryKeys := 0;
    if SplitString(sright, '(', sLeft, sRight) then begin
      if SplitString(sRight, ')', sLeft, sRight) then begin
        sRight := sLeft;
        while SplitQuote(sRight, '`', sLeft, sRight, sQuote) do begin
          inc(FPrimaryKeys);
        end;
      end;
    end;
  finally
    Unlockwrite;
  end;

end;

procedure TTableDefinition.LoadFromBlankQuery(ds:TSERowset);
var
  sLeft, sright, sQuote: string;
  td: TTableDefinition;
  fd: TFieldDefinition;
  t: integer;
begin
  td := self;
  LockWrite;
  try
    for t:=0 to ds.FieldCount-1 do begin
      fd := td.AddField;
      fd.Name := ds.FieldDefs[t].sName;
      fd.FieldType := ds.FieldDefs[t].vType;
    end;

  finally
    Unlockwrite;
  end;

end;


procedure TTableDefinition.MockUpBlankPart;
var
  p: TTablePartInfo;
begin
  LockWrite;
  try
    p := self.AddPart;

    p.HostID := 0;
  finally
    UnlockWrite;
  end;

end;

function TTableDefinition.RelationCount: integer;
begin
  LockRead;
  try
    result := self.FRelations.Count;
  finally
    UnlockRead;
  end;
end;

procedure TTableDefinition.AddField(fd: TFieldDefinition);
begin
  LockWrite;
  try
    fd.Table := self;
    self.FFields.add(fd);
  finally
    UnlockWRite;
  end;
end;

function TTableDefinition.AddPart: TTablePartInfo;
begin

  LockWrite;
  try
    result := TTablePartInfo.create;
    FParts.add(result);
  finally
    UnlockWrite;
  end;

end;

procedure TTableDefinition.AddRelation(tr: TTableRelation);
begin
  LockWrite;
  try
//    FRelations.addObject(lowercase(tr.DetailTable+'.'+tr.JoinName), tr);
    FRelations.addObject(lowercase(tr.JoinName), tr);
  finally
    UnlockWrite;
  end;
end;



function TTableDefinition.AllFieldsSeparatedByComma: string;
var
  t: integer;
begin
  result := '';
  for t:= 0 to UserFieldCount-1 do begin
    if result <> '' then
      result := result+',';
    result := result + UserFields[t].Name;
  end;

end;

function TTableDefinition.AddRelation(sToTable, sName: string): TTableRelation;
begin
  result := TTableRelation.create;
//  result.JoinName := lowercase(sToTable+'.'+sName);;
  result.DetailTAble := sToTAble;    
  result.JoinName := lowercase(sName);;

  AddRelation(result);
end;

constructor TTableDefinition.create;
begin
  inherited;
  FParts := TList.create;
  FRelations := TStringList.create;

  self.FFields := TList.create;
end;

destructor TTableDefinition.destroy;
begin
  FreeListContents(FParts);
  FParts.free;
  FParts := nil;
  FreeListContents(FFields);
  FFields.free;
  FFields := nil;
  FreeStringListContents(FRelations);
  FRelations.Free;
  FRelations := nil;


  inherited;
end;

function TTableDefinition.GetCheckSum: integer;
var
  t: integer;
begin
  LockRead;
  try
    result := 0;
    inc(result, GetCheckSumForVariant(ord(self.FPlexType)));
    inc(result, GetCheckSumForVariant(self.FName));
    inc(result, GetCheckSumForVariant(self.FPLexField));

    for t:= 0 to PartCount-1 do begin
      inc(result, parts[t].CheckSum);
    end;
    for t:= 0 to self.RelationCount-1 do begin
      inc(result, relations[t].CheckSum);
    end;

  finally
    UnlockRead;
  end;
end;

function TTableDefinition.GetFieldByName(sName: string): TFieldDefinition;
var
  i : integer;
begin
  result := nil;
  LockRead;
  try
    i := IndexOfField(sName);
    if i < 0 then
      raise exception.create('field '+sName+' could not be found in table '+Name);
    result := Fields[i];



  finally
    UnlockRead;
  end;
end;

function TTableDefinition.GetFieldCount: integer;
begin
  LockRead;
  try
    result := FFields.count;
  finally
    UnlockRead;
  end;
end;

function TTableDefinition.GetFields(idx: integer): TFieldDefinition;
begin
  LockRead;
  try
    result := FFields[idx];
  finally
    UnlockRead;
  end;

end;

function TTableDefinition.GetFullyPromoted: boolean;
begin
  LockRead;
  try
    result := FFullyPromoted;
  finally
    UnlockREad;
  end;
end;

function TTableDefinition.GetIsTemporary: boolean;
begin
  result := lowercase(copy(self.Name,1,4))='temp';
  if result then begin
    if length(name) = 4 then
      exit
    else begin
      result := charinset(self.name[5], ['0'..'9']);
//      result := self.name[5] in ['0'..'9'];
    end;
  end;
end;

function TTableDefinition.GetMultiplexed: boolean;
begin
  LockRead;
  try
    result := PartCount > 1;
  finally
    UnlockRead;
  end;
end;

function TTableDefinition.GetName: string;
begin
  LockRead;
  try
    result := FName;
  finally
    UnlockRead;
  end;
end;

function TTableDefinition.GetNative: boolean;
begin
  LockRead;
  try
    result := Fnative;
  finally
    UnlockREad;
  end;
end;

function TTableDefinition.GetPartCount: integer;
begin
  LockRead;
  try
    result := FParts.count;
    //always have at least one part
    if result = 0 then begin
      result := 1;
      MockUpBlankPart;
    end;
  finally
    UnlockRead;
  end;
end;

function TTableDefinition.GetPrimaryKeys: integer;
begin
  LockRead;
  try
    result := FPrimaryKeys;
  finally
    UnlockRead;
  end;
end;

function TTableDefinition.GetRelation(idx: integer): TTableRelation;
begin
  LockRead;
  try
    result := FRelations.objects[idx] as TTableRelation;
  finally
    UnlockRead;
  end;
end;

function TTableDefinition.GetRelationByName(sToTable, sName: string): TTableRelation;
var
  i: integer;
begin
  result := nil;
  LockRead;
  try
    i := IndexOfRelation(lowercase(sToTable+'.'+sName));
    if i < 0 then
      raise Exception.create('relation not found from table '+name+' to table '+sTOTable+' named: "'+sName+'" fullname: "'+sToTable+'.'+sName+'"');

    result := FRelations.objects[i] as TTableRelation;
  finally
    UnlockRead;
  end;
end;

function TFieldDefinition.GetTable: TTableDefinition;
begin
  LockRead;
  try
    result := FTable;
  finally
    Unlockread;
  end;
end;

function TTableDefinition.GetTablePartInfo(idx: integer): TTablePartInfo;
begin
  LockRead;
  try
    result := FParts[idx];
  finally
    UnlockRead;
  end;
end;

function TTableDefinition.GetUserFieldCount: integer;
begin
  if Native then
    result := FieldCount
  else
    result := FieldCount-(SE_KEYS+SE_FIELDS);
end;

function TTableDefinition.GetUserFields(idx: integer): TFieldDefinition;
begin
  LockREad;
  try
    if Native then
      result := FFields[idx]
    else
      result := FFields[idx+(SE_KEYS+SE_FIELDS)];

  finally
    UnLockRead;
  end;
end;

function TTableDefinition.HasField(sName: string): boolean;
begin
  result := IndexOfField(sName) >=0;
end;

function TTableDefinition.IndexOfField(sName: string): integer;
var
  t: integer;
begin
  sName := lowercase(trimstr(sName));
  result := -1;
  for t:= 0 to FieldCount-1 do begin
    if lowercase(Fields[t].Name)= sName then begin
      result := t;
      exit;
    end;
  end;

end;

function TTableDefinition.IndexOfRelation(sName: string): integer;
begin
  sName := lowercase(sName); //TODO 3: Make sure relation names are NOT case sensitive
  result := FRelations.IndexOf(sName);
end;

function TTableDefinition.IndexOfUserField(sName: string): integer;
var
  t: integer;
begin
  sName := lowercase(trimstr(sName));
  result := -1;
  for t:= 0 to UserFieldCount-1 do begin
    if lowercase(UserFields[t].Name)= sName then begin
      result := t;
      exit;
    end;
  end;

end;

function TTableDefinition.KeysSeparatedByComma: string;
var
  iKeyCount: integer;
  t: integer;
begin
  iKeyCount := self.PrimaryKeys;
  result := '';
  for t := 0 to iKeyCount-1 do begin
    if result <>'' then result := result+',';
    result := result+self.UserFields[t].Name;
  end;

end;

procedure TTableDefinition.SetFullyPromoted(const Value: boolean);
begin
  LockWrite;
  try
    FFullyPromoted := value;
  finally
    UnlockWrite;
  end;
end;


procedure TTableDefinition.SetName(const Value: string);
begin
  LockWrite;
  try
    if copy(uppercase(value), 1,4) = 'SE__' then
      FName := copy(value, 5,99999999)
    else
      FName := value;
  finally
    UnlockWrite;
  end;
end;


procedure TTableDefinition.SetNative(const value: boolean);
begin
  LockWrite;
  try
    FNative := value;
  finally
    UnlockWrite;
  end;
end;

procedure TTableDefinition.SetPrimaryKEys(const Value: integer);
begin
  LockWrite;
  try
    FPrimaryKeys := VALUE;
  finally
    Unlockwrite;
  end;
end;

procedure TFieldDefinition.SetTable(const Value: TTableDefinition);
begin
  LockWrite;
  try
    FTable := value;
  finally
    UnlockWrite;
  end;
end;

{ TTablePartInfo }


function TTablePartInfo.GetCheckSum: integer;
begin
  LockREad;
  try
    result := 0;
    inc(result, GetCheckSumForVariant(self.FPlexField));
    inc(result, GetCheckSumForVariant(self.FPlexOffset));
    inc(result, GetCheckSumForVariant(self.FPlexWindow));
    inc(result, GetCheckSumForVariant(self.FHost));
  finally
    UnlockRead;
  end;
end;

function TTablePartInfo.GetHostID: integer;
begin
  LockRead;
  try
    result := Fhost;
  finally
    UnlockRead;
  end;
end;


function TTablePartInfo.GEtPlexField: string;
begin
  LockRead;
  try
    result := FPlexField;
  finally
    UnlockRead;
  end;
end;

function TTablePartInfo.GetPlexOffset: int64;
begin
  LockRead;
  try
    result := FPlexOffset;
  finally
    UnlockRead;
  end;
end;

function TTablePartInfo.GetPlexWindow: int64;
begin
  LockRead;
  try
    result := FPlexWindow;
  finally
    UnlockRead;
  end;
end;



function TTablePartInfo.GetTable: TTableDefinition;
begin
  result := nil;
//TODO -cunimplemented: unimplemented block
end;


procedure TTablePartInfo.SetHostID(const Value: integer);
begin
  LockWrite;
  try
    FHost := value;
  finally
    UnlockWrite;
  end;
end;


procedure TTablePartInfo.SetPlexField(const Value: string);
begin
  LockWrite;
  try
    FPLexField := value;
  finally
    UnlockWRite;
  end;
end;

procedure TTablePartInfo.SetPlexOffset(const Value: int64);
begin
  LockWrite;
  try
    FPLexOffset := value;
  finally
    UnlockWRite;
  end;

end;


procedure TTablePartInfo.SetPlexWindow(const Value: int64);
begin
  LockWrite;
  try
    FPLexWindow := value;
  finally
    UnlockWRite;
  end;
end;


procedure TTablePartInfo.SetTable(const Value: TTableDefinition);
begin

//TODO -cunimplemented: unimplemented block
end;

{ TDatabaseDictionary }

function TDatabaseDictionary.AddHost: TStorageEngineInfo;
begin
  LockWRite;
  try
    result := TStorageEngineInfo.create;
    self.FHosts.add(result);
  finally
    UnlockWrite;
  end;
end;

function TDatabaseDictionary.AddTable: TTableDefinition;
begin
  LockWrite;
  try
    result := TTableDefinition.create;
    self.FTables.Add(result);
  finally
    UnlockWrite;
  end;
end;

procedure TDatabaseDictionary.AddTable(td: TTableDefinition);
begin
  LockWrite;
  try
    FTables.add(td);
  finally
    UnlockWrite;
  end;
end;

function TDatabaseDictionary.AddTableFromShowCreateTable(sShowTableReturn: string): TTableDefinition;
var
  fd: TFieldDefinition;
  sQuote, sLEft, sRight: string;
begin
  result := AddTable;
  result.LoadFromShowCreateTable(sShowTableReturn);

end;

function TDatabaseDictionary.AddTableFromBlankQuery(sName: string; ds: TSERowSet): TTableDefinition;
var
  fd: TFieldDefinition;
  sQuote, sLEft, sRight: string;
begin
  result := AddTable;
  result.Name := sNAme;
  result.LoadFromBlankQuery(ds);

end;


procedure TDatabaseDictionary.AssumeNaturalRelations;
var
  ds: TSERowSet;
  t,u,v: integer;
  td, td2: TTableDefinition;
  fd: TFieldDefinition;
  d: TDatabaseDictionary;
  s1,s2: string;
  bPrimePrime: boolean;
  bForeignPrime: boolean;
  tr: TTableRelation;
begin
  d := self;
  d.LockRead;
  try
    for t:= 0 to d.TableCount-1 do begin
      td := d.Tables[t];


      //for each field in the table
      for u := td.UserFieldCount-1 downto 0 do begin
        fd := td.UserFields[u];
        if pos('id', lowercase(fd.Name)) > 0 then begin
          //look for tables with same id
          for v := 0 to d.TableCount-1 do begin
            //skip if tables are the same (do not relate table to itself)
            if t= v then continue;

            td2 := d.Tables[v];

            if td2.HasField(fd.Name) then begin
              //make a decision as to if we should publish this relation
              //to the data model
              s1 := lowercase(fd.Name);
              s2 := lowercase(td.Name);
              s2 := stringreplace(s2,'_','', [rfReplaceAll]);
              s1 := stringreplace(s1,'_','', [rfReplaceAll]);
              s1 := copy(s1, 1, length(s1)-2);
              bPrimePrime := (s1=s2) and (u=td2.IndexOfUserField(fd.Name));

              s1 := lowercase(fd.Name);
              s2 := lowercase(td2.Name);
              s2 := stringreplace(s2,'_','', [rfReplaceAll]);
              s1 := stringreplace(s1,'_','', [rfReplaceAll]);
              s1 := copy(s1, 1, length(s1)-2);
              bForeignPrime := (s1=s2) and (u>td2.IndexOfUserField(fd.Name));

              tr := td.AddRelation(td2.Name, '');
              tr.MasterField := fd.Name;
              tr.DetailField := fd.Name;
              tr.DetailTAble := td2.Name;
              tr.Publish := bPrimePrime or bForeignPrime;
            end;
          end;
        end;

      end;
    end;
  finally
    d.UnlockRead;
  end;

end;

procedure TDatabaseDictionary.CalcCheckSum;
begin
  LockWrite;
  try
    FCheckSum := CheckSum;
  finally
    UnlockWrite;
  end;
end;

procedure TDatabaseDictionary.Clear;
begin
  FreeListContents(FHosts);
  FHosts.clear;
  FreeListContents(FTables);
  FTables.clear;

end;

procedure TDatabaseDictionary.ClearHosts;
begin
  LockWrite;
  try
    FreeListContents(FHosts);
    FHosts.clear;
  finally
    UnlockWRite;
  end;
end;

constructor TDatabaseDictionary.Create(sContext: string; sConfig: string);
begin
  inherited CReate;
  self.FTables := TList.create;
  FHosts := TList.create;
  FContext := sContext;
  FConfig := sConfig;
//  self.LoadHostsFromConfig;

end;

destructor TDatabaseDictionary.Destroy;
begin
  Clear;
  FTables.free;
  FHosts.free;
  if assigned(FOldDictionary) then
    FOldDictionary.FreeWhenUnlocked := true;
  inherited;
end;

function TDatabaseDictionary.GenerateDataModelCode(sUnitName: string): string;
var
  t,u,v: integer;
  sl: TStringlist;
  td: TTableDefinition;
  sON: string;
begin
  //TBL_msuser = class
  //private
  //function GetLinkToTop8(friendid: variant)

  sl := TStringlist.create;
  try
    sl.add('unit '+sUnitName+';');
    sl.add('interface');
    sl.add('uses variants, DB;');
    sl.add('type');
    //forward declare all tables
    for t:= 0 to self.TableCount-1 do begin
      if not self.tables[t].IsTemporary then begin
        sl.add('  TBL_'+self.Tables[t].Name+'=class;//forward');
      end;
    end;

    for t:= 0 to self.TableCount-1 do begin
      td := self.Tables[t];
      if td.IsTemporary then
        continue;
      //class header
      sl.add('  TBL_'+td.Name+'=class');
      //privates
      sl.add('  public');

      for u:= 0 to td.UserFieldCount-1 do begin
        sl.add('    _'+padstring(inttostr(u), '0', 2)+'_'+td.UserFields[u].Name+':'+FieldTypeToString(td.UserFields[u].FieldType)+';');
      end;

      //supporting relation functions
      for u := 0 to td.RelationCount-1 do begin
        if lowercase(copy(td.relations[u].DetailTable, 1,4)) = 'temp' then
          continue;

        if not td.Relations[u].Publish then
          continue;
        sON := td.Relations[u].JoinName;
        if sON <> '' then
          sON := '_'+sOn;
        sl.add('    LinkTo_'+td.relations[u].DetailTable+'_'+StringReplace(td.Relations[u].JoinName,'=','_eq_', [rfREplaceAll])+'_'+td.Relations[u].DetailField+': TBL_'+td.relations[u].DetailTable+';');
      end;
      sl.add('  end;');
    end;

    sl.add('implementation');
    sl.add('end.');

    result := sl.text;
  finally
    sl.free;
  end;



end;

function TDatabaseDictionary.GEtCheckSum: integer;
var
  t: integer;
begin
  LockRead;
  try
    if FCheckSum <> 0 then begin
      result:= FCheckSum;
      exit;
    end;

    result := 0;
    for t:= 0 to self.TableCount -1 do begin
      inc(result, self.Tables[t].CheckSum);
    end;
    for t:= 0 to self.HostCount-1 do begin
      inc(result, self.Hosts[t].CheckSum);
    end;
  finally
    UnlockRead;
  end;

end;

function TDatabaseDictionary.GetContext: string;
begin
  LockRead;
  try
    result := FContext;
  finally
    UnlockRead;
  end;
end;

function TDatabaseDictionary.GetFetched: boolean;
begin
  LockRead;
  try
    result := FFetched;
  finally
    Unlockread;
  end;

end;

function TDatabaseDictionary.GetHost(idx: integer): TStorageEngineInfo;
begin
  result := nil;
  LockRead;
  try
    if idx >= HostCount then
      raise EClassException.create('Host index out of bounds: '+inttostr(idx));
    result := FHosts[idx];
  finally
    UnlockRead;
  end;

end;

function TDatabaseDictionary.GEtHostCount: integer;
begin
  LockRead;
  try
    result := FHosts.count;
  finally
    UnlockRead;
  end;
end;


function TDatabaseDictionary.GetLoaded: boolean;
begin
  LockREad;
  try
    result := FLoaded;
  finally
    UnlockRead;
  end;
end;

function TDatabaseDictionary.GetOld: TDatabaseDictionary;
begin
  LockRead;
  try
    result := FOldDictionary;
  finally
    UnlockRead;
  end;
end;

function TDatabaseDictionary.GetStructureReport: string;
var
  t,u: integer;
  sl: TStringlist;
  td: TTableDefinition;
  fd: TFieldDefinition;
begin
  sl := TStringList.create;
  LockRead;
  try
    try
    sl.add('DATABASE DICTIONARY REPORT');
//    sl.add('--------------------------');
        for t:= 0 to self.TableCount-1 do begin
      td := self.Tables[t];
      sl.Add('------------------------------');
      sl.Add('['+td.Name+']');
      sl.Add('------------------------------');
            for u := 0 to td.FieldCount-1 do begin
        sl.add(td.Fields[u].Name);
      end;
    end;


    except
      on E: Exception do begin
        sl.add(E.Message);
      end;
    end;
  finally
    UnlockRead;
    result := sl.text;
    sl.free;
  end;


end;

function TDatabaseDictionary.GetTableCount: integer;
begin
  LockRead;
  try
    result := FTables.count;
  finally
    Unlockread;
  end;

end;

function TDatabaseDictionary.GetTableDefinition(idx: integer): TTableDefinition;
begin
  LockRead;
  try
    result := FTables[idx];
  finally
    UnlockRead;
  end;

end;

function TDatabaseDictionary.GetTableDefinitionByName(
  sName: string): TTableDefinition;
var
  t: integer;  
begin
  result := nil;
  LockRead;
  try
    sName := lowercase(sName);
    for t:= 0 to TableCount-1 do begin
      if lowercase(tables[t].Name) = sName then begin
        result := tables[t];
        break;
      end;
    end;
  finally
    UnlockRead;
  end;


end;



procedure TDatabaseDictionary.LoadHostsFromConfig;
var
  h: TStorageEngineInfo;
  ini: TMothershipInifile;
  t: integer;
  sFile: string;
  sContext: string;
begin
  LockWrite;
  try
    self.ClearHosts;

    ini := TMothershipINIFile.create;

    try
      ini.FileContents := self.Config;


      t:= 0;
      while ini.ReadString('engine'+inttostr(t), 'Host', '!undefined') <> '!undefined' do begin
        h := self.AddHost;
        h.LoadConfig(ini.FileContents,   'engine'+inttostr(t));
        inc(t);
      end;

    finally
      ini.free;
    end;

    Loaded := true;

  finally
    Unlockwrite;
  end;

end;


procedure TDatabaseDictionary.SetContext(const Value: string);
begin
  FContext := value;
  Debug.Log(self,self.classname+' context set: '+value);

end;

procedure TDatabaseDictionary.SetFetched(const Value: boolean);
begin
  LockWrite;
  try
    FFetched := value;
    if self.HostCount = 0 then
      self.LoadHostsFromConfig;
  finally
    Unlockwrite;
  end;
end;

procedure TDatabaseDictionary.SetLoaded(const Value: boolean);
begin
  LockWrite;
  try
    FLoaded := value;
  finally
    UnlockWRite;
  end;
end;

procedure TDatabaseDictionary.SetOld(const Value: TDatabaseDictionary);
begin
  LockWrite;
  try
    FOldDictionary := Value;
  finally
    UnlockWrite;
  end;
end;

procedure TDatabaseDictionary.AddHost(h: TStorageEngineInfo);
begin
  LockWrite;
  try
    FHosts.add(h);
  finally
    UnlockWrite;
  end;

end;

{ TFieldDefinition }

constructor TFieldDefinition.Create;
begin
  inherited Create;

end;

destructor TFieldDefinition.Destroy;
begin

  inherited;
end;

function TFieldDefinition.GetCheckSum: integer;
begin
  LockREad;
  try
    result := 0;
    inc(result, GetCheckSumForVariant(self.FDBType));
    inc(result, GetCheckSumForVariant(self.FName));
    inc(result, GetCheckSumForVariant(ord(self.FFieldType)));
  finally
    UnlockRead;
  end;
end;

function TFieldDefinition.GetDBTYpe: string;
begin
  LockRead;
  try
    result := FDBType;
  finally
    UnlockRead;
  end;
end;

function TFieldDefinition.GetName: string;
begin
  LockRead;
  try
    result := FNAme;
  finally
    UnlockRead;
  end;
end;

procedure TFieldDefinition.SetFieldType(const Value: TFieldType);
begin
  LockWrite;
  try
    self.FFieldType := value;
  finally
    UnlockWrite;
  end;
end;

procedure TFieldDefinition.SetName(const Value: string);
begin
  Lockwrite;
  try
    self.FName := value;
  finally
    UnlockWRite;
  end;
end;

function TFieldDefinition.GEtfieldType: TFieldType;
begin
  LockRead;
  try
    result := FFieldType;
  finally
    UnlockRead;
  end;
end;

{ TStorageEngineInfo }

function TStorageEngineInfo.GetID: integer;
begin
  LockRead;
  try
    result := FID;
  finally
    UnlockREad;
  end;
end;

function TStorageEngineInfo.GetCheckSum: integer;
begin
  LockREad;
  try
    result := 0;
    inc(result, GEtCheckSumForVAriant(self.FDB));
    inc(result, GEtCheckSumForVAriant(self.FSE));
    inc(result, GEtCheckSumForVAriant(self.FID));
    inc(result, GEtCheckSumForVAriant(self.FEndpoint));
    inc(result, GEtCheckSumForVAriant(self.FSystempower));

  finally
    UnlockRead;
  end;
end;

function TStorageEngineInfo.GEtContext: string;
begin
  LockRead;
  try
    result := FContext;
  finally
    UnlockREad;
  end;
end;

function TStorageEngineInfo.GEtDB: string;
begin
  LockRead;
  try
    result := FDB;
  finally
    UnlockRead;
  end;
end;

function TStorageEngineInfo.GEtEndpoint: string;
begin
  LockRead;
  try
    result := FEndpoint;
  finally
    UnlockRead;
  end;


end;

function TStorageEngineInfo.GetSE: string;
begin
  LockRead;
  try
    result := FSE;
  finally
    UnlockRead;
  end;
end;


function TStorageEngineInfo.GetSystemPOwer: real;
begin
  LockRead;
  try
    result := self.FSystempower;
  finally
    UnlockRead;
  end;
end;

procedure TStorageEngineInfo.LoadConfig(sINI: string; sIniSection: string);
var
  ini: TMothershipINIFile;
begin
  ini := TMothershipINIFile.Create;
  try
    ini.FileContents := sIni;
    self.SE := ini.ReadString(sIniSection, 'Host', '!undefined');
    if self.se = '!undefined' then begin
      sIniSection := 'engine';
    end;
    self.SE := ini.ReadString(sIniSection, 'Host', '!undefined');
    if self.se = '!undefined' then begin
      raise exception.create('undefined host when loading storage engine config');
    end;
    self.Endpoint := ini.ReadString(sIniSection, 'Endpoint', '!undefined');
    self.Context :=  HexToString(ini.ReadString(sIniSection, 'Context',''));
  finally
    ini.free;
  end;

end;

procedure TStorageEngineInfo.SetID(const Value: integer);
begin
  LockWrite;
  try
    FID := Value;
  finally
    UnlockWrite;
  end;
end;


procedure TStorageEngineInfo.SetContext(const Value: string);
begin
  LockWRite;
  try
    FContext := Value;
  finally
    UnlockWRite;
  end;
end;

procedure TStorageEngineInfo.SetDB(const Value: string);
begin
  LockWrite;
  try
    FDB := Value;
  finally
    UnlockWrite;
  end;
end;

procedure TStorageEngineInfo.SetEndpoint(const Value: string);
begin
  LockWrite;
  try
    FEndpoint := Value;
  finally
    UnlockWrite;
  end;
end;


procedure TStorageEngineInfo.SetSE(const Value: string);
begin
  LockWrite;
  try
    FSE := Value;
  finally
    UnlockWrite;
  end;

end;

procedure TStorageEngineInfo.SetSystemPower(const Value: real);
begin
  LockWrite;
  try
    FSystemPower := Value;
  finally
    UnlockWrite;
  end;
end;

{ TDatabaseDictionaryManager }

constructor TDatabaseDictionaryManager.create;
begin
  inherited;
  FHeads := TStringList.create;
end;

destructor TDatabaseDictionaryManager.Destroy;
begin
  FreeStringListContents(FHeads);
  FHeads.free;
  inherited;
end;

function TDatabaseDictionaryManager.GetDict(sContext: string): TDatabasedictionary;
var
  i: integer;
  s: string;
begin
  LockREad;
  try
    i := FHeads.IndexOf(lowercase(sContext));
    if i < 0 then begin
      LockWrite;
      try
        s := DLLPath+sContext+'.ini';
        s := LoadStringFromFile(s);
        result := TDatabaseDictionary.create(sContext,  s);
        result.Context := sContext;

        FHeads.AddObject(lowercase(sContext), result);
      finally
        UnlockWRite;
      end;

    end else
      result := FHeads.objects[i] as TDatabaseDictionary;
  finally
    UnLockRead;
  end;
end;


function TDatabaseDictionaryManager.IndexOfContext(sContext: string): integer;
begin
  result := FHeads.IndexOf(sContext);

end;

procedure TDatabaseDictionaryManager.Invalidate(sContext: string);
var
  dContext: TDatabaseDictionary;
begin
  LockWrite;
  try
    if self.IndexOfContext(sContext) < 0 then
      exit;

    Head[sContext].Fetched := false;

  finally
    UnlockWRite;
  end;
end;

procedure TDatabaseDictionaryManager.SetDict(sContext:string; const Value: TDatabasedictionary);
var
  oldold: TDatabaseDictionary;
  bFree: boolean;
  h: TDatabaseDictionary; //head
  i: integer;
begin
  bFree := false;
  LockWrite;
  try
    Value.CalcCheckSum;
    h := Head[sContext];
    if h = value then
      exit;
    if assigned(h) then begin
      value.OldDictionary := h;
{$IFDEF FREEDICT}
      //TODO 2: Fix this... figure out how to clean up dictionaries
      if assigned(value.Olddictionary) then begin
        oldold := value.OldDictionary.OldDictionary; //TODO 2: Make "OLDEST dictionary" property
        if assigned(oldold) then begin
          oldold.FreeWhenUnlocked := true;
        end;
      end;
{$ENDIF}

    end;

    i := FHeads.IndexOf(sContext);
    if i < 0 then
      FHeads.addobject(lowercase(sContext), value)
    else
      FHeads.objects[i] := value;

  finally
    UnlockWRite;
  end;
end;


{ TTableRelation }

function TTableRelation.GetCheckSum: integer;
begin
  LockRead;
  try
    result := 0;
    inc(result, GetCheckSumForVariant(self.FMasterTable));
    inc(result, GetCheckSumForVariant(self.FDetailTable));
    inc(result, GetCheckSumForVariant(self.FMasterField));
    inc(result, GetCheckSumForVariant(self.FDetailField));
    inc(result, GetCheckSumForVariant(self.FJoinName));
    inc(result, GetCheckSumForVariant(self.Publish));
  finally
    Unlockread;
  end;

end;

function TTableRelation.GetDetailField: string;
begin
  LockRead;
  try
    result := FDetailField;
  finally
    UnlockRead;
  end;
end;


function TTableRelation.GetDetailTAble: string;
begin
  LockRead;
  try
    result := FDetailTable;
  finally
    UnlockRead;
  end;

end;

function TTableRelation.GetJoinName: string;
begin
  LockRead;
  try
    result := FJoinName;
  finally
    UnlockRead;
  end;
end;

function TTableRelation.GetMasterField: string;
begin
  LockRead;
  try
    result := FMasterField;
  finally
    UnlockRead;
  end;
end;

function TTableRelation.GetMasterTable: string;
begin
  Lockread;
  try
    result:= FMasterTable;
  finally
    UnlockRead;
  end;
end;

function TTableRelation.GetPublish: boolean;
begin
  LockREad;
  try
    result := FPublish;
  finally
    UnlockRead;
  end;
end;

procedure TTableRelation.SetDetailField(const Value: string);
begin
  LockWrite;
  try
    FDetailField := value;
  finally
    Unlockwrite;
  end;
end;

procedure TTableRelation.SetDetailTable(const Value: string);
begin
  LockWrite; try
    FDetailTable := value;
  finally
    UnlockWRite;
  end;


end;

procedure TTableRelation.SetJoinName(const Value: string);
begin
  LockWrite;
  try
    FJoinName := value
  finally
    Unlockwrite
  end;

end;

procedure TTableRelation.SetMasterField(const Value: string);
begin
  LockWrite;
  try
    FMasterField := value;
  finally
    UnlockWrite;
  end;
end;

procedure TTableRelation.SetMasterTable(const Value: string);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TTableRelation.SetPublish(const Value: boolean);
begin
  LockWrite;
  try
    FPublish := value;
  finally
    UnlockWrite;
  end;
end;



function TDatabaseDictionary.LoadFromDatabase(bNativeMode: boolean; cb: TCompatibleQueryCallback):boolean;
var
  ds1, ds2,ds3: TSERowSet;
  y,t: integer;
  sDebugReport: string;
  h: TStorageEngineInfo;
  td: TTableDefinition;
  tr: TTableRelation;
  s: string;
begin
  result := false;
  try
    self.lockWrite;
    try
      cb('show tables', ds1);
      try
        for y := 0 to ds1.RowCount-1 do begin
          ds2 := nil;
          try
            s := ds1.values[0,y];

            if bNativeMode or (uppercase(copy(s,1,4)) = 'SE__') then begin
              cb('show create table '+ds1.values[0,y], ds2);
              td := AddTableFromShowCreateTable(ds2.values[1,0]);

              //check if native table
              if (uppercase(copy(s,1,4)) <> 'SE__') then
                td.Native := true;

              //TODO 1: SCAN Table relations
              try
                ds3 := nil;
                try
                  cb('select * from SE_RELATION where MASTER_TABLE="'+td.Name+'"', ds3);
                  if (ds3 <> nil) then begin
                    ds3.First;
                    for t:= 0 to ds3.RowCount-1 do begin
                      tr := td.AddRelation(ds3.CurRecordFields['DETAIL_TABLE'], ds3.CurRecordFields['JOIN_NAME']);
                      tr.MasterTable := ds3.CurRecordFields['MASTER_TABLE'];
                      tr.MasterField := ds3.CurRecordFields['MASTER_FIELD'];
                      tr.DetailField := ds3.CurRecordFields['DETAIL_FIELD'];
                      tr.JoinName := ds3.CurRecordFields['JOIN_NAME'];
                      tr.DetailTable := ds3.CurRecordFields['DETAIL_TABLE'];
                      tr.Publish := ds3.CurRecordFields['PUBLISHED']=1;
                      ds3.next;
                    end;
                  end;
                finally
                  ds3.free;
                  ds3 := nil;
                end;

                //stored table properties in SE_TABLE
                ds3 := nil;
                try
                  cb('select * from SE_TABLE where name="'+td.Name+'"', ds3);
                  if (ds3 <> nil) then begin
                    if ds3.RowCount > 0 then begin
                      ds3.First;
                      td.FullyPromoted := ds3.CurRecordFields['FullyPromoted'];
                    end;
                  end;
                finally
                  ds3.free;
                  ds3 := nil;
                end;
              except

              end;

            end;
          finally
            ds2.free;
            ds2 := nil;
          end;

        end;
      finally
        ds1.free;
        ds1 := nil;
      end;

      if not bNativeMode then begin
        cb('select * from se_host', ds1);
        try
          ClearHosts;
          for y := 0 to ds1.RowCount-1 do begin
            h := AddHost;
            h.ID := ds1.values[0,y];
            h.SE := ds1.values[2,y];
            h.DB := ds1.values[3,y];
            h.Endpoint := ds1.values[4,y];
            h.SystemPower := ds1.values[5,y];
            h.Context := HexToString(ds1.values[6,y]);
            h.LoadConfig(self.Context, 'engine'+inttostr(h.ID));

          end;
        finally
          ds1.free;
        end;
      end else begin
        self.AssumeNaturalRelations;
      end;




    finally
      unlockwrite;
    end;

    sDebugReport := GetStructureReport;
  finally
    //dlocal.free;
  end;
end;


initialization

DMAN_Client := TDatabaseDictionaryManager.create;
DMAN_Coord := TDatabaseDictionaryManager.create;
DMAN_SE := TDatabaseDictionaryManager.create;
//DMAN.Head := TDatabaseDictionary.create;
//DMAN.Head.LoadHostsFromFile();

finalization
DMAN_Client.free;
DMAN_Coord.free;
DMAN_SE.free;



end.
