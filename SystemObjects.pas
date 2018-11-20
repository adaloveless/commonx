unit SystemObjects;

interface

uses
  DataObject, sysutils, Dataobjectservices, Dataobjectcache, serverinterfaceinterface, variants, DODTInterface, DataObjectCommonDefinitions;

const
    CKEY_NEW_SESSION = 100;

type
  Tdorole = class(TDataObject)
  public
    constructor create(AOwner: Tobject; params: variant; ACache: TObject; extended: TExtendedDOVars); override;
    function DoNew(iSessionID: integer): boolean; override;
  end;

  TdoSession = class(TDataObject)
  public
    constructor create(AOwner: TObject;params: variant; aCache: TObject; extended: TExtendedDOVars);override;
    function DoGetAssociate(sName: string): TDataObject;override;
    function DoNew(iSessionID: integer): boolean; override;

  end;


  TdoSessionVars = class(TDataObject);
  TdoSessionVar = class(TDataObject)
  public
    constructor create(AOwner: Tobject; params: variant; ACache: TObject; extended: TExtendedDOVars); override;
  end;

  TdoUserVars = class(TDataObject);
  TdoUserVar = class(TDataObject)
  public
    constructor create(AOwner: Tobject; params: variant; ACache: TObject; extended: TExtendedDOVars); override;
  end;


  TdoMail = class(TDataObject)
  public
    function DoNew(iSessionID: integer): boolean; override;
    constructor create(AOwner: Tobject; params: variant; ACache: TObject; extended: TExtendedDOVars); override;
  end;

  TdoUser = class(TDataObject)
  public
    constructor create(AOwner: Tobject; params: variant; ACache: TObject; extended: TExtendedDOVars); override;
    function DoNew(iSessionID: integer): boolean; override;
    procedure BeforeReadfield(sfieldName: string);override;
    procedure AfterWriteField(field: TDatafield);override;
    procedure Sort(i: integer);override;
  end;





implementation

{ Tdorole }

constructor Tdorole.create(AOwner: Tobject; params: variant;
  ACache: TObject; extended: TExtendedDOVars);
begin
  inherited;
  self.AddFieldDef('Name', TstringDatafield, '');
  self.AddFieldDef('f1', TLongintDataField, 0);
  self.AddFieldDef('f2', TLongintDataField, 0);
  self.AddFieldDef('f3', TLongintDataField, 0);
  self.AddFieldDef('f4', TLongintDataField, 0);
  self.AddFieldDef('f5', TLongintDataField, 0);
  self.AddFieldDef('f6', TLongintDataField, 0);
  self.AddFieldDef('f7', TLongintDataField, 0);
  self.AddFieldDef('f8', TLongintDataField, 0);


end;

function Tdorole.DoNew(iSessionID: integer): boolean;
begin
  result := true;
end;

{ TdoSession }

constructor TdoSession.create(AOwner: TObject; params: variant;
  aCache: TObject; extended: TExtendedDOVars);
begin
  inherited;
  FieldCapacity := 2;
  AddFieldDef('UserID', TLongIntDatafield, 0);
  AddFieldDef('IdleTime', TLongintDatafield, 0);
  AddFieldDef('Bookmark', TstringDataField, '');
  AddFieldDef('LastHit', TMySqlDateTimeDAtaField, now());
  AddAssociate('user');

end;

function TdoSession.DoGetAssociate(sName: string): TDataObject;
begin
  Pointer(result) := nil;
  result := inherited DoGetAssociate(sName);

  if lowercase(sName) = 'user' then begin
    //field params: DemogID
    if not IServerInterface(TDataObjectCache(self.cache).server).LazyQueryMap(TDataObjectCache(cache), result, 'SELECT * from user where userid='+self['userid'].AsString, self.SessionID, 300000,'TdoUser', self['userid'].ASVariant, nil,'',0) then begin
      raise exception.create('Error getting '+sName);
    end;
  end;


end;

function TdoSession.DoNew(iSessionID: integer): boolean;
begin
  token.params[0] :=   IServerInterface(TDataObjectCache(self.cache).server).GetNextID(CKEY_NEW_SESSION, iSessionID);
  result := true;
end;

{ TdoSessionVar }

constructor TdoSessionVar.create(AOwner: Tobject; params: variant;
  ACache: TObject; extended: TExtendedDOVars);
begin
  inherited;
  FieldCapacity := 2;
  AddFieldDef('VarName', TstringDataField, '');
  AddFieldDef('VarValue', TstringDataField, '');

end;

{ TdoUserVar }

constructor TdoUserVar.create(AOwner: Tobject; params: variant;
  ACache: TObject; extended: TExtendedDOVars);
begin
  inherited;
  FieldCapacity := 2;
  AddFieldDef('VarName', TstringDataField, '');
  AddFieldDef('VarValue', TstringDataField, '');

end;


procedure TdoUser.AfterWriteField(field: TDatafield);
begin
  inherited;

  if pos('phone', lowercase(field.Name)) > 0 then begin
    field.AsString := field.AsString;
  end;

end;

procedure TdoUser.BeforeReadfield(sfieldName: string);
var
  sLC: string;
  sQuery: string;
  sType: string;
  vBaseKeys: variant;
  obj: TDataObject;
begin
  inherited;

  sLC := lowercase(sFieldName);

  IF sLC = 'fullname' then begin
    self[sfieldName].AsString := self['lastname'].AsString + ', '+ self['firstname'].AsString;
  end else
  IF sLC = 'age' then begin
    self[sfieldName].AsVariant := (now()-self['birthdate'].AsVariant)/365.25
  end else
  IF sLC = 'rolename' then begin
    vBaseKeys := self['roleid'].asVariant;
    sQuery := 'select * from role where roleid='+vartostr(vBaseKeys);
    sTYpe := 'TdoRole';
    MyServer(self).GhostQueryMap(TDataObjectCache(self.cache), obj, sQuery, self.SessionID, 30000, sType, vBaseKeys, nil, '', 0);
    self[sfieldName].AsString := obj['name'].AsString;
  end;


end;

constructor TdoUser.create(AOwner: Tobject; params: variant;
  ACache: TObject; extended: TExtendedDOVars);
begin
  inherited;
  AddFieldDef('FirstName', TStringDataField, '');
  AddFieldDef('LastName', TStringDataField, '');
  AddFieldDef('Phone', TStringDataField, '');
  AddFieldDef('Email', TStringDataField, '');
  AddFieldDef('State', TStringDataField, '');
  AddFieldDef('Zip', TStringDataField, '');
  AddFieldDef('Password', TStringDataField, '');
  AddFieldDef('RoleID', TLongintDataField, '');
  AddFieldDef('Title', TStringDataField, '');
  AddFieldDef('SponsorPassword', TStringDataField, '');
  AddFieldDef('Gender', TStringDataField, '');
  AddFieldDef('PIN', TStringDataField, '');
  AddFieldDef('DistributorID', TLongintDataField, 0);
  AddFieldDef('BirthDate', TMYSQLDateTimeDataField, 0.0);
  AddCalculatedField('FullName', TStringDataField);
  AddCalculatedField('RoleName', TStringDataField);
  AddCalculatedField('FitnessLevel', TFloatingPointDatafield);
  AddCalculatedField('Age', TFloatingPointDatafield);


end;

function TdoUser.DoNew(iSessionID: integer): boolean;
begin
  token.params[0] := IServerInterface(TDataObjectCache(self.cache).server).GetNextID(CKEY_NEW_USER, iSessionID);
  result := true;

end;

  function SortFL(Item1, Item2: Pointer): Integer;
  var
    o1,o2: TDataObject;
    r1,r2: real;
  begin
    o1 := TDataObject(item1);
    o2 := TDataObject(item2);
    r1 := o1['Fitnesslevel'].AsVariant;
    r2 := o2['Fitnesslevel'].AsVariant;

    result := 0;
    if r1>r2 then result := -1;
    if r1<r2 then result := 1;


  end;


function SortPercentTasksComplete(Item1, Item2: Pointer): Integer;
var
  o1,o2: TDataObject;
  r1,r2: real;
begin
  o1 := TDataObject(item1);
  o2 := TDataObject(item2);
  r1 := o1['TasksPercent'].AsVariant;
  r2 := o2['TasksPercent'].AsVariant;

  result := 0;
  if r1>r2 then result := -1;
  if r1<r2 then result := 1;


end;

function SortPercentSetsComplete(Item1, Item2: Pointer): Integer;
var
  o1,o2: TDataObject;
  r1,r2: real;
begin
  o1 := TDataObject(item1);
  o2 := TDataObject(item2);
  r1 := o1['SetsPercent'].AsVariant;
  r2 := o2['SetsPercent'].AsVariant;

  result := 0;
  if r1>r2 then result := -1;
  if r1<r2 then result := 1;


end;


procedure TdoUser.Sort(i: integer);
begin
  inherited;
  self.QuickSort(SortFL);
end;

constructor TdoMail.create(AOwner: Tobject; params: variant;
  ACache: TObject; extended: TExtendedDOVars);
begin
  inherited;

  FieldCapacity := 7;
  AddFieldDef('FromID', TLongintDataFIeld, 0);
  AddFieldDef('ToID', TLongintDataFIeld, 0);
  AddFieldDef('FromName', TSTringDataField, '');
  AddFieldDef('Subject', TStringDataField, '');
  AddFieldDef('Body', TStringDataField, '');
  AddFieldDef('Errors', TStringDataField, '');
  AddFieldDef('State', TStringDataField, '');
  AddFieldDef('ToName', TStringDAtaField, '');
  AddFieldDef('TimeStamp', TMYSQLDateTimeDataField, now);
  AddFieldDef('IsREad', TBooleanDataField, false);


end;

function TdoMail.DoNew(iSessionID: integer): boolean;
begin
  token.params[0] := IServerInterface(TDataObjectCache(self.cache).server).GetNextID(CKEY_NEW_MAIL, iSessionID);
  result := true;

end;


end.
