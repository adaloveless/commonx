unit AsyncObjects;

interface
uses
  DataObject, DataObjectCommonDefinitions, dataObjectServices, sysutils, stringx;



type
  TdoAsyncProcessList = class (TDataObject)
  public
    procedure Sort(iSortIndex : integer); override;
  end;

  TdoSystemAsyncProcessList = class (TDataObject);

  TdoAsyncProcessData = class(TDataObject)
  public
    constructor create(AOwner: TObject; params: variant; Cache: TObject; Extended: TExtendedDOVars); override;
  end;


  TdoAsyncProcess = class (TDataObject)
  public
    constructor create(AOwner: TObject; params: variant; Cache: TObject; Extended: TExtendedDOVars); override;
    function DoNew(iSessionID: integer): boolean; override;
  end;

  TdoErrorMessage = class(TDataObject)
  public
    constructor create(AOwner: TObject; params: variant; ACache: TObject; Extended: TExtendedDOVars); override;
  end;




implementation

uses ServerInterfaceInterface, DataObjectCache;

{Compare functions used for quick sorting}
//------------------------------------------------------------------------------
function CompareAsyncStartDate(item1, item2: Pointer): integer;
var
  s1, s2 : string;
begin
  s1 := LowerCase(TDataObject(item1)['Start'].AsString);
  s2 := LowerCase(TDataObject(item2)['Start'].AsString);
//  result := SortStringCompare(s1,s2);
  result := SortStringCompare(s2,s1);
end;

{ TdoAsyncProcessList }
procedure TdoAsyncProcessList.Sort(iSortIndex: integer);
begin
  QuickSort(@CompareAsyncStartDate);
end;


{ TdoAsyncProcess }

constructor TdoAsyncProcess.create(AOwner: TObject; params: variant;
  Cache: TObject; Extended: TExtendedDOVars);
begin
  inherited;
  FieldCapacity := 7;
  AddFieldDef('Name', TStringDAtaField, '');
  AddFieldDef('State', TLongintDatafield, 0); //<0 = error;  0= finished; >0 = processing
  AddFieldDef('StepNumber', TLongintDatafield, 0);
  AddFieldDef('TotalSteps', TLongintDatafield, 0);
  AddFieldDef('Start', TMYSQLDateTimeDatafield, TDateTime(NULL_DATETIME));
  AddFieldDef('End', TMYSQLDateTimeDatafield, TDateTime(NULL_DATETIME));
  AddFieldDef('LastChange', TMYSQLDateTimeDatafield, TDateTime(NULL_DATETIME));

end;

function TdoAsyncProcess.DoNew(iSessionID: integer): boolean;
begin
  token.params[0] := IServerInterface(TDataObjectCache(self.cache).server).GetNextID('async_process');
  result := true;  
end;

{ TdoAsyncProcessData }

constructor TdoAsyncProcessData.create(AOwner: TObject; params: variant;
  Cache: TObject; Extended: TExtendedDOVars);
begin
  inherited;
  FieldCapacity := 1;
  AddFieldDef('ProcessData', TStringDAtaField, '');
end;


constructor TdoErrorMessage.create(AOwner: TObject; params: variant; ACache: TObject; Extended: TExtendedDOVars);
begin
  //Keys
  //  ProcessID-long
  //  MessageID-long

  inherited;
  FieldCapacity := 1;
  AddFieldDef('ErrorMessage', TStringDAtaField, '');
  AddFieldDef('DateTime', TDateTimeDataField, '');

end;


end.
