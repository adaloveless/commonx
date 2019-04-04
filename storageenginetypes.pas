unit StorageEngineTypes;
//DONE: add mysql type for text
//DONE 1: Allow TSERowset to navigate using alternate indexes
//TODO 2: Return boolleans from database properly
//TODO 1: NULL keyword doesn't work
//TODO 1: Implement connection retries to SQL server.
interface

uses
  debug, betterobject, sqlexpr, sysutils, DB, classes, stringx, systemx, variants, typex, MultiBufferMemoryFileStream, helpers.stream, numbers, btree, JSONHelpers, mysqlstoragestring;

const
  SYSTEM_FIELD_COUNT = 1;

type
  TSERowSet = class;//forward

  TAnonRecFilter = reference to procedure (rs: TSERowSet; out accept: boolean; var bbreak: boolean);

  TSECell = variant;
  TSERow = array of TSECell;

  Tbti_Row = class(TBTreeItem)
  protected
    row: TSERow;
    indexfieldIndexes: TArray<nativeint>;
    desc: TArray<Boolean>;
    rownumber: ni;
    function Compare(const [unsafe] ACompareTo: TBTreeItem): NativeInt; override;
  end;


  TQueryList = class(TStringList)
  private
    Fidx: integer;
    function GetEOF: boolean;
  protected
  public
    procedure First;
    property EOF: boolean read GetEOF;
    procedure AddQuery(s: string);
    function GetNextQuery: string;
  end;

  TSERowsetFieldDef = record
    sName: string;
    vType: TFieldType;
  end;

  PSERowsetFieldDef = ^TSERowSetFieldDef;

  TIndexPair = packed record
    value: int64;
    addr: int64;
  end;

  TSEIndexFile = class(TbetterObject)
  private
    function Getitem(idx: int64): TIndexPair;
    function GetCount: int64;
  protected
    fs: TMultiBufferMemoryFileStream;
  public
    procedure Detach;override;
    procedure Close;
    procedure Open(sFile: string; bForWriting: boolean);
    property Items[idx: int64]: TIndexPair read Getitem;
    function AddrOf(val: int64): int64;
    property Count: int64 read GetCount;
  end;




  TSERowSet = class(TBetterObject)
  private
    FBoundToTable: string;
    procedure SetCurRecordFields(sFieldName: string; const Value: variant);
    function GEtFieldDefs(idx: integer): PSERowsetFieldDef;
    procedure SetCurRecordFieldsByIdx(idx: integer; const Value: variant);
    function GetCurRecordFieldsByIdx(idx: integer): variant;
    function GetCurRecordFields(sFieldName: string): variant;
    function GetEOF: boolean;
    function GEtFieldCount: integer;
    function GEtRowCount: integer;
    function GetFieldDef(idx: integer): PSERowsetFieldDef;
    function GetValues(x, y: integer): variant;
    procedure SetValues(x, y: integer; const Value: variant);
  protected
    FRowset: array of TSERow;
    FFieldDefs: array of TSERowsetFieldDef;
    FCursor: integer;
    FIndexValues: array of array of int64;
    FIndexNames: array of string;
    FIndex: integer; //<<--This is the index used to nagivate the rowset via first/next etc.
    FMaintain: TStringlist;
    idxs: array of TMultiBufferMemoryFileStream;
    procedure UpdateRowWidths(iOldCount: integer);
    function GetfieldValue(iRow: nativeint; sFieldName: string): variant;
  public

    constructor Create;override;
    property fields[idx: integer]: PSERowsetFieldDef read GetFieldDef;
    function AddField:PSeRowSetFieldDef;

    function IndexOfField(sName: string): integer;
    function FindValue(sField: string; vValue: variant): ni;
    function Lookup(sLookupField: string; vLookupValue: variant; sReturnField: string): variant;


    procedure SetFieldCount(iCount: integer);
    procedure SetRowCount(iCount: integer);

    property Values[x,y: integer]: variant read GetValues write SetValues;
    property CurRecordFields[sFieldName: string]: variant read GetCurRecordFields write SetCurRecordFields;default;
    property f[sFieldName: string]: variant read GetCurRecordFields write SetCurRecordFields;
    property CurRecordFieldsByIdx[idx: integer]: variant read GetCurRecordFieldsByIdx write SetCurRecordFieldsByIdx;

    procedure CopyFromDAtaSet(ds: TCustomSQLDataset; bAppend: boolean = false);
    function AddRow: integer;
    function AddRowFirst: integer;
    procedure CancelRow;

    property FieldDefs[idx: integer]: PSERowsetFieldDef read GEtFieldDefs;
    property FieldValues[iRow: nativeint; sName: string]: variant read GetfieldValue;

    property BoundToTable: string read FBoundToTable write FBoundToTable;
    property RowCount: integer read GEtRowCount;
    property FieldCount: integer read GEtFieldCount;

    procedure First;
    procedure LAst;
    procedure Next;
    procedure Previous;
    property EOF:boolean read GetEOF;

    function AddBlankIndex(sName: string): integer;
    procedure BuildIndex(sName: string; sFields: string);overload;
    procedure BuildIndex_BUBBLE(sName: string; field_priorities: TArray<nativeint>; desc: TArray<boolean>);overload;
    procedure BuildIndex_BTREE(sName: string; field_priorities: TArray<nativeint>; desc: TArray<boolean>);overload;
    procedure BuildIndex(sName: string; field_priorities: TArray<nativeint>; desc: TArray<boolean>);overload;

    function GetIndexOfIndex(sName: string): integer;
    function IndexCount: integer;

    procedure SetIndex(idx: integer);
    function GetIndexedRow(iOrderIdx: integer): integer;
    property Cursor: integer read FCursor write FCursor;
    function ToString: string;override;
    procedure Append(rs: TSERowSet);overload;
    procedure Append(rs: IHolder<TSERowSet>);overload;
    procedure AppendRowFrom(rs: TSERowSet; iRow: ni);
    procedure PrePend(rs: TSERowSet);

    procedure SavetoFile(f: string; bAppend: boolean = false);
    procedure LoadFromfile(f: string; filterfunc: TAnonRecFilter; startAtIndex: string = ''; startAtIndexValue: int64 = 0);
    procedure LoadfromCSV(f: string);
    procedure SavetoCSV(f: string);
    procedure AddrowFromString(s: string);
    function RowToString: string;
    function GetHeaderString: string;
    procedure DefineFromHeaderString(s: string);
    procedure Maintain(sField: string);
    procedure WriteToIndex(sBaseFile: string; bForAppend: boolean; iStartAddr: int64);
    procedure OpenIndexes(sBaseFile: string; bForRead, bForAppend: boolean);
    procedure CloseIndexes;
    function FindSeekInIndex(sBaseFile, sIndex: string; value: int64): int64;
    procedure CopyFieldDefsTo(rs: TSERowSet);
    procedure CopyCurrentRecordTo(rs: TSERowSet);
    procedure CopyRecordFromByName(rs: TSERowSet);
    procedure AddDeltaColumn(sName: string; sSource: string; bSourceIsReverseTime: boolean);
    procedure FromCSV(csv: string; separator: string = ',');
    function ToJSONh: IHolder<TJSON>;
    procedure Clear;
    function ToMYSQLCreateTable(sTableName: string): string;
    function ToMYSQLImport(sTableName: string): string;
    function RowToMYSQLValues(r: TSERow): string;
    function ToMYSQLValues: string;
  end;



  TSERowSetArray = array of TSERowset;



function FieldValuetoString(v: variant; vType: TFieldType): string;
function StringToFieldValue(s: string; fv: TFieldType): variant;
function MysqlTypeToFieldType(sMySQLType: string): TFieldType;
function FieldTypeToMYSQLType(ft: TFieldType; iSize: integer=0): string;
procedure FreeArrayOfRowsets(a: array of TSERowSet);

function DatasetToString(ds: TCustomSQLDAtaSet; destroyit: boolean = false): string;
function RowsetToString(ds: TSERowSet; destroyit: boolean = false): string;
function RowsetToValues(ds: TSERowset; bIncludeSEFields: boolean): string;
function RowsetToSETStatement(ds: TSERowset): string;
function stringToFieldType(s: string): TFieldType;
function DatasetToRowset(var ds: TCustomSQLDAtaset; bDestroy: boolean = true): TSERowSet;


implementation

function DatasetToRowset(var ds: TCustomSQLDAtaset; bDestroy: boolean = true): TSERowSet;
begin
  result := nil;
  try
    result := TSERowset.create;
    result.CopyFromDAtaSet(ds);

  finally
    if bDestroy then begin
      ds.free;
      ds := nil;
    end;
  end;


end;

procedure FreeArrayOfRowsets(a: array of TSERowSet);
var
  t: integer;
begin
  for t:= low(a) to high(a) do begin
    a[t].free;
    a[t] := nil;
  end;

end;

function FieldTypeToMYSQLType(ft: TFieldType; iSize: integer): string;
begin
  //list of possible types
  //float=real
  //int=integer
  //tinyint=boolean
  //bigint=int64
  //timestamp=TDateTime
  //varchar=string
  //char=string
  //text=string
  case ft of
    ftFloat: result := 'float';
    ftInteger: result := 'int';
    ftLargeInt: result := 'bigint';
    ftString: result := 'varchar('+inttostr(iSize)+')';
    ftBoolean: result := 'bool';
    ftDateTime: result := 'timestamp';
  else
    result := 'UNKNOWN TYPE';
  end
end;

function stringToFieldType(s: string): TFieldType;
begin
  s := lowercase(s);
  if s = 'integer' then begin
    result := ftInteger;
  end else
  if s = 'varchar' then begin
    result := ftString;
  end else
  if s = 'string' then begin
    result := ftString;
  end else
  if s = 'datetime' then begin
    result := ftDateTime;
  end else
  if s = 'boolean' then begin
    result := ftBoolean;
  end else
  if s = 'float' then begin
    result := ftFloat
  end else
  if s = 'bigint' then begin
    result := ftLargeInt;
  end else
  begin
    raise Exception.create('unimplemented stringToFieldType ('+s+') in unit StorageEngineTypes');
  end;

end;


function MysqlTypeToFieldType(sMySQLType: string): TFieldType;
var
  sLeft, sRight: string;
begin
  sMYSQLType := lowercase(TrimStr(sMysqlType));

  //list of possible types
  //float=real
  //int=integer
  //tinyint=boolean
  //bigint=int64
  //timestamp=TDateTime
  //varchar=string
  //char=string
  //text=string

  SplitString(sMySQLType, '(', sMySQLType, sRight);
  result := ftUnknown;
  if sMYSQLType = 'float' then
    result := ftFloat
  else
  if sMYSQLType = 'bigint' then
    result := ftLargeInt
  else
  if sMYSQLType = 'int' then
    result := ftInteger
  else
  if sMYSQLType = 'bigfloat' then
    result := ftLargeInt
  else
  if sMYSQLType = 'timestamp' then
    result := ftDateTime
  else
  if sMYSQLType = 'tinyint' then
    result := ftBoolean
  else
  if sMYSQLType = 'bool' then
    result := ftBoolean
  else
  if sMYSQLType = 'varchar' then
    result := ftString
  else
  if sMYSQLType = 'text' then
    result := ftString
  else
  if sMYSQLType = 'char' then
    result := ftString;












end;
{ TSERow }

{ TSERowSet }

procedure TSERowSet.AddDeltaColumn(sName, sSource: string; bSourceIsReverseTime: boolean);
var
  def: PSERowsetFieldDef;
  t: ni;
  i, iSource: ni;
begin
  def := self.AddField;
  def.sName := sNAme;
  def.vType := TFieldType.ftFloat;
  i := fieldcount;
  isource := IndexOfField(sSource);
  if iSource < 0 then
    raise ECritical.create('Rowset does not have field '+sSource+'. cannot compute delta column.');
  SetFieldCount(FieldCount+1);
  if bSourceIsReverseTime then begin
    for t:=0 to rowcount-2 do begin

      self.Values[i, t] := self.Values[i, t] - self.Values[i, t+1];
    end;
    self.Values[i, rowcount-1] := 0.0;
  end else begin
    self.Values[i, 0] := 0.0;
    for t:=1 to rowcount-1 do begin
      self.Values[i, t] := self.Values[i, t] - self.Values[i, t-1];
    end;

  end;



end;

function TSERowSet.AddBlankIndex(sName: string): integer;
//r: Returns index of index added
var
  i: integer;
  t: integer;
begin
  i := IndexCount+1;
  setlength(self.FIndexNames, i);
  setlength(self.FIndexValues, i);
  setlength(self.FIndexValues[i-1], RowCount);

  dec(i);
  //put unique values in each
  for t:= low(FIndexValues[i]) to high(FIndexValues[i]) do begin
    FIndexValues[i][t] := t;
  end;

  result := high(FIndexNames);


end;

function TSERowSet.AddField: PSeRowSetFieldDef;
var
  t: integer;
begin
  SetLength(FFieldDefs, length(FFieldDefs)+1);
  result := @FFieldDefs[high(FFieldDefs)];
  result.sName := '';
  result.vType := ftUnknown;
  if RowCount > 0 then begin
    for t:= 0 to RowCount-1 do begin
      SetLength(self.FRowset[t], length(FFieldDEfs));
    end;
  end;
end;

function TSERowSet.AddRow: integer;
begin
  self.SetRowCount(length(FRowset)+1);
  result := length(FRowset)-1;
  FCursor := result;
end;

function TSERowSet.AddRowFirst: integer;
var
  t,u: ni;
begin
  self.SetRowCount(length(FRowset)+1);
  result := 0;

  for t:= high(FRowset) downto 1 do begin
    for u := 0 to fieldcount-1 do begin
      self.Values[u,t] := self.values[u,t-1];
    end;
  end;
  FCursor := result;
end;

procedure TSERowSet.AddrowFromString(s: string);
var
  sl : Tstringlist;
  s1, s2, s3: string;
  t: ni;
  v: variant;
begin
  AddRow;
  sl := nil;
  try
    sl := ParseString(s,#1);
    for t:= 1 to sl.count-1 do begin
      s1 := sl[t];
      v := stringToFieldValue(s1, Self.FieldDefs[t-1].vType);
      CurRecordFieldsByIdx[t-1] := v;
    end;
  finally
    sl.free;
  end;
end;

procedure TSERowSet.Append(rs: TSERowSet);
var
  t: ni;
begin
  if rowcount = 0 then begin
    rs.CopyFieldDefsTo(self);
  end;
  rs.first;
  while not rs.EOF do begin
    AddRow;
    for t:= 0 to fieldcount-1 do begin
      CurRecordFieldsByIdx[t] := rs.CurRecordFieldsByIdx[t];
    end;
//    Debug.Log('Append #'+inttostr(rs.cursor)+': '+rs.RowToString);
    rs.Next;
  end;
end;

procedure TSERowSet.Append(rs: IHolder<TSERowSet>);
begin
  Append(rs.o);
end;

procedure TSERowSet.AppendRowFrom(rs: TSERowSet; iRow: ni);
var
  t: ni;
begin
  if rowcount = 0 then begin
    rs.CopyFieldDefsTo(self);
  end;
  AddRow;
  for t:= 0 to fieldcount-1 do begin
    values[t,cursor] := rs.values[t,iRow];
  end;
end;

procedure TSERowSet.BuildIndex(sName, sFields: string);
var
  field_priorities: TArray<nativeint>;
  desc: TArray<boolean>;
  sl : TStringlist;
  t: integer;
  iTemp: integer;
  sField: string;
  thisDesc: boolean;
begin
  sl := nil;
  try
    sl := TStringlist.create;
    sl.text := stringreplace(sFIelds, ',', #13#10, [rfReplaceAll]);

    //TODO 2: Support multilevel sorting with multiple fields
    SetLength(field_priorities, sl.count);
    SetLength(desc, sl.count);
    for t:= 0 to sl.count-1 do begin
      thisdesc := false;
      sField := sl[t];
      if zcopy(sField, 0,1) = '-' then begin
        thisdesc := true;
        sField := zcopy(sField, 1,length(sField)-1);
      end;
      iTemp := self.IndexOfField(sField);
      if iTemp < 0 then
        raise exception('Field '+sField+' could not be found when indexing');

      field_priorities[t] := iTEmp;
      desc[t] := thisdesc;
    end;

    BuildIndex(sName, field_priorities, desc);
  finally
    sl.free;
  end;
end;

procedure TSERowSet.BuildIndex(sName: string; field_priorities: TArray<nativeint>; desc: TArray<boolean>);
begin
{$IFDEF USE_BUBBLE}
  BuildIndex_BUBBLE(sName, field_priorities, desc);
{$ELSE}
  BuildIndex_BTREE(sName, field_priorities, desc);
{$ENDIF}

end;


procedure TSERowSet.BuildIndex_BTREE(sName: string;
  field_priorities: TArray<nativeint>; desc: TArray<boolean>);
var
  nu: Tbti_Row;
  bt: TBTree;
  t: ni;
  idx: ni;
  order: ni;
begin
  bt := TBtree.Create;
  try
    idx := AddBlankIndex(sName);
    for t:= 0 to rowcount-1 do begin
      nu := TBti_Row.create;
      nu.indexfieldIndexes := field_priorities;
      nu.row := Frowset[t];
      nu.rownumber := t;
      nu.desc := desc;
      bt.Add(nu);
    end;
    order := 0;
    bt.Iterate(
      procedure([unsafe] ABTreeItem:TBTreeItem)
      begin
        self.FIndexValues[idx][order] := Tbti_Row(ABtreeItem).rownumber;
        inc(order);
      end
    );

    SetIndex(idx);
  finally
    bt.free;
  end;

end;

procedure TSERowSet.BuildIndex_BUBBLE(sName: string;
  field_priorities: TArray<nativeint>; desc: TArray<boolean>);
var
  idx: integer;
  iTemp: integer;
  bSorted: boolean;
  t, f: integer;
  i1,i2: integer;
  iRowCount: integer;
  fLow, fHigh: integer;
  v1,v2: variant;

begin
  self.SetIndex(-1);
  if length(field_priorities) = 0 then
    exit;

  iRowCount := RowCount;
  if iRowCount = 0 then
    exit;

  idx := self.AddBlankIndex(sName);

  fLow := low(field_priorities);
  fhigh := high(field_priorities);

  //DONE 1: Build Quick Sort
  bSorted := false;

  while not bSorted do begin
    bSorted := true;

    //if we pass through this round and make no changes... then we're done
//      bSorted := true;

    for t := 0 to RowCount-2 do begin
      //gather indexes of rows to compare
      i1 := t; //todo 3: Rename i1 i2 to something that reflects that they are ROWS
      i2 := t+1;

        //if index hits out of bounds then break
      if (i1 >= iRowCount) or (i2 >=iRowCount) then
        break;

        //compare fields
      for f := fLow to fHigh do begin
        //gather values from rows and fields to compare
        v1 := self.Values[field_priorities[f], i1];
        v2 := self.Values[field_priorities[f], i2];
        //if values are out of order
        if desc[f] then begin
          if (v1<v2) then begin
            //swap the values
            numbers.Swap(self.FIndexValues[idx][i1],self.FIndexValues[idx][i2]);
            //flag that this changes were made...
            //so stuff is potentially not sorted
            bSorted := false;
            break;
          end;
        end else begin
          if (v1>v2) then begin
            //swap the values
            numbers.Swap(self.FIndexValues[idx][i1],self.FIndexValues[idx][i2]);
            //flag that this changes were made...
            //so stuff is potentially not sorted
            bSorted := false;
            break;
          end;
        end;
      end;
    end;
  end;

  //FINALLY SET THE INDEX TO THE ONE WE JUST BUILT;
  self.SetIndex(idx);

end;

procedure TSERowSet.CancelRow;
begin
  dec(FCursor);
  self.SetRowCount(RowCount-1);
end;

procedure TSERowSet.Clear;
begin
  SetLength(FFieldDefs,0);
  setlength(FRowset,0);
end;

procedure TSERowSet.CloseIndexes;
var
  t: ni;
begin
  for t:= 0 to high(idxs) do begin
    idxs[t].free;
  end;
  setlength(idxs,0);

end;

procedure TSERowSet.CopyCurrentRecordTo(rs: TSERowSet);
var
  f: PSERowsetFieldDef;
  t: ni;
begin
  for t := 0 to fieldcount-1 do begin
    rs.CurRecordFieldsByIdx[t] := self.CurRecordFieldsByIdx[t];
  end;
end;

procedure TSERowSet.CopyFieldDefsTo(rs: TSERowSet);
var
  f: PSERowsetFieldDef;
  t: ni;
begin

  for t := 0 to fieldcount-1 do begin
    if rs.IndexOfField(Self.FieldDefs[t].sName) < 0 then begin
      f := rs.AddField;
      f.sName := Self.FieldDefs[t].sName;
      f.vType := Self.FieldDefs[t].vType;
    end;
  end;

end;

procedure TSERowSet.CopyFromDAtaSet(ds: TCustomSQLDataset; bAppend: boolean=false);
var
  t, i,u: integer;
  s: string;
  u8: utf8string;
  c: array of char;
begin
  ds.first;
  i := 0;
  if not bAppend then begin
    self.SetFieldCount(ds.fieldcount);
    for t:= 0 to ds.fieldcount-1 do begin
      self.FFieldDefs[t].sName := ds.FieldDefs[t].Name;
      self.FFieldDefs[t].vType := ds.FieldDefs[t].DataType;
    end;
  end else begin
    if self.FieldCount <> ds.FieldCount then
      raise exception.create('appended dataset contains incorrect number of fields');
  end;

  ds.first;
  while not ds.eof do begin
    self.SetRowCount(i+1);
    for t:= 0 to ds.FieldCount-1 do begin
      if ds.Fields[t].IsBlob then begin
        if ds.FieldDefs[t].DataType = ftMemo then begin
          self.Values[t,i] := ds.fields[t].Value;
        end else begin
          SetLength(s, ds.Fields[t].DataSize);
          SEtLength(c, ds.Fields[t].DAtaSize);

          {$IFDEF MSWINDOWS}
            ds.Fields[t].GetData(@c[0]);
          {$ELSE}
            raise ECritical.create('ds.Fields[t].GetData(@c[0]); is not implemented on this platform.');
          {$ENDIF}
          for u := low(c) to high(c) do begin
            s[u+1] := c[u];
          end;
          self.Values[t,i] := s;
        end;

      end else
      if (vartype(ds.fields[t].Value) = varString)
      or (vartype(ds.fields[t].Value) = varOleStr)
      then begin
        self.Values[t,i] := ds.fields[t].Text;
      end else
      begin
        if ds.FieldDefs[t].DataType in [ftDateTime,ftTimeStamp] then begin
          if vartype(ds.Fields[t].AsVariant) = varNull then
            self.values[t,i] := NULL
          else
            self.Values[t,i] := strtodatetime(ds.Fields[t].AsVariant);
        end
        else
          self.Values[t,i] := ds.Fields[t].AsVariant;
      end;
    end;
    inc(i);
    ds.Next;
  end;
end;

procedure TSERowSet.CopyRecordFromByName(rs: TSERowSet);
var
  t: ni;
begin
  for t := 0 to rs.FieldCount-1 do
    self[rs.FieldDefs[t].sName] := rs.CurRecordFieldsByIdx[t];


end;

constructor TSERowSet.Create;
begin
  inherited;
  FIndex := -1;
  FMainTain := TStringlist.create;
end;

procedure TSERowSet.DefineFromHeaderString(s: string);
var
  sl: TStringlist;
  t: ni;
  fd: PSERowsetFieldDef;
  s1,s2: string;
begin
  sl := nil;
  try
    sl := ParseString(s, ',');
    for t:= 0 to sl.count-1  do begin
      if (t and 1) = 1 then
        continue;

      s1 := sl[t];
      s2 := sl[t+1];
      fd := self.AddField;
      fd.sName := s1;
      fd.vType := TFieldType(strtoint(trim(s2)));

    end;


  finally
    sl.free;
  end;

end;

procedure TSERowSet.First;
begin
  FCursor := 0

end;

procedure TSERowSet.FromCSV(csv, separator: string);
var
  sl: IHolder<TStringlist>;
  t,f: ni;
  slLine: TStringlist;
  pfd: PSERowsetFieldDef;
begin
  sl := stringToStringListh(csv);
  for t:= 0 to sl.o.Count-1 do begin
    slLine := nil;
    try
      slLine := SplitStringIntoStringList(sl.o[t], ',', '"');

      if t = 0 then begin
        for f:= 0 to slLine.count-1 do begin
          pfd := self.AddField;
          pfd.sName := UnQuote(slLine[f]);
          pfd.vType := ftString;
        end;
      end else begin
        AddRow;
        for f:= 0 to slLine.count-1 do begin
          values[f,t-1] := UnQuote(slLine[f]);
        end;
      end;


    finally
      slLine.free;
    end;
  end;





end;

function TSERowSet.GetCurRecordFields(sFieldName: string): variant;
var
  idx: integer;
begin
  idx := self.IndexOfField(sFieldName);

  if idx < 0 then
    raise ECritical.create('Field '+sFieldName+' not found in rowset.');
  result:= values[idx, FCursor];


 end;



function TSERowSet.GetCurRecordFieldsByIdx(idx: integer): variant;
begin
  try
    result := values[idx, FCursor];
  except
    on e:exception do begin
      result := '*BAD*'+e.Message;
    end;
  end;
end;

function TSERowSet.GetEOF: boolean;
begin
  result := (FCURSOR > (self.RowCount-1)) or (RowCount = 0);

end;

function TSERowSet.GEtFieldCount: integer;
begin
  if FFieldDefs = nil then begin
    result := 0;
  end else
    result := length(self.FFieldDefs);



end;

function TSERowSet.GetFieldDef(idx: integer): PSERowsetFieldDef;
begin
  result := @FFieldDefs[idx];
end;

function TSERowSet.GEtFieldDefs(idx: integer): PSERowsetFieldDef;
begin
  result := @self.FFielddefs[idx];
end;

function TSERowSet.GetfieldValue(iRow: nativeint; sFieldName: string): variant;
var
  i: nativeint;
begin
  i := IndexOfField(sFieldNAme);
  if i < 0 then
    raise ECritical.create('field '+sfieldname+' does not exist.');
  result := Values[i,iRow];
end;

function TSERowSet.GetHeaderString: string;
var
  t: ni;
  sLine: string;
begin
  sLine := '';
  for t:= 0 to fieldcount-1 do begin
    if t>0 then
      sLine := sLine + ',';

    sLine := sLine + Self.FieldDefs[t].sName+','+inttostr(ord(self.FieldDefs[t].vType));
  end;

  result := sLine;
end;

function TSERowSet.GetIndexedRow(iOrderIdx: integer): integer;
begin
  if FIndex < 0 then
    result := iOrderIdx
  else
  if FIndex >= RowCount then
    raise Exception.create('Row index out of bounds '+inttostr(iOrderIdx))
  else
    result := FIndexValues[FIndex][iOrderIdx];

end;

function TSERowSet.GetIndexOfIndex(sName: string): integer;
begin

//TODO -cunimplemented: unimplemented block
  raise exception.create('unimplemented');
  result := -1;
end;

function TSERowSet.GEtRowCount: integer;
begin
  result := length(FRowset);
end;

function TSERowSet.GetValues(x, y: integer): variant;
var
  iRow: integer;
begin
  iRow := GetIndexedRow(y);
  if length(FRowSet[iRow]) <> fieldCount then
    SetLength(FRowset[iRow], fieldcount);

  result := FRowset[iRow][x];
end;

function TSERowSet.IndexCount: integer;
begin
  result := length(FIndexNames);

end;

function TSERowSet.IndexOfField(sName: string): integer;
var
  t: integer;
begin
  sName := lowercase(sName);
  result := -1;
  for t:= 0 to FieldCount-1 do begin
    if lowercase(self.Fields[t].sName) = sName then begin
      result := t;
      break;
    end;
  end;
end;

procedure TSERowSet.LAst;
begin
  high(FRowset);
end;

procedure TSERowSet.LoadfromCSV(f: string);
var
  sl: TStringlist;
  sLine, sValue: string;
  row,fld: ni;
  h: IHolder<TStringlist>;
begin
  self.Clear;
  sl := TStringlist.create;
  try
    sl.LoadFromFile(f);

    if sl.count = 0 then
      raise ECritical.create('Empty CSV file');
    //header
    sLine := sl[0];
    h := stringx.ParseStringNotInH(sLine, ',', '"');
    for fld := 0 to h.o.Count-1 do begin
      sValue := h.o[fld];
      if sValue <> '' then begin
        sValue := stringreplace(sValue, ' ', '_', [rfReplaceAll]);
        sValue := stringreplace(sValue, '-', '_', [rfReplaceAll]);
        sValue := stringreplace(sValue, '/', '_per_', [rfReplaceAll]);
        if svalue <> '' then begin
          var pf := self.AddField;
          pf.sName := sValue;
          pf.vType := TFieldType.ftString;
        end;
      end;
    end;

    for row := 1 to sl.count-1 do begin
      sLine := sl[row];
      h := stringx.ParseStringNotInH(sLine, ',', '"');
      if not stringlist_valuesblank(h.o) then begin
        self.AddRow;
        for fld := 0 to lesserof(h.o.Count, fieldcount)-1 do begin
          sValue := h.o[fld];
          self.CurRecordFieldsByIdx[fld] := sValue;
        end;
      end;
    end;

  finally
    sl.free;
  end;

end;

procedure TSERowSet.LoadFromfile(f: string; filterfunc: TAnonRecFilter; startAtIndex: string = ''; startAtIndexValue: int64 = 0);
var
  sLine: string;
  c: char;
//  fs: TMBMemoryStringStream;
  fs: TMultiBufferMemoryFileStream;
  x,y: ni;
  v: variant;
  iStarT: int64;
  iLen: int64;
  bAccept: boolean;
  bHeaderFound: boolean;
  bBreak: boolean;
begin
  fs := nil;
  try
    if (not fileexists(f)) then
      raise ECritical.create('local table does not exist '+f)
    else begin
      fs := TMultiBufferMemoryFileStream.Create(f, fmOpenRead);
      fs.Seek(0,soBeginning);
    end;

    if startAtIndex <> '' then begin
      iStart := FindSeekInIndex(f, startAtIndex, startAtIndexValue);
      if iStart > 0 then
        fs.Seek(iStart, soBeginning);
    end;

    bHeaderFound := false;
    while fs.position < fs.size do begin
      iStart := fs.Position;
      bBreak := false;
      while fs.position < fs.size do begin
        stream_guaranteeread(fs, @c, sizeof(c));
        if c = #10 then begin
          iLen := (fs.Position - iStart);
          setlength(sLine, iLen shr 1);
          fs.Seek(iStart, soBeginning);
          stream_guaranteeRead(fs, @sLine[strz], iLen);
          if not bHeaderFound then begin
            DefineFromHeaderString(sLine);
            bHeaderFound := true;
            break;
          end else begin
            AddRowFromString(sLine);
            filterfunc(self, bAccept, bBreak);
            if not bAccept then
              SetLength(self.FRowset, length(FRowset)-1);
            break;
          end;
        end;
      end;
      if bBreak then
        break;
    end;


  finally
    fs.free;
    fs := nil;
  end;
end;


function TSERowSet.Lookup(sLookupField: string; vLookupValue: variant;
  sReturnField: string): variant;
var
  r: ni;
  f: ni;
begin
  result := null;
  r := FindValue(sLookupField, vLookupValue);
  f := indexoffield(sReturnfield);
  if f < 0 then
    raise ECritical.create('Return field '+sReturnField+' not found.');

  if r >=0 then
    result := values[f,r];


end;

procedure TSERowSet.Maintain(sField: string);
begin
  FMaintain.Duplicates := dupIgnore;
  FMaintain.Add(lowercase(sField));

end;

procedure TSERowSet.Next;
begin
  inc(FCursor);

end;

function TSERowSet.FindSeekInIndex(sBaseFile, sIndex: string; value: int64): int64;
var
  sIDXFile: string;
  sex: TSEIndexfile;
begin
  sIDXFile := sBasefile+'.'+sIndex+'.idx';
  if not fileexists(sIDXfile) then
    raise ECritical.create('Index not found '+sIndex);

  sex := TSEIndexFile.Create;
  try
    sex.Open(sIDXFile, false);
    result := sex.AddrOf(value);
  finally
    sex.free;
  end;




end;

function TSERowSet.FindValue(sField: string; vValue: variant): ni;
var
  t: ni;
  f: ni;
  v: variant;
begin
  result := -1;
  f := self.IndexOfField(sField);
  if f< 0 then
    raise ECritical.create('Rowset does not have field '+sField);

  for t:= 0 to rowcount-1 do begin
    v := values[f,t];
    if varType(v) = varString then begin
      if comparetext(values[f,t], vValue) = 0 then begin
        exit(t);
      end
    end else begin
      if Values[f,t] = vValue then
        exit(t);
    end;

  end;

end;

procedure TSERowSet.OpenIndexes(sBaseFile: string; bForRead, bForAppend: boolean);
var
  t: ni;
  sidx: string;
  sIdxFile: string;
begin
  setlength(idxs, FMaintain.count);
  for t:= 0 to FMaintain.Count-1 do begin
    sIDX := FMaintain[t];
    sIDXFile := sBasefile+'.'+sIDX+'.idx';
    if bForRead then begin
      if not fileexists(sIDXfile) then
        TFileStream.Create(sIDXFile, fmCreate).free;

      idxs[t] := TMultiBufferMemoryFileStream.create(sIDXFile, fmOpenRead);

    end else begin
      if (not fileexists(sIDXfile)) or (not bForAppend) then
        idxs[t] := TMultiBufferMemoryFileStream.Create(sIDXFile, fmCreate)
      else
        idxs[t] := TMultiBufferMemoryFileStream.create(sIDXFile, fmOpenReadWrite);
    end;
  end;
end;

procedure TSERowSet.PrePend(rs: TSERowSet);
var
  t: ni;
begin
  rs.last;
  while cursor>=0 do begin
    AddRowFirst;
    for t:= 0 to fieldcount-1 do begin
      CurRecordFieldsByIdx[t] := rs.CurRecordFieldsByIdx[t];
    end;
    rs.Previous;
  end;
end;

procedure TSERowSet.Previous;
begin
  dec(FCursor);
end;

function TSERowSet.RowToMYSQLValues(r: TSERow): string;
var
  t: ni;
  cell: TSECell;
begin
  result := '(';
  for t:= 0 to high(r) do begin
    cell := r[t];
    case vartype(cell) of
      varString, varUString, varOleStr:
      begin
        cell := StringReplace(cell, 'â„¢', '™', [rfReplaceAll]);
      end;
    end;
    if t > 0 then
      result := result + ',';

    result := result + gvs(cell);


  end;
  result := result + ')';
end;

function TSERowSet.RowToString: string;
var
  x,y: ni;
  v: variant;
begin
  result := '';
  y := cursor;
  for x:= 0 to self.FieldCount-1 do begin
    v := self.Values[x,y];
    result := result + (#1+fieldvaluetostring(v, self.FieldDefs[x].vType));
  end;
end;

procedure TSERowSet.SavetoCSV(f: string);
begin

end;

procedure TSERowSet.SavetoFile(f: string; bAppend: boolean);
var
  sLine: string;
//  fs: TMBMemoryStringStream;
  fs: TMultiBufferMemoryFileStream;
  x,y: ni;
  v: variant;
  iStartAddr: int64;
begin
  fs := nil;
  try
    if (not fileexists(f)) or (not bAppend) then begin
      fs := TMultiBufferMemoryFileStream.Create(f, fmCReate);
      sLine := GetHeaderString+#10;
      Stream_GuaranteeWrite(fs, @sLine[STRZ], length(sLine) shl 1);//<<--- write HEADER LINE
    end else begin
      fs := TMultiBufferMemoryFileStream.Create(f, fmOpenReadWrite);
      fs.Seek(0,soEnd);
    end;

    OpenIndexes(f, false, bAppend);
    try
      for y := 0 to Self.RowCount-1 do begin
        cursor := y;
        sLine := rowtostring+#10;
        iStartAddr := fs.Position;
        Stream_GuaranteeWrite(fs, @sLine[STRZ], length(sLine) shl 1);//<<---- WRITE individual lines
        WriteToIndex(f, bAppend, iStartAddr);
      end;
    finally
      CloseIndexes;
    end;

  finally
    fs.free;
    fs := nil;
  end;
end;

procedure TSERowSet.SetCurRecordFields(sFieldName: string;
  const Value: variant);
var
  idx: integer;
begin
  idx := self.IndexOfField(sFieldName);

  values[idx, FCursor] := value;


end;

procedure TSERowSet.SetCurRecordFieldsByIdx(idx: integer; const Value: variant);
begin
  if idx >= FieldCount then
    raise Exception('Record index out of X bounds: '+inttostr(idx));

  if FCursor > high(FRowSet)  then
    raise Exception('Record index out of X bounds: '+inttostr(idx));


  values[idx, self.FCursor] := value;
end;

procedure TSERowSet.SetFieldCount(iCount: integer);
begin
  SetLength(FFieldDefs, iCount);
  UpdateRowWidths(0);

end;

procedure TSERowSet.SetIndex(idx: integer);
begin
  FIndex := idx;
  First;
end;

procedure TSERowSet.SetRowCount(iCount: integer);
var
  iOldCount: integer;
begin
  iOldCount := length(FRowset);
  SetLength(self.FRowset, iCount);
  UpdateRowWidths(iOldCount); //TODO 2:optimize for insert and cancel... source of SLOWness

end;

procedure TSERowSet.SetValues(x, y: integer; const Value: variant);
begin
  if y >= RowCount then
    raise ECritical.create(classname+' does not have row '+inttostr(y));
  if x >= FieldCount then
    raise ECritical.create(classname+' does not have field #'+inttostr(x));

  FRowset[y][x] := value;
end;

function TSERowSet.ToJSONh: IHolder<TJSON>;
var
  x,y: ni;
  jRec: TJSON;
begin
  result := THOlder<TJSON>.create;
  result.o := TJSON.create;

  for y := 0 to rowcount-1 do begin
    jrec := nil;
    try
      jrec := TJSON.create;
      for x := 0 to fieldcount-1 do begin
        jrec.AddMemberPrimitiveVariant(self.FieldDefs[x].sName, self.Values[x,y]);
      end;

      result.o.AddIndexed(jrec);
    finally
      jrec.free;
    end;
  end;
end;

function TSERowSet.ToMYSQLCreateTable(sTableName: string): string;
var
  t: ni;
  sName: string;
begin
  result := 'create table '+sTableName+' (';

  for t:= 0 to Self.FieldCount-1 do begin
    if t> 0 then
      result := result + ',';

    sName := self.fields[t].sName;
    if sName = '' then
      sName := 'FLD'+inttostr(t);

    sName := stringreplace(sName, ' ', '_', [rfReplaceAll]);
    sName := stringreplace(sName, '-', '_', [rfReplaceAll]);
    sName := stringreplace(sName, '/', '_', [rfReplaceAll]);
    result := result + sName+' varchar(255) ';
  end;

  result := result + ');';


end;

function TSERowSet.ToMYSQLImport(sTableName: string): string;
begin
  result := 'insert into '+sTableName+' values '+ToMySQLValues+';';
end;

function TSERowSet.ToMYSQLValues: string;
var
  t: ni;
begin
  result := '';
  for t:= 0 to rowcount-1 do begin
    if t > 0 then
      result := result + ',';
    result := result + self.RowToMYSQLValues(self.FRowset[t]);
  end;
end;

function TSERowSet.ToString: string;
begin
  result := RowSettoString(self);
end;

procedure TSERowSet.UpdateRowWidths(iOldCount: integer);
var
  t: integer;
  i: integer;
begin
  i := length(self.FFieldDefs);
  for t := low(FRowSet)+iOldCount to high(FRowset) do begin
    SetLength(FRowset[t], i);
  end;

end;

procedure TSERowSet.WriteToIndex(sBaseFile: string; bForAppend: boolean; iStartAddr: int64);
var
  t: ni;
  sidx: string;
  sIdxFile: string;
  pair: TIndexPair;
begin
  for t:= 0 to FMaintain.Count-1 do begin
    sIDX := FMaintain[t];
    idxs[t].Seek(0,soEnd);
    pair.addr := iStartAddr;
    pair.value := CurRecordFields[sIDX];
    stream_GuaranteeWrite(idxs[t],@pair, sizeof(pair));
  end;

end;

{ TQueryList }

procedure TQueryList.AddQuery(s: string);
begin
  add(s);
  add('--execute--');
end;

procedure TQueryList.First;
begin
  FIdx := 0;
end;

function TQueryList.GetEOF: boolean;
begin
  result := FIDx >= (count-1);
end;

function TQueryList.GetNextQuery: string;
begin
  result := '';
  while not (self[FIDX] = '--execute--') do begin
    result := result+self[FIDX];
    inc(FIDX);
  end;
  inc(FIDX);
end;

function DatasetToString(ds: TCustomSQLDAtaSet; destroyit: boolean = false): string;
var
  iCount: integer;
  t: integer;
  s: string;
begin
  s := '';
  for t:= 0 to ds.FieldCount-1 do begin
    s := s+'['+ds.FieldDefs[t].Name+']';
  end;
  s := s+#13#10;


  ds.First;
  while not ds.Eof do begin
    for t:= 0 to ds.FieldCount-1 do begin
      s := s+'['+ds.Fields[t].AsString+']';
    end;
    s := s+#13#10;
    ds.next;
  end;

  result := s;

end;

function RowsetToString(ds: TSERowSet; destroyit: boolean = false): string;
var
  iCount: integer;
  t,u: integer;
  s: string;
  iCount2: integer;
begin
  s := '';
  for t:= 0 to ds.FieldCount-1 do begin
    s := s+'['+ds.fields[t].sName+']'#9;
  end;
  s := s+#13#10;

  iCount2 := ds.RowCount;
//  if iCount2 > 10 then
//    iCount2 := 10;
  for u := 0 to iCount2-1 do begin
    for t:= 0 to ds.FieldCount-1 do begin
      s := s+'['+vartostr(ds.Values[t,u])+']'#9;
    end;
    s := s+#13#10;
  end;


  result := s;

end;
function RowsetToValues(ds: TSERowset; bIncludeSEFields: boolean): string;
var
  x,y: integer;
  v: variant;
  sRow: string;
begin
  result := '';
  for y:= 0 to ds.RowCount-1 do begin
    sRow := '';
    for x:= 0 to ds.FieldCount-1 do begin
      if x > 0 then
        sRow := sRow+','+vartoMYSQLStorage(ds.Values[x,y])
      else begin
        if bIncludeSEFields then
          sRow := '(0,'+vartoMYSQLStorage(ds.Values[x,y])
        else
          sRow := '('+vartoMYSQLStorage(ds.Values[x,y])
      end;
    end;


    if y> 0 then
      result := result + ','+sRow+')'
    else
      result := result + sRow+')';
  end;


end;

function RowsetToSETStatement(ds: TSERowset): string;
var
  x,y: integer;
  v: variant;
  sRow: string;
begin
  if ds.RowCount > 1 then begin
    raise exception.create('Cannot use INSERT SET with more than more row in set');
  end;

  for y:= 0 to ds.RowCount-1 do begin
    sRow := '';
    for x:= 0 to ds.FieldCount-1 do begin
      if x > 0 then
        sRow := sRow+',';

      sRow := sRow+ds.fielddefs[x].sName+'='+vartoMYSQLStorage(ds.Values[x,y]);
    end;
  end;

  result := sRow;


end;

function FieldValuetoString(v: variant; vType: TFieldType): string;
begin
  case vType of
    ftTimeStamp: result := floattostr(v)
  else
    result := vartostr(v);
  end;
end;

function StringToFieldValue(s: string; fv: TFieldType): variant;
  procedure Unsupported;
  begin
    raise ECritical.create('unsupported type');
  end;
begin
  s := trim(s);
  case fv of
    ftUnknown: unsupported;
    ftString: exit(s);
    ftSmallint:exit(strtoint(s));
    ftInteger: exit(strtoint(s));
    ftWord: exit(strtoint(s));
    ftBoolean: exit(strtobool(s));
    ftFloat: exit(strtofloat(s));
    ftCurrency: exit(strtofloat(s));
    ftBCD: exit(strtoint(s));
    ftDate: exit(strtodatetime(s));
    ftTime: exit(strtodatetime(s));
    ftDateTime: exit(strtodatetime(s));
    ftBytes:unsupported;
    ftVarBytes: unsupported;
    ftAutoInc: unsupported;
    ftBlob: unsupported;
    ftMemo: exit(s);
    ftGraphic: unsupported;
    ftFmtMemo: exit(s);
    ftParadoxOle: unsupported;
    ftDBaseOle: unsupported;
    ftTypedBinary: unsupported;
    ftCursor: unsupported;
    ftFixedChar: unsupported;
    ftWideString: exit(s);
    ftLargeint:exit(strtoint(s));
    ftADT: unsupported;
    ftArray: unsupported;
    ftReference: unsupported;
    ftDataSet: unsupported;
    ftOraBlob: unsupported;
    ftOraClob: unsupported;
    ftVariant: exit(s);
    ftInterface: unsupported;
    ftIDispatch:unsupported;
    ftGuid: exit(s);
    ftTimeStamp: exit(strtofloat(s));
    ftFMTBcd: exit(strtofloat(s));
    ftFixedWideChar: exit(s);
    ftWideMemo: exit(s);
    ftOraTimeStamp: unsupported;
    ftOraInterval: unsupported;
    ftLongWord: exit(strtoint(s));
    ftShortint: exit(strtoint(s));
    ftByte: exit(strtoint(s));
    ftExtended:exit(strtofloat(s));
    ftConnection: unsupported;
    ftParams: unsupported;
    ftStream: unsupported;
    ftTimeStampOffset: unsupported;
    ftObject: unsupported;
    ftSingle: exit(strtofloat(s));
  else
    unsupported;
  end;

end;




{ TSEIndexFile }

function TSEIndexFile.AddrOf(val: int64): int64;
var
  t: ni;
begin
  for t:= 0 to Self.Count-1 do begin
    if Items[t].value = val then
      exit(t);
  end;

  exit(-1);

end;

procedure TSEIndexFile.Close;
begin
  fs.free;
  fs := nil;
end;

procedure TSEIndexFile.Detach;
begin
  if detached then exit;

  fs.free;
  fs := nil;

  inherited;

end;

function TSEIndexFile.GetCount: int64;
begin
  result := fs.size shr 4;
end;

function TSEIndexFile.Getitem(idx: int64): TIndexPair;
begin
  fs.Seek(idx shr 4, soBeginning);
  stream_GuaranteeRead(fs, @result, sizeof(result));
end;

procedure TSEIndexFile.Open(sFile: string; bForWriting: boolean);
begin
  close;

  if not fileexists(sFile) then
    TFileStream.create(sFile, fmCreate).Free;

  if bForWriting then begin
    fs := TMultiBufferMemoryFileStream.create(sFile, fmOpenReadWrite);
  end else begin
    fs := TMultiBufferMemoryFileStream.create(sFile, fmOpenRead);
  end;




end;

{ Tbti_Row }

function Tbti_Row.Compare(const [unsafe] ACompareTo: TBTreeItem): NativeInt;
var
  t: ni;
  other: Tbti_Row;
  f: ni;
begin
  other := Tbti_Row(acompareto);

  for t:= low(indexfieldIndexes) to high(indexfieldIndexes) do begin
    f := indexfieldIndexes[t];
    if desc[t] then begin
      if other.row[f] > row[f] then
        exit(-1);
      if other.row[f] < row[f] then
        exit(1);
    end else begin
      if other.row[f] < row[f] then
        exit(-1);
      if other.row[f] > row[f] then
        exit(1);
    end;
  end;

  exit(0);

end;

end.
