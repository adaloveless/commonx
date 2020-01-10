unit DataObjectFactory;

interface
uses Variants,DataObject, Exceptions, Classes, DataObjectXRef, betterobject, SharedObject, Dataobjectcache, systemx, typex, orderlyinit;

type

  TDataObjectFactory = class(TFakeLockQueuedObject)
  private
    function GetClassCount: integer;
    function GetFetchQuery(iID: integer): string;

  protected
    FFetchQueries: TStringlist;
    FXRefPool: TXRefPool;
    iLastXRefIntSearch: integer;
    iLastXRefIntSearchResult: integer;
    function GetClassNames(idx: integer): string;
  public
    property XRefPool: TXRefPool read FXRefPool;

    //Construction/destruction
    constructor create;override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    //************PRIMARY FUNCTIONS*******************//
    function LowLevelDOCreate(var obj: TDataObject; cache: TDataObjectCache; cType: TDataObjectClass; params: variant; DataCenter, DataTier, SessionID: integer):boolean; overload;
    function LowLevelDOCreate(var obj: TDataObject; cache: TDataObjectCache; sType: string; params: variant; DataCenter, DataTier, SessionID: integer):boolean; overload;


    //Content control
    procedure RegisterDataObjectClass(TClass: TDataObjectClass; iPrimaryKeyCount: integer; sFetchQuery: string); overload;
    procedure RegisterDataObjectClass(TClass: TDataObjectClass; iPrimaryKeyCount: integer); overload;

    //Cross referencing
    function GetClassTypeForClassName(sName: string): TDataObjectClass;
    function GetKeycountForClass(cClass: TDataObjectClass): nativeint;

    //Lowest level Data object creation
    function LowLowLevelDOCreate(out obj: TDataObject; cache: TDataObjectCache; cType: TDataObjectClass; params: variant; DataCenter, DataTier, SessionID: integer): boolean;overload;
    (*function LowLowLevelDOCreate(out obj: TDataObject; iType: integer; params: variant): boolean; overload;*)
    (*function LowLowLevelDOCreate(out obj: TDataObject; sType: string; params: variant): boolean; overload;*)


    property ClassNames[idx: integer]: string read GetClassNames;
    property ClassCount: integer read GetClassCount;

    procedure AddFetchQuery(iID: integer; sQuery: string);
    property FetchQueries[iID: integer]: string read GetFetchQuery;
    function IndexOfQuery(iID: integer):integer;


  end;

var
  DOCF: TDataObjectFactory;

implementation

uses sysutils, DataObjectServices;

//------------------------------------------------------------------------------
constructor TDataObjectFactory.create;
begin
  inherited;
  FXRefPool := TXRefPool.create;
  FFetchQueries := TStringlist.create;

end;

//------------------------------------------------------------------------------
destructor TDataObjectFactory.destroy;
begin
  FFetchQueries.free;
  FXRefPool.free;

  inherited;
end;
//------------------------------------------------------------------------------
procedure TDataObjectFactory.AddFetchQuery(iID: integer; sQuery: string);
begin
  LockWrite;
  try
{$IFDEF MSWINDOWS}
    FFetchQueries.addobject(sQuery,TObject(pointer(iID)));
{$ENDIF}
  finally
    Unlockwrite;
  end;
end;

procedure TDataObjectfactory.BeforeDestruction;
begin
  inherited;
end;

//------------------------------------------------------------------------------
function TDataObjectFactory.LowLevelDOCreate(var obj: TDataObject; cache: TDataObjectCache;
  sType: string; params: variant; DataCenter, Datatier: integer; sessionid: integer):boolean;
//NOT INTERFACED
var
  cType : TDataObjectClass;
  doTemp : TDataObject;
begin
  cType := self.GetClassTypeForClassName(sType);
  if cType = nil then
    raise EClassFactoryError.createFMT('No such Data object class: %s', [sType]);

  result:= LowLevelDOCreate(doTEmp, cache, cType, params, DataCenter, DataTier, sessionid);
  obj := doTemp;

end;
//------------------------------------------------------------------------------
function TDataObjectFactory.LowLevelDOCreate(var obj: TDataObject; cache: TDataObjectCache;
          cType: TDataObjectClass; params: variant; DataCenter, DataTier, SessionID: integer):boolean;
//INTERFACED
var
  xRef : PClassXRef;
  sName : string;
  sTypeName: string;

begin
  try
    result := true;

    xREf := XRefpool.FindXRefByClass(cType);

    //If class was not found... raise an exception
    if xRef.cClass = nil then
      raise Exception.createFMT('Class does not have an XRef Entry: %s',[xRef.sString]);

    //find the type name associated with the xREf
    sTypeName := xRef.sString;
    //Build the object name from the type and params
    sName := BuildObjectName(cType.ClassName, params, DataCenter, DataTier);
    //****

    //check to see if an object with the same name is already instantiated
    obj := cache.GetExistingObject(ctype.Classname, params, Datacenter, DataTier);

    //if the object exists,  return the existing object and exit
    if not (obj = nil) then begin
      //! obj.Reference; don't know what's going on here
      obj.IsLink := false;
      obj.ClearObjects;
    end
    else begin
//      xREf := XRefpool.FindXRefByClass(cType);
      result := LowLowLevelDOCreate(obj, cache, TDataObjectClass(xRef.cClass), params, DataCenter, DataTier, sessionid);
    end;

  except
    on e: Exception do begin
      raise ECritical.create('Error during unmarshall:'+ E.Message+'  Vartype = '+inttostr(varType(Params)));
    end;

  end;
end;


function TDataObjectFactory.GetFetchQuery(iID: integer): string;
var
  i: integer;
begin
  LockWrite;
  try
    i := IndexOfQuery(iID);
    if i < 0 then
      result := ''
    else
      result := FFetchQueries[i];

  finally
    Unlockwrite;
  end;


end;

function TDataObjectFactory.GetKeycountForClass(
  cClass: TDataObjectClass): nativeint;
begin
  result := XRefPool.GetPrimaryKeyCountForClass(cClass);
end;

function TDataObjectFactory.IndexOfQuery(iID: integer): integer;
begin
  LockWrite;
  try
{$IFDEF MSWINDOWS}
    result := FFetchQueries.IndexOfObject(TObject(pointer(iID)));
{$ELSE}
    result := -1;
{$ENDIF}
  finally
    Unlockwrite;
  end;

end;

//-----------------------------------------------------------------------------
function TDataObjectFactory.GetClassTypeForClassName(sName: string): TDataObjectClass;
var
  xRef: PClassXref;
begin
  xRef := XrefPool.FindXRefByString(sName);
  result := TDataObjectClass(xRef.cClass);
end;

//------------------------------------------------------------------------------
function TDataObjectFactory.LowLowLevelDOCreate(out obj: TDataObject; cache: TDataObjectCache; cType: TDataObjectClass; params: variant; DataCenter, DataTier, SessionID: integer): boolean;
//creates the appropriate Data object class, given a class name
var
  ext: TExtendedDoVars;
  xr: PClassXref;
begin
  ext.DataCenter := Datacenter;
  ext.DataTier := DataTier;
  ext.SessionID := sessionid;

  var tok := TDataObjectToken.Create;
  try
    xr := self.XRefPool.FindXRefByClass(cType);
    if xr = nil then
      raise Exception.Create('xr=nil');
    tok.TypeName :=xr.sString;
    tok.VariantParams :=params;
    tok.DataCenter := datacenter;
    tok.datatier := datatier;

    var ex := cache.GetExistingObject(tok.name);
    if ex <> nil then
      obj := ex
    else begin
      obj := cType.create(self, params, cache, ext);
      if ex <> nil then begin
        obj.free;
        obj := ex;
      end;
    end;

    result := true;
  finally
    tok.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TDataObjectFactory.RegisterDataObjectClass(
  TClass: TDataObjectClass; iPrimaryKeyCount: integer; sFetchQuery: string);
begin
  XREfpool.AddClassXRef(TClass, iPrimaryKeyCount, TClass.ClassName);
  self.AddFetchQuery(iPrimaryKeyCount, sFetchQuery);

end;
//------------------------------------------------------------------------------
function TDataObjectFactory.GetClassCount: integer;
begin
  result := XRefPool.XRefCount;

end;


function TDataObjectFactory.GetClassNames(idx: integer): string;
begin
  result := XRefPool[idx].sString;
end;

procedure TDataObjectFactory.RegisterDataObjectClass(
  TClass: TDataObjectClass; iPrimaryKeyCount: integer);
begin
  XREfpool.AddClassXRef(TClass, iPrimaryKeyCount, TClass.ClassName);

end;

procedure oinit;
begin
  DOCF := TDataObjectFactory.create;
end;

procedure ofinal;
begin
  DOCF.free;
  DOCF := nil;
end;


initialization

  init.RegisterProcs('DataObjectFactory', oinit, ofinal);

end.
