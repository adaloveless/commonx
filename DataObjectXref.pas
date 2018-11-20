unit DataObjectXref;
//This unit contains the TXrefPool class and related classes.

interface
uses sysutils, stringx;


threadvar
  iTVLastIntXRefSearch: integer;
  iTVLAstIntXRefSearchREsult: integer;

type
  TClassXRef = record
  public
    cClass: TClass;
    sString: string;
    iPrimaryKeyCount: nativeint;
    procedure Init;
    procedure Finalize;
  end;

  PClassXRef = ^TClassXref;

  TClassXRefList = array of TClassXRef;

  TXRefPool = class(TObject)
  //The TXRefPool class maintains a list of TClassXRef records and supplies functions for
  //searching the records.  Each record has 3 fields, an Integer field, a string field, and a TClass field.
  //This 3 column table is typically used by class-factory objects, as it allows
  //for the creation of a particular class type, from other parsible types of data.  For example,
  //when a request comes back from the data-tier, the appropriate object is created
  //in memory to handle the data coming back. This is done by looking up the object-ID numbers
  //passed back from the data-tier and creating the class as defined in the XRefPool.
  private
  protected
    iXRefSize : integer;
    FObjectConsts: TClassXRefList;
    FNullXRef: TClassXRef;

    procedure InitObjectConsts; virtual;
    function GetXRefs(idx: integer): PClassXRef;
    function GetXRefCount: integer;


  public
    constructor create; virtual;


    destructor Destroy; override;

    function FindXRefByClass(cClass: TClass): PClassXref;
    function GetPrimaryKeyCountForClass(cClass: TClass): nativeint;
    function FindXRefByString(str: string): PClassXref;
    function IndexOfXRefByString(str: string): integer;

    procedure AddClassXRef(cClass: TClass; i: integer; str: string);

    property XRefs[idx: integer]: PClassXRef read GetXRefs; default;
    //Array property that allows access to all TClassXRef records by index.
    property XRefCount: integer read GetXRefCount;
    //Read-only property that returns the number of TClassXref records defined in the pool.  The records are added with AddClassXRef.

  end;

implementation



//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
destructor TXRefPool.Destroy;
//A Standard destructor.
begin
  Finalize(FObjectConsts);
  inherited;
end;
//------------------------------------------------------------------------------
function TXRefPool.FindXRefByClass(cClass: TClass): PClassXRef;
//p: cClass: A CLASS variable (not a class instance)... may also be thought of as a meta-class, although that specific language is not typically used in object pascal.
//Given a class variable (not a class instance), finds a record in the Xref pool matching.  If no record is found, result.cClass will be nil.  Once you have the record, you can look up a Integer constant or string name for the class variable.
var
  t: integer;
begin
  result := nil;
  for t:= 0 to iXrefSize-1 do begin
    if FObjectConsts[t].cClass = cClass then begin
      result := @FObjectConsts[t];
      break;
    end;
  end;

end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function TXRefPool.FindXRefByString(str: string): PClassXRef;
//p: str: The string for which you are searching on.  In the Case of PWLN, this will typically be the object type name e.g. "TdoUser".
//Given a string, returns a matching XRef Record. If no record is foudn, result.cClass will be nil.
var
  t: integer;
begin
  result := @FNullXref;

  for t:= 0 to iXrefSize-1 do begin
    if lowercase(FObjectConsts[t].sString) = lowercase(str) then begin
      result := @FObjectConsts[t];
      break;
    end;
  end;

end;

//------------------------------------------------------------------------------
procedure TXRefpool.AddClassXRef(cClass: TClass; i: integer; str: string);
//Adds a Xref record to the pool.
//p: cClass: The class you are adding.  Note: NOT the class instance, for example, you can use TObject, or TList, or TComponent.
//p: i: The integer representation of the class,  a constant that represents the class to your liking.
//p: str: A string that represents the class to your liking.
begin
  if iXRefSize >= length(FObjectConsts) then
    SetLength(FObjectConsts, iXrefSize + 20);

  FObjectConsts[iXRefSize].cClass :=cClass;
  FObjectConsts[iXrefSize].iPrimaryKeyCount := i;
  FObjectConsts[iXrefSize].sString := str;
  inc(iXrefSize);
end;

procedure TXRefPool.InitObjectConsts;
//This function is only here to generify the TXrefPool class.  It is virtual, and therefore, overridable by descendant classes.  Descendant classes can overide this to define how their descendents get their information.  PWLN infuses the information from an external source so this funtion does nothing.
begin
//TODO -cunimplemented: unimplemented block
end;

constructor TXRefPool.create;
//Typical constructor, no parameters.
begin
  inherited create;
  SetLength(FObjectConsts, 20); //<<--set this length to readjust
                                   //the size of the array for object constants
  InitObjectConsts;

  FNullXRef.init;
  FNullXRef.sString := '';
  FNullXRef.cClass := nil;

end;

function TXRefPool.GetXRefs(idx: integer): PClassXRef;
//Getter for the XRefs array property.  Returns an Xref record at given index.
begin
  result := @FObjectConsts[idx];

end;

function TXRefPool.GetPrimaryKeyCountForClass(cClass: TClass): nativeint;
begin
  result := FindXRefByClass(cClass).iPrimaryKeyCount
end;

function TXRefPool.GetXRefCount: integer;
//Getter for the XRefCount property.  Returns the number of TClassXref records defined in the pool.
begin
  result := iXRefSize;
end;

{ TClassXRef }

procedure TClassXRef.Finalize;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


procedure TClassXRef.Init;
begin
  sString := '';
end;


function TXRefPool.IndexOfXRefByString(str: string): integer;
var
  t: integer;
begin
  result := -1;
  for t:= 0 to iXrefSize-1 do begin
    if lowercase(FObjectConsts[t].sString) = lowercase(str) then
      result := t;
  end;

end;

end.

