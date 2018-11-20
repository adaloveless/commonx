unit DataObject;
//this unit contains the Data object class and related classes.
{$D+}

interface

uses

	TickCount, Classes, Sysutils, sharedobject, webstring,
  InterfaceList, PersistentInterfacedObject, stringx, variants, typex, systemx, better_collections;

var
  tokencount: integer;
  fieldcount: integer;

const
  DO_EXPIRE_TIME = 60000;
  DYN_ARRAY_BLOCK_SIZE = 10;
  NULL_DATETIME : double = 0.0;
type
  TDataObjectToken = class;//forward
  TCacheInfo = record
    sType: string;
    sSubindex: string;
    iObjectCount: integer;
  end;
  TExtendedDOVars = record
    DataTier: integer;
    DataCenter: integer;
    SessionID: integer;
  end;

  TServerStat = (
    psTotalReqTime,
    psCacheSearchTime,
    psMarshallingTime,
    psConnectTime,
    psSendTime,
    psServerTime,
    psReadTime,
    psUnMarshallingTime,
    psPingTime);

  TDataObjectCacheBias = (
    cbSmall,
    cbCompound,
    cbSmallList,
    cbHuge);

  TDataFieldType = (dftUndefined, dftString, dftShort, dftLong, dftDouble, dftDateTime, dftBoolean, dftVariant);
  TDataObjectOrigin = (dorNew, dorFetch, dorGhost, dorNotValid);

  TDataObject = class; //forward declaration

  TDataField = class; //forward declaration
  TDataFieldClass = class of TDataField;

//------------------------------------------------------------------------------
  TObjectFieldValidationEvent = procedure (
    sender: TDataObject; var value: variant; field: TDataField;
    var Accept: boolean) of object;
    //Represents a function pointer to a Cross-Field Level Validation Hook

  TFieldValidationEvent = procedure (
    sender: TDatafield; var value: variant; var Accept: boolean) of object;
    //Represents a function pointer to a Field-Level Validation Hook

  TObjectValidationEvent = procedure (
    sender: TDataObject; var Accept: boolean) of object;
    //Represents a function pointer to an Object Validation Hook - Before Save ... etc

//------------------------------------------------------------------------------
  TDOFieldDef = record
    //This structure holds extrinsic/factory-definition information about the
    //fields in a Data object.
    Name: string;
    ClassVar: TDataFieldClass;
    Value: variant;
//    DefaultValue: variant;
    Instance: TDataField;
    ExtIndex: smallint;
    ValIndex: smallint;
  end;
//------------------------------------------------------------------------------
  TExtDOFieldDef = record
    //This structure holds extrinsic/factory-definition information about the
    //fields in a Data object.
    LookupObj: cardinal;
    LookupVars: variant;
    IndexField: string;
    DisplayField: string;
  end;
//------------------------------------------------------------------------------
  TValDOFieldDef = record
    OnCanSave: TFieldValidationEvent;
    OnCanExit: TFieldValidationEvent;
    OnCanChange: TFieldValidationEvent;
  end;



//#########################################################

	TDataFieldChoices = class;

//#########################################################
  TDataObject = class(TObject)
  //R:Generic container for all types of data in the system.
  //R:Maintains a list of TDataField classes
  //R:Maintains an indexed list of TDataObject classes
  //R:Maintains an named list of TDataObject classes
  //R:Maintains a reference to a TDataObjectToken class for key handling

  //The Data object is the bread and butter representation of data in the middle tier.
  //It contains four key properties.<BR><B>
  //1. token property: a token that represents its name and uniqueness, essentially the database keys (see TDataObjectToken in this unit)<BR>
  //2. fld property: a list of fields<BR>
  //3. obj property: sub-objects or 'detail' objects - ordered/sorted/numerically indexed, references to other Data objects<BR>
  //4. assoc property: 'associated' objects, that are essentialy shortcuts to related information, indexed by a friendly name.<BR></B>
  //<B>Other Notes:</B><BR>
  //All the main objects are essentially the same at design-time. Their design time class representations are mostly identical.
  //Their runtime representations however, can be very unique.
  //<P>When a class is inherited from TDataObject, it is given its uniqueness through function calls in the constructor.
  //To make your own Data object, inherit from TDataObject, override the constructor, and define your own fields with AddFieldDef() and your own associates with AddAssociate() property.
  //Then override DoGetAssociate to implement your own fetching for associated objects.
  //<P>Detail objects do not have any configurable attributes, because they typically are defined by the data tier.  However, the middle tier may implement custom sorting algorithms for detail objects.  To do this, override the sort() procedure.
  //<p>There are many supporting functions implemented in the Data object class that support manipulation of fields, detail objects, and associated objects and make working with this data easier.  Familiarize yourself with <I>every</I> property and fucntion.  This is the most important class to understand in the system.

  private
    FOrphaned: boolean;
    FCache: TObject;
    FIsChanged: boolean;
    FSpecialFieldDefs: TStringList;
    FIsLnk: boolean;
    FIsLink: boolean;
    FLinkTo: string;
    FGenesis: cardinal;
    FExpired: boolean;
    FTableLink: string;
    FFetchQuery: string;
    FIsList: boolean;
    FListOf: string;
    FIdentityKeyCount: ni;
    FOrderBy: string;
    procedure SetOrphaned(const Value: boolean);
    function GetXMLElementName: string;virtual;
    function GetSpecialFieldDefs: TStringList;
    function GetLinkTo: string;
    procedure SetLinkto(const Value: string);
    function GetAge: cardinal;
    function GetExpired: boolean;
    procedure SEtGenesis(const Value: cardinal);

    function GetExportString: string;
    function GetFetchQuery: string;
    function GetInsertQuery: string;
    function SKipInsertKEyCount: ni;
    function GetDeleteQuery: string;
    function GetUpdateQuery: string;
    function GetKeyCount: ni;
    function ReplaceQueryKeys(sQuery: string): string;
    function GetfilterPhrase: string;
    procedure SetIsChanged(const Value: boolean);
    function GetKeyUpdateQuery: string;
    function GetO(vNameOrIndex: variant): TDataobject;
    function GetautoKeyName(idx: ni): string;
    function GetSaveQuery: string;

  protected
    //Standard property storage variables (naming convention compliant)
    FAutoKeyTable: string;
    FAutoKeyField: string;
    FSessionID: integer;
    FFieldCount: smallint;
    FCreator: Tobject;
    FToken: TDataObjectToken;
    FReferences: integer;
    FKeys: array of string;
    FFilterPhrase: string;

    //object general validation
    FOnCanSave: TObjectValidationEvent;

    //TLists pointing to fields and objects //Todo:Change to TInterfaceList
    slAssociates: TStringList;
    lstNotifyObjects: TBetterList<TDataObject>;
    lstObjects: TBetterList<TObject>;
    lstPendingDelete: TBetterList<TDataObject>;
    lstCoordinators: TInterfaceList;
//    lstControls: TInterfaceList;
    lstSpecialFields: TBetterList<TObject>;
    //Flag used to raise an exception if reference count increases while
    //destroying (very bad)
    bDODestroying: boolean;
    FOrigin: TDataObjectOrigin;

    //number of extended Data obejct field defnitions
    iExtFieldDefCount: word;
    iValFieldDefCount: word;

    FRefCount: integer;

    //Getters and Setters
    function GetIFld(sName: string): TDataField;
    function GetIFldByIndex(idx: integer): TDataField;
    function GetTFld(sName: string): TDataField;
    function GetTFldByIndex(idx: integer): TDataField;
    function GetFieldCount: integer;

    function GetObject(idx: integer): TDataObject;
    function GetDetailTokens(idx: integer): TDataObjectToken;
    function GetObjectCount: integer;
    function GetAssociateByIndex(idx: integer): TDataObject;
    function GetAssocName(idx: integer): string;
    function GetAssociate(sName: string): TDataObject;
    function GetAssociateCount: integer;
    function IndexOfObjectInList(sObjectName: string;list: Tlist):integer;
    function GetName: string;
    function GetIToken: TDataObjectToken;
    function GetTToken: TDataObjectToken;
    function GetCaption: string; virtual;

    //special fields
    function HasSpecialFieldDef(sFieldName: string): boolean;
    function IsSpecialField(sFieldName: string): boolean;
    function GetSpecialField(sFieldName: string): TDataField;
    function CreateSpecialField(sFieldName: string): TDataField;
    function DoCreateSpecialField(sFieldName: string): TDataField; virtual;

    //flywieight arrays
    function GetFieldDefs(idx: integer): TDOFieldDef;
    procedure SetFieldCapacity(iSize: cardinal);
    function GetFieldCapacity: cardinal;
    function GetExtFieldCapacity: cardinal;
    procedure SetExtFieldCapacity(const Value: cardinal);
    function GetValFieldCapacity: cardinal;
    procedure SetValFieldCapacity(const Value: cardinal);


    function GetFastValues(idx: integer): variant;
    //Reads a field value without actually instatiating a field.  Returns the
    //value as a variant value.  This is sometimes necessary because instatiating
    //1000's of fields can be slow.
    procedure SetFastVAlues(idx: integer; Value: variant);
    //Writes a field value without actually instatiating a field.  Unmarshalling
    //can write values this way to make things faster, however, it is more dangerous
    //because there is less checking on the field types.

    function CanSave: boolean; virtual;
    //Asks the object whether or not it is ready to be saved.  Override this to
    //raise exceptions if an object is not in a state to be saved.

    procedure InitializeObjectList;
    //Initializes the object list upon request.  The list is normally not initialized
    //until asked for to save memory.
    procedure InitializeAssociateList;
    //Initializes the associate list upon request.  The list is normally not initialized
    //until asked for to save memory.
    procedure InitializeControlList;
    //Initializes the control list upon request.  The list is normally not initialized
    //until asked for to save memory. (OBSOLETE REMOVE)
    procedure InitializeCoordinatorList;
    //Initializes the coordinator list upon request.  The list is normally not initialized
    //until asked for to save memory. (OBSOLETE REMOVE)

    function InstantiateToken(var obj: TDataObject; Token: TDataObjectToken; iSessionID: integer): boolean;
    //Function for "lazy instantiation" of tokens. Rarely used.

    procedure AddAssociate(sName: string; sTypeName: string; Params : variant); overload;
      //Adds an STATIC "associate" definition to the object.  Specify the type name
      //and Params as you wish.
    procedure AddAssociate(sName: string {OVERRIDE DoGetAssociate}); overload;
      //Adds a dynamic associate to object (preferred)...
      //YOU MUST OVERRIDE DoGetAssociate TO RETURN THIS ASSOCIATE


    procedure AddCalculatedField(sName: string; FieldType: TDataFieldClass);
    //Adds a calculated field.  Calculated field values are determined through programmatic
    //methods.  They are not marshalled.


    function DoNew(iSessionID: integer): boolean; virtual;
    //Use DoNew to implment calls to KEY generation server requests for
    //inherited Data objects

    function DoGetAssociate(sName: string): TDataObject; virtual;
    //Override DoGetAssociate and implement a fetching mechanism when the
    //associate given in the sName parameter is requested.
    //Typically you want to call LazyFetch on a TServerInterface class (usually
    //the one from which this object comes from ... see TDataObject.DataTierID property)
    //The object you fetch is then returned as the result.

    function GetIsGhost: boolean;
    //Getter for the "IsGhost" property.  A "ghosted" object is an object whose
    //keys were not obtained from the database, but were specified directly.
    //This is common for Web applications, because the Keys are typicaly
    //acquired when the HTML form is displayed and then passed along with the
    //form during submit to eliminiate problems that could occur if the
    //submit button is clicked twice.

    function GetIsNew: boolean;
    //Getter for the "IsNew" property.  Reports whether the object should
    //be added to the Database (true), or whether the object should be updated.
    //The TServerInterface class relies on this to determine which request to
    //make of the DataTier.  Most Programmers typically don't have to worry about it
    //because the maintenance of this flag is almost always automatic.  There
    //are 1 or 2 cases where this flag has been forced in Web Page code however.

    procedure SetIsNew(const Value: boolean);
    //Setter for the "IsNew" property.  Called when the programmer wants to
    //force the IsNew flag.  Typically only called to solve problems that might
    //arise because the MiddleTier is calling an "Add" request vs. a "Change"
    //request in the DataTier or vice verse.


    procedure ClearAssociateTokenFlags;
    //Internal use only.
    procedure RegisterWithCache; virtual;
    //Registers the object in the cache.  Called upon creation.
    procedure DeRegisterWithCache; virtual;
    //Removes the object for its cache.  Called upon destruction.

    procedure AfterWriteField(field: TDataField);virtual;

    function Fetch(sType: string; tokenparams: variant; sessionid: ni): TDataObject;


  public
    FFieldDefs: array of TDOFieldDef;
    FExtFieldDefs: array of TExtDOFieldDef;
    FValFieldDefs: array of TValDOFieldDef;
      //field defs needs to be public due to property limitations

    procedure OnParentKeyUpdate(parent: TDataObject; priorkeys: variant);virtual;
    procedure InitDefinition;virtual;
    constructor create(AOwner: Tobject; params: variant; ACache: TObject; extended: TExtendedDOVars); reintroduce; virtual;
    destructor destroy; override;
    procedure Detach(bDestroying: boolean = false);

    procedure AddFieldDef(sName: string;
                          FieldType: TDataFieldClass;
                          vInitialValue: variant); overload;
    procedure AddFieldDef(sName: string;
                          FieldType: TDataFieldClass;
                          vInitialValue: variant;
                          LookupObj: cardinal;
                          LookupVars: variant;
                          DisplayField, IndexField: string); overload;
    procedure AddFieldDef(sName: string;
                          FieldType: TDataFieldClass;
                          vInitialValue: variant;
                          const CanChange, CanExit, CanSave: TFieldValidationEvent);overload;
    procedure AddFieldDef(sName: string;
                          FieldType: TDataFieldClass;
                          vInitialValue: variant;
                          LookupObj: cardinal;
                          LookupVars: variant;
                          DisplayField, IndexField: string;
                          const CanChange, CanExit, CanSave: TFieldValidationEvent);overload;

    //Causes the object to remove all references to other objects.  This is called
    //by the TDataObjectClassJanitor to eliminate problems with destruction orders.

    property IsLink: boolean read FIsLink write FIsLink;
    procedure Link(sType: string; vParams: variant);
    procedure BeforeDestruction; override;
    //TDataObject overrides TObject.BeforeDestruction to do some cleanup.


    procedure SwapObjects(idx1, idx2 : integer);
    //Helper function for sorting of detail objects.
    procedure Sort(iSortIndex : integer); virtual;
    //By default, this function does nothing.  Override it in your descendent
    //class to implmement a sorting method of your choice for detail objects.
    //The iSortIndex parameter is there so that you can implement multiple sorting
    //algorythms and request any of them by passing a different sort index.

    property Cache: TObject read FCache write FCache;
    //Returns the TDataObjectCache in which this object's life is maintained.
    //The object registers with the Cache when it is created, and deregistered when freed.

    function HasTokens: boolean;
    procedure InstantiateAllTokens;
    procedure QuickSort(CompareFunction: TListSortCompare);

    property Token: TDataObjectToken read GetTToken;
    // the token handles the task of identifying the Data object.  Why do we
    // need a separate object?  we need  separate object to represent Data
    // objects that have yet to be requested from the server.  The associate
    // and detail object lists can contain JUST TOKENs instead of full-fledged
    // domnain objects.  When a user requests an associate, the token is then
    // exchanged for the real object.  We call this "lazy" instantiation.

    property o[sNameOrIndex: variant]:TDataobject read GetO;

    property fld[sName: string]: TDataField read GetTFld;default;
    //Represents fields in the object
    // a Data object that has ONLY fields, looks an awful lot like a RECORD.
    // These fields contain the basic stuff... e.g. the name of a user, the
    // title of an activity.   The list is of TDataField, from which there are
    // several derivatives.  The TDataField code lies in THIS unit.

    property FieldByIndex[idx: integer]: TDataField read GetTFldByIndex;
    //Returns fields by index rather than the typical method (by Name).
    property FieldCount: integer read GetFieldCount;
    //Returns the number of fields in the object

    function IndexOfField(sName: string): integer;
    property FieldCapacity: cardinal read GetFieldCapacity write SetFieldCapacity;
    //Set the field capacity property in the constructor of your class, to optimize
    //object creation speed.  If this property is not set, the system may be slower
    //as the field array will have to be grown to accomodate field definitions
    //as they are added.
    property ExtFieldCapacity: cardinal read GetExtFieldCapacity write SetExtFieldCapacity;
    //Specify the number of fields which use Extended attributes. (For optomization).
    //"Extended" attributes refers to any field that is called using any form of the AddFieldDef procedure other than the simplest overloaded version.

    property ValFieldCapacity: cardinal read GetValFieldCapacity write SetValFieldCapacity;
    //Specify the number of fields that have validation events attached to them.  (For optomization).

    property FastValues[idx: integer]: variant read GetFastValues write SetFastVAlues;
    //Implements lower level reading and writing of field values (used by Marshalling).

    property assoc[sType: string]: TDataObject read GetAssociate;
    //Represents "Associated" Objects
    // Objects that relate to this object.  Defined at design-time.  E.g. the
    // assignment for a user, a user's picture,
    property AssocNames[idx: integer]: string read GetAssocName;
    //Returns the names of associates by index.
    property assocByIndex[idx: integer]: TDataObject read GetAssociateByIndex;
    //Returns a given associated object by Index rather than by the typical method (by name)
    property AssociateCount: integer read GetAssociateCount;
    //Returns the number of associated object registered with this object.

    property obj[idx: integer]: TDataObject read GetObject;
    //Represents Object content -- "Detail" objects
    // Contents are defined dynamically at runtime based on results of server
    // requests.  E.g. a User List, Activity List... etc.
    property detail[idx: integer]: TDataObject read GetObject;
    //Another name for the obj property, calls the same functions
    property DetailTokens[idx: integer]: TDataObjectToken read GetDetailTokens;
    //Returns the tokens of the detail objects (for when you don't care about anything else)
    //Using this method can be a bit faster in cases where the objects in-question may not have been fetched.

    property ObjectCount: integer read GetObjectCount;
    //Returns the number of detail objects.

    procedure AddObjectToken(obj: TDataObjectToken);
    //Adds a detail object token (not a full object). If the object is requested
    //via the obj[] or details[] property, it will be automatically requested from the
    //database and replace with a full-fledged object.
    procedure AddObject(sType: string; params: variant); overload;
    //Adds a detail object as a child of this object.
    procedure DoAddObject(obj: TDataObject; out bHandled: boolean); virtual;
    //Overridable hook that allows descendents to trigger events when objects are added. (Such as resort).
    procedure AddObject(obj: TDataObject); overload;
    //Adds a detail object as a child of this object.
    procedure AddObject(iType: integer; params: variant); overload;
    //Adds a detail object as a child of this object.
    procedure InsertObject(obj: TDataObject; iPosition: integer);
    //Inserts a detail object at the given position in the list.
    procedure ClearObjects;
    //Removes all references to other detail objects.
    function IndexOfObject(doFind: TDataObject): integer;
    //Returns the index of the reference to the detail object in question.  Keep in mind
    //that all references to objects with identical keys are compatible with this function because
    //the objects are cross-refrenced in the TDataObjectCache which guarantees
    //that there are no two objects with indentical keys instantiated in memory at the same time.
    procedure DeleteObject(iIndex: integer);
    //Deletes a detail objects at the given index.
    procedure RemoveObject(doRemove: TDataObject);
    //Removes all references to the given object from the detail list.
    procedure MoveObject(iSourceIndex, iDestIndex: integer);
    //Deletes the object at iSourceIndex and reinserts it at iDestIndex.
    function AssociateHasObject(doFind: TDataObject;
      sAssociateName: string): boolean;

    procedure RemoveDuplicates;

    function IndexOfAssociateObject(doFind: TDataObject;
      sAssociateName: string): integer;
    //Returns the index of the associate in the associate list if you wish to
    //get it by number instead of name.

    function IsAssociateInstantiated(sName: string):boolean; overload;
    //Reports whether or not the given associate has actually be materialized from the database.

    function IsAssociateInstantiated(iIndex: integer):boolean; overload;
    //Reports whether or not the given associate has actually be materialized from the database.

    function HasObject(doFind: TDataObject): boolean;
    //Returns whether or the detail object list contains the given object.  Useful
    //for preventing duplicates in the detail list.  For example, you may write:
    //If not HasObject(whatever) then AddObject(whatever);

    procedure MergeObjects(doMerger: TDataObject; bRemoveDuplicates: boolean);
    //Merges the detail objects of doMerger into the detail list of THIS object.
    //If bRemoveDuplicates is true then objects are NOT added if they already
    //exist in the list.

    property Origin: TDataObjectOrigin read FOrigin write FOrigin;
    //Reports where this object came from.  New, Fetch, Ghost...etc.

    property SpecialFieldDefs: TStringList read GetSpecialFieldDefs;
    //Calculated fields and such. Some calculated fields are built-in to the
    //TDataObject class.

    function Save(bIncludeChangedAssociates: boolean = false): boolean;
    //Saves the object to the DataTier from which it originated.

    function New(iSessionID: integer): boolean; //**Note -- non virtual... override DoNew instead
    //Creates a new instance of the object.  If proper hooks are in place in
    //descendent classes, it will also request necessary keys from the datatier to
    //ensure the uniqueness of the object.

    procedure Ghost; //Creates a NEW object WITHOUT requesting a KEY From DB
    procedure OnGhost; virtual;
    //Override this method to implement actions that occur when an object is Ghosted.


    function QueryInterface(const IID: TGUID; out Obj): HResult;
    //COM Standard.
    function _AddRef: integer; virtual;
    //COM Standard.
    function _Release: integer; virtual;
    //COM Standard.
    function _RefCount: integer; virtual;
    //COM Standard.
    procedure Reference;
    //COM Standard.
    procedure Release;
    //COM Standard.

    property References: integer read FReferences;
    //COM Standard.


    property Creator: TObject read FCreator;
    //If this object was created by another object, it is referenced here.

    property Caption: string read GetCaption;
    //This is a special value that can be implmemented in descendent classes.
    //Implements a "frieldly" name for the object. For example "TdoUser#256" could
    //be a.k.a "Smith, John".


    property Name: string read GetName;
    //This is a UNIQUE identifier for the object, it is build by concatinating the following information:
    //The Objects TypeName
    //The Object's DataCenterID
    //The Object's DatatierID
    //The Object's keys/parameters
    //Examples
    //TdoUser#1#2#256, TdoActivity#2#0#18000001, TdoRoleList#0#0#3
    property XMLElementName: string read GetXMLElementName;
    //Returns the name for this object to be used in XML documents.

    procedure SetOrphanedValues; virtual;


    function CanExitField(field: TDatafield; var value: variant): boolean; virtual;
    //OBSOLETE.
    function CanSaveField(field: TDataField; var value: variant): boolean; virtual;
    //Asks a field if it can be saved.  Works with Hooks for field validation.
    function CanChangeField(field: TDataField; var value: variant): boolean; virtual;
    //Asks a field if it can be changes.  Works with Hooks for field validation specified in the constructor with extended AddFieldDef calls.

    procedure ValidationError(sMessage, sField, sValue: string; bSevere: boolean);
    //If a validation error occurs in the middle tier (doesn't handle DT errors) this method is called.

    property IsChanged: boolean read FIsChanged write SetIsChanged;
    //Reports whether any fields in the object have been changed.
    property IsNew: boolean read GetIsNew write SetIsNew;
    //Reports if the object is NEW.
    property IsGhost: boolean read GetIsGhost;
    //Reports if the object is a FAKE NEW object.  It will behave like a new
    //object, however it will use KEYs that are dectated by the Middle tier, not the datatier.

    property IsOrphaned: boolean read FOrphaned write SetOrphaned;
    //I'm not sure where this is used or what it does.

    procedure ResetOrigin;
    //I'm not sure where this is used or what it does.

    procedure BeforeReadField(iFieldIndex: integer); overload; virtual;
    //This happens before a field is read.  Override it to handle calculated events.
    procedure BeforeReadField(sFieldName: string); overload; virtual;
    //This happens before a field is read.  Override it to handle calculated events.
    procedure BeforeSave; virtual;
    //This happens before an object is saved.  Override it to handle calculated events.

    procedure AfterSave; virtual;
    //This happens after an object is saved.  Override to trigger desired events such as saving other objects along with it.  Etc.


    //Returns a reference the the TDataObjectServices class to which this object belongs.

    property SessionID: integer read FSessionID write FSessionID;
    //The sessionID that was used to fetch/create the object.  This is the sessionID that should be
    //used to fetch associates etc.  It is important to use the proper session ID especially
    //in cases where a single page may request information from multiple databases.

    procedure RevertToDefaultValues;

    procedure ClearFieldDefs;

    property LinkTo: string read GetLinkTo write SetLinkto;

    property Expired: boolean read GetExpired write FExpired;
    property Genesis: cardinal read FGenesis write SEtGenesis;
    property Age: cardinal read GetAge;

    property ExportString: string read GetExportString;
    procedure DefineTableLink(sTableName: string; keynames: array of string; iAutoIncrementKeyCount: ni);
    procedure DefineCustomFetchQuery(sQuery: string; keyNames: array of string; iAutoIncrementKeyCount: ni);
    property TableLink: string read FTableLink;
    property Orderby: string read FOrderBy write ForderBy;
    property FetchQuery: string read GetFetchQuery;
    property InsertQuery: string read GetInsertQuery;
    property UpdateQuery: string read GetUpdateQuery;
    property DeleteQuery: string read GetDeleteQuery;
    property SaveQuery: string read GetSaveQuery;
    procedure DefineKeys(names: array of string; iAutoIncrementKeyCount: ni);
    property KeyCount: ni read GetKeyCount;
    property IsList: boolean read FIsList write FIsList;
    property ListOf: string read FListOf write FListOf;
    property FilterPhrase: string read GetfilterPhrase;
    property PendingDeleteList: TBetterList<TDataObject> read lstPendingDelete;
    property IdentityKeyCount: ni read FIdentityKeyCount;
    procedure DefineAutoKeyParams(sfield: string; sTable: string = '');
    property KeyupdateQuery: string read GetKeyUpdateQuery;
    property AutoKeyNames[idx: ni]: string read GetautoKeyName;
  end;

	TDataFieldChoices = Class(TPersistentInterfacedObject)
  //this class in an early experiment gone wrong
  //it is currently only used by the TdoUser class to convert RoleIDs into RoleNames
  //it is not worth the hassle required to program it.  i'd suggest removal
  protected
  	FDataObject:TDataObject;
    FDisplayField:string;
		FIndexField:string;
    function GetChoiceCount: integer;
    function GetChoices(idx: integer): string;
    function GetIndexes(idx: integer): string;
    function GetDisplayField: string;
    function GetIndexField: string;
    function GetDataObject:TDataObject;
	  procedure SetDisplayField(const Value: string);
    procedure SetIndexField(const Value: string);
    procedure SetDataObject(Value:TDataObject);
	public
		function ValueOf(sIndexFieldValue:string):string;
		function IndexOf(sDisplayFieldValue:string):string;
		Constructor Create; override;
    procedure BeforeDestruction; override;
		destructor Destroy; override;
		property Choices[idx:integer]:string read GetChoices;
		property ChoiceCount:integer read GetChoiceCount;
		property Indexes[idx:integer]:string read GetIndexes;
		property DataObject:TDataObject read GetDataObject write SetDataObject;
		property DisplayField:string read GetDisplayField write SetDisplayField;
    property IndexField:string read GetIndexField write SetIndexField;

  end;

  TDataObjectClass = class of TDataObject;


//------------------------------------------------------------------------------
  TDataObjectToken = class(TPersistentInterfacedObject)
  //This class represents the uniqueness of a Data object.  All objects
  //instantiated in a thread are required to have tokens composed of unique
  //values.  If you attempt to create a new objet in memory that has the same
  //token parameters as another object already in memory, the already existing
  //object will be used instead and its reference count will increase.
  //<P>The uniqueness of a doman object is composed of the following values:
  //<li>the type of object
  //<li>the keys that make it globally unique within the database</li>
  //<li>the database (datatier) id</li>
  //<li>the datacenter id</li>
  //This class is typically attached to a Data object, however tokens can somtimes be used in place of Data objects to mark where Data objects could exist.
  private
    FSession: integer;
    FDataCenter: integer;
    FDataTier: integer;
    FType: TDataObjectClass;
    FOwner: TDataObject;

    procedure SetVariantPArams(const Value: variant);


  protected
    FParams: variant;
    iParamIndex: integer;
    FObjectName: string;
    FKeys: array of string;

    function GetName: string;
    function GetConstName: string;
    function GetTypeName: string;
    procedure SetParam(idx: integer; const Value: variant);
    procedure SetTypeName(sTypeName: string);
    function GetParam(idx: integer): variant;
    function GetParamCount: integer;
    function GetVariantParams: variant;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Pool;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    property DataCenter: integer read FDataCenter write FDataCenter;
    //ID of the datacenter from which this object originated or will be added.
    property DataTier: integer read FDataTier write FDatatier;
    //ID of the datatier from which this object originated or will be added.
    property Session: integer read FSession write FSession;
    //The session Id used to request this object.

    property Name: string read GetName;
    property ConstName: string read GetConstName;
    //Queries the token for a "name" for the object.  The "name" is a friendly
    //representation of its uniqueness.<br>  Examples:
    //<li>TdoUser#0#0#12345</li>
    //<li>TdoActivity#4#2#466346</li>
    //<li>TdoGroupGraphicProgressReport#0#0#Title#All#False</li>
    property TypeName: string read GetTypeName write SetTypeName;
    //The type of object in string form.  Example: TdoUser, TdoActivity, TdoAccount
    property Params[idx: integer]: variant read GetParam write SetParam;
    //A virtual array of variants which are the keys for the object.
    property ParamCount: integer read GetParamCount;
    //Read only: Returns the number of parameters.
    function AddParam(param: variant; bNoDeregister: boolean = false): boolean; virtual;
    procedure DeleteParam(idx: integer);
    //Adds a parameter/key value to the token.


    property VariantParams: variant read GetVariantParams Write SetVariantPArams;
    //Returns the entire set of params/keys as a single variant variable.  Keep in mind that a variant can be an array of other variants.

    property Owner: TDataObject read FOwner write FOwner;



  end;





//#########################################################
  TDataObjectList = class(TDataObject)
  end;

//#########################################################
  TDoNil = class(TDataObject);

//#########################################################
  TDataField = class(TPersistentInterfacedObject)
  //the Data field represents a generic simple typethat is almost always aggregated to the Data object
  //currently there is no case where a Data field exists on its own without being attached to a Data object
  //the Data field class is typically not ever instantiated, however the subclasses are
  //subclasses include, TBooleanDataField, TLongintDatafield, TDateTimeDataField, etc.
  //all of these classes are contained within this unit
  private
    FChanging: boolean;
    FIsChanged: boolean;
    function GetOnCanChange: TFieldValidationEvent;
    function GetOnCanExit: TFieldValidationEvent;
    function GetOnCanSave: TFieldValidationEvent;
    function GetTypeName: string;
    function GEtExportString: string;

  protected
    FSpecialName: string;
    FSpecialValue: variant;
    FKeepAsString: boolean;
    FIsSpecial: boolean;
    FMyindex: integer;
    FOwner: TDataObject;
    FDataFieldChoices:TDataFieldChoices;
    FInAfterWrite: boolean;
    function GetName: string;
    procedure SetName(sName: string);
    procedure SetAsStringTemplate(str: string); virtual;
      //Note: SetAsStringTemplate is the actual setter (a template method) for
      //the AsString Property... not SetAsString.
    procedure SetAsString(str: string); virtual;abstract;
    procedure SetAsWebString(str: string); virtual;
    function GetAsString: string; virtual;
    function GetAsWebString: string; virtual;
    function GetStorageString: string; virtual;
    function GetQuotedStorageString: string; virtual;
    procedure SetAsVariantTemplate(v: variant);
      //Note: SetAsVariantTemplate is the actual setter (a template method)
      //for the AsVariant Property... not SetAsString.
    function GetAsVariant: variant; virtual;
    procedure SetAsVariant(v: variant); virtual;
    procedure RaiseTypeCastException(v: variant);
    function GetValueType: TDataFieldType;
    procedure SetOwner(owner: TDataObject);
	  function GetTDataFieldChoices: TDataFieldChoices;
    function GetHasChoices: Boolean;

    function CanChange(var value: variant): boolean; virtual;
    function CanExit: boolean; virtual;
    function CanExitField: boolean; virtual;
    function CanSave:boolean; virtual;

  public
    property Owner: TDataObject read FOwner write SetOwner;
      //Keeps track of the Data object that owns the field
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
    constructor CreateSpecial(owner: TDataObject; sName: string); virtual;
    procedure BeforeDestruction; override;
    destructor destroy; override;
    property ValueType: TDataFieldType read GetValueType;
      //Keeps track of what type of value is stored in this field (simply
      //checking the class type was not good enough)
    property Name: string read GetName write SetName;
    property AsVariant: Variant read GetAsVariant write SetAsVariantTemplate;
    property AsString: string read GetAsString write SetAsStringTemplate;
    property AsWebString: string read GetAsWebString write SetAsWebString;
    //controller references
		property DataFieldChoices:TDataFieldChoices read GetTDataFieldChoices;
	  property HasChoices:Boolean read GetHasChoices;
    property MyIndex: integer read FMyIndex write FMyIndex;
    property IsSpecial: boolean read FIsSpecial write FIsSpecial;
    property OnCanChange: TFieldValidationEvent read GetOnCanChange;
    property OnCanExit: TFieldValidationEvent read GetOnCanExit;
    property OnCanSave: TFieldValidationEvent read GetOnCanSave;
    property Changing: boolean read FChanging write Fchanging;
    property TypeName: string read GetTypeName;
    property KeepAsString: boolean read FkeepAsstring write FKeepAsString;
    procedure RevertToDefaultValue;
    property StorageString: string read GetStorageString;
    property QuotedStorageString: string read GetQuotedStorageString;
    //import/export
    property ExportString: string read GEtExportString;
    property Ischanged: boolean read FIsChanged write FIsChanged;
  end;
//#########################################################
  TBooleanDataField = class(TDataField)
  //this class represents a single Boolean value
  protected
    function GetAsString: string; override;
    procedure SetAsString(str: string); override;
    procedure SetAsVariant(v: variant); reintroduce;
    function GetStorageString: string; override;
  public
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
  end;
//#########################################################
  TDateTimeDataField = class(TDataField)
  //this class represents a a single date time value
  protected
    procedure SetAsString(str: string); override;
    procedure SetAsVariant(v: variant); reintroduce;
    function GetAsString: string; override;
  public
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
  end;


  TMYSQLDateTimeDataField = class(TDateTimeDataField)
  protected
    function GetStorageString: string; override;

  end;

  TDateDataField = class(TDataField)
  //this class represents a a single date time value
  protected
    procedure SetAsString(str: string); override;
    procedure SetAsVariant(v: variant); reintroduce;
  public
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
  end;

  TTimeDataField = class(TDataField)
  //this class represents a a single date time value
  protected
    procedure SetAsString(str: string); override;
    procedure SetAsVariant(v: variant); reintroduce;
  public
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
  end;



//#########################################################
  TstringDataField = class(TDataField)
  //this class represents a string value
  protected
    procedure SetAsString(str: string); override;
  public
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
  end;
//#########################################################
  TLongintDataField = class(TDataField)
  //this class represents a 32-bit integer value
  protected
    procedure SetAsString(str: string); override;
  public
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
  end;

  TVariantDataField = class(TDataField)
  //this class represents a 32-bit integer value
  protected
    procedure SetAsString(str: string); override;
    function GetStorageString: string; override;
    function GetQuotedStorageString: string; override;
  public
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
  end;
//#########################################################
  TSmallintDataField = class(TDatafield)
  //this class represents a 16-bit integer value
  protected
    procedure SetAsString(str: string); override;
  public
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
  end;
//#########################################################
  TFloatingPointDataField = class(TDatafield)
  //this class represents a floating point value
  protected
    procedure SetAsString(str: string); override;
  public
    constructor create(owner: TDataObject; rec: TDOFieldDef); reintroduce; virtual;
  end;

  TDataObjectObserver = class(TFakeLockQueuedObject)
  //this class shows although main objects that are currently instantiated
  private
    FList: TStringList;
    FOnUpdate: TNotifyEvent;
    FTotalObjects, FTotalRequests: integer;
    FLastAudit: string;
    function GetCount: integer;
    function GetObject(idx: integer): string;
    function GetTotalObjects: integer;
    function GetTotalRequests: integer;
    function GetLastAudit: string;
  public
    constructor Create; override;
    destructor destroy; override;
    procedure Inc(sName: string; obj: TDataObject);
    procedure Dec(obj: TDataObject);

    procedure TallyRequest(sAuditText: string);
    procedure TallyObject;
    property TotalObjects: integer read GetTotalObjects;
    property TotalRequests: integer read GetTotalRequests;
    property LastAudit: string read GetLastAudit;

    property count: integer read GetCount;
    property Objects[idx: integer]: string read GetObject;
    property OnUpdate: TnotifyEvent read FOnUpdate write FOnUpdate;
  end;


//Global functions
function StripParams(sObjectName: string): string;
function BuildObjectName(sType: string; FParams: variant; DataCenter, DataTier: integer; LengthLimit:integer = 0): string;
function NullMethod: TMethod;
function NullFieldValidationEvent: TFieldValidationEvent;

var
  DOOB: TDataOBjectObserver;

//#########################################################
//#########################################################

function CreateField(var rec: TDOFieldDef;owner: TDataObject): TDatafield;

implementation
uses
  {$IFDEF DEBUG}
  {$ENDIF}
  Dataobjectcache, serverinterfaceinterface,
  Exceptions, DataObjectFactory, DataObjectServices;

//------------------------------------------------------------------------------

//#########################################################
constructor TBooleanDataField.create;
begin
  inherited;
end;
//----------------------------------------------------------
procedure TBooleanDataField.SetAsVariant(v: variant);
//Reintroduced code that sets the boolean value
begin
  if varType(v) = varSmallint then begin
    if v=0 then self.AsVariant := true else AsVariant := false;
  end else
    owner.FFieldDefs[FMyIndex].Value := v;
end;

//#########################################################
procedure TDateTimeDatafield.SetAsString(str: string);
begin
  if str = '' then
    Asvariant := TDateTime(NULL_DATETIME)
  else
    AsVariant := strtodateTime(str);
end;

//#########################################################
procedure TDateTimeDataField.SetAsVariant(v: variant);
begin
  If vartype(AsVariant) = varNull then
    v := 0.0;

  inherited;

end;
constructor TDateTimeDataField.create(owner: TDataObject; rec: TDOFieldDef);
begin
  inherited;
  AsVariant := now;
end;
//#########################################################
constructor TLongintDataField.create;
begin
  inherited;
  AsVariant := 0;
end;
//#########################################################
function TDataObjectToken.GetName: string;
//var
//  t: integer;
//  i: integer;
begin
(*  result := TypeName;

//if not ((varType(params[t]) and varArray) = varArray) then
//  FParams := VarArrayOf([FParams]);

  for t:= 0 to ParamCount-1 do begin
    case vartype(Params[t]) of
      varNull: result := result;
      varBoolean: if params[t] then
                    result := result + '#TRUE'
                  else
                    result := result +'#FALSE';
      varinteger : result := result + '#' + inttostr(Params[t]);
      varstring  : result := result + '#' + Params[t];
      varOleStr  : result := result + '#' + Params[t];
      varDate  : result := result + '#' + vartostr(Params[t]);
    else
      //i:= varType(Params[t]);
      //raise EDataObjectError.CreateFmt('Variant type not supported. Type: %d', [i]);
    end;
  end;*)
  if FObjectName = '' then begin
    FObjectName := BuildObjectName(TypeName, VariantParams, DataCenter, DataTier);
  end;

  result := FObjectName;

end;
//------------------------------------------------------------------------------
constructor TDataObjectToken.create;
begin
  InterlockedIncrement(tokencount);
  inherited Create;

  iParamIndex := 0;
  FParams := null;


end;
//------------------------------------------------------------------------------
destructor TDataObjectToken.destroy;
begin
  InterlockedDecrement(tokencount);
  inherited;
end;
//------------------------------------------------------------------------------
function TDataObjectToken.AddParam(param: variant; bNoDeregister: boolean = false): boolean;
var
  t: integer;
  iHigh, iLow : integer;
begin
  FObjectName := '';
  if (Owner<> nil) and (not bNoDeregister) then
    Owner.DeregisterWithCache;

  result := true;

  if vartype(FParams) = varNull then
    FParams := VarArrayCreate([0,0], varVariant);
  if (VarType(param) and varArray)= varArray then begin
    iHigh := VarArrayHighBound(param, 1);
    iLow := VarArrayLowBound(param, 1);
    //check if null array was passed
    if iHigh < iLow then
      exit;

(*      //if param index is out of bounds then realloc variant array
    if ((iParamIndex) + (iHigh-iLow)+1) > VarArrayHighBound(FParams,1) then begin
      VarArrayRedim(Fparams, iParamIndex+(ihigh-iLow));
    end;*)
    for t:= iLow to iHigh do begin
      AddParam(param[t]);
(*        if ((varType(param[t]) and (varNull or varArray)) = 0) then
        FParams[iParamIndex] := param[t]
      else
        FParams[iParamIndex] := null;

      inc(iParamIndex);*)
    end
  end
  else begin
    //if param index is out of bounds then realloc variant array
    if (iParamIndex) > VarArrayHighBound(FParams,1) then begin
      VarArrayRedim(FParams, iParamIndex);
    end;
    FParams[iParamIndex] := param;
    inc(iParamIndex);
  end;
  Result := true;

  if (Owner<> nil) and (not bNoDeregister) then
    Owner.RegisterWithCache;
end;
//---------------------------------------------------------
function TDataObjectToken.GetVariantParams: variant;
begin
  result := FParams;
end;

//---------------------------------------------------------
function TDataObjectToken.GetParamCount: integer;
var
  iVarType: integer;
begin

  iVarType := VarType(FParams);
  if NOT ((iVarType and varArray) = varArray) then begin
    if iVarType = varNull then
      result := 0
    else
      result := 1;

    exit;
  end;

  result := VarArrayhighbound(FParams, 1)+1;
end;
//---------------------------------------------------------
function TDataObjectToken.GetParam(idx: integer): variant;
begin
  if NOT ((VarType(FParams) and varArray) = varArray) then begin
    result := FParams;

  end
  else begin
    //Raise exception if index is out of range
    if idx < 0 then
      raise EDataObjectError.createFMT(
        sErrOutOfBounds, ['PARAMS', 'LOW', 0, idx]);

    //Return variant at index in array
    result := FParams[idx];
  end;
end;

//------------------------------------------------------------------------------
function TDataObjectToken.GetTypeName: string;
begin
//  if FTypeConst = -1 then
//    result := ''
//  else
    result := FType.ClassName;
end;
//------------------------------------------------------------------------------
procedure TDataObjectToken.SetTypeName(sTypeName: string);
begin
  FObjectName := '';
  FType := DOCF.GetClassTypeForClassName(sTypeName);

end;


//#########################################################
constructor TSmallIntDataField.create;
begin
  inherited;
  AsVariant := 0;
end;

//#########################################################
constructor TstringDataField.create;
begin
  inherited;
  AsVariant := '';
end;


//#########################################################
//#########################################################
//---------------------------------------------------------
constructor TDataObject.create(aOwner: TObject; params: variant; ACache: TObject; extended: TExtendedDOVars);
begin
  inherited Create;
  FSpecialFieldDefs := nil;
  FCAche := ACache;
  FOrphaned := false;
  FOrigin := dorFetch;
  iValFieldDefCount := 0;
  iExtFieldDefCount := 0;

  //Field count for array (siaze of array is independent of field count)
  FFieldCount := 0;

  //Flag initialization
  bDODestroying := false;

  //Standard field initialization
  FCreator := aOwner;
  FReferences := 0;

  //Create Aggregate token
  FToken := TDataObjectToken.create;
  //FToken := nil;
  FToken.TypeName := self.className;
  FToken.AddParam(params);
  FToken.Owner := self;


  //NOTE!: These lists are created upon demand by their related properties
  //in an attempt to save memory.
  slAssociates := nil;
  lstObjects := nil;
  lstNotifyObjects := nil;
  lstSpecialFields := nil;
  lstNotifyobjects := nil;
  lstNotifyObjects := TBetterList<TDataObject>.create;

  //Init controller communications lists
  lstCoordinators := nil;
//  lstControls := nil;

  token.DataCenter := extended.DataCenter;
  token.DataTier := extended.DataTier;
  sessionid := extended.SessionID;

  FGenesis := GetTicker;

  initDefinition;

  RegisterWithCache;

  reference;





end;
//---------------------------------------------------------
(*function TDataObject.ObjAddRef: Integer;
//Description: Handles increasing reference count for the Data object.
//Returns: Number of references being used after addition.
begin
  Result := InterlockedIncrement(FComRefCount);

  {$IFDEF DODEBUG}LogMessage(self.name+' +COMREF '+inttostr(FComRefCount));{$ENDIF}


  {$IFDEF DODEBUG}
    if FComRefCount<FDORefCount then
      LogMessage(Self.name+' COM REFERENCE COUNT IS LESS THAN DO REFERENCE COUNT');
  {$ENDIF}
end;
//---------------------------------------------------------
(*function TDataObject.ObjRelease: integer;
//Description: Handles decreasing reference count for the Data object.
//Returns: Number of references being used after decrement.
begin
  Result := InterlockedDecrement(FComRefCount);
  {$IFDEF DODEBUG}
  LogMessage(self.name+' -COMREF '+inttostr(FComRefCount));
  if FComRefCount<FDORefCount then
    LogMessage(Self.name+' COM REFERENCE COUNT IS LESS THAN DO REFERENCE COUNT');
  {$ENDIF}
  if FComRefCount = 0 then Destroy;
//  if Result = 0 then release;
end;
//---------------------------------------------------------
function TDataObject._AddRef: integer;stdcall;
//Description: Exposes the _AddRef function so that it can be called manually
//by objects, not using interfaces.
//Returns: Number of references in use
begin
  result := inherited _AddRef;
end;*)
//---------------------------------------------------------
(*function TDataObject._Release: integer;stdcall;
//Description: Exposes the _Release function so that it can be called manually
//by objects, not using interfaces.
//Returns: Number of references in use
begin
  result := inherited _Release;
end;*)
//---------------------------------------------------------
destructor TDataObject.destroy;
//Description: Destroys the object, all fields it contains, and releases
//a reference to any objects it points to.
begin

  {$IFDEF DODEBUG}LogMessage(self.name+' DESTROY ComRefs:'+inttostr(FComRefCount)+' DORefs:'+inttostr(FDORefCount));{$ENDIF}

//    Auditlog('Destroy:'+self.Name, 'cache');
    bDODestroying := true;

(*    if Owner is TDataObjectFactory then
      with Owner as TDataObjectFactory do begin
        DeRegisterDataObject(self);
      end
    ;*)

    //windows.beep(100,100);
    //DOOB.Dec(self);

//    AuditLog('Destroying ' +self.name, 'bg');
//    AuditLog('Detach ' +self.name, 'bg');

    Detach(true);

//    AuditLog('Rest of free ' +self.name, 'bg');
    //windows.beep(100,100);

    //list cleanup
    lstObjects.free; //FREED BY THREAD
    slAssociates.free;

    (*while lstSpecialFields.Count > 0 do begin
      TDataField(lstSpecialFields[0]).free;
      lstSpecialFields.delete(0);
    end;*)


    lstSpecialFields.free;
    lstObjects := nil;
    slAssociates := nil;
    FSpecialFieldDefs.Free;
    FSpecialFieldDefs := nil;
    lstSpecialFields := nil;


    //windows.beep(100,100);

    {$IFNDEF NOFINALIZE}
    Finalize(FFieldDefs);
    Finalize(FValFieldDefs);
    Finalize(FExtFieldDefs);
    setlength(FFieldDefs,0);
    setlength(FValFieldDefs,0);
    setlength(FExtFieldDefs,0);
    {$ENDIF}


    DeRegisterWithCache;

    //Free the token
    FToken.Free;
    FToken := nil;



    inherited;
end;

procedure TDataObject.BeforeDestruction;
begin
    //This should ALWAYS be the case.. and the call to FREE or DESTROY, should
  //ALWAYS come from _Release.
//  windows.beep(1000,10);
//  DeRegisterWithCache;
//  windows.beep(500,10);


  inherited;
end;

//------------------------------------------------------------------------
function TDataObject.GetIFld(sName: string): TDataField;
begin
  result := GetTFld(sName);
end;
//------------------------------------------------------------------------------
function TDataObject.GetTFld(sName: string): TDataField;
var
  i : integer;
begin
  //Process special fields
  if IsSpecialField(sName) then begin
    result := GetSpecialField(sName);
    exit;
  end;

  i := IndexOfField(sName);

  if i = -1 then
    raise EDataObjectError.CreateFMT('Field %s undefined in %s', [sName, self.ClassName]);

  result := GetTFldByIndex(i);

//

end;
//------------------------------------------------------------------------------
function TDataObject.IndexOfField(sName: string): integer;
var
  t: integer;
begin
  result := -1;
  for t:= 0 to FieldCount - 1 do begin
    if lowercase(FFieldDefs[t].Name) = lowercase(sName) then begin
      result := t;
      exit;
    end;
  end;

end;
//------------------------------------------------------------------------------
function TDataObject.GetFieldCount: integer;
begin
  result := FFieldcount;
end;
//------------------------------------------------------------------------------
function TDataObject.GetCaption: string;
begin
  result := self.Token.Name;
end;
//------------------------------------------------------------------------------
//procedure TDataObject.AddFieldDef(sName: string; FieldType: TDataFieldClass; vInitialValue: variant; Method: TMethod;

//---------------------------------------------------------
function TDataObject.GetO(vNameOrIndex: variant): TDataobject;
begin
  case vartype(vNameOrIndex) of
    varInteger, varInt64, varSmallint, varByte:
      result := obj[vNameOrIndex]
  else
    result := assoc[vNameOrIndex];
  end;
end;

function TDataObject.GetObject(idx: integer): TDataObject;
var
  doTemp: TDataObject;
begin
  InitializeObjectList;

  if idx>ObjectCount-1 then
    raise Exception.create('Object Index out of bounds ('+inttostr(idx)+') in '+self.name);

  doTemp := nil;
  //if the object is is a TOKEN (never instantiated) then instantiate it
  if NOT (TObject(lstObjects[idx]) is TDataObject) then begin
    if InstantiateToken(doTemp, lstObjects[idx] as TDataObjectToken, sessionid) then begin
      TObject(lstObjects[idx]).Free;
      doTemp.Reference;
      lstObjects[idx]  := doTemp;
      result := doTemp;

    end else
      raise Exception.create('Unable to instantiate token: '+TDataObjectToken(lstObjects[idx]).Name);

  end
  //if the object is not nil then simply return the object associated with the string
  else begin
    result := TDataObject(lstObjects[idx]);
  end;

end;
//---------------------------------------------------------
function TDataObject.GetAssociateByIndex(idx: integer): TDataObject;
//Returns an "associate" object, based on an ordinal index.
//Raises exception if the associate could not be instantiated.
//Important
var
  token: TDataObjectToken;
  doTemp: TDataObject;
begin
  if (idx<0) or (idx > slAssociates.count-1) then begin
    result := nil;
    exit;
  end;

  InitializeAssociateList;

  result := nil;
  doTemp := nil;

  //if the associate definition at given index is of type '' then defer to
  //DoGetAssociate
  if (
    (NOT (slAssociates.objects[idx] is TDataObject))
  ) then begin
    result := DoGetAssociate(slAssociates[idx]);

    //if fetched then tag the token to indicate that the associate has been fetch9ed
    if slAssociates.objects[idx] <> nil then begin
      //TDataObjectToken(slAssociates.objects[idx]).variantParams := '1';
      TDataObjectToken(slAssociates.objects[idx]).free;
    end;
    slAssociates.objects[idx] := result;

    exit;

  end;


  // If INDEX outside ARRAY boundaries, just leave  //Todo:Raise exception
  if (idx >= slAssociates.Count) or (idx < 0) then begin
    Result :=nil;
    exit;
  end;

  //if the object is nil then instantiate
  if NOT (TObject(slAssociates.objects[idx]) is TDataObject) then begin
    if InstantiateToken(doTemp, (TDataobjectToken(slAssociates.objects[idx])), sessionid) then begin
      if not (doTemp = nil) then begin
        //free the old token
        token := TDataObjectToken(slAssociates.objects[idx]);
        try
          token.FreeWithReferences;
        except
        end;
        //replace with class factory result
        slAssociates.objects[idx]  := doTemp;
        result := doTemp;
                //fetched.  It is currently not done this way because the server is
        //not implemented properly for somem objects.. causeing the DOExplore
        //program to raise too many exceptions.

        //Increment the reference count of the object.
        doTemp.Reference;
      end;
    end

  end
  //if the object is not nil then simply return the object associated with the index
  else begin
    result := TDataObject(slAssociates.objects[idx]);
  end;
end;
//---------------------------------------------------------
function TDataObject.GetAge: cardinal;
begin
  result := GetTimeSInce(GetTicker, Genesis);

end;

function TDataObject.GetAssociate(sName: string): TDataObject;

var
  iObjectIndex: integer;
  t: integer;
begin
  InitializeAssociateList;

  //Find the index to the object in the instantiated object list
  iObjectIndex := -1;
  for t:= 0 to AssociateCount - 1 do begin
    if lowercase(slAssociates[t]) = lowercase(sName) then begin
      iObjectIndex := t;
    end;
  end;


  //Defer to GetAssociateBYIndex
  result := GetAssociateByIndex(iObjectIndex);
//  result := DoGetAssociate(sName);


  if result = nil then
    raise exception.create('Associate '+sName+' was not found for object '+self.name+'.');

end;

//---------------------------------------------------------
function TDataObject.IndexOfObjectInList(
    sObjectName: string; list: Tlist):integer;
//(protected)
//Returns -1 if object is not in list
var
  t: integer;
begin

  //init result
  result := -1;
  //Scan list for an object with the given name
  for t:= 0 to list.Count-1 do begin
    if obj[t].Name = sObjectName then
      result := t;
  end;

end;
//---------------------------------------------------------
function TDataObject.GetObjectCount: integer;
//(protected)
begin
  //optimized for data size: result is 0 if object list is never created
  if assigned(lstObjects) then
    result := lstObjects.count
  else
    result := 0;
end;
//---------------------------------------------------------
function TDataObject.GetAssociateCount: integer;
//(protected)
begin
  //optimized for data size: result is 0 if object list is never created
  if assigned(slAssociates) then
    result := slAssociates.count
  else
    result := 0;

end;
//---------------------------------------------------------
procedure TDataObject.Reference;
begin
  _AddRef;
end;
//---------------------------------------------------------
procedure TDataObject.Release;
begin
  _Release;
end;

//---------------------------------------------------------
procedure TDataObject.AddObjectToken(obj: TDataObjectToken);
begin
  InitializeObjectList;



  lstObjects.Add(TObject(obj));

(*  //increase size of array if needed (allocated in BLOCKS for efficiency)
  if (iObjectCount mod DYN_ARRAY_BLOCK_SIZE) = 0 then begin
    SetLength(lstObjects, ((iObjectCount+1)*10));
  end;

  //Assign the array item
  lstObjects[iObjectCount] := obj;

  //Increment count
  inc(iObjectCount);*)

end;
//---------------------------------------------------------
procedure TDataObject.AddObject(obj: TDataObject);
var
  bHandledByChild: boolean;
begin
  InitializeObjectList;

  DoAddObject(obj, bHandledByChild);

  if not bHandledByChild then begin
    lstObjects.Add(TObject(obj));
    obj.Reference;
  end;


end;
procedure TDataObject.ClearObjects;
var
  obj: TDataObject;
begin
  //if the list was never created then just exit
  try
    if not assigned(lstObjects) then
      exit;

    while lstObjects.Count > 0 do begin
      obj := lstobjects[lstObjects.count-1] as TDataObject;
      lstObjects.Delete(lstObjects.count-1);
      //obj.Release;
    end;
  except
    on E:Exception do begin
      raise Exception.create(self.name+' '+e.Message);
    end;
  end;
end;
//---------------------------------------------------------
function TDataObject.InstantiateToken(var obj: TDataObject; Token: TDataObjectToken; iSessionID: integer): boolean;
//(protected)
var
  vParams: variant;
//  t: integer;
begin
  if not (Creator is TDataObjectFactory) then begin
    raise EArchitecturalInconsistency.createFMT(
      'Data object was not created with class factory. '+
      'Unable to access associated objects.',[]);
  end;

  with Creator as TDataObjectFactory do begin
    with token do begin
      vParams := token.VariantParams;
    end;

    result := IServerInterface(TDataObjectCache(cache).server).LazyFetch(TDataObjectCache(Cache), obj, Token.TypeName, vParams, iSessionID) ;
    //note: lazy fetch will add a reference as appropriate to the object
  end;

end;
//------------------------------------------------------------------------------
procedure TDataObject.AddAssociate(sName: string; sTypeName: string; params: variant);
//Adds a link to an "associated" Data object... e.g. User - > Assignment
//sName - is the friendly name by which it will be indexed and accessed via
//the "assoc" property
//sTypeName - is the string type name of the associate e.g "TdoUserActList"
//params - parameters that must be passed to instantiate the associate upon
//demand.  This may be a single parameter or a variant array.
var
  token : TDataObjectToken;
  iTemp: integer;
begin
  InitializeAssociateList;


  iTemp := slAssociates.IndexOf(sName);
  //if the entry does not exist then add a token under the name
  if iTemp = -1 then begin
    slAssociates.Add(sName);
    iTemp := slAssociates.count-1;
    token := TDataObjectToken.create;
    slAssociates.objects[iTemp] := token;
    token.TypeName := sTypeName;
    token.AddParam(params);
  end
  //else use the existing token entry and refresh its params
  else begin
    //if a token was in the list, then refresh the properties of the token
    if slAssociates.Objects[iTemp] is TDataObjectToken then begin
      token := TDataObjectToken(slAssociates.Objects[iTemp]);
      token.TypeName := sTypeName;
      token.AddParam(params);
    end
    //if a Data object was in the list, then replace the Data object with a new token.
    else begin
      token := TDataObjectToken.create;
      slAssociates.objects[iTemp] := token;
      token.TypeName := sTypeName;
      token.AddParam(params);
    end;
  end;

end;
//------------------------------------------------------------------------------
function TDataObject.GetIFldByIndex(idx: integer): TDataField;
begin
  result := GetTFldByindex(idx);
end;

function TDataObject.GetInsertQuery: string;
var
  t: ni;
  ColumnList: string;
const
  BL = '';//would be brackets on MSSQL
  BR = '';//would be brackets on MSSQL
begin
  ColumnList := '';
  for t:= SkipInsertKeycount to high(FKeys) do begin
    ColumnList := ColumnList + BL+self.Fkeys[t]+BR+', ';
  end;

  for t:= 0 to fieldCount-1 do begin
    ColumnList := ColumnList + BL+self.fieldByIndex[t].Name+BR;
    if t < fieldcount-1 then
      ColumnList := ColumnList + ', ';
  end;

  result := 'insert into '+FTableLink+' ('+ColumnList+') ( select ';

  for t:= SkipInsertKeycount to high(FKeys) do begin
    result := result + vartostr(token.params[t])+', ';
  end;

  for t:= 0 to fieldCount-1 do begin
    result := result + self.fieldByIndex[t].QuotedStorageString;
    if t < fieldcount-1 then
      result := result + ', ';
  end;
  result := result + ')';

end;

//------------------------------------------------------------------------------
function TDataObject.GetTFldByIndex(idx: integer): TDataField;
begin
  if FFieldDefs[idx].Instance = nil then begin
    FFieldDefs[idx].Instance := FFieldDefs[idx].ClassVar.create(self, FFieldDefs[idx]);

    //Cause the field to go away with COM references
//    FFieldDefs[idx].Instance.FreeAtWill;
  end;

  result := FFieldDefs[idx].Instance;

end;
//------------------------------------------------------------------------------
function TDataObject.GetName: string;
//this is put here to prevent people from confusing the component name with the
//Data object name stored in TOKEN
begin
  if token= nil then begin
    result := '';
    exit;
  end;
  result := token.name;
end;
//------------------------------------------------------------------------------
function TDataObject.GetIToken: TDataObjectToken;
begin
  result := GetTToken;
end;
function TDataObject.GetKeyCount: ni;
begin
  result := length(FKeys);
end;

function TDataObject.GetKeyUpdateQuery: string;
begin
  if FAutoKeyField <> '' then
    result := 'select max('+FAutoKeyfield+') from '+FAutoKeyTable
  else
    result := '';

end;

//------------------------------------------------------------------------------
function TDataObject.GetTToken: TDataObjectToken;
begin
  if FToken = nil then
    raise Exception.create(self.classname+' was improperly constructed. @'+inttohex(integer(pointer(self)),8));
  result := FToken;
end;

//------------------------------------------------------------------------------
function TDataObject.GetUpdateQuery: string;
var
  t: ni;
  sfield: string;
  sHeader: string;
  sFields: string;
  sFooter: string;
  bNotFirst:boolean;
begin
  sHeader := 'Update '+TableLink+' set ';
  sfield := '';
  bNotFirst := false;
  for t:= 0 to fieldcount-1 do begin
    if fieldbyindex[t].ischanged then begin
      sField := fieldbyindex[t].Name+'='+fieldbyindex[t].QuotedStorageString;
      if bNotFirst  then
        sfields := sfields + ',' + sfield+NEWLINE
      else
        sfields := sField+NEWLINE;
      bNotFirst := true;
    end;
  end;
  sFooter := filterPhrase;
  result := sHeader+NEWLINE+sFields+NEWLINE+sFooter;



end;

//---------------------------------------------------------
function TDataObject.save(bIncludeChangedAssociates: boolean = false): boolean;
//Description: Saves the Data object to the server (simply automaticall
//passes itself to the sever interface
//Returns true if successful... false otherwise.
var
  t: integer;
begin
  if IsOrphaned then
    raise ECritical.create(token.name+' is an orphaned object and cannot be saved.');

  //If everything is validated then post
  if CanSave then begin
    BeforeSave;
    //Setup keys if not done already
    if IsGhost then New(sessionid);
    //post
    result := IServerInterface(TDataObjectCache(cache).server).Post(self, sessionid);

    if bIncludeChangedAssociates then begin
      for t:= 0 to associateCount-1 do begin
        if self.IsAssociateInstantiated(t) then
          result := AssocByIndex[t].Save;
      end;
    end;

    if result then AfterSave;

  end else
    result := false;

end;
//---------------------------------------------------------
function TDataObject.CanExitField(field: TDatafield; var value: variant): boolean;
//NOT INTERFACED
begin
  result := true;
end;


//#########################################################
//#########################################################
constructor TDatafield.create(owner: TDataObject; rec: TDOFieldDef);
var
  obj : TDataObject;
  extRec: TExtDOFieldDef;
begin

  inherited Create;
  //windows.beep(1000,10);
  FDataFieldChoices := nil;
  FOwner := owner;
  FMyIndex := owner.IndexOfField(rec.name);

  if rec.ExtIndex>-1 then begin
    extRec := owner.FExtFieldDefs[rec.Extindex];
    IServerInterface(TDataObjectCache(owner.cache).server).LazyFetch(TDataObjectCache(owner.FCache), obj, extrec.LookupObj, extrec.LookupVars, owner.sessionid);
    DataFieldChoices.DataObject := obj;
    DataFieldChoices.DisplayField := extrec.displayField;
    DataFieldChoices.IndexField := extrec.IndexField;
  end;

  FIsSpecial := false;
  FSpecialName := '';
  FChanging := false;
  FSpecialValue := '';

end;

//---------------------------------------------------------
destructor TDatafield.destroy;
begin


  inherited;

end;
//---------------------------------------------------------
procedure TstringDataField.SetAsString(str: string);
begin
  AsVariant := str;
end;
//---------------------------------------------------------
procedure TLongintDatafield.SetAsString(str: string);
var
  t: integer;
  s: string;
  bIsDash: boolean;
  bIsAbove0: boolean;
  bIsBelow9: boolean;
begin
  if (str ='') or (str = '-') then begin
    AsVariant := 0;
    exit;
  end;

  if (compareText(str, 'True')=0) then begin
    AsVariant := 1;
    exit;
  end;

  if (compareText(str, 'False')=0) then begin
    AsVariant := 0;
    exit;
  end;

  s := str;
  for t:= 1 to length(s) do begin
    bIsDash := ord(s[t]) = 45;
    bIsAbove0 := ord(s[t]) >=48;
    bIsBelow9 := ord(s[t]) <=57;

    if not bIsDash and not (bIsAbove0 and bIsBelow9) then begin
      exit;
    end;

  end;

  try
    AsVariant := longint(strtoint(str));
  except
  end;
end;
//---------------------------------------------------------
procedure TSmallintDatafield.SetAsString(str: string);
begin
  AsVariant := smallint(strtoint(str));
end;
//---------------------------------------------------------
procedure TBooleanDatafield.SetAsString(str: string);
begin
  if (str = '1') or (uppercase(copy(str,1,1))='T') or (uppercase(copy(str,1,1))='Y') then
    AsVariant := true
  else
  if (str = '0') or (uppercase(copy(str,1,1))='F') or (uppercase(copy(str,1,1))='N') then
    AsVariant := false
  else
    raise EDataFieldError.create('Unregocnized boolean '''+str+'''');
end;
//---------------------------------------------------------
procedure TDataField.SetAsStringTemplate(str: string);
//Description: This is a template method for the AsString property.  It calls
//SetAsString, but assures that necessary things happen before and/or after
//the call to SetAsString.
var
  bLocalchange: boolean;
  sOld: string;
begin
//If self hasChoices then translate str from the display value into the index
//value.

  bLocalChange := false;
  //translate'RoleName administrator' into 'RoleId 2'
  //DoTemp:= Self.DataFieldChoices.DataObject;
  try
    bLocalchange := not changing;
    if not changing then begin
      changing := true;
    end;

    If HasChoices then
      Str:= Self.DataFieldChoices.IndexOf(Str);

    sOld := AsString;
    SetAsString(str);

    if not FInAfterWrite then begin
      FInAfterWrite := true;
      try
        owner.AfterWriteField(self);
      finally
        FInAfterWrite := false;
      end;
    end;

    if sOld <> str then begin
      Ischanged := true;
      owner.IsChanged := true;
    end;

  finally
    if bLocalchange then
      changing := false;
  end;
end;
//------------------------------------------------------------------------------
function TDataField.GetAsString: string;
var
  v: variant;
begin
  try
    //showmessage(self.classname);
    if not changing then begin
      changing := true;
      try
        self.Owner.BeforeReadField(self.myIndex);
        self.Owner.BeforeReadField(self.name);

      finally
        changing := false;
      end;
    end;

    v:= AsVariant;


//result := '';

    if ((VarType(v) = varBoolean) or (self is TBooleanDataField)) then
      if AsVariant then
        result := 'true'
      else
        result := 'false'
    else
      if HasChoices then
        result := DataFieldChoices.ValueOf(VarToStr(v))
      else
        if Vartype(v) = varNull then begin
          if self.ValueType = dftString then
            result := ''
          else
            result := '0';
        end else
          result := vartoStr(v);



  except
    raise Exception.Create('DataField read error reading '+ self.Name+ ': '+Exception(ExceptObject).Message)
  end;
end;
//------------------------------------------------------------------------------
function TDatafield.GetAsVariant: variant;
begin
  if not changing then begin
    changing := true;
    try
      self.Owner.BeforeReadField(self.MyIndex);
      self.Owner.BeforeReadField(self.name);
    finally
      changing := false;
    end;
  end;

  if KeepAsString then
    result := FSpecialValue
  else begin
    //if VArType(owner.FFieldDefs[FMyIndex].Value) = varNull then
    //  raise Exception.create('Illegal value detected in '+TDataObject(owner).name+'->'+self.name);
    result := owner.FFieldDefs[FMyIndex].Value;
  end;

end;
//------------------------------------------------------------------------------
procedure TDataField.SetAsVariantTemplate(v: variant);
//Description: This is a template method for the AsString property.  It calls
//SetAsString, but assures that necessary things happen before and/or after
//the call to SetAsString.
var
  b: boolean;
  bLocalchange: boolean;
  vOldV: variant;
begin
  //change the changing setting locally if not already changing
  bLocalchange := not Changing;
  if not changing then begin
    changing := true;
  end;

  try
    b:=CanChange(v);


    if b then begin
      vOldV := Asvariant;
      SetAsVariant(v);

      if not VarSame(vOldV, v) then begin
        IsChanged := true;
        owner.IsChanged := true;
      end;
    end;
  finally
    if bLocalChange then
      changing := false;
  end;
end;
//------------------------------------------------------------------------------
procedure TDataField.SetAsVariant(v: variant);
//Contrary to popular belief... this routine is >NOT< INTERFACED
begin
  if KeepAsString then begin
    FSpecialValue := v;
    exit;
  end;

  if Vartype(v) = varNull then begin
    exit;
  end;

  //Default handling for various variant types
  case ValueType of
    dftBoolean: begin
                  if (varBoolean and VarType(v)) = varBoolean then
                    owner.FFieldDefs[FMyIndex].Value := v
                  else
                    if (varType(v) = varSmallint) or (varType(v) = varInteger) then begin
                      if v = 0 then
                        owner.FFieldDefs[FMyIndex].Value := false
                      else
                        owner.FFieldDefs[FMyIndex].Value := true;
                    end else
                      RaiseTypeCastException(v);
                end;

    dftLong:    begin
                  if ((varInteger and VarType(v)) = varInteger)
                  or ((varShortint and VarType(v)) = varShortInt)
                  or ((varSmallint and VarType(v)) = varSmallInt)
                  then
                    owner.FFieldDefs[FMyIndex].Value := v
                  else
                    RaiseTypeCastException(v);
                end;

    dftShort:   begin
                  if ((varInteger and VarType(v)) = varInteger)
                  or ((varShortint and VarType(v)) = varShortInt)
                  or ((varSmallint and VarType(v)) = varSmallInt) then
                    owner.FFieldDefs[FMyIndex].Value := Smallint(v)
                  else
                    RaiseTypeCastException(v);
                end;
    dftDouble: begin
                if (((varDouble and VArType(v)) = varDouble) or
                    ((varSingle and VArType(v)) = varSingle)) then
                  owner.FFieldDefs[fMyIndex].Value := v
                else
                  RaiseTypeCastException(v);

            end;
    dftString : begin
                  if ((varString and VarType(v)) = varString) or
                     ((varOLEStr and VarType(v)) = varOLEStr) then
                    owner.FFieldDefs[FMyIndex].Value := v
                  else
                    RaiseTypeCastException(v);//(varString, varType(v));
                end;
    dftDateTime : begin
                  owner.FFieldDefs[FMyIndex].Value := v;
                end;
    dftVariant : begin
                  owner.FFieldDefs[FMyIndex].Value := v;
                end;

    else
      raise Exception.create('Unimplemented or unsupported variant type passed to '+self.classname+' in '+owner.classname);
      RaiseTypeCastException(v);
  end;

end;



//------------------------------------------------------------------------------
procedure TDataField.RaiseTypeCastException(v: variant);
begin
  raise EDataObjectError.create(
    'Value passed to ' +self.name+':'+self.ClassName+
    ' was of unexpected type. (Type '+inttostr(varType(v))+') Object: '+self.Owner.ClassName);


end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function TDataField.GetName: string;
begin
  //If a special field
  //then we need to use the special name stored with the field
  if FIsSpecial then  //Using field directly for optimal performance
    result := FSpecialName
  else
    result := owner.FFieldDefs[FMyindex].Name;

end;
//------------------------------------------------------------------------------
procedure TDataField.SetName(sName: string);
begin
  if FIsSpecial then
    FSpecialName := sName
  else
    raise Exception.create('Cannot set name on a field.  Field not marked with SPECIAL flag.');
end;
//------------------------------------------------------------------------------
function TDatafield.GetValueType: TDataFieldType;
begin
  result := dftUndefined;

  if self.classtype = TstringDataField then
    result := dftString
  else
  if self.classtype = TSmallintDataField then
    result := dftShort
  else
  if self.classtype = TLongintDataField then
    result := dftLong
  else
  if self.classtype = TDateTimeDataField then
    result := dftDatetime
  else
  if self.classtype = TMYSQLDateTimeDataField then
    result := dftDatetime
  else
  if self.classtype = TFloatingPointDataField then
    result := dftDouble
  else
  if self.classtype = TVariantDataField then
    result := dftVariant
  else
  if self.classtype = TBooleanDataField then
    result := dftBoolean;
end;
//------------------------------------------------------------------------------
procedure TDatafield.SetOwner(owner: TDataObject);
begin
  Fowner := owner;
end;
//---------------------------------------------------------
function TDataField.CanExit: boolean;
var
  event: TFieldValidationEvent;
  v: variant;
begin
  result := true;

  v:= AsVariant;
  if assigned(OnCanExit) then begin
    event := OnCanExit;
    event(self, v, result);
  end;

  if result then begin
    result := owner.CanExitField(self, v);
    AsVariant := v;
  end;



end;
//---------------------------------------------------------
//Build
function BuildObjectName(sType: string; FParams: variant; DataCenter, DataTier: integer; LengthLimit:integer = 0): string;
var
  t: integer;
  iTemp: integer;
begin
  if varType(FParams) = varNull then begin
    result := sType+'#'+inttostr(DataCenter)+'#'+inttostr(DataTier);
    exit;
  end;

  result := sType;
  result := result + '#'+inttostr(DataCenter)+'#'+inttostr(DataTier);

  if (varType(FParams) and varArray) = varArray then begin
      iTEmp := VarArrayHighBound(FParams, 1);
      if (iTemp >= 0) and (iTemp-VarArrayLowBound(FParams,1) >= 0) then begin
        for t:= VarArrayLowBound(FParams,1) to VarArrayHighBound(FParams,1) do begin
          if (not (VarType(FParams[t])=varNull)) and (not VarIsEmpty(FParams[t])) then
            result := result +'#'+varToStr(FParams[t]);
        end;
      end
  end
  else
    result := result + '#' + VarToStr(FParams);

  if (LengthLImit > 0) and (Length(result)>LengthLimit) then begin
    result := copy(result, 1, lengthlimit)+'...';
  end;
end;
//------------------------------------------------------------------------------
function BuildObjectNameWithTypeID(iType: integer; FParams: variant; DataCenter, DataTier: integer; LengthLimit:integer = 0): string;
var
  t: integer;
  iTemp: integer;
begin
  if varType(FParams) = varNull then begin
    result := inttostr(iType)+'#'+inttostr(DataCenter)+'#'+inttostr(DataTier);
    exit;
  end;

  result := inttostr(iType);
  result := result + '#'+inttostr(DataCenter)+'#'+inttostr(DataTier);

  if (varType(FParams) and varArray) = varArray then begin
      iTEmp := VarArrayHighBound(FParams, 1);
      if (iTemp >= 0) and (iTemp-VarArrayLowBound(FParams,1) >= 0) then begin
        for t:= VarArrayLowBound(FParams,1) to VarArrayHighBound(FParams,1) do begin
          if (not (VarType(FParams[t])=varNull)) and (not VarIsEmpty(FParams[t])) then
            result := result +'#'+varToStr(FParams[t]);
        end;
      end
  end
  else
    result := result + '#' + VarToStr(FParams);

  if (LengthLImit > 0) and (Length(result)>LengthLimit) then begin
    result := copy(result, 1, lengthlimit)+'...';
  end;
end;

//------------------------------------------------------------------------------
function StripParams(sObjectName: string): string;
//Removes all Params from an object name (everything after the first #)
var
  i: integer;
begin
  //find the position of the first '#'
  i := pos('#', sObjectName);

  //if not found then return the same string passed
  if i<1 then begin
    result := SObjectName;
    exit;
  end;

  //calulate result
  result := copy(sObjectName, 1, i-1 {length = pos -1});

end;

function TDataObject.New(iSessionID: integer): boolean;
//Template Method for Creating New Data Objects
//NOTE: DoNew is ABSTRACT
begin
  //Remove from cache to not confuse names
//  DeRegisterWithCache; <--commented setting token params reregisters
  result := DoNew(iSessionID); //ABSTRACT
  IsNew := true;
  //Re-add to cache
//  RegisterWithCache; <--commented setting token params reregisters

end;

procedure TDataObjectToken.SetParam(idx: integer;
  const Value: variant);
var
  iLowBound: integer;
begin
  FObjectName := '';
  if Owner<> nil then
    Owner.DeregisterWithCache;

  if ParamCount <= 1 then
    FParams := Value
  else
  begin
    while idx>=ParamCount do
      AddParam(idx, true);

    iLowBound := VarArrayLowBound(FParams, 1);
    FParams[iLowBound+idx] := Value;

  end;


  if Owner<> nil then
    Owner.RegisterWithCache;

end;

function TDataField.GetTDataFieldChoices: TDataFieldChoices;
begin
	If FDataFieldChoices = nil then begin
		FDataFieldChoices:=TDataFieldChoices.Create;
	end;
	Result:=FDataFieldChoices;
end;

function TDataField.GetHasChoices: Boolean;
begin
	Result:=(Not (FDataFieldChoices=Nil));
end;

{ TDataFieldChoices }
//------------------------------------------------------------------------------
procedure TDataFieldChoices.BeforeDestruction;
begin
  FDataObject := nil;
  inherited;
end;
//------------------------------------------------------------------------------
constructor TDataFieldChoices.Create;
//Try
begin
	inherited;
	FDataObject:=nil;
  FDisplayField := '';
  FindexField := '';

end;
//------------------------------------------------------------------------------
destructor TDataFieldChoices.Destroy;
//Try
begin
  inherited;
end;
//------------------------------------------------------------------------------

function TDataFieldChoices.GetChoiceCount: integer;
//
begin
	Result:=DataObject.ObjectCount;
end;
//------------------------------------------------------------------------------
function TDataFieldChoices.GetChoices(idx: integer): string;
//
begin
	Result:=DataObject.obj[idx].fld[DisplayField].AsString;
end;
//------------------------------------------------------------------------------
function TDataFieldChoices.GetDisplayField: string;
//Try
begin
  Result:=FDisplayField;
end;
//------------------------------------------------------------------------------
function TDataFieldChoices.GetDataObject: TDataObject;
//Try
begin
	Result:=FDataObject;
end;
//------------------------------------------------------------------------------
function TDataFieldChoices.GetIndexes(idx: integer): string;
//Returns the index values that represent the choices
begin
  Result:=DataObject.obj[idx].fld[IndexField].AsString;
end;
//------------------------------------------------------------------------------
function TDataFieldChoices.GetIndexField: string;
//Returns the name of the field that has the internal values
begin
  	Result:=FIndexField;
end;
//------------------------------------------------------------------------------
function TDataFieldChoices.IndexOf(sDisplayFieldValue: string): string;
//Returns the internal index of a display value
//p: sDisplayFieldValue: the display value you want the index for
var
	TempIndex:integer;
begin
   //pass the display field value, return the index field value.
	Result:='';
	For TempIndex:=0 to Self.ChoiceCount-1 do begin
		If lowercase(sDisplayFieldValue)=lowercase(Self.Choices[TempIndex]) then
			Result:=Self.Indexes[TempIndex];
	end;
	If Result='' then begin
		    result := '';
	end;
end;

//------------------------------------------------------------------------------
procedure TDataFieldChoices.SetDisplayField(const Value: string);
//Try
begin
	FDisplayField:=Value;
end;
//------------------------------------------------------------------------------
procedure TDataFieldChoices.SetDataObject(Value: TDataObject);
//Try
begin
	FDataObject:=Value;
end;
//------------------------------------------------------------------------------
procedure TDataFieldChoices.SetIndexField(const Value: string);
//Try
begin
	FIndexField:=Value;
end;
//------------------------------------------------------------------------------
function TDataObject.GetSaveQuery: string;
begin
  if isnew then
    result := GEtInsertQuery
  else
    result := getUpdateQuery;
end;

function TDataObject.GetSpecialField(sFieldName: string): TDataField;
var
  t: integer;
begin
  //_caption
  //_p0...p10
  //_name

  if NOT assigned(lstSpecialFields) then
    lstSpecialFields := TBetterList<TObject>.create;

  result := nil;

  for t:= 0 to lstSpecialfields.count -1 do begin
    if TDataField(lstSpecialfields[t]).Name = sFieldName then begin
      result := TDataField(lstSpecialfields[t]);
      break;
    end;
  end;

  //if we didn't find the special field already... then create the special field
  //(special fields are created dynamically and contain special system values)
  if not assigned(result) then begin
    result := CreateSpecialField(sFieldName);
//    if assigned(result) then
//      lstSpecialFields.Add(result);

  end;

end;
//------------------------------------------------------------------------------
function TDataObject.IsSpecialField(sFieldName: string): boolean;
begin
  result := (copy(sFieldname, 1, 1) = '_') or HasSpecialFieldDef(sFieldName);

end;
//------------------------------------------------------------------------------
function TDataObject.CreateSpecialField(
  sFieldName: string): TDataField;
var
  iParamNumber : integer;
  vParam: variant;
begin
  result := nil;

  //check against _caption, and create accortion
  if lowercase(sFieldName) = '_caption' then begin
    //create the field
    result := TstringDataField.createSpecial(self,'_caption');
    result.KeepAsString := true;
    //track the field
    //lstSpecialFields.add(result);
    //set the field value
    result.AsString := self.caption;

  end;

  //check against _name, and create accortion
  if lowercase(sFieldName) = '_name' then begin
    //create the field
    result := TstringDataField.createspecial(self, '_name');
    result.KeepAsString := true;
    //track the field
    //lstSpecialFields.add(result);
  end;


  if copy(lowercase(sFieldName),1,2) = '_p' then begin
    iParamNumber := strtoint(copy(sfieldName, 3, length(sFieldName)-2));

    vParam := self.Token.Params[iParamNumber];

    case varType(vParam) of
      varInteger: begin
        //create the field
        result := TLongintDataField.createSpecial(self, sFieldName);
        result.KeepAsString := true;
      end;
      varString: begin
        //create the field
        result := TstringDataField.createSpecial(self, sFieldName);
        result.KeepAsString := true;
      end;
    end;
    //track the field

    //set the field value
    result.AsVariant := vParam;

  end;

  if not assigned(result) then
    result := DoCreateSpecialField(sFieldName);

//  if assigned(result) then
//    lstSpecialFields.add(result);

  if not assigned(result) then begin
    raise exception.create('Invalid special field name: '+sFieldName+'.');
  end;

end;
//------------------------------------------------------------------------------
function TDataFieldChoices.ValueOf(sIndexFieldValue: string): string;
var
	TempIndex:integer;
begin
   //pass the display field value, return the index field value.
	Result:='';
	For TempIndex:=0 to Self.ChoiceCount-1 do begin
		If sIndexFieldValue=Self.Indexes[TempIndex] then
			Result:=Self.Choices[TempIndex];
	end;
	If Result='' then begin
		    result := '';
	end;
end;
//------------------------------------------------------------------------------
procedure TDataField.BeforeDestruction;
begin
  if HasChoices then begin
    try
      FDataFieldChoices.free;
    except
    end;

    FDataFieldChoices := nil;
  end;

  if (FMyIndex>-1) and assigned(owner) then begin
    owner.FFieldDefs[FMyIndex].Instance := nil;
  end;


  inherited;
end;
//------------------------------------------------------------------------------
procedure TDataObject.InitDefinition;
begin
  //no definition required
end;

procedure TDataObject.InitializeAssociateList;
begin

  if not assigned(slAssociates) then
    slAssociates := TStringList.create;
end;
//------------------------------------------------------------------------------
procedure TDataObject.InitializeObjectList;
begin
  if not assigned(lstObjects) then
    lstObjects := TBetterList<TObject>.create;
end;
//------------------------------------------------------------------------------
procedure TDataObject.InitializeControlList;
begin
//  if not assigned(lstControls) then
//    lstControls := TinterfaceList.create;

end;
//------------------------------------------------------------------------------
procedure TDataObject.InitializeCoordinatorList;
begin
  if not assigned(lstCoordinators) then
    lstCoordinators := TinterfaceList.create;

end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
(*procedure TDataObject.AddFieldDef(sName: string;
  FieldType: TDataFieldClass; vInitialValue: variant; LookupObj: cardinal;
  LookupVars: variant; DisplayField, IndexField: string; bCalculatthod: TMethod;*)
//------------------------------------------------------------------------------
procedure TDataObject.SetFieldCapacity(iSize: cardinal);
begin
  SetLength(FFieldDefs, iSize+1);

end;
procedure TDataObject.SEtGenesis(const Value: cardinal);
begin
  FGenesis := Value;
  FExpired := false;
end;

//------------------------------------------------------------------------------
function TDataObject.GetFieldCapacity: cardinal;
begin
  result := length(FFieldDefs);
end;


function CreateField(var rec: TDOFieldDef;owner: TDataObject): TDatafield;
begin
  result := rec.ClassVar.create(owner, rec);
end;

function TDataObject.GetFieldDefs(idx: integer): TDOFieldDef;
begin
  result := FFieldDefs[idx];

end;

function TDataObject.GetfilterPhrase: string;
begin
  result := ReplaceQueryKeys(FfilterPhrase);
end;

constructor TDataField.CreateSpecial(owner: TDataObject;
  sName: string);
var
  rec: TDOFieldDef;
begin
  rec.Name := sName;
  rec.Value := '';
  rec.ClassVar := nil;
  rec.ExtIndex := -1;
  rec.ValIndex := -1;

  Create(owner, rec);
  IsSpecial := true;
  KeepAsString := true;
  Name := sName;
  FChanging := false;
  FOwner.lstSpecialFields.Add(self);



end;
//------------------------------------------------------------------------------
function TDataObject.GetFastValues(idx: integer): variant;
begin
  result := FFieldDefs[idx].Value
end;

//------------------------------------------------------------------------------
procedure TDataObject.SetFastVAlues(idx: integer; Value: variant);
begin
  FFieldDefs[idx].Value := value;
end;
//------------------------------------------------------------------------------
function TDataObject.GetExpired: boolean;
begin
  result := FExpired or (GetTimeSince(GetTicker, FGenesis) > DO_EXPIRE_TIME);
end;

function TDataObject.GetExportString: string;
var
  sl: TStringlist;
  sLine: string;
  t: integer;
begin
  sl := TStringlist.create;
  try
    sl.add('object='+self.token.typename);
    for t:= 0 to token.paramcount-1 do begin
      sl.add('key='+vartostr(token.params[t]));//todo 1: probably should be better
    end;
    sl.add('fields='+inttostr(self.fieldcount));
    for t:= 0 to self.fieldcount-1 do begin
      sl.add(self.fieldbyindex[t].ExportString);
    end;
    sl.add('subobjects='+inttostr(self.objectcount));
    for t:= 0 to self.objectcount-1 do begin
      sl.Add(self.obj[t].ExportString);
    end;
    result := sl.text;
  finally
    sl.free;
  end;



end;

function TDataObject.GetExtFieldCapacity: cardinal;
begin
  result := Length(FExtFieldDefs)
end;



procedure TDataObject.SetExtFieldCapacity(const Value: cardinal);
begin
  SetLength(FExtFieldDefs, Value+1)
end;
//------------------------------------------------------------------------------
function TDataObject.CanChangeField(field: TDataField; var value: variant): boolean;
begin
  //ABSTRACT
  result := true;

end;
//------------------------------------------------------------------------------
function TDataObject.CanSave: boolean;
var
  t: integer;
begin
  //iterate though all fields and ask if they can save
  result := NOT IsOrphaned;

  if result then
    for t:= 0 to FieldCount -1 do begin
      if result then
        result := FieldByIndex[t].CanSave;
    end;
  //endif

end;

function TDataObject.CanSaveField(field: TDataField; var value: variant): boolean;
begin
  //This function is blank.... but should be overriden to implement cross-field
  //save validation
  result := true;

end;

function TDataField.GetOnCanChange: TFieldValidationEvent;
var
  iXIndex: integer;
begin
  if IsSpecial then begin
    result := nil;
    exit;
  end;

  iXIndex := owner.FFieldDefs[FMyIndex].ValIndex;
  if iXIndex > -1 then begin
    result := owner.FValFieldDefs[iXIndex].OnCanChange;
  end else
    result := nil;

end;

function TDataField.GetOnCanExit: TFieldValidationEvent;
var
  iXIndex: integer;
begin
  if IsSpecial then begin
    result := nil;
    exit;
  end;

  iXIndex := owner.FFieldDefs[FMyIndex].ValIndex;
  if iXIndex > -1 then begin
    result := owner.FValFieldDefs[iXIndex].OnCanExit
  end else
    result := nil;

end;

function TDataField.GetOnCanSave: TFieldValidationEvent;
var
  iXIndex: integer;
begin
  if IsSpecial then begin
    result := nil;
    exit;
  end;

  iXIndex := owner.FFieldDefs[FMyIndex].ValIndex;
  if iXIndex > -1 then begin
    result := owner.FValFieldDefs[iXIndex].OnCanSave
  end else
    result := nil;
end;


function TDataField.GetQuotedStorageString: string;
begin
  result := StorageString;
  //if one of the types that CANNOT be passed in QUOTES
(*  if ValueType in [dftBoolean, dftDouble, dftShort, dftLong] then
    result := StorageString
  else
    result := ''''+StorageString+'''';*)

end;

function TDataField.CanChange(var value: variant): boolean;
var
  event: TFieldValidationEvent;
begin
  result := true;

  if Assigned(OnCanChange) then begin
    //Run the OnChange hook
    event := OnCanChange;
    event(self, value, result);
  end;

  if result then
    result := owner.CanChangeField(self, value);

end;

function TDataField.CanSave: boolean;
var
  event: TFieldValidationEvent;
var
  v: variant;
begin
  result := true;

  if Assigned(OnCanSave) then begin
    //Run the OnChange hook
    event := OnCanSave;
    v := Asvariant;
    event(self, v, result);
    AsVariant := v;
  end;
end;
//------------------------------------------------------------------------------
function TDataField.CanExitField: boolean;
begin
  result := CanExit; end;
//------------------------------------------------------------------------------
function TDataObject.GetValFieldCapacity: cardinal;
begin
  result := Length(FValFieldDefs);
end;
//------------------------------------------------------------------------------
procedure TDataObject.SetValFieldCapacity(const Value: cardinal);
begin
  SetLength(FValFieldDefs, value+1);

end;
function TDataObject.SKipInsertKEyCount: ni;
begin
  //ON MSSQL you would skip the identity columns on insert (jan's stuff)
  //but in this case, MYSQL, we're using pregenerated keys so none should ever be skipped
  result := 0;
end;

//------------------------------------------------------------------------------
function NullMethod: TMethod;
//Helper function that returns a null method pointer
begin
  result.code := nil;
  result.data := nil;
end;

function NullFieldValidationEvent: TFieldValidationEvent;
//helper function that returns a null field validation event
var
  method : TMethod;
begin
  method.code := nil;
  method.data := nil;
  result := TFieldValidationEvent(method);

end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TDataObject.ValidationError(sMessage, sField, sValue: string; bSevere: boolean);
begin


//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
function TDataObject.DoNew(iSessionID: integer): boolean;
//var
//  t: ni;
begin
//  for t:= 0 to high(Fkeys) do begin
//    token.params[t] := 0-t;
//  end;


end;

function TDataObject.Fetch(sType: string; tokenparams: variant;
  sessionid: ni): TDataObject;
begin
  try
    if not IServerInterface(TDataObjectCache(cache).server).Fetch(TDataObjectCache(cache),result, sType, tokenparams, self.SessionID) then begin
      raise ECritical.create('could not fetch '+sType+' from '+self.classname);
    end;
  except
    on e: exception do begin
      e.message := 'could not fetch '+sType+' from '+self.classname+': '+e.message;
      raise;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TDataObjectToken._AddRef: Integer;
begin
  result := inherited _AddRef;
end;
//------------------------------------------------------------------------------
function TDataObjectToken._Release: Integer;
begin
  result := inherited _Release;
end;
//------------------------------------------------------------------------------
function TDataObject._AddRef: integer;
begin
  inc(FRefcount);
  Result := FRefCount;
end;
//------------------------------------------------------------------------------
function TDataObject._Release: integer;
begin
  dec(FRefCount);
  Result := FRefCount;

  if Result = 0 then begin
//    AuditLog('Freeing because of release:'+self.name, 'bg');
    //Free //BeforeDestruction leaves critical section
  end else begin
  end;
end;
//------------------------------------------------------------------------------
function TDataObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;

end;
//------------------------------------------------------------------------------
function TDataObject._RefCount: integer;
begin
  Result := FRefCount;

end;

//------------------------------------------------------------------------------
procedure TDataObject.AddAssociate(sName: string{OVERRIDE DoGetAssociate});
begin
  AddAssociate(sName, '', null);

end;

//------------------------------------------------------------------------------
function TDataObject.DoGetAssociate(sName: string): TDataObject;
begin
  result := nil;
end;

//------------------------------------------------------------------------------
function TDataObject.GetIsNew: boolean;
begin
  result := (FOrigin = dorNew) or (FOrigin = dorGhost);
end;

//------------------------------------------------------------------------------
procedure TDataObject.SetIsChanged(const Value: boolean);
var
  t: ni;
begin
  FIsChanged := Value;
  if not value then begin
    for t:= 0 to fieldcount-1 do begin
      fieldbyindex[t].ischanged := false;
    end;
  end;

end;

procedure TDataObject.SetIsNew(const Value: boolean);
begin
  if Value then
    FOrigin := dorNew
  else
    FOrigin := dorFetch;

end;
//------------------------------------------------------------------------------
procedure TDataObject.AddObject(iType: integer; params: variant);
//Adds an UNINSTANTIATED reference to a Data object to the DETAIL SECTION
//of the Data object.
var
  tkTemp: TDataObjectToken;
begin
  //note that there is not need to call InitializeObjectList as AddObjectToken
  //Handles this
  tkTemp := TDataObjectToken.create;

  tkTemp.VariantParams := params;

  AddObjectToken(tkTemp);

end;
//------------------------------------------------------------------------------
procedure TDataObjectToken.SetVariantPArams(const Value: variant);
begin
  FObjectName := '';
  FParams := Value;
  if VarType(FParams) and VarArray = varArray then
    iParamIndex := VarArrayHighBound(FParams,1)-VarArrayLowBound(FParams,1)+1
  else
    iPAramIndex := 1;

end;
//------------------------------------------------------------------------------
procedure TDataObject.AddObject(sType: string; params: variant);
var
  tkTemp: TDataObjectTOken;
begin
  tkTemp := TDataObjectToken.create;

  tkTemp.TypeName := sType;
  tkTemp.VariantParams := params;

  AddObjectToken(tkTemp);

end;
//------------------------------------------------------------------------------
function TDataObject.GetIsGhost: boolean;
begin
  result := FOrigin = dorGhost;
end;
//------------------------------------------------------------------------------
procedure TDataObject.Ghost;
begin
  OnGhost;
  IsNew := true;

end;
//------------------------------------------------------------------------------
function TDataObject.GetAssocName(idx: integer): string;
begin
  result := slAssociates[idx];

end;
function TDataObject.GetautoKeyName(idx: ni): string;
begin
  result := FKeys[idx];
end;

//------------------------------------------------------------------------------
function TDataObject.GetDeleteQuery: string;
begin
  result := 'delete from '+TableLink+' where '+filterphrase;
end;

function TDataObject.GetDetailTokens(idx: integer): TDataObjectToken;
begin
  InitializeObjectList;
  //if the object is is a TOKEN (never instantiated) then return it directly from the list
  if NOT (TObject(lstObjects[idx]) is TDataObject) then begin
    result := lstObjects[idx] as TDataObjectToken;
  end
  //if the object is NOT a TOKEN (because its a Data OBJECT) then return
  //the TOKEN OF the OBJECT
  else begin
    result := TDataObject(lstObjects[idx]).token;
  end;
end;
//------------------------------------------------------------------------------
function TDataObject.HasObject(doFind: TDataObject): boolean;
begin
  result := not (IndexOfObject(doFind) = -1);
end;
//------------------------------------------------------------------------------
function TDataObject.AssociateHasObject(doFind: TDataObject; sAssociateName: string): boolean;
begin
  result := IndexOfAssociateObject(doFind, sAssociateName) > -1;

end;
//------------------------------------------------------------------------------
function TDataObject.IndexOfAssociateObject(doFind: TDataObject; sAssociateName: string): integer;
var
  t: integer;
begin
  result := -1;
  for t:=0 to objectcount-1 do begin
    if obj[t].assoc[sAssociateName] = doFind then begin
      result :=t;
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TDataObject.IndexOfObject(doFind: TDataObject): integer;
var
  t: integer;
  sName: string;
begin
  result := -1;
  //read doFind name into temporary variable for optomization
  sName := doFind.token.name;
  for t:= 0 to objectcount-1 do begin
    if obj[t].token.name = sName then begin
      result := t;
      exit;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TDataObject.SwapObjects(idx1, idx2: integer);
var
  doTemp : TDataObject;
begin
  doTemp := lstObjects[idx1] as TDataObject;
  lstObjects[idx1] := lstObjects[idx2];
  lstObjects[idx2] := doTemp;
end;

//------------------------------------------------------------------------------

procedure TDataObject.Sort(iSortIndex: integer);
begin
  self.InstantiateAllTokens;
end;


//------------------------------------------------------------------------------
procedure TDataObject.QuickSort(CompareFunction: TListSortCompare);
begin
  raise ECritical.create('Sort needs to be reimplemented for better collections');
	// sort using Delphi's TList sort function which requires a compare-value fct to be passed in
//  if (lstObjects <> nil) and (lstObjects.Count > 1) then
//    lstObjects.Sort(CompareFunction);
end;

//------------------------------------------------------------------------------
procedure TDataObjectToken.Pool;
begin
  self.free;
end;

procedure TDataObject.DefineAutoKeyParams(sfield: string; sTable: string);
begin
  FAutoKeyField := sField;
  if sTable = '' then
    FAutoKeyTable := FTableLink
  else
    FAutoKeyTable := sTable;

  if fautokeytable = '' then
    raise Ecritical.create('Auto key table not defined for '+self.classname);

end;

procedure TDataObject.DefineCustomFetchQuery(sQuery: string;
  keyNames: array of string; iAutoIncrementKeyCount: ni);
begin
  DefineKeys(keyNames, iAutoIncrementKeyCount);
  FTableLink := '';
  FFetchQuery := sQuery;
end;

procedure TDataObject.DefineKeys(names: array of string; iAutoIncrementKeyCount: ni);
var
  t: ni;
begin
  FIdentityKEycount := iAutoIncrementKeyCount;
  setlength(FKeys, length(names));
  for t:= 0 to high(Fkeys) do
    Fkeys[t] := names[t];

end;

procedure TDataObject.DefineTableLink(sTableName: string; keynames: array of string; iAutoIncrementKeyCount: ni);
var
  t: ni;
  sKeyPhrase: string;
begin
  DefineKeys(keynames, iAutoIncrementKeyCount);
  FTableLink := sTableName;

  FFilterPhrase := '';
  for t:= 0 to high(FKeys) do begin
    sKeyPhrase := '('+FKeys[t]+'=~~~'+inttostr(t)+'~~~)';
    if t > 0 then
      FFilterPhrase := FFilterPhrase + ' and '+sKeyPhrase
    else
      FFilterPhrase := FFilterPhrase + sKeyPhrase;

    FFilterPhrase := ' where '+FFilterPhrase;
  end;

  //setup a standard query for the table
  FFetchQuery := 'select * from '+FTableLink;





end;

procedure TDataObject.DeleteObject(iIndex: integer);
var
  obj: TObject;
begin
  //get the object at the index
  obj := lstObjects[iIndex];
  //delete the object from the list
  lstObjects.delete(iIndex);

  if obj is TDataObject then
    TDataObject(obj).Release;

end;

procedure TDataObject.MoveObject(iSourceIndex, iDestIndex: integer);
var
  obj: TObject;
begin
  if iDestIndex >= lstObjects.count then
    exit;

  if iDestIndex < 0 then
    exit;

  if iSourceIndex >= lstObjects.count then
    exit;

  if iSourceIndex < 0 then
    exit;


  obj := lstObjects[iSourceIndex];
  lstObjects.Delete(iSourceIndex);
  lstObjects.Insert(iDestIndex, obj);
end;

procedure TDataObject.Detach(bDestroying: boolean = false);
//Removes all objects fields and associates from the
//Data object... used by cache to put the objects
//into a state where they can be safely freed
var
  t: integer;
  obj: TObject;
  dobj: TDataObject;
begin
///    FSpecialFieldDefs: TStringList;
///    FToken: TDataObjectToken;
//    slAssociates: TStringList;
//    lstObjects: Tlist;
//    lstCoordinators: TInterfaceList;
//    lstControls: TInterfaceList;
//    lstSpecialFields: Tlist;
///    FFieldDefs: array of TDOFieldDef;
//    FExtFieldDefs: array of TExtDOFieldDef;
//    FValFieldDefs: array of TValDOFieldDef;

    if not bDestroying then begin
//      AuditLog('Detach:'+self.name, 'cache');
//      AuditLog('Field Count:'+inttostr(fFieldCount), 'cache');
//      AuditLog('Object Count:'+inttostr(ObjectCount), 'cache');
//      AuditLog('Associate Count:'+inttostr(AssociateCount), 'cache');
    end;

//    AuditLog('-Field detach ' +self.name, 'bg');
    //Destroy fields referenced in flyweight array
    for t:=0 to FFieldCount - 1 do begin
      try
        if FFieldDefs[t].Instance <> nil then begin
          FFieldDefs[t].Instance.Owner:=nil;
          FFieldDefs[t].Instance.Free;
          FFieldDefs[t].Instance := nil;
          FFieldDefs[t].Name := '';
        end;
        {$IFNDEF NOFINALIZE}
        Finalize(FFieldDefs[t]);
        {$ENDIF}
      except
      end;
    end;
    //Set Field Count to 0!  Important!!!
    FFieldCount := 0;


//    AuditLog('-Special Field detach ' +self.name, 'bg');
    //Destroy SPECIAL fields in list;
    if assigned(lstSpecialFields) then
      while lstSpecialFields.Count>0 do begin
        TDataField(lstSpecialFields[0]).free;
        lstSpecialFields.Delete(0);
      end;

//    AuditLog('-Associate detach ' +self.name, 'bg');
    //RELEASE  associates in list;
    if assigned(slAssociates) then
      while slAssociates.Count>0 do begin
        obj := TObject(slAssociates.objects[0]);
        slAssociates.Delete(0);
        if TObject(obj) is TDataObject then
         TDataObject(obj).detach
        else begin
          TDataObjectToken(obj).free;
        end;

      end;

    {$IFDEF DODEBUG}LogMessage(self.name+' DESTROY COMPLETE');{$ENDIF}

//    AuditLog('-Object detach ' +self.name, 'bg');
    //RELEASE objects in list;
    if assigned(lstObjects) then begin
(*      //free the object list in a thread
      thread := TDOReleaseThread.create;
      thread.List := lstObjects;
      thread.Resume;*)
      for t:= lstObjects.count-1 downto 0 do begin
        if t>=lstObjects.count then
          continue;
        if TObject(lstobjects[t]) is TDataObject then begin
          dobj := TDataObject(lstObjects[t]);
          lstObjects.delete(t);
//          dobj.detach;
        end
        else begin
          try
            TDataObjectToken(lstObjects[t]).free;
            lstObjects.Delete(t);
          except
          end;
        end;
      end;
    end;

end;

{ TFloatingPointDataField }

constructor TFloatingPointDataField.create(owner: TDataObject;
  rec: TDOFieldDef);
begin
  AsVariant := 0.0;
end;

procedure TFloatingPointDataField.SetAsString(str: string);
begin
  inherited;
  AsVariant := strtofloat(str);
end;


procedure TDataObject.MergeObjects(doMerger: TDataObject;
  bRemoveDuplicates: boolean);
var
  t: integer;
begin
  if not assigned(doMerger) then
    raise Exception.create('List to be merged with '+self.name+' was NIL.');

  for t:=0 to doMerger.objectcount-1 do begin
    if (not self.HasObject(doMerger.obj[t])) or (not bRemoveDuplicates) then begin
      self.AddObject(doMerger.obj[t]);
    end;
  end;
end;

procedure TDataObject.RemoveObject(doRemove: TDataObject);
var
  iPos: integer;
begin
  iPos := IndexOfObject(doRemove);
  if iPos > -1 then
    self.DeleteObject(iPos);

end;

function TDataObject.ReplaceQueryKeys(sQuery: string): string;
var
  t: ni;
begin
  result := sQuery;
  for t:= 0 to token.paramcount-1 do begin
    result := StringReplace(result, '~~~'+inttostr(t)+'~~~', vartomysqlstorage(token.params[t]), [rfReplaceAll]);
  end;

end;

procedure TDataObject.ResetOrigin;
begin
  FOrigin := dorFetch;
end;

procedure TDataObject.InsertObject(obj: TDataObject;
  iPosition: integer);
begin
  InitializeObjectList;

  lstObjects.Insert(iPosition, TObject(obj));
  obj.Reference;

end;

procedure TDataObject.SetOrphanedValues;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TDataObject.SetOrphaned(const Value: boolean);
begin
  FOrphaned := Value;
  SetOrphanedValues;
end;

function TDataField.GetAsWebString: string;
begin
  result := EncodeWebString(AsString);
end;

function TDataField.GEtExportString: string;
begin
  result := 'field::'+self.name+'='+StorageString;

end;

procedure TDataField.SetAsWebString(str: string);
begin
  AsString := DecodeWebString(str);
end;


procedure TDataObject.BeforeReadField(iFieldIndex: integer);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TDataObject.BeforeSave;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TDataObject.AfterSave;
begin

//TODO -cunimplemented: unimplemented block
end;

//------------------------------------------------------------------------------
function TDataObject.IsAssociateInstantiated(sName: string): boolean;
var
  iTemp: integer;
begin
  result := false;
  iTemp := slAssociates.IndexOf(sName);
  if iTemp > -1 then begin
    result := IsAssociateInstantiated(iTemp);
  end;
end;
//------------------------------------------------------------------------------
function TDataObject.IsAssociateInstantiated(iIndex: integer): boolean;
var
  tTemp: TDataObjectToken;
begin
  result := slAssociates.objects[iIndex] is TDataObject;
  if (not result) and (slAssociates.objects[iIndex] is TDataObjectToken) then begin
    tTemp := slAssociates.objects[iIndex] as TDataObjectToken;
    if (Vartype(tTemp.VariantParams) and varArray) = varArray then
      result := tTemp.VariantParams[0] <> NULL
    else
      result := tTemp.VariantParams <> NULL;
  end;


end;
//------------------------------------------------------------------------------
procedure TDataObject.ClearAssociateTokenFlags;
//loops through all associate records and clears the flag that indicates whether
//they've been instantiated
var
  t: integer;
  oTemp: TObject;
  tTemp: TDataObjectToken;
begin
  for t:= 0 to self.AssociateCount-1 do begin
    oTemp := slAssociates.objects[t];
    if oTemp is TDataObjectToken then begin
      tTemp := oTemp as TDataObjectToken;
      if tTemp.TypeName = '' then
        tTemp.VariantParams := null;
    end;
  end;
end;

function TDataField.GetTypeName: string;
begin
  case self.ValueType of
      dftUndefined: result := 'undefined';
         dftString: result := 'string';
          dftShort: result := 'smallint';
           dftLong: result := 'integer';
         dftDouble: result := 'float';
       dftDateTime: result := 'datetime';
        dftBoolean: result := 'boolean';
  end;

end;

function TDataObject.GetXMLElementName: string;
//Implements a generic way of naming XML elements based on this object
//Simply copies everything after the 'Tdo' in the class name
begin
  result := copy(self.ClassName, 4, length(self.ClassName));
end;

{ TDataObjectObserver }

constructor TDataObjectObserver.Create;
begin
  inherited;
  FList := TStringList.create;
  FOnUpdate := nil;
  FTotalObjects := 0;
  FTotalRequests := 0;

end;

procedure TDataObjectObserver.Dec(obj: TDataObject);
var
  iPos: integer;
begin
  LockWrite;
  try
    iPos := FList.IndexOfObject(obj);

    if iPos >=0 then
      FList.delete(iPos);
//    else
//      RAISE Exception.create(obj.name +' not found in observer');

  finally
    UnlockWrite;
  end;

  If assigned(OnUpdate) then OnUpdate(self);

end;

destructor TDataObjectObserver.destroy;
begin
  OnUpdate := nil;
  FList.Free;
  inherited;
end;

function TDataObjectObserver.GetCount: integer;
begin
  LockRead;
  try
    result := FList.count;
  finally
    Unlockread;
  end;
end;

function TDataObjectObserver.GetLastAudit: string;
begin
  LockRead;
  try
    result := makeThreadSafe(FLastAudit);
    FlastAudit := '';
  finally
    UnlockRead;
  end;
end;

function TDataObjectObserver.GetObject(idx: integer): string;
begin
  LockRead;
  try
    result := FList[idx];
    uniqueString(result);
  finally
    UnLockRead;
  end;
end;

function TDataObjectObserver.GetTotalObjects: integer;
begin
  LockRead;
  try
    result := FTotalObjects;
  finally
    unlockRead;
  end;
end;

function TDataObjectObserver.GetTotalRequests: integer;
begin
  LockRead;
  try
    result := FTotalRequests;
  finally
    unlockRead;
  end;

end;
procedure TDataObjectObserver.Inc(sName: string; obj: TDataObject);
begin
  LockWrite;
  try
//    UniqueString(sName);
    FList.addObject(inttohex(integer(obj.Cache),8)+':'+sName+'-'+inttostr(GetCurrentThreadID), obj);
    TallyObject;
  finally
    UnlockWrite;
  end;

  If assigned(OnUpdate) then OnUpdate(self);
end;


procedure TDataObject.RegisterWithCache;
begin
  DOOB.Inc(self.name, self);
  TDataObjectCache(cache).RegisterDataObject(self);
end;

procedure TDataObject.DeRegisterWithCache;
begin
  TDataObjectCache(cache).DeRegisterDataObject(self);
  DOOB.Dec(self);

end;

procedure TDataObject.OnGhost;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TDataObject.OnParentKeyUpdate(parent: TDataObject;
  priorkeys: variant);
begin
  //
end;

procedure TDataObject.AddFieldDef(sName: string;
  FieldType: TDataFieldClass; vInitialValue: variant;
  LookupObj: cardinal; LookupVars: variant; DisplayField,
  IndexField: string);
var
  method: TMethod;
begin
  method.Code := nil;
  method.Data := nil;
  AddFieldDef(sName, FieldType, vInitialValue, LookupObj, LookupVars,
              DisplayField,IndexField,
              TFieldValidationEvent(method),
              TFieldValidationEvent(method),
              TFieldValidationEvent(method));

end;


procedure TDataObject.AddFieldDef(sName: string;
  FieldType: TDataFieldClass; vInitialValue: variant);
var
  method: TMethod;
begin
  method := NullMethod;

  AddFieldDef(
    sName, FieldType, vInitialValue,
    0, null, '','',
    TFieldValidationEvent(Method),
    TFieldValidationEvent(Method),
    TFieldValidationEvent(Method));
end;

procedure TDataObject.AddFieldDef(sName: string;
  FieldType: TDataFieldClass; vInitialValue: variant;
  LookupObj: cardinal; LookupVars: variant; DisplayField,
  IndexField: string; const CanChange, CanExit,
  CanSave: TFieldValidationEvent);
var
  rec: TDOFieldDef;
  ext: TExtDOFieldDef;
  val: TValDOFieldDef;
begin

  rec.Name := sName;
  rec.ClassVar := FieldType;
  rec.Value := vInitialValue;
//  rec.DefaultValue := vInitialValue;
  rec.Instance := nil;
  rec.ExtIndex := -1;
  rec.ValIndex := -1;


  //if a lookup is defined the set extended parameters
  if LookupObj > 0 then begin
    rec.ExtIndex := iExtFieldDefCount;
    inc(iExtFieldDefCount);
    if ExtFieldCapacity < iExtFieldDefCount then
      ExtFieldCapacity := iExtFieldDefCount;

    ext.LookupObj := LookupObj;
    ext.LookupVars := LookupVars;
    ext.DisplayField := displayField;
    ext.IndexField := IndexField;

    FExtFieldDefs[rec.ExtIndex] := ext;
  end;

  //if validation is defined then
  if assigned(CanChange) or assigned(CanExit) or assigned(CanSave)then begin
    rec.ValIndex := iValFieldDefCount;
    inc(iValFieldDefCount);
    if ValFieldCapacity < iValFieldDefCount then
      ValFieldCapacity := iValFieldDefCount;

    val.OnCanSave := CanSave;
    val.OnCanExit := CanExit;
    val.OnCanChange := CanChange;

    //put the validation record into the validation flyweight array
    FValFieldDefs[rec.ValIndex] := val;
  end;

  inc(FFieldCount);
  If FieldCount > FieldCapacity then
    FieldCapacity := FieldCount;
  //add the definition to the FlyWeight Array
  FFieldDefs[FFieldCount-1] := rec;

end;

procedure TDataObject.AddFieldDef(sName: string;
  FieldType: TDataFieldClass; vInitialValue: variant; const CanChange,
  CanExit, CanSave: TFieldValidationEvent);
begin
  AddFieldDef(
    sName, FieldType, vInitialValue,
    0, null, '', '',
    CanChange, CanExit, CanSave);

end;
function TDataObject.DoCreateSpecialField(
  sFieldName: string): TDataField;
var
  tfc: TDataFieldClass;
  sLCFieldName: string;
  t: integer;
begin
  result := nil;

  sLCFieldName := lowercase(sFieldName);

  for t:= 0 to FSpecialFieldDefs.count-1 do begin
    if lowercase(FSpecialfieldDefs[t]) = sLCFieldName then begin
      tfc := TDatafieldClass(FSpecialFieldDefs.objects[t]);
      result := tfc.createspecial(self,sFieldName);
    end;
  end;


end;


function TDataObject.GetSpecialFieldDefs: TStringList;
begin
  if FSpecialFieldDefs = nil then
    FSpecialFieldDefs := TStringList.create;

  result := FSpecialFieldDefs;
end;

function TDataObject.HasSpecialFieldDef(sFieldName: string): boolean;
var
  t: integer;
begin
  result := false;
  if FSpecialFieldDefs = nil then begin
    result := false;
  end else begin
    sfieldName := lowercase(sfieldName);
    for t:= 0 to SpecialFieldDefs.count-1 do begin
      if sFieldName = lowercase(SpecialFieldDefs[t]) then begin
        result := true;
        break;
      end;
    end;
  end;
end;

procedure TDataObject.AddCalculatedField(sName: string;
  FieldType: TDataFieldClass);
begin
  SpecialFieldDefs.addObject(sName, pointer(FieldType));

end;

procedure TDataObject.BeforeReadField(sFieldName: string);
begin
  BeforeReadField(IndexOfField(sFieldName));


end;

procedure TDataObject.DoAddObject(obj: TDataObject; out bHandled: boolean);
begin
  bHandled := false;
end;

procedure TDataObjectObserver.TallyObject;
const
  BILLION = 1000000000;
begin
  LockWrite;
  try
    if FTotalObjects>2*BILLION then
      FTotalObjects := 0;

    system.inc(FTotalObjects);



  finally
    UnLockWrite;
  end;
end;

procedure TDataObjectObserver.TallyRequest(sAuditText: string);
const
  BILLION = 1000000000;
begin
  LockWrite;
  try
    if FTotalRequests>2*BILLION then
      FTotalRequests := 0;

    system.inc(FTotalRequests);
    FLastAudit := MakeThreadSafe(sAuditText);
  finally
    UnLockWrite;
  end;
end;

function TBooleanDataField.GetAsString: string;
//You should be able to read this
begin
  result := inherited GetAsString;

(*  if result = '0' then
    result := 'false'
  else
    result := 'true';*)


end;

function TBooleanDataField.GetStorageString: string;
begin
  if AsVariant then
    result := '1'
  else
    result := '0';
end;

function TDataObjectToken.GetConstName: string;
begin
  result := BuildObjectName(self.typename, variantparams, self.DataCenter, self.DataTier);

end;

function TDataObject.GetFetchQuery: string;
var
  sOrder: string;
  sFilterPhrase: string;
begin
  sOrder := OrderBy;
  sFilterPhrase := FilterPhrase;
  result := FFetchQuery;

  if sFilterPhrase <> '' then
    result := result + FfilterPhrase;

  if sOrder <> '' then
    result := result +' ORDER BY '+sOrder;

  result := ReplaceQueryKeys(result);

end;

function TDataObject.HasTokens: boolean;
var
  t: integer;
begin
  result := false;
  for t:= 0 to Objectcount-1 do begin
    if TObject(lstObjects[t]) is TDataObjectToken then begin
      result := true;
      break;
    end;
  end;

end;

procedure TDataObject.InstantiateAllTokens;
var
  t: integer;
  obj: TDataObject;
begin
  for t:= 0 to Objectcount-1 do begin
    if TObject(lstObjects[t]) is TDataObjectToken then begin
      obj := nil;
      if not self.InstantiateToken(obj, TDataObjectToken(lstobjects[t]), self.SessionID) then
        raise Exception.create('Unable to instantiate token');

      lstObjects[t] := obj;
    end;
  end;

end;


procedure TDataObject.AfterWriteField(field: TDataField);
begin

//TODO -cunimplemented: unimplemented block
end;


procedure TDataField.RevertToDefaultValue;
begin
//  owner.FFieldDefs[FMyIndex].Value := owner.FFieldDefs[FMyIndex].DefaultValue;
end;

procedure TDataObject.RevertToDefaultValues;
var
  t: integer;
begin
  for t:= 0 to FieldCount-1 do begin
    FieldByIndex[t].RevertToDefaultValue;
  end;

end;

{ TDateDataField }

constructor TDateDataField.create(owner: TDataObject;
  rec: TDOFieldDef);
begin
  inherited;
end;

procedure TDateDataField.SetAsString(str: string);
begin
  AsVariant := strtodateTime(str);

end;

procedure TDateDataField.SetAsVariant(v: variant);
begin
  inherited;
end;

{ TTimeDataField }

constructor TTimeDataField.create(owner: TDataObject;
  rec: TDOFieldDef);
begin
;
end;

procedure TTimeDataField.SetAsString(str: string);
begin
  AsVariant := strtoTime(str)
end;

procedure TTimeDataField.SetAsVariant(v: variant);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TDataObject.RemoveDuplicates;
var
  t: integer;
begin
  for t:=self.objectcount-1 downto 0 do begin
    if IndexOfObject(obj[t]) <> t then
      deleteobject(t);

  end;

end;



procedure TDataObjectToken.DeleteParam(idx: integer);
var
  v, v2: variant;
  t,i: integer;
begin
  FObjectName := '';
  if Owner<> nil then
    Owner.DeregisterWithCache;

  v := self.variantparams;



  if ((vartype(v) and varArray)<> varArray) then begin
    if idx = 0 then begin
      self.variantparams := null;
    end;
    exit;
  end;

  v2 := VarArrayCreate([0,0], varVariant);
  VarArrayRedim(v2, VarArrayHighBound(v,1)-1);

  i := VarArrayLowBound(v2,1);
  for t:= VararrayLowBound(v,1) to VarArrayHighBound(v,1) do begin
    if t-VarArrayLowBound(v,1) <> idx then begin
      v2[i] := v[t];
      inc(i);
    end;
  end;

  self.VariantParams := v2;

  if Owner<> nil then
    Owner.RegisterWithCache;

end;

function TDateTimeDataField.GetAsString: string;
begin
  If vartype(AsVariant) = varString then begin
    if AsVariant = '' then
      AsVariant := TDatetime(NULL_DATETIME)
    else
      AsVariant := strtodatetime(AsVariant);
  end;


  If vartype(AsVariant) = varNull then
    result := ''
  else
  if Asvariant = TDateTime(NULL_DATETIME) then
    result := ''
  else
  if varType(AsVariant) = varString then
    result := AsVariant
  else
    result := datetimetostr(AsVariant);

end;

{ TVariantDataField }

constructor TVariantDataField.create(owner: TDataObject;
  rec: TDOFieldDef);
begin
  inherited;

end;

function TVariantDataField.GetQuotedStorageString: string;
var
  bShouldQuote: boolean;
begin
  bShouldQuote := ((VarType(AsVariant) and varString) = varString) or ((VarType(AsVariant) and varString) = varDate);

  if bShouldQuote then
    result := '"'+StorageString+'"'
  else
    result := StorageString;
end;

function TVariantDataField.GetStorageString: string;
begin
  if VarType(AsVariant) = varDate then
    result := FormatDateTime('yyyy-mm-dd hh:mm:ss', AsVariant)
  else
    result := inherited GetStorageString;

end;

procedure TVariantDataField.SetAsString(str: string);
begin
  inherited;
  AsVariant := str;
end;


procedure TDataObject.ClearFieldDefs;
begin
  FFieldCount := 0;
end;

procedure TDataObject.Link(sType: string; vParams: variant);
var
  tok: TDataObjectToken;
begin
  tok := TDAtaObjectToken.create;
  tok.TypeName := sType;
  tok.VariantParams := vParams;
  self.AddObjectToken(tok);

end;

function TDataField.GetStorageString: string;
begin
  result := vartoMYSQLStorage(AsVariant);
end;

{ TMYSQLDateTimeDataField }

function TMYSQLDateTimeDataField.GetStorageString: string;
begin
  if vartype(AsVariant)=varnull then
    result := ''
  else
    result := FormatDateTime('yyyy-mm-dd hh:mm:ss', AsVariant);

end;

{ TDataLink }

function TDataObject.GetLinkTo: string;
begin
  result := FLinkTo;
end;

procedure TDataObject.SetLinkto(const Value: string);
begin
  FLinkTo := value;

end;


initialization

finalization
//  DOOB.free;


end.

