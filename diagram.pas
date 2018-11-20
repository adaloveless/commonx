unit diagram;
//1. fix mouse coordinates
//2. scroll with middle mouse button
//3. Multi-Select
//4. Scroll Bars

interface

uses
  debug, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs, windows, beeper, generics.collections,
  StdCtrls, advancedgraphics, messages, xmltools, math, namevaluepair, variants, sharedObject, betterobject, typex;
const
  MK_LBUTTON = 1;
  MK_RBUTTON = 2;
  MK_SHIFT = 4;
  MK_CONTROL = 8;
  MK_MBUTTON = 16;
  CLASS_TEXT_BORDER = 4;
  CLASS_TEXT_HEIGHT = 12;
  EOL = #13#10;

type
  TCompilerProgressEvent = procedure (pos, max: integer) of object;
  TCompilerClass = TBetterObjectClass;
  TDragHookType = (dhLeft, dhRight, dhTop, dhBottom, dhTopLeft, dhTopRight, dhBottomLeft, dhBottomRight, dhAll);
  TScope = (scUndefined, scPrivate, scProtected, scPublic, scPublished, scImplementation, scFinalization, scInitialization, scInterface);
  TFunctionFlags = set of (ffOverload, ffVirtual, ffDynamic, ffOverride, ffReintroduce, ffStdCall, ffRegister, ffMessage, ffAbstract);
  TSymbol = class;
  TUMLProject = class;
  TMatureConstruct = class;
  TdiagramMemberRecord = class;
  TdiagramDisplay = class;
  TSymbolList = class;
  TResolvable = class;
  TNameSpace = class;
  TResolvableList = class;



  //----------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  TCompartmentDecoration = set of (cdOverride, cdVirtual, cdDynamic, cdReintroduce,cdProperty, cdConstructor, cdDestructor, cdGetter, cdSetter);
  TCompartmentScope = (cdUndefined, cdField, cdPrivate, cdProtected, cdPublic, cdPublished);
  //----------------------------------------------------------------------------
  TCompartmentClass = class of TCompartmentItem;

  //----------------------------------------------------------------------------
  TSymbol = class(TObject)
  private
    FName: ansistring;
    FParent: TSymbol;
    FChildren: TSymbolList;
    FProject: TUMLProject;
    FDocumentation: Tstringlist;
    FResponsibilities: TStringList;
    FAuthor: ansistring;
    FDebug: ansistring;
    FNameSpace: TNamespace;
    FFullyQualifiedName: string;

    function GetChildren: TSymbolList;
    function GetHasParent: boolean;
    function GetParent: TSymbol;
    procedure SetName(const Value: ansistring);
    procedure SetParent(const Value: TSymbol);
    procedure SetProject(const Value: TUMLProject);
    function GEtParentName: ansistring;
    function GetDebugName: string;
  protected
    procedure RegisterWithParent;
    procedure DeRegisterWithParent;

  public
    procedure CopyFrom(sym: TSymbol);
    constructor create(sName: string; oProject: TUMLProject; parent: TSymbol); reintroduce; virtual;
    destructor Destroy; override;
    property Name: ansistring read FName write SetName;
    property FullyQualifiedName: string read FFullyQualifiedName write FFullyQualifiedName;
    property ParentName: ansistring read GEtParentName;
    property Children: TSymbolList read GetChildren;
    property Parent: TSymbol read GetParent write SetParent;
    property HasParent: boolean read GetHasParent;
    property Project: TUMLProject read FProject write SetProject;
    procedure RegisterChild(child: TSymbol);
    procedure DeregisterChild(child: TSymbol);
    procedure Paint(dd: TDiagramDisplay; mr: TDiagramMemberRecord);virtual;
    procedure PaintMain(dd: TDiagramDisplay; mr: TDiagramMemberRecord);virtual;
    procedure HoverPaint(dd: TdiagramDisplay; mr: TDiagramMemberRecord);virtual;
    procedure SelectPaint(dd: TdiagramDisplay; mr: TDiagramMemberRecord);virtual;
    function GetDragHookType(dd: TDiagramDisplay; mr: TDiagramMemberRecord; ClickPointX, ClickPointY: integer): TDragHookType; virtual;
    function GetIntersectPoint(mr: TDiagramMemberRecord; GlobalX, globaly: real; ddDebug: TDiagramdisplay = nil): Tpoint; virtual;
    function IsAtPoint(mt: TDiagramMemberRecord; x: integer; y: integer): boolean;virtual;
    function MouseDrag(dd: TDiagramDisplay; mr: TDiagramMemberRecord; DragDistanceX, DragDistanceY: integer): boolean; virtual;
    function MouseClick(dd: TDiagramDisplay; mr: TDiagramMemberRecord; ClickPointX, ClickPointY: integer): boolean; virtual;
    procedure MouseStartDrag(dd: TDiagramDisplay; mr: TDiagramMemberRecord; InitClickGlobalX, InitClickGlobalY: integer); virtual;
    procedure MouseOver(dd: TdiagramDisplay; mr: TDiagramMemberRecord; globalX, globalylY: integer);virtual;
    procedure MouseExit(dd: TdiagramDisplay; mr: TDiagramMemberRecord);virtual;
    property Documentation: TStringList read FDocumentation;
    procedure AddDocLine(sLine: ansistring);

    property Responsibilities: TStringList read FResponsibilities;

    property Author: ansistring read FAuthor write FAuthor;
    property DebugHTML: ansistring read FDebug write FDebug;
    property DebugName: string read GetDebugName;
    property NameSpace: TNamespace read FNameSpace;
  end;
  //----------------------------------------------------------------------------



  TCompartmentItem = class(TSymbol)
  private
    FSignature: ansistring;
    Fdecoration: TCompartmentDecoration;
//    FName: ansistring;
    FDocumentation: TStringList;
    FScope: TCompartmentScope;
    FAuthor: ansistring;
    FParameters: TNameValuePairList;
    FAlias: ansistring;
    FVisibility: ansistring;
    FInType: ansistring;
    FOutType: ansistring;
    FWebPage: ansistring;
    FIntegrationPackage: ansistring;
  public
    constructor create(sName: string; oProject: TUMLProject; parent: TSymbol); override;
    destructor destroy; override;
//    property Name: ansistring read FName write FName;
    property Signature: ansistring read FSignature write FSignature;
    property Scope: TCompartmentScope read FScope write FScope;
    property Decoration: TCompartmentDecoration read Fdecoration write FDecoration;
    property Documentation : TStringList read FDocumentation;
    property Author: ansistring read FAuthor write FAuthor;
    property Parameters: TNameValuePairList read FParameters;
    property Alias: ansistring read FAlias write FAlias;
    property WebPage: ansistring read FWebPage write FWebPage;
    property IntegrationPackage: ansistring read FIntegrationPackage write FIntegrationPackage;
    property InType: ansistring read FInType write FInType;
    property OutType: ansistring read FOutType write FOutType;
    property Visibility: ansistring read FVisibility write FVisibility;
  end;

  //----------------------------------------------------------------------------
  TMemberDefinition =class(TCompartmentItem)
  private
    FType: ansistring;
  public
    property TypeName: ansistring read FType write FType;
  end;

  TFunctionDefinition =class(TCompartmentItem)
  private
    FResultType: ansistring;
    FFunctionFlags: TFunctionFlags;
  public
    property ResultType: ansistring read FResultType write FResultType;
    property Flags: TFunctionFlags read FFunctionFlags write FFunctionFlags;
  end;

  //----------------------------------------------------------------------------
  TPropertyDefinition = class(TCompartmentItem)
  private
    FReadFrom: ansistring;
    FWriteTo: ansistring;
  public
    property ReadFrom: ansistring read FReadFrom write FReadFrom;
    property Writeto: ansistring read FWriteTo write FWriteTo;
  end;

  //----------------------------------------------------------------------------

  TPointList= class(TObject)
  private
    FPoints: array of TPoint;
    function GetCount: integer;
    function GetPoints(index: integer): TPoint;
    procedure SetPoints(index: integer; const Value: TPoint);
    function GetMaxX: integer;
    function GetMaxY: integer;
    function GetMinX: integer;
    function GetMinY: integer;
  public
    constructor create; reintroduce; virtual;
    destructor Destroy; override;
    property Points[index: integer]: TPoint read GetPoints write SetPoints;default;
    procedure Add(point: TPoint);
    procedure Change(idx: integer; x,y: integer);
    procedure Remove(point: TPoint);
    procedure Delete(idx: integer);
    property Count: integer read GetCount;
    property MaxX: integer read GetMaxX;
    property MinX: integer read GetMinX;
    property MaxY: integer read GetMaxY;
    property MinY: integer read GetMinY;
  end;
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  Tdiagram = class;
  //----------------------------------------------------------------------------
  TDiagramMemberRecord = class(TObject)
  private
    FDiagram: TDiagram;
    FSymbol: TSymbol;
    FPoints: TPointList;
    FProject: TUMLProject;
    FSelected: boolean;
    FDragPointY: integer;
    FDragPointX: integer;
    FDragOffsetY: integer;
    FDragOffsetX: integer;
    FDragStartY: integer;
    FDragStartX: integer;
    FMouseOver: boolean;
    function GetPointList: TPointList;
    function GetOwner: TDiagram;
    procedure SetSymbol(const Value: TSymbol);
    procedure SetSelected(const Value: boolean);
    function GetDragDistanceX: integer;
    function GetDragDistanceY: integer;
    procedure SetDragStartX(const Value: integer);
    procedure SetDragStartY(const Value: integer);
    procedure SetMouseOver(const Value: boolean);
  public
    constructor create(oProject: TUMLProject; diagram: TDiagram); reintroduce; virtual;
    destructor Destroy; override;
    property Project: TUMLProject read FProject write FProject;
    property Symbol: TSymbol read FSymbol write SetSymbol;
    property Points: TPointList read GetPointList;
    property DragPointX: integer read FDragPointX write FDragPointX; //??
    property DragPointY: integer read FDragPointY write FDragPointY;
    property DragStartX: integer read FDragStartX write SetDragStartX;
    property DragStartY: integer read FDragStartY write SetDragStartY;
    property DragOffsetX: integer read FDragOffsetX write FDragOffsetX;
    property DragOffsetY: integer read FDragOffsetY write FDragOffsetY;


    property DragDistanceX: integer read GetDragDistanceX;//TODO 1: bunk
    property DragDistanceY: integer read GetDragDistanceY;//TODO 1: bunk

    property Owner: TDiagram read GetOwner;
    property Selected: boolean read FSelected write SetSelected;
    property MouseOver: boolean read FMouseOver write SetMouseOver;
    procedure Paint(dd: TdiagramDisplay); virtual;
    procedure DragStart;

  end;

  //----------------------------------------------------------------------------
  TDiagramActionEvent = procedure (sender: TObject; member: TDiagramMemberRecord);

  //----------------------------------------------------------------------------
  TDiagramToolMode = (tmSelect, tmAddClass);

  TDiagramClickEvent = procedure (sender: TObject; member: TDiagramMemberRecord; x, y: integer) of object;

  TDiagramUserState = (dsDrag, dsZoom);
  //----------------------------------------------------------------------------
  TDiagramDisplay = class(TDrawingBoard)
  //1st purpose of this class is to control a canvas to display a diagram.
    //it does this by:
    //1. Reading the Diagram, Diagram Member List, and Member Records
    //2. Drawing boxes and shit based on the layout of the diagram.
  //2nd purpose of this class is to publish event for when objects in the canvas
    //are clicked on
  private
    FDiagram: TDiagram;
    FToolMode: TDiagramToolMode;
    FOnDiagramClick: TDiagramClickEvent;
    FUserState: TdiagramUserState;
    FMouseDownRecord: TDiagramMemberRecord;
    FmouseDownY: integer;
    FmouseDownX: integer;
    FDragging: boolean;
    FmouseDownGlobalX: real;
    FmouseDownGlobalY: real;
    FMouseOverRecord: TDiagramMemberRecord;
    FScrollBarH: TScrollBar;
    FScrollBarV: TScrollBar;
    FDragHookType: TDragHookType;
    procedure Change; virtual;
    procedure SetDiagram(const Value: TDiagram);
    procedure DoDraw; override;
    procedure DiagramEndClick(var msg: TMessage; mr: TdiagramMemberRecord;
      globalX, globalY: integer);
    procedure DiagramEndDrag(var msg: TMessage; mr: TdiagramMemberRecord;
      globalX, globalY: integer);
    procedure DiagramClick(var msg: TMessage; mr: TdiagramMemberRecord; globalX, globalY: integer);
    procedure DiagramStartClick(var msg: TMessage;
      mr: TdiagramMemberRecord; globalX, globalY: integer);
    procedure DiagramDrag(var msg: TMessage; mr: TdiagramMemberRecord;
      globalX, globalY: real);
    procedure DiagramMouseMessage(var msg: TMessage; globalX, globalY: integer);
    procedure DiagramMouseMove(var msg: TMessage; mr: TdiagramMemberRecord;
      globalX, globalY: integer);
    procedure SetMouseOverRecord(const Value: TDiagramMemberRecord);
    procedure SetScrollbarH(const Value: TScrollBar);
    procedure SetScrollbarV(const Value: TScrollBar);
  protected
    procedure UpdateScrollBars;
    property MouseDownRecord: TdiagramMemberRecord read FMouseDownRecord write FMouseDownREcord;
    property MouseDownX: integer read FmouseDownX write FMouseDownX;
    property MouseDownY: integer read FmouseDownY write FMouseDownY;
    property MouseDownGlobalX: real read FmouseDownGlobalX write FMouseDownGlobalX;
    property MouseDownGlobalY: real read FmouseDownGlobalY write FMouseDownGlobalY;
    property MouseOverRecord: TDiagramMemberRecord read FMouseOverRecord write SetMouseOverRecord;
    property Dragging: boolean read FDragging write FDragging;

    procedure CanvasConnect; override;
  public
    constructor Create(aOwner: TComponent);override;
    destructor Destroy; override;
    procedure Click;override;
    property ToolMode: TDiagramToolMode read FToolMode write FToolMode;
    property Diagram: TDiagram read FDiagram write SetDiagram;

    procedure WndProc(var msg: Tmessage); override;
    function FindObjectAtPoint(x,y: integer): TDiagramMemberRecord;
    procedure ScrollBarHChange(sender: TObject);
    procedure ScrollBarVChange(sender: TObject);
    procedure TextByLine(x,y: integer; iLine: integer; sText: ansistring; iBoxWidth :integer; iFontHeight: integer = CLASS_TEXT_HEIGHT; iLineSpace: integer = 13);
    procedure Invalidate; override;
  published
    property UserState : TdiagramUserState read FUserState write FUserState;
    property OnDiagramClick: TDiagramClickEvent read FOnDiagramClick write FOnDiagramClick;
    property ScrollBarH: TScrollBar read FScrollBarH write SetScrollbarH;
    property ScrollBarV: TScrollBar read FScrollBarV write SetScrollbarV;
    property DragHookType: TDragHookType read FDragHookType write FDragHookType;

  end;


  //----------------------------------------------------------------------------
  TDiagramMemberList = class(TObject)
  private
    FMembers : TList;
    FProject: TUMLProject;
    function GetCount: integer;
    function GetDiagramMember(idx: integer): TDiagramMemberRecord;
    function GetSelectCount: integer;
  public
    constructor create(oProject: TUMLProject); reintroduce; virtual;
    destructor Destroy; override;
    property Project: TUMLProject read FProject write FProject;
    property Members[idx: integer]: TDiagramMemberRecord read GetDiagramMember; default;
    property Count: integer read GetCount;
    function Add: TDiagramMemberRecord;
    procedure Remove(member: TDiagram);overload;
    procedure Remove(member: TDiagramMemberRecord); overload;
    procedure Delete(idx: integer);
    procedure ClearSelected;
    property SelectCount: integer read GetSelectCount;
    procedure DragStart;
  end;
  //----------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  TLineSymbol = class(TSymbol)
  private
    procedure Paint(dd: TDiagramDisplay; mr: TDiagramMemberRecord);override;
    procedure PaintMain(dd: TDiagramDisplay; mr: TDiagramMemberRecord);override;
  end;

  TAttachedLineSymbol = class(TLineSymbol)
  private
    FSourceSymbol: TDiagramMemberRecord;
    FtargetSymbol: TDiagramMemberRecord;
    procedure SetSourceSymbol(const Value: TDiagramMemberRecord);
    procedure SetTargetSymbol(const Value: TDiagramMemberRecord);
  public
    constructor create(sName: string; oProject: TUMLProject; parent: TSymbol); override;
    property SourceSymbol: TDiagramMemberRecord read FSourceSymbol write SetSourceSymbol;
    property TargetSymbol: TDiagramMemberRecord read FtargetSymbol write SetTargetSymbol;
    procedure Paint(dd: TDiagramDisplay; mr: TDiagramMemberRecord);override;
    procedure PaintMain(dd: TDiagramDisplay; mr: TDiagramMemberRecord);override;

  end;


  TSquareSymbol = class(TSymbol)
  private
    FDragHookType: TDragHookType;
    FConstrainYminimum: integer;
    FConstrainXminimum: integer;
  protected

  public
    constructor create(sName: string; oProject: TUMLProject; parent: TSymbol); override;
    destructor Destroy; override;
    procedure Paint(dd: TdiagramDisplay; mr: TDiagramMemberRecord);override;
    procedure PaintMain(dd: TDiagramDisplay; mr: TDiagramMemberRecord);override;
    procedure HoverPaint(dd: TdiagramDisplay; mr: TDiagramMemberRecord);override;
    procedure SelectPaint(dd: TdiagramDisplay; mr: TDiagramMemberRecord);override;
    function IsAtPoint(mr: TDiagramMemberRecord; x: integer; y: integer): boolean;override;
    function MouseDrag(dd: TDiagramDisplay; mr: TDiagramMemberRecord; DragDistanceX, DragDistanceY: integer): boolean; override;
    function MouseClick(dd: TDiagramDisplay; mr: TDiagramMemberRecord; ClickPointX, ClickPointY: integer): boolean; override;
    function  GetDragHookType(dd: TDiagramDisplay; mr: TDiagramMemberRecord; ClickPointX, ClickPointY: integer): TDragHookType; override;
    procedure MouseStartDrag(dd: TDiagramDisplay; mr: TDiagramMemberRecord; InitClickGlobalX, InitClickGlobalY: integer); override;
    function GetHeight(mr: TDiagramMemberREcord): integer;
    function GetWidth(mr: TDiagramMemberREcord): integer;
    procedure SetWidth(mr: TDiagramMemberRecord; iWidth: integer);
    procedure SetHeight(mr: TDiagramMemberRecord; iHeight: integer);
    procedure MouseOver(dd: TdiagramDisplay; mr: TDiagramMemberRecord; globalX, globalY: integer);override;
    procedure MouseExit(dd: TdiagramDisplay; mr: TDiagramMemberRecord);override;
    procedure SetTop(mr: TDiagramMemberRecord; iValue: integer);
    procedure SetLeft(mr: TDiagramMemberRecord; iValue: integer);
    procedure SetRight(mr: TDiagramMemberRecord; iValue: integer);
    procedure SetBottom(mr: TDiagramMemberRecord; iValue: integer);
    function GetTop(mr: TDiagramMemberRecord): integer;
    function GetLeft(mr: TDiagramMemberRecord): integer;
    function GetRight(mr: TDiagramMemberRecord): integer;
    function GetBottom(mr: TDiagramMemberRecord): integer;
    function GetIntersectPoint(mr: TDiagramMemberRecord; GlobalX, globaly: real; ddDebug: TDiagramdisplay = nil): Tpoint; override;
    property ConstrainYMinimum: integer read FConstrainYminimum write FConstrainYminimum;
    property ConstrainXMinimum: integer read FConstrainXminimum write FConstrainXminimum;
    function GetCenterX(mr: TDiagrammemberRecord): real;
    function GetCenterY(mr: TDiagrammemberRecord): real;

  end;

  //----------------------------------------------------------------------------
  TLegacyRecord = class(TSquareSymbol)
  private
    FItems: TList<TSymbol>;
    function GetItems(iIndex: integer): TCompartmentItem;
    function GetItemCount: integer;
    function GetMembers(iIndex: integer): TMemberDefinition;
    function GetMemberCount: integer;
  public
    constructor create(sName: string; oProject: TUMLProject; parent: TSymbol); override;
    destructor Destroy; override;
    procedure Paintmain(dd: TdiagramDisplay; mr: TDiagramMemberRecord);override;
    procedure DrawCompartmentItem(dd: TdiagramDisplay; mr: TDiagramMemberRecord; iPosition: integer; sSignature: ansistring);
    function IsRoomForCompartmentItem(dd: TdiagramDisplay; mr: TDiagramMemberRecord; iPosition: integer): boolean;

    //compartment items
    procedure DeleteItem(iIndex: integer);
    function AddItem(sName: string): TcompartmentItem; overload;virtual;
    procedure AddItem(item: TcompartmentItem); overload; virtual;
    property Items[iIndex: integer]: TCompartmentItem read GetItems;
    property ItemCount: integer read GetItemCount;
    procedure Sort;

    function GetItemTypeCount(cClass: TCompartmentClass): integer;
    function GetNthClassInstance(n: integer; cClass: Tcompartmentclass): TCompartmentItem;

    property Members[iIndex: integer]: TMemberDefinition read GetMembers;
    property MemberCount: integer read GetMemberCount;
    function AddMember(sName: ansistring): TMemberDefinition;
    //TODO 2: Allow removal of members

  end;
  //----------------------------------------------------------------------------
(*  TStructure = class(TSquareSymbol)
  private
    FItems: TList;
    function GetItems(iIndex: integer): TCompartmentItem;
    function GetItemCount: integer;
    function GetItems(iIndex: integer): TCompartmentItem;
    function GetMembers(iIndex: integer): TMemberDefinition;
  public
    constructor create(oProject: TUMLProject; parent: TSymbol); override;
    destructor Destroy; override;
    procedure Paintmain(dd: TdiagramDisplay; mr: TDiagramMemberRecord);override;
    procedure DrawCompartmentItem(dd: TdiagramDisplay; mr: TDiagramMemberRecord; iPosition: integer; sSignature: ansistring);
    function IsRoomForCompartmentItem(dd: TdiagramDisplay; mr: TDiagramMemberRecord; iPosition: integer): boolean;

    //compartment items
    procedure DeleteItem(iIndex: integer);
    function AddItem(sName: ansistring): TcompartmentItem; virtual;
    property Items[iIndex: integer]: TCompartmentItem read GetItems;
    property ItemCount: integer read GetItemCount;

    function GetItemTypeCount(cClass: TCompartmentClass): integer;
    function GetNthClassInstance(n: integer; cClass: Tcompartmentclass): TCompartmentItem;

    property Members[iIndex: integer]: TMemberDefinition read GetMembers;
    property MemberCount; integer read GetMemberCount;
    function AddMember(sName: ansistring): TMemberDefinition;
    //TODO 2: Allow removal of members

  end;*)
  //----------------------------------------------------------------------------
  TMatureConstruct = class(TLegacyRecord)
  private
    FunitName: ansistring;
    FAncestors: TResolvableList;
    FTypeParameters: string;
    function GetFunctionCount: integer;
    function GetFunctions(iIndex: integer): TfunctionDefinition;
    function GetProperties(iIndex: integer): TPropertyDefinition;
    function GetPropertyCount: integer;
    function GetAncestor(iIndex: integer): TResolvable;
    function GetAncestorCount: integer;
    function GetAncestorlist: string;
    function GetTypeParemeters: string;
    procedure SetTypeParameters(const Value: string);
  public
    constructor create(sName: string; oProject: TUMLProject; parent: TSymbol); override;
    destructor Destroy;override;
    property Properties[iIndex: integer]: TPropertyDefinition read GetProperties;
    property PropertyCount: integer read GetPropertyCount;
    function AddProperty(sName: ansistring): TPropertyDefinition;

    property Functions[iIndex: integer]: TfunctionDefinition read GetFunctions;
    property FunctionCount: integer read GetFunctionCount;
    function AddFunction(sName: ansistring): TFunctionDefinition;
    function FindFunction(sName: ansistring): TFunctionDefinition;

    property UnitName: ansistring read FUnitName write FUnitName;

    property Ancestors[iIndex: integer]: TResolvable read GetAncestor;
    procedure AddAncestor(sym: TMatureConstruct);overload;
    procedure AddAncestor(sUnresolvedName: string);overload;
    property AncestorCount: integer read GetAncestorCount;
    property AncestorList: string read GetAncestorlist;
    property TypeParameters: string read GetTypeParemeters write SetTypeParameters;
  end;

  TRecord = class(TMatureConstruct)
  end;

  TClassDefinition = class(TMatureConstruct)
  end;
  TStructure = class(TMatureConstruct)
  end;

  TVariableSymbol = class(TSymbol)
  private
    FtypeName: ansistring;
    FStorageClass: TSymbol;
  public
    property StorageClass: TSymbol read FStorageClass write FStorageClass;
    property TypeName: ansistring read FtypeName write FTypeName;

  end;

  //----------------------------------------------------------------------------
  TUnitDefinition = class(TMatureConstruct)
  end;
  //----------------------------------------------------------------------------
  TSymbolList = class(TLockQueuedObject)
  private
    FSymbols : TList<TSymbol>;
    FProject: TUMLProject;
    FOwnerName: string;
    function GetCount: integer;
    function GetSymbol(idx: integer): TSymbol;
    function GetProject: TUMLProject;
    procedure SetProject(const Value: TUMLProject);
  public
    constructor create(oProject: TUMLProject); reintroduce; virtual;
    destructor Destroy; override;
    property Symbols[idx: integer]: TSymbol read GetSymbol;default;
    property Count: integer read GetCount;
    property Project: TUMLProject read GetProject write SetProject;
    procedure RegisterSymbol(sym: TSymbol);
    procedure DeRegisterSymbol(sym: TSymbol);
    procedure Add(sym: TSymbol);
    procedure Remove(sym: TSymbol);
    procedure Delete(idx: integer);
    function HasSymbol(sym: TSymbol): boolean;overload;
    function HasSymbol(sym: ansistring): boolean;overload;
    function IndexOfSymbol(sym: TSymbol): integer;overload;
    function IndexOfSymbol(sym: ansistring): integer;overload;
    function FindSymbol(sym: ansistring): TSymbol;
    procedure Sort;
    procedure Clear;
    property OwnerName: string read FownerName write FownerName;
  end;
  //----------------------------------------------------------------------------
  TDiagram = class(TObject)
  private
    FMembers: TDiagramMemberList;
    FProject: TUMLProject;
    FName: ansistring;
    function GetMembers: TDiagramMemberList;
    function GetMaxX: integer;
    function GetMaxY: integer;
    function GetMinX: integer;
    function GetMinY: integer;
  public
    constructor create(oProject: TUMLProject); reintroduce; virtual;
    destructor Destroy; override;
    property Name: ansistring read FName write FName;
    property Members: TDiagramMemberList read GetMembers;
    property Project: TUMLProject read FProject write FProject;
    property MaxX: integer read GetMaxX;
    property MaxY: integer read GetMaxY;
  end;
  //----------------------------------------------------------------------------

  TDiagramList = class(TLockQueuedObject)
  private
    FDiagrams: TList;
    FProject: TUMLProject;

    function GetCount: integer;
    function GetDiagram(idx: integer): TDiagram;
    function GetProject: TUMLProject;
    procedure SetProject(const Value: TUMLProject);
  public
    constructor create(oProject: TUMLProject); reintroduce; virtual;
    destructor Destroy; override;
    property Diagrams[idx: integer]: TDiagram read GetDiagram;default;
    property Count: integer read GetCount;
    property Project: TUMLProject read GetProject write SetProject;
    procedure Add(dgm: TDiagram);
    procedure Remove(diagram: TDiagram);
    procedure Delete(idx: integer);
  end;

  //----------------------------------------------------------------------------
  TUMLProject = class(TLockQueuedObject)
  private
    FSymbols: TSymbolList;
    FDiagrams: TDiagramList;
    FFileName: ansistring;
    FTitle: ansistring;
    FSourceFiles: TStringList;
    FCompileQueue: TStringLIst;
    FProjectCompiler: TCompilerClass;
    FOnProgress: TCompilerProgressEvent;

    function GetDiagrams: TDiagramList;
    function GetFileName: ansistring;
    function GetSymbols: TSymbolList;
    procedure SetFileName(const Value: ansistring);
    function GetTitle: ansistring;
    procedure SetTitle(const Value: ansistring);
    function GetSourceFiles(idx: integer): ansistring;
    function GetSourceFileCount: integer;
  public
    constructor create; override;
    destructor Destroy; override;
    property Title: ansistring Read GetTitle write SetTitle;
    property FileName: ansistring read GetFileName write SetFileName;
    property Diagrams: TDiagramList read GetDiagrams;
    property Symbols: TSymbolList read GetSymbols;
    procedure Save;

    property SourceFiles[idx: integer]: ansistring read GetSourceFiles;
    property SourceFileCount: integer read GetSourceFileCount;
    procedure AddSourceFile(sFullName: ansistring);
    procedure AddSourceDir(sPath, sFilter: ansistring; bRecurse: boolean = true);

    procedure Compile(sFileName: ansistring);
    procedure CompileAll;

    procedure QueueCompile(sFileNAme: ansistring);
    procedure CompileQueue;

    function SymbolToUnitName(sSym: ansistring): ansistring;


    property ProjectCompiler: TCompilerClass read FProjectCompiler write FProjectCompiler;
    property OnProgress: TCompilerProgressEvent read FOnProgress write FOnProgress;
    procedure Clear;


  end;

  TMacro = class(TSymbol)
  private
    FCode: ansistring;
  public
    property Code: ansistring read FCode write FCode;
  end;

  TResolvable = class(TBetterObject)
  public
    name: string;
    sym: TSymbol;
    namespace: TNameSpace;
    function IsResolved: boolean;
    procedure Resolve;
  end;

  TNameSpace = class(TList<TResolvable>)
  private
    FProject: TUMLProject;
    FSpaceName: string;
    FParentSpace: TNameSpace;
    procedure SetProject(const Value: TUMLProject);
  public
    procedure AddUnresolved(sName: string);
    procedure ResolveAll;
    function Resolve(sSymName: string): TResolvable;

    property Project: TUMLProject read FProject write SetProject;
    property SpaceName: string read FSpaceName write FSpaceName;
    property ParentSpace: TNameSpace read FParentSpace write FParentSpace;
  end;

  TResolvableList = class(TNameSpace);





  IEventListener = interface(IUnknown)
  end;

function GetSlope(ax, ay, bx, by: Real): TSlopeResult;
function ProjectToXML(project: TUMLProject): ansistring;


procedure Register;

implementation

uses Dir, pascalcompiler, stringx, DirFile, ccompiler;

//------------------------------------------------------------------------------
function GetSlope(ax, ay, bx, by: Real): TSlopeResult;
var
  sink: real;
  run: real;
begin
  sink := by-ay;
  run := bx-ax;

  //if NOT verticle
  if run<>0 then begin
    result.slope := sink/run;
    result.angle := GetLineAngle(ax,ay,bx,by);

  end
  //handle verticles
  else begin
    result.slope := sink;
    result.undefined := true;
    //define angle
    if sink <= 0 then
      result.angle := 0
    else
      result.angle := DegToRad(180);

  end;


end;


procedure WaitForRetrace;
begin
(*  asm
    mov dx, $3da
@retraceloop:
    in al,dx
    test al, 8
    jz @retraceloop
  end;*)
end;

{ TDiagramList }
//------------------------------------------------------------------------------
procedure TDiagramList.Add(dgm: TDiagram);
begin
  LockWrite;
  try
    FDiagrams.Add(dgm)
  finally
    UnlockWrite;
  end;

end;

constructor TDiagramList.create(oProject: TUMLProject);
begin
  inherited Create;
  FDiagrams := Tlist.create;
  FProject := oProject;
end;
//------------------------------------------------------------------------------
procedure TDiagramList.Delete(idx: integer);
begin
  LockWrite;
  try
    Fdiagrams.delete(idx);
  finally
    UnlockWrite;
  end;
end;
//------------------------------------------------------------------------------
destructor TDiagramList.Destroy;
begin
  FDiagrams.free;
  inherited;
end;
//------------------------------------------------------------------------------
function TDiagramList.GetCount: integer;
begin
  LockRead;
  try
    result := FDiagrams.count;
  finally
    UnlockRead;
  end;
end;
//------------------------------------------------------------------------------
function TDiagramList.GetDiagram(idx: integer): TDiagram;
begin
  LockRead;
  try
    result := TDiagram(Fdiagrams[idx]);
  finally
    UnlockRead;
  end;

end;
//------------------------------------------------------------------------------
function TDiagramList.GetProject: TUMLProject;
begin
  LockRead;
  try
    result := FProject;
  finally
    UnlockRead;
  end;
end;

procedure TDiagramList.Remove(diagram: TDiagram);
begin
  LockWrite;
  try
    Fdiagrams.remove(diagram);
  finally
    UnlockWrite;
  end;

end;

{ TDiagram }

constructor TDiagram.create(oProject: TUMLProject);
begin
  FName := 'new diagram';
  FProject := oProject;
  Fmembers := TDiagramMemberlist.create(oProject);
  FProject.Diagrams.Add(self);

end;
//------------------------------------------------------------------------------
destructor TDiagram.Destroy;
begin
  FMembers.free;
  inherited;
end;
//------------------------------------------------------------------------------
function TDiagram.GetMaxX: integer;
var
  t,u: integer;
  max: integer;
begin
  //for all member records
  max := 0;
  for t := 0 to self.Members.Count-1 do begin
    if self.Members[t].Points.MaxX > max then
      max := self.Members[t].Points.MaxX;
  end;

  result := max;
end;

function TDiagram.GetMaxY: integer;
var
  t,u: integer;
  max: integer;
begin
  //for all member records
  max := 0;
  for t := 0 to self.Members.Count-1 do begin
    if self.Members[t].Points.MaxY > max then
      max := self.Members[t].Points.MaxY;
  end;

  result := max;
end;

//------------------------------------------------------------------------------
function TDiagram.GetMinX: integer;
var
  t,u: integer;
  min: integer;
begin
  //for all member records
  min := 0;
  for t := 0 to self.Members.Count-1 do begin
    if self.Members[t].Points.MinX < min then
      min := self.Members[t].Points.MinX;
  end;

  result := min;
end;
//------------------------------------------------------------------------------

function TDiagram.GetminY: integer;
var
  t,u: integer;
  min: integer;
begin
  //for all member records
  min := 0;
  for t := 0 to self.Members.Count-1 do begin
    if self.Members[t].Points.minY < min then
      min := self.Members[t].Points.minY;
  end;

  result := min;
end;

function TDiagram.GetMembers: TDiagramMemberList;
begin
  result := FMembers;
end;

{ TPointList }

//------------------------------------------------------------------------------
procedure TPointList.Add(point: TPoint);
begin
  SetLength(Fpoints, length(FPoints)+1);
  Fpoints[length(FPoints)-1] := point;
end;

//------------------------------------------------------------------------------
procedure TPointList.Change(idx, x, y: integer);
begin
  FPoints[idx].X := x;
  FPoints[idx].Y := y;
end;

constructor TPointList.create;
begin
  inherited create;
  SetLength(Fpoints, 0);

end;
//------------------------------------------------------------------------------
procedure TPointList.Delete(idx: integer);
var
  t: integer;
begin
  for t:= idx+1 to length(FPoints) do begin
    Fpoints[idx-1] := Fpoints[idx];
  end;
  SetLength(Fpoints, length(FPoints)-1);

end;

//------------------------------------------------------------------------------
destructor TPointList.Destroy;
begin

  inherited;
end;
//------------------------------------------------------------------------------
function TPointList.GetCount: integer;
begin
  result := length(FPoints);
end;
//------------------------------------------------------------------------------
function TPointList.GetMaxX: integer;
var
  t: integer;
  max: integer;
begin
  max :=0;
  for t:=0 to Count-1 do begin
    if Fpoints[t].X>max then
      max := Fpoints[t].X;
  end;

  result := max;
end;
//------------------------------------------------------------------------------
function TPointList.GetMaxY: integer;
var
  t: integer;
  max: integer;
begin
  max :=0;
  for t:=0 to Count-1 do begin
    if Fpoints[t].Y>max then
      max := Fpoints[t].Y;
  end;

  result := max;
end;
//------------------------------------------------------------------------------
function TPointList.GetMinX: integer;
//NOTE: if none <0 then minimum = 0;
var
  t: integer;
  min: integer;
begin
  Min :=0;
  for t:=0 to Count-1 do begin
    if Fpoints[t].X<min then
      min := Fpoints[t].X;
  end;

  result := min;
end;
//------------------------------------------------------------------------------
function TPointList.GetMinY: integer;
//NOTE: if none <0 then minimum = 0;
var
  t: integer;
  min: integer;
begin
  Min := 0;
  for t:=0 to Count-1 do begin
    if FPoints[t].Y<min then
      min := Fpoints[t].Y;
  end;
  result := min;
end;
//------------------------------------------------------------------------------
function TPointList.GetPoints(index: integer): TPoint;
begin
  if count < 2 then begin
    add(point(0,0));
    add(point(50,50));
  end;
  result := FPoints[index];
end;
//------------------------------------------------------------------------------
procedure TPointList.Remove(point: TPoint);
begin
  raise Exception.create('Tpointlist.remove is not implemented');

end;
//------------------------------------------------------------------------------
procedure TPointList.SetPoints(index: integer; const Value: TPoint);
begin
  Fpoints[index] := value;
end;

{ TDiagramMemberRecord }

//------------------------------------------------------------------------------
constructor TDiagramMemberRecord.create(oProject: TUMLProject; diagram: TDiagram);
begin
  inherited create;
  FSelected := false;
  FProject := oProject;
  Fdiagram := diagram;
  FSymbol := nil;
  FPoints := TPointList.create;
end;
//------------------------------------------------------------------------------
destructor TDiagramMemberRecord.Destroy;
begin

  inherited;
end;
//------------------------------------------------------------------------------
procedure TDiagramMemberRecord.DragStart;
//Records point[0] for the purposes of dragging and multi-select dragging
begin
  DragStartX := points[0].x;
  DragStartY := points[0].y;

end;

function TDiagramMemberRecord.GetDragDistanceX: integer;
begin

//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
function TDiagramMemberRecord.GetDragDistanceY: integer;
begin

//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
function TDiagramMemberRecord.GetOwner: TDiagram;
begin
  result := FDiagram;
end;

//------------------------------------------------------------------------------
function TDiagramMemberRecord.GetPointList: TPointList;
begin
  result := FPoints;
end;

//------------------------------------------------------------------------------
procedure TDiagramMemberRecord.Paint(dd: TdiagramDisplay);
begin
  if assigned(FSymbol) then begin
    self.Symbol.Paint(dd, self);
  end;

end;

//------------------------------------------------------------------------------
procedure TDiagramMemberRecord.SetDragStartX(const Value: integer);
begin
  FDragStartX := Value;

//  FDragOffsetX :=

end;

procedure TDiagramMemberRecord.SetDragStartY(const Value: integer);
begin
  FDragStartY := Value;
end;
//------------------------------------------------------------------------------
procedure TDiagramMemberRecord.SetMouseOver(const Value: boolean);
begin
  FMouseOver := Value;
end;
//------------------------------------------------------------------------------
procedure TDiagramMemberRecord.SetSelected(const Value: boolean);
begin
  FSelected := Value;

end;

//------------------------------------------------------------------------------
procedure TDiagramMemberRecord.SetSymbol(const Value: TSymbol);
begin
  FSymbol := Value;
end;

{ TDiagramMemberList }
//------------------------------------------------------------------------------
function TDiagramMemberList.Add: TDiagramMemberRecord;
begin
  result := TDiagramMemberRecord.create(project, nil);
  FMembers.add(result);
end;
//------------------------------------------------------------------------------
procedure TDiagramMemberList.ClearSelected;
var
  t: integer;
begin
  for t:=0 to self.Count-1 do begin
    self.Members[t].Selected := false;
  end;
end;
//------------------------------------------------------------------------------
constructor TDiagramMemberList.create;
begin
  inherited create;
  FMembers := TList.create;
  FProject := oProject;
end;
//------------------------------------------------------------------------------
procedure TDiagramMemberList.Delete(idx: integer);
begin
  FMembers.delete(idx);
end;
//------------------------------------------------------------------------------
destructor TDiagramMemberList.Destroy;
begin
  FMembers.free;
  inherited;
end;

procedure TDiagramMemberList.DragStart;
//records the drag start position for all member records
var
  t: integer;
begin
  for t:= 0 to count-1 do begin
    members[t].DragStart;
  end;
end;

function TDiagramMemberList.GetCount: integer;
begin
  result := Fmembers.count;
end;

function TDiagramMemberList.GetDiagramMember(
  idx: integer): TDiagramMemberRecord;
begin
  result := TDiagramMemberRecord(Fmembers[idx]);

end;
//------------------------------------------------------------------------------
function TDiagramMemberList.GetSelectCount: integer;
var
  t: integer;
begin
  result := 0;
  for t:= 0 to count-1 do begin
    if members[t].Selected then
      inc(result);
  end;

end;

procedure TDiagramMemberList.Remove(member: TDiagramMemberRecord);
begin
  FMembers.remove(member);
end;
//------------------------------------------------------------------------------
procedure TDiagramMemberList.Remove(member: TDiagram);
begin
  raise Exception.Create('TDiagramMemberList.remove not implemented');
end;

{ TSymbolList }
//------------------------------------------------------------------------------
procedure TSymbolList.Add(sym: TSymbol);
var
  existing: TSymbol;

begin
  if self = nil then
    raise Exception.create('WTF self is nIL!');

  LockWrite;
  try
    existing := self.FindSymbol(sym.Name);

    if existing = sym then exit;
    if existing <> nil then begin

      existing.Parent := nil;
      sym.CopyFrom(existing);


//      GLOG.Debug('symbol '+sym.name+' already exists... will remove');
      //self.remove(existing);
//      GLOG.Debug('symbol '+sym.name+' already exists... removed');

      existing.free;

    end;
    if sym = nil then begin
      log(self,'symbol '+sym.name+' was not found in TSymbolList.Add so it was not added');
      exit;
    end else begin
      debug.log('Adding '+sym.classname+':'+sym.name+' at '+FSymbols.count.tostring+' as child of '+self.ownername);
      FSymbols.Add(sym);
    end;
  finally
    UnlockWrite;
  end;

end;
//------------------------------------------------------------------------------
procedure TSymbolList.Clear;
var
  s: TSymbol;
begin
  while self.Count > 0 do begin
    s := symbols[0];
    self.Delete(0);
    s.free;
  end;
end;

constructor TSymbolList.create(oProject: TUMLProject);
begin
  inherited Create;
  FProject := oProject;
  FSymbols := TList<TSymbol>.create;
end;
//------------------------------------------------------------------------------
procedure TSymbolList.Delete(idx: integer);
begin
  LockWrite;
  try
    FSymbols.Delete(idx);
  finally
    UnlockWrite;
  end;

end;
//------------------------------------------------------------------------------
procedure TSymbol.DeRegisterWithParent;
begin
  if HasParent then
    parent.DeRegisterChild(self);

  FParent := nil;
end;
//------------------------------------------------------------------------------
procedure TSymbolList.DeRegisterSymbol(sym: TSymbol);
begin
  LockWrite;
  try
    if self.HasSymbol(sym) then
      self.Remove(sym);
  finally
    UnlockWrite;
  end;
end;

destructor TSymbolList.Destroy;
begin
  FSymbols.free;
  inherited;
end;
//------------------------------------------------------------------------------
function TSymbolList.FindSymbol(sym: ansistring): TSymbol;
var
  i: integer;
begin
  LockRead;
  try
    i := self.IndexOfSymbol(sym);

    if i>-1 then
      result := self.Symbols[i]
    else
      result := nil;
  finally
    UnlockRead;
  end;

end;

function TSymbolList.GetCount: integer;
begin
  LockRead;
  try
    result := FSymbols.count;
  finally
    UnlockRead;
  end;

end;
//------------------------------------------------------------------------------
function TSymbolList.GetProject: TUMLProject;
begin
  LockRead;
  try
    result := FProject;

  finally
    UnlockRead;
  end;
end;

function TSymbolList.GetSymbol(idx: integer): TSymbol;
begin
  LockRead;
  try
    result := FSymbols[idx];
  finally
    UnlockRead;
  end;
end;
//------------------------------------------------------------------------------
procedure TSymbol.RegisterWithParent;
begin
  if HasParent then
    parent.RegisterChild(self);
end;
//------------------------------------------------------------------------------
function TSymbolList.HasSymbol(sym: TSymbol): boolean;
begin
  LockRead;
  try
    result := IndexOfSymbol(sym) >=0;
  finally
    UnlockRead;
  end;

end;
//------------------------------------------------------------------------------
function TSymbolList.HasSymbol(sym: ansistring): boolean;
begin
  LockRead;
  try
    result := IndexOfsymbol(sym) >= 0;
  finally
    UnlockRead;
  end;

end;

function TSymbolList.IndexOfSymbol(sym: ansistring): integer;
var
  t: integer;
begin
  LockRead;
  try
    result := -1;
    sym := lowercase(sym);
    for t:= 0 to self.count-1 do begin
      if lowercase(symbols[t].Name) = sym then begin
        result := t;
        break;
      end;
    end;

  finally
    UnlockRead;
  end;

end;
//------------------------------------------------------------------------------
function TSymbolList.IndexOfSymbol(sym: TSymbol): integer;
var
  t: integer;
begin
  LockRead;
  try
    result := -1;
    for t:= 0 to self.count-1 do begin
      if symbols[t] = sym then begin
        result := t;
        break;
      end;
    end;
  finally
    UnlockRead;
  end;


end;



procedure TSymbolList.RegisterSymbol(sym: TSymbol);
begin
  LockWrite;
  try
    if not HasSymbol(sym) then
      add(sym);
 
  finally
    UnlockWrite;
  end;

end;

procedure TSymbolList.Remove(sym: TSymbol);
begin
  LockWrite;
  try
    FSymbols.Remove(sym);
  finally
    UnlockWrite;
  end;

end;

{ TSquareSymbol }
//------------------------------------------------------------------------------
constructor TSquareSymbol.create(sNAme: string; oProject: TUMLProject; parent: TSymbol);
begin
  inherited create(sName, oProject, parent);
  FConstrainYMinimum := 0;
  FConstrainXminimum := 0;

end;
//------------------------------------------------------------------------------
destructor TSquareSymbol.Destroy;
begin

  inherited;
end;

{ TProject }
//------------------------------------------------------------------------------
procedure TUMLProject.AddSourceDir(sPath, sFilter: ansistring; bRecurse: boolean = true);
var
  dir: TDirectory;
  t: integer;
begin
  dir := TDirectory.create(sPath, sFilter, 0,0,true);
  try
    for t:= 0 to dir.FileCount-1 do begin
      log(self,dir.Files[t].FullName);
      AddSourceFile(dir.Files[t].FullName);
    end;

    if bRecurse then
    for t:= 0 to dir.FolderCount-1 do begin
      AddSourceDir(dir.Folders[t].FullName, sFilter, true);
    end;

  finally
    dir.free;
  end;
end;

procedure TUMLProject.AddSourceFile(sFullName: ansistring);
begin
  LockString;
  try
    UniqueString(sFullName);
    FSourceFiles.add(sFullName);



  finally
    UnlockString;
  end;
end;


procedure TUMLProject.Clear;
begin
  Symbols.clear;
end;

procedure TUMLProject.Compile(sFileName: ansistring);
var
  compiler: TCompiler;
  sym: Tsymbol;
  s: ansistring;
begin
  if ProjectCompiler = nil then begin
    s := lowercase(extractfileext(sFileName));
    if  (s = '.pas') or (s = '.dpr') then begin
      ProjectCompiler := TPascalCompiler;
    end else begin
      ProjectCompiler := TCCompiler;
    end;

  end;
  compiler := TCompiler(ProjectCompiler.create);
  compiler.OnProgress := self.OnProgress;
  try
    log(self,'Compiling '+sFileName+'...','compiler');
//    compiler.Input := LoadStringFromFile(sFileName);
    compiler.project := self;


    if not compiler.compile(sFileName)  then begin
      log(self,compiler.output,'compiler');
      beeper.beep(100,100);
      //sleep(1000);
    end;

    CompileQueue;



  finally
    compiler.free;
  end;

end;


procedure TUMLProject.CompileAll;
var
  sFile: ansistring;
  t: integer;
begin

  for t:= 0 to self.SourceFileCount-1 do begin
    sFile := self.SourceFiles[t];
    try
      log(self,'Compiling: '+sFile);
      self.Compile(sFile);
    except
      on E: Exception do begin
        log(self,'Exception compiling '+sFile+'...'+e.message);
      end;
    end;
  end;

end;

procedure TUMLProject.CompileQueue;
var
  comp: TPascalCompiler;
  sFile: ansistring;
begin
  comp := TPascalCompiler.create;
  try
    comp.Project := self;

    while self.FCompileQueue.Count > 0 do begin
      sFile := SymbolToUnitName(FCompileQueue[0]);

      if (sFile <> '') and (NOT self.Symbols.HasSymbol(FCompileQueue[0])) then begin
         try
          log(self,'Compiling: '+FCompileQueue[0]);

          comp.Compile(sFile);
        except
          On E: Exception do begin
            log(self,'***'+e.Message);
          end;
        end;
      end;


      FCompileQueue.delete(0);
    end;

  finally
    comp.free;
  end;


end;

constructor TUMLProject.create;
begin
  inherited;
  FSymbols := TSymbolList.create(self);
  FSymbols.ownername := 'Project';
  FDiagrams := TDiagramList.create(self);
  FSourceFiles := TStringLIst.create;
  FFileName := '';
  FCompileQueue := TStringLIst.create;

end;
//------------------------------------------------------------------------------
destructor TUMLProject.Destroy;
begin
  FSymbols.free;
  Fdiagrams.free;
  FSourceFiles.Free;
  FCompileQueue.free;
  inherited;
end;
//------------------------------------------------------------------------------
function TUMLProject.GetDiagrams: TDiagramList;
begin
  result := Fdiagrams;
end;
//------------------------------------------------------------------------------
function TUMLProject.GetFileName: ansistring;
begin
  result := FFileName;
end;
//------------------------------------------------------------------------------
function TUMLProject.GetSourceFileCount: integer;
begin
  LockRead;
  try
    result := FSourceFiles.count;
  finally
    UnlockRead;
  end;
end;

function TUMLProject.GetSourceFiles(idx: integer): ansistring;
begin
  LockString;
  try
    result := FSourceFiles[idx];
    UniqueString(result);

  finally
    UnlockString;
  end;
end;

function TUMLProject.GetSymbols: TSymbolList;
begin
  result := FSymbols;
end;
//------------------------------------------------------------------------------
function TUMLProject.GetTitle: ansistring;
begin
  LockString;
  try
    result := FTitle;
    UniqueString(result);
  finally
    UnlockString;
  end;
end;

procedure TUMLProject.QueueCompile(sFileNAme: ansistring);
begin
  if self.Symbols.HasSymbol(sFileName) then
    exit;

  self.FCompileQueue.Add(sFileName);



end;

procedure TUMLProject.Save;
begin
  raise Exception.create('Not Implemented');
end;
//------------------------------------------------------------------------------
procedure TUMLProject.SetFileName(const Value: ansistring);
begin
  FFileName := Value;
end;
//------------------------------------------------------------------------------

function TSquareSymbol.GetBottom(mr: TDiagramMemberRecord): integer;
begin
  result := mr.Points[1].Y;
end;

function TSquareSymbol.GetCenterX(mr: TDiagrammemberRecord): real;
begin
  result := ((Getright(mr)-GetLeft(mr))/2)+GetLeft(mr);

end;

function TSquareSymbol.GetCenterY(mr: TDiagrammemberRecord): real;
begin
  result := ((GetBottom(mr)-GetTop(mr))/2)+GetTop(mr);
end;

function TSquareSymbol.GetDragHookType(dd: TDiagramDisplay;
  mr: TDiagramMemberRecord; ClickPointX,
  ClickPointY: integer): TDragHookType;
var
  xborder, yborder: real;
const
  MOUSE_BORDER = 3.0;
begin
  xborder := dd.ScaleScreenXtoGlobal(8);//Round(dd.ConvertInX(MOUSE_BORDER));
  yborder := dd.ScaleScreenXtoGlobal(8);//Round(dd.ConvertInY(MOUSE_BORDER));

//  dd.Rectangle(ClickpointX-dd.ScaleXToGlobal(1), ClickpointY-dd.ScaleYToGlobal(1), clickpointx+dd.ScaleXToGlobal(1),clickpointy+dd.ScaleYToGlobal(1),0, true);

  ClickPointX := clickpointX - mr.Points[0].X;
  ClickPointY := clickpointY - mr.Points[0].Y;

//  dd.Rectangle(ClickpointX-dd.ScaleXToGlobal(1), ClickpointY-dd.ScaleYToGlobal(1), clickpointx+dd.ScaleXToGlobal(1),clickpointy+dd.ScaleYToGlobal(1),255*65536, true);
  if dd.Diagram.Members.SelectCount > 1 then
    result := dhAll
  else
  if (ClickPointX < xborder) and (ClickPOintY < yborder) then
    result := dhTopLeft
  else
  if (ClickPointX > GetWidth(mr)-xborder) and (ClickPointY < yborder) then
    result := dhTopRight
  else
  if (ClickPointX > GetWidth(mr)-xborder) and (ClickPointY > GetHeight(mr)-yborder) then
    result := dhBottomRight
  else
  if (ClickPointX < xborder) and (ClickPointY > GetHeight(mr)-yborder) then
    result := dhBottomLeft
  else
  if (ClickPointX < xborder) then
    result := dhLeft
  else
  if (ClickPointX > GetWidth(mr)-xborder) then
    result := dhRight
  else
  if (ClickPointY < yborder) then
    result := dhTop
  else
  if (ClickPointY > GetHeight(mr)-yborder) then
    result := dhBottom
  else begin
    result := dhAll;
    //showmessage(inttostr(ClickPointX)+', '+inttostr(ClickPointY)+', '+inttostr(GetHeight(mr)-yborder));
  end;

end;

function TSquareSymbol.GetHeight(mr: TDiagramMemberRecord): integer;
begin
  result := mr.points[1].Y - mr.points[0].Y
end;
//------------------------------------------------------------------------------
function TSquareSymbol.GetIntersectPoint(mr: TDiagramMemberRecord; GlobalX,
  globaly: real; ddDebug: TDiagramdisplay = nil): Tpoint;
var
  cx,cy,sink, run, slope: real;
  x,y,x0,x1,y0, y1: real;
  rx1,rx2,ry1,ry2: real;
begin
  //globalX := 200;
  //globaly := 200;

  cx := GetCenterX(mr);
  cy := GetCenterY(mr);

  //find slope of line from given to center of square
  sink := globaly-cy;
  run := globalx-cx;
  if run <> 0 then
    slope := sink/run
  else
    slope := 0;
  if assigned(ddDebug) then
    dddebug.Text(cx,cy-45,600,15, 'cx='+floattostr(cx)+' cy='+floattostr(cy)+' run='+floattostr(run)+' slope='+floattostr(slope)+' sink='+floattostr(sink), false, false);
  //find point with identical slope that intersects vertical bound

  {determine which edge to use}
  x0 := mr.points[0].x;
  x1 := mr.points[1].x;
  if abs(globalx-x0)<abs(globalx-x1) then
    x := x0
  else
    x := x1;

  if assigned(ddDebug) then
    ddDebug.line(x, ddDebug.boundY1, x, ddDebug.BoundY2, clRed);

  {determine distance to edge}
  run := x-cx;
  sink := slope*run;

  {plot point}
  rx1 := x;
  ry1 := cy+sink;

  if assigned(ddDebug) then begin
    dddebug.Text(cx,cy-25,600,15, 'gx='+floattostr(globalx)+'gy='+floattostr(globalx)+'cx='+floattostr(cx)+' cy='+floattostr(cy)+' run='+floattostr(run)+' slope='+floattostr(slope)+' sink='+floattostr(sink), false, false);
    dddebug.Text(cx,cy-10,600,15, 'rx1='+floattostr(rx1)+' ry1='+floattostr(ry1), false, false);
  end;



  //find a point with identical slope that intersect horizontal bound

  {determine which edge to use}
  y0 := mr.points[0].y;
  y1 := mr.points[1].y;
  if abs(globaly-y0)<abs(globaly-y1) then
    y := y0
  else
    y := y1;

  if assigned(ddDebug) then
    ddDebug.line(ddDebug.boundX1,y,ddDebug.BoundX2,y, clRed);


  {determine cy-y}
  sink := y-cy;
  if slope <> 0 then
    run := sink/slope
  else
    run := 0;

  {plot point}
  rx2 := cx+run;
  ry2 := y;

  if assigned(ddDebug) then
    ddDebug.line(rx1,ry1,cx,cy, clBlue);

  if assigned(ddDebug) then
    ddDebug.line(rx2,ry2,cx,cy, clBlue);

  //determine which result to use based on which is closest to the center
  if (Getpointdistance(rx1,ry1, cx,cy) < GetpointDistance(rx2,ry2,cx,cy)) then begin


    result.X := round(rx1);
    result.y := round(ry1);
  end else begin
    result.X := round(rx2);
    result.y := round(ry2);
  end;


end;

function TSquareSymbol.GetLeft(mr: TDiagramMemberRecord): integer;
begin
  result := mr.Points[0].X;
end;

function TSquareSymbol.GetRight(mr: TDiagramMemberRecord): integer;
begin
  result := mr.Points[1].X;
end;

function TSquareSymbol.GetTop(mr: TDiagramMemberRecord): integer;
begin
  result := mr.Points[0].Y;
end;

function TSquareSymbol.GetWidth(mr: TDiagramMemberRecord): integer;
begin
  result := mr.points[1].X - mr.points[0].X
end;

procedure TSquareSymbol.HoverPaint(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord);
CONST
  BSZ = 5;
var
  bs: real;
begin
  inherited;

  BS := dd.ScaleScreenXtoGlobal(BSZ);

  //***black boxes
  //top left
  dd.Rectangle(GetLeft(mr), GetTop(mr), GetLeft(mr)+BS, GetTop(mr)+BS, clBlack, true);
  //top right
  dd.Rectangle(GetRight(mr)-BS, GetTop(mr), getRight(mr), GetTop(mr)+BS, clBlack, true);
  //bottom right
  dd.Rectangle(GetRight(mr)-BS, GetBottom(mr)-BS, getRight(mr), GetBottom(mr), clBlack, true);
  //bottom left
  dd.Rectangle(GetLeft(mr), GetBottom(mr)-BS, GetLeft(mr)+BS, GetBottom(mr), clBlack, true);

  //middle left
  dd.Rectangle(GetLeft(mr), GetTop(mr)+((GetHeight(mr)/2)-(BS/2)), GetLeft(mr)+BS, GetTop(mr)+((GetHeight(mr)/2)+(BS/2)), clBlack, true);
  //middle right
  dd.Rectangle(GetRight(mr)-BS, GetTop(mr)+((GetHeight(mr)/2)-(BS/2)), GetRight(mr), GetTop(mr)+((GetHeight(mr)/2)+(BS/2)), clBlack, true);

  //middle top
  dd.Rectangle(GetLeft(mr)+((GetWidth(mr)/2)-(BS/2)), GetTop(mr), GetLeft(mr)+((GetWidth(mr)/2)+(BS/2)), GetTop(mr)+BS, clBlack, true);
  //middle bottom
  dd.Rectangle(GetLeft(mr)+((GetWidth(mr)/2)-(BS/2)), GetBottom(mr)-BS, GetLeft(mr)+((GetWidth(mr)/2)+(BS/2)), GetBottom(mr), clBlack, true);




  //***white border around black boxes
  //top left
  dd.Rectangle(GetLeft(mr), GetTop(mr), GetLeft(mr)+BS, GetTop(mr)+BS, clWhite);
  //top right
  dd.Rectangle(GetRight(mr)-BS, GetTop(mr), getRight(mr), GetTop(mr)+BS, clWhite);
  //bottom right
  dd.Rectangle(GetRight(mr)-BS, GetBottom(mr)-BS, getRight(mr), GetBottom(mr), clWhite);
  //bottom left
  dd.Rectangle(GetLeft(mr), GetBottom(mr)-BS, GetLeft(mr)+BS, GetBottom(mr), clWhite);

  //middle left
  dd.Rectangle(GetLeft(mr), GetTop(mr)+((GetHeight(mr)/2)-(BS/2)), GetLeft(mr)+BS, GetTop(mr)+((GetHeight(mr)/2)+(BS/2)), clWhite, false);
  //middle right
  dd.Rectangle(GetRight(mr)-BS, GetTop(mr)+((GetHeight(mr)/2)-(BS/2)), GetRight(mr), GetTop(mr)+((GetHeight(mr)/2)+(BS/2)), clWhite, false);

  //middle top
  dd.Rectangle(GetLeft(mr)+((GetWidth(mr)/2)-(BS/2)), GetTop(mr), GetLeft(mr)+((GetWidth(mr)/2)+(BS/2)), GetTop(mr)+BS, clWhite, false);
  //middle bottom
  dd.Rectangle(GetLeft(mr)+((GetWidth(mr)/2)-(BS/2)), GetBottom(mr)-BS, GetLeft(mr)+((GetWidth(mr)/2)+(BS/2)), GetBottom(mr), clWhite, false);


end;

function TSquareSymbol.IsAtPoint(mr: TDiagramMemberRecord; x,
  y: integer): boolean;
begin
  if mr.Points.count<2 then
    result := false
  else
    result := ((mr.Points[0].x)<=x) and (x<=(mr.Points[1].x)) and
              ((mr.Points[0].y)<=y) and (y<=(mr.Points[1].y));
end;

function TSquareSymbol.MouseClick(dd: TDiagramDisplay; mr: TDiagramMemberRecord; ClickPointX,
  ClickPointY: integer): boolean;
begin
  result := false;
end;

function TSquareSymbol.MouseDrag(dd: TDiagramDisplay; mr: TDiagramMemberRecord;
  DragDistanceX, DragDistanceY: integer): boolean;
var
  iW, iH: integer;
  dh: TDragHookType;
begin
  //Calculdate width and height of object
  iW := mr.Points[1].X - mr.Points[0].X;
  iH := mr.Points[1].Y - mr.Points[0].Y;

  //reposition the object based on distance dragged
//  mr.Points.Change(0, mr.DragStartX + Round(dd.ConvertOutX(DragDistanceX)), mr.DragStartY + Round(dd.ConvertOutY(DragDistanceY)));
//  mr.Points.Change(1, mr.DragStartX + Round(dd.ConvertOutX(DragDistanceX))+iW, mr.DragStartY + Round(dd.ConvertOutY(DragDistanceY))+iH);
  if dd.Diagram.members.selectcount> 1 then
    dd.DraghookType := dhAll;

  case dd.DragHookType of
    dhLeft: mr.Points.Change(0, mr.DragStartX + DragDistanceX, mr.Points[0].Y);
    dhRight:mr.Points.Change(1, mr.DragStartX + DragDistanceX, mr.Points[1].Y);
    dhTop: mr.Points.Change(0, mr.Points[0].X, mr.DragStartY + DragDistanceY);
    dhBottom: mr.Points.Change(1, mr.Points[1].X, mr.DragStartY + DragDistanceY);
    dhTopLeft: begin
      mr.Points.Change(0, mr.DragStartX + DragDistanceX, mr.DragStartY + DragDistanceY);
    end;
    dhTopRight: begin
      mr.Points.Change(1, mr.Points[1].X, mr.DragStartY + DragDistanceY);
      mr.Points.Change(0, mr.DragStartX + DragDistanceX, mr.Points[0].Y);
    end;
    dhBottomLeft: begin
      mr.Points.Change(1, mr.Points[1].X, mr.DragStartY + DragDistanceY);
      mr.Points.Change(0, mr.DragStartX + DragDistanceX, mr.Points[0].Y);
    end;
    dhBottomRight: begin
      mr.Points.Change(1, mr.DragStartX + DragDistanceX, mr.DragStartY + DragDistanceY);
    end;
    dhAll: begin
      mr.Points.Change(0, mr.DragStartX + DragDistanceX, mr.DragStartY + DragDistanceY);
      mr.Points.Change(1, mr.DragStartX + DragDistanceX+iW, mr.DragStartY + DragDistanceY+iH);
    end;
  end;

  if GetWidth(mr) < 5  then begin
    SetWidth(mr, 5);
  end;

  if GetHeight(mr) < 5  then begin
    SetHeight(mr, 5);
  end;


//  mr.FPoints[1].X := GlobalX + iW;
//  mr.FPoints[1].Y := GlobalY + iH;
  result := true;
end;
//------------------------------------------------------------------------------
procedure TSquareSymbol.MouseExit(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord);
begin
  inherited;
  dd.Change;

end;

procedure TSquareSymbol.MouseOver(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord; globalX, globaly: integer);
var
  dh: TDragHookType;
begin
  dh := GetDragHookType(dd, mr, globalX,globalY);
  case dh of
    dhAll: dd.Cursor := crSizeAll;
    dhLeft: dd.Cursor := crSizeWE;
    dhRight: dd.Cursor := crSizeWE;
    dhTop: dd.Cursor := crSizeNS;
    dhBottom: dd.Cursor := crSizeNS;
    dhTopLeft: dd.Cursor := crSizeNWSE;
    dhTopRight: dd.Cursor := crSizeNESW;
    dhBottomleft: dd.Cursor := crSizeNESW;
    dhBottomRight: dd.Cursor := crSizeNWSE;
  end;
  dd.Change;

end;
//------------------------------------------------------------------------------
procedure TSquareSymbol.MouseStartDrag(dd: TDiagramDisplay;
  mr: TDiagramMemberRecord; InitClickGlobalX, InitClickGlobalY: integer);
var
  dh: TDragHookType;
begin
  //record initial position of object in DragStartX, and DragStartY
  //NOTE: WHICH COORDINATE IS STORED DEPENDS ON DragHookType
  dd.DragHookType := GetDragHookType(dd, mr, Round(dd.MouseDownGlobalX), round(dd.MouseDownGlobalY));

  dd.Diagram.Members.DragStart;

  case dd.DragHookType of
    dhLeft: mr.DragStartX := mr.points[0].X;
    dhRight: mr.DragStartX := mr.points[1].X;
    dhTop: mr.DragStartY := mr.points[0].Y;
    dhBottom: mr.DragStartY := mr.points[1].Y;
    dhTopLeft: begin
      mr.DragStartX := mr.points[0].X;
      mr.DragStartY := mr.points[0].Y;
    end;
    dhTopRight: begin
      mr.DragStartX := mr.points[1].X;
      mr.DragStartY := mr.points[0].Y;
    end;
    dhBottomLeft: begin
      mr.DragStartX := mr.points[0].X;
      mr.DragStartY := mr.points[1].Y;
    end;
    dhBottomRight: begin
      mr.DragStartX := mr.points[1].X;
      mr.DragStartY := mr.points[1].Y;
    end;
    dhAll: begin
      mr.DragStartX := mr.points[0].X;
      mr.DragStartY := mr.points[0].Y;
    end;
  end;
end;

procedure TSquareSymbol.Paint(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord);
(*var
  ix1,iy1,ix2,iy2: integer;
  pt: TPoint;
begin
  if mr.points.count < 1 then begin
    pt.x := 0;
    pt.y := 0;
    mr.points.Add(pt);
  end;

  if mr.points.count < 2 then begin
    pt.x := mr.points[0].x + 50;
    pt.y := mr.points[0].y + 50;
    mr.points.Add(pt);
  end;

  ix1 := mr.Points[0].x;
  iy1 := mr.Points[0].y;
  ix2 := mr.Points[1].x;
  iy2 := mr.Points[1].y;

  if mr.Selected then
    dd.FillColor := clYellow
  else begin
    dd.FillColor := 255;
  end;
  //dd.Circle(ix1,iy1,ix2,iy2, dd.FillColor, true);
  dd.Rectangle(ix1,iy1,ix2,iy2, dd.FillColor, true);
  dd.Rectangle(ix1,iy1,ix2,iy2, 0, false);

end;*)
begin
  if GetHeight(mr) < ConstrainYminimum then
    SetHeight(mr, ConstrainYMinimum);

  if GetWidth(mr) < ConstrainXminimum then
    SetWidth(mr, ConstrainXMinimum);

  inherited;



end;

{ TSymbol }
//------------------------------------------------------------------------------
procedure TSymbol.CopyFrom(sym: TSymbol);
begin
  if FDocumentation = nil then
    FDocumentation := tStringlist.create;
  if FResponsibilities = nil then
    FResponsibilities := tStringlist.create;

  FName := sym.FName;
  FProject := sym.FProject;
  FDocumentation.text := sym.FDocumentation.text;
  FResponsibilities.text := sym.FResponsibilities.text;
  FAuthor := sym.FAuthor;
  FDebug := sym.FDebug;

  //steal all children
  while sym.Children.Count > 0 do
    sym.Children[0].Parent := self;



end;

constructor TSymbol.create(sName: string; oProject: TUMLProject; parent: TSymbol);
begin
  inherited create;
  FAuthor := '';
  FProject := oProject;
  FName := sName;
  FChildren := TSymbolList.create(oProject);
  FChildren.ownername := self.classname+':'+self.name;
  FParent := parent;
  FNameSpace := TNameSpace.create;
  if parent <> nil then
    FNameSpace.ParentSpace := parent.NameSpace;

  RegisterWithParent;
  Fdocumentation := TStringlist.create;
  Fresponsibilities := TStringlist.create;
  if fProject <> nil then
    FProject.Symbols.Add(self);


end;
//------------------------------------------------------------------------------
destructor TSymbol.Destroy;
begin
  parent := nil;
  project := nil;

  //free all children
  while children.Count >0 do begin
    children[0].free;
  end;

  FChildren.free;
  FDocumentation.free;
  FResponsibilities.free;

  inherited;
end;
//------------------------------------------------------------------------------
function TSymbol.GetChildren: TSymbolList;
begin
  result := Fchildren;
end;
//------------------------------------------------------------------------------
function TSymbol.GetHasParent: boolean;
begin
  result := assigned(FParent)
end;
//------------------------------------------------------------------------------
function TSymbol.GetParent: TSymbol;
begin
  result := FParent;
end;
function TSymbol.GEtParentName: ansistring;
begin
  if assigned(parent) then
    result := parent.Name
  else
    result := 'no ancestor';
end;

//------------------------------------------------------------------------------
procedure TSymbol.SetName(const Value: ansistring);
begin
  FName := Value;
  if assigned(parent) then begin
    if lowercase(parent.Name) = lowercase(FNAme) then
      raise Exception.create('Symbol name cannot match parent symbol name');
  end;


end;
//------------------------------------------------------------------------------
procedure TSymbol.SetParent(const Value: TSymbol);
begin
  //deregisters withparent ONLY if assigned
  DeregisterWithParent;
  FParent := value;
  //registers with parent ONLYif assigned
  RegisterWithParent;
end;
//------------------------------------------------------------------------------
procedure TSymbol.DeregisterChild(child: TSymbol);
begin
  children.remove(child);
end;
//------------------------------------------------------------------------------
procedure TSymbol.RegisterChild(child: TSymbol);
begin
  IF child = nil then
    raise Exception.create('wtf, child is nil');
  children.Add(child);
end;
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('booger', [TDiagramDisplay, TbackBufferedControl, TfastbackBufferedControl, TDrawingBoard]);
end;
//------------------------------------------------------------------------------
procedure TSymbol.Paint(dd: TDiagramDisplay; mr: TDiagramMemberRecord);
begin
  PaintMain(dd, mr);

  if mr.MouseOver then begin
    HoverPaint(dd, mr);
  end;

  if mr.Selected then begin
    SelectPaint(dd, mr);
  end;
end;
procedure TSymbol.PaintMain(dd: TDiagramDisplay; mr: TDiagramMemberRecord);
begin
  //
end;

//------------------------------------------------------------------------------
function TSymbol.IsATPoint(mt: TDiagramMemberRecord; x,
  y: integer): boolean;
begin
  result := false;
end;

{ TDiagramDisplay }
//------------------------------------------------------------------------------
procedure TDiagramDisplay.Click;
begin
  inherited;

end;
//------------------------------------------------------------------------------
constructor TDiagramDisplay.Create(aOwner: TComponent);
begin
  inherited;
  FDiagram := nil;
  FToolMode := tmSelect;
  FOnDiagramClick := nil;
  FMouseOverRecord := nil;
  FScrollBarH := nil;
  FScrollBarV := nil;
end;
//------------------------------------------------------------------------------
destructor TDiagramDisplay.Destroy;
begin

  inherited;
end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.DiagramMouseMove(var msg: TMessage; mr: TdiagramMemberRecord; globalX, globalY: integer);
var
  dh: TDragHookType;
  oldCur: Tcursor;
begin
//  if msg.WParam >0 then
//    showmessage(inttostr(msg.WParam));
  //*********************************************Drag

  if (msg.WParam and MK_LBUTTON) = MK_LBUTTON then begin
    if not dragging then
    IF (abs(msg.LParamLo - self.MouseDownX) > 5)or
       (abs(msg.LParamHi - self.MouseDownY) > 5) then begin
      mr := MouseDownREcord;
      if assigned(mr) then begin
        mr.symbol.MouseStartDrag(self, mr, GlobalX, GlobalY);
        self.Dragging := true;
      end;
    end;

    if Dragging then
      DiagramDrag(msg, self.MouseDownRecord, globalX, globalY);
  end else
  //**********************************HOVER***************************
  begin
    oldcur := self.cursor;
    MouseOverRecord := mr;
    if assigned(mr) then begin

      mr.Symbol.MouseOver(self, mr,globalx,globaly);
    end else
      self.cursor := crDefault;

//    if self.cursor<> oldcur then
//      self.Refresh;
  end;

end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.DiagramClick(var msg: TMessage; mr: TdiagramMemberRecord; globalX, globalY: integer);
begin
  inherited;
  //select the object

  //Click with CTRL or SHIFT held
  if ((msg.WParam and (MK_CONTROL or MK_SHIFT))>0) then begin
    if assigned(mr) then begin
      mr.selected := not mr.Selected;
    end;
  //Standard click (no keys held)
  end else begin
    if assigned(mr) then begin
      //clear selection only if not selected previousy to make multi-select dragging easier
      if NOT mr.Selected then
        diagram.members.clearselected;
      mr.Selected := true;
    end else
      diagram.members.clearSelected;
  end;

  if Assigned(OnDiagramClick) then
    OnDiagramClick(self, mr, globalx, globaly);

  Draw;
end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.DiagramStartClick(var msg: TMessage; mr: TdiagramMemberRecord; globalX, globalY: integer);
//this is called when the mouse button is down.  It will record the position
//and member record when the mouseis put down.  Later when the mouse is
//lifted up.. it will compare against the recorded record to determine if the
//member reord was actually clicked.  Remember a "click" is a button down FollOWED
//by a button up on the same on-screen object.
begin
  inherited;

  //Record the member record and position of the click.
  self.MouseDownRecord :=mr;
  self.MousedownX := msg.LParamLo;
  self.MousedownY := msg.LParamHi;
  self.MousedownGlobalX := ScreenToGlobalX(msg.LParamLo);
  self.MousedownGlobalY := ScreenToGlobalY(msg.LParamHi);
  self.Dragging := false;

  if (mr = self.MouseDownRecord) and not Dragging then
    DiagramClick(msg, mr, globalX, globalY);

end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.DiagramEndClick(var msg: TMessage; mr: TdiagramMemberRecord; globalX, globalY: integer);
begin
  inherited;
  //if the member record matches the previously recorded memberrecord, then
  //select the object



end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TDiagramDisplay.DiagramEndDrag(var msg: TMessage; mr: TdiagramMemberRecord; globalX, globalY: integer);
begin
  inherited;

  //select the object
  diagram.members.clearSelected;
  if assigned(mr) then begin
    mr.Selected := true;
  end;

  if Assigned(OnDiagramClick) then
    OnDiagramClick(self, mr, globalx, globaly);

  Draw;

end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.DiagramMouseMessage(var msg: TMessage; globalX, globalY: integer);
begin
  if self.Diagram = nil then
    exit;
  case msg.Msg of
    //Previous buttonup call:  DiagramClick(msg, FindObjectAtPoint(globalX, globalY), globalX, globalY);
    WM_LBUTTONUP: DiagramEndClick(msg, FindObjectAtPoint(globalX, globalY), globalX, globalY);
    WM_LBUTTONDOWN: DiagramStartClick(msg, FindObjectAtPoint(globalX, globalY), globalX, globalY);
    WM_MOUSEMOVE: DiagramMouseMove(msg, FindObjectAtPoint(globalX, globalY), globalX, globalY);
  end;



end;
//------------------------------------------------------------------------------
function TDiagramDisplay.FindObjectAtPoint(x,
  y: integer): TDiagramMemberRecord;
var
  t: integer;
  mr: TDiagramMemberREcord;
begin
  result := nil;

  if self.Diagram = nil then
    exit;

  for t:= 0 to self.Diagram.Members.Count-1 do begin
    mr := self.diagram.members[t];
    if self.Diagram.Members[t].Symbol.IsAtPoint(mr, x, y) then begin
      result := mr;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.DoDraw;
var
  t: integer;
  memberRecord: TDiagramMemberRecord;
begin
//  WaitForRetrace;
  inherited;
  //white out
    Rectangle(0,0,BoundX2, BoundY2, clWhite, true);

  //for every symbol...paint
  if not assigned(diagram) then begin
    Rectangle(0,0,BoundX2, BoundY2, clSilver);
  end else begin
    Rectangle(0,0,BoundX2, BoundY2, clWhite );
    for t:= 0 to diagram.Members.Count-1 do begin
      memberRecord := diagram.members[t];
      memberRecord.Paint(self);
    end;
  end;


  flip;

end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.SetDiagram(const Value: TDiagram);
begin
  FDiagram := Value;
  Draw;
end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.WndProc(var msg: Tmessage);
begin
  inherited;
  if NOT (csDesigning in componentstate) then
  case msg.Msg of
    WM_LBUTTONUP, WM_LBUTTONDOWN, WM_MOUSEMOVE:
      DiagramMouseMessage(msg, Round(ScreenToGlobalX(msg.LParamLo)), Round(ScreenToGlobalY(msg.LParamHi)));
    //DiagramClick(msg);
  end;

end;
//------------------------------------------------------------------------------
function ProjectToXML(project: TUMLProject): ansistring;
var
  xmlb: TXMLBuilder;
  t,u,v: integer;
  sName: ansistring;
begin
  result := '';
  xmlb := TXMLBuilder.create;
  try
    xmlb.OpenElement('project', VarArrayOf(['name', project.Title]));
    xmlb.OpenElement('symbols', VarArrayOf(['count', project.Symbols.count]));
    for t:= 0 to project.Symbols.count-1 do begin
      sName := project.Symbols[t].Name;
      xmlb.OpenElement('symbol', VarArrayOf(['Name', sName]));
      xmlb.OpenElement('attributes',VarArrayOf([]), true);
      xmlb.OpenElement('operations',VarArrayOf([]), true);
      xmlb.OpenElement('properties',VarArrayOf([]), true);
      xmlb.OpenElement('child_symbols',VarArrayOf([]), true);

      xmlb.CloseElement('symbol');
    end;
    xmlb.CloseElement('symbols');
    xmlb.OpenElement('diagrams', VarArrayOf(['count', project.Diagrams.count]));
    for t:= 0 to project.Diagrams.count-1 do begin
      sName := project.Diagrams[t].Name;
      xmlb.OpenElement('diagram', VarArrayOf(['Name', sName]));
      //for all diagrams in project
      for u:= 0 to project.Diagrams[t].Members.count-1 do begin
        xmlb.OpenElement('member',VarArrayOf(['symbol', project.Diagrams[t].Members[u].Symbol.Name]));
        //for all points in diagrams in project
        xmlb.OpenElement('points', VarArrayOf(['Count', project.Diagrams[t].Members[t].Points.count]));
        for v := 0 to project.Diagrams[t].Members[t].Points.Count -1 do begin
          xmlb.OpenElement('point',VarArrayOf(['x', project.Diagrams[t].Members[u].Points[v].X, 'y', project.Diagrams[t].Members[u].Points[v].y]), true);
        end;
        xmlb.CloseElement('points');
        xmlb.CloseElement('member');
      end;
      xmlb.CloseElement('diagram');
    end;
    xmlb.CloseElement('diagrams');
    xmlb.CloseElement('project');
  finally
    result := xmlb.XML;
    xmlb.free;
  end;
end;
//------------------------------------------------------------------------------


procedure TDiagramDisplay.DiagramDrag(var msg: TMessage;
  mr: TdiagramMemberRecord; globalX, globalY: real);
var
   iDOX, iDOY: integer; //drag offset x,y
  iDX, iDY: integer; //distance dragged x,y
  iW, iH: integer; //width and height of object
  //TODO: make this crap more abstract.
  bHandled: boolean;
  t: integer;
begin
  if mr = nil then
    exit;

  //calculate the drag offset (relative position of mouse upon first click)
  iDOX := Round(globalX - MouseDownGlobalX);
  iDOY := Round(globalY - MouseDownGlobalY);

  //drag all selected objects
  if diagram.Members.SelectCount = 0 then begin
    mr.Symbol.MouseDrag(self, mr,iDOX, iDOY);
  end else
  for t := 0 to diagram.Members.count-1 do begin
    if diagram.members[t].Selected then
      diagram.members[t].Symbol.MouseDrag(self, diagram.members[t], iDOX, iDOY);
  end;

  Draw;






end;

//------------------------------------------------------------------------------
function TSymbol.MouseClick(dd: TDiagramDisplay; mr: TDiagramMemberRecord; ClickPointX,
  ClickPointY: integer): boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------
function TSymbol.MouseDrag(dd: TDiagramDisplay; mr: TDiagramMemberRecord; DragDistanceX,
  DragDistanceY: integer): boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------
procedure TSymbol.MouseStartDrag(dd: TDiagramDisplay;
  mr: TDiagramMemberRecord; InitClickGlobalX, InitClickGlobalY: integer);
begin

//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
procedure TSquareSymbol.PaintMain(dd: TDiagramDisplay;
  mr: TDiagramMemberRecord);
begin
  inherited;

end;
//------------------------------------------------------------------------------
procedure TSquareSymbol.SelectPaint(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord);
const
  BSZ = 5.0;
var
  BSX,BSY : real;
begin
  inherited;

  BSX := dd.SCaleScreenXToGlobal(BSZ);
  BSY := dd.ScaleScreenYToGlobal(BSZ);

  dd.Rectangle(GetLeft(mr), GetTop(mr), GetLeft(mr)+BSX, GetTop(mr)+BSY, clBlack, true);
  dd.Rectangle(GetRight(mr)-BSX, GetTop(mr), getRight(mr), GetTop(mr)+BSY, clBlack, true);
  dd.Rectangle(GetRight(mr)-BSX, GetBottom(mr)-BSY, getRight(mr), GetBottom(mr), clBlack, true);
  dd.Rectangle(GetLeft(mr), GetBottom(mr)-BSY, GetLeft(mr)+BSX, GetBottom(mr), clBlack, true);
//  dd.Rectangle(GetLeft(mr), GetTop(mr), GetLeft(mr)+5, GetTop(mr)+5, clYellow);
//  dd.Rectangle(GetRight(mr)-5, GetTop(mr), getRight(mr), GetTop(mr)+5, clYellow);
//  dd.Rectangle(GetRight(mr)-5, GetBottom(mr)-5, getRight(mr), GetBottom(mr), clYellow);
//  dd.Rectangle(GetLeft(mr), GetBottom(mr)-5, GetLeft(mr)+5, GetBottom(mr), clYellow);

end;

procedure TSquareSymbol.SetBottom(mr: TDiagramMemberRecord;
  iValue: integer);
begin
  mr.points.Change(1, mr.points[1].X, iValue);
end;
//------------------------------------------------------------------------------
procedure TSquareSymbol.SetHeight(mr: TDiagramMemberRecord;
  iHeight: integer);
begin
  mr.Points.Change(1, mr.Points[1].X, mr.points[0].Y+iHeight);
end;
//------------------------------------------------------------------------------
procedure TSquareSymbol.SetLeft(mr: TDiagramMemberRecord; ivalue: integer);
begin
  mr.points.Change(0, iValue, mr.points[0].Y);
end;
//------------------------------------------------------------------------------
procedure TSquareSymbol.SetRight(mr: TDiagramMemberRecord;
  iValue: integer);
begin
  mr.points.Change(1, iValue, mr.points[1].Y);
end;
//------------------------------------------------------------------------------
procedure TSquareSymbol.SetTop(mr: TDiagramMemberRecord; iValue: integer);
begin
  mr.points.Change(0, mr.points[0].X, iValue);
end;
//------------------------------------------------------------------------------
procedure TSquareSymbol.SetWidth(mr: TDiagramMemberRecord;
  iWidth: integer);
begin
  mr.Points.Change(1, mr.points[0].X+iWidth, mr.points[1].Y);
end;
//------------------------------------------------------------------------------
function TSymbol.GetDebugName: string;
begin
  result := classname+':'+name
end;

function TSymbol.GetDragHookType(dd: TDiagramDisplay;
  mr: TDiagramMemberRecord; ClickPointX,
  ClickPointY: integer): TDragHookType;
begin
  result := dhAll;
end;
//------------------------------------------------------------------------------
procedure TSymbol.MouseOver(dd: TdiagramDisplay; mr: TDiagramMemberRecord;
  globalX, globalylY: integer);
begin

//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.SetMouseOverRecord(
  const Value: TDiagramMemberRecord);
begin
  if FMouseOverRecord<>nil then begin
    FMouseOverrecord.MouseOver := false;
    FMouseOverRecord.Symbol.MouseExit(self, FMouseOverRecord);
  end;

  FMouseOverRecord := Value;

  if assigned(FmouseOverRecord) then begin
    FMouseOverRecord.MouseOver := true;
  end;
end;

procedure TSymbol.MouseExit(dd: TdiagramDisplay; mr: TDiagramMemberRecord);
begin
  dd.change;
end;

procedure TSymbol.HoverPaint(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TSymbol.SelectPaint(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord);
begin

//TODO -cunimplemented: unimplemented block
end;



{ TMatureConstruct }



function TLegacyRecord.AddItem(sName: string): TCompartmentItem;
var
  ci: TCompartmentItem;
begin
  debug.Log('Adding '+sNAme+' at index '+FItems.count.tostring);
  ci := TCompartmentItem.create(sName, self.Project, self);
  ci.Name := sName;

  FItems.Add(ci);
  result := ci;
end;

procedure TLegacyRecord.AddItem(item: TcompartmentItem);
begin
  if item = nil then
    raise exception.create('wtf');
  FItems.add(item);

end;

function TLegacyRecord.AddMember(sName: ansistring): TMemberDefinition;
begin
  result := TMemberDefinition.create(sName, self.Project, self);

  FItems.Add(result);
end;

constructor TLegacyRecord.create(sName: string; oProject: TUMLProject;
  parent: TSymbol);
begin
  inherited;
  FItems := TList<TSymbol>.create;
  ConstrainXMinimum := 10;
  ConstrainYMinimum := CLASS_TEXT_HEIGHT+(CLASS_TEXT_BORDER*2);

end;
//------------------------------------------------------------------------------
procedure TLegacyRecord.DeleteItem(iIndex: integer);
var
  ci: TCompartmentItem;
begin
  ci := Items[iIndex];
  ci.free;
  Fitems.delete(iIndex);
end;
//------------------------------------------------------------------------------
destructor TLegacyRecord.Destroy;
begin
  //TODO 1: Kill compartment items
  FItems.free;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TLegacyRecord.DrawCompartmentItem(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord; iPosition: integer; sSignature: ansistring);
begin
  if (NOT self.IsRoomForCompartmentItem(dd, mr, iposition+1)) and NOT (self.ItemCount-1 = iPosition) then
    sSignature := '...';

  if IsRoomForcompartmentItem(dd, mr, iposition) then
    dd.TextByLine(mr.points[0].x+CLASS_TEXT_BORDER, mr.points[0].y+ConstrainYminimum+CLASS_TEXT_BORDER, iPosition, sSignature, GetWidth(mr)-(CLASS_TEXT_BORDER*2));

end;
//------------------------------------------------------------------------------
function TLegacyRecord.GetItemCount: integer;
begin
  result := FItems.Count;
end;

function TLegacyRecord.GetItems(iIndex: integer): TCompartmentItem;
begin
  result := TCompartmentItem(FItems[iIndex]);
end;

function TLegacyRecord.GetItemTypeCount(cClass: TCompartmentClass): integer;
//returns number of compartment items that are of the given class type
var
  t: integer;
begin
  result := 0;

  for t:=0 to Itemcount-1 do begin
    if Items[t] is cClass then
      inc(result);
  end;


end;
//------------------------------------------------------------------------------
function TLegacyRecord.GetMemberCount: integer;
begin
  result := self.GetItemTypeCount(TMemberDefinition)
end;

function TLegacyRecord.GetMembers(iIndex: integer): TMemberDefinition;
begin
  result := GetNthClassInstance(iIndex, TMemberDefinition) as TMemberDefinition;

end;
//------------------------------------------------------------------------------
function TLegacyRecord.GetNthClassInstance(n: integer;
  cClass: Tcompartmentclass): TCompartmentItem;
//returns the Nth instance of a the given class in the object's compartment items
var
  i,t: integer;

begin
  result := nil;
  i := -1;

  for t:=0 to Itemcount-1 do begin
    if Items[t] is cClass then
      inc(i);

    if i=n then begin
      result := items[t];
      break;
    end;
  end;

end;

function TLegacyRecord.IsRoomForCompartmentItem(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord; iPosition: integer): boolean;
begin
  //TODO 1: COMPLETE;
  result := self.GetHeight(mr) > ConstrainYMinimum+(CLASS_TEXT_BORDER)+((iposition+1)*CLASS_TEXT_HEIGHT);
end;
//------------------------------------------------------------------------------
procedure TLegacyRecord.PaintMain(dd: TdiagramDisplay;
  mr: TDiagramMemberRecord);
var
  r,g,b: integer;
  x1,y1,x2,y2: integer;
  t: integer;
begin
  inherited;

  x1 := mr.Points[0].x;
  y1 := mr.Points[0].y;
  x2 := mr.Points[1].x;
  y2 := mr.Points[1].y;
  r := 255;
  g := 255;
  b := 196;
  //draw grey shadow
  dd.Rectangle(x1-4,y1+4,x2-4,y2+4,215+(215 shl 8)+(215 shl 16), true);
  //draw yellow matte
  dd.Rectangle(x1,y1,x2,y2,r+(g shl 8)+(b shl 16), true);
  //draw black border
  dd.Rectangle(x1,y1,x2,y2,0, false);
  //draw black border (top compartment)
  dd.Rectangle(x1,y1,x2,y1+ConstrainYMinimum,0, false);

  dd.Text(x1+CLASS_TEXT_BORDER,y1+CLASS_TEXT_BORDER, self.GetWidth(mr)-(CLASS_text_BORDER*2), CLASS_TEXT_HEIGHT, self.name, true);

  for t:=0 to ItemCount-1 do begin
    self.DrawCompartmentItem(dd, mr, t, Items[t].Signature);
  end;

end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.UpdateScrollBars;
var
  iTemp: integer;
begin
  //TODO 1: initialize scrollbars in constructor;
  if assigned(ScrollbarH) then begin
    ScrollBarH.Min := 0;
    ScrollBarH.position := round(BoundX1);
    if assigned(Diagram) then begin
      iTemp := round(self.Diagram.MaxX-DimensionX);
      if iTemp<0 then iTemp:= 0;
      ScrollBarH.Max := iTemp;
    end;

  end;

  if assigned(ScrollbarV) then begin
    ScrollBarV.Min := 0;
    ScrollBarV.position := round(BoundY1);
    if assigned(diagram) then begin
      iTemp := round(self.Diagram.MaxY-DimensionY);
      if iTemp < 0 then iTemp := 0;
      ScrollBarV.Max := iTemp;
    end;

  end;

end;

procedure TDiagramDisplay.SetScrollbarH(const Value: TScrollBar);
begin
  FScrollBarH := Value;

  if assigned(FScrollBarH) then
    FScrollBarH.OnChange := self.ScrollBarHChange;

  UpdateScrollBars;
end;

procedure TDiagramDisplay.SetScrollbarV(const Value: TScrollBar);
begin
  FScrollBarV := Value;

  if assigned(FScrollBarV) then
    FScrollBarV.OnChange := self.ScrollBarVChange;

  UpdateScrollBars;
end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.ScrollBarHChange(sender: TObject);
var
  dim : integer;
begin
  //Record dimension
  dim := round(DimensionX);

  BoundX1 := ScrollbarH.Position;
  BoundX2 := BoundX1+dim;

  Draw;


end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.ScrollBarVChange(sender: TObject);
var
  dim : real;
begin
  //Record dimension
  dim := DimensionY;

  BoundY1 := ScrollbarV.Position;
  BoundY2 := BoundY1+dim;

  Draw;
end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.CanvasConnect;
begin
  inherited;

end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.TextByLine(x,y: integer; iLine: integer; sText: ansistring; iBoxWidth :integer; iFontHeight: integer = CLASS_TEXT_HEIGHT; iLineSpace: integer = 13);
var
  realy: integer;
begin
  //based on linespace, determine REAL Y position;
  realy := y+(iLinespace*iLine);

  self.Text(x,realy, iBoxWidth, iFontHeight, sText, false, false);


end;

{ TMatureConstruct }
//------------------------------------------------------------------------------
procedure TMatureConstruct.AddAncestor(sym: TMatureConstruct);
begin
  FAncestors.AddUnresolved(sym.name);
end;

procedure TMatureConstruct.AddAncestor(sUnresolvedName: string);
begin
  Debug.Log('Defining Ancestor as: ' +sUnresolvedName);
  FAncestors.AddUnresolved(sUnresolvedName);
end;

function TMatureConstruct.AddFunction(sName: ansistring): TFunctionDefinition;
var
  func: TFunctionDefinition;
begin
  func := TfunctionDefinition.create(sName, self.Project, self);
  func.Name := sName;

  self.AddItem(func);

  result := func;

end;
//------------------------------------------------------------------------------
function TMatureConstruct.AddProperty(sName: ansistring): TPropertyDefinition;
var
  prop: TPropertyDefinition;
begin
  prop := TPropertyDefinition.create(sName, self.Project, self);
  prop.name := sName;

  self.AddItem(prop);

  result := prop;


end;
//------------------------------------------------------------------------------
constructor TMatureConstruct.create(sName: string; oProject: TUMLProject;
  parent: TSymbol);
begin
  inherited;
  FAncestors := TResolvableList.Create;
  FAncestors.project := oProject;
  FAncestors.ParentSpace := self.namespace.ParentSpace;

end;

destructor TMatureConstruct.Destroy;
begin
  FAncestors.free;
  inherited;
end;

{ TCompartmentItem }

//------------------------------------------------------------------------------
constructor TCompartmentItem.Create(sName: string; oProject: TUMLProject; parent: TSymbol);
begin
  inherited;
  FAlias := '';
  Fauthor := '';
  FDecoration := [];
  FSignature := '';
  FDocumentation := TStringList.create;
  FScope := cdUndefined;
  FParameters := TnameValuePairList.create;
end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.Change;
begin
  Draw;
  Flip;
  //UpdateScrollbars;
  //invalidate;
end;
//------------------------------------------------------------------------------
procedure TDiagramDisplay.Invalidate;
begin
  inherited;

end;

{ TLineSymbol }
//------------------------------------------------------------------------------
procedure TLineSymbol.Paint(dd: TDiagramDisplay; mr: TDiagramMemberRecord);
begin
  INHERITED;
end;

//------------------------------------------------------------------------------
procedure TLineSymbol.PaintMain(dd: TDiagramDisplay;
  mr: TDiagramMemberRecord);
var
  t: integer;
begin
  for t:= 0 to mr.points.count -2 do begin
    dd.FatLine(mr.points[t].x,mr.points[t].Y,mr.points[t+1].x,mr.points[t+1].Y,5,clBlack, not (t=0));
  end;
end;
//------------------------------------------------------------------------------
function TMatureConstruct.FindFunction(sName: ansistring): TFunctionDefinition;
var
  t: integer;
begin
  result := nil;
  for t:= 0 to self.FunctionCount-1 do begin

    if functions[t] = nil then begin
      log(self,'NIL function in function list of '+self.Name);
      continue;
    end;

    //log('Finding function :@'+inttostr(t)+' is '+functions[t].name);

    if lowercase(functions[t].Name) = lowercase(sName) then begin
      result := functions[t];
    end;
  end;

end;
//------------------------------------------------------------------------------
function TMatureConstruct.GetAncestor(iIndex: integer): TResolvable;
begin
  result := FAncestors[iIndex];
end;

function TMatureConstruct.GetAncestorCount: integer;
begin
  result := FAncestors.count;
end;

function TMatureConstruct.GetAncestorlist: string;
var
  t: ni;
begin
  result := '';
  for t:= 0 to FAncestors.Count-1 do begin
    if t > 0 then
      result := result + ',' + Ancestors[t].Name
    else
      result := Ancestors[t].Name;
  end;
end;

function TMatureConstruct.GetFunctionCount: integer;
begin
  result := self.GetItemTypeCount(TFunctionDefinition);
end;
//------------------------------------------------------------------------------
function TMatureConstruct.GetFunctions(
  iIndex: integer): TfunctionDefinition;
begin
   result := TfunctionDefinition(self.GetNthClassInstance(iIndex,TfunctionDefinition));
end;
//------------------------------------------------------------------------------
function TMatureConstruct.GetProperties(
  iIndex: integer): TPropertyDefinition;
begin
  result := TPropertyDefinition(self.GetNthClassInstance(iIndex,TPropertyDefinition));
end;
//------------------------------------------------------------------------------
function TMatureConstruct.GetPropertyCount: integer;
begin
  result := self.GetItemTypeCount(TPropertyDefinition);
end;


function TMatureConstruct.GetTypeParemeters: string;
begin
//  Lock;
  try
    result := FTypeParameters;
  finally
//    Unlock;
  end;
end;

procedure TMatureConstruct.SetTypeParameters(const Value: string);
begin
//  Lock;
  try
    FTypeParameters := value;
  finally
//    Unlock;
  end;

end;

{ TAttachedLineSymbol }

//------------------------------------------------------------------------------
constructor TAttachedLineSymbol.create(sNAme: string; oProject: TUMLProject; parent: TSymbol);
begin
  inherited;
  FSourceSymbol := nil;
  FTargetSymbol := nil;

end;
//------------------------------------------------------------------------------
procedure TAttachedLineSymbol.Paint(dd: TDiagramDisplay;
  mr: TDiagramMemberRecord);
var
  pt: Tpoint;
  sqs: TSquaresymbol;
begin
  exit;
  //initialize endpoints at center
  if TargetSymbol<> nil then begin
    sqs := TargetSymbol.Symbol as TSquareSymbol;
    mr.Points.Change(mr.points.Count-1, Round(sqs.GetCenterX(TargetSymbol)), Round(sqs.GetCenterY(TargetSymbol)));
  end;

  if SourceSymbol<> nil then begin
    sqs := SourceSymbol.Symbol as TSquareSymbol;
    mr.Points.Change(0, Round(sqs.GetCenterX(SourceSymbol)), Round(sqs.GetCenterY(SourceSymbol)));
  end;


  //range check source
  if SourceSymbol<> nil then begin
    if mr.Points.count > 1 then begin
//      pt  := sourcesymbol.Symbol.GetIntersectPoint(sourcesymbol, mr.Points[1].x, mr.Points[1].y,dd);
      pt  := sourcesymbol.Symbol.GetIntersectPoint(sourcesymbol, mr.Points[1].x, mr.Points[1].y);
      mr.Points.Change(0, pt.X, pt.y);
      //mr.Points.Change(1, 200, 200);
    end;
  end;

  if TargetSymbol<> nil then begin
    if mr.Points.count > 1 then begin
      pt  := TargetSymbol.symbol.GetIntersectPoint(targetsymbol, mr.Points[mr.Points.Count-2].x, mr.Points[mr.points.count-2].y);
//      pt  := TargetSymbol.symbol.GetIntersectPoint(targetsymbol, mr.Points[targetsymbol.Points.Count-1].x, mr.Points[targetsymbol.points.count-1].y,dd);
      mr.Points.Change(mr.points.Count-1, pt.X, pt.y);
    end;
  end;

  inherited;
end;
//------------------------------------------------------------------------------
procedure TAttachedLineSymbol.PaintMain(dd: TDiagramDisplay;
  mr: TDiagramMemberRecord);
begin
  inherited;

end;
//------------------------------------------------------------------------------
procedure TAttachedLineSymbol.SetSourceSymbol(const Value: TDiagramMemberRecord);
begin
  FSourceSymbol := Value;
end;
//------------------------------------------------------------------------------
procedure TAttachedLineSymbol.SetTargetSymbol(const Value: TDiagramMemberRecord);
begin
  FTargetSymbol := Value;
end;
//------------------------------------------------------------------------------
function TSymbol.GetIntersectPoint(mr: TDiagramMemberRecord; GlobalX,
  globaly: real; ddDebug: TDiagramdisplay = nil): Tpoint;
begin

//TODO -cunimplemented: unimplemented block
end;


{ TMemberDefinition }


{ TFunctionDefinition }
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TPropertyDefinition }



procedure TSymbol.AddDocLine(sLine: ansistring);
begin
//  if FDocumentation.text <> '' then
//    FDocumentation.text := FDocumentation.text + EOL;

  FDocumentation.text := FDocumentation.text + sLine;

end;

procedure TSymbol.SetProject(const Value: TUMLProject);
begin
  if Value = project then
    exit;
  if assigned(FProject) then
    project.Symbols.DeregisterSymbol(self);

  FProject := Value;

  if assigned(fproject) then
    project.Symbols.RegisterSymbol(self);



end;


destructor TCompartmentItem.destroy;
begin
  FParameters.free;
  FDocumentation.free;
  inherited;
end;


procedure TLegacyRecord.Sort;
var
  t: integer;
  bFound: boolean;
begin
  bFound := true;
  while bFound do begin
    bFound := false;
    for t:= 0 to self.itemcount-2 do begin
      if lowercase(TCompartmentItem(fItems[t]).Name) > lowercase(TCompartmentItem(fItems[t+1]).Name) then begin
        FItems.Exchange(t, t+1);
        bFound := true;
      end;
    end;
  end;



end;

procedure TSymbolList.SetProject(const Value: TUMLProject);
begin
  LockWrite;
  try
    FProject := value;
  finally
    UnlockWrite;
  end;
end;

procedure TSymbolList.Sort;
var
  t: integer;
  bFound: boolean;
begin
  LockWrite;
  try
    bFound := true;
    while bFound do begin
      bFound := false;

      for t:= 0 to self.count-2 do begin
        if lowercase(TSymbol(fSymbols[t]).Name) > lowercase(TSymbol(fSymbols[t+1]).Name) then begin
          FSymbols.Exchange(t, t+1);
          bFound := false;
        end;
      end;
    end;
  finally
    UnlockWrite;
  end;



end;

procedure TUMLProject.SetTitle(const Value: ansistring);
begin
  LockString;
  try
    FTitle := Value;
    UniqueString(Ftitle);
  finally
    UnlockString;
  end;
end;

procedure TDiagramList.SetProject(const Value: TUMLProject);
begin
  LockWrite;
  try
    FProject := value;
  finally
    UnlockWrite;
  end;
end;

function TUMLProject.SymbolToUnitName(sSym: ansistring): ansistring;
var
  fil: TFileInformation;
  t: integer;
begin
  fil := TFileInformation.create;
  sSym := lowercase(sSym);
  result := '';
  try
    for t:=0 to self.FSourceFiles.Count-1 do begin
      fil.FullName := FSourceFiles[t];
      if lowercase(fil.NamePart) = sSym then begin
        result := fil.FullName;
        break;
      end;
    end;
  finally
    fil.free;
  end;


end;



{ TResolvable }

function TResolvable.IsResolved: boolean;
begin
  result := sym <> nil;
end;

procedure TResolvable.Resolve;
var
  t: ni;
  ns: TNameSpace;
begin
  ns := self.namespace;
  ns.Resolve(self.Name);



end;

{ TNameSpace }

procedure TNameSpace.AddUnresolved(sName: string);
var
  sym: TResolvable;
begin
  sym := TResolvable.Create;
  sym.namespace := self;
  sym.name := sNAme;
  self.add(sym);

end;

function TNameSpace.Resolve(sSymName: string): TResolvable;
var
  t: ni;
begin
  for t:= 0 to Count-1 do begin
    if comparetext(items[t].name, sSymname)=0 then begin

    end;
  end;

end;

procedure TNameSpace.ResolveAll;
var
  t: ni;
begin
  for t:= 0 to count-1 do begin
    items[t].namespace := self;
    Items[t].Resolve;
  end;

end;

procedure TNameSpace.SetProject(const Value: TUMLProject);
begin
  FProject := Value;
end;

end.
