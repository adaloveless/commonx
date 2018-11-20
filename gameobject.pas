unit gameobject;


//proposed SpriteDefinition format

//Sheet0=whatever0.pgf
//Sheet1=whatever1.pgf
//Sheet2=whatever2.pgf
//GridX=16
//GridY=16
//Sprites=3
//Sprite0=0,0,32,64,kick
//Sprite1=32,0,32,64,punch
//Sprite2=64,0,32,64,stab

//Sprites=1
//Sprite0=G(0,0) <--- this means use the gridX and GridY values and return pos 0,0
//Sprite1=A(32,0,32,32) <1--- is means manually cut from rect


//Steps to bring in a NEW platform
//Does it support a back buffer
//Implement Parity of new abstracts
// - Reset Lights
// - Select LIghts (deprecated?)
// - Send Lights
// - Remove Shader Binding
// - AddLight
// - DrawTexturedTriangles3D
// - DrawTexturedTriangles3D2


interface

uses
  ioutils, typex, systemx, geometry, betterobject, generics.collections, namevaluepair, stringx, classfactory, betterfilestream, sysutils,
  tickcount, types, PXL.TypeDef, PXL.Types, PXL.Timing, dir, dirfile, btree, debug, pxl.textures, pxl.bitmaps, numbers, SoundDevice_Provider, SoundTools, ringstats,
  PXL.Devices, PXL.Canvas, PXL.Images, PXL.Fonts, PXL.Providers, classes, math, system.Generics.defaults, packagefile, linked_list;

const
  MAX_COLLIDER_CLASSES = 3;
  HIGH_CC = MAX_COLLIDER_CLASSES-1;
  FIXED_INTERVAL = 1/120;

type

  TLight = class;//forward
  TGCCollider = class;//forward
  TColliderIterateProcedure = reference to procedure(cThat: TGCCollider);
  TDIalog = class;//forward

  EGameInitFail = class(exception);
  TGameTime = double;
  TGameFloat = single;
  gf = TGameFloat;
  TGamePoint = TPoint2;
  TGameRect = TFloatRect4;
  TGamePackageFile = TBasicPackageFile;

  TGame = class;//forward
  TGameScene = class;//forward
  TGameComponent = class;//forward
  TSpriteLibrary = class;//forward
  TRenderableGameObject = class;//forward
  TCamera = class;//forward
  TGameObject = class;//forward
  TGameAsset = class;//forward
  TGAmeBase = class(TBetterObject)
  public
    InTrash: boolean;
  end;

  TGamePackageManager = class(TBetterObject)
  private
    FList: TList<TGamePackageFile>;
  public
    procedure Init;override;
    procedure Detach;override;
    procedure LoadPackage(gpk: string);
    function GEtAssetStream(sAssetName: string): TStream;
    procedure UnloadAllPackages;
    procedure LoadAllPackages;

  end;

  TRenderItem = class(TBTreeItem)
  public
    [unsafe] go: TGameObject;
    function Compare(const ACompareTo:TBTreeItem):ni; override;
  end;

  TLIghtItem = class(TBTreeItem)
  public
    [unsafe] light: TLight;
    [unsafe] go: TGameObject;
    vec: TVector4;
    calced: boolean;
    calcres: single;
    function Compare(const ACompareTo:TBTreeItem):ni; override;
    procedure CheckCalced;
  end;
//
  TRenderList = class(TBTree)
  public
    procedure AddGameObject([unsafe] go: TGameobject; [unsafe] cam: TCamera);
  end;

  TLightList = class(TBTree)
  public
    procedure AddGameObject([unsafe] goLight: TLight; [unsafe] obj: TGameObject);
  end;

  TGameAsset = class(TGameBAse)
  private
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}Fgame: TGame;
  public
    property Game: TGame read Fgame write FGame;
    Constructor CReate(Game: TGame);reintroduce;virtual;
  end;


  PTransForm = ^TTransform;
  TTransForm = record
  strict private
    FNoUpdate: boolean;
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}FParent: PTransForm;
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}FOwner: TGameObject;
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}FPosition: TVector4;
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}FRotation: TVector4;
    procedure SetPosition(const Value: TVector4);
    function GEtPArent: PTransForm;
    procedure SetRotation(const Value: TVEctor4);
    function GetWorldPosition: TVector4;
    procedure CalcMtx;
    procedure SetNoUpdate(const Value: boolean);
    function GetPosition: TVector4;
    function GetRotation: TVEctor4;
  private
    procedure SetWorldPosition(const Value: TVector4);
  public
    warp_mtx: TMatrix4;
    pmtx, rmtx: TMatrix4;
    mtx: TMatrix4;
    owner: TGameObject;
    DebugFlag: boolean;

    property position: TVector4 read GetPosition write SetPosition;
    property WorldPosition: TVector4 read GetWorldPosition write SetWorldPosition;
    property RotationInRots: TVEctor4 read GetRotation write SetRotation;
    procedure Init;
    property Parent: PTransForm read GEtPArent;
    function CascadeMtx: TMatrix4;
    property NoUpdate: boolean read FNoUpdate write SetNoUpdate;
    procedure PArentChanged(pfrom, pto: TGameObject);

  end;

  TJoyState = record
    xy: TPoint2;
  end;

  TGameButtonState = record
  private
    FDown: boolean;
    function GetPressed: boolean;
    function GetReleased: boolean;
    procedure SetDown(const Value: boolean);
  public
    StateChanged: boolean;
    property Down: boolean read FDown write SetDown;
    property Pressed: boolean read GetPressed;
    property Released: boolean read GetReleased;

  end;

  TInput = record
  public
    joy: array[0..1] of TJoySTate;
    skip: TGameButtonState;
    b0,b1,b2,b3: TGameButtonState;

  end;

  TGameComponent = class(TGameBAse)
  protected
    procedure SetGameObject(const Value: TGameObject);virtual;

  public
    IsAwake: boolean;
    killme: boolean;
    Fgameobject: TGameOBject;
    constructor Create(go: TGameObject);reintroduce;overload;virtual;
    constructor CReate;overload;override;

    procedure Detach;override;
    procedure FixedUpdate(deltatime: TGameTime);virtual;
    procedure Update(deltatime: TGameTime);virtual;
    procedure Awake;
    procedure OnAwake;virtual;
    property GameObject: TGameObject read FGameObject write SetGameObject;
    procedure AddScriptCommand(s: string);virtual;
    procedure ClearScripts;virtual;
  end;

  PColliderDef = ^TColliderDef;
  TColliderDef = record
    rect: TGameRect;
    classid: cardinal;
    function ToString: string;
    procedure FromString(s: string);
    class function SaveCollidersToString(a: array of TColliderDef):string;static;
  end;
  TColliderDefArray = array of TColliderDef;

  TColliderDefs = record
    classes: array[0..HIGH_CC] of TColliderDefArray;
    procedure LoadColliders(spritexy: Tpoint; nvpl: TNameValuePairList);
    procedure SaveColliders(spritexy: Tpoint; nvpl:TNameValuePairList);
    procedure Add(classtype: ni; r: TRectF);
    procedure Delete(classtype: ni; idx: ni);
  end;

  TGCCollider = class(TGameComponent)
  private
    procedure SetDef(const Value: PColliderDef);
  protected
    lastcollidetime: TGAmeTime;
    procedure CheckCollisions;virtual;
    function CheckIsCollidedWith(c: TGCCollider; out vec: TVector3): boolean;virtual;
    procedure SetGameObject(const Value: TGameObject); override;

  public
    Fdef: PColliderDef;
    safedistance: single;
    procedure Init;override;
    procedure FixedUpdate(deltatime: Double); override;
    procedure OnCollision(c: TGCCollider; vec: TVector3);virtual;
    procedure Collision(c: TGCCollider; vec: TVector3);inline;
    procedure OnAwake; override;
    procedure Detach;override;
    procedure CalculateSafeDistance;virtual;abstract;
    property Def: PColliderDef read FDef write SetDef;
  end;

  TGameComponentClass = class of TGameComponent;

  TGameObject = class(TGameBase)
  strict private
    FDebugDraw: boolean;
    FHasSleeping: boolean;
  private
    [unsafe] FParent: TGameObject;
    FName: string;
    FScene: TGameScene;
    procedure SEtParent(const Value: TGameObject);
    function GetScene: TGameScene;
    procedure SetHasSleeping(const Value: boolean);

  strict protected
    locallights: TLightLIst;//temporary, only active on Draw() and DoDraw()
  protected
    procedure DestroyComponents;
    procedure DeleteComponent(idx: ni; bDestroy: boolean = true);
    procedure CollectChildren(par: TGameObject; rl: TRenderList; ll: TLightList; cam: TCamera);

    property HasSleeping: boolean read FHasSleeping write SetHasSleeping;
  public
    IsAwake: boolean;
    IsStatic: boolean;
    AlwaysInFrustrum: boolean;
    killme: boolean;
    realdistfromcamera: VEctorFLoat;
    sortpenalty: single;
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}game: TGame;
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF} Components: TList<TGameComponent>;
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}Children: TLIst<TGameObject>;
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}TransForm: TTransForm;
    constructor Create(game: TGame);reintroduce;virtual;
    procedure Construct;virtual;
    destructor Destroy;override;
    procedure Awake;
    procedure OnAwake;virtual;

    procedure DeleteChildren;
    procedure RegisterChild(ch: TGameObject);
    procedure UnregisterChild(ch: TGameObject);

    procedure FixedUpdate(deltatime: TGameTime);virtual;
    procedure Update(deltatime: TGameTime);virtual;
    procedure AddComponent(c: TGameComponent);
    procedure Detach; override;
    property PArent: TGameObject read FParent write SEtParent;

    procedure DoDraw(cam: TCamera);virtual;
    procedure Draw(cam: TCamera; ll: TLIghtList);
    function DistanceFrom(go: TGameObject): VectorFloat;
    function DistanceFrom_unroot(go: TGameObject): VectorFloat;
    procedure SortChildren(cam: TGameObject);
    function RealDistFromCam(cam: TGameObject): vectorfloat;
    procedure UpdateDistFromCam(cam: TGameObject);inline;
    procedure REgisterComponent(c: TGameComponent);
    procedure UnregisterComponent(c: TGameComponent);
    property Name: string read FName write FName;
    function FindObjectByName(sName: string): TGameObject;
    function FindComponent<_GC_: TGameComponent>(): _GC_;overload;
    function FindComponent(sClassName: string): TGameCOmponent;overload;
    function IsInFrustrum(cam: Tcamera): boolean;
    procedure OnCollision(cThis, cThat: TGCCollider; vec: TVector3);virtual;
    property Scene: TGameScene read GetScene;
    property NilScene: TGameScene read FScene;
    procedure RemoveComponentsOfType<_GC_:TGameComponent>();
    property DebugDraw: boolean read FDebugDraw write FDebugDraw;
  end;

  TPreFAb = class(TGameObject)
  public
  end;

  TGameTimePiece = record
    frametime: TGameTime;
    framedeltatime: TGameTime;
    fixedupdatetime, targetfixedupdatetime: TGameTime;
    procedure Update(delta: TGameTime);
    procedure FixedUpdate(delta: TGameTime);
  end;

  TAudioLibrary = class(TGameAsset)
  public
    procedure PlaySoundEffect(sEffect: string; go: TGameObject; vol: single = 1.0);
  end;


  TGame = class(TBetterObject)
  private
    FOnRendered: TNotifyEvent;
    FFullResolutionSize: TPoint2px;
    FGArbage: array [0..1024] of TBetterObject;
    FGarbageIDx: ni;
    procedure UpdateProjMatrix;
    procedure SetWindowSize(const Value: TPoint2px);
    procedure SEtFullResolutionSize(const Value: TPoint2px);
    procedure UpdateViewMatrix;
  public
    rsUpdate, rsREnder: TRingStats;
    scaleat1depth: single;
    bgtexid: ni;
    timepiece: TGameTimePiece;
    XViewMatrix: TMatrix4;
    ViewAdjustMatrix: TMatrix4;
    ProjMatrix: TMatrix4;
    Input: TInput;
    ScreenScale: Single;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineAudioLIbrary: TAudioLibrary;
    EngineAudio: TAbstractSoundDevice;
    EngineDevice: TCustomDevice;
    FinalCanvas: TCustomCanvas;
    EngineImages: TAtlasImages;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;
    SupportedTargetCanvas: TCUstomCanvas;
    EnginePackages: TGamePackageManager;
    EngineRenderTarget: pxl.bitmaps.TBitmap;
    ReflectionTarget: pxl.bitmaps.TBitmap;
    TargetCanvas: TCustomCanvas;

    RenderSize: TPoint2px;
    FWindowSize: TPoint2px;
    SpriteLibrary: TSpriteLibrary;
    EngineTicks: Integer;

    ImageLenna: Integer;
    FontTahoma: Integer;
    Scene: TGameScene;
    Camera: TCamera;
    lastframetime: ticker;
    dialog: TDialog;
    constructor Create;override;
    procedure CreateFromProviders;virtual;
    procedure PrepareImages;
    destructor Destroy;override;
    procedure Detach;override;
    procedure LoadFonts;virtual;
    procedure LoadTextures;virtual;
    procedure LoadSprites;virtual;

    procedure UpdatePhysics(deltatime: Tgametime);virtual;
    procedure EngineTiming(const Sender: TObject);virtual;
    procedure EngineProcess(const Sender: TObject);virtual;
    procedure RenderScene;
    procedure RenderReflections;
    procedure ProcessInput;virtual;
    procedure DoRenderScene;virtual;
    procedure DoRenderBackGround;virtual;
    procedure DoRenderForeground;virtual;
    procedure DoRenderInterface;virtual;
    procedure PresentBackBuffer;virtual;abstract;
    procedure CreateBackBuffer;virtual;abstract;

    property OnRendered: TNotifyEvent read FOnRendered write FOnRendered;

    procedure LoadAllImages(sdir: string);
    procedure LoadSprite(sFile: string);overload;
    procedure LoadSprite(sName: string; s: TStream; len: ni);overload;


    procedure LoadAllImagesFromPAckage(gpk: string);
    procedure LoadAllSpritesFromPackage(gpk: string);
    procedure LoadAllSprites(sdir: string);

    procedure DefineView(fw, fh, w,h,scaleAT1Depth: vectorfloat; depthcenter_0to1: TPOint2);
    procedure SetDepthCenter(p0to1: TPoint2);
    property WindowSize: TPoint2px read FWindowSize write SetWindowSize;
    property FullResolutionSize: TPoint2px read FFullResolutionSize write SEtFullResolutionSize;
    procedure CollectGarbage(o: TGameBase);
    procedure EmptyGarbage;

  end;


  TGameScene = class(TGameObject)
  private
    FKillSceneOnFinish: boolean;
    FColliders: linked_list.TDirectlyLinkedList<TGCCollider>;
  public

    refcam: TCamera;
    timepiece: TGameTimePiece;
    reflectiontextureid: ni;
    procedure DoDraw(cam: TCamera);override;
    procedure OnAwake;override;
    procedure Construct;override;
    procedure Detach;override;


    procedure SceneFinish;
    procedure DoSceneFinish;virtual;
    property KillSceneOnFinish: boolean read FKillSceneOnFinish write FKillSceneOnFinish;
    procedure IterateColliders(proc: TColliderIterateProcedure);
    procedure RegisterCollider(c: TGCCollider);
    procedure UnregisterCollider(c: TGCCollider);
    procedure RenderReflectionMaps;virtual;

  end;

  PPose = ^Tpose;
  TPose = record
    TexIndex: ni;
    RefMap: ni;
    Rect: TGameRect;
    COG: TGamePoint;
    Colliders: TColliderDefs;
    Name: string;
    unityscale: vectorfloat;
    procedure Init;
  end;


  TSpriteSheet = class(TGameAsset)
  private
    FTExtureID: ni;
    FFileName: string;
    FREfMapTExtureID: ni;
    function GetPose(idx: ni): PPose;
    procedure ParseFromNVPL(nvpl: TNameValuePairList);
  public
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}defs: array of TPose;
    posedata: Pbyte;
    constructor Create(game: TGame);override;
    destructor Destroy;override;
    property TextureID: ni read FTExtureID write FTExtureID;
    property RefMapTextureID: ni read FREfMapTExtureID write FREfMapTExtureID;
    procedure AddPose(p: TPose);
    property Poses[idx: ni]: PPose read GetPose;


    procedure LoadDefs(sFile: string);
    procedure LoadDefsFromStream(sSheetNAme: string; s: TStream; len: ni);
    procedure DefineSprite(sStringDef: string; nvpl: TNameValuePairList);
    property FileName: string read FFileName;
    procedure ResolveTextureId;
    function Find(sNAme: string): PPose;

    //texture
  end;

  TSpriteLIbrary = class(TGameAsset)
  public
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}Sheets: TList<TSpriteSheet>;

    destructor Destroy;override;
    procedure DestroySheets;
    constructor CReate(game: TGame);override;
    function Find(sName: string): PPose;
  end;

  TSpriteComponent = class(TBetterObject)
  private
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF}FCenterOfGravity: TPOintF;
  public
    property CenterOfGravity: TPOintF read FCenterOfGravity write FCenterOfGravity;
  end;

  TRenderableGameObject = class(TGameObject)
  public
    material: TMaterialProperties;
    procedure Init;override;

  end;

  TSprite = class(TRenderableGameObject)
  private
    FRotationSpeed: single;
    FMirrorX: boolean;
    procedure TransformToCamera(cam: TCamera);
    function GEtAlpha: single;
    procedure SEtAlpha(const Value: single);
    procedure SetDef(const Value: PPose);
    procedure SetMirrorX(const Value: boolean);
    procedure SEtDefinitionResolved(const Value: boolean);
  protected
    FAlpha: single;
    Vertices: packed array[0..3] of TVector4;
    TransformedVertices: packed array[0..3] of TVector4;
    VertexColors: packed array[0..3] of TIntColor;
    //vex: packed array[0..3] of TVector4;
  const
      Indices: packed array[0..5] of LongInt = (0, 1, 2, 2, 3, 0);
      NilReflection: packed array[0..3,0..1] of single =((0,0),(0,0),(0,0),(0,0));
  public
    Fdef: PPose;
    dimensions: TPointF;
    FDefinitionResolved: boolean;
    procedure Init;override;
    procedure ResolveDefinition;virtual;
    procedure DoDraw(cam: TCamera);override;
    property RotationSpeed: single read FRotationSpeed write FRotationSpeed;
    procedure DrawSprite(cam: TCamera);
    procedure UpdateVectors;
    property Alpha: single read GEtAlpha write SEtAlpha;
    procedure LoadSpriteColliders();
    property Def: PPose read FDef write SetDef;
    property MirrorX: boolean read FMirrorX write SetMirrorX;
    property DefinitionREsolved: boolean read FDefinitionresolved write SEtDefinitionResolved;
    procedure TransformVerticesWithoutCamera;
  end;


  TBoxCollider = class(TGCCollider)
  protected
    function CheckIsCollidedWith(c: TGCCollider; out vec: TVector3): Boolean;override;
  public
    localcoords: array[0..1] of TVector4;
    procedure OnAwake;override;
    procedure CalculateSafeDistance; override;

  end;

  TWorldSprite = class(TSprite)
  private
    FDefName: string;
    procedure SetDefName(const Value: string);
  public
    procedure ResolveDefinition;override;
    procedure FixedUpdate(deltatime: Double); override;
    property DefName: string read FDefName write SetDefName;
    procedure OnAwake;override;

  end;

  TFrustrum = record
    XLImit: single;
    YLimit: single;
    ZLImit: single;
    ZNear: single;
    ZMultiplier: single;
  end;

  TCamera = class(TRenderableGameObject)
  private
    camMtxAtDraw: TMatrix4;
    FReflect: boolean;
    procedure CalcCompositeMatrix;
    procedure SetReflect(const Value: boolean);
  public
    frustrum: TFrustrum;
    reflectionbuffer: packed array [0..255] of TPoint2;//if you ever draw something big, check this size
    unitydepth: vectorfloat;
    depthcenter: TPoint2;
    constructor CReate(game: TGame);override;
    procedure Init;override;
    procedure REmemberMatrix;

    function WorldVectorToCameraSpace(wv: TVector4): TVector4;
    procedure DrawTexturedTriangles2(obj: TGameOBject; const Vertices, WorkingVertices: PVector4;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount,
      TriangleCount: Integer; const MAt: TMaterialProperties;  bDebug: boolean = false);

    property Reflect: boolean read FReflect write SetReflect;
  end;


  TScollableBackground = class(TGameObject)
  public

  end;

  TLight = class(TGameObject)
  public
    color: TVector4;
    ambience: single;
    pointness: single;
    range:single;
    function ToPxlLIght: TPXLLIght;
    procedure Construct;override;
  end;

  Tportrait = class(TWorldSprite)
  public

    procedure DoDraw(cam: TCamera); override;
    procedure Construct; override;

  end;

  TDialog = class(TRenderableGameObject)
  private
    FText: string;
    FTExtRate: single;
    interval: single;
    FDrawnText: string;
    Fportrait: TPortrait;
    FPortraitName: string;
    FKillAfter: single;
    FSpeaker: string;
    procedure SetTExt(const Value: string);
    procedure SetTextRate(const Value: single);
    function GetPortrait: string;
    procedure SetPortrait(const Value: string);
  public
    vertices: packed array[0..3] of TPoint2;
    transformedvertices: packed array[0..3] of TVector4;
    indices: packed array[0..5] of integer;
    texcoords: packed array[0..3] of Tpoint2;
    colors: packed array[0..3] of Tintcolor;
    txbg: ni;
    textstartdrawtime: TGameTime;
    procedure Update(deltatime: Double); override;
    procedure OnAwake; override;
    procedure Construct;override;
    procedure Detach; override;

    property TextRate: single read FTExtRate write SetTextRate;
    property Text: string read FText write SetTExt;
    property DrawnText: string read FDrawnText write FDrawnText;
    procedure DoDraw(cam: TCamera); override;
    procedure Init;override;
    property Portrait: string read GetPortrait write SetPortrait;
    property KillAfter: single read FKillAfter write FKillAfter;
    property Speaker: string read FSpeaker write FSpeaker;

  end;



function GetPackagePath: string;
function GetMEdiaPackage: string;
function GetMediaPath: string;
function GetFontPath: string;
procedure CopySprites(sFrom: string; sto: string);
function LoadCollidersFromSTring(s: string): TColliderDefArray;



var
  gameobject_distfromcameracompare: TComparison<TGameObject>;
  compfact: TClassClassFactory = nil;
  colliderdebug: boolean = false;


implementation

uses
  gamecomponentsaudio,
  helpers.stream;

{ TGameObject }

procedure CopySprites(sFrom: string; sto: string);
begin
  CopyFolder(sFrom, sTo, '*.sprite');
  CopyFolder(sFrom, sTo, '*.script');


end;

function GetMediaPackage: string;
begin
{$IFDEF MSWINDOWS}
  Result := dllpath+'Media.gpk';
{$ELSE}
  {$IFDEF ANDROID}
    Result := TPath.GetDocumentsPath + '/media.gpk';
  {$ELSE}
    Result := ExtractFilePath(ParamStr(0)) + '/media.gpk';
  {$ENDIF}
{$ENDIF}
end;
function GetPackagePath: string;
begin
{$IFDEF MSWINDOWS}
  Result := dllpath;
{$ELSE}
  {$IFDEF ANDROID}
    Result := TPath.GetDocumentsPath + '/';
  {$ELSE}
    Result := ExtractFilePath(ParamStr(0)) + '/';
  {$ENDIF}
{$ENDIF}
end;
function GetMediaPath: string;
begin
{$IFDEF MSWINDOWS}
  Result := dllpath+'Media\';
{$ELSE}
  {$IFDEF ANDROID}
    Result := TPath.GetDocumentsPath + '/Media/';
  {$ELSE}
    Result := ExtractFilePath(ParamStr(0)) + '/Media/';
  {$ENDIF}
{$ENDIF}
end;

function GetFontPath: string;
begin
{$IFDEF MSWINDOWS}
  Result := dllpath+'Fonts\';
{$ELSE}
  {$IFDEF ANDROID}
    Result := TPath.GetDocumentsPath + '/Fonts/';
  {$ELSE}
    Result := ExtractFilePath(ParamStr(0)) + '/Fonts/';
  {$ENDIF}
{$ENDIF}
end;

procedure TGameObject.AddComponent(c: TGameComponent);
begin
  Components.add(c);


end;

procedure TGameObject.Awake;
var
  t: ni;
  gc: TGameComponent;
begin


  if not IsAwake then
    OnAwake;


  if (not IsAwake) or HasSleeping then begin
    for t := 0 to components.count-1 do begin
      gc := components[t];
      if not gc.IsAwake then
        components[t].Awake;
    end;

    for t := 0 to children.count-1 do begin

      children[t].Awake;
    end;
  end;

  IsAwake := true;
  HAsSleeping := false;

  //
end;


{ TColliderDefs }

procedure TColliderDefs.Add(classtype: ni; r: TRectF);
begin
  setlength(self.classes[classtype], length(self.classes[classtype])+1);
  self.classes[classtype][high(self.classes[classtype])].rect.FromRectF(r);
  self.classes[classtype][high(self.classes[classtype])].classid := classtype;
end;

procedure TColliderDefs.Delete(classtype, idx: ni);
var
  t: ni;
begin
  if idx < 0 then exit;
  if classtype < 0 then exit;


  for t:= idx to high(classes[classtype])-1 do begin
    classes[classtype][t] := classes[classtype][t+1]
  end;
  setlength(classes[classtype], length(classes[classtype])-1);

end;

procedure TColliderDefs.LoadColliders(spritexy: Tpoint; nvpl: TNameValuePairList);
var
  t: ni;
  s: string;
begin
  for t:= 0  to HIGH_CC do begin
    s := nvpl.GetItemEx('Colliders_'+inttostr(spritexy.x)+'_'+inttostr(spritexy.y)+'-'+inttostr(t), '');
    self.classes[t] := LoadCollidersFromString(s);
  end;

end;

procedure TColliderDefs.SaveColliders(spritexy: Tpoint; nvpl: TNameValuePairList);
var
  t: ni;
begin
  for t:= 0  to HIGH_CC do begin
    nvpl.Items['Colliders_'+inttostr(spritexy.x)+'_'+inttostr(spritexy.y)+'-'+inttostr(t)].Value := TColliderDef.SaveCollidersToString(self.classes[t]);
  end;
end;



{ TGamePackageManager }

procedure TGamePackageManager.Detach;
begin
  inherited;
  if detached then
    exit;
  UnloadAllPackages;
  Flist.free;
  FList := nil;
end;

function TGamePackageManager.GEtAssetStream(sAssetName: string): TStream;
var
  t: ni;
  i: ni;
  p: TGamePackageFile;
begin
  result := nil;
  for t:= 0 to FList.count-1 do begin
    p := FList[t];
    result := p.GetAssetStream(sAssetName);
    if result <> nil then
      exit;
  end;

end;

procedure TGamePackageManager.Init;
begin
  inherited;
  FList := TList<TGamePackageFile>.create;
  LoadAllPackages;
end;

procedure TGamePackageManager.LoadAllPackages;
var
  sPath: string;
  d: TDirectory;
  fi: TFileInformation;
begin
  sPath := GetPackagePath;

  d := TDirectory.create(sPath, '*.gpk', 0,0, false, false);
  try
    while d.GetNExtFile(fi) do begin
      LoadPackage(fi.fullname);
    end;
  finally
    d.free;
    d := nil;
  end;


end;

procedure TGamePackageManager.LoadPackage(gpk: string);
var
  p: TGAmePackageFile;
begin
  p := TGamePackageFile.create;
  p.Load(gpk);
  FList.Add(p);

end;

procedure TGamePackageManager.UnloadAllPackages;
var
  p: TGamePackageFile;
begin
  while FList.count > 0 do begin
    p := FLIst[FList.count-1];
    FList.remove(p);
    p.free;
    p := nil;
  end;






end;



constructor TGameObject.Create(game: TGame);
begin

  TransForm.Init;
  TransForm.owner := self;
  inherited CReate;

  Components := TList<TGameComponent>.create;
  Children := TList<TGameObject>.create;
  self.game := game;

  Construct;



end;

procedure TGame.DefineView(fw, fh, w, h, scaleAT1Depth: vectorfloat; depthcenter_0to1: TPOint2);
var
  mtx: TMatrix4;
  p2: TPoint2;
begin
  self.scaleat1depth := scaleat1depth;
  self.RenderSize := point2px(round(w),round(h));
  self.FullResolutionSize := point2px(round(fw),round(fh));
  SetDepthCenter(depthcenter_0to1);

  //since our output uses 0,0 for the upper left corner and w,h for the bottom right corner
  //we need to make 0 whatever the view w/2 for our perspective calculations to be convenient

  //this means, we set a translation matrix to the view matrix
end;

procedure TGame.UpdateViewMatrix();
var
  mtx: TMatrix4;
begin
  mtx := IdentityMtx4;
  mtx.data[0][0] := 1;
  mtx.data[1][1] := 1;
  mtx.data[2][2] := 1;

//  Debug.ConsoleLog('View MTX: DC:'+floatprecision(camera.depthcenter.x,2)+','+floatprecision(camera.depthcenter.y,2));
//  Debug.ConsoleLog('View MTX: WS:'+floatprecision(windowsize.x,2)+','+floatprecision(windowsize.y,2));
//  Debug.ConsoleLog('View MTX: RS:'+floatprecision(rendersize.x,2)+','+floatprecision(rendersize.y,2));
//  Debug.ConsoleLog('View MTX: Scale:'+floatprecision(scaleat1depth, 2));

  //scale our coordinates
  mtx := ScaleMtx4(1);
  mtx := mtx * TranslateMtx4(Vector4((camera.depthcenter.x*RenderSize.X), (camera.depthcenter.y*rendersize.y), 0,0));
  //1 unit in world is 2 units now
  mtx := mtx * ScaleMtx4((fullresolutionsize.x/rendersize.x), (fullresolutionsize.y/rendersize.y), 1);

  ViewAdjustMatrix := mtx;
  if assigned(TargetCAnvas) then
    TargetCanvas.UnityDepth := scaleat1depth;




end;

procedure TGameObject.DeleteChildren;
var
  t: ni;
  go: TGameObject;
begin
  for t := Children.Count-1 downto 0 do begin
    go := Children[t];
    Children.delete(t);
    go.free;
    go := nil;

  end;


end;

procedure TGameObject.DeleteComponent(idx: ni; bDestroy: boolean);
var
  c: TGameComponent;
begin
  c := Components[idx];
  Components.delete(idx);
  c.free;
  c := nil;

end;

destructor TGameObject.Destroy;
begin
  inherited;
end;

procedure TGameObject.DestroyComponents;
begin
  while Components.count > 0 do begin
    DeleteComponent(0,true);
  end;


end;

procedure TGameObject.Detach;
begin
  if not detached then begin
    DeleteChildren;
    DestroyComponents;
    Components.free;
    Components := nil;
    Children.free;
    Children := nil;


    PArent := nil;
    inherited;
  end;

end;

function TGameObject.DistanceFrom(go: TGameObject): VectorFloat;
var
  v: TVector4;
begin

  v := go.TransForm.position - transform.position;


  result := power(v.x,3)+ power(v.y,3)+power(V.Z,3);
  if result <= 0 then begin
    result := power(system.abs(result),(1/3));
  end else
    RESULT := power(result,(1/3));
//  distfromcamera  := result;

end;

function TGameObject.DistanceFrom_unroot(go: TGameObject): VectorFloat;
var
  v: TVector4;
begin

  v := go.TransForm.position - transform.position;


  result := power(v.x,3)+ power(v.y,3)+power(V.Z,3);
//  if result <= 0 then begin
//    result := power(abs(result),(1/3));
//  end else
//    RESULT := power(result,(1/3));
  //distfromcamera  := result;

end;

procedure TGameObject.FixedUpdate(deltatime: TGameTime);
var
  t: ni;
  g: TGameObject;

begin
  if IsStatic then exit;

  if game.TargetCanvas = nil then
    exit;
  if not isawake then
    Awake;
  for t:= 0 to Components.count-1 do begin
      Components[t].FixedUpdate(deltatime);
  end;

  for t:= Children.count-1 downto 0 do begin
    g := children[t];
//    Debug.Log(g.ClassName);
    g.FixedUpdate(deltatime);
    if g.killme then begin
      children.Remove(g);
      g.Detach;
      g.Free;
      g := nil;
    end;

  end;



end;

function TGameObject.GetScene: TGameScene;
begin
  if self is TGameScene then begin
    result := TGameScene(self);
  end
  else begin
    if self.parent = nil then
      raise ECritical.create('Cannot reference gameobject.scene until scene is assigned, try doing whatever you''re doing OnAwake rather than Construct');
    result := self.parent.Scene;
  end;
end;

function TGameObject.IsInFrustrum(cam: Tcamera): boolean;
var
  f: TFrustrum;
  p,c: TVector4;
  m,pm,cm: TMatrix4;
begin
  result := false;

  if AlwaysInFrustrum then
    exit(true);

  f := cam.frustrum;

  pm := self.transform.CascadeMtx;
  cm := cam.transform.cascademtx;


  m := pm * InvertMtx4(cm);

  c := Vector4(0,0,0,1);
  p := Vector4(0,0,0,1) * m;


//  p := self.TransForm.worldposition;
//  c := cam.TransForm.worldposition;

  if p.Z-c.z > f.ZLImit then
    exit(false);

  if p.Z-c.z < f.ZNear then
    exit(false);


  if (abs(p.x-c.x)) > (f.XLImit*f.zmultiplier) then
    exit(false);

  if (abs(p.y-c.y)) > (f.yLImit*f.zmultiplier) then
    exit(false);

  exit(true);


end;

procedure TGameScene.IterateColliders(proc: TColliderIterateProcedure);
var
  o: TGCCollider;
begin
  o := TGCCollider(FColliders.First);
  while o <> nil do begin
    proc(o);

    o := TGCCollider(o.Next);
  end;

end;

procedure TGameScene.RegisterCollider(c: TGCCollider);
begin
  FColliders.Add(c);
//  Debug.ConsoleLog(self.classname+' register collider for '+c.GameObject.ClassName+' count '+inttostr(FColliders.Slowcount));
end;

procedure TGameScene.RenderReflectionMaps;
var
  camSave: TCamera;
begin
  exit;
  if game.reflectiontarget = nil then
    exit;
  if refcam = nil then
    exit;

  camSave := game.Camera;
  try
    game.Camera := refCam;
    game.TargetCanvas := game.ReflectionTarget.Canvas;
    game.RenderREflections;

  finally
    game.camera := camSave;
  end;

end;

procedure TGameObject.OnAwake;
begin
  //
end;

procedure TGameObject.OnCollision(cThis, cThat: TGCCollider; vec: TVector3);
begin
//  Debug.Consolelog('collision '+cThis.gameobject.classname+' with '+cThat.gameobject.classname);
  //
end;

function TGameObject.RealDistFromCam(cam: TGameObject): vectorfloat;
var
  m4 : TMatrix4;
  v4: TVector4;
begin
  m4 := TransForm.CascadeMtx*InvertMtx4(cam.transform.CascadeMtx);
  v4 := Vector4(0,0,0,1);
  v4 := v4 * m4;
  realdistfromcamera := v4.z+sortpenalty;
  result := realdistfromcamera;

end;

procedure TGameObject.RegisterChild(ch: TGameObject);
begin
  HasSleeping := true;
  Children.add(ch);
end;

procedure TGameObject.SetHasSleeping(const Value: boolean);
begin
  FHasSleeping := Value;
  if value then begin
    if parent <> nil then
      parent.HasSleeping := value;
  end;
end;

procedure TGameObject.SEtParent(const Value: TGameObject);
var
  p: TGameOBject;
  bChanged: boolean;
  wp: TVector4;
begin
  bChanged := false;
  p := FParent;
  if (p <> nil) then begin
    p.UnregisterChild(self);
    bChanged := true;
    wp := self.TransForm.WorldPosition;
  end;

  transform.PArentChanged(FParent, value);
  FParent := Value;
  p := value;

  if (p <> nil) then begin
    p.RegisterChild(self);
    if bChanged then
      self.transform.worldposition := wp;//<-preserve previous world position if switching between parents
  end;



end;

procedure TGameObject.SortChildren(cam: TGameObject);
var
  t: ni;
  m4: TMAtrix4;
  c: TGameObject;
begin
  raise ECritical.create('confused');
  for t := 0 to children.count-1 do begin
//    children[t].DistanceFrom_unroot(cam);//also stores
    m4 := cam.transform.mtx*children[t].TransForm.mtx;
    c := children[t];
    c.realdistfromcamera := (m4.Data[3][2]*m4.data[3][3])+c.sortpenalty ;
  end;


  Children.Sort(TComparer<TGameObject>.Construct(gameobject_DistFromCameraCompare));

end;

procedure TGameObject.UnregisterChild(ch: TGameObject);
begin
  Children.remove(ch);

end;

procedure TGameObject.Update(deltatime: TGameTime);
var
  t: ni;
  c: TGameComponent;
  cnt: ni;
begin
  if not isawake then
    Awake;

  if IsStatic then
    exit;

  cnt := components.count-1;
  for t:= 0 to cnt do begin
    Components[t].Update(deltatime);
  end;

  for t:= Components.count-1 downto 0 do begin
    c := Components[t];
    if c.killme then begin
      c.Detach;
//      game.collectgarbage(c);
      c.DetachAndFree;
    end;
  end;

  for t:= 0 to Children.count-1 do begin
    Children[t].Update(deltatime);
  end;
end;
{ TGameComponent }

procedure TGameComponent.AddScriptCommand(s: string);
begin
  //
end;

procedure TGameComponent.Awake;
begin
  OnAwake;
  IsAwake := true;
  //
end;

constructor TGameComponent.Create(go: TGameObject);
begin
  Create;
  gameobject := go;

end;

procedure TGameComponent.ClearScripts;
begin
  //
end;

constructor TGameComponent.CReate;
begin
  inherited;

end;

procedure TGameComponent.Detach;
begin
  gameobject.UnRegisterComponent(self);
  inherited;

end;

procedure TGameComponent.FixedUpdate(deltatime: TGameTime);
begin
  //
end;

procedure TGameComponent.OnAwake;
begin
  //
end;

procedure TGameComponent.SetGameObject(const Value: TGameObject);
begin
  if FGameOBject <> nil then
    FGameObject.UnregisterComponent(self);

  FGameObject := Value;
  value.RegisterComponent(self);

end;

procedure TGameComponent.Update(deltatime: TGameTime);
begin
  //
end;

{ TGame }

procedure TGame.CollectGarbage(o: TGameBase);
begin
  if o.inTrash then
    exit;

  o.InTrash := true;
  FGarbage[FGarbageIdx] := o;
  inc(FGarbageIdx);

  if FGarbageIDx = high(FGarbage) then
    EmptyGarbage;//WARNING?!?

end;

constructor TGame.Create;
begin
  rsUpdate := TRingStats.create;
  rsRender := TRingStats.create;

  inherited;
  lastframetime := getticker;
  EnginePackages := TGamePackageManager.create;

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;
  EngineTicks := 0;

  SpriteLibrary := TSpriteLibrary.create(self);


  CreateFromProviders;
  PrepareImages;
  LoadTextures;
  LoadSprites;
  LoadFonts;
  lastframetime := getticker;
  engineaudioLibrary := TAudioLibrary.create(self);




end;

procedure TGame.CreateFromProviders;
begin
  EngineAudio := GetPlatFormAudioDevice(true);
  //
end;

destructor TGame.Destroy;
begin
  inherited;
  rsUpdate.free;
  rsRender.free;
end;

procedure TGame.Detach;
begin

  if not detached then begin
    Scene.free;
    Scene := nil;

    SpriteLibrary.free;
    SpriteLIbrary := nil;

    EngineTimer.free;
    EngineTimer := nil;
    enginepackages.DetachAndFree;
    enginepackages := nil;

    FreePLatformAudioDevice(EngineAudio);
    EngineAudio := nil;

    EngineAudioLIbrary.DetachAndFree;
    EngineAudioLibrary := nil
  end;

  inherited;

end;

procedure TGame.DoRenderBackGround;
begin
  //


end;

procedure TGame.DoRenderForeground;
begin
  if dialog <> nil then begin
    dialog.Update(timepiece.framedeltatime);
    dialog.Draw(camera, nil);
    if dialog.killme then begin

      dialog.detachandfree;
      dialog := nil;
    end;

  end;
  //
end;

procedure TGame.DoRenderInterface;
begin
  //
end;

procedure TGame.DoRenderScene;
begin
  //
end;

procedure TGame.EmptyGarbage;
var
  t: ni;
begin
  for t:= 0 to FGarbageIdx-1 do begin
   FGarbage[t].DetachAndFree;
   FGarbage[t] := nil;
  end;
  FGarbageIdx := 0;

end;

procedure TGame.EngineProcess(const Sender: TObject);
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block                         ;
end;

procedure TGame.EngineTiming(const Sender: TObject);
var
  ft: TGameTime;
  now: ticker;
  x,y: vectorfloat;
  xx,yy: vectorfloat;
begin
  ProcessInput;
  now := getticker;
  ft := GEtTimeSince(now, lastframetime) / 1000;
  timepiece.frametime := timepiece.frametime + ft;
  UpdatePhysics(ft);
  lastframetime := now;
  // Invoke "EngineProcess" event to do processing while GPU is busy rendering the scene.
  timepiece.framedeltatime := ft;
  EngineTimer.Process;

  TargetCanvas := SupportedTargetCanvas;
  RenderScene;//renders to target canvas
  PresentBackBuffer;






end;


procedure TGame.LoadAllImages(sdir: string);
var
  dir: TDirectory;
  fil: TFileInformation;
begin
  dir := TDirectory.create(sDir, '*.png', 0,0,true, false);
  try
    while dir.GEtNextFile(fil) do begin
      if 0=comparetext(extractfileext(fil.NAme), '.sprite') then begin
        Self.EngineImages.AddFromFile(fil.FullName, fil.FullName);
      end;
    end;
  finally
    dir.Free;
    dir := nil;
  end;


end;

procedure TGame.LoadAllImagesFromPAckage(gpk: string);
var
  pkg: TBasicPackageFile;
  fi: TPackageItemHeader;
  s: TStream;
  ms: TMEmoryStream;
begin
  pkg := TBasicPackageFile.create;
  try
    pkg.Load(gpk);
    pkg.reset;
    while pkg.GetNextFile(fi,s) do begin
      try
        if 0=comparetext(extractfileext(fi.NAme), '.png') then begin
          ms := TMemoryStream.create;
          try
            stream_GuaranteeCopy(s, ms, fi.DataSize);
            ms.seek(0,0);
            engineimages.AddFromStream('.png', ms, fi.name);
          finally
            ms.free;
          end;

        end else begin
          pkg.Stream.seek(fi.CompressedSize, soFromCurrent);
        end;
      finally
        s.free;
      end;
    end;

  finally
    pkg.detach;
    pkg.free;
    pkg := nil;
  end;
end;

procedure TGame.LoadAllSprites(sdir: string);
var
  dir: TDirectory;
  fil: TFileInformation;
begin
  dir := TDirectory.create(sDir, '*.sprite', 0,0,true, false);
  try
    while dir.GEtNextFile(fil) do begin
      LoadSprite(fil.FullName);
    end;
  finally
    dir.Free;
    dir := nil;
  end;


end;


procedure TGame.LoadAllSpritesFromPackage(gpk: string);
var
  pkg: TBasicPackageFile;
  fi: TPackageItemHeader;
  s: TStream;
begin
  pkg := TBasicPackageFile.create;
  try
    pkg.Load(gpk);
    pkg.reset;
    while pkg.GetNextFile(fi,s) do begin
      try
        if 0=comparetext(extractfileext(fi.NAme), '.sprite') then begin
          LoadSprite(fi.NAme,s, fi.DataSize);
        end else begin
          pkg.stream.seek(fi.CompressedSize, soFromCurrent);
        end;
      finally
        s.free;
      end;
    end;

  finally
    pkg.detach;
    pkg.free;
    pkg := nil;
  end;
end;

procedure TGame.LoadFonts;
begin
  FontTahoma := EngineFonts.AddFromXMLFile(GetFontPath + 'Tahoma9b.png');
  if FontTahoma = -1 then
  begin
    raise EGameInitFAil.create('could not load standard Tahoma9b font.');

    Exit;
  end;
end;

procedure TGame.LoadSprite(sFile: string);
var
  ss: TSpriteSheet;
begin
  ss := TSpriteSheet.CReate(self);
  try
    SpriteLibrary.sheets.add(ss);
    ss.LoadDefs(sFile);
  finally
    //ss.free;
  end;
end;


procedure TGame.LoadSprite(sName: string; s: TStream; len: ni);
var
  ss: TSpriteSheet;
begin
  ss := TSpriteSheet.CReate(self);
  try
    SpriteLibrary.sheets.add(ss);
    ss.LoadDefsFromStream(sNAme, s, len);

  finally
    //ss.free;
  end;
end;



procedure TGame.LoadSprites;
begin
  LoadAllSpritesFromPackage(GetMediaPackage);
end;

procedure TGame.LoadTextures;
begin
  //
  LoadAllImagesFromPackage(GetMediaPackage);
end;

procedure TGame.PrepareImages;
begin
  EngineImages := TAtlasImages.Create(EngineDevice);

  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := TargetCanvas;
end;

procedure TGame.ProcessInput;
begin
//
end;

procedure TGame.RenderReflections;
var
  camSave: TCamera;
begin
  camSave := camera;
  try
    camera := Scene.refcam;
    if camera = nil then
      exit;

    TargetCanvas := ReflectionTarget.Canvas;
    TargetCanvas.BeginScene;
    try
      DoRenderBackGround;
      scene.Draw(camera,nil);
      DoRenderScene;
      TargetCanvas.flush;
    finally
      TargetCanvas.EndScene;
    end;
  finally
    camera := camSave;
  end;
end;

procedure TGame.RenderScene;
begin

  if (not Scene.IsAwake) or (Scene.HasSleeping) then
    Scene.Awake;

  rsUpdate.BeginTime;
  scene.Update(timepiece.framedeltatime);
  rsUpdate.EndTime;

  if rsUpdate.NewBAtch then
    rsUpdate.OptionDebug('Update ');


  TargetCanvas.BeginScene;
  try
    rsRender.BeginTime;
    DoRenderBackGround;
    DoRenderScene;
    if camera = nil then
      raise ECritical.create('there is no camera');
    scene.Draw(camera,nil);
    targetcanvas.DefaultLIghts;
    if targetcanvas <> ReflectionTarget.canvas then begin
      DoRenderForeGround;
      DoRenderInterface;
    end;
    //transfer render target to engine canvas

    TargetCanvas.flush;

    if assigned(OnRendered) then
      OnRendered(self);
    rsRender.EndTime;
    if rsRender.NEwBatch then
      rsRender.OptionDebug('Render ');

  finally
    TargetCanvas.EndScene;
  end;
  REnderReflections;

end;

procedure TGame.SetDepthCenter(p0to1: TPoint2);
begin
  if camera = nil then
    exit;
  if targetcanvas = nil then
    exit;
  camera.depthcenter := p0to1;
  TargetCanvas.ScreenCenter := p0to1*point2(RenderSize.x,RenderSize.y);
  updateviewmatrix;
end;

procedure TGame.SEtFullResolutionSize(const Value: TPoint2px);
begin
  FFullResolutionSize := Value;
  UpdateViewMatrix();
end;

procedure TGame.SetWindowSize(const Value: TPoint2px);
begin
  FWindowSize := Value;
  UpdateViewMatrix();
end;

procedure TGame.UpdatePhysics(deltatime: TGameTime);
var
  old: TGameTime;
  nu: TGameTime;
begin
  timepiece.targetfixedupdateTime := timepiece.targetfixedupdatetime + deltatime;
  scene.timepiece.targetfixedupdatetime := scene.timepiece.targetfixedupdatetime + deltatime;
  while timepiece.fixedupdatetime < timepiece.targetfixedupdateTime do begin
    //game
    old := timepiece.fixedupdatetime;
    nu := old + FIXED_INTERVAL;
    timepiece.fixedupdatetime := nu;
    //scene
    if scene <> nil then begin
      old := scene.timepiece.fixedupdatetime;
      nu := old + FIXED_INTERVAL;
      scene.timepiece.fixedupdatetime := nu;
      scene.FixedUpdate(FIXED_INTERVAL);
      EmptyGarbage;
    end;

    //scene

  end;
  //scene.FixedUpdate(deltatime);
end;

procedure TGame.UpdateProjMatrix;
begin
  //
end;

{ TGameAsset }

constructor TGameAsset.CReate(Game: TGame);
begin
  inherited Create;
  self.Game := game;
end;

{ TSpriteLIbrary }

constructor TSpriteLIbrary.CReate(game: TGame);
begin
  inherited;
  Sheets := TList<TSpriteSheet>.create;

end;

destructor TSpriteLIbrary.Destroy;
begin
  DestroySheets;
  Sheets.free;
  Sheets := nil;

  inherited;
end;

procedure TSpriteLIbrary.DestroySheets;
var
  t: ni;
  ss: TSpriteSheet;
begin
  for t := sheets.count-1 downto 0 do begin
    ss := Sheets[t];
    Sheets.delete(t);
    ss.free;
    ss := nil;

  end;

end;

function TSpriteLIbrary.Find(sName: string): PPose;
var
  t: ni;
  ss: TSpriteSheet;
begin
  result:= nil;
  for t := 0 to sheets.count-1 do begin
    ss := sheets[t];
    result := ss.find(sName);
    if result <> nil then
      break;
  end;

  if result = nil then
//    raise ECritical.create('Sprite not definied '+sNAme);

end;

{ TSpriteSheet }

procedure TSpriteSheet.AddPose(p: TPose);
begin
  setlength(defs, length(defs)+1);
  defs[high(defs)] := p;
  posedata := PByte(@defs[0]);
end;

constructor TSpriteSheet.Create(game: TGame);
begin
  inherited;

  FTExtureID := -1;
end;

procedure TSpriteSheet.DefineSprite(sStringDef: string; nvpl: TNameValuePairList);
var
  s, s1,s2,s3,s4: string;
  gx,gy: ni;
  def: TPose;
  pt: TPointF;
  p2: TPointF;
  wx: ni;
  wy: ni;
  swx,swy: ni;
  fr: TRectF;
begin
  if sStringDef = '' then
    raise ECritical.create('blank sprite definition');

  s := UpperCAse(zcopy(sSTringDef,0,1));
  s1 := zcopy(sStringDef, 1, length(sStringDef)-1);
  SplitString(s1, ';', s1,s2);
  SplitString(s2, ';', s2,s3);
  SplitString(s3,';', s3,s4);

  def.init;
  if s = 'G' then begin
    gx := nvpl.GetItemEx('GridX', 16);
    gy := nvpl.GetItemEx('GridY', 16);



    def.TexIndex := self.TextureID;
    def.RefMap := self.RefMapTextureID;
    pt.FromString(s1);

    wx := game.EngineImages[TextureID].Texture[0].Width;
    wy := game.EngineImages[TextureID].Texture[0].height;
    swx := wx div gx;
    swy := wy div gy;

    //def.Rect := FloatRect4((gx*pt.x),(gy*pt.y), (gx*pt.x+(wx/gx)),(gy*pt.y+(wy/gy)));
    def.Rect := FloatRect4((swx*pt.x)+0.0001, swy*pt.y+0.0001, swx-0.0002,swy-0.0002);

    p2.FromString(s2);
    def.COG.FromPOintF(p2);
    def.unityscale := strtofloat(s3);
    def.Name := s4;
    def.Colliders.LoadColliders(point(round(pt.x), round(pt.y)), nvpl);
    self.AddPose(def);
  end else
  if s = 'A' then begin
    wx := game.EngineImages[TextureID].Texture[0].Width;
    wy := game.EngineImages[TextureID].Texture[0].height;
    def.TexIndex := self.TextureId;
    fr.FromSTring(s1);
//    fr.top := fr.top / wy;
//    fr.left := fr.Left / wx;
//    fr.bottom := fr.bottom / wy;
//    fr.right := fr.right / wx;
    def.Rect.FromRectF(fr);
    p2.FromString(s2);
    def.COG.FromPOintF(p2);
    def.unityscale := strtofloat(s3);
    def.Name := s4;
    self.AddPose(def);

  end;




end;

destructor TSpriteSheet.Destroy;
begin

  inherited;
end;

function TSpriteSheet.Find(sNAme: string): PPose;
var
  t: ni;
  pose: PPose;
begin
  result := nil;
  for t := 0 to high(self.defs) do begin
    pose := GetPose(t);
    if CompareText(pose.name, sName)=0 then begin
      exit(pose);
    end;

  end;

end;

procedure TSpriteSheet.ParseFromNVPL(nvpl: TNameValuePairList);
var
  cnt: ni;
  t: ni;
  s: string;
  c: string;
begin
  cnt := nvpl.GetItemEx('Sprites', 0);
  for t := 0 to cnt - 1 do
  begin
    s := nvpl.GetItemEx('Sprite' + inttostr(t), '');
    DefineSprite(s, nvpl);
  end;
end;

function TSpriteSheet.GetPose(idx: ni): PPose;
begin
  result := @defs[idx];//PPose(Self.posedata+(idx*sizeof(TPose)));
end;

procedure TSpriteSheet.LoadDefs(sFile: string);
var
  nvpl: TNAmeValuepairList;
begin
  FFileNAme := sFile;
  ResolveTextureId;
  nvpl := TNameValuePairList.Create;
  try
    nvpl.LoadFromFile(sFile);
    ParseFromNVPL(nvpl);
  finally
    nvpl.free;
    nvpl := nil;
  end;
end;

procedure TSpriteSheet.LoadDefsFromStream(sSheetNAme: string; s: TStream; len: ni);
var
  nvpl: TNAmeValuepairList;
begin
  FFileName := sSheetNAme;
  ResolveTextureId;
  nvpl := TNameValuePairList.Create;
  try
    nvpl.LoadFromStream(s, len);
    ParseFromNVPL(nvpl);
  finally
    nvpl.free;
    nvpl := nil;
  end;

end;


procedure TSpriteSheet.ResolveTextureId;
begin
  if TextureID <> -1 then
    exit;

  textureID := game.EngineImages.IndexOf(changefileext(FileNAme, '.png'));
  RefMapTextureID := game.EngineImages.IndexOf(changefileext(Filename,'.reflection.png'));

  if textureID < 0 then
    raise ECritical.create('Image name not resolved: '+FileName)


end;


{ TRenderableGameObject }

procedure TGameObject.DoDraw;
begin
  //
end;

procedure TGameObject.Draw(cam: TCamera; ll: TLIghtList);
var
  t: ni;
  locallights: TLightList;
  pxll: TPXLLIght;
  l: TLight;
begin
  if not isawake then
    Awake;


  //take the lights we found when scanning the scene,
  //but resort them by how far they are from our object
  locallights := TLightList.create;
  try
    if ll <> nil then begin
      ll.Iterate(procedure(ABTreeItem:TBTreeItem)
        begin
          locallights.AddGameObject(TLightItem(AbtreeItem).light, self);
        end
      );
    end;

    game.TargetCanvas.ResetLights;
    locallights.Iterate(procedure(ABTreeItem:TBTreeItem; var NeedStop: boolean)
      begin
        pxll := TLightItem(AbtreeItem).light.ToPXLLIght;
        pxll.pos := cam.WorldVectorToCameraSpace(TLightItem(AbtreeItem).light.TransForm.WorldPosition);
        pxll.pos := pxll.pos * game.targetcanvas.projMAtrix;
//        pxll.pos := TLightItem(AbtreeItem).light.TransForm.WorldPosition;
        pxll.pos.w := 0;
//        if self is TWorldSprite then
//          Debug.Consolelog('pxll.pos='+floatprecision(pxll.pos.x,2)+','+floatprecision(pxll.pos.y,2)+','+floatprecision(pxll.pos.z,2)+','+floatprecision(pxll.pos.w,2));
        l := TLightItem(AbtreeItem).light;

        pxll.param.x := 1;//l.pointness;
        pxll.param.y := l.ambience;
        pxll.param.Z := l.range;
        NEedStop := Not game.TargetCanvas.AddLIght(pxll);


      end
    );

  finally
    locallights.free;
  end;

  game.TargetCanvas.sendlights;
  DoDraw(cam);
end;

{ TRenderableGameObject }

procedure TRenderableGameObject.Init;
begin
  inherited;
  material.init;
end;

{ TSprite }

procedure TSprite.DoDraw;
var
  s,ss: string;
begin
  inherited;
  ResolveDefinition;

  with game do begin
{$IFDEF LOFI}
    targetcanvas.Attributes := [];
{$ENDIF}

//    s := 'null';
//    ss := s;
//    if EngineImages[def.TexIndex] <> nil then
//      s := EngineImages[def.TexIndex].Name;
//    if EngineImages[def.RefMap] <> nil then
//      ss := EngineImages[def.RefMap].Name;

//    Debug.ConsoleLog(s+' & '+ss);
    targetcanvas.UseImagePx(EngineImages[def.TexIndex], def.Rect);
    targetcanvas.UseRefmap(EngineImages[def.RefMap]);

    DrawSprite(cam);

    //enginecanvas.TexQuad(FloatRect4RC((TPoint2(DisplaySize) * 0.5)+point2(v4.X, v4.y), Point2(sr.width, sr.height),  EngineTicks * RotationSpeed), IntColorAlpha(255));
  end;
end;

procedure TSprite.DrawSprite(cam: TCamera);
begin
  if cam = nil then begin
    TransformVerticesWithoutCamera;
    game.TargetCanvas.DrawTexturedTriangles3D2(@transformedvertices[0], @vertexcolors[0], @Indices[0], 4,2, @nilreflection[0], material);
  end
  else
    cam.DrawTexturedTriangles2(self, @Vertices[0], @TransformedVertices[0], @VertexColors[0],  @Indices[0], 4, 2, self.material, DebugDraw);
end;

function TSprite.GEtAlpha: single;
begin
  result := FAlpha;
end;

procedure TSprite.Init;
begin
  inherited;
  Fdef := nil;
  dimensions := pointf(1,1);
  VertexColors[0] := $FFFFFFFF;
  VertexColors[1] := $FFFFFFFF;
  VertexColors[2] := $FFFFFFFF;
  VertexColors[3] := $FFFFFFFF;
end;

procedure TSprite.LoadSpriteColliders();
var
  t,u: ni;
  c: TBoxCollider;
  a: TColliderDefArray;
  center: TPoint2;
begin
  RemoveComponentsOfType<TGCCollider>();

  center := def.COG;

  for u := 0 to HIGH_CC do begin
    a := def.colliders.classes[u];
    for t:= 0 to high(a) do begin
//      if (u = 2) and (self.classname = 'THobo') then
//        Debug.Consolelog('herehere');
      c := TBoxCollider.create(self);
      c.Def := PColliderDef(@def.colliders.classes[u][t]);
      c.localcoords[0].XY := a[t].rect.TopLeft-center;
      c.localcoords[1].XY := a[t].rect.BottomRight-center;
      c.localcoords[0].Z := -35;
      c.localcoords[1].Z := 35;
      c.localcoords[0].W := 1;
      c.localcoords[1].W := 1;
      c.CalculateSafeDistance;

    end;
  end;

end;

procedure TSprite.ResolveDefinition;
begin
  if DefinitionResolved then exit;

  if def = nil then
    def := game.SpriteLibrary.Find('Brick');

  updatevectors;
  LoadSpriteColliders;

  DefinitionResolved := true;

end;

procedure TSprite.SEtAlpha(const Value: nativefloat);
var
  t: ni;
begin
  for t := low(vertexcolors) to high(vertexcolors) do begin
    vertexcolors[t] := vertexcolors[t] and $FFFFFF;
    vertexcolors[t] := vertexcolors[t] or ((round(value * 255) and $ff) shl 24);


  end;


end;

procedure TSprite.SetDef(const Value: PPose);
begin
  FDef := Value;
  if FDef <> nil then begin
    LoadSpriteColliders();
    dimensions := POintF(def.Rect.Width, def.rect.height);
    DefinitionREsolved := false;
  end;
end;

procedure TSprite.SEtDefinitionResolved(const Value: boolean);
begin
  FDefinitionresolved := Value;
//  if value = false then
//    Debug.ConsoleLog('not resolved '+self.classname);
end;

procedure TSprite.SetMirrorX(const Value: boolean);
begin
  FMirrorX := Value;
  UpdateVectors;
end;

procedure TSprite.TransformToCamera(cam: TCamera);
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TSprite.TransformVerticesWithoutCamera;
var
  t: ni;
  p: PVEctor4;
  m4: TMatrix4;
begin
  t:=0;
  p := @vertices[0];
  m4 := transform.cascademtx;


  while t < 4 do begin
    TransformedVertices[t] := p^ * m4;
    inc(t);
    inc(p);
  end;

end;

{ TCamera }

procedure TCamera.CalcCompositeMatrix;
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

constructor TCamera.CReate(game: TGame);
begin
  inherited;
  CAlcCompositeMatrix;
//  TRansform.warp_mtx.Data[0][0] := -1;
//  TRansform.warp_mtx.Data[1][1] := -1;
//  TRansform.warp_mtx.Data[2][2] := -1;
//  TRansform.warp_mtx.Data[3][3] := -1;

//  Transform.ParentUpdated;
end;

procedure TCamera.DrawTexturedTriangles2(obj: TGameOBject; const Vertices, WorkingVertices: PVector4;
  const Colors: PIntColor; const Indices: PLongInt; const VertexCount,
  TriangleCount: Integer; const MAt: TMaterialProperties; bDebug: boolean = false);
var
  sz: TPointF;
  mtx: TMatrix4;
  v4: TVector4;
  szx: nativefloat;
  szy: nativefloat;
  ud: vectorfloat;
  t: ni;
  pv,wv: PVector4;
  udopvz: vectorfloat;
  pref: PPoint2;
begin
//  obj.TransForm.CalcMtx;
//  self.transform.CalcMtx;
  mtx :=  obj.transform.cascademtx * InvertMTx4(camMtxAtDraw);
//  mtx :=  InvertMTx4(camMtxAtDraw) * obj.transform.cascademtx;
  ud := unitydepth;

  //gv := game.ViewMatrix;
  //game.targetcanvas.viewMatrix := gv;
  pv := vertices;
  wv := workingvertices;
  pref := @Self.reflectionbuffer[0];

  for t := 0 to vertexcount-1 do begin
    wv^ := pv^ * mtx;
    //udopvz := (ud/wv^.z);
//    wv^.x := wv^.X * udopvz;
//    wv^.y := wv^.y * udopvz;

    wv^.w := 1;
    pref.X := 1-(wv^.x / game.RenderSize.x);//reflect based on post-transformed position
    pref.y := wv^.y / game.RenderSize.y;//reflect based on post-transformed position
    //wv^ := wv^ * gv;

    pv := PVector4(PByte(pv)+sizeof(pv^));
    wv := PVector4(PByte(wv)+sizeof(wv^));
    inc(pref);
  end;

//  raise ECritical.create('not used anymore');
  game.targetcanvas.DrawTexturedTriangles3d2(WorkingVertices, Colors,  Indices, 4, 2, @reflectionbuffer[0], Mat);

end;

procedure TCamera.Init;
var
  m4: TMatrix4;
begin
  inherited;
  m4 := transform.warp_mtx;
//  m4.data[3][3] := -1;
{$IFDEF USE_WARP_MTX}
  m4.data[0][0] := -1;
  m4.data[1][1] := -1;
  m4.data[2][2] := -1;
{$ENDIF}
//  m4.data[0][0] := 1;
//  m4.data[1][1] := 1;
//  m4.data[2][2] := -1;

  transform.warp_mtx := m4;
  frustrum.XLImit := 1500;
  frustrum.YLImit := 500;
  frustrum.ZLImit := 99999;
  frustrum.ZMultiplier := 2;


end;

procedure TCamera.REmemberMatrix;
var
  id: TMatrix4;
begin
  id := IdentityMtx4;
//  id.Data[0][0] := -1;
//  id.Data[1][1] := -1;
//  id.Data[2][2] := -1;

  camMtxAtDraw := transform.cascademtx;
end;

procedure TCamera.SetReflect(const Value: boolean);
var
  m: TMatrix4;
begin
  FReflect := Value;
  m := TransForm.warp_mtx;
  m.data[0][0] := -1;

  transform.warp_mtx := m;

end;

function TCamera.WorldVectorToCameraSpace(wv: TVector4): TVector4;
var
  sz: TPointF;
  gv, mtx: TMatrix4;
  v4: TVector4;
  ud: vectorfloat;
  t: ni;
//  d: vectorfloat;
begin
  mtx :=  InvertMTx4(camMtxAtDraw);
  ud := unitydepth;

  wv := wv * mtx;

//  if wv.z = 0  then
//    d := 1//ud*wv.z
//  else
//    d := ud/wv.z;

  gv := game.targetcanvas.ViewMatrix;
//  pref := @Self.reflectionbuffer[0];

//  wv.x := wv.X;
//  wv.y := wv.y;
//  wv.z := wv.z;
  wv.w := 1;
//  pref.X := wv.x / game.rendersize.x;//reflect based on post-transformed position
//  pref.y := wv.y / game.rendersize.y;//reflect based on post-transformed position
  wv := wv * gv;
  wv.w := 1;
//  wv := wv * game.targetcanvas.projmatrix;
  result := wv;

  //debug.consolelog(result.tostring);

end;

{ TTransForm }

procedure TTransForm.CalcMtx;
var
  vr: TVector4;
begin
  pmtx := warp_mtx * TranslateMtx4(self.position);
  vr := FRotation;
  rmtx := RotateXMtx4(vr.x * 6.28);
  rmtx := rmtx * RotateYMtx4(vr.y * 6.28);
  rmtx := rmtx * RotateZMtx4(vr.z * 6.28);

  if FRotation.x <> 0 then begin
//    Debug.log('here');
  end;

  mtx := rmtx * pmtx ;
end;

function TTransForm.CascadeMtx: TMatrix4;
var
  p: PTRansform;
begin
  p := parent;
  if p <> nil then
//    result := parent^.CascadeMtx * self.mtx
    result := self.mtx * parent^.CascadeMtx
  else
    result := self.mtx;

end;

function TTransForm.GEtPArent: PTransForm;
begin
  if owner.parent = nil then
    result := nil
  else
    result := @owner.PArent.TransForm;
end;

procedure TTransForm.Init;
begin
  fillmem(pbyte(@self), sizeof(self),0);
  FPOsition.init;
  pmtx := IdentityMtx4;
  rmtx := IdentityMtx4;
  mtx := IdentityMtx4;
  warp_mtx := IdentityMTx4;
  CalcMtx;
end;

procedure TTransForm.PArentChanged(pfrom, pto: TGameObject);
begin
  CalcMtx;
end;

procedure TTransForm.SetNoUpdate(const Value: boolean);
begin
  FNoUpdate := Value;
  if value = false then
    CalcMTx;
end;

procedure TTransForm.SetPosition(const Value: TVector4);
var
  px: TVector4;
begin
  FPosition := value;
//  px := FPosition;
//  px := px * Vector4(-1,-1,-1,1);
  CalcMtx;
end;

procedure TTransForm.SetRotation(const Value: TVEctor4);
begin
  FRotation := Value;
  CalcMtx;
end;

procedure TTransForm.SetWorldPosition(const Value: TVector4);
begin
  if Parent <> nil then
    position := value * InvertMtx4(parent.CascadeMtx)
  else
    position := value;


end;

{ TPose }




procedure TPose.Init;
begin
  unityscale := 1.0;
end;

{ TColliderDef }



procedure TColliderDef.FromString(s: string);
var
  s1,s2: string;
  r: TRectF;
begin
  s2 := s;
  SplitString(s2, ';',s1,s2);
  r.FromString(s1);
  SplitString(s2, ';',s1,s2);
  SplitString(s1, '|',s1,s2);

  classid := strtoint(s1);

  self.rect.FromRectF(r)

end;

function LoadCollidersFromSTring(s: string): TColliderDefArray;
var
  s1,s2: string;
  c: TColliderDef;
begin
  setlength(result,0);
  s2 := s;
  while SplitString(s2, '|', s1,s2) do begin
    c.FromSTring(s1);
    setlength(result, length(result)+1);
    result[high(result)] := c;
  end;

end;

class function TColliderDef.SaveCollidersToString(
  a: array of TColliderDef): string;
var
  t: Integer;
  c: TColliderDef;
  temp: string;
begin
  result := '';
  for t := 0 to high(a) do
  begin
    temp := a[t].ToString;
    result := result + temp + '|';
  end;
end;

function TColliderDef.ToString: string;
var
  r: TRectf;
begin
  r := rect.ToRectF;
  result := r.ToString+';';
  result := result + inttostr(classid)+';';

end;

{ TGameScene }

procedure TGameScene.OnAwake;
begin
  inherited;
  game.DefineView(720, 480, 360,240,1, POint2(0.5, 0.5));
end;

procedure TGameScene.Construct;
begin
  inherited;
  FColliders := TDirectlyLinkedList<TGCCollider>.create;
end;

procedure TGameScene.Detach;
begin
  inherited;
  FColliders.free;
end;

procedure TGameScene.DoDraw(cam: TCamera);
var
  rl: TRenderList;
  ll: TLightList;
  itm: TRenderItem;
  t: ni;
  m: TMatrix4;
begin
  inherited;

  if game.targetcanvas <> game.reflectiontarget.canvas then
    game.TargetCanvas.UseReflection(game.ReflectionTarget)
  else
    game.targetcanvas.usereflection(nil);




  ll := TLightLIst.create;
  try
    rl := TRenderlist.Create;
    try

      //rl.AddGameObject(self);
      CollectChildren(self, rl, ll, cam);

      cam.rememberMatrix;
      game.targetcanvas.viewmatrix := game.viewADjustmatrix;
      m := PerspectiveFOVXMtx4(2, 1/1.6, 0.1, 10000);
      game.TargetCanvas.ProjMatrix := m;

      rl.Iterate(procedure(ABTreeItem:TBTreeItem)
        begin

          TRenderItem(ABTreeItem).go.Draw(cam,ll);
        end
      );


    finally
      rl.Free;
    end;
  finally
    ll.free;
  end;

end;

procedure TGameScene.DoSceneFinish;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TGameScene.SceneFinish;
begin
  DoSceneFinish;
  if KillSceneOnFinish then
    killme := true;
end;

procedure TGameScene.UnregisterCollider(c: TGCCollider);
begin
//  Debug.ConsoleLog('Unregister collider for '+c.GameObject.ClassName);
  FColliders.Remove(c);
end;

{ TRenderItem }



procedure TRenderList.AddGameObject([unsafe] go: TGameobject; [unsafe] cam: TCamera);
var
  itm: TRenderItem;
begin
  itm := TRenderItem.Create;
  itm.go := go;
  go.UpdateDistFromCam(cam);
  if go.realdistfromcamera < 0 then
    itm.Free
  else
    self.add(itm);

end;

procedure TGameTimePiece.FixedUpdate(delta: TGameTime);
begin
  fixedupdatetime := fixedupdatetime + delta;
end;

procedure TGameTimePiece.Update(delta: TGameTime);
begin
  frametime := frametime+delta;
end;


procedure TWorldSprite.FixedUpdate(deltatime: Double);
begin
  if (isawake and (not DefinitionResolved)) then
    ResolveDefinition;
  inherited;

end;

procedure TWorldSprite.OnAwake;
begin
  inherited;
  ResolveDefinition;


end;


procedure TGCCollider.OnAwake;
begin
  inherited;
end;

procedure TGCCollider.CheckCollisions;
var
  vec: TVEctor3;
begin

  Self.GameObject.Scene.IterateColliders(
    procedure(cThat: TGCCollider)
    begin
      if cThat.gameobject <> self.gameobject then begin
        if CheckIsCollidedWith(cThat, vec) then begin
          Collision(cThat, vec);
        end;
      end;
    end
  );



end;

function TGCCollider.CheckIsCollidedWith(c: TGCCollider; out vec: TVEctor3): boolean;
begin
  result := false;
  //override ME!
end;

procedure TGCCollider.Collision(c: TGCCollider; vec: TVector3);
var
  tm: TGameTime;
begin


  //since two objects collide at the same time, we'll handle them both right now and flag the time
  //this way we won't have to two the math from both directions, doubling the
  //collision processing speed
  tm := gameobject.game.timepiece.fixedupdatetime;
//  if tm = lastcollidetime then
//    exit;

  lastcollidetime := tm;
  OnCollision(c, vec);
  c.OnCollision(self, vector3(0,0,0)-vec);
  gameobject.OnCollision(self, c, vec);
  c.gameobject.OnCollision(c, self, vector3(0,0,0)-vec);

end;

procedure TGCCollider.Detach;
var
  scn: TGameScene;
begin
  if not Detached then begin
    scn := gameobject.scene;
    if scn <> nil then
      scn.UnRegisterCollider(self);
    inherited;
  end;

end;

procedure TGCCollider.FixedUpdate(deltatime: Double);
begin
  inherited;
  if colliderdebug then begin
//    if self.gameobject.classname ='THobo' then
//      debug.consolelog('here');
  end;

  CheckCollisions;
end;

procedure TGCCollider.Init;
begin
  inherited;
end;

procedure TGCCollider.OnCollision(c: TGCCollider; vec: TVector3);
begin
  //
end;


procedure TGCCollider.SetDef(const Value: PColliderDef);
begin
  FDef := value;
  calculatesafedistance;
end;

procedure TGCCollider.SetGameObject(const Value: TGameObject);
begin
  inherited;
  gameobject.Scene.RegisterCollider(self);
end;

procedure TLIghtItem.CheckCalced;
var
  mdif, m1, m2: TMatrix4;
  v: TVEctor4;
begin
  if not calced then begin
    if go = nil then begin
      calcres := 0;
      calced := true;
      exit;
    end;
    m1 := go.transform.CascadeMtx;
    m2 := light.transform.cascademtx;

    mdif := m2*InvertMtx4(m1);
    v := VEctor4(0,0,0,1);
    v := v * mdif;
    vec := v;
    calcres := Length4(v);
  end;
  calced := true;
end;

function TLIghtItem.Compare(const ACompareTo: TBTreeItem): ni;
var
  this,that: single;
begin
  if self.go=nil then
    exit(0);
  checkcalced;
  TLIghtItem(acompareto).checkcalced;


  this := calcres;
  that := TLIghtItem(ACompareTo).calcres;
  if this < that then begin
    exit(1);
  end else
  if this > that then
    exit(-1)
  else
    exit(0);








end;


procedure TLightList.AddGameObject(goLight: TLight; obj: TGameObject);
var
  itm: TLightItem;
begin
  itm := TLightItem.Create;
  itm.go := obj;
  itm.light := goLIght;
  itm.CheckCalced;
  self.add(itm);

end;



procedure TLight.Construct;
begin
  inherited;
  range := 500;
end;

function TLight.ToPxlLIght: TPXLLIght;
begin
  result.color := self.color;
  result.param.x := self.ambience;
  result.param.y := self.pointness;
end;



procedure TWorldSprite.ResolveDefinition;
begin
  if definitionresolved then
    exit;
  def := game.SpriteLibrary.Find(defname);
  inherited;

end;

procedure TWorldSprite.SetDefName(const Value: string);
begin
  if FDefName = value then
    exit;
  FDefName := Value;
  DefinitionResolved := false;
  def := nil;
end;


procedure TAudioLibrary.PlaySoundEffect(sEffect: string; go: TGameObject; vol: single = 1.0);
var
  cga: TGCAudioSource;
begin
  cga := TGCAudioSource.create(go);
  cga.FileName := sEffect;
  cga.AutoDestroy := true;
  cga.volume := vol;



end;

procedure TDialog.Construct;
begin
  inherited;
  FPortrait := TPortrait.create(game);
  FPortrait.DefName := Portrait;
  Fportrait.parent := self;
  Fportrait.TransForm.position := vector4(0,0,1);
  FKillAFter := 4;
  textstartdrawtime := game.timepiece.frametime;

end;

procedure TDialog.Detach;
begin
  if not Detached then begin
    if game.dialog = self then
      game.dialog := nil;

    inherited;
  end;

end;

procedure TDialog.DoDraw(cam: TCamera);
const
  top = 40;
  bottom = TOP+64+8;
  left = 0;
  right = 720-4;
var
  p2: TPOint2;
begin
  inherited;
  if cam <> nil then begin
//    self.transform.worldposition := cam.TransForm.WorldPosition+vector4(0,0,1,0);
  end;

  if portrait = '' then
    exit;


//  vertices[0] := Vector4(left, top,0,0);
//  vertices[1] := Vector4(right, top,0,0);
//  vertices[2] := Vector4(right, bottom,0,0);
//  vertices[3] := Vector4(left, bottom,0,0);
  vertices[0] := POint2(left, top);
  vertices[1] := POint2(right, top);
  vertices[2] := POint2(right, bottom);
  vertices[3] := POint2(left, bottom);

  indices[0] := 0;
  indices[1] := 1;
  indices[2] := 3;
  indices[3] := 3;
  indices[4] := 1;
  indices[5] := 2;
  colors[0] := $FF000000;
  colors[1] := $FF000000;
  colors[2] := $FF100000;
  colors[3] := $FF100000;
  material.luminence := 1.0;
  material.diffuse := 0.0;
  p2 := point2(0,0);

  game.TargetCanvas.currentmaterial := self.material;
  game.TargetCanvas.DrawIndexedTriangles(@vertices[0],@colors[0], @indices[0], 4, 2);
  game.EngineFonts[game.fonttahoma].DrawText(point2(LEFT+4+64,TOP+4), Speaker, $FFFFFF00, 255);
  game.EngineFonts[game.fonttahoma].DrawText(point2(LEFT+4+64,TOP+16+4), DrawnText, $FFFFFFFF, 255);


  FPortrait.Def.unityscale := 2.0;
  Fportrait.Transform.position := vector4(left+4, top+4, 2);
  FPortrait.material.luminence := 1.0;
  FPortrait.material.diffuse := 0.0;
  FPortrait.DoDraw(nil);





end;

procedure Tportrait.Construct;
begin
  inherited;
  material.luminence := 1.0;
  material.diffuse := 0.0;
end;

procedure Tportrait.DoDraw(cam: TCamera);
begin
  material.luminence := 1.0;
  material.diffuse := 0;
  material.ambience := 0;
  inherited;

//  Debug.Log('Draw portrait');
end;

function TRenderItem.Compare(const ACompareTo: TBTreeItem): ni;
var
  [unsafe] go: TGameObject;
  dfc: vectorfloat;
{ TDialog }



{ Tportrait }



begin
  go := TRenderItem(ACompareTo).go;
  dfc := go.realdistfromcamera;
  dfc := dfc - self.go.realdistfromcamera;

  if dfc > 0 then
    exit(-1);
  if dfc < 0 then
    exit(1);
  exit(0);

end;


function TDialog.GetPortrait: string;
begin
  result := FPortraitNAme;
end;

procedure TDialog.Init;
begin
  inherited;
  TextRate := 50;
  Text := 'Put some stupid fucking text here you bitch head.';
end;

procedure TDialog.OnAwake;
begin
  inherited;
end;

procedure TDialog.SetPortrait(const Value: string);
begin
  FPortraitNAme := value;
  if Fportrait<>nil then
    Fportrait.defname := value;
end;

procedure TDialog.SetTExt(const Value: string);
begin
  FText := Value;
end;

procedure TDialog.SetTextRate(const Value: single);
begin
  FTExtRate := Value;
  interval := 1/value;
end;

procedure TDialog.Update(deltatime: Double);
var
  charstoShow: ni;
begin
  inherited;
  charsToShow := round((game.timepiece.frametime - self.textstartdrawtime) * TextRate);
  DrawnText := zcopy(Text, 0, charstoshow);
  if DrawnText = Text then begin
    killafter := killafter - deltatime;
    if killafter <= 0 then
      killme := true;
  end;



end;

{ TGameButtonState }

function TGameButtonState.GetPressed: boolean;
begin
  result := Self.statechanged and FDown;
end;

function TGameButtonState.GetReleased: boolean;
begin
  result := StateChanged and not(FDown);
end;

procedure TGameButtonState.SetDown(const Value: boolean);
begin
  if value = FDown then
    exit;
  FDown := Value;
  StateChanged := true;
end;

procedure TGameObject.UpdateDistFromCam(cam: TGameObject);
begin
  Self.RealDistFromCam(cam);
end;



procedure TSprite.UpdateVectors;
var
  sz: TPointF;
  center: TPOint2;
  mtx: TMatrix4;
  v4: TVector4;
  szx: nativefloat;
  szy: nativefloat;
  ud: vectorfloat;
  us: nativefloat;
  l, r, u, d: vectorfloat;
begin
  if def = nil then
    exit;

  us := def.unityscale;
  center := def.COG;
  center := center * us;

  sz := dimensions;
  szx := sz.x*def.unityscale;
  szy := sz.y*def.unityscale;


  mtx := game.Camera.TransForm.CascadeMtx;
  ud := game.Camera.unitydepth;

  if MirrorX then begin
    r := (0-center.x);
    l := (szX-center.x);
    u := (0-center.y);
    d := (szY-center.y);
    vertices[0] := Vector4(l,u, 0,1);
    vertices[1] := Vector4(r,u, 0,1);
    vertices[2] := Vector4(r,d, 0,1);
    vertices[3] := Vector4(l,d, 0,1);
  end else begin
    l := (0-center.x);
    r := (szX-center.x);
    u := (0-center.y);
    d := (szY-center.y);
    vertices[0] := Vector4(l,u, 0,1);
    vertices[1] := Vector4(r,u, 0,1);
    vertices[2] := Vector4(r,d, 0,1);
    vertices[3] := Vector4(l,d, 0,1);
  end;





end;

procedure TGameObject.REgisterComponent(c: TGameComponent);
begin
  components.Add(c);
  HAsSleeping := true;//<--bubbles up the parents to notify that we need to awake something
end;
procedure TGameObject.RemoveComponentsOfType<_GC_>;
var
  t: ni;
  c: TGameComponent;
begin
  for t:= Components.count-1 downto 0 do begin
    c := Components[t];
    if c is _GC_ then begin
      game.collectgarbage(c);
    end;
  end;

end;

procedure TGameObject.UnregisterComponent(c: TGameComponent);
begin
  components.Remove(c);
end;



function TGameObject.FindObjectByName(sName: string): TGameObject;
var
  t: ni;
  c: TGameOBject;
begin
  result := nil;
  if CompareText(sNAme, name)=0 then
    exit(self);


  for t:= 0 to children.count-1 do begin
    c := children[t];
    result := c.FindObjectByName(sName);
    if result <> nil then
      exit;
  end;

end;
function TGameObject.FindComponent<_GC_>(): _GC_;
var
  t: ni;
  c: TGameCOmponent;
begin
  result := nil;
  for t := 0 to components.count-1 do begin
    c := components[t];
    if c is _GC_ then
        exit(_GC_(c));
  end;

end;

function TGameObject.FindComponent(sClassName: string): TGameCOmponent;
var
  t: ni;
  c: TGameCOmponent;
begin
  result := nil;
  for t := 0 to components.count-1 do begin
    c := components[t];
    if CompareText(c.ClassName, sClassNAme)=0 then
      exit(c);
  end;

end;

procedure TGameObject.Construct;
begin
  //early construction.  IF
  //shit requires stuff already constructed, try Awake Instead.
  //Awake() happens later, on first update.
  //this one is useful for composite objects/scenes
end;

procedure TGameObject.CollectChildren(par: TGameObject; rl: TRenderList; ll: TLightList; cam: TCamera);
var
  t,c: ni;
{$IFDEF COOL_CODE}[unsafe]{$ENDIF} ch: TGameObject;
  b: boolean;
begin
  t := 0;
  c := par.Children.Count;
  while t<c do begin
    ch := par.Children[t];

    b := ch.IsInFrustrum(cam);
    if (ch.DebugDraw) and (not b) then begin
//      Debug.ConsoleLog('Not In Frustrum');
      b := ch.IsInFrustrum(cam);
//      Debug.ConsoleLog(booltostr(b));
    end;
    if b then begin
      rl.AddGameObject(ch, cam);

    end;

    if ch is TLIght then
      ll.addgameobject(TLight(ch),nil);

    CollectChildren(ch, rl, ll, cam);
    inc(t);
  end;

end;
function TTransForm.GetPosition: TVector4;
begin
  result := Fposition;
end;

function TTransForm.GetRotation: TVEctor4;
begin
  result := FRotation;
end;

function TTransForm.GetWorldPosition: TVector4;
begin
{  if self.Parent = nil then
    result := self.position
  else
    result := self.position + self.parent.position;

  result.w := 1;}

  result := VEctor4(0,0,0,1);
  result := result * CascadeMtx;
end;


procedure TBoxCollider.OnAwake;
begin
  inherited;

end;

procedure TBoxCollider.CalculateSafeDistance;
var
  p1,p2,p3,p4: Tpoint2;
  l1,l2,l3,l4: single;
  best: single;
begin
  p1 := Self.localcoords[0].XY;
  p2 := Self.localcoords[1].XY;
  p3 := point2(self.localcoords[0].x, self.localcoords[1].y);
  p4 := point2(self.localcoords[1].x, self.localcoords[0].y);


  l1 := (p1.x*p1.X)+(p1.y*p1.y);
  l2 := (p2.x*p2.X)+(p2.y*p2.y);
  l3 := (p3.x*p3.X)+(p3.y*p3.y);
  l4 := (p4.x*p4.X)+(p4.y*p4.y);

  best := l1;
  best := greaterof(best,l2);
  best := greaterof(best,l3);
  best := greaterof(best,l4);



  safedistance := sqrt(best);




end;

function TBoxCollider.CheckIsCollidedWith(c: TGCCollider;
  out vec: TVector3): Boolean;
var
  pthat, pthis: array[0..1] of TVector4;
  mtxThis, mtxThat, mtxDif: TMatrix4;
  xy1,xy2: array[0..1] of TPOint2;
  z1,z2: array[0..1] of vectorfloat;
  v1,v2: TVector4;
begin
  if c = self then
    exit(false);

  v1 := c.GameObject.TransForm.worldposition;
  v2 := self.GameObject.TransForm.worldposition;

  v1 := v2-v1;


//  if (self.gameobject.classname = 'TOldBanker')
//  and (c.gameobject.classname = 'THobo') then
//    Debug.ConsoleLog('There');

{  if colliderdebug then begin
    if (self.gameobject.classname = 'THobo')
//    and (c.gameobject.classname = 'TOldBanker')
    then
      Debug.ConsoleLog('There2');

    Debug.ConsoleLog('Check collision '+self.gameobject.classname+' with '+c.gameobject.classname);
  end;}

  if v1.Length > greaterof(self.safedistance, c.safedistance) then
    exit(false);

//  if (self.gameobject.classname = 'TOldBanker')
//  and (c.gameobject.classname = 'THobo') then
//    Debug.ConsoleLog('HEre');
//
//  if (self.gameobject.classname = 'THobo')
//  and (c.gameobject.classname = 'TOldBanker') then
//    Debug.ConsoleLog('HEre');


  vec := Vector3(0,0,0);
  if not (c is TBoxCollider) then
    raise ECritical.create('don''t understand how to collide with '+c.classname)
  else begin

    //collect data about objects' relative spaces in the world
    mtxthis := gameobject.TransForm.CascadeMtx;
    mtxthat := c.GameObject.transform.cascademtx;
    mtxdif := InvertMtx4(mtxthis);
    if (c.gameobject.classname = 'TTruckSide') and (gameobject.classname='TBernie') then
      v1 := vector3(0,0,0);
    //end;

    mtxdif := mtxdif * mtxthat;



    pthis[0] := localcoords[0];
    pthis[1] := localcoords[1];

    //this, in theory, should move points from THAT into THIS space, this means that
    //the local object, at-least will be axis-aligned.
    pthat[0] := TBoxCollider(c).localcoords[0] * mtxDif;
    pthat[1] := TBoxCollider(c).localcoords[1] * mtxDif;

    //for now we will assume that both boxes are axis-aligned
    xy1[0] := pthis[0].XY;
    xy1[1] := pthis[1].xy;
    xy2[0] := pthat[0].xy;
    xy2[1] := pthat[1].xy;
    order(xy1[0], xy1[1]);
    order(xy2[0], xy2[1]);

    //lets look at the X axis first, because we'll assume that it is the busiest
    //and the first to get tossed
    if xy1[1].x < xy2[0].x then
      exit(false);

    if xy1[0].X > xy2[1].x then
      exit(false);

    if xy1[1].y < xy2[0].y then
      exit(false);

    if xy1[0].y > xy2[1].y then
      exit(false);

    z1[0] := pthis[0].z;
    z1[1] := pthis[1].z;
    z2[0] := pthat[0].z;
    z2[1] := pthat[1].z;
    order(z1[0],z1[1]);
    order(z2[0],z2[1]);

    if z1[1] < z2[0] then
      exit(false);

     if z1[0] > z2[1] then
      exit(false);

    //determine centers of colliders;

    v1 := (pthis[0] + pthis[1]) * Vector4(0.5,0.5,0.5,0.5);
    v2 := (pthat[0] + pthat[1]) * Vector4(0.5,0.5,0.5,0.5);

    vec := (v2-v1).GetXYZ;

    result := true;




  end;


end;



initialization
  compfact := TClassClassFActory.create;
  gameobject_distfromcameracompare :=   function(const Left, Right: TGameObject): Integer
                          var f: double;
                          begin
                            if right = nil then exit(1);
                            if Left = nil then exit(-1);
                             f := left.realdistfromcamera-right.realdistfromcamera;
                             if (f>0) then exit(-1);
                             if (f<0) then exit(1);
                             exit(0);
                          end;


end.
