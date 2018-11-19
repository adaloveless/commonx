unit advancedgraphics_DX;
{x$INLINE AUTO}
{$DEFINE USE_ACTUAL_SURFACE_WIDTH}
interface

uses
  typex, tickcount, generics.collections.fixed, betterobject, controls, sysutils, geometry, AdvancedGraphics, messages,
  sharedobject, graphics, types, stdctrls, classes, direct3d9_jedi, dx9_tools, easyimage, exe, tools, colorconversion, colorblending, fastbitmap,
  extctrls, d3dutils, dxut, DXUTMisc, comctrls, systemx, d3dx9, windows, stringx, helpers.stream, stringx.ansi, numbers, orderlyinit, applicationparams, namevaluepair;


CONST
  BOUND_SNAP = 1;
  BOUND_STACK_SIZE = 256;
  PHYSICS_INCREMENT = 8;
  MAX_VERTEX_BATCH_SIZE = 20000;
type
  TDXMouseEvent = (dxmeDown, dxmeUp, dxmeMove, dxmeEnter, dxmeLeave);
  TDXTime = cardinal;
{$IFDEF CPUx64}
  TDXFloat = single;
{$ELSE}
  TDXFloat = single;
{$ENDIF}
  TDXButtonSTate = (bsUp, bsDown, bsStuckDown, bsStuckUp);
  TFontinfo = record
    cellXOffset: integer;
    cellYOffset: integer;
    cellsAcross: integer;
    overdrawX: single;
    overdrawY: single;
    tex: INativeTexture;
  end;
  TBounds = record
    x1,y1,x2,y2: TDXFloat;
    mx1,my1,mx2,my2: TDXFloat;
  end;
  TVolatileTexturePair = record
    sys,vid: IDirect3dTexture9;
  end;
  TVolatileTexturePairArray = array[0..0] of TVolatileTExturePair;
  PVolatileTexturePair = ^TVolatileTExturePair;
  PVolatileTexturePairArray = ^TVolatileTExturePairArray;

  PBounds = ^TBounds;

  TDX2d = class;//forward
  TTextFlags = (tfLast, tfBold, tfEmboss, tfShadow, tfStroke);
  TTFlags = set of ttextFlags;

  TDXControl = class(TBetterObject)
  private
    FDX: TDX2D;
    FDXChildren: TList<TDXControl>;
    FParent: TObject;
    sect: _RTL_CRITICAL_SECTION;
    FVisible: boolean;
    FTargetTop: single;
    FTargetLEft: single;
    FInMotion: boolean;
    FTargetWidth: single;
    FTargetHeight: single;
    FClickOnMouseUp: boolean;
    FScissorMe: boolean;


    procedure Detachchildren;
    procedure SetParent(const Value: Tobject);
    function GetBottom: single;
    function GEtRight: single;
    procedure SetHeight(const Value: single);
    procedure SetLeft(const Value: single);
    procedure SetTop(const Value: single);
    procedure SetWidth(const Value: single);
    function GetShowing: boolean;
  protected
    FWidth: single;
    Ftop: single;
    FLeft: single;
    FHeight: single;
    FMouseIsOver: boolean;
    FMouseRight: boolean;
    FMouseWasDown: boolean;
    ScreenBound: TSingleRect;
    mouse_buttons_down: array [0..2] of boolean;

    procedure Resized;virtual;
    procedure MouseOver;
    procedure MouseClick;
    procedure MouseRightClick;
    procedure MouseDown;
    procedure MouseUp;
    procedure MouseOut;
    procedure MouseEnter;
    procedure MouseLeave;
    procedure MouseCancel;

    procedure DoMouseover;virtual;
    procedure DoMouseClick;virtual;
    procedure DoMouseRightClick;virtual;
    procedure DoMouseDown;virtual;
    procedure DoMouseUp;virtual;
    procedure DoMouseEnter;virtual;
    procedure DoMouseLeave;virtual;
    procedure DoMouseOut;virtual;


    procedure MouseMove(x,y: single);virtual;
    function MouseEvent(dxme: TDXMouseEvent; screenx, screeny,x,y: single; ss: TShiftState): boolean;
    function IsInBounds(screenx, screeny: single): boolean;
    procedure CancelMouseButtons;
    property ClickOnMouseUp: boolean read FClickOnMouseUp write FClickOnMouseUp;
  public
    constructor Create(aDX: TDX2d);reintroduce;virtual;
    destructor Destroy;override;
    procedure Assimilate;
    property DX: TDX2d read FDX write FDX;
    procedure AddChild(c: TDXControl);
    procedure RemoveChild(C: TDXControl);
    property Parent: TObject read FParent write SetParent;

    property Left: single read FLeft write SetLeft;
    property Top: single read Ftop write SetTop;
    property Width: single read FWidth write SetWidth;
    property Height: single read FHeight write SetHeight;
    property Right: single read GEtRight;
    property Bottom: single read GetBottom;

    property TargetLEft: single read FTargetLEft write FTargetLEft;
    property TargetWidth: single read FTargetWidth write FTargetWidth;
    property TargetHeight: single read FTargetHeight write FTargetHeight;
    property TargetTop: single read FTargetTop write FTargetTop;



    function LocalToScreenX(x: single): single;
    function LocalToScreenY(Y: single): single;
    function X_RolledUp: single;
    function Y_RolledUp: single;


    procedure Draw;
    procedure DrawTop;
    procedure DrawChildren;virtual;
    procedure DrawTopChildren;virtual;

    procedure DoDraw;virtual;
    procedure DoDrawTop;virtual;
    procedure Lock;
    procedure Unlock;
    property Visible: boolean read FVisible write FVisible;
    property InMotion: boolean read FInMotion write Finmotion;
    procedure HandlePhysics(rDeltaTime: TDXTime);virtual;
    procedure HandlePhysicsOfChildren(rDeltaTime: TDXTime);virtual;

    procedure PositionBelow(cAbove: TDXControl; gap: ni = 0; match_width: boolean = true);
    procedure PositionAbove(cBelow: TDXControl; gap: ni = 0; match_width: boolean = true);
    procedure PositionLeft(cRight: TDXControl; gap: ni = 0; match_height: boolean = true);
    procedure PositionRight(cLeft: TDXControl; gap: ni = 0; match_height: boolean = true);
    property showing: boolean read GetShowing;
    property ScissorMe: boolean read FScissorMe write FScissorMe;

  end;

  TDXButton = class(TDXControl)
  private
    FOver: boolean;
    FState: TDXbuttonState;
    FOnClick: TNotifyEvent;
    FCaption: string;
    FSticky: boolean;
    FcolorwhenLit: TNativeFloatColor;
    FcolorwhenNotLit: TNativeFloatColor;
    FCharheight: single;
    FCharWidth: single;
    FTArgetCharWidth: single;
    FTArgetCharheight: single;
    FOnRightClick: TNotifyEvent;
    FStepCount: ni;
    FStep: ni;
    FAutoTextSize: boolean;
    FAnimate: boolean;
    function GetStuckDown: boolean;
    procedure SetStuckDown(const Value: boolean);
    function GetDown: boolean;
    procedure SetDown(const Value: boolean);
    procedure SetSticky(const Value: boolean);
    procedure SetCharHeight(const Value: single);
    procedure SEtCharWidth(const Value: single);
    procedure SetTargetCharWidth(const Value: single);
    procedure SetAnimate(const Value: boolean);
  strict protected
    procedure DoClick;virtual;
    procedure DoRightClick;virtual;
    property State: TDXbuttonState read FState write FState;
    procedure InternalAnimation(rDeltaTime: TDXTime);virtual;
    procedure OnStartAnimation;virtual;
    procedure OnStopAnimiation;virtual;
  protected
    procedure Resized; override;
  public
    tag: int64;
    procedure Init;override;

    property Over: boolean read FOver write FOver;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnRightClick: TNotifyEvent read FOnRightClick write FOnRightClick;
    procedure DoMouseOver;override;
    procedure DoMouseClick;override;
    procedure DoMouseRightClick;override;
    procedure DoMouseDown;override;
    procedure DoMouseUp;override;
    procedure DoMouseOut;override;
    procedure DoDraw;override;
    procedure DoDrawAnimation;virtual;
    property Caption: string read FCaption write FCaption;
    property Sticky: boolean read FSticky write SetSticky;
    property StuckDown: boolean read GetStuckDown write SetStuckDown;
    property Down: boolean read GetDown write SetDown;
    property ColorWhenLit: TNativeFloatColor read FcolorwhenLit write FColorWhenlit;
    property ColorWhenNotLit: TNativeFloatColor read FcolorwhenNotLit write FColorWhenNotlit;
    property Charwidth: single read FCharWidth write SEtCharWidth;
    property CharHeight: single read FCharheight write SetCharHeight;
    property TArgetCharwidth: single read FTArgetCharWidth write SetTargetCharWidth;
    property TArgetCharHeight: single read FTArgetCharheight write FTArgetcharheight;

    procedure HandlePhysics(rDeltaTime: TDXTime);override;
    property StepCount: ni read FStepCount write FStepCount;
    property Step: ni read FStep write FStep;
    property AutoTextSize: boolean read FAutoTextSize write FAutoTextSize;
    procedure CalcTextSize;
    property Animate: boolean read FAnimate write SetAnimate;

  end;


  TDX2D = class(TCustomPanel)
  private
    FUseScreenCoordinates: boolean;
    FLeftMargin: TDXFloat;
    FrightMargin: TDXFloat;
    FBottomMargin: TDXFloat;
    FTopMargin: TDXFloat;
    FBoundX2: TDXFloat;
    FBoundY2: TDXFloat;
    FBoundX1: TDXFloat;
    FBoundY1: TDXFloat;
    FScale: TDXFloat;
    FConstrainProportions: boolean;
    FDirty: boolean;
    FAlphaOp: TAlphaOp;
    FTextures: TList<INativeTexture>;
    FFonts: TList<TFontInfo>;
    FTextureCache: CDXUTResourceCache;
    FTargetBoundX2: TDXFloat;
    FTargetBoundY2: TDXFloat;
    FTargetBoundX1: TDXFloat;
    FTargetBoundY1: TDXFloat;
    FVertexBatch: array[0..MAX_VERTEX_BATCH_SIZE] of TNativeVertex;
    FBoundStack: array[0..BOUND_STACK_SIZE-1] of TBounds;
    FBoundStackPointer: integer;
    FBatchIndex: integer;
    fBatchType: TD3DPrimitiveType;
    FOnMouseDown: TMouseEvent;
    FFrames: integer;
    FDXControls: TList<TDXControl>;
    FDXChildren: TList<TDXControl>;
    FTexturesLoaded: boolean;
    FOnMouseEnter: TNotifyEvent ;
    FOnMouseLeave: TNotifyEvent ;
    FLastUpdateTime, FFrameTime: TDXTime;
    FCharHeight: TDXFloat;
    FCharWidth: TDXFloat;
    FCharPadV: TDXFloat;
    FCharPadU: TDXFloat;
    FTexturesInitiallyLoaded: boolean;

    FLineColor: cardinal;
    FMaxSurfaceWidth: ni;
    FMinSurfaceWidth: ni;
    FMinSurfaceHeight: ni;
    FMaxSurfaceHeight: ni;
    FSurfaceWidth: ni;
    FSurfaceheight: ni;
    function ScaleScreenToCanvas(x: TDXFloat): integer;
    function GetBottomMargin: TDXFloat;
    function GetLeftMargin: TDXFloat;
    function GetrightMargin: TDXFloat;
    function GetTopMargin: TDXFloat;
    procedure SetBottomMargin(const Value: TDXFloat);
    procedure SetLeftMargin(const Value: TDXFloat);
    procedure SetrightMargin(const Value: TDXFloat);
    procedure SetTopMargin(const Value: TDXFloat);
    function GetDimensionX: TDXFloat;
    function GetDimensiONY: TDXFloat;
    procedure SetDimensionX(const Value: TDXFloat);
    procedure SetDimensionY(const Value: TDXFloat);
    procedure SetBoundX1(const Value: TDXFloat);
    procedure SetBoundX2(const Value: TDXFloat);
    procedure SetBoundY1(const Value: TDXFloat);
    procedure SetBoundY2(const Value: TDXFloat);
    procedure SetScale(const Value: TDXFloat);
    procedure SetConstrainProportions(const Value: boolean);
    procedure SetTargetBoundX1(const Value: TDXFloat);
    procedure SetTargetBoundX2(const Value: TDXFloat);
    procedure SetTargetBoundY1(const Value: TDXFloat);
    procedure SetTargetBoundY2(const Value: TDXFloat);
    procedure SetAlphaOp(const Value: TAlphaOp);
    function GetCurrentFont: TFontInfo;
    procedure SetLineColor(const Value: cardinal);
    function GetBottomMarginF: TDXFloat;
    function GetLeftMarginF: TDXFloat;
    function GetrightMarginF: TDXFloat;
    function GetTopMarginF: TDXFloat;
    procedure SetBottomMarginF(const Value: TDXFloat);
    procedure SetLeftMarginF(const Value: TDXFloat);
    procedure SetrightMarginF(const Value: TDXFloat);
    procedure SetTopMarginF(const Value: TDXFloat);


  strict private
    linecache: ID3DXLine;
    linecache_verts: array[0..2047] of TD3DXVector2;
    linecache_idx: ni;
    procedure CommitVertexBatch;
    function BeginDXLineDraw: ID3DXLine;
    procedure EndDXLine(l: ID3DXLine);
    procedure PendingResetTimer(sender: TObject);
  protected
    fLastMouseX, FLastMouseY: nativefloat;
    fLastMouseXX, FLastMouseYY: nativefloat;
    sect: _RTL_CRITICAL_SECTION;
    material: TNativeMaterial;
    mouse_buttons_down:array[0..2] of boolean;
    font_index: ni;
    MouseHandled: boolean;

    FLoadedNativeTexture: INativeTexture;
    FLastTextFlags: TTFlags;
    FInScene:  boolean;
    bInitialized: boolean;
    ControlsDrawn: boolean;
    tmResetTimer: tTimer;

    procedure SetParent(AParent: TWinControl); override;
    procedure InitControls;virtual;

    procedure BeginVertexBatch(mode: TD3DPrimitiveType = D3DPT_trianglelist);inline;
    procedure NeedBatchSpace(iQty: integer);inline;
    procedure NextBatchVertex;inline;
    procedure EndVertexBatch;
    procedure CommitBatch;

    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure CancelMouse;

    procedure DoMouseover;virtual;
    procedure DoMouseClick;virtual;
    procedure DoMouseRightClick;virtual;
    procedure DoMouseDown;virtual;
    procedure DoMouseUp;virtual;
    procedure DoMouseEnter;virtual;
    procedure DoMouseLeave;virtual;
    procedure DoMouseOut;virtual;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;override;


    function BroadcastMouseEvent(dxme: TDXMouseEvent;shift: TShiftState; x_screen, y_screen,x,y: single): boolean;

    procedure VolatileTextureFromFastBitmap(var tex: IDirect3dTexture9; fbm: TFastbitmap);
    property CurrentFont: TFontInfo read GetCurrentFont;
    procedure DelayReset;
  public
    mouse_last_pos_for_wheel: TPoint;
    TextColor: Tcolor;
    TextStrokeColor: Tcolor;
    TextEmbossColor: Tcolor;
    TextShadowColor: TColor;
    TextOffset : TNativeVector;
    physicsUpdatedTo: ticker;
    fpsframes: ni;
    fps: ni;
    lastFPSUpdateTime: ticker;

    TextPosition: TPoint;
    dx9window: dx9_tools.TDX9Window;
    function BatchMode: boolean;inline;
    procedure LoadTexture(sfile: string);
    procedure LoadFont(sfile: string; rOverdrawX: single = 1.0; rOverDrawY: single = 1.0);

    function CreateVolatileTExtureTarget(w,h: nativeint; format: cardinal = D3DFMT_A8R8G8B8): IDirect3DTexture9;
    function CreateVolatileTexture(w,h: nativeint; format: cardinal = D3DFMT_A8R8G8B8): IDirect3DTexture9;
    function CreateCompressedTexture(w,h: nativeint): IDirect3dTExture9;
    function CreateCompressedTextureFromImage(fbm: TFastBitmap): IDirect3dTExture9;overload;
    function CreateCompressedTextureFromImage(sFile: string): IDirect3dTExture9;overload;
    function CreateCompressedTextureFromJPGPointer(var old: IDirect3dTexture9; w,h: nativeint; p: pointer; pLen: nativeint): IDirect3dTExture9;

    function CreateCompressedTextureFromCRNPointer(old: IDirect3dTexture9; w,
      h: nativeint; p: pointer; pLen: nativeint): IDirect3dTExture9;


    procedure CreateVolatileTexturePair(w,h: nativeint; out sys,vid: IDirect3DTexture9; format: cardinal = D3DFMT_A8R8G8B8; bRenderTarget: boolean = false);overload;
    procedure CreateVolatileTexturePair(w,h: nativeint; out pair: TVolatileTexturePair; format: cardinal = D3DFMT_A8R8G8B8);overload;
    procedure UpdateVolatileTexture(texsys,texvid: IDirect3dTexture9);
    procedure UpdateVolatileSurface(texsys,texvid: IDirect3dSurface9);
    procedure SetTexture(iSlot: integer);overload;
    procedure SetTexture(tex: IDirect3dTexture9);overload;
    procedure SetFont(iSlot: integer);

    procedure Unloadtexture(iSlot: integer);
    procedure Unloadfont(iSlot: integer);

    procedure Sprite(xCenter, yCenter: single; c: TColor;alpha: single; xSize: single; ySize: single = -1);overload;
    procedure Sprite(xCenter, yCenter: single; c1,c2,c3,c4: TColor;alpha: single; xSize: single; ySize: single = -1);overload;
    procedure Quad(x1,y1,x2,y2,x3,y3,x4,y4: single; clr1,clr2,clr3,clr4: TColor; alpha: TDXFloat = 1.0);

    procedure canvas_Quad(x1,y1,x2,y2,x3,y3,x4,y4: single; clr1,clr2,clr3,clr4: TColor; alpha: TDXFloat = 1.0);

    procedure canvas_batch_Quad(x1,y1,x2,y2,x3,y3,x4,y4: single; clr1,clr2,clr3,clr4: TColor; alpha: TDXFloat = 1.0);

    procedure canvas_Rectangle_Fill(x1, y1, x2, y2: TDXFloat; clr1: TColor;
      clr2: TColor; clr3: TColor; clr4: TColor; alpha: TDXFloat = 1.0);

    procedure canvas_batch_Rectangle_Fill(x1, y1, x2, y2: TDXFloat; clr1: TColor;
      clr2: TColor; clr3: TColor; clr4: TColor; alpha: TDXFloat = 1.0);

    procedure canvas_batch_Rectangle_Fill_UV(x1, y1, x2, y2,u1,v1,u2,v2: TDXFloat; clr1: TColor;
      clr2: TColor; clr3: TColor; clr4: TColor; alpha: TDXFloat = 1.0);

    procedure DLGCode(var message: TMessage);message WM_GETDLGCODE;



    procedure canvas_Rectangle_Empty(x1, y1, x2, y2: TDXFloat; clr1: TColor;
      clr2: TColor; clr3: TColor; clr4: TColor; alpha: TDXFloat = 1.0);

    procedure ClearScreen(c: Tcolor = clBlack);

    procedure canvas_Text(sText: ansistring; x1,y1: TDXFloat; c: TColor; alpha: TDXFloat = 1.0; rScale: TDXFloat = 1.0);overload;
    procedure canvas_Text(sText: ansistring; x1,y1: TDXFloat; rFlags: TTFLags);overload;
    procedure canvas_Text(sText: string; rFlags: TTFLags = [tfLast]);overload;
    procedure canvas_Text_shadow(sText: ansistring; x1,y1: TDXFloat; c: TColor; alpha: TDXFloat = 1.0; rScale: TDXFloat = 1.0);
    procedure canvas_Text_emboss(sText: ansistring; x1,y1: TDXFloat; c: TColor; alpha: TDXFloat = 1.0; rScale: TDXFloat = 1.0);

    procedure TextOut(sText: ansistring; x1, y1: TDXFloat; c: TColor; alpha, rScale: TDXFloat);overload;
    procedure TextOut(sText: ansistring; x1,y1: TDXFloat; rFlags: TTFLags);overload;


    constructor Create(aOwner: TComponent); override;

    procedure CReateDXResources;
    procedure DestroyDXResouces;
    procedure Init;virtual;

    destructor Destroy; override;
    procedure ClearTextures;
    procedure ClearControls;
    procedure Prepare;
    procedure Flip;
    procedure Reset;
    procedure ResetMargins;
    procedure Draw(bNoFlip: boolean = false);
    procedure BeforeDraw;virtual;
    procedure AfterDraw;virtual;
    procedure DoDraw; virtual;
    procedure Test;
    procedure REsize;override;

    procedure EndScene;inline;
    procedure BeginScene;inline;
    procedure UpdatePhysics;overload;inline;
    procedure UpdatePhysics(rDeltaTime: TDXTime);overload;virtual;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;

    property Frames: integer read FFrames write FFrames;
    procedure Initialize;
    procedure Paint; override;
    procedure Rectangle_Fill(x1, y1, x2, y2: TDXFloat; clr: TColor;
      alpha: TDXFloat = 1.0);overload;
    procedure Rectangle_Fill(x1, y1, x2, y2: TDXFloat; c1,c2,c3,c4: TColor;
      alpha: TDXFloat = 1.0);overload;
    procedure Rectangle(x1, y1, x2, y2: TDXFloat; color: TColor;
      Fill: boolean = false; bGradient: boolean = false;
      bgcolor: TColor = clBlack); overload; virtual;

    function ScaleGlobalXtoScreen(Xdistance: TDXFloat): TDXFloat;
    function ScaleGlobalYtoScreen(Ydistance: TDXFloat): TDXFloat;
    function ScaleGlobalXtoForm(Xdistance: TDXFloat): TDXFloat;
    function ScaleGlobalYtoForm(Ydistance: TDXFloat): TDXFloat;
    function ScaleScreenXtoGlobal(Xdistance: TDXFloat;
      bKeepSign: boolean = false): TDXFloat;
    function ScaleScreenYtoGlobal(Ydistance: TDXFloat;
      bKeepSign: boolean = false): TDXFloat;
    function GlobalToScreenX(globalX: TDXFloat): TDXFloat;
    function GlobalToScreenY(globalY: TDXFloat): TDXFloat;
    function GlobalToFormX(globalX: TDXFloat): TDXFloat;
    function GlobalToFormY(globalY: TDXFloat): TDXFloat;

    function ScreenToGlobalX(screenX: TDXFloat): TDXFloat;
    function ScreenToGlobalY(screenY: TDXFloat): TDXFloat;
    function FormToGlobalX(formX: TDXFloat): TDXFloat;
    function FormToGlobalY(formY: TDXFloat): TDXFloat;
    function FormToGlobal(formP: TnativefloatPoint): TNativeFLoatPOint;
    function GLobalToForm(formP: TnativefloatPoint): TNativeFLoatPOint;

    property TopMargin: TDXFloat read GetTopMargin write SetTopMargin;
    property LeftMargin: TDXFloat read GetLeftMargin write SetLeftMargin;
    property RightMargin: TDXFloat read GetrightMargin write SetrightMargin;
    property BottomMargin: TDXFloat read GetBottomMargin write SetBottomMargin;

    property TopMarginF: TDXFloat read GetTopMarginF write SetTopMarginF;
    property LeftMarginF: TDXFloat read GetLeftMarginF write SetLeftMarginF;
    property RightMarginF: TDXFloat read GetrightMarginF write SetrightMarginF;
    property BottomMarginF: TDXFloat read GetBottomMarginF write SetBottomMarginF;

    property UseScreenCoordinates
      : boolean read FUseScreenCoordinates write FUseScreenCoordinates;

    property DimensionX: TDXFloat read GetDimensionX write SetDimensionX;
    property DimensionY: TDXFloat read GetDimensiONY write SetDimensionY;

    property BoundX1: TDXFloat read FBoundX1 write SetBoundX1;
    property BoundX2: TDXFloat read FBoundX2 write SetBoundX2;
    property BoundY1: TDXFloat read FBoundY1 write SetBoundY1;
    property BoundY2: TDXFloat read FBoundY2 write SetBoundY2;

    property TargetBoundX1: TDXFloat read FTargetBoundX1 write SetTargetBoundX1;
    property TargetBoundX2: TDXFloat read FTargetBoundX2 write SetTargetBoundX2;
    property TargetBoundY1: TDXFloat read FTargetBoundY1 write SetTargetBoundY1;
    property TargetBoundY2: TDXFloat read FTargetBoundY2 write SetTargetBoundY2;

    procedure ApplyInterpolatedBoundsChanges;

    procedure EnableScissor(b: boolean);
    procedure ScissorControl(c: TDXControl);



    procedure BoundsAtIdentity;
    procedure PushIdentityBounds;
    procedure SetIdentityBounds;
    procedure ClearBoundsRect;
    procedure RecalcScale;
    property Scale: TDXFloat read FScale write SetScale;

    property ConstrainProportions: boolean read FConstrainProportions write
      SetConstrainProportions;

    property Dirty: boolean read FDirty write FDirty;
    property AlphaOp: TAlphaOp read FAlphaOp write SetAlphaOp;
    procedure LoadTextures;virtual;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TNotifyEvent  read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent  read FOnMouseLeave write FOnMouseLeave;
    procedure PushBounds;
    procedure PopBounds;

    procedure AddControl(C: TDXControl);
    procedure RemoveControl(C: TDXControl);
    procedure AddDXchild(C: TDXControl);
    procedure RemoveDXChild(C: TDXControl);

    procedure DrawControls;virtual;
    property TexturesLoaded: boolean read FTexturesLoaded write FtexturesLoaded;
    property TexturesInitiallyLoaded: boolean read FTexturesInitiallyLoaded write FTexturesInitiallyLoaded;
    property CharWidth: TDXFloat read FCharWidth write FcharWidth;
    property CharHeight: TDXFloat read FCharHeight write FCharHeight;
    property CharPadU: TDXFloat read FCharPadU write FCharPadU;
    property CharPadV: TDXFloat read FCharPadV write FCharPadV;

    procedure OnReset;
    procedure SetTypicalTextAlpha;
    procedure SetTextPosition(x,y: nativeint);
    procedure ResetText;
    procedure ResetSurfaceResolution(x,y: nativeint);
    function IsFunctional: boolean;inline;
    procedure Lock;
    procedure Unlock;
    property Initialized: boolean read bInitialized;
    procedure CopySurface_VidMem(src, dest: IDirect3dSurface9);
    procedure CopyTexture_SysMem(src, dest: IDirect3dTexture9);
    procedure BeginLine;
    procedure FlushLine;
    procedure EndLIne;
    procedure DrawLine(x,y: single; alpha_color: cardinal);
    property LineColor: cardinal read FLineColor write SetLineColor;
    property MinSurfaceWidth: ni read FMinSurfaceWidth write FMinSurfaceWidth;
    property MaxSurfaceWidth: ni read FMaxSurfaceWidth write FMaxSurfaceWidth;
    property MinSurfaceHeight: ni read FMinSurfaceHeight write FMinSurfaceHeight;
    property MaxSurfaceHeight: ni read FMaxSurfaceHeight write FMaxSurfaceHeight;
    property SurfaceHeight: ni read FSurfaceheight;
    property SurfaceWidth: ni read FSurfaceWidth;
  end;

{ TDrawingBoard = class(TFastBackBufferedControl)
  private
    FBoundY2: TDXFloat;
    FBoundX1: TDXFloat;
    FBoundY1: TDXFloat;
    FBoundX2: TDXFloat;
    FOnPaint: TNotifyEvent;
    FScale: TDXFloat;
    FConstrainProportions: boolean;
    FLineColor, FFillColor: TColor;
    FUseScreenCoordinates: boolean;
    FLeftMargin: TDXFloat;
    FrightMargin: TDXFloat;
    FBottomMargin: TDXFloat;
    FTopMargin: TDXFloat;
    FDEbug: boolean;
    FCachedScanLine: array of byte;
    FCachedScanLineY: integer;
    FTextRotate: TTextRotate;
    FTextJustify: TTextJustify;
    FTextHeight: TDXFloat;
    FTextColor: TColor;
    FNewTextAlginRules: boolean;
    FNewTextAlignRules: boolean;
    FTextBackgroundColor: TColor;
    FTextKnockout: TTextKnockout;
    FAutoTruncateText: boolean;
    FScreenMaskX2: TDXFloat;
    FScreenMaskY2: TDXFloat;
    FScreenMaskX1: TDXFloat;
    FScreenMaskY1: TDXFloat;
    FMouseX: integer;
    FMouseY: integer;
    FOnTDXFloatMouse: TTDXFloatMouseEvent;
    FAlphaOp: TAlphaOp;
    function GetCenterX: TDXFloat;
    function GetCenterY: TDXFloat;

    function GetFillColor: TColor;
    function GetLineColor: TColor;
    procedure SetFillColor(const Value: TColor);
    procedure SetLineColor(const Value: TColor);
    procedure SetBoundX1(const Value: TDXFloat);
    procedure SetBoundX2(const Value: TDXFloat);
    procedure SetBoundY1(const Value: TDXFloat);
    procedure SetBoundY2(const Value: TDXFloat);
    procedure SetDimensionX(const Value: TDXFloat);
    procedure SetDimensionY(const Value: TDXFloat);
    procedure SetScale(const Value: TDXFloat);
    procedure RecalcScale;
    procedure SetConstrainProportions(const Value: boolean);
    function GetDimensionX: TDXFloat;
    function GetDimensiONY: TDXFloat;
    procedure OldFatLine(gx1, gy1, gx2, gy2, width: TDXFloat; color: TColor;
      bfirstSegment: boolean);
    function GetBottomMargin: TDXFloat;
    function GetLeftMargin: TDXFloat;
    function GetrightMargin: TDXFloat;
    function GetTopMargin: TDXFloat;
    procedure SetBottomMargin(const Value: TDXFloat);
    procedure SetLeftMargin(const Value: TDXFloat);
    procedure SetrightMargin(const Value: TDXFloat);
    procedure SetTopMargin(const Value: TDXFloat);

    function CanvasToScreenY(canvasY: TDXFloat): TDXFloat;
    function GetTextColor: TColor;
    procedure SetTextcolor(const Value: TColor);
    procedure SetTextBackgroundcolor(const Value: TColor);
    function GetDrawTo_pixels(x, y: integer): TColor;
    procedure SetDrawTo_Pixels(x, y: integer; const Value: TColor);
    procedure SetScreenMask(x1, y1, x2, y2: TDXFloat);
    procedure MSG_TDXFloatMouse(var msg: TMessage);
    procedure MSG_TDXFloatMouseL(var msg: TMessage);
    procedure MSG_TDXFloatMouseR(var msg: TMessage);

  protected
    procedure DesignDraw;
    function ScaleGlobalXtoScreen(Xdistance: TDXFloat): TDXFloat;
    function ScaleGlobalYtoScreen(Ydistance: TDXFloat): TDXFloat;
    function ScaleScreenXtoGlobal(Xdistance: TDXFloat; bKeepSign: boolean = false): TDXFloat;
    function ScaleScreenYtoGlobal(Ydistance: TDXFloat; bKeepSign: boolean = false): TDXFloat;
    function ScaleScreenToCanvas(x: TDXFloat): integer;
    function ScaleCanvasToScreen(x: integer): TDXFloat;
    procedure CanvasConnect; virtual;
    procedure FindFontSize(width, height: integer; sText: ansistring);

  public
    property NewTextAlignRules
      : boolean read FNewTextAlignRules write FNewTextAlignRules;
    procedure InvertRect(canvasX1, canvasY1, canvasX2, canvasY2: integer);
    function CanvasToScreenX(canvasX: TDXFloat): TDXFloat;
    function ScreenToCanvasX(screenX: TDXFloat): TDXFloat;
    function ScreenToCanvasY(screenY: TDXFloat): TDXFloat;

    function GlobalToCanvasX(globalX: TDXFloat): TDXFloat;
    function GlobalToCanvasY(globalY: TDXFloat): TDXFloat;

    function CanvasToGlobalX(canvasX: TDXFloat): TDXFloat;
    function CanvasToGlobalY(canvasY: TDXFloat): TDXFloat;

    function GlobalToScreenX(globalX: TDXFloat): TDXFloat;
    function GlobalToScreenY(globalY: TDXFloat): TDXFloat;
    function ScreenToGlobalX(screenX: TDXFloat): TDXFloat;
    function ScreenToGlobalY(screenY: TDXFloat): TDXFloat;
    function GlobalToCanvasXi(globalX: TDXFloat): integer;
    function GlobalToCanvasYi(globalY: TDXFloat): integer;

    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;
    property LineColor: TColor read GetLineColor write SetLineColor;
    property FillColor: TColor read GetFillColor write SetFillColor;

    procedure Resize; override;

    procedure FatPoint(x, y: TDXFloat; cInner, cOuter: TColor; size: integer = 3);
    procedure FancyRectangle(x1, y1, x2, y2: TDXFloat; color: TColor);

    procedure SetPoint(x1, y1: TDXFloat; color: TColor); virtual;
    function GetFastPoint(x, y: integer): TColor;
    procedure SetFastPoint(x, y: integer; color: TColor);
    procedure SetLowResPoint(gx, gy: TDXFloat; col: TColor);
    procedure LowResLine(gx1, gy1, gx2, gy2: TDXFloat; color: TColor;
      bDotted: boolean);
    procedure FatLine(gx1, gy1, gx2, gy2: TDXFloat; width: TDXFloat; color: TColor;
      bScreenWidth: boolean); virtual;
    procedure FatBox(gx1, gy1, gx2, gy2, width: TDXFloat; color: TColor;
      bScreenWidth: boolean);
    procedure Flip; override;
    procedure Line(gx1, gy1, gx2, gy2: TDXFloat; color: TColor); virtual;
    procedure Tri(gx1, gy1, gx2, gy2, gx3, gy3: TDXFloat; color: TColor); virtual;
    procedure Slice(centerX, centerY, gx1, gy1, gx2, gy2: TDXFloat; color: TColor);
      virtual;
    procedure Quad(gx1, gy1, gx2, gy2, gx3, gy3, gx4, gy4: TDXFloat;
      color: TColor); virtual;
    procedure Rectangle(x1, y1, x2, y2: TDXFloat; color: TColor;
      Fill: boolean = false; bGradient: boolean = false;
      bgcolor: TColor = clBlack); overload;
      virtual;
// procedure Rectangle(x1,y1,x2,y2: integer; color: TColor; Fill: boolean = false); overload; virtual;
    procedure Circle(x1, y1, x2, y2: TDXFloat; color: TColor;
      Fill: boolean = false); virtual;
    procedure FatCircle(x1, y1: TDXFloat; radius: TDXFloat; width: TDXFloat; color: TColor;
      FillColor: TColor = clBlack; Fill: boolean = false); virtual;
    procedure FatPartialCircle(globalX, globalY, global_radius: TDXFloat;
      rStart, rEnd: TDXFloat; width: TDXFloat; color, FillColor: TColor;
      Fill: boolean);
    procedure Triangle(x1, y1, x2, y2, x3, y3: TDXFloat; color: TColor;
      FillColor: TColor = clBlack; bFill: boolean = false);

// property TextAlign: TTextAlign read FTextAlign write FTextAlign;
    property TextJustify: TTextJustify read FTextJustify write FTextJustify;
    property TextRotate: TTextRotate read FTextRotate write FTextRotate;
    property TextHeight: TDXFloat read FTextHeight write FTextHeight;
    procedure Text(x, y: TDXFloat; sText: ansistring); overload;
    procedure Text(globalX, globalY, width, height: TDXFloat; sText: ansistring;
      bCenter: boolean = false; bVertical: boolean = false); overload;
    procedure Text(globalX, globalY, width, height: TDXFloat; sText: ansistring;
      just: TTextJustify = tjLeft; rot: TTextRotate = trHorizontal); overload;
    property TextColor: TColor read GetTextColor write SetTextcolor;
    property TextKnockOut: TTextKnockout read FTextKnockout write FTextKnockout;
    property TextBackgroundColor: TColor read FTextBackgroundColor write
      SetTextBackgroundcolor;
    property AutoTruncateText
      : boolean read FAutoTruncateText write FAutoTruncateText;
    function TruncateText(width: integer; sText: ansistring): ansistring;

    procedure Draw; override;
    property UseScreenCoordinates
      : boolean read FUseScreenCoordinates write FUseScreenCoordinates;
    procedure New;
    procedure Clear(color: TColor = clBlack);
    procedure DebugFlip;
    property DrawTo_pixels[x: integer;
    y: integer]
      : TColor read GetDrawTo_pixels write SetDrawTo_Pixels;
    procedure DrawTo_SetPixelEx(x, y: integer; color: TColor;
      AlphaOp: TAlphaOp);
  published
    // expose
    property Align;
    property Anchors;
    property OnClick;
    // ------
    property BoundX1: TDXFloat read FBoundX1 write SetBoundX1;
    property BoundX2: TDXFloat read FBoundX2 write SetBoundX2;
    property BoundY1: TDXFloat read FBoundY1 write SetBoundY1;
    property BoundY2: TDXFloat read FBoundY2 write SetBoundY2;
    property centerX: TDXFloat read GetCenterX;
    property centerY: TDXFloat read GetCenterY;
    property DimensionX: TDXFloat read GetDimensionX write SetDimensionX;
    property DimensionY: TDXFloat read GetDimensiONY write SetDimensionY;
    property Scale: TDXFloat read FScale write SetScale;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property ConstrainProportions: boolean read FConstrainProportions write
      SetConstrainProportions;

    property TopMargin: TDXFloat read GetTopMargin write SetTopMargin;
    property LeftMargin: TDXFloat read GetLeftMargin write SetLeftMargin;
    property RightMargin: TDXFloat read GetrightMargin write SetrightMargin;
    property BottomMargin: TDXFloat read GetBottomMargin write SetBottomMargin;
    property Debug: boolean read FDEbug write FDEbug;

    property screenmaskX1: TDXFloat read FScreenMaskX1 write FScreenMaskX1;
    property screenmaskX2: TDXFloat read FScreenMaskX2 write FScreenMaskX2;
    property screenmaskY1: TDXFloat read FScreenMaskY1 write FScreenMaskY1;
    property screenmaskY2: TDXFloat read FScreenMaskY2 write FScreenMaskY2;
    procedure ClearBoundsRect;
    procedure MSG_TDXFloatMouseLDOWN(var msg: TMessage); message WM_LBUTTONDOWN;
    procedure MSG_TDXFloatMouseRDOWN(var msg: TMessage); message WM_RBUTTONDOWN;
    procedure MSG_TDXFloatMouseLUP(var msg: TMessage); message WM_LBUTTONUP;
    procedure MSG_TDXFloatMouseRUP(var msg: TMessage); message WM_RBUTTONUP;
    procedure MSG_TDXFloatMouseMove(var msg: TMessage); message WM_MOUSEMOVE;
    procedure SetLastMouseposition(x, y: integer);
    property OnTDXFloatMouse: TTDXFloatMouseEvent read FOnTDXFloatMouse write FOnTDXFloatMouse;
    property AlphaOp: TAlphaOp read FAlphaOp write FAlphaOp;
  end; }

function GetFullPathToAsset(sFile: string): string;


var
  dxdx: integer;
  VERTEX_BATCH_SIZE: nativeint;

implementation

uses
  debug;

{ TDX2D }

procedure TDX2D.ClearBoundsRect;
begin
  // no ideA WHAT THIS was supposed to do

end;

procedure TDX2D.ClearControls;
var
  c: TDXControl;
begin
  while FDXControls.Count > 0 do begin
    c := FDXcontrols[0];
    //FDXControls.Remove(c);
    Debug.ConsoleLog('Freeing '+c.classname);
    c.free;
  end;

end;

procedure TDX2D.ClearScreen(c: Tcolor);
begin
  EnableScissor(false);
  AlphaOp := aoNone;
  Rectangle_Fill(BoundX1, BoundY1, BoundX2, boundy2, c);
end;

procedure TDX2D.ClearTextures;
begin
  while FTextures.Count > 0 do begin
    UnloadTexture(0);
  end;
  while FFonts.count > 0 do begin
    Unloadfont(0);
  end;
  FTexturesInitiallyLoaded := false;
  FTexturesLoaded := false;
end;

procedure TDX2D.CommitBatch;
begin
  EndVertexBatch;
  BeginVertexBatch;
end;

procedure TDX2D.CommitVertexBatch;
begin
  if FBatchIndex <= 0 then
    exit;

  if dx9window = nil then exit;
  if dx9window.dev = nil then exit;

  d3dassert(dx9window.dev.DrawPrimitiveUP(FBatchType, FBatchindex div 3, FVertexBatch[0],
      SizeOf(TNativeVertex)));



end;

procedure TDX2D.CopySurface_VidMem(src, dest: IDirect3dSurface9);
var
  p,pb,ppb: pbyte;
  plocked: _D3DLOCKED_RECT;
  r: TRect;
  t, u: integer;
  sz: integer;
  iframe: integer;
  bithead: PBITMAPINFOHEADER;
  a: nativeint;
begin
  d3dassert(dx9window.dev.StretchRect(src, nil, dest, nil, D3DTEXF_NONE));
//  GotoTime(rTime+(1/rFrameRate));
end;

constructor TDX2D.Create(aOwner: TComponent);
begin
  inherited;

  tmResetTimer := TTimer.create(self);
  tmResetTimer.enabled := false;
  tmResetTimer.OnTimer := self.PendingResetTimer;
  InitializeCRiticalSection(sect);
  FMinSurfaceWidth := 720;
  FMaxSurfaceWidth := 1920*2;
  FMinSurfaceHeight := 480;
  FMaxSurfaceHeight := 1080*2;

  FDXControls := Tlist<TDXControl>.create;
  FDXChildren := Tlist<TDXControl>.create;

  FLastTextFlags := [tfSTroke];
  TextColor := $a0a0a0;
  TextEmbossColor := clWhite;
  TextShadowColor := clBlack;
  TExtStrokeColor := clBlack;

  FBatchIndex := -1;
  FDirty := true;
  FTextures := TList<INativeTexture>.Create;
  FFonts := TList<TFontinfo>.Create;
  FCharHeight := 16;
  FCharWidth := 12;
  fCharPadU := 0;
  fCharPadV := 0;


  CReateDXResources;

  BoundX2 := dx9window.surfacewidth;
  BoundY2 := dx9window.surfaceheight;

  material.Emissive.r := 255.0;
  material.Emissive.g := 255.0;
  material.Emissive.b := 255.0;
  material.Emissive.a := 255.0;
  material.Ambient.r := 255.0;
  material.Ambient.g := 255.0;
  material.Ambient.b := 255.0;
  material.Ambient.a := 255.0;
  material.Specular.r := 255.0;
  material.Specular.g := 255.0;
  material.Specular.b := 255.0;
  material.Specular.a := 255.0;
  material.Diffuse.r := 255.0;
  material.Diffuse.g := 255.0;
  material.Diffuse.b := 255.0;
  material.Diffuse.a := 255.0;

  Init;

  InitControls;
  physicsUpdatedTo := GetTicker;

end;

function TDX2D.CreateCompressedTexture(w, h: nativeint): IDirect3dTExture9;
begin
  Self.dx9window.dev.CreateTexture(w,h,1,D3DUSAGE_DYNAMIC,d3dfmt_dxt5,d3dpool_default{ d3dpool_systemmem},result,nil);

end;

function TDX2D.CreateCompressedTextureFromImage(
  sFile: string): IDirect3dTExture9;
var
  surf: IDirect3DSurface9;
begin
  result := CreateCompressedTexture(512,512);
  result.GetSurfaceLevel(0, surf);
  D3DXLoadSurfaceFromFileW(surf, nil, nil, pchar(sFile), nil, D3DX_DEFAULT , 0, nil);
  //D3DXSaveSurfaceToFileW(pchar(sFile+'.jpg') , D3DXIFF_JPG, surf, nil, nil);
end;

function TDX2d.CreateCompressedTextureFromCRNPointer(old: IDirect3dTexture9; w,h: nativeint; p: pointer; pLen: nativeint): IDirect3dTExture9;
var
  surf: IDirect3DSurface9;
  sTempFile: string;
  fs: TFileStream;
  m: pointer;
begin
  sTempFile := systemx.GetTempPath+inttostr(getcurrentthreadid)+'.crn';
  fs := TFileStream.Create(sTempFile, fmCReate);
  try
    Stream_GuaranteeWrite(fs, p, plen);
  finally
    fs.Free;
  end;

  RunProgramAndWait(findtool('crn2dds.exe'), '"'+sTempFile+'"', '', true, false);
  deletefile(pchar(sTempFile));
  sTempFile := changefileext(sTempFile, '.dds');
  fs := TFileStream.Create(sTempFile, fmOpenRead+fmShareExclusive);
  try
    m := GetMemory(fs.Size);
    try
      Stream_GuaranteeRead(fs, m, fs.Size);
      if assigned(old) then
        result := old
      else
        Self.dx9window.dev.CreateTexture(w,h,1,D3DUSAGE_DYNAMIC,D3DFMT_A8R8G8B8,{d3dpool_default} d3dpool_default,result,nil);

      result.GetSurfaceLevel(0, surf);
      d3dassert(D3DXLoadSurfaceFromFileInMemory(surf, nil, nil, m, fs.size, nil, D3DX_DEFAULT , 0, nil));

    finally
      freememory(m);
    end;
  finally
    fs.Free;
  end;

end;

function TDX2d.CreateCompressedTextureFromJPGPointer(var old: IDirect3dTexture9; w,h: nativeint; p: pointer; pLen: nativeint): IDirect3dTExture9;

var
  oldsurf: IDirect3dSurface9;
begin
  oldsurf := nil;
//  result := CReateCompressedTexture(w,h);
  if assigned(old) then
    result := old
  else
    Self.dx9window.dev.CreateTexture(w,h,1,0{D3DUSAGE_DYNAMIC},{D3DFMT_R5G6B5}  D3DFMT_A8R8G8B8,d3dpool_default,result,nil);

  result.GetSurfaceLevel(0, oldsurf);

  d3dassert(D3DXLoadSurfaceFromFileInMemory(oldsurf, nil, nil, p, pLen, nil, D3DX_FILTER_NONE , 0, nil));

end;

function TDX2D.CreateCompressedTextureFromImage(
  fbm: TFastBitmap): IDirect3dTExture9;
var
  surf: IDirect3DSurface9;
begin
  result := CreateCompressedTexture(fbm.Width, fbm.Height);
  result.GetSurfaceLevel(0, surf);
end;

procedure TDX2D.CReateDXResources;
begin
  dx9window := TDX9Window.Create(self);

{$IFDEF USE_ACTUAL_SURFACE_WIDTH}
  FSurfaceHeight := lesserof(greaterof(self.Height, self.MinSurfaceHeight), MaxSurfaceHeight);
  FSurfaceWidth := lesserof(greaterof(self.Width, self.MinSurfaceWidth), MaxSurfaceWidth);
{$ELSE}
  FSurfaceWidth := 1920;
  FSurfaceHeight := 1080;
{$ENDIF}
  Debug.Log('SURFACE HEIGHT = '+FSurfaceHeight.ToString);
  Debug.Log('SURFACE WIDTH = '+FSurfaceWidth.ToString);
  dx9window.SurfaceWidth := FSurfaceWidth;
  dx9window.SurfaceHeight := FSurfaceHeight;
// dx9window.parent := self;
  FTextureCache := CDXUTResourceCache.create;
  //bInitialized := false;
end;

function TDX2D.CreateVolatileTexture(w,h: nativeint; format: cardinal = D3DFMT_A8R8G8B8): IDirect3DTexture9;
var
  tex: IDirect3DTexture9;
  th: cardinal;
begin
  if not assigned(dx9window) then
    exit;

  if not assigned(dx9window.dev) then
    exit;

  //Self.dx9window.dev.CreateTexture(w,h, 1, D3DUSAGE_DYNAMIC, D3DFMT_X8R8G8B8, D3DPOOL_SYSTEMMEM, tex, @th);
  Self.dx9window.dev.CreateTexture(w,h,1,D3DUSAGE_DYNAMIC,format,{d3dpool_default} d3dpool_systemmem,tex,nil);

//  FTextures.Add(tex);
  result := tex;

end;

procedure TDX2D.CreateVolatileTexturePair(w, h: nativeint;
  out pair: TVolatileTexturePair; format: cardinal);
begin
  CreateVolatileTexturePair(w,h,pair.sys, pair.vid, format);
end;

function TDX2D.CreateVolatileTExtureTarget(w, h: nativeint; format: cardinal): IDirect3DTexture9;
begin
  //Self.dx9window.dev.CreateTexture(w,h, 1, D3DUSAGE_DYNAMIC, D3DFMT_X8R8G8B8, D3DPOOL_SYSTEMMEM, tex, @th);
  Self.dx9window.dev.CreateTexture(w,h,1,D3DUSAGE_DYNAMIC {or D3DUSAGE_AUTOGENMIPMAP},format,{d3dpool_default} d3dpool_default,result,nil);

end;

procedure TDX2D.CreateVolatileTexturePair(w, h: nativeint; out sys,
  vid: IDirect3DTexture9; format: cardinal = D3DFMT_A8R8G8B8; bRenderTarget: boolean = false);
var
  th: cardinal;
  usage: dword;
begin
  sys := nil;
  vid := nil;
  if not assigned(dx9window) then begin
    sleep(100);
    exit;
//    raise Exception.Create('dx9window not initialized yet');
  end;

  if not assigned(dx9window.dev) then begin
    sleep(100);
    exit;
//    raise Exception.Create('dx9dev not initialized yet');
  end;

  usage := D3DUSAGE_DYNAMIC;
  if bRenderTarget then
    usage := USAGE+D3dUSAGE_RENDERTARGET;

  //Self.dx9window.dev.CreateTexture(w,h, 1, D3DUSAGE_DYNAMIC, D3DFMT_X8R8G8B8, D3DPOOL_SYSTEMMEM, tex, @th);
  d3dassert(Self.dx9window.dev.CreateTexture(w,h,1,d3dusage_dynamic,format,D3DPOOL_SYSTEMMEM,sys,nil));
  if sys = nil then
    raise Exception.Create('failed on sys');

  d3dassert(Self.dx9window.dev.CreateTexture(w,h,1,d3dusage_dynamic,format,d3dpool_default,vid,nil));
  if vid = nil then
    raise exception.Create('failed on vid');



end;


procedure TDX2D.DelayReset;
begin
  tmResetTimer.Enabled := false;
  tmResetTimer.Interval := 50;
  tmResetTimer.Enabled := true;
end;

destructor TDX2D.Destroy;
begin


  FTextures.clear;
  dx9window.free;
  FTextureCache.free;
  ClearTextures;
  FTextures.free;
  FFonts.Clear;
  FFonts.Free;
  ClearControls;
  FDXcontrols.Free;
  inherited;
  DeleteCRiticalSection(sect);
end;

procedure TDX2D.DestroyDXResouces;
begin
  ClearTextures;
  FTextureCache.free;
  FTextureCache := nil;
  if dx9window <> nil then begin
    dx9window.REset;
    dx9window.dev := nil;
    dx9window.free;
    dx9window := nil;
  end;


end;

procedure TDX2D.DLGCode(var message: TMessage);
begin
  message.Result := Message.Result or DLGC_WANTCHARS or DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

procedure TDX2D.DoDraw;
begin
//  if dx9window = nil then exit;
//  if dx9window.dev = nil then exit;
  try
    Test;
  except
    DelayReset;
//    Reset;
  end;
end;

procedure TDX2D.DoMouseClick;
begin
  //dni
end;

procedure TDX2D.DoMouseDown;
begin
  //dni
end;

procedure TDX2D.DoMouseEnter;
begin
  //dni
end;

procedure TDX2D.DoMouseLeave;
begin
  //dni
end;

procedure TDX2D.DoMouseOut;
begin
  //dni
end;

procedure TDX2D.DoMouseover;
begin
  //dni
end;

procedure TDX2D.DoMouseRightClick;
begin
  //dni
end;

procedure TDX2D.DoMouseUp;
begin
  //dni
end;

function TDX2D.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  result := inherited;

end;

function TDX2D.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  result := inherited;
end;

procedure TDX2D.Draw(bNoFlip: boolean = false);
begin
  if GetTimeSince(lastFPSUpdateTime) > 1000 then begin
    fps := fpsframes;
    fpsframes := 0;
    lastFPSUpdateTime := getticker;
  end;

  inc(fpsframes);
  Initialize;
  if IsFunctional then begin


    if (dx9window= nil) or (dx9window.dev = nil) then begin
      binitialized := false;
      exit;
    end;

    BeforeDraw;

    Prepare;
    DoDraw;
    CommitBAtch;

    PushBounds;
    if not ControlsDrawn then
      DrawControls;
    if not bNoFlip then begin
      Flip;
    end;
    PopBounds;
    inc(FFrames);
    AfterDraw;
  end;

end;

procedure TDX2D.DrawControls;
var
  t: integer;
begin
  BoundX1 := 0;
  BoundX2 := surfacewidth;
  Boundy1 := 0;
  BoundY2 := surfaceheight;
  for t:= 0 to FDXChildren.Count-1 do begin
    if FDxChildren[t].Visible then begin
      FDxChildren[t].Draw;
      FDxChildren[t].DrawChildren;
    end;
  end;
  for t:= 0 to FDXChildren.Count-1 do begin
    if FDxChildren[t].Visible then begin
      FDxChildren[t].DrawTop;
      FDxChildren[t].DrawTopChildren;
    end;
  end;

  ControlsDrawn := true;
end;

procedure TDX2D.DrawLine(x, y: single; alpha_color: cardinal);
var
  v: TD3DXVector2;
begin
  if alpha_color <> LineColor then begin
    linecolor := alpha_color; //MAY TRIGGER FLUSHLINE!!!
  end;

  v.x := GlobalToScreenX(x);
  v.y := GlobalToScreenY(Y);
  linecache_verts[linecache_idx] := v;
  inc(linecache_idx);
  if linecache_idx > High(linecache_verts) then
    FlushLine;





end;

procedure TDX2D.EndScene;
begin
  if not FInScene then exit;
  dx9window.dev.EndScene;
  FInScene := false;
end;

procedure TDX2D.EndVertexBatch;
begin
  CommitVertexBatch;
  FBatchIndex := -1;
end;

procedure TDX2D.Flip;
var
  hr: HRESULT;
begin
//  Lock;
  try
    if (BatchMode) then
      EndVertexbatch;

    if FInScene then
       endscene;

    if not IsFunctional then begin
      Reset;
      exit;
    end;

    hr := dx9window.dev.Present(nil, nil, 0, nil);
    if (hr <> 0) then begin
      if hr = d3derr_devicelost then begin
        debug.log(self,'Device Lost '+inttostr(hr));
        Reset;
      end
      else begin
        debug.log(self,'Unknown HRESULT:'+inttostr(hr));
        Reset;
      end;

    end;
  finally
//    Unlock;
  end;
//  Dirty := true;
end;

procedure TDX2D.FlushLine;
var
  idx: ni;
begin
  if linecache <> nil then begin
    idx := self.linecache_idx;
    if idx > 0 then begin
      linecache.Draw(@Self.linecache_verts[0], idx, linecolor);
      linecache_verts[0] := linecache_verts[idx-1];
    end;
  end;
  linecache_idx := 0;

end;

function TDX2D.FormToGlobal(formP: TnativefloatPoint): TNativeFLoatPOint;
begin
  result.x := formtoglobalx(formp.x);
  result.y := formtoglobaly(formp.y);
end;

function TDX2D.FormToGlobalX(formX: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := formX
  else
    result := Interpolate(formX, BoundX1, BoundX2, LeftMargin,
      width - (RightMargin + LeftMargin));

end;

function TDX2D.FormToGlobalY(formY: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := formY
  else
    result := Interpolate(formY, BoundY1, BoundY2, TopMargin,
      Height - (BottomMargin + TopMargin));

end;

function TDX2D.GetBottomMargin: TDXFloat;
begin
  result := FBottomMargin;
end;

function TDX2D.GetBottomMarginF: TDXFloat;
begin
  result := BottomMargin / Surfaceheight;
end;

function TDX2D.GetCurrentFont: TFontInfo;
begin
  result := FFonts[font_index];
end;

function TDX2D.GetDimensionX: TDXFloat;
begin
  result := BoundX2 - BoundX1;
end;

function TDX2D.GetDimensiONY: TDXFloat;
begin
  result := BoundY2 - BoundY1;
end;

function GetFullPathToAsset(sFile: string): string;
var
  sTest: string;
begin
  result := dllpath+sfile;
  if fileexists(result) then
    exit;

  result := dllpath+'..\'+sfile;
  if fileexists(result) then
    exit;


  result := dllpath+'..\..\'+sfile;
  if fileexists(result) then
    exit;


  result := dllpath+'..\art\'+sfile;
  if fileexists(result) then
    exit;

  result := dllpath+'..\..\art\'+sfile;
  if fileexists(result) then
    exit;



end;

function TDX2D.GetLeftMargin: TDXFloat;
begin
  result := FLeftMargin;
end;

function TDX2D.GetLeftMarginF: TDXFloat;
begin
  result := LeftMargin / Surfacewidth;
end;

function TDX2D.GetrightMargin: TDXFloat;
begin
  result := FrightMargin;
end;

function TDX2D.GetrightMarginF: TDXFloat;
begin
  result := RightMargin / Surfacewidth;
end;

function TDX2D.GetTopMargin: TDXFloat;
begin
  result := FTopMargin;
end;

function TDX2D.GetTopMarginF: TDXFloat;
begin
  result := TopMargin / Surfaceheight;
end;

function TDX2D.GLobalToForm(formP: TnativefloatPoint): TNativeFLoatPOint;
begin
  result.x := GlobalToFormX(formp.x);
  result.y := globaltoformy(formp.y);
end;

function TDX2D.GlobalToFormX(globalX: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := globalX
  else
    result := Interpolate(globalX, (LeftMargin), ((Width - RightMargin)),
      BoundX1, BoundX2);

end;

function TDX2D.GlobalToFormY(globalY: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := globalY
  else
    result := Interpolate(globalY, (TopMargin), ((Height - BottomMargin)),
      BoundY1, BoundY2);

end;

function TDX2D.GlobalToScreenX(globalX: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := globalX
  else
    result := Interpolate(globalX, (LeftMargin), ((FSurfaceWidth -  RightMargin)),
      BoundX1, BoundX2);

end;

function TDX2D.GlobalToScreenY(globalY: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := globalY
  else
    result := Interpolate(globalY, (TopMargin), ((FSurfaceHeight - BottomMargin)),
      BoundY1, BoundY2);

end;

procedure TDX2D.Init;
begin
  inherited;
  //
end;

procedure TDX2D.InitControls;
begin
  //no implementation required
end;

procedure TDX2D.Initialize;
begin
  if not showing then
    exit;


  if (not bInitialized) or (dx9window.DisplayHandle <> self.handle) or (not (IsFunctional)) then begin
    dx9window.parent := self;
    dx9window.DisplayHandle := self.handle;
//    dx9window.SurfaceWidth := width;
//    dx9window.surfaceheight := height;

    dx9window.Initialize;
    if IsFunctional then begin
      d3dassert(dx9window.dev.SetFVF(dx9_tools.ABSTRACT_FVF));
      d3dassert(dx9window.dev.SetRenderState(D3DRS_ZENABLE, 0));
      EnableScissor(true);
    end;

    bInitialized := true;
    if IsFunctional then begin
      LoadTextures;
    end;
  end;


end;

function TDX2D.IsFunctional: boolean;
begin
  result := Assigned(dx9window) and assigned(dx9window.dev);
end;

procedure TDX2D.LoadFont(sfile: string; rOverdrawX: single = 1.0; rOverDrawY: single = 1.0);
var
  ftex: INativeTexture;
  fi : TFontInfo;
  p: TNameValuePairList;
begin
  if not IsFunctional then exit;
  dx9window.dev.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_PYRAMIDALQUAD);
  dx9window.dev.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_PYRAMIDALQUAD);
  dx9window.dev.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_PYRAMIDALQUAD);

  p := TNameValuePairList.create;
  try
    d3dassert(FTextureCache.CreateTextureFromFile(dx9window.dev, pchar(sFile),Ftex));
    //  d3dassert(FTextureCache.CreateTextureFromFileEx(dx9window.dev, pchar(sFile), cardinal(-1), cardinal(-1), cardinal(-1), 0, D3DFMT_UNKNOWN, D3DPOOL_MANAGED, 1, 1, $FFFF00FF, nil, nil, &ftex));

    fi.cellsacross := p.GetItemEx('cellsacross', 16);
    fi.cellXOffset := p.GetItemEx('cellXOffset', 0);
    fi.cellYOffset := p.GetItemEx('cellXOffset', 0);
//    fi.charWidth := p.GetItemEx('charwidth',

    fi.overdrawX := rOverdrawX;
    fi.overdrawY := rOverdrawY;
    fi.tex := ftex;
    FFonts.add(fi);



  finally
    p.free;
  end;

end;

procedure TDX2D.LoadTexture(sfile: string);
var
  ftex: INativeTexture;
begin
  if not fileexists(sFile) then
    halt(0);

  if not isFunctional then
    exit;
  dx9window.dev.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
  dx9window.dev.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
  dx9window.dev.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR);


  d3dassert(FTextureCache.CreateTextureFromFile(dx9window.dev, pchar(sFile),Ftex));
  //  d3dassert(FTextureCache.CreateTextureFromFileEx(dx9window.dev, pchar(sFile), cardinal(-1), cardinal(-1), cardinal(-1), 0, D3DFMT_UNKNOWN, D3DPOOL_MANAGED, 1, 1, $FFFF00FF, nil, nil, &ftex));


  FTextures.add(ftex);



end;

procedure TDX2D.LoadTextures;
begin
  ClearTextures;
  TexturesLoaded := IsFunctional;
  TexturesInitiallyLoaded := IsFunctional;
  //
end;

procedure TDX2D.Lock;
begin
  EnterCRiticalSection(sect);
end;

procedure TDX2D.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  xx,yy: single;
begin
  inherited;
  xx := FormToGlobalX(x);
  yy := FormToGlobalY(y);
  fLastMouseXX := xx;
  FLastMouseYY := yy;
  fLastMouseX := x;
  FLastMouseY := y;

  case button of
    mbLeft: mouse_buttons_down[0] := true;
    mbRight: mouse_buttons_down[1] := true;
    mbMiddle: mouse_buttons_down[2] := true;
  end;
  if not BroadcastMouseEvent(dxmeDown, shift, x,y,xx,yy) then
    DoMouseDown;

end;

procedure TDX2D.MouseMove(Shift: TShiftState; X, Y: integer);
var
  xx,yy: single;
begin
  inherited;
  mouse_last_pos_for_wheel.x := x;
  mouse_last_pos_for_wheel.Y := y;
  xx := FormToGlobalX(x);
  yy := FormToGlobalY(y);
  fLastMouseXX := xx;
  FLastMouseYY := yy;
  fLastMouseX := x;
  FLastMouseY := y;

  if not BroadcastMouseEvent(dxmeMove, shift, x,y,xx,yy) then
    DoMouseOver;

end;

procedure TDX2D.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  xx,yy: nativefloat;
begin
  inherited;

  xx := FormToGlobalX(x);
  yy := FormToGlobalY(y);
  fLastMouseXX := xx;
  FLastMouseYY := yy;
  fLastMouseX := x;
  FLastMouseY := y;

  case button of
    mbLeft: mouse_buttons_down[0] := false;
    mbRight: mouse_buttons_down[1] := false;
    mbMiddle: mouse_buttons_down[2] := false;
  end;


  if not BroadcastMouseEvent(dxmeUp, shift, x,y,xx,yy) then
    DoMouseUp;
  //when mouse up is handled ANYWHERE, we need to make sure it only goes to the
  //control that had the mouse down event

  CancelMouse;//THIS TELLS *ALL CONTROLS* to forget their pending mouse gestures







end;

procedure TDX2D.NeedBatchSpace(iQty: integer);
begin
  if (VERTEX_BATCH_SIZE - FBatchIndex) < iQty then begin
    EndVertexBatch;
    BeginVertexBatch();
  end;
end;

procedure TDX2D.NextBatchVertex;
begin
  inc(FBatchIndex);
end;

procedure TDX2D.OnReset;
begin
  ClearTextures;
  DestroyDXResouces;
  bInitialized := false;
  FInScene := false;
  CreateDXResources;
  LoadTextures;


end;

procedure TDX2D.Paint;
begin
   //inherited;
//  Draw;
end;

procedure TDX2D.PendingResetTimer(sender: TObject);
begin
  tmResetTimer.enabled := false;
  Reset;
end;

procedure TDX2D.PopBounds;
var
  b: PBounds;
begin
  dec(FBoundStackPointer);
  if FBoundStackPOinter < 0 then
    raise Exception.Create('Bound stack < 0!');

  b := @FBoundStack[FBoundStackPointer];

  FBoundx1 := b.x1;
  FBoundy1 := b.y1;
  FBoundx2 := b.x2;
  FBoundy2 := b.y2;
  FLeftMargin := b.mx1;
  FrightMargin := b.mx2;
  FTopMargin := b.my1;
  FBottomMargin := b.my2;



end;

procedure TDX2D.Prepare;
Type
  simple_point = dx9_tools.TNativeVertex;
var
  matView, matWorld, matProj: dx9_tools.TNativeMatrix;

begin
  ControlsDrawn := false;

  if not assigned(dx9window.dev) then
    exit;

  BeginScene;

  FilLChar(matProj, SizeOf(matProj), 0);

  d3dutil_setidentitymatrix(matProj);
  d3dutil_setidentitymatrix(matWorld);
  d3dutil_setidentitymatrix(matView);

  d3dassert(dx9window.dev.SetTransform(D3DTS_WORLD, matWorld));

  d3dassert(dx9window.dev.SetRenderState(D3DRS_LIGHTING, 0));
  // d3dutil_setviewmatrix(matView, MakeD3dVector(0,0,0), Maked3dvector(0,0,-50), Maked3dvector(0,-1,0));

  d3dassert(dx9window.dev.SetTransform(D3DTS_VIEW, matView));

  d3dutil_setprojectionmatrix(matProj, (45 / 360) * (2 * pi),
    FSurfaceWidth / FsurfaceHeight, 0.001, 1000);
  d3dutil_SetOrthographicProjectionMatrix(matProj, FSurfaceWidth, FsurfaceHeight,
    0.0);
// d3dutil_SetTranslationMatrix(matProj, 0-(clientwidth/2), 0-(clientheight/2), 0);

  d3dassert(dx9window.dev.SetTransform(D3DTS_PROJECTION, matProj));

  d3dassert(dx9window.dev.SetRenderState(D3DRS_AMBIENT, $7F7F7F));
  d3dassert(dx9window.dev.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE));


  dx9window.dev.Clear(0, nil, D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER,
    D3DCOLOR_XRGB(0, 0, 0), 1.0, 0);
end;

procedure TDX2D.PushBounds;
var
  b: PBounds;
begin
  b := @FBoundStack[FBoundStackPointer];

  b.x1 := Fboundx1;
  b.y1 := Fboundy1;
  b.x2 := Fboundx2;
  b.y2 := Fboundy2;
  b.mx1 := FLeftMargin;
  b.mx2 := FrightMargin;
  b.my1 := FTopMargin;
  b.my2 := FBottomMargin;


  inc(FBoundstackPointer);

end;

procedure TDX2D.PushIdentityBounds;
begin
  PushBounds;
  BoundsAtIdentity;

end;

procedure TDX2D.Quad(x1, y1, x2, y2, x3, y3, x4, y4: single; clr1, clr2, clr3,
  clr4: TColor; alpha: TDXFloat);
begin
  x1 := GlobalToScreenX(x1);
  y1 := GlobalToScreenY(y1);
  x2 := GlobalToScreenX(x2);
  y2 := GlobalToScreenY(y2);
  x3 := GlobalToScreenX(x3);
  y3 := GlobalToScreenY(y3);
  x4 := GlobalToScreenX(x4);
  y4 := GlobalToScreenY(y4);


  if Batchmode then
    canvas_batch_Quad(x1,y1, x2,y2, x3,y3, x4,y4, clr1,clr2,clr3,clr4, alpha)
  else
    canvas_Quad(x1,y1, x2,y2, x3,y3, x4,y4,  clr1,clr2,clr3,clr4, alpha);
end;

procedure TDX2D.RecalcScale;
var
  iDiv: TDXFloat;
  OldScale: TDXFloat;
begin
// if DimensionX < Width then
// DimensionX := Width;

// if DimensionY < Height then
// DimensionY := height;
  OldScale := FScale;

  iDiv := FBoundX2 - FBoundX1;
  if iDiv = 0 then begin
    FScale := 1;

  end
  else begin

    if width <> 0 then
      FScale := (FBoundX2 - FBoundX1) / width;

    if ConstrainProportions then begin
      FBoundY2 := trunc((height * (FScale))) + FBoundY1;


    end;
  end;

// if OldScale<>FScale then
// Invalidate;

end;

procedure TDX2D.Rectangle(x1, y1, x2, y2: TDXFloat; color: TColor;
  Fill, bGradient: boolean; bgcolor: TColor);
var
  c2: TColor;
begin
  x1 := GlobalToScreenX(x1);
  y1 := GlobalToScreenY(y1);
  x2 := GlobalToScreenX(x2);
  y2 := GlobalToScreenY(y2);

  if bGradient then begin
    c2 := bgcolor;
  end
  else begin
    c2 := color;
  end;

  canvas_Rectangle_Fill(x1, y1, x2, y2, c2, c2, color, color, 1);
end;

procedure TDX2D.Rectangle_Fill(x1, y1, x2, y2: TDXFloat; c1, c2, c3, c4: TColor;
  alpha: TDXFloat);
begin
  x1 := GlobalToScreenX(x1);
  y1 := GlobalToScreenY(y1);
  x2 := GlobalToScreenX(x2);
  y2 := GlobalToScreenY(y2);

  if Batchmode then
    canvas_batch_Rectangle_Fill(x1, y1, x2, y2, c1,c2,c3,c4, alpha)
  else
    canvas_Rectangle_Fill(x1, y1, x2, y2, c1,c2,c3,c4, alpha);

end;

procedure TDX2D.RemoveControl(C: TDXControl);
begin
//  Debug.ConsoleLog('Removing '+c.classname+' there are '+inttostr(FDXControls.count)+' controls.');
  FDxControls.Remove(c);
//  Debug.ConsoleLog('Removed '+c.classname+' there are now '+inttostr(FDXControls.count)+' controls.');
end;

procedure TDX2D.RemoveDXChild(C: TDXControl);
begin
//  Debug.ConsoleLog('Removing Child '+c.classname+' of '+self.classname+' there are '+inttostr(FDXControls.count)+' Child controls.');
  FDXChildren.remove(c);
//  Debug.ConsoleLog('Removed Child '+c.classname+' of '+self.classname+' there are '+inttostr(FDXControls.count)+' Child controls.');
end;

procedure TDX2D.Reset;
begin
  if not TexturesInitiallyLoaded then
    exit;

  DestroyDXResouces;

  CReateDXResources;
//  if IsFunctional then begin



//    dx9window.SurfaceWidth := width;
//    dx9window.SurfaceHeight := height;
//    dx9window.REset ;
//    OnREset;
//  end else
   OnReset;




end;

procedure TDX2D.ResetMargins;
begin
  TopMargin := 0;
  LeftMargin := 0;
  RightMargin := 0;
  BottomMargin := 0;
end;

procedure TDX2D.ResetSurfaceResolution(x, y: nativeint);
begin
  FSurfaceWidth := x;
  FSurfaceHeight := y;
  dx9window.surfacewidth := x;
  dx9window.surfaceheight := y;
end;

procedure TDX2D.ResetText;
begin
  CharWidth := 16;
  CharHeight := 16;
  Color := clSilver;
  FLastTextFlags := [tfStroke, tfBold];
  TextPosition.x := 0;
  TextPosition.y := 0;
  TextOffset := D3DXVector3(0,0,0);

end;

procedure TDX2D.REsize;
begin
  inherited;
{$IFDEF USE_ACTUAL_SURFACE_WIDTH}
  DelayReset;
{$ENDIF}
end;

procedure TDX2D.Rectangle_Fill(x1, y1, x2, y2: TDXFloat; clr: TColor;
  alpha: TDXFloat = 1.0);
begin
  Rectangle_Fill(x1,y1,x2,y2,clr,clr,clr,clr,alpha);

end;

function TDX2D.ScaleGlobalXtoForm(Xdistance: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := (Xdistance)
  else
    result := Interpolate(Xdistance, 0, (width) - ((RightMargin + LeftMargin)),
      0, DimensionX);

  if result < 0 then
    result := 0 - result;

end;

function TDX2D.ScaleGlobalXtoScreen(Xdistance: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := (Xdistance)
  else
    result := Interpolate(Xdistance, 0, (FSurfaceWidth) - ((RightMargin + LeftMargin)),
      0, DimensionX);

  if result < 0 then
    result := 0 - result;

end;

function TDX2D.ScaleGlobalYtoForm(Ydistance: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := (Ydistance)
  else
    result := Interpolate(Ydistance, 0,
      (height) - ((BottomMargin + TopMargin)), 0, DimensionY);

end;

function TDX2D.ScaleGlobalYtoScreen(Ydistance: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := (Ydistance)
  else
    result := Interpolate(Ydistance, 0,
      (FSurfaceHeight) - ((BottomMargin + TopMargin)), 0, DimensionY);
end;

function TDX2D.ScaleScreenToCanvas(x: TDXFloat): integer;
begin

  raise Exception.Create('unimplemented');
// TODO -cunimplemented: unimplemented block
end;

function TDX2D.ScaleScreenXtoGlobal(Xdistance: TDXFloat; bKeepSign: boolean): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := Xdistance
  else
    result := Interpolate(Xdistance, 0, DimensionX, 0,
      (FSurfaceWidth - (RightMargin + LeftMargin)));

  if not bKeepSign then
    if result < 0 then
      result := 0 - result;

end;

function TDX2D.ScaleScreenYtoGlobal(Ydistance: TDXFloat; bKeepSign: boolean): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := (Ydistance)
  else
    result := Interpolate(Ydistance, 0, DimensionY, 0,
      (FSurfaceHeight) - (BottomMargin + TopMargin));

  if not bKeepSign then
    if result < 0 then
      result := 0 - result;

end;

procedure TDX2D.ScissorControl(c: TDXControl);
var
  r: TRect;
  pr: PRect;
begin
  r.Left := trunc(c.ScreenBound.x1);
  r.Right := round(c.ScreenBound.x2);
  r.Top := trunc(c.ScreenBound.y1);
  r.Bottom := round(c.ScreenBound.y2);
  pr := @r;
  dx9window.dev.SetScissorRect(pr);
  EnableScissor(true);

end;

function TDX2D.ScreenToGlobalX(screenX: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := screenX
  else
    result := Interpolate(screenX, BoundX1, BoundX2, LeftMargin,
      FSurfaceWidth - (RightMargin + LeftMargin));

end;

function TDX2D.ScreenToGlobalY(screenY: TDXFloat): TDXFloat;
begin
  if FUseScreenCoordinates then
    result := screenY
  else
    result := Interpolate(screenY, BoundY1, BoundY2, TopMargin,
      FSurfaceHeight - (BottomMargin + TopMargin));

end;

procedure TDX2D.SetAlphaOp(const Value: TAlphaOp);
begin
  if FAlphaOp <> value then
    CommitBatch;
  FAlphaOp := value;
  if AlphaOp = aoNone then
    dx9window.dev.SetRenderState(D3DRS_ALPHABLENDENABLE, 0)
  else begin
    dx9window.dev.SetRenderState(D3DRS_ALPHABLENDENABLE, 1);
    if AlphaOp = aoAdd then begin
      dx9window.dev.SetRenderState(D3DRS_SRCBLEND, integer(D3DBLEND_SRCALPHA));
      dx9window.dev.SetRenderState(D3DRS_DESTBLEND, integer(D3dblend_one));

    end
    //Standard Alpha
    else begin
//      dx9window.dev.SetRenderState(d3drs_al, integer(D3DBLEND_SRCALPHA));
      dx9window.dev.SetRenderState(D3DRS_SRCBLEND, integer(D3DBLEND_SRCALPHA));
      dx9window.dev.SetRenderState(D3DRS_DESTBLEND, integer(D3DBLEND_INVSRCALPHA));
    end;

  end;
end;

procedure TDX2D.SetBottomMargin(const Value: TDXFloat);
begin
  FBottomMargin := Value;
end;

procedure TDX2D.SetBottomMarginF(const Value: TDXFloat);
begin
  BottomMargin := round(value * surfaceheight);
end;

procedure TDX2D.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
//  Resize;
end;

procedure TDX2D.SetBoundX1(const Value: TDXFloat);
begin
  FBoundX1 := Value;
  FTargetBoundX1 := FBoundX1;
  ClearBoundsRect;
  RecalcScale;

end;

procedure TDX2D.SetBoundX2(const Value: TDXFloat);
begin
  FBoundX2 := Value;
  FTargetBoundX2 := FBoundX2;
  ClearBoundsRect;
  RecalcScale;

end;

procedure TDX2D.SetBoundY1(const Value: TDXFloat);
begin
  FBoundY1 := Value;
  FTargetBoundY1 := FBoundY1;
  ClearBoundsRect;
  RecalcScale;

end;

procedure TDX2D.SetBoundY2(const Value: TDXFloat);
begin
  FBoundY2 := Value;
  FTargetBoundY2 := FBoundY2;
  ClearBoundsRect;
  RecalcScale;

end;

procedure TDX2D.SetConstrainProportions(const Value: boolean);
begin
  FConstrainProportions := Value;
  RecalcScale;
end;

procedure TDX2D.SetDimensionX(const Value: TDXFloat);
begin
  BoundX2 := BoundX1 + Value;

end;

procedure TDX2D.SetDimensionY(const Value: TDXFloat);
begin
  BoundY2 := BoundY1 + Value;
end;

procedure TDX2D.SetFont(iSlot: integer);
begin
  CommitBatch;

  if iSlot < 0 then begin
    dx9window.dev.SetTexture(0, nil);
    FLoadedNativeTExture := nil;
  end else begin
    dx9window.dev.SetTexture(0, FFonts[iSlot].tex);
    if FFonts[iSlot].tex <> FLoadedNativeTexture then begin
      self.CommitVertexBatch;
      FLoadedNativeTExture := FFonts[iSlot].tex;
    end;
  end;

  font_index := iSlot;


end;

procedure TDX2D.SetIdentityBounds;
begin
  BoundsAtIdentity;
end;

procedure TDX2D.SetLeftMargin(const Value: TDXFloat);
begin
  FLeftMargin := Value;
end;

procedure TDX2D.SetLeftMarginF(const Value: TDXFloat);
begin
  LeftMargin := round(value * surfacewidth);
end;

procedure TDX2D.SetLineColor(const Value: cardinal);
begin
  if linecache <> nil then
    FlushLine;

  FLineColor := Value;
end;

procedure TDX2D.SetParent(AParent: TWinControl);
begin
  inherited;
  bInitialized := false;
end;

procedure TDX2D.SetrightMargin(const Value: TDXFloat);
begin
  FrightMargin := Value;
end;

procedure TDX2D.SetrightMarginF(const Value: TDXFloat);
begin
  RightMargin := round(value * surfacewidth);
end;

procedure TDX2D.SetScale(const Value: TDXFloat);
begin
  FScale := Value;
end;

procedure TDX2D.SetTargetBoundX1(const Value: TDXFloat);
begin
  FTargetBoundX1 := Value;

end;

procedure TDX2D.SetTargetBoundX2(const Value: TDXFloat);
begin
  FTargetBoundX2 := Value;
end;

procedure TDX2D.SetTargetBoundY1(const Value: TDXFloat);
begin
  FTargetBoundY1 := Value;
end;

procedure TDX2D.SetTargetBoundY2(const Value: TDXFloat);
begin
  FTargetBoundY2 := Value;
end;

procedure TDX2D.SetTextPosition(x, y: nativeint);
begin
  TextPosition.X := x;
  TextPosition.Y := y;

end;

procedure TDX2D.SetTexture(tex: IDirect3dTexture9);
var
  hr: HRESULT;
begin

  if not IsFunctional then
    exit;

  CommitBatch;

  FLoadedNativeTexture := tex;
  hr := dx9window.dev.SetTexture(0, tex);
  if not succeeded(hr) then
    Debug.log(self,'Set Texture did not succeeed');
end;

procedure TDX2D.SetTexture(iSlot: integer);
begin

  CommitBatch;

  if iSlot < 0 then begin
    dx9window.dev.SetTexture(0, nil);
    FLoadedNativeTexture := nil;
  end else begin
    dx9window.dev.SetTexture(0, FTextures[iSlot]);
    if FTextures[iSlot] <> FLoadedNativeTexture then begin
      self.CommitVertexBatch;
      FLoadedNativeTExture := FTextures[iSlot];
    end;

  end;

end;

procedure TDX2D.SetTopMargin(const Value: TDXFloat);
begin
  FTopMargin := Value;
end;

procedure TDX2D.SetTopMarginF(const Value: TDXFloat);
begin
  TopMargin := round(value * surfaceheight);
end;

procedure TDX2D.SetTypicalTextAlpha;
begin
  AlphaOp := aoStandard;
end;

procedure TDX2D.Sprite(xCenter, yCenter: single; c: TColor; alpha, xSize,
  ySize: single);
begin
  Sprite(xCenter,yCenter,c,c,c,c,alpha, xSize, ysize);
end;

procedure TDX2D.Sprite(xCenter, yCenter: single; c1,c2,c3,c4: TColor; alpha: single; xSize,
  ySize: single);
var
  x1,y1,x2,y2,x3,y3,x4,y4: single;
  tmp: single;
begin
  if xSize < 0 then begin
    xSize := ScaleScreenXtoGlobal(ScaleGlobalYToScreen(ySize));
  end else
  if ysize < 0 then begin
    tmp := ScaleGlobalXToScreen(xSize);
    ySize := ScaleScreenYtoGlobal(tmp);
  end;


  x1 := 0 - xSize;
  x2 := 0 + xSize;
  x3 := 0 - xSize;
  x4 := 0 + xSize;
  y1 := 0 - ySize;
  y2 := 0 - ySize;
  y3 := 0 + ySize;
  y4 := 0 + ySize;

  x1 := x1 + xCenter;
  x2 := x2 + xCenter;
  x3 := x3 + xCenter;
  x4 := x4 + xCenter;
  y1 := y1 + yCenter;
  y2 := y2 + yCenter;
  y3 := y3 + yCenter;
  y4 := y4 + yCenter;

  Quad(x1,y1,x2,y2,x3,y3,x4,y4, c1,c2,c3,c4, alpha);


end;

procedure TDX2D.Test;
begin
  SetTexture(-1);
  Rectangle_Fill(BoundX1, BoundY1, BoundX2 / 2, BoundY2 / 2, clRed, 0.5);
  canvas_Rectangle_Fill(0, 0, 80, 60, clLime, clLime, clLime, clLime, 0.5);

end;


procedure TDX2D.TextOut(sText: ansistring; x1, y1: TDXFloat; rFlags: TTFLags);
begin
  x1 := GlobaltoScreenX(x1);
  y1 := GlobalToScreenY(y1);
  canvas_Text(sText, x1,y1, rFlags);
end;

procedure TDX2D.Unloadfont(iSlot: integer);
begin
  FFonts.delete(iSlot);
end;

procedure TDX2D.Unloadtexture(iSlot: integer);
begin
  FTextures.delete(iSlot);

end;

procedure TDX2D.Unlock;
begin
  LeaveCRiticalSection(sect);
end;

procedure TDX2D.UpdatePhysics;
begin
  FFrameTime := GetTicker;
  while (FFrameTime > (physicsupdatedTo+PHYSICS_INCREMENT)) do begin
    UpdatePhysics(PHYSICS_INCREMENT);
    inc(physicsUpdatedTo, PHYSICS_INCREMENT);
  end;
  FLastUpdateTime := FFrameTime;
  ApplyInterpolatedBoundsChanges;
end;

procedure TDX2D.UpdatePhysics(rDeltaTime: TDXTime);
var
  t: ni;
  c: TDXControl;
begin
  if not visible  then
    exit;
  //no implementation required
  for t := 0 to Self.FDXChildren.count-1 do begin
    c:= self.fdxchildren[t];
    c.HandlePhysics(rDeltaTime);
  end;

end;

procedure TDX2D.UpdateVolatileSurface(texsys, texvid: IDirect3dSurface9);
begin
  if texsys = nil then
    exit;

  if texvid = nil then
    exit;

  d3dassert(self.dx9window.dev.UpdateSurface(texsys, nil, texvid, nil));
end;

procedure TDX2D.CopyTexture_SysMem(src,dest: IDirect3dTexture9);
type
  TDWORDArray = array[0..0] of DWORD;
  PDWordArray = ^TDWordArray;

var
  psrc,pdest: pbyte;
  pwsrc,pwdest: PDWordArray;
  plocked_src,plocked_dest: _D3DLOCKED_RECT;
  r: TRect;
  t, u: integer;
  surf: IDirect3dsurface9;
  desc: _D3DSURFACE_DESC;
begin
  src.GetSurfaceLevel(0,surf);
  surf.GetDesc(desc);
  r.Top := 0;
  r.Left := 0;
  r.Right := desc.Width-1;
  r.Bottom := desc.height-1;


  plocked_src.Pitch := 0;
  plocked_src.pBits := nil;
  plocked_dest.Pitch := 0;
  plocked_dest.pBits := nil;
  if assigned(dest) then begin
    if succeeded(src.LockRect(0, plocked_src, nil, D3DLOCK_READONLY)) then
    try
      if succeeded(dest.LockRect(0, plocked_dest, nil, D3DLOCK_DISCARD {or D3DLOCK_NO_DIRTY_UPDATE or D3DLOCK_NOSYSLOCK}
          {or D3DLOCK_NO_DIRTY_UPDATE} { or D3DLOCK_NOOVERWRITE } {or D3DLOCK_NOSYSLOCK)})) then
      try
        psrc := plocked_src.pBits;
        pdest := plocked_dest.pBits;
        if assigned(psrc) and assigned(pdest) then begin
          for u := 0 to r.Bottom do begin
            pwsrc := pdwordarray(@psrc[plocked_src.Pitch*u]);
            pwdest := pdwordarray(@pdest[plocked_dest.Pitch*u]);
            for t := 0 to ((r.Right)) do begin
              try
                pwdest[t] := pwsrc[t];
              except
              end;
            end;
          end;
        end;

      finally
        dest.UnlockRect(0);
      end;

    finally
      src.UnlockREct(0);
    end;
  end;
end;

procedure TDX2D.UpdateVolatileTexture(texsys, texvid: IDirect3dTexture9);
begin
  if texsys = nil then
    exit;

  if texvid = nil then
    exit;

  d3dassert(self.dx9window.dev.UpdateTexture(texsys, texvid));
end;

procedure TDX2D.AddControl(C: TDXControl);
begin
  if FDXControls.IndexOf(c) >=0 then
    exit;
  FDXControls.add(c);
  c.DX := self;
  c.Assimilate;

end;

procedure TDX2D.AddDXchild(C: TDXControl);
begin
  FDXChildren.add(c);
end;

procedure TDX2D.AfterDraw;
begin
  //
end;

procedure TDX2D.ApplyInterpolatedBoundsChanges;
const
  r1 = 0.1;
  r2 = 1.0-r1;
begin
  if abs(FTargetBoundx1 - FBoundX1) > ScaleScreenXtoGlobal(BOUND_SNAP) then begin
    FBoundX1 := (FTargetBoundX1 * r1) + (FBoundX1 * r2);
  end else begin
    FBoundX1 := FTargetBoundX1;
  end;

  if abs(FTargetBoundx2 - FBoundX2) > ScaleScreenXtoGlobal(BOUND_SNAP) then begin
    FBoundX2 := (FTargetBoundX2 * r1) + (FBoundX2 * r2);
  end else begin
    FBoundX2 := FTargetBoundX2;
  end;

  if abs(FTargetBoundy1 - FBoundy1) > ScaleScreenYtoGlobal(BOUND_SNAP) then begin
    FBoundy1 := (FTargetBoundy1 * r1) + (FBoundy1 * r2);
  end else begin
    FBoundy1 := FTargetBoundy1;
  end;

  if abs(FTargetBoundy2 - FBoundy1) > ScaleScreenYtoGlobal(BOUND_SNAP) then begin
    FBoundy2 := (FTargetBoundy2 * r1) + (FBoundy2 * r2);
  end else begin
    FBoundy2 := FTargetBoundy2;
  end;

  ClearBoundsRect;
  RecalcScale;


end;

function TDX2D.BatchMode: boolean;
begin
  result := FBatchIndex >= 0;
end;

procedure TDX2D.BeforeDraw;
begin
  //
end;

procedure TDX2D.BeginScene;
begin
  FLoadedNativeTexture := nil;
  if FInScene then exit;
  dx9window.dev.BeginScene;
  FInScene := true;

end;

procedure TDX2D.BeginVertexBatch(mode: Td3dprimitivetype = d3dpt_trianglelist);
begin
  if FBatchIndex >= 0 then
    EndVertexBatch;
  FBatchIndex := 0;
  if mode <> D3DPT_INVALID_0 then
    FBatchType := mode;
end;

procedure TDX2D.BoundsAtIdentity;
begin
  BoundX1 := 0;
  BoundX2 := FSurfaceWidth-1;
  BoundY1 := 0;
  BoundY2 := FSurfaceHeight-1;
end;

function TDX2D.BroadcastMouseEvent(dxme: TDXMouseEvent; shift: TShiftState; x_screen, y_screen,x,y: single): boolean;
var
  t: nativeint;
begin
  inherited;
  result := false;
//  Debug.consolelog('mouse :'+floattostr(x_screen)+','+floattostr(y_screen)+'='+floattostr(x)+','+floattostr(y));
//  Debug.ConsoleLog('Start broadcast.');
  for t := 0 to FDXControls.Count-1 do begin
    result := FDXControls[t].MouseEvent(dxme, x_screen,y_screen,x,y,shift);
    if result then
      break;

  end;
//  Debug.ConsoleLog('Done with broadcast.');

end;

procedure TDX2D.CancelMouse;
var
  i: ni;
begin
  mouse_buttons_down[0] := false;
  mouse_buttons_down[1] := false;
  mouse_buttons_down[2] := false;
  for i := 0 to FDXcontrols.count-1 do begin
    FDXControls[i].MouseCancel;
  end;

end;

procedure TDX2D.canvas_batch_Quad(x1, y1, x2, y2, x3, y3, x4, y4: single; clr1,
  clr2, clr3, clr4: TColor; alpha: TDXFloat);
Type
  simple_point = dx9_tools.TNativeVertex;
var
  matView, matWorld, matProj: dx9_tools.TNativeMatrix;
begin

  clr1 := ColorReverse(clr1);
  clr1 := ColorAlpha(clr1, alpha);
  clr2 := ColorReverse(clr2);
  clr2 := ColorAlpha(clr2, alpha);
  clr3 := ColorReverse(clr3);
  clr3 := ColorAlpha(clr3, alpha);
  clr4 := ColorReverse(clr4);
  clr4 := ColorAlpha(clr4, alpha);

  NeedBatchSpace(6);

  FVertexBatch[FBatchIndex].x := x1;
  FVertexBatch[FBatchIndex].y := y1;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr1;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 0;
  FVertexBatch[FBatchIndex].tv := 0;
  NextBatchVertex;


  FVertexBatch[FBatchIndex].x := x2;
  FVertexBatch[FBatchIndex].y := y2;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr2;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 1;
  FVertexBatch[FBatchIndex].tv := 0;
  inc(FBatchIndex);

  FVertexBatch[FBatchIndex].x := x3;
  FVertexBatch[FBatchIndex].y := y3;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr3;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 0;
  FVertexBatch[FBatchIndex].tv := 1;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x3;
  FVertexBatch[FBatchIndex].y := y3;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr3;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 0;
  FVertexBatch[FBatchIndex].tv := 1;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x2;
  FVertexBatch[FBatchIndex].y := y2;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr2;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 1;
  FVertexBatch[FBatchIndex].tv := 0;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x4;
  FVertexBatch[FBatchIndex].y := y4;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr4;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 1;
  FVertexBatch[FBatchIndex].tv := 1;
  NextBatchVertex;

end;

procedure TDX2D.canvas_batch_Rectangle_Fill(x1, y1, x2, y2: TDXFloat; clr1, clr2,
  clr3, clr4: TColor; alpha: TDXFloat);
Type
  simple_point = dx9_tools.TNativeVertex;
var
  matView, matWorld, matProj: dx9_tools.TNativeMatrix;
begin

  clr1 := ColorReverse(clr1);
  clr1 := ColorAlpha(clr1, alpha);
  clr2 := ColorReverse(clr2);
  clr2 := ColorAlpha(clr2, alpha);
  clr3 := ColorReverse(clr3);
  clr3 := ColorAlpha(clr3, alpha);
  clr4 := ColorReverse(clr4);
  clr4 := ColorAlpha(clr4, alpha);

  NeedBatchSpace(6);

  FVertexBatch[FBatchIndex].x := x1;
  FVertexBatch[FBatchIndex].y := y1;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr1;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 0;
  FVertexBatch[FBatchIndex].tv := 0;
  NextBatchVertex;


  FVertexBatch[FBatchIndex].x := x2;
  FVertexBatch[FBatchIndex].y := y1;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr2;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 1;
  FVertexBatch[FBatchIndex].tv := 0;
  inc(FBatchIndex);

  FVertexBatch[FBatchIndex].x := x1;
  FVertexBatch[FBatchIndex].y := y2;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr3;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 0;
  FVertexBatch[FBatchIndex].tv := 1;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x1;
  FVertexBatch[FBatchIndex].y := y2;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr3;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 0;
  FVertexBatch[FBatchIndex].tv := 1;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x2;
  FVertexBatch[FBatchIndex].y := y1;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr2;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 1;
  FVertexBatch[FBatchIndex].tv := 0;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x2;
  FVertexBatch[FBatchIndex].y := y2;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr4;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := 1;
  FVertexBatch[FBatchIndex].tv := 1;
  NextBatchVertex;




end;
procedure TDX2D.canvas_batch_Rectangle_Fill_UV(x1, y1, x2, y2, u1, v1, u2,
  v2: TDXFloat; clr1, clr2, clr3, clr4: TColor; alpha: TDXFloat);
Type
  simple_point = dx9_tools.TNativeVertex;
var
  matView, matWorld, matProj: dx9_tools.TNativeMatrix;
begin

  clr1 := ColorReverse(clr1);
  clr1 := ColorAlpha(clr1, alpha);
  clr2 := ColorReverse(clr2);
  clr2 := ColorAlpha(clr2, alpha);
  clr3 := ColorReverse(clr3);
  clr3 := ColorAlpha(clr3, alpha);
  clr4 := ColorReverse(clr4);
  clr4 := ColorAlpha(clr4, alpha);

  NeedBatchSpace(6);

  FVertexBatch[FBatchIndex].x := x1;
  FVertexBatch[FBatchIndex].y := y1;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr1;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := u1;
  FVertexBatch[FBatchIndex].tv := v1;
  NextBatchVertex;


  FVertexBatch[FBatchIndex].x := x2;
  FVertexBatch[FBatchIndex].y := y1;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr2;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := u2;
  FVertexBatch[FBatchIndex].tv := v1;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x1;
  FVertexBatch[FBatchIndex].y := y2;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr3;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := u1;
  FVertexBatch[FBatchIndex].tv := v2;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x1;
  FVertexBatch[FBatchIndex].y := y2;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr3;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := u1;
  FVertexBatch[FBatchIndex].tv := v2;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x2;
  FVertexBatch[FBatchIndex].y := y1;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr2;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := u2;
  FVertexBatch[FBatchIndex].tv := v1;
  NextBatchVertex;

  FVertexBatch[FBatchIndex].x := x2;
  FVertexBatch[FBatchIndex].y := y2;
  FVertexBatch[FBatchIndex].z := 50;
  FVertexBatch[FBatchIndex].color := clr4;
  FVertexBatch[FBatchIndex].n.x := 0;
  FVertexBatch[FBatchIndex].n.y := 0;
  FVertexBatch[FBatchIndex].n.z := -1;
  FVertexBatch[FBatchIndex].tu := u2;
  FVertexBatch[FBatchIndex].tv := v2;
  NextBatchVertex;

end;

procedure TDX2D.canvas_Quad(x1, y1, x2, y2, x3, y3, x4, y4: single; clr1, clr2, clr3,
  clr4: TColor; alpha: TDXFloat);
Type
  simple_point = dx9_tools.TNativeVertex;
var
  matView, matWorld, matProj: dx9_tools.TNativeMatrix;
  p: array of TNativeVertex;
begin
  SetLength(p, 6);

  clr1 := ColorReverse(clr1);
  clr1 := ColorAlpha(clr1, alpha);
  clr2 := ColorReverse(clr2);
  clr2 := ColorAlpha(clr2, alpha);
  clr3 := ColorReverse(clr3);
  clr3 := ColorAlpha(clr3, alpha);
  clr4 := ColorReverse(clr4);
  clr4 := ColorAlpha(clr4, alpha);

  p[0].x := x1;
  p[0].y := y1;
  p[0].z := 50;
  p[0].color := clr1;
  p[0].n.x := 0;
  p[0].n.y := 0;
  p[0].n.z := -1;
  p[0].tu := 0;
  p[0].tv := 0;

  p[1].x := x2;
  p[1].y := y2;
  p[1].z := 50;
  p[1].color := clr2;
  p[1].n.x := 0;
  p[1].n.y := 0;
  p[1].n.z := -1;
  p[1].tu := 1;
  p[1].tv := 0;

  p[2].x := x3;
  p[2].y := y3;
  p[2].z := 50;
  p[2].color := clr3;
  p[2].n.x := 0;
  p[2].n.y := 0;
  p[2].n.z := -1;
  p[2].tu := 0;
  p[2].tv := 1;

  p[3].x := x3;
  p[3].y := y3;
  p[3].z := 50;
  p[3].color := clr3;
  p[3].n.x := 0;
  p[3].n.y := 0;
  p[3].n.z := -1;
  p[3].tu := 0;
  p[3].tv := 1;

  p[4].x := x2;
  p[4].y := y2;
  p[4].z := 50;
  p[4].color := clr2;
  p[4].n.x := 0;
  p[4].n.y := 0;
  p[4].n.z := -1;
  p[4].tu := 1;
  p[4].tv := 0;

  p[5].x := x4;
  p[5].y := y4;
  p[5].z := 50;
  p[5].color := clr4;
  p[5].n.x := 0;
  p[5].n.y := 0;
  p[5].n.z := -1;
  p[5].tu := 1;
  p[5].tv := 1;




// dx9window.dev.SetRenderState(d3drs_alpha, 1);
// 8  dx9window.dev.Clear(0,nil,D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER,D3DCOLOR_XRGB(0,0,255),1.0,0);


// d3dassert(dx9window.dev.SetFVF(ABSTRACT_FVF));
  d3dassert(dx9window.dev.DrawPrimitiveUP(D3DPT_TRIANGLELIST, 2, p[0],
      SizeOf(TNativeVertex)));

  // dx9window.dev.EndScene;
end;

procedure TDX2D.canvas_Rectangle_Empty(x1, y1, x2, y2: TDXFloat; clr1, clr2, clr3,
  clr4: TColor; alpha: TDXFloat);
Type
  simple_point = dx9_tools.TNativeVertex;
var
  matView, matWorld, matProj: dx9_tools.TNativeMatrix;
  p: array of TNativeVertex;
begin
  SetLength(p, 5);

  clr1 := ColorReverse(clr1);
  clr1 := ColorAlpha(clr1, alpha);
  clr2 := ColorReverse(clr2);
  clr2 := ColorAlpha(clr2, alpha);
  clr3 := ColorReverse(clr3);
  clr3 := ColorAlpha(clr3, alpha);
  clr4 := ColorReverse(clr4);
  clr4 := ColorAlpha(clr4, alpha);

  p[0].x := x1;
  p[0].y := y1;
  p[0].z := 50;
  p[0].color := clr1;
  p[0].n.x := 0;
  p[0].n.y := 0;
  p[0].n.z := -1;
  p[0].tu := 0;
  p[0].tv := 0;

  p[1].x := x2;
  p[1].y := y1;
  p[1].z := 50;
  p[1].color := clr2;
  p[1].n.x := 0;
  p[1].n.y := 0;
  p[1].n.z := -1;
  p[1].tu := 1;
  p[1].tv := 0;

  p[2].x := x2;
  p[2].y := y2;
  p[2].z := 50;
  p[2].color := clr3;
  p[2].n.x := 0;
  p[2].n.y := 0;
  p[2].n.z := -1;
  p[2].tu := 1;
  p[2].tv := 1;

  p[3].x := x1;
  p[3].y := y2;
  p[3].z := 50;
  p[3].color := clr3;
  p[3].n.x := 0;
  p[3].n.y := 0;
  p[3].n.z := -1;
  p[3].tu := 0;
  p[3].tv := 1;

  p[4].x := x1;
  p[4].y := y1;
  p[4].z := 50;
  p[4].color := clr2;
  p[4].n.x := 0;
  p[4].n.y := 0;
  p[4].n.z := -1;
  p[4].tu := 0;
  p[4].tv := 0;


  if AlphaOp = aoNone then
    dx9window.dev.SetRenderState(D3DRS_ALPHABLENDENABLE, 0)
  else begin
    dx9window.dev.SetRenderState(D3DRS_ALPHABLENDENABLE, 1);
    if AlphaOp = aoAdd then begin
      dx9window.dev.SetRenderState(D3DRS_SRCBLEND, integer(D3DBLEND_SRCALPHA));
      dx9window.dev.SetRenderState(D3DRS_DESTBLEND, integer(D3dblend_one));
    end
    else begin
      dx9window.dev.SetRenderState(D3DRS_SRCBLEND, integer(D3DBLEND_SRCALPHA));
      dx9window.dev.SetRenderState(D3DRS_DESTBLEND, integer(D3DBLEND_INVSRCALPHA));
    end;

  end;

  d3dassert(dx9window.dev.SetFVF(dx9_tools.ABSTRACT_FVF));
  d3dassert(dx9window.dev.DrawPrimitiveUP(D3DPT_LINESTRIP, 4, p[0],  SizeOf(TNativeVertex)));

  // dx9window.dev.EndScene;

end;

procedure TDX2D.canvas_Rectangle_Fill(x1, y1, x2, y2: TDXFloat; clr1: TColor;
  clr2: TColor; clr3: TColor; clr4: TColor; alpha: TDXFloat = 1.0);
Type
  simple_point = dx9_tools.TNativeVertex;
var
  matView, matWorld, matProj: dx9_tools.TNativeMatrix;
  p: array of TNativeVertex;
begin
  SetLength(p, 6);

  clr1 := ColorReverse(clr1);
  clr1 := ColorAlpha(clr1, alpha);
  clr2 := ColorReverse(clr2);
  clr2 := ColorAlpha(clr2, alpha);
  clr3 := ColorReverse(clr3);
  clr3 := ColorAlpha(clr3, alpha);
  clr4 := ColorReverse(clr4);
  clr4 := ColorAlpha(clr4, alpha);

  p[0].x := x1;
  p[0].y := y1;
  p[0].z := 50;
  p[0].color := clr1;
  p[0].n.x := 0;
  p[0].n.y := 0;
  p[0].n.z := -1;
  p[0].tu := 0;
  p[0].tv := 0;

  p[1].x := x2;
  p[1].y := y1;
  p[1].z := 50;
  p[1].color := clr2;
  p[1].n.x := 0;
  p[1].n.y := 0;
  p[1].n.z := -1;
  p[1].tu := 1;
  p[1].tv := 0;

  p[2].x := x1;
  p[2].y := y2;
  p[2].z := 50;
  p[2].color := clr3;
  p[2].n.x := 0;
  p[2].n.y := 0;
  p[2].n.z := -1;
  p[2].tu := 0;
  p[2].tv := 1;

  p[3].x := x1;
  p[3].y := y2;
  p[3].z := 50;
  p[3].color := clr3;
  p[3].n.x := 0;
  p[3].n.y := 0;
  p[3].n.z := -1;
  p[3].tu := 0;
  p[3].tv := 1;

  p[4].x := x2;
  p[4].y := y1;
  p[4].z := 50;
  p[4].color := clr2;
  p[4].n.x := 0;
  p[4].n.y := 0;
  p[4].n.z := -1;
  p[4].tu := 1;
  p[4].tv := 0;

  p[5].x := x2;
  p[5].y := y2;
  p[5].z := 50;
  p[5].color := clr4;
  p[5].n.x := 0;
  p[5].n.y := 0;
  p[5].n.z := -1;
  p[5].tu := 1;
  p[5].tv := 1;






// dx9window.dev.SetRenderState(d3drs_alpha, 1);
// 8  dx9window.dev.Clear(0,nil,D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER,D3DCOLOR_XRGB(0,0,255),1.0,0);


// d3dassert(dx9window.dev.SetFVF(ABSTRACT_FVF));
  if assigned(dx9window) and assigned(dx9window.dev) then begin
    d3dassert(dx9window.dev.DrawPrimitiveUP(D3DPT_TRIANGLELIST, 2, p[0],
        SizeOf(TNativeVertex)));
  end;

  // dx9window.dev.EndScene;

end;



procedure TDX2D.TextOut(sText: ansistring; x1, y1: TDXFloat; c: TColor; alpha,
  rScale: TDXFloat);
begin
  x1 := GlobaltoScreenX(x1);
  y1 := GlobalToScreenY(y1);
  canvas_Text(sText, x1,y1, c, alpha, rScale);
end;

procedure TDX2D.canvas_Text(sText: ansistring; x1, y1: TDXFloat; c: TColor; alpha,
  rScale: TDXFloat);
var
  t: integer;
  cx,cy,xx1,yy1,xx2,yy2,u1,u2,v1,v2: TDXFloat;
  rOverX, rOverY: TDXFloat;
begin
  x1 := {ScreenToGlobalX}(x1);
  y1 := {ScreenToGlobalY}(y1);
  if not BatchMode then
    BeginVertexBatch(d3dpt_trianglelist);

  rOverX := CurrentFont.overdrawX;
  rOverY := CurrentFont.overdrawY;
  for t:= 1 to length(sText) do begin
    if not BatchMode then
      BeginVertexBatch(d3dpt_trianglelist);

    xx1 := x1 + {ScaleSCreenXToGlobal}((rScale*CharWidth*(t-1)));
    cx := xx1 + ((rscale*charwidth)/2);
//    xx2 := x1 + {ScaleSCreenXToGlobal}((CharWidth*t*rScale)-1);

    xx1 := cx - ((rOverX*rscale*charwidth)/2);
    xx2 := cx + ((rOverX*rscale*charwidth)/2);

    yy1 := y1;
    cy := yy1 + ((rscale*charheight)/2);
    yy1 := cy - ((rOverY*rscale*charheight)/2);
    yy2 := cy + ((rOverY*rscale*charheight)/2);


//    yy2 := y1+{ScaleSCreenYToGlobal}(CharHeight-1);
    u1 := (ord(sText[t]) mod 16) / 16;
    u2 := ((ord(sText[t]) mod 16) / 16)+(1/16);
    v1 := ((ord(sText[t]) div 16) / 16);
    v2 := ((ord(sText[t]) div 16) / 16)+(1/16);



    canvas_batch_Rectangle_Fill_UV(xx1,yy1,xx2,yy2,u1+CharPadU,v1+CharPadV,u2-CharPadU,v2-CharPadV,c,c,c,c,alpha);
    //canvas_batch_Rectangle_Fill(xx1,xx2,yy1,yy2,c,c,c,c,alpha);
    //Flip;
  end;

end;

procedure TDX2D.canvas_Text(sText: ansistring; x1, y1: TDXFloat;  rFlags: TTFlags);
var
  iBold: nativeint;
  ii,i: nativeint;
  istroke: nativeint;
  iShadow: nativeint;
begin
  if tfBold in rFlags then iBold := 1 else iBold := 0;
  if tfEmboss in rFlags then inc(iBold);
  if tfStroke in rFlags then iStroke := 2 else iStroke := 0;

  iStroke := iStroke + iBOld;

  for i := 0 to iBold do begin

    AlphaOP := aoStandard;
    if tfShadow in rFlags then
      canvas_Text(sText, i+x1+3,y1+3,TExtShadowColor,1,1);
  end;

  if tfSTroke in rFlags then
  for ii := 0-iStroke to iStroke do begin
    for i := 0-iStroke to iStroke do begin
      AlphaOP := aoStandard;
      canvas_Text(sText, i+x1,ii+y1,TextStrokeColor,1,1);
    end;
  end;

  CommitVertexBatch;
  BeginVertexBatch;

   AlphaOP := aoAdd;
//  for i := 0 to iBold do begin
    if tfEmboss in rFlags then begin

      canvas_Text(sText, x1-1,y1-1,TExtEmbossColor,0.2,1);
    end;
//  end;

  CommitVertexBatch;
  BeginVertexBatch;
  AlphaOP := aoStandard;

  for i := 0 to iBold do begin
    canvas_Text(sText, i+x1,y1,TextColor,1,1);
  end;






end;


procedure TDX2D.canvas_Text_emboss(sText: ansistring; x1, y1: TDXFloat;
  c: TColor; alpha, rSCale: TDXFloat);
begin
  AlphaOP := aoStandard;
  canvas_Text(sText, x1+3,y1+3,clBlack,1,rscale);
  AlphaOP := aoAdd;
  canvas_Text(sText, x1-1,y1-1,clWhite,0.2,rscale);
  AlphaOP := aoStandard;
  canvas_Text(sText, x1,y1,c,alpha,rscale);
end;

procedure TDX2D.canvas_Text_shadow(sText: ansistring; x1, y1: TDXFloat;
  c: TColor; alpha, rScale: TDXFloat);
begin
  canvas_Text(sText, x1+3,y1+3,clBlack,0.1,scale);
  canvas_Text(sText, x1,y1,c,alpha,scale);

end;

procedure TDX2D.canvas_Text(sText: string; rFlags: TTFLags);
const
  TEXT_LINE_HEIGHT = 1.1;
var
  s,ss: ansistring;
begin
  s := ansistring(sText);

  if tfLast in rFlags then
    rFlags := FLastTextFlags;

  while splitstring(s, #10, ss, s) do begin
    ss := stringreplace(ss, #13, '', [rfReplaceAll]);
    canvas_Text(ss,TextOffset.x + (TextPosition.X * CharWidth), TExtOffset.y+(TextPosition.Y * Charheight*TEXT_LINE_HEIGHT), rFlags);
    TextPOsition.Y := TextPosition.y+1;
    TextPosition.x := 0;
  end;

  ss := stringreplace(ss, #13, '', [rfReplaceAll]);
  if ss <> '' then begin
    canvas_Text(ss,TextOffset.x + (TextPosition.X * CharWidth), TextOffset.y + (TextPosition.Y * (Charheight*TEXT_LINE_HEIGHT)), rFlags);
    TextPosition.X := TextPosition.X + length(ss);
  end;

  FLastTextFlags := rFlags;
end;


{ TDXControl }

procedure TDXControl.AddChild(c: TDXControl);
begin
  FDXChildren.add(c);
end;

procedure TDXControl.Assimilate;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TDXControl.CancelMouseButtons;
begin
  FillMem(@mouse_buttons_down[0], sizeof(mouse_buttons_down), 0);
end;

procedure TDX2D.CMMouseWheel(var Message: TCMMouseWheel);
var
  pt: TPoint;
begin
  with Message do
  begin
    Result := 0;
    pt.x := pos.x;
    pt.y := pos.y;

    if DoMouseWheel(ShiftState, WheelDelta, pt) then
      Message.Result := 1
    else if Parent <> nil then
{$IF DEFINED(CLR)}
      with UnwrapMessage(Message) do
{$ELSE}
      with TMessage(Message) do
{$IFEND}
        Result := Parent.Perform(CM_MOUSEWHEEL, WParam, LParam);
  end;

end;

constructor TDXControl.Create(aDX: TDX2d);
begin
  InitializeCriticalSection(sect);
  inherited Create;
  ScissorMe := true;

  FDXChildren := Tlist<TDXControl>.create;
  DX := aDX;
  FDX.AddControl(self);
  Parent := aDX;
  Width := 10;
  Height := 10;
  Visible := true;
  ClickOnMouseUp := true;

end;

destructor TDXControl.Destroy;
begin
  Parent := nil;
  FDX.RemoveControl(self);
  FDXChildren.Free;
  inherited;
  DeleteCriticalSection(sect);
end;

procedure TDXControl.Detachchildren;
var
  t: integer;
begin
  while FDXChildren.Count > 0 do begin
    FDXchildren[0].Parent := nil;
  end;
end;


procedure TDXControl.DoDraw;
begin
  inherited;

end;

procedure TDXControl.DoDrawTop;
begin
  inherited;
end;

procedure TDXControl.DoMouseClick;
begin
  //dni
end;

procedure TDXControl.DoMouseDown;
begin
  //dni
end;

procedure TDXControl.DoMouseEnter;
begin
  //dni
end;

procedure TDXControl.DoMouseLeave;
begin
  //dni
end;

procedure TDXControl.DoMouseOut;
begin
  //dni
end;

procedure TDXControl.DoMouseover;
begin
  //dni
end;

procedure TDXControl.DoMouseRightClick;
begin
  //dni
end;

procedure TDXControl.DoMouseUp;
begin
  //dni
end;

procedure TDXControl.Draw;
begin
  if Visible then begin

    //do not implement
    ScreenBound.x1 := FDX.globalToformX(X_RolledUp);
    ScreenBound.y1 := FDX.globalToformY(Y_RolledUp);
    ScreenBound.x2 := ScreenBound.x1+(FDX.ScaleGlobalXtoForm(width)-1);
    ScreenBound.Y2 := ScreenBound.y1+(FDX.ScaleGlobalYtoForm(height)-1);
    if ScissorMe then
      FDX.ScissorControl(self);
    DoDraw;
    FDX.EnableScissor(false);
  end;
end;

procedure TDXControl.DrawChildren;
var
  t: ni;
begin
  for t:= 0 to FDXChildren.Count-1 do begin
    if FDxChildren[t].Visible then
      FDxChildren[t].Draw;
  end;
end;

procedure TDXControl.DrawTopChildren;
var
  t: ni;
begin
  for t:= 0 to FDXChildren.Count-1 do begin
    if FDxChildren[t].Visible then
      FDxChildren[t].DrawTop;
  end;
end;

procedure TDXControl.DrawTop;
begin
  if Visible then begin

    //do not implement
    ScreenBound.x1 := FDX.globalToformX(X_RolledUp);
    ScreenBound.y1 := FDX.globalToformY(Y_RolledUp);
    ScreenBound.x2 := ScreenBound.x1+(FDX.ScaleGlobalXtoForm(width)-1);
    ScreenBound.Y2 := ScreenBound.y1+(FDX.ScaleGlobalYtoForm(height)-1);
    if ScissorMe then
      FDX.ScissorControl(self);
    DoDrawTop;
    FDX.EnableScissor(false);
  end;
end;

function TDXControl.GetBottom: single;
begin
  result := Top+HEight-1;
end;

function TDXControl.GEtRight: single;
begin
  result := LEft + Width -1;
end;

function TDXControl.GetShowing: boolean;
begin
  result := visible;
  if visible and (parent <> nil) and (parent is TDXControl) then
    result := TDXControl(parent).visible;


end;

procedure TDXControl.HandlePhysics(rDeltaTime: TDXTime);
const
  move_time = 1000;
var
  randy: nativefloat;
  diz, sliz: nativefloat;
  v1,v2: TNativeVector4;
  movemul: double;
begin
  v1.Init;
  v2.init;
  v1.y := targettop-top;
  v1.x := targetleft-left;
  v2.y := targetheight-height;
  v2.x := targetwidth-width;

//  if (v1.x <> 0) or (v1.y <> 0) then begin
//    if self is TDXButton then begin
//      if TDXButton(self).Caption = 'CLICK TO BEGIN' then begin
//        debug.log('Needs to Move by: '+v1.x.tostring+', '+v1.y.tostring);
//      end;
//    end;
//  end;

  randy := (random(65536)/65536) * 50;

  movemul := greaterof(-1.0, lesserof(1.0,((rDeltaTime+randy)/MOVE_TIME)));


  if movemul = 1 then begin
    if self is TDXButton then begin
      if TDXButton(self).Caption = 'CLICK TO BEGIN' then begin
        debug.log('multiplier: '+movemul.tostring);
      end;
    end;
  end;


  v1 := v1 * movemul;
  v2 := v2 * movemul;

//  if (v1.x <> 0) or (v1.y <> 0) then begin
//    if self is TDXButton then begin
//      if TDXButton(self).Caption = 'CLICK TO BEGIN' then begin
//        debug.log('multiplied: '+v1.x.tostring+','+v1.y.tostring);
//      end;
//    end;
//  end;

  Fleft := Fleft + v1.x;
  Ftop := Ftop + v1.y;
  width := Fwidth + v2.x;
  height := Fheight + v2.y;

  HandlePhysicsOfChildren(rDeltaTime);


end;

procedure TDXControl.HandlePhysicsOfChildren(rDeltaTime: TDXTime);
var
  t: ni;
begin
  for t:= 0 to Self.FDXChildren.count-1 do begin
    FDXChildren[t].HandlePhysics(rDeltaTime);
  end;

end;

procedure TDXControl.MouseCancel;
var
  i: ni;
begin
  FMouseWasDown := false;
  for i := 0 to FDXChildren.count-1 do begin
    FDXchildren[i].MouseCancel;
  end;
end;

procedure TDXControl.MouseClick;
begin
  inherited;
  //do not implement
  DoMouseClick;
end;

procedure TDXControl.MouseDown;
begin
  inherited;
  if FMouseIsOver then
    DoMouseDown;

end;

procedure TDXControl.MouseEnter;
begin
  inherited;
  DoMouseEnter;

end;

function TDXControl.IsInBounds(screenx, screeny: single): boolean;
begin
  result := true;
  if not visible then
    exit(false);

  if screenx < SCreenBound.x1 then begin
    result := false;
    exit;
  end;

  if screenx > SCreenBound.x2 then begin
    result := false;
    exit;
  end;

  if screeny < SCreenBound.y1 then begin
    result := false;
    exit;
  end;

  if screeny > SCreenBound.y2 then begin
    result := false;
    exit;
  end;
end;

function TDXControl.LocalToScreenX(x: single): single;
begin
  result := FDX.GlobalToScreenX(X_RolledUp);

end;

function TDXControl.LocalToScreenY(Y: single): single;
begin
  result := FDX.GlobalToScreenX(Y_RolledUp);
end;

procedure TDXControl.Lock;
begin
  EnterCriticalSection(sect);
end;

function TDXControl.MouseEvent(dxme: TDXMouseEvent; screenx, screeny,x,y: single; ss: TShiftState): boolean;
var
  bIn: boolean;
begin

  bIn := IsInBounds(screenx, screeny);
  result := bIn;
  if not self.showing then
    exit(false);


  //if in-out state has changed
  if Visible then begin
    if bIn <> FMouseIsOver then begin
  //    Debug.ConsoleLog('In-out state has changed');
      FMouseIsOver := bIn;
      if bin then
        MouseEnter
      else
        MouseLeave;

      if FMouseIsOver then
        MouseOver;

    end;
  end else begin
    if FMouseIsOver then
      MouseLeave;
  end;


  if ssRight in ss then begin
    if bIn then begin
      if not mouse_buttons_down[1] then begin
        if dxME = dxmeDown then begin
          mouse_buttons_down[1] := true;
          FMouseRight := true;
          MouseDown;
        end;
      end;
    end;
  end else begin
    if mouse_buttons_down[1] then begin
      if dxME = dxmeUp then begin
        mouse_buttons_down[1] := false;
        FMouseRight := true;
        MouseUp;
      end;
    end;

  end;

  if ssLeft in ss then begin
    if bIn then begin
      if (not mouse_buttons_down[0]) and Visible then begin
        if dxME = dxmeDown then begin
          mouse_buttons_down[0] := true;
          FMouseRight := false;
          MouseDown;
        end;
      end;
    end;
  end else begin
    if mouse_buttons_down[0] then begin
      if dxME = dxmeUp then begin
        mouse_buttons_down[0] := false;
        FMouseRight := false;
        MouseUp;
      end;

    end;
  end;

end;

procedure TDXControl.MouseLeave;
begin
  CancelMouseButtons;
  //do not implement
  DoMouseLeave;
end;

procedure TDXControl.MouseMove(x, y: single);
begin
  //do not implement




end;

procedure TDXControl.MouseOut;
begin
  inherited;
  //do not implement
  CancelMousebuttons;
  DoMouseOut;
end;

procedure TDXControl.MouseOver;
begin
  inherited;
  //do not implement
  DoMouseOver;
end;

procedure TDXControl.MouseRightClick;
begin
  inherited;
  //do not implement
  DoMouseRightClick;
end;

procedure TDXControl.MouseUp;
begin
  inherited;
  //do not implement
  if FMouseIsOver then begin
    DoMouseUp;
    if clickonmouseup then
      MouseClick;
  end;


end;

procedure TDXControl.PositionAbove(cBelow: TDXControl; gap: ni;
  match_width: boolean);
begin
  Top := cBelow.Top - (height+gap);
  if match_width then
    Width := cBelow.Width;

end;

procedure TDXControl.PositionBelow(cAbove: TDXControl; gap: ni;
  match_width: boolean);
begin
  Top := cAbove.Bottom+gap;
  if match_width then
    Width := cAbove.Width;
  Left := cAbove.Left;
end;

procedure TDXControl.PositionLeft(cRight: TDXControl; gap: ni;
  match_height: boolean);
begin
  Left := cRight.Left - (Width+gap);
  if match_height then
    Height := cRight.Height;

end;

procedure TDXControl.PositionRight(cLeft: TDXControl; gap: ni;
  match_height: boolean);
begin
  Left := cLeft.Right + gap;
  if match_height then
    Height := cLeft.Height;

  top := cLeft.top;


end;

procedure TDXControl.RemoveChild(C: TDXControl);
begin
//  Debug.ConsoleLog('Removing Child '+c.classname+' of '+self.classname+' there are '+inttostr(FDXChildren.count)+' Child controls.');
  FDXChildren.remove(c);
//  Debug.ConsoleLog('Removed Child '+c.classname+' of '+self.classname+' there are '+inttostr(FDXChildren.count)+' Child controls.');

end;

procedure TDXControl.Resized;
begin
  //


end;

procedure TDXControl.SetHeight(const Value: single);
var
  bResized: boolean;
begin
  bResized := abs(value-Fheight) > 1.0;

  FHeight := Value;
  FTargetHeight := value;

  if bResized then
    Resized;
end;

procedure TDXControl.SetLeft(const Value: single);
begin
  FLeft := Value;
  FtargetLeft := value;
end;

procedure TDXControl.SetParent(const Value: Tobject);
begin
//  if FParent = value then
//    exit;

  if assigned(FParent) then
    if FParent is TDXControl then
      TDXControl(FParent).RemoveChild(self)
    else
      TDX2D(FParent).RemoveDXChild(self);

  FParent := Value;

  if assigned(FParent) then
    if FParent is TDXControl then
      TDXControl(FParent).ADdChild(self)
    else
      TDX2D(FParent).AddDXChild(self);



end;

procedure TDXControl.SetTop(const Value: single);
begin
  Ftop := Value;
  FTargetTop := value;
end;

procedure TDXControl.SetWidth(const Value: single);
var
  bResized: boolean;
begin
  bResized := abs(value-Fwidth) > 1.0;

  FWidth := Value;
  FTargetWidth := value;

  if bResized then
    Resized;

end;

procedure TDXControl.Unlock;
begin
  LeaveCriticalSection(sect);
end;

function TDXControl.X_RolledUp: single;
begin
  if (PArent = nil) or (not (Parent is TDXControl)) then
    result := Left
  else
    result := TDXControl(Parent).X_RolledUp + Left;
end;

function TDXControl.Y_RolledUp: single;
begin
  if (PArent = nil) or (not (Parent is TDXControl)) then
    result := Top
  else
    result := TDXControl(Parent).Y_RolledUp + Top;
end;

{ TDXButton }


procedure TDXButton.CalcTextSize;
var
  sz1, sz2: double;
  l: ni;
  mar: double;
begin
  l := length(caption);
  if l = 0 then
    exit;

  mar := lesserof(width*0.2,height*0.5);
  sz1 := (width-mar)/l;
  sz2 := (height-mar);

  sz1 := lesserof(sz1,sz2);
  charwidth :=  trunc(sz1);
  charheight := charwidth;
end;

procedure TDXButton.DoClick;
begin
  if assigned(FOnclick) then
    FOnClick(self);
end;

procedure TDXButton.DoDraw;
var
  x,y: ni;
  a,b,c,d,e: single;
  l,t,w,h: single;
const
  margin = 5;
begin
  inherited;
  if AutoTextSize  then
    calctextsize;
  //no implementation requred
  FDX.SetTexture(-1);


  l := self.Left;
  w := self.width;
  t := self.Top;
  h := self.Height;

  if down then begin
      FDX.Rectangle_Fill(l, t, l+w, t+h, clWhite, 0.3*Colorwhenlit.a);
      FDX.Rectangle_Fill(l+margin, t+margin, l+w, t+h, ColorWhenLit.ToColor, 0.3*Colorwhenlit.a);
      FDX.Rectangle_Fill(l, t, l + w, t+h, ColorWhenLit.ToColor, 0.6*Colorwhenlit.a);
      //progress
      if StepCount > 0 then begin
        a := self.Left;
        b := self.top;
        c := self.Left + (Width * (Step/StepCount));
        d := self.TOp +self.Height;
        e := self.Left + self.Width;
        FDX.Rectangle_Fill(a,b,e,d, ColorWhenLit.ToColor, ColorWhenLit.ToColor, clBlack, clBlack, 0.6*Colorwhenlit.a);
        FDX.Rectangle_Fill(a,b,c,d, clWhite, clWhite, ColorWhenLit.ToColor, ColorWhenLit.ToColor, 0.6*Colorwhenlit.a);
      end;
      if FMouseIsOver then
        FDX.Rectangle_Fill(l, t, l+w, t+h, clWhite, 0.3);

  end else begin
      FDX.Rectangle_Fill(l, t, l+w, t+h, clWhite, 0.3*Colorwhennotlit.a);
      FDX.Rectangle_Fill(l+margin, t+margin, l+w, t+h, ColorWhennotLit.ToColor, 0.3*Colorwhennotlit.a);
      FDX.Rectangle_Fill(l, t, l+w, t+h, ColorWhennotLit.ToColor, 0.6*Colorwhennotlit.a);
      if StepCount > 0 then begin
        a := l;
        b := t;
        c := l + (w * (Step/StepCount));
        d := t+h;
        e := l+w;
        FDX.Rectangle_Fill(a,b,e,d, Colorwhennotlit.ToColor, Colorwhennotlit.ToColor, clBlack, clBlack, 0.6*Colorwhenlit.a);
        FDX.Rectangle_Fill(a,b,c,d, clWhite, clWhite, Colorwhennotlit.ToColor, Colorwhennotlit.ToColor, 0.6*Colorwhenlit.a);
      end;

      if FMouseIsOver then
        FDX.Rectangle_Fill(l,t,l+w, t+h, clWhite, 0.3*Colorwhennotlit.a);
  end;

  if Animate then
    DoDrawAnimation;

  FDX.ResetTExt;
  FDX.EndVertexBatch;
  FDX.ResetText;
  FDX.SetFont(0);

  FDX.CharHeight := CharHeight;
  FDX.CharWidth :=CharWidth;
//  FDX.TextColor := clWhite;
//  x := round(FDX.GlobalToScreenX(X_RolledUp));
//  y := round(FDX.GlobalToScreenY(self.Y_RolledUp));
//  x := x + round(FDX.GlobalToScreenX(Width) / 2);
//  y := y + round(FDX.GlobalToScreenY(Height) / 2);
  x := round(X_RolledUp + (Width / 2))-round((FDX.ScaleScreenXToGlobal(FDX.CharWidth*length(caption)) / 2));
  y := round(Y_RolledUp + (Height / 2))-round(FDX.ScaleScreenYToGlobal(FDX.CharHeight / 2));


  FDX.TextOffset.x := FDX.GlobalToScreenX(x);
  FDX.TextOffset.y := FDX.GlobalToScreenY(y);
  FDX.TextColor := clWhite;

  FDX.canvas_Text(caption, [tfStroke, tfBold, tfShadow]);
  FDX.ResetText;


end;


procedure TDXButton.DoDrawAnimation;
begin
  //no imp
end;

procedure TDX2d.VolatileTextureFromFastBitmap(var tex: IDirect3dTexture9; fbm: TFastbitmap);
var
  pp,pSL: pbyte;
  p: PColorArray;
  plocked: _D3DLOCKED_RECT;
  r: TRect;
  t, u: integer;
begin

//  if tex = nil then
    tex := CreateVolatileTexture(1280, 480);

  if fbm.width = 0 then exit;
  if fbm.height = 0 then exit;



  r.Top := 0;
  r.Left := 0;
  r.Right := fbm.width-1;
  r.Bottom := fbm.height-1;
  plocked.pbits := nil;
  if assigned(tex) then begin
    if succeeded(tex.LockRect(0, plocked, nil, D3DLOCK_DISCARD
        { or D3DLOCK_NOOVERWRITE } )) then
      try
        pp := plocked.pBits;
        if assigned(pp) then begin
          for u := 0 to fbm.height-1 do begin
            pSL := @pp[(plocked.Pitch * u)];
            //if assigned(pSL) then
            for t := 0 to ((fbm.width) - 1) do begin
              p := PColorArray(pSL);
              p[t] := RBSwap(fbm.Canvas.Pixels[t,u]);
            end;
          end;
        end;

      finally
        tex.UnlockRect(0);
      end;
  end;
end;



procedure TDXButton.DoMouseClick;
begin
  Doclick;
end;

procedure TDXButton.DoMouseDown;
begin
  if not FMouseRight then begin
    if sticky then
      stuckdown := not stuckdown
    else
      state := bsDown;
  end;

  if not clickonmouseup then begin
    if not FMouseRight then
      MouseClick
    else
      MouseRightClick;

  end;

end;

procedure TDXButton.DoMouseOut;
begin
  FOver := false;
end;

procedure TDXButton.DoMouseOver;
begin
  FOver := true;
end;

procedure TDXButton.DoMouseRightClick;
begin
  inherited;
  DoRightclick;
end;

procedure TDXButton.DoMouseUp;
begin
  inherited;
  if not sticky then
    state := bsup;
end;

procedure TDXButton.DoRightClick;
begin
  if assigned(FOnRightclick) then
    FOnRightClick(self);

end;

function TDXButton.GetDown: boolean;
begin
  result :=  state in [bsDown, bsStuckDown];
end;

function TDXButton.GetStuckDown: boolean;
begin
  result := state = bsStuckDown;
end;

procedure TDXButton.Init;
begin
  inherited;
  FColorWhenLit.g := 1.0;
  FcolorWhenLit.a := 0.8;

  FColorWhenNotLit.r := 1.0;
  FColorWhenNotLit.g := 1.0;
  FColorWhenNotLit.b := 1.0;
  FcolorWhenNotLit.a := 0.2;
  CharWidth := 16;
  CharHeight := 16;

  AutoTextSize := true;


end;

procedure TDXButton.InternalAnimation(rDeltaTime: TDXTime);
begin
  //no imp
end;

procedure TDXButton.OnStartAnimation;
begin
  //
end;

procedure TDXButton.OnStopAnimiation;
begin
  //
end;

procedure TDXButton.Resized;
begin
  inherited;
  if Animate then
    OnStartAnimation;
end;

procedure TDXButton.SetAnimate(const Value: boolean);
begin
  if value <> FAnimate then begin
    FAnimate := Value;
    if FAnimate then OnStartAnimation else OnStopAnimiation;

  end;

end;

procedure TDXButton.SetCharHeight(const Value: single);
begin
  FCharheight := Value;
  FTargetCharheight := value;
end;

procedure TDXButton.SEtCharWidth(const Value: single);
begin
  FCharWidth := Value;
  TargetCharWidth := value;
end;

procedure TDXButton.SetDown(const Value: boolean);
begin
  if sticky then
    StuckDown := value
  else
    Down := value;
end;

procedure TDXButton.SetSticky(const Value: boolean);
begin
  FSticky := Value;
  ClickOnMouseUp := not sticky;
end;

procedure TDXButton.SetStuckDown(const Value: boolean);
begin
  if value then
    state := bsStuckDown
  else
    state := bsStuckUp;
end;

procedure TDXButton.SetTargetCharWidth(const Value: single);
begin
//  if value = 0 then
//    raise ECritical.create('Char width cannot be 0');
  FTArgetCharWidth := Value;

end;

procedure TDXButton.HandlePhysics(rDeltaTime: TDXTime);
const
  move_time = 1000;
var
  randy: nativefloat;
  diz, sliz: nativefloat;
  v1: TNativeVector4;
begin
  if not visible then
    exit;

  inherited;
  InternalAnimation(rDeltaTime);
  if not InMotion then
    exit;
  v1.init;
  v1.x := targetcharwidth-charwidth;
  v1.y := targetcharheight-charheight;
  v1 := v1 * greaterof(-1.0, lesserof(1.0,(rDeltaTime/MOVE_TIME)));

  Fcharwidth := Fcharwidth + v1.x;
  Fcharheight := Fcharheight + v1.y;

end;

function TDX2d.BeginDXLineDraw: ID3DXLine;
begin
  //EndVertexBatch;
  D3DXCreateLine(self.dx9window.dev, result);
  result._Begin;
end;

procedure TDX2D.BeginLine;
begin
  linecache := BeginDXLineDraw;
  linecache.SetWidth(3);
  linecache_idx := 0;
end;

procedure TDX2D.EnableScissor(b: boolean);
var
  r: TRect;
  pr:PRect;
begin
  CommitVertexBatch;
  if b then
    d3dassert(dx9window.dev.SetRenderState(D3DRS_SCISSORTESTENABLE, 1))
  else
    d3dassert(dx9window.dev.SetRenderState(D3DRS_SCISSORTESTENABLE, 0));

end;

procedure TDX2d.EndDXLine(l: ID3DXLine);
begin
  l._end;
end;



procedure TDX2D.EndLIne;
begin
  FlushLine;
  EndDXLine(linecache);
  linecache := nil;
end;

procedure oinit;
var
  p: string;
  pp: ni;
begin
  p := APGet('VertexBatchSize', '2000', false);
  pp := strtoint(p);
  VERTEX_BATCH_SIZE := lesserof(MAX_VERTEX_BATCH_SIZE, pp);

end;

procedure ofinal;
begin
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

initialization

init.RegisterProcs('advancedgraphics_DX', oinit, ofinal, 'ApplicationParams');
dxdx := 0;

end.
