unit dmx_objects;
// [ ]add stage to universe
// [ ]load stage data loads all light data
// [ ]include vector graphics for stage setup

{$I 'DelphiDefs.inc'}
// 4-banger laser notes
// ch 1 - pattern type
// ch 2 - pattern
// ch 3 - x [0=center] [1-24 manual pan] note that for manual pan/tilt without motion use REALLY low values like 1-24
// ch 4 - y [0=center] [1-24 manual tilt] same as above
// ch 5 - scan variance
// ch 6 - rotation 1
// ch 7 - rotation 2
// ch 8 - rotation or animatino [0= none] [1-24 manual rotation/adnimation]
// ch 10 - x divergence
// ch 11 - y divergence



// Caliente Notes:
// DMX Manual is WRONG
// Ch 9 Controls Color
// ch 8 size
// ch 7 controls dynamic animation speed (if dynamic mode)
// Ch 6 Controls scan speed      [fast]..[slow]
// ch 5 also controls scan speed, confused... kinda stabilized maybe
// ch 4 y
// ch 3 x
// ch 2 pattern
// ch 1 mode - [off][static][dynamic][sound][auto]

// static patterns
// 0 - circle like
// - circle array
// - two circles
// - three circles
// - four circles
// - two circle (again)?
// - two circle array
// - solid line
// - ..
// - solid growable line
// - broken line
// - vertical line
// - vertical broken line
// - horizontal wave
// - horizonwal circle array
// - two vertical lines
// - big broken circle

// colors
// 0 - white
// 2 - magenta
// 4 - cyan
// 6 - yellow
// 8 - blue
// 10 - red
// 12 - green
// 14 - white+magenta
// 16 - white+cyan
// 18 - white+yellow
// 20 - magenta+cyan
// 22 - magenta+yellow
// 24 - magenta+blue
// 26 - magenta+green
// - cyan+blue
// - cyan+red
// - yellow+blue
// - yellow+red
// - green+yellow
// - red+blue
// - green+blue
// - blue+white
// - green+red
// - mag+red
// - red+white (maybe cyan)
// - cyan+green
// - green+magenta
// - white+green
// - cyan+magenta
// - magenta+green+cyan




// todo 1: create an effect stack list screen which sets just a global variable that says TFX type and layer.
// todo 1: implement a messages like "12345,change_fx_param,TFXAudioStrike,4,CoolTurbulenceRatio,6
// todo 1: publish parameters for all effects
// todo 2: publish virtual parameters for all lights


// todo 2: create blackout/base FX
// todo 2: add backout/base FX to default script
// todo 3: add timed-destroy FX
// todo 3: add random strobe FX (timed-destroy)
// todo 4: add flash FX
// todo 4: add add-color/intensity FX/// pulse
// todo 1: fix fadeout time when approaching end of track
// todo 5: implement quick out
// todo 5: revo doesn't support strobe blackout
// todo 1: color fade

// todo 1: fade to black on turbulence

//
interface

{ x$DEFINE HALOSIM }
{ x$DEFINE DISABLE_W }
{ x$DEFINE LOG_BURN }

uses
  observableobject, betterobjectfactory, math, typex, winapi.winsock,
  betterobject, Generics.Collections.fixed, sysutils, geometry, namevaluepair,
  sharedobject, applicationparams, colorblending, artnet,
{$IFNDEF DMX_REMOTE_ONLY}
  dmx_h,
{$ENDIF}
  better_collections, colorconversion, systemx, managedthread, windows, classes,
  easyimage, numbers, orderlyinit,
{$IFDEF WINDOWS}
  advancedgraphics,
  graphics,
{$ENDIF}
  debug, stringx,
  web.win.sockets, tickcount;

const
  ANG_SEARCH = 0.25;
  GROUP_SPOT = 1;
  GROUP_WASH = 2;
  GROUP_BLINDER = 4;
  GROUP_BEAM = 8;
  GROUP_LASER = 16;
  GROUP_SCAN = 32;
  GROUP_STROBE = 64;
  VUE_MACRO_SOUND = '255';
  VUE_MACRO_NONE = '0';
  BALL_HIST_SIZE = 6;

type
  TDMXTransport = (transDMX, transArtNet);

  TDMXExternalJoyControl = class; // forward
  TDMXStage = class; // forward
  TDMXLIghtGroup = class; // forward
  TDMXLightGroups = class; // forward
  TDMXChannelCluster = class; // forward
  TDMXEffects = class; // forward
  TDMXMultiverse = class;
  TEffectLibrary = TBetterObjectFactory;

  TDMXChannelDefinition = class(TBetterObject)
  private
    FDetails: string;
    FChannelOffset: integer;
    FDescription: string;
  public
    property ChannelOffset: integer read FChannelOffset write FChannelOffset;
    property Desription: string read FDescription write FDescription;
    property Details: string read FDetails write FDetails;
  end;

  IDMXInterface = interface(IInterface)
    ['{19F312BE-58B5-40AE-8A8C-09AB2228240C}']
    function GetX: NativeFloat;
    function gety: NativeFloat;
    function GetXX: NativeFloat;
    function getYY: NativeFloat;
    function GetPX: NativeFloat;
    function getPY: NativeFloat;

    procedure SetXX(const Value: NativeFloat);
    procedure SetYY(const Value: NativeFloat);
    procedure SetPX(const Value: NativeFloat);
    procedure SetPY(const Value: NativeFloat);
    function GetGroup: integer;
    procedure SetGroup(const Value: integer);
    property X: NativeFloat read GetX;
    property Y: NativeFloat read gety;
    property XX: NativeFloat read GetXX write SetXX;
    property YY: NativeFloat read getYY write SetYY;
    property PX: NativeFloat read GetPX write SetPX;
    property PY: NativeFloat read getPY write SetPY;

    procedure SetUsePictureMap(const Value: boolean);
    function GetusePictureMap: boolean;
    property UsePictureMap: boolean read GetusePictureMap
      write SetUsePictureMap;
    property Group: integer read GetGroup write SetGroup;
    function GetLightClass: string;
    property LightClass: string read GetLightClass;
    function GetBaseChannel: integer;
    property BaseChannel: integer read GetBaseChannel;
    procedure Test(b: boolean);
    procedure StrobeBlackout(bStrobeBlackout: boolean);
    procedure ClearBlackout();
  end;

  IDMXStrobe = interface(IDMXInterface)
    ['{3DE64EA8-E209-4E2B-85C5-012E7E6BDB6A}']
    function GetStrobeSpeed: NativeFloat;
    procedure SetStrobeSpeed(const Value: NativeFloat);
    function GetSTrobeIntensity: NativeFloat;
    procedure SetStrobeIntensity(const Value: NativeFloat);

    property StrobeIntensity: NativeFloat read GetSTrobeIntensity
      write SetStrobeIntensity;
    property StrobeSpeed: NativeFloat read GetStrobeSpeed write SetStrobeSpeed;
    procedure StrobeStrike;
    function GetSTrobeEnable: boolean;
    procedure SetStrobeEnable(const Value: boolean);
    property StrobeEnable: boolean read GetSTrobeEnable write SetStrobeEnable;

  end;

  IDMXBlackOut = interface(IDMXInterface)
    ['{4C06A23C-0D5F-4E9E-B73B-816364C0C806}']
    procedure SetBlackout(const b: NativeFloat);
    function GetBlackout: NativeFloat;
    property Blackout: NativeFloat read GetBlackout write SetBlackout;
  end;

  IDMXColor = interface(IDMXInterface)
    ['{AA7FC6F6-3209-417C-A764-459C24C5A6BC}']
    procedure SetColor(const c: TColor);
    function GetColor: TColor;
    property Color: TColor read GetColor write SetColor;
    procedure SaveSourceColor;
    procedure SetSourceColor(const c: TColor);
    procedure SetTargetColor(const c: TColor);
    function GetSourceColor: TColor;
    function GetColorMix: NativeFloat;
    procedure SetColorMix(const mix: NativeFloat);
    property SourceColor: TColor read GetSourceColor write SetSourceColor;
    property ColorMix: NativeFloat read GetColorMix write SetColorMix;
    procedure MixColors;
  end;

  IDMXIntensity = interface(IDMXInterface)
    ['{3045E458-1E1E-406F-A90F-8CA5877DF19C}']
    function GEtMinIntensity: NativeFloat;
    procedure SetMinIntensity(const Value: NativeFloat);
    function GEtMaxIntensity: NativeFloat;
    procedure SetMaxIntensity(const Value: NativeFloat);
    procedure SetIntensity(i: NativeFloat);
    function GetIntensity: NativeFloat;
    procedure SetTargetIntensity(const i: NativeFloat);
    function GetTargetIntensity: NativeFloat;
    property Intensity: NativeFloat read GetIntensity write SetIntensity;
    property TargetIntensity: NativeFloat read GetTargetIntensity
      write SetTargetIntensity;
    function GetburnRate: NativeFloat;
    function GetcoolRate: NativeFloat;
    procedure SetBurnRate(const Value: NativeFloat);
    procedure SetCoolRate(const Value: NativeFloat);
    property BURNRATE: NativeFloat read GetburnRate write SetBurnRate;
    property COOLRATE: NativeFloat read GetcoolRate write SetCoolRate;
    property MinIntensity: NativeFloat read GEtMinIntensity
      write SetMinIntensity;
    property MaxIntensity: NativeFloat read GEtMaxIntensity
      write SetMaxIntensity;

    procedure SetLumens(const i: NativeFloat);
    function GetLumens: NativeFloat;
    property RelativeLumens: NativeFloat read GetLumens write SetLumens;
    function GetStrobeOverdrive: boolean;
    procedure SetStrobeOverdrive(const Value: boolean);
    property StrobeOverdrive: boolean read GetStrobeOverdrive
      write SetStrobeOverdrive;
  end;

  IDMXColorAndIntensity = interface(IDMXIntensity)
    ['{B5795189-E3FE-424F-BA3F-C7360F969DB1}']
    procedure SetColor(const c: TColor);
    function GetColor: TColor;
    property Color: TColor read GetColor write SetColor;
    procedure SaveSourceColor;
    procedure SetSourceColor(const c: TColor);
    procedure SetTargetColor(const c: TColor);
    function GetTargetColor: TColor;
    function GetSourceColor: TColor;
    function GetColorMix: NativeFloat;
    procedure SetColorMix(const mix: NativeFloat);
    property SourceColor: TColor read GetSourceColor write SetSourceColor;
    property TargetColor: TColor read GetTargetColor write SetTargetColor;
    property ColorMix: NativeFloat read GetColorMix write SetColorMix;
    procedure MixColors;
  end;

  IDMXColorMixer = interface(IDMXColor)
    ['{141004E9-EDA8-4E70-855E-F488F10B37E3}']
  end;

  TDMXChannelClusterList = class(TList<TDMXChannelCluster>)
  public
    procedure ClearAndFree;
  end;

  TDMXChannelClusterClass = class of TDMXChannelCluster;
  TDMXUniverse = class; // forward

  TUniverseList<T> = class(TList<T>)
  private
    FMultiverse: TDMXMultiverse;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; reintroduce; virtual;

  end;

  TDMXObject = class(TBetterObject);

  TDMXChannelCluster = class(TDMXObject)
  private
    FDebugFlags: integer;
    FParent: TDMXChannelCluster;
    FLocalChannelData: PByte;
    Fx, Fxx: NativeFloat;
    Fy, Fyy: NativeFloat;
    FPX, FPY: NativeFloat;
    FUsePicturemap: boolean;
    FGroup: integer;
    FDisableDMXWrite: boolean;
    FNewestLight: TDMXChannelCluster;
    FTransport: TDMXTransport;
    FArtNetEndPOint: TArtNetEndpoint;
    FArtNetIP: string;
    procedure SetParent(const Value: TDMXChannelCluster);
    procedure SetBaseChannel(const Value: integer);
    procedure SetChannelcount(const Value: integer);
    function GetX: NativeFloat;
    function gety: NativeFloat;
    function GetXX: NativeFloat;
    function getYY: NativeFloat;
    function GetPX: NativeFloat;
    function getPY: NativeFloat;
    procedure SetUsePictureMap(const Value: boolean);
    function GetusePictureMap: boolean;
    procedure SetX(const Value: NativeFloat); virtual;
    procedure SetY(const Value: NativeFloat); virtual;
    procedure SetXX(const Value: NativeFloat); virtual;
    procedure SetYY(const Value: NativeFloat); virtual;
    procedure SetPX(const Value: NativeFloat); virtual;
    procedure SetPY(const Value: NativeFloat); virtual;

    procedure CoordinateUpdated; virtual;
    function GetGroup: integer;
    procedure SetGroup(const Value: integer);
    function GetChild(idx: integer): TDMXChannelCluster;
    function GetchildCount: integer;
    function GetBaseChannel: integer;
    procedure Reset;
    procedure SetArtNetEndpoint(const Value: TArtNetEndpoint);
    procedure SetArtNetIP(const Value: string);
  protected
    FBAseChannel: integer;
    FIsMacrocluster: integer;
    FChannelCount: integer;
    FLastUpdateTime: int64;
    FDeltaTime: integer;
    FFLatlights, FSubclusters: TDMXChannelClusterList;
    fTest: boolean;
    FStrobeBlackout: integer;
    FChannelDefinitions: TList<TDMXChannelDefinition>;
    procedure Channelchanged; virtual;
    procedure WriteDMXData; virtual;
    procedure ParentChanged; virtual;
    procedure UpdatePhysics(enginetime: int64); virtual;
    procedure SetTestParams(bTesting: boolean); virtual;
    procedure Init; override;
  public
    procedure ClearDefinitions;
    constructor Create; override;
    destructor Destroy; override;
    property DebugFlags: integer read FDebugFlags write FDebugFlags;
    property BaseChannel: integer read GetBaseChannel write SetBaseChannel;
    property Channelcount: integer read FChannelCount write SetChannelcount;
    property IsMacroCluster: integer read FIsMacrocluster write FIsMacrocluster;
    procedure RegisterLight(l: TDMXChannelCluster;
      bOnlyFlatList: boolean = false);
    procedure UnregisterLight(l: TDMXChannelCluster);
    procedure AfterAddLight; virtual;
    procedure Add(l: TDMXChannelCluster; iChannel: integer;
      GridX, gridY: NativeFloat; iGroup: integer;
      ArtNetIP: string = ''); overload;
    function Add(c: TDMXChannelClusterClass; iChannel: integer;
      GridX, gridY: NativeFloat; iGroup: integer; ArtNetIP: string = '')
      : TDMXChannelCluster; overload;
    property Parent: TDMXChannelCluster read FParent write SetParent;
    property LIghts: TDMXChannelClusterList read FSubclusters;
    property FlatLIghts: TDMXChannelClusterList read FFLatlights;
    procedure SetDataPointer(p: PByte);
    function GetSupportingList<i: IDMXInterface>(guid: TGUID;
      iGROUPMAsk, iGroupResult: integer): TList<i>;
    property X: NativeFloat read GetX write SetX;
    property Y: NativeFloat read gety write SetY;
    property XX: NativeFloat read GetXX write SetXX;
    property YY: NativeFloat read getYY write SetYY;
    property PX: NativeFloat read GetPX write SetPX;
    property PY: NativeFloat read getPY write SetPY;

    property UsePictureMap: boolean read GetusePictureMap
      write SetUsePictureMap;

    property Group: integer read GetGroup write SetGroup;

    procedure Defaults; virtual;
    property DisableDMXWrite: boolean read FDisableDMXWrite
      write FDisableDMXWrite;
    function GetLightClass: string;
    property LightClass: string read GetLightClass;
    property ChildCount: integer read GetchildCount;
    property Children[idx: integer]: TDMXChannelCluster read GetChild;

    procedure Test(b: boolean);
    property Testing: boolean read fTest;
    property NewestLight: TDMXChannelCluster read FNewestLight;
    procedure StrobeBlackout(b: boolean);
    property BlackOutRef: integer read FStrobeBlackout;
    procedure ClearBlackout();
    procedure DefineChannel(iChannel_1Based_FromDocumentation: integer;
      sDescription: string; sDetails: string = '');
    procedure GetAspectList<T: TDMXObject>(l: TUniverseList<T>); overload;
    function GetAspectList<T: TDMXObject>(): TUniverseList<T>; overload;
    function GetAspectInterfaceList<T: IDMXInterface>(guid: TGUID)
      : TUniverseList<T>; overload;
    procedure GetAspectInterfaceList<T: IDMXInterface>(l: TUniverseList<T>;
      guid: TGUID); overload;
    procedure NewScene;
    procedure DoNewScene; virtual;
    property ArtNetEndpoint: TArtNetEndpoint read FArtNetEndPOint
      write SetArtNetEndpoint;
    property ArtNetIP: string read FArtNetIP write SetArtNetIP;
    property Transport: TDMXTransport read FTransport write FTransport;
    procedure TryResolveArtNetIP;
    function BaseChannel0Based: ni;
  end;

  TDMXColorMacroChannel = class(TDMXChannelCluster, IDMXColor, IDMXBlackOut)
  private
    function GetColorMix: NativeFloat;
    procedure SetColorMix(const Value: NativeFloat);

  protected
    FPallete: array of TColor;
    FIndicies: array of byte;
    FColorNumber: integer;
    Fblackout: NativeFloat;
    FBlackoutColor: integer;
    FSourceColor: TColor;
    FTargetColor: TColor;
    FColorMix: NativeFloat;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor); virtual;
    function GetColornumber: byte;
    procedure SetTargetColor(const Value: TColor); virtual;
    procedure WriteDMXData; override;
    procedure SetColorNumber(const Value: byte); virtual;
    procedure SetBlackout(const b: NativeFloat);
    function GetBlackout: NativeFloat;
    function IndexOfColor(c: TColor): integer;
    function GetTargetColor: TColor;
  public
    constructor Create; override;
    property Color: TColor read GetColor write SetTargetColor;
    property ColorNumber: byte read GetColornumber write SetColorNumber;
    procedure AddtoPallete(idx: integer; c: TColor);
    property Blackout: NativeFloat read GetBlackout write SetBlackout;
    procedure Defaults; override;

    procedure SaveSourceColor;
    procedure SetSourceColor(const c: TColor);
    function GetSourceColor: TColor;
    property SourceColor: TColor read GetSourceColor write SetSourceColor;
    property TargetColor: TColor read GetTargetColor write SetTargetColor;
    property ColorMix: NativeFloat read GetColorMix write SetColorMix;
    procedure MixColors;
    procedure DoNewScene; override;


  end;

  TDMXNucleusColorMacroChannel = class(TDMXColorMacroChannel)
  private
  protected
  public
    constructor Create; override;
  end;

  TDMXVueColorMacroChannel = class(TDMXColorMacroChannel)
  private
  protected
  public
    constructor Create; override;
  end;

  TDMXElanColorMacroChannel = class(TDMXColorMacroChannel)
  private
  protected
    procedure SetColorNumber(const Value: byte); override;
    procedure WriteDMXData; override;
  public
    constructor Create; override;
  end;

  TDMXScorpionColorMacroChannel = class(TDMXColorMacroChannel)
  private
  protected
  public
    constructor Create; override;
  end;

  TDMXRGBCluster = class(TDMXChannelCluster, IDMXColor, IDMXBlackOut,
    IDMXIntensity, IDMXColorMixer, IDMXColorAndIntensity)
  private
    FMaxIntensity: NativeFloat;
    FMinIntensity: NativeFloat;
    FFIntensity: NativeFloat;
    FBurnRate: NativeFloat;
    fcoolRate: NativeFloat;
    FChannelSpacing: integer;
    FRelativeLumens: NativeFloat;
    FSTrobeOverdrive: boolean;
    FTargetColor: TColor;
    FColorMix: NativeFloat;
    procedure SetFFIntensity(const Value: NativeFloat);
    function GetburnRate: NativeFloat;
    function GetcoolRate: NativeFloat;
    procedure SetBurnRate(const Value: NativeFloat);
    procedure SetCoolRate(const Value: NativeFloat);
    function GEtMinIntensity: NativeFloat;
    procedure SetMinIntensity(const Value: NativeFloat);
    function GEtMaxIntensity: NativeFloat;
    procedure SetMaxIntensity(const Value: NativeFloat);
    procedure SetChannelspacing(const Value: integer);
    function GetStrobeOverdrive: boolean;
    procedure SetStrobeOverdrive(const Value: boolean);
    function GetTargetIntensity: NativeFloat;
    procedure SetTargetIntensity(const Value: NativeFloat);
    function GetTargetColor: TColor;
    procedure SetTargetColor(const Value: TColor);
    function GetColorMix: NativeFloat;
    procedure SetColorMix(const Value: NativeFloat);
  protected
    Fblackout: NativeFloat;
    FTargetIntensity: NativeFloat;
    FOverDrive: NativeFloat;
    FR, FG, FB: single;
    FRa, FGa, FBa: byte;
    FSourceColor: TColor;
    function GetB: single;
    function GetG: single;
    function GetR: single;
    procedure SetB(const Value: single);
    procedure SetG(const Value: single);
    procedure SetR(const Value: single);
    function GetColor: TColor; virtual;
    procedure SetColor(const Value: TColor); virtual;
    procedure SetIntensity(i: NativeFloat);
    function GetIntensity: NativeFloat;
    procedure SetBlackout(const b: NativeFloat); virtual;
    function GetBlackout: NativeFloat; virtual;
    procedure WriteDMXData; override;
    procedure UpdatePhysics(enginetime: int64); override;
    function GetLumens: NativeFloat;
    procedure SetLumens(const Value: NativeFloat);
    procedure SetTestParams(bTesting: boolean); override;
    procedure Init; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property R: single read GetR write SetR;
    property G: single read GetG write SetG;
    property b: single read GetB write SetB;
    property Color: TColor read GetColor write SetColor;
    property Blackout: NativeFloat read GetBlackout write SetBlackout;
    property Intensity: NativeFloat read GetIntensity write SetIntensity;
    property TargetIntensity: NativeFloat read GetTargetIntensity
      write SetTargetIntensity;
    property FIntensity: NativeFloat read FFIntensity write SetFFIntensity;
    property BURNRATE: NativeFloat read GetburnRate write SetBurnRate;
    property COOLRATE: NativeFloat read GetcoolRate write SetCoolRate;
    property MinIntensity: NativeFloat read GEtMinIntensity
      write SetMinIntensity;
    property MaxIntensity: NativeFloat read GEtMaxIntensity
      write SetMaxIntensity;
    property ChannelSpacing: integer read FChannelSpacing
      write SetChannelspacing;
    procedure Defaults; override;
    property RelativeLumens: NativeFloat read GetLumens write SetLumens;
    property StrobeOverdrive: boolean read GetStrobeOverdrive
      write SetStrobeOverdrive;

    procedure SaveSourceColor;
    procedure SetSourceColor(const c: TColor);
    function GetSourceColor: TColor;
    property SourceColor: TColor read GetSourceColor write SetSourceColor;
    property TargetColor: TColor read GetTargetColor write SetTargetColor;
    property ColorMix: NativeFloat read GetColorMix write SetColorMix;
    procedure MixColors;
    procedure DoNewScene; override;
  end;

  TDMXRGBWcluster = class(TDMXRGBCluster, IDMXColor, IDMXBlackOut,
    IDMXIntensity, IDMXColorMixer, IDMXColorAndIntensity)
  private
    FWRAtio: NativeFloat;
  protected
    FW: single;
    function GetW: single;
    procedure SetW(const Value: single);
    function GetColor: TColor; override;
    procedure SetColor(const Value: TColor); override;
    procedure WriteDMXData; override;
    procedure Init; override;
  public
    constructor Create; override;

    property W: single read GetW write SetW;
    property RelativeWIntensity: NativeFloat read FWRAtio write FWRAtio;

  end;

  TDMXRGBICluster = class(TDMXRGBCluster)
  protected
    procedure Init; override;
  public
    procedure WriteDMXData; override;
    constructor Create; override;
  end;

  TDMXchauvetPar56 = class(TDMXRGBICluster)
  public

  end;

  IDMXTiltPan = interface
    ['{F6843C3F-278B-401F-807E-61DF3354A3DA}']
    function GetPan: integer;
    function GetTilt: integer;
    procedure SetPan(Value: integer);
    procedure SetTilt(Value: integer);
    function GetPanOffset: NativeFloat;
    function GetTiltOffset: NativeFloat;
    procedure SetPanOffset(const Value: NativeFloat);
    procedure SetTiltOffset(const Value: NativeFloat);
    function GetPanF: NativeFloat;
    function GetTiltF: NativeFloat;
    procedure SetPanF(const Value: NativeFloat);
    procedure SetTiltF(const Value: NativeFloat);
    function GetPanTiltRatio: NativeFloat;
    procedure SetPanTiltRatio(const Value: NativeFloat);
    property Pan: integer read GetPan write SetPan;
    property Tilt: integer read GetTilt write SetTilt;
    property TiltF: NativeFloat read GetTiltF write SetTiltF;
    property PanF: NativeFloat read GetPanF write SetPanF;
    property TiltOffset: NativeFloat read GetTiltOffset write SetTiltOffset;
    property PanOffset: NativeFloat read GetPanOffset write SetPanOffset;
    property PanTiltRatio: NativeFloat read GetPanTiltRatio
      write SetPanTiltRatio;
  end;

  TDMXTiltPan = class(TDMXChannelCluster, IDMXTiltPan)
  private
    FPanF: NativeFloat;
    FTiltF: NativeFloat;
    FtiltOffset: NativeFloat;
    FPanOffset: NativeFloat;

    function GetPan: integer;
    function GetTilt: integer;
    procedure SetPan(Value: integer);
    procedure SetTilt(Value: integer);
    function GetPanOffset: NativeFloat;
    function GetTiltOffset: NativeFloat;
    procedure SetPanOffset(const Value: NativeFloat);
    procedure SetTiltOffset(const Value: NativeFloat);
    function GetPanF: NativeFloat;
    function GetTiltF: NativeFloat;
    procedure SetPanF(const Value: NativeFloat);
    procedure SetTiltF(const Value: NativeFloat);
    function GetPanTiltRatio: NativeFloat;
    procedure SetPanTiltRatio(const Value: NativeFloat);
  protected
    FMaxPanValue: nativeint;
    // <this is important for legacy integer conversion... .override.
    FMaxTiltValue: nativeint;
    // <this is important for legacy integer conversion... .override.
    FPAnTiltRatio: NativeFloat;

    procedure Init; override;

  public
    procedure WriteDMXData; override;
    property Pan: integer read GetPan write SetPan;
    property Tilt: integer read GetTilt write SetTilt;
    property TiltF: NativeFloat read GetTiltF write SetTiltF;
    property PanF: NativeFloat read GetPanF write SetPanF;
    property TiltOffset: NativeFloat read GetTiltOffset write SetTiltOffset;
    property PanOffset: NativeFloat read GetPanOffset write SetPanOffset;
    property PanTiltRatio: NativeFloat read GetPanTiltRatio
      write SetPanTiltRatio;
    procedure DoNewScene; override;
  end;

  TDMXCoarseMirror = class(TDMXTiltPan)
  public
    constructor Create;override;
    procedure Init; override;
    procedure WriteDMXData; override;
  end;

  TDMXDFineMirror = class(TDMXTiltPan)
  public
    constructor Create;override;
    procedure Init; override;
    procedure WriteDMXData; override;
  end;

  TDMXFineHeadMotion = TDMXDFineMirror;
  TDMXCoarseHeadMotion = TDMXCoarseMirror;

  TDMxThread = class; // forward

  TAnonLightOp<T: IDMXInterface> = reference to procedure(l: T; idx: ni);

  TDMXUniverse = class(TDMXChannelCluster)
  private
    FHost: string;
    FDebugMode: boolean;
    function GetData: PByte;
    function GetSourceData: PByte;
    function GetTargetData: PByte;
    procedure SetHost(const Value: string);
  protected
    FData: array [0 .. (512 * 2)] of byte;
    FArtNetData: array [0 .. (511 + 8)] of byte;
    FSourceData: array [0 .. (512 * 2)] of byte;
    FTargetData: array [0 .. (512 * 2)] of byte;

    FRemote: boolean;
    sect: _RTL_Critical_Section;
    h: pointer;
    udpc: web.win.sockets.TUdpSocket;
  public
    constructor Create(sHost: string = ''); reintroduce; virtual;
    destructor Destroy; override;
    procedure AfterAddLight; override;
    procedure Update;
    property Data: PByte read GetData;
    property SourceData: PByte read GetSourceData;
    property TargetData: PByte read GetTargetData;
    procedure Wipe;

    procedure Lock;
    procedure Unlock;
    procedure DebugChannelRange(iStart: integer; iCount: integer);

    property Remote: boolean read FRemote;
    property Host: string read FHost write SetHost;
    property DebugMode: boolean read FDebugMode write FDebugMode;
    procedure DebugByte(chan: nativeint; val: byte);

    procedure Operate<T: IDMXInterface>(guid: TGUID; sGroup: string;
      op: TAnonLightOp<T>);


    procedure DoNewScene; override;
  end;

  TDMXMultiVerse = class(TSharedList<TDMXUniverse>)
  private
    Fjoys: array [0 .. 1] of TDMXExternalJoyControl;
    FLiveMode: boolean;
    FStage: TDMXStage;
    Fthr: TDMxThread;

    FEffects: TDMXEffects;
    FEffectsGroups: array [0 .. 5] of TDMXEffects;
    FEffectLibrary: TEffectLibrary;
    FDefaultCool: NativeFloat;
    FDefaultColorFade: NativeFloat;
    FDefaultBurn: NativeFloat;
    FupdatesPaused: boolean;
    FGroups: TDMXLightGroups;
    fFlatLights: TDMXChannelClusterList;
    function GetJoys(idx: integer): TDMXExternalJoyControl;
    function GetEffectsGroup(idx: ni): TDMXEffects;

  public
    procedure Start;
    procedure Stop;

    constructor Create;override;
    procedure Detach;override;
    procedure Update;
    function FindArtNet(ip1, ip2, ip3, ip4: byte): TDMXUniverse;
    property Groups: TDMXLightGroups read FGroups;
    property Joys[idx: integer]: TDMXExternalJoyControl read GetJoys;
    property Stage: TDMXStage read FStage;
    property LiveMode: boolean read FLiveMode write FLiveMode;
    property Effects: TDMXEffects read FEffects;
    property EffectLibrary: TEffectLibrary read FEffectLibrary;
    property DefaultCool: NativeFloat read FDefaultCool write FDefaultCool;
    property DefaultBurn: NativeFloat read FDefaultBurn write FDefaultBurn;
    property Defaultcolorfade: NativeFloat read FDefaultColorFade write FDefaultColorFade;
    property EffectsGroups[idx: ni]: TDMXEffects read GetEffectsGroup;
    procedure UpdateEffects(iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single);

    procedure Pauseupdates;
    procedure ResumeUpdates;

    property FlatLights: TDMXChannelClusterList read FFlatLights;

    function GetSupportingList<i: IDMXInterface>(guid: TGUID;
      iGROUPMAsk, iGroupResult: integer): TList<i>;

  end;

  TDMXColorDashByDelegate = class(TDMXChannelCluster, IDMXColor)
  private
    FColorComponent: TDMXRGBCluster;
  protected
    procedure Channelchanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ColorComponent: TDMXRGBCluster read FColorComponent
      write FColorComponent implements IDMXColor;
  end;

  TDMXcolorBarSection = class(TDMXRGBCluster)
  private
    FSectionIndex: nativeint;
  public
    property SectionIndex: nativeint read FSectionIndex write FSectionIndex;
  end;

  TDMXColorBar = class(TDMXChannelCluster)
  protected
    FSections: TList<TDMXcolorBarSection>;
    function GetSection(idx: integer): TDMXcolorBarSection;
    procedure Channelchanged; override;
    procedure WriteDMXData; override;
    procedure CoordinateUpdated; override;

  public
    constructor Create; override;
    procedure Detach; override;
    destructor Destroy; override;
    procedure Init; override;

    property Sections[idx: integer]: TDMXcolorBarSection read GetSection;
  end;

  TDMXPixel = class(TDMXRGBCluster)
  public
  end;

  TLightOrientation = (loNormal, lo90Left, lo90Right, lo180);

  TDMXPixelWall = class(TDMXChannelCluster, IDMXStrobe)
  private
    FPixelOffset: TPoint;
    function GetPixelBase(X, Y: ni): TDMXPixel;
    function GetPixelLinear(idx: ni): TDMXPixel;
    function GetPixel(X, Y: ni): TDMXPixel;
    function GetDimensions: TPoint;
    procedure SetOrientation(const Value: TLightOrientation);
  protected
    FStrobeSpeed: nativefloat;
    FStrobeIntensity: nativefloat;
    FDimensionsBase: TPoint;
    Forientation: TLightOrientation;
    FPixels: TList<TDMXPixel>;
    tmLastStrobeEvent: ticker;
    strobestate: boolean;
    FStrobeEnable: boolean;

    procedure Channelchanged; override;
    procedure WriteDMXData; override;
    procedure CoordinateUpdated; override;

    procedure DefineDimensions; virtual; abstract;
    function GetPixelCount: ni;
    procedure OrientationChanged;virtual;

    function GetStrobeSpeed: NativeFloat;
    procedure SetStrobeSpeed(const Value: NativeFloat);
    function GetSTrobeIntensity: NativeFloat;
    procedure SetStrobeIntensity(const Value: NativeFloat);

    function GetSTrobeEnable: boolean;
    procedure SetStrobeEnable(const Value: boolean);
    procedure UpdatePhysics(enginetime: Int64); override;


  public

    constructor Create; override;
    procedure Detach; override;
    destructor Destroy; override;

    property pixels_linear[idx: ni]: TDMXPixel read GetPixelLinear;
    property pixels_base[X, Y: ni]: TDMXPixel read GetPixelBase;
    property pixels[X, Y: ni]: TDMXPixel read GetPixel;
    property PixelCount: ni read GetPixelCount;
    property Dimensions: TPoint read GetDimensions;
    property orientation: TLightOrientation read FOrientation write SetOrientation;
    property PixelOffset: TPoint read FPixelOffset write FPixelOffset;

    property StrobeSpeed: NativeFloat read GetStrobeSpeed write SetStrobeSpeed;
    procedure StrobeStrike;

    property StrobeEnable: boolean read GetSTrobeEnable write SetStrobeEnable;
    property StrobeIntensity: NativeFloat read GetSTrobeIntensity write SetStrobeIntensity;


  end;

  TDMXPixellicious = class(TDMXPixelWall)
  protected
    procedure DefineDimensions; override;
  public

  end;

  TDMXPixellicious2 = class(TDMXPixelWall)
  protected
    procedure DefineDimensions; override;
  public

  end;

  TDMXBlueGalaxy = class(TDMXChannelCluster)
  private
    FControlMode: integer;
    FParam2: integer;
    FParam3: integer;
    FParam1: integer;
    FParam4: integer;
    FIntensity: integer;
    FParam6: integer;
    FParam7: integer;
    FParam5: integer;
  protected
    Fcolor: TDMXScorpionColorMacroChannel;
    procedure Channelchanged; override;
    procedure WriteDMXData; override;

  public
    constructor Create; override;
    procedure Detach; override;
    destructor Destroy; override;
    procedure Init; override;
    property Intensity: integer read FIntensity write FIntensity;
    property Param1: integer read FParam1 write FParam1;
    property Param2: integer read FParam2 write FParam2;
    property Param3: integer read FParam3 write FParam3;
    property Param4: integer read FParam4 write FParam4;
    property Param5: integer read FParam5 write FParam5;

  end;

  TDMXScorpion = class(TDMXChannelCluster, IDMXColor)
  private
    FControlMode: integer;
    FParam2: integer;
    FParam3: integer;
    FParam1: integer;
    FParam4: integer;
  protected
    Fcolor: TDMXScorpionColorMacroChannel;
    procedure Channelchanged; override;
    procedure WriteDMXData; override;
    procedure Init; override;
  public
    constructor Create; override;
    procedure Detach; override;
    destructor Destroy; override;
    property controlmode: integer read FControlMode write FControlMode;
    property ColorChannel: TDMXScorpionColorMacroChannel read Fcolor
      implements IDMXColor;
    property Param1: integer read FParam1 write FParam1;
    property Param2: integer read FParam2 write FParam2;
    property Param3: integer read FParam3 write FParam3;
    property Param4: integer read FParam4 write FParam4;
    PROCEDURE DoNewScene; override;
  end;

  TDMXCalienteColorMacroChannel = class(TDMXColorMacroChannel)
  public
    constructor Create; override;
  end;

  TDMXCaliente = class(TDMXChannelCluster)
  private
    FPattern: byte;
    FScanSpeed: NativeFloat;
    FAnimationSpeed: NativeFloat;
    FColorNumber: byte;
    FPatternType: byte;
    FSize: NativeFloat;
  protected
    Fcolor: TDMXCalienteColorMacroChannel;
    FPanTilt: TDMXCoarseMirror;
    procedure WriteDMXData; override;
    procedure Channelchanged; override;
    procedure Init; override;
  public

    constructor Create; override;
    procedure Detach; override;
    destructor Destroy; override;
    property ColorNumber: byte read FColorNumber write FColorNumber;
    property PatternType: byte read FPatternType write FPatternType;
    property Pattern: byte read FPattern write FPattern;
    property ScanSpeed: NativeFloat read FScanSpeed write FScanSpeed;
    property AnimationSpeed: NativeFloat read FAnimationSpeed
      write FAnimationSpeed;
    property Size: NativeFloat read FSize write FSize;
    procedure DoNewScene; override;
  end;

  TDMX4Banger = class(TDMXChannelCluster)
  private
    FPatternType: byte;
    FPattern: byte;
    FScanSpeed: NativeFloat;
    FSize: NativeFloat;
    FAnimationSpeed: NativeFloat;
    FColorNumber: byte;
  protected
    procedure WriteDMXData; override;
    procedure Channelchanged; override;
    procedure Init; override;
  public

    constructor Create; override;
    procedure Detach; override;
    destructor Destroy; override;
    property ColorNumber: byte read FColorNumber write FColorNumber;
    property PatternType: byte read FPatternType write FPatternType;
    property Pattern: byte read FPattern write FPattern;
    property ScanSpeed: NativeFloat read FScanSpeed write FScanSpeed;
    property AnimationSpeed: NativeFloat read FAnimationSpeed
      write FAnimationSpeed;
    property Size: NativeFloat read FSize write FSize;
    procedure DoNewScene; override;
  end;

  TDMXSkyLaser = class(TDMXChannelCluster)
  private
    FControlMode: integer;
    FParam2: integer;
    FParam3: integer;
    FParam1: integer;
    FParam4: integer;
    FIntensity: integer;
  protected
    Fcolor: TDMXScorpionColorMacroChannel;
    procedure Channelchanged; override;
    procedure WriteDMXData; override;
    procedure Init; override;

  public
    constructor Create; override;
    procedure Detach; override;
    destructor Destroy; override;
    property Intensity: integer read FIntensity write FIntensity;
    property Param1: integer read FParam1 write FParam1;
    property Param2: integer read FParam2 write FParam2;
    property Param3: integer read FParam3 write FParam3;
    property Param4: integer read FParam4 write FParam4;
    procedure DoNewScene; override;
  end;

  TDMXvue = class(TDMXVueColorMacroChannel)
  private
    FMacroMode: integer;
    FChannelUsage: nativeint;
  protected
    FSpeed: integer;
    procedure Channelchanged; override;
    procedure WriteDMXData; override;
  public
    constructor Create; override;
    procedure Detach; override;
    procedure Init; override;
    destructor Destroy; override;
    property controlmode: integer read FColorNumber write FColorNumber;
    property MacroMode: integer read FMacroMode write FMacroMode;
    property speed: integer read FSpeed write FSpeed;
    procedure DoNewScene; override;
  end;

  TDMXUFO = class(TDMXChannelCluster)
  private
    FColorMode: integer;
    FControlMode: integer;
  protected
    procedure WriteDMXData; override;
  public
    constructor Create; override;
    property controlmode: integer read FControlMode write FControlMode;
    property ColorMode: integer read FColorMode write FColorMode;
    procedure DoNewScene; override;
  end;

  TDMXNucleus = class(TDMXChannelCluster)
  private
    FControlMode: integer;
    FMirrors: TList<TDMXCoarseMirror>;
    Fcolors: TList<TDMXNucleusColorMacroChannel>;

  protected
    procedure WriteDMXData; override;
    procedure Channelchanged; override;
    procedure CoordinateUpdated; override;
    procedure Init; override;

  public
    constructor Create; override;
    destructor Destroy; override;
    property controlmode: integer read FControlMode write FControlMode;
    property Colors: TList<TDMXNucleusColorMacroChannel> read Fcolors;
    property Mirrors: TList<TDMXCoarseMirror> read FMirrors;
    procedure DoNewScene; override;
  end;

  TDMxThread = class(TManagedThread)
  public
    procedure DoExecute; override;
  end;

  TDMXElan = class(TDMXElanColorMacroChannel)
  public
    constructor Create; override;
    procedure DoNewScene; override;
  end;

  TDMXStrobe = class(TDMXChannelCluster, IDMXStrobe)
  private
    procedure SetStrobeSpeed(const Value: NativeFloat);
    function GetStrobeSpeed: NativeFloat;
  protected
    FSTrobeIntensity: NativeFloat;
    FSTrobeSpeed: NativeFloat;
    FSTrobeStrike: boolean;
    FStrobeEnable: boolean;
    procedure WriteDMXData; override;
    function GetSTrobeIntensity: NativeFloat;
    procedure SetStrobeIntensity(const Value: NativeFloat);
    function GetSTrobeEnable: boolean;
    procedure SetStrobeEnable(const Value: boolean);

  public
    constructor Create; override;

    property StrobeIntensity: NativeFloat read GetSTrobeIntensity
      write SetStrobeIntensity;
    property StrobeSpeed: NativeFloat read GetStrobeSpeed write SetStrobeSpeed;
    property StrobeEnable: boolean read GetSTrobeEnable write SetStrobeEnable;

    procedure StrobeStrike;
    procedure DoNewScene; override;
  end;

  TDMXColorDashAccent = class(TDMXRGBWcluster)
  public
    constructor Create; override;
  end;

  TDMXColorDash = class(TDMXRGBCluster);

  TDMXcolor7 = class(TDMXRGBCluster)
  public
    procedure WriteDMXData; override;
  end;

  TDMXPro38B = class(TDMXRGBCluster)
  public
    constructor Create; override;
  end;

  TDMXOptiTri = class(TDMXRGBCluster)
  public
  public
    constructor Create; override;
  end;

  TDMXQspotColorChannel = class(TDMXColorMacroChannel)
  public
    constructor Create; override;
  end;

  TDMXAccuSpot250ColorChannel = class(TDMXColorMacroChannel)
  public
    constructor Create; override;
  end;

  TDMXSpotBarColorChannel = class(TDMXColorMacroChannel)
  protected
    procedure SetTargetColor(const Value: TColor); override;
  public

    constructor Create; override;
  end;

  TDMXFusionBar = class(TDMXChannelCluster)
  private
    FEffect: integer;
    FControlMode: integer;
    procedure SetControlMode(const Value: integer);
  protected
    procedure WriteDMXData; override;
    procedure Channelchanged; override;
    procedure CoordinateUpdated; override;
    procedure DisableSpots;
    procedure EnableSpots;
  public
    spots: array [0 .. 3] of TDMXSpotBarColorChannel;
    wash: TDMXRGBCluster;
    constructor Create; override;
    destructor Destroy; override;
    property Effect: integer read FEffect write FEffect;
    property controlmode: integer read FControlMode write SetControlMode;
    procedure DoNewScene; override;
  end;

  TDMXFlurryQ = class(TDMXChannelCluster, IDMXIntensity, IDMXColor, IDMXTiltPan,
    IDMXStrobe)
  private
    FPanTilt: TDMXFineHeadMotion;
    Fcolor: TDMXRGBWcluster;
    FSTrobeSpeed: NativeFloat;
    FAutoMode: boolean;
    FStrobeEnable: boolean;
  protected
    procedure WriteDMXData; override;
    procedure Channelchanged; override;
    procedure CoordinateUpdated; override;
    procedure Init; override;
    function GetStrobeSpeed: NativeFloat; // STROBE SPEED
    procedure SetStrobeSpeed(const Value: NativeFloat);
    function GetSTrobeIntensity: NativeFloat; // STROBE INTENSITY
    procedure SetStrobeIntensity(const Value: NativeFloat);
    function GetSTrobeEnable: boolean;
    procedure SetStrobeEnable(const Value: boolean);

  public
    constructor Create; override;
    property Color: TDMXRGBWcluster read Fcolor implements IDMXColor,
      IDMXIntensity;
    property PanTilt: TDMXFineHeadMotion read FPanTilt implements IDMXTiltPan;

    property Intensity: NativeFloat read GetSTrobeIntensity
      write SetStrobeIntensity;
    property StrobeSpeed: NativeFloat read GetStrobeSpeed write SetStrobeSpeed;
    procedure Strike;
    property AutoMode: boolean read FAutoMode write FAutoMode;

    procedure StrobeStrike;
    property StrobeEnable: boolean read GetSTrobeEnable write SetStrobeEnable;

    procedure DoNewScene; override;
  end;

  TDMXAccuSpot250 = class(TDMXChannelCluster, IDMXIntensity, IDMXTiltPan,
    IDMXStrobe)
  private
    Fgobo: byte;
    FgoboSpin: NativeFloat;
    FIntensity: NativeFloat;
    FTargetIntensity: NativeFloat;
    FMaxIntensity: NativeFloat;
    FMinIntensity: NativeFloat;
    FBurnRate: NativeFloat;
    fcoolRate: NativeFloat;
    fFocus: byte;
    FSpeed: single;
    FStrobe: integer;
    FSTrobeSpeed: single;
    FStrokeSTrike: boolean;
  protected
    FMotion: TDMXCoarseHeadMotion;
    Fcolor: TDMXAccuSpot250ColorChannel;
    FSTrobeOverdrive: boolean;
    FRelativeLumens: NativeFloat;
    FReset: boolean;
    FResetTime: int64;
    function GetIntensity: NativeFloat;
    procedure SetIntensity(Value: NativeFloat);
    function GetTargetIntensity: NativeFloat;
    procedure SetTargetIntensity(const Value: NativeFloat);
    function GEtMaxIntensity: NativeFloat;
    function GEtMinIntensity: NativeFloat;
    procedure SetMaxIntensity(const Value: NativeFloat);
    procedure SetMinIntensity(const Value: NativeFloat);

    function GetSTrobeEnable: boolean;
    procedure SetStrobeEnable(const Value: boolean);

    function GetStrobeSpeed: NativeFloat;
    procedure SetStrobeSpeed(const Value: NativeFloat);
    function GetSTrobeIntensity: NativeFloat;
    procedure SetStrobeIntensity(const Value: NativeFloat);

    procedure WriteDMXData; override;
    procedure Channelchanged; override;
    procedure CoordinateUpdated; override;
    procedure UpdatePhysics(enginetime: int64); override;
    procedure SetLumens(const i: NativeFloat);
    function GetLumens: NativeFloat;
    function GetStrobeOverdrive: boolean;
    procedure SetStrobeOverdrive(const Value: boolean);
    function GetgoboSPinValue: byte;

  public
    constructor Create; override;
    destructor Destroy; override;

    property Intensity: NativeFloat read GetIntensity write SetIntensity;
    property MinIntensity: NativeFloat read GEtMinIntensity
      write SetMinIntensity;
    property MaxIntensity: NativeFloat read GEtMaxIntensity
      write SetMaxIntensity;
    property speed: single read FSpeed write FSpeed;
    property Gobo: byte read Fgobo write Fgobo;

    property GoboSpin: NativeFloat read FgoboSpin write FgoboSpin;
    property Motion: TDMXCoarseHeadMotion read FMotion implements IDMXTiltPan;
    property Color: TDMXAccuSpot250ColorChannel read Fcolor;
    function GetburnRate: NativeFloat;
    function GetcoolRate: NativeFloat;
    procedure SetBurnRate(const Value: NativeFloat);
    procedure SetCoolRate(const Value: NativeFloat);
    property BURNRATE: NativeFloat read GetburnRate write SetBurnRate;
    property COOLRATE: NativeFloat read GetcoolRate write SetCoolRate;

    property Strobe: integer read FStrobe write FStrobe;

    property RelativeLumens: NativeFloat read GetLumens write SetLumens;
    property StrobeOverdrive: boolean read GetStrobeOverdrive
      write SetStrobeOverdrive;
    procedure Reset;
    procedure StrobeStrike;

  end;

  TDMXQSpot14 = class(TDMXChannelCluster, IDMXIntensity, IDMXTiltPan,
    IDMXStrobe)
  private
    Fgobo2, Fgobo1: byte;
    Fgobochannel2, Fgobochannel1: byte;
    FgoboSpin: NativeFloat;
    FPrism: byte;
    FIntensity: NativeFloat;
    FTargetIntensity: NativeFloat;
    FMaxIntensity: NativeFloat;
    FMinIntensity: NativeFloat;
    FBurnRate: NativeFloat;
    fcoolRate: NativeFloat;
    fFocus: byte;
    FPrismSpin: NativeFloat;
    FSpeed: single;
    FStrobe: integer;
    function GetIntensity: NativeFloat;
    procedure SetIntensity(Value: NativeFloat);
    function GetTargetIntensity: NativeFloat;
    procedure SetTargetIntensity(const Value: NativeFloat);
    procedure StrobeStrike;

    function GEtMaxIntensity: NativeFloat;
    function GEtMinIntensity: NativeFloat;
    procedure SetMaxIntensity(const Value: NativeFloat);
    procedure SetMinIntensity(const Value: NativeFloat);
    procedure SetGobo1(const Value: byte);
    procedure SetGobo2(const Value: byte);
    procedure Calc_GoboChannel1Value;
    procedure Calc_GoboChannel2Value;
    procedure SetPrismSpin(const Value: NativeFloat);
  protected
    FMotion: TDMXFineHeadMotion;
    Fcolor: TDMXQspotColorChannel;
    FSTrobeOverdrive: boolean;
    FRelativeLumens: NativeFloat;
    FReset: boolean;
    FResetTime: int64;
    function GetSTrobeEnable: boolean;
    procedure SetStrobeEnable(const Value: boolean);

    function GetStrobeSpeed: NativeFloat;
    procedure SetStrobeSpeed(const Value: NativeFloat);
    function GetSTrobeIntensity: NativeFloat;
    procedure SetStrobeIntensity(const Value: NativeFloat);

    procedure WriteDMXData; override;
    procedure Channelchanged; override;
    procedure CoordinateUpdated; override;
    procedure UpdatePhysics(enginetime: int64); override;
    function GetgoboSPinValue: byte;
    procedure SetLumens(const i: NativeFloat);
    function GetLumens: NativeFloat;
    function GetStrobeOverdrive: boolean;
    procedure SetStrobeOverdrive(const Value: boolean);

  public
    constructor Create; override;
    destructor Destroy; override;

    property Intensity: NativeFloat read GetIntensity write SetIntensity;
    property MinIntensity: NativeFloat read GEtMinIntensity
      write SetMinIntensity;
    property MaxIntensity: NativeFloat read GEtMaxIntensity
      write SetMaxIntensity;
    property GoboChannel1: byte read Fgobochannel1 write Fgobochannel1;
    property GoboChannel2: byte read Fgobochannel2 write Fgobochannel2;
    property speed: single read FSpeed write FSpeed;
    property Gobo1: byte read Fgobo1 write SetGobo1;
    property Gobo2: byte read Fgobo2 write SetGobo2;
    property Focus: byte read fFocus write fFocus;

    property Prism: byte read FPrism write FPrism;
    property GoboSpin: NativeFloat read FgoboSpin write FgoboSpin;
    property Motion: TDMXFineHeadMotion read FMotion implements IDMXTiltPan;
    property Color: TDMXQspotColorChannel read Fcolor;
    function GetburnRate: NativeFloat;
    function GetcoolRate: NativeFloat;
    procedure SetBurnRate(const Value: NativeFloat);
    procedure SetCoolRate(const Value: NativeFloat);
    property BURNRATE: NativeFloat read GetburnRate write SetBurnRate;
    property COOLRATE: NativeFloat read GetcoolRate write SetCoolRate;
    property PrismSpin: NativeFloat read FPrismSpin write SetPrismSpin;
    property Strobe: integer read FStrobe write FStrobe;

    property RelativeLumens: NativeFloat read GetLumens write SetLumens;
    property StrobeOverdrive: boolean read GetStrobeOverdrive
      write SetStrobeOverdrive;
    procedure Reset;

    procedure DoNewScene; override;

  end;

  TDMXObjectWithUniverse = class(TObservableObject)
  end;

  TDMXExternalControl = class(TDMXObjectWithUniverse)
  public
  end;

  TDMXExternalJoyControl = class(TDMXObjectWithUniverse)
  var
    FPan, FTilt: NativeFloat;
  private
    FPlane: nativeint;
    procedure SetPan(const Value: NativeFloat);
    procedure SetTilt(const Value: NativeFloat);
  public
    procedure Init; override;
    property Pan: NativeFloat read FPan write SetPan;
    property Tilt: NativeFloat read FTilt write SetTilt;
    property Plane: nativeint read FPlane write FPlane;
  end;

  TEffectParameterTYpe = (eptFloat, eptString);
  TEffectEntryType = (eetSlider, eetHex, eetString);

  TDMXEffectParameterDefinition = class
  private
    function GetasString: string;
    function GetAsInteger: nativeint;
    procedure SetAsInteger(const Value: nativeint);
  public
    Max: NativeFloat;
    Min: NativeFloat;
    Steps: nativeint;
    imptyp: TEffectParameterTYpe;
    entryType: TEffectEntryType;
    Name: string;
    UsesCustomHander: boolean;
    rValue: NativeFloat;
    sValue: string;
    property AsString: string read GetasString;
    property AsInteger: nativeint read GetAsInteger write SetAsInteger;
  end;

  TDMXEffect = class(TDMXObjectWithUniverse)
  private
    FName: string;
    FODUB: nativeint;
    function GEtName: string;
    function GetParamCount: nativeint;
    function GetGroupCount: nativeint;
    function GetGroup(idx: nativeint): TDMXLIghtGroup;
  protected
    FLights: TList<TDMXChannelCluster>;
    FParamDefinitions: TList<TDMXEffectParameterDefinition>;
    FLastUpdateTime: NativeFloat;
    FCurrentTime: NativeFloat;
    FCreationTime: NativeFloat;
    FFirstupdateReceived: boolean;
    FLastTriggerNumber: integer;
    FTriggerNumber: integer;
    FGroups: TList<TDMXLIghtGroup>;
    property FirstUpdateReceived: boolean read FFirstupdateReceived;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddLight(l: TDMXChannelCluster);
    procedure CleanupFX; virtual;
    procedure OnFXDestroyingForLight(l: TDMXChannelCluster); virtual;

    procedure UpdatePhysics(iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single);
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); virtual;
    function IndexOfGroup(sNAme: string): nativeint;
    function HasGroup(sNAme: string): boolean;
    procedure RebuildLightListFromGroups;
    procedure AddGroup(sNAme: string; bLightsOnly: boolean = false);
    procedure RemoveGroup(sNAme: string);
    procedure SEtPAram(sNAme: string; sValue: string); overload;
    procedure SEtPAram(sNAme: string; rValue: NativeFloat); overload;
    procedure SetParamLL(sNAme: string; sValue: string); overload; virtual;
    procedure SetParamLL(sNAme: string; rValue: NativeFloat); overload; virtual;
    procedure GetParam(sNAme: string; out rValue: NativeFloat); overload;
    procedure GetParam(sNAme: string; out sValue: string); overload;
    procedure GetParamLL(sNAme: string; out rValue: NativeFloat);
      overload; virtual;
    procedure GetParamLL(sNAme: string; out sValue: string); overload; virtual;

    function NewTrigger: boolean;
    property Name: string read GEtName;
    property ODUB: nativeint read FODUB write FODUB;
    property ParamCount: nativeint read GetParamCount;

    property ParamDefinitions: TList<TDMXEffectParameterDefinition>
      read FParamDefinitions;
    procedure DefineParameters; virtual;
    procedure ClearLights;
    function GetParamDefinition(sNAme: string): TDMXEffectParameterDefinition;
    property Params[sNAme: string]: TDMXEffectParameterDefinition
      read GetParamDefinition;
    property GroupCount: nativeint read GetGroupCount;
    property Groups[idx: nativeint]: TDMXLIghtGroup read GetGroup;
  end;

  TDMXDimmer = class(TDMXChannelCluster, IDMXIntensity)
  private
    FMinIntensity: NativeFloat;
    FMaxIntensity: NativeFloat;
    FTargetIntensity: NativeFloat;
    FIntensity: NativeFloat;
    FSTrobeOverdrive: boolean;
    FBurnRate: NativeFloat;
    fcoolRate: NativeFloat;
    fLumens: NativeFloat;
    FOverDrive: NativeFloat;
  protected
    procedure WriteDMXData; override;
    function GEtMinIntensity: NativeFloat;
    procedure SetMinIntensity(const Value: NativeFloat);
    function GEtMaxIntensity: NativeFloat;
    procedure SetMaxIntensity(const Value: NativeFloat);
    procedure SetIntensity(i: NativeFloat);
    function GetIntensity: NativeFloat;
    procedure SetTargetIntensity(const i: NativeFloat);
    function GetTargetIntensity: NativeFloat;

    function GetburnRate: NativeFloat;
    function GetcoolRate: NativeFloat;
    procedure SetBurnRate(const Value: NativeFloat);
    procedure SetCoolRate(const Value: NativeFloat);
    property BURNRATE: NativeFloat read GetburnRate write SetBurnRate;
    property COOLRATE: NativeFloat read GetcoolRate write SetCoolRate;
    property MinIntensity: NativeFloat read GEtMinIntensity
      write SetMinIntensity;
    property MaxIntensity: NativeFloat read GEtMaxIntensity
      write SetMaxIntensity;

    procedure SetLumens(const i: NativeFloat);
    function GetLumens: NativeFloat;
    property RelativeLumens: NativeFloat read GetLumens write SetLumens;
    function GetStrobeOverdrive: boolean;
    procedure SetStrobeOverdrive(const Value: boolean);
    property StrobeOverdrive: boolean read GetStrobeOverdrive
      write SetStrobeOverdrive;

    procedure UpdatePhysics(enginetime: int64); override;

  public

    procedure Init; override;
    constructor Create; override;
    property TargetIntensity: NativeFloat read GetTargetIntensity
      write SetTargetIntensity;
    property Intensity: NativeFloat read GetIntensity write SetIntensity;
  end;

  TDMXEffects = class(TUniverseList<TDMXEffect>)
  private
    FNewestEffect: TDMXEffect;
  public
    function Add(sTypeName: string; iODUB: nativeint): TDMXEffect;
    procedure clear;
    PROPERTY NewestEffect: TDMXEffect read FNewestEffect;
    function IndexOfEffect(sTypeName: string; iODUB: nativeint): integer;
    function FindEffect(sTypeName: string; iODUB: nativeint): TDMXEffect;
    procedure DeleteNewest(sTypeName: string; iODUB: nativeint);
  end;

  TFXStrobe = class(TDMXEffect)
  public
    procedure Init; override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;

    procedure DefineParameters; override;

    procedure OnFXDestroyingForLight(l: TDMXChannelCluster); override;

  end;

  TFXStrobeBlackout = class(TFXStrobe)
  public
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;

    procedure OnFXDestroyingForLight(l: TDMXChannelCluster); override;
  end;

  TFXStaticColor = class(TDMXEffect)
  private
    Fcolor: TColor;
    FBurnRate: single;
    FFadeTime: single;
  public
    procedure Init; override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefineParameters; override;
  end;

  TFXGridPong = class(TDMXEffect)
  private
    FBallX, FBallY: NativeFloat;
    FBallVelX, FBallVelY: NativeFloat;
    FPaddle1, FPaddle2: NativeFloat;
    FPV1, FPV2: NativeFloat;
    FBallHist: array [0 .. BALL_HIST_SIZE - 1] of TPoint;
    FHistIdx: nativeint;
  public
    procedure Init; override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;

  end;

  TFXGridFill = class(TDMXEffect)
  private
    FFillX, FFillY: nativeint;
    FRate: nativeint;
    Fcolor: TColor;
    FPass: integer;
    FColor2: TColor;
  public
    procedure Init; override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure SetParamLL(sNAme: string; sValue: string); override;
    property Rate: nativeint read FRate write FRate;
    property Color: TColor read Fcolor write Fcolor;
    property Color2: TColor read FColor2 write FColor2;
  end;

  TFXStaticStripes = class(TDMXEffect)
  private
    FColor1: TColor;
    FColor2: TColor;
    FFadeTime: single;
  public
    procedure Init; override;
    property Color1: TColor read FColor1 write FColor1;
    property Color2: TColor read FColor2 write FColor2;
    property FadeTime: single read FFadeTime write FFadeTime;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure SetParamLL(sNAme: string; sValue: string); override;
  end;

  TFXStaticCoolRate = class(TDMXEffect)
  private
    FApplytime: NativeFloat;
    fcoolRate: single;
  public
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefineParameters; override;
  end;

  TFXStaticIntensity = class(TDMXEffect)
  private
    FIntensity: single;
    fcoolRate: single;
    FBurnRate: single;
  public
    procedure Init; override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefineParameters; override;
  end;

  TDMXLIghtGroup = class(TUniverseList<TDMXChannelCluster>)
  private
    FName: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Name: string read FName write FName;

    function GetList<T: TDMXChannelCluster>: TUniverseList<T>;
  end;

  TDMXLightGroups = class(TUniverseList<TDMXLIghtGroup>)
  private
    FMultiverse: TDMXMultiVerse;
    function GetGroupByName(sNAme: string): TDMXLIghtGroup;
    function GetGroup(idx: integer): TDMXLIghtGroup;
  public
    constructor Create; override;
    function IndexOf(sNAme: string): integer;
    property ByName[sNAme: string]: TDMXLIghtGroup read GetGroupByName; default;
    property Groups[idx: integer]: TDMXLIghtGroup read GetGroup;
    property MultiVerse: TDMXMultiVerse read FMultiverse write FMultiverse;
    function Add(sNAme: string): TDMXLIghtGroup;
    procedure clear;
  end;

  TDMXGelChannel = class(TDMXChannelCluster, IDMXColor)
  private
    Fcolor: TColor;
    FGelColor: TColor;
    FBitPrecision: nativeint;
    FSourceColor: TColor;
    FTargetColor: TColor;
    function GetSourceColor: TColor;
    procedure SetSourceColor(const Value: TColor);
    function GetTargetColor: TColor;
    procedure SetTargetColor(const Value: TColor);
    function GetColorMix: NativeFloat;
    procedure SetColorMix(const Value: NativeFloat);
  protected
    FColorMix: NativeFloat;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
  public
    property Color: TColor read GetColor write SetColor;
    property SourceColor: TColor read GetSourceColor write SetSourceColor;
    property TargetColor: TColor read GetTargetColor write SetTargetColor;
    property GelColor: TColor read FGelColor write FGelColor;
    procedure WriteDMXData; override;
    property BitPrecision: nativeint read FBitPrecision write FBitPrecision;
    procedure SaveSourceColor;
    property ColorMix: NativeFloat read GetColorMix write SetColorMix;
    procedure MixColors;
    procedure DoNewScene; override;
  end;

  TDMXLEDGrid = class(TDMXChannelCluster)
  private
    function GetGrid(X, Y: nativeint): TDMXGelChannel;
  protected
    FGrid: array [0 .. 15] of array [0 .. 15] of TDMXGelChannel;
    procedure CreateGrid;
    procedure DestroyGrid;
    procedure Init; override;
    procedure Channelchanged; override;
    procedure CoordinateUpdated; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Wipe(c: TColor = clBlack);
    procedure WriteDMXData; override;
    property Grid[X, Y: nativeint]: TDMXGelChannel read GetGrid;
  end;

  TDMXPLaneCalibrationPOint = record
    light: TDMXChannelCluster;
    joy: Tnativefloatpoint;
    Pan: Tnativefloatpoint;
    Focus: NativeFloat;
    added: boolean;
  end;

  PDMXPLaneCalibrationPoint = ^TDMXPLaneCalibrationPOint;

  TDMXPLaneLight = record
  private
    function GetNearestPoint(X, Y: NativeFloat): PDMXPLaneCalibrationPoint;
    function FindCCWPoint(operandpoint, nearestpoint: TNativeVector4)
      : PDMXPLaneCalibrationPoint;
    function FindCWPoint(operandpoint, nearestpoint: TNativeVector4)
      : PDMXPLaneCalibrationPoint;
    function GetInterpolatedPoint_Old2(X, Y: NativeFloat): TNativeVector4;
  public
    light: TDMXChannelCluster;
    debug_primarypoint: PDMXPLaneCalibrationPoint;
    debug_influence1: PDMXPLaneCalibrationPoint;
    debug_influence2: PDMXPLaneCalibrationPoint;
    calibrationpoints: array of TDMXPLaneCalibrationPOint;
    procedure AddCalPoint(joyx, joyy, panx, pany, Focus: NativeFloat);
    function GetInterpolatedPoint(X, Y: NativeFloat): TNativeVector4;
    function GetInterpolatedPoint_Old(X, Y: NativeFloat): TNativeVector4;
    procedure DeleteCalPoint(idx: ni);
    procedure DefaultPoints;

  end;

  PDMXPLaneLight = ^TDMXPLaneLight;

  TDMXPlane = class(TBetterObject)
  public
    LIghts: array of TDMXPLaneLight;
    procedure AddLight(l: TDMXChannelCluster);
    function GetLightData(l: TDMXChannelCluster): PDMXPLaneLight;
    function INdexOfLight(l: TDMXChannelCluster): ni;
    procedure LoadData(nvpl: TNameValuePairList; sPrefix: string);
    procedure SaveData(nvpl: TNameValuePairList; sPrefix: string);
    procedure GetLights;

  end;

  TDMXPlanes = class(TSharedList<TDMXPlane>)
  end;

  TDMXStage = class(TSharedOBject)
  public
    planes: TDMXPlanes;
    Universe: TDMXUniverse;
    xybounds: TNativeFloatRect;
    zmax: NativeFloat;
    zmin: NativeFloat;
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadData;
    procedure SaveData;

  end;

var
  FirstUniverse: TDMXUniverse;
  MultiVerse: TDMXMultiVerse;

implementation

{ TDMXRGBCluster }

constructor TDMXRGBCluster.Create;
begin
  inherited;
  FIntensity := 1.0;
  FBurnRate := 32;
  fcoolRate := 16;
  FMaxIntensity := 1.0;
  FChannelSpacing := 1;
  FRelativeLumens := 1.0;
  DefineChannel(1, 'Red', '');
  DefineChannel(2, 'Green', '');
  DefineChannel(3, 'Blue', '');


end;

procedure TDMXRGBCluster.Defaults;
begin
  inherited;
  Intensity := 0;

end;

destructor TDMXRGBCluster.Destroy;
begin

  inherited;
end;

function TDMXRGBCluster.GetB: single;
begin
  result := FB;

end;

function TDMXRGBCluster.GetBlackout: NativeFloat;
begin
  result := Fblackout;
end;

function TDMXRGBCluster.GetburnRate: NativeFloat;
begin
  result := FBurnRate;
end;

function TDMXRGBCluster.GetColor: TColor;
begin
  result := round(R) + (round(G) * 256) + (round(b) * 65536);
end;

function TDMXRGBCluster.GetColorMix: NativeFloat;
begin
  result := FColorMix;
end;

function TDMXRGBCluster.GetcoolRate: NativeFloat;
begin
  result := fcoolRate;
end;

function TDMXRGBCluster.GetG: single;
begin
  result := FG;
end;

function TDMXRGBCluster.GetIntensity: NativeFloat;
begin
  result := FIntensity;

end;

function TDMXRGBCluster.GetLumens: NativeFloat;
begin
  result := FRelativeLumens
end;

function TDMXRGBCluster.GEtMaxIntensity: NativeFloat;
begin
  result := FMaxIntensity;
end;

function TDMXRGBCluster.GEtMinIntensity: NativeFloat;
begin
  result := FMinIntensity;
end;

function TDMXRGBCluster.GetR: single;
begin
  result := FR;
end;

function TDMXRGBCluster.GetSourceColor: TColor;
begin
  result := FSourceColor;

end;

function TDMXRGBCluster.GetStrobeOverdrive: boolean;
begin
  result := FSTrobeOverdrive;
end;

function TDMXRGBCluster.GetTargetColor: TColor;
begin
  result := FTargetColor;
end;

function TDMXRGBCluster.GetTargetIntensity: NativeFloat;
begin
  result := FTargetIntensity;
end;

procedure TDMXRGBCluster.Init;
begin
  inherited;

end;

procedure TDMXRGBCluster.MixColors;
begin
  Color := ColorBlend(SourceColor, TargetColor, ColorMix);
end;

procedure TDMXRGBCluster.DoNewScene;
begin
  Color := clBlack;
  Intensity := 0;
end;

procedure TDMXRGBCluster.UpdatePhysics(enginetime: int64);
var
  R, rr: NativeFloat;
begin
  inherited;
  if FTargetIntensity > (1.0 / RelativeLumens) then
    FOverDrive := FTargetIntensity - (1.0 / RelativeLumens)
  else
    FOverDrive := 0;

  if FTargetIntensity > FIntensity then
  begin
    R := FIntensity;
    rr := R + (BURNRATE * (FDeltaTime / 1000));
    if rr > FTargetIntensity then
      rr := FTargetIntensity;

    if rr > 1.0 then
    begin
      rr := 1.0;
    end;

    FIntensity := ((rr * (FMaxIntensity - FMinIntensity)) + FMinIntensity);

  end
  else if FTargetIntensity < FIntensity then
  begin
    R := FIntensity;
    rr := R - (COOLRATE * (FDeltaTime / 1000));
    if rr < FTargetIntensity then
      rr := FTargetIntensity;

    if rr < 0.0 then
      rr := 0.0;

    FIntensity := ((rr * (FMaxIntensity - FMinIntensity)) + FMinIntensity);;

  end;

  if FOverDrive > 0 then
  begin
    if (((enginetime + round(50 * (PX + PY)) div 50)) mod 3) <> 0 then
    begin
      FIntensity := 1 - FOverDrive;
      if FIntensity < 0 then
        FIntensity := 0;
    end;
  end;
  // color := color;

end;

procedure TDMXRGBCluster.SaveSourceColor;
begin
  FSourceColor := Color;
end;

procedure TDMXRGBCluster.SetB(const Value: single);
begin
  FB := Value;
  // WriteDMXData;
end;

procedure TDMXRGBCluster.SetBlackout(const b: NativeFloat);
begin
  Fblackout := b;
  // WriteDMXData;
end;

procedure TDMXRGBCluster.SetBurnRate(const Value: NativeFloat);
begin
  FBurnRate := Value;
end;

procedure TDMXRGBCluster.SetChannelspacing(const Value: integer);
begin
  FChannelSpacing := Value;
  Channelchanged;
end;

procedure TDMXRGBCluster.SetColor(const Value: TColor);
var
  rr: byte;
  gg: byte;
  bb: byte;
begin
  rr := Value and 255;
  gg := (Value shr 8) and 255;
  bb := (Value shr 16) and 255;
{$IFNDEF HALOSIM}
  R := rr;
  G := gg;
  b := bb;
{$ELSE}
  if R > rr then
    R := ((rr - R) * (BURNRATE / 4)) + R
  else
    R := ((rr - R) * (COOLRATE / 4)) + R;

  if G > gg then
    G := ((gg - G) * (BURNRATE / 4)) + G
  else
    G := ((gg - G) * (COOLRATE / 4)) + G;

  if b > bb then
    b := ((bb - b) * (BURNRATE / 4)) + b
  else
    b := ((bb - b) * (COOLRATE / 4)) + b;
  if R < 0 then
    R := 0;
  if G < 0 then
    G := 0;
  if b < 0 then
    b := 0;

  if R > 255 then
    R := 255;
  if G > 255 then
    G := 255;
  if b > 255 then
    b := 255;
{$ENDIF}
end;

procedure TDMXRGBCluster.SetColorMix(const Value: NativeFloat);
begin
  FColorMix := Value;
  MixColors;
end;

procedure TDMXRGBCluster.SetCoolRate(const Value: NativeFloat);
begin
  fcoolRate := Value;
end;

procedure TDMXRGBCluster.SetFFIntensity(const Value: NativeFloat);
begin
  FFIntensity := Value;
end;

procedure TDMXRGBCluster.SetG(const Value: single);
begin
  FG := Value;
  // WriteDMXData;
end;

procedure TDMXRGBCluster.SetIntensity(i: NativeFloat);
begin
  // if i > 1.0 then
  // i := 1.0;
  if i < 0.0 then
    i := 0.0;
  FTargetIntensity := i / RelativeLumens;
  // WriteDMxData;
end;

procedure TDMXRGBCluster.SetLumens(const Value: NativeFloat);
begin
  FRelativeLumens := Value;
end;

procedure TDMXRGBCluster.SetMaxIntensity(const Value: NativeFloat);
begin
  FMaxIntensity := Value;
end;

procedure TDMXRGBCluster.SetMinIntensity(const Value: NativeFloat);
begin
  FMinIntensity := Value;
end;

procedure TDMXRGBCluster.SetR(const Value: single);
begin
  FR := Value;
  // WriteDMXData;
end;

procedure TDMXRGBCluster.SetSourceColor(const c: TColor);
begin
  FSourceColor := c;
end;

procedure TDMXRGBCluster.SetStrobeOverdrive(const Value: boolean);
begin
  FSTrobeOverdrive := Value;
end;

procedure TDMXRGBCluster.SetTargetColor(const Value: TColor);
begin
  FTargetColor := Value;
end;

procedure TDMXRGBCluster.SetTargetIntensity(const Value: NativeFloat);
begin
  FTargetIntensity := Value;
end;

procedure TDMXRGBCluster.SetTestParams(bTesting: boolean);
begin
  inherited;
  Intensity := 1.0;
  Color := clWhite;
end;

procedure TDMXRGBCluster.WriteDMXData;
var
  RRRR: NativeFloat;
  obb, br, bg, bb: byte;
  ir, ig, ib: integer;
  istr: IDMXStrobe;
begin
  inherited;
  RRRR := FIntensity;
  if RRRR > 1.0 then
    RRRR := 1.0;
  if RRRR < 0.0 then
    RRRR := 0.0;

  istr := nil;
  if IsInterface(IDMXStrobe) then
    self.QueryInterface(IDMXStrobe, istr);

  if istr = nil then
    if (Parent <> nil) then
      if Parent.IsInterface(IDMXStrobe) then
        Parent.QueryInterface(IDMXStrobe, istr);

  if (FStrobeBlackout > 0) and ((istr = nil) or (istr.StrobeEnable = false))
  then
  begin
    ir := 0;
    ig := 0;
    ib := 0;
  end
  else
  begin
    ir := round(FR * RRRR * (1.0 - Fblackout));
    ig := round(FG * RRRR * (1.0 - Fblackout));
    ib := round(FB * RRRR * (1.0 - Fblackout));;
  end;

  br := ir;
  bg := ig;
  bb := ib;
  obb := FLocalChannelData[2];
  // if bb < obb then
  // br := 255;

  FLocalChannelData[0] := br;
  FLocalChannelData[1 * ChannelSpacing] := bg;
  FLocalChannelData[2 * ChannelSpacing] := bb;
end;

{ TDMXChannelCluster }

procedure TDMXChannelCluster.Add(l: TDMXChannelCluster; iChannel: integer;
  GridX: NativeFloat; gridY: NativeFloat; iGroup: integer;
  ArtNetIP: string = '');
begin
  l.SetDataPointer(@FLocalChannelData[iChannel]);
  l.Parent := self;
  l.BaseChannel := iChannel;
  l.X := GridX;
  l.Y := gridY;
  l.Group := iGroup;
  l.ArtNetIP := ArtNetIP;

  FNewestLight := l;
  AfterAddLight;

end;

function TDMXChannelCluster.Add(c: TDMXChannelClusterClass; iChannel: integer;
  GridX, gridY: NativeFloat; iGroup: integer; ArtNetIP: string = '')
  : TDMXChannelCluster;
begin
  result := c.Create;
  result.Group := iGroup;
  Add(result, iChannel, GridX, gridY, iGroup, ArtNetIP);
end;

procedure TDMXChannelCluster.AfterAddLight;
begin
  // no imp
end;

procedure TDMXChannelCluster.Channelchanged;
var
  T: integer;
begin
  if FLocalChannelData = nil then
    exit;

  for T := 0 to Channelcount - 1 do
  begin
    self.FLocalChannelData[T] := 0;
  end;

end;

procedure TDMXChannelCluster.ClearBlackout;
begin
  FStrobeBlackout := 0;
end;

procedure TDMXChannelCluster.ClearDefinitions;
begin
  while FChannelDefinitions.count > 0 do
  begin
    FChannelDefinitions[0].free;
    FChannelDefinitions.delete(0);
  end;

  FChannelDefinitions.clear;
end;

procedure TDMXChannelCluster.CoordinateUpdated;
begin
  // no implementation required
end;

constructor TDMXChannelCluster.Create;
begin
  inherited;
  FUsePicturemap := true;
  FSubclusters := TDMXChannelClusterList.Create;
  FFLatlights := TDMXChannelClusterList.Create;
  FChannelDefinitions := TList<TDMXChannelDefinition>.Create;
  FChannelcount := 1;


end;

procedure TDMXChannelCluster.Defaults;
var
  T: integer;
begin
  for T := 0 to self.LIghts.count - 1 do
  begin
    LIghts[T].Defaults;
  end;

end;

procedure TDMXChannelCluster.DefineChannel(iChannel_1Based_FromDocumentation
  : integer; sDescription, sDetails: string);
var
  def: TDMXChannelDefinition;
begin
  def := TDMXChannelDefinition.Create;
  FChannelDefinitions.Add(def);
  def.Desription := sDescription;
  def.Details := sDetails;
  def.ChannelOffset := iChannel_1Based_FromDocumentation - 1;
  self.Channelcount := greaterof(channelcount, iChannel_1based_fromdocumentation);
end;

destructor TDMXChannelCluster.Destroy;
begin
  FFLatlights.free;
  FSubclusters.ClearAndFree;
  FSubclusters.free;
  ClearDefinitions;
  FChannelDefinitions.free;
  FChannelDefinitions := nil;

  inherited;
end;

procedure TDMXChannelCluster.DoNewScene;
begin
  //
end;

procedure TDMXChannelCluster.GetAspectList<T>(l: TUniverseList<T>);
var
  i: ni;
begin
  if self is T then
    l.Add(self);

  for i := 0 to ChildCount - 1 do
  begin
    Children[i].GetAspectList<T>(l);
  end;

end;

function TDMXChannelCluster.GetAspectInterfaceList<T>(guid: TGUID)
  : TUniverseList<T>;
begin
  result := TUniverseList<T>.Create;
  GetAspectInterfaceList<T>(result, guid);
end;

procedure TDMXChannelCluster.GetAspectInterfaceList<T>(l: TUniverseList<T>;
  guid: TGUID);
var
  i: ni;
  intf: T;
  iunk: IUnknown;
begin
  iunk := self.AsInterface<IUnknown>(IUnknown);
  if self.IsInterface(guid) then
    if not(l.IndexOf(self.AsInterface<T>(guid)) >= 0) then
      l.Add(self.AsInterface<T>(guid));

  for i := 0 to ChildCount - 1 do
  begin
    Children[i].GetAspectInterfaceList<T>(l, guid);
  end;
end;

function TDMXChannelCluster.GetAspectList<T>: TUniverseList<T>;
begin
  result := TUniverseList<T>.Create;
  GetAspectList<T>(result);
end;

function TDMXChannelCluster.GetBaseChannel: integer;
begin
  result := FBAseChannel;
end;

function TDMXChannelCluster.GetChild(idx: integer): TDMXChannelCluster;
begin
  result := FSubclusters[idx];
end;

function TDMXChannelCluster.GetchildCount: integer;
begin
  result := FSubclusters.count;
end;

function TDMXChannelCluster.GetGroup: integer;
begin
  result := FGroup;
end;

function TDMXChannelCluster.GetLightClass: string;
begin
  result := self.ClassName;
end;

function TDMXChannelCluster.GetPX: NativeFloat;
begin
  result := FPX;
end;

function TDMXChannelCluster.getPY: NativeFloat;
begin
  result := FPY;
end;

function TDMXChannelCluster.GetSupportingList<i>(guid: TGUID;
  iGROUPMAsk, iGroupResult: integer): TList<i>;
var
  T: integer;
  ii: i;
begin
  result := TList<i>.Create;

  for T := 0 to FFLatlights.count - 1 do
  begin
    if FFLatlights[T].IsInterface(guid) and
      ((FFLatlights[T].Group and iGROUPMAsk) = iGroupResult) then
    begin
      FFLatlights[T].QueryInterface(guid, ii);
      result.Add(ii);
    end;
  end;

end;

function TDMXChannelCluster.GetusePictureMap: boolean;
begin
  result := FUsePicturemap;
end;

function TDMXChannelCluster.GetX: NativeFloat;
begin
  result := Fx;
end;

function TDMXChannelCluster.GetXX: NativeFloat;
begin
  result := Fxx;
end;

function TDMXChannelCluster.gety: NativeFloat;
begin
  result := Fy;
end;

function TDMXChannelCluster.getYY: NativeFloat;
begin
  result := Fyy;
end;

procedure TDMXChannelCluster.Init;
begin
  Inherited;
end;

procedure TDMXChannelCluster.NewScene;
var
  T: ni;
begin
  DoNewScene;
  for T := 0 to FSubclusters.count - 1 do
  begin
    FSubclusters[T].NewScene;
  end;
end;

procedure TDMXChannelCluster.ParentChanged;
var
  T: integer;
begin
  if assigned(FParent) then
    FParent.RegisterLight(self, true);
  for T := 0 to FSubclusters.count - 1 do
  begin
    FSubclusters[T].ParentChanged;
  end;

end;

{ TDMXChannelClusterList }

procedure TDMXChannelClusterList.ClearAndFree;
begin
  while count > 0 do
  begin
    items[0].SafeFree;
    remove(items[0]);
  end;
end;

{ TDMXRGBWcluster }

const
  WON = 196;

constructor TDMXRGBWcluster.Create;
begin
  inherited;
  DefineChannel(4, 'White', '');
  FWRAtio := 0.33;
  Channelcount := 4;

end;

function TDMXRGBWcluster.GetColor: TColor;
var
  rr, gg, bb: integer;
begin
  rr := round(R);
  gg := round(G);
  bb := round(b);

  rr := lesserof(rr + round(W * ((255 - WON) / 255)), 255);
  gg := lesserof(gg + round(W * ((255 - WON) / 255)), 255);
  bb := lesserof(bb + round(W * ((255 - WON) / 255)), 255);

  result := rr + (gg * 256) + (bb * 65536);

end;

function TDMXRGBWcluster.GetW: single;
begin
  result := FW;
end;

procedure TDMXRGBWcluster.Init;
begin
  inherited;
end;

procedure TDMXRGBWcluster.SetColor(const Value: TColor);
var
  w1, w2, w3: NativeFloat;
  ww: integer;
  rr: byte;
  gg: byte;
  bb: byte;
begin
  rr := Value and 255;
  gg := (Value shr 8) and 255;
  bb := (Value shr 16) and 255;

  if RelativeWIntensity = 0 then
  begin
    ww := 0;
  end
  else
  begin
    ww := lesserof(255, lesserof(rr, lesserof(gg, bb)));
    ww := round(ww / RelativeWIntensity);
    if ww > 255 then
      ww := 255;
  end;

  if ww > 0 then
  begin
    rr := rr - round(ww * RelativeWIntensity);
    gg := gg - round(ww * RelativeWIntensity);
    bb := bb - round(ww * RelativeWIntensity);
  end;
{$IFNDEF HALOSIM}
  R := rr;
  G := gg;
  b := bb;
  W := ww;
{$ELSE}
  if R > rr then
    R := ((rr - R) * (BURNRATE / 4)) + R
  else
    R := ((rr - R) * (COOLRATE / 4)) + R;

  if G > gg then
    G := ((gg - G) * (BURNRATE / 4)) + G
  else
    G := ((gg - G) * (COOLRATE / 4)) + G;

  if b > bb then
    b := ((bb - b) * (BURNRATE / 4)) + b
  else
    b := ((bb - b) * (COOLRATE / 4)) + b;

  if W > ww then
    W := ((ww - W) * (BURNRATE / 4)) + W
  else
    W := ((ww - W) * (COOLRATE / 4)) + W;

  if R < 0 then
    R := 0;
  if G < 0 then
    G := 0;
  if b < 0 then
    b := 0;
  if W < 0 then
    W := 0;

  if R > 255 then
    R := 255;
  if G > 255 then
    G := 255;
  if b > 255 then
    b := 255;
  if W > 255 then
    W := 255;

{$ENDIF}
end;

procedure TDMXRGBWcluster.SetW(const Value: single);
begin
  FW := Value;
  // WriteDMXData;
end;

procedure TDMXRGBWcluster.WriteDMXData;
var
  R: NativeFloat;
begin
  inherited;
  R := FIntensity;
  if R > 1 then
    R := 1.0;

  FLocalChannelData[3] := round(FW * R * (1.0 - Fblackout));
end;

{ TDMXUniverse }

procedure TDMXUniverse.AfterAddLight;
begin
  inherited;
  Multiverse.Groups['All'].Add(FNewestLight);
end;

constructor TDMXUniverse.Create(sHost: string = '');
var
  T: ni;
begin
  inherited Create;
  udpc := TUdpSocket.Create(nil);
  Host := sHost;

  FLocalChannelData := @FData[0];
  InitializeCriticalSection(sect);

  MultiVerse.Add(self);

end;

procedure TDMXUniverse.DebugByte(chan: nativeint; val: byte);
begin
  if DebugMode then
  begin
    FData[chan] := val;
    Update;
  end;
end;

procedure TDMXUniverse.DebugChannelRange(iStart, iCount: integer);
var
  s: string;
  T: integer;
begin
  s := '@' + inttostr(iStart) + ': ';

  for T := 1 to iCount do
  begin
    s := s + inttohex(FLocalChannelData[T + iStart], 2) + ' ';
  end;

  debug.ConsoleLog(s);

end;

destructor TDMXUniverse.Destroy;
var
  T: ni;
begin

  MultiVerse.remove(self);
  udpc.free;
{$IFNDEF DMX_REMOTE_ONLY}
  dmx_h.closeport(h);
{$ENDIF}
  DeleteCriticalSection(sect);



  inherited;
end;

procedure TDMXUniverse.DoNewScene;
begin
  //
end;

procedure TDMXUniverse.Operate<T>(guid: TGUID; sGroup: string;
  op: TAnonLightOp<T>);
var
  gl: TUniverseList<TDMXChannelCluster>;
  ul: TUniverseList<T>;
  tt: ni;
begin
  gl := Multiverse.Groups[sGroup].GetList<TDMXChannelCluster>;
  try
    ul := TUniverseList<T>.Create;
    try
      // collect the aspects from the group
      for tt := 0 to gl.count - 1 do
      begin
        gl[tt].GetAspectInterfaceList<T>(ul, guid);
      end;

      // operate on the collected aspects
      for tt := 0 to ul.count - 1 do
      begin
        op(ul[tt], tt);
      end;
    finally
      ul.free;
    end;
  finally
    gl.free;
  end;

end;

function TDMXUniverse.GetData: PByte;
begin
  result := @FData[0];
end;

function TDMXMultiverse.GetEffectsGroup(idx: ni): TDMXEffects;
begin
  result := FEffectsGroups[idx];
end;

function TDMXMultiverse.GetJoys(idx: integer): TDMXExternalJoyControl;
begin
  result := Fjoys[idx mod length(Fjoys)];
end;

function TDMXMultiVerse.GetSupportingList<i>(guid: TGUID; iGROUPMAsk,
  iGroupResult: integer): TList<i>;
var
  t,u: ni;
  tmp: TList<i>;

begin
  result := TList<i>.create;
  for t:= 0 to count-1 do begin
    tmp := Items[t].GetSupportingList<i>(guid, iGroupMask, iGroupResult);
    for u := 0 to tmp.count-1 do begin
      result.Add(tmp[u]);
    end;

    tmp.free;



  end;

end;

function TDMXUniverse.GetSourceData: PByte;
begin
  result := @FSourceData[0];
end;

function TDMXUniverse.GetTargetData: PByte;
begin
  result := @FTargetData[0];
end;

procedure TDMXUniverse.Lock;
begin
  EnterCriticalSection(sect);
end;

procedure TDMXMultiVerse.Pauseupdates;
begin
  FupdatesPaused := true;
end;

procedure TDMXMultiVerse.ResumeUpdates;
begin
  FupdatesPaused := false;
end;

procedure TDMXUniverse.SetHost(const Value: string);
begin
  FHost := Value;
  FRemote := FHost <> '';
  if not Remote then
  begin
{$IFNDEF DMX_REMOTE_ONLY}
    h := dmx_h.openport(0);
{$ENDIF}
  end
  else
  begin
    udpc.RemoteHost := Value;
    udpc.RemotePort := '999';
    udpc.LocalPort := '998';
    udpc.active := true;
  end;

end;

procedure TDMXMultiverse.Start;
begin
  if Fthr <> nil then
    exit;

  Fthr := TPM.NeedThread<TDMxThread>(nil);
  Fthr.BetterPriority := bpTimeCritical;
  Fthr.Start;



end;

procedure TDMXMultiverse.Stop;
begin
  if assigned(Fthr) then
  begin
    Fthr.Stop;
    Fthr.WaitForFinish;
    Fthr.free;
  end;
  Fthr := nil;
end;

procedure TDMXChannelCluster.UpdatePhysics(enginetime: int64);
var
  T: integer;
begin
  if fTest then
    SetTestParams(fTest);
  for T := 0 to FFLatlights.count - 1 do
  begin
    FFLatlights[T].UpdatePhysics(enginetime);
    if not FFLatlights[T].DisableDMXWrite then
      FFLatlights[T].WriteDMXData;
  end;
  FDeltaTime := enginetime - FLastUpdateTime;
  FLastUpdateTime := enginetime;

end;

procedure TDMXChannelCluster.RegisterLight(l: TDMXChannelCluster;
  bOnlyFlatList: boolean);
begin

  if not bOnlyFlatList then
    FSubclusters.Add(l);

  // if top node, manage list of all lights
  if Parent = nil then
  BEGIN
    if FFLatlights.IndexOf(l) < 0 then begin
      FFLatlights.Add(l);
      multiverse.flatlights.add(l);
    end;
  end
  else
    Parent.RegisterLight(l, true);

end;

procedure TDMXChannelCluster.Reset;
begin

  raise ECritical.Create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TDMXChannelCluster.SetArtNetEndpoint(const Value: TArtNetEndpoint);
begin
  FArtNetEndPOint := Value;
  if Value <> nil then
    FTransport := transArtNet
  else
    FTransport := transDMX;

end;

procedure TDMXChannelCluster.SetArtNetIP(const Value: string);
begin
  FArtNetIP := Value;
  FTransport := transArtNet;
  // TryResolveArtNetIP;
end;

procedure TDMXChannelCluster.SetBaseChannel(const Value: integer);
begin
  FBAseChannel := Value;
  Channelchanged;
end;

procedure TDMXChannelCluster.SetChannelcount(const Value: integer);
begin
  FChannelCount := Value;
//  Channelchanged;
end;

procedure TDMXChannelCluster.SetDataPointer(p: PByte);
begin
  FLocalChannelData := p;
  Channelchanged;
end;

procedure TDMXChannelCluster.SetGroup(const Value: integer);
var
  T: integer;
begin
  FGroup := Value;
  for T := 0 to FSubclusters.count - 1 do
  begin
    FSubclusters[T].Group := Value;
  end;

end;

procedure TDMXChannelCluster.SetParent(const Value: TDMXChannelCluster);
begin
  if Value = FParent then
    exit;

  if assigned(FParent) then
    FParent.UnregisterLight(self);

  FParent := Value;

  if assigned(FParent) then
    FParent.RegisterLight(self);

  ParentChanged;

end;

procedure TDMXChannelCluster.SetPX(const Value: NativeFloat);
begin
  FPX := Value;
  CoordinateUpdated;
end;

procedure TDMXChannelCluster.SetPY(const Value: NativeFloat);
begin
  FPY := Value;
  CoordinateUpdated;
end;

procedure TDMXChannelCluster.SetTestParams(bTesting: boolean);
begin
  //
end;

procedure TDMXChannelCluster.SetUsePictureMap(const Value: boolean);
begin
  FUsePicturemap := Value;
end;

procedure TDMXChannelCluster.SetX(const Value: NativeFloat);
begin
  Fx := Value;
  CoordinateUpdated;
end;

procedure TDMXChannelCluster.SetXX(const Value: NativeFloat);
begin
  Fxx := Value;
  CoordinateUpdated;
end;

procedure TDMXChannelCluster.SetY(const Value: NativeFloat);
begin
  Fy := Value;
  CoordinateUpdated;
end;

procedure TDMXChannelCluster.SetYY(const Value: NativeFloat);
begin
  Fyy := Value;
  CoordinateUpdated;
end;

procedure TDMXChannelCluster.StrobeBlackout(b: boolean);
begin
  if b then
    inc(FStrobeBlackout)
  else
    dec(FStrobeBlackout);
end;

procedure TDMXChannelCluster.Test(b: boolean);
begin
  fTest := b;
end;

procedure TDMXChannelCluster.TryResolveArtNetIP;
begin
  if FArtNetEndPOint <> nil then
    exit;

  if FArtNetIP = '' then
    exit;

  art.Lock;
  try
    FArtNetEndPOint := art.FindByIP(FArtNetIP);
    if FArtNetEndPOint = nil then
    begin
      FArtNetEndPOint := TArtNetEndpoint.Create;
      FArtNetEndPOint.IP := FArtNetIP;
    end;
  finally
    art.Unlock;
  end;

end;

procedure TDMXChannelCluster.UnregisterLight(l: TDMXChannelCluster);
begin
  FFLatlights.remove(l);
  FSubclusters.remove(l);
end;

procedure TDMXChannelCluster.WriteDMXData;
begin

  // no implementation required

end;

procedure TDMXUniverse.Unlock;
begin
  LeaveCriticalSection(sect);
end;

procedure TDMXUniverse.Update;
var
  sa: TSockAddr;
  IP: TDynByteArray;
begin


  Lock;
  try
    if not DebugMode then
      UpdatePhysics(GetTicker());

    // FirstUniverse.DebugChannelRange(129,18);
    // FirstUniverse.DebugChannelRange(37,10);
    if (not Remote) and (Transport = transArtNet) then
    begin
      TryResolveArtNetIP;
      if ArtNetEndpoint <> nil then
      begin
        art.MapDMXData(ArtNetEndpoint, @FLocalChannelData[1], 512);
      end;
    end
    else
    begin
      if Remote then
      begin
        udpc.LocalHost := '0.0.0.0';
        udpc.RemoteHost := Host;
        udpc.LocalPort := '999';
        udpc.RemotePort := '999';
        udpc.BlockMode := bmBlocking;
        udpc.active := true;
        udpc.connect;

        if Transport = transDMX then
        begin
          udpc.SendBuf(FData[1], 512);
        end
        else
        begin
          movemem32(@FArtNetData[8], @FData[0], 512);
          FArtNetData[0] := ord(ansichar('A'));
          FArtNetData[1] := ord(ansichar('D'));
          FArtNetData[2] := ord(ansichar('A'));
          FArtNetData[3] := 0;
          IP := IPToBytes(ArtNetIP);
          FArtNetData[4] := IP[0];
          FArtNetData[5] := IP[1];
          FArtNetData[6] := IP[2];
          FArtNetData[7] := IP[3];

          udpc.SendBuf(FArtNetData[0], 512 + 8);
        end;
        // Debug('sent');
        // udpc.SendTo(FData[0], 512, '192.168.33.33');
        // udpc.SendBuf(FData[0], 512);
      end
      else
      begin
{$IFNDEF DMX_REMOTE_ONLY}
        // debug.ConsoleLog(inttostr(FDAta[414]));
        dmx_h.SendData(6, @FData[0], 513);
{$ENDIF}
      end;
    end;
  finally
    Unlock;
  end;

end;

procedure TDMXMultiverse.UpdateEffects(iTriggerNumber: integer;
  rTime: NativeFloat; rAmplitude: single);
var
  T, u: integer;
begin
  Multiverse.Lock;
  try
    for T := 0 to FEffectsGroups[0].count - 1 do
    begin
      FEffectsGroups[0][T].UpdatePhysics(iTriggerNumber, rTime, rAmplitude);
    end;

    for T := 0 to FEffects.count - 1 do
    begin
      FEffects[T].UpdatePhysics(iTriggerNumber, rTime, rAmplitude);
    end;

    for u := 1 to high(FEffectsGroups) do
    begin
      for T := 0 to FEffectsGroups[u].count - 1 do
      begin
        FEffectsGroups[u][T].UpdatePhysics(iTriggerNumber, rTime, rAmplitude);
      end;
    end;
  finally
    Multiverse.Unlock;
  end;

end;


procedure TDMXUniverse.Wipe;
var
  T: integer;
begin
  for T := 0 to 512 do
  begin
    self.Data[T] := 0;
  end;
end;

{ TDMXRGBWLight }

{ TDMXCoarseMirror }

{ TDMXColorMacroChannel }

procedure TDMXColorMacroChannel.AddtoPallete(idx: integer; c: TColor);
begin
  SetLength(FPallete, length(FPallete) + 1);
  SetLength(FIndicies, length(FIndicies) + 1);
  FPallete[high(FPallete)] := c;
  FIndicies[high(FIndicies)] := idx;
end;

function TDMXChannelCluster.BaseChannel0Based: ni;
begin
  result := FBaseChannel-1;
end;

constructor TDMXColorMacroChannel.Create;
begin
  inherited;

end;

procedure TDMXColorMacroChannel.Defaults;
begin
  inherited;
  Color := clBlack;

end;

function TDMXColorMacroChannel.GetBlackout: NativeFloat;
begin
  result := Fblackout;
end;

function TDMXColorMacroChannel.GetColor: TColor;
var
  T: integer;
begin
  result := clBlack;
  for T := low(FIndicies) to HIGH(FIndicies) do
  begin
    if FIndicies[T] <= ColorNumber then
      result := FPallete[T];
  end;

end;

function TDMXColorMacroChannel.GetColorMix: NativeFloat;
var
  i: nativeint;
begin
  result := FColorMix;
end;

function TDMXColorMacroChannel.GetColornumber: byte;
begin
  result := FLocalChannelData[0];
end;

function TDMXColorMacroChannel.GetSourceColor: TColor;
begin
  result := FSourceColor;
end;

function TDMXColorMacroChannel.GetTargetColor: TColor;
begin
  result := FTargetColor;
end;

procedure TDMXColorMacroChannel.SaveSourceColor;
begin
  FSourceColor := Color;
end;

procedure TDMXColorMacroChannel.SetBlackout(const b: NativeFloat);
begin
  Fblackout := b;
end;

function TDMXColorMacroChannel.IndexOfColor(c: TColor): integer;
var
  hsl, local: THSLNativeFloatColor;
  T: integer;
  err: NativeFloat;
  maxerr: NativeFloat;
  idx: integer;
  // fScores: array of NativeFloat;
begin
  idx := 0;
  maxerr := 999999;
  // cycle through the color pallete
  // SetLength(FScores, length(FPallete));
  for T := low(FPallete) to high(FPallete) do
  begin
    hsl := RGBtoHSL(ColorToNativeFloatRGB(c));
    local := RGBtoHSL(ColorToNativeFloatRGB(FPallete[T]));

    if local.s = 0 then
    begin
      err := system.abs(hsl.l - local.l) + system.abs(hsl.s - local.s);

    end
    else
    begin
      err := system.abs(local.h - hsl.h) + system.abs(local.s - hsl.s);
    end;

    if err < maxerr then
    begin
      maxerr := err;
      idx := T;
    end
    else
    begin
      if err = maxerr then
      begin
        if system.abs(local.h - hsl.h) <
          (hsl.h - RGBtoHSL(ColorToNativeFloatRGB(FPallete[idx])).h) then
        begin
          idx := T;
        end;
      end;
    end;
  end;
  result := idx;
end;

procedure TDMXColorMacroChannel.MixColors;
begin
  ColorNumber := round(Interpolate(ColorMix, IndexOfColor(SourceColor),
    IndexOfColor(TargetColor)));
end;

procedure TDMXColorMacroChannel.DoNewScene;
begin
  Color := clBlack;
end;

procedure TDMXColorMacroChannel.SetColor(const Value: TColor);
begin

  ColorNumber := FIndicies[IndexOfColor(Value)];

end;

procedure TDMXColorMacroChannel.SetColorMix(const Value: NativeFloat);
begin
  FColorMix := Value;
  MixColors;
end;

procedure TDMXColorMacroChannel.SetColorNumber(const Value: byte);
begin
  FColorNumber := Value;
end;

procedure TDMXColorMacroChannel.SetSourceColor(const c: TColor);
begin

  // raise Exception.Create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TDMXColorMacroChannel.SetTargetColor(const Value: TColor);
begin
  FTargetColor := Value;
end;

procedure TDMXColorMacroChannel.WriteDMXData;
begin
  inherited;
  if (Blackout > 0.5) or (FStrobeBlackout > 0) then
  begin
    FLocalChannelData[0] := FBlackoutColor;
  end
  else
  begin
    FLocalChannelData[0] := FColorNumber;
  end;

end;

{ TDMXNucleusColorMacroChannel }

constructor TDMXNucleusColorMacroChannel.Create;
begin
  inherited;
  AddtoPallete(0, $000000);
  AddtoPallete(8, $0000FF);
  AddtoPallete(16, $00FF00);
  AddtoPallete(24, $FF0000);
  AddtoPallete(32, $0033FF);
  AddtoPallete(40, $0066FF);
  AddtoPallete(48, $0099FF);
  AddtoPallete(56, $00CCFF);
  AddtoPallete(64, $00FFFF);
  AddtoPallete(72, $00FFCC);
  AddtoPallete(80, $00FF99);
  AddtoPallete(88, $00FF66);
  AddtoPallete(96, $00FF33);
  AddtoPallete(104, $33FF00);
  AddtoPallete(112, $66FF00);
  AddtoPallete(120, $99FF00);
  AddtoPallete(128, $CCFF00);
  AddtoPallete(136, $FFFF00);
  AddtoPallete(144, $FFCC00);
  AddtoPallete(152, $FF9900);
  AddtoPallete(160, $FF6600);
  AddtoPallete(168, $FF3300);
  AddtoPallete(176, $FF0033);
  AddtoPallete(184, $FF0066);
  AddtoPallete(192, $FF0099);
  AddtoPallete(200, $FF00CC);
  AddtoPallete(208, $FF00FF);
  AddtoPallete(216, $CC00FF);
  AddtoPallete(224, $9900FF);
  AddtoPallete(236, $6600FF);
  AddtoPallete(242, $3300FF);
  AddtoPallete(255, $FFFFFF);

end;

{ TDMXElanColorMacroChannel }

constructor TDMXElanColorMacroChannel.Create;
begin
  inherited;
  AddtoPallete(0, $000000);
  AddtoPallete(1, $FF0000);
  AddtoPallete(2, $0000FF);
  AddtoPallete(3, $FF00FF);
  AddtoPallete(4, $00FF00);
  AddtoPallete(5, $FFFF00);
  AddtoPallete(6, $FF00FF);
  AddtoPallete(7, $FFFFFF);
  UsePictureMap := false;

end;

procedure TDMXElanColorMacroChannel.SetColorNumber(const Value: byte);
begin
  inherited;
end;

procedure TDMXElanColorMacroChannel.WriteDMXData;
begin
  // inherited;
  if Fblackout > 0.5 then
  begin
    FLocalChannelData[1] := 0;
    FLocalChannelData[0] := 0;
    FLocalChannelData[2] := 0;
  end
  else
  begin
    FLocalChannelData[1] := (FColorNumber and 1) * 8;
    FLocalChannelData[0] := (FColorNumber and 2) * 8;
    FLocalChannelData[2] := (FColorNumber and 4) * 8;
  end;

end;

{ TDMXVueColorMacroChannel }

constructor TDMXVueColorMacroChannel.Create;
begin
  inherited;

  AddtoPallete(0, $000000);
  AddtoPallete(12, $FF0000);
  AddtoPallete(18, $0000FF);
  AddtoPallete(24, $00FF00);
  AddtoPallete(30, $7F7F7F);
  AddtoPallete(36, $009FFF);
  AddtoPallete(42, $FFFF00);
  AddtoPallete(48, $FF00FF);
  AddtoPallete(48, $3FFF3F);
  AddtoPallete(48, $FF7F7F);
  AddtoPallete(54, $FF7F7F);
  AddtoPallete(60, $7F7FFF);
  AddtoPallete(66, $7FFFFF);
  AddtoPallete(72, $FF9FFF);
  AddtoPallete(78, $FFFFFF);

end;

{ TDMXScorpionColorMacroChannel }

constructor TDMXScorpionColorMacroChannel.Create;
begin
  inherited;
  AddtoPallete(0, $000000);
  AddtoPallete(5, $0000FF);
  AddtoPallete(29, $00FF00);
  AddtoPallete(57, $00FFFF);
  AddtoPallete(85, $007F00);
  AddtoPallete(113, $00007F);
  AddtoPallete(141, $007FFF);
  AddtoPallete(225, $007F7F);

end;

{ TDMXColorDash }

procedure TDMXColorDashByDelegate.Channelchanged;
begin
  inherited;
  FColorComponent.BaseChannel := BaseChannel;
  FColorComponent.SetDataPointer(self.FLocalChannelData + 0);
end;

constructor TDMXColorDashByDelegate.Create;
begin
  inherited;
  FColorComponent := TDMXRGBCluster.Create;

end;

destructor TDMXColorDashByDelegate.Destroy;
begin
  FColorComponent.free;
  inherited;
end;

{ TDMXThread }

procedure TDMxThread.DoExecute;
begin
  inherited;
  Loop := true;
//  Windows.beep(1000,10);
  MultiVerse.Update;
//  Universe.Update;
  RunHot := false;
  ColdRunInterval := (1000 div 90);

end;

{ TDMXColorBar }

procedure TDMXColorBar.Channelchanged;
var
  T: integer;
begin
  inherited;
  for T := 0 to 7 do
  begin
    FSections[T].BaseChannel := BaseChannel + (T * 3);
    FSections[T].SetDataPointer(FLocalChannelData + (T * 3));
    FSections[T].Parent := self;
  end;
  FLocalChannelData[25] := 0;
  FLocalChannelData[26] := 0;
  FLocalChannelData[27] := 255;
end;

procedure TDMXColorBar.CoordinateUpdated;
var
  T: integer;
begin
  inherited;
  for T := 0 to 7 do
  begin
    FSections[T].Y := self.Y;
    FSections[T].X := (self.X - 2) + ((T / 8) * 4);
  end;
end;

constructor TDMXColorBar.Create;
var
  T: integer;
begin
  inherited;
  FSections := TList<TDMXcolorBarSection>.Create;
  for T := 0 to 7 do
  begin
    FSections.Add(TDMXcolorBarSection.Create);
    FSections[T].FSectionIndex := T;
    FSections[T].Parent := self;
    FSections[T].X := self.X;
    FSections[T].Y := (self.Y - 2) + ((T / 8) * 4);
    FSections[T].RelativeLumens := 0.3;
  end;
  ChannelCount := 28;
end;

destructor TDMXColorBar.Destroy;
begin
  FSections.free;

  inherited;
end;

procedure TDMXColorBar.Detach;
var
  T: integer;
begin
  inherited;
  for T := 0 to 7 do
  begin
    FSections[T].SafeFree;
  end;
end;

function TDMXColorBar.GetSection(idx: integer): TDMXcolorBarSection;
begin
  result := FSections[idx];
end;

procedure TDMXColorBar.Init;
begin
  inherited;

end;

procedure TDMXColorBar.WriteDMXData;
begin
  FLocalChannelData[27] := 255;
end;

{ TDMXScorpion }

procedure TDMXScorpion.Channelchanged;
begin
  inherited;
  Fcolor.BaseChannel := BaseChannel + 1;
  Fcolor.SetDataPointer(FLocalChannelData + 1);
end;

constructor TDMXScorpion.Create;
begin
  inherited;
  Fcolor := TDMXScorpionColorMacroChannel.Create;
  Fcolor.Parent := self;
  FUsePicturemap := false;
  Fcolor.UsePictureMap := false;
  DefineChannel(1, 'Control Mode', '');
  DefineChannel(2, 'Color Selection', '');
  DefineChannel(3, 'Strobe', '0=none, 5-254=slow>fast, 255=Sound');
  DefineChannel(4, 'Rotation motor 1',
    '5-127=Clockwise, 134-255=Counter Clockwise');
  DefineChannel(5, 'Stutter motor 1',
    '0=no function, 5-56=mode 1 slow>fast(affected by ch4), 57-112=mode 3 slow>fast(affected by ch4), 113-168=mode3 slow>fast, 167-255=mode 4 slow>fast');
  DefineChannel(6, 'Rotation motor 2',
    '5-127=Clockwise, 134-255=Counter Clockwise');
  DefineChannel(7, 'Stutter motor 2',
    '0=no function, 5-56=mode 1 slow>fast(affected by ch6), 57-112=mode 3 slow>fast(affected by ch6), 113-168=mode3 slow>fast, 167-255=mode 4 slow>fast');


end;

destructor TDMXScorpion.Destroy;
begin
  Fcolor.SafeFree;
  inherited;
end;

procedure TDMXScorpion.Detach;
begin
  inherited;

end;

procedure TDMXScorpion.DoNewScene;
begin
  Param1 := 0;
  Param2 := 0;
  Param3 := 0;
  Param4 := 0;
  ColorChannel.Color := clYellow;

end;

procedure TDMXScorpion.Init;
begin
  inherited;

end;

procedure TDMXScorpion.WriteDMXData;
begin
  inherited;
  FLocalChannelData[0] := FControlMode;
  // FLocalChannelData[1] := Self.ColorChannel.ColorNumber; implemented by child object
  // channel 2 is unimplemented... it is strobe
  FLocalChannelData[2] := 0;
  FLocalChannelData[3] := FParam1;
  FLocalChannelData[4] := FParam2;
  FLocalChannelData[5] := FParam3;
  FLocalChannelData[6] := FParam4;

end;

{ TDMXvue }

procedure TDMXvue.Channelchanged;
begin
  inherited;

end;

constructor TDMXvue.Create;
begin
  inherited;
  UsePictureMap := false;
  FMacroMode := 255;
  DefineChannel(1, 'operating mode',
    '0=individual cluster control, 12+((n-1)*6)=AutoN, 228-Auto37, 236=Full Auto, 246=Sound Mode');
  DefineChannel(2, 'Cluster 1', 'pattern');
  DefineChannel(3, 'Cluster 2', 'pattern');
  DefineChannel(4, 'Cluster 3', 'pattern');
  DefineChannel(5, 'Cluster 4', 'pattern');
  DefineChannel(6, 'Strobe/Run Speed',
    '0-1=None, 2-255 Fast>Slow... applies to auto 1-13 and static, Run Speed for other autos');
  DefineChannel(7, 'rotation speed', '0-5=Stop,6-255 Slow>Fast');


end;

destructor TDMXvue.Destroy;
begin

  inherited;
end;

procedure TDMXvue.Detach;
begin
  inherited;

end;

procedure TDMXvue.DoNewScene;
begin
  inherited;
  MacroMode := 0;
  speed := 0;

end;

procedure TDMXvue.Init;
begin
  inherited;

end;

procedure TDMXvue.WriteDMXData;
begin
  inherited;
  FLocalChannelData[6] := FSpeed;
  if MacroMode = 255 then
    FLocalChannelData[0] := MacroMode;
  FLocalChannelData[5] := 0;
end;

{ TDMXElan }

constructor TDMXElan.Create;
begin
  inherited;
  DefineChannel(1,'Group 1');
  DefineChannel(2,'Group 2');
  DefineChannel(3,'Group 3');

end;

procedure TDMXElan.DoNewScene;
begin
  inherited;
  Color := clBlack;
end;

{ TDMXUFO }

constructor TDMXUFO.Create;
begin
  inherited;
  FControlMode := 255;
  FColorMode := 0;

end;

procedure TDMXUFO.DoNewScene;
begin
  controlmode := 0;
end;

procedure TDMXUFO.WriteDMXData;
begin
  inherited;
  FLocalChannelData[6] := FColorMode;
  FLocalChannelData[9] := FControlMode;
end;

{ TDMXNucleus }

procedure TDMXNucleus.Channelchanged;
var
  T: integer;
begin
  inherited;
  for T := 0 to 5 do
  begin
    Fcolors[T].BaseChannel := BaseChannel + ((T * 3) + 2);
    Fcolors[T].SetDataPointer(FLocalChannelData + ((T * 3) + 2));
    Fcolors[T].Parent := self;

    FMirrors[T].BaseChannel := BaseChannel + (T * 3);
    FMirrors[T].SetDataPointer(FLocalChannelData + (T * 3));
    FMirrors[T].Parent := self;
  end;
end;

procedure TDMXNucleus.CoordinateUpdated;
begin
  inherited;
  Fcolors[0].X := self.X + 0;
  Fcolors[1].X := self.X + 0.42;
  Fcolors[2].X := self.X + 0.42;
  Fcolors[3].X := self.X + 0;
  Fcolors[4].X := self.X - 0.42;
  Fcolors[5].X := self.X - 0.42;

  Fcolors[0].Y := self.Y - 1;
  Fcolors[1].Y := self.Y - 0.5;
  Fcolors[2].Y := self.Y + 0.5;
  Fcolors[3].Y := self.Y + 1;
  Fcolors[4].Y := self.Y + 0.5;
  Fcolors[5].Y := self.Y - 0.5;

end;

constructor TDMXNucleus.Create;
var
  T: integer;
begin
  inherited;

  FMirrors := TList<TDMXCoarseMirror>.Create;
  Fcolors := TList<TDMXNucleusColorMacroChannel>.Create;

  for T := 0 to 5 do
  begin
    Fcolors.Add(TDMXNucleusColorMacroChannel.Create);
    Fcolors[T].Parent := self;
    FMirrors.Add(TDMXCoarseMirror.Create);
    FMirrors[T].Parent := self;

  end;

  ChannelCount := 19;

end;

destructor TDMXNucleus.Destroy;
begin
  FMirrors.free;
  Fcolors.free;
  inherited;
end;

procedure TDMXNucleus.DoNewScene;
begin
  controlmode := 0;
end;

procedure TDMXNucleus.Init;
begin
  inherited;
  Channelcount := 22;
end;

procedure TDMXNucleus.WriteDMXData;
begin
  inherited;
  if FStrobeBlackout > 0 then
    FLocalChannelData[18] := 0
  else
    FLocalChannelData[18] := FControlMode;

end;

{ TDMXStrobe }

constructor TDMXStrobe.Create;
begin
  inherited;
  DefineChannel(1, 'Speed');
  DefineChannel(2, 'Intensity');
end;

procedure TDMXStrobe.DoNewScene;
begin
  StrobeIntensity := 0;
  StrobeSpeed := 0;
  StrobeEnable := false;
end;

function TDMXStrobe.GetSTrobeEnable: boolean;
begin
  result := FStrobeEnable;
end;

function TDMXStrobe.GetSTrobeIntensity: NativeFloat;
begin
  result := FSTrobeIntensity;
end;

function TDMXStrobe.GetStrobeSpeed: NativeFloat;
begin
  result := FSTrobeSpeed
end;

procedure TDMXStrobe.SetStrobeEnable(const Value: boolean);
begin
  FStrobeEnable := Value;
end;

procedure TDMXStrobe.SetStrobeIntensity(const Value: NativeFloat);
begin
  FSTrobeIntensity := Value;
end;

procedure TDMXStrobe.SetStrobeSpeed(const Value: NativeFloat);
begin
  FSTrobeSpeed := Value;
end;

procedure TDMXStrobe.StrobeStrike;
begin
  FSTrobeStrike := true;
end;

procedure TDMXStrobe.WriteDMXData;
begin
  if (FStrobeBlackout > 0) then
  begin
    FLocalChannelData[0] := round(FSTrobeSpeed * 64) + (255 - 64);
    FLocalChannelData[1] := 255
  end
  else if FSTrobeStrike then
  begin
    FLocalChannelData[0] := 0;
    FLocalChannelData[1] := 255;
    FSTrobeStrike := false;
  end
  else
  begin
    if StrobeEnable then
    begin
      FLocalChannelData[0] := round(FSTrobeSpeed * 64) + (255 - 64);
      FLocalChannelData[1] := round(FSTrobeIntensity * 255);
    end
    else
    begin
      FLocalChannelData[0] := 0;
      FLocalChannelData[1] := 0;

    end;
  end;
end;

{ TDMXQSpot14 }

procedure TDMXQSpot14.Calc_GoboChannel1Value;
begin
  Fgobochannel1 := Fgobo1 * 10;

end;

procedure TDMXQSpot14.Calc_GoboChannel2Value;
begin
  Fgobochannel2 := Fgobo2 * 10;
end;

procedure TDMXQSpot14.Channelchanged;
begin
  inherited;

  FMotion.BaseChannel := BaseChannel;
  FMotion.SetDataPointer(FLocalChannelData + 0);
  FMotion.Parent := self;

  Fcolor.BaseChannel := BaseChannel + 6;
  Fcolor.SetDataPointer(FLocalChannelData + 5);
  Fcolor.Parent := self;

end;

procedure TDMXQSpot14.CoordinateUpdated;
begin
  inherited;
  // no implementation required
  Fcolor.X := self.X;
  Fcolor.Y := self.Y;
  Fcolor.PX := self.PX;
  Fcolor.PY := self.PY;
  Fcolor.XX := self.XX;
  Fcolor.YY := self.YY;

end;

constructor TDMXQSpot14.Create;
begin
  inherited;
  FRelativeLumens := 1;
  FMotion := TDMXFineHeadMotion.Create;
  Fcolor := TDMXQspotColorChannel.Create;
  FMotion.Parent := self;
  Fcolor.Parent := self;
  FBurnRate := 32;
  fcoolRate := 16;
  FMaxIntensity := 1.0;
  fFocus := 127;
  FSpeed := 0.1;

  CoordinateUpdated;

  ChannelCount := 14;
  DefineChannel(1,'PanH');
  DefineChannel(2,'PanL');
  DefineChannel(3,'TiltH');
  DefineChannel(4,'TiltL');
  DefineChannel(5,'Color Wheel');
  DefineChannel(6,'Gobo1');
  DefineChannel(7,'Gobo2');
  DefineChannel(8,'GoboSpin');
  DefineChannel(9,'Prism');
  DefineChannel(10,'Focus');
  DefineChannel(11,'Intensity');
  DefineChannel(12,'Strobe');
  DefineChannel(13,'Unknown');
  DefineChannel(14,'Mode?');



end;

destructor TDMXQSpot14.Destroy;
begin

  inherited;
end;

procedure TDMXQSpot14.DoNewScene;
begin
  Gobo1 := 0;
  Gobo2 := 0;
  Intensity := 0;
  Color.Color := clWhite;
  Prism := 0;
  PrismSpin := 0;
  GoboSpin := 0;
  Strobe := 0;

end;

function TDMXQSpot14.GetburnRate: NativeFloat;
begin
  result := FBurnRate;
end;

function TDMXQSpot14.GetcoolRate: NativeFloat;
begin
  result := fcoolRate;
end;

function TDMXQSpot14.GetgoboSPinValue: byte;
var
  rounded: integer;
begin
  result := 0;

  if FgoboSpin > 1 then
    FgoboSpin := 1;
  if FgoboSpin < -1 then
    FgoboSpin := -1;

  rounded := round(FgoboSpin * 97);

  if rounded = 0 then
    result := 0
  else
  begin
    if rounded < 0 then
    begin
      result := 158 - rounded;
    end;

    if rounded > 0 then
    begin
      result := (158 - 97) + rounded;
    end;
  end;
end;

function TDMXQSpot14.GetIntensity: NativeFloat;
begin
  result := FIntensity;
end;

function TDMXQSpot14.GetLumens: NativeFloat;
begin
  result := FRelativeLumens;
end;

function TDMXQSpot14.GEtMaxIntensity: NativeFloat;
begin
  result := FMaxIntensity
end;

function TDMXQSpot14.GEtMinIntensity: NativeFloat;
begin
  result := FMinIntensity
end;

function TDMXQSpot14.GetSTrobeEnable: boolean;
begin
  result := FStrobe <> 0;
end;

function TDMXQSpot14.GetSTrobeIntensity: NativeFloat;
begin
  result := 1;
end;

function TDMXQSpot14.GetStrobeOverdrive: boolean;
begin
  result := FSTrobeOverdrive;
end;

function TDMXQSpot14.GetStrobeSpeed: NativeFloat;
begin
  result := 1;
end;

function TDMXQSpot14.GetTargetIntensity: NativeFloat;
begin
  result := FTargetIntensity;
end;

procedure TDMXQSpot14.Reset;
begin
  FReset := true;
  FResetTime := -1;
end;

procedure TDMXQSpot14.SetBurnRate(const Value: NativeFloat);
begin
  FBurnRate := Value;
end;

procedure TDMXQSpot14.SetCoolRate(const Value: NativeFloat);
begin
  fcoolRate := Value;
end;

procedure TDMXQSpot14.SetGobo1(const Value: byte);
begin
  Fgobo1 := Value;
  Calc_GoboChannel1Value;
end;

procedure TDMXQSpot14.SetGobo2(const Value: byte);
begin
  Fgobo2 := Value;
  Calc_GoboChannel2Value;
end;

procedure TDMXQSpot14.SetIntensity(Value: NativeFloat);
begin
  FTargetIntensity := Value;
end;

procedure TDMXQSpot14.SetLumens(const i: NativeFloat);
begin
  FRelativeLumens := i;
end;

procedure TDMXQSpot14.SetMaxIntensity(const Value: NativeFloat);
begin
  FMaxIntensity := Value;
end;

procedure TDMXQSpot14.SetMinIntensity(const Value: NativeFloat);
begin
  FMinIntensity := Value;
end;

procedure TDMXQSpot14.SetPrismSpin(const Value: NativeFloat);
var
  i: integer;
begin
  FPrismSpin := Value;
  if FPrismSpin > 0 then
  begin
    i := round(FPrismSpin * 120);
    if i > 120 then
      i := 120;

    FPrism := i;
  end;

  if FPrismSpin < 0 then
  begin
    i := round((0 - FPrismSpin) * 120);
    if i > 120 then
      i := 120;

    FPrism := 128 + i;

  end;

end;

procedure TDMXQSpot14.SetStrobeEnable(const Value: boolean);
begin
  self.Strobe := booltoint(Value);
end;

procedure TDMXQSpot14.SetStrobeIntensity(const Value: NativeFloat);
begin

  // raise Exception.create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TDMXQSpot14.SetStrobeOverdrive(const Value: boolean);
begin
  FSTrobeOverdrive := Value;
end;

procedure TDMXQSpot14.SetStrobeSpeed(const Value: NativeFloat);
begin

  // raise Exception.create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TDMXQSpot14.SetTargetIntensity(const Value: NativeFloat);
begin
  FTargetIntensity := Value;
end;

procedure TDMXQSpot14.StrobeStrike;
begin

  // raise Exception.create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TDMXQSpot14.WriteDMXData;
begin
  inherited;
  // FLocalChannelData[0] := 145;
  // FLocalChannelData[1] := 0;
  // FLocalChannelData[2] := 45;
  // FLocalChannelData[3] := 0;
  // FMotion.WriteDMXData;

  FLocalChannelData[4] := 255 - (round(speed * 255));
  // FLocalChannelData[5] <---COLOR WHEEL handled by color channel
  FLocalChannelData[6] := Fgobochannel1;
  FLocalChannelData[7] := Fgobochannel2;
  FLocalChannelData[8] := GetgoboSPinValue;
  FLocalChannelData[9] := FPrism;
  FLocalChannelData[10] := fFocus;
  if FStrobeBlackout > 0 then
    FLocalChannelData[11] := 0
  else
    FLocalChannelData[11] := round(FIntensity * 255);

  if FStrobe = 0 then
    FStrobe := 255;

  if FStrobeBlackout > 0 then
    FLocalChannelData[12] := 255
  else
    FLocalChannelData[12] := FStrobe;
  if FReset then
  begin
    FLocalChannelData[13] := 200;
  end
  else
  begin
    FLocalChannelData[13] := 255;
  end;

end;

{ TDMXTiltPan }

procedure TDMXTiltPan.DoNewScene;
begin
  //
end;

function TDMXTiltPan.GetPan: integer;
begin
  result := round(FPanF * FMaxPanValue);

end;

function TDMXTiltPan.GetPanF: NativeFloat;
begin
  result := FPanF;
end;

function TDMXTiltPan.GetPanOffset: NativeFloat;
begin
  result := FPanOffset
end;

function TDMXTiltPan.GetPanTiltRatio: NativeFloat;
begin
  result := FPAnTiltRatio;
end;

function TDMXTiltPan.GetTilt: integer;
begin
  result := round(FTiltF * FMaxTiltValue);
end;

function TDMXTiltPan.GetTiltF: NativeFloat;
begin
  result := FTiltF;
end;

function TDMXTiltPan.GetTiltOffset: NativeFloat;
begin
  result := FtiltOffset;
end;

procedure TDMXTiltPan.Init;
begin
  inherited;
  FMaxPanValue := 65535;
  FMaxTiltValue := 65535;
  FPAnTiltRatio := 2.0;

end;

procedure TDMXTiltPan.SetPan(Value: integer);
begin
  if Value > FMaxPanValue then
    Value := FMaxPanValue;
  if Value < 0 then
    Value := 0;
  FPanF := Value / FMaxPanValue;

end;

procedure TDMXTiltPan.SetPanF(const Value: NativeFloat);
begin
  FPanF := Value;
end;

procedure TDMXTiltPan.SetPanOffset(const Value: NativeFloat);
begin
  FPanOffset := Value;

end;

procedure TDMXTiltPan.SetPanTiltRatio(const Value: NativeFloat);
begin
  FPAnTiltRatio := Value;
end;

procedure TDMXTiltPan.SetTilt(Value: integer);
begin
  if Value > 65535 then
    Value := 65535;
  if Value < 0 then
    Value := 0;
  FTiltF := (Value / 65535);

end;

procedure TDMXTiltPan.SetTiltF(const Value: NativeFloat);
begin
  FTiltF := Value;
end;

procedure TDMXTiltPan.SetTiltOffset(const Value: NativeFloat);
begin
  FtiltOffset := Value;
end;

procedure TDMXTiltPan.WriteDMXData;
begin
  inherited;
end;

{ TDMXQspotColorChannel }

constructor TDMXQspotColorChannel.Create;
begin
  inherited;
  AddtoPallete(0, $FFFFFF);
  AddtoPallete(17, $0000FF);
  AddtoPallete(34, $00FFFF);
  AddtoPallete(51, $FF00FF);
  AddtoPallete(68, $007F00);
  AddtoPallete(85, $007FFF);
  AddtoPallete(102, $FF0000);
  AddtoPallete(119, $FFFF00);
  AddtoPallete(136, $00FF00);

end;

procedure TDMXQSpot14.UpdatePhysics(enginetime: int64);
var
  R, rr: NativeFloat;
begin
  inherited;
  if FReset then
  begin
    if FResetTime = -1 then
      FResetTime := enginetime
    else if enginetime - FResetTime > 10000 then
      FReset := false;

    exit;

  end;
  if FTargetIntensity > FIntensity then
  begin
{$IFNDEF LOG_BURN}
    R := FIntensity;
    rr := R + (BURNRATE * (FDeltaTime / 1000));
{$ELSE}
    R := abs(FTargetIntensity - FIntensity);
    rr := FIntensity + (R * (BURNRATE * (FDeltaTime / 1000)));
{$ENDIF}
    if rr > FTargetIntensity then
      rr := FTargetIntensity;

    // if rr > 1.0 then
    // rr := 1.0;

    FIntensity := (rr * (FMaxIntensity - FMinIntensity)) + FMinIntensity;

  end
  else if FTargetIntensity < FIntensity then
  begin
{$IFNDEF LOG_BURN}
    R := FIntensity;
    rr := R - (COOLRATE * (FDeltaTime / 1000));
{$ELSE}
    R := abs(FTargetIntensity - FIntensity);
    rr := FIntensity - (R * (COOLRATE * (FDeltaTime / 1000)));
{$ENDIF}
    if rr < FTargetIntensity then
      rr := FTargetIntensity;

    if rr < 0.0 then
      rr := 0.0;

    FIntensity := (rr * (FMaxIntensity - FMinIntensity)) + FMinIntensity;;
  end;
end;

{ TDMXSpotBarColorChannel }

constructor TDMXSpotBarColorChannel.Create;
begin
  inherited;

  AddtoPallete(0, $000000);
  AddtoPallete(10, $0000FF);
  AddtoPallete(50, $00FF00);
  AddtoPallete(90, $FF0000);
  AddtoPallete(130, $00FFFF);
  AddtoPallete(170, $FF00FF);
  AddtoPallete(210, $FFFF00);
  AddtoPallete(250, $FFFFFF);

end;

procedure TDMXSpotBarColorChannel.SetTargetColor(const Value: TColor);
begin
  inherited;

end;

{ TDMXFusionBar }

procedure TDMXFusionBar.Channelchanged;
var
  T: integer;
begin
  inherited;
  for T := 0 to 3 do
  begin
    spots[T].BaseChannel := BaseChannel + 1 + (T * 2);
    spots[T].SetDataPointer(self.FLocalChannelData + 1 + (T * 2));
  end;
  wash.BaseChannel := BaseChannel + 2;
  wash.SetDataPointer(self.FLocalChannelData + 2);

end;

procedure TDMXFusionBar.CoordinateUpdated;
begin
  inherited;
  wash.X := X;
  wash.Y := Y;
  spots[0].Y := Y;
  spots[1].Y := Y;
  spots[2].Y := Y;
  spots[3].Y := Y;

  spots[0].X := X - 1;
  spots[1].X := X - 3;
  spots[2].X := X + 1;
  spots[3].X := X + 3;

end;

constructor TDMXFusionBar.Create;
var
  T: integer;
begin
  inherited;
  for T := 0 to 3 do
  begin
    spots[T] := TDMXSpotBarColorChannel.Create;
    spots[T].Parent := self;
    spots[T].X := 1.5 - T;
  end;

  wash := TDMXRGBCluster.Create;
  wash.Parent := self;
  wash.ChannelSpacing := 2;

end;

destructor TDMXFusionBar.Destroy;
var
  T: integer;
begin
  for T := 0 to 3 do
  begin
    spots[T].free;
  end;
  wash.free;

  inherited;
end;

procedure TDMXFusionBar.DisableSpots;
var
  T: integer;
begin
  for T := 0 to 3 do
  begin
    spots[T].DisableDMXWrite := true;
  end;

end;

procedure TDMXFusionBar.DoNewScene;
begin
  controlmode := 0;
end;

procedure TDMXFusionBar.EnableSpots;
var
  T: integer;
begin
  for T := 0 to 3 do
  begin
    spots[T].DisableDMXWrite := false;
  end;

end;

procedure TDMXFusionBar.SetControlMode(const Value: integer);
var
  T: integer;
begin
  FControlMode := Value;
  if Value = 5 then
  begin
    for T := 0 to 3 do
      self.spots[0].UsePictureMap := true;
  end
  else
  begin
    for T := 0 to 3 do
      self.spots[0].UsePictureMap := false;
  end;

end;

procedure TDMXFusionBar.WriteDMXData;
begin
  case controlmode of
    0:
      begin
        FLocalChannelData[0] := 255;
        spots[0].Color := clBlack;
        spots[1].Color := clBlack;
        spots[2].Color := clBlack;
        spots[3].Color := clBlack;
      end;
    1:
      FLocalChannelData[0] := 70;
    2:
      FLocalChannelData[0] := 130;
    3:
      FLocalChannelData[0] := 155;
    4:
      FLocalChannelData[0] := 168;
    5:
      FLocalChannelData[0] := 255;
  end;

  inherited; // <----------- do inherited code in the middle yo

  case controlmode of
    0, 5:
      EnableSpots;
    1 .. 4:
      BEGIN
        DisableSpots;
        FLocalChannelData[1] := 250;

      END;
  end;

end;

{ TDMXColorDashAccent }

constructor TDMXColorDashAccent.Create;
begin
  inherited;
  FRelativeLumens := 0.4;
end;

{ TDMXPro38B }

constructor TDMXPro38B.Create;
begin
  inherited;
  RelativeLumens := 0.8;
end;

{ TDMXEffect }

{ TFXStaticLight }

{ TDMXEffect<T_LightBase> }

{ TFXStaticColor }

{ TFXStaticIntensity }

{ TFXStaticIntensity }

{ TFXStaticColor }

procedure TFXStaticColor.DefineParameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;
  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'FadeTime';
  def.Steps := 10000;
  def.Min := 0;
  def.Max := 20;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);
  def.rValue := Multiverse.Defaultcolorfade;

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Color';
  def.Steps := 10000;
  def.Min := 0;
  def.Max := 20;
  def.entryType := eetHex;
  def.imptyp := eptString;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Color2';
  def.Steps := 10000;
  def.Min := 0;
  def.Max := 20;
  def.entryType := eetHex;
  def.imptyp := eptString;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'ByCoord';
  def.Steps := 2;
  def.Min := 0;
  def.Max := 1;
  // def.entryType := eptFloat;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'StripeCount';
  def.Steps := 2;
  def.Min := 0;
  def.Max := 2;
  def.rValue := 1;

  // def.entryType := eptFloat;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

end;

procedure TFXStaticColor.Init;
begin
  inherited;

end;

procedure TFXStaticColor.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  FLastUpdateTime: NativeFloat;
  rgb: TRGBNativeFloatColor;
  idmx: IDMXColor;
begin
  inherited;
  if not l.IsInterface(IDMXColor) then
    exit;
  idmx := l as IDMXColor;

  // if we just switched it on:
  if not FFirstupdateReceived then
  begin
    // record whatever color was on before
    idmx.SaveSourceColor;
  end;

  // fade into the target color
  if (rTime - FCreationTime) < FFadeTime then
  begin
    if self.Params['stripecount'].rValue = 2 then
    begin
      if (iGroupID mod 2) = 0 then
      begin
        idmx.Color := ColorBlend(idmx.SourceColor,
          HexStringToColor(self.Params['Color'].sValue), (rTime - FCreationTime)
          / self.Params['FadeTime'].rValue);

      end
      else
      begin
        idmx.Color := ColorBlend(idmx.SourceColor,
          HexStringToColor(self.Params['Color2'].sValue),
          (rTime - FCreationTime) / self.Params['FadeTime'].rValue);

      end;

    end
    else
    begin
      idmx.Color := ColorBlend(idmx.SourceColor,
        HexStringToColor(self.Params['Color2'].sValue), (rTime - FCreationTime)
        / self.Params['FadeTime'].rValue);
    end;
  end
  else
  begin
    if self.Params['stripecount'].rValue = 2 then
    begin
      if (iGroupID mod 2) = 0 then
      begin
        idmx.Color := HexStringToColor(self.Params['Color'].sValue);
      end
      else
      begin
        idmx.Color := HexStringToColor(self.Params['Color2'].sValue);
      end;
    end
    else
    begin
      idmx.Color := HexStringToColor(self.Params['Color'].sValue);
    end;
  end;

end;

{ TFXStaticIntensity }

procedure TFXStaticIntensity.DefineParameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;
  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Intensity';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 1;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Burnrate';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 64;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'CoolRate';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 64;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  Params['Intensity'].rValue := 1.0;
  Params['BurnRate'].rValue := Multiverse.DefaultBurn;
  Params['CoolRate'].rValue := Multiverse.DefaultCool;

end;

procedure TFXStaticIntensity.Init;
begin
  inherited;

end;

procedure TFXStaticIntensity.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
begin
  inherited;

  if not l.IsInterface(IDMXIntensity) then
    exit;

  (l as IDMXIntensity).BURNRATE := Params['burnrate'].rValue;
  (l as IDMXIntensity).COOLRATE := Params['coolrate'].rValue;
  (l as IDMXIntensity).Intensity := Params['intensity'].rValue;
end;

{ TDMXLightGroups }

function TDMXLightGroups.Add(sNAme: string): TDMXLIghtGroup;
begin
  result := TDMXLIghtGroup.Create;
  result.Name := sNAme;
  inherited Add(result);

end;

procedure TDMXLightGroups.clear;
var
  T: integer;
begin
  for T := 0 to count - 1 do
  begin
    self.Groups[T].free;
  end;

  inherited clear;

  self.Add('_None');

end;

constructor TDMXLightGroups.Create;
begin
  inherited;
  clear;
end;

function TDMXLightGroups.GetGroup(idx: integer): TDMXLIghtGroup;
begin
  result := items[idx];
end;

function TDMXLightGroups.GetGroupByName(sNAme: string): TDMXLIghtGroup;
var
  i: integer;
begin

  i := IndexOf(sNAme);
  if i < 0 then
    result := Add(sNAme)
  else
    result := items[i];

  // Debug(inttohex(nativeint(pointer(self)),16)+': has '+inttostr(Count)+' groups.');
end;

function TDMXLightGroups.IndexOf(sNAme: string): integer;
var
  T: integer;
begin
  result := -1;
  sNAme := lowercase(sNAme);
  for T := 0 to count - 1 do
  begin
    if lowercase(self.Groups[T].Name) = sNAme then
    begin
      result := T;
      break;
    end;
  end;

end;

{ TDMXEffects }

function TDMXEffects.Add(sTypeName: string; iODUB: nativeint): TDMXEffect;
begin
  result := TDMXEffect(Multiverse.EffectLibrary.New(sTypeName));
  if result <> nil then
  begin
    result.ODUB := iODUB;
    inherited Add(result);
    FNewestEffect := result;
  end
  else
    raise Exception.Create('No Effect type exists named: ' + sTypeName);
end;

procedure TDMXEffects.clear;
var
  ef: TDMXEffect;
begin
  while count > 0 do
  begin
    ef := items[count - 1];
    ef.free;
    delete(count - 1);
  end;

end;

procedure TDMXEffects.DeleteNewest(sTypeName: string; iODUB: nativeint);
var
  i: integer;
  f: TDMXEffect;
begin
  FirstUniverse.Lock;
  try
    i := IndexOfEffect(sTypeName, iODUB);
    if i >= 0 then
    begin
      f := items[i];
      f.free;
      self.delete(i);

    end;
  finally
    FirstUniverse.Unlock;
  end;

end;

function TDMXEffects.FindEffect(sTypeName: string; iODUB: nativeint)
  : TDMXEffect;
var
  i: integer;
begin
  i := IndexOfEffect(sTypeName, iODUB);
  if i < 0 then
    result := nil
  else
    result := items[i];

end;

function TDMXEffects.IndexOfEffect(sTypeName: string; iODUB: nativeint)
  : integer;
var
  T: integer;
  d: TDMXEffect;
begin
  result := -1;
  for T := self.count - 1 downto 0 do
  begin
    d := self.items[T];

    if ((iODUB < 0) or (d.ODUB = iODUB)) and (sTypeName = '') or
      (d.ClassName = sTypeName) then
    begin
      result := T;
      break;
    end;
  end;
end;

{ TDMXEffect }

procedure TDMXEffect.AddGroup(sNAme: string; bLightsOnly: boolean = false);
var
  T: integer;
  lg: TDMXLIghtGroup;
begin
  lg := Multiverse.Groups.ByName[sNAme];

  if not bLightsOnly then
    FGroups.Add(lg);
  for T := 0 to lg.count - 1 do
  begin
    self.AddLight(lg[T]);
  end;

end;

procedure TDMXEffect.AddLight(l: TDMXChannelCluster);
begin
  FLights.Add(l);
end;

procedure TDMXEffect.CleanupFX;
var
  T, u: integer;
begin
  for T := 0 to FLights.count - 1 do
  begin
    for u := 0 to FLights[T].ChildCount - 1 do
    begin
      OnFXDestroyingForLight(FLights[T].Children[u]);
    end; // todo 2 : this isn't exactly fully recusive... is this bad?

    OnFXDestroyingForLight(FLights[T]);

  end;
end;

procedure TDMXEffect.ClearLights;
begin
  FLights.clear;
end;

constructor TDMXEffect.Create;
begin
  inherited;
  FLights := TList<TDMXChannelCluster>.Create;
  FParamDefinitions := TList<TDMXEffectParameterDefinition>.Create;
  DefineParameters;
  FGroups := TList<TDMXLIghtGroup>.Create; // doesn't own items
end;

procedure TDMXEffect.DefineParameters;
begin
  //
end;

destructor TDMXEffect.Destroy;
begin
  CleanupFX;
  while FParamDefinitions.count > 0 do
  begin
    FParamDefinitions[FParamDefinitions.count - 1].free;
    FParamDefinitions.delete(FParamDefinitions.count - 1);
  end;
  FParamDefinitions.free;
  FLights.free;
  FLights := nil;
  FGroups.free;
  inherited;

end;

function TDMXEffect.GetGroup(idx: nativeint): TDMXLIghtGroup;
begin
  result := FGroups[idx];
end;

function TDMXEffect.GetGroupCount: nativeint;
begin
  result := FGroups.count;
end;

function TDMXEffect.GEtName: string;
begin
  result := ClassName;
end;

procedure TDMXEffect.GetParam(sNAme: string; out rValue: NativeFloat);
var
  pd: TDMXEffectParameterDefinition;
  s: string;
  R: NativeFloat;

begin
  rValue := 0;
  pd := GetParamDefinition(sNAme);
  if pd = nil then
    exit;

  if pd.UsesCustomHander then
  begin
    if pd.imptyp = eptString then
    begin
      SetParamLL(sNAme, s);
      rValue := strtofloat(s);
    end
    else
      SetParamLL(sNAme, rValue);
  end
  else
  begin
    if pd.imptyp = eptString then
      rValue := strtofloat(pd.sValue)
    else
      rValue := pd.rValue;
  end;

end;

procedure TDMXEffect.GetParam(sNAme: string; out sValue: string);
var
  pd: TDMXEffectParameterDefinition;
  R: NativeFloat;
begin
  sValue := '';
  pd := GetParamDefinition(sNAme);
  if pd = nil then
    exit;

  if pd.UsesCustomHander then
  begin
    if pd.imptyp = eptString then
      GetParamLL(sNAme, sValue)
    else
    begin
      GetParamLL(sNAme, R);
      sValue := floattostr(R);
    end;
  end
  else
  begin
    if pd.imptyp = eptString then
      sValue := pd.sValue
    else
      sValue := floattostr(pd.rValue);
  end;

end;

function TDMXEffect.GetParamCount: nativeint;
begin
  result := FParamDefinitions.count;
end;

function TDMXEffect.GetParamDefinition(sNAme: string)
  : TDMXEffectParameterDefinition;
var
  i, T: nativeint;
begin
  result := nil;
  i := -1;
  sNAme := lowercase(sNAme);
  for T := 0 to FParamDefinitions.count - 1 do
  begin
    if lowercase(FParamDefinitions[T].Name) = sNAme then
    begin
      i := T;
      break;
    end;
  end;

  if i < 0 then
    exit;

  result := FParamDefinitions[i];

end;

procedure TDMXEffect.GetParamLL(sNAme: string; out rValue: NativeFloat);
begin
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW

  // To Use: make sure to flag the definition: "UsesCustomHandler := true"

end;

procedure TDMXEffect.GetParamLL(sNAme: string; out sValue: string);
begin
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW

  // To Use: make sure to flag the definition: "UsesCustomHandler := true"
end;

function TDMXEffect.HasGroup(sNAme: string): boolean;
begin
  result := IndexOfGroup(sNAme) >= 0;
end;

function TDMXEffect.IndexOfGroup(sNAme: string): nativeint;
var
  T: nativeint;
begin
  result := -1;
  sNAme := lowercase(sNAme);
  for T := 0 to FGroups.count - 1 do
  begin
    if lowercase(FGroups[T].Name) = sNAme then
    begin
      result := T;
      break;
    end;
  end;

end;

function TDMXEffect.NewTrigger: boolean;
begin
  result := FTriggerNumber <> FLastTriggerNumber;
end;

procedure TDMXEffect.OnFXDestroyingForLight(l: TDMXChannelCluster);
begin
  // no imp required
end;

procedure TDMXEffect.RebuildLightListFromGroups;
var
  T: integer;
begin
  FLights.clear;
  for T := 0 to FGroups.count - 1 do
  begin
    AddGroup(FGroups[T].Name, true);
  end;

end;

procedure TDMXEffect.RemoveGroup(sNAme: string);
var
  i: integer;
begin
  i := IndexOfGroup(sNAme);

  if i >= 0 then
    FGroups.delete(i);

  RebuildLightListFromGroups;
end;

procedure TDMXEffect.SEtPAram(sNAme, sValue: string);
var
  pd: TDMXEffectParameterDefinition;
begin
  pd := GetParamDefinition(sNAme);
  if pd = nil then
    exit;

  if pd.UsesCustomHander then
  begin
    if pd.imptyp = eptString then
      SetParamLL(sNAme, sValue)
    else
      SetParamLL(sNAme, strtofloat(sValue));
  end
  else
  begin
    if pd.imptyp = eptString then
      pd.sValue := sValue
    else
      pd.rValue := strtofloat(sValue);
  end;

end;

procedure TDMXEffect.SEtPAram(sNAme: string; rValue: NativeFloat);
var
  pd: TDMXEffectParameterDefinition;
begin
  pd := GetParamDefinition(sNAme);

  if pd.UsesCustomHander then
  begin
    if pd.imptyp = eptString then
      SetParamLL(sNAme, floattostr(rValue))
    else
      SetParamLL(sNAme, rValue);
  end
  else
  begin
    if pd.imptyp = eptString then
      pd.sValue := floattostr(rValue)
    else
      pd.rValue := rValue;
  end;

end;

procedure TDMXEffect.SetParamLL(sNAme: string; rValue: NativeFloat);
begin
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW

  // To Use: make sure to flag the definition: "UsesCustomHandler := true"
end;

procedure TDMXEffect.SetParamLL(sNAme, sValue: string);
begin
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW
  // USE THIS ONLY FOR CUSTOM HANDLERS... MOST PARAMS ARE AUTOMATICALLY STORED
  // IN THE DEFINITION NOW

  // To Use: make sure to flag the definition: "UsesCustomHandler := true"
end;

procedure TDMXEffect.UpdatePhysics(iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  T, u: integer;
begin
  FTriggerNumber := iTriggerNumber;
  FCurrentTime := rTime;

  if not FFirstupdateReceived then
  begin
    FCreationTime := rTime;
  end;

  // Debug.Log(self.ClassName+' has '+inttostr(FLights.Count));

  for T := 0 to FLights.count - 1 do
  begin
    try

//      if FLights[t].childcount > 100 then
//        Debug.Log('trap '+FLights[T].classname+' has '+FLights[t].childcount.tostring+' children.');
      for u := 0 to FLights[T].ChildCount - 1 do
      begin



        UpdatePhysicsforlight(T, FLights.count, FLights[T].Children[u],
          iTriggerNumber, rTime, rAmplitude);
      end; // todo 2 : this isn't exactly fully recusive... is this bad?

      UpdatePhysicsforlight(T, FLights.count, FLights[T], iTriggerNumber, rTime,
        rAmplitude);
    except
      on E: Exception do
      begin
        E.Message := 'when running ' + self.ClassName +
          ' encountered error processing ' + FLights[T].ClassName +
          ' with message ' + E.Message;
        debug.Log(self, E.Message, 'error');

      end;
    end;
  end;
  FLastUpdateTime := rTime;

  FFirstupdateReceived := true;
  FLastTriggerNumber := iTriggerNumber;

end;

procedure TDMXEffect.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
begin

  //
end;

{ TUniverseList<T> }

constructor TUniverseList<T>.Create;
begin
  inherited;

end;

destructor TUniverseList<T>.Destroy;
begin
  inherited;
end;

{ TDMXLightGroup }

constructor TDMXLIghtGroup.Create;
begin
  inherited;
end;

destructor TDMXLIghtGroup.Destroy;
begin
  inherited;
end;

function TDMXLIghtGroup.GetList<T>: TUniverseList<T>;
var
  tt: nativeint;
begin
  result := TUniverseList<T>.Create;
  for tt := 0 to count - 1 do
  begin
    self[tt].GetAspectList<T>(result);
  end;

end;

{ TFXStaticStripes }

procedure TFXStaticStripes.Init;
begin
  inherited;
  FadeTime := Multiverse.Defaultcolorfade;

end;

procedure TFXStaticStripes.SetParamLL(sNAme, sValue: string);
begin
  inherited;
  sNAme := lowercase(sNAme);
  if sNAme = 'color1' then
  begin
    Color1 := hextoint(sValue);
  end
  else if sNAme = 'color2' then
  begin
    Color2 := hextoint(sValue);
  end
  else if sNAme = 'fadetime' then
  begin
    FadeTime := strtofloat(sValue);
  end;

end;

procedure TFXStaticStripes.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  FLastUpdateTime: NativeFloat;
  rgb: TRGBNativeFloatColor;
  idmx: IDMXColor;
begin
  inherited;
  idmx := l as IDMXColor;

  if not FFirstupdateReceived then
  begin
    idmx.SaveSourceColor;
  end;

  if (rTime - FCreationTime) < FFadeTime then
  begin
    if iGroupID mod 2 = 0 then
    begin
      idmx.Color := ColorBlend(idmx.SourceColor, self.Color1,
        (rTime - FCreationTime) / self.FadeTime);
    end
    else
    begin
      idmx.Color := ColorBlend(idmx.SourceColor, self.Color2,
        (rTime - FCreationTime) / self.FadeTime);
    end;

  end
  else
  begin
    if iGroupID mod 2 = 0 then
    begin
      idmx.Color := Color1;
    end
    else
    begin
      idmx.Color := Color2;
    end;
  end;

end;

{ TDMXOptiTri }

constructor TDMXOptiTri.Create;
begin
  inherited;
  RelativeLumens := 1.0;
end;

{ TDMXSkyLaser }

procedure TDMXSkyLaser.Channelchanged;
begin
  inherited;

end;

constructor TDMXSkyLaser.Create;
begin
  inherited;
  DefineChannel(1, 'Param 1');
  DefineChannel(2, 'Param 2');
  DefineChannel(3, 'Param 3');
  DefineChannel(4, 'Param 4');

end;

destructor TDMXSkyLaser.Destroy;
begin

  inherited;
end;

procedure TDMXSkyLaser.Detach;
begin
  inherited;

end;

procedure TDMXSkyLaser.DoNewScene;
begin
  Intensity := 0;
  Param1 := 0;
  Param2 := 0;
  Param3 := 0;
  Param4 := 0;
end;

procedure TDMXSkyLaser.Init;
begin
  inherited;

end;

procedure TDMXSkyLaser.WriteDMXData;
begin
  inherited;
  if FStrobeBlackout > 0 then
  begin
    FLocalChannelData[0] := 0;
  end
  else
  begin
    FLocalChannelData[0] := FIntensity;
  end;
  FLocalChannelData[1] := FParam1;
  FLocalChannelData[2] := FParam2;
  FLocalChannelData[3] := FParam3;
  FLocalChannelData[4] := FParam4;

end;

{ TDMXRGBICluster }

constructor TDMXRGBICluster.Create;
begin
  inherited;
  DefineChannel(4,'Intensity');
  Channelcount := 4;

end;

procedure TDMXRGBICluster.Init;
begin
  inherited;
end;

procedure TDMXRGBICluster.WriteDMXData;
begin
  inherited;
  FLocalChannelData[6] := 255;
end;

{ TDMXBlueGalaxy }

procedure TDMXBlueGalaxy.Channelchanged;
begin
  inherited;

end;

constructor TDMXBlueGalaxy.Create;
begin
  inherited;
  ChannelCount := 5;
end;

destructor TDMXBlueGalaxy.Destroy;
begin

  inherited;
end;

procedure TDMXBlueGalaxy.Detach;
begin
  inherited;

end;

procedure TDMXBlueGalaxy.Init;
begin
  inherited;

end;

procedure TDMXBlueGalaxy.WriteDMXData;
begin
  inherited;
  if FStrobeBlackout > 0 then
  begin
    FLocalChannelData[0] := 0;
    FLocalChannelData[1] := 0;
  end
  else
  begin
    FLocalChannelData[0] := FParam1;
    FLocalChannelData[1] := FParam2;
    FLocalChannelData[2] := FParam3;
    FLocalChannelData[3] := FParam4;
    FLocalChannelData[4] := FParam5;

  end;

end;

{ TDMXLEDGrid }

constructor TDMXLEDGrid.Create;
begin
  inherited;
  CreateGrid;


end;

procedure TDMXLEDGrid.CreateGrid;
var
  T, u: integer;
begin
  for T := 0 to 15 do
  begin
    for u := 0 to 15 do
    begin
      FGrid[T][u] := TDMXGelChannel.Create;
      FGrid[T][u].BitPrecision := 1;
      FGrid[T][u].Parent := self;
      if T mod 2 = 0 then
      begin
        if u mod 2 = 0 then
        begin
          FGrid[T][u].GelColor := $FF;
        end
        else
        begin
          FGrid[T][u].GelColor := $FF00;
        end;
      end
      else
      begin
        if u mod 2 = 0 then
        begin
          FGrid[T][u].GelColor := $FF0000;
        end
        else
        begin
          FGrid[T][u].GelColor := $FFFFFF;
        end;
      end;
    end;
  end;

end;

destructor TDMXLEDGrid.Destroy;
begin
  DestroyGrid;
  inherited;
end;

procedure TDMXLEDGrid.DestroyGrid;
var
  T, u: integer;
begin
  for T := 0 to 15 do
  begin
    for u := 0 to 15 do
    begin
      FGrid[u][T].SafeFree;
    end;
  end;
end;

function TDMXLEDGrid.GetGrid(X, Y: nativeint): TDMXGelChannel;
begin
  result := FGrid[Y][X];
end;

procedure TDMXLEDGrid.Init;
begin
  inherited;
  Channelcount := 256;
end;

procedure TDMXLEDGrid.Wipe(c: TColor);
var
  T, u: nativeint;
begin
  for u := 0 to 15 do
  begin
    for T := 0 to 15 do
    begin
      Grid[T, u].Color := c;
    end;
  end;

end;

procedure TDMXLEDGrid.WriteDMXData;
begin
  inherited;

end;

{ TDMXGelChannel }

procedure TDMXLEDGrid.Channelchanged;
var
  X, XX, Y: nativeint;
  idx: nativeint;
  baseidx: nativeint;
begin
  inherited;
  for Y := 0 to 15 do
  begin
    for X := 0 to 15 do
    begin
      XX := X div 2;
      baseidx := ((Y div 2) * 32) + ((X div 2) * 4);
      idx := baseidx;

      // if ((x mod 2)>0) xor ((y mod 2)>0) then
      // inc(idx,2);
      if ((X mod 2) > 0) then
        inc(idx, 1);
      if ((Y mod 2) > 0) then
        inc(idx, 2);

      FGrid[Y][X].BaseChannel := self.BaseChannel + idx;
      FGrid[Y][X].SetDataPointer(FLocalChannelData + idx);
    end;
  end;

end;

procedure TDMXLEDGrid.CoordinateUpdated;
var
  T, u: nativeint;
begin
  inherited;
  for T := 0 to 15 do
  begin
    for u := 0 to 15 do
    begin
      FGrid[T][u].X := T;
      FGrid[T][u].Y := u;

    end;
  end;
end;

procedure TDMXGelChannel.DoNewScene;
begin
  Color := clBlack;
end;

function TDMXGelChannel.GetColor: TColor;
begin
  result := Fcolor;
end;

function TDMXGelChannel.GetColorMix: NativeFloat;
begin
  result := FColorMix;
end;

function TDMXGelChannel.GetSourceColor: TColor;
begin
  // no implementation
  result := FSourceColor;
end;

function TDMXGelChannel.GetTargetColor: TColor;
begin
  result := FTargetColor
end;

procedure TDMXGelChannel.MixColors;
begin
  Color := ColorBlend(SourceColor, TargetColor, ColorMix);
end;

procedure TDMXGelChannel.SaveSourceColor;
begin

  FSourceColor := Fcolor;
end;

procedure TDMXGelChannel.SetColor(const Value: TColor);
begin
  Fcolor := ColorMultiply(Value, FGelColor);
end;

procedure TDMXGelChannel.SetColorMix(const Value: NativeFloat);
begin
  FColorMix := Value;
  MixColors;
end;

procedure TDMXGelChannel.SetSourceColor(const Value: TColor);
begin
  FSourceColor := Value;

end;

procedure TDMXGelChannel.SetTargetColor(const Value: TColor);
begin
  FTargetColor := Value;
end;

procedure TDMXGelChannel.WriteDMXData;
var
  iDitherThreshold: nativeint;
  hsl, gelhsl: THSLNativeFloatColor;
begin
  inherited;

  iDitherThreshold := 1 shl (8 - BitPrecision);
  hsl := RGBtoHSL(ColorToNativeFloatRGB(Fcolor));
  gelhsl := RGBtoHSL(ColorToNativeFloatRGB(FGelColor));

  if ((hsl.l / gelhsl.l) * 255) > iDitherThreshold then
  begin
    FLocalChannelData[0] := round(hsl.l * 255);
  end
  else
  begin
    FLocalChannelData[0] := 0;
  end;

end;

{ TFXGridPong }

procedure TFXGridPong.Init;
begin
  inherited;
  FBallVelX := 0.3;
  FBallVelY := 0.3;
end;

procedure TFXGridPong.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  G: TDMXLEDGrid;
  T: integer;
begin
  inherited;
  if not(l is TDMXLEDGrid) then
    exit;

  G := l as TDMXLEDGrid;
  G.Wipe;

  FBallX := FBallX + FBallVelX;
  FBallY := FBallY + FBallVelY;
  if FBallY < 0 then
  begin
    FBallVelY := 0 - (FBallVelY * (0.5 + random(100) / 100));
    FBallY := 0;
  end;
  if FBallY > 15 then
  begin
    FBallVelY := 0 - (FBallVelY * (0.5 + random(100) / 100));
    FBallY := 15;
  end;

  if FBallX < 0 then
  begin
    FBallVelX := 0 - FBallVelX;
    FBallX := 0;
  end;
  if FBallX > 15 then
  begin
    FBallVelX := 0 - FBallVelX;
    FBallX := 15;
  end;

  FBallHist[FHistIdx].X := round(FBallX);
  FBallHist[FHistIdx].Y := round(FBallY);
  FHistIdx := (FHistIdx + 1) mod BALL_HIST_SIZE;
  for T := 0 to BALL_HIST_SIZE - 1 do
  begin
    G.Grid[FBallHist[FHistIdx].X, FBallHist[FHistIdx].Y].Color := clWhite;
  end;

  // if the velocity is towards the left, then move the left paddle
  if FBallVelX < 0 then
  begin
    FPaddle1 := (FPaddle1 * 0.1) + (FBallY * 0.9);
  end
  else
  begin
    FPaddle2 := (FPaddle2 * 0.1) + (FBallY * 0.9);
  end;

  if FPaddle1 < 1 then
    FPaddle1 := 1;
  if FPaddle1 > 14 then
    FPaddle1 := 14;
  if FPaddle2 < 1 then
    FPaddle2 := 1;
  if FPaddle2 > 14 then
    FPaddle2 := 14;

  for T := -1 to 1 do
  begin
    G.Grid[0, round(FPaddle1) + T].Color := clWhite;
    G.Grid[15, round(FPaddle2) + T].Color := clWhite;
  end;

end;

{ TFXGridFill }

procedure TFXGridFill.Init;
begin
  inherited;
  Fcolor := clWhite;
  FColor2 := clBlack;
end;

procedure TFXGridFill.SetParamLL(sNAme, sValue: string);
begin
  inherited;
  if sNAme = 'rate' then
  begin
    Rate := round(strtofloat(sValue));
  end
  else if sNAme = 'color' then
  begin
    Color := hextoint(sValue);
  end
  else if sNAme = 'color1' then
  begin
    Color := hextoint(sValue);
  end
  else if sNAme = 'color2' then
  begin
    Color2 := hextoint(sValue);
  end;

end;

procedure TFXGridFill.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  T, u: nativeint;
begin
  inherited;

  if not(l is TDMXLEDGrid) then
    exit;

  with l as TDMXLEDGrid do
  begin

    for T := 0 to Rate - 1 do
    begin
      if FPass = 0 then
      begin
        Grid[FFillX, FFillY].Color := Color;
      end
      else
        Grid[FFillX, FFillY].Color := Color2;

      FFillX := FFillX + 1;
      if FFillX = 16 then
      begin
        FFillX := 0;
        FFillY := FFillY + 1;
        if FFillY = 16 then
        begin
          FFillY := 0;
          FPass := (FPass + 1) mod 2;
        end;
      end;
    end;

  end;
end;

{ TFXStaticCoolRate }

procedure TFXStaticCoolRate.DefineParameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;
  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'CoolRate';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 64;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

end;

procedure TFXStaticCoolRate.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
begin
  inherited;
  if not l.IsInterface(IDMXIntensity) then
    exit;

  with l as IDMXIntensity do
  begin
    COOLRATE := Params['coolrate'].rValue;
  end;

end;

{ TFXStrobe }

procedure TFXStrobe.DefineParameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;
  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Speed';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 1;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);
  def.rValue := Multiverse.Defaultcolorfade;

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Intensity';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 1;
  def.entryType := eetSlider;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Randomness';
  def.Steps := 32;
  def.Min := 0;
  def.Max := 32 / 256;
  def.entryType := eetSlider;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

end;

procedure TFXStrobe.Init;
begin

  // raise Exception.create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TFXStrobe.OnFXDestroyingForLight(l: TDMXChannelCluster);
var
  s: IDMXStrobe;
begin
  inherited;
  if not(l.IsInterface(IDMXStrobe)) then
    exit;

  s := l as IDMXStrobe;

  s.StrobeEnable := false;

end;

procedure TFXStrobe.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  s: IDMXStrobe;
  speed, Intensity, randomness: NativeFloat;
begin
  inherited;
  if not(l.IsInterface(IDMXStrobe)) then
    exit;


  s := l as IDMXStrobe;

  speed := Params['Speed'].rValue;
  Intensity := Params['Intensity'].rValue;
  randomness := Params['Randomness'].rValue;

  if l.IsInterface(IDMXIntensity) then
  with l as IDMXIntensity do begin
    Intensity := Params['Intensity'].rValue;
  end;



  s.StrobeIntensity := Intensity;
  s.StrobeSpeed := speed + ((random(255) / 255) * randomness);
  if (speed > 0) and (Intensity > 0) then
  begin
    s.StrobeEnable := true;
  end
  else
  begin
    s.StrobeEnable := false;
  end;

end;

{ TFXStrobeBlackout }

procedure TFXStrobeBlackout.OnFXDestroyingForLight(l: TDMXChannelCluster);
begin
  inherited;
  if not(l.IsInterface(IDMXStrobe)) then
  begin
    l.StrobeBlackout(false);
  end;
end;

procedure TFXStrobeBlackout.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
begin
  inherited;

  if not FirstUpdateReceived then
  begin
    if not(l.IsInterface(IDMXStrobe)) then
    begin
      l.StrobeBlackout(true);
    end;
  end;

end;

{ TDMXCoarseMirror }

constructor TDMXCoarseMirror.Create;
begin
  inherited;
  DefineChannel(1, 'Pan', '');
  DefineChannel(2, 'Tilt', '');
  ChannelCount := 2;

end;

procedure TDMXCoarseMirror.Init;
begin
  inherited;
  FMaxPanValue := 255;
  FMaxTiltValue := 255;
  FPAnTiltRatio := 2.0;

end;

procedure TDMXCoarseMirror.WriteDMXData;
var
  pp, tt: NativeFloat;
  p, T: word;

begin
  inherited;


  // p := round((((FPan/65535) * (0.5-0.30)) + 0.3) * 65535);
  // t := round((((FTilt/65535) * (0.5-0.2)) + 0.2) * 65535);

  pp := FPanF + FPanOffset;
  tt := FTiltF + FtiltOffset;
  if pp > 1 then
    pp := 1;
  if pp < 0 then
    pp := 0;
  if tt > 1 then
    tt := 1;
  if tt < 0 then
    tt := 0;

  p := round(pp * 255);
  T := round(tt * 255);

  FLocalChannelData[0] := (p and 255);
  // DEBUG.ConsoleLog('pan:'+ inttohex(FLocalChannelData[0],2));
  FLocalChannelData[1] := (T and 255);

end;

{ TDMXDFineMirror }

constructor TDMXDFineMirror.Create;
begin
  inherited;
  DefineChannel(1, 'Pan_HI', '');
  DefineChannel(2, 'Pan_LO', '');
  DefineChannel(3, 'Tilt_HI', '');
  DefineChannel(4, 'Tilt_LO', '');

end;

procedure TDMXDFineMirror.Init;
begin
  inherited;
  FMaxPanValue := 65535;
  FMaxTiltValue := 65535;
  FPAnTiltRatio := 2.0;


end;

procedure TDMXDFineMirror.WriteDMXData;
var
  pp, tt: NativeFloat;
  p, T: word;

begin
  inherited;
  if FLocalChannelData = nil then
    exit;


  // p := round((((FPan/65535) * (0.5-0.30)) + 0.3) * 65535);
  // t := round((((FTilt/65535) * (0.5-0.2)) + 0.2) * 65535);

  pp := FPanF + FPanOffset;
  tt := FTiltF + FtiltOffset;
  if pp > 1 then
    pp := 1;
  if pp < 0 then
    pp := 0;
  if tt > 1 then
    tt := 1;
  if tt < 0 then
    tt := 0;

  p := round(pp * 65535);
  T := round(tt * 65535);

  FLocalChannelData[0] := (p div 256) and 255;
  FLocalChannelData[1] := (p div 1) and 255;
  FLocalChannelData[2] := (T div 256) and 255;
  FLocalChannelData[3] := (T div 1) and 255;

end;

{ TDMXFlurryQ }

procedure TDMXFlurryQ.Channelchanged;
begin
  inherited;
  Fcolor.Parent := self;
  FPanTilt.Parent := self;

  Fcolor.BaseChannel := self.BaseChannel + 5;
  Fcolor.SetDataPointer(FLocalChannelData + 5);
  FPanTilt.BaseChannel := self.BaseChannel;
  FPanTilt.SetDataPointer(FLocalChannelData + 0);

end;

procedure TDMXFlurryQ.CoordinateUpdated;
begin
  inherited;

end;

constructor TDMXFlurryQ.Create;
begin
  inherited;
  FPanTilt := TDMXFineHeadMotion.Create;
  Fcolor := TDMXRGBWcluster.Create;
  DefineChannel(5, 'Dimmer', 'Leave at 255');
  DefineChannel(10, 'Color Macros', 'Leave 0');
  DefineChannel(11, 'STrobe', '0: No Strobe; Slow->Fast');
  DefineChannel(12, 'SoundActive', '0=Off; 255=0n');
end;

procedure TDMXFlurryQ.DoNewScene;
begin
  StrobeEnable := false;
end;

function TDMXFlurryQ.GetSTrobeEnable: boolean;
begin
  result := FStrobeEnable;
end;

function TDMXFlurryQ.GetSTrobeIntensity: NativeFloat;
begin
  result := Color.Intensity;
end;

function TDMXFlurryQ.GetStrobeSpeed: NativeFloat;
begin
  result := FSTrobeSpeed;
end;

procedure TDMXFlurryQ.Init;
begin
  inherited;

end;

procedure TDMXFlurryQ.SetStrobeEnable(const Value: boolean);
begin
  FStrobeEnable := Value;
end;

procedure TDMXFlurryQ.SetStrobeIntensity(const Value: NativeFloat);
begin

  // raise Exception.create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TDMXFlurryQ.SetStrobeSpeed(const Value: NativeFloat);
begin
  FSTrobeSpeed := Value;
end;

procedure TDMXFlurryQ.Strike;
begin

  // raise Exception.create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TDMXFlurryQ.StrobeStrike;
begin

  // raise Exception.create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TDMXFlurryQ.WriteDMXData;
begin
  inherited;
  self.FLocalChannelData[4] := 255;
  self.FLocalChannelData[9] := 0;
  if StrobeEnable then
    self.FLocalChannelData[10] := round(FSTrobeSpeed * 255)
  else
    self.FLocalChannelData[10] := 0;

  if FAutoMode then
    self.FLocalChannelData[11] := 255
  else
    self.FLocalChannelData[11] := 0;


  // Debug(miscroutines.MemoryDebugString(@FLocalChannelData[0], 13));

end;

{ TDMXEffectParameterDefinition }

function TDMXEffectParameterDefinition.GetAsInteger: nativeint;
begin
  result := round(rValue);
end;

function TDMXEffectParameterDefinition.GetasString: string;
begin
  if imptyp = eptString then
    result := sValue
  else
    result := floattostr(rValue);
end;

procedure TDMXEffectParameterDefinition.SetAsInteger(const Value: nativeint);
begin
  rValue := Value;
end;

{ TDMXStage }

constructor TDMXStage.Create;
begin
  inherited;
  planes := TDMXPlanes.Create;
end;

destructor TDMXStage.Destroy;
begin
  planes.free;
  planes := nil;
  inherited;
end;

procedure TDMXStage.LoadData;
var
  T: ni;
  ap: TAppParams;
  pl: TDMXPlane;
begin
  ap := NeedAppParams;
  try
    while planes.count < 16 do
    begin
      pl := TDMXPlane.Create;
      self.planes.Add(pl);
    end;

    for T := 0 to planes.count - 1 do
    begin
      planes[T].LoadData(ap, 'pl' + inttostr(T));
    end;
  finally
    NoNeedAppParams(ap);
  end;
end;

procedure TDMXStage.SaveData;
var
  T: ni;
  ap: TAppParams;
begin
  ap := NeedAppParams;
  try
    for T := 0 to planes.count - 1 do
    begin
      planes[T].SaveData(ap, 'pl' + inttostr(T));
    end;
  finally
    NoNeedAppParams(ap);
  end;

end;

procedure oinit;
begin
  MultiVerse := TDMXMultiVerse.Create;

  FirstUniverse := TDMXUniverse.Create;
  Multiverse.EffectLibrary.RegisterClass(TFXStaticCoolRate);
  Multiverse.EffectLibrary.RegisterClass(TFXStaticColor);
  Multiverse.EffectLibrary.RegisterClass(TFXStaticStripes);
  Multiverse.EffectLibrary.RegisterClass(TFXStaticIntensity);
  Multiverse.EffectLibrary.RegisterClass(TFXGridPong);
  Multiverse.EffectLibrary.RegisterClass(TFXGridFill);
  Multiverse.EffectLibrary.RegisterClass(TFXStrobe);
  Multiverse.EffectLibrary.RegisterClass(TFXStrobeBlackout);

end;

procedure ofinal;
begin
  if assigned(FirstUniverse) then
  begin
    FirstUniverse.free;
    FirstUniverse := nil;
  end;

end;

{ TDMXPLaneLight }

procedure TDMXPLaneLight.AddCalPoint(joyx, joyy, panx, pany,
  Focus: NativeFloat);
begin
  SetLength(calibrationpoints, length(calibrationpoints) + 1);
  calibrationpoints[high(calibrationpoints)].light := light;
  calibrationpoints[high(calibrationpoints)].joy.FromFloats(joyx, joyy);
  calibrationpoints[high(calibrationpoints)].Pan.FromFloats(panx, pany);

end;

procedure TDMXPLaneLight.DefaultPoints;
var
  T, u: ni;
const
  XPOINTS = 3;
  YPOINTS = 3;
begin
  SetLength(self.calibrationpoints, 9);
  for T := 0 to XPOINTS - 1 do
  begin
    for u := 0 to YPOINTS - 1 do
    begin
      self.calibrationpoints[(T * XPOINTS) + u].joy.X := T / (XPOINTS - 1);
      self.calibrationpoints[(T * XPOINTS) + u].joy.Y := u / (YPOINTS - 1);
      self.calibrationpoints[(T * XPOINTS) + u].Focus := 0.5;
      self.calibrationpoints[(T * XPOINTS) + u].Pan.X := T / (XPOINTS - 1);
      self.calibrationpoints[(T * XPOINTS) + u].Pan.Y := u / (XPOINTS - 1);
    end;
  end;

end;

procedure TDMXPLaneLight.DeleteCalPoint(idx: ni);
var
  T: ni;
begin
  if length(calibrationpoints) = 0 then
    exit;

  if idx >= 0 then
  begin
    for T := idx to length(calibrationpoints) - 2 do
    begin
      calibrationpoints[T] := calibrationpoints[T + 1];
    end;

    SetLength(calibrationpoints, length(calibrationpoints) - 1);
  end;

end;

function TDMXPLaneLight.GetNearestPoint(X, Y: NativeFloat)
  : PDMXPLaneCalibrationPoint;
var
  T: ni;
  Test, v, dif: TNativeVector4;
  dmag, rmag: NativeFloat;
begin
  if high(calibrationpoints) < 0 then
    DefaultPoints;
  result := nil;
  // result.Init;

  v.Init;
  rmag := -1;

  // this is the value we're testing against
  Test.Init;
  Test.X := X;
  Test.Y := Y;

  // go through all the points and find the one that is closest
  for T := 0 to high(calibrationpoints) do
  begin
    v.X := calibrationpoints[T].joy.X;
    v.Y := calibrationpoints[T].joy.Y;

    // skip if the same
    if v = Test then
    begin
      result := @calibrationpoints[T];
      exit;
    end;

    dif := v - Test;

    dmag := dif.Magnitude;
    if (rmag < 0) or (dmag < rmag) then
    begin
      rmag := dmag;
      result := @calibrationpoints[T];
    end;
  end;
end;

function TDMXPLaneLight.FindCCWPoint(operandpoint, nearestpoint: TNativeVector4)
  : PDMXPLaneCalibrationPoint;
var
  T: ni;
  rel, Test, v, dif: TNativeVector4;
  dmag, rmag: NativeFloat;
  dang, rang: NativeFloat;
  rang_diff, ang_diff, off_ang, off_tang: NativeFloat;
  result_v: TNativeVector4;
  bFirst: boolean;
begin
  result := nil;
  result_v.Init;
  rang_diff := 0;

  // debug.ConsoleLog('NP:'+floattostr(nearestpoint.x)+','+floattostr(nearestpoint.y));
  // debug.ConsoleLog('OP:'+floattostr(operandpoint.x)+','+floattostr(operandpoint.y));

  v.Init;
  rmag := -1;
  rang := 0;

  rel := operandpoint - nearestpoint;
  off_ang := rel.ToFloatPOint.AngleInUCs(aoRaster);

  Test := operandpoint;

  // go through all the points and find the one that is closest CCW
  bFirst := true;
  for T := 0 to high(calibrationpoints) do
  begin
    v.X := calibrationpoints[T].joy.X;
    v.Y := calibrationpoints[T].joy.Y;

    // skip if the same
    if v = nearestpoint then
      continue;

    dif := v - nearestpoint;

    // determine some magnitudes
    dmag := dif.Magnitude;
    dang := dif.ToFloatPOint.AngleInUCs(aoRaster);

    // determine angle from nearest point
    off_tang := dif.ToFloatPOint.AngleInUCs(aoRaster);
    // determine angle difference from operand point
    ang_diff := off_tang - off_ang;
    if ang_diff > 0 then
      ang_diff := ang_diff - 1;
    // if ang_diff < -0.5 then
    // ang_diff := ang_diff + 1;
    // if ang_diff > 0.5 then
    // ang_diff := ang_diff - 1;

    // if on the same line then take the shortest magnitude.
    // if (rang_diff = 0) or (rang_diff = ang_diff) then begin
    // take shortest vector
    // if (rmag < 0) or (dmag < rmag) then begin
    // rmag := dmag;
    // rang := dang;
    // rang_diff := ang_diff;
    // result_v := v;
    // result := @Self.calibrationpoints[t];
    // end;
    // end else
    // if this is within 45-degrees and closer to the source point than the
    // previous points
    if bFirst or ((ang_diff > (0 - ANG_SEARCH)) and (ang_diff < 0)) then
    begin
      if (dmag < rmag) or bFirst or (rang_diff < (0 - ANG_SEARCH)) then
      begin
        rmag := dmag;
        rang := dang;
        rang_diff := ang_diff;
        result_v := v;
        result := @self.calibrationpoints[T];
      end;
    end
    else
      // if this angle is closest to the target angle then use it
      if (ang_diff <= (0 - ANG_SEARCH)) and (ang_diff > rang_diff) and
        (ang_diff < 0) then
      begin
        // rmag := dmag;
        // rang := dang;
        // rang_diff := ang_diff;
        // result_v := v;
        // result := @Self.calibrationpoints[t];
      end;
    bFirst := false;
  end;

  // debug.ConsoleLog('CCW:'+floattostr(result_v.x)+','+floattostr(result_v.y));
end;

function TDMXPLaneLight.FindCWPoint(operandpoint, nearestpoint: TNativeVector4)
  : PDMXPLaneCalibrationPoint;
var
  T: ni;
  rel, Test, v, dif: TNativeVector4;
  dmag, rmag: NativeFloat;
  dang, rang: NativeFloat;
  rang_diff, ang_diff, off_ang, off_tang: NativeFloat;
  result_v: TNativeVector4;
  bFirst: boolean;
begin
  result := nil;
  result_v.Init;
  rang_diff := 0;

  // debug.ConsoleLog('NP:'+floattostr(nearestpoint.x)+','+floattostr(nearestpoint.y));
  // debug.ConsoleLog('OP:'+floattostr(operandpoint.x)+','+floattostr(operandpoint.y));

  v.Init;
  rmag := -1;
  rang := 0;

  rel := operandpoint - nearestpoint;
  off_ang := rel.ToFloatPOint.AngleInUCs(aoRaster);

  Test := operandpoint;

  bFirst := true;
  // go through all the points and find the one that is closest CCW
  for T := 0 to high(calibrationpoints) do
  begin

    v.X := calibrationpoints[T].joy.X;
    v.Y := calibrationpoints[T].joy.Y;

    // skip if the same
    if v = nearestpoint then
      continue;

    dif := v - nearestpoint;

    // determine some magnitudes
    dmag := dif.Magnitude;
    dang := dif.ToFloatPOint.AngleInUCs(aoRaster);

    // determine angle from nearest point
    off_tang := dif.ToFloatPOint.AngleInUCs(aoRaster);
    // determine angle difference from operand point
    ang_diff := off_tang - off_ang;
    if ang_diff < 0 then
      ang_diff := ang_diff + 1;

    // if on the same line then take the shortest magnitude.
    // if (rang_diff = 0) or (rang_diff = ang_diff) then begin
    // take shortest vector
    // if (rmag < 0) or (dmag < rmag) then begin
    // rmag := dmag;
    // rang := dang;
    // rang_diff := ang_diff;
    // result_v := v;
    // result := @Self.calibrationpoints[t];
    // end;
    // end else
    // if this is within 45-degrees and closer to the source point than the
    // previous points
    if bFirst or ((ang_diff < ANG_SEARCH) and (ang_diff > 0)) then
    begin
      if (dmag < rmag) or bFirst or (rang_diff > ANG_SEARCH) then
      begin
        rmag := dmag;
        rang := dang;
        rang_diff := ang_diff;
        result_v := v;
        result := @self.calibrationpoints[T];
      end;
    end
    else
      // if this angle is closest to the target angle then use it
      if (ang_diff >= ANG_SEARCH) and (ang_diff < rang_diff) and (ang_diff > 0)
      then
      begin
        // rmag := dmag;
        // rang := dang;
        // rang_diff := ang_diff;
        // result_v := v;
        // result := @Self.calibrationpoints[t];
      end;
    bFirst := false;
  end;

  // debug.ConsoleLog('CW:'+floattostr(result_v.x)+','+floattostr(result_v.y));
  if result = nil then
    debug.ConsoleLog('wtf');
end;

function TDMXPLaneLight.GetInterpolatedPoint_Old2(X, Y: NativeFloat)
  : TNativeVector4;
var
  nearest: PDMXPLaneCalibrationPoint;
  nearest_v: TNativeVector4;
  op: TNativeVector4;
  influence_point1, influence_point2: PDMXPLaneCalibrationPoint;
  influence1, influence2: TNativeVector4;
  angletot, anglepercent, anglepos: NativeFloat;
  magpercent1, magpercent2: NativeFloat;
  local_inf1, local_inf2, local_op: TNativeVector4;
  blend1, blend2, blend_final: NativeFloat;
  res_line1, res_line2, res_final: TNativeVector4;
  f: NativeFloat;
  ixn_x, ixn_y: NativeFloat;
begin
  op.Init;
  op.X := X;
  op.Y := Y;
  // first find the NEAREST point
  nearest := GetNearestPoint(X, Y);
  nearest_v.Init;
  nearest_v.X := nearest.joy.X;
  nearest_v.Y := nearest.joy.Y;

  // find point ccw to the nearest point
  influence_point1 := FindCCWPoint(op, nearest_v);
  influence_point2 := FindCWPoint(op, nearest_v);

  debug_primarypoint := nearest;
  debug_influence1 := influence_point1;
  debug_influence2 := influence_point2;

  influence1.Init;
  influence2.Init;
  influence1.X := influence_point1.joy.X;
  influence1.Y := influence_point1.joy.Y;
  influence2.X := influence_point2.joy.X;
  influence2.Y := influence_point2.joy.Y;

  local_op := op - nearest_v;
  local_inf1 := influence1 - nearest_v;
  local_inf2 := influence2 - nearest_v;

  // determine angle percent from CCW to CW.
  angletot := local_inf2.ToFloatPOint.AngleInUCs(aoRaster) -
    local_inf1.ToFloatPOint.AngleInUCs(aoRaster);
  anglepos := local_op.ToFloatPOint.AngleInUCs(aoRaster) -
    local_inf1.ToFloatPOint.AngleInUCs(aoRaster);
  if angletot = 0 then
    anglepercent := 0.5
  else
    anglepercent := anglepos / angletot;

  // determine magnitude percent 1
  f := local_inf1.ToFloatPOint.Magnitude;
  if f <> 0 then
    magpercent1 := local_op.ToFloatPOint.Magnitude /
      local_inf1.ToFloatPOint.Magnitude
  else
    magpercent1 := 0;

  // determine magnitude percent 2
  f := local_inf2.ToFloatPOint.Magnitude;
  if f <> 0 then
    magpercent2 := local_op.ToFloatPOint.Magnitude / f
  else
    magpercent2 := 0;

  blend1 := magpercent1;
  blend2 := magpercent2;
  blend_final := anglepercent;

  // blend line 1
  // blend1 := (local_op * (1-magpercent1)) + (local_inf1*magpercent1);

  // blend line 2
  // blend2 := (local_op * (1-magpercent2)) + (local_inf2*magpercent2);

  // final blend
  // blend_final := (blend1 * (1-anglepercent)) + (blend2*anglepercent);

  res_line1.Init;
  res_line2.Init;
  res_final.Init;
  res_line1.X := Interpolate(blend1, nearest.Pan.X, influence_point1.Pan.X);
  res_line1.Y := Interpolate(blend1, nearest.Pan.Y, influence_point1.Pan.Y);
  res_line1.z := Interpolate(blend1, nearest.Focus, influence_point1.Focus);

  res_line2.X := Interpolate(blend2, nearest.Pan.X, influence_point2.Pan.X);
  res_line2.Y := Interpolate(blend2, nearest.Pan.Y, influence_point2.Pan.Y);
  res_line2.z := Interpolate(blend2, nearest.Focus, influence_point2.Focus);

  res_final.X := Interpolate(blend_final, res_line1.X, res_line2.X);
  res_final.Y := Interpolate(blend_final, res_line1.Y, res_line2.Y);
  res_final.z := Interpolate(blend_final, res_line1.z, res_line2.z);

  result := res_final;


  // we're looking for exactly TWO points that have influence over this

end;

function TDMXPLaneLight.GetInterpolatedPoint(X, Y: NativeFloat): TNativeVector4;
var
  f: NativeFloat;
  ixn_x, ixn_y: NativeFloat;
  op: Tnativefloatpoint;
  op_vec: TNativeVector4;
  rel_vec: TNativeVector4;
  rel_hyp_vec: TNativeVector4;
  vectors: array [0 .. 2] of TNativeVector4;
  influences: array [0 .. 2] of NativeFloat;
  points: array [0 .. 2] of PDMXPLaneCalibrationPoint;
  T: ni;
  ixn_vec: TNativeVector4;
  totalinfluence: NativeFloat;
begin
  result.Init;
  vectors[0].Init;
  vectors[1].Init;
  vectors[2].Init;
  // operand
  op_vec.Init;
  op_vec.X := X;
  op_vec.Y := Y;
  op.X := X;
  op.Y := Y;

  // first find the NEAREST point
  points[0] := GetNearestPoint(X, Y);
  points[1] := points[0];
  points[2] := points[0];
  if (system.abs(points[0].joy.X - X) < 0.0001) and
    ((system.abs(points[0].joy.Y - Y) < 0.0001)) then
  begin
    result.X := points[0].Pan.X;
    result.Y := points[0].Pan.Y;
    result.z := points[0].Focus;
    exit;
  end;

  vectors[0].X := points[0].joy.X;
  vectors[0].Y := points[0].joy.Y;

  // find point ccw to the nearest point
  points[1] := FindCCWPoint(op_vec, vectors[0]);
  // find point cw to the nearest point
  points[2] := FindCWPoint(op_vec, vectors[0]);

  if points[1] = nil then
    points[1] := points[0];

  if points[2] = nil then
    points[2] := points[0];

  self.debug_primarypoint := points[0];
  self.debug_influence1 := points[1];
  self.debug_influence2 := points[2];

  // copy points to vectors
  for T := 0 to 2 do
  begin
    vectors[T].X := points[T].joy.X;
    vectors[T].Y := points[T].joy.Y;
    influences[T] := 0;
  end;

  // now that we have 3 points.... the influence of each point will be calculated as follows:
  // influence = 1-((distance_to_hyp)/distance_to_op_point)
  // for each of the selected points
  for T := 0 to 2 do
  begin

    // determine the relative vector from the triangle point
    rel_vec := vectors[T] - op_vec;

    // determine the point at which the relative_vector line intersects the hypotenuse
    if geometry.linesIntersect(vectors[T].X, vectors[T].Y, op.X, op.Y,
      vectors[(T + 1) mod 3].X, vectors[(T + 1) mod 3].Y,
      vectors[(T + 2) mod 3].X, vectors[(T + 2) mod 3].Y, ixn_x, ixn_y) then
    begin
      // calculate relative distance of hypotenuse from tri point
      rel_hyp_vec.Init;
      ixn_vec.Init;
      ixn_vec.X := ixn_x;
      ixn_vec.Y := ixn_y;
      rel_hyp_vec := ixn_vec - vectors[T];

      if rel_hyp_vec.Magnitude = 0 then
        influences[T] := 1
      else
        influences[T] := 1 - (rel_vec.Magnitude / rel_hyp_vec.Magnitude);
    end
    else
    begin
      debug.ConsoleLog('wtf');
    end;
  end;

  // build final vectors from pan/tilt/focus information
  for T := 0 to 2 do
  begin
    vectors[T].X := points[T].Pan.X;
    vectors[T].Y := points[T].Pan.Y;
    vectors[T].z := points[T].Focus;
  end;

  // calculate final influences
  result.Init;
  totalinfluence := 0;
  for T := 0 to 2 do
  begin
    // multiply each vector by scalar influences
    result := result + (vectors[T] * influences[T]);
    totalinfluence := totalinfluence + influences[T];

  end;

  // normalize the resulting vector
  if totalinfluence <> 0 then
    result := result / totalinfluence
  else
    result.Init;

end;

function TDMXPLaneLight.GetInterpolatedPoint_Old(X, Y: NativeFloat)
  : TNativeVector4;
var
  T, u, v: ni;
  results: array of TNativeVector4;
  idx: ni;
  bala, balb, balsum: NativeFloat;
  pa, pb, pc, pp: Tnativefloatpoint;
  Max: NativeFloat;
begin
  SetLength(results, length(calibrationpoints));
  idx := 0;
  pp.X := X;
  pp.Y := Y;
  for T := 0 to High(calibrationpoints) do
  begin
    pa := self.calibrationpoints[T].joy;
    for u := 0 to high(calibrationpoints) do
    begin
      pb := self.calibrationpoints[u].joy;
      if pb = pa then
        continue;
      for v := 0 to high(calibrationpoints) do
      begin
        if (T = 0) and (u = 1) then
          self.calibrationpoints[v].added := false;
        pc := self.calibrationpoints[v].joy;
        if pc = pa then
          continue;
        if pc = pb then
          continue;

        if not(self.calibrationpoints[v].added) then
          if geometry.Tnativefloatpoint.GetAngularInterpolated_Balance(pa - pc,
            pb - pc, pp - pc, bala, balb) then
          begin
            results[idx].X := self.calibrationpoints[v].Pan.X;
            results[idx].Y := self.calibrationpoints[v].Pan.Y;
            results[idx].z := self.calibrationpoints[v].Focus;
            results[idx].W := 1 - bala;
            self.calibrationpoints[v].added := true;
            inc(idx);
            // results[idx].x := self.calibrationpoints[u].pan.x;
            // results[idx].y := self.calibrationpoints[u].pan.y;
            // results[idx].z := self.calibrationpoints[u].focus;
            // results[idx].w := balb;
            // inc(idx);

          end;
      end;

    end;
  end;

  // once we've searched everything perform weighted averages

  result.Init;
  balsum := 0;
  v := 0;
  for u := 0 to 6 do
  begin
    Max := 0;

    for T := 0 to idx - 1 do
    begin
      if results[T].W > 0 then
      begin
        if results[T].W > Max then
        begin
          v := T;
          Max := results[T].W;
        end;
      end;
    end;

    if Max > 0 then
    begin
      result := result + results[v] * results[v].W;
      balsum := balsum + results[v].W;
      results[v].W := -1;
    end;
  end;

  if balsum <> 0 then
    result := result / balsum;

end;

{ TDMXPlane }

procedure TDMXPlane.AddLight(l: TDMXChannelCluster);
begin
  SetLength(LIghts, length(LIghts) + 1);
  LIghts[high(LIghts)].light := l;
end;

function TDMXPlane.GetLightData(l: TDMXChannelCluster): PDMXPLaneLight;
var
  i: ni;
begin
  i := INdexOfLight(l);
  if i < 0 then
    result := nil
  else
    result := @LIghts[i];

end;

procedure TDMXPlane.GetLights;
var
  al: TUniverseList<TDMXTiltPan>;
  T: ni;
begin
  SetLength(self.LIghts, 0);

  al := FirstUniverse.GetAspectList<TDMXTiltPan>;
  for T := 0 to al.count - 1 do
  begin
    self.AddLight(al[T]);
  end;

end;

function TDMXPlane.INdexOfLight(l: TDMXChannelCluster): ni;
var
  T: ni;
begin
  result := -1;
  for T := low(LIghts) to high(LIghts) do
  begin
    if LIghts[T].light = l then
    begin
      result := T;
      exit;
    end;
  end;

end;

procedure TDMXPlane.LoadData(nvpl: TNameValuePairList; sPrefix: string);
var
  T: ni;
  u: ni;
begin
  GetLights;

  try
    for T := 0 to length(LIghts) - 1 do
    begin
      SetLength(LIghts[T].calibrationpoints,
        nvpl.getitemex(sPrefix + '.' + inttostr(T) + '.cals.count', 0));
      for u := 0 to high(LIghts[T].calibrationpoints) do
      begin
        LIghts[T].calibrationpoints[u].Pan.X :=
          nvpl.getitemex(sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.panx', 0.0);
        LIghts[T].calibrationpoints[u].Pan.Y :=
          nvpl.getitemex(sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.pany', 0.0);
        LIghts[T].calibrationpoints[u].joy.X :=
          nvpl.getitemex(sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.joyx', 0.0);
        LIghts[T].calibrationpoints[u].joy.Y :=
          nvpl.getitemex(sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.joyy', 0.0);
        LIghts[T].calibrationpoints[u].Focus :=
          nvpl.getitemex(sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.focus', 0.0);
      end;
    end;
  finally
  end;

end;

procedure TDMXPlane.SaveData(nvpl: TNameValuePairList; sPrefix: string);
var
  T: ni;
  u: ni;

begin
  try
    for T := 0 to high(LIghts) do
    begin
      nvpl.items[sPrefix + '.' + inttostr(T) + '.cals.count'].AsFloat :=
        length(LIghts[T].calibrationpoints);
      for u := 0 to high(LIghts[T].calibrationpoints) do
      begin
        nvpl.items[sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.panx'].AsFloat := LIghts[T].calibrationpoints[u].Pan.X;
        nvpl.items[sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.pany'].AsFloat := LIghts[T].calibrationpoints[u].Pan.Y;
        nvpl.items[sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.joyx'].AsFloat := LIghts[T].calibrationpoints[u].joy.X;
        nvpl.items[sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.joyy'].AsFloat := LIghts[T].calibrationpoints[u].joy.Y;
        nvpl.items[sPrefix + '.' + LIghts[T].light.ClassName + '@' +
          inttostr(LIghts[T].light.BaseChannel) + '.cals.' + inttostr(u) +
          '.focus'].AsFloat := LIghts[T].calibrationpoints[u].Focus;
      end;
    end;
  finally

  end;
end;

{ TDMXcolor7 }

procedure TDMXcolor7.WriteDMXData;
begin
  inherited;
  FLocalChannelData[3] := 0;
  FLocalChannelData[4] := 0;
  FLocalChannelData[5] := 0;
  FLocalChannelData[6] := 255;
end;

{ TDMXDimmer }

constructor TDMXDimmer.Create;
begin
  inherited;
  DefineChannel(1, 'Intensity');
end;

function TDMXDimmer.GetburnRate: NativeFloat;
begin
  result := FBurnRate;
end;

function TDMXDimmer.GetcoolRate: NativeFloat;
begin
  result := fcoolRate;
end;

function TDMXDimmer.GetIntensity: NativeFloat;
begin
  result := FIntensity;
end;

function TDMXDimmer.GetLumens: NativeFloat;
begin
  result := fLumens;
end;

function TDMXDimmer.GEtMaxIntensity: NativeFloat;
begin
  result := FMaxIntensity;

end;

function TDMXDimmer.GEtMinIntensity: NativeFloat;
begin
  result := FMinIntensity;
end;

function TDMXDimmer.GetStrobeOverdrive: boolean;
begin
  result := FSTrobeOverdrive;
end;

function TDMXDimmer.GetTargetIntensity: NativeFloat;
begin
  result := FTargetIntensity;
end;

procedure TDMXDimmer.Init;
begin
  inherited;
  FMaxIntensity := 1.0;
  FBurnRate := 32;
  fcoolRate := 32;
  fLumens := 1.0;
end;

procedure TDMXDimmer.SetBurnRate(const Value: NativeFloat);
begin
  FBurnRate := Value;
end;

procedure TDMXDimmer.SetCoolRate(const Value: NativeFloat);
begin
  fcoolRate := Value;
end;

procedure TDMXDimmer.SetIntensity(i: NativeFloat);
begin
  FIntensity := i;
end;

procedure TDMXDimmer.SetLumens(const i: NativeFloat);
begin
  fLumens := i;
end;

procedure TDMXDimmer.SetMaxIntensity(const Value: NativeFloat);
begin
  FMaxIntensity := Value;
end;

procedure TDMXDimmer.SetMinIntensity(const Value: NativeFloat);
begin
  FMinIntensity := Value;
end;

procedure TDMXDimmer.SetStrobeOverdrive(const Value: boolean);
begin
  FSTrobeOverdrive := Value;
end;

procedure TDMXDimmer.SetTargetIntensity(const i: NativeFloat);
begin
  FTargetIntensity := i;
end;

procedure TDMXDimmer.UpdatePhysics(enginetime: int64);
var
  R, rr: NativeFloat;
begin
  inherited;
  if FTargetIntensity > (1.0 / RelativeLumens) then
    FOverDrive := FTargetIntensity - (1.0 / RelativeLumens)
  else
    FOverDrive := 0;

  if FTargetIntensity > FIntensity then
  begin
    R := FIntensity;
    rr := R + (BURNRATE * (FDeltaTime / 1000));
    if rr > FTargetIntensity then
      rr := FTargetIntensity;

    if rr > 1.0 then
    begin
      rr := 1.0;
    end;

    FIntensity := ((rr * (FMaxIntensity - FMinIntensity)) + FMinIntensity);

  end
  else if FTargetIntensity < FIntensity then
  begin
    R := FIntensity;
    rr := R - (COOLRATE * (FDeltaTime / 1000));
    if rr < FTargetIntensity then
      rr := FTargetIntensity;

    if rr < 0.0 then
      rr := 0.0;

    FIntensity := ((rr * (FMaxIntensity - FMinIntensity)) + FMinIntensity);;

  end;

  if FOverDrive > 0 then
  begin
    if (((enginetime + round(50 * (PX + PY)) div 50)) mod 3) <> 0 then
    begin
      FIntensity := 1 - FOverDrive;
      if FIntensity < 0 then
        FIntensity := 0;
    end;
  end;

end;

procedure TDMXDimmer.WriteDMXData;
begin
  inherited;
  FLocalChannelData[0] := round(lesserof(self.Intensity * 255, 255));
end;

{ TDMXAccuSpot250ColorChannel }

constructor TDMXAccuSpot250ColorChannel.Create;
begin
  inherited;
  AddtoPallete(0, $FFFFFF);
  AddtoPallete(20, $0000FF);
  AddtoPallete(40, $FF0000);
  AddtoPallete(60, $00FFFF);
  AddtoPallete(80, $00FF00);
  AddtoPallete(100, $FFFF00);
  AddtoPallete(120, $007F00);
  AddtoPallete(160, $7F00FF);
  AddtoPallete(140, $FF7FFF);
  AddtoPallete(180, $FF003F);
end;

{ TDMXAccuSpot250 }

procedure TDMXAccuSpot250.Channelchanged;
begin
  inherited;

  FMotion.BaseChannel := BaseChannel;
  FMotion.SetDataPointer(FLocalChannelData + 0);
  FMotion.Parent := self;

  Fcolor.BaseChannel := BaseChannel + 2;
  Fcolor.SetDataPointer(FLocalChannelData + 2);
  Fcolor.Parent := self;

end;

procedure TDMXAccuSpot250.CoordinateUpdated;
begin
  inherited;
  Fcolor.X := self.X;
  Fcolor.Y := self.Y;
  Fcolor.PX := self.PX;
  Fcolor.PY := self.PY;
  Fcolor.XX := self.XX;
  Fcolor.YY := self.YY;

end;

constructor TDMXAccuSpot250.Create;
begin
  inherited;

  FRelativeLumens := 1;
  FMotion := TDMXCoarseHeadMotion.Create;
  Fcolor := TDMXAccuSpot250ColorChannel.Create;
  FMotion.Parent := self;
  Fcolor.Parent := self;
  FBurnRate := 32;
  fcoolRate := 16;
  FMaxIntensity := 1.0;
  FSpeed := 0.1;

  CoordinateUpdated;

end;

destructor TDMXAccuSpot250.Destroy;
begin
  FMotion.free;
  Fcolor.free;

  inherited;
end;

function TDMXAccuSpot250.GetburnRate: NativeFloat;
begin
  result := FBurnRate;
end;

function TDMXAccuSpot250.GetcoolRate: NativeFloat;
begin
  result := fcoolRate;
end;

function TDMXAccuSpot250.GetgoboSPinValue: byte;
begin
  if FgoboSpin < -1 then
    FgoboSpin := -1;

  if FgoboSpin > 1 then
    FgoboSpin := 1;

  if FgoboSpin < 0 then
  begin
    result := round(136 + ((255 - 136) * system.abs(FgoboSpin)));
  end
  else if FgoboSpin > 0 then
  begin
    result := round(8 + (127 - 8) * (1 - system.abs(FgoboSpin)));

  end
  else
    result := 0;

end;

function TDMXAccuSpot250.GetIntensity: NativeFloat;
begin
  result := FTargetIntensity;
end;

function TDMXAccuSpot250.GetLumens: NativeFloat;
begin
  result := FRelativeLumens;
end;

function TDMXAccuSpot250.GEtMaxIntensity: NativeFloat;
begin
  result := FMaxIntensity;
end;

function TDMXAccuSpot250.GEtMinIntensity: NativeFloat;
begin
  result := FMinIntensity;
end;

function TDMXAccuSpot250.GetSTrobeEnable: boolean;
begin
  result := FStrobe > 0;
end;

function TDMXAccuSpot250.GetSTrobeIntensity: NativeFloat;
begin
  result := 1;
end;

function TDMXAccuSpot250.GetStrobeOverdrive: boolean;
begin
  result := false;
end;

function TDMXAccuSpot250.GetStrobeSpeed: NativeFloat;
begin
  result := FSTrobeSpeed;
end;

function TDMXAccuSpot250.GetTargetIntensity: NativeFloat;
begin
  result := FTargetIntensity;
end;

procedure TDMXAccuSpot250.Reset;
begin

  raise Exception.Create('unimplemented');
  // TODO -cunimplemented: unimplemented block
end;

procedure TDMXAccuSpot250.SetBurnRate(const Value: NativeFloat);
begin
  FBurnRate := Value;
end;

procedure TDMXAccuSpot250.SetCoolRate(const Value: NativeFloat);
begin
  fcoolRate := Value;
end;

procedure TDMXAccuSpot250.SetIntensity(Value: NativeFloat);
begin
  FTargetIntensity := Value;
end;

procedure TDMXAccuSpot250.SetLumens(const i: NativeFloat);
begin
  FRelativeLumens := i;
end;

procedure TDMXAccuSpot250.SetMaxIntensity(const Value: NativeFloat);
begin
  FMaxIntensity := Value;
end;

procedure TDMXAccuSpot250.SetMinIntensity(const Value: NativeFloat);
begin
  FMinIntensity := Value;
end;

procedure TDMXAccuSpot250.SetStrobeEnable(const Value: boolean);
begin
  FStrobe := booltoint(Value);
end;

procedure TDMXAccuSpot250.SetStrobeIntensity(const Value: NativeFloat);
begin
  // always 1
end;

procedure TDMXAccuSpot250.SetStrobeOverdrive(const Value: boolean);
begin
  // always false
end;

procedure TDMXAccuSpot250.SetStrobeSpeed(const Value: NativeFloat);
begin
  FSTrobeSpeed := Value;
end;

procedure TDMXAccuSpot250.SetTargetIntensity(const Value: NativeFloat);
begin
  FTargetIntensity := Value;
end;

procedure TDMXAccuSpot250.StrobeStrike;
begin
  FStrokeSTrike := true;
end;

procedure TDMXAccuSpot250.UpdatePhysics(enginetime: int64);
var
  R, rr: NativeFloat;
begin
  inherited;
  if FReset then
  begin
    if FResetTime = -1 then
      FResetTime := enginetime
    else if enginetime - FResetTime > 10000 then
      FReset := false;

    exit;

  end;
  if FTargetIntensity > FIntensity then
  begin
{$IFNDEF LOG_BURN}
    R := FIntensity;
    rr := R + (BURNRATE * (FDeltaTime / 1000));
{$ELSE}
    R := abs(FTargetIntensity - FIntensity);
    rr := FIntensity + (R * (BURNRATE * (FDeltaTime / 1000)));
{$ENDIF}
    if rr > FTargetIntensity then
      rr := FTargetIntensity;

    // if rr > 1.0 then
    // rr := 1.0;

    FIntensity := (rr * (FMaxIntensity - FMinIntensity)) + FMinIntensity;

  end
  else if FTargetIntensity < FIntensity then
  begin
{$IFNDEF LOG_BURN}
    R := FIntensity;
    rr := R - (COOLRATE * (FDeltaTime / 1000));
{$ELSE}
    R := abs(FTargetIntensity - FIntensity);
    rr := FIntensity - (R * (COOLRATE * (FDeltaTime / 1000)));
{$ENDIF}
    if rr < FTargetIntensity then
      rr := FTargetIntensity;

    if rr < 0.0 then
      rr := 0.0;

    FIntensity := (rr * (FMaxIntensity - FMinIntensity)) + FMinIntensity;;
  end;
end;

procedure TDMXAccuSpot250.WriteDMXData;
begin
  inherited;

  FLocalChannelData[3] := Fgobo * 14;
  FLocalChannelData[4] := GetgoboSPinValue;
  if FStrobeBlackout > 0 then
    FLocalChannelData[5] := 0
  else
    FLocalChannelData[5] := round(lesserof(FIntensity, 1) * (63 - 31)) + 31;

  FLocalChannelData[6] := 60;
end;

{ TDMXExternalJoyControl }

procedure TDMXExternalJoyControl.Init;
begin
  FPan := 0.5;
  FTilt := 0.5;
end;

procedure TDMXExternalJoyControl.SetPan(const Value: NativeFloat);
begin
  FPan := Value;
end;

procedure TDMXExternalJoyControl.SetTilt(const Value: NativeFloat);
begin
  FTilt := Value;
end;

{ TDMXCalienteColorMacroChannel }

constructor TDMXCalienteColorMacroChannel.Create;
begin
  inherited;
  AddtoPallete(0, $FFFFFF);
end;

{ TDMXCaliente }

procedure TDMXCaliente.Channelchanged;
begin
  inherited;
  FPanTilt.BaseChannel := BaseChannel + 3;
  FPanTilt.SetDataPointer(FLocalChannelData + 3);
  Fcolor.BaseChannel := BaseChannel + 8;
  Fcolor.SetDataPointer(FLocalChannelData + 8);
end;

constructor TDMXCaliente.Create;
begin
  inherited;
  Fcolor := TDMXCalienteColorMacroChannel.Create;
  Fcolor.Parent := self;
  FUsePicturemap := false;
  Fcolor.UsePictureMap := false;
  FPanTilt := TDMXCoarseMirror.Create;
  FPanTilt.Parent := self;

  DefineChannel(1, 'Pattern Type');
  DefineChannel(2, 'Pattern');
  DefineChannel(3, 'unknown');
  DefineChannel(4, 'unknown2');
  DefineChannel(5, 'Scan Speed');
  DefineChannel(6, 'unknown');
  DefineChannel(7, 'Scan Speed 2');
  DefineChannel(8, 'Size');
  DefineChannel(9, 'Color Number');
  ChannelCount := 9;

end;

destructor TDMXCaliente.Destroy;
begin
  Fcolor.free;
  Fcolor := nil;
  FPanTilt.free;
  FPanTilt := nil;
  inherited;
end;

procedure TDMXCaliente.Detach;
begin
  inherited;

end;

procedure TDMXCaliente.DoNewScene;
begin
  Pattern := 0;
  ScanSpeed := 0;
  AnimationSpeed := 0;
  ColorNumber := 0;
  PatternType := 0;
  Size := 0;
end;

procedure TDMXCaliente.Init;
begin
  inherited;

end;

procedure TDMXCaliente.WriteDMXData;
begin
  inherited;
  FLocalChannelData[0] := FPatternType;
  FLocalChannelData[1] := FPattern;
  // FLocalChannelData[2] := FControlMode;
  // FLocalChannelData[3] := FControlMode;
  FLocalChannelData[4] := round((1 - FScanSpeed) * 255);
  FLocalChannelData[5] := 0;
  FLocalChannelData[6] := round((1 - FScanSpeed) * 255);
  FLocalChannelData[7] := round(FSize * 255);
  FLocalChannelData[8] := FColorNumber;

end;

{ TDMX4Banger }

procedure TDMX4Banger.Channelchanged;
begin
  inherited;
  PatternType := 0;
  Pattern := 0;
end;

constructor TDMX4Banger.Create;
begin
  inherited;
  DefineChannel(1, 'Pattern Type');
  DefineChannel(2, 'Pattern');


end;

destructor TDMX4Banger.Destroy;
begin

  inherited;
end;

procedure TDMX4Banger.Detach;
begin
  inherited;

end;

procedure TDMX4Banger.DoNewScene;
begin
  inherited;

end;

procedure TDMX4Banger.Init;
begin
  inherited;


end;

procedure TDMX4Banger.WriteDMXData;
begin
  inherited;
  FLocalChannelData[0] := FPatternType;
  FLocalChannelData[1] := FPattern;
  // FLocalChannelDAta[2] := Fpattern;
  // FLocalChannelDAta[3] := Fpattern;
  // FLocalChannelDAta[4] := Fpattern;
  // FLocalChannelDAta[5] := Fpattern;
  // FLocalChannelDAta[6] := Fpattern;
  // FLocalChannelDAta[7] := Fpattern;
  // FLocalChannelDAta[8] := Fpattern;
  // FLocalChannelDAta[9] := Fpattern;
  // FLocalChannelDAta[0] := Fpattern;

end;

{ TDMXMultiVerse }

constructor TDMXMultiVerse.Create;
var
  t: ni;
begin
  inherited;
  FFlatLights := TDMXChannelClusterList.create;
  FGroups := TDMXLightGroups.Create;

  FStage := TDMXStage.Create;

  FEffectLibrary := TEffectLibrary.Create;
  FEffects := TDMXEffects.Create;
  DefaultBurn := 8;
  DefaultCool := 8;
  Defaultcolorfade := 0.15;
  for T := 0 to high(Fjoys) do
  begin
    Fjoys[T] := TDMXExternalJoyControl.Create;
  end;

  for T := 0 to high(FEffectsGroups) do
  begin
    FEffectsGroups[T] := TDMXEffects.Create;
  end;


end;

procedure TDMXMultiVerse.Detach;
var
  t: ni;
begin
  if detached then exit;

  Stop;

  FGroups.free;
  FGroups := nil;

  FEffects.clear;
  FEffects.free;

  FEffectLibrary.free;

  for T := 0 to high(Fjoys) do
  begin
    Fjoys[T] := TDMXExternalJoyControl.Create;
  end;

  FFlatLights.free;

  inherited;

end;

function TDMXMultiVerse.FindArtNet(ip1, ip2, ip3, ip4: byte): TDMXUniverse;
var
  T: ni;
  sIP: string;
begin
  result := nil;
  sIP := ip1.tostring + '.' + ip2.tostring + '.' + ip3.tostring + '.' +
    ip4.tostring;
  Lock;
  try
    for T := 0 to count - 1 do
    begin
      if self[T].ArtNetIP = sIP then
        exit(self[T]);
    end;
  finally
    Unlock;
  end;

end;

procedure TDMXMultiVerse.Update;
var
  T: ni;
begin
  for T := 0 to count - 1 do
  begin
    self[T].Update;
  end;
end;

{ TDMXPixelWall }

procedure TDMXPixelWall.Channelchanged;
var
  T: integer;
begin
  inherited;
  for T := 0 to PixelCount - 1 do
  begin
    FPixels[T].BaseChannel := BaseChannel + (T * 3);
    FPixels[T].SetDataPointer(FLocalChannelData + (T * 3));
    FPixels[T].Parent := self;
  end;
end;

procedure TDMXPixelWall.CoordinateUpdated;
var
  x,y: integer;
  xx,yy: single;
begin
  inherited;
  for Y := 0 to Dimensions.y-1 do begin
    yy := (y - (dimensions.y/2))/4;
    for x := 0 to Dimensions.x-1 do begin
      xx := (x - (dimensions.x/2))/4;
      self.pixels[x,y].x := self.x+xx;
      self.pixels[x,y].y := self.y+yy;
    END;
  END;
end;

constructor TDMXPixelWall.Create;
var
  T: ni;
  XX, YY: ni;
begin
  inherited;
  DefineDimensions;

  FPixels := TList<TDMXPixel>.Create;
  for T := 0 to (Dimensions.X * Dimensions.Y) - 1 do
  begin
    XX := T mod Dimensions.X;
    YY := T div Dimensions.X;
    FPixels.Add(TDMXPixel.Create);
//    FSubclusters.Add(FPixels[T]);
    // FPixels[t].xy := t;
    FPixels[T].Parent := self;
    FPixels[T].X := self.X + (XX / Dimensions.Y);
    FPixels[T].Y := self.Y + ((YY - 20) / 10);
    FPixels[T].RelativeLumens := 1.0;
  end;

end;

destructor TDMXPixelWall.Destroy;
begin
  FPixels.free;
  inherited;
end;

procedure TDMXPixelWall.Detach;
var
  T: ni;
begin
  if detached then
    exit;

  for T := 0 to PixelCount - 1 do
  begin
    FPixels[T].SafeFree;
  end;

  inherited;

end;

function TDMXPixelWall.GetDimensions: TPoint;
begin
  case orientation of
    loNormal, lo180: exit(FDimensionsBase);
    lo90Left, lo90right: exit(point(FDimensionsBase.y, FDimensionsBase.x));
  end;
end;

function TDMXPixelWall.GetPixel(X, Y: ni): TDMXPixel;
begin
  result := nil;
  case orientation of
    loNormal: begin
      result := GetPixelBase(x,y);
    end;
    lo90Left: begin
      result := GetPixelBase((dimensions.y-1)-y,x);
    end;
    lo90Right:
      raise ECritical.Create('unimplemented');
    lo180: begin
      raise ECritical.Create('unimplemented');
    end;
  end;
end;

function TDMXPixelWall.GetPixelBase(X, Y: ni): TDMXPixel;
begin
  result := FPixels[(Y * FDimensionsBase.X) + X];
end;

function TDMXPixelWall.GetPixelCount: ni;
begin
  result := FDimensionsBase.X * FDimensionsBase.Y;
end;

function TDMXPixelWall.GetPixelLinear(idx: ni): TDMXPixel;
begin
  result := FPixels[idx];
end;

function TDMXPixelWall.GetSTrobeEnable: boolean;
begin
  result := FStrobeEnable;
end;

function TDMXPixelWall.GetSTrobeIntensity: NativeFloat;
begin
  result := FstrobeIntensity;
end;

function TDMXPixelWall.GetStrobeSpeed: NativeFloat;
begin
  result := FStrobeSpeed;
end;

procedure TDMXPixelWall.OrientationChanged;
begin
  CoordinateUpdated;
end;

procedure TDMXPixelWall.SetOrientation(const Value: TLightOrientation);
begin
  FOrientation := Value;
  orientationchanged;
end;

procedure TDMXPixelWall.SetStrobeEnable(const Value: boolean);
begin
  FStrobeEnable := value;
end;

procedure TDMXPixelWall.SetStrobeIntensity(const Value: NativeFloat);
begin
  FStrobeintensity := value;
end;

procedure TDMXPixelWall.SetStrobeSpeed(const Value: NativeFloat);
begin
  FStrobeSpeed := value;
end;

procedure TDMXPixelWall.StrobeStrike;
begin
  strobestate := true;
end;

procedure TDMXPixelWall.UpdatePhysics(enginetime: Int64);
var
  t: ni;
  px: TDMXPixel;
begin
  inherited;
  if strobeenable then begin
    for t:= 0 to pixelcount-1 do begin
      px := pixels_linear[t];
      if strobestate then begin
        px.Color := clWhite;
      end else begin
        px.Color := clBlack;
      end;
      px.Intensity := strobeintensity;
    end;

    if gettimesince(Self.tmLastStrobeEvent) > (round(1000-(950*StrobeSpeed)))  then begin
      strobestate := not strobestate;
      tmLastStrobeEvent := getTicker;
    end;
  end;
end;

procedure TDMXPixelWall.WriteDMXData;
begin
  inherited;
  //
end;

{ TDMXPixelWall2 }

{ TDMXPixellicious }

procedure TDMXPixellicious.DefineDimensions;
begin
  FDimensionsBase := point(40,4);
end;

{ TDMXPixellicious2 }

procedure TDMXPixellicious2.DefineDimensions;
begin
  FDimensionsBase := point(12, 12);
end;


initialization

orderlyinit.Init.RegisterProcs('dmx_objects', oinit, ofinal,
  'managedthread,artnet');

finalization

end.
