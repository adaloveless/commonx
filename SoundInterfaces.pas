unit SoundInterfaces;
{$I DelphiDefs.inc}
{$Message '********************************Compiling SoundInterfaces'}
{$IFNDEF CPUX64}
  {$DEFINE PURE_TSTEREOSOUNDSAMPLE}
{$ENDIF CPUX64}
{$DEFINE FREEWHEEL2}
{$DEFINE SAFE_OVERSAMPLE_WIEGHT}
{$DEFINE NEW_RESAMPLE}
{$DEFINE USE_READ_FUNC}

interface

uses
{$IFDEF NEED_FAKE_ANSISTRING}
  ios.stringx.iosansi,
{$ENDIF}
  system.types, soundwavesampler,
  typex, betterobject, sharedobject, classes, generics.collections.fixed,
  commandprocessor, math, geometry, multibuffermemoryfilestream,
  memoryfilestream, commandicons, sysutils, soundconversions_commandline,
{$IFDEF MSWINDOWS}
  fftw_interface,
{$ENDIF}
  debug, helpers_stream, numbers, systemx, soundsample;

const
  INVALID_RESTING_POINT: single = -9999.0;
  MAX_STREAM_BUFFER_SIZE = 44100*2;
  VU_HISTORY_SIZE = 2048;
  MAX_CHANNELS = 2;
  MODIFIER_BUFFER_COUNT=64;
  DEFAULT_OVERSAMPLE_COUNT = 2;
  VU_BUFFER_SIZE = 1024;
  DEFAULT_MODIFIER_BUFFER_SIZE = 4096;
  FW_JUMP_THRESHOLD = 2000;
  FW_DEBOUNCE_COUNT = 16;
  SOUND_IDX_TAG = 0;
  SOUND_IDX_VERSION = 4;
  SOUND_IDX_CHANS = 6;
  SOUND_IDX_LENGTH = 8;
  SOUND_IDX_CUE1 = 12;
  SOUND_IDX_CUE2 = 16;
  SOUND_IDX_LOOP1 = 20;
  SOUND_IDX_LOOP2 = 24;
  SOUND_IDX_SAMPLE_RATE = 28;
  SOUND_IDX_DATA_v2 = 28;
  SOUND_IDX_DATA_v3 = 32;

type
  TSoundPlaybackInfo = record
    Fsamplerate: integer;
    channels: integer;
    fmt: TXSoundFormat;
    procedure Init;
  public

    procedure SetSampleRate(const Value: integer);
    property SampleRate: integer read FSampleRate write SetSampleRate;
  end;
  PSoundPlaybackInfo = ^TSoundPlaybackInfo;
  TComplexFrequencyArray = array[0..MAX_CHANNELS-1] of PAfftw_complex;
  ToscMessageType = (mtGetSample, mtBeginWindow, mtEndWindow, mtAttack, mtRelease);
  TSoundOscillatorHook = procedure(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64; bIgnoreMods: boolean) of object;
  TSoundOscillatorHookSSE = procedure(mt: ToscMessageType; out res: TStereoSoundSample; iSampletime: int64; bIgnoreMods: boolean) of object;
  TsoundOscillator = class;//forward
  TOscillatorObject = class;//forward

  ISoundOscillatorRenderer = interface(IUnknown)
    function GetPlayPosition: int64;
    function GetBufferSize: nativeint;
    procedure SetBufferSize(const value:nativeint);
    function GEtNyquistFrequency: nativeint;
    function GEtSampleRate: nativeint;
    property SampleRate: nativeint read GEtSampleRate;
    property NyquistFrequency: nativeint read GEtNyquistFrequency;
    property Buffersize: nativeint read GetBufferSize write SetBufferSize;
    property PlayPosition: int64 read GetPlayPosition;
//    function AddOscillator(p: TSoundOscillatorHookSSE; o: TObject): TSoundOscillator; overload;
    function AddOscillator(p: TSoundOscillatorHookSSE; o: TObject): TSoundOscillator; overload;
    procedure AddOscillator(o: ToscillatorObject); overload;
    procedure RemoveOscillator(o: ToscillatorObject); overload;
    procedure RemoveOscillator(p: TSoundOscillatorHookSSE; o: TObject); overload;


    function GetEngineSTartTime: int64;
    procedure SetEngineStartTime(const Value: int64);
    property EngineStartTime : int64 read GetEngineSTartTime write SetEngineStartTime;

  end;


  TSoundOscillator = class(TSharedObject)
  private
    [unsafe] FThr: ISoundOscillatorRenderer;
    FObject: TObject;

  protected
    FBufL: array [0 .. (VU_BUFFER_SIZE - 1)] of nativefloat;
    FBufR: array [0 .. (VU_BUFFER_SIZE - 1)] of nativefloat;
    FVUPos: nativeint;
    FHookSSE: TSoundOscillatorHookSSE;

  public
    dev: PSoundPlaybackInfo;
    constructor Create;override;
    destructor Destroy;override;
    property HookSSE: TSoundOscillatorHookSSE read FhookSSE write FhookSSE;
    property Obj: TObject read FObject write FObject;
    procedure GetVU(out rL, rR: nativefloat);
    property thr: ISoundOscillatorRenderer read FThr write FThr;

    procedure Fill(iBufferPos: integer; mt: ToscMessageType; out res: TStereoSoundSample; iSampleTime: int64);overload;

  end;



  TSoundModifierOscillator = class; //forward

  TSoundCacheRecord = record
    Sample: TStereoSoundSample;
    Time: int64;
    procedure Init;
  end;
  TSoundCacheArray = array [0..0] of TSoundCacheRecord;
  PSoundCacheArray = ^TSoundCacheArray;
  PSoundCacheRecord = ^TSoundCacheRecord;


  ToscillatorObject = class(TSharedObject)
  strict private
    FVUBuffer: array [0 .. VU_HISTORY_SIZE] of TStereoSoundSample;

  private
    [unsafe] FMasterStream: ISoundOscillatorRenderer;
    FEngineStartedat: int64;
    FOwnedObjects: TList;
    FModifiers: TList<TSoundModifierOscillator>;
    FInSAmpling: boolean;

    FREsultCacheSize: nativeint;
    FCachedREsults: PSoundCacheArray;
    FCacheResults: boolean;
    FOnBeginSampling: TNotifyEvent;
    FOnEndSampling: TNotifyEvent;
    FTag: cardinal;

    function GetHasModifiers: boolean;
    function GetModifierCount: integer;

    procedure FreeResultCache;
    procedure SetResultCacheSize(const Value: nativeint);
    procedure SetCacheResults(const Value: boolean);

  protected
    function MasterStreamSampleRate: ni;

    procedure BeforeDestruction;override;
    procedure SetMasterStream(const Value: ISoundOscillatorRenderer);virtual;
    procedure SourcesChanged;virtual;


    property ResultCacheSize: nativeint read FREsultCacheSize write SetResultCacheSize;


  public
//    dev: PSoundPlaybackInfo;
    property Tag: cardinal read FTag write FTag;
    procedure o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64); virtual; abstract;


    property CacheREsults: boolean read FCacheResults write SetCacheResults;

    procedure GetVUPeaks(out rLeft, rRight: nativefloat);
    procedure GetVUHistory(iPos: nativeint; out rLeft, rRight: nativefloat);

    procedure Init;override;
    procedure Detach;override;
    destructor Destroy; override;
    property MasterStream: ISoundOscillatorRenderer read FMasterStream write SetMasterStream;
    procedure oo_resample(mt: ToscMessageType; out ss: TStereoSoundSample; iSampleTime: double);

    procedure oo(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64; bIgnoreMods: boolean);


    property EngineStartedAT
      : int64 read FEngineStartedat write FEngineStartedat;
    procedure AddModifier(o: TSoundModifierOscillator; bTakeOwnership: boolean);
    procedure RemoveModifier(o: TSoundModifierOscillator);
    property HasModifiers: boolean read GetHasModifiers;
    property ModifierCount: integer read GetModifierCount;
    procedure REmapSources;

    property OnBeginSampling: TNotifyEvent read FOnBeginSampling write FOnBeginSampling;
    property OnEndSampling: TNotifyEvent read FOnEndSampling write FOnEndSampling;

    procedure DoOnBeginSampling;virtual;
    procedure DoOnEndSampling;virtual;
    property InSampling: boolean read FInSampling;

  end;


  TOscBufferInfo = record
  public
    FFT_Spatial: array[0..MAX_CHANNELS-1] of PAfftw_float; //array [0..8191] of double;
    FFT_Frequency: TComplexFrequencyArray; //array [0..8191] of double;

    allocated: boolean;
    Start: nativeint;
    Length: nativeint;
    PreProcessed: boolean;
    Processed: boolean;
    Transitional: boolean;
    cmd: TCommand;
    OverSample: nativeint;
    fft_plan1: array[0..MAX_CHANNELS-1] of PByte;
    fft_plan2: array[0..MAX_CHANNELS-1] of PByte;
    procedure Init;
    procedure AllocateBuffers(iSize: nativeint);
    procedure FreeBuffers;
    procedure ToFrequencyFromSpatial;
    procedure ToSpatialFromFrequency;
    function Processing: boolean;
    procedure FinishProcessing;
    function IsHalf: boolean;
    procedure CreateFFTPlans;
    procedure DestroyFFTPlans;
    procedure Reuse;
    procedure Cleanup;
    property BufferSize: nativeint read Length write Length;
  end;

  POscBufferInfo = ^TOScbufferInfo;

  TSoundModifierOscillator = class(ToscillatorObject)
  //This class is pretty awesome.  It allows for the implementation
  //of audio effects, like delays, EQs... etc.
  //What happens is you take an oscillator object, attach it to a the master
  //stream... then attach a modifier to the oscillator
  //the oscillator will be first called, and the oscillator will use it's
  //parent to retrieve whatever samples it needs to render the effect.
  //so... the simplest effect might be a pass-through oscillator which does
  //not modify the sound at all.
  //IF YOU WANT SOMETHING MORE COMPLEX with FOURIER support... try the
  //TComplexSoundModifierOscillator which handled the FFT conversions and
  //even the cross fading of overlapped buffer for you automatically.
  private
    FSource: TOscillatorObject;
    FOversamples: nativeint;
    FBufferSize: integer;
    FOwnedbystream: boolean;

    procedure SetNewSecondaryBuffer(idx: nativeint);
    function IndexOfAnyBuffer(b: PoscBufferInfo): nativeint;
    procedure SetBufferSize(const Value: integer);

  protected
    FBufferInfos: array[0..(MODIFIER_BUFFER_COUNT-1)] of TOscBufferInfo;
    FBufferOrders: array[0..(MODIFIER_BUFFER_COUNT-1)] of POscBufferInfo;
    transbuf: POscBufferInfo;
    procedure SetSource(const Value: TOscillatorObject);virtual;

    procedure AllocateBuffers(iSamples: nativeint);
    procedure FreeBuffers;
    procedure Buffer_Process(b: POscBufferInfo);
    procedure Buffer_PreProcess(b: POscBufferInfo);
    procedure Buffer_PreProcess_Sync(b: POscBufferInfo);virtual;
    procedure Buffer_Process_Sync(b: POscBufferInfo);virtual;
    procedure Buffer_FetchForward(b: POscBufferInfo);
    function SetupNewBuffer(iSampleTime: int64; iOverSample: nativeint): boolean;

    procedure SetNewprimaryBuffer(idx: nativeint);
    function IndexOfBuffer(iPosition: int64; iOversample: nativeint): nativeint;
    procedure PreProcess(b: PoscBufferInfo);
    procedure Init;override;
  public
    NoiseTest: boolean;
    property OwnedByStream: boolean read FOwnedbystream write FOwnedbystream;
    property BufferSize: integer read FBufferSize write SetBufferSize;
    procedure Detach;override;
    destructor Destroy;override;

    property Source: TOscillatorObject read FSource write SetSource;
    property Oversamples: nativeint read FOversamples write FOVerSAmples;
    function GetBufferBase(iTargetPosition: int64; iOversample: nativeint): int64;
    procedure NeedBuffer(iPosition: int64; iOverSample: nativeint);
    function GetOversampleWeight(iOFF: nativeint): nativefloat;
    function NyquistFrequency: nativefloat;
    function SampleRate: nativefloat;
  end;


  TSoundCommand = class(TCommand)
  public
    procedure Init;override;
  end;


  Tcmd_BufferProcess = class(TSoundCommand)
  private
    FBuf: POscBufferInfo;
    FModOsc: TSoundModifierOscillator;
  public
    procedure InitExpense;override;
    procedure DoExecute;override;
    property ModOsc: TSoundModifierOscillator read FModOsc write FModOsc;
    property Buf: POscBufferInfo read FBuf write FBuf;
  end;


  TEqInfo = packed record
  public
    Bass: nativefloat;
    Mid: nativefloat;
    Treble: nativefloat;
  end;
  PEQInfo = ^TEqInfo;


  TId3Header = packed record
    threecc: array[0..2] of byte;     //ID3v2/file identifier   "ID3"
    version: array[0..1] of byte;     //ID3v2 version           $03 00
    flags: cardinal;     //ID3v2 flags             %abc00000
    junk:array[0..4] of byte;
    size_7bit: array[0..3] of byte;     //ID3v2 size              4 * %0xxxxxxx
    junk2: array[0..1] of byte;
    function Size: integer;
  end;

  TMp3Info = packed record
    id3header: TId3Header;
    samplerate: cardinal;
  end;

  TBoogerHeader = packed record
    fourcc: array [0 .. 3] of byte;
    ver: smallint;
    channels: smallint;
    length: int64;
    samplerate: cardinal;
    sampleformat: cardinal;
    cue1,cue2,loop1,loop2: int64;
    reserved: array[0..15] of int64;

    // incomplete
    procedure REadFromStream(s: TStream);
    procedure WriteToStream(s: TStream);
    procedure Init;
  end;



  TSoundFileStreamBase = class(TMemoryFileStream)
  end;
  TSoundStreamFakeBase = class(TFileStream)
  private
    __iseekposition: int64;
    function GetiSeekPosition: int64;
    procedure setiSeekPosition(const Value: int64);
  public
    BufferSize: nativeint;
    constructor Create(const AFileName: string; Mode: WORD); reintroduce;overload;virtual;
    constructor Create(const AFileName: string; Mode: WORD; Rights: cardinal);
      reintroduce;overload;virtual;
    property iSeekPosition: int64 read GetiSeekPosition write setiSeekPosition;
    procedure Prefetch(iPosition: int64; iLength: int64 = 0);
  end;

  TAbstractSoundStream = class(TStream)
  protected
    function GetSampleCount: int64;virtual;abstract;
    procedure WriteNextSample(rLeft, rRight: FLoatSample);virtual;abstract;
  end;

  TSoundSampleReadFunction = function: TStereoSoundSample of object;

  TSoundStream = class(TAbstractSoundStream)
  protected
    FSampleRate: integer;
    FChannels: smallint;
    FSamples: int64;
    FByteLength: integer;
    FBytesPerSample: byte;
    FVolume: nativefloat;
    FPan: nativefloat;
    FLoop: boolean;
    FMode: WORD;
    FEq: TEqInfo;
    FUncompressed: TMemoryStream;
    FRestingPoint: single;
    function GetSampleCount: int64;override;
  private
    FUnderStream: TStream;
    FBufferEntireFile: boolean;
    FResampled: boolean;
    FOriginalSampleRate: nativeint;
    FSampleFormat: TXSoundFormat;
    procedure SetBufferEntireFile(const Value: boolean);
    function GetRestingPOint: single;
    procedure SetSampleFormat(const Value: TXSoundFormat);
    procedure SetChannels(const Value: smallint);
  protected
    iSeekPosition: int64;
    iLastSampleStart: int64;
    mp3info: Tmp3info;
    microsoftWAV: TSoundWaveSampler;

    function GetSize: int64; override;


    procedure ApplyEq(buf: multibuffermemoryfilestream.PMBBufferInfo);
    procedure UpdateLastSampleStart;

    function GetSoundSample_8i_2ch: TStereoSoundSample;
    function GetSoundSample_16i_2ch: TStereoSoundSample;
    function GetSoundSample_24i_2ch: TStereoSoundSample;
    function GetSoundSample_32i_2ch: TStereoSoundSample;
    function GetSoundSample_64i_2ch: TStereoSoundSample;
    function GetSoundSample_32f_2ch: TStereoSoundSample;
    function GetSoundSample_64f_2ch: TStereoSoundSample;
    function GetSoundSample_8i_1ch: TStereoSoundSample;
    function GetSoundSample_16i_1ch: TStereoSoundSample;
    function GetSoundSample_24i_1ch: TStereoSoundSample;
    function GetSoundSample_32i_1ch: TStereoSoundSample;
    function GetSoundSample_64i_1ch: TStereoSoundSample;
    function GetSoundSample_32f_1ch: TStereoSoundSample;
    function GetSoundSample_64f_1ch: TStereoSoundSample;
    procedure UpdateSampleFunctionPointer;
  public
    fnReadNextSample: TSoundSampleReadFunction;
    filename: string;
    SampleDataStart: int64;

    constructor Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal);overload;
    constructor Create(const AFileName: string; Mode: cardinal);overload;
    constructor Create(stream_takes_ownership: TStream; ext: string);overload;
    destructor Destroy; override;

    procedure WriteHeader;
    function LoadHeader:TBoogerHeader;
    property channels: smallint read FChannels write SetChannels;
    property samplerate: integer read FSampleRate write FSampleRate;
    procedure Rewind;
    function GetResample(rTime: double; out ss: TStereoSoundSample): boolean;overload;
    function GetResample(targetSampleRate: ni; rTime: double; out ss: TStereoSoundSample): boolean;overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const offset: int64; Origin: TSeekOrigin): int64; override;


    function GetNextSample(out ss: TStereoSoundSample): boolean;virtual;
    procedure WriteNextSample(rLeft, rRight: FLoatSample);override;
    procedure SeekSample(iSample: int64);virtual;
    property SampleCount: int64 read GetSampleCount write FSamples;
    property ByteLengthOfSamples: integer read FByteLength;
    procedure PrefetchSample(iSample: int64; iLength: int64);
    function SamplesToBytes(iSample: int64): int64;
    property BytesperSample: byte read FBytesPerSample write FBytesPerSample;
    procedure GetPeaksFromSamplePosition(out rLeft, rRight: nativefloat);
    property Volume: nativefloat read FVolume write FVolume;
    property Pan: nativefloat read FPan write FPan;
    property Loop: boolean read FLoop write FLoop;
    property Mode: WORD read FMode write FMode;
    function GetSamplePeak(iStart, iEnd: int64;
      iChannelMask: integer = 3): nativefloat;
    function GetSampleMin(iStart, iEnd: int64; iChannelMask: integer = 3): nativefloat;
    function GetSampleAvg(iStart, iEnd: int64; iChannelMask: integer = 3): nativefloat;

    property BufferEntireFile: boolean read FBufferEntireFile write SetBufferEntireFile;

    procedure ResetUDP;
    function SamplePosition: int64;
    function PositionInSeconds: nativefloat;
    function LengthInSeconds: nativefloat;
    property Resampled: boolean read FResampled write FResampled;
    property OriginalSampleRate: nativeint read FOriginalSampleRate write FOriginalSampleRate;

    property REstingPOint: single read GetRestingPOint;
    property SampleFormat: TXSoundFormat read FSampleFormat write SetSampleFormat;

  public
    property Position;
  end;

  TSoundStreamOscillator = class(ToscillatorObject)
  strict private
    FFileName: string;
    FStream: TSoundStream;
    FStartAt: int64;
    FStartedAt: int64;
    FLastSampleREquested: int64;
    FStarted: boolean;
    FfirstSampleRequested: boolean;
    FFreewheelMode: boolean;
    FFreewheelStartSample: int64;
    FFreewheelSamplePOinter: int64;
    FFreeWheelTriggeredSample: integer;
    FFreeWheelReferenceTime: single;
{$IFDEF FREEWHEEL2}
    FFreeWheelTargetAdjustment: nativeint;
    FFreeWheelAdjustment: nativefloat;
{$ENDIF}

    FFreeWheelDebounce: array [0..FW_DEBOUNCE_COUNT] of nativefloat;
    FFreewheelDebounceIdx: integer;
    FBoost: single;
    FVolume: single;
    FBufferSize: integer;
    FSyncOffset: integer;
    FCacheResult: boolean;
    FSyncStatus: integer;
    FFreeWheelPaused: boolean;
    procedure SetFileName(const Value: string);
    procedure SetStartAt(const Value: int64);
    function GetPosition: int64;
    function GetLoop: boolean;
    procedure SetLoop(const Value: boolean);
    procedure SetFreeWheelMode(const Value: boolean);
  strict private
    property Stream: TSoundStream read FStream;
  private
    FConcluded: boolean;
    procedure SetPositionInSeconds(const Value: nativefloat);
    function GetPositionInSeconds: nativefloat;
    procedure SetPosition(const Value: int64);

  public
    SampleRate: nativeint;
    SampleCount: nativeint;
    constructor Create; override;
    destructor Destroy; override;
    property fileName: string read FFileName write SetFileName;
    procedure GiveStream(s: TSoundStream);
    procedure Start;
    procedure Pause;
    procedure UnPause;
    procedure Rewind;
    property StartAt: int64 read FStartAt write SetStartAt;
    property StartedAt: int64 read FStartedAt;
    property Started: boolean read FStarted;
    procedure o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64); override;


    // freewheel mode will drop/dupplicate samples when running as a slave to another source
    procedure FreeWheelStartAt(iSample: int64); overload;
    procedure FreeWheelStartAt(rSeconds: nativefloat); overload;
    procedure FreeWheelTriggerTimeUpdate(rSeconds: nativefloat; bInitial: boolean);
    property FreeWheelMode: boolean read FFreewheelMode write SetFreeWheelMode;
    property FreeWheelPaused: boolean read FFreeWheelPaused write FFreeWheelPaused;
    // --
    property Boost: single read FBoost write FBoost;
    property Volume: single read FVolume write FVolume;
    property Buffersize: integer read FBufferSize write FBufferSize;
    property SyncOffset: integer read FSyncOffset write FSyncOffset;
    property Loop: boolean read GetLoop write SetLoop;
    property FreewheelAdjustment: nativefloat read FFreewheelAdjustment;
    property FreewheelTargetAdjustment: nativeint read FFreewheelTargetAdjustment;


    property Position: int64 read GetPosition write SetPosition;
    property PositionInSeconds: nativefloat read GEtPositionInSeconds write SetPositionInSeconds;
    function SamplePosition: int64;
    function LengthInSeconds: nativefloat;
    property FirstSampleRequested: boolean read FFirstSampleREquested;
    property Concluded: boolean read FConcluded;

  end;

const
  SOUND_HEADER_LENGTH_v3 = sizeof(TBoogerHeader);

function GetMp3Info(sFile: string): TMp3Info;overload;
function GetMp3Info(p: Pbyte): TMp3Info;overload;


function ss_GetSoundSample_8i_2ch(strm: TStream): TStereoSoundSample;
function ss_GetSoundSample_16i_2ch(strm: TStream): TStereoSoundSample;
function ss_GetSoundSample_24i_2ch(strm: TStream): TStereoSoundSample;
function ss_GetSoundSample_32i_2ch(strm: TStream): TStereoSoundSample;
function ss_GetSoundSample_64i_2ch(strm: TStream): TStereoSoundSample;
function ss_GetSoundSample_32f_2ch(strm: TStream): TStereoSoundSample;
function ss_GetSoundSample_64f_2ch(strm: TStream): TStereoSoundSample;



implementation

uses
  soundconversion;

function ss_GetSoundSample_8i_1ch(strm: TStream): TStereoSoundSample;
var
  mono: tinyint;
begin
  Stream_GuaranteeRead(strm, @mono, sizeof(mono));
  result.Left := mono/int64($7f);
  result.Right := result.Left;
end;
function ss_GetSoundSample_16i_1ch(strm: TStream): TStereoSoundSample;
var
  mono: smallint;
begin
  Stream_GuaranteeRead(strm, @mono, sizeof(mono));
  result.Left := mono/int64($7fff);
  result.Right := result.Left;
end;
function ss_GetSoundSample_24i_1ch(strm: TStream): TStereoSoundSample;
var
  mono: uint24;
begin
  Stream_GuaranteeRead(strm, @mono, sizeof(mono));
  result.Left := mono.toint64/int64($7fffff);
  result.Right := result.Left;
end;
function ss_GetSoundSample_32i_1ch(strm: TStream): TStereoSoundSample;
var
  mono: integer;
begin
  Stream_GuaranteeRead(strm, @mono, sizeof(mono));
  result.Left := mono/int64($7fffffff);
  result.Right := result.Left;
end;
function ss_GetSoundSample_64i_1ch(strm: TStream): TStereoSoundSample;
var
  mono: int64;
begin
  Stream_GuaranteeRead(strm, @mono, sizeof(mono));
  result.Left := mono/int64($7fffffffffffffff);
  result.Right := result.Left;
end;
function ss_GetSoundSample_32f_1ch(strm: TStream): TStereoSoundSample;
var
  mono: single;
begin
  Stream_GuaranteeRead(strm, @mono, sizeof(mono));
  result.Left := mono;
  result.Right := result.Left;
end;
function ss_GetSoundSample_64f_1ch(strm: TStream): TStereoSoundSample;
var
  mono: double;
begin
  Stream_GuaranteeRead(strm, @mono, sizeof(mono));
  result.Left := mono;
  result.Right := result.Left;
end;


function ss_GetSoundSample_8i_2ch(strm: TStream): TStereoSoundSample;
var
  lr: array[0..1] of tinyint;
begin
  Stream_GuaranteeRead(strm, @lr[0], sizeof(lr));
  result.Left := lr[0]/127;
  result.Right := lr[1]/127;
end;
function ss_GetSoundSample_16i_2ch(strm: TStream): TStereoSoundSample;
var
  lr: array[0..1] of smallint;
begin
  Stream_GuaranteeRead(strm, @lr[0], sizeof(lr));
  result.Left := lr[0]/32767;
  result.Right := lr[1]/32767;
end;
function ss_GetSoundSample_24i_2ch(strm: TStream): TStereoSoundSample;
var
  lr: array[0..1] of uint24;
begin
  Stream_GuaranteeRead(strm, @lr[0], sizeof(lr));
  result.Left := lr[0].toint64/int64($7fffff);
  result.Left := lr[1].toint64/int64($7fffff);
end;
function ss_GetSoundSample_32i_2ch(strm: TStream): TStereoSoundSample;
var
  lr: array[0..1] of integer;
begin
  Stream_GuaranteeRead(strm, @lr[0], sizeof(lr));
  result.Left := lr[0]/int64($7fffffff);
  result.Right := lr[1]/int64($7fffffff);
end;
function ss_GetSoundSample_64i_2ch(strm: TStream): TStereoSoundSample;
var
  lr: array[0..1] of int64;
begin
  Stream_GuaranteeRead(strm, @lr[0], sizeof(lr));
  result.Left := lr[0]/int64($7fffffffffffffff);
  result.Right := lr[1]/int64($7ffffffffffffff);
end;

function ss_GetSoundSample_32f_2ch(strm: TStream): TStereoSoundSample;
var
  lr: array[0..1] of single;
begin
  Stream_GuaranteeRead(strm, @lr[0], sizeof(lr));
  result.Left := lr[0];
  result.Right := lr[1];
end;
function ss_GetSoundSample_64f_2ch(strm: TStream): TStereoSoundSample;
var
  lr: array[0..1] of double;
begin
  Stream_GuaranteeRead(strm, @lr[0], sizeof(lr));
  result.Left := lr[0];
  result.Right := lr[1];
end;






constructor TSoundOscillator.Create;
begin
  inherited;

end;

destructor TSoundOscillator.Destroy;
begin
  if assigned(Fthr) then begin
    Fthr.RemoveOscillator(hooksse,self);
  end;
  inherited;
end;


procedure TSoundOscillator.Fill(iBufferPos: integer; mt: ToscMessageType;
  out res: TStereoSoundSample; iSampleTime: int64);
var
  rLEft, rRight: FloatSample;
begin
  if assigned(FhookSSE) then begin
    FhookSSE(mt, res,iSampletime, false);
    FBufL[FVUPos] := res.Left;
    FBufR[FVUPos] := res.Right;
    inc(FVUPos);
    FVUPos := FVUPos mod VU_BUFFER_SIZE;
  end;
end;


procedure TSoundOscillator.GetVU(out rL, rR: nativefloat);
var
  t: integer;
  r: nativefloat;
  pp: integer;
begin
  rL := 0;
  rR := 0;

  pp := thr.PlayPosition;
  for t := 0 to VU_BUFFER_SIZE-1 do begin
    r := abs(FBufL[t]);
    if r > rL then
      rL := r;
    r := abs(FBufR[t]);
    if r > rR then
      rR := r;
  end;
end;

procedure ToscillatorObject.SetResultCacheSize(const Value: nativeint);
var
  p: pointer;
  t: integer;
begin
  if (value = FREsultCacheSize) and (FCachedResults <> nil) then
    exit;

  FreeResultCache;
  FREsultCacheSize := Value;


  GEtMem(p, value*sizeof(TSoundCacheRecord));
  FCachedREsults := p;
  for t:= 0 to value-1 do begin
    FCachedResults^[t].Init;
  end;


end;

procedure TOscillatorObject.FreeResultCache;
begin
  if FCachedResults = nil then
    exit;

  freemem(FCachedREsults);
  FCachedResults := nil;
end;

procedure TOscillatorObject.GetVUHistory(iPos: nativeint; out rLeft, rRight: nativefloat);
begin
  rLeft := fvubuffer[iPOs mod VU_HISTORY_SIZE].Left;
  rRight := fvubuffer[iPos mod VU_HISTORY_SIZE].Right;

end;

procedure TOscillatorObject.GetVUPeaks(out rLeft, rRight: nativefloat);
var
  t: integer;
begin
  rLeft := 0;
  rRight := 0;
  for t := 0 to VU_HISTORY_SIZE - 1 do begin
    if abs(FVUBuffer[t].Left) > rLeft then
      rLeft := abs(FVUBuffer[t].Left);

    if abs(FVUBuffer[t].Right) > rRight then
      rRight := abs(FVUBuffer[t].right);
  end;

end;

procedure ToscillatorObject.AddModifier(o: TSoundModifierOscillator; bTakeOwnership: boolean);
var
  i: nativeint;
begin
  Lock;
  try
    i := FModifiers.Count;
    FModifiers.add(o);

    o.ownedbystream := bTakeOwnership;

    RemapSources;
  finally
    Unlock;
  end;
end;

procedure ToscillatorObject.BeforeDestruction;
begin
  if assigned(FMasterStream) then begin
    MasterStream := nil;
  end;

  FreeResultCache;
end;

destructor ToscillatorObject.Destroy;
begin
  inherited;
end;


procedure ToscillatorObject.Detach;
var
  o: tObject;
begin
  if not Detached then begin
    masterStream := nil;

    if InSampling then
      DoOnEndSampling;

    if assigned(FOwnedObjects) then
    while (FOwnedObjects.count > 0) do begin
      o := FOwnedObjects[FOwnedObjects.count-1];
      o.Free;
      FOwnedObjects.Remove(o);
      o := nil;
    end;

    if Assigned(FModifiers) then
      FModifiers.Clear;

    FModifiers.Free;
    FModifiers := nil;


    FOwnedObjects.Free;
    FOwnedObjects := nil;
  end;

  inherited;

end;

procedure ToscillatorObject.DoOnBeginSAmpling;
begin
  if assigned(FOnBeginSampling) then FOnBeginSampling(self);
  FInSampling := true;
end;

procedure ToscillatorObject.DoOnEndSampling;
begin
  FInSampling := false;
  if assigned(FOnEndSampling) then FOnEndSampling(self);

end;

function ToscillatorObject.GetHasModifiers: boolean;
begin
  result := ModifierCount > 0;
end;

function ToscillatorObject.GetModifierCount: integer;
begin
  Lock;
  try
    result := FModifiers.Count;
  finally
    Unlock;
  end;
end;

procedure ToscillatorObject.Init;
begin
  inherited;
  FModifiers := TList<TSoundModifierOscillator>.create;
  FOwnedObjects := TList.create;
  FresultCacheSize := 44100;
  FCacheResults := false;
end;

function ToscillatorObject.MasterStreamSampleRate: ni;
begin
  if assigned(MasterStream) then
    result := MasterStream.SampleRate
  else begin
    result := 44100;
    raise ECritical.create('masterstream not assigned, cannot get masterstreamsamplerate');
  end;
end;

procedure ToscillatorObject.oo(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64; bIgnoreMods: boolean);
var
  sr: PSoundCacheRecord;
begin
  ss.init;
  case mt of
    mtBeginWindow: DoOnBeginSAmpling;
    mtEndWindow:  DoOnEndSampling;
  end;
  if (bIgnoreMods) and (mt = mtGetSample) and CacheREsults then begin
    sr := @FCachedResults^[iSampletime mod FREsultCacheSize];
//    OutPutDebugString(pchar(inttostr(sr.Time)));
    if sr.Time = iSampleTime then begin
      ss := sr.Sample;
      exit;
    end;
  end;

  if hasmodifiers and (not bIgnoreMods) then
    FModifiers[FModifiers.count-1].oo(mt, ss,iSampleTime,false)
  else
    o(mt,ss, iSampleTime);

  FVUBuffer[iSampletime mod VU_HISTORY_SIZE] := ss;

  if bIgnoreMods and CacheREsults then begin
    sr := @FCachedResults^[iSampletime mod FREsultCacheSize];
    sr.Time := iSampleTime;
    sr.Sample := ss;

  end;

end;

procedure ToscillatorObject.oo_resample(mt: ToscMessageType; out ss: TStereoSoundSample; iSampleTime: double);
var
  rWeight: nativefloat;
  iSamp1, iSamp2: int64;
  ss1, rRight1: TStereoSoundSample;
begin

  case mt of
    mtBeginWindow, mtEndWindow: oo(mt, ss, floor(iSampleTime), false);
  else
    begin
      iSamp1 := floor(iSampleTime);
      iSAmp2 := iSamp1 + 1;
      rWeight := iSampleTime - iSamp1;
      if rWeight = 0 then begin
        oo(mt, ss, iSamp1, false);
      end else begin
        oo(mt, ss1, iSamp1,false);
        oo(mt, ss, iSamp2, false);
        ss := (ss1 * (1-rWeight))+(ss * (rWeight)) ;
      end;



    end;
  end;

end;

procedure ToscillatorObject.REmapSources;
var
  t: nativeint;
  o: TSoundModifierOscillator;
begin
  for t:= 0 to FModifiers.Count-1 do begin
    o := FModifiers[t];
    //hook up the source... if it is the first modifier then it is self.
    if t = 0 then
      o.Source := self
    else
      o.Source := FModifiers[t-1];

  end;

end;

procedure ToscillatorObject.RemoveModifier(o: TSoundModifierOscillator);
begin
  if o = nil then
    exit;

  Lock;
  try
    FModifiers.remove(o);
    RemapSources;
    if o.OwnedByStream then
      o.Free;

  finally
    Unlock;
  end;
end;

procedure ToscillatorObject.SetCacheResults(const Value: boolean);
begin
  FCacheResults := Value;
  if value then begin
    ResultCacheSize := FREsultCacheSize;
  end;
end;

procedure ToscillatorObject.SetMasterStream(const Value: ISoundOscillatorRenderer);
begin
  if assigned(FMasterStream) then begin
    FMasterStream.RemoveOscillator(self);
  end;

  FMasterStream := Value;

  if assigned(FMasterStream) then begin
    FMasterStream.AddOscillator(self);
  end;

  SourcesChanged;

end;

procedure ToscillatorObject.SourcesChanged;
begin
  //no implementation neccessary
end;

{ TSoundModifierOscillator }

procedure TSoundModifierOscillator.AllocateBuffers(iSamples: nativeint);
var
  t: integer;
begin

  for t:= 0 to MODIFIER_BUFFER_COUNT-1 do begin
    Self.FBufferInfos[t].AllocateBuffers(iSamples);
    FBufferOrders[t] := @FBufferInfos[t];
  end;



end;

procedure TSoundModifierOscillator.Buffer_FetchForward(b: POscBufferInfo);
var
  t: nativeint;
  ss: TStereoSoundSample;
begin
  source.o(mtBeginWindow, ss, b.Start);
  Lock;
  try

    for t:= 0 to b.Length-1 do begin
//      Lock;
//      try
//          OutputDebugString(pchar('Calling source from '+self.ClassName+' '+inttostr(b.Start+t)));
          source.oo(mtGetSample, ss, b.Start + t, true);
//      finally
//        Unlock;
//      end;



      b.FFT_Spatial[0][t] := ss.Left;
      b.FFT_Spatial[1][t] := ss.Right;
    end;
  finally
    Unlock;
  end;
  source.o(mtEndWindow, ss, b.Start + b.Length);
end;

procedure TSoundModifierOscillator.Buffer_PreProcess(b: POscBufferInfo);
begin
//  OutputDebugString(pchar('PreProcess '+self.ClassName+' '+inttostr(b.Start)+' '+inttostr(b.Length)));
  Buffer_PreProcess_Sync(b);
  b.PreProcessed := true;

end;

procedure TSoundModifierOscillator.Buffer_PreProcess_Sync(b: POscBufferInfo);
begin
  //
end;

procedure TSoundModifierOscillator.Buffer_Process(b: POscBufferInfo);
begin
//  OutputDebugString(pchar('Process '+self.ClassName+' '+inttostr(b.Start)+' '+inttostr(b.Length)));
  Buffer_Process_Sync(b);
  b.Processed := true;
end;

procedure TSoundModifierOscillator.Buffer_Process_Sync(b: POscBufferInfo);
begin
  //
end;

destructor TSoundModifierOscillator.Destroy;
begin
  Detach;
  inherited;
end;

procedure TSoundModifierOscillator.Detach;
begin
  FreeBuffers;
  inherited;
end;

procedure TSoundModifierOscillator.FreeBuffers;
var
  t: integer;
begin

  for t:= 0 to MODIFIER_BUFFER_COUNT-1 do begin
    Self.FBufferInfos[t].FreeBuffers;
  end;
end;

function TSoundModifierOscillator.GetBufferBase(iTargetPosition: int64;
  iOversample: nativeint): int64;
var
  iOverSampleStep: int64;
begin
  iOverSampleStep := BufferSize div OverSamples;

  result := (iTargetPosition div iOverSampleStep) * iOverSampleStep;

  result := result - (iOverSampleStep* iOVersample);
end;

function TSoundModifierOscillator.GetOversampleWeight(iOFF: nativeint): nativefloat;
var
  iTemp: nativeint;
begin
{$IFDEF SAFE_OVERSAMPLE_WEIGHT}
  result := 1.0;
  iTemp := (BufferSize div (2));
  if iOFF < iTemp then begin
    result := iOFF / iTemp;
    exit;
  end;
  if iOFF > (BufferSize - iTemp) then begin
    result := (BufferSize - (iOFF+1)) / iTemp;
    if result < 0 then
      result := 0;
  end;
{$ELSE}
  result := geometry.HerbivoreCurve(iOFF/BufferSize, 0.25);
{$ENDIF}



end;

function TSoundModifierOscillator.IndexOfBuffer(iPosition: int64; iOversample: nativeint): nativeint;
var
  t: nativeint;
  iBase: int64;
begin
  result := -1;
  iBase := GetBufferBase(iPosition, iOversample);
  for t:= low(FBufferOrders) to high(FBuffeRORders) do begin
    if (FBufferOrders[t].Start = iBase) then begin
      result := t;
      exit;
    end;
  end;

end;

function TSoundModifierOscillator.IndexOfAnyBuffer(b: PoscBufferInfo): nativeint;
var
  t: nativeint;
begin
  result := -1;
  for t:= low(FBufferOrders) to high(FBuffeRORders) do begin
    if FBufferOrders[t] = b then begin
      result := t;
      exit;
    end;
  end;

end;



procedure TSoundModifierOscillator.Init;
begin
  inherited;

  OverSamples := DEFAULT_OVERSAMPLE_COUNT;
  BufferSize := DEFAULT_MODIFIER_BUFFER_SIZE;


end;


procedure TSoundModifierOscillator.NeedBuffer(iPosition: int64;
  iOverSample: nativeint);
var
  i: nativeint;
begin
  i := IndexOfBuffer(iPosition, iOverSample);
  if i < 0 then begin
//    SoundDebug('Didn''t find buffer for ('+inttostr(iPosition)+','+inttostr(iOverSample)+')');
    if SetupNewBuffer(iPosition, iOverSample) then begin
//      SoundDebug('New Buffer ('+inttostr(iPosition)+','+inttostr(iOverSample)+')');
      SetNewprimaryBuffer(high(FBufferOrders));

      if not FBufferOrders[0].PreProcessed then
        Buffer_PreProcess(FBufferOrders[0]);
{$IFDEF THREAD_SOUND_MODS}
      PreProcess(FBufferOrders[0]);
{$ELSE}
      if not FBufferOrders[0].Processed then
        Buffer_Process_Sync(FBufferOrders[0]);
{$ENDIF}
    end;

  end;
end;

function TSoundModifierOscillator.NyquistFrequency: nativefloat;
begin
  result := 22050;
  if assigned(Source) and (source is TSoundStreamOscillator) then begin
    result := TSoundStreamOscillator(source).samplerate / 2;
  end else begin
    if assigned(MasterStream) then
      result := MasterStream.NyquistFrequency;
  end;
end;

procedure TSoundModifierOscillator.PreProcess(b: PoscBufferInfo);
var
  c: Tcmd_BufferProcess;
begin
  c := Tcmd_bufferprocess.Create;
  c.ModOsc := self;
  c.Buf := b;
  c.Start;
  b.cmd := c;

end;

function TSoundModifierOscillator.SampleRate: nativefloat;
begin
  if not assigned(MasterStream) then
    result := 44100
  else
    result := Masterstream.SampleRate;
end;

procedure TSoundModifierOscillator.SetBufferSize(const Value: integer);
begin
  FBufferSize := Value;
  AllocateBuffers(FBufferSize);
end;

procedure TSoundModifierOscillator.SetNewprimaryBuffer(idx: nativeint);
var
  pb: POscBufferInfo;
  t: integer;
begin
  if idx = 0 then
    exit;
  pb := FBufferOrders[idx];

  for t := idx downto 1 do begin
    FBufferOrders[t] := FBufferOrders[t-1];
  end;
  FBufferOrders[0] := pb;
end;

procedure TSoundModifierOscillator.SetNewSecondaryBuffer(idx: nativeint);
var
  pb: POscBufferInfo;
  t: integer;
begin
  if idx < 2 then
    exit;
  pb := FBufferOrders[idx];

  for t := idx downto 2 do begin
    FBufferOrders[t] := FBufferOrders[t-1];
  end;
  FBufferOrders[1] := pb;
end;


procedure TSoundModifierOscillator.SetSource(const Value: TOscillatorObject);
begin
  FSource := Value;
  SourcesChanged;
end;


function TSoundModifierOscillator.SetupNewBuffer(iSampleTime: int64; iOverSample: nativeint): boolean;
var
  pb: POScBuffeRInfo;
  i: int64;
begin
  result := false;

  if IndexOfBuffer(iSampleTime, iOverSample) >=0 then
    exit;


  i := GetBufferBase(iSampleTime, iOverSample);
  if i < 0 then exit;

  result := true;

  pb := FBufferOrders[high(FBufferOrders)];
  pb.Reuse;
  pb.Start := i;
  pb.Length := BufferSize;
  pb.Oversample := iOverSample;

//  if pb.Transitional then
//    raise Exception.Create(self.classname+': Out of buffer space.  Trying to replace a transitional buffer used for oversampling.  Increase the number of buffers.');


end;

procedure TSoundCacheRecord.Init;
begin
  Sample.Left := 0;
  Sample.Right := 0;
  Time := -1;
end;

procedure TOscBufferInfo.AllocateBuffers(iSize: nativeint);
var
  p: pointer;
  u: integer;
begin
  BufferSize := iSize;
//  for t:= low(FFT_Spatial) to high(FFT_Spatial) do begin
    for u := 0 to MAX_CHANNELS-1 do begin
      p := self.FFT_Spatial[u];
      if p <> nil then
        FreeMemory(p);
      p := self.FFT_FRequency[u];
      if p <> nil then
        FreeMemory(p);

      p := GetMemory(iSize*SizeOf(fftw_float));
      self.FFT_Spatial[u] := p;
      p := GetMemory(iSize*SizeOf(fftw_complex));
      self.FFT_Frequency[u] := p;
      Start := -1;
      Length := iSize;
    end;
//  end;
{$IFNDEF DISABLE_FFTW}
  CreateFFTPlans;
{$ENDIF}
  allocated := true;

end;

procedure TOscBufferInfo.Cleanup;
begin
  FreeBuffers;
end;

procedure TOscBufferInfo.CreateFFTPlans;
var
  t: integer;
  r: real;
  m: integer;
  iWindowLength: integer;
  p1,p2: pointer;
begin
  inherited;

{$IFNDEF MSWINDOWS}
  raise ENotImplemented.create('not implemented on this platform');
{$ELSE}
  iWindowLength := length;//1024

  LockFFT;
  try
    for t:= 0 to MAX_CHANNELS-1 do begin
      p1 := @(FFT_Spatial[t][0]);
      p2 :=  @(FFT_Frequency[t][0]);
      fft_plan1[t] := fftw_plan_dft_r2c_1d(iWindowLength, p1, p2, FFTW_ESTIMATE );
      zeromemory(@(FFT_Frequency[t][0]), iWindowLength * (sizeof(complexSingle)));
      fft_plan2[t] := fftw_plan_dft_c2r_1d(iWindowLength, p2, p1, FFTW_ESTIMATE+FFTW_PRESERVE_INPUT );
    end;
  finally
    UnlockFFT;
  end;
{$ENDIF}

end;

procedure TOscBufferInfo.DestroyFFTPlans;
var
  t: integer;
  r: real;
  m: integer;
  iWindowLength: integer;
  p1,p2: pointer;
begin
  inherited;

  iWindowLength := length;//1024

{$IFNDEF MSWINDOWS}
  raise ENotImplemented.create('not implemented on this platform');
{$ELSE}
  LockFFT;
  try
    for t:= 0 to MAX_CHANNELS-1 do begin
      fftw_destroy_plan(fft_plan1[t]);
      fftw_destroy_plan(fft_plan2[t]);
    end;
  finally
    UnlockFFT;
  end;
{$ENDIF}

end;

procedure TOscBufferInfo.FinishProcessing;
begin
  if cmd = nil then
    exit;

  try
    cmd.WaitFor;
  finally
    cmd.Free;
    cmd := nil;
  end;
end;

procedure TOscBufferInfo.FreeBuffers;
var
  p: pointer;
  u: integer;
begin
  if not allocated then exit;

//  for t:= low(FFT_Spatial) to high(FFT_Spatial) do begin
    for u := 0 to MAX_CHANNELS-1 do begin
      p := FFT_Spatial[u];
      if p <> nil then
        Freememory(p);

      p := FFT_Frequency[u];
      if p <> nil then
        Freememory(p);

      FFT_Spatial[u] := nil;
      FFT_Frequency[u] := nil;

    end;
//  end;

  DestroyFFTPlans;
  allocated := false;

end;

procedure TOscBufferInfo.Init;
var
  t, u: nativeint;
begin
  allocated := false;
  for t:= low(FFT_Spatial) to high(FFT_Spatial) do begin
    for u := 0 to MAX_CHANNELS-1 do begin
      self.FFT_Spatial[u] := nil;
      self.FFT_Frequency[u] := nil;
    end;
  end;


  cmd := nil;
end;

function TOscBufferInfo.IsHalf: boolean;
begin
  result := (start mod BufferSize) <> 0;
end;

function TOscBufferInfo.Processing: boolean;
begin
  result := cmd <> nil;
end;

procedure TOscBufferInfo.Reuse;
begin
  Processed := false;
  PreProcessed := false;
  Start := -1;
end;

procedure TOscBufferInfo.ToFrequencyFromSpatial;
var
  t,u: integer;
begin
{$IFNDEF MSWINDOWS}
  raise ENotImplemented.create('not implemented on this platform');
{$ELSE}
  for t:= 0 to MAX_CHANNELS-1 do begin
    zeromemory(@FFT_Frequency[t][0], Length*sizeof(fftw_complex));
    fftw_execute(fft_plan1[t]);
    for u := 0 to (Length)-1 do begin
      FFT_Frequency[t][u].i := FFT_Frequency[t][u].i/Length;//TODO: this can be optimized for SSE
      FFT_Frequency[t][u].r := FFT_Frequency[t][u].r/Length;
    end;

  end;
  {$ENDIF}


end;

procedure TOscBufferInfo.ToSpatialFromFrequency;
var
  t,u: integer;
  d: fftw_float;
begin
{$IFNDEF MSWINDOWS}
  raise ENotImplemented.create('not implemented on this platform');
{$ELSE}
  for t:= 0 to MAX_CHANNELS-1 do begin
    fftw_execute(fft_plan2[t]);
    for u := 0 to (Length)-1 do begin
      d := FFT_Spatial[t][u];
      if d > 1.0 then d := 1.0;
      if d < -1.0 then d := -1.0;
      FFT_Spatial[t][u] := d;

    end;

  end;
  {$endif}
end;




{ Tcmd_BufferProcess }

procedure Tcmd_BufferProcess.DoExecute;
begin
  inherited;
  ModOsc.Buffer_Process_Sync(Buf);
end;

procedure Tcmd_BufferProcess.InitExpense;
begin
  inherited;
//  MemoryExpense := 1.0;

end;

procedure TSoundCommand.Init;
begin
  inherited;
  Icon := @CMD_ICON_SOUND;
end;


{ TSoundStreamOscillator }

constructor TSoundStreamOscillator.Create;
begin
  inherited;
  FStream := nil;
  FStartedAt := 0;
  Boost := 1;
  Volume := 1;
end;

destructor TSoundStreamOscillator.Destroy;
begin
  FStream.free;

  inherited;
end;

procedure TSoundStreamOscillator.FreeWheelStartAt(iSample: int64);
begin
  Lock;
  try
    FFreewheelSamplePOinter := iSample;
  finally
    Unlock;
  end;
end;

procedure TSoundStreamOscillator.FreeWheelStartAt(rSeconds: nativefloat);
begin
  FreeWheelStartAt(round(rSeconds * 44100) + (MasterStream.Buffersize div 2));

end;

procedure TSoundStreamOscillator.FreeWheelTriggerTimeUpdate(rSeconds: nativefloat; bInitial: boolean);
var
  i: nativeint;
  t: nativeint;
  r: nativefloat;
begin
  Lock;
  try

{$IFDEF FREEWHEEL2}
    //get the actual sample time from the main oscillator
    //use the freewheel start time to determine what point in the chase stre
    if bInitial then begin
      FStartedAt := self.MasterStream.PlayPosition;
    end;
    i := self.MasterStream.PlayPosition-FStartedAt ;

    FFreeWheelTriggeredSample := round(rSeconds * 44100);
    // + (masterstream.BufferSize div 2);
    FFreeWheelReferenceTime := rSeconds;


    if bInitial then begin
      for t:= 0 to FW_DEBOUNCE_COUNT-1 do begin
        FFreeWheelDebounce[t] := FFreeWheelTriggeredSample-i;
      end;
    end;
    Ffreewheeldebounceidx :=Ffreewheeldebounceidx mod FW_DEBOUNCE_COUNT;
    FFreeWheelDebounce[Ffreewheeldebounceidx] := (FFreeWheelTriggeredSample - i);
    inc(FFreewheelDebounceIdx);

    r := 0;
    for t:= 0 to FW_DEBOUNCE_COUNT-1 do begin
      r := r + FFreeWheelDebounce[t];
    end;

    FFreeWheelTargetAdjustment := round(r / FW_DEBOUNCE_COUNT);


    if bInitial then FFreeWheelAdjustment := FFreeWheelTargetAdjustment;
{$ELSE}
    FFreeWheelTriggeredSample := round(rSeconds * 44100);
    // + (masterstream.BufferSize div 2);
    FFreeWheelReferenceTime := rSeconds;
{$ENDIF}
  finally
    Unlock;
  end;
end;

function TSoundStreamOscillator.GetLoop: boolean;
begin
  result := FStream.Loop;
end;

function TSoundStreamOscillator.GetPosition: int64;
begin
  if Started then begin
    if assigned(masterstream) then begin
      result := round((MAsterStream.PlayPosition - FStartedAt) * (samplerate / MasterStream.SampleRate)) +  FStartAt;
    end else begin
      result := stream.sampleposition mod SampleCount
    end;
    //result := FLastSampleREquested
  end
  else
    result := FStartAt;
end;

function TSoundStreamOscillator.LengthInSeconds: nativefloat;
begin
  result := SampleCount / SampleRate;
end;


procedure TSoundStreamOscillator.o(mt: ToscMessageType;
  out ss: TStereoSoundSAmple; iSampletime: int64);
var
  iThisSample: int64;
  iDif: floatsample;
  fSample: double;
begin
  if mt = mtBeginWindow then
    exit;

  if mt = mtEndWindow then
    exit;


  if not assigned(FStream) then
    exit;

  if (not FStarted) then begin
    ss.Init;
    exit;
  end;

  if not FfirstSampleRequested then begin
    FfirstSampleRequested := true;
    if assigned(MasterStream) then
      FStartedAt := MasterStream.playposition
    else
      FStartedAt := iSampletime;
  end;
//  if iSampleTime < FStartedAT then
//    FStartedAT := iSampleTime;

  iThisSample := iSampletime - FStartedAt;

  if MAsterStream <> nil then begin
    if MasterStream.samplerate <> self.SampleRate then
      iThisSample := round(iThisSample * (samplerate / MasterStream.samplerate));
  end;

  if not FreeWheelMode then begin
    FLastSampleREquested := iThisSample + FStartAt;

    if mt = mtEndWindow then begin
// FStream.SeekSample(iThisSample+FStartAt+(self.FStream.BufferSize div 4));
// FStream.SeekSample(FFreeWheelSamplePointer);
      exit;
    end;


{$IFDEF NEW_RESAMPLE}
//    if dev = nil then
//      raise ECritical.create('dev not assigned');
//    Debug.log(self, 'oscillate');
    FStream.GetResample(MasterStreamSampleRate,iThisSample+FStartAt,ss);
//    FStream.SeekSample(iThisSample + FStartAt);
//    FStream.GetNextSample(ss);
{$ELSE}
    FStream.SeekSample(iThisSample + FStartAt);
    FStream.GetNextSample(ss);
{$ENDIF}
    if not Loop then begin
      if (iThisSample + FStartAt) >= (SampleCount-1) then begin
        Fconcluded := true;
      end;
    end;
    if concluded then begin
      ss.Init;
    end;



    ss := ss * (Boost*Volume);

  end
  else begin
{$IFDEF FREEWHEEL2}
    FLastSampleREquested := iThisSample + FStartAt;

    if mt = mtEndWindow then begin
// FStream.SeekSample(iThisSample+FStartAt+(self.FStream.BufferSize div 4));
// FStream.SeekSample(FFreeWheelSamplePointer);
      exit;
    end;


    if FreewheelPaused then begin
      ss.Init;
      exit;
    end;



    fSample := (iThisSample + FStartAt +SyncOffset)+FFreeWheelAdjustment;
    FStream.GetResample(fsample, ss);
    if not Loop then begin
      if fSample >= (SampleCount-1) then begin
        Fconcluded := true;
      end;
    end;
    if concluded then begin
      ss.init;
    end;

//    FStream.SeekSample(round((iThisSample + FStartAt)+FFreeWheelAdjustment));
 //   FStream.GetNextSample(rLeft, rRight);
//    rLeft := 0;
//    rRight := 0;
    ss := ss * (Boost*Volume);

    iDif := FFreeWheelTargetAdjustment - FFreeWheelAdjustment;
    //OutputDebugString(pchar('Dif= '+inttostr(iDif)));
    if  abs(iDif) < 512 then begin
//      if (FSyncStatus <> 0) then OutputDebugString(pchar('Tight'));
      FSyncStatus := 0;
    end;


    if (abs(iDif) < FW_JUMP_THRESHOLD) and (abs(iDif) >= 0) then begin
      if iDif > 0  then begin
//        if (FSyncStatus <> 1) then OutputDebugString(pchar('Leading'));
        FSyncStatus := 1;
        FFreeWheelAdjustment := FFreeWheelAdjustment + (0.008 * abs((iDif)/1000));
      end else begin
//        if (FSyncStatus <> -1) then OutputDebugString(pchar('Trailing'));
        FSyncStatus := -1;
        FFreeWheelAdjustment := FFreeWheelAdjustment - (0.008 * abs((iDif)/1000));
      end;
    end
    else begin
      if iDif > 0  then begin
        FFreeWheelAdjustment := FFreeWheelAdjustment + FW_JUMP_THRESHOLD;
//        inc(FFreeWheelAdjustment,256);
      end else begin
        FFreeWheelAdjustment := FFreeWheelAdjustment - FW_JUMP_THRESHOLD;
//        dec(FFreeWheelAdjustment,256);
      end;
    end

{$ELSE}

    Lock;
    try
      FLastSampleREquested := FFreewheelSamplePOinter;

      if mt = mtEndWindow then begin
        // FStream.SeekSample(FFreeWheelSamplePointer+(self.FStream.BufferSize div 4));
        // FStream.SeekSample(FFreeWheelSamplePointer);
        exit;
      end;

      FStream.SeekSample(FFreewheelSamplePOinter + SyncOffset);
      FStream.GetNextSample(rLeft, rRight);

      rLeft := rLeft * Boost * Volume;
      rRight := rRight * Boost * Volume;
      iDif := FFreewheelSamplePOinter - FFreeWheelTriggeredSample;
      if abs(iDif) > 22050 * 4 then begin
        FFreewheelSamplePOinter := FFreeWheelTriggeredSample;
        iDif := FFreewheelSamplePOinter - FFreeWheelTriggeredSample;
      end;
      if iDif < -256 then begin
        inc(FFreewheelSamplePOinter);
        if random(100) > (100 - abs(iDif / 1000)) then begin
          inc(FFreewheelSamplePOinter);
        end;
      end
      else if iDif > 256 then begin
        if random(100) > (100 - abs(iDif / 1000)) then begin
          inc(FFreewheelSamplePOinter);
        end;
      end
      else
        inc(FFreewheelSamplePOinter);

      inc(FFreeWheelTriggeredSample);
    finally
      Unlock;
    end;
{$ENDIF}

  end;


end;

procedure TSoundStreamOscillator.Pause;
begin
  FStarted := false;
  FStartAt := FLastSampleREquested;

end;


function TSoundStreamOscillator.GetPositionInSeconds: nativefloat;
begin
  if self.samplerate = 0 then
    result := 0
  else begin
    if concluded then
      result := (samplecount-1) / Self.SampleRate
    else
      result := Position / Self.SampleRate;
  end;
end;

procedure TSoundStreamOscillator.GiveStream(s: TSoundStream);
begin
  FStream := s;

  SampleRate := FStream.samplerate;
  SampleCount := FStream.SampleCount;

end;

procedure TSoundStreamOscillator.Rewind;
begin
  FStartAt := 0;
end;

function TSoundStreamOscillator.SamplePosition: int64;
begin
  if STarted then begin
    if assigned(masterstream) then begin
      result := round((MAsterStream.PlayPosition - FStartedAt) * (samplerate / MasterStream.SampleRate)) +  FStartAt;

      if not loop then begin
        if result > (samplecount-1) then
          result := SAmpleCount-1;
      end else begin
        result := result mod SAmpleCount;
      end;
    end else begin
      result := stream.sampleposition;
      if not loop then begin
        if result > (samplecount-1) then
          result := SAmpleCount-1;
      end else begin
        result := result mod SAmpleCount;
      end;

    end;
  end else
    result := FStartAt;
end;

procedure TSoundStreamOscillator.SetFileName(const Value: string);
begin
  if assigned(FStream) then begin
    FStream.free;
    FStream := nil;
  end;

  FFileName := Value;

  FStream := TSoundStream.Create(Value, fmOpenRead + fmShareDenyNone);
  FStream.Loop := Loop;
  SampleRate := FStream.samplerate;
  SampleCount := FStream.SampleCount;

end;

procedure TSoundStreamOscillator.SetFreeWheelMode(const Value: boolean);
begin
  if MasterStream <> nil then
    raise Exception.create('Set freewheel mode before adding oscillator to stream.');
  FFreewheelMode := Value;
  if value then CacheREsults := true;
end;

procedure TSoundStreamOscillator.SetLoop(const Value: boolean);
begin
  FStream.Loop := Value;
end;

procedure TSoundStreamOscillator.SetPosition(const Value: int64);
begin
  FStartAt := value;
  FLAstSAmpleRequested := value;
  if Started then
    Start;
end;

procedure TSoundStreamOscillator.SetPositionInSeconds(const Value: nativefloat);
begin
  if self.samplerate = 0 then
    Position := 0
  else
    POsition := round(Value * Self.SampleRate);
end;

procedure TSoundStreamOscillator.SetStartAt(const Value: int64);
begin
  FStartAt := Value;
  Self.FLastSampleREquested := FStartAT;
end;

procedure TSoundStreamOscillator.Start;
begin
  FfirstSampleRequested := false;
  FStarted := true;
  FConcluded := false;

  if assigned(MasterStream) then begin
    Debug.Log(self,'StartAt:'+inttostr(self.StartAt));
    while not FFirstSampleRequested do sleep(0);
    Debug.Log(self,'MasterStream Time at Start:'+inttostr(MasterStream.PlayPosition));
    Debug.Log(self,'StartedAt:'+inttostr(self.StartedAt));
    Debug.Log(self,'Position:'+inttostr(self.Position));
    Debug.Log(self,'PositionInSeconds:'+floattostr(self.PositionInSeconds));
  end;

end;

procedure TSoundStreamOscillator.UnPause;
begin
  FfirstSampleRequested := false;
  FStarted := true;
  FConcluded := false;
end;


{ TSoundStream }


procedure TSoundStream.ApplyEq(buf: multibuffermemoryfilestream.PMBBufferInfo);
begin
  inherited;
//  if buf.offset > 0 then
//    ApplyEqToStereoBuffer(P16BitStereoSoundSample(buf.ptr), buf.used div 4, @Feq);
end;

constructor TSoundStream.Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal);
begin
  FChannels := 1;
  FSampleFormat := sf8;
  inherited CReate();
  filename := afilename;
  Create(TSoundFileStreamBase.create(afilename, mode, rights, flags), extractfileext(aFileName));


end;

constructor TSoundStream.Create(const AFileName: string; Mode: cardinal);
begin
  Create(AFileName, Mode, 0,0);
end;

constructor TSoundStream.Create(stream_takes_ownership: TStream; ext: string);
var
  bh: TBoogerHeader;
begin
  FSampleFormat := sf8;
  FChannels := 1;
  inherited Create();
  FRestingPOint := INVALID_RESTING_POINT;
  FUnderStream := stream_takes_ownership;

  if (Mode = fmCreate) or ((Mode and fmOpenWrite) = fmOpenWrite) then begin
    if lowercase(ext)='.mp3' then
      raise Exception.create('cannot open mp3 for write in TSoundStream');
    FChannels := 2;
    FSampleRate := 44100;
    FBytesPerSample := 2;
    WriteHeader;
  end
  else begin
    if lowercase(ext)='.mp3' then begin
      {$IFNDEF MSWINDOWS}
        raise ENotImplemented.create('not implemented on this platform');
      {$ELSE}
      if self.filename = '' then
        raise ENotImplemented.create('mp3 decompression only supported against file-based sound streams');

      FUnCompressed := Mp3ToRawMemoryStream(filename,mp3info);
      FChannels := 2;
      Resampled := true;
      FSampleRate := mp3info.samplerate;
//      FOriginalSampleRate := 44100;

      FBytesPerSample := 2;
      FSamples := FUncompressed.Size div 4;
      UpdateLastSampleStart;
      {$endif}
    end else begin
      bh := LoadHeader;



    end;

    Rewind;
    Volume := 1.0;
  end;
end;

destructor TSoundStream.Destroy;
begin

  if (Mode = fmCreate) or ((Mode and fmOpenWrite)=fmopenWrite) then
    WriteHeader;
  FUncompressed.Free;
  FUncompressed := nil;

  FUnderStream.free;
  FUnderStream := nil;

  inherited;
end;

function TSoundStream.GetNextSample(out ss: TStereoSoundSample): boolean;

begin
  if (Position < 0) or (Position > iLastSampleStart) then begin
    ss.Init;
    result := false;
    exit;
  end;

{$IFDEF USE_READ_FUNC}
    ss := fnReadNextSample;
{$ELSE}
  begin
    var
      iL, iR: smallint;
    Read(iL, 2);
    if channels > 1 then begin
      Read(iR, 2);
    end
    else begin
      iR := iL;
    end;
    ss.Left := iL / 32767;
    ss.Right := iR / 32767;
  end;
{$ENDIF}

  ss.Left := ss.Left * Volume;
  ss.Right := ss.Right * Volume;
  if Pan > 0 then begin
    ss.Left := ss.Left * (1 - Pan);
  end;
  if Pan < 0 then begin
    ss.Right := ss.Right * (1 - (0 - Pan));
  end;

  ss.Clip;
//  if ss.Left < -1.0 then
//    ss.Left := -1.0;
//  if ss.Right < -1.0 then
//    ss.Right := -1.0;
//  if ss.Left > 1.0 then
//    ss.Left := 1.0;
//  if ss.Right > 1.0 then
//    ss.Right := 1.0;

  result := true;

end;

procedure TSoundStream.GetPeaksFromSamplePosition(out rLeft, rRight: nativefloat);
var
  t: integer;
  ss: TStereoSoundSample;
  r1,r2: FloatSample;
begin
  rLeft := 0;
  rRight := 0;
  for t := 0 to 256 do begin
    GetNextSample(ss);
    r1 := ss.Left;
    r2 := ss.Right;
    r1 := abs(r1);
    r2 := abs(r2);
    if r1 > rLeft then
      rLeft := r1;
    if r2 > rRight then
      rRight := r2;

  end;

end;

function TSoundStream.GetResample(rTime: double; out ss: TStereoSoundSample): boolean;
var
  iSAmp1, iSamp2: int64;
  rWeight: NativeFloat;
  ssTemp: TStereoSoundSample;
begin
  result := false;
  iSamp1 := Floor(rTime);
  iSamp2 := iSamp1 + 1;

  rWeight := rTime - iSAmp1;
  if rWeight = 0 then begin
    SeekSample(iSamp1);
    result := GetNextSample(ss);
  end else begin
    SeekSample(iSamp1);
    result := GetNextSample(ss);
    if (GetNextSample(ssTemp)) then begin
      ss := (ss * (1-rWeight))+(ssTemp * rWeight);
    end;

  end;

end;

function TSoundStream.GetResample(targetSampleRate: ni; rTime: double;
  out ss: TStereoSoundSample): boolean;
begin
  if targetsamplerate = 0 then
    targetsamplerate := 44100;
  var nuTime := rTime;//(TArgetSampleRate *rTime) / SampleRate;
  result := GetResample(nuTime, ss);
end;

function TSoundStream.GetRestingPOint: single;
var
  sum: single;
  cnt: ni;
  t: ni;
  ss: TStereoSoundSample;
begin
  result := 0;
  if FRestingPoint = INVALID_RESTING_POINT then begin
    cnt := 0;

    sum := 0;
    self.SeekSample(0);
    for t:= 0 to 5000 do begin
      Self.GetNextSample(ss);
      inc(cnt);
      sum := sum + ss.Left;
    end;

    RESULT := sum / cnt;
    FRestingPoint := result;


  end;
end;

function TSoundStream.GetSampleAvg(iStart, iEnd: int64;
  iChannelMask: integer): nativefloat;
var
  t: integer;
  SS: TStereoSoundSample;
  n: FloatSample;
begin
  if iStart > SampleCount then begin
    result := 0;
    exit;
  end;
  if iEnd <= iStart then begin
    result := 0;
    exit;
  end;

  if iEnd >= SampleCount then
    iEnd := SampleCount - 1;

  SeekSample(iStart);
  result := 0;
  for t := 0 to (iEnd - iStart) - 1 do begin
    GetNextSample(ss);
    case ichannelmask and 3 of
      1: result := result + ss.left;
      2: result := result + ss.right;
      3: result := result + ss.left+ss.right;
    end;

  end;
  case ichannelmask and 3 of
    1,2: result := result / ((iEnd - iStart));
    3:result := result / ((iEnd - iStart)*2);
  end;

end;

function TSoundStream.GetSampleCount: int64;
begin
  if FUncompressed <> nil then
    result := FUncompressed.Size div 4
  else begin
    if channels = 0 then
      result := 0
    else
      result := (ByteLengthOfSamples div BytesperSample) div channels;
  end;
end;

function TSoundStream.GetSampleMin(iStart, iEnd: int64;
  iChannelMask: integer): nativefloat;
var
  t: integer;
  ss: TStereoSoundSample;
  n: FloatSample;
begin
  if iStart > SampleCount then begin
    result := 0;
    exit;
  end;
  SeekSample(iStart);
  result := 1;
  if iEnd >= SampleCount then
    iEnd := SampleCount - 1;
  for t := 0 to (iEnd - iStart) - 1 do begin
    GetNextSample(ss);
    n := LesserOf(abs(ss.left), abs(ss.right));
    if n < result then begin
      result := n;
      if result = 0 then
        break;
    end;
  end;
end;

function TSoundStream.GetSamplePeak(iStart, iEnd: int64;
  iChannelMask: integer): nativefloat;
var
  t: integer;
  ss: TStereoSoundSample;
  n: FloatSample;
begin
  if iStart > SampleCount then begin
    result := 0;
    exit;
  end;
  SeekSample(iStart);
  result := 0;
  if iEnd >= SampleCount then
    iEnd := SampleCount - 1;

  for t := 0 to (iEnd - iStart) - 1 do begin
    GetNextSample(ss);
    n := GreaterOf(abs(ss.left), abs(ss.right));
    if n > result then begin
      result := n;
      if result > 0.999 then
        break;
    end;
  end;
end;

function TSoundStream.GetSize: int64;
begin
  if not Assigned(FUncompressed) then
    result := FUnderstream.size
  else
    result := FUncompressed.Size;
end;

function TSoundStream.GetSoundSample_16i_1ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_16i_1ch(self);
end;

function TSoundStream.GetSoundSample_16i_2ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_16i_2ch(self);
end;

function TSoundStream.GetSoundSample_24i_1ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_24i_1ch(self);
end;

function TSoundStream.GetSoundSample_24i_2ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_24i_2ch(self);

end;

function TSoundStream.GetSoundSample_32f_1ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_32f_1ch(self);
end;

function TSoundStream.GetSoundSample_32f_2ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_32f_2ch(self);

end;

function TSoundStream.GetSoundSample_32i_1ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_32i_1ch(self);
end;

function TSoundStream.GetSoundSample_32i_2ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_32i_2ch(self);
end;

function TSoundStream.GetSoundSample_64f_1ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_64f_1ch(self);
end;

function TSoundStream.GetSoundSample_64f_2ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_64f_2ch(self);
end;

function TSoundStream.GetSoundSample_64i_1ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_64i_1ch(self);
end;

function TSoundStream.GetSoundSample_64i_2ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_64i_2ch(self);
end;

function TSoundStream.GetSoundSample_8i_1ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_8i_1ch(self);
end;

function TSoundStream.GetSoundSample_8i_2ch: TStereoSoundSample;
begin
  result := ss_GetSoundSample_8i_2ch(self);
end;

function TSoundStream.LengthInSeconds: nativefloat;
begin
  result := samplecount / samplerate;
end;

function TSoundStream.LoadHeader: TBoogerHeader;
begin
  Seek(0, soBeginning);
  Stream_GuaranteeRead(self, Pbyte(@result), sizeof(result));
  if (result.fourcc[0] = ord(ansichar('R')))
  and (result.fourcc[1] = ord(ansichar('I')))
  and (result.fourcc[2] = ord(ansichar('F')))
  and (result.fourcc[3] = ord(ansichar('F'))) then begin
    //NEW IN 2020, better, more universal sampler for WAV files,
    //should support lots of formats and sample rates
    microsoftWAV := TSoundWaveSampler.Create;
    microsoftWAV.fileName := self.filename;

    var fmt := microsoftWAV.GetFormat;
    Channels := fmt.channels;
    FSamples := fmt.lengthInSamples;
    FSampleRate := fmt.rate;
    FBytesPerSample := fmt.bytespersample;
    SampleFormat := fmt.fmt;
    SampleDataStart := microsoftWAV.DataStart;
    FByteLength := Size - SampleDataStart;

  end else begin
    //LOAD ADA3 format
    if result.fourcc[0] <> 65{'A'} then
      raise Exception.Create('Invalid FOURCC. Only ADA3 supported for boog files');
    if result.fourcc[1] <> 68{'D'} then
      raise Exception.Create('Invalid FOURCC. Only ADA3 supported for boog files');
    if result.fourcc[2] <> 65{'A'} then
      raise Exception.Create('Invalid FOURCC. Only ADA3 supported for boog files');
    if result.fourcc[3] <> 51{'3'} then
      raise Exception.Create('Invalid FOURCC. Only ADA3 supported for boog files');

    SampleDataStart := SOUND_HEADER_LENGTH_v3;
    FChannels := result.channels;
    self.FSamples := result.length div (FChannels * 2);
    FByteLength := result.length;
    FChannels := result.channels;
    FSampleRate := result.samplerate;
    FBytesPerSample := 2;
    SampleFormat := sf16;


  end;
  UpdateLastSampleStart;

end;



function TSoundStream.PositionInSeconds: nativefloat;
begin
  result := (sampleposition) / samplerate;
end;

procedure TSoundStream.PrefetchSample(iSample: int64; iLength: int64);
begin
  if FUnderstream is TSoundFileStreamBase then
    TSoundFileStreamBase(FUnderStream).Prefetch(SampleDataStart + SamplesToBytes(iSample),
      SamplesToBytes(iLength));
end;

function TSoundStream.Read(var Buffer; Count: longint): Longint;
begin
  if not assigned(FUncompressed) then
    result := FUnderStream.read(buffer, count)
  else
    result := FUncompressed.Read(buffer, count);
end;

procedure TSoundStream.ResetUDP;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TSoundStream.Rewind;
begin
  Seek(SampleDataStart, soBeginning);
end;

function TSoundStream.SamplePosition: int64;
begin
  result := (position div 2) div channels;

end;

function TSoundStream.SamplesToBytes(iSample: int64): int64;
begin
  result := channels * BytesperSample * iSample;
end;

function TSoundStream.Seek(const offset: int64; Origin: TSeekOrigin): int64;
begin
  if not assigned(FUncompressed) then
    result := FUnderStream.seek(offset, origin)
  else
    result := FUncompressed.Seek(offset, origin);

end;

procedure TSoundStream.SeekSample(iSample: int64);
var
  hl: nativeint;

begin
  if assigned(FUncompressed) then
    hl := 0
  else
    hl := SampleDataStart;

  if Loop then
    Seek(hl + SamplesToBytes(iSample mod SampleCount),
      soBeginning)
  else
    Seek(hl + SamplesToBytes(iSample), soBeginning)
end;

procedure TSoundStream.SetBufferEntireFile(const Value: boolean);
begin
  FBufferEntireFile := Value;
  if value then begin
    if FUnderstream is TMemoryFileStream then
      TMEmoryFileStream(FUnderStream).BufferSize := lesserof(Size, 200000000);
  end;

end;

procedure TSoundStream.SetChannels(const Value: smallint);
begin
  FChannels := Value;
  UpdateSampleFunctionPointer;
end;

procedure TSoundStream.SetSampleFormat(const Value: TXSoundFormat);
begin
  FSampleFormat := Value;
  UpdateSampleFunctionPointer;



end;

procedure TSoundStream.UpdateLastSampleStart;
var
  iUseSize: int64;
begin
  if FUncompressed <> nil then
    iUseSize := FUncompressed.Size
  else
    iUsesize := size;
  iLastSampleStart := iUseSize-(channels*BytesperSample);
end;

procedure TSoundStream.UpdateSampleFunctionPointer;
begin
  case channels of
    1:
      case FSampleFormat of
        sf8: fnReadNextSample := self.GetSoundSample_8i_1ch;
        sf16: fnReadNextSample := self.GetSoundSample_16i_1ch;
        sf24: fnReadNextSample := self.GetSoundSample_24i_1ch;
        sf32: fnReadNextSample := self.GetSoundSample_32i_1ch;
        sf64: fnReadNextSample := self.GetSoundSample_64i_1ch;
        sf32f: fnReadNextSample := self.GetSoundSample_32f_1ch;
        sf64f: fnReadNextSample := self.GetSoundSample_64f_1ch;
      else
        raise ECritical.create('sample format not handled');
      end;
    2:
      case FSampleFormat of
        sf8: fnReadNextSample := self.GetSoundSample_8i_2ch;
        sf16: fnReadNextSample := self.GetSoundSample_16i_2ch;
        sf24: fnReadNextSample := self.GetSoundSample_24i_2ch;
        sf32: fnReadNextSample := self.GetSoundSample_32i_2ch;
        sf64: fnReadNextSample := self.GetSoundSample_64i_2ch;
        sf32f: fnReadNextSample := self.GetSoundSample_32f_2ch;
        sf64f: fnReadNextSample := self.GetSoundSample_64f_2ch;
      else
        raise ECritical.create('sample format not handled');
      end;
  else
    raise ECritical.create('channel count '+channels.tostring+' not handled');
  end;
end;

procedure TSoundStream.WriteHeader;
var
  bh: TBoogerHeader;

begin
  Seek(0, soBeginning);

  bh.fourcc[0] := 66{'B'};
  bh.fourcc[1] := 79{'O'};
  bh.fourcc[2] := 79{'O'};
  bh.fourcc[3] := 71{'G'};
  bh.channels := FChannels;
  bh.length := Self.Size - SOUND_HEADER_LENGTH_v3;
//  bh.length := BytesperSample * Channels * SampleCount;


  Stream_guaranteeWrite(self, Pbyte(@bh), sizeof(bh));

end;

procedure TSoundStream.WriteNextSample(rLeft, rRight: FLoatSample);
var
  iLeft, iRight: smallint;
begin
  if channels = 1 then begin
    iLeft := round((rLeft + rRight * 32767) / 2);
    Stream_guaranteeWrite(self, Pbyte(@iLeft), sizeof(iLeft));

  end
  else begin
    iLeft := round(rLeft * 32767);
    iRight := round(rRight * 32767);
    Stream_guaranteeWrite(self, Pbyte(@iLeft), sizeof(iLeft));
    Stream_guaranteeWrite(self, Pbyte(@iRight), sizeof(iRight));

  end;

end;





function GetMp3Info(sFile: string): TMp3Info;overload;
var
  p: pbyte;
  fs: TFileStream;
  t: nativeint;
  mp3header: array[0..3] of byte;
begin
//  res
    fs := TFileStream.Create(sFile, fmOpenRead+fmShareDenyWrite);
    try
      for t:= 0 to fs.size-5 do begin
        fs.seek(t,soBeginning);
        stream_guaranteeread(fs, @mp3header[0], 4, true);
        if (mp3header[0] = $ff) and ((mp3header[1] and $D0) = $D0) then begin
          result := GetMp3Info(@mp3header[0]);
          exit;
        end;
      end;


      //result := GEtMp3Info(p);
    finally
      fs.Free;
    end;
end;

{$IFDEF OLD_MP3_CODE}
var
  p: pbyte;
  fs: TFileStream;
begin
  p := GEtMemory(65536);
  try
    fs := TFileStream.Create(sFile, fmOpenRead+fmShareDenyWrite);
    try
      stream_guaranteeread(fs, p, lesserof(65536, fs.Size), true);
      result := GEtMp3Info(p);
    finally
      fs.Free;
    end;

  finally
    FreeMemory(p);

  end;
end;
{$ENDIF}

function GetMp3Info(p: Pbyte): TMp3Info;overload;
begin
//  movemem32(@result.id3header, p, sizeof(result.id3header));
//  movemem32(@mp3header[0], @p[result.id3header.Size+sizeof(TId3header)], 4);
  if p[0] <> $FF then
    raise Exception.Create('invalid mp3 header '+ inttohex(p[0],2));

  result.samplerate := 44100;
  case (p[1] shr 3) and 3 of
    0:
    case (p[2] shr 2) and 3 of     //MPEG LAYER 2.5
      0: result.samplerate := 11025;
      1: result.samplerate := 12000;
      2: result.samplerate := 8000;
    end;
    2:
    case (p[2] shr 2) and 3 of     //MPEG LAYER 2
      0: result.samplerate := 22050;
      1: result.samplerate := 24000;
      2: result.samplerate := 16000;
    end;
    3:
    case (p[2] shr 2) and 3 of     //MPEG LAYER 1
      0: result.samplerate := 44100;
      1: result.samplerate := 48000;
      2: result.samplerate := 32000;
    end;
  end;

  if ((p[3] shr 6) and 3) = 3 then
    result.samplerate := result.samplerate div  2;

end;



function TId3Header.Size: integer;
begin
  result := (size_7bit[0] shl 21) +
            (size_7bit[1] shl 14)+
            (size_7bit[2]shl 7) +
            (size_7bit[3] shl 0);

end;


{ TSoundStreamBase }

constructor TSoundStreamFakeBase.Create(const AFileName: string; Mode: WORD);
begin
  inherited Create(afilename, mode);
end;

constructor TSoundStreamFakeBase.Create(const AFileName: string; Mode: WORD;
  Rights: cardinal);
begin
  inherited Create(afilename, mode, rights);
end;

function TSoundStreamFakeBase.GetiSeekPosition: int64;
begin
  result := __iseekposition;
end;


procedure TSoundStreamFakeBase.Prefetch(iPosition, iLength: int64);
begin
  //
end;

procedure TSoundStreamFakeBase.setiSeekPosition(const Value: int64);
begin
  Self.__iseekposition := value;
end;

{ TBoogerHeader }

procedure TBoogerHeader.Init;
begin
  FillMem(@self, sizeof(self), 0);
  fourcc[0] := 65;//'A';
  fourcc[1] := 68;//'D';
  fourcc[2] := 65;//'A';
  fourcc[3] := 51;//'3';
  ver := 3;
  SampleRate := 44100;
  Channels := 2;
  cue1 := -1;
  cue2 := -1;
  loop1 := -1;
  loop2 := -1;
  length := 0;

end;

procedure TBoogerHeader.REadFromStream(s: TStream);
var
  ipos: int64;
begin
  iPOs := s.position;
  s.seek(0,soBeginning);
  stream_GuaranteeRead(s, pbyte(@self), sizeof(self));
  s.seek(iPos,soBeginning);

end;

procedure TBoogerHeader.WriteToStream(s: TStream);
var
  ipos: int64;
begin
  iPOs := s.position;
  s.seek(0,soBeginning);
  stream_GuaranteeWrite(s, pbyte(@self), sizeof(self));
  s.seek(iPos,soBeginning);

end;

{ TSoundPlaybackInfo }

procedure TSoundPlaybackInfo.Init;
begin
  Debug.log('TSoundPlaybackInfo@'+inttohex(nativeint(@self),16)+' init');
  samplerate := 44100;
  channels := 2;
  fmt:= sf16;
end;

procedure TSoundPlaybackInfo.SetSampleRate(const Value: integer);
begin
  FSampleRate := Value;
end;

end.
