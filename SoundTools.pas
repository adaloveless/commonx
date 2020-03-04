unit soundtools;
{$Message '********************************Compiling soundtools'}
{$I DelphiDefs.inc}
{$INLINE AUTO}

{$DEFINE FREEWHEEL2}

{$TypedAddress Off}
{x$O-}

{$DEFINE THREAD_SOUND_MODS}
{x$DEFINE USE_PLAYBACK_INFO}


{x$DEFINE PRESERVE_XMM}
{x$O-}
{
push rbp
push rdi
push rsi
sub rsp,$10
move rbp,rsp
move [rbp+#30],rcx

//r := gtest
lea rdi,[rbp+$00]
lea rsi,[rel $00044953]
movsq
movsq

//result := gtest
mov rdi,[rbp+$00]
lea rsi,[rel $00044944]
movsq
movsq

//end
mov rax,[rbp+$30]
lea rsp,[rbp+$10]
pop rsi
pop rdi
pop rbp
ret

}

interface

uses
  system.rtlconsts,system.types,simplequeue, synth_functions,
  commonConstants, typex, soundinterfaces, betterobject, soundsample,
{$ifdef mswindows}
  winapi.windows,
  system.win.scktcomp,
{$ENDIF}
  generics.collections.fixed, backgroundthreads, classes,  signals,
{$IFDEF WASAPI}
  AudioClient,
  AudioPolicy,
  AudioSessionTypes,
{$ENDIF}
  tools, tickcount, sysutils, commandicons, geometry, managedthread, threadmanager, commandprocessor, exe, multibuffermemoryfilestream,
  sharedobject, stringx, debug, math, idudpserver, idglobal, idsockethandle, helpers_stream, systemx, numbers;

{ x$define SOUND_STREAM_THREAD_DEBUG_ENABLE }


const
  MAX_LAG_RECORDS = 1000;
  CLAG_FINE_ADJUST =   0.00003;
  CLAG_COARSE_ADJUST =  0.0001;
  CLAG_LOCK_WINDoW = 5;
  CLAG_UNLOCK_WINDOW = 100;
  CLAG_RECORDS_WHEN_LOCKED = 399;
  CLAG_RECORDS_WHEN_UNLOCKED = 99;
  DEFAULT_TARGET_UDP_LAG = 60;
  LAG_MARGIN = 1;
  SOUND_PACKET_DATA_SIZE = 256;
  UDP_BACKLOG_SIZE = 512;
  SOUND_PACKET_LOG_SIZE = MAX_STREAM_BUFFER_SIZE div SOUND_PACKET_DATA_SIZE;
  //SOUND_PACKET_LOG_SIZE = 32*(DEFAULT_TARGET_UDP_LAG div (1000 div (48000 div SOUND_PACKET_DATA_SIZE)));
  MULTIBAND_EQ_MAX_BANDS = 64;

  BUF_CURRENT = 0;
  EQ_GUTTER = 0.3;

type
  TgamePlatform = (pfXbox, pfPC, pfPS3);

  TFreewheelState = (fwSteady, fwShouldSlowDown, fwShouldSpeedUp);

  TTempoAnalysis = record
    Tempo: nativefloat;
  end;
  TLocalMFS = TMultiBufferMemoryFileStream;


  TRFAPacket = packed record
    ThreeCC: array [0..2] of byte;
    BuffersTotal: smallint;
    BuffersNeeded: smallint;
    LastSequenceNumberProcessed: int64;
    LastSequenceNumberReceived: int64;
    pingreferencetime: cardinal;
    MissingPosition:int64;
    procedure Init;
  end;

  PRFAPacket = ^TRFAPacket;

  T16BitStereoSoundSample = packed record
    ch: array [0..1] of smallint;
  end;

  T16BitStereoSoundSAmpleArray = array [0..0] of T16BitStereoSoundSample;

  P16BitStereoSoundSample =  ^T16BitStereoSoundSample;
  P16BitStereoSoundSampleArray =  ^T16BitStereoSoundSampleArray;
//  PStereoSoundSample = ^TStereoSoundSAmple;

  TWaveInfoAfterFMT = packed record
    tag: WORD;
    channels: WORD;
    SamplesPErSecond: DWORD;
    BytesPErSecond: DWORD;
    BLockAlign: WORD;
    BitsPerSample: WORD;
  end;

  TWaveVU = packed record
    Min: single;
    Max: single;
    Avg: single;
  end;

  TWaveVUs = packed record
    Short: TWaveVU;
    Medium: TWaveVU;
    Long: TWaveVU;
  end;



  TPacketSampleFormat = T16bitStereoSoundSample;
  TPacketSoundSample = TPacketSampleFormat;
  PPacketSoundSample = ^TPacketSoundSample;


  TSoundPacket = packed record
    LocalSampleTime: int64;
    RemoteSampleTime: int64;
    MeasuredSampleLag: int64;
    Data: array[0..SOUND_PACKET_DATA_SIZE-1] of TPacketSampleFormat;
    PingReferenceTime: cardinal;
    SetBufferSizeTo: cardinal;
    RealTimeReceived: cardinal;
    LagRecordsWhenUnlocked: smallint;
    LagRecordsWhenLocked: smallint;
    TargetLag: smallint;
    FineAdjust: single;
    CoarseAdjust: single;
    LockMargin: smallint;
    UnlockMArgin: smallint;

    function RemoteSampleEndTime: int64;
    function LocalSampleEndTime: int64;
  end;






//  TAbstractSoundDevice = class; //forward



  TRemoteAudio = class(TBetterObject)
  private
    FPort: cardinal;
    FIp: string;
    FLag: nativeint;
    FMulticastPeer: boolean;
  public
    property IP: string read FIp write Fip;
    property Port: cardinal read FPort write FPort;
    property Lag: nativeint read FLag write FLag;
    property MulticastPeer: boolean read FMulticastPeer write FMulticastPeer;
  end;

  TRemoteAudioList = class(TList<TRemoteAudio>)
  public
    function IndexOf(sIP: string; iport: cardinal): nativeint;
  end;

  TQueuedUDPPacket = class(TQueueItem)
  public
    ip: string;
    port: ni;
    data: TIDBytes;
    client: TidUDPServer;
    procedure Init;override;
    procedure DoExecute;override;
  end;


  TAbstractSoundDevice = class(TManagedThread, ISoundOscillatorRenderer)
  private
    procedure SetDeviceID(const Value: integer);
    function GetDevice(idx: ni): string;
    function GetDeviceCount: ni;
    procedure SetDeviceName(const Value: string);
    function GetDeviceListH: IHolder<TStringlist>;
  protected
    Factive: boolean;
    FFrequency: cardinal;
    FEngineStartTime: int64;
    FBufferSize: integer;
    FDeviceID: integer;
    FPendingDeviceChange: boolean;
    FRemoteport: string;
    FRemoteHost: string;
    FDeviceName: string;
    FDeviceNames: TStringlist;
    FRemote: boolean;
    FOutPacket: TSoundPacket;
    FOutPacketDataPtr: nativeint;
    FPlayPosition: int64;
    FOscillators: TList<TSoundOscillator>;
    FDeviceList: TStringlist;
    FTaskHandle: THandle;
    FTaskIndex: integer;
    fMuteAudio: boolean;
    FPlayOrigin: int64;
    iters: int64;
    fillptr: int64;
    FACtualBufferSize: nativeint;
    FBuf: array [0 .. (MAX_STREAM_BUFFER_SIZE - 1)] of T16BitStereoSoundSample;

    function GetOscillatorCount: integer;
    function GetOscillator(idx: integer): TSoundOscillator;
    function GetMuteAudio: boolean;
    procedure SetMuteAudio(const Value: boolean);
    function GetPosition_int64: int64;
    function GetPlayPosition: int64;
    function GEtNyquistFrequency: nativeint;
    function GEtSampleRate: nativeint;
    procedure SetBufferSize(const Value: nativeint);
    function GetBufferSize: nativeint;
    function GetEngineSTartTime: int64;
    procedure SetEngineStartTime(const Value: int64);
    procedure ResolveDeviceName;virtual;
    procedure DeviceChanged;virtual;
  public
{$IFDEF USE_PLAYBACK_INFO}
    playbackinfo: TSoundPlaybackInfo;
{$ENDIF}
    UDP_LagRecordsWhenUnlocked: smallint;
    UDP_LagRecordsWhenLocked: smallint;
    UDP_FineAdjust: single;
    UDP_CoarseAdjust: single;
    UDP_BufferSize: cardinal;
    UDp_TargetLag: cardinal;
    UDp_LockMArgin: smallint;
    UDp_UnlockMargin: smallint;
    udpqueue: TSimpleQueue;


    remoteaudio: TRemoteAudioList;
    udpc: TidUDPServer;
    FUDPBacklog: array[0..UDP_BACKLOG_SIZE-1] of TSoundPacket;
    FUDPBAcklogPtr: nativeint;
    FBacklogFill: nativeint;
    FSampleRate: ni;
    ControlRoomVolume: single;
    procedure MakeSureQueueIsSTarted;
    procedure MakeSureQueueIsStopped;
    constructor Create(Owner: TObject; Manager: TThreadManager; pool: TThreadpoolBase); override;
    procedure CreateUDPConnection;
    destructor Destroy; override;
    procedure BeforeDestruction;override;
    procedure DoExecute; override;
    procedure BuildUDPPacket(iSampleTime: int64; rLeft, rRight: nativefloat);
    procedure DoExecuteSSE;
    procedure AudioLoop;virtual;
    procedure DoExecuteDSound;
    property OscillatorCount: integer read GetOscillatorCount;
    property Oscillators[idx: integer]: TSoundOscillator read GetOscillator;
    function AddOscillator(p: TSoundOscillatorHookSSE; o: TObject): TSoundOscillator; overload;
    procedure AddOscillator(o: ToscillatorObject); overload;
    procedure RemoveOscillator(o: ToscillatorObject); overload;
    procedure RemoveOscillator(p: TSoundOscillatorHookSSE; o: TObject); overload;
    property PlayPositionSinceEngineStart: int64 read FPlayPosition write FPlayPosition;
    property PlayPosition: int64 read GetPlayPosition;
    property PlayOrigin: int64 read FPlayOrigin write FPlayOrigin;
    property Frequency: cardinal read FFrequency;
    property MuteAudio: boolean read GetMuteAudio write SetMuteAudio;
    property Position_int64: int64 read GetPosition_int64;
    procedure ResetPlayOrigin;
    property EngineStartTime : int64 read GetEngineSTartTime write SetEngineStartTime;
    property DeviceID: integer read FDeviceID write SetDeviceID;
    property Buffersize: nativeint read GetBufferSize write SetBufferSize;
    property SampleRate: nativeint read GEtSampleRate write FSampleRate;
    property NyquistFrequency: nativeint read GEtNyquistFrequency;
    property Remote: boolean read FRemote write FRemote;
    property RemoteHost: string read FRemoteHost write FRemoteHost;
    property RemotePort: string read FRemoteport write FRemotePort;

    procedure UDPOnReceive(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);


    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure SetupWave;virtual;
    procedure CleanupWave;virtual;
    procedure BeginSampling(iSampleTime: int64);
    procedure EndSampling(iSampleTime: int64);
    function RenderSample(iSample: int64; out ss: TStereoSoundSample): boolean;

    procedure RefreshDevices;virtual;
    property DeviceCount: ni read GetDeviceCount;
    property Devices[idx: ni]: string read GetDevice;
    property DeviceName: string read FDeviceName write SetDeviceName;
    property DeviceList: IHolder<TStringlist> read GetDeviceListH;
    property Active: boolean read FActive;
  end;


  TMetronomeOscillator = class(ToscillatorObject)
  private
    FTempo: nativefloat;
    FAmplitude: nativefloat;
    FComputerBeep: boolean;
    FLastFalloff: nativefloat;
  public
    constructor Create; override;
    procedure o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64); override;
    property Tempo: nativefloat read FTempo write FTempo;
    property Volume: nativefloat read FAmplitude write FAmplitude;
    property ComputerBeep: boolean read FComputerBeep write FComputerBeep;
  end;

  TManualMetronomeOscillator = class(ToscillatorObject)
  private
    FenvelopeTime: int64;
    FEnvelope: nativefloat;
    FVolume: nativefloat;
    FLastFalloff: nativefloat;
    FBeepNow: boolean;
  protected
    property BeepNow: boolean read FBeepNow write FBeepNow;
  public
    constructor Create; override;
    procedure o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64); override;
    property Volume: nativefloat read FVOlume write FVOlume;
    procedure Beep;
  end;




  TSynchronousStreamOscillator = class(TOscillatorObject)
  private
    buf: PByte;
    buflen: nativeint;
    bufstart: int64;
    FNeedsDAta: boolean;
    function EndSample: int64;
  public
    procedure o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64); override;

    property NeedsData: boolean read FNeedsDAta;
    procedure GiveData(iStartSample: int64; p: PByte; length: nativeint);
  end;


  TComplexSoundModifierOscillator = class(TSoundModifierOscillator)
  //This class is even moar awesome than its ancestor and will allow you to
  //implement effects that need their buffers in the frequency domain.
  //for some good examples... look at the EQ imeplementation
  protected
    procedure Buffer_PreProcess_Sync(b: POscBufferInfo);override;
    procedure Buffer_Process_Sync(b: POscBufferInfo);override;
  public
    procedure o(mt: ToscMessageType; out s: TstereoSoundSample; iSampletime: int64); override;

  end;

  TFadeDirection = (fdIn, fdOut);

  Tmodosc_Fade = class(TSoundModifierOscillator)
  private
    FFadeDurationInSeconds: nativefloat;
    FDirection: TFadeDirection;
    FEnabled: boolean;
    FStartFadeOnNextSample: boolean;
    FFadeEndSample: int64;
    FFadeStartSample: int64;
    FCurrentFader: nativefloat;
    FAutomate: boolean;
    property FadeDurationInSeconds: nativefloat read FFadeDurationInSeconds write FFadeDurationInSeconds;
    property Direction: TFadeDirection read FDirection write FDirection;

  public

    procedure o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64); override;


    procedure FadeNow(rDurationInSeconds: nativefloat; dir: TFadeDirection);

    property CurrentFader: nativefloat read FCurrentFader write FCurrentFader;
    property Automate: boolean read FAutomate write FAutomate;
  end;

  TModosc_Gain = class(TSoundModifierOscillator)
  protected
    FLevel: floatsample;
  public
    procedure o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64); override;

    procedure Init;override;
    property Level: floatsample read FLevel write FLevel;
  end;


  Tmodosc_Null = class(TComplexSoundModifierOscillator)
  protected
    procedure Buffer_Process_Sync(b: POscBufferInfo);override;
  public
    procedure o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64); override;

  end;

  Tmodosc_SimpleEQ = class(TSoundModifierOscillator)
  private
    FBass: NativeFloat;
    FLevel: floatsample;
    FCrossOver: NativeFloat;
    FTreble: NativeFloat;
  public
    procedure o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64); override;
    procedure Init;override;
    property Level: floatsample read FLevel write FLevel;
    property Bass: NativeFloat read FBass write FBass;
    property Treble: NativeFloat read FTreble write FTreble;
    property CrossOver: NativeFloat read FCrossOver write FCrossOver;
  end;

  Tmodosc_EQ = class(TComplexSoundModifierOscillator)
  private
    FBass: NativeFloat;
    FCrossOver: NativeFloat;
    FTreble: NativeFloat;
    procedure DoEQ(b: POscBufferInfo; iChannel: integer);
  protected
    procedure Buffer_Process_Sync(b: POscBufferInfo);override;
    procedure Init;override;
  public

    procedure o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64); override;
    property Bass: NativeFloat read FBass write FBass;
    property Treble: NativeFloat read FTreble write FTreble;
    property CrossOver: NativeFloat read FCrossOver write FCrossOver;
  end;

  Tmodosc_MultiBandEQ = class(TComplexSoundModifierOscillator)
  private
    //FNoiseTest: boolean;
    FBandCount: nativeint;
    FBands: array [0..MULTIBAND_EQ_MAX_BANDS-1] of nativefloat;
    FCrossOvers: array[0..MULTIBAND_EQ_MAX_BANDS-2] of nativefloat;
    FCrossOverPercentOfNyquist: array[0..MULTIBAND_EQ_MAX_BANDS-2] of NativeFloat;

    procedure DoEQ(b: POscBufferInfo; iChannel: integer);
    function GEtBands(idx: nativeint): NativeFloat;
    function GetCrossOvers(idx: nativeint): NativeFloat;
    procedure SetBandCount(const Value: nativeint);
    procedure SetBands(idx: nativeint; const Value: NativeFloat);
    procedure SetCrossOvers(idx: nativeint; const Value: NativeFloat);
  protected
    procedure Buffer_Process_Sync(b: POscBufferInfo);override;
    procedure SourcesChanged;override;

  public
    procedure Init;override;
    procedure RecalcCrossovers;
    procedure o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64); override;

    property Bands[idx: nativeint]: NativeFloat read GEtBands write SetBands;
    property CrossOvers[idx: nativeint]: NativeFloat read GetCrossOvers write SetCrossOvers;
    property BandCount: nativeint read FBandCount write SetBandCount;
    //property NoiseTest: boolean read FNoiseTEst write FNoiseTEst;

  end;

  Tmodosc_SpectrumShift= class(TComplexSoundModifierOscillator)
  private
    FShiftOctaves: nativefloat;
  protected
    procedure Init;override;
    procedure DOit(b: POscBufferInfo; iChannel: integer);
    procedure Buffer_Process_Sync(b: POscBufferInfo);override;
  public
    property ShiftOctaves: nativefloat read FShiftOctaves write FShiftOctaves;
  end;

  Tmodosc_Delay = class(TComplexSoundModifierOscillator)
  private
    FDelayTimeR: nativefloat;
    FDelayTimeL: nativefloat;
    FMix: nativefloat;
    function GetCutoffFrequency: nativefloat;
    procedure SetCutoffFrequency(const Value: nativefloat);
  protected
    procedure SourcesChanged;override;
    procedure Init;override;
    procedure Buffer_Process_Sync(b: POscBufferInfo);override;
    procedure BeforeDestruction;override;
  public
    eq: Tmodosc_MultibandEQ;
    property DelayTimeL: nativefloat read FDelayTimeL write FDelayTimeL;
    property DelayTimeR: nativefloat read FDelayTimeR write FDelayTimeR;
    property CutoffFrequency: nativefloat read GetCutoffFrequency write SetCutoffFrequency;
    property Mix: nativefloat read FMix write FMix;

  end;





  TVUSummary = record
    AvgPeak: single;
    Peak: single;
    PeakL: single;
    PeakR: single;
    MinL, MaxL, MinR, MaxR: single;
    Balance: single;
    SamplesRolledUp: single;
    Triggernumber: integer;
    NewTrigger: boolean;
    procedure CopyFrom(src: TVUSummary);
    procedure BeginRollUp();
    procedure RollUpWith(src:TVUSummary);
    procedure FinishRollup();
    procedure ValidateLimits;
  end;

  TTurbulenceCollection = packed record
    function NormalizedTurbulence(): nativefloat;
    case integer of
      0: (
          Amp_Frame: TVUSummary;
          Amp_Smooth: TVUSummary;
          Amp_Beat: TVUSummary;
          Amp_MEasure: TVUSummary;
          Amp_Phrase: TVUSummary;
          Amp_Track: TVUSummary;
          Turb_ReachX: integer;
          Turb_ModulationX: single;

      );
      1:
        (
          slopes : Array[0..5] of TVUSummary;
          Turb_Reach: integer;
          Turb_Modulation: single;

        );
  end;

  TTurbulenceRecord = packed record
    StartSample: int64;
    EndSample: int64;
    case integer of
      0: (
        Low: TTurbulenceCollection;
        Mid: TTurbulenceCollection;
        High: TTurbulenceCollection;
        );
      1:
        (
        bands: Array[0..2] of TTurbulenceCollection;
        );
  end;
  TTurbulenceArray = array[0..0] of TTurbulenceRecord;
  PTurbulenceRecord = ^TTurbulenceRecord;
  PTurbulenceArray = ^TTurbulenceArray;

  TTurbRunningData = record
    LastUpdatedTime: nativefloat;
  end;


  Tcmd_GenerateTurbulenceData = class(TSoundCommand)
  private
    FFileName: string;
    FFPS: nativefloat;
    sso: TSoundStreamOscillator;
    modeq: Tmodosc_MultibandEQ;
    Fdata: PTurbulenceArray;
    FRecordCount: nativeint;
    FRebuild: boolean;

    procedure BuildFrames(pass: nativeint);
    function RollUpFrames(iFrom, iTo, iPass: nativeint): TVUSummary;
    function GetRecord(idx: integer): PTurbulenceRecord;
    procedure AllocateRecords;
    procedure CleanupRecords;
    procedure SetRecordCount(const Value: nativeint);
    function GetSSOSpan(iStart,iEnd: int64): TVUSummary;
    function FindTurbulenceReach(iStart: nativeint; iPass: nativeint; out iTurbStart: nativeint): nativeint;
    function FindTurbulenceModulation(iStart, iEnd: nativeint; iPass: nativeint): nativefloat;
    function IsNewTrigger(iREcord, iBand, iSlope: nativeint; var bGateStatus: boolean): boolean;
  protected
    procedure Init;override;
  public
    RunningData: array [0..2] of array [0..5] of TTurbRunningData;
    destructor Destroy;override;
    procedure DoExecute;override;
    property FileName: string read FFileName write FFilename;
    property Rebuild: boolean read FRebuild write FRebuild;
    function TurbFileName: string;
    property FPS: nativefloat read FFPS write FFPS;
    property Records[idx: integer]: PTurbulenceRecord read GetRecord;
    property RecordCount: nativeint read FRecordCount write SetRecordCount;
    procedure SaveToFile;
    procedure LoadFromFile;
  end;

  Tcmd_ExtractTempo = class(TSoundCommand)
  private
    FFileName: string;
  public
    property FileName: string read FFileName write FFileName;
  end;




  PSoundPacket = ^TSoundPacket;

  TSynthOscillatorObject = class(TOscillatorObject)
  protected
    FAmp: NativeFloat;
    FFreq: NativeFloat;
    FRelease: nativefloat;
    FAttack: nativefloat;
    FAttackBeginTime: int64;
    FReleaseBeginTime: int64;
  public
    CurrentSample: int64;
    WasAttacked: boolean;
    WasReleased: boolean;
    NeedsRelease: boolean;
    ReleaseComplete: boolean;
    property Freq: NativeFloat read FFreq write FFreq;
    property Amp: NativeFloat read FAmp write FAmp;
    property Attack: nativefloat read FAttack write FAttack;
    property Release: nativefloat read FRelease write FRelease;
    function IsReleaseComplete(const iSampleTime: int64): boolean;
    function GetAttackAmp(const iSampleTime: int64): nativefloat;
    function GetReleaseAmp(const iSampletime: int64): nativefloat;
    procedure o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64); override;
    procedure init;override;
  end;

  TSineWaveOscillator=class(TSynthOscillatorObject)
  public
    procedure Init;override;
    procedure o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64); override;

  end;





  TStreamPerformanceMetric = record
    iPosition: int64;
    readtime: cardinal;
    lagadjust: nativefloat;
    lag: nativefloat;
  end;



  TSoundPacketOscillator= class(TOscillatorObject)
  private
    procedure SetTargetLag(Value: nativeint);
  protected
    FIFO: array[0..SOUND_PACKET_LOG_SIZE-1] of TSoundPacket;
    FIFOptr: nativeint;
    FIFOINptr: nativeint;
    RemoteSampleTime: int64;
    FirstPacketReceived: boolean;
    CurrentPacket: PSoundPacket;
    RemoteTimeStarted: int64;
    LocalTimeStarted: int64;
    bNEwWindow: boolean;
    procedure SlowCalcRunningLagAverage;
  public
    LAG_RECORDS_WHEN_UNLOCKED: cardinal;
    LAG_RECORDS_WHEN_LOCKED: cardinal;
    LAG_LOCK_WINDOW: cardinal;
    LAG_UNLOCK_WINDOW: cardinal;
    LAG_FINE_ADJUST: single;
    LAG_COARSE_ADJUST: single;


    LagRecords: array[0..MAX_LAG_RECORDS-1] of nativefloat;
    LagAvgREcords: array[0..MAX_LAG_RECORDS-1] of nativefloat;
    StreamReadTimes: array[0..99] of TStreamPerformanceMetric;
    StreamREadTimePtr: nativeint;
    servicelagidx: ni;
    servicelags: array[0..15] of nativeint;

    Correctedlag: nativefloat;
    LastLag: nativefloat;
    LAgPtr: nativeint;
    LocalTick: int64;
    LagAdjust: nativefloat;
    Lag: nativefloat;
    FTargetLag: nativeint;
    Resets: nativeint;
    Drops: nativeint;
    PacketsReceived: nativeint;
    Latency: nativeint;
    EffectiveLagRecordCount: nativeint;
    RunningAvgLocalTime: int64;
    RunningAvgRemoteTime: int64;
    ClockREferenceOffset: int64;
    LastSequenceNumberPlayed: int64;
    LastSequenceNumberReceived: int64;
    MEasuredSampleLag: int64;

    AvgCount: integer;
    LAgRunAvg: nativefloat;
    LAgAvgAvg: nativefloat;
    bLocked: boolean;
    CReationTime: cardinal;
    constructor create;override;
    destructor destroy;override;

    procedure Reset;


    procedure o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64); override;

    function FindPacket(iSampleTime: int64; bForFuture: boolean): PSoundPacket;
    procedure AddPacket(p: TSoundPacket);

    function LocalSampleTime(iSampleTime: int64): int64;
    function LocalTimeToRemoteTime(iSampletime: int64): int64;
    function RemotetimeToLocalTime(iSampletime: int64): int64;
    property TargetLag: nativeint read FTargetLag write SetTargetLag;
    procedure SourcesChanged;override;
    procedure CalcLagAvgAvg;
    procedure CleanBillofHealth(rLag: nativefloat);
    function AvgServiceLAg: nativefloat;

  end;






{$IFDEF MSWINDOWS}
procedure ApplyEqToStereoBuffer(buf: P16BitStereoSoundSample; iSTereoSampleCount: nativeint; Feq: PEQInfo);
{$ENDIF}

procedure SoundDebug(s: string);


function ring_subtract(iptr: nativeint; buffer_size: nativeint; iDec: nativeint = 1): integer;




{$IFDEF MSWINDOWS}
procedure SoundStretchVerify(sIn, sOut: string;
  rSourceTempo, rTargetTempo: nativefloat; iPitch: integer);
{$ENDIF}
function QMarkSynth(t: int64; freq: double; amp: double): double;
function Resonate(ms: TMemoryStream; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
function NoiseSynth(amp: nativefloat): nativefloat;


function LoadSoundIntoMemory(sBoogerFile: string): TMemoryStream;
procedure SaveSoundData(sBoogerFile: string; pdata: pointer; length: int64;
  cue1, cue2, loop1, loop2: cardinal);
procedure FreeSoundFromMemory(ms: TMemoryStream);
procedure WriteDataLengthToStream(fs: TStream; iLength: cardinal); overload;
procedure WriteDataLengthToStream(fs: TStream); overload;
procedure WriteBlankBoogerHeader(fs: TStream);
procedure TruncateAtSilenceInBoogerFile(sFile: string);
{$IFDEF MSWINDOWS}
function LoadWaveHeader(sFile: string): TWaveInfoAfterFMT;
function WaveGetNumberOfSamples(sFile: string): integer;
procedure WaveToOGG(sInFile: string; sOutFile: string; rQuality: nativefloat = 8.0;
  bDownMix: boolean = false; iResample: integer = 44100);
procedure OGGToWave(sInFile: string; sOutFile: string);
procedure TrimWave(sFile: string; sOutFile: string;
  iNewLengthInSamples: integer);

function HasAwkwardSilence(sBoogFile: string; rSilenceLength: single = 0.5;
  rMargin: single = 0.2): boolean;

{$ENDIF}

function GetNormalizeLevel(sBoogFile: string): nativefloat;
procedure BuildRunningAverageFile(sBoogFile: string; sAvgFile: string);



procedure GenerateSinWave(iSampleRate: integer; rFrequency: nativefloat;
  out iLength: integer; var a: array of nativefloat);


var
  gtest: TStereoSoundSample;





implementation

uses
{$IFDEF MSWINDOWS}
  fftw_interface,
{$ENDIF MSWINDOWS}
  beeper;


procedure SoundDebug(s: string);
begin
  DEbug.Log(s);
//  outputDebugString(pchar(s));
end;









function Resonate(ms: TMemoryStream; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
var
  iPos: int64;
  rSAmple: nativefloat;
  iSamplesBackward: integer;
  iSample: smallint;

begin
  iSamplesBackward := round(samplerate / freq);

  iPos := ms.Position;
  if iPos - (iSamplesBackward * 2) >= 0 then begin
    ms.Seek(ms.Position - (iSamplesBackward * 2), 0);
    Stream_GuaranteeRead(ms, Pbyte(@iSample), 2);

    rSAmple := iSample / 32767;

    result := 0 - rSAmple * amp;
    ms.Seek(iPos, 0);
  end
  else
    result := 0.0;

end;

function TriangleSynth(t: int64; freq: nativefloat; amp: nativefloat;
  samplerate: integer = 44100): nativefloat;
var
  rWaveLength: integer;
  q1, q2, q3, q4: integer;
  offset: integer;
begin
  rWaveLength := round(samplerate / freq);

  offset := t mod rWaveLength;
  q1 := 0;
  q2 := q1 + (rWaveLength div 4);
  q3 := q1 + (rWaveLength div 2);
  q4 := q3 + (rWaveLength div 4);

  if offset > q4 then begin
    result := -1 + (offset - q4) / (rWaveLength / 4);
  end
  else if offset > q2 then begin
    result := 1 - (offset - q2) / (rWaveLength / 4);
  end
  else
    result := 0 + (offset) / (rWaveLength / 4);

  result := result * amp;

end;

function NoiseSynth(amp: nativefloat): nativefloat;
begin
  result := amp * (((random(1000000) / 500000)) - 1);

end;

{$IFDEF MSWINDOWS}
{$ENDIF}




function LoadSoundIntoMemory(sBoogerFile: string): TMemoryStream;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(sBoogerFile);
  except
    ms.free;
    ms := nil;
  end;
  result := ms;
end;

procedure FreeSoundFromMemory(ms: TMemoryStream);
begin
  ms.free;
end;

procedure SaveSoundData(sBoogerFile: string; pdata: pointer; length: int64;
  cue1, cue2, loop1, loop2: cardinal);
var
  fs: TlocalMFS;
  t: integer;
  a: PByte;
  iToWrite: integer;
begin
  length := length;
  fs := TlocalMFS.Create(sBoogerFile, fmCreate);
  try
    Stream_guaranteeWrite(fs, Pbyte(@length), 4);
    Stream_guaranteeWrite(fs, Pbyte(@cue1), 4);
    Stream_guaranteeWrite(fs, Pbyte(@cue2), 4);
    Stream_guaranteeWrite(fs, Pbyte(@loop1), 4);
    Stream_guaranteeWrite(fs, Pbyte(@loop2), 4);
    a := pdata;
    t := 0;
    while t < length do begin
      iToWrite := 4000;
      if length - t < 4000 then
        iToWrite := length - t;
      inc(t, Stream_guaranteeWrite(fs, Pbyte(@a[t]), iToWrite));
    end;

  finally
    fs.free;
  end;

end;


procedure WriteDataLengthToStream(fs: TStream);
begin
  WriteDataLengthToStream(fs, cardinal(fs.Size) - 20);

end;

procedure WriteBlankBoogerHeader(fs: TStream);
var
  zero: cardinal;
begin
  zero := 0;

  Stream_guaranteeWrite(fs, Pbyte(@zero), 4);
  Stream_guaranteeWrite(fs, Pbyte(@zero), 4);
  Stream_guaranteeWrite(fs, Pbyte(@zero), 4);
  Stream_guaranteeWrite(fs, Pbyte(@zero), 4);
  Stream_guaranteeWrite(fs, Pbyte(@zero), 4);
end;

procedure WriteDataLengthToStream(fs: TStream; iLength: cardinal);
var
  i: int64;
begin
  i := fs.Position;
  fs.Seek(0, soBeginning);
  Stream_guaranteeWrite(fs, Pbyte(@iLength), 4);
  fs.Seek(i, soBeginning);
end;

{ TSoundPlayerThread }


procedure TruncateAtSilenceInBoogerFile(sFile: string);
var
  ms: TMemoryStream;
  t: integer;
  amp: smallint;
  cnt: integer;
  val: WORD;
  p: int64;
  iSample: integer;
  l: cardinal;
begin
  exit;
  ms := LoadSoundIntoMemory(sFile);
  if ms <> nil then
    try
      amp := 0;
      cnt := 0;
      iSample := 0;
      ms.Seek(SOUND_IDX_LENGTH, soBeginning);
      Stream_GuaranteeRead(ms, Pbyte(@l), 4);

      ms.Seek(SOUND_IDX_DATA_V3, soBeginning);

      for t := 0 to l - 1 do begin

        Stream_GuaranteeRead(ms, Pbyte(@amp), 2);
        if amp < 255 then
          inc(cnt)
        else
          cnt := 0;

        if (cnt >= 11000) then begin
          // beeper.BeepArray([888,999],[80,80]);

          // rewrite length header
          val := iSample;
          p := ms.Position;
          ms.Seek(SOUND_IDX_CUE2, soBeginning);
          Stream_guaranteeWrite(ms, Pbyte(@val), 4);
          ms.Seek(p, 0);
          break;
        end;

        inc(iSample);

      end;

      // ms2.seek(0,0);
      ms.SaveToFile(sFile);

    finally
      ms.free;
    end;

end;

procedure TruncateAtSilenceInBoogerFileOld(sFile: string);
var
  ms: TMemoryStream;
  ms2: TMemoryStream;
  t: integer;
  amp: smallint;
  cnt: integer;
  val: WORD;
  p: int64;
  iSample: integer;
begin
  ms := LoadSoundIntoMemory(sFile);
  if ms <> nil then
    try
      ms2 := TMemoryStream.Create;
      try
        ms2.Seek(0, soBeginning);
        amp := 0;
        for t := 0 to 4 do begin
          Stream_GuaranteeRead(ms, Pbyte(@val), 4);
          Stream_guaranteeWrite(ms2, Pbyte(@val), 4);
        end;

        cnt := 0;
        iSample := 0;
        while (ms.Position < ms.Size) and (Stream_GuaranteeRead(ms, Pbyte(@amp), 2) > 0) do begin
          if amp < 1000 then
            inc(cnt)
          else
            cnt := 0;

          if (cnt >= 11000) then begin
            // beeper.BeepArray([888,999],[80,80]);

            // rewrite length header
            val := iSample;
            p := ms2.Position;
            ms2.Seek(SOUND_IDX_LENGTH, soBeginning);
            Stream_guaranteeWrite(ms2, Pbyte(@val), 4);
            ms2.Seek(p, 0);
            break;
          end;

          Stream_guaranteeWrite(ms2, Pbyte(@amp), 2);
          inc(iSample);

        end;

        // ms2.seek(0,0);
        ms2.SaveToFile(sFile);
      finally
        ms2.free;
      end;

    finally
      ms.free;
    end;

end;

{ Tcmd_PlaySoundFromMemory }






{$IFDEF MSWINDOWS}
function LoadWaveHeader(sFile: string): TWaveInfoAfterFMT;
var
  msIn: TlocalMFS;
  msOut: TlocalMFS;
  c1, c2, c3, c4: ansichar;
  iLen, iLen2: integer;
  i: int64;
  t: integer;
  b: boolean;
  thisAmp, lastAmp: smallint;
  avgAmp: smallint;
  dataStart: int64;
  bitrate: WORD;
  shit: int64;
  sourcepos: int64;
  rsourcepos: nativefloat;
  amp1: smallint;
  amp2: smallint;
  chans: smallint;
  BytesperSample: smallint;
  dw: DWORD;
const
  target_bitrate = 44100;

begin
  fillmem(pointer(@result), sizeof(result), 0);
  msIn := TlocalMFS.Create(sFile, fmOpenRead);
  try
    // find beginning of fmt  section
    b := false;
    while not b do begin
      Stream_GuaranteeRead(msIn, Pbyte(@c1), 1);
      if c1 = 'f' then begin
        Stream_GuaranteeRead(msIn, Pbyte(@c2), 1);
        if not(c2 = 'm') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c3), 1);
        if not(c3 = 't') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c4), 1);
        if not(c4 = ' ') then
          continue;
        b := true;
      end;
    end;

    Stream_GuaranteeRead(msIn, Pbyte(@dw), 4);
    if (dw <> sizeof(result)) then begin
      raise Exception.Create('Unrecognized wave format, fmt size mismatch');
    end;
    Stream_GuaranteeRead(msIn, Pbyte(@result), sizeof(result));
// msIn.Read(result, sizeof(result));
  finally
    msIn.free;
  end;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
function WaveGetNumberOfSamples(sFile: string): integer;
var
  msIn: TlocalMFS;
  msOut: TlocalMFS;
  c1, c2, c3, c4: ansichar;
  iLen, iLen2: integer;
  i: int64;
  t: integer;
  b: boolean;
  thisAmp, lastAmp: smallint;
  avgAmp: smallint;
  dataStart: int64;
  bitrate: WORD;
  shit: int64;
  sourcepos: int64;
  rsourcepos: nativefloat;
  amp1: smallint;
  amp2: smallint;
  chans: smallint;
  BytesperSample: smallint;
const
  target_bitrate = 44100;
begin
  result := 0;
  msIn := TlocalMFS.Create(sFile, fmOpenRead);
  try
    // find beginning of fmt  section
    b := false;
    while not b do begin
      Stream_GuaranteeRead(msIn, Pbyte(@c1), 1);
      if c1 = 'f' then begin
        Stream_GuaranteeRead(msIn, Pbyte(@c2), 1);
        if not(c2 = 'm') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c3), 1);
        if not(c3 = 't') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c4), 1);
        if not(c4 = ' ') then
          continue;
        b := true;
      end;
    end;

    // size
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 4);
    // compression PCM
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 2);
    // 1 channel
    Stream_GuaranteeRead(msIn, Pbyte(@chans), 2);
    // sample rate
    Stream_GuaranteeRead(msIn, Pbyte(@bitrate), 2);
    // bytes per second
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 4);
    // block align
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 2);
    // bits per sample
    Stream_GuaranteeRead(msIn, Pbyte(@BytesperSample), 2);

// find beginning of data section
    b := false;
    while not b do begin
      Stream_GuaranteeRead(msIn, Pbyte(@c1), 1);
      if c1 = 'd' then begin
        Stream_GuaranteeRead(msIn, Pbyte(@c2), 1);
        if not(c2 = 'a') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c3), 1);
        if not(c3 = 't') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c4), 1);
        if not(c4 = 'a') then
          continue;
        b := true;
      end;
    end;

    // read length
    Stream_GuaranteeRead(msIn, Pbyte(@iLen), 4);

    result := (iLen div BytesperSample);
  finally
    msIn.free;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure OGGToWave(sInFile: string; sOutFile: string);
var
  ss: string;
  ms: TMemoryStream;
  bExists: boolean;
begin
  exe.RunProgramAndWait(Dllpath + 'oggdec.exe',
    '"' + sInFile + '" --output "' + sOutFile + '"', '', true, false);
// WaveToBooger(ss,ss+'.boog');
// playbooger(sfile+'.boog');

end;

procedure WaveToOGG(sInFile: string; sOutFile: string; rQuality: nativefloat = 8.0;
  bDownMix: boolean = false; iResample: integer = 44100);
var
  ss: string;
  ms: TMemoryStream;
  bExists: boolean;
  sDownMix: string;
  sQuality: string;
  sResample: string;
begin
  sDownMix := '';
  if bDownMix then
    sDownMix := ' --downmix';

  sQuality := ' --quality ' + floatprecision(rQuality, 2);
  sResample := ' --resample ' + inttostr(iResample);

  exe.RunProgramAndWait(Dllpath + 'oggenc.exe',
    '"' + sInFile + '" --output "' + sOutFile + '"' + sDownMix + sQuality +
      sResample, '', true, false);
// WaveToBooger(ss,ss+'.boog');
// playbooger(sfile+'.boog');

end;
{$ENDIF MSWINDOWS}

{ Tcmd_PlaySoundFromDisk }



{$IFDEF MSWINDOWS}
procedure TrimWave(sFile: string; sOutFile: string;
  iNewLengthInSamples: integer);
var
  msIn: TlocalMFS;
  msOut: TlocalMFS;
  c1, c2, c3, c4: ansichar;
  iLen, iLen2: integer;
  i: int64;
  t: integer;
  b: boolean;
  thisAmp, lastAmp: smallint;
  avgAmp: smallint;
  dataStart: int64;
  bitrate: WORD;
  shit: int64;
  sourcepos: int64;
  rsourcepos: nativefloat;
  amp1: smallint;
  amp2: smallint;
  chans: smallint;
  BytesperSample: smallint;
  iNewlen: integer;
  iJustRead: integer;
  a: array [0 .. 128000] of byte;
  iToRead: integer;
const
  target_bitrate = 44100;
begin
  amp1 := 0;
  msIn := TlocalMFS.Create(sFile, fmOpenRead);
  try
    // find beginning of fmt  section
    b := false;
    while not b do begin
      Stream_GuaranteeRead(msIn, Pbyte(@c1), 1);
      if c1 = 'f' then begin
        Stream_GuaranteeRead(msIn, Pbyte(@c2), 1);
        if not(c2 = 'm') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c3), 1);
        if not(c3 = 't') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c4), 1);
        if not(c4 = ' ') then
          continue;
        b := true;
      end;
    end;

    // size
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 4);
    // compression PCM
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 2);
    // 2 channels
    Stream_GuaranteeRead(msIn, Pbyte(@chans), 2);
    // sample rate
    Stream_GuaranteeRead(msIn, Pbyte(@bitrate), 4);
    // bytes per second
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 4);
    // block align
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 2);
    // bits per sample
    Stream_GuaranteeRead(msIn, Pbyte(@BytesperSample), 2);
    BytesperSample := BytesperSample div 8;

// find beginning of data section
    b := false;
    while not b do begin
      Stream_GuaranteeRead(msIn, Pbyte(@c1), 1);
      if c1 = 'd' then begin
        Stream_GuaranteeRead(msIn, Pbyte(@c2), 1);
        if not(c2 = 'a') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c3), 1);
        if not(c3 = 't') then
          continue;
        Stream_GuaranteeRead(msIn, Pbyte(@c4), 1);
        if not(c4 = 'a') then
          continue;
        b := true;
      end;
    end;

    // read length

// if fileexists(soutfile) then deletefile(sOutfile);
    msOut := TlocalMFS.Create(sOutFile, fmCreate);
    try
      i := msIn.Position;
      msIn.Seek(0, 0);
      // copy the header from the old file to the new file
      for t := 0 to i - 1 do begin
        Stream_GuaranteeRead(msIn, Pbyte(@c1), 1);
        Stream_guaranteeWrite(msOut, Pbyte(@c1), 1);
      end;

      // read old length
      Stream_GuaranteeRead(msIn, Pbyte(@iLen), 4);
      // write a new length
      iNewlen := iNewLengthInSamples * BytesperSample * chans;
      Stream_guaranteeWrite(msOut, Pbyte(@iNewlen), 4);

      dataStart := msIn.Position;

      // write the data
      t := 0;
      iToRead := (LesserOf(iNewLengthInSamples * chans * BytesperSample,
          iLen * chans * BytesperSample));
      while t < iToRead do begin
        iJustRead := Stream_GuaranteeRead(msIn, @a[0],
          LesserOf(iToRead - t, 128000));
        if iJustRead = 0 then begin
          break;
        end;
        inc(t, iJustRead);
        Stream_guaranteeWrite(msOut, @a[0], iJustRead);
      end;
// for t:= 0 to (lesserof(iNewLengthInSamples, iLen)*bytespersample)-1 do begin
// msIn.read(amp1, 2);
// msOut.write(amp1, 2);
// end;

      // if growing
      if iNewLengthInSamples * chans * BytesperSample > iLen * chans *
        BytesperSample then begin
        for t := 0 to ((iNewLengthInSamples * chans * BytesperSample) - iLen)
          div 2 do begin
          Stream_guaranteeWrite(msOut, Pbyte(@amp1), 2);
        end;
      end;
    finally
      msOut.free;
    end;
  finally
    msIn.free;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure SoundStretchVerify(sIn, sOut: string;
  rSourceTempo, rTargetTempo: nativefloat; iPitch: integer);
var
  dir: nativefloat;
  rTempoChange: nativefloat;
  iTargetSamples: integer;
  iOriginalSamples: integer;
  iCurrentSamples: integer;
  iLastSAmples: integer;
  rBeats: nativefloat;
  bfirstTime: boolean;
  t: integer;
begin
  iCurrentSamples := 0;
// for t:= 1 to 10 do begin
// windows.beep(1000,100);
// sleep(1000);
// SayNatural('Hello');
// end;
// dir := 100;
  dir := 1;
  rTempoChange := ((rTargetTempo - rSourceTempo) / rSourceTempo) * 100;
  if rTargetTempo < rSourceTempo then
    dir := -1
  else
    dir := 1;
  // get the original sample count
  iOriginalSamples := WaveGetNumberOfSamples(sIn);
  // figure out how many beats there are
  rBeats := iOriginalSamples / ((44100 * 60) / rSourceTempo);
  // figure out how many samples there should be given the beat count and target tempo
  iTargetSamples := trunc(rBeats * ((44100 * 60 / rTargetTempo)));

  bfirstTime := true;

{ repeat
    if bfirstTime then begin
      bfirstTime := false;
    end else begin

    end; }
    // stretch
  if fileexists(sOut) then
    deletefile(sOut);
    // windows.beep(1000,10);
  RunProgramAndWait(Dllpath + 'soundstretch.exe',
    '"' + sIn + '" "' + sOut + '" -tempo=' + floattostr(rTempoChange)
      + ' -pitch=' + inttostr(iPitch), '', true, false);
// SayNatural(floattostr(rTempoChange));
// windows.beep(100*trunc(abs(dir)),100);
    // count samples
  iLastSAmples := iCurrentSamples;
  iCurrentSamples := WaveGetNumberOfSamples(sOut);

{ if icurrentSamples = iTargetSamples then begin
      //SayNatural('Found it!');
      exit;
    end;

    if (iCurrentSamples = iLastSamples) and  (abs(dir) < 100) then begin
      CopyFile(pchar(sout), pchar(sOut+'.temp'),false);
      TrimWave(sOut+'.temp', sOut, iTargetSamples);
      exit;
    end;

    if dir < 0 then begin
      if iCurrentSamples > iTargetSamples then begin
        dir := (0-dir)/2;
      end;
      rTempoChange := rTempoChange + dir;

    end else begin
      if iCurrentSamples < iTargetSamples then begin
        dir := (0-dir)/2;
      end;
      rTempoChange := rTempoChange + dir;
    end;

  until false; }
  TrimWave(sOut + '.temp', sOut, iTargetSamples);

end;
{$ENDIF}

{ TAbstractSoundDevice }


procedure TAbstractSoundDevice.AddOscillator(o: ToscillatorObject);
begin
  Debug.Log(self, 'sample rate clobbered!');
{$IFDEF USE_PLAYBACK_INFO}
  o.dev := @playbackinfo;
  o.dev.init;
{$ENDIF}

  if (o.MasterStream <> ISoundOscillatorRenderer(self)) then begin
    o.MasterStream := self;
  end
  else
    self.AddOscillator(o.oo, o);

end;

procedure TAbstractSoundDevice.AudioLoop;
begin
  //
end;

function TAbstractSoundDevice.AddOscillator(p: TSoundOscillatorHookSSE;
  o: TObject): TSoundOscillator;
var
  osc: TSoundOscillator;
begin
  result := nil;
  osc := nil;
  Lock;
  try
    osc := TSoundOscillator.Create;
{$IFDEF USE_PLAYBACK_INFO}
    osc.dev := @playbackinfo;
{$ENDIF}
    osc.HookSSE := p;
    osc.Obj := o;
    osc.thr := self;
    FOscillators.add(osc);
// fobjects.add(o);
    result := osc;
  finally
    Unlock;
  end;
end;

procedure TAbstractSoundDevice.BeforeDestruction;
begin
  MakeSureQueueIsStopped;
  CleanupWave;
  inherited;

//  if OscillatorCount > 0 then
//    raise Exception.Create('cannot destroy a sound stream thread until all oscillators are removed.');
end;

procedure TAbstractSoundDevice.BeginSampling(iSampleTime: int64);
var
  t: ni;
  junk: TStereoSoundSample;
begin
  for t:= 0 to self.OscillatorCount-1 do begin
    oscillators[t].Fill(iSAmpleTime, ToscMessageType.mtBeginWindow, junk, iSampleTime);
  end;
end;

var
  iLAstDebugSampleTime: int64;
procedure TAbstractSoundDevice.BuildUDPPacket(iSampleTime: int64; rLeft, rRight: nativefloat);
var
  b: TidBytes;
  sip: string;
  iPort: cardinal;
  ra: TRemoteAudio;
  t,u: integer;
  qi: TQueuedUDPPacket;
begin
  if udpc=nil then
    exit;

  if FoutPacketDataPtr = 0 then begin
    FOutPacket.RemoteSampleTime := iSampleTime;
    FOutPacket.LocalSampleTime := iSampleTime;
  end;

  FOutPacket.LagRecordsWhenUnlocked := UDP_LagRecordsWhenUnlocked;
  FOutPacket.LagRecordsWhenlocked   := UDP_LagRecordsWhenLocked;
  FOutPacket.FineAdjust             := UDP_FineAdjust;
  FOutPacket.CoarseAdjust           := UDP_CoarseAdjust;
  FOutPacket.SetBufferSizeTo        := UDP_BufferSize;
  FOutPacket.TargetLag              := UDP_TargetLag;
  FOutPacket.LockMargin             := UDP_LockMArgin;
  FOutPacket.UnLockMargin           := UDP_UnLockMArgin;



  FOutPacket.Data[FOutPacketDataPtr].ch[0] := round(rLeft * 32767);
  FOutPacket.Data[FOutPacketDataPtr].ch[1] := round(rRight * 32767);
  inc(FOutPacketDataPtr);

  MakeSureQueueIsSTarted;

  if FOutPacketDataPtr = SOUND_PACKET_DATA_SIZE then begin
    setlength(b, sizeof(FOutPacket));
    Foutpacket.SetBufferSizeTo:= self.Buffersize;
    movemem32(@b[0], @FOutpacket, sizeof(FOutPacket));
    try
      Lock;
      try
        for t:= 0 to remoteaudio.Count-1 do begin
          ra := self.remoteaudio[t];
          //send backlogs

//          for u := 0 to UDP_BACKLOG_SIZE-1 do begin
//            if FBacklogFill = UDP_BACKLOG_SIZE then begin
//              movemem32(@b[0], @FUDPBacklog[u], sizeof(FOutPacket));
//              udpc.SendBuffer(ra.IP, ra.Port, b);
//            end;
//          end;


          FOutpacket.MeasuredSampleLag := ra.Lag;
//          FOutPacket.PingReferenceTime := GetTicker;
          FOutPacket.PingReferenceTime := 0;//GetTicker;
          movemem32(@b[0], @FOutpacket, sizeof(FOutPacket));


          qi := TQueuedUDPPacket.create;
          qi.ip := ra.ip;
          qi.port := ra.port;
          qi.data := b;
          qi.client := udpc;
          udpqueue.AddItem(qi);
//          udpc.SendBuffer(ra.Ip, ra.port, b);
          //here;
        end;

        //add to backlog
        FUDPBAcklogPtr := (FUDPBAcklogPtr + 1) mod UDP_BACKLOG_SIZE;
        if FBacklogFill < UDP_BACKLOG_SIZE then
          inc(FBacklogFill);
        movemem32(@FUDPBackLog[FUDPBacklogptr], @FOutPacket, sizeof(FOutpacket));

        if ((iSampleTime - iLastDebugSampleTime) > 5000)then begin
          //Debug('Packet:'+inttostr(FOutpacket.LocalSampleTime));
          iLastDebugSampleTime := iSampleTime;
        end;

//          udpc.SendBuffer(ra.IP, ra.Port, b);
//          udpc.SendBuffer(ra.IP, ra.Port, b);


      finally
        Unlock;
      end;
    finally
    end;
//    udpc.SendBuf(FOutPacket, sizeof(Foutpacket));
    FOutPacketDataPtr := 0;
  end;



end;


constructor TAbstractSoundDevice.Create(Owner: TObject; Manager: TThreadManager; pool: TThreadpoolBase);
begin
  inherited;
  debug.log(self, 'Creating sound device');
  FDeviceList := TStringlist.create;
  FOscillators := TList<TSoundOscillator>.Create;

  ControlRoomVolume := 1.0;
  udpc := nil;
  remoteaudio := TRemoteAudioList.create;
  BufferSize := 4096;
  UDP_TargetLag := DEFAULT_TARGET_UDP_LAG;
{$IFDEF USE_PLAYBACK_INFO}
  playbackinfo.Init;
{$ENDIF}
end;

procedure TAbstractSoundDevice.CreateUDPConnection;
var
  bind: TidSocketHandle;
begin
  if not remote then begin
    udpc.Free;
    udpc := nil;
    exit;
  end;

  //udpc.Free;
  //udpc := nil;

  if udpc = nil then begin

    udpc := TidUDPServer.Create(nil);
    bind := udpc.Bindings.Add;
    bind.SetBinding('0.0.0.0', 798);
    bind.SetNagleOpt(false);
    udpc.ThreadedEvent := true;

    udpc.OnUDPRead:= UDPOnReceive;


    udpc.Active := true;


//    udpc.LocalPort := '798';
//    udpc.RemoteHost := remotehost;
//    udpc.RemotePort := remoteport;
//    udpc.
//    udpc.OnReceive := self.UDPOnReceive;
//    udpc.BlockMode := bmBlocking;
//    udpc.Active := true;

  end;

end;

destructor TAbstractSoundDevice.Destroy;
begin
  udpc.Free;
  AvRevertMmThreadCharacteristics_Safe(FTaskHandle);

  FOscillators.free;
  remoteaudio.Free;
  FDeviceList.free;
  FDeviceList := nil;
  inherited;
end;

procedure TAbstractSoundDevice.DeviceChanged;
begin
  //no implementation required
end;

function ring_subtract(iptr: nativeint; buffer_size: nativeint; iDec: nativeint = 1): integer;
begin
  dec(iptr, iDec);
  if iptr < 0 then
    iptr := iptr + ((buffer_size));
  result := iptr;
end;

procedure TAbstractSoundDevice.DoExecute;
begin
  DoExecuteSSE;

end;


procedure TAbstractSoundDevice.SetupWave;
begin
  //

end;

procedure TAbstractSoundDevice.CleanupWave;
begin
  //
end;
procedure TAbstractSoundDevice.DoExecuteSSE;
begin
  inherited;

  iters := 0;
  Status := 'Doing Stuff';


  FPendingDeviceChange := true;
  try

      // ---


      FTaskIndex := 0;
      FTaskHandle := AvSetMmThreadCharacteristics_Safe('Pro Audio', FTaskIndex);

      while not StopREquested do begin
        if FPendingDeviceChange then begin
          CleanupWave;
          SetupWave;
          FPendingDeviceChange := false;
        end;
        AudioLoop;
      end;


  finally
    CleanupWave;
  end;

// waveOutWrite(hWave,

end;


procedure TAbstractSoundDevice.EndSampling;
var
  t: ni;
  junk: TStereoSoundSample;
begin
  for t:= 0 to self.OscillatorCount-1 do begin
    oscillators[t].Fill(iSAmpleTime, ToscMessageType.mtEndWindow, junk, iSampleTime);
  end;
end;

var
  gflag: boolean;


procedure TAbstractSoundDevice.DoExecuteDSound;
begin

  raise Exception.create('unimplemented');
end;


function TAbstractSoundDevice.GetOscillatorCount: integer;
begin
  Lock;
  try
    result := FOscillators.count;
  finally
    Unlock;
  end;
end;

function TAbstractSoundDevice.GetPlayPosition: int64;
begin
  result := (FPlayPosition - FPlayOrigin)
end;

function TAbstractSoundDevice.GetPosition_int64: int64;
begin
  result := (fillptr + (iters * FBufferSize)) div 2;
end;

function TAbstractSoundDevice.GEtSampleRate: nativeint;
begin
{$IFDEF USE_PLAYBACK_INFO}
  result := playbackinfo.samplerate;
{$ELSE}
  result := FSampleRAte;
{$ENDIF}
end;

procedure TAbstractSoundDevice.MakeSureQueueIsSTarted;
begin
  if udpqueue = nil then begin
    udpqueue := TPM.NeedThread<TSimpleQueue>(nil);
    udpqueue.betterPriority := bpTimeCritical;
    udpqueue.Start;
  end;
end;

procedure TAbstractSoundDevice.MakeSureQueueIsStopped;
begin
  if assigned(udpqueue) then begin
    udpqueue.BeginStop;
    TPM.NoNeedThread(udpqueue);
    udpqueue := nil;
  end;
end;

function TAbstractSoundDevice.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if self.GetInterface(iid, obj) then
    result := S_OK
  else
    result := E_NOINTERFACE;
end;

procedure TAbstractSoundDevice.RemoveOscillator(o: ToscillatorObject);
begin

  RemoveOscillator(o.oo, o);

  gflag := true;
end;

function TAbstractSoundDevice.GetBufferSize: nativeint;
begin
  result := FBufferSize;
end;

function TAbstractSoundDevice.GetDevice(idx: ni): string;
begin
  result := FDeviceList[idx];
end;

function TAbstractSoundDevice.GetDeviceCount: ni;
begin
  result := FDeviceList.count;
end;

function TAbstractSoundDevice.GetDeviceListH: IHolder<TStringlist>;
begin
  result := THolder<TStringlist>.create;
  result.o := TStringlist.create;
  result.o.Assign(FDeviceList);
end;

procedure TAbstractSoundDevice.RefreshDevices;
begin
  FDeviceList.clear;
  FDeviceList.Add('WAVE_MAPPER');

end;

function TAbstractSoundDevice.GetEngineSTartTime: int64;
begin
  result := FEngineSTartTime;
end;

function TAbstractSoundDevice.GetMuteAudio: boolean;
begin
  Lock;
  try
    result := fMuteAudio;
  finally
    Unlock;
  end;
end;

function TAbstractSoundDevice.GEtNyquistFrequency: nativeint;
begin
  result := SampleRate div 2;
end;

function TAbstractSoundDevice.GetOscillator(idx: integer): TSoundOscillator;
begin
  result := FOscillators[idx];


end;

procedure TAbstractSoundDevice.RemoveOscillator(p: TSoundOscillatorHookSSE; o: TObject);
var
  t: integer;
  pp: TSoundOscillatorHook;
begin
  Lock;
  try
    for t := FOscillators.count - 1 downto 0 do begin
      pp := FOscillators[t].HookSSE;
      if (pointer(@pp) = pointer(@p)) and (FOscillators[t].Obj = o) then begin
        FOscillators[t].free;
        FOscillators.delete(t);
      end;
    end;
  finally
    Unlock;
  end;
end;

function TAbstractSoundDevice.RenderSample(iSample: int64;
  out ss: TStereoSoundSample): boolean;
var
  rCurrent, rTemp: TStereoSoundSample;
  t: ni;
  junk: TStereoSoundSample;
begin
  result := true;
  for t:= 0 to self.OscillatorCount-1 do begin
    oscillators[t].Fill(iSAmple, ToscMessageType.mtGetSample, ss, iSample);
  end;

  ss := ss * ControlRoomVolume;
end;

procedure TAbstractSoundDevice.ResetPlayOrigin;
begin
  FPlayOrigin := FPlayPosition;

  fillptr := 0;
  iters := 0;

end;

procedure TAbstractSoundDevice.ResolveDeviceName;
begin
  if FDeviceID < 0 then
    FDeviceNAme := 'No Device Selected'
  else
    FDeviceName := 'index '+FDeviceID.tostring;


end;

procedure TAbstractSoundDevice.SetBufferSize(const Value: nativeint);
begin
  Lock;
  try
    FBufferSize := Value;
  finally
    Unlock;
  end;
end;

procedure TAbstractSoundDevice.SetDeviceID(const Value: integer);
begin
  FDeviceID := Value;
end;

procedure TAbstractSoundDevice.SetDeviceName(const Value: string);
begin
  FDeviceName := Value;
  if FPendingDeviceChange and Active then begin
    Debug.Log('Another device change is still pending');
    sleep(2000);
  end;
  FPendingDeviceChange := true;


end;

procedure TAbstractSoundDevice.SetEngineStartTime(const Value: int64);
begin
  FEngineStartTime := value;
end;

procedure TAbstractSoundDevice.SetMuteAudio(const Value: boolean);
var
  t: integer;
begin
  Lock;
  try
    if Value then begin
      for t := low(FBuf) to high(FBuf) do begin
        FBuf[t].ch[0] := 0;
        FBuf[t].ch[1] := 0;
      end;

    end;

    fMuteAudio := Value;

  finally
    Unlock;
  end;

end;

procedure TAbstractSoundDevice.UDPOnReceive(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  p: PRFAPacket;
  s: string;
  i,u: nativeint;
  ra: TRemoteAudio;
  b: TidBytes;
begin
  if (AData[0] = 82{ord(ansichar('R'))}) //request
  and (AData[1] = 70{ord(ansichar('F'))}) //for
  and (AData[2] = 65{ord(ansichar('A'))}) //audio
  then begin
    p := PRFAPacket(@Adata[0]);
//    if p.pingreferencetime > 0 then
//      Debug('Ping: '+inttostr(gettimesince(p.pingreferencetime))+' ms.');

    Lock;
    try
      s := abinding.PeerIP +':'+inttostr(abinding.PeerPort);
//      Debug(s);
      i := remoteaudio.IndexOf(abinding.peerip, abinding.PeerPort);
      if i < 0 then begin
        ra := TRemoteAudio.Create;
        ra.IP := abinding.PeerIP;
        ra.Port := abinding.PeerPort;


        if copy(ra.IP, STRZ,3) = '224' then begin
          ra.MultiCastPeer := true;

        end;
        remoteaudio.Add(ra);
      end else begin
        ra := remoteaudio[i];
        ra.Lag := self.GetPosition_int64 - p.LastSequenceNumberProcessed;
        //Debug(abinding.peerip+' Lag: '+inttostr(ra.lag)+' samples ('+inttostr(ra.lag*1000 div 44100)+' ms.)');
      end;

      if p.MissingPosition <> 0 then begin
        SetLength(b, sizeof(FOutpacket));
        for u := 0 to UDP_BACKLOG_SIZE-1 do begin
          if FBacklogFill = UDP_BACKLOG_SIZE then begin
            if FUDPBacklog[u].RemoteSampleTime = p.MissingPOsition then begin
              movemem32(@b[0], @FUDPBacklog[u], sizeof(FOutPacket));
              udpc.SendBuffer(ra.IP, ra.Port, b);
            end;
          end;
        end;
      end;



    finally
      Unlock;

    end;

  end;




end;




{ Tcmd_PlaySoundFromMemory_MonoOrStereo44100 }




procedure GenerateSinWave(iSampleRate: integer; rFrequency: nativefloat;
  out iLength: integer; var a: array of nativefloat);
var
  t: integer;
begin
  iLength := round(iSampleRate / rFrequency);

  for t := 0 to iLength - 1 do begin
    a[t] := sIn((t / iLength) * 6.28);
  end;
end;



{$IFDEF MSWINDOWS}

{$ENDIF}

{ TOscillatorObject }


{ Tcmd_Mp3ToBooger }





{ TMetronomeOscillator }

constructor TMetronomeOscillator.Create;
begin
  inherited;
  FTempo := 96;
  FAmplitude := 1;
end;


procedure TMetronomeOscillator.o(mt: ToscMessageType;
  out ss: TStereoSoundSample; iSampletime: int64);
var
  iProgression: integer;
  iBeatSamples: nativefloat;
  rFalloff: nativefloat;
  rAmp: nativefloat;
begin
  if mt <> mtGetSample then
    exit;

  if FTempo = 0 then begin
    ss.init;
  end;

  iBeatSamples := (44100 * 60) / FTempo;
  iProgression := (iSampletime - MasterStream.EngineStartTime) mod trunc
    (iBeatSamples);
  rFalloff := (iProgression / iBeatSamples) * 8;

  if ComputerBeep then begin
    if iProgression <  FLastFalloff then begin
      {$IFDEF MSWINDOWS}
        winapi.windows.Beep(1000,100);
      {$ENDIF}
    end;
  end;

  FLastFalloff := iProgression;
  rAmp := FAmplitude - rFalloff;
  if rAmp < 0 then
    rAmp := 0;

  ss.Left := rAmp * sIn(iSampletime / 5);
  ss.Right := ss.Left;

  ss.Clip;



// rLeft := nativefloatRandom(rAmp * 2, 65536, false)-1.0;
// rRight := nativefloatRandom(rAmp * 2, 65536, false)-1.0;

end;

procedure BuildRunningAverageFile(sBoogFile: string; sAvgFile: string);
var
  ss: TSoundStream;
  sOut: TlocalMFS;
  l, r: single;
  iStart, iEnd: int64;
  rPeak: single;
  wvu: TWaveVUs;
  t: integer;
  iPos: int64;
const
// SMALL_WINDOW = 64;
// MEDIUM_WINDOW = 512;
// LONG_WINDOW = 1024;
  SMALL_WINDOW = 441;
  MEDIUM_WINDOW = 4410;
  LONG_WINDOW = 44100;

begin
  ss := TSoundStream.Create(sBoogFile, fmOpenRead + fmShareDenyNone);
  try
    sOut := TlocalMFS.Create(sAvgFile, fmCreate);
    try
      iStart := 0;
      iEnd := round(((ss.SampleCount - 1) / 44100) * 60);
      ss.Position := iStart;

      for t := iStart to iEnd do begin
        iPos := round((t / 60) * 44100);
        wvu.Short.Min := ss.GetSampleMin(iPos - SMALL_WINDOW,
          iPos + SMALL_WINDOW);
        wvu.Short.Max := ss.GetSamplePeak(iPos - SMALL_WINDOW,
          iPos + SMALL_WINDOW);
        wvu.Short.Avg := ss.GetSampleAvg(iPos - SMALL_WINDOW,
          iPos + SMALL_WINDOW);
        wvu.Medium.Min := ss.GetSampleMin(iPos - MEDIUM_WINDOW,
          iPos + MEDIUM_WINDOW);
        wvu.Medium.Max := ss.GetSamplePeak(iPos - MEDIUM_WINDOW,
          iPos + MEDIUM_WINDOW);
        wvu.Medium.Avg := ss.GetSampleAvg(iPos - MEDIUM_WINDOW,
          iPos + MEDIUM_WINDOW);
        wvu.Long.Min := ss.GetSampleMin(iPos - LONG_WINDOW, iPos + LONG_WINDOW);
        wvu.Long.Max := ss.GetSamplePeak(iPos - LONG_WINDOW,
          iPos + LONG_WINDOW);
        wvu.Long.Avg := ss.GetSampleAvg(iPos - LONG_WINDOW, iPos + LONG_WINDOW);
        Stream_guaranteeWrite(sOut, @wvu, sizeof(wvu));

      end;

    finally
      sOut.free;
    end;
  finally
    ss.free;
  end;
end;

function GetNormalizeLevel(sBoogFile: string): nativefloat;
var
  ss: TSoundStream;
  l, r: FloatSample;
  iStart, iEnd: int64;
  rPeak: FloatSample;
  rMargin: nativefloat;
  s: TStereoSoundSample;
begin
  rPeak := 0;
  result := 1.0;
  rMargin := 0.2;
  ss := TSoundStream.Create(sBoogFile, fmOpenRead + fmShareDenyNone);
  try
    iStart := round(ss.Size * rMargin);
    iEnd := ss.Size - iStart;
    ss.Position := iStart;
    rPeak := 0;
    while ss.Position < iEnd do begin
      if ss.GetNextSample(s) then begin
        if abs(s.left) > rPeak then
          rPeak := abs(s.left);
        if abs(s.right) > rPeak then
          rPeak := abs(s.right);
      end;
    end;
  finally
    if rPeak = 0 then
      result := 1
    else
      result := 1 / rPeak;
    ss.free;
  end;

end;

function HasAwkwardSilence(sBoogFile: string; rSilenceLength: single = 0.5;
  rMargin: single = 0.2): boolean;
var
  ss: TSoundStream;
  l, r, ll, rR: FloatSample;
  iStart, iEnd: int64;
  iSilenceCount: integer;
  s: TStereosoundSample;
begin
  result := false;
  ss := TSoundStream.Create(sBoogFile, fmOpenRead + fmShareDenyNone);
  try
    iStart := round(ss.Size * rMargin);
    iEnd := ss.Size - iStart;
    ss.Position := iStart;

    iSilenceCount := 0;
    ll := 0;
    rR := 0;
    while ss.Position < iEnd do begin
      if ss.GetNextSample(s) then begin
        l := s.Left;
        r := s.Right;
        if ((abs(l - ll) * 65536) < 255) and ((abs(r - rR) * 65536) < 255) then
          begin
          inc(iSilenceCount);
          if iSilenceCount > (rSilenceLength * 44100) then begin
            result := true;
            exit;
          end;
        end
        else begin
          iSilenceCount := 0;
        end;
        ll := l;
        rR := r;
      end;
    end;
  finally
    ss.free;
  end;

end;

{ Tcmd_WaveStretch }







{$IFDEF MSWINDOWS}
procedure ApplyEqToStereoBufferPart(buf: P16BitStereoSoundSample; iSTereoSampleCount: nativeint; Feq: PEQInfo);
var
  t,u,i,f: integer;
  r: real;
  la: system.PDouble; //array [0..8191] of double;
  lOUT: system.PDouble; //array [0..16383] of double;
  rplan, rplan2: PByte;
  m: integer;
  absval,cc,erg,correction: real;


  iWidth: extended;
  fMax: extended;
  rBucketTop: extended;
  rBucketBottom: extended;
  i1,i2: floatsample;
  iWindowLength: integer;
  Pin: PSmallint;
  Pout, pout2: system.PDouble;
const
  SPLIT = 1;
  sample_interleave = 0;
begin
  iWindowLength := iStereoSampleCount;//1024
  GetMem(la, iWindowLength*sizeof(single));
  GetMem(lout, (iWindowLength) * sizeof(extended));
  try


    rPlan := nil;

    correction := 44100 / iWindowlength;

    fMax := 0;
  //  la := fftw_malloc(iWindowLength*sizeof(single));
  //  lout := fftw_malloc(iWindowlength*sizeofsingle));
    try


      //rplan := fftw_plan_dft_r2r_1d(iWindowLength, @la[0], @lout[0], FFTW_R2HC, FFTW_FORWARD);
      LockFFT;
      try
        rplan := fftw_plan_dft_r2c_1d(iWindowLength, la, lout, FFTW_ESTIMATE);
      finally
        UnlockFFT;
      end;
      try
        // we have no imaginary data, so clear idata
        zeromemory(pbyte(lout), iWindowLength * 2);


        // fill rdata with actual data
        Pin := @buf.ch[0];
        POUt := la;
        inc(pin, sample_interleave);
        for u := 0 to iWindowLength-1 do begin
          //i1 := P16BitStereoSoundSample(PByte(buf)+(u*sizeof(T16BitStereoSoundSample))).ch[sample_interleave];
          i1 := pin^;
          pout^ := i1;
          inc(pin,2);//2 for stereo
          inc(pout);
        end;
        // make fft
        fftw_execute(rplan);
        //
        //	// post-process FFT data: make absolute values, and calculate
        //	//   real frequency of each power line in the spectrum

          m := 0;

          POUt := lout;


          for i := 1 to (iWindowLength-2) div SPLIT do begin
            //point pout2 to sample adjacent
            pout2 := pout;
            inc(pout2);


//            if IsNaN(PDouble(PByte(lout)+(i*(sizeof(double))))^) or ((PDouble(PByte(lout)+((i*2)*(sizeof(double))))^=0) and (PDouble(PByte(lout)+(((i*2)+1)*(sizeof(double))))^=0)) then
//              absval := 0
//            else
//              absval := sqrt(power(pout^,2)+power(pout2^,2));


            m := i div 2;
            cc := m * correction;

//            if cc < 500 then
//              Pout^ := Pout^ * 2.0;



            inc(pin);
            inc(pout,2);
          end;

          LockFFT;
          try
            rplan2 := fftw_plan_dft_c2r_1d(iWindowLength div 2, lout, la, FFTW_ESTIMATE+FFTW_PRESERVE_INPUT );
          finally
            UnlockFFT;
          end;
          fftw_execute(rplan2);

          pout := la;
          pin := @buf.ch[sample_interleave];

          for u := 0 to (iWindowLength div 2) -1 do begin
            i1 := pout^;
            if IsNAn(i1) then
              i1 := 0;
            pin^ := round(i1);
            inc(pin,2);//2 for stereo
            inc(pout);
          end;

          if assigned(rPlan2) then begin
            LockFFT;
            try
              fftw_destroy_plan(rplan2);
            finally
              UnlockFFT;
            end;
          end;


        except
        end;



    finally
  //    fftw_free(la);
  //    fftw_free(lout);
      if assigned(rPlan) then begin
        LockFFT;
        try
          fftw_destroy_plan(rplan);
        finally
          UnlockFFT;
        end;
      end;
    end;
  finally
    FreeMem(la);
    Freemem(lout);
  end;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
procedure ApplyEqToStereoBuffer(buf: P16BitStereoSoundSample; iSTereoSampleCount: nativeint; Feq: PEQInfo);
var
  t: nativeint;
  iToGo: nativeint;
begin
  t := 0;
  while t < iStereoSampleCount  do begin
    iToGo := LesserOf(1024, iStereoSampleCount-t);
    ApplyEqToStereoBufferPart(p16bitStereoSoundSample(pbyte(buf)+(t*4)),  iToGo, Feq);
    inc(t, iToGo);
  end;

end;
{$ENDIF}







{ TOscBufferInfo }



{ Tmodosc_Null }

procedure Tmodosc_Null.Buffer_Process_Sync(b: POscBufferInfo);
begin
  inherited;

end;

procedure Tmodosc_Null.o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64);
begin
  //
  inherited;
  source.o(mt, s, iSampleTime);//<<<---- VERY IMPORTANT DUHHHH



end;


{ TComplexSoundModifierOscillator }

procedure TComplexSoundModifierOscillator.Buffer_PreProcess_Sync(
  b: POscBufferInfo);
var
  t: integer;
begin
  inherited;
  if b = nil then exit;

  Buffer_FetchForward(b);
  if NoiseTest then begin
    for t:= 0 to (b.Length-1) do begin
      b.FFT_Spatial[0][t] := (random(2000)/1000)-1;
      b.FFT_Spatial[1][t] := (random(2000)/1000)-1;
    end;
  end;

end;

procedure TComplexSoundModifierOscillator.Buffer_Process_Sync(
  b: POscBufferInfo);
begin
  inherited;
  //


end;

procedure TComplexSoundModifierOscillator.o(mt: ToscMessageType; out s: TstereoSoundSample; iSampletime: int64);
var
  i: nativeint;
  iCount: integer;
  b: POscBufferInfo;
  bO: POscBufferInfo;
  iOff: nativeint;
  ii: nativeint;
  t: nativeint;
  ss,ss2: TStereoSoundSample;
  ssWeight: TStereoSoundSample;
  f: nativefloat;
  iCount2: nativeint;
begin
  iCount2 := 0;
//  OutputDebugString(pchar('Complex o '+self.classname+' '+inttostr(iSampleTime)));
  if mt in [mtBeginWindow, mtEndWindow] then
    exit;
  i := IndexOfBuffer(iSampleTime,0);//make sure we have a primary sample buffer

  for t:= 0 to OverSamples-1 do begin
    NeedBuffer(iSampletime, t);
  end;


  //go through all the buffers and if they have the sample we need... use it
  ss.Left := 0;
  ss.Right := 0;
  ssWeight.Left := 0;
  ssWeight.Right := 0;
  iCount := 0;
  //first simply count up the number of oversamples we have available
  for t:= 0 to MODIFIER_BUFFER_COUNT-1 do begin
    b := FBufferOrders[t];
    iOFF := iSampleTime-b.Start;
    if b.start < 0 then continue;
    if (iOFF < 0) then continue;
    if (iOFF >= BufferSize) then
      continue;
    inc(iCount);
    if iCount >= Oversamples then break;
  end;
  //use any oversamples we have
  for t:= 0 to MODIFIER_BUFFER_COUNT-1 do begin
    b := FBufferOrders[t];
    b.FinishProcessing;
    iOFF := iSampleTime-b.Start;
    if b.start < 0 then continue;
    if (iOFF < 0) then continue;
    if (iOFF >= BufferSize) then
      continue;
    //^^^^^^^^//  continue if sample not within buffer range

    //if the sample is useful --------------------------------------------------

    ss2.Left := b.FFT_Spatial[0][iOFF];
    ss2.Right := b.FFT_Spatial[1][iOFF];


    if iCount > 1 then
      f := GetOverSampleWeight(iOFF)
    else
      f := 1;

    //f := f * f;

    ss := ss + (ss2 * f);
//    ss := ss + ss2;;
    ssWeight.Left := ssWeight.Left + f;
    ssWeight.Right := ssWeight.Right + f;
//    ssWeight.Left := ssWeight.Left + 1;
//    ssWeight.Right := ssWeight.Right + 1;


    inc(iCount2);
    if iCount2 >= OverSamples then break;
  end;

  if ssWeight.Left = 0 then begin
    ss.Left := 0;
  end else begin
    ss.Left := ss.Left / ssWeight.Left;

  end;

  if ssWeight.Right = 0 then begin
    ss.Right := 0;
  end else begin
    ss.Right := ss.Right / ssWeight.Right;

  end;

  s := ss;

end;

{ Tmodosc_EQ }

procedure Tmodosc_EQ.Buffer_Process_Sync(b: POscBufferInfo);
var
  t: integer;
begin
  inherited;

  b.ToFrequencyFromSpatial;
  for t:= 0 to MAX_CHANNELS-1 do begin
    DoEQ(b, t);
  end;
  b.ToSpatialFromFrequency;

end;
procedure Tmodosc_EQ.DoEQ(b: POscBufferInfo; iChannel: integer);
var
//  lOUT: array [0..511] of fftw_complex;
//  la: array[0..DEFAULT_MODIFIER_BUFFER_SIZE-1] of fftw_float;
  lOUT: PAfftw_complex;
  la: PAfftw_float;
  t,u,i,f: integer;
  r: real;
//  la: system.PDouble; //array [0..8191] of double;

  m: integer;
  absval,cc,erg,correction: real;


  iWidth: extended;
  fMax: extended;
  rBucketTop: extended;
  rBucketBottom: extended;
  i1,i2: floatsample;
  iWindowLength: integer;
  Pin: PSmallint;
  Pout, pout2: system.PSingle;
  p1,p2: pointer;
  rrr,rr: nativefloat;
const
  SPLIT = 1;
  sample_interleave = 0;
begin
  inherited;
  iWindowLength := b.Length;

        //eq shit

        for t := 0 to (iWindowlength)-1 do begin
          rr := t/iWindowLength;
//          if abs(rr-CrossOver) <= EQ_GUTTER then begin
//            rrr := INterpolate(rr, Bass, Treble, CrossOver-rr, CrossOver+rr);
//            b.FFT_Frequency[iChannel][t].i := b.FFT_Frequency[iChannel][t].i * rrr;
//            b.FFT_Frequency[iChannel][t].r := b.FFT_Frequency[iChannel][t].r * rrr;
//          end else
          if rr < CrossOver then begin
            b.FFT_Frequency[iChannel][t].i := b.FFT_Frequency[iChannel][t].i * Bass;
            b.FFT_Frequency[iChannel][t].r := b.FFT_Frequency[iChannel][t].r * Bass;
          end else begin
            b.FFT_Frequency[iChannel][t].i := b.FFT_Frequency[iChannel][t].i * Treble;
            b.FFT_Frequency[iChannel][t].r := b.FFT_Frequency[iChannel][t].r * Treble;
          end;

        end;

end;

procedure Tmodosc_EQ.Init;
begin
  inherited;
  Bass := 1;
  Treble := 1;
  CrossOver := 0.5;
end;

procedure Tmodosc_EQ.o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64);
begin
  inherited;

end;


{ Tmodosc_SpectrumShift }

procedure Tmodosc_SpectrumShift.Buffer_Process_Sync(b: POscBufferInfo);
var
  t: nativeint;
begin
  inherited;
  b.ToFrequencyFromSpatial;
  for t:= 0 to MAX_CHANNELS-1 do begin
    Doit(b, t);
  end;
  b.ToSpatialFromFrequency;
end;

procedure Tmodosc_SpectrumShift.DOit(b: POscBufferInfo; iChannel: integer);
var
  iWindowLength: nativeint;
  t: nativeint;
  rr: nativefloat;
  rDivisor: nativefloat;
  tempArray: PAfftw_complex;
  srcidx,lastsrcidx: nativeint;
  iSize: nativeint;
begin
//  exit;
  iWindowLength := b.Length;

      iSize := sizeof(fftw_complex)*iWindowLength;
      GetMem(tempArray, iSize);     //todo 1 - GEtMEm has the potential to block and probably shouldn't be done in an audio thread

      if ShiftOctaves = 0 then exit;

      if ShiftOctaves > 0 then begin
        rDivisor := power(2, ShiftOctaves);

        FillMem(@tempArray[0], iSize, 0);

        lastsrcidx := iWindowLength;
        for t := (iWindowlength)-1 downto 0 do begin

          rr := t/iWindowLength;
          srcidx := round(t / rDivisor);

          if lastsrcidx = srcidx then
            continue;

          if srcidx >= iWindowLength then
            continue;
          tempArray[t].i := tempArray[t].i + b.FFT_Frequency[iChannel][srcidx].i;
          tempArray[t].r := tempArray[t].r + b.FFT_Frequency[iChannel][srcidx].r;
          lastsrcidx := srcidx;
        end;

//        for t := 0 to (iWindowlength)-1 do begin
        movemem32(@b.FFT_Frequency[iChannel][0], @tempArray[0],iSize);
//          b.FFT_Frequency[iChannel][t].r := tempArray[t].r;
//          b.FFT_Frequency[iChannel][t].i := tempArray[t].i;
//        end;

      end else begin
        FillMem(@tempArray[0], iSize, 0);

        rDivisor := power(2, ShiftOctaves);
        rDivisor := abs(rDivisor);

        for t := (iWindowlength)-1 downto 0 do begin
          rr := t/iWindowLength;
          srcidx := round(t * rDivisor);
          if srcidx >= iWindowLength then
            continue;
          tempArray[srcidx].i := tempArray[srcidx].i + b.FFT_Frequency[iChannel][t].i;
          tempArray[srcidx].r := tempArray[srcidx].r + b.FFT_Frequency[iChannel][t].r;

        end;

//        for t := (iWindowlength)-1 downto 0 do begin
//          rr := t/iWindowLength;
//          srcidx := round(t / rDivisor);
//          if srcidx >= iWindowLength then
//            continue;
//          tempArray[t].i := tempArray[t].i + b.FFT_Frequency[iChannel][srcidx].i;
//          tempArray[t].r := tempArray[t].r + b.FFT_Frequency[iChannel][srcidx].r;
//        end;

        movemem32(@b.FFT_Frequency[iChannel][0], @tempArray[0], iSize);
//        for t := 0 to (iWindowlength)-1 do begin
//          b.FFT_Frequency[iChannel][t].r := tempArray[t].r;
//          b.FFT_Frequency[iChannel][t].i := tempArray[t].i;
//        end;


      end;
  freemem(temparray);
end;


procedure Tmodosc_SpectrumShift.Init;
begin
  inherited;
  BufferSize := 16384;
  self.Oversamples := 1;
  FShiftOctaves := 0;
end;


function QMarkSynth(t: int64; freq: double; amp: double): double;
var
  p,q,r,s,m,n: int64;
  d,y,x: double;

begin
    {/* Minkowski's question mark function */}

        x := (t/44100)*freq;
        p := floor(x); //long p=x; if ((double)p>x) --p; /* p=floor(x) */

        q := 1; r := p+1; s := 1; //long q=1, r=p+1, s=1, m, n;
        d:=1; y:=p; //double d=1, y=p;


        if (t<p) or ((p<0) xor (r<=0)) then begin //        if (x<(double)p||(p<0)^(r<=0)) return x; /* out of range ?(x) =~ x */
          result := 0;
          exit;
        end;


        while true do begin         //for (;;) /* invariants: q*r-p*s==1 && (double)p/q <= x && x < (double)r/s */
                d := d / 2;  if (y+d) = y then break;  //d/=2; if (y+d==y) break; /* reached max possible precision */
                m := p+r; if (m<0) xor (p<0) then break; //                m=p+r; if ((m<0)^(p<0)) break; /* sum overflowed */

                n := q+s; if (n<0) then break; //n=q+s; if (n<0) break; /* sum overflowed */


                //if (x<(double)m/n) r=m, s=n;
                //else y+=d, p=m, q=n;
                if (x<(m/n)) then begin
                  r := m;
                  s := n;
                end else begin
                  y := y+d;
                  p := m;
                  q := n;
                end;
        end;


        result := (y+d)*amp; //        return y+d; /* final round-off */


end;


{ Tmodosc_MultiBandEQ }

procedure Tmodosc_MultiBandEQ.Buffer_Process_Sync(b: POscBufferInfo);
var
  t: integer;
begin
  inherited;
  b.ToFrequencyFromSpatial;
  for t:= 0 to MAX_CHANNELS-1 do begin
    DoEQ(b, t);
  end;
  b.ToSpatialFromFrequency;

end;


procedure Tmodosc_MultiBandEQ.DoEQ(b: POscBufferInfo; iChannel: integer);
var
//  lOUT: array [0..511] of fftw_complex;
//  la: array[0..DEFAULT_MODIFIER_BUFFER_SIZE-1] of fftw_float;
  lOUT: PAfftw_complex;
  la: PAfftw_float;
  t,u,i,f: integer;
  r: real;
//  la: system.PDouble; //array [0..8191] of double;

  m: integer;
  absval,cc,erg,correction: real;


  iWidth: extended;
  fMax: extended;
  rBucketTop: nativeint;
  rBucketBottom: nativeint;
  i1,i2: floatsample;
  iWindowLength: integer;
  Pin: PSmallint;
  Pout, pout2: system.PSingle;
  p1,p2: pointer;
  rrr,rr: nativefloat;
  iCurrentBand: nativeint;
  rBand: nativefloat;
const
  SPLIT = 1;
  sample_interleave = 0;
begin
  inherited;
  iWindowLength := b.Length;
  RecalcCrossOvers;
        //eq shit
      for u := 0 to BandCount-1 do begin
        if u = 0 then
          rBucketBottom := 0
        else
          rBucketBottom := round(FCrossOverPercentOfNyquist[u-1] * iWindowLength);

        if u = BandCount-1 then
          rBucketTop := iWindowLength
        else
          rBucketTop := round(FCrossOverPercentOfNyquist[u] * iWindowLength);

        rBand := Bands[u];
        if rBand <> 1 then begin
          for t := rBucketBottom to rBucketTop-1 do begin
            b.FFT_Frequency[iChannel][t].i := b.FFT_Frequency[iChannel][t].i * rBand;
            b.FFT_Frequency[iChannel][t].r := b.FFT_Frequency[iChannel][t].r * rBand;
          end;
        end;
      end;

end;

function Tmodosc_MultiBandEQ.GEtBands(idx: nativeint): NativeFloat;
begin
  result := FBands[idx];
end;

function Tmodosc_MultiBandEQ.GetCrossOvers(idx: nativeint): NativeFloat;
begin
  result := FCrossOvers[idx];
end;

procedure Tmodosc_MultiBandEQ.Init;
var
  t: integer;
begin
  inherited;
  for t:= low(FBands) to high(FBands) do begin
    FBands[t] := 1.0;
  end;



  BandCount := 4;
  CrossOvers[0] := 125;
  CrossOvers[1] := 500;
  CrossOvers[2] := 4000;

  RecalcCrossovers;
end;

procedure Tmodosc_MultiBandEQ.o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64);
begin
  inherited;
end;

procedure Tmodosc_MultiBandEQ.RecalcCrossovers;
var
  t: nativeint;
  nf: nativefloat;
begin
  nf := Self.NyquistFrequency;
  for t:= 0 to BandCount-2 do begin
    FCrossOverPercentOfNyquist[t] := FCrossOvers[t] / nf;
  end;

end;

procedure Tmodosc_MultiBandEQ.SetBandCount(const Value: nativeint);
begin
  FBandCount := Value;
end;

procedure Tmodosc_MultiBandEQ.SetBands(idx: nativeint;
  const Value: NativeFloat);
begin
  FBands[idx] := value;
end;

procedure Tmodosc_MultiBandEQ.SetCrossOvers(idx: nativeint;
  const Value: NativeFloat);
begin
  FCrossOvers[idx] := value;
end;

procedure Tmodosc_MultiBandEQ.SourcesChanged;
begin
  inherited;
  RecalcCrossovers;
end;

{ Tmodosc_Delay }

procedure Tmodosc_Delay.BeforeDestruction;
begin
  inherited;
  eq.free;
end;

procedure Tmodosc_Delay.Buffer_Process_Sync(b: POscBufferInfo);
var
  t,u: integer;
  rRight: floatsample;
  ss, ssJunk: TStereoSoundSample;
begin
  inherited;

//  b.ToFrequencyFromSpatial;
//  for t:= 0 to MAX_CHANNELS-1 do begin
    eq.o(mtBeginWindow, ss, b.Start+0);
    //eq.NoiseTest := true;
    for u := 0 to b.Length-1 do begin
      if DelayTimeL = DelayTimeR then begin
        eq.o(mtGetSample, ss, round(u+b.Start-(DelayTimeL* 44100)));
      end else begin
        eq.o(mtGetSample, ss, round(u+b.Start-(DelayTimeL* 44100)));
        eq.o(mtGetSample, ssJUnk, round(u+b.Start-(DelayTimeR *44100)));
        ss.Right := ssJunk.Right;
      end;
      b.FFT_Spatial[0][u] := b.FFT_Spatial[0][u]+(ss.Left*Mix);
      b.FFT_Spatial[1][u] := b.FFT_Spatial[1][u]+(ss.Right*Mix);//todo : SSE optimize
    end;
    eq.o(mtEndWindow, ss, b.Start+0);
//  end;
//  b.ToSpatialFromFrequency;

end;

function Tmodosc_Delay.GetCutoffFrequency: nativefloat;
begin
  result := eq.CrossOvers[0];
end;

procedure Tmodosc_Delay.Init;
begin
  inherited;
  eq := Tmodosc_MultibandEQ.Create;
  eq.BandCount := 2;
  eq.Bands[0] := 0;
  eq.Bands[1] := 1;
  eq.CrossOvers[0] := 500;
  DelayTimeL := 0.4;
  DelayTimeR := 0.2;
  Mix := 0.1;
end;

procedure Tmodosc_Delay.SetCutoffFrequency(const Value: nativefloat);
begin
  eq.CrossOvers[0] := value;
end;

procedure Tmodosc_Delay.SourcesChanged;
begin
  inherited;
  eq.source := self.Source;
end;

{ TSoundCacheRecord }


{ Tcmd_GenerateTurbulenceData }

procedure Tcmd_GenerateTurbulenceData.AllocateRecords;
var
  t: integer;
  p: pointer;
begin
//  p := GetMemory(1);
//  FreeMemeory(p);




  FData := GetMemory(sizeof(TturbulenceRecord)*FRecordCount);
  for t:= 0 to Recordcount-1 do begin
    Records[t].StartSample := round(t * ((sso.SampleCount) / RecordCount));
    Records[t].EndSample := round((t+1) * ((sso.SampleCount) / RecordCount))-1;
  end;
end;

procedure Tcmd_GenerateTurbulenceData.BuildFrames(pass: nativeint);
var
  t,u: nativeint;
  vu: TVUSummary;
  tn: nativeint;
  b: boolean;
  iTurbStart: nativeint;
begin
  StepCount := RecordCount * 6;

  case pass of
    0: begin
      modeq.CrossOvers[0] := 1;
      modeq.CrossOvers[1] := 150;
    end;
    1: begin
      modeq.CrossOvers[0] := 250;
      modeq.CrossOvers[1] := 750;
    end;
    2: begin
      modeq.CrossOvers[0] := 750;
      modeq.CrossOvers[1] := 8000;
    end;
  end;

  Status := FileName;
  self.sso.StartAt := 0;
  self.sso.Start;
  for t:= 0 to Recordcount-1 do begin
    if t mod 100 = 0 then
      Step := (((pass*2)+0)*recordcount)+t;
    vu := GetSSOSpan(records[t].StartSample, records[t].EndSample);
    records[t].bands[pass].Amp_Frame := vu;
    records[t].bands[pass].Amp_Smooth := RollUpFrames(t-round(FPS/ 16), t{+round(FPS/ 5)},pass);
    records[t].bands[pass].Amp_Beat := RollUpFrames(t-round((FPS / 2)), t{+round(FPS/ 2)},pass);
    records[t].bands[pass].Amp_Measure := RollUpFrames(t-round(FPS * 2), t{+round(FPS * 2)},pass);
    records[t].bands[pass].Amp_Phrase := RollUpFrames(t-round(FPS * 16), t{+round(FPS * 16)},pass);
    records[t].bands[pass].Amp_Frame.ValidateLimits;
    records[t].bands[pass].Amp_Smooth.ValidateLimits;
    records[t].bands[pass].Amp_Beat.ValidateLimits;
    records[t].bands[pass].Amp_Measure.ValidateLimits;
    records[t].bands[pass].Amp_Phrase.ValidateLimits;

  end;


  vu := RollUpFrames(0, RecordCount-1,pass);
  for t:= 0 to RecordCount-1 do begin
    records[t].bands[pass].Amp_TRack := vu;
  end;


  u := 0;
  b := false;
  while u < 5 do begin
    tn := 0;
    for t:= 0 to RecordCount-1 do begin
      if IsNewTrigger(t, pass, u, b) then begin
        inc(tn);
        records[t].bands[pass].slopes[u].Triggernumber := tn;
        records[t].bands[pass].slopes[u].NewTrigger := true;
      end else begin
        records[t].bands[pass].slopes[u].Triggernumber := tn;
        records[t].bands[pass].slopes[u].NewTrigger := false;
      end;
    end;
    inc(u);
  end;

  for t:= 0 to Recordcount-1 do begin
    if t mod 100 = 0 then
      Step := (((pass*2)+1)*recordcount)+t;

    records[t].bands[pass].Turb_Reach := FindTurbulenceReach(t, pass, iTurbStart);
    records[t].bands[pass].Turb_Modulation := FindTurbulenceModulation(iTurbStart,records[t].bands[pass].Turb_Reach, pass);
  end;







end;

procedure Tcmd_GenerateTurbulenceData.CleanupRecords;
begin
  if FData = nil then exit;

  FreeMemory(FDAta);
  FData := nil;
end;

destructor Tcmd_GenerateTurbulenceData.Destroy;
begin
  CleanupRecords;

  inherited;
end;

procedure Tcmd_GenerateTurbulenceData.DoExecute;
begin
  inherited;
  sso := nil;
  modeq := nil;

  try
    sso := TSoundStreamOscillator.Create;
    sso.fileName := FileName;
  //  sso.stream.BufferEntireFile := true;


    modeq := Tmodosc_MultibandEQ.create;
    modeq.BandCount := 3;
    modeq.Bands[0] := 0;
    modeq.Bands[1] := 1;
    modeq.Bands[2] := 0;
    modeq.source := sso;
    sso.AddModifier(modeq, false);



    RecordCount := round(((sso.SampleCount / sso.samplerate) *  Self.FPS)+1);


    if fileexists(TurbFileName) and (not REbuild) then
      LoadFromFile
    else begin
      BuildFrames(0);
      BuildFrames(1);
      BuildFrames(2);




      SavetoFile;
    end;
  finally

    if assigned(sso) then
      sso.Removemodifier(modeq);

    if assigned(modeq) then begin
      modeq.Source := nil;
      modeq.Detach;

      modeq.free;
    end;
    sso.free;
    modeq := nil;
    sso := nil;

  end;





end;

function Tcmd_GenerateTurbulenceData.FindTurbulenceModulation(iStart, iEnd,
  iPass: nativeint): nativefloat;
var
  t: integer;
  old, nu: nativefloat;
  min,peaks, avg: nativefloat;
const
  PEAK_SCALE = 0.9;
begin

  peaks := 0;
  avg := 0;

  //protect against DIV/0
  if iStart = iEnd then begin
    result := 0;
    exit;
  end;


{$DEFINE OLD_MOD_CALC}
{$IFDEF OLD_MOD_CALC}
  peaks := 0;
  min := 1;
  for t:= iStart to iEnd do begin

    //for the beat... look at the average peaks and peak peaks
    if records[t].bands[iPass].Amp_Smooth.Peak < min then begin
      min := records[t].bands[iPass].Amp_Smooth.Peak;
      if min = 0 then break;
    end;
    if records[t].bands[iPass].Amp_Smooth.Peak > peaks then begin
      peaks := records[t].bands[iPass].Amp_Smooth.Peak;
      if peaks = 1 then break;
    end;

    //sum up changes in frame peaks




//    peaks := peaks + records[t].bands[iPass].Amp_Frame.Peak;
//    avg := avg + records[t].bands[iPass].Amp_Frame.AvgPeak;
  end;

//  peaks := peaks / (iEnd-iStart);
//  avg := records[t].bands[iPass].Amp_Beat.AvgPeak;
  if iEnd > 9555 then
    result := peaks-min
  else
    result := peaks-min;

  if result < 0 then
    result := 0;
{$ELSE}
  old := records[iStart].bands[iPass].Amp_Frame.Peak * PEAK_SCALE;
  peaks := 0;
  for t:= iStart+1 to iEnd do begin
    nu := records[t].bands[iPass].Amp_Frame.Peak * PEAK_SCALE;
    peaks := peaks + abs(old-nu);
    old := nu;
  end;
  peaks :=   peaks / ((iEnd-iStart)+1);
  if peaks > 1 then
    peaks := 1;

  result := peaks;

{$ENDIF}




end;

function Tcmd_GenerateTurbulenceData.FindTurbulenceReach(
  iStart: nativeint; iPass: nativeint; out iTurbStart: nativeint): nativeint;
var
  t: integer;
  iReference: nativeint;
begin
  result := iStart;
  iReference := records[iStart].bands[iPass].slopes[0].Triggernumber;
  iTurbStart := iStart;

{$IFDEF TUEB_REACH_BACKWARDS}
  for t:= iStart downto 0 do begin
    iTurbSTart := t;
    if records[t].bands[iPass].slopes[0].Triggernumber <> iReference then begin
      break;
    end;

  end;
{$ENDIF}

  for t:= iStart to RecordCount-1 do begin
    result := t;
    if records[t].bands[iPass].slopes[0].Triggernumber <> iReference then begin
      exit;
    end;
  end;
end;

function Tcmd_GenerateTurbulenceData.GetRecord(idx: integer): PTurbulenceRecord;
begin
  if idx >= FRecordCount then
    idx := FRecordCount-1;

  if idx < 0 then begin
    result := nil;
    exit;
  end;

  result := PTurbulenceRecord(@FData[idx]);
end;

function Tcmd_GenerateTurbulenceData.GetSSOSpan(iStart,
  iEnd: int64): TVUSummary;
var
  t: int64;
  ss: TStereoSoundSample;
begin
  if sso.samplerate <> 44100 then begin
    iStart := round(iStart * 1.0 {(44100 / sso.samplerate)});
    iEnd := round(iEnd * 1.0 {(44100 / sso.samplerate)});
  end;

  if iStart < 0 then
    iStart := 0;
  if iEnd >= sso.SampleCount then begin
    iEnd := sso.SampleCount-1;
  end;
  result.Peak := 0;
  result.PeakL := 0;
  result.PeakR := 0;
  result.Balance := 0;
  result.MaxR := -1;
  result.MaxL := -1;
  result.MinL := 1;
  result.MinR := 1;

  if not sso.Started then
    sso.start;

  sso.oo(mtBeginWindow,ss,iStart, false);
  try
    t := iStart;
    while t<iEnd do begin
      sso.oo(mtGetSample,ss, t, false);
      if ss.Left < result.MinL then result.MinL := ss.Left;
      if ss.Right < result.MinR then result.MinR := ss.Right;
      if ss.Left > result.MaxL then result.MaxL := ss.Left;
      if ss.Right > result.MaxR then result.MaxR := ss.Right;
      inc(t);
    end;
    result.PeakL := (result.MaxL - result.MinL)/2;
    result.PeakR := (result.MaxR - result.MinR)/2;
    result.Peak := (result.PeakL+result.PeakR)/2.0;
    result.Balance := result.PeakR - result.PeakL;

  finally
    sso.oo(mtEndWindow,ss, iStart, false);
  end;

end;

procedure Tcmd_GenerateTurbulenceData.Init;
begin
  inherited;
  FPS := 45;
  Icon := @CMD_ICON_SOUND;
end;

function Tcmd_GenerateTurbulenceData.IsNewTrigger(iRecord, iBand,
  iSlope: nativeint; var bGateStatus: boolean ): boolean;
var
  //rMine: nativefloat;
  myrec: PTurbulenceRecord;
begin

  //if record number is 0 then no
  if iRecord = 0 then begin
    result := false;
    exit;
  end;


  myrec := records[iRecord];

  result := false;
  if bGateStatus then begin
    if (myrec.bands[iBand].Amp_Frame.Peak < ((myrec.bands[iBand].Amp_Beat.AvgPeak+myrec.bands[iBand].NormalizedTurbulence) / 2))
    then
      bGateStatus := false;
  end;

  if not bGateStatus then begin
    if (myrec.bands[iBand].Amp_Frame.Peak > myrec.bands[iBand].Amp_Beat.AvgPeak)
    and (myrec.bands[iBand].Amp_Frame.Peak > myrec.bands[iBand].NormalizedTurbulence)
    then begin
      bGateStatus := true;
      result := bGateStatus;
    end;

  end;

//  RunningData[iBand][iSlope].Update(rTime, result);

end;

procedure Tcmd_GenerateTurbulenceData.LoadFromFile;
var
  ms: TlocalMFS;
begin
  ms := TlocalMFS.Create(TurbFileName, fmOpenRead+fmShareDenyNone);
  try
    Stream_GuaranteeRead(ms, @Fdata[0], sizeof(TturbulenceRecord)*FRecordCount);
  finally
    ms.free;
  end;


end;

function Tcmd_GenerateTurbulenceData.RollUpFrames(iFrom,
  iTo, iPass: nativeint): TVUSummary;
var
  t: nativeint;
begin
  if iFrom < 0 then begin
    iFrom := 0;
  end;
  if iTo > RecordCount -1 then begin
    iTo := RecordCount -1;
  end;

  if iFrom > RecordCount -1 then begin
    exit;
  end;


  result := Records[iFrom].bands[ipass].Amp_Frame;
  result.BeginRollup();
  for t:= iFrom+1 to iTo do begin
    result.RollUpWith(records[t].bands[ipass].Amp_Frame);
  end;
  result.FinishRollup();

end;

procedure Tcmd_GenerateTurbulenceData.SaveToFile;
begin
  SaveMemoryAsFile(@Fdata[0],sizeof(TturbulenceRecord)*FRecordCount, TurbFileName);
end;

procedure Tcmd_GenerateTurbulenceData.SetRecordCount(const Value: nativeint);
begin
  CleanupRecords;

  FRecordCount := Value;

  AllocateRecords;
end;

function Tcmd_GenerateTurbulenceData.TurbFileName: string;
begin
  result := changefileext(filename,'.turb');
end;

{ TVUSummary }

procedure TVUSummary.BeginRollUp;
begin
  SamplesRolledUp := 1;
  AVgPeak := 0;
end;

procedure TVUSummary.CopyFrom(src: TVUSummary);
begin
  movemem32(@src, @self, sizeof(TVUSummary));
end;

procedure TVUSummary.FinishRollup;
begin
  Balance := Balance / SamplesRolledUp;
  AvgPeak := AvgPeak / SamplesRolledup;
end;

procedure TVUSummary.RollUpWith(src: TVUSummary);
begin
  if src.Peak > peak then peak := src.Peak;
  if src.PeakL > peakL then peakL := src.Peakl;
  if src.PeakR > PeakR then peakr := src.Peakr;
  if src.MinL < MinL then minl := src.MinL;
  if src.MinR < MinR then minr := src.MinR;
  if src.MaxL > MaxL then maxl := src.MaxL;
  if src.MaxR > MaxR then maxr := src.MaxR;
  self.AvgPeak := AvgPeak + src.Peak;

  Balance := balance + src.Balance;
  SamplesRolledUp := SamplesRolledUp + 1;

end;


procedure TVUSummary.ValidateLimits;
begin
  if avgpeak > 1.0 then avgpeak := 1.0;
  if peak > 1.0 then peak := 1.0;
  if peakl > 1.0 then peakl := 1.0;
  if peakr > 1.0 then peakr := 1.0;
  if balance > 1.0 then balance := 1.0;
  if balance < -1.0 then balance := -1.0;
  if isNan(avgPeak) then
    avgPeak := 0;
  if isNan(peak) then
    avgPeak := 0;
  if isNan(peakl) then
    avgPeak := 0;
  if isNan(peakr) then
    avgPeak := 0;
  if isNan(balance) then
    avgPeak := 0;
end;

{ TTurbulenceCollection }

function TTurbulenceCollection.NormalizedTurbulence: nativefloat;
begin
  if Turb_ReachX = 0 then
    result := 0
  else
    result := Amp_Beat.Peak - Amp_Beat.AvgPeak;
end;

{ TTurbRunningData }








{ TSoundPacketOscillator<T> }

procedure TSoundPacketOscillator.AddPacket(p: TSoundPacket);
var
  i: int64;
begin
  Lock;
  try
    if FindPacket(p.RemoteSampleTime, true) <> nil then
      exit;

    LAstSequenceNumberReceived := p.RemoteSampleTime;
    p.LocalSampleTime := LocalTick;
    p.RealTimeReceived := GetTicker;
    //put the packet into the FIFO
    FIFO[FIFOInptr] := p;
    FIFOInptr := (FIFOinptr + 1) MOD SOUND_PACKET_LOG_SIZE;

//    Debug(inttostr(p.RemoteSampleTime));

    if not FirstPacketReceived then begin
      RemoteTimeStarted := p.RemoteSampleTime;
      FirstPacketReceived := true;
      LocalTimeStarted := LocalTick;
      ClockReferenceOffset := p.RemoteSampleTime-p.LocalSampleTime;
      CleanBillOfHealth(0);
    end;

    MEasuredSampleLag := p.MeasuredSampleLag;


    RunningAvgLocalTime := RunningAvgLocalTime+ p.LocalSampleTime;
    RunningAvgRemoteTime := RunningAvgRemoteTime+p.RemoteSampleTime;
    AVgCount := AvgCount +1;

    i := p.RemoteSampleTime-p.LocalSampleTime;
    if i > ClockREferenceOffset then
      ClockREferenceOffset := i;

     inc(packetsreceived);

  finally
    Unlock;
  end;
end;

constructor TSoundPacketOscillator.create;
var
  t: integer;
begin
  inherited;
  EffectiveLagRecordCount := LAG_RECORDS_WHEN_UNLOCKED;
  Latency := 0;
  TargetLag := DEFAULT_TARGET_UDP_LAG;
  CReationTime := GetTicker;

  for t:= low(lagrecords) to high(lagrecords) do begin
    lagrecords[t] := TargetLag;
  end;


  LAgRunAVg := TargetLAg * EffectiveLAgRecordCount;


  LAG_RECORDS_WHEN_UNLOCKED := CLAG_RECORDS_WHEN_UNLOCKED;
  LAG_RECORDS_WHEN_LOCKED := CLAG_RECORDS_WHEN_LOCKED;
  LAG_LOCK_WINDOW := CLAG_LOCK_WINDOW;
  LAG_UNLOCK_WINDOW := CLAG_UNLOCK_WINDOW;
  LAG_FINE_ADJUST := CLAG_FINE_ADJUST;
  LAG_COARSE_ADJUST := CLAG_COARSE_ADJUST;


end;

destructor TSoundPacketOscillator.destroy;
begin
  inherited;
end;

function TSoundPacketOscillator.FindPacket(iSampleTime: int64; bForFuture: boolean): PSoundPacket;
var
  t,tt,ttt: nativeint;
  a: nativefloat;
  tm: cardinal;
begin
  Lock;
  try
    for tt:= low(FIFO) to high(FIFO) do begin
      t := ((FIFOptr-1)+tt) MOD SOUND_PACKET_LOG_SIZE;
      result := @FIFO[t];

      if (iSampleTime >= result.RemoteSampleTime) and (iSampleTime <= result.RemoteSampleEndTime) then begin
        FIFOptr := t;
        if not bForFuture then begin

          //if (LagRecords[LagPtr] = 0) or (lag <> TargetLag) then begin

          a := lagRunAVg;
          a := a - lagRecords[LagPtr];
          tm := gettimesince(result.RealTimeReceived);
          if a< 0 then
            LagRecords[Lagptr] := tm
          else
            LagRecords[Lagptr] := tm;
          a := a + lagRecords[LagPtr];
          LAgRunAVg := a;
          LastLag := CorrectedLag;
          if bLocked then begin
            //lag := LagRunAVg / EffectiveLagRecordCount;
            SlowCalcRunningLagAverage;
            CorrectedLag := LAg;//(Lag * 0.33) + (LAstLag * 0.67);
          end else begin
            //lag := LagRunAVg / EffectiveLagRecordCount;
            SlowCalcRunningLagAverage;
            CorrectedLag := lag;
          end;





          LagAvgREcords[lagptr] := lag;

          if EFfectiveLagRecordCount <> 0 then begin
            inc(LagPtr);
            LagPtr := LAgPtr mod EffectiveLagRecordCount;
          end;

          //end;
        end;



////        Lag := l;


        exit;
      end;



    end;

    //if packetsreceived >  then
//      REset;
    result := nil;//else result is nil

  finally
    Unlock;
  end;

end;

function TSoundPacketOscillator.LocalSampleTime(iSampleTime: int64): int64;
begin
  result := iSampleTime - LocalTimeStarted;
end;

function TSoundPacketOscillator.LocalTimeToRemoteTime(
  iSampletime: int64): int64;
begin
  if AvgCount = 0 then
    result := iSampleTime
  else begin
//  result := (iSampleTime - LocalTimeStarted) + RemoteTimeStarted;
//    result := (iSampleTime - (RunningAvgLocalTime div AvgCount)) + (RunningAvgRemoteTime div AvgCount);
    result := iSampleTime + ClockREferenceOffset;
  end;


end;

procedure TSoundPacketOscillator.o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64);
var
  iTime: int64;
  ss: PPacketSoundSample;
  ptr: nativeint;
  r: nativefloat;
  l: nativefloat;
  oldpacket: PSoundPacket;
begin
  inherited;



  case mt of
    mtBeginWindow: begin
      if (currentpacket <> nil) and (masterstream <> nil) then begin

        if nativeint(self.MasterStream.Buffersize) <> nativeint(CurrentPacket.SetBufferSizeTo) then begin
//          TArgetLag := round(1000*((currentPacket.SetBufferSizeTo / 44100) * 0.8));
//          if TargetLag < 10 then
//              TargetLag := 10;
          self.MasterStream.Buffersize := currentpacket.SetBufferSizeTo;
        end;

        self.TargetLag := CurrentPacket.TargetLag;
        self.LAG_RECORDS_WHEN_UNLOCKED := CurrentPacket.LagRecordsWhenUnlocked;
        self.LAG_RECORDS_WHEN_LOCKED := CurrentPacket.LagRecordsWhenlocked;
        self.LAG_COARSE_ADJUST := CurrentPacket.CoarseAdjust;
        self.LAG_Fine_ADJUST := CurrentPacket.FineAdjust;
        self.Lag_LOCK_WINDOW := CurrentPacket.LockMargin;
        self.Lag_UNLOCK_WINDOW := CurrentPacket.UnLockMargin;


      end;

      bNewWindow  := true;
    end;
    mtEndWindow: begin
    end;
    mtGetSample: begin
//      s.left := SineSynth(iSampleTime, 1000, 0.25);
//      s.right := SineSynth(iSampleTime, 1000, 0.25);
//      exit;

      if bNewWindow then begin
        self.StreamReadTimes[StreamReadTimePtr].iPosition := iSampleTime;
        self.StreamReadTimes[StreamReadTimePtr].readTime := GetTicker;
        self.StreamReadTimes[StreamReadTimePtr].lagadjust := GetTicker;
        self.StreamReadTimes[StreamReadTimePtr].lag := LAg;

        StreamREadTimePtr := (StreamReadtimePtr+1) mod (high(StreamReadTimes)+1);
        bNewWindow := false;
      end;




      //LocalTick := MasterStream.PlayPosition;
      LocalTick := iSampleTime;//LocalSampleTime(iSampleTime);


      LAtency := round((TargetLag / 1000) * MasterStream.SampleRate);
      iTime := LocalTimeToRemoteTime(iSampleTime)- (Latency);

      if bLocked then begin
        l := CorrectedLag;
      end else begin
        l := LAg;
//        if l > 100 then
//          l := 100;
      end;
      //if l < 0 then l := 0;
      l := AvgServiceLag;

      if bLocked then begin
        r := (TargetLag-l);
        LagAdjust := LagAdjust +(LAG_FINE_ADJUST * r*0.01);
        if (l > (TArgetLag+integer(LAG_UNLOCK_WINDOW))) or (l < (TargetLag-integer(LAG_UNLOCK_WINDOW))) then begin
          bLocked := false;
          EffectiveLagRecordCount := LAG_RECORDS_WHEN_UNLOCKED;
        end;
      end else begin
        r := (TargetLag-l);
        LagAdjust := LagAdjust -(LAG_COARSE_ADJUST * r * 0.01);
        if (l < (TargetLag+integer(LAG_LOCK_WINDOW))) and (l > (TargetLag-integer(LAG_LOCK_WINDOW))) then begin
          if not bLocked then begin
            bLocked := true;
            EffectiveLagRecordCount := LAG_RECORDS_WHEN_UNLOCKED;
            CleanBillofHealth(l);
          end;
        end;
      end;

//      if MEasuredSampleLag > ((1024*3)+256) then begin
//        r :=  MeasuredSampleLag - ((1024*3)+256);
//        LagAdjust := LagAdjust +(0.000001 * r);
//      end else
//      if MEasuredSampleLag < ((1024*3)-256) then begin
//        r := ((1024*3)-256) - MeasuredSampleLag;
//        LagAdjust := LagAdjust -(0.000001 * r);
//      end;

      iTime := iTime+ round(LagAdjust);


      LAstSequenceNumberPlayed := iTime;
      if (CurrentPacket = nil) or (iTime < CurrentPacket.RemoteSampleTime) or (iTime > Currentpacket.RemoteSampleEndTime) then begin

        oldpacket := currentpacket;
        CurrentPacket := FindPacket(iTime, false);
        if currentpacket = nil then begin
          sleep(5);
          CurrentPacket :=FindPacket(iTime, false);
        end;

        if (currentpacket <> oldpacket)
        and (CurrentPacket <> nil) then begin
          ServiceLags[servicelagidx] := ((LocalTick-CurrentPacket.LocalSampleTime)*1000) div 44100;
          inc(servicelagidx);
          servicelagidx := servicelagidx mod length(servicelags);
        end;



      end;

      if CurrentPacket <> nil then begin
        ptr := iTime-CurrentPacket.RemoteSampleTime;
        if (ptr < 0) or (ptr > SOUND_PACKET_DATA_SIZE) then begin
          Reset;
          exit;
          //Debug('packet is fucked up: '+inttostr(CurrentPacket.LocalSampleTime)+','+inttostr(CurrentPacket.RemoteSampleTime));
          //raise Exception.Create('Packet time miscalculation');
        end;
        ss := @CurrentPacket.Data[ptr];


        s.Left := ss.ch[0] / 32767;
        s.Right := ss.ch[1] / 32767;

//        if not bLocked then begin
//          rLeft := rLEft * 0.10;
//          rRight := rRight * 0.10;
//        end;


      end else begin
//        Debug('Packet not found at '+inttostr(iTime));
//        Reset;
        inc(Drops);
        if (Drops > 100) then begin
          Reset;
          Drops := 0;
        end;
        s.Left := 0;
        s.Right := 0;
      end;


    end;
  end;


end;


function TSoundPacketOscillator.RemotetimeToLocalTime(
  iSampletime: int64): int64;
begin
//  result := (iSampleTime - (RunningAvgRemoteTime div AvgCount)) + (RunningAvgLocalTime div AvgCount);
  result := iSampleTime - ClockREferenceOffset;
end;

procedure TSoundPacketOscillator.Reset;
var
  t: integer;
  i: nativeint;
begin
  FirstPacketReceived := false;
  packetsreceived := 0;
  sourceschanged;
  inc(REsets);


  EffectiveLagRecordCount := LAG_RECORDS_WHEN_UNLOCKED;

//  CleanBillOfHealth(20);

  bLocked := false;



end;

procedure TSoundPacketOscillator.SetTargetLag(Value: nativeint);
begin
  if value = FTargetLag then
    exit;
  if value = 0 then value := 1;
  FTargetLag := Value;
  REset;
end;

function TSoundPacketOscillator.AvgServiceLAg: nativefloat;
var
  t: ni;
begin
  result := 0;
  for t:= 0 to high(servicelags) do begin
    result := result + servicelags[t];
  end;

  result := result / length(servicelags);

end;

procedure TSoundPacketOscillator.CalcLagAvgAvg;
var
  t: nativeint;
  a: nativefloat;
begin
  a := 0;
  for t:= 0 to high(LagAvgREcords) do begin
    a := a + LagAVgRecords[t];
  end;

  LagAvgAvg := a / length(LagAvgREcords);
end;

procedure TSoundPacketOscillator.CleanBillofHealth(rLag: nativefloat);
var
  t: integer;
begin
  for t:= 0 to High(LagRecords) do begin
    LAgRecords[t] := rLag;

  end;
  LagRunAvg := rLag * EffectiveLAgRecordCount;

end;

procedure TSoundPacketOscillator.SourcesChanged;
var
  iBufferNeeded: nativeint;
begin
  inherited;
  if assigned(MasterStream) then begin
(*    iBufferNeeded := (round(44100/(1000/TargetLag))*2);

    if self.MasterStream.Buffersize < iBufferNeeded then begin
      self.MasterStream.Buffersize := iBufferNeeded;
    end;*)


  end;
end;

{ TSoundPacket }

function TSoundPacket.LocalSampleEndTime: int64;
begin
  result := LocalSampleTime + SOUND_PACKET_DATA_SIZE;
end;

function TSoundPacket.RemoteSampleEndTime: int64;
begin
  result := RemoteSampleTime + (SOUND_PACKET_DATA_SIZE-1);
end;

{ TSynchronousStreamOscillator }

function TSynchronousStreamOscillator.EndSample: int64;
begin
  result := ((buflen div 4)+bufstart)-1;
end;

procedure TSynchronousStreamOscillator.GiveData(iStartSample: int64; p: PByte;
  length: nativeint);
begin
  while not needsdata do
    sleep(0);

  if buf <> nil then
    FreeMemory(buf);
  buf := GetMemory(buflen);
  movemem32(buf, p, length);
  buflen := length;
  bufstart := iStartSample;


end;

procedure TSynchronousStreamOscillator.o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64);
var
  iLocalSampleTime: int64;
  ps: P16BitStereoSoundSample;
  off: int64;
begin
  iLocalSampleTime := iSampleTime;
  if mt = mtGetSample then begin
    while (buf = nil) or (iLocalSampleTime > EndSample) do begin
      FNeedsDAta := true;
    end;

    FNeedsDAta := false;

    off := (iLocalSampleTime-bufstart) * 4;
    if off < 0 then begin
      ss.Init;
    end else begin
      ps := P16bitStereoSoundSample(@buf[off]);
      ss.Left := ps.ch[0] /32768;
      ss.Right := ps.ch[1] /32768;
    end;
  end;
end;





{ TDT3Header }





{ Tmodosc_Fade }

procedure Tmodosc_Fade.FadeNow(rDurationInSeconds: nativefloat; dir: TFadeDirection);
begin
  Automate := true;
  if rDurationInSeconds < 0.01 then
    rDurationInSeconds := 0.01;

  FStartFadeOnNextSample := true;
  FFadeDurationInSeconds := rDurationInSeconds;
  FDirection := dir;

  if dir = fdIn then
    FCurrentFader := 0.0
  else
    FCurrentFader := 1.0;

end;

procedure Tmodosc_Fade.o(mt: ToscMessageType; out ss: TStereoSoundSample;
  iSampletime: int64);
begin
  inherited;
  source.o(mt, ss,iSampleTime);//<<<---- VERY IMPORTANT DUHHHH

  if mt = mtGetSample then begin
    if FStartFadeOnNextSample then begin
      FFadeStartSample := iSampleTime;
      FFadeEndSample := iSampleTime + round(Self.FadeDurationInSeconds*self.SampleRate);
      FStartFadeOnNextSample := false;
    end;

    if Automate then begin
      if direction = fdIn then begin
        if iSampleTime <= FFadeStartSample then begin
          FCurrentFader := 0.0;

        end else
        if iSampleTime >= FFadeEndSample then begin
          FCurrentFader := 1.0;
          Automate := false;
        end else
          FCurrentFader := (iSAmpletime-FFadeStartSample) / (FFadeEndSample-FFadeStartSample);
      end else begin
        if iSampleTime <= FFadeStartSample then begin
          FCurrentFader := 1.0;
          //Automate := false;
        end else
        if iSampleTime >= FFadeEndSample then begin
          FCurrentFader := 0.0;
          AutoMate := false;
        end else
          FCurrentFader := 1-((iSAmpletime-FFadeStartSample) / (FFadeEndSample-FFadeStartSample));
      end;
    end;

    ss := ss * FCurrentFader;

  end else begin

  end;
end;

{ TTimeCorrellator }


{ TRemoteAudioList }

function TRemoteAudioList.IndexOf(sIP: string; iport: cardinal): nativeint;
var
  t: integer;
begin
  result := -1;
  for t:= 0 to count-1 do begin
    if (self[t].IP = sIP) and  (self[t].Port = iPort) then begin
      result := t;
      exit;
    end;
  end;

end;

{ TSoundCommand }


{ Tcmd_PlayStream_Partial }

{ TRFAPacket }

procedure TRFAPacket.Init;
begin
  MissingPosition := 0;
  ThreeCC[0] := 82{'R'};
  ThreeCC[1] := 70{'F'};
  ThreeCC[2] := 65{'A'};

end;

{ TDSoundUT }

{ TDSoundUT }



procedure TSineWaveOscillator.Init;
begin
  inherited;
  FFreq := 450;
  FAmp := 0.25;
end;

procedure TSineWaveOscillator.o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64);
var
  aa, ra: nativefloat;
begin
  inherited;
  if mt=mtGetSample then begin
    aa := GetAttackAmp(iSampleTime);
    ra := GetreleaseAmp(iSampleTime);
    s.Left := SineSynth(iSampleTime, Freq, Amp) * aa * ra;
    s.Right := SineSynth(iSampleTime, Freq, Amp) * aa * ra;
  end;
end;


procedure TSoundPacketOscillator.SlowCalcRunningLagAverage;
var
  t: integer;
begin
  if EffectiveLagRecordCount = 0 then begin
    Lag := 1;
    exit;
  end;
//  if bLocked then begin
    LAgRunAvg := 0;
    for t:= 0 to EffectiveLagRecordCount-1 do begin
      LAgRunAvg := LagRunAvg + LagRecords[t];
    end;
//  end;
  Lag := LagRunAvg / EffectiveLagRecordCount;
end;

procedure Tmodosc_gain.Init;
begin
  FLevel := 1.0;
end;

procedure Tmodosc_gain.o(mt: ToscMessageType; out ss: TStereosoundSample; iSampletime: int64);
begin
  inherited;
  source.o(mt, ss, iSampleTime);//<<<---- VERY IMPORTANT DUHHHH

  ss := ss * LEvel;

end;



//-----------------------------------------------------------------------------
constructor TManualMetronomeOscillator.Create;
begin
  inherited;
  FVolume := 1.0;
  FEnvelope := 0;

end;
//-----------------------------------------------------------------------------
procedure TManualMetronomeOscillator.Beep();
begin
  BeepNow := true;
end;
//-----------------------------------------------------------------------------
procedure TManualMetronomeOscillator.o(mt: ToscMessageType; out ss: TStereoSoundSample; iSampletime: int64);
begin
  case mt of
    mtGetSample:
      begin

        if (BeepNow) then begin
          BeepNow := false;
          FEnvelopeTime := iSampleTime;
        end;

        if (iSampleTime < FEnvelopeTime) then
          FEnvelope := 0
        else if (iSampleTime - FEnvelopeTime) > 50 then
          FEnvelope := 0
        else
          FEnvelope := 1-((iSampleTime - FEnvelopeTime) / 50);

        ss.Left := (Random(1000000)/1000000) * FEnvelope * FVolume;
        ss.Right := (Random(1000000)/1000000) * FEnvelope * FVolume;
      end;
  end;
end;
//-----------------------------------------------------------------------------


{ Tmodosc_SimpleEQ }

procedure Tmodosc_SimpleEQ.Init;
begin
  inherited;

end;

procedure Tmodosc_SimpleEQ.o(mt: ToscMessageType; out s: TStereoSoundSample; iSampletime: int64);
var
  iWindowSize: nativeint;
begin
  inherited;
  //to cut the high-end we simply average out the wave over time
  //the 100% mix resulting sample is the average of the other samples across the average window
  //1-Determine window size
//  iWindowSize := CrossOver


  //2-Average samples backwards in time
  //

  //source.o(mt, rLeft, rRight, iSampleTime);//<<<---- VERY IMPORTANT DUHHHH

//  rLeft := rLEft * LEvel;
//  rRight := rRight * Level;


end;


{ TQueuedUDPPacket }

procedure TQueuedUDPPacket.DoExecute;
begin
  inherited;
  client.SendBuffer(ip, port, data);
end;

procedure TQueuedUDPPacket.Init;
begin
  inherited;
  self.AutoDestroy := true;
end;

{ TSynthOscillatorObject }

function TSynthOscillatorObject.GetAttackAmp(const iSampleTime: int64): nativefloat;
begin
  if self.masterstream = nil then
    exit(0.0);

  result := lesserof(1.0, (((iSampleTime - FAttackBeginTime)) / (self.MasterStream.SampleRate * FAttack)));
end;


function TSynthOscillatorObject.GetReleaseAmp(
  const iSampletime: int64): nativefloat;
begin
  if FReleaseBeginTime = 0 then
    exit(1.0);
  result := lesserof(1.0, (((iSampleTime - FReleaseBeginTime)) / (self.MasterStream.SampleRate * FRelease)));
  result := 1.0-result;
  ReleaseComplete := result = 0.0;
end;

procedure TSynthOscillatorObject.init;
begin
  inherited;
  FAttack := 1/1000;
  FRelease := 500/1000;
end;

function TSynthOscillatorObject.IsReleaseComplete(const iSampleTime: int64): boolean;
begin
  result := releaseComplete;
end;

procedure TSynthOscillatorObject.o(mt: ToscMessageType;
  out s: TStereoSoundSample; iSampletime: int64);
begin
  inherited;
  if not WasAttacked then begin
    WasAttacked := true;
    o(mtAttack, s, iSampleTime);

  end;
  if NeedsRelease then begin
    NeedsRelease := false;
    if not WasReleased then begin
      WasReleased := true;
      o(mtRelease, s, iSampleTime);

    end;
  end;

  case mt of
    mtAttack:
    begin
      FAttackBeginTime := iSampleTime;
      s.Left := 0;
      s.right := 0;
    end;
    mtRelease:
    begin
      FReleaseBeginTime := iSampleTime;
      s.Left := 0;
      s.right := 0;
    end;
  end;
end;

initialization
  gtest.left := 0;
  gtest.Right := 0;
  gflag := false;

end.



