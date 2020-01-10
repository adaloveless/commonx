unit ffmpeg_tools;
{$DEFINE USE_MFS}
{$DEFINE DO_THUMB}
{$DEFINE DO_OQ}
{$DEFINE DO_UQ}
{x$define DO_LQ}
{x$DEFINE DO_SWAP_CR}
{$DEFINE TIME_LIMIT}
{x$DEFINE REXE}//define this only at the projet level


interface

uses
  ExternalMemoryStream, helpers_stream,numbers,search, encodermaster,
  soundconversions_commandline, rtti_helpers, commands_file,
  debug, tickcount, soundinterfaces, commandicons, managedthread, sharedobject,
  commandprocessor, soundtools, classes, betterobject, easyimage, dir, dirfile,
  windows, systemx, typex, stringx, exe, sysutils, MultiBufferMemoryFileStream,
  vcl.imaging.jpeg;

const
ACODEC_Safe = 'aac';
ACODEC_4k = 'mp3';
ACODEC_Apple = 'eac3';

CMD_ICON_SUITE: TCommandIcon = (BitDepth: 32; RDepth:8;GDepth:8;BDepth:8;ADepth:8;Height:32;Width:32;
 data:
(
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$03010109,$6D150D83,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000003,$801C157F,$FD2B1DD5,$14080621,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$5B1C165E,$FF3C2DD6,$FF3726D4,$721B126C,$2D0C0734,$10050316,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$3D191544,$F84D3FD9,$FF4838D6,$FF4330D4,$FF3E29D1,$FF3922CF,$FF361DCC,$CB2D15B6,$851F0C78,$370E0537,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00020303,$2829342C,$6545525F,$E6594CCE,$FF5A4AD9,$FF5442D7,$FF4F3BD4,$FF4A33D1,$FF452CCF,$FF4126CC,$FF3E20C9,$FF3B1BC7,$FF3A17C5,$BD3110A4,$3F14063F,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$02050706,$524B6155,$C48EBCA8,$FF68B5C7,$FF626BD8,$FF6C5CDC,$FF6655DA,$FF614DD7,$FF5B46D4,$FF563ED2,$FF5136CF,$FF4D2FCC,$FF4929C9,$FF4624C6,$FF441EC4,$FF481DC5,$FE4E1DC8,$8A2F0F7E,$0604010F,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$2A27352F,$BA96C1AB,$FF9FE0C9,$FF2A9BB1,$FF607AD7,$FF7C6DDF,$FF7766DD,$FF725FDA,$FF6D58D8,$FF6851D5,$FF6249D2,$FF5D41CF,$FF593ACC,$FF5533C9,$FF512DC6,$FF4E27C3,$FF4C22C0,$FF5223C2,$FF682FD2,$B6521CAD,$110C031C,$00000000,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$594C675C,$F3BBF2D9,$FFA9E5CC,$FF269DA0,$FF5F8CD7,$FF8D7CEE,$FF8877E7,$FF8270DF,$FF7D69DB,$FF7963D8,$FF745BD6,$FF6E53D3,$FF6C4CD9,$FF6945D8,$FF643FD4,$FF6037CE,$FF5B31C7,$FF572AC0,$FF5525BD,$FF5825BD,$FF7B42D9,$C3642BBB,$10100323,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$02020303,$7F5A8776,$FFB5F0D7,$FFB5EDD3,$FF35A698,$FF159AAA,$FF5EA8DE,$FF909DF3,$FF9E89F2,$FF907AEB,$FF8B74E4,$FF856DDC,$FF8066D6,$FF7B5FD3,$FF7C5ADD,$FF7D56E4,$FF794EE1,$FF7245DB,$FF6E3ED6,$FF6A37D0,$FF6430C5,$FF5F29BA,$FF5F26B7,$FF7B41D1,$BB7130CF,$0C1A0438,$00000000,$00000000,$00000000),
($00000000,$00000000,$00010302,$79478870,$FF95EAC9,$FFAFEFD1,$FF6AC3AB,$FF0F9894,$FF12A6BC,$FF15B1DC,$FF16B1E8,$FF60B5F0,$F1B6AAF5,$FFA589F1,$FF9478E8,$FF8F72E1,$FF886AD7,$D27C5DC2,$634E376A,$6B583C7C,$865E3C8D,$BC7946BA,$FE8F4DE4,$FF7A3ED6,$FF7235C9,$FF6A2EB9,$FF6829B3,$FF7D3ECE,$8F6721C4,$01080117,$00000000,$00000000),
($00000000,$00000202,$452A835B,$FF70E6B9,$FF8DE8C1,$FFA2E9C8,$FF199884,$FF11A5A8,$FF16B9D5,$FF1DBEE9,$F25CCAEE,$85658693,$0D121617,$454C3F5F,$BB977DC7,$FFAA88EE,$FF9775E3,$D88765CC,$00000000,$00000000,$00000000,$00000000,$5E59285D,$F9A64DC9,$FF9247D8,$FF7E3AC8,$FF7532B6,$FF722EAF,$F47A33C4,$2733096C,$00000000,$00000000),
($00000000,$11115031,$DA5DE8A9,$FF68E2B1,$FF85E7BA,$FF71CCAA,$FF0F9B8B,$FF14B7BF,$FF2EC9E2,$B96EB5C5,$272A373A,$00000000,$00000000,$00000000,$00000000,$3F433753,$BD9978C1,$FCAB82E9,$15191223,$00000000,$00000000,$00000000,$00000000,$6F622B62,$FF9A3A9B,$FF9F46C8,$FF8E40C9,$FF7F36B1,$FF7D32AC,$945D1D94,$00000000,$00000000),
($00000000,$7442C685,$FF6DE9B2,$FF5EDEA7,$FF7EE7B3,$FF50B899,$FF11A99D,$FE2EC5CD,$89539299,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$4D52405F,$2F32263C,$00000000,$00000000,$00000000,$00000000,$060B050B,$DBA842A3,$FF953085,$FFA243B6,$FF9844C3,$FF8739AA,$E781329F,$060A040C,$00000000),
($06072315,$DB78F0B3,$FF53E0A4,$FF57DC9E,$FF7DE6AD,$FF37AD90,$FF20B4A8,$83408D91,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$867B3178,$FFB73AA0,$FF932D7C,$FFA846B4,$FF9A45B3,$FF913DA5,$503D1945,$00000000),
($34124A31,$FF86F1BB,$FF39D693,$FF4FDA94,$FF7BE5A8,$FF21A687,$A7258E85,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$525B245B,$FFD641B6,$FFB33491,$FF942C79,$FFB24EBA,$FF9B46A6,$89612966,$00000000),
($6A247652,$FF7EEDB7,$FF34D388,$FF49D88B,$FF76E2A0,$F223AA8D,$1C041F1E,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$303E173E,$FFEB48C6,$FFCF399F,$FFA92E80,$FFA13789,$FFAF54B1,$B1833C83,$00000000),
($942F986C,$FF64E4A6,$FF32D080,$FF42D580,$FF6ADD92,$AC3A917B,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$1A260D25,$FFF647C8,$FFE23AA3,$FFBF3088,$FF9F2870,$FFB551A5,$C9A44F9B,$00000000),
($B031AA76,$FF4CDB94,$FF32CD78,$FF3DD175,$FF59D87F,$87578A73,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$101C091A,$FFF943BE,$FFEA379D,$FFCE3387,$FFBB3B72,$FFAE3F86,$D2AF59A0,$00000000),
($BB2CA970,$FF3ED587,$FF32CB72,$FF38CC6B,$FF4DD371,$A6649E71,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$0D1C0817,$FFF93DAF,$FFEE498D,$FFEA8A66,$FFD46D58,$FFB43277,$CAB863A4,$00000000),
($B927A468,$FF36CF7C,$FF33C96C,$FF36C762,$FF47CF66,$EF77D487,$13121913,$00000000,$09090C09,$403C4A3D,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$141F0B15,$FFF8708D,$FFE99C5C,$FFD78847,$FFD07A3E,$FFBD2D68,$B2A75F93,$00000000),
($AA23955B,$FF33CB72,$FF34C766,$FF35C35B,$FF41CB5C,$FF5DD16D,$865F8463,$08070B07,$A67DA880,$9E819C83,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$1B231C10,$B2B78E50,$FFDF9956,$FFD58947,$FFCD7D3C,$FFC37331,$FFC2434D,$8C8A5379,$00000000),
($8D1E7D48,$FF33C96C,$FF34C560,$FF35C155,$FF3CC452,$FF50CC5D,$F37CD283,$B66FAD73,$FFA1E3A6,$AF8EB091,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$01030201,$555C4E2D,$E6DAAB62,$FFD89755,$FFD28948,$FFC97C3D,$FFC37332,$FFC16D28,$FFC55F3B,$545E3E54,$00000000),
($62165931,$FF34C766,$FF35C35B,$FF36BF51,$FF38BE4A,$FF49C754,$FF60CB68,$FF7AD181,$FF99E09E,$BD8FB392,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$181C1A0F,$A3A68F53,$FFE1AF66,$FFD49955,$FFCE8B49,$FFC57D3D,$FFC37432,$FFC16D29,$FFBE6720,$FFBD641D,$2C3A2015,$00000000),
($290C2F18,$FF34C560,$FF35C155,$FF36BE4C,$FF37BB45,$FF41BE4B,$FF56C55F,$FF71CE78,$FF90DD96,$CE9DCBA0,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$1118150D,$BCC0A561,$FFD6A762,$FFD19A56,$FFC98C4A,$FFC47F3E,$FFC37533,$FFC16D29,$FFBE6720,$FFBC6218,$FFBA5D12,$806A3407,$00000000),
($00000402,$BE2BA14A,$FF36BF50,$FF37BD48,$FF37BA42,$FF3CBC45,$FF4EC257,$FF68CB6F,$FF87DA8D,$DD9DD0A0,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$323C2D18,$9B8D6432,$FFC9853F,$FFC37833,$FFC06E29,$FFBE6720,$FFBC6218,$FFBA5D11,$FFB85A0C,$D5A6500A,$01060300),
($00000000,$4713451D,$FF36BE4C,$FF37BB44,$FF37BA41,$FF39BB42,$FF47C050,$FF5FC867,$FF7DD684,$F3A6E4A8,$25242D05,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$01030201,$ABAC7833,$FFD08234,$FFCA752A,$FFBE6720,$FFBC6218,$FFBA5D11,$FFB85A0C,$FFB8590B,$FFB8590B,$514B2405),
($02020302,$79297436,$FF37BC48,$FF37BA42,$FF37BA41,$FF37BA41,$FF41BD4A,$FF57C55F,$FF75D37C,$FFA0E3A4,$F3ABD417,$796D800E,$0F131603,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$00000000,$5C674A1F,$FFDA8E37,$FFCD7A2B,$FFC26C21,$FFBC6218,$FFBA5D12,$FFB85A0C,$FFB8590B,$C69E4C09,$F5B8590B,$B2934708),
($7D2D7A3E,$FF37BE4C,$FF37BB44,$FF37BA41,$FF37BA41,$FF37BA41,$FF3CBC46,$FF4FC258,$FF6CD073,$FF98E09D,$FFA2C827,$FFAFCC18,$E3BBD019,$72727A0E,$19212304,$00000000,$00000000,$00000000,$00000000,$00000000,$47574516,$F5DD9639,$FFCC7D2B,$FFC67122,$FFBB6319,$FFBA5E12,$FFB85A0D,$FFB8590B,$B5954808,$00000000,$12190D02,$5D572A05),
($0C061509,$49124119,$731D6323,$9B257D2C,$C42D9A35,$EE36B440,$FF39BB43,$FF48C051,$FF63CD6B,$FF8CDD92,$FFA0C23C,$FFA5BD16,$FFB0C217,$FFBBC618,$FACDD21A,$AEA5A515,$7A6E6D0F,$6763630E,$795F5E0E,$B6A99323,$FEDD9B37,$FFCA802C,$FFC67422,$FFBB661A,$FFBA5E13,$FFB85A0D,$FFB8590B,$DDA7510A,$1B1F0F01,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$00000000,$1112120F,$C497B272,$FF73B441,$FF68B33E,$FF80C667,$FF94B13D,$FF8DA013,$FF96A314,$FF9DA514,$FFA0A315,$FF9E9B14,$FF969113,$FF9B8E18,$FFC9A330,$FFD79938,$FFC7832C,$FFC27622,$FFB9681A,$FFB96014,$FFB95B0D,$FFB8590B,$E4AE540A,$2F351A02,$00000000,$00000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$00000000,$02000000,$05000000,$1911110D,$B7A7A67F,$FFB2BE68,$FF8D9F33,$FF758714,$FF74810F,$FF77800F,$FF777C0F,$FF7B7B12,$FF968D22,$FFB79F34,$FFD9AB42,$FFC79235,$FFC3842C,$FFBB7622,$FFB76B1A,$FFB76214,$FFB85E10,$FFB65A0C,$D89F4D09,$35271302,$06000000,$03000000,$01000000,$00000000,$00000000),
($00000000,$00000000,$00000000,$03000000,$08000000,$11000000,$1C000000,$2D040402,$AC83815D,$FFDAD598,$FFDEDA90,$FFC4C169,$FFBDB758,$FFC9BB58,$FFDFC860,$FFD3B351,$FFC49E41,$FFBF9135,$FFBA842C,$FFB57722,$FFB56E1A,$FFB66817,$FFB76A1E,$FFB46216,$CC894409,$3E110801,$21000000,$14000000,$0A000000,$04000000,$01000000,$00000000)
 ));

const
  extract_image_ext = 'jpg';

type
  TREXE = class(TCommand)
  protected
    FRemoteCPUExpense: single;
    procedure BuildCommandline;virtual;
    procedure DoExecute; override;

    procedure CC(ccStatus: TConsoleCaptureStatus; sData: string);virtual;
    function GetCPUExpense: single;reintroduce;
    procedure SetCPUExpense(Value: single);reintroduce;
    procedure OnStart;override;
  public
    prog: string;
    params: string;
    gpuparams: string;
    workingdir: string;
    hide: boolean;//noeffect
    batchwrap: boolean;//noeffect
    ConsoleRedirect: boolean;//always true
    CaptureConsoleOutput: boolean;
    ConsoleOutput: string;


    Timeout: int64;
    procedure InitExpense; override;
    property CPuExpense: single read GetCPUExpense write SetCPUExpense;
  end;
{$IFDEF REXE}
  TUnitExe = TREXE;
{$ELSE}
  TUnitExe = Tcmd_RunExe;
{$ENDIF}


  TFFProbe = record
    w,h: single;
    VideoCodec: string;
    VideoCodecEx: string;
    AudioCodec: string;
    AudioCodecEx: string;
    procedure Init;
    function ToString: string;
    function Is5_1: boolean;
    function Is7_1: boolean;

  end;
  TBleedingEdge = (beAncient, beStable, be4kEra);
  TJVIDFormat = (jvfJPG, jvfDDS, jvfCRN);
  TJVIDMainHeader = packed record
    fourcc: array [0..3] of ansichar;
    version: cardinal;
    display_width: cardinal;
    display_height: cardinal;
    internal_width: cardinal;
    internal_height: cardinal;
    numberofframes: cardinal;
    framerate: cardinal;
    indexoffset: int64;
    format: TJVIDFormat;
    procedure Init;
  end;
  TJVIDHeader = packed record
    FrameNumber: cardinal;
    PrevFrameSize: cardinal;
    FrameSize: cardinal;
    CheckSum: cardinal;
    procedure CalculateChecksum;
    function VerifyChecksum: boolean;
  end;

  Tcmd_VideoToDmxVideo = class(TCommand)
  private
    FOutputFile: string;
    FInputfile: string;
    function GetOutputAudioFile: string;
  public
    procedure Init;override;
    class function Go(sINput: string; sOutput: string): Tcmd_VideoToDmxVideo;
    procedure DoExecute;override;
    procedure InitExpense;override;
    property InputFile: string read FInputfile write FInputFile;
    property OutputFile: string read FOutputFile write FOutputFile;
    property OutputAudioFile: string read GetOutputAudioFile;
  end;

  Tcmd_VideoToJVID = class(TCommand)
  private
    FOutputFile: string;
    FInputfile: string;
    function GetOutputAudioFile: string;
  public
    procedure Init;override;
    class function Go(sINput: string; sOutput: string): Tcmd_VideoToJVID;
    procedure DoExecute;override;
    procedure InitExpense;override;
    property InputFile: string read FInputfile write FInputFile;
    property OutputFile: string read FOutputFile write FOutputFile;
    property OutputAudioFile: string read GetOutputAudioFile;
  end;

  Tcmd_ConversionSuite = class(TCommand)
  protected
    procedure MakeFilm(rootfolder:string; sFile: string);overload;
    procedure MakeFilm(rootfolder:string; sbase: string; fil: TFileInformation);overload;
    procedure DoExecute; override;
    function TarTempPath: string;
    function TarExtPath: string;
    function TarTempFileName: string;
  public
    useremotes: boolean;
    clean: boolean;
    trackingnumber: int64;
    filename: string;
    procedure InitExpense; override;
    procedure RenameExistingRelatedFiles;
    procedure Init; override;


  end;


  Tint64Array = array[0..0] of int64;
  PInt64Array = ^Tint64Array;

  TJVIdReader = class;//forward

  Tcmd_NextFrame = class (TCommand)
  private
    FReader: TJVIDReader;
  public
    property Reader: TJVIDReader read FReader write Freader;
    procedure DoExecute;override;
  end;

{$IFDEF USE_MFS}
  TJVIDFileStream = TMultiBufferMemoryFileSTream;
{$ELSE}
  TJVIDFileStream = TFileStream;
{$ENDIF}

  TJVIdReader = class(TSharedOBject)
  protected
    stream: TJVIDFileStream;
    procedure ReadCurrentFrame;
    procedure ReadCurrentFrameHeader;
    procedure ReadCurrentFrameBuffer;
  public

    framememory: Pbyte;
    mainheader: TJVIDMainHeader;
    header: TJVIDHeader;
    framenumber: nativeint;
    framememorylength: nativeint;
    index: PInt64Array;
    cmdNextFRame: Tcmd_NextFrame;
    thrPrefetch: TExternalEventThread;
    PrefetchRequestFlag: nativeint;
    PrefetchAckFlag: nativeint;

    procedure Init;override;
    destructor Destroy;override;
    procedure OnNewVideo;virtual;

    procedure LoadFromFile(sFile: string);
    procedure NextFrame(iCount: integer = 1);
    procedure PrevFrame(iCount: integer = 1);
    procedure GotoFrame(iNumber: nativeint);
    procedure GotoTime(rTime: nativefloat);
    procedure Cleanup;
    procedure CleanupMain;
    function FramePOinterAt: nativeint;
    function FileName: string;
    function GetStreamFromFrameMemory: TExternalMemoryStream;
    function TimeToFrameNumber(rTime: nativefloat): nativeint;
    procedure CompleteCommand;
    procedure PrefetchNextFrame;
    procedure StartPrefetchThread;
    procedure StopPrefetchThread;

    procedure PrefetchThread_OnExecute(sender: TExternalEventThread);
  end;

  Tcmd_XBOXtranscode = class(TUnitExe)
  private
    Finfile: string;
    Foutfile: string;
  protected
    procedure Init;override;
    procedure InitExpense;override;
    procedure BuildCommandLine;override;
  public
    property InFile: string read Finfile write finfile;
    property outfile: string read Foutfile write foutfile;
  end;

  Tcmd_Mobiletranscode = class(TUnitExe)
  private
    Finfile: string;
    Foutfile: string;
  protected
    procedure InitExpense;override;
    procedure BuildCommandLine;override;
  public
    property InFile: string read Finfile write finfile;
    property outfile: string read Foutfile write foutfile;
  end;

  TFFMPEG_Action = (ffExtractFrame,ffExtractFrames, ffToDmxVideo, ffToMp3, ffToChromecastLQ, ffToChromecastHQ, ffToChromecastOQ, ffToChromeCastUQ, ffToOculus,ffProbe, ffSwapCR);

  Tcmd_FFMPEG = class(TUnitExe)
  private
    FW: nativeint;
    Fh: nativeint;
    Foutfile: string;
    FAction: TFFMPeg_Action;
    Finfile: string;
    Foutdir: string;
    Findir: string;
    FBleedingEdge: TBleedingEdge;
    FAlternate: boolean;
    FOverWriteOutput: boolean;
    FTotalFrames: ni;
    FDurationInMS: int64;
    procedure SetOutputFile(const Value: string);
  protected
    procedure InitExpense;override;
    procedure DoExecute;override;
    procedure PreProcess;override;
    procedure PostProcess;override;
    procedure BuildCommandLine;override;
    procedure CC(ccStatus: TConsoleCaptureStatus; sData: string);override;
    procedure OnStart;override;
   public

    procedure Init; override;
    constructor Create; override;
    property ACtion: TFFMPeg_Action read FAction write FAction;
    procedure AfterConstruction; override;

    property InputFile: string read Finfile write finfile;
    property outputdirectory: string read Foutdir write foutdir;
    property InputDirectory: string read Findir write findir;
    property outputFile: string read Foutfile write SEtOutputFile;
    property Width: nativeint read FW write Fw;
    property Height: nativeint read Fh write Fh;
    property BleedingEdge: TBleedingEdge read FBleedingEdge write FBleedingEdge;
    property OverwriteOutput: boolean read FOverWriteOutput write FOverwriteOutput;
  end;


function ffMpegProbeStr(sFile: string; bSynchronous: boolean = true): string;
function ffMpegProbe(sFile: string): TFFProbe;

function ffGetRelatedFileName(sFile, sQualityNoDot: string; bAppendExt: boolean): string;
function ffFindRelatedFile(sFile, sQualityNoDot: string): string;



procedure VideoToDmxVideo(sInputFile: string; sOutputFile: string; cmd: TCommand = nil);
procedure VideoToJVid(sInputFile: string; sOutputFile: string; cmd: TCommand = nil; fmt: TJVIDFormat = jvfJPG);
procedure CreateJVIDFromFrames_JPG(LengthInSeconds: nativefloat; sInputDir: string; sOutFile: string; sINputFilter: string = '*.jpg'; cmd: TCommand = nil);
procedure CreateJVIDFromFrames_Crunch(LengthInSeconds: nativefloat; sInputDir: string; sOutFile: string; sINputFilter: string = '*.jpg'; CrunchFmt: TJVIDFormat = jvfCRN; sCrunchQuality: nativeint = 128; cmd: TCommand = nil);

//procedure REbuildVideoFrames(sInputDir: string; sOutputFile: string);
procedure REbuildVideoFrames(sInputDir: string; sOutputFile: string; w,h,fps: integer);
procedure REbuildVideoFrames_DateOrder(sInputDir: string; sOutputFile: string; w,h,fps: nativeint; ext: string = 'jpg');
procedure REbuildVideoFrames_DateOrder_AggSubfolder(sInputDir: string; sOutputFile: string; w,h,fps: nativeint; ext: string = 'jpg');

procedure ExtractVideoFrames(sInputFile: string; sOutputDir: string; w: nativeint = 0; h: nativeint = 0);
function BeginExtractVideoFrames(sInputFile: string; sOutputDir: string; w: nativeint = 0; h: nativeint = 0): Tcmd_FFMPEG;
procedure EndExtractVideoFrames(cexe: Tcmd_FFMPEG);
procedure ConformVideoFiles(sOutputDir: string; bPad: boolean; bDeleteDuplicates: boolean);
function BeginTranscodeForXBOX(sFile: string; sOutFile:string): Tcmd_XBOXtranscode;
procedure EndTranscodeForXbox(c: Tcmd_XBOXtranscode);
//d:\source\DMX_O_Matic\ffmpeg.exe -i "h:\archives\tv\Workaholics\Episode 1 - Piss & Shit.mkv" -f mp4 -vcodec mpeg4 -b 2000000 -acodec libfaac -ac 2 -ab 128000 i:\tv\test.mp4


function ffmpeg_cpus(relativepower: single): single;
function ffmpeg: string;
//function ffmpeg_ex(sVariation: string = '_bleeding_edge_sept'):string;
function ffmpeg_ex(sVariation: string = '_2020'):string;
implementation

uses
  servervideoparser, tools;

function ffmpeg_cpus(relativepower: single): single;
begin
  result := greaterof(1,lesserof(GetEnabledCPUCount,16*relativepower));//ffmpeg doesn't really scale past 16 cpus
end;

function ffmpeg: string;
begin
  result := FindTool('ffmpeg.exe');
//  result := adjustpath(extractfilepath(dllname))+'ffmpeg.exe';
end;

function ffmpeg_ex(sVariation: string):string;
begin
  result := FindTool('ffmpeg'+svariation+'.exe');
//  result := adjustpath(extractfilepath(dllname))+'ffmpeg.exe';
end;



procedure REbuildVideoFrames(sInputDir: string; sOutputFile: string; w,h,fps: integer);
var
  dir: TDirectory;
  fil: TfileInformation;
  sNewName: string;
  sLeft: string;
  sRight: string;
  sPArams: string;

begin
//ffmpeg -f image2 -i c:working.%d.jpg -b 1000000 -s 720x480 -aspect 16:9 -f mpeg2video o:output.avi



  dir := TDirectory.create(sInputDir, '*.'+extract_image_ext, 0,0, true);
  try
    while dir.getnextfile(fil) do begin
      sNewName := fil.Name;
      SplitString(sNewName, '.', sLeft, sRight);
      sNewName := sLeft+'.';
      SplitString(sRight,'.', sLeft, sRight);
      sNewName := slash(fil.Path)+sNewName+ inttostr(strtoint(sLeft))+'.'+sRight;
      debug.log('ren: '+fil.fullname+' to '+sNewName);
      RenameFile(fil.fullname, sNewName);
    end;

  finally
    dir.free;
  end;

  debug.log('RUN: '+ffmpeg);
  sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sInputDir+'working.%d.jpg" -b 1000000 -s '+inttostr(w)+'x'+inttostr(h)+' -aspect 16:9 -r '+inttostr(fps)+' "'+sOutputFile+'"';
  debug.log(sParams);
  exe.RunProgramAndWait(ffmpeg, sParams, '');

end;

procedure REbuildVideoFrames_DateOrder_AggSubfolder(sInputDir: string; sOutputFile: string; w,h,fps: nativeint; ext: string = 'jpg');
var
  f,dir: TDirectory;
  fil: TfileInformation;
  sNewName: string;
  sLeft: string;
  sRight: string;
  sPArams: string;
  t,u: integer;
  sTempFOlder: string;
  idx: nativeint;
begin
//ffmpeg -f image2 -i c:working.%d.jpg -b 1000000 -s 720x480 -aspect 16:9 -f mpeg2video o:output.avi
  idx := 0;
  //sTempFolder := adjustpath(sInputdir)+'temp\';
  sTempFolder := 'c:\temp\video\';
  DeleteFolder(sTempFolder);
  forcedirectories(sTempFolder);
  f := TDirectory.create(sINputDir, '*.*', 0,0,true, true, false);
  try
    for u := 0 to f.foldercount-1 do begin
      dir := TDirectory.Create(f.Folders[u].FullName, '*.*', 0,0);
      try
        dir.SortByDate;
        if lowercase(dir.path) <> lowercase(sTempFolder) then
        for t:= 0 to dir.Filecount-1 do begin
          fil := dir.Files[t];
          if fil.Size = 0 then begin
            DeleteFile(fil.FullName);
          end else begin
            CopyFile(pchar(fil.FullName), pchar(sTempFolder+inttostr(idx)+'.jpg'), false);
            inc(idx);
          end;
        end;
      finally
        dir.Free;
      end;

    end;


  finally
    f.Free;
  end;

  Debug.log('RUN: '+ffmpeg_ex());
  //took out aspect 16:9
  if (w>0) and (h>0) then begin
    sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sTempFolder+'%d.jpg" -b 1000000 -s '+inttostr(w)+'x'+inttostr(h)+' -r '+inttostr(fps)+' "'+sOutputFile+'"';
  end else begin
    sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sTempFolder+'%d.jpg" -b 1000000 -r '+inttostr(fps)+' "'+sOutputFile+'"';
  end;

  debug.log(sParams);
  exe.RunProgramAndWait(ffmpeg_ex(), sParams, '',true );
  DeleteFolder(sTempFolder);
end;

procedure REbuildVideoFrames_DateOrder(sInputDir: string; sOutputFile: string; w,h,fps: nativeint; ext: string = 'jpg');
var
  dir: TDirectory;
  fil: TfileInformation;
  sNewName: string;
  sLeft: string;
  sRight: string;
  sPArams: string;
  t: integer;
  sTempFOlder: string;
begin
//ffmpeg -f image2 -i c:working.%d.jpg -b 1000000 -s 720x480 -aspect 16:9 -f mpeg2video o:output.avi

  sTempFolder := slash(sInputdir)+'temp\';
  DeleteFolder(sTempFolder);
  forcedirectories(sTempFolder);
  dir := TDirectory.create(sInputDir, '*.'+ext, 0,0, true);
  dir.SortByDate;
  try
    for t:= 0 to dir.Filecount-1 do begin
      fil := dir.Files[t];
      if (w = 0) or (h = 0) then
        GetImageFileDimensions(dir.Files[t].FullName, w,h);
      if fil.Size = 0 then begin
        DeleteFile(fil.FullName);
      end else begin
        CopyFile(pchar(fil.FullName), pchar(sTempFolder+'temp'+inttostr(t)+'.jpg'), false);
      end;
    end;




//      sNewName := fil.Name;
//      SplitString(sNewName, '.', sLeft, sRight);
//      sNewName := sLeft+'.';
//      SplitString(sRight,'.', sLeft, sRight);
//      sNewName := adjustpath(fil.Path)+sNewName+ inttostr(strtoint(sLeft))+'.'+sRight;
//      debug.log('ren: '+fil.fullname+' to '+sNewName);
//      RenameFile(fil.fullname, sNewName);


  finally
    dir.free;
  end;



  debug.log('RUN: '+ffmpeg_ex());
  //took out aspect 16:9
  if (w>0) and (h>0) then begin
    sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sTempFolder+'temp%d.jpg" -b 1000000 -s '+inttostr(w)+'x'+inttostr(h)+' -r '+inttostr(fps)+' "'+sOutputFile+'"';
  end else begin
    sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sTempFolder+'temp%d.jpg" -b 1000000 -r '+inttostr(fps)+' "'+sOutputFile+'"';
  end;

  debug.log(sParams);
  exe.RunProgramAndWait(ffmpeg, sParams, '',true );
  DeleteFolder(sTempFolder);
end;

type
  TJVIDBS = class(TBetterObject)
  private

  public

    jpg: TJpegIMage;
    jpgf: TStream;
    ms: TMemoryStream;
    q: nativeint;
    HighSize: nativeint;
    LowSize: nativeint;
    function BSFunc(test: int64): nativeint;
    procedure Search(testfirst: nativeint);
    procedure Init;override;
  end;

function TJVIDBS.BSFunc(test: int64): nativeint;
begin
  if test > 100 then begin
    result := 1;
    exit;
  end;
  if test < 1 then begin
    result := -1;
    exit;
  end;

  q := test;

  ms.Free;
  ms := nil;
  ms := Tmemorystream.Create;
  if (jpgf.Size < HighSize) then begin// THIS IS A SPECIAL CASE
                                      // THERE IS NO SENSE in recompressing
                                      //if the native input is already small
                                      //enough.  It'll just make it slower
                                      //and look like shit.
    jpgf.Seek(0,soBeginning);
    ms.CopyFrom(jpgf, jpgf.Size);
    result := 0;
    exit;
  end else begin
    jpgf.Seek(0,soBeginning);
    jpg.LoadFromStream(jpgf);
    jpg.DIBNeeded;
    jpg.dibneeded;
    jpg.CompressionQuality := q;
    jpg.Smoothing := true;
    jpg.Compress;
    jpg.SaveToStream(ms);
    //Debug('q='+inttostr(q)+' size='+inttostr(ms.Size));
  end;

  if q < 10 then
    result := 0
  else
  if (ms.size > LowSize) and (ms.size < HighSize) then begin
    result := 0;
  end else
  if ms.size <= LowSize then
    result := -1
  else
    result := 1;
end;

procedure CreateJVIDFromFrames_JPG(LengthInSeconds: nativefloat; sInputDir: string; sOutFile: string; sINputFilter: string = '*.jpg'; cmd: TCommand = nil);
type
  TlocalStream = TFileStream;
var
  dir: TDirectory;
  sOut: TlocalStream;
  fil: TFileInformation;
  jpgf: TlocalStream;
  jpg: TJpegImage;
  wh: TJpegImage;
  mainhead: TJVIDMainHeader;
  head: TJVIDHeader;
  prevsize: cardinal;
  t: cardinal;
  a: Pint64array;
  bs: TJVIDBS;
  ms: TMemoryStream;
  q: nativeint;
begin
  prevsize := 0;
  t := 0;
  q := 100;
  dir := TDirectory.Create(sInputDir, sInputFilter, 0,0,false, true,false);
  try
    sOut := TlocalStream.Create(sOutFile, fmCreate);
    try
      sOut.Seek(0,0);
      mainhead.Init;
      mainhead.framerate := round(dir.Filecount / lengthinseconds);
      //determine width and height
      wh := TJpegImage.Create;
      try
        dir.filecount;
        wh.LoadFromFile(dir.Files[0].FullName);
        mainhead.display_width := wh.Width;
        mainhead.display_height := wh.Height;
        mainhead.internal_width := wh.Width;
        mainhead.internal_height := wh.Height;


//        mainhead.internal_width := 1 shl (HighOrderBit(mainhead.display_width)+1);
//        mainhead.internal_height := 1 shl (HighOrderBit(mainhead.display_height)+1);
      finally
        wh.Free;
      end;
      mainhead.numberofframes := dir.Filecount;
      stream_guaranteewrite(sOut, @mainhead, sizeof(mainhead));

      a := GetMemory(sizeof(int64) * dir.filecount);
      if (mainhead.internal_width <> mainhead.display_width) and (mainhead.internal_height <> mainhead.display_height) then
        ResizeImages(sINputDir, '*.jpg', mainhead.internal_width, mainhead.internal_height);



      while dir.GetNextFile(fil) do begin
        jpgf := TlocalStream.Create(fil.FullName, fmOpenRead+fmShareDenyWrite);
        try
          jpg := TJPEGImage.Create;
          try
            ms := nil;
            try
              bs := TJVIDBS.Create;
              bs.ms := ms;
              bs.jpgf := jpgf;
              bs.jpg := jpg;
              bs.Search(q);
              q := bs.q;

              if assigned(cmd) then begin
                cmd.StepCount := dir.Filecount;
                cmd.Step := t;
              end;

              ms := bs.ms;

              ms.Seek(0,soBeginning);

              head.FrameNumber := t;
              head.FrameSize := ms.Size;
              head.PrevFrameSize := prevsize;
              head.CalculateChecksum;
              a[t] := sOut.position;
              Stream_guaranteewrite(sOut, @head, sizeof(head));
              sOut.CopyFrom(ms, ms.Size);

              prevsize := head.FrameSize;
              inc(t);
            finally
              ms.Free;
              ms := nil;
            end;
          finally
            jpg.Free;
          end;
        finally
          jpgf.Free;
        end;
      end;
      mainhead.indexoffset := sOut.Position;
      Stream_guaranteewrite(sOut, @a[0], sizeof(int64) * dir.filecount);
      //rewrite the main header to refresh the indexoffset
      sOut.Seek(0,soBeginning);
      stream_guaranteewrite(sOut, @mainhead, sizeof(mainhead));

    finally
      sOut.Free;
    end;
  finally
    dir.Free;
  end;

end;


procedure CreateJVIDFromFrames_Crunch(LengthInSeconds: nativefloat; sInputDir: string; sOutFile: string; sINputFilter: string = '*.jpg'; CrunchFmt: TJVIDFormat = jvfCRN; sCrunchQuality: nativeint = 128; cmd: TCommand = nil);
var
  dir: TDirectory;
  sOut: TMultibufferMemoryFileStream;
  fil: TFileInformation;
  crunchf: TMultibufferMemoryFileStream;
  sCrunchFile: string;
  wh: TJpegImage;
  mainhead: TJVIDMainHeader;
  head: TJVIDHeader;
  prevsize: cardinal;
  t: cardinal;
  a: Pint64array;
  bs: TJVIDBS;
  q: nativeint;
  sCrunchExt: string;
  c: TUnitExe;
begin
  if CrunchFmt = jvfDDS then
    sCrunchExt := 'dds';
  if CrunchFmt = jvfCRN then
    sCrunchExt := 'crn';


  prevsize := 0;
  t := 0;
  q := 100;

  c := TUnitExe.create;
  c.Prog := findtool('crunch_x64.exe');
  c.Params := '/mipMode none /quiet /fileformat '+sCrunchExt+' /quality '+inttostr(sCrunchQuality)+' /outsamedir -file "'+slash(sInputDir)+'*.jpg'+'"';
  c.Hide := true;
//  c.CPUExpense := GEtNumberOfProcessors;
//  c.ConsoleRedirect := false;
  c.Resources.SetResourceUsage('crunch', 1.0);
  c.Start;
  c.WaitFor;
  c.Free;
  c := nil;

  dir := TDirectory.Create(sInputDir, '*.'+sCrunchExt, 0,0,false, true,false);
  try
    sOut := TMultiBufferMemoryFileStream.Create(sOutFile, fmCreate);
    try
      sOut.Seek(0,0);
      mainhead.Init;
      mainhead.framerate := round(dir.Filecount / lengthinseconds);
      mainhead.format := CrunchFmt;
      //determine width and height
      wh := TJpegImage.Create;
      try
        dir.filecount;
        wh.LoadFromFile(changefileext(dir.Files[0].FullName,'.jpg'));
        mainhead.display_width := wh.Width;
        mainhead.display_height := wh.Height;
      finally
        wh.Free;
      end;
      mainhead.numberofframes := dir.Filecount;
      stream_guaranteewrite(sOut, @mainhead, sizeof(mainhead));

      a := GetMemory(sizeof(int64) * dir.filecount);

      while dir.GetNextFile(fil) do begin
        sCrunchFile := fil.FullName;

        crunchf := TMultibufferMemoryFileStream.Create(sCrunchFile, fmOpenRead+fmShareDenyWrite);
        try

          if assigned(cmd) then begin
            cmd.StepCount := dir.Filecount;
            cmd.Step := t;
          end;


          crunchf.Seek(0,soBeginning);

          head.FrameNumber := t;
          head.FrameSize := crunchf.Size;
          head.PrevFrameSize := prevsize;
          head.CalculateChecksum;
          a[t] := sOut.position;
          Stream_guaranteewrite(sOut, @head, sizeof(head));
          sOut.CopyFrom(crunchf, crunchf.Size);

          prevsize := head.FrameSize;
          inc(t);
        finally
          crunchf.Free;
        end;
      end;
      mainhead.indexoffset := sOut.Position;
      Stream_guaranteewrite(sOut, @a[0], sizeof(int64) * dir.filecount);
      //rewrite the main header to refresh the indexoffset
      sOut.Seek(0,soBeginning);
      stream_guaranteewrite(sOut, @mainhead, sizeof(mainhead));

    finally
      sOut.Free;
    end;
  finally
    dir.Free;
  end;

end;

procedure ConformVideoFiles(sOutputDir: string; bPad: boolean; bDeleteDuplicates: boolean);
var
  soutputFile: string;
  t: integer;
  dir: TDirectory;
  fil: TFileInformation;
  sLeft, sRight: string;
  sNewName: string;
begin

  forcedirectories(sOutputDir);
  sOutputFile := slash(sOutputDir)+'working.%d.'+extract_image_ext;
  dir := TDirectory.create(sOutputDir, '*.'+extract_image_ext, 0,0, true);
  try
    while dir.getnextfile(fil) do begin
      sNewName := fil.Name;
      SplitString(sNewName, '.', sLeft, sRight);
      sNewName := sLeft+'.';
      SplitString(sRight,'.', sLeft, sRight);
      if bPAd then begin
        sNewName := slash(fil.Path)+sNewName+ padstring(sLeft,'0', 15)+'.'+sRight;
      end else begin
        sNewName := slash(fil.Path)+sNewName+ inttostr(strtoint(sLeft))+'.'+sRight;
      end;
      if (sNewName <> fil.fullname) then begin
        if bDeleteDuplicates and FileExists(sNewName) then begin
          DeleteFile(fil.FullName);
        end else begin
          RenameFile_Verify(fil.fullname, sNewName);
        end;
      end;
    end;

  finally
    dir.free;
  end;


end;

function BeginExtractVideoFrames(sInputFile: string; sOutputDir: string; w: nativeint = 0; h: nativeint = 0): Tcmd_FFMPEG;
var
  soutputFile: string;
  t: integer;
  dir: TDirectory;
  fil: TFileInformation;
  sLeft, sRight: string;
  sNewName: string;
  sDir: string;
  cexe: Tcmd_FFMPEG;
  sDim: string;
begin




//  exe.RunProgramAndWait(ffmpeg, '-i "'+sInputFile+'" -y -f image2 -b 100000 -s '+sDim+' "'+sOutputFIle+'"', extractfilepath(ffmpeg), true);

  dir := TDirectory.create(sOutputDir, '*.'+extract_image_ext,0,0,false);
  try
    for t:= 0 to dir.Filecount-1 do begin
      DeleteFile(dir.files[t].fullname);
    end;
  finally
    dir.free;
  end;



  cexe := Tcmd_FFMPEG.Create;
  cexe.BleedingEdge := beStable;
  cexe.WorkingDir := extractfilepath(ffmpeg);
  cexe.OutputDIrectory :=   sOutputDir;
  cexe.InputFile := sInputFile;
  cexe.Action := ffExtractFrames;
  cexe.width := w;
  cexe.height := h;
  cexe.Hide := true;
  cexe.Start;

  result := cexe;

end;

procedure EndExtractVideoFrames(cexe: Tcmd_FFMPEG);
var
  dir: TDirectory;
  t: nativeint;
  fil: TFileInformation;
  sNewName: string;
  sLeft, sRight: string;
begin

  cexe.WaitFor;


  //pad all the frame numbers

  dir := TDirectory.create(cexe.outputdirectory, '*.'+extract_image_ext, 0,0, true);
  try
    while dir.getnextfile(fil) do begin
      sNewName := fil.Name;
      SplitString(sNewName, '.', sLeft, sRight);
      sNewName := sLeft+'.';
      SplitString(sRight,'.', sLeft, sRight);
      sNewName := slash(fil.Path)+sNewName+ padstring(sLeft,'0', 5)+'.'+sRight;
      RenameFile(fil.fullname, sNewName);
    end;

  finally
    dir.free;
  end;

  cexe.free;
  cexe := nil;


end;
procedure ExtractVideoFrames(sInputFile: string; sOutputDir: string; w: nativeint = 0; h: nativeint = 0);
begin
  EndExtractVideoFrames(beginExtractVideoFrames(sInputfile, soutputDir,  w,h));
end;


{ Tcmd_XBOXtranscode }

procedure Tcmd_XBOXtranscode.BuildCommandLine;
begin
  inherited;
  //d:\source\DMX_O_Matic\ffmpeg.exe -i "h:\archives\tv\Workaholics\Episode 1 - Piss & Shit.mkv" -f mp4 -vcodec mpeg4 -b 2000000 -acodec libfaac -ac 2 -ab 128000 i:\tv\test.mp4
  Prog := ffmpeg;
  Params := '-y -i "'+finfile+'" -f mp4 -vcodec mpeg4 -b 2000000 -acodec libfaac -ac 2 -ab 128000 "'+foutfile+'"';
  Hide := true;

  //GLOG.LogToDisk(Prog+' '+Params);
end;
procedure Tcmd_XBOXtranscode.Init;
begin
  inherited;
  Icon := @CMD_ICON_REEL;
end;

procedure Tcmd_XBOXtranscode.InitExpense;
begin
  inherited;
  CPUExpense := 1.0;
end;

function BeginTranscodeForXBOX(sFile: string; sOutFile:string): Tcmd_XBOXtranscode;
begin
  result := Tcmd_XBOXtranscode.Create;
  result.infile := sfile;
  result.outfile := sOutfile;
  result.start;

end;
procedure EndTranscodeForXbox(c: Tcmd_XBOXtranscode);
begin
  c.WaitFor;
  c.Free;
  c := nil;
end;



{ Tcmd_FFMPEG }


{ Tcmd_FFMPEG }

procedure Tcmd_FFMPEG.AfterConstruction;
begin
  inherited;
  outputFile := '';//sets resource usage
end;

procedure Tcmd_FFMPEG.BuildCommandLine;
var
  sDim: string;
  br: string;
  p: TFFProbe;
  sThreads: string;
begin
  inherited;

  Name := extractFileName(outputfile);
  ConsoleRedirect := true;
  Hide := true;
  var accel := '';//'-hwaccel dxva2 ';
  if ffmpeg_cpus(1) >= 8 then
    accel := '';

  if lowercase(extractfileext(InputFile)) = '.webm' then
    BleedingEdge := beSTable;

  case bleedingedge of
    beAncient: prog := ffmpeg;
    beStable: prog := ffmpeg_ex();
    be4kEra: prog := ffmpeg_ex('_4k');
  end;

  p.init;
  if action <> ffProbe then
    p := ffMpegProbe(InputFile);

  if (Width > 0) and (Height > 0) then
    sDim := ' -s '+inttostr(Width)+'x'+inttostr((height shr 1) shl 1);

  var AUDIO51_CORRECTION := '';

  sThreads := ' ';//' -threads '+ffmpeg_cpus.tostring+' ';

  case Action of
    ffProbe: begin
      Params := '-i "'+InputFile+'"';
      CPuExpense := 0.0;
    end;
    ffExtractFrame: begin
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -ss 00:05:00.000 -y -r 30 -f image2 -q:a 0 -q:v 0 -b 100000'+sDim+' -vframes 1 "'+OutputFIle+'"';
        GPUParams := '-hwaccel_device ##gpu## -hwaccel cuda '+Params;
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -y -f image2 -sameq '+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffExtractFrames: begin
      OutputFile := slash(OutputDirectory)+'working.%d.'+extract_image_ext;
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -y -r 30 -f image2 -q:a 0 -q:v 0 -b 100000'+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -y -f image2 -sameq '+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffToDMXVideo: begin
//      OutputFile := slash(OutputDirectory)+'working.%d.'+extract_image_ext;
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -q:a 0 -q:v 0 -r 30 '+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffSwapCR: begin
      if outputfile <> '' then begin
        forcedirectories(extractfilepath(outputfile));
      end;
      if p.Is5_1 then begin
        if p.AudioCodec = 'eac3' then begin
          Params := accel+'-y'+sThreads+'-i "'+InputFile+'" -filter_complex "channelmap=map=FL-FL|FC-FC|FR-FR|LFE-LFE|SL-SL|SR-SR:channel_layout=5.1" -vcodec copy "'+OutputFile+'"';
        end else
          Params := accel+'-y'+sThreads+'-i "'+InputFile+'" -filter_complex "channelmap=map=FL-FL|FC-FC|FR-FR|LFE-LFE|BL-BL|BR-BR:channel_layout=5.1" -vcodec copy "'+OutputFile+'"';
      end else
      if p.Is7_1 then begin
        Params := accel+'-y'+sThreads+'-i "'+InputFile+'" -filter_complex "channelmap=map=FL-FL|FR-FC|FC-FR|LFE-LFE|SL-SL|SR-SR|BL-BL|BR-BR:channel_layout=7.1" -acodec copy -vcodec copy "'+OutputFile+'"';
      end;
      if outputfile <> '' then begin
        forcedirectories(extractfilepath(outputfile));
        SaveStringAsFile(outputfile+'.txt',params+NL+gpuparams);
      end;
    end;
    ffToChromeCastOQ: begin
      //D:\source\Pascal\64\MovieMaker\..\..\tools\ffmpeg_4k.exe -re
      //-i "\\192.168.33.3\d$\media\_movies\Red Sparrow\Red.Sparrow.2018.2160p.BluRay.x264.8bit.SDR.DTS-HD.MA.TrueHD.7.1.Atmos-SWTYBLZ.mkv"
      //-c:v libx265 -preset veryslow -vf "scale=3840:2160" -b:v 12M -sc_threshold 0
      //"\\192.168.33.3\d$\media\_movies\Red Sparrow\oq\oq.Red.Sparrow.2018.2160p.BluRay.x264.8bit.SDR.DTS-HD.MA.TrueHD.7.1.Atmos-SWTYBLZ_2.mkv.mp4"
      if (p.w > 1920) then begin
        if outputfile <> '' then begin
          forcedirectories(extractfilepath(outputfile));
        end;

        sDim := '';
        if FAlternate then changefileext(OutputFile, '.webm');
        if BleedingEdge <> beAncient then begin
          Params := '-y'+sThreads+'-i "'+InputFile+'" -q:a 0 -q:v 0 -c:v libx265 -crf 20 -preset ultrafast -level:v 5.1 -pix_fmt yuv420p -movflags faststart -acodec '+ACODEC_4k+' '+sDim+' -movflags faststart  "'+OutputFile+'"';
          GPUParams := '-hwaccel_device ##gpu## -hwaccel cuda  -y '+sThreads+
                      '-i "'+InputFile+'" -q:a 0 -q:v 0 -c:v hevc_nvenc -crf 20 -b:v 15M -maxrate:v 20M -rc:v vbr_hq -cq:v 10 -gpu ##gpu## -level:v 5.1 -pix_fmt yuv420p -movflags faststart -acodec '+ACODEC_4k+' '+sDim+' -movflags faststart  "'+OutputFile+'"';
(*
e:ffmpeg_4k.exe -hwaccel_device 1 -hwaccel cuda -y -i "\\192.168.101.123\media\_movies\Wreck-It Ralph\Wreck-It Ralph.mkv" -t 1:00 -q:a 0 -q:v 0 -c:v hevc_nvenc -rc:v vbr_hq -cq:v 10 -gpu 1 -level:v 5.1 -pix_fmt yuv420p -movflags faststart -acodec mp3  -movflags faststart  "\\192.168.101.123\media\_movies\Wreck-It Ralph\oq\oq.Wreck-It Ralph.mp4"
*)



        end else begin
  //        Params := '-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
        end;
        if outputfile <> '' then begin
          forcedirectories(extractfilepath(outputfile));
          SaveStringAsFile(outputfile+'.txt',params+NL+gpuparams);
        end;
      end;
    end;
    ffToChromeCastUQ: begin
      if (p.w > 1920) then begin
        if outputfile <> '' then begin
          forcedirectories(extractfilepath(outputfile));
        end;

        sDim := '';
        if FAlternate then changefileext(OutputFile, '.webm');
        if BleedingEdge <> beAncient then begin
          Params := accel+'-y'+sThreads+'-i "'+InputFile+'" -c:v libx264 -profile:v high -level:v 5.2 -pix_fmt yuv420p -preset ultrafast -acodec '+ACODEC_4k+' '+sDim+' "'+OutputFile+'"';
          GPUParams := '-hwaccel_device ##gpu## -hwaccel cuda  -y '
                       +sThreads+'-i "'+InputFile+'" -q:a 0 -q:v 0 -c:v h264_nvenc -crf 20 -rc:v vbr_hq -cq:v 10 -b:v 15M -maxrate:v 20M -gpu ##gpu## -level:v 5.1 -pix_fmt yuv420p -movflags faststart -acodec '+ACODEC_4k+' '+sDim+' -movflags faststart  "'+OutputFile+'"';

        end else begin
  //        Params := '-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
        end;
        if outputfile <> '' then begin
          forcedirectories(extractfilepath(outputfile));
          SaveStringAsFile(outputfile+'.txt',params+NL+gpuparams);
        end;
      end;

    end;
    ffToChromeCastHQ: begin
      if outputfile <> '' then begin
        forcedirectories(extractfilepath(outputfile));
        SaveStringAsFile(outputfile+'.txt',params+NL+gpuparams);
      END;
      if ((p.AudioCodec = 'eac3') or (p.audiocodec = 'ac3'))
      and (p.Is5_1) then begin//-filter_complex "channelmap=map=FL-FL|FC-FC|FR-FR|LFE-LFE|SL-SL|SR-SR:channel_layout=5.1"
        AUDIO51_CORRECTION := ' -af "channelmap=0|1|2|3|4|5:5.1"';
      end;
      if ((p.AudioCodec = 'eac3') or (p.audiocodec = 'ac3'))
      and (p.Is7_1) then begin
        AUDIO51_CORRECTION := ' -af "channelmap=0|1|2|3|4|5|6|7:7.1"';
      end;


      if (sDim = '') and (p.w > 1920) then begin
        sDim := '-s 1920x'+inttostr(round(1920*(p.h/p.w)));
      end;
      if FAlternate then changefileext(OutputFile, '.webm');
      if BleedingEdge <> beAncient then begin
        Params := ' -y '+sThreads+'-i "'+InputFile+'" '+AUDIO51_CORRECTION+' -q:a 0 -q:v 3 -vcodec h264 -pix_fmt yuv420p -acodec '+ACODEC_Safe+AUDIO51_CORRECTION+' '+sDim+' "'+OutputFIle+'"';
        GPUParams := '-hwaccel_device ##gpu## -hwaccel cuda  -y '+sThreads+'-i "'+InputFile+'" '+AUDIO51_CORRECTION+' -q:a 0 -q:v 3 -vcodec h264_nvenc -gpu ##gpu## -b:v 15M -maxrate:v 20M -pix_fmt yuv420p -acodec '+ACODEC_Safe+AUDIO51_CORRECTION+' '+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
      end;
      try
        SaveStringAsFile(outputfile+'.txt',params+NL+gpuparams);
      except
      end;
    end;
    ffToOculus: begin
      if FAlternate then changefileext(OutputFile, '.webm');
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -q:a 0 -q:v 0 -s 3840x1080 -acodec '+ACODEC_SAFE+AUDIO51_CORRECTION+' '+sDim+' "'+OutputFIle+'"';
        GPUParams := '-hwaccel_device ##gpu## -hwaccel cuda  -y '+sThreads+'-i "'+InputFile+'" -q:a 0 -q:v 0 -s 3840x1080 -acodec '+ACODEC_SAFE+AUDIO51_CORRECTION+' '+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffToChromeCastLQ: begin
      if outputfile <> '' then begin
        forcedirectories(extractfilepath(outputfile));
        SaveStringAsFile(outputfile+'.txt',params+NL+gpuparams);
      END;

      if (p.AudioCodec = 'eac3')
      and (p.Is5_1) then begin
//        AUDIO51_CORRECTION := ' -filter_complex "channelmap=map=FL-FL|FR-FC|FC-FR|LFE-LFE|SL-BL|SR-BR:channel_layout=5.1"';
      end;
      if (p.AudioCodec = 'eac3')
      and (p.Is7_1) then begin
//        AUDIO51_CORRECTION := ' -filter_complex "channelmap=map=FL-FL|FR-FC|FC-FR|LFE-LFE|BL-BL|BR-BR|SL-SL|SR-SR:channel_layout=7.1"';
      end;

      if (sDim = '') and (p.w > 1920) then begin
        sDim := '-s 1920x'+inttostr(round(1920*(p.h/p.w)));
      end;
      if FAlternate then changefileext(OutputFile, '.webm');
      if BleedingEdge <> beAncient then begin
        Params := accel+'-y'+sThreads+'-i "'+InputFile+'"'+AUDIO51_CORRECTION+' -c:v libx264 -crf 20 -maxrate 300k -acodec '+ACODEC_SAFE+' -bufsize 1834k '+sDim+' "'+OutputFIle+'"';
        GPUParams := '-hwaccel_device ##gpu## -hwaccel cuda  -y '+sThreads+'-i "'+InputFile+'" -q:a 0 -q:v 0 -s 3840x1080 -acodec '+ACODEC_SAFE+AUDIO51_CORRECTION+' '+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffToMp3: begin
      if lowercase(extractfileext(INputFile)) = '.flv' then begin
        br := '128k -ar 44100'
      end else begin
        br := '256k'
      end;

      //if bUseAsyncFlag then
      Params := '-y'+sThreads+'-vsync 1 -async 1 -ab '+br+' -i "' + INputFile + '" "' + OutputFile + '"'
      //else
      //  result.Params := '-y -vsync 1 -ab '+br+' -i "' + sFile + '" "' + sOutFile + '"';

    end;
  end;


{$IFDEF CONSOLE}
  Writeln('');
  Writeln('Command-line: '+self.prog+' '+self.params);
  Writeln('');
  Writeln('');
{$ENDIF}

end;




procedure Tcmd_FFMPEG.CC(ccStatus: TConsoleCaptureStatus; sData: string);
var
  sLeft: string;
  sRight: string;
  timeh: IHolder<TStringList>;

  f: single;
  t: ni;
  function DurationToMS(s: string): int64;
  begin
    result := 0;
        timeh := ParseStringH(sLeft, ':');
        if timeh.o.count = 3 then begin
          f := strtofloat(timeh.o[2]);
          result := round(f*1000);
          result := result + (strtoint(timeh.o[1])*60*1000);
          result := result + (strtoint(timeh.o[0])*60*60*1000);
        end;
  end;
begin
  inherited CC(ccStatus, sData);
  if (FTotalFrames = 0) and (FDurationInMS=0) then begin
    if SplitString(sData, 'NUMBER_OF_FRAMES:', sLeft, sRight) then begin
      If SplitString(sright, #13, sLeft, sright) then begin
        sLEft := trim(sLeft);
        if IsInteger(sLeft) then
          FTotalFrames := strtoint(sLeft);
          StepCount := FtotalFrames;
      end;
    end else
    if SplitString(sData, 'NUMBER_OF_FRAMES-eng:', sLeft, sRight) then begin
      If SplitString(sright, #13, sLeft, sright) then begin
        sLEft := trim(sLeft);
        if IsInteger(sLeft) then
          FTotalFrames := strtoint(sLeft);
          StepCount := FtotalFrames;
      end;
    end else
    if SplitString(sData, 'Duration: ', sLeft, sRight) then begin
      If SplitString(sright, ',', sLeft, sright) then begin
        sLEft := trim(sLeft);

        try
          FDurationInMS :=  DurationToMS(sLeft);
          StepCount := FDurationInMS;
        except
        end;

      end;
    end;
  end;

  if FTotalFrames > 0 then begin
    if SplitString(sData, 'frame=', sLeft, sRight) then begin
      If SplitString(sright, ' fps', sLeft, sright) then begin
        sLeft := trim(sLeft);
        if IsInteger(sLeft) then
          Step := strtoint(trim(sLeft));

      end;
    end;
  end else
  if FdurationInMS > 0 then begin
    if SplitString(sData, 'time=', sLeft, sRight) then begin
      If SplitString(sright, ' bitrate', sLeft, sright) then begin
        sLeft := trim(sLeft);
        try
          Step := DurationToMS(sLeft);
        except
        end;

      end;
    end;
  end;



end;

constructor Tcmd_FFMPEG.Create;
begin
  inherited;

end;

procedure Tcmd_FFMPEG.DoExecute;
begin
  inherited;
//  if dir.GetFileSize(OutputFile) = 0 then begin
//    deletefile(outputfile);
//    FAlternate := true;
//    inherited;
//  end;
//{$IFNDEF REXE}
    try
      if outputfile <> '' then begin
        forcedirectories(extractfilepath(outputfile));
        SaveStringAsFile(outputfile+'.log',consoleoutput);
      end;
    except
    end;
//{$ENDIF}

end;

procedure Tcmd_FFMPEG.Init;
begin
  inherited;
end;

procedure Tcmd_FFMPEG.InitExpense;
begin
  inherited;
  Timeout := (60000*60)*24;//24-hour timeout
  Icon := @CMD_ICON_REEL;

end;


procedure Tcmd_FFMPEG.OnStart;
begin
  inherited;
  if inputfile = '' then
    raise Ecritical.create('no input file!');
end;

procedure Tcmd_FFMPEG.PostProcess;
var
  dir: TDirectory;
  t: nativeint;
  fil: TFileInformation;
  sNewName: string;
  sLeft, sRight: string;
begin

   Status := 'PostProcesss';
//  dir := TDirectory.create(OutputDirectory, '*.'+extract_image_ext,0,0,false);
//  try
//    for t:= 0 to dir.Filecount-1 do begin
//      DeleteFile(dir.files[t].fullname);
//    end;
//  finally
//    dir.free;
//  end;

  //pad all the frame numbers

  dir := TDirectory.create(OutputDirectory, '*.'+extract_image_ext, 0,0, true);
  try
    while dir.getnextfile(fil) do begin
      sNewName := fil.Name;
      SplitString(sNewName, '.', sLeft, sRight);
      sNewName := sLeft+'.';
      SplitString(sRight,'.', sLeft, sRight);
      sNewName := slash(fil.Path)+sNewName+ padstring(sLeft,'0', 5)+'.'+sRight;
      RenameFile_Verify(fil.fullname, sNewName);
    end;

  finally
    dir.free;
  end;

  inherited;
end;

procedure Tcmd_FFMPEG.PreProcess;
begin
  inherited;
  Status := 'PreProcess';

  case ACtion of
    //when extracting frames, delete old files first
    ffExtractFrames: begin


      DeleteFolder(OutputDirectory);


      while not directoryexists(OutputDirectory) DO begin
        Status := 'not DirectoryExists '+OutputDirectory;
        deletefile(unslash(OutputDirectory));
        if pos(':', OutputDirectory) > 0 then begin
          forcedirectories(OutputDirectory);
        end else begin
          forcedirectories(dllpath+OutputDirectory);
        end;

        //mkdir(sDir);
      end;
    end;
  end;

  if fileexists(OutputFile) then begin
    if not OverwriteOutput then
      Cancel;
  end;

end;

procedure Tcmd_FFMPEG.SEtOutputFile(const Value: string);
begin
  Resources.SetResourceUsage('ffmpeg '+Foutfile, 0.0);
  Foutfile := Value;
  Resources.SetResourceUsage('ffmpeg '+Foutfile, 1.0);
end;

{ Tcmd_Mobiletranscode }

procedure Tcmd_Mobiletranscode.BuildCommandLine;
begin
  inherited;
  //d:\source\DMX_O_Matic\ffmpeg.exe -i "h:\archives\tv\Workaholics\Episode 1 - Piss & Shit.mkv" -f mp4 -vcodec mpeg4 -b 2000000 -acodec libfaac -ac 2 -ab 128000 i:\tv\test.mp4
  Prog := ffmpeg;
//  Params := '-y -i "'+finfile+'" -f mp4 -vcodec mpeg2 -s 850x480 -b 2000000 -acodec libfaac -ac 2 -ab 128000 "'+foutfile+'"';
  Params := '-y -i "'+finfile+'" -s 850x480 -ac 2 "'+foutfile+'"';
  Hide := true;

  Debug.Log(Prog+' '+Params);

end;

procedure Tcmd_Mobiletranscode.InitExpense;
begin
  inherited;
  CPUExpense := 1.0;
  Icon := @CMD_ICON_REEL;
end;

{ TJVIDHeader }

procedure TJVIDHeader.CalculateChecksum;
begin
  CheckSum := FrameSize xor PrevFrameSize xor FrameNumber;
end;

function TJVIDHeader.VerifyChecksum: boolean;
begin
  result := Checksum = (FrameSize xor PrevFrameSize xor FrameNumber);
end;

{ TJVIdReader }

procedure TJVIdReader.Cleanup;
begin
  if framememory <> nil then begin
    FreeMemory(framememory);
    FrameMemory := nil;
  end;

end;

procedure TJVIdReader.CleanupMain;
begin
  if index <> nil then
    FreeMemory(index);
  index := nil;

end;

procedure TJVIdReader.COmpleteCommand;
begin
  while not PrefetchAckFlag = 1 do
    sleep(0);

//  if assigned(cmdNextFRame) then begin
//    cmdNExtFrame.waitfor;
//    cmdNExtFrame.free;
//    cmdNExtFrame := nil
//  end;

end;

destructor TJVIdReader.Destroy;
begin
  StopPrefetchThread;
  COmpleteCommand;
  Cleanup;

  stream.Free;
  stream := nil;

  inherited;
end;

function TJVIdReader.FileName: string;
begin
  if stream = nil then
    result := ''
  else
    result := stream.FileName;
end;

function TJVIdReader.FramePOinterAt: nativeint;
begin
  result := FrameNumber + 1;
end;

function TJVIdReader.GetStreamFromFrameMemory: TExternalMemoryStream;
begin
  result := TExternalMemoryStream.Create;
  result.SetExternalPointer(framememory, framememorylength);
//  stream_guaranteewrite(result, framememory, framememorylength);
//  result.Seek(0,soBeginning);



end;

procedure TJVIdReader.GotoFrame(iNumber: nativeint);
var
  tm,tm2: cardinal;
begin
  try
    if iNumber = framenumber then
      exit;

    if iNumber >= nativeint(self.mainheader.numberofframes) then
      exit;


    if stream = nil then
      exit;

    if iNumber >= nativeint(mainheader.numberofframes) then
      exit;
    if iNumber < 0 then
      exit;


    tm := GetTicker;
    try
      stream.seek(index[iNumber], soBeginning);
      ReadCurrentFrame;
    finally
      tm2 := GEtTimeSince(tm);
      if tm2 > (1000/30) then begin
        Debug.log('Frame grab time: '+inttostr(tm2));
      end;
    end;
  finally
    //stream.BeginFlushAndPrefetch(random(5000000));
  end;

//  if iNumber < FrameNumber then begin
//    PrevFrame(FrameNumber-iNumber);
//  end else begin
//    NExtFrame(iNumber-FrameNumber);
//  end;

end;

procedure TJVIdReader.GotoTime(rTime: nativefloat);
var
  i: nativeint;
begin
  i := round(rTime * mainheader.framerate);
  GotoFrame(i);


end;

procedure TJVIdReader.Init;
begin
  inherited;
  StartPrefetchThread;
end;

procedure TJVIdReader.LoadFromFile(sFile: string);
var
  iPos: int64;
begin
  CleanupMain;
  Cleanup;

  FrameNumber := -1;
  if not fileexists(sFile) then
    exit;

  stream := TJVIDFileStream.Create(sfile, fmOpenRead+fmShareDenyWrite);
//  stream.buffersize := sizeof(header);
{$IFDEF USE_MFS}
  stream.buffersize := 100000000;
  stream.OptimizeForBinarySearch;
{$ENDIF}
  stream.Seek(0,0);
  Stream_GuaranteeRead(stream, @mainheader, sizeof(mainheader));
  index := GetMemory(mainheader.numberofframes* sizeof(int64));
  stream.Seek(mainheader.indexoffset, soBeginning);
  Stream_GuaranteeRead(stream, @index[0], sizeof(int64)*mainheader.numberofframes);
  OnNewVideo;
  GotoFrame(0);

end;

procedure TJVIdReader.NextFrame(iCount: integer = 1);
begin
  if FramePOinterAt >= nativeint(mainheader.numberofframes) then
    exit;

{$IFDEF USE_MFS}
  if iCount > 300 then
    stream.buffersize := sizeof(header)
  else
    stream.BufferSize := 1000000;
{$ENDIF}

  while iCount > 1 do begin
    ReadCurrentFrameHeader;
    stream.Seek(header.FrameSize, soCurrent);
    inc(framenumber);
    dec(iCount);
  end;

  ReadCurrentFrame;
  inc(framenumber);
  //ReadCurrentFrame;

end;

procedure TJVIdReader.OnNewVideo;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TJVIdReader.PrefetchNextFrame;
begin
  PrefetchAckFlag := 0;
//  PrefetchRequestFlag := 1;
//  cmdNextFRame := Tcmd_NextFrame.Create;
//  cmdNextFrame.Reader := self;
//  cmdNextFrame.Start;
end;

procedure TJVIdReader.PrefetchThread_OnExecute(sender: TExternalEventThread);
begin
  if PrefetchRequestFlag = 1 then begin
    ///sender.SetMM('Pro Audio');
    NExtFrame;
    PrefetchRequestFlag := 0;
    PrefetchAckFlag := 1;
  end;

end;

procedure TJVIdReader.PrevFrame(iCount: integer = 1);
var
  i: int64;
begin
  //first lets rewind to the beginning of the current frame
  i := (header.FrameSize+sizeof(header));
  i := 0-i;
  stream.Seek(i, soCurrent);

  while iCount > 0 do begin
    i := (header.PrevFrameSize+sizeof(header));
    i := 0-i;
    stream.Seek(i, soCurrent);
    ReadCurrentFrameHeader;
    dec(framenumber);
    dec(iCount);

    if iCount > 0 then
      stream.Seek(0-sizeof(header), soCurrent);



  end;

  ReadCurrentFrameBuffer;
end;

procedure TJVIdReader.ReadCurrentFrame;
begin
  ReadCurrentFrameHeader;
  ReadCurrentFrameBuffer;
end;

procedure TJVIdReader.ReadCurrentFrameBuffer;
begin
  Cleanup;
  framememory := GetMemory(header.FrameSize);
  framememorylength := header.FrameSize;
  Stream_GuaranteeRead(stream, @framememory[0], header.FrameSize);
end;

procedure TJVIdReader.ReadCurrentFrameHeader;
begin
  Stream_GuaranteeREad(stream, @header, sizeof(header));
end;

procedure TJVIdReader.StartPrefetchThread;
begin
  thrPrefetch := TPM.NeedThread<TExternalEventThread>(nil);
  //thrPrefetch.Loop := true;
  thrPRefetch.OnExecute := self.PrefetchThread_OnExecute;

  thrPrefetch.Start;
  thrPrefetch.safeResume;
end;

procedure TJVIdReader.StopPrefetchThread;
begin
  thrPrefetch.WaitForFinish;
  TPM.NoNeedThread(thrPrefetch);
  thrPrefetch := nil;

end;

function TJVIdReader.TimeToFrameNumber(rTime: nativefloat): nativeint;
begin
  result := round(rTime * mainheader.framerate);
end;

{ TJVIDMainHeader }

procedure TJVIDMainHeader.Init;
begin
  fourcc[0] := 'J';
  fourcc[1] := 'V';
  fourcc[2] := 'I';
  fourcc[3] := 'D';
  version := 1;
end;

procedure VideoToJVid(sInputFile: string; sOutputFile: string; cmd: TCommand = nil; fmt: TJVIDFormat = jvfJPG);
var
  sFile, sIn, sTemp2, sTemp, sOut, smp3, sboog: string;
  mainhead: TJVIDMainHeader;
  bh: TBoogerHeader;
  ss: TSoundStream;
  iLen: nativefloat;
  c: Tcmd_RunExe;
begin
  iLen := 0;
  if assigned(cmd) then cmd.Stepcount := 5;
  sFile := sInputFile;
  sIn := sInputFile;
  sTemp2 := GetTempPath+'videoproc\'+inttostr(GetCurrentThreadID())+'\';
  forcedirectories(sTemp2);
  copyfile(pchar(sIn), pchar(sTemp2+extractfilename(sIn)), false);
  sIn := pchar(sTemp2+extractfilename(sIn));
  sTemp := GetTempPath+'videoproc\'+inttostr(GetCurrentThreadID())+'\frames\';
  forcedirectories(sTemp);
  try
    sOut := sOutputFile;
    //1---------------------------------------------------------
    if assigned(cmd) then cmd.Step := 1;
    sMp3 := sIn+'.mp3';
    c := BeginVideoToMp3(sIn, false, sMp3);
    ffmpeg_tools.ExtractVideoFrames(sIn, sTemp);
  //  ffmpeg_tools.ConformVideoFiles(sTemp, true, true);
    //2---------------------------------------------------------
    if assigned(cmd) then cmd.Step := 2;
    mainhead.Init;

    EndVideoToMp3(c);
    //3---------------------------------------------------------
    if assigned(cmd) then cmd.Step := 3;
    ss := TSoundStream.Create(sMp3, fmOpenRead+fmShareDenyWrite);
    try
      if ss.samplerate = 0 then
        raise Exception.Create('could not determine sample rate of mp3:'+sMp3);
      iLen := ss.SampleCount / ss.samplerate;
    finally
      ss.Free;
    end;


    case fmt of
      jvfJPG: ffmpeg_tools.CReateJVidFromFrames_JPG(iLen, sTemp, sOut, '*.jpg', cmd);
      jvfCRN: ffmpeg_tools.CReateJVidFromFrames_Crunch(iLen, sTemp, sOut, '*.jpg', fmt, 128, cmd);
      jvfDDS: ffmpeg_tools.CReateJVidFromFrames_Crunch(iLen, sTemp, sOut, '*.jpg', fmt, 255, cmd);
    end;

    //5---------------------------------------------------------
    if assigned(cmd) then cmd.Step := 5;

    copyfile(pchar(sMp3), pchar(changefileext(sOut, '.mp3')), false);
  finally
    DeleteFolder(sTemp2);
  end;
end;


procedure VideoToDmxVideo(sInputFile: string; sOutputFile: string; cmd: TCommand = nil);
var
  sFile, sIn, sTemp2, sTurb, sTemp, sHue, sOut, smp3, sboog: string;
  sFinalhue, sFinalturb, sfinalmp3, sFinalmp4: string;
  sLckFile: string;
  bNeedsTurb, bNeedsHue, bNeedsMp3, bNeedsmp4, bNeedsFrames: boolean;
  mainhead: TJVIDMainHeader;
  bh: TBoogerHeader;
  ss: TSoundStream;
  iLen: nativefloat;
  c: Tcmd_RunExe;
  c2: TCmd_FFmpeg;
  c3: Tcmd_RenderHuestogram;
  turb: TCmd_GEnerateTurbulenceData;
  fsLck: TFileStream;
  dtLck: TDateTime;
begin
  c3 := nil;
  c2 := nil;
  fsLck := nil;
  try
    try
      sFinalHue := sOutputFile+'.hue';
      sFinalTurb := sOutputFile+'.turb';
      sFinalMp3 := sOutputFile+'.mp3';
      sFinalMp4 := sOutputFile+'.mp4';
      sLckFile := sOutputFile+'.lck';
      bNeedsMp3 := not fileexists(sFinalMp3);
      bNeedsMp4 := not fileexists(sFinalMp4);
      bNeedsTurb := not fileexists(sFinalTurb);
      bNeedsHue := not fileexists(sFinalHue);
      if bNeedsHue then bNeedsMp3 := true;
      bNeedsFRames := bNeedsHue;

      if not (bNeedsMp3 or bNeedsMp4 or bNeedsHue or bNeedsTurb or bNeedsFrames)
      then
        exit;

      //get lock
      try

        if fileexists(sLckFile) then begin
          dtLck := FileAge(sLckFile);
          deletefile(sLckFile);

          if FileExists(sFinalHue) and (FileAge(sFinalHue) >= dtLck) then deletefile(sFinalHue);
          if FileExists(sFinalTurb) and (FileAge(sFinalTurb) >= dtLck) then deletefile(sFinalTurb);
          if FileExists(sFinalMp3) and (FileAge(sFinalMp3) >= dtLck) then deletefile(sFinalMp3);
          if FileExists(sFinalMp4) and (FileAge(sFinalMp4) >= dtLck) then deletefile(sFinalMp4);


        end;


        if fileexists(sLckFile) then
          exit;
        fsLck := TFileStream.create(sLckFile, fmCReate);
      except
        fsLck.free;
        exit;
      end;







      if assigned(cmd) then cmd.Stepcount := 5;
      sFile := sInputFile;
      sIn := sInputFile;

      sTemp2 := GetTempPath+'videoproc\'+inttostr(GetCurrentThreadID())+'\';
      forcedirectories(sTemp2);
      copyfile(pchar(sIn), pchar(sTemp2+extractfilename(sIn)), false);
      if fileexists(sFinalMp3) then
        copyfile(pchar(sFinalMp3), pchar(sMp3), false);

      sIn := pchar(sTemp2+extractfilename(sIn));
      sMp3 := sIn+'.mp3';

      if bNeedsFRames then begin
        sTemp := GetTempPath+'videoproc\'+inttostr(GetCurrentThreadID())+'\frames\';
        forcedirectories(sTemp);
      end;
      try
        sOut := sOutputFile;

        //1---------------------------------------------------------
        if assigned(cmd) then cmd.Step := 1;


        if bNeedsMp4 then begin
          //start command to convert video to mp4
          sMp3 := sIn+'.mp3';
          sTurb := sIn+'.turb';
          c2 := Tcmd_FFMPEG.Create;
          c2.FAction := TFFMPEG_Action.ffToDMXVideo;
          c2.InputFile := sIn;
          c2.outputFile := changefileext(sOutputFile,'.mp4');
          c2.Start;
        end;
        try


          //ffmpeg_tools.ExtractVideoFrames(sIn, sTemp);
        //  ffmpeg_tools.ConformVideoFiles(sTemp, true, true);
          //2---------------------------------------------------------
          if assigned(cmd) then cmd.Step := 2;



          if bNeedsMp3 then begin
            //! Mp3 must be made before huestogram can be built
            c := BeginVideoToMp3(sIn, false, sMp3);
            //Status := 'Waiting for mp3 to finish extraction.';
            EndVideoToMp3(c);
          end;

          if bNeedsMp3 then
            copyfile(pchar(sMp3), pchar(sFinalMp3), false);


          turb := nil;
          if bNeedsTurb then begin
            turb := Tcmd_GenerateTurbulenceData.Create;
            turb.FileName := sFinalMp3;
            turb.Rebuild := true;
            turb.Start;
          end;
          try

            //LockConsole;
            if bNeedsHue then begin
              c3 := servervideoparser.Tcmd_RenderHuestogram.create;
              c3.TempDir := sTEmp;
              c3.FileName := sIn;
              c3.OutputFileName := sOut;
            end;
            try
              if bNeedsHue then begin
                if assigned(c3) then
                  c3.Start;
              end;
              try
              finally
                if bNeedsHue Then begin
                  if assigned(c3) then
                    c3.waitfor;
                end;
              end;
            finally
              if bNeedsHue then begin
                if assigned(c3) then begin
                  c3.Free;
                  c3 := nil;
                end;
              end;
            end;
          finally
            if bNeedsTurb then begin
              turb.WaitFor;
              turb.Free;
              turb := nil;
            end;
          end;
          //UnlockConsole;
        finally
          if bNeedsMp4 then begin
            if assigned(c2) then begin
              c2.WaitFor;
              c2.Free;
              c2 := nil;
            end;
          end;
        end;


        //5---------------------------------------------------------
        if assigned(cmd) then cmd.Step := 5;

        if bNeedsTurb then
          copyfile(pchar(sTurb), pchar(sOutputFile+'.turb'), false);
      finally
        DeleteFolder(sTemp2);
      end;
    except
      on e:exception do begin
        Debug.Log(e.classname+' threw exception: '+e.classname+' '+e.message)
      end;
    end;
  finally
    fsLck.free;
    fslck := nil;

     if fileexists(sLckFile) then
      deletefile(sLckFile);


  end;


end;


{ Tcmd_VideoToJVID }

procedure Tcmd_VideoToJVID.DoExecute;
begin
  inherited;
  VideoToJVid(InputFile, OutputFile, self);

end;

function Tcmd_VideoToJVID.GetOutputAudioFile: string;
begin
  result := changefileext(OutputFile, '.mp3');
end;

class function Tcmd_VideoToJVID.Go(sINput, sOutput: string): Tcmd_VideoToJVID;
begin
  result := Tcmd_VideoToJVID.Create;

  result.InputFile := sInput;
  result.OutputFile := sOutput;
  result.Start;
end;

procedure Tcmd_VideoToJVID.Init;
begin
  inherited;
  Icon := @CMD_ICON_REEL;
end;

procedure Tcmd_VideoToJVID.InitExpense;
begin
  inherited;
  CPUExpense := 0;
  resources.SetResourceUsage('ffmpeg', lesserof(1.0,2/(GetEnabledCPUCount)));
end;

procedure TJVIDBS.Init;
begin
  inherited;
  HighSize := 80000;
  LowSize := 3000;
end;

procedure TJVIDBS.Search(testfirst: nativeint);
begin
  BSFunc(testfirst);
  if (ms.size >= HighSize) or (ms.size <= LowSize) then begin
    raise ECritical.create('deprecated');
//    BinarySearch(BSFunc);
  end;
end;

{ Tcmd_NextFrame }

procedure Tcmd_NextFrame.DoExecute;
begin
  inherited;
  if Reader.PrefetchRequestFlag = 1 then begin
    reader.NextFrame;
    Reader.PrefetchRequestFlag := 0;
    Reader.PrefetchAckFlag := 1;
  end;


end;

{ Tcmd_VideoToDmxVideo }

procedure Tcmd_VideoToDmxVideo.DoExecute;
begin
  inherited;
  //LockConsole;
  self.FRaiseExceptions := false;
  Status := ExtractFileName(InputFile);
  VideoToDmxVideo(InputFile, OutputFile, self);


  //UnlockConsole;
end;

function Tcmd_VideoToDmxVideo.GetOutputAudioFile: string;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

class function Tcmd_VideoToDmxVideo.Go(sINput,
  sOutput: string): Tcmd_VideoToDmxVideo;
begin
  result := Tcmd_VideoToDmxVideo.Create;

  result.InputFile := sInput;
  result.OutputFile := sOutput;
  result.Start;
end;

procedure Tcmd_VideoToDmxVideo.Init;
begin
  inherited;
  Icon := @CMD_ICON_REEL;
end;

procedure Tcmd_VideoToDmxVideo.InitExpense;
begin
  inherited;
  CPUExpense := 0;
  resources.SetResourceUsage('ffmpeg', lesserof(1.0,1/(lesserof(16,GetEnabledCPUCount))));
end;


function ffMpegProbe(sFile: string): TFFProbe;
var
  junk, s, sl, sr: string;
  h: IHolder<TStringList>;
  t: ni;
begin
  result.init;
  s := ffMpegProbeStr(sFile);
  if SplitString(s, 'Stream #0:0', sl, sr) then begin
    sr := stringreplace(sr, CRLF, LF, [rfReplaceAll]);
    h := ParseStringh(sr, LF);
    if h.o.Count > 0 then begin
      h := ParseStringh(h.o[0], ',');
      for t:= 0 to h.o.count-1 do begin
        s := h.o[t];
        s := Trim(s);

        if SplitString(s, 'x', sl,sr) then begin
          SplitString(sr,' ',sr, junk);
          if IsInteger(sl) and IsInteger(sr) then begin
            result.w := strtofloat(sl);
            result.h := strtofloat(sr);
          end;
        end;
      end;

    end;
  end;

  //parse streams
  begin
    s := ffMpegProbeStr(sFile);
    for var x := 0 to 32 do begin
      for var y := 0 to 32 do begin
        //if we have such a stream
        if SplitString(s, 'Stream #'+x.ToString+':'+y.tostring+'(', sl,sr)
        or SplitString(s, 'Stream #'+x.ToString+':'+y.tostring+':', sl,sr)
        then begin
          //identify the stream type
          Splitstring(sr,#10, sl, sr);
          sl := trim(sl);
          if SplitString(sl, 'Video: ', sl, sr) then begin
            result.VideoCodecEx := sr;
            SplitString(sr,',',result.VideoCodec, sr);
          end;
          if SplitString(sl, 'Audio: ', sl, sr) then begin
            result.AudioCodecEx := sr;
            SplitString(sr,',',result.AudioCodec, sr);
          end;

        end;
      end;
    end;

  end;


end;

function ffMpegProbeStr(sFile: string; bSynchronous: boolean = true): string;
var
  c: Tcmd_FFMPEG;
begin
  if sFile = '' then
    raise ECritical.create('no file specified');
  c := Tcmd_FFMPEG.Create;
  try
    c.bleedingedge := be4kEra;
    c.InputFile := sFile;
    c.ACtion := ffProbe;
    c.CaptureConsoleoutput := true;
    c.CPUExpense := 0;
    c.resources.SetResourceUsage('ffmpeg_probe', 1.0);
    if bSynchronous then
      c.Execute
    else begin
      c.Start;
      c.WaitFor;
    end;
    result := c.Consoleoutput;
  finally
    c.Free;
  end;

end;

{ TFFProbe }

procedure TFFProbe.Init;
begin
  w := 0;
  h := 0;

end;

function TFFProbe.Is5_1: boolean;
begin
  result := zpos('5.1', AudioCodecEx) >=0;
end;

function TFFProbe.Is7_1: boolean;
begin
  result := zpos('7.1', AudioCodecEx) >=0;
end;

function TFFProbe.ToString: string;
begin
  result := 'w: '+w.tostring+' h: '+h.tostring+NL+
            'Video: '+videocodec+'    '+
            'Audio: '+audiocodec+nl+
            'VideoEx: '+videocodecex+nl+
            'AudioEx: '+audiocodecex;

end;

{ Tcmd_ConversionSuite }

procedure Tcmd_ConversionSuite.DoExecute;
var
  originalfile: string;
  starlog: string;
  procedure UntarLog(s: string);
  begin
    starlog := starlog + s + NL;
    SaveStringAsFile(Originalfile+'.untar.log', starlog);
{$IFDEF CONSOLE}
    WriteLn(s);
{$ENDIF}
  end;
begin
  inherited;
  cpuexpense := 0;
  RenameExistingRelatedFiles;

  if comparetext(extractfileext(filename),'.tar')<>0 then begin
    MakeFilm(extractfilepath(filename),filename);
  end else begin
    var untarredtempserverfolder := changefileext(filename,'');
    originalfile := filename;
    try
      UntarLog('tar file found waiting for resource');
      Resources.SetResourceUsage('tartemp',1.0);
      WaitForResources;
      gNamedLocks.GetLock('tartemp');
      try
        UntarLog('deleting path '+tartemppath);
        deletefolder(tartemppath);
        UntarLog('forcing path '+tartemppath);
        forcedirectories(tartemppath);
        Status := 'move '+extractfilename(filename);
        var sexe := findtool('tar.exe');
        WaitForResources;
        begin
          UntarLog('copy '+filename+'->'+tartempfilename);
          var c := TFileCopyCommand.BeginCopyFile(filename, tartempfilename);
          try
            self.WaitForAnotherCommand(c);
            c.waitfor;

          finally
            c.Free;
          end;
        end;
        Status := 'waiting for write access to '+tartempfilename;
        if not systemx.waitForwritable(filename,60000*60) then
          raise ECritical.create('unable to get write access to '+filename);

        Status := 'unwrap '+extractfilename(tartempfilename);
        untarlog(status);
        begin
          var tarprog := sexe;
          var tarparams := '-xf '+quote(tartempfilename)+' --force-local';
          var tardir := TarExtPath;
          var c := Tcmd_RunExe.create;
          try
            c.prog := tarprog;
            c.Params := tarparams;
            c.Hide := true;
            c.WorkingDir := tardir;
            forcedirectories(tardir);
            c.start;
            c.waitfor;
          finally
            c.free;
          end;
        end;

        forcedirectories(untarredtempserverfolder);
        CopyFolder(tarextpath, untarredtempserverfolder,'*.*');
    //    deletefile(TarTempFilename);
      (*  while true do begin
          var yield := GetLargestFile(TarExtPath).name;
          untarlog('tar yielded '+yield);
          if yield = '' then
            raise ECritical.create(filename+' produced no yield');
          if not systemx.waitForwritable(yield,60000*60) then
            raise ECritical.create('unable to get write access to '+yield);

          filename := changefileext(filename, extractfileext(yield));
          untarlog('moving '+yield+'->'+filename);
          begin
            var c := TFileMoveCommand.BeginMoveFile(yield, filename);
            try
              self.WaitForAnotherCommand(c);
            finally
              c.Free;
            end;
          end;
          untarlog('testing target '+filename);
          if not systemx.waitForwritable(filename,60000*60) then
            raise ECritical.create('unable to get write access to '+filename);
        end;*)
        untarlog('delete folder '+tartemppath);
        deletefolder(tartemppath);
        Resources.SetResourceUsage('tartemp',0.0);
      finally
        gNamedLocks.ReleaseLock('tartemp');
      end;
      var proc: TProc<TfileInformation> := procedure(fil: TfileInformation) begin
        makefilm(fil.path,fil.fullname);
      end;

      IterateTree(untarredtempserverfolder, '*.mkv', proc);
      IterateTree(untarredtempserverfolder, '*.avi', proc);
      IterateTree(untarredtempserverfolder, '*.mov', proc);
      IterateTree(untarredtempserverfolder, '*.mpeg', proc);
      IterateTree(untarredtempserverfolder, '*.webm', proc);
      IterateTree(untarredtempserverfolder, '*.mp4', proc);
      IterateTree(untarredtempserverfolder, '*.mpeg4', proc);


      MakeFilm(extractfilepath(filename),filename);
    except
      on E: Exception do begin
        UntarLog(e.message);
      end;
    end;

    SaveStringAsFile(Originalfile+'.untar.log', starlog);


  end;


end;

function ffFindRelatedFile(sFile, sQualityNoDot: string): string;
begin
  result := ffGetRelatedFilename(sFile, sQualityNoDot, true);
  if fileexists(result) then exit;
  result := ffGetRelatedFilename(sFile, sQualityNoDot, false);
  if fileexists(result) then exit;


end;

function ffGetRelatedFileName(sFile, sQualityNoDot: string;
  bAppendExt: boolean): string;
var
  qq: string;
begin
  qq := sQualityNoDot;
  if bAppendExt then
    result := slash(extractfilepath(sFile))+qq+'\'+qq+'.'+extractfilename(sFile)+'.mp4'
  else
    result := slash(extractfilepath(sFile))+qq+'\'+qq+'.'+changefileext(extractfilename(sFile),'.mp4');


end;

procedure Tcmd_ConversionSuite.Init;
begin
  inherited;
  Icon := @CMD_ICON_SUITE;
end;

procedure Tcmd_ConversionSuite.InitExpense;
begin
  inherited;
  CPUExpense := 0;
end;

procedure Tcmd_ConversionSuite.MakeFilm(rootfolder, sFile: string);
var
  fil: TFileInformation;
begin
  fil := TFileInformation.create;
  begin
    fil.fullname := sFile;
    MakeFilm(rootfolder, extractfilepath(sFile), fil);
  end;
end;

procedure Tcmd_ConversionSuite.MakeFilm(rootfolder:string; sbase: string; fil: TFileInformation);
var
//  sOutmp4: string;
  cHQCR, cLQCR, cTH,cHQ,cOQ,cUQ,cLQ,c, cMain, cDep: Tcmd_FFMPEG;
  sSub: string;
  sBaseMod: string;
  sDir2: string;
  sFile: string;
  sThumbFile: string;
  probe: TFFProbe;
  sHQOut: string;

begin
  sThumbfile := '';

  //dont convert output files that already exist!
  if zpos('hq.', lowercase(fil.name)) = 0 then
    exit;
  if zpos('lq.', lowercase(fil.name)) = 0 then
    exit;
  if zpos('oq.', lowercase(fil.name)) = 0 then
    exit;
  if zpos('uq.', lowercase(fil.name)) = 0 then
    exit;

  cLQ := nil; cHQ := nil; cOQ := nil; cUQ := nil; cTH := nil;
  cHQCR := nil; cLQCR := nil;


//  sOutmp4 := slash(edit2.text) + fil.NAme + '.mp4';/////<<<<< BAD
  cdep := nil;
  cMain := nil;
  if zpos('hq.', lowercase(fil.name)) <> 0 then
  begin
    c := Tcmd_FFMPEG.Create;
    c.ACtion := ffToChromecastHQ;
    if zpos('SBS', uppercase(fil.name)) >= 0 then
    begin
  //raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
    end;
    c.BleedingEdge := beStable;
    c.CPUExpense := ffmpeg_cpus(1);
    c.MemoryExpenseGB := 0.5;
    c.InputFile := fil.FullName;
    probe := ffMpegProbe(fil.fullname);
    if probe.w > 1920 then begin
      c.BleedingEdge := be4kEra;
    end;
    sSub := zcopy(fil.FullName, length(sBase), 999999);
    sBaseMod := stringreplace(sBase, 'process', 'processed', [rfIgnoreCase]);
    c.OutputFile := changefileext(sBaseMod + sSub , '.mp4');
    sDir2 := ExtractFilePath(c.outputFile);
    sFile := ExtractFileName(c.outputFile);
    sDir2 := slash(sDir2) + 'hq\';
//    forcedirectories(sDir2);
    c.FireForget := false;
    c.outputFile :=  slash(sDir2) + 'hq.' + sFile;
    sHQOut := c.outputfile;
    sThumbFile := changefileext(c.outputFile,'.jpg');
    if clean then deletefile(c.outputFile);
    if fileexists(c.outputFile) then begin
      c.free;
      c :=nil;
    end
    else begin
      cDep := c;
      cMain := c;
    end;
    cHQ := c;
  end;
{$IFDEF DO_SWAP_CR}
//  if not fileexists(sThumbFile) then begin
  if assigned(cMain) then begin
    c := Tcmd_FFMPEG.Create;
    c.ACtion := ffSwapCR;
    c.BleedingEdge := be4kEra;
    c.CPUExpense := ffmpeg_cpus(3/16);
    c.MemoryExpenseGB := 0.5;
    c.InputFile := sHQout;
    c.FireForget := false;
    if clean then deletefile(c.outputFile);
    c.outputFile := slash(ExtractFilepath(sHQOut))+'swapcr.'+extractfilename(sHQOut);
    if cDep <> nil then
      c.AddDependency(cDep);
    c.Start;
    cHQCR := c;
  end;
{$ENDIf}
{$IFDEF DO_THUMB}
  if not fileexists(sThumbFile) then begin
    c := Tcmd_FFMPEG.Create;
    c.ACtion := ffExtractFrame;
    c.BleedingEdge := be4kEra;
    c.CPUExpense := ffmpeg_cpus(3/16);
    c.MemoryExpenseGB := 0.5;
    c.InputFile := sHQout;
    c.FireForget := false;
    if clean then deletefile(c.outputFile);
    c.outputFile := sThumbFile;
    if cDep <> nil then
      c.AddDependency(cDep);
    c.Start;
    cTH := c;
  end;
{$ENDIF}
if cMain <> nil then
    cMain.start;
{$IFDEF DO_OQ}
  if zpos('oq.', lowercase(fil.name)) < 0 then
  begin
    c := Tcmd_FFMPEG.Create;
    c.ACtion := ffToChromecastOQ;
    if zpos('SBS', uppercase(fil.name)) >= 0 then
    begin
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
    end;
    c.BleedingEdge := be4kEra;
    c.CPUExpense := ffmpeg_cpus(2);
    c.MemoryExpenseGB := 0.5;
    c.InputFile := fil.FullName;
    sSub := zcopy(fil.FullName, length(sBase), 999999);
    sBaseMod := stringreplace(sBase, 'process', 'processed', [rfIgnoreCase]);
    c.OutputFile := changefileext(sBaseMod + sSub, '.mp4');
    sDir2 := ExtractFilePath(c.outputFile);
    sFile := ExtractFileName(c.outputFile);
    sDir2 := slash(sDir2) + 'oq\';
//    forcedirectories(sDir2);
    c.outputFile := slash(sDir2) + 'oq.' + sFile;
    if clean then deletefile(c.outputFile);
    if fileexists(c.outputFile) then begin
      c.free;
      c := nil;
    end else
      c.Start;

    cOQ := c;
  end;
{$ENDIF}
{$IFDEF DO_UQ}
  if zpos('uq.', lowercase(fil.name)) < 0 then
  begin
    c := Tcmd_FFMPEG.Create;
    c.ACtion := ffToChromecastUQ;
    if zpos('SBS', uppercase(fil.name)) >= 0 then
    begin
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
    end;
    c.BleedingEdge := be4kEra;
    c.CPUExpense := ffmpeg_cpus(1);
    c.MemoryExpenseGB := 0.5;
    c.InputFile := fil.FullName;
    sSub := zcopy(fil.FullName, length(sBase), 999999);
    sBaseMod := stringreplace(sBase, 'process', 'processed', [rfIgnoreCase]);
    c.OutputFile := changefileext(sBaseMod + sSub, '.mp4');
    sDir2 := ExtractFilePath(c.outputFile);
    sFile := ExtractFileName(c.outputFile);
    sDir2 := slash(sDir2) + 'uq\';
//    forcedirectories(sDir2);
    c.outputFile := slash(sDir2) + 'uq.' + sFile;
    if clean then deletefile(c.outputFile);
    if fileexists(c.outputFile) then begin
      c.free;
      c := nil;
    end else
      c.Start;

    cUQ := c;
  end;
{$ENDIF}
{$IFDEF DO_LQ}
  if zpos('lq.', lowercase(fil.name)) < 0 then
  begin
    //--------------------------------------
    c := Tcmd_FFMPEG.Create;
    c.ACtion := ffToChromecastLQ;
    c.BleedingEdge := beStable;
    c.CPUExpense := ffmpeg_cpus(0.5);
    c.MemoryExpenseGB := 0.5;
    c.InputFile := fil.FullName;
    sSub := zcopy(fil.FullName, length(sBase), 999999);
    sBaseMod := stringreplace(sBase, 'process', 'processed', [rfIgnoreCase]);
    c.OutputFile := changefileext(sBaseMod + sSub, '.mp4');
    sDir2 := ExtractFilePath(c.outputFile);
    sFile := ExtractFileName(c.outputFile);
    sDir2 := slash(sDir2) + 'lq\';
//    forcedirectories(sDir2);
    c.outputFile := slash(sDir2) + 'lq.' + sFile;
    c.FireForget := false;
    if clean then deletefile(c.outputFile);
    if fileexists(c.outputFile) then begin
      c.free;
      c := nil;
    end else
      c.Start;

    cLQ := c;

  end;
{$ENDIF}


  if assigned(cLQ) then
    WaitForAnotherCommand(cLQ);
  if assigned(cHQ) then
    WaitForanotherCommand(cHQ);
  if assigned(cUQ) then
    WaitForanotherCommand(cUQ);
  if assigned(cOQ) then
    WaitForAnotherCommand(cOQ);
  if assigned(cTH) then
    WaitForAnotherCommand(cTH);
  if assigned(cHQCR) then begin
    WaitForAnotherCommand(cHQCR);
    if fileexists(cHQ.outputfile) then begin
      if fileexists(cHQCR.outputfile) then begin
        deletefile(cHQ.outputfile);
        RenameFile(cHQCR.outputfile, cHQ.outputFile);
      end;
    end;
  end;
  if assigned(cLQCR) then
    WaitForAnotherCommand(cLQCR);



  try
    if assigned(cLQ) then
      cLQ.free;
  except
  end;

  try
    if assigned(cHQ) then
      cHQ.free;
  except
  end;
  try
    if assigned(cOQ) then
      cOQ.free;
  except
  end;
  try
    if assigned(cUQ) then
      cUQ.free;
  except
  end;
  try
    if assigned(cTH) then
      cTH.free;
  except
  end;
  try
    if assigned(cHQCR) then
      cHQCR.free;
  except
  end;
  try
    if assigned(cLQCR) then
      cLQCR.free;
  except
  end;

//  if assigned(c4) then
//    WaitForanotherCommand(c4);
end;

procedure Tcmd_ConversionSuite.RenameExistingRelatedFiles;
  procedure doit(qq: string);
  var
    s,ss: string;
  begin
    s := ffGetRelatedFileName(filename, qq, true);
    ss := ffGetRelatedFileName(filename, qq, false);
    if fileexists(s) then
      RenameFile(s,ss);

  end;
begin
  doit('hq');
  doit('oq');
  doit('lq');
  doit('uq');
end;

function Tcmd_ConversionSuite.TarExtPath: string;
begin
  result := TarTempPath+'ext\';
end;

function Tcmd_ConversionSuite.TarTempFileName: string;
begin
  result := slash(tartemppath)+extractfilename(filename);
end;

function Tcmd_ConversionSuite.TarTempPath: string;
begin
  result := slash(dllpath)+'temp\';
end;

{ TREXE }

procedure TREXE.BuildCommandline;
begin
  //
end;

procedure TREXE.CC(ccStatus: TConsoleCaptureStatus; sData: string);
begin
// stub has no effect for remote exes
end;

procedure TREXE.DoExecute;
var
  w: TEncoderWork;
begin
  inherited;
  BuildCommandline;
  w.Init;
  w.prog :='..\..\tools\'+extractfilename(self.prog);
  w.params := self.params;
  w.gpuparams := self.gpuparams;
  w.CPUExpense := self.CPUExpense;
  w.MemoryGBExpense := self.MemoryExpenseGB;
  var hand := em.QueueWork(w);

  cc(ccStart, '');
  repeat
    w := em.GetWorkStatus(hand);
{$IFDEF REXE_WORK_STEP}
    Step := w.Step;
    Stepcount := w.StepCount;
{$ENDIF}
    Status := w.Status;
    if not w.Complete then
      sleep(1000);

    cc(ccProgress, w.DrainConsole);

  until w.Complete;

  em.ReleaseWorkItem(w.handle);

  cc(ccend, '');

  ConsoleOutput := w.ConsoleOutput;


end;

function TREXE.GetCPUExpense: single;
begin
  result := FRemoteCPUExpense;
end;

procedure TREXE.InitExpense;
begin
  inherited;
  var c, m: single;
  c := 0.0;
  m := 0.0;
  resources.SetResourceUsage('RemoteCPU', TCommand(self).CPuExpense);
//  resources.SetResourceUsage('RemoteGB', TCommand(self).MemoryExpenseGB);


end;

procedure TREXE.OnStart;
begin
  inherited;//
end;

procedure TREXE.SetCPUExpense(Value: single);
begin
  FRemoteCPuExpense := value;

end;

end.
