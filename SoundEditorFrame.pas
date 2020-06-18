unit SoundEditorFrame;

interface

// uses
// Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
// Dialogs, ExtCtrls;

uses
  soundinterfaces, typex, colorconversion, Geometry, Forms, Classes, StdCtrls, ExtCtrls, Controls, graphics, soundtools, math,
  debug, windows, sysutils, betterobject, Generics.Collections.fixed, systemx, numbers, stringx,managedthread, soundsample,
  memoryfilestream, Menus, GlassControls, advancedgraphics, framebasevcl, dialogs, fileex, helpers_stream, colorblending,
  Vcl.CheckLst, Vcl.ComCtrls, graphicwincontrol, SoundDevice_PortAudio, SoundConversion, soundtools_easywindows;

const
  AA = 4;
  BUTTON_WIDTH = 55;
  BUTTON_MARGIN = 1;
  BUTTON_TOP = 4;
  KNOB_SIZE = 4;
  MAX_PARAM_CHANNELS = 255;
  SOUND_EDITOR_FILE_BUFFER_SIZE = 1000000000 div 125;
type
  TSampleSummary = (ssSignedPeak, ssSignedValley, ssSignedAverage, ssUnsignedPeak, ssUnsignedValley, ssUnsignedAverage, ssBalanceAverage, ssBalancePeak, ssOffBalanceAverage,ssOffBalancePeak);
  TLoadFromRawFormat = (rfMono, rfStereoInterlaced);
  TDrawMode = (dmAmp, dmWave);

type
  iptr = ^integer;
  siptr = ^smallint;
  TSoundEditorFileStream = TMemoryFileStream;

  TSoundMarkerData = packed record
  private
    function GetCaption: string;
    procedure SetCaption(const Value: string);
  public
    FType: integer;
    FStart: int64;
    FEnd: int64;
    FHardEdge: boolean;
    FVelocity: single;
    Fcaption: array[0..31] of char;

    property Caption: string read GetCaption write SetCaption;
  end;

  TSoundMarker = class(TBetterObject)
  private
    Data: TSoundMarkerData;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
  protected
  public
    property MarkerType: integer read Data.FType write Data.FType;
    property StartPoint: int64 read Data.FStart write Data.FStart;
    property EndPoint: int64 read Data.FEnd write Data.FEnd;
    property Velocity: single read Data.FVelocity write Data.FVelocity;
    property HardEdge: boolean read Data.FHardEdge write Data.FHardEdge;
    procedure WriteToStream(s: TStream);
    procedure ReadFromStream(s: TStream);
    property Caption: string read GetCaption write SetCaption;

  end;

  TInterpolationType = (itNone, itLinear);

  TCurveRec = packed record
    Sample: int64;
    Level: single;
    Interpolation: TInterpolationType; // Integer enum size=4
    ItParam1: single;
    ItParam2: single;
    ItParam3: single;
    ItParam4: single;
    reserved: array [0 .. 63] of byte;
  end;

  TCurve = class(TBetterObject)
  private
    procedure SetLevel(const Value: single);
  public

    Data: TCurveRec;
    FLevelAtmouseDown: nativefloat;
    constructor create;override;
    property Sample: int64 read Data.Sample write Data.Sample;
    property Level: single read Data.Level write SetLevel;
    property Interpolation
      : TInterpolationType read Data.Interpolation write Data.Interpolation;
    procedure LoadFromStream(s: TStream);
    procedure SaveToStream(s: TStream);
    property LevelAtmouseDown: nativefloat read FLevelAtMouseDown;
    procedure SaveLevel;
    procedure ApplyConstraints;
    procedure CopyFrom(cSource: TCurve);
  end;

  TsoundMarkerList = class(TList<TSoundMarker>)
  public
    procedure AppendFromFile(sFile: string; iBaseMarkerType: integer);
    procedure LoadFromFile(sFile: string);
    procedure SaveToFile(sFile: string);
    procedure SaveToFile_Direct(sFile: string);
    procedure Sort;overload;
    procedure Sort(var iTrack: integer);overload;
    procedure Swap(i1, i2: integer);

    procedure ClearAndFree;
    procedure SetVelocityToIndex();
  end;

  TCurveList = class(TList<TCurve>)
  private
    procedure SaveToFile_Direct(sFile: string);
    function FindLastIndexFromSample(iSample: integer): integer;
  public
    destructor Destroy;override;
    procedure CreateInitialCurve;
    procedure LoadFromStream(s: TStream);
    procedure LoadFromFile(sFile: string);
    procedure FreeAndClearObjects;
    procedure SaveToFile(sFile: string);
    procedure SaveToStream(s: TStream);
    function SortCurve: boolean;
    procedure DeleteAndFree(idx: integer);
    procedure CopyFrom(clSource: TCurveList);
    function GetInterpolatedValue(iSample: int64): nativefloat;
    function GetCurveBefore(iSample: int64): integer;
    procedure Normalize;
    procedure NormalizeTo(r: nativefloat);
    procedure DeGlitch();
    procedure Posterize(iLevels: integer);
    function MaxLevel: nativefloat;
  end;

  TParamChannel = class(TBetterObject)
  strict private
    FAxies: TList<TCurveList>;
    FMarkers: TSoundMarkerList;
    function GetCurve(idx: integer): TCurveList;
    function GetCurveT: TCurveList;
    function GetCurveX: TCurveList;
    function GetCurveY: TCurveList;
    function GetCurveZ: TCurveList;
    function GetAxisCount: integer;
  strict protected
    FConstant: integer;
    procedure SaveCurves(sBoogerFile: string);
    procedure LoadCurves(sBoogerFile: string);
    procedure SaveMarkers(sBoogerFile: string);
    procedure LoadMarkers(sBoogerFile: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ClearAxies;
    procedure ClearAxisData;
    procedure ClearMarkers;
    property Constant: integer read FConstant write FConstant;
    property Axies[idx: integer]: TCurveList read GetCurve; default;
    property AxisCount: integer read GetAxisCount;
    property CurveX: TCurveList read GetCurveX;
    property CurveY: TCurveList read GetCurveY;
    property CurveZ: TCurveList read GetCurveZ;
    property CurveT: TCurveList read GetCurveT;
    property Markers: TSoundMarkerList read FMarkers;
    procedure SaveToFile(sBoogerFile: string);
    procedure LoadFromFile(sBoogerFile: string);
    function GetCurveFileName(sBoogerFile: string; Channel, Axis: integer)
      : string;
    function GetMarkerFileName(sBoogerFile: string; Channel: integer)
      : string;

    function HasData: boolean;
  end;

  TPlarity = (polPos, polNeg, polZero);
  TSoundGUISTate = (ssBase, ssZoomIn);

  TfrmSoundEditor = class(TfrmFrameBase)
    panButtons: TPanel;
    btZoomOut: TButton;
    btDeselect: TButton;
    btZoomIn: TButton;
    btPlay: TButton;
    Button2: TButton;
    Draw: TTimer;
    btnSaveMarkers: TButton;
    Button3: TButton;
    Panel1: TPanel;
    btMark: TButton;
    btUnMark: TButton;
    btInLeft: TButton;
    btOutLeft: TButton;
    btOutRight: TButton;
    btInRight: TButton;
    btLoopLEft: TButton;
    Button1: TButton;
    btLoopOutRight: TButton;
    btLoopRight: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    btnSaveCurves: TButton;
    popWave: TPopupMenu;
    Dele1: TMenuItem;
    Cancel1: TMenuItem;
    pbWave: TBackBufferedControl;
    btnDelete: TButton;
    rbMarks: TRadioButton;
    rbCurves: TRadioButton;
    Button4: TButton;
    scrollbar: TScrollBar;
    Button5: TButton;
    edAxies: TComboBoxEx;
    procedure pbWavePaint(Sender: TObject);
    procedure ZoomInClick(Sender: TObject);
    procedure ZoomOutClick(Sender: TObject);
    procedure MarkClick(Sender: TObject);
    procedure UnMarkClick(Sender: TObject);
    procedure UnSelectClick(Sender: TObject);
    procedure TweakInLeftClick(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure TweakInRightClick(Sender: TObject);
    procedure TweakInLoopLeftClick(Sender: TObject);
    procedure TweakInLoopRightClick(Sender: TObject);
    procedure TweakOutLeftClick(Sender: TObject);
    procedure TweakOutRightClick(Sender: TObject);
    procedure TweakOutLoopLeftClick(Sender: TObject);
    procedure TweakOutLoopRightClick(Sender: TObject);
    procedure btZoomOutClick(Sender: TObject);
    procedure DrawTimer(Sender: TObject);
    procedure btnSaveMarkersClick(Sender: TObject);
    procedure btPlayClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure pbWaveDblClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure btnSaveCurvesClick(Sender: TObject);
    procedure popWavePopup(Sender: TObject);
    procedure Dele1Click(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure rbMarksClick(Sender: TObject);
    procedure rbCurvesClick(Sender: TObject);
    procedure frmFrameBaseupdateState(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure edAxiesChange(Sender: TObject);

  private
    turb: Tcmd_GenerateTurbulenceData;
    FACtiveCurve: TCurve;
    FDrawMarks: boolean;
    FAA: integer;
    FZooming: boolean;
    FParamChannels: TList<TParamChannel>;
    FEditingChannel: integer;
    FEditingAxis: integer;
    FLastMouseDownX: integer;
    FLastMouseDownY: integer;
    FOnDataChanged: TNotifyEvent;
    FCurvesNeedSaving: boolean;
    FLoadedMetaFrom: String;
    FMarkersNeedSaving: boolean;
    FCurveMask: integer;
    FWaveAlpha: nativefloat;
    FAlphaBackGroundColor: TColor;
    FTempo: nativefloat;
    FMarkMode: boolean;
    FFileName: string;
    FDeviceIndex: nativeint;
    FDrawMode: TDrawMode;
    function GetScreenPositionForSample(x: integer): integer;
    procedure HLINE(x1, y1, x2, y2: integer; color: TColor; alpha: nativefloat);
    procedure manualline(x1, y1, x2, y2: integer; color: TColor; alpha: nativefloat);
    procedure VLINE(x1, y1, x2, y2: integer; color: TColor; alpha: nativefloat);
    procedure nativefloatpixel(x, y: nativefloat; color: TColor; alpha: nativefloat);
    procedure SetPixelEx(x, y: integer; color: TColor; alpha: nativefloat);
    function GetMonoMergedSamples(idx: int64): nativefloat;
    function GetMonoMergedIntegerSamples(idx: int64): integer;
    function GEtParamChannels(idx: integer): TParamChannel;
    procedure MouseDownForCurve(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure MouseDownForWave(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    function GetCurveKnobPosition(rAmp: nativefloat): integer;
    function GetAmplitudeFromScreen(y: nativefloat): nativefloat;
    procedure GetKnobWindow(x, y: integer; out xStart, xEnd: int64;
      out yStart, yEnd: nativefloat);
    function GetPointToPlot(x, y: integer; out ox: int64; out oy: nativefloat)
      : boolean;
    procedure SetEditingChannel(const Value: integer);
    function GetSampleEx(idx: int64; iChannel: integer): nativefloat;
    procedure SetCurveMask(const Value: integer);
    procedure DrawCurveMarkers;
    procedure DrawCurveAxies(iCh, iStart, iThru: integer);
    procedure SetEditingAxis(const Value: integer);
    procedure SetAlphaBackGroundcolor(const Value: TColor);
    procedure SetWaveAlpha(const Value: nativefloat);
    procedure ValidateMarkerIndex;
    procedure SetTempo(const Value: nativefloat);
    procedure SetMarkMode(const Value: boolean);
    procedure SetActiveCurve(const Value: TCurve);
    procedure UpdateNames;
    function GetFSamples(idx: int64): smallint;
    procedure SetDeviceIndex(const Value: nativeint);
    procedure SetDrawMode(const Value: TDrawMode);
  protected
    FViewStart: int64;
    FViewend: int64;
    FSelectEnd: int64;
    FSelectStart: int64;
    FMouseIsDown: boolean;
    FNoPaint: boolean;
    FSampleStart: int64;
    FSampleend: int64;
    FLoopStart: int64;
    FLoopend: int64;
    FStream: TSoundStream;
    FSoundThread: TSoundDevice_portAudio;
    FOsc_ForPlayback: TSoundStreamOscillator;
    FMarkers: TsoundMarkerList;
    FEvents: TsoundMarkerList;
    FChannels: integer;
    FLoadedFrom: String;
    FOnMarkerChanged: TNotifyEvent;
    FSelectedMarkerIndex: integer;

    procedure SetViewEnd(Value: int64);
    procedure SetViewStart(Value: int64);

    function GetDataLength: int64;
    function GetSampleLength: int64;
    procedure SetState(const Value: TSoundGUISTate);
    procedure SetSelectEnd(const Value: int64);

    procedure DrawEvents;
    procedure DrawCurves;
    procedure DrawKnob(xScreen, yScreen: integer; c: TColor; bBackground: boolean = false);
    procedure DrawKnobLine(x1Screen, y1Screen, x2Screen, y2Screen: integer;
      c: TColor);
    function GetEventColor(iType: integer): TColor;
    procedure SetNoPaint(const Value: boolean);
    procedure SetLoopEnd(const Value: int64);
    procedure SetLoopStart(const Value: int64);
    procedure SetSampleEnd(const Value: int64);
    procedure SetSampleStart(const Value: int64);
    procedure SetSelectStart(const Value: int64);
    function GetStream: TSoundStream;
    function GetMarkerCount: integer;
    function GetMarkers(idx: integer): TSoundMarker;
    function GetSelectedMarker: TSoundMarker;
    procedure SetSelectedMarker(const Value: TSoundMarker);
    procedure SetSelectedMarkerIndex(const Value: integer);
    procedure Play(iFrom, iTo: int64);
    function GetIsPlaying: boolean;
    function GetEvents(idx: integer): TSoundMarker;
    procedure AfterDrawSample(iScreenPosition: integer); virtual;
    procedure Line(x1, y1, x2, y2: integer; color: TColor);
    procedure ValidateSelection;
    procedure ValidateView;
  public
//    scrollbar: TScrollBar;
    FCurveNames: array[0..199] of array[0..11] of string;
    Fstate: TSoundGUISTate;
    // btnZoomIn: TButton;
    // btnZoomOut: TButton;
    // btnMark: TButton;
    // btnUnMark: TButton;
    // btnUnSelect: TButton;
    // btnTweakInLeft: TButton;
    // btnTweakInRight: TButton;
    // btnTweakOutLeft: TButton;
    // btnTweakOutRight: TButton;
    // btnTweakLoopInLeft: TButton;
    // btnTweakLoopInRight: TButton;
    // btnTweakLoopOutLeft: TButton;
    // btnTweakLoopOutRight: TButton;

    // btnPLay: TButton;
    lblDebug: TLabel;
    RefreshingScrollbar: boolean;
    FPlayCommand: Tcmd_PlayStream_Partial;

    property FSamples[idx: int64]: smallint read GetFSamples;

    procedure SaveCurves;
    procedure LoadCurves;

    constructor Create(aowner: TComponent); reintroduce; virtual;
    procedure CReateSoundThread;
    procedure BeforeDestruction;override;
    destructor Destroy; override;
    procedure InitParamChannels;
    procedure ZoomKeepLevel(iPOint: integer);
    procedure PanelPaint;
    property Stream: TSoundSTream read GetStream;
    procedure LoadFromFile(sAdaOrmp3File: ansistring; sMetaFile: string = '');
    procedure AfterLoadFromFile; virtual;
    procedure SaveToFile(sBoogerFile: ansistring);
    procedure SaveMetaData;
    procedure LoadMetaData;
    procedure SaveLoop(sBoogerFile: ansistring);

    property DataLength: int64 read GetDataLength;
    property SAmpleLength: int64 read GetSampleLength;

    property LoadedFrom: String read FFileName write FFileName;
    property LoadedMetaFrom: String read FLoadedMetaFrom write FLoadedMetaFrom;


    procedure CleanupPlayCommand;

    procedure LoadMarkers();
    procedure LoadEvents(sFile: string);

    procedure SaveMarkers();

    property MarkerList: TSoundMarkerList read FMarkers;
    property Markers[idx: integer]: TSoundMarker read GetMarkers;
    property Events[idx: integer]: TSoundMarker read GetEvents;
    procedure ClearMarkers;
    procedure ClearEvents;
    property MarkerCount: integer read GetMarkerCount;
    procedure AddMarker(sm: TSoundMarker);
    procedure DeleteMarker(idx: integer);
    function IsInMarker(x: int64): boolean;
    property MonoMergedSamples[idx: int64]: nativefloat read GetMonoMergedSamples;
    property MonoMergedIntegerSamples[idx: int64]
      : integer read GetMonoMergedIntegerSamples;
    property ParamChannels[idx: integer]: TParamChannel read GEtParamChannels;
    property EditingChannel
      : integer read FEditingChannel write SetEditingChannel;
    property EditingAxis: integer read FEditingAxis write SetEditingAxis;

    procedure SetLastMouseDown(x, y: integer);
    property LastMouseDownX: integer read FLastMouseDownX write FLastMouseDownX;
    property LastMouseDownY: integer read FLastMouseDownY write FLastMouseDownY;
    procedure FreeAndClearParamChannels;
    function SampleToBeat(iSample: int64): nativefloat;
    property ActiveCurve: TCurve read FActiveCurve write SetActiveCurve;
    procedure DrawWave;

    procedure ToneGate(iOutputChannel, iPitchChannel, iPitchAxis,
      iVelAxis: integer; rLowRange, rhighRange: nativefloat; rOpenGateAt: nativefloat = 0.70; rCloseGateAT: nativefloat = 0.40);
    procedure TransientDetection(
          iOutputChannel, iPitchChannel,
          iPitchAxis, iVelAxis: integer;
          rOpenGateAt: nativefloat = 0.03; rCloseGateAT: nativefloat = 0.05);

    procedure TransientDetectionFromSpectrum(
          iOutputChannel, iInputChannel: integer;
          rOpenGateAt: nativefloat = 0.009; rCloseGateAT: nativefloat = 0.02);


  published
    property DeviceIndex: nativeint read FDeviceIndex write SetDeviceIndex;
    property ViewStart: int64 read FViewStart write SetViewStart;
    property ViewEnd: int64 read FViewend write SetViewEnd;
    property SampleStart: int64 read FSampleStart write SetSampleStart;
    property SampleEnd: int64 read FSampleend write SetSampleEnd;
    property LoopStart: int64 read FLoopStart write SetLoopStart;
    property LoopEnd: int64 read FLoopend write SetLoopEnd;
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode;

    procedure Box(x1, y1, x2, y2: integer; color: TColor);

    procedure HookMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure HookMouseMove(Sender: TObject; Shift: TShiftState; x, y: integer);
    procedure HookMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);

    function GetSampleOnDisplay(x: nativefloat): integer;

    property SelectStart: int64 read FSelectStart write SetSelectStart;
    property SelectEnd: int64 read FSelectEnd write SetSelectEnd;

    property MouseIsDown: boolean read FMouseIsDown;
    property State: TSoundGUISTate read Fstate write SetState;
    procedure Debug(sMessage: ansistring);
    function VIEW_HEIGHT: integer;
    function VIEW_WIDTH: integer;
    function BUTTON_TOP: integer;
    procedure ZoomTo(iStart, iEnd: int64; rMarginPercent: nativefloat = 0.25);
    procedure ZoomMakeShowing(iPoint1, iPOint2: int64);
    procedure REfreshScrollbar;
    procedure Resize; override;
    property NoPaint: boolean read FNoPaint write SetNoPaint;

    function SearchForZeroCrossing(iStart: int64; bLeft, bRight: boolean)
      : int64;
    procedure UnMark;

    procedure PlayScrub(iStart: integer; iLength: integer = (44100 shr 1));
    procedure PlayLoop(iRepeats: integer = 2);

    property OnMarkerChanged
      : TNotifyEvent read FOnMarkerChanged write FOnMarkerChanged;
    procedure MarkerChanged;
    property SelectedMarkerIndex: integer read FSelectedMarkerIndex write
      SetSelectedMarkerIndex;
    property SelectedMarker: TSoundMarker read GetSelectedMarker write
      SetSelectedMarker;
    procedure PlaySelectedMarker;
    procedure PlaySelection;
    procedure StopPlaying;
    property IsPlaying: boolean read GetIsPlaying;
    property DrawMarks: boolean read FDrawMarks write FDrawMarks;
    property AA: integer read FAA write FAA;
    function IndexOfMarker(sm: TSoundMarker): integer;
    function Playing: boolean;
    property Zooming: boolean read FZooming write FZooming;
    function HasParamChannel(iConstant: integer): boolean;
    function IndexOfParamChannel(iConstant: integer): integer;
    procedure NotifyDatachange;
    function GetSampleSummary(iStart, iEnd: int64; ss: TSampleSummary; ichannelMask: integer = 3): nativefloat;
    procedure DataChanged;
    procedure DirtyDisplay;
    property CurvesNeedSaving: boolean read FCurvesNeedSaving write FCurvesNeedSaving;
    property MarkersNeedSaving: boolean read FMarkersNeedSaving write FMarkersNeedSaving;
    property FileName: string read FFileName write FFilename;
  published
    property OnDataChanged
      : TNotifyEvent read FOnDataChanged write FOnDataChanged;

    function SortCurve: boolean;
    property CurveMask: integer read FCurveMask write SetCurveMask;
    property AlphabackGroundColor: TColor read FAlphaBackGroundColor write SetAlphaBackGroundcolor;
    property WaveAlpha: nativefloat read FWaveAlpha write SetWaveAlpha;
    property Tempo: nativefloat read FTempo write SetTempo;
    property MarkMode: boolean read FMarkMode write SetMarkMode;

    procedure BuildMetadatafromTurb;
    procedure NameAxis(iChannel, iAxis: nativeint; sName: string);
    procedure ChangedDeviceID(iDeviceID: integer);
  end;
{$R *.dfm}

implementation

uses EasyImage, ProgressForm;

{ TSoundEditor }

procedure TfrmSoundEditor.AddMarker(sm: TSoundMarker);
var
  idx, t: integer;
begin
  // SayNatural('Marker added.');
  idx := -1;
  for t := 0 to FMarkers.count - 1 do
  begin
    if (FMarkers[t].StartPoint > sm.StartPoint) then
    begin
      idx := t;
    end;
  end;
  if (idx < 0) then
    FMarkers.add(sm)
  else
    FMarkers.insert(idx, sm);

  SaveMarkers;
  pbWave.Dirty := true;
end;

procedure TfrmSoundEditor.AfterDrawSample;
begin
  // no implementation reqd
end;

procedure TfrmSoundEditor.AfterLoadFromFile;
begin
  // no implementation reqd
end;

procedure TfrmSoundEditor.BeforeDestruction;
begin
  inherited;
  CleanupPlayCommand;
  if assigned(FSoundThread) then begin
    TPM.NoNeedThread(FsoundThread);
    FSoundThread := nil;
  end;

end;

procedure TfrmSoundEditor.Box(x1, y1, x2, y2: integer; color: TColor);
begin
  pbWave.Drawto.brush.color := color;
  pbWave.Drawto.pen.color := color;
  pbWave.Drawto.FillRect(Rect(x1, y1, x2 + 1, y2 + 1));

end;

procedure TfrmSoundEditor.Line(x1, y1, x2, y2: integer; color: TColor);
begin
  if AA = 0 then
  begin
    pbWave.Drawto.brush.color := color;
    pbWave.Drawto.pen.color := color;
    pbWave.Drawto.MoveTo(x1, y1);
    pbWave.Drawto.LineTo(x2, y2);
  end
  else
  begin
    manualline(x1, y1, x2, y2, color, 1);
  end;

end;

procedure TfrmSoundEditor.manualline(x1, y1, x2, y2: integer; color: TColor;
  alpha: nativefloat);
begin
  if (x2 - x1) > (y2 - y1) then
  begin
    HLINE(x1, y1, x2, y2, color, alpha);
  end
  else
  begin
    VLINE(x1, y1, x2, y2, color, alpha);
  end;
end;

function TfrmSoundEditor.HasParamChannel(iConstant: integer): boolean;
begin
  result := IndexOfParamChannel(iConstant) >= 0;
end;

procedure TfrmSoundEditor.HLINE(x1, y1, x2, y2: integer; color: TColor;
  alpha: nativefloat);
var
  t: integer;
  ry: nativefloat;
begin
  if (x2 < x1) then
  begin
    Swap(x1, x2);
    Swap(y1, y2);
  end;

  for t := x1 to x2 do
  begin
    // r := interpolate(
    ry := interpolate(t, y1, y2, x1, x2);
    nativefloatpixel(t, ry, color, alpha);
  end;
end;

procedure TfrmSoundEditor.rbCurvesClick(Sender: TObject);
begin
  if updatingstate then exit;
  rbMarks.checked := not rbCurves.Checked;
  MarkMode := rbMarks.checked;
  DirtyDisplay;
end;

procedure TfrmSoundEditor.rbMarksClick(Sender: TObject);
begin
  if updatingstate then exit;
  rbCurves.checked := not rbMarks.Checked;
  MarkMode := rbMarks.checked;

  DirtyDisplay;
end;

procedure TfrmSoundEditor.nativefloatpixel(x, y: nativefloat; color: TColor; alpha: nativefloat);
var
  x1, x2, y1, y2: integer;
  a1, a2, a3, a4: nativefloat;
  al, ar, at, ab: nativefloat;
begin
  a1 := 0;
  a2 := 0;
  a3 := 0;
  a4 := 0;
  al := 0;
  ar := 0;
  at := 0;
  ab := 0;

  x1 := trunc(x);
  x2 := trunc(x) + 1;
  y1 := trunc(y);
  y2 := trunc(y) + 1;

  ar := x - trunc(x);
  al := 1 - al;
  ab := y - trunc(y);
  at := 1 - ab;

  a1 := al * at;
  a2 := ar * at;
  a3 := al * ab;
  a4 := ar * ab;

  SetPixelEx(x1, y1, color, a1 * alpha);
  SetPixelEx(x2, y1, color, a2 * alpha);
  SetPixelEx(x1, y2, color, a3 * alpha);
  SetPixelEx(x2, y2, color, a4 * alpha);

end;

procedure TfrmSoundEditor.SetPixelEx(x, y: integer; color: TColor; alpha: nativefloat);
begin
  pbWave.Drawto.pixels[x, y] := colorblend(pbWave.Drawto.pixels[x, y], color,
    alpha);
end;

procedure TfrmSoundEditor.VLINE(x1, y1, x2, y2: integer; color: TColor;
  alpha: nativefloat);
var
  t: integer;
  rx: nativefloat;
begin
  if (y2 < y1) then
  begin
    Swap(x1, x2);
    Swap(y1, y2);
  end;

  for t := y1 to y2 do
  begin
    // r := interpolate(
    rx := interpolate(t, x1, x2, y1, y2);
    nativefloatpixel(rx, t, color, alpha);
  end;

end;

procedure TfrmSoundEditor.btnDeleteClick(Sender: TObject);
var
  t: integer;
  cv: TCurve;
  sm: TSoundMarker;
begin
  inherited;
  if MessageDlg('Deleting! There is no undo! Are you sure?', mtConfirmation, [mbYes, mbNo],0) <> mrYes then begin
    exit;
  end;


  if MarkMode then begin
    for t:= ParamChannels[EditingChannel].Markers.Count-1 downto 0 do begin
      sm := ParamChannels[EditingChannel].Markers[t];
      if ((sm.EndPoint >=  selectStart) and (sm.EndPoint <=  selectEnd))
      or ((sm.StartPoint >=  selectStart) and (sm.StartPoint <=  selectEnd)) then begin
        ParamChannels[EditingChannel].Markers.delete(t);
        sm.free;
      end;
    end;
  end else begin
    for t:= ParamChannels[EditingChannel][EditingAxis].Count-1 downto 0 do begin
      cv := ParamChannels[EditingChannel][EditingAxis][t];
      if (cv.Sample >= SelectStart)
      and (cv.Sample <=SelectEnd) then begin
        ParamChannels[EditingChannel][EditingAxis].Delete(t);
        cv.free;
      end;
    end;
  end;
  dirtyDisplay;

end;

procedure TfrmSoundEditor.btnSaveCurvesClick(Sender: TObject);
begin
  SaveCurves;
end;

procedure TfrmSoundEditor.btnSaveMarkersClick(Sender: TObject);
begin
  if SelectedMarker <> nil then
  begin
    SelectedMarker.StartPoint := SelectStart;
    SelectedMarker.EndPoint := SelectEnd;
    MarkerChanged;
    SaveMarkers;
  end;
end;

procedure TfrmSoundEditor.btPlayClick(Sender: TObject);
begin
  if Playing then begin
    CleanupPlayCommand;
    btPlay.Caption := '&Play';
    exit;
  end;




  if LoopStart > -1 then
    PlayLoop
  else
    Play(SelectStart, SelectEnd);

  btPlay.caption := '&Stop';
end;

procedure TfrmSoundEditor.btZoomOutClick(Sender: TObject);
begin
  ZoomTo(0, SAmpleLength - 1);
end;

procedure TfrmSoundEditor.BuildMetadatafromTurb;
var
  t: integer;
  c: TCurve;
  sm: TSoundMarker;
  b: integer;
  ot: nativeint;
  nt: nativeint;
begin
  ot := 0;
  sm := nil;
  for b:= 0 to 2 do begin
    NameAxis(b, 0, 'Amp_Frame.Peak');
    NameAxis(b, 1, 'Amp_Beat.Peak');
    NameAxis(b, 2, 'Amp_Beat.AvgPeak');
    NameAxis(b, 3, 'NormalizedTurbulence');
    NameAxis(b, 4, 'Turb_Modulation');

    for t:= 0 to turb.RecordCount-1 do begin
      c := TCurve.Create;
      c.Data.Sample := turb.Records[t].StartSample;
      c.Data.Level := turb.Records[t].bands[b].Amp_Frame.Peak;
      self.ParamChannels[b].Axies[0].Add(c);

      c := TCurve.Create;
      c.Data.Sample := turb.Records[t].StartSample;
      c.Data.Level := turb.Records[t].bands[b].Amp_Beat.Peak;
      self.ParamChannels[b].Axies[1].Add(c);

      c := TCurve.Create;
      c.Data.Sample := turb.Records[t].StartSample;
      c.Data.Level := turb.Records[t].bands[b].Amp_Beat.AVgPeak;
      self.ParamChannels[b].Axies[2].Add(c);

      c := TCurve.Create;
      c.Data.Sample := turb.Records[t].StartSample;
      c.Data.Level := turb.Records[t].bands[b].NormalizedTurbulence;
      self.ParamChannels[b].Axies[3].Add(c);

      c := TCurve.Create;
      c.Data.Sample := turb.Records[t].StartSample;
      c.Data.Level := turb.Records[t].bands[b].Turb_Modulation;
      self.ParamChannels[b].Axies[4].Add(c);



      nt := turb.Records[t].bands[b].Amp_Beat.TriggerNumber;
      if nt <> ot then begin
        if assigned(sm) then begin
          sm.EndPoint := turb.Records[t].StartSample;
          self.ParamChannels[b].Markers.Add(sm);
          sm := nil;
        end;

        sm := TsoundMarker.Create;
        sm.StartPoint := turb.Records[t].StartSample;
      end;

      ot := nt;
    end;

    if assigned(sm) then begin
       sm.EndPoint := turb.Records[turb.RecordCount-1].StartSample;
       self.ParamChannels[b].Markers.Add(sm);
       sm := nil;
    end;


  end;


end;

procedure TfrmSoundEditor.Button3Click(Sender: TObject);
var
  t: integer;
  ab: int64;
  sm: TSoundMarker;
  i: integer;
  lst: TSoundMarkerList;
begin
  ab := 999999999999;
  i := -1;

  if EditingChannel < 0 then
    lst := FMarkers
  else
    lst := ParamChannels[Editingchannel].Markers;

  for t := 0 to lst.count - 1 do
  begin
    sm := lst[t];
    if abs(sm.StartPoint - SelectStart) < ab then
    begin
      i := t;
      ab := abs(sm.StartPoint - SelectStart);
    end;
  end;

  SelectedMarkerIndex := i;
  if i > -1 then
  begin
    SelectStart := lst[i].StartPoint;
    SelectEnd := lst[i].EndPoint;
  end;

  DirtyDisplay;

end;

procedure TfrmSoundEditor.Button4Click(Sender: TObject);
begin
  inherited;
  SelectEnd := SampleLength-1;
  SelectStart := 0;
end;

procedure TfrmSoundEditor.Button5Click(Sender: TObject);
var
  c: Tcmd_GenerateTurbulenceData;
begin
  ProgressForm.BeginProgress;
  try
    if assigned(turb) then begin
      turb.cancel;
      turb.waitfor;
      turb.free;
      turb := nil;
    end;

    c := Tcmd_GenerateTurbulenceData.Create;
    c.Rebuild := true;
    c.FileName := self.FileName;
    c.Start;
    ProgressForm.frmProgress.WatchSingleCommand(c);
    c.WaitFor;

    turb := c;

    BuildMetadatafromTurb;
    EditingChannel := 0;

  finally
    ProgressForm.EndProgress;
  end;

end;

function TfrmSoundEditor.BUTTON_TOP: integer;
begin
  result := VIEW_HEIGHT + 10;
end;

procedure TfrmSoundEditor.ChangedDeviceID(iDeviceID: integer);
begin
  if iDeviceID = FSoundthread.DeviceID then
    exit;
  FSoundThread.stop;
  FSoundThread.DeviceID := iDeviceID;
  FSoundThread.Start;


end;

procedure TfrmSoundEditor.CleanupPlayCommand;
begin
  if FPlayCommand <> nil then
  begin
    FPlayCommand.Cancel;
    FPlayCommand.WaitFor;
    FPlayCommand.free;
    FPlayCommand := nil;
  end;

end;

procedure TfrmSoundEditor.ClearEvents;
begin
  while (FEvents.count > 0) do
  begin
    FEvents[FEvents.count - 1].free;
    FEvents.delete(FEvents.count - 1);
  end;
end;

procedure TfrmSoundEditor.ClearMarkers;
begin
  FMarkers.ClearAndFree;
end;

procedure TfrmSoundEditor.ComboBox1Change(Sender: TObject);
begin
  EditingChannel := ComboBox1.Itemindex - 1;
  UpdateNames;

end;

constructor TfrmSoundEditor.Create(aowner: TComponent);
begin
  inherited;
  FMarkers := TsoundMarkerList.Create();
  FEvents := TsoundMarkerList.Create();
  CReateSoundThread;

  //FullRepaint := false;

  // AA := 1;
  //doublebuffered := true;

  SelectStart := -1;
//  scrollbar := TScrollBar.Create(self);
//  scrollbar.Parent := self;
//  scrollbar.Align := alBottom;
//  scrollbar.OnChange := self.ScrollBarChange;



  // play loop
  // btnPlay := TButton.create(self);
  // with btnPlay do begin
  // left := btnTweakOutRight.left+btnTweakOutRight.width+BUTTON_MARGIN;
  // width := BUTTON_WIDTH;
  // top := BUTTON_TOP;
  // parent := self;
  // caption := '&Play';
  // OnClick := self.PlayClick;
  // anchors :=  [akLeft,akBottom];
  // end;

  lblDebug := TLabel.Create(self);
  lblDebug.Parent := self;
  lblDebug.top := VIEW_HEIGHT;
  lblDebug.left := btPlay.left + btPlay.width + BUTTON_MARGIN;
  lblDebug.anchors := [akLeft, akBottom];

  InitParamChannels;

  EditingChannel := -1;

  FCurveMask := 65535;

  FWaveAlpha := 0.5;


end;

procedure TfrmSoundEditor.CReateSoundThread;
begin
  FSoundThread := TPM.NeedThread<TSoundDevice_PortAudio>(nil);//  TSoundDevice_PortAudio.Create(self, nil);
  FSoundThread.DeviceID := self.DeviceIndex;
  FSoundThread.Start;



end;

procedure TfrmSoundEditor.DataChanged;
begin
  pbWave.Dirty := true;
  NotifyDatachange;
  ValidateMarkerIndex;
  CurvesNeedSaving := true;
end;

procedure TfrmSoundEditor.Debug(sMessage: ansistring);
begin
  lblDebug.caption := sMessage;
end;

procedure TfrmSoundEditor.Dele1Click(Sender: TObject);
begin
  ParamChannels[EditingChannel][EditingAxis].remove(FACtiveCurve);
  pbWave.dirty := true;
end;

procedure TfrmSoundEditor.DeleteMarker(idx: integer);
begin
  FMarkers[idx].free;
  FMarkers.delete(idx);
  SaveMarkers;
end;

destructor TfrmSoundEditor.Destroy;
begin

  if assigned(FMarkers) then
  while FMarkers.count > 0 do
  begin
    FMarkers[FMarkers.count - 1].free;
    FMarkers.delete(FMarkers.count - 1);
  end;
  FMarkers.free;
  FMarkers := nil;
  if assigned(FEvents) then
  while FEvents.count > 0 do
  begin
    FEvents[FEvents.count - 1].free;
    FEvents.delete(FEvents.count - 1);
  end;
  FEvents.free;
  FEvents := nil;
  if assigned(FParamChannels) then
  while FParamChannels.count > 0 do begin
    fParamChannels[0].free;
    FParamChannels.delete(0);
  end;


  inherited;
end;

procedure TfrmSoundEditor.DirtyDisplay;
begin
  ValidateMarkerIndex;
  pbWave.dirty := true;
  notifyDataChange;
  UpdateState;
end;

function TfrmSoundEditor.GetDataLength: int64;
begin
  result := FStream.SampleCount;


  if FViewend > result then
    FViewend := result;
  if FViewStart > result then
    FViewStart := result;

end;

function TfrmSoundEditor.GetEventColor(iType: integer): TColor;
var
  hsl: THSLnativefloatColor;
begin
  case iType mod 12 of
    0:result := $FF;
    1:result := $7FFF;
    2:result := clYellow;
    3:result := $FF00;
    4:result := $FF7F00;
    5:result := $FF0000;
    6:result := $FF007F;
    7:result := $3f3f3f;
    8:result := $7f7f7f;
    9:result := $7f7fff;
    10:result := $fF7f7f;
    11:result := $ff7fff;
  else
    result := clBlue;
  end;


end;

function TfrmSoundEditor.GetEvents(idx: integer): TSoundMarker;
begin
  result := FEvents[idx];
end;

function TfrmSoundEditor.GetFSamples(idx: int64): smallint;
var
  lr: TStereoSoundSample;
begin
  FStream.GetResample(idx, lr);
  result := round(((lr.Left+lr.right) / 2) * 32767);
end;

function TfrmSoundEditor.GetIsPlaying: boolean;
begin
  if not(assigned(FPlayCommand)) then
  begin
    result := false;
  end
  else
  begin
    result := FPlayCommand.running;
  end;

end;

function TfrmSoundEditor.GetMarkerCount: integer;
begin
  result := FMarkers.count;
end;

function TfrmSoundEditor.GetMarkers(idx: integer): TSoundMarker;
begin
  result := FMarkers[idx];
end;

function TfrmSoundEditor.GetMonoMergedIntegerSamples(idx: int64): integer;
begin
  if FChannels > 1 then
  begin
    result := (FSamples[idx * 2]) + (FSamples[(idx * 2) + 1]);
  end
  else
  begin
    result := (FSamples[idx]);
  end;
end;

function TfrmSoundEditor.GetSampleEx(idx: int64; iChannel: integer): nativefloat;
begin
  if FChannels > 1 then
  begin
    idx := idx * FChannels;
    if idx > high(FStream.Samplecount) then
      idx := high(FStream.Samplecount);

    result := (FSamples[(idx) + iChannel] / 32768);
  end
  else
  begin
    result := (FSamples[idx] / 32768);
  end;
end;

function TfrmSoundEditor.GetMonoMergedSamples(idx: int64): nativefloat;
begin
  if idx < 0 then idx := 0;

  if FChannels > 1 then
  begin

    idx := idx * 2;
    if idx > FStream.Samplecount then
      idx := FStream.Samplecount;

    result := (FSamples[idx] / 32768) + (FSamples[(idx) + 1] / 32768);
  end
  else
  begin
    if idx > FStream.Samplecount then
      idx := FStream.Samplecount;

    if self = nil then
      raise exception.create('WTF');

    result := (FSamples[idx] / 32768);
  end;
end;

function TfrmSoundEditor.GEtParamChannels(idx: integer): TParamChannel;
var
  pc: TParamChannel;
begin
  while (FParamChannels.count - 1) < idx do
  begin
    pc := TParamChannel.Create;
    pc.Constant := FParamChannels.count;
    FParamChannels.add(pc);
  end;
  result := FParamChannels[idx];
end;

function TfrmSoundEditor.GetSampleLength: int64;
begin
  result := FStream.SampleCount;
//  result := (GetDataLength) div (2 * FChannels);
end;

function TfrmSoundEditor.GetSampleOnDisplay(x: nativefloat): integer;
begin
  if x > SAmpleLength then
    x := SAmpleLength;

  result := round((ViewEnd - ViewStart) * (x / width)) + ViewStart;

end;

function TfrmSoundEditor.GetSampleSummary(iStart, iEnd: int64;
  ss: TSampleSummary; ichannelMask: integer = 3): nativefloat;
var
  t: integer;
  rr,r,r1,r2: nativefloat;
begin
  result := 0;
  case ss of
    ssUnsignedAverage:
    begin
      for t:= iStart to iEnd do begin
        result := result + abs(MonoMergedSamples[t]);
      end;
      result := result / ((iEnd-iStart)+1);
      if result > 1.0 then
        result := 1.0;
    end;

    ssUnsignedPeak:
    begin
      for t:= iStart to iEnd do begin
        r := abs(MonoMergedSamples[t]);
        if r > result then
          result := r;
      end;

      if result > 1.0 then
        result := 1.0;

    end;


    ssBalanceAverage:
    begin
      r1 := 0;
      r2 := 0;
      for t:= iStart to iEnd do begin
        r1 := r1 + abs(GetSampleEx(t,0));
        r2 := r2 + abs(GetSampleEx(t,1));
      end;

      r1 := r1 / (iEnd-iStart);
      r2 := r2 / (iEnd-iStart);

      result := r2-r1;
      if result < 0 then begin
        rr := (0-(abs(r1)+abs(r2)));
        if rr = 0 then
          result := 0
        else
          result := r1/rr;
      end else begin
        rr := ((abs(r1)+abs(r2)));
        if rr = 0 then
          result := 0
        else
          result := r1/rr;
      end;

      result := (rr / 2) + 0.5;


      if result > 1.0 then
        result := 1.0;

      if result < 0.0 then
        result := 0.0;
    end;

    ssBalancePeak:
    begin
      r1 := 0;
      r2 := 0;
      for t:= iStart to iEnd do begin
        if abs(GetSampleEx(t,0)) > r1 then
          r1 := abs(GetSampleEx(t,0));
        if abs(GetSampleEx(t,1)) > r2 then
          r2 := abs(GetSampleEx(t,0));
      end;

      result := ((r2-r1) / 2) + 0.5;


      if result > 1.0 then
        result := 1.0;

      if result < 0.0 then
        result := 0.0;
    end;

    ssOffBalancePeak:
    begin
      r := 0;
      for t:= iStart to iEnd do begin
        r1 := abs(GetSampleEx(t,0));
        r2 := abs(GetSampleEx(t,1));
        if r2 = 0 then
          r := -1
        else
          r := (r2-r1)/r2;
        if abs(r) > result then
          result := r;
      end;

      result := (result / 2) + 0.5;


      if result > 1.0 then
        result := 1.0;

      if result < 0.0 then
        result := 0.0;
    end;

    ssOffBalanceAverage:
    begin
      r := 0;
      r1 := 0;
      r2 := 0;
      for t:= iStart to iEnd do begin
        r1 := abs(GetSampleEx(t,0));
        r2 := abs(GetSampleEx(t,1));
        if r2 = 0 then
          rr := 0
        else
          rr := ((r2-r1)/r2);
        r := r + rr;
      end;

      result := r;
      result := (result / (iEnd-iStart));

      result := (result / 2) + 0.5;

      if result > 1.0 then
        result := 1.0;

      if result < 0.0 then
        result := 0.0;
    end;
  end;
//  TSampleSummary = (ssSignedPeak, ssSignedValley, ssSignedAverage, ssUnsignedPeak, ssUnsignedValley, ssUnsignedAverage, ssBalanceAverage);
end;

function TfrmSoundEditor.GetAmplitudeFromScreen(y: nativefloat): nativefloat;
begin
  result := (((VIEW_HEIGHT - 1) - y) / (VIEW_HEIGHT- 1));
end;

function TfrmSoundEditor.GetScreenPositionForSample(x: integer): integer;
var
  r: nativefloat;
  s, e: nativefloat;
begin
  // if x>SampleLength then
  // x := SampleLength;
  // result := round(((x*((ViewEnd-ViewStart)/width))-ViewStart));
  r := width / (ViewEnd - ViewStart);


  s := 0 - (r * ViewStart);
  if (r = 0) then
    e := s + ((SAmpleLength - 1))
  else
    e := s + ((SAmpleLength - 1) / r);
  // result := round(interpolate(x, s,e, 0,width-1));

  result := round((x - ViewStart) * r);

  // result := round(width*(x/(ViewEnd-ViewStart)))+ViewStart;
end;

procedure TfrmSoundEditor.ValidateMarkerIndex;
begin
  if EditingChannel >= 0 then begin
    if SelectedMarkerIndex > ParamChannels[EditingChannel].Markers.count-1 then
      SelectedMarkerIndex := -1;
  end else begin
    if SelectedMarkerIndex > MarkerCount then
      SelectedMarkerIndex := -1;
  end;

end;

function TfrmSoundEditor.GetSelectedMarker: TSoundMarker;
begin
   ValidateMarkerIndex;

  if (SelectedMarkerIndex < 0) then
    result := nil
  else
    if EditingChannel >= 0 then
      result := Paramchannels[EditingChannel].Markers[SelectedMarkerIndex]
    else
      result := Markers[SelectedMarkerIndex];

end;

procedure TfrmSoundEditor.MarkClick(Sender: TObject);
begin
  if SampleStart = -1 then
  begin
    SampleStart := SelectStart;
    SampleEnd := SelectEnd;
  end;

  LoopStart := SelectStart;
  LoopEnd := SelectEnd;

end;

procedure TfrmSoundEditor.MarkerChanged;
begin
  pbWave.Dirty := true;
  if assigned(FOnMarkerChanged) then
  begin
    FOnMarkerChanged(self);

  end;
end;

procedure TfrmSoundEditor.HookMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
begin
  SetLastMouseDown(x, y);
  FMouseIsDown := true;

  if EditingChannel >= 0 then
  begin
    MouseDownForCurve(Sender, Button, Shift, x, y);
  end;

  if FACtiveCurve = nil then
    MouseDownForWave(Sender, Button, Shift, x, y)
  else
    pbWave.Dirty := true;

end;

function TfrmSoundEditor.GetPointToPlot(x, y: integer; out ox: int64;
  out oy: nativefloat): boolean;
begin
  ox := GetSampleOnDisplay(x);
  oy := GetAmplitudeFromScreen(y);
  result := true;
end;

procedure TfrmSoundEditor.GetKnobWindow(x, y: integer; out xStart, xEnd: int64;
  out yStart, yEnd: nativefloat);
begin
  // determine sample points
  xStart := GetSampleOnDisplay(x - KNOB_SIZE);
  xEnd := GetSampleOnDisplay(x + (KNOB_SIZE + 1));

  // determine amplitude points
  yStart := GetAmplitudeFromScreen(y - KNOB_SIZE);
  yEnd := GetAmplitudeFromScreen(y + (KNOB_SIZE + 1));

end;

procedure TfrmSoundEditor.MouseDownForCurve(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; x, y: integer);
var
  t, u: integer;
  xStart, xEnd: int64;
  yStart, yEnd, rr: nativefloat;
  cv: TCurve;
  cl: TCurveList;
begin


  FACtiveCurve := nil;
  GetKnobWindow(x, y, xStart, xEnd, yStart, yEnd);



  if yStart > yEnd then
  begin
    rr := yEnd;
    yEnd := yStart;
    yStart := rr;
  end;
  // search through active layer to see if there's an appropriate point that we're grabbing
  // check only active axis first
  if EditingAxis >=0 then
  for t := 0 to ParamChannels[EditingChannel][EditingAxis].count - 1 do
  begin
    cv := ParamChannels[EditingChannel][EditingAxis][t];
    if (cv.Sample >= xStart) and (cv.Sample <= xEnd) and (cv.Level >= yStart)
      and (cv.Level <= yEnd) then
    begin
      FACtiveCurve := cv;
      cl := ParamChannels[EditingChannel].Axies[EditingAxis];
      for u := 0 to cl.count-1 do begin
        cl[u].SaveLevel;
      end;

      exit;
    end;
  end;

  for u := 0 to ParamChannels[EditingChannel].AxisCount - 1 do
    for t := 0 to ParamChannels[EditingChannel][u].count - 1 do
    begin
      cv := ParamChannels[EditingChannel][u][t];
      if (cv.Sample >= xStart) and (cv.Sample <= xEnd) and (cv.Level >= yStart)
        and (cv.Level <= yEnd) then
      begin
        FACtiveCurve := cv;
        EditingAxis := u;
        exit;
      end;
    end;

end;

function TfrmSoundEditor.GetCurveKnobPosition(rAmp: nativefloat): integer;
begin
  if IsNAN(rAmp) then
    rAmp := 0.0;

  if rAmp > 1.0 then
    rAmp := 1.0;

  if rAmp < -1.0 then
    rAmp := -1.0;




  result := round((VIEW_HEIGHT - 1) - (VIEW_HEIGHT * rAmp));
end;

procedure TfrmSoundEditor.MouseDownForWave
  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: integer);
begin

  case Button of
    mbLeft:
      if y < VIEW_HEIGHT then
      begin
        if (ssShift in Shift) then
        begin
          if ssAlt in Shift then
          begin
            self.SelectEnd := GetSampleOnDisplay(x);
          end
          else
          begin
            self.SelectStart := GetSampleOnDisplay(x);
          end;
        end
        else
        begin
          self.SelectStart := GetSampleOnDisplay(x);
          FMouseIsDown := true;
        end;

      end;
    mbRight:
      if y < VIEW_HEIGHT then
      begin
        self.PlayScrub(GetSampleOnDisplay(x));
      end;
  end;

end;

procedure TfrmSoundEditor.NameAxis(iChannel, iAxis: nativeint; sName: string);
begin
  FCurveNames[iChannel][iAxis] := sName;
  UpdateNames;
end;

procedure TfrmSoundEditor.UpdateNames;
var
  t,u: integer;
  s: string;
begin
  if EditingChannel < 0 then exit;
  if EditingAxis < 0 then exit;
  for u := 0 to MAX_PARAM_CHANNELS-1 do begin
    //Todo 1: I didn't finish naming the param channels... I only did the axies
  end;

  for t:= 0 to 11 do begin
    s := FCurveNames[EditingChannel][t];
    if s = '' then s := 'Axis '+inttostr(t);
    edAxies.items[t] := s;
    if t = EditingAxis then
     edAxies.Text := s;

  end;

end;

procedure TfrmSoundEditor.HookMouseMove(Sender: TObject; Shift: TShiftState;
  x, y: integer);
var
  t: integer;
  cvTemp: TCurve;
  cl: TcurveList;
  rDelta: nativefloat;
begin
  if pbWave.dirty then
    DrawWave;

  if ssShift in Shift then
  begin
    if (ssAlt) in Shift then
    begin
      // SelectStart := GetSampleOnDisplay(x);
    end
    else
    begin
      // SelectEnd := GetSampleOnDisplay(x);
    end;
  end
  else
  begin
    if FACtiveCurve <> nil then
    begin
      if FMouseIsDown then
      begin
        FACtiveCurve.Level := GetAmplitudeFromScreen(y);
        if FACtiveCurve.Level < 0 then
          FACtiveCurve.Level := 0;
        FACtiveCurve.Sample := GetSampleOnDisplay(x);

        cl := ParamChannels[EditingChannel].Axies[EditingAxis];
        rDelta := FActiveCurve.Level - FActiveCurve.LevelAtMouseDown;
        for t:= 0 to cl.Count-1 do begin
          cvTemp := cl[t];
          if (cvTemp.Sample >= SelectStart) and (cvTemp.Sample <=SelectEnd) then begin
            cvTemp.Level := cvTemp.LevelAtMouseDown+rDelta;
            cvTemp.ApplyConstraints;
          end;
        end;

        // DoubleBuffered := true;
        DirtyDisplay;
        //pbWave.Draw;
      end;
    end
    else
    begin
      if FMouseIsDown then
      begin
        SelectEnd := GetSampleOnDisplay(x);

        // PlayScrub(GetSampleOnDisplay(x));
      end;

      if y < self.VIEW_HEIGHT then
      begin
        if ssRight in Shift then
        begin
          PlayScrub(GetSampleOnDisplay(x));
        end;

      end;
    end;
  end;

end;

procedure TfrmSoundEditor.HookMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
begin



  // windows.beep(800,10);
  if FACtiveCurve = nil then
  begin
    if FMouseIsDown then
      self.SelectEnd := GetSampleOnDisplay(x);
  end;

  FMouseIsDown := false;
  // FACtiveCurve := nil;
  if SortCurve then
  begin
    dirtyDisplay;
  end;
  DataChanged;


end;

procedure TfrmSoundEditor.NotifyDatachange;
begin
  if assigned(FOnDataChanged) then
  begin
    FOnDataChanged(self);
  end;
end;

procedure Sortnativefloats(var r1, r2, r3, r4: nativefloat);
var
  a: array [0 .. 3] of nativefloat;
  t: integer;
  bSorted: boolean;
begin
  a[0] := abs(r1);
  a[1] := abs(r2);
  a[2] := abs(r3);
  a[3] := abs(r4);

  bSorted := false;
  while not bSorted do
  begin
    bSorted := true;
    for t := 0 to 2 do
    begin
      if a[t] < a[t + 1] then
      begin
        r1 := a[t];
        a[t + 1] := a[t];
        a[t] := r1;
        bSorted := false;
      end;
    end;
  end;

  r1 := a[0];
  r2 := a[1];
  r3 := a[2];
  r4 := a[3];

end;

procedure TfrmSoundEditor.DrawWave;
var
  t: integer;
  r: nativefloat;
  amp: nativefloat;
  val: smallint;
  x: integer;
  amp1, amp2, amp3, amp4: nativefloat;
  ccbg,c1, c2, c3, c4: TColor;
  cbg, cc1, cc2, cc3, cc4: TColor;
  hlColor: TColor;
  bhighLight: boolean;
  bg: TColor;
  endtext: integer;
  beat, beatfraction: nativefloat;
  beatcolor: Tcolor;
  ampMax, ampMin: nativefloat;
const
  beatcolors : array [0..3] of Tcolor = (clYellow, clBlue, clBlue, clBlue);
begin
  inherited;
  if NoPaint Then
    exit;

  pbWave.Prepare;
  // Box(0,0,width-1, 100, clTeal);


  if FStream = nil then
    exit;

  if FViewend = FViewStart then
    FViewend := FViewStart + 1;

  ccbg := colorblend(AlphabackGroundColor, colorblend(clTeal, clBlack, 0.5), WaveAlpha);





  for t := 0 to width - 1 do
  begin
    x := GetSampleOnDisplay(t);
    //apply gradient levels to colors based on the current beat
    beat := SampletoBeat(x);
    beatfraction := beat-trunc(beat);
    beatcolor := clBlack;//beatcolors[trunc(beat) mod 4];
    beatfraction := 0.75 -(beatfraction*16);
    if beatfraction < 0 then
      beatfraction := 0;


    bg := colorblend(ccbg, beatcolor, beatfraction);
    cc1 := colorblend(bg, colorblend(bg, clWhite, 0.20), WaveAlpha);
    cc2 := colorblend(bg, colorblend(bg, clWhite, 0.40), WaveAlpha);
    cc3 := colorblend(bg, colorblend(bg, clWhite, 0.60), WaveAlpha);
    cc4 := colorblend(bg, colorblend(bg, clWhite, 0.80), WaveAlpha);

    c1 := colorADD(cc1, beatcolor, beatfraction);
    c2 := colorADD(cc2, beatcolor, beatfraction);
    c3 := colorADD(cc3, beatcolor, beatfraction);
    c4 := colorADD(cc4, beatcolor, beatfraction);
//    c1 := cc1;
//    c2 := cc2;
//    c3 := cc3;
//    c4 := cc4;





    amp1 := MonoMergedSamples[GetSampleOnDisplay(t)];
    amp2 := MonoMergedSamples[GetSampleOnDisplay(t + 0.25)];
    amp3 := MonoMergedSamples[GetSampleOnDisplay(t + 0.50)];
    amp4 := MonoMergedSamples[GetSampleOnDisplay(t + 0.75)];
    ampMax := greaterof(amp1,greaterof(amp2, greaterof(amp3, amp4)));
    ampMin := lesserof(amp1,lesserof(amp2, lesserof(amp3, amp4)));
    Sortnativefloats(amp1, amp2, amp3, amp4);//note! also turns to ABS

    hlColor := clBlack;
    bhighLight := false;
    if (SelectStart > -1) and (x >= SelectStart) and (x <= SelectEnd) then
    begin
      bhighLight := true;
      if hlColor = clBlack then
        hlColor := clWhite
      else
        hlColor := colorblend(hlColor, clWhite, 0.5);
    end;
    if (SampleStart > -1) and (x >= SampleStart) and (x <= SampleEnd) then
    begin
      bhighLight := true;
      bhighLight := true;
      if hlColor = clBlack then
        hlColor := clGreen
      else
        hlColor := colorblend(hlColor, clGreen, 0.5);
    end;
    if (LoopStart > -1) and (x >= LoopStart) and (x <= LoopEnd) then
    begin
      bhighLight := true;
      bhighLight := true;
      if hlColor = clBlack then
        hlColor := clYellow
      else
        hlColor := colorblend(hlColor, clYellow, 0.5);
    end;
    if IsInMarker(x) then
    begin
      bhighLight := true;
      bhighLight := true;
      if hlColor = clBlack then
        hlColor := clRed
      else
        hlColor := colorblend(hlColor, clRed, 0.5);
    end;
    if (FPlayCommand <> nil) then
    begin
      if (FPlayCommand.running) then
      begin
        if (x >= (FPlayCommand.Step + FPlayCommand.Osc.Position)) and
          (x <= (FPlayCommand.Step + FPlayCommand.Osc.Position) +
            (GetSampleOnDisplay(2) - GetSampleOnDisplay(0))) then
        begin
          bhighLight := true;
          if hlColor = clBlack then
            hlColor := clYellow
          else
            hlColor := colorblend(hlColor, clYellow, 0.75);
        end;
      end;
    end;

    with pbWave do
      if bhighLight then
      begin
        case DrawMode of
          dmAmp: begin
            Box(t, 0, t, VIEW_HEIGHT, colorblend(bg, hlColor, 0.5));
            Box(t, (VIEW_HEIGHT shr 1) - round(amp1 * (VIEW_HEIGHT shr 1)), t,
              (VIEW_HEIGHT shr 1) + round(amp1 * (VIEW_HEIGHT shr 1)), colorblend
                (c1, hlColor, 0.5));
            Box(t, (VIEW_HEIGHT shr 1) - round(amp2 * (VIEW_HEIGHT shr 1)), t,
              (VIEW_HEIGHT shr 1) + round(amp2 * (VIEW_HEIGHT shr 1)), colorblend
                (c2, hlColor, 0.5));
            Box(t, (VIEW_HEIGHT shr 1) - round(amp3 * (VIEW_HEIGHT shr 1)), t,
              (VIEW_HEIGHT shr 1) + round(amp3 * (VIEW_HEIGHT shr 1)), colorblend
                (c3, hlColor, 0.5));
            Box(t, (VIEW_HEIGHT shr 1) - round(amp4 * (VIEW_HEIGHT shr 1)), t,
              (VIEW_HEIGHT shr 1) + round(amp4 * (VIEW_HEIGHT shr 1)), colorblend
                (c4, hlColor, 0.5));
          end;
          dmWave: begin
            Box(t, 0, t, VIEW_HEIGHT, colorblend(bg, hlColor, 0.5));
            Box(  {x1}t,
                  {y1}(VIEW_HEIGHT shr 1) + round(ampMin * (VIEW_HEIGHT shr 1)),
                  {x2}t+1,
                  {y2}(VIEW_HEIGHT shr 1) + round(ampMax * (VIEW_HEIGHT shr 1)+1),
                  {c }colorblend(clLime, hlColor, 0.5)
                );
//            Box(t, (VIEW_HEIGHT shr 1) - round(amp2 * (VIEW_HEIGHT shr 1)), t,
//              (VIEW_HEIGHT shr 1) + round(amp2 * (VIEW_HEIGHT shr 1)), colorblend
//                (c2, hlColor, 0.5));
//            Box(t, (VIEW_HEIGHT shr 1) - round(amp3 * (VIEW_HEIGHT shr 1)), t,
//              (VIEW_HEIGHT shr 1) + round(amp3 * (VIEW_HEIGHT shr 1)), colorblend
//                (c3, hlColor, 0.5));
//            Box(t, (VIEW_HEIGHT shr 1) - round(amp4 * (VIEW_HEIGHT shr 1)), t,
//              (VIEW_HEIGHT shr 1) + round(amp4 * (VIEW_HEIGHT shr 1)), colorblend
//                (c4, hlColor, 0.5));
          end;
        end;
      end
      else
      begin
        case DrawMode of
          dmAmp: begin
            Box(t, 0, t, VIEW_HEIGHT, bg);
            Box(t, (VIEW_HEIGHT shr 1) - round(amp1 * (VIEW_HEIGHT shr 1)), t,
              (VIEW_HEIGHT shr 1) + round(amp1 * (VIEW_HEIGHT shr 1)), c1);
            Box(t, (VIEW_HEIGHT shr 1) - round(amp2 * (VIEW_HEIGHT shr 1)), t,
              (VIEW_HEIGHT shr 1) + round(amp2 * (VIEW_HEIGHT shr 1)), c2);
            Box(t, (VIEW_HEIGHT shr 1) - round(amp3 * (VIEW_HEIGHT shr 1)), t,
              (VIEW_HEIGHT shr 1) + round(amp3 * (VIEW_HEIGHT shr 1)), c3);
            Box(t, (VIEW_HEIGHT shr 1) - round(amp4 * (VIEW_HEIGHT shr 1)), t,
              (VIEW_HEIGHT shr 1) + round(amp4 * (VIEW_HEIGHT shr 1)), c4);
          end;
          dmWave: begin
            Box(t, 0, t, VIEW_HEIGHT, bg);
            Box(  {x1}t,
                  {y1}(VIEW_HEIGHT shr 1) + round(ampMin * (VIEW_HEIGHT shr 1)),
                  {x2}t+1,
                  {y2}(VIEW_HEIGHT shr 1) + round(ampMax * (VIEW_HEIGHT shr 1)+1),
                  {c }clLIme
                );


          end;
        end;
      end;
    AfterDrawSample(t);

    Box(t, VIEW_HEIGHT, t, pbWave.Height, colorblend(bg, clWhite, t/width));

  end;

  endtext := -1;
  if DrawMarks then
    if (not assigned(FPlayCommand)) or (not FPlayCommand.running) then
      for t := 0 to width - 1 do
      begin
        if t < endtext then
        begin
          continue;
        end;
        x := GetSampleOnDisplay(t);
        endtext := t + pbWave.Drawto.TextWidth('|' + inttostr(x) + '   ');
        pbWave.Drawto.pen.Mode := pmXor;
        pbWave.Drawto.TextOut(t, VIEW_HEIGHT - pbWave.Drawto.TextHeight('0'),
          '|' + inttostr(x) + '   ');

      end;

  DrawEvents;
  DrawCurves;
  DrawCurveMarkers;

  pbWave.dirty := false;
  pbWave.Flip;


end;

procedure TfrmSoundEditor.edAxiesChange(Sender: TObject);
begin
  SetEditingAxis(edAxies.ItemIndex);
end;

procedure TfrmSoundEditor.FrameResize(Sender: TObject);
begin
//  pbWave.Dirty := true;
  Dirtydisplay;
end;

procedure TfrmSoundEditor.FreeAndClearParamChannels;
var
  t: integer;
begin
  for t := FParamChannels.count - 1 downto 0 do
  begin
    FParamChannels[t].free;
    FParamChannels.delete(t);
  end;

end;

procedure TfrmSoundEditor.frmFrameBaseupdateState(Sender: TObject);
begin
  inherited;
  rbMarks.checked := MarkMode;
  rbCurves.checked := not MarkMode;

end;

procedure TfrmSoundEditor.Play(iFrom: int64; iTo: int64);
begin
  ValidateSelection;
  if iFrom > -1 then
  begin
    CleanupPlayCommand;
    FPlayCommand := Tcmd_PlayStream_Partial.Create;
    FPlayCommand.SampleRate := Self.FStream.samplerate;
    FPlayCommand.StartSample := iFrom;
    FPlayCommand.EndSample := iTo;
    FPlayCommand.Osc := Fosc_ForPlayback;
    FPlayCommand.Thread := self.FSoundThread;
    FPlayCommand.Start;




(*    FPlayCommand := Tcmd_PlaySoundFromMemory_MonoOrStereo44100.Create;
    // todo1: hook here
    FPlayCommand.DeviceIndex := DeviceIndex;
    FPlayCommand.ForeignOffset := iFrom;
    FPlayCommand.Memory := PSmallint(PAnsiChar(self.) + 4 +
        (iFrom * 2 * FChannels));
    FPlayCommand.Channels := FChannels;
    if iTo > (SAmpleLength - 1) then
      iTo := SAmpleLength - 1;
    FPlayCommand.LengthLimit := (iTo - iFrom) * 2;
    FPlayCommand.Start();*)

    // SoundTools.PlayRawSound44Mono(PAnsiChar(self.Data)+4+(SelectStart*2), (SelectEnd-SelectStart)*2);
  end
  else
  begin
    CleanupPlayCommand;
(*    FPlayCommand := Tcmd_PlaySoundFromMemory_MonoOrStereo44100.Create;
    // todo1: hook here
    FPlayCommand.DeviceIndex := 3;
    FPlayCommand.ForeignOffset := 0;
    FPlayCommand.Memory := PSmallint(PAnsiChar(self.Data) + 4);
    FPlayCommand.Channels := FChannels;
    FPlayCommand.LengthLimit := DataLength div FChannels;
    FPlayCommand.Start();*)

  end;

end;

function TfrmSoundEditor.Playing: boolean;
begin
  result := IsPlaying;
end;

procedure TfrmSoundEditor.REfreshScrollbar;
var
  b: boolean;
begin
  RefreshingScrollbar := true;
  b := NoPaint;
  NoPaint := true;
  try

    if (self.scrollbar.Min <> 0) then
      self.scrollbar.Min := 0;

    if Zooming or (abs(self.scrollbar.PageSize - (ViewEnd - ViewStart)) > 50)
      then
      self.scrollbar.PageSize := ViewEnd - ViewStart;

    if (self.scrollbar.LargeChange <> scrollbar.PageSize div 3) then
      self.scrollbar.LargeChange := scrollbar.PageSize div 3;

    if (self.scrollbar.Position > ((SAmpleLength - 1) - scrollbar.PageSize))
      then
    begin
      scrollbar.Position := ((SAmpleLength - 1) - scrollbar.PageSize);
      // self.scrollbar.PageSize := 1;
    end;
    if scrollbar.Position <> ViewStart then
      self.scrollbar.Position := ViewStart;

    if (self.scrollbar.MAx <> SAmpleLength) then
      self.scrollbar.MAx := SAmpleLength;

  finally
    NoPaint := b;
    RefreshingScrollbar := false;
  end;

end;

procedure TfrmSoundEditor.Resize;
begin
  inherited;
  DrawWave;
end;

procedure TfrmSoundEditor.ScrollBarChange(Sender: TObject);
var
  ps: int64;
begin
  if not RefreshingScrollbar then
  begin
    RefreshingScrollbar := true;
    try
      ps := scrollbar.PageSize;
      self.ViewStart := scrollbar.Position;
      self.ViewEnd := ViewStart + ps;
    finally
      RefreshingScrollbar := false;
    end;
  end;
end;

procedure TfrmSoundEditor.SetActiveCurve(const Value: TCurve);
begin
  FActiveCurve := Value;
  dirtydisplay;
end;

procedure TfrmSoundEditor.SetAlphaBackGroundcolor(const Value: TColor);
begin
  FAlphaBackGroundColor := Value;
  DirtyDisplay;
end;

procedure TfrmSoundEditor.SetCurveMask(const Value: integer);
begin
  FCurveMask := Value;
  pbWave.dirty := true;
end;


procedure TfrmSoundEditor.SetDeviceIndex(const Value: nativeint);
begin
  FDeviceIndex := Value;
  self.ChangedDeviceID(value);
end;

procedure TfrmSoundEditor.SetDrawMode(const Value: TDrawMode);
begin
  FDrawMode := Value;
  DirtyDisplay;
end;

procedure TfrmSoundEditor.SetEditingAxis(const Value: integer);
begin
  FEditingAxis := Value;
  DirtyDisplay;
  edAxies.Itemindex := value;
  MarkMode := false;
end;

procedure TfrmSoundEditor.SetEditingChannel(const Value: integer);
begin
  if Value <> FEditingChannel then
  begin
    FEditingChannel := Value;
    dirtyDisplay;
    ComboBox1.Itemindex := Value + 1;
  end;

end;

procedure TfrmSoundEditor.SetSelectedMarker(const Value: TSoundMarker);
begin
  FSelectedMarkerIndex := FMarkers.IndexOf(Value);
end;

procedure TfrmSoundEditor.SetSelectedMarkerIndex(const Value: integer);
begin
  FSelectedMarkerIndex := Value;
end;

procedure TfrmSoundEditor.SetSelectEnd(const Value: int64);
begin
  FSelectEnd := Value;
  Debug(inttostr(SelectStart) + '-' + inttostr(SelectEnd) + ' ' + floatprecision
      (((SelectEnd - SelectStart) / 44100) * 1000, 2) + ' ms');
  //DirtyDisplay;
  DrawWave;
end;

procedure TfrmSoundEditor.SetState(const Value: TSoundGUISTate);
begin
  Fstate := Value;
  case Fstate of
    ssZoomIn:
      begin
        btZoomIn.Enabled := false;
        DirtyDisplay;
      end;
  else
    btZoomIn.Enabled := true;
  end;
end;

procedure TfrmSoundEditor.SetTempo(const Value: nativefloat);
begin
  FTempo := Value;
  dirtyDisplay;
end;

procedure TfrmSoundEditor.SetViewEnd(Value: int64);
begin
  if Value > SAmpleLength - 1 then
    Value := SAmpleLength - 1;
  if Value = FViewend then
    exit;

  FViewend := Value;
  REfreshScrollbar;
  DrawWave;
end;

procedure TfrmSoundEditor.SetViewStart(Value: int64);
begin
  if Value < 0 then
    Value := 0;
  if Value = FViewStart then
    exit;

  FViewStart := Value;
  REfreshScrollbar;
  DrawWave;
end;

procedure TfrmSoundEditor.SetWaveAlpha(const Value: nativefloat);
begin
  FWaveAlpha := Value;
  DirtyDisplay;
end;

function TfrmSoundEditor.SortCurve: boolean;
// returns true if anything was sorted
begin
  result := false;
  if EditingChannel < 0 then
    exit;
  if EditingAxis < 0 then
    exit;

  result := ParamChannels[EditingChannel][EditingAxis].SortCurve;
end;

procedure TfrmSoundEditor.StopPlaying;
begin
  if assigned(FPlayCommand) then
  begin
    FPlayCommand.Lock;
    try
      if FPlayCommand.running then
        FPlayCommand.Cancel;
    finally
      FPlayCommand.Unlock;
    end;
  end;
  CleanupPlayCommand;
end;

procedure TfrmSoundEditor.DrawCurveAxies(iCh, iStart, iThru: integer);
var
  t: integer;
  cv, cv1, cv2: TCurve;
  bHead, bTail: boolean;
  tax, y, x1, x2, ax: integer;
  cl: TColor;
  cnt: integer;
  np1,np2: nativefloat;
  np1i,np2i: nativeint;
begin
  cv2 := nil;
  np1 := 0;
  np2 := 0;
  for tax := iStart to iThru do
  begin
    if tax < 0 then continue;

    if (1 shl tax) and (CurveMask) = 0 then continue;


    for t := 0 to ParamChannels[iCh].Axies[tax].count - 2 do
    begin
      if tax >=ParamChannels[iCh].AxisCount then
        continue;

      cv1 := ParamChannels[iCh].Axies[tax][t];
      cv2 := ParamChannels[iCh].Axies[tax][t + 1];
      if cv1 = nil then continue;
      if cv2 = nil then continue;

      cv := cv1;
      if cv2.Sample < ViewStart then
        continue;
      if cv1.Sample > ViewEnd then
        continue;



      if cv2 = FACtiveCurve then
      begin
        cl := clWhite;
      end
      else
      begin
        //if (cv2.Sample <= SelectEnd) and (cv2.Sample >= SelectStart) then begin
        //  cl := colorBlend(GetEventColor(tax), clWhite, 0.5);
        //end else
          cl := GetEventColor(tax);
      end;



      if (tax <> EditingAxis) and (not MarkMode) then
        cl := ColorBlend(clBlack, cl, 0.75);

      if t= 128 then begin
        Debuglog.log(ltAll, 'cv1['+inttostr(t)+']'+floattostr(cv1.Level));
        Debuglog.log(ltAll, 'cv2['+inttostr(t)+']'+floattostr(cv2.Level));
      end;
      try
        np1 := GetCurveKnobPosition(cv1.Level);
        np2 := GetCurveKnobPosition(cv2.Level);
      except
        on E:Exception do begin
          Debuglog.log(ltAll, 'Exception drawing [ch,tax,t]:'+inttostr(iCh)+','+inttostr(tax)+','+inttostr(t));
        end;
      end;
      np1i := round(np1);
      np2i := round(np2);
      DrawKnobLine(GetScreenPositionForSample(cv1.Sample),
        np1i, GetScreenPositionForSample
          (cv2.Sample), np2i, cl);

    end;

    for t := 0 to ParamChannels[iCh].Axies[tax].count - 1 do
    begin
      cv1 := ParamChannels[iCh].Axies[tax][t];
      cv := cv1;
      if assigned(cv2) and (cv2.Sample < ViewStart) then
        continue;
      if cv1.Sample > ViewEnd then
        continue;

      if cv = FACtiveCurve then
      begin
        cl := clWhite;
      end
      else
      begin
        if (cv.Sample <= SelectEnd) and (cv.Sample >= SelectStart) then begin
          cl := colorBlend(GetEventColor(tax), clBlack, 0.75);
        end else
          cl := GetEventColor(tax);
      end;

      if (tax <> EditingAxis) {and (not MarkMode)} then
        cl := ColorBlend(clBlack, cl, 0.75);


      DrawKnob(GetScreenPositionForSample(cv.Sample), GetCurveKnobPosition
          (cv.Level), cl, (tax <> EditingAxis) or (MarkMode));

    end;
  end;
end;
procedure TfrmSoundEditor.DrawCurves;
var
  t: integer;
  cv, cv1, cv2: TCurve;
  bHead, bTail: boolean;
  tax, l, y, x1, x2, ax: integer;
  cl: TColor;
const
  Height = 5;
begin
  if EditingChannel < 0 then
    exit;

  for l := EditingChannel to EditingChannel do
  begin

    DrawCurveAxies(l, 0, ParamChannels[l].AxisCount-1);
    DrawCurveAxies(l, EditingAxis, EditingAxis);

  end;
end;

procedure TfrmSoundEditor.DrawCurveMarkers;
var
  t: integer;
  sm: TSoundMarker;
  bHead, bTail: boolean;
  y, x1, x2: integer;
  lst: TSoundMarkerlist;
const
  Height = 20;
begin
  if Editingchannel < 0 then exit;

  lst := Paramchannels[Editingchannel].Markers;

  for t := 0 to lst.count - 1 do
  begin
    sm := lst[t];
    if sm.EndPoint < ViewStart then
      continue;
    if sm.StartPoint > ViewEnd then
      continue;

    bHead := sm.StartPoint >= ViewStart;
    bTail := sm.EndPoint <= ViewEnd;

    if (bHead) then
      x1 := GetScreenPositionForSample(sm.StartPoint)
    else
      x1 := 0;

    if (bTail) then
      x2 := GetScreenPositionForSample(sm.EndPoint)
    else
      x2 := VIEW_WIDTH - 1;

    y := (VIEW_HEIGHT shr 1) + (sm.MarkerType * Height) + ((Height + 1) shr 1);

    Box(x1, y, x2, y, GetEventColor(sm.MarkerType));


    y := (VIEW_HEIGHT shr 1) +(sm.MarkerType * Height);

    if (bHead) then
      Box(x1-1, y, x1+1, y + Height, GetEventColor(sm.MarkerType));

    if (bTail) then
      Box(x2-1, y, x2+1, y + Height, GetEventColor(sm.MarkerType));

    pbWave.DrawTo.TextOut(x1,y, sm.Caption);

  end;
end;


procedure TfrmSoundEditor.DrawEvents;
var
  t: integer;
  sm: TSoundMarker;
  bHead, bTail: boolean;
  y, x1, x2: integer;
const
  Height = 5;
begin
  for t := 0 to FEvents.count - 1 do
  begin
    sm := FEvents[t];
    if sm.EndPoint < ViewStart then
      continue;
    if sm.StartPoint > ViewEnd then
      continue;

    bHead := sm.StartPoint >= ViewStart;
    bTail := sm.EndPoint <= ViewEnd;

    if (bHead) then
      x1 := GetScreenPositionForSample(sm.StartPoint)
    else
      x1 := 0;

    if (bTail) then
      x2 := GetScreenPositionForSample(sm.EndPoint)
    else
      x2 := VIEW_WIDTH - 1;

    y := (sm.MarkerType * Height) + ((Height + 1) shr 1);

    Box(x1, y, x2, y, GetEventColor(sm.MarkerType));

    y := (sm.MarkerType * Height);

    if (bHead) then
      Box(x1, y, x1, y + Height, GetEventColor(sm.MarkerType));

    if (bTail) then
      Box(x2, y, x2, y + Height, GetEventColor(sm.MarkerType));

  end;
end;

procedure TfrmSoundEditor.DrawKnob(xScreen, yScreen: integer; c: TColor; bBackground: boolean = false);
var
  t,u: integer;
  r: TRect;
  rAlpha: nativefloat;
begin
  if xScreen < 0 then exit;
  if yScreen > pbWave.Width then exit;

  if bBackground then begin

//    r.left := xScreen - (KNOB_SIZE shr 1);
//    r.top := yScreen - (KNOB_SIZE shr 1);
//    r.Right := xScreen + (KNOB_SIZE shr 1) + 1;
//    r.Bottom := yScreen + (KNOB_SIZE shr 1) + 1;
//
//    for u:= 0 to KNOB_SIZE do begin
//      for t:= 0 to KNOB_SIZE do begin
//        rAlpha := 1-((sqrt((t*t)+(u*u)) / (KNOB_SIZE)));
//        if rAlpha < 0 then continue;
//
//        pbWave.Drawto.pixels[xScreen-t,yScreen-u] := colorBlend(pbWave.drawto.pixels[xScreen-t,yScreen-u], c, rAlpha);
//        pbWave.Drawto.pixels[xScreen+t,yScreen-u] := colorBlend(pbWave.drawto.pixels[xScreen+t,yScreen-u], c, rAlpha);
//        pbWave.Drawto.pixels[xScreen-t,yScreen+u] := colorBlend(pbWave.drawto.pixels[xScreen-t,yScreen+u], c, rAlpha);
//        pbWave.Drawto.pixels[xScreen+t,yScreen+u] := colorBlend(pbWave.drawto.pixels[xScreen+t,yScreen+u], c, rAlpha);
//
//
//      end;
//    end;

    pbWave.Drawto.brush.color := colorblend(c, clBlack, 0.5);
    pbWave.Drawto.pen.color := clBlack;
    r.left := xScreen - KNOB_SIZE;
    r.top := yScreen - KNOB_SIZE;
    r.Right := xScreen + KNOB_SIZE + 1;
    r.Bottom := yScreen + KNOB_SIZE + 1;

    pbWave.Drawto.FillRect(r);

   // pbWave.Drawto.FillRect(r);
  end else begin
    pbWave.Drawto.brush.color := colorblend(c, clBlack, 0.5);
    pbWave.Drawto.pen.color := clBlack;
    r.left := xScreen - KNOB_SIZE;
    r.top := yScreen - KNOB_SIZE;
    r.Right := xScreen + KNOB_SIZE + 1;
    r.Bottom := yScreen + KNOB_SIZE + 1;

    pbWave.Drawto.FillRect(r);

    pbWave.Drawto.brush.color := c;
    pbWave.Drawto.pen.color := clBlack;
    r.left := xScreen - (KNOB_SIZE shr 1);
    r.top := yScreen - (KNOB_SIZE shr 1);
    r.Right := xScreen + (KNOB_SIZE shr 1) + 1;
    r.Bottom := yScreen + (KNOB_SIZE shr 1) + 1;

    pbWave.Drawto.FillRect(r);
  end;
end;

procedure TfrmSoundEditor.DrawKnobLine(x1Screen, y1Screen, x2Screen,
  y2Screen: integer; c: TColor);
var
  r: TRect;
begin
  pbWave.Drawto.brush.color := c;
  pbWave.Drawto.pen.color := c;
  r.left := x1Screen;
  r.top := y1Screen;
  r.Right := x2Screen;
  r.Bottom := y2Screen;

  // pbWave.Drawto.FillRect(r);

  pbWave.Drawto.MoveTo(r.left, r.top);
  pbWave.Drawto.LineTo(r.Right, r.Bottom);

end;

procedure TfrmSoundEditor.DrawTimer(Sender: TObject);
begin
  if assigned(FPlayCommand) then
  begin
    if (FPlayCommand.running) then
    begin
      self.ZoomKeepLevel(FPlayCommand.Osc.Position + FPlayCommand.Step);

      // ((FPlayCommand.ForeignOffset-(FPlayCommand.NumberOfSamples div 4)), FPLayCommand.ForeignOffset+FPlayCommand.NumberOfSamples+(FPlayCommand.NumberOfSamples div 4));
    end else begin
      if FPlayCommand.IsComplete then begin
        CleanupPLayCommand;
        btPlay.caption := 'Play';
      end;
    end;
  end;
end;

procedure TfrmSoundEditor.ValidateSelection;
var
  iTemp: int64;
begin

  if SelectEnd > (SAmpleLength - 1) then
  begin
    // FSelectStart := -1;
    FSelectEnd := (SAmpleLength - 1);
    dirtydisplay;
  end;

  if SelectStart > SelectEnd then
  begin
    iTemp := SelectStart;
    FSelectStart := FSelectEnd;
    FSelectEnd := iTemp;
  end;

  if SelectStart > (SAmpleLength - 1) then
  begin
    FSelectStart := -1;
    FSelectEnd := -1;
    dirtydisplay;
  end;

end;

procedure TfrmSoundEditor.ValidateView;
var
  iTemp: int64;
begin
  if ViewStart > (SAmpleLength - 1) then
  begin
    FSelectStart := -1;
    FSelectEnd := -1;
    dirtydisplay;
  end;

  if ViewEnd > (SAmpleLength - 1) then
  begin
    FSelectStart := -1;
    FSelectEnd := -1;
    dirtydisplay;
  end;

  if ViewStart > ViewEnd then
  begin
    iTemp := FViewStart;
    FViewStart := FViewend;
    FViewend := FViewStart;

  end;

end;

function TfrmSoundEditor.VIEW_HEIGHT: integer;
begin
  result := Height - (panButtons.Height + 30);

end;

function TfrmSoundEditor.VIEW_WIDTH: integer;
begin
  result := width;

end;

procedure TfrmSoundEditor.ZoomInClick(Sender: TObject);
begin
  if SelectStart > -1 then
    ZoomTo(SelectStart, SelectEnd)
  else
    ZoomTo(0, SAmpleLength - 1);

end;

procedure TfrmSoundEditor.ZoomKeepLevel(iPOint: integer);
var
  iStart: integer;
  iEnd: integer;
  w: integer;
begin
  iStart := ViewStart;
  iEnd := ViewEnd;
  w := iEnd - iStart;

  if (iPOint < iStart) or (iPOint > iEnd) then
  begin
    ZoomTo(iPOint, iPOint + w, 0);
  end
  else
  begin
    DrawWave;
  end;
end;

procedure TfrmSoundEditor.ZoomMakeShowing(iPoint1, iPOint2: int64);
var
  iStart: int64;
  iEnd: int64;
begin
  iStart := ViewStart;
  iEnd := ViewEnd;

  if (iStart > iPoint1) then
    iStart := LesserOf(int64(iPoint1), int64(iPOint2));
  if (iStart > iPOint2) then
    iStart := LesserOf(int64(iPoint1), int64(iPOint2));
  if (iEnd < iPoint1) then
    iEnd := GreaterOf(iPoint1, iPOint2);
  if (iEnd < iPOint2) then
    iEnd := GreaterOf(iPoint1, iPOint2);

  if (iStart = iPoint1) or (iStart = iPOint2) then
  begin
    iStart := iStart - ((iEnd - iStart) div 4)
  end;

  if (iEnd = iPoint1) or (iEnd = iPOint2) then
  begin
    iEnd := iEnd + ((iEnd - iStart) div 4)
  end;

  ZoomTo(iStart, iEnd, 0);

end;

procedure TfrmSoundEditor.ZoomOutClick(Sender: TObject);
var
  viewwidth: int64;
  viewcenter: int64;
begin
  viewwidth := ViewEnd - ViewStart;
  viewcenter := ViewStart + (viewwidth shr 1);
  ZoomTo(viewcenter - (viewwidth), viewcenter + (viewwidth));

end;

procedure TfrmSoundEditor.ZoomTo(iStart, iEnd: int64;
  rMarginPercent: nativefloat = 0.25);
const
  steps = 750;
var
  t: integer;
  iStartStart, iEndStart: integer;
  tm1, tm2: cardinal;
  w: integer;
begin
  if iStart < 00 then iStart := 0;
  Zooming := true;
  try
    w := iEnd - iStart;
    iEnd := iStart + w + round(w * rMarginPercent);
    iStart := iStart - round(w * rMarginPercent);

    if (ViewStart = iStart) and (ViewEnd = iEnd) then
      exit;

    iStartStart := ViewStart;
    iEndStart := ViewEnd;

    tm1 := GetTickCount;
    tm2 := tm1;
    while tm2 - tm1 <> steps do
    begin
      tm2 := GetTickCount;
      if tm2 < tm1 then
        tm1 := tm2;
      if tm2 - tm1 > steps then
        tm2 := tm1 + steps;
      t := tm2 - tm1;

      NoPaint := true;
      ViewStart := round(Geometry.interpolate(t, steps, iStartStart, iStart));
      ViewEnd := round(Geometry.interpolate(t, steps, iEndStart, iEnd));
      NoPaint := false;
      if (ViewStart = iStart) and (ViewEnd = iEnd) then
        break;
    end;

    ViewStart := iStart;
    ViewEnd := iEnd;
    REfreshScrollbar;
  finally
    Zooming := false;
  end;

end;

procedure TfrmSoundEditor.PanelPaint;
begin
  inherited;
  DrawWave;
end;

procedure TfrmSoundEditor.pbWaveDblClick(Sender: TObject);
var
  t: integer;
  xStart, xEnd: int64;
  yStart, yEnd: nativefloat;
  c: TCurve;
begin
  if EditingChannel < 0 then
    exit;

  if EditingAxis < 0 then
    exit;


  GetPointToPlot(LastMouseDownX, LastMouseDownY, xStart, yStart);

  c := TCurve.Create;
  c.Sample := xStart;
  c.Level := yStart;
  c.Interpolation := itLinear;

  ParamChannels[EditingChannel][EditingAxis].add(c);

  SortCurve;
  DataChanged;

end;

procedure TfrmSoundEditor.pbWavePaint(Sender: TObject);
begin
  try
    DrawWave;
  except
  end;

end;

procedure TfrmSoundEditor.SetNoPaint(const Value: boolean);
begin
  if (FNoPaint <> Value) and (not Value) then
  begin
    FNoPaint := Value;
    DrawWave;
  end
  else
    FNoPaint := Value;

end;

procedure TfrmSoundEditor.SetLastMouseDown(x, y: integer);
begin
  FLastMouseDownX := x;
  FLastMouseDownY := y;
end;

procedure TfrmSoundEditor.SetLoopEnd(const Value: int64);
begin
  FLoopend := Value;
  DrawWave;
end;

procedure TfrmSoundEditor.SetLoopStart(const Value: int64);
begin
  FLoopStart := Value;
  DrawWave;
end;

procedure TfrmSoundEditor.SetMarkMode(const Value: boolean);
begin
  FMarkMode := Value;
  DirtyDisplay;
end;

procedure TfrmSoundEditor.SetSampleEnd(const Value: int64);
begin
  FSampleend := Value;
  DrawWave;
end;

procedure TfrmSoundEditor.SetSampleStart(const Value: int64);
begin
  FSampleStart := Value;
  DrawWave;
end;

procedure TfrmSoundEditor.UnMarkClick(Sender: TObject);
begin
  UnMark;
end;

procedure TfrmSoundEditor.TweakInLeftClick(Sender: TObject);
begin
  SampleStart := self.SearchForZeroCrossing(SampleStart, true, false);
end;

procedure TfrmSoundEditor.TweakInLoopLeftClick(Sender: TObject);
begin
  LoopStart := self.SearchForZeroCrossing(LoopStart, true, false);
end;

procedure TfrmSoundEditor.TweakInLoopRightClick(Sender: TObject);
begin
  LoopStart := self.SearchForZeroCrossing(LoopStart, false, true);
end;

procedure TfrmSoundEditor.TweakInRightClick(Sender: TObject);
begin
  SampleStart := self.SearchForZeroCrossing(SampleStart, false, true);
end;

function TfrmSoundEditor.SearchForZeroCrossing
  (iStart: int64; bLeft, bRight: boolean): int64;
var
  i: integer;
  amp, ampL, ampR: smallint;
  bNeg: boolean;
  iPos: int64;
  iPos2: int64;
  bZeroFound: boolean;
begin
  result := 0;
  ipos := 0;
  iPos2 := 0;
  // record the initial amp
  amp := FSamples[iStart];
  // flip the polarity if negative
  if amp < 0 then
  begin
    amp := 0 - amp;
    bNeg := true;
  end
  else
    bNeg := false;

  bZeroFound := false;
  if bLeft xor bRight then
    i := 1
  else
    i := 0;
  // while not zero found
  while not bZeroFound do
  begin

    // if look left
    if bLeft then
    begin
      // get left sample
      iPos := iStart - i;
      iPos2 := iPos;
      if iPos < 0 then
      begin
        iPos := 0;
        if bLeft xor bRight then
          break;
      end;

      ampL := FSamples[iPos];
      // flip the polarity if original sample was negative
      if bNeg then
        ampL := 0 - ampL;

      // if the original sample + this sample < the original sample
      // or this sample is 0 then its a zero crossing
      if (ampL <= 0) then
      begin
        bZeroFound := true;
        result := iPos;
        break;
      end;
    end;

    // if look right
    if bRight then
    begin
      // get left sample
      iPos := iStart + i;

      if iPos > SAmpleLength - 1 then
      BEGIN
        iPos := SAmpleLength - 1;
        if bLeft xor bRight then
          break;
      END;
      ampR := FSamples[iStart + i];
      // flip the polarity if original sample was negative
      if bNeg then
        ampR := 0 - ampR;

      // if the original sample + this sample < the original sample
      // or this sample is 0 then its a zero crossing
      if (ampR <= 0) then
      begin
        bZeroFound := true;
        result := iPos;
        break;
      end;
    end;

    // increment offset
    inc(i);
    if bLeft and bRight then
      if (iPos2 = 0) and (iPos = SAmpleLength - 1) then
      begin
        result := 0;
        break;
      end;

  end;

end;

procedure TfrmSoundEditor.UnMark;
begin
  // SelectStart := -1;
  SampleStart := -1;
  LoopStart := -1;

end;

procedure TfrmSoundEditor.TweakOutLeftClick(Sender: TObject);
begin
  SampleEnd := self.SearchForZeroCrossing(SampleEnd, true, false);
end;

procedure TfrmSoundEditor.TweakOutLoopLeftClick(Sender: TObject);
begin
  LoopEnd := self.SearchForZeroCrossing(LoopEnd, true, false);
end;

procedure TfrmSoundEditor.TweakOutLoopRightClick(Sender: TObject);
begin
  LoopEnd := self.SearchForZeroCrossing(LoopEnd, false, true);
end;

procedure TfrmSoundEditor.TweakOutRightClick(Sender: TObject);
begin
  SampleEnd := self.SearchForZeroCrossing(SampleEnd, false, true);
end;

procedure TfrmSoundEditor.UnSelectClick(Sender: TObject);
begin
  SelectStart := -1;
  DrawWave;
end;

procedure TfrmSoundEditor.SetSelectStart(const Value: int64);
begin
  FSelectStart := Value;
  DrawWave;
end;

procedure TfrmSoundEditor.PlayScrub(iStart, iLength: integer);
begin
  if iStart + iLength > SAmpleLength then
    iLength := SAmpleLength - iStart;

//  soundtools.PlayRawSound44Mono(@FSamples[iStart], iLength);

end;

procedure TfrmSoundEditor.PlaySelectedMarker;
begin
  if SelectedMarker = nil then
    exit;

  Play(SelectedMarker.StartPoint, SelectedMarker.EndPoint);
end;

procedure TfrmSoundEditor.PlaySelection;
begin
  Play(SelectStart, SelectEnd);
end;

procedure TfrmSoundEditor.popWavePopup(Sender: TObject);
begin
  Dele1.Enabled := FACtiveCurve <> nil;
end;

procedure TfrmSoundEditor.PlayLoop(iRepeats: integer);
var
  temp: array of smallint;
  idx: integer;
  off: integer;
  u, t: integer;
  a, b, c: smallint;
  p: nativefloat;
  sample_blend: integer;
  procedure ArrayWrite(amp: smallint);
  begin
    if length(temp) < idx + 1 then
      setlength(temp, idx + 1);
    temp[idx] := amp;
    inc(idx);
  end;

begin
  idx := 0;

  // calc length
  // SetLength(temp, (SampleEnd-SampleStart+1)-((iRepeats-1)*(LoopEnd-LoopStart+1)));
  setlength(temp, 0);
  sample_blend := (LoopEnd - LoopStart) shr 1;
  if sample_blend > 1500 then
    sample_blend := 1500;

  // loop from sample start to loop start fading in volume
  for t := SampleStart to LoopStart - 1 do
  begin
    a := MonoMergedIntegerSamples[t];
    a := round(interpolate(t, 0, a, SampleStart, LoopStart - 1));
    ArrayWrite(a);
  end;

  // loop through repeated section
  for u := 1 to iRepeats do
  begin
    // begin -- first X samples
    if u = 1 then
    begin
      for t := LoopStart to LoopStart + sample_blend - 1 do
      begin
        a := MonoMergedIntegerSamples[t];
        ArrayWrite(a);
      end;
    end;

    // always the middle
    for t := LoopStart + sample_blend to (LoopEnd - sample_blend) - 1 do
    begin
      a := MonoMergedIntegerSamples[t];
      ArrayWrite(a);
    end;

    // all but last blend back to beginning
    if (u < iRepeats) then
    begin
      for t := LoopEnd - sample_blend to LoopEnd do
      begin
        off := t - (LoopEnd - sample_blend);
        a := MonoMergedIntegerSamples[t];
        b := MonoMergedIntegerSamples[LoopStart + off];
        p := off / sample_blend;
        p := p;
        c := round(interpolate(p, 1, a, b));
        ArrayWrite(c);
      end;
    end
    // last play tail unfaded
    else
    begin
      for t := LoopEnd - sample_blend to LoopEnd do
      begin
        a := MonoMergedIntegerSamples[t];
        ArrayWrite(a);
      end;
    end;
  end;

  // loop from loop end to sample end fading OUT volume
  for t := LoopEnd + 1 to SampleEnd - 1 do
  begin
    a := MonoMergedIntegerSamples[t];
    a := round(interpolate(t, a, 0, LoopEnd, SampleEnd - 1));
    ArrayWrite(a);
  end;

  // if idx <> length(temp) then
  // raise exception.create('IDX='+inttostr(idx-1)+' length(temp)='+inttostr(length(temp)));
  PlayRawSoundMono(@temp[0], length(temp) * 2, FStream.samplerate);

end;

function TfrmSoundEditor.GetStream: TSoundSTream;
begin
  result := FStream;
end;

function TfrmSoundEditor.IndexOfMarker(sm: TSoundMarker): integer;
begin
  result := FMarkers.IndexOf(sm);
end;

function TfrmSoundEditor.IndexOfParamChannel(iConstant: integer): integer;
var
  t: integer;
begin
  result := -1;
  for t := 0 to FParamChannels.count - 1 do
  begin
    if FParamChannels[t].Constant = iConstant then
    begin
      result := t;
      break;
    end;
  end;

end;

procedure TfrmSoundEditor.InitParamChannels;
begin
  FParamChannels := TList<TParamChannel>.Create;
end;

function TfrmSoundEditor.IsInMarker(x: int64): boolean;
var
  t: integer;
  m: TSoundMarker;
begin
  for t := 0 to FMarkers.count - 1 do
  begin
    m := FMarkers[t];
    if (m.StartPoint >= x) then
      continue;
    if (m.EndPoint <= x) then
      continue;

    result := true;
    exit;
  end;
  result := false;
end;

procedure TfrmSoundEditor.LoadCurves;
var
  t: integer;
begin
  FreeAndClearParamChannels;

  for t := 0 to MAX_PARAM_CHANNELS - 1 do
  begin
    ParamChannels[t].LoadFromFile(LoadedMetaFrom);
  end;


  DataChanged;
  CurvesNeedSaving := false;

end;

procedure TfrmSoundEditor.LoadEvents(sFile: string);
var
  sl: TStringlist;
  sm: TSoundMarker;
  t: integer;
begin
  sl := TStringlist.Create;
  try
    ClearEvents;

    if not fileexists(sFile) then
      exit;
    sl.LoadFromFile(sFile);
    for t := 0 to sl.count - 1 do
    begin
      if (t mod 3) <> 0 then
        continue;

      sm := TSoundMarker.Create;
      sm.MarkerType := strtoint(sl[t + 0]);
      sm.StartPoint := strtoint64(sl[t + 1]);
      sm.EndPoint := strtoint64(sl[t + 2]);
      FEvents.add(sm);
    end;

  finally
    sl.free;
  end;

end;

procedure TfrmSoundEditor.LoadFromFile(sAdaOrMp3File: ansistring; sMetaFile: string = '');
begin
  if sMetaFile = '' then
    sMetaFile := sAdaOrMp3File;

  StopPlaying;
  NoPaint := true;
  try

    FSampleStart := -1;
    FSampleend := -1;
    FLoopStart := -1;
    FLoopend := -1;

    if FStream <> nil then
    begin
      //FSamples := nil;
      FStream.Free;
      FStream := nil;
      // FSamples := Nil;
      FSampleStart := -1;
      FSampleend := -1;
      FLoopStart := -1;
      FLoopend := -1;
      // FViewStart := 0;
      // FViewend := 0;
    end;

    FStream := TSoundStream.Create(sAdaOrMp3File, fmOpenREad+fmShareDenyNone);
    try
      FOsc_ForPlayback := TSoundStreamOscillator.create;
      FOsc_ForPlayback.FileName := sAdaOrMp3File;


//      miscroutines.MoveMem32(FData, FStream.Memory, FStream.Size);
//      FChannels := siptr(PAnsiChar(@PAnsiChar(FData)[0]) + SOUND_IDX_CHANS)^;
//      FSampleStart := iptr(PAnsiChar(@PAnsiChar(FData)[0]) + SOUND_IDX_CUE1)^;
//      FSampleend := iptr(PAnsiChar(@PAnsiChar(FData)[0]) + SOUND_IDX_CUE2)^;
//      FLoopStart := iptr(PAnsiChar(@PAnsiChar(FData)[0]) + SOUND_IDX_Loop1)^;
//      FLoopend := iptr(PAnsiChar(@PAnsiChar(FData)[0]) + SOUND_IDX_Loop2)^;
//      FSampleStart := iptr(PAnsiChar(@PAnsiChar(FData)[0]) + SOUND_IDX_CUE1)^;
//      FSampleend := iptr(PAnsiChar(@PAnsiChar(FData)[0]) + SOUND_IDX_CUE2)^;
//      FLoopStart := iptr(PAnsiChar(@PAnsiChar(FData)[0]) + SOUND_IDX_Loop1)^;
//      FLoopend := iptr(PAnsiChar(@PAnsiChar(FData)[0]) + SOUND_IDX_Loop2)^;


      if (FViewend = 0) or (FViewend > SAmpleLength - 1) then
      begin
        FViewStart := 0;
        FViewend := SAmpleLength - 1;
      end;

    finally
      //FStream.free;
    end;

    LoadedFrom := sAdaOrMp3File;
    LoadedMetaFrom := sMetaFile;

    LoadMarkers();

    ValidateView;
    ValidateSelection;

    LoadCurves;
    AfterLoadFromFile;
    ZoomTo(0, SAmpleLength - 1);

    FileName := sAdaOrMp3File;

  finally
    NoPaint := false;
  end;

  DataChanged;

end;

procedure TfrmSoundEditor.LoadMarkers();
var
  sl: TStringlist;
  sm: TSoundMarker;
  t: integer;
  sFile: string;
begin
  sl := TStringlist.Create;
  try
    ClearMarkers;

    sFile := changefileext(LoadedMetaFrom, '.mark');
    if not fileexists(sFile) then
      exit;
    sl.LoadFromFile(sFile);
    for t := 0 to sl.count - 1 do
    begin
      if (t mod 3) <> 0 then
        continue;

      sm := TSoundMarker.Create;
      sm.MarkerType := strtoint(sl[t + 0]);
      sm.StartPoint := strtoint64(sl[t + 1]);
      sm.EndPoint := strtoint64(sl[t + 2]);
      FMarkers.add(sm);
    end;

  finally
    sl.free;
  end;

end;

procedure TfrmSoundEditor.LoadMetaData;
begin
  LoadMarkers;
  LoadCurves;

end;

function TfrmSoundEditor.SampleToBeat(iSample: int64): nativefloat;
begin
  if Tempo = 0 then
    result := 0
  else
    result := iSample / (44100/(Tempo/60));

end;

procedure TfrmSoundEditor.SaveCurves;
var
  t: integer;
begin
  for t := 0 to FParamChannels.count - 1 do
  begin
    FParamChannels[t].SaveToFile(LoadedMetaFrom);
  end;


  DataChanged;
  CurvesNeedSaving := false;

end;

procedure TfrmSoundEditor.SaveLoop(sBoogerFile: ansistring);
begin
//  soundtools.SaveSoundData(sBoogerFile, @FSamples[SampleStart], 2 *
//      (SampleEnd - SampleStart + 1), 0, SampleEnd - SampleStart + 1,
//    LoopStart - SampleStart, LoopEnd - SampleStart);
end;

procedure TfrmSoundEditor.SaveMarkers();
begin
  FMarkers.SaveToFile(changefileext(LoadedMetaFrom, '.mark'));
end;

procedure TfrmSoundEditor.SaveMetaData;
begin
  SaveMarkers;
  SaveCurves;
end;

procedure TfrmSoundEditor.SaveToFile(sBoogerFile: ansistring);
begin
//  soundtools.SaveSoundData(sBoogerFile, @FSamples[0], DataLength, SampleStart,
//    SampleEnd, LoopStart, LoopEnd);
  LoadedFrom := sBoogerFile;
  FileName := sboogerFile;
end;
{ TsoundMarkerList }

procedure TsoundMarkerList.AppendFromFile(sFile: string;
  iBaseMarkerType: integer);
{$IFDEF SOUND_MARKER_STRING_FILE_FORMAT}
var
  sl: TStringlist;
  sm: TSoundMarker;
  t: integer;
begin
  sl := TStringlist.Create;
  try

    if not fileexists(sFile) then
      exit;
    sl.LoadFromFile(sFile);
    for t := 0 to sl.count - 1 do
    begin
      if (t mod 3) <> 0 then
        continue;

      sm := TSoundMarker.Create;
      sm.MarkerType := strtoint(sl[t + 0]) + iBaseMarkerType;
      sm.StartPoint := strtoint64(sl[t + 1]);
      sm.EndPoint := strtoint64(sl[t + 2]);
      add(sm);
    end;

  finally
    sl.free;
  end;
end;
{$ELSE}

var
  sm: TSoundMarker;
  t: integer;
  fs: TSoundEditorFileStream;
begin
  if not fileexists(sFile) then
    exit;

  fs := TSoundEditorFileStream.Create(sFile, fmOpenread + fmShareDenyNone);
  fs.BufferSize := SOUND_EDITOR_FILE_BUFFER_SIZE;
  try
    while ((fs.Position + sizeof(TsoundMarkerData)) <= (fs.Size)) do
    begin
      sm := TsoundMarker.create;
      sm.ReadFromStream(fs);
      Add(sm);
    end;





  finally
    fs.free;
  end;
end;
{$ENDIF}

procedure TsoundMarkerList.ClearAndFree;
begin
  while (self.count > 0) do
  begin
    self[self.count - 1].free;
    self.delete(self.count - 1);
  end;

end;

procedure TsoundMarkerList.LoadFromFile(sFile: string);
begin
  ClearAndFree;
  AppendFromFile(sFile, 0);
end;

procedure TsoundMarkerList.SaveToFile(sFile: string);
var
  f1,f2: string;
begin

  f1 := sfile;
  f2 := sfile+'.temp';

  if fileexists(f2) then
    deletefile(f2);

  try

    SaveToFile_direct(f2);
    if not CompareFiles(f1,f2) then begin
      SaveToFile_direct(f1);
    end;
  finally
    if fileexists(f2) then
      deletefile(f2);

  end;

end;

procedure TsoundMarkerList.SaveToFile_Direct(sFile: string);
{$IFDEF SOUND_MARKER_STRING_FILE_FORMAT}
var
  sl: TStringlist;
  sm: TSoundMarker;
  t: integer;
begin

  sl := TStringlist.Create;
  try
    Sort;
    for t := 0 to self.count - 1 do
    begin
      sm := self[t];
      sl.add(inttostr(sm.MarkerType));
      sl.add(inttostr(sm.StartPoint));
      sl.add(inttostr(sm.EndPoint));
    end;

    sl.SaveToFile(sFile);

  finally
    sl.free;
  end;

end;
{$ELSE}

var
  fs: TSoundEditorFileStream;
  t: integer;
begin
  if fileexists(sFile) then
    deletefile(sFile);

  if count = 0 then exit;

  fs := TSoundEditorFileStream.Create(sFile, fmCreate);
  fs.BufferSize := SOUND_EDITOR_FILE_BUFFER_SIZE;
  try
    for t := 0 to count - 1 do
    begin
      self[t].WriteToStream(fs);

    end;
  finally
    fs.free;
  end;
end;

procedure TsoundMarkerList.SetVelocityToIndex;
var
  t: integer;
begin
  for t:= 0 to count-1 do begin
    self[t].Velocity := t;
  end;
end;

procedure TsoundMarkerList.Sort(var iTrack: integer);
var
  t: integer;
  bSorted: boolean;
begin
  bSorted := false;

  while not bSorted do
  begin
    bSorted := true;
    for t := 0 to count - 2 do
    begin
      if (self[t].StartPoint > self[t + 1].StartPoint) then
      begin
        if iTrack = t then
          inc(iTrack)
        else
        if iTrack = (t+1) then
          dec(iTrack);

        self.Swap(t, t + 1);
        bSorted := false;
      end;
    end;

  end;
end;

{$ENDIF}

procedure TsoundMarkerList.Sort;
var
  iTrack: integer;
begin
  iTrack := 0;
  sort(iTrack);

end;


procedure TsoundMarkerList.Swap(i1, i2: integer);
var
  sm: TSoundMarker;
begin
  sm := self[i1];
  self[i1] := self[i2];
  self[i2] := sm;

end;

{ TSoundMarker }

function TSoundMarker.GetCaption: string;
begin
  result := Data.caption
end;

procedure TSoundMarker.ReadFromStream(s: TStream);
begin
  s.Read(Data, sizeof(Data));

end;

procedure TSoundMarker.SetCaption(const Value: string);
begin
  data.caption := value;
end;

procedure TSoundMarker.WriteToStream(s: TStream);
begin
  s.Write(Data, sizeof(Data));


end;

{ TCurveList }

procedure TCurveList.CopyFrom(clSource: TCurveList);
var
  t: integer;
  c: TCurve;
begin
  for t:= 0 to clSource.count-1 do begin
    c := Tcurve.Create;
    c.CopyFrom(clSource[t]);
    add(c);
    Sortcurve;

  end;
end;

procedure TCurveList.CreateInitialCurve;
begin
  if count = 0 then
  begin
    add(TCurve.Create);
  end;
end;

procedure TCurveList.DeGlitch;
var
  t: integer;
  c1,c2,c3: TCurve;
begin
  for t:= self.count-1 downto 2 do begin
    c1 := self[t];
    c2 := self[t-1];
    c3 := self[t-2];
    if c1.level = c3.level then begin
      self.DeleteAndFree(t-1);
    end;
  end;
end;

procedure TCurveList.DeleteAndFree(idx: integer);
begin
  items[idx].free;
  delete(idx);
end;

destructor TCurveList.Destroy;
begin

  FreeAndClearObjects;

  inherited;
end;

procedure TCurveList.FreeAndClearObjects;
var
  t: integer;
begin
  for t := count - 1 downto 0 do
  begin
    self[t].free;
    self.delete(t);
  end;

end;

function TCurveList.GetCurveBefore(iSample: int64): integer;
var
  idx: integer;
begin
  idx := 0;
  while (idx < count) and (iSample > self[idx].Sample) do begin
    inc(idx);
  end;
  dec(idx);
  if idx < 0 then
    idx := count-1;


  result := idx;

end;

function TCurveList.GetInterpolatedValue(iSample: int64): nativefloat;
var
  i1,i2: integer;
begin
  if count = 0 then begin
    result := -1;
    exit;
  end;
  i1 := GetCurveBefore(iSAmple);
  i2 := i1 + 1;
  i2 := i2 mod count;
  if i1 < 0 then
    result := -1
  else begin
    result := (iSample-self[i1].Sample)/(self[i2].Sample-self[i1].sample);
    result := (result * (self[i2].Level-self[i1].level))+self[i1].level;
  end;
end;

procedure TCurveList.LoadFromFile(sFile: string);
var
  mfs: TSoundEditorFileStream;
begin
  FreeAndClearObjects;

  if not fileexists(sFile) then
    exit;
  mfs := TSoundEditorFileStream.Create(sFile, fmOpenread + fmShareDenyWrite);
  mfs.BufferSize := SOUND_EDITOR_FILE_BUFFER_SIZE;
  try

    LoadFromStream(mfs);
  finally
    mfs.free;
  end;
end;

procedure TCurveList.LoadFromStream(s: TStream);
var
  t: int64;
  cv: TCurve;
begin
  while s.Position < s.Size do
  begin
    cv := TCurve.Create;
    add(cv);
    cv.LoadFromStream(s);
  end;

end;

function TCurveList.MaxLevel: nativefloat;
var
  t: integer;
begin
  result := 0;
  for t:= 0 to count-1 do begin
    if self[t].Level > result then
      result := self[t].Level;
  end;
end;

procedure TCurveList.Normalize;
var
  t: integer;
  iAX: integer;
  r, rMin, rMax: nativefloat;
begin
  rMin := 1.0;
  rMax := 0.0;
  for t:= 0 to self.count-1 do begin
    r := self[t].Level;
    if r < rMin then rMin := r;
    if r > rMax then rMax := r;
  end;


  for t:= 0 to self.count-1 do begin
    r := self[t].Level;
    if rmax = rMin then
      r := 0
    else
      r := (r - rMin) / (rmax-rMin);

    self[t].Level := r;
  end;
end;

procedure TCurveList.NormalizeTo(r: nativefloat);
var
  t: integer;
begin
  if r = 0 then exit;

  for t:= 0 to count-1 do begin
    self[t].Level := self[t].Level / r;
  end;

end;

procedure TCurveList.Posterize(iLevels: integer);
var
  t: integer;
begin
  for t:= 0 to count-1 do begin
    self[t].level := round(self[t].level * iLevels) / ilevels;
  end;

end;

procedure TCurveList.SaveToFile(sFile: string);
var
  f1,f2: string;
begin

  f1 := sfile;
  f2 := sfile+'.temp';

  if fileexists(f2) then
    deletefile(f2);

  try

    SaveToFile_direct(f2);
    if not CompareFiles(f1,f2) then begin
      SaveToFile_direct(f1);
    end;
  finally
    if fileexists(f2) then
      deletefile(f2);

  end;

end;

procedure TCurveList.SaveToFile_Direct(sFile: string);
var
  mfs: TSoundEditorFileStream;
  sl: TStringlist;
  t: integer;
begin
  if fileexists(sFile) then
    deletefile(sFile);

  if count = 0 then
    exit;

  mfs := TSoundEditorFileStream.Create(sFile, fmCreate);
  mfs.BufferSize := SOUND_EDITOR_FILE_BUFFER_SIZE;
  try
    SaveToStream(mfs);
  finally
    mfs.free;
  end;

{$IFDEF GENERATE_CURVE_DEBUG}
  sl := Tstringlist.create;
  try
    for t:= 0 to count-1 do begin
      sl.add(inttostr(self[t].Sample)+','+floattostr(self[t].level));
    end;
    sl.SaveToFile(sFile+'.debug');

  finally
    sl.free;
  end;
{$ENDIF}

end;

procedure TCurveList.SaveToStream(s: TStream);
var
  t: integer;
begin
  for t := 0 to count - 1 do
  begin
    self[t].SaveToStream(s);
  end;

end;

function TCurveList.SortCurve: boolean;
// returns true if anything was sorted
var
  t: integer;
  bGood: boolean;
  c1, c2, c3: TCurve;
begin
  result := false;

  bGood := false;

  while not bGood do
  begin
    bGood := true;

    for t := 0 to count - 2 do
    begin
      c1 := items[t];
      c2 := items[t + 1];
      if c1.sample < 0 then c1.sample := 0;
      if c2.sample < 0 then c2.sample := 0;

      if c1.Sample > c2.Sample then
      begin
        items[t + 1] := c1;
        items[t] := c2;
        bGood := false;
        result := true;
      end;
    end;
  end;

end;

{ TParamChannel }

procedure TParamChannel.ClearAxies;
begin
  while FAxies.count > 0 do begin
    FAxies[0].free;
    fAxies.delete(0);
  end;
end;

procedure TParamChannel.ClearAxisData;
var
  t: integer;
begin
  for t:= 0 to Axiscount-1 do begin
    while FAxies[0].Count > 0 do begin
      FAxies[0].Free;
      FAxies.Delete(0);
    end;

  end;

end;

procedure TParamChannel.ClearMarkers;
begin
  FMarkers.clearandFree;

end;

constructor TParamChannel.Create;
begin
  inherited;
  FAxies := TList<TCurveList>.Create;
  FMarkers := TSoundMarkerList.create;

  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  FAxies.add(TCurveList.Create);
  // FAxies[0].CreateInitialCurve;
  // FAxies[1].CreateInitialCurve;
  // FAxies[2].CreateInitialCurve;
  // FAxies[3].CreateInitialCurve;

end;

destructor TParamChannel.Destroy;
begin
  FAxies.free;
  FMarkers.Free;
  FAxies := nil;
  Fmarkers := nil;

  inherited;
end;

function TcurveList.FindLastIndexFromSample(iSample: integer): integer;
var
  t: integer;
begin
  result := 0;
  for t:= 0 to count-1 do begin
    if self[t].sample > iSample then begin
      result := t;
      exit;
    end;
  end;

  result := result -1;

end;

function TParamChannel.GetAxisCount: integer;
begin
  result := FAxies.count;
end;

function TParamChannel.GetCurve(idx: integer): TCurveList;
begin
  result := FAxies[idx];
end;

function TParamChannel.GetCurveFileName(sBoogerFile: string;
  Channel, Axis: integer): string;
begin
  result := changefileext(sBoogerFile, '.channel.' + inttostr(Channel)
      + '.axis.' + inttostr(Axis) + '.curve');
  result := StringReplace(result, '.originalstream', '', [rfreplaceall,
    rfignorecase]);

end;

function TParamChannel.GetCurveT: TCurveList;
begin
  result := Axies[3];
end;

function TParamChannel.GetCurveX: TCurveList;
begin
  result := Axies[0];
end;

function TParamChannel.GetCurveY: TCurveList;
begin
  result := Axies[1];
end;

function TParamChannel.GetCurveZ: TCurveList;
begin
  result := Axies[2];
end;

function TParamChannel.GetMarkerFileName(sBoogerFile: string; Channel: integer): string;
begin
  result := changefileext(sBoogerFile, '.channel.' + inttostr(Channel)+'.mark');
  result := StringReplace(result, '.originalstream', '', [rfreplaceall,
    rfignorecase]);
end;

function TParamChannel.HasData: boolean;
var
  u: integer;
begin
  result := false;
  for u := 0 to AxisCount - 1 do
  begin
    if Axies[u].count > 0 then
    begin
      result := true;
      exit;
    end;
  end;

end;

procedure TParamChannel.LoadCurves(sBoogerFile: string);
var
  sFile: string;
  t: integer;
begin
  for t := 0 to AxisCount - 1 do
  begin
    sFile := GetCurveFileName(sBoogerFile, Constant, t);

    Axies[t].LoadFromFile(sFile);

  end;
end;

procedure TParamChannel.LoadFromFile(sBoogerFile: string);
begin
  Loadcurves(sBoogerFile);
  LoadMarkers(sBoogerFile);
end;

procedure TParamChannel.LoadMarkers(sBoogerFile: string);
var
  sFile: string;
begin
  FMarkers.clearandfree;
  sFile := GetMarkerFilename(sBoogerFile, FConstant);
  if fileexists(sFile) then begin
    FMarkers.LoadFromfile(sFile);
  end;



end;

procedure TParamChannel.SaveCurves(sBoogerFile: string);
var
  sFile: string;
  t: integer;
begin
  for t := 0 to AxisCount - 1 do
  begin
    sFile := GetCurveFileName(sBoogerFile, Constant, t);

    Axies[t].SaveToFile(sFile);

  end;


end;

procedure TParamChannel.SaveMarkers(sBoogerFile: string);
var
  sFile: string;
begin
  sFile := GetMarkerFilename(sBoogerFile, FConstant);
  FMarkers.SetVelocityToIndex();
  FMarkers.SaveToFile(sFile);

end;

procedure TParamChannel.SaveToFile(sBoogerFile: string);
begin
  SaveCurves(sBoogerFile);
  SaveMarkers(SBoogerfile);
end;

{ TCurve }

procedure TCurve.ApplyConstraints;
begin
  if Data.level > 1.0 then Data.Level := 1.0;
  if Data.Level < 0.0 then Data.Level := 0;
end;

procedure TCurve.CopyFrom(cSource: TCurve);
begin
  movemem32(@Data, @(cSource.Data), SizeOf(TCurveRec));

end;

constructor TCurve.create;
begin
  inherited;
  Data.Interpolation := itLinear;
end;

procedure TCurve.LoadFromStream(s: TStream);
begin
  Stream_GuaranteeRead(s, @Data, sizeof(Data));

end;

procedure TCurve.SaveLevel;
begin
  FLevelAtMouseDown := Data.Level;
end;

procedure TCurve.SaveToStream(s: TStream);
begin
  Stream_GuaranteeWrite(s, @Data, sizeof(Data));

end;


procedure TCurve.SetLevel(const Value: single);
begin
  Data.Level := value;
end;


procedure TfrmSoundEditor.ToneGate(iOutputChannel, iPitchChannel,
  iPitchAxis, iVelAxis: integer; rLowRange, rhighRange: nativefloat; rOpenGateAt: nativefloat; rCloseGateAT: nativefloat);
var
  rCurrentPitch, rCurrentVel: nativefloat;
  t: integer;
  bOpen: boolean;
  m: TSoundMarker;
begin
  m := nil;
  try
    bOpen := false;
    for t:= 0 to SampleLength-1 do begin
      if (t mod 100) <> 0 then continue;
      rCurrentPitch :=  ParamChannels[iPitchChannel].Axies[iPitchAxis].GetInterpolatedValue(t);
      rCurrentVel :=  ParamChannels[iPitchChannel].Axies[iVelAxis].GetInterpolatedValue(t);
      if not bOpen then begin
        if (rCurrentPitch <= rHighRange)
        and (rCurrentPitch >= rLowRange)
        and (rCurrentVel >= rOpenGateAt) then begin
          m := TSoundMarker.create;
          m.StartPoint := t;
          bopen := true;
        end;
      end else begin
        if (rCurrentPitch > rHighRange)
        or (rCurrentPitch < rLowRange)
        or (rCurrentVel <= rCloseGateAt) then begin
          m.EndPoint := t;
          ParamChannels[iOutputChannel].Markers.add(m);
          DrawWave;
          m := nil;
          bOpen := false;
        end;
      end;
    end;
  finally
    m.free;
    m := nil;
  end;


end;

procedure TfrmSoundEditor.TransientDetection(iOutputChannel, iPitchChannel,
  iPitchAxis, iVelAxis: integer; rOpenGateAt: nativefloat; rCloseGateAT: nativefloat);
var
  rCurrentPitch, rCurrentVel: nativefloat;
  t: integer;
  bOpen: boolean;
  m: TSoundMarker;
  rLastPitch: nativefloat;
  ilastPitchIndex, iPitchIndex: integer;
  rLastVelocity: nativefloat;
  bOpening: boolean;
begin
  m := nil;
  try
    bOpen := false;
    rLastPitch := -1;
    rLastVelocity := ParamChannels[iPitchChannel].Axies[iVelAxis].GetInterpolatedValue(0);;
    bopening := false;
    for t:= 0 to SampleLength-1 do begin
      if (t mod 100) <> 0 then continue;
      rCurrentPitch :=  ParamChannels[iPitchChannel].Axies[iPitchAxis].GetInterpolatedValue(t);
      rCurrentVel :=  ParamChannels[iPitchChannel].Axies[iVelAxis].GetInterpolatedValue(t);
      iPitchIndex :=  ParamChannels[iPitchChannel].Axies[iPitchAxis].FindLastIndexfromSample(t);

      //if the gate is CLOSED
      if not bOpen then begin
        if ((rCurrentVel-rLastVelocity) >= rOpenGateAt) then begin
          if not bopening then begin
            m := TSoundMarker.create;
            m.StartPoint := t;
            bopen := true;
            bopening := true;
          end;
        end;
      end
      //if the gate is OPEN
      else begin
        //if the gate is CLOSING
        if (rCurrentVel <= rCloseGateAt) then begin
          m.EndPoint := t;
          ParamChannels[iOutputChannel].Markers.add(m);
          if ParamChannels[iOutputChannel].Markers.Count mod 10 = 0 then
            DrawWave;


          m := nil;
          bOpen := false;
          bopening := false;
        end
        else begin
          if ((rCurrentVel-rLastVelocity) >= rOpenGateAt) then begin
            if not bOpening then begin
              m.EndPoint := t;
              ParamChannels[iOutputChannel].Markers.add(m);
              if ParamChannels[iOutputChannel].Markers.Count mod 10 = 0 then
                DrawWave;
              m := nil;
              m := TSoundMarker.create;
              m.StartPoint := t;
              bOpening := true;
            end;
          end else begin
            if bOpening then begin
              bOpening := rcurrentVel > rLastVelocity;
            end;
          end;
          //if the pitch is changing
//          if (rLastPitch <> rCurrentPitch) and (iLastPitchIndex <> iPitchIndex) then begin
//            m.EndPoint := t;
//            ParamChannels[iOutputChannel].Markers.add(m);
//            DrawWave;
//            m := nil;
//            m := TSoundMarker.create;
//            m.StartPoint := t;
//          end;
        end;

      end;
      rLastPitch := rCurrentpitch;
      iLastPitchIndex := iPitchIndex;
      rLastVelocity := rCurrentVel;
    end;
  finally
    m.free;
    m := nil;
  end;


end;



procedure TfrmSoundEditor.TransientDetectionFromSpectrum(iOutputChannel,
  iInputChannel: integer; rOpenGateAt, rCloseGateAT: nativefloat);
var
  t,u: integer;
  bOpen: boolean;
  m: TSoundMarker;
  rC, rL: nativefloat;
  bOpening: boolean;
  bTemp: boolean;
begin
  ParamChannels[iOutputChannel].Markers.ClearAndFree;
  m := nil;
  try
    bOpen := false;
    bopening := false;
    for t:= 0 to SampleLength-1 do begin
      if (t mod 100) <> 0 then continue;

      //if the gate is CLOSED
      if not bOpen then begin
        for u := 0 to 11 do begin

          rC := ParamChannels[iInputChannel].Axies[u].GetInterpolatedValue(t);
          if t < 100 then
            rL := 0
          else
            rL := ParamChannels[iInputChannel].Axies[u].GetInterpolatedValue(t-100);
          if ((rC-rL) >= rOpenGateAt) and (rC > rCloseGateAt) then begin
            if not bopening then begin
              m := TSoundMarker.create;
              m.StartPoint := t;
              bopen := true;
              bopening := true;
              break;
            end;
          end;
        end;
      end
      //if the gate is OPEN
      else begin
        //if the gate is CLOSING
        bTemp := true;
        for u := 0 to 11 do begin
          rC := ParamChannels[iInputChannel].Axies[u].GetInterpolatedValue(t);
          if t < 100 then
            rL := 0
          else
            rL := ParamChannels[iInputChannel].Axies[u].GetInterpolatedValue(t-100);
          if (rC >= rCloseGateAt) then begin
            bTemp := false;
          end;
        end;
        if bTemp then begin
          m.EndPoint := t;
          ParamChannels[iOutputChannel].Markers.add(m);
          if ParamChannels[iOutputChannel].Markers.Count mod 10 = 0 then
            DrawWave;
          m := nil;
          bOpen := false;
          bopening := false;
        end
        else begin
          bTemp := false;
          for u := 0 to 11 do begin
            rC := ParamChannels[iInputChannel].Axies[u].GetInterpolatedValue(t);
            if t < 100 then
              rL := 0
            else
              rL := ParamChannels[iInputChannel].Axies[u].GetInterpolatedValue(t-100);
            if ((rC-rL) >= rOpenGateAt) then begin
              bTemp := true;
              break;
            end;
          end;
          if bTemp then begin
              if not bOpening then begin
                m.EndPoint := t;
                ParamChannels[iOutputChannel].Markers.add(m);
                if ParamChannels[iOutputChannel].Markers.Count mod 10 = 0 then
                  DrawWave;
                m := nil;
                m := TSoundMarker.create;
                m.StartPoint := t;
                bOpening := true;
              end;

          end else begin
            if bOpening then begin
              bopening := false;
              for u := 0 to 11 do begin
                rC := ParamChannels[iInputChannel].Axies[u].GetInterpolatedValue(t);
                if t < 100 then
                  rL := 0
                else
                  rL := ParamChannels[iInputChannel].Axies[u].GetInterpolatedValue(t-100);

                if ((rC > rL)) then begin
                  bopening := true;
                  break;
                end;
              end;
            end;
          end;
        end;

      end;
    end;
  finally
    m.free;
    m := nil;
  end;


end;


{ TSoundMarkerData }

function TSoundMarkerData.GetCaption: string;
begin
  result := pchar(@FCaption[0]);
end;

procedure TSoundMarkerData.SetCaption(const Value: string);
begin
  fillmem(@FCaption[0], sizeof(Fcaption), 0);
  MOVEMEM32(@Fcaption[0], @VAlue[STRZ], length(value)*sizeof(char));
end;

end.
