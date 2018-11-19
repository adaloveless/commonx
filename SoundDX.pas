unit SoundDX;

interface

uses
  System.SysUtils, System.Classes, DXSounds, soundtools, sharedobject, soundtoolsex, soundinterfaces, forms,orderlyinit,
  betterobject,  wave, winapi.windows, generics.collections.fixed, typex,  mmsystem, soundsample;

type
  TDSoundOscillatorRenderer = class;//forward (TCustomWaveStream2, ISoundOscillatorREnderer)

  TDXSoundEngine = class(TSharedObject)
  private
    procedure DXSoundFinalize(Sender: TObject);
    procedure DXSoundInitialize(Sender: TObject);
    { Private declarations }

  public
    RenderingStream: TAudioStream;
    DXSound: TDXSound;
    Renderer: TDSoundOscillatorREnderer;
    Initialized: boolean;
    constructor create;reintroduce;virtual;
    destructor destroy;override;
    function Initialize: boolean;


    { Public declarations }
  end;

  TDSoundOscillatorRenderer = class (TCustomWaveStream2, ISoundOscillatorREnderer)
  private
    sect: _RTL_CRITICAL_SECTION;
    FRefCount: Integer;
    FLocalBufferSize: nativeint;
    FLAstPosition: nativeint;
    FIndiePos: int64;
    FFreeWithReferences: boolean;
    FOscillators: TList<TSoundOscillator>;
    FEngine: TDXSoundEngine;
    dxAudioStream: TAudioStream;
    Initialized: boolean;
    procedure SetLocalBufferSize(const Value: nativeint);
    function GEtNyquistFrequency: nativeint;
    function GEtSampleRate: nativeint;
    function GetBufferSize: nativeint;
    procedure SetBufferSize(const value: nativeint);
    procedure RenderSample(iStreamPosition: int64;
      var sample: T16BitStereoSoundSample);
    function GEtOscillator(idx: integer): TSoundOscillator;
    function GEtOscillatorCount: nativeint;
  public
    //ssa: array[0..44099] of T16BitstereoSoundSample;
    FLocalBuffer: PbyteArray;

    constructor Create(engine: TDXSoundEngine = nil);reintroduce;virtual;
    destructor Destroy;override;
    function ReadWave(var Buffer; Count: Longint): Longint; override;
    procedure Initialize;

    procedure RenderSamples(iStartPosition, iStartBufferPosition: nativeint; iCount: nativeint);
    property LocalBufferSize: nativeint read FLocalBufferSize write SetLocalBufferSize;


    //-------------------------------
    property SampleRate: nativeint read GEtSampleRate;
    property NyquistFrequency: nativeint read GEtNyquistFrequency;



    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property RefCount: Integer read FRefCount;
    property FreeWithReferences: boolean read FFreeWithReferences write FFreeWithReferences;
    function GetPlayPosition: int64;
    property PlayPosition: int64 read GetPlayPosition;

    function AddOscillator(p: TSoundOscillatorHookSSE; o: TObject): TSoundOscillator; overload;
    procedure AddOscillator(o: ToscillatorObject); overload;
    procedure RemoveOscillator(o: ToscillatorObject); overload;
    procedure RemoveOscillator(p: TSoundOscillatorHookSSE; o: TObject); overload;

    procedure Lock;
    procedure Unlock;

    function GetEngineSTartTime: int64;
    procedure SetEngineStartTime(const Value: int64);
    property EngineStartTime : int64 read GetEngineSTartTime write SetEngineStartTime;
    property Engine: TDXSoundEngine read FEngine;
    property Oscillators[idx: integer]: TSoundOscillator read GEtOscillator;
    property OscillatorCount: nativeint read GEtOscillatorCount;

  end;


procedure ShutDownAudio;



var
  DXS: TDXSoundEngine;


implementation

{ TDXSoundEngine }

constructor TDXSoundEngine.create;
begin
  inherited CReate;

  Renderer := TDSoundOscillatorRenderer.Create(self);
end;

destructor TDXSoundEngine.destroy;
begin
  DXSound.Free;
  inherited;
end;




procedure TDXSoundEngine.DXSoundFinalize(Sender: TObject);
begin
  RenderingStream.Free;
  Renderer.Free;
  RenderingStream := nil;
end;


procedure TDXSoundEngine.DXSoundInitialize(Sender: TObject);
begin

  RenderingStream := TAudioStream.Create(DXSound.DSound);
  RenderingStream.WaveStream := Renderer;
  RenderingStream.AutoUpdate := True;
  RenderingStream.BufferLength := 1000;
  RenderingStream.Play;

end;


function TDXSoundEngine.Initialize: boolean;
begin
  result := initialized;
  if initialized then exit;
  Lock;
  try
    DXSound := TDXSound.Create(application.mainform);
    DXSound.OnInitialize := self.DXSoundInitialize;
    DXSound.OnFinalize := self.DXSoundFinalize;
    DXSound.Options := DXSound.Options + [soGlobalFocus];
    DXSound.Initialize;
  finally
    Unlock;
  end;
  Initialized := true;
  result := initialized;
end;


constructor TDSoundOscillatorRenderer.Create(engine: TDXSoundEngine = nil);
var
  t: integer;
begin
  inherited CReate;
  if engine = nil then
    Fengine := SoundDX.DXS
  else
    self.FEngine := engine;


  FOscillators := TList<TSoundOscillator>.create;
  InitializeCriticalSection(sect);
  SetFormatSize(sizeof(TWaveFormatEx));
  Self.SetPCMFormat(44100, 16, 2);





end;

destructor TDSoundOscillatorRenderer.Destroy;
begin
  LocalBufferSize := 0;//<<---frees FLocalBuffer
  DeleteCriticalSection(sect);
  FOscillators.Free;
  FOscillators := nil;
  dxaudiostream.Free;
  inherited;
end;

function TDSoundOscillatorRenderer.ReadWave(var Buffer;
  Count: Integer): Longint;
var
  ss: T16BitStereoSoundSample;
  pb :PByteArray;
  t: integer;
  iToWrite: nativeint;
  iWriteFrom: nativeint;
  ssa: P16BitStereoSoundSampleArray;
begin

  inherited;
  if (FIndiePos mod 4) <> 0 then
    raise exception.create('this doesn''t support positions != multiples of 4');

  if (count mod 4) <> 0 then
    raise exception.create('this doesn''t support counts != multiples of 4');




  //if count < 8820 then
  //Debug('buf='+inttostr(position)+'+'+inttostr(count));
//  Debug('delta='+inttostr(FIndiePos-Flastposition));


  if LocalBufferSize < count then
    LocalBufferSize := count;

  ssa := P16BitStereoSoundSampleArray(FlocalBuffer);
  try
    self.RenderSamples(FIndiePos div 4, 0, count div 4);

    //iToWrite := lesserof(count, sizeof(ssa)-(Position mod sizeof(ssa)));
    iToWrite := count;//((count div 4) * 4);
    //iWRiteFrom :=position mod sizeof(ssa);
    iWriteFrom := 0;
    pb := PByteArray(@ssa^[0]);
    move(pb^[iWriteFrom], buffer, iToWrite);
    result := iToWrite;
//    position := position + iToWrite;
    FIndiePos := FIndiePos + iToWrite;
  finally
//    FreeMemory(ssa);
  end;

  FLAstPosition := FIndiePos;


end;

procedure TDSoundOscillatorRenderer.RemoveOscillator(o: ToscillatorObject);
begin
  RemoveOscillator(o.oo, o);

//  gflag := true;

end;

procedure TDSoundOscillatorRenderer.RemoveOscillator(p: TSoundOscillatorHookSSE;
  o: TObject);
var
  t: integer;
begin
  Lock;
  try
    for t := FOscillators.count - 1 downto 0 do begin
      if (@FOscillators[t].HookSSE = @p) and (FOscillators[t].Obj = o) then begin
        FOscillators[t].free;
        FOscillators.delete(t);
      end;
    end;

  finally
    Unlock;
  end;
end;

procedure TDSoundOscillatorRenderer.RenderSample(iStreamPosition: int64; var sample: T16BitStereoSoundSample);
var
  rTemp, rCurrent: TStereoSoundSample;
  t: integer;
  rLeft, rRight: floatsample;

begin


  rCurrent.Left := 0;
  rCurrent.Right := 0;

  for t := 0 to OscillatorCount - 1 do begin
    Oscillators[t].Fill(nativeint(0), mtGetSample, rTemp, iStreamPosition);

    //rTemp := rTemp / rSum;

    rCurrent := rCurrent + rTemp;


    //rCurrent.Left := rCurrent.Left + rTemp.Left;

  end;

  sample.ch[0] := round(rCurrent.Left * 32767);
  sample.ch[1] := round(rCurrent.Right * 32767);

end;
procedure TDSoundOscillatorRenderer.RenderSamples(iStartPosition,
  iStartBufferPosition: nativeint; iCount: nativeint);
var
  t: integer;
  ssa: P16BitStereoSoundSampleArray;
  rJunk: TStereoSoundSample;

begin
  Lock;
  try
    //Debug(inttostr(iStartBufferPosition));
    for t := 0 to OscillatorCount - 1 do begin
      Oscillators[t].fill(0,mtBeginWindow, rJunk, 0);
    end;


    ssa := P16BitStereoSoundSampleArray(FLocalBuffer);
    for t := 0 to iCount-1 do begin
      RenderSample(iStartPosition+t, ssa[iStartBufferPosition+t]);
    end;



    for t := 0 to OscillatorCount - 1 do begin
      Oscillators[t].fill(0, mtEndWindow, rJunk, 0);
    end;
  finally
    Unlock;
  end;


end;

procedure TDSoundOscillatorRenderer.SetBufferSize(const value: nativeint);
begin
  Self.FLocalBufferSize := value;
end;

procedure TDSoundOscillatorRenderer.SetEngineStartTime(const Value: int64);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TDSoundOscillatorRenderer.SetLocalBufferSize(const Value: nativeint);
begin
  if value = FLocalBufferSize then
    exit;

  if FlocalBuffer <> nil then
    FreeMemory(FLocalBuffer);


  FLocalBufferSize := Value;


  if FLocalBufferSize = 0 then
    FlocalBuffer := nil
  else
    FLocalBuffer := GetMemory(FlocalBufferSize);

end;

procedure TDSoundOscillatorRenderer.Unlock;
begin
  LeaveCRiticalSection(sect);
end;

function TDSoundOscillatorRenderer.GetBufferSize: nativeint;
begin
  result := Self.FLocalBufferSize;
end;

function TDSoundOscillatorRenderer.GetEngineSTartTime: int64;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TDSoundOscillatorRenderer.GEtNyquistFrequency: nativeint;
begin
  result := 44100 div 2;
end;

function TDSoundOscillatorRenderer.GEtOscillator(
  idx: integer): TSoundOscillator;
begin
  Lock;
  try
    result := FOscillators[idx];
  finally
    Unlock;
  end;
end;

function TDSoundOscillatorRenderer.GEtOscillatorCount: nativeint;
begin
  Lock;
  try
    result := FOscillators.count;
  finally
    Unlock;
  end;
end;

function TDSoundOscillatorRenderer.GetPlayPosition: int64;
begin
  result := FLAstPosition;
end;

function TDSoundOscillatorRenderer.GEtSampleRate: nativeint;
begin
  result := 44100;
end;

procedure TDSoundOscillatorRenderer.Initialize;
begin
  if not Engine.Initialize then exit;


  if initialized then exit;

  dxAudiostream := TAudioStream.Create(Engine.DXSound.DSound);
  dxAudiostream.WaveStream := self;
  dxAudiostream.AutoUpdate := True;
  dxAudiostream.BufferLength := 10;
  dxAudiostream.Play;

  initialized := true;

end;

procedure TDSoundOscillatorRenderer.Lock;
begin
  EnterCriticalSection(sect);
end;

function TDSoundOscillatorRenderer.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;

end;

function TDSoundOscillatorRenderer._AddRef: Integer; stdcall;
begin
  InterlockedIncrement(FRefCount);
end;
function TDSoundOscillatorRenderer._Release: Integer; stdcall;
begin
{$IFNDEF NO_INTERLOCKED_INSTRUCTIONS}
  result := InterlockedDecrement(FRefCount);
{$ELSE}
  EnterCriticalSection(FRefSect);
  dec(FRefCount);
  Result := FRefCount;
  LeaveCriticalSection(FRefSect);
{$ENDIF}
  if (Result = 0) and FreeWithReferences then
    Destroy;


end;
procedure TDSoundOscillatorRenderer.AddOscillator(o: ToscillatorObject);
var
  osc: TSoundOscillator;
begin
  Initialize;

  if (o.MasterStream <> ISoundOscillatorRenderer(self)) then begin
    o.MasterStream := self;
  end
  else
    self.AddOscillator(o.oo, o);

end;

function TDSoundOscillatorRenderer.AddOscillator(p: TSoundOscillatorHookSSE;
  o: TObject): TSoundOscillator;
var
  osc: TSoundOscillator;
begin
  Initialize;

  osc := nil;
  Lock;
  try
    osc := TSoundOscillator.Create;
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

procedure TDSoundOscillatorRenderer.AfterConstruction;
begin
  //
end;
procedure TDSoundOscillatorRenderer.BeforeDestruction;
begin
  if RefCount > 1 then begin
    FreeWithReferences := true;
    raise EAbort.create('Trying to free '+self.ClassName+' with more than 1 reference');

  end;
end;


procedure ShutDownAudio;
begin
  DXS.Free;
  DXS := nil;

end;


procedure oinit;
begin
  DXS := TDXSoundEngine.Create;
end;

procedure ofinal;
begin
  ShutDownAudio;//YOU SHOULD PROBABLY DO THIS IN THE MAIN FORM!!! Go Ahead...

end;

initialization
  init.RegisterProcs('SoundDX', oinit, ofinal);



finalization






end.
