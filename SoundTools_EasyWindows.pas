unit SoundTools_EasyWindows;
{$Message '********************************Compiling SoundTools_EasyWindows'}

interface

uses
  debug, exe, numbers, winapi.mmsystem, managedthread, classes, typex, systemx, soundinterfaces, commandprocessor, soundtools, sysutils, helpers_stream, backgroundthreads, geometry, stringx, commandicons, soundsample;

type
  TSoundPlayerThread = class(TProcessorThread)
  private
    FMemory: TMemoryStream;
  public
    procedure InitFrompool;override;
    procedure DoExecute; override;
    procedure OnFinish;override;
    property Memory: TMemoryStream read FMemory write FMemory;
  end;

  Tcmd_PlaySoundFromMemory_Mono44100 = class(TSoundCommand)
  private
    FBuf: TRawSoundArrayPOinterMono;
    FLengthLimit: integer;
    FForeignOffset: int64;
    function GetNumberOfSamples: integer;
    function GEtSizeOfBuffer: integer;
    procedure SetBuf(const Value: soundsample.TRawSoundArrayPOinterMono);
  public
    procedure Init; override;
    procedure DoExecute; override;
    property Memory: TRawSoundArrayPOinterMono read FBuf write SetBuf;
    property LengthLimit: integer read FLengthLimit write FLengthLimit;
    property NumberOfSamples: integer read GetNumberOfSamples;
    property SizeOfbuffer: integer read GEtSizeOfBuffer;
    property ForeignOffset: int64 read FForeignOffset write FForeignOffset;
  end;

  Tcmd_PlaySoundFromMemory_MonoOrStereo44100 = class(TSoundCommand)
  private
    FBuf: psmallint;
    FLengthLimit: integer;
    FForeignOffset: int64;
    FChannels: integer;
    FDeviceIndex: nativeint;
    function GetNumberOfSamples: integer;
    function GEtSizeOfBuffer: integer;
  public

    procedure Init; override;
    procedure DoExecute; override;
    destructor Destroy; override;
    property DeviceIndex: nativeint read FDeviceIndex write FDeviceIndex;
    property Memory: psmallint read FBuf write FBuf;
    property LengthLimit: integer read FLengthLimit write FLengthLimit;
    property NumberOfSamples: integer read GetNumberOfSamples;
    property SizeOfbuffer: integer read GEtSizeOfBuffer;
    property ForeignOffset: int64 read FForeignOffset write FForeignOffset;
    property channels: integer read FChannels write FChannels;
  end;

  Tcmd_PlayStream_Partial = class(TCommand)
  private
   FStream: TSoundStream;
    FThread: TAbstractSoundDevice;
    FOsc: TSoundStreamOscillator;
  public
    StartSample: int64;
    EndSample: int64;
    SampleRate: ni;

    procedure DoExecute;override;
    procedure Init; override;
    property Thread: TAbstractSoundDevice read FThread write FThread;
    property Osc: TSoundStreamOscillator read FOsc write FOsc;
  end;



  Tcmd_PlaySoundFromDisk = class(TSoundCommand)
  private
    FFileName: string;
  public
    property fileName: string read FFileName write FFileName;
    procedure DoExecute; override;
  end;

  Tcmd_PlaySoundFromMemory_Stereo44100 = class(TSoundCommand)
  protected
    FBufL, FBufR, FBuf: TRawSoundArrayPOinterMono;
  public
    procedure DoExecute; override;
    procedure Setup; virtual;
    procedure Play; virtual;
    property MemoryL: TRawSoundArrayPOinterMono read FBufL write FBufL;
    property MemoryR: TRawSoundArrayPOinterMono read FBufR write FBufR;
    procedure Multiplex;
    procedure ReplaceBuffers(br, bl: soundsample.TRawSoundArrayPOinterMono);

  end;

  Tcmd_Beep = class(Tcmd_PlaySoundFromMemory_Stereo44100)
  private
    FAttackTime: integer;
    FFreq: integer;
    FReleaseTime: integer;
    FDuration: integer;
    FChorus: integer;
    FManualSetup: boolean;
  public
    procedure Setup; override;
    property Frequency: integer read FFreq write FFreq;
    property AttackTime: integer read FAttackTime write FAttackTime;
    property ReleaseTime: integer read FReleaseTime write FReleaseTime;
    property Duration: integer read FDuration write FDuration;
    property Chorus: integer read FChorus write FChorus;
    property ManualSetup: boolean read FManualSetup write FManualSetup;

    class procedure Beep(freq, Duration, attack, release, Chorus: integer);static;
    class procedure BeepWait(freq, Duration, attack, release, Chorus: integer);static;
    class procedure BeepChord(freq: array of integer; Duration: integer;
      attack: integer = 10; release: integer = 1000);
    class procedure BeepArray(freq: array of integer;
      Duration: array of integer; attack: integer = 10;
      release: integer = 1000);
  end;


  Tcmd_WaveStretch = class(TSoundCommand)
  private
    fNewLength: int64;
    FOutputFile: string;
    FInputFile: string;
    FNewSampleRate: integer;
    FRoundBoundary: boolean;
    FBoundary: integer;
    procedure RecalculateExpense;
    procedure SetInputFile(const Value: string);
    procedure SetOutputFile(const Value: string);
  public
    procedure DoExecute; override;
    property InputFile: string read FInputFile write SetInputFile;
    property OutputFile: string read FOutputFile write SetOutputFile;
    property NewLength: int64 read fNewLength write fNewLength;
    property NewSampleRate: integer read FNewSampleRate write FNewSampleRate;
    property RoundBoundary: boolean read FRoundBoundary write FRoundBoundary;
    property Boundary: integer read FBoundary write FBoundary;

  end;


  Tcmd_WaveToFSB = class(Tcmd_RunExe)
  private
    FInFile: string;
    FDownMix: boolean;
    FQuality: nativefloat;
    FOUtfile: string;
    FFormat: string;
  protected
    procedure BuildCommandLine;override;
  public
    procedure Init;override;
    procedure InitExpense;override;
    procedure DoExecute;override;
    property InFile: string read FInFile write FinFile;
    property OutFile: string read FOUtfile write Foutfile;
    property Quality: nativefloat read FQuality write FQuality;
    property DownMix: boolean read FDownMix write FDownMix;
    property Format: string read FFormat write FFormat;
  end;




function BeginWaveToFSB(sInFile: string; sOutFile: string; sFormat: string; rQuality: nativefloat = 4.0;
  bDownMix: boolean = false; iResample: integer = 44100): Tcmd_WaveToFSB;
procedure EndWaveToFSB(var c: Tcmd_WaveToFSB);

procedure PlayOGG(sFile: string);
procedure PlayRawSoundMono(pdata: pointer; length: integer; SampleRate: cardinal = 44100);
procedure PlayRawSoundStereo(pdata: pointer; length: integer; SampleRate: cardinal = 44100);


function Begin_PlaySound(sFile: string;
  bforget: boolean): Tcmd_PlaySoundFromDisk;
procedure End_PlaySound(var cmd: Tcmd_PlaySoundFromDisk);

procedure Begin_Beep(freq, Duration, attack, release, Chorus: integer);
procedure Begin_BeepArray(freq: array of integer; Duration: array of integer;
    attack, release, Chorus: integer);
procedure Begin_BeepChord(freq: array of integer;
  Duration, attack, release, Chorus: integer);
procedure End_Beep(c: Tcmd_Beep);
procedure End_BeepArray(c: Tcmd_Beep);
procedure End_BeepChord(c: Tcmd_Beep);
function Booger_GetHeader(sFile: string): TboogerHeader;


implementation

uses
  beeper, booger, SOUNDCONVERSION_WINDOWS;




procedure PlayRawSoundStereo(pdata: pointer; length: integer; SampleRate: cardinal = 44100);
const
  main_size_pos = 4;
  data_size_pos = 4;
var
  ms: TMemoryStream;
  fmt: TWaveFormat;
  iPos: int64;
  c: cardinal;
  si: smallint;
  i: integer;
  t: integer;
  amp: nativefloat;
  pc: PAnsiChar;
  ch: ansichar;
  thr: TSoundPlayerThread;
begin
  thr := nil;
  ms := TMemoryStream.Create;
  try
    Stream_guaranteeWrite(ms, @PAnsiChar('RIFF')[0], 4);
    Stream_guaranteeWrite(ms, @PAnsiChar('SIZE')[0], 4);
    // wave chunk
    Stream_guaranteeWrite(ms, @PAnsiChar('WAVE')[0], 4);
    Stream_guaranteeWrite(ms, @PAnsiChar('fmt ')[0], 4);
    // size
    c := 16;
    Stream_guaranteeWrite(ms, Pbyte(@c), 4);
    // compression PCM
    si := 1;
    Stream_guaranteeWrite(ms, Pbyte(@si), 2);

    // 1 channel
    si := 2;
    Stream_guaranteeWrite(ms, Pbyte(@si), 2);

    // sample rate
    i := 44100;
    Stream_guaranteeWrite(ms, Pbyte(@i), 4);

    // bytes per second
    i := 88200*2;
    Stream_guaranteeWrite(ms, Pbyte(@i), 4);

    // block align
    si := 32 div 8;
    Stream_guaranteeWrite(ms, Pbyte(@si), 2);

    // bits per sample
    si := 16;
    Stream_guaranteeWrite(ms, Pbyte(@si), 2);

    // data chunk
    Stream_guaranteeWrite(ms, @PAnsiChar('data')[0], 4);
    i := length;
    Stream_guaranteeWrite(ms, Pbyte(@i), 4);

    pc := PAnsiChar(pdata);
    for t := 0 to length - 1 do begin
      ch := pc[t];
      Stream_guaranteeWrite(ms, Pbyte(@ch), 1);
    end;

    iPos := ms.Position - 8;
    ms.Seek(main_size_pos, 0);
    Stream_guaranteeWrite(ms, Pbyte(@iPos), 4);

    ms.Seek(0, 0);

    thr := TSoundPlayerThread(TPM.NeedThread(TSoundPlayerThread,
        BackGroundthreadMan));
    //thr.FreeOnTerminate := false;
    thr.Loop := false;
    thr.Memory := ms;
    thr.Start;
    thr.WaitForFinish;
// PlaySound(ms.memory, 0, SND_SYNC+SND_NODEFAULT+SND_MEMORY);

  finally
    if (assigned(thr)) then
      TPM.NoNeedthread(thr);
//      thr.ToPool;
    // owned by thread now
    // ms.free;
  end;
end;

procedure PlayRawSoundMono(pdata: pointer; length: integer; SampleRate: cardinal = 44100);
const
  main_size_pos = 4;
  data_size_pos = 4;
var
  ms: TMemoryStream;
  fmt: TWaveFormat;
  iPos: int64;
  c: cardinal;
  si: smallint;
  i: integer;
  t: integer;
  amp: nativefloat;
  pc: PAnsiChar;
  ch: ansichar;
  thr: TSoundPlayerThread;
begin
  thr := nil;
  ms := TMemoryStream.Create;
  try
    Stream_guaranteeWrite(ms, @PAnsiChar('RIFF')[0], 4);
    Stream_guaranteeWrite(ms, @PAnsiChar('SIZE')[0], 4);
    // wave chunk
    Stream_guaranteeWrite(ms, @PAnsiChar('WAVE')[0], 4);
    Stream_guaranteeWrite(ms, @PAnsiChar('fmt ')[0], 4);
    // size
    c := 16;
    Stream_guaranteeWrite(ms, Pbyte(@c), 4);
    // compression PCM
    si := 1;
    Stream_guaranteeWrite(ms, Pbyte(@si), 2);
    // 1 channel
    si := 1;
    Stream_guaranteeWrite(ms, Pbyte(@si), 2);
    // sample rate
    i := 44100;
    Stream_guaranteeWrite(ms, Pbyte(@i), 4);
    // bytes per second
    i := 88200;
    Stream_guaranteeWrite(ms, Pbyte(@i), 4);
    // block align
    si := 16 div 8;
    Stream_guaranteeWrite(ms, Pbyte(@si), 2);
    // bits per sample
    si := 16;
    Stream_guaranteeWrite(ms, Pbyte(@si), 2);

    // data chunk
    Stream_guaranteeWrite(ms, @PAnsiChar('data')[0], 4);
    i := length;
    Stream_guaranteeWrite(ms, Pbyte(@i), 4);

    pc := PAnsiChar(pdata);
    for t := 0 to length - 1 do begin
      ch := pc[t];
      Stream_guaranteeWrite(ms, Pbyte(@ch), 1);
    end;

    iPos := ms.Position - 8;
    ms.Seek(main_size_pos, 0);
    Stream_guaranteeWrite(ms, Pbyte(@iPos), 4);

    ms.Seek(0, 0);

    thr := TSoundPlayerThread(TPM.NeedThread(TSoundPlayerThread,
        BackGroundthreadMan));
    //thr.FreeOnTerminate := false;
    thr.Loop := false;
    thr.Memory := ms;
    thr.Start;
    thr.WaitForFinish;
// PlaySound(ms.memory, 0, SND_SYNC+SND_NODEFAULT+SND_MEMORY);
    thr.WaitForFinish;
  finally
    if (assigned(thr)) then
      TPM.NoNeedthread(thr);
    // owned by thread now
    // ms.free;
  end;
end;


procedure TSoundPlayerThread.DoExecute;
var
  thr: TSoundPlayerThread;
begin
  inherited;

  if memory = nil then
    exit;
  PlaySound(Memory.Memory, 0, SND_SYNC + SND_NODEFAULT + SND_MEMORY);
end;

procedure Tcmd_PlaySoundFromDisk.DoExecute;
begin
  inherited;
  PlaySound(PChar(FFileName), 0, 0);
end;

procedure Tcmd_PlaySoundFromMemory_Mono44100.DoExecute;
var
  hWave: THandle;
  wf: TWaveFormatEx;
  mmr: mmresult;
  waveHeader: TWaveHdr;
  waveHeader_tag: WaveHdr_tag;
  mmtime: winapi.MMSystem.mmtime;
begin

// tWAVEFORMATEX = packed record
// wFormatTag: Word;         { format type }
// nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
// nSamplesPerSec: DWORD;  { sample rate }
// nAvgBytesPerSec: DWORD; { for buffer estimation }
// nBlockAlign: Word;      { block size of data }
// wBitsPerSample: Word;   { number of bits per sample of mono data }
// cbSize: Word;           { the count in bytes of the size of }
// end;

  // wave format
  wf.wFormatTag := WAVE_FORMAT_PCM; // PInt(PAnsiChar('WAVE'))^;;
  wf.nChannels := 1;
  wf.nSamplesPerSec := 44100;
  wf.nAvgBytesPerSec := 88200;
  wf.nBlockAlign := 2;
  wf.wBitsPerSample := 16;
  wf.cbSize := 0;

  mmr := waveoutopen(PHWaveOut(@hWave), WAVE_MAPPER, @wf, 0, 0, 0);

  if not(mmr = MMSYSERR_NOERROR) then begin
    Status := ('returned error: ' + inttostr(mmr));
    exit;
  end;

  try

    // wave header tag

    waveHeader.lpData := PAnsiChar(FBuf);
    if LengthLimit < 0 then
      waveHeader.dwBufferLength := ( high(FBuf) * 2)
    else
      waveHeader.dwBufferLength := LengthLimit;

    waveHeader.dwBytesREcorded := 0;
    waveHeader.dwUser := 0;
    waveHeader.dwFlags := 0;
    waveHeader.dwLoops := 1;
    waveHeader.lpNext := nil;
    waveHeader.reserved := 0;

    // ---
    mmr := waveOutPrepareHeader(hWave, @waveHeader, sizeof(waveHeader));
    if not(mmr = MMSYSERR_NOERROR) then begin
      Status := ('waveOutPrepareHeader returned error: ' + inttostr(mmr));
      exit;
    end;

    try

// ---
      mmr := waveOutWrite(hWave, @waveHeader, sizeof(waveHeader));
      if not(mmr = MMSYSERR_NOERROR) then begin
        Status := ('waveOutWrite returned error: ' + inttostr(mmr));
        exit;
      end;

      // ---
      mmtime.wType := TIME_SAMPLES;
      repeat
        mmr := waveOutGetPosition(hWave, @mmtime, sizeof(mmtime));
        if not(mmr = MMSYSERR_NOERROR) then begin
          Status := ('waveOutGetPosition returned error: ' + inttostr(mmr));
          exit;
        end;

        StepCount := NumberOfSamples;
        Step := mmtime.sample;
        sleep(1);
        if IsCancelled then begin
          waveOutReset(hWave);
          break;
        end;
      until (integer(mmtime.sample) >= (NumberOfSamples - 1));

      mmr := waveOutReset(hWave);
      if not(mmr = MMSYSERR_NOERROR) then begin
        Status := ('waveOutPause returned error: ' + inttostr(mmr));
        exit;
      end;

    finally

      // ---
      mmr := waveOutUnprepareHeader(hWave, @waveHeader, sizeof(waveHeader));
      if not(mmr = MMSYSERR_NOERROR) then begin
        // note! this line tricks the optimizer don't remove
        Status := ('waveOutUnprepareHeader returned error: ' + inttostr(mmr)
            + ' hack=' + inttostr(FBuf[0])); // note! this line tricks the optimizer don't remove
        // note! this line tricks the optimizer don't remove

// exit;
      end;
    end;

    // ---
  finally

    mmr := waveOutClose(hWave);
    if not(mmr = MMSYSERR_NOERROR) then begin
      Status := ('waveOutClose returned error: ' + inttostr(mmr));
// exit;
    end;
  end;

// waveOutWrite(hWave,

end;




procedure TSoundPlayerThread.InitFrompool;
begin
  inherited;
  Loop := true;

end;

procedure TSoundPlayerThread.OnFinish;
begin
  inherited;
  Memory.free;
  Memory := nil;

end;

{ Tcmd_PlaySoundFromMemory_Stereo44100 }

procedure Tcmd_PlaySoundFromMemory_Stereo44100.DoExecute;
begin
{$IFDEF DISABLE_SOUND}
  exit;
{$ENDIF}
  Setup;
  Play;
end;

procedure Tcmd_PlaySoundFromMemory_Stereo44100.Multiplex;
var
  t: integer;
begin
  SetLength(FBuf, 2 * LesserOf(length(FBufL), length(FBufR)));
  for t := 0 to LesserOf(length(FBufL), length(FBufR)) do begin
    FBuf[(t shl 1) + 0] := FBufL[t];
    FBuf[(t shl 1) + 1] := FBufR[t];
  end;

end;

procedure Tcmd_PlaySoundFromMemory_Stereo44100.Play;
var
  hWave: THandle;
  wf: TWaveFormatEx;
  mmr: mmresult;
  waveHeader: TWaveHdr;
  waveHeader_tag: WaveHdr_tag;
  mmtime: winapi.MMSystem.mmtime;
begin

// tWAVEFORMATEX = packed record
// wFormatTag: Word;         { format type }
// nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
// nSamplesPerSec: DWORD;  { sample rate }
// nAvgBytesPerSec: DWORD; { for buffer estimation }
// nBlockAlign: Word;      { block size of data }
// wBitsPerSample: Word;   { number of bits per sample of mono data }
// cbSize: Word;           { the count in bytes of the size of }
// end;
  Multiplex;

  // wave format
  wf.wFormatTag := WAVE_FORMAT_PCM; // PInt(PAnsiChar('WAVE'))^;;
  wf.nChannels := 2;
  wf.nSamplesPerSec := 44100;
  wf.nAvgBytesPerSec := 88200 * 2;
  wf.nBlockAlign := 4;
  wf.wBitsPerSample := 16;
  wf.cbSize := 0;

  mmr := waveoutopen(PHWaveOut(@hWave), WAVE_MAPPER, @wf, 0, 0, 0);

  if not(mmr = MMSYSERR_NOERROR) then begin
    Status := ('returned error: ' + inttostr(mmr));
    exit;
  end;

  try

    // wave header tag

    waveHeader.lpData := PAnsiChar(FBuf);
    waveHeader.dwBufferLength := ( high(FBuf) * 2); // sizeof(Fbuf);
    waveHeader.dwBytesREcorded := 0;
    waveHeader.dwUser := 0;
    waveHeader.dwFlags := 0; // WHDR_BEGINLOOP or WHDR_ENDLOOP;
    waveHeader.dwLoops := 1;
    waveHeader.lpNext := nil;
    waveHeader.reserved := 0;

    // ---
    mmr := waveOutPrepareHeader(hWave, @waveHeader, sizeof(waveHeader));
    if not(mmr = MMSYSERR_NOERROR) then begin
      Status := ('waveOutPrepareHeader returned error: ' + inttostr(mmr));
      exit;
    end;

    try

// ---
      mmr := waveOutWrite(hWave, @waveHeader, sizeof(waveHeader));
      if not(mmr = MMSYSERR_NOERROR) then begin
        Status := ('waveOutWrite returned error: ' + inttostr(mmr));
        exit;
      end;

      // ---
      mmtime.wType := TIME_SAMPLES;
      repeat
        mmr := waveOutGetPosition(hWave, @mmtime, sizeof(mmtime));
        if not(mmr = MMSYSERR_NOERROR) then begin
          Status := ('waveOutGetPosition returned error: ' + inttostr(mmr));
          exit;
        end;

        StepCount := high(FBuf) div 2;
        Step := mmtime.sample;
        sleep(1);
      until integer(mmtime.sample) >= ( high(FBuf) div 2);

      // sleep(8000);
      // MMR := waveOutStopPlaying(hWave);
      mmr := waveOutReset(hWave);
      if not(mmr = MMSYSERR_NOERROR) then begin
        Status := ('waveOutPause returned error: ' + inttostr(mmr));
        exit;
      end;

    finally

      // ---
      mmr := waveOutUnprepareHeader(hWave, @waveHeader, sizeof(waveHeader));
      if not(mmr = MMSYSERR_NOERROR) then begin
        // note! this line tricks the optimizer don't remove
        Status := ('waveOutUnprepareHeader returned error: ' + inttostr(mmr)
            + ' hack=' + inttostr(FBuf[0])); // note! this line tricks the optimizer don't remove
        // note! this line tricks the optimizer don't remove

// exit;
      end;
    end;

    // ---
  finally

    mmr := waveOutClose(hWave);
    if not(mmr = MMSYSERR_NOERROR) then begin
      Status := ('waveOutClose returned error: ' + inttostr(mmr));
// exit;
    end;
  end;

// waveOutWrite(hWave,

end;

procedure Tcmd_PlaySoundFromMemory_Stereo44100.ReplaceBuffers(br, bl: soundsample.TRawSoundArrayPOinterMono);
begin

  FBufL := bl;
  FBufR := br;

end;

procedure Tcmd_PlaySoundFromMemory_Stereo44100.Setup;
begin
  // null
end;

function Tcmd_PlaySoundFromMemory_Mono44100.GetNumberOfSamples: integer;
begin
  if LengthLimit < 0 then
    result := length(FBuf)
  else
    result := LengthLimit div 2;
end;

function Tcmd_PlaySoundFromMemory_Mono44100.GEtSizeOfBuffer: integer;
begin
  if LengthLimit < 0 then
    result := length(FBuf)
  else
    result := LengthLimit;

  result := result * 2;

end;

procedure Tcmd_PlaySoundFromMemory_Mono44100.Init;
begin
  inherited;
  FLengthLimit := -1;
  SetLength(FBuf, 0);
end;

procedure Tcmd_PlaySoundFromMemory_Mono44100.SetBuf
  (const Value: soundsample.TRawSoundArrayPOinterMono);
begin
  SetLength(FBuf, length(Value));

  MoveMem32(@FBuf[0], @Value[0], length(Value) * sizeof(FBuf[0]));

end;

procedure PlayOGG(sFile: string);
var
  ss: string;
  ms: TMemoryStream;
  bExists: boolean;
begin
  ss := Dllpath + 'temp.wav';
  exe.RunProgramAndWait(Dllpath + 'oggdec.exe', sFile + ' --output ' + ss, '',
    false, false);

  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(ss);
    PlaySound(ms.Memory, 0, SND_NODEFAULT + SND_MEMORY);
  finally
    ms.free;
  end;
// WaveToBooger(ss,ss+'.boog');
// playbooger(sfile+'.boog');

end;


destructor Tcmd_PlaySoundFromMemory_MonoOrStereo44100.Destroy;
begin

  inherited;
end;

procedure Tcmd_PlaySoundFromMemory_MonoOrStereo44100.DoExecute;
var
  hWave: THandle;
  wf: TWaveFormatEx;
  mmr: mmresult;
  waveHeader: TWaveHdr;
  waveHeader_tag: WaveHdr_tag;
  mmtime: winapi.MMSystem.mmtime;
begin


// tWAVEFORMATEX = packed record
// wFormatTag: Word;         { format type }
// nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
// nSamplesPerSec: DWORD;  { sample rate }
// nAvgBytesPerSec: DWORD; { for buffer estimation }
// nBlockAlign: Word;      { block size of data }
// wBitsPerSample: Word;   { number of bits per sample of mono data }
// cbSize: Word;           { the count in bytes of the size of }
// end;

  // wave format
  wf.wFormatTag := WAVE_FORMAT_PCM; // PInt(PAnsiChar('WAVE'))^;;
  wf.nChannels := FChannels;
  wf.nSamplesPerSec := 44100;
  wf.nAvgBytesPerSec := 88200 * FChannels;
  wf.nBlockAlign := 4;
  wf.wBitsPerSample := 16;
  wf.cbSize := 0;

  mmr := waveoutopen(phwaveout(@hWave), FDeviceIndex, @wf, 0, 0, 0);

  if not(mmr = MMSYSERR_NOERROR) then begin
    Status := ('returned error: ' + inttostr(mmr));
    exit;
  end;

  try

    // wave header tag

    waveHeader.lpData := PAnsiChar(FBuf);
    if LengthLimit < 0 then begin
      raise Exception.Create('Unassigned Length Limit is no longer supported');
// waveHeader.dwBufferLength := (high(FBuf)*2)
    end
    else
      waveHeader.dwBufferLength := LengthLimit * FChannels;

    waveHeader.dwBytesREcorded := 0;
    waveHeader.dwUser := 0;
    waveHeader.dwFlags := 0;
    waveHeader.dwLoops := 1;
    waveHeader.lpNext := nil;
    waveHeader.reserved := 0;

    // ---
    mmr := waveOutPrepareHeader(hWave, @waveHeader, sizeof(waveHeader));
    if not(mmr = MMSYSERR_NOERROR) then begin
      Status := ('waveOutPrepareHeader returned error: ' + inttostr(mmr));
      exit;
    end;

    try

// ---
      mmr := waveOutWrite(hWave, @waveHeader, sizeof(waveHeader));
      if not(mmr = MMSYSERR_NOERROR) then begin
        Status := ('waveOutWrite returned error: ' + inttostr(mmr));
        exit;
      end;

      // ---
      mmtime.wType := TIME_SAMPLES;
      repeat
        mmr := waveOutGetPosition(hWave, @mmtime, sizeof(mmtime));
        if not(mmr = MMSYSERR_NOERROR) then begin
          Status := ('waveOutGetPosition returned error: ' + inttostr(mmr));
          exit;
        end;

        StepCount := NumberOfSamples;
        Step := mmtime.sample;
        sleep(1);
        if IsCancelled then begin
          // waveOutReset(hWave);
          break;
        end;
      until (integer(mmtime.sample) >= (NumberOfSamples - 1));

      mmr := waveOutReset(hWave);
      if not(mmr = MMSYSERR_NOERROR) then begin
        Status := ('waveOutPause returned error: ' + inttostr(mmr));
        exit;
      end;

    finally

      // ---
      mmr := waveOutUnprepareHeader(hWave, @waveHeader, sizeof(waveHeader));
      if not(mmr = MMSYSERR_NOERROR) then begin
        // note! this line tricks the optimizer don't remove
        Status := ('waveOutUnprepareHeader returned error: ' + inttostr(mmr)
            + ' hack=' + inttostr(FBuf^)); // note! this line tricks the optimizer don't remove
        // note! this line tricks the optimizer don't remove

// exit;
      end;
    end;

    // ---
  finally

    mmr := waveOutClose(hWave);
    if not(mmr = MMSYSERR_NOERROR) then begin
      Status := ('waveOutClose returned error: ' + inttostr(mmr));
// exit;
    end;
  end;

// waveOutWrite(hWave,

end;

function Tcmd_PlaySoundFromMemory_MonoOrStereo44100.GetNumberOfSamples: integer;
begin
  if LengthLimit < 0 then
    result := 0
  else
    result := LengthLimit div FChannels;

end;

function Tcmd_PlaySoundFromMemory_MonoOrStereo44100.GEtSizeOfBuffer: integer;
begin
  if LengthLimit < 0 then
    result := 0
  else
    result := LengthLimit;

  result := result * 2;

end;

procedure Tcmd_PlaySoundFromMemory_MonoOrStereo44100.Init;
begin
  inherited;
  FLengthLimit := -1;
// SetLength(FBuf, 0);
  FDeviceIndex := nativeint(WAVE_MAPPER);

end;

{ TSoundStreamOscillator }

procedure Tcmd_PlayStream_Partial.DoExecute;
begin
  inherited;

  Thread.AddOscillator(FOsc);
  try
    FOsc.Pause;
    FOsc.StartAt := StartSample;
    FOsc.Start;
    FOsc.Loop := false;
    FOsc.SampleRate := SampleRate;

    while not FOsc.FirstSampleRequested do begin
      sleep(1);
      if self.IsCancelled then
         break;
    end;
    while (FOSc.Position < EndSample) and (FOsc.Started = true) do begin
      sleep(1);
      if self.IsCancelled then
         break;
    end;

    FOsc.Pause;


  finally
    Thread.RemoveOscillator(Fosc);
  end;
end;


{ Tcmd_Beep }

class procedure Tcmd_Beep.Beep(freq, Duration, attack, release,
  Chorus: integer);
var
  c: Tcmd_Beep;
begin
  if (Duration = 0) then
    exit;
  c := Tcmd_Beep.Create();
  try
    c.Frequency := freq;
    c.ReleaseTime := release;
    c.AttackTime := attack;
    c.Duration := Duration;
    c.Chorus := Chorus;
    c.fireForget := true;
    c.Start();

// c.WaitFor();
  finally
// c.free;
  end;

end;

class procedure Tcmd_Beep.BeepArray(freq, Duration: array of integer;
  attack, release: integer);
var
  c: Tcmd_Beep;
  bl, br: soundsample.TRawSoundArrayPOinterMono;
begin
  c := Tcmd_Beep.Create();
  try
    c.ReleaseTime := release;
    c.AttackTime := attack;
// c.Chorus := chorus;
    c.fireForget := true;
    c.ManualSetup := true;
    br := nil;
    bl := nil;
    beeper.BeepArray(bl, freq, Duration, attack, release);
    beeper.BeepArray(br, freq, Duration, attack, release);
    c.ReplaceBuffers(bl, br);
    c.Start();

// c.WaitFor();
  finally
// c.free;
  end;

end;

class procedure Tcmd_Beep.BeepChord(freq: array of integer;
  Duration, attack, release: integer);
var
  c: Tcmd_Beep;
begin

  if (Duration = 0) then
    exit;
  c := Tcmd_Beep.Create();
  try
    c.ReleaseTime := release;
    c.AttackTime := attack;
    c.Duration := Duration;
    c.fireForget := true;
    c.ManualSetup := true;
    beeper.BeepChord(c.FBufL, freq, Duration, attack, release);
    beeper.BeepChord(c.FBufR, freq, Duration, attack, release);

    c.Start();

// c.WaitFor();
  finally
// c.free;
  end;
end;

class procedure Tcmd_Beep.BeepWait(freq, Duration, attack, release,
  Chorus: integer);
var
  c: Tcmd_Beep;
begin
  if (Duration = 0) then
    exit;
  c := Tcmd_Beep.Create();
  try
    c.Frequency := freq;
    c.ReleaseTime := release;
    c.AttackTime := attack;
    c.Duration := Duration;
    c.Chorus := Chorus;
    c.fireForget := false;//you can't wait for a fire-forget command
    c.Start();

    c.WaitFor();
  finally
    c.free;
  end;

end;

procedure Tcmd_Beep.Setup;
begin
  inherited;
  beeper.Beep2(FBufL, Frequency + Chorus, Duration, AttackTime, ReleaseTime);
  beeper.Beep2(FBufR, Frequency - Chorus, Duration, AttackTime, ReleaseTime);
end;

function Begin_PlaySound(sFile: string;
  bforget: boolean): Tcmd_PlaySoundFromDisk;
begin
  result := Tcmd_PlaySoundFromDisk.Create();
  result.fileName := sFile;
  if bforget then
    result.OwnedByProcessor := true;
  result.Start;
end;

procedure End_PlaySound(var cmd: Tcmd_PlaySoundFromDisk);
begin
  if cmd = nil then
    exit;
  try
    cmd.waitFor;
  finally
    cmd.free;
    cmd := nil;
  end;
end;


// ------------------------------------------------------------------------------
procedure Begin_Beep(freq, Duration, attack, release, Chorus: integer);
begin
  Tcmd_Beep.Beep(freq, Duration, attack, release, 1);
end;

// ------------------------------------------------------------------------------
procedure Begin_BeepArray(freq: array of integer; Duration: array of integer;
  attack, release, Chorus: integer);
begin
  Tcmd_Beep.BeepArray(freq, Duration, attack, release);
end;

// ------------------------------------------------------------------------------
procedure Begin_BeepChord(freq: array of integer;
  Duration, attack, release, Chorus: integer);
begin
  Tcmd_Beep.BeepChord(freq, Duration, attack, release);
end;

// ------------------------------------------------------------------------------
procedure End_Beep(c: Tcmd_Beep);
begin
// c.waitfor;
// c.free;
end;

// ------------------------------------------------------------------------------
procedure End_BeepArray(c: Tcmd_Beep);
begin
// c.waitfor;
// c.free;
end;

// ------------------------------------------------------------------------------
procedure End_BeepChord(c: Tcmd_Beep);
begin
// c.waitfor;
// c.free;
end;

function Booger_GetHeader(sFile: string): TboogerHeader;
var
  ss: TSoundStream;
begin
  ss := TSoundStream.Create(sFile, fmOpenRead+fmShareDenyNone);
  try
    result := ss.LoadHeader;
  finally
    ss.Free;
  end;

end;


procedure Tcmd_WaveStretch.DoExecute;
var
  sInBoog: string;
  sOutBoog: string;
  ss1, ss2: TSoundStream;
  t: int64;
  rLeft, rRight: FLoatSample;
  rLeft1, rRight1: FLoatSample;
  iSample: single;
  iSample1: single;
  iSampleBlend: single;
  rTemp: single;
  c: Tcmd_PlaySoundFromDisk;
  s,s1: TStereoSoundSample;
begin
  inherited;
  sInBoog := InputFile + '.boog';
  sOutBoog := OutputFile + '.boog';
  StepCount := 3;
  Step := 0;
  WaveToBooger(InputFile, sInBoog);
  Step := 1;

  ss1 := TSoundStream.Create(sInBoog, fmOpenRead + fmShareDenyNone);
  try
    ss2 := TSoundStream.Create(sOutBoog, fmCreate);

    ss2.channels := ss1.channels;

    if NewSampleRate <> 0 then
      ss2.samplerate := NewSampleRate;

    if RoundBoundary then begin
      NewLength := round((ss1.SampleCount * (ss2.samplerate / ss1.samplerate))
          / self.Boundary) * self.Boundary;
    end;

    ss2.SampleCount := NewLength-1;

    try
      t := 0;
      ss2.SeekSample(0);
      StepCount := NewLength-1;
      while t <= NewLength-1 do begin
        Step := t;
        iSample := Interpolate(t, 0, ss1.SampleCount - 1, 0, NewLength-1);
        iSample1 := trunc(iSample);
        iSampleBlend := iSample - iSample1;
        ss1.SeekSample(round(iSample1));
        ss1.GetNextSample(s);
        ss1.GetNextSample(s1);

        //ss := (ss1*iSampleBlend)+(ss*(1-iSampleBlend));
        s.Left := Interpolate(iSampleBlend, s.Left, s1.Left);
        s.Right := Interpolate(iSampleBlend, s.Right, s1.Right);

        ss2.WriteNextSample(s.Left, s.Right);

        inc(t);
      end;

    finally
      ss2.free;
    end;
  finally
    ss1.free;
  end;

  BoogerToWave(sOutBoog, OutputFile, NewSampleRate);

  if fileexists(sInBoog) then
    deletefile(sInBoog);

  if fileexists(sOutBoog) then
    deletefile(sOutBoog);
end;



procedure Tcmd_WaveStretch.RecalculateExpense;
begin
  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure Tcmd_WaveStretch.SetInputFile(const Value: string);
begin
  FInputFile := Value;
end;

procedure Tcmd_WaveStretch.SetOutputFile(const Value: string);
begin
  FOutputFile := Value;
end;


procedure Tcmd_WaveToFSB.BuildCommandLine;
var
  c: Tcmd_WaveStretch;
  sIntermediateFile: string;
begin
  //fsbankexcl.exe -format mp3 -quality 40 -o PATH_TO_OUTPUTFILE\filename.fsb PATH_TO_WAV_FILE\filename.wav
  //C:\Source\jammusic\gameaudio\sfc\fsbankex.exe -format mp3 -quality 0 -o "..\FinalGameAudio\test_deploy2\Samples\8 Bit Lover" "C:\Source\jammusic\gameaudio\sfc\..\..\originalSamples\8 Bit Lover\Bank4Loop4.wav"
  sIntermediatefile := systemx.GetTempPath+extractfilename(self.FInFile);
//  c := Tcmd_WaveStretch.create;
//  try
//    c.InputFile := InFile;
//    c.OutputFile := sIntermediatefile;
//    c.Boundary := 1152;
//    c.NewSampleRate := 44100;
//    c.Start;
//    self.WaitForAnotherCommand(c);
//  finally
//    c.Free;
//  end;
  sIntermediateFile := InFile;
  Prog := DLLPath+'fsbankexcl.exe';
  Params := '-format '+Format+' -quality '+inttostr(round(quality*10))+' -o '+Quote(OutFile)+' '+Quote(sIntermediatefile);
//  DeleteFile(sIntermediatefile);
end;

function BeginWaveToFSB(sInFile: string; sOutFile: string; sFormat: string; rQuality: nativefloat = 4.0;
  bDownMix: boolean = false; iResample: integer = 44100): Tcmd_WaveToFSB;
begin
  result := Tcmd_WaveToFSB.create;
  result.InFile := sInFile;
  result.OutFile := sOutFile;
  result.DownMix := bDownMix;
  result.Quality := rQuality;
  result.Format := sFormat;
  result.Hide := true;
  result.Start;

  if bDownmix or (iResample <> 44100) then begin
    Debug.Log(nil,'Warning: WaveToFSB does not implement downmixing or resampling', 'Error');
  end;
end;

procedure EndWaveToFSB(var c: Tcmd_WaveToFSB);
begin
  try
    c.WaitFor;
  finally
    c.Free;
    c := nil;
  end;

end;

procedure WaveToFSB(sInFile: string; sOutFile: string; sFormat: string; rQuality: nativefloat = 8.0;
  bDownMix: boolean = false; iResample: integer = 44100);
var
  c: Tcmd_WaveToFSB;
begin
  c := BeginWaveToFSB(sInfile, soutFile, sformat, rQuality, bDownMIx, iResample);
  EndWaveToFSB(c);
end;



{ Tcmd_WaveToFSB }


procedure Tcmd_WaveToFSB.DoExecute;
begin
  BuildCommandLIne;
  inherited;
//  sleep(10000);

end;

procedure Tcmd_WaveToFSB.Init;
begin
  inherited;
  FQuality := 4.0;
  Icon := @CMD_ICON_SOUND;

end;

procedure Tcmd_WaveToFSB.InitExpense;
begin
  inherited;
  CPUExpense := 1.0;
end;




procedure Tcmd_PlayStream_Partial.Init;
begin
  inherited;
  SampleRate := 44100;
end;

end.
