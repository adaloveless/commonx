unit SoundDevice_MM;
{$Message '********************************Compiling SoundDevice_MM'}

interface

uses
  tickcount,
{$IFDEF MSWINDOWS}
  winapi.mmsystem,
{$ENDIF}
  msacm, soundtools, typex, soundinterfaces, sysutils, classes, signals, ringbuffer, systemx, windows, soundsample;

type
  TSoundDevice_MM = class(TAbstractSoundDevice, ISoundOscillatorRenderer)
  private
    FSampleRate: ni;
    waveHeader: TWaveHdr;
    waveHeader_tag: WaveHdr_tag;

  public
    procedure Init;override;
    procedure SetupWave;override;
    procedure CleanupWave;override;
    function GetSamplePosition: int64;
    property SampleRate: ni read FSampleRate write FSampleRate;
    procedure AudioLoop;override;
  end;

function GetStandardWaveFormat(samplerate: cardinal = 44100): TWaveFormatEx;



implementation

{ TSoundStreamThread_MM }


function GetAudioDeviceList: TStringList;
var
  t: integer;
  s: string;
  stuff: tagWAVEOUTCAPSW;
begin
  result := TStringlist.Create;


  for t:= 0 to waveOutGetNumDevs-1 do begin
    waveoutGEtDevCaps(t, @stuff, sizeof(stuff));
    s := stuff.szPname;
    result.Add(s);
  end;

end;


function GetStandardWaveFormat(samplerate: cardinal = 44100): TWaveFormatEx;
begin
  result.wFormatTag := WAVE_FORMAT_PCM; // PInt(PAnsiChar('WAVE'))^;;
  result.nChannels := 2;
  result.nSamplesPerSec := samplerate;
  result.nAvgBytesPerSec := samplerate * 2 * result.nChannels;
  result.nBlockAlign := 4;
  result.wBitsPerSample := 16;
  result.cbSize := 0;



end;


procedure TSoundDevice_MM.AudioLoop;
var
  wf: TWaveFormatEx;
  mmr: mmresult;
  mmtime,mmtimelast, mmtimedelta: winapi.MMSystem.mmtime;
//  r1, r2: FLoatSample;
  ss: TStereoSoundSample;
  rCurrent, rTemp: TStereoSoundSample;
  tm, targetTm, filled, tmEndWall: int64;

  t: integer;
  iFillcount: integer;
  tmMeasure: cardinal;
  tmstarted: cardinal;
  tmLastIter: cardinal;
  tmExpectedIterTime: nativefloat;
  tmSince: cardinal;
  expectediters: nativefloat;

const
  ITER_TOLERANCE = 1;

begin
  inherited;
  tmMeasure := GetTicker;
  tm := GetTicker; // initializations
  tmStarted := tm;
  mmtime.wType := TIME_SAMPLES;


      mmr := waveOutGetPosition(hWave, @mmtime, sizeof(mmtime));
      FPlayPOsition := mmtime.sample;
      mmtimedelta := mmtime;

      repeat
        if IsSignaled(evStop) then
          exit;

        if TryLock then
          try
            if waveheader.dwBufferLength <> (DWORD(FBufferSize) * DWORD(4)) then begin
              CleanupWave;
              SetupWave;
            end;

            mmtimelast := mmtime;
            mmr := waveOutGetPosition(hWave, @mmtime, sizeof(mmtime));
            if not(mmr = MMSYSERR_NOERROR) then begin
              Status := ('waveOutGetPosition returned error: ' + inttostr(mmr));
              exit;
            end;

            mmtimedelta.sample := mmtime.sample-mmtimelast.sample;

            FPlayPosition := FPlayPOsition + (mmtimedelta.sample);


          // StepCount := high(FBuf) div 2;
          // Step := mmtime.sample;
          // RUN THE OSCILLATORS
          // Status := inttostr(mmtime.sample);

            targetTm := (((mmtime.sample) mod dword(FBufferSize)));//(( high(FBuf) + 1) div 2)));
{$IFDEF SOUND_STREAM_THREAD_DEBUG_ENABLE}
            filled := (targetTm * 2) - fillptr;
            if filled < 0 then
              filled := filled + (FBufferSize);

          // filled := FBufferSize - filled;

            if TryLock() then
            try
              Status := 'F:' + inttostr(Frequency) + ' Osc: ' + inttostr
                (OscillatorCount) + ' Buf: ' + inttostr(filled)
                + '/' + inttostr(FBufferSize)
                + ' TargetTm:' + inttostr(targetTm) + ' PP:' + inttostr
                (FPlayPosition);
              StepCount := FBufferSize;
              Step := filled;
            finally
              Unlock();
            end;
{$ELSE}
          // Stepcount := FBufferSize div 2;
{$ENDIF}
            for t := 0 to OscillatorCount - 1 do begin
              Oscillators[t].Fill(fillptr, mtBeginWindow, ss, 0);
            end;

            iFillcount := 0;
            tmEndWall := ring_subtract(targetTm, FBufferSize, 1);
            tmEndWall := tmEndWAll mod FBufferSize;
            while ((fillptr) <> tmEndWall) do begin
              FillMem(Pbyte(@rCurrent), sizeof(rCurrent),0);
              tm := (fillptr + (iters * FBufferSize));
              Lock;
              try
                for t := 0 to OscillatorCount - 1 do begin
                  Oscillators[t].Fill(fillptr, mtGetSample, rTemp, tm);

                  rCurrent := rCurrent + rTemp;
                  //rCurrent.Left := rCurrent.Left + rTemp.Left;

                end;
              finally
                Unlock;
              end;


              if fMuteAudio then begin
                FillMem(PByte(@rCurrent), sizeof(rCurrent),0);
                break;
              end
              else begin
                rTEmp.Left := ControlRoomVolume;
                rTemp.Right := ControlRoomVolume;
                rCurrent := rCurrent * rTemp;
              end;

              if rCurrent.Left > 1.0 then
                rCurrent.Left := 1.0;
              if rCurrent.Left < -1.0 then
                rCurrent.Left := -1.0;
              if rCurrent.Right > 1.0 then
                rCurrent.Right := 1.0;
              if rCurrent.Right < -1.0 then
                rCurrent.Right := -1.0;



              CreateUDPConnection;
              BuildUDPPacket(tm, rCurrent.Left, rCurrent.Right);
              if (udpc <> nil) then begin
                rCurrent.Left := 0;
                rCurrent.Right := 0;
              end;

              FBuf[fillptr].ch[0] := trunc(rCurrent.Left * 32767);
              FBuf[fillptr].ch[1] := trunc(rCurrent.Right * 32767);
              inc(fillptr);
              fillptr := fillptr mod FBufferSize;
              if fillptr = 0 then begin
                inc(iters);
                tmExpectedIterTime := 1000*(FBufferSize / samplerate);
                tmSince := GetTimeSince(tmStarted);
                //we expect that X iterations have completed since start
                expectediters := tmSince / tmExpectedIterTime;
                if abs(expectediters-iters) > ITER_TOLERANCE then begin
                  iters := round(expectedIters);
                  FPLayPosition := round(expectediters*FBufferSize);
                end;
                tmLastIter := GetTicker;


              end;

              inc(iFillcount);

            end;
          // step := Stepcount-iFillcount;

            for t := 0 to OscillatorCount - 1 do begin
              Oscillators[t].Fill(fillptr, mtEndWindow, ss, 0);
            end;



          finally
            Unlock;
          end;

        FFrequency := GetTimeSince(tmMeasure);
        tmMeasure := GetTicker;

        WaitForSingleObject(self.handle, 10);
// sleepex(10,true);

      until StopRequested;
// until mmtime.sample >= (high(Fbuf) div 2);

end;

procedure TSoundDevice_MM.CleanupWave;
begin
  inherited;

end;

function TSoundDevice_MM.GetSamplePosition: int64;
begin
  result:= 0;
end;

procedure TSoundDevice_MM.Init;
begin
  inherited;
  samplerate := 44100;
  betterPriority := bpTimeCritical;
  DeviceID := WAVE_MAPPER;

end;

procedure TSoundDevice_MM.SetupWave;
var
  wf: TWaveFormatEx;
  mmr: MMResult;
begin
  inherited;

  fillptr := 0;
// tWAVEFORMATEX = packed record
// wFormatTag: Word;         { format type }
// nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
// nSamplesPerSec: DWORD;  { sample rate }
// nAvgBytesPerSec: DWORD; { for buffer estimation }
// nBlockAlign: Word;      { block size of data }
// wBitsPerSample: Word;   { number of bits per sample of mono data }
// cbSize: Word;           { the count in bytes of the size of }
// end;
  // MultiPlex;

  // wave format
  wf.wFormatTag := WAVE_FORMAT_PCM; // PInt(PAnsiChar('WAVE'))^;;
  wf.nChannels := 2;
  wf.nSamplesPerSec := 44100;
  wf.nAvgBytesPerSec := 88200 * 2;
  wf.nBlockAlign := 4;
  wf.wBitsPerSample := 16;
  wf.cbSize := 0;

  mmr := waveoutopen(phwaveout(@hWave), DeviceID, @wf, 0, 0, 0);

  if not(mmr = MMSYSERR_NOERROR) then begin
    Status := ('returned error: ' + inttostr(mmr));
    exit;
  end;

  // wave header tag
  waveHeader.lpData := PAnsiChar(@FBuf);
  waveHeader.dwBufferLength := BufferSize * 4;//(( high(FBuf) + 1) * 2); // sizeof(Fbuf);
  waveHeader.dwBytesREcorded := 0;
  waveHeader.dwUser := 0;
  waveHeader.dwFlags := WHDR_BEGINLOOP or WHDR_ENDLOOP;
  waveHeader.dwLoops := $FFFFFFFF;
  waveHeader.lpNext := nil;
  waveHeader.reserved := 0;

  // ---
  mmr := waveOutPrepareHeader(hWave, @waveHeader, sizeof(waveHeader));
  if not(mmr = MMSYSERR_NOERROR) then begin
    Status := ('waveOutPrepareHeader returned error: ' + inttostr(mmr));
    exit;
  end;

  mmr := waveOutWrite(hWave, @waveHeader, sizeof(waveHeader));
  if not(mmr = MMSYSERR_NOERROR) then begin
    Status := ('waveOutWrite returned error: ' + inttostr(mmr));
    exit;
  end;
end;

end.
