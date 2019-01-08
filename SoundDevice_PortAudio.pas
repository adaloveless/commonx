unit SoundDevice_PortAudio;

interface

uses
  debug, tickcount, soundtools, typex, soundinterfaces, sysutils, classes,
  signals, ringbuffer, systemx, windows, orderlyinit,
  portaudio, synth_functions, soundsample, stringx;

type
  TSoundDevice_PortAudio = class(TAbstractSoundDevice, ISoundOscillatorRenderer)
  private
    DevicesScanned: boolean;
    shuttingdown: boolean;
    FSampleRate: ni;
    function GetPADeviceCount: ni;
    function GetPADevice(idx: ni): PPaDeviceInfo;
    function GetPAHostAPICount: ni;
    function GetPAHostApiInfo(idx: ni): TPaHostApiInfo;
  strict protected
    procedure RefreshDevices;override;
    property PADeviceCount: ni read GetPADeviceCount;
    property PADevices[idx: ni]: PPaDeviceInfo read GetPADevice;
    function PADeviceNameToPAIdx: ni;
    property PAHostApicount: ni read GetPAHostAPICount;
    property PAHostApis[idx: ni]: TPaHostApiInfo read GetPAHostApiInfo;
  protected
    procedure DeviceChanged; override;
  public
    str: pointer;
    lastsamplenumber:int64;

    procedure InitFromPool;override;
    procedure Init;override;
    procedure SetupWave;override;
    procedure CleanupWave;override;
    function GetSamplePosition: int64;
    property SampleRate: ni read FSampleRate write FSampleRate;
    procedure AudioLoop;override;
    function PAAudioFill(const inputbuffer, outputbuffer: pointer; framesperbuffer: cardinal; const timeinfo: TPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags): integer;


  end;

  Tpatestdata = packed record
    left, right: single;
  end;


function patestcallback(const inputbuffer, outputbuffer: pointer; framesperbuffer: cardinal; const timeinfo: TPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags; userdata:pointer): integer; cdecl;
function pacallback_global(const inputbuffer, outputbuffer: pointer; framesperbuffer: cardinal; const timeinfo: TPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags; userdata:pointer): integer; cdecl;
//function HostAPIToString(const api: TPaHostApiTypeId): string;
function StringToHostApi(const s: string): ni;

implementation

{ TSoundStreamThread_MM }

procedure oinit;
var
  err: TPaError;
begin
  err := pa_initialize();
  if err <> 0 then begin
    Debug.Log('PORT AUDIO INITIALIZATION FAILED code:'+inttostr(err));
    sleep(8000);
    halt;
  end;
end;


procedure ofinal;
begin
  pa_terminate();
end;


{ TSoundDevice_PortAudio }

procedure TSoundDevice_PortAudio.AudioLoop;
begin
  inherited;
  //while not StopREquested do
    sleep(100);
end;


procedure TSoundDevice_PortAudio.CleanupWave;
begin
  inherited;
  FActive := false;
  shuttingdown := true;
  Pa_StopStream(str);
//  while not (Pa_IsStreamSTopped(str)=1) do
//    sleep(100);
  Pa_CloseStream(str);
end;


procedure TSoundDevice_PortAudio.DeviceChanged;
begin
  inherited;

end;

procedure TSoundDevice_PortAudio.RefreshDevices;
var
  t: ni;
begin
  try
    FDevicelist.clear;
    for t:= 0 to PADeviceCount-1 do begin
      if PADevices[t].maxOutputChannels > 0 then begin
        var hapi := PAHostApis[PAdevices[t].hostApi];
        FDeviceList.add(hapi.name+'/'+PAdevices[t].name);
      end;
    end;
    //if this is the first time we've scanned the devices, also set
    //default device
    if self.DeviceCount > 0 then begin
      DeviceName := Self.Devices[0];
    end;
  finally
    DevicesScanned := true;
  end;

end;

function TSoundDevice_PortAudio.GetPADevice(idx: ni): PPaDeviceInfo;
begin
  result := Pa_GetDeviceInfo(idx);
end;

function TSoundDevice_PortAudio.GetPADeviceCount: ni;
begin
  result := Pa_GetDeviceCount;
end;

function TSoundDevice_PortAudio.GetPAHostAPICount: ni;
begin
  result := PAHostApicount;
end;

function TSoundDevice_PortAudio.GetPAHostApiInfo(idx: ni): TPaHostApiInfo;
begin
  result := Pa_GetHostApiInfo( idx )^;
end;

function TSoundDevice_PortAudio.GetSamplePosition: int64;
begin
  result := 0;
end;

procedure TSoundDevice_PortAudio.Init;
begin
  inherited;
  samplerate := 44100;
//  DeviceNAme := 'Microsoft Sound Mapper - Output';
//  devidx := 16;
end;

procedure TSoundDevice_PortAudio.InitFromPool;
begin
  inherited;
  RefreshDevices;
  if self.DeviceCount > 0 then begin
    DeviceName := Self.Devices[0];
  end;
end;

function TSoundDevice_PortAudio.PAAudioFill(const inputbuffer,
  outputbuffer: pointer; framesperbuffer: cardinal;
  const timeinfo: TPaStreamCallbackTimeInfo;
  statusFlags: TPaStreamCallbackFlags): integer;
var
  ss: TStereoSoundSample;
  rCurrent, rTemp: TStereoSoundSample;
  tm, targetTm, filled, tmEndWall: int64;

  t,s: integer;
  iFillcount: integer;
  tmMeasure: cardinal;
  tmLastIter: cardinal;
  tmExpectedIterTime: nativefloat;
  tmSince: cardinal;
  expectediters: nativefloat;
  sampletime: int64;
  thissample: int64;
  outt: PSingle;
const
  ITER_TOLERANCE = 1;

begin
  inherited;
  if shuttingdown then
    exit(0);
  result := 0;
//  tmMeasure := GetTicker;

  sampletime := round(timeinfo.outputBufferDacTime*SampleRate);
  sampletime := lastsamplenumber;
  FPlayPOsition := sampletime;

//      repeat
//        if IsSignaled(evStop) then
//          exit;

          Lock;
          try

            for t := 0 to OscillatorCount - 1 do begin
              Oscillators[t].Fill(sampletime, mtBeginWindow, ss, sampletime);
            end;

            outt := PSingle(outputbuffer);
            for s := 0 to framesperbuffer-1 do
            begin
              thissample := sampletime+s;
              rCurrent.Init;
              for t := 0 to OscillatorCount - 1 do begin
                Oscillators[t].Fill(thissample, mtGetSample, rTemp, thissample);
                rCurrent := rCurrent + rTemp;
              end;
//              rTemp.Left := SineSynth(thissample, 1500, 0.2, 44100);
//              rTemp.right := rTemp.Left;



              if fMuteAudio then begin
                FillMem(@rCurrent, sizeof(rCurrent),0);
              end
              else begin
                rTEmp.Left := ControlRoomVolume;
                rTemp.Right := ControlRoomVolume;
                rCurrent := rCurrent * rTemp;
              end;

              rCurrent.Clip;


              if self.Remote then begin
              CreateUDPConnection;
                BuildUDPPacket(sampletime, rCurrent.Left, rCurrent.Right);
                if (udpc <> nil) then begin
                  rCurrent.Left := 0;
                  rCurrent.Right := 0;
                end;
              end;

              outt^ := rCurrent.Left;
              inc(outt);
              outt^ := rCurrent.Right;
              inc(outt);


              FBuf[fillptr].ch[0] := trunc(rCurrent.Left * 32767);
              FBuf[fillptr].ch[1] := trunc(rCurrent.Right * 32767);
              inc(fillptr);
              if fillptr > Length(FBuf) then
                fillptr := 0;
              inc(lastsamplenumber);
            end;



            //inc(iFillcount);

            for t := 0 to OscillatorCount - 1 do begin
              Oscillators[t].Fill(fillptr, mtEndWindow, ss, 0);
            end;



          finally
            Unlock;
          end;

        //FFrequency := GetTimeSince(tmMeasure);
        //mMeasure := GetTicker;

        //WaitForSingleObject(self.handle, 10);


end;

function TSoundDevice_PortAudio.PADeviceNameToPAIdx: ni;
var
  t: ni;
  di: PPaDeviceInfo;
  sApi, sDevice: string;
begin
  result := -1;
  SplitString(DeviceName, '/', sApi, sDevice);
  for t:= 0 to PADeviceCount-1 do begin
    di := GetPADevice(t);
//    debug.consolelog('Audio Device ['+inttostr(t)+']='+di.name);
//    debug.consolelog('API='+inttostr(di.hostApi));
    var sThisApi := PaHostApis[di.hostapi].name;
    if (di.name = sDevice) and (sThisApi=sApi) then
      exit(t);
  end;

end;



procedure TSoundDevice_PortAudio.SetupWave;
var
  sp: TPaStreamParameters;
  err: TPaError;
begin
  inherited;
  shuttingdown := false;
  //Pa_Initialize;
  Debug.Log('Setup '+DeviceName);
  sp.device := PADeviceNameToPAIdx;
  sp.channelCount := 2;
  sp.sampleFormat := paFloat32;

  var info := Pa_GetDeviceInfo(sp.device);
  if info <> nil then
    sp.suggestedLatency := info.defaultLowOutputLatency;

  sp.hostApiSpecificStreamInfo := nil;

  err := Pa_OpenStream(str, nil, @sp, 44100, 256, 0,@pacallback_global, pointer(self));
  //err := Pa_OpenStream(str, nil, @sp, 44100, 256, 0,@patestcallback, pointer(self));
  if err <> 0 then begin
    Debug.Consolelog('Error '+inttostr(err)+' when opening audio!');
  end;
  //Pa_OpenDefaultStream(str, 0, 2, paFloat32, 44100, 512, @pacallback_global, pointer(self));
  Pa_StartStream(str);
  FActive := true;

end;


function patestcallback(const inputbuffer, outputbuffer: pointer; framesperbuffer: cardinal; const timeinfo: TPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags; userdata:pointer): integer; cdecl;
var
  outt: PSingle;
  i: cardinal;
  tm: int64;
begin
    ///* Cast data passed through stream to our structure. */
    //paTestData *data = (paTestData*)userData;
    outt := PSingle(outputbuffer);//    float *out = (float*)outputBuffer;

    for i := 0 to FramesPerBuffer-1 do
    begin
        tm := round(i+(timeinfo.outputBufferDacTime*44100));
        outt^ := SineSynth(tm, 1500, 0.25, 44100);
        inc(outt);
        outt^ := SineSynth(tm, 1500, 0.25, 44100);
        inc(outt);

    end;
    result := 0;
end;

function pacallback_global(const inputbuffer, outputbuffer: pointer; framesperbuffer: cardinal; const timeinfo: TPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags; userdata:pointer): integer; cdecl;
begin
  result := TSoundDevice_PortAudio(userdata).PAAudioFill(inputbuffer, outputbuffer, framesperbuffer, timeinfo, statusFlags);
end;

//function HostAPIToString(const api: TPaHostApiTypeId): string;
//begin
//  result := '';
//  case api of
//    paInDevelopment: result := 'InDevelopment';
//    paDirectSound: result := 'DirectSound';
//    paMME: result := 'MME';
//    paASIO: result := 'ASIO';
//    paSoundManager: result := 'SoundManager';
//    paCoreAudio: result := 'CoreAudio';
//    paOSS: result := 'OSS';
//    paALSA: result := 'ALSA';
//    paAL: result := 'AL';
//    paBeOS: result := 'BeOS';
//    paWDMKS: result := 'WDMKS';
//    paJACK: result := 'JACK';
//    paWASAPI: result := 'WASAPI';
//    paAudioScienceHPI: result := 'AudioScienceHPI';
//  else
//    raise ECritical.create('Unsupported Port Audio API');
//  end;
//
//end;

function StringToHostApi(const s: string): ni;
begin
  result := 0;
  if s = 'InDevelopment' then exit(paInDevelopment);
  if s = 'DirectSound' then exit(paDirectSound);
  if s = 'MME' then exit(paMME);
  if s = 'ASIO' then exit(paASIO);
  if s = 'SoundManager' then exit(paSoundManager);
  if s = 'CoreAudio' then exit(paCoreAudio);
  if s = 'OSS' then exit(paOSS);
  if s = 'ALSA' then exit(paALSA);
  if s = 'AL' then exit(paAL);
  if s = 'BeOS' then exit(paBeOS);
  if s = 'WDMKS' then exit(paWDMKS);
  if s = 'JACK' then exit(paJACK);
  if s = 'WASAPI' then exit(paWASAPI);
  if s = 'AudioScienceHPI' then exit(paAudioScienceHPI);


end;



initialization

init.RegisterProcs('SoundDevice_PortAudio', oinit, ofinal, 'ManagedThread');



finalization


end.
