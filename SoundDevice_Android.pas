unit SoundDevice_Android;

interface

uses
  AndroidApi.JNI.Media, managedthread, Androidapi.Jni.Provider, typex,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Media, Androidapi.Jni.JavaTypes,
  FMX.Types, Androidapi.Jni, AndroidAPI.JNIBridge, AndroidAPI.helpers, fmx.platform.android,
  FMX.Controls.Presentation, FMX.StdCtrls, synth_functions, soundsample,
  debug, tickcount, soundtools, soundinterfaces, sysutils, classes, signals, ringbuffer, systemx, orderlyinit;

const
  SAMPLE_RATE = 44100;
  MAX_BUF_SIZE = 88200*4;

type
  TStereoSAmple = packed record
    l,r: smallint;
  end;

  TSoundDevice_Android = class(TAbstractSoundDevice, ISoundOscillatorRenderer)
  strict private
    jathold: JAudioTRack;
    [unsafe] jat: JAudioTRack;
    jabhold: TJAvaArray<byte>;
    [unsafe] jab: TJavaArray<byte>;
    mbs: integer;
    samplesout: int64;
    wrote: int64;
    buf: array [0..MAX_BUF_SIZE-1] of TStereoSample;
    errors: ni;
  public
    procedure AudioLoop; override;
    procedure SetupWave; override;
    procedure CleanupWave; override;
  end;



implementation

{ TSoundDevice_PortAudio }

procedure TSoundDevice_Android.AudioLoop;
var
  t: int64;
  samp: int64;
  pb: Pbyte;
  bytecount: ni;
  cx,dx: ni;
  justwrote: ni;
  ms: ni;
  f,lf,tf: ni;
  ss: TStereoSoundSample;
begin
  inherited;
  t := 0;
  samp := samplesout;
  ms := mbs div Sizeof(TStereoSample);
  BeginSampling(samp);
  try
    while t < ms do begin
      self.RenderSample(samp, ss);
      buf[t].l := round(ss.Left * 32767);
      buf[t].r := round(ss.right * 32767);
      inc(samp);
      inc(t);
    end;
  finally
    EndSampling(samp);
  end;
  bytecount := mbs;

  try
    pb := @buf[0];
    dx := 0;
    cx := bytecount;
    while cx > 0 do begin
      jab.Items[dx] := pb^;
      inc(pb);
      inc(dx);
      dec(cx);
    end;
    justwrote := jat.write(jab, 0, bytecount);
    if justwrote < mbs then
      inc(errors);
    inc(wrote, justwrote);
    inc(samplesout, justwrote div sizeof(TStereoSample));


  finally
  end;
  runhot := true;



end;


procedure TSoundDevice_Android.CleanupWave;
begin
  inherited;
  jab.free;
  jab := nil;
  jabhold.free;
  jabhold := nil;
  jat := nil;
  jathold := nil;



end;

procedure TSoundDevice_Android.SetupWave;
var
  am: JAudioMAnagerClass;
  af: JAudioFormatClass;
  at: JAudioTrackClass;
  i1,i2,i3,i4,i5,i6: integer;
  mp: cardinal;
begin
  inherited;


//  self.Policy := 1;
//  self.Priority := $FFFFFED;

  Loop := true;
  am := TJAudioMAnager.JavaClass;
  af := TJAudioFormat.JavaClass;
  at := TJAudioTrack.javaClass;

  mbs := TJAudioTrack.JavaClass.getMinBufferSize(44100,  af.CHANNEL_CONFIGURATION_STEREO, af.ENCODING_PCM_16BIT);
  mbs := mbs;
//  SetLength(buf, mbs div sizeof(TStereoSample));

  i1 := TJAUdioManager.JavaClass.STREAM_MUSIC;
  i2 := 44100;
  i3 := af.CHANNEL_CONFIGURATION_STEREO;
  i4 := af.ENCODING_PCM_16BIT;
  i5 := mbs;
  i6 := at.MODE_STREAM;
  jathold := TJAudioTrack.JavaClass.init(i1,i2,i3,i4,i5,i6);
  jat := jathold;
  jat.setPlaybackRate(44100);

  jabhold := TJavaArray<byte>.create(mbs);
  jab := jabhold;

  //jat := TJAudioTrack.JavaClass.init(TJAUdioManager.JavaClass.STREAM_MUSIC, 44100,af.CHANNEL_CONFIGURATION_STEREO, af.ENCODING_PCM_16BIT, mbs, at.MODE_STREAM);

  jat.play;


end;

end.
