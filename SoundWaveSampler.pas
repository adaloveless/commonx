unit SoundWaveSampler;
{$I DelphiDefs.inc}
interface

uses
{$IFDEF NEED_FAKE_ANSISTRING}
  ios.stringx.iosansi,
{$ENDIF}
  classes, betterobject, multibuffermemoryfilestream, systemx, typex, helpers_stream, sysutils, soundsample;


type
  TLocalFileStream = TMultiBufferMemoryFileStream;

  TXSoundFormat = (sf8, sf16, sf24, sf32, sf64, sf32f, sf64f);

  TSoundInfo = record
  public
    fmt: TXSoundFormat;
    channels: smallint;
    rate: cardinal;
    bitspersample: ni;
    bytespersample: ni;
    lengthInSamples: ni;
    procedure AutoConfigure;
  end;

  TSoundSampleFunc = function (sample: int64; startchan: ni = 0): TStereoSoundSample of object;

  TSoundWaveSampler = class(TBetterObject)
  private
    FFilename: string;
    procedure SetFileName(const Value: string);
    procedure CleanupStream;
    function GetSoundSample_<T>(sample: int64; startchan:ni=0): TStereoArray<T>;
    function GetSoundSample_8i(sample: int64; startchan: ni = 0): TStereoSoundSample;
    function GetSoundSample_16i(sample: int64; startchan: ni = 0): TStereoSoundSample;
    function GetSoundSample_24i(sample: int64; startchan: ni = 0): TStereoSoundSample;
    function GetSoundSample_32i(sample: int64; startchan: ni = 0): TStereoSoundSample;
    function GetSoundSample_64i(sample: int64; startchan: ni = 0): TStereoSoundSample;
    function GetSoundSample_32f(sample: int64; startchan: ni = 0): TStereoSoundSample;
    function GetSoundSample_64f(sample: int64; startchan: ni = 0): TStereoSoundSample;
  protected
    strm: TStream;
    fmt: TSoundInfo;
    dataLength: cardinal;

    fnGetSoundSample: TSoundSampleFunc;
    procedure AutoConfigure;
    procedure ReadFormat_WAV;
    procedure ReadFormat;

  public
    datastart: int64;
    procedure Detach; override;
    property FileName: string read FFilename write SetFileName;
    function GetFormat: TSoundinfo;
  end;



implementation

{ TSoundWaveSampler }

{ TSoundInfo }



procedure TSoundWaveSampler.ReadFormat_Wav;
var
  c1, c2, c3, c4: ansichar;
  shit: int64;
  b: boolean;
begin
    // find beginning of fmt  section
    b := false;
    while not b do begin
      Stream_GuaranteeRead(strm, Pbyte(@c1), 1);

      if c1 = 'f' then begin
        Stream_GuaranteeRead(strm, Pbyte(@c2), 1);
        if not(c2 = 'm') then
          continue;
        Stream_GuaranteeRead(strm, Pbyte(@c3), 1);
        if not(c3 = 't') then
          continue;
        Stream_GuaranteeRead(strm, Pbyte(@c4), 1);
        if not(c4 = ' ') then
          continue;
        b := true;
      end;
    end;

    // chunk size
    Stream_GuaranteeRead(strm, Pbyte(@shit), 4);
    // compression PCM
    Stream_GuaranteeRead(strm, Pbyte(@shit), 2);
    // 1 channel
    Stream_GuaranteeRead(strm, Pbyte(@fmt.channels), 2);
    // sample rate
    Stream_GuaranteeRead(strm, Pbyte(@fmt.rate), 4);
    // bytes per second
    Stream_GuaranteeRead(strm, Pbyte(@shit), 4);
    // block align
    Stream_GuaranteeRead(strm, Pbyte(@shit), 2);
    // bits per sample
    var bitspersample: smallint := 0;
    Stream_GuaranteeRead(strm, Pbyte(@bitsperSample), 2);
    fmt.bitspersample := bitspersample;
    case fmt.bitspersample of
      8: fmt.fmt := sf8;
      16: fmt.fmt := sf16;
      24: fmt.fmt := sf24;
      32: fmt.fmt := sf32f;
      64: fmt.fmt := sf64f;
    end;



// find beginning of data section
    b := false;
    while not b do begin
      Stream_GuaranteeRead(strm, Pbyte(@c1), 1);
      if c1 = 'd' then begin
        Stream_GuaranteeRead(strm, Pbyte(@c2), 1);
        if not(c2 = 'a') then
          continue;
        Stream_GuaranteeRead(strm, Pbyte(@c3), 1);
        if not(c3 = 't') then
          continue;
        Stream_GuaranteeRead(strm, Pbyte(@c4), 1);
        if not(c4 = 'a') then
          continue;
        b := true;
      end;
    end;
    fmt.bytespersample := fmt.bitspersample div 8;

    // read length
    Stream_GuaranteeRead(strm, Pbyte(@dataLength), 4);
    dataStart := strm.Position;
    var divisor: ni := fmt.bytespersample*fmt.channels;
    if divisor = 0 then begin
      if fmt.channels = 0 then begin
        raise ECritical.create('WAV cannot have 0 channels');
      end;
      if fmt.channels = 0 then begin
        raise ECritical.create('WAV cannot have 0 bytes per sample');
      end;
    end;
    AutoConfigure;
    fmt.lengthInSamples := strm.Size div (fmt.bytespersample*fmt.channels);



end;

procedure TSoundWaveSampler.AutoConfigure;
begin
  fmt.autoconfigure;
  CASE fmt.fmt of
    sf8: fnGetSoundSample :=GetSoundSample_8i;
    sf16: fnGetSoundSample :=GetSoundSample_16i;
    sf24: fnGetSoundSample :=GetSoundSample_24i;
    sf32: fnGetSoundSample :=GetSoundSample_32i;
    sf32f: fnGetSoundSample :=GetSoundSample_32f;
    sf64f: fnGetSoundSample :=GetSoundSample_64f;
  else
    raise Ecritical.create('no GetSoundSampleF defined for sound format');
  end;

end;

procedure TSoundWaveSampler.CleanupStream;
begin
  if strm <> nil then begin
    strm.free;
    strm := nil;
  end;
end;

procedure TSoundWaveSampler.Detach;
begin
  if detached then exit;

  CleanupStream;
  inherited;

end;


function TSoundWaveSampler.GetSoundSample_<T>(sample: int64;
  startchan: ni): TStereoArray<T>;
begin
  var chanstoread := 2;
  if startchan=(fmt.channels-1) then
    chansToRead := 1;

  strm.Seek(sample*(sizeof(T)*fmt.channels)+(startchan*sizeof(T)), soBeginning);
  Stream_GuaranteeRead(strm, @result[0],sizeof(T)*chansToRead);


end;

function TSoundWaveSampler.GetFormat: TSoundinfo;
begin
  result := fmt;
end;

function TSoundWaveSampler.GetSoundSample_16i(sample: int64;
  startchan: ni): TStereoSoundSample;
begin
  var a : TStereoArray<smallint> := GetSoundSample_<smallint>(sample, startchan);
  result.FromStereoArray_16i(a);
end;

function TSoundWaveSampler.GetSoundSample_24i(sample: int64;
  startchan: ni): TStereoSoundSample;
begin
  var a : TStereoArray<uint24> := GetSoundSample_<uint24>(sample, startchan);
  result.FromStereoArray_24i(a);
end;

function TSoundWaveSampler.GetSoundSample_32f(sample: int64;
  startchan: ni): TStereoSoundSample;
begin
  var a : TStereoArray<single> := GetSoundSample_<single>(sample, startchan);
  result.FromStereoArray_32f(a);

end;

function TSoundWaveSampler.GetSoundSample_32i(sample: int64;
  startchan: ni): TStereoSoundSample;
begin
  var a : TStereoArray<integer> := GetSoundSample_<integer>(sample, startchan);
  result.FromStereoArray_32i(a);

end;

function TSoundWaveSampler.GetSoundSample_64f(sample: int64;
  startchan: ni): TStereoSoundSample;
begin
  var a : TStereoArray<double> := GetSoundSample_<double>(sample, startchan);
  result.FromStereoArray_64f(a);


end;

function TSoundWaveSampler.GetSoundSample_64i(sample: int64;
  startchan: ni): TStereoSoundSample;
begin
  var a : TStereoArray<int64> := GetSoundSample_<int64>(sample, startchan);
  result.FromStereoArray_64i(a);

end;

function TSoundWaveSampler.GetSoundSample_8i(sample: int64;
  startchan: ni): TStereoSoundSample;
begin
  var a : TStereoArray<shortint> := GetSoundSample_<shortint>(sample, startchan);
  result.FromStereoArray_8i(a);

end;

procedure TSoundWaveSampler.ReadFormat;
begin
  ReadFormat_WAV;

end;


procedure TSoundWaveSampler.SetFileName(const Value: string);
begin
  cleanupstream;
  FFilename := Value;
  strm :=TLocalFileStream.create(value, fmopenRead+fmShareDenyWrite);
  ReadFormat;


end;

{ TSoundInfo }

procedure TSoundInfo.AutoConfigure;
begin
  case fmt of
    sf8: bitspersample := 8;
    sf16: bitspersample := 16;
    sf24: bitspersample := 24;
    sf32,sf32f: bitspersample :=32;
    sf64,sf64f: bitspersample :=64;
  end;
  bytespersample := bitspersample shr 3;


end;

end.
