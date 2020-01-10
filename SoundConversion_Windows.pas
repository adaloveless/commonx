unit SoundConversion_Windows;

interface
{$IFDEF MSWINDOWS}
uses
  systemx, memoryfilestream, soundinterfaces, windows, sysutils, typex,
  helpers_stream, classes, maths, geometry, soundsample,
  debug, dir,dirfile, commandprocessor, generics.collections, mmsystem;
type
  TLocalMFS = TMemoryFileStream;
  Tcmd_Mp3ToBooger = class(TSoundCommand)
  private
    FInfile: string;
    FOutFile: string;
  public
    procedure DoExecute; override;
    property InFile: string read FInfile write FInfile;
    property OutFile: string read FOutFile write FOutFile;
  end;

  Tcmd_WaveToBooger = class(TCommand)
  private
    FIn: string;
    FOut: string;
  public
    procedure InitExpense;override;
    procedure DoExecute;override;
    property InFile: string read FIn write Fin;
    property OutFile: string read FOut write FOut;
  end;

  Tcmd_BoogerToWave = class(TCommand)
  private
    FIn: string;
    FOut: string;
  public
    procedure InitExpense;override;
    procedure DoExecute;override;
    property InFile: string read FIn write Fin;
    property OutFile: string read FOut write FOut;
  end;


procedure Mp3toBooger(sFile: string; sOutFile: string);
procedure WaveToBooger(sFile: string; sOutFile: string);
procedure WaveToBoogerPure(sFile: string; sOutFile: string);
procedure WaveToBoogerSSE_Sub32(msIn, msOut: TLocalMFS; chans: nativeint; dataStart: int64;samplerate: nativeint;iInLenInSAmples: nativeint);
procedure WaveToBoogerSSE_Sub24(msIn, msOut: TLocalMFS; chans: nativeint; dataStart: int64;samplerate: nativeint;iInLenInSAmples: nativeint);
procedure WaveToBoogerSSE_Sub16(msIn, msOut: TLocalMFS; chans: nativeint; dataStart: int64;samplerate: nativeint;iInLenInSAmples: nativeint);
procedure WaveToBoogerSSE(sFile: string; sOutFile: string);
procedure AllWaveToBooger(sInDir: string; sOutDir: string);
procedure BoogerToWave(sFile: string; sOutFile: string; iforceSampleRate: integer = 0);


{$ENDIF}
implementation
{$IFDEF MSWINDOWS}
uses
  soundconversions_commandline, booger;



procedure Tcmd_Mp3ToBooger.DoExecute;
begin
  inherited;
{$IFDEF MSWINDOWS}
  Mp3toBooger(InFile, OutFile);
{$ENDIF}
end;


procedure Mp3toBooger(sFile: string; sOutFile: string);
var
  smp3, swav, sboog: string;
  bWavExisted: boolean;
begin
  smp3 := sFile;
  swav := changefileext(sFile, '.wav');
  sboog := sOutFile;

  bWavExisted := FileExists(changefileext(sFile, '.wav'));
  if lowercase(extractfileext(sFile)) <> '.wav' then
    Mp3ToWave(sFile);
  WaveToBooger(swav, sboog);
  if not bWavExisted then
    deletefile(swav);

end;

{$IFDEF MSWINDOWS}
procedure WaveToBoogerPURE(sFile: string; sOutFile: string);
var
  msIn: TlocalMFS;
  msOut: TlocalMFS;
  c1, c2, c3, c4: ansichar;
  iLen, iLen2: integer;
  i: int64;
  t: integer;
  b: boolean;
  iTemp: integer;
  thisAmp, lastAmp: nativeint;
  avgAmp: smallint;
  dataStart: int64;
  bitrate: DWORD;
  shit: int64;
  sourcepos: int64;
  rsourcepos: nativefloat;
  amp1: smallint;
  amp2: smallint;
  chans: smallint;
  ver: smallint;
  i1, i2: int64;
  rStagger: single;
  iSamplesToRemap: integer;
  iOriginalSamplestoRemap: integer;
  BytesperSample: smallint;
const
  target_bitrate = 44100;
begin
  msIn := TlocalMFS.Create(sFile, fmOpenRead);
  msIn.BufferSize := 512;
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

    // chunk size
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 4);
    // compression PCM
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 2);
    // 1 channel
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
    Stream_GuaranteeRead(msIn, Pbyte(@iLen), 4);
    dataStart := msIn.Position;
    msOut := TlocalMFS.Create(sOutFile, fmCreate);
    msout.Buffersize := 100000000;
    try
      // version
      ver := 2;
      c1 := 'B';
      c2 := 'O';
      c3 := 'O';
      c4 := 'G';
      Stream_guaranteeWrite(msOut, Pbyte(@c1), 1);
      Stream_guaranteeWrite(msOut, Pbyte(@c2), 1);
      Stream_guaranteeWrite(msOut, Pbyte(@c3), 1);
      Stream_guaranteeWrite(msOut, Pbyte(@c4), 1);
      Stream_guaranteeWrite(msOut, Pbyte(@ver), 2);
      // channels
      Stream_guaranteeWrite(msOut, Pbyte(@chans), 2);
      // calc new length
      iLen2 := round((target_bitrate / bitrate) * (iLen div BytesperSample));
      iLen2 := iLen2 * 2;
      Stream_guaranteeWrite(msOut, Pbyte(@iLen2), 4);
      iLen2 := iLen2 div 2;
      shit := -1;
      // cue point 1
      Stream_guaranteeWrite(msOut, Pbyte(@shit), 4);
      // cue point 2
      Stream_guaranteeWrite(msOut, Pbyte(@shit), 4);
      // loop point 1
      Stream_guaranteeWrite(msOut, Pbyte(@shit), 4);
      // loop point 2
      Stream_guaranteeWrite(msOut, Pbyte(@shit), 4);

      lastAmp := 0;
      amp1 := 0;
      amp2 := 0;
      iSamplesToRemap := (iLen2 div chans);
      iOriginalSamplestoRemap := (iLen div chans) div BytesperSample;

      for t := 1 to iSamplesToRemap do begin
        rsourcepos := ((t / iSamplesToRemap) * iOriginalSamplestoRemap) - 1;

        if chans = 2 then begin
          // LEFT CHANNEL
          i1 := dataStart + (trunc(rsourcepos) * chans * BytesperSample);
          i2 := dataStart + (trunc(rsourcepos + 1) * chans * BytesperSample);

          msIn.Seek(i1, soBeginning);
          Stream_GuaranteeRead(msIn, Pbyte(@amp1), BytesperSample, false);
          msIn.Seek(i2, soBeginning);
          Stream_GuaranteeRead(msIn, Pbyte(@amp2), BytesperSample, false);
          rStagger := rsourcepos - (trunc(rsourcepos));
          iTemp := round(Interpolate(rStagger, 1, amp1, amp2));

          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;

          thisAmp := iTemp;
          Stream_guaranteeWrite(msOut, Pbyte(@thisAmp), 2);

          // RIGHT CHANNEL
          i1 := dataStart + (trunc(rsourcepos) * chans * BytesperSample) + BytesperSample;
          i2 := dataStart + (trunc(rsourcepos + 1) * chans * BytesperSample)+ BytesperSample;

          i1 := dataStart + (trunc(rsourcepos) * chans * BytesperSample);
          i2 := dataStart + (trunc(rsourcepos + 1) * chans * BytesperSample);

          msIn.Seek(i1, soBeginning);
          Stream_GuaranteeRead(msIn, Pbyte(@amp1), BytesperSample, false);
          msIn.Seek(i2, soBeginning);
          Stream_GuaranteeRead(msIn, Pbyte(@amp2), BytesperSample, false);
          rStagger := rsourcepos - (trunc(rsourcepos));
          iTemp := round(Interpolate(rStagger, 1, amp1, amp2));

          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;
          thisAmp := iTemp;

          Stream_guaranteeWrite(msOut, Pbyte(@thisAmp), 2);

        end
        else begin
          msIn.Seek(((trunc(rsourcepos) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp1), BytesperSample, false);
          msIn.Seek(((trunc(rsourcepos + 1) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp2), BytesperSample, false);
          iTemp := round(Interpolate(rsourcepos - (trunc(rsourcepos)), 1, amp1,
              amp2));
          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;
          thisAmp := iTemp;

          Stream_guaranteeWrite(msOut, Pbyte(@thisAmp), 2);

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
procedure WaveToBoogerSSE_Sub24(msIn, msOut: TlocalMFS; chans: nativeint; dataStart: int64; samplerate: nativeint;iInLenInSAmples: nativeint);
var
  ss1,ss2,ss3,ss4,ss5: TStereoSoundSample;
  c1, c2, c3, c4: ansichar;
  iLen2: integer;
  i: int64;
  t: integer;
  b: boolean;
  iTemp: nativeint;
  outAmp, lastAmp: cardinal;

  avgAmp: smallint;
  shit: int64;
//  sourcepos: int64;
  rsourcepos: nativefloat;
  amp1: nativeint;
  amp2: nativeint;
  sixbyte: int64;
  doubleamp: cardinal;
  ver: smallint;
  i1, i2: int64;
  rStagger: double;
  rFloor: double;
  iSamplesToRemap: integer;
  iOriginalSamplestoRemap: integer;
  BytesperSample: smallint;

  sz: int64;
  stride: nativeint;
  bigun: cardinal;
  smallun: word;
const
  target_samplerate = 44100;
  shifter = 24;
begin
//  GetMem(ss1, sizeof(TStereoSoundSample));
//  GetMem(ss2, sizeof(TStereoSoundSample));
//  GetMem(ss3, sizeof(TStereoSoundSample));
//  GetMem(ss4, sizeof(TStereoSoundSample));
//  GetMem(ss5, sizeof(TStereoSoundSample));
  try

    BytesPerSample := 3; //may be 32-bit aligned?
    stride := chans * BytesPerSample;

      lastAmp := 0;
      amp1 := 0;
      amp2 := 0;

      iOriginalSamplestoRemap := iInLenINSamples;
      iLen2 := round((target_samplerate / samplerate) * iInLenINSamples);
      iSamplesToRemap := (iLen2 div chans);

      sz := msIn.Size;
      ss5.Left := 1/32768;
      ss5.Right := 1/32768;

      for t := 1 to iSamplesToRemap do begin
        rsourcepos := ((t / iSamplesToRemap) * iOriginalSamplestoRemap) - 1;


        if chans = 2 then begin
          sixbyte := 0;
          i1 := dataStart + (trunc(rsourcepos) * stride);
//          i2 := dataStart + (trunc(rsourcepos+1) * stride);
          i2 := i1 + stride;
//          i2 := i1 + (chans * BytesperSample);

          rStagger := Frac(rSourcePOs);

          msIn.Seek(i1, soBeginning);


          Stream_GuaranteeRead(msIn, Pbyte(@sixbyte), stride);
//          Stream_GuaranteeRead(msIn, @bigun, 4);
//          Stream_GuaranteeRead(msIn, @smallun, 2);
//          sixbyte := int64(bigun)+(int64(smallun) shl 32);


          ss1.Left := integer(cardinal(sixbyte and $FFFFFF) shl 8);
          ss1.Right := integer(int64(sixbyte and $FFFFFF000000) shr 16);          //GLOG.Debug(inttohex(sixbyte, 12));


          //rSTagger := 0;
          if rStagger <> 0 then begin
            msIn.Seek(i2, soBeginning);
            sixbyte := 0;
            if i2 < sz then begin
              Stream_GuaranteeRead(msIn, Pbyte(@sixbyte), stride);
//              Stream_GuaranteeRead(msIn, @bigun, 4);
//              Stream_GuaranteeRead(msIn, @smallun, 2);
//              sixbyte := int64(bigun)+(int64(smallun) shl 32);
            end;
            ss2.Left := integer(cardinal(sixbyte and $FFFFFF) shl 8);
            ss2.Right := integer(int64(sixbyte and $FFFFFF000000) shr 16);          //GLOG.Debug(inttohex(sixbyte, 12));
          end;



//          if rSTagger =1 then
//            rStagger := 0;
          if rStagger = 0 then begin
            ss4 := ss1 * ss5;
          end
          else begin
//            ss3.Left := rStagger;
//            ss3.Right := rStagger;

            ss4.InterpolateAndScale(ss1,ss2,rStagger, 32767);
//            ss4 := ((ss2-ss1) * (ss3))+ss1;
          end;







          iTemp := round(ss4.Left);

          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;

          amp1 := iTemp;



          iTemp := round(ss4.Right);

          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;

          amp2 := iTemp;

//          amp2 := round(SineSynth(t, 500+((25000 * ((t)/iSamplesToRemap))), 3000, 44100));
//          amp1 := round(SineSynth(t, 700+((15000 * ((t)/iSamplesToRemap))), 3000, 44100));




          doubleamp := (word(amp2) shl 16)+word(amp1);
          //glog.debug(inttohex(doubleamp, 8));

          Stream_guaranteeWrite(msOut, Pbyte(@doubleamp), 4);





        end
        else begin
          amp1 := 0;
          amp2 := 0;
          msIn.Seek(((trunc(rsourcepos) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp1), BytesperSample, false);
          msIn.Seek(((trunc(rsourcepos + 1) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp2), BytesperSample, false);
          iTemp := round(Interpolate(rsourcepos - (trunc(rsourcepos)), 1, amp1,
              amp2));
          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;
          outAmp := iTemp;

          Stream_guaranteeWrite(msOut, Pbyte(@outAmp), 2);

        end;
      end;
  finally
//    FreeMem(ss1);
//    FreeMem(ss2);
//    FreeMem(ss3);
//    FreeMem(ss4);
//    FreeMem(ss5);
  end;

end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure WaveToBoogerSSE_Sub64(msIn, msOut: TlocalMFS; chans: nativeint; dataStart: int64; samplerate: nativeint;iInLenInSAmples: nativeint);
var
  ss1,ss2,ss3,ss4: TStereoSoundSample;
  c1, c2, c3, c4: ansichar;
  iLen2: integer;
  i: int64;
  t: integer;
  b: boolean;
  iTemp: nativeint;
  outAmp, lastAmp: double;

  avgAmp: smallint;
  shit: int64;
//  sourcepos: int64;
  rsourcepos: double;
  famp1, famp2: double;
  amp1: nativeint;
  amp2: nativeint;
  sixteenbyte: array[0..1] of int64;
  doubleamp: cardinal;
  ver: smallint;
  i1, i2: int64;
  rStagger: double;
  rFloor: double;
  iSamplesToRemap: integer;
  iOriginalSamplestoRemap: integer;
  BytesperSample: smallint;

  sz: int64;
  stride: nativeint;
  bigun: cardinal;
  smallun: word;
const
  target_samplerate = 44100;
  shifter = 32;
begin
//  GetMem(ss1, sizeof(TStereoSoundSample));
//  GetMem(ss2, sizeof(TStereoSoundSample));
//  GetMem(ss3, sizeof(TStereoSoundSample));
//  GetMem(ss4, sizeof(TStereoSoundSample));
//  GetMem(ss5, sizeof(TStereoSoundSample));
  try

    BytesPerSample := 8; //may be 32-bit aligned?
    stride := chans * BytesPerSample;

      lastAmp := 0;
      amp1 := 0;
      amp2 := 0;

      iOriginalSamplestoRemap := iInLenInSamples;
      iLen2 := trunc((target_samplerate / samplerate) * iInLenInSamples);
      iSamplesToRemap := (iLen2);

      sz := msIn.Size;
//      ss5.Left := 32768;
//      ss5.Right := 32768;

//      Debug.ConsoleLog('WaveToBoogerSSE_Sub32 will remap '+inttostr(iSamplesToRemap)+' samples from '+inttostr((iInLenInSamples)));
      for t := 1 to iSamplesToRemap do begin

        rsourcepos := ((t / iSamplesToRemap) * iOriginalSamplestoRemap) - 1;
        if rsourcepos >= iOriginalSamplestoRemap then begin
          Debug.ConsoleLog('End:'+floattostr(rSourcePos)+'='+inttostr(iOriginalSamplesToRemap-1));
          rsourcepos := iOriginalSamplestoRemap-1;
          Debug.ConsoleLog('Now:'+floattostr(rSourcePos));
        end;

        if chans = 2 then begin
          sixteenbyte[0] := 0;
          sixteenbyte[1] := 0;

          i1 := dataStart + (trunc(rsourcepos) * stride);
//          i2 := dataStart + (trunc(rsourcepos+1) * stride);
          i2 := i1 + stride;
//          i2 := i1 + (chans * BytesperSample);

          rStagger := Frac(rSourcePOs);



          msIn.Seek(i1, soBeginning);


          Stream_GuaranteeRead(msIn, Pbyte(@sixteenbyte[0]), stride);
//          Stream_GuaranteeRead(msIn, @bigun, 4);
//          Stream_GuaranteeRead(msIn, @smallun, 2);
//          sixbyte := int64(bigun)+(int64(smallun) shl 32);

          famp1 := pdouble(pbyte(@sixteenbyte[0]))^;
          ss1.Left := famp1;
          famp2 := pdouble(pbyte(@sixteenbyte[1]))^;




          ss1.Right := famp2;
//          ss1.Left := integer(cardinal(eightbyte and $FFFFFFFF));
//          ss1.Right := integer(cardinal((eightbyte shr shifter) and $FFFFFFFF));          //GLOG.Debug(inttohex(sixbyte, 12));


{$DEFINE DISABLE_RESAMPLLING}
{$IFDEF DISABLE_RESAMPLING}
          rSTagger := 0;
{$ENDIF}
          if rStagger <> 0 then begin
            msIn.Seek(i2, soBeginning);
            sixteenbyte[0] := 0;
            sixteenbyte[1] := 0;
            if i2 < sz then begin
              Stream_GuaranteeRead(msIn, Pbyte(@sixteenbyte[0]), stride);
//              Stream_GuaranteeRead(msIn, @bigun, 4);
//              Stream_GuaranteeRead(msIn, @smallun, 2);
//              sixbyte := int64(bigun)+(int64(smallun) shl 32);
            end;
            ss2.Left := pdouble(pbyte(@sixteenbyte[0]))^;
            ss2.Right := pdouble(pbyte(@sixteenbyte[1]))^;
          end;



//          if rSTagger =1 then
//            rStagger := 0;
          if rStagger = 0 then begin
            ss4 := ss1 * 32767;
          end
          else begin
//            ss3.Left := rStagger;
//            ss3.Right := rStagger;

            ss4.InterpolateAndScale(ss1,ss2,rStagger, 32767)
//            ss4 := ((ss2-ss1) * (ss3))+ss1;
          end;


          iTemp := round(ss4.Left);

          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;

          amp1 := iTemp;



          iTemp := round(ss4.Right);

          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;

          amp2 := iTemp;

//          amp2 := round(SineSynth(t, 500+((25000 * ((t)/iSamplesToRemap))), 3000, 44100));
//          amp1 := round(SineSynth(t, 700+((15000 * ((t)/iSamplesToRemap))), 3000, 44100));



        //rSourcePos := rSourcePOs * 2;
//        if (t mod 10000)=0 then
//          Debug.ConsoleLog(inttostr(t)+'='+inttostr(trunc(rSourcePOs))+' <'+inttostr(amp1)+'!'+inttostr(amp2)+'> ');

          doubleamp := (word(amp2) shl 16)+word(amp1);
          //glog.debug(inttohex(doubleamp, 8));

          Stream_guaranteeWrite(msOut, Pbyte(@doubleamp), 4);
          //Stream_guaranteeWrite(msOut, @doubleamp, 4);





        end
        else begin
          amp1 := 0;
          amp2 := 0;
          msIn.Seek(((trunc(rsourcepos) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp1), BytesperSample, false);
          msIn.Seek(((trunc(rsourcepos + 1) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp2), BytesperSample, false);
          iTemp := round(Interpolate(rsourcepos - (trunc(rsourcepos)), 1, amp1,
              amp2)* (1/32767));
          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;
          outAmp := iTemp;

          Stream_guaranteeWrite(msOut, Pbyte(@outAmp), 2);

        end;
      end;
  finally
//    FreeMem(ss1);
//    FreeMem(ss2);
//    FreeMem(ss3);
//    FreeMem(ss4);
//    FreeMem(ss5);
  end;

end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure WaveToBoogerSSE_Sub32(msIn, msOut: TlocalMFS; chans: nativeint; dataStart: int64; samplerate: nativeint;iInLenInSAmples: nativeint);
var
  ss1,ss2,ss3,ss4: TStereoSoundSample;
  c1, c2, c3, c4: ansichar;
  iLen2: integer;
  i: int64;
  t: integer;
  b: boolean;
  iTemp: nativeint;
  outAmp, lastAmp: single;

  avgAmp: smallint;
  shit: int64;
//  sourcepos: int64;
  rsourcepos: double;
  famp1, famp2: single;
  amp1: nativeint;
  amp2: nativeint;
  eightbyte: int64;
  doubleamp: cardinal;
  ver: smallint;
  i1, i2: int64;
  rStagger: double;
  rFloor: double;
  iSamplesToRemap: integer;
  iOriginalSamplestoRemap: integer;
  BytesperSample: smallint;

  sz: int64;
  stride: nativeint;
  bigun: cardinal;
  smallun: word;
const
  target_samplerate = 44100;
  shifter = 32;
begin
//  GetMem(ss1, sizeof(TStereoSoundSample));
//  GetMem(ss2, sizeof(TStereoSoundSample));
//  GetMem(ss3, sizeof(TStereoSoundSample));
//  GetMem(ss4, sizeof(TStereoSoundSample));
//  GetMem(ss5, sizeof(TStereoSoundSample));
  try

    BytesPerSample := 4; //may be 32-bit aligned?
    stride := chans * BytesPerSample;

      lastAmp := 0;
      amp1 := 0;
      amp2 := 0;

      iOriginalSamplestoRemap := iInLenInSamples;
      iLen2 := trunc((target_samplerate / samplerate) * iInLenInSamples);
      iSamplesToRemap := (iLen2);

      sz := msIn.Size;
//      ss5.Left := 32768;
//      ss5.Right := 32768;

//      Debug.ConsoleLog('WaveToBoogerSSE_Sub32 will remap '+inttostr(iSamplesToRemap)+' samples from '+inttostr((iInLenInSamples)));
      for t := 1 to iSamplesToRemap do begin

        rsourcepos := ((t / iSamplesToRemap) * iOriginalSamplestoRemap) - 1;
        if rsourcepos >= iOriginalSamplestoRemap then begin
          Debug.ConsoleLog('End:'+floattostr(rSourcePos)+'='+inttostr(iOriginalSamplesToRemap-1));
          rsourcepos := iOriginalSamplestoRemap-1;
          Debug.ConsoleLog('Now:'+floattostr(rSourcePos));
        end;

        if chans = 2 then begin
          eightbyte := 0;

          i1 := dataStart + (trunc(rsourcepos) * stride);
//          i2 := dataStart + (trunc(rsourcepos+1) * stride);
          i2 := i1 + stride;
//          i2 := i1 + (chans * BytesperSample);

          rStagger := Frac(rSourcePOs);



          msIn.Seek(i1, soBeginning);


          Stream_GuaranteeRead(msIn, Pbyte(@eightbyte), stride);
//          Stream_GuaranteeRead(msIn, @bigun, 4);
//          Stream_GuaranteeRead(msIn, @smallun, 2);
//          sixbyte := int64(bigun)+(int64(smallun) shl 32);

          famp1 := psingle(pbyte(@eightbyte))^;
          ss1.Left := famp1;
          famp2 := psingle(pbyte(@eightbyte)+4)^;




          ss1.Right := famp2;
//          ss1.Left := integer(cardinal(eightbyte and $FFFFFFFF));
//          ss1.Right := integer(cardinal((eightbyte shr shifter) and $FFFFFFFF));          //GLOG.Debug(inttohex(sixbyte, 12));


{$DEFINE DISABLE_RESAMPLLING}
{$IFDEF DISABLE_RESAMPLING}
          rSTagger := 0;
{$ENDIF}
          if rStagger <> 0 then begin
            msIn.Seek(i2, soBeginning);
            eightbyte := 0;
            if i2 < sz then begin
              Stream_GuaranteeRead(msIn, Pbyte(@eightbyte), stride);
//              Stream_GuaranteeRead(msIn, @bigun, 4);
//              Stream_GuaranteeRead(msIn, @smallun, 2);
//              sixbyte := int64(bigun)+(int64(smallun) shl 32);
            end;
            ss2.Left := psingle(pbyte(@eightbyte))^;
            ss2.Right := psingle(pbyte(@eightbyte)+4)^;
          end;



//          if rSTagger =1 then
//            rStagger := 0;
          if rStagger = 0 then begin
            ss4 := ss1 * 32767;
          end
          else begin
//            ss3.Left := rStagger;
//            ss3.Right := rStagger;

            ss4.InterpolateAndScale(ss1,ss2,rStagger, 32767)
//            ss4 := ((ss2-ss1) * (ss3))+ss1;
          end;


          iTemp := round(ss4.Left);

          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;

          amp1 := iTemp;



          iTemp := round(ss4.Right);

          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;

          amp2 := iTemp;

//          amp2 := round(SineSynth(t, 500+((25000 * ((t)/iSamplesToRemap))), 3000, 44100));
//          amp1 := round(SineSynth(t, 700+((15000 * ((t)/iSamplesToRemap))), 3000, 44100));



        //rSourcePos := rSourcePOs * 2;
//        if (t mod 10000)=0 then
//          Debug.ConsoleLog(inttostr(t)+'='+inttostr(trunc(rSourcePOs))+' <'+inttostr(amp1)+'!'+inttostr(amp2)+'> ');

          doubleamp := (word(amp2) shl 16)+word(amp1);
          //glog.debug(inttohex(doubleamp, 8));

          Stream_guaranteeWrite(msOut, Pbyte(@doubleamp), 4);
          //Stream_guaranteeWrite(msOut, @doubleamp, 4);

//          if (t mod 100) = 0 then
//            debug.consolelog(inttohex(doubleamp, 8));



        end
        else begin
          amp1 := 0;
          amp2 := 0;
          msIn.Seek(((trunc(rsourcepos) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp1), BytesperSample, false);
          msIn.Seek(((trunc(rsourcepos + 1) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp2), BytesperSample, false);
          iTemp := round(Interpolate(rsourcepos - (trunc(rsourcepos)), 1, amp1,
              amp2)* (1/32767));
          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;
          outAmp := iTemp;

          Stream_guaranteeWrite(msOut, Pbyte(@outAmp), 2);

        end;
      end;
  finally
//    FreeMem(ss1);
//    FreeMem(ss2);
//    FreeMem(ss3);
//    FreeMem(ss4);
//    FreeMem(ss5);
  end;

end;
{$ENDIF}



{$IFDEF MSWINDOWS}
procedure WaveToBoogerSSE_Sub16(msIn, msOut: TlocalMFS; chans: nativeint; dataStart: int64;samplerate: nativeint;iInLenInSAmples: nativeint);
var
  c1, c2, c3, c4: ansichar;
  iLen2: integer;
  i: int64;
  t: integer;
  b: boolean;
  iTemp: nativeint;
  thisAmp, lastAmp: smallint;
  avgAmp: smallint;
  shit: int64;
//  sourcepos: int64;
  rsourcepos: nativefloat;
  amp1: smallint;
  amp2: smallint;
  doubleamp: cardinal;
  ver: smallint;
  i1, i2: int64;
  rStagger: double;
  rFloor: double;
  iSamplesToRemap: integer;
  iOriginalSamplestoRemap: integer;
  BytesperSample: smallint;
  ss1,ss2,ss3,ss4: TStereoSoundSample;
  sz: int64;
const
  target_samplerate = 44100;
begin
  BytesPerSample := 2; //may be 32-bit aligned?

      lastAmp := 0;
      amp1 := 0;
      amp2 := 0;

      iOriginalSamplestoRemap := iInLenINSamples;
      iLen2 := round((target_samplerate / samplerate) * iInLenINSamples);
      iSamplesToRemap := iLen2;

      sz := msIn.Size;

      for t := 1 to iSamplesToRemap do begin
        rsourcepos := ((t / iSamplesToRemap) * iOriginalSamplestoRemap) - 1;


        if chans = 2 then begin
          // LEFT CHANNEL
          i1 := dataStart + (trunc(rsourcepos) * chans * BytesperSample);
          i2 := dataStart + (trunc(rsourcepos+1) * chans * BytesperSample);
          //i2 := i1 + (chans * BytesperSample);

          rStagger := Frac(rSourcePOs);

          msIn.Seek(i1, soBeginning);
          if i2 < sz then
            Stream_GuaranteeRead(msIn, Pbyte(@doubleamp), BytesperSample*chans);
          ss1.Left := smallint(doubleamp and 65535);
          ss1.Right := smallint((doubleamp shr 16) and 65535);


          if rStagger <> 0 then begin
            msIn.Seek(i2, soBeginning);
            if i2 < sz then
              Stream_GuaranteeRead(msIn, Pbyte(@doubleamp), BytesperSample*chans);
            ss2.Left := smallint(doubleamp and 65535);
            ss2.Right := smallint((doubleamp shr 16) and 65535);
          end;



//          if rSTagger =1 then
//            rStagger := 0;
          if rStagger = 0 then
            ss4 := ss1
          else begin
            ss3.Left := rStagger;
            ss3.Right := rStagger;

            ss4 := ((ss2-ss1) * (ss3))+ss1;
          end;



          iTemp := round(ss4.Left);

{          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;}

          amp1 := iTemp;



          iTemp := round(ss4.Right);

{          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;}

          amp2 := iTemp;


          doubleamp := (word(amp2) shl 16)+word(amp1);

          Stream_guaranteeWrite(msOut, Pbyte(@doubleamp), 4);



        end
        else begin
          msIn.Seek(((trunc(rsourcepos) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp1), BytesperSample, false);
          msIn.Seek(((trunc(rsourcepos + 1) * BytesperSample)) + dataStart, 0);
          Stream_GuaranteeRead(msIn, Pbyte(@amp2), BytesperSample, false);
          iTemp := round(Interpolate(rsourcepos - (trunc(rsourcepos)), 1, amp1,
              amp2));
          if iTemp > 32767 then
            iTemp := 32767;
          if iTemp < -32768 then
            iTemp := -32768;
          thisAmp := iTemp;

          Stream_guaranteeWrite(msOut, Pbyte(@thisAmp), 2);

        end;
      end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure WaveToBoogerSSE(sFile: string; sOutFile: string);
var
  msIn: TlocalMFS;
  msOut: TlocalMFS;
  c1, c2, c3, c4: ansichar;
  iLen, iLen2: integer;
  i: int64;
  t: integer;
  b: boolean;
  iTemp: nativeint;
  thisAmp, lastAmp: smallint;
  avgAmp: smallint;
  dataStart: int64;
  samplerate: DWORD;
  shit: nativeint;
//  sourcepos: int64;
  rsourcepos: nativefloat;
  amp1: smallint;
  amp2: smallint;
  doubleamp: cardinal;
  chans: smallint;
  ver: smallint;
  i1, i2: int64;
  rStagger: double;
  rFloor: double;
  iSamplesToRemap: integer;
  iOriginalSamplestoRemap: integer;
  BitsPerSample, BytesperSample: smallint;
  ss1,ss2,ss3,ss4: TStereoSoundSample;
  sz: int64;
  iOriginalLenInSamples: int64;
  bh: TBoogerHeader;
const
  target_bitrate = 44100;
begin
  msIn := TlocalMFS.Create(sFile, fmOpenRead);
  msIn.BufferSize := 100000000;
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

    // chunk size
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 4);
    // compression PCM
    shit := 0;
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 2);
    if (shit <> 1) and (shit <> 3) then
      raise Exception.Create('Unknown compression '+inttostr(shit));
    // 1 channel
    Stream_GuaranteeRead(msIn, Pbyte(@chans), 2);
    // sample rate
    Stream_GuaranteeRead(msIn, Pbyte(@samplerate), 4);
    // bytes per second
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 4);
    // block align
    Stream_GuaranteeRead(msIn, Pbyte(@shit), 2);
    // bits per sample
    Stream_GuaranteeRead(msIn, Pbyte(@BitsperSample), 2);
    BytesperSample := BitsPerSample div 8;

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
    dataStart := msIn.Position;
    msOut := TlocalMFS.Create(sOutFile, fmCreate);
    msout.Buffersize := 100000000;
    try
      // version
      iOriginalLenInSamples := iLen div (bytespersample * chans);
      iLen2 := trunc((target_bitrate / samplerate) * iOriginalLenInSamples);
      iLen2 := iLen2 * 2 * chans;
      bh.Init;
      bh.ver := 3;
      bh.channels := chans;
      bh.length := iLen2;
      bh.samplerate := 44100;
      msOut.seek(0,soBeginning);
      Stream_GuaranteeWrite(msOut, pbyte(@bh), sizeof(bh));

      if BitsPerSample = 16 then begin
        WaveToBoogerSSE_Sub16(msIn, msOut, chans, datastart, samplerate, iOriginalLenInSamples);
      end else
      if BitsPerSample = 24 then begin
        WaveToBoogerSSE_Sub24(msIn, msOut, chans,datastart, samplerate, iOriginalLenInSamples);
      end else
      if BitsPerSample = 32 then begin
        WaveToBoogerSSE_Sub32(msIn, msOut, chans,datastart, samplerate, iOriginalLenInSamples);
      end else
      if BitsPerSample = 64 then begin
        WaveToBoogerSSE_Sub64(msIn, msOut, chans,datastart, samplerate, iOriginalLenInSamples);
      end else
        raise Exception.Create('Unsupported WAV bit depth '+inttostr(BitsPerSample));



    finally
      msOut.free;
    end;
  finally
    msIn.free;
  end;
end;

procedure WaveToBooger(sFile: string; sOutFile: string);
//var
//  tmStart: cardinal;
begin
//  tmStart := GetTicker;


  WaveToBoogerSSE(sFile, sOutFile);

//  WaveToBoogerPure(sFile, sOutFile);

//  GLog.debug(extractfilename(sFile)+' was converted to boog in '+inttostr(GEtTimeSince(tmStart))+'ms.','SSE_Stats');
end;


{$ENDIF}


{ Tcmd_WaveToBooger }

procedure Tcmd_WaveToBooger.DoExecute;
begin
  inherited;
  WaveToBooger(InFile, OutFile);
end;

procedure Tcmd_WaveToBooger.InitExpense;
begin
  inherited;
end;

{ Tcmd_BoogerToWave }

procedure Tcmd_BoogerToWave.DoExecute;
begin
  inherited;
  BoogerToWave(InFile, OutFile);
end;

procedure Tcmd_BoogerToWave.InitExpense;
begin
  inherited;

end;


procedure AllWaveToBooger(sInDir: string; sOutDir: string);
var
  d: TDirectory;
  f,ff: TFileInformation;
  cl: TCommandList<Tcmd_WavetoBooger>;
  c: Tcmd_WaveToBooger;
  sSrc, sTarget: string;
  bDo: boolean;
begin
  cl := TCommandList<Tcmd_WavetoBooger>.create;
  try
    ff := TFileInformation.create;
    try
      d := TDirectory.create(sInDir, '*.wav', 0,0, true, false, false);
      try
        while d.GetNextFile(f) do begin
          bDo := false;
          sSrc := f.fullname;
          sTarget := slash(sOutDir)+extractfilenamepart(sSrc)+'.boog';
          if not fileexists(sTarget) then begin
            bDo := true;
          end else begin
            bDo := FileAge(sSrc) > FileAge(sTarget);
          end;

          if bDo then begin
            c := Tcmd_WaveToBooger.create;
            c.InFile := sSrc;
            c.OutFile := sTarget;
            cl.add(c);
            c.start;

          end;
        end;
      finally
        d.Free;
      end;
    finally
      ff.Free;
    end;

    cl.WaitForAll;
    cl.ClearAndDestroyCommands;
  finally
    cl.Free;
    cl := nil;
  end;


end;

procedure BoogerToWave(sFile: string; sOutFile: string; iforceSampleRate: integer = 0);
var
  mfs: TlocalMFS;
  ss: TSoundStream;
  iData: cardinal;
  iSmallData: WORD;
  wf: waveformat_tag;
  t: integer;
  b: byte;
  iRate: integer;
  si: smallint;
  c: cardinal;
begin
  ss := TSoundStream.Create(sFile, fmOpenRead + fmShareDenyNone);
  try
    mfs := TlocalMFS.Create(sOutFile, fmCreate);
    try
      mfs.Seek(0,soBeginning);
      iData := $46464952; // RIFF
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4);

      iData := $0; // CHUNK SIZE (FILESIZE-8) --- we will write this later
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4);

      iData := $45564157;// WAVE
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4);

      iData := $20746d66; // fmt
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4);

      iData := 16;
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4); // size of fmt block

      iSmallData := WAVE_FORMAT_PCM; // compression code
      Stream_guaranteeWrite(mfs, Pbyte(@iSmallData), 2);

      iSmallData := ss.channels; // channels
      Stream_guaranteeWrite(mfs, Pbyte(@iSmallData), 2);

      if iForceSampleRate <> 0 then
        iData := iForcesampleRate
      else
        iData := ss.samplerate; // sample rate
      iRate := iData;
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4);

      iData := iRate * ss.BytesperSample * ss.channels; // bytes per second
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4);

// block align
      iSmallData := ss.BytesperSample * ss.channels;
      Stream_guaranteeWrite(mfs, Pbyte(@iSmallData), 2);

      iSmallData := 16; // bits per sample
      Stream_guaranteeWrite(mfs, Pbyte(@iSmallData), 2);

//      iSmallData := 0; // extra format bytes
//      Stream_guaranteeWrite(mfs, @iSmallData, 2);

      iData := $61746164; // 'data'
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4);


//      Debug.ConsoleLog('BoogerToWave will convert '+inttostr(ss.samplecount)+' samples');
      iData := ss.SampleCount * ss.channels * 2; // length of samples
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4);

{$IFDEF BYTECOPY}
      // sample data
      ss.SeekSample(0);
      if (ss.position + iDAta) > ss.size then
        raise ECritical.create('missing data '+inttostr((ss.position + iDAta) - ss.size)+' bytes');
      for t := 0 to iData-1 do begin
        Stream_GuaranteeRead(ss, Pbyte(@b), 1);
        Stream_guaranteeWrite(mfs, Pbyte(@b), 1);
      end;
{$ELSE}
      for t:= 0 to ss.samplecount-1 do begin
        ss.seeksample(t);
        case ss.channels of
          1: begin
            Stream_GuaranteeRead(ss, Pbyte(@si), sizeof(si));
            Stream_guaranteeWrite(mfs, Pbyte(@si), sizeof(si));
          end;
          2: begin
            Stream_GuaranteeRead(ss, Pbyte(@c), sizeof(c));
            Stream_guaranteeWrite(mfs, Pbyte(@c), sizeof(c));
          end;
        else
          raise ECritical.create('invalid channel count');
        end;

      end;

{$ENDIF}

      iData := mfs.Position - 8;
      mfs.Seek(4, soBeginning);
      Stream_guaranteeWrite(mfs, Pbyte(@iData), 4); // final length

    finally
      mfs.free;
    end;
  finally
    ss.free;
  end;

end;

{$ENDIF}


end.
