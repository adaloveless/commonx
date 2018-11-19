unit SoundConversion;
{$Message '********************************Compiling SoundConversion'}

interface
{$IFDEF MSWINDOWS}
{$IFNDEF MSWINDOWS}
  {$ERROR 'only supported on windows'}
{$ENDIF}

uses
  numbers, systemx, classes,
{$IFDEF MSWINDOWS}
  msacm, mmsystem,
{$ENDIF}
  soundinterfaces, multibuffermemoryfilestream, typex, sounddevice_mm, sysutils, helpers.stream ;


function MP3ToRawMemoryStream(sMP3File: string; out info: TMp3info): TMemoryStream;

{$ENDIF}
implementation

{$IFDEF MSWINDOWS}
function MP3ToRawMemoryStream(sMP3File: string; out info: TMp3info): TMemoryStream;
const
  MP3_BLOCK_SIZE = 522 div 1;
type
  TStreamType = TMultiBufferMemoryFileStream;
var
  wf2: TWaveFormatEx;
  wf1: Tmpeglayer3waveformat_tag;
  has: HACMStream;
  ash: TACMSTREAMHEADER;
  targetsize: cardinal;
  bufSource, bufTarget: PByte;
  mmr: cardinal;
  fsin: TStreamType;
  iToRead: nativeint;
  iRead: nativeint;
  f: dword;
  iPos: int64;
  mp3info: TMp3Info;
begin
  result := nil;
  try
    mp3info := Getmp3info(smp3File);
    //mp3info.samplerate := 22050;

    wf2 := GetStandardWaveFormat(mp3info.samplerate);
    FillMem(Pbyte(@wf1), sizeof(wf1), 0);



    wf1.wfx.cbSize := MPEGLAYER3_WFX_EXTRA_BYTES;
    wf1.wfx.wFormatTag := WAVE_FORMAT_MPEGLAYER3;
    wf1.wfx.nChannels := 2;
    wf1.wfx.nAvgBytesPerSec := 128;//128 * (1024 div 8);  // not really used but must be one of 64, 96, 112, 128, 160kbps
    wf1.wfx.wBitsPerSample := 0;                  // MUST BE ZERO
    wf1.wfx.nBlockAlign := 1;                     // MUST BE ONE
    wf1.wfx.nSamplesPerSec := mp3info.samplerate;              // 44.1kHz
    wf1.fdwFlags := MPEGLAYER3_FLAG_PADDING_OFF;
    wf1.nBlockSize := MP3_BLOCK_SIZE;             // voodoo value #1
    wf1.nFramesPerBlock := 1;                     // MUST BE ONE
    wf1.nCodecDelay := 0;//1393;                      // voodoo value #2
    wf1.wID := MPEGLAYER3_ID_MPEG;

    bufSource := GetMemory(wf1.nBlockSize);
    try
      mmr := acmStreamOpen(@has, 0, @wf1.wfx, @wf2, nil, nil, nil, 0);
      try
        if mmr <> 0 then
          raise Exception.Create('assert '+inttostr(mmr));
        mmr := acmStreamSize(has, MP3_BLOCK_SIZE, targetsize, ACM_STREAMSIZEF_SOURCE);
        bufTarget := GetMemory(targetsize);

        try
    //  ACMSTREAMHEADER mp3streamHead;



          FillMem(Pbyte(@ash), sizeof(ash), 0);                         //  ZeroMemory( &mp3streamHead, sizeof(ACMSTREAMHEADER ) );
          ash.cbStruct := sizeof(TACMSTREAMHEADER);              //  mp3streamHead.cbStruct = sizeof(ACMSTREAMHEADER );
          ash.pbSrc := bufSource;                                //  mp3streamHead.pbSrc = mp3buf;
          ash.pbDst := bufTarget;                                //  mp3streamHead.pbDst = rawbuf;
          ash.cbSrcLength := wf1.nBlockSize;                     //  mp3streamHead.cbSrcLength = MP3_BLOCK_SIZE;
          ash.cbDstLength := targetsize;                         //  mp3streamHead.cbDstLength = rawbufsize;
          mmr := acmStreamPrepareHeader(has, @ash, 0);            //  mmr = acmStreamPrepareHeader( g_mp3stream, &mp3streamHead, 0 );
          try
            fsIn := TStreamType.Create(sMp3File, fmOpenRead+fmShareDenyWrite);
            fsIn.BufferSize := 2000000;
            result := TMemoryStream.Create;
            result.Seek(0,0);
            try
              while fsIn.Position < fsIn.Size do begin
                iPos := result.Position;
                iToRead := lesserof(fsIn.Size-fsIn.Position, wf1.nBlockSize);
                //Debug.Log(inttostr(fsIn.position)+':'+inttostr(fsIn.Size));
                iRead := Stream_GuaranteeRead(fsIn, @bufSource[0], iToRead);
                ash.cbSrcLengthUsed := iRead;
                f := 0;
                if fsIn.Position = 0 then
                  f := ACM_STREAMCONVERTF_START;
                if (iRead < iToRead) or (fsIn.Position = fsIn.Size) then
                  f := f + ACM_STREAMCONVERTF_END;
                mmr := acmStreamConvert(has, ash,ACM_STREAMCONVERTF_BLOCKALIGN+ f);
                if ash.cbDstLengthUsed <> 0 then
                  Stream_GuaranteeWrite(result, bufTarget, ash.cbDstLengthUsed);


              end;
            finally
              fsIn.Free;
            end;

          finally
            acmStreamUnprepareHeader(has, @ash, 0);
          end;
        finally
          FreeMemory(bufTarget);
        end;
      finally
        mmr := acmStreamClose(has, 0);
      end;
    finally
      FreeMemory(bufSource);
    end;

    info := mp3info;
  except
    result.free;
    result := nil;
    raise;
  end;
end;
{$ENDIF}

end.
