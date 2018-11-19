unit booger;

interface

uses
  debug, exe, numbers, winapi.mmsystem,
  managedthread, classes, typex, systemx,
  soundinterfaces, commandprocessor, soundtools,
  sysutils, helpers.stream, backgroundthreads,
  geometry, stringx, commandicons,
  soundtools_easywindows;

procedure PlayBooger(pRoot: pointer); overload;
procedure PlayAda3(pRoot: pointer); overload;
procedure PlayBooger(sFile: string); overload;
procedure WriteHeader(msOut: TStream; chans, samplerate: ni; iLen: int64);
procedure ChangeLength(s: TStream; iLenInSamples: int64);










implementation


uses
  soundconversion_windows;


procedure PlayAda3(pRoot: pointer);
type
  iptr = ^integer;
  bptr = PAnsiChar;
  bbptr = PByte;
  siptr = ^smallint;
var
  l: cardinal;
  lEnd: cardinal;
  iChans: smallint;
  iSAmpleRatE: cardinal;
begin
  l := iptr(pRoot)^;
  if (bbptr(pRoot) + 0)^ <> ord('A') then
    exit;
  if (bbptr(pRoot) + 1)^ <> ord('D') then
    exit;
  if (bbptr(pRoot) + 2)^ <> ord('A') then
    exit;
  if (bbptr(pRoot) + 3)^ <> ord('3') then
    exit;

  iSAmpleRate := iptr(bptr(pRoot) + SOUND_IDX_SAMPLE_RATE)^;
  lEnd := iptr(bptr(pRoot) + SOUND_IDX_LENGTH)^;
  iChans := siptr(bptr(pRoot) + SOUND_IDX_CHANS)^;

  if lEnd = $FFFFFFFF then
    lEnd := l;

  if iChans = 1 then
    PlayRawSoundMono(PAnsiChar(pRoot) + SOUND_HEADER_LENGTH_v3, lEnd, iSampleRate)
  else
    PlayRawSoundStereo(PAnsiChar(pRoot) + SOUND_HEADER_LENGTH_v3, lEnd, iSampleRate)
end;

procedure PlayBooger(pRoot: pointer);
type
  iptr = ^integer;
  bptr = PAnsiChar;
  bbptr = PByte;
  siptr = ^smallint;
var
  l: cardinal;
  lEnd: cardinal;
  iChans: smallint;
  iSAmpleRatE: cardinal;
begin
  l := iptr(pRoot)^;
  if (bbptr(pRoot) + 0)^ <> ord('B') then begin
    PLayAda3(pRoot);//<<<---------DEFER
    exit;
  end;
  if (bbptr(pRoot) + 1)^ <> ord('O') then
    exit;
  if (bbptr(pRoot) + 2)^ <> ord('O') then
    exit;
  if (bbptr(pRoot) + 3)^ <> ord('G') then
    exit;

  iSAmpleRate := 44100;
  lEnd := iptr(bptr(pRoot) + SOUND_IDX_LENGTH)^;
  iChans := siptr(bptr(pRoot) + SOUND_IDX_CHANS)^;

  if lEnd = $FFFFFFFF then
    lEnd := l;

  if iChans = 1 then
    PlayRawSoundMono(PAnsiChar(pRoot) + SOUND_HEADER_LENGTH_v3, lEnd, iSampleRate)
  else
    PlayRawSoundStereo(PAnsiChar(pRoot) + SOUND_HEADER_LENGTH_v3, lEnd, iSampleRate)
end;


procedure PlayBooger(sFile: string);
var
  ms: TMemoryStream;
  l: cardinal;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(sFile);
    if ms.Memory <> nil then
      PlayBooger(ms.Memory);

  finally
    ms.free;
  end;

end;

procedure WriteHeader(msOut: TStream; chans, samplerate: ni; iLen: int64);
var
  shit, iLen32: int64;
  ver: byte;
  c1,c2,c3,c4: ansichar;
  samplerate32: cardinal;
  bh: TBoogerHeader;
begin
  samplerate32 := samplerate;
  bh.init;
  bh.channels := chans;
  bh.samplerate := samplerate;
  bh.length := iLen;
  msOut.Seek(0,soBeginning);
  bh.WriteToStream(msOut);
  stream_GuaranteeWrite(msOut, pbyte(@bh), sizeof(bh));



end;

procedure ChangeLength(s: TStream; iLenInSamples: int64);
var
  iPOs: int64;
  bh: TBoogerHeader;
begin
  iPos := s.Position;
  bh.REadFromStream(s);
  bh.length := iLenInSamples;
  bh.WriteToStream(s);
  s.seek(iPos, soBeginning);


end;





{ TBoogerHeader }


end.
