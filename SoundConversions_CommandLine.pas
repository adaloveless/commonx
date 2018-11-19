unit SoundConversions_CommandLine;
{$Message '********************************Compiling SoundConversions_CommandLine'}

interface
{$IFDEF MSWINDOWS}

uses
  exe, typex, commandprocessor, sysutils, tools, systemx;

const
  FFMPEG_TIMEOUT = 90000;




procedure EndVideoToMp3(c: Tcmd_RunExe);
function BeginVideoToSafeVideo(sFile: string; sOutFile: string = ''): Tcmd_RunExe;
function BeginVideoToSafeVideoHD(sFile: string; sOutFile: string = ''): Tcmd_RunExe;
procedure EndVideoToSafeVideo(c: Tcmd_RunExe);
function BeginVideoToMp3(sFile: string; bUseAsyncFlag: boolean;
  sOutFile: string = ''): Tcmd_RunExe; overload;
function BeginVideoToMp3(sFile: string; sOutFile: string = ''): Tcmd_RunExe;
  overload;

procedure SoundStretch(iPitch: integer; iSourceTempo: nativefloat; iTargetTempo: nativefloat;
  sInFile: string; sOutFile: string);

function SoundStretch_Begin(iPitch: integer; iSourceTempo: nativefloat;
  iTargetTempo: nativefloat; sInFile: string; sOutFile: string): TBetterProcessInformation;
procedure SoundStretch_End(handle: TBetterProcessInformation);

function BeginWaveToMp3(sFile: string;sOutFile: string = ''; cDependsOn: TCommand = nil): Tcmd_RunExe;
procedure EndWaveToMp3(c: TCmd_RunExe);
function BeginMp3ToWave(sFile: string): Tcmd_RunExe;
procedure EndMp3ToWave(c: Tcmd_RunExe);
procedure Mp3ToWave(sFile: string);
procedure VideoToMp3(sFile: string);


{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  soundtools_easywindows, booger;
procedure VideoToMp3(sFile: string);
begin
  if not fileexists(changefileext(sFile, '.mp3')) then
    EndVideoToMp3(BeginVideoToMp3(sFile));
end;

function BeginVideoToMp3(sFile: string; sOutFile: string = ''): Tcmd_RunExe;
begin
  result := BeginVideoToMp3(sFile, true, sOutFile);
end;

function BeginVideoToMp3(sFile: string; bUseAsyncFlag: boolean;
  sOutFile: string = ''): Tcmd_RunExe;
var
  br: string;
begin
  if sOutFile = '' then
    sOutFile := changefileext(sFile, '.mp3');
  result := Tcmd_RunExe.Create;
  if lowercase(extractfileext(sFile)) = '.webm' then begin
    result.Prog := findtool('ffmpeg_bleeding_edge_jan.exe');
  end else begin
    result.Prog := findtool('ffmpeg.exe');
  end;
  result.timeout := FFMPEG_TIMEOUT;




  if lowercase(extractfileext(sFile)) = '.flv' then begin
    br := '128k -ar 44100'
  end else begin
    br := '256k'
  end;


  if bUseAsyncFlag then
    result.Params := '-y -vsync 1 -async 1 -ab '+br+' -i "' + sFile + '" "' + sOutFile + '"'
  else
    result.Params := '-y -vsync 1 -ab '+br+' -i "' + sFile + '" "' + sOutFile + '"';

  result.Hide := true;
  result.Start;
end;

procedure EndVideoToMp3(c: Tcmd_RunExe);
begin
  c.waitFor;
  c.free;
end;

function BeginVideoToSafeVideoHD(sFile: string;
  sOutFile: string = ''): Tcmd_RunExe;
begin
  if sOutFile = '' then
    sOutFile := changefileext(sFile, '.safe.mp4');
  result := Tcmd_RunExe.Create;
  if lowercase(extractfileext(sFile))='.webm' then begin
    result.Params := '-y -vsync 0 -i "' + sFile + '" -q:a 0 -q:v 0 -r 30 -ab 256k "' + sOutFile + '"';
    result.Prog := FindTool('ffmpeg_bleeding_edge_jan.exe');
    //result.ConsoleRedirect := false;

    result.Timeout := 15000;
  end else begin
    result.Params := '-y -vsync 1 -i "' + sFile + '" -q:a 0 -q:v 0 -r 30 -ab 256k "' + sOutFile + '"';
    result.Prog := FindTool('ffmpeg.exe');
    result.Timeout := FFMPEG_TIMEOUT;
  end;



  result.Hide := true;
  result.Start;
end;

function BeginVideoToSafeVideo(sFile: string;
  sOutFile: string = ''): Tcmd_RunExe;
begin
  if sOutFile = '' then
    sOutFile := changefileext(sFile, '.safe.mpeg');
  result := Tcmd_RunExe.Create;
  result.Prog := FindTool('ffmpeg.exe');
  result.Params := '-y -i "' + sFile + '" -sameq "' + sOutFile + '"';

  result.Hide := true;
  result.Timeout := FFMPEG_TIMEOUT;
  result.Start;
end;

procedure EndVideoToSafeVideo(c: Tcmd_RunExe);
begin
  if c= nil then
    exit;

  try
    c.waitFor;
  finally
    c.free;
  end;
end;

procedure SoundStretch(iPitch: integer; iSourceTempo: nativefloat; iTargetTempo: nativefloat;
  sInFile: string; sOutFile: string);
var
  sparams: string;
begin
  sparams := '"' + sInFile + '" "' + sOutFile + '" ' + floattostr(iSourceTempo)
    + ' ' + floattostr(iTargetTempo) + ' ' + floattostr(iPitch);

  RunProgramAndWait(Dllpath + 'BetterSoundStretch.exe', sparams, '', true,
    false);
end;

function SoundStretch_Begin(iPitch: integer; iSourceTempo: nativefloat;
  iTargetTempo: nativefloat; sInFile: string; sOutFile: string): TBetterProcessInformation;
var
  sparams: string;
begin
  sparams := '"' + sInFile + '" "' + sOutFile + '" ' + floattostr(iSourceTempo)
    + ' ' + floattostr(iTargetTempo) + ' ' + floattostr(iPitch);

  result := RunProgram(Dllpath + 'BetterSoundStretch.exe', sparams, '', true,
    false);
end;

procedure SoundStretch_End(handle: TBetterProcessInformation);
begin
  WaitForExe(handle);
end;

// ------------------------------------------------------------------------------
function BeginWaveToMp3(sFile: string;sOutFile: string = ''; cDependsOn: TCommand = nil): Tcmd_RunExe;
var
  sTarget: string;
begin
  if sOutFile = '' then
    sTarget := changefileext(sFile, '.mp3')
  else
    sTarget := sOutFile;

{$IFDEF USE_LAME_TO_DECODE_MP3s}
  result := Tcmd_RunExe.Create;
  result.Prog := FindTool('lame.exe');
  result.Params := '--quiet "' + sFile + '" "' + sTarget + '" --encode';
  result.Hide := true;
// result.batchwrap := true;
  result.ConsoleRedirect := true;
  if assigned(cDependsOn) then
    result.addDependency(cDependsOn);
  result.Start;
{$ELSE}
  result := Tcmd_RunExe.Create;
  result.Prog := FindTool('ffmpeg.exe');
  if fileexists(sTarget) then
    deletefile(sTarget);
  result.Params := '-y -i "' + sFile + '"  -ab 256 "' + sTarget + '"';
  result.Hide := true;
// result.batchwrap := true;
  result.ConsoleRedirect := true;
  result.CPUExpense := 1.0;
  if assigned(cDependsOn) then
    result.addDependency(cDependsOn);
  result.Start;


{$ENDIF}
end;

procedure EndWaveToMp3(c: TCmd_RunExe);
begin
  try
    c.waitFor;
  finally
    c.free;
  end;
end;


function BeginMp3ToWave(sFile: string): Tcmd_RunExe;
var
  sTarget: string;
begin
  sTarget := changefileext(sFile, '.wav');
{$IFDEF USE_LAME_TO_DECODE_MP3s}
  result := Tcmd_RunExe.Create;
  result.Prog := FindTool('lame.exe');
  result.Params := '--mp3input --quiet "' + sFile + '" "' + sTarget + '" --decode';
  result.Hide := true;
// result.batchwrap := true;
  result.ConsoleRedirect := true;
  result.Start;
{$ELSE}
  result := Tcmd_RunExe.Create;
  result.Prog := FindTool('ffmpeg.exe');
  if fileexists(sTarget) then
    deletefile(sTarget);
  result.Params := '-y -i "' + sFile + '" "' + sTarget + '"';
  result.Hide := true;
// result.batchwrap := true;
  result.ConsoleRedirect := true;
  result.Start;
{$ENDIF}

end;

// ------------------------------------------------------------------------------
procedure EndMp3ToWave(c: Tcmd_RunExe);
begin
  try
    c.waitFor;
  finally
    c.free;

  end;
end;

// ------------------------------------------------------------------------------
procedure Mp3ToWave(sFile: string);
var
  c: Tcmd_RunExe;
begin
  c := BeginMp3ToWave(sFile);
  EndMp3ToWave(c);

end;



{$ENDIF MSWINDOWS}





end.
