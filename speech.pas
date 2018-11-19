unit Speech;

interface

uses commandprocessor, winapi.activex, soundtools, https, httpclient, winapi.MMSystem, sysutils, stringx, typex, winapi.windows, beeper, sharedobject,
  backgroundcommandprocessor, speechlib_TLB,orderlyinit, systemx, booger, soundconversion_windows;

{$DEFINE SPEECHFALLBACK}
{x$DEFINE NOSPEECH}

type
  TTextToSpeechEngine = class (TLockQueuedObject)
  protected
  public
    procedure Say(sString: string; bWait: boolean = true; sVoice: string = '');
  end;

procedure Say(sString: string; bWait: boolean = false; sVoice: string = 'ATT DTNV 1.4 Crystal16');


function GetNaturalString(sString: string): string;


procedure SayNatural(sString: string; bWait: boolean = false; sVoice: string = ''; bCached: boolean = true);
procedure SayNaturalBlocking(sString: string; bWait: boolean = false; sVoice: string = ''; bCached: boolean = true);
procedure SayNaturalOld(sString: string; sVoice: string = '');
procedure SayNaturalCached(sString: string; bWait: boolean = false; sVoice: string = '');

(*  <SELECT NAME="voice" SIZE="1" CLASS="mono">
    <OPTION CLASS="mono"  VALUE="crystal">Crystal ... US English</OPTION>
    <OPTION CLASS="mono"     VALUE="mike">Mike ...... US English</OPTION>
    <OPTION CLASS="mono"     VALUE="rich">Rich ...... US English</OPTION>
    <OPTION CLASS="mono"   VALUE="lauren">Lauren .... US English</OPTION>
    <OPTION CLASS="mono"   VALUE="claire">Claire .... US English</OPTION>
    <OPTION CLASS="mono"     VALUE="rosa">Rosa ...... Latin Am. Spanish</OPTION>
    <OPTION CLASS="mono"  VALUE="alberto">Alberto ... Latin Am. Spanish</OPTION>
    <OPTION CLASS="mono"    VALUE="klara">Klara ..... German</OPTION>
    <OPTION CLASS="mono"   VALUE="reiner">Reiner .... German</OPTION>
    <OPTION CLASS="mono"    VALUE="alain">Alain ..... French</OPTION>
    <OPTION CLASS="mono" VALUE="juliette">Juliette .. French</OPTION>
    <OPTION CLASS="mono"  VALUE="charles">Charles ... UK English</OPTION>
    <OPTION CLASS="mono"   VALUE="audrey">Audrey .... UK English</OPTION>
  </SELECT>
*)

type
  Tcmd_SayNatural = class (TCommand)
  private
    FText: string;
  public
    procedure InitExpense;override;
    procedure DoExecute;override;
    property Text: string read FText write FText;
  end;

var
  SpeechError: boolean;


function FileNameForString(sString: string): string;

function SelectRandomVoice: string;

var
  sectSpeech: TCLXCriticalSection;
  TTS: TTextToSpeechEngine;

procedure LockSpeech;inline;
procedure UnlockSpeech;inline;
implementation

uses WebString;
procedure LockSpeech;inline;
begin
  EnterCriticalSection(sectSpeech);
end;
procedure UnlockSpeech;inline;
begin
  LeaveCriticalSection(sectSpeech);
end;

procedure Say(sString: string; bWait: boolean = false; sVoice: string = 'ATT DTNV 1.4 Crystal16');
begin
{$IFDEF NOSPEECH}
  exit;
{$ENDIF}
  try
    if not SpeechError then
      TTS.Say(sstring, bWait, sVoice);
  except
    SpeechError := true;
  end;
end;


procedure SayNatural(sString: string; bWait: boolean = false; sVoice: string = ''; bCached: boolean = true);
var
  cmd: Tcmd_SayNatural;
begin
{$IFDEF SPEECHFALLBACK}
  Say(GetNaturalString(sString), bWait);
  exit;
{$ENDIF}

  if bWait then begin
    while BGCmd.HasCommand(Tcmd_SayNatural) do
      sleep(10);
  end else begin
    if BGCmd.HasCommand(Tcmd_SayNatural) then
      exit;
  end;

  cmd := Tcmd_SayNatural.create;
  cmd.Text := sString;
  cmd.Process;
end;

procedure SayNaturalBlocking(sString: string; bWait: boolean = false; sVoice: string = ''; bCached: boolean = true);
begin
  if sString = '' then
    exit;

{$IFDEF SPEECHFALLBACK}
  Say(sString);
  exit;
{$ENDIF}

  sString := GetNaturalString(sString);

//  Say(sString);
//  exit;

  if bCached then
    SayNaturalCached(sString, bWait, sVoice)
  else
    SayNaturalOld(sString, sVoice);



end;

procedure SayNaturalOld(sString: string; sVoice: string);
var
  htp: THTTPCLient;
  s: string;
begin
  if svoice = '' then
    svoice := SelectRandomVoice;
  try
    //post
    htp := THTTPClient.create;
    if fileexists('say.wav') then
      deletefile('say.wav');

    try
      htp.AddPostParam('txt', sString);
      htp.AddPostParam('voice', sVoice);
      htp.AddPostParam('speakButton', 'SPEAK');
  //    htp.AddPostParam('downloadButon', 'DOWNLOAD');


      if htp.Post('http://www2.research.att.com/~ttsweb/tts/demo.php') then begin
        s := FindLinkInHTML('http://hegel.research.att.com/', htp.InBody,  'here');
        if htp.Get(s)
        then begin
          htp.SaveToFile('say.wav');
          s := slash(extractfilepath(DLLName))+'say.wav';

          PlaySound(PChar(s), 0, SND_SYNC+SND_NODEFAULT+SND_FILENAME);
        end;
      end;
    finally
      htp.free;

      if fileexists('say.wav') then
        deletefile('say.wav');

    end;
  except
  end;

end;


procedure SayNaturalCached(sString: string; bWait: boolean; sVoice: string);
var
  htp: THTTPCLient;
  s: string;
  sFile: string;
  iFlags: integer;
  sURL: string;
begin
  if svoice = '' then
    svoice := SelectRandomVoice;
  try
    //post
    htp := THTTPClient.create;


    sFile := extractfilepath(DLLName)+'voices\'+sVoice+'\'+FileNameForstring(sString);

    try
      htp.AddPostParam('txt', sString+'...');
      htp.AddPostParam('voice', sVoice);
      htp.AddPostParam('speakButton', 'SPEAK');
  //    htp.AddPostParam('downloadButon', 'DOWNLOAD');




      if (not fileexists(sfile+'.boog')) then begin
        if (not fileexists(sfile)) then begin
          //sOLD URL = 'http://hegel.research.att.com/'
          if htp.Post('http://192.20.225.36/tts/cgi-bin/nph-talk') then begin
            s := FindLinkInHTML('http://192.20.225.36/', htp.InBody,  'here');
            if s ='' then exit;
            LockSpeech;
            try
              if htp.Get(s)
              then begin
                forceDirectories(extractfilepath(sFile));
                htp.SaveToFile(sFile);

              end;
            finally
              UnlockSpeech;
            end;
          end else begin
            raise Exception.create('Unable to download speech');
          end;
        end;

        WaveToBooger(sFile, sfile+'.boog');
        //SoundTools.TruncateAtSilenceInBoogerFile(sfile+'.boog');
        try
          DeleteFile(PChar(sFile));
        except
        end;

      end;

      if bWait then
        iFlags := SND_SYNC+SND_NODEFAULT+SND_FILENAME
      else
        iFlags := SND_ASYNC+SND_NODEFAULT+SND_FILENAME;



      Playbooger(sfile+'.boog');
//      PlaySound(char(sFile), 0, iFlags);
    finally
      htp.free;
    end;
  except
    raise;
    SayNaturalOld(sString, svoice);
  end;

end;





function SelectRandomVoice: string;
var
  x: integer;
begin
  Randomize;
  x := random(8);
  case x of
    1: result :=  'crystal';
    2: result :=   'mike';
    3: result :=   'rich';
    4: result :=  'lauren';
    5: result := 'claire';
    6: result := 'charles';
    7: result := 'anjali';
  else
    result := 'audrey';
  end;

end;


function FileNameForString(sString: string): string;
begin
  result := sString;

  result := stringreplace(result, ' ','_', [rfReplaceall]);
  result := stringreplace(result, '/','_', [rfReplaceall]);
  result := stringreplace(result, '\','_', [rfReplaceall]);
  result := stringreplace(result, '?','_', [rfReplaceall]);
  result := stringreplace(result, '!','_', [rfReplaceall]);
  result := stringreplace(result, '.','_', [rfReplaceall]);
  result := stringreplace(result, '$','_', [rfReplaceall]);
  result := stringreplace(result, '!','_', [rfReplaceall]);
  result := stringreplace(result, ':','_', [rfReplaceall]);
  result := stringreplace(result, '"','_', [rfReplaceall]);
  result := stringreplace(result, '''','_', [rfReplaceall]);
  result := stringreplace(result, ',','_', [rfReplaceall]);
  result := stringreplace(result, #13,' ', [rfReplaceall]);
  result := stringreplace(result, #10,' ', [rfReplaceall]);
  result := TRim(result);

  result := result + '.wav';




end;


function GetNaturalString(sString: string): string;
//add a space between TWO successive capital letters
//if a '.' is followed by an alpha numeric character
//  then pronounce 'dot'
var
  t, u: integer;
  bFoundAlpha: boolean;
  c, cNext: char;
  sTemp, sLeft, sRight: string;
begin
  if length(sString) < 3 then begin
    result := sString;
    exit;
  end;

  cNext := #0;

  result := '';
  for t := 1 to length(sString)-1 do begin
    c := sString[t];
    cNext := sString[t+1];

    if charinset(c,[':']) then begin
      result := result;
    end else
    if charinset(c , ['A'..'Z','a'..'z']) and charinset(cNext , ['A'..'Z']) then begin
//      if c ='A' then
//        result := result + 'Aee'+' '
//      else
        result := result + c;
    end else
    if charinset(c , ['.']) then begin
      sTemp := copy(sString, t, 999999);
      SplitString(sTemp, ' ', sLeft, sRight);
      bFoundAlpha := false;
      for u:= 1 to length(sLeft) do begin
        if charinset(sLeft[u], ['A'..'Z', 'a'..'z']) then begin
          bFoundAlpha := true;
        end;
      end;

      if bFoundAlpha then
         result := result+' dot '
      else
       result := result+'.';
    end
    else
      result := result+c;

  end;

  result := result+cNext;


end;

{ TTextToSpeechEngine }

procedure TTextToSpeechEngine.Say(sString: string; bWait: boolean = true; sVoice: string = '');
var
  sp: TspVoice;
const
  test =  '<EMPH>Hello</EMPH><PRON SYM="f eh l ow">Fellow</PRON>developers. I can '+
          'speak<PITCH MIDDLE="+10">in a high pitch like this.</PITCH>and<PITCH '+
          'MIDDLE="-10">or a low one that sounds like this.</PITCH>I can speak<RATE '+
          'SPEED="+5">very very quickly like a chipmunk</RATE>and <RATE SPEED="-10">or very '+
          'very slowly.</RATE>I can speak <VOLUME LEVEL="30">quietly if you like</VOLUME> or '+
          '<VOLUME LEVEL="100">loudly.</VOLUME>I can spell out this word. '+
          '<SPELL>ExceleTel</SPELL><silence msec="500"/><VOICE '+
          'REQUIRED="Gender=Female;Age!=Child">and even talk '+
          'like a little girl</VOICE><VOICE REQUIRED="Gender=Male">or like an adult '+
          'male</VOICE>and much much more<silence msec="500"/>Good luck with your ExceleTel '+
          'TeleTools experience. You can start by pressing some digits when you are connected on a '+
          'call. Enjoy!';
begin
{$IFDEF NOSPEECH}
  exit;
{$ENDIF}
  if sString = '' then

  Coinitialize(nil);
  sp := TSpVoice.Create(nil);
  try

    sp.Speak(sString, 0);
//    GLOG.debug('Say: '+sString,'speech');
    OutputDebugString(pchar('Say: '+sString));


  finally
    sp.Free;
  end;

//  speechlib_tlb.

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


{ Tcmd_SayNatural }

procedure Tcmd_SayNatural.DoExecute;
begin
  inherited;
{$IFDEF SPEECHFALLBACK}
//  Say(Text, true);
{$ELSE}
  SayNaturalBlocking(Text, true);
{$ENDIF}

end;

procedure Tcmd_SayNatural.InitExpense;
begin
  FireForget := true;
  CPUExpense := 0;
end;

procedure oinit;
begin
SpeechError := false;
InitializeCriticalSection(sectSpeech);
TTS := TTextToSpeechEngine.create;

end;

procedure ofinal;
begin
BGCmd.WaitForAll;
DeleteCriticalSection(sectSpeech);
TTS.free;


end;

initialization
  init.RegisterProcs('Speech', oinit, ofinal);

finalization
end.
