Unit Wavein;
{Copyright 2002, Gary Darby, Intellitech Systems Inc., www.DelphiForFun.org

 This program may be used or modified for any non-commercial purpose
 so long as this original notice remains in place.
 All other rights are reserved
 }

{Based on a TWaveRecorder class developed by John Mertus in 1996 and found at
  http://www.gnomehome.demon.nl/uddf/pages/sound.htm  }

{*************************************************************************}
Interface

Uses Windows, MMSystem, SysUtils;

{  The following defines a class TWavein for sound card input.  }
{  User provides a message handling procedure to be called whenever
   a buffer of data is full.                                    }

Const MAX_BUFFERS = 8;

type
  {Event definitions}
  TBufferFullEvent = procedure (Sender : TObject; Data : Pointer; Size : longint) of object;
  TErrorEvent=procedure(s:string) of object; {OnError}

  TWaveIn = class(TObject)
      Constructor Create(newFormHandle:HWnd; BfSize, newTotalBuffers : Integer);
      Destructor  Destroy;      Override;

    private
    fBufferSize:Integer; // Requested size of buffer
    BufIndex:Integer;

    pWaveHeader:Array [0..MAX_BUFFERS-1] of PWAVEHDR;
    dwByteDataSize:DWORD;
    dwTotalWaveSize:DWORD;

    bDeviceOpen:Boolean;
    FormHandle:HWND;

    Function  InitWaveHeaders : Boolean;
    Function  AllocPCMBuffers : Boolean;
    Procedure FreePCMBuffers;

    Function  AllocWaveFormatEx : Boolean;
    Procedure FreeWaveFormatEx;

    Function  AllocWaveHeaders : Boolean;
    Procedure FreeWaveHeader;


    Procedure CloseWaveDevice;
    procedure fposterror(s:string);

    public { Public declarations }
      RecordActive:Boolean;  {we are recording}
      ptrWaveFmtEx:PWaveFormatEx;
      WaveBufSize:Integer;    // Size aligned to nBlockAlign Field
      IniTWavein:Boolean;
      RecErrorMessage:String;
      QueuedBuffers,ProcessedBuffers:Integer;
      pWaveBuffer:Array [0..MAX_BUFFERS-1] of pointer {lpstr}{gdd};
      WaveIn:HWAVEIN;  { Wavedevice handle }
      {OnBufferFull:TBufferFullEvent;}
      OnError:TErrorEvent;
      TotalBuffers:Integer;
      Function  AddNextBuffer : Boolean;
      Procedure StopInput;
      Function  StartInput:Boolean;
      Function  SetupInput:Boolean;
  end;

implementation

{*************** GetErrorText *********}
Function GetErrorText(iErr : Integer) : String;
{Put the WaveIn error messages in a string format. }
{ iErr is the error number }
Var
  PlayInErrorMsg:Array [0..255] of Char;
Begin
  waveInGetErrorText(iErr,PlayInErrorMsg,255);
  result := StrPas(PlayInErrorMsg);
End;

{*************** InitWaveHeaders **********************}
Function TWavein.AllocWaveFormatEx : Boolean;
{ Allocate the largest format size required from installed ACM's         }
begin
  new(ptrWavefmtEx);
  If (ptrWaveFmtEx = Nil) Then
  begin
    fposterror('Error locking WaveFormatEx memory');
    AllocWaveFormatEx := False;
    Exit;
  end;
  { initialize the format to standard PCM }
  {ZeroMemory( lptrwavefmtex, maxFmtSize );}
  with ptrwavefmtex^ do
  begin
    wFormatTag := WAVE_FORMAT_PCM;
    nChannels := 1;
    nSamplesPerSec := 44100;
    nBlockAlign := 1;
    wBitsPerSample := 8;
    nAvgBytesPerSec := nSamplesPerSec*(wBitsPerSample div 8)*nChannels;
    cbSize := 0;
  end;
  { Success, go home }
  AllocWaveFormatEx := True;
end;

{******************* InitWaveHeaders *******************}
function TWavein.InitWaveHeaders : Boolean;
{Allocate memory, zero out wave headers and initialize                 }
var  i:Integer;
begin
  { make the wave buffer size a multiple of the block align... }
  WaveBufSize := fBufferSize - (fBufferSize mod ptrwavefmtex.nBlockAlign);
  {Set the wave headers }
  for i := 0 to TotalBuffers-1 Do
  with pWaveHeader[i]^ Do
  begin
    lpData := pWaveBuffer[i];      {address of the waveform buffer}
    dwBufferLength := WaveBufSize; {length, in bytes, of the buffer}
    dwBytesRecorded := 0;
    dwUser := 0;                   {32 bits of user data}
    dwFlags := 0;
    dwLoops := 0;
    lpNext := Nil;
    reserved := 0;
  end;
  result:=TRUE;
end;


{***************** AllocWaveHeader ******************}
Function TWavein.AllocWaveHeaders : Boolean;
{Allocate and lock header memory                                       }
var i : Integer;
begin
  for i := 0 to TotalBuffers-1 Do
  begin
    new(pwaveheader[i]);
    If (pwaveheader[i] = Nil ) Then
    begin
      { NOTE: This could lead to a memory leak, fix someday }
      fposterror('Could not lock header memory for recording');
      AllocWaveHeaders := FALSE;
      Exit;
    end;
  end;
  AllocWaveHeaders := TRUE;
end;

{************ FreeWaveHeader ******************}
Procedure TWavein.FreeWaveHeader;
{Free up the memory AllocWaveHeaders allocated}
var i:Integer;
begin
  For i := 0 to TotalBuffers-1 Do
  begin
    dispose(pwaveheader[i]);
    pwaveheader[i]:=nil;
  end;
end;

{*************** AllocPCMBuffers *************}
function TWavein.AllocPCMBuffers : Boolean;
{Allocate and lock the waveform memory}
var i:Integer;
begin
  for i:=0 to TotalBuffers-1 Do
  begin
    getmem(pWaveBuffer[i],fBufferSize);
    If (pWaveBuffer[i] = Nil) Then
    begin
      { Possible Memory Leak here }
      fposterror('Error Locking wave buffer memory');
      AllocPCMBuffers := False;
      Exit;
    end;
    pWaveHeader[i].lpData := pWaveBuffer[i];
  end;
  AllocPCMBuffers := TRUE;
end;

{***************** FreePCMBuffers ************}
Procedure TWavein.FreePCMBuffers;
{Free up the memory AllocPCMBuffers used.                              }
var  i : Integer;
begin
  for i := 0 to TotalBuffers-1 do   freemem(pwavebuffer[i]);
end;

{**************** FreeWaveFormatEx ************}
   Procedure TWavein.FreeWaveFormatEx;
{ Free up the ExFormat headers                               }
begin
  If (ptrWaveFmtEx = Nil) Then Exit;
  dispose(ptrWavefmtEx);
  ptrWaveFmtEx := Nil;
end;

{************* TWavein.Create ******************}
   Constructor TWavein.Create(NewFormHandle:HWnd;
                                   BFSize, newTotalBuffers : Integer);

{  Set up the wave headers, initializes the data pointers and
   allocate the sampling buffers
   BFSize is the size of the buffer in BYTES
}
Var
  i : Integer;
begin
  Inherited Create;
  formhandle:=newformhandle;
  for i := 0 to newTotalBuffers-1 Do     {gdd}
  begin
    pWaveBuffer[i] := Nil;
    ptrWaveFmtEx := Nil;
  end;
  fBufferSize := BFSize;
  totalbuffers:=newtotalbuffers;
  {allocate memory for wave format structure }
  if(Not AllocWaveFormatEx) Then
  begin
    IniTWavein := FALSE;
    exit;
  end;
  {find a device compatible with the available wave characteristics }
  if (waveInGetNumDevs < 1 ) Then
  begin
    fposterror('No wave audio recording devices found');
    IniTWavein := FALSE;
    exit;
  end;
  {allocate the wave header memory }
  if (Not AllocWaveHeaders) Then
  begin
    IniTWavein := FALSE;
    exit;
  end;
  {allocate the wave data buffer memory }
  if (Not AllocPCMBuffers)  Then
  begin
    IniTWavein := FALSE;
    exit;
  end;
  InitWaveIn := TRUE;
end;

{***************** Destroy ***************}
destructor TWavein.Destroy;
{Free up memory allocated by IniTWavein }
begin
  FreeWaveFormatEx;
  FreePCMBuffers;
  FreeWaveHeader;
  inherited Destroy;
end;

{************* CloseWaveDevice  *****************}

Procedure TWavein.CloseWaveDevice;
{ Close the waveform device}
var
  i, ierr : Integer;
  s:string;
begin
  { if the device is already closed, just return }
  if (Not bDeviceOpen) Then Exit;
  {unprepare the headers }
  for i := 0 to TotalBuffers-1 Do
  begin
    ierr:=waveInUnprepareHeader(WaveIn, pWaveHeader[i], sizeof(TWAVEHDR));
    if ierr<> 0 then
    begin
      case ierr of
        MMSYSERR_INVALHANDLE:s:=' Specified device handle is invalid ';
        MMSYSERR_NODRIVER:s:='	No device driver is present';
        MMSYSERR_NOMEM:s:=' Unable to allocate or lock memory';
        WAVERR_STILLPLAYING:s:='Buffer still being propcessed';
        else s:='Uknown Error';
      end;
      fposterror('Error in waveInUnprepareHeader ' + s);
    end;
  end;
  { save the total size recorded and update the display }
  dwTotalwavesize := dwBytedatasize;
  { close the wave input device }
  if (waveInClose(WaveIn) <> 0) Then
  fposterror('Error closing input device');
  {tell this function we are now closed }
  bDeviceOpen := FALSE;
end;

{************* StopInput *********************}
Procedure TWavein.StopInput;
{Stop recording and close the device}
var  iErr : Integer;
begin
  If (Not bDeviceOpen) Then Exit;
  RecordActive := False;
  {ierr:=waveinstop(wavein);}
  ierr := waveinreset(wavein);{hangs if callback procedure is used}
  { stop recording and return queued buffers }
  if (iErr <> 0) then fposterror('Error in waveInReset');
  CloseWaveDevice;
end;

{******************* AddNextBuffer *************}
Function TWavein.AddNextBuffer : Boolean;
{ Add a buffer to the input queue and toggles buffer index.       }
Var iErr : Integer;

begin
  result:=true;
  if not recordactive then exit; {can't add if recording has been stopped}
  { queue the buffer for input }
  iErr := waveInAddBuffer(WaveIn, pwaveheader[bufindex], sizeof(TWAVEHDR));
  If (iErr <> 0) Then
  begin
    StopInput;
    fposterror('Error adding buffer, recording stopped: ' + GetErrorText(iErr));
    result:=FALSE;
    exit;
  end;

  { toggle for next buffer }
  bufindex := (bufindex+1) mod TotalBuffers;
  QueuedBuffers := QueuedBuffers + 1;
  result:=TRUE;
end;

(*  Replaced with calling form message handler
{******************  BufferDoneCallBack *********** }
Procedure BufferDoneCallBack(
                 hW    : HWAVE;      {handle of waveform device}
                 uMsg  : DWORD;      {sent message}
                 dwInstance : DWORD; {instance data}
                 dwParam1 : DWORD;   {application-defined parameter}
                 dwParam2 : DWORD    {application-defined parameter}
                 );  stdcall;

{Called each time the wave device has info, e.g. fills a buffer}
Var BaseRecorder : TWavein;
begin
  BaseRecorder := TWavein(DwInstance);
  try
    With BaseRecorder Do
    begin
      If umsg=MM_WIM_DATA then
      begin
        If (RecordActive) then
        begin
          {
          if (assigned(OnBufferFull))
          then OnBufferfull(pWaveBuffer[ProcessedBuffers Mod TotalBuffers],
                                                        WaveBufSize);
           }
          BaseRecorder.AddNextBuffer;
          inc(ProcessedBuffers);
        end;
      end
      else if umsg=MM_WIM_CLOSE
      then fposterror('Closing');
    end;
  except  baserecorder.StopInput;
  end;
end;
*)


{*****************  StartInput *****************}
Function TWaveIn.StartInput : Boolean;
{Start recording}
var   iErr, i : Integer;
begin
  result:=false;
  { start recording to first buffer }
  iErr := WaveInStart(WaveIn);
  If (iErr <> 0) Then
  begin
    CloseWaveDevice;
    fposterror('Error starting wavein, close attempted: ' + GetErrorText(iErr));
  end
  else
  begin
    RecordActive := TRUE;
    { queue the next buffers }
    For i := 1 to TotalBuffers-1 Do
    If (Not AddNextBuffer) Then
    begin
      fposterror('AddNextBuffer failed from open');
      exit;
    end;
    result:=True;
  end;
end;

{*************** SetupInput  *******************}
Function TWavein.SetupInput:Boolean;
{open the device}
var  iErr, i : Integer;
begin
  if recordactive then StopInput;
  if bdeviceopen then closewavedevice;  {already open - close it first}
  dwTotalwavesize := 0;
  dwBytedatasize := 0;
  bufindex := 0;
  ProcessedBuffers := 0;
  QueuedBuffers := 0;
  { open the device for recording }
  (*iErr := waveInOpen(@WaveIn, WAVE_MAPPER, ptrWaveFmtEx,
          Integer(@BufferDoneCallBack),
                 Integer(self), CALLBACK_FUNCTION {+ WAVE_ALLOWSYNC} );
  *)
  iErr := waveInOpen(@WaveIn, WAVE_MAPPER, ptrWaveFmtEx,
                      formhandle, 0, CALLBACK_WINDOW);
  if (iErr <> 0) then
  begin
    fposterror('Could not open the input device for recording: ' + ^M
                   +' '+ GetErrorText(iErr));
    result:=FALSE;
    exit;
  end;
  { tell CloseWaveDeviceRecord() that the device is open }
  bDeviceOpen := TRUE;
  { prepare the headers }
  InitWaveHeaders;
  For i := 0 to TotalBuffers-1 Do
  begin
    iErr := waveInPrepareHeader( WaveIn, pWaveHeader[I], sizeof(TWAVEHDR));
    If (iErr <> 0) Then
    begin
      CloseWaveDevice;
      fposterror('Error preparing header for recording: ' + ^M +
                          GetErrorText(iErr));
      result:=FALSE;
      Exit;
    end;
  end;
  {add the first buffer }
  If (Not AddNextBuffer) then result:=FALSE
  else result:=TRUE;
end;

{**************** fposterror ***********}
procedure TWavein.fposterror(s:string);
begin
  RecErrorMessage:=s;
  if assigned(OnError) then Onerror(s);
end;

end.

