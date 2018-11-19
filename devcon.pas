unit devcon;

interface

uses
  typex, exe, betterobject, tools, classes, stringx, CommandProcessor, systemx;

const
  DEFAULT_DEVICE_SEARCH = '@"USB\VID_29AA*"';
  MIDI_DEVICE_SEARCH =  '@"SWD\MMDEVAPI\MIDII*"';

type
  TDevCon = class(TBetterObject)
  private
    FProg: TNotifyEvent;
  protected
    capturedData: string;
    procedure capturehook(ccStatus: TConsoleCaptureStatus;sBufferData: string);

  public
    Step, StepCount: ni;
    procedure CleanupDrivers(iLeaveAtLeast: nativeint; sLEaveMatch: string; sMatch: string = DEFAULT_DEVICE_SEARCH);
    function GetDriverList(sMatch: string = DEFAULT_DEVICE_SEARCH): string;
    property ProgressCallback: TNotifyEvent read FProg write FProg;
  end;


  Tcmd_CleanupDrivers = class(TCommand)
  private
    FLEave: ni;
    FMatch: string;
    FLeaveMatch: string;
    procedure prog(sender: TObject);
  public
    procedure Init;override;
    procedure InitExpense;override;
    procedure DoExecute;override;
    property Match: string read FMatch write FMatch;
    property Leave: ni read FLEave write FLeave;
    property LeaveMatch: string read FLeaveMatch write FLeaveMatch;

  end;




implementation







{ TDevCon }

procedure TDevCon.capturehook(ccStatus: TConsoleCaptureStatus;
  sBufferData: string);
begin
  if ccStatus = ccProgress then
    captureddata := captureddata+sBufferData;
end;

procedure TDevCon.CleanupDrivers(iLeaveAtLeast: nativeint; sLEaveMatch,
  sMatch: string);
var
  t: ni;
  iIgnored: ni;
  sl: TStringLIst;
  s,s2,s3: string;
  bDoneIgnoring, bMatch: boolean;
begin
  iIgnored := 0;
  sl := TStringlist.create;
  try
    StepCount := 1;
    Step := 0;
    sl.text := getDriverList(sMatch);

    StepCount := sl.Count-1;
    for t:= 0 to sl.count-1 do begin
      Step := t;
      if assigned(ProgressCallback) then
        ProgressCallback(self);
      s := sl[t];
      bDoneIgnoring := (iIgnored >= iLeaveAtLeast);
      bMatch := (zpos(sLEaveMatch, s, 0) <> 0);
      if bMatch then begin
        if not bDoneIgnoring then begin
          inc(iIgnored);
        end else begin
          if splitstring(s,':', s2,s3) then begin
            if SplitString(s2, ' ', s2,s3) then begin
              exe.RunProgramAndWait(FindTool('devcon.exe'), 'remove @"'+s2+'"', '', true, false, true );
            end;
          end;
        end;
      end;
    end;
  finally
    sl.free;
  end;

end;

function TDevCon.GetDriverList(sMatch: string = DEFAULT_DEVICE_SEARCH): string;
begin
  capturedData := '';
  runandcapture(FindTool('devcon.exe')+' findall '+sMatch, capturehook);
  result := capturedData;

end;

{ Tcmd_CleanupDrivers }

procedure Tcmd_CleanupDrivers.DoExecute;
var
  dc: TDevCon;
begin
  inherited;
  sleep(2000);
  dc := TDevCon.create;
  try
    dc.ProgressCallback := prog;
    dc.CleanupDrivers(Leave, LeaveMatch, Match);
  finally
    dc.free;
  end;
end;

procedure Tcmd_CleanupDrivers.Init;
begin
  inherited;
  MAtch := DEFAULT_DEVICE_SEARCH;
  LeaveMatch := 'JamStik';
  Leave := 1;
end;

procedure Tcmd_CleanupDrivers.InitExpense;
begin
  inherited;
  CPUExpense := 0.0;
  resources.SetResourceUsage('Driver', 1.0);
end;

procedure Tcmd_CleanupDrivers.prog(sender: TObject);
begin
  StepCount := TDevCon(sender).StepCount;
  Step := TDevCon(sender).Step;
end;

end.
