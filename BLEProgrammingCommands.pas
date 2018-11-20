unit BLEProgrammingCommands;

interface

uses
  debug, tickcount, commandprocessor, exe, sysutils, systemx, typex, stringx, simplebleconnection, simplecommport, latchcontrol_FTDI;

const
  PROG_WAIT = 60000;
type
  TBLEProgrammingCommand = class(TCommand)
  private
    FLoadBat: string;
    FIdx: ni;
    FPort: string;
    FTestPort: TSimpleBLEMIDIConnection;
    FMac: string;
    FDoNotTest: boolean;
    FNoDelay: boolean;
    procedure SetIdx(const Value: ni);
  public
    lc: TLatchControl;
    Latchbit: ni;
    LAtchAlwaysOn: boolean;
    output: string;
    property LoadBat: string read FLoadBat write FLoadbat;
    property Port: string read FPort write FPort;
    property TestPort: TSimpleBLEMIDIConnection read FTestPort write FTestPort;
    property Idx: ni read FIdx write SetIdx;
    property Mac6: string read FMac write FMac;
    procedure DOExecute;override;
    function Prog: boolean;
    function Test: boolean;
    property DoNotTest: boolean read FDoNotTest write FDoNotTest;
    procedure CapHook(ccStatus: TConsoleCaptureStatus;  sBufferData: string);
    property NoDelay: boolean read FNoDelay write FNodelay;
    procedure InitExpense; override;
  end;


implementation

{ TBLEProgrammingCommand }

procedure TBLEProgrammingCommand.CapHook(ccStatus: TConsoleCaptureStatus;
  sBufferData: string);
begin
  case ccStatus of
    ccStart:
      Output := '';
    ccProgress:
      Output := output + sBufferData;
  end;
end;

procedure TBLEProgrammingCommand.DOExecute;
var
  cp: TSimpleCommPortConnection;
begin
  Debug.Log(self, 'Command--Begin');
  if idx < 1 then
    raise ECritical.Create('Set IDX on '+self.ClassName);

  if mac6 = '' then
    raise Ecritical.Create('Set Mac6');

Debug.Log(self, 'Command--Check COMM Port');
try
    cp := TSimpleCommPortConnection.create;
    try
      if port = 'COM34' then
        debug.log(self, 'com34');
      cp.EndPoint := port;

      cp.Connect;
    finally
      cp.free;
    end;
  except
    on E: exception do begin
      cresult := false;
      self.status := port+' Failed COM Test: '+E.message;
      exit;
    end;
  end;


  CResult := true;
  try
    if port='COM32' then
      debug.log(self, 'here');
    if prog then begin
      if not DoNotTest then
        test;
    end;
  except
    on e: Exception do begin
      CResult := false;
      self.Status := 'Exception!';
    end;
  end;

  if not CResult then begin
    self.Status := 'Retrying...';
    if prog then begin
      if not DoNotTest then
        test;
    end;
  end;


end;
procedure TBLEProgrammingCommand.InitExpense;
begin
  inherited;
  CPUExpense := 0.0;
  //MemoryExpense := 1.0;
end;

function TBLEProgrammingCommand.Prog: boolean;
var
  ex: exe.Tcmd_RunExe;
  sProg: string;
  sStatusFile: string;
  sStatus: string;
begin
  inherited;
  Debug.Log(self, 'Command--Prog--Latch OFF');
  if not LatchAlwaysOn then begin
    lc.BitOff(LatchBit);
    sleep(1000);
  end;
  Debug.Log(self, 'Command--Prog--Latch ON');
  if not LatchAlwaysOn then begin
    lc.BitOn(LatchBit);
    sleep(2000);
  end else
    if assigned(lc) then begin
      Debug.Log(self, 'Command--Prog--LATCH IS ALWAYS ON!');
      lc.SetValue($ff);
    end;


  result := true;

  CResult := true;
  Debug.Log(self, 'Command--Prog--Program...');
  Status := 'Program...';
  sStatusFile := slash(extractfilepath(loadBat))+'result_'+inttostr(idx)+'.txt';
  if fileexists(sStatusFIle) then
    deletefile(sStatusFile);
  Debug.Log(self, 'Command--Prog--Program...Run...');
  RunProgramAndWait(LoadBat, Port+' '+Mac6+' '+inttostr(idx), extractfilepath(LoadBat), true, true, false);

  Debug.Log(self, 'Command--Prog--Program...Run...Status...');
  if not fileexists(sStatusFile) then begin
    CResult := false;
    Status := 'Failed to find status of operation';
    exit;
  end;

  sStatus := LoadStringFromFile(sStatusFile);
  if zpos('failed', lowercase(sStatus)) >=0 then begin
    Status := sStatus;
    CResult := false;
    result := false;
    exit;
  end else
    CResult := true;

  output := sStatus;
  Debug.Log(self, 'Command--Prog--Program...Run..Status...'+output);

  //RunAndCapture('"'+LoadBat+'" '+Port+' '+inttostr(idx), caphook);
  //RunAndCapture('c:\test.bat x y', caphook);
  status := output;
//  ex := Tcmd_RunExe.Create;
//  try
//    ex.Prog := LoadBat;
//    ex.Params := Port+' '+inttostr(idx);
//    ex.Hide := true;
//    ex.ConsoleRedirect := true;
//    ex.CaptureConsoleoutput := true;
//    ex.Start;
//    ex.WaitFor;
//    output := ex.ConsoleOutput;
//
//  finally
//    ex.Free;
//  end;


  //SLEEP(3000);
end;

procedure TBLEProgrammingCommand.SetIdx(const Value: ni);
begin
  FIdx := Value;
  Resources.SetResourceUsage('BUILD'+inttostr(FIdx),1.0);
end;

function TBLEProgrammingCommand.Test: boolean;
var
  bleServer: TSimpleCommportConnection;
  bleClient: TSimpleBLEMIDIConnection;
  a: array[0..2] of byte;
  b: array[0..2] of byte;
  t: ni;
  tm, tmStart: ticker;
begin
  Debug.Log(self, 'Command--Test');
  bleClient := TestPort;
  bleClient.Lock;
  try
    result := true;
    tmStart := getticker;
    tm := tmStart;


{$IFDEF DO_TEST1}
  Debug.Log(self, 'Command--Test 1');
    Status := 'Test 1...';
    bleServer := TSimpleCommPortConnection.create;
    try
//      bleServer.Endpoint := Port;
//      bleServer.Connect;
//      bleServer.baudrate := 31250;

      bleClient.EndPoint := 'FF736A'+Mac6;
      Debug.Log(self, 'Command--Test '+bleClient.EndPoint);
      if bleClient.Connect then begin
        result := true;
        CRESULT := true;
//        exit;
      end else begin
        Debug.Log(self, 'Command--Failed to connect LOG: '+Self.TestPort.selflog.text);
        result := false;
        CRESULT := false;
        STATUS := 'Failed to connect '+Self.TestPort.selflog.text;
//        exit;
      end;

{      a[0] := $80;
      a[1] := $66;
      a[2] := $77;
      b[0] := 0;
      b[1] := 0;
      b[2] := 0;
      bleClient.SendData(@a[0], 3, true);
      if not bleServer.WaitForData(8000) then begin
        Cresult := false;
        result := false;
        Status := 'Failed to receive data that was sent.';
      end else begin
        bleServer.ReadData(@b[0], 3, true);
        for t:= 0 to high(a) do begin
          if a[t] <> b[t] then begin
            result := false;
            Cresult := false;
            Status := 'Data did not match';
            exit;
          end;
        end;
      end;}

      if not NoDelay then begin
        tmStart := getticker;
        repeat
          Status := 'Wait '+inttostr(round(gettimesince(tmStart)/PROG_WAIT * 100))+'%';
          Debug.Log(self, status);
          sleep(500);
        until gettimesince(tmSTart) > PROG_WAIT;
      end;

      bleClient.Disconnect;
    finally
      bleServer.free;
    end;
{$ENDIF}

    bleClient.CommandReset;
    tmStart := getticker;
    if not NoDelay then begin
      repeat
        Status := 'Wait '+inttostr(round(gettimesince(tmStart)/PROG_WAIT * 100))+'%';
        sleep(200);

      until gettimesince(tmSTart) > PROG_WAIT;
    end;

    bleClient.CommandReset;


    Debug.Log(self, 'Command--Test 2');
    Status := 'Test 2...';
    bleServer := TSimpleCommPortConnection.create;
    try
//      bleServer.Endpoint := Port;
//      bleServer.Connect;
//      bleServer.baudrate := 31250;

      bleClient.EndPoint := 'FF736A'+Mac6;
      Debug.Log(self, 'Command--Test 2...'+bleClient.EndPoint);
      if bleClient.Connect then begin
        result := true;
        CRESULT := true;
        if not LatchAlwaysOn then
          lc.BitOff(LatchBit);//turn off power to device when successful to reduce interference from devices that get 2nd chances.
//        exit;
      end else begin
        result := false;
        CRESULT := false;
        STATUS := 'Failed to connect '+Self.TestPort.selflog.text;
        Debug.Log(self, 'Command--Test 2...FAILED TO CONNECT LOG: '+status);
//        exit;
      end;

{      a[0] := $80;
      a[1] := $66;
      a[2] := $77;
      b[0] := 0;
      b[1] := 0;
      b[2] := 0;
      bleClient.SendData(@a[0], 3, true);
      if not bleServer.WaitForData(8000) then begin
        Cresult := false;
        result := false;
        Status := 'Failed to receive data that was sent.';
      end else begin
        bleServer.ReadData(@b[0], 3, true);
        for t:= 0 to high(a) do begin
          if a[t] <> b[t] then begin
            result := false;
            Cresult := false;
            Status := 'Data did not match';
            exit;
          end;
        end;
      end;}



      bleClient.Disconnect;
    finally
      bleServer.free;
    end;
  finally
    bleClient.Unlock;
  end;


end;



end.
