unit robot_movement;


interface

uses
  mplayer, robotics, windows, controls, managedthread, typex, systemx;

type
  TRobotMovement = procedure(FRobo: TLynxArm);

procedure PutDiscInTray(FRobo: TLynxArm);
procedure DumpDisc(FRobo: TLynxArm);
procedure GetDiscFromTray(FRobo: TLynxArm);
procedure GetDiscFRomBurner(FRobo: TLynxArm);
procedure GetDiscFromPowerfile(FRobo: TLynxArm);
procedure GetDiscFromPowerfile1(FRobo: TLynxArm);
procedure GetDiscFromPowerfile2(FRobo: TLynxArm);
procedure GetDiscFromPowerfile3(FRobo: TLynxArm);
procedure PutDiscInPOwerfile(FRobo: TLynxArm);
procedure PutDiscInBurner(FRobo: TLynxARm);
procedure GotoSleepPOsition(FRobo: TLynxARm);
procedure SleepRobot(FRobo: TLynxARm);
procedure WakeRobot(FRobo: TLynxArm);
procedure AnsiCharge(FRobo: TLynxArm);
procedure Suck(FRobo: TLynxArm);
procedure Hold(FRobo: TLynxARm);
procedure Drop(FRobo: TLynxArm);
procedure GetDiscFromStack1(FRobo: TLynxARm);

type
  TRobotThread = class(TProcessorThread)
  private
    FFunc: TRobotMovement;
    Frobot: TLynxARm;
    function GetFunc: TRobotMovement;
    procedure SetFunc(const Value: TRobotMovement);
    function GEtRobot: TLynxARm;
    procedure SetRobot(const Value: TLynxARm);
  public
    property robot: TLynxARm read GEtRobot write SetRobot;
    property func: TRobotMovement read GetFunc write SetFunc;
    procedure DoExecute;override;
  end;

function ThreadMove(FRobo: TLynxArm; func: TRobotMovement): TRobotThread;
procedure WaitForThread(thr: TRobotThread);
procedure FadeOutLeds(robot: TLynxARm);
procedure FadeInLeds(robot: TLynxArm);

procedure RobotDemo(FRobo: TLynxArm; var iPhase: ni);



implementation

procedure GetDiscFRomBurner(FRobo: TLynxArm);
begin
  with Frobo do begin

    OpenClamp;
//    MoveServo(0,1665);
//    MoveServo(1,2170);
//    MoveServo(2,2140);
//    MoveServo(3,1382);
//    MoveServo(4,700);
//    MoveServo(0,1587);
//    MoveServo(1,1830);
//    MoveServo(2,2140);
//    MoveServo(3,1720);
//    MoveServo(4,730);
MoveServo(0,1681);
MoveServo(1,1880);
MoveServo(2,2070);
MoveServo(3,1697);
MoveServo(4,700);
MoveServo(5,1920);
    Go;
    RCX.RCXProcedure(4);
    WaitForMovement;
MoveServo(0,2043);
    Go;
    WAitForMovement;
    CloseClamp;
    MoveServo(2,1500);
    Go;
    RCX.RCXProcedure(3);
    WAitForMovement;
    MoveSafe;
  end;

end;

procedure GetDiscFromTray(FRobo: TLynxArm);
var
  bb: byte;
begin
  with FRobo do begin
    FRobo.RCX.RCXFunction(11,bb,5000);
    //hover
    MoveServo(0,1540);
    MoveServo(1,1100);
    MoveServo(2,1070);
    MoveServo(3,630);
    MoveServo(4,1810);
    MoveServo(5,1200);
    Go;
    WaitForMovement;
    FRobo.CloseClamp;


    //down
    MoveServo(1,970);
    Go;
    WaitForMovement;
    FRobo.OpenClamp;

    //suck
    FRobo.RCX.RCXFunction(10,bb,5000);
    MoveServo(1,1100);
    Go;
    WaitForMovement;
    sleep(3000);

     //Pull Up
    MoveServo(0,1540);
    MoveServo(1,1240);
    MoveServo(2,1070);
    MoveServo(3,630);
    MoveServo(4,1810);
    MoveServo(5,1920);
    Go;


  end;
end;

procedure PutDiscInTray(FRobo: TLynxArm);
var
  bb: byte;
begin
  with FRobo do begin

    //hover
    WaitForMovement;
    MoveServo(0,1540);
    MoveServo(1,1170);
    MoveServo(2,1070);
    MoveServo(3,630);
    MoveServo(4,1810);
    MoveServo(5,1920);
    Go;
    WaitForMovement;
    FRobo.CloseClamp;

    //release
    FRobo.RCX.RCXFunction(12,bb,5000);


  end;
end;


procedure GetDiscFromPowerfile2(FRobo: TLynxArm);
begin
  with FRobo do begin
    sleep(8000);
    WaitForMovement;
    FRobo.CloseClamp;
    Go;
    WAitForMovement;

    MoveServo(0,1594);
    MoveServo(1,1230);
    MoveServo(2,1500);
    MoveServo(3,1000);
    MoveServo(4,1480);
  //MoveServo(5,1200);

  //  MoveServo(0,1459);
  //MoveServo(1,1170);
  //MoveServo(2,1500);
  //MoveServo(3,1000);
  //MoveServo(4,1485);
    Go;
    WaitForMovement;
    MoveSafe;

  end;

end;

procedure GetDiscFromPowerfile3(FRobo: TLynxArm);
begin
  with FRobo do begin
//    sleep(8000);
    WaitForMovement;
    FRobo.CloseClamp;
    Go;
    WAitForMovement;

    MoveServo(0,1594);
    MoveServo(1,1230);
    MoveServo(2,1500);
    MoveServo(3,1000);
    MoveServo(4,1480);
  //MoveServo(5,1200);

  //  MoveServo(0,1459);
  //MoveServo(1,1170);
  //MoveServo(2,1500);
  //MoveServo(3,1000);
  //MoveServo(4,1485);
    Go;
    WaitForMovement;
    MoveSafe;

  end;

end;


procedure GetDiscFromPowerfile1(FRobo: TLynxArm);
begin
  with FRobo do begin

    FRobo.OpenClamp;

    MoveServo(0,1594);
    MoveServo(1,1230);
    MoveServo(2,1500);
    MoveServo(3,1000);
    MoveServo(4,1480);

    Go;
    WAitForMovement;
    MoveServo(0,1590);
    MoveServo(1,690);
    MoveServo(2,1300);
    MoveServo(3,800);
    MoveServo(4,1540);
    Go;
  end;

end;

procedure PutDiscInPOwerfile(FRobo: TLynxArm);
begin
with FRobo do begin
//  MoveServo(0,1500);
//  MoveServo(1,970);
//  MoveServo(2,1820);
//  MoveServo(3,1500);
//  MoveServo(4,1500);
//MoveServo(0,1470);
MoveServo(0,1550);
MoveServo(1,1100);
MoveServo(2,1550);
MoveServo(3,1000);
MoveServo(4,1540);
//MoveServo(5,1920);
  Go;
  WaitForMovement;
  sleep(500);
  FRobo.OpenClamp;

  MoveServo(0,1400);
  Go;
  WAitForMovement;
  MoveServo(0,1650);
  Go;
  WaitForMovement;
//  MoveServo(0,1470);
  MoveServo(0,1550);
  MoveServo(1,1100);
  MoveServo(2,1550);
  MoveServo(3,1000);
  MoveServo(4,1540);
  Go;
  MoveSafe;

end;

end;
procedure PutDiscInBurner(FRobo: TLynxARm);
begin
  with FRobo do begin
//    RCX.RCXProcedure(3);
//    MoveServo(0,2090);
//    MoveServo(1,1820);
//    MoveServo(2,1850);
//    MoveServo(3,1110);
//    MoveServo(4,700);
MoveServo(0,2059);
MoveServo(1,1587);
MoveServo(2,1697);
MoveServo(3,1177);
MoveServo(4,700);
    Go;
    WaitForMovement;
    OpenClamp;
    MoveServo(0,2090);
    MoveServo(1,1820);
    MoveServo(2,1850);
    MoveServo(3,830);
    MoveServo(4,700);
    Go;
    WaitForMovement;
    MoveServo(0,1570);
    MoveServo(1,1492);
    MoveServo(2,1886);
    MoveServo(3,1300);
    MoveServo(4,700);
    Go;
    WaitForMovement;
    CloseClamp;
    MoveServo(0,1897);
    MoveServo(1,1508);
    MoveServo(2,1886);
    MoveServo(3,1300);
    MoveServo(4,700);
    Go;
    WaitForMovement;
    MoveServo(1,1608);
    Go;
    WaitForMovement;

    //check left
    MoveServo(0,2210);
    MoveServo(1,1320);
    MoveServo(2,1540);
    MoveServo(3,1350);
    MoveServo(4,700);
    Go;
    WaitForMovement;
    MoveServo(1,1250);
    Go;
    WaitForMovement;
    MoveServo(3,1270);
    Go;
    WaitForMovement;

    //front check
    MoveServo(0,2020);
    MoveServo(1,1280);
    MoveServo(2,1540);
    MoveServo(3,1380);
    MoveServo(4,700);
    Go;
    WaitForMovement;
    MoveServo(3,1280);
    MoveServo(4,700);
    Go;
    WaitForMovement;
    MoveServo(1,1500);
    Go;
    WaitForMovement;

    //backcheck
    MoveServo(0,2106);
    MoveServo(1,1760);
    MoveServo(2,2091);
    MoveServo(3,1146);
    MoveServo(4,700);
    Go;
    WaitForMovement;

    MoveServo(0,2106);
    MoveServo(1,1728);
    MoveServo(2,2106);
    MoveServo(3,1272);
    MoveServo(4,700);
    Go;
    WaitForMovement;

    MoveSafe;

  end;

end;






{ TRobotThread }

procedure TRobotThread.DoExecute;
begin
  inherited;

  Lock;
  try
    func(robot);
  finally
    Unlock;
  end;


end;

function TRobotThread.GetFunc: TRobotMovement;
begin
  Lock;
  try
    result := FFunc;
  finally
    Unlock;
  end;
end;

function TRobotThread.GEtRobot: TLynxARm;
begin
  Lock;
  try
    result := FRobot;
  finally
    UNLOCK;
  end;
end;

procedure TRobotThread.SetFunc(const Value: TRobotMovement);
begin
  Lock;
  try
    FFunc := value;
  finally
    Unlock;
  end;
end;

procedure TRobotThread.SetRobot(const Value: TLynxARm);
begin
  Lock;
  try
    FRobot := value;
  finally
    Unlock;
  end;
end;

function ThreadMove(FRobo: TLynxArm; func: TRobotMovement): TRobotThread;
begin
  result := TRobotThread.create(nil, nil);
  result.robot := FRobo;
  result.func := func;
  result.Start;

end;

procedure WaitForThread(thr: TRobotThread);
begin
  while not thr.ready do
    sleep(100);

  thr.Free;

end;

procedure GotoSleepPOsition(FRobo: TLynxARm);
begin
  with FRobo do begin
    MoveServo(0,626);
    MoveServo(1,1035);
    MoveServo(2,1965);
    MoveServo(3,2500);
    MoveServo(4,700);
    MoveServo(5,1500);
    Go;
//    WaitForMovement;
  end;

end;
procedure SleepRobot(FRobo: TLynxARm);
begin
  GotoSleepPosition(FRobo);
  FRobo.WAitForMovement;
  FadeOutLeds(FRobo);
  FRobo.AllOff;
end;
procedure WakeRobot(FRobo: TLynxArm);
begin
//  FRobo.IgnoreSpeedLimit:= true;
  GotoSleepPOsition(FRobo);

  FadeInLeds(FRobo);

  with FRobo do begin
    WaitForMovement;
    MoveServo(1,1500);
    Go; WaitForMovement;
    MoveServo(2,570);
    MoveServo(3,1500);
    Go; WaitForMovement;


    MoveServo(4,1500);
    MoveServo(5,1500);
    SetServoHome(0,1500);
    Go;

  end;

  //  FRobo.IgnoreSpeedLimit := false;
//  FRobo.MoveHome;

end;

procedure FadeOutLeds(robot: TLynxARm);
var
  tm1,tm2: cardinal;
  t: integer;
  l: integer;
begin
  tm1 := GEtTickCount;
  repeat
    tm2 := GEtTickCount;
    if tm2 < tm1 then
      tm1 := tm2;


    l := 800-(tm2-tm1);
    if l < 0 then l := 0;

    for t:= 16 to 20 do begin
      robot.LedLight(t, l/800);
    end;

    sleep(100);

    if tm2-tm1 > 800 then
      break;


  until false;

end;

procedure FadeInLeds(robot: TLynxArm);
var
  tm1,tm2: cardinal;
  t: integer;
  l: integer;
begin
  tm1 := GEtTickCount;
  repeat
    tm2 := GEtTickCount;
    if tm2 < tm1 then
      tm1 := tm2;


    l := (tm2-tm1);
    if l < 0 then l := 0;

    for t:= 16 to 20 do begin
      robot.LedLight(t, l/800);
    end;
    sleep(100);

    if tm2-tm1 > 800 then
      break;


  until false;
end;

procedure DumpDisc(FRobo: TLynxArm);
begin
  with FRobo do begin
    MoveServo(0,1570);
    MoveServo(1,1130);
    MoveServo(2,1190);
    MoveServo(3,1300);
    MoveServo(4,700);
    Go;
    WaitForMovement;
    OpenClamp;
    MoveSafe;
  end;
end;
procedure GetDiscFromPowerfile(FRobo: TLynxArm);
begin
  GetDiscFromPowerfile1(FRobo);
  GetDiscFromPowerfile3(FRobo);
end;

procedure AnsiCharge(FRobo: TLynxArm);
var
  b: byte;
begin
  FRobo.RCX.RCXFunction(11,b,15000);
end;
procedure Suck(FRobo: TLynxArm);
var
  b: byte;
begin
  FRobo.RCX.RCXFunction(10,b,5000);
end;

procedure Hold(FRobo: TLynxARm);
var
  b: byte;

begin
FRobo.RCX.RCXFunction(13,b,5000);
end;

procedure Drop(FRobo: TLynxArm);
var
  b: byte;

begin
FRobo.RCX.RCXFunction(12,b,5000);
end;

procedure GetDiscFromStack1(FRobo: TLynxARm);
begin
  with FRobo do begin
    FRobo.DrainComm;
    CloseClamp;
    AnsiCharge(FRobo);
    MoveServo(0,1320);
    MoveServo(1,1870);
    MoveServo(2,1660);
    MoveServo(3,690);
    MoveServo(4,1500);
    MoveServo(5,1500);
    Go;
    WaitForMovement;
    MoveServo(1,1461);
    MoveServo(2,1660);
    MoveServo(3,820);
    GoUntil(IsInputDLow);
    Suck(FRobo);
    sleep(8000);
    MoveServo(1,1870);
    MoveServo(2,1660);
    MoveServo(3,690);
    Go;
    WAitForMoveMent;
    MoveHome;
    Hold(FRobo);


  end;



end;

procedure RobotDemo(FRobo: TLynxArm; var iPhase: ni);
begin
  //straight up
  with FRobo do
  begin
    case iPhase of

      0:
      begin
        MoveServo(0,1500);
        MoveServo(1,1500);
        MoveServo(2,1500);
        MoveServo(3,1500);
        MoveServo(4,1500);
        MoveServo(5,1500);
        Go;
        WaitForMovement;
        inc(iPhase);
      end;

      1://salute
      begin
        MoveServo(0,2453);
        MoveServo(1,1902);
        MoveServo(2,1587);
        MoveServo(3,1445);
        MoveServo(4,1636);
        MoveServo(5,1206);
        Go;
        WaitForMovement;
        inc(iPhase);
      end;

      2: //sniff around 1
      begin
        MoveServo(0,957);
        MoveServo(1,1728);
        MoveServo(2,1933);
        MoveServo(3,1177);
        MoveServo(4,723);
        MoveServo(5,1359);
        Go;
        WaitForMovement;
        inc(iPhase);
      end;

      3: //sniff around 2
      begin
        MoveServo(0,2217);
        MoveServo(1,1728);
        MoveServo(2,1933);
        MoveServo(3,1177);
        MoveServo(4,723);
        MoveServo(5,1359);
        Go;
        WaitForMovement;
        inc(iPhase);
      end;

      4: //cobra
      begin
        MoveServo(0,2217);
        MoveServo(1,1728);
        MoveServo(2,783);
        MoveServo(3,705);
        MoveServo(4,1636);
        MoveServo(5,1597);
        Go;
        WaitForMovement;
        inc(iPhase);
      end;

      5: //laugh 1
      begin
        MoveServo(0,2169);
        MoveServo(1,1902);
        MoveServo(2,1587);
        MoveServo(3,1177);
        MoveServo(4,723);
        MoveServo(5,1217);
        Go;
        WaitForMovement;
        inc(iPhase);
      end;

      6: //laugh 2
      begin
        openclamp;
        Go;
        WaitForMovement;
        inc(iPhase);
      end;

      7: //extend out
      begin
        MoveServo(0,1713);
        MoveServo(1,1004);
        MoveServo(2,705);
        MoveServo(3,1303);
        MoveServo(4,1031);
        MoveServo(5,1517);
        Go;
        WaitForMovement;
        iPhase := 1;
      end;
    end;

  end;


end;


end.
