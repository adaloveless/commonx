unit robotics;

interface

uses
  sharedobject, SimpleCommPort, sysutils, windows, speech, typex;

type
  TServoLImits = record
    Low: integer;
    High: integer;
    Home: integer;
    SpeedLimit: integer;
    PulseMoveMent: boolean;
    CurrentPOsition: integer;
  end;


  PServoLImits = ^TServoLimits;

  TServoController = class;
  TRoboCheckProc = function(Robo: TServoController): boolean;

  TQueryPositionEvent = procedure(sender: TServoController; iServo: integer; iPOsition: integer) of object;

  TServoController = class(TSharedObject)
  private
    FConnection: TSimpleCommPortConnection;
    FConnected: boolean;
    FPort: ansistring;
    FLimits: array[0..31] of TServoLimits;
    FOnQueryPosition: TQueryPositionEvent;
    FQuerying: boolean;
    FMAxServo: integer;
    FVA, FVB, FVC, FVD: integer;
    FA,FB,FC,FD: boolean;
    FIgNoreSpeedLimit: boolean;
    function GetVA: integer;
    function GetVB: integer;
    function GetVC: integer;
    function GetVD: integer;
    function GetServoLimits(idx: integer): PServoLimits;


  public
    constructor create;override;
    destructor destroy;override;
    procedure PowerUp;
    property connection: TSimpleCommPortConnection read FConnection;
    property Connected: boolean read FConnected;
    procedure Connect;
    procedure Disconnect;
    property Port: ansistring read FPort write Fport;

    function MoveServo(iServo: integer; position: integer; iTime: integer = -1): boolean;overload;
    function MoveServoF(iServo: integer; position: nativefloat; iTime: integer = -1): boolean;overload;
    procedure BuildDefaultLimits;virtual;
    procedure SetServoLimit(iServo, low, high: integer);
    procedure SetServoHome(iServer, home: integer);
    procedure SetServoSpeedLimit(iServo, iSpeed: integer);
    procedure SetServoPulseMovement(iServo: integer; b: boolean);
    property Limits[idx: integer]: PServoLimits read GetServoLimits;
    procedure Go;
    procedure GoUntil(proc: TRoboCheckProc);
    procedure StopMoveMent;
    procedure MoveHome(bWait: boolean = true);virtual;

    function QueryPulse(iServo: integer): integer;
    function IsMoving: boolean;
    procedure WaitForMovement;
    procedure PulseMove(iCount: integer);
    procedure DrainComm;
    procedure MotorOff(iServo: integer);
    procedure AllOff;
    property OnQueryPosition: TQueryPositionEvent read FOnQueryPosition write FOnQueryPosition;
    procedure DoOnQueryPosition(iServo: integer; iPOsition: integer);
    property MaxServo: integer read FMAxServo write FMaxServo;


    function ReadAnalog(input: AnsiChar): integer;
    function ReadInputs: integer;

    property VA: integer read GetVA;
    property VB: integer read GetVB;
    property VC: integer read GetVC;
    property VD: integer read GetVD;
    property A: boolean read FA;
    property B: boolean read FB;
    property C: boolean read FC;
    property D: boolean read FD;

    procedure LedLight(ch: integer; rLevel: real);
    property IgnoreSpeedLimit: boolean read FIgNoreSpeedLimit write FIgnoreSpeedLImit;
  end;

  TRCXLink = class(TSharedObject)
  private

  protected
    com: TSimpleCommPortConnection;
  public
    constructor create;override;
    destructor destroy;override;
    procedure SendLegoMessage(b: byte);
    function GetLegoMessage(var b: byte): boolean;
    procedure RCXFunction(bOut: byte; out bIn: byte; iTimeOut: integer = 2000);
    procedure RCXProcedure(bOUT: byte; iTimeOut: integer = 5000);
    procedure DrainInputBuffer;
  end;

  TLynxArm = class(TServoController)
  private
    FRCX: TRCXLink;

  public
    constructor create;override;
    destructor destroy;override;
    procedure BuildDefaultLimits; override;
    procedure GrabCd;
    procedure HoverOverTray;
    procedure GrabReady;
    procedure CloseClamp;
    procedure OpenClamp;
    procedure RelaxClamp;
    procedure PowerClamp;
    procedure MOveHome(bWait: boolean = true);override;
    procedure MoveSafe;
    property RCX: TRCXLink read FRCX write FRCX;
  end;

function IsInputDLow(robo: TServoController): boolean;


implementation

{ TServoController }
function IsInputDLow(robo: TServoController): boolean;
begin
  result := not robo.D;
end;

constructor TServoController.create;
begin
  inherited;
  MAxServo := 31;
  FConnection := TSimpleCommPortConnection.create;
  FPort := 'COM16';
  FConnection.EndPoint := 'COM16';

  Fconnection.Connect;
  self.DrainComm;
  self.BuildDefaultLimits;
end;

procedure TServoController.Disconnect;
begin
  FConnection.Disconnect;
end;

destructor TServoController.destroy;
begin
  Fconnection.Destroy;
  inherited;
end;

procedure TServoController.Connect;
begin
  FConnection.EndPoint := Fport;
  FConnection.Connect;
  FConnection.BaudRate := 115200;
  if FConnection.BaudRate <> 115200 then
    raise exception.create('Baud Rate was not read as set');


end;

procedure TServoController.BuildDefaultLimits;
var
  t: integer;
begin
  for t:= 0 to MaxServo do begin
    SetServoLimit(t, 900, 2100);
    SetSErvoHome(t,1500);
    SetServoSpeedLimit(t, 65535);
    SetServoPulseMoveMent(t, false);

  end;

end;


procedure TServoController.SetServoLImit(iServo, low, high: integer);
begin
  FLimits[iServo].Low := low;
  FLimits[iServo].High:= high;

end;

function TServoController.MoveServo(iServo, position:integer; iTime: integer = -1): boolean;
var
  s: ansistring;
begin
//  if iTime < 0 then
//    iTime := self.FDefaultMoveTime;

  if position < FLimits[iServo].Low then
    position := FLimits[iServo].Low;

  if position > FLimits[iServo].High then
    position := FLimits[iServo].High;

  s := '#'+inttostr(iServo)+'P'+inttostr(position);

  if iTime > 0 then
    s := s+'T'+inttostr(iTime);


  if not self.IgnoreSpeedLimit then
    s := s+'S'+inttostr(FLimits[iServo].SpeedLimit);


  connection.SendString(s);
  Limits[iServo].CurrentPosition := position;


//  sleep(iTime);

end;

procedure TServoController.Go;
begin
  self.connection.SendString(#13);
end;

procedure TServoController.GoUntil(proc: TRoboCheckProc);
begin
  Go;
  while self.IsMoving do begin
    if proc(self) then
      StopMoveMent;
  end;

end;

function TServoController.IsMoving: boolean;
var
  i: integer;
  c: AnsiChar;
begin
  connection.SendString('Q'#13);
  i:= 0;
  while i = 0 do begin
    i := connection.ReadData(@c,1);
    sleep(1);
  end;
  result := c = '+';

end;

procedure TServoController.LedLight(ch: integer; rLevel: real);
var
  s: ansistring;
begin
  s := '#'+inttostr(ch);
  if rLevel > 1.0 then
    rLevel := 1.0;

  if rLEvel>=1.0 then
    s := s+'H'
  else
  if rLevel=0.0 then
    s := s+'L'
  else
    s := '';


  if s= '' then begin
    moveServo(ch, round(rLevel*2500), 500);
    Go;
  end else
    connection.SendString(s+#13);

end;

procedure TServoController.SetServoHome(iServer, home: integer);
begin
  FLImits[iServer].Home := home;
end;

procedure TServoController.MoveHome(bWait: boolean = true);
var
  t: integer;
begin
  for t:= 0 to MaxServo do begin
    MoveServo(t, FLimits[t].Home);
  end;
  Go;
  if bWAit then
    WaitForMovement;
end;

function TServoController.MoveServoF(iServo: integer; position: nativefloat;
  iTime: integer): boolean;
var
  r: nativefloat;
  w: nativefloat;
begin
  w := FLImits[iServo].High-FLimits[iServo].Low;
  r := (position*w)+FLimits[iServo].Low;
  if w = 0 then
    exit;

  result := MoveServo(iServo, round(r), iTime);
end;

procedure TServoController.PowerUp;
var
  t: integer;
begin
  SayNatural('Powering up');
  for t:= 0 to MaxServo do begin
//    SayNatural(inttostr(t));
    MoveServo(t, FLimits[t].Home);
  end;
  Go;
  WaitForMovement;

end;

function TServoController.QueryPulse(iServo: integer): integer;
var
  c: AnsiChar;
begin
  result := 0;
//  while result = 0 do begin
    c := #0;
    self.connection.SendString('QP'+inttostr(iServo)+#13);
    self.connection.WaitForData(1000);
    self.connection.ReadData(@c, 1);
    result := ord(c)*10;
  DoOnQueryPOsition(iServo, result);

//  end;

end;

function TServoController.ReadAnalog(input: AnsiChar): integer;
var
  c: AnsiChar;
begin
  connection.SendString('V'+input+#13);
  connection.WaitForData(1000);
  connection.ReadData(@c, 1);
  result := ord(c);
end;

function TServoController.ReadInputs: integer;
var
  c: array[0..3] of AnsiChar;
begin
  connection.SendString('A B C D'#13);
  connection.WaitForData(1000);
  connection.ReadData(@c[0], 4, true);
  FA := c[0]='1';
  FB := c[1]='1';
  FC := c[2]='1';
  FD := c[3]='1';

//  result := (FA shl 24)+(FB shl 16)+(FC shl 8)+FD;


end;

procedure TServoController.SetServoSpeedLimit(iServo, iSpeed: integer);
begin
  FLimits[iServo].SpeedLimit := iSpeed;
end;

procedure TServoController.StopMoveMent;
var
  t: integer;
begin
  for t:= 0 to 15 do begin
    self.MoveServo(t, self.QueryPulse(t));
  end;
  Go;


end;

procedure TServoController.WaitForMovement;
var
  b: boolean;
  c: AnsiChar;
  i: integer;
  t: integer;
  tmStart, tmEnd: cardinal;
begin
  c := #0;
  b := true ;

  while b do begin
    connection.SendString('Q'#13);
    sleep(500);
//    if not b then
//      sleep(100);
    i:= 0;
    tmSTart := GetTickCount;
    while i = 0 do begin
      i := connection.ReadData(@c,1);
      sleep(1);
      tmEnd := GetTickCount;
      if tmStart > tmEnd then tmStart := tmEnd;

      if (tmend-tmstart > 10000) then
        break;
    end;
    b := c = '+';
    if b then
      sleep(100);




  end;

  for t:= 0 to MaxServo do begin
    self.QueryPulse(t);
  end;


//  PulseMove;




end;

procedure TServoController.DrainComm;
var
  i: integer;
  c: AnsiChar;
begin
  try
    while connection.readdata(PByte(PAnsiChar(c)),1) > 0 do
      sleep(0);
  except
  end;
end;

function TServoController.GetServoLimits(idx: integer): PServoLimits;
begin
  result := @FLImits[idx];
end;

function TServoController.GetVA: integer;
begin
  result := FVA;
end;

function TServoController.GetVB: integer;
begin
  result := FVB;
end;

function TServoController.GetVC: integer;
begin
  result := FVC;
end;

function TServoController.GetVD: integer;
begin
  result := FVD;
end;

procedure TServoController.MotorOff(iServo: integer);
begin
  connection.SendString('#'+inttostr(iServo)+'L');
end;

procedure TServoController.AllOff;
var
  t: integer;
begin
  for t:= 0 to MaxServo do begin
    connection.SendString('#'+inttostr(t)+'L'#13);

  end;
end;

procedure TServoController.SetServoPulseMovement(iServo: integer; b: boolean);
begin
  FLimits[iServo].PulseMoveMent := b;
end;

procedure TServoController.PulseMove(iCount: integer);
var
  t: integer;
  u: integer;
  bAny: boolean;
begin
  self.Go;


  exit;
  bAny := false;
  for u := 1 to iCount do begin
    for t:= 0 to MaxServo do begin
      if Limits[t].PulseMovement then begin
        connection.SendString('#'+inttostr(t)+'L'#13);
        bAny := true;
      end;
    end;

    if not bAny then exit;
    sleep(100);
//    windows.beep(100,10);
    for t:= 0 to MaxServo do begin
      if Limits[t].PulseMovement then begin
        self.MoveServo(t, self.Limits[t].CurrentPOsition);
        self.Go;
      end;
    end;
    sleep(100);
  end;



end;

procedure TServoController.DoOnQueryPosition(iServo, iPOsition: integer);
begin
  if FQuerying then exit;
  FQuerying := true;
  try
    if assigned(FOnQueryPOsition) then
      FOnQueryPosition(self, iServo, iPosition);
  finally
    FQuerying := false;
  end;
end;

{ TLynxArm }

procedure TLynxArm.BuildDefaultLimits;
begin
  inherited;
  MaxServo := 5;
  SetServoLimit(0,500,2500);
  SetServoLimit(1,500,2500);
  SetServoLimit(2,500,2500);
  SetServoLimit(3,500,2500);
  SetServoLimit(4,700,2150);
  SetServoLimit(5,1200,1920);
  SetServoLimit(16,0,2500);
  SetServoLimit(17,0,2500);
  SetServoLimit(18,0,2500);
  SetServoLimit(19,0,2500);
  SetServoLimit(20,0,2500);
  SetServoLimit(21,0,2500);
  SetServoLimit(22,0,2500);
  SetServoLimit(23,0,2500);
  SetServoLimit(24,0,2500);


  SetServoHome(0,1500);
  SetServoHome(1,1500);
  SetServoHome(2,570);
  SetServoHome(3,1500);
  SetServoHome(4,1500);
  SetServoHome(5,1500);

  SetServoSpeedLimit(0,550);
  SetServoSpeedLimit(1,400);
  SetServoSpeedLimit(2,400);
  SetServoSpeedLimit(3,700);
  SetServoSpeedLimit(4,1500);
  SetServoSpeedLimit(5,1500);

//  SetServoPulseMovement(0,true);


end;

procedure TLynxArm.OpenClamp;
begin
//  PowerClamp;
  MoveServo(5, 3000);
  Go;
  WaitForMovement;
  RElaxClamp;
end;

procedure TLynxArm.CloseClamp;
begin
//  PowerClamp;
  MoveServo(5, 0);
  Go;
  WaitForMovement;
  RElaxClamp;
end;

constructor TLynxArm.create;
begin
  inherited;
  FRCX := TRCXLInk.create;
end;

destructor TLynxArm.destroy;
begin
  inherited;
  FRCX.free;
end;

procedure TLynxArm.GrabCd;
begin
MoveServo(0,1620);
MoveServo(1,920);
MoveServo(2,1740);
MoveServo(3,2358);
MoveServo(4,1588);
MoveServo(5,1260);
go;
exit;


MoveServo(0,1620);
MoveServo(1,925);
MoveServo(2,1744);
MoveServo(3,1500);
MoveServo(4,1490);
MoveServo(5,1260);

  Go;
  WaitForMovement;
  MoveServo(0,1618);
  MoveServo(1,626);
  MoveServo(2,1492);
  MoveServo(3,1500);
  MoveServo(4,1489);
  MoveServo(5,1260);
  Go;
  WaitForMovement;
MoveServo(0,1620);
MoveServo(1,925);
MoveServo(2,1744);
MoveServo(3,1500);
MoveServo(4,1490);
MoveServo(5,1260);
  Go;
  WaitForMovement;

end;

procedure TLynxArm.RelaxClamp;
begin
  connection.SendString('#5L'#13);
end;


procedure TLynxArm.PowerClamp;
begin
  MoveServo(5, limits[5].CurrentPosition);
  Go;
//  connection.SendString('#5'+inttostr[FLimits].CurrentPosition'#13);
end;

procedure TLynxArm.MOveHome;
begin
  inherited;
  RelaxClamp;
  LEdLIght(16, 1);
  LEdLIght(17, 1);
  LEdLIght(18, 1);
  LEdLIght(19, 1);
  LEdLIght(20, 1);


end;

procedure TLynxArm.MoveSafe;
begin
  MoveServo(0,1571);
  MoveServo(1,1618);
  MoveServo(2,1193);
  MoveServo(3,1303);
  MoveServo(4,700);
  Go;
  WaitForMovement;
end;

procedure TLynxArm.HoverOverTray;
begin
MoveServo(0,2122);
Go;
WaitForMovement;
PulseMove(20);
PowerClamp;
MoveServo(5,1920);
Go;
WAitForMovement;
MoveServo(1,1413);
MoveServo(2,1417);
MoveServo(3,591);
MoveServo(4,2150);
Go;
WAitForMovement;
//grab it
PulseMove(5);
MoveServo(0,2120);
MoveServo(1,1410);
MoveServo(2,1476);
MoveServo(3,657);
MoveServo(4,2150);
Go;
WaitForMovement;
CloseClamp;
WaitForMovement;


//lift off
MoveServo(1,1413);
MoveServo(2,1417);
MoveServo(3,591);
MoveServo(4,2150);
Go;
WaitForMovement;
RElaxClamp;

MoveHome;


end;

procedure TLynxArm.GrabReady;
begin
WaitForMovement;
CloseClamp;
WaitForMovement;
RelaxClamp;
MOveHome;

end;

{ TRCXLink }

constructor TRCXLink.create;
begin
  inherited;
  com := TSimpleCommPortConnection.create;
  com.EndPoint := '\\.\LEGOTOWER1';
//  com.Connect;

end;

destructor TRCXLink.destroy;
begin
  com.free;
  inherited;
end;

procedure TRCXLink.DrainInputBuffer;
var
  b: byte;
begin
  while GetLegoMessage(b) do
    sleep(1);
end;

function TRCXLink.GetLegoMessage(var b: byte): boolean;
var
  a: array [0..8] of byte;
  i: integer;
  r: integer;
begin
  if not com.Connected then exit;

  result := false;
  i := 0;
  while i < 9 do begin
   r := com.readdata(@a[i], 9-i);
   if r=0 then
     exit;

   i := i+r;
  end;

  b := a[5];
  result := true;


end;

procedure TRCXLink.SendLegoMessage(b: byte);
var
  a: array [0..8] of byte;
begin
  a[0] := $55;
  a[1] := $FF;
  a[2] := $00;
  a[3] := $F7;
  a[4] := $08;
  a[5] := b;
  a[6] := $FF-b;
  a[7] := $F7+b;
  a[8] := b+(a[6]-a[7]);

  com.SendData(@a[0], 9);
end;

procedure TRCXLink.RCXFunction(bOut: byte; out bIn: byte; iTimeOut: integer = 2000);
var
  tm1,tm2: cardinal;
label
  retry, wait_for_message;
begin
  tm1 := GetTickCount;

  DrainINputBuffer;  


retry:
  SendLegoMessage(bOut);

wait_for_message:
  if not GetLegoMessage(bIn) then begin
    tm2 := GetTickCount;
    if tm2 < tm1  then
      tm1 := tm2;

    if (tm2-tm1)> iTimeOut then
      goto retry;

    goto wait_for_message;
  end;


end;


procedure TRCXLink.RCXProcedure(bOUT: byte; iTimeOut: integer);
var
  b: byte;
begin
  RCXFunction(bOUt, b, iTimeOUt);
  

end;

end.
