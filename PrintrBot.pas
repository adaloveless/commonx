unit PrintrBot;

interface

uses
  debug, simplequeue, simplecommport, commandprocessor, typex, systemx,stringx, betterobject, ringbuffer, signals, classes, sharedobject, sysutils, beeper, geometry, math, ApplicationParams,
{$IFDEF DO_CAM}
  webcam,
{$ENDIF}
  managedthread;


type
  localfloat = double;
  TMoveFlags = set of (mfx,mfy,mfz,mfslowest, mfSlow, mfFast);
const
  mfAll: TMoveFlags = [mfx,mfy,mfz];
  mfAllFast: TMoveFlags = [mfx,mfy,mfz, mfFast];
  mfAllSlow: TMoveFlags = [mfx,mfy,mfz, mfSlow];
  NUTY1: array of localfloat = [68,76,83.5,91.1, 98,104.5];
  NUTY2: array of localfloat = [68,76,84,92, 100,108];
  NUTZ: array of localfloat = [47.5,47,46.5,46,45.75,45];

const
  SAFE_HEIGHT = 59.0;
  QUICK_SAFE_HEIGHT = 41.0;
  FINGER_Z = 0;
  FINGER_X = 0;

type
  TBotVec = record
    x,y,z: double;
  end;
  TPrintrBot = class;//forward

  TBotCommand = class(TQueueItem)
  public
    bot: TPrintrBot;
    procedure init;override;
  end;

  Tcmd_Home = class(TBotCommand)
  public
    procedure DoExecute;override;
  end;

  Tcmd_HomeXY = class(TBotCommand)
  public
    procedure DoExecute;override;
  end;



  Tcmd_CheckCalibrated = class(TBotCommand)
  public
    procedure DoExecute;override;
  end;


  Tcmd_Move = class(TBotCommand)
  public
    flags: TMoveFlags;
    xyz: TBotVec;
    procedure Init;override;
    procedure DoExecute;override;
  end;

  Tcmd_MoveRelative = class(TBotCommand)
  public
    flags: TMoveFlags;
    xyz: TBotVec;
    procedure Init;override;
    procedure DoExecute;override;
  end;


  Tcmd_Safeheight = class(TBotCommand)
  public
    procedure DoExecute;override;
  end;

  Tcmd_QuickSafeheight = class(TBotCommand)
  public
    procedure DoExecute;override;
  end;

  Tcmd_MetalCheck = class(TBotCommand)
  public
    metal: boolean;
    procedure Init;override;
    procedure DoExecute;override;
  end;

  Tcmd_WalkString = class(TBotCommand)
  public
    string_number: ni;
    fromnut: localfloat;
    height_from_string: localfloat;
    procedure DoExecute;override;
  end;

  TPrintrBot = class(TSharedObject)
  private
    FPort: string;
    rbIn: TRingBuffer;

    sCurrentLine: string;
    FWaitingforack: boolean;
    FMetal: boolean;
    FZ: localfloat;
    FX: localfloat;
    FY: localfloat;
    FOffX: localfloat;
    FOffY: localfloat;
    FOffZ: localfloat;

    procedure SetPort(const Value: string);
    procedure DrainIncoming;
    procedure CheckForData;
    procedure SetX(const Value: localfloat);
    procedure SetY(const Value: localfloat);
    procedure SetZ(const Value: localfloat);
    function GetHeightFromStringFromZ(iStringNumber: ni; z: localfloat): localfloat;
    function GetCalibrated: boolean;
    procedure SetCalibrated(const Value: boolean);
  public
{$IFDEF DO_CAM}
    cam: TCamThread;
{$ENDIF}
    cq: TSimpleQueue;
    slIncoming: TStringlist;
    conn: TSimpleCommPortConnection;
    BaudRate: ni;
    SafeHeightAchieved: boolean;
    Homed: boolean;
    constructor Create;override;
    destructor Destroy;override;
    property Calibrated: boolean read GetCalibrated write SetCalibrated;
    property Port: string read FPort write SetPort;
    procedure Open;

    procedure Close;
    procedure SendCommand(s: string);
    function IsLineCOmplete: boolean;
    function ProcessIncomingMessage: boolean;
    procedure ProcessIncomingMessages;
    property WaitingForack: boolean read FWaitingforack write FWaitingforack;
    property Metal: boolean read FMetal;
    procedure Home;
    procedure HomeXY;
    procedure WAitForAck;
    function TryWaitForAck: boolean;
    procedure CheckForMetal;
    property X: localfloat read FX write SetX;
    property Y: localfloat read FY write SetY;
    property Z: localfloat read FZ write SetZ;
    procedure Calibrate;
    procedure MoveRelative(x,y,z: localfloat);
    procedure MoveAbsolute(x,y,z: localfloat; flags: TMoveFlags; bForCalibration: boolean = false);
    procedure SafeHeight;
    procedure QuickSafeHeight;
    procedure Dwell;
    procedure StringWalk(iStringNumber: ni; fromnut: localfloat; height_from_string: localfloat);
    function GetStringPOsition(iStringNumber: ni; fromnut: localfloat; height_from_string: localfloat; out outofbounds: boolean): TNativeVector4;
    function Begin_Home: Tcmd_Home;
    procedure Begin_MoveAbsolute(x,y,z: localfloat; flags: TMoveFlags);overload;
    procedure Begin_MoveAbsolute(xyz: TBotVec; flags: TMoveFlags);overload;
    procedure Begin_MoveRelative(x,y,z: localfloat; flags: TMoveFlags);
    procedure End_Command(c: TBotCommand);
    procedure End_Home(c: TBotCommand);
    function Backgroundcommands: boolean;
    procedure WaitForCommands;
    procedure Begin_StringWalk(iStringNumber: ni; fromnut: localfloat; height_from_string: localfloat);
  end;



implementation

{ TPrintrBot }

function TPrintrBot.Backgroundcommands: boolean;
begin
  result := (cq.estimated_queue_size+cq.estimated_backlog_size) > 0;
end;

function TPrintrBot.Begin_Home: Tcmd_Home;
begin
  result := Tcmd_Home.create;
  result.bot := self;
  cq.AddItem(result);
end;

procedure TPrintrBot.Begin_MoveAbsolute(x, y, z: localfloat; flags: TMoveFlags);
var
  c: Tcmd_Move;
begin
  c := Tcmd_Move.create;
  c.bot := self;
  c.xyz.x := x;
  c.xyz.y := y;
  c.xyz.z := z;
  c.flags := flags;
  cq.AddItem(c);

end;

procedure TPrintrBot.Begin_MoveAbsolute(xyz: TBotVec; flags: TMoveFlags);
begin
  Begin_moveAbsolute(xyz.x, xyz.y,xyz.z, flags);

end;

procedure TPrintrBot.Begin_MoveRelative(x, y, z: localfloat; flags: TMoveFlags);
var
  c: Tcmd_MoveRelative;
begin
  c := Tcmd_MoveRelative.create;
  c.bot := self;
  c.xyz.x := x;
  c.xyz.y := y;
  c.xyz.z := z;
  c.flags := flags;
  cq.AddItem(c);
end;

procedure TPrintrBot.Begin_StringWalk(iStringNumber: ni; fromnut,
  height_from_string: localfloat);
var
  c: Tcmd_WAlkString;
begin
  c := TCmd_WalkString.create;
  c.string_number := iStringNumber;
  c.fromnut := fromnut;
  c.height_from_string := height_from_string;
  c.bot := self;
  cq.AddItem(c);

end;

procedure TPrintrBot.Calibrate;
begin
  SafeHeight;
  Home;
  SafeHeight;

end;

procedure TPrintrBot.CheckForData;
var
  iRead: ni;
begin
  if conn.WaitForData(1) then begin
    iREad := conn.ReadData(rbIn.NextWritePointer, rbIn.MaxNextWrite, false, 0);
    rbIn.MoveWriteHeadPointer(iRead);
  end;
end;

procedure TPrintrBot.CheckForMetal;
begin
  WaitForAck;
  SendCommand('G31');
  WaitForAck;

end;


procedure TPrintrBot.Close;
begin
  conn.Disconnect;
end;

constructor TPrintrBot.Create;
begin
  inherited;
  conn := TSimpleCommPortConnection.create;
  rbIn := TRingBuffer.create;
  slIncoming := TStringlist.create;
  cq := TPM.Needthread<TSimpleQueue>(nil);
  cq.start;
{$IFDEF DO_CAM}
  cam := TPM.NeedThread<TCamThread>(nil);
  cam.DeviceIndex := 0;
{$ENDIF}

end;

destructor TPrintrBot.Destroy;
begin
  Close;
  conn.free;
  rbIn.free;
  slIncoming.free;
  cq.WaitForAll;
  cq.Stop;
  cq.WaitForFinish;
  cq.free;
  inherited;
end;

procedure TPrintrBot.DrainINcoming;
var
  s: ansistring;
  s1: string;
  iSz: ni;
begin
  iSZ := rbIn.AvailableDataSize;
  if iSz = 0 then
    exit;
  setlength(s, iSZ);
  rbIn.GuaranteeGetData(@s[STRZ], iSZ);
  sCurrentLine := sCurrentLine + s;
  while IsLineComplete do begin
    splitString(sCurrentLine, #10, s1,sCurrentLine);
    slIncoming.add(s1);
  end;
end;

procedure TPrintrBot.Dwell;
begin
  SendCommand('G4 P0');
  WaitForAck;
  //beeper.beep(1000,100);
end;

procedure TPrintrBot.End_Command(c: TBotCommand);
begin
  c.WAitFor;
end;

procedure TPrintrBot.End_Home(c: TBotCommand);
begin
  c.WAitFor;
end;

function TPrintrBot.GetCalibrated: boolean;
begin
  result := SafeHeightAchieved and Homed;
end;

function TPrintrBot.GetHeightFromStringFromZ(iStringNumber: ni; z: localfloat): localfloat;
begin
  result := (z-NUTZ[iStringNumber])-FINGER_Z;
end;

function TPrintrBot.GetStringPOsition(iStringNumber: ni; fromnut,
  height_from_string: localfloat; out outofbounds: boolean): TNativeVector4;
var
  xx,yy,zz: localfloat;
const
  surface_string : array of localfloat = [45,45,44,43,43,43];
begin
  outofbounds := false;
  xx := (fromnut * 150)+FINGER_X;
  yy := Interpolate(fromnut, NUTY1[iStringNumber], NUTY2[iStringNumber]);
  if height_from_string < -5 then
    height_from_string := -5;
  zz := NUTZ[iStringNumber]+height_from_string+FINGER_Z;

  //make sure we're clear of the nut
  if xx < 9 then begin
    if zz < 47.5 then
      zz := 47.5;
  end;

  if (xx >32) and (xx <44) then begin
    if zz < (surface_string[iStringNumber] +1) then begin
      zz := surface_string[iStringNumber] +1;
      outofbounds := true;
    end;
  end;

  if (xx >63) and (xx <74) then begin
    if zz < (surface_string[iStringNumber] +1) then begin
      zz := surface_string[iStringNumber] +1;
      outofbounds := true;
    end;
  end;

  if (xx >91) and (xx <107) then begin
    if zz < (surface_string[iStringNumber] +1) then begin
      zz := surface_string[iStringNumber] +1;
      outofbounds := true;
    end;
  end;

  if (xx >118) and (xx <134) then begin
    if zz < (surface_string[iStringNumber] +1) then begin
      zz := surface_string[iStringNumber] +1;
      outofbounds := true;
    end;
  end;

  if (xx >142) and (xx <9999) then begin
    if zz < (surface_string[iStringNumber] +1) then begin
      zz := surface_string[iStringNumber] +1;
      outofbounds := true;
    end;
  end;

  if zz < surface_string[iStringNumber] then begin
    zz := surface_string[iStringNumber];
    outofbounds := true;
  end;

  if xx > 142 then begin
    xx := 142;
    outofbounds:= true;
  end;

  result.x := xx;
  result.y := yy;
  result.z := zz;
  result.w := 0;

end;

procedure TPrintrBot.Home;
begin
  ProcessIncomingMessages;

  SendCommand('G1 Z64 F3000');
  WaitForAck;
  MoveAbsolute(0,0,0, [mfx, mfy, mfFast], true);
//  SendCommand('G28 X0');
//  WAitForAck;
  SendCommand('G28 Y0');
  WaitForAck;
  SendCommand('G28 X0');
  WaitForAck;
  SendCommand('G28 Z0');
  WaitForAck;
  fx := 0;
  fY := 0;
  fZ := 0;

  homed := true;

end;

procedure TPrintrBot.HomeXY;
begin

  ProcessIncomingMessages;
  SafeHeight;
  MoveAbsolute(0,0,0, [mfx, mfy, mfFast], true);
  SendCommand('G28 Y0');
  WaitForAck;
  SendCommand('G28 X0');
  WaitForAck;
  fx := 0;
  fY := 0;

end;

function TPrintrBot.IsLineCOmplete: boolean;
var
  t: ni;
begin
  result := zpos(#10, sCurrentLIne) >=0;
end;

procedure TPrintrBot.MoveAbsolute(x, y, z: localfloat; flags: TMoveFlags; bForCalibration: boolean = false );
var
  s: string;
  xx,yy,zz: localfloat;
begin
  if not bForCalibration then
    if not calibrated then calibrate;
  WaitForAck;
  Dwell;

  APBegin;
  try
    UPBegin;
    try
      xx := x + APGet('bot.offset.x',0.0)+UPGet('bot.offset.x',0.0);
      yy := y + APGet('bot.offset.y',0.0)+UPGet('bot.offset.y',0.0);
      zz := z + APGet('bot.offset.z',0.0)+UPGet('bot.offset.z',0.0);
    finally
      UPEnd;
    end;
  finally
    APend;
  end;

  s := 'G0';
  if mfx in flags then begin
    s := s+' X'+floattostr(xx);
    FX := x;
  end;
  if mfy in flags then begin
    s := s+' Y'+floattostr(yy);
    Fy := y;
  end;

  if mfz in flags then begin
    s := s+' Z'+floattostr(zZ);
    Fz := z;
  end;

  if mfSlowest in flags then begin
    s := s+ ' F100';
  end else
  if mfSlow in flags then begin
    s := s+ ' F1000';
  end else
  if mfFast in flags then begin
    s := s+ ' F3000';
  end else
    s := s+ ' F3000';



  SendCommand(s);





end;

procedure TPrintrBot.MoveRelative(x, y, z: localfloat);
begin
  if not Calibrated then
    Calibrate;
  MoveAbsolute(self.x+x, self.y+y, self.Z+z, mfAll);

end;

procedure TPrintrBot.Open;
begin
  conn.EndPoint := port;
  conn.Connect;
  if baudrate = 0 then
    conn.BaudRate := 250000
  else
    conn.baudrate := 115200;


{$IFDEF DO_CAM}
  cam.Start;
{$ENDIF}

end;

function TPrintrBot.ProcessIncomingMessage: boolean;
var
  s: string;
  s1,s2: string;
begin
  result := false;
  s := slIncoming[0];
  slincoming.Delete(0);
  if s = '' then begin
    result := false;
    exit;
  end;


  if s = 'ok' then begin
    WAitingForAck := false;
    result := true;
  end;

  if splitstring(s, ' ', s1,s2) then begin
    if s1 = 'Z_STOP:' then begin
      FMEtal := s2 = '0';
      result := true;
    end;
  end;







end;

procedure TPrintrBot.ProcessIncomingMessages;
var
  s: string;
begin
  CheckForData;
  DrainIncoming;
  while slIncoming.count > 0 do begin
    processincomingmessage;


  end;

end;

procedure TPrintrBot.QuickSafeHeight;
begin
  MoveAbsolute(0,0,QUiCK_SAFE_HEIGHT, [mfZ, mfFast], true);
end;

procedure TPrintrBot.SafeHeight;
begin
  MoveAbsolute(0,0,SAFE_HEIGHT, [mfZ, mfFast], true);
  SafeHeightAchieved := true;
end;

procedure TPrintrBot.SendCommand(s: string);
begin
  Lock;
  try
    WaitForAck;
    WaitingForAck := true;
    Debug.Log(s);
    conn.SendString(s+#10);
  finally
    Unlock;
  end;
end;

procedure TPrintrBot.SetCalibrated(const Value: boolean);
begin
  SafeHeightAchieved := value;
  Homed := value;
end;

procedure TPrintrBot.SetPort(const Value: string);
var
  bWasConnected: boolean;
begin
  IF VALUE <> FPort then begin
    if assigned(conn) then begin
      conn.Disconnect;
    end;
  end;

  FPort := Value;
end;

procedure TPrintrBot.SetX(const Value: localfloat);
begin
  FX := Value;
end;

procedure TPrintrBot.SetY(const Value: localfloat);
begin
  FY := Value;
end;

procedure TPrintrBot.SetZ(const Value: localfloat);
begin
  FZ := Value;
end;

procedure TPrintrBot.StringWalk(iStringNumber: ni; fromnut,
  height_from_string: localfloat);
var
  v: TNativeVector4;
  b: boolean;
begin
  v := GetStringPOsition(iStringNumber, fromnut, height_from_string,b);
  if (z < NUTZ[iStringNumber]+FINGER_Z) then begin
    if ((v.x <> x) or (v.y<>y)) then
      MoveAbsolute(0,0,48, [mfZ, mfFast]);
  end else begin
    if height_from_string <=0 then
      MoveAbsolute(v.x,v.y,NUTZ[iStringNumber]+FINGER_Z, mfAllFast);
  end;
  MoveAbsolute(v.x,v.y,v.z, mfAllSlow);

end;

procedure TPrintrBot.WAitForAck;
begin
  while WaitingForAck do begin
    ProcessIncomingMessages;
    sleep(10);
  end;

end;

function TPrintrBot.TryWaitForAck: boolean;
begin
  ProcessIncomingMessages;
  result := WaitingForAck;
end;

procedure TPrintrBot.WaitForCommands;
begin
  cq.WaitForAll;
end;

{ Tcmd_Home }

procedure Tcmd_Home.DoExecute;
begin
  inherited;
  bot.Home;
end;

{ TBotCommand }

procedure TBotCommand.init;
begin
  inherited;
  autodestroy := true;
end;

{ Tcmd_Move }

procedure Tcmd_Move.DoExecute;
begin
  inherited;
  bot.MoveAbsolute(xyz.x,xyz.y,xyz.z, flags);
  if not autodestroy then
    bot.MoveAbsolute(xyz.x,xyz.y,xyz.z, flags);//this is to prevent early completion of the command

//  bot.WAitForAck;
end;

procedure Tcmd_Move.Init;
begin
  inherited;
  flags := mfAllFast;
end;

{ Tcmd_MetalCheck }

procedure Tcmd_MetalCheck.DoExecute;
begin
  inherited;
  bot.CheckForMetal;
  metal := bot.Metal;
end;

procedure Tcmd_MetalCheck.Init;
begin
  inherited;
  autodestroy := false;
end;

{ Tcmd_WAlkString }

procedure Tcmd_WAlkString.DoExecute;
begin
  inherited;
  bot.StringWalk(string_number, fromnut, height_from_string);
  bot.Dwell;
end;

{ Tcmd_Safeheight }

procedure Tcmd_Safeheight.DoExecute;
begin
  inherited;
  bot.SafeHeight;
end;

{ Tcmd_CheckCalibrated }

procedure Tcmd_CheckCalibrated.DoExecute;
begin
  inherited;
  if not bot.calibrated then
    bot.calibrate;
end;

{ Tcmd_HomeXY }

procedure Tcmd_HomeXY.DoExecute;
begin
  inherited;
  bot.HomeXY;
end;

{ Tcmd_Relative }

procedure Tcmd_MoveRelative.DoExecute;
begin
  inherited;
  bot.MoveRelative(xyz.x,xyz.y,xyz.z);
  if not self.AutoDestroy then
    bot.MoveRelative(0,0,0);
end;

procedure Tcmd_MoveRelative.Init;
begin
  inherited;
  flags := mfAllFast;
  xyz.x := 0;
  xyz.y := 0;
  xyz.z := 0;
end;

{ Tcmd_QuickSafeheight }

procedure Tcmd_QuickSafeheight.DoExecute;
begin
  inherited;
  bot.QuickSafeHeight;
end;

end.
