unit CyMiniProg;

interface



uses
  debug, betterobject, PSocProgrammerCOMLib_TLB, classes, variants, sysutils, systemx, stringx, typex;

type
  TCyMiniProg = class(TBetterObject)
  private
    prog: IPSoCProgrammerCOM_Object;
    Fport: string;
    FPower: boolean;
    procedure Assert(context: string; err: widestring);
    function IsChipNotProtected: boolean;
    procedure ProgramFlash;
    procedure SetPower(const Value: boolean);
  public

    loadedhexsz: integer;
    isopen: boolean;
    status: string;
    constructor Create; override;

    function GetPorts: IHolder<TStringList>;

    property Port: string read Fport write FPort;
    procedure SelectPort(idx: ni);
    procedure OpenPort;
    procedure ClosePort;

    procedure ProgTest;
    procedure InitializePort;
    property Power: boolean read FPower write SetPower;
    procedure ProgramPort;
    procedure EraseBoard;
    procedure ReadHexFile(sFile: string);
    procedure SetLed(idx: ni; state: ni);
    procedure Identify(bOn: boolean);
    procedure SetVoltage(v: single);

  end;

implementation
{ TCyMiniProg }

procedure TCyMiniProg.Assert(context: string; err: widestring);
begin
  Status := context+' '+err;
  Debug.Log(self, status);
  if err <> '' then
    raise ECritical.create('In '+context+' error: '+err);
end;

procedure TCyMiniProg.ClosePort;
var
  err: widestring;
begin
  if not isopen then
    exit;

//Identify(false);
  prog.ClosePort(err);
  assert('ClosePort', err);
  isOpen := false;
end;

constructor TCyMiniProg.Create;
begin
  inherited;
  prog := CoPSoCProgrammerCOM_Object.Create;

end;

procedure TCyMiniProg.EraseBoard;
var
  err: widestring;
begin
  prog.SetAcquireMode('Reset', err);
  assert('SetAcquireMode', err);


  prog.DAP_Acquire(err);
  assert('DAP_Acquire', err);

  prog.PSoC4_EraseAll(err);
  assert('PSoC4_EraseAll', err);
end;

function TCyMiniProg.GetPorts: IHolder<TStringList>;
var
  olev: OleVariant;
  err: widestring;
  t,l,h: ni;

begin
  result := THolder<TSTringlist>.create;
  result.o := TStringlist.create;
  self.prog.GetPorts(olev, err);
  l := VarArrayLowBound(olev, 1);
  h := VarArrayHighBound(olev, 1);

  for t:= l to h do begin
    result.o.add(vartostr(olev[t]));
  end;
end;

procedure TCyMiniProg.Identify(bOn: boolean);
begin
  if bOn then begin
    setled(0, 1);
    setled(1, 2);
    setled(2, 1);
    setled(3, 2);
  end else begin
    setled(0, 0);
    setled(1, 0);
    setled(2, 0);
    setled(3, 0);

  end;


end;

procedure TCyMiniProg.InitializePort;
var
  err: widestring;
begin
  prog.SetPowerVoltage('3.3', err);
  assert('SetPowerVoltage', err);

//  prog.PowerOn(err);
//  assert('PowerOn', err);

  prog.SetProtocol(SWD, err);
  assert('SetProtocol', err);

  prog.SetProtocolConnector(1,err);
  assert('SetProtocolConnector', err);

  prog.SetProtocolClock(FREQ_03_0,err);
  assert('SetProtocolClock', err);


end;

procedure TCyMiniProg.OpenPort;
var
  err: widestring;
begin
  prog.OpenPort(port,err);
  assert('OpenPort', err);
  isopen := true;

end;


function TCyMiniProg.IsChipNotProtected(): boolean;
var
  flashProt: olevariant;
  chipProt: olevariant;
  res: integer;
  err: widestring;
begin
  flashProt := 0;
  prog.PSoC4_ReadProtection(flashprot, chipprot, err);
  assert('ReadProtection', err);
  if chipprot[0] = $00 then
    Assert('CHIP PROTECT!', 'Transition to virgin unallowed!  Will Destroy Chip!');

end;


procedure TCyMiniProg.ProgramFlash;
var
  rowstotal, rowsperflash, rowsize: integer;
  err: widestring;
  i: ni;
  vr: integer;
begin
  prog.PSoC4_GetFlashInfo(rowsPerFlash, rowSize, err);
  assert('PSoC4_GetFlashInfo', err);

  rowstotal := loadedhexsz div rowsize;

  SetLed(2,2);

  for i := 0 to rowstotal-1 do begin
    prog.PSoC4_ProgramRowFromHex(i, err);
    assert('PSoC4_ProgramRowFromHex('+inttostr(i)+')', err);
  end;
  SetLed(2,3);

  SetLed(3,2);
  for i := 0 to rowstotal-1 do begin
    prog.PSoC4_VerifyRowFromHex(i, vr, err);
    assert('PSoC4_VerifyRowFromHex('+inttostr(i)+')', err);
  end;
  SetLed(3,3);

  prog.PSoC4_ProtectAll(err);
  assert('PSoC4_ProtectAll', err);

  prog.PSoC4_VerifyProtect(err);
  assert('PSoC4_VerifyProtect', err);





end;

procedure TCyMiniProg.ProgramPort;
var
  err: widestring;
  data: olevariant;
  cs: integer;
begin
  prog.HEX_ReadChipProtection(data, err);
  assert('HEX_ReadChipProtection', err);
  if data[0] = $00 then
    Assert('CHIP PROTECT!', 'Transition to virgin unallowed!  Will Destroy Chip!');

  prog.SetAcquireMode('Reset', err);
  assert('SetAcquireMode', err);


  prog.DAP_Acquire(err);
  assert('DAP_Acquire', err);


  SetLed(1,2);
  prog.PSoC4_EraseAll(err);
  assert('PSoC4_EraseAll', err);
  SetLed(1,3);


  prog.PSoC4_CheckSum($8000, cs, err);

  ProgramFlash;

  prog.DAP_ReleaseChip(err);
  assert('DAP_ReleaseChip', err);




end;

procedure TCyMiniProg.ProgTest;
begin
  SelectPort(0);
  OpenPort;
  try
    InitializePort;
    ReadHexFile('g:\my drive\zivix_shared_engineering\firmware\Alpha\Jamstik7\TEST.hex');
    ProgramPort;

  finally
    ClosePort;
  end;


end;

procedure TCyMiniProg.ReadHexFile(sFile: string);
var
  err: widestring;
begin
  if not fileexists(sFile) then
    assert('fileexists', sFile+' does not exist.');
  prog.ReadHexFile(sfile, loadedhexsz, err);
  assert('ReadHexFile', err);
end;

procedure TCyMiniProg.SelectPort(idx: ni);
var
  sl: IHolder<TStringList>;
begin
  if idx < 0 then
    exit;

  if isOpen then
    ClosePort;



  sl := Self.GetPorts;
  if idx < sl.o.count then
    Port := sl.o[idx]
  else
    raise ECritical.create('port '+idx.tostring+' was not found');

  openPort;

end;

procedure TCyMiniProg.SetLed(idx, state: ni);
var
  err: widestring;
begin
  prog.ProgrammerLedState(idx, state, err);
  assert('ProgrammerLedState', err);
end;

procedure TCyMiniProg.SetPower(const Value: boolean);
var
  err: widestring;
begin
  FPower := Value;
  OpenPort;
  if value then begin
    SetVoltage(3.3);
    prog.PowerOn(err);
    assert('PowerOn',err);
    setled(1, 3);
  end else begin
    prog.PowerOff(err);
    assert('PowerOff', err);
    setled(1, 0);
  end;


end;

procedure TCyMiniProg.SetVoltage(v: single);
var
  err: widestring;
begin
  if v = 0.0 then
    prog.SetPowerVoltage('External', err)
  else
    prog.SetPowerVoltage(floattostr(v),err);
  assert('SetPowerVoltage', err);

end;

end.
