unit RelayControl;

interface

uses
  typex, systemx, latchcontrol, SimpleCommPort, sysutils, stringx;

type
  TRelayControl = class(TAbstractLatchControl)
  private
    Fport: string;
    FBoardID: string;
    procedure SetPort(const Value: string);
  protected
    cp: TSimpleCommPortConnection;
  public
    constructor CReate;override;
    procedure Init;override;
    procedure Detach;override;
    procedure SetValue(b: byte);override;
    procedure Biton(bit: byte);override;
    procedure BitOff(bit: byte);override;
    property Port: string read Fport write SetPort;
    function IsFunctional: boolean;
    function IsBoard(alpha: string): boolean;
    property BoardID: string read FBoardID write FBOardID;
  end;


implementation

{ TRelayControl }

procedure TRelayControl.BitOff(bit: byte);
begin
  inherited;
  lock;
  try
    if not cp.Connected then
      cp.connect;
    cp.SendString(BoardID+'L'+inttostr(bit+1)+#13);
  finally
    Unlock;
  end;
end;

procedure TRelayControl.Biton(bit: byte);
begin
  inherited;
  Lock;
  try
    if not cp.Connected then
      cp.connect;
    cp.SendString(BoardID+'H'+inttostr(bit+1)+#13);
  finally
    Unlock;
  end;
end;

constructor TRelayControl.CReate;
begin
  inherited;
  cp := TSimpleCommPOrtConnection.create;

end;

procedure TRelayControl.Detach;
begin
  inherited;

end;

procedure TRelayControl.Init;
begin
  inherited;
  BoardID := 'A';
end;

function TRelayControl.IsBoard(alpha: string): boolean;
var
  a: array[0..31] of byte;
  r: integer;
  t: ni;
begin
  Lock;
  try
    result := false;
    if not cp.connected then
      cp.connect;
    cp.SendString(alpha+'!0'#13);
    sleep(50);

    r := cp.ReadData(@a[0], sizeof(a), false, 100);
    if r > 0 then begin
      result := false;
      for t:= 0 to r-1 do begin
        if a[t] = 13 then
          result := true;
      end;

      result := result and ((a[0] >= 48) or (a[0] <= 57));
    end;

    if result then
      BoardID := alpha;
  finally
    Unlock;
  end;
end;

function TRelayControl.IsFunctional: boolean;
var
  a: array[0..63] of byte;
  r: integer;
  t: ni;
begin
  Lock;
  try
    result := false;
    cp.SendString(BoardID+'R1'#13);
    sleep(50);

    r := cp.ReadData(@a[0], sizeof(a), false, 100);
    if r > 0 then begin
      result := false;
      for t:= 0 to r-1 do begin
        if a[t] = 13 then
          result := true;
      end;

      result := result and ((a[0] >= 48) or (a[0] <= 57));
    end;
  finally
    Unlock;
  end;

end;

procedure TRelayControl.SetPort(const Value: string);
begin
  if assigned(cp) then begin
    if cp.Connected then
      cp.Disconnect;
    cp.free;
    cp := nil;
  end;

  if not assigned(cp) then begin
    cp := TSimpleCommPortConnection.Create;
    Fport := Value;
    cp.EndPoint := Value;
    cp.Connect;
    cp.BaudRate := 9600;
  end;


end;

procedure TRelayControl.SetValue(b: byte);
begin
  inherited;
  Lock;
  try
    if not cp.Connected then
      cp.connect;
    cp.SendString(BoardID+'W'+inttostr(b)+#13);
  finally
    Unlock;
  end;


end;

end.
