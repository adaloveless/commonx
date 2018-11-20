unit LatchControl_FTDI;

interface


uses
  typex, systemx, latchcontrol, ftd2xx, tickcount;

type
  TLatchControl = class(TAbstractLatchControl)
  PRIVATE
    h: ni;
    remembered_value: byte;
  public
    delay: ticker;
    devidx: ni;
    procedure Init;override;
    procedure BeforeDestruction;override;
    procedure SetValue(b: byte);override;
    procedure Biton(bit: byte);override;
    procedure BitOff(bit: byte);override;
    constructor Create(idx: ni);reintroduce;virtual;
  end;



implementation


procedure TLatchControl.BeforeDestruction;
begin
  inherited;
  ftd2xx.FT_Close(h);
end;

procedure TLatchControl.BitOff(bit: byte);
begin
  lock;
  try
    SEtValue(remembered_value and (not (1 shl bit)));
  finally
    unlock;
  end;
end;

procedure TLatchControl.Biton(bit: byte);
begin
  lock;
  try
    SEtValue(remembered_value or (1 shl bit));
  finally
    unlock;
  end;
end;

constructor TLatchControl.Create(idx: ni);
begin
  inherited Create;
  self.devidx := idx;
  ftd2xx.FT_Open(devidx, @h);
  FT_SetBaudRate(h, 2400);
  FT_SetBitMode(h, $ff, $ff);
  delay := 0;

end;

procedure TLatchControl.Init;
begin
  inherited;
end;

procedure TLatchControl.SetValue(b: byte);
var
  iWritten: int64;
begin
  lock;
  try
    iWritten := 0;
    FT_Write(h, @b, 1, @iWritten);
    remembered_value := b;
    if delay > 0 then
      sleep(delay);
  finally
    unlock;
  end;
end;



end.
