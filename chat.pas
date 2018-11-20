unit chat;

interface


uses
  systemx, typex, SimpleReliableUDP, betterobject, sharedobject, sysutils;

type
  TChatter = class(TSharedObject)
  public
    endp: TREliableUDPEndpoint;
  end;
  TChatEvent = packed record
  private
    function GetWho: string;
    procedure SetWho(value: string);
    function GetWhat: string;
    procedure SetWhat(value: string);
  public
    flags: ni;
    time: TDateTime;
    Fwho: array[0..64] of byte;
    Fwhat: array[0..255] of byte;
    procedure Init;
    property Who: string read GetWho write SetWho;
    property What: string read GetWhat write SetWhat;

  end;


implementation

{ TChatEvent }

function TChatEvent.GetWhat: string;
begin
  result := PChar(@FWhat[0]);
end;

function TChatEvent.GetWho: string;
begin
  result := PChar(@FWho[0]);
end;

procedure TChatEvent.Init;
begin
  inherited;
  FillMem(pbyte(@self), sizeof(self),0);
  time := now;
end;

procedure TChatEvent.SetWhat(value: string);
begin
  movemem32(@FWhat[0], @value[strz], (length(value)+1)*sizeof(char));
end;

procedure TChatEvent.SetWho(value: string);
begin
  movemem32(@FWho[0], @value[strz], (length(value)+1)*sizeof(char));
end;

end.
