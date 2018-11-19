unit NetLogPacket;

{$I DelphiDefs.inc}

interface


uses
{$IFDEF NEED_FAKE_ANSISTRING}
  ios.stringx.iosansi,
{$ENDIF}
  systemx;

type
  TPeerInfo = record
    PeerIP: string;
    PeerPort: word;
  end;

  TLogPacketHeader = packed record
    hi: array[0..3] of byte;
    procedure Init;
    procedure SetLogL;
    procedure SetLogA;
    procedure SetPing;
    procedure SetPong;
    function IsLogL: boolean;
    function IsLogA: boolean;
    function IsPing: boolean;
    function IsPong: boolean;
    function IsRESET: boolean;
    function IsLogO: boolean;
  end;
  PLogPacketHeader = ^TLogPacketHeader;

  TLogAckPacket = packed record
    h: TLogPacketHeader;
    ackLevel: int64;
  end;

implementation


procedure TLogPacketHeader.Init;
begin
  fillmem(@self, sizeof(self), 0);
end;

function TLogPacketHeader.IsLogL: boolean;
begin
  result := true;
  if hi[0] <> ord(ansichar('L')) then exit(false);
  if hi[1] <> ord(ansichar('O')) then exit(false);
  if hi[2] <> ord(ansichar('G')) then exit(false);
  if hi[3] <> ord(ansichar('L')) then exit(false);

end;

function TLogPacketHeader.IsLogO: boolean;
begin
  //out of order signal
  result := true;
  if hi[0] <> ord(ansichar('L')) then exit(false);
  if hi[1] <> ord(ansichar('O')) then exit(false);
  if hi[2] <> ord(ansichar('G')) then exit(false);
  if hi[3] <> ord(ansichar('O')) then exit(false);
end;

function TLogPacketHeader.IsPing: boolean;
begin
  result := true;
  if hi[0] <> ord(ansichar('P')) then exit(false);
  if hi[1] <> ord(ansichar('I')) then exit(false);
  if hi[2] <> ord(ansichar('N')) then exit(false);
  if hi[3] <> ord(ansichar('G')) then exit(false);
end;

function TLogPacketHeader.IsPong: boolean;
begin
  result := true;
  if hi[0] <> ord(ansichar('P')) then exit(false);
  if hi[1] <> ord(ansichar('O')) then exit(false);
  if hi[2] <> ord(ansichar('N')) then exit(false);
  if hi[3] <> ord(ansichar('G')) then exit(false);

end;

function TLogPacketHeader.IsRESET: boolean;
begin
  result := true;
  if hi[0] <> ord(ansichar('R')) then exit(false);
  if hi[1] <> ord(ansichar('E')) then exit(false);
  if hi[2] <> ord(ansichar('S')) then exit(false);
  if hi[3] <> ord(ansichar('T')) then exit(false);
end;

function TLogPacketHeader.IsLogA: boolean;
begin
  result := true;
  if hi[0] <> ord(ansichar('L')) then exit(false);
  if hi[1] <> ord(ansichar('O')) then exit(false);
  if hi[2] <> ord(ansichar('G')) then exit(false);
  if hi[3] <> ord(ansichar('A')) then exit(false);
end;

procedure TLogPacketHeader.SetLogL;
begin
  hi[0] := ord(ansichar('L'));
  hi[1] := ord(ansichar('O'));
  hi[2] := ord(ansichar('G'));
  hi[3] := ord(ansichar('L'));
end;

procedure TLogPacketHeader.SetPing;
begin
  hi[0] := ord(ansichar('P'));
  hi[1] := ord(ansichar('I'));
  hi[2] := ord(ansichar('N'));
  hi[3] := ord(ansichar('G'));

end;

procedure TLogPacketHeader.SetPong;
begin
  hi[0] := ord(ansichar('P'));
  hi[1] := ord(ansichar('O'));
  hi[2] := ord(ansichar('N'));
  hi[3] := ord(ansichar('G'));

end;

procedure TLogPacketHeader.SEtLogA;
begin
  hi[0] := ord(ansichar('L'));
  hi[1] := ord(ansichar('O'));
  hi[2] := ord(ansichar('G'));
  hi[3] := ord(ansichar('A'));
end;


end.
