unit RDTPSpamPacketHelpers;

interface

uses packet;

type

  TSpamActions = integer;
const
  saHandled = 1;
  saMark = 2;
  saBlacklist = 4;
  saDivert = 8;
  saDelete = 16;
const
  saGeorgeBush = saDivert+saMark+saBlacklist+saDelete;
  saCritical = saDivert+saMark+saBlacklist;
  saModerate = saMark;

procedure WriteTSpamActionsToPacket(packet: TRDTPPacket; act: TSpamActions);
procedure ReadTSpamACtionsFromPAcket(Packet: TRDTPPacket; out act: TSpamACtions);


implementation

procedure WriteTSpamActionsToPacket(packet: TRDTPPacket; act: TSpamActions);
var
  c: cardinal;
begin
  packet.Addlong(act);

end;

procedure ReadTSpamACtionsFromPAcket(Packet: TRDTPPacket; out act: TSpamACtions);
begin
  act := packet.SeqRead;
end;


end.
