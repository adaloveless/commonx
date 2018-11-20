unit PacketHelpers_TerrainData;

interface

uses
  terraindata, packet;

procedure WriteTTileDataToPacket(packet: TRDTPPacket; td: TTileData);
procedure GetTTileDataFromPacket(packet: TRDTPPacket; out td: TTileData);



implementation

procedure WriteTTileDataToPacket(packet: TRDTPPacket; td: TTileData);
begin
  packet.AddInt(td.TerrainType);
  packet.AddDouble(td.Elevation);
end;
procedure GetTTileDataFromPacket(packet: TRDTPPacket; out td: TTileData);
begin
  td.TerrainType := packet.SeqRead;
  td.Elevation := packet.SeqRead;
end;




end.
