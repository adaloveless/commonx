unit VideoMarshall;

interface

uses
  packet, sysutils, variants, dir, dirfile, classes, systemx;

type
  TPlaybackInfo = packed record
    Position: real;
    Length: real;
    Playing: boolean;
    PlayingForScrub: boolean;
    PlaystateAfterPrepare: integer;
    LastPLayState: integer;
  end;

procedure GetTPlaybackInfoFromPacket(packet: TRDTPPacket; out res: TPlaybackInfo);
procedure WriteTPlaybackInfoToPacket(packet: TRDTPPacket; pi: TPlaybackInfo);







implementation


procedure GetTPlaybackInfoFromPacket(packet: TRDTPPacket; out res: TPlaybackInfo);
var
  p: PByte;
  iLen: int64;
begin
  p := packet.SeqReadBytes(iLen);

  if iLen <> sizeof(res) then
    raise Exception.Create('TPlaybackinfo size does not match size in packet');

  MoveMem32(@res, p, iLen);

  FreeMem(p);


end;

procedure WriteTPlaybackInfoToPacket(packet: TRDTPPacket; pi: TPlaybackInfo);
var
  p: PByte;
  iLen: integer;
begin
  packet.AddBytes(Pbyte(@pi), sizeof(pi));

end;



end.
