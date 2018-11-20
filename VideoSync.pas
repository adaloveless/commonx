unit VideoSync;

interface

uses
  classes, windows, mmsystem, typex, systemx, sysutils;

type
  TVideoSyncTrack = packed record
    Alpha: byte;
    Time: single;
    FrameRate: byte;
    TimeToOut: single;
    FileName: array[0..512] of ansichar;
    procedure SetFileName(value: string);
  end;

  TVideoSyncPacket = packed record
    SequenceNumber: int64;
    tr: array[0..1] of TVideoSyncTrack;
  end;


implementation

{ TVideoSyncPacket }


{ TVideoSyncTrack }

procedure TVideoSyncTrack.SetFileName(value: string);
var
  s: ansistring;
  p: PByte;
begin
  s := ansistring(value);
  p := @s[1];
  if p = nil then
    filename[0] := #0
  else
    movemem32(@filename[0], @s[1], length(s)+1);

end;

end.
