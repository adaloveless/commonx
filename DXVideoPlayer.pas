unit DXVideoPlayer;

interface

uses
  classes, direct3d, miscroutines, soundtools, ffmpeg_tools, advancedgraphics_dx, advancedgraphics, easyimage, graphics, sysutils, windows;

type
  TUDPVideoTrackInfo = packed record
    CurrentFrame: cardinal;
    FadeWeight: smallint;
    LengthOfName: smallint;
  end;
  TUDPVideoServerPacket = packed record
    t1: TUDPVideoTrackInfo;
    t2: TUDPVideoTrackInfo;
    function t1file: string;
    function t2file: string;
  end;
  TDXVideoPlayer = class(TDX2d)
  public
    procedure DoDraw;override;
    procedure InitTextures;override;
  end;

implementation

end.
