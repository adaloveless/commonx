unit RDTPVideoDisplayServerImplib;
{GEN}
{TYPE IMPLIB}
{SERVICENAME VideoDisplay}
{RQFILE RDTPVideoDisplayRQs.txt}
{END}
{$DEFINE DIRECT}
interface

uses
  RDTPVideoDisplayServer, RDTPSocketServer, forms, windows, videomarshall,miscroutines;

type
  TVideoDisplayServer = class(TVideoDisplayServerBase)
  private
  protected
  public
{INTERFACE_START}
    procedure RQ_PrepareVideo(sFile:string; sAudioFile:string);overload;override;
    procedure RQ_PlayVideo(rStartingPoint:nativefloat);overload;override;
    function RQ_GetPosition():NativeFloat;overload;override;
    procedure RQ_Pause();overload;override;
    procedure RQ_Rewind();overload;override;
    procedure RQ_SetVolume(rVol:NativeFloat);overload;override;
    procedure RQ_Fade(rVolFrom:NativeFloat; rVolTo:NativeFloat; rTime:NativeFloat; bNoVideo:boolean);overload;override;
    function RQ_GetLength():NativeFloat;overload;override;
    function RQ_GetPlaybackInfo(out position:NativeFloat; out length:NativeFloat; out playing:boolean):boolean;overload;override;
    procedure RQ_SetPosition(pos:NativeFloat);overload;override;
    procedure RQ_Unload();overload;override;
    procedure RQ_SetBoost(rMultiplier:NativeFloat);overload;override;
    function RQ_GetPlaybackInfoEx(out pi:TPlaybackInfo):boolean;overload;override;
    function RQ_SetVideoSync(rSeconds:NativeFloat):boolean;overload;override;
    function RQ_SetVideoZoom(rZoom:NativeFloat):boolean;overload;override;
    function RQ_SetVideoCentering(x:integer; y:integer):boolean;overload;override;
    function RQ_SetEQ(x1:NativeFloat; x2:NativeFloat; x3:NativeFloat; x4:NativeFloat; b1:NativeFloat; b2:NativeFloat; b3:NativeFloat; b4:NativeFloat; b5:NativeFloat):boolean;overload;override;
    function RQ_SetVideoState(bOn:boolean):boolean;overload;override;

{INTERFACE_END}
  end;

  TRDTPVideoDisplayServer = class(TRDTPSocketServer<TVideoDisplayServer>);



implementation

uses
  FormVideo;
{ TVideoDisplayServer }

procedure TVideoDisplayServer.RQ_Fade(rVolFrom, rVolTo, rTime: NativeFloat; bNoVideo: boolean);
begin
  inherited;

  frmvideo.Lock;
  try
    frmVideo.ConfigureFade(rVolFrom, rVolTo, rTime, bNoVideo);
  finally
    frmVideo.Unlock;
  end;

end;

function TVideoDisplayServer.RQ_GetLength: nativefloat;
begin
  result := frmVideo.Length;

end;

function TVideoDisplayServer.RQ_GetPlaybackInfo(out position, length: nativefloat; out playing:boolean): boolean;
begin
  result := true;
  frmVideo.Lock;
  try
    position := frmVideo.position;
    length := frmvideo.length;
    playing := (frmVideo.volatiledata.LastKnownPlayState = 3) and (not frmVideo.PlayingForScrub);
  finally
    frmVideo.Unlock;
  end;

end;

function TVideoDisplayServer.RQ_GetPlaybackInfoEx(out pi: TPlaybackInfo): boolean;
begin
  result := false;
  frmVideo.Lock;
  try
    while not result do
    try
      pi.position := frmVideo.position;
      pi.length := frmvideo.length;
      pi.playing := (frmVideo.volatiledata.LastKnownPlayState = 3) and (not frmVideo.PlayingForScrub);
      pi.PlayingForScrub := frmVideo.PlayingforScrub;
      pi.LastPlayState := frmvideo.LastPlayState;
      pi.PlayStateAfterPrepare := frmVideo.PlaystateAfterPrepare;
      result := true;
    except
      result := false;
    end;
  finally
    frmVideo.Unlock;
  end;
end;

function TVideoDisplayServer.RQ_GetPosition: nativefloat;
begin
  //windows.Beep(1000,50);
  result := frmVideo.Position;
end;

procedure TVideoDisplayServer.RQ_Pause;
begin
  inherited;
  frmVideo.Lock;
  try
  finally
    frmVideo.Unlock;
  end;

{$IFDEF DIRECT}
    frmVideo.ST_Pause;
{$ELSE}
    frmVideo.PostPause;
{$ENDIF}
end;

procedure TVideoDisplayServer.RQ_PlayVideo(rStartingPoint:nativefloat);
begin
  inherited;
  frmVideo.Lock;
  try
    frmVideo.StartPlayAt := rStartingPOint;
    frmVideo.PlayingForScrub := false;
  finally
    frmVideo.Unlock;
  end;

{$IFDEF DIRECT}
    frmVideo.ST_Play();
{$ELSE}
    frmVideo.PlayingForScrub := false;
    frmVideo.PostPlay;
{$ENDIF}


end;

procedure TVideoDisplayServer.RQ_PrepareVideo(sFile: string; sAudioFile:string);
begin
  inherited;
  frmVideo.Lock;
  try
    if frmVideo.filename = sFile then
      exit;
    frmVideo.filename := sFile;
    frmvideo.AudiofileName  := sAudioFile;
  finally
    frmVideo.Unlock;
  end;
{$IFDEF DIRECT}
    frmVideo.ST_Prepare();
{$ELSE}
    frmVideo.NeedsPrepare := true;
{$ENDIF}



end;

procedure TVideoDisplayServer.RQ_Rewind;
begin
  inherited;
{$IFDEF DIRECT}
  frmVideo.ST_Prepare();
{$ELSE}
  frmVideo.PostRewind;
{$ENDIF}

end;

procedure TVideoDisplayServer.RQ_SetBoost(rMultiplier: nativefloat);
begin
  inherited;
  frmVideo.Boost := rMultiplier;
end;

function TVideoDisplayServer.RQ_SetEQ(x1, x2, x3, x4, b1, b2, b3,
  b4,b5: NativeFloat): boolean;
begin
  frmVideo.SetEQ(x1,x2,x3,x4,b1,b2,b3,b4,b5);
  result := true;
end;

procedure TVideoDisplayServer.RQ_SetPosition(pos: nativefloat);
begin
  inherited;
  frmVideo.Position := pos;
end;

function TVideoDisplayServer.RQ_SetVideoCentering(x, y: integer): boolean;
begin
  postmessage(frmVideo.Handle, FormVideo.WM_SET_CENTER, x,y);
  result := true;
end;

function TVideoDisplayServer.RQ_SetVideoState(bOn: boolean): boolean;
begin
  postmessage(frmVideo.Handle, FormVideo.WM_SET_VIDEO_STATE, booltoint(bOn),0);
  result := true;
end;

function TVideoDisplayServer.RQ_SetVideoSync(rSeconds: nativefloat): boolean;
begin
  postmessage(frmVideo.Handle, FormVideo.WM_SET_SYNC, round(rSEconds*1000),0);
  result := true;
end;

function TVideoDisplayServer.RQ_SetVideoZoom(rZoom: nativefloat): boolean;
begin
  postmessage(frmVideo.Handle, FormVideo.WM_SET_ZOOM, round(rZoom * 10000), 0);
  result := true;
end;

procedure TVideoDisplayServer.RQ_SetVolume(rVol: nativefloat);
begin
  inherited;
  frmVideo.Volume := rVol;
end;



procedure TVideoDisplayServer.RQ_Unload;
begin
  frmVideo.ST_TryLikeHell(tlhStop);

end;

end.
