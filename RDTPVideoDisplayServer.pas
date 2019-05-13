unit RDTPVideoDisplayServer;
{GEN}
{TYPE SERVER}
{CLASS TVideoDisplayServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPVideoDisplayServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPVideoDisplayRQs.txt}
{END}
interface


uses
  VideoMarshall, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TVideoDisplayServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_PrepareVideo_string_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_PlayVideo_nativefloat(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetPosition(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Pause(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Rewind(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetVolume_NativeFloat(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Fade_NativeFloat_NativeFloat_NativeFloat_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetLength(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetPlaybackInfo_NativeFloat_NativeFloat_boolean(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetPosition_NativeFloat(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Unload(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetBoost_NativeFloat(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetPlaybackInfoEx_TPlaybackInfo(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetVideoSync_NativeFloat(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetVideoZoom_NativeFloat(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetVideoCentering_integer_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetEQ_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat(proc: TRDTPProcessor);
    procedure RQ_HANDLE_SetVideoState_boolean(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    procedure RQ_PrepareVideo(sFile:string; sAudioFile:string);overload;virtual;abstract;
    procedure RQ_PlayVideo(rStartingPoint:nativefloat);overload;virtual;abstract;
    function RQ_GetPosition():NativeFloat;overload;virtual;abstract;
    procedure RQ_Pause();overload;virtual;abstract;
    procedure RQ_Rewind();overload;virtual;abstract;
    procedure RQ_SetVolume(rVol:NativeFloat);overload;virtual;abstract;
    procedure RQ_Fade(rVolFrom:NativeFloat; rVolTo:NativeFloat; rTime:NativeFloat; bNoVideo:boolean);overload;virtual;abstract;
    function RQ_GetLength():NativeFloat;overload;virtual;abstract;
    function RQ_GetPlaybackInfo(out position:NativeFloat; out length:NativeFloat; out playing:boolean):boolean;overload;virtual;abstract;
    procedure RQ_SetPosition(pos:NativeFloat);overload;virtual;abstract;
    procedure RQ_Unload();overload;virtual;abstract;
    procedure RQ_SetBoost(rMultiplier:NativeFloat);overload;virtual;abstract;
    function RQ_GetPlaybackInfoEx(out pi:TPlaybackInfo):boolean;overload;virtual;abstract;
    function RQ_SetVideoSync(rSeconds:NativeFloat):boolean;overload;virtual;abstract;
    function RQ_SetVideoZoom(rZoom:NativeFloat):boolean;overload;virtual;abstract;
    function RQ_SetVideoCentering(x:integer; y:integer):boolean;overload;virtual;abstract;
    function RQ_SetEQ(x1:NativeFloat; x2:NativeFloat; x3:NativeFloat; x4:NativeFloat; b1:NativeFloat; b2:NativeFloat; b3:NativeFloat; b4:NativeFloat; b5:NativeFloat):boolean;overload;virtual;abstract;
    function RQ_SetVideoState(bOn:boolean):boolean;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPVideoDisplayServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_PrepareVideo_string_string(proc: TRDTPProcessor);
var
  sFile:string;
  sAudioFile:string;
begin
  GetstringFromPacket(proc.request, sFile);
  GetstringFromPacket(proc.request, sAudioFile);
  RQ_PrepareVideo(sFile, sAudioFile);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_PlayVideo_nativefloat(proc: TRDTPProcessor);
var
  rStartingPoint:nativefloat;
begin
  GetnativefloatFromPacket(proc.request, rStartingPoint);
  RQ_PlayVideo(rStartingPoint);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_GetPosition(proc: TRDTPProcessor);
var
  res: NativeFloat;
begin
  res := RQ_GetPosition();
  WriteNativeFloatToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_Pause(proc: TRDTPProcessor);
begin
  RQ_Pause();
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_Rewind(proc: TRDTPProcessor);
begin
  RQ_Rewind();
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_SetVolume_NativeFloat(proc: TRDTPProcessor);
var
  rVol:NativeFloat;
begin
  GetNativeFloatFromPacket(proc.request, rVol);
  RQ_SetVolume(rVol);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_Fade_NativeFloat_NativeFloat_NativeFloat_boolean(proc: TRDTPProcessor);
var
  rVolFrom:NativeFloat;
  rVolTo:NativeFloat;
  rTime:NativeFloat;
  bNoVideo:boolean;
begin
  GetNativeFloatFromPacket(proc.request, rVolFrom);
  GetNativeFloatFromPacket(proc.request, rVolTo);
  GetNativeFloatFromPacket(proc.request, rTime);
  GetbooleanFromPacket(proc.request, bNoVideo);
  RQ_Fade(rVolFrom, rVolTo, rTime, bNoVideo);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_GetLength(proc: TRDTPProcessor);
var
  res: NativeFloat;
begin
  res := RQ_GetLength();
  WriteNativeFloatToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_GetPlaybackInfo_NativeFloat_NativeFloat_boolean(proc: TRDTPProcessor);
var
  res: boolean;
  position:NativeFloat;
  length:NativeFloat;
  playing:boolean;
begin
  res := RQ_GetPlaybackInfo(position, length, playing);
  WritebooleanToPacket(proc.response, res);
  WriteNativeFloatToPacket(proc.response, position);
  WriteNativeFloatToPacket(proc.response, length);
  WritebooleanToPacket(proc.response, playing);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_SetPosition_NativeFloat(proc: TRDTPProcessor);
var
  pos:NativeFloat;
begin
  GetNativeFloatFromPacket(proc.request, pos);
  RQ_SetPosition(pos);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_Unload(proc: TRDTPProcessor);
begin
  RQ_Unload();
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_SetBoost_NativeFloat(proc: TRDTPProcessor);
var
  rMultiplier:NativeFloat;
begin
  GetNativeFloatFromPacket(proc.request, rMultiplier);
  RQ_SetBoost(rMultiplier);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_GetPlaybackInfoEx_TPlaybackInfo(proc: TRDTPProcessor);
var
  res: boolean;
  pi:TPlaybackInfo;
begin
  res := RQ_GetPlaybackInfoEx(pi);
  WritebooleanToPacket(proc.response, res);
  WriteTPlaybackInfoToPacket(proc.response, pi);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_SetVideoSync_NativeFloat(proc: TRDTPProcessor);
var
  res: boolean;
  rSeconds:NativeFloat;
begin
  GetNativeFloatFromPacket(proc.request, rSeconds);
  res := RQ_SetVideoSync(rSeconds);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_SetVideoZoom_NativeFloat(proc: TRDTPProcessor);
var
  res: boolean;
  rZoom:NativeFloat;
begin
  GetNativeFloatFromPacket(proc.request, rZoom);
  res := RQ_SetVideoZoom(rZoom);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_SetVideoCentering_integer_integer(proc: TRDTPProcessor);
var
  res: boolean;
  x:integer;
  y:integer;
begin
  GetintegerFromPacket(proc.request, x);
  GetintegerFromPacket(proc.request, y);
  res := RQ_SetVideoCentering(x, y);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_SetEQ_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat(proc: TRDTPProcessor);
var
  res: boolean;
  x1:NativeFloat;
  x2:NativeFloat;
  x3:NativeFloat;
  x4:NativeFloat;
  b1:NativeFloat;
  b2:NativeFloat;
  b3:NativeFloat;
  b4:NativeFloat;
  b5:NativeFloat;
begin
  GetNativeFloatFromPacket(proc.request, x1);
  GetNativeFloatFromPacket(proc.request, x2);
  GetNativeFloatFromPacket(proc.request, x3);
  GetNativeFloatFromPacket(proc.request, x4);
  GetNativeFloatFromPacket(proc.request, b1);
  GetNativeFloatFromPacket(proc.request, b2);
  GetNativeFloatFromPacket(proc.request, b3);
  GetNativeFloatFromPacket(proc.request, b4);
  GetNativeFloatFromPacket(proc.request, b5);
  res := RQ_SetEQ(x1, x2, x3, x4, b1, b2, b3, b4, b5);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TVideoDisplayServerBase.RQ_HANDLE_SetVideoState_boolean(proc: TRDTPProcessor);
var
  res: boolean;
  bOn:boolean;
begin
  GetbooleanFromPacket(proc.request, bOn);
  res := RQ_SetVideoState(bOn);
  WritebooleanToPacket(proc.response, res);
end;



{ TVideoDisplayServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TVideoDisplayServerBase.create;
begin
  inherited;
  ServiceName := 'VideoDisplay';
end;

destructor TVideoDisplayServerBase.destroy;
begin

  inherited;
end;


function TVideoDisplayServerBase.Dispatch: boolean;
var
  iRQ: integer;
begin

  result := false;

  iRQ := request.data[0];
  request.seqseek(3);
  case iRQ of
    0: begin
        result := true;
//        beeper.Beep(100,100);
       end;
  
    //PrepareVideo
    $6000:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of PrepareVideo','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_PrepareVideo_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of PrepareVideo','RDTPCALLS');
{$ENDIF}
      end;

    //PlayVideo
    $6001:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of PlayVideo','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_PlayVideo_nativefloat(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of PlayVideo','RDTPCALLS');
{$ENDIF}
      end;

    //GetPosition
    $6002:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetPosition','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetPosition(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetPosition','RDTPCALLS');
{$ENDIF}
      end;

    //Pause
    $6003:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Pause','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Pause(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Pause','RDTPCALLS');
{$ENDIF}
      end;

    //Rewind
    $6004:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Rewind','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Rewind(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Rewind','RDTPCALLS');
{$ENDIF}
      end;

    //SetVolume
    $6005:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetVolume','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetVolume_NativeFloat(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetVolume','RDTPCALLS');
{$ENDIF}
      end;

    //Fade
    $6006:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Fade','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Fade_NativeFloat_NativeFloat_NativeFloat_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Fade','RDTPCALLS');
{$ENDIF}
      end;

    //GetLength
    $6007:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetLength','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetLength(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetLength','RDTPCALLS');
{$ENDIF}
      end;

    //GetPlaybackInfo
    $6008:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetPlaybackInfo','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetPlaybackInfo_NativeFloat_NativeFloat_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetPlaybackInfo','RDTPCALLS');
{$ENDIF}
      end;

    //SetPosition
    $6009:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetPosition','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetPosition_NativeFloat(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetPosition','RDTPCALLS');
{$ENDIF}
      end;

    //Unload
    $6010:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Unload','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Unload(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Unload','RDTPCALLS');
{$ENDIF}
      end;

    //SetBoost
    $6011:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetBoost','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetBoost_NativeFloat(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetBoost','RDTPCALLS');
{$ENDIF}
      end;

    //GetPlaybackInfoEx
    $6012:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetPlaybackInfoEx','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetPlaybackInfoEx_TPlaybackInfo(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetPlaybackInfoEx','RDTPCALLS');
{$ENDIF}
      end;

    //SetVideoSync
    $6013:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetVideoSync','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetVideoSync_NativeFloat(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetVideoSync','RDTPCALLS');
{$ENDIF}
      end;

    //SetVideoZoom
    $6014:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetVideoZoom','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetVideoZoom_NativeFloat(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetVideoZoom','RDTPCALLS');
{$ENDIF}
      end;

    //SetVideoCentering
    $6015:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetVideoCentering','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetVideoCentering_integer_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetVideoCentering','RDTPCALLS');
{$ENDIF}
      end;

    //SetEQ
    $6016:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetEQ','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetEQ_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat_NativeFloat(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetEQ','RDTPCALLS');
{$ENDIF}
      end;

    //SetVideoState
    $6017:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetVideoState','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetVideoState_boolean(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetVideoState','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


