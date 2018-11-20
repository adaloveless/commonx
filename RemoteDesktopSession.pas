unit RemoteDesktopSession;
{$DEFINE OLD_PERFORMANCE}
interface

uses ping, comctrls,graphics, dialogs, windows, sysutils, stringx, velocitypanel, systemx, stdctrls, controls, glasscontrols, extctrls, betterobject, classes, generics.collections.fixed, MSTSCLib_TLB;

const
  BARSIZE = 15;
  THuMBHEIGHT = 120;
  TS_PERF_DISABLE_NOTHING = $00000000;
  TS_PERF_DISABLE_WALLPAPER = $00000001;
  TS_PERF_DISABLE_FULLWINDOWDRAG = $00000002;
  TS_PERF_DISABLE_MENUANIMATIONS = $00000004;
  TS_PERF_DISABLE_THEMING = $00000008;
  TS_PERF_ENABLE_ENHANCED_GRAPHICS = $00000010;
  TS_PERF_DISABLE_CURSOR_SHADOW = $00000020;
  TS_PERF_DISABLE_CURSORSETTINGS = $00000040;
  TS_PERF_ENABLE_FONT_SMOOTHING = $00000080;
  TS_PERF_ENABLE_DESKTOP_COMPOSITION = $00000100;
  TS_PERF_DEFAULT_NONPERFCLIENT_SETTING = $40000000;
  TS_PERF_RESERVED1 = $80000000;
type
{$IFDEF VISTA}
  TNativeClient = TMsRDPClient7;
{$ELSE}
  TNativeClient = TMsRDPClient2;
{$ENDIF}


  TRemoteBandWidth = (bwUltraLow, bwLow, bwMedium, bwHigh, bwUltraHigh);
  TRemoteDesktopSession = class(TVelocityPanel)
  private
    FName, FUser, FDomain: string;
    FFollowUpTime: TDateTime;
    FBoolean: boolean;
    FFollowupStartTime: TDateTime;
    FPassword: string;
    FNote: string;
    FheightATConnect: integer;
    FWidthATConnect: integer;
    FBandwidth: TREmoteBandwidth;
    FThumbnailmode: boolean;
    FTitle: TLabel;
    FAdmin: boolean;
    FPingable: boolean;
    FDoNotPing: boolean;
    FServer: string;

    function GetConnected: boolean;
    function GetServer: string;
    procedure SetServer(const Value: string);
    function GetDomain: string;
    function GetUser: string;
    procedure SetDomain(const Value: string);
    procedure SetUser(const Value: string);
    function GEtPassword: string;
    procedure SetPassword(const Value: string);
    function getFollowupProgression: real;
    function GetBandWidthString: string;
    procedure SEtBandWidthString(const Value: string);
    procedure SetThumbnailMode(const Value: boolean);
  strict protected
    FClientImmediate: TNativeClient;
    FPingCmd: Tcmd_Ping;
    procedure SEtClientBandwidthParams;
    property Client: TNativeclient read FClientImmediate;
    procedure CreateNativeClient;
  public
    constructor Create(aowner: TComponent);override;

    destructor Destroy;override;

    //client
    procedure ShowClient(parent: TWinControl);
    procedure HideClient;

    property Name: string read FName write FName;
    property Connected: boolean read GetConnected;
    property Server: string read GetServer write SetServer;
    property Domain: string read GetDomain write SetDomain;
    property User: string read GetUser write SetUser;
    property Password: string read GEtPassword write SetPassword;
    property Note: string read Fnote write FNote;


    procedure SaveToFile(sfile: string);
    procedure LoadFromfile(sfile: string);

    PROCEDURE Connect;
    procedure Disconnect;

    //icon

    //image
    //follow up time
    property followUptime: TDateTime read FFollowUpTime write FfollowUpTime;
    property FollowupStartTime: TDateTime read FFollowupStartTime write FFollowupStartTime;
    //follow up
    property followUp: boolean read FBoolean write Fboolean;
    property FollowupProgression: real read getFollowupProgression;
    procedure WakeUp;

    procedure AskNote;
    procedure Resize;override;
    function GetRecommendedDimensions: TPOint;
    function GetRecommendedDimensionsByHeight: TPOint;


    property widthATConnect: integer read FWidthATConnect;
    property heightATConnect: integer read FheightATConnect;
    property BandWidthString: string read GetBandWidthString write SEtBandWidthString;
    property BandWidth: TREmoteBandwidth read FBandwidth write FbandWidth;

    property ThumbnailMode: boolean read FThumbnailmode write SetThumbnailMode;
    //connected
    //display order
    //parent client
    //x,y position
    property Admin: boolean read FAdmin write FAdmin;
    procedure UpdateStatus;
    property Pingable: boolean read FPingable write FPIngable;
    procedure Ping_Async;
    function PIng_IsComplete: boolean;
    procedure Ping_End;
    property DoNotPIng: boolean read FDoNotPing write FDoNotPing;
  end;








implementation

{ TRemoteDesktopSession }

procedure TRemoteDesktopSession.AskNote;
var
  s:string;
begin
  inputQuery('Notes for '+Name, 'Note:', FNote);

end;

procedure TRemoteDesktopSession.Connect;
begin
  CreateNativeClient;

  FClientImmediate.Server := FServer;

  if trim(Fdomain) <> '' then
    FClientImmediate.UserName := Fdomain+'\'+Fuser
  else
    FClientImmediate.userName := FUser;

  if FPassword <>'' then begin
    FClientImmediate.AdvancedSettings2.Set_ClearTextPassword(FPassword);
  end;

{$IFDEF VISTA}
  FClientImmediate.AdvancedSettings7.ConnectToAdministerServer := Admin;
  //FClient.AdvancedSettings8.RedirectDirectX := true;

  //FClient.AdvancedSettings8.
{$ELSE}
  FClientImmediate.AdvancedSettings2.ConnectToServerConsole := ADmin;
{$ENDIF}


  if FClientImmediate.parent <> nil then begin
    FWidthAtConnect := TargetWidth;
    FHeightAtConnect := TargetHeight;
  end;

//  Fclient.DesktopWidth := fclient.width;
//  Fclient.Desktopheight := fclient.height;

  SetClientBandwidthParams;
  FClientImmediate.connect;
  Wakeup;


end;

constructor TRemoteDesktopSession.Create;
begin
  inherited;
//  CReateNativeClient;

  Ftitle := TLabel.create(self);
  Ftitle.parent := self;
  Ftitle.visible := false;


end;

procedure TRemoteDesktopSession.CreateNativeClient;
begin
  if assigned(FClientImmediate) then begin
    FClientImmediate.free;
    FClientImmediate := nil;
  end;
  FClientImmediate := TNativeClient.create(self);
  FClientImmediate.AdvancedSettings.allowBackgroundInput := 1;
  FClientImmediate.doublebuffered := true;
  FClientImmediate.parent := self;
  FClientImmediate.width := self.width;
  FClientImmediate.height := self.height;
  FClientImmediate.AdvancedSettings8.RedirectDirectX := false;
  FClientImmediate.AdvancedSettings8.Compress := 0;
//  FclientImmediate.AdvancedSettings8.NegotiateSecurityLayer := false;
end;

destructor TRemoteDesktopSession.Destroy;
begin
  if assigned(FClientImmediate) then begin
    FClientImmediate.parent := nil;
    FClientImmediate.free;
    FClientImmediate:= nil;
  end;
  inherited;
end;

procedure TRemoteDesktopSession.Disconnect;
begin
  if assigned(FCLientImmediate) then begin
    FClientImmediate.Disconnect;
    FClientImmediate.free;
    FCLientImmediate := nil;
  end;
end;

function TRemoteDesktopSession.GetBandWidthString: string;
begin
  case FBandwidth of
    bwUltraLow: result := 'Ultra Low Bandwidth';
    bwLow: result := 'Low Bandwidth';
    bwMedium: result := 'Medium Bandwidth';
    bwHigh: result := 'High Bandwidth';
    bwUltraHigh: result := 'Ultra High Bandwidth';
  else
    result := 'unknown bandwidth';
  end;
end;

function TRemoteDesktopSession.GetConnected: boolean;
begin
  if FClientImmediate= nil then
    exit(false);

  result := FClientImmediate.Connected <> 0;
end;

function TRemoteDesktopSession.GetDomain: string;
begin
  result := FDomain;
end;

function TRemoteDesktopSession.getFollowupProgression: real;
var
  n: TDateTime;
  d:real;
begin
  n := now;
  d := (Followuptime-FollowupStartTime);
  if d = 0 then
    result := 0
  else
    result := (n-FollowupStartTime)/d;
  if result < 0 then result := 0;
  if result > 1 then result := 1;

end;

function TRemoteDesktopSession.GEtPassword: string;
begin
  result := Fpassword;
//  result := FClient.password;
end;

function TRemoteDesktopSession.GetRecommendedDimensions: TPOint;
var
  w,h: integer;
begin
  w := 0;
  h := 0;
  if connected then begin
    h := HeightAtConnect;
    w := WidthAtConnect;
  end;



  if w =0 then w := self.width;
  if h =0 then h := self.height;

  result.x := self.width;
  if w > 0 then begin
    if (w = width) and (h < height) then
      result.y := height
    else
      result.y := trunc(self.width*(h/w));
  end else begin
    result.y := 1;
  end;


  if result.y > height then
    result := GetREcommendedDimensionsByHeight;
end;

function TRemoteDesktopSession.GetRecommendedDimensionsByHeight: TPOint;
var
  w,h: integer;
begin
  w := 0;
  h := 0;
  if connected then begin
    h := HeightAtConnect;
    w := WidthAtConnect;
  end;



  if w =0 then w := self.width;
  if h =0 then h := self.height;

  result.y := self.height;
  if w > 0 then begin
    if (w = width) and (h < height) then
      result.x := width
    else
      result.x := trunc(self.height*(w/h));
  end else begin
    result.y := 1;
  end;
end;

function TRemoteDesktopSession.GetServer: string;
begin
  result := self.FServer;

end;

function TRemoteDesktopSession.GetUser: string;
begin
  result := FUser;
end;

procedure TRemoteDesktopSession.HideClient;
begin
  if assigned(FClientImmediate) then begin
    FClientImmediate.Parent := nil;
  end;
end;


procedure TRemoteDesktopSession.LoadFromfile(sfile: string);
var
  sl: TStringlist;
begin
  sl := TStringList.create;
  try
    sl.LoadFromfile(sfile);
    self.Name := sl[0];
    self.Server := sl[1];
    self.User := sl[2];
    self.Domain := sl[3];
    self.Password := sl[4];
    if sl.count >5 then begin
      self.followuptime := strtofloat(sl[5]);
      self.followup := strtobool(sl[6]);
    end;
    if sl.count >7 then begin
      self.followupstarttime := strtofloat(sl[7]);
    end;
    if sl.count>8 then begin
      self.Note := sl[8];
    end;
    if sl.count>9 then begin
      admin := strtobool(sl[9]);
    end else begin
      admin := true;
    end;
    if sl.Count> 10 then begin
      DoNotPIng := strtobool(sl[10]);
    end else begin
      DoNotPing := false;
    end;
  finally
    sl.free;
  end;

end;

procedure TRemoteDesktopSession.Ping_Async;
begin
  if DonotPing then begin
    Pingable := true;
    exit;
  end;
  Ping_End;

  FPingCmd := Tcmd_Ping.Create;
  FPIngCmd.Resources.SetResourceUsage('ping', 1/4);
  FPIngCmd.Host := self.Server;
  FPingCmd.Start;
end;

procedure TRemoteDesktopSession.Ping_End;
begin
  if not assigned(FPingCmd) then
    exit;

  FpingCmd.WaitFor;

  FPingable := FPIngCmd.Result;

  FPIngCmd.Free;
  FPIngCmd := nil;


end;

function TRemoteDesktopSession.PIng_IsComplete: boolean;
begin
  if FPingCmd = nil then begin
    result := true;
    exit;
  end;

  result := FPIngCmd.IsComplete;


end;

procedure TRemoteDesktopSession.Resize;
var
  p: TPoint;
begin
  inherited;
  p := GetRecommendedDimensions;

  if assigned(FClientImmediate) then begin
    FClientImmediate.align := alNone;
    if Thumbnailmode then begin

      FClientImmediate.SetBounds(0,BARSIZE, p.x,p.y-BARSIZE);

    end else begin

      FClientImmediate.SetBounds(0,0, p.x,p.y);
    end;
  end;

end;

procedure TRemoteDesktopSession.SaveToFile(sfile: string);
var
  sl: TStringlist;
begin
  sl := TStringList.create;
  try
    sl.add(self.Name);
    sl.add(self.Server);
    sl.add(self.User);
    sl.add(self.Domain);
    sl.add(self.Password);
    sl.add(floattostr(self.FollowUpTime));
    sl.add(booltostr(self.Followup));
    sl.add(floattostr(self.FollowupStarttime));
    sl.add(self.Note);
    sl.add(booltostr(admin));
    sl.Add(booltostr(donotping));
    sl.Savetofile(sfile);

  finally
    sl.free;
  end;

end;

procedure TRemoteDesktopSession.SEtBandWidthString(const Value: string);
begin
  if lowercase(value) = 'ultra low bandwidth' then FBandwidth := bwUltraLow;
  if lowercase(value) = 'low bandwidth' then FBandwidth := bwlow;
  if lowercase(value) = 'medium bandwidth' then FBandwidth := bwmedium;
  if lowercase(value) = 'high bandwidth' then FBandwidth := bwhigh;
  if lowercase(value) = 'ultra high bandwidth' then FBandwidth := bwultrahigh;
end;

procedure TRemoteDesktopSession.SEtClientBandwidthParams;
begin
  if not assigned(FClientImmediate) then
    exit;
{$IFDEF VISTA}
  FClientImmediate.AdvancedSettings6.RedirectDevices := true;
  FClientImmediate.AdvancedSettings6.RedirectPOSDevices := true;

{
  MSTSCLibMinorVersion = 0;
  TS_PERF_DISABLE_NOTHING = $00000000;
  TS_PERF_DISABLE_WALLPAPER = $00000001;
  TS_PERF_DISABLE_FULLWINDOWDRAG = $00000002;
  TS_PERF_DISABLE_MENUANIMATIONS = $00000004;
  TS_PERF_DISABLE_THEMING = $00000008;
  TS_PERF_ENABLE_ENHANCED_GRAPHICS = $00000010;
  TS_PERF_DISABLE_CURSOR_SHADOW = $00000020;
  TS_PERF_DISABLE_CURSORSETTINGS = $00000040;
  TS_PERF_ENABLE_FONT_SMOOTHING = $00000080;
  TS_PERF_ENABLE_DESKTOP_COMPOSITION = $00000100;
  TS_PERF_DEFAULT_NONPERFCLIENT_SETTING = $40000000;
  TS_PERF_RESERVED1 = $80000000;
}
  case Bandwidth of
    bwMedium:

  end;
{$IFDEF OLD_PERFORMANCE}
  case BandWidth of
    bwUltraLow: begin
      FClientImmediate.ColorDepth := 8;
      FClientImmediate.AdvancedSettings2.PerformanceFlags :=
        TS_PERF_DISABLE_WALLPAPER or
        TS_PERF_DISABLE_FULLWINDOWDRAG or
        TS_PERF_DISABLE_MENUANIMATIONS or
        TS_PERF_DISABLE_THEMING or
        TS_PERF_DISABLE_CURSOR_SHADOW or
        TS_PERF_DISABLE_CURSORSETTINGS;

    end;
    bwLow: begin
      FClientImmediate.ColorDepth := 16;
      FClientImmediate.AdvancedSettings2.PerformanceFlags :=
        TS_PERF_DISABLE_WALLPAPER or
        TS_PERF_DISABLE_FULLWINDOWDRAG or
        TS_PERF_DISABLE_MENUANIMATIONS or
        TS_PERF_DISABLE_THEMING;

    end;
    bwMedium: begin
      FClientImmediate.ColorDepth := 16;
      FClientImmediate.AdvancedSettings2.PerformanceFlags :=
        TS_PERF_DISABLE_WALLPAPER or
        TS_PERF_ENABLE_ENHANCED_GRAPHICS or
        TS_PERF_DISABLE_CURSOR_SHADOW or
        TS_PERF_DISABLE_CURSORSETTINGS;

    end;
    bwHigh: begin
      FClientImmediate.ColorDepth := 32;
      FClientImmediate.AdvancedSettings2.PerformanceFlags :=
        TS_PERF_DISABLE_WALLPAPER or
        TS_PERF_ENABLE_ENHANCED_GRAPHICS //or
//        TS_PERF_ENABLE_FONT_SMOOTHING or
(*        TS_PERF_ENABLE_DESKTOP_COMPOSITION*);


    end;
    bwUltraHigh: begin
      FClientImmediate.ColorDepth := 32;
      FClientImmediate.AdvancedSettings2.PerformanceFlags :=
        TS_PERF_ENABLE_ENHANCED_GRAPHICS or
        TS_PERF_ENABLE_FONT_SMOOTHING //or
(*        TS_PERF_ENABLE_DESKTOP_COMPOSITION*);

    end;
  end;
{$ENDIF}



//  FClient.AdvancedSettings6.BitmapVirtualCache32BppSize := 48000000;
  FClientImmediate.AdvancedSettings6.AudioRedirectionMode := 0;
{$ENDIF}
  FClientImmediate.AdvancedSettings2.ShadowBitmap := 1;
  FClientImmediate.AdvancedSettings2.EnableWindowsKey := 1;
  FClientImmediate.AdvancedSettings2.SmartSizing := true;
  FClientImmediate.AdvancedSettings.Compress := 1;
  FClientImmediate.AdvancedSettings.BitmapPeristence := 1;





end;

procedure TRemoteDesktopSession.SetDomain(const Value: string);
begin
  FDomain := value;
end;

procedure TRemoteDesktopSession.SetPassword(const Value: string);
begin
  FPassword := value;
end;

procedure TRemoteDesktopSession.SetServer(const Value: string);
begin
  FServer := value;
end;

procedure TRemoteDesktopSession.SetThumbnailMode(const Value: boolean);
begin
  FThumbnailmode := Value;
  if FThumbnailMode then begin
    client.top := BARSIZE;
    client.height :=  GetREcommendedDimensions.y-BARSIZE;
    color := clBlack;
    FTitle.visible := true;
    Ftitle.onclick := self.OnClick;
    Ftitle.caption := self.name;

  end else begin
    client.top := 0;
    Ftitle.visible := false;
    client.height :=  GetREcommendedDimensions.y;
  end;
end;

procedure TRemoteDesktopSession.SetUser(const Value: string);
begin
  FUser := value;
end;

procedure TRemoteDesktopSession.ShowClient(parent: TWinControl);
begin
  if FClientImmediate = nil then
    exit;
  self.parent := parent;

  FClientImmediate.Parent := self;
  self.Margins.Top := 0;
  self.Margins.Left := 0;
  self.Margins.Right := 0;
  self.Margins.Bottom := 0;
  visible := true;
//  FClient.left := 0;
//  fClient.Top := 0;
  FClientImmediate.Width := width;
  FClientImmediate.height := height;
  //FClient.align := alClient;
//  self.align := alClient;



end;

procedure TRemoteDesktopSession.UpdateStatus;
begin
  FTitle.caption := self.name+' - '+self.note;
  //transparent := false;
  Ftitle.transparent := false;
  if FollowUp and (FollowupProgression=1) then begin
    Ftitle.color := clYellow;
    FTitle.caption := self.name+' - '+self.note;
  end else begin
    Ftitle.color := clWhite;
    FTitle.caption := self.name;
  end;

end;

procedure TRemoteDesktopSession.WakeUp;
begin
  if FClientImmediate = nil then
    exit;

  FClientImmediate.enabled := true;
  windows.SetFocus(FClientImmediate.handle);

  FClientImmediate.bringtofront;
  FClientImmediate.AdvancedSettings2.enablemouse := 1;

//  SendMessage(FClient.handle, WM_USER, 0, 0);
end;

end.
