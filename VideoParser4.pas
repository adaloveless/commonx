unit VideoParser4;

interface
uses
  windows, advancedgraphics_dx, easyimage, directshow9, direct3d9_jedi, graphics,
  sysutils, fileserviceClientEx, managedthread, d3dx9, sharedobject,
  types, activex, betterobject, classes, commandprocessor, memoryfilestream,
  miscroutines, generics.Collections.fixed, dxerr9;

{$DEFINE UPDATE_SURFACE_IN_THREAD}
{$DEFINE VID_TEX_IN_SYS_MEM}

const
  CLSID_CAKEWALK_QUICKTIME_SOURCE_FILTER  :TGUID = '{44BA3F3D-A412-486B-95C9-9D63056E963C}';
  CLSID_COLOR_SPACE_CONVERTER             :TGUID = '{1643E180-90F5-11CE-97D5-00AA0055595A}';
  CLSID_VIDEO_MIXING_RENDERER_9           :TGUID = '{51B4ABF3-748F-4E3B-A276-C828330E926A}';
  CLSID_DIRECT_SOUND_DEVICE               :TGUID = '{79376820-07D0-11CF-A24D-0020AFD79767}';

  FRAME_BUFFER_SIZE = 2;
type
  PBITMAPINFOHEADER = ^BITMAPINFOHEADER;

  TVideoTexture = record
    tex: IDirect3dTexture9;
    surf: IDirect3dSurface9;
  end;
  TVideoTExtureArray = array[0..0] of TVideoTexture;
  PVolatileTexturePair = ^TVideoTExture;
  PVideoTExtureArray = ^TVideoTextureArray;


  TTimeAdviseRequest = class(TBetterObject)
  public
    cookie: DWORD;
    periodic: boolean;
    timetoadvise: TReferenceTime;
    basetime: TReferenceTime;
    hEvent: THandle;
    bComplete: boolean;
    procedure SignalIftime(refTime: TReferenceTime);
  end;

  TMediaClockThread = class(TExternalEventThread);

  TMediaClock = class(TSharedObject, IReferenceClock)
  private
    FExternalTime: TReferenceTime;
    procedure SetExternalTime(const Value: TReferenceTime);
    function GetExternalTime: TReferenceTime;
  public
    thr: TMediaClockThread;
    FAdviseCookieID: dword;
    FAdviseRequests: TList<TTimeAdviseREquest>;
    procedure Init;override;
    destructor Destroy;override;
    function GEtTime: TReferenceTime;overload;
    function GetTime(out pTime: TReferenceTime): HResult; overload;stdcall;
    function AdviseTime(rtBaseTime, rtStreamTime: TReferenceTime; hEvent: THandle; out pdwAdviseCookie: DWORD): HResult; stdcall;
    function AdvisePeriodic(const rtStartTime, rtPeriodTime: TReferenceTime; hSemaphore: THandle; out pdwAdviseCookie: DWORD): HResult; stdcall;
    function Unadvise(dwAdviseCookie: DWORD): HResult; stdcall;
    function IndexOfAdvise(dwAdviseCookie: DWORD): nativeint;
    procedure UpdateAdviseEvents;
    procedure OnThreadExecute(sender: TExternalEventThread);
    procedure StartThread;
    procedure StopThread;
    property ExternalTime: TReferenceTime read GetExternalTime write SetExternalTime;
  end;

  TVideoBitmap = packed record
    p: PByte;
    size: longint;
    framenumber: nativeint;
    frametime: nativefloat;
    ready: boolean;
    available: boolean;
    function Header: PBITMAPINFOHEADER;
    function Data: pbyte;
    procedure NeedSize(iSize: nativeint);
    procedure Cleanup;
    procedure Init;
    procedure ApplytoTexture(tex: IDirect3dTexture9);
    procedure NoNeed;
  end;

  PVideoBitmap = ^TVideoBitmap;

  TVideoParser3 = class;//forward

  TVideoAllocatorPresenter = class(TSharedObject, IVMRSurfaceAllocator9, IVMRImagePresenter9)
  public
    FParsers: TList<TVideoParser3>;
    FNextUserID: nativeint;
  public
    procedure Init;override;
    destructor Destroy;override;
    procedure Attach(vp: TVideoParser3);
    procedure Detach(vp: TVideoParser3);


    function FindUser(dwUserID: dword; out vp3: TVideoParser3): boolean;
    //ALLOCATOR
    function InitializeDevice(dwUserID: DWORD; lpAllocInfo: PVMR9AllocationInfo; var lpNumBuffers: DWORD): HResult; stdcall;
    function TerminateDevice(dwID: DWORD): HResult; stdcall;
    function GetSurface(dwUserID: DWORD; SurfaceIndex: DWORD; SurfaceFlags: DWORD; out lplpSurface: IDirect3DSurface9): HResult; stdcall;
    function AdviseNotify(lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9): HResult; stdcall;
    //PRESENTER
    function StartPresenting(dwUserID: DWORD): HResult;stdcall;
    function StopPresenting(dwUserID: DWORD): HResult; stdcall;
    function PresentImage(dwUserID: DWORD; lpPresInfo: PVMR9PresentationInfo): HResult; stdcall;
  end;


  TVideoParser3 = class(TSharedObject)
  private
    FFrameBuffers: array[0..FRAME_BUFFER_SIZE-1] of TVideoBitmap;
    FOnFrameComplete: TNotifyEvent;
    FEstLen: real;
    FReserved: boolean;
    FClippingwindow: HWND;
    FDX: TDX2d;
    Fh: nativeint;
    Fw: nativeint;
    FFileName: string;
    procedure SEtAllocatorPResenter(const Value: TVideoAllocatorPresenter);
  public
    userid: Dword;
    allocator_presenter: TVideoAllocatorPresenter;
    media: IMediaDet;
    Graph: IGraphBuilder;
    MediaSeeking: IMediaSeeking;
    MediaControl: IMediaControl;
    MediaEvent: IMediaEventEx;
    MEdiaFilter: IMediaFilter;
    GrabberFilter: IBaseFilter;
    Grabber: ISampleGrabber;
    NullFilter: IBaseFilter;
    vmr9: IBaseFilter;
    movie_texture: TFastBitmap;
    resized_texture: TFastBitmap;
    iLastFrame: integer;
    rFrameRate: double;
    seektime: nativefloat;
    custom_clock: IReferenceClock;
    clkObj: TMediaClock;
    vmr_mix_control: IVMRMixerControl9;
    vmr_win_control: IVMRWindowlessControl9;
    san: IVMRSurfaceAllocatorNotify9;
    vtps: PVideoTextureArray;

    tex_video_memory: IDirect3dTExture9;
    surf_video_memory: IDirect3dSurface9;
    buffer_count: cardinal;
    frame_ready: longbool;
    frame_presented: longbool;

    procedure Init;override;
    procedure Detach;override;
    destructor Destroy;override;//

    procedure InitMedia(sfile: string);//
    procedure CloseMedia;
    procedure CleanupVTps;
    function FileName: string;
    procedure InitFilterGraph;
    procedure InitFilterGraph_EasyWay;
    procedure InitFilterGraph_HardWay;


    property OnFrameComplete
      : TNotifyEvent read FOnFrameComplete write FOnFrameComplete;
    property Reserved: boolean read FReserved write FReserved;
    procedure GotoTime(rTime: nativefloat);


    property ClippingWindow: HWND read FClippingwindow write FClippingWindow;
    property dx: TDX2d read FDX write FDX;

    property w: nativeint read Fw write Fw;
    property h: nativeint read Fh write Fh;

    //ALLOCATOR
    function InitializeDevice(lpAllocInfo: PVMR9AllocationInfo; var lpNumBuffers: DWORD): HResult; stdcall;
    function TerminateDevice(): HResult; stdcall;
    function GetSurface(SurfaceIndex: DWORD; SurfaceFlags: DWORD; out lplpSurface: IDirect3DSurface9): HResult; stdcall;
    function AdviseNotify(lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9): HResult; stdcall;
    //PRESENTER
    function StartPresenting(): HResult;stdcall;
    function StopPresenting(): HResult; stdcall;
    function PresentImage(lpPresInfo: PVMR9PresentationInfo): HResult; stdcall;
    property AllocatorPresenter: TVideoAllocatorPresenter read allocator_presenfter write SEtAllocatorPResenter;
  end;

function AddFilterByCLSID(pgraph:IGraphBuilder; clsid:TGUID; out ppF:IBaseFilter; name: string): HRESULT;
function IsPinConnected(pPin: IPin; out res: boolean): HResult;overload;
function IsPinConnected(pPin: IPin): boolean;overload;
function GetPinDirection(ppin: IPin): PIN_DIRECTION;
function IsPinMatch(ppin: IPin; dir: PIN_DIRECTION; connected: boolean): boolean;
function FindFilter(graph: IGraphBuilder; CLSID: TGUID): IBaseFilter;
function Succeeded(Res: HResult; sAnnotation: string): Boolean;
function Failed(Res: HResult; sAnnotation: string): Boolean;
procedure ASSERTSUCCESS(sAnnotation: string; Res: HRESULT);


implementation

{ TVideoParser3 }
procedure ASSERTSUCCESS(sAnnotation: string; Res: HRESULT) ;
begin
  if not SUCCEEDED(res, sAnnotation) then
    raise Exception.create('Assertion failed at ** '+sAnnotation+' ** Description:'+dxerr9.DXGetErrorDescription9W(res));
end;


function FindFilter(graph: IGraphBuilder; CLSID: TGUID): IBaseFilter;
var
  enum: IEnumFilters;
  f: cardinal;
  bf: IBaseFilter;
  ifo: _FilterInfo;
  csid: TGUID;
begin
  if failed(graph.EnumFilters(enum),'get enumfilters from graph') then
    exit;

  enum.Reset;
  while Succeeded(enum.Next(1, bf, nil),'next filter') do begin
    if bf = nil then
      break;

    bf.QueryFilterInfo(ifo);
    bf.GetClassID(csid);
    Debug(ifo.achName+ GUIDToString(csid));

    if csid = CLSID then begin
      result := bf;
      exit;
    end;


  end;

  result := nil;
  raise Exception.Create('could not find filter');





end;



function Succeeded(Res: HResult; sAnnotation: string): Boolean;
begin
  Result := Res and $80000000 = 0;
  if not result then
    Debug('Failed: '+inttohex(res,8)+ ' at '+sAnnotation);
end;

function Failed(Res: HResult; sAnnotation: string): Boolean;
begin
  Result := Res and $80000000 <> 0;
  if result then
    Debug('Failed: '+inttohex(res,8)+ ' at '+sAnnotation);
end;

function FindUnconnectedPin(filt: IBaseFilter; dir: PIN_DIRECTION): IPin;
var
  penum: IEnumPins;
  pin: IPin;
  bFound: boolean;
  hr: HRESULT;
begin
  pin := nil;
  penum := nil;
  bFound := false;
  hr := filt.EnumPins(penum);
  if failed(hr,'enum pins') then
    raise exception.create('failed to enum pins');

  while penum.Next(1, pin, nil) = S_OK do begin
    if IsPinMatch(pin, dir, false) then begin
      result := pin;
      exit;
    end;
  end;

  result := nil;
end;



function IsPinMatch(ppin: IPin; dir: PIN_DIRECTION; connected: boolean): boolean;
begin
  result := (IsPinConnected(ppin) = connected) and (GetPinDirection(ppin) = dir);
end;

function IsPinConnected(pPin: IPin): boolean;overload;
begin
  if not succeeded(IsPinConnected(ppin, result),'IsPinConnected') then
    raise Exception.Create('could not determine if pin was connected');
end;
function GetPinDirection(ppin: IPin): PIN_DIRECTION;
begin
  if not succeeded(ppin.QueryDirection(result),'QueryDirection') then
      raise exception.Create('could not query pin direction');
end;

function IsPinConnected(pPin: IPin; out res: boolean): HResult;
var
  pinto: IPin;
  hr: HRESULT;
begin
  hr := ppin.ConnectedTo(pinto);
  if succeeded(hr,'ConnectedTo') then begin
    res := true;
  end else begin
    if hr = VFW_E_NOT_CONNECTED then begin
      res := false;
    end;
  end;

end;

function AddFilterByCLSID(pgraph:IGraphBuilder; clsid:TGUID; out ppF:IBaseFilter; name: string): HRESULT;
var
  pFilter: IBaseFilter;
begin

    ppF := nil;
    pFilter := nil;

    AssertSuccess('Create filter for Add', CoCreateInstance(clsid, nil, CLSCTX_INPROC_SERVER, IBaseFilter, pFilter));
    AssertSuccess('Add filter to graph', pGraph.AddFilter(pFilter, pchar(name)));

    ppF := pFilter;

    result := result;
end;

function TVideoParser3.AdviseNotify(
  lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9): HResult;
var
  hMon: THandle;
begin
  dx.Lock;
  try
    san := lpIVMRSurfAllocNotify;
    hMon := dx.dx9window.pEnum.GetAdapterMonitor(D3DADAPTER_DEFAULT);
    result := san.SetD3DDevice(dx.dx9window.dev, hMon);
    result := S_OK;
  finally
    dx.Unlock;
  end;
end;

procedure TVideoParser3.CleanupVTps;
var
  t: integer;
begin
  if vtps <> nil then begin
    for t:= 0 to BUFFER_COUNT-1 do begin
      vtps[t].tex._Release;
      vtps[t].surf._Release;
      vtps[t].tex := nil;
      vtps[t].surf := nil;
    end;
    Freememory(vtps);
  end;

  vtps := nil;



end;

procedure TVideoParser3.CloseMedia;
begin
//  assertsuccess('remove clock', MediaFilter.SetSyncSource(nil));
  if assigned(clkObj) then begin
    clkObj.freeWithReferences := true;
  end;
  clkObj := nil;
  custom_clock := nil;


  CleanupVtps;
  san := nil;//IVMRSurfaceAllocatorNotify9;
  media := nil;
  MediaSeeking := nil;
  vmr9 := nil;
  mediacontrol := nil;
  vmr_mix_control := nil;
  vmr_win_control := nil;
  media := nil;//IMediaDet;
  Graph := nil;//IGraphBuilder;
  MediaSeeking := nil;//IMediaSeeking;
  MediaControl := nil;//IMediaControl;
  MediaEvent := nil;//IMediaEventEx;
  MEdiaFilter := nil;//IMediaFilter;
  GrabberFilter := nil;//IBaseFilter;
  Grabber := nil;//ISampleGrabber;
  NullFilter := nil;//IBaseFilter;
  vmr9 := nil;//IBaseFilter;
  custom_clock := nil;//IReferenceClock;
  clkObj := nil;//TMediaClock;
  vmr_mix_control := nil;//IVMRMixerControl9;
  vmr_win_control := nil;//IVMRWindowlessControl9;

  tex_video_memory := nil;//IDirect3dTExture9;
  if vtps <> nil then
    FreeMemory(vtps);
  vtps := nil;

end;



destructor TVideoParser3.Destroy;
begin
  CloseMedia;
  inherited;
end;

procedure TVideoParser3.Detach;
begin
  inherited;
  CloseMedia;

end;


function TVideoParser3.FileName: string;
begin
  result := FFileName;
end;



function TVideoParser3.GetSurface(dwUserID, SurfaceIndex, SurfaceFlags: DWORD;
  out lplpSurface: IDirect3DSurface9): HResult;
begin
  dx.lock;
  try
//    result := vtps[SurfaceIndex].sys.GetSurfaceLevel(0,lplpsurface);
    lplpSurface := vtps[SurfaceIndex].surf;
    result := S_OK;
//    AssertSuccess('get surface', result);
  finally
    dx.Unlock;
  end;


end;

procedure TVideoParser3.GotoTime(rTime: nativefloat);
begin
  seektime := rTime;



end;


procedure TVideoParser3.Init;
var
  t: nativeint;
begin
  inherited;
  w := 1920;
  h := 1080;

  for t:= 0 to FRAME_BUFFER_SIZE-1 do begin
    FFrameBuffers[t].Init;
  end;

end;

procedure TVideoParser3.InitFilterGraph_HardWay;
var
  hr: HRESULT;
  mt: _AMMediaType;
  bf1,bf2,bf3: IBaseFilter;
  p: iPIn;
  s: IDirectShowStream;
begin
(*
  hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
    TGUID(IGraphBuilder), Graph);
  if (FAILED(hr,'CoCreateInstance')) then begin
    Debug('Failed:'+inttostr(hr));
    exit;
  end;

  hr := Graph.QueryInterface(IMediaControl, MediaControl);
  if (FAILED(hr,'Graph.QueryInterface(IMediaControl...')) then
    exit;

  hr := Graph.QueryInterface(IMediaEvent, MediaEvent);

  if (FAILED(hr,'Graph.QueryInterface(IMediaEvent...')) then
    exit;

  ZeroMemory(@mt, sizeof(mt));
  mt.majortype := MEDIATYPE_Video;
  mt.subtype := MEDIASUBTYPE_RGB24;

//  CLSID_CAKEWALK_QUICKTIME_SOURCE_FILTER  :TGUID = '{44BA3F3D-A412-486B-95C9-9D63056E963C}';
//  CLSID_COLOR_SPACE_CONVERTER             :TGUID = '{1643E180-90F5-11CE-97D5-00AA0055595A}';
//  CLSID_VIDEO_MIXING_RENDERER_9           :TGUID = '{51B4ABF3-748F-4E3B-A276-C828330E926A}';

  if failed(graph.AddSourceFilter(pchar(FindMedia('Cee Lo Green - Bright Lights Bigger City.mp4')),pchar('saurce'), bf1),'AddSourceFilter') then
    exit;
//  AddFilterByCLSID(graph, CLSID_CAKEWALK_QUICKTIME_SOURCE_FILTER, bf1, 'Fram dah cakewalk yah');

//  AddFilterByCLSID(graph, CLSID_COLOR_SPACE_CONVERTER,            bf2, 'Fram dah cakewalk yah');
  AddFilterByCLSID(graph, {CLSID_VideoMixingRenderer}CLSID_VIDEO_MIXING_RENDERER_9,          bf3, 'Fram dah cakewalk yah');

  //connect filters
//  graph.Connect(FindUnconnectedPin(bf1, PINDIR_OUTPUT), FindUnconnectedPin(bf2, PINDIR_INPUT));
  graph.Connect(FindUnconnectedPin(bf1, PINDIR_OUTPUT), FindUnconnectedPin(bf3, PINDIR_INPUT));


//  hr := CoCreateInstance(CLSID_VideoMixingRenderer9, nil, CLSCTX_INPROC_SERVER, CLSID_VideoMixingRenderer9, vmr9);
  if (FAILED(hr,'Connect')) then begin
    Debug('Failed:'+inttostr(hr));
    exit;
  end;

  //try to get the stream from the source
  if Failed(Graph.QueryInterface(IMediaControl, MediaControl),'Graph.QueryInterface(IMediaControl...') then
    exit;

  if Failed(Graph.QueryInterface(IMediaFilter, MediaFilter),'Graph.QueryInterface(IMEdiaFilter...') then
    exit;


  clkObj := TMediaClock.Create;
  custom_clock := clkObj;

  MediaFilter.SetSyncSource(custom_clock);


  AssertSuccess('Run', MediaControl.Run);
    exit;


  exit;*)
end;

function TVideoParser3.InitializeDevice(dwUserID: DWORD;
  lpAllocInfo: PVMR9AllocationInfo; var lpNumBuffers: DWORD): HResult;
var
  t: integer;
  hand: THandle;
  tx: IDirect3dTexture9;
  surf: IDirect3dSurface9;
begin
  dx.Lock;
  try
    if not assigned(dx) then
      raise Exception.Create('dx not set');

    if vtps <> nil then
      FreeMemory(vtps);

    buffer_count := lpNumBuffers;
    vtps := getmemory(sizeof(TVideoTexture)*BUFFER_COUNT);
    ZeroMemory(vtps,sizeof(TVideoTexture)*BUFFER_COUNT);
    w :=   lpAllocInfo.dwWidth;
    h := lpallocInfo.dwHeight;
    lpAllocInfo.dwFlags := lpAllocInfo.dwFlags or  VMR9AllocFlag_TextureSurface;
  //  lpAllocInfo.Format := 21;
  //  lpAllocInfo.Pool := d3dpool_systemmem;

    //san.AllocateSurfaceHelper(lpAllocInfo, buffer_count, vtps[0]);

    for t:= 0 to buffer_count-1 do begin
      //dx.CreateVolatileTexturepair(w,h,vtps[t].sys, tex_video_memory,lpAllocInfo.Format, true);
{$IFDEF VID_TEX_IN_SYS_MEM}
      AssertSuccess('create texture with format hints', dx.dx9window.dev.CreateTexture(w,h,1,d3dusage_dynamic,lpAllocInfo.Format,D3dPOOL_SYSTEMMEM,tx,nil));
{$ELSE}
      AssertSuccess('create texture with format hints', dx.dx9window.dev.CreateTexture(w,h,1,d3dusage_dynamic,lpAllocInfo.Format,lpAllocInfo.Pool,tx,nil));
{$ENDIF}

      AssertSuccess('get surface from frame buffer', tx.GetSurfaceLevel(0, surf));
      vtps[t].tex := tx;
      vtps[t].surf := surf;
      tx._AddRef;
      surf._AddRef;

    end;

    //video memory texture
    AssertSuccess('create video memory texture', dx.dx9window.dev.CreateTexture(w,h,1,d3dusage_dynamic,21,d3dpool_default,tex_video_memory,nil));
    AssertSuccess('get surface from video texture', tex_video_memory.GetSurfaceLevel(0, surf_video_memory));
    tex_video_memory._AddRef;
    surf_video_memory._AddRef;
//    hand := 0;
//    AssertSuccess('create render target', dx.dx9window.dev.CreateRenderTarget(w,h,21, _D3DMULTISAMPLE_TYPE.D3DMULTISAMPLE_NONE, 0, true, surf_video_memory, @hand));

  {
    for t:= 0 to BUFFER_COUNT-1 do begin
      AssertSuccess('create tex '+inttostr(t), dx.dx9window.dev.CreateTexture(w,h,1,D3DUSAGE_DYNAMIC,lpAllocinfo.Format,lpAllocInfo.Pool,vtps[t].sys,nil));
    end;

    AssertSuccess('create video tex' ,dx.dx9window.dev.CreateTexture(w,h,1,0,21,d3dpool_default,tex_video_memory,nil));
                                    //Self.dx9window.dev.CreateTexture(w,h,1,D3DUSAGE_DYNAMIC,format            ,d3dpool_default,vid,nil));
   }
    result := S_OK;
  finally
    dx.Unlock;
  end;
//  raise Exception.Create('not implemented');
end;

procedure TVideoParser3.InitFilterGraph_EasyWay;
var
  hr: HRESULT;
  mt: _AMMediaType;
  bf1,bf2,bf3: IBaseFilter;
  p: iPIn;
  vmr9: IBaseFilter;
  cfg: IVMRFilterConfig9;
  src: IBaseFilter;
  r1,r2: TRECT;
begin

  hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
    TGUID(IGraphBuilder), Graph);
  if (FAILED(hr,'CoCreateInstance')) then begin
    Debug('Failed:'+inttostr(hr));
    exit;
  end;

  graph.RenderFile(pchar(FileName), nil);


  AssertSuccess('QueryInterface(IMediacontrol...',Graph.QueryInterface(IMediaControl, MediaControl));
  AssertSuccess('QueryInterface(IMediaSeeking...',Graph.QueryInterface(IMediaSeeking, MediaSeeking));

  if Failed(Graph.QueryInterface(IMediaFilter, MediaFilter),'QueryInterface(IMediaFilter...') then
    exit;


  AssertSuccess('Remove Video Renderer', Graph.RemoveFilter(FindFilter(graph, CLSID_VideoMixingRenderer)));
  AssertSuccess('Remove Audio Renderer', graph.RemoveFilter(FindFilter(graph,CLSID_DIRECT_SOUND_DEVICE)));


  AddFilterByCLSID(graph, CLSID_VIDEO_MIXING_RENDERER_9, vmr9, 'VMR9');
  //SETUP VMR RENDERING
  AssertSuccess('Get config interface from vmr9', vmr9.QueryInterface(IID_IVMRFilterConfig9, cfg));
  AssertSuccess('Set Windowless mode', cfg.SetRenderingMode(VMR9Mode_Renderless));
  AssertSuccess('get surface allocator notify', vmr9.QueryInterface(IID_IVMRSurfaceAllocatorNotify9, san));
  AssertSuccess('advise surface allocator', san.AdviseSurfaceAllocator(userid, self));
  AssertSuccess('advise presenter', self.AdviseNotify(san));

//
{$IFDEF WINDOWLESS_MODE}
  AssertSuccess('Get window control from vmr9', vmr9.QueryInterface(IID_IVMRWindowlessControl9, vmr_win_control));
  r1 := Rect(0,0,256,256);
  r2 := rect(0,0,256,256);
  vmr_win_control.SetVideoPosition(nil, @r2);
  vmr_win_control.SetVideoClippingWindow(Self.ClippingWindow);
{$ENDIF}


  clkobj := TMediaClock.Create;
  custom_clock := clkObj;
  MediaFilter.SetSyncSource(custom_clock);

  //connect vmr9 - AFTER properties are set!
  src := FindFilter(graph, CLSID_COLOR_SPACE_CONVERTER);
  AssertSuccess('Connect VMR9', graph.Connect(FindUnconnectedPin(src, PINDIR_OUTPUT), FindUnconnectedPin(vmr9, PINDIR_INPUT)));




  AssertSuccess('run', MediaControl.Run);







end;

procedure TVideoParser3.InitFilterGraph;
begin
  InitFilterGraph_EasyWay;

end;

procedure TVideoParser3.InitMedia(sfile: string);
var
  StreamLength: double;
  p: pbyte;
  sz: longint;
  fb: TFastBitmap;
  bm: TBitmap;
  t: integer;
  rLen: double;
begin
  CloseMedia;
  if not fileexists(sFile) then
    exit;
  FFIleName := sFile;
  CoInitialize(nil);
  CoCreateInstance(CLSID_MediaDet, nil, CLSCTX_INPROC_SERVER, IID_IMediaDet,
    media);
  media.put_Filename(sfile);
  media.put_CurrentStream(0);

  media.put_CurrentStream(0);
  InitFilterGRaph;


end;






function TVideoParser3.PresentImage(dwUserID: DWORD;
  lpPresInfo: PVMR9PresentationInfo): HResult;
var
  tex: IDirect3DTexture9;
  surf, src,dest: IDirect3DSurface9;
  t: nativeint;
  plocked: _D3DLOCKED_RECT;
begin

{$IFDEF VID_TEX_IN_SYS_MEM}
//  dx.UpdateVolatileTexture(vtps[0].tex, tex_video_memory);
{$ENDIF}

  frame_ready := true;


  while not frame_presented do  ;

  frame_ready := false;

  while frame_presented do  ;

  exit;





//  dx.Lock;
//  try
//    for t:= 0 to BUFFER_COUNT-1 do begin
//      tex := vtps[t].sys;
//      tex.GetSurfaceLevel(0,surf);
//      if surf = lpPresInfo.lpSurf then
//        break;
//    end;
{$IFDEF UPDATE_SURFACE_IN_THREAD}
//    if (tex_video_memory.LockRect(0, plocked, nil, D3DLOCK_DISCARD { or D3DLOCK_NOOVERWRITE } ) = S_OK) then
    try
//      tex_video_memory.GetSurfaceLevel(0,surf);
//      assertsuccess('present image',dx.dx9window.dev.UpdateSurface(lpPresInfo.lpSurf, nil, surf, nil));

        dx.Lock;
        try


          //dx.UpdateVolatileTexture(vtps[0].sys, tex_video_memory);
          //dx.UpdateVolatileSurface(lpPresInfo.lpSurf, Self.surf_video_memory);
          //dx.CopySurface_VidMem(lpPresInfo.lpSurf, Self.surf_video_memory);
          //assertsuccess('stretchrect', dx.dx9window.dev.StretchRect(lpPresInfo.lpSurf, nil, surf_video_memory, nil, 0));







//          tex_video_memory_private.GetSurfaceLevel(0, src);
//          tex_video_memory.GetSurfaceLevel(0, dest);
//          dx.CopySurface_VidMem(src, dest);



          //m_Device.StretchRectangle(_source, _target, TextureFilter.None);
        finally
          dx.Unlock;
        end;
    finally
//      tex_video_memory.UnlockRect(0);
    end;
{$ENDIF}

//    tex := vtps[0].sys;
//    dx.UpdateVolatileTexture(tex, tex_video_memory);

//  finally
//    dx.Unlock;
//  end;
  result := S_OK;
//  raise Exception.Create('not implemented');
end;

procedure TVideoParser3.SEtAllocatorPResenter(const Value: TvideoAllocatorPresenter);
begin
  if value = allocator_presenter then
    exit;

  if assigned(allocator_presenter) then
    allocator_presenter.Detach(self);

  allocator_presender := value
  if assigned
end;

function TVideoParser3.StartPresenting(dwUserID: DWORD): HResult;
begin
  result := S_OK;
//  raise Exception.Create('not implemented');
end;


function TVideoParser3.StopPresenting(dwUserID: DWORD): HResult;
begin
  result := S_OK;
//  raise Exception.Create('not implemented');
end;


function TVideoParser3.TerminateDevice(dwID: DWORD): HResult;
begin
  dx.Lock;
  try
    CleanupVtps;
    result := S_OK;
  finally
    dx.Unlock;
  end;
//  raise Exception.Create('not implemented');
end;


{ TVideoBitmap }

procedure TVideoBitmap.ApplytoTexture(tex: IDirect3dTexture9);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TVideoBitmap.Cleanup;
begin
  if p <> nil then
    FreeMemory(p);
  size := 0;
end;

function TVideoBitmap.Data: pbyte;
begin
  result := p + sizeof(BITMAPINFOHEADER);
end;

function TVideoBitmap.Header: PBITMAPINFOHEADER;
begin
  result := PBITMAPINFOHEADER(p);
end;

procedure TVideoBitmap.Init;
begin
  p := nil;
  size := 0;
  Available := true;
end;

procedure TVideoBitmap.NeedSize(iSize: nativeint);
begin
  if size < iSize then begin
    p := GetMemory(iSize);
    size := iSize;
  end;
end;

procedure TVideoBitmap.NoNeed;
begin
  available := true;
end;

{ TVideoReadAheadThread }


{ TMediaClock }

function TMediaClock.AdvisePeriodic(const rtStartTime,
  rtPeriodTime: TReferenceTime; hSemaphore: THandle;
  out pdwAdviseCookie: DWORD): HResult;
begin
  //this function deals with a period event... I don't think I need it
  raise Exception.Create('not supported');
  result := S_OK;

end;

function TMediaClock.AdviseTime(rtBaseTime, rtStreamTime: TReferenceTime;
  hEvent: THandle; out pdwAdviseCookie: DWORD): HResult;
var
  adv: TTimeAdviseRequest;
begin
  //this function generates an event at a particular time in the stream... I don't think I need it
  //if I were to implement it, I would create a list of advise requests and check
  //the clock against the reference time and fire events at the appropriate trigger time

  adv := TTimeAdviseRequest.Create;
  Lock;
  try
    adv.basetime := 0;
    adv.timetoadvise := rtStreamTime;
    adv.hEvent := hEvent;
    adv.cookie := FAdviseCookieID;
    inc(FAdviseCookieID);
    FAdviseRequests.Add(adv);
  finally
    Unlock;
  end;


  pdwAdviseCookie := 0;
  result := S_OK;

end;

destructor TMediaClock.Destroy;
begin
  StopThread;

  FAdviseRequests.Free;
  FAdviseRequests := nil;

  inherited;
end;

function TMediaClock.GEtTime: TReferenceTime;
begin
  result := TReferenceTime(ExternalTime);
end;

function TMediaClock.GetExternalTime: TReferenceTime;
begin
  Lock;
  try
    result := FExternalTime;
  finally
    Unlock;
  end;
end;

function TMediaClock.GetTime(out pTime: TReferenceTime): HResult;
begin
  ptime := GetTime;
  result := S_OK;
end;

function TMediaClock.IndexOfAdvise(dwAdviseCookie: DWORD): nativeint;
var
  t: integer;
begin
  result := -1;
  Lock;
  try
    for t:= 0 to FAdviseRequests.Count-1 do begin
      if FAdviseRequests[t].cookie = dwAdviseCookie then begin
        result := t;
        exit;
      end;
    end;
  finally
    Unlock;
  end;

end;

procedure TMediaClock.Init;
begin
  inherited;
  Self.FAdviseRequests := TList<TTimeAdviseRequest>.create;
  StartThread;
end;

procedure TMediaClock.OnThreadExecute(sender: TExternalEventThread);
begin
  sleep(100);
  //self.UpdateAdviseEvents;
end;

procedure TMediaClock.SetExternalTime(const Value: TReferenceTime);
begin
  Lock;
  try
    FExternalTime := Value;
  finally
    Unlock;
  end;
  self.UpdateAdviseEvents;
end;

procedure TMediaClock.StartThread;
begin
  StopThread;
  thr := TMediaClockThread.Create(self, nil, true);
  thr.OnExecute := Self.OnThreadExecute;
  thr.loop := true;
  thr.resume;
end;

procedure TMediaClock.StopThread;
begin
  if assigned(thr) then begin
    thr.terminate;
    thr.waitfor;
    thr.free;
  end;
  thr := nil;
end;

function TMediaClock.Unadvise(dwAdviseCookie: DWORD): HResult;
var
  i: nativeint;
begin
  //this function removes an event request... I don't think I need it
    result := S_OK;
  Lock;
  try
    i := IndexOfAdvise(dwAdviseCookie);
    if i >=0 then
      FAdviseRequests.delete(i);
  finally
    unlock;
  end;

end;

procedure TMediaClock.UpdateAdviseEvents;
var
  t: integer;
begin
  Lock;
  try
    for t:= FAdviseRequests.Count-1 downto 0 do begin
      FAdviseRequests[t].SignalIfTime(GEtTime);
    end;
  finally
    Unlock;
  end;

end;

{ TTimeAdviseRequest }

procedure TTimeAdviseRequest.SignalIftime(refTime: TReferenceTime);
begin
  //exit if already done
  if bComplete then exit;

  if refTime > (self.basetime+self.timetoadvise) then begin
    SetEvent(hEvent);
    bComplete := true;
  end;
end;

{ TVideoAllocatorPresenter }

function TVideoAllocatorPresenter.AdviseNotify(
  lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9): HResult;
begin
  //todo 1: implement
end;
procedure TVideoAllocatorPresenter.Attach(vp: TVideoParser3);
begin
  Lock;
  try
    FParsers.Remove(vp);
    vp.userid := FNextUserID;
  finally
    Unlock;
  end;
end;

destructor TVideoAllocatorPresenter.Destroy;
begin
  FParsers.Free;
  inherited;
end;

procedure TVideoAllocatorPresenter.Detach(vp: TVideoParser3);
begin
  Lock;
  try
    FParsers.Remove(vp);
  finallly
    Unlock;
  end;
end;

function TVideoAllocatorPresenter.FindUser(dwUserID: dword;
  out vp3: TVideoParser3): boolean;
var
  t: nativeint;
begin
  vp3 := nil;
  result := false;
  Lock;
  try
    if FParsers[t].userid = dwUserID then begin
      vp3 := FParsers[t];
      result := true;
      exit;
    end;
  finally
    Unlock;
  end;
end;

function TVideoAllocatorPresenter.GetSurface(dwUserID, SurfaceIndex,
  SurfaceFlags: DWORD; out lplpSurface: IDirect3DSurface9): HResult;
var
  vp3: TVideoParser3;
begin
  Lock;
  try
    if FindUser(dwUserID, vp3) then
      result := vp3.GetSurface(surfaceindex, surfaceflags, lplpSurface)
    else
      result := D3DERR_INVALIDDEVICE;
  finally
    Unlock;
  end;
end;

procedure TVideoAllocatorPresenter.Init;
begin
  inherited;
  FParsers := TList< TVideoParser3>.create;
end;

function TVideoAllocatorPresenter.InitializeDevice(dwUserID: DWORD;
  lpAllocInfo: PVMR9AllocationInfo; var lpNumBuffers: DWORD): HResult;
var
  vp3: TVideoParser3;
begin
  Lock;
  try
    if FindUser(dwUserID, vp3) then
      result := vp3.InitializeDefice(surfaceindex, surfaceflags, lplpSurface)
    else
      result := D3DERR_INVALIDDEVICE;
  finally
    Unlock;
  end;

end;

function TVideoAllocatorPresenter.PresentImage(dwUserID: DWORD;
  lpPresInfo: PVMR9PresentationInfo): HResult;
var
  vp3: TVideoParser3;
begin
  Lock;
  try
    if FindUser(dwUserID, vp3) then
      result := vp3.InitializeDefice(surfaceindex, surfaceflags, lplpSurface);
  finally
    Unlock;
  end;

end;

function TVideoAllocatorPresenter.StartPresenting(dwUserID: DWORD): HResult;
var
  vp3: TVideoParser3;
begin
  Lock;
  try
    if FindUser(dwUserID, vp3) then
      result := vp3.StartPresenting;
  finally
    Unlock;
  end;

end;

function TVideoAllocatorPresenter.StopPresenting(dwUserID: DWORD): HResult;
var
  vp3: TVideoParser3;
begin
  Lock;
  try
    if FindUser(dwUserID, vp3) then
      result := vp3.StopPresenting;
  finally
    Unlock;
  end;

end;

function TVideoAllocatorPresenter.TerminateDevice(dwID: DWORD): HResult;
var
  vp3: TVideoParser3;
begin
  Lock;
  try
    if FindUser(dwID, vp3) then
      result := vp3.TerminateDevice()
    else
      result := D3DERR_INVALIDDEVICE;
  finally
    Unlock;
  end;

end;

end.
