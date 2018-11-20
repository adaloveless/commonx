unit VideoParser2;

interface
uses
  typex, windows, advancedgraphics_dx, easyimage, directshow9, direct3d9_jedi, graphics,
  sysutils, fileserviceClientEx, managedthread, d3dx9, sharedobject,
  types, activex, betterobject, classes, commandprocessor, memoryfilestream, debug,
  systemx, generics.Collections, tools, fastbitmap;

const
  CLSID_CAKEWALK_QUICKTIME_SOURCE_FILTER  :TGUID = '{44BA3F3D-A412-486B-95C9-9D63056E963C}';
  CLSID_COLOR_SPACE_CONVERTER             :TGUID = '{1643E180-90F5-11CE-97D5-00AA0055595A}';
  CLSID_VIDEO_MIXING_RENDERER_9           :TGUID = '{51B4ABF3-748F-4E3B-A276-C828330E926A}';
  CLSID_DIRECT_SOUND_DEVICE               :TGUID = '{79376820-07D0-11CF-A24D-0020AFD79767}';

  FRAME_BUFFER_SIZE = 2;
type
  PBITMAPINFOHEADER = ^BITMAPINFOHEADER;

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

  TVideoPArser2 = class;//forward

  TVideoReadAheadThread = class(TManagedThread)
  private
    FParser: TVideoParser2;
  public
    procedure DoExecute;override;
    property Parser: TVideoParser2 read FParser write FParser;
  end;


  TVideoParser2 = class(TSharedObject)
  private
    FFrameBuffers: array[0..FRAME_BUFFER_SIZE-1] of TVideoBitmap;
    FOnFrameComplete: TNotifyEvent;
    FEstLen: real;
    FReserved: boolean;
    FClippingwindow: HWND;
    FDX: TDX2d;
  public
    lastw: nativeint;
    lasth: nativeint;
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
    thr: TVideoReadAheadThread;
    iLastFrame: integer;
    rFrameRate: double;
    seektime: nativefloat;
    custom_clock: IReferenceClock;
    vmr_mix_control: IVMRMixerControl9;
    vmr_win_control: IVMRWindowlessControl9;

    procedure Init;override;
    destructor Destroy;override;//
    procedure InitMedia(sfile: string);//
    procedure InitFilterGraph;
    procedure InitFilterGraph_EasyWay;
    procedure InitFilterGraph_HardWay;

    function IndexOfCachedFrame(framenumber: nativeint): nativeint;

    function GetCachedFrame(idx: nativeint): PVideoBitmap;
    procedure Direct_GetFrameBits(rTime: nativefloat; bm: PVideoBitmap);

    procedure LoadVideoFrameToTexture(rTime: double; var tex_sys, tex_vid: IDirect3dTexture9; dx: TDX2d; texw, texh: nativeint);
    procedure CloseMedia;
    property OnFrameComplete
      : TNotifyEvent read FOnFrameComplete write FOnFrameComplete;
    property EstimatedLength: real read FEstLen write FEstLen;
    property Reserved: boolean read FReserved write FReserved;
    procedure StartThread;
    procedure StopThread;
    procedure GotoTime(rTime: nativefloat);
    function AllReady: boolean;

    function TimeToFrameNumber(rTime: nativefloat): nativeint;
    function FrameNumberToTime(iFrameNumber: nativeint): nativefloat;
    property ClippingWindow: HWND read FClippingwindow write FClippingWindow;
    property dx: TDX2d read FDX write FDX;
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

{ TVideoParser2 }
procedure ASSERTSUCCESS(sAnnotation: string; Res: HRESULT);
begin
  if not SUCCEEDED(res, sAnnotation) then
    raise Exception.create('Assertion failed at: '+sAnnotation);
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
    Debug.log(ifo.achName+ GUIDToString(csid));

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
    Debug.log('Failed: '+inttohex(res,8)+ ' at '+sAnnotation);
end;

function Failed(Res: HResult; sAnnotation: string): Boolean;
begin
  Result := Res and $80000000 <> 0;
  if result then
    Debug.log('Failed: '+inttohex(res,8)+ ' at '+sAnnotation);
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

function TVideoParser2.AllReady: boolean;
var
  t: integer;
begin
  result := true;
  for t:= 0 to FRAME_BUFFER_SIZE-1 do begin
    if FFrameBuffers[t].ready = false then begin
      result := false;
      break;
    end;
  end;


end;

procedure TVideoParser2.CloseMedia;
begin
  media := nil;
  MediaSeeking := nil;
  vmr9 := nil;
  mediacontrol := nil;
  vmr_mix_control := nil;
  vmr_win_control := nil;

end;

destructor TVideoParser2.Destroy;
begin
  StopThread;
  CloseMedia;
  inherited;
end;

procedure TVideoParser2.Direct_GetFrameBits(rTime: nativefloat;
  bm: PVideoBitmap);
begin
//  bm.frametime := rTime;
  if bm.p = nil then begin
    bm.size := 1920 * 1080 * 5;
    bm.p := GetMemory(bm.size);
  end;
  bm.framenumber := round(rTime * self.rFrameRate);
  assert(media.GetBitmapBits(rTime, @bm.size, bm.p, lastw, lasth)=0);

end;

function TVideoParser2.FrameNumberToTime(iFrameNumber: nativeint): nativefloat;
begin
  result := iFramenumber / rFrameRate;
end;

function TVideoParser2.GetCachedFrame(idx: nativeint): PVideoBitmap;
var
  i: integer;
begin
  result := nil;
  Lock;
  try
    i := Self.IndexOfCachedFrame(idx);
    if i >=0 then
      result := @Self.FFrameBuffers[i];
  finally
    Unlock;
  end;


end;

procedure TVideoParser2.GotoTime(rTime: nativefloat);
begin
  seektime := rTime;



end;

function TVideoParser2.IndexOfCachedFrame(framenumber: nativeint): nativeint;
var
  t: integer;
begin
  result := -1;
  Lock;
  try
    for t:= 0 to FRAME_BUFFER_SIZE-1 do begin
      if FFrameBuffers[t].framenumber = framenumber then begin
        result := t;
        break;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TVideoParser2.Init;
var
  t: nativeint;
begin
  inherited;
  lastw := 1920;
  lasth := 1080;

  for t:= 0 to FRAME_BUFFER_SIZE-1 do begin
    FFrameBuffers[t].Init;
  end;

end;

procedure TVideoParser2.InitFilterGraph_HardWay;
var
  hr: HRESULT;
  mt: _AMMediaType;
  bf1,bf2,bf3: IBaseFilter;
  p: iPIn;
  s: IDirectShowStream;
begin

  hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
    TGUID(IGraphBuilder), Graph);
  if (FAILED(hr,'CoCreateInstance')) then begin
    Debug.log('Failed:'+inttostr(hr));
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
    Debug.log('Failed:'+inttostr(hr));
    exit;
  end;

  //try to get the stream from the source
  if Failed(Graph.QueryInterface(IMediaControl, MediaControl),'Graph.QueryInterface(IMediaControl...') then
    exit;

  if Failed(Graph.QueryInterface(IMediaFilter, MediaFilter),'Graph.QueryInterface(IMEdiaFilter...') then
    exit;


  custom_clock := TMediaClock.Create;
  MediaFilter.SetSyncSource(custom_clock);


  AssertSuccess('Run', MediaControl.Run);
    exit;


  exit;
end;

procedure TVideoParser2.InitFilterGraph_EasyWay;
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
    Debug.log('Failed:'+inttostr(hr));
    exit;
  end;

  graph.RenderFile(pchar(FindMedia('Cee Lo Green - Bright Lights Bigger City.mp4')), nil);


  AssertSuccess('QueryInterface(IMediacontrol...',Graph.QueryInterface(IMediaControl, MediaControl));
  AssertSuccess('QueryInterface(IMediaSeeking...',Graph.QueryInterface(IMediaSeeking, MediaSeeking));

  if Failed(Graph.QueryInterface(IMediaFilter, MediaFilter),'QueryInterface(IMediaFilter...') then
    exit;


  AssertSuccess('Remove Video Renderer', Graph.RemoveFilter(FindFilter(graph, CLSID_VideoMixingRenderer)));
  AssertSuccess('Remove Audio Renderer', graph.RemoveFilter(FindFilter(graph,CLSID_DIRECT_SOUND_DEVICE)));


  custom_clock := TMediaClock.Create;
  MediaFilter.SetSyncSource(custom_clock);

  AddFilterByCLSID(graph, CLSID_VIDEO_MIXING_RENDERER_9, vmr9, 'VMR9');
  //SETUP VMR RENDERING
  AssertSuccess('Get config interface from vmr9', vmr9.QueryInterface(IID_IVMRFilterConfig9, cfg));
  AssertSuccess('Set Windowless mode', cfg.SetRenderingMode(VMR9Mode_Windowless));
//
  AssertSuccess('Get window control from vmr9', vmr9.QueryInterface(IID_IVMRWindowlessControl9, vmr_win_control));
  r1 := Rect(0,0,256,256);
  r2 := rect(0,0,256,256);
  vmr_win_control.SetVideoPosition(nil, @r2);
  vmr_win_control.SetVideoClippingWindow(Self.ClippingWindow);

  AssertSuccess('Get window control from vmr9', vmr9.QueryInterface(IID_IVMRWindowlessControl9, vmr_win_control));

  //connect vmr9 - AFTER properties are set!
  src := FindFilter(graph, CLSID_COLOR_SPACE_CONVERTER);
  AssertSuccess('Connect VMR9', graph.Connect(FindUnconnectedPin(src, PINDIR_OUTPUT), FindUnconnectedPin(vmr9, PINDIR_INPUT)));


  MediaControl.Run;







end;

procedure TVideoParser2.InitFilterGraph;
begin
  exit;
  InitFilterGraph_EasyWay;

end;

procedure TVideoParser2.InitMedia(sfile: string);
var
  StreamLength: double;
  p: pbyte;
  sz: longint;
  fb: TFastBitmap;
  bm: TBitmap;
  t: integer;
  rLen: double;
begin
  CoInitialize(nil);
  CoCreateInstance(CLSID_MediaDet, nil, CLSCTX_INPROC_SERVER, IID_IMediaDet,
    media);
  media.put_Filename(sfile);
  media.put_CurrentStream(0);
  media.get_StreamLength(rLen);
  rLen := rLen / 4;
  rLen := rLen / 44100;
  EstimatedLength := rLen;
  media.put_CurrentStream(0);
  InitFilterGRaph;


  media.get_FrameRate(rFrameRate);
  media.EnterBitmapGrabMode(0.0);

  IF rFrameRate = 0 then
    rFrameRate := 30;


  StartThread;
end;


procedure TVideoParser2.LoadVideoFrameToTexture(rTime: double; var tex_sys, tex_vid: IDirect3dTexture9; dx: TDX2d; texw, texh: nativeint);
var
  p,pb,ppb: pbyte;
  plocked: _D3DLOCKED_RECT;
  r: TRect;
  t, u: integer;
  sz: integer;
  iframe: integer;
  bithead: PBITMAPINFOHEADER;
  a: nativeint;
  v: PVideoBitmap;
begin
  GotoTime(rTime);
  if (tex_sys = nil) or (tex_vid = nil) or (texw <> lastw) or (texh <> lasth) then begin
    dx.CreateVolatileTexturePair(texw,texh, tex_sys, tex_vid);
    lastw := texw;
    lasth := texh;
  end;

  sz := texw * texh * 5;
  try
    iframe := trunc(rTime * rFrameRate);

    if iframe = iLastFrame then
      exit;

    iLastFrame := iframe;
//    movie_texture_sys := CreateVolatileTexture(512,512);


    if assigned(tex_sys) then begin
      v := nil;
      while v = nil do begin
        v := GetCachedFrame(TimeToFrameNumber(rTime));
//        if v = nil then
//          sleep(0);
      end;

      while not v.ready do
        {sleep(0)};


      p := v.p;

      if True then begin
        bithead := PBITMAPINFOHEADER(p);

        inc(p, sizeof(BITMAPINFOHEADER));
        if succeeded(tex_sys.LockRect(0, plocked, nil, D3DLOCK_DISCARD
            { or D3DLOCK_NOOVERWRITE } ),'Lock Rect') then
          try
            pb := plocked.pBits;
            if assigned(p) then begin
              for u := bithead.biheight-1 downto 0 do begin
                ppb := @pb[u*plocked.Pitch];
                a := round((1-(u/bithead.biHeight))*255);
                for t := 0 to (bithead.biWidth-1) do begin
                  PDWordArray(ppb)[0] := PDWordArray(p)[0] or $FF000000; inc(ppb,4); inc(p,3);
//                  ppb[0] := p[0]; inc(ppb); inc(p);
//                  ppb[0] := p[0]; inc(ppb); inc(p);
//                  ppb[0] := p[0]; inc(ppb); inc(p);
//                  ppb[0] := a; inc(ppb);
                end;
              end;
            end;
          finally
            tex_sys.UnlockRect(0);
          end;
      end;
    end;
  finally
    //FreeMemory(p);
  end;

//  GotoTime(rTime+(1/rFrameRate));
end;




procedure TVideoParser2.StartThread;
begin
  StopThread;
  thr := TVideoReadAheadThread.Create(self, nil);
  thr.Parser := self;
  thr.Start;
end;

procedure TVideoParser2.StopThread;
begin
  if assigned(thr) then begin
    thr.Terminate;
    thr.WaitFor;
    thr.Free;
  end;
  thr := nil;
end;

function TVideoParser2.TimeToFrameNumber(rTime: nativefloat): nativeint;
begin
  result := round(rTime * rFrameRate);
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

procedure TVideoReadAheadThread.DoExecute;
var
  t: integer;
  v: PVideoBitmap;
  iStartFrame,ii: nativeint;
begin
  inherited;
  CoInitialize(nil);
  while not terminated do begin

    iStartFrame := parser.TimeToFrameNumber(parser.seektime);
    ii := iStartFrame;


    //under lock... setup the array
    Parser.Lock;
    try
      for t := 0 to FRAME_BUFFER_SIZE-1 do begin
        v := @FParser.FFrameBuffers[t];
        if v.available or (v.framenumber < iStartFrame) then begin
          v.Available := false;
          v.ready := false;
          v.framenumber := ii;
          v.frametime := parser.FrameNumberToTime(ii);
          inc(ii);
        end;
      end;
    finally
      Parser.Unlock;
    end;

    //work to complete the array... not under lock
    for t := 0 to FRAME_BUFFER_SIZE-1 do begin
      v := @FParser.FFrameBuffers[t];
      if v.framenumber = iStartFrame then
        Priority := tpHighest
      else
        Priority := tpLower;
      if (not v.ready) then begin
        parser.Direct_GetFrameBits(v.frametime, v);

        v.ready := true;
      end;
      if parser.TimeToFrameNumber(parser.seektime) <> iStartFrame then
        break;//something changed... give up
    end;
  end;



end;

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
    adv.basetime := rtBaseTime;
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
  result := TReferenceTime(GetTickCount)*TReferenceTime(10000);
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
  self.UpdateAdviseEvents;
end;

procedure TMediaClock.StartThread;
begin
  StopThread;
  thr := TMediaClockThread.Create(self, nil);
  thr.OnExecute := Self.OnThreadExecute;
  thr.loop := true;
  thr.start;
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

end.
