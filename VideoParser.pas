unit VideoParser;
{x$INLINE AUTO}
{$DEFINE SERVER_COMPATIBLE}

interface

uses
  windows, advancedgraphics_dx, easyimage, directshow9, direct3d9_jedi, graphics,
  sysutils, fileserviceClientEx,
  types, activex, betterobject, classes, commandprocessor, memoryfilestream,
  miscroutines, generics.Collections.fixed;

const
  HUESTOGRAM_FRAMETIME = 1 / 30;

type
  TVideoParser = class(TBetterObject)
  private
    FOnFrameComplete: TNotifyEvent;
    FEstLen: real;
    FReserved: boolean;
  public
    media: IMediaDet;
    Graph: IGraphBuilder;
    MediaControl: IMediaControl;
    MediaEvent: IMediaEventEx;
    GrabberFilter: IBaseFilter;
    Grabber: ISampleGrabber;
    NullFilter: IBaseFilter;
    movie_texture: TFastBitmap;
    resized_texture: TFastBitmap;

    iLastFrame: integer;
    rFrameRate: double;
    destructor Destroy;override;
    procedure InitMedia(sfile: string);
    procedure InitFilterGraph;
    procedure LoadVideoFrameToTexture(rTime: double);
    procedure LoadVideoFrameToFastBitmap(rTime: double; w, h: integer;
      out fb: TFastBitmap);
    procedure CloseMedia;
    property OnFrameComplete
      : TNotifyEvent read FOnFrameComplete write FOnFrameComplete;
    property EstimatedLength: real read FEstLen write FEstLen;
    property Reserved: boolean read FReserved write FReserved;
  end;

  Tcmd_RenderHuestogram = class(TCommand)
  private
    FFileName: string;
    FWidth: integer;
    FHeight: integer;
    FLengthInSeconds: real;
    FRemote: boolean;
    vps: TList<TVideoParser>;
  public
    procedure DoExecute; override;
    procedure DoExecuteLocal;
    procedure DoExecuteRemote;
    procedure DoExecuteLocalMulti;

    property FileName: string read FFileName write FFileName;
    property Width: integer read FWidth write FWidth;
    property height: integer read FHeight write FHeight;
    property LengthinSeconds: real read FLengthInSeconds write FLengthInSeconds;
    property Remote: boolean read FRemote write FRemote;
    function NeedVp: TVideoParser;
    procedure NoNeedVp(vp: TVideoParser);

  end;

  Tcmd_RenderHuestogramFrame = class(TCommand)
  private
    FOutput: Thuestogram;
    FTime: real;
    FVP: TVideoParser;
    FMasterCommand: Tcmd_RenderHuestogram;

  public
    procedure InitExpense;override;
    Destructor Destroy; override;

    procedure DoExecute; override;

    property vp: TVideoParser read FVP write FVP;
    property FrameTime: real read FTime write FTime;
    property Output: Thuestogram read FOutput;
    property MasterCommand
      : Tcmd_RenderHuestogram read FMasterCommand write FMasterCommand;

  end;

implementation

{ TDX2d_UT }

function ConnectFilters(pGraph: IGraphBuilder; // Filter Graph Manager.
  pOut: IPin; // Output pin on the upstream filter.
  pDest: IBaseFilter): HRESULT; // Downstream filter
var
  hr: HRESULT;
  pin: IPin;
  pins: IENumPins;
begin

    // Find an input pin on the downstream filter.
  hr := pDest.EnumPins(pins);
  if not succeeded(hr) then begin
    result := hr;
    exit;
  end;
  hr := pDest.EnumPins(pins);
  if (succeeded(hr)) then begin
        // Try to connect them.
    hr := pGraph.Connect(pOut, pin);
    pin := nil;
  end;
  result := hr;
end;

procedure TVideoParser.CloseMedia;
begin

  media := nil;

end;

destructor TVideoParser.Destroy;
begin
  CloseMedia;
  inherited;
end;

procedure TVideoParser.InitFilterGraph;
var
  hr: HRESULT;
  mt: _AMMediaType;
begin
  hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
    TGUID(IGraphBuilder), Graph);
  if (FAILED(hr)) then
    exit;

  hr := Graph.QueryInterface(IMediaControl, MediaControl);
  if (FAILED(hr)) then
    exit;

  hr := Graph.QueryInterface(IMediaEvent, MediaEvent);

  if (FAILED(hr)) then
    exit;

  ZeroMemory(@mt, sizeof(mt));
  mt.majortype := MEDIATYPE_Video;
  mt.subtype := MEDIASUBTYPE_RGB24;

  hr := Grabber.SetMediaType(mt);
  if (FAILED(hr)) then
    exit;

    // Create the Sample Grabber filter.
  hr := CoCreateInstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC_SERVER,
    IBaseFilter, GrabberFilter);
  if (FAILED(hr)) then
    exit;

  hr := Graph.AddFilter(GrabberFilter, pchar('Sample Grabber'));
  if (FAILED(hr)) then
    exit;

  hr := GrabberFilter.QueryInterface(ISampleGrabber, Grabber);
  if (FAILED(hr)) then
    exit;

    // connect filters

  hr := CoCreateInstance(CLSID_NullRenderer, nil, CLSCTX_INPROC_SERVER,
    IBaseFilter, NullFilter);
  if (FAILED(hr)) then
    exit;

  hr := Graph.AddFilter(NullFilter, pchar('Null Filter'));
  if (FAILED(hr)) then
    exit;

// hr := Graph.Connect(NullFilter., GrabberFilter);
  if (FAILED(hr)) then
    exit;

end;

procedure TVideoParser.InitMedia(sfile: string);
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

  media.get_FrameRate(rFrameRate);
  media.EnterBitmapGrabMode(0.0);

  IF rFrameRate = 0 then
    rFrameRate := 30;
end;

procedure TVideoParser.LoadVideoFrameToFastBitmap(rTime: double; w, h: integer;
  out fb: TFastBitmap);
var
  op, p, pb: pbyte;
  plocked: _D3DLOCKED_RECT;
  r: TRect;
  t, u: integer;
  sz: integer;
  iframe: integer;
  sg: ISampleGrabber;

begin
  sz := w * h * 4;
  GetMem(p, sz);
  op := p;
  try
    iframe := trunc(rTime * rFrameRate);

    iLastFrame := iframe;

    fb := TFastBitmap.create;
    fb.Width := w;
    fb.height := h;
    fb.New;

    if assigned(fb) then begin
      // media.GetSampleGrabber(sg);

      if succeeded(media.GetBitmapBits(rTime, @sz, p, w, h)) then begin
        inc(p, sizeof(BITMAPINFOHEADER));

        if assigned(p) then begin
          for u := fb.height - 1 downto 0 do begin
            for t := 0 to fb.Width - 1 do begin
              if (t mod 4) <> 0 then begin inc(p,3); continue; end;
              if (u mod 4) <> 0 then begin inc(p, 3);continue; end;
              fb.Canvas.Pixels[t, u] := p[2] + (p[1] shl 8) + (p[0] shl 16);

            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(op);
  end;
end;

procedure TVideoParser.LoadVideoFrameToTexture(rTime: double);
var
  p, pb: pbyte;
  plocked: _D3DLOCKED_RECT;
  r: TRect;
  t, u: integer;
  sz: integer;
  iframe: integer;
  sg: ISampleGrabber;
begin
  sz := 512 * 512 * 4;
  GetMem(p, sz);
  iframe := trunc(rTime * rFrameRate);

  if iframe = iLastFrame then
    exit;

  iLastFrame := iframe;

  if assigned(movie_texture) then begin
    media.GetSampleGrabber(sg);

    if succeeded(media.GetBitmapBits(rTime, @sz, p, 720, 480)) then begin
      inc(p, sizeof(BITMAPINFOHEADER));
      movie_texture.Width := 720;
      movie_texture.Width := 480;
      movie_texture.New;
      pb := plocked.pBits;
      if assigned(p) then begin
        for u := 511 downto 0 do begin
          for t := 0 to (511) do begin
            pb[0 + (t * 4) + (u * plocked.Pitch)] := p[0];
            pb[1 + (t * 4) + (u * plocked.Pitch)] := p[1];
            pb[2 + (t * 4) + (u * plocked.Pitch)] := p[2];
            inc(p, 3);

          end;
        end;
      end;
    end;
  end;
end;

{ Tcmd_RenderHuestogram }

procedure Tcmd_RenderHuestogram.DoExecute;
begin
  inherited;

  if Remote then
    DoExecuteRemote
  else
    DoExecuteLocal;

end;

procedure Tcmd_RenderHuestogram.DoExecuteLocal;
VAR
  vp: TVideoParser;
  Output: TMemoryFileStream;
  rTime: real;
  fb: TFastBitmap;
  h: Thuestogram;
  tmStart: cardinal;
  tmnow: cardinal;
  iFrame: integer;
begin
  vp := TVideoParser.create;
  h := Thuestogram.create;
  try
    vp.InitMedia(FileName);
    Output := TMemoryFileStream.create(changefileext(stringreplace(FileName, '.safe.', '.', [rfIgnoreCase]), '.hue'),
      fmCreate);
    Output.seek(0, 0);
    try
      rTime := 0;
      tmStart := GetTickCount;
      iFrame := 0;
      while rTime <= LengthinSeconds do begin
        tmnow := GetTickCount;
        if (iFrame mod 5) = 0 then begin
          vp.LoadVideoFrameToFastBitmap(rTime, 720, 480, fb);
          try
            h.FromFastBitMap(fb);
            h.SaveToStream(Output);
            if GEtTimeSince(tmnow, tmStart) > 0 then begin
              Status := h.Debug + ' ...' + floatprecision(rTime, 1)
                + '/' + floatprecision(LengthinSeconds, 1)
                + ' Rate:' + floatprecision
                (rTime / (GEtTimeSince(tmnow, tmStart) / 1000), 3);
            end;
            Step := round(rTime);
            StepCount := trunc(LengthinSeconds + 1);
          finally
            fb.Free;
          end;
        end else begin
          h.SaveToStream(output);
        end;
        iFrame := iFrame + 1;
        rTime := iFrame / 30;
      end;

    finally
      Output.Free;
    end;
  finally
    vp.Free;
    h.Free;
  end;

end;

procedure Tcmd_RenderHuestogram.DoExecuteLocalMulti;
VAR

  vp: TVideoParser;
  Output: TMemoryFileStream;
  rTime: real;
  fb: TFastBitmap;
  tmStart: cardinal;
  tmnow: cardinal;
  t: integer;
  vpMin: TVideoParser;
  c: Tcmd_RenderHuestogramFrame;
  cl: TList<Tcmd_RenderHuestogramFrame>;
begin
  CPuExpense := 0;
  // create video parsers
  vps := TList<TVideoParser>.create;
  cl := TList<Tcmd_RenderHuestogramFrame>.create;

  for t := 0 to GetnumberofProcessors do begin
    vp := TVideoParser.create;
    vp.InitMedia(FileName);
    vps.Add(vp);
  end;
  try
    // setup output stream
    Output := TMemoryFileStream.create(changefileext(FileName, '.hue'),
      fmCreate);
    Output.seek(0, 0);
    try
      rTime := 0;

      //create list of commands
      while rTime <= LengthinSeconds do begin
        c := Tcmd_RenderHuestogramFrame.create;
        c.MasterCommand := self;
        c.FrameTime := rTime;
        rTime := rtime + HUESTOGRAM_FRAMETIME;
        cl.add(c);
        C.sTART;
      end;

      rTime := 0;
      tmStart := GetTickCount; // for rate calculation
      while cl.count > 0 do begin
        tmnow := GetTickCount; // for rate calculation

        c := cl[0];
        rTime := c.FrameTime;

        try
          c.WaitFor;
          c.Output.SaveToStream(output);


          if GEtTimeSince(tmnow, tmStart) > 0 then begin
            Status := c.output.Debug + ' ...' + floatprecision(rTime, 1)
              + '/' + floatprecision(LengthinSeconds, 1)
              + ' Rate:' + floatprecision
              (rTime / (GEtTimeSince(tmnow, tmStart) / 1000), 3);
          end;
        finally
          c.Free;
          cl.delete(0);
        end;

        Step := round(rTime);
        StepCount := trunc(LengthinSeconds + 1);
      end;

      // wait for final frames

    finally
      // cleanup output stream
      Output.Free;
    end;
  finally
    // cleanup
    while cl.Count > 0 do begin
      c := cl[0];
      c.Free;
      cl.Delete(0);
    end;
    while vps.Count > 0 do begin
      vp := vps[0];
      vp.Free;
      vps.Delete(0);
    end;
    cl.free;
    vps.Free;
  end;

end;

procedure Tcmd_RenderHuestogram.DoExecuteRemote;
var
  fsc: TFileServiceClientEx;
  f2: string;
begin
  fsc := TFileServiceClientEx.create(DEFAULT_HOST, DEFAULT_PORT);
  try
    fsc.PutFileEx(FileName, extractfilename(FileName));
    fsc.BuildHueFile(extractfilename(FileName), LengthinSeconds);
    f2 := changefileext(FileName, '.hue');
    fsc.GetFileEx(extractfilename(f2), f2);
    fsc.DeleteFile(extractfilename(f2));
    fsc.DeleteFile(extractfilename(FileName));

  finally
    fsc.Free;
  end;
end;

{ Tcmd_RenderHuestogramFrame }

destructor Tcmd_RenderHuestogramFrame.Destroy;
begin
  FOutput.Free;
  FOutput := nil;
  MasterCommand.NoNeedVp(vp);
  vp := nil;
  inherited;
end;

procedure Tcmd_RenderHuestogramFrame.DoExecute;
var
  fb: TFastBitmap;
begin
  inherited;

  fb := nil;
  vp := nil;
  while vp = nil do begin
    vp := MasterCommand.NeedVp;
    if vp = nil then
      sleepex(10, true);
  end;

  try
    fb := nil;
    while fb = nil do begin
      try
        vp.LoadVideoFrameToFastBitmap(FrameTime, 72, 48, fb);
      except
        on e:exception do begin
          status := e.message;
      end;
      end;
    end;
  finally
    MasterCommand.noNeedVp(vp);
  end;

  vp := nil;
  try
    FOutput := Thuestogram.create;
    Output.FromFastBitMap(fb);

  finally
    fb.Free;
  end;
end;

procedure Tcmd_RenderHuestogramFrame.InitExpense;
begin
  inherited;
  CPUExpense := 1;
end;

function Tcmd_RenderHuestogram.NeedVp: TVideoParser;
var
  t: integer;
begin
  Lock;
  try
    result := nil;
    for t := 0 to vps.Count - 1 do begin
      if not vps[t].Reserved then begin
        vps[t].Reserved := true;
        result := vps[t];
        exit;
      end;
    end;
  finally
    Unlock;
  end;

end;

procedure Tcmd_RenderHuestogram.NoNeedVp(vp: TVideoParser);
begin
  if not assigned(vp) then
    exit;

  Lock;
  try
    vp.Reserved := false;
  finally
    Unlock;
  end;
end;

initialization


end.
