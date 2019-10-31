unit dx9_tools;

interface
uses
  direct3d9_jedi, sysutils, classes, controls, windows, graphics, dialogs, debug, math;
const
  ABSTRACT_RHW_FVF = D3DFVF_XYZRHW or  D3DFVF_DIFFUSE;
  ABSTRACT_FVF = D3DFVF_XYZ or   D3DFVF_DIFFUSE or D3DFVF_NORMAL or D3DFVF_TEX1;

type
  TD3DVAlue = single;
  Td3dVertex = packed record
    x, y, z:single;
    n: Td3dVector;
    Color: TD3DColor;
    tu,tv: Single;
  end;

  TRHWVertex = packed record
    x, y, z:single;
    rhw: single;
    Color: TD3DColor;
    tu,tv: Single;
  end;


  TDirect3DTexture = class;
  TDirect3dTexture9 = TDirect3DTexture;//forward;

  TNativeLIght = TD3DLight9;

  TNativeVertex = Td3dVertex;
  TNativeRHWVertex = TRHWVertex;
  TNativeValue = TD3DValue;
  TNativeMatrix = direct3d9_jedi.TD3DMatrix;
  PNativeMatrix = ^TNativeMatrix;
  TNativeVEctor = Td3dVector;
  TNativeMaterial = TD3DMaterial9;
  TNativeTExture = TDirect3DTexture9;
  PNativeMaterial = ^TNativeMAterial;

  INativeSurface = IDirect3dSurface9;
  INAtiveTExture = IDirect3dTExture9;

  PNativeVertex = ^TNativeVertex;
  INativeDevice = IDirect3DDevice9;
  TNativeDevice = IDirect3dDevice9;
  TNativeViewPort = TD3DViewport9;
  PNativeVector = ^TNativeVector;

  TDX9Window = class;//forward


  TDirect3DSurface = class
  private
    FSurface: INativeSurface;
  public
    property Surface: INativeSurface read FSurface;
  end;

  TDirect3DTexture = class
  private
    FSurface: TDirect3DSurface;
    FTexture: INativeTexture;
    function GetTexture: IDirect3DTexture9;
  public
    constructor Create(win: Tdx9Window; g: TGraphic; bSomething: boolean);
    property Native: INativeTexture read FTexture;
    property Surface: TDirect3DSurface read FSurface;
  end;


  TDX9Window = class(TGraphicControl)
  private
    FHandle: DXHWND;
    FSurfaceWidth: integer;
    FSurfaceHEight: integer;

    function GetPresentParameters(hdc: THandle): TD3DPresentParameters;
    function GetSurfaceHeight: integer;
    function GetSurfaceWidth: integer;
    procedure SetDisplayHandle(const Value: DXHWND);
  protected
  public
    pEnum: IDirect3D9;
    dev: IDirect3DDevice9;
    d3dpp: TD3dPresentParameters;

    constructor Create(AOwner: TComponent);override;
    property DisplayHandle: DXHWND read FHandle write SetDisplayHandle;
    procedure Initialize;
    procedure ColorBars;
    property SurfaceWidth: integer read GetSurfaceWidth write FSurfaceWidth;
    property SurfaceHeight: integer read GetSurfaceHeight write FSurfaceHEight;

    procedure REset;


 
  end;

procedure AssignMyVertex(var Vertex: TnativeRHWVertex; x, y, z, rhw: Single; Color: TD3DColor);

function VectorAdd(v1, v2: TD3DVector) : TD3DVector;
function VectorSub(v1, v2: TD3DVector) : TD3DVector;
function VectorMulS(v: TD3DVector; s: TD3DValue) : TD3DVector;
function VectorDivS(v: TD3DVector; s: TD3DValue) : TD3DVector;
function VectorMul(v1, v2: TD3DVector) : TD3DVector;
function VectorDiv(v1, v2: TD3DVector) : TD3DVector;
function VectorSmaller(v1, v2: TD3DVector) : boolean;
function VectorSmallerEquel(v1, v2: TD3DVector) : boolean;
function VectorEquel(v1, v2: TD3DVector) : boolean;
function VectorSquareMagnitude(v: TD3DVector) : TD3DValue;
function VectorMagnitude(v: TD3DVector) : TD3DValue;
function VectorNormalize(v: TD3DVector) : TD3DVector;
function VectorMin(v: TD3DVector) : TD3DValue;
function VectorDotProduct(v1, v2: TD3DVector): TD3DValue;
function VectorCrossProduct(const v1, v2: TD3DVector): TD3DVector;

procedure FloatCheck();

implementation

{ TDX9Window }

procedure AssignMyVertex(var Vertex: TnativeRHWVertex; x, y, z, rhw: Single; Color: TD3DColor);
 begin
   Vertex.x:= x; Vertex.y:= y; Vertex.z:= z; Vertex.rhw:= rhw;
   Vertex.Color:= Color;
 end;
procedure TDX9Window.ColorBars;
var
  vTriangle: array [0..2] of TRHWVertex;
begin
      AssignMyVertex(vTriangle[0], 160, 060, 0.5, 2, D3DCOLOR_XRGB($ff, $00, $00));
      AssignMyVertex(vTriangle[1], 240, 180, 0.5, 2, D3DCOLOR_XRGB($00, $ff, $00));
      AssignMyVertex(vTriangle[2], 080, 180, 0.5, 2, D3DCOLOR_XRGB($00, $00, $ff));
//  dev.SetRenderState(d3drs_FILLMODE, 2);
  dev.BeginScene;
//  dev.Clear(0, nil, D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER,
//    D3DCOLOR_XRGB(0, 0, 0), 1, 0);
  dev.SetFVF(ABSTRACT_RHW_FVF);
//  dev.SetRenderState(D3DRS_CLIPPING, Integer(FALSE));
//      g_d3ddev.SetVertexShader(D3DFVF_XYZRHW or D3DFVF_DIFFUSE);
  dev.DrawPrimitiveUP(D3DPT_TRIANGLELIST, 1, vTriangle, 20);
  dev.EndScene;

  dev.Present(nil, nil, 0, nil);

end;

constructor TDX9Window.Create(AOwner: TComponent);
begin
  inherited;


end;

function TDX9Window.GetPresentParameters(hdc: THandle): TD3DPresentParameters;
var
  ibits: integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Windowed:= True;
  Result.hDeviceWindow:= self.DisplayHandle;
  Result.BackBufferWidth:= surfacewidth;
  Result.BackBufferHeight:= surfaceheight;
  ibits := Windows.GetDeviceCaps(hdc, BITSPIXEL);
  OutputDebugString(pchar('BPP='+inttostr(iBits)));
  if iBits = 16
    then Result.BackBufferFormat:= D3DFMT_R5G6B5
//    else Result.BackBufferFormat:= D3DFMT_A8R8G8B8;
    else Result.BackBufferFormat:= D3DFMT_X8R8G8B8;
  Result.BackBufferCount:= 1;
  Result.EnableAutoDepthStencil:= True;
  Result.AutoDepthStencilFormat:= D3DFMT_D16;
//  result.MultiSampleType := d3dmultisample_4_samples;
  result.MultiSampleType := d3dmultisample_2_samples;
//  result.MultiSampleType := D3DMULTISAMPLE_NONMASKABLE;
  Result.SwapEffect:= D3DSWAPEFFECT_DISCARD;
end;

function TDX9Window.GetSurfaceHeight: integer;
begin
  Result := FSurfaceHEight;
  if result = 0 then
    result := parent.height;
end;

function TDX9Window.GetSurfaceWidth: integer;
begin
  Result := FSurfaceWidth;

  if result = 0 then
    result := parent.width;
end;

procedure TDX9Window.Initialize;
var
  hr: HRESULT;
  hdc: THandle;
  halsal: _d3ddevtype;
  vp: integer;
begin
  pEnum:= Direct3DCreate9(D3D_SDK_VERSION);
  if (pEnum = nil) then Halt(1);



  //get handle to your window
  hdc:= GetDC(displayhandle);
  if hdc = 0 then Halt(1);


  halsal := D3DDEVTYPE_HAL;
  vp := D3DCREATE_HARDWARE_VERTEXPROCESSING+D3DCREATE_FPU_PRESERVE;
//  halsal := D3DDEVTYPE_REF;
//  vp := D3DCREATE_SOFTWARE_VERTEXPROCESSING;


  d3dpp:= GetPresentParameters(hdc);
  Debug.log(self,'PPSize='+inttostr(sizeof(_D3DPRESENT_PARAMETERS_)));
  Debug.log(self,'BackBufferWidth='+inttostr((d3dpp.BackBufferWidth)));
  Debug.log(self,'BackBufferFormat='+inttostr((d3dpp.BackBufferFormat)));
  Debug.log(self,'BackBufferCount='+inttostr((d3dpp.BackBufferCount)));
  Debug.log(self,'MultiSampleType='+inttostr(ord(d3dpp.MultiSampleType)));
  Debug.log(self,'MultiSampleQuality='+inttostr((d3dpp.MultiSampleQuality)));
  Debug.log(self,'SwapEffect='+inttostr(ord(d3dpp.SwapEffect)));
  Debug.log(self,'hDeviceWindow='+inttostr((d3dpp.hDeviceWindow)));
  Debug.log(self,'Windowed='+inttostr(ord(d3dpp.Windowed)));
  Debug.log(self,'EnableAutoDepthStencil='+inttostr(ord(d3dpp.EnableAutoDepthStencil)));
  Debug.log(self,'AutoDepthStencilFormat='+inttostr(ord(d3dpp.AutoDepthStencilFormat)));
  Debug.log(self,'Flags='+inttostr((d3dpp.Flags)));
  Debug.log(self,'FullScreen_RefreshRateInHz='+inttostr((d3dpp.FullScreen_RefreshRateInHz)));
  Debug.log(self,'PresentationInterval='+inttostr((d3dpp.PresentationInterval)));

  Debug.log(self,'D3DADAPTER_DEFAULT='+inttostr((D3DADAPTER_DEFAULT)));
  Debug.log(self,'halsal='+inttostr(ord(halsal)));
  Debug.log(self,'displayhandle='+inttostr((displayhandle)));
  Debug.log(self,'vp='+inttostr((vp)));
//  Debug.log(self,'dev='+inttostr(ord(dev)));

  Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
  hr:= pEnum.CreateDevice(D3DADAPTER_DEFAULT, halsal, displayhandle,
    vp, @d3dpp, {out} dev);
  Math.SetExceptionMask([exOverflow, exUnderflow, exPrecision]);

  if (FAILED(hr)) then begin
    d3dpp.MultiSampleType := D3DMULTISAMPLE_NONE;

    hr:= pEnum.CreateDevice(D3DADAPTER_DEFAULT, halsal, displayhandle,
      vp, @d3dpp, dev);
    if (Failed(hr)) then
    begin
      if d3dpp.BackBufferFormat = D3DFMT_X8R8G8B8 then begin
        d3dpp.BackBufferFormat := D3DFMT_A8R8G8B8
      end;
      hr:= pEnum.CreateDevice(D3DADAPTER_DEFAULT, halsal, displayhandle,
        vp, @d3dpp, dev);

    end;
  end;


  ReleaseDC(displayhandle, hdc);

  if (FAILED(hr)) then
  begin
//    Showmessage('Failed to create render surface. Result = '+inttostr(hr));
//    raise Exception.create(
//    Halt(1);
  end;

  //ShowWindow(g_hwnd, SW_SHOWDEFAULT);
  //UpdateWindow(g_hwnd);




end;






procedure TDX9Window.REset;
var
  d3dpp: TD3dPresentParameters;
  hdc: THandle;
begin
  hdc:= GetDC(displayhandle);
  try

    d3dpp := GetPresentParameters(hdc);
    if dev <> nil then
      dev.Reset(d3dpp);
  finally
    ReleaseDc(displayhandle, hdc);
  end;

end;

procedure TDX9Window.SetDisplayHandle(const Value: DXHWND);
begin
  FHandle := Value;
  debug.Log('display handle set 0x'+inttohex(value,8));
end;

{ TDirect3DTexture }

constructor TDirect3DTexture.Create(win: Tdx9Window; g: TGraphic;
  bSomething: boolean);
begin
//  inherited;
//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TDirect3DTexture.GetTexture: IDirect3DTexture9;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function VectorAdd(v1, v2: TD3DVector) : TD3DVector;
begin
  result.x := v1.x+v2.x;
  result.y := v1.y+v2.y;
  result.z := v1.z+v2.z;
end;


function VectorSub(v1, v2: TD3DVector) : TD3DVector;
begin
  result.x := v1.x-v2.x;
  result.y := v1.y-v2.y;
  result.z := v1.z-v2.z;
end;

function VectorMulS(v: TD3DVector; s: TD3DValue) : TD3DVector;
begin
  result.x := v.x*s;
  result.y := v.y*s;
  result.z := v.z*s;
end;

function VectorDivS(v: TD3DVector; s: TD3DValue) : TD3DVector;
begin
  result.x := v.x/s;
  result.y := v.y/s;
  result.z := v.z/s;
end;

function VectorMul(v1, v2: TD3DVector) : TD3DVector;
begin
  result.x := v1.x*v2.x;
  result.y := v1.y*v2.y;
  result.z := v1.z*v2.z;
end;

function VectorDiv(v1, v2: TD3DVector) : TD3DVector;
begin
  result.x := v1.x/v2.x;
  result.y := v1.y/v2.y;
  result.z := v1.z/v2.z;
end;

function VectorSmaller(v1, v2: TD3DVector) : boolean;
begin
  result := (v1.x < v2.x) and (v1.y < v2.y) and (v1.z < v2.z);
end;

function VectorSmallerEquel(v1, v2: TD3DVector) : boolean;
begin
  result := (v1.x <= v2.x) and (v1.y <= v2.y) and (v1.z <= v2.z);
end;

function VectorEquel(v1, v2: TD3DVector) : boolean;
begin
  result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
end;

function VectorSquareMagnitude(v: TD3DVector) : TD3DValue;
begin
  result := (v.x*v.x) + (v.y*v.y) + (v.z*v.z);
end;

function VectorMagnitude(v: TD3DVector) : TD3DValue;
begin
  result := sqrt( (v.x*v.x) + (v.y*v.y) + (v.z*v.z) );
end;

function VectorNormalize(v: TD3DVector) : TD3DVector;
begin
  result := VectorDivS(v,VectorMagnitude(v));
end;

function VectorMin(v: TD3DVector) : TD3DValue;
var
  ret : TD3DValue;
begin
  ret := v.x;
  if (v.y < ret) then ret := v.y;
  if (v.z < ret) then ret := v.z;
  result := ret;
end;

function VectorDotProduct(v1, v2: TD3DVector): TD3DValue;
begin
  Result := (v1.x*v2.x) + (v1.y * v2.y) + (v1.z*v2.z);
end;

function VectorCrossProduct(const v1, v2: TD3DVector): TD3DVector;
begin
  Result.x := (v1.y*v2.z) - (v1.z*v2.y);
  Result.y := (v1.z*v2.x) - (v1.x*v2.z);
  Result.z := (v1.x*v2.y) - (v1.y*v2.x);
end;

procedure FloatCheck();
begin
  showmessage(floattostr(1.6));
end;

end.
