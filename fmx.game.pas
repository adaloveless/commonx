unit fmx.game;

interface
{xDEFINE NO_BACK_BUFFERS}

uses
  typex, systemx, PXL.Canvas, PXL.Images, PXL.Fonts, pxl.timing, pxl.types, pxl.typedef, tickcount,
  PXL.Providers, PXL.FMBridge,  gameobject, pxl.devices, sysutils, types, classes, pxl.bitmaps;

type

  TFMXGame = class(TGame)
  private
    FFilterFInalCanvas: boolean;
  public
    FMBridge: TFMBridge;
    constructor Create;override;
    procedure CReateFromProviders;override;
    procedure LoadFonts;override;
    procedure LoadTextures;override;
    destructor Destroy;override;

    procedure EngineProcess(const Sender: TObject);override;

    procedure DoRenderBackGRound;override;
    procedure DoRenderForeGround;override;
    procedure PResentBackBuffer;override;
    procedure CreateBackBuffer;override;
    property FilterFinalCanvas: boolean read FFilterFInalCanvas write FFilterFinalCanvas;

  end;


implementation

{ TFMXGame }





constructor TFMXGame.Create;
begin
  inherited;

end;

procedure TFMXGame.CreateBackBuffer;
var
  ebm: TBitmap;
begin
  inherited;

  ebm := TBitmap.Create(EngineDEvice);
  ebm.Storage := TBitmapStorage.Lockable;
//  ebm.SetSize(256,256);
  ebm.SetSize(360*2,240*2);


  EngineRenderTarget := ebm;



  //ebm is already initialized
{
  if not EngineRenderTarget.Canvas.Initialize then
  begin
    raise EGameInitFail.create('could not create EngineRenderTarget');
  end;
}

  {$IFDEF LOFI}
    ebm.Canvas.Attributes := [];
  {$ELSE}
  {$ENDIF}

  SupportedTargetCanvas := EngineRenderTarget.Canvas;

  ebm := TBitmap.create(EngineDevice);
  ebm.Storage := TBitmapStorage.Lockable;
  ebm.SetSize(360*2,240*2);
  ReflectionTarget := ebm;

end;

procedure TFMXGame.CreateFromProviders;
var
  ebm: PXL.bitmaps.TBitmap;
begin
  inherited;
  FMBridge := TFMBridge.Create;

  DeviceProvider := FMBridge.CreateProvider;
  ScreenScale := FMBridge.ScreenScale;

  EngineDevice := DeviceProvider.CreateDevice;
  if (EngineDevice is TCustomStateDevice) and (not TCustomStateDevice(EngineDevice).Initialize) then
  begin
    raise EGameInitFail.create('could not create EngineDevice');
  end;


  Finalcanvas := DeviceProvider.CreateCanvas(EngineDevice);
{$IFDEF LOFI}
  Finalcanvas.Attributes := [];
{$ENDIF}
  if not Finalcanvas.Initialize then
  begin
    raise EGameInitFail.create('could not create EngineCanvas');
  end;
  {$IFDEF NO_BACK_BUFFERS}
  SupportedTargetCanvas := FinalCanvas;
  {$ELSE}
  CreateBackBuffer;
  {$ENDIF}





end;

destructor TFMXGame.Destroy;
begin
  EngineFonts.Free;
  EngineImages.Free;
  EngineRenderTarget.Free;
  FinalCanvas.Free;
  EngineDevice.Free;
  FMBridge.Free;

  inherited;
end;

procedure TFMXGame.EngineProcess(const Sender: TObject);
begin
  inc(engineticks);
end;



procedure TFMXGame.LoadFonts;
begin
  inherited;

end;


procedure TFMXGame.LoadTextures;
begin
  inherited;

end;

procedure TFMXGame.PResentBackBuffer;
var
  x,xx,y,yy: vectorfloat;
  m, mm: TMatrix4;
begin
  inherited;

  if FinalCanvas <> TargetCAnvas then
  if FinalCanvas.BeginScene then
  try
    FinalCanvas.flush;
    FinalCanvas.DefaultLights;

    xx := RenderSize.x;
    yy := rendersize.y;
    x := windowsize.x;
    y := windowsize.y;
    FinalCanvas.ViewMatrix := IdentityMtx4;

    FinalCanvas.UseImage(EngineRenderTarget);
    FinalCanvas.UseReflectionPx2(nil, floatrect4(0,0,x,0,x,y,0,y), 0);

    m := IdentityMtx4;
    m.data[0][0] := 1/(x/2);
    m.data[1][1] := 1/(y/2);
    m.data[3][0] := -1;
    m.data[3][1] := -1;
    //m := mm * m;
    FinalCanvas.ProjMatrix := m;//TransPoseMtx4(m);
    FinalCanvas.TexQuad(floatrect4(0,0,x,0,x,y,0,y), IntColor4($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF));
    //FinalCanvas.TexQuad(floatrect4(-x,-y,x,-y,x,y,-x,y), IntColor4($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF));
    FinalCanvas.flush;

//    m := IdentityMtx4;
//    m.data[0][0] := 0.25;
//    m.data[1][1] := 0.25;
//    FinalCanvas.ProjMatrix := m;//TransPoseMtx4(m);
//    FinalCanvas.TexQuad(floatrect4(-1,-1,1,-1,1,1,-1,1), IntColor4($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF));

  finally
    FinalCanvas.EndScene;
  end;


end;

procedure TFMXGame.DoRenderBackground;
var
  J, I: Integer;
  Omega, Kappa: VectorFloat;
begin
  inherited;
   //Draw gray background.
//   for J := 0 to WindowSize.Y div 40 do
//     for I := 0 to WindowSize.X div 40 do
//        FinalCanvas.FillQuad(
//          FloatRect4(I * 40, J * 40, 40, 40),
//          IntColor4($FF585858, $FF505050, $FF484848, $FF404040));
//
//  for I := 0 to DisplaySize.X div 40 do
//    EngineCanvas.Line(
//      Point2(I * 40.0, 0.0),
//      Point2(I * 40.0, DisplaySize.Y),
//      $FF555555);
//
//  for J := 0 to DisplaySize.Y div 40 do
//    EngineCanvas.Line(
//      Point2(0.0, J * 40.0),
//      Point2(DisplaySize.X, J * 40.0),
//      $FF555555);

  // Draw an animated hole.
//  EngineCanvas.QuadHole(
//    Point2(0.0, 0.0),
//    DisplaySize,
//    Point2(
//      DisplaySize.X * 0.5 + Cos(EngineTicks * 0.0073) * DisplaySize.X * 0.25,
//      DisplaySize.Y * 0.5 + Sin(EngineTicks * 0.00312) * DisplaySize.Y * 0.25),
//    Point2(80.0, 100.0),
//    $20FFFFFF, $80955BFF, 16);



  // Draw an animated Arc.
//  Omega := EngineTicks * 0.0274;
//  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.01854) * 0.5 * Pi;
//
//  EngineCanvas.FillArc(
//    Point2(DisplaySize.X * 0.1, DisplaySize.Y * 0.9),
//    Point2(75.0, 50.0),
//    Omega, Omega + Kappa, 32,
//    IntColor4($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

  // Draw an animated Ribbon.
//  Omega := EngineTicks * 0.02231;
//  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.024751) * 0.5 * Pi;
//
//  EngineCanvas.FillRibbon(
//    Point2(DisplaySize.X * 0.9, DisplaySize.Y * 0.85),
//    Point2(25.0, 20.0),
//    Point2(70.0, 80.0),
//    Omega, Omega + Kappa, 32,
//    IntColor4($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

end;

procedure TFMXGame.DoRenderForeGround;
var
  w,h: single;
begin
  inherited;

  TargetCanvas.Flush;

  targetCanvas.RemoveShaderBinding(1);
  targetCanvas.RemoveShaderBinding(2);

  targetcanvas.DefaultLIghts;
  w := RenderSize.X/2;
  h := RenderSize.Y/2;
  targetcanvas.ViewMatrix := IdentityMtx4 * TranslateMtx4(-w,-h);
  targetcanvas.projmatrix := OrthogonalBDSMtx4(-w, w, h,-h, -1, 1);
  EngineFonts[FontTahoma].Canvas := TargetCanvas;
  EngineFonts[FontTahoma].DrawText(
    Point2(4.0, 4.0),
    'FPS: ' + IntToStr(EngineTimer.FrameRate),
    IntColor2($FFFFE887, $FFFF0000));

  EngineFonts[FontTahoma].DrawText(
    Point2(4.0, 24.0),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    IntColor2($FFE8FFAA, $FF12C312));

{
  EngineFonts[FontTahoma].DrawText(
    Point2(4.0, 44.0),
    'CAM: ' + Camera.TransForm.worldposition.ToSTring,
    IntColor2($FFFFE887, $FFFF0000));
}
  TargetCanvas.Flush;
  exit;



  //EngineCanvas.SetEffect(
  // Draw the image of famous Lenna.
//  EngineCanvas.Attributes := [TCanvasAttribute.Antialias];
//  EngineCanvas.UseImagePx(EngineImages[ImageLenna], FloatRect4(0, 0, 32, 32));
//
//  EngineCanvas.TexQuad(FloatRect4RC(TPoint2(DisplaySize) * 0.5, Point2(150.0, 150.0),
//    EngineTicks * 0.1),
//    IntColorAlpha(255));
end;

end.
