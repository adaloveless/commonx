unit dmx_effects;

//todo 1: update DMX UT to show the actual channel values next to the slider

interface

uses
  colorblending, math, sysutils, typex,  dmx_objects, graphics, geometry, systemx, soundtools, numbers, stringx, fastbitmap, types;


type
  TFXSpotStaticJoyPosition = class(TDMXEffect)
  public
    procedure Init;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefinePArameters;override;
  end;

  TFXSpotStaticJoyPositions = class(TDMXEffect)
  protected
    compiled: string;
  public
    procedure Init;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefinePArameters;override;

  end;


  TFXSpotByExternalControl = class(TDMXEffect)
  public
    procedure Init;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefinePArameters;override;
  end;

  TFXSpotSwirl = class(TDMXEffect)
  private
    FMagnitude, FMagnitudeX, FMagnitudeY: nativefloat;
  public
    procedure Init;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;

    procedure DefinePArameters;override;
  end;

  TFXCaliente = class(TDMXEffect)
  public
    procedure Init;override;
    procedure DefinePArameters;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;

  end;

  TFX4Banger = class(TDMXEffect)
  public
    procedure Init;override;
    procedure DefinePArameters;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;

  end;

  TFXSineWaveIntensityAdd = class(TDMXEffect)
  private
    FMagnitude, FMagnitudeX, FMagnitudeY: nativefloat;
    FLastTime, FLAstFreq, FPhaseCorrection, FLastPhase: nativefloat;

  public
    procedure Init;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;

    procedure DefinePArameters;override;
  end;


  TFXSpotPulseWave = class(TDMXEffect)
  private
    FMagnitude, FMagnitudeX, FMagnitudeY: nativefloat;
    FEffectTime: nativefloat;
    FXdir: nativefloat;
    FYdir: nativefloat;
    FStartTime: nativefloat;
  public
    procedure Init;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;

    procedure SetParamLL(sName: string; sValue: string); override;
    property Magnitude: nativefloat read FMagnitude write FMagnitude;
    property MagnitudeX: nativefloat read FMagnitudeX write FMagnitudeX;
    property MagnitudeY: nativefloat read FMagnitudeY write FMagnitudeY;
    property XDir: nativefloat read FXdir write FXDir;
    property YDir: nativefloat read FYdir write FYDir;
    property EffectTime: nativefloat read FEffectTime write FEffectTime;
  end;

  TFXVUBar = class(TDMXEffect)
  public
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefineParameters;override;
  end;

  TFXVUByColor = class(TDMXEffect)
  public
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefineParameters;override;
  end;

  TFXSimpleGraphic = class(TDMXEffect)
  private
    loadedFrom: string;
    fbm: TFastBitmap;
    procedure CleanupBitmap;
    procedure checkBitmapInit;
    procedure CheckLoaded;
  public
    procedure Detach;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefineParameters;override;
  end;


  TFXMoveThenBlankGroup = class(TDMXEffect)
  private
    FMagnitudeX: nativefloat;
    FMagnitudeY: nativefloat;
    FTimeOff: nativefloat;
    FTimeOn: nativefloat;

  public
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefineParameters;override;
  end;


  TFun = record
    idx: ni;
    points: array[0..9] of TPointF;
    v: TPointF;
    procedure Init;
  end;

  Tglobalfire = record
    lastframe: ni;
    funn: array[0..199] of TFun;
    procedure MoveFun;
    procedure Init;
  end;

  TFXPixelFire = class(TDMXEffect)
  private
    loadedFrom: string;
  public
    procedure Detach;override;
    procedure UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
      l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
      rAmplitude: single); override;
    procedure DefineParameters;override;
  end;

var
  globalfire: TGlobalFire;

implementation

{ TFXSpotSwirl }

procedure TFXSpotSwirl.DefinePArameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'MagnitudeX';
  def.Min := -1.0;
  def.Max := 1.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 1.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'MagnitudeY';
  def.Min := -1.0;
  def.Max := 1.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 1.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'revolution_time';
  def.Min := 0.5;
  def.Max := 20.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 4.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'magnitude';
  def.Min := -1.0;
  def.Max := 1.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 0.1;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'stagger';
  def.Min := 1.0;
  def.Max := -1.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 0.3;
  FParamDefinitions.Add(def);





end;

procedure TFXSpotSwirl.Init;
begin
  inherited;
end;


procedure TFXSpotSwirl.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  tp: TDMXTiltPan;
  x,y: nativefloat;
  rRevTime: nativefloat;
  Stagger, MAgnitude, MagnitudeX, MagnitudeY, REvolutionTime: nativefloat;

begin
  inherited;

  if not (l is TDMXTiltPan) then begin
    exit;
  end;

  tp := l as TDMXTiltPan;


  RevolutionTime := Params['revolution_time'].rValue;
  Stagger := Params['stagger'].rValue;
  MagnitudeX := Params['magnitudex'].rValue;
  MagnitudeY := Params['magnitudey'].rValue;
  Magnitude := Params['magnitude'].rValue;

  rRevTime := (rTime - (floor(rTime / RevolutionTime)*RevolutionTime))/RevolutionTime;
  rRevTime := rREvTime + (iGroupID * Stagger);


  x := sin(rRevTime*6.28);
  y := cos(rRevTime*6.28);
  x := x * MagnitudeX * Magnitude*tp.PanTiltRatio;
  y := y * Magnitudey * Magnitude;

  tp.TiltOffset := x;
  tp.PanOffset := y;






end;

{ TFXSpotPulseWave }

procedure TFXSpotPulseWave.Init;
begin
  inherited;
  EffectTime := 4.0;
  Magnitude := 0.1;
  MagnitudeX := 1.0;
  MagnitudeY := 1.0;
  XDir := 1;
  YDir := 1;
  FStartTime := -1;
end;

procedure TFXSpotPulseWave.SetParamLL(sName, sValue: string);
begin
  inherited;
  if sNAme = 'xdir' then begin
    FXDir := strtofloat(sValue);
  end else
  if sNAme = 'ydir' then begin
    FYDir := strtofloat(sValue);
  end else
  if sNAme = 'effect_time' then begin
    FeffectTime := strtofloat(sValue);
  end else
  if sNAme = 'magnitude' then begin
    FMagnitude := strtofloat(sValue);
  end else
  if sNAme = 'magnitudeX' then begin
    FMagnitudeX := strtofloat(sValue);
  end else
  if sNAme = 'magnitudeY' then begin
    FMagnitudeY := strtofloat(sValue);
  end;

end;

procedure TFXSpotPulseWave.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  spt: TDMXQspot14;
  rPos: nativefloat;
  rBrightness: nativefloat;
begin
  inherited;
  if FStartTIme < 0 then begin
    FStartTime := rTime;
  end;


  //TODO 5: IF you ever do realtime mode... the rTime parameter is passed as a samplecount..not number of seconds.

  if not (l is TDMXQSpot14) then begin
    exit;
  end;

  spt := l as TDMXQSpot14;


  rPos := Interpolate(rTime - FStartTime, FEffectTime, 0, 1);
  if rPos < 0 then
    rPos := 0;


  if rPos > 1 then
    rPos := 1;

  rBrightness := sin(rPos * 3.14);

  spt.Motion.TiltOffset := Interpolate(rPos, Magnitude * MagnitudeX * (0), Magnitude * MagnitudeX * (0+XDir));
  spt.Motion.PanOffset := Interpolate(rPos, Magnitude * MagnitudeY * (0), Magnitude * MagnitudeX * (0+YDir));
  spt.Intensity := rBrightNess;



end;

{ TFXMoveThenBlankGroup }

procedure TFXMoveThenBlankGroup.DefineParameters;
var
  def: TDMXEffectParameterDefinition;
begin
  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'MagnitudeX';
  def.Min := 0;
  def.Max := 1.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 1.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'MagnitudeY';
  def.Min := 0;
  def.Max := 1.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 1.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'TimeOn';
  def.Min := 0;
  def.Max := 20.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 1.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'TimeOff';
  def.Min := 0;
  def.Max := 20.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 1.0;
  FParamDefinitions.Add(def);


end;


procedure TFXMoveThenBlankGroup.UpdatePhysicsforlight(iGroupID,
  iGroupOf: integer; l: TDMXChannelCluster; iTriggerNumber: integer;
  rTime: NativeFloat; rAmplitude: single);
var
  r, rCycleBase, rCycleTime,rStageTime: nativefloat;
  rI: nativefloat;
  MagnitudeX, MagnitudeY, TimeOn, TimeOff: nativefloat;

begin
  inherited;
  TimeOff := Params['TimeOff'].rValue;
  TimeOn := Params['TimeOn'].rValue;
  MagnitudeX := Params['MagnitudeX'].rValue;
  MagnitudeY := Params['MagnitudeY'].rValue;


  rTime := rTime + (((iGRoupOf-iGroupID)/iGroupOF) * (TimeOn+TImeOff));
  if (l.IsInterface(IDMXTiltPan)) then begin
    if (TimeOn+TimeOff) = 0 then
      exit;

    r := (rTime / (TimeON+TimeOff));
    rCycleBase := floor(r)* (TimeOn+TimeOff);
    rCycleTime := rTime - rCycleBase;

   // Debug(FLoatToStr(rCycleTime));
   if (rCycleTime<TimeOn) then begin
      rStageTime := rCycleTime / TimeOn;
      with l as IDMXTiltPan do begin
        PanOffSet := MagnitudeX*((rStageTime * 2)-1);
        TiltOffSet := MagnitudeY*((rStageTime * 2)-1);
      end;
    end else begin
      rStageTime := (rCycleTime - TimeOn) / TimeOff;
      with l as IDMXTiltPan do begin
        PanOffSet := MagnitudeX*-1;
        TiltOffSet := MagnitudeY*-1;
      end;
    end;
  end;

  if (l.IsInterface(IDMXIntensity)) then begin
    if (TimeOn+TimeOff) = 0 then exit;

    r := (rTime / (TimeON+TimeOff));
    rCycleBase := floor(r)* (TimeOn+TimeOff);
    rCycleTime := rTime - rCycleBase;

   if (rCycleTime<TimeOn) then begin
      rStageTime := rCycleTime / TimeOn;

      with l as IDMXIntensity do begin
        ri := rStageTime * 2;
        ri := ri -1;
        ri := 1-(abs(ri));
        Intensity := ri;

      end;
    end else begin
      rStageTime := (rCycleTime - TimeOn) / TimeOff;

      with l as IDMXIntensity do begin
        Intensity := 0.0;
      end;
    end;
  end;

end;

{ TFXSineWaveIntensityAdd }

procedure TFXSineWaveIntensityAdd.DefinePArameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Frequency';
  def.Min := 1;
  def.Max := 3000;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 500.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Magnitude';
  def.Min := 0;
  def.Max := 3.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 1.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Intensity';
  def.Min := 0;
  def.Max := 3.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 1.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Lowcut';
  def.Min := 0;
  def.Max := 1.0;
  def.Steps := 65536;
  def.imptyp := eptFloat;
  def.rValue := 0.7;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'STagger';
  def.Min := 0;
  def.Max := 1.0;
  def.Steps := 255;
  def.imptyp := eptFloat;
  def.rValue := 0.5;
  FParamDefinitions.Add(def);



end;

procedure TFXSineWaveIntensityAdd.Init;
begin
  inherited;

end;

procedure TFXSineWaveIntensityAdd.UpdatePhysicsforlight(iGroupID,
  iGroupOf: integer; l: TDMXChannelCluster; iTriggerNumber: integer;
  rTime: NativeFloat; rAmplitude: single);
var
  STagger, Mag, Freq, lowCut, Int: nativefloat;
  rVal: nativefloat;
  rDIV: nativefloat;
  phase: nativefloat;
begin
  inherited;

  if not (l.IsInterface(IDMXIntensity)) then exit;

  Mag := Params['Magnitude'].rValue;
  Freq := Params['Frequency'].rValue;
  lowcut := Params['LowCut'].rValue;
  Int := Params['Intensity'].rValue;
  STagger := Params['Stagger'].rValue;

  //if frequency changed, then we need to correct the phase



  //generate a sin wave
  phase := ((rTime/100)*FReq)/6.23;

  phase := phase-floor(phase);
  if iGroupID = 0 then begin
    if FLastFreq <> Freq then begin
      FLastPhase := ((rTime/100)*FLastFReq)/6.23;//calculate the phase that MIGHT have been
      FLAstPhase := (FLastPhase-floor(FLastPhase))-FPhaseCorrection;
      FPhaseCorrection := phase - FLastPhase;
    end;
  end;


  phase := phase - FPhaseCorrection;
  phase := phase - negfloor(phase);


//  if iGroupID = 0 then begin
//    FLastPhase := phase;
//    FLAstFreq := FReq;
//    FLastTime := rTime;
//  end;
  phase := phase + ((Stagger*(l.X+l.Y)));
  rVal := sin(6.23*(phase));

  rVAl := rVal * Mag;

  //normalize wave betwenn 0 and 1
  rVal := (rVal / 2) + 0.5;


  //scale based on the cutoff
  if lowcut < 1.0 then begin
    rVal := (rVal - lowcut) * (1/(1.0-lowcut));
  end;

  if rVal < 0 then
    rVal := 0;



  //pull back or up based on intensity
  rVal := rVal * Int;


  with l as  IDMXIntensity do begin
    TargetIntensity := TargetIntensity + rVal;
//    if iGroupID = 0 then begin
//      Debug(floattostr(rVal)+':::::'+floattostr(Intensity));
//      sleep(100);
//    end;
  end;

  FLastFreq := Freq;

end;

{ TFXVUBar }

procedure TFXVUBar.DefineParameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Scale';
  def.Min := 1;
  def.Max := 8;
  def.Steps := 8;
  def.imptyp := eptFloat;
  def.rValue := 80.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Intensity';
  def.Min := 0.0;
  def.Max := 1.0;
  def.Steps := 255;
  def.imptyp := eptFloat;
  def.rValue := 0.1;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'MinIntensity';
  def.Min := 0.0;
  def.Max := 1.0;
  def.Steps := 255;
  def.imptyp := eptFloat;
  def.rValue := 0.1;
  FParamDefinitions.Add(def);


end;

procedure TFXVUBar.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  cb: TDMXColorBar;
  pw: TDMXPixelWall;
  t,u, uu: integer;
  r: nativefloat;
  Scale: nativefloat;
  Intensity: nativefloat;
  Minintensity: nativefloat;
begin
  inherited;

  if not ((l is TDMXColorBar) or (l is TDMXPixelWall)) then
    exit;

  Scale := Params['Scale'].rValue;
  Intensity := Params['intensity'].rValue;
  MinIntensity := Params['MinIntensity'].rvalue;


  if l is TDMXColorBar then begin
    cb := l as TDMXColorbar;


    for t:= 0 to 7 do begin
      r := power(rAmplitude,2) * Scale;



      r := r - t;
      if r < 0 then r := 0;
      if r > 1 then r := 1;
      cb.Sections[t].Intensity := (r * Intensity);
    end;
  end else
  if l is TDMXPixelWall then begin
    pw := l as TDMXPixelWall;
    if pw.strobeenable then exit;

    for u := 0 to pw.Dimensions.y-1 do begin
      uu := (pw.Dimensions.y-1) -u;

      for t:= 0 to pw.Dimensions.x-1 do begin
        r := power(rAmplitude,2) * Scale * 4;

        r := r - ((pw.Dimensions.y-1)-u);
        if r < 0 then r := 0;
        if r > 1 then r := 1;
        //r := Interpolate(MinIntensity, Intensity, r);
        pw.pixels[t,uu].Intensity := r*Intensity;
      end;
    end;
  end;



end;

{ TFXSpotByExternalControl }

procedure TFXSpotByExternalControl.DefinePArameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;
  def := TDMXEffectParameterDefinition.create;
  def.Name := 'joyidx';
  def.Steps := 2;
  def.Min := 0;
  def.Max := 1;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);
end;

procedure TFXSpotByExternalControl.Init;
begin
  inherited;

end;

procedure TFXSpotByExternalControl.UpdatePhysicsforlight(iGroupID,
  iGroupOf: integer; l: TDMXChannelCluster; iTriggerNumber: integer;
  rTime: NativeFloat; rAmplitude: single);
var
  al: TUniverseList<TDMXTiltPan>;
  i: ni;
  vec: TNativeVector4;
  idx: ni;
  ld: PDMXPLaneLight;
  joy : TDMXExternalJoyControl;
begin
  inherited;

  al := l.GetAspectList<TDMXTiltPan>;
  try
    for i := 0 to al.count-1 do begin
      idx := round(Params['joyidx'].rValue);
      if idx >= 0 then begin
        joy := Multiverse.Joys[idx];
        ld := Multiverse.Stage.planes[joy.Plane].GetLightData(l);
        if ld <> nil then begin
          vec := ld.GetInterpolatedPoint(joy.Pan, joy.Tilt);
          al[i].PanF := vec.x;
          al[i].TiltF := vec.y;
        end;
      end;
    end;
  finally
    al.free;
  end;
end;

{ TFXSpotStaticJoyPosition }

procedure TFXSpotStaticJoyPosition.DefinePArameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;

  def := TDMXEffectParameterDefinition.create;
  def.Name := 'plane';
  def.Steps := 3;
  def.Min := 0;
  def.Max := 3;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.create;
  def.Name := 'pan';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 1;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.create;
  def.Name := 'tilt';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 1;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);



end;

procedure TFXSpotStaticJoyPosition.Init;
begin
  inherited;



end;


procedure TFXSpotStaticJoyPosition.UpdatePhysicsforlight(iGroupID,
  iGroupOf: integer; l: TDMXChannelCluster; iTriggerNumber: integer;
  rTime: NativeFloat; rAmplitude: single);
var
  sGroup: string;
  sLeft, sRight: string;
  sIP, sX, sY: string;
  fx,fy: nativefloat;
  ip: ni;
  sLIght, sCoords: string;
  iLightChan: nativeint;
  ld: PDMXPLaneLight;
  al: TUniverseList<TDMXTiltPan>;
  t: ni;
  vec: TNativeVector4;
begin
  inherited;

  //@360=0.5,0.5;@368=0.35,0.35


  ip := round(Self.Params['plane'].rValue);

  fx := self.params['pan'].rvalue;
  fy := self.params['tilt'].rvalue;


  ld := Multiverse.Stage.planes[ip].GetLightData(l);
  if ld <> nil then begin
    vec := ld.GetInterpolatedPoint(fx,fy);
    al := l.GetAspectList<TDMXTiltPan>();
    try
      for t := 0 to al.count-1 do begin
        al[t].PanF := vec.x;
        al[t].TiltF := vec.y;
      end;
    FINALLY
      AL.FREE;
    END;
  end;
end;
procedure TFXSpotStaticJoyPositions.DefinePArameters;
var
  def: TDMXEffectParameterDEfinition;
begin
  inherited;


  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Positions';
  def.imptyp := TEffectParameterTYpe.eptString;
  def.entryType := eetString;
  FParamDefinitions.Add(def);


end;

procedure TFXSpotStaticJoyPositions.Init;
begin
  inherited;

end;

procedure TFXSpotStaticJoyPositions.UpdatePhysicsforlight(iGroupID,
  iGroupOf: integer; l: TDMXChannelCluster; iTriggerNumber: integer;
  rTime: NativeFloat; rAmplitude: single);
var
  sGroup: string;
  sLeft, sRight: string;
  sIP, sX, sY: string;
  fx,fy: nativefloat;
  ip: ni;
  sLIght, sCoords: string;
  iLightChan: nativeint;
  ld: PDMXPLaneLight;
  al: TUniverseList<TDMXTiltPan>;
  t: ni;
  vec: TNativeVector4;
begin
  inherited;

  //@360=0.5,0.5;@368=0.35,0.35


  sGroup := Params['Positions'].sValue;
//  sGroup := ParamDefinitions['
  while SplitString(sGroup, '|', sLeft, sGroup) do begin

    if SplitString(sLeft, '>', sLIght, sCoords) then begin
      if SplitString(sCoords, '>', sIP, sX) then begin
        if SplitString(sX, '>', sX, sY) then begin
          try
            iLightChan := strtoint(sLight);
            ip := strtoint(sIP);
            fx := strtofloat(sX);
            fy := strtofloat(sY);

            if l.BaseChannel = iLightchan then begin
              ld := Multiverse.Stage.planes[ip].GetLightData(l);
              if ld <> nil then begin
                vec := ld.GetInterpolatedPoint(fx,fy);
                al := l.GetAspectList<TDMXTiltPan>();
                TRY
                  for t := 0 to al.count-1 do begin
                    al[t].PanF := vec.x;
                    al[t].TiltF := vec.y;
                  end;
                FINALLY
                  AL.FREE;
                END;
              end;
            end;
          except
          end;
        end;
      end;
    end;
  end;



end;

{begin
    exit;
  end;

  tp := l as TDMXTiltPan;


  RevolutionTime := Params['revolution_time'].rValue;
  Stagger := Params['stagger'].rValue;
  MagnitudeX := Params['magnitudex'].rValue;
  MagnitudeY := Params['magnitudey'].rValue;
  Magnitude := Params['magnitude'].rValue;

  rRevTime := (rTime - (floor(rTime / RevolutionTime)*RevolutionTime))/RevolutionTime;
  rRevTime := rREvTime + (iGroupID * Stagger);


  x := sin(rRevTime*6.28);
  y := cos(rRevTime*6.28);
  x := x * MagnitudeX * Magnitude*tp.PanTiltRatio;
  y := y * Magnitudey * Magnitude;

  tp.TiltOffset := x;
  tp.PanOffset := y;

end;}

{ TFXCaliente }

procedure TFXCaliente.DefinePArameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;
  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'PatternType';
  def.Min := 0;
  def.Max := 255;
  def.Steps := 256;
  def.imptyp := eptFloat;
  def.rValue := 0.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Pattern';
  def.Min := 0;
  def.Max := 255;
  def.Steps := 256;
  def.imptyp := eptFloat;
  def.rValue := 0.0;
  FParamDefinitions.Add(def);

end;

procedure TFXCaliente.Init;
begin
  inherited;

end;

procedure TFXCaliente.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  cal: TDMXCaliente;
begin
  inherited;
  if l is TDMXCaliente then begin
    cal := TDMXCaliente(l);
    cal.Pattern := self.Params['Pattern'].ASInteger;
    cal.PatternType := self.Params['PatternType'].ASInteger;
  end;
end;

{ TFX4Banger }

procedure TFX4Banger.DefinePArameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;
  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'PatternType';
  def.Min := 0;
  def.Max := 255;
  def.Steps := 256;
  def.imptyp := eptFloat;
  def.rValue := 0.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Pattern';
  def.Min := 0;
  def.Max := 255;
  def.Steps := 256;
  def.imptyp := eptFloat;
  def.rValue := 0.0;
  FParamDefinitions.Add(def);

end;

procedure TFX4Banger.Init;
begin
  inherited;

end;

procedure TFX4Banger.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  cal: TDMX4Banger;
begin
  inherited;
  if l is TDMX4Banger then begin
    cal := TDMX4Banger(l);
    cal.Pattern := self.Params['Pattern'].ASInteger;
    cal.PatternType := self.Params['PatternType'].ASInteger;
  end;
end;
{ TFXSimpleGraphic }

procedure TFXSimpleGraphic.checkBitmapInit;
begin
  if fbm = nil then
    fbm := TFastBitmap.create;

end;

procedure TFXSimpleGraphic.CheckLoaded;
var
  loadFrom: string;
begin
  loadFrom := Params['filename'].AsString;
  if loadedfrom = loadfrom then
    exit;
  checkBitmapInit;
  loadedfrom := Params['FileName'].AsString;
  fbm.LoadFromFile(Params['FileName'].AsString);

end;

procedure TFXSimpleGraphic.CleanupBitmap;
begin
  fbm.free;
  fbm := nil;

end;

procedure TFXSimpleGraphic.DefineParameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;
  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'FileName';
  def.imptyp := eptString;
  def.entryType := eetString;
  FParamDefinitions.Add(def);

end;

procedure TFXSimpleGraphic.Detach;
begin
  CleanupBitmap;
  inherited;

end;

procedure TFXSimpleGraphic.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  pw: TDMXPixelWall;
  t,u: integer;
  r: nativefloat;
  Scale: nativefloat;
  Intensity: nativefloat;
  c: Tcolor;
begin
  inherited;

  //only works with derivatives of pixel wall
  if not (l is TDMXPixelWall) then
    exit;


  pw := l as TDMXPixelWall;
  CheckLoaded;

  for u := 0 to pw.Dimensions.y-1 do begin
    for t:= 0 to pw.Dimensions.X-1 do begin
      c := fbm.Canvas.Pixels[t+pw.pixeloffset.x,u+pw.pixeloffset.y];
      pw.pixels[t,u].color := c;
      pw.pixels[t,u].Intensity := 1;
    end;
  end;


end;

{ TFXVUByColor }

procedure TFXVUByColor.DefineParameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Scale';
  def.Min := 1;
  def.Max := 8;
  def.Steps := 8;
  def.imptyp := eptFloat;
  def.rValue := 80.0;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.Create;
  def.Name := 'Intensity';
  def.Min := 0.0;
  def.Max := 1.0;
  def.Steps := 255;
  def.imptyp := eptFloat;
  def.rValue := 0.1;
  FParamDefinitions.Add(def);
end;

procedure TFXVUByColor.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  cb: TDMXColorBar;
  pw: TDMXPixelWall;
  t,u, uu: integer;
  r: nativefloat;
  Scale: nativefloat;
  Intensity: nativefloat;
begin
  inherited;

  if not ((l is TDMXColorBar) or (l is TDMXPixelWall)) then
    exit;

  Scale := Params['Scale'].rValue;
  Intensity := Params['intensity'].rValue;


  if l is TDMXColorBar then begin
    cb := l as TDMXColorbar;


    for t:= 0 to 7 do begin
      r := power(rAmplitude,2) * Scale;



      r := r - t;
      if r < 0 then r := 0;
      if r > 1 then r := 1;
      cb.Sections[t].Intensity := (r * Intensity);
    end;
  end else
  if l is TDMXPixelWall then begin
    pw := l as TDMXPixelWall;
    if pw.strobeenable then exit;

    for u := 0 to pw.Dimensions.y-1 do begin
      uu := (pw.Dimensions.y-1) -u;

      for t:= 0 to pw.Dimensions.x-1 do begin
        r := power(rAmplitude,2) * Scale * 4;

        r := r - ((pw.Dimensions.y-1)-u);
        if r < 0 then r := 0;
        if r > 1 then r := 1;
        pw.pixels[t,uu].Color := colorblend(pw.pixels[t,uu].Color, clWhite, (r * Intensity));
      end;
    end;
  end;


end;

{ TFXPixelFire }

procedure TFXPixelFire.DefineParameters;
var
  def: TDMXEffectParameterDefinition;
begin
  inherited;

  def := TDMXEffectParameterDefinition.create;
  def.Name := 'x1';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 255;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.create;
  def.Name := 'x2';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 255;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.create;
  def.Name := 'y1';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 255;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);

  def := TDMXEffectParameterDefinition.create;
  def.Name := 'y2';
  def.Steps := 256;
  def.Min := 0;
  def.Max := 255;
  def.imptyp := eptFloat;
  FParamDefinitions.Add(def);




end;

procedure TFXPixelFire.Detach;
begin
  if detached then exit;



  inherited;
end;

procedure TFXPixelFire.UpdatePhysicsforlight(iGroupID, iGroupOf: integer;
  l: TDMXChannelCluster; iTriggerNumber: integer; rTime: NativeFloat;
  rAmplitude: single);
var
  pw: TDMXPixelWall;
  t,u: integer;
  r: nativefloat;
  Scale: nativefloat;
  Intensity: nativefloat;
  c: Tcolor;
  finalx, finaly: single;
  f: TFun;
  bx1,bx2,by1,by2: single;
  ix, iy: ni;
  width: single;
  height: single;
begin
  inherited;

  if iGroupID = 0 then
    globalfire.MoveFun;

  //only works with derivatives of pixel wall
  if not (l is TDMXPixelWall) then
    exit;


  pw := l as TDMXPixelWall;

  bx1 := self.params['x1'].rValue;
  bx2 := self.params['x2'].rValue;
  by1 := self.params['y1'].rValue;
  by2 := self.params['y2'].rValue;
  width := bx2-bx1;
  height := by2-by1;

  for u := 0 to pw.Dimensions.y-1 do begin
    for t:= 0 to pw.Dimensions.X-1 do begin
      pw.pixels[t,u].color := clBlack;
      pw.pixels[t,u].Intensity := 0;
    end;
  end;

  for t:= 0 to high(globalfire.funn) do begin
    f := globalfire.funn[t];
    finalX := f.points[0].X;
    finalY := f.points[0].Y;
    finalX := Interpolate(finalX, width , bx1, bx2);
    finalY := Interpolate(finalY, height , by1, by2);
    ix := round(finalx);
    iy := round(finaly);
    pw.pixels[ix,iy].color := clRed;
    pw.pixels[ix,iy].Intensity := pw.pixels[ix,iy].Intensity + 0.1;

  end;






end;

{ TFun }

procedure TFun.Init;
begin
  idx := 0;
  Self.points[0].X := (random(16000)/1000)-8;
  Self.points[0].Y := (random(9000)/1000)-4.5;
  v.x := 0;
  v.y := 0;
end;

procedure Tglobalfire.Init;
begin
  //
end;

procedure Tglobalfire.MoveFun;
var
  t,u: ni;
  p,m,d: TPOintF;

  v: TPOintF;
  gm: single;
CONST
  GRAVITY = 0.00001;
begin
  for t:= 0 to high(funn) do begin
    p := funn[t].points[funn[t].idx];
    v := funn[t].v;
    for u := 0 to high(funn) do begin
      m := funn[u].points[funn[u].idx];

      if t = u then continue;

      d := p-m;

      gm := GRAVITY*((d.x*d.x)+(d.y*d.y));

      if d.x < 0 then begin
        V.x := V.x + (gm);
      end else
      if d.x > 0 then begin
        V.x := V.x - (gm);
      end;

      if d.y < 0 then begin
        V.y := V.y + (gm);
      end else
      if d.y >  0then begin
        V.y := V.y - (gm);
      end;

    end;

    gm := GRAVITY*((p.x*p.x)+(p.y*p.y));

    if p.x < 0 then begin
      V.x := V.x + (gm);
    end else
    if p.x > 0 then begin
      V.x := V.x - (gm);
    end;

    if p.y < 0 then begin
      V.y := V.y + (gm);
    end else
    if p.y > 0 then begin
      V.y := V.y - (gm);
    end;

    funn[t].v := v;
    p := p + (v*0.01);
    funn[t].points[funn[t].idx] := p;

  end;
end;


initialization

Multiverse.EffectLibrary.RegisterClass(TFXSpotSwirl);
Multiverse.EffectLibrary.RegisterClass(TFXSpotPulseWave);
Multiverse.EffectLibrary.RegisterClass(TFXMoveThenBlankGroup);
Multiverse.EffectLibrary.RegisterClass(TFXSineWaveIntensityAdd);
Multiverse.EffectLibrary.RegisterClass(TFXVUBar);
Multiverse.EffectLibrary.RegisterClass(TFXVUByColor);
Multiverse.EffectLibrary.RegisterClass(TFXSpotStaticJoyPosition);
Multiverse.EffectLibrary.RegisterClass(TFXSpotStaticJoyPositions);
Multiverse.EffectLibrary.RegisterClass(TFXSpotByExternalControl);
Multiverse.EffectLibrary.RegisterClass(TFXCaliente);
Multiverse.EffectLibrary.RegisterClass(TFX4Banger);
Multiverse.EffectLibrary.RegisterClass(TFXSimpleGraphic);
Multiverse.EffectLibrary.RegisterClass(TFXPixelFire);
globalfire.init;



end.

