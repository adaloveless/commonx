unit PXL.Devices.Android;
{
  This file is part of Asphyre Framework, also known as Pascal eXtended Library (PXL).
  Copyright (c) 2000 - 2015  Yuriy Kotsarenko

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.
}
interface

{$INCLUDE PXL.Config.inc}

{.$DEFINE ANDROID_DEBUG}

uses
  SysUtils, Android.EGL, Android.AppGlue, PXL.TypeDef, PXL.Types, PXL.Classes, PXL.Timing, PXL.ImageFormats,
  PXL.Devices, PXL.Providers, PXL.Types.GLES;

type
  TRenderingStrategy = (Continuous, OnDemand);

  TPresentationAttribute = (KeepScreenOn, FullScreen);
  TPresentationAttributes = set of TPresentationAttribute;

  TTactileEventType = (TouchDown, TouchMove, TouchUp);

  TTactileEventInfo = record
    PointerCount: Integer;
    PointerIndex: Integer;
    Positions: array of TPoint2px;
  end;

  TTactileEvent = procedure(const EventType: TTactileEventType; const EventInfo: TTactileEventInfo);

  TAndroidApplication = class(TCustomDevice)
  private const
    ContentRectChangedWaitTime = 250; // milliseconds
    FastProcessingWaitTime = 500; // milliseconds
  private type
    TDeviceState = (NotCreated, Created, Liberated, Released);
    TContentRectState = (UpToDate, NeedChange, Modified);
  private
    FImageFormatManager: TImageFormatManager;
    FDeviceProvider: TGraphicsDeviceProvider;
    FDeviceContext: TGLESDeviceContext;

    FTimer: TMultimediaTimer;

    FAndroidApp: PAndroid_app;
    FDisplay: EGLDisplay;
    FSurface: EGLSurface;
    FContext: EGLContext;
    FContentRect: TIntRect;
    FDeviceState: TDeviceState;
    FTerminateRequested: Boolean;
    FRenderingStrategy: TRenderingStrategy;
    FNeedFastProcessing: Boolean;
    FFastProcessing: Boolean;
    FFastProcessingTime: Cardinal;
    FClientSurfaceDirty: Boolean;
    FPresentationAttributes: TPresentationAttributes;
    FNativeLibraryDir: StdString;
    FContentRectState: TContentRectState;
    FContentRectChangeTime: Cardinal;
    FDisplayScale: Single;
    FFontScale: Single;
    FOrientation: Integer;
    FPaused: Boolean;

    procedure SetRenderingStrategy(const Value: TRenderingStrategy);
    function GetFastProcessing: Boolean;

    function InitializeDisplay: Boolean;
    procedure FinalizeDisplay;
    function RecoverDisplay: Boolean;
    procedure InstantRepaint;
    procedure RenderAndMainLoop;

    procedure TimerProcess(const Sender: TObject);
    procedure HandleException(const E: Exception; const Action: StdString = '');

    procedure UpdateFastProcessing;
    procedure RetrieveDeviceState(out NewContentRect: TIntRect; out NewDisplayScale, NewFontScale: Single;
      out NewOrientation: Integer);
    procedure EnableWindowFlags;
    procedure UpdateFromDeviceState;
    procedure InitFromDeviceState;
    procedure CheckUpdateFromDeviceState;

    procedure CreateVolatileResources;
    procedure DestroyVolatileResources;
    procedure RestoreVolatileResources;
    procedure ReleaseVolatileResources;

    procedure HandleCommand(Cmd: LongInt);
  protected
    function GetDeviceContext: TCustomDeviceContext; override;
  public
    constructor Create(const AAndroidApp: PAndroid_app); reintroduce;
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;

    procedure Terminate;
    procedure Invalidate;
    procedure BeginFastProcessing;

    property ImageFormatManager: TImageFormatManager read FImageFormatManager;

    property AndroidApp: PAndroid_app read FAndroidApp;

    property Display: EGLDisplay read FDisplay;
    property Surface: EGLSurface read FSurface;
    property Context: EGLContext read FContext;

    property Timer: TMultimediaTimer read FTimer;

    property RenderingStrategy: TRenderingStrategy read FRenderingStrategy write SetRenderingStrategy;
    property PresentationAttributes: TPresentationAttributes read FPresentationAttributes write FPresentationAttributes;

    property FastProcessing: Boolean read GetFastProcessing;
    property NativeLibraryDir: StdString read FNativeLibraryDir;

    property ContentRect: TIntRect read FContentRect;
    property DisplayScale: Single read FDisplayScale;
    property FontScale: Single read FFontScale;
    property Orientation: Integer read FOrientation;
    property Paused: Boolean read FPaused;
  end;

var
  Application: TAndroidApplication = nil;

  { This event occurs right after Android application has been started. During this event and only at this event the
    presentation attributes can be changed. Note that the rendering system is not yet ready at this point. }
  HookApplicationCreate: TProcedure = nil;

  { This event occurs right before Android application will be destroyed. }
  HookApplicationDestroy: TProcedure = nil;

  { This event occurs when Android application needs to repaint itself, which includes clearing the screen. }
  HookApplicationPaint: TProcedure = nil;

  { This event occurs right before Android main loop (which includes rendering). This can be useful for some quick
    checks before rendering starts, but it is better to use time-consuming code in "MainLoop" event to take advantage
    of CPU/GPU parallelism. }
  HookApplicationBeforeMainLoop: TProcedure = nil;

  { This event occurs right after rendering commands have been sent to GPU, but before the scene is presented on the
    screen. Any time-consuming processing should be made here to take advantage of CPU/GPU parallelism, so while GPU
    is rendering the current scene, CPU can work on other calculations. }
  HookApplicationMainLoop: TProcedure = nil;

  { This event occurs during system processing loop, which always occurs even when application is in paused state.
    Only very critical and quick tasks can be done here. }
  HookApplicationSystemLoop: TProcedure = nil;

  { This event occurs during normal application execution (when not in paused state) at rate of 60 times per second. }
  HookApplicationProcess: TProcedure = nil;

  { This event occurs when device has changed orientation, display or font scale. }
  HookApplicationDeviceChange: TProcedure = nil;

  { This event occurs when device has resumed executing this application. }
  HookApplicationDeviceResume: TProcedure = nil;

  { This event occurs when device has paused executing this application. }
  HookApplicationDevicePause: TProcedure = nil;

  { This event occurs when application should create device-related resources such as canvas, load images and so on. }
  HookApplicationCreateResources: TProcedure = nil;

  { This event occurs when application should release device-related resources such as canvas and images. }
  HookApplicationDestroyResources: TProcedure = nil;

  { This event occurs when application is restored from pause state and can now recreate any graphics-related
    resources. }
  HookApplicationRestoreResources: TProcedure = nil;

  { This event occurs when application is paused to release any graphics-related resources. }
  HookApplicationReleaseResources: TProcedure = nil;

  { Occurs to handle any screen tactile and/or mouse events. }
  HookApplicationTactileEvent: TTactileEvent = nil;

// This function needs to be passed to "android_main" at library startup.
procedure DefaultApplicationEntry(AndroidApp: Pandroid_app);

implementation

uses
  jni, Android.GLES2, Android.Input, Android.Looper, Android.NativeWindow, Android.NativeActivity, Android.JniClasses,
  PXL.Logs, PXL.Bitmaps, PXL.Providers.GLES;

constructor TAndroidApplication.Create(const AAndroidApp: PAndroid_app);
begin
  FImageFormatManager := TImageFormatManager.Create;
  FDeviceProvider := TGLESProvider.Create(FImageFormatManager);

  inherited Create(FDeviceProvider);

  Increment_PXL_ClassInstances;

  FDeviceContext := TGLESDeviceContext.Create(Self);

  FAndroidApp := AAndroidApp;
  FTechnology := TDeviceTechnology.OpenGL_ES;
  FTechVersion := $200;
  FDeviceState := TDeviceState.NotCreated;
  FTerminateRequested := False;
  FFastProcessing := False;
  FRenderingStrategy := TRenderingStrategy.Continuous;
  FClientSurfaceDirty := False;
  FContentRectState := TContentRectState.UpToDate;
  FPresentationAttributes := [];
  FPaused := False;

  FTimer := TMultimediaTimer.Create;
  FTimer.OnProcess := TimerProcess;
end;

destructor TAndroidApplication.Destroy;
begin
  try
    FreeAndNil(FTimer);
    FreeAndNil(FDeviceContext);
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;

  FDeviceProvider.Free;
  FImageFormatManager.Free;
end;

function TAndroidApplication.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FDeviceContext;
end;

procedure TAndroidApplication.SetRenderingStrategy(const Value: TRenderingStrategy);
begin
  if FRenderingStrategy <> Value then
  begin
    FRenderingStrategy := Value;

    if FRenderingStrategy = TRenderingStrategy.OnDemand then
      Invalidate;
  end;
end;

function TAndroidApplication.GetFastProcessing: Boolean;
begin
  Result := FNeedFastProcessing or FFastProcessing;
end;

function TAndroidApplication.InitializeDisplay: Boolean;
const
  ContextAttribs: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
const
  ConfigAttribs: array[0..10] of EGLint = (
    EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
    EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
    EGL_BLUE_SIZE, 8,
    EGL_GREEN_SIZE, 8,
    EGL_RED_SIZE, 8,
    EGL_NONE);
var
  Config: EGLConfig;
  NumConfigs, Format: EGLint;
begin
  FDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if FDisplay = nil then
  begin
    LogText('(EGL) Could not retreive default display.', TLogType.Error);
    Exit(False);
  end;

  if eglInitialize(FDisplay, nil, nil) <> EGL_TRUE then
  begin
    LogText('(EGL) Could not initialize display.', TLogType.Error);
    Exit(False);
  end;

  if eglChooseConfig(FDisplay, @ConfigAttribs[0], @Config, 1, @NumConfigs) <> EGL_TRUE then
  begin
    LogText('(EGL) Could not find suitable configuration.', TLogType.Error);
    FinalizeDisplay;
    Exit(False);
  end;

  eglGetConfigAttrib(FDisplay, Config, EGL_NATIVE_VISUAL_ID, @Format);
  ANativeWindow_setBuffersGeometry(FAndroidApp.window, 0, 0, Format);

  FSurface := eglCreateWindowSurface(FDisplay, Config, FAndroidApp.window, nil);
  if FSurface = EGL_NO_SURFACE then
  begin
    LogText('(EGL) Could create window surface.', TLogType.Error);
    FinalizeDisplay;
    Exit(False);
  end;

  FContext := eglCreateContext(FDisplay, Config, EGL_NO_CONTEXT, @ContextAttribs[0]);
  if FContext = EGL_NO_CONTEXT then
  begin
    LogText('(EGL) Could create context.', TLogType.Error);
    FinalizeDisplay;
    Exit(False);
  end;

  if eglMakeCurrent(FDisplay, FSurface, FSurface, FContext) <> EGL_TRUE then
  begin
    LogText('(EGL) Could create make the context current.', TLogType.Error);
    FinalizeDisplay;
    Exit(False);
  end;

  Result := True;
end;

procedure TAndroidApplication.FinalizeDisplay;
begin
  if FDisplay <> EGL_NO_DISPLAY then
  begin
    eglMakeCurrent(FDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);

    if FContext <> EGL_NO_CONTEXT then
    begin
      eglDestroyContext(FDisplay, FContext);
      FContext := EGL_NO_CONTEXT;
    end;

    if FSurface <> EGL_NO_SURFACE then
    begin
      eglDestroySurface(FDisplay, FSurface);
      FSurface := EGL_NO_SURFACE;
    end;

    eglTerminate(FDisplay);
    FDisplay := EGL_NO_DISPLAY;
  end;

  FContentRect := IntRectBDS(0, 0, 0, 0);
end;

function TAndroidApplication.RecoverDisplay: Boolean;
begin
  if (FDisplay = EGL_NO_DISPLAY) or (FSurface = EGL_NO_SURFACE) then
    Exit(False);

  if eglMakeCurrent(FDisplay, FSurface, FSurface, FContext) <> EGL_TRUE then
    begin
      LogText('(EGL) Could create make the context current.', TLogType.Error);
      FinalizeDisplay;
      Exit(False);
    end;

  Result := True;
end;

function TAndroidApplication.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor;
  const DepthValue: Single; const StencilValue: Cardinal): Boolean;
var
  Flags: Cardinal;
begin
  if ClearTypes = [] then
    Exit(False);

  Flags := 0;

  if TClearType.Color in ClearTypes then
  begin
    glClearColor(TIntColorRec(ColorValue).Red / 255.0, TIntColorRec(ColorValue).Green / 255.0,
      TIntColorRec(ColorValue).Blue / 255.0, TIntColorRec(ColorValue).Alpha / 255.0);
    Flags := Flags or GL_COLOR_BUFFER_BIT;
  end;

  if TClearType.Depth in ClearTypes then
  begin
    glClearDepthf(DepthValue);
    Flags := Flags or GL_DEPTH_BUFFER_BIT;
  end;

  if TClearType.Stencil in ClearTypes then
  begin
    glClearStencil(StencilValue);
    Flags := Flags or GL_STENCIL_BUFFER_BIT;
  end;

  glClear(Flags);

  Result := glGetError = GL_NO_ERROR;
end;

procedure TAndroidApplication.InstantRepaint;
begin
  if (FDisplay = EGL_NO_DISPLAY) or (FSurface = EGL_NO_SURFACE) or (FDeviceState <> TDeviceState.Created) then
    Exit;

  { Viewport is given using traditional OpenGL coordinates, where origin is located at bottom left. Therefore, status
    bar height affects the height of viewport, but not its vertical position. }
  glViewport(FContentRect.Left, 0, FContentRect.Width, FContentRect.Height);

  try
    if Assigned(HookApplicationPaint) then
      HookApplicationPaint;
  except
    on E: Exception do
      HandleException(E);
  end;

  eglSwapBuffers(FDisplay, FSurface);
end;

procedure TAndroidApplication.RenderAndMainLoop;
var
  NeedSwapBuffers: Boolean;
begin
  try
    if (FDeviceState = TDeviceState.Created) and Assigned(HookApplicationBeforeMainLoop) then
      HookApplicationBeforeMainLoop;
  except
    on E: Exception do
      HandleException(E);
  end;

  NeedSwapBuffers := False;

  if (FRenderingStrategy = TRenderingStrategy.Continuous) or FClientSurfaceDirty then
  begin
    if (FDisplay <> EGL_NO_DISPLAY) and (FSurface <> EGL_NO_SURFACE) and (FDeviceState = TDeviceState.Created) then
    begin
      if FRenderingStrategy = TRenderingStrategy.OnDemand then
        FClientSurfaceDirty := False;

      { Viewport is given using traditional OpenGL coordinates, where origin is located at bottom left.
        Therefore, status bar height affects the height of viewport, but not its vertical position. }
      glViewport(FContentRect.Left, 0, FContentRect.Width, FContentRect.Height);

      try
        if Assigned(HookApplicationPaint) then
          HookApplicationPaint;
      except
        on E: Exception do
          HandleException(E);
      end;

      NeedSwapBuffers := True;
    end;
  end;

  if FDeviceState = TDeviceState.Created then
  try
    if Assigned(HookApplicationMainLoop) then
      HookApplicationMainLoop;

    Timer.NotifyTick(False);
    Timer.Process;
  except
    on E: Exception do
      HandleException(E);
  end;

  if NeedSwapBuffers then
    eglSwapBuffers(FDisplay, FSurface);
end;

procedure TAndroidApplication.TimerProcess(const Sender: TObject);
begin
  if Assigned(HookApplicationProcess) then
    HookApplicationProcess;
end;

procedure TAndroidApplication.HandleException(const E: Exception; const Action: StdString);
begin
  if Length(Action) > 0 then
    LogText('[Exception during ' + Action + ' in "' + E.ClassName + '"]: ' + E.Message, TLogType.Error)
  else
    LogText('[Exception in "' + E.ClassName + '"]: ' + E.Message, TLogType.Error);

  Terminate;
end;

procedure TAndroidApplication.Terminate;
begin
  if not FTerminateRequested then
  begin
    ANativeActivity_finish(FAndroidApp.activity);
    FTerminateRequested := True;
  end;
end;

procedure TAndroidApplication.Invalidate;
begin
  inherited;

  if FRenderingStrategy = TRenderingStrategy.OnDemand then
  begin
    FClientSurfaceDirty := True;
    BeginFastProcessing;
  end;
end;

procedure TAndroidApplication.BeginFastProcessing;
begin
  if FRenderingStrategy = TRenderingStrategy.OnDemand then
    FNeedFastProcessing := True;
end;

procedure TAndroidApplication.UpdateFastProcessing;
var
  CurTime: Cardinal;
begin
  if FNeedFastProcessing then
  begin
    FFastProcessing := True;
    FFastProcessingTime := GetSystemTickCount;

    FNeedFastProcessing := False;
  end
  else if FFastProcessing then
  begin
    CurTime := GetSystemTickCount;
    if TickCountInBetween(FFastProcessingTime, CurTime) > FastProcessingWaitTime then
      FFastProcessing := False;
  end;
end;

procedure TAndroidApplication.RetrieveDeviceState(out NewContentRect: TIntRect;
  out NewDisplayScale, NewFontScale: Single; out NewOrientation: Integer);
var
  Session: TJavaSession;

  JActivity: TJActivity;
  JDisplayMetrics: TJDisplayMetrics;
  JWindowManager: TJWindowManager;
  JDisplay: TJDisplay;
  JWindow: TJWindow;
  JView: TJView;
  JRect: TJRect;
  JApplicationInfo: TJApplicationInfo;

  Rect: jobject;
  Window: jobject;
  View: jobject;
  Display: jobject;
  WindowManager: jobject;
  DisplayMetrics: jobject;
  ApplicationInfo: jobject;
begin
  FillChar(NewContentRect, SizeOf(TIntRect), 0);
  NewDisplayScale := 1.0;
  NewFontScale := 1.0;
  NewOrientation := 0;

  Session := TJavaSession.Create(AndroidApp);

  JActivity := TJActivity.Create(Session);
  JDisplayMetrics := TJDisplayMetrics.Create(Session);
  JWindowManager := TJWindowManager.Create(Session);
  JDisplay := TJDisplay.Create(Session);
  JWindow := TJWindow.Create(Session);
  JView := TJView.Create(Session);
  JRect := TJRect.Create(Session);

  Window := JActivity.GetWindow(AndroidApp.activity.clazz);
  if Window <> nil then
  begin
    View := JWindow.GetDecorView(Window);
    if View <> nil then
    begin
      Rect := JRect.New;

      JView.GetDrawingRect(View, Rect);

      NewContentRect.Left := JRect.Left[Rect];
      NewContentRect.Top := JRect.Top[Rect];
      NewContentRect.Right := JRect.Right[Rect];
      NewContentRect.Bottom := JRect.Bottom[Rect];

{     LogText('DrawingRect: ' + IntToStr(NewContentRect.Left) + ', ' + IntToStr(NewContentRect.Top) + ', ' +
        IntToStr(NewContentRect.Right) + ', ' + IntToStr(NewContentRect.Bottom));}

      JView.GetWindowVisibleDisplayFrame(View, Rect);

      if not (TPresentationAttribute.FullScreen in PresentationAttributes) then
        Inc(NewContentRect.Top, JRect.Top[Rect]);

{     NewContentRect.Left := JRect.Left[Rect];
      NewContentRect.Top := JRect.Top[Rect];
      NewContentRect.Right := JRect.Right[Rect];
      NewContentRect.Bottom := JRect.Bottom[Rect];

      LogText('DisplayFrame: ' + IntToStr(NewContentRect.Left) + ', ' + IntToStr(NewContentRect.Top) + ', ' +
        IntToStr(NewContentRect.Right) + ', ' + IntToStr(NewContentRect.Bottom));}

      Session.Release(Rect);
      Session.Release(View);
    end;

    Session.Release(Window);
  end;

  WindowManager := JActivity.GetWindowManager(AndroidApp.activity.clazz);
  if WindowManager <> nil then
  begin
    Display := JWindowManager.GetDefaultDisplay(WindowManager);
    if Display <> nil then
    begin
      DisplayMetrics := JDisplayMetrics.New;

      JDisplay.GetMetrics(Display, DisplayMetrics);

      NewDisplayScale := JDisplayMetrics.Density[DisplayMetrics];
      NewFontScale := JDisplayMetrics.ScaledDensity[DisplayMetrics];

      case JDisplay.GetRotation(Display) of
        1: // ROTATION_90
          NewOrientation := 1;

        2: // ROTATION_180
          NewOrientation := 2;

        3: // ROTATION_270
          NewOrientation := 3;
      end;

      Session.Release(DisplayMetrics);
      Session.Release(Display);
    end;

    Session.Release(WindowManager);
  end;

  JRect.Free;
  JView.Free;
  JWindow.Free;
  JDisplay.Free;
  JWindowManager.Free;
  JDisplayMetrics.Free;

  if Length(FNativeLibraryDir) < 1 then
  begin
    JApplicationInfo := TJApplicationInfo.Create(Session);

    ApplicationInfo := JActivity.GetApplicationInfo(AndroidApp.activity.clazz);
    if ApplicationInfo <> nil then
    begin
      FNativeLibraryDir := JApplicationInfo.NativeLibraryDir[ApplicationInfo];

      Session.Release(ApplicationInfo);
    end;

    JApplicationInfo.Free;
  end;

  JActivity.Free;

  Session.Free;
end;

procedure TAndroidApplication.EnableWindowFlags;
var
  Session: TJavaSession;
  JActivity: TJActivity;
  JWindow: TJWindow;
  Window: jobject;
begin
  Session := TJavaSession.Create(AndroidApp);

  JActivity := TJActivity.Create(Session);
  JWindow := TJWindow.Create(Session);

  Window := JActivity.GetWindow(AndroidApp.activity.clazz);
  if Window <> nil then
  begin
    if TPresentationAttribute.KeepScreenOn in PresentationAttributes then
      JWindow.AddFlags(Window, TJWindow.FLAG_KEEP_SCREEN_ON);

    if TPresentationAttribute.FullScreen in PresentationAttributes then
      JWindow.AddFlags(Window, TJWindow.FLAG_FULLSCREEN);

    Session.Release(Window);
  end;

  JWindow.Free;
  JActivity.Free;

  Session.Free;
end;

procedure TAndroidApplication.UpdateFromDeviceState;
var
  NewContentRect: TIntRect;
  NewDisplayScale, NewFontScale: Single;
  NewOrientation: Integer;
  CurTickCount: Cardinal;
  NeedNotifyChange: Boolean;
begin
  RetrieveDeviceState(NewContentRect, NewDisplayScale, NewFontScale, NewOrientation);

  NeedNotifyChange := False;
  CurTickCount := GetSystemTickCount;

  if FContentRect <> NewContentRect then
  begin
    FContentRect := NewContentRect;

    if FContentRectState = TContentRectState.NeedChange then
      FContentRectState := TContentRectState.Modified;

  {$IFDEF ANDROID_DEBUG}
    LogText('ContentRect: ' + IntToStr(FContentRect.Left) + ', ' + IntToStr(FContentRect.Top) + ', ' +
      IntToStr(FContentRect.Right) + ', ' + IntToStr(FContentRect.Bottom));
  {$ENDIF}

    FContentRectChangeTime := CurTickCount;
    NeedNotifyChange := True;
  end;

  if (Abs(NewDisplayScale - FDisplayScale) > VectorEpsilon) or (Abs(NewFontScale - FFontScale) > VectorEpsilon) or
    (NewOrientation <> FOrientation) then
    begin
      FDisplayScale := NewDisplayScale;
      FFontScale := NewFontScale;
      FOrientation := NewOrientation;

    {$IFDEF ANDROID_DEBUG}
      LogText('Display Scale: ' + FloatToStr(FDisplayScale));
      LogText('Font Scale: ' + FloatToStr(FFontScale));
      LogText('Rotation: ' + IntToStr(FOrientation));
    {$ENDIF}

      FContentRectChangeTime := CurTickCount;
      NeedNotifyChange := True;
    end;

  if NeedNotifyChange then
  begin
    try
      if Assigned(HookApplicationDeviceChange) then
        HookApplicationDeviceChange;
    except
      on E: Exception do
        HandleException(E);
    end;

    Invalidate;
  end;

  if (FContentRectState = TContentRectState.Modified) and (TickCountInBetween(CurTickCount, FContentRectChangeTime) >
    ContentRectChangedWaitTime) then
  begin
    FContentRectState := TContentRectState.UpToDate;

    if not NeedNotifyChange then
      Invalidate;
  end;
end;

procedure TAndroidApplication.InitFromDeviceState;
begin
  RetrieveDeviceState(FContentRect, FDisplayScale, FFontScale, FOrientation);

{$IFDEF ANDROID_DEBUG}
  LogText('ContentRect: ' + IntToStr(FContentRect.Left) + ', ' + IntToStr(FContentRect.Top) + ', ' +
    IntToStr(FContentRect.Right) + ', ' + IntToStr(FContentRect.Bottom));

  LogText('Display Scale: ' + FloatToStr(FDisplayScale));
  LogText('Font Scale: ' + FloatToStr(FFontScale));
  LogText('Rotation: ' + IntToStr(FOrientation));
{$ENDIF}

  try
    if Assigned(HookApplicationDeviceChange) then
      HookApplicationDeviceChange;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TAndroidApplication.CheckUpdateFromDeviceState;
begin
  if FContentRectState <> TContentRectState.UpToDate then
    UpdateFromDeviceState;
end;

procedure TAndroidApplication.CreateVolatileResources;
begin
{$IFDEF ANDROID_DEBUG}
  LogText(' + Create Resources');
{$ENDIF}

  try
    if Assigned(HookApplicationCreateResources) then
      HookApplicationCreateResources;
  except
    on E: Exception do
      HandleException(E, 'CreateVolatile');
  end;
end;

procedure TAndroidApplication.DestroyVolatileResources;
begin
{$IFDEF ANDROID_DEBUG}
  LogText(' + Destroy Resources');
{$ENDIF}

  try
    if Assigned(HookApplicationDestroyResources) then
      HookApplicationDestroyResources;
  except
    on E: Exception do
      HandleException(E, 'DestroyVolatile');
  end;
end;

procedure TAndroidApplication.RestoreVolatileResources;
begin
{$IFDEF ANDROID_DEBUG}
  LogText(' + Restore Resources');
{$ENDIF}

  try
    OnRestore.Notify(Self);

    if Assigned(HookApplicationRestoreResources) then
      HookApplicationRestoreResources;
  except
    on E: Exception do
      HandleException(E, 'RestoreVolatile');
  end;
end;

procedure TAndroidApplication.ReleaseVolatileResources;
begin
{$IFDEF ANDROID_DEBUG}
  LogText(' + Release Resources');
{$ENDIF}

  try
    if Assigned(HookApplicationReleaseResources) then
      HookApplicationReleaseResources;

    OnRelease.Notify(Self);
  except
    on E: Exception do
      HandleException(E, 'ReleaseVolatile');
  end;
end;

procedure TAndroidApplication.HandleCommand(Cmd: LongInt);
begin
  case Cmd of
    APP_CMD_INIT_WINDOW:
      if FAndroidApp.window <> nil then
      begin
        InitializeDisplay;
        InitFromDeviceState;

        if FDeviceState = TDeviceState.NotCreated then
        begin
        {$IFDEF ANDROID_DEBUG}
          LogText('Create Volatile Resources');
        {$ENDIF}

          CreateVolatileResources;
          FDeviceState := TDeviceState.Created;
        end
        else if FDeviceState = TDeviceState.Released then
        begin
        {$IFDEF ANDROID_DEBUG}
          LogText('Restore Volatile Resources');
        {$ENDIF}

          RestoreVolatileResources;
          FDeviceState := TDeviceState.Created;
        end;

        InstantRepaint;
      end;

    APP_CMD_TERM_WINDOW:
    begin
      if FDeviceState = TDeviceState.Created then
      begin
        ReleaseVolatileResources;
        FDeviceState := TDeviceState.Released;
      end
      else if FDeviceState = TDeviceState.Liberated then
        FDeviceState := TDeviceState.Released;

      FinalizeDisplay;
    end;

    APP_CMD_LOST_FOCUS:
      InstantRepaint;

    APP_CMD_CONFIG_CHANGED:
    begin
      FContentRectState := TContentRectState.NeedChange;
      FContentRectChangeTime := GetSystemTickCount;
    end;

{    APP_CMD_LOW_MEMORY: }

    APP_CMD_RESUME:
      begin
        if FDeviceState = TDeviceState.Liberated then
          if RecoverDisplay then
          begin
            RestoreVolatileResources;
            FDeviceState := TDeviceState.Created;
          end;

        Timer.Reset;

        FPaused := False;

        try
          if Assigned(HookApplicationDeviceResume) then
            HookApplicationDeviceResume;
        except
          on E: Exception do
            HandleException(E, 'CmdResume');
        end;
      end;

{    APP_CMD_SAVE_STATE: }

    APP_CMD_PAUSE:
      begin
        if FDeviceState = TDeviceState.Created then
        begin
          ReleaseVolatileResources;
          FDeviceState := TDeviceState.Liberated;
        end;

        Timer.Reset;
        FPaused := True;

        try
          if Assigned(HookApplicationDevicePause) then
            HookApplicationDevicePause;
        except
          on E: Exception do
            HandleException(E, 'CmdPause');
        end;
      end;

    APP_CMD_DESTROY:
      if FDeviceState <> TDeviceState.NotCreated then
      begin
        DestroyVolatileResources;
        FDeviceState := TDeviceState.NotCreated;
      end;
  end;
end;

function ApplicationHandleInput(AndroidApp: PAndroid_app; Event: PAInputEvent): LongInt; cdecl;
var
  Application: TAndroidApplication;
  EventType, EventAction, I: LongInt;
  EventInfo: TTactileEventInfo;
begin
  Application := TAndroidApplication(AndroidApp.userData);

  EventType := AInputEvent_getType(Event);

  if (EventType = AINPUT_EVENT_TYPE_MOTION) and Assigned(HookApplicationTactileEvent) then
  begin
    FillChar(EventInfo, SizeOf(TTactileEventInfo), 0);

    EventAction := AKeyEvent_getAction(Event);

    EventInfo.PointerIndex := (EventAction and AMOTION_EVENT_ACTION_PointerIndex_MASK) shr
      AMOTION_EVENT_ACTION_PointerIndex_SHIFT;

    EventAction := EventAction and AMOTION_EVENT_ACTION_MASK;
    EventInfo.PointerCount := AMotionEvent_getPointerCount(Event);

    { Note: AMOTION_EVENT_ACTION_POINTER_UP seems to be unreliable when more than 2 pointers are involved in gesture,
      so it doesn't return proper Pointer Index. }

    SetLength(EventInfo.Positions, EventInfo.PointerCount);

    for I := 0 to EventInfo.PointerCount - 1 do
    begin
      EventInfo.Positions[I].X := Round(AMotionEvent_getX(Event, I));
      EventInfo.Positions[I].Y := Round(AMotionEvent_getY(Event, I));
    end;

    try
      case EventAction of
        AMOTION_EVENT_ACTION_DOWN:
          HookApplicationTactileEvent(TTactileEventType.TouchDown, EventInfo);

        AMOTION_EVENT_ACTION_MOVE:
          HookApplicationTactileEvent(TTactileEventType.TouchMove, EventInfo);

        AMOTION_EVENT_ACTION_UP:
          HookApplicationTactileEvent(TTactileEventType.TouchUp, EventInfo);

        AMOTION_EVENT_ACTION_POINTER_DOWN:
          HookApplicationTactileEvent(TTactileEventType.TouchDown, EventInfo);

        AMOTION_EVENT_ACTION_POINTER_UP:
          HookApplicationTactileEvent(TTactileEventType.TouchUp, EventInfo);
      end;
    except
      on E: Exception do
        Application.HandleException(E, 'HandleInput');
    end;
  end;

  Result := 0;
end;

procedure ApplicationHandleCommand(AndroidApp: Pandroid_app; Cmd: LongInt); cdecl;
var
  Application: TAndroidApplication;
begin
  Application := TAndroidApplication(AndroidApp.userData);

{$IFDEF ANDROID_DEBUG}
  case Cmd of
    APP_CMD_INIT_WINDOW:
      LogText('APP_CMD_INIT_WINDOW');

    APP_CMD_TERM_WINDOW:
      LogText('APP_CMD_TERM_WINDOW');

    APP_CMD_GAINED_FOCUS:
      LogText('APP_CMD_GAINED_FOCUS');

    APP_CMD_LOST_FOCUS:
      LogText('APP_CMD_LOST_FOCUS');

    APP_CMD_CONFIG_CHANGED:
      LogText('APP_CMD_CONFIG_CHANGED');

    APP_CMD_LOW_MEMORY:
      LogText('APP_CMD_LOW_MEMORY');

    APP_CMD_START:
      LogText('APP_CMD_START');

    APP_CMD_RESUME:
      LogText('APP_CMD_RESUME');

    APP_CMD_SAVE_STATE:
      LogText('APP_CMD_SAVE_STATE');

    APP_CMD_PAUSE:
      LogText('APP_CMD_PAUSE');

    APP_CMD_STOP:
      LogText('APP_CMD_STOP');

    APP_CMD_DESTROY:
      LogText('APP_CMD_DESTROY');
  end;
{$ENDIF}


  Application.HandleCommand(Cmd);
end;

{$IFDEF ANDROID_DEBUG}
var
  ApplicationEntryCallCount: Integer = 0;
{$ENDIF}

procedure DefaultApplicationEntry(AndroidApp: Pandroid_app);
const
  DefaultPollWaitTime = 50; // milliseconds
  DefaultPollWaitTimePaused = 250; // milliseconds
var
  PollWaitTime: LongInt;
  Terminated: Boolean;
  PollSource: Pandroid_poll_source;
  PollResult, Events: LongInt;
begin
{$IFDEF ANDROID_DEBUG}
  Inc(ApplicationEntryCallCount);

  if ApplicationEntryCallCount < 2 then
    LogText('PXL_AppGlue: DefaultApplicationEntry')
  else
    LogText('PXL_AppGlue: Secondary (?) DefaultApplicationEntry');
{$ENDIF}

  if not LoadEGLExtensions then
    LogText('Failed to load EGL extensions.', TLogType.Error);

  if not LoadGLES2Extensions then
    LogText('Failed to load GLESv2 extensions.', TLogType.Error);

  Application := TAndroidApplication.Create(AndroidApp);

  AndroidApp.onAppCmd := ApplicationHandleCommand;
  AndroidApp.onInputEvent := ApplicationHandleInput;
  AndroidApp.userData := Application;

  TAssetStream.AssetManager := AndroidApp.activity.assetManager;

  try
    if Assigned(HookApplicationCreate) then
      HookApplicationCreate;
  except
    on E: Exception do
      Application.HandleException(E, 'Main (1)');
  end;

  Application.EnableWindowFlags;

  Terminated := False;

  while not Terminated do
  begin
    repeat
      if not Application.Paused then
      begin
        if Application.RenderingStrategy = TRenderingStrategy.OnDemand then
        begin
          if Application.FastProcessing then
            PollWaitTime := 0
          else
            PollWaitTime := DefaultPollWaitTime;
        end
        else
          PollWaitTime := 0;
      end
      else
        PollWaitTime := DefaultPollWaitTimePaused;

      PollResult := ALooper_pollAll(PollWaitTime, nil, @Events, PPointer(@PollSource));
      if PollResult = ALOOPER_POLL_ERROR then
      begin
        LogText('Looper object returned error.', TLogType.Error);

        Terminated := True;
        Break;
      end;

      if (PollSource <> nil) or (Events > 0) then
        Application.BeginFastProcessing;

      if PollSource <> nil then
        PollSource.process(AndroidApp, PollSource);

      if AndroidApp.destroyRequested <> 0 then
        Terminated := True;
    until (PollResult < 0) or Terminated;

    if Terminated then
      Break;

    Application.CheckUpdateFromDeviceState;

    try
      if Assigned(HookApplicationSystemLoop) then
        HookApplicationSystemLoop;
    except
      on E: Exception do
        Application.HandleException(E, 'Main (2)');
    end;

    Application.RenderAndMainLoop;
    Application.UpdateFastProcessing;
  end;

  try
    if Assigned(HookApplicationDestroy) then
      HookApplicationDestroy;
  except
    on E: Exception do
      Application.HandleException(E, 'Main (3)');
  end;

  FreeAndNil(Application);

  if TAssetStream.AssetManager = AndroidApp.activity.assetManager then
    TAssetStream.AssetManager := nil;

  UnloadGLES2Extensions;
  UnloadEGLExtensions;

{$IFDEF ANDROID_DEBUG}
  LogText('Exiting PXL_MAIN.');
{$ENDIF}
end;

end.
