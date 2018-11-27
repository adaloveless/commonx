unit Android.JniClasses;
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
{.$DEFINE JNI_DEBUG}

uses
  jni, Android.AppGlue;

type
  TJavaSession = class
  private
    FAndroidApp: PAndroid_app;
    FEnv: PJNIEnv;
  public
    constructor Create(AAndroidApp: PAndroid_app);
    destructor Destroy; override;

    procedure Release(var JavaObj: jobject);

    property AndroidApp: PAndroid_app read FAndroidApp;
    property Env: PJNIEnv read FEnv;
  end;

  TCustomJavaClass = class
  strict private
    FSession: TJavaSession;
    FJavaClass: jclass;

    MethodIDs: array of jmethodID;
    FieldIDs: array of jfieldID;

    function SessionValid: Boolean;
    procedure LoadJavaClass;

    procedure AddMethodID(MethodID: jmethodID);
    procedure ReleaseMethodIDs;

    procedure AddFieldID(FieldID: jfieldID);
    procedure ReleaseFieldIDs;
  strict protected
    function GetJavaMethod(const MethodName, Signature: AnsiString): jmethodID;
    function GetJavaStaticMethod(const MethodName, Signature: AnsiString): jmethodID;
    function GetJavaField(const FieldName, Signature: AnsiString): jfieldID;

    function JavaNewObject(InitID: jmethodID): jobject;

    function GetIntField(JavaObj: jobject; FieldID: jfieldID): Integer;
    procedure SetIntField(JavaObj: jobject; FieldID: jfieldID; Value: Integer);

    function GetFloatField(JavaObj: jobject; FieldID: jfieldID): Single;
    procedure SetFloatField(JavaObj: jobject; FieldID: jfieldID; Value: Single);
  protected
    function GetJavaClassLink: AnsiString; virtual; abstract;
    procedure LoadJavaContent; virtual; abstract;

    property Session: TJavaSession read FSession;
  public
    constructor Create(ASession: TJavaSession);
    destructor Destroy; override;

    property JavaClass: jclass read FJavaClass;
  end;

  TJRect = class(TCustomJavaClass)
  private
    javaMethod_init: jmethodID;
    javaField_left: jfieldID;
    javaField_top: jfieldID;
    javaField_right: jfieldID;
    javaField_bottom: jfieldID;

    function GetLeft(Rect: jobject): Integer;
    procedure SetLeft(Rect: jobject; Value: Integer);
    function GetTop(Rect: jobject): Integer;
    procedure SetTop(Rect: jobject; Value: Integer);
    function GetRight(Rect: jobject): Integer;
    procedure SetRight(Rect: jobject; Value: Integer);
    function GetBottom(Rect: jobject): Integer;
    procedure SetBottom(Rect: jobject; Value: Integer);
  protected
    function GetJavaClassLink: AnsiString; override;
    procedure LoadJavaContent; override;
  public
    function New: jobject;

    property Left[Rect: jobject]: Integer read GetLeft write SetLeft;
    property Top[Rect: jobject]: Integer read GetTop write SetTop;
    property Right[Rect: jobject]: Integer read GetRight write SetRight;
    property Bottom[Rect: jobject]: Integer read GetBottom write SetBottom;
  end;

  TJWindow = class(TCustomJavaClass)
  public const
    FLAG_KEEP_SCREEN_ON = 128;
    FLAG_FULLSCREEN = 1024;
    FEATURE_NO_TITLE = 1;
  private
    javaMethod_getDecorView: jmethodid;
    javaMethod_addFlags: jmethodid;
    javaMethod_clearFlags: jmethodid;
    javaMethod_requestFeature: jmethodid;
  protected
    function GetJavaClassLink: AnsiString; override;
    procedure LoadJavaContent; override;
  public
    function GetDecorView(Window: jobject): jobject;
    procedure AddFlags(Window: jobject; Flags: Integer);
    procedure ClearFlags(Window: jobject; Flags: Integer);
    function RequestFeature(Window: jobject; FeatureID: Integer): Boolean;
  end;

  TJDisplayMetrics = class(TCustomJavaClass)
  private
    javaMethod_init: jmethodID;
    javaField_density: jfieldID;
    javaField_scaledDensity: jfieldID;

    function GetDensity(DisplayMetrics: jobject): Single;
    function GetScaledDensity(DisplayMetrics: jobject): Single;
  protected
    function GetJavaClassLink: AnsiString; override;
    procedure LoadJavaContent; override;
  public
    function New: jobject;

    property Density[DisplayMetrics: jobject]: Single read GetDensity;
    property ScaledDensity[DisplayMetrics: jobject]: Single read GetScaledDensity;
  end;

  TJDisplay = class(TCustomJavaClass)
  private
    javaMethod_getMetrics: jmethodID;
    javaMethod_getRotation: jmethodID;
  protected
    function GetJavaClassLink: AnsiString; override;
    procedure LoadJavaContent; override;
  public
    procedure GetMetrics(Display, Metrics: jobject);
    function GetRotation(Display: jobject): Integer;
  end;

  TJWindowManager = class(TCustomJavaClass)
  private
    javaMethod_getDefaultDisplay: jmethodID;
  protected
    function GetJavaClassLink: AnsiString; override;
    procedure LoadJavaContent; override;
  public
    function GetDefaultDisplay(WindowManager: jobject): jobject;
  end;

  TJActivity = class(TCustomJavaClass)
  private
    javaMethod_getWindow: jmethodID;
    javaMethod_getWindowManager: jmethodID;
    javaMethod_getApplicationInfo: jmethodID;
    javaMethod_requestWindowFeature: jmethodID;
  protected
    function GetJavaClassLink: AnsiString; override;
    procedure LoadJavaContent; override;
  public
    function GetWindow(Activity: jobject): jobject;
    function GetWindowManager(Activity: jobject): jobject;
    function GetApplicationInfo(Activity: jobject): jobject;
    function RequestWindowFeature(Activity: jobject; FeatureID: Integer): Boolean;
  end;

  TJView = class(TCustomJavaClass)
  private
    javaMethod_getDrawingRect: jmethodid;
    javaMethod_getWindowVisibleDisplayFrame: jmethodid;
  protected
    function GetJavaClassLink: AnsiString; override;
    procedure LoadJavaContent; override;
  public
    procedure GetDrawingRect(View, Rect: jobject);
    procedure GetWindowVisibleDisplayFrame(View, Rect: jobject);
  end;

  TJSystem = class(TCustomJavaClass)
  private
    javaStaticMethod_loadLibrary: jmethodid;
  protected
    function GetJavaClassLink: AnsiString; override;
    procedure LoadJavaContent; override;
  public
    procedure LoadLibrary(const LibName: AnsiString);
  end;

  TJApplicationInfo = class(TCustomJavaClass)
  private
    javaField_nativeLibraryDir: jfieldID;
    function GetNativeLibraryDir(ApplicationInfo: jobject): AnsiString;
  protected
    function GetJavaClassLink: AnsiString; override;
    procedure LoadJavaContent; override;
  public
    property NativeLibraryDir[ApplicationInfo: jobject]: AnsiString read GetNativeLibraryDir;
  end;

implementation

uses
  PXL.Logs;

const
  JniPrefix = '(JNI) ';

constructor TJavaSession.Create(AAndroidApp: PAndroid_app);
begin
  inherited Create;

  FAndroidApp := AAndroidApp;

  if (FAndroidApp <> nil) and (FAndroidApp.activity <> nil) and (FAndroidApp.activity.vm <> nil) then
    AndroidApp.activity.vm^.AttachCurrentThread(AndroidApp.activity.vm, @FEnv, nil);
end;

destructor TJavaSession.Destroy;
begin
  if (FAndroidApp <> nil) and (FAndroidApp.activity <> nil) and (FAndroidApp.activity.vm <> nil) and (FEnv <> nil) then
  begin
    AndroidApp.activity.vm^.DetachCurrentThread(AndroidApp.activity.vm);
    FEnv := nil;
  end;

  FAndroidApp := nil;

  inherited;
end;

procedure TJavaSession.Release(var JavaObj: jobject);
begin
  if (FEnv <> nil) and (JavaObj <> nil) then
  begin
    FEnv^.DeleteLocalRef(FEnv, JavaObj);
    JavaObj := nil;
  end
  else
    JavaObj := nil;
end;

constructor TCustomJavaClass.Create(ASession: TJavaSession);
begin
  inherited Create;

  FSession := ASession;

  if SessionValid then
  begin
    LoadJavaClass;
    LoadJavaContent;
  end;
end;

destructor TCustomJavaClass.Destroy;
begin
  if SessionValid then
  begin
    ReleaseFieldIDs;
    ReleaseMethodIDs;

    if FJavaClass <> nil then
    begin
      FSession.Env^.DeleteLocalRef(FSession.Env, FJavaClass);
      FJavaClass := nil;
    end;

    FSession := nil;
  end;

  inherited;
end;

function TCustomJavaClass.SessionValid: Boolean;
begin
  Result := (FSession <> nil) and (FSession.Env <> nil);
end;

procedure TCustomJavaClass.LoadJavaClass;
var
  ClassLink: AnsiString;
begin
  ClassLink := GetJavaClassLink;
  if Length(ClassLink) < 1 then
  begin
    LogText(JniPrefix + 'Class link not specified for "' + ClassName + '".', TLogType.Error);
    Exit;
  end;

  FJavaClass := FSession.Env^.FindClass(FSession.Env, PAnsiChar(ClassLink));
  if FJavaClass = nil then
  begin
    LogText(JniPrefix + 'Could not locate java class "' + ClassLink + '" for class "' + ClassName + '".',
      TLogType.Error);
    Exit;
  end;

{$IFDEF JNI_DEBUG}
  LogText(JniPrefix + 'Loaded java class "' + ClassLink + '" for class "' + ClassName + '".');
{$ENDIF}
end;

procedure TCustomJavaClass.AddMethodID(MethodID: jmethodID);
var
  Index: Integer;
begin
  Index := Length(MethodIDs);
  SetLength(MethodIDs, Index + 1);

  MethodIDs[Index] := MethodID;
end;

procedure TCustomJavaClass.ReleaseMethodIDs;
var
  I: Integer;
begin
  for I := Length(MethodIDs) - 1 downto 0 do
    if MethodIDs[I] <> nil then
    begin
      FSession.Env^.DeleteLocalRef(FSession.Env, MethodIDs[I]);
      MethodIDs[I] := nil;
    end;

  SetLength(MethodIDs, 0);
end;

procedure TCustomJavaClass.AddFieldID(FieldID: jfieldID);
var
  Index: Integer;
begin
  Index := Length(FieldIDs);
  SetLength(FieldIDs, Index + 1);

  FieldIDs[Index] := FieldID;
end;

procedure TCustomJavaClass.ReleaseFieldIDs;
var
  I: Integer;
begin
  for I := Length(FieldIDs) - 1 downto 0 do
    if FieldIDs[I] <> nil then
    begin
      FSession.Env^.DeleteLocalRef(FSession.Env, FieldIDs[I]);
      FieldIDs[I] := nil;
    end;

  SetLength(FieldIDs, 0);
end;

function TCustomJavaClass.GetJavaMethod(const MethodName, Signature: AnsiString): jmethodID;
begin
  Result := FSession.Env^.GetMethodID(FSession.Env, FJavaClass, PAnsiChar(MethodName), PAnsiChar(Signature));
  if Result = nil then
    LogText(JniPrefix + 'Could not get ID for method "' + MethodName + '" in class "' + ClassName + '".',
      TLogType.Error);

{$IFDEF JNI_DEBUG}
  if Result <> nil then
    LogText(JniPrefix + 'Loaded ID for method "' + MethodName + '" in class "' + ClassName + '".');
{$ENDIF}

{  if Result <> nil then
    AddMethodID(Result);}
end;

function TCustomJavaClass.GetJavaStaticMethod(const MethodName, Signature: AnsiString): jmethodID;
begin
  Result := FSession.Env^.GetStaticMethodID(FSession.Env, FJavaClass, PAnsiChar(MethodName), PAnsiChar(Signature));
  if Result = nil then
    LogText(JniPrefix + 'Could not get ID for static method "' + MethodName + '" in class "' + ClassName + '".',
      TLogType.Error);

{$IFDEF JNI_DEBUG}
  if Result <> nil then
    LogText(JniPrefix + 'Loaded ID for static method "' + MethodName + '" in class "' + ClassName + '".');
{$ENDIF}
end;

function TCustomJavaClass.GetJavaField(const FieldName, Signature: AnsiString): jfieldID;
begin
  Result := FSession.Env^.GetFieldID(FSession.Env, FJavaClass, PAnsiChar(FieldName), PAnsiChar(Signature));
  if Result = nil then
    LogText(JniPrefix + 'Could not get ID for field "' + FieldName + '" in class "' + ClassName + '".',
      TLogType.Error);

{$IFDEF JNI_DEBUG}
  if Result <> nil then
    LogText(JniPrefix + 'Loaded ID for field "' + FieldName + '" in class "' + ClassName + '".');
{$ENDIF}

{  if Result <> nil then
    AddFieldID(Result);}
end;

function TCustomJavaClass.JavaNewObject(InitID: jmethodID): jobject;
begin
  Result := FSession.Env^.NewObject(FSession.Env, JavaClass, InitID);
  if Result = nil then
    LogText(JniPrefix + 'Could not create new object for class "' + ClassName + '".',
      TLogType.Error);

{$IFDEF JNI_DEBUG}
  if Result <> nil then
    LogText(JniPrefix + 'Created new object for class "' + ClassName + '".');
{$ENDIF}
end;

function TCustomJavaClass.GetIntField(JavaObj: jobject; FieldID: jfieldID): Integer;
begin
  Result := FSession.Env^.GetIntField(FSession.Env, JavaObj, FieldID);
end;

procedure TCustomJavaClass.SetIntField(JavaObj: jobject; FieldID: jfieldID; Value: Integer);
begin
  FSession.Env^.SetIntField(FSession.Env, JavaObj, FieldID, Value);
end;

function TCustomJavaClass.GetFloatField(JavaObj: jobject; FieldID: jfieldID): Single;
begin
  Result := FSession.Env^.GetFloatField(FSession.Env, JavaObj, FieldID);
end;

procedure TCustomJavaClass.SetFloatField(JavaObj: jobject; FieldID: jfieldID; Value: Single);
begin
  FSession.Env^.SetFloatField(FSession.Env, JavaObj, FieldID, Value);
end;

function TJRect.GetJavaClassLink: AnsiString;
begin
  Result := 'android/graphics/Rect';
end;

procedure TJRect.LoadJavaContent;
begin
  javaMethod_init := GetJavaMethod('<init>', '()V');
  javaField_left := GetJavaField('left', 'I');
  javaField_top := GetJavaField('top', 'I');
  javaField_right := GetJavaField('right', 'I');
  javaField_bottom := GetJavaField('bottom', 'I');
end;

function TJRect.New: jobject;
begin
  Result := JavaNewObject(javaMethod_init);
end;

function TJRect.GetLeft(Rect: jobject): Integer;
begin
  Result := GetIntField(Rect, javaField_left);
end;

procedure TJRect.SetLeft(Rect: jobject; Value: Integer);
begin
  SetIntField(Rect, javaField_left, Value);
end;

function TJRect.GetTop(Rect: jobject): Integer;
begin
  Result := GetIntField(Rect, javaField_top);
end;

procedure TJRect.SetTop(Rect: jobject; Value: Integer);
begin
  SetIntField(Rect, javaField_top, Value);
end;

function TJRect.GetRight(Rect: jobject): Integer;
begin
  Result := GetIntField(Rect, javaField_right);
end;

procedure TJRect.SetRight(Rect: jobject; Value: Integer);
begin
  SetIntField(Rect, javaField_right, Value);
end;

function TJRect.GetBottom(Rect: jobject): Integer;
begin
  Result := GetIntField(Rect, javaField_bottom);
end;

procedure TJRect.SetBottom(Rect: jobject; Value: Integer);
begin
  SetIntField(Rect, javaField_bottom, Value);
end;

function TJWindow.GetJavaClassLink: AnsiString;
begin
  Result := 'android/view/Window';
end;

procedure TJWindow.LoadJavaContent;
begin
  javaMethod_getDecorView := GetJavaMethod('getDecorView', '()Landroid/view/View;');
  javaMethod_addFlags := GetJavaMethod('addFlags', '(I)V');
  javaMethod_clearFlags := GetJavaMethod('clearFlags', '(I)V');
  javaMethod_requestFeature := GetJavaMethod('requestFeature', '(I)Z');
end;

function TJWindow.GetDecorView(Window: jobject): jobject;
begin
  Result := Session.Env^.CallObjectMethod(Session.Env, Window, javaMethod_getDecorView);
end;

procedure TJWindow.AddFlags(Window: jobject; Flags: Integer);
var
  javaArg: jvalue;
begin
  javaArg.i := Flags;
  Session.Env^.CallVoidMethodA(Session.Env, Window, javaMethod_addFlags, @javaArg);
end;

procedure TJWindow.ClearFlags(Window: jobject; Flags: Integer);
var
  javaArg: jvalue;
begin
  javaArg.i := Flags;
  Session.Env^.CallVoidMethodA(Session.Env, Window, javaMethod_clearFlags, @javaArg);
end;

//--------------------------------------------------------------------------------------------------------------------
function TJWindow.RequestFeature(Window: jobject; FeatureID: Integer): Boolean;
var
  javaArg: jvalue;
begin
  javaArg.i := FeatureID;
  Result := Session.Env^.CallBooleanMethodA(Session.Env, Window, javaMethod_requestFeature, @javaArg) <> 0;
end;

function TJDisplayMetrics.GetJavaClassLink: AnsiString;
begin
  Result := 'android/util/DisplayMetrics';
end;

procedure TJDisplayMetrics.LoadJavaContent;
begin
  javaMethod_init := GetJavaMethod('<init>', '()V');
  javaField_density := GetJavaField('density', 'F');
  javaField_scaledDensity := GetJavaField('scaledDensity', 'F');
end;

function TJDisplayMetrics.New: jobject;
begin
  Result := JavaNewObject(javaMethod_init);
end;

function TJDisplayMetrics.GetDensity(DisplayMetrics: jobject): Single;
begin
  Result := GetFloatField(DisplayMetrics, javaField_density);
end;

function TJDisplayMetrics.GetScaledDensity(DisplayMetrics: jobject): Single;
begin
  Result := GetFloatField(DisplayMetrics, javaField_scaledDensity);
end;

function TJDisplay.GetJavaClassLink: AnsiString;
begin
  Result := 'android/view/Display';
end;

procedure TJDisplay.LoadJavaContent;
begin
  javaMethod_getMetrics := GetJavaMethod('getMetrics', '(Landroid/util/DisplayMetrics;)V');
  javaMethod_getRotation := GetJavaMethod('getRotation', '()I');
end;

procedure TJDisplay.GetMetrics(Display, Metrics: jobject);
var
  javaArg: jvalue;
begin
  javaArg.l := Metrics;
  Session.Env^.CallVoidMethodA(Session.Env, Display, javaMethod_getMetrics, @javaArg);
end;

function TJDisplay.GetRotation(Display: jobject): Integer;
begin
  Result := Session.Env^.CallIntMethod(Session.Env, Display, javaMethod_getRotation);
end;

function TJWindowManager.GetJavaClassLink: AnsiString;
begin
  Result := 'android/view/WindowManager';
end;

procedure TJWindowManager.LoadJavaContent;
begin
  javaMethod_getDefaultDisplay := GetJavaMethod('getDefaultDisplay', '()Landroid/view/Display;');
end;

function TJWindowManager.GetDefaultDisplay(WindowManager: jobject): jobject;
begin
  Result := Session.Env^.CallObjectMethod(Session.Env, WindowManager, javaMethod_getDefaultDisplay);
end;

function TJActivity.GetJavaClassLink: AnsiString;
begin
  Result := 'android/app/Activity';
end;

procedure TJActivity.LoadJavaContent;
begin
  javaMethod_getWindow := GetJavaMethod('getWindow', '()Landroid/view/Window;');
  javaMethod_getWindowManager := GetJavaMethod('getWindowManager', '()Landroid/view/WindowManager;');
  javaMethod_getApplicationInfo := GetJavaMethod('getApplicationInfo', '()Landroid/content/pm/ApplicationInfo;');
  javaMethod_requestWindowFeature := GetJavaMethod('requestWindowFeature', '(I)Z');
end;

function TJActivity.GetWindow(Activity: jobject): jobject;
begin
  Result := Session.Env^.CallObjectMethod(Session.Env, Activity, javaMethod_getWindow);
end;

//--------------------------------------------------------------------------------------------------------------------
function TJActivity.GetWindowManager(Activity: jobject): jobject;
begin
  Result := Session.Env^.CallObjectMethod(Session.Env, Activity, javaMethod_getWindowManager);
end;

//--------------------------------------------------------------------------------------------------------------------
function TJActivity.GetApplicationInfo(Activity: jobject): jobject;
begin
  Result := Session.Env^.CallObjectMethod(Session.Env, Activity, javaMethod_getApplicationInfo);
end;

function TJActivity.RequestWindowFeature(Activity: jobject; FeatureID: Integer): Boolean;
var
  javaArg: jvalue;
begin
  javaArg.i := FeatureID;
  Result := Session.Env^.CallBooleanMethodA(Session.Env, Activity, javaMethod_requestWindowFeature, @javaArg) <> 0;
end;

function TJView.GetJavaClassLink: AnsiString;
begin
  Result := 'android/view/View';
end;

procedure TJView.LoadJavaContent;
begin
  javaMethod_getDrawingRect := GetJavaMethod('getDrawingRect', '(Landroid/graphics/Rect;)V');
  javaMethod_getWindowVisibleDisplayFrame := GetJavaMethod('getWindowVisibleDisplayFrame', '(Landroid/graphics/Rect;)V');
end;

procedure TJView.GetDrawingRect(View, Rect: jobject);
var
  javaArg: jvalue;
begin
  javaArg.l := Rect;
  Session.Env^.CallVoidMethodA(Session.Env, View, javaMethod_getDrawingRect, @javaArg);
end;

procedure TJView.GetWindowVisibleDisplayFrame(View, Rect: jobject);
var
  javaArg: jvalue;
begin
  javaArg.l := Rect;
  Session.Env^.CallVoidMethodA(Session.Env, View, javaMethod_getWindowVisibleDisplayFrame, @javaArg);
end;

function TJSystem.GetJavaClassLink: AnsiString;
begin
  Result := 'java/lang/System';
end;

//--------------------------------------------------------------------------------------------------------------------
procedure TJSystem.LoadJavaContent;
begin
  javaStaticMethod_loadLibrary := GetJavaStaticMethod('loadLibrary', '(Ljava/lang/String;)V');
end;

procedure TJSystem.LoadLibrary(const LibName: AnsiString);
var
  lParams: array[0..2] of JValue;
begin
  lParams[0].l := Session.Env^.NewStringUTF(Session.Env, PAnsiChar(LibName));
  Session.Env^.CallStaticIntMethodA(Session.Env, JavaClass, javaStaticMethod_loadLibrary, @lParams[0]);
  Session.Env^.DeleteLocalRef(Session.Env, lParams[0].l);
end;

function TJApplicationInfo.GetJavaClassLink: AnsiString;
begin
  Result := 'android/content/pm/ApplicationInfo';
end;

procedure TJApplicationInfo.LoadJavaContent;
begin
  javaField_nativeLibraryDir := GetJavaField('nativeLibraryDir', 'Ljava/lang/String;');
end;

function TJApplicationInfo.GetNativeLibraryDir(ApplicationInfo: jobject): AnsiString;
var
  IsCopy: jboolean;
  Temp: jstring;
  Res: PAnsiChar;
begin
  Temp := Session.Env^.GetObjectField(Session.Env, ApplicationInfo, javaField_nativeLibraryDir);

  Res := Session.Env^.GetStringUTFChars(Session.Env, Temp, @IsCopy);
  Result := Res;

  Session.Env^.ReleaseStringUTFChars(Session.Env, Temp, Res);
end;

end.

