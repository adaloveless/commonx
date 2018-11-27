{
  Copyright (C) 2010 The Android Open Source Project

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
   http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed
  on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for
  the specific language governing permissions and limitations under the License.

  Android NDK C/C++ file:
    android_native_app_glue.h
    android_native_app_glue.c

  Original source code was taken from:
    %NDK_DIR%/sources/android/native_app_glue

  Pascal translation by Yuriy Kotsarenko, August 2015.
}
unit Android.AppGlue;

interface

{$INCLUDE Android.Config.inc}

uses
  unixtype, Android.Input, Android.NativeActivity, Android.Configuration, Android.Looper, Android.NativeWindow,
  Android.Rect;

{$INCLUDE Android.LibDefs.inc}

{
  The native activity interface provided by <android/native_activity.h> is based on a set of application-provided
  callbacks that will be called by the Activity's main thread when certain events occur.

  This means that each one of this callbacks _should_ _not_ block, or they risk having the system force-close the
  application. This programming model is direct, lightweight, but constraining.

  The 'threaded_native_app' static library is used to provide a different execution model where the application can
  implement its own main event loop in a different thread instead. Here's how it works:

  1/ The application must provide a function named "android_main()" that will be called when the activity is created,
     in a new thread that is distinct from the activity's main thread.

  2/ android_main() receives a pointer to a valid "android_app" structure that contains references to other important
     objects, e.g. the ANativeActivity obejct instance the application is running in.

  3/ the "android_app" object holds an ALooper instance that already listens to two important things:

       - activity lifecycle events (e.g. "pause", "resume"). See APP_CMD_XXX declarations below.

       - input events coming from the AInputQueue attached to the activity.

     Each of these correspond to an ALooper identifier returned by ALooper_pollOnce with values of LOOPER_ID_MAIN and
     LOOPER_ID_INPUT, respectively.

     Your application can use the same ALooper to listen to additional file-descriptors. They can either be callback
     based, or with return identifiers starting with LOOPER_ID_USER.

  4/ Whenever you receive a LOOPER_ID_MAIN or LOOPER_ID_INPUT event, the returned data will point to an
     android_poll_source structure. You can call the process() function on it, and fill in android_app->onAppCmd and
     android_app->onInputEvent to be called for your own processing of the event.

     Alternatively, you can call the low-level functions to read and process the data directly... look at the
     process_cmd() and process_input() implementations in the glue to see how to do this.

  See the sample named "native-activity" that comes with the NDK with a full usage example.
  Also look at the JavaDoc of NativeActivity.
}
type
  Pandroid_app = ^android_app;

  Pandroid_poll_source = ^android_poll_source;

  android_poll_source = record
    { The identifier of this source. May be LOOPER_ID_MAIN or LOOPER_ID_INPUT. }
    id: LongInt;

    { The android_app this ident is associated with. }
    app: Pandroid_app;

    { Function to call to perform the standard processing of data from this source. }
    process: procedure(app: Pandroid_app; Source: Pandroid_poll_source); cdecl;
  end;

{
  This is the interface for the standard glue code of a threaded application. In this model, the application's code is
  running in its own thread separate from the main thread of the process. It is not required that this thread be
  associated with the Java VM, although it will need to be in order to make JNI calls any Java objects.
}
  android_app = record
    { The application can place a pointer to its own state object here if it likes. }
    userData: Pointer;

    { Fill this in with the function to process main app commands (APP_CMD_*) }
    onAppCmd: procedure(app: Pandroid_app; cmd: LongInt); cdecl;

    { Fill this in with the function to process input events. At this point the event has already been
      pre-dispatched, and it will be finished upon return. Return 1 if you have handled the event, 0 for any default
      dispatching. }
    onInputEvent: function(app: PAndroid_app; event: PAInputEvent): LongInt; cdecl;

    { The ANativeActivity object instance that this app is running in. }
    activity: PANativeActivity;

    { The current configuration the app is running in. }
    config: PAConfiguration;

    { This is the last instance's saved state, as provided at creation time. It is NULL if there was no state.
      You can use this as you need; the memory will remain around until you call android_app_exec_cmd() for
      APP_CMD_RESUME, at which point it will be freed and savedState set to NULL. These variables should only be
      changed when processing a APP_CMD_SAVE_STATE, at which point they will be initialized to NULL and you can
      malloc your state and place the information here. In that case the memory will be freed for you later. }
    savedState: Pointer;
    savedStateSize: size_t;

    { The ALooper associated with the app's thread. }
    looper: PALooper;

    { When non-NULL, this is the input queue from which the app will receive user input events. }
    inputQueue: PAInputQueue;

    { When non-NULL, this is the window surface that the app can draw in. }
    window: PANativeWindow;

    { Current content rectangle of the window; this is the area where the window's content should be placed to be
      seen by the user. }
    contentRect: ARect;

    { Current state of the app's activity.  May be either APP_CMD_START, APP_CMD_RESUME, APP_CMD_PAUSE, or
      APP_CMD_STOP; see below. }
    activityState: LongInt;

    { This is non-zero when the application's NativeActivity is being destroyed and waiting for the app thread to
      complete. }
    destroyRequested: LongInt;

    { Below are "private" implementation of the glue code. }
    mutex: pthread_mutex_t;
    cond: pthread_cond_t;

    msgread: LongInt;
    msgwrite: LongInt;

    thread: pthread_t;

    cmdPollSource: android_poll_source;
    inputPollSource: android_poll_source;

    running: LongInt;
    stateSaved: LongInt;
    destroyed: LongInt;
    redrawNeeded: LongInt;
    pendingInputQueue: PAInputQueue;
    pendingWindow: PANativeWindow;
    pendingContentRect: ARect;
  end;

const
  { Looper data ID of commands coming from the app's main thread, which is returned as an identifier from
    ALooper_pollOnce(). The data for this identifier is a pointer to an android_poll_source structure.
    These can be retrieved and processed with android_app_read_cmd() and android_app_exec_cmd(). }
  LOOPER_ID_MAIN = 1;

  { Looper data ID of events coming from the AInputQueue of the application's window, which is returned as an
    identifier from ALooper_pollOnce(). The data for this identifier is a pointer to an android_poll_source structure.
    These can be read via the inputQueue object of android_app. }
  LOOPER_ID_INPUT = 2;

  { Start of user-defined ALooper identifiers. }
  LOOPER_ID_USER = 3;

  { Command from main thread: the AInputQueue has changed. Upon processing this command, android_app->inputQueue will
    be updated to the new queue (or NULL).  }
  APP_CMD_INPUT_CHANGED = 0;

  { Command from main thread: a new ANativeWindow is ready for use. Upon receiving this command, android_app->window
    will contain the new window surface. }
  APP_CMD_INIT_WINDOW = 1;

  { Command from main thread: the existing ANativeWindow needs to be terminated. Upon receiving this command,
    android_app->window still contains the existing window; after calling android_app_exec_cmd it will be set to NULL. }
  APP_CMD_TERM_WINDOW = 2;

  { Command from main thread: the current ANativeWindow has been resized.
    Please redraw with its new size. }
  APP_CMD_WINDOW_RESIZED = 3;

  { Command from main thread: the system needs that the current ANativeWindow be redrawn. You should redraw the window
    before handing this to android_app_exec_cmd() in order to avoid transient drawing glitches. }
  APP_CMD_WINDOW_REDRAW_NEEDED = 4;

  { Command from main thread: the content area of the window has changed, such as from the soft input window being
    shown or hidden. You can find the new content rect in android_app::contentRect. }
  APP_CMD_CONTENT_RECT_CHANGED = 5;

  { Command from main thread: the app's activity window has gained input focus. }
  APP_CMD_GAINED_FOCUS = 6;

  { Command from main thread: the app's activity window has lost input focus. }
  APP_CMD_LOST_FOCUS = 7;

  { Command from main thread: the current device configuration has changed. }
  APP_CMD_CONFIG_CHANGED = 8;

  { Command from main thread: the system is running low on memory. Try to reduce your memory use. }
  APP_CMD_LOW_MEMORY = 9;

  { Command from main thread: the app's activity has been started. }
  APP_CMD_START = 10;

  { Command from main thread: the app's activity has been resumed. }
  APP_CMD_RESUME = 11;

  { Command from main thread: the app should generate a new saved state for itself, to restore from later if needed.
    If you have saved state, allocate it with malloc and place it in android_app.savedState with the size in
    android_app.savedStateSize. The will be freed for you later. }
  APP_CMD_SAVE_STATE = 12;

  { Command from main thread: the app's activity has been paused. }
  APP_CMD_PAUSE = 13;

  { Command from main thread: the app's activity has been stopped. }
  APP_CMD_STOP = 14;

  { Command from main thread: the app's activity is being destroyed, and waiting for the app thread to clean up and
    exit before proceeding. }
  APP_CMD_DESTROY = 15;

{
  Call when ALooper_pollAll() returns LOOPER_ID_MAIN, reading the next app command message.
}
function android_app_read_cmd(android_app: Pandroid_app): ShortInt; cdecl;

{
  Call with the command returned by android_app_read_cmd() to do the initial pre-processing of the given command.
  You can perform your own actions for the command after calling this function.
}
procedure android_app_pre_exec_cmd(android_app: Pandroid_app; cmd: ShortInt); cdecl;

{
  Call with the command returned by android_app_read_cmd() to do the final post-processing of the given command.
  You must have done your own actions for the command before calling this function.
}
procedure android_app_post_exec_cmd(android_app: Pandroid_app; cmd: ShortInt); cdecl;

{
  Dummy function you can call to ensure glue code isn't stripped.
}
procedure app_dummy;

{
  This is the function that application code must implement, representing the main entry to the app.
}
var
  android_main: procedure(app: Pandroid_app);

procedure ANativeActivity_onCreate(activity: PANativeActivity; savedState: Pointer; savedStateSize: size_t); cdecl;

implementation

uses
  SysUtils, Android.PThread, Android.Log;

const
  LOG_TAG = 'PXL';

{$REGION 'LIBC Functions'}

function libc_read(handle: LongInt; buf: Pointer; count: size_t): ssize_t; cdecl;
  external libc name 'read';

function libc_write(handle: LongInt; buf: Pointer; count: size_t): ssize_t; cdecl;
  external libc name 'write';

function libc_close(desc: LongInt): LongInt; cdecl;
  external libc name 'close';

function libc_malloc(size: size_t): Pointer; cdecl;
  external libc name 'malloc';

procedure libc_free(data: Pointer); cdecl;
  external libc name 'free';

function pipe(pipe_des: PLongInt): LongInt; cdecl;
  external libc name 'pipe';

procedure LOGE(const Text: AnsiString);
begin
  __android_log_write(ANDROID_LOG_ERROR, LOG_TAG, PAnsiChar(Text));
end;

{$IFDEF APPGLUE_DEBUG}
procedure LOGV(const Text: AnsiString);
begin
  __android_log_write(ANDROID_LOG_VERBOSE, LOG_TAG, PAnsiChar(Text));
end;
{$ENDIF}

{$ENDREGION}
{$REGION 'AppGlue functions (Main thread)'}

procedure free_saved_state(android_app: Pandroid_app); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: free_saved_state');
{$ENDIF}

  pthread_mutex_lock(@android_app.mutex);
  if android_app.savedState <> nil then
  begin
    libc_free(android_app.savedState);
    android_app.savedState := nil;
    android_app.savedStateSize := 0;
  end;
  pthread_mutex_unlock(@android_app.mutex);
end;

function android_app_read_cmd(android_app: Pandroid_app): ShortInt; cdecl;
var
  cmd: ShortInt;
begin
  if libc_read(android_app.msgread, @cmd, SizeOf(cmd)) = SizeOf(cmd) then
  begin
    case cmd of
      APP_CMD_SAVE_STATE:
        free_saved_state(android_app);
    end;

    Exit(cmd);
  end
  else
    LOGE('No data on command pipe!');

  Result := -1;
end;

procedure android_app_pre_exec_cmd(android_app: Pandroid_app; cmd: ShortInt); cdecl;
begin
  case cmd of
    APP_CMD_INPUT_CHANGED:
    begin
      pthread_mutex_lock(@android_app.mutex);
      if android_app.inputQueue <> nil then
        AInputQueue_detachLooper(android_app.inputQueue);
      android_app.inputQueue := android_app.pendingInputQueue;
      if android_app.inputQueue <> nil then
        AInputQueue_attachLooper(android_app.inputQueue, android_app.looper, LOOPER_ID_INPUT,
          nil, @android_app.inputPollSource);
      pthread_cond_broadcast(@android_app.cond);
      pthread_mutex_unlock(@android_app.mutex);
    end;

    APP_CMD_INIT_WINDOW:
    begin
      pthread_mutex_lock(@android_app.mutex);
      android_app.window := android_app.pendingWindow;
      pthread_cond_broadcast(@android_app.cond);
      pthread_mutex_unlock(@android_app.mutex);
    end;

    APP_CMD_TERM_WINDOW:
      pthread_cond_broadcast(@android_app.cond);

    APP_CMD_RESUME,
    APP_CMD_START,
    APP_CMD_PAUSE,
    APP_CMD_STOP:
    begin
      pthread_mutex_lock(@android_app.mutex);
      android_app.activityState := cmd;
      pthread_cond_broadcast(@android_app.cond);
      pthread_mutex_unlock(@android_app.mutex);
    end;

    APP_CMD_CONFIG_CHANGED:
      AConfiguration_fromAssetManager(android_app^.config, android_app^.activity^.assetManager);

    APP_CMD_DESTROY:
      android_app^.destroyRequested := 1;
  end;
end;

procedure android_app_post_exec_cmd(android_app: Pandroid_app; cmd: ShortInt); cdecl;
begin
  case cmd of
    APP_CMD_TERM_WINDOW:
    begin
      pthread_mutex_lock(@android_app.mutex);
      android_app.window := nil;
      pthread_cond_broadcast(@android_app.cond);
      pthread_mutex_unlock(@android_app.mutex);
    end;

    APP_CMD_SAVE_STATE:
    begin
      pthread_mutex_lock(@android_app.mutex);
      android_app.stateSaved := 1;
      pthread_cond_broadcast(@android_app.cond);
      pthread_mutex_unlock(@android_app.mutex);
    end;

    APP_CMD_RESUME:
      free_saved_state(android_app);
  end;
end;

procedure app_dummy;
begin
end;

procedure android_app_destroy(android_app: Pandroid_app); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: android_app_destroy');
{$ENDIF}

  free_saved_state(android_app);
  pthread_mutex_lock(@android_app.mutex);
  if android_app.inputQueue <> nil then
    AInputQueue_detachLooper(android_app.inputQueue);
  AConfiguration_delete(android_app.config);
  android_app.destroyed := 1;
  pthread_cond_broadcast(@android_app.cond);
  pthread_mutex_unlock(@android_app.mutex);
  // Can't touch android_app object after this.
end;

procedure process_input(app: Pandroid_app; Source: Pandroid_poll_source); cdecl;
var
  event: PAInputEvent;
  processed: LongInt;
  handled: Int32;
begin
  event := nil;
  processed := 0;

  while AInputQueue_getEvent(app.inputQueue, @event) >= 0 do
  begin
    if AInputQueue_preDispatchEvent(app.inputQueue, event) <> 0 then
      Continue;

    handled := 0;

    if Assigned(app.onInputEvent) then
      handled := app.onInputEvent(app, event);

    AInputQueue_finishEvent(app.inputQueue, event, handled);
    processed := 1;
  end;

  if processed = 0 then
    LOGE('Failure reading next input event');
end;

procedure process_cmd(app: Pandroid_app; Source: Pandroid_poll_source); cdecl;
var
  cmd: ShortInt;
begin
  cmd := android_app_read_cmd(app);
  android_app_pre_exec_cmd(app, cmd);
  if Assigned(app.onAppCmd) then
    app.onAppCmd(app, cmd);
  android_app_post_exec_cmd(app, cmd);
end;

function android_app_entry(param: Pointer): Pointer; cdecl;
var
  android_app: Pandroid_app;
  looper: PALooper;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: android_app_entry');
{$ENDIF}

  android_app := Pandroid_app(param);

  android_app.config := AConfiguration_new;
  AConfiguration_fromAssetManager(android_app.config, android_app.activity.assetManager);

  android_app.cmdPollSource.id := LOOPER_ID_MAIN;
  android_app.cmdPollSource.app := android_app;
  android_app.cmdPollSource.process := @process_cmd;
  android_app.inputPollSource.id := LOOPER_ID_INPUT;
  android_app.inputPollSource.app := android_app;
  android_app.inputPollSource.process := @process_input;

  looper := ALooper_prepare(ALOOPER_PREPARE_ALLOW_NON_CALLBACKS);
  ALooper_addFd(looper, android_app.msgread, LOOPER_ID_MAIN, ALOOPER_EVENT_INPUT, nil, @android_app.cmdPollSource);
  android_app.looper := looper;

  pthread_mutex_lock(@android_app.mutex);
  android_app.running := 1;
  pthread_cond_broadcast(@android_app.cond);
  pthread_mutex_unlock(@android_app.mutex);

  android_main(android_app);

  android_app_destroy(android_app);

  Result := nil;
end;

{$ENDREGION}
{$REGION 'AppGlue functions (Java thread)'}

function android_app_create(activity: PANativeActivity; savedState: Pointer; savedStateSize: size_t): Pandroid_app; cdecl;
var
  _android_app: Pandroid_app;
  msgpipe: array[0..1] of LongInt;
  attr: pthread_attr_t;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: android_app_create');
{$ENDIF}

  _android_app := Pandroid_app(libc_malloc(SizeOf(android_app)));

  FillChar(_android_app^, SizeOf(android_app), 0);
  _android_app.activity := activity;

  pthread_mutex_init(@_android_app.mutex, nil);
  pthread_cond_init(@_android_app.cond, nil);

  if savedState <> nil then
  begin
    _android_app.savedState := libc_malloc(savedStateSize);
    _android_app.savedStateSize := savedStateSize;
    Move(savedState^, _android_app.savedState^, savedStateSize);
  end;

  if pipe(@msgpipe[0]) <> 0 then
  begin
    LOGE('could not create pipe');
    Exit(nil);
  end;

  _android_app.msgread := msgpipe[0];
  _android_app.msgwrite := msgpipe[1];

  pthread_attr_init(@attr);
  pthread_attr_setdetachstate(@attr, PTHREAD_CREATE_DETACHED);

  pthread_create(@_android_app.thread, @attr, @android_app_entry, _android_app);

  // Wait for thread to start.
  pthread_mutex_lock(@_android_app.mutex);

  while _android_app.running = 0 do
    pthread_cond_wait(@_android_app.cond, @_android_app.mutex);

  pthread_mutex_unlock(@_android_app.mutex);

  Result := _android_app;
end;

procedure android_app_write_cmd(android_app: Pandroid_app; cmd: ShortInt); cdecl;
begin
  if libc_write(android_app.msgwrite, @cmd, SizeOf(cmd)) <> SizeOf(cmd) then
    LOGE('Failure writing android_app cmd');
end;

procedure android_app_set_input(android_app: Pandroid_app; inputQueue: PAInputQueue); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: android_app_set_input');
{$ENDIF}

  pthread_mutex_lock(@android_app.mutex);

  android_app.pendingInputQueue := inputQueue;
  android_app_write_cmd(android_app, APP_CMD_INPUT_CHANGED);

  while android_app.inputQueue <> android_app.pendingInputQueue do
    pthread_cond_wait(@android_app.cond, @android_app.mutex);

  pthread_mutex_unlock(@android_app.mutex);
end;

procedure android_app_set_window(android_app: Pandroid_app; window: PANativeWindow); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: android_app_set_window');
{$ENDIF}

  pthread_mutex_lock(@android_app.mutex);

  if android_app.pendingWindow <> nil then
    android_app_write_cmd(android_app, APP_CMD_TERM_WINDOW);

  android_app.pendingWindow := window;

  if window <> nil then
    android_app_write_cmd(android_app, APP_CMD_INIT_WINDOW);

  while android_app.window <> android_app.pendingWindow do
    pthread_cond_wait(@android_app.cond, @android_app.mutex);

  pthread_mutex_unlock(@android_app.mutex);
end;

procedure android_app_set_activity_state(android_app: Pandroid_app; cmd: ShortInt); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: android_app_set_activity_state');
{$ENDIF}

  pthread_mutex_lock(@android_app.mutex);
  android_app_write_cmd(android_app, cmd);

  while android_app.activityState <> cmd do
    pthread_cond_wait(@android_app.cond, @android_app.mutex);

  pthread_mutex_unlock(@android_app.mutex);
end;

procedure android_app_free(android_app: Pandroid_app); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: android_app_free');
{$ENDIF}

  pthread_mutex_lock(@android_app.mutex);
  android_app_write_cmd(android_app, APP_CMD_DESTROY);

  while android_app.destroyed = 0 do
    pthread_cond_wait(@android_app.cond, @android_app.mutex);

  pthread_mutex_unlock(@android_app.mutex);

  libc_close(android_app.msgread);
  libc_close(android_app.msgwrite);

  pthread_cond_destroy(@android_app.cond);
  pthread_mutex_destroy(@android_app.mutex);

  libc_free(android_app);
end;

procedure onDestroy(activity: PANativeActivity); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onDestroy');
{$ENDIF}

  android_app_free(Pandroid_app(activity.instance));
end;

procedure onStart(activity: PANativeActivity); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onStart');
{$ENDIF}

  android_app_set_activity_state(Pandroid_app(activity.instance), APP_CMD_START);
end;

procedure onResume(activity: PANativeActivity); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onResume');
{$ENDIF}

  android_app_set_activity_state(Pandroid_app(activity.instance), APP_CMD_RESUME);
end;

function onSaveInstanceState(activity: PANativeActivity; outLen: psize_t): Pointer; cdecl;
var
  android_app: Pandroid_app;
  savedState: Pointer;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onSaveInstanceState');
{$ENDIF}

  android_app := activity.instance;
  savedState := nil;

  pthread_mutex_lock(@android_app.mutex);

  android_app.stateSaved := 0;
  android_app_write_cmd(android_app, APP_CMD_SAVE_STATE);

  while android_app.stateSaved = 0 do
    pthread_cond_wait(@android_app.cond, @android_app.mutex);

  if android_app.savedState <> nil then
  begin
    savedState := android_app.savedState;
    outLen^ := android_app.savedStateSize;
    android_app.savedState := nil;
    android_app.savedStateSize := 0;
  end;

  pthread_mutex_unlock(@android_app.mutex);

  Result := savedState;
end;

procedure onPause(activity: PANativeActivity); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onPause');
{$ENDIF}

  android_app_set_activity_state(Pandroid_app(activity.instance), APP_CMD_PAUSE);
end;

procedure onStop(activity: PANativeActivity); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onStop');
{$ENDIF}

  android_app_set_activity_state(Pandroid_app(activity.instance), APP_CMD_STOP);
end;

procedure onConfigurationChanged(activity: PANativeActivity); cdecl;
var
  android_app: Pandroid_app;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onConfigurationChanged');
{$ENDIF}

  android_app := activity.instance;
  android_app_write_cmd(android_app, APP_CMD_CONFIG_CHANGED);
end;

procedure onLowMemory(activity: PANativeActivity); cdecl;
var
  android_app: Pandroid_app;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onLowMemory');
{$ENDIF}

  android_app := activity.instance;
  android_app_write_cmd(android_app, APP_CMD_LOW_MEMORY);
end;

procedure onWindowFocusChanged(activity: PANativeActivity; focused: Integer); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onWindowFocusChanged');
{$ENDIF}

  if focused <> 0 then
    android_app_write_cmd(activity.instance, APP_CMD_GAINED_FOCUS)
  else
    android_app_write_cmd(activity.instance, APP_CMD_LOST_FOCUS);
end;

procedure onNativeWindowCreated(activity: PANativeActivity; window: PANativeWindow); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onNativeWindowCreated');
{$ENDIF}

  android_app_set_window(activity.instance, window);
end;

procedure onNativeWindowDestroyed(activity: PANativeActivity; window: PANativeWindow); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onNativeWindowDestroyed');
{$ENDIF}

  android_app_set_window(activity.instance, nil);
end;

procedure onInputQueueCreated(activity: PANativeActivity; queue: PAInputQueue); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onInputQueueCreated');
{$ENDIF}

  android_app_set_input(activity.instance, queue);
end;

procedure onInputQueueDestroyed(activity: PANativeActivity; queue: PAInputQueue); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: onInputQueueDestroyed');
{$ENDIF}

  android_app_set_input(activity.instance, nil);
end;

procedure ANativeActivity_onCreate(activity: PANativeActivity; savedState: Pointer; savedStateSize: size_t); cdecl;
begin
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: ANativeActivity_onCreate');
{$ENDIF}

  activity.callbacks.onDestroy := @onDestroy;
  activity.callbacks.onStart := @onStart;
  activity.callbacks.onResume := @onResume;
  activity.callbacks.onSaveInstanceState := @onSaveInstanceState;
  activity.callbacks.onPause := @onPause;
  activity.callbacks.onStop := @onStop;
  activity.callbacks.onConfigurationChanged := @onConfigurationChanged;
  activity.callbacks.onLowMemory := @onLowMemory;
  activity.callbacks.onWindowFocusChanged := @onWindowFocusChanged;
  activity.callbacks.onNativeWindowCreated := @onNativeWindowCreated;
  activity.callbacks.onNativeWindowDestroyed := @onNativeWindowDestroyed;
  activity.callbacks.onInputQueueCreated := @onInputQueueCreated;
  activity.callbacks.onInputQueueDestroyed := @onInputQueueDestroyed;

  activity.instance := android_app_create(activity, savedState, savedStateSize);
end;

{$ENDREGION}

// Note: this method needs to be exported in main project source.
{exports
  ANativeActivity_onCreate;}

end.
