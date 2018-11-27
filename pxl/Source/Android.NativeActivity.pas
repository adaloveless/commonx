{
  Copyright (C) 2010 The Android Open Source Project

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
   http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed
  on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for
  the specific language governing permissions and limitations under the License.

  Android NDK C/C++ file:
    android/native_activity.h

  Original source code was taken from:
    %NDK_DIR%/platforms/android-9/arch-arm/usr/include/

  Pascal translation by Yuriy Kotsarenko, August 2015.
}
unit Android.NativeActivity;

{
  IMPORTANT NOTICE:

    This file is part of Android's set of stable system headers exposed by the Android NDK (Native Development Kit)
    since platform release 1.5

    Third-party source AND binary code relies on the definitions here to be FROZEN ON ALL UPCOMING PLATFORM RELEASES.

    - DO NOT MODIFY ENUMS (EXCEPT IF YOU ADD NEW 32-BIT VALUES)
    - DO NOT MODIFY CONSTANTS OR FUNCTIONAL MACROS
    - DO NOT CHANGE THE SIGNATURE OF FUNCTIONS IN ANY WAY
    - DO NOT CHANGE THE LAYOUT OR SIZE OF STRUCTURES
 }

{
  Support routines to send messages to the Android in-kernel log buffer, which can later be accessed through the
  'logcat' utility.

  Each log message must have
    - a priority
    - a log tag
    - some text

  The tag normally corresponds to the component that emits the log message, and should be reasonably small.

  Log message text may be truncated to less than an implementation-specific limit (e.g. 1023 characters max).

  Note that a newline character ("\n") will be appended automatically to your log message, if not already there.
  It is not possible to send several messages and have them appear on a single line in logcat.

  PLEASE USE LOGS WITH MODERATION:

   - Sending log messages eats CPU and slow down your application and the system.

   - The circular log buffer is pretty small (<64KB), sending many messages might push off other important log messages
     from the rest of the system.

   - In release builds, only send log messages to account for exceptional conditions.

  NOTE: These functions MUST be implemented by /system/lib/liblog.so
 }

interface

{$INCLUDE Android.Config.inc}

uses
  unixtype, jni, Android.AssetManager, Android.NativeWindow, Android.Input, Android.Rect;

{$INCLUDE Android.LibDefs.inc}

type
  Psize_t = ^size_t;

  PANativeActivityCallbacks = ^ANativeActivityCallbacks;

{
  This structure defines the native side of an android.app.NativeActivity.
  It is created by the framework, and handed to the application's native code as it is being launched.
}
  PANativeActivity = ^ANativeActivity;
  ANativeActivity = record
    { Pointer to the callback function table of the native application.
      You can set the functions here to your own callbacks. The callbacks pointer itself here should not be changed;
      it is allocated and managed for you by the framework. }
    callbacks: PANativeActivityCallbacks;

    { The global handle on the process's Java VM. }
    vm: PJavaVM;

    { JNI context for the main thread of the app. Note that this field can ONLY be used from the main thread of the
      process; that is, the thread that calls into the ANativeActivityCallbacks. }
    env: PJNIEnv;

    { The NativeActivity object handle.

      IMPORTANT NOTE: This member is mis-named. It should really be named 'activity' instead of 'clazz', since it's a
      reference to the NativeActivity instance created by the system for you.

      We unfortunately cannot change this without breaking NDK source-compatibility. }
    clazz: jobject;

    { Path to this application's internal data directory. }
    internalDataPath: PAnsiChar;

    { Path to this application's external (removable/mountable) data directory. }
    externalDataPath: PAnsiChar;

    { The platform's SDK version code. }
    sdkVersion: LongInt;

    { This is the native instance of the application.  It is not used by the framework, but can be set by the
      application to its own instance state. }
    instance: Pointer;

    { Pointer to the Asset Manager instance for the application. The application uses this to access binary assets
      bundled inside its own .apk file.  }
    assetManager: PAAssetManager;
  end;

{
  These are the callbacks the framework makes into a native application.
  All of these callbacks happen on the main thread of the application.
  By default, all callbacks are NULL; set to a pointer to your own function to have it called.
}
  ANativeActivityCallbacks = record
    { NativeActivity has started. See Java documentation for Activity.onStart() for more information. }
    onStart: procedure(activity: PANativeActivity); cdecl;

    { NativeActivity has resumed. See Java documentation for Activity.onResume() for more information. }
    onResume: procedure(activity: PANativeActivity); cdecl;

    { Framework is asking NativeActivity to save its current instance state.
      See Java documentation for Activity.onSaveInstanceState() for more information.
      The returned pointer needs to be created with malloc(); the framework will call free() on it for you.
      You also must fill in outSize with the number of bytes in the allocation.
      Note that the saved state will be persisted, so it can not contain any active entities (pointers to memory, file
      descriptors, etc). }
    onSaveInstanceState: function(activity: PANativeActivity; outSize: Psize_t): Pointer; cdecl;

    { NativeActivity has paused. See Java documentation for Activity.onPause() for more information. }
    onPause: procedure(activity: PANativeActivity); cdecl;

    { NativeActivity has stopped. See Java documentation for Activity.onStop() for more information. }
    onStop: procedure(activity: PANativeActivity); cdecl;

    { NativeActivity is being destroyed. See Java documentation for Activity.onDestroy() for more information. }
    onDestroy: procedure(activity: PANativeActivity); cdecl;

    { Focus has changed in this NativeActivity's window. This is often used, for example, to pause a game when it loses
      input focus. }
    onWindowFocusChanged: procedure(activity: PANativeActivity; HasFocus: Integer); cdecl;

    { The drawing window for this native activity has been created. You can use the given native window object to start
      drawing. }
    onNativeWindowCreated: procedure(activity: PANativeActivity; window: PANativeWindow); cdecl;

    { The drawing window for this native activity has been resized. You should retrieve the new size from the window
      and ensure that your rendering in it now matches. }
    onNativeWindowResized: procedure(activity: PANativeActivity; window: PANativeWindow); cdecl;

    { The drawing window for this native activity needs to be redrawn. To avoid transient artifacts during screen
      changes (such resizing after rotation), applications should not return from this function until they have
      finished drawing their window in its current state. }
    onNativeWindowRedrawNeeded: procedure(activity: PANativeActivity; window: PANativeWindow); cdecl;

    { The drawing window for this native activity is going to be destroyed.
      You MUST ensure that you do not touch the window object after returning from this function: in the common case of
      drawing to the window from another thread, that means the implementation of this callback must properly
      synchronize with the other thread to stop its drawing before returning from here. }
    onNativeWindowDestroyed: procedure(activity: PANativeActivity; window: PANativeWindow); cdecl;

    { The input queue for this native activity's window has been created.
      You can use the given input queue to start retrieving input events. }
    onInputQueueCreated: procedure(activity: PANativeActivity; queue: PAInputQueue); cdecl;

    { The input queue for this native activity's window is being destroyed.
      You should no longer try to reference this object upon returning from this function. }
    onInputQueueDestroyed: procedure(activity: PANativeActivity; queue: PAInputQueue); cdecl;

    { The rectangle in the window in which content should be placed has changed. }
    onContentRectChanged: procedure(activity: PANativeActivity; rect: PARect); cdecl;

    { The current device AConfiguration has changed. The new configuration can be retrieved from assetManager. }
    onConfigurationChanged: procedure(activity: PANativeActivity); cdecl;

    { The system is running low on memory. Use this callback to release resources you do not need, to help the system
      avoid killing more important processes. }
    onLowMemory: procedure(activity: PANativeActivity); cdecl;
  end;

{
  This is the function that must be in the native code to instantiate the application's native activity. It is called
  with the activity instance (see above); if the code is being instantiated from a previously saved instance,
  the savedState will be non-NULL and point to the saved data.  You must make any copy of this data you need -- it
  will be released after you return from this function.
 }
  ANativeActivity_createFunc = procedure(activity: PANativeActivity; savedState: Pointer;
    savedStateSize: size_t); cdecl;

{
  Flags for ANativeActivity_showSoftInput; see the Java InputMethodManager API for documentation.
}
const
  ANATIVEACTIVITY_SHOW_SOFT_INPUT_IMPLICIT = $0001;
  ANATIVEACTIVITY_SHOW_SOFT_INPUT_FORCED = $0002;

{
  Flags for ANativeActivity_hideSoftInput; see the Java InputMethodManager API for documentation.
}
  ANATIVEACTIVITY_HIDE_SOFT_INPUT_IMPLICIT_ONLY = $0001;
  ANATIVEACTIVITY_HIDE_SOFT_INPUT_NOT_ALWAYS = $0002;

{
  The name of the function that NativeInstance looks for when launching its native code. This is the default function
  that is used, you can specify "android.app.func_name" string meta-data in your manifest to use a different function.
}
{var
  ANativeActivity_onCreate: ANativeActivity_createFunc;}

{
  Finish the given activity.  Its finish() method will be called, causing it to be stopped and destroyed. Note that
  this method can be called from *any* thread; it will send a message to the main thread of the process where the Java
  finish call will take place.
}
procedure ANativeActivity_finish(activity: PANativeActivity); cdecl;
  external libandroid name 'ANativeActivity_finish';

{
  Change the window format of the given activity. Calls getWindow().setFormat() of the given activity. Note that this
  method can be called from *any* thread; it will send a message to the main thread of the process where the Java
  finish call will take place.
}
procedure ANativeActivity_setWindowFormat(activity: PANativeActivity; format: LongInt); cdecl;
  external libandroid name 'ANativeActivity_setWindowFormat';

{
  Change the window flags of the given activity. Calls getWindow().setFlags() of the given activity. Note that this
  method can be called from *any* thread; it will send a message to the main thread of the process where the Java
  finish call will take place. See window.h for flag constants.
}
procedure ANativeActivity_setWindowFlags(activity: PANativeActivity; addFlags, removeFlags: LongWord); cdecl;
  external libandroid name 'ANativeActivity_setWindowFlags';

{
  Show the IME while in the given activity. Calls InputMethodManager.showSoftInput() for the given activity. Note that
  this method can be called from *any* thread; it will send a message to the main thread of the process where the Java
  finish call will take place.
}
procedure ANativeActivity_showSoftInput(activity: PANativeActivity; flags: LongWord); cdecl;
  external libandroid name 'ANativeActivity_showSoftInput';

{
  Hide the IME while in the given activity. Calls InputMethodManager.hideSoftInput() for the given activity. Note that
  this method can be called from *any* thread; it will send a message to the main thread of the process where the Java
  finish call will take place.
}
procedure ANativeActivity_hideSoftInput(activity: PANativeActivity; flags: LongWord); cdecl;
  external libandroid name 'ANativeActivity_hideSoftInput';

implementation

{exports
  ANativeActivity_onCreate;}

end.
