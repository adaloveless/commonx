unit Android.NativeWindow;
{
  Copyright (C) 2010 The Android Open Source Project

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
   http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed
  on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for
  the specific language governing permissions and limitations under the License.

  Android NDK C/C++ files:
    android/native_window.h
    android/native_window_jni.h

  Original source code was taken from:
    %NDK_DIR%/platforms/android-9/arch-arm/usr/include/

  Pascal translation by Yuriy Kotsarenko, August 2015.
}
interface

{$INCLUDE Android.Config.inc}

uses
  jni, Android.Rect;

{$INCLUDE Android.LibDefs.inc}

const
{ Pixel formats that a window can use }
  WINDOW_FORMAT_RGBA_8888 = 1;
  WINDOW_FORMAT_RGBX_8888 = 2;
  WINDOW_FORMAT_RGB_565   = 4;

type
  PANativeWindow = ^ANativeWindow;
  ANativeWindow = record
  end;

  PANativeWindow_Buffer = ^ANativeWindow_Buffer;
  ANativeWindow_Buffer = record
    width: LongInt;

    { The number of pixels that are shown vertically. }
    height: LongInt;

    { The number of *pixels* that a line in the buffer takes in memory.  This may be >= width. }
    stride: LongInt;

    { The format of the buffer.  One of WINDOW_FORMAT_* }
    format: LongInt;

    { The actual bits. }
    bits: Pointer;

    { Do not touch. }
    reserved: array[0..5] of LongWord;
  end;

{ Acquire a reference on the given ANativeWindow object.  This prevents the object from being deleted until the
  reference is removed. }
procedure ANativeWindow_acquire(window: PANativeWindow); cdecl;
  external libandroid name 'ANativeWindow_acquire';

{ Remove a reference that was previously acquired with ANativeWindow_acquire(). }
procedure ANativeWindow_release(window: PANativeWindow); cdecl;
  external libandroid name 'ANativeWindow_release';

{ Return the current width in pixels of the window surface.  Returns a negative value on error. }
function ANativeWindow_getWidth(window: PANativeWindow): LongInt; cdecl;
  external libandroid name 'ANativeWindow_getWidth';

{ Return the current height in pixels of the window surface.  Returns a negative value on error. }
function ANativeWindow_getHeight(window: PANativeWindow): LongInt; cdecl;
  external libandroid name 'ANativeWindow_getHeight';

{ Return the current pixel format of the window surface.  Returns a negative value on error. }
function ANativeWindow_getFormat(window: PANativeWindow): LongInt; cdecl;
  external libandroid name 'ANativeWindow_getFormat';

{
  Change the format and size of the window buffers.

  The width and height control the number of pixels in the buffers, not the dimensions of the window on screen.
  If these are different than the window's physical size, then it buffer will be scaled to match that size when
  compositing it to the screen.

  For all of these parameters, if 0 is supplied then the window's base value will come back in force.
}
function ANativeWindow_setBuffersGeometry(window: PANativeWindow; width, height, format: LongInt): LongInt; cdecl;
  external libandroid name 'ANativeWindow_setBuffersGeometry';

{ Lock the window's next drawing surface for writing. }
function ANativeWindow_lock(window: PANativeWindow; outBuffer: PANativeWindow_Buffer;
  inOutDirtyBounds: PARect): LongInt; cdecl;
  external libandroid name 'ANativeWindow_lock';

{ Unlock the window's drawing surface after previously locking it,
  posting the new buffer to the display. }
function ANativeWindow_unlockAndPost(window: PANativeWindow): LongInt; cdecl;
  external libandroid name 'ANativeWindow_unlockAndPost';

{
  Return the ANativeWindow associated with a Java Surface object, for interacting with it through native code.
  This acquires a reference on the ANativeWindow that is returned; be sure to use ANativeWindow_release() when done
  with it so that it doesn't leak.
}
function ANativeWindow_fromSurface(env: PJNIEnv; surface: jobject): PANativeWindow; cdecl;
  external libandroid name 'ANativeWindow_fromSurface';

implementation

end.

