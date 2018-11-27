{
  Copyright (C) 2010 The Android Open Source Project

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
   http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed
  on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for
  the specific language governing permissions and limitations under the License.

  Android NDK C/C++ files:
    android/asset_manager.h
    android/asset_manager_jni.h

  Original source code was taken from:
    %NDK_DIR%/platforms/android-9/arch-arm/usr/include/

  Pascal translation by Yuriy Kotsarenko, August 2015.
}
unit Android.AssetManager;

interface

{$INCLUDE Android.Config.inc}

uses
  unixtype, jni;

{$INCLUDE Android.LibDefs.inc}

type
  Poff_t = ^off_t;
  Poff64_t = ^off64_t;

  PAAssetManager = ^AAssetManager;
  AAssetManager = record
  end;

  PAAssetDir = ^AAssetDir;
  AAssetDir = record
  end;

  PAAsset = ^AAsset;
  AAsset = record
  end;

const
{ Available modes for opening assets }
  AASSET_MODE_UNKNOWN = 0;
  AASSET_MODE_RANDOM = 1;
  AASSET_MODE_STREAMING = 2;
  AASSET_MODE_BUFFER = 3;

{
  Given a Dalvik AssetManager object, obtain the corresponding native AAssetManager object.  Note that the caller is
  responsible for obtaining and holding a VM reference to the jobject to prevent its being garbage collected while the
  native object is in use.
}
function AAssetManager_fromJava(env: PJNIEnv; assetManager: jobject): PAAssetManager; cdecl;
  external libandroid name 'AAssetManager_fromJava';

{
  Open the named directory within the asset hierarchy.  The directory can then be inspected with the AAssetDir
  functions.  To open the top-level directory,pass in "" as the dirName.

  The object returned here should be freed by calling AAssetDir_close().
}
function AAssetManager_openDir(mgr: PAAssetManager; dirName: PAnsiChar): PAAssetDir; cdecl;
  external libandroid name 'AAssetManager_openDir';

{
  Open an asset.

  The object returned here should be freed by calling AAsset_close().
}
function AAssetManager_open(mgr: PAAssetManager; filename: PAnsiChar; Mode: LongInt): PAAsset; cdecl;
  external libandroid name 'AAssetManager_open';

{
  Iterate over the files in an asset directory.  A NULL string is returned when all the file names have been returned.

  The returned file name is suitable for passing to AAssetManager_open().

  The string returned here is owned by the AssetDir implementation and is not guaranteed to remain valid if any other
  calls are made on this AAssetDir instance.
}
function AAssetDir_getNextFileName(assetDir: PAAssetDir): PAnsiChar; cdecl;
  external libandroid name 'AAssetDir_getNextFileName';

{
  Reset the iteration state of AAssetDir_getNextFileName() to the beginning.
}
procedure AAssetDir_rewind(assetDir: PAAssetDir); cdecl;
  external libandroid name 'AAssetDir_rewind';

{
  Close an opened AAssetDir, freeing any related resources.
}
procedure AAssetDir_close(assetDir: PAAssetDir); cdecl;
  external libandroid name 'AAssetDir_close';

{
  Attempt to read 'count' bytes of data from the current offset.

  Returns the number of bytes read, zero on EOF, or < 0 on error.
}
function AAsset_read(asset: PAAsset; buf: Pointer; count: size_t): LongInt; cdecl;
  external libandroid name 'AAsset_read';

{
  Seek to the specified offset within the asset data.  'whence' uses the same constants as lseek()/fseek().

  Returns the new position on success, or (off_t) -1 on error.
}
function AAsset_seek(asset: PAAsset; offset: off_t; whence: LongInt): off_t; cdecl;
  external libandroid name 'AAsset_seek';

{
  Seek to the specified offset within the asset data.  'whence' uses the same constants as lseek()/fseek().

  Uses 64-bit data type for large files as opposed to the 32-bit type used by AAsset_seek.

  Returns the new position on success, or (off64_t) -1 on error.
}
function AAsset_seek64(asset: PAAsset; offset: off64_t; whence: LongInt): off64_t; cdecl;
  external libandroid name 'AAsset_seek64';

{
  Close the asset, freeing all associated resources.
}
procedure AAsset_close(asset: PAAsset); cdecl;
  external libandroid name 'AAsset_close';

{
  Get a pointer to a buffer holding the entire contents of the assset.

  Returns nil on failure.
}
function AAsset_getBuffer(asset: PAAsset): Pointer; cdecl;
  external libandroid name 'AAsset_getBuffer';

{
  Report the total size of the asset data.
}
function AAsset_getLength(asset: PAAsset): off_t; cdecl;
  external libandroid name 'AAsset_getLength';

{
  Report the total size of the asset data. Reports the size using a 64-bit number insted of 32-bit as AAsset_getLength.
}
function AAsset_getLength64(asset: PAAsset): off64_t; cdecl;
  external libandroid name 'AAsset_getLength64';

{
  Report the total amount of asset data that can be read from the current position.
}
function AAsset_getRemainingLength(asset: PAAsset): off_t; cdecl;
  external libandroid name 'AAsset_getRemainingLength';

{
  Report the total amount of asset data that can be read from the current position.

  Uses a 64-bit number instead of a 32-bit number as AAsset_getRemainingLength does.
}
function AAsset_getRemainingLength64(asset: PAAsset): off64_t; cdecl;
  external libandroid name 'AAsset_getRemainingLength64';

{
  Open a new file descriptor that can be used to read the asset data.

  Returns < 0 if direct fd access is not possible (for example, if the asset is compressed).
}
function AAsset_openFileDescriptor(asset: PAAsset; outStart, outLength: Poff_t): LongInt; cdecl;
  external libandroid name 'AAsset_openFileDescriptor';

{
  Open a new file descriptor that can be used to read the asset data.

  Uses a 64-bit number for the offset and length instead of 32-bit instead of as AAsset_openFileDescriptor does.

  Returns < 0 if direct fd access is not possible (for example, if the asset is compressed).
}
function AAsset_openFileDescriptor64(asset: PAAsset; outStart, outLength: Poff64_t): LongInt; cdecl;
  external libandroid name 'AAsset_openFileDescriptor64';

{
  Returns whether this asset's internal buffer is allocated in ordinary RAM (i.e. not mmapped).
}
function AAsset_isAllocated(asset: PAAsset): LongInt; cdecl;
  external libandroid name 'AAsset_isAllocated';

implementation

end.
