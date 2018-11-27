{
  Copyright (C) 2008 The Android Open Source Project
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
  following conditions are met:
    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
      disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
      following disclaimer in the documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Android NDK C/C++ file:
    dlfcn.h

  Original source code was taken from:
    %NDK_DIR%/platforms/android-9/arch-arm/usr/include/

  Pascal translation by Yuriy Kotsarenko, August 2015.
}
unit Android.DlFcn;

interface

{$INCLUDE Android.Config.inc}
{$INCLUDE Android.LibDefs.inc}

type
  PDl_info = ^Dl_info;
  Dl_info = record
    dli_fname: PAnsiChar; // Pathname of shared object that contains address
    dli_fbase: Pointer;   // Address at which shared object is loaded
    dli_sname: PAnsiChar; // Name of nearest symbol with address lower than addr
    dli_saddr: Pointer;   // Exact address of symbol named in dli_sname
  end;

const
  RTLD_NOW  = 0;
  RTLD_LAZY = 1;

  RTLD_LOCAL  = 0;
  RTLD_GLOBAL = 2;

  RTLD_DEFAULT = Pointer($FFFFFFFF);
  RTLD_NEXT = Pointer($FFFFFFFE);

function dlopen(filename: PAnsiChar; flag: LongInt): Pointer; cdecl;
  external libdl name 'dlopen';

function dlclose(handle: Pointer): LongInt; cdecl;
  external libdl name 'dlclose';

function dlerror: PAnsiChar; cdecl;
  external libdl name 'dlerror';

function dlsym(handle: Pointer; symbol: PAnsiChar): Pointer; cdecl;
  external libdl name 'dlsym';

function dladdr(addr: Pointer; info: PDL_info): LongInt; cdecl;
  external libdl name 'dladdr';

implementation

end.
