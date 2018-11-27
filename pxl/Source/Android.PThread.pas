{
  Copyright (C) 2008 The Android Open Source Project
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
  following conditions are met:
    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
      disclaimer.
    * Redistributions in binary form must reproduce the above copyright  notice, this list of conditions and the
      following disclaimer in the documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Android NDK C/C++ file:
    pthread.h

  Original source code was taken from:
    %NDK_DIR%/platforms/android-9/arch-arm/usr/include/

  Pascal translation by Yuriy Kotsarenko, August 2015.
}
unit Android.PThread;

interface

{$INCLUDE Android.Config.inc}

uses
  unixtype, baseunix;

{$INCLUDE Android.LibDefs.inc}

type
  Psched_param = ^sched_param;
  Psigset_t = ^sigset_t;

  Pclockid_t = ^clockid_t;
  clockid_t = LongInt;

  Ppthread_mutex_t = ^pthread_mutex_t;
  pthread_mutex_t = record
    value: LongInt;
  end;

  Ppthread_cond_t = ^pthread_cond_t;
  pthread_cond_t = record
    value: LongInt;
  end;

  Ppthread_attr_t = ^pthread_attr_t;
  pthread_attr_t = record
    flags: LongWord;
    stack_base: Pointer;
    stack_size: size_t;
    guard_size: size_t;
    sched_policy: LongInt;
    sched_priority: LongInt;
  end;

  Ppthread_mutexattr_t = ^pthread_mutexattr_t;
  pthread_mutexattr_t = LongInt;

  Ppthread_condattr_t = ^pthread_condattr_t;
  pthread_condattr_t = LongInt;

  Ppthread_key_t = ^pthread_key_t;
  pthread_key_t = LongInt;

  Ppthread_t = ^pthread_t;
  pthread_t = LongWord;

  Ppthread_once_t = ^pthread_once_t;
  pthread_once_t = LongInt;

  Ppthread_rwlockattr_t = ^pthread_rwlockattr_t;
  pthread_rwlockattr_t = LongInt;

  Ppthread_rwlock_t = ^pthread_rwlock_t;
  pthread_rwlock_t = record
    lock: pthread_mutex_t;
    cond: pthread_cond_t;
    numLocks: LongInt;
    writerThreadId: LongInt;
    pendingReaders: LongInt;
    pendingWriters: LongInt;
    reserved: array[0..3] of Pointer; // for future extensibility
  end;

  pthread_create_func = function(arg: Pointer): Pointer; cdecl;
  pthread_key_create_proc = procedure (arg: Pointer); cdecl;

const
  PTHREAD_MUTEX_INIT_VALUE = 0;
  PTHREAD_RECURSIVE_MUTEX_INIT_VALUE = $4000;
  PTHREAD_ERRORCHECK_MUTEX_INIT_VALUE = $8000;

  PTHREAD_MUTEX_NORMAL = 0;
  PTHREAD_MUTEX_RECURSIVE = 1;
  PTHREAD_MUTEX_ERRORCHECK = 2;

  PTHREAD_MUTEX_ERRORCHECK_NP = PTHREAD_MUTEX_ERRORCHECK;
  PTHREAD_MUTEX_RECURSIVE_NP = PTHREAD_MUTEX_RECURSIVE;

  PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL;

  PTHREAD_CREATE_DETACHED = $00000001;
  PTHREAD_CREATE_JOINABLE = $00000000;

  PTHREAD_ONCE_INIT = 0;

  PTHREAD_PROCESS_PRIVATE = 0;
  PTHREAD_PROCESS_SHARED = 1;

  PTHREAD_SCOPE_SYSTEM = 0;
  PTHREAD_SCOPE_PROCESS = 1;

function pthread_attr_init(attr: Ppthread_attr_t): LongInt; cdecl;
  external libc name 'pthread_attr_init';
function pthread_attr_destroy(attr: Ppthread_attr_t): LongInt; cdecl
  external libc name 'pthread_attr_destroy';

function pthread_attr_setdetachstate(attr: Ppthread_attr_t; state: LongInt): LongInt; cdecl
  external libc name 'pthread_attr_setdetachstate';
function pthread_attr_getdetachstate(attr: Ppthread_attr_t; state: PLongInt): LongInt; cdecl;
  external libc name 'pthread_attr_getdetachstate';

function pthread_attr_setschedpolicy(attr: Ppthread_attr_t; policy: LongInt): LongInt; cdecl;
  external libc name 'pthread_attr_setschedpolicy';
function pthread_attr_getschedpolicy(attr: Ppthread_attr_t; policy: PLongInt): LongInt; cdecl;
  external libc name 'pthread_attr_getschedpolicy';

function pthread_attr_setschedparam(attr: Ppthread_attr_t; param: Psched_param): LongInt; cdecl;
  external libc name 'pthread_attr_setschedparam';
function pthread_attr_getschedparam(attr: Ppthread_attr_t; param: Psched_param): LongInt; cdecl;
  external libc name 'pthread_attr_getschedparam';

function pthread_attr_setstacksize(attr: Ppthread_attr_t; stack_size: size_t): LongInt; cdecl;
  external libc name 'pthread_attr_setstacksize';
function pthread_attr_getstacksize(attr: Ppthread_attr_t; stack_size: Psize_t): LongInt; cdecl;
  external libc name 'pthread_attr_getstacksize';

function pthread_attr_setstackaddr(attr: Ppthread_attr_t; stackaddr: Pointer): LongInt; cdecl;
  external libc name 'pthread_attr_setstackaddr';
function pthread_attr_getstackaddr(attr: Ppthread_attr_t; stackaddr: PPointer): LongInt; cdecl;
  external libc name 'pthread_attr_getstackaddr';

function pthread_attr_setstack(attr: Ppthread_attr_t; stackaddr: Pointer; stack_size: size_t): LongInt; cdecl;
  external libc name 'pthread_attr_setstack';
function pthread_attr_getstack(attr: Ppthread_attr_t; stackaddr: PPointer; stack_size: Psize_t): LongInt; cdecl;
  external libc name 'pthread_attr_getstack';

function pthread_attr_setguardsize(attr: Ppthread_attr_t; guard_size: size_t): LongInt; cdecl;
  external libc name 'pthread_attr_setguardsize';
function pthread_attr_getguardsize(attr: Ppthread_attr_t; guard_size: Psize_t): LongInt; cdecl;
  external libc name 'pthread_attr_getguardsize';

function pthread_attr_setscope(attr: Ppthread_attr_t; scope: LongInt): LongInt; cdecl;
  external libc name 'pthread_attr_setscope';

{ Note: original declaration seemed incorrect (?):
    int pthread_attr_getscope(pthread_attr_t const *attr); }
function pthread_attr_getscope(attr: Ppthread_attr_t; scope: PLongInt): LongInt; cdecl;
  external libc name 'pthread_attr_getscope';

function pthread_getattr_np(thid: pthread_t; attr: Ppthread_attr_t): LongInt; cdecl;
  external libc name 'pthread_getattr_np';

function pthread_create(thread: Ppthread_t; attr: Ppthread_attr_t; start_routine: pthread_create_func;
  arg: Pointer): LongInt; cdecl;
  external libc name 'pthread_create';
procedure pthread_exit(retval: PLongInt); cdecl;
  external libc name 'pthread_exit';
function pthread_join(thid: pthread_t; ret_val: PPointer): LongInt; cdecl;
  external libc name 'pthread_join';
function pthread_detach(thid: pthread_t): LongInt; cdecl;
  external libc name 'pthread_detach';
function pthread_self: pthread_t; cdecl;
  external libc name 'pthread_self';
function pthread_equal(one, two: pthread_t): LongInt; cdecl;
  external libc name 'pthread_equal';

function pthread_getschedparam(thid: pthread_t; policy: PLongInt; param: Psched_param): LongInt; cdecl;
  external libc name 'pthread_getschedparam';
function pthread_setschedparam(thid: pthread_t; policy: LongInt; param: Psched_param): LongInt; cdecl;
  external libc name 'pthread_setschedparam';

function pthread_mutexattr_init(attr: Ppthread_mutexattr_t): LongInt; cdecl;
  external libc name 'pthread_mutexattr_init';
function pthread_mutexattr_destroy(attr: Ppthread_mutexattr_t): LongInt; cdecl;
  external libc name 'pthread_mutexattr_destroy';
function pthread_mutexattr_gettype(attr: Ppthread_mutexattr_t; _type: PLongInt): LongInt; cdecl;
  external libc name 'pthread_mutexattr_gettype';
function pthread_mutexattr_settype(attr: Ppthread_mutexattr_t; _type: LongInt): LongInt; cdecl;
  external libc name 'pthread_mutexattr_settype';
function pthread_mutexattr_setpshared(attr: Ppthread_mutexattr_t; pshared: LongInt): LongInt; cdecl;
  external libc name 'pthread_mutexattr_setpshared';
function pthread_mutexattr_getpshared(attr: Ppthread_mutexattr_t; pshared: PLongInt): LongInt; cdecl;
  external libc name 'pthread_mutexattr_getpshared';

function pthread_mutex_init(mutex: Ppthread_mutex_t; attr: Ppthread_mutexattr_t): LongInt; cdecl;
  external libc name 'pthread_mutex_init';
function pthread_mutex_destroy(mutex: Ppthread_mutex_t): LongInt; cdecl;
  external libc name 'pthread_mutex_destroy';
function pthread_mutex_lock(mutex: Ppthread_mutex_t): LongInt; cdecl;
  external libc name 'pthread_mutex_lock';
function pthread_mutex_unlock(mutex: Ppthread_mutex_t): LongInt; cdecl;
  external libc name 'pthread_mutex_unlock';
function pthread_mutex_trylock(mutex: Ppthread_mutex_t): LongInt; cdecl;
  external libc name 'pthread_mutex_trylock';

// Note: the following method is not exported in "libc.so".
// function pthread_mutex_timedlock(mutex: Ppthread_mutex_t; ts: ptimespec): LongInt; cdecl;
//   external libc name 'pthread_mutex_timedlock';

function pthread_condattr_init(attr: Ppthread_condattr_t): LongInt; cdecl;
  external libc name 'pthread_condattr_init';
function pthread_condattr_getpshared(attr: Ppthread_condattr_t; pshared: PLongInt): LongInt; cdecl;
  external libc name 'pthread_condattr_getpshared';
function pthread_condattr_setpshared(attr: Ppthread_condattr_t; pshared: LongInt): LongInt; cdecl;
  external libc name 'pthread_condattr_setpshared';
function pthread_condattr_destroy(attr: Ppthread_condattr_t): LongInt; cdecl;
  external libc name 'pthread_condattr_destroy';

function pthread_cond_init(cond: Ppthread_cond_t; attr: Ppthread_condattr_t): LongInt; cdecl;
  external libc name 'pthread_cond_init';

function pthread_cond_destroy(cond: Ppthread_cond_t): LongInt; cdecl;
  external libc name 'pthread_cond_destroy';

function pthread_cond_broadcast(cond: Ppthread_cond_t): LongInt; cdecl;
  external libc name 'pthread_cond_broadcast';

function pthread_cond_signal(cond: Ppthread_cond_t): LongInt; cdecl;
  external libc name 'pthread_cond_signal';

function pthread_cond_wait(cond: Ppthread_cond_t; mutex: Ppthread_mutex_t): LongInt; cdecl;
  external libc name 'pthread_cond_wait';

function pthread_cond_timedwait(cond: Ppthread_cond_t; mutex: Ppthread_mutex_t; abstime: ptimespec): LongInt; cdecl;
  external libc name 'pthread_cond_timedwait';

{ same as pthread_cond_timedwait, except the 'abstime' given refers to the CLOCK_MONOTONIC clock instead, to avoid any
  problems when the wall-clock time is changed brutally }
function pthread_cond_timedwait_monotonic_np(cond: Ppthread_cond_t; mutex: Ppthread_mutex_t;
  abstime: ptimespec): LongInt; cdecl;
  external libc name 'pthread_cond_timedwait_monotonic_np';

{ DEPRECATED. same as pthread_cond_timedwait_monotonic_np()
  unfortunately pthread_cond_timedwait_monotonic has shipped already }
function pthread_cond_timedwait_monotonic(cond: Ppthread_cond_t; mutex: Ppthread_mutex_t;
  abstime: ptimespec): LongInt; cdecl;
  external libc name 'pthread_cond_timedwait_monotonic';

{ same as pthread_cond_timedwait, except the 'reltime' given refers is relative to the current time. }
function pthread_cond_timedwait_relative_np(cond: Ppthread_cond_t; mutex: Ppthread_mutex_t;
  reltime: ptimespec): LongInt; cdecl;
  external libc name 'pthread_cond_timedwait_relative_np';

function pthread_cond_timeout_np(cond: Ppthread_cond_t; mutex: Ppthread_mutex_t; msecs: LongWord): LongInt; cdecl;
  external libc name 'pthread_cond_timeout_np';

{ same as pthread_mutex_lock(), but will wait up to 'msecs' milli-seconds before returning. same return values than
  pthread_mutex_trylock though, i.e. returns EBUSY if the lock could not be acquired after the timeout expired. }
function pthread_mutex_lock_timeout_np(mutex: Ppthread_mutex_t; msecs: LongWord): LongInt; cdecl;
  external libc name 'pthread_mutex_lock_timeout_np';

function pthread_rwlockattr_init(attr: Ppthread_rwlockattr_t): LongInt; cdecl;
  external libc name 'pthread_rwlockattr_init';
function pthread_rwlockattr_destroy(attr: Ppthread_rwlockattr_t): LongInt; cdecl;
  external libc name 'pthread_rwlockattr_destroy';
function pthread_rwlockattr_setpshared(attr: Ppthread_rwlockattr_t; pshared: LongInt): LongInt; cdecl;
  external libc name 'pthread_rwlockattr_setpshared';
function pthread_rwlockattr_getpshared(attr: Ppthread_rwlockattr_t; pshared: PLongInt): LongInt; cdecl;
  external libc name 'pthread_rwlockattr_getpshared';

function pthread_rwlock_init(rwlock: Ppthread_rwlock_t; attr: Ppthread_rwlockattr_t): LongInt; cdecl;
  external libc name 'pthread_rwlock_init';
function pthread_rwlock_destroy(rwlock: Ppthread_rwlock_t): LongInt; cdecl;
  external libc name 'pthread_rwlock_destroy';

function pthread_rwlock_rdlock(rwlock: Ppthread_rwlock_t): LongInt; cdecl;
  external libc name 'pthread_rwlock_rdlock';
function pthread_rwlock_tryrdlock(rwlock: Ppthread_rwlock_t): LongInt; cdecl;
  external libc name 'pthread_rwlock_tryrdlock';
function pthread_rwlock_timedrdlock(rwlock: Ppthread_rwlock_t; abs_timeout: ptimespec): LongInt; cdecl;
  external libc name 'pthread_rwlock_timedrdlock';

function pthread_rwlock_wrlock(rwlock: Ppthread_rwlock_t): LongInt; cdecl;
  external libc name 'pthread_rwlock_wrlock';
function pthread_rwlock_trywrlock(rwlock: Ppthread_rwlock_t): LongInt; cdecl;
  external libc name 'pthread_rwlock_trywrlock';
function pthread_rwlock_timedwrlock(rwlock: Ppthread_rwlock_t; abs_timeout: ptimespec): LongInt; cdecl;
  external libc name 'pthread_rwlock_timedwrlock';

function pthread_rwlock_unlock(rwlock: Ppthread_rwlock_t): LongInt; cdecl;
  external libc name 'pthread_rwlock_unlock';

function pthread_key_create(key: Ppthread_key_t; destructor_function: pthread_key_create_proc): LongInt; cdecl;
  external libc name 'pthread_key_create';

{ Original code was:
    int pthread_key_delete (pthread_key_t);

  But the following declaration seems consistent in other headers. }
function pthread_key_delete(key: pthread_key_t): LongInt; cdecl;
  external libc name 'pthread_key_delete';
function pthread_setspecific(key: pthread_key_t; value: Pointer): LongInt; cdecl;
  external libc name 'pthread_setspecific';
function pthread_getspecific(key: pthread_key_t): Pointer; cdecl;
  external libc name 'pthread_getspecific';

function pthread_kill(tid: pthread_t; sig: LongInt): LongInt; cdecl;
  external libc name 'pthread_kill';
function pthread_sigmask(how: LongInt; _set, oset: Psigset_t): LongInt; cdecl;
  external libc name 'pthread_sigmask';

function pthread_getcpuclockid(tid: pthread_t; clockid: Pclockid_t): LongInt; cdecl;
  external libc name 'pthread_getcpuclockid';

function pthread_once(once_control: Ppthread_once_t; init_routine: TProcedure): LongInt; cdecl;
  external libc name 'pthread_once';

function pthread_setname_np(thid: pthread_t; thname: PAnsiChar): LongInt; cdecl;
  external libc name 'pthread_setname_np';

implementation

end.
