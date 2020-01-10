unit android.better_pthread;

interface

{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2019 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

uses
  classes, types, sysutils;

type
  size_t = nativeint;
  timer_t = {$IFDEF ANDROID32}Integer{$ENDIF ANDROID32}{$IFDEF ANDROID64}Pointer{$ENDIF ANDROID64};
  {$EXTERNALSYM timer_t}
  Ptimer_t = ^timer_t;

  blkcnt_t = {$IFDEF ANDROID32}UInt32{$ENDIF ANDROID32}{$IFDEF ANDROID64}LongWord{$ENDIF ANDROID64};
  {$EXTERNALSYM blkcnt_t}
  Pblkcnt_t = ^blkcnt_t;

  blksize_t = {$IFDEF ANDROID32}UInt32{$ENDIF ANDROID32}{$IFDEF ANDROID64}LongWord{$ENDIF ANDROID64};
  {$EXTERNALSYM blksize_t}
  Pblksize_t = ^blksize_t;

  clock_t = {$IFDEF ANDROID32}Int32{$ENDIF ANDROID32}{$IFDEF ANDROID64}LongWord{$ENDIF ANDROID64};
  {$EXTERNALSYM clock_t}
  Pclock_t = ^clock_t;

  clockid_t = {$IFDEF ANDROID32}Integer{$ENDIF ANDROID32}{$IFDEF ANDROID64}Int32{$ENDIF ANDROID64};
  {$EXTERNALSYM clockid_t}
  Pclockid_t = ^clockid_t;

  dev_t = {$IFDEF ANDROID32}UInt32{$ENDIF ANDROID32}{$IFDEF ANDROID64}LongWord{$ENDIF ANDROID64};
  {$EXTERNALSYM dev_t}
  Pdev_t = ^dev_t;

  fsblkcnt_t = {$IFDEF ANDROID32}UInt32{$ENDIF ANDROID32}{$IFDEF ANDROID64}LongWord{$ENDIF ANDROID64};
  {$EXTERNALSYM fsblkcnt_t}
  Pfsblkcnt_t = ^fsblkcnt_t;

  fsfilcnt_t = {$IFDEF ANDROID32}UInt32{$ENDIF ANDROID32}{$IFDEF ANDROID64}LongWord{$ENDIF ANDROID64};
  {$EXTERNALSYM fsfilcnt_t}
  Pfsfilcnt_t = ^fsfilcnt_t;

  gid_t = UInt32;
  {$EXTERNALSYM gid_t}
  Pgid_t = ^gid_t;

  id_t = UInt32;
  {$EXTERNALSYM id_t}
  _Pid_t = ^id_t;

  ino_t = {$IFDEF ANDROID32}Integer{$ENDIF ANDROID32}{$IFDEF ANDROID64}LongWord{$ENDIF ANDROID64};
  {$EXTERNALSYM ino_t}
  Pino_t = ^ino_t;

  key_t = {$IFDEF ANDROID32}Integer{$ENDIF ANDROID32}{$IFDEF ANDROID64}Int32{$ENDIF ANDROID64};
  {$EXTERNALSYM key_t}
  Pkey_t = ^key_t;

{$IFDEF ANDROID32}
  mode_t = Word;
  {$EXTERNALSYM mode_t}
{$ENDIF ANDROID32}
{$IFDEF ANDROID64}
  {$IFDEF CPU64BITS}
  mode_t = UInt32;
  {$ELSE !CPU64BITS}
  mode_t = UInt16;
  {$ENDIF CPU64BITS}
  {$EXTERNALSYM mode_t}
{$ENDIF ANDROID64}
  Pmode_t = ^mode_t;

  nlink_t = {$IFDEF ANDROID32}Word{$ENDIF ANDROID32}{$IFDEF ANDROID64}UInt32{$ENDIF ANDROID64};
  {$EXTERNALSYM nlink_t}
  Pnlink_t = ^nlink_t;

{$IFNDEF _OFF_T_DEFINED_}
{$DEFINE _OFF_T_DEFINED_}
  off_t = Longint;
  {$EXTERNALSYM off_t}
  Poff_t = ^off_t;

  off64_t = Int64;
  {$EXTERNALSYM off64_t}
  Poff64_t = ^off64_t;
{$ENDIF}

  pid_t = {$IFDEF ANDROID32}Integer{$ENDIF ANDROID32}{$IFDEF ANDROID64}Int32{$ENDIF ANDROID64};
  {$EXTERNALSYM pid_t}
  Ppid_t = ^pid_t;

{$IFDEF ANDROID32}
  {$IFDEF CPU64BITS}
  ssize_t = Int64;
  {$ELSE}
  ssize_t = Integer;
  {$ENDIF}
  {$EXTERNALSYM ssize_t}
{$ENDIF ANDROID32}
{$IFDEF ANDROID64}
  ssize_t = Longint;
  {$EXTERNALSYM ssize_t}
{$ENDIF ANDROID64}
  Pssize_t = ^ssize_t;

{$IFDEF ANDROID32}
  uid_t = UInt32;
  {$EXTERNALSYM uid_t}
{$ENDIF ANDROID32}
{$IFDEF ANDROID64}
  {$IFDEF CPU64BITS}
  uid_t = UInt32;
  {$ELSE !CPU64BITS}
  uid_t = UInt16;
  {$ENDIF CPU64BITS}
  {$EXTERNALSYM uid_t}
{$ENDIF ANDROID64}
  Puid_t = ^uid_t;

  suseconds_t = {$IFDEF ANDROID32}LongInt{$ENDIF ANDROID32}{$IFDEF ANDROID64}NativeInt{$ENDIF ANDROID64};
  {$EXTERNALSYM suseconds_t}
  Psuseconds_t = ^suseconds_t;

{ PThreads Support }

type

 uint32_t = UInt32;
 {$EXTERNALSYM uint32_t}
 int32_t = Int32;
 {$EXTERNALSYM int32_t}

{$IFNDEF _PTHREAD_ATTR_T_DEFINED}
{$IFDEF ANDROID32}
  pthread_attr_t = record
    flags: uint32_t;
    stack_base: Pointer;
    stack_size: size_t;
    guard_size: size_t;
    sched_policy: int32_t;
    sched_priority: int32_t;
  end;
  {$EXTERNALSYM pthread_attr_t}
{$ENDIF ANDROID32}
{$IFDEF ANDROID64}
  pthread_attr_t = record
    flags: uint32_t;
    stack_base: Pointer;
    stack_size: size_t;
    guard_size: size_t;
    sched_policy: int32_t;
    sched_priority: int32_t;
{$IFDEF CPU64BITS}
    __reserved: array[1..16] of byte;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_attr_t}
{$ENDIF ANDROID64}
{$ENDIF _PTHREAD_ATTR_T_DEFINED}
  Ppthread_attr_t = ^pthread_attr_t;

{$IFDEF ANDROID32}
  pthread_cond_t = record
    value: Integer;
  end;
  {$EXTERNALSYM pthread_cond_t}
{$ENDIF ANDROID32}
{$IFDEF ANDROID64}
  pthread_cond_t = record
{$IFDEF CPU64BITS}
    value: array [1..12] of int32_t;
{$ELSE !CPU64BITS}
    value: int32_t;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_cond_t}
{$ENDIF ANDROID64}

  pthread_condattr_t = {$IFDEF ANDROID32}Integer{$ENDIF ANDROID32}{$IFDEF ANDROID64}NativeInt{$ENDIF ANDROID64};
  {$EXTERNALSYM pthread_condattr_t}
  Ppthread_condattr_t = ^pthread_condattr_t;

  pthread_key_t = {$IFDEF ANDROID32}Integer{$ENDIF ANDROID32}{$IFDEF ANDROID64}Int32{$ENDIF ANDROID64};
  {$EXTERNALSYM pthread_key_t}

{$IFDEF ANDROID32}
  pthread_mutex_t = record
    value: Integer;
  end;
  {$EXTERNALSYM pthread_mutex_t}
{$ENDIF ANDROID32}
{$IFDEF ANDROID64}
  pthread_mutex_t = record
{$IFDEF CPU64BITS}
    value: array[1..10] of int32_t;
{$ELSE !CPU64BITS}
    value: int32_t;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_mutex_t}
{$ENDIF ANDROID64}

  pthread_once_t = {$IFDEF ANDROID32}Integer{$ENDIF ANDROID32}{$IFDEF ANDROID64}Int32{$ENDIF ANDROID64};
  {$EXTERNALSYM pthread_once_t}

{$IFDEF ANDROID32}
  pthread_rwlock_t = record
    lock: pthread_mutex_t;
    cond: pthread_cond_t;
    numLocks: integer;
    writerThreadId: integer;
    pendingReaders: integer;
    pendingWriters: integer;
    reserved: array[0..3] of Pointer;  //* for future extensibility */
  end;
  {$EXTERNALSYM pthread_rwlock_t}
{$ENDIF ANDROID32}
{$IFDEF ANDROID64}
  pthread_rwlock_t = record
{$IFDEF CPU64BITS}
    __private: array[1..14] of int32_t;
{$ELSE !CPU64BITS}
    __private: array[1..10] of int32_t;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_rwlock_t}
{$ENDIF ANDROID64}



implementation

end.
