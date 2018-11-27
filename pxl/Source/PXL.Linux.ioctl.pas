unit PXL.Linux.ioctl;
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
{
  "ioctl.h" translation to Pascal by Yuriy Kotsarenko, 2015.
  The original C header file was obtained at:
    http://www.cs.fsu.edu/~baker/devices/lxr/http/source/linux/include/asm-i386/ioctl.h?v=2.6.11.8
}
interface

{$INCLUDE PXL.Linux.config.inc}

{ ioctl command encoding: 32 bits total, command in lower 16 bits, size of the parameter structure in the lower 14 bits
  of the upper 16 bits. Encoding the size of the parameter structure in the ioctl request is useful for catching
  programs compiled with old versions and to avoid overwriting user space outside the user buffer area. The highest 2
  bits are reserved for indicating the ``access mode''. NOTE: This limits the max parameter size to 16kB -1 ! }

{ The following is for compatibility across the various Linux platforms. The i386 ioctl numbering scheme doesn't really
  enforce a type field. De facto, however, the top 8 bits of the lower 16 bits are indeed used as a type field, so we
  might just as well make this explicit here. Please be sure to use the decoding macros below from now on. }

const
  _IOC_NRBITS = 8;
  _IOC_TYPEBITS = 8;
  _IOC_SIZEBITS = 14;
  _IOC_DIRBITS = 2;

  _IOC_NRMASK = (1 shl _IOC_NRBITS) - 1;
  _IOC_TYPEMASK = (1 shl _IOC_TYPEBITS) - 1;
  _IOC_SIZEMASK = (1 shl _IOC_SIZEBITS) - 1;
  _IOC_DIRMASK = (1 shl _IOC_DIRBITS) - 1;

  _IOC_NRSHIFT = 0;
  _IOC_TYPESHIFT = _IOC_NRSHIFT + _IOC_NRBITS;
  _IOC_SIZESHIFT = _IOC_TYPESHIFT + _IOC_TYPEBITS;
  _IOC_DIRSHIFT = _IOC_SIZESHIFT + _IOC_SIZEBITS;

{ Direction bits. }
  _IOC_NONE = 0;
  _IOC_WRITE = 1;
  _IOC_READ = 2;

function _IOC(const dir, &type, nr, size: Integer): Integer; inline;

{ used to create numbers }
function _IO(const &type, nr: Integer): Integer; inline;
function _IOR(const &type, nr, size: Integer): Integer; inline;
function _IOW(const &type, nr, size: Integer): Integer; inline;
function _IOWR(const &type, nr, size: Integer): Integer; inline;

{ used to decode ioctl numbers }
function _IOC_DIR(const nr: Integer): Integer; inline;
function _IOC_TYPE(const nr: Integer): Integer; inline;
function _IOC_NR(const nr: Integer): Integer; inline;
function _IOC_SIZE(const nr: Integer): Integer; inline;

const
{ ...and for the drivers/sound files... }
  IOC_IN = _IOC_WRITE shl _IOC_DIRSHIFT;
  IOC_OUT = _IOC_READ shl _IOC_DIRSHIFT;
  IOC_INOUT = (_IOC_WRITE or _IOC_READ) shl _IOC_DIRSHIFT;
  IOCSIZE_MASK = _IOC_SIZEMASK shl _IOC_SIZESHIFT;
  IOCSIZE_SHIFT = _IOC_SIZESHIFT;

implementation

function _IOC(const dir, &type, nr, size: Integer): Integer;
begin
  Result := (dir shl _IOC_DIRSHIFT) or (&type shl _IOC_TYPESHIFT) or (nr shl _IOC_NRSHIFT) or (size shl _IOC_SIZESHIFT);
end;

function _IO(const &type, nr: Integer): Integer;
begin
  Result := _IOC(_IOC_NONE, &type, nr, 0);
end;

function _IOR(const &type, nr, size: Integer): Integer;
begin
  Result := _IOC(_IOC_READ, &type, nr, size);
end;

function _IOW(const &type, nr, size: Integer): Integer;
begin
  Result := _IOC(_IOC_WRITE, &type, nr, size);
end;

function _IOWR(const &type, nr, size: Integer): Integer;
begin
  Result := _IOC(_IOC_READ or _IOC_WRITE, &type, nr, size);
end;

function _IOC_DIR(const nr: Integer): Integer;
begin
  Result := (nr shr _IOC_DIRSHIFT) and _IOC_DIRMASK;
end;

function _IOC_TYPE(const nr: Integer): Integer;
begin
  Result := (nr shr _IOC_TYPESHIFT) and _IOC_TYPEMASK;
end;

function _IOC_NR(const nr: Integer): Integer;
begin
  Result := (nr shr _IOC_NRSHIFT) and _IOC_NRMASK;
end;

function _IOC_SIZE(const nr: Integer): Integer;
begin
  Result := (nr shr _IOC_SIZESHIFT) and _IOC_SIZEMASK;
end;

end.
