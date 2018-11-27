unit PXL.Sysfs.Buses;
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

{$IFDEF FPC}
  {$PACKRECORDS C}
  {$MODE DELPHI}
{$ELSE}
  {$ALIGN ON}
{$ENDIF}

//

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

// http://www.cs.fsu.edu/~baker/devices/lxr/http/source/linux/include/linux/spi/spidev.h
  SPI_CPHA = $01; // clock phase
  SPI_CPOL = $02; // clock polarity

  SPI_MODE_0 = 0 or 0;
  SPI_MODE_1 = 0 or SPI_CPHA;
  SPI_MODE_2 = SPI_CPOL or 0;
  SPI_MODE_3 = SPI_CPOL or SPI_CPHA;

  SPI_CS_HIGH = $04;   // chipselect active high?
  SPI_LSB_FIRST = $08; // per-word bits-on-wire
  SPI_3WIRE = $10;     // SI/SO signals shared
  SPI_LOOP = $20;      // loopback mode
  SPI_NO_CS = $40;     // 1 dev/bus, no chipselect
  SPI_READY = $80;     // slave pulls low to pause

  SPI_IOC_MAGIC = Ord('k');

type
  spi_ioc_transfer = record
    tx_buf: UInt64;
    rx_buf: UInt64;
    len: LongWord;
    speed_hz: LongWord;
    delay_usecs: Word;
    bits_per_word: Byte;
    cs_change: Byte;
    pad: LongWord;
  end;

function SPI_MSGSIZE(const N: Integer): Integer;
function SPI_IOC_MESSAGE(const N: Integer): Integer;

function SPI_IOC_RD_MODE: Integer;
function SPI_IOC_WR_MODE: Integer;

function SPI_IOC_RD_LSB_FIRST: Integer;
function SPI_IOC_WR_LSB_FIRST: Integer;
function SPI_IOC_RD_BITS_PER_WORD: Integer;
function SPI_IOC_WR_BITS_PER_WORD: Integer;
function SPI_IOC_RD_MAX_SPEED_HZ: Integer;
function SPI_IOC_WR_MAX_SPEED_HZ: Integer;

// http://i2c-tools.sourcearchive.com/documentation/3.0.3-5/i2c-dev_8h_source.html

{ I2C Message - used for pure i2c transaction, also from /dev interface }
type
  i2c_msg = record
    addr: Word;    // slave address
    flags: Word;
    len: SmallInt; // msg length
    buf: Pointer;  // pointer to msg data
  end;

const
  I2C_M_TEN = $10;  // we have a ten bit chip address
  I2C_M_RD = $01;
  I2C_M_NOSTART= $4000;
  I2C_M_REV_DIR_ADDR = $2000;
  I2C_M_IGNORE_NAK = $1000;
  I2C_M_NO_RD_ACK = $0800;

  { To determine what functionality is present }
  I2C_FUNC_I2C = $00000001;
  I2C_FUNC_10BIT_ADDR = $00000002;
  I2C_FUNC_PROTOCOL_MANGLING = $00000004; // I2C_M_{REV_DIR_ADDR,NOSTART,..}
  I2C_FUNC_SMBUS_PEC = $00000008;
  I2C_FUNC_SMBUS_BLOCK_PROC_CALL = $00008000; // SMBus 2.0
  I2C_FUNC_SMBUS_QUICK = $00010000;
  I2C_FUNC_SMBUS_READ_BYTE = $00020000;
  I2C_FUNC_SMBUS_WRITE_BYTE = $00040000;
  I2C_FUNC_SMBUS_READ_BYTE_DATA = $00080000;
  I2C_FUNC_SMBUS_WRITE_BYTE_DATA = $00100000;
  I2C_FUNC_SMBUS_READ_WORD_DATA = $00200000;
  I2C_FUNC_SMBUS_WRITE_WORD_DATA = $00400000;
  I2C_FUNC_SMBUS_PROC_CALL = $00800000;
  I2C_FUNC_SMBUS_READ_BLOCK_DATA = $01000000;
  I2C_FUNC_SMBUS_WRITE_BLOCK_DATA = $02000000;
  I2C_FUNC_SMBUS_READ_I2C_BLOCK = $04000000; // I2C-like block xfer
  I2C_FUNC_SMBUS_WRITE_I2C_BLOCK = $08000000; // w/ 1-byte reg. addr.

  I2C_FUNC_SMBUS_BYTE = I2C_FUNC_SMBUS_READ_BYTE or I2C_FUNC_SMBUS_WRITE_BYTE;
  I2C_FUNC_SMBUS_BYTE_DATA = I2C_FUNC_SMBUS_READ_BYTE_DATA or I2C_FUNC_SMBUS_WRITE_BYTE_DATA;
  I2C_FUNC_SMBUS_WORD_DATA =  I2C_FUNC_SMBUS_READ_WORD_DATA or I2C_FUNC_SMBUS_WRITE_WORD_DATA;
  I2C_FUNC_SMBUS_BLOCK_DATA = I2C_FUNC_SMBUS_READ_BLOCK_DATA or I2C_FUNC_SMBUS_WRITE_BLOCK_DATA;
  I2C_FUNC_SMBUS_I2C_BLOCK = I2C_FUNC_SMBUS_READ_I2C_BLOCK or I2C_FUNC_SMBUS_WRITE_I2C_BLOCK;

  { Old name, for compatibility }
  I2C_FUNC_SMBUS_HWPEC_CALC = I2C_FUNC_SMBUS_PEC;

  { Data for SMBus Messages }
  I2C_SMBUS_BLOCK_MAX = 32; // As specified in SMBus standard
  I2C_SMBUS_I2C_BLOCK_MAX = 32; // Not specified but we use same structure

type
  Pi2c_smbus_data = ^i2c_smbus_data;
  i2c_smbus_data = record
  case Integer of
    0: (_byte: Byte);
    1: (_word: Word);
    2: (block: array[0..I2C_SMBUS_BLOCK_MAX + 1] of Byte); // block[0] is used for length
                                                           // and one more for PEC
  end;

const
  { smbus_access read or write markers }
  I2C_SMBUS_READ = 1;
  I2C_SMBUS_WRITE = 0;

  { SMBus transaction types (size parameter in the above functions)
    Note: these no longer correspond to the (arbitrary) PIIX4 internal codes! }
  I2C_SMBUS_QUICK = 0;
  I2C_SMBUS_BYTE = 1;
  I2C_SMBUS_BYTE_DATA = 2;
  I2C_SMBUS_WORD_DATA = 3;
  I2C_SMBUS_PROC_CALL = 4;
  I2C_SMBUS_BLOCK_DATA = 5;
  I2C_SMBUS_I2C_BLOCK_BROKEN = 6;
  I2C_SMBUS_BLOCK_PROC_CALL = 7; // SMBus 2.0
  I2C_SMBUS_I2C_BLOCK_DATA = 8;

  { commands for the ioctl like i2c_command call: note that additional calls are defined in the algorithm and hw
    dependent layers - these can be listed here, or see the corresponding header files. }
  // -> bit-adapter specific ioctls
  I2C_RETRIES = $0701; // number of times a device address
                       // should be polled when not acknowledging
  I2C_TIMEOUT = $0702; // set timeout - call with int

  // this is for i2c-dev.c
  I2C_SLAVE = $0703; // Change slave address
                     // Attn.: Slave address is 7 or 10 bits
  I2C_SLAVE_FORCE = $0706; // Change slave address
                           // Attn.: Slave address is 7 or 10 bits
                           // This changes the address, even if it is already taken!
  I2C_TENBIT = $0704; // 0 for 7 bit addrs, != 0 for 10 bit

  I2C_FUNCS = $0705; // Get the adapter functionality
  I2C_RDWR = $0707; // Combined R/W transfer (one stop only)
  I2C_PEC = $0708; // != 0 for SMBus PEC

  I2C_SMBUS = $0720; // SMBus-level access

  // Note: 10-bit addresses are NOT supported!

type
  // This is the structure as used in the I2C_SMBUS ioctl call
  i2c_smbus_ioctl_data = record
    read_write: Byte;
    command: Byte;
    size: Integer;
    data: Pi2c_smbus_data;
  end;

  // This is the structure as used in the I2C_RDWR ioctl call
  i2c_rdwr_ioctl_data = record
    msgs: ^i2c_msg; // pointers to i2c_msgs
    nmsgs: Integer; // number of i2c_msgs
  end;

function i2c_smbus_access(_file: Integer; read_write, command: Byte; size: Integer;
  data: Pi2c_smbus_data): Integer; inline;
function i2c_smbus_write_quick(_file: Integer; value: Byte): Integer; inline;
function i2c_smbus_read_byte(_file: Integer): Integer; inline;
function i2c_smbus_write_byte(_file: Integer; value: Byte): Integer; inline;
function i2c_smbus_read_byte_data(_file: Integer; command: Byte): Integer; inline;
function i2c_smbus_write_byte_data(_file: Integer; command, value: Byte): Integer; inline;
function i2c_smbus_read_word_data(_file: Integer; command: Byte): Integer; inline;
function i2c_smbus_write_word_data(_file: Integer; command: Byte; value: Word): Integer; inline;
function i2c_smbus_process_call(_file: Integer; command: Byte; value: Word): Integer; inline;

// Returns the number of read bytes
function i2c_smbus_read_block_data(_file: Integer; command: Byte; values: Pointer): Integer; inline;
function i2c_smbus_write_block_data(_file: Integer; command, length: Byte; values: Pointer): Integer; inline;

// Returns the number of read bytes
{ Until kernel 2.6.22, the length is hardcoded to 32 bytes. If you ask for less than 32 bytes, your code will only work
  with kernels 2.6.23 and later. }
function i2c_smbus_read_i2c_block_data(_file: Integer; command, length: Byte; values: Pointer): Integer; inline;
function i2c_smbus_write_i2c_block_data(_file: Integer; command, length: Byte; values: Pointer): Integer; inline;

// Returns the number of read bytes
function i2c_smbus_block_process_call(_file: Integer; command, length: Byte; values: Pointer): Integer; inline;

implementation

uses
  BaseUnix;

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

function SPI_MSGSIZE(const N: Integer): Integer;
begin
  if N * SizeOf(spi_ioc_transfer) < 1 shl _IOC_SIZEBITS then
    Result := N * SizeOf(spi_ioc_transfer)
  else
    Result := 0;
end;

function SPI_IOC_MESSAGE(const N: Integer): Integer;
begin
  Result := _IOW(SPI_IOC_MAGIC, 0, Byte(SPI_MSGSIZE(N)));
end;

function SPI_IOC_RD_MODE: Integer;
begin
  Result := _IOR(SPI_IOC_MAGIC, 1, SizeOf(Byte));
end;

function SPI_IOC_WR_MODE: Integer;
begin
  Result := _IOW(SPI_IOC_MAGIC, 1, SizeOf(Byte));
end;

function SPI_IOC_RD_LSB_FIRST: Integer;
begin
  Result := _IOR(SPI_IOC_MAGIC, 2, SizeOf(Byte));
end;

function SPI_IOC_WR_LSB_FIRST: Integer;
begin
  Result := _IOW(SPI_IOC_MAGIC, 2, SizeOf(Byte));
end;

function SPI_IOC_RD_BITS_PER_WORD: Integer;
begin
  Result := _IOR(SPI_IOC_MAGIC, 3, SizeOf(Byte));
end;

function SPI_IOC_WR_BITS_PER_WORD: Integer;
begin
  Result := _IOW(SPI_IOC_MAGIC, 3, SizeOf(Byte));
end;

function SPI_IOC_RD_MAX_SPEED_HZ: Integer;
begin
  Result := _IOR(SPI_IOC_MAGIC, 4, SizeOf(LongWord));
end;

function SPI_IOC_WR_MAX_SPEED_HZ: Integer;
begin
  Result := _IOW(SPI_IOC_MAGIC, 4, SizeOf(LongWord));
end;

function i2c_smbus_access(_file: Integer; read_write, command: Byte; size: Integer; data: Pi2c_smbus_data): Integer;
var
  args: i2c_smbus_ioctl_data;
begin
  args.read_write := read_write;
  args.command := command;
  args.size := size;
  args.data := data;
  Result := fpioctl(_file, I2C_SMBUS, @args);
end;

function i2c_smbus_write_quick(_file: Integer; value: Byte): Integer;
begin
  Result := i2c_smbus_access(_file, value, 0, I2C_SMBUS_QUICK, nil);
end;

function i2c_smbus_read_byte(_file: Integer): Integer;
var
  data: i2c_smbus_data;
begin
  if i2c_smbus_access(_file, I2C_SMBUS_READ, 0, I2C_SMBUS_BYTE, @data) = 0 then
    Result := data._byte
  else
    Result := -1;
end;

function i2c_smbus_write_byte(_file: Integer; value: Byte): Integer;
begin
  Result := i2c_smbus_access(_file, I2C_SMBUS_WRITE, value, I2C_SMBUS_BYTE, nil);
end;

function i2c_smbus_read_byte_data(_file: Integer; command: Byte): Integer;
var
  data: i2c_smbus_data;
begin
  if i2c_smbus_access(_file, I2C_SMBUS_READ, command, I2C_SMBUS_BYTE_DATA, @data) = 0 then
    Result := data._byte
  else
    Result := -1;
end;

function i2c_smbus_write_byte_data(_file: Integer; command, value: Byte): Integer;
var
  data: i2c_smbus_data;
begin
  data._byte := value;
  Result := i2c_smbus_access(_file, I2C_SMBUS_WRITE, command, I2C_SMBUS_BYTE_DATA, @data);
end;

function i2c_smbus_read_word_data(_file: Integer; command: Byte): Integer;
var
  data: i2c_smbus_data;
begin
  if i2c_smbus_access(_file, I2C_SMBUS_READ, command, I2C_SMBUS_WORD_DATA, @data) = 0 then
    Result := data._word
  else
    Result := -1;
end;

function i2c_smbus_write_word_data(_file: Integer; command: Byte; value: Word): Integer;
var
  data: i2c_smbus_data;
begin
  data._word := value;
  Result := i2c_smbus_access(_file, I2C_SMBUS_WRITE, command, I2C_SMBUS_WORD_DATA, @data);
end;

function i2c_smbus_process_call(_file: Integer; command: Byte; value: Word): Integer;
var
  data: i2c_smbus_data;
begin
  data._word := value;
  if i2c_smbus_access(_file, I2C_SMBUS_WRITE, command, I2C_SMBUS_PROC_CALL, @data) = 0 then
    Result := data._word
  else
    Result := -1;
end;

// Returns the number of read bytes
function i2c_smbus_read_block_data(_file: Integer; command: Byte; values: Pointer): Integer;
var
  data: i2c_smbus_data;
begin
  if i2c_smbus_access(_file, I2C_SMBUS_READ, command, I2C_SMBUS_BLOCK_DATA, @data) = 0 then
  begin
    if data.block[0] > 0 then
      Move(data.block[1], values^, data.block[0]);
    Result := data.block[0];
  end
  else
    Result := -1;
end;

function i2c_smbus_write_block_data(_file: Integer; command, length: Byte; values: Pointer): Integer;
var
  data: i2c_smbus_data;
begin
  if length > I2C_SMBUS_I2C_BLOCK_MAX then
    length := I2C_SMBUS_I2C_BLOCK_MAX;
  if length > 0 then
    Move(values^, data.block[1], length);
  data.block[0] := length;
  Result := i2c_smbus_access(_file, I2C_SMBUS_WRITE, command, I2C_SMBUS_BLOCK_DATA, @data);
end;

// Returns the number of read bytes
{ Until kernel 2.6.22, the length is hardcoded to 32 bytes. If you ask for less than 32 bytes, your code will only work
  with kernels 2.6.23 and later. }
function i2c_smbus_read_i2c_block_data(_file: Integer; command, length: Byte; values: Pointer): Integer;
var
  data: i2c_smbus_data;
  size: Integer;
begin
  if length > I2C_SMBUS_I2C_BLOCK_MAX then
    length := I2C_SMBUS_I2C_BLOCK_MAX;
  data.block[0] := length;
  if length = I2C_SMBUS_I2C_BLOCK_MAX then
    size := I2C_SMBUS_I2C_BLOCK_BROKEN
  else
    size := I2C_SMBUS_I2C_BLOCK_DATA;
  if i2c_smbus_access(_file, I2C_SMBUS_READ, command, size, @data) = 0 then
  begin
    if data.block[0] > 0 then
      Move(data.block[1], values^, data.block[0]);
    Result := data.block[0];
  end
  else
    Result := -1;
end;

function i2c_smbus_write_i2c_block_data(_file: Integer; command, length: Byte; values: Pointer): Integer;
var
  data: i2c_smbus_data;
begin
  if length > I2C_SMBUS_I2C_BLOCK_MAX then
    length := I2C_SMBUS_I2C_BLOCK_MAX;
  if length > 0 then
    Move(values^, data.block[1], length);
  data.block[0] := length;
  Result := i2c_smbus_access(_file, I2C_SMBUS_WRITE, command, I2C_SMBUS_I2C_BLOCK_BROKEN, @data);
end;

// Returns the number of read bytes
function i2c_smbus_block_process_call(_file: Integer; command, length: Byte; values: Pointer): Integer;
var
  data: i2c_smbus_data;
begin
  if length > I2C_SMBUS_I2C_BLOCK_MAX then
    length := I2C_SMBUS_I2C_BLOCK_MAX;
  if length > 0 then
    Move(values^, data.block[1], length);
  data.block[0] := length;
  if i2c_smbus_access(_file, I2C_SMBUS_WRITE, command, I2C_SMBUS_BLOCK_PROC_CALL, @data) = 0 then
  begin
    if data.block[0] > 0 then
      Move(data.block[1], values^, data.block[0]);
    Result := data.block[0];
  end
  else
    Result := -1;
end;

end.
