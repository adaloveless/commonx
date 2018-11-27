unit PXL.Windows.UART;
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

uses
  SysUtils, PXL.TypeDef, PXL.Boards.Types;

type
  TWinUART = class(TCustomPortUART)
  public const
    MaxSupportedBaudRate = 115200;
  private
    FSystemPath: StdString;
    FHandle: TUntypedHandle;

    FBaudRate: Integer;
    FBitsPerWord: Integer;
    FParity: TParity;
    FStopBits: TStopBits;

    procedure UpdateCommState;
  protected
    function GetBaudRate: Integer; override;
    procedure SetBaudRate(const Value: Integer); override;
    function GetBitsPerWord: Integer; override;
    procedure SetBitsPerWord(const Value: Integer); override;
    function GetParity: TParity; override;
    procedure SetParity(const Value: TParity); override;
    function GetStopBits: TStopBits; override;
    procedure SetStopBits(const Value: TStopBits); override;
  public
    constructor Create(const ASystemPath: StdString); // e.g. "\\.\COM1"
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    procedure Flush; override;

    property SystemPath: StdString read FSystemPath;
    property Handle: TUntypedHandle read FHandle;

    property BaudRate: Integer read FBaudRate write SetBaudRate;
    property BitsPerWord: Integer read FBitsPerWord write SetBitsPerWord;
    property Parity: TParity read FParity write SetParity;
    property StopBits: TStopBits read FStopBits write SetStopBits;
  end;

  EWinUARTGeneric = class(Exception);

  EWinUARTInvalidParams = class(EWinUARTGeneric);
  EWinUARTOpen = class(EWinUARTGeneric);
  EWinUARTFlush = class(EWinUARTGeneric);

  EWinUARTCommState = class(EWinUARTGeneric);
  EWinUARTSetCommState = class(EWinUARTCommState);
  EWinUARTSetCommTimeouts = class(EWinUARTCommState);

resourcestring
  SCannotOpenFileForUART = 'Cannot open UART file <%s> for reading and writing.';
  SCannotSetCommState = 'Cannot set COMM state for UART (%s).';
  SCannotSetCommTimeouts = 'Cannot set COMM timeouts for UART (%s).';
  SInvalidParameters = 'The specified parameters are invalid.';
  SCannotFlushUARTBuffers = 'Cannot flush UART buffers.';

implementation

uses
  Windows;

const
  DCB_BINARY = $1;
  DCB_PARITY = $2;

constructor TWinUART.Create(const ASystemPath: StdString);
begin
  inherited Create;

  FSystemPath := ASystemPath;

  FHandle := CreateFile(PStdChar(FSystemPath), GENERIC_WRITE or GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
  if FHandle = TUntypedHandle(INVALID_HANDLE_VALUE) then
    raise EWinUARTOpen.CreateFmt(SCannotOpenFileForUART, [FSystemPath]);

  FBaudRate := MaxSupportedBaudRate;
  FBitsPerWord := 8;

  UpdateCommState;
end;

destructor TWinUART.Destroy;
begin
  if FHandle <> TUntypedHandle(INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(FHandle);
    FHandle := TUntypedHandle(INVALID_HANDLE_VALUE);
  end;

  inherited;
end;

procedure TWinUART.UpdateCommState;
var
  DCB: TDCB;
  Timeouts: TCommTimeouts;
begin
  FillChar(DCB, SizeOf(TDCB), 0);

  DCB.DCBlength := SizeOf(TDCB);
  DCB.BaudRate := FBaudRate;
  DCB.flags := DCB_BINARY;
  DCB.ByteSize := FBitsPerWord;
  DCB.XonChar := #17;
  DCB.XoffChar := #19;

  if FParity <> TParity.None then
  begin
    DCB.Flags := DCB.Flags or DCB_PARITY;
    DCB.Parity := Ord(FParity);
  end;

  case FStopBits of
    TStopBits.One:
      DCB.StopBits := ONESTOPBIT;

    TStopBits.OneDotFive:
      DCB.StopBits := ONE5STOPBITS;

    TStopBits.Two:
      DCB.StopBits := TWOSTOPBITS;
  end;

  if not SetCommState(FHandle, DCB) then
    raise EWinUARTSetCommState.CreateFmt(SCannotSetCommState, [FSystemPath]);

  FillChar(Timeouts, SizeOf(TCommTimeouts), 0);
  Timeouts.ReadIntervalTimeout := High(LongWord);

  if not SetCommTimeouts(FHandle, Timeouts) then
    raise EWinUARTSetCommTimeouts.CreateFmt(SCannotSetCommTimeouts, [FSystemPath]);
end;

function TWinUART.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

procedure TWinUART.SetBaudRate(const Value: Integer);
begin
  if FBaudRate <> Value then
  begin
    FBaudRate := Value;
    UpdateCommState;
  end;
end;

function TWinUART.GetBitsPerWord: Integer;
begin
  Result := FBitsPerWord;
end;

procedure TWinUART.SetBitsPerWord(const Value: Integer);
begin
  if FBitsPerWord <> Value then
  begin
    FBitsPerWord := Value;
    UpdateCommState;
  end;
end;

function TWinUART.GetParity: TParity;
begin
  Result := FParity;
end;

procedure TWinUART.SetParity(const Value: TParity);
begin
  if FParity <> Value then
  begin
    FParity := Value;
    UpdateCommState;
  end;
end;

function TWinUART.GetStopBits: TStopBits;
begin
  Result := FStopBits;
end;

procedure TWinUART.SetStopBits(const Value: TStopBits);
begin
  if FStopBits <> Value then
  begin
    FStopBits := Value;
    UpdateCommState;
  end;
end;

function TWinUART.Read(const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  BytesRead: Cardinal;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise EWinUARTInvalidParams.Create(SInvalidParameters);

  if not ReadFile(FHandle, Buffer^, BufferSize, BytesRead, nil) then
    Exit(0);

  Result := BytesRead;
end;

function TWinUART.Write(const Buffer: Pointer; const BufferSize: Integer): Integer;
var
  BytesWritten: Cardinal;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise EWinUARTInvalidParams.Create(SInvalidParameters);

  if not WriteFile(FHandle, Buffer^, BufferSize, BytesWritten, nil) then
    Exit(0);

  Result := BytesWritten;
end;

procedure TWinUART.Flush;
begin
  if not FlushFileBuffers(FHandle) then
    raise EWinUARTFlush.Create(SCannotFlushUARTBuffers);
end;

end.
