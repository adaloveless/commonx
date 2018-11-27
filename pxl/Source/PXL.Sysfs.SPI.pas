unit PXL.Sysfs.SPI;
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
  PXL.TypeDef, PXL.Boards.Types, PXL.Sysfs.Buses, PXL.Sysfs.Types;

type
  TSysfsSPI = class(TCustomPortSPI)
  public const
    DefaultFrequency = 8000000;
    DefaultMode = SPI_MODE_0;
    DefaultBitsPerWord = 8;
  private
    FSystemPath: StdString;
    FHandle: TUntypedHandle;

    FMode: Integer;
    FBitsPerWord: Integer;
    FFrequency: Integer;
    FChipSelectAttributes: TChipSelectAttributes;

    procedure UpdateRWMode;
  protected
    function GetMode: Integer; override;
    procedure SetMode(const Value: Integer); override;
    function GetBitsPerWord: Integer; override;
    procedure SetBitsPerWord(const Value: Integer); override;
    function GetFrequency: Integer; override;
    procedure SetFrequency(const Value: Integer); override;
    function GetChipSelectAttributes: TChipSelectAttributes; override;
    procedure SetChipSelectAttributes(const Value: TChipSelectAttributes); override;
  public
    constructor Create(const ASystemPath: StdString; const AFrequency: Integer = DefaultFrequency;
      const AMode: Integer = DefaultMode; const ABitsPerWord: Integer = DefaultBitsPerWord);
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Integer): Integer; override;

    property SystemPath: StdString read FSystemPath;
    property Handle: TUntypedHandle read FHandle;

    property Mode: Integer read FMode write SetMode;
    property BitsPerWord: Integer read FBitsPerWord write SetBitsPerWord;
    property Frequency: Integer read FFrequency write SetFrequency;
    property ChipSelectAttributes: TChipSelectAttributes read FChipSelectAttributes write SetChipSelectAttributes;
  end;

  ESysfsSPIOpen = class(ESysfsFileOpen);

  ESysfsSPIMode = class(ESysfsGeneric);
  ESysfsSPIWriteMode = class(ESysfsSPIMode);
  ESysfsSPIReadMode = class(ESysfsSPIMode);

  ESysfsSPIBitsPerWord = class(ESysfsGeneric);
  ESysfsSPIWriteBitsPerWord = class(ESysfsSPIBitsPerWord);
  ESysfsSPIReadBitsPerWord = class(ESysfsSPIBitsPerWord);

  ESysfsSPIFrequency = class(ESysfsGeneric);
  ESysfsSPIWriteFrequency = class(ESysfsSPIBitsPerWord);
  ESysfsSPIReadFrequency = class(ESysfsSPIBitsPerWord);

  ESysfsSPITransfer = class(ESysfsGeneric);

resourcestring
  SCannotOpenFileForSPI = 'Cannot open SPI file <%s> for reading and writing.';
  SCannotSetSPIWriteMode = 'Cannot set SPI write mode to <%d>.';
  SCannotSetSPIReadMode = 'Cannot set SPI read mode to <%d>.';
  SCannotSetSPIWriteBitsPerWord = 'Cannot set SPI write bits per word to <%d>.';
  SCannotSetSPIReadBitsPerWord = 'Cannot set SPI read bits per word to <%d>.';
  SCannotSetSPIWriteFrequency = 'Cannot set SPI write frequency to <%d>.';
  SCannotSetSPIReadFrequency = 'Cannot set SPI read frequency to <%d>.';
  SCannotSPITransferBytes = 'Cannot transfer <%d> data byte(s) through SPI bus.';

implementation

uses
  SysUtils, BaseUnix;

constructor TSysfsSPI.Create(const ASystemPath: StdString; const AFrequency, AMode, ABitsPerWord: Integer);
begin
  inherited Create;

  FSystemPath := ASystemPath;

  FMode := -1;
  FBitsPerWord := -1;
  FFrequency := -1;

  FHandle := fpopen(FSystemPath, O_RDWR);
  if FHandle < 0 then
  begin
    FHandle := 0;
    raise ESysfsSPIOpen.Create(Format(SCannotOpenFileForSPI, [FSystemPath]));
  end;

  SetMode(AMode);
  SetBitsPerWord(ABitsPerWord);
  SetFrequency(AFrequency);
end;

destructor TSysfsSPI.Destroy;
begin
  if FHandle <> 0 then
  begin
    fpclose(FHandle);
    FHandle := 0;
  end;

  inherited;
end;

procedure TSysfsSPI.UpdateRWMode;
var
  ModeValue, Param: Byte;
begin
  ModeValue := FMode;
  if TChipSelectAttribute.ActiveHigh in FChipSelectAttributes then
    ModeValue := ModeValue or SPI_CS_HIGH;
  if TChipSelectAttribute.Disable in FChipSelectAttributes then
    ModeValue := ModeValue or SPI_NO_CS;

  Param := ModeValue;

  if fpioctl(FHandle, SPI_IOC_WR_MODE, @Param) < 0 then
    raise ESysfsSPIWriteMode.Create(Format(SCannotSetSPIWriteMode, [FMode]));

  Param := ModeValue;

  if fpioctl(FHandle, SPI_IOC_RD_MODE, @Param) < 0 then
    raise ESysfsSPIReadMode.Create(Format(SCannotSetSPIReadMode, [FMode]));
end;

function TSysfsSPI.GetMode: Integer;
begin
  Result := FMode;
end;

procedure TSysfsSPI.SetMode(const Value: Integer);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    UpdateRWMode;
  end;
end;

function TSysfsSPI.GetBitsPerWord: Integer;
begin
  Result := FBitsPerWord;
end;

procedure TSysfsSPI.SetBitsPerWord(const Value: Integer);
var
  BitsValue: Byte;
begin
  if FBitsPerWord <> Value then
  begin
    FBitsPerWord := Value;

    BitsValue := FBitsPerWord;

    if fpioctl(FHandle, SPI_IOC_WR_BITS_PER_WORD, @BitsValue) < 0 then
      raise ESysfsSPIWriteBitsPerWord.Create(Format(SCannotSetSPIWriteBitsPerWord, [FBitsPerWord]));

    BitsValue := FBitsPerWord;

    if fpioctl(FHandle, SPI_IOC_RD_BITS_PER_WORD, @BitsValue) < 0 then
      raise ESysfsSPIReadBitsPerWord.Create(Format(SCannotSetSPIReadBitsPerWord, [FBitsPerWord]));
  end;
end;

function TSysfsSPI.GetFrequency: Integer;
begin
  Result := FFrequency;
end;

procedure TSysfsSPI.SetFrequency(const Value: Integer);
var
  FrequencyValue: LongWord;
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;

    FrequencyValue := FFrequency;

    if fpioctl(FHandle, SPI_IOC_WR_MAX_SPEED_HZ, @FrequencyValue) < 0 then
      raise ESysfsSPIWriteFrequency.Create(Format(SCannotSetSPIWriteFrequency, [FFrequency]));

    FrequencyValue := FFrequency;

    if fpioctl(FHandle, SPI_IOC_RD_MAX_SPEED_HZ, @FrequencyValue) < 0 then
      raise ESysfsSPIReadFrequency.Create(Format(SCannotSetSPIReadFrequency, [FFrequency]));
  end;
end;

function TSysfsSPI.GetChipSelectAttributes: TChipSelectAttributes;
begin
  Result := FChipSelectAttributes;
end;

procedure TSysfsSPI.SetChipSelectAttributes(const Value: TChipSelectAttributes);
begin
  if FChipSelectAttributes <> Value then
  begin
    FChipSelectAttributes := Value;
    UpdateRWMode;
  end;
end;

function TSysfsSPI.Read(const Buffer: Pointer; const BufferSize: Integer): Integer;
begin
  Result := Transfer(Buffer, nil, BufferSize);
end;

function TSysfsSPI.Write(const Buffer: Pointer; const BufferSize: Integer): Integer;
begin
  Result := Transfer(nil, Buffer, BufferSize);
end;

function TSysfsSPI.Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Integer): Integer;
var
  Data: spi_ioc_transfer;
begin
  if ((ReadBuffer = nil) and (WriteBuffer = nil)) or (BufferSize <= 0) then
    raise ESysfsInvalidParams.Create(SInvalidParameters);

  Data.tx_buf := PtrUInt(WriteBuffer);
  Data.rx_buf := PtrUInt(ReadBuffer);
  Data.len := BufferSize;
  Data.delay_usecs := 0;
  Data.speed_hz := FFrequency;
  Data.bits_per_word := FBitsPerWord;

  Result := fpioctl(FHandle, SPI_IOC_MESSAGE(1), @Data);
  if Result < 0 then
    raise ESysfsSPITransfer.Create(Format(SCannotSPITransferBytes, [BufferSize]));
end;

end.

