unit PXL.Sysfs.Types;
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
  SysUtils, PXL.TypeDef;

type
  ESysfsGeneric = class(Exception);

  ESysfsFileOpen = class(ESysfsGeneric);
  ESysfsFileOpenWrite = class(ESysfsFileOpen);
  ESysfsFileOpenRead = class(ESysfsFileOpen);
  ESysfsFileOpenReadWrite = class(ESysfsFileOpen);

  ESysfsFileAccess = class(ESysfsGeneric);
  ESysfsFileWrite = class(ESysfsFileAccess);
  ESysfsFileRead = class(ESysfsFileAccess);
  ESysfsFileMemoryMap = class(ESysfsFileAccess);

  ESysfsInvalidParams = class(ESysfsGeneric);

procedure WriteTextToFile(const FileName, Text: StdString);
function TryWriteTextToFile(const FileName, Text: StdString): Boolean;

function ReadCharFromFile(const FileName: StdString): StdChar;
function TryReadCharFromFile(const FileName: StdString; out Value: StdChar): Boolean;

function ReadTextFromFile(const FileName: StdString): StdString;
function TryReadTextFromFile(const FileName: StdString; out Value: StdString): Boolean;

resourcestring
  SCannotOpenFileForWriting = 'Cannot open file <%s> for writing.';
  SCannotOpenFileForReading = 'Cannot open file <%s> for reading.';
  SCannotOpenFileForReadingWriting = 'Cannot open file <%s> for reading and writing.';
  SCannotWriteTextToFile = 'Cannot write text <%s> to file <%s>.';
  SCannotReadTextFromFile = 'Cannot read text from file <%s>.';
  SCannotMemoryMapFile = 'Cannot map file <%s> to memory.';
  SInvalidParameters = 'The specified parameters are invalid.';

implementation

uses
  BaseUnix;

const
  PercentualLengthDiv = 4;
  StringBufferSize = 8;

procedure WriteTextToFile(const FileName, Text: StdString);
var
  Handle: TUntypedHandle;
begin
  Handle := fpopen(FileName, O_WRONLY);
  if Handle < 0 then
    raise ESysfsFileOpenWrite.Create(Format(SCannotOpenFileForWriting, [FileName]));
  try
    if fpwrite(Handle, Text[1], Length(Text)) <> Length(Text) then
      raise ESysfsFileWrite.Create(Format(SCannotWriteTextToFile, [Text, FileName]));
  finally
    fpclose(Handle);
  end;
end;

function TryWriteTextToFile(const FileName, Text: StdString): Boolean;
var
  Handle: TUntypedHandle;
begin
  Handle := fpopen(FileName, O_WRONLY);
  if Handle < 0 then
    Exit(False);
  try
    Result := fpwrite(Handle, Text[1], Length(Text)) = Length(Text);
  finally
    fpclose(Handle);
  end;
end;

function ReadCharFromFile(const FileName: StdString): StdChar;
var
  Handle: TUntypedHandle;
begin
  Handle := fpopen(FileName, O_RDONLY);
  if Handle < 0 then
    raise ESysfsFileOpenRead.Create(Format(SCannotOpenFileForReading, [FileName]));
  try
  {$IF SIZEOF(STDCHAR) > 1}
    Result := #0;
  {$ENDIF}
    if fpread(Handle, Result, 1) <> 1 then
      raise ESysfsFileRead.Create(Format(SCannotReadTextFromFile, [FileName]));
  finally
    fpclose(Handle);
  end;
end;

function TryReadCharFromFile(const FileName: StdString; out Value: StdChar): Boolean;
var
  Handle: TUntypedHandle;
begin
  Handle := fpopen(FileName, O_RDONLY);
  if Handle < 0 then
    Exit(False);
  try
  {$IF SIZEOF(STDCHAR) > 1}
    Value := #0;
  {$ENDIF}
    Result := fpread(Handle, Value, 1) = 1;
  finally
    fpclose(Handle);
  end;
end;

function ReadTextFromFile(const FileName: StdString): StdString;
var
  Handle: TUntypedHandle;
  Buffer: array[0..StringBufferSize - 1] of Byte;
  I, BytesRead, TextLength, NewTextLength: Integer;
begin
  TextLength := 0;

  Handle := fpopen(FileName, O_RDONLY);
  if Handle < 0 then
    raise ESysfsFileOpenRead.Create(Format(SCannotOpenFileForReading, [FileName]));
  try
    SetLength(Result, StringBufferSize);

    repeat
      BytesRead := fpread(Handle, Buffer[0], StringBufferSize);
      if (BytesRead < 0) or ((BytesRead = 0) and (TextLength <= 0)) then
        raise ESysfsFileRead.Create(Format(SCannotReadTextFromFile, [FileName]));

      if Length(Result) < TextLength + BytesRead then
      begin
        NewTextLength := Length(Result) + StringBufferSize + (Length(Result) div PercentualLengthDiv);
        SetLength(Result, NewTextLength);
      end;

      for I := 0 to BytesRead - 1 do
        Result[1 + TextLength + I] := Chr(Buffer[I]);

      Inc(TextLength, BytesRead);
    until BytesRead <= 0;
  finally
    fpclose(Handle);
  end;

  SetLength(Result, TextLength);
end;

function TryReadTextFromFile(const FileName: StdString; out Value: StdString): Boolean;
var
  Handle: TUntypedHandle;
  Buffer: array[0..StringBufferSize - 1] of Byte;
  I, BytesRead, TextLength, NewTextLength: Integer;
begin
  TextLength := 0;

  Handle := fpopen(FileName, O_RDONLY);
  if Handle < 0 then
  begin
    SetLength(Value, 0);
    Exit(False);
  end;

  try
    SetLength(Value, StringBufferSize);

    repeat
      BytesRead := fpread(Handle, Buffer[0], StringBufferSize);
      if (BytesRead < 0) or ((BytesRead = 0) and (TextLength <= 0)) then
      begin
        SetLength(Value, 0);
        Exit(False);
      end;

      if Length(Value) < TextLength + BytesRead then
      begin
        NewTextLength := Length(Value) + StringBufferSize + (Length(Value) div PercentualLengthDiv);
        SetLength(Value, NewTextLength);
      end;

      for I := 0 to BytesRead - 1 do
        Value[1 + TextLength + I] := Chr(Buffer[I]);

      Inc(TextLength, BytesRead);
    until BytesRead <= 0;
  finally
    fpclose(Handle);
  end;

  SetLength(Value, TextLength);
  Result := True;
end;

end.
