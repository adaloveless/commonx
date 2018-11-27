unit PXL.Providers.Auto;
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
{< Automatic cross-platform graphics provider selection and creation. }
interface

{$INCLUDE PXL.Config.inc}

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ELSE}
  {$ALIGN ON}
{$ENDIF}

uses
  PXL.ImageFormats, PXL.Providers;

{ Creates an instance of @link(TGraphicsDeviceProvider) depending on current platform, OS and available support. }
function CreateDefaultProvider(const AImageFormatManager: TCustomImageFormatManager = nil): TGraphicsDeviceProvider;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, PXL.Providers.DX9, PXL.Providers.DX11
{$ELSE}
  {$IFDEF SINGLEBOARD}
    PXL.Providers.GLES
  {$ELSE}
    PXL.Providers.GL
  {$ENDIF}
{$ENDIF};

{$IFDEF MSWINDOWS}
const
  VER_MAJORVERSION = $2;
  VER_MINORVERSION = $1;
  VER_GREATER = 2;
  VER_GREATER_EQUAL = 3;

type
  POSVERSIONINFOEX = ^OSVERSIONINFOEX;
  OSVERSIONINFOEX = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;

function VerSetConditionMask(dwlConditionMask: ULONGLONG; dwTypeBitMask: DWORD;
  dwConditionMask: BYTE): ULONGLONG; stdcall; external 'kernel32.dll' name 'VerSetConditionMask';

function VerifyVersionInfoW(lpVersionInfo: POSVERSIONINFOEX; dwTypeMask: DWORD;
  dwlConditionMask: DWORDLONG): BOOL; stdcall; external 'kernel32.dll' name 'VerifyVersionInfoW';

function IsWindows7OrGreater: Boolean;
var
  VersionInfoEx: OSVERSIONINFOEX;
  ConditionMask: UInt64;
begin
  FillChar(VersionInfoEx, SizeOf(OSVERSIONINFOEX), 0);
  VersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVERSIONINFOEX);

  VersionInfoEx.dwMajorVersion := 6;
  VersionInfoEx.dwMinorVersion := 0;

  ConditionMask := 0;
  ConditionMask := VerSetConditionMask(ConditionMask, VER_MAJORVERSION, VER_GREATER_EQUAL);
  ConditionMask := VerSetConditionMask(ConditionMask, VER_MINORVERSION, VER_GREATER);

  Result := VerifyVersionInfoW(@VersionInfoEx, VER_MAJORVERSION or VER_MINORVERSION, ConditionMask);
end;
{$ENDIF}

function CreateDefaultProvider(const AImageFormatManager: TCustomImageFormatManager): TGraphicsDeviceProvider;
begin
{$IFDEF MSWINDOWS}
  if IsWindows7OrGreater then
    Result := TDX11Provider.Create(AImageFormatManager)
  else
    Result := TDX9Provider.Create(AImageFormatManager);
{$ELSE}
  {$IFDEF SINGLEBOARD}
    Result := TGLESProvider.Create(AImageFormatManager);
  {$ELSE}
    Result := TGLProvider.Create(AImageFormatManager);
  {$ENDIF}
{$ENDIF}
end;

end.
