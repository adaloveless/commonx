unit PXL.Types.GL;
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
  Classes, PXL.TypeDef, PXL.Devices;

type
  TExtensions = class
  private
    FStrings: TStringList;

    FEXT_framebuffer_object: Boolean;
    FEXT_packed_depth_stencil: Boolean;
    FEXT_separate_specular_color: Boolean;
    FEXT_swap_control: Boolean;
    FARB_framebuffer_object: Boolean;

    FMajorVersion: Integer;
    FMinorVersion: Integer;

    procedure Populate;
    procedure DecodeVersionString;
    procedure CheckPopularExtensions;
    function GetExtension(const ExtName: StdString): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function VersionCheck(const AMajorVersion, AMinorVersion: Integer): Boolean;

    property Strings: TStringList read FStrings;
    property Extension[const ExtName: StdString]: Boolean read GetExtension; default;

    property MajorVersion: Integer read FMajorVersion;
    property MinorVersion: Integer read FMinorVersion;

    property EXT_framebuffer_object: Boolean read FEXT_framebuffer_object;
    property EXT_packed_depth_stencil: Boolean read FEXT_packed_depth_stencil;
    property EXT_separate_specular_color: Boolean read FEXT_separate_specular_color;
    property EXT_swap_control: Boolean read FEXT_swap_control;
    property ARB_framebuffer_object: Boolean read FARB_framebuffer_object;
  end;

  TGLDeviceContext = class(TCustomDeviceContext)
  private
    FExtensions: TExtensions;
    FFrameBufferLevel: Integer;

  {$IFDEF FPC}
    class var FLibraryLoaded: Boolean;
  {$ENDIF}

    function GetExtensions: TExtensions;
  public
    destructor Destroy; override;

  {$IFDEF FPC}
    function LoadLibrary: Boolean;
  {$ENDIF}

    procedure FrameBufferLevelIncrement;
    procedure FrameBufferLevelDecrement;

    property Extensions: TExtensions read GetExtensions;
    property FrameBufferLevel: Integer read FFrameBufferLevel;
  end;

implementation

uses
{$IFDEF FPC}
  gl, glext,
{$ELSE}
  {$IFDEF MSWINDOWS}
    Winapi.OpenGL,
  {$ENDIF}

  {$IFDEF MACOS}
    Macapi.CocoaTypes, Macapi.OpenGL,
  {$ENDIF}
{$ENDIF}

  SysUtils;

type
  TExtString = {$IFDEF FPC} AnsiString {$ELSE} string {$ENDIF};
  TExtChar = {$IFDEF FPC} AnsiChar {$ELSE} Char {$ENDIF};

const
  GL_EXT_framebuffer_object = 'GL_EXT_framebuffer_object';
  GL_EXT_packed_depth_stencil = 'GL_EXT_packed_depth_stencil';
  GL_EXT_separate_specular_color = 'GL_EXT_separate_specular_color';
  GL_EXT_swap_control = 'WGL_EXT_swap_control';
  GL_ARB_framebuffer_object = 'GL_ARB_framebuffer_object';

function GetGLString(const Name: GLenum): TExtString;
begin
{$IFDEF FPC}
  Result := PAnsiChar(glGetString(Name));
{$ELSE}
  Result := StdString(MarshaledAString(glGetString(Name)));
{$ENDIF}
end;

function IsNumberChar(const Value: TExtChar): Boolean;
begin
  Result := (Value >= '0') and (Value <= '9');
end;

function ExtractNumberWord(const Text: TExtString; var CharPos: Integer; const MaxCount: Integer = 0): TExtString;
var
  StartAt, CopyCount: Integer;
begin
  while (not IsNumberChar(Text[CharPos])) and (CharPos <= Length(Text)) do
    Inc(CharPos);

  if CharPos > Length(Text) then
    Exit('');

  StartAt := CharPos;

  while IsNumberChar(Text[CharPos]) and (CharPos <= Length(Text)) do
    Inc(CharPos);

  CopyCount := CharPos - StartAt;
  if CopyCount < 1 then
    Exit('');

  if (MaxCount > 0) and (CopyCount > MaxCount) then
    CopyCount := MaxCount;

  Result := Copy(Text, StartAt, CopyCount);
end;

constructor TExtensions.Create;
begin
  inherited;

  FStrings := TStringList.Create;
  FStrings.CaseSensitive := False;

  Populate;
  DecodeVersionString;
  CheckPopularExtensions;
end;

destructor TExtensions.Destroy;
begin
  FStrings.Free;

  inherited;
end;

procedure TExtensions.Populate;
var
  SpacePos: Integer;
  ExtText: TExtString;
begin
  ExtText := GetGLString(GL_EXTENSIONS);
  if glGetError <> GL_NO_ERROR then
    Exit;

  while Length(ExtText) > 0 do
  begin
    SpacePos := Pos(' ', ExtText);
    if SpacePos <> 0 then
    begin
      FStrings.Add(Trim(Copy(ExtText, 1, SpacePos - 1)));
      ExtText := Copy(ExtText, SpacePos + 1, Length(ExtText) - SpacePos);
    end
    else
    begin
      if Length(ExtText) > 0 then
        FStrings.Add(Trim(ExtText));

      Break;
    end;
  end;

  FStrings.Sorted := True;
end;

procedure TExtensions.DecodeVersionString;
var
  VersionText: TExtString;
  CharPos: Integer;
begin
  VersionText := GetGLString(GL_VERSION);

  CharPos := 1;

  FMajorVersion := StrToIntDef(ExtractNumberWord(VersionText, CharPos), 0);

  if CharPos <= Length(VersionText) then
    FMinorVersion := StrToIntDef(ExtractNumberWord(VersionText, CharPos, 1), 0);
end;

procedure TExtensions.CheckPopularExtensions;
begin
  FEXT_framebuffer_object := GetExtension(GL_EXT_framebuffer_object);
  FEXT_packed_depth_stencil := GetExtension(GL_EXT_packed_depth_stencil);
  FEXT_separate_specular_color := GetExtension(GL_EXT_separate_specular_color);
  FEXT_swap_control := GetExtension(GL_EXT_swap_control);
  FARB_framebuffer_object := GetExtension(GL_ARB_framebuffer_object);
end;

function TExtensions.VersionCheck(const AMajorVersion, AMinorVersion: Integer): Boolean;
begin
  Result :=
    (FMajorVersion > AMajorVersion) or
    ((FMajorVersion = AMajorVersion) and (FMinorVersion >= AMinorVersion));
end;

function TExtensions.GetExtension(const ExtName: StdString): Boolean;
begin
  Result := FStrings.IndexOf(ExtName) <> -1;
end;

{$REGION 'TGLDeviceContext'}

destructor TGLDeviceContext.Destroy;
begin
  FExtensions.Free;

  inherited;
end;

function TGLDeviceContext.GetExtensions: TExtensions;
begin
  if FExtensions = nil then
    FExtensions := TExtensions.Create;

  Result := FExtensions;
end;

procedure TGLDeviceContext.FrameBufferLevelIncrement;
begin
  Inc(FFrameBufferLevel);
end;

procedure TGLDeviceContext.FrameBufferLevelDecrement;
begin
  if FFrameBufferLevel > 0 then
    Dec(FFrameBufferLevel);
end;

{$IFDEF FPC}

function TGLDeviceContext.LoadLibrary: Boolean;
var
  LoadRes: Boolean;
begin
  if not FLibraryLoaded then
  begin
    LoadRes := False;

    if GetExtensions.MajorVersion >= 4 then
      LoadRes := Load_GL_version_4_0
    else if FExtensions.MajorVersion >= 3 then
    begin
      if FExtensions.MinorVersion >= 3 then
        LoadRes := Load_GL_version_3_3
      else if FExtensions.MinorVersion >= 2 then
        LoadRes := Load_GL_version_3_2
      else if FExtensions.MinorVersion >= 1 then
        LoadRes := Load_GL_version_3_1
      else
        LoadRes := Load_GL_version_3_0;
    end
    else if FExtensions.MajorVersion >= 2 then
    begin
      if FExtensions.MinorVersion >= 1 then
        LoadRes := Load_GL_version_2_1
      else
        LoadRes := Load_GL_version_2_0;
    end
    else if FExtensions.MajorVersion >= 1 then
    begin
      if FExtensions.MinorVersion >= 5 then
        LoadRes := Load_GL_version_1_5;
    end;

    if LoadRes and FExtensions.EXT_framebuffer_object and (not Load_GL_EXT_framebuffer_object) then
      LoadRes := False;

    if LoadRes and (FExtensions.ARB_framebuffer_object or FExtensions.VersionCheck(3, 0)) and
      (not Load_GL_ARB_framebuffer_object(True)) then
      LoadRes := False;

    FLibraryLoaded := LoadRes;
  end;

  Result := FLibraryLoaded;
end;

{$ENDIF}
{$ENDREGION}

initialization
{$IFDEF FPC}
  TGLDeviceContext.FLibraryLoaded := False;
{$ENDIF}

end.
