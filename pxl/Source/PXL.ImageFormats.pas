unit PXL.ImageFormats;
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
{< Base foundation and declarations for supporting loading and saving different image formats in the framework. }
interface

{$INCLUDE PXL.Config.inc}

uses
  Classes, PXL.TypeDef, PXL.Types, PXL.Classes, PXL.Surfaces;

type
  { Abstract image format manager, which provides facilities for loading and saving different image formats. }
  TCustomImageFormatManager = class abstract
  public
    { Loads image from specified stream.
        @param(Extension Extension (including dot, e.g. ".png") that represents the format in which the image is
          stored in the stream.)
        @param(Stream Stream which will be used for reading the image from. The current position of the stream will be
          used and after the call it will be adjusted to be right at the end of image data.)
        @param(DestSurface The destination surface where image data will be saved. It does need to be a valid
          instance of @link(TPixelSurface), but it may or may not be empty.)
        @param(AlphaFormatRequest The preference for premultipled or non-premultiplied alpha that should be used
          while loading the image. This is just a suggestion for the manager and it may or may not be followed
          depending on platform implementation.)
        @returns(@True when successful or @False otherwise.)
    }
    function LoadFromStream(const Extension: StdString; const Stream: TStream; const DestSurface: TPixelSurface;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare): Boolean; virtual; abstract;

    { Saves image to specified stream.
        @param(Extension Extension (including dot, e.g. ".png") that represents the format in which the image should
          be stored in the stream.)
        @param(Stream Stream which will be used for writing the image to. The current position of the stream will be
          used and after the call it will be adjusted to be right at the end of image data.)
        @param(SourceSurface The source surface where image data will be taken from. Note that the surface pixel format
          may determine the format in which the image will be saved. However, since not all pixel formats may be
          supported, the function may fail. Typically. @italic(TPixelFormat.A8R8G8B8) is the most widely supported
          format and should work under most circumstances.)
        @param(Quality Generic parameter that can be used as hint for saved image quality. For instance, for JPEG files,
          this is typically just an integer number (typecast to Pointer) representing quality between 0 and 100.)
        @returns(@True when successful or @False otherwise.)
    }
    function SaveToStream(const Extension: StdString; const Stream: TStream;
      const SourceSurface: TPixelSurface; const Quality: Pointer = nil): Boolean; virtual; abstract;

    { Loads image from specified file.
        @param(FileName A valid file name (with extension) that includes full path that represents the image.)
        @param(DestSurface The destination surface where image data will be saved. It does need to be a valid
          instance of @link(TPixelSurface), but it may or may not be empty.)
        @param(AlphaFormatRequest The preference for premultipled or non-premultiplied alpha that should be used
          while loading the image. This is just a suggestion for the manager and it may or may not be followed
          depending on platform implementation.)
        @returns(@True when successful or @False otherwise.)
    }
    function LoadFromFile(const FileName: StdString; const DestSurface: TPixelSurface;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare): Boolean; virtual; abstract;

    { Saves image to specified file.
        @param(FileName A valid file name (with extension) that includes full path where the image is to be saved.
          The path to this file must exist.)
        @param(SourceSurface The source surface where image data will be taken from. Note that the surface pixel format
          may determine the format in which the image will be saved. However, since not all pixel formats may be
          supported, the function may fail. Typically. @italic(TPixelFormat.A8R8G8B8) is the most widely supported
          format and should work under most circumstances.)
        @param(Quality Generic parameter that can be used as hint for saved image quality. For instance, for JPEG files,
          this is typically just an integer number (typecast to Pointer) representing quality between 0 and 100.)
        @returns(@True when successful or @False otherwise.)
    }
    function SaveToFile(const FileName: StdString; const SourceSurface: TPixelSurface;
      const Quality: Pointer = nil): Boolean; virtual; abstract;

    { Loads image from specified file located in /assets sub-folder. This function only works on @italic(Android)
      platform.
        @param(FileName A valid file name (with extension) that represents the image.)
        @param(DestSurface The destination surface where image data will be saved. It does need to be a valid
          instance of @link(TPixelSurface), but it may or may not be empty.)
        @param(AlphaFormatRequest The preference for premultipled or non-premultiplied alpha that should be used
          while loading the image. This is just a suggestion for the manager and it may or may not be followed
          depending on platform implementation.)
        @returns(@True when successful or @False otherwise.)
    }
    function LoadFromAsset(const AssetName: StdString; const DestSurface: TPixelSurface;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare): Boolean; virtual; abstract;
  end;

  TImageFormatManager = class;

  { Extension class for @link(TImageFormatManager) that supports saving and loading different image formats and can be
    plugged to one or more image format managers. }
  TCustomImageFormatHandler = class abstract
  private
    FManager: TImageFormatManager;
    FHandlerIndexTemp: Integer;
  protected
    { Returns reference to image format manager that is associated with this handler. If there is more than one manager
      referring to this handler, then the first one is returned. }
    function GetManager: TImageFormatManager; virtual;

    { Registers a specific extension to be processed by this handler along with some custom @italic(Context) pointer,
      which will be saved and later provided to @link(LoadFromStream) and @link(SaveToStream) methods. }
    procedure RegisterExtension(const Extension: StdString; const Context: Pointer = nil); virtual;

    { This method is executed during creation of @link(TCustomImageFormatHandler) class to register extensions that are
      supported by the handler. During this call, one or more @link(RegisterExtension) calls should be made to register
      supported extensions. }
    procedure RegisterExtensions; virtual; abstract;
  public
    { Creates instance of image format handler and associates it with the provided manager class. }
    constructor Create(const AManager: TImageFormatManager);
    { @exclude } destructor Destroy; override;

    { Loads image from the stream. @italic(Context) parameter will contain the same value as the one passed to
      @link(RegisterExtension) function during creation. The rest of parameters have the same meaning as in methods
      inside @link(TCustomImageFormatManager) class. }
    function LoadFromStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean; virtual; abstract;

    { Saves image to the stream. @italic(Context) parameter will contain the same value as the one passed to
      @link(RegisterExtension) function during creation. The rest of parameters have the same meaning as in methods
      inside @link(TCustomImageFormatManager) class. }
    function SaveToStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean; virtual; abstract;

    { Reference to image format manager that is associated with this class. }
    property Manager: TImageFormatManager read GetManager;
  end;

  { General-purpose Image Format Manager that has pluggable mechanism so that handlers (those derived from
    @link(TCustomImageFormatHandler) class) can be associated with it to load and save different image formats. If
    multiple handlers that support the same image format are associated, then the one that was most recently associated
    will be used. }
  TImageFormatManager = class(TCustomImageFormatManager)
  private type
    THandlerExtension = record
      Extension: StdString;
      Context: Pointer;
    end;

    THandlerRegistry = record
      Handler: TCustomImageFormatHandler;

      Extensions: array of THandlerExtension;
      ExtensionsDirty: Boolean;

      procedure ExtensionsSwap(const Index1, Index2: Integer);
      function ExtensionsCompare(const Index1, Index2: Integer): Integer; inline;
      function ExtensionsSplit(const Start, Stop: Integer): Integer;
      procedure ExtensionsSort(const Start, Stop: Integer);
      procedure ProcessExtensions;
      function IndexOfExtension(const Extension: StdString): Integer;
      function AddExtension(const Extension: StdString; const Context: Pointer): Integer;
      procedure RemoveExtension(const ExtesionIndex: Integer);
    end;
  private
    FEntries: array of THandlerRegistry;

    procedure UnregisterHandlerIndex(const HandlerIndex: Integer);
    procedure FindExtension(const Extension: StdString; out HandlerIndex, ExtensionIndex: Integer);
  public
    { @exclude } constructor Create;
    { @exclude } destructor Destroy; override;

    { Returns index (or the first occurrence) of the specified image format handler that is associated with this class. }
    function IndexOfHandler(const ImageFormatHandler: TCustomImageFormatHandler): Integer;

    { Registers specified image format handler with the manager and returns its index in the list. }
    function RegisterHandler(const ImageFormatHandler: TCustomImageFormatHandler): Integer;

    { Removes specified image format handler from the list that was previously associated with the manager.}
    procedure UnregisterHandler(const ImageFormatHandler: TCustomImageFormatHandler);

    { Removes all the associations with image format handlers from the list. }
    procedure Clear;

    { Registers the specified image format handler (that was previously associated by using @link(RegisterHandler) call,
      to the specified extension. If multiple handlers are registered to the same extension, then the handler that was
      most recently registered will be used. @italic(Context) parameter will be saved and then passed to the
      appropriate methods inside @link(TCustomImageFormatHandler) class when loading and saving images. }
    function RegisterExtension(const Extension: StdString; const Handler: TCustomImageFormatHandler;
      const Context: Pointer): Boolean;

    { Removes the registry for given extension associated with the provided handler. If the handler is not specified,
      or is @nil, then the first handler that supports such extension will be looked for. @True is returned when the
      method success and @False otherwise. If it is necessary to unregister all handlers for specific extension, then t
      his method should be called multiple times until @False
      is returned. }
    function UnregisterExtension(const Extension: StdString; const Handler: TCustomImageFormatHandler = nil): Boolean;

    { Loads image from specified stream. This function will look through the internal registry and use one of the most
      recently registered handlers that supports such extension. If no handler supports such extension, the method will
      fail.
        @param(Extension Extension (including dot, e.g. ".png") that represents the format in which the image is
          stored in the stream.)
        @param(Stream Stream which will be used for reading the image from. The current position of the stream will be
          used and after the call it will be adjusted to be right at the end of image data.)
        @param(DestSurface The destination surface where image data will be saved. It does need to be a valid
          instance of @link(TPixelSurface), but it may or may not be empty.)
        @param(AlphaFormatRequest The preference for premultipled or non-premultiplied alpha that should be used
          while loading the image. This is just a suggestion for the manager and it may or may not be followed
          depending on platform implementation.)
        @returns(@True when successful or @False otherwise.)
    }
    function LoadFromStream(const Extension: StdString; const Stream: TStream; const DestSurface: TPixelSurface;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare): Boolean; override;

    { Saves image to specified stream. This function will look through the internal registry and use one of the most
      recently registered handlers that supports such extension. If no handler supports such extension, the method will
      fail.
        @param(Extension Extension (including dot, e.g. ".png") that represents the format in which the image should
          be stored in the stream.)
        @param(Stream Stream which will be used for writing the image to. The current position of the stream will be
          used and after the call it will be adjusted to be right at the end of image data.)
        @param(SourceSurface The source surface where image data will be taken from. Note that the surface pixel format
          may determine the format in which the image will be saved. However, since not all pixel formats may be
          supported, the function may fail. Typically. @italic(TPixelFormat.A8R8G8B8) is the most widely supported
          format and should work under most circumstances.)
        @param(Quality Generic parameter that can be used as hint for saved image quality. For instance, for JPEG files,
          this is typically just an integer number (typecast to Pointer) representing quality between 0 and 100.)
        @returns(@True when successful or @False otherwise.)
    }
    function SaveToStream(const Extension: StdString; const Stream: TStream;
      const SourceSurface: TPixelSurface; const Quality: Pointer = nil): Boolean; override;

    { Loads image from specified file. This function will look through the internal registry and use one of the most
      recently registered handlers that supports such extension. If no handler supports such extension, the method will
      fail.
        @param(FileName A valid file name (with extension) that includes full path that represents the image.)
        @param(DestSurface The destination surface where image data will be saved. It does need to be a valid
          instance of @link(TPixelSurface), but it may or may not be empty.)
        @param(AlphaFormatRequest The preference for premultipled or non-premultiplied alpha that should be used
          while loading the image. This is just a suggestion for the manager and it may or may not be followed
          depending on platform implementation.)
        @returns(@True when successful or @False otherwise.)
    }
    function LoadFromFile(const FileName: StdString; const DestSurface: TPixelSurface;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare): Boolean; override;

    { Saves image to specified file. This function will look through the internal registry and use one of the most
      recently registered handlers that supports such extension. If no handler supports such extension, the method will
      fail.
        @param(FileName A valid file name (with extension) that includes full path where the image is to be saved.
          The path to this file must exist.)
        @param(SourceSurface The source surface where image data will be taken from. Note that the surface pixel format
          may determine the format in which the image will be saved. However, since not all pixel formats may be
          supported, the function may fail. Typically. @italic(TPixelFormat.A8R8G8B8) is the most widely supported
          format and should work under most circumstances.)
        @param(Quality Generic parameter that can be used as hint for saved image quality. For instance, for JPEG files,
          this is typically just an integer number (typecast to Pointer) representing quality between 0 and 100.)
        @returns(@True when successful or @False otherwise.)
    }
    function SaveToFile(const FileName: StdString; const SourceSurface: TPixelSurface;
      const Quality: Pointer = nil): Boolean; override;

    { Loads image from specified file located in /assets sub-folder. This function will look through the internal registry and use one of the most
      recently registered handlers that supports such extension. If no handler supports such extension, the method will
      fail. This function only works on @italic(Android) platform.
        @param(FileName A valid file name (with extension) that represents the image.)
        @param(DestSurface The destination surface where image data will be saved. It does need to be a valid
          instance of @link(TPixelSurface), but it may or may not be empty.)
        @param(AlphaFormatRequest The preference for premultipled or non-premultiplied alpha that should be used
          while loading the image. This is just a suggestion for the manager and it may or may not be followed
          depending on platform implementation.)
        @returns(@True when successful or @False otherwise.)
    }
    function LoadFromAsset(const AssetName: StdString; const DestSurface: TPixelSurface;
      const AlphaFormatRequest: TAlphaFormatRequest = TAlphaFormatRequest.DontCare): Boolean; override;
  end;

implementation

uses
  SysUtils;

{$REGION 'TCustomImageFormatHandler'}

constructor TCustomImageFormatHandler.Create(const AManager: TImageFormatManager);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FManager := AManager;
  if FManager <> nil then
  begin
    FHandlerIndexTemp := FManager.RegisterHandler(Self);
    try
      RegisterExtensions;
    finally
      FHandlerIndexTemp := -1;
    end;
  end;
end;

destructor TCustomImageFormatHandler.Destroy;
begin
  try
    if FManager <> nil then
    begin
      FManager.UnregisterHandler(Self);
      FManager := nil;
    end;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TCustomImageFormatHandler.GetManager: TImageFormatManager;
begin
  Result := FManager;
end;

procedure TCustomImageFormatHandler.RegisterExtension(const Extension: StdString; const Context: Pointer);
begin
  if FManager <> nil then
    FManager.RegisterExtension(Extension, Self, Context);
end;

{$ENDREGION}
{$REGION 'TImageFormatManager.THandlerRegistry'}

procedure TImageFormatManager.THandlerRegistry.ExtensionsSwap(const Index1, Index2: Integer);
var
  TempValue: THandlerExtension;
begin
  TempValue := Extensions[Index1];
  Extensions[Index1] := Extensions[Index2];
  Extensions[Index2] := TempValue;
end;

function TImageFormatManager.THandlerRegistry.ExtensionsCompare(const Index1, Index2: Integer): Integer;
begin
  Result := CompareText(Extensions[Index1].Extension, Extensions[Index2].Extension);
end;

function TImageFormatManager.THandlerRegistry.ExtensionsSplit(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := Start;

  while Left <= Right do
  begin
    while (Left <= Stop) and (ExtensionsCompare(Left, Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (ExtensionsCompare(Right, Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      ExtensionsSwap(Left, Right);
  end;

  ExtensionsSwap(Start, Right);
  Result := Right;
end;

procedure TImageFormatManager.THandlerRegistry.ExtensionsSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := ExtensionsSplit(Start, Stop);

    ExtensionsSort(Start, SplitPt - 1);
    ExtensionsSort(SplitPt + 1, Stop);
  end;
end;

procedure TImageFormatManager.THandlerRegistry.ProcessExtensions;
begin
  if Length(Extensions) > 1 then
    ExtensionsSort(0, Length(Extensions) - 1);

  ExtensionsDirty := False;
end;

function TImageFormatManager.THandlerRegistry.IndexOfExtension(const Extension: StdString): Integer;
var
  Left, Right, Pivot, Res: Integer;
begin
  if ExtensionsDirty then
    ProcessExtensions;

  Left := 0;
  Right := Length(Extensions) - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;
    Res := CompareText(Extensions[Pivot].Extension, Extension);

    if Res = 0 then
      Exit(Pivot);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

function TImageFormatManager.THandlerRegistry.AddExtension(const Extension: StdString; const Context: Pointer): Integer;
begin
  Result := Length(Extensions);
  SetLength(Extensions, Result + 1);

  Extensions[Result].Extension := Extension;
  Extensions[Result].Context := Context;

  ExtensionsDirty := True;
end;

procedure TImageFormatManager.THandlerRegistry.RemoveExtension(const ExtesionIndex: Integer);
var
  I: Integer;
begin
  if (ExtesionIndex < 0) or (ExtesionIndex >= Length(Extensions)) then
    Exit;

  for I := ExtesionIndex to Length(Extensions) - 2 do
    Extensions[I] := Extensions[I + 1];

  SetLength(Extensions, Length(Extensions) - 1);
end;

{$ENDREGION}
{$REGION 'TImageFormatManager'}

constructor TImageFormatManager.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TImageFormatManager.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TImageFormatManager.IndexOfHandler(const ImageFormatHandler: TCustomImageFormatHandler): Integer;
var
  I: Integer;
begin
  for I := Length(FEntries) - 1 downto 0 do
    if FEntries[I].Handler = ImageFormatHandler then
      Exit(I);

  Result := -1;
end;

function TImageFormatManager.RegisterHandler(const ImageFormatHandler: TCustomImageFormatHandler): Integer;
begin
  Result := IndexOfHandler(ImageFormatHandler);
  if Result = -1 then
  begin
    Result := Length(FEntries);
    SetLength(FEntries, Result + 1);
  end;

  FEntries[Result].Handler := ImageFormatHandler;
end;

procedure TImageFormatManager.UnregisterHandlerIndex(const HandlerIndex: Integer);
var
  I: Integer;
begin
  if (HandlerIndex < 0) or (HandlerIndex >= Length(FEntries)) then
    Exit;

  for I := HandlerIndex to Length(FEntries) - 2 do
    FEntries[I] := FEntries[I + 1];

  SetLength(FEntries, Length(FEntries) - 1);
end;

procedure TImageFormatManager.UnregisterHandler(const ImageFormatHandler: TCustomImageFormatHandler);
begin
  UnregisterHandlerIndex(IndexOfHandler(ImageFormatHandler));
end;

procedure TImageFormatManager.Clear;
begin
  SetLength(FEntries, 0);
end;

function TImageFormatManager.RegisterExtension(const Extension: StdString;
  const Handler: TCustomImageFormatHandler; const Context: Pointer): Boolean;
var
  Index: Integer;
begin
  if Handler = nil then
    Exit(False);

  if Handler.FHandlerIndexTemp <> -1 then
    Index := Handler.FHandlerIndexTemp
  else
    Index := IndexOfHandler(Handler);

  if Index = -1 then
    Exit(False);

  Result := FEntries[Index].AddExtension(Extension, Context) <> -1;
end;

procedure TImageFormatManager.FindExtension(const Extension: StdString; out HandlerIndex, ExtensionIndex: Integer);
var
  I: Integer;
begin
  for I := Length(FEntries) - 1 downto 0 do
  begin
    ExtensionIndex := FEntries[I].IndexOfExtension(Extension);
    if ExtensionIndex <> -1 then
    begin
      HandlerIndex := I;
      Exit;
    end;
  end;

  HandlerIndex := -1;
  ExtensionIndex := -1;
end;

function TImageFormatManager.UnregisterExtension(const Extension: StdString;
  const Handler: TCustomImageFormatHandler): Boolean;
var
  HandlerIndex, ExtensionIndex: Integer;
begin
  if Handler <> nil then
  begin
    HandlerIndex := IndexOfHandler(Handler);
    if HandlerIndex = -1 then
      Exit(False);

    ExtensionIndex := FEntries[HandlerIndex].IndexOfExtension(Extension);
  end
  else
    FindExtension(Extension, HandlerIndex, ExtensionIndex);

  if (HandlerIndex = -1) or (ExtensionIndex = -1) then
    Exit(False);

  FEntries[HandlerIndex].RemoveExtension(ExtensionIndex);
  Result := True;
end;

function TImageFormatManager.LoadFromStream(const Extension: StdString; const Stream: TStream;
  const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  HandlerIndex, ExtensionIndex: Integer;
begin
  if (Stream = nil) or (DestSurface = nil) then
    Exit(False);

  FindExtension(Extension, HandlerIndex, ExtensionIndex);
  if (HandlerIndex = -1) or (ExtensionIndex = -1) then
    Exit(False);

  Result := FEntries[HandlerIndex].Handler.LoadFromStream(FEntries[HandlerIndex].Extensions[ExtensionIndex].Context,
    Extension, Stream, DestSurface, AlphaFormatRequest);
end;

function TImageFormatManager.SaveToStream(const Extension: StdString; const Stream: TStream;
  const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean;
var
  HandlerIndex, ExtensionIndex: Integer;
begin
  if (Stream = nil) or (SourceSurface = nil) then
    Exit(False);

  FindExtension(Extension, HandlerIndex, ExtensionIndex);
  if (HandlerIndex = -1) or (ExtensionIndex = -1) then
    Exit(False);

  Result := FEntries[HandlerIndex].Handler.SaveToStream(FEntries[HandlerIndex].Extensions[ExtensionIndex].Context,
    Extension, Stream, SourceSurface, Quality);
end;

function TImageFormatManager.LoadFromFile(const FileName: StdString; const DestSurface: TPixelSurface;
  const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadFromStream(ExtractFileExt(FileName), Stream, DestSurface, AlphaFormatRequest);
    finally
      Stream.Free;
    end;
  except
    Exit(False);
  end;
end;

function TImageFormatManager.LoadFromAsset(const AssetName: StdString; const DestSurface: TPixelSurface;
  const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  Stream: TAssetStream;
begin
  try
    Stream := TAssetStream.Create(AssetName);
    try
      Result := LoadFromStream(ExtractFileExt(AssetName), Stream, DestSurface, AlphaFormatRequest);
    finally
      Stream.Free;
    end;
  except
    Exit(False);
  end;
end;

function TImageFormatManager.SaveToFile(const FileName: StdString; const SourceSurface: TPixelSurface;
  const Quality: Pointer): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
    try
      Result := SaveToStream(ExtractFileExt(FileName), Stream, SourceSurface, Quality);
    finally
      Stream.Free;
    end;
  except
    Exit(False);
  end;
end;

{$ENDREGION}

end.
