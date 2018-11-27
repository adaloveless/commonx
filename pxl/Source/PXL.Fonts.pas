unit PXL.Fonts;
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
{< Pre-rendered bitmap fonts with Unicode support and visual effects such as border and shadow, customized spacing
   between individual letter pairs, rendering using vertical color gradient, formatted text among other features. }
interface

{$INCLUDE PXL.Config.inc}

{ The following option controls the inclusion of system 8x8 or 9x8 font, which is integrated into executable and
  always available. The inclusion of system font increases application size and memory usage by 2.5 KiB. }
{$DEFINE IncludeSystemFont}

uses
  Classes, PXL.TypeDef, PXL.Types, PXL.Lists, PXL.Devices, PXL.Textures, PXL.Images, PXL.Canvas, PXL.XML;

type
  { Text alignment when drawing with certain functions. }
  TTextAlignment = (
    { Text should be aligned to the beginning (either top or left depending on context). }
    Start,

    { Text should be centered in the middle. }
    Middle,

    { Text should be aligned to the end (either bottom or right depending on context). }
    Final);

{$IFDEF IncludeSystemFont}
  { Type of image for system font. }
  TSystemFontImage = (
    { 8x8 font that was commonly used in text mode on EGA displays with 640x350 resolution. }
    Font8x8,

    { 9x8 font that was commonly used in text mode on VGA displays with 720x400 resolution. }
    Font9x8);
{$ENDIF}

  { Bitmap font implementation. }
  TBitmapFont = class
  private type
    PStyleState = ^TStyleState;
    TStyleState = record
      Color: TIntColor2;
      Style: Cardinal;
    end;

    TParagraphWord = record
      WordText: UniString;
      ParagraphIndex: Integer;
    end;

    PLetterEntry = ^TLetterEntry;
    TLetterEntry = packed record
      TopBase: Byte;
      BottomBase: Byte;
      LeadingSpace: ShortInt;
      TrailingSpace: ShortInt;
      MapLeft: Word;
      MapTop: Word;
      MapWidth: Byte;
      MapHeight: Byte;
    end;
  public const
    { Default image extension when loading fonts provided as pair of XML and image files. }
    DefaultImageExtension = '.png';

    { Default XML extension when loading fonts provided as pair of XML and image files. }
    DefaultXMLExtension = '.xml';

    { Default font extension when loading binary fonts. }
    DefaultBinaryExtension = '.font';
  public type
    { Text tags that allow changing colors directly inside text. These tags are specified similarly to XML tags but
      in simplified form. For instance, tags with names "red" and "green" with their corresponding colors can be used
      as in the following text: @br @code(This is a <green>demo <red>string</> with custom color</> tag.) }
    TStyleTags = class
    protected type
      { Pointer to @link(TEntry). }
      PEntry = ^TEntry;

      { Style tag entry. }
      TEntry = record
        { Name of the tag (not case-sensitive). }
        Name: UniString;

        { Color gradient associated with the tag. }
        Color: TIntColor2;

        { Text style (currently unsupported). }
        Style: Cardinal;
      end;
    private
      FEntries: array of TEntry;
      FEntriesDirty: Boolean;

      procedure SwapEntries(const Index1, Index2: Integer);
      function SortSplitEntries(const Start, Stop: Integer): Integer;
      procedure QuickSortEntries(const Start, Stop: Integer);
      function IndexOf(const Name: UniString): Integer;
      procedure Remove(const Index: Integer);

      class function UniCompareText(const Text1, Text2: UniString): Integer; inline;
    protected
      { Searches for the given tag and returns pointer to its entry. If no tag with such name exists, @nil is
        returned. }
      function Find(const Name: UniString): PEntry;
    public
      { Inserts new tag with the given name, color and style to the list. }
      procedure Insert(const Name: UniString; const Color: TIntColor2; const Style: Cardinal = 0);

      { Remove all existing tags from the list. }
      procedure Clear;

      { Removes tag with the specified name from the list. If no such tag exists, this method does nothing. }
      procedure Delete(const Name: UniString);
    end;

    { Spacing information between each pair of individual letters to provide pixel-perfect text rendering. }
    TKernings = class
    private
      FHashArray: packed array[0..255, 0..255] of ShortInt;
      FExtArray: TPointList;

      function GetShift(const Code1, Code2: Integer): Integer;
    public
      constructor Create;
      destructor Destroy; override;

      { Specifies the spacing between two character codes specified by their Unicode number. }
      procedure Spec(const Code1, Code2: Integer; Shift: Integer); overload;

      { Specifies the spacing between two UFT-16 characters. }
      procedure Spec(const Code1, Code2: WideChar; const Shift: Integer); overload;

      { Returns the spacing between two character codes specified by their Unicode number. If no entry for that
        combination exists, zero (no change) is returned. }
      property Shift[const Code1, Code2: Integer]: Integer read GetShift; default;
    end;

    { Event invoked by @link(DrawTextCustom) method for custom text rendering, called for each text letter.
        @param(Sender Reference to class that invoked this method.)
        @param(FontImage Font image that should be used as a source for drawing letter.)
        @param(SourceRect Rectangle in font image that contains letter image.)
        @param(DestRect Destination rectangle, where font image should be rendered.)
        @param(Color Color, which should be used for rendering the letter.)
        @param(UserContext User context parameter passed to @link(DrawTextCustom).) }
    TTextEntryEvent = procedure(const Sender: TObject; const FontImage: TAtlasImage; const SourceRect: TIntRect;
      const DestRect: TFloatRect; const Color: TIntColor4; const UserContext: Pointer);
  private const
    StyleStackCount = 16;
    MaxEntriesCount = 65536;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;
    FSubscribedTextures: Boolean;

    FKernings: TKernings;

    FName: StdString;

    FParagraphWords: array of TParagraphWord;
    FStyleStates: array[0..StyleStackCount - 1] of TStyleState;
    FStyleStateCount: Integer;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FCanvas: TCustomCanvas;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FStyleTags: TStyleTags;

    FParagraphSeparators: UniString;
    FWordSeparators: UniString;

    FScale: VectorFloat;
    FInterleave: VectorFloat;

    FSpaceWidth: VectorFloat;
    FVerticalSpace: VectorFloat;

    function RecreateImage(const TextureSize: TPoint2px; const PixelFormat: TPixelFormat): Boolean;
    function ReadFontEntriesFromStream(const Stream: TStream): Boolean;
    function ReadTextureFromStream(const Stream: TStream; const Texture: TCustomLockableTexture;
      const DataFormat: TPixelFormat; const ActualHeight: Integer): Boolean;
    function ReadFontImageFromStream(const Stream: TStream; const PixelFormat: TPixelFormat): Boolean;

  {$IFDEF IncludeSystemFont}
    function ReadTextureFromSystemFont(const Texture: TCustomLockableTexture): Boolean;
  {$ENDIF}

    function ReadEntryFromXML(const Node: TXMLNode): Boolean;
    function ReadFromXMLNode(const Node: TXMLNode): Boolean;

    procedure ClearStyleStates;
    procedure PushStyleState(const Color: TIntColor2; const Style: Cardinal = 0);
    function PeekStyleState: PStyleState;
    procedure PopStyleState;

    function ParseTag(const Text: UniString; var CharIndex: Integer; const IgnoreStyles: Boolean = False): Boolean;

    function IsWordSeparator(const Value: WideChar): Boolean;
    function ExtractParagraphWord(const Text: UniString; var CharIndex: Integer; var ParagraphIndex: Integer;
      out WordText: UniString): Boolean;
    procedure SplitTextIntoWords(const Text: UniString);
  protected
    { Current font size that defines maximum rectangle that a letter can occupy. }
    FSize: TPoint2px;

    { Current font image that contains pre-rendered letters. }
    FImage: TAtlasImage;

    { Information regarding locations, spacing and rendering parameters for each of available letters. }
    FEntries: array[0..MaxEntriesCount - 1] of TLetterEntry;

    { Loads letter information from XML file located in the stream. Returns @True when succeeded and @False otherwise. }
    function LoadEntriesFromXMLStream(const Stream: TStream): Boolean;

    { Loads letter information from external XML file. Returns @True when succeeded and @False otherwise. }
    function LoadEntriesFromXMLFile(const FileName: StdString): Boolean;

    { Loads letter information from file within /assets sub-folder. Returns @True when succeeded and @False otherwise. }
    function LoadEntriesFromXMLAsset(const AssetName: StdString): Boolean;
  public
    { Creates new instance of @code(TBitmapFont) class bound to the specified device. @code(ASubscribedTextures)
      indicates whether the textures contained in font image should be subscribed to handle "lost device" events.
      If this font is part of @link(TBitmapFonts) list, which provides its own notification events regarding device
      status, then @code(ASubscribedTextures) should be set to @False. }
    constructor Create(const ADevice: TCustomDevice; const ASubscribedTextures: Boolean = True);
    { @exclude } destructor Destroy; override;

    { Draws text by invoking external rendering event for each of the letters.
        @param(Position Starting position of text at top/left.)
        @param(Text The actual text to be drawn.)
        @param(Color Two colors representing vertical gradient to fill the letters with.)
        @param(Alpha A separate alpha representing transparency of the font (in addition to alpha provided in
          @code(Color)).)
        @param(TextEntryEvent Event to be called for each letter being rendered (this excludes whitespace characters
          such as space or #0).)
        @param(UserContext User context parameter that will be passed to each @code(TextEntryEvent) call.)
        @param(RestartStyling Whether the initial style of the text should be reset. This is typically needed when
          rendering multi-line text such as in case of @link(DrawTextBox).) }
    function DrawTextCustom(const Position: TPoint2; const Text: UniString; const Color: TIntColor2;
      const Alpha: VectorFloat; const TextEntryEvent: TTextEntryEvent; const UserContext: Pointer;
      const RestartStyling: Boolean = True): Boolean;

    { Draws text at the given position.
        @param(Position Starting position of text at top/left.)
        @param(Text The actual text to be drawn.)
        @param(Color Two colors representing vertical gradient to fill the letters with.)
        @param(Alpha A separate alpha representing transparency of the font (in addition to alpha provided in
          @code(Color)).) }
    procedure DrawText(const Position: TPoint2; const Text: UniString; const Color: TIntColor2;
      const Alpha: VectorFloat = 1);

    { Draws text at the given position with specific alignment.
        @param(Position Starting position of text at top/left.)
        @param(Text The actual text to be drawn.)
        @param(Color Two colors representing vertical gradient to fill the letters with.)
        @param(HorizAlign Horizontal text alignment in relation to starting position.)
        @param(VertAlign Vertical text alignment in relation to starting position.)
        @param(Alpha A separate alpha representing transparency of the font (in addition to alpha provided in
          @code(Color)).)
        @param(AlignToPixels Whether to align the resulting text position to start at integer location so that
          all letters are properly aligned to pixels. This may result in clearer text but can appear choppy during
          text animations (e.g. zoom in or out).) }
    procedure DrawTextAligned(const Position: TPoint2; const Text: UniString; const Color: TIntColor2;
      const HorizAlign, VertAlign: TTextAlignment; const Alpha: VectorFloat = 1;
      const AlignToPixels: Boolean = True);

    { Draws text centered around the specified position.
        @param(Position Origin around which the text will be rendered.)
        @param(Text The actual text to be drawn.)
        @param(Color Two colors representing vertical gradient to fill the letters with.)
        @param(Alpha A separate alpha representing transparency of the font (in addition to alpha provided in
          @code(Color)).)
        @param(AlignToPixels Whether to align the resulting text position to start at integer location so that
          all letters are properly aligned to pixels. This may result in clearer text but can appear choppy during
          text animations (e.g. zoom in or out).) }
    procedure DrawTextCentered(const Position: TPoint2; const Text: UniString; const Color: TIntColor2;
      const Alpha: VectorFloat = 1; const AlignToPixels: Boolean = True);

    { Returns total area size that given text string will occupy when rendered. }
    function TextExtent(const Text: UniString): TPoint2;

    { Returns total area width that given text string will occupy when rendered. }
    function TextWidth(const Text: UniString): VectorFloat;

    { Returns total area height that given text string will occupy when rendered. }
    function TextHeight(const Text: UniString): VectorFloat;

    { Returns total area size (rounded to nearest integer) that given text string will occupy when rendered. }
    function TextExtentInt(const Text: UniString): TPoint2px;

    { Returns total area width (rounded to nearest integer) that given text string will occupy when rendered. }
    function TextWidthInt(const Text: UniString): VectorInt;

    { Returns total area height (rounded to nearest integer) that given text string will occupy when rendered. }
    function TextHeightInt(const Text: UniString): VectorInt;

    { Draws text containing multiple lines onto the designated area.
        @param(TopLeft Top/left origin of designated area.)
        @param(BoxSize Width and height of the designated area in relation to top/left origin.)
        @param(ParagraphShift Offset to apply when new text paragraph begins.)
        @param(Text Multi-line text to be drawn.)
        @param(Color Two colors representing vertical gradient to fill the letters with.)
        @param(Alpha A separate alpha representing transparency of the font (in addition to alpha provided in
          @code(Color)).) }
    procedure DrawTextBox(const TopLeft, BoxSize, ParagraphShift: TPoint2; const Text: UniString;
      const Color: TIntColor2; const Alpha: VectorFloat = 1);

    { Provides information regarding individual letter position and sizes for the given text string when rendered.
      This can be useful for components such as text edit box, for highlighting and selecting different letters. }
    procedure TextRects(const Text: UniString; const List: TFloatRectList);

    { Loads binary font from the given stream. This includes both letter information and font image. The given
      pixel format, if specified, will be used as a hint for initializing font letters image. Returns @True when
      successful and @False otherwise. }
    function LoadFromBinaryStream(const Stream: TStream;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Boolean;

    { Loads binary font from external file. This includes both letter information and font image. The given
      pixel format, if specified, will be used as a hint for initializing font letters image. Returns @True when
      successful and @False otherwise. }
    function LoadFromBinaryFile(const FileName: StdString;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Boolean;

    { Loads binary font from file in /assets sub-folder. This includes both letter information and font image.
      The given pixel format, if specified, will be used as a hint for initializing font letters image. Returns @True
      when successful and @False otherwise. }
    function LoadFromBinaryAsset(const AssetName: StdString;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Boolean;

    { Loads font letter information and letter image from their corresponding streams. This uses image format manager
      reference from associated device. The image extension indicates what image format for letters image is used.
      The given pixel format, if specified, will be used as a hint for initializing font letters image.
      Returns @True when successful and @False otherwise. }
    function LoadFromXMLStream(const ImageExtension: StdString; const ImageStream, XMLStream: TStream;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Boolean;

    { Loads font letter information and letter image from external files on disk. This methods accepts that one of
      file names is left empty and will be guessed by either changing extension from ".xml" to ".png" or vice-versa.
      This uses image format manager reference from associated device. The given pixel format, if specified, will be
      used as a hint for initializing font letters image. Returns @True when successful and @False otherwise. }
    function LoadFromXMLFile(const ImageFileName: StdString; const XMLFileName: StdString = '';
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Boolean;

    { Loads font letter information and letter image from files in /assets sub-folder. This methods accepts that one of
      file names is left empty and will be guessed by either changing extension from ".xml" to ".png" or vice-versa.
      This uses image format manager reference from associated device. The given pixel format, if specified, will be
      used as a hint for initializing font letters image. Returns @True when successful and @False otherwise. }
    function LoadFromXMLAsset(const ImageAssetName: StdString; const XMLAssetName: StdString = '';
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Boolean;

  {$IFDEF IncludeSystemFont}
    { Loads and initializes one of the predefined system fonts. These are embedded within the final application and
      can be used at any time. The drawback of this is that these fonts don't look pretty as the pre-rendered ones and
      typically contain only ASCII characters. The given pixel format, if specified, will be used as a hint for
      initializing font letters image. Returns @True when successful and @False otherwise. }
    function LoadSystemFont(const FontImage: TSystemFontImage = TSystemFontImage.Font8x8;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Boolean;
  {$ENDIF}

    { The device to which this font's image is bound to. }
    property Device: TCustomDevice read FDevice;

    { Indicates whether this font's image has textures subscribed to device events, handling scenarios such as
      "device lost". If this is set to @False, likely the handling is provided by parent list class such as
      @link(TBitmapFonts) by calling the appropriate image functions. }
    property SubscribedTextures: Boolean read FSubscribedTextures;

    { Unique name of the font by which it can be referenced in @link(TBitmapFonts) list. }
    property Name: StdString read FName write FName;

    { Individual spacing between each combination of two letters to provide pixel-perfect text rendering. }
    property Kernings: TKernings read FKernings;

    { Destination canvas to which the text should be rendered to. This can be changed between different drawing calls
      to a different canvas, as long as such canvas is bound to the same device as the font. }
    property Canvas: TCustomCanvas read FCanvas write FCanvas;

    { The image containing all available font letters. }
    property Image: TAtlasImage read FImage;

    { Style tags that can be used to provide different colors in large texts. }
    property StyleTags: TStyleTags read FStyleTags write FStyleTags;

    { Font size that defines maximum rectangle that a letter can occupy. }
    property Size: TPoint2px read FSize;

    { Font width that defines maximum width in pixels that a letter can occupy. }
    property Width: VectorInt read FSize.X;

    { Font height that defines maximum height in pixels that a letter can occupy. }
    property Height: VectorInt read FSize.Y;

    { Characters that can be used to separate words in multi-line text drawing with @link(DrawTextBox). }
    property WordSeparators: UniString read FWordSeparators write FWordSeparators;

    { Characters that can be used to indicate start of new paragraph in multi-line text drawing with
      @link(DrawTextBox). }
    property ParagraphSeparators: UniString read FParagraphSeparators write FParagraphSeparators;

    { Global font scale that is applied to the whole rendered text. Changing this value will likely result in
      non-pixel-perfect text rendering appearing blurry. However, it can be used for text animations. }
    property Scale: VectorFloat read FScale write FScale;

    { Global spacing that will be added horizontally between text letters. This can be used to expand or shrink the
      text. }
    property Interleave: VectorFloat read FInterleave write FInterleave;

    { The width in pixels corresponding to "space" or other empty characters (that is, characters without an image). }
    property SpaceWidth: VectorFloat read FSpaceWidth write FSpaceWidth;

    { Global spacing that will be added vertically between text lines when drawing with @link(DrawTextBox). }
    property VerticalSpace: VectorFloat read FVerticalSpace write FVerticalSpace;
  end;

  { The list that may contain one or more instances of @link(TBitmapFont) and provide facilities to search for fonts
    by their unique names, font loading and handling "device lost" scenario. }
  TBitmapFonts = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FCanvas: TCustomCanvas;

    FFonts: array of TBitmapFont;
    FStyleTags: TBitmapFont.TStyleTags;

    FSearchList: array of Integer;
    FSearchListDirty: Boolean;

    FDeviceRestoreHandle: Cardinal;
    FDeviceReleaseHandle: Cardinal;

    function GetCount: Integer;
    function GetItem(const Index: Integer): TBitmapFont;

    function InsertFont: Integer;
    function GetFont(const Name: StdString): TBitmapFont;

    procedure OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
    procedure OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);

    procedure InitSearchList;
    procedure SwapSearchList(const Index1, Index2: Integer);
    function CompareSearchList(const Index1, Index2: Integer): Integer;
    function SplitSearchList(const Start, Stop: Integer): Integer;
    procedure SortSearchList(const Start, Stop: Integer);
    procedure UpdateSearchList;
    procedure SetCanvas(const Value: TCustomCanvas);
  public
    { Creates new instance of @code(TBitmapFonts) class bound to the specified device. Elements inside this list will
      also be bound to this device. }
    constructor Create(const ADevice: TCustomDevice);
    { @exclude } destructor Destroy; override;

    { Loads binary font from the given stream and adds its to the list with the given name. The given pixel format,
      if specified, will be used as a hint for initializing font letters image. Returns font index in the list when
      successful and -1 otherwise. }
    function AddFromBinaryStream(const Stream: TStream; const FontName: StdString = '';
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

    { Loads binary font from external file and adds its to the list with font name equal to file name (without
      extension). The given pixel format, if specified, will be used as a hint for initializing font letters image.
      Returns font index in the list when successful and -1 otherwise. }
    function AddFromBinaryFile(const FileName: StdString;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

    { Loads binary font from file in /assets sub-folder and adds its to the list with font name equal to file name
      (without extension). The given pixel format, if specified, will be used as a hint for initializing font letters
      image. Returns font index in the list when successful and -1 otherwise. }
    function AddFromBinaryAsset(const AssetName: StdString;
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

    { Loads font letter information and letter image from their corresponding streams and adds the resulting font to
      the list with the given name. This uses image format manager reference from associated device. The image
      extension indicates what image format for letters image is used. The given pixel format, if specified, will be
      used as a hint for initializing font letters image. Returns font index in the list when successful and -1
      otherwise. }
    function AddFromXMLStream(const ImageExtension: StdString; const ImageStream, XMLStream: TStream;
      const FontName: StdString = ''; const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

    { Loads font letter information and letter image from external files on disk and adds the resulting font to
      the list with the name equal to that of file name (without extension). This methods accepts that one of
      file names is left empty and will be guessed by either changing extension from ".xml" to ".png" or vice-versa.
      This uses image format manager reference from associated device. The given pixel format, if specified, will be
      used as a hint for initializing font letters image. Returns font index in the list when successful and -1
      otherwise. }
    function AddFromXMLFile(const ImageFileName: StdString; const XMLFileName: StdString = '';
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

    { Loads font letter information and letter image from external files in /assets sub-folder and adds the resulting
      font to the list with the name equal to that of file name (without extension). This methods accepts that one of
      file names is left empty and will be guessed by either changing extension from ".xml" to ".png" or vice-versa.
      This uses image format manager reference from associated device. The given pixel format, if specified, will be
      used as a hint for initializing font letters image. Returns font index in the list when successful and -1
      otherwise. }
    function AddFromXMLAsset(const ImageAssetName: StdString; const XMLAssetName: StdString = '';
      const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;

  {$IFDEF IncludeSystemFont}
    { Adds and initializes one of the predefined system fonts to the list with the given name. These are embedded
      within the final application and can be used at any time. The drawback of this is that these fonts don't look
      pretty as the pre-rendered ones and typically contain only ASCII characters. The given pixel format, if specified,
      will be used as a hint for initializing font letters image. Returns font index in the list when successful and -1
      otherwise. }
    function AddSystemFont(const FontImage: TSystemFontImage = TSystemFontImage.Font8x8;
      const FontName: StdString = ''; const PixelFormat: TPixelFormat = TPixelFormat.Unknown): Integer;
  {$ENDIF}

    { Returns the index of font with given unique name (not case-sensitive) in the list. If no font with such name
      exists, returns -1. }
    function IndexOf(const FontName: StdString): Integer; overload;

    { Returns the index of the given font element in the list. If such font is not found, -1 is returned. }
    function IndexOf(const Element: TBitmapFont): Integer; overload;

    { Inserts the given font to the list and returns its index. }
    function Insert(const Font: TBitmapFont): Integer;

    { Includes the specified font to the list, if it wasn't included previously. This implies searching the list
      before adding the element, which may impact performance. }
    function Include(const Element: TBitmapFont): Integer;

    { Removes font with the specified index from the list. }
    procedure Remove(const Index: Integer);

    { Removes all font entries from the list. }
    procedure Clear;

    { Indicates that one of the fonts had its name changed, so the list needs to be refreshed to make searching by
      name (@link(IndexOf) function and @link(Font) property) work properly. }
    procedure MarkSearchDirty;

    { Global font style tags that can be shared among all fonts contained in this list. }
    property StyleTags: TBitmapFont.TStyleTags read FStyleTags;

    { The device to which this list is bound to. }
    property Device: TCustomDevice read FDevice;

    { Destination canvas to which the text should be rendered to. This can be changed between different drawing calls
      to a different canvas, as long as such canvas is bound to the same device as the fonts. Note that setting this
      property changes @code(Canvas) value for all the fonts in the list to this same value. }
    property Canvas: TCustomCanvas read FCanvas write SetCanvas;

    { Total number of elements in the list. }
    property Count: Integer read GetCount;

    { Provides access to individual fonts in the list by the corresponding index. If the index is outside of valid
      range, @nil is returned. }
    property Items[const Index: Integer]: TBitmapFont read GetItem; default;

    { Provides access to individual fonts in the list by unique font name (not case-sensitive). If no font with such
      name is found, @nil is returned. }
    property Font[const Name: StdString]: TBitmapFont read GetFont;
  end;

implementation

uses
  SysUtils, Math, PXL.Consts, PXL.Logs, PXL.Classes, PXL.Formats;

{$IFDEF IncludeSystemFont}
  {$INCLUDE PXL.SystemFont.inc}
{$ENDIF}

{$REGION 'TBitmapFont.TStyleTags'}

class function TBitmapFont.TStyleTags.UniCompareText(const Text1, Text2: UniString): Integer;
begin
{$IFDEF DELPHI_2009_UP}
  Result := CompareText(Text1, Text2);
{$ELSE}
  Result := WideCompareText(Text1, Text2);
{$ENDIF}
end;

procedure TBitmapFont.TStyleTags.Insert(const Name: UniString; const Color: TIntColor2; const Style: Cardinal);
var
  Index: Integer;
begin
  Index := Length(FEntries);
  SetLength(FEntries, Index + 1);

  FEntries[Index].Name  := Name;
  FEntries[Index].Color := Color;
  FEntries[Index].Style := Style;

  FEntriesDirty := True;
end;

procedure TBitmapFont.TStyleTags.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FEntries)) then
    Exit;

  for I := Index to Length(FEntries) - 2 do
    FEntries[I] := FEntries[I + 1];

  SetLength(FEntries, Length(FEntries) - 1);

  FEntriesDirty := True;
end;

procedure TBitmapFont.TStyleTags.Delete(const Name: UniString);
begin
  Remove(IndexOf(Name));
end;

procedure TBitmapFont.TStyleTags.Clear;
begin
  SetLength(FEntries, 0);
  FEntriesDirty := False;
end;

procedure TBitmapFont.TStyleTags.SwapEntries(const Index1, Index2: Integer);
var
  TempValue: TEntry;
begin
  TempValue := FEntries[Index1];
  FEntries[Index1] := FEntries[Index2];
  FEntries[Index2] := TempValue;
end;

function TBitmapFont.TStyleTags.SortSplitEntries(const Start, Stop: Integer): Integer;
var
  Left, Right: Integer;
  Pivot: UniString;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FEntries[Start].Name;

  while Left <= Right do
  begin
    while (Left <= Stop) and (UniCompareText(FEntries[Left].Name, Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (UniCompareText(FEntries[Right].Name, Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      SwapEntries(Left, Right);
  end;

  SwapEntries(Start, Right);

  Result:= Right;
end;

procedure TBitmapFont.TStyleTags.QuickSortEntries(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt:= SortSplitEntries(Start, Stop);

    QuickSortEntries(Start, SplitPt - 1);
    QuickSortEntries(SplitPt + 1, Stop);
  end;
end;

function TBitmapFont.TStyleTags.IndexOf(const Name: UniString): Integer;
var
  Left, Right, Middle, Res: Integer;
begin
  if FEntriesDirty then
  begin
    QuickSortEntries(0, Length(FEntries) - 1);
    FEntriesDirty := False;
  end;

  Result := -1;

  Left := 0;
  Right := Length(FEntries) - 1;

  while Left <= Right do
  begin
    Middle := (Left + Right) div 2;
    Res := UniCompareText(FEntries[Middle].Name, Name);

    if Res = 0 then
    begin
      Result:= Middle;
      Break;
    end;

    if Res > 0 then
      Right:= Middle - 1
    else
      Left:= Middle + 1;
  end;
end;

function TBitmapFont.TStyleTags.Find(const Name: UniString): PEntry;
var
  Index: Integer;
begin
  Index := IndexOf(Name);

  if Index <> -1 then
    Result := @FEntries[Index]
  else
    Result := nil;
end;

{$ENDREGION}
{$REGION 'TBitmapFont.TKernings'}

constructor TBitmapFont.TKernings.Create;
begin
  inherited;

  FExtArray := TPointList.Create;
  FillChar(FHashArray, SizeOf(FHashArray), 0);
end;

destructor TBitmapFont.TKernings.Destroy;
begin
  FExtArray.Free;

  inherited;
end;

function TBitmapFont.TKernings.GetShift(const Code1, Code2: Integer): Integer;
var
  Index: Integer;
begin
  if (Code1 < 0) or (Code2 < 0) then
    Exit(0);

  if (Code1 <= 255) and (Code2 <= 255) then
    Exit(FHashArray[Code1, Code2]);

  Index := FExtArray.IndexOf(Point2px(Code1, Code2));

  if Index <> -1 then
    Result := PtrInt(FExtArray[Index].Data)
  else
    Result := 0;
end;

procedure TBitmapFont.TKernings.Spec(const Code1, Code2: Integer; Shift: Integer);
var
  Pos: TPoint2px;
  Index: Integer;
begin
  if (Code1 < 0) or (Code2 < 0) then
    Exit;

  Shift := Saturate(Shift, -128,  127);

  if (Code1 <= 255) and (Code2 <= 255) then
  begin
    FHashArray[Code1, Code2] := Shift;
    Exit;
  end;

  Pos := Point2px(Code1, Code2);

  Index := FExtArray.IndexOf(Pos);
  if Index = -1 then
    Index := FExtArray.Add(Pos);

  FExtArray[Index].Data := Pointer(Shift);
end;

procedure TBitmapFont.TKernings.Spec(const Code1, Code2: WideChar; const Shift: Integer);
begin
  Spec(Ord(Code1), Ord(Code2), Shift);
end;

{$ENDREGION}
{$REGION 'TBitmapFont Callbacks'}

procedure DrawTextCallback(const Sender: TObject; const FontImage: TAtlasImage; const SourceRect: TIntRect;
  const DestRect: TFloatRect; const Color: TIntColor4; const UserContext: Pointer);
begin
  TCustomCanvas(UserContext).UseReflectionPx2(nil, floatrect4(0,0,0,0), 0);
  TCustomCanvas(UserContext).UseImagePx(FontImage, FloatRect4(SourceRect));
  TCustomCanvas(UserContext).TexQuad(FloatRect4(DestRect), Color);
end;

{$ENDREGION}
{$REGION 'TBitmapFont'}

constructor TBitmapFont.Create(const ADevice: TCustomDevice; const ASubscribedTextures: Boolean);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FDevice := ADevice;
  FSubscribedTextures := ASubscribedTextures;

  FKernings := TKernings.Create;

  FScale := 1.0;
  FInterleave := -1;
  FSpaceWidth := 5.0;
  FVerticalSpace := 2.0;

  FParagraphSeparators := #10;
  FWordSeparators := #13#32#8;
end;

destructor TBitmapFont.Destroy;
begin
  try
    FImage.Free;
    FKernings.Free;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TBitmapFont.RecreateImage(const TextureSize: TPoint2px; const PixelFormat: TPixelFormat): Boolean;
var
  Texture: TCustomLockableTexture;
begin
  if FImage = nil then
  begin
    FImage := TAtlasImage.Create(FDevice, FSubscribedTextures);
    FImage.DynamicImage := False;
    FImage.MipMapping := False;
    FImage.PixelFormat := PixelFormat;
  end;

  Texture := nil;

  if FImage.TextureCount = 1 then
  begin
    Texture := FImage.Texture[0];
    if Texture <> nil then
      if Texture.Size <> TextureSize then
        Texture := nil;
  end;

  if Texture = nil then
  begin
    FImage.ClearTextures;
    Texture := FImage.InsertTexture(TextureSize.X, TextureSize.Y);
  end;

  Result := Texture <> nil;
end;

function TBitmapFont.ReadFontEntriesFromStream(const Stream: TStream): Boolean;
var
  I, GlyphCount, CharCode: Integer;
begin
  FillChar(FEntries, SizeOf(FEntries), 0);

  try
    // Font Width and Height
    FSize.X := Stream.GetWord;
    FSize.Y := Stream.GetWord;

    // Width of space or blank characters.
    FSpaceWidth := Stream.GetSmallInt;

    // Number of Glyphs
    GlyphCount := Stream.GetLongInt;

    // Individual Glyphs
    for I := 0 to GlyphCount - 1 do
    begin
      // Character Code
      CharCode := Stream.GetWord;
      if (CharCode < 0) or (CharCode > High(Word)) then
        Exit(False);

      with FEntries[CharCode] do
      begin
        // Vertical Margins
        TopBase := Stream.GetSmallInt;
        BottomBase := Stream.GetSmallInt;

        // Glyph Position
        MapLeft := Stream.GetLongInt;
        MapTop := Stream.GetLongInt;

        // Glyph Size
        MapWidth := Stream.GetWord;
        MapHeight := Stream.GetWord;

        // Horizontal Placement Margins
        LeadingSpace := Stream.GetSmallInt;
        TrailingSpace := Stream.GetSmallInt;
      end;
    end;
  except
    Exit(False);
  end;

  Result := True;
end;

function TBitmapFont.ReadTextureFromStream(const Stream: TStream; const Texture: TCustomLockableTexture;
  const DataFormat: TPixelFormat; const ActualHeight: Integer): Boolean;
var
  DataBits, TempBits: Pointer;
  I, DataBytes, TempBytes: Integer;
  LockedPixels: TLockedPixels;
begin
  if Texture = nil then
    Exit(False);

  if not Texture.Lock(LockedPixels) then
    Exit(False);
  try
    try
      DataBytes := Texture.Width * DataFormat.Bytes;

      if Texture.PixelFormat = DataFormat then
      begin // Fast native copy.
        for I := 0 to Texture.Height - 1 do
        begin
          if I < ActualHeight then
            Stream.Read(LockedPixels.Scanline[I]^, DataBytes)
          else
            FillChar(LockedPixels.Scanline[I]^, DataBytes, 0);
        end;
      end
      else
      begin // Pixel format conversion is required.
        GetMem(DataBits, DataBytes);
        try
          if DataFormat = TPixelFormat.A8R8G8B8 then
          begin // 32-bit RGBA to custom format.
            for I := 0 to Texture.Height - 1 do
            begin
              if I < ActualHeight then
                Stream.Read(DataBits^, DataBytes)
              else
                FillChar(DataBits^, DataBytes, 0);

              Pixel32ToXArray(DataBits, LockedPixels.Scanline[I], Texture.PixelFormat, Texture.Width);
            end;
          end
          else if Texture.PixelFormat = TPixelFormat.A8R8G8B8 then
          begin // Custom format to 32-bit RGBA.
            for I := 0 to Texture.Height - 1 do
            begin
              if I < ActualHeight then
                Stream.Read(DataBits^, DataBytes)
              else
                FillChar(DataBits^, DataBytes, 0);

              PixelXTo32Array(DataBits, LockedPixels.Scanline[I], DataFormat, Texture.Width);
            end;
          end
          else
          begin // Custom to custom pixel format.
            TempBytes := Texture.Width * SizeOf(TIntColor);
            GetMem(TempBits, TempBytes);
            try
              for I := 0 to Texture.Height - 1 do
              begin
                if I < ActualHeight then
                  Stream.Read(DataBits^, DataBytes)
                else
                  FillChar(DataBits^, DataBytes, 0);

                PixelXTo32Array(DataBits, TempBits, DataFormat, Texture.Width);
                Pixel32ToXArray(TempBits, LockedPixels.Scanline[I], Texture.PixelFormat, Texture.Width);
              end;
            finally
              FreeMem(TempBits);
            end;
          end;
        finally
          FreeMem(DataBits);
        end;
      end;
    except
      Exit(False);
    end;
  finally
    Texture.Unlock;
  end;

  Result := True;
end;

function TBitmapFont.ReadFontImageFromStream(const Stream: TStream; const PixelFormat: TPixelFormat): Boolean;
var
  SkippedLines, ActualHeight: Integer;
  TextureSize: TPoint2px;
  DataFormat, TextureFormat: TPixelFormat;
  Texture: TCustomLockableTexture;
begin
  try
    // Texture Width and Height
    TextureSize.X := Stream.GetLongInt;
    TextureSize.Y := Stream.GetLongInt;

    if (TextureSize.X < 1) or (TextureSize.Y < 1) then
      Exit(False);

    // Texture Pixel Format
    DataFormat := TPixelFormat(Stream.GetByte);
    if (DataFormat < Low(TPixelFormat)) or (DataFormat > High(TPixelFormat)) or
      (DataFormat = TPixelFormat.Unknown) then
      Exit(False);

    TextureFormat := PixelFormat;
    if TextureFormat = TPixelFormat.Unknown then
      TextureFormat := DataFormat;

    if not RecreateImage(TextureSize, TextureFormat) then
      Exit(False);

    if (FImage = nil) or (FImage.TextureCount < 1) then
      Exit(False);

    Texture := FImage.Texture[0];
    if Texture = nil then
      Exit(False);

    // Texture Lines that are skipped
    SkippedLines := Stream.GetLongInt;
    if SkippedLines > 0 then
      ActualHeight := Max(TextureSize.Y - SkippedLines, 0)
    else
      ActualHeight := TextureSize.Y;

    // Texture Data
    Result := ReadTextureFromStream(Stream, Texture, DataFormat, ActualHeight);
  except
    Exit(False);
  end;
end;

{$IFDEF IncludeSystemFont}
function TBitmapFont.ReadTextureFromSystemFont(const Texture: TCustomLockableTexture): Boolean;

  function IsPixelOpaque(const X, Y: Integer): Boolean; inline;
  begin
    Result := SystemFont8x8[(Y shl 4) + (X shr 3)] and (1 shl (X and $07)) > 0;
  end;

var
  TempBits: Pointer;
  I, J, TempBytes, BlockNo: Integer;
  LockedPixels: TLockedPixels;
  DestPixel: PIntColor;
begin
  if Texture = nil then
    Exit(False);

  if not Texture.Lock(LockedPixels) then
    Exit(False);
  try
    try
      TempBytes := Texture.Width * SizeOf(TIntColor);
      GetMem(TempBits, TempBytes);
      try
        for J := 0 to Texture.Height - 1 do
        begin
          DestPixel := TempBits;

          if Texture.Size.X = 144 then
            for BlockNo := 0 to 15 do
            begin // 9x8 font
              for I := 0 to 7 do
              begin
                if IsPixelOpaque(BlockNo * 8 + I, J) then
                  DestPixel^ := IntColorWhite
                else
                  DestPixel^ := IntColorTranslucentBlack;

                Inc(DestPixel);
              end;

              // Repeat last pixel
              if IsPixelOpaque(BlockNo * 8 + 7, J) then
                DestPixel^ := IntColorWhite
              else
                DestPixel^ := IntColorTranslucentBlack;

              Inc(DestPixel);
            end
          else
            for I := 0 to Texture.Width - 1 do
            begin // 8x8 font
              if IsPixelOpaque(I, J) then
                DestPixel^ := IntColorWhite
              else
                DestPixel^ := IntColorTranslucentBlack;

              Inc(DestPixel);
            end;

          Pixel32ToXArray(TempBits, LockedPixels.Scanline[J], Texture.PixelFormat, Texture.Width);
        end;
      finally
        FreeMem(TempBits);
      end;
    except
      Exit(False);
    end;
  finally
    Texture.Unlock;
  end;

  Result := True;
end;
{$ENDIF}

function TBitmapFont.LoadFromBinaryStream(const Stream: TStream; const PixelFormat: TPixelFormat): Boolean;
begin
  if Stream = nil then
    Exit(False);

  if not ReadFontEntriesFromStream(Stream) then
    Exit(False);

  Result := ReadFontImageFromStream(Stream, PixelFormat);
end;

function TBitmapFont.LoadFromBinaryFile(const FileName: StdString; const PixelFormat: TPixelFormat): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadFromBinaryStream(Stream, PixelFormat);
    finally
      Stream.Free;
    end;
  except
    Exit(False);
  end;

  if Result then
    FName := ChangeFileExt(FileName, '');
end;

function TBitmapFont.LoadFromBinaryAsset(const AssetName: StdString; const PixelFormat: TPixelFormat): Boolean;
var
  Stream: TAssetStream;
begin
  try
    Stream := TAssetStream.Create(AssetName);
    try
      Result := LoadFromBinaryStream(Stream, PixelFormat);
    finally
      Stream.Free;
    end;
  except
    Exit(False);
  end;

  if Result then
    FName := ChangeFileExt(AssetName, '');
end;

function TBitmapFont.ReadEntryFromXML(const Node: TXMLNode): Boolean;
var
  CharCode: Integer;
begin
  CharCode := StrToIntDef(Node.FieldValue['ascii'], -1);

  if (CharCode < 0) or (CharCode > 255) then
    CharCode := StrToIntDef(Node.FieldValue['ucode'], -1);

  if (CharCode < 0) or (CharCode > High(Word)) then
    Exit(False);

  with FEntries[CharCode] do
  begin
    TopBase := Saturate(StrToIntDef(Node.FieldValue['top'], 0), 0, 255);
    BottomBase := Saturate(StrToIntDef(Node.FieldValue['bottom'], 0), 0, 255);
    MapLeft := Saturate(StrToIntDef(Node.FieldValue['x'], 0), 0, 65535);
    MapTop := Saturate(StrToIntDef(Node.FieldValue['y'], 0), 0, 65535);
    MapWidth := Saturate(StrToIntDef(Node.FieldValue['width'], 0), 0, 255);
    MapHeight := Saturate(StrToIntDef(Node.FieldValue['height'], 0), 0, 255);
    LeadingSpace := Saturate(StrToIntDef(Node.FieldValue['leading'], 0), -128, 127);
    TrailingSpace := Saturate(StrToIntDef(Node.FieldValue['trailing'], 0), -128, 127);
  end;

  Result := True;
end;

function TBitmapFont.ReadFromXMLNode(const Node: TXMLNode): Boolean;
var
  Child: TXMLNode;
begin
  Result := False;

  FSize.X := StrToIntDef(Node.FieldValue['width'], 0);
  FSize.Y := StrToIntDef(Node.FieldValue['height'], 0);

  FSpaceWidth := StrToIntDef(Node.FieldValue['space'], 0);
  if FSpaceWidth <= 0 then
    FSpaceWidth := FSize.X div 4;

  for Child in Node do
    if SameText(Child.Name, 'item') then
    begin
      Result := ReadEntryFromXML(Child);
      if not Result then
        Break;
    end;
end;

function TBitmapFont.LoadEntriesFromXMLStream(const Stream: TStream): Boolean;
var
  Node: TXMLNode;
begin
  Node := LoadXMLFromStream(Stream);
  if Node = nil then
    Exit(False);

  try
    Result := ReadFromXMLNode(Node);
  finally
    Node.Free;
  end;
end;

function TBitmapFont.LoadEntriesFromXMLFile(const FileName: StdString): Boolean;
var
  Node: TXMLNode;
begin
  Node := LoadXMLFromFile(FileName);
  if Node = nil then
    Exit(False);

  try
    Result := ReadFromXMLNode(Node);
  finally
    Node.Free;
  end;
end;

function TBitmapFont.LoadEntriesFromXMLAsset(const AssetName: StdString): Boolean;
var
  Node: TXMLNode;
begin
  Node := LoadXMLFromAsset(AssetName);
  if Node = nil then
    Exit(False);

  try
    Result := ReadFromXMLNode(Node);
  finally
    Node.Free;
  end;
end;

function TBitmapFont.LoadFromXMLStream(const ImageExtension: StdString; const ImageStream, XMLStream: TStream;
  const PixelFormat: TPixelFormat): Boolean;
begin
  if (ImageStream = nil) or (XMLStream = nil) then
    Exit(False);

  if FImage = nil then
  begin
    FImage := TAtlasImage.Create(FDevice, FSubscribedTextures);
    FImage.PixelFormat := PixelFormat;
    FImage.DynamicImage := False;
    FImage.MipMapping := False;
  end;

  if not FImage.LoadFromStream(ImageExtension, ImageStream) then
    Exit(False);

  Result := LoadEntriesFromXMLStream(XMLStream);
  if not Result then
    FreeAndNil(FImage);
end;

function TBitmapFont.LoadFromXMLFile(const ImageFileName: StdString;
  const XMLFileName: StdString; const PixelFormat: TPixelFormat): Boolean;
var
  Text: StdString;
begin
  if (Length(ImageFileName) < 1) and (Length(XMLFileName) < 1) then
    Exit(False);

  if FImage = nil then
  begin
    FImage := TAtlasImage.Create(FDevice, FSubscribedTextures);
    FImage.PixelFormat := PixelFormat;
    FImage.DynamicImage := False;
    FImage.MipMapping := False;
  end;

  Text := ImageFileName;
  if Length(Text) < 1 then
    Text := ChangeFileExt(XMLFileName, DefaultImageExtension);

  if not FImage.LoadFromFile(Text) then
    Exit(False);

  Text := XMLFileName;
  if Length(Text) < 1 then
    Text := ChangeFileExt(ImageFileName, DefaultXMLExtension);

  Result := LoadEntriesFromXMLFile(Text);
  if Result then
    FName := ChangeFileExt(Text, '')
  else
    FreeAndNil(FImage);
end;

function TBitmapFont.LoadFromXMLAsset(const ImageAssetName: StdString;
  const XMLAssetName: StdString; const PixelFormat: TPixelFormat): Boolean;
var
  Text: StdString;
begin
  if (Length(ImageAssetName) < 1) and (Length(XMLAssetName) < 1) then
    Exit(False);

  if FImage = nil then
  begin
    FImage := TAtlasImage.Create(FDevice, FSubscribedTextures);
    FImage.PixelFormat := PixelFormat;
    FImage.DynamicImage := False;
    FImage.MipMapping := False;
  end;

  Text := ImageAssetName;
  if Length(Text) < 1 then
    Text := ChangeFileExt(XMLAssetName, DefaultImageExtension);

  if not FImage.LoadFromAsset(Text) then
    Exit(False);

  Text := XMLAssetName;
  if Length(Text) < 1 then
    Text := ChangeFileExt(ImageAssetName, DefaultXMLExtension);

  Result := LoadEntriesFromXMLAsset(Text);
  if Result then
    FName := ChangeFileExt(Text, '')
  else
    FreeAndNil(FImage);
end;

{$IFDEF IncludeSystemFont}
function TBitmapFont.LoadSystemFont(const FontImage: TSystemFontImage; const PixelFormat: TPixelFormat): Boolean;
var
  FontSize: TPoint2px;
  Texture: TCustomLockableTexture;
  I: Integer;
begin
  if FontImage = TSystemFontImage.Font9x8 then
    FontSize := Point2px(144, 128)
  else
    FontSize := Point2px(128, 128);

  FillChar(FEntries, SizeOf(FEntries), 0);

  if FontImage = TSystemFontImage.Font9x8 then
  begin
    FSize := Point2px(9, 8);
    FInterleave := 0;
  end
  else
  begin
    FSize := Point2px(8, 8);
    FInterleave := 1;
  end;

  FSpaceWidth := 4;
  FVerticalSpace := 0;

  for I := 0 to 255 do
    with FEntries[I] do
    begin
      if FontImage = TSystemFontImage.Font9x8 then
      begin
        MapLeft := (I mod 16) * 9 + SystemFont8x8Starts[I];
        MapWidth := Max(1 + Integer(SystemFont8x8Ends[I]) - Integer(SystemFont8x8Starts[I]), 0);

        if MapWidth > 0 then
          Inc(MapWidth);
      end
      else
      begin
        MapLeft := (I mod 16) * 8 + SystemFont8x8Starts[I];
        MapWidth := Max(1 + Integer(SystemFont8x8Ends[I]) - Integer(SystemFont8x8Starts[I]), 0);
      end;

      MapTop := (I div 16) * 8;
      MapHeight := 8;
    end;

  if not RecreateImage(FontSize, PixelFormat) then
    Exit(False);

  if (FImage = nil) or (FImage.TextureCount < 1) then
    Exit(False);

  Texture := FImage.Texture[0];
  if Texture = nil then
    Exit(False);

  if not ReadTextureFromSystemFont(Texture) then
    Exit(False);

  Result := True;
end;
{$ENDIF}

procedure TBitmapFont.ClearStyleStates;
begin
  FStyleStateCount := 0;
end;

procedure TBitmapFont.PushStyleState(const Color: TIntColor2; const Style: Cardinal);
begin
  if FStyleStateCount < StyleStackCount then
  begin
    FStyleStates[FStyleStateCount].Color := Color;
    FStyleStates[FStyleStateCount].Style := Style;
  end;

  Inc(FStyleStateCount);
end;

function TBitmapFont.PeekStyleState: PStyleState;
begin
  if (FStyleStateCount > 0) and (FStyleStateCount <= StyleStackCount) then
    Result := @FStyleStates[FStyleStateCount - 1]
  else
    Result := nil;
end;

procedure TBitmapFont.PopStyleState;
begin
  if FStyleStateCount > 0 then
    Dec(FStyleStateCount);
end;

function TBitmapFont.ParseTag(const Text: UniString; var CharIndex: Integer; const IgnoreStyles: Boolean): Boolean;
var
  Entry: TStyleTags.PEntry;
  TextStartPos, TextSize, PrevCharIndex: Integer;
  TagName: UniString;
begin
  PrevCharIndex := CharIndex;

  // -> Check whether there is a tag.
  if Text[CharIndex] <> '<' then
    Exit(False);

  // -> Check for invalid "<" tag at the end of string.
  if CharIndex >= Length(Text) then
    Exit(False);

  // "<<" indicates "<".
  if Text[CharIndex + 1] = '<' then
  begin
    Inc(CharIndex);
    Exit(False);
  end;

  // -> Mark the beginning of tag text.
  Inc(CharIndex);

  TextStartPos := CharIndex;
  TextSize := 0;

  // -> Scan for the end of the tag.
  while (CharIndex <= Length(Text)) and (Text[CharIndex] <> '>') do
  begin
    Inc(TextSize);
    Inc(CharIndex);
  end;

  // -> Check if tag was not closed at the end of string.
  if CharIndex > Length(Text) then
  begin
    CharIndex := PrevCharIndex;
    Exit(False);
  end;

  // -> Skip ">" letter.
  Inc(CharIndex);
  Result := True;

  if IgnoreStyles then
    Exit;

  // -> Extract tag name from the string.
  TagName := Copy(Text, TextStartPos, TextSize);
  if TagName = '/' then
  begin
    PopStyleState;
    Exit;
  end;

  if FStyleTags <> nil then
  begin
    Entry := FStyleTags.Find(TagName);
    if Entry <> nil then
      PushStyleState(Entry.Color, Entry.Style);
  end;
end;

function TBitmapFont.DrawTextCustom(const Position: TPoint2; const Text: UniString; const Color: TIntColor2;
  const Alpha: VectorFloat; const TextEntryEvent: TTextEntryEvent; const UserContext: Pointer;
  const RestartStyling: Boolean): Boolean;
var
  CharEntry: PLetterEntry;
  CharIndex, CharCode, PrevCode, IntAlpha: Integer;
  DrawPos: VectorFloat;
  DrawRect: TFloatRect;
  CharColor: TIntColor2;
  Style: PStyleState;
begin
  if (FImage = nil) or (not Assigned(TextEntryEvent)) then
    Exit(False);

  if RestartStyling then
  begin
    ClearStyleStates;
    PushStyleState(Color);
  end;

  DrawPos := Position.X;
  CharIndex := 1;
  PrevCode := -1;
  IntAlpha := Saturate(Round(Alpha * 255), 0, 255);

  while CharIndex <= Length(Text) do
  begin
    if ParseTag(Text, CharIndex, False) then
      Continue;

    CharCode := Ord(Text[CharIndex]);
    CharEntry := @FEntries[CharCode];
    Style := PeekStyleState;

    if (Style = nil) or (CharEntry.MapWidth < 1) or (CharEntry.MapHeight < 1) then
    begin
      Inc(CharIndex);
      DrawPos := DrawPos + FSpaceWidth * FScale;
      Continue;
    end;

    if PrevCode <> -1 then
      DrawPos := DrawPos + FKernings[PrevCode, CharCode];

    PrevCode := CharCode;

    DrawPos := DrawPos + CharEntry.LeadingSpace * FScale;

    DrawRect.Left := DrawPos;
    DrawRect.Top := Position.Y + CharEntry.TopBase * FScale;
    DrawRect.Right := DrawRect.Left + CharEntry.MapWidth * FScale;
    DrawRect.Bottom := DrawRect.Top + CharEntry.MapHeight * FScale;

    CharColor.First := IntColor(LerpPixels(Style.Color.First, Style.Color.Second, CharEntry.TopBase / FSize.Y),
      IntAlpha);
    CharColor.Second := IntColor(LerpPixels(Style.Color.First, Style.Color.Second, (CharEntry.TopBase +
      CharEntry.MapHeight) / FSize.Y), IntAlpha);

    TextEntryEvent(Self, FImage, IntRect(CharEntry.MapLeft, CharEntry.MapTop, CharEntry.MapWidth,
      CharEntry.MapHeight), DrawRect, IntColor4(CharColor.First, CharColor.First, CharColor.Second,
      CharColor.Second) , UserContext);

    Inc(CharIndex);
    DrawPos := DrawPos + (CharEntry.MapWidth + CharEntry.TrailingSpace + FInterleave) * FScale;
  end;

  if RestartStyling then
    ClearStyleStates;

  Result := True;
end;

function TBitmapFont.TextExtent(const Text: UniString): TPoint2;
var
  CharEntry: PLetterEntry;
  CharIndex, CharCode, PrevCode: Integer;
  KerningAdjust: Boolean;
begin
  CharIndex := 1;
  PrevCode := -1;

  Result.X := 0;
  Result.Y := FSize.Y * FScale;

  KerningAdjust := False;

  while CharIndex <= Length(Text) do
  begin
    if ParseTag(Text, CharIndex, True) then
      Continue;

    CharCode := Ord(Text[CharIndex]);
    CharEntry := @FEntries[CharCode];

    if (CharEntry.MapWidth < 1) or (CharEntry.MapHeight < 1) then
    begin
      Inc(CharIndex);

      Result.X := Result.X + FSpaceWidth * FScale;
      Continue;
    end;

    Inc(CharIndex);

    Result.X := Result.X + FKernings[PrevCode, CharCode];
    PrevCode := CharCode;

    Result.X := Result.X + (CharEntry.MapWidth + CharEntry.LeadingSpace + CharEntry.TrailingSpace +
      FInterleave) * FScale;

    KerningAdjust := True;
  end;

  if KerningAdjust then
    Result.X := Result.X - FInterleave * FScale;
end;

procedure TBitmapFont.TextRects(const Text: UniString; const List: TFloatRectList);
var
  CharIndex, CharCode, PrevCode: Integer;
  CharEntry: PLetterEntry;
  RectPos: VectorFloat;
  Rect: TFloatRect;
begin
  RectPos := 0;
  CharIndex := 1;
  PrevCode := -1;

  Rect.Top := 0;
  Rect.Bottom := FSize.Y * FScale;

  while CharIndex <= Length(Text) do
  begin
    if ParseTag(Text, CharIndex, False) then
      Continue;

    CharCode := Ord(Text[CharIndex]);
    CharEntry := @FEntries[CharCode];

    if (CharEntry.MapWidth < 1) or (CharEntry.MapHeight < 1) then
    begin
      Inc(CharIndex);

      Rect.Left := RectPos;
      Rect.Right := RectPos + FSpaceWidth * FScale;
      List.Add(Rect);

      RectPos := RectPos + FSpaceWidth * FScale;
      Continue;
    end;

    RectPos := RectPos + FKernings[PrevCode, CharCode];
    PrevCode := CharCode;

    RectPos := RectPos + CharEntry.LeadingSpace * FScale;

    Rect.Left := RectPos;
    Rect.Right := RectPos + (CharEntry.MapWidth + CharEntry.TrailingSpace) * FScale;
    List.Add(Rect);

    Inc(CharIndex);
    RectPos := RectPos + (CharEntry.MapWidth + CharEntry.TrailingSpace + FInterleave) * FScale;
  end;
end;

function TBitmapFont.TextWidth(const Text: UniString): VectorFloat;
begin
  Result := TextExtent(Text).X;
end;

function TBitmapFont.TextHeight(const Text: UniString): VectorFloat;
begin
  Result := TextExtent(Text).Y;
end;

function TBitmapFont.TextExtentInt(const Text: UniString): TPoint2px;
var
  Ext: TPoint2;
begin
  Ext := TextExtent(Text);

  Result.X := Round(Ext.X);
  Result.Y := Round(Ext.Y);
end;

function TBitmapFont.TextWidthInt(const Text: UniString): VectorInt;
begin
  Result := TextExtentInt(Text).X;
end;

function TBitmapFont.TextHeightInt(const Text: UniString): VectorInt;
begin
  Result := TextExtentInt(Text).Y;
end;

procedure TBitmapFont.DrawText(const Position: TPoint2; const Text: UniString; const Color: TIntColor2;
  const Alpha: VectorFloat);
begin
  if FCanvas <> nil then
    DrawTextCustom(Position, Text, Color, Alpha, DrawTextCallback, FCanvas);
end;

procedure TBitmapFont.DrawTextAligned(const Position: TPoint2; const Text: UniString; const Color: TIntColor2;
  const HorizAlign, VertAlign: TTextAlignment; const Alpha: VectorFloat; const AlignToPixels: Boolean);
var
  DrawAt, TextSize: TPoint2;
begin
  if (HorizAlign = TTextAlignment.Start) and (VertAlign = TTextAlignment.Start) then
  begin
    if AlignToPixels then
      DrawAt := Point2(Round(Position.X), Round(Position.Y))
    else
      DrawAt := Position;

    DrawText(DrawAt, Text, Color, Alpha);
    Exit;
  end;

  DrawAt := Position;
  TextSize := TextExtent(Text);

  case HorizAlign of
    TTextAlignment.Middle:
      DrawAt.X := Position.X - TextSize.X * 0.5;

    TTextAlignment.Final:
      DrawAt.X := Position.X - TextSize.X;

    else
      DrawAt.X := Position.X;
  end;

  case VertAlign of
    TTextAlignment.Middle:
      DrawAt.Y := Position.Y - TextSize.Y * 0.5;

    TTextAlignment.Final:
      DrawAt.Y := Position.Y - TextSize.Y;

    else
      DrawAt.Y := Position.Y;
  end;

  if AlignToPixels then
    DrawAt := Point2(Round(DrawAt.X), Round(DrawAt.Y))
  else
    DrawAt := Position;

  DrawText(DrawAt, Text, Color, Alpha);
end;

procedure TBitmapFont.DrawTextCentered(const Position: TPoint2; const Text: UniString; const Color: TIntColor2;
  const Alpha: VectorFloat; const AlignToPixels: Boolean);
begin
  DrawTextAligned(Position, Text, Color, TTextAlignment.Middle, TTextAlignment.Middle, Alpha, AlignToPixels);
end;

function TBitmapFont.IsWordSeparator(const Value: WideChar): Boolean;
begin
  Result := (Value = #32) or (Pos(Value, FWordSeparators) <> 0);
end;

function TBitmapFont.ExtractParagraphWord(const Text: UniString; var CharIndex: Integer; var ParagraphIndex: Integer;
  out WordText: UniString): Boolean;
var
  WordStart, WordLength: Integer;
begin
  WordText := '';

  // Skip all unused characters.
  while (CharIndex <= Length(Text)) and IsWordSeparator(Text[CharIndex]) do
    Inc(CharIndex);

  // Check for end of text.
  if CharIndex > Length(Text) then
    Exit(False);

  // Check for next paragraph.
  if Pos(Text[CharIndex], FParagraphSeparators) <> 0 then
  begin
    Inc(ParagraphIndex);
    Inc(CharIndex);
    Exit(True);
  end;

  // Start parsing the word.
  WordStart := CharIndex;
  WordLength := 0;

  while (CharIndex <= Length(Text)) and (Pos(Text[CharIndex], FWordSeparators) = 0) do
  begin
    Inc(CharIndex);
    Inc(WordLength);
  end;

  // -> Extract text segment.
  WordText := Copy(Text, WordStart, WordLength);
  Result := WordLength > 0;
end;

procedure TBitmapFont.SplitTextIntoWords(const Text: UniString);

  function AddWord(const WordText: UniString; const ParagraphIndex: Integer): Integer;
  begin
    Result := Length(FParagraphWords);
    SetLength(FParagraphWords, Result + 1);

    FParagraphWords[Result].WordText := WordText;
    FParagraphWords[Result].ParagraphIndex := ParagraphIndex;
  end;

var
  ParagraphIndex, CharIndex: Integer;
  WordText: UniString;
begin
  SetLength(FParagraphWords, 0);

  CharIndex := 1;
  ParagraphIndex := 0;

  while ExtractParagraphWord(Text, CharIndex, ParagraphIndex, WordText) do
    if Length(WordText) > 0 then
      AddWord(WordText, ParagraphIndex);
end;

procedure TBitmapFont.DrawTextBox(const TopLeft, BoxSize, ParagraphShift: TPoint2; const Text: UniString;
  const Color: TIntColor2; const Alpha: VectorFloat);
var
  ParagraphIndex, NextParagraphIndex: Integer;
  WordIndex, LastWordIndexInLine, WordCountInLine, SubIndex: Integer;
  PredWordsInLineWidth, TotalWordsInLineWidth, TotalWhiteSpace, RemainingLineWidth, BlankSpacePerWord: VectorFloat;
  Position, CurTextSize: TPoint2;
  LineHeight, DrawOffset: VectorFloat;
begin
  if FCanvas = nil then
    Exit;

  SplitTextIntoWords(Text);

  ParagraphIndex := -1;
  WordIndex := 0;

  ClearStyleStates;
  PushStyleState(Color);

  Position.X := TopLeft.X;

  while WordIndex < Length(FParagraphWords) do
  begin
    PredWordsInLineWidth := 0;
    TotalWordsInLineWidth := 0;
    TotalWhiteSpace := 0;
    RemainingLineWidth := BoxSize.X - (Position.X - TopLeft.X);

    LastWordIndexInLine := WordIndex;
    NextParagraphIndex := ParagraphIndex;

    while (TotalWordsInLineWidth + TotalWhiteSpace < RemainingLineWidth) and
      (LastWordIndexInLine < Length(FParagraphWords)) and (NextParagraphIndex = ParagraphIndex) do
    begin
      PredWordsInLineWidth := TotalWordsInLineWidth;
      TotalWordsInLineWidth := TotalWordsInLineWidth + TextWidth(FParagraphWords[LastWordIndexInLine].WordText);
      TotalWhiteSpace := TotalWhiteSpace + FSpaceWidth * FScale;
      NextParagraphIndex := FParagraphWords[LastWordIndexInLine].ParagraphIndex;

      Inc(LastWordIndexInLine);
    end;

    WordCountInLine := (LastWordIndexInLine - WordIndex) - 1;
    if (LastWordIndexInLine >= Length(FParagraphWords)) and
      (TotalWordsInLineWidth + TotalWhiteSpace < RemainingLineWidth) then
    begin
      Inc(WordCountInLine);
      PredWordsInLineWidth := TotalWordsInLineWidth;
    end;

    if WordCountInLine < 1 then
    begin
      // Case 1. New paragraph.
      if NextParagraphIndex <> ParagraphIndex then
      begin
        Position.X := TopLeft.X + ParagraphShift.X;

        if WordIndex < 1 then
          Position.Y := TopLeft.Y
        else
          Position.Y := Position.Y + ParagraphShift.Y;

        ParagraphIndex := NextParagraphIndex;

        Continue;
      end
      else
        // Case 2. Exhausted words or size doesn't fit.
        Break;
    end;

    if WordCountInLine > 1 then
      BlankSpacePerWord := (RemainingLineWidth - PredWordsInLineWidth) / (WordCountInLine - 1)
    else
      BlankSpacePerWord := 0;

    if ((NextParagraphIndex <> ParagraphIndex) and (WordCountInLine > 1)) or
      (WordIndex + WordCountInLine >= Length(FParagraphWords)) then
      BlankSpacePerWord := FSpaceWidth * FScale;

    LineHeight := 0;
    DrawOffset := 0;

    for SubIndex := WordIndex to WordIndex + WordCountInLine - 1 do
    begin
      DrawTextCustom(Point2(Position.X + Round(DrawOffset), Position.Y), FParagraphWords[SubIndex].WordText,
        IntColorWhite2, Alpha, DrawTextCallback, FCanvas, False);

      CurTextSize := TextExtent(FParagraphWords[SubIndex].WordText);

      DrawOffset := DrawOffset + CurTextSize.X + BlankSpacePerWord;
      LineHeight := Max(LineHeight, CurTextSize.Y);
    end;

    Position.X := TopLeft.X;
    Position.Y := Position.Y + LineHeight + FVerticalSpace;

    if Position.Y >= TopLeft.Y + BoxSize.Y then
      Break;

    Inc(WordIndex, WordCountInLine);
  end;

  ClearStyleStates;
end;

{$ENDREGION}
{$REGION 'TBitmapFonts'}

constructor TBitmapFonts.Create(const ADevice: TCustomDevice);
begin
  inherited Create;

  FDevice := ADevice;

  try
    FStyleTags := TBitmapFont.TStyleTags.Create;

    if FDevice <> nil then
    begin
      if FDevice.OnRestore <> nil then
        FDeviceRestoreHandle := FDevice.OnRestore.Subscribe(OnDeviceRestore);

      if FDevice.OnRelease <> nil then
        FDeviceReleaseHandle := FDevice.OnRelease.Subscribe(OnDeviceRelease);
    end;
  finally
    Increment_PXL_ClassInstances;
  end;
end;

destructor TBitmapFonts.Destroy;
begin
  try
    if FDevice <> nil then
    begin
      if (FDeviceReleaseHandle <> 0) and (FDevice.OnRelease <> nil) then
        FDevice.OnRelease.Unsubscribe(FDeviceReleaseHandle);

      if (FDeviceRestoreHandle <> 0) and (FDevice.OnRestore <> nil) then
        FDevice.OnRestore.Unsubscribe(FDeviceRestoreHandle);
    end
    else
    begin
      FDeviceReleaseHandle := 0;
      FDeviceRestoreHandle := 0;
    end;

    Clear;
    FStyleTags.Free;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TBitmapFonts.GetCount: Integer;
begin
  Result := Length(FFonts);
end;

function TBitmapFonts.GetItem(const Index: Integer): TBitmapFont;
begin
  if (Index >= 0) and (Index < Length(FFonts)) then
    Result := FFonts[Index]
  else
    Result := nil;
end;

procedure TBitmapFonts.SetCanvas(const Value: TCustomCanvas);
var
  I: Integer;
begin
  if FCanvas <> Value then
  begin
    FCanvas := Value;

    for I := 0 to Length(FFonts) - 1 do
      FFonts[I].Canvas := FCanvas;
  end;
end;

procedure TBitmapFonts.OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
var
  I: Integer;
  FailedToRestore: Boolean;
begin
  FailedToRestore := False;

  for I := 0 to Length(FFonts) - 1 do
    if (FFonts[I] <> nil) and (FFonts[I].Image <> nil) and (not FFonts[I].SubscribedTextures) and
      (not FFonts[I].Image.DeviceRestore) then
      FailedToRestore := True;

  if FailedToRestore then
    LogText(SCannotRestoreImages, TLogType.Error);
end;

procedure TBitmapFonts.OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);
var
  I: Integer;
begin
  for I := 0 to Length(FFonts) - 1 do
    if (FFonts[I] <> nil) and (FFonts[I].Image <> nil) and (not FFonts[I].SubscribedTextures) then
      FFonts[I].Image.DeviceRelease;
end;

function TBitmapFonts.InsertFont: Integer;
begin
  Result := Length(FFonts);
  SetLength(FFonts, Result + 1);

  FFonts[Result] := TBitmapFont.Create(FDevice, False);
  FFonts[Result].StyleTags := FStyleTags;
  FFonts[Result].Canvas := FCanvas;
end;

procedure TBitmapFonts.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FFonts)) then
    Exit;

  FFonts[Index].Free;

  for I := Index to Length(FFonts) - 2 do
    FFonts[I] := FFonts[I + 1];

  SetLength(FFonts, Length(FFonts) - 1);
  FSearchListDirty := True;
end;

procedure TBitmapFonts.Clear;
var
  I: Integer;
begin
  for I := Length(FFonts) - 1 downto 0 do
    FFonts[I].Free;

  SetLength(FFonts, 0);
  FSearchListDirty := True;
end;

function TBitmapFonts.AddFromBinaryStream(const Stream: TStream; const FontName: StdString;
  const PixelFormat: TPixelFormat): Integer;
begin
  Result := InsertFont;
  if not FFonts[Result].LoadFromBinaryStream(Stream, PixelFormat) then
  begin
    Remove(Result);
    Exit(-1);
  end;

  FFonts[Result].Name := FontName;
end;

function TBitmapFonts.AddFromBinaryFile(const FileName: StdString; const PixelFormat: TPixelFormat): Integer;
begin
  Result := InsertFont;
  if not FFonts[Result].LoadFromBinaryFile(FileName, PixelFormat) then
  begin
    Remove(Result);
    Exit(-1);
  end;
end;

function TBitmapFonts.AddFromBinaryAsset(const AssetName: StdString; const PixelFormat: TPixelFormat): Integer;
begin
  Result := InsertFont;
  if not FFonts[Result].LoadFromBinaryAsset(AssetName, PixelFormat) then
  begin
    Remove(Result);
    Exit(-1);
  end;
end;

function TBitmapFonts.AddFromXMLStream(const ImageExtension: StdString; const ImageStream, XMLStream: TStream;
  const FontName: StdString; const PixelFormat: TPixelFormat): Integer;
begin
  Result := InsertFont;
  if not FFonts[Result].LoadFromXMLStream(ImageExtension, ImageStream, XMLStream, PixelFormat) then
  begin
    Remove(Result);
    Exit(-1);
  end;

  FFonts[Result].Name := FontName;
end;

function TBitmapFonts.AddFromXMLFile(const ImageFileName: StdString; const XMLFileName: StdString;
  const PixelFormat: TPixelFormat): Integer;
begin
  Result := InsertFont;
  if not FFonts[Result].LoadFromXMLFile(ImageFileName, XMLFileName, PixelFormat) then
  begin
    Remove(Result);
    Exit(-1);
  end;
end;

function TBitmapFonts.AddFromXMLAsset(const ImageAssetName: StdString;
  const XMLAssetName: StdString; const PixelFormat: TPixelFormat): Integer;
begin
  Result := InsertFont;
  if not FFonts[Result].LoadFromXMLAsset(ImageAssetName, XMLAssetName, PixelFormat) then
  begin
    Remove(Result);
    Exit(-1);
  end;
end;

{$IFDEF IncludeSystemFont}
function TBitmapFonts.AddSystemFont(const FontImage: TSystemFontImage; const FontName: StdString;
  const PixelFormat: TPixelFormat): Integer;
begin
  Result := InsertFont;

  if not FFonts[Result].LoadSystemFont(FontImage, PixelFormat) then
  begin
    Remove(Result);
    Exit(-1);
  end;

  FFonts[Result].Name := FontName;
end;
{$ENDIF}

procedure TBitmapFonts.InitSearchList;
var
  I: Integer;
begin
  if Length(FSearchList) <> Length(FFonts) then
    SetLength(FSearchList, Length(FFonts));

  for I := 0 to Length(FFonts) - 1 do
    FSearchList[I] := I;
end;

procedure TBitmapFonts.SwapSearchList(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FSearchList[Index1];
  FSearchList[Index1] := FSearchList[Index2];
  FSearchList[Index2] := TempValue;
end;

function TBitmapFonts.CompareSearchList(const Index1, Index2: Integer): Integer;
begin
  Result := CompareText(FFonts[FSearchList[Index1]].Name, FFonts[FSearchList[Index2]].Name);
end;

function TBitmapFonts.SplitSearchList(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FSearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (CompareSearchList(FSearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (CompareSearchList(FSearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      SwapSearchList(Left, Right);
  end;

  SwapSearchList(Start, Right);
  Result := Right;
end;

procedure TBitmapFonts.SortSearchList(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := SplitSearchList(Start, Stop);

    SortSearchList(Start, SplitPt - 1);
    SortSearchList(SplitPt + 1, Stop);
  end;
end;

procedure TBitmapFonts.UpdateSearchList;
begin
  InitSearchList;

  if Length(FSearchList) > 1 then
    SortSearchList(0, Length(FSearchList) - 1);

  FSearchListDirty := False;
end;

function TBitmapFonts.IndexOf(const FontName: StdString): Integer;
var
  Left, Right, Pivot, Res: Integer;
begin
  if FSearchListDirty then
    UpdateSearchList;

  Left := 0;
  Right := Length(FSearchList) - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;
    Res := CompareText(FFonts[FSearchList[Pivot]].Name, FontName);

    if Res = 0 then
      Exit(FSearchList[Pivot]);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

function TBitmapFonts.IndexOf(const Element: TBitmapFont): Integer;
var
  I: Integer;
begin
  if Element = nil then
    Exit(-1);

  for I := 0 to Length(FFonts) - 1 do
    if FFonts[I] = Element then
      Exit(I);

  Result := -1;
end;

function TBitmapFonts.Insert(const Font: TBitmapFont): Integer;
begin
  Result := Length(FFonts);
  SetLength(FFonts, Result + 1);

  FFonts[Result] := Font;
  FFonts[Result].StyleTags := FStyleTags;
  FFonts[Result].Canvas := FCanvas;
end;

function TBitmapFonts.Include(const Element: TBitmapFont): Integer;
begin
  Result := IndexOf(Element);
  if Result = -1 then
    Result := Insert(Element);
end;

function TBitmapFonts.GetFont(const Name: StdString): TBitmapFont;
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if Index <> -1 then
    Result := FFonts[Index]
  else
    Result := nil;
end;

procedure TBitmapFonts.MarkSearchDirty;
begin
  FSearchListDirty := True;
end;

{$ENDREGION}

end.
