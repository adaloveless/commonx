unit DirectWrite_JSB;

///////////////////////////////////////////////////////////////////////////////
// Title: Translation of DirectX C++ header files for use with Delphi 2009 and later
//
// File name: DirectWrite_JSB.pas
//
// Originator: J S Bladen, Sheffield, UK.
//
// Copyright: J S Bladen, Sheffield, UK.
//
// Translation date and time (UTC): 07/10/2010 15:08:59
//
// Email: DirectXForDelphi@jsbmedical.co.uk
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Original file(s):
//   DCommon.h
//   DWrite.h
//
// Copyright (C) Microsoft Corporation.
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Software licence:
//
// Use of this "software" is subject to the following software licence:
//
// ***** BEGIN LICENCE BLOCK *****
//
// 1) This software is distributed in the hope that it will be useful, but without warranty of any kind.
// 2) The copyright and/or originator notice(s) may not be altered or removed.
// 3) This software may be used for commercial or non-commercial use.
// 4) This software may be redistributed, provided no charge is made.
// 5) There is no obligation to make source code available to end users even if the software is modified.
// 6) Modified versions of this software will be subject to this software licence.
// 7) If the software is modified, the changes must be marked in the source code with the contributors ID (e.g. name)
//    before redistribution.
//
// ***** END LICENCE BLOCK *****
//
// In addition, users of this software are strongly encouraged to contact the originator with feedback, corrections and
// suggestions for improvement.
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Translation notes:
//
// 1) This software is preliminary. For the latest version please see "http://DirectXForDelphi.blogspot.com/".
//
// 2) The header filename suffix "_JSB" is to distinguish the files from the equivalent JEDI/Clootie files
//    and must be left in place". Interface units from different sources may not work correctly together.
//
// 3) By default, optional interface output method and function parameters are translated as "out InterfaceName:IInterfaceName",
//    not "pInterfaceName:PIInterfaceName". This is because with the pointer version, Delphi does not appear to call the
//    COM Release method on the supplied interface before assigning a new value. To pass a nil parameter, use
//    "IInterfaceName(nil^)".
//
//    PLEASE NOTE: This is different to the equivalent JEDI/Clootie files, though only minimal source code changes
//    should be required.
//
//    If you want to use pointers instead, define the conditional define "UsePointersForOptionalOutputInterfaces" but ensure
//    that the interface variable is set to nil before calling the method.
//
// 4) Please contact me if you are interested in versions for FPC or C++ etc.
//
// JSB
///////////////////////////////////////////////////////////////////////////////

interface

{$Z4}

uses
  Windows, SysUtils, MultiMon, {$IFDEF UseJSBErrors} UDirectXErrors, {$ENDIF} DXTypes_JSB, DXGI_JSB, D3D10_1_JSB;

const
  DLL_DWrite='DWrite.dll';

///////////////////////////////////////////////////////////////////////////////
// Begin "DCommon.h"
///////////////////////////////////////////////////////////////////////////////

type
  TDWrite_MeasuringMode=
  (
    DWRITE_MEASURING_MODE_NATURAL,
    DWRITE_MEASURING_MODE_GDI_CLASSIC,
    DWRITE_MEASURING_MODE_GDI_NATURAL
  );
  PTDWrite_MeasuringMode=^TDWrite_MeasuringMode;
  DWRITE_MEASURING_MODE=TDWrite_MeasuringMode;
  PDWRITE_MEASURING_MODE=^TDWrite_MeasuringMode;

///////////////////////////////////////////////////////////////////////////////
// End "DCommon.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "DWrite.h"
///////////////////////////////////////////////////////////////////////////////

function DWRITE_MAKE_OPENTYPE_TAG(a,b,c,d:LongWord):LongWord;

const
  DWRITE_ALPHA_MAX=255;
  FACILITY_DWRITE=$898;
  DWRITE_HR_ERR_Base=(1 shl 31) or (FACILITY_DWRITE shl 16)+$5000;
  //
  DWRITE_ERROR_FILEFORMAT=HResult(DWRITE_HR_ERR_Base or $000);
  DWRITE_ERROR_UNEXPECTED=HResult(DWRITE_HR_ERR_Base or $001);
  DWRITE_ERROR_NOFONT=HResult(DWRITE_HR_ERR_Base or $002);
  DWRITE_ERROR_FILENOTFOUND=HResult(DWRITE_HR_ERR_Base or $003);
  DWRITE_ERROR_FILEACCESS=HResult(DWRITE_HR_ERR_Base or $004);
  DWRITE_ERROR_FONTCOLLECTIONOBSOLETE=HResult(DWRITE_HR_ERR_Base or $005);
  DWRITE_ERROR_ALREADYREGISTERED=HResult(DWRITE_HR_ERR_Base or $006);

type
  TDWrite_FontFileType=
  (
    DWRITE_FONT_FILE_TYPE_UNKNOWN,
    DWRITE_FONT_FILE_TYPE_CFF,
    DWRITE_FONT_FILE_TYPE_TRUETYPE,
    DWRITE_FONT_FILE_TYPE_TRUETYPE_COLLECTION,
    DWRITE_FONT_FILE_TYPE_TYPE1_PFM,
    DWRITE_FONT_FILE_TYPE_TYPE1_PFB,
    DWRITE_FONT_FILE_TYPE_VECTOR,
    DWRITE_FONT_FILE_TYPE_BITMAP
  );
  PTDWrite_FontFileType=^TDWrite_FontFileType;
  DWRITE_FONT_FILE_TYPE=TDWrite_FontFileType;
  PDWRITE_FONT_FILE_TYPE=^TDWrite_FontFileType;

  TDWrite_FontFaceType=
  (
    DWRITE_FONT_FACE_TYPE_CFF,
    DWRITE_FONT_FACE_TYPE_TRUETYPE,
    DWRITE_FONT_FACE_TYPE_TRUETYPE_COLLECTION,
    DWRITE_FONT_FACE_TYPE_TYPE1,
    DWRITE_FONT_FACE_TYPE_VECTOR,
    DWRITE_FONT_FACE_TYPE_BITMAP,
    DWRITE_FONT_FACE_TYPE_UNKNOWN
  );
  PTDWrite_FontFaceType=^TDWrite_FontFaceType;
  DWRITE_FONT_FACE_TYPE=TDWrite_FontFaceType;
  PDWRITE_FONT_FACE_TYPE=^TDWrite_FontFaceType;

  TDWrite_FontSimulations=
  (
    DWRITE_FONT_SIMULATIONS_NONE=$0000,
    DWRITE_FONT_SIMULATIONS_BOLD=$0001,
    DWRITE_FONT_SIMULATIONS_OBLIQUE=$0002
  );
  PTDWrite_FontSimulations=^TDWrite_FontSimulations;
  DWRITE_FONT_SIMULATIONS=TDWrite_FontSimulations;
  PDWRITE_FONT_SIMULATIONS=^TDWrite_FontSimulations;

  TDWrite_FontWeight=
  (
    DWRITE_FONT_WEIGHT_THIN=100,
    DWRITE_FONT_WEIGHT_EXTRA_LIGHT=200,
    DWRITE_FONT_WEIGHT_ULTRA_LIGHT=200,
    DWRITE_FONT_WEIGHT_LIGHT=300,
    DWRITE_FONT_WEIGHT_NORMAL=400,
    DWRITE_FONT_WEIGHT_REGULAR=400,
    DWRITE_FONT_WEIGHT_MEDIUM=500,
    DWRITE_FONT_WEIGHT_DEMI_BOLD=600,
    DWRITE_FONT_WEIGHT_SEMI_BOLD=600,
    DWRITE_FONT_WEIGHT_BOLD=700,
    DWRITE_FONT_WEIGHT_EXTRA_BOLD=800,
    DWRITE_FONT_WEIGHT_ULTRA_BOLD=800,
    DWRITE_FONT_WEIGHT_BLACK=900,
    DWRITE_FONT_WEIGHT_HEAVY=900,
    DWRITE_FONT_WEIGHT_EXTRA_BLACK=950,
    DWRITE_FONT_WEIGHT_ULTRA_BLACK=950
  );
  PTDWrite_FontWeight=^TDWrite_FontWeight;
  DWRITE_FONT_WEIGHT=TDWrite_FontWeight;
  PDWRITE_FONT_WEIGHT=^TDWrite_FontWeight;

  TDWrite_FontStretch=
  (
    DWRITE_FONT_STRETCH_UNDEFINED=0,
    DWRITE_FONT_STRETCH_ULTRA_CONDENSED=1,
    DWRITE_FONT_STRETCH_EXTRA_CONDENSED=2,
    DWRITE_FONT_STRETCH_CONDENSED=3,
    DWRITE_FONT_STRETCH_SEMI_CONDENSED=4,
    DWRITE_FONT_STRETCH_NORMAL=5,
    DWRITE_FONT_STRETCH_MEDIUM=5,
    DWRITE_FONT_STRETCH_SEMI_EXPANDED=6,
    DWRITE_FONT_STRETCH_EXPANDED=7,
    DWRITE_FONT_STRETCH_EXTRA_EXPANDED=8,
    DWRITE_FONT_STRETCH_ULTRA_EXPANDED=9
  );
  PTDWrite_FontStretch=^TDWrite_FontStretch;
  DWRITE_FONT_STRETCH=TDWrite_FontStretch;
  PDWRITE_FONT_STRETCH=^TDWrite_FontStretch;

  TDWrite_FontStyle=
  (
    DWRITE_FONT_STYLE_NORMAL,
    DWRITE_FONT_STYLE_OBLIQUE,
    DWRITE_FONT_STYLE_ITALIC
  );
  PTDWrite_FontStyle=^TDWrite_FontStyle;
  DWRITE_FONT_STYLE=TDWrite_FontStyle;
  PDWRITE_FONT_STYLE=^TDWrite_FontStyle;

  TDWrite_InformationalStringId=
  (
    DWRITE_INFORMATIONAL_STRING_NONE,
    DWRITE_INFORMATIONAL_STRING_COPYRIGHT_NOTICE,
    DWRITE_INFORMATIONAL_STRING_VERSION_STRINGS,
    DWRITE_INFORMATIONAL_STRING_TRADEMARK,
    DWRITE_INFORMATIONAL_STRING_MANUFACTURER,
    DWRITE_INFORMATIONAL_STRING_DESIGNER,
    DWRITE_INFORMATIONAL_STRING_DESIGNER_URL,
    DWRITE_INFORMATIONAL_STRING_DESCRIPTION,
    DWRITE_INFORMATIONAL_STRING_FONT_VENDOR_URL,
    DWRITE_INFORMATIONAL_STRING_LICENSE_DESCRIPTION,
    DWRITE_INFORMATIONAL_STRING_LICENSE_INFO_URL,
    DWRITE_INFORMATIONAL_STRING_WIN32_FAMILY_NAMES,
    DWRITE_INFORMATIONAL_STRING_WIN32_SUBFAMILY_NAMES,
    DWRITE_INFORMATIONAL_STRING_PREFERRED_FAMILY_NAMES,
    DWRITE_INFORMATIONAL_STRING_PREFERRED_SUBFAMILY_NAMES,
    DWRITE_INFORMATIONAL_STRING_SAMPLE_TEXT
  );
  PTDWrite_InformationalStringId=^TDWrite_InformationalStringId;
  DWRITE_INFORMATIONAL_STRING_ID=TDWrite_InformationalStringId;
  PDWRITE_INFORMATIONAL_STRING_ID=^TDWrite_InformationalStringId;

  TDWrite_FontMetrics=record
    DesignUnitsPerEm:Word;
    Ascent:Word;
    Descent:Word;
    LineGap:SmallInt;
    CapHeight:Word;
    XHeight:Word;
    UnderlinePosition:SmallInt;
    UnderlineThickness:Word;
    StrikethroughPosition:SmallInt;
    StrikethroughThickness:Word;
  end;
  PTDWrite_FontMetrics=^TDWrite_FontMetrics;
  DWRITE_FONT_METRICS=TDWrite_FontMetrics;
  PDWRITE_FONT_METRICS=^TDWrite_FontMetrics;

  TDWrite_GlyphMetrics=record
    LeftSideBearing:Integer;
    AdvanceWidth:LongWord;
    RightSideBearing:Integer;
    TopSideBearing:Integer;
    AdvanceHeight:LongWord;
    BottomSideBearing:Integer;
    VerticalOriginY:Integer;
  end;
  PTDWrite_GlyphMetrics=^TDWrite_GlyphMetrics;
  DWRITE_GLYPH_METRICS=TDWrite_GlyphMetrics;
  PDWRITE_GLYPH_METRICS=^TDWrite_GlyphMetrics;

  TDWrite_GlyphOffset=record
    AdvanceOffset:Single;
    AscenderOffset:Single;
  end;
  PTDWrite_GlyphOffset=^TDWrite_GlyphOffset;
  DWRITE_GLYPH_OFFSET=TDWrite_GlyphOffset;
  PDWRITE_GLYPH_OFFSET=^TDWrite_GlyphOffset;

  TDWrite_FactoryType=
  (
    DWRITE_FACTORY_TYPE_SHARED,
    DWRITE_FACTORY_TYPE_ISOLATED
  );
  PTDWrite_FactoryType=^TDWrite_FactoryType;
  DWRITE_FACTORY_TYPE=TDWrite_FactoryType;
  PDWRITE_FACTORY_TYPE=^TDWrite_FactoryType;

  IDWriteFontFileStream=interface;
  PIDWriteFontFileStream=^IDWriteFontFileStream;

  IDWriteFontFileLoader=interface(IUnknown)
    ['{727CAD4E-D6AF-4C9E-8A08-D695B11CAA49}']
    function CreateStreamFromKey
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      out FontFileStream:IDWriteFontFileStream (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteLocalFontFileLoader=interface(IDWriteFontFileLoader)
    ['{B2D9F3EC-C9FE-4A11-A2EC-D86208F7C0A2}']
    function GetFilePathLengthFromKey
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      out FilePathLength:LongWord (* __out *)
    ):HResult; stdcall;

    function GetFilePathFromKey
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      FilePath:PWideChar; (* __out_ecount_z(filePathSize) *)
      FilePathSize:LongWord
    ):HResult; stdcall;

    function GetLastWriteTimeFromKey
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      out LastWriteTime:FILETIME (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontFileStream=interface(IUnknown)
    ['{6D4865FE-0AB8-4D91-8F62-5DD6BE34A3E0}']
    function ReadFileFragment
    (
      FragmentStart:PPointer; (* __deref_out_bcount(fragmentSize) *)
      FileOffset:UInt64;
      FragmentSize:UInt64;
      out FragmentContext:Pointer (* __out *)
    ):HResult; stdcall;

    procedure ReleaseFileFragment
    (
      FragmentContext:Pointer
    ); stdcall;

    function GetFileSize
    (
      out FileSize:UInt64 (* __out *)
    ):HResult; stdcall;

    function GetLastWriteTime
    (
      out LastWriteTime:UInt64 (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontFile=interface(IUnknown)
    ['{739D886A-CEF5-47DC-8769-1A8B41BEBBB0}']
    function GetReferenceKey
    (
      FontFileReferenceKey:PPointer; (* __deref_out_bcount(*fontFileReferenceKeySize) *)
      out FontFileReferenceKeySize:LongWord (* __out *)
    ):HResult; stdcall;

    function GetLoader
    (
      out FontFileLoader:IDWriteFontFileLoader (* __out *)
    ):HResult; stdcall;

    function Analyze
    (
      out IsSupportedFontType:LongBool; (* __out *)
      out FontFileType:TDWrite_FontFileType; (* __out *)
      FontFaceType:PTDWrite_FontFaceType; (* __out_opt *)
      out NumberOfFaces:LongWord (* __out *)
    ):HResult; stdcall;
  end;
  PIDWriteFontFile=^IDWriteFontFile;

  TDWrite_PixelGeometry=
  (
    DWRITE_PIXEL_GEOMETRY_FLAT,
    DWRITE_PIXEL_GEOMETRY_RGB,
    DWRITE_PIXEL_GEOMETRY_BGR
  );
  PTDWrite_PixelGeometry=^TDWrite_PixelGeometry;
  DWRITE_PIXEL_GEOMETRY=TDWrite_PixelGeometry;
  PDWRITE_PIXEL_GEOMETRY=^TDWrite_PixelGeometry;

  TDWrite_RenderingMode=
  (
    DWRITE_RENDERING_MODE_DEFAULT,
    DWRITE_RENDERING_MODE_ALIASED,
    DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC,
    DWRITE_RENDERING_MODE_CLEARTYPE_GDI_NATURAL,
    DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL,
    DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL_SYMMETRIC,
    DWRITE_RENDERING_MODE_OUTLINE
  );
  PTDWrite_RenderingMode=^TDWrite_RenderingMode;
  DWRITE_RENDERING_MODE=TDWrite_RenderingMode;
  PDWRITE_RENDERING_MODE=^TDWrite_RenderingMode;

  TDWrite_Matrix=record
    M11:Single;
    M12:Single;
    M21:Single;
    M22:Single;
    Dx:Single;
    Dy:Single;
  end;
  PTDWrite_Matrix=^TDWrite_Matrix;
  DWRITE_MATRIX=TDWrite_Matrix;
  PDWRITE_MATRIX=^TDWrite_Matrix;

  IDWriteRenderingParams=interface(IUnknown)
    ['{2F0DA53A-2ADD-47CD-82EE-D9EC34688E75}']
    function GetGamma:Single; stdcall;
    function GetEnhancedContrast:Single; stdcall;
    function GetClearTypeLevel:Single; stdcall;
    function GetPixelGeometry:TDWrite_PixelGeometry; stdcall;
    function GetRenderingMode:TDWrite_RenderingMode; stdcall;
  end;

  IDWriteGeometrySink=IUnknown; // JSB: Acutally ID2D1SimplifiedGeometrySink.

  IDWriteFontFace=interface(IUnknown)
    ['{5F49804D-7024-4D43-BFA9-D25984F53849}']
    function GetType:TDWrite_FontFaceType; stdcall;

    function GetFiles
    (
      var NumberOfFiles:LongWord; (* __inout *)
      FontFiles:PIDWriteFontFile (* __out_ecount_opt(*numberOfFiles) *)
    ):HResult; stdcall;

    function GetIndex:LongWord; stdcall;

    function GetSimulations:TDWrite_FontSimulations; stdcall;

    function IsSymbolFont:LongBool; stdcall;

    procedure GetMetrics
    (
      out FontFaceMetrics:TDWrite_FontMetrics (* __out *)
    ); stdcall;

    function GetGlyphCount:Word; stdcall;

    function GetDesignGlyphMetrics
    (
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphCount:LongWord;
      GlyphMetrics:PTDWrite_GlyphMetrics; (* __out_ecount(glyphCount) *)
      IsSideways:LongBool=FALSE
    ):HResult; stdcall;

    function GetGlyphIndices
    (
      CodePoints:PLongWord; (* __in_ecount(codePointCount) *)
      CodePointCount:LongWord;
      GlyphIndices:PWord (* __out_ecount(codePointCount) *)
    ):HResult; stdcall;

    function TryGetFontTable
    (
      OpenTypeTableTag:LongWord; (* __in *)
      TableData:PPointer; (* __deref_out_bcount(*tableSize) *)
      out TableSize:LongWord; (* __out *)
      out TableContext:Pointer; (* __out *)
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    procedure ReleaseFontTable
    (
      TableContext:Pointer (* __in *)
    ); stdcall;

    function GetGlyphRunOutline
    (
      EmSize:Single;
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphAdvances:PSingle; (* __in_ecount_opt(glyphCount) *)
      GlyphOffsets:PTDWrite_GlyphOffset; (* __in_ecount_opt(glyphCount) *)
      GlyphCount:LongWord;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      GeometrySink:IDWriteGeometrySink
    ):HResult; stdcall;

    function GetRecommendedRenderingMode
    (
      EmSize:Single;
      PixelsPerDip:Single;
      MeasuringMode:TDWrite_MeasuringMode;
      RenderingParams:IDWriteRenderingParams;
      out RenderingMode:TDWrite_RenderingMode (* __out *)
    ):HResult; stdcall;

    function GetGdiCompatibleMetrics
    (
      EmSize:Single;
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      out FontFaceMetrics:TDWrite_FontMetrics (* __out *)
    ):HResult; stdcall;

    function GetGdiCompatibleGlyphMetrics
    (
      EmSize:Single;
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      UseGdiNatural:LongBool;
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphCount:LongWord;
      GlyphMetrics:PTDWrite_GlyphMetrics; (* __out_ecount(glyphCount) *)
      IsSideways:LongBool=FALSE
    ):HResult; stdcall;
  end;

  IDWriteFactory=interface;
  PIDWriteFactory=^IDWriteFactory;

  IDWriteFontFileEnumerator=interface;
  PIDWriteFontFileEnumerator=^IDWriteFontFileEnumerator;

  IDWriteFontCollectionLoader=interface(IUnknown)
    ['{CCA920E4-52F0-492B-BFA8-29C72EE0A468}']
    function CreateEnumeratorFromKey
    (
      Factory:IDWriteFactory;
      CollectionKey:Pointer; (* __in_bcount(collectionKeySize) *)
      CollectionKeySize:LongWord;
      out FontFileEnumerator:IDWriteFontFileEnumerator (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontFileEnumerator=interface(IUnknown)
    ['{72755049-5FF7-435D-8348-4BE97CFA6C7C}']
    function MoveNext
    (
      out HasCurrentFile:LongBool (* __out *)
    ):HResult; stdcall;

    function GetCurrentFontFile
    (
      out FontFile:IDWriteFontFile (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteLocalizedStrings=interface(IUnknown)
    ['{08256209-099A-4B34-B86D-C22B110E7771}']
    function GetCount:LongWord; stdcall;

    function FindLocaleName
    (
      LocaleName:PWideChar; (* __in_z *)
      out Index:LongWord; (* __out *)
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    function GetLocaleNameLength
    (
      Index:LongWord;
      out Length:LongWord (* __out *)
    ):HResult; stdcall;

    function GetLocaleName
    (
      Index:LongWord;
      LocaleName:PWideChar; (* __out_ecount_z(size) *)
      Size:LongWord
    ):HResult; stdcall;

    function GetStringLength
    (
      Index:LongWord;
      out Length:LongWord (* __out *)
    ):HResult; stdcall;

    function GetString
    (
      Index:LongWord;
      StringBuffer:PWideChar; (* __out_ecount_z(size) *)
      Size:LongWord
    ):HResult; stdcall;
  end;

  IDWriteFontFamily=interface;
  PIDWriteFontFamily=^IDWriteFontFamily;

  IDWriteFont=interface;
  PIDWriteFont=^IDWriteFont;

  IDWriteFontCollection=interface(IUnknown)
    ['{A84CEE02-3EEA-4EEE-A827-87C1A02A0FCC}']
    function GetFontFamilyCount:LongWord; stdcall;

    function GetFontFamily
    (
      Index:LongWord;
      out FontFamily:IDWriteFontFamily (* __out *)
    ):HResult; stdcall;

    function FindFamilyName
    (
      FamilyName:PWideChar; (* __in_z *)
      out Index:LongWord; (* __out *)
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    function GetFontFromFontFace
    (
      FontFace:IDWriteFontFace;
      out Font:IDWriteFont (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontList=interface(IUnknown)
    ['{1A0D8438-1D97-4EC1-AEF9-A2FB86ED6ACB}']
    function GetFontCollection
    (
      out FontCollection:IDWriteFontCollection (* __out *)
    ):HResult; stdcall;

    function GetFontCount:LongWord; stdcall;

    function GetFont
    (
      Index:LongWord;
      out Font:IDWriteFont (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontFamily=interface(IDWriteFontList)
    ['{DA20D8EF-812A-4C43-9802-62EC4ABD7ADD}']
    function GetFamilyNames
    (
      out Names:IDWriteLocalizedStrings (* __out *)
    ):HResult; stdcall;

    function GetFirstMatchingFont
    (
      Weight:TDWrite_FontWeight;
      Stretch:TDWrite_FontStretch;
      Style:TDWrite_FontStyle;
      out MatchingFont:IDWriteFont (* __out *)
    ):HResult; stdcall;

    function GetMatchingFonts
    (
      Weight:TDWrite_FontWeight;
      Stretch:TDWrite_FontStretch;
      Style:TDWrite_FontStyle;
      out MatchingFonts:IDWriteFontList (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFont=interface(IUnknown)
    ['{ACD16696-8C14-4F5D-877E-FE3FC1D32737}']
    function GetFontFamily
    (
      out FontFamily:IDWriteFontFamily (* __out *)
    ):HResult; stdcall;

    function GetWeight:TDWrite_FontWeight; stdcall;

    function GetStretch:TDWrite_FontStretch; stdcall;

    function GetStyle:TDWrite_FontStyle; stdcall;

    function IsSymbolFont:LongBool; stdcall;

    function GetFaceNames
    (
      out Names:IDWriteLocalizedStrings (* __out *)
    ):HResult; stdcall;

    function GetInformationalStrings
    (
      InformationalStringID:TDWrite_InformationalStringId;
      out InformationalStrings:IDWriteLocalizedStrings; (* __out *)
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    function GetSimulations:TDWrite_FontSimulations; stdcall;

    procedure GetMetrics
    (
      out FontMetrics:TDWrite_FontMetrics (* __out *)
    ); stdcall;

    function HasCharacter
    (
      UnicodeValue:LongWord;
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    function CreateFontFace
    (
      out FontFace:IDWriteFontFace (* __out *)
    ):HResult; stdcall;
  end;

  TDWrite_ReadingDirection=
  (
    DWRITE_READING_DIRECTION_LEFT_TO_RIGHT,
    DWRITE_READING_DIRECTION_RIGHT_TO_LEFT
  );
  PTDWrite_ReadingDirection=^TDWrite_ReadingDirection;
  DWRITE_READING_DIRECTION=TDWrite_ReadingDirection;
  PDWRITE_READING_DIRECTION=^TDWrite_ReadingDirection;

  TDWrite_FlowDirection=
  (
    DWRITE_FLOW_DIRECTION_TOP_TO_BOTTOM
  );
  PTDWrite_FlowDirection=^TDWrite_FlowDirection;
  DWRITE_FLOW_DIRECTION=TDWrite_FlowDirection;
  PDWRITE_FLOW_DIRECTION=^TDWrite_FlowDirection;

  TDWrite_TextAlignment=
  (
    DWRITE_TEXT_ALIGNMENT_LEADING,
    DWRITE_TEXT_ALIGNMENT_TRAILING,
    DWRITE_TEXT_ALIGNMENT_CENTER
  );
  PTDWrite_TextAlignment=^TDWrite_TextAlignment;
  DWRITE_TEXT_ALIGNMENT=TDWrite_TextAlignment;
  PDWRITE_TEXT_ALIGNMENT=^TDWrite_TextAlignment;

  TDWrite_ParagraphAlignment=
  (
    DWRITE_PARAGRAPH_ALIGNMENT_NEAR,
    DWRITE_PARAGRAPH_ALIGNMENT_FAR,
    DWRITE_PARAGRAPH_ALIGNMENT_CENTER
  );
  PTDWrite_ParagraphAlignment=^TDWrite_ParagraphAlignment;
  DWRITE_PARAGRAPH_ALIGNMENT=TDWrite_ParagraphAlignment;
  PDWRITE_PARAGRAPH_ALIGNMENT=^TDWrite_ParagraphAlignment;

  TDWrite_WordWrapping=
  (
    DWRITE_WORD_WRAPPING_WRAP,
    DWRITE_WORD_WRAPPING_NO_WRAP
  );
  PTDWrite_WordWrapping=^TDWrite_WordWrapping;
  DWRITE_WORD_WRAPPING=TDWrite_WordWrapping;
  PDWRITE_WORD_WRAPPING=^TDWrite_WordWrapping;

  TDWrite_LineSpacingMethod=
  (
    DWRITE_LINE_SPACING_METHOD_DEFAULT,
    DWRITE_LINE_SPACING_METHOD_UNIFORM
  );
  PTDWrite_LineSpacingMethod=^TDWrite_LineSpacingMethod;
  DWRITE_LINE_SPACING_METHOD=TDWrite_LineSpacingMethod;
  PDWRITE_LINE_SPACING_METHOD=^TDWrite_LineSpacingMethod;

  TDWrite_TrimmingGranularity=
  (
    DWRITE_TRIMMING_GRANULARITY_NONE,
    DWRITE_TRIMMING_GRANULARITY_CHARACTER,
    DWRITE_TRIMMING_GRANULARITY_WORD
  );
  PTDWrite_TrimmingGranularity=^TDWrite_TrimmingGranularity;
  DWRITE_TRIMMING_GRANULARITY=TDWrite_TrimmingGranularity;
  PDWRITE_TRIMMING_GRANULARITY=^TDWrite_TrimmingGranularity;

  TDWrite_FontFeatureTag=
  (
    DWRITE_FONT_FEATURE_TAG_ALTERNATIVE_FRACTIONS=$63726661,
    DWRITE_FONT_FEATURE_TAG_PETITE_CAPITALS_FROM_CAPITALS=$63703263,
    DWRITE_FONT_FEATURE_TAG_SMALL_CAPITALS_FROM_CAPITALS=$63733263,
    DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_ALTERNATES=$746c6163,
    DWRITE_FONT_FEATURE_TAG_CASE_SENSITIVE_FORMS=$65736163,
    DWRITE_FONT_FEATURE_TAG_GLYPH_COMPOSITION_DECOMPOSITION=$706d6363,
    DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_LIGATURES=$67696c63,
    DWRITE_FONT_FEATURE_TAG_CAPITAL_SPACING=$70737063,
    DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_SWASH=$68777363,
    DWRITE_FONT_FEATURE_TAG_CURSIVE_POSITIONING=$73727563,
    DWRITE_FONT_FEATURE_TAG_DEFAULT=$746c6664,
    DWRITE_FONT_FEATURE_TAG_DISCRETIONARY_LIGATURES=$67696c64,
    DWRITE_FONT_FEATURE_TAG_EXPERT_FORMS=$74707865,
    DWRITE_FONT_FEATURE_TAG_FRACTIONS=$63617266,
    DWRITE_FONT_FEATURE_TAG_FULL_WIDTH=$64697766,
    DWRITE_FONT_FEATURE_TAG_HALF_FORMS=$666c6168,
    DWRITE_FONT_FEATURE_TAG_HALANT_FORMS=$6e6c6168,
    DWRITE_FONT_FEATURE_TAG_ALTERNATE_HALF_WIDTH=$746c6168,
    DWRITE_FONT_FEATURE_TAG_HISTORICAL_FORMS=$74736968,
    DWRITE_FONT_FEATURE_TAG_HORIZONTAL_KANA_ALTERNATES=$616e6b68,
    DWRITE_FONT_FEATURE_TAG_HISTORICAL_LIGATURES=$67696c68,
    DWRITE_FONT_FEATURE_TAG_HALF_WIDTH=$64697768,
    DWRITE_FONT_FEATURE_TAG_HOJO_KANJI_FORMS=$66a668,
    DWRITE_FONT_FEATURE_TAG_JIS04_FORMS=$3430706a,
    DWRITE_FONT_FEATURE_TAG_JIS78_FORMS=$3837706a,
    DWRITE_FONT_FEATURE_TAG_JIS83_FORMS=$3338706a,
    DWRITE_FONT_FEATURE_TAG_JIS90_FORMS=$3039706a,
    DWRITE_FONT_FEATURE_TAG_KERNING=$6e72656b,
    DWRITE_FONT_FEATURE_TAG_STANDARD_LIGATURES=$6167696c,
    DWRITE_FONT_FEATURE_TAG_LINING_FIGURES=$6d756e6c,
    DWRITE_FONT_FEATURE_TAG_LOCALIZED_FORMS=$6c6366c,
    DWRITE_FONT_FEATURE_TAG_MARK_POSITIONING=$6b72616d,
    DWRITE_FONT_FEATURE_TAG_MATHEMATICAL_GREEK=$6b72676d,
    DWRITE_FONT_FEATURE_TAG_MARK_TO_MARK_POSITIONING=$6b6d6b6d,
    DWRITE_FONT_FEATURE_TAG_ALTERNATE_ANNOTATION_FORMS=$746c616e,
    DWRITE_FONT_FEATURE_TAG_NLC_KANJI_FORMS=$6b636c6e,
    DWRITE_FONT_FEATURE_TAG_OLD_STYLE_FIGURES=$6d756e6,
    DWRITE_FONT_FEATURE_TAG_ORDINALS=$6e64726,
    DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_ALTERNATE_WIDTH=$746c6170,
    DWRITE_FONT_FEATURE_TAG_PETITE_CAPITALS=$70616370,
    DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_FIGURES=$6d756e70,
    DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_WIDTHS=$64697770,
    DWRITE_FONT_FEATURE_TAG_QUARTER_WIDTHS=$64697771,
    DWRITE_FONT_FEATURE_TAG_REQUIRED_LIGATURES=$67696c72,
    DWRITE_FONT_FEATURE_TAG_RUBY_NOTATION_FORMS=$79627572,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_ALTERNATES=$746c6173,
    DWRITE_FONT_FEATURE_TAG_SCIENTIFIC_INFERIORS=$666e6973,
    DWRITE_FONT_FEATURE_TAG_SMALL_CAPITALS=$70636d73,
    DWRITE_FONT_FEATURE_TAG_SIMPLIFIED_FORMS=$6c706d73,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_1=$31307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_2=$32307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_3=$33307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_4=$34307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_5=$35307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_6=$36307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_7=$37307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_8=$38307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_9=$39307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_10=$30317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_11=$31317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_12=$32317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_13=$33317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_14=$34317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_15=$35317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_16=$36317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_17=$37317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_18=$38317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_19=$39317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_20=$30327373,
    DWRITE_FONT_FEATURE_TAG_SUBSCRIPT=$73627573,
    DWRITE_FONT_FEATURE_TAG_SUPERSCRIPT=$73707573,
    DWRITE_FONT_FEATURE_TAG_SWASH=$68737773,
    DWRITE_FONT_FEATURE_TAG_TITLING=$6c746974,
    DWRITE_FONT_FEATURE_TAG_TRADITIONAL_NAME_FORMS=$6d616e74,
    DWRITE_FONT_FEATURE_TAG_TABULAR_FIGURES=$6d756e74,
    DWRITE_FONT_FEATURE_TAG_TRADITIONAL_FORMS=$64617274,
    DWRITE_FONT_FEATURE_TAG_THIRD_WIDTHS=$64697774,
    DWRITE_FONT_FEATURE_TAG_UNICASE=$63696e75,
    DWRITE_FONT_FEATURE_TAG_SLASHED_ZERO=$672657a
  );
  PTDWrite_FontFeatureTag=^TDWrite_FontFeatureTag;
  DWRITE_FONT_FEATURE_TAG=TDWrite_FontFeatureTag;
  PDWRITE_FONT_FEATURE_TAG=^TDWrite_FontFeatureTag;

  TDWrite_TextRange=record
    StartPosition:LongWord;
    Length:LongWord;
  end;
  PTDWrite_TextRange=^TDWrite_TextRange;
  DWRITE_TEXT_RANGE=TDWrite_TextRange;
  PDWRITE_TEXT_RANGE=^TDWrite_TextRange;

  TDWrite_FontFeature=record
    NameTag:TDWrite_FontFeatureTag;
    Parameter:LongWord;
  end;
  PTDWrite_FontFeature=^TDWrite_FontFeature;
  DWRITE_FONT_FEATURE=TDWrite_FontFeature;
  PDWRITE_FONT_FEATURE=^TDWrite_FontFeature;

  TDWrite_TypographicFeatures=record
    Features:PTDWrite_FontFeature; (* __field_ecount(featureCount) *)
    FeatureCount:LongWord;
  end;
  PTDWrite_TypographicFeatures=^TDWrite_TypographicFeatures;
  PPTDWrite_TypographicFeatures=^PTDWrite_TypographicFeatures;
  DWRITE_TYPOGRAPHIC_FEATURES=TDWrite_TypographicFeatures;
  PDWRITE_TYPOGRAPHIC_FEATURES=^TDWrite_TypographicFeatures;

  TDWrite_Trimming=record
    Granularity:TDWrite_TrimmingGranularity;
    Delimiter:LongWord;
    DelimiterCount:LongWord;
  end;
  PTDWrite_Trimming=^TDWrite_Trimming;
  DWRITE_TRIMMING=TDWrite_Trimming;
  PDWRITE_TRIMMING=^TDWrite_Trimming;

  IDWriteTypography=interface;
  PIDWriteTypography=^IDWriteTypography;

  IDWriteInlineObject=interface;
  PIDWriteInlineObject=^IDWriteInlineObject;

  IDWriteTextFormat=interface(IUnknown)
    ['{9C906818-31D7-4FD3-A151-7C5E225DB55A}']
    function SetTextAlignment
    (
      TextAlignment:TDWrite_TextAlignment
    ):HResult; stdcall;

    function SetParagraphAlignment
    (
      ParagraphAlignment:TDWrite_ParagraphAlignment
    ):HResult; stdcall;

    function SetWordWrapping
    (
      WordWrapping:TDWrite_WordWrapping
    ):HResult; stdcall;

    function SetReadingDirection
    (
      ReadingDirection:TDWrite_ReadingDirection
    ):HResult; stdcall;

    function SetFlowDirection
    (
      FlowDirection:TDWrite_FlowDirection
    ):HResult; stdcall;

    function SetIncrementalTabStop
    (
      IncrementalTabStop:Single
    ):HResult; stdcall;

    function SetTrimming
    (
      const TrimmingOptions:TDWrite_Trimming; (* __in *)
      TrimmingSign:IDWriteInlineObject
    ):HResult; stdcall;

    function SetLineSpacing
    (
      LineSpacingMethod:TDWrite_LineSpacingMethod;
      LineSpacing:Single;
      Baseline:Single
    ):HResult; stdcall;

    function GetTextAlignment:TDWrite_TextAlignment; stdcall;

    function GetParagraphAlignment:TDWrite_ParagraphAlignment; stdcall;

    function GetWordWrapping:TDWrite_WordWrapping; stdcall;

    function GetReadingDirection:TDWrite_ReadingDirection; stdcall;

    function GetFlowDirection:TDWrite_FlowDirection; stdcall;

    function GetIncrementalTabStop:Single; stdcall;

    function GetTrimming
    (
      out TrimmingOptions:TDWrite_Trimming; (* __out *)
      out TrimmingSign:IDWriteInlineObject (* __out *)
    ):HResult; stdcall;

    function GetLineSpacing
    (
      out LineSpacingMethod:TDWrite_LineSpacingMethod; (* __out *)
      out LineSpacing:Single; (* __out *)
      out Baseline:Single (* __out *)
    ):HResult; stdcall;

    function GetFontCollection
    (
      out FontCollection:IDWriteFontCollection (* __out *)
    ):HResult; stdcall;

    function GetFontFamilyNameLength:LongWord; stdcall;

    function GetFontFamilyName
    (
      FontFamilyName:PWideChar; (* __out_ecount_z(nameSize) *)
      NameSize:LongWord
    ):HResult; stdcall;

    function GetFontWeight:TDWrite_FontWeight; stdcall;

    function GetFontStyle:TDWrite_FontStyle; stdcall;

    function GetFontStretch:TDWrite_FontStretch; stdcall;

    function GetFontSize:Single; stdcall;

    function GetLocaleNameLength:LongWord; stdcall;

    function GetLocaleName
    (
      LocaleName:PWideChar; (* __out_ecount_z(nameSize) *)
      NameSize:LongWord
    ):HResult; stdcall;
  end;

  IDWriteTypography=interface(IUnknown)
    ['{55F1112B-1DC2-4B3C-9541-F46894ED85B6}']
    function AddFontFeature
    (
      FontFeature:TDWrite_FontFeature
    ):HResult; stdcall;

    function GetFontFeatureCount:LongWord; stdcall;

    function GetFontFeature
    (
      FontFeatureIndex:LongWord;
      out FontFeature:TDWrite_FontFeature (* __out *)
    ):HResult; stdcall;
  end;

  TDWrite_ScriptShapes=
  (
    DWRITE_SCRIPT_SHAPES_DEFAULT=0,
    DWRITE_SCRIPT_SHAPES_NO_VISUAL=1
  );
  PTDWrite_ScriptShapes=^TDWrite_ScriptShapes;
  DWRITE_SCRIPT_SHAPES=TDWrite_ScriptShapes;
  PDWRITE_SCRIPT_SHAPES=^TDWrite_ScriptShapes;

  TDWrite_ScriptAnalysis=record
    Script:Word;
    Shapes:TDWrite_ScriptShapes;
  end;
  PTDWrite_ScriptAnalysis=^TDWrite_ScriptAnalysis;
  DWRITE_SCRIPT_ANALYSIS=TDWrite_ScriptAnalysis;
  PDWRITE_SCRIPT_ANALYSIS=^TDWrite_ScriptAnalysis;

  TDWrite_BreakCondition=
  (
    DWRITE_BREAK_CONDITION_NEUTRAL,
    DWRITE_BREAK_CONDITION_CAN_BREAK,
    DWRITE_BREAK_CONDITION_MAY_NOT_BREAK,
    DWRITE_BREAK_CONDITION_MUST_BREAK
  );
  PTDWrite_BreakCondition=^TDWrite_BreakCondition;
  DWRITE_BREAK_CONDITION=TDWrite_BreakCondition;
  PDWRITE_BREAK_CONDITION=^TDWrite_BreakCondition;

  TDWrite_LineBreakpoint=record
    LineBreakpointConditions:Byte;
  end;
  PTDWrite_LineBreakpoint=^TDWrite_LineBreakpoint;
  DWRITE_LINE_BREAKPOINT=TDWrite_LineBreakpoint;
  PDWRITE_LINE_BREAKPOINT=^TDWrite_LineBreakpoint;

  TDWrite_NumberSubstitutionMethod=
  (
    DWRITE_NUMBER_SUBSTITUTION_METHOD_FROM_CULTURE,
    DWRITE_NUMBER_SUBSTITUTION_METHOD_CONTEXTUAL,
    DWRITE_NUMBER_SUBSTITUTION_METHOD_NONE,
    DWRITE_NUMBER_SUBSTITUTION_METHOD_NATIONAL,
    DWRITE_NUMBER_SUBSTITUTION_METHOD_TRADITIONAL
  );
  PTDWrite_NumberSubstitutionMethod=^TDWrite_NumberSubstitutionMethod;
  DWRITE_NUMBER_SUBSTITUTION_METHOD=TDWrite_NumberSubstitutionMethod;
  PDWRITE_NUMBER_SUBSTITUTION_METHOD=^TDWrite_NumberSubstitutionMethod;

  IDWriteNumberSubstitution=interface(IUnknown)
    ['{14885CC9-BAB0-4F90-B6ED-5C366A2CD03D}']
  end;

  TDWrite_ShapingTextProperties=record
    ShapingTextProperties:Word;
  end;
  PTDWrite_ShapingTextProperties=^TDWrite_ShapingTextProperties;
  DWRITE_SHAPING_TEXT_PROPERTIES=TDWrite_ShapingTextProperties;
  PDWRITE_SHAPING_TEXT_PROPERTIES=^TDWrite_ShapingTextProperties;

  TDWrite_ShapingGlyphProperties=record
    ShapingGlyphProperties:Word;
  end;
  PTDWrite_ShapingGlyphProperties=^TDWrite_ShapingGlyphProperties;
  DWRITE_SHAPING_GLYPH_PROPERTIES=TDWrite_ShapingGlyphProperties;
  PDWRITE_SHAPING_GLYPH_PROPERTIES=^TDWrite_ShapingGlyphProperties;

  IDWriteTextAnalysisSource=interface(IUnknown)
    ['{688E1A58-5094-47C8-ADC8-FBCEA60AE92B}']
    function GetTextAtPosition
    (
      TextPosition:LongWord;
      out TextString:PWideChar; (* __out *)
      out TextLength:LongWord (* __out *)
    ):HResult; stdcall;

    function GetTextBeforePosition
    (
      TextPosition:LongWord;
      out TextString:PWideChar; (* __out *)
      out TextLength:LongWord (* __out *)
    ):HResult; stdcall;

    function GetParagraphReadingDirection:TDWrite_ReadingDirection; stdcall;

    function GetLocaleName
    (
      TextPosition:LongWord;
      out TextLength:LongWord; (* __out *)
      LocaleName:PPWideChar (* __out_z *)
    ):HResult; stdcall;

    function GetNumberSubstitution
    (
      TextPosition:LongWord;
      out TextLength:LongWord; (* __out *)
      out NumberSubstitution:IDWriteNumberSubstitution (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteTextAnalysisSink=interface(IUnknown)
    ['{5810CD44-0CA0-4701-B3FA-BEC5182AE4F6}']
    function SetScriptAnalysis
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      const ScriptAnalysis:TDWrite_ScriptAnalysis (* __in *)
    ):HResult; stdcall;

    function SetLineBreakpoints
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      LineBreakpoints:PTDWrite_LineBreakpoint (* __in_ecount(textLength) *)
    ):HResult; stdcall;

    function SetBidiLevel
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      ExplicitLevel:Byte;
      ResolvedLevel:Byte
    ):HResult; stdcall;

    function SetNumberSubstitution
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      NumberSubstitution:IDWriteNumberSubstitution (* __notnull *)
    ):HResult; stdcall;
  end;

  IDWriteTextAnalyzer=interface(IUnknown)
    ['{B7E6163E-7F46-43B4-84B3-E4E6249C365D}']
    function AnalyzeScript
    (
      AnalysisSource:IDWriteTextAnalysisSource;
      TextPosition:LongWord;
      TextLength:LongWord;
      AnalysisSink:IDWriteTextAnalysisSink
    ):HResult; stdcall;

    function AnalyzeBidi
    (
      AnalysisSource:IDWriteTextAnalysisSource;
      TextPosition:LongWord;
      TextLength:LongWord;
      AnalysisSink:IDWriteTextAnalysisSink
    ):HResult; stdcall;

    function AnalyzeNumberSubstitution
    (
      AnalysisSource:IDWriteTextAnalysisSource;
      TextPosition:LongWord;
      TextLength:LongWord;
      AnalysisSink:IDWriteTextAnalysisSink
    ):HResult; stdcall;

    function AnalyzeLineBreakpoints
    (
      AnalysisSource:IDWriteTextAnalysisSource;
      TextPosition:LongWord;
      TextLength:LongWord;
      AnalysisSink:IDWriteTextAnalysisSink
    ):HResult; stdcall;

    function GetGlyphs
    (
      TextString:PWideChar; (* __in_ecount(textLength) *)
      TextLength:LongWord;
      FontFace:IDWriteFontFace;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      const ScriptAnalysis:TDWrite_ScriptAnalysis; (* __in *)
      LocaleName:PWideChar; (* __in_z_opt *)
      NumberSubstitution:IDWriteNumberSubstitution; (* __maybenull *)
      Features:PPTDWrite_TypographicFeatures; (* __in_ecount_opt(featureRanges) *)
      FeatureRangeLengths:PLongWord; (* __in_ecount_opt(featureRanges) *)
      FeatureRanges:LongWord;
      MaxGlyphCount:LongWord;
      ClusterMap:PWord; (* __out_ecount(textLength) *)
      TextProps:PTDWrite_ShapingTextProperties; (* __out_ecount(textLength) *)
      GlyphIndices:PWord; (* __out_ecount(maxGlyphCount) *)
      GlyphProps:PTDWrite_ShapingGlyphProperties; (* __out_ecount(maxGlyphCount) *)
      out ActualGlyphCount:LongWord (* __out *)
    ):HResult; stdcall;

    function GetGlyphPlacements
    (
      TextString:PWideChar; (* __in_ecount(textLength) *)
      ClusterMap:PWord; (* __in_ecount(textLength) *)
      TextProps:PTDWrite_ShapingTextProperties; (* __in_ecount(textLength) *)
      TextLength:LongWord;
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphProps:PTDWrite_ShapingGlyphProperties; (* __in_ecount(glyphCount) *)
      GlyphCount:LongWord;
      FontFace:IDWriteFontFace;
      FontEmSize:Single;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      const ScriptAnalysis:TDWrite_ScriptAnalysis; (* __in *)
      LocaleName:PWideChar; (* __in_z_opt *)
      Features:PPTDWrite_TypographicFeatures; (* __in_ecount_opt(featureRanges) *)
      FeatureRangeLengths:PLongWord; (* __in_ecount_opt(featureRanges) *)
      FeatureRanges:LongWord;
      GlyphAdvances:PSingle; (* __out_ecount(glyphCount) *)
      GlyphOffsets:PTDWrite_GlyphOffset (* __out_ecount(glyphCount) *)
    ):HResult; stdcall;

    function GetGdiCompatibleGlyphPlacements
    (
      TextString:PWideChar; (* __in_ecount(textLength) *)
      ClusterMap:PWord; (* __in_ecount(textLength) *)
      TextProps:PTDWrite_ShapingTextProperties; (* __in_ecount(textLength) *)
      TextLength:LongWord;
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphProps:PTDWrite_ShapingGlyphProperties; (* __in_ecount(glyphCount) *)
      GlyphCount:LongWord;
      FontFace:IDWriteFontFace;
      FontEmSize:Single;
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      UseGdiNatural:LongBool;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      const ScriptAnalysis:TDWrite_ScriptAnalysis; (* __in *)
      LocaleName:PWideChar; (* __in_z_opt *)
      Features:PPTDWrite_TypographicFeatures; (* __in_ecount_opt(featureRanges) *)
      FeatureRangeLengths:PLongWord; (* __in_ecount_opt(featureRanges) *)
      FeatureRanges:LongWord;
      GlyphAdvances:PSingle; (* __out_ecount(glyphCount) *)
      GlyphOffsets:PTDWrite_GlyphOffset (* __out_ecount(glyphCount) *)
    ):HResult; stdcall;
  end;

  TDWrite_GlyphRun=record
    FontFace:IDWriteFontFace; (* __notnull *)
    FontEmSize:Single;
    GlyphCount:LongWord;
    GlyphIndices:PWord; (* __field_ecount(glyphCount) *)
    GlyphAdvances:PSingle; (* __field_ecount_opt(glyphCount) *)
    GlyphOffsets:PTDWrite_GlyphOffset; (* __field_ecount_opt(glyphCount) *)
    IsSideways:LongBool;
    BidiLevel:LongWord;
  end;
  PTDWrite_GlyphRun=^TDWrite_GlyphRun;
  DWRITE_GLYPH_RUN=TDWrite_GlyphRun;
  PDWRITE_GLYPH_RUN=^TDWrite_GlyphRun;

  TDWrite_GlyphRunDescription=record
    LocaleName:PWideChar; (* __nullterminated *)
    _String:PWideChar; (* __field_ecount(stringLength) *)
    StringLength:LongWord;
    ClusterMap:PWord; (* __field_ecount(stringLength) *)
    TextPosition:LongWord;
  end;
  PTDWrite_GlyphRunDescription=^TDWrite_GlyphRunDescription;
  DWRITE_GLYPH_RUN_DESCRIPTION=TDWrite_GlyphRunDescription;
  PDWRITE_GLYPH_RUN_DESCRIPTION=^TDWrite_GlyphRunDescription;

  TDWrite_Underline=record
    Width:Single;
    Thickness:Single;
    Offset:Single;
    RunHeight:Single;
    ReadingDirection:TDWrite_ReadingDirection;
    FlowDirection:TDWrite_FlowDirection;
    LocaleName:PWideChar; (* __nullterminated *)
    MeasuringMode:TDWrite_MeasuringMode;
  end;
  PTDWrite_Underline=^TDWrite_Underline;
  DWRITE_UNDERLINE=TDWrite_Underline;
  PDWRITE_UNDERLINE=^TDWrite_Underline;

  TDWrite_Strikethrough=record
    Width:Single;
    Thickness:Single;
    Offset:Single;
    ReadingDirection:TDWrite_ReadingDirection;
    FlowDirection:TDWrite_FlowDirection;
    LocaleName:PWideChar; (* __nullterminated *)
    MeasuringMode:TDWrite_MeasuringMode;
  end;
  PTDWrite_Strikethrough=^TDWrite_Strikethrough;
  DWRITE_STRIKETHROUGH=TDWrite_Strikethrough;
  PDWRITE_STRIKETHROUGH=^TDWrite_Strikethrough;

  TDWrite_LineMetrics=record
    Length:LongWord;
    TrailingWhitespaceLength:LongWord;
    NewlineLength:LongWord;
    Height:Single;
    Baseline:Single;
    IsTrimmed:LongBool;
  end;
  PTDWrite_LineMetrics=^TDWrite_LineMetrics;
  DWRITE_LINE_METRICS=TDWrite_LineMetrics;
  PDWRITE_LINE_METRICS=^TDWrite_LineMetrics;

  TDWrite_ClusterMetrics=record
    Width:Single;
    Length:Word;
    ClusterMetrics:Word;
  end;
  PTDWrite_ClusterMetrics=^TDWrite_ClusterMetrics;
  DWRITE_CLUSTER_METRICS=TDWrite_ClusterMetrics;
  PDWRITE_CLUSTER_METRICS=^TDWrite_ClusterMetrics;

  TDWrite_TextMetrics=record
    Left:Single;
    Top:Single;
    Width:Single;
    WidthIncludingTrailingWhitespace:Single;
    Height:Single;
    LayoutWidth:Single;
    LayoutHeight:Single;
    MaxBidiReorderingDepth:LongWord;
    LineCount:LongWord;
  end;
  PTDWrite_TextMetrics=^TDWrite_TextMetrics;
  DWRITE_TEXT_METRICS=TDWrite_TextMetrics;
  PDWRITE_TEXT_METRICS=^TDWrite_TextMetrics;

  TDWrite_InlineObjectMetrics=record
    Width:Single;
    Height:Single;
    Baseline:Single;
    SupportsSideways:LongBool;
  end;
  PTDWrite_InlineObjectMetrics=^TDWrite_InlineObjectMetrics;
  DWRITE_INLINE_OBJECT_METRICS=TDWrite_InlineObjectMetrics;
  PDWRITE_INLINE_OBJECT_METRICS=^TDWrite_InlineObjectMetrics;

  TDWrite_OverhangMetrics=record
    Left:Single;
    Top:Single;
    Right:Single;
    Bottom:Single;
  end;
  PTDWrite_OverhangMetrics=^TDWrite_OverhangMetrics;
  DWRITE_OVERHANG_METRICS=TDWrite_OverhangMetrics;
  PDWRITE_OVERHANG_METRICS=^TDWrite_OverhangMetrics;

  TDWrite_HitTestMetrics=record
    TextPosition:LongWord;
    Length:LongWord;
    Left:Single;
    Top:Single;
    Width:Single;
    Height:Single;
    BidiLevel:LongWord;
    IsText:LongBool;
    IsTrimmed:LongBool;
  end;
  PTDWrite_HitTestMetrics=^TDWrite_HitTestMetrics;
  DWRITE_HIT_TEST_METRICS=TDWrite_HitTestMetrics;
  PDWRITE_HIT_TEST_METRICS=^TDWrite_HitTestMetrics;

  IDWriteTextRenderer=interface;
  PIDWriteTextRenderer=^IDWriteTextRenderer;

  IDWriteInlineObject=interface(IUnknown)
    ['{8339FDE3-106F-47AB-8373-1C6295EB10B3}']
    function Draw
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      Renderer:IDWriteTextRenderer;
      OriginX:Single;
      OriginY:Single;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function GetMetrics
    (
      out Metrics:TDWrite_InlineObjectMetrics (* __out *)
    ):HResult; stdcall;

    function GetOverhangMetrics
    (
      out Overhangs:TDWrite_OverhangMetrics (* __out *)
    ):HResult; stdcall;

    function GetBreakConditions
    (
      out BreakConditionBefore:TDWrite_BreakCondition; (* __out *)
      out BreakConditionAfter:TDWrite_BreakCondition (* __out *)
    ):HResult; stdcall;
  end;

  IDWritePixelSnapping=interface(IUnknown)
    ['{EAF3A2DA-ECF4-4D24-B644-B34F6842024B}']
    function IsPixelSnappingDisabled
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      out IsDisabled:LongBool (* __out *)
    ):HResult; stdcall;

    function GetCurrentTransform
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      out Transform:TDWrite_Matrix (* __out *)
    ):HResult; stdcall;

    function GetPixelsPerDip
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      out PixelsPerDip:Single (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteTextRenderer=interface(IDWritePixelSnapping)
    ['{EF8A8135-5CC6-45FE-8825-C5A0724EB819}']
    function DrawGlyphRun
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      MeasuringMode:TDWrite_MeasuringMode;
      const GlyphRun:TDWrite_GlyphRun; (* __in *)
      const GlyphRunDescription:TDWrite_GlyphRunDescription; (* __in *)
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function DrawUnderline
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      const Underline:TDWrite_Underline; (* __in *)
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function DrawStrikethrough
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      const Strikethrough:TDWrite_Strikethrough; (* __in *)
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function DrawInlineObject
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      OriginX:Single;
      OriginY:Single;
      InlineObject:IDWriteInlineObject;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;
  end;

  IDWriteTextLayout=interface(IDWriteTextFormat)
    ['{53737037-6D14-410B-9BFE-0B182BB70961}']
    function SetMaxWidth
    (
      MaxWidth:Single
    ):HResult; stdcall;

    function SetMaxHeight
    (
      MaxHeight:Single
    ):HResult; stdcall;

    function SetFontCollection
    (
      FontCollection:IDWriteFontCollection;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontFamilyName
    (
      FontFamilyName:PWideChar; (* __in_z *)
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontWeight
    (
      FontWeight:TDWrite_FontWeight;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontStyle
    (
      FontStyle:TDWrite_FontStyle;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontStretch
    (
      FontStretch:TDWrite_FontStretch;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontSize
    (
      FontSize:Single;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetUnderline
    (
      HasUnderline:LongBool;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetStrikethrough
    (
      HasStrikethrough:LongBool;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetDrawingEffect
    (
      DrawingEffect:IUnknown;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetInlineObject
    (
      InlineObject:IDWriteInlineObject;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetTypography
    (
      Typography:IDWriteTypography;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetLocaleName
    (
      LocaleName:PWideChar; (* __in_z *)
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function GetMaxWidth:Single; stdcall;

    function GetMaxHeight:Single; stdcall;

    function GetFontCollection
    (
      CurrentPosition:LongWord;
      out FontCollection:IDWriteFontCollection; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontFamilyNameLength
    (
      CurrentPosition:LongWord;
      out NameLength:LongWord; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontFamilyName
    (
      CurrentPosition:LongWord;
      FontFamilyName:PWideChar; (* __out_ecount_z(nameSize) *)
      NameSize:LongWord;
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontWeight
    (
      CurrentPosition:LongWord;
      out FontWeight:TDWrite_FontWeight; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontStyle
    (
      CurrentPosition:LongWord;
      out FontStyle:TDWrite_FontStyle; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontStretch
    (
      CurrentPosition:LongWord;
      out FontStretch:TDWrite_FontStretch; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontSize
    (
      CurrentPosition:LongWord;
      out FontSize:Single; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetUnderline
    (
      CurrentPosition:LongWord;
      out HasUnderline:LongBool; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetStrikethrough
    (
      CurrentPosition:LongWord;
      out HasStrikethrough:LongBool; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetDrawingEffect
    (
      CurrentPosition:LongWord;
      out DrawingEffect:IUnknown; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetInlineObject
    (
      CurrentPosition:LongWord;
      out InlineObject:IDWriteInlineObject; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetTypography
    (
      CurrentPosition:LongWord;
      out Typography:IDWriteTypography; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetLocaleNameLength
    (
      CurrentPosition:LongWord;
      out NameLength:LongWord; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetLocaleName
    (
      CurrentPosition:LongWord;
      LocaleName:PWideChar; (* __out_ecount_z(nameSize) *)
      NameSize:LongWord;
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function Draw
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      Renderer:IDWriteTextRenderer;
      OriginX:Single;
      OriginY:Single
    ):HResult; stdcall;

    function GetLineMetrics
    (
      LineMetrics:PTDWrite_LineMetrics; (* __out_ecount_opt(maxLineCount) *)
      MaxLineCount:LongWord;
      out ActualLineCount:LongWord (* __out *)
    ):HResult; stdcall;

    function GetMetrics
    (
      out TextMetrics:TDWrite_TextMetrics (* __out *)
    ):HResult; stdcall;

    function GetOverhangMetrics
    (
      out Overhangs:TDWrite_OverhangMetrics (* __out *)
    ):HResult; stdcall;

    function GetClusterMetrics
    (
      ClusterMetrics:PTDWrite_ClusterMetrics; (* __out_ecount_opt(maxClusterCount) *)
      MaxClusterCount:LongWord;
      out ActualClusterCount:LongWord (* __out *)
    ):HResult; stdcall;

    function DetermineMinWidth
    (
      out MinWidth:Single (* __out *)
    ):HResult; stdcall;

    function HitTestPoint
    (
      PointX:Single;
      PointY:Single;
      out IsTrailingHit:LongBool; (* __out *)
      out IsInside:LongBool; (* __out *)
      out HitTestMetrics:TDWrite_HitTestMetrics (* __out *)
    ):HResult; stdcall;

    function HitTestTextPosition
    (
      TextPosition:LongWord;
      IsTrailingHit:LongBool;
      out PointX:Single; (* __out *)
      out PointY:Single; (* __out *)
      out HitTestMetrics:TDWrite_HitTestMetrics (* __out *)
    ):HResult; stdcall;

    function HitTestTextRange
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      OriginX:Single;
      OriginY:Single;
      HitTestMetrics:PTDWrite_HitTestMetrics; (* __out_ecount_opt(maxHitTestMetricsCount) *)
      MaxHitTestMetricsCount:LongWord;
      out ActualHitTestMetricsCount:LongWord (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteBitmapRenderTarget=interface(IUnknown)
    ['{5E5A32A3-8DFF-4773-9FF6-0696EAB77267}']
    function DrawGlyphRun
    (
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      MeasuringMode:TDWrite_MeasuringMode;
      const GlyphRun:TDWrite_GlyphRun; (* __in *)
      RenderingParams:IDWriteRenderingParams;
      TextColor:TCOLORREF;
      BlackBoxRect:PTRect=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetMemoryDC:HDC; stdcall;

    function GetPixelsPerDip:Single; stdcall;

    function SetPixelsPerDip
    (
      PixelsPerDip:Single
    ):HResult; stdcall;

    function GetCurrentTransform
    (
      out Transform:TDWrite_Matrix (* __out *)
    ):HResult; stdcall;

    function SetCurrentTransform
    (
      Transform:PTDWrite_Matrix (* __in_opt *)
    ):HResult; stdcall;

    function GetSize
    (
      out Size:TSIZE (* __out *)
    ):HResult; stdcall;

    function Resize
    (
      Width:LongWord;
      Height:LongWord
    ):HResult; stdcall;
  end;

  IDWriteGdiInterop=interface(IUnknown)
    ['{1EDD9491-9853-4299-898F-6432983B6F3A}']
    function CreateFontFromLOGFONT
    (
      const LogFont:TLOGFONTW; (* __in *)
      out Font:IDWriteFont (* __out *)
    ):HResult; stdcall;

    function ConvertFontToLOGFONT
    (
      Font:IDWriteFont;
      out LogFont:TLOGFONTW; (* __out *)
      out IsSystemFont:LongBool (* __out *)
    ):HResult; stdcall;

    function ConvertFontFaceToLOGFONT
    (
      Font:IDWriteFontFace;
      out LogFont:TLOGFONTW (* __out *)
    ):HResult; stdcall;

    function CreateFontFaceFromHdc
    (
      hDC:HDC;
      out FontFace:IDWriteFontFace (* __out *)
    ):HResult; stdcall;

    function CreateBitmapRenderTarget
    (
      hDC:HDC; (* __in_opt *)
      Width:LongWord;
      Height:LongWord;
      out RenderTarget:IDWriteBitmapRenderTarget (* __out *)
    ):HResult; stdcall;
  end;

  TDWrite_TextureType=
  (
    DWRITE_TEXTURE_ALIASED_1x1,
    DWRITE_TEXTURE_CLEARTYPE_3x1
  );
  PTDWrite_TextureType=^TDWrite_TextureType;
  DWRITE_TEXTURE_TYPE=TDWrite_TextureType;
  PDWRITE_TEXTURE_TYPE=^TDWrite_TextureType;

  IDWriteGlyphRunAnalysis=interface(IUnknown)
    ['{7D97DBF7-E085-42D4-81E3-6A883BDED118}']
    function GetAlphaTextureBounds
    (
      TextureType:TDWrite_TextureType;
      out TextureBounds:TRect (* __out *)
    ):HResult; stdcall;

    function CreateAlphaTexture
    (
      TextureType:TDWrite_TextureType;
      const TextureBounds:TRect; (* __in *)
      AlphaValues:PByte; (* __out_bcount(bufferSize) *)
      BufferSize:LongWord
    ):HResult; stdcall;

    function GetAlphaBlendParams
    (
      RenderingParams:IDWriteRenderingParams;
      out BlendGamma:Single; (* __out *)
      out BlendEnhancedContrast:Single; (* __out *)
      out BlendClearTypeLevel:Single (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFactory=interface(IUnknown)
    ['{B859EE5A-D838-4B5B-A2E8-1ADC7D93DB48}']
    function GetSystemFontCollection
    (
      out FontCollection:IDWriteFontCollection; (* __out *)
      CheckForUpdates:LongBool=FALSE
    ):HResult; stdcall;

    function CreateCustomFontCollection
    (
      CollectionLoader:IDWriteFontCollectionLoader;
      CollectionKey:Pointer; (* __in_bcount(collectionKeySize) *)
      CollectionKeySize:LongWord;
      out FontCollection:IDWriteFontCollection (* __out *)
    ):HResult; stdcall;

    function RegisterFontCollectionLoader
    (
      FontCollectionLoader:IDWriteFontCollectionLoader
    ):HResult; stdcall;

    function UnregisterFontCollectionLoader
    (
      FontCollectionLoader:IDWriteFontCollectionLoader
    ):HResult; stdcall;

    function CreateFontFileReference
    (
      FilePath:PWideChar; (* __in_z *)
      LastWriteTime:PFILETIME; (* __in_opt *)
      out FontFile:IDWriteFontFile (* __out *)
    ):HResult; stdcall;

    function CreateCustomFontFileReference
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      FontFileLoader:IDWriteFontFileLoader;
      out FontFile:IDWriteFontFile (* __out *)
    ):HResult; stdcall;

    function CreateFontFace
    (
      FontFaceType:TDWrite_FontFaceType;
      NumberOfFiles:LongWord;
      FontFiles:PIDWriteFontFile; (* __in_ecount(numberOfFiles) *)
      FaceIndex:LongWord;
      FontFaceSimulationFlags:TDWrite_FontSimulations;
      out FontFace:IDWriteFontFace (* __out *)
    ):HResult; stdcall;

    function CreateRenderingParams
    (
      out RenderingParams:IDWriteRenderingParams (* __out *)
    ):HResult; stdcall;

    function CreateMonitorRenderingParams
    (
      Monitor:HMONITOR;
      out RenderingParams:IDWriteRenderingParams (* __out *)
    ):HResult; stdcall;

    function CreateCustomRenderingParams
    (
      Gamma:Single;
      EnhancedContrast:Single;
      ClearTypeLevel:Single;
      PixelGeometry:TDWrite_PixelGeometry;
      RenderingMode:TDWrite_RenderingMode;
      out RenderingParams:IDWriteRenderingParams (* __out *)
    ):HResult; stdcall;

    function RegisterFontFileLoader
    (
      FontFileLoader:IDWriteFontFileLoader
    ):HResult; stdcall;

    function UnregisterFontFileLoader
    (
      FontFileLoader:IDWriteFontFileLoader
    ):HResult; stdcall;

    function CreateTextFormat
    (
      FontFamilyName:PWideChar; (* __in_z *)
      FontCollection:IDWriteFontCollection; (* __maybenull *)
      FontWeight:TDWrite_FontWeight;
      FontStyle:TDWrite_FontStyle;
      FontStretch:TDWrite_FontStretch;
      FontSize:Single;
      LocaleName:PWideChar; (* __in_z *)
      out TextFormat:IDWriteTextFormat (* __out *)
    ):HResult; stdcall;

    function CreateTypography
    (
      out Typography:IDWriteTypography (* __out *)
    ):HResult; stdcall;

    function GetGdiInterop
    (
      out gdiInterop:IDWriteGdiInterop (* __out *)
    ):HResult; stdcall;

    function CreateTextLayout
    (
      _String:PWideChar; (* __in_ecount(stringLength) *)
      StringLength:LongWord;
      TextFormat:IDWriteTextFormat;
      MaxWidth:Single;
      MaxHeight:Single;
      out TextLayout:IDWriteTextLayout (* __out *)
    ):HResult; stdcall;

    function CreateGdiCompatibleTextLayout
    (
      _String:PWideChar; (* __in_ecount(stringLength) *)
      StringLength:LongWord;
      TextFormat:IDWriteTextFormat;
      LayoutWidth:Single;
      LayoutHeight:Single;
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      UseGdiNatural:LongBool;
      out TextLayout:IDWriteTextLayout (* __out *)
    ):HResult; stdcall;

    function CreateEllipsisTrimmingSign
    (
      TextFormat:IDWriteTextFormat;
      out TrimmingSign:IDWriteInlineObject (* __out *)
    ):HResult; stdcall;

    function CreateTextAnalyzer
    (
      out TextAnalyzer:IDWriteTextAnalyzer (* __out *)
    ):HResult; stdcall;

    function CreateNumberSubstitution
    (
      SubstitutionMethod:TDWrite_NumberSubstitutionMethod; (* __in *)
      LocaleName:PWideChar; (* __in_z *)
      IgnoreUserOverride:LongBool; (* __in *)
      out NumberSubstitution:IDWriteNumberSubstitution (* __out *)
    ):HResult; stdcall;

    function CreateGlyphRunAnalysis
    (
      const GlyphRun:TDWrite_GlyphRun; (* __in *)
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      RenderingMode:TDWrite_RenderingMode;
      MeasuringMode:TDWrite_MeasuringMode;
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      out GlyphRunAnalysis:IDWriteGlyphRunAnalysis (* __out *)
    ):HResult; stdcall;
  end;

// JSB: Warning! The initial RefCount is 2, not 1, and the object frees when you call "Release" when RefCount is 2.
{$IFDEF UseRuntimeLinking}var DWriteCreateFactory:{$ENDIF}function{$IFNDEF UseRuntimeLinking}DWriteCreateFactory{$ENDIF}
(
  FactoryType:TDWrite_FactoryType; (* __in *)
  const InterfaceID:TGUID; (* __in *)
  out Factory {Interface} (* __out *)
):HResult; stdcall; {$IFNDEF UseRuntimeLinking} external DLL_DWrite; {$ENDIF}

///////////////////////////////////////////////////////////////////////////////
// End "DWrite.h"
///////////////////////////////////////////////////////////////////////////////

{$IFDEF UseRuntimeLinking}
procedure Link;
{$ENDIF}

implementation

function DWRITE_MAKE_OPENTYPE_TAG(a,b,c,d:LongWord):LongWord;
begin
  Result:=(d shl 24) or (c shl 16) or (b shl 8) or a;
end;

{$IFDEF UseJSBErrors}
function HResultToString(Value:HRESULT):String;
begin
  Result:='';
  if SUCCEEDED(Value) then Exit;

  case Value of
    DWRITE_ERROR_FILEFORMAT: Result:='DirectWrite: Invalid file format.';
    DWRITE_ERROR_UNEXPECTED: Result:='DirectWrite: Unexpected error.';
    DWRITE_ERROR_NOFONT: Result:='DirectWrite: No font.';
    DWRITE_ERROR_FILENOTFOUND: Result:='DirectWrite: File not found.';
    DWRITE_ERROR_FILEACCESS: Result:='DirectWrite: File access denied.';
    DWRITE_ERROR_FONTCOLLECTIONOBSOLETE: Result:='DirectWrite: Font collection is obsolete.';
    DWRITE_ERROR_ALREADYREGISTERED: Result:='DirectWrite: Already registered.';
  end;
end;
{$ENDIF}

{$IFDEF UseRuntimeLinking}
function LoadDLL(DLLName:String):HModule;
begin
  Result:=LoadLibrary(PChar(DLLName));
  if Result=0 then
    raise Exception.Create('Dynamic link library (DLL) '''+DLLName+''' is not available.');
end;

function LinkMethod(hDLL:HModule;MethodName,DLLName:String):Pointer;
begin
  Result:=GetProcAddress(hDLL,PChar(MethodName));
  if Result=nil then
    raise Exception.Create('Failed to link to method '''+MethodName+''' in dynamic link library (DLL) '''+DLLName+'''.');
end;

procedure Link;
var
  hDLL_DWrite:HModule;
begin
  hDLL_DWrite:=LoadDLL(DLL_DWrite);

  DWriteCreateFactory:=LinkMethod(hDLL_DWrite,'DWriteCreateFactory',DLL_DWrite);
end;
{$ENDIF}

{$IFDEF UseJSBErrors}
initialization
begin
  AddDirectXHResultToStringHandler(HResultToString);
end;
{$ENDIF}

end.
