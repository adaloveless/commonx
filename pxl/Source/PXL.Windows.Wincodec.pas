unit PXL.Windows.Wincodec;
{
  Windows Headers translation by Yuriy Kotsarenko, August 2015. Revision 1.0.

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.

  Translated Windows C/C++ file:
    wincodec.h

  Original source code was taken from:
    %WINDOWS_KITS%\8.1\Include\um\

  Note: portions of this file were translated using automated tool.
}
interface

{$IFDEF FPC}
  {$PACKRECORDS C}
  {$MODE DELPHI}
{$ELSE}
  {$ALIGN ON}
{$ENDIF}

uses
  ActiveX, Windows;

const
  WINCODEC_SDK_VERSION1 = $0236;
  WINCODEC_SDK_VERSION2 = $0237;
  WINCODEC_SDK_VERSION = WINCODEC_SDK_VERSION1;

  FACILITY_WINCODEC_ERR = $898;
  WINCODEC_ERR_BASE = $2000;

  WINCODEC_ERR_ABORTED = E_ABORT;
  WINCODEC_ERR_ACCESSDENIED = E_ACCESSDENIED;
  WINCODEC_ERR_GENERIC_ERROR = E_FAIL;
  WINCODEC_ERR_INVALIDPARAMETER = E_INVALIDARG;
  WINCODEC_ERR_NOTIMPLEMENTED = E_NOTIMPL;
  WINCODEC_ERR_OUTOFMEMORY = E_OUTOFMEMORY;

  WINCODEC_ERR_ALREADYLOCKED = $88982F0D;
  WINCODEC_ERR_BADHEADER = $88982F61;
  WINCODEC_ERR_BADIMAGE = $88982F60;
  WINCODEC_ERR_BADMETADATAHEADER = $88982F63;
  WINCODEC_ERR_BADSTREAMDATA = $88982F70;
  WINCODEC_ERR_CODECNOTHUMBNAIL = $88982F44;
  WINCODEC_ERR_CODECPRESENT = $88982F43;
  WINCODEC_ERR_CODECTOOMANYSCANLINES = $88982F46;
  WINCODEC_ERR_COMPONENTINITIALIZEFAILURE = $88982F8B;
  WINCODEC_ERR_COMPONENTNOTFOUND = $88982F50;
  WINCODEC_ERR_DUPLICATEMETADATAPRESENT = $88982F8D;
  WINCODEC_ERR_FRAMEMISSING = $88982F62;
  WINCODEC_ERR_IMAGESIZEOUTOFRANGE = $88982F51;
  WINCODEC_ERR_INSUFFICIENTBUFFER = $88982F8C;
  WINCODEC_ERR_INTERNALERROR = $88982F48;
  WINCODEC_ERR_INVALIDPROGRESSIVELEVEL = $88982F95;
  WINCODEC_ERR_INVALIDQUERYCHARACTER = $88982F93;
  WINCODEC_ERR_INVALIDQUERYREQUEST = $88982F90;
  WINCODEC_ERR_INVALIDREGISTRATION = $88982F8A;
  WINCODEC_ERR_NOTINITIALIZED = $88982F0C;
  WINCODEC_ERR_PALETTEUNAVAILABLE = $88982F45;
  WINCODEC_ERR_PROPERTYNOTFOUND = $88982F40;
  WINCODEC_ERR_PROPERTYNOTSUPPORTED = $88982F41;
  WINCODEC_ERR_PROPERTYSIZE = $88982F42;
  WINCODEC_ERR_PROPERTYUNEXPECTEDTYPE = $88982F8E;
  WINCODEC_ERR_REQUESTONLYVALIDATMETADATAROOT = $88982F92;
  WINCODEC_ERR_SOURCERECTDOESNOTMATCHDIMENSIONS = $88982F49;
  WINCODEC_ERR_STREAMNOTAVAILABLE = $88982F73;
  WINCODEC_ERR_STREAMREAD = $88982F72;
  WINCODEC_ERR_STREAMWRITE = $88982F71;
  WINCODEC_ERR_TOOMUCHMETADATA = $88982F52;
  WINCODEC_ERR_UNEXPECTEDMETADATATYPE = $88982F91;
  WINCODEC_ERR_UNEXPECTEDSIZE = $88982F8F;
  WINCODEC_ERR_UNKNOWNIMAGEFORMAT = $88982F07;
  WINCODEC_ERR_UNSUPPORTEDOPERATION = $88982F81;
  WINCODEC_ERR_UNSUPPORTEDPIXELFORMAT = $88982F80;
  WINCODEC_ERR_UNSUPPORTEDVERSION = $88982F0B;
  WINCODEC_ERR_VALUEOUTOFRANGE = $88982F05;
  WINCODEC_ERR_VALUEOVERFLOW = $80070216;
  WINCODEC_ERR_WIN32ERROR = $88982F94;
  WINCODEC_ERR_WRONGSTATE = $88982F04;

  WICRawChangeNotification_ExposureCompensation = $00000001;
  WICRawChangeNotification_NamedWhitePoint = $00000002;
  WICRawChangeNotification_KelvinWhitePoint = $00000004;
  WICRawChangeNotification_RGBWhitePoint = $00000008;
  WICRawChangeNotification_Contrast = $00000010;
  WICRawChangeNotification_Gamma = $00000020;
  WICRawChangeNotification_Sharpness = $00000040;
  WICRawChangeNotification_Saturation = $00000080;
  WICRawChangeNotification_Tint = $00000100;
  WICRawChangeNotification_NoiseReduction = $00000200;
  WICRawChangeNotification_DestinationColorContext = $00000400;
  WICRawChangeNotification_ToneCurve = $00000800;
  WICRawChangeNotification_Rotation = $00001000;
  WICRawChangeNotification_RenderMode = $00002000;

  WICColorContextUninitialized = 0;
  WICColorContextProfile = $1;
  WICColorContextExifColorSpace = $2;

  WICBitmapNoCache = 0;
  WICBitmapCacheOnDemand = $1;
  WICBitmapCacheOnLoad = $2;

  WICDecodeMetadataCacheOnDemand = 0;
  WICDecodeMetadataCacheOnLoad = $1;

  WICBitmapEncoderCacheInMemory = 0;
  WICBitmapEncoderCacheTempFile = $1;
  WICBitmapEncoderNoCache = $2;

  WICDecoder = $1;
  WICEncoder = $2;
  WICPixelFormatConverter = $4;
  WICMetadataReader = $8;
  WICMetadataWriter = $10;
  WICPixelFormat = $20;
  WICAllComponents = $3F;

  WICComponentEnumerateDefault = 0;
  WICComponentEnumerateRefresh = $1;
  WICComponentEnumerateDisabled = $80000000;
  WICComponentEnumerateUnsigned = $40000000;
  WICComponentEnumerateBuiltInOnly = $20000000;

  WICBitmapInterpolationModeNearestNeighbor = 0;
  WICBitmapInterpolationModeLinear = $1;
  WICBitmapInterpolationModeCubic = $2;
  WICBitmapInterpolationModeFant = $3;

  WICBitmapPaletteTypeCustom = 0;
  WICBitmapPaletteTypeMedianCut = $1;
  WICBitmapPaletteTypeFixedBW = $2;
  WICBitmapPaletteTypeFixedHalftone8 = $3;
  WICBitmapPaletteTypeFixedHalftone27 = $4;
  WICBitmapPaletteTypeFixedHalftone64 = $5;
  WICBitmapPaletteTypeFixedHalftone125 = $6;
  WICBitmapPaletteTypeFixedHalftone216 = $7;
  WICBitmapPaletteTypeFixedWebPalette = WICBitmapPaletteTypeFixedHalftone216;
  WICBitmapPaletteTypeFixedHalftone252 = $8;
  WICBitmapPaletteTypeFixedHalftone256 = $9;
  WICBitmapPaletteTypeFixedGray4 = $A;
  WICBitmapPaletteTypeFixedGray16 = $B;
  WICBitmapPaletteTypeFixedGray256 = $C;

  WICBitmapDitherTypeNone = 0;
  WICBitmapDitherTypeSolid = 0;
  WICBitmapDitherTypeOrdered4x4 = $1;
  WICBitmapDitherTypeOrdered8x8 = $2;
  WICBitmapDitherTypeOrdered16x16 = $3;
  WICBitmapDitherTypeSpiral4x4 = $4;
  WICBitmapDitherTypeSpiral8x8 = $5;
  WICBitmapDitherTypeDualSpiral4x4 = $6;
  WICBitmapDitherTypeDualSpiral8x8 = $7;
  WICBitmapDitherTypeErrorDiffusion = $8;

  WICBitmapUseAlpha = 0;
  WICBitmapUsePremultipliedAlpha = $1;
  WICBitmapIgnoreAlpha = $2;

  WICBitmapTransformRotate0 = 0;
  WICBitmapTransformRotate90 = $1;
  WICBitmapTransformRotate180 = $2;
  WICBitmapTransformRotate270 = $3;
  WICBitmapTransformFlipHorizontal = $8;
  WICBitmapTransformFlipVertical = $10;

  WICBitmapLockRead = $1;
  WICBitmapLockWrite = $2;

  WICBitmapDecoderCapabilitySameEncoder = $1;
  WICBitmapDecoderCapabilityCanDecodeAllImages = $2;
  WICBitmapDecoderCapabilityCanDecodeSomeImages = $4;
  WICBitmapDecoderCapabilityCanEnumerateMetadata = $8;
  WICBitmapDecoderCapabilityCanDecodeThumbnail = $10;

  WICProgressOperationCopyPixels = $1;
  WICProgressOperationWritePixels = $2;
  WICProgressOperationAll = $FFFF;

  WICProgressNotificationBegin = $10000;
  WICProgressNotificationEnd = $20000;
  WICProgressNotificationFrequent = $40000;
  WICProgressNotificationAll = $FFFF0000;

  WICComponentSigned = $1;
  WICComponentUnsigned = $2;
  WICComponentSafe = $4;
  WICComponentDisabled = $80000000;

  WICGifLogicalScreenSignature = $1;
  WICGifLogicalScreenDescriptorWidth = $2;
  WICGifLogicalScreenDescriptorHeight = $3;
  WICGifLogicalScreenDescriptorGlobalColorTableFlag = $4;
  WICGifLogicalScreenDescriptorColorResolution = $5;
  WICGifLogicalScreenDescriptorSortFlag = $6;
  WICGifLogicalScreenDescriptorGlobalColorTableSize = $7;
  WICGifLogicalScreenDescriptorBackgroundColorIndex = $8;
  WICGifLogicalScreenDescriptorPixelAspectRatio = $9;

  WICGifImageDescriptorLeft = $1;
  WICGifImageDescriptorTop = $2;
  WICGifImageDescriptorWidth = $3;
  WICGifImageDescriptorHeight = $4;
  WICGifImageDescriptorLocalColorTableFlag = $5;
  WICGifImageDescriptorInterlaceFlag = $6;
  WICGifImageDescriptorSortFlag = $7;
  WICGifImageDescriptorLocalColorTableSize = $8;

  WICGifGraphicControlExtensionDisposal = $1;
  WICGifGraphicControlExtensionUserInputFlag = $2;
  WICGifGraphicControlExtensionTransparencyFlag = $3;
  WICGifGraphicControlExtensionDelay = $4;
  WICGifGraphicControlExtensionTransparentColorIndex = $5;

  WICGifApplicationExtensionApplication = $1;
  WICGifApplicationExtensionData = $2;

  WICGifCommentExtensionText = $1;

  WICJpegCommentText = $1;

  WICJpegLuminanceTable = $1;

  WICJpegChrominanceTable = $1;

  WIC8BIMIptcPString = 0;
  WIC8BIMIptcEmbeddedIPTC = $1;

  WIC8BIMResolutionInfoPString = $1;
  WIC8BIMResolutionInfoHResolution = $2;
  WIC8BIMResolutionInfoHResolutionUnit = $3;
  WIC8BIMResolutionInfoWidthUnit = $4;
  WIC8BIMResolutionInfoVResolution = $5;
  WIC8BIMResolutionInfoVResolutionUnit = $6;
  WIC8BIMResolutionInfoHeightUnit = $7;

  WIC8BIMIptcDigestPString = $1;
  WIC8BIMIptcDigestIptcDigest = $2;

  WICPngGamaGamma = $1;

  WICPngBkgdBackgroundColor = $1;

  WICPngItxtKeyword = $1;
  WICPngItxtCompressionFlag = $2;
  WICPngItxtLanguageTag = $3;
  WICPngItxtTranslatedKeyword = $4;
  WICPngItxtText = $5;

  WICPngChrmWhitePointX = $1;
  WICPngChrmWhitePointY = $2;
  WICPngChrmRedX = $3;
  WICPngChrmRedY = $4;
  WICPngChrmGreenX = $5;
  WICPngChrmGreenY = $6;
  WICPngChrmBlueX = $7;
  WICPngChrmBlueY = $8;

  WICPngHistFrequencies = $1;

  WICPngIccpProfileName = $1;
  WICPngIccpProfileData = $2;

  WICPngSrgbRenderingIntent = $1;

  WICPngTimeYear = $1;
  WICPngTimeMonth = $2;
  WICPngTimeDay = $3;
  WICPngTimeHour = $4;
  WICPngTimeMinute = $5;
  WICPngTimeSecond = $6;

  WICSectionAccessLevelRead = $1;
  WICSectionAccessLevelReadWrite = $3;

  WICPixelFormatNumericRepresentationUnspecified = 0;
  WICPixelFormatNumericRepresentationIndexed = $1;
  WICPixelFormatNumericRepresentationUnsignedInteger = $2;
  WICPixelFormatNumericRepresentationSignedInteger = $3;
  WICPixelFormatNumericRepresentationFixed = $4;
  WICPixelFormatNumericRepresentationFloat = $5;

  WICTiffCompressionDontCare = 0;
  WICTiffCompressionNone = $1;
  WICTiffCompressionCCITT3 = $2;
  WICTiffCompressionCCITT4 = $3;
  WICTiffCompressionLZW = $4;
  WICTiffCompressionRLE = $5;
  WICTiffCompressionZIP = $6;
  WICTiffCompressionLZWHDifferencing = $7;

  WICJpegYCrCbSubsamplingDefault = 0;
  WICJpegYCrCbSubsampling420 = $1;
  WICJpegYCrCbSubsampling422 = $2;
  WICJpegYCrCbSubsampling444 = $3;

  WICPngFilterUnspecified = 0;
  WICPngFilterNone = $1;
  WICPngFilterSub = $2;
  WICPngFilterUp = $3;
  WICPngFilterAverage = $4;
  WICPngFilterPaeth = $5;
  WICPngFilterAdaptive = $6;

  WICWhitePointDefault = $1;
  WICWhitePointDaylight = $2;
  WICWhitePointCloudy = $4;
  WICWhitePointShade = $8;
  WICWhitePointTungsten = $10;
  WICWhitePointFluorescent = $20;
  WICWhitePointFlash = $40;
  WICWhitePointUnderwater = $80;
  WICWhitePointCustom = $100;
  WICWhitePointAutoWhiteBalance = $200;
  WICWhitePointAsShot = WICWhitePointDefault;

  WICRawCapabilityNotSupported = 0;
  WICRawCapabilityGetSupported = $1;
  WICRawCapabilityFullySupported = $2;
  WICRAWCAPABILITIES_FORCE_DWORD = $7FFFFFFF;

  WICRawRotationCapabilityNotSupported = 0;
  WICRawRotationCapabilityGetSupported = $1;
  WICRawRotationCapabilityNinetyDegreesSupported = $2;
  WICRawRotationCapabilityFullySupported = $3;

  WICAsShotParameterSet = $1;
  WICUserAdjustedParameterSet = $2;
  WICAutoAdjustedParameterSet = $3;

  WICRawRenderModeDraft = $1;
  WICRawRenderModeNormal = $2;
  WICRawRenderModeBestQuality = $3;

  CATID_WICBitmapDecoders: TGuid = '{7ED96837-96F0-4812-B211-F13C24117ED3}';
  CATID_WICBitmapEncoders: TGuid = '{AC757296-3522-4E11-9862-C17BE5A1767E}';
  CATID_WICFormatConverters: TGuid = '{7835EAE8-BF14-49D1-93CE-533A407B2248}';
  CATID_WICMetadataReader: TGuid = '{05AF94D8-7174-4CD2-BE4A-4124B80EE4B8}';
  CATID_WICMetadataWriter: TGuid = '{ABE3B9A4-257D-4B97-BD1A-294AF496222E}';
  CATID_WICPixelFormats: TGuid = '{2B46E70F-CDA7-473E-89F6-DC9630A2390B}';

  CLSID_WICBmpDecoder: TGuid = '{6B462062-7CBF-400D-9FDB-813DD10F2778}';
  CLSID_WICBmpEncoder: TGuid = '{69BE8BB4-D66D-47C8-865A-ED1589433782}';
  CLSID_WICDefaultFormatConverter: TGuid = '{1A3F11DC-B514-4B17-8C5F-2154513852F1}';
  CLSID_WICFormatConverterHighColor: TGuid = '{AC75D454-9F37-48F8-B972-4E19BC856011}';
  CLSID_WICFormatConverterNChannel: TGuid = '{C17CABB2-D4A3-47D7-A557-339B2EFBD4F1}';
  CLSID_WICFormatConverterWMPhoto: TGuid = '{9CB5172B-D600-46BA-AB77-77BB7E3A00D9}';
  CLSID_WICGifDecoder: TGuid = '{381DDA3C-9CE9-4834-A23E-1F98F8FC52BE}';
  CLSID_WICGifEncoder: TGuid = '{114F5598-0B22-40A0-86A1-C83EA495ADBD}';
  CLSID_WICIcoDecoder: TGuid = '{C61BFCDF-2E0F-4AAD-A8D7-E06BAFEBCDFE}';
  CLSID_WICImagingCategories: TGuid = '{FAE3D380-FEA4-4623-8C75-C6B61110B681}';
  CLSID_WICImagingFactory: TGuid = '{CACAF262-9370-4615-A13B-9F5539DA4C0A}';
  CLSID_WICImagingFactory1: TGuid = '{CACAF262-9370-4615-A13B-9F5539DA4C0A}';
  CLSID_WICImagingFactory2: TGuid = '{317D06E8-5F24-433D-BDF7-79CE68D8ABC2}';
  CLSID_WICJpegDecoder: TGuid = '{9456A480-E88B-43EA-9E73-0B2D9B71B1CA}';
  CLSID_WICJpegEncoder: TGuid = '{1A34F5C1-4A5A-46DC-B644-1F4567E7A676}';
  CLSID_WICPngDecoder: TGuid = '{389EA17B-5078-4CDE-B6EF-25C15175C751}';
  CLSID_WICPngDecoder1: TGuid = '{389EA17B-5078-4CDE-B6EF-25C15175C751}';
  CLSID_WICPngDecoder2: TGuid = '{E018945B-AA86-4008-9BD4-6777A1E40C11}';
  CLSID_WICPngEncoder: TGuid = '{27949969-876A-41D7-9447-568F6A35A4DC}';
  CLSID_WICTiffDecoder: TGuid = '{B54E85D9-FE23-499F-8B88-6ACEA713752B}';
  CLSID_WICTiffEncoder: TGuid = '{0131BE10-2001-4C5F-A9B0-CC88FAB64CE8}';
  CLSID_WICWmpDecoder: TGuid = '{A26CEC36-234C-4950-AE16-E34AACE71D0D}';
  CLSID_WICWmpEncoder: TGuid = '{AC4CE3CB-E1C1-44CD-8215-5A1665509EC2}';

  GUID_ContainerFormatBmp: TGuid = '{0AF1D87E-FCFE-4188-BDEB-A7906471CBE3}';
  GUID_ContainerFormatGif: TGuid = '{1F8A5601-7D4D-4CBD-9C82-1BC8D4EEB9A5}';
  GUID_ContainerFormatIco: TGuid = '{A3A860C4-338F-4C17-919A-FBA4B5628F21}';
  GUID_ContainerFormatJpeg: TGuid = '{19E4A5AA-5662-4FC5-A0C0-1758028E1057}';
  GUID_ContainerFormatPng: TGuid = '{1B7CFAF4-713F-473C-BBCD-6137425FAEAF}';
  GUID_ContainerFormatTiff: TGuid = '{163BCC30-E2E9-4F0B-961D-A3E9FDB788A3}';
  GUID_ContainerFormatWmp: TGuid = '{57A37CAA-367A-4540-916B-F183C5093A4B}';
  GUID_VendorMicrosoft: TGuid = '{F0E749CA-EDEF-4589-A73A-EE0E626A2A2B}';
  GUID_VendorMicrosoftBuiltIn: TGuid = '{257A30FD-06B6-462B-AEA4-63F70B86E533}';

  SID_WICPixelFormatDontCare = '{6FDDC324-4E03-4BFE-B185-3D77768DC900}';

  GUID_WICPixelFormatDontCare: TGuid = SID_WICPixelFormatDontCare;
  GUID_WICPixelFormat1bppIndexed: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC901}';
  GUID_WICPixelFormat2bppIndexed: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC902}';
  GUID_WICPixelFormat4bppIndexed: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC903}';
  GUID_WICPixelFormat8bppIndexed: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC904}';
  GUID_WICPixelFormatBlackWhite: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC905}';
  GUID_WICPixelFormat2bppGray: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC906}';
  GUID_WICPixelFormat4bppGray: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC907}';
  GUID_WICPixelFormat8bppGray: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC908}';
  GUID_WICPixelFormat8bppAlpha: TGuid = '{E6CD0116-EEBA-4161-AA85-27DD9FB3A895}';
  GUID_WICPixelFormat16bppBGR555: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC909}';
  GUID_WICPixelFormat16bppBGR565: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC90A}';
  GUID_WICPixelFormat16bppBGRA5551: TGuid = '{05EC7C2B-F1E6-4961-AD46-E1CC810A87D2}';
  GUID_WICPixelFormat16bppGray: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC90B}';
  GUID_WICPixelFormat24bppBGR: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC90C}';
  GUID_WICPixelFormat24bppRGB: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC90D}';
  GUID_WICPixelFormat32bppBGR: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC90E}';
  GUID_WICPixelFormat32bppBGRA: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC90F}';
  GUID_WICPixelFormat32bppPBGRA: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC910}';
  GUID_WICPixelFormat32bppGrayFloat: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC911}';
  GUID_WICPixelFormat32bppRGBA: TGuid = '{F5C7AD2D-6A8D-43DD-A7A8-A29935261AE9}';
  GUID_WICPixelFormat32bppPRGBA: TGuid = '{3CC4A650-A527-4D37-A916-3142C7EBEDBA}';
  GUID_WICPixelFormat48bppRGB: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC915}';
  GUID_WICPixelFormat48bppBGR: TGuid = '{E605A384-B468-46CE-BB2E-36F180E64313}';
  GUID_WICPixelFormat64bppRGBA: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC916}';
  GUID_WICPixelFormat64bppBGRA: TGuid = '{1562FF7C-D352-46F9-979E-42976B792246}';
  GUID_WICPixelFormat64bppPRGBA: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC917}';
  GUID_WICPixelFormat64bppPBGRA: TGuid = '{8C518E8E-A4EC-468B-AE70-C9A35A9C5530}';
  GUID_WICPixelFormat16bppGrayFixedPoint: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC913}';
  GUID_WICPixelFormat32bppBGR101010: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC914}';
  GUID_WICPixelFormat48bppRGBFixedPoint: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC912}';
  GUID_WICPixelFormat48bppBGRFixedPoint: TGuid = '{49CA140E-CAB6-493B-9DDF-60187C37532A}';
  GUID_WICPixelFormat96bppRGBFixedPoint: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC918}';
  GUID_WICPixelFormat128bppRGBAFloat: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC919}';
  GUID_WICPixelFormat128bppPRGBAFloat: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC91A}';
  GUID_WICPixelFormat128bppRGBFloat: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC91B}';
  GUID_WICPixelFormat32bppCMYK: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC91C}';
  GUID_WICPixelFormat64bppRGBAFixedPoint: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC91D}';
  GUID_WICPixelFormat64bppBGRAFixedPoint: TGuid = '{356DE33C-54D2-4A23-BB04-9B7BF9B1D42D}';
  GUID_WICPixelFormat64bppRGBFixedPoint: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC940}';
  GUID_WICPixelFormat128bppRGBAFixedPoint: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC91E}';
  GUID_WICPixelFormat128bppRGBFixedPoint: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC941}';
  GUID_WICPixelFormat64bppRGBAHalf: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC93A}';
  GUID_WICPixelFormat64bppRGBHalf: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC942}';
  GUID_WICPixelFormat48bppRGBHalf: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC93B}';
  GUID_WICPixelFormat32bppRGBE: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC93D}';
  GUID_WICPixelFormat16bppGrayHalf: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC93E}';
  GUID_WICPixelFormat32bppGrayFixedPoint: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC93F}';
  GUID_WICPixelFormat32bppRGBA1010102: TGuid = '{25238D72-FCF9-4522-B514-5578E5AD55E0}';
  GUID_WICPixelFormat32bppRGBA1010102XR: TGuid = '{00DE6B9A-C101-434B-B502-D0165EE1122C}';
  GUID_WICPixelFormat64bppCMYK: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC91F}';
  GUID_WICPixelFormat24bpp3Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC920}';
  GUID_WICPixelFormat32bpp4Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC921}';
  GUID_WICPixelFormat40bpp5Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC922}';
  GUID_WICPixelFormat48bpp6Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC923}';
  GUID_WICPixelFormat56bpp7Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC924}';
  GUID_WICPixelFormat64bpp8Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC925}';
  GUID_WICPixelFormat48bpp3Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC926}';
  GUID_WICPixelFormat64bpp4Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC927}';
  GUID_WICPixelFormat80bpp5Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC928}';
  GUID_WICPixelFormat96bpp6Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC929}';
  GUID_WICPixelFormat112bpp7Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC92A}';
  GUID_WICPixelFormat128bpp8Channels: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC92B}';
  GUID_WICPixelFormat40bppCMYKAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC92C}';
  GUID_WICPixelFormat80bppCMYKAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC92D}';
  GUID_WICPixelFormat32bpp3ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC92E}';
  GUID_WICPixelFormat40bpp4ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC92F}';
  GUID_WICPixelFormat48bpp5ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC930}';
  GUID_WICPixelFormat56bpp6ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC931}';
  GUID_WICPixelFormat64bpp7ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC932}';
  GUID_WICPixelFormat72bpp8ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC933}';
  GUID_WICPixelFormat64bpp3ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC934}';
  GUID_WICPixelFormat80bpp4ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC935}';
  GUID_WICPixelFormat96bpp5ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC936}';
  GUID_WICPixelFormat112bpp6ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC937}';
  GUID_WICPixelFormat128bpp7ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC938}';
  GUID_WICPixelFormat144bpp8ChannelsAlpha: TGuid = '{6FDDC324-4E03-4BFE-B185-3D77768DC939}';
  GUID_WICPixelFormatUndefined: TGuid = SID_WICPixelFormatDontCare;

  SID_IWICPalette = '{00000040-A8F2-4877-BA0A-FD2B6645FB94}';
  SID_IWICBitmapSource = '{00000120-A8F2-4877-BA0A-FD2B6645FB94}';
  SID_IWICFormatConverter = '{00000301-A8F2-4877-BA0A-FD2B6645FB94}';
  SID_IWICBitmapScaler = '{00000302-A8F2-4877-BA0A-FD2B6645FB94}';
  SID_IWICBitmapClipper = '{E4FBCF03-223D-4E81-9333-D635556DD1B5}';
  SID_IWICBitmapFlipRotator = '{5009834F-2D6A-41CE-9E1B-17C5AFF7A782}';
  SID_IWICBitmapLock = '{00000123-A8F2-4877-BA0A-FD2B6645FB94}';
  SID_IWICBitmap = '{00000121-A8F2-4877-BA0A-FD2B6645FB94}';
  SID_IWICColorContext = '{3C613A02-34B2-44EA-9A7C-45AEA9C6FD6D}';
  SID_IWICColorTransform = '{B66F034F-D0E2-40AB-B436-6DE39E321A94}';
  SID_IWICFastMetadataEncoder = '{B84E2C09-78C9-4AC4-8BD3-524AE1663A2F}';
  SID_IWICStream = '{135FF860-22B7-4DDF-B0F6-218F4F299A43}';
  SID_IWICEnumMetadataItem = '{DC2BB46D-3F07-481E-8625-220C4AEDBB33}';
  SID_IWICMetadataQueryReader = '{30989668-E1C9-4597-B395-458EEDB808DF}';
  SID_IWICMetadataQueryWriter = '{A721791A-0DEF-4D06-BD91-2118BF1DB10B}';
  SID_IWICBitmapEncoder = '{00000103-A8F2-4877-BA0A-FD2B6645FB94}';
  SID_IWICBitmapFrameEncode = '{00000105-A8F2-4877-BA0A-FD2B6645FB94}';
  SID_IWICBitmapDecoder = '{9EDDE9E7-8DEE-47EA-99DF-E6FAF2ED44BF}';
  SID_IWICBitmapSourceTransform = '{3B16811B-6A43-4EC9-B713-3D5A0C13B940}';
  SID_IWICBitmapFrameDecode = '{3B16811B-6A43-4EC9-A813-3D930C13B940}';
  SID_IWICProgressiveLevelControl = '{DAAC296F-7AA5-4DBF-8D15-225C5976F891}';
  SID_IWICProgressCallback = '{4776F9CD-9517-45FA-BF24-E89C5EC5C60C}';
  SID_IWICBitmapCodecProgressNotification = '{64C1024E-C3CF-4462-8078-88C2B11C46D9}';
  SID_IWICComponentInfo = '{23BC3F0A-698B-4357-886B-F24D50671334}';
  SID_IWICFormatConverterInfo = '{9F34FB65-13F4-4F15-BC57-3726B5E53D9F}';
  SID_IWICBitmapCodecInfo = '{E87A44C4-B76E-4C47-8B09-298EB12A2714}';
  SID_IWICBitmapEncoderInfo = '{94C9B4EE-A09F-4F92-8A1E-4A9BCE7E76FB}';
  SID_IWICBitmapDecoderInfo = '{D8CD007F-D08F-4191-9BFC-236EA7F0E4B5}';
  SID_IWICPixelFormatInfo = '{E8EDA601-3D48-431A-AB44-69059BE88BBE}';
  SID_IWICPixelFormatInfo2 = '{A9DB33A2-AF5F-43C7-B679-74F5984B5AA4}';
  SID_IWICImagingFactory = '{EC5EC8A9-C395-4314-9C77-54D7A935FF70}';
  SID_IWICDevelopRawNotificationCallback = '{95C75A6E-3E8C-4EC2-85A8-AEBCC551E59B}';
  SID_IWICDevelopRaw = '{FBEC5E44-F7BE-4B65-B7F8-C0C81FEF026D}';

  IID_IWICPalette: TGuid = SID_IWICPalette;
  IID_IWICBitmapSource: TGuid = SID_IWICBitmapSource;
  IID_IWICFormatConverter: TGuid = SID_IWICFormatConverter;
  IID_IWICBitmapScaler: TGuid = SID_IWICBitmapScaler;
  IID_IWICBitmapClipper: TGuid = SID_IWICBitmapClipper;
  IID_IWICBitmapFlipRotator: TGuid = SID_IWICBitmapFlipRotator;
  IID_IWICBitmapLock: TGuid = SID_IWICBitmapLock;
  IID_IWICBitmap: TGuid = SID_IWICBitmap;
  IID_IWICColorContext: TGuid = SID_IWICColorContext;
  IID_IWICColorTransform: TGuid = SID_IWICColorTransform;
  IID_IWICFastMetadataEncoder: TGuid = SID_IWICFastMetadataEncoder;
  IID_IWICStream: TGuid = SID_IWICStream;
  IID_IWICEnumMetadataItem: TGuid = SID_IWICEnumMetadataItem;
  IID_IWICMetadataQueryReader: TGuid = SID_IWICMetadataQueryReader;
  IID_IWICMetadataQueryWriter: TGuid = SID_IWICMetadataQueryWriter;
  IID_IWICBitmapEncoder: TGuid = SID_IWICBitmapEncoder;
  IID_IWICBitmapFrameEncode: TGuid = SID_IWICBitmapFrameEncode;
  IID_IWICBitmapDecoder: TGuid = SID_IWICBitmapDecoder;
  IID_IWICBitmapSourceTransform: TGuid = SID_IWICBitmapSourceTransform;
  IID_IWICBitmapFrameDecode: TGuid = SID_IWICBitmapFrameDecode;
  IID_IWICProgressiveLevelControl: TGuid = SID_IWICProgressiveLevelControl;
  IID_IWICProgressCallback: TGuid = SID_IWICProgressCallback;
  IID_IWICBitmapCodecProgressNotification: TGuid = SID_IWICBitmapCodecProgressNotification;
  IID_IWICComponentInfo: TGuid = SID_IWICComponentInfo;
  IID_IWICFormatConverterInfo: TGuid = SID_IWICFormatConverterInfo;
  IID_IWICBitmapCodecInfo: TGuid = SID_IWICBitmapCodecInfo;
  IID_IWICBitmapEncoderInfo: TGuid = SID_IWICBitmapEncoderInfo;
  IID_IWICBitmapDecoderInfo: TGuid = SID_IWICBitmapDecoderInfo;
  IID_IWICPixelFormatInfo: TGuid = SID_IWICPixelFormatInfo;
  IID_IWICPixelFormatInfo2: TGuid = SID_IWICPixelFormatInfo2;
  IID_IWICImagingFactory: TGuid = SID_IWICImagingFactory;
  IID_IWICDevelopRawNotificationCallback: TGuid = SID_IWICDevelopRawNotificationCallback;
  IID_IWICDevelopRaw: TGuid = SID_IWICDevelopRaw;

type
  PWICColor = ^WICColor;
  WICColor = LongWord;

  PWICPixelFormatGUID = ^WICPixelFormatGUID;
  WICPixelFormatGUID = TGuid;

  PWICColorContextType = ^WICColorContextType;
  WICColorContextType = LongWord;

  PWICBitmapCreateCacheOption = ^WICBitmapCreateCacheOption;
  WICBitmapCreateCacheOption = LongWord;

  PWICDecodeOptions = ^WICDecodeOptions;
  WICDecodeOptions = LongWord;

  PWICBitmapEncoderCacheOption = ^WICBitmapEncoderCacheOption;
  WICBitmapEncoderCacheOption = LongWord;

  PWICComponentType = ^WICComponentType;
  WICComponentType = LongWord;

  PWICComponentEnumerateOptions = ^WICComponentEnumerateOptions;
  WICComponentEnumerateOptions = LongWord;

  PWICBitmapInterpolationMode = ^WICBitmapInterpolationMode;
  WICBitmapInterpolationMode = LongWord;

  PWICBitmapPaletteType = ^WICBitmapPaletteType;
  WICBitmapPaletteType = LongWord;

  PWICBitmapDitherType = ^WICBitmapDitherType;
  WICBitmapDitherType = LongWord;

  PWICBitmapAlphaChannelOption = ^WICBitmapAlphaChannelOption;
  WICBitmapAlphaChannelOption = LongWord;

  PWICBitmapTransformOptions = ^WICBitmapTransformOptions;
  WICBitmapTransformOptions = LongWord;

  PWICProgressOperation = ^WICProgressOperation;
  WICProgressOperation = LongWord;

  PWICSectionAccessLevel = ^WICSectionAccessLevel;
  WICSectionAccessLevel = LongWord;

  PWICPixelFormatNumericRepresentation = ^WICPixelFormatNumericRepresentation;
  WICPixelFormatNumericRepresentation = LongWord;

  PWICNamedWhitePoint = ^WICNamedWhitePoint;
  WICNamedWhitePoint = LongWord;

  PWICRawCapabilities = ^WICRawCapabilities;
  WICRawCapabilities = LongWord;

  PWICRawRotationCapabilities = ^WICRawRotationCapabilities;
  WICRawRotationCapabilities = LongWord;

  PWICRawParameterSet = ^WICRawParameterSet;
  WICRawParameterSet = LongWord;

  PWICRawRenderMode = ^WICRawRenderMode;
  WICRawRenderMode = LongWord;

  PWICRect = ^WICRect;
  WICRect = record
    X: LongInt;
    Y: LongInt;
    Width: LongInt;
    Height: LongInt;
  end;

  PWICBitmapPattern = ^WICBitmapPattern;
  WICBitmapPattern = record
    Position: ULARGE_INTEGER;
    Length: LongWord;
    Pattern: PByte;
    Mask: PByte;
    EndOfStream: LongBool;
  end;

  PWICRawCapabilitiesInfo = ^WICRawCapabilitiesInfo;
  WICRawCapabilitiesInfo = record
    cbSize: LongWord;
    CodecMajorVersion: LongWord;
    CodecMinorVersion: LongWord;
    ExposureCompensationSupport: WICRawCapabilities;
    ContrastSupport: WICRawCapabilities;
    RGBWhitePointSupport: WICRawCapabilities;
    NamedWhitePointSupport: WICRawCapabilities;
    NamedWhitePointSupportMask: LongWord;
    KelvinWhitePointSupport: WICRawCapabilities;
    GammaSupport: WICRawCapabilities;
    TintSupport: WICRawCapabilities;
    SaturationSupport: WICRawCapabilities;
    SharpnessSupport: WICRawCapabilities;
    NoiseReductionSupport: WICRawCapabilities;
    DestinationColorProfileSupport: WICRawCapabilities;
    ToneCurveSupport: WICRawCapabilities;
    RotationSupport: WICRawRotationCapabilities;
    RenderModeSupport: WICRawCapabilities;
  end;

  PWICRawToneCurvePoint = ^WICRawToneCurvePoint;
  WICRawToneCurvePoint = record
    Input: Double;
    Output: Double;
  end;

  PWICRawToneCurve = ^WICRawToneCurve;
  WICRawToneCurve = record
    PointCount: LongWord;
    Points: array[0..0] of WICRawToneCurvePoint;
  end;

  PFNProgressNotification = ^FNProgressNotification;
  FNProgressNotification = function(Data: Pointer; FrameNum: LongWord; Operation: WICProgressOperation;
    Progress: Double): HResult; stdcall;

  PPropBag2 = ^TPropBag2;
  TPropBag2 = record
    dwType: LongWord;
    vt: Word;
    cfType: Word;
    dwHint: LongWord;
    pstrName: POleStr;
    clsid: TCLSID;
  end;

  IWICBitmapSource = interface;
  IWICMetadataQueryWriter = interface;
  IWICBitmapFrameEncode = interface;
  IWICBitmapFrameDecode = interface;
  IWICBitmapEncoderInfo = interface;
  IWICBitmapDecoderInfo = interface;

  PIPropertyBag2 = ^IPropertyBag2;
  IPropertyBag2 = interface(IUnknown)
    ['{22F55882-280B-11d0-A8A9-00A0C90C2004}']
    function Read(PropBag: PPropBag2; ErrLog: IErrorLog; Value: PVariant; Error: PHResult): HResult; stdcall;
    function Write(Properties: LongWord; PropBag: PPropBag2; Value: PVariant): HResult; stdcall;
    function CountProperties(var Properties: LongWord): HResult; stdcall;
    function GetPropertyInfo(iProperty, cProperties: LongWord; PropBag: PPropBag2;
      var pcProperties: LongWord): HResult; stdcall;
    function LoadObject(Name: POleStr; Hint: DWORD; UnkObject: IUnknown; ErrLog: IErrorLog): HResult; stdcall;
  end;

  IWICPalette = interface(IUnknown)
    [SID_IWICPalette]
    function InitializePredefined(PaletteType: WICBitmapPaletteType; AddTransparentColor: LongBool): HResult; stdcall;
    function InitializeCustom(Colors: PWICColor; Count: LongWord): HResult; stdcall;
    function InitializeFromBitmap(Surface: IWICBitmapSource; Count: LongWord;
      AddTransparentColor: LongBool): HResult; stdcall;
    function InitializeFromPalette(Palette: IWICPalette): HResult; stdcall;
    function GetType(out PaletteType: WICBitmapPaletteType): HResult; stdcall;
    function GetColorCount(out Count: LongWord): HResult; stdcall;
    function GetColors(Count: LongWord; Colors: PWICColor; out ActualColors: LongWord): HResult; stdcall;
    function IsBlackWhite(out IsBlackWhite: LongBool): HResult; stdcall;
    function IsGrayscale(out IsGrayscale: LongBool): HResult; stdcall;
    function HasAlpha(out HasAlpha: LongBool): HResult; stdcall;
  end;

  IWICBitmapSource = interface(IUnknown)
    [SID_IWICBitmapSource]
    function GetSize(out Width, Height: LongWord): HResult; stdcall;
    function GetPixelFormat(out PixelFormat: WICPixelFormatGUID): HResult; stdcall;
    function GetResolution(out DpiX, DpiY: Double): HResult; stdcall;
    function CopyPalette(Palette: IWICPalette): HResult; stdcall;
    function CopyPixels(Rect: PWICRect; Stride, BufferSize: LongWord; Buffer: Pointer): HResult; stdcall;
  end;

  IWICFormatConverter = interface(IWICBitmapSource)
    [SID_IWICFormatConverter]
    function Initialize(Source: IWICBitmapSource; const DestFormat: WICPixelFormatGUID;
      Dither: WICBitmapDitherType; Palette: IWICPalette; AlphaThresholdPercent: Double;
      PaletteTranslate: WICBitmapPaletteType): HResult; stdcall;
    function CanConvert(const SourcePixelFormat, DestPixelFormat: WICPixelFormatGUID;
      out CanConvert: LongBool): HResult; stdcall;
  end;

  IWICBitmapScaler = interface(IWICBitmapSource)
    [SID_IWICBitmapScaler]
    function Initialize(Source: IWICBitmapSource; Width, Height: LongWord;
      Mode: WICBitmapInterpolationMode): HResult; stdcall;
  end;

  IWICBitmapClipper = interface(IWICBitmapSource)
    [SID_IWICBitmapClipper]
    function Initialize(Source: IWICBitmapSource; const Rect: WICRect): HResult; stdcall;
  end;

  IWICBitmapFlipRotator = interface(IWICBitmapSource)
    [SID_IWICBitmapFlipRotator]
    function Initialize(Source: IWICBitmapSource; Options: WICBitmapTransformOptions): HResult; stdcall;
  end;

  IWICBitmapLock = interface(IUnknown)
    [SID_IWICBitmapLock]
    function GetSize(out Width, Height: LongWord): HResult; stdcall;
    function GetStride(out Stride: LongWord): HResult; stdcall;
    function GetDataPointer(out DataSize: LongWord; var Data: Pointer): HResult; stdcall;
    function GetPixelFormat(out PixelFormat: WICPixelFormatGUID): HResult; stdcall;
  end;

  IWICBitmap = interface(IWICBitmapSource)
    [SID_IWICBitmap]
    function Lock(const RectLock: WICRect; Flags: LongWord; out Lock: IWICBitmapLock): HResult; stdcall;
    function SetPalette(Palette: IWICPalette): HResult; stdcall;
    function SetResolution(DpiX, DpiY: Double): HResult; stdcall;
  end;

  PIWICColorContext = ^IWICColorContext;
  IWICColorContext = interface(IUnknown)
    [SID_IWICColorContext]
    function InitializeFromFilename(FileName: PWideChar): HResult; stdcall;
    function InitializeFromMemory(Buffer: Pointer; BufferSize: LongWord): HResult; stdcall;
    function InitializeFromExifColorSpace(Value: LongWord): HResult; stdcall;
    function GetType(out &Type: WICColorContextType): HResult; stdcall;
    function GetProfileBytes(BufferSize: LongWord; Buffer: Pointer; out ActualSize: LongWord): HResult; stdcall;
    function GetExifColorSpace(out Value: LongWord): HResult; stdcall;
  end;

  IWICColorTransform = interface(IWICBitmapSource)
    [SID_IWICColorTransform]
    function Initialize(BitmapSource: IWICBitmapSource; ContextSource, ContextDest: IWICColorContext;
      const PixelFormatDest: WICPixelFormatGUID): HResult; stdcall;
  end;

  IWICFastMetadataEncoder = interface(IUnknown)
    [SID_IWICFastMetadataEncoder]
    function Commit: HResult; stdcall;
    function GetMetadataQueryWriter(out MetadataQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;

  IWICStream = interface(IStream)
    [SID_IWICStream]
    function InitializeFromIStream(Stream: IStream): HResult; stdcall;
    function InitializeFromFilename(FileName: PWideChar; DesiredAccess: LongWord): HResult; stdcall;
    function InitializeFromMemory(Buffer: Pointer; BufferSize: LongWord): HResult; stdcall;
    function InitializeFromIStreamRegion(Stream: IStream; Offset, MaxSize: UInt64): HResult; stdcall;
  end;

  IWICEnumMetadataItem = interface(IUnknown)
    [SID_IWICEnumMetadataItem]
    function Next(RetrieveCount: LongWord; Schema: PPROPVARIANT; Items, Values: PPROPVARIANT;
      out FetchedCount: LongWord): HResult; stdcall;
    function Skip(SkipCount: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out EnumMetadataItem: IWICEnumMetadataItem): HResult; stdcall;
  end;

  IWICMetadataQueryReader = interface(IUnknown)
    [SID_IWICMetadataQueryReader]
    function GetContainerFormat(out ContainerFormat: TGuid): HResult; stdcall;
    function GetLocation(MaxLength: LongWord; Namespace: PWideChar; out ActualLength: LongWord): HResult; stdcall;
    function GetMetadataByName(Name: PWideChar; Value: PPROPVARIANT): HResult; stdcall;
    function GetEnumerator(out EnumString: IEnumString): HResult; stdcall;
  end;

  IWICMetadataQueryWriter = interface(IWICMetadataQueryReader)
    [SID_IWICMetadataQueryWriter]
    function SetMetadataByName(Name: PWideChar; const Value: TPropVariant): HResult; stdcall;
    function RemoveMetadataByName(Name: PWideChar): HResult; stdcall;
  end;

  IWICBitmapEncoder = interface(IUnknown)
    [SID_IWICBitmapEncoder]
    function Initialize(Stream: IStream; CacheOption: WICBitmapEncoderCacheOption): HResult; stdcall;
    function GetContainerFormat(out ContainerFormat: TGuid): HResult; stdcall;
    function GetEncoderInfo(out EncoderInfo: IWICBitmapEncoderInfo): HResult; stdcall;
    function SetColorContexts(Count: LongWord; ColorContexts: PIWICColorContext): HResult; stdcall;
    function SetPalette(Palette: IWICPalette): HResult; stdcall;
    function SetThumbnail(Thumbnail: IWICBitmapSource): HResult; stdcall;
    function SetPreview(Preview: IWICBitmapSource): HResult; stdcall;
    function CreateNewFrame(out FrameEncode: IWICBitmapFrameEncode;
      out EncoderOptions: IPropertyBag2): HResult; stdcall;
    function Commit: HResult; stdcall;
    function GetMetadataQueryWriter(out MetadataQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;

  IWICBitmapFrameEncode = interface(IUnknown)
    [SID_IWICBitmapFrameEncode]
    function Initialize(EncoderOptions: IPropertyBag2): HResult; stdcall;
    function SetSize(Width, Height: LongWord): HResult; stdcall;
    function SetResolution(DpiX, DpiY: Double): HResult; stdcall;
    function SetPixelFormat(var PixelFormat: WICPixelFormatGUID): HResult; stdcall;
    function SetColorContexts(Count: LongWord; ColorContext: PIWICColorContext): HResult; stdcall;
    function SetPalette(Palette: IWICPalette): HResult; stdcall;
    function SetThumbnail(Thumbnail: IWICBitmapSource): HResult; stdcall;
    function WritePixels(LineCount, Stride, BufferSize: LongWord; Pixels: Pointer): HResult; stdcall;
    function WriteSource(BitmapSource: IWICBitmapSource; Rect: PWICRect): HResult; stdcall;
    function Commit: HResult; stdcall;
    function GetMetadataQueryWriter(out MetadataQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;

  IWICBitmapDecoder = interface(IUnknown)
    [SID_IWICBitmapDecoder]
    function QueryCapability(Stream: IStream; out Capability: LongWord): HResult; stdcall;
    function Initialize(Stream: IStream; CacheOptions: WICDecodeOptions): HResult; stdcall;
    function GetContainerFormat(out ContainerFormat: TGuid): HResult; stdcall;
    function GetDecoderInfo(out DecoderInfo: IWICBitmapDecoderInfo): HResult; stdcall;
    function CopyPalette(Palette: IWICPalette): HResult; stdcall;
    function GetMetadataQueryReader(out MetadataQueryReader: IWICMetadataQueryReader): HResult; stdcall;
    function GetPreview(out BitmapSource: IWICBitmapSource): HResult; stdcall;
    function GetColorContexts(Count: LongWord; ColorContexts: PIWICColorContext;
      out ActualCount: LongWord): HResult; stdcall;
    function GetThumbnail(out Thumbnail: IWICBitmapSource): HResult; stdcall;
    function GetFrameCount(out Count: LongWord): HResult; stdcall;
    function GetFrame(Index: LongWord; out BitmapFrame: IWICBitmapFrameDecode): HResult; stdcall;
  end;

  IWICBitmapSourceTransform = interface(IUnknown)
    [SID_IWICBitmapSourceTransform]
    function CopyPixels(const Rect: WICRect; Width, Height: LongWord; const DestFormat: WICPixelFormatGUID;
      Transform: WICBitmapTransformOptions; Stride, BufferSize: LongWord; Buffer: Pointer): HResult; stdcall;
    function GetClosestSize(var Width, Height: LongWord): HResult; stdcall;
    function GetClosestPixelFormat(var DestFormat: WICPixelFormatGUID): HResult; stdcall;
    function DoesSupportTransform(Transform: WICBitmapTransformOptions;
      out IsSupported: LongBool): HResult; stdcall;
  end;

  IWICBitmapFrameDecode = interface(IWICBitmapSource)
    [SID_IWICBitmapFrameDecode]
    function GetMetadataQueryReader(out MetadataQueryReader: IWICMetadataQueryReader): HResult; stdcall;
    function GetColorContexts(Count: LongWord; ColorContexts: PIWICColorContext;
      out ActualCount: LongWord): HResult; stdcall;
    function GetThumbnail(out Thumbnail: IWICBitmapSource): HResult; stdcall;
  end;

  IWICProgressiveLevelControl = interface(IUnknown)
    [SID_IWICProgressiveLevelControl]
    function GetLevelCount(out Levels: LongWord): HResult; stdcall;
    function GetCurrentLevel(out Level: LongWord): HResult; stdcall;
    function SetCurrentLevel(Level: LongWord): HResult; stdcall;
  end;

  IWICProgressCallback = interface(IUnknown)
    [SID_IWICProgressCallback]
    function Notify(FrameNum: LongWord; Operation: WICProgressOperation; Progress: Double): HResult; stdcall;
  end;

  IWICBitmapCodecProgressNotification = interface(IUnknown)
    [SID_IWICBitmapCodecProgressNotification]
    function RegisterProgressNotification(ProgressNotification: PFNProgressNotification; Data: Pointer;
      ProgressFlags: LongWord): HResult; stdcall;
  end;

  IWICComponentInfo = interface(IUnknown)
    [SID_IWICComponentInfo]
    function GetComponentType(out &Type: WICComponentType): HResult; stdcall;
    function GetCLSID(out CLSID: TGuid): HResult; stdcall;
    function GetSigningStatus(out Status: LongWord): HResult; stdcall;
    function GetAuthor(AuthorBufferSize: LongWord; Author: PWideChar;
      out ActualBufferSize: LongWord): HResult; stdcall;
    function GetVendorGUID(out Vendor: TGuid): HResult; stdcall;
    function GetVersion(VersionBufferSize: LongWord; Version: PWideChar;
      out ActualBufferSize: LongWord): HResult; stdcall;
    function GetSpecVersion(SpecVersionBufferSize: LongWord; SpecVersion: PWideChar;
      out ActualBufferSize: LongWord): HResult; stdcall;
    function GetFriendlyName(FriendlyNameBufferSize: LongWord; FriendlyName: PWideChar;
      out ActualBufferSize: LongWord): HResult; stdcall;
  end;

  IWICFormatConverterInfo = interface(IWICComponentInfo)
    [SID_IWICFormatConverterInfo]
    function GetPixelFormats(FormatCount: LongWord; PixelFormatGUIDs: PWICPixelFormatGUID;
      out ActualCount: LongWord): HResult; stdcall;
    function CreateInstance(out Converter: IWICFormatConverter): HResult; stdcall;
  end;

  IWICBitmapCodecInfo = interface(IWICComponentInfo)
    [SID_IWICBitmapCodecInfo]
    function GetContainerFormat(out ContainerFormat: TGuid): HResult; stdcall;
    function GetPixelFormats(FormatsCount: LongWord; PixelFormats: PGuid; out ActualCount: LongWord): HResult; stdcall;
    function GetColorManagementVersion(ColorManagementVersionBufferSize: LongWord; ColorManagementVersion: PWideChar;
      out ActualBufferSize: LongWord): HResult; stdcall;
    function GetDeviceManufacturer(DeviceManufacturerBufferSize: LongWord; DeviceManufacturer: PWideChar;
      out ActualBufferSize: LongWord): HResult; stdcall;
    function GetDeviceModels(DeviceModelsBufferSize: LongWord; DeviceModels: PWideChar;
      out ActualBufferSize: LongWord): HResult; stdcall;
    function GetMimeTypes(MimeTypesBufferSize: LongWord; MimeTypes: PWideChar;
      out ActualBufferSize: LongWord): HResult; stdcall;
    function GetFileExtensions(FileExtensionsBufferSize: LongWord; FileExtensions: PWideChar;
      out ActualBufferSize: LongWord): HResult; stdcall;
    function DoesSupportAnimation(out SupportAnimation: LongBool): HResult; stdcall;
    function DoesSupportChromakey(out SupportChromakey: LongBool): HResult; stdcall;
    function DoesSupportLossless(out SupportLossless: LongBool): HResult; stdcall;
    function DoesSupportMultiframe(out SupportMultiframe: LongBool): HResult; stdcall;
    function MatchesMimeType(MimeType: PWideChar; out Matches: LongBool): HResult; stdcall;
  end;

  IWICBitmapEncoderInfo = interface(IWICBitmapCodecInfo)
    [SID_IWICBitmapEncoderInfo]
    function CreateInstance(out BitmapEncoder: IWICBitmapEncoder): HResult; stdcall;
  end;

  IWICBitmapDecoderInfo = interface(IWICBitmapCodecInfo)
    [SID_IWICBitmapDecoderInfo]
    function GetPatterns(PatternsCount: LongWord; Patterns: PWICBitmapPattern; SupportedPatternCount,
      SupportedPatternBufferSize: PLongWord): HResult; stdcall;
    function MatchesPattern(Stream: IStream; out Matches: LongBool): HResult; stdcall;
    function CreateInstance(out BitmapDecoder: IWICBitmapDecoder): HResult; stdcall;
  end;

  IWICPixelFormatInfo = interface(IWICComponentInfo)
    [SID_IWICPixelFormatInfo]
    function GetFormatGUID(out Format: TGuid): HResult; stdcall;
    function GetColorContext(out ColorContext: IWICColorContext): HResult; stdcall;
    function GetBitsPerPixel(out BitsPerPixel: LongWord): HResult; stdcall;
    function GetChannelCount(out ChannelCount: LongWord): HResult; stdcall;
    function GetChannelMask(ChannelIndex, MaskBufferSize: LongWord; MaskBuffer: Pointer;
      out ActualSize: LongWord): HResult; stdcall;
  end;

  IWICPixelFormatInfo2 = interface(IWICPixelFormatInfo)
    [SID_IWICPixelFormatInfo2]
    function SupportsTransparency(out SupportsTransparency: LongBool): HResult; stdcall;
    function GetNumericRepresentation(
      out pNumericRepresentation: WICPixelFormatNumericRepresentation): HResult; stdcall;
  end;

  IWICImagingFactory = interface(IUnknown)
    [SID_IWICImagingFactory]
    function CreateDecoderFromFilename(FileName: PWideChar; const Vendor: TGuid; DesiredAccess: LongWord;
      MetaDataOptions: WICDecodeOptions; out Decoder: IWICBitmapDecoder): HResult; stdcall;
    function CreateDecoderFromStream(Stream: IStream; const Vendor: TGuid; MetaDataOptions: WICDecodeOptions;
      out Decoder: IWICBitmapDecoder): HResult; stdcall;
    function CreateDecoderFromFileHandle(&File: ULONG_PTR; const Vendor: TGuid; MetaDataOptions: WICDecodeOptions;
      out Decoder: IWICBitmapDecoder): HResult; stdcall;
    function CreateComponentInfo(const Component: TGuid; out Info: IWICComponentInfo): HResult; stdcall;
    function CreateDecoder(const ContainerFormat: TGuid; const Vendor: TGuid;
      out Decoder: IWICBitmapDecoder): HResult; stdcall;
    function CreateEncoder(const ContainerFormat: TGuid; const Vendor: TGuid;
      out Encoder: IWICBitmapEncoder): HResult; stdcall;
    function CreatePalette(out Palette: IWICPalette): HResult; stdcall;
    function CreateFormatConverter(out FormatConverter: IWICFormatConverter): HResult; stdcall;
    function CreateBitmapScaler(out BitmapScaler: IWICBitmapScaler): HResult; stdcall;
    function CreateBitmapClipper(out BitmapClipper: IWICBitmapClipper): HResult; stdcall;
    function CreateBitmapFlipRotator(out BitmapFlipRotator: IWICBitmapFlipRotator): HResult; stdcall;
    function CreateStream(out Stream: IWICStream): HResult; stdcall;
    function CreateColorContext(out ColorContext: IWICColorContext): HResult; stdcall;
    function CreateColorTransformer(out ColorTransform: IWICColorTransform): HResult; stdcall;
    function CreateBitmap(Width, Height: LongWord; const PixelFormat: WICPixelFormatGUID;
      Option: WICBitmapCreateCacheOption; out Bitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromSource(BitmapSource: IWICBitmapSource; Option: WICBitmapCreateCacheOption;
      out Bitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromSourceRect(BitmapSource: IWICBitmapSource; X, Y, Width, Height: LongWord;
      out Bitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromMemory(Width, Height: LongWord; const PixelFormat: WICPixelFormatGUID; Stride,
      BufferSize: LongWord; Buffer: Pointer; out Bitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromHBITMAP(BitmapHandle: HBitmap; Palette: HPalette; Options: WICBitmapAlphaChannelOption;
      out Bitmap: IWICBitmap): HResult; stdcall;
    function CreateBitmapFromHICON(IconHandle: HIcon; out Bitmap: IWICBitmap): HResult; stdcall;
    function CreateComponentEnumerator(ComponentTypes, Options: LongWord;
      out EnumUnknown: IEnumUnknown): HResult; stdcall;
    function CreateFastMetadataEncoderFromDecoder(Decoder: IWICBitmapDecoder;
      out FastEncoder: IWICFastMetadataEncoder): HResult; stdcall;
    function CreateFastMetadataEncoderFromFrameDecode(FrameDecoder: IWICBitmapFrameDecode;
      out FastEncoder: IWICFastMetadataEncoder): HResult; stdcall;
    function CreateQueryWriter(const MetadataFormat: TGuid; const Vendor: TGuid;
      out QueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
    function CreateQueryWriterFromReader(QueryReader: IWICMetadataQueryReader; const Vendor: TGuid;
      out QueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;

  IWICDevelopRawNotificationCallback = interface(IUnknown)
    [SID_IWICDevelopRawNotificationCallback]
    function Notify(NotificationMask: LongWord): HResult; stdcall;
  end;

  IWICDevelopRaw = interface(IWICBitmapFrameDecode)
    [SID_IWICDevelopRaw]
    function QueryRawCapabilitiesInfo(out Info: WICRawCapabilitiesInfo): HResult; stdcall;
    function LoadParameterSet(ParameterSet: WICRawParameterSet): HResult; stdcall;
    function GetCurrentParameterSet(out CurrentParameterSet: IPropertyBag2): HResult; stdcall;
    function SetExposureCompensation(EV: Double): HResult; stdcall;
    function GetExposureCompensation(out EV: Double): HResult; stdcall;
    function SetWhitePointRGB(Red, Green, Blue: LongWord): HResult; stdcall;
    function GetWhitePointRGB(out Red, Green, Blue: LongWord): HResult; stdcall;
    function SetNamedWhitePoint(WhitePoint: WICNamedWhitePoint): HResult; stdcall;
    function GetNamedWhitePoint(out WhitePoint: WICNamedWhitePoint): HResult; stdcall;
    function SetWhitePointKelvin(WhitePointKelvin: LongWord): HResult; stdcall;
    function GetWhitePointKelvin(out WhitePointKelvin: LongWord): HResult; stdcall;
    function GetKelvinRangeInfo(out MinKelvinTemp, MaxKelvinTemp, KelvinTempStepValue: LongWord): HResult; stdcall;
    function SetContrast(Contrast: Double): HResult; stdcall;
    function GetContrast(out Contrast: Double): HResult; stdcall;
    function SetGamma(Gamma: Double): HResult; stdcall;
    function GetGamma(out Gamma: Double): HResult; stdcall;
    function SetSharpness(Sharpness: Double): HResult; stdcall;
    function GetSharpness(out Sharpness: Double): HResult; stdcall;
    function SetSaturation(Saturation: Double): HResult; stdcall;
    function GetSaturation(out Saturation: Double): HResult; stdcall;
    function SetTint(Tint: Double): HResult; stdcall;
    function GetTint(out Tint: Double): HResult; stdcall;
    function SetNoiseReduction(NoiseReduction: Double): HResult; stdcall;
    function GetNoiseReduction(out NoiseReduction: Double): HResult; stdcall;
    function SetDestinationColorContext(ColorContext: IWICColorContext): HResult; stdcall;
    function SetToneCurve(ToneCurveSize: LongWord; ToneCurve: PWICRawToneCurve): HResult; stdcall;
    function GetToneCurve(ToneCurveBufferSize: LongWord; ToneCurve: PWICRawToneCurve;
      out ActualToneCurveBufferSize: LongWord): HResult; stdcall;
    function SetRotation(Rotation: Double): HResult; stdcall;
    function GetRotation(out Rotation: Double): HResult; stdcall;
    function SetRenderMode(RenderMode: WICRawRenderMode): HResult; stdcall;
    function GetRenderMode(out RenderMode: WICRawRenderMode): HResult; stdcall;
    function SetNotificationCallback(Callback: IWICDevelopRawNotificationCallback): HResult; stdcall;
  end;

const
  WindowsCodecsLib = 'WindowsCodecs';

function WICConvertBitmapSource(const DestFormat: WICPixelFormatGUID; Source: IWICBitmapSource;
  out Dest: IWICBitmapSource): HResult; stdcall; external WindowsCodecsLib name 'WICConvertBitmapSource';

function WICCreateBitmapFromSection(Width, Height: LongWord; const PixelFormat: WICPixelFormatGUID;
  Section: THandle; Stride, Offset: LongWord; out Bitmap: IWICBitmap): HResult; stdcall;
  external WindowsCodecsLib name 'WICCreateBitmapFromSection';

function WICCreateBitmapFromSectionEx(Width, Height: LongWord; const PixelFormat: WICPixelFormatGUID;
  Section: THandle; Stride, Offset: LongWord; DesiredAccessLevel: WICSectionAccessLevel;
  out Bitmap: IWICBitmap): HResult; stdcall; external WindowsCodecsLib name 'WICCreateBitmapFromSectionEx';

function WICMapGuidToShortName(const Guid: TGuid; NameBufferSize: LongWord; Name: PWideChar;
  out ActualBufferSize: LongWord): HResult; stdcall; external WindowsCodecsLib name 'WICMapGuidToShortName';

function WICMapShortNameToGuid(Name: PCWSTR; out Guid: TGuid): HResult; stdcall;
  external WindowsCodecsLib name 'WICMapShortNameToGuid';

function WICMapSchemaToName(const MetadataFormat: TGuid; Schema: PWideChar; NameBufferSize: LongWord; Name: PWideChar;
  out ActualBufferSize: LongWord): HResult; stdcall; external WindowsCodecsLib name 'WICMapSchemaToName';

implementation

end.
