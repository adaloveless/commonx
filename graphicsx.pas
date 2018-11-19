unit graphicsx;
{$i delphidefs.inc}

interface

uses
{$IFDEF FMX}
  system.UITypes, system.UIConsts,
{$ELSE}
  graphics,
{$ENDIF}
  types;

{$IFDEF FMX}
const
  clBlack = claBlack;
  clSilver = claSilver;
  clGrey = claGrey;
  clNavy = claNavy;
  clBlue = claBlue;
  clGreen = claGreen;
  clLime = claLime;
  clRed = claRed;
  clMagenta = claMagenta;
  clFuchia = claMagenta;
  clCyan = claCyan;
{$ENDIF}

type
{$IFDEF FMX}
  TColor = TAlphaColor;
  TPixelFormat = (pf8bit, pf16bit, pf24Bit, pf32bit);
{$ELSE}
  TColor = graphics.TColor;
  TPixelFormat = graphics.Tpixelformat;
{$ENDIF}



implementation

end.
