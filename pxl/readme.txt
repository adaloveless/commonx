LICENSE: NOTE!  The software contained in this folder and subfolders
is distributed under the Apache 2.0 license.  This license overrules any lesser
license that may have been included along with accompanying code/materials.

-------------------------------------------------------------------------------

           Pascal eXtended Library v1.0.0, dated 02-Oct-2015.
             Copyright (c) 2000 - 2015  Yuriy Kotsarenko
                      http://www.afterwarp.net

-------------------------------------------------------------------------------

             Heavily Modified by Jason Nelson (adaloveless) 2016-2018

                Notice of changes:

 This version of the PXL is heavily modified.  Whereas PXL was originally a 
2D library with the aim of portability to ultra-low-end devices as well as 
higher end systems.  This version reinstates traditional 3D functionality 
in favor of Matrix-based operations.  It is best used in conjunction with
the commonx library (https://github.com/adaloveless/commonx), which extends
the library with TGameObject, TCamera, TLight, TScene, and TSprite classes for
some classic 8-16bit arcade style scaled sprite style visuals.

This fork of the original PXL code is very much in flux as is the adaloveless/commonx 
library.  Use at your own risk.  Please contribute changes back to the original
repository and consider becoming an official contributor.  The remainder of this document
is in it's original form but may not apply to this heavily modified source base.

--------------------------------------------------------------------------------


This product requires either Embarcadero Delphi XE 8 (or any later version) or
FreePascal 3.0 with Lazarus 1.4 (or any later versions).

When used with Embarcadero Delphi, the following platforms are supported:

1) Windows 32 and 64-bit
2) Mac OS X
3) Android
4) iOS 32 and 64-bit

Installation is quite straightforward, you just need to add full path to
"Source" folder in IDE's "library path" for each of the platforms. Please visit
our web site for additional installation instructions.

When used with FreePascal/Lazarus, the following platforms are supported:

1) Windows 32 and 64-bit
2) Mac OS X Carbon (32-bit) and Cocoa (64-bit)
3) Linux 32 and 64-bit
4) Linux ARM-EL and ARM-HF
5) Compact SingleBoard Computers (w/Linux OS):
  I) Raspberry PI with native support for fast System Clock, GPIO, I2C and SPI.
     GPU rendering both on HDMI display and to external displays is supported.
  II) Intel Galileo gen1 and gen2 with native support for GPIO (including
      "fast" pins), I2C and SPI.
  III) Intel Edison
  IV) Other generic platforms with support for GPIO, I2C, SPI, UART and V4L2.
6) Android (experimental)

If you have a working FreePascal/Lazarus environment with cross-compilation
to the necessary platforms, then no additional installation is required, just
open any of the available samples and start working from there.

Detailed installation instructions for each platform, along with some tips
for cross-compilations are available on our web site.

In order to use PXL documentation, locate "Help" sub-folder in this package
and open "index.html". Alternatively, you can create a shortcut to this file
on your desktop.

Remember that Pascal eXtended Library is still under active development, so
please keep checking our web site for updates and other important information.

You can discuss the development of Pascal eXtended Library (PXL) on our forums:
    http://www.afterwarp.net/forum

Remember that this library and its source code are distributed under terms of
GNU Lesser General Public License (LGPL) version 3. By using this product, you
acknowledge your consent to be bound by the terms of LGPL.

-------------------------------------------------------------------------------
Some of the included artwork was made by Humberto Andrade and is copyright
protected. Any redistribution of this artwork outside of the framework is
strictly prohibited.

If you have any inquries, send e-mail to: yunkot@gmail.com