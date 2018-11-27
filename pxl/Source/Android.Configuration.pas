{
  Copyright (C) 2010 The Android Open Source Project

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
   http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed
  on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for
  the specific language governing permissions and limitations under the License.

  Android NDK C/C++ file:
    android/configuration.h

  Original source code was taken from:
    %NDK_DIR%/platforms/android-9/arch-arm/usr/include/

  Pascal translation by Yuriy Kotsarenko, August 2015.
}
unit Android.Configuration;

interface

{$INCLUDE Android.Config.inc}

uses
  Android.AssetManager;

{$INCLUDE Android.LibDefs.inc}

type
  PAConfiguration = ^AConfiguration;
  AConfiguration = record
  end;

const
  ACONFIGURATION_ORIENTATION_ANY = $0000;
  ACONFIGURATION_ORIENTATION_PORT = $0001;
  ACONFIGURATION_ORIENTATION_LAND = $0002;
  ACONFIGURATION_ORIENTATION_SQUARE = $0003;

  ACONFIGURATION_TOUCHSCREEN_ANY = $0000;
  ACONFIGURATION_TOUCHSCREEN_NOTOUCH = $0001;
  ACONFIGURATION_TOUCHSCREEN_STYLUS = $0002;
  ACONFIGURATION_TOUCHSCREEN_FINGER = $0003;

  ACONFIGURATION_DENSITY_DEFAULT = 0;
  ACONFIGURATION_DENSITY_LOW = 120;
  ACONFIGURATION_DENSITY_MEDIUM = 160;
  ACONFIGURATION_DENSITY_HIGH = 240;
  ACONFIGURATION_DENSITY_NONE = $FFFF;

  ACONFIGURATION_KEYBOARD_ANY = $0000;
  ACONFIGURATION_KEYBOARD_NOKEYS = $0001;
  ACONFIGURATION_KEYBOARD_QWERTY = $0002;
  ACONFIGURATION_KEYBOARD_12KEY = $0003;

  ACONFIGURATION_NAVIGATION_ANY = $0000;
  ACONFIGURATION_NAVIGATION_NONAV = $0001;
  ACONFIGURATION_NAVIGATION_DPAD = $0002;
  ACONFIGURATION_NAVIGATION_TRACKBALL = $0003;
  ACONFIGURATION_NAVIGATION_WHEEL = $0004;

  ACONFIGURATION_KEYSHIDDEN_ANY = $0000;
  ACONFIGURATION_KEYSHIDDEN_NO = $0001;
  ACONFIGURATION_KEYSHIDDEN_YES = $0002;
  ACONFIGURATION_KEYSHIDDEN_SOFT = $0003;

  ACONFIGURATION_NAVHIDDEN_ANY = $0000;
  ACONFIGURATION_NAVHIDDEN_NO = $0001;
  ACONFIGURATION_NAVHIDDEN_YES = $0002;

  ACONFIGURATION_SCREENSIZE_ANY = $00;
  ACONFIGURATION_SCREENSIZE_SMALL = $01;
  ACONFIGURATION_SCREENSIZE_NORMAL = $02;
  ACONFIGURATION_SCREENSIZE_LARGE = $03;
  ACONFIGURATION_SCREENSIZE_XLARGE = $04;

  ACONFIGURATION_SCREENLONG_ANY = $00;
  ACONFIGURATION_SCREENLONG_NO = $1;
  ACONFIGURATION_SCREENLONG_YES = $2;

  ACONFIGURATION_UI_MODE_TYPE_ANY = $00;
  ACONFIGURATION_UI_MODE_TYPE_NORMAL = $01;
  ACONFIGURATION_UI_MODE_TYPE_DESK = $02;
  ACONFIGURATION_UI_MODE_TYPE_CAR = $03;

  ACONFIGURATION_UI_MODE_NIGHT_ANY = $00;
  ACONFIGURATION_UI_MODE_NIGHT_NO = $1;
  ACONFIGURATION_UI_MODE_NIGHT_YES = $2;

  ACONFIGURATION_MCC = $0001;
  ACONFIGURATION_MNC = $0002;
  ACONFIGURATION_LOCALE = $0004;
  ACONFIGURATION_TOUCHSCREEN = $0008;
  ACONFIGURATION_KEYBOARD = $0010;
  ACONFIGURATION_KEYBOARD_HIDDEN = $0020;
  ACONFIGURATION_NAVIGATION = $0040;
  ACONFIGURATION_ORIENTATION = $0080;
  ACONFIGURATION_DENSITY = $0100;
  ACONFIGURATION_SCREEN_SIZE = $0200;
  ACONFIGURATION_VERSION = $0400;
  ACONFIGURATION_SCREEN_LAYOUT = $0800;
  ACONFIGURATION_UI_MODE = $1000;

{
  Create a new AConfiguration, initialized with no values set.
}
function AConfiguration_new: PAConfiguration; cdecl;
  external libandroid name 'AConfiguration_new';

{
  Free an AConfiguration that was previously created with AConfiguration_new().
}
procedure AConfiguration_delete(config: PAConfiguration); cdecl;
  external libandroid name 'AConfiguration_delete';

{
  Create and return a new AConfiguration based on the current configuration in use in the given AssetManager.
}
procedure AConfiguration_fromAssetManager(_out: PAConfiguration; am: PAAssetManager); cdecl;
  external libandroid name 'AConfiguration_fromAssetManager';

{
  Copy the contents of 'src' to 'dest'.
}
procedure AConfiguration_copy(dest, src: PAConfiguration); cdecl;
  external libandroid name 'AConfiguration_copy';

{
  Return the current MCC set in the configuration.  0 if not set.
}
function AConfiguration_getMcc(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getMcc';

{
  Set the current MCC in the configuration.  0 to clear.
}
procedure AConfiguration_setMcc(config: PAConfiguration; mcc: LongInt); cdecl;
  external libandroid name 'AConfiguration_setMcc';

{
  Return the current MNC set in the configuration.  0 if not set.
}
function AConfiguration_getMnc(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getMnc';

{
  Set the current MNC in the configuration.  0 to clear.
}
procedure AConfiguration_setMnc(config: PAConfiguration; mnc: LongInt); cdecl;
  external libandroid name 'AConfiguration_setMnc';

{
  Return the current language code set in the configuration.  The output will be filled with an array of two
  characters. They are not 0-terminated.  If a language is not set, they will be 0.
}
procedure AConfiguration_getLanguage(config: PAConfiguration; outLanguage: PAnsiChar); cdecl;
  external libandroid name 'AConfiguration_getLanguage';

{
  Set the current language code in the configuration, from the first two characters in the string.
}
procedure AConfiguration_setLanguage(config: PAConfiguration; language: PAnsiChar); cdecl;
  external libandroid name 'AConfiguration_setLanguage';

{
  Return the current country code set in the configuration.  The output will be filled with an array of two characters.
  They are not 0-terminated.  If a country is not set, they will be 0.
}
procedure AConfiguration_getCountry(config: PAConfiguration; outCountry: PAnsiChar); cdecl;
  external libandroid name 'AConfiguration_getCountry';

{
  Set the current country code in the configuration, from the first two characters in the string.
}
procedure AConfiguration_setCountry(config: PAConfiguration; country: PAnsiChar); cdecl;
  external libandroid name 'AConfiguration_setCountry';

{
  Return the current ACONFIGURATION_ORIENTATION_* set in the configuration.
}
function AConfiguration_getOrientation(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getOrientation';

{
  Set the current orientation in the configuration.
}
procedure AConfiguration_setOrientation(config: PAConfiguration; orientation: LongInt); cdecl;
  external libandroid name 'AConfiguration_setOrientation';

{
  Return the current ACONFIGURATION_TOUCHSCREEN_* set in the configuration.
}
function AConfiguration_getTouchscreen(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getTouchscreen';

{
  Set the current touchscreen in the configuration.
}
procedure AConfiguration_setTouchscreen(config: PAConfiguration; touchscreen: LongInt); cdecl;
  external libandroid name 'AConfiguration_setTouchscreen';

{
  Return the current ACONFIGURATION_DENSITY_* set in the configuration.
}
function AConfiguration_getDensity(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getDensity';

{
  Set the current density in the configuration.
}
procedure AConfiguration_setDensity(config: PAConfiguration; density: LongInt); cdecl;
  external libandroid name 'AConfiguration_setDensity';

{
  Return the current ACONFIGURATION_KEYBOARD_* set in the configuration.
}
function AConfiguration_getKeyboard(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getKeyboard';

{
  Set the current keyboard in the configuration.
}
procedure AConfiguration_setKeyboard(config: PAConfiguration; keyboard: LongInt); cdecl;
  external libandroid name 'AConfiguration_setKeyboard';

{
  Return the current ACONFIGURATION_NAVIGATION_* set in the configuration.
}
function AConfiguration_getNavigation(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getNavigation';

{
  Set the current navigation in the configuration.
}
procedure AConfiguration_setNavigation(config: PAConfiguration; navigation: LongInt); cdecl;
  external libandroid name 'AConfiguration_setNavigation';

{
  Return the current ACONFIGURATION_KEYSHIDDEN_* set in the configuration.
}
function AConfiguration_getKeysHidden(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getKeysHidden';

{
  Set the current keys hidden in the configuration.
}
procedure AConfiguration_setKeysHidden(config: PAConfiguration; keysHidden: LongInt); cdecl;
  external libandroid name 'AConfiguration_setKeysHidden';

{
  Return the current ACONFIGURATION_NAVHIDDEN_* set in the configuration.
}
function AConfiguration_getNavHidden(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getNavHidden';

{
  Set the current nav hidden in the configuration.
}
procedure AConfiguration_setNavHidden(config: PAConfiguration; navHidden: LongInt); cdecl;
  external libandroid name 'AConfiguration_setNavHidden';

{
  Return the current SDK (API) version set in the configuration.
}
function AConfiguration_getSdkVersion(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getSdkVersion';

{
  Set the current SDK version in the configuration.
}
procedure AConfiguration_setSdkVersion(config: PAConfiguration; sdkVersion: LongInt); cdecl;
  external libandroid name 'AConfiguration_setSdkVersion';

{
  Return the current ACONFIGURATION_SCREENSIZE_* set in the configuration.
}
function AConfiguration_getScreenSize(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getScreenSize';

{
  Set the current screen size in the configuration.
}
procedure AConfiguration_setScreenSize(config: PAConfiguration; screenSize: LongInt); cdecl;
  external libandroid name 'AConfiguration_setScreenSize';

{
  Return the current ACONFIGURATION_SCREENLONG_* set in the configuration.
}
function AConfiguration_getScreenLong(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getScreenLong';

{
  Set the current screen long in the configuration.
}
procedure AConfiguration_setScreenLong(config: PAConfiguration; screenLong: LongInt); cdecl;
  external libandroid name 'AConfiguration_setScreenLong';

{
  Return the current ACONFIGURATION_UI_MODE_TYPE_* set in the configuration.
}
function AConfiguration_getUiModeType(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getUiModeType';

{
  Set the current UI mode type in the configuration.
}
procedure AConfiguration_setUiModeType(config: PAConfiguration; uiModeType: LongInt); cdecl;
  external libandroid name 'AConfiguration_setUiModeType';

{
  Return the current ACONFIGURATION_UI_MODE_NIGHT_* set in the configuration.
}
function AConfiguration_getUiModeNight(config: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_getUiModeNight';

{
  Set the current UI mode night in the configuration.
}
procedure AConfiguration_setUiModeNight(config: PAConfiguration; uiModeNight: LongInt); cdecl;
  external libandroid name 'AConfiguration_setUiModeNight';

{
  Perform a diff between two configurations.  Returns a bit mask of ACONFIGURATION_* constants, each bit set meaning
  that configuration element is different between them.
}
function AConfiguration_diff(config1, config2: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_diff';

{
  Determine whether 'base' is a valid configuration for use within the environment 'requested'.  Returns 0 if there are
  any values in 'base' that conflict with 'requested'.  Returns 1 if it does not conflict.
}
function AConfiguration_match(base, requested: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_match';

{
  Determine whether the configuration in 'test' is better than the existing configuration in 'base'.  If 'requested' is
  non-NULL, this decision is based on the overall configuration given there.  If it is NULL, this decision is simply
  based on which configuration is more specific.  Returns non-0 if 'test' is better than 'base'.

  This assumes you have already filtered the configurations with AConfiguration_match().
}
function AConfiguration_isBetterThan(base, test, requested: PAConfiguration): LongInt; cdecl;
  external libandroid name 'AConfiguration_isBetterThan';

implementation

end.
