

{$IFNDEF RTTI}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
{$DEFINE RUDP1}
{$IFDEF FPC}
{$DEFINE LAZ}
{$mode Delphi}
{$H+}
{$modeswitch advancedrecords+}
{$DEFINE USE_SYNCOBJS}
{x$DEFINE SUPPORTS_RECORD_HELPERS}
{$DEFINE GE_XE8}
{$DEFINE GE_XE7}
{$DEFINE GE_XE6}
{$DEFINE GE_XE5}
{$DEFINE GE_XE4}
{$DEFINE GE_XE3}
{$DEFINE GE_XE2}
{$DEFINE GE_XE}
{$DEFINE GE_2010}
{$DEFINE GE_2009}
{$DEFINE GE_2007NET}
{$DEFINE GE_2007}
{$DEFINE GE_2006}
{$DEFINE GE_2005}
{$DEFINE GE_8NET}
{$DEFINE GE_7}
{$DEFINE GE_6}
{$DEFINE GE_5}
{$DEFINE GE_4}
{$DEFINE GE_3}
{$DEFINE GE_2}
{$DEFINE GE_1}
{$DEFINE GT_XE8}
{$DEFINE GT_XE7}
{$DEFINE GT_XE6}
{$DEFINE GT_XE5}
{$DEFINE GT_XE4}
{$DEFINE GT_XE3}
{$DEFINE GT_XE2}
{$DEFINE GT_XE}
{$DEFINE GT_2010}
{$DEFINE GT_2009}
{$DEFINE GT_2007NET}
{$DEFINE GT_2007}
{$DEFINE GT_2006}
{$DEFINE GT_2005}
{$DEFINE GT_8NET}
{$DEFINE GT_7}
{$DEFINE GT_6}
{$DEFINE GT_5}
{$DEFINE GT_4}
{$DEFINE GT_3}
{$DEFINE GT_2}
{$DEFINE GT_1}
{$DEFINE LT_XE6}
{$DEFINE LT_XE5}
{$DEFINE LT_XE4}
{$DEFINE LT_XE3}
{$DEFINE LT_XE2}
{$DEFINE LT_XE}
{$DEFINE LT_2010}
{$DEFINE LT_2009}
{$DEFINE LT_2007NET}
{$DEFINE LT_2007}
{$DEFINE LT_2006}
{$DEFINE LT_2005}
{$DEFINE LT_8NET}
{$DEFINE LT_7}
{$DEFINE LT_6}
{$DEFINE LT_5}
{$DEFINE LT_4}
{$DEFINE LT_3}
{$DEFINE LT_2}
{$DEFINE LT_1}
{$DEFINE LE_XE7}
{$DEFINE LE_XE6}
{$DEFINE LE_XE5}
{$DEFINE LE_XE4}
{$DEFINE LE_XE3}
{$DEFINE LE_XE2}
{$DEFINE LE_XE}
{$DEFINE LE_2010}
{$DEFINE LE_2009}
{$DEFINE LE_2007NET}
{$DEFINE LE_2007}
{$DEFINE LE_2006}
{$DEFINE LE_2005}
{$DEFINE LE_8NET}
{$DEFINE LE_7}
{$DEFINE LE_6}
{$DEFINE LE_5}
{$DEFINE LE_4}
{$DEFINE LE_3}
{$DEFINE LE_2}
{$DEFINE LE_1}

{$ELSE}
{$DEFINE SUPPORTS_RECORD_HELPERS}
{$IF CompilerVersion >=29.0}{$DEFINE GE_XE8}{$ENDIF}
{$IF CompilerVersion >=28.0}{$DEFINE GE_XE7}{$ENDIF}
{$IF CompilerVersion >=27.0}{$DEFINE GE_XE6}{$ENDIF}
{$IF CompilerVersion >=26.0}{$DEFINE GE_XE5}{$ENDIF}
{$IF CompilerVersion >=25.0}{$DEFINE GE_XE4}{$ENDIF}
{$IF CompilerVersion >=24.0}{$DEFINE GE_XE3}{$ENDIF}
{$IF CompilerVersion >=23.0}{$DEFINE GE_XE2}{$ENDIF}
{$IF CompilerVersion >=22.0}{$DEFINE GE_XE}{$ENDIF}
{$IF CompilerVersion >=21.0}{$DEFINE GE_2010}{$ENDIF}
{$IF CompilerVersion >=20.0}{$DEFINE GE_2009}{$ENDIF}
{$IF CompilerVersion >=19.0}{$DEFINE GE_2007NET}{$ENDIF}
{$IF CompilerVersion >=18.5}{$DEFINE GE_2007}{$ENDIF}
{$IF CompilerVersion >=18.0}{$DEFINE GE_2006}{$ENDIF}
{$IF CompilerVersion >=17.0}{$DEFINE GE_2005}{$ENDIF}
{$IF CompilerVersion >=16.0}{$DEFINE GE_8NET}{$ENDIF}
{$IF CompilerVersion >=15.0}{$DEFINE GE_7}{$ENDIF}
{$IF CompilerVersion >=14.0}{$DEFINE GE_6}{$ENDIF}
{$IF CompilerVersion >=13.0}{$DEFINE GE_5}{$ENDIF}
{$IF CompilerVersion >=12.0}{$DEFINE GE_4}{$ENDIF}
{$IF CompilerVersion >=10.0}{$DEFINE GE_3}{$ENDIF}
{$IF CompilerVersion >=9.0}{$DEFINE GE_2}{$ENDIF}
{$IF CompilerVersion >=8.0}{$DEFINE GE_1}{$ENDIF}

{$IF CompilerVersion > 29.0}{$DEFINE GT_XE8}{$ENDIF}
{$IF CompilerVersion > 28.0}{$DEFINE GT_XE7}{$ENDIF}
{$IF CompilerVersion > 27.0}{$DEFINE GT_XE6}{$ENDIF}
{$IF CompilerVersion > 26.0}{$DEFINE GT_XE5}{$ENDIF}
{$IF CompilerVersion > 25.0}{$DEFINE GT_XE4}{$ENDIF}
{$IF CompilerVersion > 24.0}{$DEFINE GT_XE3}{$ENDIF}
{$IF CompilerVersion > 23.0}{$DEFINE GT_XE2}{$ENDIF}
{$IF CompilerVersion > 22.0}{$DEFINE GT_XE}{$ENDIF}
{$IF CompilerVersion > 21.0}{$DEFINE GT_2010}{$ENDIF}
{$IF CompilerVersion > 20.0}{$DEFINE GT_2009}{$ENDIF}
{$IF CompilerVersion > 19.0}{$DEFINE GT_2007NET}{$ENDIF}
{$IF CompilerVersion > 18.5}{$DEFINE GT_2007}{$ENDIF}
{$IF CompilerVersion > 18.0}{$DEFINE GT_2006}{$ENDIF}
{$IF CompilerVersion > 17.0}{$DEFINE GT_2005}{$ENDIF}
{$IF CompilerVersion > 16.0}{$DEFINE GT_8NET}{$ENDIF}
{$IF CompilerVersion > 15.0}{$DEFINE GT_7}{$ENDIF}
{$IF CompilerVersion > 14.0}{$DEFINE GT_6}{$ENDIF}
{$IF CompilerVersion > 13.0}{$DEFINE GT_5}{$ENDIF}
{$IF CompilerVersion > 12.0}{$DEFINE GT_4}{$ENDIF}
{$IF CompilerVersion > 10.0}{$DEFINE GT_3}{$ENDIF}
{$IF CompilerVersion > 9.0}{$DEFINE GT_2}{$ENDIF}
{$IF CompilerVersion > 8.0}{$DEFINE GT_1}{$ENDIF}

{$IF CompilerVersion <30.0}{$DEFINE LT_XE9}{$ENDIF}
{$IF CompilerVersion <29.0}{$DEFINE LT_XE8}{$ENDIF}
{$IF CompilerVersion <28.0}{$DEFINE LT_XE7}{$ENDIF}
{$IF CompilerVersion <27.0}{$DEFINE LT_XE6}{$ENDIF}
{$IF CompilerVersion <26.5}{$DEFINE LT_XE5}{$ENDIF}
{$IF CompilerVersion <25.0}{$DEFINE LT_XE4}{$ENDIF}
{$IF CompilerVersion <24.0}{$DEFINE LT_XE3}{$ENDIF}
{$IF CompilerVersion <23.0}{$DEFINE LT_XE2}{$ENDIF}
{$IF CompilerVersion <22.0}{$DEFINE LT_XE}{$ENDIF}
{$IF CompilerVersion <21.0}{$DEFINE LT_2010}{$ENDIF}
{$IF CompilerVersion <20.0}{$DEFINE LT_2009}{$ENDIF}
{$IF CompilerVersion <19.0}{$DEFINE LT_2007NET}{$ENDIF}
{$IF CompilerVersion <18.5}{$DEFINE LT_2007}{$ENDIF}
{$IF CompilerVersion <18.0}{$DEFINE LT_2006}{$ENDIF}
{$IF CompilerVersion <17.0}{$DEFINE LT_2005}{$ENDIF}
{$IF CompilerVersion <16.0}{$DEFINE LT_8NET}{$ENDIF}
{$IF CompilerVersion <15.0}{$DEFINE LT_7}{$ENDIF}
{$IF CompilerVersion <14.0}{$DEFINE LT_6}{$ENDIF}
{$IF CompilerVersion <13.0}{$DEFINE LT_5}{$ENDIF}
{$IF CompilerVersion <12.0}{$DEFINE LT_4}{$ENDIF}
{$IF CompilerVersion <10.0}{$DEFINE LT_3}{$ENDIF}
{$IF CompilerVersion <9.0}{$DEFINE LT_2}{$ENDIF}
{$IF CompilerVersion <8.0}{$DEFINE LT_1}{$ENDIF}

{$IF CompilerVersion <=30.0}{$DEFINE LE_XE9}{$ENDIF}
{$IF CompilerVersion <=29.0}{$DEFINE LE_XE8}{$ENDIF}
{$IF CompilerVersion <=28.0}{$DEFINE LE_XE7}{$ENDIF}
{$IF CompilerVersion <=27.0}{$DEFINE LE_XE6}{$ENDIF}
{$IF CompilerVersion <=26.5}{$DEFINE LE_XE5}{$ENDIF}
{$IF CompilerVersion <=25.0}{$DEFINE LE_XE4}{$ENDIF}
{$IF CompilerVersion <=24.0}{$DEFINE LE_XE3}{$ENDIF}
{$IF CompilerVersion <=23.0}{$DEFINE LE_XE2}{$ENDIF}
{$IF CompilerVersion <=22.0}{$DEFINE LE_XE}{$ENDIF}
{$IF CompilerVersion <=21.0}{$DEFINE LE_2010}{$ENDIF}
{$IF CompilerVersion <=20.0}{$DEFINE LE_2009}{$ENDIF}
{$IF CompilerVersion <=19.0}{$DEFINE LE_2007NET}{$ENDIF}
{$IF CompilerVersion <=18.5}{$DEFINE LE_2007}{$ENDIF}
{$IF CompilerVersion <=18.0}{$DEFINE LE_2006}{$ENDIF}
{$IF CompilerVersion <=17.0}{$DEFINE LE_2005}{$ENDIF}
{$IF CompilerVersion <=16.0}{$DEFINE LE_8NET}{$ENDIF}
{$IF CompilerVersion <=15.0}{$DEFINE LE_7}{$ENDIF}
{$IF CompilerVersion <=14.0}{$DEFINE LE_6}{$ENDIF}
{$IF CompilerVersion <=13.0}{$DEFINE LE_5}{$ENDIF}
{$IF CompilerVersion <=12.0}{$DEFINE LE_4}{$ENDIF}
{$IF CompilerVersion <=10.0}{$DEFINE LE_3}{$ENDIF}
{$IF CompilerVersion <=9.0}{$DEFINE LE_2}{$ENDIF}
{$IF CompilerVersion <=8.0}{$DEFINE LE_1}{$ENDIF}

{$IF CompilerVersion >=29.0}{$IF CompilerVersion <30.0}{$DEFINE IS_XE8}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=28.0}{$IF CompilerVersion <29.0}{$DEFINE IS_XE7}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=27.0}{$IF CompilerVersion <28.0}{$DEFINE IS_XE6}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=26.0}{$IF CompilerVersion <27.0}{$DEFINE IS_XE5}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=25.0}{$IF CompilerVersion <26.0}{$DEFINE IS_XE4}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=24.0}{$IF CompilerVersion <25.0}{$DEFINE IS_XE3}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=23.0}{$IF CompilerVersion <24.0}{$DEFINE IS_XE2}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=22.0}{$IF CompilerVersion <23.0}{$DEFINE IS_XE}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=21.0}{$IF CompilerVersion <22.0}{$DEFINE IS_2010}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=20.0}{$IF CompilerVersion <21.0}{$DEFINE IS_2009}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=19.0}{$IF CompilerVersion <20.0}{$DEFINE IS_2007NET}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=18.5}{$IF CompilerVersion <19.0}{$DEFINE IS_2007}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=18.0}{$IF CompilerVersion <18.5}{$DEFINE IS_2006}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=17.0}{$IF CompilerVersion <18.0}{$DEFINE IS_2005}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=16.0}{$IF CompilerVersion <17.0}{$DEFINE IS_8NET}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=15.0}{$IF CompilerVersion <16.0}{$DEFINE IS_7}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=14.0}{$IF CompilerVersion <15.0}{$DEFINE IS_6}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=13.0}{$IF CompilerVersion <14.0}{$DEFINE IS_5}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=12.0}{$IF CompilerVersion <13.0}{$DEFINE IS_4}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=10.0}{$IF CompilerVersion <12.0}{$DEFINE IS_3}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=9.0}{$IF CompilerVersion <10.0}{$DEFINE IS_2}{$ENDIF}{$ENDIF}
{$IF CompilerVersion >=8.0}{$IF CompilerVersion <9.0}{$DEFINE IS_1}{$ENDIF}{$ENDIF}
{$ENDIF}

{$IFDEF MACOS}
  {$DEFINE NOT_WIN}
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE NOT_WIN}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF WIN64}
  {$DEFINE WINDOWS}
{$ENDIF}
{$IFDEF WIN}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF WIN32}
  {$IFDEF CPUX86}
    {$DEFINE OLD_CALL_CONVENTIONS}
    {$DEFINE STDCALL}
    {$DEFINE CCALL}
  {$ENDIF}
{$ENDIF}

{$IFNDEF WINDOWS}
{$DEFINE USE_SYNCOBJS}
{$DEFINE NO_LEGACY_STRINGS}
{$DEFINE NOASM}
{$DEFINE NEED_FAKE_ANSISTRING}
{$ENDIF}
{$IFDEF IOS}
  {$DEFINE MOBILE}
{$ENDIF}
{$IFDEF ANDROID}
  {$DEFINE MOBILE}
{$ENDIF}




