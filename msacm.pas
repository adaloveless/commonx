unit MSACM;
{$Message '********************************Compiling MSACM'}

// ----------------------------------------------------------------------
//
// msacm.pas
// 
// Description:
//
//   Audio Compression Manager Public Header File
//
//   Converted for Delphi 2.x by Armin Sander, Digital SimpleX
//                               (armin@dsx.de)
//
//   please mail any changes and fixes!
//
// History:
//
//   1.0 - first official release (3 Feb 1997)
//
// Disclaimer:
//
//   This product is supplied as is. The author disclaims all warranties,
//   expressed or implied, including, without limitation, the warranties
//   of merchantability and of fitness for any purpose. The author assumes
//   no liability for damages, direct or consequential, which may result
//   from the use of this product.
//
// ----------------------------------------------------------------------

interface
uses Windows, MMSystem;

// MMREG include

const
  MPEGLAYER3_ID_MPEG = 1;
  MPEGLAYER3_FLAG_PADDING_OFF = $00000002;
  MPEGLAYER3_WFX_EXTRA_BYTES = 12;
  WAVE_FORMAT_MPEG       = $00000050;       {MP3 format}
  WAVE_FORMAT_MPEGLAYER3 = $00000055;       {MP3 format}
	WAVE_FILTER_UNKNOWN        =  $0000;
	WAVE_FILTER_DEVELOPMENT    = ($FFFF);

type
  Tmpeglayer3waveformat_tag=packed record
    wfx:TWAVEFORMATEX;
    wID:word;
    fdwFlags:dword;
    nBlockSize:word;
    nFramesPerBlock:word;
    nCodecDelay:word;
  end;
	PWAVEFILTER = ^TWAVEFILTER;
	TWAVEFILTER = packed record
		cbStruct    : DWORD   ;        // Size of the filter in bytes
		dwFilterTag : DWORD   ;        // filter type
		fdwFilter   : DWORD   ;        // Flags for the filter (Universal Dfns)
		dwReserved  : array [0..5] of DWORD;      // Reserved for system use
	end;

const
	DRV_MAPPER_PREFERRED_INPUT_GET  = (DRV_USER + 0);

const
	DRV_MAPPER_PREFERRED_OUTPUT_GET = (DRV_USER + 2);

	DRVM_MAPPER_STATUS = $2000;

const
	WIDM_MAPPER_STATUS          = (DRVM_MAPPER_STATUS + 0);
	WAVEIN_MAPPER_STATUS_DEVICE = 0;
	WAVEIN_MAPPER_STATUS_MAPPED = 1;
	WAVEIN_MAPPER_STATUS_FORMAT = 2;

const
	WODM_MAPPER_STATUS           = (DRVM_MAPPER_STATUS + 0);
	WAVEOUT_MAPPER_STATUS_DEVICE = 0;
	WAVEOUT_MAPPER_STATUS_MAPPED = 1;
	WAVEOUT_MAPPER_STATUS_FORMAT = 2;

type
	HACMDRIVERID = THandle;
	PHACMDRIVERID = ^HACMDRIVERID;

type
	HACMDRIVER = THandle;
	PHACMDRIVER = ^HACMDRIVER;

type
	HACMSTREAM = THandle;
	PHACMSTREAM = ^HACMSTREAM;

type
	HACMOBJ = THandle;
	PHACMOBJ = ^HACMOBJ;

const
	ACMERR_BASE         = (512);
	ACMERR_NOTPOSSIBLE  = (ACMERR_BASE + 0);
	ACMERR_BUSY         = (ACMERR_BASE + 1);
	ACMERR_UNPREPARED   = (ACMERR_BASE + 2);
	ACMERR_CANCELED     = (ACMERR_BASE + 3);

const
	MM_ACM_OPEN        = (MM_STREAM_OPEN);  // conversion callback messages
	MM_ACM_CLOSE       = (MM_STREAM_CLOSE);
	MM_ACM_DONE        = (MM_STREAM_DONE);

function acmGetVersion : DWORD; stdcall;

function acmMetrics(hao : HACMOBJ; uMetric : UINT; var pMetric) : MMRESULT; stdcall;

const
	ACM_METRIC_COUNT_DRIVERS            = 1;
	ACM_METRIC_COUNT_CODECS             = 2;
	ACM_METRIC_COUNT_CONVERTERS         = 3;
	ACM_METRIC_COUNT_FILTERS            = 4;
	ACM_METRIC_COUNT_DISABLED           = 5;
	ACM_METRIC_COUNT_HARDWARE           = 6;
	ACM_METRIC_COUNT_LOCAL_DRIVERS      = 20;
	ACM_METRIC_COUNT_LOCAL_CODECS       = 21;
	ACM_METRIC_COUNT_LOCAL_CONVERTERS   = 22;
	ACM_METRIC_COUNT_LOCAL_FILTERS      = 23;
	ACM_METRIC_COUNT_LOCAL_DISABLED     = 24;
	ACM_METRIC_HARDWARE_WAVE_INPUT      = 30;
	ACM_METRIC_HARDWARE_WAVE_OUTPUT     = 31;
	ACM_METRIC_MAX_SIZE_FORMAT          = 50;
	ACM_METRIC_MAX_SIZE_FILTER          = 51;
	ACM_METRIC_DRIVER_SUPPORT           = 100;
	ACM_METRIC_DRIVER_PRIORITY          = 101;

type
	ACMDRIVERENUMCB = function(hadid : HACMDRIVERID; dwInstance : DWORD; fdwSupport : DWORD) : BOOL; stdcall;

function acmDriverEnum(fnCallback : ACMDRIVERENUMCB; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

const
	ACM_DRIVERENUMF_NOLOCAL     = $40000000;
	ACM_DRIVERENUMF_DISABLED    = $80000000;

function acmDriverID(hao : HACMOBJ; var phadid : HACMDRIVERID; fdwDriverID : DWORD) : MMRESULT; stdcall;

function acmDriverAddA(var phadid : HACMDRIVERID; hinstModule : THandle; lParam : LPARAM; dwPriority : DWORD; fdwAdd : DWORD) : MMRESULT; stdcall;

function acmDriverAddW(var phadid : HACMDRIVERID; hinstModule : THandle; lParam : LPARAM; dwPriority : DWORD; fdwAdd : DWORD) : MMRESULT; stdcall;

function acmDriverAdd(var phadid : HACMDRIVERID; hinstModule : THandle; lParam : LPARAM; dwPriority : DWORD; fdwAdd : DWORD) : MMRESULT; stdcall;

const
	ACM_DRIVERADDF_FUNCTION     = $00000003;  // lParam is a procedure
	ACM_DRIVERADDF_NOTIFYHWND   = $00000004;  // lParam is notify hwnd
	ACM_DRIVERADDF_TYPEMASK     = $00000007;  // driver type mask
	ACM_DRIVERADDF_LOCAL        = $00000000;  // is local to current task
	ACM_DRIVERADDF_GLOBAL       = $00000008;  // is global

type
	ACMDRIVERPROC = function(_0 : DWORD; _1 : HACMDRIVERID; _2 : UINT; _3 : LPARAM; _4 : LPARAM) : LRESULT; stdcall;
	LPACMDRIVERPROC = ^ACMDRIVERPROC;

function acmDriverRemove(hadid : HACMDRIVERID; fdwRemove : DWORD) : MMRESULT; stdcall;

function acmDriverOpen(var phad : HACMDRIVER; hadid : HACMDRIVERID; fdwOpen : DWORD) : MMRESULT; stdcall;

function acmDriverClose(had : HACMDRIVER; fdwClose : DWORD) : MMRESULT; stdcall;

function acmDriverMessage(had : HACMDRIVER; uMsg : UINT; lParam1 : LPARAM; lParam2 : LPARAM) : LRESULT; stdcall;

const
	ACMDM_USER                  = (DRV_USER + $0000);
	ACMDM_RESERVED_LOW          = (DRV_USER + $2000);
	ACMDM_RESERVED_HIGH         = (DRV_USER + $2FFF);

	ACMDM_BASE                  = ACMDM_RESERVED_LOW;

	ACMDM_DRIVER_ABOUT          = (ACMDM_BASE + 11);

function acmDriverPriority(hadid : HACMDRIVERID; dwPriority : DWORD; fdwPriority : DWORD) : MMRESULT; stdcall;

const
	ACM_DRIVERPRIORITYF_ENABLE      = $00000001;
	ACM_DRIVERPRIORITYF_DISABLE     = $00000002;
	ACM_DRIVERPRIORITYF_ABLEMASK    = $00000003;
	ACM_DRIVERPRIORITYF_BEGIN       = $00010000;
	ACM_DRIVERPRIORITYF_END         = $00020000;
	ACM_DRIVERPRIORITYF_DEFERMASK   = $00030000;

const
	ACMDRIVERDETAILS_SHORTNAME_CHARS    = 32;
	ACMDRIVERDETAILS_LONGNAME_CHARS     = 128;
	ACMDRIVERDETAILS_COPYRIGHT_CHARS    = 80;
	ACMDRIVERDETAILS_LICENSING_CHARS    = 128;
	ACMDRIVERDETAILS_FEATURES_CHARS     = 512;

type
	PACMDRIVERDETAILSA = ^TACMDRIVERDETAILSA;
	TACMDRIVERDETAILSA = packed record
		cbStruct      : DWORD;              // number of valid bytes in structure

		fccType       : FOURCC;             // compressor type 'audc'
		fccComp       : FOURCC;             // sub-type (not used; reserved)

		wMid          : WORD;               // manufacturer id
		wPid          : WORD;               // product id

		vdwACM        : DWORD;              // version of the ACM *compiled* for
		vdwDriver     : DWORD;              // version of the driver

		fdwSupport    : DWORD;              // misc. support flags
		cFormatTags   : DWORD;              // total unique format tags supported
		cFilterTags   : DWORD;              // total unique filter tags supported

		hicon         : HICON;              // handle to custom icon

		szShortName   : array[0..ACMDRIVERDETAILS_SHORTNAME_CHARS-1] of char;
		szLongName    : array[0..ACMDRIVERDETAILS_LONGNAME_CHARS-1] of char;
		szCopyright   : array[0..ACMDRIVERDETAILS_COPYRIGHT_CHARS-1] of char;
		szLicensing   : array[0..ACMDRIVERDETAILS_LICENSING_CHARS-1] of char;
		szFeatures    : array[0..ACMDRIVERDETAILS_FEATURES_CHARS-1] of char;
	end;

type
	PACMDRIVERDETAILSW = ^TACMDRIVERDETAILSW;
	TACMDRIVERDETAILSW = packed record
		cbStruct      : DWORD;              // number of valid bytes in structure

		fccType       : FOURCC;             // compressor type 'audc'
		fccComp       : FOURCC;             // sub-type (not used; reserved)

		wMid          : WORD;               // manufacturer id
		wPid          : WORD;               // product id

		vdwACM        : DWORD;              // version of the ACM *compiled* for
		vdwDriver     : DWORD;              // version of the driver

		fdwSupport    : DWORD;              // misc. support flags
		cFormatTags   : DWORD;              // total unique format tags supported
		cFilterTags   : DWORD;              // total unique filter tags supported

		hicon         : HICON;              // handle to custom icon

		szShortName   : array[0..ACMDRIVERDETAILS_SHORTNAME_CHARS-1] of wchar;
		szLongName    : array[0..ACMDRIVERDETAILS_LONGNAME_CHARS-1] of wchar;
		szCopyright   : array[0..ACMDRIVERDETAILS_COPYRIGHT_CHARS-1] of wchar;
		szLicensing   : array[0..ACMDRIVERDETAILS_LICENSING_CHARS-1] of wchar;
		szFeatures    : array[0..ACMDRIVERDETAILS_FEATURES_CHARS-1] of wchar;
	end;

type
	TACMDRIVERDETAILS       = TACMDRIVERDETAILSA;
	PACMDRIVERDETAILS       = PACMDRIVERDETAILSA;

const
	// ACMDRIVERDETAILS_FCCTYPE_AUDIOCODEC = mmioFOURCC('a', 'u', 'd', 'c');
	ACMDRIVERDETAILS_FCCCOMP_UNDEFINED  = 0;

function ACMDRIVERDETAILS_FCCTYPE_AUDIOCODE : FOURCC;

const
	ACMDRIVERDETAILS_SUPPORTF_CODEC     = $00000001;
	ACMDRIVERDETAILS_SUPPORTF_CONVERTER = $00000002;
	ACMDRIVERDETAILS_SUPPORTF_FILTER    = $00000004;
	ACMDRIVERDETAILS_SUPPORTF_HARDWARE  = $00000008;
	ACMDRIVERDETAILS_SUPPORTF_ASYNC     = $00000010;
	ACMDRIVERDETAILS_SUPPORTF_LOCAL     = $40000000;
	ACMDRIVERDETAILS_SUPPORTF_DISABLED  = $80000000;

function acmDriverDetailsA(hadid : HACMDRIVERID; var padd : TACMDRIVERDETAILSA; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmDriverDetailsW(hadid : HACMDRIVERID; var padd : TACMDRIVERDETAILSW; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmDriverDetails(hadid : HACMDRIVERID; var padd : TACMDRIVERDETAILS; fdwDetails : DWORD) : MMRESULT; stdcall;

const
	ACMFORMATTAGDETAILS_FORMATTAG_CHARS = 48;

type
	PACMFORMATTAGDETAILSA = ^TACMFORMATTAGDETAILSA;
	TACMFORMATTAGDETAILSA = packed record
		cbStruct         : DWORD;
		dwFormatTagIndex : DWORD;
		dwFormatTag      : DWORD;
		cbFormatSize     : DWORD;
		fdwSupport       : DWORD;
		cStandardFormats : DWORD;
		szFormatTag      : array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of char;
	end;

type
	PACMFORMATTAGDETAILSW = ^TACMFORMATTAGDETAILSW;
	TACMFORMATTAGDETAILSW = packed record
		cbStruct         : DWORD;
		dwFormatTagIndex : DWORD;
		dwFormatTag      : DWORD;
		cbFormatSize     : DWORD;
		fdwSupport       : DWORD;
		cStandardFormats : DWORD;
		szFormatTag      : array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of wchar;
	end;

type
	TACMFORMATTAGDETAILS = TACMFORMATTAGDETAILSA;
	PACMFORMATTAGDETAILS = PACMFORMATTAGDETAILSA;

function acmFormatTagDetailsA(had : HACMDRIVER; var paftd : TACMFORMATTAGDETAILSA; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmFormatTagDetailsW(had : HACMDRIVER; var paftd : TACMFORMATTAGDETAILSW; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmFormatTagDetails(had : HACMDRIVER; var paftd : TACMFORMATTAGDETAILS; fdwDetails : DWORD) : MMRESULT; stdcall;

const
	ACM_FORMATTAGDETAILSF_INDEX         = $00000000;
	ACM_FORMATTAGDETAILSF_FORMATTAG     = $00000001;
	ACM_FORMATTAGDETAILSF_LARGESTSIZE   = $00000002;
	ACM_FORMATTAGDETAILSF_QUERYMASK     = $0000000F;

type
	ACMFORMATTAGENUMCBA = function(hadid : HACMDRIVERID; const paftd : TACMFORMATTAGDETAILSA; dwInstance : DWORD; fdwSupport : DWORD) : BOOL; stdcall;

function acmFormatTagEnumA(had : HACMDRIVER; var paftd : TACMFORMATTAGDETAILSA; fnCallback : ACMFORMATTAGENUMCBA; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

type
	ACMFORMATTAGENUMCBW = function(hadid : HACMDRIVERID; const paftd : TACMFORMATTAGDETAILSW; dwInstance : DWORD; fdwSupport : DWORD) : BOOL; stdcall;

function acmFormatTagEnumW(had : HACMDRIVER; var paftd : TACMFORMATTAGDETAILSW; fnCallback : ACMFORMATTAGENUMCBW; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

type
	ACMFORMATTAGENUMCB = ACMFORMATTAGENUMCBA;

function acmFormatTagEnum(had : HACMDRIVER; var paftd : TACMFORMATTAGDETAILS; fnCallback : ACMFORMATTAGENUMCB; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

const
	ACMFORMATDETAILS_FORMAT_CHARS   = 128;

type
	PACMFORMATDETAILSA = ^TACMFORMATDETAILSA;
	TACMFORMATDETAILSA = packed record
		cbStruct      : DWORD;
		dwFormatIndex : DWORD;
		dwFormatTag   : DWORD;
		fdwSupport    : DWORD;
		pwfx          : PWAVEFORMATEX;
		cbwfx         : DWORD;
		szFormat      : array[0..ACMFORMATDETAILS_FORMAT_CHARS-1] of char;
	end;

type
	PACMFORMATDETAILSW = ^TACMFORMATDETAILSW;
	TACMFORMATDETAILSW = packed record
		cbStruct      : DWORD;
		dwFormatIndex : DWORD;
		dwFormatTag   : DWORD;
		fdwSupport    : DWORD;
		pwfx          : PWAVEFORMATEX;
		cbwfx         : DWORD;
		szFormat      : array[0..ACMFORMATDETAILS_FORMAT_CHARS-1] of wchar;
	end;

type
	PACMFORMATDETAILS = PACMFORMATDETAILSA;
	TACMFORMATDETAILS = TACMFORMATDETAILSA;

function acmFormatDetailsA(had : HACMDRIVER; var pafd : TACMFORMATDETAILSA; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmFormatDetailsW(had : HACMDRIVER; var pafd : TACMFORMATDETAILSW; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmFormatDetails(had : HACMDRIVER; var pafd : TACMFORMATDETAILS; fdwDetails : DWORD) : MMRESULT; stdcall;

const
	ACM_FORMATDETAILSF_INDEX        = $00000000;
	ACM_FORMATDETAILSF_FORMAT       = $00000001;
	ACM_FORMATDETAILSF_QUERYMASK    = $0000000F;

type
	ACMFORMATENUMCBA = function(hadid : HACMDRIVERID; const pafd : TACMFORMATDETAILSA; dwInstance : DWORD; fdwSupport : DWORD) : BOOL; stdcall;

function acmFormatEnumA(had : HACMDRIVER; var pafd : TACMFORMATDETAILSA; fnCallback : ACMFORMATENUMCBA; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

type
	ACMFORMATENUMCBW = function(hadid : HACMDRIVERID; const pafd : TACMFORMATDETAILSW; dwInstance : DWORD; fdwSupport : DWORD) : BOOL; stdcall;

function acmFormatEnumW(had : HACMDRIVER; var pafd : TACMFORMATDETAILSW; fnCallback : ACMFORMATENUMCBW; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

type
	ACMFORMATENUMCB = ACMFORMATENUMCBA;

function acmFormatEnum(had : HACMDRIVER; var pafd : TACMFORMATDETAILS; fnCallback : ACMFORMATENUMCB; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

const
	ACM_FORMATENUMF_WFORMATTAG       = $00010000;
	ACM_FORMATENUMF_NCHANNELS        = $00020000;
	ACM_FORMATENUMF_NSAMPLESPERSEC   = $00040000;
	ACM_FORMATENUMF_WBITSPERSAMPLE   = $00080000;
	ACM_FORMATENUMF_CONVERT          = $00100000;
	ACM_FORMATENUMF_SUGGEST          = $00200000;
	ACM_FORMATENUMF_HARDWARE         = $00400000;
	ACM_FORMATENUMF_INPUT            = $00800000;
	ACM_FORMATENUMF_OUTPUT           = $01000000;

function acmFormatSuggest(had : HACMDRIVER; const pwfxSrc : TWAVEFORMATEX; var pwfxDst : TWAVEFORMATEX; cbwfxDst : DWORD; fdwSuggest : DWORD) : MMRESULT; stdcall;

const
	ACM_FORMATSUGGESTF_WFORMATTAG       = $00010000;
	ACM_FORMATSUGGESTF_NCHANNELS        = $00020000;
	ACM_FORMATSUGGESTF_NSAMPLESPERSEC   = $00040000;
	ACM_FORMATSUGGESTF_WBITSPERSAMPLE   = $00080000;

	ACM_FORMATSUGGESTF_TYPEMASK         = $00FF0000;

const
	ACMHELPMSGSTRINGA       = 'acmchoose_help';
	ACMHELPMSGSTRINGW       = 'acmchoose_help';
	ACMHELPMSGCONTEXTMENUA  = 'acmchoose_contextmenu';
	ACMHELPMSGCONTEXTMENUW  = 'acmchoose_contextmenu';
	ACMHELPMSGCONTEXTHELPA  = 'acmchoose_contexthelp';
	ACMHELPMSGCONTEXTHELPW  = 'acmchoose_contexthelp';

const
	ACMHELPMSGSTRING        = ACMHELPMSGSTRINGA;
	ACMHELPMSGCONTEXTMENU   = ACMHELPMSGCONTEXTMENUA;
	ACMHELPMSGCONTEXTHELP   = ACMHELPMSGCONTEXTHELPA;

const
	MM_ACM_FORMATCHOOSE             = ($8000);

	FORMATCHOOSE_MESSAGE            = 0;
	FORMATCHOOSE_FORMATTAG_VERIFY   = (FORMATCHOOSE_MESSAGE+0);
	FORMATCHOOSE_FORMAT_VERIFY      = (FORMATCHOOSE_MESSAGE+1);
	FORMATCHOOSE_CUSTOM_VERIFY      = (FORMATCHOOSE_MESSAGE+2);

type
	ACMFORMATCHOOSEHOOKPROCA = function(hwnd : HWND; uMsg : UINT; wParam : WPARAM; lParam : LPARAM) : UINT; stdcall;

type
	ACMFORMATCHOOSEHOOKPROCW = function(hwnd : HWND; uMsg : UINT; wParam : WPARAM; lParam : LPARAM) : UINT; stdcall;

type
	ACMFORMATCHOOSEHOOKPROC = ACMFORMATCHOOSEHOOKPROCA;

type
	PACMFORMATCHOOSEA = ^TACMFORMATCHOOSEA;
	TACMFORMATCHOOSEA = packed record
		cbStruct        : DWORD           ;            // sizeof(ACMFORMATCHOOSE)
		fdwStyle        : DWORD           ;            // chooser style flags

		hwndOwner       : HWND            ;            // caller's window handle

		pwfx            : PWAVEFORMATEX   ;            // ptr to wfx buf to receive choice
		cbwfx           : DWORD           ;            // size of mem buf for pwfx
		pszTitle        : LPCSTR          ;            // dialog box title bar

		szFormatTag     : array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of char;
		szFormat        : array[0..ACMFORMATDETAILS_FORMAT_CHARS-1] of char;

		pszName         : LPSTR           ;            // custom name selection
		cchName         : DWORD           ;            // size in chars of mem buf for pszName

		fdwEnum         : DWORD           ;            // format enumeration restrictions
		pwfxEnum        : PWAVEFORMATEX   ;            // format describing restrictions

		hInstance       : THandle         ;            // app instance containing dlg template
		pszTemplateName : LPCSTR          ;            // custom template name
		lCustData       : LPARAM          ;            // data passed to hook fn.
		pfnHook         : ACMFORMATCHOOSEHOOKPROCA ;   // ptr to hook function
	end;

type
	PACMFORMATCHOOSEW = ^TACMFORMATCHOOSEW;
	TACMFORMATCHOOSEW = packed record
		cbStruct        : DWORD           ;            // sizeof(ACMFORMATCHOOSE)
		fdwStyle        : DWORD           ;            // chooser style flags

		hwndOwner       : HWND            ;            // caller's window handle

		pwfx            : PWAVEFORMATEX   ;            // ptr to wfx buf to receive choice
		cbwfx           : DWORD           ;            // size of mem buf for pwfx
		pszTitle        : LPCWSTR         ;            // dialog box title bar

		szFormatTag     : array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of wchar;
		szFormat        : array[0..ACMFORMATDETAILS_FORMAT_CHARS-1] of wchar;

		pszName         : LPWSTR          ;            // custom name selection
		cchName         : DWORD           ;            // size in chars of mem buf for pszName

		fdwEnum         : DWORD           ;            // format enumeration restrictions
		pwfxEnum        : PWAVEFORMATEX   ;            // format describing restrictions

		hInstance       : THandle         ;            // app instance containing dlg template
		pszTemplateName : LPCWSTR         ;            // custom template name
		lCustData       : LPARAM          ;            // data passed to hook fn.
		pfnHook         : ACMFORMATCHOOSEHOOKPROCW ;   // ptr to hook function
	end;

type
	TACMFORMATCHOOSE    = TACMFORMATCHOOSEA;
	PACMFORMATCHOOSE    = PACMFORMATCHOOSEA;

const
	ACMFORMATCHOOSE_STYLEF_SHOWHELP              = $00000004;
	ACMFORMATCHOOSE_STYLEF_ENABLEHOOK            = $00000008;
	ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATE        = $00000010;
	ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATEHANDLE  = $00000020;
	ACMFORMATCHOOSE_STYLEF_INITTOWFXSTRUCT       = $00000040;
	ACMFORMATCHOOSE_STYLEF_CONTEXTHELP           = $00000080;

function acmFormatChooseA(var pafmtc : TACMFORMATCHOOSEA) : MMRESULT; stdcall;

function acmFormatChooseW(var pafmtc : TACMFORMATCHOOSEW) : MMRESULT; stdcall;

function acmFormatChoose(var pafmtc : TACMFORMATCHOOSE) : MMRESULT; stdcall;

const
	ACMFILTERTAGDETAILS_FILTERTAG_CHARS = 48;

type
	PACMFILTERTAGDETAILSA = ^TACMFILTERTAGDETAILSA;
	TACMFILTERTAGDETAILSA = packed record
		cbStruct         : DWORD           ;
		dwFilterTagIndex : DWORD           ;
		dwFilterTag      : DWORD           ;
		cbFilterSize     : DWORD           ;
		fdwSupport       : DWORD           ;
		cStandardFilters : DWORD           ;
		szFilterTag      : array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of char;
	end;

type
	PACMFILTERTAGDETAILSW = ^TACMFILTERTAGDETAILSW;
	TACMFILTERTAGDETAILSW = packed record
		cbStruct         : DWORD           ;
		dwFilterTagIndex : DWORD           ;
		dwFilterTag      : DWORD           ;
		cbFilterSize     : DWORD           ;
		fdwSupport       : DWORD           ;
		cStandardFilters : DWORD           ;
		szFilterTag      : array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of wchar;
	end;

type
	PACMFILTERTAGDETAILS = PACMFILTERTAGDETAILSA;
	TACMFILTERTAGDETAILS = TACMFILTERTAGDETAILSA;

function acmFilterTagDetailsA(had : HACMDRIVER; var paftd : TACMFILTERTAGDETAILSA; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmFilterTagDetailsW(had : HACMDRIVER; var paftd : TACMFILTERTAGDETAILSW; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmFilterTagDetails(had : HACMDRIVER; var paftd : TACMFILTERTAGDETAILS; fdwDetails : DWORD) : MMRESULT; stdcall;

const
	ACM_FILTERTAGDETAILSF_INDEX         = $00000000;
	ACM_FILTERTAGDETAILSF_FILTERTAG     = $00000001;
	ACM_FILTERTAGDETAILSF_LARGESTSIZE   = $00000002;
	ACM_FILTERTAGDETAILSF_QUERYMASK     = $0000000F;

type
	ACMFILTERTAGENUMCBA = function(hadid : HACMDRIVERID; const paftd : TACMFILTERTAGDETAILSA; dwInstance : DWORD; fdwSupport : DWORD) : BOOL; stdcall;

function acmFilterTagEnumA(had : HACMDRIVER; var paftd : TACMFILTERTAGDETAILSA; fnCallback : ACMFILTERTAGENUMCBA; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

type
	ACMFILTERTAGENUMCBW = function(hadid : HACMDRIVERID; const paftd : TACMFILTERTAGDETAILSW; dwInstance : DWORD; fdwSupport : DWORD) : BOOL; stdcall;

function acmFilterTagEnumW(had : HACMDRIVER; var paftd : TACMFILTERTAGDETAILSW; fnCallback : ACMFILTERTAGENUMCBW; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

type
	ACMFILTERTAGENUMCB = ACMFILTERTAGENUMCBA;

function acmFilterTagEnum(had : HACMDRIVER; var paftd : TACMFILTERTAGDETAILS; fnCallback : ACMFILTERTAGENUMCB; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

const
	ACMFILTERDETAILS_FILTER_CHARS   = 128;

type
	PACMFILTERDETAILSA = ^TACMFILTERDETAILSA;
	TACMFILTERDETAILSA = packed record
		cbStruct      : DWORD           ;
		dwFilterIndex : DWORD           ;
		dwFilterTag   : DWORD           ;
		fdwSupport    : DWORD           ;
		pwfltr        : PWAVEFILTER     ;
		cbwfltr       : DWORD           ;
		szFilter      : array[0..ACMFILTERDETAILS_FILTER_CHARS-1] of char;
	end;

type
	PACMFILTERDETAILSW = ^TACMFILTERDETAILSW;
	TACMFILTERDETAILSW = packed record
		cbStruct      : DWORD           ;
		dwFilterIndex : DWORD           ;
		dwFilterTag   : DWORD           ;
		fdwSupport    : DWORD           ;
		pwfltr        : PWAVEFILTER     ;
		cbwfltr       : DWORD           ;
		szFilter      : array[0..ACMFILTERDETAILS_FILTER_CHARS-1] of wchar;
	end;

type
	PACMFILTERDETAILS = PACMFILTERDETAILSA;
	TACMFILTERDETAILS = TACMFILTERDETAILSA;

function acmFilterDetailsA(had : HACMDRIVER; var pafd : TACMFILTERDETAILSA; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmFilterDetailsW(had : HACMDRIVER; var pafd : TACMFILTERDETAILSW; fdwDetails : DWORD) : MMRESULT; stdcall;

function acmFilterDetails(had : HACMDRIVER; var pafd : TACMFILTERDETAILS; fdwDetails : DWORD) : MMRESULT; stdcall;

const
	ACM_FILTERDETAILSF_INDEX        = $00000000;
	ACM_FILTERDETAILSF_FILTER       = $00000001;
	ACM_FILTERDETAILSF_QUERYMASK    = $0000000F;

type
	ACMFILTERENUMCBA = function(hadid : HACMDRIVERID; const pafd : TACMFILTERDETAILSA; dwInstance : DWORD; fdwSupport : DWORD) : BOOL; stdcall;

function acmFilterEnumA(had : HACMDRIVER; var pafd : TACMFILTERDETAILSA; fnCallback : ACMFILTERENUMCBA; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

type
	ACMFILTERENUMCBW = function(hadid : HACMDRIVERID; const pafd : TACMFILTERDETAILSW; dwInstance : DWORD; fdwSupport : DWORD) : BOOL; stdcall;

function acmFilterEnumW(had : HACMDRIVER; var pafd : TACMFILTERDETAILSW; fnCallback : ACMFILTERENUMCBW; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

type
	ACMFILTERENUMCB = ACMFILTERENUMCBA;

function acmFilterEnum(had : HACMDRIVER; var pafd : TACMFILTERDETAILS; fnCallback : ACMFILTERENUMCB; dwInstance : DWORD; fdwEnum : DWORD) : MMRESULT; stdcall;

const
	ACM_FILTERENUMF_DWFILTERTAG         = $00010000;

const
	MM_ACM_FILTERCHOOSE             = ($8000);

	FILTERCHOOSE_MESSAGE            = 0;
	FILTERCHOOSE_FILTERTAG_VERIFY   = (FILTERCHOOSE_MESSAGE+0);
	FILTERCHOOSE_FILTER_VERIFY      = (FILTERCHOOSE_MESSAGE+1);
	FILTERCHOOSE_CUSTOM_VERIFY      = (FILTERCHOOSE_MESSAGE+2);

type
	ACMFILTERCHOOSEHOOKPROCA = function(hwnd : HWND; uMsg : UINT; wParam : WPARAM; lParam : LPARAM) : UINT; stdcall;

type
	ACMFILTERCHOOSEHOOKPROCW = function(hwnd : HWND; uMsg : UINT; wParam : WPARAM; lParam : LPARAM) : UINT; stdcall;

type
	ACMFILTERCHOOSEHOOKPROC = ACMFILTERCHOOSEHOOKPROCA;

type
	PACMFILTERCHOOSEA = ^TACMFILTERCHOOSEA;
	TACMFILTERCHOOSEA = packed record
		cbStruct        : DWORD           ;            // sizeof(ACMFILTERCHOOSE)
		fdwStyle        : DWORD           ;            // chooser style flags

		hwndOwner       : HWND            ;            // caller's window handle

		pwfltr          : PWAVEFILTER     ;            // ptr to wfltr buf to receive choice
		cbwfltr         : DWORD           ;            // size of mem buf for pwfltr

		pszTitle        : LPCSTR          ;

		szFilterTag     : array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of char;
		szFilter        : array[0..ACMFILTERDETAILS_FILTER_CHARS-1] of char;
		pszName         : LPSTR           ;            // custom name selection
		cchName         : DWORD           ;            // size in chars of mem buf for pszName

		fdwEnum         : DWORD           ;            // filter enumeration restrictions
		pwfltrEnum      : PWAVEFILTER     ;            // filter describing restrictions

		hInstance       : THandle         ;            // app instance containing dlg template
		pszTemplateName : LPCSTR          ;            // custom template name
		lCustData       : LPARAM          ;            // data passed to hook fn.
		pfnHook         : ACMFILTERCHOOSEHOOKPROCA ;   // ptr to hook function
	end;

type
	PACMFILTERCHOOSEW = ^TACMFILTERCHOOSEW;
	TACMFILTERCHOOSEW = packed record
		cbStruct        : DWORD           ;            // sizeof(ACMFILTERCHOOSE)
		fdwStyle        : DWORD           ;            // chooser style flags

		hwndOwner       : HWND            ;            // caller's window handle

		pwfltr          : PWAVEFILTER     ;            // ptr to wfltr buf to receive choice
		cbwfltr         : DWORD           ;            // size of mem buf for pwfltr

		pszTitle        : LPCWSTR         ;

		szFilterTag     : array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of wchar;
		szFilter        : array[0..ACMFILTERDETAILS_FILTER_CHARS-1] of wchar;
		pszName         : LPWSTR          ;            // custom name selection
		cchName         : DWORD           ;            // size in chars of mem buf for pszName

		fdwEnum         : DWORD           ;            // filter enumeration restrictions
		pwfltrEnum      : PWAVEFILTER     ;            // filter describing restrictions

		hInstance       : THandle         ;            // app instance containing dlg template
		pszTemplateName : LPCWSTR         ;            // custom template name
		lCustData       : LPARAM          ;            // data passed to hook fn.
		pfnHook         : ACMFILTERCHOOSEHOOKPROCW ;   // ptr to hook function
	end;

type
	PACMFILTERCHOOSE = PACMFILTERCHOOSEA;
	TACMFILTERCHOOSE = TACMFILTERCHOOSEA;

const
	ACMFILTERCHOOSE_STYLEF_SHOWHELP              = $00000004;
	ACMFILTERCHOOSE_STYLEF_ENABLEHOOK            = $00000008;
	ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATE        = $00000010;
	ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATEHANDLE  = $00000020;
	ACMFILTERCHOOSE_STYLEF_INITTOFILTERSTRUCT    = $00000040;
	ACMFILTERCHOOSE_STYLEF_CONTEXTHELP           = $00000080;

function acmFilterChooseA(var pafltrc : TACMFILTERCHOOSEA) : MMRESULT; stdcall;

function acmFilterChooseW(var pafltrc : TACMFILTERCHOOSEW) : MMRESULT; stdcall;

function acmFilterChoose(var pafltrc : TACMFILTERCHOOSE) : MMRESULT; stdcall;

type
	PACMSTREAMHEADER = ^TACMSTREAMHEADER;
	TACMSTREAMHEADER = packed record
		cbStruct         : DWORD           ;              // sizeof(ACMSTREAMHEADER)
		fdwStatus        : DWORD           ;              // ACMSTREAMHEADER_STATUSF_*
		dwUser           : DWORD_PTR       ;              // user instance data for hdr
		pbSrc            : PBYTE           ;
		cbSrcLength      : DWORD           ;
		cbSrcLengthUsed  : DWORD           ;
		dwSrcUser        : DWORD_PTR       ;              // user instance data for src
		pbDst            : PBYTE           ;
		cbDstLength      : DWORD           ;
		cbDstLengthUsed  : DWORD           ;
		dwDstUser        : DWORD_PTR       ;              // user instance data for dst
		dwReservedDriver : array [0..9] of DWORD_PTR;         // driver reserved work space
(*
typedef struct {
  DWORD     cbStruct;
  DWORD     fdwStatus;
  DWORD_PTR dwUser;
  LPBYTE    pbSrc;
  DWORD     cbSrcLength;
  DWORD     cbSrcLengthUsed;
  DWORD_PTR dwSrcUser;
  LPBYTE    pbDst;
  DWORD     cbDstLength;
  DWORD     cbDstLengthUsed;
  DWORD_PTR dwDstUser;
  DWORD     dwReservedDriver[10];
} ACMSTREAMHEADER;
*)


	end;

const
	ACMSTREAMHEADER_STATUSF_DONE     = $00010000;
	ACMSTREAMHEADER_STATUSF_PREPARED = $00020000;
	ACMSTREAMHEADER_STATUSF_INQUEUE  = $00100000;

function acmStreamOpen(phas : PHACMSTREAM; had : HACMDRIVER; const pwfxSrc : PWAVEFORMATEX; const pwfxdst : PWAVEFORMATEX; pwfltr : PWAVEFILTER; dwCallback : PDWORD; dwInstance : PDWORD; fdwOpen : DWORD) : MMRESULT; stdcall;

const
	ACM_STREAMOPENF_QUERY           = $00000001;
	ACM_STREAMOPENF_ASYNC           = $00000002;
	ACM_STREAMOPENF_NONREALTIME     = $00000004;

function acmStreamClose(has : HACMSTREAM; fdwClose : DWORD) : MMRESULT; stdcall;

function acmStreamSize(has : HACMSTREAM; cbInput : DWORD; var pdwOutputByte : DWORD; fdwSize : DWORD) : MMRESULT; stdcall;

const
	ACM_STREAMSIZEF_SOURCE          = $00000000;
	ACM_STREAMSIZEF_DESTINATION     = $00000001;
	ACM_STREAMSIZEF_QUERYMASK       = $0000000F;

function acmStreamReset(has : HACMSTREAM; fdwReset : DWORD) : MMRESULT; stdcall;

function acmStreamMessage(has : HACMSTREAM; uMsg : UINT; lParam1 : LPARAM; lParam2 : LPARAM) : MMRESULT; stdcall;

function acmStreamConvert(has : HACMSTREAM; var pash : TACMSTREAMHEADER; fdwConvert : DWORD) : MMRESULT; stdcall;

const
	ACM_STREAMCONVERTF_BLOCKALIGN   = $00000004;
	ACM_STREAMCONVERTF_START        = $00000010;
	ACM_STREAMCONVERTF_END          = $00000020;

function acmStreamPrepareHeader(has : HACMSTREAM; pash : PACMSTREAMHEADER; fdwPrepare : DWORD) : MMRESULT; stdcall;

function acmStreamUnprepareHeader(has : HACMSTREAM; pash : PACMSTREAMHEADER; fdwUnprepare : DWORD) : MMRESULT; stdcall;

implementation

const
	msacm32 = 'msacm32.dll';

// acm

function acmGetVersion; external msacm32 name 'acmGetVersion';
function acmMetrics; external msacm32 name 'acmMetrics';

// acmDriver

function acmDriverEnum; external msacm32 name 'acmDriverEnum';
function acmDriverID; external msacm32 name 'acmDriverID';

function acmDriverAddA; external msacm32 name 'acmDriverAddA';
function acmDriverAddW; external msacm32 name 'acmDriverAddW';
function acmDriverAdd; external msacm32 name 'acmDriverAddA';

function acmDriverRemove; external msacm32 name 'acmDriverRemove';
function acmDriverOpen; external msacm32 name 'acmDriverOpen';
function acmDriverClose; external msacm32 name 'acmDriverClose';
function acmDriverMessage; external msacm32 name 'acmDriverMessage';
function acmDriverPriority; external msacm32 name 'acmDriverPriority';

function ACMDRIVERDETAILS_FCCTYPE_AUDIOCODE : FOURCC;
begin
	Result := MMSystem.mmioStringToFOURCC('audc', 0);
end;

function acmDriverDetailsA; external msacm32 name 'acmDriverDetailsA';
function acmDriverDetailsW; external msacm32 name 'acmDriverDetailsW';
function acmDriverDetails; external msacm32 name 'acmDriverDetailsA';

// acmFormat

function acmFormatTagDetailsA; external msacm32 name 'acmFormatTagDetailsA';
function acmFormatTagDetailsW; external msacm32 name 'acmFormatTagDetailsW';
function acmFormatTagDetails; external msacm32 name 'acmFormatTagDetailsA';

function acmFormatTagEnumA; external msacm32 name 'acmFormatTagEnumA';
function acmFormatTagEnumW; external msacm32 name 'acmFormatTagEnumW';
function acmFormatTagEnum; external msacm32 name 'acmFormatTagEnumA';

function acmFormatDetailsA; external msacm32 name 'acmFormatDetailsA';
function acmFormatDetailsW; external msacm32 name 'acmFormatDetailsW';
function acmFormatDetails; external msacm32 name 'acmFormatDetailsA';

function acmFormatEnumA; external msacm32 name 'acmFormatEnumA';
function acmFormatEnumW; external msacm32 name 'acmFormatEnumW';
function acmFormatEnum; external msacm32 name 'acmFormatEnumA';

function acmFormatSuggest; external msacm32 name 'acmFormatSuggest';

function acmFormatChooseA; external msacm32 name 'acmFormatChooseA';
function acmFormatChooseW; external msacm32 name 'acmFormatChooseW';
function acmFormatChoose; external msacm32 name 'acmFormatChooseA';

// acmFilter

function acmFilterTagDetailsA; external msacm32 name 'acmFilterTagDetailsA';
function acmFilterTagDetailsW; external msacm32 name 'acmFilterTagDetailsW';
function acmFilterTagDetails; external msacm32 name 'acmFilterTagDetailsA';

function acmFilterTagEnumA; external msacm32 name 'acmFilterTagEnumA';
function acmFilterTagEnumW; external msacm32 name 'acmFilterTagEnumW';
function acmFilterTagEnum; external msacm32 name 'acmFilterTagEnumA';

function acmFilterDetailsA; external msacm32 name 'acmFilterDetailsA';
function acmFilterDetailsW; external msacm32 name 'acmFilterDetailsW';
function acmFilterDetails; external msacm32 name 'acmFilterDetailsA';

function acmFilterEnumA; external msacm32 name 'acmFilterEnumA';
function acmFilterEnumW; external msacm32 name 'acmFilterEnumW';
function acmFilterEnum; external msacm32 name 'acmFilterEnumA';

function acmFilterChooseA; external msacm32 name 'acmFilterChooseA';
function acmFilterChooseW; external msacm32 name 'acmFilterChooseW';
function acmFilterChoose; external msacm32 name 'acmFilterChooseA';

// acmStream

function acmStreamOpen; external msacm32 name 'acmStreamOpen';
function acmStreamClose; external msacm32 name 'acmStreamClose';
function acmStreamSize; external msacm32 name 'acmStreamSize';
function acmStreamReset; external msacm32 name 'acmStreamReset';
function acmStreamMessage; external msacm32 name 'acmStreamMessage';
function acmStreamConvert; external msacm32 name 'acmStreamConvert';
function acmStreamPrepareHeader; external msacm32 name 'acmStreamPrepareHeader';
function acmStreamUnprepareHeader; external msacm32 name 'acmStreamUnprepareHeader';

end.

