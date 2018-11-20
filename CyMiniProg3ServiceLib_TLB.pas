unit CyMiniProg3ServiceLib_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 6/25/2018 10:24:41 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files (x86)\Cypress\Programmer\Service\CyMiniProg3Service.exe (1)
// LIBID: {EFA2CD39-2352-4165-A71F-83858CA0C2D5}
// LCID: 0
// Helpfile: 
// HelpString: CyMiniProg3Service 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Hint: Enum Member 'ON' of '__MIDL_ICyMiniProg3_v1_0003' changed to 'ON_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  CyMiniProg3ServiceLibMajorVersion = 1;
  CyMiniProg3ServiceLibMinorVersion = 0;

  LIBID_CyMiniProg3ServiceLib: TGUID = '{EFA2CD39-2352-4165-A71F-83858CA0C2D5}';

  IID_ICyMiniProg3_v1: TGUID = '{D55C2A73-7FCA-47B8-8076-B9EE22CE6745}';
  IID_ICyMiniProg3_v2: TGUID = '{8093A3D4-CF1A-4092-9620-57A5CDCCE4D0}';
  IID_ICyMiniProg3_v3: TGUID = '{C1179083-5983-4C08-8BB7-3AD21A61BA77}';
  IID_ICyMiniProg3_v4: TGUID = '{F1C66846-910A-46EA-86E1-C96C54A074BE}';
  IID_ICyMiniProg3_v5: TGUID = '{101BE394-DC29-43DA-9764-0E184965ADE6}';
  CLASS_CyMiniProg3: TGUID = '{DE39E772-2776-4264-AEEA-44098B4AE944}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum __MIDL_ICyMiniProg3_v1_0009
type
  __MIDL_ICyMiniProg3_v1_0009 = TOleEnum;
const
  SUCCESS = $00000000;
  IN_USE = $00000001;
  NOT_ACQUIRED = $00000002;
  INVALID_DEVICE = $00000003;
  INVALID_MODE = $00000004;
  INVALID_PARAM = $00000005;
  MP3_ERROR = $00000006;
  UNKNOWN = $00000007;

// Constants for enum __MIDL_ICyMiniProg3_v1_0002
type
  __MIDL_ICyMiniProg3_v1_0002 = TOleEnum;
const
  LED_1_RED = $00000000;
  LED_2_RED = $00000001;
  LED_3_GREEN = $00000002;
  LED_4_YELLOW = $00000003;

// Constants for enum __MIDL_ICyMiniProg3_v1_0003
type
  __MIDL_ICyMiniProg3_v1_0003 = TOleEnum;
const
  OFF = $00000000;
  FLASH1 = $00000001;
  FLASH2 = $00000002;
  ON_ = $00000003;

// Constants for enum __MIDL_ICyMiniProg3_v1_0004
type
  __MIDL_ICyMiniProg3_v1_0004 = TOleEnum;
const
  ID_NORMAL = $00000000;
  ID_CHASER = $00000001;
  ID_ON = $00000002;
  ID_OFF = $00000003;

// Constants for enum __MIDL_ICyMiniProg3_v1_0007
type
  __MIDL_ICyMiniProg3_v1_0007 = TOleEnum;
const
  EP0 = $00000000;
  EP1 = $00000001;
  EP2 = $00000002;
  EP4 = $00000004;
  EP6 = $00000006;

// Constants for enum __MIDL_ICyMiniProg3_v1_0006
type
  __MIDL_ICyMiniProg3_v1_0006 = TOleEnum;
const
  FREQ_A_48_0 = $00000000;
  FREQ_A_24_0 = $00000004;
  FREQ_A_16_0 = $00000010;
  FREQ_A_12_0 = $00000084;
  FREQ_A_08_0 = $00000090;
  FREQ_A_06_0 = $00000060;
  FREQ_A_03_2 = $00000018;
  FREQ_A_03_0 = $000000E0;
  FREQ_A_01_6 = $00000098;
  FREQ_A_01_5 = $000000C0;
  FREQ_B_24_0 = $00000001;
  FREQ_B_12_0 = $00000005;
  FREQ_B_08_0 = $00000011;
  FREQ_B_06_0 = $00000061;
  FREQ_B_04_0 = $00000091;
  FREQ_B_03_0 = $00000041;
  FREQ_B_01_6 = $00000019;
  FREQ_B_01_5 = $000000C1;
  FREQ_B_00_8 = $00000099;
  FREQ_C_06_0 = $00000062;
  FREQ_C_03_0 = $00000042;
  FREQ_C_01_5 = $000000C2;
  FREQ_C_00_4 = $0000001A;
  FREQ_C_00_2 = $0000009A;

// Constants for enum __MIDL_ICyMiniProg3_v1_0001
type
  __MIDL_ICyMiniProg3_v1_0001 = TOleEnum;
const
  IDLE = $00000000;
  JTAG = $00000001;
  I2C = $00000002;
  SWD = $00000004;
  SWV = $00000008;
  SWD_SWV = $0000000C;
  ISSP = $00000010;
  ETM = $00000020;
  ISSP_INT = $00000110;

// Constants for enum __MIDL_ICyMiniProg3_v1_0005
type
  __MIDL_ICyMiniProg3_v1_0005 = TOleEnum;
const
  VTARG_1_8 = $0000000E;
  VTARG_2_5 = $0000000D;
  VTARG_3_3 = $0000000B;
  VTARG_5_0 = $00000007;

// Constants for enum __MIDL_ICyMiniProg3_v1_0008
type
  __MIDL_ICyMiniProg3_v1_0008 = TOleEnum;
const
  RESET = $00000000;
  POWER_CYCLE = $00000001;
  VOLTAGE_SENSE = $00000002;

// Constants for enum __MIDL_ICyMiniProg3_v1_0010
type
  __MIDL_ICyMiniProg3_v1_0010 = TOleEnum;
const
  PSoC3_TEST = $00000000;
  PSoC3_DEBUG = $00000001;
  M0S8 = $00000002;
  ALL = $000000FF;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ICyMiniProg3_v1 = interface;
  ICyMiniProg3_v1Disp = dispinterface;
  ICyMiniProg3_v2 = interface;
  ICyMiniProg3_v2Disp = dispinterface;
  ICyMiniProg3_v3 = interface;
  ICyMiniProg3_v3Disp = dispinterface;
  ICyMiniProg3_v4 = interface;
  ICyMiniProg3_v4Disp = dispinterface;
  ICyMiniProg3_v5 = interface;
  ICyMiniProg3_v5Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CyMiniProg3 = ICyMiniProg3_v5;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//

  CyErrorCode = __MIDL_ICyMiniProg3_v1_0009; 
  CyLed = __MIDL_ICyMiniProg3_v1_0002; 
  CyLedState = __MIDL_ICyMiniProg3_v1_0003; 
  CyLedIdentifyState = __MIDL_ICyMiniProg3_v1_0004; 

{$ALIGN 2}
  CyMP3Status = record
    RESULT: Byte;
    VTARG_SET: Byte;
    VTARG_MEASURED: Smallint;
    ACTIVE_INTERFACE: Byte;
    ACTIVE_PORT: Byte;
    MP3_VERSION: Smallint;
    PSOC_VERSION: Smallint;
    FPGA_VERSION: Byte;
    BOOTLOADER_VERSION: Smallint;
    MP3_FLAGS_FPGA_POWER_STATE: Byte;
    MP3_FLAGS_SW_VALID: Byte;
    MP3_FLAGS_VTARG_FAULT: Byte;
    MP3_FLAGS_VTARG_ON: Byte;
    MP3_FLAGS_VCC1_3_ON: Byte;
    MP3_FLAGS_FPGA_MBX_ERR: Byte;
    MP3_FLAGS_PSOC_I2C_ERR: Byte;
    CLOCK_FREQUENCY: Byte;
  end;

  CyMiniProg3EndPoint = __MIDL_ICyMiniProg3_v1_0007; 
  CyJtagClock = __MIDL_ICyMiniProg3_v1_0006; 
  CyTransferMode = __MIDL_ICyMiniProg3_v1_0001; 
  CyVTargVoltage = __MIDL_ICyMiniProg3_v1_0005; 
  CyProgrammingMode = __MIDL_ICyMiniProg3_v1_0008; 
  CyAcquireTarget = __MIDL_ICyMiniProg3_v1_0010; 

// *********************************************************************//
// Interface: ICyMiniProg3_v1
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D55C2A73-7FCA-47B8-8076-B9EE22CE6745}
// *********************************************************************//
  ICyMiniProg3_v1 = interface(IDispatch)
    ['{D55C2A73-7FCA-47B8-8076-B9EE22CE6745}']
    function GetVersion: Integer; safecall;
    function GetDevicesChangedLocation: WideString; safecall;
    function EnumerateMiniProgs(out miniProg3s: PSafeArray): CyErrorCode; safecall;
    function EnumerateDevices(const miniProg3: WideString; out devices: PSafeArray): CyErrorCode; safecall;
    procedure SetLedState(const miniProg3: WideString; led: CyLed; state: CyLedState; 
                          out error: CyErrorCode); safecall;
    procedure VisualIdentify(const miniProg3: WideString; state: CyLedIdentifyState; 
                             out error: CyErrorCode); safecall;
    function GetMiniProgStatus(const miniProg3: WideString; out status: CyMP3Status): CyErrorCode; safecall;
    function SetTimeOut(const miniProg3: WideString; endPoint: CyMiniProg3EndPoint; 
                        timeout: Integer; ownerPID: Integer): CyErrorCode; safecall;
    function SetJtagClock(const miniProg3: WideString; clock: CyJtagClock; ownerPID: Integer): CyErrorCode; safecall;
    function SetMode(const miniProg3: WideString; mode: CyTransferMode; port10N5: Shortint; 
                     ownerPID: Integer): CyErrorCode; safecall;
    function SetVTarg(const miniProg3: WideString; voltage: CyVTargVoltage; 
                      supplyVoltage: Shortint; ownerPID: Integer): CyErrorCode; safecall;
    function AcquireDevice(const miniProg3: WideString; deviceNum: Integer; ownerPID: Integer; 
                           out mp3Handle: Int64): CyErrorCode; safecall;
    function ReleaseDevice(mp3Handle: Int64): CyErrorCode; safecall;
    function SendJtagPacket(tdi_tms: PSafeArray; read: Shortint; mp3Handle: Int64; 
                            out RESULT: PSafeArray): CyErrorCode; safecall;
    function SendSwdPacket(data: PSafeArray; mp3Handle: Int64; out RESULT: PSafeArray): CyErrorCode; safecall;
    function RequestSwvPackets(mp3Handle: Int64; out RESULT: PSafeArray): CyErrorCode; safecall;
    function BulkTransfer(endPoint: CyMiniProg3EndPoint; data: PSafeArray; readNwrite: Shortint; 
                          mp3Handle: Int64; out RESULT: PSafeArray): CyErrorCode; safecall;
    function PortAcquire(const miniProg3: WideString; mode: CyProgrammingMode; 
                         portAcquireKey: Integer; writeTestKey: Shortint; testModeAddress: Integer; 
                         testModeKey: Integer; retryLimit: Integer): CyErrorCode; safecall;
    function GetPortAcquireStatus(const miniProg3: WideString; abort: Shortint; 
                                  out acquireStatus: Integer): CyErrorCode; safecall;
    function ToggleXres(const miniProg3: WideString; active: Shortint): CyErrorCode; safecall;
    function SetupSWV(const miniProg3: WideString; targetFreq: Integer; manchesterNuart: Shortint; 
                      commitTime: Smallint): CyErrorCode; safecall;
    function RequestEtmPackets(mp3Handle: Int64; out RESULT: PSafeArray): CyErrorCode; safecall;
  end;

// *********************************************************************//
// DispIntf:  ICyMiniProg3_v1Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D55C2A73-7FCA-47B8-8076-B9EE22CE6745}
// *********************************************************************//
  ICyMiniProg3_v1Disp = dispinterface
    ['{D55C2A73-7FCA-47B8-8076-B9EE22CE6745}']
    function GetVersion: Integer; dispid 1;
    function GetDevicesChangedLocation: WideString; dispid 2;
    function EnumerateMiniProgs(out miniProg3s: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 3;
    function EnumerateDevices(const miniProg3: WideString; 
                              out devices: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 4;
    procedure SetLedState(const miniProg3: WideString; led: CyLed; state: CyLedState; 
                          out error: CyErrorCode); dispid 5;
    procedure VisualIdentify(const miniProg3: WideString; state: CyLedIdentifyState; 
                             out error: CyErrorCode); dispid 6;
    function GetMiniProgStatus(const miniProg3: WideString; 
                               out status: {NOT_OLEAUTO(CyMP3Status)}OleVariant): CyErrorCode; dispid 7;
    function SetTimeOut(const miniProg3: WideString; endPoint: CyMiniProg3EndPoint; 
                        timeout: Integer; ownerPID: Integer): CyErrorCode; dispid 8;
    function SetJtagClock(const miniProg3: WideString; clock: CyJtagClock; ownerPID: Integer): CyErrorCode; dispid 10;
    function SetMode(const miniProg3: WideString; mode: CyTransferMode; port10N5: Shortint; 
                     ownerPID: Integer): CyErrorCode; dispid 11;
    function SetVTarg(const miniProg3: WideString; voltage: CyVTargVoltage; 
                      supplyVoltage: Shortint; ownerPID: Integer): CyErrorCode; dispid 12;
    function AcquireDevice(const miniProg3: WideString; deviceNum: Integer; ownerPID: Integer; 
                           out mp3Handle: Int64): CyErrorCode; dispid 13;
    function ReleaseDevice(mp3Handle: Int64): CyErrorCode; dispid 14;
    function SendJtagPacket(tdi_tms: {NOT_OLEAUTO(PSafeArray)}OleVariant; read: Shortint; 
                            mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 15;
    function SendSwdPacket(data: {NOT_OLEAUTO(PSafeArray)}OleVariant; mp3Handle: Int64; 
                           out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 16;
    function RequestSwvPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 17;
    function BulkTransfer(endPoint: CyMiniProg3EndPoint; data: {NOT_OLEAUTO(PSafeArray)}OleVariant; 
                          readNwrite: Shortint; mp3Handle: Int64; 
                          out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 18;
    function PortAcquire(const miniProg3: WideString; mode: CyProgrammingMode; 
                         portAcquireKey: Integer; writeTestKey: Shortint; testModeAddress: Integer; 
                         testModeKey: Integer; retryLimit: Integer): CyErrorCode; dispid 20;
    function GetPortAcquireStatus(const miniProg3: WideString; abort: Shortint; 
                                  out acquireStatus: Integer): CyErrorCode; dispid 21;
    function ToggleXres(const miniProg3: WideString; active: Shortint): CyErrorCode; dispid 22;
    function SetupSWV(const miniProg3: WideString; targetFreq: Integer; manchesterNuart: Shortint; 
                      commitTime: Smallint): CyErrorCode; dispid 23;
    function RequestEtmPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 24;
  end;

// *********************************************************************//
// Interface: ICyMiniProg3_v2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8093A3D4-CF1A-4092-9620-57A5CDCCE4D0}
// *********************************************************************//
  ICyMiniProg3_v2 = interface(ICyMiniProg3_v1)
    ['{8093A3D4-CF1A-4092-9620-57A5CDCCE4D0}']
    function SendI2CPacket(data: PSafeArray; mp3Handle: Int64; restartLocation: Integer; 
                           out RESULT: PSafeArray): CyErrorCode; safecall;
  end;

// *********************************************************************//
// DispIntf:  ICyMiniProg3_v2Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8093A3D4-CF1A-4092-9620-57A5CDCCE4D0}
// *********************************************************************//
  ICyMiniProg3_v2Disp = dispinterface
    ['{8093A3D4-CF1A-4092-9620-57A5CDCCE4D0}']
    function SendI2CPacket(data: {NOT_OLEAUTO(PSafeArray)}OleVariant; mp3Handle: Int64; 
                           restartLocation: Integer; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 25;
    function GetVersion: Integer; dispid 1;
    function GetDevicesChangedLocation: WideString; dispid 2;
    function EnumerateMiniProgs(out miniProg3s: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 3;
    function EnumerateDevices(const miniProg3: WideString; 
                              out devices: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 4;
    procedure SetLedState(const miniProg3: WideString; led: CyLed; state: CyLedState; 
                          out error: CyErrorCode); dispid 5;
    procedure VisualIdentify(const miniProg3: WideString; state: CyLedIdentifyState; 
                             out error: CyErrorCode); dispid 6;
    function GetMiniProgStatus(const miniProg3: WideString; 
                               out status: {NOT_OLEAUTO(CyMP3Status)}OleVariant): CyErrorCode; dispid 7;
    function SetTimeOut(const miniProg3: WideString; endPoint: CyMiniProg3EndPoint; 
                        timeout: Integer; ownerPID: Integer): CyErrorCode; dispid 8;
    function SetJtagClock(const miniProg3: WideString; clock: CyJtagClock; ownerPID: Integer): CyErrorCode; dispid 10;
    function SetMode(const miniProg3: WideString; mode: CyTransferMode; port10N5: Shortint; 
                     ownerPID: Integer): CyErrorCode; dispid 11;
    function SetVTarg(const miniProg3: WideString; voltage: CyVTargVoltage; 
                      supplyVoltage: Shortint; ownerPID: Integer): CyErrorCode; dispid 12;
    function AcquireDevice(const miniProg3: WideString; deviceNum: Integer; ownerPID: Integer; 
                           out mp3Handle: Int64): CyErrorCode; dispid 13;
    function ReleaseDevice(mp3Handle: Int64): CyErrorCode; dispid 14;
    function SendJtagPacket(tdi_tms: {NOT_OLEAUTO(PSafeArray)}OleVariant; read: Shortint; 
                            mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 15;
    function SendSwdPacket(data: {NOT_OLEAUTO(PSafeArray)}OleVariant; mp3Handle: Int64; 
                           out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 16;
    function RequestSwvPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 17;
    function BulkTransfer(endPoint: CyMiniProg3EndPoint; data: {NOT_OLEAUTO(PSafeArray)}OleVariant; 
                          readNwrite: Shortint; mp3Handle: Int64; 
                          out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 18;
    function PortAcquire(const miniProg3: WideString; mode: CyProgrammingMode; 
                         portAcquireKey: Integer; writeTestKey: Shortint; testModeAddress: Integer; 
                         testModeKey: Integer; retryLimit: Integer): CyErrorCode; dispid 20;
    function GetPortAcquireStatus(const miniProg3: WideString; abort: Shortint; 
                                  out acquireStatus: Integer): CyErrorCode; dispid 21;
    function ToggleXres(const miniProg3: WideString; active: Shortint): CyErrorCode; dispid 22;
    function SetupSWV(const miniProg3: WideString; targetFreq: Integer; manchesterNuart: Shortint; 
                      commitTime: Smallint): CyErrorCode; dispid 23;
    function RequestEtmPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 24;
  end;

// *********************************************************************//
// Interface: ICyMiniProg3_v3
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C1179083-5983-4C08-8BB7-3AD21A61BA77}
// *********************************************************************//
  ICyMiniProg3_v3 = interface(ICyMiniProg3_v2)
    ['{C1179083-5983-4C08-8BB7-3AD21A61BA77}']
    function PortAcquire(mode: CyProgrammingMode; portAcquireKey: Integer; writeTestKey: Shortint; 
                         testModeAddress: Integer; testModeKey: Integer; retryLimit: Integer; 
                         mp3Handle: Int64): CyErrorCode; safecall;
    function GetHwRevID(const miniProg3: WideString; out revID: Integer): CyErrorCode; safecall;
  end;

// *********************************************************************//
// DispIntf:  ICyMiniProg3_v3Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C1179083-5983-4C08-8BB7-3AD21A61BA77}
// *********************************************************************//
  ICyMiniProg3_v3Disp = dispinterface
    ['{C1179083-5983-4C08-8BB7-3AD21A61BA77}']
    function PortAcquire(mode: CyProgrammingMode; portAcquireKey: Integer; writeTestKey: Shortint; 
                         testModeAddress: Integer; testModeKey: Integer; retryLimit: Integer; 
                         mp3Handle: Int64): CyErrorCode; dispid 26;
    function GetHwRevID(const miniProg3: WideString; out revID: Integer): CyErrorCode; dispid 27;
    function SendI2CPacket(data: {NOT_OLEAUTO(PSafeArray)}OleVariant; mp3Handle: Int64;
                           restartLocation: Integer; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 25;
    function GetVersion: Integer; dispid 1;
    function GetDevicesChangedLocation: WideString; dispid 2;
    function EnumerateMiniProgs(out miniProg3s: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 3;
    function EnumerateDevices(const miniProg3: WideString;
                              out devices: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 4;
    procedure SetLedState(const miniProg3: WideString; led: CyLed; state: CyLedState;
                          out error: CyErrorCode); dispid 5;
    procedure VisualIdentify(const miniProg3: WideString; state: CyLedIdentifyState;
                             out error: CyErrorCode); dispid 6;
    function GetMiniProgStatus(const miniProg3: WideString;
                               out status: {NOT_OLEAUTO(CyMP3Status)}OleVariant): CyErrorCode; dispid 7;
    function SetTimeOut(const miniProg3: WideString; endPoint: CyMiniProg3EndPoint;
                        timeout: Integer; ownerPID: Integer): CyErrorCode; dispid 8;
    function SetJtagClock(const miniProg3: WideString; clock: CyJtagClock; ownerPID: Integer): CyErrorCode; dispid 10;
    function SetMode(const miniProg3: WideString; mode: CyTransferMode; port10N5: Shortint;
                     ownerPID: Integer): CyErrorCode; dispid 11;
    function SetVTarg(const miniProg3: WideString; voltage: CyVTargVoltage;
                      supplyVoltage: Shortint; ownerPID: Integer): CyErrorCode; dispid 12;
    function AcquireDevice(const miniProg3: WideString; deviceNum: Integer; ownerPID: Integer;
                           out mp3Handle: Int64): CyErrorCode; dispid 13;
    function ReleaseDevice(mp3Handle: Int64): CyErrorCode; dispid 14;
    function SendJtagPacket(tdi_tms: {NOT_OLEAUTO(PSafeArray)}OleVariant; read: Shortint;
                            mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 15;
    function SendSwdPacket(data: {NOT_OLEAUTO(PSafeArray)}OleVariant; mp3Handle: Int64;
                           out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 16;
    function RequestSwvPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 17;
    function BulkTransfer(endPoint: CyMiniProg3EndPoint; data: {NOT_OLEAUTO(PSafeArray)}OleVariant;
                          readNwrite: Shortint; mp3Handle: Int64;
                          out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 18;
    function PortAcquire2(const miniProg3: WideString; mode: CyProgrammingMode;
                         portAcquireKey: Integer; writeTestKey: Shortint; testModeAddress: Integer;
                         testModeKey: Integer; retryLimit: Integer): CyErrorCode; dispid 20;
    function GetPortAcquireStatus(const miniProg3: WideString; abort: Shortint;
                                  out acquireStatus: Integer): CyErrorCode; dispid 21;
    function ToggleXres(const miniProg3: WideString; active: Shortint): CyErrorCode; dispid 22;
    function SetupSWV(const miniProg3: WideString; targetFreq: Integer; manchesterNuart: Shortint;
                      commitTime: Smallint): CyErrorCode; dispid 23;
    function RequestEtmPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 24;
  end;

// *********************************************************************//
// Interface: ICyMiniProg3_v4
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F1C66846-910A-46EA-86E1-C96C54A074BE}
// *********************************************************************//
  ICyMiniProg3_v4 = interface(ICyMiniProg3_v3)
    ['{F1C66846-910A-46EA-86E1-C96C54A074BE}']
    function PortAcquire(const miniProg3: WideString; mode: CyProgrammingMode; retryLimit: Integer): CyErrorCode; safecall;
  end;

// *********************************************************************//
// DispIntf:  ICyMiniProg3_v4Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F1C66846-910A-46EA-86E1-C96C54A074BE}
// *********************************************************************//
  ICyMiniProg3_v4Disp = dispinterface
    ['{F1C66846-910A-46EA-86E1-C96C54A074BE}']
    function PortAcquire(const miniProg3: WideString; mode: CyProgrammingMode; retryLimit: Integer): CyErrorCode; dispid 28;
    function PortAcquire2(mode: CyProgrammingMode; portAcquireKey: Integer; writeTestKey: Shortint;
                         testModeAddress: Integer; testModeKey: Integer; retryLimit: Integer; 
                         mp3Handle: Int64): CyErrorCode; dispid 26;
    function GetHwRevID(const miniProg3: WideString; out revID: Integer): CyErrorCode; dispid 27;
    function SendI2CPacket(data: {NOT_OLEAUTO(PSafeArray)}OleVariant; mp3Handle: Int64; 
                           restartLocation: Integer; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 25;
    function GetVersion: Integer; dispid 1;
    function GetDevicesChangedLocation: WideString; dispid 2;
    function EnumerateMiniProgs(out miniProg3s: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 3;
    function EnumerateDevices(const miniProg3: WideString; 
                              out devices: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 4;
    procedure SetLedState(const miniProg3: WideString; led: CyLed; state: CyLedState; 
                          out error: CyErrorCode); dispid 5;
    procedure VisualIdentify(const miniProg3: WideString; state: CyLedIdentifyState; 
                             out error: CyErrorCode); dispid 6;
    function GetMiniProgStatus(const miniProg3: WideString; 
                               out status: {NOT_OLEAUTO(CyMP3Status)}OleVariant): CyErrorCode; dispid 7;
    function SetTimeOut(const miniProg3: WideString; endPoint: CyMiniProg3EndPoint; 
                        timeout: Integer; ownerPID: Integer): CyErrorCode; dispid 8;
    function SetJtagClock(const miniProg3: WideString; clock: CyJtagClock; ownerPID: Integer): CyErrorCode; dispid 10;
    function SetMode(const miniProg3: WideString; mode: CyTransferMode; port10N5: Shortint; 
                     ownerPID: Integer): CyErrorCode; dispid 11;
    function SetVTarg(const miniProg3: WideString; voltage: CyVTargVoltage; 
                      supplyVoltage: Shortint; ownerPID: Integer): CyErrorCode; dispid 12;
    function AcquireDevice(const miniProg3: WideString; deviceNum: Integer; ownerPID: Integer; 
                           out mp3Handle: Int64): CyErrorCode; dispid 13;
    function ReleaseDevice(mp3Handle: Int64): CyErrorCode; dispid 14;
    function SendJtagPacket(tdi_tms: {NOT_OLEAUTO(PSafeArray)}OleVariant; read: Shortint; 
                            mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 15;
    function SendSwdPacket(data: {NOT_OLEAUTO(PSafeArray)}OleVariant; mp3Handle: Int64; 
                           out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 16;
    function RequestSwvPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 17;
    function BulkTransfer(endPoint: CyMiniProg3EndPoint; data: {NOT_OLEAUTO(PSafeArray)}OleVariant; 
                          readNwrite: Shortint; mp3Handle: Int64; 
                          out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 18;
    function PortAcquire3(const miniProg3: WideString; mode: CyProgrammingMode;
                         portAcquireKey: Integer; writeTestKey: Shortint; testModeAddress: Integer; 
                         testModeKey: Integer; retryLimit: Integer): CyErrorCode; dispid 20;
    function GetPortAcquireStatus(const miniProg3: WideString; abort: Shortint; 
                                  out acquireStatus: Integer): CyErrorCode; dispid 21;
    function ToggleXres(const miniProg3: WideString; active: Shortint): CyErrorCode; dispid 22;
    function SetupSWV(const miniProg3: WideString; targetFreq: Integer; manchesterNuart: Shortint; 
                      commitTime: Smallint): CyErrorCode; dispid 23;
    function RequestEtmPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 24;
  end;

// *********************************************************************//
// Interface: ICyMiniProg3_v5
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {101BE394-DC29-43DA-9764-0E184965ADE6}
// *********************************************************************//
  ICyMiniProg3_v5 = interface(ICyMiniProg3_v4)
    ['{101BE394-DC29-43DA-9764-0E184965ADE6}']
    function PortAcquire(const miniProg3: WideString; target: CyAcquireTarget; 
                         mode: CyProgrammingMode; retryLimit: Integer): CyErrorCode; safecall;
    function SendSwdPacket(const miniProg3: WideString; data: PSafeArray; out RESULT: PSafeArray): CyErrorCode; safecall;
  end;

// *********************************************************************//
// DispIntf:  ICyMiniProg3_v5Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {101BE394-DC29-43DA-9764-0E184965ADE6}
// *********************************************************************//
  ICyMiniProg3_v5Disp = dispinterface
    ['{101BE394-DC29-43DA-9764-0E184965ADE6}']
    function PortAcquire(const miniProg3: WideString; target: CyAcquireTarget; 
                         mode: CyProgrammingMode; retryLimit: Integer): CyErrorCode; dispid 29;
    function SendSwdPacket(const miniProg3: WideString; data: {NOT_OLEAUTO(PSafeArray)}OleVariant; 
                           out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 30;
    function PortAcquire2(const miniProg3: WideString; mode: CyProgrammingMode; retryLimit: Integer): CyErrorCode; dispid 28;
    function PortAcquire3(mode: CyProgrammingMode; portAcquireKey: Integer; writeTestKey: Shortint;
                         testModeAddress: Integer; testModeKey: Integer; retryLimit: Integer; 
                         mp3Handle: Int64): CyErrorCode; dispid 26;
    function GetHwRevID(const miniProg3: WideString; out revID: Integer): CyErrorCode; dispid 27;
    function SendI2CPacket(data: {NOT_OLEAUTO(PSafeArray)}OleVariant; mp3Handle: Int64; 
                           restartLocation: Integer; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 25;
    function GetVersion: Integer; dispid 1;
    function GetDevicesChangedLocation: WideString; dispid 2;
    function EnumerateMiniProgs(out miniProg3s: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 3;
    function EnumerateDevices(const miniProg3: WideString; 
                              out devices: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 4;
    procedure SetLedState(const miniProg3: WideString; led: CyLed; state: CyLedState; 
                          out error: CyErrorCode); dispid 5;
    procedure VisualIdentify(const miniProg3: WideString; state: CyLedIdentifyState; 
                             out error: CyErrorCode); dispid 6;
    function GetMiniProgStatus(const miniProg3: WideString; 
                               out status: {NOT_OLEAUTO(CyMP3Status)}OleVariant): CyErrorCode; dispid 7;
    function SetTimeOut(const miniProg3: WideString; endPoint: CyMiniProg3EndPoint; 
                        timeout: Integer; ownerPID: Integer): CyErrorCode; dispid 8;
    function SetJtagClock(const miniProg3: WideString; clock: CyJtagClock; ownerPID: Integer): CyErrorCode; dispid 10;
    function SetMode(const miniProg3: WideString; mode: CyTransferMode; port10N5: Shortint; 
                     ownerPID: Integer): CyErrorCode; dispid 11;
    function SetVTarg(const miniProg3: WideString; voltage: CyVTargVoltage; 
                      supplyVoltage: Shortint; ownerPID: Integer): CyErrorCode; dispid 12;
    function AcquireDevice(const miniProg3: WideString; deviceNum: Integer; ownerPID: Integer; 
                           out mp3Handle: Int64): CyErrorCode; dispid 13;
    function ReleaseDevice(mp3Handle: Int64): CyErrorCode; dispid 14;
    function SendJtagPacket(tdi_tms: {NOT_OLEAUTO(PSafeArray)}OleVariant; read: Shortint; 
                            mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 15;
    function SendSwdPacket2(data: {NOT_OLEAUTO(PSafeArray)}OleVariant; mp3Handle: Int64;
                           out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 16;
    function RequestSwvPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 17;
    function BulkTransfer(endPoint: CyMiniProg3EndPoint; data: {NOT_OLEAUTO(PSafeArray)}OleVariant;
                          readNwrite: Shortint; mp3Handle: Int64;
                          out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 18;
    function PortAcquire4(const miniProg3: WideString; mode: CyProgrammingMode;
                         portAcquireKey: Integer; writeTestKey: Shortint; testModeAddress: Integer;
                         testModeKey: Integer; retryLimit: Integer): CyErrorCode; dispid 20;
    function GetPortAcquireStatus(const miniProg3: WideString; abort: Shortint; 
                                  out acquireStatus: Integer): CyErrorCode; dispid 21;
    function ToggleXres(const miniProg3: WideString; active: Shortint): CyErrorCode; dispid 22;
    function SetupSWV(const miniProg3: WideString; targetFreq: Integer; manchesterNuart: Shortint; 
                      commitTime: Smallint): CyErrorCode; dispid 23;
    function RequestEtmPackets(mp3Handle: Int64; out RESULT: {NOT_OLEAUTO(PSafeArray)}OleVariant): CyErrorCode; dispid 24;
  end;

// *********************************************************************//
// The Class CoCyMiniProg3 provides a Create and CreateRemote method to          
// create instances of the default interface ICyMiniProg3_v5 exposed by              
// the CoClass CyMiniProg3. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCyMiniProg3 = class
    class function Create: ICyMiniProg3_v5;
    class function CreateRemote(const MachineName: string): ICyMiniProg3_v5;
  end;

implementation

uses System.Win.ComObj;

class function CoCyMiniProg3.Create: ICyMiniProg3_v5;
begin
  Result := CreateComObject(CLASS_CyMiniProg3) as ICyMiniProg3_v5;
end;

class function CoCyMiniProg3.CreateRemote(const MachineName: string): ICyMiniProg3_v5;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CyMiniProg3) as ICyMiniProg3_v5;
end;

end.
