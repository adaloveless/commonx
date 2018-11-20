unit PSoCProgrammerCOMLib_TLB;

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
// File generated on 6/26/2018 10:23:46 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files (x86)\Cypress\Programmer\PSoCProgrammerCOM.exe (1)
// LIBID: {97FF1B53-6477-4BD1-8A29-ADDB58E862E5}
// LCID: 0
// Helpfile: 
// HelpString: PSoCProgrammerCOM 24.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
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
  PSoCProgrammerCOMLibMajorVersion = 24;
  PSoCProgrammerCOMLibMinorVersion = 0;

  LIBID_PSoCProgrammerCOMLib: TGUID = '{97FF1B53-6477-4BD1-8A29-ADDB58E862E5}';

  DIID__IPSoCProgrammerCOM_ObjectEvents: TGUID = '{F2082001-3EA0-403B-8634-DD3FCBAB8BD6}';
  IID_IPSoCProgrammerCOM_Object: TGUID = '{5C08A7E9-3B1C-47D3-AB19-F47B67C95897}';
  CLASS_PSoCProgrammerCOM_Object: TGUID = '{B2BF84F0-815B-4431-98FC-C6291CCD7A32}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum enumInterfaces
type
  enumInterfaces = TOleEnum;
const
  JTAG = $00000001;
  ISSP = $00000002;
  I2C = $00000004;
  SWD  = $00000008;
  SPI = $00000010;
  SWD_SWV = $00000020;

// Constants for enum enumFrequencies
type
  enumFrequencies = TOleEnum;
const
  FREQ_48_0 = $00000000;
  FREQ_24_0 = $00000004;
  FREQ_16_0 = $00000010;
  FREQ_12_0 = $00000084;
  FREQ_08_0 = $00000090;
  FREQ_06_0 = $00000060;
  FREQ_04_0 = $00000091;
  FREQ_03_2 = $00000018;
  FREQ_03_0 = $000000E0;
  FREQ_01_6 = $00000098;
  FREQ_01_5 = $000000C0;
  FREQ_00_8 = $00000099;
  FREQ_00_4 = $0000001A;
  FREQ_00_2 = $0000009A;
  FREQ_RESET = $000000FC;

// Constants for enum enumChipIDType
type
  enumChipIDType = TOleEnum;
const
  SILICON_ID = $00000001;
  JTAG_ID = $00000002;

// Constants for enum __MIDL___MIDL_itf_PSoCProgrammerCOM_0000_0000_0001
type
  __MIDL___MIDL_itf_PSoCProgrammerCOM_0000_0000_0001 = TOleEnum;
const
  ARRAY_FLASH = $00000001;
  ARRAY_EEPROM = $00000002;
  ARRAY_NVL_USER = $00000004;
  ARRAY_NVL_FACTORY = $00000008;
  ARRAY_NVL_WO_LATCHES = $00000010;
  ARRAY_ALL = $0000001F;

// Constants for enum enumI2Cspeed
type
  enumI2Cspeed = TOleEnum;
const
  CLK_100K = $00000001;
  CLK_400K = $00000002;
  CLK_50K = $00000004;
  CLK_1000K = $00000005;

// Constants for enum enumSpiBitOrder
type
  enumSpiBitOrder = TOleEnum;
const
  MSB = $00000000;
  LSB = $00000001;

// Constants for enum enumSpiMode
type
  enumSpiMode = TOleEnum;
const
  Mode_00 = $00000000;
  Mode_01 = $00000001;
  Mode_02 = $00000002;
  Mode_03 = $00000003;

// Constants for enum enumSWVMode
type
  enumSWVMode = TOleEnum;
const
  TX8 = $00000001;
  MANCHESTER = $00000002;

// Constants for enum enumHexFileType
type
  enumHexFileType = TOleEnum;
const
  UNKNOWN = $00000000;
  PSoC1 = $00000001;
  PSoC3_ES0 = $00000002;
  PSoC3_ES2 = $00000003;
  PSoC5 = $00000004;
  PSoC4 = $00000005;
  Gen4_B2B = $00000006;
  SPIRIT = $00000007;
  STREET_FIGHTER = $00000008;
  EEPROM = $00000009;

// Constants for enum enumValidAcquireModes
type
  enumValidAcquireModes = TOleEnum;
const
  CAN_RESET_ACQUIRE = $00000001;
  CAN_POWER_CYCLE_ACQUIRE = $00000002;
  CAN_POWER_DETECT_ACQUIRE = $00000004;
  CAN_SOFT_RESET_ACQUIRE = $00000008;

// Constants for enum enumCanPowerDevice
type
  enumCanPowerDevice = TOleEnum;
const
  CAN_POWER_DEVICE = $00000001;
  CAN_READ_POWER = $00000002;
  CAN_MEASURE_POWER = $00000004;
  CAN_MEASURE_POWER_2 = $00000008;

// Constants for enum enumCanProgram
type
  enumCanProgram = TOleEnum;
const
  CAN_PROGRAM_CARBON = $00000001;
  CAN_PROGRAM_ENCORE = $00000002;

// Constants for enum enumUpgradeFirmware
type
  enumUpgradeFirmware = TOleEnum;
const
  INITIALIZE = $00000000;
  UPGRADE_BLOCK = $00000001;
  VERIFY_BLOCK = $00000002;
  FINALIZE = $00000003;

// Constants for enum enumVoltages
type
  enumVoltages = TOleEnum;
const
  VOLT_50V = $00000001;
  VOLT_33V = $00000002;
  VOLT_25V = $00000004;
  VOLT_18V = $00000008;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _IPSoCProgrammerCOM_ObjectEvents = dispinterface;
  IPSoCProgrammerCOM_Object = interface;
  IPSoCProgrammerCOM_ObjectDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  PSoCProgrammerCOM_Object = IPSoCProgrammerCOM_Object;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//

  CFamilyInfo = record
    sFamilyName: WideString;
    sDisplayName: WideString;
    iFamilyCode: SYSINT;
    iProtocol: SYSINT;
    iBytesPerBlock: SYSINT;
  end;

{$ALIGN 8}
  CDeviceInfo = record
    sFamilyName: WideString;
    iFamilyCode: SYSINT;
    iPins: SYSINT;
    iFlashSize: SYSINT;
    iAcquireModes: SYSINT;
    siliconIDs: OleVariant;
    sVoltage: WideString;
  end;

  enumSonosArrays = __MIDL___MIDL_itf_PSoCProgrammerCOM_0000_0000_0001; 

{$ALIGN 1}
  CProgrammerCaps = record
    m_bValidAcquireModes: Byte;
    m_bCanPowerDevice: Byte;
    m_bCanUpdateFirmware: Byte;
    m_bCanProgram: Byte;
  end;


// *********************************************************************//
// DispIntf:  _IPSoCProgrammerCOM_ObjectEvents
// Flags:     (4096) Dispatchable
// GUID:      {F2082001-3EA0-403B-8634-DD3FCBAB8BD6}
// *********************************************************************//
  _IPSoCProgrammerCOM_ObjectEvents = dispinterface
    ['{F2082001-3EA0-403B-8634-DD3FCBAB8BD6}']
    function Connected(const deviceName: WideString): HResult; dispid 1;
    function Disconnected(const deviceName: WideString): HResult; dispid 2;
    function Notify(const msg: WideString): HResult; dispid 3;
    function USB2IIC_ReceivedData(dataOUT: OleVariant): HResult; dispid 4;
  end;

// *********************************************************************//
// Interface: IPSoCProgrammerCOM_Object
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5C08A7E9-3B1C-47D3-AB19-F47B67C95897}
// *********************************************************************//
  IPSoCProgrammerCOM_Object = interface(IDispatch)
    ['{5C08A7E9-3B1C-47D3-AB19-F47B67C95897}']
    function Acquire(out strError: WideString): Integer; safecall;
    function AcquireChip(out strError: WideString): Integer; safecall;
    function AcquireChipWithDelay(delay: Integer; out strError: WideString): Integer; safecall;
    function SetAcquireMode(const mode: WideString; out strError: WideString): Integer; safecall;
    function GetAcquireMode(out mode: WideString; out strError: WideString): Integer; safecall;
    function GetPorts(out ports: OleVariant; out strError: WideString): Integer; safecall;
    function ReleaseChip(out strError: WideString): Integer; safecall;
    function OpenPort(const port: WideString; out strError: WideString): Integer; safecall;
    function GetSiliconId(out siliconID: OleVariant; out strError: WideString): Integer; safecall;
    function ClosePort(out strError: WideString): Integer; safecall;
    function IsPortOpen(out isOpen: Integer; out strError: WideString): Integer; safecall;
    function _StartSelfTerminator(clientProcessID: Integer): Integer; safecall;
    function GetProgrammerVersion(out versionString: WideString; out strError: WideString): Integer; safecall;
    function SetPowerVoltage(const voltage: WideString; out strError: WideString): Integer; safecall;
    function PowerOn(out strError: WideString): Integer; safecall;
    function PowerOff(out strError: WideString): Integer; safecall;
    function GetPower(out power: Integer; out strError: WideString): Integer; safecall;
    function GetProgrammerCapabilities(out caps: OleVariant; out strError: WideString): Integer; safecall;
    function ReadHexFile(const filename: WideString; out imageSize: Integer; 
                         out strError: WideString): Integer; safecall;
    function ReadHexData(addr: Integer; size: Integer; out data: OleVariant; 
                         out strError: WideString): Integer; safecall;
    function ReadHexProtection(addr: Integer; size: Integer; out data: OleVariant; 
                               out strError: WideString): Integer; safecall;
    function ReadHexChecksum(out checksum: Word; out strError: WideString): Integer; safecall;
    function Calibrate(calibrateIndex: Integer; out strError: WideString): Integer; safecall;
    function EraseAll(out strError: WideString): Integer; safecall;
    function ProtectAll(out strError: WideString): Integer; safecall;
    function Protect(bankID: Integer; out strError: WideString): Integer; safecall;
    function VerifyProtect(out strError: WideString): Integer; safecall;
    function GetFlashCharacteristics(out blockSize: Integer; out banks: Integer; 
                                     out blocksPerBank: Integer; out strError: WideString): Integer; safecall;
    function checksum(blockID: Integer; out checksum: Word; out strError: WideString): Integer; safecall;
    function CheckSum1(blockID: Integer; bank: Integer; out checksum: Word; out strError: WideString): Integer; safecall;
    function SetBank(bank: Integer; out strError: WideString): Integer; safecall;
    function EraseBlock(blockID: Integer; out sscResult: Integer; out strError: WideString): Integer; safecall;
    function EraseBlock1(blockID: Integer; bank: Integer; out sscResult: Integer; 
                         out strError: WideString): Integer; safecall;
    function ReadBlock(blockID: Integer; out data: OleVariant; out sscResult: Integer; 
                       out strError: WideString): Integer; safecall;
    function ReadBlock1(bank: Integer; blockID: Integer; out data: OleVariant; 
                        out sscResult: Integer; out strError: WideString): Integer; safecall;
    function WriteBlock(blockID: Integer; block: OleVariant; out sscResult: Integer; 
                        out strError: WideString): Integer; safecall;
    function WriteBlock1(bank: Integer; blockID: Integer; block: OleVariant; 
                         out sscResult: Integer; out strError: WideString): Integer; safecall;
    function WriteBlockFromHex(hexBlockID: Integer; flashBlockID: Integer; out sscResult: Integer; 
                               out strError: WideString): Integer; safecall;
    function WriteBlockFromHex1(hexBlockID: Integer; bank: Integer; flashBlockID: Integer; 
                                out sscResult: Integer; out strError: WideString): Integer; safecall;
    function TableRead(tableID: Integer; out XA: Integer; out table: OleVariant; 
                       out sscResult: Integer; out strError: WideString): Integer; safecall;
    function VerifyBlock(blockID: Integer; block: OleVariant; out verResult: Integer; 
                         out strError: WideString): Integer; safecall;
    function VerifyBlock1(bank: Integer; blockID: Integer; block: OleVariant; 
                          out verResult: Integer; out strError: WideString): Integer; safecall;
    function VerifyBlockFromHex(hexBlockID: Integer; out verResult: Integer; 
                                out strError: WideString): Integer; safecall;
    function Version: WideString; safecall;
    function ReadIO(addr: Integer; size: Integer; out data: OleVariant; out strError: WideString): Integer; safecall;
    function WriteIO(addr: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function ReadRAM(addr: Integer; size: Integer; out data: OleVariant; out strError: WideString): Integer; safecall;
    function WriteRAM(addr: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function TestClock(number: Integer; out strError: WideString): Integer; safecall;
    function UpdateProgrammer(const arguments: WideString; out strError: WideString): Integer; safecall;
    function UpdateProgrammer1(cmd: Integer; const arguments: WideString; out blockNo: Integer; 
                               out strError: WideString): Integer; safecall;
    function GetFamilyList(out familyList: OleVariant; out strError: WideString): Integer; safecall;
    function GetDeviceList(const family: WideString; out deviceList: OleVariant; 
                           out strError: WideString): Integer; safecall;
    function ReadProtection(bank: Integer; out block: OleVariant; out sscResult: Integer; 
                            out strError: WideString): Integer; safecall;
    function RunFile(const scriptFile: WideString; const reportFile: WideString; 
                     out strError: WideString): Integer; safecall;
    function RunConsole(const cmd: WideString; out strError: WideString): Integer; safecall;
    function GetDeviceMatchedList(siliconID: OleVariant; out deviceList: OleVariant; 
                                  out strError: WideString): Integer; safecall;
    function GetDeviceInfo(const devNameDisp: WideString; out family: WideString; 
                           out familyCode: Integer; out pins: Integer; out flashSize: Integer; 
                           out acquireModes: Integer; out siliconIDs: OleVariant; 
                           out strError: WideString): Integer; safecall;
    function WriteHexFile(const filename: WideString; out strError: WideString): Integer; safecall;
    function WriteHexData(address: Integer; block: OleVariant; out strError: WideString): Integer; safecall;
    function WriteHexProtection(address: Integer; block: OleVariant; out strError: WideString): Integer; safecall;
    function SetChipType(familyID: Integer; out strError: WideString): Integer; safecall;
    function GetFamilyInfo(const familyName: WideString; out familyInfo: CFamilyInfo; 
                           out strError: WideString): Integer; safecall;
    function GetDeviceInfo1(const devNameDisp: WideString; out deviceInfo: CDeviceInfo; 
                            out strError: WideString): Integer; safecall;
    function GetPowerVoltage(out voltage: WideString; out strError: WideString): Integer; safecall;
    function GetPower1(out power: Integer; out voltage: Integer; out strError: WideString): Integer; safecall;
    function SetProtocol(protocol: enumInterfaces; out strError: WideString): Integer; safecall;
    function SetProtocolConnector(connector: Integer; out strError: WideString): Integer; safecall;
    function SetProtocolClock(clock: enumFrequencies; out strError: WideString): Integer; safecall;
    function GetProgrammerCapsByName(const portName: WideString; out caps: OleVariant; 
                                     out strError: WideString): Integer; safecall;
    function ProgrammerLedState(ledNo: Integer; ledState: Integer; out strError: WideString): Integer; safecall;
    function JTAG_EnumerateDevices(out devices: OleVariant; out strError: WideString): Integer; safecall;
    function SetAutoReset(mode: Integer; out strError: WideString): Integer; safecall;
    function GetPower2(out power: Integer; out voltage1: Integer; out voltage2: Integer; 
                       out strError: WideString): Integer; safecall;
    function CheckSum_(blockID: Integer; out checksum: Smallint; out strError: WideString): Integer; safecall;
    function CheckSum1_(blockID: Integer; bank: Integer; out checksum: Smallint; 
                        out strError: WideString): Integer; safecall;
    function ReadHexChecksum_(out checksum: Smallint; out strError: WideString): Integer; safecall;
    function GetProgrammerRevID(out revID: Integer; out strError: WideString): Integer; safecall;
    function ToggleReset(polarity: Integer; duration: Integer; out strError: WideString): Integer; safecall;
    function JTAG_SetChainConfig(selectedDevice: Byte; Ir: OleVariant; out strError: WideString): Integer; safecall;
    function JTAG_SetIR(Ir: Integer; out strError: WideString): Integer; safecall;
    function JTAG_ShiftDR(drSize: Integer; drIn: OleVariant; out drOut: OleVariant; 
                          out strError: WideString): Integer; safecall;
    function JTAG_ShiftIR(irSize: Integer; irIn: OleVariant; out irOut: OleVariant; 
                          out strError: WideString): Integer; safecall;
    function GetDeviceListBySiliconID(siliconID: OleVariant; ignoreRevisionID: Byte; 
                                      out deviceList: OleVariant; out strError: WideString): Integer; safecall;
    function HEX_CompatibleWithSupersetPart(idType: enumChipIDType; hexID: OleVariant; 
                                            chipID: OleVariant; out compatible: WordBool; 
                                            out strError: WideString): Integer; safecall;
    function PSoC3_SetJtagChainConfig(selectedDevice: Byte; IrDr: OleVariant; 
                                      out strError: WideString): Integer; safecall;
    function PSoC3_SetProtocolClock(clock: enumFrequencies; out strError: WideString): Integer; safecall;
    function PSoC3_GetSonosArrays(arrayGroup: enumSonosArrays; out arrays: OleVariant; 
                                  out strError: WideString): Integer; safecall;
    function PSoC3_GetFlashArrayInfo(arrayID: Integer; out rowSize: Integer; 
                                     out rowsPerArray: Integer; out eccPresence: Integer; 
                                     out strError: WideString): Integer; safecall;
    function PSoC3_WriteIO(baseAddr: Integer; data: Integer; out strError: WideString): Integer; safecall;
    function PSoC3_ReadIO(baseAddr: Integer; out data: Integer; out strError: WideString): Integer; safecall;
    function PSoC3_PollIO(baseAddr: Integer; expectedData: Integer; timeOut: Integer; 
                          out strError: WideString): Integer; safecall;
    function PSoC3_PollIO1(baseAddr: Integer; expectedMask: Integer; timeOut: Integer; 
                           out strError: WideString): Integer; safecall;
    function PSoC3_EraseAll(out strError: WideString): Integer; safecall;
    function PSoC3_WriteNvlArray(arrayID: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC3_GetJtagID(out jtagID: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC3_ReadHexImageSizes(out flashSize: Integer; out configSize: Integer; 
                                     out eepromSize: Integer; out nvlUserSize: Integer; 
                                     out nvlWoSize: Integer; out strError: WideString): Integer; safecall;
    function PSoC3_ReadHexJtagID(out jtagID: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC3_ReadHexNvlCustom(addr: Integer; size: Integer; out data: OleVariant; 
                                    out strError: WideString): Integer; safecall;
    function PSoC3_ReadHexNvlWo(address: Integer; size: Integer; out data: OleVariant; 
                                out strError: WideString): Integer; safecall;
    function PSoC3_ReadHexEEPROM(addr: Integer; size: Integer; out data: OleVariant; 
                                 out strError: WideString): Integer; safecall;
    function PSoC3_ReadHexConfig(addr: Integer; size: Integer; out data: OleVariant; 
                                 out strError: WideString): Integer; safecall;
    function PSoC3_ReadNvlArray(arrayID: Integer; out data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC3_GetEccStatus(out status: Integer; out strError: WideString): Integer; safecall;
    function PSoC3_ProgramRow(arrayID: Integer; rowID: Integer; data: OleVariant; 
                              out strError: WideString): Integer; safecall;
    function PSoC3_ReadRow(arrayID: Integer; rowID: Integer; eccSpace: Integer; 
                           out data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC3_ProtectArray(arrayID: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC3_ProtectAll(out strError: WideString): Integer; safecall;
    function PSoC3_ReadProtection(arrayID: Integer; out data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC3_VerifyProtect(out strError: WideString): Integer; safecall;
    function PSoC3_CheckSum(arrayID: Integer; startRowID: Integer; noOfRows: Integer; 
                            out checksum: LongWord; out strError: WideString): Integer; safecall;
    function PSoC3_ProgramRowFromHex(arrayID: Integer; rowID: Integer; eccOption: Integer; 
                                     out strError: WideString): Integer; safecall;
    function PSoC3_VerifyRowFromHex(arrayID: Integer; rowID: Integer; eccOption: Integer; 
                                    out verResult: Integer; out strError: WideString): Integer; safecall;
    function PSoC3_WriteRow(arrayID: Integer; rowID: Integer; data: OleVariant; 
                            out strError: WideString): Integer; safecall;
    function PSoC3_AcquireChip(out strError: WideString): Integer; safecall;
    function PSoC3_ReleaseChip(out strError: WideString): Integer; safecall;
    function PSoC3_DebugPortConfig(transferMode: Integer; out strError: WideString): Integer; safecall;
    function PSoC3_ProgramRowFast(arrayID: Integer; rowID: Integer; eccOption: Integer; 
                                  out strError: WideString): Integer; safecall;
    function PSoC3_Acquire(out strError: WideString): Integer; safecall;
    function PSoC3_ReadHexExtra(out extra: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC3_InitChip(out strError: WideString): Integer; safecall;
    function PSoC3_EraseRow(arrayID: Integer; rowID: Integer; out strError: WideString): Integer; safecall;
    function PSoC3_CheckSum_(arrayID: Integer; startRowID: Integer; noOfRows: Integer; 
                             out checksum: Integer; out strError: WideString): Integer; safecall;
    function PSoC3_GetEepromArrayInfo(arrayID: Integer; out rowSize: Integer; 
                                      out rowsPerArray: Integer; out strError: WideString): Integer; safecall;
    function PSoC3_EraseSector(arrayID: Integer; sectorID: Integer; out strError: WideString): Integer; safecall;
    function HEX_ReadChipProtection(out data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC4_WriteRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC4_ProgramRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC4_EraseAll(out strError: WideString): Integer; safecall;
    function PSoC4_CheckSum(rowID: Integer; out checksum: Integer; out strError: WideString): Integer; safecall;
    function PSoC4_WriteProtection(flashProtect: OleVariant; chipProtect: OleVariant; 
                                   out strError: WideString): Integer; safecall;
    function PSoC4_ReadRow(rowID: Integer; out data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC4_ReadProtection(out flashProtect: OleVariant; out chipProtect: OleVariant; 
                                  out strError: WideString): Integer; safecall;
    function PSoC4_ProtectAll(out strError: WideString): Integer; safecall;
    function PSoC4_VerifyProtect(out strError: WideString): Integer; safecall;
    function PSoC4_GetFlashInfo(out rowsPerFlash: Integer; out rowSize: Integer; 
                                out strError: WideString): Integer; safecall;
    function PSoC4_ProgramRowFromHex(rowID: Integer; out strError: WideString): Integer; safecall;
    function PSoC4_VerifyRowFromHex(rowID: Integer; out verResult: Integer; out strError: WideString): Integer; safecall;
    function PSoC4_WriteNvl(data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC4_ReadNvl(out data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC4_GetSiliconID(out siliconID: OleVariant; out strError: WideString): Integer; safecall;
    function HEX_WriteChipProtection(data: OleVariant; out strError: WideString): Integer; safecall;
    function I2C_DataTransfer(deviceAddr: Integer; mode: Integer; readLen: Integer; 
                              dataIN: OleVariant; out dataOUT: OleVariant; out strError: WideString): Integer; safecall;
    function I2C_SetSpeed(speed: enumI2Cspeed; out strError: WideString): Integer; safecall;
    function I2C_GetSpeed(out speed: enumI2Cspeed; out strError: WideString): Integer; safecall;
    function I2C_GetDeviceList(out deviceList: OleVariant; out strError: WideString): Integer; safecall;
    function I2C_ResetBus(out strError: WideString): Integer; safecall;
    function I2C_SendData(deviceAddress: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function I2C_ReadData(deviceAddress: Integer; readSize: Integer; out data: OleVariant; 
                          out strError: WideString): Integer; safecall;
    function I2C_SetTimeout(timeOut: Integer; out strError: WideString): Integer; safecall;
    function I2C_GetTimeout(out timeOut: Integer; out strError: WideString): Integer; safecall;
    function I2C_ReadDataFromReg(deviceAddr: Integer; regAddr: OleVariant; readSize: Integer; 
                                 out data: OleVariant; out strError: WideString): Integer; safecall;
    function USB2IIC_DataTransfer(dataIN: OleVariant; out dataOUT: OleVariant; 
                                  out strError: WideString): Integer; safecall;
    function USB2IIC_SendData(dataIN: OleVariant; out strError: WideString): Integer; safecall;
    function USB2IIC_AsyncMode(fMode: Integer; out strError: WideString): Integer; safecall;
    function USB2IIC_AsyncMode1(fMode: Integer; extra: OleVariant; out strError: WideString): Integer; safecall;
    function SPI_ConfigureBus(bitOrder: enumSpiBitOrder; mode: enumSpiMode; freq: Integer; 
                              extra: OleVariant; out strError: WideString): Integer; safecall;
    function SPI_GetSupportedFreq(out freq: OleVariant; out strError: WideString): Integer; safecall;
    function SPI_DataTransfer(mode: Integer; dataIN: OleVariant; out dataOUT: OleVariant; 
                              out strError: WideString): Integer; safecall;
    function jtagio(read: Byte; tdi_tms: OleVariant; out tdo: OleVariant): Integer; safecall;
    function jtagior(address: Integer; out data: Integer): Integer; safecall;
    function jtagiow(address: Integer; data: Integer): Integer; safecall;
    function swdior(address: Integer; out data: Integer): Integer; safecall;
    function swdiow(address: Integer; data: Integer): Integer; safecall;
    function swdior_raw(input: OleVariant; out output: OleVariant): Integer; safecall;
    function swdiow_raw(input: OleVariant; out output: OleVariant): Integer; safecall;
    function SetMp3Handle(mp3Handle: Integer): Integer; safecall;
    function SetMp3OwnerPID(mp3OwnerPID: Integer): Integer; safecall;
    function SWD_SendPacket(dataIN: OleVariant; out dataOUT: OleVariant; out strError: WideString): Integer; safecall;
    function SWD_LineReset(out strError: WideString): Integer; safecall;
    function SWV_Setup(mode: enumSWVMode; targetFreq: Integer; extra: OleVariant; 
                       out strError: WideString): Integer; safecall;
    function SWV_ReadData(out dataOUT: OleVariant; out strError: WideString): Integer; safecall;
    function HEX_ReadConfig(addr: Integer; size: Integer; out data: OleVariant; 
                            out strError: WideString): Integer; safecall;
    function HEX_ReadExtra(out extra: OleVariant; out strError: WideString): Integer; safecall;
    function HEX_ReadEEPROM(addr: Integer; size: Integer; out data: OleVariant; 
                            out strError: WideString): Integer; safecall;
    function HEX_ReadImageSizes(out flashSize: Integer; out configSize: Integer; 
                                out eepromSize: Integer; out nvlUserSize: Integer; 
                                out nvlWoSize: Integer; out strError: WideString): Integer; safecall;
    function HEX_ReadJtagID(out jtagID: OleVariant; out strError: WideString): Integer; safecall;
    function HEX_ReadNvlCustom(addr: Integer; size: Integer; out data: OleVariant; 
                               out strError: WideString): Integer; safecall;
    function HEX_ReadNvlWo(address: Integer; size: Integer; out data: OleVariant; 
                           out strError: WideString): Integer; safecall;
    function HEX_ReadFile(const filename: WideString; out imageSize: Integer; 
                          out strError: WideString): Integer; safecall;
    function HEX_ReadData(addr: Integer; size: Integer; out data: OleVariant; 
                          out strError: WideString): Integer; safecall;
    function HEX_ReadChecksum(out checksum: Smallint; out strError: WideString): Integer; safecall;
    function HEX_ReadProtection(addr: Integer; size: Integer; out data: OleVariant; 
                                out strError: WideString): Integer; safecall;
    function HEX_WriteData(address: Integer; block: OleVariant; out strError: WideString): Integer; safecall;
    function HEX_WriteProtection(address: Integer; block: OleVariant; out strError: WideString): Integer; safecall;
    function HEX_WriteFile(const filename: WideString; out strError: WideString): Integer; safecall;
    function HEX_FileVersion(out hexFileType: enumHexFileType; out hexFileVersion: SYSUINT; 
                             out strError: WideString): Integer; safecall;
    function HEX_WriteBlock(addr: LongWord; data: OleVariant; out strError: WideString): Integer; safecall;
    function HEX_ReadBlock(addr: LongWord; size: Integer; out data: OleVariant; 
                           out strError: WideString): Integer; safecall;
    function HEX_WriteEEPROM(address: Integer; block: OleVariant; out strError: WideString): Integer; safecall;
    function DAP_Acquire(out strError: WideString): Integer; safecall;
    function DAP_AcquireChip(out strError: WideString): Integer; safecall;
    function DAP_InitChip(out strError: WideString): Integer; safecall;
    function DAP_WriteIO(baseAddr: Integer; data: Integer; out strError: WideString): Integer; safecall;
    function DAP_ReadIO(baseAddr: Integer; out data: Integer; out strError: WideString): Integer; safecall;
    function DAP_PollIO(baseAddr: Integer; expectedData: Integer; timeOut: Integer; 
                        out strError: WideString): Integer; safecall;
    function DAP_PollIO1(baseAddr: Integer; expectedMask: Integer; timeOut: Integer; 
                         out strError: WideString): Integer; safecall;
    function DAP_ReleaseChip(out strError: WideString): Integer; safecall;
    function DAP_GetJtagID(out jtagID: OleVariant; out strError: WideString): Integer; safecall;
    function DAP_AcquireChip1(mode: OleVariant; out strError: WideString): Integer; safecall;
    function DAP_WriteRaw(address: Byte; data: Integer; out status: Byte; out strError: WideString): Integer; safecall;
    function DAP_ReadRaw(address: Byte; out data: Integer; out status: Byte; 
                         out strError: WideString): Integer; safecall;
    function DAP_AcquireChip2(mode: OleVariant; out strError: WideString): Integer; safecall;
    function DAP_JTAGtoSWD(out strError: WideString): Integer; safecall;
    function DAP_SWDtoJTAG(out strError: WideString): Integer; safecall;
    function DAP_JTAGtoDS(out strError: WideString): Integer; safecall;
    function DAP_SWDtoDS(out strError: WideString): Integer; safecall;
    function DAP_DStoSWD(out strError: WideString): Integer; safecall;
    function DAP_DStoJTAG(out strError: WideString): Integer; safecall;
    function GetRowsPerArrayInFlash(out rowsPerArray: Integer; out strError: WideString): Integer; safecall;
    function FL_Init(data: OleVariant; out strError: WideString): Integer; safecall;
    function FL_ProgramPage(pageID: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function FL_UnInit(out strError: WideString): Integer; safecall;
    function FL_ProgramSector(address: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function FM0_FL_ProgramRowFromHex(pageID: Integer; size: Integer; out strError: WideString): Integer; safecall;
    function FM0_VerifyRowFromHexOneRead(rowID: Integer; size: Integer; chipData: OleVariant; 
                                         out verResult: Byte; out strError: WideString): Integer; safecall;
    function FM0_ProgramRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function FM0_EraseAll(out strError: WideString): Integer; safecall;
    function FM0_CheckSum(rowID: Integer; out checksum: Integer; out strError: WideString): Integer; safecall;
    function FM0_ReadRow(rowID: Integer; out data: OleVariant; out strError: WideString): Integer; safecall;
    function FM0_GetFlashInfo(out rowsPerFlash: Integer; out rowSize: Integer; 
                              out strError: WideString): Integer; safecall;
    function FM0_ProgramRowFromHex(rowID: Integer; out strError: WideString): Integer; safecall;
    function FM0_VerifyRowFromHex(rowID: Integer; out verResult: Integer; out strError: WideString): Integer; safecall;
    function FM0_GetSiliconID(out siliconID: OleVariant; out strError: WideString): Integer; safecall;
    function FM0_EraseSector(rowID: Integer; out strError: WideString): Integer; safecall;
    function PSoC6_WriteRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC6_ProgramRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC6_EraseAll(out strError: WideString): Integer; safecall;
    function PSoC6_CheckSum(rowID: Integer; out checksum: Integer; out strError: WideString): Integer; safecall;
    function PSoC6_CheckSumSpecific(flashRegion: Byte; scope: Byte; rowID: Integer; 
                                    out checksum: Integer; out strError: WideString): Integer; safecall;
    function PSoC6_WriteProtection(lifeCycle: Byte; secureRestrict: OleVariant; 
                                   deadRestrict: OleVariant; voltageVerification: WordBool; 
                                   out strError: WideString): Integer; safecall;
    function PSoC6_ReadRow(rowID: Integer; out data: OleVariant; out strError: WideString): Integer; safecall;
    function PSoC6_ReadProtection(out chipProtect: Byte; out strError: WideString): Integer; safecall;
    function PSoC6_ProtectAll(voltageVerification: WordBool; out strError: WideString): Integer; safecall;
    function PSoC6_GetFlashInfo(out rowsPerFlash: Integer; out rowSize: Integer; 
                                out strError: WideString): Integer; safecall;
    function PSoC6_ProgramRowFromHex(hexRowID: Integer; out strError: WideString): Integer; safecall;
    function PSoC6_VerifyRowFromHex(hexRowID: Integer; out verResult: Integer; 
                                    out strError: WideString): Integer; safecall;
    function PSoC6_GetSiliconID(out siliconID: OleVariant; out familyIdHi: Integer; 
                                out familyIdLo: Integer; out revisionIdMaj: Integer; 
                                out revisionIdMin: Integer; out siliconIdHi: Integer; 
                                out siliconIdLo: Integer; out sromFmVersionMaj: Integer; 
                                out sromFmVersionMin: Integer; out protectState: Integer; 
                                out strError: WideString): Integer; safecall;
    function PSoC6_WriteRowFromHex(hexRowID: Integer; out strError: WideString): Integer; safecall;
    function HEX_GetRowAddress(hexRowID: Integer; out rowAddr: Integer; out strError: WideString): Integer; safecall;
    function HEX_GetRowsCount(out rowsPerHex: Integer; out strError: WideString): Integer; safecall;
  end;

// *********************************************************************//
// DispIntf:  IPSoCProgrammerCOM_ObjectDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5C08A7E9-3B1C-47D3-AB19-F47B67C95897}
// *********************************************************************//
  IPSoCProgrammerCOM_ObjectDisp = dispinterface
    ['{5C08A7E9-3B1C-47D3-AB19-F47B67C95897}']
    function Acquire(out strError: WideString): Integer; dispid 1;
    function AcquireChip(out strError: WideString): Integer; dispid 2;
    function AcquireChipWithDelay(delay: Integer; out strError: WideString): Integer; dispid 3;
    function SetAcquireMode(const mode: WideString; out strError: WideString): Integer; dispid 4;
    function GetAcquireMode(out mode: WideString; out strError: WideString): Integer; dispid 5;
    function GetPorts(out ports: OleVariant; out strError: WideString): Integer; dispid 6;
    function ReleaseChip(out strError: WideString): Integer; dispid 7;
    function OpenPort(const port: WideString; out strError: WideString): Integer; dispid 8;
    function GetSiliconId(out siliconID: OleVariant; out strError: WideString): Integer; dispid 9;
    function ClosePort(out strError: WideString): Integer; dispid 10;
    function IsPortOpen(out isOpen: Integer; out strError: WideString): Integer; dispid 11;
    function _StartSelfTerminator(clientProcessID: Integer): Integer; dispid 12;
    function GetProgrammerVersion(out versionString: WideString; out strError: WideString): Integer; dispid 13;
    function SetPowerVoltage(const voltage: WideString; out strError: WideString): Integer; dispid 14;
    function PowerOn(out strError: WideString): Integer; dispid 15;
    function PowerOff(out strError: WideString): Integer; dispid 16;
    function GetPower(out power: Integer; out strError: WideString): Integer; dispid 17;
    function GetProgrammerCapabilities(out caps: OleVariant; out strError: WideString): Integer; dispid 18;
    function ReadHexFile(const filename: WideString; out imageSize: Integer; 
                         out strError: WideString): Integer; dispid 19;
    function ReadHexData(addr: Integer; size: Integer; out data: OleVariant; 
                         out strError: WideString): Integer; dispid 20;
    function ReadHexProtection(addr: Integer; size: Integer; out data: OleVariant; 
                               out strError: WideString): Integer; dispid 21;
    function ReadHexChecksum(out checksum: Word; out strError: WideString): Integer; dispid 22;
    function Calibrate(calibrateIndex: Integer; out strError: WideString): Integer; dispid 23;
    function EraseAll(out strError: WideString): Integer; dispid 24;
    function ProtectAll(out strError: WideString): Integer; dispid 25;
    function Protect(bankID: Integer; out strError: WideString): Integer; dispid 26;
    function VerifyProtect(out strError: WideString): Integer; dispid 27;
    function GetFlashCharacteristics(out blockSize: Integer; out banks: Integer; 
                                     out blocksPerBank: Integer; out strError: WideString): Integer; dispid 28;
    function checksum(blockID: Integer; out checksum: Word; out strError: WideString): Integer; dispid 29;
    function CheckSum1(blockID: Integer; bank: Integer; out checksum: Word; out strError: WideString): Integer; dispid 30;
    function SetBank(bank: Integer; out strError: WideString): Integer; dispid 31;
    function EraseBlock(blockID: Integer; out sscResult: Integer; out strError: WideString): Integer; dispid 32;
    function EraseBlock1(blockID: Integer; bank: Integer; out sscResult: Integer; 
                         out strError: WideString): Integer; dispid 33;
    function ReadBlock(blockID: Integer; out data: OleVariant; out sscResult: Integer; 
                       out strError: WideString): Integer; dispid 34;
    function ReadBlock1(bank: Integer; blockID: Integer; out data: OleVariant; 
                        out sscResult: Integer; out strError: WideString): Integer; dispid 35;
    function WriteBlock(blockID: Integer; block: OleVariant; out sscResult: Integer; 
                        out strError: WideString): Integer; dispid 36;
    function WriteBlock1(bank: Integer; blockID: Integer; block: OleVariant; 
                         out sscResult: Integer; out strError: WideString): Integer; dispid 37;
    function WriteBlockFromHex(hexBlockID: Integer; flashBlockID: Integer; out sscResult: Integer; 
                               out strError: WideString): Integer; dispid 38;
    function WriteBlockFromHex1(hexBlockID: Integer; bank: Integer; flashBlockID: Integer; 
                                out sscResult: Integer; out strError: WideString): Integer; dispid 39;
    function TableRead(tableID: Integer; out XA: Integer; out table: OleVariant; 
                       out sscResult: Integer; out strError: WideString): Integer; dispid 40;
    function VerifyBlock(blockID: Integer; block: OleVariant; out verResult: Integer; 
                         out strError: WideString): Integer; dispid 41;
    function VerifyBlock1(bank: Integer; blockID: Integer; block: OleVariant; 
                          out verResult: Integer; out strError: WideString): Integer; dispid 42;
    function VerifyBlockFromHex(hexBlockID: Integer; out verResult: Integer; 
                                out strError: WideString): Integer; dispid 43;
    function Version: WideString; dispid 44;
    function ReadIO(addr: Integer; size: Integer; out data: OleVariant; out strError: WideString): Integer; dispid 45;
    function WriteIO(addr: Integer; data: OleVariant; out strError: WideString): Integer; dispid 46;
    function ReadRAM(addr: Integer; size: Integer; out data: OleVariant; out strError: WideString): Integer; dispid 47;
    function WriteRAM(addr: Integer; data: OleVariant; out strError: WideString): Integer; dispid 48;
    function TestClock(number: Integer; out strError: WideString): Integer; dispid 49;
    function UpdateProgrammer(const arguments: WideString; out strError: WideString): Integer; dispid 52;
    function UpdateProgrammer1(cmd: Integer; const arguments: WideString; out blockNo: Integer; 
                               out strError: WideString): Integer; dispid 53;
    function GetFamilyList(out familyList: OleVariant; out strError: WideString): Integer; dispid 54;
    function GetDeviceList(const family: WideString; out deviceList: OleVariant; 
                           out strError: WideString): Integer; dispid 55;
    function ReadProtection(bank: Integer; out block: OleVariant; out sscResult: Integer; 
                            out strError: WideString): Integer; dispid 56;
    function RunFile(const scriptFile: WideString; const reportFile: WideString; 
                     out strError: WideString): Integer; dispid 57;
    function RunConsole(const cmd: WideString; out strError: WideString): Integer; dispid 58;
    function GetDeviceMatchedList(siliconID: OleVariant; out deviceList: OleVariant; 
                                  out strError: WideString): Integer; dispid 59;
    function GetDeviceInfo(const devNameDisp: WideString; out family: WideString; 
                           out familyCode: Integer; out pins: Integer; out flashSize: Integer; 
                           out acquireModes: Integer; out siliconIDs: OleVariant; 
                           out strError: WideString): Integer; dispid 60;
    function WriteHexFile(const filename: WideString; out strError: WideString): Integer; dispid 61;
    function WriteHexData(address: Integer; block: OleVariant; out strError: WideString): Integer; dispid 62;
    function WriteHexProtection(address: Integer; block: OleVariant; out strError: WideString): Integer; dispid 63;
    function SetChipType(familyID: Integer; out strError: WideString): Integer; dispid 64;
    function GetFamilyInfo(const familyName: WideString; 
                           out familyInfo: {NOT_OLEAUTO(CFamilyInfo)}OleVariant; 
                           out strError: WideString): Integer; dispid 65;
    function GetDeviceInfo1(const devNameDisp: WideString; 
                            out deviceInfo: {NOT_OLEAUTO(CDeviceInfo)}OleVariant; 
                            out strError: WideString): Integer; dispid 66;
    function GetPowerVoltage(out voltage: WideString; out strError: WideString): Integer; dispid 67;
    function GetPower1(out power: Integer; out voltage: Integer; out strError: WideString): Integer; dispid 68;
    function SetProtocol(protocol: enumInterfaces; out strError: WideString): Integer; dispid 69;
    function SetProtocolConnector(connector: Integer; out strError: WideString): Integer; dispid 70;
    function SetProtocolClock(clock: enumFrequencies; out strError: WideString): Integer; dispid 71;
    function GetProgrammerCapsByName(const portName: WideString; out caps: OleVariant; 
                                     out strError: WideString): Integer; dispid 72;
    function ProgrammerLedState(ledNo: Integer; ledState: Integer; out strError: WideString): Integer; dispid 73;
    function JTAG_EnumerateDevices(out devices: OleVariant; out strError: WideString): Integer; dispid 74;
    function SetAutoReset(mode: Integer; out strError: WideString): Integer; dispid 75;
    function GetPower2(out power: Integer; out voltage1: Integer; out voltage2: Integer; 
                       out strError: WideString): Integer; dispid 76;
    function CheckSum_(blockID: Integer; out checksum: Smallint; out strError: WideString): Integer; dispid 77;
    function CheckSum1_(blockID: Integer; bank: Integer; out checksum: Smallint; 
                        out strError: WideString): Integer; dispid 78;
    function ReadHexChecksum_(out checksum: Smallint; out strError: WideString): Integer; dispid 79;
    function GetProgrammerRevID(out revID: Integer; out strError: WideString): Integer; dispid 80;
    function ToggleReset(polarity: Integer; duration: Integer; out strError: WideString): Integer; dispid 81;
    function JTAG_SetChainConfig(selectedDevice: Byte; Ir: OleVariant; out strError: WideString): Integer; dispid 82;
    function JTAG_SetIR(Ir: Integer; out strError: WideString): Integer; dispid 83;
    function JTAG_ShiftDR(drSize: Integer; drIn: OleVariant; out drOut: OleVariant; 
                          out strError: WideString): Integer; dispid 84;
    function JTAG_ShiftIR(irSize: Integer; irIn: OleVariant; out irOut: OleVariant; 
                          out strError: WideString): Integer; dispid 85;
    function GetDeviceListBySiliconID(siliconID: OleVariant; ignoreRevisionID: Byte; 
                                      out deviceList: OleVariant; out strError: WideString): Integer; dispid 90;
    function HEX_CompatibleWithSupersetPart(idType: enumChipIDType; hexID: OleVariant; 
                                            chipID: OleVariant; out compatible: WordBool; 
                                            out strError: WideString): Integer; dispid 91;
    function PSoC3_SetJtagChainConfig(selectedDevice: Byte; IrDr: OleVariant; 
                                      out strError: WideString): Integer; dispid 501;
    function PSoC3_SetProtocolClock(clock: enumFrequencies; out strError: WideString): Integer; dispid 502;
    function PSoC3_GetSonosArrays(arrayGroup: enumSonosArrays; out arrays: OleVariant; 
                                  out strError: WideString): Integer; dispid 503;
    function PSoC3_GetFlashArrayInfo(arrayID: Integer; out rowSize: Integer; 
                                     out rowsPerArray: Integer; out eccPresence: Integer; 
                                     out strError: WideString): Integer; dispid 504;
    function PSoC3_WriteIO(baseAddr: Integer; data: Integer; out strError: WideString): Integer; dispid 505;
    function PSoC3_ReadIO(baseAddr: Integer; out data: Integer; out strError: WideString): Integer; dispid 506;
    function PSoC3_PollIO(baseAddr: Integer; expectedData: Integer; timeOut: Integer; 
                          out strError: WideString): Integer; dispid 507;
    function PSoC3_PollIO1(baseAddr: Integer; expectedMask: Integer; timeOut: Integer; 
                           out strError: WideString): Integer; dispid 508;
    function PSoC3_EraseAll(out strError: WideString): Integer; dispid 509;
    function PSoC3_WriteNvlArray(arrayID: Integer; data: OleVariant; out strError: WideString): Integer; dispid 510;
    function PSoC3_GetJtagID(out jtagID: OleVariant; out strError: WideString): Integer; dispid 511;
    function PSoC3_ReadHexImageSizes(out flashSize: Integer; out configSize: Integer; 
                                     out eepromSize: Integer; out nvlUserSize: Integer; 
                                     out nvlWoSize: Integer; out strError: WideString): Integer; dispid 512;
    function PSoC3_ReadHexJtagID(out jtagID: OleVariant; out strError: WideString): Integer; dispid 513;
    function PSoC3_ReadHexNvlCustom(addr: Integer; size: Integer; out data: OleVariant; 
                                    out strError: WideString): Integer; dispid 514;
    function PSoC3_ReadHexNvlWo(address: Integer; size: Integer; out data: OleVariant; 
                                out strError: WideString): Integer; dispid 515;
    function PSoC3_ReadHexEEPROM(addr: Integer; size: Integer; out data: OleVariant; 
                                 out strError: WideString): Integer; dispid 516;
    function PSoC3_ReadHexConfig(addr: Integer; size: Integer; out data: OleVariant; 
                                 out strError: WideString): Integer; dispid 517;
    function PSoC3_ReadNvlArray(arrayID: Integer; out data: OleVariant; out strError: WideString): Integer; dispid 518;
    function PSoC3_GetEccStatus(out status: Integer; out strError: WideString): Integer; dispid 519;
    function PSoC3_ProgramRow(arrayID: Integer; rowID: Integer; data: OleVariant; 
                              out strError: WideString): Integer; dispid 520;
    function PSoC3_ReadRow(arrayID: Integer; rowID: Integer; eccSpace: Integer; 
                           out data: OleVariant; out strError: WideString): Integer; dispid 521;
    function PSoC3_ProtectArray(arrayID: Integer; data: OleVariant; out strError: WideString): Integer; dispid 522;
    function PSoC3_ProtectAll(out strError: WideString): Integer; dispid 523;
    function PSoC3_ReadProtection(arrayID: Integer; out data: OleVariant; out strError: WideString): Integer; dispid 524;
    function PSoC3_VerifyProtect(out strError: WideString): Integer; dispid 525;
    function PSoC3_CheckSum(arrayID: Integer; startRowID: Integer; noOfRows: Integer; 
                            out checksum: LongWord; out strError: WideString): Integer; dispid 526;
    function PSoC3_ProgramRowFromHex(arrayID: Integer; rowID: Integer; eccOption: Integer; 
                                     out strError: WideString): Integer; dispid 527;
    function PSoC3_VerifyRowFromHex(arrayID: Integer; rowID: Integer; eccOption: Integer; 
                                    out verResult: Integer; out strError: WideString): Integer; dispid 528;
    function PSoC3_WriteRow(arrayID: Integer; rowID: Integer; data: OleVariant; 
                            out strError: WideString): Integer; dispid 529;
    function PSoC3_AcquireChip(out strError: WideString): Integer; dispid 530;
    function PSoC3_ReleaseChip(out strError: WideString): Integer; dispid 531;
    function PSoC3_DebugPortConfig(transferMode: Integer; out strError: WideString): Integer; dispid 532;
    function PSoC3_ProgramRowFast(arrayID: Integer; rowID: Integer; eccOption: Integer; 
                                  out strError: WideString): Integer; dispid 533;
    function PSoC3_Acquire(out strError: WideString): Integer; dispid 534;
    function PSoC3_ReadHexExtra(out extra: OleVariant; out strError: WideString): Integer; dispid 535;
    function PSoC3_InitChip(out strError: WideString): Integer; dispid 536;
    function PSoC3_EraseRow(arrayID: Integer; rowID: Integer; out strError: WideString): Integer; dispid 537;
    function PSoC3_CheckSum_(arrayID: Integer; startRowID: Integer; noOfRows: Integer; 
                             out checksum: Integer; out strError: WideString): Integer; dispid 538;
    function PSoC3_GetEepromArrayInfo(arrayID: Integer; out rowSize: Integer; 
                                      out rowsPerArray: Integer; out strError: WideString): Integer; dispid 539;
    function PSoC3_EraseSector(arrayID: Integer; sectorID: Integer; out strError: WideString): Integer; dispid 540;
    function HEX_ReadChipProtection(out data: OleVariant; out strError: WideString): Integer; dispid 600;
    function PSoC4_WriteRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; dispid 601;
    function PSoC4_ProgramRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; dispid 602;
    function PSoC4_EraseAll(out strError: WideString): Integer; dispid 603;
    function PSoC4_CheckSum(rowID: Integer; out checksum: Integer; out strError: WideString): Integer; dispid 604;
    function PSoC4_WriteProtection(flashProtect: OleVariant; chipProtect: OleVariant; 
                                   out strError: WideString): Integer; dispid 605;
    function PSoC4_ReadRow(rowID: Integer; out data: OleVariant; out strError: WideString): Integer; dispid 606;
    function PSoC4_ReadProtection(out flashProtect: OleVariant; out chipProtect: OleVariant; 
                                  out strError: WideString): Integer; dispid 608;
    function PSoC4_ProtectAll(out strError: WideString): Integer; dispid 609;
    function PSoC4_VerifyProtect(out strError: WideString): Integer; dispid 610;
    function PSoC4_GetFlashInfo(out rowsPerFlash: Integer; out rowSize: Integer; 
                                out strError: WideString): Integer; dispid 611;
    function PSoC4_ProgramRowFromHex(rowID: Integer; out strError: WideString): Integer; dispid 612;
    function PSoC4_VerifyRowFromHex(rowID: Integer; out verResult: Integer; out strError: WideString): Integer; dispid 613;
    function PSoC4_WriteNvl(data: OleVariant; out strError: WideString): Integer; dispid 614;
    function PSoC4_ReadNvl(out data: OleVariant; out strError: WideString): Integer; dispid 615;
    function PSoC4_GetSiliconID(out siliconID: OleVariant; out strError: WideString): Integer; dispid 616;
    function HEX_WriteChipProtection(data: OleVariant; out strError: WideString): Integer; dispid 617;
    function I2C_DataTransfer(deviceAddr: Integer; mode: Integer; readLen: Integer; 
                              dataIN: OleVariant; out dataOUT: OleVariant; out strError: WideString): Integer; dispid 1000;
    function I2C_SetSpeed(speed: enumI2Cspeed; out strError: WideString): Integer; dispid 1001;
    function I2C_GetSpeed(out speed: enumI2Cspeed; out strError: WideString): Integer; dispid 1002;
    function I2C_GetDeviceList(out deviceList: OleVariant; out strError: WideString): Integer; dispid 1003;
    function I2C_ResetBus(out strError: WideString): Integer; dispid 1004;
    function I2C_SendData(deviceAddress: Integer; data: OleVariant; out strError: WideString): Integer; dispid 1005;
    function I2C_ReadData(deviceAddress: Integer; readSize: Integer; out data: OleVariant; 
                          out strError: WideString): Integer; dispid 1006;
    function I2C_SetTimeout(timeOut: Integer; out strError: WideString): Integer; dispid 1007;
    function I2C_GetTimeout(out timeOut: Integer; out strError: WideString): Integer; dispid 1008;
    function I2C_ReadDataFromReg(deviceAddr: Integer; regAddr: OleVariant; readSize: Integer; 
                                 out data: OleVariant; out strError: WideString): Integer; dispid 1009;
    function USB2IIC_DataTransfer(dataIN: OleVariant; out dataOUT: OleVariant; 
                                  out strError: WideString): Integer; dispid 1100;
    function USB2IIC_SendData(dataIN: OleVariant; out strError: WideString): Integer; dispid 1101;
    function USB2IIC_AsyncMode(fMode: Integer; out strError: WideString): Integer; dispid 1102;
    function USB2IIC_AsyncMode1(fMode: Integer; extra: OleVariant; out strError: WideString): Integer; dispid 1103;
    function SPI_ConfigureBus(bitOrder: enumSpiBitOrder; mode: enumSpiMode; freq: Integer; 
                              extra: OleVariant; out strError: WideString): Integer; dispid 1200;
    function SPI_GetSupportedFreq(out freq: OleVariant; out strError: WideString): Integer; dispid 1201;
    function SPI_DataTransfer(mode: Integer; dataIN: OleVariant; out dataOUT: OleVariant; 
                              out strError: WideString): Integer; dispid 1202;
    function jtagio(read: Byte; tdi_tms: OleVariant; out tdo: OleVariant): Integer; dispid 2010;
    function jtagior(address: Integer; out data: Integer): Integer; dispid 2011;
    function jtagiow(address: Integer; data: Integer): Integer; dispid 2012;
    function swdior(address: Integer; out data: Integer): Integer; dispid 2013;
    function swdiow(address: Integer; data: Integer): Integer; dispid 2014;
    function swdior_raw(input: OleVariant; out output: OleVariant): Integer; dispid 2015;
    function swdiow_raw(input: OleVariant; out output: OleVariant): Integer; dispid 2016;
    function SetMp3Handle(mp3Handle: Integer): Integer; dispid 2017;
    function SetMp3OwnerPID(mp3OwnerPID: Integer): Integer; dispid 2018;
    function SWD_SendPacket(dataIN: OleVariant; out dataOUT: OleVariant; out strError: WideString): Integer; dispid 2019;
    function SWD_LineReset(out strError: WideString): Integer; dispid 2020;
    function SWV_Setup(mode: enumSWVMode; targetFreq: Integer; extra: OleVariant; 
                       out strError: WideString): Integer; dispid 2021;
    function SWV_ReadData(out dataOUT: OleVariant; out strError: WideString): Integer; dispid 2022;
    function HEX_ReadConfig(addr: Integer; size: Integer; out data: OleVariant; 
                            out strError: WideString): Integer; dispid 3010;
    function HEX_ReadExtra(out extra: OleVariant; out strError: WideString): Integer; dispid 3011;
    function HEX_ReadEEPROM(addr: Integer; size: Integer; out data: OleVariant; 
                            out strError: WideString): Integer; dispid 3012;
    function HEX_ReadImageSizes(out flashSize: Integer; out configSize: Integer; 
                                out eepromSize: Integer; out nvlUserSize: Integer; 
                                out nvlWoSize: Integer; out strError: WideString): Integer; dispid 3013;
    function HEX_ReadJtagID(out jtagID: OleVariant; out strError: WideString): Integer; dispid 3014;
    function HEX_ReadNvlCustom(addr: Integer; size: Integer; out data: OleVariant; 
                               out strError: WideString): Integer; dispid 3015;
    function HEX_ReadNvlWo(address: Integer; size: Integer; out data: OleVariant; 
                           out strError: WideString): Integer; dispid 3016;
    function HEX_ReadFile(const filename: WideString; out imageSize: Integer; 
                          out strError: WideString): Integer; dispid 3017;
    function HEX_ReadData(addr: Integer; size: Integer; out data: OleVariant; 
                          out strError: WideString): Integer; dispid 3018;
    function HEX_ReadChecksum(out checksum: Smallint; out strError: WideString): Integer; dispid 3019;
    function HEX_ReadProtection(addr: Integer; size: Integer; out data: OleVariant; 
                                out strError: WideString): Integer; dispid 3020;
    function HEX_WriteData(address: Integer; block: OleVariant; out strError: WideString): Integer; dispid 3021;
    function HEX_WriteProtection(address: Integer; block: OleVariant; out strError: WideString): Integer; dispid 3022;
    function HEX_WriteFile(const filename: WideString; out strError: WideString): Integer; dispid 3023;
    function HEX_FileVersion(out hexFileType: enumHexFileType; out hexFileVersion: SYSUINT; 
                             out strError: WideString): Integer; dispid 3024;
    function HEX_WriteBlock(addr: LongWord; data: OleVariant; out strError: WideString): Integer; dispid 3025;
    function HEX_ReadBlock(addr: LongWord; size: Integer; out data: OleVariant; 
                           out strError: WideString): Integer; dispid 3026;
    function HEX_WriteEEPROM(address: Integer; block: OleVariant; out strError: WideString): Integer; dispid 3027;
    function DAP_Acquire(out strError: WideString): Integer; dispid 3100;
    function DAP_AcquireChip(out strError: WideString): Integer; dispid 3101;
    function DAP_InitChip(out strError: WideString): Integer; dispid 3102;
    function DAP_WriteIO(baseAddr: Integer; data: Integer; out strError: WideString): Integer; dispid 3103;
    function DAP_ReadIO(baseAddr: Integer; out data: Integer; out strError: WideString): Integer; dispid 3104;
    function DAP_PollIO(baseAddr: Integer; expectedData: Integer; timeOut: Integer; 
                        out strError: WideString): Integer; dispid 3105;
    function DAP_PollIO1(baseAddr: Integer; expectedMask: Integer; timeOut: Integer; 
                         out strError: WideString): Integer; dispid 3106;
    function DAP_ReleaseChip(out strError: WideString): Integer; dispid 3107;
    function DAP_GetJtagID(out jtagID: OleVariant; out strError: WideString): Integer; dispid 3108;
    function DAP_AcquireChip1(mode: OleVariant; out strError: WideString): Integer; dispid 3109;
    function DAP_WriteRaw(address: Byte; data: Integer; out status: Byte; out strError: WideString): Integer; dispid 3110;
    function DAP_ReadRaw(address: Byte; out data: Integer; out status: Byte; 
                         out strError: WideString): Integer; dispid 3111;
    function DAP_AcquireChip2(mode: OleVariant; out strError: WideString): Integer; dispid 3112;
    function DAP_JTAGtoSWD(out strError: WideString): Integer; dispid 3113;
    function DAP_SWDtoJTAG(out strError: WideString): Integer; dispid 3114;
    function DAP_JTAGtoDS(out strError: WideString): Integer; dispid 3115;
    function DAP_SWDtoDS(out strError: WideString): Integer; dispid 3116;
    function DAP_DStoSWD(out strError: WideString): Integer; dispid 3117;
    function DAP_DStoJTAG(out strError: WideString): Integer; dispid 3118;
    function GetRowsPerArrayInFlash(out rowsPerArray: Integer; out strError: WideString): Integer; dispid 3119;
    function FL_Init(data: OleVariant; out strError: WideString): Integer; dispid 3120;
    function FL_ProgramPage(pageID: Integer; data: OleVariant; out strError: WideString): Integer; dispid 3121;
    function FL_UnInit(out strError: WideString): Integer; dispid 3122;
    function FL_ProgramSector(address: Integer; data: OleVariant; out strError: WideString): Integer; dispid 3123;
    function FM0_FL_ProgramRowFromHex(pageID: Integer; size: Integer; out strError: WideString): Integer; dispid 3124;
    function FM0_VerifyRowFromHexOneRead(rowID: Integer; size: Integer; chipData: OleVariant; 
                                         out verResult: Byte; out strError: WideString): Integer; dispid 3125;
    function FM0_ProgramRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; dispid 3126;
    function FM0_EraseAll(out strError: WideString): Integer; dispid 3127;
    function FM0_CheckSum(rowID: Integer; out checksum: Integer; out strError: WideString): Integer; dispid 3128;
    function FM0_ReadRow(rowID: Integer; out data: OleVariant; out strError: WideString): Integer; dispid 3129;
    function FM0_GetFlashInfo(out rowsPerFlash: Integer; out rowSize: Integer; 
                              out strError: WideString): Integer; dispid 3130;
    function FM0_ProgramRowFromHex(rowID: Integer; out strError: WideString): Integer; dispid 3131;
    function FM0_VerifyRowFromHex(rowID: Integer; out verResult: Integer; out strError: WideString): Integer; dispid 3132;
    function FM0_GetSiliconID(out siliconID: OleVariant; out strError: WideString): Integer; dispid 3133;
    function FM0_EraseSector(rowID: Integer; out strError: WideString): Integer; dispid 3134;
    function PSoC6_WriteRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; dispid 3135;
    function PSoC6_ProgramRow(rowID: Integer; data: OleVariant; out strError: WideString): Integer; dispid 3136;
    function PSoC6_EraseAll(out strError: WideString): Integer; dispid 3137;
    function PSoC6_CheckSum(rowID: Integer; out checksum: Integer; out strError: WideString): Integer; dispid 3138;
    function PSoC6_CheckSumSpecific(flashRegion: Byte; scope: Byte; rowID: Integer; 
                                    out checksum: Integer; out strError: WideString): Integer; dispid 3139;
    function PSoC6_WriteProtection(lifeCycle: Byte; secureRestrict: OleVariant; 
                                   deadRestrict: OleVariant; voltageVerification: WordBool; 
                                   out strError: WideString): Integer; dispid 3140;
    function PSoC6_ReadRow(rowID: Integer; out data: OleVariant; out strError: WideString): Integer; dispid 3141;
    function PSoC6_ReadProtection(out chipProtect: Byte; out strError: WideString): Integer; dispid 3142;
    function PSoC6_ProtectAll(voltageVerification: WordBool; out strError: WideString): Integer; dispid 3143;
    function PSoC6_GetFlashInfo(out rowsPerFlash: Integer; out rowSize: Integer; 
                                out strError: WideString): Integer; dispid 3144;
    function PSoC6_ProgramRowFromHex(hexRowID: Integer; out strError: WideString): Integer; dispid 3145;
    function PSoC6_VerifyRowFromHex(hexRowID: Integer; out verResult: Integer; 
                                    out strError: WideString): Integer; dispid 3146;
    function PSoC6_GetSiliconID(out siliconID: OleVariant; out familyIdHi: Integer; 
                                out familyIdLo: Integer; out revisionIdMaj: Integer; 
                                out revisionIdMin: Integer; out siliconIdHi: Integer; 
                                out siliconIdLo: Integer; out sromFmVersionMaj: Integer; 
                                out sromFmVersionMin: Integer; out protectState: Integer; 
                                out strError: WideString): Integer; dispid 3147;
    function PSoC6_WriteRowFromHex(hexRowID: Integer; out strError: WideString): Integer; dispid 3148;
    function HEX_GetRowAddress(hexRowID: Integer; out rowAddr: Integer; out strError: WideString): Integer; dispid 3160;
    function HEX_GetRowsCount(out rowsPerHex: Integer; out strError: WideString): Integer; dispid 3168;
  end;

// *********************************************************************//
// The Class CoPSoCProgrammerCOM_Object provides a Create and CreateRemote method to          
// create instances of the default interface IPSoCProgrammerCOM_Object exposed by              
// the CoClass PSoCProgrammerCOM_Object. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPSoCProgrammerCOM_Object = class
    class function Create: IPSoCProgrammerCOM_Object;
    class function CreateRemote(const MachineName: string): IPSoCProgrammerCOM_Object;
  end;

implementation

uses System.Win.ComObj;

class function CoPSoCProgrammerCOM_Object.Create: IPSoCProgrammerCOM_Object;
begin
  Result := CreateComObject(CLASS_PSoCProgrammerCOM_Object) as IPSoCProgrammerCOM_Object;
end;

class function CoPSoCProgrammerCOM_Object.CreateRemote(const MachineName: string): IPSoCProgrammerCOM_Object;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PSoCProgrammerCOM_Object) as IPSoCProgrammerCOM_Object;
end;

end.
