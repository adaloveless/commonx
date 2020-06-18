unit RDPCOMAPILib_TLB;

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
// File generated on 5/5/2020 2:30:42 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Windows\SysWOW64\rdpsharercom.dll (1)
// LIBID: {CC802D05-AE07-4C15-B496-DB9D22AA0A84}
// LCID: 0
// Helpfile: 
// HelpString: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Hint: Member 'Property' of 'IRDPSRAPISessionProperties' changed to 'Property_'
//   Error creating palette bitmap of (TRDPViewer) : Registry key CLSID\{32BE5ED2-5C86-480F-A914-0FF8885A1B3F}\ToolboxBitmap32 not found
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleCtrls, Vcl.OleServer, Winapi.ActiveX;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  RDPCOMAPILibMajorVersion = 1;
  RDPCOMAPILibMinorVersion = 0;

  LIBID_RDPCOMAPILib: TGUID = '{CC802D05-AE07-4C15-B496-DB9D22AA0A84}';

  DIID__IRDPSessionEvents: TGUID = '{98A97042-6698-40E9-8EFD-B3200990004B}';
  IID_IRDPViewerRenderingSurface: TGUID = '{56BFCE32-83E9-414D-82E8-F31D01C62CB5}';
  IID_IRDPViewerInputSink: TGUID = '{BB590853-A6C5-4A7B-8DD4-76B69EEA12D5}';
  IID_IRDPSRAPIAudioStream: TGUID = '{E3E30EF9-89C6-4541-BA3B-19336AC6D31C}';
  IID_IRDPSRAPIPerfCounterLoggingManager: TGUID = '{9A512C86-AC6E-4A8E-B1A4-FCEF363F6E64}';
  IID_IRDPSRAPIPerfCounterLogger: TGUID = '{071C2533-0FA4-4E8F-AE83-9C10B4305AB5}';
  IID_IRDPSRAPIViewer: TGUID = '{C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}';
  CLASS_RDPViewer: TGUID = '{32BE5ED2-5C86-480F-A914-0FF8885A1B3F}';
  IID_IRDPSRAPIAttendeeManager: TGUID = '{BA3A37E8-33DA-4749-8DA0-07FA34DA7944}';
  IID_IRDPSRAPIAttendee: TGUID = '{EC0671B3-1B78-4B80-A464-9132247543E3}';
  IID_IRDPSRAPIInvitation: TGUID = '{4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}';
  IID_IRDPSRAPIInvitationManager: TGUID = '{4722B049-92C3-4C2D-8A65-F7348F644DCF}';
  IID_IRDPSRAPIApplicationFilter: TGUID = '{D20F10CA-6637-4F06-B1D5-277EA7E5160D}';
  IID_IRDPSRAPIApplicationList: TGUID = '{D4B4AEB3-22DC-4837-B3B6-42EA2517849A}';
  IID_IRDPSRAPIApplication: TGUID = '{41E7A09D-EB7A-436E-935D-780CA2628324}';
  IID_IRDPSRAPIWindowList: TGUID = '{8A05CE44-715A-4116-A189-A118F30A07BD}';
  IID_IRDPSRAPIWindow: TGUID = '{BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}';
  IID_IRDPSRAPIVirtualChannelManager: TGUID = '{0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}';
  IID_IRDPSRAPIVirtualChannel: TGUID = '{05E12F95-28B3-4C9A-8780-D0248574A1E0}';
  IID_IRDPSRAPISessionProperties: TGUID = '{339B24F2-9BC0-4F16-9AAC-F165433D13D4}';
  CLASS_RDPSRAPISessionProperties: TGUID = '{DD7594FF-EA2A-4C06-8FDF-132DE48B6510}';
  CLASS_RDPSRAPIInvitationManager: TGUID = '{53D9C9DB-75AB-4271-948A-4C4EB36A8F2B}';
  CLASS_RDPSRAPIInvitation: TGUID = '{49174DC6-0731-4B5E-8EE1-83A63D3868FA}';
  CLASS_RDPSRAPIAttendeeManager: TGUID = '{D7B13A01-F7D4-42A6-8595-12FC8C24E851}';
  CLASS_RDPSRAPIAttendee: TGUID = '{74F93BB5-755F-488E-8A29-2390108AEF55}';
  IID_IRDPSRAPIAttendeeDisconnectInfo: TGUID = '{C187689F-447C-44A1-9C14-FFFBB3B7EC17}';
  CLASS_RDPSRAPIAttendeeDisconnectInfo: TGUID = '{B47D7250-5BDB-405D-B487-CAAD9C56F4F8}';
  CLASS_RDPSRAPIApplicationFilter: TGUID = '{E35ACE89-C7E8-427E-A4F9-B9DA072826BD}';
  CLASS_RDPSRAPIApplicationList: TGUID = '{9E31C815-7433-4876-97FB-ED59FE2BAA22}';
  CLASS_RDPSRAPIApplication: TGUID = '{C116A484-4B25-4B9F-8A54-B934B06E57FA}';
  CLASS_RDPSRAPIWindowList: TGUID = '{9C21E2B8-5DD4-42CC-81BA-1C099852E6FA}';
  CLASS_RDPSRAPIWindow: TGUID = '{03CF46DB-CE45-4D36-86ED-ED28B74398BF}';
  IID_IRDPSRAPITcpConnectionInfo: TGUID = '{F74049A4-3D06-4028-8193-0A8C29BC2452}';
  CLASS_RDPSRAPITcpConnectionInfo: TGUID = '{BE49DB3F-EBB6-4278-8CE0-D5455833EAEE}';
  IID_IRDPSRAPISharingSession: TGUID = '{EEB20886-E470-4CF6-842B-2739C0EC5CFB}';
  IID_IRDPSRAPISharingSession2: TGUID = '{FEE4EE57-E3E8-4205-8FB0-8FD1D0675C21}';
  CLASS_RDPSession: TGUID = '{9B78F0E6-3E05-4A5B-B2E8-E743A8956B65}';
  IID_IRDPSRAPITransportStream: TGUID = '{36CFA065-43BB-4EF7-AED7-9B88A5053036}';
  IID_IRDPSRAPITransportStreamBuffer: TGUID = '{81C80290-5085-44B0-B460-F865C39CB4A9}';
  IID_IRDPSRAPITransportStreamEvents: TGUID = '{EA81C254-F5AF-4E40-982E-3E63BB595276}';
  IID_IRDPSRAPIFrameBuffer: TGUID = '{3D67E7D2-B27B-448E-81B3-C6110ED8B4BE}';
  CLASS_RDPSRAPIFrameBuffer: TGUID = '{A4F66BCC-538E-4101-951D-30847ADB5101}';
  CLASS_RDPTransportStreamBuffer: TGUID = '{8D4A1C69-F17F-4549-A699-761C6E6B5C0A}';
  CLASS_RDPTransportStreamEvents: TGUID = '{31E3AB20-5350-483F-9DC6-6748665EFDEB}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0028_0001
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0028_0001 = TOleEnum;
const
  CONST_MAX_CHANNEL_MESSAGE_SIZE = $00000400;
  CONST_MAX_CHANNEL_NAME_LEN = $00000008;
  CONST_MAX_LEGACY_CHANNEL_MESSAGE_SIZE = $00064000;
  CONST_ATTENDEE_ID_EVERYONE = $FFFFFFFF;
  CONST_ATTENDEE_ID_HOST = $00000000;
  CONST_CONN_INTERVAL = $00000032;
  CONST_ATTENDEE_ID_DEFAULT = $FFFFFFFF;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0001
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0001 = TOleEnum;
const
  CTRL_LEVEL_MIN = $00000000;
  CTRL_LEVEL_INVALID = $00000000;
  CTRL_LEVEL_NONE = $00000001;
  CTRL_LEVEL_VIEW = $00000002;
  CTRL_LEVEL_INTERACTIVE = $00000003;
  CTRL_LEVEL_REQCTRL_VIEW = $00000004;
  CTRL_LEVEL_REQCTRL_INTERACTIVE = $00000005;
  CTRL_LEVEL_MAX = $00000005;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0002
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0002 = TOleEnum;
const
  ATTENDEE_DISCONNECT_REASON_MIN = $00000000;
  ATTENDEE_DISCONNECT_REASON_APP = $00000000;
  ATTENDEE_DISCONNECT_REASON_ERR = $00000001;
  ATTENDEE_DISCONNECT_REASON_CLI = $00000002;
  ATTENDEE_DISCONNECT_REASON_MAX = $00000002;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0003
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0003 = TOleEnum;
const
  CHANNEL_PRIORITY_LO = $00000000;
  CHANNEL_PRIORITY_MED = $00000001;
  CHANNEL_PRIORITY_HI = $00000002;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0004
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0004 = TOleEnum;
const
  CHANNEL_FLAGS_LEGACY = $00000001;
  CHANNEL_FLAGS_UNCOMPRESSED = $00000002;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0005
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0005 = TOleEnum;
const
  CHANNEL_ACCESS_ENUM_NONE = $00000000;
  CHANNEL_ACCESS_ENUM_SENDRECEIVE = $00000001;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0006
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0006 = TOleEnum;
const
  ATTENDEE_FLAGS_LOCAL = $00000001;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0007
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0007 = TOleEnum;
const
  WND_FLAG_PRIVILEGED = $00000001;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0008
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0008 = TOleEnum;
const
  APP_FLAG_PRIVILEGED = $00000001;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0009
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0009 = TOleEnum;
const
  RDPSRAPI_MOUSE_BUTTON_BUTTON1 = $00000000;
  RDPSRAPI_MOUSE_BUTTON_BUTTON2 = $00000001;
  RDPSRAPI_MOUSE_BUTTON_BUTTON3 = $00000002;
  RDPSRAPI_MOUSE_BUTTON_XBUTTON1 = $00000003;
  RDPSRAPI_MOUSE_BUTTON_XBUTTON2 = $00000004;
  RDPSRAPI_MOUSE_BUTTON_XBUTTON3 = $00000005;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0000_0010
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0000_0010 = TOleEnum;
const
  RDPSRAPI_KBD_CODE_SCANCODE = $00000000;
  RDPSRAPI_KBD_CODE_UNICODE = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _IRDPSessionEvents = dispinterface;
  IRDPViewerRenderingSurface = interface;
  IRDPViewerInputSink = interface;
  IRDPSRAPIAudioStream = interface;
  IRDPSRAPIPerfCounterLoggingManager = interface;
  IRDPSRAPIPerfCounterLogger = interface;
  IRDPSRAPIViewer = interface;
  IRDPSRAPIViewerDisp = dispinterface;
  IRDPSRAPIAttendeeManager = interface;
  IRDPSRAPIAttendeeManagerDisp = dispinterface;
  IRDPSRAPIAttendee = interface;
  IRDPSRAPIAttendeeDisp = dispinterface;
  IRDPSRAPIInvitation = interface;
  IRDPSRAPIInvitationDisp = dispinterface;
  IRDPSRAPIInvitationManager = interface;
  IRDPSRAPIInvitationManagerDisp = dispinterface;
  IRDPSRAPIApplicationFilter = interface;
  IRDPSRAPIApplicationFilterDisp = dispinterface;
  IRDPSRAPIApplicationList = interface;
  IRDPSRAPIApplicationListDisp = dispinterface;
  IRDPSRAPIApplication = interface;
  IRDPSRAPIApplicationDisp = dispinterface;
  IRDPSRAPIWindowList = interface;
  IRDPSRAPIWindowListDisp = dispinterface;
  IRDPSRAPIWindow = interface;
  IRDPSRAPIWindowDisp = dispinterface;
  IRDPSRAPIVirtualChannelManager = interface;
  IRDPSRAPIVirtualChannelManagerDisp = dispinterface;
  IRDPSRAPIVirtualChannel = interface;
  IRDPSRAPIVirtualChannelDisp = dispinterface;
  IRDPSRAPISessionProperties = interface;
  IRDPSRAPISessionPropertiesDisp = dispinterface;
  IRDPSRAPIAttendeeDisconnectInfo = interface;
  IRDPSRAPIAttendeeDisconnectInfoDisp = dispinterface;
  IRDPSRAPITcpConnectionInfo = interface;
  IRDPSRAPITcpConnectionInfoDisp = dispinterface;
  IRDPSRAPISharingSession = interface;
  IRDPSRAPISharingSessionDisp = dispinterface;
  IRDPSRAPISharingSession2 = interface;
  IRDPSRAPISharingSession2Disp = dispinterface;
  IRDPSRAPITransportStream = interface;
  IRDPSRAPITransportStreamBuffer = interface;
  IRDPSRAPITransportStreamEvents = interface;
  IRDPSRAPIFrameBuffer = interface;
  IRDPSRAPIFrameBufferDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  RDPViewer = IRDPSRAPIViewer;
  RDPSRAPISessionProperties = IRDPSRAPISessionProperties;
  RDPSRAPIInvitationManager = IRDPSRAPIInvitationManager;
  RDPSRAPIInvitation = IRDPSRAPIInvitation;
  RDPSRAPIAttendeeManager = IRDPSRAPIAttendeeManager;
  RDPSRAPIAttendee = IRDPSRAPIAttendee;
  RDPSRAPIAttendeeDisconnectInfo = IRDPSRAPIAttendeeDisconnectInfo;
  RDPSRAPIApplicationFilter = IRDPSRAPIApplicationFilter;
  RDPSRAPIApplicationList = IRDPSRAPIApplicationList;
  RDPSRAPIApplication = IRDPSRAPIApplication;
  RDPSRAPIWindowList = IRDPSRAPIWindowList;
  RDPSRAPIWindow = IRDPSRAPIWindow;
  RDPSRAPITcpConnectionInfo = IRDPSRAPITcpConnectionInfo;
  RDPSession = IRDPSRAPISharingSession2;
  RDPSRAPIFrameBuffer = IRDPSRAPIFrameBuffer;
  RDPTransportStreamBuffer = IRDPSRAPITransportStreamBuffer;
  RDPTransportStreamEvents = IRDPSRAPITransportStreamEvents;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PByte1 = ^Byte; {*}

  RDPENCOMAPI_CONSTANTS = __MIDL___MIDL_itf_rdpencomapi_0000_0028_0001; 
  CTRL_LEVEL = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0001; 
  ATTENDEE_DISCONNECT_REASON = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0002; 
  CHANNEL_PRIORITY = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0003; 
  CHANNEL_FLAGS = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0004; 
  CHANNEL_ACCESS_ENUM = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0005; 
  RDPENCOMAPI_ATTENDEE_FLAGS = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0006; 
  RDPSRAPI_WND_FLAGS = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0007; 
  RDPSRAPI_APP_FLAGS = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0008; 

  __ReferenceRemainingTypes__ = record
    __ctrlLevel__: CTRL_LEVEL;
    __attendeeDisconnectReason__: ATTENDEE_DISCONNECT_REASON;
    __channelPriority__: CHANNEL_PRIORITY;
    __channelFlags__: CHANNEL_FLAGS;
    __channelAccessEnum__: CHANNEL_ACCESS_ENUM;
    __rdpencomapiAttendeeFlags__: RDPENCOMAPI_ATTENDEE_FLAGS;
    __rdpsrapiWndFlags__: RDPSRAPI_WND_FLAGS;
    __rdpsrapiAppFlags__: RDPSRAPI_APP_FLAGS;
  end;

  RDPSRAPI_MOUSE_BUTTON_TYPE = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0009; 
  RDPSRAPI_KBD_CODE_TYPE = __MIDL___MIDL_itf_rdpencomapi_0000_0000_0010; 

// *********************************************************************//
// DispIntf:  _IRDPSessionEvents
// Flags:     (4096) Dispatchable
// GUID:      {98A97042-6698-40E9-8EFD-B3200990004B}
// *********************************************************************//
  _IRDPSessionEvents = dispinterface
    ['{98A97042-6698-40E9-8EFD-B3200990004B}']
    procedure OnAttendeeConnected(const pAttendee: IDispatch); dispid 301;
    procedure OnAttendeeDisconnected(const pDisconnectInfo: IDispatch); dispid 302;
    procedure OnAttendeeUpdate(const pAttendee: IDispatch); dispid 303;
    procedure OnConnectionEstablished; dispid 305;
    procedure OnConnectionFailed; dispid 308;
    procedure OnConnectionTerminated(discReason: Integer; ExtendedInfo: Integer); dispid 306;
    procedure OnConnectionAuthenticated; dispid 307;
    procedure OnError(ErrorInfo: OleVariant); dispid 304;
    procedure OnApplicationOpen(const pApplication: IDispatch); dispid 316;
    procedure OnApplicationClose(const pApplication: IDispatch); dispid 317;
    procedure OnApplicationUpdate(const pApplication: IDispatch); dispid 318;
    procedure OnWindowOpen(const pWindow: IDispatch); dispid 319;
    procedure OnWindowClose(const pWindow: IDispatch); dispid 320;
    procedure OnWindowUpdate(const pWindow: IDispatch); dispid 321;
    procedure OnControlLevelChangeRequest(const pAttendee: IDispatch; RequestedLevel: CTRL_LEVEL); dispid 309;
    procedure OnGraphicsStreamPaused; dispid 310;
    procedure OnGraphicsStreamResumed; dispid 311;
    procedure OnChannelDataReceived(const pChannel: IUnknown; lAttendeeId: Integer; 
                                    const bstrData: WideString); dispid 314;
    procedure OnChannelDataSent(const pChannel: IUnknown; lAttendeeId: Integer; BytesSent: Integer); dispid 315;
    procedure OnSharedRectChanged(left: Integer; top: Integer; right: Integer; bottom: Integer); dispid 323;
    procedure OnFocusReleased(iDirection: SYSINT); dispid 324;
    procedure OnSharedDesktopSettingsChanged(width: Integer; height: Integer; colordepth: Integer); dispid 325;
    procedure OnViewingSizeChanged(left: Integer; top: Integer; right: Integer; bottom: Integer); dispid 340;
    procedure OnControlLevelChangeResponse(const pAttendee: IDispatch; RequestedLevel: CTRL_LEVEL; 
                                           ReasonCode: Integer); dispid 338;
  end;

// *********************************************************************//
// Interface: IRDPViewerRenderingSurface
// Flags:     (0)
// GUID:      {56BFCE32-83E9-414D-82E8-F31D01C62CB5}
// *********************************************************************//
  IRDPViewerRenderingSurface = interface(IUnknown)
    ['{56BFCE32-83E9-414D-82E8-F31D01C62CB5}']
    function SetRenderingSurface(const pRenderingSurface: IUnknown; surfaceWidth: Integer; 
                                 surfaceHeight: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRDPViewerInputSink
// Flags:     (0)
// GUID:      {BB590853-A6C5-4A7B-8DD4-76B69EEA12D5}
// *********************************************************************//
  IRDPViewerInputSink = interface(IUnknown)
    ['{BB590853-A6C5-4A7B-8DD4-76B69EEA12D5}']
    function SendMouseButtonEvent(buttonType: RDPSRAPI_MOUSE_BUTTON_TYPE; vbButtonDown: WordBool; 
                                  xPos: LongWord; yPos: LongWord): HResult; stdcall;
    function SendMouseMoveEvent(xPos: LongWord; yPos: LongWord): HResult; stdcall;
    function SendMouseWheelEvent(wheelRotation: Word): HResult; stdcall;
    function SendKeyboardEvent(codeType: RDPSRAPI_KBD_CODE_TYPE; keycode: Word; vbKeyUp: WordBool; 
                               vbRepeat: WordBool; vbExtended: WordBool): HResult; stdcall;
    function SendSyncEvent(syncFlags: LongWord): HResult; stdcall;
    function BeginTouchFrame: HResult; stdcall;
    function AddTouchInput(contactId: SYSUINT; event: SYSUINT; x: SYSINT; y: SYSINT): HResult; stdcall;
    function EndTouchFrame: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIAudioStream
// Flags:     (0)
// GUID:      {E3E30EF9-89C6-4541-BA3B-19336AC6D31C}
// *********************************************************************//
  IRDPSRAPIAudioStream = interface(IUnknown)
    ['{E3E30EF9-89C6-4541-BA3B-19336AC6D31C}']
    function Initialize(out pnPeriodInHundredNsIntervals: Int64): HResult; stdcall;
    function Start: HResult; stdcall;
    function Stop: HResult; stdcall;
    function GetBuffer(out ppbData: PByte1; out pcbData: SYSUINT; out pTimestamp: Largeuint): HResult; stdcall;
    function FreeBuffer: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIPerfCounterLoggingManager
// Flags:     (0)
// GUID:      {9A512C86-AC6E-4A8E-B1A4-FCEF363F6E64}
// *********************************************************************//
  IRDPSRAPIPerfCounterLoggingManager = interface(IUnknown)
    ['{9A512C86-AC6E-4A8E-B1A4-FCEF363F6E64}']
    function CreateLogger(const bstrCounterName: WideString; 
                          out ppLogger: IRDPSRAPIPerfCounterLogger): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIPerfCounterLogger
// Flags:     (0)
// GUID:      {071C2533-0FA4-4E8F-AE83-9C10B4305AB5}
// *********************************************************************//
  IRDPSRAPIPerfCounterLogger = interface(IUnknown)
    ['{071C2533-0FA4-4E8F-AE83-9C10B4305AB5}']
    function LogValue(lValue: Int64): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIViewer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}
// *********************************************************************//
  IRDPSRAPIViewer = interface(IDispatch)
    ['{C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}']
    procedure Connect(const bstrConnectionString: WideString; const bstrName: WideString; 
                      const bstrPassword: WideString); safecall;
    procedure Disconnect; safecall;
    function Get_Attendees: IRDPSRAPIAttendeeManager; safecall;
    function Get_Invitations: IRDPSRAPIInvitationManager; safecall;
    function Get_ApplicationFilter: IRDPSRAPIApplicationFilter; safecall;
    function Get_VirtualChannelManager: IRDPSRAPIVirtualChannelManager; safecall;
    procedure Set_SmartSizing(pvbSmartSizing: WordBool); safecall;
    function Get_SmartSizing: WordBool; safecall;
    procedure RequestControl(CtrlLevel: CTRL_LEVEL); safecall;
    procedure Set_DisconnectedText(const pbstrDisconnectedText: WideString); safecall;
    function Get_DisconnectedText: WideString; safecall;
    procedure RequestColorDepthChange(Bpp: Integer); safecall;
    function Get_Properties: IRDPSRAPISessionProperties; safecall;
    function StartReverseConnectListener(const bstrConnectionString: WideString; 
                                         const bstrUserName: WideString; 
                                         const bstrPassword: WideString): WideString; safecall;
    property Attendees: IRDPSRAPIAttendeeManager read Get_Attendees;
    property Invitations: IRDPSRAPIInvitationManager read Get_Invitations;
    property ApplicationFilter: IRDPSRAPIApplicationFilter read Get_ApplicationFilter;
    property VirtualChannelManager: IRDPSRAPIVirtualChannelManager read Get_VirtualChannelManager;
    property SmartSizing: WordBool read Get_SmartSizing write Set_SmartSizing;
    property DisconnectedText: WideString read Get_DisconnectedText write Set_DisconnectedText;
    property Properties: IRDPSRAPISessionProperties read Get_Properties;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIViewerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}
// *********************************************************************//
  IRDPSRAPIViewerDisp = dispinterface
    ['{C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}']
    procedure Connect(const bstrConnectionString: WideString; const bstrName: WideString; 
                      const bstrPassword: WideString); dispid 104;
    procedure Disconnect; dispid 105;
    property Attendees: IRDPSRAPIAttendeeManager readonly dispid 203;
    property Invitations: IRDPSRAPIInvitationManager readonly dispid 204;
    property ApplicationFilter: IRDPSRAPIApplicationFilter readonly dispid 215;
    property VirtualChannelManager: IRDPSRAPIVirtualChannelManager readonly dispid 206;
    property SmartSizing: WordBool dispid 238;
    procedure RequestControl(CtrlLevel: CTRL_LEVEL); dispid 108;
    property DisconnectedText: WideString dispid 237;
    procedure RequestColorDepthChange(Bpp: Integer); dispid 115;
    property Properties: IRDPSRAPISessionProperties readonly dispid 202;
    function StartReverseConnectListener(const bstrConnectionString: WideString; 
                                         const bstrUserName: WideString; 
                                         const bstrPassword: WideString): WideString; dispid 116;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIAttendeeManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BA3A37E8-33DA-4749-8DA0-07FA34DA7944}
// *********************************************************************//
  IRDPSRAPIAttendeeManager = interface(IDispatch)
    ['{BA3A37E8-33DA-4749-8DA0-07FA34DA7944}']
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(Id: Integer): IRDPSRAPIAttendee; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Id: Integer]: IRDPSRAPIAttendee read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIAttendeeManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BA3A37E8-33DA-4749-8DA0-07FA34DA7944}
// *********************************************************************//
  IRDPSRAPIAttendeeManagerDisp = dispinterface
    ['{BA3A37E8-33DA-4749-8DA0-07FA34DA7944}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Id: Integer]: IRDPSRAPIAttendee readonly dispid 0; default;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIAttendee
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EC0671B3-1B78-4B80-A464-9132247543E3}
// *********************************************************************//
  IRDPSRAPIAttendee = interface(IDispatch)
    ['{EC0671B3-1B78-4B80-A464-9132247543E3}']
    function Get_Id: Integer; safecall;
    function Get_RemoteName: WideString; safecall;
    function Get_ControlLevel: CTRL_LEVEL; safecall;
    procedure Set_ControlLevel(pVal: CTRL_LEVEL); safecall;
    function Get_Invitation: IRDPSRAPIInvitation; safecall;
    procedure TerminateConnection; safecall;
    function Get_Flags: Integer; safecall;
    function Get_ConnectivityInfo: IUnknown; safecall;
    property Id: Integer read Get_Id;
    property RemoteName: WideString read Get_RemoteName;
    property ControlLevel: CTRL_LEVEL read Get_ControlLevel write Set_ControlLevel;
    property Invitation: IRDPSRAPIInvitation read Get_Invitation;
    property Flags: Integer read Get_Flags;
    property ConnectivityInfo: IUnknown read Get_ConnectivityInfo;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIAttendeeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EC0671B3-1B78-4B80-A464-9132247543E3}
// *********************************************************************//
  IRDPSRAPIAttendeeDisp = dispinterface
    ['{EC0671B3-1B78-4B80-A464-9132247543E3}']
    property Id: Integer readonly dispid 201;
    property RemoteName: WideString readonly dispid 243;
    property ControlLevel: CTRL_LEVEL dispid 242;
    property Invitation: IRDPSRAPIInvitation readonly dispid 205;
    procedure TerminateConnection; dispid 106;
    property Flags: Integer readonly dispid 230;
    property ConnectivityInfo: IUnknown readonly dispid 231;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIInvitation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}
// *********************************************************************//
  IRDPSRAPIInvitation = interface(IDispatch)
    ['{4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}']
    function Get_ConnectionString: WideString; safecall;
    function Get_GroupName: WideString; safecall;
    function Get_Password: WideString; safecall;
    function Get_AttendeeLimit: Integer; safecall;
    procedure Set_AttendeeLimit(pRetVal: Integer); safecall;
    function Get_Revoked: WordBool; safecall;
    procedure Set_Revoked(pRetVal: WordBool); safecall;
    property ConnectionString: WideString read Get_ConnectionString;
    property GroupName: WideString read Get_GroupName;
    property Password: WideString read Get_Password;
    property AttendeeLimit: Integer read Get_AttendeeLimit write Set_AttendeeLimit;
    property Revoked: WordBool read Get_Revoked write Set_Revoked;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIInvitationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}
// *********************************************************************//
  IRDPSRAPIInvitationDisp = dispinterface
    ['{4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}']
    property ConnectionString: WideString readonly dispid 232;
    property GroupName: WideString readonly dispid 233;
    property Password: WideString readonly dispid 234;
    property AttendeeLimit: Integer dispid 235;
    property Revoked: WordBool dispid 236;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIInvitationManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4722B049-92C3-4C2D-8A65-F7348F644DCF}
// *********************************************************************//
  IRDPSRAPIInvitationManager = interface(IDispatch)
    ['{4722B049-92C3-4C2D-8A65-F7348F644DCF}']
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(Item: OleVariant): IRDPSRAPIInvitation; safecall;
    function Get_Count: Integer; safecall;
    function CreateInvitation(const bstrAuthString: WideString; const bstrGroupName: WideString; 
                              const bstrPassword: WideString; AttendeeLimit: Integer): IRDPSRAPIInvitation; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Item: OleVariant]: IRDPSRAPIInvitation read Get_Item; default;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIInvitationManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4722B049-92C3-4C2D-8A65-F7348F644DCF}
// *********************************************************************//
  IRDPSRAPIInvitationManagerDisp = dispinterface
    ['{4722B049-92C3-4C2D-8A65-F7348F644DCF}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Item: OleVariant]: IRDPSRAPIInvitation readonly dispid 0; default;
    property Count: Integer readonly dispid 244;
    function CreateInvitation(const bstrAuthString: WideString; const bstrGroupName: WideString; 
                              const bstrPassword: WideString; AttendeeLimit: Integer): IRDPSRAPIInvitation; dispid 107;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIApplicationFilter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D20F10CA-6637-4F06-B1D5-277EA7E5160D}
// *********************************************************************//
  IRDPSRAPIApplicationFilter = interface(IDispatch)
    ['{D20F10CA-6637-4F06-B1D5-277EA7E5160D}']
    function Get_Applications: IRDPSRAPIApplicationList; safecall;
    function Get_Windows: IRDPSRAPIWindowList; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(pRetVal: WordBool); safecall;
    property Applications: IRDPSRAPIApplicationList read Get_Applications;
    property Windows: IRDPSRAPIWindowList read Get_Windows;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIApplicationFilterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D20F10CA-6637-4F06-B1D5-277EA7E5160D}
// *********************************************************************//
  IRDPSRAPIApplicationFilterDisp = dispinterface
    ['{D20F10CA-6637-4F06-B1D5-277EA7E5160D}']
    property Applications: IRDPSRAPIApplicationList readonly dispid 217;
    property Windows: IRDPSRAPIWindowList readonly dispid 216;
    property Enabled: WordBool dispid 219;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIApplicationList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D4B4AEB3-22DC-4837-B3B6-42EA2517849A}
// *********************************************************************//
  IRDPSRAPIApplicationList = interface(IDispatch)
    ['{D4B4AEB3-22DC-4837-B3B6-42EA2517849A}']
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(Item: Integer): IRDPSRAPIApplication; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Item: Integer]: IRDPSRAPIApplication read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIApplicationListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D4B4AEB3-22DC-4837-B3B6-42EA2517849A}
// *********************************************************************//
  IRDPSRAPIApplicationListDisp = dispinterface
    ['{D4B4AEB3-22DC-4837-B3B6-42EA2517849A}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Item: Integer]: IRDPSRAPIApplication readonly dispid 0; default;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIApplication
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41E7A09D-EB7A-436E-935D-780CA2628324}
// *********************************************************************//
  IRDPSRAPIApplication = interface(IDispatch)
    ['{41E7A09D-EB7A-436E-935D-780CA2628324}']
    function Get_Windows: IRDPSRAPIWindowList; safecall;
    function Get_Id: Integer; safecall;
    function Get_Shared: WordBool; safecall;
    procedure Set_Shared(pRetVal: WordBool); safecall;
    function Get_Name: WideString; safecall;
    function Get_Flags: LongWord; safecall;
    property Windows: IRDPSRAPIWindowList read Get_Windows;
    property Id: Integer read Get_Id;
    property Shared: WordBool read Get_Shared write Set_Shared;
    property Name: WideString read Get_Name;
    property Flags: LongWord read Get_Flags;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIApplicationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41E7A09D-EB7A-436E-935D-780CA2628324}
// *********************************************************************//
  IRDPSRAPIApplicationDisp = dispinterface
    ['{41E7A09D-EB7A-436E-935D-780CA2628324}']
    property Windows: IRDPSRAPIWindowList readonly dispid 0;
    property Id: Integer readonly dispid 201;
    property Shared: WordBool dispid 220;
    property Name: WideString readonly dispid 214;
    property Flags: LongWord readonly dispid 223;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIWindowList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8A05CE44-715A-4116-A189-A118F30A07BD}
// *********************************************************************//
  IRDPSRAPIWindowList = interface(IDispatch)
    ['{8A05CE44-715A-4116-A189-A118F30A07BD}']
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(Item: Integer): IRDPSRAPIWindow; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Item: Integer]: IRDPSRAPIWindow read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIWindowListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8A05CE44-715A-4116-A189-A118F30A07BD}
// *********************************************************************//
  IRDPSRAPIWindowListDisp = dispinterface
    ['{8A05CE44-715A-4116-A189-A118F30A07BD}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Item: Integer]: IRDPSRAPIWindow readonly dispid 0; default;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIWindow
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}
// *********************************************************************//
  IRDPSRAPIWindow = interface(IDispatch)
    ['{BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}']
    function Get_Id: Integer; safecall;
    function Get_Application: IRDPSRAPIApplication; safecall;
    function Get_Shared: WordBool; safecall;
    procedure Set_Shared(pRetVal: WordBool); safecall;
    function Get_Name: WideString; safecall;
    procedure Show; safecall;
    function Get_Flags: LongWord; safecall;
    property Id: Integer read Get_Id;
    property Application: IRDPSRAPIApplication read Get_Application;
    property Shared: WordBool read Get_Shared write Set_Shared;
    property Name: WideString read Get_Name;
    property Flags: LongWord read Get_Flags;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIWindowDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}
// *********************************************************************//
  IRDPSRAPIWindowDisp = dispinterface
    ['{BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}']
    property Id: Integer readonly dispid 201;
    property Application: IRDPSRAPIApplication readonly dispid 211;
    property Shared: WordBool dispid 220;
    property Name: WideString readonly dispid 213;
    procedure Show; dispid 114;
    property Flags: LongWord readonly dispid 224;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIVirtualChannelManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}
// *********************************************************************//
  IRDPSRAPIVirtualChannelManager = interface(IDispatch)
    ['{0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}']
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(Item: OleVariant): IRDPSRAPIVirtualChannel; safecall;
    function CreateVirtualChannel(const bstrChannelName: WideString; Priority: CHANNEL_PRIORITY; 
                                  ChannelFlags: LongWord): IRDPSRAPIVirtualChannel; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Item: OleVariant]: IRDPSRAPIVirtualChannel read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIVirtualChannelManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}
// *********************************************************************//
  IRDPSRAPIVirtualChannelManagerDisp = dispinterface
    ['{0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Item: OleVariant]: IRDPSRAPIVirtualChannel readonly dispid 0; default;
    function CreateVirtualChannel(const bstrChannelName: WideString; Priority: CHANNEL_PRIORITY; 
                                  ChannelFlags: LongWord): IRDPSRAPIVirtualChannel; dispid 109;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIVirtualChannel
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05E12F95-28B3-4C9A-8780-D0248574A1E0}
// *********************************************************************//
  IRDPSRAPIVirtualChannel = interface(IDispatch)
    ['{05E12F95-28B3-4C9A-8780-D0248574A1E0}']
    procedure SendData(const bstrData: WideString; lAttendeeId: Integer; ChannelSendFlags: LongWord); safecall;
    procedure SetAccess(lAttendeeId: Integer; AccessType: CHANNEL_ACCESS_ENUM); safecall;
    function Get_Name: WideString; safecall;
    function Get_Flags: Integer; safecall;
    function Get_Priority: CHANNEL_PRIORITY; safecall;
    property Name: WideString read Get_Name;
    property Flags: Integer read Get_Flags;
    property Priority: CHANNEL_PRIORITY read Get_Priority;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIVirtualChannelDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05E12F95-28B3-4C9A-8780-D0248574A1E0}
// *********************************************************************//
  IRDPSRAPIVirtualChannelDisp = dispinterface
    ['{05E12F95-28B3-4C9A-8780-D0248574A1E0}']
    procedure SendData(const bstrData: WideString; lAttendeeId: Integer; ChannelSendFlags: LongWord); dispid 110;
    procedure SetAccess(lAttendeeId: Integer; AccessType: CHANNEL_ACCESS_ENUM); dispid 111;
    property Name: WideString readonly dispid 207;
    property Flags: Integer readonly dispid 208;
    property Priority: CHANNEL_PRIORITY readonly dispid 209;
  end;

// *********************************************************************//
// Interface: IRDPSRAPISessionProperties
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {339B24F2-9BC0-4F16-9AAC-F165433D13D4}
// *********************************************************************//
  IRDPSRAPISessionProperties = interface(IDispatch)
    ['{339B24F2-9BC0-4F16-9AAC-F165433D13D4}']
    function Get_Property_(const PropertyName: WideString): OleVariant; safecall;
    procedure Set_Property_(const PropertyName: WideString; pVal: OleVariant); safecall;
    property Property_[const PropertyName: WideString]: OleVariant read Get_Property_ write Set_Property_; default;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPISessionPropertiesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {339B24F2-9BC0-4F16-9AAC-F165433D13D4}
// *********************************************************************//
  IRDPSRAPISessionPropertiesDisp = dispinterface
    ['{339B24F2-9BC0-4F16-9AAC-F165433D13D4}']
    property Property_[const PropertyName: WideString]: OleVariant dispid 0; default;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIAttendeeDisconnectInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C187689F-447C-44A1-9C14-FFFBB3B7EC17}
// *********************************************************************//
  IRDPSRAPIAttendeeDisconnectInfo = interface(IDispatch)
    ['{C187689F-447C-44A1-9C14-FFFBB3B7EC17}']
    function Get_Attendee: IRDPSRAPIAttendee; safecall;
    function Get_Reason: ATTENDEE_DISCONNECT_REASON; safecall;
    function Get_Code: Integer; safecall;
    property Attendee: IRDPSRAPIAttendee read Get_Attendee;
    property Reason: ATTENDEE_DISCONNECT_REASON read Get_Reason;
    property Code: Integer read Get_Code;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIAttendeeDisconnectInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C187689F-447C-44A1-9C14-FFFBB3B7EC17}
// *********************************************************************//
  IRDPSRAPIAttendeeDisconnectInfoDisp = dispinterface
    ['{C187689F-447C-44A1-9C14-FFFBB3B7EC17}']
    property Attendee: IRDPSRAPIAttendee readonly dispid 0;
    property Reason: ATTENDEE_DISCONNECT_REASON readonly dispid 240;
    property Code: Integer readonly dispid 241;
  end;

// *********************************************************************//
// Interface: IRDPSRAPITcpConnectionInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F74049A4-3D06-4028-8193-0A8C29BC2452}
// *********************************************************************//
  IRDPSRAPITcpConnectionInfo = interface(IDispatch)
    ['{F74049A4-3D06-4028-8193-0A8C29BC2452}']
    function Get_Protocol: Integer; safecall;
    function Get_LocalPort: Integer; safecall;
    function Get_LocalIP: WideString; safecall;
    function Get_PeerPort: Integer; safecall;
    function Get_PeerIP: WideString; safecall;
    property Protocol: Integer read Get_Protocol;
    property LocalPort: Integer read Get_LocalPort;
    property LocalIP: WideString read Get_LocalIP;
    property PeerPort: Integer read Get_PeerPort;
    property PeerIP: WideString read Get_PeerIP;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPITcpConnectionInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F74049A4-3D06-4028-8193-0A8C29BC2452}
// *********************************************************************//
  IRDPSRAPITcpConnectionInfoDisp = dispinterface
    ['{F74049A4-3D06-4028-8193-0A8C29BC2452}']
    property Protocol: Integer readonly dispid 225;
    property LocalPort: Integer readonly dispid 226;
    property LocalIP: WideString readonly dispid 227;
    property PeerPort: Integer readonly dispid 228;
    property PeerIP: WideString readonly dispid 229;
  end;

// *********************************************************************//
// Interface: IRDPSRAPISharingSession
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EEB20886-E470-4CF6-842B-2739C0EC5CFB}
// *********************************************************************//
  IRDPSRAPISharingSession = interface(IDispatch)
    ['{EEB20886-E470-4CF6-842B-2739C0EC5CFB}']
    procedure Open; safecall;
    procedure Close; safecall;
    procedure Set_colordepth(pColorDepth: Integer); safecall;
    function Get_colordepth: Integer; safecall;
    function Get_Properties: IRDPSRAPISessionProperties; safecall;
    function Get_Attendees: IRDPSRAPIAttendeeManager; safecall;
    function Get_Invitations: IRDPSRAPIInvitationManager; safecall;
    function Get_ApplicationFilter: IRDPSRAPIApplicationFilter; safecall;
    function Get_VirtualChannelManager: IRDPSRAPIVirtualChannelManager; safecall;
    procedure Pause; safecall;
    procedure Resume; safecall;
    procedure ConnectToClient(const bstrConnectionString: WideString); safecall;
    procedure SetDesktopSharedRect(left: Integer; top: Integer; right: Integer; bottom: Integer); safecall;
    procedure GetDesktopSharedRect(out pleft: Integer; out ptop: Integer; out pright: Integer; 
                                   out pbottom: Integer); safecall;
    property colordepth: Integer read Get_colordepth write Set_colordepth;
    property Properties: IRDPSRAPISessionProperties read Get_Properties;
    property Attendees: IRDPSRAPIAttendeeManager read Get_Attendees;
    property Invitations: IRDPSRAPIInvitationManager read Get_Invitations;
    property ApplicationFilter: IRDPSRAPIApplicationFilter read Get_ApplicationFilter;
    property VirtualChannelManager: IRDPSRAPIVirtualChannelManager read Get_VirtualChannelManager;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPISharingSessionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EEB20886-E470-4CF6-842B-2739C0EC5CFB}
// *********************************************************************//
  IRDPSRAPISharingSessionDisp = dispinterface
    ['{EEB20886-E470-4CF6-842B-2739C0EC5CFB}']
    procedure Open; dispid 100;
    procedure Close; dispid 101;
    property colordepth: Integer dispid 239;
    property Properties: IRDPSRAPISessionProperties readonly dispid 202;
    property Attendees: IRDPSRAPIAttendeeManager readonly dispid 203;
    property Invitations: IRDPSRAPIInvitationManager readonly dispid 204;
    property ApplicationFilter: IRDPSRAPIApplicationFilter readonly dispid 215;
    property VirtualChannelManager: IRDPSRAPIVirtualChannelManager readonly dispid 206;
    procedure Pause; dispid 112;
    procedure Resume; dispid 113;
    procedure ConnectToClient(const bstrConnectionString: WideString); dispid 117;
    procedure SetDesktopSharedRect(left: Integer; top: Integer; right: Integer; bottom: Integer); dispid 102;
    procedure GetDesktopSharedRect(out pleft: Integer; out ptop: Integer; out pright: Integer; 
                                   out pbottom: Integer); dispid 103;
  end;

// *********************************************************************//
// Interface: IRDPSRAPISharingSession2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FEE4EE57-E3E8-4205-8FB0-8FD1D0675C21}
// *********************************************************************//
  IRDPSRAPISharingSession2 = interface(IRDPSRAPISharingSession)
    ['{FEE4EE57-E3E8-4205-8FB0-8FD1D0675C21}']
    procedure ConnectUsingTransportStream(const pStream: IRDPSRAPITransportStream; 
                                          const bstrGroup: WideString; 
                                          const bstrAuthenticatedAttendeeName: WideString); safecall;
    function Get_FrameBuffer: IRDPSRAPIFrameBuffer; safecall;
    procedure SendControlLevelChangeResponse(const pAttendee: IRDPSRAPIAttendee; 
                                             RequestedLevel: CTRL_LEVEL; ReasonCode: Integer); safecall;
    property FrameBuffer: IRDPSRAPIFrameBuffer read Get_FrameBuffer;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPISharingSession2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FEE4EE57-E3E8-4205-8FB0-8FD1D0675C21}
// *********************************************************************//
  IRDPSRAPISharingSession2Disp = dispinterface
    ['{FEE4EE57-E3E8-4205-8FB0-8FD1D0675C21}']
    procedure ConnectUsingTransportStream(const pStream: IRDPSRAPITransportStream; 
                                          const bstrGroup: WideString; 
                                          const bstrAuthenticatedAttendeeName: WideString); dispid 127;
    property FrameBuffer: IRDPSRAPIFrameBuffer readonly dispid 254;
    procedure SendControlLevelChangeResponse(const pAttendee: IRDPSRAPIAttendee; 
                                             RequestedLevel: CTRL_LEVEL; ReasonCode: Integer); dispid 148;
    procedure Open; dispid 100;
    procedure Close; dispid 101;
    property colordepth: Integer dispid 239;
    property Properties: IRDPSRAPISessionProperties readonly dispid 202;
    property Attendees: IRDPSRAPIAttendeeManager readonly dispid 203;
    property Invitations: IRDPSRAPIInvitationManager readonly dispid 204;
    property ApplicationFilter: IRDPSRAPIApplicationFilter readonly dispid 215;
    property VirtualChannelManager: IRDPSRAPIVirtualChannelManager readonly dispid 206;
    procedure Pause; dispid 112;
    procedure Resume; dispid 113;
    procedure ConnectToClient(const bstrConnectionString: WideString); dispid 117;
    procedure SetDesktopSharedRect(left: Integer; top: Integer; right: Integer; bottom: Integer); dispid 102;
    procedure GetDesktopSharedRect(out pleft: Integer; out ptop: Integer; out pright: Integer; 
                                   out pbottom: Integer); dispid 103;
  end;

// *********************************************************************//
// Interface: IRDPSRAPITransportStream
// Flags:     (0)
// GUID:      {36CFA065-43BB-4EF7-AED7-9B88A5053036}
// *********************************************************************//
  IRDPSRAPITransportStream = interface(IUnknown)
    ['{36CFA065-43BB-4EF7-AED7-9B88A5053036}']
    function AllocBuffer(maxPayload: Integer; out ppBuffer: IRDPSRAPITransportStreamBuffer): HResult; stdcall;
    function FreeBuffer(const pBuffer: IRDPSRAPITransportStreamBuffer): HResult; stdcall;
    function WriteBuffer(const pBuffer: IRDPSRAPITransportStreamBuffer): HResult; stdcall;
    function ReadBuffer(const pBuffer: IRDPSRAPITransportStreamBuffer): HResult; stdcall;
    function Open(const pCallbacks: IRDPSRAPITransportStreamEvents): HResult; stdcall;
    function Close: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRDPSRAPITransportStreamBuffer
// Flags:     (0)
// GUID:      {81C80290-5085-44B0-B460-F865C39CB4A9}
// *********************************************************************//
  IRDPSRAPITransportStreamBuffer = interface(IUnknown)
    ['{81C80290-5085-44B0-B460-F865C39CB4A9}']
    function Get_Storage(out ppbStorage: PByte1): HResult; stdcall;
    function Get_StorageSize(out plMaxStore: Integer): HResult; stdcall;
    function Get_PayloadSize(out plRetVal: Integer): HResult; stdcall;
    function Set_PayloadSize(plRetVal: Integer): HResult; stdcall;
    function Get_PayloadOffset(out plRetVal: Integer): HResult; stdcall;
    function Set_PayloadOffset(plRetVal: Integer): HResult; stdcall;
    function Get_Flags(out plFlags: Integer): HResult; stdcall;
    function Set_Flags(plFlags: Integer): HResult; stdcall;
    function Get_Context(out ppContext: IUnknown): HResult; stdcall;
    function Set_Context(const ppContext: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRDPSRAPITransportStreamEvents
// Flags:     (0)
// GUID:      {EA81C254-F5AF-4E40-982E-3E63BB595276}
// *********************************************************************//
  IRDPSRAPITransportStreamEvents = interface(IUnknown)
    ['{EA81C254-F5AF-4E40-982E-3E63BB595276}']
    procedure OnWriteCompleted(const pBuffer: IRDPSRAPITransportStreamBuffer); stdcall;
    procedure OnReadCompleted(const pBuffer: IRDPSRAPITransportStreamBuffer); stdcall;
    procedure OnStreamClosed(hrReason: HResult); stdcall;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIFrameBuffer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3D67E7D2-B27B-448E-81B3-C6110ED8B4BE}
// *********************************************************************//
  IRDPSRAPIFrameBuffer = interface(IDispatch)
    ['{3D67E7D2-B27B-448E-81B3-C6110ED8B4BE}']
    function Get_width: Integer; safecall;
    function Get_height: Integer; safecall;
    function Get_Bpp: Integer; safecall;
    function GetFrameBufferBits(x: Integer; y: Integer; width: Integer; Heigth: Integer): PSafeArray; safecall;
    property width: Integer read Get_width;
    property height: Integer read Get_height;
    property Bpp: Integer read Get_Bpp;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIFrameBufferDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3D67E7D2-B27B-448E-81B3-C6110ED8B4BE}
// *********************************************************************//
  IRDPSRAPIFrameBufferDisp = dispinterface
    ['{3D67E7D2-B27B-448E-81B3-C6110ED8B4BE}']
    property width: Integer readonly dispid 252;
    property height: Integer readonly dispid 251;
    property Bpp: Integer readonly dispid 253;
    function GetFrameBufferBits(x: Integer; y: Integer; width: Integer; Heigth: Integer): {NOT_OLEAUTO(PSafeArray)}OleVariant; dispid 149;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TRDPViewer
// Help String      : 
// Default Interface: IRDPSRAPIViewer
// Def. Intf. DISP? : No
// Event   Interface: _IRDPSessionEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TRDPViewerOnAttendeeConnected = procedure(ASender: TObject; const pAttendee: IDispatch) of object;
  TRDPViewerOnAttendeeDisconnected = procedure(ASender: TObject; const pDisconnectInfo: IDispatch) of object;
  TRDPViewerOnAttendeeUpdate = procedure(ASender: TObject; const pAttendee: IDispatch) of object;
  TRDPViewerOnConnectionTerminated = procedure(ASender: TObject; discReason: Integer; 
                                                                 ExtendedInfo: Integer) of object;
  TRDPViewerOnError = procedure(ASender: TObject; ErrorInfo: OleVariant) of object;
  TRDPViewerOnApplicationOpen = procedure(ASender: TObject; const pApplication: IDispatch) of object;
  TRDPViewerOnApplicationClose = procedure(ASender: TObject; const pApplication: IDispatch) of object;
  TRDPViewerOnApplicationUpdate = procedure(ASender: TObject; const pApplication: IDispatch) of object;
  TRDPViewerOnWindowOpen = procedure(ASender: TObject; const pWindow: IDispatch) of object;
  TRDPViewerOnWindowClose = procedure(ASender: TObject; const pWindow: IDispatch) of object;
  TRDPViewerOnWindowUpdate = procedure(ASender: TObject; const pWindow: IDispatch) of object;
  TRDPViewerOnControlLevelChangeRequest = procedure(ASender: TObject; const pAttendee: IDispatch; 
                                                                      RequestedLevel: CTRL_LEVEL) of object;
  TRDPViewerOnChannelDataReceived = procedure(ASender: TObject; const pChannel: IUnknown; 
                                                                lAttendeeId: Integer; 
                                                                const bstrData: WideString) of object;
  TRDPViewerOnChannelDataSent = procedure(ASender: TObject; const pChannel: IUnknown; 
                                                            lAttendeeId: Integer; BytesSent: Integer) of object;
  TRDPViewerOnSharedRectChanged = procedure(ASender: TObject; left: Integer; top: Integer; 
                                                              right: Integer; bottom: Integer) of object;
  TRDPViewerOnFocusReleased = procedure(ASender: TObject; iDirection: SYSINT) of object;
  TRDPViewerOnSharedDesktopSettingsChanged = procedure(ASender: TObject; width: Integer; 
                                                                         height: Integer; 
                                                                         colordepth: Integer) of object;
  TRDPViewerOnViewingSizeChanged = procedure(ASender: TObject; left: Integer; top: Integer; 
                                                               right: Integer; bottom: Integer) of object;
  TRDPViewerOnControlLevelChangeResponse = procedure(ASender: TObject; const pAttendee: IDispatch; 
                                                                       RequestedLevel: CTRL_LEVEL; 
                                                                       ReasonCode: Integer) of object;

  TRDPViewer = class(TOleControl)
  private
    FOnAttendeeConnected: TRDPViewerOnAttendeeConnected;
    FOnAttendeeDisconnected: TRDPViewerOnAttendeeDisconnected;
    FOnAttendeeUpdate: TRDPViewerOnAttendeeUpdate;
    FOnConnectionEstablished: TNotifyEvent;
    FOnConnectionFailed: TNotifyEvent;
    FOnConnectionTerminated: TRDPViewerOnConnectionTerminated;
    FOnConnectionAuthenticated: TNotifyEvent;
    FOnError: TRDPViewerOnError;
    FOnApplicationOpen: TRDPViewerOnApplicationOpen;
    FOnApplicationClose: TRDPViewerOnApplicationClose;
    FOnApplicationUpdate: TRDPViewerOnApplicationUpdate;
    FOnWindowOpen: TRDPViewerOnWindowOpen;
    FOnWindowClose: TRDPViewerOnWindowClose;
    FOnWindowUpdate: TRDPViewerOnWindowUpdate;
    FOnControlLevelChangeRequest: TRDPViewerOnControlLevelChangeRequest;
    FOnGraphicsStreamPaused: TNotifyEvent;
    FOnGraphicsStreamResumed: TNotifyEvent;
    FOnChannelDataReceived: TRDPViewerOnChannelDataReceived;
    FOnChannelDataSent: TRDPViewerOnChannelDataSent;
    FOnSharedRectChanged: TRDPViewerOnSharedRectChanged;
    FOnFocusReleased: TRDPViewerOnFocusReleased;
    FOnSharedDesktopSettingsChanged: TRDPViewerOnSharedDesktopSettingsChanged;
    FOnViewingSizeChanged: TRDPViewerOnViewingSizeChanged;
    FOnControlLevelChangeResponse: TRDPViewerOnControlLevelChangeResponse;
    FIntf: IRDPSRAPIViewer;
    function  GetControlInterface: IRDPSRAPIViewer;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Attendees: IRDPSRAPIAttendeeManager;
    function Get_Invitations: IRDPSRAPIInvitationManager;
    function Get_ApplicationFilter: IRDPSRAPIApplicationFilter;
    function Get_VirtualChannelManager: IRDPSRAPIVirtualChannelManager;
    function Get_Properties: IRDPSRAPISessionProperties;
  public
    procedure Connect(const bstrConnectionString: WideString; const bstrName: WideString; 
                      const bstrPassword: WideString);
    procedure Disconnect;
    procedure RequestControl(CtrlLevel: CTRL_LEVEL);
    procedure RequestColorDepthChange(Bpp: Integer);
    function StartReverseConnectListener(const bstrConnectionString: WideString; 
                                         const bstrUserName: WideString; 
                                         const bstrPassword: WideString): WideString;
    property  ControlInterface: IRDPSRAPIViewer read GetControlInterface;
    property  DefaultInterface: IRDPSRAPIViewer read GetControlInterface;
    property Attendees: IRDPSRAPIAttendeeManager read Get_Attendees;
    property Invitations: IRDPSRAPIInvitationManager read Get_Invitations;
    property ApplicationFilter: IRDPSRAPIApplicationFilter read Get_ApplicationFilter;
    property VirtualChannelManager: IRDPSRAPIVirtualChannelManager read Get_VirtualChannelManager;
    property Properties: IRDPSRAPISessionProperties read Get_Properties;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property SmartSizing: WordBool index 238 read GetWordBoolProp write SetWordBoolProp stored False;
    property DisconnectedText: WideString index 237 read GetWideStringProp write SetWideStringProp stored False;
    property OnAttendeeConnected: TRDPViewerOnAttendeeConnected read FOnAttendeeConnected write FOnAttendeeConnected;
    property OnAttendeeDisconnected: TRDPViewerOnAttendeeDisconnected read FOnAttendeeDisconnected write FOnAttendeeDisconnected;
    property OnAttendeeUpdate: TRDPViewerOnAttendeeUpdate read FOnAttendeeUpdate write FOnAttendeeUpdate;
    property OnConnectionEstablished: TNotifyEvent read FOnConnectionEstablished write FOnConnectionEstablished;
    property OnConnectionFailed: TNotifyEvent read FOnConnectionFailed write FOnConnectionFailed;
    property OnConnectionTerminated: TRDPViewerOnConnectionTerminated read FOnConnectionTerminated write FOnConnectionTerminated;
    property OnConnectionAuthenticated: TNotifyEvent read FOnConnectionAuthenticated write FOnConnectionAuthenticated;
    property OnError: TRDPViewerOnError read FOnError write FOnError;
    property OnApplicationOpen: TRDPViewerOnApplicationOpen read FOnApplicationOpen write FOnApplicationOpen;
    property OnApplicationClose: TRDPViewerOnApplicationClose read FOnApplicationClose write FOnApplicationClose;
    property OnApplicationUpdate: TRDPViewerOnApplicationUpdate read FOnApplicationUpdate write FOnApplicationUpdate;
    property OnWindowOpen: TRDPViewerOnWindowOpen read FOnWindowOpen write FOnWindowOpen;
    property OnWindowClose: TRDPViewerOnWindowClose read FOnWindowClose write FOnWindowClose;
    property OnWindowUpdate: TRDPViewerOnWindowUpdate read FOnWindowUpdate write FOnWindowUpdate;
    property OnControlLevelChangeRequest: TRDPViewerOnControlLevelChangeRequest read FOnControlLevelChangeRequest write FOnControlLevelChangeRequest;
    property OnGraphicsStreamPaused: TNotifyEvent read FOnGraphicsStreamPaused write FOnGraphicsStreamPaused;
    property OnGraphicsStreamResumed: TNotifyEvent read FOnGraphicsStreamResumed write FOnGraphicsStreamResumed;
    property OnChannelDataReceived: TRDPViewerOnChannelDataReceived read FOnChannelDataReceived write FOnChannelDataReceived;
    property OnChannelDataSent: TRDPViewerOnChannelDataSent read FOnChannelDataSent write FOnChannelDataSent;
    property OnSharedRectChanged: TRDPViewerOnSharedRectChanged read FOnSharedRectChanged write FOnSharedRectChanged;
    property OnFocusReleased: TRDPViewerOnFocusReleased read FOnFocusReleased write FOnFocusReleased;
    property OnSharedDesktopSettingsChanged: TRDPViewerOnSharedDesktopSettingsChanged read FOnSharedDesktopSettingsChanged write FOnSharedDesktopSettingsChanged;
    property OnViewingSizeChanged: TRDPViewerOnViewingSizeChanged read FOnViewingSizeChanged write FOnViewingSizeChanged;
    property OnControlLevelChangeResponse: TRDPViewerOnControlLevelChangeResponse read FOnControlLevelChangeResponse write FOnControlLevelChangeResponse;
  end;

// *********************************************************************//
// The Class CoRDPSRAPISessionProperties provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPISessionProperties exposed by              
// the CoClass RDPSRAPISessionProperties. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPISessionProperties = class
    class function Create: IRDPSRAPISessionProperties;
    class function CreateRemote(const MachineName: string): IRDPSRAPISessionProperties;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIInvitationManager provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIInvitationManager exposed by              
// the CoClass RDPSRAPIInvitationManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIInvitationManager = class
    class function Create: IRDPSRAPIInvitationManager;
    class function CreateRemote(const MachineName: string): IRDPSRAPIInvitationManager;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIInvitation provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIInvitation exposed by              
// the CoClass RDPSRAPIInvitation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIInvitation = class
    class function Create: IRDPSRAPIInvitation;
    class function CreateRemote(const MachineName: string): IRDPSRAPIInvitation;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIAttendeeManager provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIAttendeeManager exposed by              
// the CoClass RDPSRAPIAttendeeManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIAttendeeManager = class
    class function Create: IRDPSRAPIAttendeeManager;
    class function CreateRemote(const MachineName: string): IRDPSRAPIAttendeeManager;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIAttendee provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIAttendee exposed by              
// the CoClass RDPSRAPIAttendee. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIAttendee = class
    class function Create: IRDPSRAPIAttendee;
    class function CreateRemote(const MachineName: string): IRDPSRAPIAttendee;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIAttendeeDisconnectInfo provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIAttendeeDisconnectInfo exposed by              
// the CoClass RDPSRAPIAttendeeDisconnectInfo. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIAttendeeDisconnectInfo = class
    class function Create: IRDPSRAPIAttendeeDisconnectInfo;
    class function CreateRemote(const MachineName: string): IRDPSRAPIAttendeeDisconnectInfo;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIApplicationFilter provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIApplicationFilter exposed by              
// the CoClass RDPSRAPIApplicationFilter. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIApplicationFilter = class
    class function Create: IRDPSRAPIApplicationFilter;
    class function CreateRemote(const MachineName: string): IRDPSRAPIApplicationFilter;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIApplicationList provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIApplicationList exposed by              
// the CoClass RDPSRAPIApplicationList. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIApplicationList = class
    class function Create: IRDPSRAPIApplicationList;
    class function CreateRemote(const MachineName: string): IRDPSRAPIApplicationList;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIApplication provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIApplication exposed by              
// the CoClass RDPSRAPIApplication. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIApplication = class
    class function Create: IRDPSRAPIApplication;
    class function CreateRemote(const MachineName: string): IRDPSRAPIApplication;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIWindowList provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIWindowList exposed by              
// the CoClass RDPSRAPIWindowList. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIWindowList = class
    class function Create: IRDPSRAPIWindowList;
    class function CreateRemote(const MachineName: string): IRDPSRAPIWindowList;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIWindow provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIWindow exposed by              
// the CoClass RDPSRAPIWindow. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIWindow = class
    class function Create: IRDPSRAPIWindow;
    class function CreateRemote(const MachineName: string): IRDPSRAPIWindow;
  end;

// *********************************************************************//
// The Class CoRDPSRAPITcpConnectionInfo provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPITcpConnectionInfo exposed by              
// the CoClass RDPSRAPITcpConnectionInfo. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPITcpConnectionInfo = class
    class function Create: IRDPSRAPITcpConnectionInfo;
    class function CreateRemote(const MachineName: string): IRDPSRAPITcpConnectionInfo;
  end;

// *********************************************************************//
// The Class CoRDPSession provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPISharingSession2 exposed by              
// the CoClass RDPSession. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSession = class
    class function Create: IRDPSRAPISharingSession2;
    class function CreateRemote(const MachineName: string): IRDPSRAPISharingSession2;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIFrameBuffer provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIFrameBuffer exposed by              
// the CoClass RDPSRAPIFrameBuffer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIFrameBuffer = class
    class function Create: IRDPSRAPIFrameBuffer;
    class function CreateRemote(const MachineName: string): IRDPSRAPIFrameBuffer;
  end;

// *********************************************************************//
// The Class CoRDPTransportStreamBuffer provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPITransportStreamBuffer exposed by              
// the CoClass RDPTransportStreamBuffer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPTransportStreamBuffer = class
    class function Create: IRDPSRAPITransportStreamBuffer;
    class function CreateRemote(const MachineName: string): IRDPSRAPITransportStreamBuffer;
  end;

// *********************************************************************//
// The Class CoRDPTransportStreamEvents provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPITransportStreamEvents exposed by              
// the CoClass RDPTransportStreamEvents. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPTransportStreamEvents = class
    class function Create: IRDPSRAPITransportStreamEvents;
    class function CreateRemote(const MachineName: string): IRDPSRAPITransportStreamEvents;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

procedure TRDPViewer.InitControlData;
const
  CEventDispIDs: array [0..23] of DWORD = (
    $0000012D, $0000012E, $0000012F, $00000131, $00000134, $00000132,
    $00000133, $00000130, $0000013C, $0000013D, $0000013E, $0000013F,
    $00000140, $00000141, $00000135, $00000136, $00000137, $0000013A,
    $0000013B, $00000143, $00000144, $00000145, $00000154, $00000152);
  CControlData: TControlData2 = (
    ClassID:      '{32BE5ED2-5C86-480F-A914-0FF8885A1B3F}';
    EventIID:     '{98A97042-6698-40E9-8EFD-B3200990004B}';
    EventCount:   24;
    EventDispIDs: @CEventDispIDs;
    LicenseKey:   nil (*HR:$80004002*);
    Flags:        $00000000;
    Version:      500);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := UIntPtr(@@FOnAttendeeConnected) - UIntPtr(Self);
end;

procedure TRDPViewer.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IRDPSRAPIViewer;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TRDPViewer.GetControlInterface: IRDPSRAPIViewer;
begin
  CreateControl;
  Result := FIntf;
end;

function TRDPViewer.Get_Attendees: IRDPSRAPIAttendeeManager;
begin
  Result := DefaultInterface.Attendees;
end;

function TRDPViewer.Get_Invitations: IRDPSRAPIInvitationManager;
begin
  Result := DefaultInterface.Invitations;
end;

function TRDPViewer.Get_ApplicationFilter: IRDPSRAPIApplicationFilter;
begin
  Result := DefaultInterface.ApplicationFilter;
end;

function TRDPViewer.Get_VirtualChannelManager: IRDPSRAPIVirtualChannelManager;
begin
  Result := DefaultInterface.VirtualChannelManager;
end;

function TRDPViewer.Get_Properties: IRDPSRAPISessionProperties;
begin
  Result := DefaultInterface.Properties;
end;

procedure TRDPViewer.Connect(const bstrConnectionString: WideString; const bstrName: WideString; 
                             const bstrPassword: WideString);
begin
  DefaultInterface.Connect(bstrConnectionString, bstrName, bstrPassword);
end;

procedure TRDPViewer.Disconnect;
begin
  DefaultInterface.Disconnect;
end;

procedure TRDPViewer.RequestControl(CtrlLevel: CTRL_LEVEL);
begin
  DefaultInterface.RequestControl(CtrlLevel);
end;

procedure TRDPViewer.RequestColorDepthChange(Bpp: Integer);
begin
  DefaultInterface.RequestColorDepthChange(Bpp);
end;

function TRDPViewer.StartReverseConnectListener(const bstrConnectionString: WideString; 
                                                const bstrUserName: WideString; 
                                                const bstrPassword: WideString): WideString;
begin
  Result := DefaultInterface.StartReverseConnectListener(bstrConnectionString, bstrUserName, 
                                                         bstrPassword);
end;

class function CoRDPSRAPISessionProperties.Create: IRDPSRAPISessionProperties;
begin
  Result := CreateComObject(CLASS_RDPSRAPISessionProperties) as IRDPSRAPISessionProperties;
end;

class function CoRDPSRAPISessionProperties.CreateRemote(const MachineName: string): IRDPSRAPISessionProperties;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPISessionProperties) as IRDPSRAPISessionProperties;
end;

class function CoRDPSRAPIInvitationManager.Create: IRDPSRAPIInvitationManager;
begin
  Result := CreateComObject(CLASS_RDPSRAPIInvitationManager) as IRDPSRAPIInvitationManager;
end;

class function CoRDPSRAPIInvitationManager.CreateRemote(const MachineName: string): IRDPSRAPIInvitationManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIInvitationManager) as IRDPSRAPIInvitationManager;
end;

class function CoRDPSRAPIInvitation.Create: IRDPSRAPIInvitation;
begin
  Result := CreateComObject(CLASS_RDPSRAPIInvitation) as IRDPSRAPIInvitation;
end;

class function CoRDPSRAPIInvitation.CreateRemote(const MachineName: string): IRDPSRAPIInvitation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIInvitation) as IRDPSRAPIInvitation;
end;

class function CoRDPSRAPIAttendeeManager.Create: IRDPSRAPIAttendeeManager;
begin
  Result := CreateComObject(CLASS_RDPSRAPIAttendeeManager) as IRDPSRAPIAttendeeManager;
end;

class function CoRDPSRAPIAttendeeManager.CreateRemote(const MachineName: string): IRDPSRAPIAttendeeManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIAttendeeManager) as IRDPSRAPIAttendeeManager;
end;

class function CoRDPSRAPIAttendee.Create: IRDPSRAPIAttendee;
begin
  Result := CreateComObject(CLASS_RDPSRAPIAttendee) as IRDPSRAPIAttendee;
end;

class function CoRDPSRAPIAttendee.CreateRemote(const MachineName: string): IRDPSRAPIAttendee;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIAttendee) as IRDPSRAPIAttendee;
end;

class function CoRDPSRAPIAttendeeDisconnectInfo.Create: IRDPSRAPIAttendeeDisconnectInfo;
begin
  Result := CreateComObject(CLASS_RDPSRAPIAttendeeDisconnectInfo) as IRDPSRAPIAttendeeDisconnectInfo;
end;

class function CoRDPSRAPIAttendeeDisconnectInfo.CreateRemote(const MachineName: string): IRDPSRAPIAttendeeDisconnectInfo;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIAttendeeDisconnectInfo) as IRDPSRAPIAttendeeDisconnectInfo;
end;

class function CoRDPSRAPIApplicationFilter.Create: IRDPSRAPIApplicationFilter;
begin
  Result := CreateComObject(CLASS_RDPSRAPIApplicationFilter) as IRDPSRAPIApplicationFilter;
end;

class function CoRDPSRAPIApplicationFilter.CreateRemote(const MachineName: string): IRDPSRAPIApplicationFilter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIApplicationFilter) as IRDPSRAPIApplicationFilter;
end;

class function CoRDPSRAPIApplicationList.Create: IRDPSRAPIApplicationList;
begin
  Result := CreateComObject(CLASS_RDPSRAPIApplicationList) as IRDPSRAPIApplicationList;
end;

class function CoRDPSRAPIApplicationList.CreateRemote(const MachineName: string): IRDPSRAPIApplicationList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIApplicationList) as IRDPSRAPIApplicationList;
end;

class function CoRDPSRAPIApplication.Create: IRDPSRAPIApplication;
begin
  Result := CreateComObject(CLASS_RDPSRAPIApplication) as IRDPSRAPIApplication;
end;

class function CoRDPSRAPIApplication.CreateRemote(const MachineName: string): IRDPSRAPIApplication;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIApplication) as IRDPSRAPIApplication;
end;

class function CoRDPSRAPIWindowList.Create: IRDPSRAPIWindowList;
begin
  Result := CreateComObject(CLASS_RDPSRAPIWindowList) as IRDPSRAPIWindowList;
end;

class function CoRDPSRAPIWindowList.CreateRemote(const MachineName: string): IRDPSRAPIWindowList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIWindowList) as IRDPSRAPIWindowList;
end;

class function CoRDPSRAPIWindow.Create: IRDPSRAPIWindow;
begin
  Result := CreateComObject(CLASS_RDPSRAPIWindow) as IRDPSRAPIWindow;
end;

class function CoRDPSRAPIWindow.CreateRemote(const MachineName: string): IRDPSRAPIWindow;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIWindow) as IRDPSRAPIWindow;
end;

class function CoRDPSRAPITcpConnectionInfo.Create: IRDPSRAPITcpConnectionInfo;
begin
  Result := CreateComObject(CLASS_RDPSRAPITcpConnectionInfo) as IRDPSRAPITcpConnectionInfo;
end;

class function CoRDPSRAPITcpConnectionInfo.CreateRemote(const MachineName: string): IRDPSRAPITcpConnectionInfo;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPITcpConnectionInfo) as IRDPSRAPITcpConnectionInfo;
end;

class function CoRDPSession.Create: IRDPSRAPISharingSession2;
begin
  Result := CreateComObject(CLASS_RDPSession) as IRDPSRAPISharingSession2;
end;

class function CoRDPSession.CreateRemote(const MachineName: string): IRDPSRAPISharingSession2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSession) as IRDPSRAPISharingSession2;
end;

class function CoRDPSRAPIFrameBuffer.Create: IRDPSRAPIFrameBuffer;
begin
  Result := CreateComObject(CLASS_RDPSRAPIFrameBuffer) as IRDPSRAPIFrameBuffer;
end;

class function CoRDPSRAPIFrameBuffer.CreateRemote(const MachineName: string): IRDPSRAPIFrameBuffer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIFrameBuffer) as IRDPSRAPIFrameBuffer;
end;

class function CoRDPTransportStreamBuffer.Create: IRDPSRAPITransportStreamBuffer;
begin
  Result := CreateComObject(CLASS_RDPTransportStreamBuffer) as IRDPSRAPITransportStreamBuffer;
end;

class function CoRDPTransportStreamBuffer.CreateRemote(const MachineName: string): IRDPSRAPITransportStreamBuffer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPTransportStreamBuffer) as IRDPSRAPITransportStreamBuffer;
end;

class function CoRDPTransportStreamEvents.Create: IRDPSRAPITransportStreamEvents;
begin
  Result := CreateComObject(CLASS_RDPTransportStreamEvents) as IRDPSRAPITransportStreamEvents;
end;

class function CoRDPTransportStreamEvents.CreateRemote(const MachineName: string): IRDPSRAPITransportStreamEvents;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPTransportStreamEvents) as IRDPSRAPITransportStreamEvents;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TRDPViewer]);
end;

end.
