unit ActiveWinamp_TLB;

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

// $Rev: 17244 $
// File generated on 11/19/2010 11:23:43 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: c:\Program Files (x86)\Winamp\Plugins\gen_activewa.dll (1)
// LIBID: {142FF258-EE9C-4527-B2C7-4EAD10B752D9}
// LCID: 0
// Helpfile: 
// HelpString: gen_activewa 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// Errors:
//   Hint: Member 'Repeat' of 'IApplication' changed to 'Repeat_'
//   Hint: Parameter 'to' of IPlaylist.SwapIndex changed to 'to_'
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ActiveWinampMajorVersion = 1;
  ActiveWinampMinorVersion = 0;

  LIBID_ActiveWinamp: TGUID = '{142FF258-EE9C-4527-B2C7-4EAD10B752D9}';

  DIID__IApplicationEvents: TGUID = '{A1B0673A-2632-4FCA-8091-4CFD1F2A7710}';
  IID_IApplication: TGUID = '{2EBD7857-B229-4247-9FAA-17C505410474}';
  CLASS_Application: TGUID = '{1C7F39AF-65C0-4C14-A392-6B4714705DC2}';
  IID_IPlaylist: TGUID = '{83F2DBE6-FA22-4D6B-A281-0427D7AF490C}';
  IID_IMediaItem: TGUID = '{E7B30607-7180-4E40-A5C7-AF9F7D1C30C7}';
  IID_IMediaLibrary: TGUID = '{4033571A-3035-4B64-8A3A-E316691C972C}';
  CLASS_Playlist: TGUID = '{5108E2F5-A7E8-4284-9555-9FA8C3994B9C}';
  CLASS_MediaItem: TGUID = '{CF07CEDE-5BA9-414D-87C1-28737EDEE17D}';
  CLASS_MediaLibrary: TGUID = '{9A01F0B7-47F9-41F6-93C4-1B2ED242DA4B}';
  IID_ISiteManager: TGUID = '{1C95F212-3B28-4713-B6AB-35C422409D75}';
  CLASS_SiteManager: TGUID = '{89BBB22F-117F-4548-A2F8-FFDEBDF456D2}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _IApplicationEvents = dispinterface;
  IApplication = interface;
  IApplicationDisp = dispinterface;
  IPlaylist = interface;
  IPlaylistDisp = dispinterface;
  IMediaItem = interface;
  IMediaItemDisp = dispinterface;
  IMediaLibrary = interface;
  IMediaLibraryDisp = dispinterface;
  ISiteManager = interface;
  ISiteManagerDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Application = IApplication;
  Playlist = IPlaylist;
  MediaItem = IMediaItem;
  MediaLibrary = IMediaLibrary;
  SiteManager = ISiteManager;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PWideString1 = ^WideString; {*}


// *********************************************************************//
// DispIntf:  _IApplicationEvents
// Flags:     (4096) Dispatchable
// GUID:      {A1B0673A-2632-4FCA-8091-4CFD1F2A7710}
// *********************************************************************//
  _IApplicationEvents = dispinterface
    ['{A1B0673A-2632-4FCA-8091-4CFD1F2A7710}']
    procedure ChangedTrack; dispid 1;
    procedure ChangedVolume; dispid 2;
    procedure ChangedStatus; dispid 3;
    procedure PlaybackEOF; dispid 4;
  end;

// *********************************************************************//
// Interface: IApplication
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2EBD7857-B229-4247-9FAA-17C505410474}
// *********************************************************************//
  IApplication = interface(IDispatch)
    ['{2EBD7857-B229-4247-9FAA-17C505410474}']
    procedure SayHi; safecall;
    function Get_Playlist: IPlaylist; safecall;
    function Get_MediaLibrary: IMediaLibrary; safecall;
    procedure Play; safecall;
    procedure StopPlayback; safecall;
    procedure Pause; safecall;
    procedure Previous; safecall;
    procedure Skip; safecall;
    function LoadItem(var Filename: WideString): IMediaItem; safecall;
    function SetTimeout(timeout: Integer; const timeoutfunction: IDispatch): Integer; safecall;
    procedure CancelTimer(timerid: Integer); safecall;
    function GetIniFile: WideString; safecall;
    function GetIniDirectory: WideString; safecall;
    procedure ExecVisPlugin(const VisDllFile: WideString); safecall;
    function Get_Skin: WideString; safecall;
    procedure Set_Skin(const pVal: WideString); safecall;
    function Get_Shuffle: WordBool; safecall;
    procedure Set_Shuffle(pVal: WordBool); safecall;
    function Get_Repeat_: WordBool; safecall;
    procedure Set_Repeat_(pVal: WordBool); safecall;
    procedure RestartWinamp; safecall;
    procedure ShowNotification; safecall;
    function GetWaVersion: Integer; safecall;
    function GetSendToItems: OleVariant; safecall;
    function Get_Volume: Byte; safecall;
    procedure Set_Volume(pVal: Byte); safecall;
    function Get_Panning: SYSINT; safecall;
    procedure Set_Panning(pVal: SYSINT); safecall;
    function Get_PlayState: Integer; safecall;
    function Get_Position: Integer; safecall;
    procedure Set_Position(pVal: Integer); safecall;
    procedure RunScript(const scriptfile: WideString; const arguments: WideString); safecall;
    procedure UpdateTitle; safecall;
    function Get_Hwnd: Integer; safecall;
    function SendMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer; safecall;
    function PostMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer; safecall;
    property Playlist: IPlaylist read Get_Playlist;
    property MediaLibrary: IMediaLibrary read Get_MediaLibrary;
    property Skin: WideString read Get_Skin write Set_Skin;
    property Shuffle: WordBool read Get_Shuffle write Set_Shuffle;
    property Repeat_: WordBool read Get_Repeat_ write Set_Repeat_;
    property Volume: Byte read Get_Volume write Set_Volume;
    property Panning: SYSINT read Get_Panning write Set_Panning;
    property PlayState: Integer read Get_PlayState;
    property Position: Integer read Get_Position write Set_Position;
    property Hwnd: Integer read Get_Hwnd;
  end;

// *********************************************************************//
// DispIntf:  IApplicationDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2EBD7857-B229-4247-9FAA-17C505410474}
// *********************************************************************//
  IApplicationDisp = dispinterface
    ['{2EBD7857-B229-4247-9FAA-17C505410474}']
    procedure SayHi; dispid 1;
    property Playlist: IPlaylist readonly dispid 2;
    property MediaLibrary: IMediaLibrary readonly dispid 3;
    procedure Play; dispid 4;
    procedure StopPlayback; dispid 5;
    procedure Pause; dispid 6;
    procedure Previous; dispid 7;
    procedure Skip; dispid 8;
    function LoadItem(var Filename: WideString): IMediaItem; dispid 9;
    function SetTimeout(timeout: Integer; const timeoutfunction: IDispatch): Integer; dispid 10;
    procedure CancelTimer(timerid: Integer); dispid 11;
    function GetIniFile: WideString; dispid 12;
    function GetIniDirectory: WideString; dispid 13;
    procedure ExecVisPlugin(const VisDllFile: WideString); dispid 14;
    property Skin: WideString dispid 15;
    property Shuffle: WordBool dispid 16;
    property Repeat_: WordBool dispid 17;
    procedure RestartWinamp; dispid 18;
    procedure ShowNotification; dispid 19;
    function GetWaVersion: Integer; dispid 20;
    function GetSendToItems: OleVariant; dispid 21;
    property Volume: Byte dispid 22;
    property Panning: SYSINT dispid 23;
    property PlayState: Integer readonly dispid 24;
    property Position: Integer dispid 25;
    procedure RunScript(const scriptfile: WideString; const arguments: WideString); dispid 26;
    procedure UpdateTitle; dispid 27;
    property Hwnd: Integer readonly dispid 28;
    function SendMsg(msg: Integer; wParam: Integer; lParam: {??Int64}OleVariant): Integer; dispid 29;
    function PostMsg(msg: Integer; wParam: Integer; lParam: {??Int64}OleVariant): Integer; dispid 30;
  end;

// *********************************************************************//
// Interface: IPlaylist
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {83F2DBE6-FA22-4D6B-A281-0427D7AF490C}
// *********************************************************************//
  IPlaylist = interface(IDispatch)
    ['{83F2DBE6-FA22-4D6B-A281-0427D7AF490C}']
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(index: Integer): IMediaItem; safecall;
    function Get_Position: Integer; safecall;
    procedure Set_Position(pVal: Integer); safecall;
    function GetSelection: OleVariant; safecall;
    procedure Clear; safecall;
    procedure DeleteIndex(index: Integer); safecall;
    procedure SwapIndex(from: SYSINT; to_: SYSINT); safecall;
    procedure FlushCache; safecall;
    function Get_Hwnd: Integer; safecall;
    function SendMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer; safecall;
    function PostMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[index: Integer]: IMediaItem read Get_Item; default;
    property Position: Integer read Get_Position write Set_Position;
    property Hwnd: Integer read Get_Hwnd;
  end;

// *********************************************************************//
// DispIntf:  IPlaylistDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {83F2DBE6-FA22-4D6B-A281-0427D7AF490C}
// *********************************************************************//
  IPlaylistDisp = dispinterface
    ['{83F2DBE6-FA22-4D6B-A281-0427D7AF490C}']
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[index: Integer]: IMediaItem readonly dispid 0; default;
    property Position: Integer dispid 2;
    function GetSelection: OleVariant; dispid 3;
    procedure Clear; dispid 4;
    procedure DeleteIndex(index: Integer); dispid 5;
    procedure SwapIndex(from: SYSINT; to_: SYSINT); dispid 6;
    procedure FlushCache; dispid 7;
    property Hwnd: Integer readonly dispid 8;
    function SendMsg(msg: Integer; wParam: Integer; lParam: {??Int64}OleVariant): Integer; dispid 9;
    function PostMsg(msg: Integer; wParam: Integer; lParam: {??Int64}OleVariant): Integer; dispid 10;
  end;

// *********************************************************************//
// Interface: IMediaItem
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {E7B30607-7180-4E40-A5C7-AF9F7D1C30C7}
// *********************************************************************//
  IMediaItem = interface(IDispatch)
    ['{E7B30607-7180-4E40-A5C7-AF9F7D1C30C7}']
    function Get_Name: WideString; safecall;
    function Get_Filename: WideString; safecall;
    function Get_Position: Integer; safecall;
    procedure Set_Position(pVal: Integer); safecall;
    function Get_Title: WideString; safecall;
    procedure Set_Title(const pVal: WideString); safecall;
    function ATFString(const ATFSpecification: WideString): WideString; safecall;
    procedure Enqueue; safecall;
    function Get_Artist: WideString; safecall;
    procedure Set_Artist(const pVal: WideString); safecall;
    function Get_Album: WideString; safecall;
    procedure Set_Album(const pVal: WideString); safecall;
    function Get_Rating: Byte; safecall;
    procedure Set_Rating(pVal: Byte); safecall;
    function Get_Playcount: Integer; safecall;
    procedure Set_Playcount(pVal: Integer); safecall;
    procedure Insert(index: Integer); safecall;
    function Get_LastPlay: Integer; safecall;
    procedure Set_LastPlay(pVal: Integer); safecall;
    procedure RefreshMeta; safecall;
    function Get_DbIndex: Integer; safecall;
    function Get_Length: Integer; safecall;
    function Get_Track: Integer; safecall;
    function Get_Genre: WideString; safecall;
    procedure Set_Genre(const pVal: WideString); safecall;
    property Name: WideString read Get_Name;
    property Filename: WideString read Get_Filename;
    property Position: Integer read Get_Position write Set_Position;
    property Title: WideString read Get_Title write Set_Title;
    property Artist: WideString read Get_Artist write Set_Artist;
    property Album: WideString read Get_Album write Set_Album;
    property Rating: Byte read Get_Rating write Set_Rating;
    property Playcount: Integer read Get_Playcount write Set_Playcount;
    property LastPlay: Integer read Get_LastPlay write Set_LastPlay;
    property DbIndex: Integer read Get_DbIndex;
    property Length: Integer read Get_Length;
    property Track: Integer read Get_Track;
    property Genre: WideString read Get_Genre write Set_Genre;
  end;

// *********************************************************************//
// DispIntf:  IMediaItemDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {E7B30607-7180-4E40-A5C7-AF9F7D1C30C7}
// *********************************************************************//
  IMediaItemDisp = dispinterface
    ['{E7B30607-7180-4E40-A5C7-AF9F7D1C30C7}']
    property Name: WideString readonly dispid 0;
    property Filename: WideString readonly dispid 1;
    property Position: Integer dispid 2;
    property Title: WideString dispid 3;
    function ATFString(const ATFSpecification: WideString): WideString; dispid 4;
    procedure Enqueue; dispid 5;
    property Artist: WideString dispid 6;
    property Album: WideString dispid 7;
    property Rating: Byte dispid 8;
    property Playcount: Integer dispid 9;
    procedure Insert(index: Integer); dispid 10;
    property LastPlay: Integer dispid 11;
    procedure RefreshMeta; dispid 12;
    property DbIndex: Integer readonly dispid 13;
    property Length: Integer readonly dispid 14;
    property Track: Integer readonly dispid 15;
    property Genre: WideString dispid 16;
  end;

// *********************************************************************//
// Interface: IMediaLibrary
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4033571A-3035-4B64-8A3A-E316691C972C}
// *********************************************************************//
  IMediaLibrary = interface(IDispatch)
    ['{4033571A-3035-4B64-8A3A-E316691C972C}']
    function RunQueryArray(const QueryString: WideString): OleVariant; safecall;
    function GetItem(const Filename: WideString): IMediaItem; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(index: Integer): IMediaItem; safecall;
    function Get_Hwnd: Integer; safecall;
    function SendMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer; safecall;
    function PostMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[index: Integer]: IMediaItem read Get_Item; default;
    property Hwnd: Integer read Get_Hwnd;
  end;

// *********************************************************************//
// DispIntf:  IMediaLibraryDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4033571A-3035-4B64-8A3A-E316691C972C}
// *********************************************************************//
  IMediaLibraryDisp = dispinterface
    ['{4033571A-3035-4B64-8A3A-E316691C972C}']
    function RunQueryArray(const QueryString: WideString): OleVariant; dispid 1;
    function GetItem(const Filename: WideString): IMediaItem; dispid 2;
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[index: Integer]: IMediaItem readonly dispid 0; default;
    property Hwnd: Integer readonly dispid 3;
    function SendMsg(msg: Integer; wParam: Integer; lParam: {??Int64}OleVariant): Integer; dispid 4;
    function PostMsg(msg: Integer; wParam: Integer; lParam: {??Int64}OleVariant): Integer; dispid 5;
  end;

// *********************************************************************//
// Interface: ISiteManager
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {1C95F212-3B28-4713-B6AB-35C422409D75}
// *********************************************************************//
  ISiteManager = interface(IDispatch)
    ['{1C95F212-3B28-4713-B6AB-35C422409D75}']
    procedure AttachEvents(const EventObject: IDispatch; const ObjectPrefix: WideString); safecall;
    procedure Quit; safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const pVal: WideString); safecall;
    function Get_arguments: WideString; safecall;
    property Description: WideString read Get_Description write Set_Description;
    property arguments: WideString read Get_arguments;
  end;

// *********************************************************************//
// DispIntf:  ISiteManagerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {1C95F212-3B28-4713-B6AB-35C422409D75}
// *********************************************************************//
  ISiteManagerDisp = dispinterface
    ['{1C95F212-3B28-4713-B6AB-35C422409D75}']
    procedure AttachEvents(const EventObject: IDispatch; const ObjectPrefix: WideString); dispid 1;
    procedure Quit; dispid 2;
    property Description: WideString dispid 3;
    property arguments: WideString readonly dispid 4;
  end;

// *********************************************************************//
// The Class CoApplication provides a Create and CreateRemote method to          
// create instances of the default interface IApplication exposed by              
// the CoClass Application. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoApplication = class
    class function Create: IApplication;
    class function CreateRemote(const MachineName: string): IApplication;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TApplication
// Help String      : Application Class
// Default Interface: IApplication
// Def. Intf. DISP? : No
// Event   Interface: _IApplicationEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TApplicationProperties= class;
{$ENDIF}
  TApplication = class(TOleServer)
  private
    FOnChangedTrack: TNotifyEvent;
    FOnChangedVolume: TNotifyEvent;
    FOnChangedStatus: TNotifyEvent;
    FOnPlaybackEOF: TNotifyEvent;
    FIntf: IApplication;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TApplicationProperties;
    function GetServerProperties: TApplicationProperties;
{$ENDIF}
    function GetDefaultInterface: IApplication;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_Playlist: IPlaylist;
    function Get_MediaLibrary: IMediaLibrary;
    function Get_Skin: WideString;
    procedure Set_Skin(const pVal: WideString);
    function Get_Shuffle: WordBool;
    procedure Set_Shuffle(pVal: WordBool);
    function Get_Repeat_: WordBool;
    procedure Set_Repeat_(pVal: WordBool);
    function Get_Volume: Byte;
    procedure Set_Volume(pVal: Byte);
    function Get_Panning: SYSINT;
    procedure Set_Panning(pVal: SYSINT);
    function Get_PlayState: Integer;
    function Get_Position: Integer;
    procedure Set_Position(pVal: Integer);
    function Get_Hwnd: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IApplication);
    procedure Disconnect; override;
    procedure SayHi;
    procedure Play;
    procedure StopPlayback;
    procedure Pause;
    procedure Previous;
    procedure Skip;
    function LoadItem(var Filename: WideString): IMediaItem;
    function SetTimeout(timeout: Integer; const timeoutfunction: IDispatch): Integer;
    procedure CancelTimer(timerid: Integer);
    function GetIniFile: WideString;
    function GetIniDirectory: WideString;
    procedure ExecVisPlugin(const VisDllFile: WideString);
    procedure RestartWinamp;
    procedure ShowNotification;
    function GetWaVersion: Integer;
    function GetSendToItems: OleVariant;
    procedure RunScript(const scriptfile: WideString; const arguments: WideString);
    procedure UpdateTitle;
    function SendMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
    function PostMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
    property DefaultInterface: IApplication read GetDefaultInterface;
    property Playlist: IPlaylist read Get_Playlist;
    property MediaLibrary: IMediaLibrary read Get_MediaLibrary;
    property PlayState: Integer read Get_PlayState;
    property Hwnd: Integer read Get_Hwnd;
    property Skin: WideString read Get_Skin write Set_Skin;
    property Shuffle: WordBool read Get_Shuffle write Set_Shuffle;
    property Repeat_: WordBool read Get_Repeat_ write Set_Repeat_;
    property Volume: Byte read Get_Volume write Set_Volume;
    property Panning: SYSINT read Get_Panning write Set_Panning;
    property Position: Integer read Get_Position write Set_Position;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TApplicationProperties read GetServerProperties;
{$ENDIF}
    property OnChangedTrack: TNotifyEvent read FOnChangedTrack write FOnChangedTrack;
    property OnChangedVolume: TNotifyEvent read FOnChangedVolume write FOnChangedVolume;
    property OnChangedStatus: TNotifyEvent read FOnChangedStatus write FOnChangedStatus;
    property OnPlaybackEOF: TNotifyEvent read FOnPlaybackEOF write FOnPlaybackEOF;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TApplication
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TApplicationProperties = class(TPersistent)
  private
    FServer:    TApplication;
    function    GetDefaultInterface: IApplication;
    constructor Create(AServer: TApplication);
  protected
    function Get_Playlist: IPlaylist;
    function Get_MediaLibrary: IMediaLibrary;
    function Get_Skin: WideString;
    procedure Set_Skin(const pVal: WideString);
    function Get_Shuffle: WordBool;
    procedure Set_Shuffle(pVal: WordBool);
    function Get_Repeat_: WordBool;
    procedure Set_Repeat_(pVal: WordBool);
    function Get_Volume: Byte;
    procedure Set_Volume(pVal: Byte);
    function Get_Panning: SYSINT;
    procedure Set_Panning(pVal: SYSINT);
    function Get_PlayState: Integer;
    function Get_Position: Integer;
    procedure Set_Position(pVal: Integer);
    function Get_Hwnd: Integer;
  public
    property DefaultInterface: IApplication read GetDefaultInterface;
  published
    property Skin: WideString read Get_Skin write Set_Skin;
    property Shuffle: WordBool read Get_Shuffle write Set_Shuffle;
    property Repeat_: WordBool read Get_Repeat_ write Set_Repeat_;
    property Volume: Byte read Get_Volume write Set_Volume;
    property Panning: SYSINT read Get_Panning write Set_Panning;
    property Position: Integer read Get_Position write Set_Position;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoPlaylist provides a Create and CreateRemote method to          
// create instances of the default interface IPlaylist exposed by              
// the CoClass Playlist. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPlaylist = class
    class function Create: IPlaylist;
    class function CreateRemote(const MachineName: string): IPlaylist;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TPlaylist
// Help String      : Playlist Class
// Default Interface: IPlaylist
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TPlaylistProperties= class;
{$ENDIF}
  TPlaylist = class(TOleServer)
  private
    FIntf: IPlaylist;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TPlaylistProperties;
    function GetServerProperties: TPlaylistProperties;
{$ENDIF}
    function GetDefaultInterface: IPlaylist;
  protected
    procedure InitServerData; override;
    function Get_Count: Integer;
    function Get_Item(index: Integer): IMediaItem;
    function Get_Position: Integer;
    procedure Set_Position(pVal: Integer);
    function Get_Hwnd: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IPlaylist);
    procedure Disconnect; override;
    function GetSelection: OleVariant;
    procedure Clear;
    procedure DeleteIndex(index: Integer);
    procedure SwapIndex(from: SYSINT; to_: SYSINT);
    procedure FlushCache;
    function SendMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
    function PostMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
    property DefaultInterface: IPlaylist read GetDefaultInterface;
    property Count: Integer read Get_Count;
    property Item[index: Integer]: IMediaItem read Get_Item; default;
    property Hwnd: Integer read Get_Hwnd;
    property Position: Integer read Get_Position write Set_Position;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TPlaylistProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TPlaylist
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TPlaylistProperties = class(TPersistent)
  private
    FServer:    TPlaylist;
    function    GetDefaultInterface: IPlaylist;
    constructor Create(AServer: TPlaylist);
  protected
    function Get_Count: Integer;
    function Get_Item(index: Integer): IMediaItem;
    function Get_Position: Integer;
    procedure Set_Position(pVal: Integer);
    function Get_Hwnd: Integer;
  public
    property DefaultInterface: IPlaylist read GetDefaultInterface;
  published
    property Position: Integer read Get_Position write Set_Position;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoMediaItem provides a Create and CreateRemote method to          
// create instances of the default interface IMediaItem exposed by              
// the CoClass MediaItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMediaItem = class
    class function Create: IMediaItem;
    class function CreateRemote(const MachineName: string): IMediaItem;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TMediaItem
// Help String      : MediaItem Class
// Default Interface: IMediaItem
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TMediaItemProperties= class;
{$ENDIF}
  TMediaItem = class(TOleServer)
  private
    FIntf: IMediaItem;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TMediaItemProperties;
    function GetServerProperties: TMediaItemProperties;
{$ENDIF}
    function GetDefaultInterface: IMediaItem;
  protected
    procedure InitServerData; override;
    function Get_Name: WideString;
    function Get_Filename: WideString;
    function Get_Position: Integer;
    procedure Set_Position(pVal: Integer);
    function Get_Title: WideString;
    procedure Set_Title(const pVal: WideString);
    function Get_Artist: WideString;
    procedure Set_Artist(const pVal: WideString);
    function Get_Album: WideString;
    procedure Set_Album(const pVal: WideString);
    function Get_Rating: Byte;
    procedure Set_Rating(pVal: Byte);
    function Get_Playcount: Integer;
    procedure Set_Playcount(pVal: Integer);
    function Get_LastPlay: Integer;
    procedure Set_LastPlay(pVal: Integer);
    function Get_DbIndex: Integer;
    function Get_Length: Integer;
    function Get_Track: Integer;
    function Get_Genre: WideString;
    procedure Set_Genre(const pVal: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMediaItem);
    procedure Disconnect; override;
    function ATFString(const ATFSpecification: WideString): WideString;
    procedure Enqueue;
    procedure Insert(index: Integer);
    procedure RefreshMeta;
    property DefaultInterface: IMediaItem read GetDefaultInterface;
    property Name: WideString read Get_Name;
    property Filename: WideString read Get_Filename;
    property DbIndex: Integer read Get_DbIndex;
    property Length: Integer read Get_Length;
    property Track: Integer read Get_Track;
    property Position: Integer read Get_Position write Set_Position;
    property Title: WideString read Get_Title write Set_Title;
    property Artist: WideString read Get_Artist write Set_Artist;
    property Album: WideString read Get_Album write Set_Album;
    property Rating: Byte read Get_Rating write Set_Rating;
    property Playcount: Integer read Get_Playcount write Set_Playcount;
    property LastPlay: Integer read Get_LastPlay write Set_LastPlay;
    property Genre: WideString read Get_Genre write Set_Genre;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TMediaItemProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TMediaItem
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TMediaItemProperties = class(TPersistent)
  private
    FServer:    TMediaItem;
    function    GetDefaultInterface: IMediaItem;
    constructor Create(AServer: TMediaItem);
  protected
    function Get_Name: WideString;
    function Get_Filename: WideString;
    function Get_Position: Integer;
    procedure Set_Position(pVal: Integer);
    function Get_Title: WideString;
    procedure Set_Title(const pVal: WideString);
    function Get_Artist: WideString;
    procedure Set_Artist(const pVal: WideString);
    function Get_Album: WideString;
    procedure Set_Album(const pVal: WideString);
    function Get_Rating: Byte;
    procedure Set_Rating(pVal: Byte);
    function Get_Playcount: Integer;
    procedure Set_Playcount(pVal: Integer);
    function Get_LastPlay: Integer;
    procedure Set_LastPlay(pVal: Integer);
    function Get_DbIndex: Integer;
    function Get_Length: Integer;
    function Get_Track: Integer;
    function Get_Genre: WideString;
    procedure Set_Genre(const pVal: WideString);
  public
    property DefaultInterface: IMediaItem read GetDefaultInterface;
  published
    property Position: Integer read Get_Position write Set_Position;
    property Title: WideString read Get_Title write Set_Title;
    property Artist: WideString read Get_Artist write Set_Artist;
    property Album: WideString read Get_Album write Set_Album;
    property Rating: Byte read Get_Rating write Set_Rating;
    property Playcount: Integer read Get_Playcount write Set_Playcount;
    property LastPlay: Integer read Get_LastPlay write Set_LastPlay;
    property Genre: WideString read Get_Genre write Set_Genre;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoMediaLibrary provides a Create and CreateRemote method to          
// create instances of the default interface IMediaLibrary exposed by              
// the CoClass MediaLibrary. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMediaLibrary = class
    class function Create: IMediaLibrary;
    class function CreateRemote(const MachineName: string): IMediaLibrary;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TMediaLibrary
// Help String      : MediaLibrary Class
// Default Interface: IMediaLibrary
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TMediaLibraryProperties= class;
{$ENDIF}
  TMediaLibrary = class(TOleServer)
  private
    FIntf: IMediaLibrary;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TMediaLibraryProperties;
    function GetServerProperties: TMediaLibraryProperties;
{$ENDIF}
    function GetDefaultInterface: IMediaLibrary;
  protected
    procedure InitServerData; override;
    function Get_Item(index: Integer): IMediaItem;
    function Get_Hwnd: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMediaLibrary);
    procedure Disconnect; override;
    function RunQueryArray(const QueryString: WideString): OleVariant;
    function GetItem(const Filename: WideString): IMediaItem;
    function SendMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
    function PostMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
    property DefaultInterface: IMediaLibrary read GetDefaultInterface;
    property Item[index: Integer]: IMediaItem read Get_Item; default;
    property Hwnd: Integer read Get_Hwnd;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TMediaLibraryProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TMediaLibrary
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TMediaLibraryProperties = class(TPersistent)
  private
    FServer:    TMediaLibrary;
    function    GetDefaultInterface: IMediaLibrary;
    constructor Create(AServer: TMediaLibrary);
  protected
    function Get_Item(index: Integer): IMediaItem;
    function Get_Hwnd: Integer;
  public
    property DefaultInterface: IMediaLibrary read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoSiteManager provides a Create and CreateRemote method to          
// create instances of the default interface ISiteManager exposed by              
// the CoClass SiteManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSiteManager = class
    class function Create: ISiteManager;
    class function CreateRemote(const MachineName: string): ISiteManager;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSiteManager
// Help String      : SiteManager Class
// Default Interface: ISiteManager
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSiteManagerProperties= class;
{$ENDIF}
  TSiteManager = class(TOleServer)
  private
    FAutoQuit: Boolean;
    FIntf: ISiteManager;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TSiteManagerProperties;
    function GetServerProperties: TSiteManagerProperties;
{$ENDIF}
    function GetDefaultInterface: ISiteManager;
  protected
    procedure InitServerData; override;
    function Get_Description: WideString;
    procedure Set_Description(const pVal: WideString);
    function Get_arguments: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISiteManager);
    procedure Disconnect; override;
    procedure AttachEvents(const EventObject: IDispatch; const ObjectPrefix: WideString);
    procedure Quit;
    property DefaultInterface: ISiteManager read GetDefaultInterface;
    property arguments: WideString read Get_arguments;
    property Description: WideString read Get_Description write Set_Description;
  published
    property AutoQuit: Boolean read FAutoQuit write FAutoQuit; 
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSiteManagerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSiteManager
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSiteManagerProperties = class(TPersistent)
  private
    FServer:    TSiteManager;
    function    GetDefaultInterface: ISiteManager;
    constructor Create(AServer: TSiteManager);
  protected
    function Get_Description: WideString;
    procedure Set_Description(const pVal: WideString);
    function Get_arguments: WideString;
  public
    property DefaultInterface: ISiteManager read GetDefaultInterface;
  published
    property Description: WideString read Get_Description write Set_Description;
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'Digital Tundra';

  dtlOcxPage = 'Digital Tundra';

implementation

uses ComObj;

class function CoApplication.Create: IApplication;
begin
  Result := CreateComObject(CLASS_Application) as IApplication;
end;

class function CoApplication.CreateRemote(const MachineName: string): IApplication;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Application) as IApplication;
end;

procedure TApplication.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{1C7F39AF-65C0-4C14-A392-6B4714705DC2}';
    IntfIID:   '{2EBD7857-B229-4247-9FAA-17C505410474}';
    EventIID:  '{A1B0673A-2632-4FCA-8091-4CFD1F2A7710}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TApplication.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IApplication;
  end;
end;

procedure TApplication.ConnectTo(svrIntf: IApplication);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TApplication.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TApplication.GetDefaultInterface: IApplication;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TApplicationProperties.Create(Self);
{$ENDIF}
end;

destructor TApplication.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TApplication.GetServerProperties: TApplicationProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TApplication.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnChangedTrack) then
         FOnChangedTrack(Self);
    2: if Assigned(FOnChangedVolume) then
         FOnChangedVolume(Self);
    3: if Assigned(FOnChangedStatus) then
         FOnChangedStatus(Self);
    4: if Assigned(FOnPlaybackEOF) then
         FOnPlaybackEOF(Self);
  end; {case DispID}
end;

function TApplication.Get_Playlist: IPlaylist;
begin
    Result := DefaultInterface.Playlist;
end;

function TApplication.Get_MediaLibrary: IMediaLibrary;
begin
    Result := DefaultInterface.MediaLibrary;
end;

function TApplication.Get_Skin: WideString;
begin
    Result := DefaultInterface.Skin;
end;

procedure TApplication.Set_Skin(const pVal: WideString);
  { Warning: The property Skin has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Skin := pVal;
end;

function TApplication.Get_Shuffle: WordBool;
begin
    Result := DefaultInterface.Shuffle;
end;

procedure TApplication.Set_Shuffle(pVal: WordBool);
begin
  DefaultInterface.Set_Shuffle(pVal);
end;

function TApplication.Get_Repeat_: WordBool;
begin
    Result := DefaultInterface.Repeat_;
end;

procedure TApplication.Set_Repeat_(pVal: WordBool);
begin
  DefaultInterface.Set_Repeat_(pVal);
end;

function TApplication.Get_Volume: Byte;
begin
    Result := DefaultInterface.Volume;
end;

procedure TApplication.Set_Volume(pVal: Byte);
begin
  DefaultInterface.Set_Volume(pVal);
end;

function TApplication.Get_Panning: SYSINT;
begin
    Result := DefaultInterface.Panning;
end;

procedure TApplication.Set_Panning(pVal: SYSINT);
begin
  DefaultInterface.Set_Panning(pVal);
end;

function TApplication.Get_PlayState: Integer;
begin
    Result := DefaultInterface.PlayState;
end;

function TApplication.Get_Position: Integer;
begin
    Result := DefaultInterface.Position;
end;

procedure TApplication.Set_Position(pVal: Integer);
begin
  DefaultInterface.Set_Position(pVal);
end;

function TApplication.Get_Hwnd: Integer;
begin
    Result := DefaultInterface.Hwnd;
end;

procedure TApplication.SayHi;
begin
  DefaultInterface.SayHi;
end;

procedure TApplication.Play;
begin
  DefaultInterface.Play;
end;

procedure TApplication.StopPlayback;
begin
  DefaultInterface.StopPlayback;
end;

procedure TApplication.Pause;
begin
  DefaultInterface.Pause;
end;

procedure TApplication.Previous;
begin
  DefaultInterface.Previous;
end;

procedure TApplication.Skip;
begin
  DefaultInterface.Skip;
end;

function TApplication.LoadItem(var Filename: WideString): IMediaItem;
begin
  Result := DefaultInterface.LoadItem(Filename);
end;

function TApplication.SetTimeout(timeout: Integer; const timeoutfunction: IDispatch): Integer;
begin
  Result := DefaultInterface.SetTimeout(timeout, timeoutfunction);
end;

procedure TApplication.CancelTimer(timerid: Integer);
begin
  DefaultInterface.CancelTimer(timerid);
end;

function TApplication.GetIniFile: WideString;
begin
  Result := DefaultInterface.GetIniFile;
end;

function TApplication.GetIniDirectory: WideString;
begin
  Result := DefaultInterface.GetIniDirectory;
end;

procedure TApplication.ExecVisPlugin(const VisDllFile: WideString);
begin
  DefaultInterface.ExecVisPlugin(VisDllFile);
end;

procedure TApplication.RestartWinamp;
begin
  DefaultInterface.RestartWinamp;
end;

procedure TApplication.ShowNotification;
begin
  DefaultInterface.ShowNotification;
end;

function TApplication.GetWaVersion: Integer;
begin
  Result := DefaultInterface.GetWaVersion;
end;

function TApplication.GetSendToItems: OleVariant;
begin
  Result := DefaultInterface.GetSendToItems;
end;

procedure TApplication.RunScript(const scriptfile: WideString; const arguments: WideString);
begin
  DefaultInterface.RunScript(scriptfile, arguments);
end;

procedure TApplication.UpdateTitle;
begin
  DefaultInterface.UpdateTitle;
end;

function TApplication.SendMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
begin
  Result := DefaultInterface.SendMsg(msg, wParam, lParam);
end;

function TApplication.PostMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
begin
  Result := DefaultInterface.PostMsg(msg, wParam, lParam);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TApplicationProperties.Create(AServer: TApplication);
begin
  inherited Create;
  FServer := AServer;
end;

function TApplicationProperties.GetDefaultInterface: IApplication;
begin
  Result := FServer.DefaultInterface;
end;

function TApplicationProperties.Get_Playlist: IPlaylist;
begin
    Result := DefaultInterface.Playlist;
end;

function TApplicationProperties.Get_MediaLibrary: IMediaLibrary;
begin
    Result := DefaultInterface.MediaLibrary;
end;

function TApplicationProperties.Get_Skin: WideString;
begin
    Result := DefaultInterface.Skin;
end;

procedure TApplicationProperties.Set_Skin(const pVal: WideString);
  { Warning: The property Skin has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Skin := pVal;
end;

function TApplicationProperties.Get_Shuffle: WordBool;
begin
    Result := DefaultInterface.Shuffle;
end;

procedure TApplicationProperties.Set_Shuffle(pVal: WordBool);
begin
  DefaultInterface.Set_Shuffle(pVal);
end;

function TApplicationProperties.Get_Repeat_: WordBool;
begin
    Result := DefaultInterface.Repeat_;
end;

procedure TApplicationProperties.Set_Repeat_(pVal: WordBool);
begin
  DefaultInterface.Set_Repeat_(pVal);
end;

function TApplicationProperties.Get_Volume: Byte;
begin
    Result := DefaultInterface.Volume;
end;

procedure TApplicationProperties.Set_Volume(pVal: Byte);
begin
  DefaultInterface.Set_Volume(pVal);
end;

function TApplicationProperties.Get_Panning: SYSINT;
begin
    Result := DefaultInterface.Panning;
end;

procedure TApplicationProperties.Set_Panning(pVal: SYSINT);
begin
  DefaultInterface.Set_Panning(pVal);
end;

function TApplicationProperties.Get_PlayState: Integer;
begin
    Result := DefaultInterface.PlayState;
end;

function TApplicationProperties.Get_Position: Integer;
begin
    Result := DefaultInterface.Position;
end;

procedure TApplicationProperties.Set_Position(pVal: Integer);
begin
  DefaultInterface.Set_Position(pVal);
end;

function TApplicationProperties.Get_Hwnd: Integer;
begin
    Result := DefaultInterface.Hwnd;
end;

{$ENDIF}

class function CoPlaylist.Create: IPlaylist;
begin
  Result := CreateComObject(CLASS_Playlist) as IPlaylist;
end;

class function CoPlaylist.CreateRemote(const MachineName: string): IPlaylist;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Playlist) as IPlaylist;
end;

procedure TPlaylist.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{5108E2F5-A7E8-4284-9555-9FA8C3994B9C}';
    IntfIID:   '{83F2DBE6-FA22-4D6B-A281-0427D7AF490C}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TPlaylist.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IPlaylist;
  end;
end;

procedure TPlaylist.ConnectTo(svrIntf: IPlaylist);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TPlaylist.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TPlaylist.GetDefaultInterface: IPlaylist;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TPlaylist.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TPlaylistProperties.Create(Self);
{$ENDIF}
end;

destructor TPlaylist.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TPlaylist.GetServerProperties: TPlaylistProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TPlaylist.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TPlaylist.Get_Item(index: Integer): IMediaItem;
begin
    Result := DefaultInterface.Item[index];
end;

function TPlaylist.Get_Position: Integer;
begin
    Result := DefaultInterface.Position;
end;

procedure TPlaylist.Set_Position(pVal: Integer);
begin
  DefaultInterface.Set_Position(pVal);
end;

function TPlaylist.Get_Hwnd: Integer;
begin
    Result := DefaultInterface.Hwnd;
end;

function TPlaylist.GetSelection: OleVariant;
begin
  Result := DefaultInterface.GetSelection;
end;

procedure TPlaylist.Clear;
begin
  DefaultInterface.Clear;
end;

procedure TPlaylist.DeleteIndex(index: Integer);
begin
  DefaultInterface.DeleteIndex(index);
end;

procedure TPlaylist.SwapIndex(from: SYSINT; to_: SYSINT);
begin
  DefaultInterface.SwapIndex(from, to_);
end;

procedure TPlaylist.FlushCache;
begin
  DefaultInterface.FlushCache;
end;

function TPlaylist.SendMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
begin
  Result := DefaultInterface.SendMsg(msg, wParam, lParam);
end;

function TPlaylist.PostMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
begin
  Result := DefaultInterface.PostMsg(msg, wParam, lParam);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TPlaylistProperties.Create(AServer: TPlaylist);
begin
  inherited Create;
  FServer := AServer;
end;

function TPlaylistProperties.GetDefaultInterface: IPlaylist;
begin
  Result := FServer.DefaultInterface;
end;

function TPlaylistProperties.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TPlaylistProperties.Get_Item(index: Integer): IMediaItem;
begin
    Result := DefaultInterface.Item[index];
end;

function TPlaylistProperties.Get_Position: Integer;
begin
    Result := DefaultInterface.Position;
end;

procedure TPlaylistProperties.Set_Position(pVal: Integer);
begin
  DefaultInterface.Set_Position(pVal);
end;

function TPlaylistProperties.Get_Hwnd: Integer;
begin
    Result := DefaultInterface.Hwnd;
end;

{$ENDIF}

class function CoMediaItem.Create: IMediaItem;
begin
  Result := CreateComObject(CLASS_MediaItem) as IMediaItem;
end;

class function CoMediaItem.CreateRemote(const MachineName: string): IMediaItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_MediaItem) as IMediaItem;
end;

procedure TMediaItem.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{CF07CEDE-5BA9-414D-87C1-28737EDEE17D}';
    IntfIID:   '{E7B30607-7180-4E40-A5C7-AF9F7D1C30C7}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TMediaItem.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMediaItem;
  end;
end;

procedure TMediaItem.ConnectTo(svrIntf: IMediaItem);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TMediaItem.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TMediaItem.GetDefaultInterface: IMediaItem;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TMediaItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TMediaItemProperties.Create(Self);
{$ENDIF}
end;

destructor TMediaItem.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TMediaItem.GetServerProperties: TMediaItemProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TMediaItem.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

function TMediaItem.Get_Filename: WideString;
begin
    Result := DefaultInterface.Filename;
end;

function TMediaItem.Get_Position: Integer;
begin
    Result := DefaultInterface.Position;
end;

procedure TMediaItem.Set_Position(pVal: Integer);
begin
  DefaultInterface.Set_Position(pVal);
end;

function TMediaItem.Get_Title: WideString;
begin
    Result := DefaultInterface.Title;
end;

procedure TMediaItem.Set_Title(const pVal: WideString);
  { Warning: The property Title has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Title := pVal;
end;

function TMediaItem.Get_Artist: WideString;
begin
    Result := DefaultInterface.Artist;
end;

procedure TMediaItem.Set_Artist(const pVal: WideString);
  { Warning: The property Artist has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Artist := pVal;
end;

function TMediaItem.Get_Album: WideString;
begin
    Result := DefaultInterface.Album;
end;

procedure TMediaItem.Set_Album(const pVal: WideString);
  { Warning: The property Album has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Album := pVal;
end;

function TMediaItem.Get_Rating: Byte;
begin
    Result := DefaultInterface.Rating;
end;

procedure TMediaItem.Set_Rating(pVal: Byte);
begin
  DefaultInterface.Set_Rating(pVal);
end;

function TMediaItem.Get_Playcount: Integer;
begin
    Result := DefaultInterface.Playcount;
end;

procedure TMediaItem.Set_Playcount(pVal: Integer);
begin
  DefaultInterface.Set_Playcount(pVal);
end;

function TMediaItem.Get_LastPlay: Integer;
begin
    Result := DefaultInterface.LastPlay;
end;

procedure TMediaItem.Set_LastPlay(pVal: Integer);
begin
  DefaultInterface.Set_LastPlay(pVal);
end;

function TMediaItem.Get_DbIndex: Integer;
begin
    Result := DefaultInterface.DbIndex;
end;

function TMediaItem.Get_Length: Integer;
begin
    Result := DefaultInterface.Length;
end;

function TMediaItem.Get_Track: Integer;
begin
    Result := DefaultInterface.Track;
end;

function TMediaItem.Get_Genre: WideString;
begin
    Result := DefaultInterface.Genre;
end;

procedure TMediaItem.Set_Genre(const pVal: WideString);
  { Warning: The property Genre has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Genre := pVal;
end;

function TMediaItem.ATFString(const ATFSpecification: WideString): WideString;
begin
  Result := DefaultInterface.ATFString(ATFSpecification);
end;

procedure TMediaItem.Enqueue;
begin
  DefaultInterface.Enqueue;
end;

procedure TMediaItem.Insert(index: Integer);
begin
  DefaultInterface.Insert(index);
end;

procedure TMediaItem.RefreshMeta;
begin
  DefaultInterface.RefreshMeta;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TMediaItemProperties.Create(AServer: TMediaItem);
begin
  inherited Create;
  FServer := AServer;
end;

function TMediaItemProperties.GetDefaultInterface: IMediaItem;
begin
  Result := FServer.DefaultInterface;
end;

function TMediaItemProperties.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

function TMediaItemProperties.Get_Filename: WideString;
begin
    Result := DefaultInterface.Filename;
end;

function TMediaItemProperties.Get_Position: Integer;
begin
    Result := DefaultInterface.Position;
end;

procedure TMediaItemProperties.Set_Position(pVal: Integer);
begin
  DefaultInterface.Set_Position(pVal);
end;

function TMediaItemProperties.Get_Title: WideString;
begin
    Result := DefaultInterface.Title;
end;

procedure TMediaItemProperties.Set_Title(const pVal: WideString);
  { Warning: The property Title has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Title := pVal;
end;

function TMediaItemProperties.Get_Artist: WideString;
begin
    Result := DefaultInterface.Artist;
end;

procedure TMediaItemProperties.Set_Artist(const pVal: WideString);
  { Warning: The property Artist has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Artist := pVal;
end;

function TMediaItemProperties.Get_Album: WideString;
begin
    Result := DefaultInterface.Album;
end;

procedure TMediaItemProperties.Set_Album(const pVal: WideString);
  { Warning: The property Album has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Album := pVal;
end;

function TMediaItemProperties.Get_Rating: Byte;
begin
    Result := DefaultInterface.Rating;
end;

procedure TMediaItemProperties.Set_Rating(pVal: Byte);
begin
  DefaultInterface.Set_Rating(pVal);
end;

function TMediaItemProperties.Get_Playcount: Integer;
begin
    Result := DefaultInterface.Playcount;
end;

procedure TMediaItemProperties.Set_Playcount(pVal: Integer);
begin
  DefaultInterface.Set_Playcount(pVal);
end;

function TMediaItemProperties.Get_LastPlay: Integer;
begin
    Result := DefaultInterface.LastPlay;
end;

procedure TMediaItemProperties.Set_LastPlay(pVal: Integer);
begin
  DefaultInterface.Set_LastPlay(pVal);
end;

function TMediaItemProperties.Get_DbIndex: Integer;
begin
    Result := DefaultInterface.DbIndex;
end;

function TMediaItemProperties.Get_Length: Integer;
begin
    Result := DefaultInterface.Length;
end;

function TMediaItemProperties.Get_Track: Integer;
begin
    Result := DefaultInterface.Track;
end;

function TMediaItemProperties.Get_Genre: WideString;
begin
    Result := DefaultInterface.Genre;
end;

procedure TMediaItemProperties.Set_Genre(const pVal: WideString);
  { Warning: The property Genre has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Genre := pVal;
end;

{$ENDIF}

class function CoMediaLibrary.Create: IMediaLibrary;
begin
  Result := CreateComObject(CLASS_MediaLibrary) as IMediaLibrary;
end;

class function CoMediaLibrary.CreateRemote(const MachineName: string): IMediaLibrary;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_MediaLibrary) as IMediaLibrary;
end;

procedure TMediaLibrary.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{9A01F0B7-47F9-41F6-93C4-1B2ED242DA4B}';
    IntfIID:   '{4033571A-3035-4B64-8A3A-E316691C972C}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TMediaLibrary.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMediaLibrary;
  end;
end;

procedure TMediaLibrary.ConnectTo(svrIntf: IMediaLibrary);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TMediaLibrary.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TMediaLibrary.GetDefaultInterface: IMediaLibrary;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TMediaLibrary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TMediaLibraryProperties.Create(Self);
{$ENDIF}
end;

destructor TMediaLibrary.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TMediaLibrary.GetServerProperties: TMediaLibraryProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TMediaLibrary.Get_Item(index: Integer): IMediaItem;
begin
    Result := DefaultInterface.Item[index];
end;

function TMediaLibrary.Get_Hwnd: Integer;
begin
    Result := DefaultInterface.Hwnd;
end;

function TMediaLibrary.RunQueryArray(const QueryString: WideString): OleVariant;
begin
  Result := DefaultInterface.RunQueryArray(QueryString);
end;

function TMediaLibrary.GetItem(const Filename: WideString): IMediaItem;
begin
  Result := DefaultInterface.GetItem(Filename);
end;

function TMediaLibrary.SendMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
begin
  Result := DefaultInterface.SendMsg(msg, wParam, lParam);
end;

function TMediaLibrary.PostMsg(msg: Integer; wParam: Integer; lParam: Int64): Integer;
begin
  Result := DefaultInterface.PostMsg(msg, wParam, lParam);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TMediaLibraryProperties.Create(AServer: TMediaLibrary);
begin
  inherited Create;
  FServer := AServer;
end;

function TMediaLibraryProperties.GetDefaultInterface: IMediaLibrary;
begin
  Result := FServer.DefaultInterface;
end;

function TMediaLibraryProperties.Get_Item(index: Integer): IMediaItem;
begin
    Result := DefaultInterface.Item[index];
end;

function TMediaLibraryProperties.Get_Hwnd: Integer;
begin
    Result := DefaultInterface.Hwnd;
end;

{$ENDIF}

class function CoSiteManager.Create: ISiteManager;
begin
  Result := CreateComObject(CLASS_SiteManager) as ISiteManager;
end;

class function CoSiteManager.CreateRemote(const MachineName: string): ISiteManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SiteManager) as ISiteManager;
end;

procedure TSiteManager.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{89BBB22F-117F-4548-A2F8-FFDEBDF456D2}';
    IntfIID:   '{1C95F212-3B28-4713-B6AB-35C422409D75}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TSiteManager.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ISiteManager;
  end;
end;

procedure TSiteManager.ConnectTo(svrIntf: ISiteManager);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TSiteManager.DisConnect;
begin
  if Fintf <> nil then
  begin
    if FAutoQuit then
      Quit();
    FIntf := nil;
  end;
end;

function TSiteManager.GetDefaultInterface: ISiteManager;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TSiteManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSiteManagerProperties.Create(Self);
{$ENDIF}
end;

destructor TSiteManager.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSiteManager.GetServerProperties: TSiteManagerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TSiteManager.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TSiteManager.Set_Description(const pVal: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pVal;
end;

function TSiteManager.Get_arguments: WideString;
begin
    Result := DefaultInterface.arguments;
end;

procedure TSiteManager.AttachEvents(const EventObject: IDispatch; const ObjectPrefix: WideString);
begin
  DefaultInterface.AttachEvents(EventObject, ObjectPrefix);
end;

procedure TSiteManager.Quit;
begin
  DefaultInterface.Quit;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSiteManagerProperties.Create(AServer: TSiteManager);
begin
  inherited Create;
  FServer := AServer;
end;

function TSiteManagerProperties.GetDefaultInterface: ISiteManager;
begin
  Result := FServer.DefaultInterface;
end;

function TSiteManagerProperties.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TSiteManagerProperties.Set_Description(const pVal: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pVal;
end;

function TSiteManagerProperties.Get_arguments: WideString;
begin
    Result := DefaultInterface.arguments;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TApplication, TPlaylist, TMediaItem, TMediaLibrary, 
    TSiteManager]);
end;

end.
