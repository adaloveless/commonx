{$IFNDEF INCLUDED}
unit fmx_messages_windows;
{$ENDIF}

interface

uses
    WinApi.Windows, WinApi.Messages,
    System.Classes, System.SysUtils, System.SyncObjs,
    Generics.Collections,
    FMX.Forms, FMX.Platform.Win;

const
    WM_USER = WinApi.Messages.WM_USER;

type
    TMessage        = WinApi.Messages.TMessage;
    WPARAM          = WinApi.Windows.WPARAM;
    LPARAM          = WinApi.Windows.LPARAM;
    TMessageHandler = procedure (var Msg: TMessage) of object;
    TWndProc        = function (hwnd   : HWND;
                                uMsg   : UINT;
                                wParam : WPARAM;
                                lParam : LPARAM): LRESULT; stdcall;

    TMessagingSystem = class(TComponent)
    protected
        FHWnd             : HWND;
        FHandlers         : TDictionary<nativeint,Tmessagehandler>;
        FOriginalWndProc  : TWndProc;
        FLastError        : String;
        FCritSect         : TCriticalSection;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        function RegisterMessageHandler(uMsg    : NativeInt;
                                        Handler : TMessageHandler) : Boolean;
        function PostMessage(uMsg   : NativeInt;
                             WParam : WPARAM;
                             LParam : LPARAM) : Boolean;
        property LastError : String read FLastError;
    end;

function GetCurrentThreadId: DWORD; stdcall;

implementation

var
  MsgSysAtom       : TAtom;
  MsgSysAtomString : String;


function WndProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
    Msg     : TMessage;
    MsgSys  : TMessagingSystem;
    Handler : TMessageHandler;
    Status  : Boolean;
begin
    // Search if the window handle is associated with TMessageingInstance
    // We know this because we registered an atom for that purpose
    if GlobalFindAtomW(PChar(MsgSysAtomString)) <> MsgSysAtom then begin
        // Not found, just do default processing
        Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
        Exit;
    end;
    // Fetch the atom property and cast it to a TMessagingSystem class
    MsgSys := TMessagingSystem(GetProp(hwnd, MakeIntAtom(MsgSysAtom)));

    // Now use the dictionary to see if the message is one we'll handle
    MsgSys.FCritSect.Enter;
    try
        Status := MsgSys.FHandlers.TryGetValue(uMsg, Handler);
    finally
        MsgSys.FCritSect.Leave;
    end;
    if Status then begin
        // Found the message and his message handler. Call it using
        // the TMessage record to hold the values
        Msg.Msg    := uMsg;
        Msg.WParam := wParam;
        Msg.LParam := lParam;
        Msg.Result := 0;
        Handler(Msg);
        Result := Msg.Result;
    end
    else begin
        // Not one of our messages, just execute original window procedure
        Result := MsgSys.FOriginalWndProc(hwnd, uMsg, wParam, lParam);
    end;
end;

{ TMessagingSystem }

constructor TMessagingSystem.Create(AOwner: TComponent);
begin
    if not (AOwner is TCommonCustomForm) then
        raise Exception.Create('TMessagingSystem.Create failed. Invalid owner');
    inherited Create(AOwner);
    FCritSect  := TCriticalSection.Create;
    FHandlers  := TDictionary<nativeint, Tmessagehandler>.Create;

    // Find window handle corresponding to the owner form
    FHWnd := WindowHandleToPlatform(TCommonCustomForm(AOwner).Handle).Wnd;

    // If not already done, register the atom we'll use to associate
    // our messaging system with the window handle
    if MsgSysAtom = 0 then begin
        MsgSysAtomString := 'OverbyteMessagingSystem' +
                                     IntToHex(GetCurrentProcessID, 8);
        MsgSysAtom       := GlobalAddAtomW(PChar(MsgSysAtomString));
    end;

    // Associate our messaging system with the window handle
    SetProp(FHWnd, MakeIntAtom(MsgSysAtom), THandle(Self));

    // Subclass the form. That is change his handling procedure
    FOriginalWndProc := TWndProc(GetWindowLongPtr(FHWnd, GWLP_WNDPROC));
    SetWindowLongPtr(FHWnd, GWLP_WNDPROC, NativeInt(@WndProc));
end;

destructor TMessagingSystem.Destroy;
begin
    if Assigned(FOriginalWndProc) then begin
        SetWindowLongPtr(FHWnd, GWLP_WNDPROC, NativeInt(@FOriginalWndProc));
        FOriginalWndProc := nil;
    end;
    FreeAndNil(FHandlers);
    FreeAndNil(FCritSect);
    inherited Destroy;
end;

function TMessagingSystem.RegisterMessageHandler(
    uMsg    : NativeInt;
    Handler : TMessageHandler): Boolean;
begin
    FCritSect.Enter;
    try
        FHandlers.AddOrSetValue(uMsg, Handler);
    finally
        FCritSect.Leave;
    end;
    Result := TRUE;
end;

function TMessagingSystem.PostMessage(
    uMsg   : NativeInt;
    WParam : WPARAM;
    LParam : LPARAM): Boolean;
begin
    Result := WinApi.Windows.PostMessage(FHWnd, uMsg, WParam, LParam);
end;

function GetCurrentThreadId: DWORD; stdcall;
begin
    Result := WinApi.Windows.GetCurrentThreadId;
end;

end.
