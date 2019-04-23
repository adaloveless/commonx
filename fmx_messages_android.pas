{$IFNDEF INCLUDED}
unit fmx_messages_android;
{$ENDIF}

interface

uses
    System.SysUtils, System.Types, System.Classes, System.SyncObjs,
    Generics.Collections,
    FMX.Platform.Android,
    Androidapi.AppGlue, Androidapi.Looper,
    Posix.UniStd, Posix.Errno, Posix.StrOpts, Posix.PThread;

const
    WM_USER         = 1024;

type
    LPARAM  = NativeInt;
    WPARAM  = NativeInt;
    LRESULT = NativeInt;

    TMessage = record
        Msg    : NativeInt;
        WParam : WPARAM;
        LParam : LPARAM;
        Result : LRESULT;
    end;
    TMessageHandler = procedure (var Msg: TMessage) of object;

    TMessagingSystem = class(TComponent)
    protected
        FPipeFD    : TPipeDescriptors;
        FData      : Byte;
        FHandlers  : TDictionary<nativeint,tmessagehandler>;
        FLastError : String;
        FCritSect  : TCriticalSection;
        procedure HandleMessage(var Msg : TMessage);
        function  CreatePipe: Integer;
        procedure ClosePipe;
        procedure InstallEventHandler;
        procedure UninstallEventHandler;
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

    HWND   = TMessagingSystem;

function GetCurrentThreadID : TThreadID;

implementation

function LooperCallback(
    FileDescriptor : Integer;
    Events         : Integer;
    Data           : Pointer): Integer; cdecl;
var
    Len : Integer;
    Msg : TMessage;
    Obj : TMessagingSystem;
begin
    Result := 1;
    // Data contains a reference to our class
    if Data = nil then
        Exit;
    // Ready to cast to our class
    Obj := TMessagingSystem(Data);
    // Check if it's our ReadDes
    Obj.FCritSect.Enter;
    try
        if FileDescriptor <> Obj.FPipeFD.ReadDes then
            Exit;
    finally
        Obj.FCritSect.Leave;
    end;

    while TRUE do begin
        Len := __read(FileDescriptor, @Msg, SizeOf(Msg));
        if Len <= 0 then
            break;
        Obj.HandleMessage(Msg);
    end;
end;

{ TMessagingSystem }

constructor TMessagingSystem.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FCritSect  := TCriticalSection.Create;
    FHandlers  := TDictionary<nativeint,tmessagehandler>.Create;
    CreatePipe;
    InstallEventHandler;
end;

destructor TMessagingSystem.Destroy;
begin
    UninstallEventHandler;
    ClosePipe;
    FreeAndNil(FCritSect);
    inherited Destroy;
end;

function TMessagingSystem.CreatePipe: Integer;
var
    Status  : Integer;
    Val     : Integer;
const
    FIONBIO = $5421;
begin
    FCritSect.Enter;
    try
        if (FPipeFD.ReadDes <> 0) or (FPipeFD.WriteDes <> 0) then begin
            FLastError := 'Pipe already created';
            Result := -1;
            Exit;
        end;
        Status := Pipe(FPipeFD);
        if Status = -1 then begin
            Result := errno;
            FLastError := 'Pipe() failed. Error #' + IntToStr(Result);
        end
        else begin
            Result := 0;
            Val := 1;
            if ioctl(FPipeFD.ReadDes, FIONBIO, @Val) = -1 then begin
                Result := errno;
                FLastError := 'ioctl(FIONBIO) failed. Error #' + IntToStr(Result);
                Exit;
            end;
        end;
    finally
        FCritSect.Leave;
    end;
end;

procedure TMessagingSystem.ClosePipe;
begin
    FCritSect.Enter;
    try
        if FPipeFD.ReadDes <> 0 then begin
            __close(FPipeFD.ReadDes);
            FPipeFD.ReadDes  := 0;
        end;
        if FPipeFD.WriteDes <> 0 then begin
            __close(FPipeFD.WriteDes);
            FPipeFD.WriteDes := 0;
        end;
    finally
        FCritSect.Leave;
    end;
end;

procedure TMessagingSystem.InstallEventHandler;
var
    AndroidApp : PAndroid_app;
    Data       : Pointer;
const
    LOOPER_ID_MESSAGE_OVERBYTE = LOOPER_ID_USER;
begin
    AndroidApp := GetAndroidApp;

    Data := Self;
    ALooper_addFd(AndroidApp.looper,
                  FPipeFD.ReadDes,
                  LOOPER_ID_MESSAGE_OVERBYTE,
                  ALOOPER_EVENT_INPUT,
                  LooperCallback,
                  Data);
end;

procedure TMessagingSystem.UninstallEventHandler;
var
    AndroidApp : PAndroid_app;
begin
    FCritSect.Enter;
    try
        if FPipeFD.ReadDes <> 0 then begin
            AndroidApp := GetAndroidApp;
            ALooper_removeFd(AndroidApp.looper, FPipeFD.ReadDes);
        end;
    finally
        FCritSect.Leave;
    end;
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
    WParam : WParam;
    LParam : LParam): Boolean;
var
    Msg : TMessage;
begin
    Result := FALSE;
    FCritSect.Enter;
    try
        if FPipeFD.WriteDes = 0 then begin
            FLastError := 'Pipe is not open';
            Exit;
        end;
        Msg.Msg    := uMsg;
        Msg.WParam := WParam;
        Msg.LParam := LParam;
        Msg.Result := 0;

        if __write(FPipeFD.WriteDes, @Msg, SizeOf(Msg)) = -1 then begin
            FLastError := 'write() failed. ErrCode=' + IntToStr(errno);
            Exit;
        end;
    finally
        FCritSect.Leave;
    end;
    Result := TRUE;
end;

procedure TMessagingSystem.HandleMessage(var Msg: TMessage);
var
    Handler : TMessageHandler;
    Status  : Boolean;
begin
    FCritSect.Enter;
    try
        Status := FHandlers.TryGetValue(Msg.Msg, Handler);
    finally
        FCritSect.Leave;
    end;
    if Status then
        Handler(Msg);
end;

function GetCurrentThreadID : TThreadID;
begin
    Result := Posix.PThread.GetCurrentThreadID;
end;

end.
