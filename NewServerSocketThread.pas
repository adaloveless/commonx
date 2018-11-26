unit NewServerSocketThread;

interface

//uses
//  Sockets, classes, windows, windows, sysutils, speech, winsock;
//
//const
//  THREAD_CACHE_SIZE = 8;
//
//type
//  TBetterClientSocketThread = class(TClientSocketThread)
//  public
//    procedure Execute; override;
//
//  end;
//
//  TBetterTCPServer = class(TCustomTcpServer)
//  public
//    constructor create(aowner: Tcomponent; port: ansistring);reintroduce;virtual;
//    destructor destroy; override;
//    procedure DoAccept(ClientSocket: TCustomIpClient); override;
//  end;
//
//  TBestTCPServer = class(TIPSocket)
//  protected
//    FServerBlockMode: TServerSocketBlockMode;
//    FListening: Boolean;
//    FServerSocketThread: TServerSocketThread;
//{$IFDEF LINUX}
//    FThreadLock: TRTLCriticalSection;
//{$ENDIF}
//    FOnAccept: TSocketAcceptEvent;
//    FOnGetThread: TGetThreadEvent;
//    FOnListening: TNotifyEvent;
//
//    procedure GetThread(Sender: TObject; var ClientSocketThread: TClientSocketThread);//
//    function GetServerSocketThread: TServerSocketThread;//
//    procedure SetServerSocketThread(Value: TServerSocketThread);//
//    procedure SetServerBlockMode(Value: TServerSocketBlockMode);//
//
//  protected
//    procedure DoAccept(ClientSocket: TCustomIpClient); virtual;//
//    function Listen(backlog: Integer = SOMAXCONN): Boolean;//
//
//  public
//    constructor Create(AOwner: TComponent); override;//
//    destructor Destroy; override;//
//    procedure Open; override;//
//    procedure Close; override;//
//    function Accept: Boolean; overload;//
//    function Accept(var ClientSocket: TCustomIpClient): Boolean; overload;//
//    function WaitForConnection: Boolean;//
//
//    property BlockMode: TServerSocketBlockMode read FServerBlockMode write SetServerBlockMode default bmThreadBlocking;
//    property Listening: Boolean read FListening;
//    property ServerSocketThread: TServerSocketThread read GetServerSocketThread write SetServerSocketThread;
//    property OnAccept: TSocketAcceptEvent read FOnAccept write FOnAccept;
//    property OnGetThread: TGetThreadEvent read FOnGetThread write FOnGetThread;
//    property OnListening: TNotifyEvent read FOnListening write FOnListening;
//  end;
//
//
//  TNewServerSocketThread = class(TServerSocketThread)
//  private
//    Fring: boolean;
//    FPoolIndex: Integer;
//    FServerSocket: TCustomTcpServer;
//    FThreadCacheSize: Integer;
//    FThreadPool: TList;
//    FDeadThreads: TList;
//    FOnGetThread: TGetThreadEvent;
//
//    sect: TCLXCRiticalSection;
//    procedure SetThreadCacheSize(Value: Integer);
//    function GetRing: boolean;
//    procedure SetRing(const Value: boolean);
//
//  protected
//    function AddClientSocketThread: TClientSocketThread;
//    function CreateThread: TClientSocketThread; override;
//    function FetchClientSocketThread: TClientSocketThread;
//    procedure RemoveClientSocketThread(ClientSocketThread: TClientSocketThread);
//
//  public
//    Ready: boolean;
//    constructor Create(AServerSocket: TCustomTcpServer);
//    destructor Destroy; override;
//    procedure ClearThreadPool;reintroduce;
//    procedure Execute; override;
//    procedure CleanThreadPool;
//    property ServerSocket: TCustomTcpServer read FServerSocket;
//    property ThreadCacheSize: Integer read FThreadCacheSize write SetThreadCacheSize default 10;
//    property ThreadPool: TList read FThreadPool;
//    property OnGetThread: TGetThreadEvent read FOnGetThread write FOnGetThread;
//    procedure Lock;
//    procedure UnLock;
//    procedure DoneWithThread(thr: TClientSocketThread);
//    function WaitForConnection: boolean;
//    property Ring: boolean read GetRing write SetRing;
//    procedure GetStats(out Pool, Dead: integer);
//
//  end;
//
//
implementation
//
//{ TNewServerSocketThread }
//
//procedure TNewServerSocketThread.Execute;
//var
//  T: TClientSocketThread;
//begin
////  SayNatural('Server socket is accepting connections', true);
//
//  Ready := true;
//  while not Terminated (*and Assigned(FServerSocket) and FServerSocket.Listening*) do
//  begin
//     if WaitForConnection then begin
////      SayNatural('Incoming!',true);
//      if not Terminated then
//      begin
//        //set ring
//        Ring := true;
//
//        T := FetchClientSocketThread;
//        if not Assigned(T) then
//          T := AddClientSocketThread;
//        if Assigned(T) then begin
//          T.Resume;
//
//          //wait for thread to accept the ring
//          while ring do
//            sleep(10);
//
//          CleanThreadPool;
//        end;
//      end;
//    end else begin
//      CleanThreadPool;
//    end;
//  end;
//end;
//
//
//
//function TNewServerSocketThread.AddClientSocketThread: TClientSocketThread;
//begin
//  Result := nil;
//  if Assigned(FServerSocket) (*and (FThreadPool.Count < FThreadCacheSize)*) then
//  begin
//    if Assigned(FOnGetThread) then
//      FOnGetThread(Self, Result);
//    if not Assigned(Result) then
//      Result := CreateThread;
//    Lock;
//    try
//    if Assigned(Result) then
//      FThreadPool.Add(Result);
//    finally
//      Unlock;
//    end;
//  end
//end;
//
//
//procedure TNewServerSocketThread.ClearThreadPool;
//var
//  I: Integer;
//begin
//  // The code below terminates and frees threads synchronusly here
//  // free is automatically terminating and waiting for the thread!
//  while FThreadPool.Count > 0  do
//    with TClientSocketThread(FThreadPool[0]) do
//    begin
//        FreeOnTerminate     := False;
////        ServerSocketThread := Nil; // do not remove itself from the list
//        Free;
//    end;
//
//  while FDeadThreads.Count > 0 do
//    with TClientSocketThread(FDeadThreads[0]) do
//    begin
//        FreeOnTerminate     := False;
////        FServerSocketThread := Nil; // do not remove itself from the list
//        Free;
//    end;
//
//  FDeadThreads.Clear;
//  FThreadPool.Clear;
//end;
//
//constructor TNewServerSocketThread.Create(AServerSocket: TCustomTcpServer);
//begin
//  inherited Create(AServerSocket);
//
//  FreeOnTerminate := True;
//  FServerSocket := AServerSocket;
//  FThreadCacheSize := THREAD_CACHE_SIZE;;
//  FThreadPool := TList.Create;
//  FDeadThreads := TList.create;
//  InitializeCriticalSection(sect);
//
//
//  if not (AServerSocket is TBetterTCPServer) then
//    raise Exception.create('TNewServersocketThread no longer works with TCustomTcpServer');
//
//
//
//  self.Resume;
//
//end;
//
//function TNewServerSocketThread.CreateThread: TClientSocketThread;
//begin
//  Result := TClientSocketThread.Create(Self);
//end;
//
//destructor TNewServerSocketThread.Destroy;
//begin
////  SayNatural('Destroying socket thread.', true);
//  Lock;
//  try
//    ClearThreadPool;
//    while FThreadPool.Count > 0 do;
//    FThreadPool.Free;
//  finally
//    Unlock;
//  end;
//  inherited Destroy;
//  DeleteCriticalSection(sect);
//
//end;
//
//function TNewServerSocketThread.FetchClientSocketThread: TClientSocketThread;
////Returns a thread from the threadpool or creates a new one if necessary
//var
//  IndexRef: Integer;
//  t: integer;
//  thr: TClientSocketThread;
//begin
//  Lock;
//  try
//    Result := nil;
//    if Assigned(FServerSocket) and (FThreadPool.Count > 0) then
//    begin
//      IndexRef := FPoolIndex;
//      repeat
//        if indexRef > FThreadPool.count-1 then
//          indexref := FThreadPool.count-1;
//
//        FPoolIndex := (FPoolIndex + 1) mod FThreadPool.Count;
//        Result := FThreadPool[FPoolIndex];
//      until (FPoolIndex = IndexRef) or Result.Suspended;
//
//      if not Result.Suspended then
//        Result := nil;
//
//      if (result = nil) and (FDeadThreads.count > 0)then begin
//        result :=  TClientSocketThread(FDeadThreads[0]);
//        FDeadThreads.remove(result);
//        FThreadPool.add(result);
//      end;
//
//      if FThreadPool.Count > FThreadCacheSize then begin
//        for t:= FThreadPool.Count-1 downto 0 do begin
//          thr := FThreadPool[t];
//          if thr.Suspended then begin
//            FThreadPool.Remove(thr);
//            FDeadThreads.Add(thr);
////            DoneWithThread(thr);
//          end;
//        end;
//      end;
//    end;
//  finally
//    Unlock;
//  end;
//end;
//
//procedure TNewServerSocketThread.RemoveClientSocketThread(
//  ClientSocketThread: TClientSocketThread);
//begin
//  Lock;
//  try
//    FThreadPool.Remove(ClientSocketThread);
//    FDeadThreads.Remove(ClientSocketThread);
//
//    CleanThreadPool;
//
//  finally
//    Unlock;
//  end;
//end;
//
//procedure TNewServerSocketThread.SetThreadCacheSize(Value: Integer);
//begin
//  FThreadCacheSize := Value;
//end;
//
//procedure TNewServerSocketThread.CleanThreadPool;
//var
//  t: integer;
//  thr: TClientSocketThread;
//begin
//
//
//  self.Lock;
//  try
//    if (FDeadThreads.count + FThreadPool.Count) <= self.ThreadCacheSize then
//      exit;
//
//    for t:=FDeadThreads.Count-1 downto 0 do begin
//      thr := FDeadThreads[t];
//
//      thr.FreeOnTerminate := true;
//      if thr.suspended then
//        thr.resume;
//      thr.terminate;
////      thr.free;
//
//     FDeadThreads.Remove(thr);
//    end;
//
//    for t:=FThreadPool.Count-1 downto 0 do begin
//      thr := FThreadPool[t];
//      if thr.Suspended then begin
//        FThreadPool.Remove(thr);
//        FDeadThreads.Add(thr);
//      end;
//
//    end;
//
//  finally
//    self.Unlock;
//  end;
//
//end;
//
//procedure TNewServerSocketThread.Lock;
//begin
//  EnterCriticalSection(sect);
//end;
//
//procedure TNewServerSocketThread.UnLock;
//begin
//  LeaveCriticalsection(sect);
//
//end;
//
//procedure TNewServerSocketThread.DoneWithThread(thr: TClientSocketThread);
//begin
//  Lock;
//  try
//    FDeadThreads.remove(thr);
//    FThreadPool.remove(thr);
////    thr.Suspended := true;
//    FDeadThreads.insert(0, thr);
//  finally
//    Unlock;
//  end;
//
//end;
//
//function TNewServerSocketThread.WaitForConnection: boolean;
//var
//  ReadReady, ExceptFlag: Boolean;
//begin
//  Result := False;
//  if self.ServerSocket.Select(@ReadReady, nil, @ExceptFlag, 1000) then
//    Result := ReadReady and not ExceptFlag;
//
//end;
//
//
//procedure TNewServerSocketThread.GetStats(out Pool, Dead: integer);
//begin
//  Lock;
//  try
//    Pool := FThreadPool.count;
//    Dead := FDeadThreads.count;
//  finally
//    Unlock;
//  end;
//end;
//
//{ TBetterClientSocketThread }
//
//
//
//function TNewServerSocketThread.GetRing: boolean;
//begin
//  Lock;
//  try
//    result := FRing;
//  finally
//    Unlock;
//  end;
//end;
//
//procedure TNewServerSocketThread.SetRing(const Value: boolean);
//begin
//  Lock;
//  try
//    FRing := value;
//  finally
//    Unlock;
//  end;
//end;
//
//{ TBetterClientSocketThread }
//
//
//procedure TBetterClientSocketThread.Execute;
//var
//  FClientSocket: TCustomIPClient;
//begin
//  while not Terminated do
//  begin
//    if Assigned(ServerSocketThread) and Assigned(ServerSocketThread.ServerSocket) then
//    begin
//      FClientSocket := TCustomIpClient.Create(nil);
//      try
//        ServerSocketThread.ServerSocket.Accept(FClientSocket);
//      finally
//        FClientSocket.Free;
//        FClientSocket := nil;
//        if not Terminated then
//          Suspend;
//      end;
//    end;
//  end;
//end;
//
//{ TBetterTCPServer }
//
//constructor TBetterTCPServer.create(aowner: Tcomponent; port: ansistring);
//var
//  thr: TNewServerSocketThread;
//begin
//  inherited create(aowner);
//
//  LocalPort := port;
//
//  self.ServerSocketThread := nil;
//  thr := TNewServerSocketThread.create(self);
////  if ServerSocketThread.Suspended then
////    ServerSocketThread.Resume;
//
//  while not thr.Ready do
//    sleep(100);
//
//
//  if ServerSocketThread <> thr then
//    self.ServerSocketThread := thr;
//
//
//
////  SayNatural('Creating a better TCP server', true);
//
//
//end;
//
//destructor TBetterTCPServer.destroy;
//begin
//  self.ServerSocketThread := nil;
//  inherited;
//end;
//
//procedure TBetterTCPServer.DoAccept(ClientSocket: TCustomIpClient);
//var
//  thr: TNewServerSocketThread;
//begin
//(*  if not (serversocketthread is TNewServerSocketThread) then begin
//    thr := TNewServerSocketThread.create(self);
//    serversocketthread := thr;
//    while not thr.Ready do
//      sleep(100);
//    self.Active := true;
//  end;*)
//
//  if serversocketthread is TNewServerSocketThread then begin
//    TNewServerSocketThread(serversocketthread).Ring := false;
//  end else begin
//    SayNatural('Error',true);
//  end;
//  inherited;
//
//end;
//
//
//
//{ TBestTCPServer }
//
//function TBestTCPServer.Accept(var ClientSocket: TCustomIpClient): Boolean;
//var
//  sock: TSocket;
//  addr: TSockAddr;
//  len: Integer;
//begin
//  Result := False;
//  len := sizeof(addr);
//  Fillchar(addr, sizeof(addr), 0);
//  try
//{$IFDEF MSWINDOWS}
//    Sock := ErrorCheck(WinSock.accept(self.Handle, @addr, @len));
//{$ENDIF}
//{$IFDEF LINUX}
//    Sock := ErrorCheck(Libc.accept(FSocket, @addr, @len));
//{$ENDIF}
//  except
//    Sock := INVALID_SOCKET;
//  end;
//  if Sock <> INVALID_SOCKET then
//  begin
//    Result := True;
//    ClientSocket.FActive := True;
//    ClientSocket.FConnected := True;
//    ClientSocket.FSocket := Sock;
//    ClientSocket.FDomain := FDomain;
//    ClientSocket.SockType := FSockType;
//    ClientSocket.FProtocol := FProtocol;
//    ClientSocket.FBlockMode := FBlockMode;
//    ClientSocket.FRemoteHost := inet_ntoa(addr.sin_addr);
//    ClientSocket.FRemotePort := IntToStr(ntohs(addr.sin_port));
//    DoAccept(ClientSocket);
//  end;
//end;
//
//function TBestTCPServer.Accept: Boolean;
//var
//  ClientSocket: TCustomIpClient;
//begin
//  ClientSocket := TCustomIpClient.Create(nil);
//  try
//    Result := Accept(ClientSocket);
//  finally
//    ClientSocket.Free;
//  end;
//end;
//
//procedure TBestTCPServer.Close;
//begin
//  if (BlockMode = bmThreadBlocking) and Assigned(FServerSocketThread) then
//  begin
//    FServerSocketThread.Terminate;
//    FServerSocketThread := nil;
//{$IFDEF LINUX}
//    EnterCriticalSection(FThreadLock);
//    try
//    finally
//      LeaveCriticalSection(FThreadLock);
//    end;
//{$ENDIF}
//  end;
//  FListening := False;
//  inherited Close;
//end;
//
//constructor TBestTCPServer.Create(AOwner: TComponent);
//begin
//  inherited Create(AOwner);
//  FListening := False;
//  FServerSocketThread := nil;
//{$IFDEF LINUX}
//  InitializeCriticalSection(FThreadLock);
//{$ENDIF}
//  FOnAccept := nil;
//  FOnGetThread := nil;
//  FOnListening := nil;
//  BlockMode := bmThreadBlocking;
//end;
//
//destructor TBestTCPServer.Destroy;
//begin
//  inherited Open;
//
//  if Bind then
//    if Listen then
//      if BlockMode = bmThreadBlocking then
//      begin
//        GetServerSocketThread;
//        if Assigned(FServerSocketThread) and FServerSocketThread.Suspended then
//          FServerSocketThread.Resume;
//      end;
//end;
//
//procedure TBestTCPServer.DoAccept(ClientSocket: TCustomIpClient);
//begin
//  if Assigned(FOnAccept) then
//    FOnAccept(Self, ClientSocket);
//end;
//
//function TBestTCPServer.GetServerSocketThread: TServerSocketThread;
//begin
//  if not Assigned(FServerSocketThread) then
//    FServerSocketThread := TServerSocketThread.Create(Self);
//  if Assigned(FServerSocketThread) then
//    FServerSocketThread.OnGetThread := GetThread;
//  Result := FServerSocketThread;
//
//end;
//
//procedure TBestTCPServer.GetThread(Sender: TObject;
//  var ClientSocketThread: TClientSocketThread);
//begin
//  if Assigned(FOnGetThread) then
//    FOnGetThread(Self, ClientSocketThread);
//
//end;
//
//function TBestTCPServer.Listen(backlog: Integer): Boolean;
//begin
//  if Active and not FListening then
//  begin
//{$IFDEF MSWINDOWS}
//    FListening := ErrorCheck(WinSock.listen(FSocket, backlog)) = 0;
//{$ENDIF}
//{$IFDEF LINUX}
//    FListening := ErrorCheck(Libc.listen(FSocket, backlog)) = 0;
//{$ENDIF}
//  end;
//  Result := FListening;
//
//end;
//
//procedure TBestTCPServer.Open;
//begin
//  inherited Open;
//
//  if Bind then
//    if Listen then
//      if BlockMode = bmThreadBlocking then
//      begin
//        GetServerSocketThread;
//        if Assigned(FServerSocketThread) and FServerSocketThread.Suspended then
//          FServerSocketThread.Resume;
//      end;
//end;
//
//procedure TBestTCPServer.SetServerBlockMode(Value: TServerSocketBlockMode);
//begin
//  if Value <> FServerBlockMode then
//  begin
//    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
//      Close;
//    FServerBlockMode := Value;
//    if Value = bmThreadBlocking then
//      inherited BlockMode := bmBlocking
//    else
//      inherited BlockMode := Value;
//  end;
//
//end;
//
//procedure TBestTCPServer.SetServerSocketThread(Value: TServerSocketThread);
//begin
//  if Assigned(FServerSocketThread) then
//  begin
//    FServerSocketThread.Terminate;
//    Close;
//  end;
//  FServerSocketThread := Value;
//
//end;
//
//function TBestTCPServer.WaitForConnection: Boolean;
//var
//  ReadReady, ExceptFlag: Boolean;
//begin
//  Result := False;
//{$IFDEF LINUX}
//  if BlockMode = bmThreadBlocking then
//  begin
//    // Hack to avoid server thread block forever in linux
//    EnterCriticalSection(FThreadLock);
//    try
//      if Select(@ReadReady, nil, @ExceptFlag, 1000) then
//        Result := ReadReady and not ExceptFlag;
//    finally
//      LeaveCriticalSection(FThreadLock);
//    end;
//  end
//  else
//{$ENDIF}
//    if Select(@ReadReady, nil, @ExceptFlag, -1) then
//      Result := ReadReady and not ExceptFlag;
//end;
//
end.
