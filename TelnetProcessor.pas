unit TelnetProcessor;

interface
uses
  sysutils, better_sockets, sharedobject,  ipclientwrapper, classes, stringx, stringx.ansi;

type
  TTElnetSession = class;


  TTelnetProcessor = class(TSharedObject)
  private
    FEOL: ansistring;
    FInteractiveMode: boolean;
    FHistory: TStringlist;
    function GetCommandHistory(idx: integer): ansistring;
    procedure HandleControlSeq;
    function GetEOL: ansistring;
    procedure SetEOL(const Value: ansistring);
  protected
    FCommandBuffer: ansistring;
    ipc: TMyIpclient;
    control: boolean;
    control_seq: ansistring;
    FCommandIndex: integer;
    procedure CheckCommandIndexBounds;
  public
    constructor Create;override;
    destructor destroy;override;
    procedure Process(socket: TBetterCustomIPClient);
    procedure RegisterSession(session: TTelnetSession);
    procedure DeregisterSession(session: TTelnetSession);
    procedure AddToCommandBuffer(s: ansistring);overload;
    procedure AddToCommandBuffer(c: AnsiChar);overload;
    procedure ProcessCommand;overload;
    procedure Prompt;virtual;
    procedure Greet;virtual;
    procedure ProcessCommand(sFullLine: ansistring; sCMD: ansistring; params: TStringList);overload;virtual;

    property EOL: ansistring read GetEOL write SetEOL;
    procedure Error(sMessage: ansistring);virtual;
    property InteractiveMode: boolean read FInteractiveMode write FInteractiveMode;
    procedure EraseDrawCommandBuffer;
    procedure RedrawCommandBuffer;
    property CommandHistory[idx: integer]: ansistring read GetCommandHistory;
    procedure PreviousCommand;
    procedure NextCommand;
    procedure AddCommandToHistory(sCommand: ansistring);

  end;

  TTelnetSession = class(TSharedObject)
  private
    FProcessor: TTelnetProcessor;
    procedure SetProcessor(const Value: TTelnetProcessor);
  public
    constructor create(Processor: TTelnetProcessor);reintroduce;virtual;
    destructor destroy;override;

    property Processor: TTelnetProcessor read FProcessor write SetProcessor;

    procedure RegisterWithProcessor;
    procedure DeRegisterWithProcessor;
  end;





implementation

{ TTelnetProcessor }

procedure TTelnetProcessor.AddToCommandBuffer(s: ansistring);
var
  t: integer;
begin

  //if interactive
  if InteractiveMode then begin
    //add shit individually to check for control AnsiChars
    for t:= 1 to length(s) do begin
      AddToCommandBuffer(s[t]);
    end;
  end else
  begin
    FCommandBuffer := FCommandBuffer + s;
  end;
end;

procedure TTelnetProcessor.AddCommandToHistory(sCommand: ansistring);
begin
  FHistory.add(sCommand);
  self.FCommandIndex := FHistory.count-1;
end;

procedure TTelnetProcessor.AddToCommandBuffer(c: AnsiChar);
begin
  if c = #8 then begin
    control_seq := #8;
    HandleControlSeq;

  end else
  if c= #$1b then begin
     control := true;
     control_seq := '';
  end
  else begin
    if control then begin
        control_seq := control_seq + c;
        if length(control_seq) = 2 then begin
          HandleControlSeq;
          control := false;
        end;

    end else begin
      FCommandBuffer := FCommandBuffer + c;

    end;
  end;
end;

procedure TTelnetProcessor.HandleControlSeq;
begin
  if control_seq = #8 then begin
    EraseDrawCommandBuffer;
    FCommandBuffer := copy(FCommandBuffer, 1, length(FCommandBuffer)-1);
    RedrawCommandBuffer;
  end;

  if control_seq = '[A' then begin
    PreviousCommand;
  end;

  if control_seq = '[B' then begin
    NExtCommand;
  end;




end;


procedure TTelnetProcessor.NextCommand;
begin
  self.EraseDrawCommandBuffer;
  inc(FCommandIndex);
  self.CheckCommandIndexBounds;
  FCommandBuffer := FHistory[FCommandIndex];
  self.RedrawCommandBuffer;
end;

procedure TTelnetProcessor.DeregisterSession(session: TTelnetSession);
begin

//TODO -cunimplemented: unimplemented block
end;


destructor TTelnetProcessor.destroy;
begin
  FHistory.free;
  inherited;
end;

procedure TTelnetProcessor.EraseDrawCommandBuffer;
var
  t: integer;
begin
  ipc.SendText(#13);
  prompt;
  for t:= 1 to length(FCommandBuffer) do begin
    ipc.SendText(' ');
  end;
end;

procedure TTelnetProcessor.Error(sMessage: ansistring);
begin
  ipc.SendText('*ERROR*'+sMessage+EOL);
end;

function TTelnetProcessor.GetCommandHistory(idx: integer): ansistring;
begin

//TODO -cunimplemented: unimplemented block
end;

function TTelnetProcessor.GetEOL: ansistring;
begin
  Lock;
  try
    result := FEOL;
  finally
    Unlock;
  end;
end;

procedure TTelnetProcessor.Greet;
begin
  ipc.SendLine('Generic Telnet Server Processor');
  ipc.SendLine('(c)2005 Digital Tundra LLC');
end;

procedure TTelnetProcessor.PreviousCommand;
begin
  self.EraseDrawCommandBuffer;
  dec(FCommandIndex);
  self.CheckCommandIndexBounds;
  FCommandBuffer := FHistory[FCommandIndex];
  self.RedrawCommandBuffer;



end;

procedure TTelnetProcessor.Process(socket: TBetterCustomIPclient);
var
  c: ansistring;
begin
  ipc := TMYIPClient.create(false);
  ipc.Sendimmediate:=true;
  ipc.socket := socket;
  try
    Greet;
    Prompt;
    //echo mode
    while ipc.socket.Connected do begin

      c := ipc.ReceiveText(1);
      AddToCommandBuffer(c);
      if c = #10 then begin
        try
          ProcessCommand;
        except
          on e:Exception do begin
             ipc.SendLine('Error: '+e.message);
          end;
        end;
        Prompt;
      end;
    end;

  finally
    ipc.free;
    ipc := nil;
  end;



end;

procedure TTelnetProcessor.ProcessCommand;
var
  sLine: ansistring;
  sl : TStringList;
  sCMD: ansistring;
  sLeft, sright: ansistring;
begin

  sl := TStringList.create;
  try
    sLine := stringReplace(FCommandBuffer, #13, '', [rfReplaceAll]);
    sLine := stringReplace(sLine, #10, '', [rfReplaceAll]);
    FCommandBuffer := '';
    if not SplitString(sLine,ansistring(' '), sCMD, sRight) then begin
      sCMD := sLine;
    end else begin
      sl.text := stringReplace(sRight, ' ', #13#10, [rfReplaceAll]);
    end;

    self.AddCommandToHistory(sLine);
    self.ProcessCommand(sLine, sCMD, sl);
  finally
    sl.free;
  end;


  sLine := FCommandBuffer;
  FCommandBuffer := '';
  ipc.SendText(sLine);

end;



procedure TTelnetProcessor.ProcessCommand(sFullLine, sCMD: ansistring;
  params: TStringList);
begin
  ipc.SendLine('Unknown command: '+sCMD);

end;

procedure TTelnetProcessor.Prompt;
begin
  ipc.SendText('>');
end;

procedure TTelnetProcessor.RedrawCommandBuffer;
begin
  ipc.SendText(#13);
  prompt;
  ipc.SendText(FcommandBuffer);
end;

procedure TTelnetProcessor.RegisterSession(session: TTelnetSession);
begin
  //Todo: Complete REgisterSession
end;

procedure TTelnetProcessor.SetEOL(const Value: ansistring);
begin
  Lock;
  try
    FEOL := Value;
  finally
    Unlock;
  end;
end;

{ TTelnetSession }

constructor TTelnetSession.create(Processor: TTelnetProcessor);
begin
  //Todo: Complete DeRegisterSession
end;

procedure TTelnetSession.DeRegisterWithProcessor;
begin
  if FProcessor = nil then
    exit;

  FProcessor.DeregisterSession(self);

end;

destructor TTelnetSession.destroy;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TTelnetSession.RegisterWithProcessor;
begin
  if FProcessor = nil then
    exit;
  FProcessor.RegisterSession(self);
end;

procedure TTelnetSession.SetProcessor(const Value: TTelnetProcessor);
begin
  DeRegisterWithProcessor;
  Lock;
  try
    FProcessor := Value;
  finally
    UnLock;
  end;
  RegisterWithProcessor;

end;

procedure TTelnetProcessor.CheckCommandIndexBounds;
begin
  if FCommandIndex < 0 then FCommandINdex := 0;
  if FCommandIndex > FHistory.count-1 then
    FCommandIndex := FHistory.count-1;

end;

constructor TTelnetProcessor.Create;
begin
  inherited;
  FEOL := #13#10;
  FHistory := TSTringList.create;
end;


end.
