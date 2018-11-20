unit MotherShipTelnetDebug;

interface

uses
  telnetprocessor, classes, sysutils;

type
  TMotherShipDebugTelnetProcessor = class(TTelnetProcessor)
  private
  public
    constructor create;override;
    procedure ProcessCommand(sFullLine: ansistring; sCMD: ansistring; Params: TStringList);override;
    procedure Prompt;override;
    procedure Greet;override;

    procedure cmd_GetActiveRequests(sFullLine: ansistring; sCMD: ansistring; Params: TStringLIst);
    procedure cmd_GetRequestDetails(sFullLine: ansistring; sCMD: ansistring; Params: TStringLIst);


  end;


implementation

uses RequestInfo, REquestManager, systemx, stringx, typex;

{ TMotherShipDebugTelnetProcessor }

procedure TMotherShipDebugTelnetProcessor.cmd_GetActiveRequests(sFullLine,
  sCMD: ansistring; Params: TStringLIst);
var
  t: integer;
begin
  rqMan.Lock;
  try
    ipc.SendText('Count: '+inttostr(rqMan.count)+EOL);
    for t:= 0 to rqMan.Count-1 do begin
      ipc.SendText(inttostr(t)+':'+inttostr(rqMan.Ages[t])+rqMan.Requests[t]+EOL);
    end;
  finally
    rqMan.Unlock;
  end;

end;

procedure TMotherShipDebugTelnetProcessor.cmd_GetRequestDetails(sFullLine,
  sCMD: ansistring; Params: TStringLIst);
var
  iIndex: integer;
  rq: TRequestInfo;
begin
  //collect and validate
  if not IsInteger(params[0]) then begin
    error('Expected integer: '+params[0]);
    exit;
  end;

  iIndex := strtoint(params[0]);


  //process
  rqMan.Lock;
  try
    if iIndex > rqMan.count-1 then begin
      error('Request not found');
      exit;
    end;


    rq := rqMan.realrequests[iIndex];

    rq.Request.LockRead;
    try
      ipc.SendText(rq.request.Document+EOL);
      ipc.SendText(rq.Response.DebugLog.text+EOL);
    finally
      rq.Request.UnlockRead;
    end;


  finally
    rqMan.Unlock;
  end;




end;

constructor TMotherShipDebugTelnetProcessor.create;
begin
  inherited;
  InteractiveMOde := true; 
end;

procedure TMotherShipDebugTelnetProcessor.Greet;
begin
  ipc.SendLine('Mothership Debug Server 0.009');
  ipc.SendLine('(c)2006 Digital Tundra LLC');


end;

procedure TMotherShipDebugTelnetProcessor.ProcessCommand(sFullLine,
  sCMD: ansistring; Params: TStringList);
begin
  sCMD := lowercase(sCMD);
  if sCMD = 'getactiverequests' then begin
    cmd_GetActiveREquests(sFULLLIne, sCMD, Params);

  end else
  if sCMD = 'getrequestdetails' then begin
    cmd_GetRequestDetails(sFULLLIne, sCMD, Params);
  
  end else
  begin
    inherited;
  end;



end;

procedure TMotherShipDebugTelnetProcessor.Prompt;
begin
  ipc.SendText('>')

end;

end.
