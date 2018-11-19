unit RDTPKeyBotServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPKeyBotRQs.txt}
{END}
interface

uses RDTPProcessor, keybotservice, sysutils, systemx, typex,  RDTPKeyBotServer, RDTPServerlist;

type
  TKeyBotServer = class(TKeyBotServerBase)
{INTERFACE_START}
    function RQ_GetNextID(iID:integer):int64;overload;override;
    function RQ_GetNextID_str(sID:string):int64;overload;override;
    function RQ_SetNextID_str(sID:string; val:int64):boolean;overload;override;
    function RQ_SetNextID(iID:integer; val:int64):boolean;overload;override;

{INTERFACE_END}
  end;

function PATHForStuff(proc: TRDTPProcessor): string;

implementation

function PATHForStuff(proc: TRDTPProcessor): string;
begin
  result := DLLPath+'keys';
end;


function TKeyBotServer.RQ_GetNextID(iID:integer):int64;
begin
  result := keybot.GetNextID(PathForStuff(self), inttostr(iID));
end;

function TKeyBotServer.RQ_GetNextID_str(sID:string):int64;
begin
  result := keybot.GetNextID(PathForStuff(self), sID);
end;




function TKeyBotServer.RQ_SetNextID_str(sID:string; val:int64):boolean;
begin
  keybot.SetNextID(PathForStuff(self), sID, val);
  result := true;
end;

function TKeyBotServer.RQ_SetNextID(iID:integer; val:int64):boolean;
begin
  keybot.SetNextID(PathForStuff(self), inttostr(iID), val);
  result := true;
end;


initialization
  RDTPServers.RegisterRDTPProcessor('keybot',TKeyBotServer);


end.
