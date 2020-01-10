unit RDTPBeeperImpLib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPBeeperRQs.txt}
{END}

interface
uses
  beeper,windows, rdtpProcessor, RDTPBeeperServer, sysutils, debug, orderlyinit;

type
  TRDTPBeeperServer = class(TRDTPBeeperServerBase)
  private
  protected
  public
{INTERFACE_START}
    function RQ_Beep(freq:integer; duration:integer):boolean;overload;override;
    procedure RQ_BeepForget(freq:integer; duration:integer);overload;override;
    function RQ_TestInteger(a:integer; b:integer):integer;overload;override;
    function RQ_TestInt64(a:Int64; b:Int64):Int64;overload;override;
    function RQ_TestString(a:string; b:string):string;overload;override;
    function RQ_HelloTroy(ErectPenis:integer):string;overload;override;
    function RQ_HelloIvana(WriteToLog:string; freq:integer; duration:integer; attack:integer; release:integer):string;overload;override;

{INTERFACE_END}
  end;

implementation

uses rdtpserverlist;

//------------------------------------------------------------------------------
function TRDTPBeeperServer.RQ_Beep(freq:integer; duration:integer):boolean;
begin
  Debug.Log(self,'Beep!');
  beeper.Beep(freq, duration);
  result := true;
end;

procedure TRDTPBeeperServer.RQ_BeepForget(freq:integer; duration:integer);
begin
Log(self,'Beep! ... annd forget.');
  beeper.Beep(freq, duration);
end;

function TRDTPBeeperServer.RQ_HelloIvana(WriteToLog: string; freq, duration,
  attack, release: integer): string;
begin
  Debug.Log(self,WriteToLog);
  beeper.Beep(freq, duration, attack, release);
  result := 'Hello back!';
end;

function TRDTPBeeperServer.RQ_HelloTroy(ErectPenis: integer): string;
begin
  result := 'Hello Troy ('+inttostr(ErectPenis)+')';
end;

function TRDTPBeeperServer.RQ_TestInt64(a, b: Int64): Int64;
begin
  result := a+b;
end;

function TRDTPBeeperServer.RQ_TestInteger(a, b: integer): integer;
begin
  result := a+b;
end;

function TRDTPBeeperServer.RQ_TestString(a, b: string): string;
begin
  result := a+b;

end;


procedure oinit;
begin
  RDTPServers.RegisterRDTPProcessor('beeper', TRDTPBeeperServer);
end;

initialization

init.registerprocs('beeper', oinit, nil, 'RDTPServerList');
//need uses rdtpserverlist;


end.
