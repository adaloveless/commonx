unit RDTPSQLConnectionServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPSQLConnectionRQs.txt}
{END}
interface

uses
   debug, rdtpprocessor, RDTPSQLConnectionServer, RDTPServerList, storageenginetypes;


type
  TRDTPSQLConnectionServer = class(TRDTPSQLConnectionServerBase)
  private
  protected
  public
{INTERFACE_START}
    function RQ_Test():integer;overload;override;
    function RQ_WriteQuery(sQuery:string):boolean;overload;override;
    function RQ_ReadyToWriteBehind():boolean;overload;override;
    procedure RQ_WriteBehind(sQuery:string);overload;override;
    function RQ_ReadQuery(sQuery:string):TSERowSet;overload;override;

{INTERFACE_END}
  end;
implementation


function TRDTPSQLConnectionServer.RQ_ReadQuery(
  sQuery: string): TSERowSet;
begin
  CheckContextSet;
  data.ExecuteRead(sQuery, result);

end;

function TRDTPSQLConnectionServer.RQ_ReadyToWriteBehind: boolean;
begin
  result := true;
end;

function TRDTPSQLConnectionServer.RQ_Test: integer;
begin
  result := 666;
end;


procedure TRDTPSQLConnectionServer.RQ_WriteBehind(sQuery: string);
begin
  Data.ExecuteWriteBehind(sQuery);
end;

function TRDTPSQLConnectionServer.RQ_WriteQuery(sQuery:string):boolean;
begin
  inherited;

//  debug.consolelog('write query request dispatched in context='+FContext);
  Data.ExecuteWriteBehind(sQuery);
  result := true;
end;

initialization

RDTPServers.RegisterRDTPProcessor('RDTPSQLConnection', TRDTPSQLConnectionServer);

end.
