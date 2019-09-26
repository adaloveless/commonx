unit RDTPSQLConnectionServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPSQLConnectionRQs.txt}
{END}
interface

uses
   typex, debug, rdtpprocessor, RDTPSQLConnectionServer, RDTPServerList, storageenginetypes, classes, systemx, exe, sysutils, stringx, dir,dirfile, commands_system, abstractrdtpdatamodule;


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
    function RQ_BackProc(exe_no_path:string; commandlineparams:string; backinputstringcontent:string; backinputfile:string; backoutputfile:string):TStream;overload;override;
    procedure RQ_BeginTransaction();overload;override;
    function RQ_Commit():boolean;overload;override;
    function RQ_Rollback():boolean;overload;override;
    procedure RQ_BeginTransactionOn(channel_const:integer);overload;override;
    procedure RQ_CommitOn(channel_const:integer);overload;override;
    procedure RQ_RollbackOn(channel_const:integer);overload;override;
    function RQ_ReadOn(channel_const:integer; query:string):TSERowSet;overload;override;
    function RQ_WriteOn(channel_const:integer; query:string):boolean;overload;override;
    procedure RQ_WriteBehindOn(channel_const:integer; query:string);overload;override;
    function RQ_GetNextID(key:string):int64;overload;override;
    procedure RQ_SetNextID(key:string; id:int64);overload;override;
    function RQ_GetNextIDEx(key:string; table:string; field:string):int64;overload;override;

{INTERFACE_END}
  end;
implementation


function TRDTPSQLConnectionServer.RQ_BackProc(exe_no_path:string; commandlineparams:string; backinputstringcontent:string; backinputfile:string; backoutputfile:string):TStream;
begin
  var temppath := systemx.GetTempPathForThread;

  forcedirectories(temppath);
  dir.Deletefilespec(temppath, '*.*');

  var fullexepath := dllpath+extractfilename(exe_no_path);
  var fullinputfile := temppath+extractfilename(backinputfile);
  var fulloutputfile := temppath+extractfilename(backoutputfile);

  //save input file in working directory
  SaveStringAsFile(fullinputfile, backinputstringcontent);

  //run EXE using working directory
  var c := exe.Tcmd_RunExe.Create;
  c.Prog := fullexepath;
  c.WorkingDir := temppath;
  c.Params := quote(temppath)+' '+commandlineparams;
  c.ConsoleRedirect := false;
  c.Start;
  try
    c.WaitFor;
  finally
    c.Free;
    c := nil;
  end;

  //check for results
  if fileExists(fulloutputfile) then begin
    result := TfileStream.create(backoutputfile, fmOpenRead+fmShareDenyNone);
    Debug.Log('attaching stream to '+backoutputfile+' that is '+result.size.tostring+' bytes');
  end else begin
    var errorfile := changefileext(backoutputfile, '.error');
    if fileexists(errorfile) then
      raise ECritical.Create('back process returned error: '+loadfileasstring(errorfile));
    raise Ecritical.create('exe did not generate expected output');
  end;


end;

procedure TRDTPSQLConnectionServer.RQ_BeginTransaction;
begin
  inherited;
  data.BeginTransaction;
end;

procedure TRDTPSQLConnectionServer.RQ_BeginTransactionOn(
  channel_const: integer);
begin
  inherited;
  data.BeginTransactionOn(TSQLChannel(channel_const));
end;

function TRDTPSQLConnectionServer.RQ_Commit: boolean;
begin
  data.Commit;
  result := true;
end;

procedure TRDTPSQLConnectionServer.RQ_CommitOn(channel_const: integer);
begin
  inherited;
  data.CommitOn(TSQLChannel(channel_const));
end;

function TRDTPSQLConnectionServer.RQ_GetNextID(key: string): int64;
begin
 result := data.GetNextID(key);
end;

function TRDTPSQLConnectionServer.RQ_GetNextIDEx(key, table,
  field: string): int64;
begin
  result := data.GetNextIDEx(key, table, field);
end;

function TRDTPSQLConnectionServer.RQ_ReadOn(channel_const: integer;
  query: string): TSERowSet;
begin
  result := data.readOn(TSQLChannel(channel_const), query);
end;

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

function TRDTPSQLConnectionServer.RQ_Rollback: boolean;
begin
  data.Rollback;
  result := true;
end;

procedure TRDTPSQLConnectionServer.RQ_RollbackOn(channel_const: integer);
begin
  inherited;
  data.RollbackOn(TSQLChannel(channel_const));
end;

procedure TRDTPSQLConnectionServer.RQ_SetNextID(key: string; id: int64);
begin
  inherited;
  data.SetNextID(key, id);
end;

function TRDTPSQLConnectionServer.RQ_Test: integer;
begin
  result := 666;
end;


procedure TRDTPSQLConnectionServer.RQ_WriteBehind(sQuery: string);
begin
  Data.ExecuteWriteBehind(sQuery);
end;

procedure TRDTPSQLConnectionServer.RQ_WriteBehindOn(channel_const: integer;
  query: string);
begin
  inherited;
  data.WriteOn(TSQLChannel(channel_const), query);


end;

function TRDTPSQLConnectionServer.RQ_WriteOn(channel_const: integer;
  query: string): boolean;
begin
  data.WriteOn(TSQLChannel(channel_const), query);
  result := true;

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
