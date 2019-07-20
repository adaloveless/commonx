unit RDTPSQLConnectionServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPSQLConnectionRQs.txt}
{END}
interface

uses
   typex, debug, rdtpprocessor, RDTPSQLConnectionServer, RDTPServerList, storageenginetypes, classes, systemx, exe, sysutils, stringx, dir,dirfile, commands_system;


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
  end else
    raise Ecritical.create('exe did not generate expected output');


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
