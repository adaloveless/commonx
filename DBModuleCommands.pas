unit DBModuleCommands;

interface

uses
  RDTP_DatabaseConnection, stringx, systemx, typex, commandprocessor, sysutils, dir, classes, storageenginetypes, debug, betterobject, variants;

type
  TDBCommand = class(TCommand)
  private
    FRecordResults: boolean;
    FDBcontext: string;
  protected
    slWriteLog, slProblemLog: TStringlist;
    dbm: TBestDatabaseDM;
    procedure DoExecute; override;
    procedure DBExecute;virtual;abstract;
  public
    procedure Detach; override;

    property DBcontext: string read FDBcontext write FDBcontext;
    property RecordResults: boolean read FRecordResults write FRecordResults;
    procedure RecordQuery(sQuery: string);overload;
    procedure RecordQuery(sl: TStringlist);overload;//ONE QUERY PER LINE NO CRLFS IN QUERY!
    procedure InitExpense; override;
    procedure WriteAndLog(sQuery: string);
    procedure WriteQuery(sQuery: string);
    function ReadQuery(sQuery: string): TSERowSet;
    function ReadQueryH(sQuery: string): IHolder<TSERowSet>;
    function FunctionQuery(sQuery: string; default: ni): ni;overload;
    function FunctionQuery(sQuery: string; default: string): string;overload;
    procedure Problem(sProblem: string);
  end;


function GetReposFolder: string;


implementation


{ TDBCommand }

procedure TDBCommand.Detach;
begin
  if detached then exit;
  slWriteLog.free;
  slWriteLog := nil;
  slProblemLog.free;
  slProblemLog := nil;
  inherited;

end;

procedure TDBCommand.DoExecute;
begin
  inherited;
  slWriteLog := TStringlist.create;
  slProblemLog := TStringlist.create;
  dbm := TBestDatabaseDM.create;
  try
//  dbm.Context := 'simple;db=mmm_crimp_hd;host=crimp-hd.mysql.database.azure.com;user=crimpdbadmin@crimp-hd;pass=B@W@v}B5y^ue';
    dbm.Context := DBContext;
//    dbm.ConfigureFromContext;
    dbm.ConnectWrite;

    DBExecute;

    var finalfile := GetReposFolder+'_import.sql';
    var problemfile := GEtReposFolder+'_problems.txt';

    slProblemLog.savetoFile(problemfile);
    slWriteLog.savetoFile(finalfile);



  finally
    dbm.Free;
  end;

end;


function TDBCommand.FunctionQuery(sQuery, default: string): string;
begin
  var rows := ReadQueryH(sQuery);
  if rows.o.RowCount < 1 then
    exit(default)
  else begin
    if varisnull(rows.o.values[0,0]) then
      exit('')
    else
      exit(rows.o.values[0,0]);
  end;
end;

function TDBCommand.FunctionQuery(sQuery: string; default: ni): ni;
begin
  var rows := ReadQueryH(sQuery);
  if rows.o.RowCount < 1 then
    exit(default)
  else
    exit(rows.o.values[0,0]);
end;

procedure TDBCommand.InitExpense;
begin
  inherited;
  memoryexpense := 1.0;
end;

procedure TDBCommand.Problem(sProblem: string);
begin
  slProblemLog.add(sProblem);
end;

function TDBCommand.ReadQuery(sQuery: string): TSERowSet;
begin
  result := nil;
  dbm.ExecuteRead(sQuery, result);

end;

function TDBCommand.ReadQueryH(sQuery: string): IHolder<TSERowSet>;
begin
  result := THolder<TSERowSet>.create;
  result.o := ReadQuery(sQuery);
end;

procedure TDBCommand.RecordQuery(sl: TStringlist);
var
  t: ni;
begin
  for t:= 0 to sl.count-1 do begin
    RecordQuery(sl[t]);
  end;
end;

procedure TDBCommand.RecordQuery(sQuery: string);
begin
  if sQuery = '' then
    exit;
  if sQuery[high(sQuery)] <> ';' then
    sQuery := sQuery + ';';
  var sdir := GetReposFolder;
  forcedirectories(sdir);
//  var sFile := GetNextSequentialFileName(sDir,'.sql');
//  stringx.SaveStringAsFile(sFile, sQuery);
  slWriteLog.add(sQuery);

end;


procedure TDBCommand.WriteAndLog(sQuery: string);
begin
//  Debug.Log(sQuery);
  WriteQuery(sQuery);
  RecordQuery(sQuery);
end;

procedure TDBCommand.WriteQuery(sQuery: string);
begin
  dbm.ExecuteWrite(sQuery);
end;

function GetReposFolder: string;
begin
  result := dllpath+'repos\crimphd\';
end;


end.
