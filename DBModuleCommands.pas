unit DBModuleCommands;

interface

uses
  mysqlstoragestring, DatabaseconnectionDm, stringx, systemx, typex, commandprocessor, sysutils, dir, classes, storageenginetypes, debug, betterobject, variants;

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
    function GetCreateTable(sTable: string): string;overload;
    procedure Problem(sProblem: string);
    procedure BackupAndFlag(tblFrom: string; tblTo: string = '');
    procedure RestoreAndUnflag(tblOriginal, tblBackup: string);
    procedure UnflagBackup(tblOriginal, tblBackup: string);
    procedure RestoreUnfinishedOperations;
    procedure CreateBackupSystem;
    procedure AssertUncommitted;
    function TableHasColumn(sTable, sColumn: string): boolean;
    function GetColumnsEndingWith(sTable, sColumnEnd: string): TArray<string>;
  end;


var
  datafolder: string;
  contextfile: string;

implementation


{ TDBCommand }

procedure TDBCommand.AssertUncommitted;
var
  i: ni;
begin
  i := FunctionQuery('select count(*) from backup_table_log', 0);
  if i>0 then
    raise ECritical.create('***** THERE ARE UNCOMMITTED OPERATIONS, RUN COMMIT or ROLLBACK before continuing!!*********');
end;

procedure TDBCommand.BackupAndFlag(tblFrom, tblTo: string);
begin
  if tblTo = '' then
    tblTo := '_rollback_'+tblFrom;
  var cnt := functionquery('select count(*) from backup_table_log where tbl_original='+gvs(tblFrom)+' and tbl_backup='+gvs(tblTo),0);
  if cnt > 0 then
    raise ECritical.create('rollback first! there are unsucessful operations that have not been rolled back');
  WriteQuery('drop table if exists '+tblTo);
  dbm.CopyTable(tblFrom, tblTo);
//  WriteQuery('create table '+tblTo+' like '+tblFrom);
//  WriteQuery('insert into '+tblto+' select * from '+tblFrom);
  WriteQuery('insert into backup_table_log values (now(),'+gvs(tblFrom)+','+gvs(tblTo)+')');

end;


procedure TDBCommand.CreateBackupSystem;
begin
  WriteQuery('create table if not exists backup_table_log (ts timestamp, tbl_original varchar(255), tbl_backup varchar(255))');
end;

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
    CreateBackupSystem;
    DBExecute;

    var finalfile := datafolder+'_import.sql';
    var problemfile := datafolder+'_problems.txt';

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

function TDBCommand.GetColumnsEndingWith(sTable, sColumnEnd: string): TArray<string>;
var
  s, sJunk: string;
  t: ni;
  sl: IHolder<TStringList>;

begin
  s := GetCreateTable(sTable);
  s := lowercase(s);
  splitstringNocase(s, 'KEY ', s, sJunk);

  sl := ParseStringH(s, '`');
  for t := sl.o.count-1 downto 0 do begin
    if (t and 1)= 0 then
      sl.o.Delete(t);
  end;

  setlength(result,0);
  for t := 0 to sl.o.count-1 do begin
    if comparetext(zcopy(sl.o[t], length(sl.o[t])-length(sColumnEnd), length(sColumnEnd)), sColumnEnd)=0 then begin
      setlength(result,length(result)+1);
      result[high(result)] := sl.o[t];
    end;
  end;

end;

function TDBCommand.GetCreateTable(sTable: string): string;
begin
  var rows := ReadQueryH('show create table '+sTable);
  if rows.o.RowCount < 1 then
    exit('')
  else begin
    if rows.o.FieldCount < 2 then
      exit('');

    if varisnull(rows.o.values[1,0]) then
      exit('')
    else
      exit(rows.o.values[1,0]);
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

procedure TDBCommand.RestoreAndUnflag(tblOriginal, tblBackup: string);
var
  cnt: ni;
begin
  try
    cnt := FunctionQuery('select count(*) from '+tblBackup,0);
  except
    cnt := 0;
  end;
  if cnt > 0 then begin
//  if cnt = 0 then
//    raise ECritical.create('cannot restore from an empty backup');
    WriteQuery('drop table if exists '+tblOriginal);
    dbm.CopyTable(tblBackup, tblOriginal);
  //  WriteQuery('create table '+tblOriginal+' like '+tblBackup);
  //  WriteQuery('insert into '+tblOriginal+' select * from '+tblBackup);
    UnFlagBackUp(tblOriginal, tblBackup);
    WriteQuery('drop table if exists '+tblBackup);
  end;

end;

procedure TDBCommand.RestoreUnfinishedOperations;
begin
  var rs := ReadQueryH('select * from backup_table_log order by ts desc');
  for var t := 0 to rs.o.RowCount-1 do begin
    rs.o.Cursor := t;
    RestoreAndUnflag(rs.o['tbl_original'],rs.o['tbl_backup']);

  end;
end;


function TDBCommand.TableHasColumn(sTable, sColumn: string): boolean;
var
  s: string;
  t: ni;
begin
  s := GetCreateTable(sTable);
  s := lowercase(s);
  result := zpos(lowercase('`'+sColumn+'`'), s) >= 0;
end;

procedure TDBCommand.UnflagBackup(tblOriginal, tblBackup: string);
begin
  WriteQuery('delete from backup_table_log where tbl_original='+gvs(tblOriginal)+' and tbl_backup='+gvs(tblBackup));
end;

procedure TDBCommand.RecordQuery(sQuery: string);
begin
  exit;
  if sQuery = '' then
    exit;
  if sQuery[high(sQuery)] <> ';' then
    sQuery := sQuery + ';';
  var sdir := datafolder;
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



end.
