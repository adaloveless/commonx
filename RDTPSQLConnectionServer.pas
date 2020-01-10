unit RDTPSQLConnectionServer;
{GEN}
{TYPE SERVER}

{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPSQLConnectionSErverImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPSQLConnectionRQs.txt}
{CLASS TRDTPSQLConnectionServer}

{END}
interface


uses
  StorageEngineTypes, classes, RDTPProcessorForMySQL, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TRDTPSQLConnectionServerBase = class(TRDTPProcessorForMYSQL)
  private
    
    procedure RQ_HANDLE_Test(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_WriteQuery_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_ReadyToWriteBehind(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_WriteBehind_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_ReadQuery_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_BackProc_string_string_string_string_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_BeginTransaction(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_Commit(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_Rollback(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_BeginTransactionOn_integer(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_CommitOn_integer(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_RollbackOn_integer(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_ReadOn_integer_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_WriteOn_integer_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_WriteBehindOn_integer_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_GetNextID_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_SetNextID_string_int64(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_GetNextIDEx_string_string_string_int64(proc: TRDTPProcessorForMYSQL);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_Test():integer;overload;virtual;abstract;
    function RQ_WriteQuery(sQuery:string):boolean;overload;virtual;abstract;
    function RQ_ReadyToWriteBehind():boolean;overload;virtual;abstract;
    procedure RQ_WriteBehind(sQuery:string);overload;virtual;abstract;
    function RQ_ReadQuery(sQuery:string):TSERowSet;overload;virtual;abstract;
    function RQ_BackProc(exe_no_path:string; commandlineparams:string; backinputstringcontent:string; backinputfile:string; backoutputfile:string):TStream;overload;virtual;abstract;
    procedure RQ_BeginTransaction();overload;virtual;abstract;
    function RQ_Commit():boolean;overload;virtual;abstract;
    function RQ_Rollback():boolean;overload;virtual;abstract;
    procedure RQ_BeginTransactionOn(channel_const:integer);overload;virtual;abstract;
    procedure RQ_CommitOn(channel_const:integer);overload;virtual;abstract;
    procedure RQ_RollbackOn(channel_const:integer);overload;virtual;abstract;
    function RQ_ReadOn(channel_const:integer; query:string):TSERowSet;overload;virtual;abstract;
    function RQ_WriteOn(channel_const:integer; query:string):boolean;overload;virtual;abstract;
    procedure RQ_WriteBehindOn(channel_const:integer; query:string);overload;virtual;abstract;
    function RQ_GetNextID(key:string):int64;overload;virtual;abstract;
    procedure RQ_SetNextID(key:string; id:int64);overload;virtual;abstract;
    function RQ_GetNextIDEx(key:string; table:string; field:string; count:int64):int64;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPSQLConnectionSErverImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_Test(proc: TRDTPProcessorForMYSQL);
var
  res: integer;
begin
  res := RQ_Test();
  WriteintegerToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_WriteQuery_string(proc: TRDTPProcessorForMYSQL);
var
  res: boolean;
  sQuery:string;
begin
  GetstringFromPacket(proc.request, sQuery);
  res := RQ_WriteQuery(sQuery);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_ReadyToWriteBehind(proc: TRDTPProcessorForMYSQL);
var
  res: boolean;
begin
  res := RQ_ReadyToWriteBehind();
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_WriteBehind_string(proc: TRDTPProcessorForMYSQL);
var
  sQuery:string;
begin
  GetstringFromPacket(proc.request, sQuery);
  RQ_WriteBehind(sQuery);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_ReadQuery_string(proc: TRDTPProcessorForMYSQL);
var
  res: TSERowSet;
  sQuery:string;
begin
  GetstringFromPacket(proc.request, sQuery);
  res := RQ_ReadQuery(sQuery);
  WriteTSERowSetToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_BackProc_string_string_string_string_string(proc: TRDTPProcessorForMYSQL);
var
  res: TStream;
  exe_no_path:string;
  commandlineparams:string;
  backinputstringcontent:string;
  backinputfile:string;
  backoutputfile:string;
begin
  GetstringFromPacket(proc.request, exe_no_path);
  GetstringFromPacket(proc.request, commandlineparams);
  GetstringFromPacket(proc.request, backinputstringcontent);
  GetstringFromPacket(proc.request, backinputfile);
  GetstringFromPacket(proc.request, backoutputfile);
  res := RQ_BackProc(exe_no_path, commandlineparams, backinputstringcontent, backinputfile, backoutputfile);
  WriteTStreamToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_BeginTransaction(proc: TRDTPProcessorForMYSQL);
begin
  RQ_BeginTransaction();
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_Commit(proc: TRDTPProcessorForMYSQL);
var
  res: boolean;
begin
  res := RQ_Commit();
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_Rollback(proc: TRDTPProcessorForMYSQL);
var
  res: boolean;
begin
  res := RQ_Rollback();
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_BeginTransactionOn_integer(proc: TRDTPProcessorForMYSQL);
var
  channel_const:integer;
begin
  GetintegerFromPacket(proc.request, channel_const);
  RQ_BeginTransactionOn(channel_const);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_CommitOn_integer(proc: TRDTPProcessorForMYSQL);
var
  channel_const:integer;
begin
  GetintegerFromPacket(proc.request, channel_const);
  RQ_CommitOn(channel_const);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_RollbackOn_integer(proc: TRDTPProcessorForMYSQL);
var
  channel_const:integer;
begin
  GetintegerFromPacket(proc.request, channel_const);
  RQ_RollbackOn(channel_const);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_ReadOn_integer_string(proc: TRDTPProcessorForMYSQL);
var
  res: TSERowSet;
  channel_const:integer;
  query:string;
begin
  GetintegerFromPacket(proc.request, channel_const);
  GetstringFromPacket(proc.request, query);
  res := RQ_ReadOn(channel_const, query);
  WriteTSERowSetToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_WriteOn_integer_string(proc: TRDTPProcessorForMYSQL);
var
  res: boolean;
  channel_const:integer;
  query:string;
begin
  GetintegerFromPacket(proc.request, channel_const);
  GetstringFromPacket(proc.request, query);
  res := RQ_WriteOn(channel_const, query);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_WriteBehindOn_integer_string(proc: TRDTPProcessorForMYSQL);
var
  channel_const:integer;
  query:string;
begin
  GetintegerFromPacket(proc.request, channel_const);
  GetstringFromPacket(proc.request, query);
  RQ_WriteBehindOn(channel_const, query);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_GetNextID_string(proc: TRDTPProcessorForMYSQL);
var
  res: int64;
  key:string;
begin
  GetstringFromPacket(proc.request, key);
  res := RQ_GetNextID(key);
  Writeint64ToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_SetNextID_string_int64(proc: TRDTPProcessorForMYSQL);
var
  key:string;
  id:int64;
begin
  GetstringFromPacket(proc.request, key);
  Getint64FromPacket(proc.request, id);
  RQ_SetNextID(key, id);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPSQLConnectionServerBase.RQ_HANDLE_GetNextIDEx_string_string_string_int64(proc: TRDTPProcessorForMYSQL);
var
  res: int64;
  key:string;
  table:string;
  field:string;
  count:int64;
begin
  GetstringFromPacket(proc.request, key);
  GetstringFromPacket(proc.request, table);
  GetstringFromPacket(proc.request, field);
  Getint64FromPacket(proc.request, count);
  res := RQ_GetNextIDEx(key, table, field, count);
  Writeint64ToPacket(proc.response, res);
end;



{ TRDTPSQLConnectionServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TRDTPSQLConnectionServerBase.create;
begin
  inherited;
  ServiceName := 'RDTPSQLConnection';
end;

destructor TRDTPSQLConnectionServerBase.destroy;
begin

  inherited;
end;


function TRDTPSQLConnectionServerBase.Dispatch: boolean;
var
  iRQ: integer;
begin

  result := false;

  iRQ := request.data[0];
  request.seqseek(3);
  case iRQ of
    0: begin
        result := true;
//        beeper.Beep(100,100);
       end;
  
    //Test
    $1110:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Test','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Test(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Test','RDTPCALLS');
{$ENDIF}
      end;

    //WriteQuery
    $1111:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of WriteQuery','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_WriteQuery_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of WriteQuery','RDTPCALLS');
{$ENDIF}
      end;

    //ReadyToWriteBehind
    $1112:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ReadyToWriteBehind','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ReadyToWriteBehind(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ReadyToWriteBehind','RDTPCALLS');
{$ENDIF}
      end;

    //WriteBehind
    $1113:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of WriteBehind','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_WriteBehind_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of WriteBehind','RDTPCALLS');
{$ENDIF}
      end;

    //ReadQuery
    $1114:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ReadQuery','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ReadQuery_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ReadQuery','RDTPCALLS');
{$ENDIF}
      end;

    //BackProc
    $1115:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of BackProc','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_BackProc_string_string_string_string_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of BackProc','RDTPCALLS');
{$ENDIF}
      end;

    //BeginTransaction
    $1116:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of BeginTransaction','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_BeginTransaction(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of BeginTransaction','RDTPCALLS');
{$ENDIF}
      end;

    //Commit
    $1117:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Commit','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Commit(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Commit','RDTPCALLS');
{$ENDIF}
      end;

    //Rollback
    $1118:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Rollback','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Rollback(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Rollback','RDTPCALLS');
{$ENDIF}
      end;

    //BeginTransactionOn
    $111A:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of BeginTransactionOn','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_BeginTransactionOn_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of BeginTransactionOn','RDTPCALLS');
{$ENDIF}
      end;

    //CommitOn
    $111B:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of CommitOn','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_CommitOn_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of CommitOn','RDTPCALLS');
{$ENDIF}
      end;

    //RollbackOn
    $111C:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of RollbackOn','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_RollbackOn_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of RollbackOn','RDTPCALLS');
{$ENDIF}
      end;

    //ReadOn
    $111D:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ReadOn','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ReadOn_integer_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ReadOn','RDTPCALLS');
{$ENDIF}
      end;

    //WriteOn
    $111E:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of WriteOn','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_WriteOn_integer_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of WriteOn','RDTPCALLS');
{$ENDIF}
      end;

    //WriteBehindOn
    $111F:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of WriteBehindOn','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_WriteBehindOn_integer_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of WriteBehindOn','RDTPCALLS');
{$ENDIF}
      end;

    //GetNextID
    $1120:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetNextID','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetNextID_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetNextID','RDTPCALLS');
{$ENDIF}
      end;

    //SetNextID
    $1121:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of SetNextID','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_SetNextID_string_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of SetNextID','RDTPCALLS');
{$ENDIF}
      end;

    //GetNextIDEx
    $1122:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetNextIDEx','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetNextIDEx_string_string_string_int64(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetNextIDEx','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


