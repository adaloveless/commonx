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
  StorageEngineTypes, RDTPProcessorForMySQL, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TRDTPSQLConnectionServerBase = class(TRDTPProcessorForMYSQL)
  private
    
    procedure RQ_HANDLE_Test(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_WriteQuery_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_ReadyToWriteBehind(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_WriteBehind_string(proc: TRDTPProcessorForMYSQL);
    procedure RQ_HANDLE_ReadQuery_string(proc: TRDTPProcessorForMYSQL);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_Test():integer;overload;virtual;abstract;
    function RQ_WriteQuery(sQuery:string):boolean;overload;virtual;abstract;
    function RQ_ReadyToWriteBehind():boolean;overload;virtual;abstract;
    procedure RQ_WriteBehind(sQuery:string);overload;virtual;abstract;
    function RQ_ReadQuery(sQuery:string):TSERowSet;overload;virtual;abstract;


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

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


