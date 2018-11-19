unit RDTPLightShowServer;
{GEN}
{TYPE SERVER}
{CLASS TLightShowServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPLightShowServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPLightShowRQs.txt}
{END}
interface


uses
  classes, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, rdtpprocessorformysql, packethelpers, debug, RDTPServerList;



type
  TLightShowServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_GetFiles(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetQueue(proc: TRDTPProcessor);
    procedure RQ_HANDLE_QueueItem_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_NextItem(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Pause(proc: TRDTPProcessor);
    procedure RQ_HANDLE_Rewind(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetRemoteData(proc: TRDTPProcessor);
    procedure RQ_HANDLE_UnQueueItem_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_QueueMoveUp_integer(proc: TRDTPProcessor);
    procedure RQ_HANDLE_QueueMoveDown_integer(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_GetFiles():TStringList;overload;virtual;abstract;
    function RQ_GetQueue():TStringList;overload;virtual;abstract;
    procedure RQ_QueueItem(sName:string);overload;virtual;abstract;
    procedure RQ_NextItem();overload;virtual;abstract;
    procedure RQ_Pause();overload;virtual;abstract;
    procedure RQ_Rewind();overload;virtual;abstract;
    function RQ_GetRemoteData():TRemoteData;overload;virtual;abstract;
    procedure RQ_UnQueueItem(iIndex:integer);overload;virtual;abstract;
    procedure RQ_QueueMoveUp(iIndex:integer);overload;virtual;abstract;
    procedure RQ_QueueMoveDown(iIndex:integer);overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPLightShowServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_GetFiles(proc: TRDTPProcessor);
var
  res: TStringList;
begin
  res := RQ_GetFiles();
  WriteTStringListToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_GetQueue(proc: TRDTPProcessor);
var
  res: TStringList;
begin
  res := RQ_GetQueue();
  WriteTStringListToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_QueueItem_string(proc: TRDTPProcessor);
var
  sName:string;
begin
  GetstringFromPacket(proc.request, sName);
  RQ_QueueItem(sName);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_NextItem(proc: TRDTPProcessor);
begin
  RQ_NextItem();
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_Pause(proc: TRDTPProcessor);
begin
  RQ_Pause();
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_Rewind(proc: TRDTPProcessor);
begin
  RQ_Rewind();
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_GetRemoteData(proc: TRDTPProcessor);
var
  res: TRemoteData;
begin
  res := RQ_GetRemoteData();
  WriteTRemoteDataToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_UnQueueItem_integer(proc: TRDTPProcessor);
var
  iIndex:integer;
begin
  GetintegerFromPacket(proc.request, iIndex);
  RQ_UnQueueItem(iIndex);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_QueueMoveUp_integer(proc: TRDTPProcessor);
var
  iIndex:integer;
begin
  GetintegerFromPacket(proc.request, iIndex);
  RQ_QueueMoveUp(iIndex);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TLightShowServerBase.RQ_HANDLE_QueueMoveDown_integer(proc: TRDTPProcessor);
var
  iIndex:integer;
begin
  GetintegerFromPacket(proc.request, iIndex);
  RQ_QueueMoveDown(iIndex);
  proc.ForgetResult := true
end;



{ TLightShowServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TLightShowServerBase.create;
begin
  inherited;
  ServiceName := 'LightShow';
end;

destructor TLightShowServerBase.destroy;
begin

  inherited;
end;


function TLightShowServerBase.Dispatch: boolean;
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
  
    //GetFiles
    $4442:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetFiles','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetFiles(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetFiles','RDTPCALLS');
{$ENDIF}
      end;

    //GetQueue
    $4443:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetQueue','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetQueue(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetQueue','RDTPCALLS');
{$ENDIF}
      end;

    //QueueItem
    $4444:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of QueueItem','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_QueueItem_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of QueueItem','RDTPCALLS');
{$ENDIF}
      end;

    //NextItem
    $4445:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of NextItem','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_NextItem(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of NextItem','RDTPCALLS');
{$ENDIF}
      end;

    //Pause
    $4446:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Pause','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Pause(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Pause','RDTPCALLS');
{$ENDIF}
      end;

    //Rewind
    $4447:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of Rewind','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_Rewind(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of Rewind','RDTPCALLS');
{$ENDIF}
      end;

    //GetRemoteData
    $4448:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetRemoteData','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetRemoteData(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetRemoteData','RDTPCALLS');
{$ENDIF}
      end;

    //UnQueueItem
    $4450:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of UnQueueItem','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_UnQueueItem_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of UnQueueItem','RDTPCALLS');
{$ENDIF}
      end;

    //QueueMoveUp
    $4451:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of QueueMoveUp','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_QueueMoveUp_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of QueueMoveUp','RDTPCALLS');
{$ENDIF}
      end;

    //QueueMoveDown
    $4452:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of QueueMoveDown','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_QueueMoveDown_integer(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of QueueMoveDown','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


