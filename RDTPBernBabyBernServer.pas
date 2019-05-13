unit RDTPBernBabyBernServer;
{GEN}
{TYPE SERVER}
{CLASS TRDTPBernBabyBernServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPBernBabyBernServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPBernBabyBernRQs.txt}
{END}
interface


uses
  classes, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TRDTPBernBabyBernServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_ListFiles_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetFile_string(proc: TRDTPProcessor);
    procedure RQ_HANDLE_PutFile_string_TStream(proc: TRDTPProcessor);
    procedure RQ_HANDLE_PutSpriteSheet_string_TStream_TStream_TStream(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetSpriteSheet_string_TStream_TStream_TStream(proc: TRDTPProcessor);
    procedure RQ_HANDLE_PutSpriteSheet2_string_TStream_TStream_TStream_TStream(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetSpriteSheet2_string_TStream_TStream_TStream_TStream(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_ListFiles(filespec:string):string;overload;virtual;abstract;
    function RQ_GetFile(filename:string):TStream;overload;virtual;abstract;
    function RQ_PutFile(filename:string; bytes:TStream):boolean;overload;virtual;abstract;
    procedure RQ_PutSpriteSheet(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream);overload;virtual;abstract;
    function RQ_GetSpriteSheet(filename:string; out bytes0:TStream; out bytes1:TStream; out bytes2:TStream):boolean;overload;virtual;abstract;
    procedure RQ_PutSpriteSheet2(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream; refmap:TStream);overload;virtual;abstract;
    function RQ_GetSpriteSheet2(filename:string; out bytes0:TStream; out bytes1:TStream; out bytes2:TStream; out refmap:TStream):boolean;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPBernBabyBernServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBernBabyBernServerBase.RQ_HANDLE_ListFiles_string(proc: TRDTPProcessor);
var
  res: string;
  filespec:string;
begin
  GetstringFromPacket(proc.request, filespec);
  res := RQ_ListFiles(filespec);
  WritestringToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBernBabyBernServerBase.RQ_HANDLE_GetFile_string(proc: TRDTPProcessor);
var
  res: TStream;
  filename:string;
begin
  GetstringFromPacket(proc.request, filename);
  res := RQ_GetFile(filename);
  WriteTStreamToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBernBabyBernServerBase.RQ_HANDLE_PutFile_string_TStream(proc: TRDTPProcessor);
var
  res: boolean;
  filename:string;
  bytes:TStream;
begin
  GetstringFromPacket(proc.request, filename);
  GetTStreamFromPacket(proc.request, bytes);
  res := RQ_PutFile(filename, bytes);
  WritebooleanToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBernBabyBernServerBase.RQ_HANDLE_PutSpriteSheet_string_TStream_TStream_TStream(proc: TRDTPProcessor);
var
  filename:string;
  bytes0:TStream;
  bytes1:TStream;
  bytes2:TStream;
begin
  GetstringFromPacket(proc.request, filename);
  GetTStreamFromPacket(proc.request, bytes0);
  GetTStreamFromPacket(proc.request, bytes1);
  GetTStreamFromPacket(proc.request, bytes2);
  RQ_PutSpriteSheet(filename, bytes0, bytes1, bytes2);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBernBabyBernServerBase.RQ_HANDLE_GetSpriteSheet_string_TStream_TStream_TStream(proc: TRDTPProcessor);
var
  res: boolean;
  filename:string;
  bytes0:TStream;
  bytes1:TStream;
  bytes2:TStream;
begin
  GetstringFromPacket(proc.request, filename);
  res := RQ_GetSpriteSheet(filename, bytes0, bytes1, bytes2);
  WritebooleanToPacket(proc.response, res);
  WriteTStreamToPacket(proc.response, bytes0);
  WriteTStreamToPacket(proc.response, bytes1);
  WriteTStreamToPacket(proc.response, bytes2);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBernBabyBernServerBase.RQ_HANDLE_PutSpriteSheet2_string_TStream_TStream_TStream_TStream(proc: TRDTPProcessor);
var
  filename:string;
  bytes0:TStream;
  bytes1:TStream;
  bytes2:TStream;
  refmap:TStream;
begin
  GetstringFromPacket(proc.request, filename);
  GetTStreamFromPacket(proc.request, bytes0);
  GetTStreamFromPacket(proc.request, bytes1);
  GetTStreamFromPacket(proc.request, bytes2);
  GetTStreamFromPacket(proc.request, refmap);
  RQ_PutSpriteSheet2(filename, bytes0, bytes1, bytes2, refmap);
  proc.ForgetResult := true
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TRDTPBernBabyBernServerBase.RQ_HANDLE_GetSpriteSheet2_string_TStream_TStream_TStream_TStream(proc: TRDTPProcessor);
var
  res: boolean;
  filename:string;
  bytes0:TStream;
  bytes1:TStream;
  bytes2:TStream;
  refmap:TStream;
begin
  GetstringFromPacket(proc.request, filename);
  res := RQ_GetSpriteSheet2(filename, bytes0, bytes1, bytes2, refmap);
  WritebooleanToPacket(proc.response, res);
  WriteTStreamToPacket(proc.response, bytes0);
  WriteTStreamToPacket(proc.response, bytes1);
  WriteTStreamToPacket(proc.response, bytes2);
  WriteTStreamToPacket(proc.response, refmap);
end;



{ TRDTPBernBabyBernServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TRDTPBernBabyBernServerBase.create;
begin
  inherited;
  ServiceName := 'BernBabyBern';
end;

destructor TRDTPBernBabyBernServerBase.destroy;
begin

  inherited;
end;


function TRDTPBernBabyBernServerBase.Dispatch: boolean;
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
  
    //ListFiles
    $3001:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of ListFiles','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_ListFiles_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of ListFiles','RDTPCALLS');
{$ENDIF}
      end;

    //GetFile
    $3002:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetFile','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetFile_string(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetFile','RDTPCALLS');
{$ENDIF}
      end;

    //PutFile
    $3003:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of PutFile','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_PutFile_string_TStream(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of PutFile','RDTPCALLS');
{$ENDIF}
      end;

    //PutSpriteSheet
    $3004:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of PutSpriteSheet','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_PutSpriteSheet_string_TStream_TStream_TStream(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of PutSpriteSheet','RDTPCALLS');
{$ENDIF}
      end;

    //GetSpriteSheet
    $3005:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetSpriteSheet','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetSpriteSheet_string_TStream_TStream_TStream(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetSpriteSheet','RDTPCALLS');
{$ENDIF}
      end;

    //PutSpriteSheet2
    $3006:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of PutSpriteSheet2','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_PutSpriteSheet2_string_TStream_TStream_TStream_TStream(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of PutSpriteSheet2','RDTPCALLS');
{$ENDIF}
      end;

    //GetSpriteSheet2
    $3007:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetSpriteSheet2','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetSpriteSheet2_string_TStream_TStream_TStream_TStream(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetSpriteSheet2','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


