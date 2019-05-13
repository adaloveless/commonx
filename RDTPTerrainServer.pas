unit RDTPTerrainServer;
{GEN}
{TYPE SERVER}
{CLASS TTerrainServer}
{ANCESTOR TRDTPProcessor}
{IMPLIB RDTPTerrainServerImplib}
{TEMPLATE RDTP_gen_server_template.pas}
{RQFILE RDTPTerrainRQs.txt}
{END}
interface


uses
  TerrainData, PacketHelpers_TerrainData, typex, packet, systemx, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug, RDTPServerList;



type
  TTerrainServerBase = class(TRDTPProcessor)
  private
    
    procedure RQ_HANDLE_Test(proc: TRDTPProcessor);
    procedure RQ_HANDLE_GetSingleTileData_double_double(proc: TRDTPProcessor);

  protected
    

  public
    constructor Create;override;
    destructor Destroy;override;

    

    
    function RQ_Test():integer;overload;virtual;abstract;
    function RQ_GetSingleTileData(Long:double; Lat:double):TTileData;overload;virtual;abstract;


    function Dispatch: boolean;override;
  end;

procedure LocalDebug(s: string; sfilter: string = '');

implementation
uses
  RDTPTerrainServerImplib, ImpJunk;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTerrainServerBase.RQ_HANDLE_Test(proc: TRDTPProcessor);
var
  res: integer;
begin
  res := RQ_Test();
  WriteintegerToPacket(proc.response, res);
end;
//-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-xx-x-x-x-x-x-x-
procedure TTerrainServerBase.RQ_HANDLE_GetSingleTileData_double_double(proc: TRDTPProcessor);
var
  res: TTileData;
  Long:double;
  Lat:double;
begin
  GetdoubleFromPacket(proc.request, Long);
  GetdoubleFromPacket(proc.request, Lat);
  res := RQ_GetSingleTileData(Long, Lat);
  WriteTTileDataToPacket(proc.response, res);
end;



{ TTerrainServer }

procedure LocalDebug(s: string; sfilter: string = '');
begin
  Debug.Log(nil, s, sFilter);
end;

constructor TTerrainServerBase.create;
begin
  inherited;
  ServiceName := 'Terrain';
end;

destructor TTerrainServerBase.destroy;
begin

  inherited;
end;


function TTerrainServerBase.Dispatch: boolean;
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

    //GetSingleTileData
    $1112:
      begin
{$IFDEF RDTP_LOGGING}
        LocalDebug('Begin Server Handling of GetSingleTileData','RDTPCALLS');
{$ENDIF}
        result := true;//set to true BEFORE calling in case of exception
        RQ_HANDLE_GetSingleTileData_double_double(self);
{$IFDEF RDTP_LOGGING}
        LocalDebug('End Server Handling of GetSingleTileData','RDTPCALLS');
{$ENDIF}
      end;

  end;


  if not result then
    result := Inherited Dispatch;
end;




end.


