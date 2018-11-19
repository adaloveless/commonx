unit RDTPMultiplexerServer;
interface


uses
  tickcount, stringx, classes, packet, betterobject, genericRDTPClient, sysutils, windows, variants, rdtpprocessor, packethelpers, debug,  RDTPServerList;



type


  TRDTPMultiplexerProcessor = class(TRDTPProcessor)
  protected
    FServices: TStringlist;
    function IndexOfService(sService: ansistring): integer;
    function HasService(sService: ansistring): boolean;
    procedure AddService(sService: ansistring; p: TRDTPProcessor);
    procedure ClearServices;
  public
    procedure DoIdle;override;
    constructor Create;override;
    destructor Destroy;override;

    function Dispatch: boolean;override;

    function NeedService(sService: ansistring): TRDTPProcessor;
  end;

implementation

uses helpers.list;

procedure TRDTPMultiplexerProcessor.AddService(sService: ansistring; p: TRDTPProcessor);
begin
  FServices.addobject(lowercase(sService), p);

end;

procedure TRDTPMultiplexerProcessor.ClearServices;
var
  t: integer;
begin
  self.Lock;
  try
    for t:= 0 to FServices.Count-1 do begin
      RDTPServers.NoNeedRDTPProcessor(FServices[t]);
    end;
  finally
    self.Unlock;
  end;

end;

constructor TRDTPMultiplexerProcessor.create;
begin
  inherited;


  FServices := TStringlist.create;
end;

destructor TRDTPMultiplexerProcessor.destroy;
begin
  ClearServices;
  FreeStringListcontents(FServices);
  FServices.free;
  FServices := nil;
  inherited;
end;


function TRDTPMultiplexerProcessor.Dispatch: boolean;
var
  sService: string;
  procClass: TRDTPProcessorClass;
  proc: TRDTPProcessor;
begin

  result := false;


  //request.seqseek(2);
  sService := request.Data[2];
  request.seqseek(0);

  if sService = '' then begin
    result := inherited Dispatch;
    exit;
  end;


{$IFDEF RDTP_LOGGING}  Debug.Log(self,'Requesting service: '+sService);{$ENDIF}
  proc := NeedService(sService);
  try
    proc.Resources := self.Resources;
    proc.socket := self.socket;
    proc.Request := self.request;
    proc.response := self.Response;

    try
//      SayNatural('Request');
      result := proc.MasterDispatch;
    except
      on e: Exception do begin
        Debug.Log(self,'Exception unhandled in MasterDispatch:'+e.message);
        raise;
      end;
    end;
    ForgetResult := proc.ForgetResult;
  finally
    proc.request := nil;
    proc.response := nil;
    proc.socket := nil;

//    proc.free;
  end;
end;



function TRDTPMultiplexerProcessor.HasService(sService: ansistring): boolean;
begin
  result := IndexOfService(sService) >= 0;
end;

function TRDTPMultiplexerProcessor.IndexOfService(sService: ansistring): integer;
begin
  result := FServices.IndexOf(lowercase(sService));

end;


function TRDTPMultiplexerProcessor.NeedService(
  sService: ansistring): TRDTPProcessor;
var
  i: integer;
  c: TRDTPProcessorClass;
  p: TRDTPProcessor;
begin
  //check if we already have the service
  i := IndexOfService(sService);
  if i < 0 then begin
    c := RDTPServers.NEedRDTPProcessor(sService);
    p := c.Create;
    p.PlatformOptions := self.PlatformOptions;
    AddService(sService, p);
//    p.Context := self.Context;
  end else begin
    p := TRDTPPRocessor(FServices.objects[i]);
    p.PlatformOptions := self.PlatformOptions;
  end;

  result := p;


end;

procedure TRDTPMultiplexerProcessor.DoIdle;
var
  s: string;
begin
  inherited;

  if RDTPServers.Upgrading then begin
    ClearServices;
  end;

  while RDTPServers.Upgrading do begin
    sleep(100);
  end;
end;

end.


