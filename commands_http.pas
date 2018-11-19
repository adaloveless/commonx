unit commands_http;

interface

uses
  commandprocessor, httpclient, sysutils, tickcount;

type
  Tcmd_HTTPDownload = class(TCommand)
  private
    FURL: string;
    FClient: THTTPCLient;
    FTimeout: ticker;
    function GetTimeout: ticker;
    procedure SetTimeout(const Value: ticker);
  public
    constructor Create;override;
    destructor Destroy;override;

    property URL: string read FURL write FURL;
    property Client: THTTPCLient read FClient;

    procedure DoExecute;override;
    procedure OnHTTPProgress(pos, max: int64);
    property Timeout: ticker read GetTimeout write SetTimeout;
  end;

  Tcmd_HTTPDownLoadToFile = class(Tcmd_HTTPDownload)
  private
    FFile: string;
  public
    IgnoreIfTargetExists: boolean;
    procedure InitExpense;override;
    procedure DoExecute;override;
    property LocalFile: string read FFile write FFile;
  end;


implementation


{ Tcmd_HTTPDownload }

constructor Tcmd_HTTPDownload.Create;
begin
  inherited;
  FClient := THTTPCLient.create;
  FTimeout := 300000;
end;

destructor Tcmd_HTTPDownload.Destroy;
begin
  FClient.free;
  inherited;
end;

procedure Tcmd_HTTPDownload.DoExecute;
begin
  inherited;
  FClient.OnProgress := self.OnHTTPProgress;
  FClient.TimeOut := Timeout;
  FClient.Get(URL, '');
//  beeper.beep(1000,100);
end;

function Tcmd_HTTPDownload.GetTimeout: ticker;
begin
  result := FTimeout;
end;

procedure Tcmd_HTTPDownload.OnHTTPProgress(pos, max: int64);
begin
  self.Step := pos;
  self.StepCount := max;
  self.NotifyProgress;
end;

procedure Tcmd_HTTPDownload.SetTimeout(const Value: ticker);
begin
  FTimeout := value;
end;

{ THTTPDownLoadToFileCommand }

procedure Tcmd_HTTPDownLoadToFile.DoExecute;
begin
  if not FileExists(localfile) then begin
    inherited;
    Client.SaveToFile(LocalFile);
  end;

end;


procedure Tcmd_HTTPDownLoadToFile.InitExpense;
begin
//  inherited;
  NetworkExpense := 1/8;
end;

end.
