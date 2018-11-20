unit dosvconfig;

interface
uses registry, Transport, sysutils,
  windows;

type
  EDOSVConfigurationError = class(Exception);

  TDOSVConfiguration = class
  private
    FContext: ansistring;
    procedure WriteConfigString(keyname, keyValue: widestring);
    function ReadConfigString(keyname: widestring): widestring;
    function GetBandWidthLimit: integer;
    procedure SetBandWidthLimit(const Value: integer);
  protected
//    reg: TRegistry;
    FRegistryKey: widestring;
    function GetHost: widestring;
    function GetEndpoint: widestring;
    function GetNetworkInterface: TNetworkInterface;
    procedure SetHost(sHostName: widestring);
    procedure SetEndPoint(sEndPoint: widestring);
    procedure SetNetworkInterface(ni: TnetworkInterface);
    procedure RaiseError;
  public
    constructor Create(sRegistryKey: widestring);
    destructor Destroy; override;
    property NetworkInterface: TNetworkInterface
      read GetNetworkInterface
      write SetNetworkInterface;
    property HostName: widestring read GetHost write SetHost;
    property EndPoint: widestring read GetEndpoint write SetEndPoint;
    property BandWidthLimit: integer read GetBandWidthLimit write SetBandWidthLimit;
    property Context: ansistring read FContext write FContext;
  end;

procedure LogConfigMessage(sMessage: widestring);


implementation

uses Inifiles, systemx, stringx;

procedure LogConfigMEssage(sMessage: widestring);
begin
  {$IFDEF COMPILE_DOSV_IN_EXE}
  {$ENDIF}
end;


//------------------------------------------------------------------------------
constructor TDOSVConfiguration.Create(sRegistryKey: widestring);
begin
  inherited Create;
//  reg := TRegistry.create;
//  reg.RootKey := HKEY_LOCAL_MACHINE;
//  reg.OpenKey(sRegistryKey, false);
  FRegistryKey:=sRegistryKey;


end;
//------------------------------------------------------------------------------
destructor TDOSVConfiguration.Destroy;
begin
//  reg.free;
  inherited;

end;

//------------------------------------------------------------------------------
function TDOSVConfiguration.GetHost: widestring;
begin
  result := ReadConfigString('HostName');
end;
//------------------------------------------------------------------------------
function TDOSVConfiguration.GetEndPoint: widestring;
begin
  result := ReadConfigString('EndPoint');
end;
//------------------------------------------------------------------------------
function TDOSVConfiguration.GetNetworkInterface: TNetworkInterface;
begin
  result := TNetworkInterface(strtoint(ReadConfigString('NetworkInterface')));
end;
//------------------------------------------------------------------------------
procedure TDOSVConfiguration.SetHost(sHostName: widestring);
begin
  WriteConfigString('HostName', sHostName);
end;
//------------------------------------------------------------------------------
procedure TDOSVConfiguration.SetEndPoint(sEndPoint: widestring);
begin
  WriteConfigString('EndPoint', sEndPoint);
end;
//------------------------------------------------------------------------------
procedure TDOSVConfiguration.SetNetworkInterface(ni: TnetworkInterface);
begin
  WriteConfigString('NetworkInterface', inttostr(integer(ni)));
end;
//------------------------------------------------------------------------------
procedure TDOSVConfiguration.RaiseError;
begin
  raise EDOSVConfigurationError.CreateFmt('Your registry settings are invalid, '+
    'please rerun the Pathways Workstation Configuration Utility',[])
end;

//------------------------------------------------------------------------------
procedure TDOSVConfiguration.WriteConfigString(keyname, keyvalue: widestring);
var
  ini: TIniFile;
  sTemp: Array[0..255] of AnsiChar;
begin
(*  if not reg.OpenKey(FRegistryKey, true) then begin
    RaiseError;
  end else begin
    reg.WriteString(keyname, keyvalue);
    reg.CloseKey;
  end;
*)
  if (GetWindowsDirectory(@sTemp[0], 255) > 255) then begin
    RaiseError;
  end
  else begin
    ini := TIniFile.Create(slash(sTemp) + 'webconfig.ini');
    try
      ini.WriteString(FRegistryKey, keyname, keyvalue);
    finally
      ini.Free;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TDOSVConfiguration.ReadConfigString(keyname: widestring): widestring;
var
  ini: TIniFile;
  sTemp: array[0..255] of AnsiChar;
begin
(*  if not reg.OpenKey(FRegistryKey, false) then begin
    LogConfigMessage('Could not open registry key: '+FRegistryKey);
    result := '';
  end else begin
    result := reg.ReadString(keyname);
    reg.CloseKey;
  end;
*)

  if (GetWindowsDirectory(@sTemp[0], 255) > 255) then begin
    LogConfigMessage(sTemp + ' is much too long for a windows directory!');
    result := '';
  end
  else begin
    ini := TIniFile.Create(slash(sTemp) + 'webconfig.ini');
    try
      result := ini.ReadString(FRegistryKey, keyname, '');
    finally
      ini.free;
    end;

  end;

end;
//------------------------------------------------------------------------------
function TDOSVConfiguration.GetBandWidthLimit: integer;
var
  s: ansistring;
begin
  try
    s:= ReadConfigString('BandWidthLimit');
    if s = '' then
      result:=0
    else
      result := strtoint(s);

  except
    raise Exception.create('Error reading BandWidthLimit registry key');
  end;
end;
//------------------------------------------------------------------------------
procedure TDOSVConfiguration.SetBandWidthLimit(const Value: integer);
begin
 WriteConfigString('BandWidthLimit', inttostr(value));

end;

end.
