unit CCBridge;





interface

uses
  https, debug, orderlyinit, stringx, systemx, typex, sysutils, betterobject, better_collections, commandprocessor, exe, simplewinsock, better_telnet, idglobal, jsonhelpers, sharedobject, fileserviceclient;

type
  TTransactionReference = string;
  TTransactionStatus = (tsNone, tsUnconfirmed, tsConfirmed, tsFailed);

  TCCBridgeAbstract = class(TSharedObject)
  private
    FCC: string;
  protected
    FAccount: string;
    FTXFee: double;
    FLastTransaction: string;
  public

    function GetBalance: double;virtual;abstract;
    function Send(toAddr: string; amount: double; memo: string): TTransactionReference;virtual;abstract;
    function CheckTransactionStatus(sReferenceInfo: TTransactionReference): TTransactionStatus;virtual;abstract;
    procedure Init; override;
    property AccountNumber: string read FAccount write FAccount;
    property TXFee: double read FTXFee write FTXFee;
    property LastTransaction: string read FLastTransaction;
    property CC: string read FCC write FCC;
  end;

  TCMDAccount = class(TCCBridgeAbstract)
  private
    FHostname: string;
    FEndPoint: string;
  protected
    cli: TFileServiceClient;
    function RunRemoteCommand(sExe, sParams: string): string;
    procedure CheckOpen;
    procedure Close;
  public
    procedure Init; override;
    procedure Detach; override;

    property Hostname: string read FHostname write Fhostname;
    property Endpoint: string read FEndPoint write FEndpoint;


  end;

  TBTCAccount = class(TCMDAccount)
  protected
    function RunBTCCommand(sCommand: string): string;
  public

    function GetBalance:double;override;
    function Send(toAddr: string; amount: Double; memo: string): string; override;
    procedure Init; override;
    function CheckTransactionStatus(sReferenceInfo: TTransactionReference): TTransactionStatus;override;
  end;

  TWallets = class (TsharedList<TCCBridgeAbstract>)
  public

  end;



  THTTPAccount = class(TCCBridgeAbstract)
  protected
    FBaseURL: string;
    function Get(sRelativeURL: string): THTTPResults;
    function Post(sRelativeURL: string; sBody: string; sEncoding: string = 'application/json'):THTTPResults;
  public
    property BaseURL: string read FBaseURL write FBaseURL;

  end;

  TETHAccount = class(THTTPAccount)
  public
    function GetBalance:double;override;
    function Send(toAddr: string; amount: Double; memo: string): string; override;
    procedure Init; override;
    function CheckTransactionStatus(sReferenceInfo: TTransactionReference): TTransactionStatus;override;
  end;

  TTelnetAccount = class(TCCBridgeAbstract)
  private
    FPS: string;
    FHostname: string;
    FUserName: string;
    FIncoming: string;
    FInSub: boolean;
  protected
    procedure OpenConnection;
    procedure DoLogin;
    procedure SendLine(s: string);
    function GetLine: string;
    function GetLineIfHas: string;
    function HasLine: boolean;
    procedure CheckOpen;
    procedure ClearInput;
    procedure TelnetOnDataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);

  public
    conn: TIdTelnet;
    procedure Init;override;
    procedure Detach; override;

    property UserName: string read FUserName write FUserName;
    property PS: string read FPS write FPS;
    property HostName: string read FHostname write FHostName;
  end;

  TZECAccount = class(TTelnetAccount)
  public
    function GetBalance:double;override;
    function Send(toAddr: string; amount: Double; memo: string): string; override;
    procedure Init; override;
    function CheckTransactionStatus(sReferenceInfo: TTransactionReference): TTransactionStatus;override;



  end;


function BTCEXE: string;
function BTCOutputFile: string;




var
  wallets: TWallets;



implementation

{ TBTCAccount }


function TBTCAccount.CheckTransactionStatus(
  sReferenceInfo: TTransactionReference): TTransactionStatus;
var
  s: string;
  s1,s2: string;
  iConfirm: int64;
begin
  result := tsNone;
  s := RunBTCCommand('gettransaction '+sReferenceinfo);
  splitstring(s, '"confirmations"', s1,s2);
  SplitString(s2, ':', s1,s2);
  splitString(s2, ',', s1,s2);

  iConfirm := strtoint64(trim(s1));
  if iconfirm > 1 then begin
    exit(tsConfirmed);
  end;
  if iconfirm < -1 then begin
    exit(tsFailed);
  end;



end;

function TBTCAccount.GetBalance: double;
begin
  result := strtofloat(RunBTCCommand('getbalance'));
end;


procedure TBTCAccount.Init;
begin
  inherited;
  FCC := 'BTC';
  FTXFee := 0.0036;//per kilo (typically a transaction is 266 bytes)
  HostName := '192.168.101.57';
  Endpoint := '420';
end;

function TBTCAccount.RunBTCCommand(sCommand: string): string;
begin
  result := trim(RunRemoteCommand('c:\program files\bitcoin\daemon\bitcoin-cli.exe',
                              '-rpccookiefile=e:\bcdata\.cookie '+sCommand));

end;

function TBTCAccount.Send(toAddr: string; amount: Double; memo: string): string;
begin
  RunBTCCommand('settxfee '+floattostr(FTXFee));
  result := RunBTCCommand('sendtoaddress '+toAddr+' '+floattostr(amount)+' '+memo);
  FLastTransaction := result;
end;

function BTCEXE: string;
begin
  exit('c:\program files\bitcoin\daemon\bitcoin-cli.exe');
end;
function BTCOutputFile: string;
begin
  exit('e:\btc.txt');
end;



procedure oinit;
var
  wallet: TCCBridgeAbstract;
begin
  wallets := nil;

  wallets := TWallets.Create;
  wallets.ownsobjects := true;


//  wallet := TBTCAccount.Create;
//  wallet.AccountNumber := '1F1irS5PxVpnoVPSk9awsf7jbPc4gTKmhP';
//  wallets.Add(wallet);


//  wallet := TETHAccount.create;
//  wallet.AccountNumber := '0xC0Bb3E1aD07f75f07EcA7902f3057d5c32ed9FE9';
//  wallets.Add(wallet);

//  wallet := TZECAccount.Create;
//  wallet.AccountNumber := 't1WY1GdhW6sYZK1zA68RbKj8oZZx9enuJat';
//  wallets.Add(wallet);






end;

procedure ofinal;
begin
  wallets.Free;
  wallets := nil;
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;


{ TETHAccount }

function TETHAccount.CheckTransactionStatus(
  sReferenceInfo: TTransactionReference): TTransactionStatus;
begin
  raise ECritical.create('not implemented');
end;

function TETHAccount.GetBalance: double;
var
  dict: TJSON;
  res: THTTPResults;
  s: string;
begin
  //DOCUMENTATION: https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_getbalance
  res := self.Post('', '{"jsonrpc":"2.0","method":"eth_getBalance","params":["'+accountnumber+'","latest"],"id":1}');

  result := 0.0;
  dict := nil;
  try
    if res.Success then begin
      dict := JSONtoDictionary(res.Body);
      dict.TryGetValue('result',s);
      s := '$'+zcopy(s, 2, length(s)-2);

      result := strtoint64(s) / 1000000000000000000;


    end;
  finally
    dict.free;
  end;


end;

procedure TETHAccount.Init;
begin
  inherited;
  FCC := 'ETH';
  BaseURL := 'http://192.168.101.57:8545';
//  BaseURL := 'http://192.168.101.57:5985';
  AccountNumber := '0xC0Bb3E1aD07f75f07EcA7902f3057d5c32ed9FE9';
end;

function TETHAccount.Send(toAddr: string; amount: Double; memo: string): string;
begin
  raise ECritical.create('not implemented');
end;

{ TCCBridgeAbstract }


{ TCCBridgeAbstract }

procedure TCCBridgeAbstract.Init;
begin
  inherited;
end;

{ TTelnetAccount }

procedure TTelnetAccount.CheckOpen;
begin
  if conn.Connected then exit;
  OpenConnection;
end;

procedure TTelnetAccount.ClearInput;
begin
  Lock;
  FIncoming := '';
  Unlock;

end;

procedure TTelnetAccount.Detach;
begin
  if detached then exit;

  conn.free;
  conn := nil;

  inherited;

end;

procedure TTelnetAccount.DoLogin;
var
  s: ansistring;
begin
{
255 253 1    IAC DO ECHO
255 253 31   IAC DO NAWS
255 251 1    IAC WILL ECHO
255 251 3    IAC WILL SUPPRESS-GO-AHEAD
So you should respond:

255 252 1    IAC WONT ECHO
255 252 31   IAC WONT NAWS
255 254 1    IAC DONT ECHO
255 254 3    IAC DONT SUPPRESS-GO-AHEAD
}

  conn.LocalEcho := true;//tell server will echo locally so shit isn't sent back to us
  while not (FIncoming='LinuxWallets login: ') do GetLineIfHas;
  SendLine(UserName);
  while not (FIncoming='Password: ') do GetLineIfHas;
  SendLine(PS);
  SendLine('');
  GetLine;
  sleep(5000);
  ClearInput;


end;

function TTelnetAccount.GetLine: string;
begin
  while not HasLine do
    sleep(100);
  Lock;
  SplitString(FIncoming, LF, result, FIncoming);
  Unlock;
  result := trim(result);
  debug.log('TELNET-IN: '+result);

end;

function TTelnetAccount.GetLineIfHas: string;
begin
  result :='';
  if HasLine then
    result := GetLine;
end;

function TTelnetAccount.HasLine: boolean;
begin
  Lock;
  result := zpos(LF, Fincoming) >=0;
  Unlock;

end;

procedure TTelnetAccount.Init;
begin
  conn := TIDTelnet.Create(nil);

  inherited;

end;

procedure TTelnetAccount.OpenConnection;
begin
  conn.Host := HostName;
  conn.Port := 23;
  conn.ThreadedEvent := true;
  conn.OnDataAvailable := self.TelnetOnDataAvailable;
  conn.Connect;
  conn.LocalEcho := true;

  DoLogin;
end;


procedure TTelnetAccount.SendLine(s: string);
var
  ss: ansistring;
begin
  ss := ansistring(s+LF);
  conn.SendString(ss);
  GetLine;//skip echo of line

end;

procedure TTelnetAccount.TelnetOnDataAvailable(Sender: TIdTelnet;
  const Buffer: TIdBytes);
var
  t: ni;
  got: ni;
begin

  got := length(buffer);
  t := 0;
  while t < length(buffer) do begin
    Lock;
    FIncoming := FIncoming + char(ansichar(buffer[t]));
    Unlock;
    inc(t);
  end;

end;

{ TZECAccount }

function TZECAccount.CheckTransactionStatus(
  sReferenceInfo: TTransactionReference): TTransactionStatus;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TZECAccount.GetBalance: double;
var
  s: string;
begin
  Self.CheckOpen;
  ClearInput;
  SendLine('zcash-cli getbalance');
  s := GetLine;
  result := strtofloat(s);

end;

procedure TZECAccount.Init;
begin
  inherited;
  CC := 'ZEC';
  HostName := '192.168.101.55';
  UserName := 'jason';
  PS := '123qawsed!@#QAWSED';
end;

function TZECAccount.Send(toAddr: string; amount: Double; memo: string): string;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

{ THTTPAccount }

function THTTPAccount.Get(sRelativeURL: string): THTTPResults;
begin
  result := HTTPSGet(Self.BaseURL + sRelativeURL);
end;

function THTTPAccount.Post(sRelativeURL, sBody,
  sEncoding: string): THTTPResults;
begin
  result := HTTPSPost(Self.BaseURL + sRelativeURL, sBody, sEncoding);
end;

{ TCMDAccount }

procedure TCMDAccount.CheckOpen;
begin
  if (cli = nil) or (not cli.Connected) then begin
    if cli<>nil then
      close;
    cli := TFileServiceClient.create(hostname, endpoint);
  end;

end;

procedure TCMDAccount.Close;
begin
  if cli <> nil then
    cli.free;

  cli := nil;
end;

procedure TCMDAccount.Detach;
begin
  if detached then
    exit;

  cli.free;
  cli := nil;

  inherited;

end;

procedure TCMDAccount.Init;
begin
  inherited;

end;

function TCMDAccount.RunRemoteCommand(sExe, sParams: string): string;
begin
  CheckOpen;
  result := cli.ExecuteAndCapture('', sExe, sParams);
end;

initialization
  init.RegisterProcs('CCBridge', oinit, ofinal, 'CommandProcessor,SimpleReliableUDP');

end.
