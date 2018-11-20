unit wificonnection;

interface

uses
  systemx, stringx, typex, commandprocessor, exe, tools;


type
  Tcmd_ConnectToSSID = class(TCommand)
  private
    Fssid: string;
  public
    procedure InitExpense;override;
    procedure DoExecute;override;
    property ssid: string read Fssid write FSsid;
  end;


procedure ConnectToSSID(ssid: string);


implementation

{ Tcmd_ConnectToSSID }

procedure Tcmd_ConnectToSSID.DoExecute;
begin
  inherited;
  ConnectToSSID(ssid);


end;

procedure ConnectToSSID(ssid: string);
var
  stool: string;
begin
  stool := tools.FindTool('WindowsAutomaticNetworkConnection.exe');
  exe.RunProgramAndWait(sTool, ssid, '', true, false);
end;


procedure Tcmd_ConnectToSSID.InitExpense;
begin
  inherited;
  CPUExpense := 0;
end;

end.
