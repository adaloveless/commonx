unit BLEX;

interface

uses
  typex, stringx, systemx, sharedobject, commandprocessor, system.bluetooth,
{$DEFINE SYNC}
{$IFDEF SYNC}
  system.bluetooth.components,
{$ELSE}
  Bluetoothcomponentthreaded,
{$ENDIF}
  classes, betterobject, sysutils, debug;

type
  TBLEEnumerator = class; // forward

  TBLECharacteristicEvent = procedure(const ACharacteristic
    : TBluetoothGattCharacteristic) of object;
  TBLEDeviceEvent = procedure(const ADevice: TBluetoothLEDevice) of object;


{$IFDEF SYNC}
  TLocalBLE = TBluetoothLE;
{$ELSE}
  TLocalBLE = TBluetoothLEThreaded;
{$ENDIF}



  TBLEEnumerator = class(TBetterObject) // USE THREAD LOCAL
  private
    ble: TLocalBLE;
    FScanTime: ni;
    FOnRead: TBLECharacteristicEvent;
    FChooseDevice: string;
    FOnConnected: TBLEDeviceEvent;
    FOnDisconnected: TBLEDeviceEvent;
    FChooseService: TBluetoothUUID;
    FChooseCharacteristic: TBluetoothUUID;
    FConnected: boolean;
    FDiscovering: boolean;
    FFound: ni;
    procedure bleEndDiscoverDevices(const Sender: TObject;
      const ADeviceList: TBluetoothLEDeviceList);
    procedure bleEndDiscoverServices(const Sender: TObject;
      const AServiceList: TBluetoothGattServiceList);
    procedure bleCharacteristicRead(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    procedure bleCharacteristicReadRequest(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic;
      var AGattStatus: TBluetoothGattStatus);
    procedure bleCharacteristicWrite(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    procedure bleCharacteristicWriteRequest(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic;
      var AGattStatus: TBluetoothGattStatus; const AValue: TArray<system.Byte>);
    procedure bleReliableWriteCompleted(const Sender: TObject;
      AGattStatus: TBluetoothGattStatus);
    procedure bleConnectedDevice(const Sender: TObject;
      const ADevice: TBluetoothLEDevice);
    procedure bleDescriptorRead(const Sender: TObject;
      const ADescriptor: TBluetoothGattDescriptor;
      AGattStatus: TBluetoothGattStatus);
    procedure bleDescriptorWrite(const Sender: TObject;
      const ADescriptor: TBluetoothGattDescriptor;
      AGattStatus: TBluetoothGattStatus);
    procedure bleDisconnectDevice(const Sender: TObject;
      const ADevice: TBluetoothLEDevice);
    procedure bleDiscoverLEDevice(const Sender: TObject;
      const ADevice: TBluetoothLEDevice; Rssi: Integer;
      const ScanResponse: TScanResponse);
    procedure bleReadRSSI(const Sender: TObject; ARssiValue: Integer;
      AGattStatus: TBluetoothGattStatus);
    procedure bleServiceAdded(const Sender: TObject;
      const AService: TBluetoothGattService;
      const AGattStatus: TBluetoothGattStatus);
    procedure bleServicesDiscovered(const Sender: TObject;
      const AServiceList: TBluetoothGattServiceList);
    procedure tmPollTimer(Sender: TObject);
    procedure bleCharacteristicSubscribed(const Sender: TObject;
      const AClientId: string;
      const ACharacteristic: TBluetoothGattCharacteristic);

    procedure GetServiceAndCharacteristics;
    function GetDevices: TBluetoothLEDeviceList;
    { Private declarations }
  public
    FBLEDevice: TBluetoothLEDevice;
    FBLEGattService: TBluetoothGattService;
    FBLEGattCharacteristic: TBluetoothGattCharacteristic;
    FACtiveChar: TBluetoothGattCharacteristic;

    { Public declarations }
    chr: TBluetoothGattCharacteristic;
    dev: TBluetoothLEDevice;
    devs: TBluetoothLEDeviceList;
    srvs: TBluetoothGattServiceList;
    svc: TBluetoothGattService;

    procedure Init; override;
    procedure Detach; override;
    property Devices: TBluetoothLEDeviceList read GetDevices;
    // invalid until scan is done

    property ScanTime: ni read FScanTime write FScanTime;

    property OnRead: TBLECharacteristicEvent read FOnRead write FOnRead;
    property OnConnected: TBLEDeviceEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TBLEDeviceEvent read FOnDisconnected
      write FOnDisconnected;

    function getDeviceByName(sName: string): TBluetoothLEDevice;

    property ChooseDevice: string read FChooseDevice write FChooseDevice;
    property ChooseService: TBluetoothUUID read FChooseService
      write FChooseService;
    property ChooseCharacteristic: TBluetoothUUID read FChooseCharacteristic
      write FChooseCharacteristic;
    procedure Connect_Sync;
    procedure Connect_Async;
    property Connected: boolean read FConnected write FConnected;
    property Discovering: boolean read FDiscovering;
    property Found: ni read FFound;
    procedure WriteToSubScribedCharacteristic(bs: TArray<byte>);
  end;

  Tcmd_BLEDiscoverAndConnect = class(TCommand)
  private
  protected
    procedure DoExecute;override;
  public
    enum: TBleEnumerator;
  end;


implementation

{ TBLEEnumerator }

procedure TBLEEnumerator.bleCharacteristicRead(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
var
  byts: TArray<byte>;
begin
  byts := ACharacteristic.GetValue;
  if length(byts) = 0 then
    exit;

{$IFDEF DEBUGBLEX}
  Log(self, 'Read Characteristic: ' + MemoryToString(@byts[0],
    length(byts)));
{$ENDIF}
  if assigned(FOnRead) then
    FOnRead(ACharacteristic);
end;

procedure TBLEEnumerator.bleCharacteristicReadRequest(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  var AGattStatus: TBluetoothGattStatus);
begin
  Log('Read Request!');
end;

procedure TBLEEnumerator.bleCharacteristicSubscribed(const Sender: TObject;
  const AClientId: string; const ACharacteristic: TBluetoothGattCharacteristic);
begin
  Log('Subscribed!');
  chr := ACharacteristic;

end;

procedure TBLEEnumerator.bleCharacteristicWrite(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
begin
  Log('Write!');
end;

procedure TBLEEnumerator.bleCharacteristicWriteRequest(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  var AGattStatus: TBluetoothGattStatus; const AValue: TArray<system.Byte>);
begin
  Log('Write Request!');
end;

procedure TBLEEnumerator.bleConnectedDevice(const Sender: TObject;
  const ADevice: TBluetoothLEDevice);
begin
  Log('Device Connected!');
  FConnected := true;
  if assigned(FOnConnected) then
    FOnConnected(ADevice);
end;

procedure TBLEEnumerator.bleDescriptorRead(const Sender: TObject;
  const ADescriptor: TBluetoothGattDescriptor;
  AGattStatus: TBluetoothGattStatus);
begin
  Log('Descriptor Read!');
end;

procedure TBLEEnumerator.bleDescriptorWrite(const Sender: TObject;
  const ADescriptor: TBluetoothGattDescriptor;
  AGattStatus: TBluetoothGattStatus);
var
  b: TBytes;
begin
  setlength(b, 2);
  b[0] := 1;
  b[1] := 0;
  ADescriptor.SetValue(b);
  Log('Descriptor Write!');
end;

procedure TBLEEnumerator.bleDisconnectDevice(const Sender: TObject;
  const ADevice: TBluetoothLEDevice);
begin
  Log('Device Disconnected!');
  FConnected := false;
  if assigned(FOnDisconnected) then
    FOnDisconnected(ADevice);
end;

procedure TBLEEnumerator.bleDiscoverLEDevice(const Sender: TObject;
  const ADevice: TBluetoothLEDevice; Rssi: Integer;
  const ScanResponse: TScanResponse);
begin
  Log('Discover LE Device! '+Adevice.Address);
  // lb.Items.Add(adevice.Identifier);
end;

procedure TBLEEnumerator.bleEndDiscoverDevices(const Sender: TObject;
  const ADeviceList: TBluetoothLEDeviceList);
var
  t: ni;
  s1: string;
begin
  devs := ADeviceList;
  if devs = nil then begin
    FDiscovering := false;
    exit;
  end;

  FFound := devs.count;
  Log('Discovered ' + inttostr(devs.count) + ' Bluetooth devices.');

  for t := 0 to devs.count - 1 do
  begin
    s1 := stringreplace(devs[t].Identifier, ':', '', [rfReplaceall]);
    Debug.Log('Device: '+s1);
//    if comparetext(s1, sName) = 0 then
//      exit(Devices[t]);
  end;

  if ChooseDevice <> '' then
  begin
    dev := getDeviceByName(ChooseDevice);
    if dev <> nil then
    begin
      ble.DiscoverServices(dev);
      GetServiceAndCharacteristics;
    end
    else
      FDiscovering := false;

  end;

end;

procedure TBLEEnumerator.bleEndDiscoverServices(const Sender: TObject;
  const AServiceList: TBluetoothGattServiceList);
var
  t: ni;
begin
  //
  Log('End discover services');
  srvs := AServiceList;
  svc := nil;

  FDiscovering := AServiceList.count > 0;

  for t := 0 to AServiceList.count - 1 do
  begin
    if (AServiceList.Items[t].UUID = ChooseService) then
    begin
      svc := AServiceList.Items[t];
      GetServiceAndCharacteristics;
      break;
    end;
  end;

end;

procedure TBLEEnumerator.bleReadRSSI(const Sender: TObject; ARssiValue: Integer;
  AGattStatus: TBluetoothGattStatus);
begin
  Log('Read RSSI');
end;

procedure TBLEEnumerator.bleReliableWriteCompleted(const Sender: TObject;
  AGattStatus: TBluetoothGattStatus);
begin
  Log('Reliable Write Complete!');
end;

procedure TBLEEnumerator.bleServiceAdded(const Sender: TObject;
  const AService: TBluetoothGattService;
  const AGattStatus: TBluetoothGattStatus);
begin
  Log('Service Added!');
end;

procedure TBLEEnumerator.bleServicesDiscovered(const Sender: TObject;
  const AServiceList: TBluetoothGattServiceList);
begin
  Log('Services Discovered!');
end;

procedure TBLEEnumerator.Connect_Async;
var
  cmd: Tcmd_BLEDiscoverAndConnect;
begin
  cmd := Tcmd_BLEDiscoverAndConnect.create;
  try
    cmd.enum := self;
    cmd.start;
    cmd.WaitFor;
  finally
    cmd.free;
  end;

end;

procedure TBLEEnumerator.Connect_Sync;
begin
  Log(self, 'Starting Connection');
  ble.Enabled := true;
  //sleep(4000);
  FDiscovering := true;
  ble.DiscoverDevices(16000);


end;

procedure TBLEEnumerator.Detach;
begin
  if Detached then
    exit;

  ble.free;
  ble := nil;
  inherited;

end;

function TBLEEnumerator.getDeviceByName(sName: string): TBluetoothLEDevice;
var
  t: ni;
  s1: string;
begin
  result := nil;

  if Devices = nil then
    exit;

  for t := 0 to Devices.count - 1 do
  begin
    s1 := stringreplace(Devices[t].Identifier, ':', '', [rfReplaceall]);
    if comparetext(s1, sName) = 0 then
      exit(Devices[t]);
  end;

end;

procedure TBLEEnumerator.tmPollTimer(Sender: TObject);
var
  b: TBytes;
begin
  // exit;
  //
  if FACtiveChar <> nil then
  begin
    if ble.ReadCharacteristic(dev, chr) then
    begin
      b := FACtiveChar.VALUE;

      Log('read success ' + MemoryToString(@b[0], length(b)));
    end;

  end;
end;

procedure TBLEEnumerator.WriteToSubScribedCharacteristic(bs: TArray<byte>);
var
  //member ble is an instance of TBluetoothLE
  chr: TBluetoothGattCharacteristic;
begin

  chr := ble.GetCharacteristic(FBLEGattService, ChooseCharacteristic);
  chr.SetValue(bs);
  ble.WriteCharacteristic(self.dev, chr);
end;

function TBLEEnumerator.GetDevices: TBluetoothLEDeviceList;
begin
  result := devs;

end;

procedure TBLEEnumerator.GetServiceAndCharacteristics;
var
  bgc: TBluetoothGattCharacteristic;

begin
  if Fconnected then
    exit;

  try
    FBLEGattService := nil;
    FBLEGattCharacteristic := nil;
    // get Weight Service by UUID
    FBLEGattService := ble.GetService(dev, ChooseService);

    if FBLEGattService <> nil then
    begin
      Log('Service: ' + FBLEGattService.UUID.ToString);
      // get Weight Characteristic
      Log('Looking Char: ' + ChooseCharacteristic.ToString);

      // GET A LIST OF CHARACTERISTICS
      ble.GetCharacteristics(FBLEGattService);

      // get the characteristic object from the list
      FBLEGattCharacteristic := ble.GetCharacteristic(FBLEGattService, ChooseCharacteristic);

      Log('Char: ' + FBLEGattCharacteristic.UUID.ToString);
      // (FBleGattCharacteristic as TAndroidBluetoothGattCharacteristic).hack;
      // subscribe to the  BLE_MIDI service
      ble.SubscribeToCharacteristic(dev, FBLEGattCharacteristic);
      // HACK IT
      // TAndroidBluetoothGattCharacteristic(FBLeGattCharacteristic).Hack;
      Log('Subscribed to Characteristic');
      chr := FBLEGattCharacteristic;


      FConnected := true;

    end
    else
    begin
      Log('service not found');
    end;
  finally
    FDiscovering := false;
  end;

end;

procedure TBLEEnumerator.Init;
begin
  inherited;

  ble := TLocalBLE.create(nil);
  ble.OnCharacteristicRead := self.bleCharacteristicRead;
  ble.OnEndDiscoverDevices := self.bleEndDiscoverDevices;
  ble.OnConnectedDevice := self.bleConnectedDevice;
  ble.OnDisconnectDevice := self.bleDisconnectDevice;
  ble.OnServicesDiscovered := self.bleServicesDiscovered;
  ble.OnDiscoverLEDevice := self.bleDiscoverLEDevice;
  ble.OnEndDiscoverServices := self.bleEndDiscoverServices;
  ble.OnCharacteristicSubscribed := self.bleCharacteristicSubscribed;

end;

{ Tcmd_BLEDiscoverAndConnect }

procedure Tcmd_BLEDiscoverAndConnect.DoExecute;
begin
  inherited;
  enum.Connect_Sync;


end;

end.
