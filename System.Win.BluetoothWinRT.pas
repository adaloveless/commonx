{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Win.BluetoothWinRT;

interface

uses
  System.SysUtils, System.Bluetooth;

{$SCOPEDENUMS ON}

type

  TPlatformWinRTBluetoothLEManager = class(TBluetoothLEManager)
  public
    class function GetBluetoothManager: TBluetoothLEManager; override;
  end;

implementation

uses
  Winapi.Windows, System.Types, System.Generics.Collections, System.SyncObjs, System.StrUtils, System.Classes,
  WinApi.Bluetooth, WinApi.BluetoothLE, System.Win.Winrt, Winapi.winrt, Winapi.Foundation, Winapi.Foundation.Types,
  Winapi.Foundation.Collections, Winapi.Devices.Enumeration, Winapi.Devices, Winapi.Devices.Bluetooth,
  Winapi.Devices.Bluetooth.Advertisement, Winapi.CommonTypes, Winapi.Storage.Streams, System.NetConsts, System.RTLConsts;

const
  SBluetoothMACAddressFormat = '%0.2X:%0.2X:%0.2X:%0.2X:%0.2X:%0.2X'; // Do not translate

type

  TAsyncOperation<T: IInterface> = class
  private type
    TCancelProcedure = reference to procedure;
    TThreadTimer = class(TThread)
    private
      FTimeout: Cardinal;
      FEvent: TEvent;
      FOnTimer: TCancelProcedure;
      procedure Cancel;
    public
      constructor Create(const ACancelProc: TCancelProcedure; Timeout: Cardinal); overload;
      destructor Destroy; override;
      procedure Execute; override;
    end;
    class function Wait(const AAsyncOp: T; var AsyncOpResult: T; ATimeout: Cardinal = INFINITE): AsyncStatus;
  end;

  TWinRTBluetoothLEManager = class(TPlatformWinRTBluetoothLEManager)
  private
    FLEAdapter: TBluetoothLEAdapter;
  protected
    function GetRadioAdapter: Boolean;
    function GetConnectionState: TBluetoothConnectionState; override;
    function DoGetAdapter: TBluetoothLEAdapter; override;
    // LE Fucntionality
    function DoGetGattServer: TBluetoothGattServer; override;
    function DoGetSupportsGattClient: Boolean; override;
    function DoGetSupportsGattServer: Boolean; override;
    function DoEnableBluetooth: Boolean; override;
  public
    destructor Destroy; override;
  end;

  TWinRTBluetoothLEAdapter = class;

  TBLEAdvertisementReceivedEventHandler = class(TInspectableObject,
    TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs_Delegate_Base,
    TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs)
 private
    [Weak] FAdapter: TWinRTBluetoothLEAdapter;
    procedure Invoke(sender: IBluetoothLEAdvertisementWatcher; args: IBluetoothLEAdvertisementReceivedEventArgs); safecall;
  public
    constructor Create(const AAdapter: TWinRTBluetoothLEAdapter);
  end;

  TWinRTBluetoothLEAdapter = class(TBluetoothLEAdapter)
  private type
    TStopDiscoveryProc = procedure of object;
    TDiscoverThreadTimer = class(TThread)
    private
      [Weak] FAdapter: TBluetoothLEAdapter;
      FTimeout: Cardinal;
      FOnTimer: TStopDiscoveryProc;
      FEvent: TEvent;
      procedure Cancel;
    public
      constructor Create(const AnAdapter: TBluetoothLEAdapter; const AStopDiscoveryProc: TStopDiscoveryProc; Timeout: Cardinal); overload;
      destructor Destroy; override;
      procedure Execute; override;
    end;
  private
    FAdvertisementWatcher: IBluetoothLEAdvertisementWatcher;
    FBLEAdvertisementReceivedDelegate: TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs;
    FBLEAdvertisementReceivedDelegateERT: EventRegistrationToken;
    FScannedLEDevices: TBluetoothLEDeviceDictionary;
    FTimerThread: TDiscoverThreadTimer;
    FBluetoothLEScanFilterList: TBluetoothLEScanFilterList;
    procedure GetBLEPairedDevices;
  protected
    FRadioInfo: TBluetoothRadioInfo;
    function GetAdapterName: string; override;
    procedure SetAdapterName(const Value: string); override;
    function GetAddress: System.Bluetooth.TBluetoothMacAddress; override;
    function DoStartDiscovery(Timeout: Cardinal; const AFilterUUIDList: TBluetoothUUIDsList = nil;
      const ABluetoothLEScanFilterList: TBluetoothLEScanFilterList = nil): Boolean; override;
    function DoStartDiscoveryRaw(const ABluetoothLEScanFilterList: TBluetoothLEScanFilterList = nil; Refresh: Boolean = True): Boolean; override;
    procedure DoCancelDiscovery; override;
    procedure DoDiscoveryEnd(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList); override;
    function GetScanMode: TBluetoothScanMode; override;
    function GetState: TBluetoothAdapterState; override;
    procedure BLEAdvertisementReceived(const AAdvertisement: IBluetoothLEAdvertisement;
      const AdvertisementType: BluetoothLEAdvertisementType; AAddress: UInt64; ARSSI: SmallInt; const ATimestamp: DateTime);
    procedure StopDiscovery;
    procedure DoDeviceDiscovered(const ADevice: System.Bluetooth.TBluetoothLEDevice; ANewDevice: Boolean;
      const ABluetoothLEScanFilterList: TBluetoothLEScanFilterList); override;
  public
    constructor Create(const AManager: TBluetoothLEManager; const ARadioInfo: TBluetoothRadioInfo);
    destructor Destroy; override;
  end;

  TWinRTBluetoothLEAdvertiseData = class(TBluetoothLEAdvertiseData)
  private
    FDevice: System.Bluetooth.TBluetoothLEDevice;
  protected
    function DoAddServiceUUID(const AServiceUUID: TBluetoothUUID): Boolean; override;
    procedure DoRemoveServiceUUID(const AServiceUUID: TBluetoothUUID); override;
    procedure DoClearServiceUUIDs; override;
    function DoAddServiceData(const AServiceUUID: TBluetoothUUID; const AData: TBytes): Boolean; override;
    procedure DoRemoveServiceData(const AServiceUUID: TBluetoothUUID); override;
    procedure DoClearServiceData; override;
    function ContainsServiceUUID(const AServiceUUID: TBluetoothUUID): Boolean; override;
    function GetServiceUUIDs: TArray<TBluetoothUUID>; override;
    function GetServiceData: TArray<TServiceDataRawData>; override;
    function GetDataForService(const AServiceUUID: TBluetoothUUID): TBytes; override;
    procedure SetLocalName(const ALocalName: string); override;
    function GetLocalName: string; override;
    procedure SetTxPowerLevel(ATxPowerLevel: Integer); override;
    function GetTxPowerLevel: Integer; override;
    procedure SetManufacturerSpecificData(const AManufacturerSpecificData: TBytes); override;
    function GetManufacturerSpecificData: TBytes; override;
  public
    constructor Create(const ADevice: System.Bluetooth.TBluetoothLEDevice);
  end;

  TWinRTBluetoothLEDevice = class;

  TConnectionStatusChangeEventHandler = class(TInspectableObject,
    TypedEventHandler_2__IBluetoothLEDevice__IInspectable_Delegate_Base,
    TypedEventHandler_2__IBluetoothLEDevice__IInspectable)
  private
    [Weak] FDevice: TWinRTBluetoothLEDevice;
    procedure Invoke(sender: IBluetoothLEDevice; args: IInspectable); safecall;
  public
    constructor Create(const ADevice: TWinRTBluetoothLEDevice);
  end;

  TWinRTBluetoothLEDevice = class(System.Bluetooth.TBluetoothLEDevice)
  private
    FClosed: Boolean;
    FLEAdapter: TWinRTBluetoothLEAdapter;
    FConnectionStatusChangeDelegate: TypedEventHandler_2__IBluetoothLEDevice__IInspectable;
    procedure CheckInitialized;
    procedure CheckNotClosed;
  protected
    FId: HSTRING;
    FAddress: UInt64;
    FBluetoothLEDevice: IBluetoothLEDevice;
    FDeviceName: string;
    FReliableWriteTransaction: GenericAttributeProfile_IGattReliableWriteTransaction;
    function DoCreateAdvertiseData: TBluetoothLEAdvertiseData; override;
    function GetAddress: TBluetoothMacAddress; override;
    function GetDeviceName: string; override;
    function GetBluetoothType: TBluetoothType; override;
    function GetIdentifier: string; override;
    function GetIsConnected: Boolean; override;
    procedure DoAbortReliableWrite; override;
    function DoBeginReliableWrite: Boolean; override;
    function DoExecuteReliableWrite: Boolean; override;
    function DoDiscoverServices: Boolean; override;
    function DoReadCharacteristic(const ACharacteristic: TBluetoothGattCharacteristic): Boolean; override;
    function DoReadDescriptor(const ADescriptor: TBluetoothGattDescriptor): Boolean; override;
    function DoWriteCharacteristic(const ACharacteristic: TBluetoothGattCharacteristic): Boolean; override;
    function DoWriteDescriptor(const ADescriptor: TBluetoothGattDescriptor): Boolean; override;
    function DoReadRemoteRSSI: Boolean; override;
    function DoSetCharacteristicNotification(const ACharacteristic: TBluetoothGattCharacteristic; Enable: Boolean): Boolean; override;
    procedure ConnectionStatusChanged;
    function DoDisconnect: Boolean; override;
    function DoConnect: Boolean; override;
    procedure CloseServices;
    procedure SetScanned(AScanned: Boolean);
    function GetScanned: Boolean;
  public
    class function AddressFromId(const AId: HSTRING): Int64;
    property Scanned: Boolean read GetScanned write SetScanned;
    constructor Create(const AId: HSTRING; const ALEAdapter: TWinRTBluetoothLEAdapter; AutoConnect: Boolean); overload;
    constructor Create(const AAddress: UInt64; const ALEAdapter: TWinRTBluetoothLEAdapter; AutoConnect: Boolean); overload;
    destructor Destroy; override;
  end;

  TWinRTBluetoothGattServer = class(TBluetoothGattServer)
  private
    FAdvertismentPublisher: IBluetoothLEAdvertisementPublisher;
  protected
    { Service Management }
    function DoCreateService(const AnUUID: TBluetoothUUID; AType: TBluetoothServiceType): TBluetoothGattService; override;
    function DoCreateIncludedService(const AService: TBluetoothGattService; const AnUUID: TBluetoothUUID;
      AType: TBluetoothServiceType): TBluetoothGattService; override;
    { Characteristic Management }
    function DoCreateCharacteristic(const AService: TBluetoothGattService; const AnUUID: TBluetoothUUID;
      const AProps: TBluetoothPropertyFlags; const ADescription: string = ''): TBluetoothGattCharacteristic; override;
    { Descriptor Management }
    function DoCreateDescriptor(const ACharacteristic: TBluetoothGattCharacteristic; const AnUUID: TBluetoothUUID): TBluetoothGattDescriptor; override;
    function DoCreateAdvertiseData: TBluetoothLEAdvertiseData; override;
    { Add the previously created Services and characteristics... }
    function DoAddService(const AService: TBluetoothGattService): Boolean; override;
    function DoAddCharacteristic(const AService: TBluetoothGattService; const ACharacteristic: TBluetoothGattCharacteristic): Boolean; override;
	  function DoGetServices: TBluetoothGattServiceList; override;
    procedure DoClose; override;
    procedure DoClearServices; override;

    procedure DoCharacteristicReadRequest(const ADevice: System.Bluetooth.TBluetoothLEDevice; ARequestId: Integer; AnOffset: Integer;
      const AGattCharacteristic: TBluetoothGattCharacteristic); override;
    procedure DoCharacteristicWriteRequest(const ADevice: System.Bluetooth.TBluetoothLEDevice; ARequestId: Integer;
      const AGattCharacteristic: TBluetoothGattCharacteristic; APreparedWrite: Boolean; AResponseNeeded: Boolean;
      AnOffset: Integer; const AValue: TBytes); override;
    procedure DoUpdateCharacteristicValue(const ACharacteristic: TBluetoothGattCharacteristic); override;
    procedure DoServiceAdded(AStatus: TBluetoothGattStatus; const AService: TBluetoothGattService); override;

    /// <summary> Advertises peripheral manager data </summary>
    procedure DoStartAdvertising; override;
    /// <summary> Stops advertising peripheral manager data </summary>
    procedure DoStopAdvertising; override;
    /// <summary> A Boolean value indicating whether the peripheral is currently advertising data </summary>
    function DoIsAdvertising: Boolean; override;
  public
    constructor Create(const AManager: TBluetoothLEManager);
    destructor Destroy; override;
  end;

  TWinRTBluetoothGattService = class(TBluetoothGattService)
  private
    FUUID: TGUID;
  protected
    [Weak] FDevice: TWinRTBluetoothLEDevice;
    FGattService: GenericAttributeProfile_IGattDeviceService;
    FType: TBluetoothServiceType;
    function GetServiceUUID: TBluetoothUUID; override;
    function GetServiceType: TBluetoothServiceType; override;
    function DoGetCharacteristics: TBluetoothGattCharacteristicList; override;
    function DoGetIncludedServices: TBluetoothGattServiceList; override;
    function DoCreateCharacteristic(const AUuid: TBluetoothUUID; APropertyFlags: TBluetoothPropertyFlags;
      const ADescription: string): TBluetoothGattCharacteristic; override;
    function DoCreateIncludedService(const AnUUID: TBluetoothUUID; AType: TBluetoothServiceType): TBluetoothGattService; override;
    // Add the previously created Services and characteristics...
    function DoAddIncludedService(const AService: TBluetoothGattService): Boolean; override;
    function DoAddCharacteristic(const ACharacteristic: TBluetoothGattCharacteristic): Boolean; override;
    procedure CheckNotClosed;
    procedure Close;
  public
    constructor Create(const ADevice: TWinRTBluetoothLEDevice; const AGattService: GenericAttributeProfile_IGattDeviceService;
      AType: TBluetoothServiceType);
    destructor Destroy; override;
  end;

  TWinRTBluetoothGattCharacteristic = class;

  TGattValueChangedEventHandler = class(TInspectableObject,
    TypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs_Delegate_Base,
    TypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs)
  private
    [Weak] FGattCharacteristic: TWinRTBluetoothGattCharacteristic;
     procedure Invoke(sender: GenericAttributeProfile_IGattCharacteristic; args: GenericAttributeProfile_IGattValueChangedEventArgs); safecall;
  public
    constructor Create(const AGattCharacteristic: TWinRTBluetoothGattCharacteristic);
  end;

  TWinRTBluetoothGattCharacteristic = class(TBluetoothGattCharacteristic)
  private
    FValue: TBytes;
    FGattValueChanged: TypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs;
    FGattValueChangedERT: EventRegistrationToken;
    function UpdateValueFromDevice: TBluetoothGattStatus;
    function SetValueToDevice: TBluetoothGattStatus;
  protected
    [Weak] FBluetoothGattService: TWinRTBluetoothGattService;
    FGattCharacteristic: GenericAttributeProfile_IGattCharacteristic;
    FValueChangeEventHandle: TBluetoothGattEventHandle;
    function GetUUID: TBluetoothUUID; override;
    function GetProperties: TBluetoothPropertyFlags; override;
    function DoAddDescriptor(const ADescriptor: TBluetoothGattDescriptor): Boolean; override;
    function DoGetDescriptors: TBluetoothGattDescriptorList; override;
    function DoCreateDescriptor(const AUUID: TBluetoothUUID): TBluetoothGattDescriptor; override;
    function DoGetValue: TBytes; override;
    procedure DoSetValue(const AValue: TBytes); override;
    procedure GattValueChangedEvent(const Args: GenericAttributeProfile_IGattValueChangedEventArgs);
    function SetClientCharacteristicConfigurationDescriptor(
      const ADescriptorValue: GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue): Boolean;
  public
    constructor Create(const AService: TWinRTBluetoothGattService; const AGattCharacteristic: GenericAttributeProfile_IGattCharacteristic);
    destructor Destroy; override;
  end;

  TWinRTBluetoothGattDescriptor = class(TBluetoothGattDescriptor)
  private
    FValue: TBytes;
    function UpdateValueFromDevice: TBluetoothGattStatus;
    function SetValueToDevice: TBluetoothGattStatus;
  protected
    FGattDescriptor: GenericAttributeProfile_IGattDescriptor;
    // Characteristic Extended Properties
    function DoGetReliableWrite: Boolean; override;
    function DoGetWritableAuxiliaries: Boolean; override;
    // Characteristic User Description
    function DoGetUserDescription: string; override;
    procedure DoSetUserDescription(const Value: string); override;
    // Client Characteristic Configuration
    procedure DoSetNotification(const Value: Boolean); override;
    function DoGetNotification: Boolean; override;
    procedure DoSetIndication(const Value: Boolean); override;
    function DoGetIndication: Boolean; override;
    // Server Characteristic Configuration
    function DoGetBroadcasts: Boolean; override;
    procedure DoSetBroadcasts(const Value: Boolean); override;
    //Characteristic Presentation Format
    function DoGetFormat: TBluetoothGattFormatType; override;
    function DoGetExponent: ShortInt; override;
    function DoGetFormatUnit: TBluetoothUUID; override;
    function DoGetValue: TBytes; override;
    procedure DoSetValue(const AValue: TBytes); override;
    function GetUUID: TBluetoothUUID; override;
  public
    constructor Create(const ACharacteristic: TWinRTBluetoothGattCharacteristic;
      const AGattDescriptor: GenericAttributeProfile_IGattDescriptor);
  end;

 {Helper Functions}

function TBthLeUuidToUUID(const Uuid: TBthLeUuid): TBluetoothUUID; inline;
var
  TempGuuid: TBluetoothUUID;
begin
  if Uuid.IsShortUuid then
  begin
    TempGuuid := BTH_LE_ATT_BLUETOOTH_BASE_GUID;
    Inc(TempGuuid.D1, Uuid.ShortUuid);
    Result := TempGuuid;
  end
  else
    Result := Uuid.LongUuid;
end;

function BLEUuidToString(Uuid: TBthLeUuid): string; inline;
begin
  Result := TBthLeUuidToUUID(Uuid).ToString;
end;

function CheckOSVersionForGattClient: Boolean;
begin
  Result := TOSVersion.Check(10);
end;

function CheckOSVersionForGattServer: Boolean;
begin
  Result := TOSVersion.Check(10);
end;

function BthAddressToString(const AnAddress: TBluetoothAddress): string; inline;
begin
  Result := Format(SBluetoothMACAddressFormat, [AnAddress.rgBytes[5], AnAddress.rgBytes[4],
    AnAddress.rgBytes[3], AnAddress.rgBytes[2], AnAddress.rgBytes[1], AnAddress.rgBytes[0]]);
end;

function BytesFromIBuffer(const ABuffer: IBuffer): TBytes;
var
  LReader: IDataReader;
  LLength: Cardinal;
begin
  LReader := TDataReader.Statics.FromBuffer(ABuffer);
  LLength := LReader.UnconsumedBufferLength;
  SetLength(Result, LLength);
  if LLength > 0 then
    LReader.ReadBytes(LLength, @Result[0]);
end;

function BytesToIBuffer(const ABytes: TBytes; AOffset: Integer = 0): IBuffer;
var
  LWriter: IDataWriter;
  LNumBytes: Integer;
begin
  LWriter := TDataWriter.Create;
  LNumBytes := Length(ABytes) - AOffset;
  if LNumBytes > 0 then
    LWriter.WriteBytes(LNumBytes, @ABytes[AOffset]);
  Result := LWriter.DetachBuffer;
end;

{ TPlatformBluetoothLEManager }
class function TPlatformWinRTBluetoothLEManager.GetBluetoothManager: TBluetoothLEManager;
begin
  Result := TWinRTBluetoothLEManager.Create;
end;

{ TWinBluetoothLEManager }

function TWinRTBluetoothLEManager.DoGetGattServer: TBluetoothGattServer;
begin
  Result := TWinRTBluetoothGattServer.Create(Self);
end;

function TWinRTBluetoothLEManager.DoGetAdapter: TBluetoothLEAdapter;
begin
  if not GetRadioAdapter then
    FLEAdapter := nil;
  Result := FLEAdapter;
end;

function TWinRTBluetoothLEManager.DoGetSupportsGattClient: Boolean;
begin
  Result := CheckOSVersionForGattClient;
end;

function TWinRTBluetoothLEManager.DoGetSupportsGattServer: Boolean;
begin
  Result := CheckOSVersionForGattServer;
end;

function TWinRTBluetoothLEManager.GetConnectionState: TBluetoothConnectionState;
begin
  if GetRadioAdapter then
    Result := TBluetoothConnectionState.Connected
  else
    Result := TBluetoothConnectionState.Disconnected;
end;

function TWinRTBluetoothLEManager.GetRadioAdapter: Boolean;
var
  btfrp: TBlueToothFindRadioParams;
  hRadio: THandle;
  hFind: HBLUETOOTH_RADIO_FIND;
  LRadioInfo: TBluetoothRadioInfo;
begin
  FillChar(btfrp, SizeOf(btfrp), 0);
  btfrp.dwSize := SizeOf(btfrp);

  hFind := BluetoothFindFirstRadio(btfrp, hRadio);
  if hFind <> 0 then
  begin
    Result := True;
    if FLEAdapter = nil then
    begin
      LRadioInfo.dwSize := SizeOf(TBluetoothRadioInfo);
      BluetoothGetRadioInfo(hRadio, LRadioInfo);
      FLEAdapter := TWinRTBluetoothLEAdapter.Create(Self, LRadioInfo);
    end;
    BluetoothFindRadioClose(hFind);
  end
  else
    Result := False;
end;

destructor TWinRTBluetoothLEManager.Destroy;
begin
  if FLEAdapter <> nil then
  begin
    TWinRTBluetoothLEAdapter(FLEAdapter).StopDiscovery;
    FLEAdapter.Free;
  end;
  inherited;
end;

function TWinRTBluetoothLEManager.DoEnableBluetooth: Boolean;
begin
  Result := False;
end;

{ TWinRTBluetoothLEAdapter.TThreadTimer }

procedure TWinRTBluetoothLEAdapter.TDiscoverThreadTimer.Cancel;
begin
  Terminate;
  FEvent.SetEvent;
  FOnTimer := nil;
end;

constructor TWinRTBluetoothLEAdapter.TDiscoverThreadTimer.Create(const AnAdapter: TBluetoothLEAdapter;
  const AStopDiscoveryProc: TStopDiscoveryProc; Timeout: Cardinal);
begin
  inherited Create(True);
  FAdapter := AnAdapter;
  FOnTimer := AStopDiscoveryProc;
  FTimeout := Timeout;
  FEvent := TEvent.Create;
end;

destructor TWinRTBluetoothLEAdapter.TDiscoverThreadTimer.Destroy;
begin
  Cancel;
  inherited;
  FEvent.Free;
end;

procedure TWinRTBluetoothLEAdapter.TDiscoverThreadTimer.Execute;
begin
  inherited;
  FEvent.WaitFor(FTimeout);
  if (not Terminated) and Assigned(FOnTimer) then
    begin
      try
        FOnTimer;
      except
        if Assigned(System.Classes.ApplicationHandleException) then
          System.Classes.ApplicationHandleException(Self)
        else
          raise;
      end;
    end;
end;

{ TWinBluetoothLEAdapter }

procedure TWinRTBluetoothLEAdapter.BLEAdvertisementReceived(const AAdvertisement: IBluetoothLEAdvertisement;
  const AdvertisementType: BluetoothLEAdvertisementType; AAddress: UInt64; ARSSI: SmallInt;
  const ATimestamp: DateTime);
var
  LId: string;
  LDevice: System.Bluetooth.TBluetoothLEDevice;
  I: Integer;
  LSection: IBluetoothLEAdvertisementDataSection;
  LDataBytes: TBytes;
  LNew: Boolean;
begin
  LId := Format('%.12x', [AAddress]);
  LNew := False;
  LDevice := TWinRTBluetoothLEManager.GetDeviceInList(LId, FManager.AllDiscoveredDevices);
  if LDevice = nil then
  begin
    LNew := True;
    LDevice := TWinRTBluetoothLEDevice.Create(AAddress, Self, True);
  end;

  if AAdvertisement.DataSections.Size > 0 then
    for I := 0 to AAdvertisement.DataSections.Size - 1 do
    begin
      LSection := AAdvertisement.DataSections.GetAt(I);
      LDataBytes := BytesFromIBuffer(LSection.Data);
      if LDevice.AdvertisedData = nil then
        TWinRTBluetoothLEDevice(LDevice).FAdvertisedData := TScanResponse.Create;
      LDevice.AdvertisedData.AddOrSetValue(TScanResponseKey(LSection.DataType), LDataBytes);
      if AAdvertisement.LocalName <> 0 then
      begin
        TWinRTBluetoothLEDevice(LDevice).FDeviceName := AAdvertisement.LocalName.ToString;
        TWinRTBluetoothLEAdvertiseData(LDevice.ScannedAdvertiseData).FLocalName := AAdvertisement.LocalName.ToString;
      end;
    end;

  if AAdvertisement.ServiceUuids.Size > 0 then
    for I := 0 to AAdvertisement.ServiceUuids.Size - 1 do
      if not TWinRTBluetoothLEAdvertiseData(LDevice.ScannedAdvertiseData).FServiceUUIDs.Contains(AAdvertisement.ServiceUuids.GetAt(I)) then
      	TWinRTBluetoothLEAdvertiseData(LDevice.ScannedAdvertiseData).FServiceUUIDs.Add(AAdvertisement.ServiceUuids.GetAt(I));

  DoDeviceDiscovered(LDevice, LNew, nil);
end;

constructor TWinRTBluetoothLEAdapter.Create(const AManager: TBluetoothLEManager; const ARadioInfo: TBluetoothRadioInfo);
begin
  inherited Create(AManager);
  FScannedLEDevices := TBluetoothLEDeviceDictionary.Create;
  FRadioInfo := ARadioInfo;
  FBLEAdvertisementReceivedDelegate := TBLEAdvertisementReceivedEventHandler.Create(Self);
  FAdvertisementWatcher := TBluetoothLEAdvertisementWatcher.Create;
  FBLEAdvertisementReceivedDelegateERT := FAdvertisementWatcher.add_Received(FBLEAdvertisementReceivedDelegate);
  FAdvertisementWatcher.ScanningMode := BluetoothLEScanningMode.Active;
end;

destructor TWinRTBluetoothLEAdapter.Destroy;
begin
  StopDiscovery;
  if FBLEAdvertisementReceivedDelegateERT.Value <> 0 then
    FAdvertisementWatcher.remove_Received(FBLEAdvertisementReceivedDelegateERT);
  TBLEAdvertisementReceivedEventHandler(FBLEAdvertisementReceivedDelegate).Free;
  FBluetoothLEScanFilterList.Free;
  FScannedLEDevices.Free;
  inherited;
end;

procedure TWinRTBluetoothLEAdapter.GetBLEPairedDevices;
var
  LDeviceInformationCollectionAsyncOp: IAsyncOperation_1__IVectorView_1__IDeviceInformation;
  LBLEDevicesFound: IVectorView_1__IDeviceInformation;
  LWinBluetoothLEDevice: TWinRTBluetoothLEDevice;
  I: Integer;
begin
  for I := 0 to FManager.AllDiscoveredDevices.Count - 1 do
    TWinRTBluetoothLEDevice(FManager.AllDiscoveredDevices[I]).FPaired := False;
  if TAsyncOperation<IAsyncOperation_1__IVectorView_1__IDeviceInformation>.Wait(
      TDeviceInformation.Statics.FindAllAsync(TBluetoothLEDevice.Statics.GetDeviceSelector),
      LDeviceInformationCollectionAsyncOp) = AsyncStatus.Completed then
  begin
    LBLEDevicesFound := LDeviceInformationCollectionAsyncOp.GetResults;
    if LBLEDevicesFound.Size > 0 then
      for I := 0 to LBLEDevicesFound.Size - 1 do
      begin
        LWinBluetoothLEDevice := TWinRTBluetoothLEDevice(TWinRTBluetoothLEManager.GetDeviceInList
          (Format('%.12x', [TWinRTBluetoothLEDevice.AddressFromId(LBLEDevicesFound.GetAt(I).Id)]), FManager.AllDiscoveredDevices));
        if LWinBluetoothLEDevice = nil then
        begin
          LWinBluetoothLEDevice := TWinRTBluetoothLEDevice.Create(LBLEDevicesFound.GetAt(I).Id, Self, False);
          LWinBluetoothLEDevice := TWinRTBluetoothLEDevice(TWinRTBluetoothLEManager.AddDeviceToList
            (System.Bluetooth.TBluetoothLEDevice(LWinBluetoothLEDevice), FManager.AllDiscoveredDevices));
        end;
        LWinBluetoothLEDevice.FPaired := True;
      end;
  end;
end;

function TWinRTBluetoothLEAdapter.DoStartDiscovery(Timeout: Cardinal; const AFilterUUIDList: TBluetoothUUIDsList;
  const ABluetoothLEScanFilterList: TBluetoothLEScanFilterList): Boolean;
var
  I: Integer;
begin
  GetBLEPairedDevices;
  FScannedLEDevices.Clear;
  FBluetoothLEScanFilterList := ABluetoothLEScanFilterList;
  FAdvertisementWatcher.AdvertisementFilter.Advertisement.ServiceUuids.Clear;
  if AFilterUUIDList <> nil then
    for I := 0 to AFilterUUIDList.Count - 1 do
      FAdvertisementWatcher.AdvertisementFilter.Advertisement.ServiceUuids.Append(AFilterUUIDList[I]);
  FAdvertisementWatcher.Start;

  FTimerThread := TDiscoverThreadTimer.Create(Self, DoCancelDiscovery, Timeout);
  FTimerThread.Start;
  Result := True;
end;

function TWinRTBluetoothLEAdapter.DoStartDiscoveryRaw(const ABluetoothLEScanFilterList: TBluetoothLEScanFilterList;
  Refresh: Boolean): Boolean;
begin
  FScannedLEDevices.Clear;
  FBluetoothLEScanFilterList := ABluetoothLEScanFilterList;
  FAdvertisementWatcher.Start;
  Result := True;
end;

function TWinRTBluetoothLEAdapter.GetAdapterName: string;
begin
  Result := FRadioInfo.szName;
end;

function TWinRTBluetoothLEAdapter.GetAddress: System.Bluetooth.TBluetoothMacAddress;
var
  LAddress: TBluetoothAddress;
begin
  Result := '00:00:00:00:00:00'; // Do not translate
  LAddress := FRadioInfo.address;
  if LAddress.ullLong <> BLUETOOTH_NULL_ADDRESS.ullLong then
    Result := BthAddressToString(LAddress);
end;

function TWinRTBluetoothLEAdapter.GetScanMode: TBluetoothScanMode;
begin
  Result := Default(TBluetoothScanMode);
end;

function TWinRTBluetoothLEAdapter.GetState: TBluetoothAdapterState;
begin
  Result := Default(TBluetoothAdapterState);
end;

procedure TWinRTBluetoothLEAdapter.SetAdapterName(const Value: string);
begin
  raise EBluetoothAdapterException.Create(SBluetoothNotImplemented);
end;

procedure TWinRTBluetoothLEAdapter.StopDiscovery;
begin
  if FAdvertisementWatcher.Status = BluetoothLEAdvertisementWatcherStatus.Started then
  begin
    FAdvertisementWatcher.Stop;
    if FTimerThread <> nil then
    begin
      FTimerThread.Free;
      FTimerThread := nil;
    end;
  end;
end;

procedure TWinRTBluetoothLEAdapter.DoCancelDiscovery;
begin
  StopDiscovery;
  DoDiscoveryEnd(Self, nil);
end;

procedure TWinRTBluetoothLEAdapter.DoDeviceDiscovered(
  const ADevice: System.Bluetooth.TBluetoothLEDevice; ANewDevice: Boolean;
  const ABluetoothLEScanFilterList: TBluetoothLEScanFilterList);
var
  LDiscovered: Boolean;
begin
  LDiscovered := True;

  if (not ADevice.Scanned) then
  begin
    if (FBluetoothLEScanFilterList <> nil) then
      LDiscovered := DoDeviceOvercomesFilters(ADevice, FBluetoothLEScanFilterList);

    if ANewDevice then
      TWinRTBluetoothLEManager.AddDeviceToList(ADevice, FManager.AllDiscoveredDevices);

    if LDiscovered then
    begin
      //if TWinRTBluetoothLEDevice(ADevice).Paired then  //  Need to be thought
      TWinRTBluetoothLEManager.AddDeviceToList(ADevice, FManager.LastDiscoveredDevices);
      TWinRTBluetoothLEDevice(ADevice).Scanned := True;
    end;
  end;

  if LDiscovered then
    DoDiscoverDevice(Self, ADevice, ADevice.LastRSSI, ADevice.AdvertisedData);
end;

procedure TWinRTBluetoothLEAdapter.DoDiscoveryEnd(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
begin
  inherited DoDiscoveryEnd(Sender, nil);
end;

{ TWinBluetoothGattClient }

procedure TWinRTBluetoothLEDevice.CheckNotClosed;
begin
  if FClosed then
    raise EBluetoothServiceException.Create(SBluetoothLEDeviceDisconnectedExplicity);
end;

procedure TWinRTBluetoothLEDevice.CloseServices;
var
  LService: TBluetoothGattService;
begin
  for LService in FServices do
    (LService as TWinRTBluetoothGattService).Close;
end;

procedure TWinRTBluetoothLEDevice.ConnectionStatusChanged;
var
  LConnected: Boolean;
begin
  LConnected := GetIsConnected;
  if LConnected and Assigned(OnConnect) then
    OnConnect(Self)
  else
    if (not LConnected) and Assigned(OnDisconnect) then
      OnDisconnect(Self);
end;

constructor TWinRTBluetoothLEDevice.Create(const AAddress: UInt64; const ALEAdapter: TWinRTBluetoothLEAdapter;
  AutoConnect: Boolean);
begin
  inherited Create(AutoConnect);
  FAddress := AAddress;
  FLEAdapter := ALEAdapter;
end;

constructor TWinRTBluetoothLEDevice.Create(const AId: HSTRING; const ALEAdapter: TWinRTBluetoothLEAdapter;
  AutoConnect: Boolean);
begin
  inherited Create(AutoConnect);
  FId := AId;
  FAddress := AddressFromId(AId);
  FLEAdapter := ALEAdapter;
end;

destructor TWinRTBluetoothLEDevice.Destroy;
begin
  FReliableWriteTransaction := nil;
  DoDisconnect;
  inherited;
end;

procedure TWinRTBluetoothLEDevice.DoAbortReliableWrite;
begin
  FReliableWriteTransaction := nil;
end;

function TWinRTBluetoothLEDevice.DoBeginReliableWrite: Boolean;
begin
  FReliableWriteTransaction := TGenericAttributeProfile_GattReliableWriteTransaction.Create;
  Result := True;
end;

function TWinRTBluetoothLEDevice.DoDiscoverServices: Boolean;
var
  I: Integer;
  LGattService: GenericAttributeProfile_IGattDeviceService;
begin
  Result := True;
  FServices.Clear;
  CheckInitialized;
  if FBluetoothLEDevice.GattServices.Size > 0 then
    for I := 0 to FBluetoothLEDevice.GattServices.Size - 1 do
    begin
      LGattService := FBluetoothLEDevice.GattServices.GetAt(I);
      FServices.Add(TWinRTBluetoothGattService.Create(Self, LGattService, TBluetoothServiceType.Primary));
    end;
  DoOnServicesDiscovered(Self, FServices);
end;

function TWinRTBluetoothLEDevice.DoExecuteReliableWrite: Boolean;
var
  LWriteValueAsync: IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus;
begin
  CheckNotClosed;
  if FReliableWriteTransaction <> nil then
  begin
    if TAsyncOperation<IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus>.Wait(
      FReliableWriteTransaction.CommitAsync, LWriteValueAsync) = AsyncStatus.Completed then
        Result := LWriteValueAsync.GetResults = GenericAttributeProfile_GattCommunicationStatus.Success
    else
      Result := False;
    FReliableWriteTransaction := nil;
  end
  else
    Result := False;
end;

function TWinRTBluetoothLEDevice.DoReadCharacteristic(const ACharacteristic: TBluetoothGattCharacteristic): Boolean;
begin
  CheckNotClosed;
  TThread.CreateAnonymousThread(procedure
    begin
      DoOnCharacteristicRead(ACharacteristic, TWinRTBluetoothGattCharacteristic(ACharacteristic).UpdateValueFromDevice);
    end).Start;
  Result := True;
end;

function TWinRTBluetoothLEDevice.DoReadDescriptor(const ADescriptor: TBluetoothGattDescriptor): Boolean;
begin
  CheckNotClosed;
  TThread.CreateAnonymousThread(procedure
    begin
      DoOnDescriptorRead(ADescriptor, TWinRTBluetoothGattDescriptor(ADescriptor).UpdateValueFromDevice);
    end).Start;
  Result := True;
end;

function TWinRTBluetoothLEDevice.DoReadRemoteRSSI: Boolean;
begin
  { Not supported on windows }
  Result := False;
end;

function TWinRTBluetoothLEDevice.DoConnect: Boolean;
begin
  { Not supported on windows }
  Result := False;
end;

function TWinRTBluetoothLEDevice.DoCreateAdvertiseData: TBluetoothLEAdvertiseData;
begin
  Result := TWinRTBluetoothLEAdvertiseData.Create(Self);
end;

function TWinRTBluetoothLEDevice.DoSetCharacteristicNotification(const ACharacteristic: TBluetoothGattCharacteristic;
  Enable: Boolean): Boolean;
var
  LDescriptorValue: GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue;
begin
  CheckNotClosed;

  if TBluetoothProperty.Notify in ACharacteristic.Properties then
      LDescriptorValue := GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue.Notify
    else
      if TBluetoothProperty.Indicate in ACharacteristic.Properties then
        LDescriptorValue := GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue.Indicate
      else
        Exit(False);

  if not Enable then
    LDescriptorValue := GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue.None;

  Result := TWinRTBluetoothGattCharacteristic(ACharacteristic).SetClientCharacteristicConfigurationDescriptor(LDescriptorValue);
end;

function TWinRTBluetoothLEDevice.DoWriteCharacteristic(const ACharacteristic: TBluetoothGattCharacteristic): Boolean;
begin
  CheckNotClosed;
  TThread.CreateAnonymousThread(procedure
    begin
      DoOnCharacteristicWrite(ACharacteristic, TWinRTBluetoothGattCharacteristic(ACharacteristic).SetValueToDevice);
    end).Start;
  Result := True;
end;

function TWinRTBluetoothLEDevice.DoWriteDescriptor(const ADescriptor: TBluetoothGattDescriptor): Boolean;
begin
  CheckNotClosed;
  TThread.CreateAnonymousThread(procedure
    begin
      DoOnDescriptorWrite(ADescriptor, TWinRTBluetoothGattDescriptor(ADescriptor).SetValueToDevice);
    end).Start;
  Result := True;
end;


function TWinRTBluetoothLEDevice.GetAddress: TBluetoothMacAddress;
begin
  Result := Format('%.12x', [FAddress]).ToUpper;
  Result.Insert(2,':');
  Result.Insert(5,':');
  Result.Insert(8,':');
  Result.Insert(11,':');
  Result.Insert(14,':');
end;

function TWinRTBluetoothLEDevice.GetBluetoothType: TBluetoothType;
begin
  Result := TBluetoothType.LE;
end;

function TWinRTBluetoothLEDevice.GetDeviceName: string;
begin
  Result := FDeviceName;
end;

function TWinRTBluetoothLEDevice.GetIdentifier: string;
begin
  Result := Format('%.12x', [FAddress]);
end;

function TWinRTBluetoothLEDevice.GetIsConnected: Boolean;
begin
  if FBluetoothLEDevice <> nil then
    Result := FBluetoothLEDevice.ConnectionStatus = BluetoothConnectionStatus.Connected
  else
    Result := False;
end;

function TWinRTBluetoothLEDevice.GetScanned: Boolean;
begin
  Result := FScanned;
end;

procedure TWinRTBluetoothLEDevice.SetScanned(AScanned: Boolean);
begin
  FScanned := AScanned;
end;

class function TWinRTBluetoothLEDevice.AddressFromId(const AId: HSTRING): Int64;
var
  LTemp: string;
  LDevPos: Integer;
begin
  LTemp := AId.ToString;
  LDevPos := LTemp.ToUpper.IndexOf('#DEV_');  // Do not translate
  if LDevPos <> -1 then
    LTemp := LTemp.Substring(LDevPos + 5, 12)
  else
    LTemp := ReplaceStr(RightStr(LTemp, 17), ':', '');
  Result := StrToInt64('$' + LTemp);
end;

procedure TWinRTBluetoothLEDevice.CheckInitialized;
var
  LBLEDeviceAsyncOp: IAsyncOperation_1__IBluetoothLEDevice;
begin
  if (FBluetoothLEDevice = nil) or FClosed then
  begin
    if FId = 0 then
      raise EBluetoothDeviceException.Create(SBluetoothLEDeviceNotPaired);
    if TAsyncOperation<IAsyncOperation_1__IBluetoothLEDevice>.Wait(
        TBluetoothLEDevice.Statics.FromIdAsync(FId), LBLEDeviceAsyncOp) = AsyncStatus.Completed then
      begin
        FBluetoothLEDevice := LBLEDeviceAsyncOp.GetResults;
        FClosed := False;
        if DeviceName = '' then
          FDeviceName := FBluetoothLEDevice.Name.ToString;
        FConnectionStatusChangeDelegate := TConnectionStatusChangeEventHandler.Create(Self);
        FBluetoothLEDevice.add_ConnectionStatusChanged(FConnectionStatusChangeDelegate);
      end;
  end;
end;

function TWinRTBluetoothLEDevice.DoDisconnect: Boolean;
var
  LBLEClosable: IClosable;
begin
  if (not FClosed) and (FBluetoothLEDevice <> nil) and (FBluetoothLEDevice.QueryInterface(IClosable, LBLEClosable) = 0) then
  begin
    CloseServices;
    LBLEClosable.Close;
    FClosed := True;
    Result := True;
  end
  else
    Result := False;
end;

{ TWinBluetoothGattCharacteristic }

constructor TWinRTBluetoothGattCharacteristic.Create(const AService: TWinRTBluetoothGattService;
  const AGattCharacteristic: GenericAttributeProfile_IGattCharacteristic);
begin
  inherited Create(AService);
  FBluetoothGattService := AService;
  FGattCharacteristic := AGattCharacteristic;
  if (TBluetoothProperty.Notify in Properties) or (TBluetoothProperty.Indicate in Properties) then
    FGattValueChanged := TGattValueChangedEventHandler.Create(Self);
end;

destructor TWinRTBluetoothGattCharacteristic.Destroy;
begin
  if FGattValueChangedERT.Value <> 0 then
    FGattCharacteristic.remove_ValueChanged(FGattValueChangedERT);
  inherited;
end;

function TWinRTBluetoothGattCharacteristic.DoAddDescriptor(const ADescriptor: TBluetoothGattDescriptor): Boolean;
begin
  Result := False;
end;

function TWinRTBluetoothGattCharacteristic.DoCreateDescriptor(const AUUID: TBluetoothUUID): TBluetoothGattDescriptor;
begin
  raise EBluetoothLECharacteristicException.Create(SBluetoothNotImplemented);
  Result := nil;
end;

function TWinRTBluetoothGattCharacteristic.DoGetDescriptors: TBluetoothGattDescriptorList;
var
  LGattDescriptors: IVectorView_1__GenericAttributeProfile_IGattDescriptor;
  I: Integer;
begin
  FDescriptors.Clear;
  LGattDescriptors := (FGattCharacteristic as GenericAttributeProfile_IGattCharacteristic2).GetAllDescriptors;
  if LGattDescriptors.Size > 0 then
    for I := 0 to LGattDescriptors.Size - 1 do
      FDescriptors.Add(TWinRTBluetoothGattDescriptor.Create(Self, LGattDescriptors.GetAt(I)));
  Result := FDescriptors;
end;

function TWinRTBluetoothGattCharacteristic.DoGetValue: TBytes;
begin
  Result := FValue;
end;

procedure TWinRTBluetoothGattCharacteristic.DoSetValue(const AValue: TBytes);
begin
  FValue := AValue;
end;

procedure TWinRTBluetoothGattCharacteristic.GattValueChangedEvent(const Args: GenericAttributeProfile_IGattValueChangedEventArgs);
begin
  FValue := BytesFromIBuffer(Args.CharacteristicValue);
  TWinRTBluetoothGattService(FService).FDevice.DoOnCharacteristicRead(Self, TBluetoothGattStatus.Success);
end;

function TWinRTBluetoothGattCharacteristic.GetProperties: TBluetoothPropertyFlags;

  function HasProperty(const Props: GenericAttributeProfile_GattCharacteristicProperties;
    const AProperty: GenericAttributeProfile_GattCharacteristicProperties): Boolean;
  begin
    Result := (Ord(Props) and Ord(AProperty) = Ord(AProperty));
  end;

var
  LProps: GenericAttributeProfile_GattCharacteristicProperties;
begin
  Result := [];
  LProps := FGattCharacteristic.CharacteristicProperties;
  if HasProperty(LProps, GenericAttributeProfile_GattCharacteristicProperties.Broadcast) then
    Include(Result, TBluetoothProperty.Broadcast);
  if HasProperty(LProps, GenericAttributeProfile_GattCharacteristicProperties.Read) then
    Include(Result, TBluetoothProperty.Read);
  if HasProperty(LProps, GenericAttributeProfile_GattCharacteristicProperties.Write) then
    Include(Result, TBluetoothProperty.Write);
  if HasProperty(LProps, GenericAttributeProfile_GattCharacteristicProperties.WriteWithoutResponse) then
    Include(Result, TBluetoothProperty.WriteNoResponse);
  if HasProperty(LProps, GenericAttributeProfile_GattCharacteristicProperties.AuthenticatedSignedWrites) then
    Include(Result, TBluetoothProperty.SignedWrite);
  if HasProperty(LProps, GenericAttributeProfile_GattCharacteristicProperties.Notify) then
    Include(Result, TBluetoothProperty.Notify);
  if HasProperty(LProps, GenericAttributeProfile_GattCharacteristicProperties.Indicate) then
    Include(Result, TBluetoothProperty.Indicate);
end;

function TWinRTBluetoothGattCharacteristic.GetUUID: TBluetoothUUID;
begin
  Result := FGattCharacteristic.Uuid;
end;

function TWinRTBluetoothGattCharacteristic.SetClientCharacteristicConfigurationDescriptor(
   const ADescriptorValue: GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue): Boolean;
var
  LWriteDescValueAsync: IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus;
begin
  Result := False;
  if ADescriptorValue = GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue.None then
  begin
    if FGattValueChangedERT.Value <> 0 then
    begin
      FGattCharacteristic.remove_ValueChanged(FGattValueChangedERT);
      FGattValueChangedERT.Value := 0;
    end;
  end
  else
    if (TBluetoothProperty.Notify in Properties) or (TBluetoothProperty.Indicate in Properties) then
    begin
      if FGattValueChangedERT.Value <> 0 then
        FGattCharacteristic.remove_ValueChanged(FGattValueChangedERT);
      FGattValueChangedERT := FGattCharacteristic.add_ValueChanged(FGattValueChanged);
    end;

  if TAsyncOperation<IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus>.Wait(
      FGattCharacteristic.WriteClientCharacteristicConfigurationDescriptorAsync(ADescriptorValue),
      LWriteDescValueAsync) = AsyncStatus.Completed then
    Result := LWriteDescValueAsync.GetResults = GenericAttributeProfile_GattCommunicationStatus.Success;
end;

function TWinRTBluetoothGattCharacteristic.SetValueToDevice: TBluetoothGattStatus;
var
  LWriteValueAsync: IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus;
  LWriteOption: GenericAttributeProfile_GattWriteOption;
begin
  Result := TBluetoothGattStatus.Failure;
  if FBluetoothGattService.FDevice.FReliableWriteTransaction <> nil then
  begin
    FBluetoothGattService.FDevice.FReliableWriteTransaction.WriteValue(FGattCharacteristic, BytesToIBuffer(FValue));
    Result := TBluetoothGattStatus.Success;
  end
  else
  begin
    if TBluetoothProperty.WriteNoResponse in Properties then
      LWriteOption := GenericAttributeProfile_GattWriteOption.WriteWithoutResponse
    else
      LWriteOption := GenericAttributeProfile_GattWriteOption.WriteWithResponse;

    if TAsyncOperation<IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus>.Wait(
      FGattCharacteristic.WriteValueAsync(BytesToIBuffer(FValue), LWriteOption),
      LWriteValueAsync) = AsyncStatus.Completed then
      if LWriteValueAsync.GetResults = GenericAttributeProfile_GattCommunicationStatus.Success then
        Result := TBluetoothGattStatus.Success;
  end;
end;

function TWinRTBluetoothGattCharacteristic.UpdateValueFromDevice: TBluetoothGattStatus;
var
  LGattReadValueAsyncOp: IAsyncOperation_1__GenericAttributeProfile_IGattReadResult;
  LReadResult: GenericAttributeProfile_IGattReadResult;
begin
  Result := TBluetoothGattStatus.Failure;
  if TAsyncOperation<IAsyncOperation_1__GenericAttributeProfile_IGattReadResult>.Wait(
      FGattCharacteristic.ReadValueAsync(BluetoothCacheMode.Uncached),
      LGattReadValueAsyncOp) = AsyncStatus.Completed then
  begin
    LReadResult := LGattReadValueAsyncOp.GetResults;
    if LReadResult.Status = GenericAttributeProfile_GattCommunicationStatus.Success then
    begin
      FValue := BytesFromIBuffer(LReadResult.Value);
      Result := TBluetoothGattStatus.Success;
    end;
  end;
end;

{ TWinBluetoothGattService }

procedure TWinRTBluetoothGattService.CheckNotClosed;
begin
  if FDevice.FClosed then
    raise EBluetoothServiceException.Create(SBluetoothLEDeviceDisconnectedExplicity);
end;

procedure TWinRTBluetoothGattService.Close;
var
  LGattServiceClosable: IClosable;
begin
  if (FGattService <> nil) and (FGattService.QueryInterface(IClosable, LGattServiceClosable) = 0) then
  begin
    LGattServiceClosable.Close;
    FGattService := nil;
  end;
end;

constructor TWinRTBluetoothGattService.Create(const ADevice: TWinRTBluetoothLEDevice;
  const AGattService: GenericAttributeProfile_IGattDeviceService; AType: TBluetoothServiceType);
begin
  inherited Create(TGUID.Empty, AType);
  FDevice := ADevice;
  FGattService := AGattService;
  FUUID := FGattService.Uuid;
  FType := AType;
end;

destructor TWinRTBluetoothGattService.Destroy;
begin
  Close;
  inherited;
end;

function TWinRTBluetoothGattService.DoGetCharacteristics: TBluetoothGattCharacteristicList;
var
  I: Integer;
  LGattCharacteristics: IVectorView_1__GenericAttributeProfile_IGattCharacteristic;
begin
  CheckNotClosed;
  FCharacteristics.Clear;
  LGattCharacteristics := (FGattService as GenericAttributeProfile_IGattDeviceService2).GetAllCharacteristics;
  if LGattCharacteristics.Size > 0 then
    for I := 0 to LGattCharacteristics.Size - 1 do
      FCharacteristics.Add(TWinRTBluetoothGattCharacteristic.Create(Self, LGattCharacteristics.GetAt(I)));
  Result := FCharacteristics;
end;

function TWinRTBluetoothGattService.DoGetIncludedServices: TBluetoothGattServiceList;
var
  I: Integer;
  LGattServices: IVectorView_1__GenericAttributeProfile_IGattDeviceService;
begin
  CheckNotClosed;
  FIncludedServices.Clear;
  LGattServices := (FGattService as GenericAttributeProfile_IGattDeviceService2).GetAllIncludedServices;
  if LGattServices.Size > 0 then
    for I := 0 to LGattServices.Size - 1 do
      FIncludedServices.Add(TWinRTBluetoothGattService.Create(FDevice, LGattServices.GetAt(I), TBluetoothServiceType.Primary));
  Result := FIncludedServices;
end;

function TWinRTBluetoothGattService.DoAddIncludedService(const AService: TBluetoothGattService): Boolean;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := False;
end;

function TWinRTBluetoothGattService.DoAddCharacteristic(const ACharacteristic: TBluetoothGattCharacteristic): Boolean;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := False;
end;

function TWinRTBluetoothGattService.DoCreateCharacteristic(const AUuid: TBluetoothUUID; APropertyFlags: TBluetoothPropertyFlags;
  const ADescription: string): TBluetoothGattCharacteristic;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := nil;
end;

function TWinRTBluetoothGattService.DoCreateIncludedService(const AnUUID: TBluetoothUUID; AType: TBluetoothServiceType): TBluetoothGattService;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := nil;
end;

function TWinRTBluetoothGattService.GetServiceType: TBluetoothServiceType;
begin
  Result := FType;
end;

function TWinRTBluetoothGattService.GetServiceUUID: TBluetoothUUID;
begin
  Result := FUUID;
end;

{ TWinBluetoothGattDescriptor }

constructor TWinRTBluetoothGattDescriptor.Create(const ACharacteristic: TWinRTBluetoothGattCharacteristic;
  const AGattDescriptor: GenericAttributeProfile_IGattDescriptor);
begin
  inherited Create(ACharacteristic);
  FGattDescriptor := AGattDescriptor;
end;

function TWinRTBluetoothGattDescriptor.DoGetBroadcasts: Boolean;
begin
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.ServerCharacteristicConfiguration  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);

  Result := TBluetoothProperty.Broadcast in FCharacteristic.Properties;
end;

function TWinRTBluetoothGattDescriptor.DoGetExponent: ShortInt;
begin
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.CharacteristicPresentationFormat  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);
  Result := ShortInt(Value[1]);
end;

function TWinRTBluetoothGattDescriptor.DoGetFormat: TBluetoothGattFormatType;
begin
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.CharacteristicPresentationFormat  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);
  Result := TBluetoothGattFormatType(Value[0]);
end;

function TWinRTBluetoothGattDescriptor.DoGetFormatUnit: TBluetoothUUID;
var
  LValue: Word;
begin
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.CharacteristicPresentationFormat  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);
  if Length(FValue) < 4 then
    LValue := 0
  else
    LValue := FValue[2] or (FValue[3] shl 8);
  Result := TBluetoothUUIDHelper.GetBluetoothUUID(LValue);
end;

function TWinRTBluetoothGattDescriptor.DoGetIndication: Boolean;
begin
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.ClientCharacteristicConfiguration  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);

  Result := TBluetoothProperty.Indicate in FCharacteristic.Properties;
end;

function TWinRTBluetoothGattDescriptor.DoGetNotification: Boolean;
begin
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.ClientCharacteristicConfiguration  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);

  Result := TBluetoothProperty.Notify in FCharacteristic.Properties;
end;

function TWinRTBluetoothGattDescriptor.DoGetReliableWrite: Boolean;
const
  LProp = GenericAttributeProfile_GattCharacteristicProperties.ReliableWrites;
var
  LProps: GenericAttributeProfile_GattCharacteristicProperties;
begin
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.CharacteristicExtendedProperties  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);
  LProps := TWinRTBluetoothGattCharacteristic(FCharacteristic).FGattCharacteristic.CharacteristicProperties;
  Result := Ord(LProps) and Ord(LProp) = Ord(LProp);
end;

function TWinRTBluetoothGattDescriptor.DoGetUserDescription: string;
begin
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.CharacteristicUserDescription  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);
  Result := TEncoding.Unicode.GetString(FValue);
end;

function TWinRTBluetoothGattDescriptor.DoGetValue: TBytes;
begin
  Result := FValue;
end;

function TWinRTBluetoothGattDescriptor.DoGetWritableAuxiliaries: Boolean;
const
  LProp = GenericAttributeProfile_GattCharacteristicProperties.ReliableWrites;
var
  LProps: GenericAttributeProfile_GattCharacteristicProperties;
begin
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.CharacteristicExtendedProperties  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);
  LProps := TWinRTBluetoothGattCharacteristic(FCharacteristic).FGattCharacteristic.CharacteristicProperties;
  Result := Ord(LProps) and Ord(LProp) = Ord(LProp);
end;

procedure TWinRTBluetoothGattDescriptor.DoSetBroadcasts(const Value: Boolean);
var
  B: TBytes;
begin
  if not (TBluetoothProperty.Broadcast in FCharacteristic.Properties) then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);

  if Value then
    B := [$01, $00]
  else
    B := [$00, $00];
  SetValue(B);
end;

procedure TWinRTBluetoothGattDescriptor.DoSetIndication(const Value: Boolean);
var
  B: TBytes;
begin
  inherited;
   if not (TBluetoothProperty.Indicate in FCharacteristic.Properties) then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);

  if Value then
    B := [$02, $00]
  else
    B := [$00, $00];
  SetValue(B);
end;

procedure TWinRTBluetoothGattDescriptor.DoSetNotification(const Value: Boolean);
var
  B: TBytes;
begin
  inherited;
  if not (TBluetoothProperty.Notify in FCharacteristic.Properties) then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);

  if Value then
    B := [$01, $00]
  else
    B := [$00, $00];
  SetValue(B);
end;

procedure TWinRTBluetoothGattDescriptor.DoSetUserDescription(const Value: string);
begin
  inherited;
  if UUID <> TGenericAttributeProfile_GattDescriptorUuids.Statics.CharacteristicUserDescription  then
    raise EBluetoothLEDescriptorException.Create(SBluetoothDescriptorTypeError);
  DoSetValue(TEncoding.Unicode.GetBytes(Value));
end;

procedure TWinRTBluetoothGattDescriptor.DoSetValue(const AValue: TBytes);
begin
  FValue := AValue;
end;

function TWinRTBluetoothGattDescriptor.GetUUID: TBluetoothUUID;
begin
  Result := FGattDescriptor.Uuid;
end;

function TWinRTBluetoothGattDescriptor.SetValueToDevice: TBluetoothGattStatus;
var
  LWriteValueAsync: IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus;
begin
  Result := TBluetoothGattStatus.Failure;
  if (TAsyncOperation<IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus>.Wait(
      FGattDescriptor.WriteValueAsync(BytesToIBuffer(FValue)), LWriteValueAsync) = AsyncStatus.Completed) and
      (LWriteValueAsync.GetResults = GenericAttributeProfile_GattCommunicationStatus.Success) then
    Result := TBluetoothGattStatus.Success;
end;

function TWinRTBluetoothGattDescriptor.UpdateValueFromDevice: TBluetoothGattStatus;
var
  LGattReadValueAsyncOp: IAsyncOperation_1__GenericAttributeProfile_IGattReadResult;
  LReadResult: GenericAttributeProfile_IGattReadResult;
begin
  Result := TBluetoothGattStatus.Success;
  if TAsyncOperation<IAsyncOperation_1__GenericAttributeProfile_IGattReadResult>.Wait(
      FGattDescriptor.ReadValueAsync(BluetoothCacheMode.Uncached),
      LGattReadValueAsyncOp) = AsyncStatus.Completed then
  begin
    LReadResult := LGattReadValueAsyncOp.GetResults;
    if LReadResult.Status = GenericAttributeProfile_GattCommunicationStatus.Success then
      FValue := BytesFromIBuffer(LReadResult.Value)
    else
      Result := TBluetoothGattStatus.Failure;
  end;
end;

{ TConnectionStatusChangeEventHandler }

constructor TConnectionStatusChangeEventHandler.Create(const ADevice: TWinRTBluetoothLEDevice);
begin
  inherited Create;
  FDevice := ADevice;
end;

procedure TConnectionStatusChangeEventHandler.Invoke(sender: IBluetoothLEDevice; args: IInspectable);
begin
  FDevice.ConnectionStatusChanged;
end;

{ TGattValueChangedEventHandler }

constructor TGattValueChangedEventHandler.Create(const AGattCharacteristic: TWinRTBluetoothGattCharacteristic);
begin
  inherited Create;
  FGattCharacteristic := AGattCharacteristic;
end;

procedure TGattValueChangedEventHandler.Invoke(sender: GenericAttributeProfile_IGattCharacteristic;
  args: GenericAttributeProfile_IGattValueChangedEventArgs);
begin
  FGattCharacteristic.GattValueChangedEvent(args);
end;

{ TAsyncOperation }

class function TAsyncOperation<T>.Wait(const AAsyncOp: T; var AsyncOpResult: T; ATimeout: Cardinal): AsyncStatus;
var
  LAsyncInfo: IAsyncInfo;
  LTimer: TThreadTimer;
begin

  if AAsyncOp.QueryInterface(IAsyncInfo, LAsyncInfo) <> 0 then
    raise Exception.Create(SNoAsyncInfo);

  LTimer := TThreadTimer.Create(
    procedure
    begin
        LAsyncInfo.Cancel;
    end,
    ATimeout);

  LTimer.Start;

  while LAsyncInfo.Status = AsyncStatus.Started do
    TThread.Yield;

  LTimer.Free;

  AsyncOpResult := AAsyncOp;
  Result := LAsyncInfo.Status;
end;

{ TAsyncOperation.TThreadTimer }

procedure TAsyncOperation<T>.TThreadTimer.Cancel;
begin
  Terminate;
  FEvent.SetEvent;
  FOnTimer := nil;
end;

constructor TAsyncOperation<T>.TThreadTimer.Create(const ACancelProc: TCancelProcedure; Timeout: Cardinal);
begin
  inherited Create(True);
  FOnTimer := ACancelProc;
  FTimeout := Timeout;
  FEvent := TEvent.Create;
end;

destructor TAsyncOperation<T>.TThreadTimer.Destroy;
begin
  Cancel;
  FEvent.Free;
  inherited;
end;

procedure TAsyncOperation<T>.TThreadTimer.Execute;
begin
  inherited;
  FEvent.WaitFor(FTimeout);
  if not Terminated and Assigned(FOnTimer) then
    FOnTimer;
end;

{ TBLEAdvertisementReceivedEventHandler }

constructor TBLEAdvertisementReceivedEventHandler.Create(const AAdapter: TWinRTBluetoothLEAdapter);
begin
  inherited Create;
  FAdapter := AAdapter;
end;

procedure TBLEAdvertisementReceivedEventHandler.Invoke(sender: IBluetoothLEAdvertisementWatcher;
  args: IBluetoothLEAdvertisementReceivedEventArgs);
begin
  FAdapter.BLEAdvertisementReceived(args.Advertisement, args.AdvertisementType, args.BluetoothAddress,
    args.RawSignalStrengthInDBm, args.Timestamp);
end;

{ TWinRTBluetoothLEAdvertiseData }

function TWinRTBluetoothLEAdvertiseData.ContainsServiceUUID(const AServiceUUID: TBluetoothUUID): Boolean;
begin
  Result := FServiceUUIDs.Contains(AServiceUUID);
end;

constructor TWinRTBluetoothLEAdvertiseData.Create(const ADevice: System.Bluetooth.TBluetoothLEDevice);
begin
  inherited Create;
  FDevice := ADevice;
end;

function TWinRTBluetoothLEAdvertiseData.DoAddServiceData(const AServiceUUID: TBluetoothUUID;
  const AData: TBytes): Boolean;
begin
// Not supported in Windows
  Result := False;
end;

function TWinRTBluetoothLEAdvertiseData.DoAddServiceUUID(const AServiceUUID: TBluetoothUUID): Boolean;
begin
  // Not supported in Windows
  Result := False;
end;

procedure TWinRTBluetoothLEAdvertiseData.DoClearServiceData;
begin
  inherited;
// Not supported in Windows
end;

procedure TWinRTBluetoothLEAdvertiseData.DoClearServiceUUIDs;
begin
  inherited;
// Not supported in Windows
end;

procedure TWinRTBluetoothLEAdvertiseData.DoRemoveServiceData(const AServiceUUID: TBluetoothUUID);
begin
  inherited;
// Not supported in Windows
end;

procedure TWinRTBluetoothLEAdvertiseData.DoRemoveServiceUUID(const AServiceUUID: TBluetoothUUID);
begin
  inherited;
// Not supported in Windows
end;

function TWinRTBluetoothLEAdvertiseData.GetDataForService(const AServiceUUID: TBluetoothUUID): TBytes;
begin
  if Length(GetServiceData) > 0 then
    FServiceData.TryGetValue(AServiceUUID, Result)
  else
    Result := nil;
end;

function TWinRTBluetoothLEAdvertiseData.GetLocalName: string;
begin
  Result := FLocalName;
end;

function TWinRTBluetoothLEAdvertiseData.GetManufacturerSpecificData: TBytes;
begin
  if (FDevice <> nil) and (FDevice.AdvertisedData <> nil) then
    if FDevice.AdvertisedData.ContainsKey(TScanResponseKey.ManufacturerSpecificData) then
      FManufacturerSpecificData := FDevice.AdvertisedData.Items[TScanResponseKey.ManufacturerSpecificData];
  Result := FManufacturerSpecificData;
end;

function TWinRTBluetoothLEAdvertiseData.GetServiceData: TArray<TServiceDataRawData>;
var
  LData: TBytes;
  LServiceTBytes: TBytes;
  LServiceUUID: TGUID;
  LSize: Integer;
  LServiceData: TPair<TBluetoothUUID,TBytes>;
begin
  if (FDevice <> nil) and (FDevice.AdvertisedData <> nil) then
    if FDevice.AdvertisedData.ContainsKey(TScanResponseKey.ServiceData) then
    begin
      LData := FDevice.AdvertisedData.Items[TScanResponseKey.ServiceData];
      LServiceUUID := TBluetoothUUIDHelper.GetBluetoothUUID(PWord(@LData[0])^);
      LSize := Length(LData) - 2;
      SetLength(LServiceTBytes, LSize);
      Move(LData[2], LServiceTBytes[0], LSize);
      FServiceData.AddOrSetValue(LServiceUUID, LServiceTBytes);
    end;

  // Prepared to be an array, but it will just own one element for now.
  SetLength(Result, FServiceData.count);
  LSize := 0;
  for LServiceData in FServiceData do
  begin
    Result[LSize].create(LServiceData);;
    Inc(LSize);
  end;
end;

function TWinRTBluetoothLEAdvertiseData.GetServiceUUIDs: TArray<TBluetoothUUID>;
begin
  Result := FServiceUUIDs.ToArray;
end;

function TWinRTBluetoothLEAdvertiseData.GetTxPowerLevel: Integer;
begin
  if (FDevice <> nil) and (FDevice.AdvertisedData <> nil) then
    if FDevice.AdvertisedData.ContainsKey(TScanResponseKey.TxPowerLevel) then
      FTxPowerLevel := ShortInt(FDevice.AdvertisedData.Items[TScanResponseKey.TxPowerLevel]);
  Result := FTxPowerLevel;
end;

procedure TWinRTBluetoothLEAdvertiseData.SetLocalName(const ALocalName: string);
begin
  inherited;
// Not supported in Windows
end;

procedure TWinRTBluetoothLEAdvertiseData.SetManufacturerSpecificData(const AManufacturerSpecificData: TBytes);
begin
  inherited;
  FManufacturerSpecificData := AManufacturerSpecificData;
end;

procedure TWinRTBluetoothLEAdvertiseData.SetTxPowerLevel(ATxPowerLevel: Integer);
begin
  inherited;
// Not supported in Windows
end;

{ TWinRTBluetoothGattServer }

constructor TWinRTBluetoothGattServer.Create(const AManager: TBluetoothLEManager);
begin
  inherited;
  FAdvertismentPublisher := TBluetoothLEAdvertisementPublisher.Create;
end;

destructor TWinRTBluetoothGattServer.Destroy;
begin
  if IsAdvertising then
    StopAdvertising;
  inherited;
end;

function TWinRTBluetoothGattServer.DoAddCharacteristic(const AService: TBluetoothGattService;
  const ACharacteristic: TBluetoothGattCharacteristic): Boolean;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := False;
end;

function TWinRTBluetoothGattServer.DoAddService(const AService: TBluetoothGattService): Boolean;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := False;
end;

procedure TWinRTBluetoothGattServer.DoClearServices;
begin
  inherited;
  // Not implemented
end;

procedure TWinRTBluetoothGattServer.DoClose;
begin
  inherited;
  // Not implemented
end;

function TWinRTBluetoothGattServer.DoCreateAdvertiseData: TBluetoothLEAdvertiseData;
begin
  Result := TWinRTBluetoothLEAdvertiseData.Create(nil);
end;

function TWinRTBluetoothGattServer.DoCreateCharacteristic(const AService: TBluetoothGattService;
  const AnUUID: TBluetoothUUID; const AProps: TBluetoothPropertyFlags;
  const ADescription: string): TBluetoothGattCharacteristic;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := nil;
end;

function TWinRTBluetoothGattServer.DoCreateDescriptor(const ACharacteristic: TBluetoothGattCharacteristic;
  const AnUUID: TBluetoothUUID): TBluetoothGattDescriptor;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := nil;
end;

function TWinRTBluetoothGattServer.DoCreateIncludedService(const AService: TBluetoothGattService;
  const AnUUID: TBluetoothUUID; AType: TBluetoothServiceType): TBluetoothGattService;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := nil;
end;

function TWinRTBluetoothGattServer.DoCreateService(const AnUUID: TBluetoothUUID;
  AType: TBluetoothServiceType): TBluetoothGattService;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := nil;
end;

function TWinRTBluetoothGattServer.DoGetServices: TBluetoothGattServiceList;
begin
  raise EBluetoothServiceException.Create(SBluetoothNotImplemented);
  Result := nil;
end;

function TWinRTBluetoothGattServer.DoIsAdvertising: Boolean;
begin
  Result := FAdvertismentPublisher.Status = BluetoothLEAdvertisementPublisherStatus.Started
end;

procedure TWinRTBluetoothGattServer.DoCharacteristicReadRequest(const ADevice: System.Bluetooth.TBluetoothLEDevice; ARequestId: Integer; 
  AnOffset: Integer; const AGattCharacteristic: TBluetoothGattCharacteristic);
begin
  // Not implemented
end;

procedure TWinRTBluetoothGattServer.DoCharacteristicWriteRequest(const ADevice: System.Bluetooth.TBluetoothLEDevice; ARequestId: Integer;
  const AGattCharacteristic: TBluetoothGattCharacteristic; APreparedWrite: Boolean; AResponseNeeded: Boolean;
  AnOffset: Integer; const AValue: TBytes);
begin
  // Not implemented
end;

procedure TWinRTBluetoothGattServer.DoUpdateCharacteristicValue(const ACharacteristic: TBluetoothGattCharacteristic);
begin
  // Not implemented
end;

procedure TWinRTBluetoothGattServer.DoServiceAdded(AStatus: TBluetoothGattStatus; const AService: TBluetoothGattService);
begin
  // Not implemented
end;

procedure TWinRTBluetoothGattServer.DoStartAdvertising;
var
  LManufacturerData: IBluetoothLEManufacturerData;
  LRawManufacturerData: TBytes;
  LCompanyID: Word;
begin
  inherited;
  if IsAdvertising then
    StopAdvertising;

  LRawManufacturerData := AdvertiseData.ManufacturerSpecificData;
  FAdvertismentPublisher.Advertisement.ManufacturerData.Clear;
  if Length(LRawManufacturerData) >= 2  then
  begin
    LManufacturerData := TBluetoothLEManufacturerData.Create;
    Move(LRawManufacturerData[0], LCompanyId, SizeOf(Word));
    LManufacturerData.CompanyId := LCompanyID;
    if Length(LRawManufacturerData) >= 3  then
      LManufacturerData.Data := BytesToIBuffer(LRawManufacturerData, 2);
    FAdvertismentPublisher.Advertisement.ManufacturerData.Append(LManufacturerData);
    FAdvertismentPublisher.Start;
  end
  else
    raise EBluetoothLEAdvertiseDataException.Create(SBluetoothLEAdvertisementEmpty);

end;

procedure TWinRTBluetoothGattServer.DoStopAdvertising;
begin
  if IsAdvertising then
    FAdvertismentPublisher.Stop;
end;

end.
