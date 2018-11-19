unit SimpleBLEIOConnection;
{$INCLUDE 'DelphiDefs.inc'}

interface

uses
  SimpleAbstractconnection, blex, ringbuffer, system.Bluetooth, classes,debug,sysutils, signals,
{x$DEFINE SYNC}
{$IFNDEF SYNC}
  Bluetoothcomponentthreaded,
{$ELSE}
  System.Bluetooth.Components,
{$ENDIF}
{$IFDEF FMX}
  fmx.forms,
{$ELSE}
  forms,
{$ENDIF}
  simplemidibase, typex, tickcount, numbers, systemx;

type
  TMIDIMessageTracker = packed record
    sz: ni;
    datact: ni;
    data: array[0..256] of byte;

  end;

  TPDU = record
    data: array[0..19] of byte;
    len: ni;
    inSysex: boolean;
    newesttimestamp: ticker;
  end;

  TIncomingMessage = record
    data: TArray<byte>;
  end;


  TSimpleBleIOconnection = class(TSimpleAbstractConnection)
  private
    FService: TBluetoothUUID;
    FCharacteristic: TBluetoothUUID;
  strict protected
    function DoCheckForData: Boolean; override;
  protected
    FEnumerator: blex.TBLEEnumerator;
    incomingdata: array[0..2047] of TIncomingMessage;
    incomingWritePtr: ni;
    incomingReadPtr: ni;
    evData: TSignal;
    procedure AdvanceDataPtr(var val: ni);


    procedure EnumeratorOnREad(const ACharacteristic: TBluetoothGattCharacteristic);


  public
    procedure Init;override;
    procedure Detach;override;

    function GetConnected: boolean;override;
    function DoReadData(buffer: pbyte; len: integer): integer;override;
    function DoSendData(buffer: pbyte; len: integer): integer;override;

    function DoWaitForData(timeout: cardinal): boolean;override;

    function DoConnect: boolean;override;
    procedure DoDisconnect;override;
    property Service: TBluetoothUUID read FService write FService;
    property Characteristic: TBluetoothUUID read FCharacteristic write FCharacteristic;

  end;

implementation

{ TSimpleBleMIDIconnection }

procedure TSimpleBleIOconnection.AdvanceDataPtr(var val: ni);
begin
  inc(val);
  if val >= Length(incomingdata) then
    val := 0;


end;

function TSimpleBleIOconnection.DoConnect: boolean;
var
  dev: TBluetoothLEDevice;
  chr: TBluetoothGattCharacteristic;
  gattsvc: TBluetoothGattService;
  tmStart: ticker;
begin
  if not (GetCurrentThreadID = MainThreadID) then
    raise ECritical.create(self.classname+' can only connect from Main Thread');
  result := Connected;

  if result then
    exit;

  fEnumerator.ScanTime := 12000;
  FEnumerator.ChooseDevice := HostName;
  FEnumerator.ChooseService := Service;
  FEnumerator.Choosecharacteristic := Characteristic;
  FEnumerator.Connect_Sync;
  result := true;





  tmStart := GetTicker;
  while GEtTimeSince(tmStart) < 120000 do
  begin
    application.ProcessMessages;

    if not FEnumerator.Discovering then
      break;

    if FEnumerator.Connected then
      break
    else
      sleep(100);
  end;


  result := Connected;




end;

procedure TSimpleBleIOconnection.Detach;
begin
  if detached then exit;


  FEnumerator.free;
  FEnumerator := nil;

  evData.free;
  evData := nil;

  inherited;

end;

procedure TSimpleBleIOconnection.DoDisconnect;
begin

  inherited;

end;

function TSimpleBleIOconnection.DoCheckForData: Boolean;
begin
  raise ECritical.create(self.ClassName+' is not intended for data polling.');
end;

function TSimpleBleIOconnection.DoReadData(buffer: pbyte;
  len: integer): integer;
var
  dat: TArray<byte>;
begin
  Lock;
  try
    if incomingReadPtr = incomingWritePtr then
      exit(0);
    dat := incomingdata[incomingReadPtr].data;
    advancedataptr(incomingReadPtr);
    result := lesserof(len, length(dat));
    movemem32(buffer, @dat[0], result);
    if incomingReadPtr = incomingWritePtr then
      Signal(evData, false);

    if len < length(dat) then
      raise ECritical.create('when reading '+self.ClassName+' you must read an entire packet at once (typically >= 20 bytes)');
  finally
    Unlock;
  end;
end;

function TSimpleBleIOconnection.DoSendData(buffer: pbyte;
  len: integer): integer;
var
  byts: Tarray<byte>;
  x: ni;
begin
  Lock;
  try
  len := lesserof(len, 20);
  result := len;
  setlength(byts, len);

  x := 0;
  while len > 0 do begin
    byts[x] := buffer[x];
    dec(len);
    inc(x);
  end;
  //movemem32(@byts[0], buffer, len);
  FEnumerator.WriteToSubscribedCharacteristic(byts);
  finally
    Unlock;
  end;



end;

function TSimpleBleIOconnection.DoWaitForData(timeout: cardinal): boolean;
begin
  result := false;
  if incomingWritePtr <> incomingreadptr then
    exit(true);

  result := WaitforSignal(evData, timeout);
end;

procedure TSimpleBleIOconnection.EnumeratorOnREad(
  const ACharacteristic: TBluetoothGattCharacteristic);
begin
  lOCK;
  TRY
    setlength(incomingdata[incomingWritePtr].data, length(acharacteristic.value));
    movemem32(@incomingdata[incomingWritePtr].data[0], @acharacteristic.value[0], length(acharacteristic.value));
//    incomingdata[incomingWritePtr].data := acharacteristic.Value;
    AdvanceDataPtr(incomingwriteptr);
{$IFDEF DEBUGBLEIO}
    debug.log('incoming write ptr='+inttostr(incomingwriteptr));
{$ENDIF}
    signal(evData, true);
  FINALLY
    unlock;
  end;

end;

function TSimpleBleIOconnection.GetConnected: boolean;
begin
  result := FEnumerator.connected;
end;

procedure TSimpleBleIOconnection.Init;
begin
  inherited;
  evData := TSignal.create;
  FEnumerator := TBLEEnumerator.create;

  FEnumerator.OnRead := self.EnumeratorOnRead;

  incomingWritePtr := 0;
  incomingReadPtr := 0;

end;


end.
