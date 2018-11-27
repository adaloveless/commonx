unit SimpleBLEMIDIConnection;

interface


//[ ] When receiving data... work out receive function
//      --- throw out header (it just has a useless time stamp
//      --- read next byte... var the pointers!
//      --- interpret each message with the following rules
//            --- if high bit is set, it could be either a TIMESTAMP or a RUNNING STATUS
//            --- basically, I think the rule is to save the timestamp temporarily
//            --- count number of high bits since last low bit
//            --- if count < 2 then byte was a timestamp
//            --- if count = 2 then byte was a status byte




uses
  SimpleAbstractconnection, ringbuffer, system.Bluetooth, simplemidibase, typex, tickcount, simplebleioconnection, midiconsts, systemx, debug, stringx;

const
  BLE_MIDI_CHAR : TBluetoothUUID =      '{7772E5DB-3868-4112-A1A9-F2669D106BF3}';
  BLE_JS_CONFIG_CHAR : TBluetoothUUID = '{5E9BF2A8-F93F-4481-A67E-3B2F4A07891A}';
  BLE_MIDI_SERVICE: TBluetoothUUID = '{03B80E5A-EDE8-4B33-A751-6CE34EC4C700}';

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
    procedure Reset;
  end;


  TSimpleBleMIDIconnection = class(TSimpleMIDIBase)
  strict protected
    function DoCheckForData: Boolean; override;
  protected
    FUnderClass: TSimpleBLEIOConnection;

    fifoin: TRingBuffer;
    fifoout: TRingBuffer;

    inRunningStatus: byte;
    highbytes: array[0..19] of byte;/// for tracking timestamps vs running statuses
    highbyteidx: ni;

    pdu: TPDU;
    outmsg: TMIDIMessageTracker;

    procedure RecoverFromFailedSysex;
    procedure midi_start_packet;
    procedure midi_add_nonSysex_Bytes_to_packet(p:pbyte; len:ni);
    procedure midi_add_sysex_byte_to_packet(data: byte);

    procedure CommitPDU;

    procedure PacketizeOutgoingFifo;
    procedure PacketizeRealtimeCase(bufbyte: byte);

    procedure EnumeratorOnREad(const ACharacteristic: TBluetoothGattCharacteristic);

    procedure PumpDataFromUnderClass;
    procedure InterpretBLEMIDIEvent(var pb: Pbyte; var len_remaining: ni);
    procedure RemoveTimeStamps(var pb: Pbyte; var len_remaining: ni);

    function SendAsBleMIDI(buffer: pbyte; length: ni): ni;
    function DoReadData(buffer: pbyte; length: integer): integer;override;
    function DoSendData(buffer: pbyte; length: integer): integer;override;
    function DoWaitForData(timeout: cardinal): boolean;override;


  public
    procedure Init;override;
    procedure Detach;override;

    function GetConnected: boolean;override;


    function DoConnect: boolean;override;
    procedure DoDisconnect;override;
    procedure CleanupUnderclass;
    procedure ResetHighbytes;
    procedure CommitHighbyte(b: byte);

  end;

implementation

{ TSimpleBleMIDIconnection }

procedure TSimpleBleMIDIconnection.CleanupUnderclass;
begin
  FUnderclass.free;
  FUnderclass := nil;
end;

procedure TSimpleBleMIDIconnection.CommitHighbyte(b: byte);
begin
  highbytes[highbyteidx] := b;
  inc(highbyteidx);
end;

procedure TSimpleBleMIDIconnection.CommitPDU;
begin
  if (pdu.len) <= 0 then
    exit;
  Log('Committing PDU : '+memorytostring(@pdu.data[0], pdu.len));

  FUnderclass.Lock;
  try
    FUnderClass.SendData(@pdu.data[0], pdu.len, true);
  finally
    FUnderClass.unlock;
  end;
  pdu.reset;

end;

function TSimpleBleMIDIconnection.DoConnect: boolean;
var
  dev: TBluetoothLEDevice;
  chr: TBluetoothGattCharacteristic;
  gattsvc: TBluetoothGattService;
begin
  if self.hostname = '' then
    raise ECritical.create('host name is blank.  '+self.classname+' uses host-name for MAC of target, not "endpoint"');

  result := false;
  if FUnderclass <> nil then
    result := FUnderclass.connected;

  if result then
    exit;

  CleanupUnderClass;

  FUnderClass := TSimpleBleIOconnection.create;

  FUnderclass.HostName := self.HostName;
  FUnderclass.Service := BLE_MIDI_SERVICE;
  FUnderClass.Characteristic := BLE_MIDI_CHAR;
  result := FUnderClass.Connect;





end;

procedure TSimpleBleMIDIconnection.Detach;
begin
  if detached then exit;

  CleanupUnderclass;

  fifoout.free;
  fifoout := nil;
  fifoin.free;
  fifoin := nil;


  inherited;

end;

procedure TSimpleBleMIDIconnection.DoDisconnect;
begin
  inherited;

  if assigned(FUnderclass) then begin
    CleanupUnderclass;
  end;

end;

function TSimpleBleMIDIconnection.DoCheckForData: Boolean;
begin
  raise ECritical.create(self.ClassName+' is not intended for data polling.');
end;

function TSimpleBleMIDIconnection.DoReadData(buffer: pbyte;
  length: integer): integer;
begin
  PumpDataFromUnderClass;
  result := fifoin.GetAvailableChunk(buffer, length);
end;

function TSimpleBleMIDIconnection.DoSendData(buffer: pbyte;
  length: integer): integer;
begin
  result := SendAsBleMIDI(buffer, length);

end;

function TSimpleBleMIDIconnection.DoWaitForData(timeout: cardinal): boolean;
begin
  if assigned(FUnderClass) then
    result := Funderclass.WaitForData(timeout)
  else
    result := false;

end;

procedure TSimpleBleMIDIconnection.EnumeratorOnREad(
  const ACharacteristic: TBluetoothGattCharacteristic);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TSimpleBleMIDIconnection.GetConnected: boolean;
begin
  if FUnderclass = nil then
    exit(false);
  result := FUnderclass.connected;
end;

procedure TSimpleBleMIDIconnection.Init;
begin
  inherited;
  FUnderclass := nil;

  fifoin := TRingBuffer.Create;
  fifoin.Size := 65536;

  fifoout := TRingBuffer.create;
  fifoout.size := 65536;


end;

procedure TSimpleBleMIDIconnection.InterpretBLEMIDIEvent(var pb: Pbyte;
  var len_remaining: ni);
var
  iSize: ni;
  b: byte;
begin
  inc(pb);//first byte is ALWAYS timestamp, throw it away
  //if the next byte has high-bit set then it is
  //a new status, save it as the running status

  b := pb^;
  if MidiIsStatus(b) then begin
    iSize := midiconsts.MidiMessageSize(b);

  end;



end;

procedure TSimpleBleMIDIconnection.midi_add_nonSysex_Bytes_to_packet(p: pbyte;
  len: ni);
begin
//static UINT16 timestamp;
//BLEPROFILE_DB_PDU *db_pdu = &(db_pdu_to_send_shared[destination]);

//if (db_pdu->len > 20 - 1 - len) // 20 - 1 byte timestamp - len
  //midi_queue_message_for_sending(destination);
  if (pdu.len) > (20 -1 -len) then
    commitpdu;


//if (db_pdu->len == 0) {
  //timestamp = GetTimestamp();
  //midi_start_packet(destination, timestamp);
//}
  if (pdu.len = 0) then begin
    pdu.newesttimestamp := getticker;
    midi_start_packet();
  end;

//db_pdu->pdu[db_pdu->len++] = 0x80 | (timestamp & 0x7F); // timestamp
  pdu.data[pdu.len] := $80;
  inc(pdu.len);

//while (len--)
  //db_pdu->pdu[db_pdu->len++] = *(data++);
  while len > 0 do begin
    pdu.data[pdu.len] := p^;
    inc(p);
    inc(pdu.len);
    dec(len);
  end;

//if (db_pdu->len == 20) // buffer full
  //midi_queue_message_for_sending(destination);

  if (pdu.len = 20) then
    commitpdu;
end;

procedure TSimpleBleMIDIconnection.midi_start_packet;
begin
  //	BLEPROFILE_DB_PDU *db_pdu = &(db_pdu_to_send_shared[destination]);
  //	db_pdu->pdu[0] = 0x80 | ((timestamp >> 7) & 0x3F); // header
  pdu.data[0] := $80 or ((GetTicker() shr 7) and $3f);


  //	db_pdu->len = 1;
  pdu.len := 1;


end;

procedure TSimpleBleMIDIconnection.midi_add_sysex_byte_to_packet(data: byte);
begin

    //	static UINT8 inSysEx = 0;
    //	static UINT16 timestamp;
    //	BLEPROFILE_DB_PDU *db_pdu = &(db_pdu_to_send_shared[destination]);
    //
    //
    //	if ((!inSysEx) && (data == 0xF0)) {
    if (not pdu.inSysex) and (data = $f0) then begin

      //		inSysEx = 1;
      pdu.inSysex := true;

      //		timestamp = GetTimestamp();
      pdu.newesttimestamp := GetTicker();

      //		if (db_pdu->len > 18) // need room for 2 bytes
      //			midi_queue_message_for_sending(destination);
      if (pdu.len > 18) then
        CommitPDU;



      //		if (db_pdu->len == 0)
      //			midi_start_packet(destination, timestamp);
      if pdu.len = 0 then
        midi_start_packet;

      //		db_pdu->pdu[db_pdu->len++] = 0x80 | (timestamp & 0x7F); // timestamp
      pdu.data[pdu.len] := $80 or (pdu.newesttimestamp and $7f);
      inc(pdu.len);

      //		db_pdu->pdu[db_pdu->len++] = 0xF0; // SysEx start
      pdu.data[pdu.len] := $f0;
      inc(pdu.len);


    //	} else if (inSysEx) {
    end else if (pdu.inSysex) then begin

      //		if (data & 0x80) // SysEx end, or other 1-byte system message
      //				{
      if (data and $80) <> 0 then begin

        //			if (db_pdu->len > 18) // need room for 2 bytes
        //				midi_queue_message_for_sending(destination);
        if (pdu.len > 18) then
          CommitPDU();

        //			if (db_pdu->len == 0)
        //				midi_start_packet(destination, timestamp);
        if (pdu.len = 0) then
          midi_start_packet();


        //			db_pdu->pdu[db_pdu->len++] = 0x80 | (timestamp & 0x7F); // timestamp
        pdu.data[pdu.len] := $80 or (pdu.newesttimestamp and $7f);
        inc(pdu.len);

        //			db_pdu->pdu[db_pdu->len++] = data;
        pdu.data[pdu.len] := data;
        inc(pdu.len);

        //			if (data == 0xF7)
        //				inSysEx = 0;
        if (data = $f7) then
          pdu.inSysex := false;

      //		} else // sysex data byte
      end else

      //		{
      begin
        //			if (db_pdu->len > 19) // need room for 1 bytes
        //				midi_queue_message_for_sending(destination);
        if (pdu.len > 19) then
          commitpdu;

        //
        //			if (db_pdu->len == 0)
        //				midi_start_packet(destination, timestamp);
        if (pdu.len = 0) then
          midi_start_packet;


        //
        //			db_pdu->pdu[db_pdu->len++] = data; // SysEx data byte
        pdu.data[pdu.len] := data;
        inc(pdu.len);

      end;
    //		}
    //
    //	}
    end;


    //	 //else we're not in a sysex and it wasn't a start byte
    //	if (db_pdu->len == 20) // buffer full
    //		midi_queue_message_for_sending(destination);

    if (pdu.len = 20) then
      commitpdu;

end;

procedure TSimpleBleMIDIconnection.PacketizeOutgoingFifo;
var
  i: ni;
  bufByte: byte;
begin
//static UINT16 mCurrentMsgSz = 0; // total number of bytes in the current message
//static UINT16 mCurrentDataCt = 0; // the number of bytes received so far in the current message
//static UINT8 mCurrentData[ZX_MIDIPROCESS_CMDBUFSIZE + 2] = { 0 };
//UINT8 i, bufByte;


//{
  // empty the FIFO
  //while (puart_rxFifoNotEmpty() && puart_read(&bufByte)) {
  //	{

  while (fifoout.AvailableDataSize > 0) do
  begin
    fifoout.GetByte(bufByte);

    //if ((bufByte & 0x80) != 0) // status message
        //{
    if ((bufByte and $80) <> 0) then begin
      //switch (bufByte & 0xF0) {
      case(bufByte and $f0) of

        // three byte messages
        //case 0x80:
        //case 0x90:
        //case 0xA0:
        //case 0xB0:
        //case 0xE0:
        $80, $90, $a0, $b0, $e0:
        begin
          //RecoverFromFailedSysEx(currentDestination); // we've received a character not allowed in a sys-ex. Assume we missed the end of sys-ex and recover
          //????

          //if (mCurrentData[0] != 0xF0) // if not in a sysex message
          //{
            //mCurrentData[0] = bufByte; // set status message
            //mCurrentDataCt = 1;
            //mCurrentMsgSz = 3;
          //}
          if (outmsg.data[0]<>$f0) then
          begin
            outmsg.data[0] := bufByte;
            outmsg.datact := 1;
            outmsg.sz := 3;
          end;
          //break;
        end;
        // two byte messages
        //case 0xC0:
        //case 0xD0:
        $c0, $d0:
        begin
          //RecoverFromFailedSysEx(currentDestination); // we've received a character not allowed in a sys-ex. Assume we missed the end of sys-ex and recover
          //?????

          //if (mCurrentData[0] != 0xF0) // if not in a sysex message
          //{
            //mCurrentData[0] = bufByte; // set status message
            //mCurrentDataCt = 1;
            //mCurrentMsgSz = 2;
          //}
          if (outmsg.data[0] <> $f0) then
          begin
            outmsg.data[0] := bufByte;
            outmsg.datact := 1;
            outmsg.sz := 2;
          end;
          //break;
        end;
        // system messages
        //case 0xF0: {
        $f0:
        begin
        //}
          PacketizeRealtimeCase(bufbyte);
        end
      end;

      //} else // data byte
    end
    else begin
        //if (mCurrentData[0] == 0xF0) // sysEx
        //{
        if outmsg.data[0] = $f0 then
        begin
          //if (mCurrentDataCt < mCurrentMsgSz)
            //mCurrentData[mCurrentDataCt++] = bufByte;
          if (outmsg.datact < outmsg.sz) then begin
            outmsg.data[outmsg.datact] := bufbyte;
            inc(outmsg.datact);
          end;
          //midi_add_sysex_byte_to_packet(currentDestination,								bufByte);
          midi_add_sysex_byte_to_packet(bufByte);

        //} else if (mCurrentData[0] != 0) // has a current status
        end else if (outmsg.data[0] <> 0) then
        //{
        begin
          //if (mCurrentDataCt < mCurrentMsgSz)
            //mCurrentData[mCurrentDataCt++] = bufByte;
          if (outmsg.datact < outmsg.sz) then begin
            outmsg.data[outmsg.datact] := bufByte;
            inc(outmsg.datact);
          end;

          //if (mCurrentDataCt >= mCurrentMsgSz) {
          if (outmsg.datact >=  outmsg.sz) then begin
            //midi_add_nonSysex_bytes_to_packet(currentDestination, mCurrentData, mCurrentDataCt);
            midi_add_nonsysex_bytes_to_packet(@outmsg.data[0], outmsg.datact);

            //if (sendImmediately)
              //midi_queue_message_for_sending(currentDestination);
            CommitPDU;

            //mCurrentDataCt = 1; // keep running status byte
            outmsg.datact := 1;
          end
          //}
        end;
    end;
    //}
  end;
  //}

  CommitPDU;

end;

procedure TSimpleBleMIDIconnection.PacketizeRealtimeCase(bufbyte: byte);
begin
  case bufByte of
    //switch (bufByte) {
    //case 0xF0: // start of sysex msg
    $f0: begin
      RecoverFromFailedSysex;
      //RecoverFromFailedSysEx(currentDestination); // we've received a character not allowed in a sys-ex. Assume we missed the end of sys-ex and recover
      //if (mCurrentData[0] != 0xF0) // if not currently in a sysex message
      //{
        //mCurrentData[0] = bufByte; // set status message
        //mCurrentDataCt = 1;
        //mCurrentMsgSz = 256;
        //midi_add_sysex_byte_to_packet(
            //currentDestination, bufByte);
      //}

      if (outmsg.data[0] <> $f0) then begin
        outmsg.data[0] := bufByte;
        outmsg.datact := 1;
        outmsg.sz := 256;
        midi_add_sysex_byte_to_packet(bufByte);
      end;
      //break;
    end;
    //case 0xF7: // end of sysex
    //{
    $f7:
    begin
      //if (mCurrentData[0] == 0xF0) // if currently in a sysex message
          //{
        //midi_add_sysex_byte_to_packet(currentDestination, bufByte);
        //if (sendImmediately)
          //midi_queue_message_for_sending(currentDestination);
        //processSysEx(mCurrentData, mCurrentDataCt);
        //mCurrentData[0] = 0;
        //mCurrentDataCt = 0;
        //mCurrentMsgSz = 0;
      //}

      if (outmsg.data[0] = $f0) then begin
        midi_add_sysex_byte_to_packet(bufByte);
        commitpdu;
        outmsg.data[0] := 0;
        outmsg.datact := 0;
        outmsg.sz := 0;
      end;
      //break;
    //}
    end;
    //case 0xF6: // 1 byte system messages
    //case 0xF8:
    //case 0xFA:
    //case 0xFB:
    //case 0xFC:
    //case 0xFE:
    //case 0xFF:{
    $f6,$f8, $fa,$fb,$fc, $fe, $ff:
    begin
      //midi_add_nonSysex_bytes_to_packet(currentDestination, &bufByte, 1);
      midi_add_nonsysex_bytes_to_packet(@bufByte, 1);

      //if (sendImmediately)
        //midi_queue_message_for_sending(
            //currentDestination);
      commitpdu;

      //break;
    //}
    end;
    //case 0xF1: // 2 byte system messages
    //case 0xF3: {
      //mCurrentData[0] = bufByte;
      //mCurrentDataCt = 1;
      //mCurrentMsgSz = 2;
      //break;
    //}
    $f1, $f3:
    begin
      outmsg.data[0] := bufByte;
      outmsg.datact := 1;
      outmsg.sz := 2;

    end;
    //case 0xF2: // 3 byte system message
    //{
      //mCurrentData[0] = bufByte;
      //mCurrentDataCt = 1;
      //mCurrentMsgSz = 3;
      //break;
    //}
    $f2: begin
      outmsg.data[0] := bufByte;
      outmsg.datact := 1;
      outmsg.sz := 3;
    end;
    //}
    //break;
  //}
  else
    //default case
  end;

end;


procedure TSimpleBleMIDIconnection.PumpDataFromUnderClass;
var
  dat: array[0..19] of byte;
  iGot: ni;
  pb: Pbyte;
  len: ni;
begin
  FUnderclass.lock;
  try
    iGot :=  FUnderClass.ReadData(@dat[0], length(dat));
    if iGot = 0 then
      exit;


    //interpret packet

    pb := @dat[0];
    len := iGot;

    RemoveTimeStamps(pb, len);


    if fifoin.SpaceAvailable >= len then begin
      fifoin.BufferData(pb, len);
    end;
  finally
    FUnderclass.unlock;
  end;

end;

procedure TSimpleBleMIDIconnection.RecoverFromFailedSysex;
begin
  //
end;

procedure TSimpleBleMIDIconnection.RemoveTimeStamps(var pb: Pbyte;
  var len_remaining: ni);
//IN A NUTSHELL
//We don't want timestamps.  We do stuff real-time
//The only thing that really differentiates BLE-MIDI from standard MIDI is that
//there is a 1-byte header on each packet
//and there's this whole idea of timestamps (useless IMO).
// Basically the rule is that
//   - any status byte has to be preceded by a timestamp byte
//   - timeStamp bytes and status bytes BOTH have high bits set
//   - since they both have the high-bit set, to remove the timestamp
//   - we just have to remove any cases where there are two bytes in a row with
//     high bits set.   We throw out the first and keep the second
//   - We KEEP ALL DATA that does not have a high-bit set
//   - SYSEX messages are basically two endcaps (with high-bits and timestamps)
//     with low-bit bytes in the middle... so this works out nicely for
//     parsing sysex messages as well.



var
  tmp: array[0..19] of byte;
  t: ni;
  outidx: ni;

  procedure CommitByte(b: byte);
  begin
    pb[outidx] := b;
    inc(outidx);
  end;

begin



  if len_remaining < 2 then
    exit;//this should not be allowed

  movemem32(@tmp[0], pb, len_remaining);
  outidx := 0;




  //if byte[1]'s high bit is not set, then this is a SYSEX continuation
  if not midiisstatus(tmp[1]) then begin
    t := 1;//start parsing at index 1 for sysex continuations
  end else
    t := 2;//start parsing at index 2 for regular packets (idx[1] should contain a timestamp)



  //remove the first byte in any cases where there are two bytes in a row with their high-bit set
  while t < (len_remaining) do begin
    //if this byte and previous byte have high bit set
    //then the valuable data is the second one
    if not (midiisstatus(tmp[t])) then begin
      CommitByte(pb[t]);
      inc(t);
    end else
    if (MidiIsStatus(tmp[t-1])) and (MidiIsStatus(tmp[t])) then begin
      CommitByte(pb[t]);
      inc(t);
    end else
      inc(t);
  end;

  Debug.Log(self, MemoryDebugString(pb, outidx));
  len_remaining := outidx;

end;

procedure TSimpleBleMIDIconnection.ResetHighbytes;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TSimpleBleMIDIconnection.SendAsBleMIDI(buffer: pbyte; length: ni): ni;
begin
  result := fifoout.BufferData(buffer, length);
  while fifoout.AvailableDataSize > 0 do begin
    PacketizeOutgoingFifo;
  end;
end;

{ TPDU }

procedure TPDU.Reset;
begin
  len := 0;
end;

end.
