unit zx_firmware;

interface

uses
  typex, systemx, simpleabstractconnection, stringx, sharedobject, classes, tickcount, datahandler, hexconvert, beeper, speech, debug, sysutils, endian, numbers, Compressor_Simple;

type
  TZXFirmwareHandler = class(TSharedObject)
  private
    FFirmwareupdateMOde: boolean;
    slHex: TStringList;
    slHexAdv: TStringlist;
    FCheckCount: int64;
    CompressionConfirmed: boolean;
    FirmwareResult: string;
    CompressionMode: ni;

    procedure BeginCalMessage(b: byte);
    procedure CalMessage(b: byte);
    procedure EndCalMessage(b: byte);
    procedure BeginOutputChecksumming;
    function EndOutputchecksumming: integer;


    procedure SetFirmwareUpdateMode(const Value: boolean);
    procedure SendHexLine(iLineNumber: fi; bCompress: boolean; bAltFile: boolean);
  public
    conn: IDataHandler;
    prog: TVolatileProgression;
    Firmware_TimeOfLastNudge, Firmware_TimeOfLastAck: ticker;
    Firmware_LinesPerSecond: fi;
    Firmware_LastLinesPerSecond: fi;
    LastStatUpdate: ticker;

    procedure EnterFirmWareUpdateMode(sHexFile: string);

    function Interpret_Protocol0_Firmware_Sysex(lastbyte: byte): boolean;

    procedure TXBufferAs7Bit(p: pbyte; len: ni);
    procedure EValuateFirmwareUpdateTimers;
    procedure Init; override;
    destructor Destroy; override;
    property FirmWareupdateMode: boolean read FFirmwareupdateMOde write SetFirmwareUpdateMode;
    procedure ResetFirmwareStats;
    procedure LockComms;
    procedure UnlockComms;
    procedure ZXMIDIHEader;
    procedure SetFirmwareUpdateParams(iCompressMode, iLineBatchSize, iPrefetchTimer: ni);
    function FirmwareStatusMessage: string;
  end;

implementation

{ TZXFirmwareHandler }

procedure TZXFirmwareHandler.BeginCalMessage(b: byte);
begin
//  conn.LockComms;
  conn.GuaranteeSendData(@b, 1);
end;

procedure TZXFirmwareHandler.BeginOutputChecksumming;
begin
//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
  FCheckCount := 0;
end;

procedure TZXFirmwareHandler.CalMessage(b: byte);
begin
  conn.GuaranteeSendData(@b, 1);
  inc(FCheckCount);
end;

destructor TZXFirmwareHandler.Destroy;
begin
  slHex.free;
  slHexAdv.free;

  inherited;
end;

procedure TZXFirmwareHandler.EndCalMessage(b: byte);
begin
  try
    conn.GuaranteeSendData(@b, 1);
  finally
    conn.Flush;
//    conn.unlockComms;
  end;

end;

function TZXFirmwareHandler.EndOutputchecksumming: integer;
begin
  //result := $55;
  result := FCheckCount;
end;


procedure TZXFirmwareHandler.EnterFirmWareUpdateMode(sHexFile: string);
var
  cc: byte;
  len: cardinal;
  len2: cardinal;
begin

  LockComms;
  try
    slHex.LoadFromFile(sHexFile);
    slHexAdv.text := HextoZXHex(slHEx.text, 2048*2);
    Firmware_TimeOfLastAck := GEtTicker;
    FirmWareResult := 'Starting Firmware Update';

    //--------------------------------------

    FirmWareUpdateMode := true;
    BeginCalMessage($F0);
    BeginOutputCheckSumming;
    ZXMIDIHeader();
    CalMessage($50);//CMD
    CompressionConfirmed := false;
    if compressionMode >=2 then
      len := slHexAdv.count
    else
      len := slHex.count;
    TXBufferAs7Bit(@len, 4);


    CalMessage(EndoutputChecksumming);
    ENdCalMessage($F7);

  finally
    UnlockComms;
  end;
end;

procedure TZXFirmwareHandler.EValuateFirmwareUpdateTimers;
begin
  if not firmwareupdatemode then exit;

  if (GetTimeSince(Firmware_TimeOfLastAck) > 16000) then begin
    FirmWareUpdateMode := false;
    beeper.beepchord([100,90,144], 500, 50, 400);
    FirmwareResult := 'Timed out, messages from device ceased.';
    SayNatural('Firmware update Timed Out');
  end;

  IF GetTimeSince(LastStatUpdate) >= 1000 then begin
    Firmware_LastLinesPerSecond := Firmware_LinesPerSecond;
    Firmware_LinesPerSecond := 0;
    LastStatUpdate := GetTicker;
  end;

end;

function TZXFirmwareHandler.FirmwareStatusMessage: string;
begin
  result :=
    'Updating Firmware... Compression:'+booltostrex(CompressionConfirmed,'On', 'Off')+' '+
      inttostr(Firmware_LastLinesPerSecond)+
      ' LPS Line: '+inttostr(prog.StepsCompleted)+' of '+inttostr(prog.TotalSteps);
end;

procedure TZXFirmwareHandler.Init;
begin
  inherited;
  slHex := TStringlist.create;
  slHexAdv := TStringlist.create;
end;

function TZXFirmwareHandler.Interpret_Protocol0_Firmware_Sysex(lastbyte: byte): boolean;
const
  FRET_MSG_CHECKSUM_BYTE_COUNT = 1;
var
  b,bb: byte;
  bIsHot: boolean;
  f,s: byte;
  ss: array [0 .. 6] of byte;
  sss: array [0 .. 120] of byte;
  fret_extended: array[0..6] of byte;
  fivebyte: array [0..4] of byte;
  iTemp, iTemp2,t, u, v, uu: nativeint;
  rr: NativeFloat;
  gain_level: nativeint;
  checksum: byte;
  sxl: ansistring;
  precision: byte;
  bbyte: byte;
  line: cardinal;
  idx: nativeint;
  xsz: nativeint;
  cal: array[0..((5*6)-1)] of byte;
  bExtendedData: boolean;
  ssss: string;
  cmd: ni;
  prot: byte;
  bCompressed: boolean;
  bUseAdvHex: boolean;
  slUsedHex: TStringlist;
const
  stringmap: array [0 .. 5] of nativeint = (0,1,2,3,4,5);
  ddd = ',';

  fields = 7;
begin
  result := false;
  if lastbyte > 0 then
    b := lastbyte
  else
    conn.GuaranteeReadData(@b, 1);
  t := 0;

  cmd := b;
  case cmd of
    $51,$57,$5A: begin
      if not FirmwareUpdateMode then
        exit;

      if cmd = $5a then
        CompressionConfirmed := true;
      bCompressed := (cmd = $57) or (cmd=$5a);

      if (cmd<>$5a) and CompressionConfirmed then begin
        Debug.ConsoleLog('Compression mode '+inttostr(compressionmode)+' doesn''t support 0x'+inttohex(cmd,2));
//        FirmWareupdateMode := false;
        exit;
      end;

      bUseAdvHex := (cmd=$5a);
      if bUseAdvHex then
        slUsedHex := slHexAdv
      else
        slUsedHex := slHex;


      //Debug.consolelog('CMD: 0x'+inttohex(b,2)+' is FW Line Batch Request');
      conn.GuaranteeReadData(@fivebyte[0], 5);
      Convert7BitsTo8Bits(@fivebyte[0], @line, 5);

      EndianSwap(@line, 4);
      //Debug.consolelog('LIne '+inttostr(line));

      conn.GuaranteeReadData(@b, 1);
      iTemp := b;
//      debug.consolelog('Line :'+inttostr(line)+' Count:'+inttostr(iTemp));
      conn.GuaranteeReadData(@b, 1);
      if (b <> $55) and (b <> 10) then
        Debug.consolelog('throwing out command, parity invalid')
      else begin
        //Debug.consoleLog('Line:'+inttostr(line));
        if not FirmWareupdateMode then
          exit;

        //Debug.ConsoleLog(inttostr(iTemp)+':'+inttostr(line));
        if (int64(line) < slUsedHex.count) then begin
          Firmware_TimeOfLastAck := getTicker;

          for t := 0 to iTemp-1 do begin
            sendhexline(fi(line)+fi(t), (bCompressed) and (ni(line)>ni(prog.StepsCompleted)),bUseAdvHex);
          end;
          prog.StepsCompleted := line;
          prog.TotalSteps := slUsedHex.Count-1;
        end else begin
          //beeper.beeparray([1000,900,1000,900,1000,900],[50,50,50,50,50,50], 0,0);
        end;
      end;
    end;
    $54: begin
      conn.GuaranteeReadData(@b, 1);
      SayNatural('Firmware update complete code '+inttostr(b));
      if FirmwareUpdateMode then
        beeper.beepchord([300,600,900], 1000, 300, 300);
      FirmwareUpdateMode := false;

    end;
  else
    Debug.log(self,'No Handler for CMD '+inttohex(b,2));

  end;

end;


procedure TZXFirmwareHandler.LockComms;
begin
  conn.LockComms;

end;

procedure TZXFirmwareHandler.ResetFirmwareStats;
begin
  Firmware_LastLinesPerSecond := Firmware_LinesPerSecond;
  Firmware_LinesPerSecond := 0;
end;

procedure TZXFirmwareHandler.SendHexLine(iLineNumber: fi; bCompress,
  bAltFile: boolean);
var
  cc: byte;
  x: cardinal;
  s: ansistring;
  ss: ansistring;
  t: fi;
  newlen: fi;
  orig,bytes,check: TDynByteArray;
  slUsedHex: TStringlist;
begin
  slUsedHex := slHex;
  if bAltFile then
    slUsedHex := slHexAdv;
  setlength(bytes, 250);
  LockComms;
  try
    IF iLIneNumber < 0 then
      exit;
    if iLineNumber >= slUsedHex.count then
      exit; //ignore invalid line requests (client should time out eventually)

    BeginCalMessage($F0);
    BeginOutputCheckSumming;
    ZXMIDIHeader();
    s := slUsedHex[iLineNumber];
{x$DEFINE ASCII_FW}
{$IFDEF ASCII_FW}
    CalMessage($52);//CMD
{$ELSE}
    t := 0;
    //Debug.ConsoleLog(inttostr(iLIneNumber)+': '+s);
    while t< length(s) div 2 do begin
      ss := zcopy(s, 1+(t*2),2);
      bytes[t] := strtoint('$'+ss);
      inc(t);
    end;
    setlength(bytes, t);
    newlen := t;

    if bCompress then begin
      //Debug.ConsoleLog(memorydebugstring(@bytes[0], length(bytes)));
      orig := bytes;
      bytes := Compressor_Simple.CompressBytes(orig);
//      Debug.ConsoleLog('RLE Compressed Line = '+MemoryToString(@bytes[0], length(bytes)));
//      check := DecompressBytes(bytes);
      newlen := length(bytes);
//      if length(orig) <> length(check) then
//        raise ECritical.create('Compression Length Failure');
//      if not CompareMem(@orig[0], @check[0], length(orig)) then
//        raise ECritical.create('Compression Failure');

      if length(bytes) > length(orig) then begin
        bytes := orig;
        bCompress := false;
        newlen := length(bytes);
      end;
    end else begin
//      Debug.ConsoleLog('UnCompressed Line = '+MemoryToString(@bytes[0], length(bytes)));
    end;



    if bCompress then
      CalMessage($58)
    else begin
      CalMessage($53);
//      sleep(20);
    end;
{$ENDIF}
    x := iLineNumber;
    endianswap(@x, 4);
    TXBufferAs7Bit(@x, 4);//send line number
    inc(Firmware_LinesPerSecond);
{$IFDEF ASCII_FW}
    for t:= 1 to length(s) do begin
      CalMessage(byte(s[t]));
    end;
{$else}


    TXBufferAs7Bit(@bytes[0], newlen);

{$endif}
    CalMessage(EndoutputChecksumming);
    ENdCalMessage($F7);

  finally
    UnlockComms;
  end;
end;


procedure TZXFirmwareHandler.SetFirmwareUpdateMode(const Value: boolean);
begin
  FFirmwareupdateMOde := Value;
end;

procedure TZXFirmwareHandler.TXBufferAs7Bit(p: pbyte; len: ni);
var
  pp: pbyte;
  newlen: ni;
  t: fi;
begin
  newlen := Get7BitEncodedBufferLength(len);
  pp := GetMemory(newlen);
  try
    if not Convert8BitsTo7Bits(p, len, pp, newlen) then begin
      raise ECritical.Create('Failed to conver 8-bits to 7-bits');
    end;

    for t:= 0 to newlen-1 do begin
      CalMessage(pp[t]);
    end;

  finally
    FreeMemory(pp);
  end;
end;

procedure TZXFirmwareHandler.UnlockComms;
begin
  conn.UnlockComms;
end;

procedure TZXFirmwareHandler.ZXMIDIHEader;
begin
  CalMessage($00);
  CalMessage($02);
  CalMessage($02);
end;

procedure TZXFirmwareHandler.SetFirmwareUpdateParams(iCompressMode: ni; iLineBatchSize: ni; iPrefetchTimer:ni);
begin
{$DEFINE ENABLE_COMPRESSION}
{$IFDEF ENABLE_COMPRESSION}
    Debug.ConsoleLog('Trying to set compression mode:'+inttostr(iCOmpressMode)+' Batch='+inttostr(iLineBatchSize)+' Latency='+inttostr(iPrefetchTimer));
    BeginCalMessage($F0);
    BeginOutputCheckSumming;
    ZXMIDIHeader();
    CalMessage($59);//CMD
    compressionmode := iCompressMode;
    CalMEssage(iCompressMode);//enable compression
    CalMEssage(iLineBatchSize);//FIRMWARE BATCH SIZE
    CalMEssage(greaterof(0,iPrefetchTimer-35));//FIRMWARE BATCH SIZE
    CalMessage(EndoutputChecksumming);
    CalMessage($F7);
{$ENDIF}
end;


end.
