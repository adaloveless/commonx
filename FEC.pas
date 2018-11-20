unit FEC;

interface

uses
  idglobal, typex, systemx, BetterIdUDPServer,tickcount, sharedobject, classes, BetterIdUDPBase,Betteridsockethandle, numbers, debug, sysutils, idstackconsts, simplequeue, backgroundthreads, managedthread;
{x$INLINE AUTO}
{$IFDEF MSWINDOWS}
  {$DEFINE QUICKINIT}
{$ENDIF}
{$DEFINE POINTERS}
{$DEFINE OUT_QUEUE}
{x$DEFINE GIANT}
{$DEFINE THROTTLEOUT}//throttling should be done at the endpoint level

//done 1 - pad zeros on last packet before reconstruction (else reconstruction will fail)
//done 1 - zero memory before reconstruct! or use different copy for first piece
//todo 1 - determine what should happen to constructor after submitted to the main stream
//todo 1 - hijack whatever UDP send is used by the RUDP stuff
//todo 1 - hijack the RUDP Base Class





//1. Packet Comes in
//2. FEC Header Examined, "this is part 1 of 5"
//3. FEC Constructor is found for peer binding
//  3a. If Not constructor is found, then one is assigned
//4. Packet is Dispatched to the FEC Contstructor
//  4a. FEC Constructor discards the packet if its part is already assigned
//  4b. FEC Constructor Gives packet, minus FEC Header, to FECPart array
//  4c. If At Least n-1 parts have been found, then
        //4b1. Build Parts into single
          //- Check Bits, if lower-bits are complete
              //-Copy n-1 packets to result buffer
          //- ELSE
              //-Build missing packet from Parity
              //-Copy n-1 packets to result buffer

        //4b2. return "parts complete"

//  4d. If more parts needed then return "parts needed"
//5. If FEC Constructor Returns "parts complete"
// -- CAll Virtual Override DoUDPRead() with reconstructed part



CONST

  FEC_OVERSAMPLE_COUNT = 1;
  MAX_CONSTRUCTORS = 100;
  MAX_PART_SIZE = 450;
{$IFDEF GIANT}
  MAX_PARTS = 250;
{$ELSE}
  MAX_PARTS = 60;
{$ENDIF}
  PARITY_FREQ = 8;
  parity_zone_size = PARITY_FREQ-1;
  payloads_per_zone = parity_zone_size;
  MAX_FEC_MESSAGE_SIZE = MAX_PART_SIZE * (((MAX_PARTS) div PARITY_FREQ) * (PARITY_FREQ-1));
const
{$IFDEF GIANT}
  PB_ONE : TGiantFuckingInt = (i1:1; i2:0; i3:0; i4:0);
  PB_ZERO : TGiantFuckingInt = (i1:0; i2:0; i3:0; i4:0);
{$ELSE}
  PB_ONE : Uint64 = 1;
  PB_ZERO : Uint64 = 0;
{$ENDIF}

type
{$IFDEF GIANT}
  TPartBits = TGiantFuckingInt;
{$ELSE}
  TPartBits = UInt64;
{$ENDIF}
  TFECResult = (fecPartsNeeded, fecPartsComplete, fecResourceFlood, fecStaleData, fecInvalidData);

  TFECServer = class;//forward

  TFECReport = packed record
    //since a message can be constructed from an incomplete
    //number of packet, it is possible to determine the when
    //the health of a message is teetering on the edge
    //of going lost
    //since we want to push the pipe as hard as we can without losing messages
    //it is ideal to throttle speed to a certain loss-level (packet loss is okay, but not message loss)
    //this structure represents an individual message, which is a string of packets
    parts: word;           //total parts in message (including lost ones)
    dups: word;            //total parts duplicate
    packetslost: word;         //total parts missing
    retry_estimate: byte;  //max dups of a particular part seen  (it is possible that a retry would not generate any dups however)
    par_freq: byte;
    procedure Init;
    function packets: ni;
    function Quality: single;
  end;
  PFECReport = ^TFECReport;

  TFECStats = packed record
    good: ni;
    badcheck: ni;
    partial: ni;
    OutOfConstructors: ni;       //If we run out of constructors, and have to overwrite the oldest constructor
    NonStaleConstructorUse: ni;  //if we start using constructors that are marked "complete" but not stale
    StaleConstructorUse: ni;     //if we use a constructor that is marked "stale"
  end;

  TFECQueueItem =  class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    srv: TFECServer;
    data: PByte;
    datalen: ni;
    PeerIP: string;
    PeerPort: ni;
    procedure Init;override;
    procedure Detach;override;
  end;



  TSimplePacketQueue = class(TSimpleQueue)
  public

  end;


  TSimplePacketOutQueue = class(TSimpleQueue)
  protected
{$IFDEF THROTTLEOUT}
    //bytes_per_hr_tick: double;
    ticks_per_byte: double;
    lasttxtime: ticker;
    lasttxbytes: ni;
    function QuotaAVailable: boolean;
    function GetNextItem: TQueueItem; override;
    procedure SpeedUp;
    procedure SlowDown;
    procedure Init;override;
{$ENDIF}
{$IFDEF CUSTOM_ORDERING}
    function GetNextItem: TQueueItem; override;
{$ENDIF}
  end;

  TSimplePAcketInQueue = class(TSimpleQueue)
  protected
    function GetNextItem: TQueueItem; override;
  end;

  TFECQueueItem_OUT =  class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    srv: TFECServer;
{$IFDEF POINTERS}
    data: PByte;
    datalen: ni;
{$ELSE}
    data: TIDBytes;
{$ENDIF}
    PeerIP: string;
    PeerPort: ni;
    FutureTime: ticker;
    procedure Init;override;
    procedure Detach;override;
  end;


  TFECPacketHeader = packed record
    cc0: byte;
    cc1: byte;
    part: byte;
    partof: byte;
    pid: cardinal;
    totalmessagesize: cardinal;
    checksum: cardinal;
    procedure Init;
    function IsValid: boolean;
    function DebugString: string;
  end;
  PFECPacketHeader = ^TFECPacketHeader;

  TFECPart = record
    bytes: array[0..MAX_PART_SIZE] of byte;
    byte_size: ni;
    dups: ni;
    procedure GivePacket(p: pbyte; cnt: ni);
    procedure INit;
  end;
  PFECPart = ^TFECPart;

  TFECConstructor = record
    assigned: boolean;
    completed: boolean;
    assign_time: ticker;
    Binding: TIdSocketHandle;
    part_assignments: TPartBits;
    expected_parts: ni;
    packetid: cardinal;
    count_received: ni;
    totalmessagesize: ni;
    expected_checksum: cardinal;
    partof: ni;
    missing_test_result: ni;
    server: TFECServer;
    health: TFECReport;
    parts: array[0..MAX_PARTS-1] of TFECPart;//<--Note that this cannot be changed unless you offer more bits to part_assignments
    complete_bytes: array[0..(MAX_PART_SIZE*MAX_PARTS)-1] of byte;
    procedure Init;inline;
    procedure SetPartAssignments(const Value: TPartBits);
    procedure AssignTo(Abinding: TIDSocketHandle; pid: ni; expected_sum: cardinal; partof: ni);inline;
    function DispatchPacket(fech: PFECPacketHeader; data: PByte; dataLength: ni): TFecResult;
    function Complete: boolean;
    function HasPart(const part: ni): boolean;inline;
    procedure SetPartComplete(const part, partof: ni);inline;
    function CopyCompletion: boolean;
    function ParityReconstruct: boolean;
    //property Part_Assignments: TPartBits read FPart_Assignments write SetPartAssignments;
    function GEtCompletedBytes: TIdBytes;inline;
    function CanComplete: boolean;
    procedure RollUpHealth;
  private
    function CanCompleteZone(zidx: ni): boolean;
    function NumberOfZones: ni;
    function ParityReconstructZone(zidx: ni): boolean;
    function CompletionReport: string;
  end;
  PFECConstructor = ^TFECConstructor;

  TFecConstructors = class(TSharedObject)
  strict private
    FReconstruct: array[0..65535] of byte;//pointers assigning clients/ports to constructors
    FConstructors: array[0..MAX_CONSTRUCTORS-1] of TFECConstructor;
    last_constructor_idx: ni;
    Fserver: TFECServer;
    function FindConstructor(binding: TIdSocketHandle; pid: cardinal; expected_sum: cardinal; parts: ni): PFECConstructor;
    function NewConstructor(binding: TIdSocketHandle; pid: cardinal; expected_sum: cardinal; bCheckCompleted: boolean; parts: ni): PFECConstructor;
    function HijackOldestConstructor(binding: TIdSocketHandle; pid: cardinal; expected_sum: cardinal; bCheckCompleted: boolean; parts: ni): PFECConstructor;
  private
    procedure SetSErver(const Value: TFECServer);
  public
    property Server: TFECServer read FServer write SetSErver;
    function DispatchPacket(binding: TIdSocketHandle; p_IncludesFecHeader: pbyte; pl: ni; out fec: PFecConstructor): TFECResult;
  end;

  //--------------------------------------------------------------------------
  TFECServer = class(TIdUDPServer)
  private
    procedure UdpRead;
    procedure SendMultipleFECUDP(basetm: ticker; interval: ticker; const host: string; const port: word;  const data: TidBytes; const Amax_part_size: ni; const fecpid: ni; single: ni);
    procedure SendMultipleFECUDP_Randomized(basetm: ticker; interval: ticker; const host: string; const port: word;  const data: TidBytes; const Amax_part_size: ni; const fecpid: ni);
    procedure GEtFecConfig(tms: ni; out parts, zones, payloads_per_zone,
      payload_packets, parity_packets, chunk_size: ni);
  strict protected
    detached: boolean;
    fecs: TFECConstructors;
    optsset: boolean;
    queue_out: TSimpleQueue;
  public
    csLock: TCLXCriticalSection;
    last_fecpid: cardinal;
    max_message_size: cardinal;
    queue_in: TSimpleQUeue;
    stats: TFECSTats;
    constructor Create(AOwner: TComponent);reintroduce;virtual;//todo 1:translate
    destructor Destroy;override;//todo 1:translate
    procedure Detach;virtual;//todo 1:translate
    procedure DetachAndFree;virtual;//todo 1:translate
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------
    procedure DoUDPReadOuter(AThread: TIdUDPListenerThread; const XData: TIdBytes; ABinding: TIdSocketHandle); override;//todo 1:translate
    procedure DoUDPRead_BETTER(const HealthData: TFECREport; const XData: TIDBytes; PeerIP: string; PEerPort: ni);virtual;
    procedure ProcessUDPReadOuter(qi: TFECQueueItem);
//    procedure DoUDPRead(AThread: TIdUDPListenerThread; const XData: TIdBytes; ABinding: TIdSocketHandle); override;//todo 1:translate
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------
    function SendBufferWithFec(const basetm, interval: ticker; const AHost: string; const APort: TIdPort; const ABuffer : TIdBytes; chunksize: cardinal): cardinal;inline;
    procedure ReSendBufferWithFec(const basetm, interval: ticker; const AHost: string; const APort: TIdPort; const ABuffer : TIdBytes; fecpid: cardinal; const FECSize: cardinal);
    function NExtFecPID: ni;inline;
    procedure SetOptions;
    procedure BestSend(hr_time: ticker; const AHost: string; const APort:TIDport; const ABuffer: TIDBytes);

  end;

function IsParityPart(idx, freq: ni): boolean;





implementation

{ TFECConstructor }

function IsParityPart(idx, freq: ni): boolean;
begin
  result := (idx mod freq) = (freq-1)
end;

procedure TFECServer.GEtFecConfig(tms: ni; out parts, zones, payloads_per_zone, payload_packets, parity_packets, chunk_size: ni);
var
  highbyte: ni;
  moddy: ni;
begin
  if tms > MAX_FEC_MESSAGE_SIZE then
    raise ECRitical.create('CAnnot create fec packet with message > '+inttostr(MAX_FEC_MESSAGE_SIZE)+' with this configuration.');
  highbyte := tms-1;


  //Valid Configs - assume part size = 3 for simplicity
  //                            [ms] [cs] [zones]
  //[.][p]                       1    1      1
  //[.][.][p]                    2    1      1
  //[..][.x][pp]                 3    2      1
  //[..][..][pp]                 4    2      1
  //[...][..x][ppp]              5    3      1
  //[...][...][ppp]              6    3      1
  //[..][..][pp][..][.x][pp]     7    2      2
  //[..][..][pp][..][..][pp]     8    2      2
  //[...][...][ppp][...][p]      9    3      2
  //[...][...][ppp][.][p]        10   3      2
  //[...][...][ppp][.][p]        11   3      2
  //[...][...][ppp][.][p]        12   3      2
  //[...][...][ppp][.][p]        13   3      3
  //[...][...][ppp][.][p]        14   3      3
  //[...][...][ppp][.][p]        15   3      3

  highbyte := tms-1;//address of last byte in packet
  zones := (((highbyte) div (MAX_PART_SIZE*fec.payloads_per_zone))+1);
          //  ((1-1) div 3)+1 = 1
          //  ((2-1) div 3)+1 = 1
          //  ((3-1) div 3)+1 = 1
          //  ((4-1) div 3)+1 = 2

  //now that we know how many zones we need, we should optimize the zones to have payloads
  //of equal size
  if zones > 1 then
    payload_packets := zones * fec.PAYLOADS_PER_ZONE
  else
    payload_packets := (highbyte div MAX_PART_SIZE)+1;



  moddy := lesserof(tms mod payload_packets,1);//divides evenly?
  chunk_size := (tms div payload_packets) + moddy;
              //  (1 div 2) + 1 = 1
              //  (2 div 2) + 0 = 1
              //  (3 div 2) + 1 = 2
              //  (4 div 2) + 0 = 2
              //  (6 div 2) + 0 = 3
              //  (7 div 2) + 1 = 4
  parity_packets := zones;
  parts := payload_packets + parity_packets;
end;

function optbasetm(tm: ticker): ticker;inline;
begin
//  result := greaterof(tm,gethighresticker);
  result := tm;
end;

procedure TFEcServer.SendMultipleFECUDP(basetm: ticker; interval: ticker; const host: string; const port: word; const data: TidBytes; const Amax_part_size: ni; const fecpid: ni; single: ni);
var
  b, fecbytes, btemp: TIdBytes;
  fech: TFECPacketHeader;
  fech_ptr: PFECPAcketHEader;
  iCan, iChunkSize: ni;
  tms: ni;
  sz, t,cx: ni;
  parts: ni;
  pid: cardinal;
  partid: ni;
  iSum: int64;
  parity_packets: ni;
  payload_packets: ni;
  zones: ni;
  moddy: ni;
  highbyte: ni;
  payloads_per_zone: ni;
begin
  CalculateChecksum(@data[0], length(data), {out} iSum);

  if self.binding = nil then
    raise ECritical.create('udp is not bound');

  if not optsset then
    SetOptions;


  fech.Init;
  sz := sizeof(fech);
  tms := length(data);
  GEtFecConfig(tms, parts, zones, payloads_per_zone, payload_packets, parity_packets, iChunkSize);
  fech.totalmessagesize := tms;


  t := 0;

  setlength(fecbytes, iChunkSize+sz);
  fillmem(@fecbytes[0], iChunkSize+sz, 0);

  cx := tms;

  if parts > MAX_PARTS then
    raise ECritical.create('too many parts '+inttostr(parts)+' for datasize '+inttostr(length(data)));

  pid := fecpid;
  self.last_fecpid := pid;
  partid := 0;
  //fec packet setup
  fech.Init;
  fech.part := parts-1;
  fech.partof := parts;
  fech.totalmessagesize := tms;
  fech.pid := pid;
  fech.checksum := iSum;
  movemem32(@fecbytes[0], @fech, sz);
  fech_ptr := @fecbytes[0];

  while (cx > 0) or (partid < (parts)) do begin
    if ((partid+1) = parts)
    or (isparitypart(partid, parity_freq)) then begin
      fech_ptr.part := partid;
      //send the parity information after 8 packets
//      Debug.ConsoleLog('Sending Fec PArt '+inttostr(fech_ptr.part)+' len='+inttostr(length(fecbytes)));

      setlength(fecbytes, iChunkSize+sz);
      if (single < 0) or (partid=single) then
        self.BestSend(optbasetm(basetm)+((interval*random(parts))), host, port, fecbytes);
      fillmem(@fecbytes[sz], iChunkSize, 0);
    end else begin
      iCan := lesserof(cx, iChunksize);
      //Debug.Consolelog('PartID '+inttostr(partid)+' Can='+inttostr(iCan)+' ToGo='+inttostr(cx));
      setlength(b, iCan+sz);
      if length(b) > (MAX_PART_SIZE+sz) then
        Raise ECritical.create('length(b) > MAX_PART_SIZE');
      fech.Init;
      fech.part := partid;
      fech.partof := parts;
      fech.totalmessagesize := tms;
      fech.pid := pid;
      fech.checksum := iSum;
      movemem32(@b[0], @fech, sz);
      if iCan > 0 then
        movemem32(@b[sz], @data[t], iCan);

//      if random(100) > 30 then
      if (single < 0) or (partid=single) then
        self.BestSend(optbasetm(basetm)+((interval*random(parts))), host,port,b);
//      sleep(0);
      if iCan > 0 then
        movemem32Xor(@fecbytes[sz], @b[sz], iCan);
      inc(t, iCan);
      dec(cx, iCan);
    end;
    inc(partid);
    if (partid >= (parts-1)) and (cx > 0) then
      raise ECRitical.create('data still left after FEC creation! cx='+inttostr(cx)+' tms='+inttostr(tms)+' parts='+inttostr(parts));
  end;

end;

procedure TFECServer.SendMultipleFECUDP_Randomized(basetm: ticker; interval: ticker; const host: string;
  const port: word; const data: TidBytes; const Amax_part_size, fecpid: ni);
var
  tms, zones, parts, payloads_per_zone, parity_packets, chunk_size, payload_packets: ni;
  rnd: TDynInt64Array;
  t: ni;
begin
  tms := length(data);
  GEtFecConfig(tms, parts, zones, payloads_per_zone, payload_packets, parity_packets, chunk_size);
  rnd := RAndomOrder(parts);

  for t := 0 to high(rnd) do begin
    SendMultipleFECUDP(basetm, interval, host, port, data, AMax_part_size, fecpid, rnd[t]);
  end;

end;

procedure TFECServer.SetOptions;
begin
  Binding.SetSockOpt(ID_SOL_SOCKET, ID_SO_SNDBUF, ID_UDP_BUFFERSIZE);
  Binding.SetSockOpt(ID_SOL_SOCKET, ID_SO_RCVBUF, ID_UDP_BUFFERSIZE);
//  Binding.UseNagle := false;
//  Binding.GetSockOpt(Id_SOL_SOCKET, Id_SOCK_DGRAM
  optsset := true;
end;

procedure TFECConstructor.AssignTo(Abinding: TIDSocketHandle; pid: ni; expected_sum: cardinal; partof: ni);
begin
  self.Init;
  self.binding := abinding;
  assign_time := getticker;
  assigned := true;
  packetid := pid;
  part_assignments := PB_ZERO;
  self.partof := partof;
  count_received := 0;
  expected_Checksum := expected_sum;
  health.init;
  health.parts := partof;
  health.par_freq := PARITY_FREQ;
end;


function TFecConstructor.NumberOfZones: ni;
begin
  result := ((expected_parts-1) div PARITY_FREQ)+1;
end;


function TFecConstructor.CanCompleteZone(zidx: ni): boolean;
var
  zmask: TPartBits;
  offmask: TPartBits;
  zparts: TPartBits;
  hob: ni;
begin
  missing_test_result := -1;
  zmask := (PB_ONE shl PARITY_FREQ)-PB_ONE;
  //cut off expected number of parts in the highest zone
  offmask := (((PB_ONE shl uint64(expected_parts))-PB_ONE));
  offmask := offmask shr (zidx*PARITY_FREQ);
  offmask := offmask and zmask;
  zmask := zmask and offmask;//?


  zparts := (part_assignments shr (zidx * uint64(PARITY_FREQ))) and zmask;
  zparts := (zparts xor zmask);
//  if zidx = 0 then begin
//    Debug.Consolelog('here');
//  end;
  hob := highorderbit(zparts);
  if hob < 0 then
    exit(true);

  GFI_ONE.FromInt64(1);
  result := zparts = (PB_ONE shl hob);
  if result then
    missing_test_result := hob;



end;

function TFECConstructor.CanComplete: boolean;
var
  noz: ni;
begin
  //if we have at least 8 of 9 parts from each zone then we're good to go
  noz := NumberOfZones;
  while noz > 0 do begin
    dec(noz);
    if not CanCompleteZone(noz) then begin
      {$IFDEF COMPLETION_REPORTS}
        Debug.ConsoleLog('Can''t Complete! '+CompletionReport);
      {$ENDIF}
      exit(false);
    end;
  end;

//  Debug.Consolelog('Can Complete with '+inttohex(part_assignments,0));
{$IFDEF COMPLETION_REPORTS}
  Debug.ConsoleLog('Can    Complete! '+CompletionReport);
{$ENDIF}
  result := true;

end;

function TFECConstructor.PArityReconstruct: boolean;
var
  noz: ni;
begin
  //if we have at least 8 of 9 parts from each zone then we're good to go
  noz := NumberOfZones;
  while noz > 0 do begin
    dec(noz);
    if not PArityREconstructZone(noz) then
      exit(false);
  end;

  result := true;
end;

function TFECConstructor.Complete: boolean;
var
  po, pof: ni;
  pomask: TPartBits;
begin
  pof := expected_parts;


  if not CanComplete then begin
    exit(false);
  end;

  //else reconstruct the missing packet from the parity
  if not ParityReconstruct then
    exit(false);

  exit(CopyCompletion);


end;

function TFECConstructor.CompletionReport: string;
var
  noz: ni;
begin
  //if we have at least 8 of 9 parts from each zone then we're good to go
  noz := NumberOfZones;
  result := '[';
  while noz > 0 do begin
    dec(noz);
    if not CanCompleteZone(noz) then
      result := result + 'x'
    else
      result := result + 'C'
  end;

  result := result + ']'+inttostr(packetid)+'[';


  noz := expected_parts;
  while noz > 0 do begin
    dec(noz);
    if HasPart(noz) then
      result := result + '+'
    else
      result := result + '-';
  end;

  result := result + ']';

//  Debug.Consolelog('Can Complete with '+inttohex(part_assignments,0));

end;

function TFECConstructor.CopyCompletion: boolean;
var
  cx, pof, t: ni;
  pfp: PFECPart;
  p: pbyte;
  bs: ni;
  tms: cardinal;
  iSum: int64;
  iFirstSum: cardinal;
  bGoodSum: boolean;
  movedsize: ni;
begin
  pof := expected_parts;
  cx := pof;
  t := 0;
  p := @Self.complete_bytes[0];
  tms := 0;
  iFirstSum := expected_checksum;
  movedsize := 0;
  while cx > 0 do begin;
    dec(cx);
    if (not (isparitypart(t,parity_freq))) and (t < (expected_parts-1)) then begin
      pfp := @Self.parts[t];
      bs := pfp^.byte_size;
      movemem32(p, @(pfp^.bytes[0]), bs);
      //Debug.Consolelog('Using '+inttostr(bs)+' bytes from '+inttostr(t));
      inc(movedsize, bs);
      inc(p, bs);
    end;
    inc(t);
  end;
  CalculateChecksum(@self.complete_bytes[0], totalmessagesize, iSum);
  bGoodSum := iSum=expected_checksum;
  if not bGoodSum then begin
    Debug.ConsoleLog('bad checksum '+inttostr(Self.packetid)+' moved size='+inttostr(movedsize)+' expectedsize='+inttostr(totalmessagesize));
    init;
  end;
  RollUpHealth;
  exit(bGoodSum);


end;

function TFECConstructor.DispatchPacket(fech: PFECPacketHeader; data: PByte;
  dataLength: ni): TFecResult;
var
  pfp: PFecPart;
begin
//  Debug.ConsoleLog('Got '+fech.DebugString);

  //now we do the fun part
  //we assume that this fecheader belongs to this constructor
  //the part information contained in the packet, however, has not been processed

  //  4a. FEC Constructor discards the packet if its part is already assigned
//  if fech.part in [51,52] then
//    Debug.Consolelog('here');
  if HasPart(fech.part) then begin
    Self.parts[fech.part].dups := Self.parts[fech.part].dups+1;
    exit(fecStaleData);
  end;


  //  4b. FEC Constructor Gives packet, minus FEC Header, to FECPart array
  //  Note, header was removed from pointer in outer method


  pfp := @Self.parts[fech.part];


  if datalength > length(pfp^.bytes) then
    exit(fecInvalidDAta);

  if partof <> fech.partof then
    Debug.Consolelog('part count not the SAME!');
  pfp^.GivePacket(data, datalength);
  totalmessagesize := fech.totalmessagesize;

  SetPartComplete(fech.part, fech.partof);
  inc(count_received);


//  if fech.part = 53 then
//    Debug.Consolelog(inttostr(fech.part))
//  else
//    Debug.Consolelog(inttostr(fech.part));

  if Complete then begin
    exit(fecPartsComplete);
  end;

  result := fecPartsNeeded;



end;

function TFECConstructor.GEtCompletedBytes: TIdBytes;
begin
  setlength(result, totalmessagesize);
  movemem32(@result[0], @self.complete_bytes[0], totalmessagesize);
end;

function TFECConstructor.HasPart(const part: ni): boolean;
var
  bitmask: TPartBits;
begin
  bitmask := GFI_ONE shl part;
  exit(((part_assignments) and bitmask) = bitmask);

end;

procedure TFECConstructor.Init;
begin
  FillMem(@self, sizeof(self), 0);
  Binding := nil;
  assigned := false;

end;

function  TFECConstructor.ParityReconstructZone(zidx: ni): boolean;
var
  pomask: TPartBits;
  bits: TPartBits;
  missing: ni;
  t, last: ni;
  pfp, miss: PFECPart;
  bs: ni;
  startingpart: ni;
  endpart: ni;
  zoneparts: ni;
begin
  startingpart := zidx * PARITY_FREQ;
  endpart := lesserof(startingpart+(PARITY_FREQ-1), expected_parts-1);
  zoneparts := (endpart-startingpart)+1;

  result := false;
  if zoneparts = 0 then
    exit;
  if expected_parts > MAX_PARTS then
    exit;

  if not CanCompleteZone(zidx) then
    exit;



  missing := missing_test_result;//flag filled by CanCompleteZone()
  if missing < 0 then
    exit(true);
  missing := missing + startingpart;

  //now that we know the missing part, we construct the part
  //from the others xor parity
  if (missing >= expected_parts) then
    exit;

  miss := @self.parts[missing];
  miss^.init;
  pfp := nil;
  last := expected_parts-1;
  bs := 0;
  //if we're missing the last zone, no reconstruction is necessary
  if (missing)=endpart then
    exit(true);

//  if missing >=0 then
//    Debug.ConsoleLog('Zone '+inttostr(zidx)+' Missing: '+INTTOstr(missing+1)+' of '+inttostr(expected_parts));

  for t := startingpart to endpart do begin
    if (t=missing) then
      continue;
    pfp := @Self.parts[t];
    bs := greaterof(bs, pfp^.byte_size);
    MoveMem32Xor(@(miss^.bytes[0]),@(pfp^.bytes[0]), pfp^.byte_size);
  end;

  if pfp = nil then
    raise ECritical.create('this should never happen');

  //missing length with either be
  //1.The Chunk Size
  //2.totalmessagesize - (chunksize * partidx)
  if missing = last-1 then
    miss^.byte_size := totalmessagesize-(bs * missing)
  else
    miss^.byte_size := bs;

//  Debug.Consolelog('Recovered missing '+inttostr(bs)+' bytes');

  result := true;

end;

procedure TFECConstructor.RollUpHealth;
var
  t: ni;
  pfp: PFECPart;
  rt: ni;
  miss: ni;
begin
  rt := 0;
  miss := 0;
  for t:= 0 to partof-1 do begin
    pfp := @Self.parts[t];
    rt := greaterof(rt, pfp.dups);

  end;
  for t:= 0 to partof-1 do begin
    pfp := @Self.parts[t];
    if not haspart(t) then
      inc(miss)
    else
      inc(miss, (rt-pfp.dups));//if 0% packet loss, then dups should equal retries, else packets were lost in the retry phase
  end;
  health.retry_estimate := rt;
  health.packetslost := miss;




//  health.par_parts := partof;


end;

procedure TFECConstructor.SetPartAssignments(const Value: TPartBits);
begin
//  Debug.ConsoleLog('bits '+inttohex(value,0));
//  FPart_Assignments := Value;
end;

procedure TFECConstructor.SetPartComplete(const part, partof: ni);
var
  bitmask: TPartBits;
begin
//  DEbug.ConsoleLog('Got part '+inttostr(part)+' of '+inttostr(partof));
  bitmask := GFI_ONE shl part;
  part_assignments := part_assignments or bitmask;
  expected_parts := partof;

end;

{ TFECPacketHeader }

function TFECPacketHeader.DebugString: string;
begin
  result := 'pid:'+IntToHex(pid,0)+' part='+IntToStr(part+1)+' of '+inttostr(partof);
end;

procedure TFECPacketHeader.Init;
begin
{$IFDEF QUICKINIT}
  PUint64(@self)^ := $0000000000004346;
  totalmessagesize := 0;
  checksum := 0;
{$ELSE}
  cc0 := $46;//F
  cc1 := $43;//C
  part := 0;
  partof := 0;
  packetid := 0;
  totalmessagesize := 0;
  checksum := 0;
{$ENDIF}


end;

function TFECPacketHeader.IsValid: boolean;
begin
  result := (cc0 = $46) and (cc1 = $43);
end;

{ TFECServer }

procedure TFECServer.BestSend(hr_time: ticker; const AHost: string; const APort: TIDport;
  const ABuffer: TIDBytes);
{$IFNDEF OUT_QUEUE}
begin
  SendBuffer(AHost, APort, ABuffer);
end;
{$ELSE}
var
  qi: TFECQueueItem_OUT;
  b: TIDBytes;
  l: ni;
begin
  qi := TFECQueueItem_OUT.Create;
  qi.srv := self;
  l := length(ABuffer);
{$IFDEF POINTERS}
  qi.datalen := l;
  if l > 0 then begin
    qi.data := GetMemory(l);
    movemem32(qi.data, @ABuffer[0], l)
  end else
    qi.data := nil;
{$ELSE}
  //can't just use ABuffer directly because it is passed as a CONST in upper layers
  setlength(qi.data, length(ABuffer));
  movemem32(@qi.data[0], @ABuffer[0], length(ABuffer));
{$ENDIF}

  qi.peerip := ahost;
  qi.peerport := aport;
  qi.FutureTime := hr_time;
  queue_out.additem(qi);


end;
{$ENDIF}

constructor TFECServer.Create(AOwner: TComponent);
begin
  inherited;
  ics(csLock);

  fecs := TFecConstructors.Create;
  queue_in := TSimplePAcketInQueue.create(nil, nil);
//  queue_in.MaxItemsInQueue := 128;
  queue_in.Start;
  queue_out := TSimplePAcketOutQueue.create(nil,nil);
//  queue_out.MaxItemsInQueue := 64;
  queue_out.start;

end;

destructor TFECServer.Destroy;
begin
  DEtach;
  inherited;
  lcs(csLock);
end;

procedure TFECServer.Detach;
begin
  if detached then exit;
  queue_in.Stop;
  queue_out.stop;
  queue_in.SafeWaitFor;
  queue_out.safewaitfor;
  TPM.NoNEedThread(queue_in);
  TPM.NoNEedThread(queue_out);
  fecs.free;
  fecs := nil;
  Detached := true;

end;

procedure TFECServer.DetachAndFree;
begin
  Detach;
  Free;
end;


procedure TFECServer.DoUDPReadOuter(AThread: TIdUDPListenerThread;
  const XData: TIdBytes; ABinding: TIdSocketHandle);
var
  qi: TFECQueueItem;
  l: ni;
  p: Pbyte;
begin
//  ecs(csLock);
  try
  qi := TFECQueueItem.create;
  qi.srv := self;
  l := length(XData);
  p := nil;
  if l > 0 then begin
    p := GEtMemory(l);
    movemem32(p, @Xdata[0], l);
  end;
  qi.data := p;
  qi.datalen := l;
  qi.PeerIP := ABinding.PeerIP;
  qi.PeerPort := ABinding.PeerPort;
  queue_in.AddItem(qi);
  finally
//    lcs(csLock);
  end;


end;


procedure TFECServer.DoUDPRead_BETTER(const HealthData: TFECREport; const XData: TIDBytes; PeerIP: string;
  PEerPort: ni);
begin
  if assigned(FOnUDPRead) then
    FOnUDPRead(nil, Xdata, nil);
  //
end;

function TFECServer.NExtFecPID: ni;
begin
  result := last_fecpid + 1;
  last_fecpid := result;
end;

procedure TFECServer.ProcessUDPReadOuter(qi: TFECQueueItem);
var
  fec: PFECConstructor;
  b: TidBytes;
  bUnlocked: boolean;
begin
  bUnlocked := false;
  fecs.Lock;
  try
    if fecs.DispatchPacket(binding, qi.data, qi.datalen, fec) = fecPartsComplete then begin
      b := fec.GEtCompletedBytes;
      fec.Completed := true;
      fecs.Unlock;
      bUnlocked := true;
      DoUDPREad_BETTER(fec.health, b, qi.PeerIP, qi.PeerPort);
    end;
  finally
    if not bUnlocked then
      fecs.unlock;
  end;
end;

procedure TFECServer.ReSendBufferWithFec(const basetm, interval: ticker; const AHost: string;
  const APort: TIdPort; const ABuffer: TIdBytes; fecpid: cardinal; const FECSize: cardinal);
var
  cx: ni;
begin
  cx := FEC_OVERSAMPLE_COUNT;
  while cx > 0 do begin
    dec(cx);
//    SendMultipleFECUDP_Randomized(basetm, interval, ahost, aport, abuffer, fecsize, fecpid);
    SendMultipleFECUDP(basetm, interval, ahost, aport, abuffer, fecsize, fecpid,-1);
    //SendMultipleFECUDP(ahost, aport, abuffer, fecsize, fecpid, -1);
  END;
end;

function TFECServer.SendBufferWithFec(const basetm, interval:ticker; const AHost: string;
  const APort: TIdPort; const ABuffer: TIdBytes; chunksize: cardinal): cardinal;
var
  i: cardinal;
  cx: ni;
begin
  inherited;
  i := NextFecPid;
  cx := FEC_OVERSAMPLE_COUNT;
  while cx > 0 do begin
    dec(cx);
    SendMultipleFECUDP(basetm, interval, ahost, aport, abuffer, chunksize, i,-1);
  end;
  result := i;
end;

procedure TFECServer.UdpRead;
begin
  inherited;
  //Put FEC Checks/reconstruction here
end;

{ TFecConstructors }

function TFecConstructors.DispatchPacket(binding: TIdSocketHandle; p_IncludesFecHeader: pbyte;
  pl: ni; out fec: PFECConstructor): TFECResult;
var
  fech: TFECPacketHeader;
  con: PFECConstructor;
begin
  //Perform under external LOCK!
  if pl < sizeof(fech) then
    exit(fecInvalidDAta);

  fech := PFecPacketHeader(p_IncludesFecHeader)^;
  if not Fech.IsValid then
    exit(fecInvalidData);

  //Find a constructor
  con := FindConstructor(binding, fech.pid, fech.checksum, fech.partof);
  if con <> nil then begin
    if con.complete then begin
      con.init;//Initialize the constructor, we've gotten +1 more packets than we needed (no parity errors)
      exit(fecStaleData);
    end;
  end;
  //if a constructor for this packet was not found, then create a new constructor
  if con = nil then
    con := NewConstructor(binding, fech.pid, fech.checksum, false, fech.partof);
  //if STILL we don't have a constructor, then hijack a completed constructor
  if con = nil then
    con := NewConstructor(binding, fech.pid, fech.checksum, true, fech.partof);
  if con = nil then
    con := HijackOldestConstructor(binding, fech.pid, fech.checksum, true, fech.partof);

  if con = nil then
    exit(fecResourceFlood);

  result := con.DispatchPacket(@fech, p_IncludesFecHeader+sizeof(fech), pl-sizeof(fech));
  fec := con;

end;

function TFecConstructors.FindConstructor(binding: TIdSocketHandle;
  pid: cardinal; expected_sum: cardinal; parts: ni): PFECConstructor;
var
  idx,cx: ni;
  pc: PFECConstructor;
begin
  Lock;
  try
    cx := Length(FConstructors);
    idx := last_constructor_idx;
    while cx > 0 do begin
      pc := @FConstructors[idx];
      if ((pc^.assigned))
      and (pc^.Binding = binding)
      and (pc^.expected_checksum = expected_sum)
      and (pc^.partof = parts)
      and (pc^.packetid = pid) then begin
        last_constructor_idx := idx;
        exit(pc);
      end;
      inc(idx);
      if (idx>high(Fconstructors)) then
        idx := 0;

      dec(cx);
    end;
    exit(nil);
  finally
    Unlock;
  end;
end;

function TFecConstructors.HijackOldestConstructor(binding: TIdSocketHandle; pid,
  expected_sum: cardinal; bCheckCompleted: boolean; parts: ni): PFECConstructor;
var
  idx,cx: ni;
  pc: PFECConstructor;
  tm, tmNow: ticker;
  maxage: ticker;
  maxidx: ni;
begin
  tmNow := GEtTicker;
  Lock;
  try
    cx := Length(FConstructors);
    idx := last_constructor_idx;
    maxidx := idx+length(FConstructors)-1;
    maxidx := maxidx mod length(FConstructors);
    maxage := 0;
    while cx > 0 do begin
      pc := @FConstructors[idx];
      gettimesince(tmNow, pc^.assign_time);
      tm := gettimesince(tmNow, pc.assign_time);
      if tm > maxage then begin
        maxage := tm;
        maxidx := idx;
      end;


      inc(idx);
      if (idx>high(Fconstructors)) then
        idx := 0;

      dec(cx);
    end;

    pc := @FConstructors[maxidx];
    last_constructor_idx := maxidx;
    pc^.AssignTo(binding, pid, expected_sum, parts);
      //Debug.Consolelog('NEw Constructor '+inttostr(pid));
    exit(pc);
  finally
    Unlock;
  end;
end;
function TFecConstructors.NewConstructor(binding: TIdSocketHandle;
  pid: cardinal; expected_sum: cardinal; bCheckCompleted: boolean; parts: ni): PFECConstructor;
var
  idx,cx: ni;
  pc: PFECConstructor;
begin
  Lock;
  try
    cx := Length(FConstructors);
    idx := last_constructor_idx;
    while cx > 0 do begin
      pc := @FConstructors[idx];
      if ((pc^.assigned = false)  or (bCheckCOmpleted and pc^.completed))
      or (gettimesince(pc^.assign_time) > 4000) then begin
        last_constructor_idx := idx;
        pc^.AssignTo(binding, pid, expected_sum, parts);
        //Debug.Consolelog('NEw Constructor '+inttostr(pid));
        exit(pc);
      end;

      inc(idx);
      if (idx>high(Fconstructors)) then
        idx := 0;

      dec(cx);
    end;
//    Debug.Consolelog('Out of Constructor SLOTS! '+inttostr(pid));
    exit(nil);
  finally
    Unlock;
  end;
end;

procedure TFecConstructors.SetSErver(const Value: TFECServer);
var
  t: ni;
begin
  FServer := Value;
  for t:= 0 to high(FConstructors) do begin
    FConstructors[t].server := server;
  end;

end;

{ TFECPart }

procedure TFECPart.GivePacket(p: pbyte; cnt: ni);
begin
  fillmem(@bytes[0], cnt, 0);
  movemem32(@bytes[0], p, cnt);
  byte_size := cnt;
end;

procedure TFECPart.INit;
begin
  FillMem(@bytes[0], sizeof(bytes),0);
end;

{ TFECQueueItem }

procedure TFECQueueItem.Detach;
begin
  if data <> nil then
    FreeMemory(data);

  data := nil;

  inherited;


end;

procedure TFECQueueItem.DoExecute;
begin
  inherited;
  srv.ProcessUDPReadOuter(self);
end;

procedure TFECQueueItem.Init;
begin
  inherited;
  autodestroy := true;
end;

{ TFECQueueItem_OUT }

procedure TFECQueueItem_OUT.Detach;
begin
{$IFDEF POINTERS}
  if data <> nil then

    FreeMemory(data);

  data := nil;
{$ENDIF}


  inherited;

end;

procedure TFECQueueItem_OUT.DoExecute;
var
  idb: TIDBytes;
begin
  inherited;
//  ecs(srv.csLock);
  try
{$IFDEF POINTERS}
    setlength(idb, self.datalen);
    movemem32(@idb[0], self.data, self.datalen);
    srv.SendBuffer(self.PeerIP, self.PeerPort, idb);
{$ELSE}
    srv.SendBuffer(self.PeerIP, self.PeerPort, self.data);
{$ENDIF}
  finally
//    lcs(srv.csLock);
  end;

end;

procedure TFECQueueItem_OUT.Init;
begin
  inherited;

end;

{ TSimplePacketOutQueue }

{$IFDEF CUSTOM_ORDERING}
function TSimplePacketOutQueue.GetNextItem: TQueueItem;
var
  t: ni;
  itm: fec.TFECQueueItem;
  iBest: ni;
  rnd: ni;
begin
  if FWorkingItems.count = 0 then
    exit(nil);

  rnd := random(FWorkingItems.count-1);
  exit(FWorkingItems[rnd]);


  itm := TFECQueueItem(FWorkingItems[0]);
  iBest := itm.datalen;
  result := itm;

  for t := 1 to FWorkingItems.count-1 do begin
    itm := TFECQueueItem(FWorkingItems[t]);
    if (itm.datalen) > iBest then begin
      result := itm;
      iBest := itm.datalen;
    end;
  end;
end;
{$ENDIF}

{ TSimplePAcketInQueue }

function TSimplePAcketInQueue.GetNextItem: TQueueItem;
var
  t: ni;
  itm: fec.TFECQueueItem;
  iBest: ni;
begin
  exit(inherited);
  if FWorkingItems.count = 0 then
    exit(nil);

  itm := TFECQueueItem(FWorkingItems[0]);
  iBest := itm.datalen;
  result := itm;

  for t := 1 to FWorkingItems.count-1 do begin
    itm := TFECQueueItem(FWorkingItems[t]);
    if (itm.datalen) < iBest then begin
      result := itm;
      iBest := itm.datalen;
    end;
  end;
end;

{ TSimplePacketOutQueue }

{$IFDEF THROTTLEOUT}
function TSimplePacketOutQueue.GetNextItem: TQueueItem;
var
  t: ni;
  itm: TQueueItem;
begin

  result := nil;
//  while result = nil do begin
    for t:= 0 to FWorkingITems.Count-1 do begin
      itm := FWorkingItems[0];
      if (itm = nil) and (FWorkingItems.count > 0) then
        raise ECritical.create('');
      if TFECQueueItem_OUT(itm).FutureTime < GetHighResTicker then begin
        result := itm;
        lasttxtime := GetHighResTicker;
        lasttxbytes := TFECQueueItem(result).datalen;
        exit;
      end else begin
        //Debug.Consolelog('throttle delay');
      end;
    end;
//  end;


end;
procedure TSimplePacketOutQueue.Init;
begin
  ticks_per_byte := (10*MILLION)/((1000*MILLION)/8);

  inherited;

end;

function TSimplePacketOutQueue.QuotaAVailable: boolean;
var
  tmSince: ticker;
begin
  tmSince := gettimesincehr(lasttxtime);
  result := tmSince > (ticks_per_byte * (lasttxbytes));
end;

procedure TSimplePacketOutQueue.SlowDown;
begin
  ticks_per_byte := ticks_per_byte * 2;
end;

procedure TSimplePacketOutQueue.SpeedUp;
begin
  ticks_per_byte := ticks_per_byte / 2;
end;


{$ENDIF}



{ TFECReport }


procedure TFECReport.Init;
begin
  fillmem(@self, sizeof(self), 0);
end;

function TFECReport.packets: ni;
begin
  result := parts*(retry_estimate+1);
end;


function TFECReport.Quality: single;
begin
  result := 1-(packetslost/packets);
end;

end.
