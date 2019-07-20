unit NetworkBuffer;
//
{$I delphidefs.inc}
interface

uses
  Classes, SysUtils,
{$IFDEF IOS}
//  stringx.ios.ansi,
{$ENDIF}
  variants, stringx, typex, systemx, tickcount;



type
  TNetworkBuffer = class
  private
    FAllocSize: integer;
    FAssigned: boolean;
    FLength: integer;
    FSeqPos: integer;
    slPrivateLog: TStringlist;
    procedure GrowBuffer(iLength: cardinal);inline;
    procedure AlterIPos(var iPos: nativeint);inline;

  public
    FRealBuffer : PByte;
    FSize: integer;
    constructor Create(size: integer);reintroduce;virtual;
    destructor Destroy; override;

    procedure SaveDebugPacket(sErrorTag: string);
    procedure PrivateLog(sMessage: string);


    procedure AssignBuffer(buffer: PByte; iLength: integer; iAllocatedLength: integer = -1);inline;
    property RawBuffer: PByte read FRealBuffer;
    property IsAssigned: boolean read FAssigned write FAssigned;


    procedure PokeByte(b: byte; iPos: nativeint = -1);inline;
    procedure PokeDateTime(dt: TDateTime; iPos: nativeint = -1);inline;
    procedure PokeDouble(d: double; iPos: nativeint= -1);inline;
    procedure PokeLong(l: integer; iPos: nativeint = -1);inline;
    procedure PokeLongLong(i: int64; iPos: nativeint = -1);inline;
    procedure PokeShort(i: smallint; iPos: nativeint = -1);inline;
    procedure PokeBoolean(b: boolean; iPos: nativeint = -1);inline;
    procedure PokeFmtString(s: string; iPos: nativeint = -1);inline;
    procedure PokeRawString(s: string; iPos: nativeint = -1);inline;
    procedure PokeZeroString(s: string; iPos: nativeint = -1);inline;
    procedure pokeBytes(ptr: PByte;iPos: nativeint; lLength: nativeint = -1);inline;


    //The following private functions get stuff from the internal buffer at the
    //given address.  The values are decoded in "network order".
    function PeekByte(iPos: nativeint): byte;
    function PeekDateTime(iPos: nativeint) : TDateTime;
    function PeekDouble(iPos: nativeint): double;
    function PeekLong(iPos: nativeint): integer;
    function PeekLongLong(iPos: nativeint): int64;
    function PeekShort(iPos: nativeint): smallint;
    function PeekBoolean(iPos: nativeint): boolean;
    function PeekRawString(iPos: nativeint; lLength: nativeint): string;
    function PeekFmtString(iPos: nativeint): string;
    function PeekRawBytes(iPos: nativeint; lLength: int64): variant;
    function PeekFmtBytes(iPos: nativeint; out iLength: int64): PByte;

    procedure Reset;
    property Length: integer read FLength;
    procedure AllocBuffer(iSize: integer);
  end;

type
  ConvertRecord = record
    case integer of
      0 : (ArraySide: array[1..8] of byte);
      1 : (dVal : double);
  end;

implementation

uses Debug;

//############################################################################


//------------------------------------------------------------------------------
constructor TNetworkBuffer.create(size: integer);
var
  t: integer;
begin
  inherited Create;
  slPrivateLog := TStringlist.Create;
  AllocBuffer(size);

  FAssigned:= false;
  FSize := size;

  //glog.Debug('**New Packet**', 'packet');

end;

//------------------------------------------------------------------------------
destructor TNetworkBuffer.destroy;
begin
  if FRealBuffer <> nil then
    FreeMem(FRealBuffer);
  slPrivateLog.free;
  slPrivateLog := nil;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TNetworkbuffer.pokeByte(b: byte; iPos: nativeint);
//puts a byte "b" into buffer at Position "iPos"
begin
  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke byte '+inttostr(b)+'@'+inttostr(iPos), 'packet');
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke byte @'+inttohex(iPos, 4)+' '+inttostr(b));
{$ENDIF}

  AlterIPos(iPos);
  GrowBuffer(iPos+1+1);
  FRealBuffer[iPos] := b;
  inc(FSeqPos, 1);



end;
//------------------------------------------------------------------
procedure TNetworkbuffer.pokeShort(i: smallint; iPos: nativeint);
//puts a short into the buffer in network order;
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke short @'+inttohex(iPos, 4)+' '+inttostr(i));
{$ENDIF}
  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke short '+inttostr(i)+'@'+inttostr(iPos), 'packet');
  AlterIPos(iPos);
  GrowBuffer(iPos+1+2);
  FRealBuffer[iPos+1] := ((i and $FF00) shr 8);
  FRealBuffer[iPos+0] := (i and $00FF);
  inc(FSeqPos, 2);
end;
//------------------------------------------------------------------------------
procedure TNetworkbuffer.pokeLong(l: integer; iPos: nativeint);
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke long @'+inttohex(iPos, 4)+' '+inttostr(l));
{$ENDIF}

  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke long '+floattostr(l)+'@'+inttostr(iPos), 'packet');
  AlterIPos(iPos);
  GrowBuffer(iPos+1+4);
  FRealBuffer[iPos+3] := ((l and $FF000000) shr 24);
//  showmessage(inttohex(ord(FRealBuffer[iPos+0]),2));

  FRealBuffer[iPos+2] := ((l and $00FF0000) shr 16);
//  showmessage(inttohex(ord(FRealBuffer[iPos+1]),2));

  FRealBuffer[iPos+1] := ((l and $0000FF00) shr 8);
//  showmessage(inttohex(ord(FRealBuffer[iPos+2]),2));

  FRealBuffer[iPos+0] := ((l and $000000FF));
//  showmessage(inttohex(ord(FRealBuffer[iPos+3]),2));
  inc(FSeqPos, 4);
end;
//------------------------------------------------------------------------------
procedure TNetworkbuffer.pokeRawString(s: string; iPos: nativeint);
//puts a string (S) into buffer at Position (iPos)
var
  t: integer;
  pc: PWideChar;
  pb: PByte;
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke Raw String @'+inttohex(iPos, 4)+' '+s);
{$ENDIF}

  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke raw string "'+s+'"@'+inttostr(iPos), 'packet');
  AlterIPos(iPos);
  GrowBuffer(iPos+sizeof(char)+(system.length(s)*sizeof(char)));
  //we might be able to optomize this
  for t:= STRZ to (system.length(s)-1)+STRZ do begin
    pc := @s[t];
    Pb := pbyte(pc);

    FRealBuffer[iPos+((t-STRZ)*sizeof(char))+0] := pb[0];
    FREalBuffer[iPOs+((t-STRZ)*sizeof(char))+1] := pb[1];
  end;

  inc(FSeqPos, system.length(s)*sizeof(char));

end;
//------------------------------------------------------------------------------
procedure TNetworkbuffer.pokeFmtString(s: string; iPos: nativeint);
//puts a string (S) into buffer at Position (iPos)
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke Formatted String @'+inttohex(iPos, 4)+' '+s);
{$ENDIF}
  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke fmt string "'+s+'"@'+inttostr(iPos), 'packet');
  AlterIPos(iPos);
  GrowBuffer(iPos+1+8+(system.length(s)*sizeof(char)));
  PokeLongLong(system.length(s), iPos);
  PokeRawString(s, iPos+8);
  //inc(FSeqPos, ((system.length(s))*sizeof(char))+);
end;

//------------------------------------------------------------------------------
procedure TNetworkbuffer.pokeBytes(ptr: PByte;iPos: nativeint; lLength: nativeint);
var
  t: integer;
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke Bytes @'+inttohex(iPos, 4)+' length '+inttostr(lLength));
{$ENDIF}

  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke bytes('+inttostr(lLength), 'packet');
  AlterIPos(iPos);
  GrowBuffer(iPos+1+lLength);
//  for t:= 0 to lLength-1 do begin
//    FRealBuffer[iPos+t] := ptr[t];
//  end;
  if (ptr <> nil) and (lLength > 0) then begin
    MoveMem32(@FRealBuffer[iPos], ptr, lLength);
  end;
  inc(FSeqPos, lLength);
end;
//------------------------------------------------------------------------------
function TNetworkBuffer.PeekByte(iPos: nativeint): byte;
begin
  result := ord(FRealBuffer[iPos]);
{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek Byte @'+inttohex(iPos, 4)+ 'got:'+inttostr(result)+' ('+inttohex(result,2)+')');
{$ENDIF}
end;
//------------------------------------------------------------------------------
function TNetworkBuffer.PeekShort(iPos: nativeint): smallint;
begin
  result := (ord(FRealBuffer[iPos+1]) shl 8)+ord(FRealBuffer[iPos+0]);
{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek Short @'+inttohex(iPos, 4)+ 'got:'+inttostr(result)+' ('+inttohex(result,4)+')');
{$ENDIF}
end;
//------------------------------------------------------------------------------
function TNetworkBuffer.PeekLong(iPos: nativeint): integer;
begin
  result := (ord(FRealBuffer[iPos+3]) shl 24)+(ord(FRealBuffer[iPos+2]) shl 16)+
            (ord(FRealBuffer[iPos+1]) shl 8)+ord(FRealBuffer[iPos+0]);
{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek Long @'+inttohex(iPos, 4)+ 'got:'+inttostr(result)+' ('+inttohex(result,8)+')');
{$ENDIF}
end;
//------------------------------------------------------------------------------
function TNetworkBuffer.PeekRawString(iPos: nativeint; lLength: nativeint ): string;
var
  pc: PWideChar;
  ch: WideChar;
  t: integer;
begin

  SetLength(result, lLength);
  for t:= STRZ to ((lLength-1)+STRZ) do begin
    ch := PWideChar(@FRealbuffer[iPos])[(t-STRZ)];
    result[t] := char(ch);
  end;

{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek Raw String @'+inttohex(iPos, 4)+ 'got:'+result);
{$ENDIF}

end;

//------------------------------------------------------------------------------
function TNetworkBuffer.PeekFmtBytes(iPos: nativeint;
  out iLength: int64): PByte;
begin
  iLength := PeekLonglong(iPos);
  system.GetMem(result, iLength);
  moveMem32(result, @FRealBuffer[iPos+8], iLength);


{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek Fmt Bytes @'+inttohex(iPos, 4)+ 'got '+inttostr(iLength)+' bytes');
{$ENDIF}

end;


function TNetworkBuffer.PeekFmtString(iPos: nativeint): string;
begin
  result := PeekRawString(iPos+8, PeekLongLong(iPos));

{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek Fmt String @'+inttohex(iPos, 4)+ 'got '+result);
{$ENDIF}

end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
function TNetworkBuffer.PeekRawBytes(iPos: nativeint; lLength: int64): variant;
var
  t: nativeint;
  pb: PByte;
  b: byte;
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek Raw bytes shouldn''t ever be called!!');
{$ENDIF}
  raise exception.create('this is shit...rewrite it');
//  for t:= 0 to lLength-1 do begin
//    FRealBuffer[t] := FRealBuffer[iPos+t]
//  end;
  result := VarArrayCreate([0, lLength-1], varByte);
  for t := 0 to lLength-1 do begin
    b := FRealBuffer[iPos+t];

    result[t] := b;
  end;
end;
//------------------------------------------------------------------------------
procedure TNetworkBuffer.AssignBuffer(buffer: PByte; iLength: integer; iAllocatedLength: integer = -1);
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Buffer ASSIGNED from external source length:'+inttostr(iLength));
{$ENDIF}
  FreeMem(FRealBuffer);
  FRealBuffer := buffer;
  FAssigned := true;
  FLength := iLength;
  if iAllocatedLength < 0 then
    FSize := iLength
  else
    FSize := iAllocatedLength;

end;
//------------------------------------------------------------------------------
function TNetworkBuffer.PeekDouble(iPos: nativeint): double;
var
  DInst : ConvertRecord;
  i     : integer;
begin
  for i := 0 to 7 do begin
    DInst.ArraySide[1 + i] := Ord(FRealBuffer[iPos + i]);
  end;
  try
    result := DInst.DVal;
{$IFDEF PRIVATE_LOG}
    PrivateLog('Peek Double @'+inttohex(iPos, 4)+ 'got:'+floattostr(result));
{$ENDIF}
  except
    on e: exception do begin
      e.message := 'Error during double conversion data is ['+inttohex(DInst.ArraySide[1],2)+','+inttohex(DInst.ArraySide[2],2)+','+inttohex(DInst.ArraySide[3],2)+','+inttohex(DInst.ArraySide[4],2)+','+inttohex(DInst.ArraySide[5],2)+','+inttohex(DInst.ArraySide[6],2)+','+inttohex(DInst.ArraySide[7],2)+','+inttohex(DInst.ArraySide[8],2)+'] '+e.message;
      PrivateLog('**'+e.Message);
      raise;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TNetworkBuffer.PokeDouble(d: double; iPos: nativeint);
var
  DInst : ConvertRecord;
  i     : integer;
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke double @'+inttohex(iPos, 4)+' '+floattostr(d));
{$ENDIF}

  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke double '+floattostr(d)+'@'+inttostr(iPos), 'packet');
  AlterIPos(iPos);
  GrowBuffer(iPos+1+8);
  DInst.DVal := d;
  for i := 0 to 7 do begin
    FRealBuffer[iPos + i] := (DInst.ArraySide[1 + i]);
  end;
  inc(FSeqPos, 8);
end;
//------------------------------------------------------------------------------
function TNetworkBuffer.PeekDateTime(iPos: nativeint): TDateTime;
var
  d: double;
begin
  d := PeekDouble(iPos);
//  d :=0.0;
  result := TdateTime(d);

{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek DateTime @'+inttohex(iPos, 4)+ 'got '+datetimetostr(result));
{$ENDIF}
end;
//------------------------------------------------------------------------------
procedure TNetworkBuffer.PokeDateTime(dt: TDateTime; iPos: nativeint);
begin
  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke datetime '+datetimetostr(dt)+'@'+inttostr(iPos), 'packet');
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke DateTime @'+inttohex(iPos, 4)+ 'dt '+datetimetostr(dt));
{$ENDIF}
  AlterIPos(iPos);
  GrowBuffer(iPos+1+8);
  PokeDouble(double(dt), iPos);
  inc(FSeqPos, 8);
end;
//------------------------------------------------------------------------------
function TNetworkBuffer.PeekLongLong(iPos: nativeint): int64;
var
  iHigh: integer;
  iLow: integer;
begin
  iHigh := peeklong(iPos+4);
  iLow := peeklong(iPos+0);

  result := (int64(iHigh) shl 32) or cardinal(iLow);

{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek LongLong (int64) @'+inttohex(iPos, 4)+ 'got:'+inttostr(result)+' ('+inttohex(result,16)+')');
{$ENDIF}

end;
//------------------------------------------------------------------------------
procedure TNetworkBuffer.PokeLongLong(i: int64; iPos: nativeint);
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke longlong @'+inttohex(iPos, 4)+' '+inttostr(i));
{$ENDIF}

  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke longlong '+inttostr(i)+'@'+inttostr(iPos), 'packet');
  AlterIPos(iPos);
  GrowBuffer(iPos+1+8);
  PokeLong(integer((i and $FFFFFFFF00000000) shr 32), iPos+4);
  PokeLong(integer(i and $00000000FFFFFFFF), iPos);
  inc(FSeqPos, 8);
end;

function TNetworkBuffer.PeekBoolean(iPos: nativeint): boolean;
begin
  result := PeekByte(iPos) <> 0;

{$IFDEF PRIVATE_LOG}
  PrivateLog('Peek Boolean (int64) @'+inttohex(iPos, 4)+ 'got:'+booltostr(result));
{$ENDIF}
end;

procedure TNetworkBuffer.PokeBoolean(b: boolean; iPos: nativeint);
begin
  //glog.Debug('['+inttohex(integer(pointer(self)),8)+']Poke bool '+booltostr(b)+'@'+inttostr(iPos), 'packet');
  AlterIPos(iPos);
  GrowBuffer(iPos+1+1);
  if b then
    PokeByte(1, iPos)
  else
    PokeByte(0, iPos);

  inc(FSeqPos, 1);
end;

procedure TNetworkBuffer.GrowBuffer(iLength: cardinal);
var
  nc: Pchar;
begin
  if int64(iLength) > int64(FLength) then
    FLength := iLength-1;
  if int64(iLength) > int64(FAllocSize) then begin
    Reallocmem(FrealBuffer, 1 shl (HighOrderBit(iLength+1)+1));
  end;

//    nc := FRealBuffer;
//    Allocbuffer(1 shl (HighOrderBit(iLength+1)+1));
//    MoveMem32((@FRealbuffer[0]), (@nc[0]), FSize);
//    FreeMem(nc);
//  end;

end;



procedure TNetworkBuffer.AllocBuffer(iSize: integer);
begin
  system.GetMem(FRealBuffer, iSize);
  FAllocSize := iSize;
end;

procedure TNetworkBuffer.AlterIPos(var iPos: nativeint);
begin
  if iPos < 0 then begin
    iPos := FSeqPos;
  end;
end;

procedure TNetworkBuffer.PokeZeroString(s: string; iPos: nativeint);
var
  t: integer;
  iLen: integer;
begin
{$IFDEF PRIVATE_LOG}
  PrivateLog('Poke Zero String @'+inttohex(iPos, 4)+' '+s);
{$ENDIF}

  raise Exception.create('deprecated');
  iLen := system.length(s);
  for t:= 1 to iLen do begin
    if s[t] = #0 then begin
      iLen := t-1;
    end;
  end;

  AlterIPos(iPos);
  GrowBuffer(iPos+1+iLen);
  //we might be able to optimize this
  for t:= 1 to system.length(s) do begin
    FRealBuffer[iPos+t-1] := (ord((s[t])) and 255);
  end;
  inc(FSeqPos, system.length(s));

  //tag a 0 on the end of the string
  PokeByte(0);


end;

procedure TNetworkBuffer.PrivateLog(sMessage: string);
begin
  if slPrivateLog = nil then
    raise ECritical.create('private log is nil, this indicates Tnetworkbuffer wasn''t properly created.');
  self.slPrivateLog.Add(sMessage);
end;

procedure TNetworkBuffer.Reset;
begin
  self.GrowBuffer(0);
  FSeqPos := 0;

end;

procedure TNetworkBuffer.SaveDebugPacket(sErrorTag: string);
var
  sFilename: string;
begin
  sFileName := DLLPath+inttostr(GetTicker);

  SaveStringAsFile(sFileName+'.txt', sErrorTag+#13#10+'----------------'+slPrivateLog.text);
  SaveMemoryAsFile(@FRealBuffer[0],  FLength, sFileName+'.bin');


end;

end.
