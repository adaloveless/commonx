unit RingBufferHelper;

interface

uses
  ringbuffer, simpleabstractconnection, numbers, typex;

procedure PumpFromConnectionToRingBuffer(conn: TSimpleAbstractConnection; rb: TRingBuffer);
procedure PumpFromRingBufferToConnection(conn: TSimpleAbstractConnection; rb: TRingBuffer);


implementation



procedure PumpFromConnectionToRingBuffer(conn: TSimpleAbstractConnection; rb: TRingBuffer);
var
  iGet, iGot: ni;
  p: PByte;
begin
  rb.GetWriteableChunkVars(p, iGet);
  if iGet > 0 then begin
    iGot := conn.ReadData(p, iGet);
    rb.MoveWriteHeadPOinter(iGot);
  end;

end;

procedure PumpFromRingBufferToConnection(conn: TSimpleAbstractConnection; rb: TRingBuffer);
var
  cx: ni;
  iGet,iGot: ni;
  p: Pbyte;
begin
  rb.GetReadableChunkVars(p, iGet);
  iGot := conn.SendData(p, iGet, false);
  rb.MoveStompTailPointer(iGot);



end;



end.
