unit helpers.sockets;

interface

uses
  typex,systemx, sockfix, better_sockets, classes, numbers, debug, sysutils,idtcpconnection,idglobal, better_indy;

procedure Socket_GuaranteeWrite(s: TidTCPConnection; p: Pbyte; iLength: ni);overload;
procedure Socket_GuaranteeRead(s: TCustomIPClient; p: pbyte; iLength: ni);overload;
procedure Socket_GuaranteeWrite(s: TCustomIPClient; p: pbyte; iLength: ni);overload;
procedure Socket_GuaranteeWriteStreamPart(str: TStream; rangestart, rangeend_notincluding: int64; s: TCustomIPClient);overload;
function Socket_Read(s: TCustomIPClient; p: pbyte; iLength: ni): ni;overload;






implementation

uses
  helpers_stream;


procedure Socket_GuaranteeWriteStreamPart(str: TStream; rangestart, rangeend_notincluding: int64; s: TCustomIPClient);overload;
var
  a: array[0..511] of byte;
  iPos: int64;
  iRead: ni;
  iTotalRead: ni;
begin
  str.seek(rangeStart, soBeginning);
  iTotalRead := 0;
  if rangeend_notincluding > str.Size then
    rangeend_notincluding := str.size;
  while str.Position <= rangeEnd_notincluding do begin
    iREAd := lesserof((rangeend_notincluding-str.position)+1, 512);
    inc(iTotalRead, iRead);
    if iREad <= 0 then
      break;

    stream_guaranteeread(str, @a[0], iRead);
    Socket_GuaranteeWrite(s, @a[0], iRead);
  end;
  debug.consolelog(inttostr(iTotalRead)+' bytes written.');

end;
function Socket_Read(s: TCustomIPClient; p: pbyte; iLength: ni): ni;
var
  iJustRead: ni;
begin
  iJustRead := 0;
  if s.WaitForData(1) then begin
    iJustRead := s.ReceiveBuf(p[0], iLength);
    if ijustRead = 0 then
      raise ENetworkError.create('connection dropped');
  end;

  result := iJustRead;


end;

//------------------------------------------------------------------------------
procedure Socket_GuaranteeRead(s: TCustomIPClient; p: pbyte; iLength: ni);
var
  iREad, iJustRead: ni;
begin
  iRead := 0;
  while iRead < iLength do begin

    if s.WaitForData(1000) then begin
      iJustRead := s.ReceiveBuf(p[iRead], iLength-iRead);
      if ijustRead = 0 then
        raise ENetworkError.create('connection dropped');
      if iJustRead < 0 then  begin
        Debug.Log('socket returned error');
        sleep(200);
      end;
    end else
      iJustRead := 0;
    inc(iRead, iJustRead);
  end;


end;
//------------------------------------------------------------------------------
procedure Socket_GuaranteeWrite(s: TCustomIPClient; p: pbyte; iLength: ni);
var
  iWrote, iJustWrote: ni;
begin
  iWrote := 0;
  while iWrote < iLength do begin
    iJustWrote := s.SendBuf(p[iWrote], iLength-iWrote);
    if iJustWrote < 0 then begin
      Debug.Log('Unable to guarantee socket write');
      raise ECritical.create('unable to guarantee socket write');
//      sleep(100);
    end;
    inc(iWrote, iJustWrote);
  end;
end;


procedure Socket_GuaranteeWrite(s: TidTCPConnection; p: Pbyte; iLength: ni);overload;
begin
  IOHandler_GuaranteeWrite(s.IOHandler, p, ilength);
end;



end.
