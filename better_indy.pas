unit better_indy;
//This unit exists to resolve conflicts between the udp component for Xe2, xE3 and Xe4
//which have different definitions for OnUdpRead.  Use OnUDPRead_Multiversion

{$I DelphiDefs.inc}

interface

uses
  IdSocketHandle, IdBaseComponent, classes, systemx, sysutils, debug, fifo, numbers,
  IdComponent, IdUDPBase, IdUDPServer, idglobal, idtcpserver, idiohandler, managedthread, orderlyinit;

type
  TDTUDPListenerThread = TIdUDPListenerThread;

  TDTSocketHandle = TIdSocketHandle;

  TUDPReadEvent_XE3 = procedure(AThread: TIdUDPListenerThread; AData: array of Byte; ABinding: TIdSocketHandle) of object;
  TUDPREadEvent_ALL = TUDPReadEvent_XE3;
{$IFDEF GE_XE4}
//  TidBytes = array of byte;
{$else}
  TidBytes = TArray<byte>;
{$ENDIF}
  TDTBytes = TidBytes;

  TDTUDPServer = class (TIdUDPServer)
  protected
    FOnUdpReadMV: TUDPREadEvent_ALL;
  strict private
    property OnUDPRead;
  published
{$IFDEF GE_XE4}
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); override;
{$else}
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); override;
{$ENDIF}
    property OnUDPRead_Multiversion: TUDPREadEvent_ALL read FOnUdpReadMV write FOnUdpReadMV;
  end;

  TDTTCPServer = class(TidTCPServer)
  public

  end;


  TIndySocketThreader = class(TManagedThread)
  private
    FIOH: TidIOHandler;
  public
    fifo_in: TByteFifo;
    fifo_out: TByteFifo;
    closing: boolean;

    procedure Init;override;
    destructor Destroy;override;

    property IOH: TidIOHandler read FIOH write FIOH;

    procedure DoWork;
    procedure DoExecute;override;
  end;




function IOHandler_GuaranteeRead(ist: TIndySocketThreader; p: PByte; iCount: nativeint): boolean;overload;
function IOHandler_GuaranteeRead(ioh: TidIOHandler; p: PByte; iCount: nativeint): boolean;overload;
function IOHandler_GuaranteeWrite(ist: TIndySocketThreader; p: PByte; iCount: nativeint): boolean;overload;
function IOHandler_GuaranteeWrite(ioh: TidIOHandler; p: PByte; iCount: nativeint): boolean;overload;


implementation

function IOHandler_GuaranteeWrite(ist: TIndySocketThreader; p: PByte; iCount: nativeint): boolean;overload;
var
  iWritten: nativeint;
begin
  iWritten := 0;
  while iWritten < icount do begin
    while ist.fifo_out.AvailableSpace = 0 do sleep(1);
    ist.fifo_out.Put(p[iWritten]);
    inc(iWritten);
  end;
  result := true;
end;
function IOHandler_GuaranteeRead(ist: TIndySocketThreader; p: PByte; iCount: nativeint): boolean;overload;
var
  iRead: nativeint;
begin
  iRead := 0;
  while iRead < icount do begin
    while ist.fifo_in.AvailableData = 0 do sleep(1);
    p[iRead] := ist.fifo_in.Get;
    inc(iRead);
  end;
  result := true;
end;


function IOHandler_GuaranteeRead(ioh: TidIOHandler; p: PByte; iCount: nativeint): boolean;
var
  i: nativeint;
  idb: TIDBytes;
  iRead, iJustread: nativeint;
begin
  i := 0;
//  Debug('Read '+inttostr(iCount)+' bytes from iohandler.');


  iRead := 0;
  while iRead < icount do begin
    iJustRead := lesserof(iCount-iRead, ioh.RecvBufferSize);

//    ioh.CheckForDataOnSource(0);
    ioh.ReadBytes(idb, iJustread,false);
    //bytesToRaw(idb, p[iRead], length(idb));
    movemem32(@p[iRead], @idb[0], iJustRead);
    inc(iRead, iJustRead);
  end;

//  Debug('Done Reading '+inttostr(iCount)+' bytes from iohandler.');

  result := true;

end;


function IOHandler_GuaranteeWrite(ioh: TidIOHandler; p: PByte; iCount: nativeint): boolean;
var
  i: nativeint;
  b: byte;
  bb: TIdBytes;
  iWritten, iToWrite: nativeint;
begin
  i := 0;
  SetLength(bb,iCount);
  movemem32(@bb[0], p, iCount);
//  Debug('Write '+inttostr(iCount)+' bytes to iohandler.');
  if iCount = 48 then begin
//    Debug('bb[0]= '+inttohex(bb[0],2));
  end;
//  ioh.WriteBufferOpen;
  iWritten := 0;
  repeat
//    Debug('Write @'+inttostr(iWritten));
    iToWrite := LesserOf(iCount-iWritten, 65536*64);
    ioh.Write(bb, iToWrite, iWritten);
    inc(iWritten, iToWrite);
//    ioh.CheckForDataOnSource(0);
  until iWritten >= iCount;


//  ioh.WriteBufferFlush;
//  ioh.WriteBufferClose;
//  Debug('Wrote '+inttostr(iCount)+' bytes to iohandler.');
  result := true;
end;

{ TDTUDPServer }




procedure TDTUDPServer.DoUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
begin
  inherited;
  if Assigned(OnUDPRead_MultiVersion) then
    OnUDPRead_MultiVersion(AThread, AData, ABinding);
end;

//procedure Register;
//begin
//  RegisterComponents('Indy_Better', [TDTUDPServer]);
//end;


{ TIndySocketThreader }

procedure TIndySocketThreader.DoWork;
var
  iToRead: nativeint;
  iAvail,i: nativeint;
  outt: TIdBytes;
  idb: TIDBytes;
  b: byte;
begin
  if not closing then
  while ioh.Readable(0) and (fifo_in.Availablespace > 0)  do begin
    Debug.Log(self,'About to read... fifo has '+inttostr(fifo_in.AvailableData));
    b := ioh.ReadByte;
    Debug.Log(self,'Read 0x'+inttohex(b,2));
    fifo_in.Put(b);
  end;

  iAvail := fifo_out.AvailableData;
  if iAvail > 0 then begin
    setlength(idb, iAvail);
    i := 0;
    while i < iAvail do begin
      idb[i] := fifo_out.Get;
      inc(i);

    end;

    ioh.Write(idb);
  end;



end;

destructor TIndySocketThreader.Destroy;
begin
  fifo_in.Free;
  fifo_out.Free;
  fifo_in := nil;
  fifo_out := nil;
  inherited;

end;

procedure TIndySocketThreader.Init;
begin
  inherited;
  fifo_in := TByteFifo.Create;
  fifo_in.Size := 65536*4;
  fifo_out := TByteFifo.Create;
  fifo_out.Size := 65536*4;
end;

procedure TIndySocketThreader.DoExecute;
begin
  inherited;
  while not StopREquested do begin
    try
      DoWork;
    except
      closing := true;
      Stop(true);
      TPM.NoNeedthread(self);
    end;
  end;
end;

procedure oinit;
begin
//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;
procedure ofinal;
begin
//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

initialization
init.RegisterProcs('better_indy', oinit, ofinal, 'managedthread');


end.
