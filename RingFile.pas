unit RingFile;

interface

uses
  typex, systemx, sharedobject, QueueStream, helpers.stream, numbers, sysutils, signals, stringx, tickcount, betterobject;

type
  TRingFile = class(TSharedObject)
  private
    FSize: int64;
    function GetDataAvailable: int64;
    function GetSpaceAvailable: int64;
    procedure SetSize(const Value: int64);
    function GetfileName: string;
    procedure SetFileNAme(const Value: string);
  protected
    FWritePtr: int64;
    FReadPtr: int64;
    OpenAllSignals: boolean;
    FSTream: TAdaptiveQueuedFileStream;

    procedure MovePtr(ptr: PInt64; by: int64);
  public
    evEmpty, evData: TSignal;
    constructor Create;override;
    destructor Destroy;override;
    procedure Detach;override;
    property FileName: string read GetfileName write SetFileNAme;
    function PutData(const p: PByte; const l: ni): ni;
    function GetData(const p: PByte; const l: ni): ni;
    function GuaranteePutData(const p: PByte; const l: ni): ni;
    function GuaranteeGetData(const p: pbyte; const l: ni; iTimeOut: ni = 16000): ni;
    property SpaceAvailable: int64 read GetSpaceAvailable;
    property DataAvailable: int64 read GetDataAvailable;
    property Size: int64 read FSize write SetSize;
    function DebugString: string;
  end;


implementation

{ TRingFile }

constructor TRingFile.Create;
begin
  inherited;
  evEmpty := TSignal.create;
  signal(evEmpty, true);
  evData := TSignal.create;
  FSize := 1000*MILLION;


end;

function TRingFile.DebugString: string;
begin
  result := 'sz='+commaize(size)+' w='+commaize(FWritePtr)+' r='+commaize(FReadptr)+' '+FileNAme;
end;

destructor TRingFile.Destroy;
begin

  inherited;
end;

procedure TRingFile.Detach;
begin
  OpenAllSignals := true;
  signal(evData,true);
  signal(evEmpty,true);

  FStream.free;
  FStream := nil;
  inherited;

end;

function TRingFile.GetData(const p: PByte;const  l: ni): ni;
var
  icanGet: int64;
begin
  Lock;
  try
    icanGet := FWritePtr - FReadPtr;
    if iCanGet < 0 then begin
      iCanGet := Fsize - FReadPtr;
    end;

    iCanGet := lesserof(l, iCanGet);
    FStream.Seek(FReadPtr, 0);
    result := Stream_Guaranteeread(FStream, p, iCanGet);
    moveptr(@FReadPtr, result);
    Signal(evEmpty, (DataAvailable=0)  or OpenAllSignals);
    Signal(evDAta, (DataAvailable>0)  or OpenAllSignals);
  finally
    Unlock;
  end;

end;

function TRingFile.GetDataAvailable: int64;
begin
  result := FWritePtr - FReadPtr;
  if result < 0 then
    result := result + FSize;
end;

function TRingFile.GetfileName: string;
begin
  result := '';
  if FStream = nil then
    exit;

  result := FStream.FileNAme;

end;

function TRingFile.GetSpaceAvailable: int64;
begin
  result := FSize - DataAvailable;
end;

function TRingFile.GuaranteeGetData(const p: pbyte; const l: ni; iTimeOut: ni = 16000): ni;
var
  tm: ticker;
begin
  result := GetData(p, l);
  tm := GetTicker;
  while result < l do begin
    if WaitForSignal(evData, 1000) then begin
      result := result + GetData(@p[result], l-result);
      tm := GetTicker;
    end else
      if GetTimeSince(tm) > iTimeOut then begin
        raise ECritical.create('Ring File timeout');
      end;


  end;

end;

function TRingFile.GuaranteePutData(const p: PByte; const l: ni): ni;
begin
  result := PutData(p, l);
  if result < l then
    result := result + PutData(@p[result], l-result);
end;

procedure TRingFile.MovePtr(ptr: PInt64; by: int64);
begin
  inc(Ptr^, by);
  while Ptr^ >= FSize do
    dec(Ptr^, size);

  while ptr^ < 0 do
    inc(ptr^, size);

end;

function TRingFile.PutData(const p: PByte; const l: ni): ni;
var
  icanPut: int64;
begin
  Lock;
  try
    //determine how many we can put in this iteration
    //iCanPut is the number of bytes until wrap around
    //aaaaaaaa      bbbbbbbb        cccccccc  dddddddd
    //[--++--]      [+---++]        [++--++]  [------]
    //[  r w ]      [ w  r ]        [  w r ]  [  &   ]
    //[012345]      [012345]        [012345]  [012345]

    icanPut := (FReadPtr-FWritePtr)-1; //iCanPut = (a:(2-4)=-2 b:4-1=3 c: (4-2)=2 d: 2-2=0) - 1 = a:-3 b:3 c:1 d: -1
    if iCanPut < 0 then begin
    iCanPut := Fsize - FWritePtr; //iCanPut = a: 3, 3, 1,5
    end;

    iCanput := lesserof(l, iCanPut);

    Stream_Grow(FStream, FWritePtr);
    FStream.Seek(FWritePtr, 0);
    result := Stream_GuaranteeWrite(FStream, p, iCanPut);
    moveptr(@FWritePtr, result);
    Signal(evData, (DataAVailable>0) or OpenAllSignals);
  finally
    unlock;
  end;

end;

procedure TRingFile.SetFileNAme(const Value: string);
begin
  if FStream <> nil then begin
    FStream.free;
    FStream := nil;
  end;

  FStream := TAdaptiveQueuedFileStream.Create(value, fmOpenREadWrite+fmShareExclusive);


end;

procedure TRingFile.SetSize(const Value: int64);
begin
  if DataAvailable > 0 then
    raise ECritical.Create('Cannot set size of '+self.ClassName+' when ' +inttostr(DataAvailable)+' bytes are in the buffer.  Buffer must be empty.');

  FSize := Value;

end;

end.
