unit WriteBuilder;

interface

uses
  typex, betterobject, systemx;

type
  TWriteBuilder = class(TBetterObject)
  private
    data: PByte;
    datawriteptr: PByte;
    DataCapacity: TSize;
    dataidx: ni;
    FStartPoint: int64;
    procedure SetDataCapacity(const iSz: TSize);
    procedure GrowDataCapacity(const iSz: TSize);
  public
    procedure Init;override;
    procedure Detach;override;
    procedure New;
    property DataSize: ni read dataidx;
    procedure AppendData(const p: Pbyte; sz: TSize);
    property StartPOint: int64 read FStartPoint write FStartPoint;
    property Buffer: PByte read data;

  end;



implementation

{ TWriteBuilder }

procedure TWriteBuilder.AppendData(const p: Pbyte; sz: TSize);
begin
  GrowDataCapacity(DataSize+int64(sz));
  movemem32(datawriteptr, p, sz);
  inc(datawriteptr, sz);
  inc(dataidx, sz);
end;

procedure TWriteBuilder.Detach;
begin
  if detached then exit;
  freememory(data);
  data := nil;

  inherited;

end;

procedure TWriteBuilder.GrowDataCapacity(const iSz: TSize);
begin
  if iSZ > DataCapacity then
    SetDataCapacity(iSz);
end;

procedure TWriteBuilder.Init;
begin
  inherited;

  SetDataCapacity(512);


end;

procedure TWriteBuilder.New;
begin
  dataidx := 0;
  datawriteptr := data;
end;

procedure TWriteBuilder.SetDataCapacity(const iSz: TSize);
begin
  if iSz = DataCapacity then
    exit;

  DataCapacity := iSz;

  if iSz = 0 then begin
    if data <> nil then
      FreeMemory(data);
  end else begin
    if data <> nil then
      data := reallocmemory(data, iSz)
    else
      data := GetMemory(iSZ);
  end;

  datawriteptr := @data[dataidx];//IMPORTANT... base ptr may have changed



end;

end.
