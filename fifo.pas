unit fifo;
{$INLINE AUTO}

interface

uses
  betterobject, sharedobject, stringx, systemx, typex;

CONST
  FIFO_DEFAULT_SIZE = 1024;
type
  TFifo<T> = class(TSharedObject)
  private
    a: array of T;
    getptr: nativeint;
    putptr: nativeint;
    function GetSize: nativeint;
    procedure SetSize(const Value: nativeint);
    procedure Advance(var v: nativeint; iBy: nativeint = 1);
  public
    constructor Create;override;
    destructor Destroy;override;
    property Size: nativeint read GetSize write SetSize;
    procedure Put(const val: T);
    function Get: T;
    function AvailableData: nativeint;
    function Availablespace: nativeint;
  end;

  TByteFifo = class(TFifo<byte>)
  public
    function DebugContents: string;
  end;


implementation

{ TFifo<T> }

procedure TFifo<T>.Advance(var v: nativeint; iBy: nativeint);
begin
  v := v + iBy;
  if v>size then
    v := v mod size;
  if v < 0 then begin
    while (v < 0) do v := v + size;//todo 5 : make this more efficient
  end;


end;

function TFifo<T>.AvailableData: nativeint;
begin
  Lock;
  try
    result := PutPTr-GetPtr;
    if result < 0 then
      result := result + size;
  finally
    Unlock;
  end;

end;

function TFifo<T>.Availablespace: nativeint;
begin
  result := size-AvailableData;
end;

constructor TFifo<T>.Create;
begin
  inherited;
  Size := FIFO_DEFAULT_SIZE;
end;

destructor TFifo<T>.Destroy;
begin

  inherited;
end;

function TFifo<T>.Get: T;
begin
  Lock;
  try
    result := a[getptr];
    advance(getptr);
  finally
    Unlock;
  end;
end;

function TFifo<T>.GetSize: nativeint;
begin
  result := length(a);
end;

procedure TFifo<T>.Put(const val: T);
begin
  Lock;
  try
    a[putptr] := val;
    advance(putptr);
  finally
    Unlock;
  end;
end;

procedure TFifo<T>.SetSize(const Value: nativeint);
begin
  setlength(a, value);
end;

{ TByteFifo }

function TByteFifo.DebugContents: string;
begin
  result := memorydebugstring(@a[0], size)+CRLF;
  result := result + StringRepeat('   ', self.getptr)+'^get'+CRLF;
  result := result + StringRepeat('   ', self.putptr)+'^put'+CRLF;




end;

end.
