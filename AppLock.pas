unit AppLock;

interface

uses
  sharedobject;



type
  TFakeLock = class(TObject)
  public
    procedure lock;
    procedure unlock;
  end;

  TAppLock = class(TSharedObject)
  public
  end;

var
  AL: TappLock;


implementation

{ TFakeLock }

procedure TFakeLock.lock;
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TFakeLock.unlock;
begin

//TODO -cunimplemented: unimplemented block
end;

initialization
  AL := TAppLock.create;

finalization
  AL.free;

end.
