unit RegisteredMemory;

interface

uses
  betterobjectregistry, typex, systemx, betterobject, sharedobject, generics.collections, debug, sysutils;

type
  TRegisteredMemory = class
  private
    procedure RegisterWith;
    procedure UnregisterWith;
  public
    p: pointer;
    size: TSize;
    outer: string;
    constructor Create(size: TSize; sTag: string);virtual;
    destructor Destroy;override;
  end;


  TMemoryRegistry = class(TSharedObject)
  protected
    FList: TList<TRegisteredMemory>;
    function IndexOF(p: pointer): ni;
  public
    procedure Init;override;
    procedure Detach;override;
    procedure FreeByPointer(p: pointer);
    function Allocate(sz: TSize; sTag: string): pointer;
  end;

function GetRegisteredMemory(sz: TSize; sTag: string): pointer;inline;
procedure FreeRegisteredMemory(p: pointer);inline;


var
  memReg: TMemoryRegistry;



implementation

{ TRegisteredMemory }

function GetRegisteredMemory(sz: TSize; sTag: string): pointer;
begin
{$IFDEF REGISTER_MEMORY}
  result := memReg.Allocate(sz, sTag)
{$ELSE}
  result := GetMemory(sz);
{$ENDIF}
end;
procedure FreeRegisteredMemory(p: pointer);
begin
{$IFDEF REGISTER_MEMORY}
  memReg.FreeByPointer(p);
{$ELSE}
  Freememory(p);
{$ENDIF}
end;



constructor TRegisteredMemory.Create(size: TSize; sTag: string);
begin
  inherited CReate;
  self.outer := sTag;
  self.size := size;
  p := GetMemory(size);
  registerwith;

end;

destructor TRegisteredMemory.Destroy;
begin
  FreeMemory(p);
  unregisterwith;
  inherited;
end;

procedure TRegisteredMemory.RegisterWith;
begin
  bor.ObjectCreated(self.ClassType, outer);
end;

procedure TRegisteredMemory.UnregisterWith;
begin
  bor.ObjectDestroyed(self.ClassType, outer);
end;

{ TMemoryRegistry }

function TMemoryRegistry.Allocate(sz: TSize; sTag: string): pointer;
var
  obj: TRegisteredMemory;
begin
  obj := TRegisteredMemory.Create(sz, sTag);
  Lock;
  try
    FList.Add(obj);
    result := obj.p;
  finally
    Unlock;
  end;

end;

procedure TMemoryRegistry.Detach;
var
  t: ni;
begin
  if detached then exit;
  if FLIst.count>0 then begin
    Debug.log(FList.count.ToString+' memory leaks detected!');

    for t := 0 to FList.Count-1 do begin
      Debug.Log('Leak :'+FList[t].size.ToString);
    end;

  end;

  FList.Free;


  inherited;

end;


procedure TMemoryRegistry.FreeByPointer(p: pointer);
var
  idx: ni;
  obj: TRegisteredMemory;
begin
  obj := nil;
  lock;
  try
    idx := IndexOf(p);
    if idx >=0 then begin
      obj := FList[idx];
      FList.Delete(idx);

    end else begin
      raise ECritical.create('Catastrophic memory failure!');
    end;
  finally
    unlock;
    if obj <> nil then
      obj.Free;
  end;

end;

function TMemoryRegistry.IndexOF(p: pointer): ni;
var
  t: ni;
begin
//  lock;
  try
    result := -1;
    for t:= 0 to FList.count-1 do begin
      if FList[t].p = p then
        exit(t);
    end;
  finally
//    unlock;
  end;


end;

procedure TMemoryRegistry.Init;
begin
  inherited;
  FList := TList<TRegisteredMemory>.create;
end;


initialization
  memReg := TMEmoryREgistry.create;

finalization
  memREg.free;
  memREg := nil;

end.
