unit BetterObjectRegistry;
{$I DelphiDefs.inc}
interface

uses
  typex, classes, betterobject, generics.collections.fixed, systemx, sysutils;


type
  TBOREntry = class(TObject)
  public
    classref: TClass;
    outerclassref: string;
    count: nativeint;
  end;


  TBetterObjectRegistry = class(TObject)
  private
    FObjects: Tlist<TBOREntry>;
    sect: TCLXCriticalSection;
    function GetObject(idx: integer): TBOREntry;
  public
    constructor Create;virtual;
    destructor Destroy;override;

    function IndexOf(ref: TClass; sTag: string): nativeint;
    procedure ObjectCreated(ref: TClass; sTag: string);
    procedure ObjectDestroyed(ref: TClass; sTag: string);
    property Objects[idx: integer]: TBOREntry read GetObject;
    function Count: nativeint;
    procedure Lock;
    procedure Unlock;
    function GetDebugInfo: string;
  end;


var
  bor: TBetterObjectRegistry;


implementation

uses
  orderlyinit;

{ TBetterObjectRegistry }




function TBetterObjectRegistry.Count: nativeint;
begin
  Lock;
  try
    result := FObjects.Count;
  finally
    Unlock;
  end;
end;

constructor TBetterObjectRegistry.Create;
begin
  systemx.InitializeCriticalSection(sect);
  FObjects := TList<TBOREntry>.create;
end;

destructor TBetterObjectRegistry.Destroy;
begin
  FObjects.free;
  DeleteCriticalSection(sect);
  inherited;
end;

function TBetterObjectRegistry.GetDebugInfo: string;
var
  s: string;
  t: ni;
begin
  result := '';
  for t:= 0 to Count-1 do begin
    result := result+self.Objects[t].classref.ClassName+':'+inttostr(objects[t].count)+#13#10;
  end;



end;

function TBetterObjectRegistry.GetObject(idx: integer): TBOREntry;
begin
  result := nil;
  Lock;
  try
    result := FObjects[idx];
  finally
    Unlock;
  end;
end;

function TBetterObjectRegistry.IndexOf(ref: TClass; sTag: string): nativeint;
var
  t: integer;
  bore: TBOREntry;
begin
  result := -1;
  for t:= 0 to FObjects.Count-1 do begin
    bore := FObjects[t];
    if (bore.classref = ref) and (bore.outerclassref = sTag) then begin
      result := t;
      exit;
    end;
  end;

end;

procedure TBetterObjectRegistry.Lock;
begin
  EnterCriticalSection(sect);
end;

procedure TBetterObjectRegistry.ObjectCreated(ref: TClass; sTag: string);
var
  i: nativeint;
  bor: TBOREntry;
begin
  Lock;
  try
    i := indexof(ref, sTag);
    if  i < 0 then begin
      bor := TBOREntry.create;
      bor.classref := ref;
      bor.outerclassref := sTag;
      FObjects.add(bor);
      i := FObjects.count-1;
    end else
      bor := FObjects[i];

    bor.count := bor.count + 1;


  finally
    Unlock;
  end;
end;

procedure TBetterObjectRegistry.ObjectDestroyed(ref: TClass; sTag: string);
var
  i: nativeint;
  bor: TBOREntry;
begin
  Lock;
  try
    i := indexOf(ref, sTag);
    if i< 0 then
      exit;

    bor := FObjects[i];

    bor.count := bor.count-1;

  finally
    Unlock;

  end;
end;

procedure TBetterObjectRegistry.Unlock;
begin
  LeaveCriticalSection(sect);
end;

procedure oinit;
begin
  bor := TBetterObjectRegistry.Create;
end;
procedure ofinal;
begin
//  bor.free;
//  bor := nil;

end;


initialization

orderlyinit.init.RegisterProcs('betterobjectregistry', oinit, ofinal, '');



end.
