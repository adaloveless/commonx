unit variantlist;

interface

uses variants, typex, systemx, stringx, sysutils;

type
  TVariantList = class
  private

    function GetItem(idx: integer): variant;
    procedure Setitem(idx: integer; const Value: variant);
    function GetCount: integer;
  protected
    FItems: array of variant;
  public
    property items[idx: integer]: variant read GetItem write Setitem; default;
    property count: integer read GetCount;
    procedure Add(v: variant);
    procedure Delete(idx: integer);
    procedure Clear;

  end;

implementation

{ TVariantList }

procedure TVariantList.Add(v: variant);
var
  idx: integer;
begin
  idx := length(FItems);
  SetLength(FItems, idx+1);
  Fitems[idx] := v;
end;

procedure TVariantList.Clear;
begin
  SetLength(FItems,0);
end;


procedure TVariantList.Delete(idx: integer);
var
  t: integer;
begin
  for t:=idx to high(FItems)-1 do begin
    FItems[t] := FItems[t+1];
  end;
  SetLength(FItems, length(FItems)-1);


end;

function TVariantList.GetCount: integer;
begin
  result := length(FItems);
end;

function TVariantList.GetItem(idx: integer): variant;
begin
  if idx >= count then
    raise ECritical.create('index '+inttostr(idx)+' is out of range of variant list');
  result := FItems[idx];
end;

procedure TVariantList.Setitem(idx: integer; const Value: variant);
begin
  if idx >= count then
    raise ECritical.create('index '+inttostr(idx)+' is out of range of variant list');

  FItems[idx] := value;
end;

end.
