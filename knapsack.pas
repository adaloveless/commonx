unit knapsack;

interface

uses
  betterobject, typex, systemx, sharedobject, better_collections;

type
  TKnapSackItem = class(TBetterObject)
  strict private
    FTag: string;
    FWeight: double;
  private
    FUsed: boolean;
  public
    property Tag: string read FTag write FTag;
    property Weight: double read FWeight write FWeight;
    property Used: boolean read FUsed write FUsed;
  end;


  TKnapSackItems<TYP: TKnapSackItem> = class(TBetterObject)
  strict private
    function GetTotalWeight: double;
  strict
  private
    function GEtCount: nativeint;
  private
    function GetItem(idx: nativeint): TYP; protected
    FItems: TBetterList<TYP>;
    FRunningTotalWeight: double;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Item[idx: nativeint]: TYP read GetItem;default;
    property TotalWeight: double read GetTotalWeight;
    procedure Add(itm: TYP);virtual;
    procedure Remove(itm: TYP);
    procedure Delete(index: nativeint);
    property Count: nativeint read GEtCount;
    procedure ResetUsedFlags;
    procedure ClearAndDestroyItems;
    procedure ClearWithoutDestroyingItems;
  end;

  TKnapSack<TYP: TKnapSackItem> = class(TKnapSackItems<TYP>)
  strict private
    FWeightLimit: double;
    FItemLImit: nativeint;
  public
    property WeightLimit: double read FWeightLimit write FWeightLImit;
    property ItemLimit: nativeint read FItemLImit write FItemLImit;
  end;

implementation

{ TKnapSackItems<TYP> }

procedure TKnapSackItems<TYP>.Add(itm: TYP);
begin
  FItems.add(itm);
  FRunningTotalWeight := FRunningTotalWeight + itm.weight;
end;


procedure TKnapSackItems<TYP>.ClearAndDestroyItems;
var
  itm: TYP;
begin
  while FItems.count > 0 do begin
    itm := FItems.Last;
    FItems.Delete(FItems.count-1);
    itm.free;
    itm := nil;
  end;

end;

procedure TKnapSackItems<TYP>.ClearWithoutDestroyingItems;
var
  itm: TYP;
begin
  while FItems.count > 0 do begin
    itm := FItems.Last;
    FItems.Delete(FItems.count-1);
    //itm.free;
    //itm := nil;
  end;
end;

constructor TKnapSackItems<TYP>.Create;
begin
  inherited;
  FItems := TBetterList<TYP>.create;
end;

procedure TKnapSackItems<TYP>.Delete(index: nativeint);
begin
  FRunningTotalWeight := FRunningTotalWeight - FItems[index].weight;
  FItems.delete(index);
end;

destructor TKnapSackItems<TYP>.Destroy;
begin

  ClearAnddestroyItems;
  FItems.free;
  inherited;
end;

function TKnapSackItems<TYP>.GEtCount: nativeint;
begin
  result := FItems.Count;
end;

function TKnapSackItems<TYP>.GetItem(idx: nativeint): TYP;
begin
  result := FItems[idx];
end;

function TKnapSackItems<TYP>.GetTotalWeight: double;
begin
  exit(FRunningTotalWeight);

end;

procedure TKnapSackItems<TYP>.Remove(itm: TYP);
begin
  FItems.remove(itm);
  FRunningTotalWeight := FRunningTotalWeight - itm.weight;
end;

procedure TKnapSackItems<TYP>.ResetUsedFlags;
var
  t: nativeint;
begin
  for t:= 0 to FItems.count-1 do begin
    Fitems[t].used := false;
  end;

end;

end.
