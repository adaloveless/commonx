unit CryptoCompare;

interface

uses
  systemx, typex, stringx, betterobject, sharedobject, JSONHelpers, httpsvaluecache, sysutils, generics.collections.fixed, variants;


type
  TCryptoCompareCoin = record
    baseRecord: IHolder<TJSON>;
    details: IHolder<TJSON>;
    social: IHolder<TJSON>;
  end;


  TCryptCompare = class(TBetterObject)
  private

    function GetCoinByIndex(idx: ni): TCryptoCompareCoin;
    function GEtCoinBySymbol(sSymbol: string): TCryptoCompareCoin;
    function GetRecord(sMember: string): TCryptoCompareCoin;
  public
    masterlist: IHolder<TJSON>;
    procedure Refresh;
    property Coins[idx: ni]: TCryptoCompareCoin read GetCoinByIndex;
    property Coin[sSymbol: string]: TCryptoCompareCoin read GEtCoinBySymbol;
    function CoinCount: ni;



  end;



implementation

{ TCryptCompare }

function TCryptCompare.CoinCount: ni;
var
  kc: TDictionary<string,TJSONDictionary>.TKeyCollection;
  //kc: TJSON.TKeyCollection;
begin
  if masterlist = nil then
    exit(0);
  result := masterlist.o['Data'].named.Count;
end;

function TCryptCompare.GetCoinByIndex(idx: ni): TCryptoCompareCoin;

begin
  raise ECritical.create('fuck you');
//  exit(GEtRecord(idx));
end;

function TCryptCompare.GEtCoinBySymbol(sSymbol: string): TCryptoCompareCoin;
begin
  exit(GetRecord(sSymbol));

end;

function TCryptCompare.GetRecord(sMember: string): TCryptoCompareCoin;
//var
//  kc: TDictionary<string,TJSONDictionary>.TKeyCollection;
var
  sNodeName: string;
  node: TJSON;
begin
//  kc := masterlist.o['Data'].n.Keys;

  if masterlist.o.HasNode('Data.'+uppercase(sMember)) then begin
    node := masterlist.o['Data'][uppercase(sMember)];
    result.baseRecord := StrToJSONH(node.ToJson);
    result.details := STrToJSONh(httpscache.Acquire('https://www.cryptocompare.com/api/data/coinsnapshotfullbyid/?id='+vartostr(node['Id'].Value), '', 300000));
    result.social := STrToJSONh(httpscache.Acquire('https://www.cryptocompare.com/api/data/socialstats/?id='+vartostr(node['Id'].Value), '', 300000));

  end else begin
    result.baseRecord := StrToJSONH('null');
    result.details := result.baserecord;
    result.social := result.baserecord;
  end;

end;

procedure TCryptCompare.Refresh;
var
  h: IHolder<TJSON>;
begin
  h := STrToJSONh(httpscache.Acquire('https://min-api.cryptocompare.com/data/all/coinlist', '', 300000));
  masterlist := h;

end;

end.
