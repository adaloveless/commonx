unit TerrainDAta;

interface

uses
  graphics;

const
  TER_FLAG_DEEP = $10;
  TER_UNDEFINED= 0;
  TER_WATER = 1;
  TER_GRASS = 2;
  TER_ROAD_MOTORWAY = 3;
  TER_ROAD_TRUNK = 4;
  TER_ROAD_PRIMARY = 5;
  TER_ROAD_SECONDARY = 6;
  TER_ROAD_TERTIARY = 7;
  TER_URBAN = 8;
  TER_RIVER = 9;

type
  TTileData = packed record
  private
    function GetTerrainBase: byte;
    procedure SetTerrainBase(const Value: byte);
    function GetDeep: boolean;
    procedure SetDeep(const Value: boolean);
  public
    Version: byte;
    Flags: byte;
    TerrainType: byte;
    Elevation: single;
    TopoColor: TColor;
    CheckSum: byte;
    Shadow: byte;
    function CalculateChecksum: byte;
    function IsChecksumValid: boolean;
    procedure SetChecksum;
    property TerrainBase: byte read GetTerrainBase write SetTerrainBase;
    property DeepSearch: boolean read GetDeep write SetDeep;
  end;

  TTerrainData = packed record
  end;

implementation

{ TTileData }

function TTileData.CalculateChecksum: byte;
var
  t: nativeint;
begin
  result := 0;
  for t := 0 to nativeint(addr(checksum))-nativeint(addr(self))-1 do begin
    result := result xor pbyte(@self)[t];
  end;

end;

function TTileData.GetDeep: boolean;
begin
  result := (flags and TER_FLAG_DEEP) <> 0;
end;

function TTileData.GetTerrainBase: byte;
begin
  result := Flags and $0F;
end;

function TTileData.IsChecksumValid: boolean;
begin
  result := Checksum = calculatechecksum;
end;

procedure TTileData.SetChecksum;
begin
  Checksum := calculatechecksum;
end;

procedure TTileData.SetDeep(const Value: boolean);
begin
  flags := flags or TER_FLAG_DEEP;
end;

procedure TTileData.SetTerrainBase(const Value: byte);
begin
  flags := (flags and $f0) or value;

end;

end.
