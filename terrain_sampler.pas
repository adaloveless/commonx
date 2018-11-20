unit terrain_sampler;

interface

uses
  hgt_sampler, sharedobject, typex, systemx, stringx, terraindata;

type
  TTerrainSampler = class(TSharedObject)
  protected
    bin: TBinSampler;
  public
    function GetTileData(long, lat: double): TTileDAta;

  end;


implementation

{ TTerrainSampler }

function TTerrainSampler.GetTileData(long, lat: double): TTileDAta;
begin
  bin := TBinSampler.create;
  try
    bin.LoadFiles('h:\terraindata\ascii');
    result.elevation := bin.GetSample(long, lat);


    result.TerrainType := 1;
  finally
    bin.free;
  end;


end;

end.
