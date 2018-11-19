unit RDTPTerrainServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPTerrainRQs.txt}
{END}
interface

uses
   hgt_sampler, rdtpprocessor, RDTPTerrainServer, terrainData, RDTPServerList,osm;


type
  TTerrainServer = class(TTerrainServerBase)
  private
  protected
  public
{INTERFACE_START}
    function RQ_Test():integer;overload;override;
    function RQ_GetSingleTileData(Long:double; Lat:double):TTileData;overload;override;

{INTERFACE_END}
  end;
implementation
{ TTerrainServer }

function TTerrainServer.RQ_GetSingleTileData(Long, Lat: double): TTileData;
var
  bin: TBinSAmpler;
  o: TOSM_Getter;
  xml: string;
const
  TILE_RADIUS = 0.0005;

begin

  bin := TBinSampler.create;
  try
    bin.LoadFiles('h:\terraindata\ascii');
    result.TerrainType := 0;
    result.elevation := bin.GetSample(long, lat);
    if result.elevation < 0 then
      result.elevation := 0;

    o := TOSM_Getter.create;
    o.CacheDir := 'h:\terraindata\osm\';
    try
      xml := o.GetXMLData(long-TILE_RADIUS, 0-(lat+TILE_RADIUS), long+TILE_RADIUS, 0-(lat-TILE_RADIUS));
      if pos('water', xml) > 0 then
        result.TerrainType := TER_WATER
      else
      if pos('motorway', xml) > 0 then
        result.TerrainType := TER_HIGHWAY
      else
        result.TerrainType := TER_GRASS;
    finally
      o.free;
    end;
  finally
    bin.free;
  end;

end;

function TTerrainServer.RQ_Test: integer;
begin
  result := 666;
end;


initialization

RDTPServers.RegisterRDTPProcessor('terrain', TTerrainServer);

end.
