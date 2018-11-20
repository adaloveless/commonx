unit DODTInterface;

interface

uses serverinterfaceinterface, Dataobjectcache, Dataobject;

function MyServer(o: TDataObject): IServerInterface;



implementation

function MyServer(o: TDataObject): IServerInterface;
var
  c: TDataObjectCache;
  s: IServerInterface;
begin
  c := TDataObjectCache(o.cache);
  s := IServerInterface(c.server);

  result := s;

end;

end.
