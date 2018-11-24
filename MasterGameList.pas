unit MasterGameList;

interface

uses debug, tickcount, sysutils, sharedobject, systemx, generics.collections.fixed,
    managedthread, serverinterface, serverinterfaceinterface, gamelist,
    DataObjectServices, dataobject, dataobjectcache, dataobjectcachejanitor,
    Xref, dataobjectcachemanager, betterobject, packet, commands_system, orderlyinit;


var
  GL: TMasterGameList;

implementation
uses TowerGameAI;



procedure oinit;
begin
//  raise Exception.create('unimplemented');

  GL := TMasterGameList.Create;

end;

procedure ofinal;
begin
//  raise Exception.create('unimplemented');
  GL.free;


end;

initialization


orderlyinit.init.RegisterProcs('MasterGamelist', oinit, ofinal, 'GameList');



finalization


end.
