unit osmdb;

interface

uses
  rdtpdb;


function NeedOSMDB: TRDTPDB;
procedure NoNeedOSMDB(db: TRDTPDB);



implementation

function NeedOSMDB: TRDTPDB;
begin
   result := TrdtpDB.create;
   result.Connect('localhost', 'osm', 'root', 'shad0ws');
end;

procedure NoNeedOSMDB(db: TRDTPDB);
begin
  db.free;
  db := nil;
end;


end.
