unit ShareFinder;

interface


uses
  systemx, stringx, sysutils;


function GetFileServiceSharePath: string;


implementation


function GetFileServiceSharePath: string;
begin
  var p := dllpath;
  var drv := zcopy(p, 0,2);
  if length(drv) < 2 then
    exit(dllpath+'share\');

  if drv[low(drv)+1] <> ':' then
    exit(dllpath+'share\');

  exit(drv+'\FileServiceShare\');



end;




end.
