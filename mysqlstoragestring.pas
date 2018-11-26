unit mysqlstoragestring;

interface

uses
  variants, stringx, systemx;

function GetStorageString(vT: integer; v: variant): ansistring;

implementation


function GetStorageString(vT: integer; v: variant): ansistring;
begin

  if (vt=varSTring) or (vt=varUString) or (vt=varOleStr) then begin
    result := Quote(SQLEscape(vartostr(v)));
  end else
  if vt = varDate then begin
    result := Quote(datetoMYSQLDate(v));
  end else
  begin
    result := VarToStr(v);
    IF result = 'INF' then
      result := '0.0';
    if result = 'NAN' then
      result := '0.0';
  end;

//  end else begin
//    raise exception.create('vartype not handled in mysqlstoragestring.getstoragestring');
//  end;



end;

end.
