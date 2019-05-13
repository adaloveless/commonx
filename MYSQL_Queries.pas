unit MYSQL_Queries;

interface

uses
  storageenginetypes, mysqlstoragestring, sysutils;


function GetUpdateQueryForRow(sTable: string; rs: TSERowSet; sKey: string; vValue: variant): string;



implementation


function GetUpdateQueryForRow(sTable: string; rs: TSERowSet; sKey: string; vValue: variant): string;
var
  f: integer;
  sets: string;
begin
  result := 'update '+sTable+' set ';
  sets := '';
  for f := 0 to rs.fieldcount-1 do begin
    if sets <> '' then begin
      sets := sets + ', ';
    end;
    if lowercase(rs.FieldDefs[f].sName) <> lowercase(sKey) then begin
      var sname := rs.FieldDefs[f].sName;
      sets := sets + sName+'='+gvs(rs[sName]);
    end;
  end;

  result := result + sets;

  result := result + ' where '+sKey+'='+gvs(vValue);

end;


end.
