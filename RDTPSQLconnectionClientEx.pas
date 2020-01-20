unit RDTPSQLconnectionClientEx;

interface

uses
  RDTPSQLConnection, betterobject, storageenginetypes;

type
  TRDTPSQLConnectionClientEx = class(TRDTPSQLConnectionClient)
  public
    function ReadQueryH(sQuery: string): IHolder<TSERowset>;
    function ReadOnH(ch: integer; sQuery: string): IHolder<TSERowset>;
    function FunctionQueryOn(ch: integer; sQuery: string; def: int64): int64;overload;
    function FunctionQueryOn(ch: integer; sQuery: string; def: string): string;overload;
    function FunctionQuery(sQuery: string; def: string): string;overload;
    function FunctionQuery(sQuery: string; def: int64): int64;overload;


  end;

implementation

{ TRDTPSQLConnectionClientEx }

function TRDTPSQLConnectionClientEx.FunctionQuery(sQuery, def: string): string;
begin
  result := FunctionQueryOn(0, sQuery, def);
end;


function TRDTPSQLConnectionClientEx.FunctionQuery(sQuery: string;
  def: int64): int64;
begin
  result := FunctionQueryOn(0, sQuery, def);
end;

function TRDTPSQLConnectionClientEx.FunctionQueryOn(ch: integer; sQuery,
  def: string): string;
var
  h: IHolder<TSERowSet>;
begin
  result := def;
  h := ReadOnH(ch, sQuery);
  if h.o.RowCount > 0 then
    result := h.o.Values[0,0];



end;

function TRDTPSQLConnectionClientEx.FunctionQueryOn(ch: integer; sQuery: string;
  def: int64): int64;
var
  h: IHolder<TSERowSet>;
begin
  result := def;
  h := ReadOnH(ch, sQuery);
  if h.o.RowCount > 0 then
    result := h.o.Values[0,0];

end;

function TRDTPSQLConnectionClientEx.ReadOnH(ch: integer;
  sQuery: string): IHolder<TSERowset>;
begin
  result := THolder<TSERowSet>.create;
  result.o := ReadOn(ch, sQuery);
end;

function TRDTPSQLConnectionClientEx.ReadQueryH(
  sQuery: string): IHolder<TSERowset>;
begin
  result := Tholder<TSERowset>.create;
  result.o := ReadQuery(sQuery);
end;


end.
