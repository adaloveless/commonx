unit RDTPSQLconnectionClientEx;

interface

uses
  RDTPSQLConnection, betterobject, storageenginetypes;

type
  TRDTPSQLConnectionClientEx = class(TRDTPSQLConnectionClient)
  public
    function ReadQueryH(sQuery: string): IHolder<TSERowset>;
    function ReadOnH(ch: integer; sQuery: string): IHolder<TSERowset>;



  end;

implementation

{ TRDTPSQLConnectionClientEx }

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
