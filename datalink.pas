unit datalink;

interface

uses
  classes, db;

type
  TDataLink = record
    VarType: integer;
    DataSet: TDataSet;
    TableName: ansistring;
    FieldName: ansistring;
    Component: TComponent;
  end;

  PDataLInk = ^TDatalink;

implementation

end.
