unit RDTPProcessorForLegacyMYSQL;

interface

uses
  better_sockets, windows, RDTPProcessor, packet, abstractrdtpdatamodule, LegacyMYSQLRDTPDataModule;


type
  TRDTPProcessorForLegacyMYSQL = class(TRDTPProcessor)
  protected
    function GetData: TAbstractRDTPDataModule; override;
  public



  end;


implementation

{ TRDTPProcessorForMYSQL }

function TRDTPProcessorForLegacyMYSQL.GetData: TAbstractRDTPDataModule;
begin
  datapool.product := TLegacyMYSQLRDTPDataModule;
  result := inherited GetData;
end;

end.
