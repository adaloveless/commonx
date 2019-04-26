unit RDTPProcessorForMYSQL;


interface

uses
  better_sockets, windows, RDTPProcessor, packet, abstractrdtpdatamodule, RDTP_DatabaseConnection;


type
  TRDTPProcessorForMYSQL = class(TRDTPProcessor)
  protected
    function GetData: TAbstractRDTPDataModule; override;
  public

  end;


implementation

{ TRDTPProcessorForMYSQL }

function TRDTPProcessorForMYSQL.GetData: TAbstractRDTPDataModule;
begin

//  datapool.product := TMYSQLRDTPDataModule;
  CheckGetContext;
  datapool.factory.RegisterClass(self.ClassType, TBestDatabaseDM);
  result := inherited GetData;
end;

end.
