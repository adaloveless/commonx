unit RDTPProcessorForMYSQL;

interface

uses
  better_sockets, windows, RDTPProcessor, packet, abstractrdtpdatamodule, MYSQLRDTPDataModule;


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
  datapool.factory.RegisterClass(self.ClassType, TMYSQLRDTPDataModule);
  result := inherited GetData;
end;

end.
