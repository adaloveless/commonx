unit RDTPProcessorForKEYBOT;
//todo 2: Add ability to generate this file automaticalyl from RDTP gen.  Change Data to AbstractData and then generate a new hard property to retrieve real data type.


interface

uses
  better_sockets, windows, RDTPProcessor, packet, abstractrdtpdatamodule, MYSQLRDTPDataModule;


type
  TRDTPProcessorForKEYBOT = class(TRDTPProcessor)
  protected
    function GetData: TAbstractRDTPDataModule; override;
  public



  end;


implementation


function TRDTPProcessorForKEYBOT.GetData: TAbstractRDTPDataModule;
begin
//  datapool.product := TMYSQLRDTPDataModule;
  result := inherited GetData;
end;

end.
