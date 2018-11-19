unit RDTPProcessorForStorageEngine;

interface

uses
  better_sockets, windows, RDTPProcessor, packet, abstractrdtpdatamodule, storageenginerdtpdatamodule;


type
  TRDTPProcessorForStorageEngine = class(TRDTPProcessor)
  protected
    function GetData: TAbstractRDTPDataModule; override;
  public
  



  end;


implementation



function TRDTPProcessorForStorageEngine.GetData: TAbstractRDTPDataModule;
begin
  datapool.product := TStorageEngineRDTPDataModule;
  result := inherited GetData;

end;

end.
