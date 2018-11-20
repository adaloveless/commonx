unit VDSideLoad;

interface

uses
  betterobject, virtualdisk_advanced, systemx, sysutils, classes;

implementation

type
  TVDSideLoad = class(TBetterObject)
  protected
    vat: TVirtualAddressTable;
  public
    procedure LoadFromFile(sfile: string);

  end;




{ TVDSideLoad }

procedure TVDSideLoad.LoadFromFile(sfile: string);
var
  fs: TFileSTream;
begin
  fs := TFileStream.create(sFile, fmOpenRead);
  try
    vat.ReadFromStream(fs);
  finally
    fs.free;
  end;

end;

end.
