unit MiscInterfaces;

interface


type
  ICheckSum = interface(IUnknown)
    function GetCheckSum: integer;
    property CheckSum: integer read GetCheckSum;
  end;



implementation

end.
