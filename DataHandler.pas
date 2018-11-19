unit DataHandler;

interface

uses
  typex, systemx;

type
  IDataHandler = interface
    function SendData(p: pbyte; sz: ni): ni;overload;
    function ReadData(p: pbyte; sz: ni): ni;
    function GetCharWait(p: pbyte; iTimeOut: ni=-1): boolean;
    function GuaranteeReadData(p:pbyte; l: ni; iTimeout: ni = 2000): ni;
    function GuaranteeSendData(p: pbyte; l: ni; iTimeout: ni = -1): boolean;
    procedure LockComms;
    procedure UnlockComms;
    procedure Flush;
  end;

implementation

end.
