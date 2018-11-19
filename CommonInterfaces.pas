unit CommonInterfaces;
//This unit contains interfaces for general-purpose use.
interface
const
  GUID_IUnknownEnhanced : TGUID = '{C3A564C0-3F7B-11D3-AC25-0080C859E788}';
  GUID_IDelegatedLifeManager : TGUID = '{C3A564C1-3F7B-11D3-AC25-0080C859E788}';
type
//------------------------------------------------------------------------------
  IUnknownEnhanced = interface(IUnknown)
    ['{C3A564C0-3F7B-11D3-AC25-0080C859E788}']
    function _RefCount: integer; stdcall;
  end;

  ILockableObject = interface
    procedure Lock;
    procedure UnLock;
  end;

  IMonitoredObject = interface(ILockableObject)
    function GetStatus: string;
    function GetCycles: integer;
    property Status: string read GetStatus;
    property cycles: integer read GetCycles;
  end;

implementation

end.
