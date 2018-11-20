unit dmx_h;

interface

const
{$ifndef CPUX86}
  dmx_dll = 'usb_dmx_dll_64.dll';
{$ELSE}
  //{$ERROR requires debug DLLs don't use }
  dmx_dll = 'usb_dmx_dll.dll';
{$ENDIF}

const
  SET_DMX_RX_MODE = 5;
  SET_DMX_TX_MODE = 6;

{$DEFINE DEL}


function OpenPort(deviceid: integer): pointer;cdecl;external dmx_dll {$IFDEF DEL}delayed{$ENDIF};
function ClosePort(device_handle: pointer): integer;cdecl;external dmx_dll {$IFDEF DEL}delayed{$ENDIF};
Procedure SendData(iLabel: integer; data: pointer; length: integer);cdecl;external dmx_dll {$IFDEF DEL}delayed{$ENDIF};
function ReceiveData(labl: byte; data: pointer; var expected_length: cardinal): byte;cdecl;external dmx_dll {$IFDEF DEL}delayed{$ENDIF};

implementation



end.
