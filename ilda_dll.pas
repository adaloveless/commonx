unit ilda_dll;

interface

uses windows;

const
  EDDLL = 'EtherDream.dll';

//J4CDAC_API int __stdcall EtherDreamGetCardNum(void);
function EtherDreamGetCardNum(): integer; stdcall; external EDDLL;

//J4CDAC_API void __stdcall EtherDreamGetDeviceName(const int *CardNum, char *buf, int max);
procedure EtherDreamGetDeviceName(var cardnum: integer; buf: PAnsiChar; max: integer); stdcall; external EDDLL;

//J4CDAC_API bool __stdcall EtherDreamOpenDevice(const int *CardNum);
function EtherDreamOpenDevice(var cardnum: integer): boolean;

//J4CDAC_API bool __stdcall EtherDreamWriteFrame(const int *CardNum, const struct EAD_Pnt_s* data, int Bytes, uint16_t PPS, uint16_t Reps);
function EtherDreamWriteFCrame(var CardNum: integer; var data: EAD_Pnt_s; bytes: integer; pps: DWORD; reps: DWORD): bool;
//J4CDAC_API int __stdcall EtherDreamGetStatus(const int *CardNum);
//J4CDAC_API bool __stdcall EtherDreamStop(const int *CardNum);
//J4CDAC_API bool __stdcall EtherDreamCloseDevice(const int *CardNum);
//J4CDAC_API bool __stdcall EtherDreamClose(void);


implementation

end.
