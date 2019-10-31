unit fftw_interface;
{$Message '********************************Compiling fftw_interface'}
{ -------------------------------------------------------------- }
{ Delphi interface to the fftw library -- fftw Version 3.0.1. }
{ Note that this interface is incomplete. Additional function }
{ interface entries may be added in an anologous manner, see }
{ fftw  for more details. }
{ }
{ Last modified 22/DEC/03 }
{ Written by Mark G. Beckett (g.beckett@epcc.ed.ac.uk }
{ -------------------------------------------------------------- }

{$IFNDEF MSWINDOWS}
  {$Error 'This unit is only supported on windows'}
{$ENDIF}
{$DEFINE DELAYLOAD}

interface

uses
  orderlyinit, systemx, signals;
{$IFDEF CPUX64}
const fftdll = 'fftw3_x64.dll';
{$ELSE}
const fftdll ='fftw3.dll';
{$ENDIF}

procedure LockFFT;
procedure UnlockFFT;



{ EPCC (MGB) - Note that the fftw memory allocation routines can not be used
  for data arrays, because the DLL is allocated a separate heap that is not
  accessible from the Delphi runtime environment. }
function fftw_malloc(n: Integer): Pointer; cdecl;

procedure fftw_free(p: Pointer); cdecl;

function fftw_plan_dft_1d(n: Integer; inData: PSingle; outData: PSingle;
  sign: Integer; flags: Longword): Pointer; cdecl;

function fftw_plan_dft_r2c_1d(n: Integer; inData: PDouble; outData: PDouble; flags: Longword): Pointer; cdecl;

function fftw_plan_r2r_1d(n: Integer; inData: PSingle; outData: PSingle;
  sign: Integer; flags: Longword): Pointer; cdecl;


function fftw_plan_dft_r2c_3d(nx: Integer; ny: Integer; nz: Integer;
  inData: PSingle; outData: PSingle; flags: Longword): Pointer; cdecl;

function fftw_plan_dft_c2r_3d(nx: Integer; ny: Integer; nz: Integer;
  inData: PSingle; outData: PSingle; flags: Longword): Pointer; cdecl;

function fftw_plan_dft_c2r_1d(n: Integer; inData: PDouble; outData: PDouble;
  flags: Longword): Pointer; cdecl;

procedure fftw_destroy_plan(plan: Pointer); cdecl;

procedure fftw_execute(plan: Pointer); cdecl;

const
  { EPCC (MGB) - fftw documented constants, taken from "api/fftw3.h". Comments
    to the right of the definitions are transcribed from the original header
    file. }

  fftw_FORWARD = -1;
  fftw_BACKWARD = 1;

  fftw_MEASURE = 0;
  fftw_DESTROY_INPUT = 1; { 1U << 0 }
  fftw_UNALIGNED = 2; { 1U << 1 }
  fftw_CONSERVE_MEMORY = 4; { 1U << 2 }
  fftw_EXHAUSTIVE = 8; { 1U << 3 } { NO_EXHAUSTIVE is default }
  fftw_PRESERVE_INPUT = 16; { 1U << 4 } { cancels fftw_DESTROY_INPUT }
  fftw_PATIENT = 32; { 1U << 5 } { IMPATIENT is default }
  fftw_ESTIMATE = 64; { 1U << 6 }

  { fftw undocumented constants have not been defined in this implementation.
    They are not required for typical usage of the library. }


implementation


uses
  SysUtils;

var
  _fftw_sect: systemx.TCLXCriticalSection;

function fftw_malloc(n: Integer): Pointer; cdecl; external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}
procedure fftw_free(p: Pointer); cdecl; external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}
{ Commented prototypes are taken from fftw library. }

{ fftw_plan fftw_plan_dft_1d)(int n, C *in, C *out, int sign, unsigned flags) }
function fftw_plan_dft_1d(n: Integer; inData: PSingle; outData: PSingle;
  sign: Integer; flags: Longword): Pointer; cdecl; external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}

{ fftw_plan fftw_plan_dft_r2c_1d(int n, float *in, \
  fftw_complex *out, unsigned flags); }
function fftw_plan_dft_r2c_1d(n: Integer; inData: PDouble; outData: PDouble;
  flags: Longword): Pointer; cdecl; external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}

function fftw_plan_r2r_1d(n: Integer; inData: PSingle; outData: PSingle;
  sign: Integer; flags: Longword): Pointer; cdecl; external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}


{ fftw_plan fftw_plan_dft_r2c_3d(int nx, int ny, int nz, R *in, C *out,
  unsigned flags) }
function fftw_plan_dft_r2c_3d(nx: Integer; ny: Integer; nz: Integer;
  inData: PSingle; outData: PSingle; flags: Longword): Pointer; cdecl;
external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}

{ fftw_plan fftw_plan_dft_c2r_1d(int n, C *in, R *out, unsigned flags) }
function fftw_plan_dft_c2r_1d(n: Integer; inData: PDouble; outData: PDouble;
  flags: Longword): Pointer; cdecl; external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}

{ fftw_plan fftw_plan_dft_c2r_3d(int nx, int ny, int nz,
  C *in, R *out, unsigned flags) }
function fftw_plan_dft_c2r_3d(nx: Integer; ny: Integer; nz: Integer;
  inData: PSingle; outData: PSingle; flags: Longword): Pointer; cdecl;
external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}

{ void fftw_destroy_plan(fftw_plan p) }
procedure fftw_destroy_plan(plan: Pointer); cdecl; external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}

{ void fftw_execute(const fftw_plan p) }
procedure fftw_execute(plan: Pointer); cdecl; external fftdll {$IFDEF DELAYLOAD}delayed;{$ENDIF}

procedure LockFFT;
begin
  ecs(_fftw_sect);
end;
procedure UnlockFFT;
begin
  lcs(_fftw_sect);
end;

procedure oinit;
begin
  ics(_fftw_sect);
end;

procedure ofinal;
begin
  dcs(_fftw_sect);

end;

initialization
  init.RegisterProcs('fftw_interface', oinit, ofinal);



finalization




end.
