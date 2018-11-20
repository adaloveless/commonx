unit DXTypes_JSB;

///////////////////////////////////////////////////////////////////////////////
//
// Copyright J S Bladen, Sheffield, UK.
//
// Date and time: 05/09/2009 13:48:30
//
// For use with JSB Delphi DirectX units.
//
// Please send bug reports, suggestions, requests and other comments to: DelphiInterfaceUnits@jsbmedical.co.uk
//
///////////////////////////////////////////////////////////////////////////////

interface

uses
  Windows;

type
  TInteger64=Int64;
  TUnsignedInteger64=UInt64;
  PLongBool=^LongBool;
  PHRESULT=^HRESULT;
  SIZE_T=LongWord; // 32 bit Delphi only!
  PSIZE_T=^SIZE_T;
  HANDLE=THandle;
  RECT=TRect;
  PTRect=PRect;
  PTPoint=PPoint;
  TColorArray=array[0..3] of Single;
  TQuadArray_UInt=array[0..3] of LongWord;
  TQuadArray_Float=array[0..3] of Single;

  D3DCOLOR=LongWord;
  TD3DColor=D3DCOLOR;
  PD3DColor=^TD3DColor;
  PTD3DColor=^TD3DColor;

  D3DCOLORVALUE=packed record
    R:Single;
    G:Single;
    B:Single;
    A:Single;
  end;
  TD3DColorValue=D3DCOLORVALUE;
  PD3DColorValue=^TD3DColorValue;
  PTD3DColorValue=^TD3DColorValue;

function ColorArray(R,G,B,A:Single):TColorArray;

const
  INT64_MAX=Int64(9223372036854775807);
  ULONGLONG_MAX=UInt64($FFFFFFFFFFFFFFFF);
  UINT64_MAX=UInt64($FFFFFFFFFFFFFFFF);

implementation

function ColorArray(R,G,B,A:Single):TColorArray;
begin
  Result[0]:=R;
  Result[1]:=G;
  Result[2]:=B;
  Result[3]:=A;
end;

end.



