unit opencl_colorconversion;

interface

uses
  cl, cl_gl, betterobject, typex, systemx,sysutils, math, stringx, opencl_better;


type
  TOpenCL_RGBtoHSL = class(TOpenCL)
  public
    procedure Init;override;
  end;




implementation

{ TOpenCL_RGBtoHSL }

procedure TOpenCL_RGBtoHSL.Init;
begin
  inherited;
  Prog := '';
  Prog := Prog + '__kernel void RGBSwap(__global uint* data, __global uint* outdata)';
  Prog := Prog + '{';
  Prog := Prog + '	// Index of the elements to add';
  Prog := Prog + '	unsigned int n = get_global_id(0);';
  Prog := Prog + '  ';
  Prog := Prog + '  outdata[n] = (((data[n]>>16) & 0xFF) << 0) | (((data[n]>>8) & 0xFF) << 8) | (((data[n]>>0) & 0xFF) << 16) ';
  Prog := Prog + '}';

end;

end.
