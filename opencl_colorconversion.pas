unit opencl_colorconversion;

interface

uses
  cl, cl_gl, betterobject, typex, systemx,sysutils, math, stringx, opencl_better;


type
  TOpenCL_RGBtoHSL = class(TOpenCL)
  private
  protected
    procedure PrepareParams; override;
    procedure GetResults; override;
    procedure Init;override;
  public
    Input: array of cl_float;
    Output: array of cl_float;    
        

  end;




implementation

{ TOpenCL_RGBtoHSL }


procedure TOpenCL_RGBtoHSL.Init;
begin
  inherited;
  //NOTE: this doesn't actually do what it says... just an experinent
  Prog := '';
  Prog := Prog + '__kernel void main(__global float* outdata, __global float* data)';
  Prog := Prog + '{';
  Prog := Prog + '	';
  Prog := Prog + '	unsigned int n = get_global_id(0);';
  Prog := Prog + '  ';
  Prog := Prog + '  outdata[n] = data[n] / 2.0;';
  Prog := Prog + '}';

//  prog := prog + '__kernel void main(__global float* c, __global float* a,__global float* b) ';
//  prog := prog + '{ ';
//  prog := prog + '	/* Index of the elements to add */';
//  prog := prog + '	unsigned int n = get_global_id(0); ';
//  prog := prog + '	/* Sum the n’th element of vectors a and b and store in c */';
//  prog := prog + '	c[n] = a[n] + b[n]; ';
//  prog := prog + '} ';

end;

procedure TOpenCL_RGBtoHSL.PrepareParams;
begin
  inherited;
  AddOutput(@Output[0], length(Output)*sizeof(output[0]));
  AddInput(@Input[0], length(Input)*sizeof(input[0]));
  Iterations := length(output);
end;



procedure TOpenCL_RGBtoHSL.Getresults;
begin
  inherited;
//  if (self.Outputs[0].sz mod (sizeof(output[0])) <> 0) then
//    raise ECritical.create('output size mismatch!');
//  setlength(output, self.Outputs[0].sz div (sizeof(output[0])));
//  MoveMem32(@Output[0], self.outputs[0].mem, self.outputs[0].sz);
end;

end.
