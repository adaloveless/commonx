unit opencl_ut;

interface

uses
  cl, cl_gl, betterobject, typex, systemx,sysutils, math, stringx, opencl_better;

const
  MYSIZE=2048;
  // Some interesting data for the vectors
  InitialData1: array[0..19] of cl_float = (37.4,50,54,50,56,0,43,43,7.4,71,32,36,16,43,56,100,50,25,15,17);
  InitialData2: array[0..12] of cl_float = (35,51,54.5,58,55,3.2,36,69,27,39,35,40,16);
type
  TOpenCL_UT = class(TOpenCL)
  public
  	HostVector1, HostVector2, HostOutputVector: array[0..MYSIZE-1] of cl_float;
    GPUVector1, GPUVector2, GPUOutputVector: cl_mem;
    procedure PrepareParams;override;
    procedure GetResults;override;
  end;

implementation

{ TOpenCL_UT }

procedure TOpenCL_UT.GetResults;
begin
  inherited;

	clEnqueueReadBuffer(CommandQueue, GPUOutputVector, CL_TRUE, 0, sizeof(HostOutputVector[0]) * MYSIZE, @HostOutputVector[0], 0, nil, nil);
  // Free memory
	clReleaseMemObject(GPUVector1);
	clReleaseMemObject(GPUVector2);
	clReleaseMemObject(GPUOutputVector);



end;

procedure TOpenCL_UT.PrepareParams;
var
  i: ni;
begin
  inherited;

	// Initialize with some interesting repeating data
	for i:=0 to MYSIZE-1 do begin
		HostVector1[i]:=InitialData1[i mod 20];
		HostVector2[i]:=InitialData2[i mod 13];
	end;


	// Allocate GPU memory for source vectors AND initialize from CPU memory
	GPUVector1:=clCreateBuffer(context, CL_MEM_READ_ONLY or CL_MEM_COPY_HOST_PTR, sizeof(HostVector1[0]) * MYSIZE, @HostVector1[0], nil);
	GPUVector2:=clCreateBuffer(context, CL_MEM_READ_ONLY or CL_MEM_COPY_HOST_PTR, sizeof(HostVector2[0]) * MYSIZE, @HostVector2[0], nil);
	// Allocate output memory on GPU
	GPUOutputVector:=clCreateBuffer(context, CL_MEM_WRITE_ONLY, sizeof(HostOutputVector[0]) * MYSIZE, nil, nil);
	// In the next step we associate the GPU memory with the Kernel arguments
	clSetKernelArg(clKernel, 0, sizeof(cl_mem), @GPUOutputVector);
	clSetKernelArg(clKernel, 1, sizeof(cl_mem), @GPUVector1);
	clSetKernelArg(clKernel, 2, sizeof(cl_mem), @GPUVector2);




end;

end.
