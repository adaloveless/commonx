unit opencl_better;

interface

uses
  cl, cl_gl, betterobject, typex, systemx,sysutils, math, stringx, debug;

const
  c_device_types:array[1..3] of cardinal=(CL_DEVICE_TYPE_CPU, CL_DEVICE_TYPE_GPU, CL_DEVICE_TYPE_ACCELERATOR);

type
	// platform+device_type couples
	t_platform_device=record
		platform_id: cl_platform_id; // index for platform_id
		device_type: cl_device_type; // CL_DEVICE_TYPE_CPU; CL_DEVICE_TYPE_GPU; CL_DEVICE_TYPE_ACCELERATOR
	end;

  PCLFloatArray = cl_mem;

  EOpenCLError = class(Exception);
  TCLMem = record
    localmem: PByte;
    clmem: cl_mem;
    sz: ni;
  end;

  TOpenCL = class(TBetterObject)
  private
    FDeviceIndex: cl_uint;
    FProg: string;
    FFunc: string;
    FMainFunction: string;
  protected

  	num_devices_returned: cl_uint;
    errcode_ret: cl_int;
    platform_devices: array of t_platform_device;
  	device_ids: array of cl_device_id; // compute device IDs
  	context: cl_context; // compute context
    clKernel: cl_kernel;
    clProgram: cl_program;
    CommandQueue: cl_command_queue;
    aglobalIterations: array[0..0] of size_t;

    inputs: array of TCLMem;
    outputs: array of TCLMem;

    function AddInput(mem: PByte; sz: ni): cl_mem;
    function AddOutput(mem: PByte; sz: ni): cl_mem;

    procedure EnumDevices;
    procedure Prepare;
    procedure Build;
    procedure PrepareParams;virtual;
    procedure Run;
    procedure GetResults;virtual;
    procedure Unprepare;
    procedure Init;override;
    procedure clAssert(err: ni);
  strict private
    procedure UnprepareParams;
  public


    constructor Create;override;
    property DeviceIndex: cl_uint read FDeviceIndex write FDeviceIndex;
    property Prog: string read FProg write FProg;
    property Func: string read FFunc write FFunc;
    property MainFunction: string read FMainFunction write FMainFunction;
    procedure Execute;

    procedure LoadCLFile(sFile: string);
    property Iterations: size_t read aglobalIterations[0] write aglobalIterations[0];
  end;

implementation

{ TOpenCL }

function TOpenCL.AddInput(mem: PByte; sz: ni): cl_mem;
begin
  result := clCreateBuffer(context, CL_MEM_READ_ONLY or CL_MEM_COPY_HOST_PTR, sz, mem, nil);
  setlength(inputs, length(inputs)+1);
  inputs[high(inputs)].localmem := mem;
  inputs[high(inputs)].sz:= sz;
  inputs[high(inputs)].clmem := result;

end;

function TOpenCL.AddOutput(mem: PByte; sz: ni): cl_mem;
begin
  result := clCreateBuffer(context, CL_MEM_WRITE_ONLY, sz, nil, nil);
  setlength(outputs, length(outputs)+1);
  outputs[high(outputs)].localmem := mem;
  outputs[high(outputs)].sz:= sz;
  outputs[high(outputs)].clmem := result;
end;

procedure TOpenCL.Build;
var
  i: integer;
  sourceStr: AnsiString;
  sourceSize: size_t;
  sourcePAnsiChar: PAnsiChar;

  GPUVector1, GPUVector2, GPUOutputVector: cl_mem;
  s, error_string: AnsiString;
  returned_size: size_t;
  f: ansistring;
begin
  // Create OpenCL program with source code
  sourceStr:=FProg;
  sourceSize:=Length(sourceStr);
  sourcePAnsiChar:=PAnsiChar(sourceStr);
  clProgram := clCreateProgramWithSource(context, 1, @sourcePAnsiChar, @sourceSize, @errcode_ret);
	if errcode_ret<>CL_SUCCESS then begin
    raise EOpenCLError.create('Error: clCreateProgramWithSource failed!');
    clReleaseContext(context);
    exit;
  end;

	// Build the program (OpenCL JIT compilation)
	if CL_SUCCESS<>clBuildProgram(clProgram, 0, nil, nil, nil, nil) then begin
    error_string:='Error: clBuildProgram failed! ';
    clGetProgramBuildInfo(clProgram, device_ids[0], CL_PROGRAM_BUILD_LOG, 0, nil, @returned_size);
    SetLength(s, returned_size+2);
    clGetProgramBuildInfo(clProgram, device_ids[0], CL_PROGRAM_BUILD_LOG, Length(s), PAnsiChar(s), @returned_size);
    SetLength(s, Min(Pos(#0, s)-1, returned_size-1));
    error_string:=error_string+s;
    Debug.Log(error_string);
    clReleaseProgram(clProgram);
    clReleaseContext(context);
    raise EOpenCLError.create(error_string);
    exit;
  end;
	// Create a handle to the compiled OpenCL function (Kernel)
  f := ansistring(MainFunction);
	clKernel:=clCreateKernel(clProgram, PAnsiChar(f), nil);
  if clKErnel = nil then
    raise EOpenCLError.Create('probably did not find '+f+'()');
	CommandQueue:=clCreateCommandQueue(context, device_ids[0], 0, nil);
end;

procedure TOpenCL.clAssert(err: ni);
begin
  if err <> 0 then
    raise EOpenCLError.create('OpenCL assert '+inttostr(err));
end;

constructor TOpenCL.Create;
begin
  inherited;
  EnumDevices;
  iterations := 1;
end;

procedure TOpenCL.EnumDevices;
var
  i, j: integer;
	num_platforms_returned: cl_uint;
  platform_id: array of cl_platform_id; // platform id
  i_pl: integer;
  i_type: integer;
  device_vendor, device_name: AnsiString;
  returned_size: size_t;
begin
  if not OpenCL_loaded then begin
    raise EOpenCLError.create('Error: OpenCL.dll not loaded!');
    exit;
  end;

  // Number of platforms
	errcode_ret:=clGetPlatformIDs(0, nil, @num_platforms_returned);
	if (errcode_ret <> CL_SUCCESS) then begin
		raise EOpenCLError.create('Error: Failed to get number of platforms!');
		exit;
  end;
  // Connect to a platform
  SetLength(platform_id, num_platforms_returned);
	errcode_ret:=clGetPlatformIDs(num_platforms_returned, @platform_id[0], @num_platforms_returned);
	if (errcode_ret <> CL_SUCCESS) then begin
		raise EOpenCLError.create('Error: Failed to find platform!');
		exit;
  end;

	// find platform+device_type
	SetLength(platform_devices, 0);
  for i_pl:=0 to num_platforms_returned-1 do for i_type:=1 to 3 do
  	if (CL_SUCCESS=clGetDeviceIDs(platform_id[i_pl], c_device_types[i_type], 0, nil, @num_devices_returned)) and (num_devices_returned>=1)
    then begin
    	SetLength(platform_devices, Length(platform_devices)+1);
		  platform_devices[High(platform_devices)].platform_id:=platform_id[i_pl];
			platform_devices[High(platform_devices)].device_type:=c_device_types[i_type];
    end;
	// list platform+device_type couples
	for i:=0 to High(platform_devices) do begin
		errcode_ret:=clGetDeviceIDs(platform_devices[i].platform_id, platform_devices[i].device_type, 0, nil, @num_devices_returned);
		if (errcode_ret<>CL_SUCCESS) then begin
  		raise EOpenCLError.create('Error: Failed to create a device group!');
	  	exit;
    end;
		SetLength(device_ids, num_devices_returned);
		errcode_ret:=clGetDeviceIDs(platform_devices[i].platform_id, platform_devices[i].device_type, num_devices_returned, @device_ids[0], @num_devices_returned);
		for j:=0 to num_devices_returned-1 do begin
      SetLength(device_name, 1024);
			clGetDeviceInfo(device_ids[j], CL_DEVICE_NAME, Length(device_name), PAnsiChar(device_name), @returned_size);
      SetLength(device_name, Min(Pos(#0, device_name)-1, returned_size-1));
      SetLength(device_vendor, 1024);
			clGetDeviceInfo(device_ids[j], CL_DEVICE_VENDOR, Length(device_vendor), PAnsiChar(device_vendor), @returned_size);
      SetLength(device_vendor, Min(Pos(#0, device_vendor)-1, returned_size-1));

		end;
		SetLength(device_ids, 0);
	end;

end;


procedure TOpenCL.Execute;
begin
  Prepare;
  Build;

  PrepareParams;//override this to define inputs/outputs with AddInput and AddOutput
  var idx := 0;

  //outputs first
  for var t := 0 to high(outputs) do begin
  	clAssert(clSetKernelArg(clKernel, idx, sizeof(cl_mem), @outputs[t].clmem));
    inc(idx);
  end;

  //inputs second
  for var t := 0 to high(inputs) do begin
  	clAssert(clSetKernelArg(clKernel, idx, sizeof(cl_mem), @inputs[t].clmem));
    inc(idx);
  end;

  Run;

  //read back inputs
  for var t := 0 to high(outputs) do begin
  	clAssert(clEnqueueReadBuffer(CommandQueue, outputs[t].clmem, CL_TRUE, 0, outputs[t].sz, outputs[t].localmem, 0, nil, nil));
  end;

  GetResults;//copy buffers back to objects (if required)

  Unprepare;
end;

procedure TOpenCL.GetResults;
begin
  //
end;

procedure TOpenCL.Init;
begin
  inherited;
  FMainFunction := 'main';
end;

procedure TOpenCL.LoadCLFile(sFile: string);
begin
  FProg := LoadFileAsString(sFile);
end;

procedure TOpenCL.Prepare;
begin
	// Get compute devices from platform
	errcode_ret:=clGetDeviceIDs(platform_devices[DeviceIndex].platform_id, platform_devices[DeviceIndex].device_type, 0, nil, @num_devices_returned);
  SetLength(device_ids, num_devices_returned);
  errcode_ret:=clGetDeviceIDs(platform_devices[DeviceIndex].platform_id, platform_devices[DeviceIndex].device_type, num_devices_returned, @device_ids[0], @num_devices_returned);
  if (errcode_ret<>CL_SUCCESS) then begin
    raise EOpenCLError.create('Error: Failed to create a device group!');
    exit;
  end;
	// Create a compute context
	context:=clCreateContext(nil, num_devices_returned, @device_ids[0], nil, nil, @errcode_ret);
  if (errcode_ret<>CL_SUCCESS) then begin
    raise EOpenCLError.create('Error: Failed to create a compute context!!');
    exit;
  end;
end;

procedure TOpenCL.PrepareParams;
begin
  //no implementation required
end;

procedure TOpenCL.Run;
begin
	var status := clEnqueueNDRangeKernel(CommandQueue, clKernel, 1{dimensions}, nil, @aglobaliterations[0], nil, 0, nil, nil);
  classert(status);

  if (status <> 0) then begin
    debug.log('CL_ERROR '+status.tostring);
  end;



end;

procedure TOpenCL.Unprepare;
begin
  UnprepareParams;
	clReleaseCommandQueue(CommandQueue);
	clReleaseKernel(clKernel);
  clReleaseProgram(clProgram);
 	clReleaseContext(context);
end;

procedure TOpenCL.UnprepareParams;
begin
  //
end;

end.
