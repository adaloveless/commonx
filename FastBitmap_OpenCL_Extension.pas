unit FastBitmap_OpenCL_Extension;

interface

uses
  opencl_better, typex, types, sysutils, classes, systemx;


type
  TCLBitmapInfo = record
    width, height: int64;
    yStrideInPixels: int64;
    ptr: tDynByteArray;
    pixelsize: int64;
    function sz: int64;
    function Iterations: int64;
    procedure Init;
  end;

  TopenCL_FastBitmap = class(TOpenCL)
  protected
    procedure PrepareParams; override;
    procedure GetResults;override;
  public
    src, dest: TCLBitmapInfo;


  end;



implementation

{ TopenCL_FastBitmap }


procedure TopenCL_FastBitmap.GetResults;
begin
  inherited;
//
end;

procedure TopenCL_FastBitmap.PrepareParams;
begin
  inherited;
  AddOutput(@dest.ptr[0], dest.sz);
  AddInput(@dest.width, sizeof(dest.width));
  AddInput(@dest.height, sizeof(dest.height));
  AddInput(@dest.ystrideInPixels, sizeof(dest.ystrideInPixels));
  AddInput(@src.ptr[0], src.sz);
  AddInput(@src.width, sizeof(src.width));
  AddInput(@src.height, sizeof(src.height));
  AddInput(@src.ystrideInPixels, sizeof(src.ystrideInPixels));
  Iterations := dest.iterations;
end;

{ TBitmapInfo }

procedure TCLBitmapInfo.Init;
begin
  width := 0;
  Height := 0;
  pixelsize := 4;
  ptr := nil;
end;

function TCLBitmapInfo.Iterations: int64;
begin
  result := Width * height;
end;

function TCLBitmapInfo.sz: int64;
begin
  result := ystrideinpixels * Height * pixelsize;
end;

end.
