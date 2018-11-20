unit fft_easy;

interface

uses
  typex, fftw_interface, systemx, debug, geometry;


type
  complex = packed record
    re: double;
    im: double;
  end;



procedure FFT_RealToComplex_Normalized(itemCount_TimeDomain: nativeint; input: PDouble; output: PComplex);
procedure FFT_ComplexToReal_Normalized(itemCount_TimeDomain: nativeint; input: PComplex; output: PDouble; scratchSpace: PComplex = nil);
procedure FFT_RealToComplex(itemCount_TimeDomain: nativeint; input: PDouble; output: PComplex);
procedure FFT_ComplexToReal(itemCount_TimeDomain: nativeint; input: PComplex; output: PDouble);

procedure FFT_ComplexToReal_Normalized_Extrapolate(itemCount_TimeDomain: nativeint; outItemCount_TimeDomain: nativeint; input: PComplex; output: PDouble; scratchSpace: PComplex = nil);
procedure FFT_Extrapolate_Predictive(itemCount_TimeDomain: nativeint; outItemCount_TimeDomain: nativeint; input: PDouble; output: PDouble; scratchSpace: PComplex = nil);



implementation


procedure FFT_Extrapolate_Predictive(itemCount_TimeDomain: nativeint; outItemCount_TimeDomain: nativeint; input: PDouble; output: PDouble; scratchSpace: PComplex = nil);
var
  tempOut: array of double;
  bit: ni;
  start: PDouble;
  sz: ni;
  t: ni;
  tt: ni;
  d: Pdouble;
  cx: ni;
  middle: array of Complex;
begin
  setlength(tempOut, outItemCount_TimeDomain);
  bit := HighOrderBit(ItemCount_TimeDomain);
  fillmem(pbyte(output), 0, outItemcount_TimeDomain*sizeof(double));
  setlength(middle, itemCount_TimeDomain*2);

  cx := 0;

  for t:= 0 to itemCount_TimeDomain-1 do begin
    d := PDouble(PByte(output)+(sizeof(double)*t));
    d^ := PDouble(PByte(input)+(sizeof(double)*t))^;
  end;

  sz := itemcount_TimeDomain;
{x$DEFINE POWER2}
{$IFDEF POWER2}
  while bit > 1 do begin
    sz := itemcount_TimeDomain;
    sz := 1 shl bit;
{$ELSE}
  while sz > 1 do begin
{$ENDIF}
    start := PDouble(pbyte(input)+(sizeof(double)*(itemcount_TimeDomain-sz)));

    FFT_RealToComplex_Normalized(sz, start, @middle[0]);
    FFT_ComplexToReal_Normalized_Extrapolate(sz, sz * 2, @middle[0], @tempOut[0]);


    for t:= (sz) to (sz * 2)-1 do begin
      tt := ((sz * 2)-1)-(t-sz);
//    for t:= 0 to (sz-1) do begin
      d := PDouble(PByte(output)+(sizeof(double)*((t-sz)+itemCount_TimeDomain)));

      if cx = 0 then
        d^ :=  tempOut[tt]
      else
//        d^ := (d^ + tempOut[t]) /2;
        d^ := Interpolate(t, d^, tempOut[tt], sz, sz * 2);

    end;
    dec(bit);
    inc(cx);
{$IFNDEF POWER2}
    dec(sz,16);
{$ENDIF}
//    break;
  end;
end;

procedure FFT_RealToComplex_Normalized(itemCount_TimeDomain: nativeint; input: PDouble; output: PComplex);
var
  cx: ni;
  save: PDouble;
begin

  FFT_RealToComplex(itemCount_TimeDomain, input, output);

  //normalize before FFT
  cx := itemCount_TimeDomain div 2;
  while cx > 0 do begin
    output^.i := output^.i / (0-(itemCount_TimeDomain/2));
    inc(output);
    dec(cx);
  end;

end;

procedure FFT_ComplexToReal_Normalized(itemCount_TimeDomain: nativeint; input: PComplex; output: PDouble; scratchSpace: PComplex = nil);
var
  cx: ni;
  save: PDouble;
  bFree: boolean;
  sz: ni;
  pc: PComplex;
begin
  bFree := scratchSpace = nil;

  sz := sizeof(complex)*itemCount_TimeDomain;
  if scratchSpace = nil then
    scratchSpace := GetMemory(sz);

  save := output;

  movemem32(scratchSpace, input, sz);
  pc := scratchSpace;


  //denormalize before FFT
  cx := itemCount_TimeDomain;
  while cx > 0 do begin
    pc^.i := pc^.i * (0-(itemCount_TimeDomain/2));
    inc(pc);
    dec(cx);
  end;

  FFT_ComplexToReal(itemCount_TimeDomain, scratchSpace, output);

  if bFree then
    FreeMemory(scratchSpace);


end;

procedure FFT_ComplexToReal_Normalized_Extrapolate(itemCount_TimeDomain: nativeint; outItemCount_TimeDomain: nativeint; input: PComplex; output: PDouble; scratchSpace: PComplex = nil);
var
  cx: ni;
  save: PDouble;
  bFree: boolean;
  sz: ni;
  pc, pc2: PComplex;
  rold, iold: double;
  r1,r2,i1,i2: double;
begin
  bFree := scratchSpace = nil;

  sz := sizeof(complex)*outitemCount_TimeDomain;
  if scratchSpace = nil then
    scratchSpace := GetMemory(sz);

  save := output;

  movemem32(scratchSpace, input, sz);


  //denormalize before FFT
  cx := itemCount_TimeDomain;
  pc := scratchSpace;
  while cx > 0 do begin
    pc^.i := pc^.i * (0-(outitemCount_TimeDomain/2));
    inc(pc);
    dec(cx);
  end;


  rold := 0;
  iold := 0;
  //widen array
  cx := itemCount_TimeDomain;
  pc := pComplex(pbyte(scratchSpace)+(cx*sizeof(Complex)));
  pc2 := pComplex(pbyte(scratchSpace)+(cx*sizeof(Complex))+((cx*sizeof(Complex))));
  repeat
    dec(cx);

    dec(pc);
//2
    i1 := pc^.i ;
    r1 := pc^.r ;
//1
    dec(pc2);
    pc2^.i := i1*0;
    pc2^.r := r1*0;

    dec(pc2);
    pc2^.i := i1*2;
    pc2^.r := r1*2;

//end
//    rold := pc^.r;
//    iold := pc^.i;
  until cx = 0;



  FFT_ComplexToReal(outitemCount_TimeDomain, scratchSpace, output);

  if bFree then
    FreeMemory(scratchSpace);

end;




procedure FFT_RealToComplex(itemCount_TimeDomain: nativeint; input: PDouble; output: PComplex);
var
  rplan: PByte;
begin
  LockFFT;
  try

//  if (nativeint(input) and 15) <> 0 then
//    raise ECritical.create('input is not aligned!');
//
//  if (nativeint(output) and 15) <> 0 then
//    raise ECritical.create('output is not aligned!');

//  Debug.Log('FFT Create');
  rplan := fftw_plan_dft_r2c_1d(itemCount_TimeDomain, input, PDouble(output), FFTW_ESTIMATE);  //<<<<----PLAN
  try
//    Debug.Log('FFT Execute');
    fftw_execute(rplan);
  finally
//    Debug.Log('FFT Destroy');
    fftw_destroy_plan(rplan);
//    Debug.Log('FFT Finish');
  end;
  finally
  UnlockFFT;
  end;



end;

procedure FFT_ComplexToReal(itemCount_TimeDomain: nativeint; input: PComplex; output: PDouble);
var
  rplan: PByte;
  cx: ni;
begin
  LockFFT;
  try
//  if (nativeint(input) and 15) <> 0 then
//    raise ECritical.create('input is not aligned!');
//
//  if (nativeint(output) and 15) <> 0 then
//    raise ECritical.create('output is not aligned!');

//  Debug.Log('FFT Create2');
  rplan := fftw_plan_dft_c2r_1d(itemCount_TimeDomain, PDouble(input), output, FFTW_ESTIMATE);  //<<<<----PLAN
  try
//    Debug.Log('FFT Execute2');
    fftw_execute(rplan);

//    Debug.Log('FFT Normalize');
    cx := itemCount_TimeDomain;
    while cx > 0 do begin
      output^ := output^ / (itemCount_TimeDomain);
      inc(output);
      dec(cx);
    end;
  finally
//    Debug.Log('FFT Destroy2');
    fftw_destroy_plan(rplan);
//    Debug.Log('FFT Finish2');
  end;
  finally
  UnlockFFT;
  end;
end;


end.
