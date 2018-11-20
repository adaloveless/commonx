unit spectrum_viewer;

interface

uses
  typex, soundinterfaces, systemx, advancedgraphics_dx, geometry, easyimage, colorconversion, graphics, direct3d9_jedi, soundtools, fftw_interface, math, advancedgraphics;


const
  band_count = 256;
  highband = band_count-1;
  FALL_RATE = 9000;
type
  TSpectrumViewer = class(TDX2d)
  public
    Buckets: array[0..highband] of extended;
    Peaks: array[0..highband] of nativefloat;
    Counts: array[0..highband] of integer;
    sso: TSoundStreamOscillator;
    procedure Anal;

    procedure DoDraw;override;
    function Bands(i: nativeint): nativeint;
    procedure Fall;


  end;

implementation

{ TSpectrumViewer }

procedure TSpectrumViewer.Anal;
var
  t,u,i,f: integer;
  r: real;
  la: array [0..8191] of double;
  lOUT: array [0..8191] of fftw_complex;
  rplan: PByte;
  m: integer;
  absval,cc,erg,correction: real;


  iWidth: extended;
  fMax: extended;
  rBucketTop: extended;
  rBucketBottom: extended;
  i1,i2: single;
  iWindowLength: integer;
  iPos: int64;
const
  SPLIT = 2;
  BAND_BASE  = 150;
//  bands: array [0..31] of integer = (
//     BAND_BASE+100,
//     BAND_BASE+200,
//     BAND_BASE+300,
//     BAND_BASE+400,
//     BAND_BASE+500,
//     BAND_BASE+750,
//     BAND_BASE+1000,
//     BAND_BASE+1500,
//     BAND_BASE+2000, BAND_BASE+4000, BAND_BASE+8000, BAND_BASE+16000, BAND_BASE+22000);
//  bands: array [0..12] of integer = (250,500,1000,2000,3000,4000,5000,6000,7000,8000,10000,11000,12000);

begin
  if not assigned(sso) then
    exit;


  rPlan := nil;
  iWindowLength := 2048;
  correction := 44100 / (iWindowlength / 2);

  fMax := 0;
//  la := fftw_malloc(iWindowLength*sizeof(single));
//  lout := fftw_malloc(iWindowlength*sizeof(single));
  try


    //rplan := fftw_plan_dft_r2r_1d(iWindowLength, @la[0], @lout[0], FFTW_R2HC, FFTW_FORWARD);
    LockFFT;
    try
      rplan := fftw_plan_dft_r2c_1d(iWindowLength, @la[0], @lout[0], FFTW_ESTIMATE);
    finally
      UnlockFFT;
    end;
    try
      // we have no imaginary data, so clear idata
      for u := low(lout) to high(lout) do begin
        lout[u].r := 0;
        lout[u].i := 0;
      end;

      //CLEAR BUCKETS
      for f:= 0 to highband do begin
        buckets[f] := 0;
        counts[f] := 0;
      end;

      // fill rdata with actual data
      for u := 0 to iWindowLength-1 do begin

        sso.GetVUHistory(u,i1,i2);
        i1 := (i1+i2)/2;
        la[u] := round(i1*32);
      end;
      // make fft
      fftw_execute(rplan);
      //
      //	// post-process FFT data: make absolute values, and calculate
      //	//   real frequency of each power line in the spectrum

        m := 0;
        for i := 1 to (iWindowLength-2) div SPLIT do begin

          absval := (lout[i].r+lout[i].i)/iWindowLength;
//
//          absval := abs(lout[i*2]);
          m := i ;
          cc := m * correction;

          for f := 0 to highband do begin

{$IFDEF DONT_USE_BANDS}
            rBucketBottom := (146+((50*(round(sqrt(2 shl f))))));
            rBucketTop := (146+((50*(round(sqrt(2 shl (f+1)))))));
{$ELSE}
            rBucketBottom := bands(f);
            rBucketTop := bands(f+1);
{$ENDIF}

            iWidth := rBucketTop-rBucketBottom;


            if (cc >= rBucketBottom)
            and (cc < rBucketTop) then begin


              //compensate for lower frequencies having more power
              //absval := absval * (1+(2*(f/11)));
{xDEFINE PEAK_SEARCH}
{$IFDEF PEAK_SEARCH}
              if absval > aBuckets[f] then
                aBuckets[f] := ((absval));
              acounts[f] := 1;
{$ELSE}
              Buckets[f] := Buckets[f]+((absval));
              counts[f] := counts[f] + 1;
{$ENDIF}
            end;
          end;
        end;

        for f := 0 to highband do begin
          if counts[f] = 0 then
            buckets[f] := 0
          else
            buckets[f] := buckets[f] / counts[f];

          if buckets[f] > peaks[f] then
            peaks[f] := buckets[f];
        end;



        for f := 0 to highband do begin
          if buckets[f] > fMax then fMax := buckets[f];
        end;
      except
      end;






  finally
//    fftw_free(la);
//    fftw_free(lout);
    if assigned(rPlan) then begin
      LockFFT;
      try
        fftw_destroy_plan(rplan);
      finally
        UnlockFFT;
      end;
    end;
  end;
end;

function TSpectrumViewer.Bands(i: nativeint): nativeint;
var
  baseint: nativeint;
begin

  result := round(146*((i/band_count)*110));


end;

procedure TSpectrumViewer.DoDraw;
var
  t: integer;
  c1,c2: integer;
begin
  inherited;
  try
    AlphaOp := aoStandard;
    rectangle_fill(boundx1,boundy1,boundx2,boundy2,clBlack, 1);
    BeginVertexBatch(D3dPT_TriangleLIst);
    BoundX1 := 0;
    BoundX2 := band_count;
    BoundY1 := 0;
    BoundY2 := 1;

    Fall;
    Anal;


    AlphaOp := aoAdd;
    for t:= 0 to highband do begin
      c1 := colorblend(clRed, clBlue, t/highband);
      rectangle_fill(t,boundy2-buckets[t],t+1, boundy2, c1, clBlack, clBlack, c1);
    end;
    for t:= 0 to highband do begin
      c1 := colorblend(clRed, clBlue, t/highband);
      rectangle_fill(t,boundy2-peaks[t],t+1, boundy2, c1, clBlack, clBlack, c1);
    end;
  finally
    EndVertexBatch();
  end;

end;

procedure TSpectrumViewer.Fall;
var
  t: integer;
begin
  for t:= 0 to highband do begin
    peaks[t] := peaks[t] - FALL_RATE;
    if peaks[t] < 0 then
      peaks[t] := 0;
  end;

end;

end.
