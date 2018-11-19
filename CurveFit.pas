unit CurveFit;

{ Unit CurveFit contains procedure PolyFit.

  Purpose:

      Least-squares curve fit of arbitrary order.

  This code was derived from:
  
  ******************************************
  ****   Scientific Subroutine Library  ****
  ****         for Turbo Pascal         ****
  ******************************************

  The following programs were written by Allen Miller and appear in the
  book entitled "Pascal Programs For Scientists And Engineers" which is
  published by Sybex, 1981.
  They were originally typed and submitted to MTPUG in Oct. 1982
    Juergen Loewner
    Hoher Heckenweg 3
    D-4400 Muenster
  They have had minor corrections and adaptations for Turbo Pascal by
    Jeff Weiss
    1572 Peacock Ave.
    Sunnyvale, CA 94087.

2005 Feb 24   Changed all instances of type Real floating type
              to Double. - WPostma.
              
2000 Oct 28  Updated for Delphi 4, open array parameters.
             This allows the routine to be generalised so that it is no longer
             hard-coded to make a specific order of best fit or work with a
             specific number of points.
2001 Jan 07  Update Web site address
2003 Mar 05  Released for Delphi 5 as well
             Trap error when all Y values were the same (thanks Kelvin Lee)

Copyright © David J Taylor, Edinburgh and others listed above
Web site:  www.satsignal.net
E-mail:    davidtaylor@writeme.com
}

interface

procedure PolyFit (const x, y: array of Double;
                   var coefs: array of Double;
                   var correl_coef: Double;
                   const npoints, nterms: Integer);


implementation

uses
  SysUtils;

// Matrix multiplication routine
// A = transpose X times X
// G = Y times X     

type
  matrix = array of array of double;

// Form square coefficient matrix
procedure square (const x: matrix;
		  const y: array of double;
                  var a: matrix;
                  var g: array of double;
                  const nrow, ncol: integer);
var
 i, k, l: integer;
begin
  for k := 0 to ncol - 1 do
    begin
    for l := 0 to k do
      begin
      a [k, l] := 0.0;
      for i := 0 to nrow - 1 do
        begin
        a [k, l] := a [k, l] + x [i, l] * x [i, k];
        if k <> l then a [l, k] := a [k, l]
        end
      end;
    g [k] := 0.0;
    for i := 0 to nrow - 1 do g [k] := g [k] + y [i] * x [i, k]
    end;
end;


procedure swap (var a, b: Double);
var
 hold: Double;
begin
  hold := a;
  a := b;
  b := hold
end;


procedure GaussJordan (var b: matrix;            // square matrix of coefficients
                       const y: array of double;   // constant vector
                       var coef: array of double;  // solution vector
                       const ncol: integer;      // order of matrix
                       var error: boolean);      // true if matrix singular

{ Gauss Jordan matrix inversion and solution }

{ B (n, n) coefficient matrix becomes inverse }
{ Y (n) original constant vector }
{ W (n, m) constant vector(s) become solution vector }
{ DETERM is the determinant }
{ ERROR = 1 if singular }
{ INDEX (n, 3) }
{ NV is number of constant vectors }

var
  nv: integer;
  index: array of array of integer;
  w: array of array of Double;

  procedure GaussJordan2;
  var
    determ: Double;
    irow, icol: integer;

    procedure GaussJordan3;
    var
      m: integer;
    begin
      // Interchange rows to put pivot on diagonal
      if irow <> icol then
        begin
        determ := -determ;
        for m := 0 to ncol - 1 do swap (b [irow, m], b [icol, m]);
        if nv > 0 then
          for m := 0 to nv - 1 do swap (w [irow, m], w [icol, m])
        end;
    end;

  var
    i, j, k, m, n: integer;
    big, t: Double;
    pivot: Double;
  begin  { procedure gausj2 }
    // actual start of gaussj
    error := false;
    nv := 1;                  // single constant vector
    for i := 0 to ncol - 1 do
      begin
      w [i, 0] := y [i];      // copy constant vector
      index [i, 2] := -1
      end;
    determ := 1.0;
    for i := 0 to ncol - 1 do
      begin
      // Search for largest element
      big := 0.0;
      for j := 0 to ncol - 1 do
        begin
        if index [j, 2] <> 0 then
          begin
          for k := 0 to ncol - 1 do
            begin
           if index [k, 2] > 0 then
              begin
              error := true;
              Raise
                EMathError.Create ('Error in GaussJordan2: matrix is singular');
              end;
            if index [k, 2] < 0 then
              if abs (b [j, k]) > big then
                begin
                irow := j;
                icol := k;
                big := abs (b [j, k])
                end
            end       { k-loop }
          end
        end;		{ j-loop }
      index [icol, 2] := index [icol, 2] + 1;
      index [i, 0] := irow;
      index [i, 1] := icol;

      GaussJordan3;    // further subdivision of gaussj
                       // divide pivot row by pivot column

      pivot := b [icol, icol];
      determ := determ * pivot;
      b [icol, icol] := 1.0;

      for m := 0 to ncol - 1 do b [icol, m] := b [icol, m] / pivot;
      if nv > 0 then
        for m := 0 to nv - 1 do w [icol, m] := w [icol, m] / pivot;

      // Reduce nonpivot rows
      for n := 0 to ncol - 1 do
        begin
        if n <> icol then
          begin
          t := b [n, icol];
          b [n, icol] := 0.0;
          for m := 0 to ncol - 1 do b [n, m] := b [n, m] - b [icol, m] * t;
          if nv > 0 then
            for m := 0 to nv - 1 do w [n, m] := w [n, m] - w [icol, m] * t;
          end;
        end;

      end;       { i-loop }
  end;		{ GaussJordan2 }

var
  i, k, m: integer;
  irow, icol: integer;
begin		{ gauss-jordan main routine }
  SetLength (w, ncol, ncol);
  SetLength (index, ncol, 3);

  try
    GaussJordan2;         // first half of GaussJordan

    // Interchange columns
    for i := 0 to ncol - 1 do
      begin
      m := ncol - i - 1;
      if index [m, 0] <> index [m, 1] then
        begin
        irow := index [m, 0];
        icol := index [m, 1];
        for k := 0 to ncol - 1 do swap (b [k, irow], b [k, icol])
        end
      end;

    for k := 0 to ncol - 1 do
      if index [k, 2] <> 0 then
        begin
        error := true;
        Raise EMathError.Create ('Error in GaussJordan: matrix is singular');
        end;

    for i := 0 to ncol - 1 do coef [i] := w [i, 0];
  finally
    w := nil;
    index := nil;
  end;

end;	{ procedure GaussJordan }


// Least-squares fit to nrow sets of x and y pairs of points

procedure PolyFit (const x, y: array of Double; {was Real}
                   var coefs: array of Double; {was Real}
                   var correl_coef: Double;
                   const npoints, nterms: Integer);
var
  error: boolean;
  i, j: integer;
  xi, yi, yc, srs, sum_y,sum_y2: Double;
  xmatr: matrix;        // Data matrix
  a: matrix;
  g: Array of Double;     // Constant vector
begin
  if nterms < 1 then
    Raise EMathError.Create ('PolyFit called with less than one term');
  if npoints < 2 then
    Raise EMathError.Create ('PolyFit called with less than two points');

  SetLength (g, nterms);
  SetLength (a, nterms, nterms);
  SetLength (xmatr, npoints, nterms);
  for i := 0 to npoints - 1 do
    begin       { setup x matrix }
    xi := x [i];
    xmatr [i, 0] := 1.0;	        { first column }
    for j := 1 to nterms - 1 do
      xmatr [i, j] := xmatr [i, j - 1] * xi;
    end;
  square (xmatr, y, a, g, npoints, nterms);
  GaussJordan (a, g, coefs, nterms, error);
  sum_y := 0.0;
  sum_y2 := 0.0;
  srs := 0.0;
  for i := 0 to npoints - 1 do
    begin
      yi := y [i];
      yc := 0.0;
      for j := 0 to nterms - 1 do
	yc := yc + coefs [j] * xmatr [i, j];
      srs := srs + sqr (yc - yi);
      sum_y := sum_y + yi;
      sum_y2 := sum_y2 + yi * yi
    end;                      

  // If all Y values are the same, avoid dividing by zero
  correl_coef := sum_y2 - sqr (sum_y) / npoints;
  // Either return 0 or the correct value of correlation coefficient
  if correl_coef <> 0 then correl_coef := srs / correl_coef;
  if correl_coef >= 1
    then correl_coef := 0.0
    else correl_coef := sqrt (1.0 - correl_coef);

  g := nil;
  a := nil;
  xmatr := nil;
end;


end.

