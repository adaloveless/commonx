
unit fft_native2;

interface

{ fft and inverse fft routines }
{ C version by Bob Palais  }
{ Converted to Pascal by Richard Palais   palais@brandeis.edu  }

{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{This unit provides two routines, (fft and ifft), for }
{implementing the Discrete Fourier Transform and its inverse }
{(using one variant of the Fast Fourier Transform algorithm). }
{These routines operate (in place) on a one-dimensional }
{array of length N of elements of type complex, where the}
{complex type should be defined by:
{   complex = record                             }
{			re, im: double;                    }
{	        	end;                            .}
{The positive integer N should be a power of two }
{that is less than or equal to a constant,}
{ MaxFFTArraySize (set to 1024 below, but it }
{can be increased at the expense of using more memory.}
{An array on which fft and ifft is to operate should be declared }
{as a pointer :
{  MyFFTArrayPtr: FFTArrayPtr      }
{that should be initialized with a call like:}
{MyFFTArrayPtr :=  FFTArrayPtr(NewPtr(SizeOf(FFTArray))): }
{(of course in a unit that "uses" this unit) }
{and it should eventually be disposed of with a call like:}
{DisposePtr(Ptr(MyFFTArrayPtr));}

{The array will usually be set up using some routine like }
{    for i:= 0 to N-1 do                               }
{      MyFFTArrayPtr^[i] := g(i);                    }
{where g(i) is some function that assigns a complex number}
{to the integer i. To Fourier transform the array call}

{   fft(MyFFTArrayPtr, N); }

{and to inverse Fourier transform it call}
{   fft(MyFFTArrayPtr, N); }

{Of course arrays of real numbers can also be transformed }
{using the usual convention that a real number as a complex }
{number with imaginary part equal to zero.}

{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

const
  MaxFFTArraySize = 1024;
  pi = 3.1415926535898;

type

 complex = record
			re, im: double;
	        	end;

  FFTArray = array[0..MaxFFTArraySize-1] of complex;
  FFTArrayPtr = ^FFTArray;

var
  twid :FFTArrayPtr;
  CurrentFFTArraySize:integer;

 procedure SetupTwid(NewFFTarraySize:integer);
 procedure  fft(c:FFTArrayPtr; N:integer);
 procedure ifft(c:FFTArrayPtr; N:integer);
 procedure ifft_extrapolate(c:FFTArrayPtr; N:Integer);                  (* inverse fft *)

 implementation

 procedure SetupTwid(NewFFTarraySize:integer);
    var
      j:integer;
      h:double;
    begin
      if twid <> nil then
        freememory(twid);

      twid := GetMemory(NewFFTArraySize*sizeof(complex));
       if not (NewFFTarraySize = CurrentFFTArraySize) then
         begin
             h := 2*pi/NewFFTarraySize;
             for j := 0 to (NewFFTarraySize div 2) - 1 do
               begin
                 twid^[j].re := cos(j*h);
                 twid^[j].im := -sin(j*h);
               end;
            CurrentFFTArraySize := NewFFTarraySize;
          end;
    end;

 function Cadd(a, b:complex):complex;
	var c:complex;
	 begin
	   c.re:=a.re + b.re;
	   c.im:=a.im + b.im;
	   Cadd := c;
	 end;

 function Csub(a,b: complex):complex;
	var c:complex;
	 begin
       c.re := a.re - b.re;
	   c.im := a.im - b.im;
	   Csub := c;
	 end;

 function Cmul(a,b:complex):complex;
	var c:complex;
	 begin
        c.re := a.re * b.re - a.im * b.im;
        c.im := a.im * b.re + a.re * b.im;
		Cmul := c;
	 end;

 function Comp(a,b:double):complex;
	var c:complex;
	 begin
		c.re := a;
		c.im := b;
		Comp := c;
	 end;

 function Cdiv(a, b:complex):complex;
	var c:complex;
	    mod2:double;
	 begin
       mod2 := sqr(b.re) + sqr(b.im);
       c.re :=(a.re * b.re + a.im * b.im)/mod2;
       c.im :=(a.im * b.re - a.re * b.im)/mod2;
	   Cdiv := c;
	 end;

 function RCmul(a:double; b:complex):complex;
	var c:complex;
	 begin
       c.re := a * b.re;
       c.im := a * b.im;
	   Rcmul := c;
	 end;

 function Cexp(x,y:double):complex;
	 var c:complex;
	  begin
        c.re:= exp(x)*cos(y);
        c.im:= exp(x)*sin(y);
	    cexp := c;
	  end;


 function Cconjg(c:complex):complex;
	var d:complex;
	  begin
        d.re :=   c.re;
	    d.im := - c.im;
	    Cconjg := d;
	  end;

 function Cabs(c:complex):double;
	var d:double;
	 begin
         d:=sqrt(c.re * c.re + c.im * c.im);
	     Cabs := d;
	 end;

 function Cexpi(y:double):complex;
	var c:complex;
	 begin
       c.re := cos(y);
       c.im := sin(y);
	   Cexpi :=  c;
	 end;



 procedure fft( c:FFTArrayPtr; N:integer);      		  (* fft *)

	 var
	    i,j,bit:integer;
	    numtrans,sizetrans,halfsize:integer;
	    ind,sub:integer;
	    iminus,iplus:integer;
	    twiddle:complex;
	    but1,but2:complex;
	    temp:complex;

	 begin
	    SetupTwid(N);
        i:=0;   										  (* bit reversal *)
	    for j:=1 to N - 1 do   							  (* count with j *)
	        begin 										  (* binary add 1 to i in mirror *)
              bit:=N div 2;
	          while ( i >= bit ) do   					  (* until you encounter 0, change 1 to 0 *)
	             begin
                   i:=i-bit;
	               bit:=bit div 2;
	            end;
	          i := i+bit;		         				  (* then change 0 to 1 *)
	          if  (i<j) then
	            begin
                 temp:=c^[i]; c^[i]:=c^[j]; c^[j]:=temp;  (* swap once for each pair *)
	            end;
	        end;

	   numtrans := N;
	   sizetrans := 1;

	  while (numtrans > 1) do
	    begin
          numtrans := numtrans div 2;                      (* at each level, do half as many *)
	      halfsize := sizetrans;
	      sizetrans:= 2 * sizetrans;                       (* subtransforms of twice the size *)
	      for ind := 0  to  halfsize - 1 do                (* index in each subtransform *)
	        begin
              twiddle := twid^[ind * numtrans];	           (* sharing common twiddle *)
	          for sub :=0 to numtrans -1  do               (* index of subtransform *)
	             begin
                   iplus:=sub * sizetrans + ind;           (* indices for butterfly *)
	               iminus := iplus + halfsize;
	               but1 := c^[iplus];
	               but2 := Cmul(twiddle,c^[iminus]);        (* lower one gets twiddled *)
	               c^[iplus]  :=  Cadd(but1,but2);          (* butterfly *)
	               c^[iminus] :=  Csub(but1,but2);
	             end;
	        end;
	   end;
  end;


procedure ifft(c:FFTArrayPtr; N:Integer);                  (* inverse fft *)
var k:integer;
  Ninv:double;
begin
  SetupTwid(N);
    Ninv:=1.0/N;
  for k:= 0 to N - 1 do
     c^[k]:=Cconjg(c^[k]);
  fft(c,N);
  for k:=0 to N - 1 do
      c^[k]:=RCmul(Ninv,Cconjg(c^[k]));
end;

procedure ifft_extrapolate(c:FFTArrayPtr; N:Integer);                  (* inverse fft *)
var k:integer;
  Ninv:double;
begin
  SetupTwid(N*2);
    Ninv:=1.0/N;
  for k:= 0 to (N*2) - 1 do
     c^[k]:=Cconjg(c^[(k)]);
  fft(c,N);
  for k:=0 to (N*2) - 1 do
      c^[k]:=RCmul(Ninv,Cconjg(c^[(k)]));
end;


initialization
  twid := nil;

finalization
  if twid <> nil then
    freememory(twid);

end.
