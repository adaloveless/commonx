unit maths;

interface

uses
  systemx, stringx, typex, curvefit, types, numbers, math;

type
  TDoubleArray = array of double;
  TFitResults = record
    correlation_coefficient: double;
    terms: array of double;
    function GetPoint(x: double): double;
  end;
  TPointArray = array of TPoint;

function AppendMirrorArray(a: array of double): TDoubleArray;
function CalculateFit(x,y: array of double; bForceOrigin: boolean = false; number_of_terms: ni = 2): TFitResults;
function CenterIterate(w,h: ni; t: ni): TPoint;
function CenterIterate2(cx,cy: ni; t: ni): TPoint;
function CreateCenterIterateMap(w,h: ni): TPointArray;

implementation

function CreateCenterIterateMap(w,h: ni): TPointArray;
var
  r: ni;
  rEnd: ni;
  x,y: ni;
  tick: ni;
  cx,cy: ni;
  lx1,lx2,ly1,ly2: ni;
  cx1,cx2,cy1,cy2: ni;
begin
  r := 0;
  rEnd := greaterof(w div 2, h div 2);
  tick := -1;
  cx := w div 2;
  cy := h div 2;
  setlength(result, w*h);

  while r < rEnd do begin
    cx1 := cx-r;
    cx2 := cx+r;
    cy1 := cy-r;
    cy2 := cy+r;

    for y := cy1 to cy2 do begin
      for x := cx1 to cx2 do begin
        if x < 0 then continue;
        if x >= w then continue;
        if y < 0 then continue;
        if y >= h then continue;

        if (x<=cx1)
        or (x>=cx2)
        or (y<=cy1)
        or (y>=cy2)
        then begin
          if tick = w*h then
            exit;
          inc(tick);
          result[tick].x := x;
          result[tick].y := y;
        end;
      end;
    end;
    inc(r);
  end;

  cx1 := 0;
  cx2 := h-1;
  cy1 := 0;
  cy2 := w-1;
  for y := 0 to h-1 do begin
    for x := 0 to w-1 do begin
      if (x<=cx1)
      or (x>=cx2)
      or (y<=cy1)
      or (y>=cy2)
      then begin
        inc(tick);
        if tick = w*h then
          exit;
        result[tick].x := x;
        result[tick].y := y;
      end;
    end;
  end;
end;


function CenterIterate2(cx,cy: ni; t: ni): TPoint;
var
  r: ni;
  rEnd: ni;
  x,y: ni;
  tick: ni;
  lx1,lx2,ly1,ly2: ni;
  cx1,cx2,cy1,cy2: ni;
begin
  r := 0;
  tick := -1;

  cx1 := cx-r;
  cx2 := cx+r;
  cy1 := cy-r;
  cy2 := cy+r;

  while tick < t do begin
    for y := cy1 to cy2 do begin
      for x := cx1 to cx2 do begin
        if (x<=cx1)
        or (x>=cx2)
        or (y<=cy1)
        or (y>=cy2)
        then begin
          inc(tick);
          result.x := x;
          result.y := y;
        end;
      end;
    end;
    inc(r);
  end;



end;


function CenterIterate(w,h: ni; t: ni): TPoint;
var
  r: ni;
  rEnd: ni;
  x,y: ni;
  tick: ni;
  cx,cy: ni;
  lx1,lx2,ly1,ly2: ni;
  cx1,cx2,cy1,cy2: ni;
begin
  r := 0;
  rEnd := w div 2;
  tick := -1;
  cx := w div 2;
  cy := h div 2;

  while r < rEnd do begin
    cx1 := cx-r;
    cx2 := cx+r;
    cy1 := cy-r;
    cy2 := cy+r;

    for y := cy1 to cy2 do begin
      for x := cx1 to cx2 do begin
        if (x<=cx1)
        or (x>=cx2)
        or (y<=cy1)
        or (y>=cy2)
        then begin
          inc(tick);
          result.x := x;
          result.y := y;
          if tick = t then
            exit;
        end;
      end;
    end;
    inc(r);
  end;

  cx1 := 0;
  cx2 := h-1;
  cy1 := 0;
  cy2 := w-1;
  for y := 0 to h-1 do begin
    for x := 0 to w-1 do begin
      if (x<=cx1)
      or (x>=cx2)
      or (y<=cy1)
      or (y>=cy2)
      then begin
        inc(tick);
        result.x := x;
        result.y := y;
        if tick = t then
          exit;
      end;
    end;
  end;
end;


function AppendMirrorArray(a: array of double): TDoubleArray;
var
  l: ni;
  t: ni;
begin
  l := length(a);
  setlength(result, l*2);
  for t := 0 to l-1 do begin
    result[t] := a[t];
    result[t+l] := 0-a[t];
  end;
end;

function CalculateFit(x,y: array of double; bForceOrigin: boolean = false; number_of_terms: ni = 2): TFitResults;
var
  coefs: array of double;
  correl: double;
begin
  setlength(coefs, number_of_terms);
  if bForceOrigin then begin
    PolyFit(AppendMirrorArray(x),AppendMirrorArray(y),coefs, correl, length(x)*2, number_of_terms)

  end else
    PolyFit(x,y,coefs, correl, length(x), number_of_terms);
  setlength(result.terms, length(coefs));
  movemem32(@result.terms[0], @coefs[0], length(result.terms)*sizeof(double));
  //result.terms := coefs;
  result.correlation_coefficient := correl;

end;



{ TFitResults }

function TFitResults.GetPoint(x: double): double;
var
  t: ni;
begin
  result := terms[0];
  for t:= 1 to high(terms) do begin
    result := result + (terms[t]*power(x,t));
  end;
end;

end.
