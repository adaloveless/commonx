unit sort;

interface

uses
  typex;

function Order(var i1,i2: nativeint):boolean;overload;
function Order(var i1,i2: integer): boolean;overload;
function Order(var i1,i2: byte): boolean;overload;
function Order(var i1,i2: nativefloat): boolean;overload;


implementation


function Order(var i1,i2: nativeint):boolean;
var
  t: nativeint;
begin
  result := false;
  if (i1>i2) then begin
    t := i1;
    i1 := i2;
    i2 := t;
    result := true;
  end;
end;


function Order(var i1,i2: byte):boolean;
var
  t: byte;
begin
  result := false;
  if (i1>i2) then begin
    t := i1;
    i1 := i2;
    i2 := t;
    result := true;
  end;
end;

function  Order(var i1,i2: nativefloat):boolean;
var
  t: nativefloat;
begin
  result := false;
  if (i1>i2) then begin
    t := i1;
    i1 := i2;
    i2 := t;
    result := true;
  end;
end;

function Order(var i1,i2: integer):boolean;
var
  t: integer;
begin
  result := false;
  if (i1>i2) then begin
    t := i1;
    i1 := i2;
    i2 := t;
    result := true;
  end;
end;



end.
