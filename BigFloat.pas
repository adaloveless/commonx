unit BigFloat;

interface

uses
  stringx, typex, sysutils;

type
  TBigFloat = record
    sig: int64;
    exp: int64;
    procedure optimize;
    class operator add(a,b: TBigFloat): TbigFloat;
    class operator subtract(a,b: Tbigfloat): Tbigfloat;
    class operator multiply(a,b: Tbigfloat): Tbigfloat;
    class operator divide(a,b: Tbigfloat): Tbigfloat;
    function ToString: string;
    procedure FromString(s: string);
  end;

implementation

{ BigFloat }



function FillStr(sContain: char; len: ni): string;
var
  t: ni;
begin
  setlength(result, len);
  for t := STRZ to STRZ+len do
    result[t] := sContain;


end;

class operator TBigFloat.add(a, b: TBigFloat): TbigFloat;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

class operator TBigFloat.divide(a, b: Tbigfloat): Tbigfloat;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TBigFloat.FromString(s: string);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

class operator TBigFloat.multiply(a, b: Tbigfloat): Tbigfloat;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TBigFloat.optimize;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

class operator TBigFloat.subtract(a, b: Tbigfloat): Tbigfloat;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TBigFloat.ToString: string;
var
  siglen: ni;
begin
  result := inttostr(sig);
  if exp  < 0 then begin
    siglen := (sig div 10)+1;
    if (0-exp) < siglen then //0.5 exp=-1  5/10=0... exp < 0  ... .0
      result := '.'+fillstr('0',exp-siglen)+result
    else
    if exp > 0 then
      result := result + fillstr('0',exp)
    else
      result := zcopy(result, 0, length(result)-(0-exp))+'.'+zcopy(result, length(result)-(0-exp), length(result))

  end;

end;

end.
