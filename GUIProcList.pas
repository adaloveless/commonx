unit GUIProcList;

interface

uses
  typex, windows, better_collections, controls, generics.collections;


type
  TGUIProcRec = record
    c: TWinControl;
    p: TWndMethod;
  end;

  PGUIProcRec = ^TguiPRocRec;


  TGUIProcList = class(Tlist<TGUIProcRec>)
  public
    function findmethod(c: TWinControl): TWndMethod;
    function findcontrol(c: TWinControl): ni;
    function hascontrol(c: TWinControl): boolean;

  end;




implementation

{ TGUIProcList }

function TGUIProcList.findcontrol(c: TWinControl): ni;
var
  t: ni;
begin
  result := -1;
  for t:= 0 to Count-1 do begin
    if items[t].c = c then
      exit(t);
  end;

end;

function TGUIProcList.findmethod(c: TWinControl): TWndMethod;
var
  t: ni;
  m: TWndMethod;
begin
  result := nil;
  for t:= 0 to Count-1 do begin
    if items[t].c = c then begin
      m := items[t].p;
      result := m;
      exit;
    end;
  end;

end;

function TGUIProcList.hascontrol(c: TWinControl): boolean;
begin
  result := findcontrol(c) >= 0;
end;

end.
