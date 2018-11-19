unit helpers.list;

interface

uses system.Types, classes;

procedure FreeListContents(lst: TList);
procedure FreeStringListContents(lst: TStringlist);
procedure FreeList(var lst: TList);

implementation

procedure FreeListContents(lst: TList);
//a: Jason Nelson
//Frees objects in a list.  LIST ITEMS ARE CAST AS TOBJECT...  So be
//advised of the dangers that might exist with some objects.
//DO NOT use this to free components.... particularly if the components
//support interfaces, due to the potential that exceptions can be raised
//during destruction.
var
  p: pointer;
begin
  while lst.count > 0 do begin
    p := lst[0];
    TObject(p).Free;
    lst.Remove(p);
  end;
end;

{-------------------------------------------------------------------------}
procedure FreeList(var lst: TList);
begin
  FreeListContents(lst);
  lst.free;
  lst := nil;
end;


procedure FreeStringListContents(lst: TStringlist);
var
  t: integer;
begin
  for t:= lst.Count-1 downto 0 do begin
    lst.Objects[t].Free;
    lst.objects[t] := nil;
  end;
  lst.Clear;

end;



end.
