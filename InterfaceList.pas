unit InterfaceList;
//this unit contains the TInterfaceList class

interface
uses classes;

type
  TInterfaceList = class
    //This class is like a TList, but it allows interfaces to be properly
    //reference counted.
    FSize : integer;
    Farray : array of IUnknown;
    function GetItem(idx: integer): IUnknown;
    function GetCount: integer;
  private
    procedure SetItem(idx: integer; const Value: IUnknown);
  published
  public
    constructor create; reintroduce;
    destructor destroy; override;
    procedure Add(obj: IUnknown);
    procedure Delete(idx: integer);
    function Remove(obj: IUnknown): boolean;
    function IndexOf(obj: IUnknown): integer;
    procedure Clear;
    property Count: integer read GetCount;
    property Items[idx: integer]: IUnknown read GetItem write SetItem; default;
  end;

implementation
//------------------------------------------------------------------------------
constructor TInterfaceList.create;
//Standard.
begin
  inherited;
  FSize := 0;
end;
//------------------------------------------------------------------------------
destructor TInterfaceList.destroy;
//Standard.
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TInterfaceList.Add(obj: IUnknown);
//Adds an interface to the list.
begin
  inc(FSize);
  if FSize>Length(FArray) then begin

    if FSIZE<64 then
      //if a small list, increment little by little to conserve memory
      SetLength(FArray, FSize+8)
    else
      //if a large list, icrement by bigger chunks to increse performance
      SetLength(FArray, FSize+64)
  end;

  FArray[FSize-1] := obj;


end;
//------------------------------------------------------------------------------
procedure TInterfaceList.Delete(idx: integer);
//Deletes an interface a given index.
var
  t: integer;
begin
  FArray[idx] := nil;

  for t:= idx to FSize - 2 do begin
    FArray[t] := FArray[t+1];
  end;

  FArray[FSize-1] := nil;

  dec(FSize);
  if FSize<(Length(FArray)div 2) then
    SetLength(FArray, FSize);



end;
//------------------------------------------------------------------------------
function TInterfaceList.IndexOf(obj: IUnknown): integer;
//Returns the Index of a given interface.
var
  t: integer;
begin
  result := -1;

  for t:= 0 to FSize -1 do begin
    if FArray[t] = obj then begin
      result := t;
      break;
    end;
  end;


end;
//------------------------------------------------------------------------------
procedure TInterfaceList.Clear;
//Removes all interfaces from the list.
begin
  while FSize > 0 do begin
    Delete(FSize-1);
  end;
end;
//------------------------------------------------------------------------------
function TInterfaceList.GetItem(idx: integer): IUnknown;
//Returns an interface a given index. Getter for the Items property.
begin
  result := FArray[idx];
end;
//------------------------------------------------------------------------------
function TInterfaceList.Remove(obj: IUnknown): boolean;
//Removes an object from the list using the object itself.
//Returns: true if the object was removed, false if the object in question was
//never in the list to begin with.
var
  i: integer;
begin
  i := IndexOF(obj);

  if i>-1 then begin
    Delete(i);
    Result := true;
  end else
    result := false;
end;
procedure TInterfaceList.SetItem(idx: integer; const Value: IUnknown);
begin

  self.Farray[idx] := value;
end;

//------------------------------------------------------------------------------
function TInterfaceList.GetCount: integer;
//Returns the number of interfaces in the list.  Getter for the count property.
begin
  result := FSize;
end;

end.
