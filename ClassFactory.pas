unit ClassFactory;
{$INCLUDE DelphiDefs.inc}

interface

uses
  typex, generics.collections.fixed, sysutils, classes, betterobject, sharedobject;

type

  TClass = class of TBetterObject;

  TClassFactory<TTT: class> = class(TBetterObject)
  //todo 5: this class can be optimized if the class list is sorted
  //todo 5: and a binary search is used
  //todo 5: also store a lowercase index of classnames
  private
    FList: TList;
    function GetClass(idx: ni): TClass;
    function Getcount: ni;
  public
    constructor Create;override;
    destructor Destroy;override;
    property Classes[idx: ni]: TClass read GetClass;
    function IndexOf(const sClassName: string): ni;
    function CreateClass(sName: string): TTT;
    procedure RegisterClass(cl: TClass);
    property Count: ni read Getcount;
  end;

  TClassClassFactory = class(TSharedObject)
  //todo 5: this class can be optimized if the class list is sorted
  //todo 5: and a binary search is used
  //todo 5: also store a lowercase index of classnames
  private
    FListCReate: TList;
    FListFor: TList;
    function GetClass(idx: ni): TBetterObjectClass;
    function Getcount: ni;
  public
    constructor Create;override;
    destructor Destroy;override;
    property Classes[idx: ni]: TBetterObjectClass read GetClass;
    function IndexOf(const sFor: TClass): ni;
    function CreateClass(cfor: TClass): TBetterObject;
    function CreateClassByName(sName: string): TBetterObject;
    procedure RegisterClass(cfor_pointertoCLASS: pointer; cl: TBetterObjectClass);
    property Count: ni read Getcount;
  end;


implementation

{ TUnitTestFactory<T> }

constructor TClassFactory<TTT>.Create;
begin
  inherited;
  FList := TList.create;

end;

function TClassFactory<TTT>.CreateClass(sName: string): TTT;
var
  i: ni;
begin
  i := self.IndexOf(sName);
  if i < 0 then
    result := TTT(nil)
  else
    result := TTT(Classes[i].create);


end;

destructor TClassFactory<TTT>.Destroy;
begin

  FList.Free;
  inherited;
end;

function TClassFactory<TTT>.GetClass(idx: ni): TClass;
begin
  result := FList[idx];
end;

function TClassFactory<TTT>.Getcount: ni;
begin
  result := FList.Count;
end;

function TClassFactory<TTT>.IndexOf(const sClassName: string): ni;
var
  t: ni;
  s: string;
begin
  s := lowercase(sClassname);
  result := -1;
  for t:= 0 to Flist.Count-1 do begin
    if lowercase(TClass(FList[t]).Classname) = s then begin
      result := t;
      exit;
    end;
  end;
end;

procedure TClassFactory<TTT>.RegisterClass(cl: TClass);
begin
  fList.Add(cl);
end;



{ TClassClassFactory<T_FOR, T_CREATE> }

constructor TClassClassFactory.Create;
begin
  inherited;
  FListCReate := Tlist.create;
  FlistFor := TList.create;
end;

function TClassClassFactory.CreateClass(cfor: TClass): TBetterObject;
var
  i: ni;
begin
  i := IndexOf(cfor);
  result := nil;
  if i >=0 then
    result := Classes[i].Create;

end;

function TClassClassFactory.CreateClassByName(sName: string): TBetterObject;
var
  t: ni;
begin
  result := nil;
  for t := 0 to FListFor.count-1 do begin
    if comparetext(classes[t].ClassName, sNAme)=0 then
      result := classes[t].Create;
  end;

end;

destructor TClassClassFactory.Destroy;
begin
  FListcreate.Free;
  FListFor.free;
  inherited;
end;

function TClassClassFactory.GetClass(idx: ni): TBetterObjectClass;
begin
  result := TBetterObjectClass(FListCreate[idx]);

end;

function TClassClassFactory.Getcount: ni;
begin
  result := FListFor.count;

end;


function TClassClassFactory.IndexOf(const sFor: TClass): ni;
var
  t: ni;
begin
  result := -1;
  //probably dont need locks because we generally only ADD to these lists
  for t:= 0 to FListFor.Count-1 do begin
    if FListFor[t] = pointer(sFor) then begin
      result := t;
      exit;
    end;
  end;

end;

procedure TClassClassFactory.RegisterClass(cfor_pointertoCLASS: pointer; cl: TBetterObjectClass);
begin
  lock;
  try
    if flistcreate.indexof(cl)<0 then begin
      FListCReate.add(cl);
      FListFor.add(cfor_pointertoCLASS);
    end;
  finally
    unlock;
  end;
end;

end.
