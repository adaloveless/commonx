unit ut_List;

interface

uses
  unittest, better_collections, stringx, numbers, betterobject, typex, systemx, sysutils;

type
  TTestObject = class(TBetterobject)
  public
    tag: string;
    function Debug: string;
    constructor Create(sTag: string); reintroduce;virtual;
  end;

  TLocalList = class(TBetterList<TTEstObject>);

  TUT_List = class(TUnitTest)
  public
    m,i,s,p,n,e,o,t,a: TTestObject;
    procedure CreateCommonObjects;
    procedure DestroyCommonObjects;
    procedure AddTestSet(l: TLocalList);
    procedure DoExecute; override;
    function BasicTest(): string;
    function InsertTest(idx: ni): string;
    function ComprehensiveTest: string;
    function DeleteTest(idx: ni): string;
    function RemoveTest(sREmove: TTestObject): string;
    function DebugListState(l: TLocalList): string;
  end;


implementation

{ TUT_List }

procedure TUT_List.AddTestSet(l: TLocalList);
begin
  l.Add(m);
  l.Add(i);
  l.Add(s);
  l.Add(s);
  l.Add(i);
  l.Add(s);
  l.Add(s);
  l.Add(i);
  l.Add(p);
  l.Add(p);
  l.Add(i);
end;

function TUT_List.BasicTest: string;
var
  l: TLocalList;
begin
  VariationNAme := 'Add [m i s s i s s i p p i]';
  l := TLocalList.create;

  try
    AddTestSet(l);
    result := DebugListSTate(l);

  finally
    l.free;
  end;

end;

function TUT_List.ComprehensiveTest: string;
var
  l: TLocalList;
  itm:  TTestObject;
begin
  VariationNAme := 'Add [m i s s i s s i p p i]'+NEWLINE+
                   'then convert to m i n n e s o t a ';
  l := TLocalList.create;
  try
    AddTestSet(l);
    l.Delete(2);
    l.Delete(3);
    l.insert(2, n);
    l.insert(3, n);
    l.Delete(4);
    l.insert(4,e);
    l.Delete(6);
    l.insert(6,o);
    while l.remove(p)>=0 do ;
    l.Delete(l.Count-1);
    l.Delete(l.Count-1);
    l.Add(t);
    l.add(a);

    result := DebugListSTate(l);

  finally
    l.free;
  end;
end;

procedure TUT_List.CreateCommonObjects;
begin
  m := TTestObject.create('M');
  i := TTestObject.create('I');
  s := TTestObject.create('S');
  p := TTestObject.create('P');
  n := TTestObject.create('N');
  e := TTestObject.create('E');
  o := TTestObject.create('O');
  t := TTestObject.create('T');
  a := TTestObject.create('A');
end;

function TUT_List.DebugListState(l: TLocalList): string;
var
  t: ni;
begin
  result := '';
  for t:= 0 to l.Count-1 do begin
    result := result + '[('+inttostr(t)+') '+l[t].Debug+']'+NEWLINE;
  end;
end;

function TUT_List.DeleteTest(idx: ni): string;
var
  l: TLocalList;
  itm:  TTestObject;
begin
  VariationNAme := 'Add [m i s s i s s i p p i]'+NEWLINE+
                   'then Delete index '+idx.tostring;
  l := TLocalList.create;
  try

    AddTestSet(l);
    itm := l.items[idx];
    l.delete(idx);
    result := DebugListSTate(l);

  finally
    l.free;
  end;

end;

procedure TUT_List.DestroyCommonObjects;
begin
  m.free;
  i.free;
  s.free;
  p.free;
  n.free;
  e.free;
  o.free;
  t.free;
  a.free;

end;

procedure TUT_List.DoExecute;
begin
  inherited;
  CreateCommonObjects;
  try

  case variation of
    10: utresult := BasicTest();
    20..29: utresult := DeleteTest(variation-20);
    30: utresult := RemoveTest(s);
    31: utresult := RemoveTest(i);
    32: utresult := RemoveTest(a);
    40..49: utresult := InsertTest(variation-40);
    50: utResult := ComprehensiveTest;
  end;

  finally
    DestroyCommonObjects;
  end;
end;


function TUT_List.InsertTest(idx: ni): string;
var
  l: TLocalList;
  itm:  TTestObject;
begin
  VariationNAme := 'Add [m i s s i s s i p p i]'+NEWLINE+
                   'then insert A at '+idx.tostring;
  l := TLocalList.create;
  try

    AddTestSet(l);
    l.Insert(idx, a);
    result := DebugListSTate(l);

  finally
    l.free;
  end;
end;

function TUT_List.RemoveTest(sREmove: TTestObject): string;
var
  l: TLocalList;
  itm:  TTestObject;
begin
  VariationNAme := 'Add [m i s s i s s i p p i]'+NEWLINE+
                   'then remove all '+sRemove.Tag;
  l := TLocalList.create;
  try

    AddTestSet(l);
    while l.Remove(sRemove) >=0 do ;
    result := DebugListSTate(l);

  finally
    l.free;
  end;

end;


{ TTestObject }

constructor TTestObject.Create(sTag: string);
begin
  tag := sTag;
end;

function TTestObject.Debug: string;
begin
  result := {inttohex(nativeint(pointer(self)), sizeof(nativeint) * 2)+' '+}Tag
end;

initialization
  UTF.RegisterClass(TUT_List);



end.

