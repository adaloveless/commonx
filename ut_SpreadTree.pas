unit UT_SpreadTree;

interface


uses
  unittest, typex, systemx, sysutils, spreadtree, stringx;

const
  tests: array of string = [
    'not used',

    //most basic
    'FM"Doc1":{ "A": 1, "B": "=A" }',

    //found in parent scope
    'FM"Doc1":{ "A": 2, "SubObj": {"B": "=A"} }',

    //abiguous scopes, should find local scope
    'FM"Doc1":{ "A": 3, "SubObj": {"B": "=A"} }'+CRLF+
    '"Doc2":{ "A": 4, "SubObj": {"B": "=A"} }',

    //found in different documents without explicit scope
    'FM"Doc1":{ "A1": 5, "SubObj": {"B1": "=A2"} }'+CRLF+
    '"Doc2":{ "A2": 6, "SubObj": {"B2": "=A1"} }',

    //explicit scope resolution, cross documents
    'FM"Doc1":{ "A": 7, "SubObj": {"B": "=Doc2.A"} }'+CRLF+
    '"Doc2":{ "A": 8, "SubObj": {"B": "=Doc1.A"} }',

    //Complex result
    'FM"Doc1":{ "A": 7, "SubObj": {"B": "=Doc2.A"} }'+CRLF+
    '"Doc2":{ "A": 8, "SubObj": "=Doc1.SubObj"}',

    //basic operations on integers
    'FM"Doc1":{ "A": "=5+2"}'+CRLF+
    '"Doc2":{ "A": "=5-2"}'+CRLF+
    '"Doc3":{ "A": "=5*2"}'+CRLF+
    '"Doc4":{ "A": "=5/2"}',

    //basic operations on integers
    'FM"Doc1":{ "A": "=5+2+1"}'+CRLF+
    '"Doc2":{ "A": "=5-2-1"}'+CRLF+
    '"Doc3":{ "A": "=5*2*3"}'+CRLF+
    '"Doc4":{ "A": "=5/2/3"}',

    //basic operations on floats
    'FM"Doc1":{ "A": "=5.1+2.3"}'+CRLF+
    '"Doc2":{ "A": "=5.1-2.3"}'+CRLF+
    '"Doc3":{ "A": "=5.1*2.3"}'+CRLF+
    '"Doc4":{ "A": "=5.1/2.3"}',

    //negative cases
    'FM"Doc1":{ "A": "=-1*2.3"}'+CRLF+
    '"Doc2":{ "A": "=-1.0*-2.3"}',

    'FM'+
    '"Doc1":{ "A": 3, "B": 7, "C": "=A+B" }',

    'FM'+
    '"Doc1":{ "C": "=2+3" }'+CRLF+
    '"Doc2":{ "A": "=C"}',

    'FM'+
    '"Doc1":{ "A": 3, "B": 7, "C": "=A+B" }'+CRLF+
    '"Doc2":{ "A": "=C"}'




  ];

type
  TUT_SpreadTree = class(TUnitTest)
  protected
    procedure DoExecute; override;

  end;



implementation

{ TUT_SpreadTree }

procedure TUT_SpreadTree.DoExecute;
begin
  inherited;

  if variation < length(tests) THEN BEGIN
    var st := TSpreadTree.create;
    try
      st.FromQuickSave_Code(tests[variation]);
      VariationName := tests[variation];
      st.Solve;
      self.utresult := st.ToQuickSave_Results;

    finally
      st.free;
    end;


  end;




end;

initialization
  UTF.RegisterClass(TUT_SpreadTree);

end.
