unit UT_SpreadTree;

interface


uses
  unittest, typex, systemx, sysutils, spreadtree;

const
  tests: array of string = [
    'not used',
    'FMAss:{ "Fart": 2, "MoreFarts": "=Fart" }'
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
      Self.UTResult := st.GetResultsJSON;

    finally
      st.free;
    end;


  end;




end;

initialization
  UTF.RegisterClass(TUT_SpreadTree);

end.
