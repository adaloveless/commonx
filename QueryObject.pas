unit QueryObject;

interface
uses Dataobject, Dataobjectfactory;

type
  TdoQuery = class(TDataObject)
  public
    constructor create(AOwner: Tobject; params: variant; ACache: TObject; extended: TExtendedDOVars); override;
  end;

implementation

{ TdoTime }

constructor TdoQuery.create(AOwner: Tobject; params: variant;
  ACache: TObject; extended: TExtendedDOVars);
begin
  inherited;
  //field definitions are dynamic for this object


end;

initialization


end.
