unit DBJoinDictionary;

interface



uses
  namevaluepair, betterobject, sysutils;


type
  TDBJoinDictionary = class(TSharedobject)
  protected
    nvp: TNameValuePairList;
  public
    procedure RegisterJoin(db, t1, t2, clause: string);
    function FindJoin(db, t1, t2: string): string;
    function Joinclause(db, t1,t2: string): string;
    constructor Create; override;
    destructor Destroy; override;
  end;



implementation

{ TDBJoinDictionary }

constructor TDBJoinDictionary.Create;
begin
  inherited;
  nvp := TNameValuePairList.create;
end;

destructor TDBJoinDictionary.Destroy;
begin
  nvp.free;
  inherited;
end;

function TDBJoinDictionary.FindJoin(db, t1, t2: string): string;
begin
  var l : ILock := Self.LockI;
  result := nvp.GetItemEx(db+'::'+t1+'::'+t2, '');
end;

function TDBJoinDictionary.Joinclause(db, t1, t2: string): string;
begin
  result := FindJoin(db,t1,t2);
  result := stringreplace(result, '#t1#', t1, [rfIgnoreCase, rfReplaceall]);
  result := stringreplace(result, '#t2#', t2, [rfIgnoreCase, rfReplaceall]);
end;

procedure TDBJoinDictionary.RegisterJoin(db, t1, t2, clause: string);
begin
  var l : ILock := Self.LockI;
  nvp.autoadd := true;
  nvp.items[db+'::'+t1+'::'+t2].Value := clause;
end;

end.
