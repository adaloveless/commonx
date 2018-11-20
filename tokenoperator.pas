unit tokenoperator;

interface

uses
  typex, systemx, stringx, betterobject, classes;

type
  TTokenOperator = class(TBetterObject)
  public
    token: string;
    op: string;
    sublist: pointer;
  end;

  TokenOperatorList = class(TBetterObject)
  public
    operatorlist: TStringList;
    tokens: TStringList;
    tokenOperators: TStringList;
    procedure Init;override;
    procedure Detach;override;
    procedure Parse(s: string);
  end;


implementation

{ TokenOperatorList }

procedure TokenOperatorList.Detach;
begin
  if detached then exit;
  operatorlist.free;
  tokens.free;
  tokenoperators.free;
  operatorlist := nil;
  tokens := nil;
  tokenoperators := nil;
  inherited;
end;

procedure TokenOperatorList.Init;
begin
  inherited;
  operatorlist := TStringlist.create;
  tokens:= TStringList.create;
  tokenoperators := TStringLIst.create;


end;

procedure TokenOperatorList.Parse(s: string);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

end.
