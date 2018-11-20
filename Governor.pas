unit Governor;

interface


uses
  systemx, stringx, orderlyinit, sharedobject, generics.collections;


type
  TAmbassadorRecord = record
    COmputerName: string;
    Application: string;
    POrt: int64;
  end;

  TAmbassadorObject = class(TSharedObject)
  public
    rec: TAmbassadorREcord;
  end;

  TGovernor = class(TSharedObject)
  strict private
    FList: TList<TAmbassadorObject>;
  public
    procedure Init;override;
    procedure Detach;override;
    procedure Add(ComputerName: string; ApplicationNAme: string; port:int64);

  end;



var
  G_Governor: TGovernor;

implementation

procedure oinit;
begin
  G_Governor := TGovernor.create;
end;

procedure ofinal;
begin
  G_Governor.free;
  G_Governor := nil;
end;

{ TGovernor }

procedure TGovernor.Add(ComputerName, ApplicationNAme: string; port: int64);
var
  ao: TAmbassadorObject;
begin
  Lock;
  try
    ao := TAmbassadorObject.create;
    ao.rec.COmputerName := COmputerName;
    ao.rec.Application := ApplicationName;
    ao.rec.POrt := port;
    FList.add(ao);
  finally
    Unlock;
  end;
end;

procedure TGovernor.Detach;
var
  ao: TAmbassadorObject;
begin
  if DEtached then
    exit;

  Lock;
  try
    while FList.Count > 0 do begin
      ao := FList[0];
      FList.Delete(0);
      ao.free;
      ao := nil;
    end;
  finally
    Unlock;
  end;
  inherited;

end;

procedure TGovernor.Init;
begin
  inherited;
  FList := TList<TAmbassadorObject>.create;


end;

initialization

orderlyinit.init.RegisterProcs('Governor',  oinit, ofinal, 'ManagedThread,RDTPServerList');




end.
