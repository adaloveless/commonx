unit DestructionQueue;

interface
uses
  debug, managedthread, generics.collections.fixed, classes, sysutils, better_collections, betterobject, orderlyinit;

type
  EDestructionQueueError = class(Exception);

  TDestructionQueue = class(TManagedThread)
  protected
    FList: TSharedList<TObject>;
    FIncoming: TSharedList<TObject>;
  public
    procedure Init;override;
    procedure Detach;override;
    procedure Add(o: TObject);
    procedure DoExecute;override;


  end;

var
  dq: TDestructionQueue;

implementation

{ TDestructionQueue }

procedure TDestructionQueue.Add(o: TObject);
begin
  if o = nil then begin
    Debug.Log(self,'Why are you adding a nil object to the DQ?');
    exit;
  end;

  FIncoming.Lock;
  try
    FIncoming.add(o);
    HAsWork := true;//NOTE: HASWORK should always be evaluated under the FINcoming lock;
  finally
    FIncoming.Unlock;
  end;
end;

procedure TDestructionQueue.Detach;
begin
  inherited;
  if FList.count> 0 then
    raise EDestructionQueueError.create('Cannot detach the destruction queue until its objects are destroyed.');
  FList.Free;
  FIncoming.free;


end;

procedure TDestructionQueue.DoExecute;
var
  o: TObject;
  t: nativeint;
begin
  o := nil;
  if (FList.Count > 0) then begin
    o := FList[0];
    FList.Delete(0);
  end else begin
    FIncoming.Lock;
    try
      for t:= 0 to FIncoming.count-1 do begin
        fList.add(FIncoming[t]);
      end;
      FIncoming.Clear;
      HasWork := FLIst.count > 0;//NOTE: HASWORK should always be evaluated under the FINcoming lock;
    finally
      Fincoming.unlock;
    end;
  end;

  if o is TBetterObject then
    TBetterObject(o).Detach;

  o.free;
end;

procedure TDestructionQueue.Init;
begin
  inherited;
  FIncoming := TSharedList<TObject>.create;
  FList := TSharedList<TObject>.create;
  Loop := true;

end;

procedure oinit;
begin
  dq := TPM.NeedThread<TDestructionQueue>(nil);
  dq.BeginStart;
//  dq.EndStart;
//  dq.Start;
end;

procedure ofinal;
begin
  dq.EndStart;
  dq.Stop;
  dq.SafeWaitFor;
  TPM.NoNeedthread(dq);
  dq := nil;
end;

procedure oLATEfinal;
begin
  //dq.free;
  dq := nil;
end;

initialization
  orderlyinit.init.RegisterProcs('DestructionQueue',oinit, nil, ofinal, oLATEfinal, 'BackgroundThreads,ManagedThread');




end.


