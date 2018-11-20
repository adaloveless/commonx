unit StackTrace;

interface

uses
  typex, systemx, managedthread, sharedobject, betterobject, classes;


type
  TStackTrace = class(TSharedObject)
  protected
    FList: TStringlist;
  public
    procedure Init;override;
    procedure Detach; override;
    procedure Push(scontext: string);
    procedure Pop;
  end;

procedure InitStackTrace;
procedure CleanupStackTrace;


threadvar
  stacktracer: TStackTrace;

implementation

{ TStackTrace }

procedure InitStackTrace;
begin
  stacktracer := TSTackTRace.create;
end;

procedure CleanupStackTrace;
begin
  stacktracer.free;
  stacktracer := nil;
end;



procedure TStackTrace.Detach;
begin
  inherited;
  FList.free;
end;

procedure TStackTrace.Init;
begin
  inherited;
  FList := TStringlist.create;

end;

procedure TStackTrace.Pop;
begin
  lock;
  try
    FList.delete(FList.count-1);
  finally
    unlock;
  end;

end;

procedure TStackTrace.Push(scontext: string);
begin
  lock;
  try
    FList.add(sContext);
  finally
    unlock;
  end;

end;

end.
