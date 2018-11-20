unit ObservableObject;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, betterobject, generics.collections.fixed;

type
  TObservableObject = class;//forward

  ISimpleObserver = interface(IUnknown)
    procedure ObserverDetach(obj: TObservableObject);
  end;

  TObservableObject = class(TBetterObject)
  strict private
    FObservers: TList<ISimpleObserver>;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Detach;override;
    procedure AddObserver(obj: ISimpleObserver);
    procedure RemoveObserver(obj: ISimpleObserver);
  end;




implementation

{ TObservableObject }

procedure TObservableObject.AddObserver(obj: ISimpleObserver);
begin
  FObservers.Add(obj);
end;

constructor TObservableObject.Create;
begin
  inherited;
  FObservers := TList<ISimpleObserver>.create;
end;

destructor TObservableObject.Destroy;
begin

  inherited;
  FObservers.free;
end;

procedure TObservableObject.Detach;
begin
  inherited;
  while FObservers.Count > 0 do begin
    removeobserver(FObservers[FObservers.Count-1]);
  end;
end;

procedure TObservableObject.RemoveObserver(obj: ISimpleObserver);
begin
  obj.ObserverDetach(self);
  FObservers.Remove(obj);


end;


end.

