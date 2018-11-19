unit BetterObjectFactory;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, betterobject, generics.collections.fixed;

type
  TBetterObjectFactory = class(TBetterObject)
  private
    FClasses: TList<TBetterObjectClass>;
    function GetCount: nativeint;
    function GetClass(idx: nativeint): TBetterObjectClass;
    function GEtClassByName(sName: string): TBetterObjectClass;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure RegisterClass(fxClass: TBetterObjectClass);
    function New(sName: string): TBetterObject;
    property Count: nativeint read GetCount;
    property Classes[idx: nativeint]: TBetterObjectClass read GetClass;
    property ClassesByName[sName: string]: TBetterObjectClass read GEtClassByName;
  end;


implementation


{ TBetterObjectFactory }

constructor TBetterObjectFactory.Create;
begin
  inherited;
  Fclasses := TList<TBetterObjectClass>.create;
end;

destructor TBetterObjectFactory.Destroy;
begin
  FClasses.free;
  inherited;
end;

function TBetterObjectFactory.GetClass(idx: nativeint): TBetterObjectClass;
begin
  result := FClasses[idx];
end;

function TBetterObjectFactory.GEtClassByName(sName: string): TBetterObjectClass;
var
  t: integer;
begin
  result := nil;

  sName := lowercase(sName);
  for t:= 0 to Fclasses.count-1 do begin
    if lowercase(Fclasses[t].ClassName) = sName then begin
      result := FClasses[t];
      break;
    end;
  end;

end;

function TBetterObjectFactory.GetCount: nativeint;
begin
//  Lock;
  try
    result := FClasses.Count;
  finally
//    Unlock;
  end;

end;

function TBetterObjectFactory.New(sName: string): TBetterObject;
var
  t: integer;
begin
  result := nil;
  sName := lowercase(sName);
  for t:= 0 to Fclasses.count-1 do begin
    if lowercase(Fclasses[t].ClassName) = sName then begin
      result := FClasses[t].create;
      break;
    end;
  end;

end;

procedure TBetterObjectFactory.RegisterClass(fxClass: TBetterObjectClass);
begin
  FClasses.Add(fxClass);

end;


end.

