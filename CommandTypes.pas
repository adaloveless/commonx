unit CommandTypes;

interface

uses
  commandprocessor;

type
  TFunctionCommand<T> = class(TCommand)
  private
    FResult: T;
    procedure SetREsult(const Value: T);
  public
    property Result: T read FResult write SetREsult;
  end;

implementation


{ TFunctionCommand<T> }

{ TFunctionCommand<T> }

procedure TFunctionCommand<T>.SetREsult(const Value: T);
begin
  FprocessLater := false;
  FResult := Value;
end;

end.
