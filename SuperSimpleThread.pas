unit SuperSimpleThread;

interface

uses
  debug, classes;


type
  TThreadTest = class(TThread)
  protected
    procedure Execute; override;
  end;
var
  test: TThreadTest;

implementation



{ TThreadTest }

procedure TThreadTest.Execute;
begin
  inherited;
  Debug.Log(self, 'SUPER SIMPLE THREAD TEST <-------------------------------------');
end;

initialization
begin
  test := TThreadTest.Create(true);
  test.FreeOnTerminate := true;
  test.resume;


end;


finalization
  test := nil;

end.
