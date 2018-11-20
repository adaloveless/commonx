unit comport;

interface

uses
  sysutils, windows;

type
  TComPort = class
  public
    procedure Open(pipe: ansistring);
    procedure Close;
    procedure Write(s: ansistring);
    function Read(len: integer; out s: ansistring): integer;

  end;



implementation






{ TComPort }

procedure TComPort.Write(s: ansistring);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TComPort.Open(pipe: ansistring);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure TComPort.Close;
begin

//TODO -cunimplemented: unimplemented block
end;



function TComPort.Read(len: integer; out s: ansistring): integer;
begin

//TODO -cunimplemented: unimplemented block
end;

end.
