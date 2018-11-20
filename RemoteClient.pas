unit RemoteClient;
{x$INLINE AUTO}
interface

uses sockets, graphics, sysutils, scktcomp, classes;


type
  TRemoteClient = class
  public
    Socket: TCustomIPClient;

    constructor create;reintroduce; virtual;
    destructor destroy; override;

    procedure Connect(sHost, sPort: ansistring);
    procedure MouseMove(x,y: integer);
    procedure MouseClick(x,y: integer);
    procedure MouseDown(x,y: integer);
    procedure MouseUp(x,y: integer);
    procedure ScreenShot(bmScreen: TBitmap; iQuad: integer = 0);
    procedure Exit;
    procedure Lock;
    procedure Unlock;


  end;

function LesserOf(i1, i2: integer): integer;

implementation


function LesserOf(i1, i2: integer): integer;
begin
  if i1 < i2 then
    result := i1
  else
    result := i2;
end;


{ TRemoteClient }

procedure TRemoteClient.Connect(sHost, sPort: ansistring);
begin
  socket.RemoteHost := sHost;
  socket.RemotePOrt := sPort;
  socket.Connect;
end;

constructor TRemoteClient.create;
begin
  socket := TCustomIPClient.create(nil);
end;

destructor TRemoteClient.destroy;
begin
  socket.free;
  inherited;
end;

procedure TRemoteClient.Exit;
begin
  socket.Sendln('exit');
  socket.Close;

end;

procedure TRemoteClient.Lock;
begin
  socket.Sendln('lock');

end;

procedure TRemoteClient.MouseClick(x, y: integer);
begin
  socket.Sendln('mc '+inttostr(x)+'|'+inttostr(y));
end;

procedure TRemoteClient.MouseDown(x, y: integer);
begin
  socket.Sendln('md '+inttostr(x)+'|'+inttostr(y));
end;

procedure TRemoteClient.MouseMove(x, y: integer);
begin
  socket.Sendln('mm '+inttostr(x)+'|'+inttostr(y));
end;

procedure TRemoteClient.MouseUp(x, y: integer);
begin
  socket.Sendln('mu '+inttostr(x)+'|'+inttostr(y));
end;

procedure TRemoteClient.ScreenShot(bmScreen: TBitmap; iQuad: integer = 0);
var
  ms: TMemoryStream;
  iTotalLength: integer;
  buffer: array[0..32768] of AnsiChar;
  iLength, iReceived: integer;
begin
  ms := TMemoryStream.create;
  try

    socket.Sendln('ss '+inttostr(iQuad));
    iTotalLength := strtoint(socket.Receiveln());
    ms.Seek(0,0);

    iReceived := 0;
    repeat
      iLength := socket.ReceiveBuf(buffer, LesserOf(32768, iTotalLength-iReceived));
      inc(iReceived, iLength);
      ms.WriteBuffer(buffer, iLength);
    until iReceived >= iTotalLength;

    ms.Seek(0,0);

    bmScreen.LoadFromStream(ms);


  finally
    ms.free;
  end;


end;

procedure TRemoteClient.Unlock;
begin
  socket.Sendln('unlock');
end;

end.
