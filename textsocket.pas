unit TextSocket;

interface

uses
  systemx, ScktComp, betterobject, sharedobject, classes, sysutils, sockfix;

type
  TTextSocket = class (TSharedObject)
  private
    Incoming1: array[0..9000] of AnsiChar;
    Incoming2: array of AnsiChar;
    FSocket: TCustomIPclient;
    FTerminated: boolean;
    function Getsocket: TCustomIPClient;
    procedure SetSocket(const Value: TCustomIPClient);
  public
    constructor create; override;
    destructor destroy; override;
    property socket : TCustomIPClient read Getsocket write SetSocket;
    function BufferIncoming: boolean;
    function ReadLine: ansistring;
    procedure SendText(sText: ansistring);
    property Terminated: boolean read FTerminated write FTerminated;
  end;


implementation

{ TTextSocket }


{ TTextSocket }

function TTextSocket.BufferIncoming: boolean;
var
  iLength, iLength2: integer;
  pc: PAnsiChar;

begin
  result := false;
  //STAGE 1
  //Pull data off the socket into Incoming1, the static buffer
  if socket = nil then
    raise Exception.create('Socket Dropped');

  if socket.WaitforData(1) then begin
    result := true;
    iLength := socket.ReceiveBuf(Incoming1, 9000);

    iLength2 := length(Incoming2);

    //if we have data left over from before
    if iLength2>0 then begin
      //grow the layer2 buffer (the dynamic one) to be big enough to hold all the shit
      SetLength(Incoming2, iLength2+iLength);
      //set pc to point to the beginning of the NEW section of the grown buffer
      pc := @Incoming2[iLength2];
    end
    //otherwise
    else begin
      //grow the layer2 buffer to be big enough for what we just read
      SetLength(Incoming2, iLength2+iLength);
      //set pc to the beginning of the dynamic buffer
      pc := @Incoming2[0];
    end;

    //move memory from the static to the dynamic buffer
    MoveMem32(@Incoming1, pc, iLength);
  end;
end;

constructor TTextSocket.create;
begin
  inherited;
  FSocket := TCustomIPClient.create(nil);
end;

destructor TTextSocket.destroy;
begin
  Fsocket.free;
  inherited;
end;

function TTextSocket.Getsocket: TCustomIPClient;
begin
  Lock;
  try
    result := FSocket;
  finally
    Unlock;
  end;

end;

function TTextSocket.ReadLine: ansistring;
var
  iLength, iLength2: integer;
  t, u: integer;
begin
  if socket = nil then
    socket := FSocket;

  try
    result := '';

    while (result = '') and (not Terminated) do begin

      //call this to move stuff off the socket into Incoming2, a dynamic buffer of unread data
      BufferIncoming;


      iLength := length(Incoming2);
      for t:= Low(Incoming2) to (iLength-1)-Low(Incoming2) do begin
        if ((Incoming2[t] = #13) and (Incoming2[t+1] = #10))
        or (Incoming2[t] = #10) then begin
          setlength(result, t+1+(1-Low(Incoming2)));
          for u:= Low(Incoming2) to t+1 do begin
            result[u+(1-Low(Incoming2))] := Incoming2[u];
          end;
          (*for u:= t+2 to length(Incoming2) do begin
            Incoming2[u-(t+2)] := Incoming2[u];
          end;
          SetLength(Incoming2, length(Incoming2)-t+2);*)
          Incoming2 := copy(Incoming2, t+2+Low(Incoming2), length(Incoming2)-t+2);
          break;
        end;
      end;
    end;
  except
    Terminated := true;
  end;
end;

procedure TTextSocket.SendText(sText: ansistring);
begin
  FSocket.Sendln(sText);
end;

procedure TTextSocket.SetSocket(const Value: TCustomIPClient);
begin
  Lock;
  try
    FSocket := value;
  finally
    Unlock;
  end;
end;

end.
