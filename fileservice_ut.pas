unit fileservice_ut;

interface

uses
  typex, systemx, commandprocessor, fileserviceclientex, classes, helpers_stream, debug, sysutils, beeper;

type
  Tcmd_UTFileServiceClient = class(TCommand)

  public
    procedure DoExecute; override;
    procedure InitExpense; override;
  end;

implementation

{ Tcmd_UTFileServiceClient }

procedure Tcmd_UTFileServiceClient.DoExecute;
var
  cli: TFileServiceClientEx;
  strOut: TStream;
  strIn: TMEmoryStream;
  strInCopy: TMEmoryStream;
  cs1, cs2: int64;
  p: pbyte;
  cx: ni;
begin
  inherited;
  strOut := nil;
  strInCopy := nil;
    Debug.Log('******************** GO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
  //cli := TFileServiceClientEx.create('192.168.101.35', '420');
  cli := TFileServiceClientEx.create('ip2.digitaltundra.com', '420');
  try
    strIn := TMemorySTream.create;
    strIn.Size := 33*million;
    cx := strIn.Size;
    p := strIn.Memory;
    while cx > 0 do begin
      p^ := random(255);
      inc(p);
      dec(cx);
    end;


    strInCopy := TMemoryStream.create;
    strIn.Seek(0,soBeginning);
    stream_GuaranteeCopy(strIn, strInCopy, strIn.Size);
    strIn.seek(0, soBeginning);
    strOut := cli.EchoStream(strIn);{strin is detroyed by this operation}

    Debug.Log('********************'+inttostr(stream_Compare(strInCopy, strOut)));
    beeper.Beep(1000,100);


  finally
    strOut.free;
    strInCopy.free;
    cli.free;
  end;


end;

procedure Tcmd_UTFileServiceClient.InitExpense;
begin
  inherited;
  CPUExpense := 0.0;
end;

end.
