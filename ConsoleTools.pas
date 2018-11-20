unit ConsoleTools;

interface

uses
  commandprocessor, stringx, typex, systemx, sysutils, betterobject, windows;

type
  TConsole = class(TBetterObject)
  private
    FHandle: THandle;
    FForeGroundColorNumber: nativeint;
    FBAckGroundColorNumber: nativeint;
    procedure SetBackGroundColorNumber(const Value: nativeint);
    procedure SetForeGroundColorNumber(const Value: nativeint);
  protected

  public
    property Handle: THandle read FHandle;
    procedure Init;override;
    procedure SetTextAttribute(iAttr: nativeint);
    procedure Write(sString: string);
    property ForeGroundColorNumber: nativeint read FForeGroundColorNumber write SetForeGroundColorNumber;
    property BackGroundColorNumber: nativeint read FBAckGroundColorNumber write SetBackGroundColorNumber;
  end;

procedure WatchCommandInConsole(c: TCommand);

var
  con: TConsole;


implementation


procedure WatchCommandInConsole(c: TCommand);
var
  s: string;
begin
  while not c.WaitFor(250) do begin
    if assigned(c.Thread) then begin
      c.Lock;
      try
        s := zcopy(c.Thread.GetSignalDebug+' '+floatprecision(c.PercentComplete*100,0)+'% '+c.Status,0,70)+CR;
        Write(s);
      finally
        c.unlock;
      end;

    end;
  end;

  s := floatprecision(c.PercentComplete*100,0)+'% '+ zcopy(c.Status,0,70)+CR;
  Write(s);




end;

procedure TConsole.Init;
begin
  inherited;
  Fhandle := GetStdHandle(STD_OUTPUT_HANDLE);
end;

procedure TConsole.SetBackGroundColorNumber(const Value: nativeint);
begin
{$IFDEF CONSOLE}
  FBAckGroundColorNumber := Value;
  SetConsoleTextAttribute(handle, ((FBAckGroundColorNumber and $f) shl 4) or (FForeGroundColorNumber and $f));
{$ENDIF}
end;


procedure TConsole.SetForeGroundColorNumber(const Value: nativeint);
begin
{$IFDEF CONSOLE}
  FForeGroundColorNumber := Value;
  SetConsoleTextAttribute(handle, ((FBAckGroundColorNumber and $f) shl 4) or (FForeGroundColorNumber and $f));
{$ENDIF}
end;

procedure TConsole.SetTextAttribute(iAttr: nativeint);
begin
{$IFDEF CONSOLE}
  windows.SetConsoleTextAttribute(handle, iAttr);
{$ENDIF}
end;

procedure TConsole.Write(sString: string);
begin
{$IFDEF CONSOLE}
  Writeln(sString);
{$ENDIF}
end;

end.
