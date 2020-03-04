unit ConsoleTools;

interface

uses
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  commandprocessor, stringx, typex, systemx, sysutils, betterobject;

type
  TConsole = class(TBetterObject)
  private
{$IFDEF MSWINDOWS}
    FHandle: THandle;
{$ENDIF}
    FForeGroundColorNumber: nativeint;
    FBAckGroundColorNumber: nativeint;
    procedure SetBackGroundColorNumber(const Value: nativeint);
    procedure SetForeGroundColorNumber(const Value: nativeint);
  protected

  public
{$IFDEF MSWINDOWS}
    property Handle: THandle read FHandle;
{$ENDIF}
    procedure Init;override;
    procedure SetTextAttribute(iAttr: nativeint);
    procedure Write(sString: string);
    property ForeGroundColorNumber: nativeint read FForeGroundColorNumber write SetForeGroundColorNumber;
    property BackGroundColorNumber: nativeint read FBAckGroundColorNumber write SetBackGroundColorNumber;
  end;

procedure WatchCommandInConsole(c: TCommand);
procedure ClearScreen;

var
  con: TConsole;


implementation

function TextBar(pct: single): string;
begin
  result := '..........';
  for var t := 0 to (round((pct*100)/10))-1 do begin
    result[t+low(result)] := '#';
  end;
  result := '['+result+']';

end;

procedure WatchCommandInConsole(c: TCommand);
var
  s: string;
  iPrevWriteSize: ni;
begin
  iPrevWriteSize := 0;
  while not c.WaitFor(250) do begin
    if assigned(c.Thread) then begin
      c.Lock;
      try
        s := zcopy(c.Thread.GetSignalDebug+' '+floatprecision(c.PercentComplete*100,0)+'% '+TextBar(c.PercentComplete)+c.Status,0,70)+CR;
        Write(stringx.StringRepeat(' ', iPrevWriteSize)+#13);
        Write(s);
        iPrevWriteSize := length(s);
      finally
        c.unlock;
      end;

    end;
  end;
  Write(stringx.StringRepeat(' ', iPrevWriteSize)+#13);
  s := floatprecision(c.PercentComplete*100,0)+'% '+ zcopy(c.Status,0,70)+CR;
  Write(s);




end;

procedure TConsole.Init;
begin
  inherited;
{$IFDEF MSWINDOWS}
  Fhandle := GetStdHandle(STD_OUTPUT_HANDLE);
{$ENDIF}
end;

procedure TConsole.SetBackGroundColorNumber(const Value: nativeint);
begin
{$IFDEF CONSOLE}
  FBAckGroundColorNumber := Value;
{$IFDEF MSWINDOWS}
  SetConsoleTextAttribute(handle, ((FBAckGroundColorNumber and $f) shl 4) or (FForeGroundColorNumber and $f));
{$ENDIF}
{$ENDIF}
end;


procedure TConsole.SetForeGroundColorNumber(const Value: nativeint);
begin
{$IFDEF CONSOLE}
  FForeGroundColorNumber := Value;
{$IFDEF MSWINDOWS}
  SetConsoleTextAttribute(handle, ((FBAckGroundColorNumber and $f) shl 4) or (FForeGroundColorNumber and $f));
{$ENDIF}
{$ENDIF}
end;

procedure TConsole.SetTextAttribute(iAttr: nativeint);
begin
{$IFDEF CONSOLE}
{$IFDEF MSWINDOWS}
  windows.SetConsoleTextAttribute(handle, iAttr);
{$ENDIF}
{$ENDIF}
end;

procedure TConsole.Write(sString: string);
begin
{$IFDEF CONSOLE}
  Writeln(sString);
{$ENDIF}
end;

procedure ClearScreen;
{$IFNDEF MSWINDOWS}
begin
end;
{$ELSE}
var
  stdout: THandle;
  csbi: TConsoleScreenBufferInfo;
  ConsoleSize: DWORD;
  NumWritten: DWORD;
  Origin: TCoord;
begin
  stdout := GetStdHandle(STD_OUTPUT_HANDLE);
  Win32Check(stdout<>INVALID_HANDLE_VALUE);
  Win32Check(GetConsoleScreenBufferInfo(stdout, csbi));
  ConsoleSize := csbi.dwSize.X * csbi.dwSize.Y;
  Origin.X := 0;
  Origin.Y := 0;
  Win32Check(FillConsoleOutputCharacter(stdout, ' ', ConsoleSize, Origin,
    NumWritten));
  Win32Check(FillConsoleOutputAttribute(stdout, csbi.wAttributes, ConsoleSize, Origin,
    NumWritten));
  Win32Check(SetConsoleCursorPosition(stdout, Origin));
end;
{$ENDIF}

end.
