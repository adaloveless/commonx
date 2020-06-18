unit ConsoleTools;

interface

uses
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  commandprocessor, stringx, typex, systemx, sysutils, betterobject;

const
  CC_RED = 12;
  CC_YELLOW = 14;
  CC_WHITE = 15;
  CC_SILVER = 7;
  CC_GREY = 8;
  CC_BLUE = 9;
  CC_GREEN = 10;
  NLR = '`cF`'+CRLF;

type
  TConsole = class(TBetterObject)
  private
{$IFDEF MSWINDOWS}
    FHandle: THandle;
{$ENDIF}
    twiddleframe: ni;
    FForeGroundColorNumber: nativeint;
    FBAckGroundColorNumber: nativeint;
    procedure SetBackGroundColorNumber(const Value: nativeint);
    procedure SetForeGroundColorNumber(const Value: nativeint);
  protected
                public
    AutoCRLF: boolean;
{$IFDEF MSWINDOWS}
    property Handle: THandle read FHandle;
{$ENDIF}
    procedure Init;override;
    procedure SetTextAttribute(iAttr: nativeint);
    procedure Write(sString: string);
    procedure WriteLn(sString: string);
    procedure WhiteSpace(linecount: ni = 4);
    procedure Append(sString: string);
    property ForeGroundColorNumber: nativeint read FForeGroundColorNumber write SetForeGroundColorNumber;
    property BackGroundColorNumber: nativeint read FBAckGroundColorNumber write SetBackGroundColorNumber;
    procedure Twiddle;
    procedure WriteEx(sExpression: string; sEsc: char = '`');
    procedure WriteOk(bOk: boolean);
    procedure WatchCommand(c: TCommand);
  end;

procedure WatchCommandInConsole(c: TCommand);
procedure ClearScreen;



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

procedure TConsole.Append(sString: string);
begin
{$IFDEF CONSOLE}
  system.Write(sString);
{$ENDIF}
end;

procedure TConsole.Init;
begin
  inherited;
{$IFDEF MSWINDOWS}
  Fhandle := GetStdHandle(STD_OUTPUT_HANDLE);
{$ENDIF}
  AutoCRLF := true;
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

procedure TConsole.Twiddle;
begin
  twiddleframe := (twiddleframe+1) and 15;
  case twiddleframe of
    0: WriteEx('`cE`[`cF`*`c7`*`c8`*      `cE`]'+CR);
    1: WriteEx('`cE`[`c7`*`cF`*       `cE`]'+CR);
    2: WriteEx('`cE`[`c8`*`c7`*`cF`*      `cE`]'+CR);
    3: WriteEx('`cE`[ `c8`*`c7`*`cF`*     `cE`]'+CR);
    4: WriteEx('`cE`[  `c8`*`c7`*`cF`*    `cE`]'+CR);
    5: WriteEx('`cE`[   `c8`*`c7`*`cF`*   `cE`]'+CR);
    6: WriteEx('`cE`[    `c8`*`c7`*`cF`*  `cE`]'+CR);
    7: WriteEx('`cE`[     `c8`*`c7`*`cF`* `cE`]'+CR);
    8: WriteEx('`cE`[      `c8`*`c7`*`cF`*`cE`]'+CR);
    9: WriteEx('`cE`[       `cF`*`c7`*`cE`]'+CR);
   10: WriteEx('`cE`[      `cF`*`c7`*`c8`*`cE`]'+CR);
   11: WriteEx('`cE`[     `cF`*`c7`*`c8`* `cE`]'+CR);
   12: WriteEx('`cE`[    `cF`*`c7`*`c8`*  `cE`]'+CR);
   13: WriteEx('`cE`[   `cF`*`c7`*`c8`*   `cE`]'+CR);
   14: WriteEx('`cE`[  `cF`*`c7`*`c8`*    `cE`]'+CR);
   15: WriteEx('`cE`[ `cF`*`c7`*`c8`*     `cE`]'+CR);
  end;



end;

procedure TConsole.WatchCommand(c: TCommand);
begin
  WAtchCommandInConsole(c);
end;

procedure TConsole.WhiteSpace(linecount: ni);
begin
  for var t := 1 to linecount do
    WriteEx(NLR);
end;

procedure TConsole.Write(sString: string);
begin
{$IFDEF CONSOLE}
  if AutoCRLF then
    system.Writeln(sString)
  else
    system.Write(sString);

{$ENDIF}
end;

procedure TConsole.WriteEx(sExpression: string; sEsc: char = '`');
begin
  var slh := ParseStringH(sExpression, sESC);
  for var t := 0 to slh.o.count-1 do begin
    var sSection: string := slh.o[t];
    if (t and 1) = 0 then begin
      Append(slh.o[t]);
    end else begin
      if length(sSection) < 1 then
        Append(sEsc)
      else begin
        var cmd := lowercase(sSection[low(sSection)]);
        if cmd = 'c' then begin
          var clr := lowercase(sSection[high(sSection)]);
          var iclr := strtoint('$'+clr);
          SetTextAttribute(iclr);
        end;
        if cmd = 'n' then begin
          Append('CRLF');
        end;
      end;
    end;
  end;
end;

procedure TConsole.WriteLn(sString: string);
begin
{$IFDEF CONSOLE}
  system.Writeln(sSTring);
{$ENDIF}
end;

procedure TConsole.WriteOk(bOK: boolean);
begin
  if bOK then
    WriteEx('`cA`Ok!`cF`'+CRLF)
  else
    WriteEx('`cC`FAIL!`cF`'+CRLF);

end;

procedure ClearScreen;
{$IFNDEF MSWINDOWS}
begin
  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
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
