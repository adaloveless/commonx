unit FileLauncher;

interface

uses
{$IFDEF MSWINDOWS}
Winapi.ShellAPI, Winapi.Windows;
{$ENDIF MSWINDOWS}
{$IFDEF MACOS}
Posix.Stdlib;
{$ENDIF MACOS}

type
TFileLauncher = class
  class procedure Open(const FilePath: string);
end;

implementation

class procedure TFileLauncher.Open(const FilePath: string);
begin
{$IFDEF MSWINDOWS}
ShellExecute(0, 'OPEN', PChar(FilePath), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}

{$IFDEF MACOS}
_system(PAnsiChar('open '+'"'+AnsiString(FilePath)+'"'));
{$ENDIF MACOS}
end;

end.
