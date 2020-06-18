unit MacApiEx;

interface


uses
  classes;

const
	libc = '/usr/lib/libc.dylib';

type
	PIOFile = Pointer;

function popen(const Command: PAnsiChar; Modes: PAnsiChar): PIOFile; cdecl;	external libc name 'popen';
function pclose(Stream: PIOFile): Integer; cdecl; external libc name 'pclose';
function feof(Stream: PIOFile): Integer; cdecl; external libc name 'feof';
function fread(Ptr: Pointer; Size: LongWord; N: LongWord; Stream: PIOFile): LongWord; cdecl; external libc name 'fread';
function wait(__stat_loc: PInteger): Integer; cdecl; external libc name 'wait';

procedure MakeFileExecutable(sFile: string);
procedure Execute(sFile: string);
procedure ExecCmdLine_Forget(const CmdLine: string);

implementation


procedure ExecCmdLine_Forget(const CmdLine: string);
begin
	popen(PAnsiChar(ansistring(CmdLine)), 'r');
end;

procedure ExecCmdLine(const CmdLine: string; CmdResult: TStrings);
var
	Output: PIOFile;
	Buffer: PAnsiChar;
	TempString: ansistring;
	Line: ansistring;
	BytesRead: Integer;
const
	BufferSize: Integer = 8192;
begin
	TempString := '';
	Output := popen(PAnsiChar(ansistring(CmdLine)), 'r');
	GetMem(Buffer, BufferSize);
	if Assigned(Output) then
		try
			while feof(Output) = 0 do
			begin
				BytesRead := fread(Buffer, 1, BufferSize, Output);
				SetLength(TempString, length(TempString) + BytesRead);
				Move(Buffer^, TempString[length(TempString) - (BytesRead - 1)],	BytesRead);
				while Pos(#10, TempString) > 0 do
				begin
					Line := Copy(TempString, 1, Pos(#10, TempString) - 1);
					if CmdResult <> nil then CmdResult.Add(UTF8ToString(Line));
					TempString := Copy(TempString, Pos(#10, TempString) + 1,length(TempString));
				end;
			end;
		finally
			pclose(Output);
			wait(nil);
			FreeMem(Buffer, BufferSize);
		end;
end;

procedure MakefileExecutable(sfile: string);
begin
  var sl:= TStrings.create;
  try
    ExecCmdLine('chmod +x '+sFile+'', sl);
  finally
    sl.free;
  end;

end;

procedure Execute(sFile: string);
begin
  var sl:= TStrings.create;
  try
    ExecCmdLine(sFile, sl);
  finally
    sl.free;
  end;

end;



end.
