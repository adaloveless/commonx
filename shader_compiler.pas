unit shader_compiler;

interface

{$IFDEF MSWINDOWS}
uses
  stringx, typex, dir, dirfile,exe, tools, sysutils, systemx;

type
  EShaderCompileError = class(Exception);


procedure CompileShader(sFile: string);
procedure CompileDX11Shaders(sDir: string = '');
procedure CompileShaders(sDir: string = '');

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
procedure CompileShaders(sDir: string = '');
begin
  CompileDX11Shaders(sDir);
end;


procedure CompileShader(sFile: string);
var
  tool: string;
  sOutFile: string;
  c: Tcmd_RunExe;
  s: string;
  sList: string;
begin
  tool := findTool('fxc.exe');
  sOutFile := changefileext(sFile, '.fxc');
  sList := changefileext(sfile, '.txt');
{x$DEFINE COMPILE_WITH_COMMANDS}
{$IFDEF COMPILE_WITH_COMMANDS}
  c :=  TCmd_RunExe.create;
  try
    c.Prog := tool;
    c.Params := '/T ps_5_0 "'+sFile+'" /Fo "'+sOutFile+'" 2>"'+dllpath+'hlsloutput.txt"';
    c.WorkingDir := extractfilepath(tool);
    c.hide := true;
    //c.CaptureConsoleoutput := true;
    c.batchwrap := true;
    c.Start;
    c.WaitFor;
//    if zpos('succeeded', lowercase(c.ConsoleOutput)) < 0 then begin
//      raise ECritical.create(c.ConsoleOutput);
//    end;
  finally
    c.free;
  end;
{$ELSE}
  if comparetext(extractfileext(sFile),'.vs')=0 then
    RunProgramAndWait(tool, '/T vs_5_0 "'+sFile+'" /Fc "'+sList+'" /Fo "'+sOutFile+'" 2>"'+dllpath+'hlsloutput.txt"', extractfilepath(tool), true, true, false)
  else
    RunProgramAndWait(tool, '/T ps_5_0 "'+sFile+'" /Fc "'+sList+'" /Fo "'+sOutFile+'" 2>"'+dllpath+'hlsloutput.txt"', extractfilepath(tool), true, true, false);
{$ENDIF}
  s := LoadStringFromFile(dllpath+'hlsloutput.txt');
  if s<>'' then begin
      raise EShaderCompileError.create(s);
  end;



end;

procedure CompileDX11Shaders(sDir: string = '');
var
  dir: TDirectory;
  t: ni;
begin
  if sDir = '' then
    sDir := dllpath;
  dir := TDirectory.create(sDir, '*.ps', 0,0, false, false);
  try
    for t:= 0 to dir.filecount-1 do begin
      CompileShader(dir.files[t].FullName);
    end;

  finally
    dir.free;
  end;

  dir := TDirectory.create(sDir, '*.vs', 0,0, false, false);
  try
    for t:= 0 to dir.filecount-1 do begin
      CompileShader(dir.files[t].FullName);
    end;

  finally
    dir.free;
  end;

  if sDir = '' then
    sDir := extractfilepath(FindTool('basic.hlsl'));
  dir := TDirectory.create(sDir, '*.hlsl', 0,0, false, false);
  try
    for t:= 0 to dir.filecount-1 do begin
      CompileShader(dir.files[t].FullName);
    end;

  finally
    dir.free;
  end;


end;
{$ENDIF}

end.
