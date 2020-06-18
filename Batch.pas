unit Batch;
// Contains Code for doing batch-file-like things in a console app.. with
// visual feedback

interface

uses
  commands_file, commands_system, ConsoleGlobal, consoletools, dir, dirfile, systemx, typex, numbers, commandprocessor, stringx, sysutils, exe, filemirror;



type
  TBatch = class
    class function VerifyAccessToFolder(sRemote: string): boolean;
    class function MapDrive(sDriveLetterPlusColon, sRemote: string): boolean;
    class procedure ReplicateFolder(sRemote, sLocal: string;
                                    bDeleteExtras: boolean; bSilent: boolean = false);

    class function ExeBegin(sExe: string; sParams: string; sWkDir: string):Tcmd_RunExe;
    class procedure ExeFinish(c: Tcmd_RunExe);
    class procedure Movefile(sSource, sDest: string);
  end;



implementation

{ TBatch }

class function TBatch.ExeBegin(sExe, sParams, sWkDir: string): Tcmd_RunExe;
begin
  con.WriteEx('Launching: `cE`'+sExe+NLR);
  con.WriteEx('Params   : `cE`'+stringreplace(sParams,'`','``',[rfReplaceAll])+NLR);
  result := Tcmd_RunExe.create;
  result.Prog := sExe;
  result.Params := sParams;
  result.WorkingDir :=sWkDir;
//  result.batchwrap := true;
  result.CaptureConsoleoutput := true;
  result.Start;

end;

class procedure TBatch.ExeFinish(c: Tcmd_RunExe);
begin
  c.waitfor;
  con.WriteLn(c.ConsoleOutput);
  c.Free;

end;

class function TBatch.MapDrive(sDriveLetterPlusColon, sRemote: string): boolean;
begin
  exe.RunProgramAndWait(GetSystemDir+'net.exe', '/DELETE '+sDriveLetterPlusColon,'', true);
  exe.RunProgramAndWait(GetSystemDir+'net.exe', 'use '+sDriveLetterPlusColon+' '+quote(sRemote),'', true);
  result := VerifyAccessToFolder(sDriveLetterPlusColon+'\');
end;

class procedure TBatch.Movefile(sSource, sDest: string);
begin
  con.writeEx('`cE`Moving: `cF`'+sSource+'`cE`->`cF`'+sDest+NLR);
  var mf := TFileCopyCommand.create;
  try
    mf.Source := sSource;
    mf.Destination := sDest;
    mf.Start;
    con.WatchCommand(mf);
    mf.WaitFor;
  finally
    mf.free;
  end;
end;

class procedure TBatch.ReplicateFolder(sRemote, sLocal: string;
  bDeleteExtras: boolean; bSilent: boolean = false);
var
  fil: TFileInformation;
begin
  if not bSilent then begin
    con.SetTextAttribute(CC_YELLOW);
    con.WriteEx('`cE`Replicating: `cF`');
    con.WriteEx(sRemote);
    con.WriteEx('`cE`->`cF');
    con.WriteEx(sLocal+CRLF);
  end else
    con.Twiddle;

  if not bDeleteExtras then
    NotImplemented;

  var cop := FileMirror.BeginReplicate(sRemote, sLocal, 'c:\trash');
  if not bSilent then
    con.WatchCommand(cop);
  cop.WaitFor;
  con.WriteEx('Waiting for files to copy'+NLR);


  while commands_file.fileCommands.commandcount >  commands_file.fileCommands.CompleteCount  do begin
    con.WriteEx('Waiting on '+fileCommands.CommandCount.tostring+' commands ');
    con.Twiddle;
    sleep(100);
  end;

  while BGCmd.commandcount >  BGCmd.CompleteCount  do begin
    con.WriteEx('Waiting on '+BGCmd.CommandCount.tostring+' commands ');
    con.Twiddle;
    sleep(100);
  end;
  cop.free;

  con.WriteOk(true);






end;

class function TBatch.VerifyAccessToFolder(sRemote: string): boolean;
begin
  con.WriteLn('Verifying Access to '+sRemote);
  result := DirectoryExists(sRemote);
  con.WriteOK(result);





end;

end.
