unit FileEx;

interface

uses
  MultiBufferMemoryFileStream, memoryfilestream, classes, sysutils, System.IOUtils, debug, stringx, systemx, helpers_stream, typex;


function CompareFiles(sFile1, sFile2: string): boolean;
function GetAbsoluteFileName(sPath, sFile: string): string;
function IsRelative(sFile: string): boolean;

function WarnIfDeleteFailed(sFile: string): boolean;

implementation


function WarnIfDeleteFailed(sFile: string): boolean;
begin
  result := false;
  if fileexists(sFile) then begin
    Debug.Log(nil,'Warning! Delete failure (open?): '+sFile);
    result := true;
  end;
end;


function CompareFiles(sFile1, sFile2: string): boolean;
type
  TFSType = TMemoryFileStream;
var
  fs1,fs2: TFSType;
  t,u: integer;
  //b1,b2: byte;
  a1,a2: array of byte;
  i,ii: ni;
begin
  result := true;

  if not TFile.Exists(sFile1) then begin
    result := false;
    exit;
  end;

  if not fileexists(sFile2) then begin
    result := false;
    exit;
  end;

  setlength(a1, 65536);
  setlength(a2, 65536);

  fs1 := nil;
  fs2 := nil;
  try
    fs1 := TMemoryFileStream.create(sFile1, fmOpenRead+fmsharedenywrite);
    try
      fs2 := TMemoryFileSTream.create(sFile2, fmOpenRead+fmsharedenywrite);
      if fs1.Size <> fs2.Size then begin
        result := false;
        exit;
      end;

      fs1.seek(0,soBeginning);
      fs2.seek(0,soBeginning);
      for t := 0 to fs1.size-1 do
      begin
        i := Stream_guaranteeRead(fs1,@a1[0],65536, false);
        ii := Stream_guaranteeRead(fs2,@a2[0],i, false);
        if  ii < i then  begin
          result := false;
          exit;
        end;
        //GLOG.Debug(inttostr(b1)+']['+inttostr(b2));
        for u := 0 to i-1 do begin
          if a1[u] <> a2[u] then begin
            result := false;
            exit;
          end;
        end;
      end;

    finally
      fs2.free;
    end;
  finally
    fs1.free;
  end;

end;


function GetAbsoluteFileName(sPath, sFile: string): string;
begin
  if IsRelative(sFile) then begin
    result := slash(sPath)+sFile;
  end else
    result := sFile;
end;

function IsRelative(sFile: string): boolean;
begin
  result := (not (zcopy(sFile, 0,2) = '\\')) and (not (zpos(':', sFile) >= 0));
end;




end.
