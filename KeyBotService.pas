unit KeyBotService;

interface

uses
  sharedobject, betterobject, classes, systemx, sysutils, typex,  stringx, orderlyinit;
type
  TKeyBot = class (TSharedObject)
  protected
    function TryGetNextID(sDIR: ansistring; sIDFile: ansistring; out i:int64): boolean;
    function TrySetNextID(sDir: ansistring; sIDFile: ansistring; i: int64): boolean;
  public

    function GetNextID(sDir: ansistring; sIDFILE: ansistring): int64;
    procedure SetNextID(sDIR: ansistring ; sIDFile: ansistring; i: int64);
  end;

var
  KEYBOT: TKeybot;

implementation

{ TKeyBot }

function TKeyBot.GetNextID(sDir, sIDFILE: ansistring): int64;
begin
  while not TryGetNextID(sDIR, sIDFile, result) do
    sleep(1000);
end;

procedure TKeyBot.SetNextID(sDIR, sIDFile: ansistring; i: int64);
begin
  while not TrySetNextID(sDIR, sIDFile, i) do
    sleep(100);
end;

function TKeyBot.TryGetNextID(sDir, sIDFILE: ansistring; out i:int64): boolean;
var
  sFile: ansistring;
  fs: TFileStream;
  i2, i3: int64;
begin
  sFile := slash(sDIR)+sIDFile;
  result := true;
  Lock;
  try
    try
      if fileexists(sFile) then begin
        fs := TFileStream.create(sFile, fmOpenRead, fmShareExclusive);
        try
          fs.Seek(0,0);
          try
            fs.Read(i, sizeof(i));
            fs.Read(i2, sizeof(i2));
            fs.Read(i3, sizeof(i3));
            if ((i xor $FFFFFFFF) <> i2) then i := 0;
            if ((i xor $77777777) <> i3) then i := 0;

          except
            result := false;
          end;
        finally
          fs.free;
        end;
      end else begin
        i := 1;
      end;

      SetNextID(sDir, sIDFile, i + 1);
    except
      result := false;
    end;

  finally
    Unlock;
  end;
end;

function TKeyBot.TrySetNextID(sDir, sIDFile: ansistring; i: int64): boolean;
var
  sFile: ansistring;
  fs: TFileStream;
  i2,i3: int64;
begin
  sFile := slash(sDIR)+sIDFile;
  result := true;
  Lock;
  try
    try
      forcedirectories(extractfilepath(sfile));

      if fileexists(sFile) then
        fs := TFileStream.create(sFile, fmOpenWrite, fmShareExclusive)
      else
        fs := TFileStream.create(sFile, fmCreate, fmShareExclusive);

      try
        fs.Seek(0,0);
        try
          i2 := i xor $FFFFFFFF;
          i3 := i xor $77777777;
          fs.Write(i, sizeof(i));
          fs.Write(i2, sizeof(i2));
          fs.Write(i3, sizeof(i3));
        except
          result := false;
        end;
      finally
        fs.free;
      end;

    except
      result := false;
    end;

  finally
    Unlock;
  end;
end;

procedure oinit;
begin
  KEYBOT := TKeybot.create;
end;

procedure ofinal;
begin
  KEYBOT.free;
  KEYBOT := nil;


end;

initialization
  init.RegisterProcs('KeyBotService', oinit, ofinal);


finalization

end.
