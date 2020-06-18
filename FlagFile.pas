unit FlagFile;

interface


uses
  betterobject, systemx, classes, ExceptionsX, sysutils, typex;

type
  ILockFile = IHolder<TFileStream>;
  TFlagFileHandler = class(TSharedObject)
  public
    class function TryHoldFlag(sFile: string): ILockFile;
    class function AssertFlagHold(sFile: string): ILockFile;
  end;




implementation

{ TFlagFileHandler }


class function TFlagFileHandler.AssertFlagHold(sFile: string): ILockFile;
begin
  result := TryHoldFlag(sFile);
  if result = nil then
    raise ECritical.create('Could not hold lock file, another process might be running! '+sFile);

end;

class function TFlagFileHandler.TryHoldFlag(sFile: string): ILockFile;
begin
  try
    result := nil;
    var fs := TFileStream.create(sFile, fmCReate);
    result := Tholder<TfileStream>.create;
    result.o := fs;

  except
    result := nil;
  end;
end;

end.
