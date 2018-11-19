unit BetterFileStream;

interface

uses
  classes, sysutils, system.RTLConsts, systemx, betteradapters;


type



  TBetterFileStream = class(THandleStream)
  strict private
    FFileName: string;
    function GetEof: boolean;
  public
    property eof: boolean read GetEof;
    constructor Create(const AFileName: string; Mode: cardinal); overload; virtual;
    constructor Create(const AFileName: string; Mode: cardinal; Rights: Cardinal); overload; virtual;
    constructor Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal); overload; virtual;
    destructor Destroy; override;
    property FileName: string read FFileName;
    procedure Reopen;

  end;


  TFileStreamWithVirtualConstructors = class(TBetterFileStream)
  public
    constructor Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal);override;
    constructor Create(const AFileName: string; Mode: Word; Rights:cardinal; Flags:cardinal);reintroduce;overload;
    constructor Create(const AFileName:  string; Mode: Word; Rights:cardinal);reintroduce;overload;
    constructor Create(const AFileName: string; Mode: Word);reintroduce;overload;

  end;

  TMoreBetterFileStream = class(TFileStreamWithVirtualConstructors)
  public
  end;

const
  FILE_FLAG_WRITE_THROUGH = $80000000;
  FILE_FLAG_NO_BUFFERING =  $20000000;


implementation

{ TBetterFileStream }


constructor TBetterFileStream.Create(const AFileName: string; Mode: cardinal);
{$IF Defined(MSWINDOWS)}
begin
  Create(AFilename, Mode, 0);
end;
{$ELSEIF Defined(POSIX)}
begin
  Create(AFilename, Mode, FileAccessRights);
end;
{$ENDIF POSIX}

constructor TBetterFileStream.Create(const AFileName: string; Mode: cardinal; Rights: Cardinal; Flags: cardinal);
var
  LShareMode: Word;
begin
  if (Mode and fmCreate = fmCreate) then
  begin
    LShareMode := Mode and $FF;
    if LShareMode = $FF then
      LShareMode := fmShareExclusive; // For compat in case $FFFF passed as Mode
    inherited Create(BetterFileCreate(AFileName, LShareMode, Rights, Flags));
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFCreateError.CreateResFmt(@SFCreateErrorEx, [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end
  else
  begin
{$IFDEF MSWINDOWS}
    inherited Create(BetterFileOpen(AFileName, Mode, rights, flags));
{$ELSE !MSWINDOWS}

    inherited Create(BetterFileOpen(AFileName, Mode, rights, flags));
{$ENDIF MSWINDOWS}
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end;
  FFileName := AFileName;
end;

constructor TBetterFileStream.Create(const AFileName: string; Mode,
  Rights: Cardinal);
var
  LShareMode: Word;
begin
  if (Mode and fmCreate = fmCreate) then
  begin
    LShareMode := Mode and $FF;
    if LShareMode = $FF then
      LShareMode := fmShareExclusive; // For compat in case $FFFF passed as Mode
    inherited Create(FileCreate(AFileName, LShareMode, Rights));
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFCreateError.CreateResFmt(PResStringRec(@SFCreateErrorEx), [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end
  else
  begin
{$IFDEF MSWINDOWS}
    inherited Create(FileOpen(AFileName, Mode or Rights));
{$ELSE !MSWINDOWS}

    inherited Create(FileOpen(AFileName, Mode));
{$ENDIF MSWINDOWS}
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFOpenError.CreateResFmt(PResStringRec(@SFOpenErrorEx), [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end;
  FFileName := AFileName;
end;

destructor TBetterFileStream.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    FileClose(FHandle);
  FHandle := INVALID_HANDLE_VALUE;
  inherited Destroy;
end;

function TBetterFileStream.GetEof: boolean;
begin
  result := position >= size;
end;

procedure TBetterFileStream.Reopen;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    FileClose(FHandle);

end;





{ TFileStreamWithVirtualConstructors }

constructor TFileStreamWithVirtualConstructors.Create(const AFileName: string;
  Mode: Word; Rights: cardinal);
begin
  Create(afilename, mode, rights,0);
end;

constructor TFileStreamWithVirtualConstructors.Create(const AFileName: string;
  Mode: Word);
begin
  Create(afilename, cardinal(mode), cardinal(0),cardinal(0));
end;

constructor TFileStreamWithVirtualConstructors.Create(const AFileName: string;
  Mode: Word; Rights, Flags: cardinal);
begin
  Create(afilename, cardinal(mode), rights, flags);
end;

constructor TFileStreamWithVirtualConstructors.Create(const AFileName: string;
  Mode, Rights, Flags: cardinal);
begin
  inherited CReate(afilename, mode, rights, flags);
end;




end.
