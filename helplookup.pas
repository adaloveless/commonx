unit Helplookup;

interface

uses
  betterobject, sharedobject, sysutils, RequestInfo, webstring, stringx, stringx.ansi, classes,orderlyinit;

type
  THelplookup = class (TFakeLockQueuedObject)
  private
    FFileName: string;
    slLookupFile: TStringlist;
    function GetLookupFile: TStringList;
    function GetFileName: string;
    procedure SetFileName(const Value: string);

  protected
    property LookupFile: TStringList read GetLookupFile;

  public
    constructor Create; override;
    destructor destroy; override;
    function LookupHelpPage(rqInfo: TRequestInfo; sPage: string = ''): string; overload;
    property FileName: string read GetFileName write SetFileName;

  end;


implementation

{ THelplookup }

constructor THelplookup.Create;
begin
  inherited;
  slLookupFile := TStringList.create;
end;
//------------------------------------------------------------------------------
destructor THelplookup.destroy;
begin
  inherited;
  slLookupFile.free;
  slLookupFile := nil;
end;
//------------------------------------------------------------------------------
function THelplookup.GetFileName: string;
begin
  result := MakeThreadSafe(FFileName);

end;
//------------------------------------------------------------------------------
function THelplookup.GetLookupFile: TStringList;
begin
  result := slLookupFile;

end;
//------------------------------------------------------------------------------
function THelplookup.LookupHelpPage(rqInfo: TRequestInfo; sPage: string = ''): string;
var
  t: integer;
  sLeft, sRight: string;
begin
  LockRead;
  try
    if sPage = '' then
      sPage := rqinfo.request.document;

    uniqueString(sPage);


    for t:= 0 to slLookupFile.count-1 do begin
      SplitString(slLookupFile[t], ',', sLeft, sRight);
      if lowercase(sLeft) = lowercase(sPage) then begin
        result := sRight;
      end;
    end;

  finally
    UnLockRead;
  end;

end;
//------------------------------------------------------------------------------
procedure THelplookup.SetFileName(const Value: string);
begin
  LockWrite;
  try
    FFileName := MakeThreadSafe(Value);

//    slLookupFile.LoadFromFile(FFileName);

  finally
    UnlockWrite;
  end;

end;

initialization


finalization





end.
