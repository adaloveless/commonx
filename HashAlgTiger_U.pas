unit HashAlgTiger_U;
// Description: Tiger (Wrapper for the Tiger Hashing Engine)
// By Sarah Dean
// Email: sdean12@softhome.net
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes,
  HashAlg_U, HashAlgTigerEngine_U;

type
  THashAlgTiger = class(THashAlg)
  private
    tigerEngine: THashAlgTigerEngine;

    function  GetPasses(): integer;
    procedure SetPasses(passes: integer);
  protected
    { Protected declarations }
  public
    property passes: integer read GetPasses write SetPasses;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    function  HashMemory(memBlock: Pointer; len: cardinal): THashArray;
    function  HashString(theString: ansistring): THashArray; override;
    function  HashFile(filename: ansistring; var digest: THashArray): boolean; override;

  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
     SysUtils; // needed for fmOpenRead


procedure Register;
begin
  RegisterComponents('Hash', [THashAlgTiger]);

end;

constructor THashAlgTiger.Create(AOwner: TComponent);
begin
  inherited;
  tigerEngine:= THashAlgTigerEngine.Create();
  fDigestSize := 192;
  fDigestTitle := 'Tiger';

  Passes := 3;

end;

destructor THashAlgTiger.Destroy();
begin
  tigerEngine.Free();
  inherited;

end;

function  THashAlgTiger.GetPasses(): integer;
begin
  Result := tigerEngine.Passes;
end;

procedure THashAlgTiger.SetPasses(passes: integer);
begin
  tigerEngine.Passes := passes;
end;

function THashAlgTiger.HashMemory(memBlock: Pointer; len: cardinal): THashArray;
var
  digest: THashArray;
  tempStr: array of byte;
  context: Tiger_CTX;
  i: cardinal;
begin
  SetLength(tempStr, len);
  for i:=0 to len-1 do
    begin
    tempStr[i] := (PByteArray(memBlock))[i];
    end;

  tigerEngine.TigerInit(context);
  tigerEngine.TigerUpdate(context, tempStr, len);
  tigerEngine.TigerFinal(digest, context);

  Result := digest;

end;

function THashAlgTiger.HashString(theString: ansistring): THashArray;
var
  digest: THashArray;
  len: cardinal;
  context: Tiger_CTX;
  tempStr: array of byte;
  i: cardinal;
begin
  len := length(theString);

  SetLength(tempStr, len);
  for i:=1 to len do
    begin
    tempStr[i-1] := byte((theString)[i]);
    end;

  tigerEngine.TigerInit(context);
  tigerEngine.TigerUpdate(context, tempStr, len);
  tigerEngine.TigerFinal(digest, context);

  Result := digest;

end;

function THashAlgTiger.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: Tiger_CTX;
  len: cardinal;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      tigerEngine.TigerInit(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        tigerEngine.TigerUpdate(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      tigerEngine.TigerFinal(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


END.

