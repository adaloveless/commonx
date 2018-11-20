unit HashAlgSHA256_U;
// Description: SHA-256 Hash (Wrapper for the SHA-256 Hashing Engine)
// By Sarah Dean
// Email: sdean12@softhome.net
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes,
  HashAlg_U, HashAlgSHA256Engine_U;

type
  THashAlgSHA256 = class(THashAlg)
  private
    shaXXXEngine: THashAlgSHA256Engine;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    function  HashMemory(memBlock: Pointer; len: cardinal): THashArray;
    function  HashString(theString: ansistring): THashArray; override;
    function  HashFile(filename: ansistring; var digest: THashArray): boolean; override;

    function  HashToDisplay(theHash: THashArray): ansistring; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
     SysUtils; // needed for fmOpenRead


procedure Register;
begin
  RegisterComponents('Hash', [THashAlgSHA256]);

end;

constructor THashAlgSHA256.Create(AOwner: TComponent);
begin
  inherited;
  shaXXXEngine:= THashAlgSHA256Engine.Create();
  fDigestSize := 256;
  fDigestTitle := 'SHA-256';

end;

destructor THashAlgSHA256.Destroy();
begin
  shaXXXEngine.Free();
  inherited;

end;

function THashAlgSHA256.HashMemory(memBlock: Pointer; len: cardinal): THashArray;
var
  digest: THashArray;
  tempStr: array of byte;
  context: SHA256_CTX;
  i: cardinal;
begin
  SetLength(tempStr, len);
  for i:=0 to len-1 do
    begin
    tempStr[i] := (PByteArray(memBlock))[i];
    end;

  shaXXXEngine.SHA256Init(context);
  shaXXXEngine.SHA256Update(context, tempStr, len);
  shaXXXEngine.SHA256Final(digest, context);

  Result := digest;

end;

function THashAlgSHA256.HashString(theString: ansistring): THashArray;
var
  digest: THashArray;
  len: cardinal;
  context: SHA256_CTX;
  tempStr: array of byte;
  i: cardinal;
begin
  len := length(theString);

  SetLength(tempStr, len);
  for i:=1 to len do
    begin
    tempStr[i-1] := byte((theString)[i]);
    end;

  shaXXXEngine.SHA256Init(context);
  shaXXXEngine.SHA256Update(context, tempStr, len);
  shaXXXEngine.SHA256Final(digest, context);

  Result := digest;

end;

function THashAlgSHA256.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: SHA256_CTX;
  len: cardinal;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      shaXXXEngine.SHA256Init(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        shaXXXEngine.SHA256Update(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      shaXXXEngine.SHA256Final(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


function THashAlgSHA256.HashToDisplay(theHash: THashArray): ansistring;
var
  retVal: ansistring;
begin
  retVal := inherited HashToDisplay(theHash);

  insert(' ', retVal, 57);
  insert(' ', retVal, 49);
  insert(' ', retVal, 41);
  insert(' ', retVal, 33);
  insert(' ', retVal, 25);
  insert(' ', retVal, 17);
  insert(' ', retVal, 9);

  Result := retVal;

end;


END.

