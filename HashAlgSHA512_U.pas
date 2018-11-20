unit HashAlgSHA512_U;
// Description: SHA-512 Hash (Wrapper for the SHA-512 Hashing Engine)
// By Sarah Dean
// Email: sdean12@softhome.net
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes,
  HashAlg_U, HashAlgSHA512Engine_U;

type
  THashAlgSHA512 = class(THashAlg)
  private
    shaXXXEngine: THashAlgSHA512Engine;
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
  RegisterComponents('Hash', [THashAlgSHA512]);

end;

constructor THashAlgSHA512.Create(AOwner: TComponent);
begin
  inherited;
  shaXXXEngine:= THashAlgSHA512Engine.Create();
  fDigestSize := 512;
  fDigestTitle := 'SHA-512';

end;

destructor THashAlgSHA512.Destroy();
begin
  shaXXXEngine.Free();
  inherited;

end;

function THashAlgSHA512.HashMemory(memBlock: Pointer; len: cardinal): THashArray;
var
  digest: THashArray;
  tempStr: array of byte;
  context: SHA512_CTX;
  i: cardinal;
begin
  SetLength(tempStr, len);
  for i:=0 to len-1 do
    begin
    tempStr[i] := (PByteArray(memBlock))[i];
    end;

  shaXXXEngine.SHA512Init(context);
  shaXXXEngine.SHA512Update(context, tempStr, len);
  shaXXXEngine.SHA512Final(digest, context);

  Result := digest;

end;

function THashAlgSHA512.HashString(theString: ansistring): THashArray;
var
  digest: THashArray;
  len: cardinal;
  context: SHA512_CTX;
  tempStr: array of byte;
  i: cardinal;
begin
  len := length(theString);

  SetLength(tempStr, len);
  for i:=1 to len do
    begin
    tempStr[i-1] := byte((theString)[i]);
    end;

  shaXXXEngine.SHA512Init(context);
  shaXXXEngine.SHA512Update(context, tempStr, len);
  shaXXXEngine.SHA512Final(digest, context);

  Result := digest;

end;

function THashAlgSHA512.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: SHA512_CTX;
  len: cardinal;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      shaXXXEngine.SHA512Init(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        shaXXXEngine.SHA512Update(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      shaXXXEngine.SHA512Final(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


function THashAlgSHA512.HashToDisplay(theHash: THashArray): ansistring;
var
  retVal: ansistring;
begin
  retVal := inherited HashToDisplay(theHash);

  insert(' ', retVal, 113);
  insert(' ', retVal, 97);
  insert(' ', retVal, 81);
  insert(' ', retVal, 65);
  insert(' ', retVal, 49);
  insert(' ', retVal, 33);
  insert(' ', retVal, 17);

  Result := retVal;

end;


END.

