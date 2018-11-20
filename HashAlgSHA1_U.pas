unit HashAlgSHA1_U;
// Description: SHA-1 Hash (Wrapper for the SHA-1 Hashing Engine)
// By Sarah Dean
// Email: sdean12@softhome.net
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes,
  HashAlg_U, HashAlgSHA1Engine_U;

type
  THashAlgSHA1 = class(THashAlg)
  private
    sha1Engine: THashAlgSHA1Engine;
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
  RegisterComponents('Hash', [THashAlgSHA1]);

end;

constructor THashAlgSHA1.Create(AOwner: TComponent);
begin
  inherited;
  sha1Engine:= THashAlgSHA1Engine.Create();
  fDigestSize := 160;
  fDigestTitle := 'SHA-1';

end;

destructor THashAlgSHA1.Destroy();
begin
  sha1Engine.Free();
  inherited;

end;

function THashAlgSHA1.HashMemory(memBlock: Pointer; len: cardinal): THashArray;
var
  digest: THashArray;
  tempStr: array of byte;
  context: SHA1_CTX;
  i: cardinal;
begin
  SetLength(tempStr, len);
  for i:=0 to len-1 do
    begin
    tempStr[i] := (PByteArray(memBlock))[i];
    end;

  sha1Engine.SHA1Init(context);
  sha1Engine.SHA1Update(context, tempStr, len);
  sha1Engine.SHA1Final(digest, context);

  Result := digest;

end;

function THashAlgSHA1.HashString(theString: ansistring): THashArray;
var
  digest: THashArray;
  len: cardinal;
  context: SHA1_CTX;
  tempStr: array of byte;
  i: cardinal;
begin
  len := length(theString);

  SetLength(tempStr, len);
  for i:=1 to len do
    begin
    tempStr[i-1] := byte((theString)[i]);
    end;

  sha1Engine.SHA1Init(context);
  sha1Engine.SHA1Update(context, tempStr, len);
  sha1Engine.SHA1Final(digest, context);

  Result := digest;

end;

function THashAlgSHA1.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: SHA1_CTX;
  len: cardinal;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      sha1Engine.SHA1Init(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        sha1Engine.SHA1Update(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      sha1Engine.SHA1Final(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


function THashAlgSHA1.HashToDisplay(theHash: THashArray): ansistring;
var
  retVal: ansistring;
begin
  retVal := inherited HashToDisplay(theHash);

  insert(' ', retVal, 33);
  insert(' ', retVal, 25);
  insert(' ', retVal, 17);
  insert(' ', retVal, 9);

  Result := retVal;

end;


END.

