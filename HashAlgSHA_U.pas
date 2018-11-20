unit HashAlgSHA_U;
// Description: SHA Hash (Wrapper for the SHA Hashing Engine)
// By Sarah Dean
// Email: sdean12@softhome.net
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes,
  HashAlg_U, HashAlgSHAEngine_U;

type
  THashAlgSHA = class(THashAlg)
  private
    shaEngine: THashAlgSHAEngine;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

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
  RegisterComponents('Hash', [THashAlgSHA]);
end;

constructor THashAlgSHA.Create(AOwner: TComponent);
begin
  inherited;
  shaEngine:= THashAlgSHAEngine.Create();
  fDigestSize := 160;
  fDigestTitle := 'SHA';

end;

destructor THashAlgSHA.Destroy();
begin
  shaEngine.Free();
  inherited;

end;

function THashAlgSHA.HashString(theString: ansistring): THashArray;
var
  digest: THashArray;
  len: cardinal;
  context: SHA_CTX;
  tempStr: array of byte;
  i: integer;
begin
  len := length(theString);

  SetLength(tempStr, len);
  for i:=1 to len do
    begin
    tempStr[i-1] := byte((theString)[i]);
    end;

  shaEngine.SHAInit(context);
  shaEngine.SHAUpdate(context, tempStr, len);
  shaEngine.SHAFinal(digest, context);

  Result := digest;

end;

function THashAlgSHA.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: SHA_CTX;
  len: integer;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      shaEngine.SHAInit(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        shaEngine.SHAUpdate(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      shaEngine.SHAFinal(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


function THashAlgSHA.HashToDisplay(theHash: THashArray): ansistring;
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

