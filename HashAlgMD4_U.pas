unit HashAlgMD4_U;
// Description: MD5 Hash (Wrapper for the MD5 Hashing Engine)
// By Sarah Dean
// Email: sdean12@softhome.net
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes,
  HashAlg_U,
  HashAlgMD4Engine_U;

type
  THashAlgMD4 = class(THashAlg)
  private
    md4Engine: THashAlgMD4Engine;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

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
  RegisterComponents('Hash', [THashAlgMD4]);
end;

constructor THashAlgMD4.Create(AOwner: TComponent);
begin
  inherited;
  md4Engine:= THashAlgMD4Engine.Create();
  fDigestSize := 128;
  fDigestTitle := 'MD4';

end;

destructor THashAlgMD4.Destroy();
begin
  md4Engine.Free();
  inherited;

end;

function THashAlgMD4.HashString(theString: ansistring): THashArray;
var
  // My MD5
  context: MD4_CTX;
  digest: THashArray;
  len: cardinal;
  tempStr: array of byte;
  i: integer;
begin
  len := length(theString);

  SetLength(tempStr, len);
  for i:=1 to len do
    begin
    tempStr[i-1] := byte((theString)[i]);
    end;

  md4Engine.MD4Init(context);
  md4Engine.MD4Update(context, tempStr, len);
  md4Engine.MD4Final(digest, context);

  Result := digest;

end;

function THashAlgMD4.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: MD4_CTX;
  len: integer;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      md4Engine.MD4Init(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        md4Engine.MD4Update(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      md4Engine.MD4Final(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


END.

