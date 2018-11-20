unit HashAlgMD5_U;
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
  HashAlgMD5Engine_U;

type
  THashAlgMD5 = class(THashAlg)
  private
    md5Engine: THashAlgMD5Engine;
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
  RegisterComponents('Hash', [THashAlgMD5]);

end;

constructor THashAlgMD5.Create(AOwner: TComponent);
begin
  inherited;
  md5Engine:= THashAlgMD5Engine.Create();
  fDigestSize := 128;
  fDigestTitle := 'MD5';

end;

destructor THashAlgMD5.Destroy();
begin
  md5Engine.Free();
  inherited;

end;

function THashAlgMD5.HashString(theString: ansistring): THashArray;
var
  // My MD5
  context: MD5_CTX;
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

  md5Engine.MD5Init(context);
  md5Engine.MD5Update(context, tempStr, len);
  md5Engine.MD5Final(digest, context);

  Result := digest;

end;

function THashAlgMD5.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: MD5_CTX;
  len: integer;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      md5Engine.MD5Init(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        md5Engine.MD5Update(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      md5Engine.MD5Final(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


END.

