unit HashAlgMD2_U;
// Description: MD2 Hash (Wrapper for the MD5 Hashing Engine)
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
  HashAlgMD2Engine_U;

type
  THashAlgMD2 = class(THashAlg)
  private
    md2Engine: THashAlgmd2Engine;
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
  RegisterComponents('Hash', [THashAlgMD2]);
end;

constructor THashAlgMD2.Create(AOwner: TComponent);
begin
  inherited;
  md2Engine:= THashAlgmd2Engine.Create();
  fDigestSize := 128;
  fDigestTitle := 'MD2';

end;

destructor THashAlgMD2.Destroy();
begin
  md2Engine.Free();
  inherited;

end;

function THashAlgMD2.HashString(theString: ansistring): THashArray;
var
  // My MD5
  context: MD2_CTX;
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

  md2Engine.MD2Init(context);
  md2Engine.MD2Update(context, tempStr, len);
  md2Engine.MD2Final(digest, context);

  Result := digest;

end;

function THashAlgMD2.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: MD2_CTX;
  len: integer;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      md2Engine.MD2Init(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        md2Engine.MD2Update(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      md2Engine.MD2Final(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


END.

