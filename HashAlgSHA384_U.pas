unit HashAlgSHA384_U;
// Description: SHA-384 Hash (Wrapper for the SHA-384 Hashing Engine)
// By Sarah Dean
// Email: sdean12@softhome.net
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes,
  HashAlg_U, HashAlgSHA384Engine_U;

type
  THashAlgSHA384 = class(THashAlg)
  private
    shaXXXEngine: THashAlgSHA384Engine;
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
  RegisterComponents('Hash', [THashAlgSHA384]);

end;

constructor THashAlgSHA384.Create(AOwner: TComponent);
begin
  inherited;
  shaXXXEngine:= THashAlgSHA384Engine.Create();
  fDigestSize := 384;
  fDigestTitle := 'SHA-384';

end;

destructor THashAlgSHA384.Destroy();
begin
  shaXXXEngine.Free();
  inherited;

end;

function THashAlgSHA384.HashMemory(memBlock: Pointer; len: cardinal): THashArray;
var
  digest: THashArray;
  tempStr: array of byte;
  context: SHA384_CTX;
  i: cardinal;
begin
  SetLength(tempStr, len);
  for i:=0 to len-1 do
    begin
    tempStr[i] := (PByteArray(memBlock))[i];
    end;

  shaXXXEngine.SHA384Init(context);
  shaXXXEngine.SHA384Update(context, tempStr, len);
  shaXXXEngine.SHA384Final(digest, context);

  Result := digest;

end;

function THashAlgSHA384.HashString(theString: ansistring): THashArray;
var
  digest: THashArray;
  len: cardinal;
  context: SHA384_CTX;
  tempStr: array of byte;
  i: cardinal;
begin
  len := length(theString);

  SetLength(tempStr, len);
  for i:=1 to len do
    begin
    tempStr[i-1] := byte((theString)[i]);
    end;

  shaXXXEngine.SHA384Init(context);
  shaXXXEngine.SHA384Update(context, tempStr, len);
  shaXXXEngine.SHA384Final(digest, context);

  Result := digest;

end;

function THashAlgSHA384.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: SHA384_CTX;
  len: cardinal;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      shaXXXEngine.SHA384Init(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        shaXXXEngine.SHA384Update(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      shaXXXEngine.SHA384Final(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


function THashAlgSHA384.HashToDisplay(theHash: THashArray): ansistring;
var
  retVal: ansistring;
begin
  retVal := inherited HashToDisplay(theHash);

  insert(' ', retVal, 81);
  insert(' ', retVal, 65);
  insert(' ', retVal, 49);
  insert(' ', retVal, 33);
  insert(' ', retVal, 17);

  Result := retVal;

end;


END.

