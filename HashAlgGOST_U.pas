unit HashAlgGOST_U;
// Description: GOST R 34.11-94 (Wrapper for the GOST Hashing Engine)
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
  HashAlgGOSTEngine_U;

type
  THashAlgGOST = class(THashAlg)
  private
    GOSTEngine: THashAlgGOSTEngine;
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
  RegisterComponents('Hash', [THashAlgGOST]);

end;

constructor THashAlgGOST.Create(AOwner: TComponent);
begin
  inherited;
  GOSTEngine:= THashAlgGOSTEngine.Create();
  GOSTEngine.gosthash_init();
  fDigestSize := 256;
  fDigestTitle := 'GOST R 34.11-94';

end;

destructor THashAlgGOST.Destroy();
begin
  GOSTEngine.Free();
  inherited;

end;

function THashAlgGOST.HashString(theString: ansistring): THashArray;
var
  // My GOST
  context: GostHashCtx;
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

  GOSTEngine.gosthash_reset(context);
  GOSTEngine.gosthash_update(context, tempStr, len);
  GOSTEngine.gosthash_final(context, digest);

  Result := digest;

end;

function THashAlgGOST.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: GostHashCtx;
  len: integer;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      GOSTEngine.gosthash_reset(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        GOSTEngine.gosthash_update(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      GOSTEngine.gosthash_final(context, digest);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;

// xxx - get rid of this
function THashAlgGOST.HashToDisplay(theHash: THashArray): ansistring;
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

