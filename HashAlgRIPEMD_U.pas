unit HashAlgRIPEMD_U;
// Description: RIPEMD Hash (Wrapper for the RIPEMD Hashing Engine)
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
  HashAlgRIPEMDEngine_U;

type
  THashAlgRIPEMD = class(THashAlg)
  private
    ripemdEngine: THashAlgRIPEMDEngine;
  protected
    procedure SetDigestSize(size: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    function  HashString(theString: ansistring): THashArray; override;
    function  HashFile(filename: ansistring; var digest: THashArray): boolean; override;

    function  HashToDisplay(theHash: THashArray): ansistring; override;
  published
    property DigestSize: integer read fDigestSize write SetDigestSize;
  end;

  THashAlgRIPEMD128 = class(THashAlgRIPEMD)
    constructor Create(AOwner: TComponent); override;
  end;

  THashAlgRIPEMD160 = class(THashAlgRIPEMD)
    constructor Create(AOwner: TComponent); override;
  end;

  THashAlgRIPEMD256 = class(THashAlgRIPEMD)
    constructor Create(AOwner: TComponent); override;
  end;

  THashAlgRIPEMD320 = class(THashAlgRIPEMD)
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

uses
     SysUtils; // needed for fmOpenRead


procedure Register;
begin
  RegisterComponents('Hash', [THashAlgRIPEMD]);
  RegisterComponents('Hash', [THashAlgRIPEMD128]);
  RegisterComponents('Hash', [THashAlgRIPEMD160]);
  RegisterComponents('Hash', [THashAlgRIPEMD256]);
  RegisterComponents('Hash', [THashAlgRIPEMD320]);
end;

constructor THashAlgRIPEMD128.Create(AOwner: TComponent);
begin
  inherited;
  fDigestSize:= 128;
  fDigestTitle := 'RIPEMD-128';

end;

constructor THashAlgRIPEMD160.Create(AOwner: TComponent);
begin
  inherited;
  fDigestSize:= 160;
  fDigestTitle := 'RIPEMD-160';

end;

constructor THashAlgRIPEMD256.Create(AOwner: TComponent);
begin
  inherited;
  fDigestSize:= 256;
  fDigestTitle := 'RIPEMD-256';

end;

constructor THashAlgRIPEMD320.Create(AOwner: TComponent);
begin
  inherited;
  fDigestSize:= 320;
  fDigestTitle := 'RIPEMD-320';

end;

constructor THashAlgRIPEMD.Create(AOwner: TComponent);
begin
  inherited;
  ripemdEngine:= THashAlgRIPEMDEngine.Create();
  fDigestTitle := 'RIPEMD-xxx';

end;

destructor THashAlgRIPEMD.Destroy();
begin
  ripemdEngine.Free();
  inherited;

end;

function THashAlgRIPEMD.HashString(theString: ansistring): THashArray;
var
  // My RIPEMD
  context: RIPEMD_CTX;
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

  ripemdEngine.OutputLength := DigestSize;
  ripemdEngine.RIPEMDInit(context);
  ripemdEngine.RIPEMDUpdate(context, tempStr, len);
  ripemdEngine.RIPEMDFinal(digest, context);

  Result := digest;

end;

function THashAlgRIPEMD.HashFile(filename: ansistring; var digest: THashArray): boolean;
var
  context: RIPEMD_CTX;
  len: integer;
  buffer: array [0..1023] of byte;
  inputFile: TFileStream;
begin
  Result := FALSE;

  try
    inputFile := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      ripemdEngine.OutputLength := DigestSize;
      ripemdEngine.RIPEMDInit(context);

      len := inputFile.Read(buffer, sizeof(buffer));
      while (len>0) do
        begin
        ripemdEngine.RIPEMDUpdate(context, buffer, len);
        len := inputFile.Read(buffer, sizeof(buffer));
        end;

      ripemdEngine.RIPEMDFinal(digest, context);

      Result := TRUE;

    finally
      inputFile.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;


function THashAlgRIPEMD.HashToDisplay(theHash: THashArray): ansistring;
var
  i: integer;
  retVal: ansistring;
begin
  retVal := '';
  for i:=0 to (DigestSize div 8)-1 do
    begin
    retVal := retVal + inttohex(theHash[i], 2);
    end;

  Result := retVal;

end;

procedure THashAlgRIPEMD.SetDigestSize(size: integer);
begin
  if (size<>128) AND
     (size<>160) AND
     (size<>256) AND
     (size<>320) then
    begin
    fDigestSize := size;
    end;
    
end;

END.

