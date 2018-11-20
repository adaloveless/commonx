unit HashAlgUnified_U;
// Description: 
// By Sarah Dean
// Email: sdean12@softhome.net
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  classes,
  HashAlg_U;

type
  // !! WARNING !!
  // When updating this array; ensure that a corresponding hash object is
  // created in the constructor
  fhHashType = (
                hashMD2,
                hashMD4,
                hashMD5,
                hashSHA,
                hashSHA1,
                hashSHA256,
                hashSHA384,
                hashSHA512,
                hashRIPEMD128,
                hashRIPEMD160,
                hashRIPEMD256,
                hashRIPEMD320,
                hashGOST,
                hashTiger
               );

type
  THashAlgUnified = class(THashAlg)
  private
    { Private declarations }
  protected
    fHashObjs: array [fhHashType] of THashAlg;
    fActiveHash: fhHashType;

    function GetDigestName(): ansistring;
    function GetDigestSize: integer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    function  HashString(theString: ansistring): THashArray; override;
    function  HashFile(filename: ansistring; var digest: THashArray): boolean; override;

    function  HashToDisplay(theHash: THashArray): ansistring; override;
  published
    property Title: ansistring read GetDigestName;
    property DigestSize: integer read GetDigestSize;

    property ActiveHash: fhHashType read fActiveHash write fActiveHash;

  end;

procedure Register;

implementation

uses
   HashAlgMD2_U,
   HashAlgMD4_U,
   HashAlgMD5_U,
   HashAlgSHA_U,
   HashAlgSHA1_U,
   HashAlgSHA256_U,
   HashAlgSHA384_U,
   HashAlgSHA512_U,
   HashAlgRIPEMD_U,
   HashAlgGOST_U,
   HashAlgTiger_U;


procedure Register;
begin
  RegisterComponents('Hash', [THashAlgUnified]);
end;


constructor THashAlgUnified.Create(AOwner: TComponent);
begin
  inherited;

  // Default to SHA-1 (can be any...)
  ActiveHash := hashSHA1;

  // Create objects
  fHashObjs[hashMD2] := THashAlgMD2.Create(nil);
  fHashObjs[hashMD4] := THashAlgMD4.Create(nil);
  fHashObjs[hashMD5] := THashAlgMD5.Create(nil);
  fHashObjs[hashSHA] := THashAlgSHA.Create(nil);
  fHashObjs[hashSHA1] := THashAlgSHA1.Create(nil);
  fHashObjs[hashSHA256] := THashAlgSHA256.Create(nil);
  fHashObjs[hashSHA384] := THashAlgSHA384.Create(nil);
  fHashObjs[hashSHA512] := THashAlgSHA512.Create(nil);
  fHashObjs[hashRIPEMD128] := THashAlgRIPEMD128.Create(nil);
  fHashObjs[hashRIPEMD160] := THashAlgRIPEMD160.Create(nil);
  fHashObjs[hashRIPEMD256] := THashAlgRIPEMD256.Create(nil);
  fHashObjs[hashRIPEMD320] := THashAlgRIPEMD320.Create(nil);
  fHashObjs[hashGOST] := THashAlgGOST.Create(nil);
  fHashObjs[hashTiger] := THashAlgTiger.Create(nil);

end;


destructor THashAlgUnified.Destroy();
var
  i: fhHashType;
begin
  for i:=low(fHashObjs) to high(fHashObjs) do
    begin
    fHashObjs[i].Free();
    end;

  inherited;
end;




function THashAlgUnified.GetDigestName(): ansistring;
begin
  Result := fHashObjs[ActiveHash].Title;
end;


function THashAlgUnified.GetDigestSize: integer;
begin
  Result := fHashObjs[ActiveHash].DigestSize;
end;


function  THashAlgUnified.HashString(theString: ansistring): THashArray;
begin
  Result := fHashObjs[ActiveHash].HashString(theString);
end;


function  THashAlgUnified.HashFile(filename: ansistring; var digest: THashArray): boolean;
begin
  Result := fHashObjs[ActiveHash].HashFile(filename, digest);
end;


function  THashAlgUnified.HashToDisplay(theHash: THashArray): ansistring;
begin
  Result := fHashObjs[ActiveHash].HashToDisplay(theHash);
end;



END.


