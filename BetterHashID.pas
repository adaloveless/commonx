unit BetterHashID;

interface

uses SysUtils, IdGlobal, IdHash, IdHashMessageDigest,IdCoder, IdCoderMIME;



function GetMD5Hash(input:string): string;
function GetMD5HashBase64(input:string): string;

implementation

function GetMD5Hash(input:string): string;
begin
  with TIdHashMessageDigest5.Create do
  try
      var hash := HashStringAsHex(input);

      result := IdGlobal.IndyLowerCase ( hash );
  finally
      Free;
  end;
end;

function GetMD5HashBase64(input:string): string;
begin
  with TIdHashMessageDigest5.Create do
  try
      var hash := HashString(input);
      var base64 := TIdEncoderMIME.EncodeBytes(hash);

      result := base64;
  finally
      Free;
  end;
end;


end.
