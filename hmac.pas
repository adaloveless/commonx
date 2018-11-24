unit hmac;

interface

uses
  System.SysUtils,
  EncdDecd,
  IdHMAC,
  IdSSLOpenSSL,
  helpers.indy,
  idglobal,
  IdHash;

type
  localstringtype = ansistring;

  THMACUtils<T: TIdHMAC, constructor> = class
  public
    class function HMAC(aKey, aMessage: localstringtype): TIdBytes;
    class function HMAC_HexStr(aKey, aMessage: localstringtype): localstringtype;
    class function HMAC_Base64(aKey, aMessage: localstringtype): localstringtype;
  end;

implementation

class function THMACUtils<T>.HMAC(aKey, aMessage: localstringtype): TIdBytes;
var
  _HMAC: T;
begin
  if not IdSSLOpenSSL.LoadOpenSSLLibrary then Exit;
  _HMAC:= T.Create;
  try
    _HMAC.Key := AnsiStringToIDBytes(aKey);
    Result:= _HMAC.HashValue(AnsiStringToIDBytes(aMessage));
  finally
    _HMAC.Free;
  end;
end;

class function THMACUtils<T>.HMAC_HexStr(aKey, aMessage: localstringtype): localstringtype;
var
  I: Byte;
begin
  Result:= '';//'0x';
  for I in HMAC(aKey, aMessage) do
    Result:= Result + IntToHex(I, 2);
end;

class function THMACUtils<T>.HMAC_Base64(aKey, aMessage: localstringtype): localstringtype;
var
  _HMAC: TIdBytes;
begin
  _HMAC:= HMAC(aKey, aMessage);
  Result:= EncodeBase64(_HMAC, Length(_HMAC));
end;

end.

(*
Below there’s an example of how to use the THMACUtils class.

program HMACSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  HMAC,
  IdHMACSHA1,
  IdHashMessageDigest;

begin
  try
    Write('HMAC_SHA1("key", "message")'#9#9'= ');
    Writeln(THMACUtils<TIdHMACSHA1>.HMAC_HexStr('key', 'message' ));
    Writeln;

    Write('HMAC_SHA256("key", "message")'#9#9'= ');
    Writeln(THMACUtils<TIdHMACSHA256>.HMAC_HexStr('key', 'message' ));
    Writeln;

    Write('HMAC_SHA1_Base64("key", "message")'#9'= ');
    Writeln(THMACUtils<TIdHMACSHA1>.HMAC_Base64('key', 'message' ));
    Writeln;

    Write('HMAC_SHA256_Base64("key", "message")'#9'= ');
    Writeln(THMACUtils<TIdHMACSHA256>.HMAC_Base64('key', 'message' ));

    Readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

*)
