unit Hash;

interface

uses
  HashAlgMD5_U, HashAlg_U, sysutils, classes;

function GetMD5Hash(s: ansistring): ansistring;
function GetHMACMD5Hash(sKey: ansistring; sPayload: ansistring): ansistring;
function GetMD5HashRaw(s: ansistring): ansistring;
function MyGetHMACMD5Hash(sKey: ansistring; sPayload: ansistring): ansistring;


implementation

uses cryptlib, DelphiCryptlib;

function GetMD5Hash(s: ansistring): ansistring;
var
  t: integer;
  ss: ansistring;
begin
  result := '';
  ss := GetMD5HashRaw(s);
  for t:= 1 to 16 do begin
    result := result + ansistring(inttohex(ord(ss[t]),2));
  end;

end;

function GetMD5HashRaw(s: ansistring): ansistring;
var
  h: THashAlgMd5;
  hd: THashArray;
  t: integer;
begin
  result := '';
  h := THashAlgMD5.Create(nil);
  try
    hd := h.HashString(s);
    for t:= 0 to 15 do begin
      result := result + chr(hd[t]);
    end;
  finally
    h.free;
  end;

end;


function MyGetHMACMD5Hash(sKey: ansistring; sPayload: ansistring): ansistring;
var
  t: integer;
  k1,k2: ansistring;
  h, s1,s2,s3: ansistring;
const
  ipad = $36;
  opad = $5C;

begin
  //    (1) append zeros to the end of K to create a B byte ansistring
  //        (e.g., if K is of length 20 bytes and B=64, then K will be
  //         appended with 44 zero bytes 0x00)
  k1 := sKey;
  while length(k1) < 64 do
      k1 := k1+#0;

  //    (2) XOR (bitwise exclusive-OR) the B byte ansistring computed in step
  //        (1) with ipad
  for t:= 1 to 64 do
    k1[t] := ansichar(ord(k1[t]) xor ipad);

  //    (3) append the stream of data 'text' to the B byte ansistring resulting
  //        from step (2)
  s1 := k1+sPayLoad;

  //    (4) apply H to the stream generated in step (3)
  h := GetMD5HashRaw(s1);

  //    (5) XOR (bitwise exclusive-OR) the B byte ansistring computed in
  //        step (1) with opad
  k2 := sKey;
  while length(k2) < 64 do
      k2 := k2+#0;

  for t:= 1 to 64 do
    k2[t] := ansichar(ord(k2[t]) xor opad);

//    (6) append the H result from step (4) to the B byte ansistring
//        resulting from step (5)
  s2 := k2 + h;


//    (7) apply H to the stream generated in step (6) and output
//        the result
  result := GetMD5Hash(s2);

end;


function GetHMACMD5Hash(sKey: ansistring; sPayload: ansistring): ansistring;
var
  ReadLen: Integer;
  HashCode: ansistring;
  Hash: TCryptKey;
begin
  Hash := nil;
  try
    Hash := TCryptKey.Create(CRYPT_ALGO_HMAC_MD5);
    Hash.Password := sKey;
//    Hash.SetOption(CRYPT_OPTION_MISC_ASYNCINIT, False);
    Hash.Encrypt(PAnsiChar(sPayload), length(sPayLoad));


//    while hash.AsyncQuery() = 0 do
//      sleep(1);

    sleep(10000);
    HashCode := Hash.HashValue;       { get the binary hashcode as ansistring value }
                                      { convert hash to hex ansistring in result }
    SetString(result, nil, Length(HashCode) * 2);
    BinToHex(PAnsiChar(HashCode), PAnsiChar(result), Length(HashCode));
  finally
    if Assigned(Hash) then
      Hash.Free;
  end;
end;

end.
