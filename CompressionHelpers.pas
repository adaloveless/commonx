unit CompressionHelpers;

interface

uses
  zip, typex;


function CompressStr(s: ansistring): RawByteString;
function DecompressStr(bs: RawByteString): ansistring;


implementation

function CompressStr(s: ansistring): RawByteString;
var
  sz: nativeint;
begin
  setlength(result, length(s)*4);
  sz := zip.ZipRam(@s[low(s)], @result[low(result)], length(s), length(result), nil);
  setlength(result, sz);

end;

function DecompressStr(bs: RawByteString): ansistring;
var
  sz: nativeint;
begin
  setlength(result, length(bs)*3);
  sz := zip.UnZipRam(@bs[low(bs)], @result[low(result)], length(bs), length(result), nil);
  //if unzipram returns <0 then there wasn't enough memory... try again with 0-result memory allocated
  if sz < 0 then begin
    setlength(result, 0-sz);
    sz := zip.UnZipRam(@bs[low(bs)], @result[low(result)], length(bs), length(result), nil);
    if sz < 0 then
      raise ECritical.create('failed to unzip ram');
  end;
  if sz > 0 then
    setlength(result, sz);

end;



end.
