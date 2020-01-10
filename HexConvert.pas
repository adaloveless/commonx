unit HexConvert;

interface

uses classes, stringx, typex, numbers, debug, helpers_stream, MultiBufferMemoryFileStream;


//Some Example Intel Hex LInes
//[len] [addr] [record type] [data] [checksum]
//:02   0000   04            8000 7A
//:20   4000   00            481FD70380004024EBCD404048261E26C048D70380003EFAC60CE0B083C9E3CD 99
//:02   0000   04            8006 74
//:20   0000   00            0000000000000000000000000000000000000000000000000000000000000000 E0
//:20   FFE0   00            00000000000000000000000000000000000000000000000000000000FB73C2E0 F1
//:04   0000   05            80004024 13
//:00   0000   01            FF


//Proposed Advanced Hex Type
//[len] [record type] {addr} [data] [checksum] [xor-sum]
//:02   04            0000   8000   7A         86
//:20   00            4000   481FD70380004024EBCD404048261E26C048D70380003EFAC60CE0B083C9E3CD 99 XS
//:02   04            0000   8006   74         80
//:20   00            0000   0000000000000000000000000000000000000000000000000000000000000000 E0 XS
//:20   00            FFE0   00000000000000000000000000000000000000000000000000000000FB73C2E0 F1 XS
//:04   05            0000   80004024 13 XS
//:00   01            FF     XS



//Notes
// if high-bit is set on length, then length is actually a 15-bit integer, read one more byte, mask off high-bit. BIG endgia
//Record Type is Second, where it should be



type
  THexLine = record
//    recordtype
  end;



function BinToHex(sFile: string; bytesPerLine: ni = 64): string;
function ConvertHex(sHexString: string; iMaxBytesPerLine: integer = 1024): string;
function ResizeHexLines(sHexString: string; iMaxBytesPerLine: integer): string;

function FindMemoryByte(sl: TStrings; addr: integer): byte;
function HexToLongHexLine(s: string): string;
procedure HextoLongHex(sl: TStrings);
function GetHexStartingAddress(s: string):integer;
function ReadHexFromString(s: string; iPos, iLength: integer): integer;
function ZReadHexFromString(s: string; iPosZeroBased, iLength: integer): integer;
function GetHexDataLength(s: string): integer;
function GetHexRecordType(s: string):integer;
function GetHexEndingAddress(s: string):integer;
function GetHexData(s: string): string;
function VerifyHexChecksum(s: string): boolean;
function IsLongLine(sLine: string): boolean;
function GetHexDataBytes(sLIne: string; idx:ni;iLen:ni): TDynByteArray;
function GetHexDataByte(sLine: string; idx:ni): byte;

function HexToZXHex(sIn: string; iMaxZeroPack: ni = 8192): string;
procedure CYACDtoBin(sInFile: string; sOutFile: string);


implementation

uses sysutils;


procedure CYACDtoBin(sInFile: string; sOutFile: string);
var
  fs: TMultibufferMemoryFileStream;
  c: byte;
  t: ni;
  s: string;
  ac: ansichar;
  idx: ni;
  localbuf: array[0..4097] of byte;
  localbufidx: ni;

  procedure LocalFlush;
  begin
    stream_GuaranteeWrite(fs, @localbuf[0], localbufidx);
    localbufidx := 0;
  end;

  procedure LocalWrite(c: byte);
  begin
    localbuf[localbufidx] := c;
    inc(localbufidx);
    if localbufidx = length(localbuf) then
      LocalFlush;
  end;
begin
  localbufidx := 0;
  fs := TMultibufferMemoryFileStream.create(sOutFile, fmCreate);
  try
    sInFile := LoadfileAsString(sInFile);
    idx := 0;
    setlength(s, length(sInFile));
    for t:= 0 to length(sInFile)-1 do begin
      ac := ansichar(sInFile[t]);
      if (ac <> ansichar(':'))
      and (ac <> ansichar(#13))
      and (ac <> ansichar(#10))
      then begin
        s[idx] := sInFile[t];
        inc(idx);
      end;
    end;

    setlength(s, idx);
    sInFile := s;
    for t := 0 to (length(sInFile) div 2)-1 do begin
      if (t and $2FF) = 0 then
        Debug.Log('Convert:'+inttostr(t));

      c := strtoint('$'+zcopy(sInFile, (t*2), 2));
      LocalWrite(c);
//        stream_GuaranteeWrite(fs, @c, 1);
//      end;
    end;

    LocalFlush;
  finally
    fs.free;
  end;


end;

function ReadHexFromString(s: string; iPos, iLength: integer): integer;
var
  stemp: string;
begin
  sTemp := copy(s, iPos, iLength);

  result := strtoint('$'+sTemp);

end;

function ZReadHexFromString(s: string; iPosZeroBased, iLength: integer): integer;
begin
  result := ReadHexFromString(s, STRZ+iPosZeroBased, iLength);
end;


function HexToLongHexLine(s: string): string;overload;
begin
  result := '!00'+copy(s, 2, length(s));
end;

function GetHexDataLength(s: string): integer;
begin
  if IsLongLIne(s) then
    result := ReadHexFromString(s, 2, 4)
  else
    result := ReadHexFromString(s, 2, 2);
end;

function GetHexStartingAddress(s: string):integer;
begin
  if IsLongLIne(s) then
    result := ReadHexFromString(s,6,4)
  else
    result := ReadHexFromString(s,4,4)
end;

function GetHexRecordType(s: string):integer;
begin
  if IsLongLIne(s) then
    result := ReadHexFromString(s,10,2)
  else
    result := ReadHexFromString(s,8,2)
end;

function GetHexEndingAddress(s: string):integer;
begin
  result := GetHexStartingAddress(s)+GetHexDataLength(s);
end;

function GetHexData(s: string): string;
begin
  if IsLongLIne(s) then
    result := zcopy(s, 12-1, GetHexDataLength(s)*2)
  else
    result := zcopy(s, 10-1, GetHexDataLength(s)*2);

end;

function AddCheckSum(sLine: string): string;
var
  t: integer;
  cs: integer;
begin
  cs := 0;
  for t:= STRZ to (length(sLine)-1)+STRZ do begin
    if t mod 2 = 1 then continue;

    inc(cs, ReadHexFromString(sLine, t, 2));
  end;

  cs := cs and 255;
  cs := cs xor 255;
  cs := cs +1;
  cs := cs and 255;

  result := sLine+inttohex(cs, 2);
end;


function CombineHexLines(sLine, sNextLine: string): string;
var
  sData1, sData2: string;
begin
  sData1 := GetHexData(sLine);
  sData2 := GetHexData(sNextLine);

  result := ':'+inttohex((length(sData1)+length(sData2)) div 2, 4)+
                inttohex(GetHexStartingAddress(sLine),4)+
                copy(sLine, 10, 2)+
                sData1+
                sData2;


  result := Addchecksum(result);

end;



function ConvertHex(sHexString: string; iMaxBytesPerLine: integer = 1024): string;
var
  slSource, slTarget: TStringList;
  wStartAddr, wStarAddr2: word;
  sThisLine, sNextLine: string;
  sLine: string;
  iLine: integer;
  bIsLongHex: boolean;

  function ShouldCommit(sLine, sNextLine: string): boolean;
  var
    bConsecutive: boolean;
    iCombinedLength: integer;
    iStart, iEnd: integer;
    bDifferentTypes: boolean;
  begin
    if sLine = '' then begin
      result := false;
      exit;
    end;

    iStart := GetHexRecordType(sLine);
    iEnd := GetHexRecordType(sNextLine);
    bDifferentTypes := iStart <> iEnd;

    iStart := GetHexEndingAddress(sLine);
    iEnd := GetHexStartingAddress(sNextLine);
    bConsecutive := iStart  = iend ;
    iCombinedLength := GetHexDataLength(sLine)+GetHexDataLength(sNextLine);
    result := bDifferentTypes or (not bConsecutive) or (iCombinedLength > iMaxBytesPerLine);
  end;
begin
  slSource := TStringList.create;
  slTarget := TStringList.create;
  try
    slSource.text := sHexString;

    iLIne := 0;
    //if we CAN combine the next line then combine it

    repeat

      sNextLine := HexToLongHexLine(slSource[iLine]);
      bIsLongHex := true;

      if iLine = slSource.count-1 then begin
        if sLIne <> '' then
          slTarget.Add(sLine);
        slTarget.add(sNextLine);
        break;
      end;
      //if the working line needs to be committed then
      if ShouldCommit(sLine, sNextLine) then begin
        //commit the working line to the target string list
        slTarget.add(sLine);
        //reset the working line to blank
        if GetHexRecordType(sLine) = 01 then break;
        sLine := '';
      end else begin
        //if sLine is blank
        if sLine = '' then begin
          //set working line to current line
          sLine := sNextLine;
          inc(iLine);
        end else begin
          sLine := CombineHexLines(sLine, sNextLine);
          inc(iLine);

        end;

      end;

      //if we're done then break
//      if iLine = slSource.count then break;


    until 2+2=5;

    result := slTarget.Text;
  finally
    slSource.free;
    slTarget.free;
  end;






end;

function GetHexDataBytes(sLIne: string; idx:ni;iLen:ni): TDynByteArray;
var
  t: ni;
begin

  setlengtH(result, iLen);
  for t:= 0 to iLen-1 do begin
    result[t] := GetHexDataByte(sLine, t+idx);
  end;
end;

function GetHexDataByte(sLine: string; idx:ni): byte;
var
  bIsLongHex: boolean;
begin
  bIsLongHex := zcopy(sLine,0,1)='!';
  if bIsLongHex then
    result := ReadHexFromString(sLine, (12+(idx*2))-1, 2)
  else
    result := ReadHexFromString(sLine, (10+(idx*2))-1, 2);


end;

function IsLongLine(sLine: string): boolean;
begin
  if length(sLine) < 1 then
    raise ECritical.create('blank line in hex');
  result := zcopy(sLine, 0,1) = '!';
end;

function FindMemoryByte(sl: TStrings; addr: integer): byte;
var
  t: integer;
  iaddr: integer;
  iaddr2: integer;
  iLine: integer;
  offset: int64;
  iRecType: integer;
  bIsLongHex: boolean;
  datastart: integer;
begin
  offset := 0;
  iLine := -1;
  bIsLongHex := zcopy(sl[0],0,1)='!';
  try
    for t:= 0 to sl.count-1 do begin
      iRecType := GetHexRecordType(sl[t]);
      if iRecType = 4 then begin
        offset := ((GetHexDataByte(sl[t],0) shl 8)+GetHexDataByte(sl[t],1)) shl 16;
      end;
      iAddr := GetHexStartingAddress(sl[t]);
      iaddr2 := GetHexEndingAddress(sl[t]);
      if (addr >= iAddr) and (addr < iAddr2) then begin
        iLine := t;
      end;
    end;


    if iLine = -1 then begin
      result := 255;
      exit;
    end else begin
      iAddr := GetHexStartingAddress(sl[iLine]);
      if bIsLongHex then
        datastart := 12
      else
        datastart := 10;
      result := ReadHexFromString(sl[iLine], datastart+((addr-iAddr)*2), 2);
    end;


  finally
  end;


end;

procedure HextoLongHex(sl: TStrings);
var
  t: integer;
begin
  for t:= 0 to sl.count-1 do begin
    sl[t] := HextoLongHexLine(sl[t]);
  end;

end;

function VerifyHexChecksum(s: string): boolean;
var
  t: integer;
  check, cs, i: integer;
begin
  result := true;

  cs := 0;
  for t:= 1 to length(s)-2 do begin
    if (t mod 2) = 1 then continue;

    i := ReadHexFromString(s, t, 2);
    inc(cs,i);
  end;

  cs := cs and 255;

  check := ReadHexFromString(s, length(s)-1, 2);
  if (cs + check) and 255 <> 0 then
    raise Exception.create('checksum invalid');



end;

function BytesToString(data: TDynByteArray): string;
var
  t: ni;
begin
  result := '';
  for t:= low(data) to high(data) do begin
    result := result + inttohex(data[t],2);
  end;
end;


function ResizeHexLines(sHexString: string; iMaxBytesPerLine: integer): string;
var
  slIn, slOut: TStringlist;
  sLine: string;
  iBytes: ni;
  iRec: ni;
  iOffSet: int64;
  iLineAddr: int64;
  iRemain: ni;
  iCan,idx: ni;
  data: TdynByteArray;
  sOut: string;
  t: ni;
begin
  iOffset := 0;
  slIn := TStringlist.create;
  slOut := TStringlist.create;
  try
    slIn.text := sHexString;
    for t:= 0 to slIn.count-1 do begin
      sLine := slIn[t];
      //get record type
      iRec := GetHexRecordType(sLine);
      if iRec = 04 then begin
        iOffset := (GetHexDataByte(sLine, 0) shl 24)+(GetHexDataByte(sLIne, 1) shl 16);
        slOut.Add(sLine);
        continue;//there is no data in this line and its address is irrelevant
      end;

      if iRec = 05 then begin
        slOut.Add(sLine);
        continue;//there is no data in this line and its address is irrelevant
      end;

      if iRec = 01 then begin
        slOut.Add(sLine);
        continue;//there is no data in this line and its address is irrelevant
      end;

      iLineAddr := iOffset+GetHexStartingAddress(sLIne);

      //determine number of bytes in the line
      iBytes := GetHexDataLength(sLIne);

      iRemain := iBytes;
      idx := 0;
      while iRemain > 0 do begin
        iCan := lesserof(iRemain,8);
        data := GetHexDataBytes(sLine, idx, iCan);

        sOut := ':'+IntToHex(iCan,2)+inttohex(GetHexStartingAddress(sLine)+idx,4)+IntToHex(iRec,2);
        sOut := sOut + bytestostring(data);
        sOut := AddChecksum(sOut);
        slOut.Add(sOut);

        inc(idx, iCan);
        dec(iRemain,iCan);
      end;
    end;
    result := slOut.text;
  finally
    slin.free;
    slout.free;
  end;


end;


function IsAllZeros(sLine: string): boolean;
var
  b: TDynbyteArray;
  t: ni;
begin
  result := false;
  if GetHexRecordType(sLine) <> 0 then
    exit;
  if GetHexDataLength(sLine) = 0 then
    exit;
  b := GetHexDataBytes(sLIne, 0, GetHexDataLength(sLIne));
  for t := 0 to high(b) do begin
    if b[t] <> 0 then
      exit;
  end;

  //if we make it here... we're all ZEROS
  result := true;


end;

function BinToHex(sFile: string; bytesPerLine: ni): string;
//converts CYACD to standard intel hex file (NOT LONG HEX)
var
  fs: TFileStream;
  sLine: string;
  a: array of byte;
  t: ni;
  addr: int64;
  distToBoundary: int64;
  lastsegment: int64;
  segment: int64;
begin
  //CYACD (IN)
  //(SKIP FIRSTLINE)
  //[ArrayID -1 byte] [RowNumber-2 bytes][Data Len-2bytes][Data][Checksum 1-byte]

  //INTEL HEX (OUT)
  //[len] [addr 2-bytes] [record type] [data] [checksum]

  addr := 0;
  lastSegment := -1;
  segment := -1;

  fs := TFileStream.create(sFile, fmOpenRead+fmShareDenyNone);
  try
    while fs.position < fs.size do begin
      disttoboundary := $10000 - (addr and ($FFFF));

      lastSegMent := segment;
      segment := addr shr 16;

      //IF THE SEGMENT ADDRESS CHANGED, ADD A SEGMENT RECORD
      if segment <> lastSegment then begin
        sLIne := AddCheckSum(':02000004'+inttohex(segment, 4));
        result := result + sLine+NEWLINE;
      end;

      setlength(a, lesserof(distToBoundary, lesserof(bytesPerLine, fs.size-fs.position)));
      t := 0;
      stream_GuaranteeRead(fs, @a[0], length(a));
      sLine := ':'+
          IntToHex(length(a),2)+//LEN
          IntToHex(addr and $ffff,4)+//ADDR (note, use segment addressing for higher addresses)
          '00'+//Record type 0 = data
          MemoryToString(@a[0], length(a));

      sLine := AddCheckSum(sLine);

      result := result + (sLine+NEWLINE);
      inc(addr, length(a));
    end;

    sLine := ':00000001FF';//end of file
    result := result + sLine+NEWLINE;

  finally
    fs.free;
  end;

end;

function HexToZXHex(sIn: string; iMaxZeroPack: ni = 8192): string;
  //WHAT IS A ZXHEX!?
//-- adds new line-type to hex file, 0x80 record type indicates a bunch of 0s
type
  TZeros = record
    startaddr: int64;
    count: int64;
  end;
var
  t: ni;
  slOut, slIn: TStringlist;
  sPreviousLIne,sLIne: string;
  prevaddr, prevlen: int64;
  prevtype: ni;
  thisaddr: int64;
  len: int64;
  rectype: ni;
  sOutLine: string;
  bCanMerge: boolean;
  segment: int64;
  b: TDynByteArray;

  z: TZeros;
  procedure CommitZeros;
  var
    ii,bbb,bct,bc: ni;

  begin
    //commit zeros if there are some
    if (z.startaddr>=0) then begin
      if z.count < 256 then
        bc := 1
      else
      if z.count > 65535 then
        bc := 3
      else
        bc := 2;

      setlength(b, 4+bc);

      b[0] := bc;
      b[1] := (z.startaddr shr 8) and $ff;//addr seg offset
      b[2] := (z.startaddr shr 0) and $ff;//addr seg offset
      b[3] := $0A;//record type

      for bct := bc-1 downto 0 do begin
        ii := 4+((bc-1)-bct);
        bbb := (z.count shr (8*(bct))) and $ff;
        b[ii] := bbb;
      end;
      slOut.add(AddCheckSum(':'+MemoryToString(b)));
    end;
    z.startaddr := -1;
  end;
begin
  prevaddr := 0;
  prevlen := 0;
  prevtype := 0;
  z.startaddr := -1;
  z.count := 0;
  bCanMerge := false;
  slOut := Tstringlist.create;
  try
    slIn := STringToStringList(sIn);
    try
      for t:=0 to slIn.count-1 do
      begin
        //get the line
        sLine := slIn[t];
        rectype := GetHexRecordType(sLIne);
        thisaddr := GetHexStartingAddress(sLIne);
        len := GetHexDataLength(sLine);
        if (thisaddr and $ffff) = $bfC0 then
          debug.consolelog('here');
        case rectype of
          0:
          begin
            if not IsAllZeros(sLIne) then
            begin
              CommitZeros;
              slOut.Add(sLine);//add as is
            end else
            begin
              if (prevaddr+prevlen) <> thisaddr then begin
                CommitZeros;
              end;
              if prevtype <> 0 then begin
                CommitZeros;
              end;
              if z.count >= (2048*4) then begin
                CommitZeros;
              end;

              //if we don't have a z, init
              if z.startaddr < 0 then begin
                z.startaddr := thisaddr;
                z.count := 0;
              end;
              //update zeros
              inc(z.count, len);
            end;
          end;
          1:
          begin
            CommitZeros;
            slOut.add(sLine);//add as is
          end;
          4:
          begin
            CommitZeros;
            b := GetHexDataBytes(sLine, 0, 2);
            segment := (b[0] shl 24) or (b[1] shl 16);
            slOut.add(sLIne);//add as is
          end;
        end;
        prevaddr := thisaddr;
        prevlen := len;
        prevtype := rectype;
      end;

      CommitZeros;
      result:= slOut.text;


    finally
      slIn.free;
    end;
    result := slOUt.text;
  finally
    slOut.free;
  end;
end;

end.
