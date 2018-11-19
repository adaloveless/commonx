unit Compressor_Simple;

interface

uses
  typex, systemx, numbers;

CONST
  COMP_ZEROS = $40;
  COMP_REPEAT = $00;
  COMP_LITERAL = $80;
  COMP_STRIDE = $C0;

type
  TCompRec = record
    reps: ni;
    value: byte;
    procedure Init;
    procedure Step;
  end;
  PCompRec = ^TCompRec;  //pointer to TCompRec type definition
  TCompRecArray = array of TCompRec;

function CompressBytes(bytes: TDynByteArray; iStride: ni; iOffset: ni): TDynByteArray;overload;
function CompressBytes(bytes: TDynByteArray): TDynByteArray;overload;
function CompressBytesLiteral(bytes: TDynByteArray): TDynByteArray;
function InterlaceBytes(bytes: TDynByteArray; iStride: ni; iOffset: ni): TDynByteArray;
function GetCompRecs(bytes: TDynByteArray): TCompRecArray;
function InterlaceIdx(idx: ni; iStride: ni; iOffset: ni; iWidth: ni): ni;
function DecompressBytes(src:TDynByteArray): TDynByteArray;

implementation


function GetCompRecs(bytes: TDynByteArray): TCompRecArray;
var
  outidx: ni;
  inidx: ni;
  cur: TCompRec;
begin
  inidx := low(bytes);
  outidx := 0;
  setlength(result, length(bytes));//temporary, reset later

  while inidx <= high(Bytes) do begin
    //start with the current byte
    cur.Init;
    cur.value := bytes[inidx];
    cur.reps := 0;
    //walk forward until byte changes or end of line
    repeat
      cur.Step;
      inc(inidx);
    until (inidx > high(bytes)) or (bytes[inidx]<>cur.value);

    //add to result
    result[outidx] := cur;
    inc(outidx);
  end;

  setlength(result, outidx);




end;

function CompressBytes(bytes: TDynByteArray; iStride: ni; iOffset: ni): TDynByteArray;overload;
var
  recs: TCompRecArray;
  inidx: ni;
  outidx: ni;
  pcr: PCompRec;
  lit: byte;
  t: ni;
  bLast: boolean;
  literal: TDynByteArray;
  litidx: ni;
  bStop: boolean;
  procedure CommitLiteralArray;
  var
    tt: ni;
  begin
    if litidx = 0 then exit;
    if bStop then exit;
    result[outidx] := COMP_LITERAL or litidx;
    inc(outidx);
    for tt := 0 to litidx-1 do begin
      result[outidx] := literal[tt];
      inc(outidx);
      if (outidx >= length(bytes)) then begin
        bStop := true;
        break;
      end;
    end;

    litidx := 0;

  end;
begin
  LITIDX := 0;
  bStop := false;
  setlength(literal, length(bytes));
  bytes := Interlacebytes(bytes, iStride, iOffset);
  recs := GetCompRecs(bytes);
  setlength(result, length(bytes));//temporary

  //move through the comp recs, encode output
  inidx := low(recs);
  outidx := low(result);

  if (iStride > 1) then begin
    result[outidx] := COMP_STRIDE or (iStride-1) or (iOffset shl 3);
    inc(outidx);
    result[outidx] := length(bytes);
    inc(outidx);
  end;

  //go through the comp recs
  while inidx <= high(recs) do begin
    bLast := inidx >= high(recs);
    pcr := @recs[inidx];

    if ((pcr.reps > 1) and (pcr.value=0)) then begin
      CommitLiteralArray;//first commit a literal (if exists)
      result[outidx] := pcr.reps or COMP_ZEROS;
      inc(outidx);
    end else
    if (pcr.reps > 2) then begin
      CommitLiteralArray;//first commit a literal (if exists)
      result[outidx] := pcr.reps or COMP_REPEAT;
      inc(outidx);
      result[outidx] := pcr.value;
      inc(outidx);
    end else begin
      //render to literal array
      for t:= 0 to pcr.reps-1 do begin
        literal[litidx] := pcr.value;
        inc(litidx);
      end;
    end;

    if (outidx >= length(bytes)) then begin
      bStop := true;
      break;
    end;
    inc(inidx);
  end;

  CommitLiteralArray;//if there are any remaining literals, commit them

  //if we stopped due to overflow, kill results
  if bStop then
    setlength(result,0)
  else
  //else
    setlength(result, outidx);



end;

function CompressBytes(bytes: TDynByteArray): TDynByteArray;overload;
var
  s,o: ni;
  a: TDynByteArray;
begin
  //start with a literal compression
  result := CompressBytesLiteral(bytes);

  //go through possible compressions
  for s:=1 to lesserof(8,length(bytes)-1) do begin
    for o := 0 to 0{(s-1)} do begin
      a := CompressBytes(bytes, s,o);
      //if this one is better, use this one as the result
      if (length(a) > 0) and (length(a) < length(result)) then begin
        result := a;
      end;
    end;
  end;
end;


function CompressBytesLiteral(bytes: TDynByteArray): TDynByteArray;
begin
  setlength(result, length(bytes)+1);
  result[0] := length(bytes) or COMP_LITERAL;
  movemem32(@result[1], @bytes[0], length(bytes));
end;

function InterlaceIdx(idx: ni; iStride: ni; iOffset: ni; iWidth: ni): ni;
var
  i: ni;
begin
  i := 0;
  result := iOffset;
  while i < idx do begin
    result := result + iStride;
    if result >= iWidth then
    begin
      dec(result, iWidth);
    end;
    inc(i);
  end;


end;

function InterlaceBytes(bytes: TDynByteArray; iStride: ni; iOffset: ni): TDynByteArray;
var
  t,x,w: ni;
  remainder: ni;
begin
  w := length(bytes);
  setlength(result, w);
  remainder := iStride - (w mod istride);
  for t:=0 to length(bytes)-1 do begin
    x := InterlaceIdx(t, iStride, iOffset, w);
    result[t] := bytes[x];
  end;
end;

{ TCompRec }

procedure TCompRec.Init;
begin
  reps := 0;
  value := 0;
end;

procedure TCompRec.Step;
begin
  inc(reps);
end;

function DecompressBytes(src:TDynByteArray): TDynByteArray;
var
  x: ni;
  sz: ni;
  inidx, outidx,stride,off,cx,wid,max_stride_idx: ni;
  b,nib,lowbits: byte;

  function STRIDE_IDX(_a_:ni): ni;
  begin
    result := interlaceIdx(_a_, stride, off, wid);
    if result > max_stride_idx then
      max_stride_idx := result;
  end;

begin
  sz := length(src);
  setlength(result, 256);

	inidx := 0;
	outidx := 0;
	stride := 1;
	off := 0;

	wid := 45;//only becomes relevant if stride is changed to > 1
	max_stride_idx := 0;// sorry about this hack... the only way to determine the actual length of the data is to track the highest index that was decompressed (which can be interlaced)

	while (inidx < sz) do
  begin
		b := src[inidx]; inc(inidx);
		nib := b and $C0;
		lowbits := b and $3f;

		case nib of
			COMP_STRIDE:
			begin
				stride := (lowbits and 7)+1;
				off := (lowbits and (7 shl 3)) shr 3;
				wid := src[inidx];
        inc(inidx);

			end;
			COMP_LITERAL:
			begin
				cx := lowbits;
				while (cx>0) do
				begin
					result[STRIDE_IDX(outidx)] := src[inidx];
          INC(outidx);inc(inidx);
					dec(cx);
				end;
			end;
			COMP_ZEROS:
			begin
				cx := lowbits;
				while (cx>0) do
				begin
					result[STRIDE_IDX(outidx)] := 0;
          INC(outidx);
					dec(cx);
				end;
			end;
			COMP_REPEAT:
			begin
				cx := lowbits;
				while (cx>0) do
        begin
					result[STRIDE_IDX(outidx)] := src[inidx];inc(outidx);
					dec(cx);
        end;
        inc(inidx);
			end

    end;


  end;
  setlength(result, max_stride_idx+1);

end;

end.
