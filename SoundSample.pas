unit SoundSample;

interface

uses
  systemx, typex, sysutils;

type
{$IFDEF CPUX64}
  FLoatSample = double;

{$ELSE}
  FLoatSample = single;
  {$DEFINE PURE_TSTEREOSOUNDSAMPLE}
{$ENDIF}


  PStereoSoundSample = pointer;

  TRawSoundArrayPOinterMono = array of smallint;
  TRawSoundArrayPOinterStereo = array of smallint;

  TStereoArray<T> = array[0..1] of T;


  TStereoSoundSample = packed record
    Left: floatsample;
    Right: floatsample;
    class operator add(const a,b: TStereoSoundSample): TStereoSoundSample;{$IFDEF PURE_TSTEREOSOUNDSAMPLE}inline;{$ENDIF}
    class operator subtract(const a,b: TStereoSoundSample): TStereoSoundSample;{$IFDEF PURE_TSTEREOSOUNDSAMPLE}inline;{$ENDIF}
    class operator multiply(const a,b: TStereoSoundSample): TStereoSoundSample;overload;{$IFDEF PURE_TSTEREOSOUNDSAMPLE}inline;{$ENDIF}
    class operator multiply(const a: TStereoSoundSample; const scalar: double): TStereoSoundSample;overload;{$IFDEF PURE_TSTEREOSOUNDSAMPLE}inline;{$ENDIF}
    class operator divide(const a,b: TStereoSoundSample): TStereoSoundSample;{$IFDEF PURE_TSTEREOSOUNDSAMPLE}inline;{$ENDIF}
    procedure InterpolateFrom(a,b: TStereoSoundSample; iFade: double);{$IFDEF PURE_TSTEREOSOUNDSAMPLE}inline;{$ENDIF}
    procedure InterpolateAndScale(a,b: TStereoSoundSample; iFade: double; iScale: double);overload;{$IFDEF PURE_TSTEREOSOUNDSAMPLE}inline;{$ENDIF}
    procedure InterpolateAndScale(a,b, iFade, iScale: TStereoSoundSample);overload;{$IFDEF PURE_TSTEREOSOUNDSAMPLE}inline;{$ENDIF}
    procedure InterpolateAndScale(a,b, Fade, Scale: PStereoSoundSample);overload;{$IFDEF PURE_TSTEREOSOUNDSAMPLE}inline;{$ENDIF}
    procedure Init;
    procedure Clip;
    procedure FromStereoArray_8i(a: TStereoArray<shortint>);
    procedure FromStereoArray_16i(a: TStereoArray<smallint>);
    procedure FromStereoArray_24i(a: TStereoArray<uint24>);
    procedure FromStereoArray_32i(a: TStereoArray<integer>);
    procedure FromStereoArray_64i(a: TStereoArray<int64>);
    procedure FromStereoArray_32f(a: TStereoArray<single>);
    procedure FromStereoArray_64f(a: TStereoArray<double>);
    function Amp: floatsample;
  end;

implementation


{ TStereoSoundSample }

class operator TStereoSoundSample.add(const a,
  b: TStereoSoundSample): TStereoSoundSample;
{$IFDEF PURE_TSTEREOSOUNDSAMPLE}
begin
  result.Left := a.Left+b.Left;
  result.Right := a.Right+b.Right;
end;
{$ELSE}
asm
  .NOFRAME
  //begin
{  push rbp
  push rdi
  push rsi
  sub rsp,$20
  mov rbp,rsp
  mov rsi,rdx
  lea rdi,[rbp+$10]
  movsq
  movsq
  lea rdi,[rbp+$00]
  mov rsi,r8
  movsq
  movsq
  mov [rbp+$40],rcx}

{$IFDEF SLOWASM}
  //SoundTools.pas.3952: result.Left := a.Left+b.Left;
  //mov rax,[rbp+$40]
  movsd xmm4,qword ptr [a.Left] //[rbp+$10]
  addsd xmm4,qword ptr [b.Left]//[rbp+$00]
  movsd qword ptr [result.Left],xmm4

  //SoundTools.pas.3953: result.Right := a.Right+b.Right;
  //mov rax,[rbp+$40]
  movsd xmm4,qword ptr [a.Right]
  addsd xmm4,qword ptr [b.Right]
  movsd qword ptr [result.Right],xmm4
{$ELSE}
{$IFDEF PRESERVE_XMM}
  push xmm4
  push xmm5
{$ENDIF}
  movupd xmm4,[a]
  movupd xmm5,[b]
  addpd xmm4,xmm5
  movupd [result],xmm4
{$IFDEF PRESERVE_XMM}
  pop xmm4
  pop xmm5
{$ENDIF}
{$ENDIF}

  //SoundTools.pas.3954: end;
  //mov rax,[rbp+$40]
{  lea rsp,[rbp+$20]
  pop rsi
  pop rdi
  pop rbp}
  ret
end;
{$ENDIF}

procedure TStereoSoundSample.InterpolateAndScale(a, b: TStereoSoundSample;
  iFade, iScale: double);
var
  f,s: TStereoSoundSample;
{$IFDEF PURE_TSTEREOSOUNDSAMPLE}
begin
  f.Left := iFade;
  f.right := f.left;
  s.Left := iSCale;
  s.Right := iSCale;
  self := (((b-a) * (f))+a) * s;
end;
{$ELSE}
asm
  .NOFRAME
//  mov r10, self
//  and r10, 15
//  jz @aligned
  movsd [f.left], xmm3   //fade
  movsd [f.right], xmm3
//  movsd xmm7, [iSCale]
  movsd [s.left], xmm0  //scale
  movsd [s.right], xmm0

  movupd xmm5,[b]
  movupd xmm4,[a]
  subpd xmm5,xmm4

  movupd xmm6,[f]
  mulpd xmm5,xmm6
  addpd xmm5,xmm4

  movupd xmm7,[s]
  mulpd xmm5,xmm7

  movupd [self],xmm5
  jmp @ret
@aligned: //disabled
  movsd [f.left], xmm3
  movsd [f.right], xmm3
//  movsd xmm7, [iSCale]
  movsd [s.left], xmm0
  movsd [s.right], xmm0
//  movapd xmm7,[s]
//  movapd xmm4,[a]
  movapd xmm5,[b]
//  movapd xmm6,[f]
  subpd xmm5,[a]
  mulpd xmm5,[f]
  addpd xmm5,[a]
  mulpd xmm5,[s]
  movapd [self],xmm5
@ret:

end;
{$ENDIF PURE_TSTEREOSOUNDSAMPLE}

procedure TStereoSoundSample.InterpolateAndScale(a, b, iFade,
  iScale: TStereoSoundSample);
{$IFDEF PURE_TSTEREOSOUNDSAMPLE}
begin
  self := (((b-a) * (iFade))+a) * iSCale;
end;
{$ELSE}
asm
  .NOFRAME

  movupd xmm4,[a]
  movupd xmm5,[b]
  subpd xmm5,xmm4
  movupd xmm6,[iFade]
  mulpd xmm5,xmm6
  addpd xmm5,xmm4
  movupd xmm7,[iScale]
  mulpd xmm5,xmm7
  movupd [self],xmm5
  jmp @ret
@aligned: //disabled
  movapd xmm5,[b]
  subpd xmm5,[a]
  mulpd xmm5,[iFade]
  addpd xmm5,[a]
  mulpd xmm5,[iScale]
  movapd [self],xmm5
@ret:

end;
{$ENDIF PURE_TSTEREOSOUNDSAMPLE}

function TStereoSoundSample.Amp: floatsample;
begin
  result := (Left+right)*0.5;
end;

procedure TStereoSoundSample.Clip;
var
  l,r: FloatSample;
begin
  l := Left;
  r := right;
  if l < -1.0 then l := -1.0;
  if r < -1.0 then r := -1.0;
  if l > 1.0 then r := 1.0;
  if r > 1.0 then r := 1.0;
  Left := l;
  Right := r;
end;

procedure TStereoSoundSample.FromStereoArray_16i(a: TStereoArray<smallint>);
begin
  Left := a[0] /32767;
  Right := a[1] / 32767;


end;


procedure TStereoSoundSample.FromStereoArray_24i(a: TStereoArray<uint24>);
begin
  Left := a[0].toint64 / int64($7FFFFF);
  Right := a[1].toint64 / int64($7FFFFF);
end;

procedure TStereoSoundSample.FromStereoArray_32f(a: TStereoArray<single>);
begin
  left := a[0];
  right := a[1];
end;

procedure TStereoSoundSample.FromStereoArray_32i(a: TStereoArray<integer>);
begin
  left := ni(a[0])/ni($7fffffff);
  right := ni(a[1])/ni($7fffffff);
end;

procedure TStereoSoundSample.FromStereoArray_64f(a: TStereoArray<double>);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TStereoSoundSample.FromStereoArray_64i(a: TStereoArray<int64>);
begin
  left := int64(a[0])/int64($7fffffffffffffff);
  right := int64(a[1])/int64($7fffffffffffffff);

end;

procedure TStereoSoundSample.FromStereoArray_8i(a: TStereoArray<shortint>);
begin
  left := ni(a[0])/ni($7f);
  right := ni(a[1])/ni($7f);
end;

class operator TStereoSoundSample.divide(const a,
  b: TStereoSoundSample): TStereoSoundSample;
{$IFDEF PURE_TSTEREOSOUNDSAMPLE}
begin
  result.Left := a.Left/b.Left;
  result.Right := a.Right/b.Right;
end;

{$ELSE}
asm
.NOFRAME
{$IFDEF SLOWASM}
  //SoundTools.pas.3952: result.Left := a.Left+b.Left;
  //mov rax,[rbp+$40]
  movsd xmm4,qword ptr [a.Left] //[rbp+$10]
  addsd xmm4,qword ptr [b.Left]//[rbp+$00]
  movsd qword ptr [result.Left],xmm4

  //SoundTools.pas.3953: result.Right := a.Right+b.Right;
  //mov rax,[rbp+$40]
  movsd xmm4,qword ptr [a.Right]
  addsd xmm4,qword ptr [b.Right]
  movsd qword ptr [result.Right],xmm4
{$ELSE}
{$IFDEF PRESERVE_XMM}
  push xmm4
  push xmm5
{$ENDIF}
  movupd xmm4,[a]
  movupd xmm5,[b]
  divpd xmm4,xmm5
  movupd [result],xmm4
{$IFDEF PRESERVE_XMM}
  pop xmm4
  pop xmm5
{$ENDIF}
{$ENDIF}
  ret
end;
{$ENDIF}

procedure TStereoSoundSample.InterpolateFrom(a, b: TStereoSoundSample;
  iFade: double);
{$IFDEF PURE_TSTEREOSOUNDSAMPLE}
var
  f,s: TStereoSoundSample;
begin
  f.Left := iFade;
  f.right := f.left;
  self := ((b-a) * (f))+a;
end;
{$ELSE}
var
  f: TStereoSoundSample;
asm
  .NOFRAME
  movsd [f.left], xmm3
  movsd [f.right], xmm3
  movupd xmm4,[a]
  movupd xmm5,[b]
  movupd xmm6,[f]
  subpd xmm5,xmm4
  mulpd xmm5,xmm6
  addpd xmm5,xmm4
  movupd [self],xmm5
end;
{$ENDIF PURE_TSTEREOSOUNDSAMPLE}



class operator TStereoSoundSample.multiply(const a: TStereoSoundSample; const scalar: double): TStereoSoundSample;
{$IFDEF PURE_TSTEREOSOUNDSAMPLE}
begin
  result.Left := a.Left*scalar;
  result.Right := a.Right*scalar;
end;
{$ELSE}
var
  b: TStereoSoundSample;
begin
  b.Left := scalar;
  b.Right := scalar;
  result := a*b;
end;
{$ENDIF}


class operator TStereoSoundSample.multiply(const a,
  b: TStereoSoundSample): TStereoSoundSample;
{$IFDEF PURE_TSTEREOSOUNDSAMPLE}
begin
  result.Left := a.Left*b.Left;
  result.Right := a.Right*b.Right;
end;
{$ELSE}
asm
  .NOFRAME
{$IFDEF SLOWASM}
  //SoundTools.pas.3952: result.Left := a.Left+b.Left;
  //mov rax,[rbp+$40]
  movsd xmm4,qword ptr [a.Left] //[rbp+$10]
  addsd xmm4,qword ptr [b.Left]//[rbp+$00]
  movsd qword ptr [result.Left],xmm4

  //SoundTools.pas.3953: result.Right := a.Right+b.Right;
  //mov rax,[rbp+$40]
  movsd xmm4,qword ptr [a.Right]
  mulsd xmm4,qword ptr [b.Right]
  movsd qword ptr [result.Right],xmm4
{$ELSE}
{$IFDEF PRESERVE_XMM}
  push xmm4
  push xmm5
{$ENDIF}
  movupd xmm4,[a]
  movupd xmm5,[b]
  mulpd xmm4,xmm5
  movupd [result],xmm4
{$IFDEF PRESERVE_XMM}
  pop xmm4
  pop xmm5
{$ENDIF}
{$ENDIF}
  ret
end;
{$ENDIF}

class operator TStereoSoundSample.subtract(const a,
  b: TStereoSoundSample): TStereoSoundSample;
{$IFDEF PURE_TSTEREOSOUNDSAMPLE}
begin
  result.Left := a.Left-b.Left;
  result.Right := a.Right-b.Right;
end;
{$ELSE}
asm
  .NOFRAME
{$IFDEF SLOWASM}
  //SoundTools.pas.3952: result.Left := a.Left+b.Left;
  //mov rax,[rbp+$40]
  movsd xmm4,qword ptr [a.Left] //[rbp+$10]
  addsd xmm4,qword ptr [b.Left]//[rbp+$00]
  movsd qword ptr [result.Left],xmm4

  //SoundTools.pas.3953: result.Right := a.Right+b.Right;
  //mov rax,[rbp+$40]
  movsd xmm4,qword ptr [a.Right]
  addsd xmm4,qword ptr [b.Right]
  movsd qword ptr [result.Right],xmm4
{$ELSE}
{$IFDEF PRESERVE_XMM}
  push xmm4
  push xmm5
{$ENDIF}
  movupd xmm4,[a]
  movupd xmm5,[b]
  subpd xmm4,xmm5
  movupd [result],xmm4
{$IFDEF PRESERVE_XMM}
  pop xmm4
  pop xmm5
{$ENDIF}
{$ENDIF}
  ret
end;
{$ENDIF}


procedure TStereoSoundSample.InterpolateAndScale(a, b, Fade,
  Scale: PStereoSoundSample);
{$IFDEF PURE_TSTEREOSOUNDSAMPLE}
type
  PT = ^TStereoSoundSample;
begin
  self := (((PT(b)^-PT(a)^) * (PT(fade)^))+PT(a)^) * PT(scale)^;
end;
{$ELSE}
asm
  movupd xmm6, [a]
  movupd xmm7, [b]
  movupd xmm8, [fade]
  mov rax, scale
  movupd xmm9, [rax]
  subpd xmm7,xmm6
  mulpd xmm7, xmm8
  addpd xmm7, xmm6
  mulpd xmm7, xmm9
  movupd [self],xmm7


end;
{$ENDIF}


procedure TStereoSoundSample.Init;
begin
  fillmem(PByte(@self), sizeof(self), 0);
end;



end.
