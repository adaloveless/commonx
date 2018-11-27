void vs_main(
  float2 InPos: POSITION0,
  float2 InTex: TEXCOORD0,
  float2 InTex: TEXCOORD1,
  float4 InCol: COLOR0,
  out float4 OutPos: SV_POSITION,
  out float2 OutTex: TEXCOORD2,
  out float2 OutTex: TEXCOORD3,
  out float4 OutCol: COLOR0)
{
  OutPos = float4(InPos, 0.0, 1.0);
  OutTex = InTex;
  OutCol = InCol;

}
