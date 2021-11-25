#define IN_HLSL
#include "../torque.hlsl"
 

TORQUE_UNIFORM_SAMPLER2D(ribTex, 0);

struct v2f
{
   float4 hpos            : SV_Position;
   float4 color           : COLOR0;
   float2 texCoord        : TEXCOORD0;
   float2 shiftdata       : TEXCOORD1;
};
 
float4 main(v2f IN) : SV_Target0
{
   float4 Tex = TORQUE_TEX2D(ribTex,IN.texCoord);
	Tex.a *= IN.color.a;
	return hdrEncode(Tex);
}