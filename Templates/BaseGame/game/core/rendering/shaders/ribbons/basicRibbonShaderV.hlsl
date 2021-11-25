#include "../shaderModel.hlsl"
#include "../shaderVertex.hlsl"
 
struct a2v
{
   TORQUE_LOCATION(kPosition) float3 position        : POSITION;
   TORQUE_LOCATION(kColor) float4 color           : COLOR;
   TORQUE_LOCATION(kNormal) float3 normal          : NORMAL;
   TORQUE_LOCATION(kTexCoord0) float2 texCoord        : TEXCOORD0;
   TORQUE_LOCATION(kTexCoord1) float2 shiftdata       : TEXCOORD1;
};

struct v2f
{
   float4 hpos : SV_Position;
   TORQUE_LOCATION(0) float4 color           : COLOR;
   TORQUE_LOCATION(1) float2 texCoord        : TEXCOORD0;
   TORQUE_LOCATION(2) float2 shiftdata       : TEXCOORD1;
};
 
uniform float4x4 modelview;
uniform float3   eyePos;
 
v2f main(a2v IN)
{
    v2f OUT;
 
    OUT.hpos = mul(modelview, float4(IN.position, 1.0));
    OUT.color = IN.color;
    OUT.texCoord = IN.texCoord;
    OUT.shiftdata = IN.shiftdata;
   
    return OUT;
}