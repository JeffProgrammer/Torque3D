#include "../shaderModel.hlsl"
 
struct v2f
{
   float4 hpos : SV_Position;
   TORQUE_LOCATION(0) float4 color           : COLOR0;
   TORQUE_LOCATION(1) float2 texCoord        : TEXCOORD0;
   TORQUE_LOCATION(2) float2 shiftdata       : TEXCOORD1;
};
 
float4 main(v2f IN) : SV_Target0
{
   float fade = 1.0 - abs(IN.shiftdata.y - 0.5) * 2.0;
   IN.color.xyz = IN.color.xyz + pow(fade, 4) / 10;
   IN.color.a = IN.color.a * fade;
   return IN.color;
}