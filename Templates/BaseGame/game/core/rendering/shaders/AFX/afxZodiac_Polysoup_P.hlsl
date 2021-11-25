
//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~~//
// Arcane-FX - PIXEL SHADER
//
// afxZodiac_Polysoup_P.hlsl
//   This is the pixel shader for rendering zodiacs on polysoup models.
//   
// Copyright (C) Faust Logic, Inc.
//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~~//

#include "../shaderModel.hlsl"

struct ConnectData
{
   float4 hpos : SV_Position;
   TORQUE_LOCATION(0) float2 texCoord     : TEXCOORD0;
};

TORQUE_UNIFORM_SAMPLER2D(zodiacMap,0);
uniform float4    zodiacColor;

float4 main( ConnectData IN ) : SV_Target0
{
   float4 outColor = zodiacColor*TORQUE_TEX2D(zodiacMap, IN.texCoord);   
   return outColor;
}

//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~~//
