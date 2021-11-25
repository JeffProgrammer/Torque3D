
//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~~//
// Arcane-FX - VERTEX SHADER
//
// afxZodiac_Polysoup_P.hlsl
//   This is the pixel shader for rendering zodiacs on polysoup models.
//   
// Copyright (C) Faust Logic, Inc.
//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~~//

#include "../shaderModel.hlsl"
#include "../shaderVertex.hlsl"

struct ConnectData
{
   float4 hpos : SV_Position;
   TORQUE_LOCATION(0) float2 texCoord     : TEXCOORD0;
};

uniform float4x4 modelView;

//-----------------------------------------------------------------------------
// Main
//-----------------------------------------------------------------------------
ConnectData main( VertexIn_PCT IN )
{
   ConnectData OUT;

   OUT.hpos = mul(modelView, float4(IN.pos,1.0));
   OUT.texCoord = IN.uv0;
    
   return OUT;
}

//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~~//
