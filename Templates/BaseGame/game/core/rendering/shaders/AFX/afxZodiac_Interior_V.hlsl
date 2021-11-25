
//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~~//
// Arcane-FX - VERTEX SHADER
//
// afxZodiac_Interior_P.hlsl
//   This is the pixel shader for rendering zodiacs on interiors.
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

//-----------------------------------------------------------------------------
// Main
//-----------------------------------------------------------------------------
ConnectData main( VertexIn_PCT IN,
                  uniform float4x4 modelView : register(C0)
                  )
{
   ConnectData OUT;

   OUT.hpos = mul(modelView, float4(IN.pos,1.0)); 
   OUT.texCoord = IN.uv0;
    
   return OUT;
}

//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~//~~~~~~~~~~~~~~~~~~~~~//
