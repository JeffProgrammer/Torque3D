//-----------------------------------------------------------------------------
// Copyright (c) 2012 GarageGames, LLC
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
//-----------------------------------------------------------------------------

#include "shaderModel.hlsl"

struct Vert
{
   TORQUE_LOCATION(kPosition) float3 position  : POSITION;
   TORQUE_LOCATION(kNormal) float3 normal      : NORMAL;
   TORQUE_LOCATION(kTangent) float3 T          : TANGENT;
   TORQUE_LOCATION(kColor) float4 color        : COLOR;
   TORQUE_LOCATION(kTexCoord0) float2 texCoord : TEXCOORD0;
};

struct Conn
{
	float4 position : SV_Position;
   TORQUE_LOCATION(0) float4 color : COLOR0;
	TORQUE_LOCATION(1) float2 texCoord	: TEXCOORD0;
	TORQUE_LOCATION(2) float fade : TEXCOORD1;
};

uniform float4x4 modelview;
uniform float shadowLength;
uniform float3 shadowCasterPosition;

Conn main( Vert In )
{
    Conn Out;

    // Decals are in world space.
    Out.position = mul( modelview, float4( In.position, 1.0 ) );
 
    Out.color = In.color;
    Out.texCoord = In.texCoord;
 
    float fromCasterDist = length( In.position - shadowCasterPosition ) - shadowLength;   
    Out.fade = 1.0 - saturate( fromCasterDist / shadowLength );
    
   return Out;
}
