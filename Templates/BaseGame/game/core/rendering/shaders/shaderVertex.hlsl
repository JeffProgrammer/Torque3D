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

#ifndef TORQUE_SHADERVERTEX_H
#define TORQUE_SHADERVERTEX_H

#include "shaderModel.hlsl"
//------------------------------------------------------------------------------
// Vertex Attribute Locations
// must map to c++ side - TODO use shader interop file to share with hlsl<->cpp
//------------------------------------------------------------------------------
static const int kPosition = 0;
static const int kNormal = 1;
static const int kColor = 2;
static const int kTangent = 3;
static const int kTangentW = 4;
static const int kBinormal = 5;
static const int kTexCoord0 = 6;
static const int kTexCoord1 = 7;
static const int kTexCoord2 = 8;
static const int kTexCoord3 = 9;
static const int kTexCoord4 = 10;
static const int kTexCoord5 = 11;
static const int kTexCoord6 = 12;
static const int kTexCoord7 = 13;
static const int kTexCoord8 = 14;
static const int kTexCoord9 = 15;
static const int kBlendWeight0 = kTexCoord6;
static const int kBlendIndex0 = kTexCoord2;

//------------------------------------------------------------------------------
// Vertex Input Structures
// These structures map to FVFs/Vertex Declarations in Torque. See gfxStructs.h
//------------------------------------------------------------------------------

struct VertexIn_P
{
   TORQUE_LOCATION(kPosition) float3 pos     : POSITION;
};

struct VertexIn_PT
{
   TORQUE_LOCATION(kPosition) float3 pos     : POSITION;
   TORQUE_LOCATION(kTexCoord0) float2 uv0    : TEXCOORD0;
};

struct VertexIn_PTTT
{
   TORQUE_LOCATION(kPosition) float3 pos      : POSITION;
   TORQUE_LOCATION(kTexCoord0) float2 uv0     : TEXCOORD0;
   TORQUE_LOCATION(kTexCoord1) float2 uv1     : TEXCOORD1;
   TORQUE_LOCATION(kTexCoord2) float2 uv2     : TEXCOORD2;
};

struct VertexIn_PC
{
   TORQUE_LOCATION(kPosition) float3 pos     : POSITION;
   TORQUE_LOCATION(kColor) float4 color      : COLOR;
};

struct VertexIn_PNC
{
   TORQUE_LOCATION(kPosition) float3 pos     : POSITION;
   TORQUE_LOCATION(kNormal) float3 normal    : NORMAL;
   TORQUE_LOCATION(kColor) float4 color      : COLOR;
};

struct VertexIn_PCT
{
   TORQUE_LOCATION(kPosition) float3 pos     : POSITION;
   TORQUE_LOCATION(kColor) float4 color      : COLOR;
   TORQUE_LOCATION(kTexCoord0) float2 uv0    : TEXCOORD0;
};

struct VertexIn_PN
{
   TORQUE_LOCATION(kPosition) float3 pos      : POSITION;
   TORQUE_LOCATION(kNormal) float3 normal     : NORMAL;
};

struct VertexIn_PNT
{
   TORQUE_LOCATION(kPosition) float3 pos      : POSITION;
   TORQUE_LOCATION(kNormal) float3 normal     : NORMAL;
   TORQUE_LOCATION(kTexCoord0) float2 uv0     : TEXCOORD0;
};

struct VertexIn_PNTT
{
   TORQUE_LOCATION(kPosition) float3 pos       : POSITION;
   TORQUE_LOCATION(kNormal) float3 normal      : NORMAL;
   TORQUE_LOCATION(kTangent) float3 tangent    : TANGENT;
   TORQUE_LOCATION(kTexCoord0) float2 uv0      : TEXCOORD0;
};

struct VertexIn_PNCT
{
   TORQUE_LOCATION(kPosition) float3 pos     : POSITION;
   TORQUE_LOCATION(kNormal) float3 normal    : NORMAL;
   TORQUE_LOCATION(kColor) float4 color      : COLOR;
   TORQUE_LOCATION(kTexCoord0) float2 uv0    : TEXCOORD0;
};

struct VertexIn_PNTTTB
{
   TORQUE_LOCATION(kPosition) float3 pos        : POSITION;
   TORQUE_LOCATION(kNormal) float3 normal       : NORMAL; 
   TORQUE_LOCATION(kTexCoord0) float2 uv0       : TEXCOORD0;
   TORQUE_LOCATION(kTexCoord1) float2 uv1       : TEXCOORD1;   
   TORQUE_LOCATION(kTexCoord2) float3 T         : TEXCOORD2;
   TORQUE_LOCATION(kTexCoord3) float3 B         : TEXCOORD3;
};

struct VertexIn_PNBTT
{
   TORQUE_LOCATION(kPosition) float3 pos      : POSITION;
   TORQUE_LOCATION(kNormal) float3 normal     : NORMAL;
   TORQUE_LOCATION(kBinormal) float3 binormal : BINORMAL;
   TORQUE_LOCATION(kTangent) float3 tangent   : TANGENT;
   TORQUE_LOCATION(kTexCoord0) float2 uv0     : TEXCOORD0;
};

#endif