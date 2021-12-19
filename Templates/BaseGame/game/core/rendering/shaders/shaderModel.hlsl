//-----------------------------------------------------------------------------
// Copyright (c) 2021 Tim Barnes
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

#ifndef TORQUE_SHADERMODEL_H
#define TORQUE_SHADERMODEL_H

// Sampler uniforms
#define TORQUE_UNIFORM_SAMPLER1D(tex,regist) uniform Texture1D tex : register(t##regist); uniform SamplerState sampler_##tex : register(s##regist)
#define TORQUE_UNIFORM_SAMPLER2D(tex,regist) uniform Texture2D tex : register(t##regist); uniform SamplerState sampler_##tex : register(s##regist)
#define TORQUE_UNIFORM_SAMPLER2DCMP(tex,regist) uniform Texture2D tex : register(t##regist); uniform SamplerComparisonState sampler_##tex : register(s##regist)
#define TORQUE_UNIFORM_SAMPLER3D(tex,regist) uniform Texture3D tex : register(t##regist); uniform SamplerState sampler_##tex : register(s##regist)
#define TORQUE_UNIFORM_SAMPLERCUBE(tex,regist) uniform TextureCube tex : register(t##regist); uniform SamplerState sampler_##tex : register(s##regist)
#define TORQUE_UNIFORM_SAMPLERCUBEARRAY(tex,regist) uniform TextureCubeArray tex : register(t##regist); uniform SamplerState sampler_##tex : register(s##regist)
// Sampling functions
#define TORQUE_TEX1D(tex,coords) tex.Sample(sampler_##tex,coords)
#define TORQUE_TEX2D(tex,coords) tex.Sample(sampler_##tex,coords)
#define TORQUE_TEX2DPROJ(tex,coords) tex.Sample(sampler_##tex,coords.xy / coords.w)
#define TORQUE_TEX3D(tex,coords) tex.Sample(sampler_##tex,coords)
#define TORQUE_TEXCUBE(tex,coords) tex.Sample(sampler_##tex,coords)
#define TORQUE_TEXCUBEARRAY(tex,coords) tex.Sample(sampler_##tex,coords)
// The mipmap LOD is specified in coord.w
#define TORQUE_TEX2DLOD(tex,coords) tex.SampleLevel(sampler_##tex,coords.xy,coords.w)
#define TORQUE_TEXCUBELOD(tex,coords) tex.SampleLevel(sampler_##tex,coords.xyz,coords.w)
#define TORQUE_TEXCUBEARRAYLOD(tex,coords,id,lod) tex.SampleLevel(sampler_##tex,float4(coords.xyz,id),lod)
// Tex2d comparison
#define TORQUE_TEX2DCMP(tex,coords,compare) tex.SampleCmpLevelZero(sampler_##tex,coords,compare)

#define TORQUE_TEX2DGATHER(tex,coords,compare,offset) tex.GatherCmp(sampler_##tex,coords,compare,offset)

//helper if you want to pass sampler/texture in a function
//1D
#define TORQUE_SAMPLER1D(tex) Texture1D tex, SamplerState sampler_##tex
#define TORQUE_SAMPLER1D_MAKEARG(tex) tex, sampler_##tex
//2D
#define TORQUE_SAMPLER2D(tex) Texture2D tex, SamplerState sampler_##tex
#define TORQUE_SAMPLER2DARRAY(tex) Texture2DArray tex, SamplerState sampler_##tex
#define TORQUE_SAMPLER2D_MAKEARG(tex) tex, sampler_##tex
// Sampler comparison state - use above MAKEARG with this
#define TORQUE_SAMPLER2DCMP(tex) Texture2D tex, SamplerComparisonState sampler_##tex
//Cube
#define TORQUE_SAMPLERCUBE(tex) TextureCube tex, SamplerState sampler_##tex
#define TORQUE_SAMPLERCUBE_MAKEARG(tex) tex, sampler_##tex
#define TORQUE_SAMPLERCUBEARRAY(tex) TextureCubeArray tex, SamplerState sampler_##tex
#define TORQUE_SAMPLERCUBEARRAY_MAKEARG(tex) tex, sampler_##tex

//Push constants - Struct must be named PushConstant
#ifdef TORQUE_VULKAN
#define TORQUE_PUSHCONSTANT(varName) [[vk::push_constant]] PushConstant varName
#else
//will go into $Global cbuffer
#define TORQUE_PUSHCONSTANT(varName) uniform PushConstant varName
#endif

//Specialization constants
#ifdef TORQUE_VULKAN
#define TORQUE_CONSTANTID(Id) [[vk::constant_id(Id)]]
#else
#define TORQUE_CONSTANTID(Id)
#endif

//Input/Output explicit location
#ifdef TORQUE_VULKAN
#define TORQUE_LOCATION(x) [[vk::location(x)]]
#else
#define TORQUE_LOCATION(x)
#endif

#endif