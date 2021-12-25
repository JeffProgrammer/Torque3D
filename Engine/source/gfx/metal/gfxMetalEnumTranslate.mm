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

#include "console/console.h"
#include "gfx/metal/gfxMetalEnumTranslate.h"

#import <Metal/Metal.h>

MTLPrimitiveType GFXMetalPrimType[GFXPT_COUNT];
MTLBlendFactor GFXMetalBlend[GFXBlend_COUNT];
MTLBlendOperation GFXMetalBlendOp[GFXBlendOp_COUNT];
MTLCompareFunction GFXMetalCmpFunc[GFXCmp_COUNT];
MTLSamplerMipFilter GFXMetalTextureFilter[GFXTextureFilter_COUNT];
MTLCullMode GFXMetalCullMode[GFXCull_COUNT];
MTLTriangleFillMode GFXMetalFillMode[GFXFill_COUNT];
MTLStencilOperation GFXMetalStencilOp[GFXStencilOp_COUNT];

void GFXMetalEnumTranslate::init()
{
   GFXMetalPrimType[GFXPointList] = MTLPrimitiveTypePoint;
   GFXMetalPrimType[GFXLineList] = MTLPrimitiveTypeLine;
   GFXMetalPrimType[GFXLineStrip] = MTLPrimitiveTypeLineStrip;
   GFXMetalPrimType[GFXTriangleList] = MTLPrimitiveTypeTriangle;
   GFXMetalPrimType[GFXTriangleStrip] = MTLPrimitiveTypeLineStrip;
   
   GFXMetalBlend[GFXBlendZero] = MTLBlendFactorZero;
   GFXMetalBlend[GFXBlendOne] = MTLBlendFactorOne;
   GFXMetalBlend[GFXBlendSrcColor] = MTLBlendFactorSourceColor;
   GFXMetalBlend[GFXBlendInvSrcColor] = MTLBlendFactorOneMinusSourceColor;
   GFXMetalBlend[GFXBlendSrcAlpha] = MTLBlendFactorSourceAlpha;
   GFXMetalBlend[GFXBlendInvSrcAlpha] = MTLBlendFactorOneMinusSourceAlpha;
   GFXMetalBlend[GFXBlendDestAlpha] = MTLBlendFactorDestinationAlpha;
   GFXMetalBlend[GFXBlendInvDestAlpha] = MTLBlendFactorOneMinusDestinationAlpha;
   GFXMetalBlend[GFXBlendDestColor] = MTLBlendFactorDestinationColor;
   GFXMetalBlend[GFXBlendInvDestColor] = MTLBlendFactorOneMinusDestinationColor;
   GFXMetalBlend[GFXBlendSrcAlphaSat] = MTLBlendFactorSourceAlphaSaturated;
   
   GFXMetalBlendOp[GFXBlendOpAdd] = MTLBlendOperationAdd;
   GFXMetalBlendOp[GFXBlendOpSubtract] = MTLBlendOperationSubtract;
   GFXMetalBlendOp[GFXBlendOpRevSubtract] = MTLBlendOperationReverseSubtract;
   GFXMetalBlendOp[GFXBlendOpMin] = MTLBlendOperationMin;
   GFXMetalBlendOp[GFXBlendOpMax] = MTLBlendOperationMax;
   
   GFXMetalCmpFunc[GFXCmpNever] = MTLCompareFunctionNever;
   GFXMetalCmpFunc[GFXCmpLess] = MTLCompareFunctionLess;
   GFXMetalCmpFunc[GFXCmpEqual] = MTLCompareFunctionEqual;
   GFXMetalCmpFunc[GFXCmpLessEqual] = MTLCompareFunctionLessEqual;
   GFXMetalCmpFunc[GFXCmpGreater] = MTLCompareFunctionGreater;
   GFXMetalCmpFunc[GFXCmpNotEqual] = MTLCompareFunctionNotEqual;
   GFXMetalCmpFunc[GFXCmpGreaterEqual] = MTLCompareFunctionGreaterEqual;
   GFXMetalCmpFunc[GFXCmpAlways] = MTLCompareFunctionAlways;
   
   GFXMetalTextureFilter[GFXTextureFilterNone] = MTLSamplerMipFilterNotMipmapped;
   GFXMetalTextureFilter[GFXTextureFilterPoint] = MTLSamplerMipFilterNearest;
   GFXMetalTextureFilter[GFXTextureFilterLinear] = MTLSamplerMipFilterLinear;
   GFXMetalTextureFilter[GFXTextureFilterAnisotropic] = MTLSamplerMipFilterLinear;
   
   GFXMetalCullMode[GFXCullNone] = MTLCullModeNone;
   GFXMetalCullMode[GFXCullCW] = MTLCullModeFront;
   GFXMetalCullMode[GFXCullCCW] = MTLCullModeBack;
   
   GFXMetalFillMode[GFXFillPoint] = MTLTriangleFillModeFill;
   GFXMetalFillMode[GFXFillWireframe] = MTLTriangleFillModeLines;
   GFXMetalFillMode[GFXFillSolid] = MTLTriangleFillModeFill;
   
   GFXMetalStencilOp[GFXStencilOpKeep] = MTLStencilOperationKeep;
   GFXMetalStencilOp[GFXStencilOpZero] = MTLStencilOperationZero;
   GFXMetalStencilOp[GFXStencilOpReplace] = MTLStencilOperationReplace;
   GFXMetalStencilOp[GFXStencilOpIncrSat] = MTLStencilOperationIncrementClamp;
   GFXMetalStencilOp[GFXStencilOpDecrSat] = MTLStencilOperationDecrementClamp;
   GFXMetalStencilOp[GFXStencilOpInvert] = MTLStencilOperationInvert;
   GFXMetalStencilOp[GFXStencilOpIncr] = MTLStencilOperationIncrementWrap;
   GFXMetalStencilOp[GFXStencilOpDecr] = MTLStencilOperationDecrementWrap;
}

