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

#ifndef _GFXMETALSTATEBLOCK_H_
#define _GFXMETALSTATEBLOCK_H_

#include "gfx/gfxStateBlock.h"

namespace DictHash
{
   U32 hash(const GFXSamplerStateDesc &data);
}

class GFXMetalStateBlock : public GFXStateBlock
{   
public:

   GFXMetalStateBlock(const GFXStateBlockDesc& desc);
   virtual ~GFXMetalStateBlock();

   /// Called by Metal device to active this state block.
   /// @param oldState  The current state, used to make sure we don't set redundant states on the device.  Pass NULL to reset all states.
   void activate(GFXMetalStateBlock* oldState);

   // 
   // GFXStateBlock interface
   //

   /// Returns the hash value of the desc that created this block
   virtual U32 getHashValue() const;

   /// Returns a GFXStateBlockDesc that this block represents
   virtual const GFXStateBlockDesc& getDesc() const;

   //
   // GFXResource
   //
   virtual void zombify() { }
   /// When called the resource should restore all device sensitive information destroyed by zombify()
   virtual void resurrect() { }
private:

   // D3D11_BLEND_DESC mBlendDesc;
   // D3D11_RASTERIZER_DESC mRasterizerDesc;
   // D3D11_DEPTH_STENCIL_DESC mDepthStencilDesc; 
   // D3D11_SAMPLER_DESC mSamplerDesc[GFX_TEXTURE_STAGE_COUNT];

   // ID3D11BlendState* mBlendState;
   // ID3D11DepthStencilState* mDepthStencilState;
   // ID3D11RasterizerState* mRasterizerState;
   // ID3D11SamplerState* mSamplerStates[GFX_TEXTURE_STAGE_COUNT];

   GFXStateBlockDesc mDesc;
   U32 mCachedHashValue;
   // Cached D3D specific things, these are "calculated" from GFXStateBlock
   U32 mColorMask; 
};

typedef StrongRefPtr<GFXMetalStateBlock> GFXMetalStateBlockRef;

#endif
