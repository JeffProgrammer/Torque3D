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

#include "gfx/gfxDevice.h"
#include "gfx/Metal/gfxMetalStateBlock.h"
#include "gfx/Metal/gfxMetalEnumTranslate.h"
#include "core/crc.h"

namespace DictHash
{
   inline U32 hash(const GFXSamplerStateDesc &data)
   {
      return CRC::calculateCRC(&data, sizeof(GFXSamplerStateDesc));
   }
}

GFXMetalStateBlock::GFXMetalStateBlock(const GFXStateBlockDesc& desc)
{
   PROFILE_SCOPE(GFXMetalStateBlock_CreateStateBlock);

   mDesc = desc;
   mCachedHashValue = desc.getHashValue();

   // Color writes
   mColorMask = 0;
}

GFXMetalStateBlock::~GFXMetalStateBlock()
{

}

/// Returns the hash value of the desc that created this block
U32 GFXMetalStateBlock::getHashValue() const
{
   return mCachedHashValue;
}

/// Returns a GFXStateBlockDesc that this block represents
const GFXStateBlockDesc& GFXMetalStateBlock::getDesc() const
{
   return mDesc;
}

/// Called by D3D11 device to active this state block.
/// @param oldState  The current state, used to make sure we don't set redundant states on the device.  Pass NULL to reset all states.
void GFXMetalStateBlock::activate(GFXMetalStateBlock* oldState)
{
   PROFILE_SCOPE(GFXMetalStateBlock_Activate);
}