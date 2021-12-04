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

#include "gfx/gl/gfxGLTextureArray.h"
#include "gfx/gl/gfxGLTextureObject.h"
#include "gfx/gl/gfxGLUtils.h"
#include "core/util/tVector.h"

GFXGLTextureArray::GFXGLTextureArray()
{
   mTextureArray = 0;
}

GFXGLTextureArray::~GFXGLTextureArray()
{
   glDeleteTextures(1, &mTextureArray);
}

void GFXGLTextureArray::init()
{
   PRESERVE_2D_TEXTURE_ARRAY();
   glGenTextures(1, &mTextureArray);
   glBindTexture(GL_TEXTURE_2D_ARRAY, mTextureArray);
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAX_LEVEL, mMin(mMipLevels - 1, 1));
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

   glTexStorage3D(GL_TEXTURE_2D_ARRAY, mMipLevels, GFXGLTextureInternalFormat[mFormat], mWidth, mHeight, mArraySize);
}

void GFXGLTextureArray::_setTexture(const GFXTexHandle& texture, U32 slot)
{
   PRESERVE_2D_TEXTURE_ARRAY();
   glBindTexture(GL_TEXTURE_2D_ARRAY, mTextureArray);

   GFXGLTextureObject* texObj = dynamic_cast<GFXGLTextureObject*>(texture.getPointer());
   for (U32 mip = 0; mip < mMipLevels; ++mip)
   {
      U8* buf = texObj->getTextureData(mip);
      const U32 mipWidth = getMax(U32(1), mWidth >> mip);
      const U32 mipHeight = getMax(U32(1), mHeight >> mip);
      if (mIsCompressed)
      {
         glCompressedTexSubImage3D(
            GL_TEXTURE_2D_ARRAY,
            mip, 0, 0,
            slot, mipWidth, mipHeight, 1,
            GFXGLTextureFormat[mFormat], GFXGLTextureType[mFormat], buf
         );
      }
      else
      {
         glTexSubImage3D(
            GL_TEXTURE_2D_ARRAY,
            mip, 0, 0,
            slot, mipWidth, mipHeight, 1,
            GFXGLTextureFormat[mFormat], GFXGLTextureType[mFormat], buf
         );
      }
      delete[] buf;
   }
}

void GFXGLTextureArray::setToTexUnit(U32 tuNum)
{
   static_cast<GFXGLDevice*>(getOwningDevice())->setTextureArrayInternal(tuNum, this);
}

void GFXGLTextureArray::bind(U32 textureUnit) const
{
   glActiveTexture(GL_TEXTURE0 + textureUnit);
   glBindTexture(GL_TEXTURE_2D_ARRAY, mTextureArray);

   dynamic_cast<GFXGLDevice*>(getOwningDevice())->getOpenglCache()->setCacheBindedTex(textureUnit, GL_TEXTURE_2D_ARRAY, mTextureArray);

   GFXGLStateBlockRef sb = dynamic_cast<GFXGLDevice*>(GFX)->getCurrentStateBlock();
   AssertFatal(sb, "GFXGLTextureArray::bind - No active stateblock!");
   if (!sb)
      return;

   const GFXSamplerStateDesc& ssd = sb->getDesc().samplers[textureUnit];
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, minificationFilter(ssd.minFilter, ssd.mipFilter, 0));
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GFXGLTextureFilter[ssd.magFilter]);
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAX_LEVEL, mMin(mMipLevels - 1, 1));
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GFXGLTextureAddress[ssd.addressModeU]);
   glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GFXGLTextureAddress[ssd.addressModeV]);
}

void GFXGLTextureArray::zombify()
{
}

void GFXGLTextureArray::resurrect()
{
}
