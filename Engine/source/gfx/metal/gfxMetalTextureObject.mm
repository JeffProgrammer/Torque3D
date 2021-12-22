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

#include "gfx/metal/gfxMetalTextureObject.h"
#include "platform/profiler.h"
#include "console/console.h"

#ifdef TORQUE_DEBUG
U32 GFXMetalTextureObject::mTexCount = 0;
#endif

GFXMetalTextureObject::GFXMetalTextureObject( GFXDevice * d, GFXTextureProfile *profile) : GFXTextureObject( d, profile )
{
#ifdef D3D11_DEBUG_SPEW
   mTexCount++;
   Con::printf("+ texMake %d %x", mTexCount, this);
#endif

   mD3DTexture = NULL;
   mLocked = false;
   mAutoGenMips = false;
   mD3DSurface = NULL;
   dMemset(&mLockRect, 0, sizeof(mLockRect));
   dMemset(&mLockBox, 0, sizeof(mLockBox));
   mLockedSubresource = 0;
   isManaged = false;
}

GFXMetalTextureObject::~GFXMetalTextureObject()
{
   kill();
#ifdef D3D11_DEBUG_SPEW
   mTexCount--;
   Con::printf("+ texkill %d %x", mTexCount, this);
#endif
}

GFXLockedRect *GFXMetalTextureObject::lock(U32 mipLevel /*= 0*/, RectI *inRect /*= NULL*/)
{
   AssertFatal( !mLocked, "GFXMetalTextureObject::lock - The texture is already locked!" );

   const U32 width = mTextureSize.x >> mipLevel;
   const U32 height = mTextureSize.y >> mipLevel;
   const U32 depth = is3D ? mTextureSize.z >> mipLevel : 1;

   //calculate locked box region and offset
   if (inRect)
   {
      if ((inRect->point.x + inRect->extent.x > width) || (inRect->point.y + inRect->extent.y > height))
         AssertFatal(false, "GFXMetalTextureObject::lock - Rectangle too big!");

      mLockBox.top = inRect->point.y;
      mLockBox.left = inRect->point.x;
      mLockBox.bottom = inRect->point.y + inRect->extent.y;
      mLockBox.right = inRect->point.x + inRect->extent.x;
      mLockBox.back = depth;
      mLockBox.front = 0;

      //calculate offset
      offset = inRect->point.x * getFormatByteSize() + inRect->point.y * mapInfo.RowPitch;
   }
   else
   {
      mLockBox.top = 0;
      mLockBox.left = 0;
      mLockBox.bottom = height;
      mLockBox.right = width;
      mLockBox.back = depth;
      mLockBox.front = 0;
   }

   mLocked = true;
   mLockRect.pBits = static_cast<U8*>(mapInfo.pData) + offset;
   mLockRect.Pitch = mapInfo.RowPitch;

   return (GFXLockedRect*)&mLockRect;
}

void GFXMetalTextureObject::unlock(U32 mipLevel)
{
   AssertFatal( mLocked, "GFXMetalTextureObject::unlock - Attempting to unlock a surface that has not been locked" );

   //profile in the unlock function because all the heavy lifting is done here
   PROFILE_START(GFXMetalTextureObject_lockRT);

   PROFILE_END();

   mLockedSubresource = 0;
   mLocked = false;
}

void GFXMetalTextureObject::release()
{

}

void GFXMetalTextureObject::zombify()
{
   // Managed textures are managed by D3D
   AssertFatal(!mLocked, "GFXMetalTextureObject::zombify - Cannot zombify a locked texture!");
   if(isManaged)
      return;
   release();
}

void GFXMetalTextureObject::resurrect()
{
	// Managed textures are managed by D3D
   if(isManaged)
      return;

   static_cast<GFXD3D11TextureManager*>(TEXMGR)->refreshTexture(this);
}

bool GFXMetalTextureObject::copyToBmp(GBitmap* bmp)
{
   if (!bmp)
      return false;

   // check format limitations
   // at the moment we only support RGBA for the source (other 4 byte formats should
   // be easy to add though)
   AssertFatal(mFormat == GFXFormatR16G16B16A16F || mFormat == GFXFormatR8G8B8A8 || mFormat == GFXFormatR8G8B8A8_LINEAR_FORCE || mFormat == GFXFormatR8G8B8A8_SRGB || mFormat == GFXFormatR8G8B8, "copyToBmp: invalid format");
   if (mFormat != GFXFormatR16G16B16A16F && mFormat != GFXFormatR8G8B8A8 && mFormat != GFXFormatR8G8B8A8_LINEAR_FORCE && mFormat != GFXFormatR8G8B8A8_SRGB && mFormat != GFXFormatR8G8B8)
      return false;

   PROFILE_START(GFXMetalTextureObject_copyToBmp);

   AssertFatal(bmp->getWidth() == getWidth(), avar("GFXMetalTextureObject::copyToBmp - Width mismatch: %i vs %i", bmp->getWidth(), getWidth()));
   AssertFatal(bmp->getHeight() == getHeight(), avar("GFXMetalTextureObject::copyToBmp - Height mismatch: %i vs %i", bmp->getHeight(), getHeight()));
   const U32 mipLevels = getMipLevels();

   bmp->setHasTransparency(mHasTransparency);

   return true;
}

