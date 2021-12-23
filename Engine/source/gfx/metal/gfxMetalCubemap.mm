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

#include "gfx/metal/gfxMetalCubemap.h"
#include "gfx/gfxCardProfile.h"
#include "gfx/gfxTextureManager.h"
#include "gfx/metal/gfxMetalEnumTranslate.h"
#include "gfx/bitmap/imageUtils.h"

GFXMetalCubemap::GFXMetalCubemap()
   : mTexSize(0)
{
	mDynamic = false;
   mAutoGenMips = false;
	mFaceFormat = GFXFormatR8G8B8A8;
}

GFXMetalCubemap::~GFXMetalCubemap()
{
	releaseSurfaces();
}

void GFXMetalCubemap::releaseSurfaces()
{
   if (mDynamic)
		GFXTextureManager::removeEventDelegate(this, &GFXMetalCubemap::_onTextureEvent);
}

void GFXMetalCubemap::_onTextureEvent(GFXTexCallbackCode code)
{
   if (code == GFXZombify)
      releaseSurfaces();
   else if (code == GFXResurrect)
      initDynamic(mTexSize);
}

void GFXMetalCubemap::initStatic(GFXTexHandle *faces)
{
   AssertFatal( faces, "GFXMetalCubemap::initStatic - Got null GFXTexHandle!" );
	AssertFatal( *faces, "empty texture passed to CubeMap::create" );
}

void GFXMetalCubemap::initStatic(DDSFile *dds)
{
   AssertFatal(dds, "GFXMetalCubemap::initStatic - Got null DDS file!");
   AssertFatal(dds->isCubemap(), "GFXMetalCubemap::initStatic - Got non-cubemap DDS file!");
   AssertFatal(dds->mSurfaces.size() == 6, "GFXMetalCubemap::initStatic - DDS has less than 6 surfaces!");  
   
   // NOTE - check tex sizes on all faces - they MUST be all same size
   mTexSize = dds->getWidth();
   mFaceFormat = dds->getFormat();
   mMipMapLevels = dds->getMipLevels();
}

void GFXMetalCubemap::initDynamic(U32 texSize, GFXFormat faceFormat, U32 mipLevels)
{
	if(!mDynamic)
		GFXTextureManager::addEventDelegate(this, &GFXMetalCubemap::_onTextureEvent);

	mDynamic = true;
	mTexSize = texSize;
	mFaceFormat = faceFormat;
    if (!mipLevels)
       mAutoGenMips = true;

    mMipMapLevels = mipLevels;

   bool compressed = ImageUtil::isCompressedFormat(mFaceFormat);
}

//-----------------------------------------------------------------------------
// Set the cubemap to the specified texture unit num
//-----------------------------------------------------------------------------
void GFXMetalCubemap::setToTexUnit(U32 tuNum)
{

}

void GFXMetalCubemap::zombify()
{

}

void GFXMetalCubemap::resurrect()
{

}

//-----------------------------------------------------------------------------
// Cubemap Array
//-----------------------------------------------------------------------------

GFXMetalCubemapArray::GFXMetalCubemapArray()
{
}

GFXMetalCubemapArray::~GFXMetalCubemapArray()
{

}

//TODO: really need a common private 'init' function to avoid code double up with these init* functions
void GFXMetalCubemapArray::init(GFXCubemapHandle *cubemaps, const U32 cubemapCount)
{
   AssertFatal(cubemaps, "GFXMetalCubemapArray::initStatic - Got null GFXCubemapHandle!");
   AssertFatal(*cubemaps, "GFXMetalCubemapArray::initStatic - Got empty cubemap!");

   U32 downscalePower = GFXTextureManager::smTextureReductionLevel;
   U32 scaledSize = cubemaps[0]->getSize();

   if (downscalePower != 0)
   {
      // Otherwise apply the appropriate scale...
      scaledSize >>= downscalePower;
   }

   //all cubemaps must be the same size,format and number of mipmaps. Grab the details from the first cubemap
   mSize = scaledSize;
   mFormat = cubemaps[0]->getFormat();
   mMipMapLevels = cubemaps[0]->getMipMapLevels() - downscalePower;
   mNumCubemaps = cubemapCount;
}

//Just allocate the cubemap array but we don't upload any data
void GFXMetalCubemapArray::init(const U32 cubemapCount, const U32 cubemapFaceSize, const GFXFormat format)
{
   U32 downscalePower = GFXTextureManager::smTextureReductionLevel;
   U32 scaledSize = cubemapFaceSize;

   if (downscalePower != 0)
   {
      scaledSize >>= downscalePower;
   }

   mSize = scaledSize;
   mMipMapLevels = ImageUtil::getMaxMipCount(cubemapFaceSize, cubemapFaceSize) - downscalePower;
   mNumCubemaps = cubemapCount;
   mFormat = format;
}

void GFXMetalCubemapArray::updateTexture(const GFXCubemapHandle &cubemap, const U32 slot)
{
   AssertFatal(slot <= mNumCubemaps, "GFXMetalCubemapArray::updateTexture - trying to update a cubemap texture that is out of bounds!");
   AssertFatal(mFormat == cubemap->getFormat(), "GFXMetalCubemapArray::updateTexture - Destination format doesn't match");
   AssertFatal(mSize == cubemap->getSize(), "GFXMetalCubemapArray::updateTexture - Destination size doesn't match");
   AssertFatal(mMipMapLevels == cubemap->getMipMapLevels(), "GFXMetalCubemapArray::updateTexture - Destination mip levels doesn't match");
}

void GFXMetalCubemapArray::copyTo(GFXCubemapArray *pDstCubemap)
{

}

void GFXMetalCubemapArray::setToTexUnit(U32 tuNum)
{

}

void GFXMetalCubemapArray::zombify()
{

}

void GFXMetalCubemapArray::resurrect()
{

}
