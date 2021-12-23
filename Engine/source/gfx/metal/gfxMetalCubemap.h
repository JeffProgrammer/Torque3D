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

#ifndef _GFXMETALCUBEMAP_H_
#define _GFXMETALCUBEMAP_H_

#include "gfx/Metal/gfxMetalDevice.h"
#include "gfx/gfxCubemap.h"
#include "gfx/gfxResource.h"
#include "gfx/gfxTarget.h"

const U32 CubeFaces = 6;
const U32 MaxMipMaps = 13; //todo this needs a proper static value somewhere to sync up with other classes like GBitmap

class GFXMetalCubemap : public GFXCubemap
{
public:
   virtual void initStatic( GFXTexHandle *faces );
   virtual void initStatic( DDSFile *dds );
   virtual void initDynamic( U32 texSize, GFXFormat faceFormat = GFXFormatR8G8B8A8, U32 mipLevels = 0);
   virtual void setToTexUnit( U32 tuNum );
   virtual U32 getSize() const { return mTexSize; }
   virtual GFXFormat getFormat() const { return mFaceFormat; }

   GFXMetalCubemap();
   virtual ~GFXMetalCubemap();

   // GFXResource interface
   virtual void zombify();
   virtual void resurrect();

   virtual bool isInitialized() { false; }
private:
   friend class GFXMetalTextureTarget;
   friend class GFXMetalDevice;

   bool mAutoGenMips;
   bool mDynamic;
   U32  mTexSize;
   GFXFormat mFaceFormat;
   
   void releaseSurfaces();

   /// The callback used to get texture events.
   /// @see GFXTextureManager::addEventDelegate
   void _onTextureEvent(GFXTexCallbackCode code);
};

class GFXMetalCubemapArray : public GFXCubemapArray
{
public:
   GFXMetalCubemapArray();
   virtual ~GFXMetalCubemapArray();
   virtual void init(GFXCubemapHandle *cubemaps, const U32 cubemapCount);
   virtual void init(const U32 cubemapCount, const U32 cubemapFaceSize, const GFXFormat format);
   virtual void updateTexture(const GFXCubemapHandle &cubemap, const U32 slot);
   virtual void copyTo(GFXCubemapArray *pDstCubemap);
   virtual void setToTexUnit(U32 tuNum);

   // GFXResource interface
   virtual void zombify();
   virtual void resurrect();

private:
   friend class GFXMetalTextureTarget;
   friend class GFXMetalDevice;
};

#endif
