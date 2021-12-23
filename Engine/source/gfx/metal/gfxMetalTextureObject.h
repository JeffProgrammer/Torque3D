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

#ifndef _GFXMETALTEXTUREOBJECT_H_
#define _GFXMETALTEXTUREOBJECT_H_

#include "gfx/metal/gfxMetalDevice.h"
#include "gfx/gfxTextureHandle.h"
#include "gfx/gfxTextureManager.h"

class GFXMetalTextureObject : public GFXTextureObject
{
protected:
   static U32 mTexCount;
   GFXTexHandle mStagingTex;
   bool mLocked;
   bool mAutoGenMips;
   
   GFXLockedRect mLockBox;
   GFXLockedRect mLockRect;

   U32 mLockedSubresource;
public:

   GFXMetalTextureObject( GFXDevice * d, GFXTextureProfile *profile);
   ~GFXMetalTextureObject();  

   void release();

   bool autoGenMips() const { return mAutoGenMips; }
   void setAutoGenMips(bool val) { mAutoGenMips = val; }

   bool isManaged; //setting to true tells this texture not to be released from being zombify

   virtual GFXLockedRect * lock(U32 mipLevel = 0, RectI *inRect = NULL);
   virtual void unlock(U32 mipLevel = 0 );

   virtual bool			copyToBmp(GBitmap* bmp);

   // GFXResource
   void zombify();
   void resurrect();

#ifdef TORQUE_DEBUG
   virtual void pureVirtualCrash() {};
#endif
};

#endif
