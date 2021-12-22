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

#ifndef _GFX_METAL_GFXMETALWINDOWTARGET_H_
#define _GFX_METAL_GFXMETALWINDOWTARGET_H_

#include "gfx/metal/gfxMetalDevice.h"
#include "gfx/metal/gfxMetalTextureObject.h"
#include "gfx/gfxTarget.h"
#include "math/mPoint3.h"
#include "math/mPoint2.h"

class GFXMetalWindowTarget : public GFXWindowTarget
{
   friend class GFXMetalDevice;

   /// Maximum size we can render to.
   Point2I mSize;

   /// Internal interface that notifies us we need to reset our video mode.
   void resetMode();

   /// Is this a secondary window
   bool mSecondaryWindow;
public:

   GFXMetalWindowTarget();
   ~GFXMetalWindowTarget();
 
   virtual const Point2I getSize();
   virtual GFXFormat getFormat();
   virtual bool present();

   void initPresentationParams();
   void createSwapChain();
   void createBuffersAndViews();
   void setBackBuffer();

   virtual void activate();   

   void zombify();
   void resurrect();

   virtual void resolveTo( GFXTextureObject *tex );
};

#endif
