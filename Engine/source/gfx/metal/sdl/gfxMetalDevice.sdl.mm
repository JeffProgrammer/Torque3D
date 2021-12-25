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

#include "gfx/metal/gfxMetalDevice.h"
#include "gfx/metal/gfxMetalWindowTarget.h"
#include "windowManager/sdl/sdlWindow.h"
#include <SDL.h>

static inline void EnumerateVideoModes(Vector<GFXVideoMode>& outModes)
{
   int count = SDL_GetNumDisplayModes( 0 );
   if( count < 0)
   {
      AssertFatal(0, "");
      return;
   }
   
   SDL_DisplayMode mode;
   for(int i = 0; i < count; ++i)
   {
      SDL_GetDisplayMode( 0, i, &mode);
      GFXVideoMode outMode;
      outMode.resolution.set( mode.w, mode.h );
      outMode.refreshRate = mode.refresh_rate;

      // BBP = 32 for some reason the engine knows it should be 32, but then we
      // add some extra code to break what the engine knows.
      //outMode.bitDepth = SDL_BYTESPERPIXEL( mode.format );     // sets bitdepths to 4
      //outMode.bitDepth = SDL_BITSPERPIXEL(mode.format);        // sets bitdepth to 24
      
      // hardcoded magic numbers ftw
      // This value is hardcoded in DX, probably to avoid the shenanigans going on here
      outMode.bitDepth = 32;

      outMode.wideScreen = (mode.w / mode.h) > (4 / 3);
      outMode.fullScreen = true;
      
      outModes.push_back( outMode );
   }
}

void GFXMetalDevice::enumerateAdapters(Vector<GFXAdapter *> &adapterList)
{
   AssertFatal( SDL_WasInit(SDL_INIT_VIDEO), "");
   
   GFXAdapter *adapter = new GFXAdapter;
   adapter->mIndex = 0;
   
   adapter->mType = Metal;
   adapter->mShaderModel = 5.0f;
   adapter->mCreateDeviceInstanceDelegate = mCreateDeviceInstance;
   
   EnumerateVideoModes(adapter->mAvailableModes);
   adapterList.push_back(adapter);
}

void GFXMetalDevice::enumerateVideoModes()
{
   mVideoModes.clear();
   EnumerateVideoModes(mVideoModes);
}

void GFXMetalDevice::init(const GFXVideoMode &mode, PlatformWindow *window)
{
   PlatformWindowSDL* sdlWindow = dynamic_cast<PlatformWindowSDL*>(window);
   AssertFatal(sdlWindow, "window is not an SDL window for metal");
   
   mContext = SDL_CreateRenderer(sdlWindow->getSDLWindow(), -1, 0);
   mSwapChain = (__bridge CAMetalLayer*)SDL_RenderGetMetalLayer(static_cast<SDL_Renderer*>(mContext));
   
   initGLState();
   
   mProjectionMatrix.identity();
   
   mInitialized = true;
   deviceInited();
}

void GFXMetalDevice::destroyPlatform()
{
   SDL_DestroyRenderer(static_cast<SDL_Renderer*>(mContext));
}

GFXWindowTarget *GFXMetalDevice::allocWindowTarget( PlatformWindow *window )
{
   GFXMetalWindowTarget* ggwt = new GFXMetalWindowTarget();

   //first window
   if (!mContext)
   {
      init(window->getVideoMode(), window);
      ggwt->mSecondaryWindow = false;
   }
   else
      ggwt->mSecondaryWindow = true;

   ggwt->registerResourceWithDevice(this);

   return ggwt;
}
