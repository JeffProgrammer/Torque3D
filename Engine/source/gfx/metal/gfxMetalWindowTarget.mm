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

#include "platform/platform.h"
#include "gfx/metal/gfxMetalTarget.h"
#include "gfx/metal/gfxMetalCubemap.h"
#include "gfx/metal/gfxMetalEnumTranslate.h"
#include "gfx/gfxDebugEvent.h"
#include "gfx/gfxStringEnumTranslate.h"
#include "windowManager/win32/win32Window.h"

GFXMetalWindowTarget::GFXMetalWindowTarget()
{
   mWindow = NULL;
   mBackBuffer = NULL;
   mSecondaryWindow = false;
}

GFXMetalWindowTarget::~GFXMetalWindowTarget()
{

}

void GFXMetalWindowTarget::initPresentationParams()
{
   // Get some video mode related info.
   const GFXVideoMode& vm = mWindow->getVideoMode();
   HWND hwnd = (HWND)mWindow->getSystemWindow(PlatformWindow::WindowSystem_Windows);

   // Do some validation...
   if (vm.fullScreen && mSecondaryWindow)
   {
      AssertFatal(false, "GFXMetalWindowTarget::initPresentationParams - Cannot go fullscreen with secondary window!");
   }
}

const Point2I GFXMetalWindowTarget::getSize()
{
   return mWindow->getVideoMode().resolution; 
}

GFXFormat GFXMetalWindowTarget::getFormat()
{ 
   S32 format = mPresentationParams.BufferDesc.Format;
   GFXREVERSE_LOOKUP( GFXD3D11TextureFormat, GFXFormat, format );
   return (GFXFormat)format;
}

bool GFXMetalWindowTarget::present()
{
   return true;
}

void GFXMetalWindowTarget::createSwapChain()
{

}

void GFXMetalWindowTarget::createBuffersAndViews()
{
   const GFXVideoMode& vm = mWindow->getVideoMode();
}

void GFXMetalWindowTarget::resetMode()
{

}

void GFXMetalWindowTarget::zombify()
{
   SAFE_RELEASE(mBackBuffer);
}

void GFXMetalWindowTarget::resurrect()
{
   setBackBuffer();
}

void GFXMetalWindowTarget::setBackBuffer()
{

}

void GFXMetalWindowTarget::activate()
{
   GFXDEBUGEVENT_SCOPE(GFXPCMetalWindowTarget_activate, ColorI::RED);

   // Update our video mode here, too.
   GFXVideoMode vm;
   vm = mWindow->getVideoMode();
   vm.resolution.x = pp.BufferDesc.Width;
   vm.resolution.y = pp.BufferDesc.Height;
   vm.fullScreen = !pp.Windowed;
   mSize = vm.resolution;
}

void GFXMetalWindowTarget::resolveTo(GFXTextureObject *tex)
{
   GFXDEBUGEVENT_SCOPE(GFXPCMetalWindowTarget_resolveTo, ColorI::RED);
}

