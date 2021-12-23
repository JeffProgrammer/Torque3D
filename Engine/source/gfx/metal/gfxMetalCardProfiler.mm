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

#include "gfx/metal/gfxMetalCardProfiler.h"
#include "gfx/metal/gfxMetalDevice.h"
#include "gfx/metal/gfxMetalEnumTranslate.h"

GFXMetalCardProfiler::GFXMetalCardProfiler()
{
   
}

GFXMetalCardProfiler::~GFXMetalCardProfiler()
{
   
}

void GFXMetalCardProfiler::init()
{
   mRendererString = "Metal";
   Parent::init();
}

void GFXMetalCardProfiler::setupCardCapabilities()
{

}

bool GFXMetalCardProfiler::_queryCardCap(const String& query, U32& foundResult)
{
   // Just doing what the D3D11 layer does
   return 0;
}

bool GFXMetalCardProfiler::_queryFormat(const GFXFormat fmt, const GFXTextureProfile *profile, bool &inOutAutogenMips)
{
   // We assume if the format is valid that we can use it for any purpose.
   // This may not be the case, but we have no way to check short of in depth 
   // testing of every format for every purpose.  And by testing, I mean sitting
   // down and doing it by hand, because there is no OpenGL API to check these
   // things.
   //return GFXGLTextureInternalFormat[fmt] != GL_ZERO;
   return false;
}
