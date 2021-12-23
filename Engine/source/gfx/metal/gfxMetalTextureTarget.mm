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
#include "gfx/metal/gfxMetalTextureTarget.h"
#include "gfx/metal/gfxMetalCubemap.h"
#include "gfx/metal/gfxMetalEnumTranslate.h"
#include "gfx/gfxDebugEvent.h"
#include "gfx/gfxStringEnumTranslate.h"

GFXMetalTextureTarget::GFXMetalTextureTarget(bool genMips) 
   :  mTargetSize( Point2I::Zero ),
      mTargetFormat( GFXFormatR8G8B8A8 )
{
   for(S32 i=0; i<MaxRenderSlotId; i++)
   {

   }

   mGenMips = genMips;
}

GFXMetalTextureTarget::~GFXMetalTextureTarget()
{
   // Release anything we might be holding.
   for(S32 i=0; i<MaxRenderSlotId; i++)
   {

   }

   zombify();
}

void GFXMetalTextureTarget::attachTexture( RenderSlot slot, GFXTextureObject *tex, U32 mipLevel/*=0*/, U32 zOffset /*= 0*/ )
{
   GFXDEBUGEVENT_SCOPE( GFXPCMetalTextureTarget_attachTexture, ColorI::RED );

   AssertFatal(slot < MaxRenderSlotId, "GFXMetalTextureTarget::attachTexture - out of range slot.");

   // Mark state as dirty so device can know to update.
   invalidateState();

   if(slot == Color0)
   {
      mTargetSize = Point2I::Zero;
      mTargetFormat = GFXFormatR8G8B8A8;
   }

   // Are we clearing?
   if(!tex)
   {
      // Yup - just exit, it'll stay NULL.      
      return;
   }
}


void GFXMetalTextureTarget::attachTexture( RenderSlot slot, GFXCubemap *tex, U32 face, U32 mipLevel/*=0*/ )
{
   GFXDEBUGEVENT_SCOPE( GFXPCMetalTextureTarget_attachTexture_Cubemap, ColorI::RED );

   AssertFatal(slot < MaxRenderSlotId, "GFXMetalTextureTarget::attachTexture - out of range slot.");

   // Mark state as dirty so device can know to update.
   invalidateState();

   // Cast the texture object to D3D...
   AssertFatal(!tex || static_cast<GFXMetalCubemap*>(tex), "GFXMetalTextureTarget::attachTexture - invalid cubemap object.");

   if(slot == Color0)
   {
      mTargetSize = Point2I::Zero;
      mTargetFormat = GFXFormatR8G8B8A8;
   }

   // Are we clearing?
   if(!tex)
   {
      // Yup - just exit, it'll stay NULL.      
      return;
   }
}

void GFXMetalTextureTarget::activate()
{
   GFXDEBUGEVENT_SCOPE( GFXPCD3D11TextureTarget_activate, ColorI::RED );

   //AssertFatal( mTargets[GFXTextureTarget::Color0], "GFXMetalTextureTarget::activate() - You can never have a NULL primary render target!" );
  
   // Clear the state indicator.
   stateApplied();
}

void GFXMetalTextureTarget::deactivate()
{
   if (!mGenMips)
      return;
}

void GFXMetalTextureTarget::resolve()
{
   GFXDEBUGEVENT_SCOPE( GFXPCMetalTextureTarget_resolve, ColorI::RED );
}

void GFXMetalTextureTarget::resolveTo( GFXTextureObject *tex )
{
   GFXDEBUGEVENT_SCOPE( GFXPCMetalTextureTarget_resolveTo, ColorI::RED );
}

void GFXMetalTextureTarget::zombify()
{
   for(U32 i = 0; i < MaxRenderSlotId; i++)
      attachTexture(RenderSlot(i), NULL);
}

void GFXMetalTextureTarget::resurrect()
{
}
