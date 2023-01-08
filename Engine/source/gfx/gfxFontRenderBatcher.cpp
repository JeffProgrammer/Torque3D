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

#include "gfx/gfxFontRenderBatcher.h"
#include "gfx/gFont.h"
#include "gui/render/guiRender.h"
#include "gfx/gfxGenericShaderPipeline.h"

FontRenderBatcher::FontRenderBatcher() : mStorage(8096)
{
   mFont = NULL;
   mLength = 0;
   if (!mFontSB)
   {
      GFXStateBlockDesc f;
      f.zDefined = true;
      f.zEnable = false;
      f.zWriteEnable = false;
      f.cullDefined = true;
      f.cullMode = GFXCullNone;
      f.blendDefined = true;
      f.blendEnable = true;
      f.blendSrc = GFXBlendSrcAlpha;
      f.blendDest = GFXBlendInvSrcAlpha;

      f.setColorWrites(true, true, true, false); // NOTE: comment this out if alpha write is needed
      mFontSB = GFX->createStateBlock(f);
   }
   
   if (!mSampler)
   {
      GFXSamplerDesc2 sampler;
      sampler.magFilter = GFXTextureFilterPoint;
      sampler.minFilter = GFXTextureFilterPoint;
      sampler.addressModeU = GFXAddressClamp;
      sampler.addressModeV = GFXAddressClamp;
      
      mSampler = GFX->createSampler(sampler);
   }
}

void FontRenderBatcher::render(GuiRenderManager* manager, F32 rot, const Point2F &offset )
{
   if( mLength == 0 )
      return;
   
   Vector<GuiRenderInstDraw*> drawInsts;
   
   MatrixF rotMatrix;

   bool doRotation = rot != 0.f;
   if(doRotation)
      rotMatrix.set( EulerF( 0.0, 0.0, mDegToRad( rot ) ) );

   GFXHandle<GFXGraphicsPipelineHandle> pipeline = GFX->getShaderPipelineManager()->getShaderPipeline(GFXGenericShaderPipelineType::GSPAddColorTexture);
   
   for( S32 i = 0; i < mSheets.size(); i++ )
   {
      // Do some early outs...
      if(!mSheets[i])
         continue;

      if(!mSheets[i]->numChars)
         continue;
      
      GFXTextureObject* tex = mFont->getTextureHandle(i);
      
      GuiRenderInstDraw* drawInst = manager->allocRenderInstDraw(mSheets[i]->numChars * 6);
      drawInst->pipeline = pipeline;
      drawInst->primitiveType = GFXTriangleList;
      drawInst->texture = tex;
      drawInst->sampler = mSampler;
      drawInst->primitiveCount = mSheets[i]->numChars * 6;
      
      drawInsts.push_back(drawInst);
      
      GFXVertexPCT *verts = drawInst->vertexList.vertices;

      U32 currentPt = 0;
      for( S32 j = 0; j < mSheets[i]->numChars; j++ )
      {
         // Get some general info to proceed with...
         const CharMarker &m = mSheets[i]->charIndex[j];
         const PlatformFont::CharInfo &ci = mFont->getCharInfo( m.c );

         // Where are we drawing it?
         F32 drawY = offset.y + mFont->getBaseline() - ci.yOrigin * TEXT_MAG;
         F32 drawX = offset.x + m.x + ci.xOrigin;

         // Figure some values.
         const F32 texWidth = (F32)tex->getWidth();
         const F32 texHeight = (F32)tex->getHeight();
         const F32 texLeft   = (F32)(ci.xOffset)             / texWidth;
         const F32 texRight  = (F32)(ci.xOffset + ci.width)  / texWidth;
         const F32 texTop    = (F32)(ci.yOffset)             / texHeight;
         const F32 texBottom = (F32)(ci.yOffset + ci.height) / texHeight;

         const F32 fillConventionOffset = GFX->getFillConventionOffset();
         const F32 screenLeft   = drawX - fillConventionOffset;
         const F32 screenRight  = drawX - fillConventionOffset + ci.width * TEXT_MAG;
         const F32 screenTop    = drawY - fillConventionOffset;
         const F32 screenBottom = drawY - fillConventionOffset + ci.height * TEXT_MAG;

         // Build our vertices. We NEVER read back from the buffer, that's
         // incredibly slow, so for rotation do it into tmp. This code is
         // ugly as sin.
         Point3F tmp;

         tmp.set( screenLeft, screenTop, 0.f );
         if(doRotation)
            rotMatrix.mulP( tmp, &verts[currentPt].point);
         else
            verts[currentPt].point = tmp;
         verts[currentPt].color = m.color;
         verts[currentPt].texCoord.set( texLeft, texTop );
         currentPt++;

         tmp.set( screenLeft, screenBottom, 0.f );
         if(doRotation)
            rotMatrix.mulP( tmp, &verts[currentPt].point);
         else
            verts[currentPt].point = tmp;
         verts[currentPt].color = m.color;
         verts[currentPt].texCoord.set( texLeft, texBottom );
         currentPt++;

         tmp.set( screenRight, screenBottom, 0.f );
         if(doRotation)
            rotMatrix.mulP( tmp, &verts[currentPt].point);
         else
            verts[currentPt].point = tmp;
         verts[currentPt].color = m.color;
         verts[currentPt].texCoord.set( texRight, texBottom );
         currentPt++;

         tmp.set( screenRight, screenBottom, 0.f );
         if(doRotation)
            rotMatrix.mulP( tmp, &verts[currentPt].point);
         else
            verts[currentPt].point = tmp;
         verts[currentPt].color = m.color;
         verts[currentPt].texCoord.set( texRight, texBottom );
         currentPt++;

         tmp.set( screenRight, screenTop, 0.f );
         if(doRotation)
            rotMatrix.mulP( tmp, &verts[currentPt].point);
         else
            verts[currentPt].point = tmp;
         verts[currentPt].color = m.color;
         verts[currentPt].texCoord.set( texRight, texTop );
         currentPt++;

         tmp.set( screenLeft, screenTop, 0.f );
         if(doRotation)
            rotMatrix.mulP( tmp, &verts[currentPt].point);
         else
            verts[currentPt].point = tmp;
         verts[currentPt].color = m.color;
         verts[currentPt].texCoord.set( texLeft, texTop );
         currentPt++;
      }
   }

   manager->pushStateBlock(mFontSB);
   for (U32 i = 0; i < drawInsts.size(); i++)
   {
      manager->pushDrawInst(drawInsts[i]);
   }
}

void FontRenderBatcher::queueChar( UTF16 c, S32 &currentX, GFXVertexColor &currentColor )
{
   const PlatformFont::CharInfo &ci = mFont->getCharInfo( c );
   U32 sidx = ci.bitmapIndex;

   if( ci.width != 0 && ci.height != 0 )
   {
      SheetMarker &sm = getSheetMarker(sidx);

      CharMarker &m = sm.charIndex[sm.numChars];
      sm.numChars++;

      m.c = c;
      m.x = (F32)currentX;
      m.color = currentColor;
   }

   currentX += ci.xIncrement;
}

FontRenderBatcher::SheetMarker & FontRenderBatcher::getSheetMarker( U32 sheetID )
{
   // Add empty sheets up to and including the requested sheet if necessary
   if (mSheets.size() <= sheetID)
   {
      S32 oldSize = mSheets.size();
      mSheets.setSize( sheetID + 1 );
      for ( S32 i = oldSize; i < mSheets.size(); i++ )
         mSheets[i] = NULL;
   }

   // Allocate if it doesn't exist...
   if (!mSheets[sheetID])
   {
      S32 size = sizeof( SheetMarker) + mLength * sizeof( CharMarker );
      mSheets[sheetID] = (SheetMarker *)mStorage.alloc(size);
      mSheets[sheetID]->numChars = 0;
      mSheets[sheetID]->startVertex = 0; // cosmetic initialization
   }

   return *mSheets[sheetID];
}

void FontRenderBatcher::init( GFont *font, U32 n )
{
   // Clear out batched results
   dMemset(mSheets.address(), 0, mSheets.memSize());
   mSheets.clear();
   mStorage.freeBlocks(true);

   mFont = font;
   mLength = n;
}
