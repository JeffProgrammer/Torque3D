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
#include "gfx/metal/gfxMetalDevice.h"

#include "gfx/metal/gfxMetalTextureArray.h"
#include "platform/platformGL.h"

#include "gfx/gfxCubemap.h"
#include "gfx/gfxDrawUtil.h"

#include "gfx/metal/gfxMetalEnumTranslate.h"
#include "gfx/metal/gfxMetalVertexBuffer.h"
#include "gfx/metal/gfxMetalPrimitiveBuffer.h"
#include "gfx/metal/gfxMetalTextureTarget.h"
#include "gfx/metal/gfxMetalTextureManager.h"
#include "gfx/metal/gfxMetalTextureObject.h"
#include "gfx/metal/gfxMetalCubemap.h"
#include "gfx/metal/gfxMetalCardProfiler.h"
#include "gfx/metal/gfxMetalWindowTarget.h"
#include "platform/platformDlibrary.h"
#include "gfx/metal/gfxMetalShader.h"
#include "gfx/primBuilder.h"
#include "console/console.h"
#include "gfx/metal/gfxMetalOcclusionQuery.h"
#include "materials/shaderData.h"
#include "gfx/metal/gfxMetalVertexDecl.h"
#include "shaderGen/shaderGen.h"

GFXAdapter::CreateDeviceInstanceDelegate GFXMetalDevice::mCreateDeviceInstance(GFXMetalDevice::createInstance); 

GFXDevice *GFXMetalDevice::createInstance( U32 adapterIndex )
{
   return new GFXMetalDevice(adapterIndex);
}

void GFXMetalDevice::initGLState()
{  
   // We don't currently need to sync device state with a known good place because we are
   // going to set everything in GFXMetalStateBlock, but if we change our GFXMetalStateBlock strategy, this may
   // need to happen.
   
   // Deal with the card profiler here when we know we have a valid context.
   mCardProfiler = new GFXMetalCardProfiler();
   mCardProfiler->init(); 

   mShaderModel = 5.0f;


   //install vsync callback
   Con::NotifyDelegate clbk(this, &GFXMetalDevice::vsyncCallback);
   Con::addVariableNotify("$pref::Video::disableVerticalSync", clbk);
}

void GFXMetalDevice::vsyncCallback()
{

}

GFXMetalDevice::GFXMetalDevice(U32 adapterIndex) :
   mAdapterIndex(adapterIndex),
   mNeedUpdateVertexAttrib(false),
   mCurrentPB(NULL),
   mDrawInstancesCount(0),
   mCurrentShader( NULL ),
   m_mCurrentWorld(true),
   m_mCurrentView(true),
   mContext(NULL),
   mPixelFormat(NULL),
   mMaxShaderTextures(2),
   mMaxFFTextures(2),
   mMaxTRColors(1),
   mClip(0, 0, 0, 0),
   mWindowRT(NULL)
{
   for(int i = 0; i < VERTEX_STREAM_COUNT; ++i)
   {
      mCurrentVB[i] = NULL;
      mCurrentVB_Divisor[i] = 0;
   }

   // Initiailize capabilities to false.
   memset(&mCapabilities, 0, sizeof(Capabilities));

   GFXMetalEnumTranslate::init();

   GFXVertexColor::setSwizzle( &Swizzles::rgba );

   // OpenGL have native RGB, no need swizzle
   mDeviceSwizzle32 = &Swizzles::rgba;
   mDeviceSwizzle24 = &Swizzles::rgb;

   mTextureManager = new GFXMetalTextureManager();

   //for(U32 i = 0; i < GFX_TEXTURE_STAGE_COUNT; i++)
      //mActiveTextureType[i] = GL_ZERO;

   mNumVertexStream = 2;

   for(U32 i = 0; i < GS_COUNT; ++i)
      mModelViewProjSC[i] = NULL;

   mCurrentConstBuffer = NULL;
}

GFXMetalDevice::~GFXMetalDevice()
{
   mCurrentStateBlock = NULL;

   for(U32 i = 0; i < VERTEX_STREAM_COUNT; ++i)      
      mCurrentVB[i] = NULL;
   mCurrentPB = NULL;
   
   for(U32 i = 0; i < mVolatileVBs.size(); i++)
      mVolatileVBs[i] = NULL;
   for(U32 i = 0; i < mVolatilePBs.size(); i++)
      mVolatilePBs[i] = NULL;

   // Clear out our current texture references
   for (U32 i = 0; i < GFX_TEXTURE_STAGE_COUNT; i++)
   {
      mCurrentTexture[i] = NULL;
      mNewTexture[i] = NULL;
      mCurrentCubemap[i] = NULL;
      mNewCubemap[i] = NULL;
   }

   mRTStack.clear();
   mCurrentRT = NULL;

   if( mTextureManager )
   {
      mTextureManager->zombify();
      mTextureManager->kill();
   }

   GFXResource* walk = mResourceListHead;
   while(walk)
   {
      walk->zombify();
      walk = walk->getNextResource();
   }
      
   if( mCardProfiler )
   {
      SAFE_DELETE( mCardProfiler );
   }
}

void GFXMetalDevice::zombify()
{
   mTextureManager->zombify();

   //for(int i = 0; i < VERTEX_STREAM_COUNT; ++i)
   //   if(mCurrentVB[i])
   //      mCurrentVB[i]->finish();
   //if(mCurrentPB)
   //      mCurrentPB->finish();
   
   //mVolatileVBs.clear();
   //mVolatilePBs.clear();
   GFXResource* walk = mResourceListHead;
   while(walk)
   {
      walk->zombify();
      walk = walk->getNextResource();
   }
}

void GFXMetalDevice::resurrect()
{
   GFXResource* walk = mResourceListHead;
   while(walk)
   {
      walk->resurrect();
      walk = walk->getNextResource();
   }
   for(int i = 0; i < VERTEX_STREAM_COUNT; ++i)   
      if(mCurrentVB[i])
         mCurrentVB[i]->prepare();
   if(mCurrentPB)
      mCurrentPB->prepare();
   
   mTextureManager->resurrect();
}

GFXVertexBuffer* GFXMetalDevice::findVolatileVBO(U32 numVerts, const GFXVertexFormat *vertexFormat, U32 vertSize)
{
   PROFILE_SCOPE(GFXMetalDevice_findVBPool);
   for(U32 i = 0; i < mVolatileVBs.size(); i++)
      if (  mVolatileVBs[i]->mNumVerts >= numVerts &&
            mVolatileVBs[i]->mVertexFormat.isEqual( *vertexFormat ) &&
            mVolatileVBs[i]->mVertexSize == vertSize &&
            mVolatileVBs[i]->getRefCount() == 1 )
         return mVolatileVBs[i];

   // No existing VB, so create one
   PROFILE_SCOPE(GFXMetalDevice_createVBPool);
   StrongRefPtr<GFXMetalVertexBuffer> buf(new GFXMetalVertexBuffer(GFX, numVerts, vertexFormat, vertSize, GFXBufferTypeVolatile));
   buf->registerResourceWithDevice(this);
   mVolatileVBs.push_back(buf);
   return buf.getPointer();
}

GFXPrimitiveBuffer* GFXMetalDevice::findVolatilePBO(U32 numIndices, U32 numPrimitives)
{
   for(U32 i = 0; i < mVolatilePBs.size(); i++)
      if((mVolatilePBs[i]->mIndexCount >= numIndices) && (mVolatilePBs[i]->getRefCount() == 1))
         return mVolatilePBs[i];
   
   // No existing PB, so create one
   StrongRefPtr<GFXMetalPrimitiveBuffer> buf(new GFXMetalPrimitiveBuffer(GFX, numIndices, numPrimitives, GFXBufferTypeVolatile));
   buf->registerResourceWithDevice(this);
   mVolatilePBs.push_back(buf);
   return buf.getPointer();
}

GFXVertexBuffer *GFXMetalDevice::allocVertexBuffer(   U32 numVerts, 
                                                   const GFXVertexFormat *vertexFormat, 
                                                   U32 vertSize, 
                                                   GFXBufferType bufferType,
                                                   void* data )  
{
   PROFILE_SCOPE(GFXMetalDevice_allocVertexBuffer);
   if(bufferType == GFXBufferTypeVolatile)
      return findVolatileVBO(numVerts, vertexFormat, vertSize);
         
   GFXMetalVertexBuffer* buf = new GFXMetalVertexBuffer( GFX, numVerts, vertexFormat, vertSize, bufferType );
   buf->registerResourceWithDevice(this);   

   if(data)
   {
      void* dest;
      buf->lock(0, numVerts, &dest);
      dMemcpy(dest, data, vertSize * numVerts);
      buf->unlock();
   }

   return buf;
}

GFXPrimitiveBuffer *GFXMetalDevice::allocPrimitiveBuffer( U32 numIndices, U32 numPrimitives, GFXBufferType bufferType, void* data ) 
{
   GFXPrimitiveBuffer* buf;
   
   if(bufferType == GFXBufferTypeVolatile)
   {
      buf = findVolatilePBO(numIndices, numPrimitives);
   }
   else
   {
      buf = new GFXMetalPrimitiveBuffer(GFX, numIndices, numPrimitives, bufferType);
      buf->registerResourceWithDevice(this);
   }
   
   if(data)
   {
      void* dest;
      buf->lock(0, numIndices, &dest);
      dMemcpy(dest, data, sizeof(U16) * numIndices);
      buf->unlock();
   }
   return buf;
}

void GFXMetalDevice::setVertexStream( U32 stream, GFXVertexBuffer *buffer )
{
   AssertFatal(stream <= 1, "GFXMetalDevice::setVertexStream only support 2 stream (0: data, 1: instancing)");

   //if(mCurrentVB[stream] != buffer)
   {
      // Reset the state the old VB required, then set the state the new VB requires.
      if( mCurrentVB[stream] )
      {     
         //mCurrentVB[stream]->finish();
      }

      mCurrentVB[stream] = static_cast<GFXMetalVertexBuffer*>( buffer );

      mNeedUpdateVertexAttrib = true;
   }
}

void GFXMetalDevice::setVertexStreamFrequency( U32 stream, U32 frequency )
{
   if( stream == 0 )
   {
      mCurrentVB_Divisor[stream] = 0; // non instanced, is vertex buffer
      mDrawInstancesCount = frequency; // instances count
   }
   else
   {
      AssertFatal(frequency <= 1, "GFXMetalDevice::setVertexStreamFrequency only support 0/1 for this stream" );
      if( stream == 1 && frequency == 1 )
         mCurrentVB_Divisor[stream] = 1; // instances data need a frequency of 1
      else
         mCurrentVB_Divisor[stream] = 0;
   }

   mNeedUpdateVertexAttrib = true;
}

GFXCubemap* GFXMetalDevice::createCubemap()
{ 
   GFXMetalCubemap* cube = new GFXMetalCubemap();
   cube->registerResourceWithDevice(this);
   return cube; 
};

GFXCubemapArray *GFXMetalDevice::createCubemapArray()
{
   GFXMetalCubemapArray* cubeArray = new GFXMetalCubemapArray();
   cubeArray->registerResourceWithDevice(this);
   return cubeArray;
}

GFXTextureArray* GFXMetalDevice::createTextureArray()
{
   GFXMetalTextureArray* textureArray = new GFXMetalTextureArray();
   textureArray->registerResourceWithDevice(this);
   return textureArray;
}

bool GFXMetalDevice::beginSceneInternal()
{
   return true;
}

void GFXMetalDevice::endSceneInternal() 
{
   // nothing to do for opengl
   mCanCurrentlyRender = false;
}

void GFXMetalDevice::copyResource(GFXTextureObject* pDst, GFXCubemap* pSrc, const U32 face)
{
   AssertFatal(pDst, "GFXMetalDevice::copyResource: Destination texture is null");
   AssertFatal(pSrc, "GFXMetalDevice::copyResource: Source cubemap is null");

   GFXMetalTextureObject* gGLDst = static_cast<GFXMetalTextureObject*>(pDst);
   GFXMetalCubemap* pGLSrc = static_cast<GFXMetalCubemap*>(pSrc);

   const GFXFormat format = pGLSrc->getFormat();
   const bool isCompressed = false; //ImageUtil::isCompressedFormat(format);
   const U32 mipLevels = pGLSrc->getMipMapLevels();
   const U32 texSize = pGLSrc->getSize();

   for (U32 mip = 0; mip < mipLevels; mip++)
   {
      const U32 mipSize =  texSize >> mip;
   }
}

void GFXMetalDevice::clear(U32 flags, const LinearColorF& color, F32 z, U32 stencil)
{
   // Make sure we have flushed our render target state.
   _updateRenderTargets();
}

void GFXMetalDevice::clearColorAttachment(const U32 attachment, const LinearColorF& color)
{
   const F32 clearColor[4] = { color.red, color.green, color.blue, color.alpha };
}

GFXVertexDecl* GFXMetalDevice::allocVertexDecl( const GFXVertexFormat *vertexFormat ) 
{
   PROFILE_SCOPE(GFXMetalDevice_allocVertexDecl);
   typedef Map<void*, GFXMetalVertexDecl> GFXMetalVertexDeclMap;
   static GFXMetalVertexDeclMap declMap;   
   GFXMetalVertexDeclMap::Iterator itr = declMap.find( (void*)vertexFormat->getDescription().c_str() ); // description string are interned, safe to use c_str()
   if(itr != declMap.end())
      return &itr->value;

   GFXMetalVertexDecl &decl = declMap[(void*)vertexFormat->getDescription().c_str()];   
   //decl.init(vertexFormat);
   return &decl;
}

void GFXMetalDevice::setVertexDecl( const GFXVertexDecl *decl )
{
   //static_cast<const GFXMetalVertexDecl*>(decl)->prepareVertexFormat();
}

inline void GFXMetalDevice::preDrawPrimitive()
{
   if( mStateDirty )
   {
      updateStates();
   }
   
   if(mCurrentShaderConstBuffer)
      setShaderConstBufferInternal(mCurrentShaderConstBuffer);

   if( mNeedUpdateVertexAttrib )
   {
      AssertFatal(mCurrVertexDecl, "");
      const GFXMetalVertexDecl* decl = static_cast<const GFXMetalVertexDecl*>(mCurrVertexDecl);
      
      for(int i = 0; i < getNumVertexStreams(); ++i)
      {
         if(mCurrentVB[i])
         {
            //mCurrentVB[i]->prepare(i, mCurrentVB_Divisor[i]);    // GL_ARB_vertex_attrib_binding
            //decl->prepareBuffer_old( i, mCurrentVB[i]->mBuffer, mCurrentVB_Divisor[i] ); // old vertex buffer/format
         }
      }

      //decl->updateActiveVertexAttrib( GFXMetal->getOpenglCache()->getCacheVertexAttribActive() );
   }   

   mNeedUpdateVertexAttrib = false;
}

inline void GFXMetalDevice::postDrawPrimitive(U32 primitiveCount)
{
   mDeviceStatistics.mDrawCalls++;
   mDeviceStatistics.mPolyCount += primitiveCount;
}

void GFXMetalDevice::drawPrimitive( GFXPrimitiveType primType, U32 vertexStart, U32 primitiveCount ) 
{
   preDrawPrimitive();
  
   if(mDrawInstancesCount)
   {

   }
   else
   {

   }

   postDrawPrimitive(primitiveCount);
}

void GFXMetalDevice::drawIndexedPrimitive(   GFXPrimitiveType primType, 
                                          U32 startVertex, 
                                          U32 minIndex, 
                                          U32 numVerts, 
                                          U32 startIndex, 
                                          U32 primitiveCount )
{
   preDrawPrimitive();

   if(mDrawInstancesCount)
   {

   }
   else
   {
      
   }

   postDrawPrimitive(primitiveCount);
}

void GFXMetalDevice::setPB(GFXMetalPrimitiveBuffer* pb)
{
   mCurrentPB = pb;
}

void GFXMetalDevice::setTextureInternal(U32 textureUnit, const GFXTextureObject*texture)
{

}

void GFXMetalDevice::setCubemapInternal(U32 textureUnit, const GFXMetalCubemap* texture)
{

}

void GFXMetalDevice::setCubemapArrayInternal(U32 textureUnit, const GFXMetalCubemapArray* texture)
{

}

void GFXMetalDevice::setTextureArrayInternal(U32 textureUnit, const GFXMetalTextureArray* texture)
{

}

void GFXMetalDevice::setClipRect(const RectI& inRect)
{
   AssertFatal(mCurrentRT.isValid(), "GFXMetalDevice::setClipRect - must have a render target set to do any rendering operations!");

   // Clip the rect against the renderable size.
   Point2I size = mCurrentRT->getSize();
   RectI maxRect(Point2I(0, 0), size);
   mClip = inRect;
   mClip.intersect(maxRect);

   // Create projection matrix.  See http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/ortho.html
   const F32 left = mClip.point.x;
   const F32 right = mClip.point.x + mClip.extent.x;
   const F32 bottom = mClip.extent.y;
   const F32 top = 0.0f;
   const F32 nearPlane = 0.0f;
   const F32 farPlane = 1.0f;

   const F32 tx = -(right + left) / (right - left);
   const F32 ty = -(top + bottom) / (top - bottom);
   const F32 tz = -(farPlane + nearPlane) / (farPlane - nearPlane);

   static Point4F pt;
   pt.set(2.0f / (right - left), 0.0f, 0.0f, 0.0f);
   mProjectionMatrix.setColumn(0, pt);

   pt.set(0.0f, 2.0f / (top - bottom), 0.0f, 0.0f);
   mProjectionMatrix.setColumn(1, pt);

   pt.set(0.0f, 0.0f, -2.0f / (farPlane - nearPlane), 0.0f);
   mProjectionMatrix.setColumn(2, pt);

   pt.set(tx, ty, tz, 1.0f);
   mProjectionMatrix.setColumn(3, pt);

   // Translate projection matrix.
   static MatrixF translate(true);
   pt.set(0.0f, -mClip.point.y, 0.0f, 1.0f);
   translate.setColumn(3, pt);

   mProjectionMatrix *= translate;

   MatrixF mTempMatrix(true);
   setViewMatrix(mTempMatrix);
   setWorldMatrix(mTempMatrix);

   // Set the viewport to the clip rect
   RectI viewport(mClip.point.x, mClip.point.y, mClip.extent.x, mClip.extent.y);
   setViewport(viewport);
}

/// Creates a state block object based on the desc passed in.  This object
/// represents an immutable state.
GFXStateBlockRef GFXMetalDevice::createStateBlockInternal(const GFXStateBlockDesc& desc)
{
   return GFXStateBlockRef(new GFXMetalStateBlock(desc));
}

/// Activates a stateblock
void GFXMetalDevice::setStateBlockInternal(GFXStateBlock* block, bool force)
{
   AssertFatal(dynamic_cast<GFXMetalStateBlock*>(block), "GFXMetalDevice::setStateBlockInternal - Incorrect stateblock type for this device!");
   GFXMetalStateBlock* glBlock = static_cast<GFXMetalStateBlock*>(block);
   GFXMetalStateBlock* glCurrent = static_cast<GFXMetalStateBlock*>(mCurrentStateBlock.getPointer());
   if (force)
      glCurrent = NULL;
      
   glBlock->activate(glCurrent); // Doesn't use current yet.
   mCurrentGLStateBlock = glBlock;
}

//------------------------------------------------------------------------------

GFXTextureTarget * GFXMetalDevice::allocRenderToTextureTarget(bool genMips)
{
   GFXMetalTextureTarget *targ = new GFXMetalTextureTarget(genMips);
   targ->registerResourceWithDevice(this);
   return targ;
}

GFXFence * GFXMetalDevice::createFence()
{
   //GFXFence* fence = _createPlatformSpecificFence();
   //if(!fence)
      //fence = new GFXGeneralFence( this );
      
   //fence->registerResourceWithDevice(this);
   //return fence;
   return NULL;
}

GFXOcclusionQuery* GFXMetalDevice::createOcclusionQuery()
{   
   GFXOcclusionQuery *query = new GFXMetalOcclusionQuery( this );
   query->registerResourceWithDevice(this);
   return query;
}

void GFXMetalDevice::setupGenericShaders( GenericShaderType type ) 
{
   AssertFatal(type != GSTargetRestore, "");

   if (mGenericShader[GSColor] == NULL)
   {
      ShaderData* shaderData;

      shaderData = new ShaderData();
      shaderData->setField("DXVertexShaderFile", ShaderGen::smCommonShaderPath + String("/fixedFunction/colorV.hlsl"));
      shaderData->setField("DXPixelShaderFile", ShaderGen::smCommonShaderPath + String("/fixedFunction/colorP.hlsl"));
      shaderData->registerObject();
      mGenericShader[GSColor] = shaderData->getShader();
      mGenericShaderBuffer[GSColor] = mGenericShader[GSColor]->allocConstBuffer();
      mModelViewProjSC[GSColor] = mGenericShader[GSColor]->getShaderConstHandle("$modelView");
      Sim::getRootGroup()->addObject(shaderData);

      shaderData = new ShaderData();
      shaderData->setField("DXVertexShaderFile", ShaderGen::smCommonShaderPath + String("/fixedFunction/modColorTextureV.hlsl"));
      shaderData->setField("DXPixelShaderFile", ShaderGen::smCommonShaderPath + String("/fixedFunction/modColorTextureP.hlsl"));
      shaderData->setSamplerName("$diffuseMap", 0);
      shaderData->registerObject();
      mGenericShader[GSModColorTexture] = shaderData->getShader();
      mGenericShaderBuffer[GSModColorTexture] = mGenericShader[GSModColorTexture]->allocConstBuffer();
      mModelViewProjSC[GSModColorTexture] = mGenericShader[GSModColorTexture]->getShaderConstHandle("$modelView");
      Sim::getRootGroup()->addObject(shaderData);

      shaderData = new ShaderData();
      shaderData->setField("DXVertexShaderFile", ShaderGen::smCommonShaderPath + String("/fixedFunction/addColorTextureV.hlsl"));
      shaderData->setField("DXPixelShaderFile", ShaderGen::smCommonShaderPath + String("/fixedFunction/addColorTextureP.hlsl"));
      shaderData->setSamplerName("$diffuseMap", 0);
      shaderData->registerObject();
      mGenericShader[GSAddColorTexture] = shaderData->getShader();
      mGenericShaderBuffer[GSAddColorTexture] = mGenericShader[GSAddColorTexture]->allocConstBuffer();
      mModelViewProjSC[GSAddColorTexture] = mGenericShader[GSAddColorTexture]->getShaderConstHandle("$modelView");
      Sim::getRootGroup()->addObject(shaderData);

      shaderData = new ShaderData();
      shaderData->setField("DXVertexShaderFile", ShaderGen::smCommonShaderPath + String("/fixedFunction/textureV.hlsl"));
      shaderData->setField("DXPixelShaderFile", ShaderGen::smCommonShaderPath + String("/fixedFunction/textureP.hlsl"));
      shaderData->setSamplerName("$diffuseMap", 0);
      shaderData->registerObject();
      mGenericShader[GSTexture] = shaderData->getShader();
      mGenericShaderBuffer[GSTexture] = mGenericShader[GSTexture]->allocConstBuffer();
      mModelViewProjSC[GSTexture] = mGenericShader[GSTexture]->getShaderConstHandle("$modelView");
      Sim::getRootGroup()->addObject(shaderData);
   }

   MatrixF tempMatrix =  mProjectionMatrix * mViewMatrix * mWorldMatrix[mWorldStackSize];  
   mGenericShaderBuffer[type]->setSafe(mModelViewProjSC[type], tempMatrix);

   setShader( mGenericShader[type] );
   setShaderConstBuffer( mGenericShaderBuffer[type] );
}

GFXShader* GFXMetalDevice::createShader()
{
   GFXMetalShader* shader = new GFXMetalShader();
   shader->registerResourceWithDevice( this );
   return shader;
}

void GFXMetalDevice::setShader(GFXShader *shader, bool force)
{
   if(mCurrentShader == shader && !force)
      return;

   if ( shader )
   {
      GFXMetalShader *glShader = static_cast<GFXMetalShader*>( shader );
      //glShader->bindProgramPipeline();
      mCurrentShader = shader;
   }
   else
   {
      setupGenericShaders();
   }
}

void GFXMetalDevice::setShaderConstBufferInternal(GFXShaderConstBuffer* buffer)
{
   if (buffer)
   {
      PROFILE_SCOPE(GFXMetalDevice_setShaderConstBufferInternal);
      AssertFatal(static_cast<GFXMetalShaderConstBuffer*>(buffer), "Incorrect shader const buffer type for this device!");
      GFXMetalShaderConstBuffer* glBuffer = static_cast<GFXMetalShaderConstBuffer*>(buffer);

      glBuffer->activate(mCurrentConstBuffer);
      mCurrentConstBuffer = glBuffer;
   }
   else
   {
      mCurrentConstBuffer = NULL;
   }
}

U32 GFXMetalDevice::getNumSamplers() const
{
   return getMin((U32)GFX_TEXTURE_STAGE_COUNT, mShaderModel > 0.001f ? mMaxShaderTextures : mMaxFFTextures);
}

GFXTextureObject* GFXMetalDevice::getDefaultDepthTex() const 
{
   //if(mWindowRT && mWindowRT->getPointer())
      //return static_cast<GFXMetalWindowTarget*>( mWindowRT->getPointer() )->mBackBufferDepthTex.getPointer();

   return NULL;
}

U32 GFXMetalDevice::getNumRenderTargets() const 
{ 
   return mMaxTRColors; 
}

void GFXMetalDevice::_updateRenderTargets()
{
   if ( mRTDirty || mCurrentRT->isPendingState() )
   {
      if ( mRTDeactivate )
      {
         mRTDeactivate->deactivate();
         mRTDeactivate = NULL;   
      }
      
      // NOTE: The render target changes is not really accurate
      // as the GFXTextureTarget supports MRT internally.  So when
      // we activate a GFXTarget it could result in multiple calls
      // to SetRenderTarget on the actual device.
      mDeviceStatistics.mRenderTargetChanges++;

      GFXMetalTextureTarget *tex = dynamic_cast<GFXMetalTextureTarget*>( mCurrentRT.getPointer() );
      if ( tex )
      {
         //tex->applyState();
         //tex->makeActive();
      }
      else
      {
         GFXMetalWindowTarget *win = dynamic_cast<GFXMetalWindowTarget*>( mCurrentRT.getPointer() );
         AssertFatal( win != NULL, 
                     "GFXMetalDevice::_updateRenderTargets() - invalid target subclass passed!" );
         
         //win->makeActive();
         
         //if( win->mContext != static_cast<GFXMetalDevice*>(GFX)->mContext )
         //{
         //   mRTDirty = false;
         //   GFX->updateStates(true);
         //}
      }
      
      mRTDirty = false;
   }
   
   if ( mViewportDirty )
   {
      mViewportDirty = false;
   }
}

GFXFormat GFXMetalDevice::selectSupportedFormat(   GFXTextureProfile* profile, 
                                                const Vector<GFXFormat>& formats, 
                                                bool texture, 
                                                bool mustblend,
                                                bool mustfilter )
{
   for(U32 i = 0; i < formats.size(); i++)
   {
      // Single channel textures are not supported by FBOs.
      if(profile->testFlag(GFXTextureProfile::RenderTarget) && (formats[i] == GFXFormatA8 || formats[i] == GFXFormatL8 || formats[i] == GFXFormatL16))
         continue;
      //if(GFXMetalTextureInternalFormat[formats[i]] == GL_ZERO)
      //   continue;
      
      return formats[i];
   }
   
   return GFXFormatR8G8B8A8;
}

U32 GFXMetalDevice::getTotalVideoMemory()
{
   return 0;
}

//
// Register this device with GFXInit
//
class GFXMetalRegisterDevice
{
public:
   GFXMetalRegisterDevice()
   {
      GFXInit::getRegisterDeviceSignal().notify(&GFXMetalDevice::enumerateAdapters);
   }
};

static GFXMetalRegisterDevice pGLRegisterDevice;

//DefineEngineFunction(cycleResources, void, (),, "")
//{
//   static_cast<GFXMetalDevice*>(GFX)->zombify();
//   static_cast<GFXMetalDevice*>(GFX)->resurrect();
//}
