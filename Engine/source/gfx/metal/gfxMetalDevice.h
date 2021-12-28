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

#ifndef _GFXMETALDEVICE_
#define _GFXMETALDEVICE_

#include "platform/platform.h"

#include "gfx/gfxDevice.h"
#include "gfx/gfxInit.h"

#include "windowManager/platformWindow.h"
#include "gfx/gfxFence.h"
#include "gfx/gfxResource.h"
#include "gfx/metal/gfxMetalStateBlock.h"

#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>

class GFXMetalTextureArray;
class GFXMetalVertexBuffer;
class GFXMetalPrimitiveBuffer;
class GFXMetalTextureTarget;
class GFXMetalCubemap;
class GFXMetalCubemapArray;
class GFXMetalStateCache;
class GFXMetalVertexDecl;
class GFXMetalShaderConstBuffer;

class GFXMetalDevice : public GFXDevice
{
public:
   struct Capabilities
   {
      bool anisotropicFiltering;
      bool bufferStorage;
      bool textureStorage;
      bool copyImage;
      bool vertexAttributeBinding;
      bool khrDebug;
      bool extDebugMarker;
   };
   Capabilities mCapabilities;

   void zombify();
   void resurrect();
   GFXMetalDevice(U32 adapterIndex);
   virtual ~GFXMetalDevice();

   static void enumerateAdapters( Vector<GFXAdapter*> &adapterList );
   static GFXDevice *createInstance( U32 adapterIndex );

   virtual void init( const GFXVideoMode &mode, PlatformWindow *window = NULL );
   void destroyPlatform();

   virtual void activate() { }
   virtual void deactivate() { }
   virtual GFXAdapterType getAdapterType() { return OpenGL; }

   virtual void enterDebugEvent(const ColorI& color, const char *name) {}
   virtual void leaveDebugEvent() {}
   virtual void setDebugMarker(const ColorI& color, const char *name) {}

   virtual void enumerateVideoModes();

   virtual U32 getTotalVideoMemory();

   virtual GFXCubemap * createCubemap();
   virtual GFXCubemapArray *createCubemapArray();
   virtual GFXTextureArray *createTextureArray();

   virtual F32 getFillConventionOffset() const { return 0.0f; }


   ///@}

   /// @name Render Target functions
   /// @{

   ///
   virtual GFXTextureTarget *allocRenderToTextureTarget(bool genMips = true);
   virtual GFXWindowTarget *allocWindowTarget(PlatformWindow *window);
   virtual void _updateRenderTargets();

   ///@}

   /// @name Shader functions
   /// @{
  
   virtual void setShader(GFXShader *shader, bool force = false);
   
   /// @attention GL cannot check if the given format supports blending or filtering!
   virtual GFXFormat selectSupportedFormat(GFXTextureProfile *profile,
	   const Vector<GFXFormat> &formats, bool texture, bool mustblend, bool mustfilter);
      
   /// Returns the number of texture samplers that can be used in a shader rendering pass
   virtual U32 getNumSamplers() const;

   /// Returns the number of simultaneous render targets supported by the device.
   virtual U32 getNumRenderTargets() const;

   virtual GFXShader* createShader();
   //TODO: implement me!
   virtual void copyResource(GFXTextureObject *pDst, GFXCubemap *pSrc, const U32 face);
   virtual void clear( U32 flags, const LinearColorF& color, F32 z, U32 stencil );
   virtual void clearColorAttachment(const U32 attachment, const LinearColorF& color);
   virtual bool beginSceneInternal();
   virtual void endSceneInternal();

   virtual void drawPrimitive( GFXPrimitiveType primType, U32 vertexStart, U32 primitiveCount );

   virtual void drawIndexedPrimitive(  GFXPrimitiveType primType, 
                                       U32 startVertex, 
                                       U32 minIndex, 
                                       U32 numVerts, 
                                       U32 startIndex, 
                                       U32 primitiveCount );

   virtual void setClipRect( const RectI &rect );
   virtual const RectI &getClipRect() const { return mClip; }

   virtual void preDestroy() { Parent::preDestroy(); }

   virtual U32 getMaxDynamicVerts() { return GFX_MAX_DYNAMIC_VERTS; }
   virtual U32 getMaxDynamicIndices() { return GFX_MAX_DYNAMIC_INDICES; }
   
   GFXFence *createFence();
   
   GFXOcclusionQuery* createOcclusionQuery();

   GFXMetalStateBlockRef getCurrentStateBlock() { return mCurrentGLStateBlock; }
   
   virtual void setupGenericShaders( GenericShaderType type = GSColor );
   
   ///
   bool supportsAnisotropic() const { return mCapabilities.anisotropicFiltering; }

   GFXTextureObject* getDefaultDepthTex() const;

   /// Returns the number of vertex streams supported by the device.	
   const U32 getNumVertexStreams() const { return mNumVertexStream; }

   const char* interpretDebugResult(long result) { return "Not Implemented"; };
   
   inline id<MTLDevice> getDevice() { return mDevice; }
protected:   
   /// Called by GFXDevice to create a device specific stateblock
   virtual GFXStateBlockRef createStateBlockInternal(const GFXStateBlockDesc& desc);
   /// Called by GFXDevice to actually set a stateblock.
   virtual void setStateBlockInternal(GFXStateBlock* block, bool force);

   /// Track the last const buffer we've used.  Used to notify new constant buffers that
   /// they should send all of their constants up
   StrongRefPtr<GFXMetalShaderConstBuffer> mCurrentConstBuffer;
   /// Called by base GFXDevice to actually set a const buffer
   virtual void setShaderConstBufferInternal(GFXShaderConstBuffer* buffer);

   virtual void setTextureInternal(U32 textureUnit, const GFXTextureObject*texture);
   virtual void setCubemapInternal(U32 textureUnit, const GFXMetalCubemap* texture);
   virtual void setCubemapArrayInternal(U32 textureUnit, const GFXMetalCubemapArray* texture);
   virtual void setTextureArrayInternal(U32 textureUnit, const GFXMetalTextureArray* texture);

   /// @name State Initalization.
   /// @{

   /// State initalization. This MUST BE CALLED in setVideoMode after the device
   /// is created.
   virtual void initStates() { }

   virtual GFXVertexBuffer *allocVertexBuffer(  U32 numVerts, 
                                                const GFXVertexFormat *vertexFormat,
                                                U32 vertSize, 
                                                GFXBufferType bufferType,
                                                void* data = NULL);
   virtual GFXPrimitiveBuffer *allocPrimitiveBuffer( U32 numIndices, U32 numPrimitives, GFXBufferType bufferType, void* data = NULL );
   
   // NOTE: The GL device doesn't need a vertex declaration at
   // this time, but we need to return something to keep the system
   // from retrying to allocate one on every call.
   virtual GFXVertexDecl* allocVertexDecl( const GFXVertexFormat *vertexFormat );

   virtual void setVertexDecl( const GFXVertexDecl *decl );

   virtual void setVertexStream( U32 stream, GFXVertexBuffer *buffer );
   virtual void setVertexStreamFrequency( U32 stream, U32 frequency );

private:
   typedef GFXDevice Parent;
   
   friend class GFXMetalTextureObject;
   friend class GFXMetalCubemap;
   friend class GFXMetalCubemapArray;
   friend class GFXMetalTextureArray;
   friend class GFXMetalWindowTarget;
   friend class GFXMetalPrimitiveBuffer;
   friend class GFXMetalVertexBuffer;
   
   void* mContext;
   CAMetalLayer* mSwapChain;
   id<MTLDevice> mDevice;
   id<MTLCommandQueue> mCommandQueue;
   id<MTLCommandBuffer> mCommandBuffer;
   id<MTLRenderCommandEncoder> mRenderEncoder;

   static GFXAdapter::CreateDeviceInstanceDelegate mCreateDeviceInstance;

   U32 mAdapterIndex;
   
   StrongRefPtr<GFXMetalVertexBuffer> mCurrentVB[VERTEX_STREAM_COUNT];
   U32 mCurrentVB_Divisor[VERTEX_STREAM_COUNT];
   bool mNeedUpdateVertexAttrib;
   StrongRefPtr<GFXMetalPrimitiveBuffer> mCurrentPB;
   U32 mDrawInstancesCount;
   
   GFXShader* mCurrentShader;
   GFXShaderRef mGenericShader[GS_COUNT];
   GFXShaderConstBufferRef mGenericShaderBuffer[GS_COUNT];
   GFXShaderConstHandle *mModelViewProjSC[GS_COUNT];
   
   /// Since GL does not have separate world and view matrices we need to track them
   MatrixF m_mCurrentWorld;
   MatrixF m_mCurrentView;

   void* mPixelFormat; 

   U32 mNumVertexStream;
   
   U32 mMaxShaderTextures;
   U32 mMaxFFTextures;

   U32 mMaxTRColors;

   RectI mClip;
   
   GFXMetalStateBlockRef mCurrentGLStateBlock;
   
   Vector< StrongRefPtr<GFXMetalVertexBuffer> > mVolatileVBs; ///< Pool of existing volatile VBs so we can reuse previously created ones
   Vector< StrongRefPtr<GFXMetalPrimitiveBuffer> > mVolatilePBs; ///< Pool of existing volatile PBs so we can reuse previously created ones

   void preDrawPrimitive();
   void postDrawPrimitive(U32 primitiveCount);  
   
   GFXVertexBuffer* findVolatileVBO(U32 numVerts, const GFXVertexFormat *vertexFormat, U32 vertSize); ///< Returns an existing volatile VB which has >= numVerts and the same vert flags/size, or creates a new VB if necessary
   GFXPrimitiveBuffer* findVolatilePBO(U32 numIndices, U32 numPrimitives); ///< Returns an existing volatile PB which has >= numIndices, or creates a new PB if necessary

   void vsyncCallback(); ///< Vsync callback
   
   void initGLState(); ///< Guaranteed to be called after all extensions have been loaded, use to init card profiler, shader version, max samplers, etc.
   
   void setPB(GFXMetalPrimitiveBuffer* pb); ///< Sets mCurrentPB

   GFXWindowTargetRef *mWindowRT;
};

#define GFXMetal static_cast<GFXMetalDevice*>(GFXDevice::get())
#endif
