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

#ifndef _GFXGLDEVICE_H_
#define _GFXGLDEVICE_H_

#include "platform/platform.h"

#include "gfx/gfxDevice.h"
#include "gfx/gfxInit.h"

#include "gfx/gl/tGL/tGL.h"

#include "windowManager/platformWindow.h"
#include "gfx/gfxFence.h"
#include "gfx/gfxResource.h"
#include "gfx/gl/gfxGLStateBlock.h"

#include <vector>
#include <unordered_map>
#include <variant>

class GFXGLTextureArray;
class GFXGLVertexBuffer;
class GFXGLPrimitiveBuffer;
class GFXGLTextureTarget;
class GFXGLCubemap;
class GFXGLCubemapArray;
class GFXGLStateCache;
class GFXGLVertexDecl;

class GFXGLDevice : public GFXDevice
{
public:
   struct GLCapabilities
   {
      bool anisotropicFiltering;
      bool bufferStorage;
      bool textureStorage;
      bool copyImage;
      bool vertexAttributeBinding;
      bool khrDebug;
      bool extDebugMarker;
   };
   GLCapabilities mCapabilities;

   void zombify();
   void resurrect();
   GFXGLDevice(U32 adapterIndex);
   virtual ~GFXGLDevice();

   static void enumerateAdapters( Vector<GFXAdapter*> &adapterList );
   static GFXDevice *createInstance( U32 adapterIndex );

   virtual void init( const GFXVideoMode &mode, PlatformWindow *window = NULL );

   virtual void activate() { }
   virtual void deactivate() { }
   virtual GFXAdapterType getAdapterType() { return OpenGL; }

   virtual void enterDebugEvent(ColorI color, const char *name);
   virtual void leaveDebugEvent();
   virtual void setDebugMarker(ColorI color, const char *name);

   virtual void enumerateVideoModes();

   virtual U32 getTotalVideoMemory_GL_EXT();
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
   virtual F32 getPixelShaderVersion() const { return mPixelShaderVersion; }
   virtual void  setPixelShaderVersion( F32 version ) { mPixelShaderVersion = version; }
   
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

   GFXGLStateBlockRef getCurrentStateBlock() { return mCurrentGLStateBlock; }
   
   virtual void setupGenericShaders( GenericShaderType type = GSColor );
   
   ///
   bool supportsAnisotropic() const { return mCapabilities.anisotropicFiltering; }

   GFXGLStateCache* getOpenglCache() { return mOpenglStateCache; }

   GFXTextureObject* getDefaultDepthTex() const;

   /// Returns the number of vertex streams supported by the device.	
   const U32 getNumVertexStreams() const { return mNumVertexStream; }

   bool glUseMap() const { return mUseGlMap; }   
   const char* interpretDebugResult(long result) { return "Not Implemented"; };
protected:   
   /// Called by GFXDevice to create a device specific stateblock
   virtual GFXStateBlockRef createStateBlockInternal(const GFXStateBlockDesc& desc);
   /// Called by GFXDevice to actually set a stateblock.
   virtual void setStateBlockInternal(GFXStateBlock* block, bool force);   

   /// Called by base GFXDevice to actually set a const buffer
   virtual void setShaderConstBufferInternal(GFXShaderConstBuffer* buffer);

   virtual void setTextureInternal(U32 textureUnit, const GFXTextureObject*texture);
   virtual void setCubemapInternal(U32 textureUnit, const GFXGLCubemap* texture);
   virtual void setCubemapArrayInternal(U32 textureUnit, const GFXGLCubemapArray* texture);
   virtual void setTextureArrayInternal(U32 textureUnit, const GFXGLTextureArray* texture);

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
   
   friend class GFXGLTextureObject;
   friend class GFXGLCubemap;
   friend class GFXGLCubemapArray;
   friend class GFXGLTextureArray;
   friend class GFXGLWindowTarget;
   friend class GFXGLPrimitiveBuffer;
   friend class GFXGLVertexBuffer;

   static GFXAdapter::CreateDeviceInstanceDelegate mCreateDeviceInstance;

   U32 mAdapterIndex;
   
   StrongRefPtr<GFXGLVertexBuffer> mCurrentVB[VERTEX_STREAM_COUNT];
   U32 mCurrentVB_Divisor[VERTEX_STREAM_COUNT];
   bool mNeedUpdateVertexAttrib;
   StrongRefPtr<GFXGLPrimitiveBuffer> mCurrentPB;
   U32 mDrawInstancesCount;
   
   GFXShader* mCurrentShader;
   GFXShaderRef mGenericShader[GS_COUNT];
   GFXShaderConstBufferRef mGenericShaderBuffer[GS_COUNT];
   GFXShaderConstHandle *mModelViewProjSC[GS_COUNT];
   
   /// Since GL does not have separate world and view matrices we need to track them
   MatrixF m_mCurrentWorld;
   MatrixF m_mCurrentView;

   void* mContext;
   void* mPixelFormat;

   F32 mPixelShaderVersion;

   U32 mNumVertexStream;
   
   U32 mMaxShaderTextures;
   U32 mMaxFFTextures;

   U32 mMaxTRColors;

   RectI mClip;
   
   GFXGLStateBlockRef mCurrentGLStateBlock;
   
   GLenum mActiveTextureType[GFX_TEXTURE_STAGE_COUNT];
   
   Vector< StrongRefPtr<GFXGLVertexBuffer> > mVolatileVBs; ///< Pool of existing volatile VBs so we can reuse previously created ones
   Vector< StrongRefPtr<GFXGLPrimitiveBuffer> > mVolatilePBs; ///< Pool of existing volatile PBs so we can reuse previously created ones

   GLsizei primCountToIndexCount(GFXPrimitiveType primType, U32 primitiveCount);
   void preDrawPrimitive();
   void postDrawPrimitive(U32 primitiveCount);  
   
   GFXVertexBuffer* findVolatileVBO(U32 numVerts, const GFXVertexFormat *vertexFormat, U32 vertSize); ///< Returns an existing volatile VB which has >= numVerts and the same vert flags/size, or creates a new VB if necessary
   GFXPrimitiveBuffer* findVolatilePBO(U32 numIndices, U32 numPrimitives); ///< Returns an existing volatile PB which has >= numIndices, or creates a new PB if necessary

   void vsyncCallback(); ///< Vsync callback
   
   void initGLState(); ///< Guaranteed to be called after all extensions have been loaded, use to init card profiler, shader version, max samplers, etc.
   
   GFXFence* _createPlatformSpecificFence(); ///< If our platform (e.g. OS X) supports a fence extenstion (e.g. GL_APPLE_fence) this will create one, otherwise returns NULL
   
   void setPB(GFXGLPrimitiveBuffer* pb); ///< Sets mCurrentPB

   GFXGLStateCache *mOpenglStateCache;

   GFXWindowTargetRef *mWindowRT;

   bool mUseGlMap;
   
   // gfx2
private:
   friend class GFXCmdBuffer;

   struct GLBindGroupLayout
   {
      struct Layout
      {
         String name;
         U32 binding;
         GFXResourceType type;
         GFXShaderStageFlags stages;
      } resources[GFX_MAX_RESOURCES_PER_BINDGROUP];

      U32 resourceCount;
      U32 numberOfDynamicResources;
   };

   struct GLInputLayoutState
   {
      struct
      {
         struct
         {
            U32 attributeIndex;
            GLenum type;
            U32 elementCount;
            U32 localOffset;
            bool normalized;
         } elements[GFX_MAX_INPUT_LAYOUT_ATTRIBUTES];

         U32 elementCount;
         U32 stride;
         bool isInstancing;

      } buffers[GFX_MAX_BOUND_VERTEX_BUFFERS];
      U32 bufferCount = 0;
      U32 numberOfAttributes = 0;
   };

   struct GLPipelineState
   {
      U32 bindGroupLayout;
      GLInputLayoutState inputLayoutState;
      GLuint program;
      U32 pushConstantSize;
   };

   struct GLBindGroup
   {
      struct ConstantBuffer
      {
         GFXHandle<GFXConstantBufferHandle> buffer;
         U32 offset = 0;
         U32 size = 0;
      };
      struct Sampler
      {
         GFXHandle<GFXSamplerHandle> sampler;
         U32 textureUnit;
      };
      struct Texture
      {
         GFXHandle<GFXTextureHandle> texture;
         U32 textureUnit;
      };
      

      U32 count = 0;
      struct Resources
      {
         GFXResourceType type;
         std::variant<ConstantBuffer, Sampler, Texture> resource;
      } resources[GFX_MAX_RESOURCES_PER_BINDGROUP];
      
      GFXHandle<GFXBindGroupLayoutHandle> bindGroupLayout;
   };
   
   struct GLTextureState
   {
      GLenum type;
      U32 width = 0;
      U32 height = 0;
      U32 depth = 0;
      U32 numberLayers = 0;
      GLenum format = 0;
      GFXTextureProfile* profile;
      U32 numMipLevels = 1;
      GLuint textureHandle;
   };
   
   U32 mNextHandle = 0;
   
   std::unordered_map<U32, GLPipelineState> mGLPipelineStateTable;
   std::unordered_map<U32, GLBindGroup> mGLBindGroupTable;
   std::unordered_map<U32, GLBindGroupLayout> mGLBindGroupLayoutTable;
   std::unordered_map<U32, GLuint> mGLSamplerStateTable;
   std::unordered_map<U32, GLuint> mGLVertexBufferTable;
   std::unordered_map<U32, GLuint> mGLIndexBufferTable;
   std::unordered_map<U32, GLuint> mGLConstantBufferTable;
   std::unordered_map<U32, GLTextureState> mGLTextureTable;

   GLuint mPushConstantBuffer;
   GLint mPushConstantBindingLocation;

   // capabilities
   struct
   {
      bool separateVertexFormat = false;
      bool bufferStorage = false;
      bool anisotropicFiltering = false;
      S32 uboAlignment = 0;
   } mGLCaps;

   enum
   {
      MAX_ATTRIBUTES = 16,
      MAX_PUSH_CONSTANT_SIZE = 128,
      MAX_PUSH_CONSTANT_CALLS_PER_CMD_BUFFER = 512
   };

   struct
   {
      bool attributesBound[MAX_ATTRIBUTES];
      U32 currentPipeline = 0;
      U32 currentBindGroupLayout = 0;
   } mInternalGLState;

   void verifyBeforeDraw();
   void init2();

public:
   virtual GFXCmdBuffer* allocCmdBuffer() override;
   virtual void executeCmdBuffer(GFXCmdBuffer* cmdBuffer) override;

   virtual GFXHandle<GFXSamplerHandle> createSampler(const GFXSamplerDesc2& desc) override;
   virtual void destroySampler(GFXHandle<GFXSamplerHandle> sampler) override;

   virtual GFXHandle<GFXTextureHandle> createTexture(const GFXTextureDesc& desc) override;
   virtual void destroyTexture(GFXHandle<GFXTextureHandle> texture) override;

   virtual GFXHandle<GFXVertexBufferHandle2> createVertexBuffer(const GFXVertexBufferDesc& desc) override;
   virtual void destroyVertexBuffer(GFXHandle<GFXVertexBufferHandle2> vertexBuffer) override;

   virtual GFXHandle<GFXIndexBufferHandle> createIndexBuffer(const GFXIndexBufferDesc& desc) override;
   virtual void destroyIndexBuffer(GFXHandle<GFXIndexBufferHandle> indexBuffer) override;

   virtual GFXHandle<GFXConstantBufferHandle> createConstantBuffer(const GFXConstBufferDesc& desc) override;
   virtual void destroyConstantBuffer(GFXHandle<GFXConstantBufferHandle> constantBuffer) override;

   virtual GFXHandle<GFXGraphicsPipelineHandle> createGraphicsPipeline(const GFXPipelineDesc& desc) override;
   virtual void destroyGraphicsPipeline(GFXHandle<GFXGraphicsPipelineHandle> pipeline) override;

   virtual GFXHandle<GFXBindGroupLayoutHandle> createBindGroupLayout(const GFXBindGroupLayoutDesc& desc) override;
   virtual void deleteBindGroupLayout(GFXHandle<GFXBindGroupLayoutHandle> layout) override;

   virtual GFXHandle<GFXBindGroupHandle> createBindGroup(GFXHandle<GFXBindGroupLayoutHandle> bindGroupLayoutHandle, const GFXBindGroupDesc& desc) override;
   virtual void deleteBindGroup(GFXHandle<GFXBindGroupHandle> bindGroup) override;
};

#define GFXGL static_cast<GFXGLDevice*>(GFXDevice::get())
#endif
