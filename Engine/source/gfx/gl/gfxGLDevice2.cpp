#include "gfx/gl/gfxGLDevice.h"
#include "gfx/gl/gfxGLEnumTranslate.h"
#include "gfx/gl/gfxGLVertexAttribLocation.h"
#include "gfx/gl/gfxGLUtils.h"
#include "gfx/gl/gfxGLTextureObject.h"
#include "gfx/gfxCmdBuffer.h"

GFXCmdBuffer* GFXGLDevice::allocCmdBuffer()
{
   return NULL;
}

#if TORQUE_DEBUG
void GFXGLDevice::verifyBeforeDraw()
{
   // If we do not have separate vertex format, we must ensure that all attributes have been bound before drawing happens on the pipeline.
   if (!mGLCaps.separateVertexFormat)
   {
      const GLPipelineState& pipelineState = mGLPipelineStateTable[mInternalGLState.currentPipeline];
      AssertFatal(mGLPipelineStateTable.find(mInternalGLState.currentPipeline) != mGLPipelineStateTable.end(), "GFXGLDevice Command Buffer :: No Pipeline Bound!");

      for (S32 i = 0; i < pipelineState.inputLayoutState.numberOfAttributes; ++i)
      {
         AssertFatal(mInternalGLState.attributesBound[i], "GFXGLDevice Command Buffer :: Not all Attributes Bound to Vertex Buffers before drawing!");
      }
   }
}
#else
void GFXGLDevice::verifyBeforeDraw() {}
#endif

void GFXGLDevice::init2()
{
   // enable dummy VAO
   GLuint vao;
   glGenVertexArrays(1, &vao);
   glBindVertexArray(vao);

   //enable sRGB
   glEnable(GL_FRAMEBUFFER_SRGB);

   //enable seamless cubemapping
   glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS);

   // grab ubo alignment
   glGetIntegerv(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT, &mGLCaps.uboAlignment);
   mSizeOfPushConstant = mMax(mGLCaps.uboAlignment, MAX_PUSH_CONSTANT_SIZE);

   // initialize push constant buffer (TODO: make this a pool of buffers)
   glGenBuffers(1, &mPushConstantBuffer);
   glBindBuffer(GL_UNIFORM_BUFFER, mPushConstantBuffer);
   glBufferData(GL_UNIFORM_BUFFER, mSizeOfPushConstant * MAX_PUSH_CONSTANT_CALLS_PER_CMD_BUFFER, nullptr, GL_DYNAMIC_DRAW);
   glBindBuffer(GL_UNIFORM_BUFFER, 0);

   // cache push constant binding location as the last binding location
   GLint maxLocations;
   glGetIntegerv(GL_MAX_UNIFORM_BUFFER_BINDINGS, &maxLocations);
   mPushConstantBindingLocation = maxLocations - 1;
}

void GFXGLDevice::executeCmdBuffer(GFXCmdBuffer* cmdBuffer)
{
   GLenum currentIndexBufferByteSize = GL_NONE;
   GFXHandle<GFXIndexBufferHandle> currentBoundIndexBuffer(0);

   {
      // first thing we do, is upload the push constant buffer
      //
      // TODO: Performance...(this is really slow!)
      glBindBuffer(GL_UNIFORM_BUFFER, mPushConstantBuffer);
      void* buffer = glMapBuffer(GL_UNIFORM_BUFFER, GL_WRITE_ONLY);
      dMemcpy(buffer, cmdBuffer->mPushConstantMemory.data, cmdBuffer->mPushConstantMemory.count * mSizeOfPushConstant);
      glUnmapBuffer(GL_UNIFORM_BUFFER);
   }

   U32 cmdBufferOffset = 0;
   for (;;)
   {
      switch (cmdBuffer->mBuffer.data[cmdBufferOffset++])
      {
      case GFX_CMD_VIEWPORT:
         {
            S32 x = (S32)cmdBuffer->mBuffer.data[cmdBufferOffset++];
            S32 y = (S32)cmdBuffer->mBuffer.data[cmdBufferOffset++];
            S32 w = (S32)cmdBuffer->mBuffer.data[cmdBufferOffset++];
            S32 h = (S32)cmdBuffer->mBuffer.data[cmdBufferOffset++];
            glViewport(x, y, w, h);
            break;
         }
      case GFX_CMD_SET_GRAPHICS_PIPELINE:
         {
            U32 handle = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            const GLPipelineState &pipelineState = mGLPipelineStateTable[handle];
            mInternalGLState.currentPipeline = handle;
            
            // Cache bind group layout handle
            AssertFatal(mGLBindGroupLayoutTable.find(pipelineState.bindGroupLayout) != mGLBindGroupLayoutTable.end(), "GFXGLDevice Command Buffer :: Unable to find Bind Group Layout");
            mInternalGLState.currentBindGroupLayout = pipelineState.bindGroupLayout;

            // Disable from previous pipeline bind...
            for (U32 i = 0; i < MAX_ATTRIBUTES; ++i)
            {
               if (mInternalGLState.attributesBound[i])
               {
                  glDisableVertexAttribArray(i);
                  mInternalGLState.attributesBound[i] = false;
               }
            }

            if (mGLCaps.separateVertexFormat)
            {
               for (U32 i = 0; i < pipelineState.inputLayoutState.bufferCount; ++i)
               {
                  bool isInstancing = pipelineState.inputLayoutState.buffers[i].isInstancing;

                  for (U32 j = 0; j < pipelineState.inputLayoutState.buffers[i].elementCount; ++j)
                  {
                     GLint attributeLocation = pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex;
                     GLint elementCount = pipelineState.inputLayoutState.buffers[i].elements[j].elementCount;
                     GLenum type = pipelineState.inputLayoutState.buffers[i].elements[j].type;
                     GLboolean normalized = pipelineState.inputLayoutState.buffers[i].elements[j].normalized;
                     GLuint relativeOffset = pipelineState.inputLayoutState.buffers[i].elements[j].localOffset;

                     glEnableVertexAttribArray(attributeLocation);
                     glVertexAttribFormat(attributeLocation, elementCount, type, normalized, relativeOffset);
                     glVertexAttribDivisor(attributeLocation, isInstancing ? 1 : 0);

                     mInternalGLState.attributesBound[attributeLocation] = true;
                  }
               }
            }

            break;
         }
      case GFX_CMD_SET_STATE_BLOCK:
         {
            U32 stateBlockHandle = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            GFXStateBlockRef stateBlock = cmdBuffer->mStateBlockData.stateBlocks[stateBlockHandle];
            const GFXStateBlockDesc& state = stateBlock->getDesc();

            // Rasterizer State
            if (mInternalGLState.currentStateBlock != stateBlock)
            {
               if (state.zEnable)
                  glEnable(GL_DEPTH_TEST);
               else
                  glEnable(GL_DEPTH_TEST);

               if (state.zBias)
               {
                  glDisable(GL_POLYGON_OFFSET_FILL);
               }
               else
               {
                  //this assumes 24bit depth
                  const F32 depthMul = F32((1 << 24) - 1);
                  glEnable(GL_POLYGON_OFFSET_FILL);
                  glPolygonOffset(state.zSlopeBias, state.zBias * depthMul);
               }

               if (state.cullMode)
               {
                  glEnable(GL_CULL_FACE);
                  glCullFace(GFXGLCullMode[state.cullMode]);
               }
               else
               {
                  glDisable(GL_CULL_FACE);
               }

               glPolygonMode(GL_FRONT_AND_BACK, GFXGLFillMode[state.fillMode]);
            }

            // Depth Stencil State
            {
               glDepthMask(state.zWriteEnable);
               glDepthFunc(state.zFunc);

               if (state.stencilEnable)
               {
                  glEnable(GL_STENCIL_TEST);
                  glStencilFunc(state.stencilFunc, state.stencilRef, state.stencilMask);
                  glStencilOp(state.stencilFailOp, state.stencilZFailOp, state.stencilPassOp);
                  glStencilMask(state.stencilWriteMask);
               }
               else
               {
                  glDisable(GL_STENCIL_TEST);
               }
            }

            // Blend State
            {
               if (state.blendEnable)
               {
                  glEnable(GL_BLEND);

                  if (state.separateAlphaBlendEnable)
                  {
                     glBlendFuncSeparate(GFXGLBlend[state.blendSrc], GFXGLBlend[state.blendDest], GFXGLBlend[state.separateAlphaBlendSrc], GFXGLBlend[state.separateAlphaBlendDest]);
                     glBlendEquationSeparate(GFXGLBlendOp[state.blendOp], GFXGLBlendOp[state.separateAlphaBlendOp]);
                  }
                  else
                  {
                     glBlendFunc(GFXGLBlend[state.blendSrc], GFXGLBlend[state.blendDest]);
                     glBlendEquation(GFXGLBlendOp[state.blendOp]);
                  }
               }
               else
               {
                  glDisable(GL_BLEND);
               }

               glColorMask(state.colorWriteRed, state.colorWriteBlue, state.colorWriteGreen, state.colorWriteAlpha);
            }

            mInternalGLState.currentStateBlock = stateBlock;

            // we have to do this after referencing it as we increment it when inserting into the command buffer.
            stateBlock->decRefCount();
            break;
         }
      case GFX_CMD_SET_VERTEX_BUFFER:
         {
            U32 slot = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 handle = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 offset = cmdBuffer->mBuffer.data[cmdBufferOffset++];

            const GLPipelineState& pipelineState = mGLPipelineStateTable[mInternalGLState.currentPipeline];

            AssertFatal(mGLVertexBufferTable.find(handle) != mGLVertexBufferTable.end(), "GFXGLDevice Command Buffer :: Vertex Buffer not found.");
            AssertFatal(mGLPipelineStateTable.find(handle) != mGLPipelineStateTable.end(), "GFXGLDevice Command Buffer :: No Pipeline Bound!");

            GLuint buffer = mGLVertexBufferTable[handle];
            U32 stride = pipelineState.inputLayoutState.buffers[slot].stride;

            if (mGLCaps.separateVertexFormat)
            {
               glBindVertexBuffer(slot, buffer, (GLintptr)offset, stride);
            }
            else
            {
               glBindBuffer(GL_ARRAY_BUFFER, buffer);

               bool isInstancing = pipelineState.inputLayoutState.buffers[slot].isInstancing;
               for (U32 j = 0; j < pipelineState.inputLayoutState.buffers[slot].elementCount; ++j)
               {
                  GLint attributeLocation = pipelineState.inputLayoutState.buffers[slot].elements[j].attributeIndex;
                  GLint elementCount = pipelineState.inputLayoutState.buffers[slot].elements[j].elementCount;
                  GLenum type = pipelineState.inputLayoutState.buffers[slot].elements[j].type;
                  GLboolean normalized = pipelineState.inputLayoutState.buffers[slot].elements[j].normalized;
                  GLuint relativeOffset = pipelineState.inputLayoutState.buffers[slot].elements[j].localOffset;

                  glEnableVertexAttribArray(attributeLocation);
                  glVertexAttribPointer(attributeLocation, elementCount, type, normalized, stride, (void*)(relativeOffset + offset));
                  glVertexAttribDivisor(attributeLocation, isInstancing ? 1 : 0);

                  mInternalGLState.attributesBound[attributeLocation] = true;
               }
            }
            break;
         }
      case GFX_CMD_SET_INDEX_BUFFER:
         {
            U32 handle = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            bool is16Bit = (bool)cmdBuffer->mBuffer.data[cmdBufferOffset++];

            AssertFatal(mGLIndexBufferTable.find(handle) != mGLIndexBufferTable.end(), "GFXGLDevice Command Buffer :: Index Buffer not found");

            GLuint buffer = mGLIndexBufferTable[handle];
            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffer);

            currentBoundIndexBuffer = handle;
            currentIndexBufferByteSize = is16Bit ? GL_UNSIGNED_SHORT : GL_UNSIGNED_BYTE;
            break;
         }
      case GFX_CMD_SET_BIND_GROUP:
         {
            U32 dynamicOffsets[GFX_MAX_RESOURCES_PER_BINDGROUP];
            
            U32 set = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 bindGroupHandle = cmdBuffer->mBuffer.data[cmdBufferOffset];
            U32 numberOfDynamicOffsets = cmdBuffer->mBuffer.data[cmdBufferOffset];
            for (U32 i = 0; i < numberOfDynamicOffsets; ++i)
            {
               dynamicOffsets[i] = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            }
            
            AssertFatal(mGLBindGroupTable.find(bindGroupHandle) != mGLBindGroupTable.end(), "GFXGLDevice Command Buffer :: Bind Group not found");
            const GLBindGroup& bindGroup = mGLBindGroupTable[bindGroupHandle];
            
            AssertFatal(mGLBindGroupLayoutTable.find(bindGroup.bindGroupLayout) != mGLBindGroupLayoutTable.end(), "GFXGLDevice Bind Group Layout not Found");
            const GLBindGroupLayout& bindGroupLayout = mGLBindGroupLayoutTable[bindGroup.bindGroupLayout];
            
            AssertFatal(bindGroupLayout.numberOfDynamicResources == numberOfDynamicOffsets, avar("GFXGLDevice Command Buffer :: Number of dynamic offsets [%d] does not match number of dynamic resources [%d] in Bind Group", numberOfDynamicOffsets, bindGroupLayout.numberOfDynamicResources));

            // TODO: Replace GFXTextureObject with GFXTextureHandle
            
            U32 offsetIndex = 0;
            for (U32 i = 0; i < bindGroup.count; i++)
            {
               if (bindGroup.resources[i].type != GFXResourceDynamicConstantBufferType)
                  continue;
               
               switch (bindGroup.resources[i].type)
               {
               case GFXResourceSamplerType:
                  {
                     const auto& sampler = std::get<GLBindGroup::Sampler>(bindGroup.resources[i].resource);
                     
                     AssertFatal(mGLSamplerStateTable.find(sampler.sampler) != mGLSamplerStateTable.end(), avar("GFXGLDevice Command Buffer :: Sampler [%u] is not found!", (U32)sampler.sampler));
                     
                     GLuint samplerHandle = mGLSamplerStateTable[sampler.sampler];
                     glBindSampler(sampler.textureUnit, samplerHandle);
                     break;
                  }
               case GFXResourceTextureType:
                  {
                     const auto& texture = std::get<GLBindGroup::Texture>(bindGroup.resources[i].resource);
                     
                     GFXGLTextureObject* textureObject = static_cast<GFXGLTextureObject*>(texture.texture);
                     
                     glActiveTexture(GL_TEXTURE0 + texture.textureUnit);
                     glBindTexture(textureObject->getBinding(), textureObject->getHandle());
                     break;
                  }
               case GFXResourceConstantBufferType:
                  {
                     const auto& cbuffer = std::get<GLBindGroup::ConstantBuffer>(bindGroup.resources[i].resource);
                     AssertFatal(mGLConstantBufferTable.find(cbuffer.buffer) != mGLConstantBufferTable.end(), avar("GFXGLDevice Command Buffer :: Constant Buffer [%u] is not found!", (U32)cbuffer.buffer));
                     
                     GLuint bufferHandle = mGLConstantBufferTable[cbuffer.buffer];
                     U32 bindingLocation = bindGroupLayout.resources[i].binding;
                     glBindBufferRange(GL_UNIFORM_BUFFER, bindingLocation, bufferHandle, cbuffer.offset, cbuffer.size);
                     break;
                  }
               case GFXResourceDynamicConstantBufferType:
                  {
                     const auto& cbuffer = std::get<GLBindGroup::ConstantBuffer>(bindGroup.resources[i].resource);
                     
                     AssertFatal(mGLConstantBufferTable.find(cbuffer.buffer) != mGLConstantBufferTable.end(), avar("GFXGLDevice Command Buffer :: Dynamic Constant Buffer [%u] is not found!", (U32)cbuffer.buffer));
                     
                     GLuint bufferHandle = mGLConstantBufferTable[cbuffer.buffer];
                     
                     U32 offset = cbuffer.offset + dynamicOffsets[offsetIndex++];
                     U32 bindingLocation = bindGroupLayout.resources[i].binding;

                     glBindBufferRange(GL_UNIFORM_BUFFER, bindingLocation, bufferHandle, offset, cbuffer.size);
                     break;
                  }
               default:
                  {
                     AssertISV(false, "GFXGLDevice Command Buffer :: Invalid resource type in SetBindGroup");
                     break;
                  }
               }
            }
            
            break;
         }
      case GFX_CMD_SET_PUSH_TEXTURE_BIND_GROUP:
         {
            U32 set = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 count = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            
            // TODO: Replace GFXTextureObject with GFXTextureHandle
            
            for (U32 i = 0; i < count; ++i)
            {
               U32 textureUnit = cmdBuffer->mBuffer.data[cmdBufferOffset++];
               U32 textureHandle = cmdBuffer->mBuffer.data[cmdBufferOffset++];
               U32 samplerHandle = cmdBuffer->mBuffer.data[cmdBufferOffset++];
               
               AssertFatal(mGLSamplerStateTable.find(samplerHandle) != mGLSamplerStateTable.end(), avar("GFXGLDevice Command Buffer :: Sampler [%u] is not found!", (U32)samplerHandle));

               
               GLuint samplerGLHandle = mGLSamplerStateTable[samplerHandle];

               GFXGLTextureObject* textureObject = static_cast<GFXGLTextureObject*>(cmdBuffer->mTextureData.textures[textureHandle]);
               
               glBindSampler(textureUnit, samplerGLHandle);
               
               glActiveTexture(GL_TEXTURE0 + textureUnit);
               glBindTexture(textureObject->getBinding(), textureObject->getHandle());
            }
            break;
         }
      case GFX_CMD_SET_PUSHCONSTANTS:
         {
            U32 index = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 offset = mSizeOfPushConstant * index;

            glBindBufferRange(GL_UNIFORM_BUFFER, mPushConstantBindingLocation, mPushConstantBuffer, offset, mSizeOfPushConstant);
            break;
         }
      case GFX_CMD_DRAW_PRIMITIVES:
         {
            U32 type = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 start = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 count = cmdBuffer->mBuffer.data[cmdBufferOffset++];

            verifyBeforeDraw();

            glDrawArrays(GFXGLPrimType[type], start, count);
            break;
         }
      case GFX_CMD_DRAW_PRIMITIVES_INSTANCED:
         {
            U32 type = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 start = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 count = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 instanceCount = cmdBuffer->mBuffer.data[cmdBufferOffset++];

            verifyBeforeDraw();

            glDrawArraysInstanced(GFXGLPrimType[type], start, count, instanceCount);
            break;
         }
      case GFX_CMD_DRAW_INDEXED_PRIMITIVES:
         {
            AssertFatal(currentBoundIndexBuffer != 0, "GFXGLDevice Command Buffer :: no index buffer set when trying to draw indexed data");
            AssertFatal(currentIndexBufferByteSize != GL_NONE, "GFXGLDevice Command Buffer :: no index buffer byte size set");

            U32 type = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 count = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 indexBufferOffset = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 baseVertex = cmdBuffer->mBuffer.data[cmdBufferOffset++];

            verifyBeforeDraw();

            glDrawElementsBaseVertex(GFXGLPrimType[type], count, currentIndexBufferByteSize, (void*)indexBufferOffset, baseVertex);
            break;
         }
      case GFX_CMD_DRAW_INDEXED_PRIMITIVES_INSTANCED:
         {
            AssertFatal(currentBoundIndexBuffer != 0, "GFXGLDevice Command Buffer :: no index buffer set when trying to draw indexed data");
            AssertFatal(currentIndexBufferByteSize != GL_NONE, "GFXGLDevice Command Buffer :: no index buffer byte size set");

            U32 type = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 count = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 indexBufferOffset = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 baseVertex = cmdBuffer->mBuffer.data[cmdBufferOffset++];
            U32 instanceCount = cmdBuffer->mBuffer.data[cmdBufferOffset++];

            verifyBeforeDraw();

            glDrawElementsInstancedBaseVertex(GFXGLPrimType[type], count, currentIndexBufferByteSize, (void*)indexBufferOffset, instanceCount, baseVertex);
            break;
         }
      case GFX_CMD_END:
         {
            goto done;
            break;
         }
      }
   }
done:
   glFlush();
   ;
}

void* GFXGLDevice::mapBuffer(GFXHandle<GFXVertexBufferHandle2> handle)
{
   AssertFatal(mGLVertexBufferTable.find(handle) != mGLVertexBufferTable.end(), "GFXGLDevice VertexBuffer not Found");
   
   // TODO: performance, glMapBuffer with default GL_WRITE_ONLY flag is probably the slow path!
   GLuint buffer = mGLVertexBufferTable[handle];
   
   glBindBuffer(GL_ARRAY_BUFFER, buffer);
   return glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
}

void GFXGLDevice::unmapBuffer(GFXHandle<GFXVertexBufferHandle2> handle)
{
   AssertFatal(mGLVertexBufferTable.find(handle) != mGLVertexBufferTable.end(), "GFXGLDevice VertexBuffer not Found");
   
   GLuint buffer = mGLVertexBufferTable[handle];
   
   glBindBuffer(GL_ARRAY_BUFFER, buffer);
   glUnmapBuffer(GL_ARRAY_BUFFER);
}

GFXHandle<GFXSamplerHandle> GFXGLDevice::createSampler(const GFXSamplerDesc2& desc)
{
   GLuint samplerId;
   glGenSamplers(1, &samplerId);
   glSamplerParameteri(samplerId, GL_TEXTURE_MIN_FILTER, minificationFilter(desc.minFilter, desc.mipFilter));
   glSamplerParameteri(samplerId, GL_TEXTURE_MAG_FILTER, GFXGLTextureFilter[desc.magFilter]);
   glSamplerParameteri(samplerId, GL_TEXTURE_WRAP_S, GFXGLTextureAddress[desc.addressModeU]);
   glSamplerParameteri(samplerId, GL_TEXTURE_WRAP_T, GFXGLTextureAddress[desc.addressModeV]);
   glSamplerParameteri(samplerId, GL_TEXTURE_WRAP_R, GFXGLTextureAddress[desc.addressModeW]);
   glSamplerParameteri(samplerId, GL_TEXTURE_COMPARE_FUNC, GFXGLCmpFunc[desc.samplerFunc]);
   if (desc.mipLODBias != 0.0)
      glSamplerParameterf(samplerId, GL_TEXTURE_LOD_BIAS, desc.mipLODBias);
   if (mGLCaps.anisotropicFiltering)
      glSamplerParameteri(samplerId, GL_TEXTURE_MAX_ANISOTROPY_EXT, desc.maxAnisotropy);
   
   U32 handle = mNextHandle++;
   mGLSamplerStateTable[handle] = samplerId;
   return GFXHandle<GFXSamplerHandle>(handle);
}

void GFXGLDevice::destroySampler(GFXHandle<GFXSamplerHandle> sampler)
{
   AssertFatal(mGLSamplerStateTable.find(sampler) != mGLSamplerStateTable.end(), "GFXGLDevice Sampler not Found");

   GLuint samplerId = mGLSamplerStateTable[sampler];
   mGLSamplerStateTable.erase(mGLSamplerStateTable.find(sampler));

   glDeleteSamplers(1, &samplerId);
}

GFXHandle<GFXTextureHandle> GFXGLDevice::createTexture(const GFXTextureDesc& desc)
{
   GLuint textureId;
   glGenTextures(1, &textureId);
   
   GLTextureState state = {};
   state.width = desc.width;
   state.height = desc.height;
   state.depth = desc.depth;
   state.type = GFXGLTextureBindingType[desc.type];
   state.format = GFXGLTextureInternalFormat[desc.format];
   state.numMipLevels = desc.numMipLevels;
   state.numberLayers = desc.numberLayers;
   state.profile = desc.profile;
   state.textureHandle = textureId;
   
   glBindTexture(state.type, textureId);
   
   switch (state.type)
   {
   case GL_TEXTURE_1D:
      glTexStorage1D(textureId, state.numMipLevels, state.format, state.width);
      break;
   case GL_TEXTURE_CUBE_MAP:
      TORQUE_CASE_FALLTHROUGH;
   case GL_TEXTURE_2D:
      glTexStorage2D(textureId, state.numMipLevels, state.format, state.width, state.height);
      break;
   case GL_TEXTURE_3D:
      TORQUE_CASE_FALLTHROUGH;
   case GL_TEXTURE_2D_ARRAY:
      TORQUE_CASE_FALLTHROUGH;
   case GL_TEXTURE_CUBE_MAP_ARRAY:
      glTexStorage3D(textureId, state.numMipLevels, state.format, state.width, state.height, state.type == GL_TEXTURE_3D ? state.depth : state.numberLayers);
      break;
   default:
      AssertISV(false, "GFXGLDevice :: Invalid Texture Bind Type");
   }
   
   U32 handle = mNextHandle++;
   mGLTextureTable[handle] = state;
   return GFXHandle<GFXTextureHandle>(handle);
}

void GFXGLDevice::destroyTexture(GFXHandle<GFXTextureHandle> texture)
{
   AssertFatal(mGLTextureTable.find(texture) != mGLTextureTable.end(), avar("GFXGLDevice Texture [%u] not Found", (U32)texture));

   GLuint handle = mGLTextureTable[texture].textureHandle;
   mGLTextureTable.erase(mGLTextureTable.find(texture));

   glDeleteTextures(1, &handle);
}

GFXHandle<GFXVertexBufferHandle2> GFXGLDevice::createVertexBuffer(const GFXVertexBufferDesc& desc)
{
   // TODO: support ARB_buffer_store/EXT_buffer_storage (GL 4.4, ES Extension)
   
   GLuint bufferId;
   glGenBuffers(1, &bufferId);
   glBindBuffer(GL_ARRAY_BUFFER, bufferId);
   glBufferData(GL_ARRAY_BUFFER, desc.size, desc.data, GFXGLBufferType[desc.bufferType]);
   
   U32 handle = mNextHandle++;
   mGLVertexBufferTable[handle] = bufferId;
   return GFXHandle<GFXVertexBufferHandle2>(handle);
}

void GFXGLDevice::destroyVertexBuffer(GFXHandle<GFXVertexBufferHandle2> vertexBuffer)
{
   AssertFatal(mGLVertexBufferTable.find(vertexBuffer) != mGLVertexBufferTable.end(), "GFXGLDevice Vertex Buffer not Found");

   GLuint bufferId = mGLVertexBufferTable[vertexBuffer];
   mGLVertexBufferTable.erase(mGLVertexBufferTable.find(vertexBuffer));

   glDeleteBuffers(1, &bufferId);
}

GFXHandle<GFXIndexBufferHandle> GFXGLDevice::createIndexBuffer(const GFXIndexBufferDesc& desc)
{
   // TODO: support ARB_buffer_store/EXT_buffer_storage (GL 4.4, ES Extension)
   
   GLuint bufferId;
   glGenBuffers(1, &bufferId);
   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, bufferId);
   glBufferData(GL_ELEMENT_ARRAY_BUFFER, desc.size, desc.data, GFXGLBufferType[desc.bufferType]);
   
   U32 handle = mNextHandle++;
   mGLIndexBufferTable[handle] = bufferId;
   
   return GFXHandle<GFXIndexBufferHandle>(handle);
}

void GFXGLDevice::destroyIndexBuffer(GFXHandle<GFXIndexBufferHandle> indexBuffer)
{
   AssertFatal(mGLIndexBufferTable.find(indexBuffer) != mGLIndexBufferTable.end(), "GFXGLDevice Index Buffer not Found");

   GLuint bufferId = mGLIndexBufferTable[indexBuffer];
   mGLIndexBufferTable.erase(mGLIndexBufferTable.find(indexBuffer));

   glDeleteBuffers(1, &bufferId);
}

GFXHandle<GFXConstantBufferHandle> GFXGLDevice::createConstantBuffer(const GFXConstBufferDesc& desc)
{
   // TODO: support ARB_buffer_store/EXT_buffer_storage (GL 4.4, ES Extension)
   
   GLuint bufferId;
   glGenBuffers(1, &bufferId);
   glBindBuffer(GL_UNIFORM_BUFFER, bufferId);
   glBufferData(GL_UNIFORM_BUFFER, desc.size, desc.data, GFXGLBufferType[desc.bufferType]);
   
   U32 handle = mNextHandle++;
   mGLConstantBufferTable[handle] = bufferId;
   
   return GFXHandle<GFXConstantBufferHandle>(handle);
}

void GFXGLDevice::destroyConstantBuffer(GFXHandle<GFXConstantBufferHandle> constantBuffer)
{
   AssertFatal(mGLConstantBufferTable.find(constantBuffer) != mGLConstantBufferTable.end(), "GFXGLDevice Constant Buffer not Found");

   GLuint bufferId = mGLConstantBufferTable[constantBuffer];
   mGLConstantBufferTable.erase(mGLConstantBufferTable.find(constantBuffer));

   glDeleteBuffers(1, &bufferId);
}

GFXHandle<GFXGraphicsPipelineHandle> GFXGLDevice::createGraphicsPipeline(const GFXPipelineDesc& desc)
{
   GLPipelineState pipelineState = {};
   pipelineState.inputLayoutState.bufferCount = desc.inputLayout.count;

   U32 texCoordIndex = 0;
   
   // Input Layout
   U32 numberOfAttributes = 0;
   for (U32 i = 0; i < desc.inputLayout.count; ++i)
   {
      const struct GFXInputLayoutBufferDesc& bufferDesc = desc.inputLayout.buffers[i];

      pipelineState.inputLayoutState.buffers[i].elementCount = bufferDesc.count;

      U32 localOffset = 0;
      for (U32 j = 0; j < bufferDesc.count; ++j)
      {
         const String& semantic = bufferDesc.elements[j].semanticName;

         if (semantic == GFXSemantic::POSITION)
         {
            pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex = Torque::GL_VertexAttrib_Position;
            pipelineState.inputLayoutState.buffers[i].elements[j].type = GL_FLOAT;
            pipelineState.inputLayoutState.buffers[i].elements[j].elementCount = 3;
            pipelineState.inputLayoutState.buffers[i].elements[j].normalized = false;
            pipelineState.inputLayoutState.buffers[i].elements[j].localOffset += localOffset;
            localOffset += 3 * sizeof(F32);
         }
         else if (semantic == GFXSemantic::NORMAL)
         {
            pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex = Torque::GL_VertexAttrib_Normal;
            pipelineState.inputLayoutState.buffers[i].elements[j].type = GL_FLOAT;
            pipelineState.inputLayoutState.buffers[i].elements[j].elementCount = 3;
            pipelineState.inputLayoutState.buffers[i].elements[j].normalized = false;
            pipelineState.inputLayoutState.buffers[i].elements[j].localOffset += localOffset;
            localOffset += 3 * sizeof(F32);
         }
         else if (semantic == GFXSemantic::TANGENT)
         {
            pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex = Torque::GL_VertexAttrib_Tangent;
            pipelineState.inputLayoutState.buffers[i].elements[j].type = GL_FLOAT;
            pipelineState.inputLayoutState.buffers[i].elements[j].elementCount = 3;
            pipelineState.inputLayoutState.buffers[i].elements[j].normalized = false;
            pipelineState.inputLayoutState.buffers[i].elements[j].localOffset += localOffset;
            localOffset += 3 * sizeof(F32);
         }
         else if (semantic == GFXSemantic::TANGENTW)
         {
            pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex = Torque::GL_VertexAttrib_TangentW;
            pipelineState.inputLayoutState.buffers[i].elements[j].type = GL_FLOAT;
            pipelineState.inputLayoutState.buffers[i].elements[j].elementCount = 4;
            pipelineState.inputLayoutState.buffers[i].elements[j].normalized = false;
            pipelineState.inputLayoutState.buffers[i].elements[j].localOffset += localOffset;
            localOffset += 4 * sizeof(F32);
         }
         else if (semantic == GFXSemantic::BINORMAL)
         {
            pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex = Torque::GL_VertexAttrib_Binormal;
            pipelineState.inputLayoutState.buffers[i].elements[j].type = GL_FLOAT;
            pipelineState.inputLayoutState.buffers[i].elements[j].elementCount = 3;
            pipelineState.inputLayoutState.buffers[i].elements[j].normalized = false;
            pipelineState.inputLayoutState.buffers[i].elements[j].localOffset += localOffset;
            localOffset += 3 * sizeof(F32);
         }
         else if (semantic == GFXSemantic::COLOR)
         {
            pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex = Torque::GL_VertexAttrib_Color;
            pipelineState.inputLayoutState.buffers[i].elements[j].type = GL_UNSIGNED_BYTE;
            pipelineState.inputLayoutState.buffers[i].elements[j].elementCount = 4;
            pipelineState.inputLayoutState.buffers[i].elements[j].normalized = true;
            pipelineState.inputLayoutState.buffers[i].elements[j].localOffset += localOffset;
            localOffset += 4 * sizeof(U8);
         }
         else if (semantic == GFXSemantic::BLENDWEIGHT)
         {
            pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex = Torque::GL_VertexAttrib_BlendWeight0 + bufferDesc.elements[j].semanticIndex;
            pipelineState.inputLayoutState.buffers[i].elements[j].type = GL_FLOAT;
            pipelineState.inputLayoutState.buffers[i].elements[j].elementCount = 4;
            pipelineState.inputLayoutState.buffers[i].elements[j].normalized = false;
            pipelineState.inputLayoutState.buffers[i].elements[j].localOffset += localOffset;
            localOffset += 4 * sizeof(F32);
         }
         else if (semantic == GFXSemantic::BLENDINDICES)
         {
            pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex = Torque::GL_VertexAttrib_BlendIndex0 + bufferDesc.elements[j].semanticIndex;
            pipelineState.inputLayoutState.buffers[i].elements[j].type = GL_UNSIGNED_BYTE;
            pipelineState.inputLayoutState.buffers[i].elements[j].elementCount = 4;
            pipelineState.inputLayoutState.buffers[i].elements[j].normalized = false; // should be true??
            pipelineState.inputLayoutState.buffers[i].elements[j].localOffset += localOffset;
            localOffset += 4 * sizeof(U8);
         }
         else if (semantic == GFXSemantic::TEXCOORD)
         {
            texCoordIndex = getMax(texCoordIndex, bufferDesc.elements[j].semanticIndex);
            U32 elementCount = 0;
            
            GFXDeclType type = bufferDesc.elements[j].type;
            if (type == GFXDeclType_Float)
               elementCount = 1;
            else if (type == GFXDeclType_Float2)
               elementCount = 2;
            else if (type == GFXDeclType_Float3)
               elementCount = 3;
            else if (type == GFXDeclType_Float4)
               elementCount = 4;
            else
            {
               AssertISV(false, "GFXGLDevice :: Invalid type for Texture Semantic");
            }
            
            pipelineState.inputLayoutState.buffers[i].elements[j].attributeIndex = Torque::GL_VertexAttrib_TexCoord0 + texCoordIndex;
            pipelineState.inputLayoutState.buffers[i].elements[j].type = GL_FLOAT;
            pipelineState.inputLayoutState.buffers[i].elements[j].elementCount = elementCount;
            pipelineState.inputLayoutState.buffers[i].elements[j].normalized = false;
            pipelineState.inputLayoutState.buffers[i].elements[j].localOffset += localOffset;
            localOffset += elementCount * sizeof(F32);
         }
         else
         {
            AssertISV(false, avar("GFXGLDevice Invalid Semantic Name [%s]", semantic.c_str()));
         }
         numberOfAttributes++;
      }
      
      pipelineState.inputLayoutState.buffers[i].isInstancing = desc.inputLayout.buffers[i].isInstancingBuffer;
      pipelineState.inputLayoutState.buffers[i].stride = desc.inputLayout.buffers[i].stride;
   }

   pipelineState.inputLayoutState.numberOfAttributes = numberOfAttributes;

   // Bind Group Layout
   pipelineState.bindGroupLayout = U32(desc.bindGroupLayout);

   // Shader Modules
   //pipelineState.program

   // TODO: cache push constant size...

   return GFXHandle<GFXGraphicsPipelineHandle>(0);
}

void GFXGLDevice::destroyGraphicsPipeline(GFXHandle<GFXGraphicsPipelineHandle> pipeline)
{
   AssertFatal(mGLPipelineStateTable.find(pipeline) != mGLPipelineStateTable.end(), "GFXGLDevice Pipeline not Found");

   const GLPipelineState& pipelineState = mGLPipelineStateTable[pipeline];
   glDeleteProgram(pipelineState.program);

   mGLPipelineStateTable.erase(mGLPipelineStateTable.find(pipeline));
}

GFXHandle<GFXBindGroupLayoutHandle> GFXGLDevice::createBindGroupLayout(const GFXBindGroupLayoutDesc& desc)
{
   GLBindGroupLayout layout = {};
   layout.resourceCount = desc.resourceCount;
   
   U32 numberOfDynamicResources = 0;
   
   for (U32 i = 0; i < desc.resourceCount; i++)
   {
      layout.resources[i].name = desc.resources[i].name;
      layout.resources[i].binding = desc.resources[i].binding;
      layout.resources[i].stages = desc.resources[i].stages;
      layout.resources[i].type = desc.resources[i].type;
      
      if (desc.resources[i].type == GFXResourceDynamicConstantBufferType)
      {
         numberOfDynamicResources++;
      }
   }
   
   layout.numberOfDynamicResources = numberOfDynamicResources;

   U32 handle = mNextHandle++;
   mGLBindGroupLayoutTable[handle] = layout;

   return GFXHandle<GFXBindGroupLayoutHandle>(handle);
}

void GFXGLDevice::deleteBindGroupLayout(GFXHandle<GFXBindGroupLayoutHandle> layout)
{
   AssertFatal(mGLBindGroupLayoutTable.find(layout) != mGLBindGroupLayoutTable.end(), "GFXGLDevice Bind Group Layout not Found");
   mGLBindGroupLayoutTable.erase(mGLBindGroupLayoutTable.find(layout));
}

GFXHandle<GFXBindGroupHandle> GFXGLDevice::createBindGroup(GFXHandle<GFXBindGroupLayoutHandle> bindGroupLayoutHandle, const GFXBindGroupDesc& desc)
{
   AssertFatal(mGLBindGroupLayoutTable.find(bindGroupLayoutHandle) != mGLBindGroupLayoutTable.end(), "GFXGLDevice Bind Group Layout not Found");
   const GLBindGroupLayout& layout = mGLBindGroupLayoutTable[bindGroupLayoutHandle];
 
   AssertFatal(desc.count <= GFX_MAX_RESOURCES_PER_BINDGROUP, "GGFXGLDevice Bind Group :: Number of resources exceeds GFX_MAX_RESOURCES_PER_BINDGROUP");
   AssertFatal(desc.count == layout.resourceCount, "GFXGLDevice Bind Group :: Resource Count does not match the Layout Count");

   GLBindGroup group = {};
   group.count = desc.count;
   group.bindGroupLayout = bindGroupLayoutHandle;

   for (U32 i = 0; i < desc.count; i++)
   {
      GFXResourceType resourceType = layout.resources[i].type;
      group.resources[i].type = resourceType;

      GLBindGroup::ConstantBuffer constantBuffer;
      GLBindGroup::Sampler sampler;
      GLBindGroup::Texture texture;

      switch (resourceType)
      {
      case GFXResourceConstantBufferType:
         TORQUE_CASE_FALLTHROUGH;
      case GFXResourceDynamicConstantBufferType:
         constantBuffer.buffer = desc.resources[i].constantBuffer.handle;
         constantBuffer.offset = desc.resources[i].constantBuffer.offset;
         constantBuffer.size = desc.resources[i].constantBuffer.size;
         group.resources[i].resource = constantBuffer;
         break;
      case GFXResourceSamplerType:
         sampler.sampler = desc.resources[i].sampler.handle;
         sampler.textureUnit = desc.resources[i].sampler.textureUnit;
         group.resources[i].resource = sampler;
         break;
      case GFXResourceTextureType:
         texture.texture = desc.resources[i].texture;
         group.resources[i].resource = texture;
         break;
      default:
         AssertISV(false, "GFXGLDevice Bind Group :: Invalid Resource Type");
      };
   }
   
   U32 handle = mNextHandle++;
   mGLBindGroupTable[handle] = group;
   
   return GFXHandle<GFXBindGroupHandle>(handle);
}

void GFXGLDevice::deleteBindGroup(GFXHandle<GFXBindGroupHandle> bindGroup)
{
   AssertFatal(mGLBindGroupTable.find(bindGroup) != mGLBindGroupTable.end(), "GFXGLDevice Bind Group not Found");
   mGLBindGroupTable.erase(mGLBindGroupTable.find(bindGroup));
}
