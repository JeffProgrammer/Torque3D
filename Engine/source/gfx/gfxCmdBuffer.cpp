#include "platform/platform.h"
#include "gfx/gfxDevice.h"
#include "gfx/gfxCmdBuffer.h"

GFXCmdBuffer::GFXCmdBuffer(GFXDevice* device) :
   mDevice(device)
{
   mRecording = true;

   dMemset(&mBuffer, 0, sizeof(mBuffer));
   dMemset(&mPushConstantMemory, 0, sizeof(mPushConstantMemory));
   dMemset(&mStateBlockData, 0, sizeof(mStateBlockData));
}

GFXCmdBuffer::~GFXCmdBuffer()
{

}

void GFXCmdBuffer::setViewport(S32 x, S32 y, S32 w, S32 h)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");

   mBuffer.append(GFX_CMD_VIEWPORT);
   mBuffer.append(x);
   mBuffer.append(y);
   mBuffer.append(w);
   mBuffer.append(h);
}

void GFXCmdBuffer::setGraphicsPipeline(GFXHandle<GFXGraphicsPipelineHandle> pipeline)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");

   mBuffer.append(GFX_CMD_SET_GRAPHICS_PIPELINE);
   mBuffer.append(pipeline);
}

void GFXCmdBuffer::setStateBlock(GFXStateBlockRef state)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");
   AssertFatal(mStateBlockData.count < MAX_STATE_BLOCK_RESOURCES, avar("GFXCmdBuffer cannot call setStateBlock more than %d times in a single command buffer", MAX_STATE_BLOCK_RESOURCES));

   U32 stateBlockHandle = mStateBlockData.count++;
   mStateBlockData.stateBlocks[stateBlockHandle] = state;

   mBuffer.append(GFX_CMD_SET_STATE_BLOCK);
   mBuffer.append(stateBlockHandle);
}

void GFXCmdBuffer::setVertexBuffer(U32 slot, GFXHandle<GFXVertexBufferHandle2> buffer, U32 offset)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");
   
   mBuffer.append(GFX_CMD_SET_VERTEX_BUFFER);
   mBuffer.append(slot);
   mBuffer.append(buffer);
   mBuffer.append(offset);
}

void GFXCmdBuffer::setIndexBuffer(GFXHandle<GFXIndexBufferHandle> buffer, bool is16Bit)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");
   
   mBuffer.append(GFX_CMD_SET_INDEX_BUFFER);
   mBuffer.append(buffer);
   mBuffer.append(is16Bit);
}

void GFXCmdBuffer::setBindGroup(U32 set, GFXHandle<GFXBindGroupHandle> bindGroup, U32 offsetCount, U32* offsets)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");
   AssertFatal(set <= GFX_MAX_BIND_GROUPS, avar("BindGroup Set must be within the range of [0, %d]", GFX_MAX_BIND_GROUPS - 1));
   AssertFatal(offsetCount < GFX_MAX_RESOURCES_PER_BINDGROUP, avar("Offset Count must be less than %d", GFX_MAX_RESOURCES_PER_BINDGROUP - 1));
   
   mBuffer.append(GFX_CMD_SET_BIND_GROUP);
   mBuffer.append(set);
   mBuffer.append(bindGroup);
   mBuffer.append(offsetCount);
   for (U32 i = 0; i < offsetCount; ++i)
   {
      mBuffer.append(offsets[i]);
   }
}

void GFXCmdBuffer::setPushConstants(GFXShaderStageFlags stageFlags, U32 size, void* data)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");
   AssertFatal(size < 128, "Push Constant size must be no more than 128 bytes. Use a buffer for bigger data.");

   S32 sizeOfPushConstant = mDevice->mSizeOfPushConstant;
   AssertFatal(mPushConstantMemory.count * sizeOfPushConstant < MEMORY_CONSTANT_PAGE_SIZE, "Ran out of Constant Buffer space!");

   dMemcpy(mPushConstantMemory.data + (sizeOfPushConstant * mPushConstantMemory.count), data, size);
   U32 idx = mPushConstantMemory.count++;
   
   mBuffer.append(GFX_CMD_SET_PUSHCONSTANTS);
   mBuffer.append(idx);
}

void GFXCmdBuffer::drawPrimitives(GFXPrimitiveType type, U32 start, U32 count)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");

   mBuffer.append(GFX_CMD_DRAW_PRIMITIVES);
   mBuffer.append(type);
   mBuffer.append(start);
   mBuffer.append(count);
}

void GFXCmdBuffer::drawPrimitivesInstanced(GFXPrimitiveType type, U32 start, U32 count, U32 instanceCount)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");

   mBuffer.append(GFX_CMD_DRAW_PRIMITIVES_INSTANCED);
   mBuffer.append(type);
   mBuffer.append(start);
   mBuffer.append(count);
   mBuffer.append(instanceCount);
}

void GFXCmdBuffer::drawIndexedPrimitives(GFXPrimitiveType type, U32 count, U32 indexBufferOffset, U32 baseVertex)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");

   mBuffer.append(GFX_CMD_DRAW_INDEXED_PRIMITIVES);
   mBuffer.append(type);
   mBuffer.append(count);
   mBuffer.append(indexBufferOffset);
   mBuffer.append(baseVertex);
}

void GFXCmdBuffer::drawIndexedPrimitivesInstanced(GFXPrimitiveType type, U32 count, U32 indexBufferOffset, U32 baseVertex, U32 instanceCount)
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");

   mBuffer.append(GFX_CMD_DRAW_INDEXED_PRIMITIVES_INSTANCED);
   mBuffer.append(type);
   mBuffer.append(count);
   mBuffer.append(indexBufferOffset);
   mBuffer.append(baseVertex);
   mBuffer.append(instanceCount);
}

void GFXCmdBuffer::endRecording()
{
   AssertFatal(!mRecording, "GFXCmdBuffer is not currently recording!");
   mRecording = false;

   mBuffer.append(GFX_CMD_END);
}
