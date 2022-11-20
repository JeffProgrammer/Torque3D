#include "gui/render/guiRender.h"
#include "gfx/gfxDevice.h"
#include "gfx/gfxCmdBuffer.h"

GuiRenderManager::GuiRenderManager()
{
   mRenderInsts.reserve(2048);
   mCurrentVertexData.data = (char*)dMalloc_aligned(VERTEX_BUFFER_SIZE_MB, 16);
   clear();
}

GuiRenderManager::~GuiRenderManager()
{
   dFree_aligned(mCurrentVertexData.data);
}

GuiRenderInst* GuiRenderManager::allocRenderInst(U32 numberOfVertices)
{
   GuiRenderInst inst = {};
   inst.vertexList = allocVertices(numberOfVertices);
   
   mRenderInsts.push_back(inst);
   return &mRenderInsts.last();
}

void GuiRenderManager::clear()
{
   mCurrentVertexData.count = 0;
   mCurrentVertexData.offset = 0;
   dMemset(mCurrentVertexData.data, 0, VERTEX_BUFFER_SIZE_MB);
   mRenderInsts.clear();
}

GuiVertexList GuiRenderManager::allocVertices(U32 numberOfVertices)
{
   U32 sizeNeeded = numberOfVertices * sizeof(GFXVertexPCT);
   
   U32 offset = mCurrentVertexData.offset;
   
   AssertFatal(offset + sizeNeeded < VERTEX_BUFFER_SIZE_MB , "Ran out of VBO space");
   GFXVertexPCT* ptr = reinterpret_cast<GFXVertexPCT*>(mCurrentVertexData.data + offset);
   mCurrentVertexData.offset += sizeNeeded;
   
   GuiVertexList list;
   list.vertices = ptr;
   list.count = numberOfVertices;
   list.drawVboStart = offset / sizeof(GFXVertexPCT);
   
   return list;
}

//-----------------------------------------------------------------------------

GuiRenderBinExecutor::GuiRenderBinExecutor()
{
   
}

GuiRenderBinExecutor::~GuiRenderBinExecutor()
{
   
}

void GuiRenderBinExecutor::execute(GFXCmdBuffer *cmdBuffer, GuiRenderManager *renderManager)
{
   const Vector<GuiRenderInst>& renderInsts = renderManager->getRenderInsts();
   void* vertexData = renderManager->vertexData();
   
   uploadVertexData(vertexData);
   
   for (const GuiRenderInst& inst : renderInsts)
   {
      if (inst.pipeline != GFXHandleNone)
      {
         cmdBuffer->setGraphicsPipeline(inst.pipeline);
      }
      
      if (inst.stateBlock != nullptr)
      {
         cmdBuffer->setStateBlock(inst.stateBlock);
      }
      
      cmdBuffer->drawPrimitives(inst.primitiveType, inst.vertexList.drawVboStart, inst.vertexList.count);
   }
   
   cmdBuffer->endRecording();
}

void GuiRenderBinExecutor::createVertexBuffer()
{
   GFXVertexBufferDesc desc = {};
   desc.bufferType = GFXBufferTypeDynamic;
   desc.size = VERTEX_BUFFER_SIZE_MB;
   desc.data = nullptr;
   
   mVertexBufferHandle = GFX->createVertexBuffer(desc);
}

void GuiRenderBinExecutor::uploadVertexData(void* vertexData)
{
   void* ptr = GFX->mapBuffer(mVertexBufferHandle);
   dMemcpy(ptr, vertexData, VERTEX_BUFFER_SIZE_MB);
   GFX->unmapBuffer(mVertexBufferHandle);
}
