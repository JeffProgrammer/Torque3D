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

GuiRenderInstDraw* GuiRenderManager::allocRenderInstDraw(U32 numberOfVertices)
{
   GuiRenderInstDraw* inst = mRenderInstMemory.alloc<GuiRenderInstDraw>();
   constructInPlace(inst);
   
   inst->type = GuiRenderInstType::DRAW;
   inst->vertexList = allocVertices(numberOfVertices);
   
   return inst;
}

GuiRenderInstClipRect* GuiRenderManager::allocRenderInstClipRect()
{
   GuiRenderInstClipRect* inst = mRenderInstMemory.alloc<GuiRenderInstClipRect>();
   constructInPlace(inst);
   
   inst->type = GuiRenderInstType::CLIP_RECT;
   
   return inst;
}

GuiRenderInstStateBlock* GuiRenderManager::allocRenderInstStateBlock()
{
   GuiRenderInstStateBlock* inst = mRenderInstMemory.alloc<GuiRenderInstStateBlock>();
   constructInPlace(inst);

   inst->type = GuiRenderInstType::STATEBLOCK;

   return inst;
}

void GuiRenderManager::clear()
{
   mCurrentVertexData.count = 0;
   mCurrentVertexData.offset = 0;
   dMemset(mCurrentVertexData.data, 0, VERTEX_BUFFER_SIZE_MB);
   mRenderInsts.clear();
   mRenderInstMemory.clear();
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

static TORQUE_FORCEINLINE
void processClipRectInst(GFXCmdBuffer* cmdBuffer, const GuiRenderInstClipRect* clipRectInst)
{
   const RectI rect = clipRectInst->rect;
   const float* matrix = (const float*)clipRectInst->projMatrix;

   cmdBuffer->setViewport(rect);
   cmdBuffer->setPushConstants(GFXShaderStageVertex, sizeof(MatrixF), (void*)matrix);
}

static TORQUE_FORCEINLINE
void processStateBlockInst(GFXCmdBuffer* cmdBuffer, const GuiRenderInstStateBlock* stateBlockInst)
{
   if (stateBlockInst->stateBlock != nullptr)
      cmdBuffer->setStateBlock(stateBlockInst->stateBlock);
}

static TORQUE_FORCEINLINE
void processDrawInst(GFXCmdBuffer* cmdBuffer, const GuiRenderInstDraw* drawInst, GFXHandle<GFXVertexBufferHandle2> vertexBuffer)
{
   if (drawInst->pipeline != GFXHandleNone)
   {
      cmdBuffer->setGraphicsPipeline(drawInst->pipeline);

      // We use the same vbo for all...only set it once if we haven't
      // we need to set it everytime until we force separate vertex attribs in opengl
      cmdBuffer->setVertexBuffer(0, vertexBuffer, 0);
   }

   if (drawInst->texture != nullptr || drawInst->sampler != GFXHandleNone)
   {
      AssertFatal(drawInst->texture != nullptr, "GuiRenderBinExecutor::execute Must set texture if setting sampler");
      AssertFatal(drawInst->sampler != GFXHandleNone, "GuiRenderBinExecutor::execute Must set sampler if setting texture");

      cmdBuffer->setTexture(0, drawInst->texture, drawInst->sampler);
   }

   AssertFatal(drawInst->primitiveCount > 0, "GuiRenderBinExecutor::execute Must have primitive count greater than 0");
   cmdBuffer->drawPrimitives(drawInst->primitiveType, drawInst->vertexList.drawVboStart, drawInst->primitiveCount);
}


GuiRenderBin::GuiRenderBin()
{
   
}

GuiRenderBin::~GuiRenderBin()
{
   
}

void GuiRenderBin::fillCommandBuffer(GFXCmdBuffer *cmdBuffer, GuiRenderManager *renderManager)
{
   const Vector<GuiRenderInst*>& renderInsts = renderManager->getRenderInsts();
   void* vertexData = renderManager->vertexData();
   
   uploadVertexData(vertexData);

   for (const GuiRenderInst* inst : renderInsts)
   {
      switch (inst->type)
      {
         case GuiRenderInstType::CLIP_RECT:
         {
            const auto clipRectInst = static_cast<const GuiRenderInstClipRect*>(inst);
            processClipRectInst(cmdBuffer, clipRectInst);
            break;
         }

         case GuiRenderInstType::STATEBLOCK:
         {
            const auto stateBlockInst = static_cast<const GuiRenderInstStateBlock*>(inst);
            processStateBlockInst(cmdBuffer, stateBlockInst);
            break;
         }
         
         case GuiRenderInstType::DRAW:
         {
            const auto drawInst = static_cast<const GuiRenderInstDraw*>(inst);
            processDrawInst(cmdBuffer, drawInst, mVertexBufferHandle);
            break;
         }
      }
   }
   
   cmdBuffer->endRecording();
}

void GuiRenderBin::createVertexBuffer()
{
   GFXVertexBufferDesc desc = {};
   desc.bufferType = GFXBufferTypeDynamic;
   desc.size = VERTEX_BUFFER_SIZE_MB;
   desc.data = nullptr;
   
   mVertexBufferHandle = GFX->createVertexBuffer(desc);
}

void GuiRenderBin::uploadVertexData(void* vertexData)
{
   void* ptr = GFX->mapBuffer(mVertexBufferHandle);
   dMemcpy(ptr, vertexData, VERTEX_BUFFER_SIZE_MB);
   GFX->unmapBuffer(mVertexBufferHandle);
}
