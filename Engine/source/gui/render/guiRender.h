#ifndef GUI_GUI_RENDER_H
#define GUI_GUI_RENDER_H

#include "platform/platform.h"
#include "gfx/gfxEnums.h"
#include "gfx/gfxStructs.h"
#include "core/dataChunker.h"

class GFXStateBlock;
class GFXTextureObject;

class GFXCmdBuffer;
class GFXDevice;

struct GuiVertexList
{
   friend class GuiRenderManager;
   friend class GuiRenderBin;
   
   GFXVertexPCT *vertices;
   
private:
   U32 count;
   U32 drawVboStart;
};

enum class GuiRenderInstType
{
   CLIP_RECT,
   DRAW
};

struct GuiRenderInst
{
   GuiRenderInstType type;
};

struct GuiRenderInstDraw : public GuiRenderInst
{
   GFXHandle<GFXGraphicsPipelineHandle> pipeline;
   GFXStateBlock *stateBlock;
   GFXTextureObject *texture; // TODO: Replace with GFXHandle<Texture> when we move to new GFX fully
   GFXHandle<GFXSamplerHandle> sampler;
   
   GuiVertexList vertexList;
   GFXPrimitiveType primitiveType;
};

struct GuiRenderInstClipRect : public GuiRenderInst
{
   RectI rect;
   MatrixF projMatrix;
};

enum GuiRenderConstants
{
   MEGABYTE = 1 << 20,
   VERTEX_BUFFER_SIZE_MB = MEGABYTE * 4,
};

// This class gets injected into the ::onRender functions in Gui
class GuiRenderManager
{
public:
   GuiRenderManager();
   ~GuiRenderManager();
   
   
   GuiRenderInstDraw* allocRenderInstDraw(U32 numberOfVertices);
   GuiRenderInstClipRect* allocRenderInstClipRect();
   
   void clear();
   
   inline void* vertexData()
   {
      return mCurrentVertexData.data;
   }
   
   inline const Vector<GuiRenderInst*>& getRenderInsts()
   {
      return mRenderInsts;
   }
   
private:
   struct VertexData
   {
      char* data;
      U32 offset;
      S32 count;
   };
   
   GuiVertexList allocVertices(U32 numberOfVertices);
   
   VertexData mCurrentVertexData;
   
   // TODO: make this a vector so we can use more than 1 4MB buffer
   //Vector<VertexData> mVertexData;
   
   Vector<GuiRenderInst*> mRenderInsts;
   MultiTypedChunker mRenderInstMemory;
};

// This class actually converts GuiRenderInst into GFXCmdBuffer calls.
class GuiRenderBin
{
public:
   GuiRenderBin();
   ~GuiRenderBin();
   void fillCommandBuffer(GFXCmdBuffer *cmdBuffer, GuiRenderManager* renderManager);
   
private:
   void processClipRectInst(GFXCmdBuffer *cmdBuffer, const GuiRenderInstClipRect* clipRectInst);
   void processDrawInst(GFXCmdBuffer *cmdBuffer, const GuiRenderInstDraw* drawInst);
   
   void createVertexBuffer();
   void uploadVertexData(void* vertexData);
   
   GFXHandle<GFXVertexBufferHandle2> mVertexBufferHandle;
};

#endif
