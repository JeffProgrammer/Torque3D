#ifndef _GUI_GUI_RENDER_H_
#define _GUI_GUI_RENDER_H_

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
   GFXVertexPCT *vertices;
   
   U32 count;
   U32 drawVboStart;
};

enum class GuiRenderInstType
{
   CLIP_RECT,
   STATEBLOCK,
   DRAW
};

struct GuiRenderInst
{
   friend class GuiRenderBin;
   friend class GuiRenderManager;
protected:
   GuiRenderInstType type;
};

struct GuiRenderInstDraw : public GuiRenderInst
{
   GFXHandle<GFXGraphicsPipelineHandle> pipeline;
   GFXTextureObject *texture = nullptr; // TODO: Replace with GFXHandle<Texture> when we move to new GFX fully
   GFXHandle<GFXSamplerHandle> sampler;
   
   GuiVertexList vertexList;
   GFXPrimitiveType primitiveType;
};

struct GuiRenderInstClipRect : public GuiRenderInst
{
   RectI rect;
   MatrixF projMatrix;
};

struct GuiRenderInstStateBlock : public GuiRenderInst
{
   GFXStateBlock* stateBlock = nullptr;
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
   GuiRenderInstStateBlock* allocRenderInstStateBlock();
   
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
   void createVertexBuffer();
   void uploadVertexData(void* vertexData);
   
   GFXHandle<GFXVertexBufferHandle2> mVertexBufferHandle;
};

#endif
