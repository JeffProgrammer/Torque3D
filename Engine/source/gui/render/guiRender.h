#ifndef GUI_GUI_RENDER_H
#define GUI_GUI_RENDER_H

#include "gfx/gfxEnums.h"
#include "gfx/gfxStructs.h"

class GFXStateBlock;
class GFXTextureObject;

struct GuiVertexList
{
   GFXVertexPCT *vertices;
   
private:
   U32 count;
   U32 internalOffset;
};

struct GuiRenderInst
{
   GFXHandle<PipelineHandle> pipeline;
   GFXStateBlock *stateBlock;
   GFXTextureObject *texture; // TODO: Replace with GFXHandle<Texture> when we move to new GFX fully
   
   GuiVertexList *vertexList;
   GFXPrimitiveType primitiveType;
};

// This class gets injected into the ::onRender functions in Gui
class GuiRenderManager
{
   friend struct GuiVertexList;
public:
   GuiRenderInst* allocRenderInst() { return nullptr; }
   
   GuiVertexList* allocVertexList(U32 numberOfVertices);
};

// This class actually converts GuiRenderInst into GFXCmdBuffer calls.
class GuiRenderBinExecutor
{
   
};

#endif
