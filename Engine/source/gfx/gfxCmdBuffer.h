#include "gfx/gfxStructs.h"

class GFXGLDevice;

enum GFXCommand
{
   GFX_CMD_VIEWPORT,
   GFX_CMD_SET_GRAPHICS_PIPELINE,
   GFX_CMD_SET_STATE_BLOCK,
   
   GFX_CMD_SET_VERTEX_BUFFER,
   GFX_CMD_SET_INDEX_BUFFER,
   GFX_CMD_SET_BIND_GROUP,
   GFX_CMD_SET_PUSH_TEXTURE_BIND_GROUP,
   GFX_CMD_SET_PUSHCONSTANTS,

   GFX_CMD_DRAW_PRIMITIVES,
   GFX_CMD_DRAW_PRIMITIVES_INSTANCED,
   GFX_CMD_DRAW_INDEXED_PRIMITIVES,
   GFX_CMD_DRAW_INDEXED_PRIMITIVES_INSTANCED,

   GFX_CMD_END
};

class GFXCmdBuffer
{
public:
   enum
   {
      MEMORY_BUFFER_PAGE_SIZE = 16384,
      MEMORY_CONSTANT_PAGE_SIZE = 16384 * 4,
      MAX_STATE_BLOCK_RESOURCES = 4096,
      MAX_TEXTURE_RESOURCES = 8912 // TODO: Remove once we use GFXTextureHandle
   };

   GFXCmdBuffer(GFXDevice* device);
   ~GFXCmdBuffer();

   void setViewport(S32 x, S32 y, S32 w, S32 h);
   void setGraphicsPipeline(GFXHandle<GFXGraphicsPipelineHandle> pipeline);
   void setStateBlock(GFXStateBlockRef stateBlock);

   void setVertexBuffer(U32 slot, GFXHandle<GFXVertexBufferHandle2> buffer, U32 offset);
   void setIndexBuffer(GFXHandle<GFXIndexBufferHandle> buffer, bool is16Bit);
   void setBindGroup(U32 set, GFXHandle<GFXBindGroupHandle> bindGroup, U32 offsetCount, U32* offsets);
   void setPushTextureBindGroup(U32 set, U32 count, const GFXPushTextureDesc* desc);
   void setPushConstants(GFXShaderStageFlags stageFlags, U32 size, void* data);
   
   void drawPrimitives(GFXPrimitiveType type, U32 start, U32 count);
   void drawPrimitivesInstanced(GFXPrimitiveType type, U32 start, U32 count, U32 instanceCount);
   void drawIndexedPrimitives(GFXPrimitiveType type, U32 count, U32 indexBufferOffset, U32 baseVertex);
   void drawIndexedPrimitivesInstanced(GFXPrimitiveType type, U32 count, U32 indexBufferOffset, U32 baseVertex, U32 instanceCount);

   void endRecording();

private:
   friend class GFXGLDevice;
   GFXDevice* mDevice;

   struct StateBlockData
   {
      GFXStateBlockRef stateBlocks[MAX_STATE_BLOCK_RESOURCES];
      U32 count;
   } mStateBlockData;
   
   struct TextureData
   {
      GFXTextureObject* textures[MAX_TEXTURE_RESOURCES];
      U32 count;
   } mTextureData;

   struct PushConstantStruct
   {
      U8 data[MEMORY_CONSTANT_PAGE_SIZE];
      U32 count;
   } mPushConstantMemory;

   struct
   {
      TORQUE_FORCEINLINE void append(U32 datum)
      {
         AssertFatal(offset < MEMORY_BUFFER_PAGE_SIZE, "Ran out of CommandBuffer space!");
         
         data[offset++] = datum;
      }

      U32 data[MEMORY_BUFFER_PAGE_SIZE];
      U32 offset;
   } mBuffer;

   bool mRecording;
};
