#include "gfx/gfxGenericShaderPipeline.h"
#include "gfx/gfxStructs.h"

class GFXGLGenericShaderPipelineManager : public GFXGenericShaderPipelineManager
{
public:
   virtual void init() override;
   virtual GFXHandle<GFXGraphicsPipelineHandle> getShaderPipeline(GFXGenericShaderPipelineType pipelineType) override;

protected:
   GFXShader* createShader(const String& vertPath, const String& pixelPath) const;

   GFXHandle<GFXGraphicsPipelineHandle> mPipelines[GFXGenericShaderPipelineType::GSP_COUNT];
};
