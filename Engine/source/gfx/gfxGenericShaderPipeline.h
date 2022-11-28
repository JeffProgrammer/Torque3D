#pragma once

#include "gfx/gfxDevice.h"
#include "gfx/gfxStructs.h"

enum GFXGenericShaderPipelineType
{
    GSPColor = 0,
    GSPTexture,
    GSPModColorTexture,
    GSPAddColorTexture,
    GSP_COUNT
};

#define GFXGenericShaderPipeline GFXDevice::get()->getShaderPipelineManager() 

class GFXGenericShaderPipelineManager
{
public:
    virtual void init() = 0;
    virtual GFXHandle<GFXGraphicsPipelineHandle> getShaderPipeline(GFXGenericShaderPipelineType pipelineType) = 0;
};
