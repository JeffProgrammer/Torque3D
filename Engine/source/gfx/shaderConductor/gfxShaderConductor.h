#ifndef _GFX_SHADER_CONDUCTOR_H_
#define _GFX_SHADER_CONDUCTOR_H_

#include "gfx/gfxShaderTranslation.h"

class GFXShaderConductor : public GFXShaderTranslator
{
   virtual bool compile(const String& file, ShaderStage stage, UTF8*& outShaderData) override;
};

#endif
