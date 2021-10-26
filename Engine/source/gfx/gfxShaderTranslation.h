#ifndef _GFX_SHADER_TRANSLATION_H_
#define _GFX_SHADER_TRANSLATION_H_

#include "platform/platform.h"
#include "gfx/gfxStructs.h"

class GFXShaderTranslator
{
public:
   enum class Language
   {
      GLSL
   };

   enum class Optimization
   {
      NONE,
      FULL
   };

   enum class ShaderStage
   {
      VERTEX_SHADER,
      PIXEL_SHADER
   };

   inline void setLanguage(Language language);
   inline void setOptimizationLevel(Optimization optimization);
   inline void setMacros(const Vector<GFXShaderMacro>& macros);

   virtual bool compile(const String& file, ShaderStage stage) = 0;

protected:
   Language mLanguage = Language::GLSL;
   Optimization mOptimization = Optimization::NONE;
   Vector<GFXShaderMacro> mShaderMacros;
};

#endif
