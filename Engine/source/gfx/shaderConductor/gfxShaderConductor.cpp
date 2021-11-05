#include "gfx/shaderConductor/gfxShaderConductor.h"
#include "platform/platformDlibrary.h"

#include <ShaderConductor/Native.h>

namespace GFXShaderConductorImpl
{
   void init();
   void shutdown();
}

MODULE_BEGIN(GFX_SHADER_CONDUCTOR)

MODULE_INIT
{
   GFXShaderConductorImpl::init();
}

MODULE_SHUTDOWN
{
   GFXShaderConductorImpl::shutdown();
}

MODULE_END;

namespace GFXShaderConductorImpl
{
   DLibraryRef sShaderConductor;

   void (*compileFn)(SourceDescription*, OptionsDescription*, TargetDescription*, ResultDescription*);

   static void init()
   {
      sShaderConductor = OsLoadLibrary("ShaderConductorWrapper");
      AssertISV(sShaderConductor, "Unable to load ShaderConductorWrapper.dll");

      compileFn = (void(*)(SourceDescription*, OptionsDescription*, TargetDescription*, ResultDescription*))sShaderConductor->bind("Compile");
      AssertISV(compileFn, "Unable to load symbols for ShaderConductorWrapper.dll");
   }

   static void shutdown()
   {
      // TODO: shut down library
   }

   static ShaderConductor::ShaderStage translateStage(GFXShaderTranslator::ShaderStage stage)
   {
      switch (stage)
      {
      case GFXShaderTranslator::ShaderStage::VERTEX_SHADER:
         return ShaderConductor::ShaderStage::VertexShader;
      case GFXShaderTranslator::ShaderStage::PIXEL_SHADER:
         return ShaderConductor::ShaderStage::PixelShader;
      }
   }

   static Vector<MacroDefine> mapMacros(const Vector<GFXShaderMacro>& macros)
   {
      Vector<MacroDefine> macroVec;

      for (const auto& macro : macros)
      {
         ShaderConductor::MacroDefine define;
         define.name = macro.name.c_str();
         define.value = macro.value.c_str();

         macroVec.push_back(define);
      }

      return macroVec;
   }
};

bool GFXShaderConductor::compile(const String& file, ShaderStage stage, UTF8*& outShaderData)
{
   String source;

   const Vector<MacroDefine>& macros = GFXShaderConductorImpl::mapMacros(mShaderMacros);

   SourceDescription sourceDesc;
   sourceDesc.entryPoint = "Main";
   sourceDesc.stage = GFXShaderConductorImpl::translateStage(stage);
   sourceDesc.source = source.c_str();
   sourceDesc.defines = macros.address();
   sourceDesc.numDefines = macros.size();

   OptionsDescription optionsDesc;
   optionsDesc.disableOptimizations = mOptimization == Optimization::NONE;
#ifdef TORQUE_DEBUG
   optionsDesc.enableDebugInfo = true;
#else
   optionsDesc.enableDebugInfo = false;
#endif
   optionsDesc.optimizationLevel = mOptimization == Optimization::FULL ? 3 : 0;
   optionsDesc.shaderModel = ShaderModel{ 5, 0 };

   TargetDescription targetDesc;
   targetDesc.shadingLanguage = ShadingLanguage::Glsl;
   targetDesc.version = "330";

   ResultDescription resultDesc;

   GFXShaderConductorImpl::compileFn(&sourceDesc, &optionsDesc, &targetDesc, &resultDesc);

   if (resultDesc.hasError)
   {
      Con::errorf("ShaderConductor Error: %s", resultDesc.errorWarningMsg);
      return false;
   }

   outShaderData = (UTF8*)resultDesc.target;
   return true;
}
