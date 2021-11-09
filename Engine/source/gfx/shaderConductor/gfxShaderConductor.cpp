#include "gfx/shaderConductor/gfxShaderConductor.h"
#include "platform/platformDlibrary.h"

#include <ShaderConductor/Native.h>

namespace GFXShaderConductorImpl
{
   static void init();
   static void shutdown();
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

GFXShaderTranslator* GFXShaderTranslator::create()
{
   return new GFXShaderConductor();
}

namespace GFXShaderConductorImpl
{
   DLibraryRef sShaderConductor;

   typedef void (*CompileProc)(SourceDescription*, OptionsDescription*, TargetDescription*, ResultDescription*);
   typedef ShaderConductorBlob* (*CreateBlobProc)(const void*, int);
   typedef void (*DestroyBlobProc)(ShaderConductorBlob*);
   typedef const void* (*GetBlobDataProc)(ShaderConductorBlob*);
   typedef int (*GetBlobSizeProc)(ShaderConductorBlob*);

   CompileProc compileFn;
   CreateBlobProc createBlobFn;
   DestroyBlobProc destroyBlobFn;
   GetBlobDataProc getBlobDataFn;
   GetBlobSizeProc getBlobSizeFn;

   static void init()
   {
#if defined(TORQUE_OS_WINDOWS)
      const char* lib = "ShaderConductorWrapper.dll";
#elif defined(TORQUE_OS_MAC)
      const char* lib = "libShaderConductorWrapper.dylib";
#else
      const char* lib = "libShaderConductorWrapper.so";
#endif
      
      sShaderConductor = OsLoadLibrary(lib);
      AssertISV(sShaderConductor, avar("Unable to load dynamic library %s", lib));

      compileFn = (CompileProc)sShaderConductor->bind("Compile");
      createBlobFn = (CreateBlobProc)sShaderConductor->bind("CreateShaderConductorBlob");
      destroyBlobFn = (DestroyBlobProc)sShaderConductor->bind("DestroyShaderConductorBlob");
      getBlobDataFn = (GetBlobDataProc)sShaderConductor->bind("GetShaderConductorBlobData");
      getBlobSizeFn = (GetBlobSizeProc)sShaderConductor->bind("GetShaderConductorBlobSize");
   }

   static void shutdown()
   {
      delete sShaderConductor;
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

bool GFXShaderConductor::compile(const String& source, ShaderStage stage, UTF8*& outShaderData)
{
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
      Con::errorf("ShaderConductor Error: %s", (char*)GFXShaderConductorImpl::getBlobDataFn(resultDesc.errorWarningMsg));
      return false;
   }

   S32 shaderSize = GFXShaderConductorImpl::getBlobSizeFn(resultDesc.target);
   const char* data = (const char*)GFXShaderConductorImpl::getBlobDataFn(resultDesc.target);
   
   //outShaderData = UTF8(data);
   return true;
}
