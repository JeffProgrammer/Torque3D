#include "gfx/shaderConductor/gfxShaderConductor.h"
#include "platform/platformDlibrary.h"

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
   bool (*compileFn)();

   void init()
   {
      sShaderConductor = OsLoadLibrary("ShaderConductorWrapper");
      AssertISV(sShaderConductor, "Unable to load ShaderConductorWrapper.dll");

      compileFn = reinterpret_cast<bool(*)()>(sShaderConductor->bind("Compile"));
      AssertISV(compileFn, "Unable to load symbols for ShaderConductorWrapper.dll");
   }

   void shutdown()
   {
      // TODO: shut down library
   }
};

bool GFXShaderConductor::compile(const String& file, ShaderStage stage)
{
   return GFXShaderConductorImpl::compileFn();
}
