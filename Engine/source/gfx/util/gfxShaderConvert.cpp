//-----------------------------------------------------------------------------
// Copyright (c) 2012 GarageGames, LLC
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
//-----------------------------------------------------------------------------

#include "gfx/util/gfxShaderConvert.h"
#include "platform/platform.h"
#include "console/console.h"
#include "core/frameAllocator.h"
#include "core/stream/fileStream.h"
#include "shaderGen/shaderGen.h"
#include "glslang/Public/ShaderLang.h"
#include "glslang/SPIRV/GlslangToSpv.h"
#include "spirv_glsl.hpp"

//Use the module system to init/finalize the glslang process
MODULE_BEGIN(GFX_GLSLANG)

MODULE_INIT
{
   glslang::InitializeProcess();
}

MODULE_SHUTDOWN
{
   glslang::FinalizeProcess();
}

MODULE_END;

//default resource limits
const TBuiltInResource DefaultTBuiltInResource =
{
   /* .MaxLights = */ 32,
   /* .MaxClipPlanes = */ 6,
   /* .MaxTextureUnits = */ 32,
   /* .MaxTextureCoords = */ 32,
   /* .MaxVertexAttribs = */ 64,
   /* .MaxVertexUniformComponents = */ 4096,
   /* .MaxVaryingFloats = */ 64,
   /* .MaxVertexTextureImageUnits = */ 32,
   /* .MaxCombinedTextureImageUnits = */ 80,
   /* .MaxTextureImageUnits = */ 32,
   /* .MaxFragmentUniformComponents = */ 4096,
   /* .MaxDrawBuffers = */ 32,
   /* .MaxVertexUniformVectors = */ 128,
   /* .MaxVaryingVectors = */ 8,
   /* .MaxFragmentUniformVectors = */ 16,
   /* .MaxVertexOutputVectors = */ 16,
   /* .MaxFragmentInputVectors = */ 15,
   /* .MinProgramTexelOffset = */ -8,
   /* .MaxProgramTexelOffset = */ 7,
   /* .MaxClipDistances = */ 8,
   /* .MaxComputeWorkGroupCountX = */ 65535,
   /* .MaxComputeWorkGroupCountY = */ 65535,
   /* .MaxComputeWorkGroupCountZ = */ 65535,
   /* .MaxComputeWorkGroupSizeX = */ 1024,
   /* .MaxComputeWorkGroupSizeY = */ 1024,
   /* .MaxComputeWorkGroupSizeZ = */ 64,
   /* .MaxComputeUniformComponents = */ 1024,
   /* .MaxComputeTextureImageUnits = */ 16,
   /* .MaxComputeImageUniforms = */ 8,
   /* .MaxComputeAtomicCounters = */ 8,
   /* .MaxComputeAtomicCounterBuffers = */ 1,
   /* .MaxVaryingComponents = */ 60,
   /* .MaxVertexOutputComponents = */ 64,
   /* .MaxGeometryInputComponents = */ 64,
   /* .MaxGeometryOutputComponents = */ 128,
   /* .MaxFragmentInputComponents = */ 128,
   /* .MaxImageUnits = */ 8,
   /* .MaxCombinedImageUnitsAndFragmentOutputs = */ 8,
   /* .MaxCombinedShaderOutputResources = */ 8,
   /* .MaxImageSamples = */ 0,
   /* .MaxVertexImageUniforms = */ 0,
   /* .MaxTessControlImageUniforms = */ 0,
   /* .MaxTessEvaluationImageUniforms = */ 0,
   /* .MaxGeometryImageUniforms = */ 0,
   /* .MaxFragmentImageUniforms = */ 8,
   /* .MaxCombinedImageUniforms = */ 8,
   /* .MaxGeometryTextureImageUnits = */ 16,
   /* .MaxGeometryOutputVertices = */ 256,
   /* .MaxGeometryTotalOutputComponents = */ 1024,
   /* .MaxGeometryUniformComponents = */ 1024,
   /* .MaxGeometryVaryingComponents = */ 64,
   /* .MaxTessControlInputComponents = */ 128,
   /* .MaxTessControlOutputComponents = */ 128,
   /* .MaxTessControlTextureImageUnits = */ 16,
   /* .MaxTessControlUniformComponents = */ 1024,
   /* .MaxTessControlTotalOutputComponents = */ 4096,
   /* .MaxTessEvaluationInputComponents = */ 128,
   /* .MaxTessEvaluationOutputComponents = */ 128,
   /* .MaxTessEvaluationTextureImageUnits = */ 16,
   /* .MaxTessEvaluationUniformComponents = */ 1024,
   /* .MaxTessPatchComponents = */ 120,
   /* .MaxPatchVertices = */ 32,
   /* .MaxTessGenLevel = */ 64,
   /* .MaxViewports = */ 16,
   /* .MaxVertexAtomicCounters = */ 0,
   /* .MaxTessControlAtomicCounters = */ 0,
   /* .MaxTessEvaluationAtomicCounters = */ 0,
   /* .MaxGeometryAtomicCounters = */ 0,
   /* .MaxFragmentAtomicCounters = */ 8,
   /* .MaxCombinedAtomicCounters = */ 8,
   /* .MaxAtomicCounterBindings = */ 1,
   /* .MaxVertexAtomicCounterBuffers = */ 0,
   /* .MaxTessControlAtomicCounterBuffers = */ 0,
   /* .MaxTessEvaluationAtomicCounterBuffers = */ 0,
   /* .MaxGeometryAtomicCounterBuffers = */ 0,
   /* .MaxFragmentAtomicCounterBuffers = */ 1,
   /* .MaxCombinedAtomicCounterBuffers = */ 1,
   /* .MaxAtomicCounterBufferSize = */ 16384,
   /* .MaxTransformFeedbackBuffers = */ 4,
   /* .MaxTransformFeedbackInterleavedComponents = */ 64,
   /* .MaxCullDistances = */ 8,
   /* .MaxCombinedClipAndCullDistances = */ 8,
   /* .MaxSamples = */ 4,
   /* .maxMeshOutputVerticesNV = */ 256,
   /* .maxMeshOutputPrimitivesNV = */ 512,
   /* .maxMeshWorkGroupSizeX_NV = */ 32,
   /* .maxMeshWorkGroupSizeY_NV = */ 1,
   /* .maxMeshWorkGroupSizeZ_NV = */ 1,
   /* .maxTaskWorkGroupSizeX_NV = */ 32,
   /* .maxTaskWorkGroupSizeY_NV = */ 1,
   /* .maxTaskWorkGroupSizeZ_NV = */ 1,
   /* .maxMeshViewCountNV = */ 4,
   /* .maxDualSourceDrawBuffersEXT = */ 1,

   /* .limits = */ {
      /* .nonInductiveForLoops = */ 1,
      /* .whileLoops = */ 1,
      /* .doWhileLoops = */ 1,
      /* .generalUniformIndexing = */ 1,
      /* .generalAttributeMatrixVectorIndexing = */ 1,
      /* .generalVaryingIndexing = */ 1,
      /* .generalSamplerIndexing = */ 1,
      /* .generalVariableIndexing = */ 1,
      /* .generalConstantMatrixVectorIndexing = */ 1,
  }
};

class GFXShaderIncluder : public glslang::TShader::Includer
{
public:

   GFXShaderIncluder(const String& basePath) : mBasePath(basePath) {}

   virtual ~GFXShaderIncluder()
   {
      for (U32 i = 0; i < mInlcudeResults.size(); i++)
      {
         delete[] mInlcudeResults[i]->userData;
         delete mInlcudeResults[i];
      }
   }

   //sometimes the base file may be from shadergen which uses root, in these cases the base path won't pickup regular include shaders from the common shader folder,
   // we use the common shader folder as the system folder and try and load from there
   virtual IncludeResult* includeSystem(const char* headerName, const char* includerName, size_t inclusionDepth)
   {
      //make the path
      Torque::Path path = Torque::Path::Join(ShaderGen::smCommonShaderPath, '/', headerName);
      path = Torque::Path::CompressPath(path);

      void* pData = nullptr;
      U32 dataSize = 0;
      if (!Torque::FS::ReadFile(path, pData, dataSize, true))
      {
         // Ok... now try using the filename stripped of any relative paths
         path = Torque::Path::Join(ShaderGen::smCommonShaderPath, '/', path.getFullFileName());

         if (!Torque::FS::ReadFile(path, pData, dataSize, true))
         {
            Con::errorf("IncludeHandler: Failed to open include '%s'", path.getFullPath().c_str());
            return nullptr;
         }
      }

      IncludeResult* pResult = new IncludeResult(path.getFullPath().c_str(), (char*)pData, dataSize, nullptr);
      mInlcudeResults.push_back(pResult);
      return pResult;
   }

   virtual IncludeResult* includeLocal(const char* headerName, const char* includerName, size_t inclusionDepth)
   {
      //check if we already have the file
      for (U32 i = 0; i < mInlcudeResults.size(); i++)
      {
         if (mInlcudeResults[i]->headerName == headerName)
            return mInlcudeResults[i];
      }

      // First try making the path relative to the parent.
      Torque::Path path = Torque::Path::Join(mBasePath, '/', headerName);
      path = Torque::Path::CompressPath(path);

      void* pData = nullptr;
      U32 dataSize = 0;
      if (!Torque::FS::ReadFile(path, pData, dataSize, true))
      {
         // Ok... now try using the path as is.
         path = String(headerName);
         path = Torque::Path::CompressPath(path);

         if (!Torque::FS::ReadFile(path, pData, dataSize, true))
         {
            return nullptr; //this will try again in includeSystem function
         }
      }

      IncludeResult* pResult = new IncludeResult(path.getFullPath().c_str(), (char*)pData, dataSize, nullptr);
      mInlcudeResults.push_back(pResult);
      return pResult;
   }

   virtual void releaseInclude(IncludeResult* result) {}


protected:

   Vector<IncludeResult*> mInlcudeResults;
   String mBasePath;
};

namespace
{
   EShLanguage _convertStage(GFXShaderStage stage)
   {
      switch (stage)
      {
      case GFXShaderStageVertex: return EShLangVertex;
      case GFXShaderStagePixel: return EShLangFragment;
      default: return EShLangVertex;
      };
   }

}

bool GFXShaderConvert::convert(const GFXShaderConvertOptions& options, GFXShaderConvertOutput& output)
{
   //check we have valid options
   if (options.sourceLang == GFXShaderLanguageNone || options.targetLang == GFXShaderLanguageNone ||
      options.stage == GFXShaderStageNone || options.filePath.isEmpty())
   {
      Con::errorf("GFXShaderConvert::compile: Invalid conversion options");
      return false;
   }

   //remove this check once we add support for more langugages
   if (options.sourceLang != GFXShaderLanguageHLSL)
   {
      Con::errorf("GFXShaderConvert::compile: We only support HLSL source shaders currently");
      return false;
   }

   //open the source file
   FileStream s;
   if (!s.open(options.filePath, Torque::FS::File::Read))
   {
      Con::errorf("GFXShaderConvert::compile - Failed to open shader file '%s'.", options.filePath.getFullPath().c_str());

      return false;
   }

   //read the source into shaderSourceBuffer
   FrameAllocatorMarker fam;
   char* shaderSourceBuffer = NULL;
   const U32 bufSize = s.getStreamSize();
   shaderSourceBuffer = (char*)fam.alloc(bufSize + 1);
   s.read(bufSize, shaderSourceBuffer);
   shaderSourceBuffer[bufSize] = 0;

   //preamble
   String preamble = String::EmptyString;
   for (U32 i=0; i < options.macros.size(); i++)
   {
      const GFXShaderMacro& macro = options.macros[i];
      if (macro.name.isNotEmpty())
      {
         preamble += String::ToString("#define %s %s\n", macro.name.c_str(), macro.value.c_str());
      }
   }

   EShLanguage stage = _convertStage(options.stage);

   glslang::TShader shader(stage);
   shader.setStrings(&shaderSourceBuffer, 1);
   shader.setPreamble(preamble.c_str());
   shader.setEntryPoint("main"); //t3d doesn't support shader entry points
   shader.setSourceEntryPoint("main"); //t3d doesn't support shader entry points
   shader.setTextureSamplerTransformMode(EShTexSampTransUpgradeTextureRemoveSampler);

   //we can hardcode these for now as we only support hlsl to either spirv or glsl via the vulkan path
   shader.setEnvInput(glslang::EShSourceHlsl, stage, glslang::EShClientVulkan, 100);
   shader.setEnvClient(glslang::EShClientVulkan, glslang::EShTargetVulkan_1_1);
   shader.setEnvTarget(glslang::EShTargetSpv, glslang::EShTargetSpv_1_3);

   //setup the include loader
   GFXShaderIncluder includer(options.filePath.getRootAndPath());

   EShMessages messages = (EShMessages)(EShMsgSpvRules | EShMsgReadHlsl | EShMsgHlslLegalization);

   //parse the shader
   if (!shader.parse(&DefaultTBuiltInResource, 100, false, messages, includer))
   {
      Con::errorf("Shader failed to compile: %s", options.filePath.getFullPath().c_str());
      Con::errorf("Shader Info Log: %s", shader.getInfoLog());

      //uncomment to see the final source code that is failing
      //std::string failedSrcCode;
      //shader.preprocess(&DefaultTBuiltInResource, 100, ENoProfile, false, false, messages, &failedSrcCode, includer);
      return false;
   }

   glslang::TProgram program;
   program.addShader(&shader);

   //link the shader
   if (!program.link(messages))
   {
      Con::errorf("Shader program failed to link: %s", options.filePath.getFullPath().c_str());
      Con::errorf("Shader Info Log: %s", shader.getInfoLog());
      return false;
   }

   //can't use torque Vector, it requires a std::vector :(
   std::vector<U32> spirv;
   glslang::GlslangToSpv(*program.getIntermediate(stage), spirv);

   if (!spirv.size())
   {
      Con::errorf("GFXShaderConvert::convert: Failed to generate SPIR-V for shader: %s", options.filePath.getFullPath().c_str());
      return false;
   }

   //finalize the targets
   if (options.targetLang == GFXShaderLanguageSPIRV)
   {      
      output.language = GFXShaderLanguageSPIRV;
      //copy the std::vector to the t3d vector
      output.srcUint.setSize((U32)spirv.size());
      dMemcpy(output.srcUint.address(), spirv.data(), spirv.size() * sizeof(U32));
      return true;
   }
   else if (options.targetLang == GFXShaderLanguageGLSL)
   {
      //spirv_cross for spirv->glsl
      spirv_cross::CompilerGLSL compiler(spirv);

      //Compiler options
      spirv_cross::CompilerGLSL::Options glslOptions = {};
      glslOptions.vertex.flip_vert_y = true;
      glslOptions.separate_shader_objects = true;
      glslOptions.emit_uniform_buffer_as_plain_uniforms = false;
      glslOptions.enable_420pack_extension = false;
      glslOptions.emit_push_constant_as_uniform_buffer = true;
      glslOptions.emit_line_directives = true;
      glslOptions.version = 410;
      
      //set the options
      compiler.set_common_options(glslOptions);

      //compile to glsl
      std::string glslSrc = compiler.compile();

      if (glslSrc.empty())
      {
         Con::errorf("GFXShaderConvert::convert: Failed to generate GLSL from SPIR-V for shader: %s", options.filePath.getFullPath().c_str());
         return false;
      }

      //set output language
      output.language = GFXShaderLanguageGLSL;
      //copy the source string
      output.srcString = glslSrc.c_str();

      return true;
   }

   Con::errorf("GFXShaderConvert::convert: We don't support this converting to this target lanague currently.");
   return false;
}
