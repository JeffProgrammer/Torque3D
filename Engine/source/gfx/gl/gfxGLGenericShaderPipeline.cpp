#include "gfx/gl/gfxGLGenericShaderPipeline.h"
#include "gfx/gl/gfxGLVertexAttribLocation.h"

// Note: We use the same vertex format of PCT even for generic shaders that do not need all 3.
// this is so we can pack buffers, at the expense of slightly wasted space.
static void getInputLayout(GFXInputLayoutDesc &inputLayout)
{
   GFXInputLayoutElementDesc positionElement = {};
   positionElement.semanticName = GFXSemantic::POSITION;
   positionElement.type = GFXDeclType_Float3;

   GFXInputLayoutElementDesc colorElement = {};
   colorElement.semanticName = GFXSemantic::COLOR;
   colorElement.type = GFXDeclType_Color;

   GFXInputLayoutElementDesc textCoordElement = {};
   textCoordElement.semanticName = GFXSemantic::TEXCOORD;
   textCoordElement.semanticIndex = 0;
   textCoordElement.type = GFXDeclType_Float2;

   GFXInputLayoutBufferDesc inputLayoutBuffer = {};
   inputLayoutBuffer.isInstancingBuffer = false;
   inputLayoutBuffer.stride = sizeof(GFXVertexPCT);
   inputLayoutBuffer.elements[0] = positionElement;
   inputLayoutBuffer.elements[1] = colorElement;
   inputLayoutBuffer.elements[2] = textCoordElement;
   inputLayoutBuffer.count = 3;

   inputLayout.buffers[0] = inputLayoutBuffer;
   inputLayout.count = 1;
}

void GFXGLGenericShaderPipelineManager::init()
{
   GFXGraphicsPipelineDesc commonDesc = {};
   getInputLayout(commonDesc.inputLayout);
   commonDesc.pushConstantSize = sizeof(MatrixF);

   commonDesc.shader = createShader(String("/fixedFunction/gfx2/colorV.glsl"), String("/fixedFunction/gfx2/colorP.glsl"));
   mPipelines[GFXGenericShaderPipelineType::GSPColor] = GFX->createGraphicsPipeline(commonDesc);

   commonDesc.shader = createShader(String("/fixedFunction/gfx2/textureV.glsl"), String("/fixedFunction/gfx2/textureP.glsl"));
   mPipelines[GFXGenericShaderPipelineType::GSPTexture] = GFX->createGraphicsPipeline(commonDesc);

   commonDesc.shader = createShader(String("/fixedFunction/gfx2/modColorTextureV.glsl"), String("/fixedFunction/gfx2/modColorTextureP.glsl"));
   mPipelines[GFXGenericShaderPipelineType::GSPModColorTexture] = GFX->createGraphicsPipeline(commonDesc);

   commonDesc.shader = createShader(String("/fixedFunction/gfx2/addColorTextureV.glsl"), String("/fixedFunction/gfx2/addColorTextureP.glsl"));
   mPipelines[GFXGenericShaderPipelineType::GSPAddColorTexture] = GFX->createGraphicsPipeline(commonDesc);
}

GFXHandle<GFXGraphicsPipelineHandle> GFXGLGenericShaderPipelineManager::getShaderPipeline(GFXGenericShaderPipelineType pipelineType)
{
   return mPipelines[pipelineType];
}

GFXShader* GFXGLGenericShaderPipelineManager::createShader(const String& vertPath, const String& pixelPath) const
{
   Torque::Path vertFile = String("core/rendering/shaders") + vertPath;
   Torque::Path pixelFile = String("core/rendering/shaders") + pixelPath;

   Vector<GFXShaderMacro> macros;
   Vector<String> samplers;

   GFXShader* shader = GFX->createShader();
   bool success = shader->init(vertFile, pixelFile, 5.0, macros, samplers, true);
   AssertISV(success, "GFXGLGenericShaderPipelineManager :: Failed to Init Shader");
   
   return shader;
}
