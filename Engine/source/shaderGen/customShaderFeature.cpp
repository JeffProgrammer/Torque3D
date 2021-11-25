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

#include "shaderGen/customShaderFeature.h"
#include "shaderGen/HLSL/customFeatureHLSL.h"
#include "math/mathIO.h"
#include "scene/sceneRenderState.h"
#include "core/stream/bitStream.h"
#include "materials/sceneData.h"
#include "gfx/gfxDebugEvent.h"
#include "gfx/gfxTransformSaver.h"
#include "renderInstance/renderPassManager.h"


IMPLEMENT_CONOBJECT(CustomShaderFeatureData);

ConsoleDocClass(CustomShaderFeatureData,
   "@brief A Shader Feature with custom definitions.\n\n"
   "This class allows for the creation and implementation of a ShaderGen ShaderFeature "
   "Implemented either engine side or script, and facilitates passing along of per-instance "
   "ShaderData. "
   "@ingroup Examples\n");

//-----------------------------------------------------------------------------
// Object setup and teardown
//-----------------------------------------------------------------------------
CustomShaderFeatureData::CustomShaderFeatureData()
{
   mFeatureHLSL = nullptr;
}

CustomShaderFeatureData::~CustomShaderFeatureData()
{
}

//-----------------------------------------------------------------------------
// Object Editing
//-----------------------------------------------------------------------------
void CustomShaderFeatureData::initPersistFields()
{
   // SceneObject already handles exposing the transform
   Parent::initPersistFields();
}

bool CustomShaderFeatureData::onAdd()
{
   if (!Parent::onAdd())
      return false;

   mFeatureHLSL = new CustomFeatureHLSL();
   mFeatureHLSL->mOwner = this;

   return true;
}

void CustomShaderFeatureData::onRemove()
{
   Parent::onRemove();
}

//Shadergen setup functions
void CustomShaderFeatureData::addVariable(String name, String type, String defaultValue)
{
   mFeatureHLSL->addVariable(name, type, defaultValue);
}

void CustomShaderFeatureData::addUniform(String name, String type, String defaultValue, U32 arraySize)
{
   mFeatureHLSL->addUniform(name, type, defaultValue, arraySize);
}

void CustomShaderFeatureData::addSampler(String name, String type, U32 arraySize)
{
   mFeatureHLSL->addSampler(name, type, arraySize);
}

void CustomShaderFeatureData::addTexture(String name, String type, String samplerState, U32 arraySize)
{
   mFeatureHLSL->addTexture(name, type, samplerState, arraySize);
}

void CustomShaderFeatureData::addConnector(String name, String type, String elementName)
{
   mFeatureHLSL->addConnector(name, type, elementName);
}

void CustomShaderFeatureData::addVertTexCoord(String name)
{
   mFeatureHLSL->addVertTexCoord(name);
}

bool CustomShaderFeatureData::hasFeature(String name)
{
   return mFeatureHLSL->hasFeature(name);
}

void CustomShaderFeatureData::writeLine(String format, S32 argc, ConsoleValue* argv)
{
   mFeatureHLSL->writeLine(format, argc, argv);
}

DefineEngineMethod(CustomShaderFeatureData, addVariable, void, (String name, String type, String defaultValue), ("", "", ""), "")
{
   object->addVariable(name, type, defaultValue);
}

DefineEngineMethod(CustomShaderFeatureData, addUniform, void, (String name, String type, String defaultValue, U32 arraySize), ("", "", "", 0), "")
{
   object->addUniform(name, type, defaultValue, arraySize);
}

DefineEngineMethod(CustomShaderFeatureData, addSampler, void, (String name, U32 arraySize), ("", 0), "")
{
   object->addSampler(name, "", arraySize);
}

DefineEngineMethod(CustomShaderFeatureData, addTexture, void, (String name, String type, String samplerState, U32 arraySize), ("", "", 0), "")
{
   object->addTexture(name, type, samplerState, arraySize);
}

DefineEngineMethod(CustomShaderFeatureData, addConnector, void, (String name, String type, String elementName), ("", "", ""), "")
{
   object->addConnector(name, type, elementName);
}

DefineEngineMethod(CustomShaderFeatureData, addVertTexCoord, void, (String name), (""), "")
{
   object->addVertTexCoord(name);
}

DefineEngineStringlyVariadicMethod(CustomShaderFeatureData, writeLine, void, 3, 0, "( string format, string args... ) Dynamically call a method on an object.\n"
   "@param method Name of method to call.\n"
   "@param args Zero or more arguments for the method.\n"
   "@return The result of the method call.")
{
   object->writeLine(argv[2].getString(), argc - 3, argv + 3);
}

DefineEngineMethod(CustomShaderFeatureData, hasFeature, bool, (String name), (""), "")
{
   return object->hasFeature(name);
}
