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

#ifndef _CUSTOM_FEATURE_HLSL_H_
#define _CUSTOM_FEATURE_HLSL_H_

#ifndef _SHADERGEN_HLSL_SHADERFEATUREHLSL_H_
#include "shaderGen/HLSL/shaderFeatureHLSL.h"
#endif
#ifndef _LANG_ELEMENT_H_
#include "shaderGen/langElement.h"
#endif
#ifndef _GFXDEVICE_H_
#include "gfx/gfxDevice.h"
#endif
#ifndef _FEATUREMGR_H_
#include "shaderGen/featureMgr.h"
#endif
#ifndef _MATERIALFEATURETYPES_H_
#include "materials/materialFeatureTypes.h"
#endif
#ifndef _MATERIALFEATUREDATA_H_
#include "materials/materialFeatureData.h"
#endif

#ifndef CUSTOMSHADERFEATURE_H
#include "shaderGen/customShaderFeature.h"
#endif

class CustomShaderFeatureData;

class CustomFeatureHLSL : public ShaderFeatureHLSL
{
   friend class CustomShaderFeatureData;

   struct VarHolder
   {
      String varName;
      String type;
      String defaultValue;
      S32 elementId;
      bool uniform;
      bool sampler;
      bool texture;
      bool texCoord;
      U32 constNum;
      U32 arraySize;
      String structName;
      ConstantSortPosition constSortPos;

      VarHolder() :
         varName(""),
         type(""),
         defaultValue(""),
         elementId(-1),
         uniform(false),
         sampler(false),
         texture(false),
         texCoord(false),
         constNum(0),
         arraySize(0),
         structName(""),
         constSortPos(cspUninit)
      {
      }

      VarHolder(String _varName, String _type, String _defaultValue) :
         elementId(-1), uniform(false), sampler(false), texture(false), texCoord(false), constNum(0), arraySize(0), structName(""), constSortPos(cspUninit)
      {
         varName = _varName;
         type = _type;
         defaultValue = _defaultValue;
      }
   };

   Vector<VarHolder> mVars;

   Vector<VarHolder> mConnectorVars;

   enum outputState
   {
      NoOutput,
      VertexOutput,
      PixelOutput
   };

   outputState mOutputState;

public:
   CustomShaderFeatureData* mOwner;

   Vector<ShaderComponent*> mComponentList;
   MaterialFeatureData mFeatureData;

protected:
   MultiLine* meta;

public:

   //****************************************************************************
   // Accu Texture
   //****************************************************************************
   virtual void processVert(Vector<ShaderComponent*>& componentList,
      const MaterialFeatureData& fd);

   virtual void processPix(Vector<ShaderComponent*>& componentList,
      const MaterialFeatureData& fd);

   virtual Material::BlendOp getBlendOp() { return Material::LerpAlpha; }

   virtual Resources getResources(const MaterialFeatureData& fd)
   {
      Resources res;
      res.numTex = 1;
      res.numTexReg = 1;
      return res;
   }

   virtual void setTexData(Material::StageData& stageDat,
      const MaterialFeatureData& fd,
      RenderPassData& passData,
      U32& texIndex);

   virtual String getName()
   {
      return mOwner->getName();
   }

   bool hasFeature(String name);

   void addUniform(String name, String type, String defaultValue, U32 arraySize = 0);
   void addVariable(String name, String type, String defaultValue);
   void addSampler(String name, String type, U32 arraySize = 0);
   void addTexture(String name, String type, String samplerState, U32 arraySize);
   void addConnector(String name, String type, String elementName);
   void addVertTexCoord(String name);
   void writeLine(String format, S32 argc, ConsoleValue* argv);
};

#endif
