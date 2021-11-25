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

#ifndef _GFX_UTIL_GFXSHADERCONVERT_H_
#define _GFX_UTIL_GFXSHADERCONVERT_H_

#include "core/util/str.h"
#include "core/util/tVector.h"
#include "core/util/path.h"
#include "gfx/gfxStructs.h"

//maybe move these to gfxEnums.h?
enum GFXShaderStage
{
   GFXShaderStageNone,
   GFXShaderStageVertex,
   GFXShaderStagePixel
};

enum GFXShaderLanguage
{
   GFXShaderLanguageNone,
   GFXShaderLanguageHLSL,
   GFXShaderLanguageGLSL,
   GFXShaderLanguageSPIRV
};

//maybe move to gfxStructs.h?
struct GFXShaderConvertOptions
{
   //Path of the source shader to be converted
   Torque::Path filePath;
   //Language of the source shader
   GFXShaderLanguage sourceLang = GFXShaderLanguageNone;
   //Language of the target shader
   GFXShaderLanguage targetLang = GFXShaderLanguageNone;
   //Shader stage
   GFXShaderStage stage = GFXShaderStageNone;
   //Optional macros
   Vector<GFXShaderMacro> macros;
   //Optimizations
   bool optimize = false;
   //Validate - not yet implemented
   bool validate = false;
};

struct GFXShaderConvertOutput
{
   //language of the output source - GFXShaderLanguageSPIRV uses srcUint, the rest use srcString
   GFXShaderLanguage language;
   //source output
   String srcString;
   Vector<U32> srcUint;
};

class GFXShaderConvert
{
public:
   //only converting from hlsl source to either spirv or glsl is supported for now
   static bool convert(const GFXShaderConvertOptions& options, GFXShaderConvertOutput& output);
};

#endif
