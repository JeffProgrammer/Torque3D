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

#ifndef _GFXSTRUCTS_H_
#define _GFXSTRUCTS_H_

#ifndef _COLOR_H_
#include "core/color.h"
#endif
#ifndef _GFXVERTEXCOLOR_H_
#include "gfx/gfxVertexColor.h"
#endif
#ifndef _GFXENUMS_H_
#include "gfx/gfxEnums.h"
#endif
#ifndef _MMATH_H_
#include "math/mMath.h"
#endif
#ifndef _PROFILER_H_
#include "platform/profiler.h"
#endif
#ifndef _GFXRESOURCE_H_
#include "gfx/gfxResource.h"
#endif
#ifndef _REFBASE_H_
#include "core/util/refBase.h"
#endif
#ifndef _GFXVERTEXTYPES_H_
#include "gfx/gfxVertexTypes.h"
#endif

//-----------------------------------------------------------------------------

class GFXTextureObject;

struct GFXVideoMode 
{
   GFXVideoMode();

   Point2I resolution;
   U32 bitDepth;
   U32 refreshRate;
   bool fullScreen;
   bool wideScreen;
   // When this is returned from GFX, it's the max, otherwise it's the desired AA level.
   U32 antialiasLevel;

   inline bool operator == ( const GFXVideoMode &otherMode ) const 
   {
      if( otherMode.fullScreen != fullScreen )
         return false;
      if( otherMode.resolution != resolution )
         return false;
      if( otherMode.bitDepth != bitDepth )
         return false;
      if( otherMode.refreshRate != refreshRate )
         return false;
      if( otherMode.wideScreen != wideScreen )
         return false;
      if( otherMode.antialiasLevel != antialiasLevel)
         return false;

      return true;
   }
   
   inline bool operator !=( const GFXVideoMode& otherMode ) const
   {
      return !( *this == otherMode );
   }

   /// Fill whatever fields we can from the passed string, which should be 
   /// of form "width height [bitDepth [refreshRate] [antialiasLevel]]" Unspecified fields
   /// aren't modified, so you may want to set defaults before parsing.
   void parseFromString( const char *str );

   /// Gets a string representation of the object as
   /// "resolution.x resolution.y fullScreen bitDepth refreshRate antialiasLevel"
   ///
   /// \return (string) A string representation of the object.
   const String toString() const;
};

//-----------------------------------------------------------------------------

struct GFXPrimitive
{
   GFXPrimitiveType type;

   U32 startVertex;    /// offset into vertex buffer to change where vertex[0] is
   U32 minIndex;       /// minimal value we will see in the indices
   U32 startIndex;     /// start of indices in buffer
   U32 numPrimitives;  /// how many prims to render
   U32 numVertices;    /// how many vertices... (used for locking, we lock from minIndex to minIndex + numVertices)

   GFXPrimitive()
   {
      type = GFXPT_FIRST;
      startVertex = 0;
      minIndex = 0;
      startIndex = 0;
      numPrimitives = 0;
      numVertices = 0;
   }
};

/// Passed to GFX for shader defines.
struct GFXShaderMacro
{
   GFXShaderMacro() {}

   GFXShaderMacro( const GFXShaderMacro &macro )
      :  name( macro.name ), 
         value( macro.value ) 
      {}

   GFXShaderMacro(   const String &name_, 
                     const String &value_ = String::EmptyString )
      :  name( name_ ), 
         value( value_ ) 
      {}

   ~GFXShaderMacro() {}

   /// The macro name.
   String name;

   /// The optional macro value.
   String value;

   static void stringize( const Vector<GFXShaderMacro> &macros, String *outString );
};

struct GFXGraphicsPipelineHandle {};
struct GFXVertexBufferHandle2 {}; // remove 2 after we drop the old GFXVertexBufferHandle template class
struct GFXIndexBufferHandle {};
struct GFXConstantBufferHandle {};
struct GFXTextureHandle {};
struct GFXBindGroupLayoutHandle {};
struct GFXBindGroupHandle {};
struct GFXSamplerHandle {};

template<typename T>
struct GFXHandle
{
   operator U32() { return mHandle; }
   operator const U32() const { return mHandle; }

   GFXHandle() : mHandle(0) {}
   explicit GFXHandle(U32 handle) : mHandle(handle) {}

   GFXHandle(const GFXHandle<T>& handle) : mHandle(handle) {}

   GFXHandle<T>& operator=(U32 handle) { mHandle = handle; return *this; }
private:
   U32 mHandle;
};

#define GFXHandleNone 0

struct GFXVertexBufferDesc
{
   GFXBufferType bufferType = GFXBufferTypeStatic;
   U32 size = 0;
   void* data = NULL;
};

struct GFXIndexBufferDesc
{
   GFXBufferType bufferType = GFXBufferTypeStatic;
   U32 size = 0;
   void* data = NULL;
};

struct GFXConstBufferDesc
{
   GFXBufferType bufferType = GFXBufferTypeStatic;
   U32 size = 0;
   void* data = NULL;
};

struct GFXSamplerDesc2 // remove 2 after we drop old one
{
   GFXTextureAddressMode addressModeU = GFXAddressWrap;
   GFXTextureAddressMode addressModeV = GFXAddressWrap;
   GFXTextureAddressMode addressModeW = GFXAddressWrap;

   GFXTextureFilterType magFilter = GFXTextureFilterLinear;
   GFXTextureFilterType minFilter = GFXTextureFilterLinear;
   GFXTextureFilterType mipFilter = GFXTextureFilterLinear;

   GFXCmpFunc samplerFunc = GFXCmpNever;
   U32 maxAnisotropy = 1;
   F32 mipLODBias = 0.0;
};

struct GFXInputLayoutElementDesc
{
   String semanticName;
   U32 semanticIndex;
   GFXDeclType type;
};

struct GFXInputLayoutBufferDesc
{
   GFXInputLayoutElementDesc elements[GFX_MAX_INPUT_LAYOUT_ATTRIBUTES];
   U32 count = 0;
   U32 stride = 0;
   bool isInstancingBuffer = false;
};

struct GFXInputLayoutDesc
{
   GFXInputLayoutBufferDesc buffers[GFX_MAX_BOUND_VERTEX_BUFFERS];
   U32 count = 0;
};

struct GFXResourceDynamicConstantBufferLayoutDesc
{
   struct
   {
      String name;
      U32 slotNumber;
      U32 size;
   } resource[GFX_MAX_DYNAMIC_CONSTANT_BUFFERS];
   U32 count = 0;
};

struct GFXPipelineDesc
{
   GFXInputLayoutDesc inputLayout;
   GFXShader* shader; // TODO: Clean this up
   GFXResourceDynamicConstantBufferLayoutDesc dynamicConstBufferLayout;
   U32 pushConstantSize;
};

struct GFXTextureProfile;

struct GFXTextureDesc
{
   GFXTextureBindingType type;
   U32 width = 0;
   U32 height = 0;
   U32 depth = 0;
   U32 numberLayers = 0;
   GFXFormat format;
   GFXTextureProfile* profile;
   U32 numMipLevels = 1;
};

#endif // _GFXSTRUCTS_H_
