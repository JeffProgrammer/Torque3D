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

#ifndef _GFXMETALSHADER_H_
#define _GFXMETALSHADER_H_

#include "core/util/path.h"
#include "core/util/tDictionary.h"
#include "gfx/gfxShader.h"
#include "gfx/gfxResource.h"
#include "gfx/genericConstBuffer.h"
#include "gfx/metal/gfxMetalDevice.h"

class GFXMetalShader;

//-----------------------------------------------------------------------------
// GFXMetalConstBufferLayout
//-----------------------------------------------------------------------------

class GFXMetalConstBufferLayout : public GenericConstBufferLayout
{
public:
   GFXMetalConstBufferLayout() {}
   
   /// We need to manually set the size due to D3D11 alignment
   void setSize(U32 size){ mBufferSize = size;}

   /// Set a parameter, given a base pointer
   virtual bool set(const ParamDesc& pd, const GFXShaderConstType constType, const U32 size, const void* data, U8* basePointer);

protected:
   /// Set a matrix, given a base pointer
   virtual bool setMatrix(const ParamDesc& pd, const GFXShaderConstType constType, const U32 size, const void* data, U8* basePointer);

};

//-----------------------------------------------------------------------------
// GFXMetalShaderConstHandle
//-----------------------------------------------------------------------------

class GFXMetalShaderConstHandle : public GFXShaderConstHandle
{
public:   

   // GFXShaderConstHandle
   const String& getName() const;
   GFXShaderConstType getType() const;
   U32 getArraySize() const;

   WeakRefPtr<GFXMetalShader> mShader;

   bool mVertexConstant;
   GenericConstBufferLayout::ParamDesc mVertexHandle;
   bool mPixelConstant;
   GenericConstBufferLayout::ParamDesc mPixelHandle;
   
   /// Is true if this constant is for hardware mesh instancing.
   ///
   /// Note: We currently store its settings in mPixelHandle.
   ///
   bool mInstancingConstant;

   void setValid( bool valid ) { mValid = valid; }
   S32 getSamplerRegister() const;

   // Returns true if this is a handle to a sampler register.
   bool isSampler() const 
   {
      return ( mPixelConstant && mPixelHandle.constType >= GFXSCT_Sampler ) || ( mVertexConstant && mVertexHandle.constType >= GFXSCT_Sampler );
   }

   /// Restore to uninitialized state.
   void clear()
   {
      mShader = NULL;
      mVertexConstant = false;
      mPixelConstant = false;
      mInstancingConstant = false;
      mVertexHandle.clear();
      mPixelHandle.clear();
      mValid = false;
   }

   GFXMetalShaderConstHandle();
};

//-----------------------------------------------------------------------------
// GFXMetalShaderConstBuffer
//-----------------------------------------------------------------------------

class GFXMetalShaderConstBuffer : public GFXShaderConstBuffer
{
   friend class GFXMetalShader;
public:

   GFXMetalShaderConstBuffer(GFXMetalShader* shader,
      GFXMetalConstBufferLayout* vertexLayout,
      GFXMetalConstBufferLayout* pixelLayout);

   virtual ~GFXMetalShaderConstBuffer();

   /// Called by GFXMetalDevice to activate this buffer.
   /// @param mPrevShaderBuffer The previously active buffer
   void activate(GFXMetalShaderConstBuffer *prevShaderBuffer);

   /// Used internally by GFXMetalShaderConstBuffer to determine if it's dirty.
   bool isDirty();

   /// Called from GFXMetalShader when constants have changed and need
   /// to be the shader this buffer references is reloaded.
   void onShaderReload(GFXMetalShader *shader);

   // GFXShaderConstBuffer
   virtual GFXShader* getShader();
   virtual void set(GFXShaderConstHandle* handle, const F32 fv);
   virtual void set(GFXShaderConstHandle* handle, const Point2F& fv);
   virtual void set(GFXShaderConstHandle* handle, const Point3F& fv);
   virtual void set(GFXShaderConstHandle* handle, const Point4F& fv);
   virtual void set(GFXShaderConstHandle* handle, const PlaneF& fv);
   virtual void set(GFXShaderConstHandle* handle, const LinearColorF& fv);
   virtual void set(GFXShaderConstHandle* handle, const S32 f);
   virtual void set(GFXShaderConstHandle* handle, const Point2I& fv);
   virtual void set(GFXShaderConstHandle* handle, const Point3I& fv);
   virtual void set(GFXShaderConstHandle* handle, const Point4I& fv);
   virtual void set(GFXShaderConstHandle* handle, const AlignedArray<F32>& fv);
   virtual void set(GFXShaderConstHandle* handle, const AlignedArray<Point2F>& fv);
   virtual void set(GFXShaderConstHandle* handle, const AlignedArray<Point3F>& fv);
   virtual void set(GFXShaderConstHandle* handle, const AlignedArray<Point4F>& fv);
   virtual void set(GFXShaderConstHandle* handle, const AlignedArray<S32>& fv);
   virtual void set(GFXShaderConstHandle* handle, const AlignedArray<Point2I>& fv);
   virtual void set(GFXShaderConstHandle* handle, const AlignedArray<Point3I>& fv);
   virtual void set(GFXShaderConstHandle* handle, const AlignedArray<Point4I>& fv);
   virtual void set(GFXShaderConstHandle* handle, const MatrixF& mat, const GFXShaderConstType matType = GFXSCT_Float4x4);
   virtual void set(GFXShaderConstHandle* handle, const MatrixF* mat, const U32 arraySize, const GFXShaderConstType matrixType = GFXSCT_Float4x4);

   // GFXResource
   virtual const String describeSelf() const;
   virtual void zombify();
   virtual void resurrect();

protected:

   void _createBuffers();

   template<class T>
   inline void SET_CONSTANT(GFXShaderConstHandle* handle,
      const T& fv,
      GenericConstBuffer *vBuffer,
      GenericConstBuffer *pBuffer);

   // Constant buffers
   //ID3D11Buffer* mConstantBufferV;
   //ID3D11Buffer* mConstantBufferP;

   /// We keep a weak reference to the shader 
   /// because it will often be deleted.
   WeakRefPtr<GFXMetalShader> mShader;

   //vertex
   GFXMetalConstBufferLayout* mVertexConstBufferLayout;
   GenericConstBuffer* mVertexConstBuffer;
   //pixel
   GFXMetalConstBufferLayout* mPixelConstBufferLayout;
   GenericConstBuffer* mPixelConstBuffer;
};

//-----------------------------------------------------------------------------
// GFXMetalShader
//-----------------------------------------------------------------------------

class GFXMetalShader : public GFXShader
{
   friend class GFXMetalDevice;
   friend class GFXMetalShaderConstBuffer;

public:
   typedef Map<String, GFXMetalShaderConstHandle*> HandleMap;

   GFXMetalShader();
   virtual ~GFXMetalShader();   

   // GFXShader
   virtual GFXShaderConstBufferRef allocConstBuffer();
   virtual const Vector<GFXShaderConstDesc>& getShaderConstDesc() const;
   virtual GFXShaderConstHandle* getShaderConstHandle(const String& name); 
   virtual GFXShaderConstHandle* findShaderConstHandle(const String& name);
   virtual U32 getAlignmentValue(const GFXShaderConstType constType) const;
   virtual bool getDisassembly( String &outStr ) const;

   // GFXResource
   virtual void zombify();
   virtual void resurrect();

protected:

   virtual bool _init();   

   static const U32 smCompiledShaderTag;

   //ID3D11VertexShader *mVertShader;
   //ID3D11PixelShader *mPixShader;

   GFXMetalConstBufferLayout* mVertexConstBufferLayout;
   GFXMetalConstBufferLayout* mPixelConstBufferLayout;   

   HandleMap mHandles;

   /// The shader disassembly from DX when this shader is compiled.
   /// We only store this data in non-release builds.
   String mDissasembly;

   /// Vector of sampler type descriptions consolidated from _compileShader.
   Vector<GFXShaderConstDesc> mSamplerDescriptions;

   /// Vector of descriptions (consolidated for the getShaderConstDesc call)
   Vector<GFXShaderConstDesc> mShaderConsts;
   
   // These two functions are used when compiling shaders from hlsl
   virtual bool _compileShader( const Torque::Path &filePath, 
                                const String &target, 
                                //const D3D_SHADER_MACRO *defines, 
                                GenericConstBufferLayout *bufferLayout, 
                                Vector<GFXShaderConstDesc> &samplerDescriptions );

   //bool _convertShaderVariable(const D3D11_SHADER_TYPE_DESC &typeDesc, GFXShaderConstDesc &desc);
  
   // This is used in both cases
   void _buildShaderConstantHandles(GenericConstBufferLayout *layout, bool vertexConst);
   
   void _buildSamplerShaderConstantHandles( Vector<GFXShaderConstDesc> &samplerDescriptions );

   /// Used to build the instancing shader constants from 
   /// the instancing vertex format.
   void _buildInstancingShaderConstantHandles();
};

inline bool GFXMetalShader::getDisassembly(String &outStr) const
{
   outStr = mDissasembly;
   return (outStr.isNotEmpty());
}

#endif
