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

#ifndef _GFXGLSHADER_H_
#define _GFXGLSHADER_H_

#include "core/util/refBase.h"
#include "gfx/gfxShader.h"
#include "gfx/gl/tGL/tGL.h"
#include "gfx/genericConstBuffer.h"
#include "core/util/tSignal.h"
#include "core/util/tDictionary.h"

//forward decalre
class GFXGLDevice;
class GFXGLShader;

//-----------------------------------------------------------------------------
// GFXGLConstBufferLayout
//-----------------------------------------------------------------------------

class GFXGLConstBufferLayout : public GenericConstBufferLayout
{
public:
   GFXGLConstBufferLayout() {}

   /// We need to manually set the size due to alignment
   void setSize(U32 size) { mBufferSize = size; }

   /// Set a parameter, given a base pointer
   virtual bool set(const ParamDesc& pd, const GFXShaderConstType constType, const U32 size, const void* data, U8* basePointer);

protected:
   /// Set a matrix, given a base pointer
   virtual bool setMatrix(const ParamDesc& pd, const GFXShaderConstType constType, const U32 size, const void* data, U8* basePointer);
};

//-----------------------------------------------------------------------------
// GFXGLShaderConstHandle
//-----------------------------------------------------------------------------

class GFXGLShaderConstHandle : public GFXShaderConstHandle
{
public:

   GFXGLShaderConstHandle();
   // GFXShaderConstHandle
   const String& getName() const;
   GFXShaderConstType getType() const;
   U32 getArraySize() const;

   WeakRefPtr<GFXGLShader> mShader;

   bool mVertexConstant;
   GenericConstBufferLayout::ParamDesc mVertexHandle;
   bool mFragConstant;
   GenericConstBufferLayout::ParamDesc mFragHandle;

   /// Is true if this constant is for hardware mesh instancing.
   ///
   /// Note: We currently store its settings in mFragHandle.
   ///
   bool mInstancingConstant;

   void setValid(bool valid) { mValid = valid; }
   S32 getSamplerRegister() const;

   // Returns true if this is a handle to a sampler register.
   bool isSampler() const
   {
      return (mFragConstant && mFragHandle.constType >= GFXSCT_Sampler) || (mVertexConstant && mVertexHandle.constType >= GFXSCT_Sampler);
   }

   /// Restore to uninitialized state.
   void clear()
   {
      mShader = NULL;
      mVertexConstant = false;
      mFragConstant = false;
      mInstancingConstant = false;
      mVertexHandle.clear();
      mFragHandle.clear();
      mValid = false;
   }   
};

//-----------------------------------------------------------------------------
// GFXGLShaderConstBuffer
//-----------------------------------------------------------------------------

class GFXGLShaderConstBuffer : public GFXShaderConstBuffer
{
public:
   GFXGLShaderConstBuffer(GFXGLShader* shader,
      GFXGLConstBufferLayout* vertexLayout,
      GFXGLConstBufferLayout* fragLayout);

   virtual ~GFXGLShaderConstBuffer();

   /// Called by GFXGLDevice to activate this buffer.
   void activate(GFXGLShaderConstBuffer* prevShaderBuffer);

   /// Called when the shader this buffer references is reloaded.
   void onShaderReload(GFXGLShader* shader);

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
   virtual void zombify() {}
   virtual void resurrect() {}

private:

   void _createBuffers();
   bool isDirty();

   template<class T>
   inline void SET_CONSTANT(GFXShaderConstHandle* handle,
      const T& fv,
      GenericConstBuffer* vBuffer,
      GenericConstBuffer* fBuffer);

   friend class GFXGLShader;

   WeakRefPtr<GFXGLShader> mShader;

   //vertex
   GFXGLConstBufferLayout* mVertexConstBufferLayout;
   GenericConstBuffer* mVertexConstBuffer;
   //pixel
   GFXGLConstBufferLayout* mFragConstBufferLayout;
   GenericConstBuffer* mFragConstBuffer;

   //uniform buffers
   GLuint mVertexUBO;
   GLuint mFragUBO;
};

//-----------------------------------------------------------------------------
// GFXGLShader
//-----------------------------------------------------------------------------

class GFXGLShader : public GFXShader
{
   typedef Map<String, GFXGLShaderConstHandle*> HandleMap;
   friend class GFXGLShaderConstBuffer;

public:
   GFXGLShader(GFXGLDevice* device);
   virtual ~GFXGLShader();

   /// @name GFXShader interface
   /// @{
   virtual GFXShaderConstHandle* getShaderConstHandle(const String& name);
   virtual GFXShaderConstHandle* findShaderConstHandle(const String& name);

   /// Returns our list of shader constants, the material can get this and just set the constants it knows about
   virtual const Vector<GFXShaderConstDesc>& getShaderConstDesc() const;

   /// Returns the alignment value for constType
   virtual U32 getAlignmentValue(const GFXShaderConstType constType) const;

   /// Returns the size (in bytes) for this constType
   U32 getConstantSize(const GFXShaderConstType constType) const;

   virtual GFXShaderConstBufferRef allocConstBuffer();

   /// @}
   
   /// @name GFXResource interface
   /// @{
   virtual void zombify();
   virtual void resurrect();
   /// @}      

   /// Activates this shader in the GL context.
   void bindProgramPipeline();
   
protected:
  
   virtual bool _init();   

   bool _compileShader(  const Torque::Path &file, 
                     bool isVertex, 
                     const Vector<GFXShaderMacro> &macros );

   void clearShaders();

   void _getShaderConstants(GLuint program, GFXGLConstBufferLayout* bufferLayout, Vector<GFXShaderConstDesc>& samplerDescriptions);

   void _buildShaderConstantHandles(GenericConstBufferLayout* layout, bool vertexConst);
   void _buildSamplerShaderConstantHandles(Vector<GFXShaderConstDesc>& samplerDescriptions);
   void _buildInstancingShaderConstantHandles();

   /// @name Internal GL handles
   /// @{
   GLuint mProgramPipeline;
   GLuint mVertexProgram;
   GLuint mFragmentProgram;
   /// @}

   GFXGLConstBufferLayout* mVertexConstBufferLayout;
   GFXGLConstBufferLayout* mFragConstBufferLayout;
    
   HandleMap mHandles;
   Vector<GFXShaderConstDesc> mSamplerDescriptions;
   Vector<GFXShaderConstDesc> mShaderConsts;

   GFXGLDevice* mDevice;

};

#endif // _GFXGLSHADER_H_
