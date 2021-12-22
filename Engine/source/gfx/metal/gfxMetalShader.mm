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

#include "platform/platform.h"
#include "gfx/metal/gfxMetalShader.h"
#include "core/frameAllocator.h"
#include "core/stream/fileStream.h"
#include "core/util/safeDelete.h"
#include "console/console.h"

//-----------------------------------------------------------------------------
// GFXMetalConstBufferLayout
//-----------------------------------------------------------------------------

bool GFXMetalConstBufferLayout::set(const ParamDesc& pd, const GFXShaderConstType constType, const U32 inSize, const void* data, U8* basePointer)
{
   PROFILE_SCOPE(GFXMetalConstBufferLayout_set);
   S32 size = inSize;
   // Shader compilers like to optimize float4x4 uniforms into float3x3s.
   // So long as the real paramater is a matrix of-some-type and the data
   // passed in is a MatrixF ( which is will be ), we DO NOT have a
   // mismatched const type.
   AssertFatal(pd.constType == constType ||
      (
         (pd.constType == GFXSCT_Float2x2 ||
            pd.constType == GFXSCT_Float3x3 ||
            pd.constType == GFXSCT_Float4x3 ||
            pd.constType == GFXSCT_Float4x4) &&
         (constType == GFXSCT_Float2x2 ||
            constType == GFXSCT_Float3x3 ||
            constType == GFXSCT_Float4x3 ||
            constType == GFXSCT_Float4x4)
         ), "Mismatched const type!");

   // This "cute" bit of code allows us to support 2x3 and 3x3 matrices in shader constants but use our MatrixF class.  Yes, a hack. -BTR
   switch (pd.constType)
   {
   case GFXSCT_Float2x2:
   case GFXSCT_Float3x3:
   case GFXSCT_Float4x3:
   case GFXSCT_Float4x4:
      return setMatrix(pd, constType, size, data, basePointer);
      break;
      // TODO add other AlignedVector here
   case GFXSCT_Float2:
      if (size > sizeof(Point2F))
         size = pd.size;
   default:
      break;
   }

   AssertFatal(pd.size >= size, "Not enough room in the buffer for this data!");

   // Ok, we only set data if it's different than the data we already have, this maybe more expensive than just setting the data, but 
   // we'll have to do some timings to see.  For example, the lighting shader constants rarely change, but we can't assume that at the
   // renderInstMgr level, but we can check down here. -BTR
   if (dMemcmp(basePointer + pd.offset, data, size) != 0)
   {
      dMemcpy(basePointer + pd.offset, data, size);
      return true;
   }
   return false;
}

bool GFXMetalConstBufferLayout::setMatrix(const ParamDesc& pd, const GFXShaderConstType constType, const U32 size, const void* data, U8* basePointer)
{
   PROFILE_SCOPE(GFXMetalConstBufferLayout_setMatrix);

   if (pd.constType == GFXSCT_Float4x4)
   {
      // Special case, we can just blast this guy.
      AssertFatal(pd.size >= size, "Not enough room in the buffer for this data!");
      if (dMemcmp(basePointer + pd.offset, data, size) != 0)
      {
         dMemcpy(basePointer + pd.offset, data, size);
         return true;
      }

      return false;
   }
   else
   {
      PROFILE_SCOPE(GFXMetalConstBufferLayout_setMatrix_not4x4);

      // Figure out how big of a chunk we are copying.  We're going to copy 4 columns by N rows of data
      U32 csize;
      switch (pd.constType)
      {
      case GFXSCT_Float2x2:
         csize = 24; //this takes up 16+8
         break;
      case GFXSCT_Float3x3:
         csize = 44; //This takes up 16+16+12
         break;
      case GFXSCT_Float4x3:
         csize = 48;
         break;
      default:
         AssertFatal(false, "Unhandled case!");
         return false;
         break;
      }

      // Loop through and copy 
      bool ret = false;
      U8* currDestPointer = basePointer + pd.offset;
      const U8* currSourcePointer = static_cast<const U8*>(data);
      const U8* endData = currSourcePointer + size;
      while (currSourcePointer < endData)
      {
         if (dMemcmp(currDestPointer, currSourcePointer, csize) != 0)
         {
            dMemcpy(currDestPointer, currSourcePointer, csize);
            ret = true;
         }
         else if (pd.constType == GFXSCT_Float4x3)
         {
            ret = true;
         }

         currDestPointer += csize;
         currSourcePointer += sizeof(MatrixF);
      }

      return ret;
   }
}

//-----------------------------------------------------------------------------
// GFXMetalShaderConstHandle
//-----------------------------------------------------------------------------

GFXMetalShaderConstHandle::GFXMetalShaderConstHandle()
{
   clear();
}

const String& GFXMetalShaderConstHandle::getName() const
{
   if ( mVertexConstant )
      return mVertexHandle.name;
   else
      return mPixelHandle.name;
}

GFXShaderConstType GFXMetalShaderConstHandle::getType() const
{
   if ( mVertexConstant )
      return mVertexHandle.constType;
   else
      return mPixelHandle.constType;
}

U32 GFXMetalShaderConstHandle::getArraySize() const
{
   if ( mVertexConstant )
      return mVertexHandle.arraySize;
   else
      return mPixelHandle.arraySize;
}

S32 GFXMetalShaderConstHandle::getSamplerRegister() const
{
   if ( !mValid || !isSampler() )
      return -1;

   // We always store sampler type and register index in the pixelHandle,
   // sampler registers are shared between vertex and pixel shaders anyway.

   return mPixelHandle.offset;   
}

//-----------------------------------------------------------------------------
// GFXMetalShaderConstBuffer
//-----------------------------------------------------------------------------

GFXMetalShaderConstBuffer::GFXMetalShaderConstBuffer( GFXMetalShader* shader, 
                                                      GFXMetalConstBufferLayout* vertexLayout,
                                                      GFXMetalConstBufferLayout* pixelLayout)
{
   AssertFatal( shader, "GFXMetalShaderConstBuffer() - Got a null shader!" );

   // We hold on to this so we don't have to call
   // this virtual method during activation.
   mShader = shader;

   mConstantBufferV = NULL;
   mConstantBufferP = NULL;

   //Mandatory
   mVertexConstBufferLayout = vertexLayout;
   mVertexConstBuffer = new GenericConstBuffer(vertexLayout);
    
   mPixelConstBufferLayout = pixelLayout;
   mPixelConstBuffer = new GenericConstBuffer(pixelLayout);

   _createBuffers();	
}

GFXMetalShaderConstBuffer::~GFXMetalShaderConstBuffer()
{   
   // release constant buffers
   SAFE_RELEASE(mConstantBufferP);
   SAFE_RELEASE(mConstantBufferV);
   SAFE_DELETE(mVertexConstBuffer);
   SAFE_DELETE(mPixelConstBuffer);

   if ( mShader )
      mShader->_unlinkBuffer( this );
}

void GFXMetalShaderConstBuffer::_createBuffers()
{
   // Create a vertex constant buffer
   if (mVertexConstBufferLayout->getBufferSize())
   {

   }

   // Create a pixel constant buffer
   if (mPixelConstBufferLayout->getBufferSize())
   {

   }
}

GFXShader* GFXMetalShaderConstBuffer::getShader()
{
   return mShader;
}

// This is kind of cheesy, but I don't think templates would work well here because 
// these functions potentially need to be handled differently by other derived types
template<class T>
inline void GFXMetalShaderConstBuffer::SET_CONSTANT(  GFXShaderConstHandle* handle, const T& fv,
                                                      GenericConstBuffer *vBuffer, GenericConstBuffer *pBuffer )
{
   AssertFatal(static_cast<const GFXMetalShaderConstHandle*>(handle), "Incorrect const buffer type!");
   const GFXMetalShaderConstHandle* h = static_cast<const GFXMetalShaderConstHandle*>(handle);
   AssertFatal(h, "Handle is NULL!" );
   AssertFatal(h->isValid(), "Handle is not valid!" );
   AssertFatal(!h->isSampler(), "Handle is sampler constant!" );
   AssertFatal(!mShader.isNull(), "Buffer's shader is null!" );
   AssertFatal(!h->mShader.isNull(), "Handle's shader is null!" );
   AssertFatal(h->mShader.getPointer() == mShader.getPointer(), "Mismatched shaders!");

   if ( h->mInstancingConstant )
   {
      dMemcpy( mInstPtr+h->mPixelHandle.offset, &fv, sizeof( fv ) );
      return;
   }
   if (h->mVertexConstant)
      vBuffer->set(h->mVertexHandle, fv);
   if (h->mPixelConstant)
      pBuffer->set(h->mPixelHandle, fv);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const F32 fv) 
{
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point2F& fv) 
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point3F& fv) 
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point4F& fv) 
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const PlaneF& fv) 
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const LinearColorF& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const S32 f)
{ 
   // This is the only type that is allowed to be used
   // with a sampler shader constant type, but it is only
   // allowed to be set from GLSL.
   //
   // So we ignore it here... all other cases will assert.
   //
   if ( ((GFXMetalShaderConstHandle*)handle)->isSampler() )
      return;

   SET_CONSTANT(handle, f, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point2I& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point3I& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point4I& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<F32>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point2F>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point3F>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point4F>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<S32>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point2I>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point3I>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point4I>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mPixelConstBuffer);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const MatrixF& mat, const GFXShaderConstType matrixType) 
{    
   AssertFatal(handle, "Handle is NULL!" );
   AssertFatal(handle->isValid(), "Handle is not valid!" );

   AssertFatal(static_cast<const GFXMetalShaderConstHandle*>(handle), "Incorrect const buffer type!"); 
   const GFXMetalShaderConstHandle* h = static_cast<const GFXMetalShaderConstHandle*>(handle); 
   AssertFatal(!h->isSampler(), "Handle is sampler constant!" );
   AssertFatal(h->mShader == mShader, "Mismatched shaders!"); 

   MatrixF transposed;
   if (matrixType == GFXSCT_Float4x3)
   {
      transposed = mat;
   }
   else
   {
      mat.transposeTo(transposed);
   }

   if (h->mInstancingConstant) 
   {
      if ( matrixType == GFXSCT_Float4x4 )
         dMemcpy( mInstPtr+h->mPixelHandle.offset, mat, sizeof( mat ) );
         
      // TODO: Support 3x3 and 2x2 matricies?      
      return;
   }

   if (h->mVertexConstant) 
      mVertexConstBuffer->set(h->mVertexHandle, transposed, matrixType); 
   if (h->mPixelConstant) 
      mPixelConstBuffer->set(h->mPixelHandle, transposed, matrixType);
}

void GFXMetalShaderConstBuffer::set(GFXShaderConstHandle* handle, const MatrixF* mat, const U32 arraySize, const GFXShaderConstType matrixType)
{
   AssertFatal(handle, "Handle is NULL!" );
   AssertFatal(handle->isValid(), "Handle is not valid!" );

   AssertFatal(static_cast<const GFXMetalShaderConstHandle*>(handle), "Incorrect const buffer type!"); 
   const GFXMetalShaderConstHandle* h = static_cast<const GFXMetalShaderConstHandle*>(handle); 
   AssertFatal(!h->isSampler(), "Handle is sampler constant!" );
   AssertFatal(h->mShader == mShader, "Mismatched shaders!"); 

   static Vector<MatrixF> transposed;
   if (arraySize > transposed.size())
      transposed.setSize(arraySize);

   if (matrixType == GFXSCT_Float4x3)
   {
      dMemcpy(transposed.address(), mat, arraySize * sizeof(MatrixF));
   }
   else
   {
      for (U32 i = 0; i < arraySize; i++)
         mat[i].transposeTo(transposed[i]);
   }

   // TODO: Maybe support this in the future?
   if (h->mInstancingConstant) 
      return;

   if (h->mVertexConstant) 
      mVertexConstBuffer->set(h->mVertexHandle, transposed.begin(), arraySize, matrixType);
   if (h->mPixelConstant) 
      mPixelConstBuffer->set(h->mPixelHandle, transposed.begin(), arraySize, matrixType);
}

const String GFXMetalShaderConstBuffer::describeSelf() const
{
   String ret;
   ret = String("   GFXMetalShaderConstBuffer\n");

   for (U32 i = 0; i < mVertexConstBufferLayout->getParameterCount(); i++)
   {
      GenericConstBufferLayout::ParamDesc pd;
      mVertexConstBufferLayout->getDesc(i, pd);

      ret += String::ToString("      Constant name: %s", pd.name.c_str());
   }

   return ret;
}

void GFXMetalShaderConstBuffer::zombify()
{
}

void GFXMetalShaderConstBuffer::resurrect()
{
}

bool GFXMetalShaderConstBuffer::isDirty()
{
   bool ret = mVertexConstBuffer->isDirty();
   ret |= mPixelConstBuffer->isDirty();
 
   return ret;
}

void GFXMetalShaderConstBuffer::activate( GFXMetalShaderConstBuffer *prevShaderBuffer )
{
   PROFILE_SCOPE(GFXMetalShaderConstBuffer_activate);

   // Clear the lost state.
   mWasLost = false;
}

void GFXMetalShaderConstBuffer::onShaderReload( GFXMetalShader *shader )
{
   AssertFatal( shader == mShader, "GFXMetalShaderConstBuffer::onShaderReload is hosed!" );

   SAFE_RELEASE(mConstantBufferP);
   SAFE_RELEASE(mConstantBufferV);

   SAFE_DELETE( mVertexConstBuffer );
   SAFE_DELETE( mPixelConstBuffer );
        
   AssertFatal( mVertexConstBufferLayout == shader->mVertexConstBufferLayout, "GFXMetalShaderConstBuffer::onShaderReload is hosed!" );
   AssertFatal( mPixelConstBufferLayout == shader->mPixelConstBufferLayout, "GFXMetalShaderConstBuffer::onShaderReload is hosed!" );

   mVertexConstBuffer = new GenericConstBuffer( mVertexConstBufferLayout );      
   mPixelConstBuffer = new GenericConstBuffer( mPixelConstBufferLayout ); 
  
   _createBuffers();
	
   // Set the lost state.
   mWasLost = true;
}

//-----------------------------------------------------------------------------
// GFXMetalShader
//-----------------------------------------------------------------------------

GFXMetalShader::GFXMetalShader()
{
   VECTOR_SET_ASSOCIATION( mShaderConsts );
   mVertexConstBufferLayout = NULL;
   mPixelConstBufferLayout = NULL;
}

GFXMetalShader::~GFXMetalShader()
{
   for (HandleMap::Iterator i = mHandles.begin(); i != mHandles.end(); i++)
      delete i->value;

   // delete const buffer layouts
   SAFE_DELETE(mVertexConstBufferLayout);
   SAFE_DELETE(mPixelConstBufferLayout);
}

bool GFXMetalShader::_init()
{
   PROFILE_SCOPE( GFXMetalShader_Init );

   // // Create the macro array including the system wide macros.
   // const U32 macroCount = smGlobalMacros.size() + mMacros.size() + 2;
   // FrameTemp<D3D_SHADER_MACRO> d3dMacros( macroCount );

   // for ( U32 i=0; i < smGlobalMacros.size(); i++ )
   // {
   //    d3dMacros[i].Name = smGlobalMacros[i].name.c_str();
   //    d3dMacros[i].Definition = smGlobalMacros[i].value.c_str();
   // }

   // for ( U32 i=0; i < mMacros.size(); i++ )
   // {
   //    d3dMacros[i+smGlobalMacros.size()].Name = mMacros[i].name.c_str();
   //    d3dMacros[i+smGlobalMacros.size()].Definition = mMacros[i].value.c_str();
   // }

   // d3dMacros[macroCount - 2].Name = "TORQUE_SM";
   // d3dMacros[macroCount - 2].Definition = D3D11->getShaderModelStr().c_str();

   // memset(&d3dMacros[macroCount - 1], 0, sizeof(D3D_SHADER_MACRO));

   if ( !mVertexConstBufferLayout )
      mVertexConstBufferLayout = new GFXD3D11ConstBufferLayout();
   else
      mVertexConstBufferLayout->clear();

   if ( !mPixelConstBufferLayout )
      mPixelConstBufferLayout = new GFXD3D11ConstBufferLayout();
   else
      mPixelConstBufferLayout->clear(); 

   
   mSamplerDescriptions.clear();
   mShaderConsts.clear();

   String vertTarget = D3D11->getVertexShaderTarget();
   String pixTarget = D3D11->getPixelShaderTarget();


   if (!mVertexFile.isEmpty() && !_compileShader( mVertexFile, vertTarget, d3dMacros, mVertexConstBufferLayout, mSamplerDescriptions ) )
      return false;

   if (!mPixelFile.isEmpty() && !_compileShader( mPixelFile, pixTarget, d3dMacros, mPixelConstBufferLayout, mSamplerDescriptions ) )
      return false;

   // Existing handles are resored to an uninitialized state.
   // Those that are found when parsing the layout parameters
   // will then be re-initialized.
   HandleMap::Iterator iter = mHandles.begin();
   for ( ; iter != mHandles.end(); iter++ )        
      (iter->value)->clear();      

   _buildShaderConstantHandles(mVertexConstBufferLayout, true);
   _buildShaderConstantHandles(mPixelConstBufferLayout, false);

   _buildSamplerShaderConstantHandles( mSamplerDescriptions );
   _buildInstancingShaderConstantHandles();

   // Notify any existing buffers that the buffer 
   // layouts have changed and they need to update.
   Vector<GFXShaderConstBuffer*>::iterator biter = mActiveBuffers.begin();
   for ( ; biter != mActiveBuffers.end(); biter++ )
      ((GFXMetalShaderConstBuffer*)(*biter))->onShaderReload( this );

   return true;
}

bool GFXMetalShader::_compileShader( const Torque::Path &filePath, 
                                    const String& target,                                  
                                    //const D3D_SHADER_MACRO *defines, 
                                    GenericConstBufferLayout* bufferLayout,
                                    Vector<GFXShaderConstDesc> &samplerDescriptions )
{
   PROFILE_SCOPE( GFXMetalShader_CompileShader );
   return true;
}

bool GFXMetalShader::_convertShaderVariable(const D3D11_SHADER_TYPE_DESC &typeDesc, GFXShaderConstDesc &desc)
{
   switch (typeDesc.Type)
   {
   case D3D_SVT_INT:
   {
      switch (typeDesc.Class)
      {
      case D3D_SVC_SCALAR:
         desc.constType = GFXSCT_Int;
         break;
      case D3D_SVC_VECTOR:
      {
         switch (typeDesc.Columns)
         {
         case 1:
            desc.constType = GFXSCT_Int;
            break;
         case 2:
            desc.constType = GFXSCT_Int2;
            break;
         case 3:
            desc.constType = GFXSCT_Int3;
            break;
         case 4:
            desc.constType = GFXSCT_Int4;
            break;
         }
      }
      break;
      }
      break;
   }   
   case D3D_SVT_FLOAT:
   {
      switch (typeDesc.Class)
      {
      case D3D_SVC_SCALAR:
         desc.constType = GFXSCT_Float;
         break;
      case D3D_SVC_VECTOR:
      {
         switch (typeDesc.Columns)
         {
         case 1:
            desc.constType = GFXSCT_Float;
            break;
         case 2:
            desc.constType = GFXSCT_Float2;
            break;
         case 3:
            desc.constType = GFXSCT_Float3;
            break;
         case 4:
            desc.constType = GFXSCT_Float4;
            break;
         }
      }
      break;
      case D3D_SVC_MATRIX_ROWS:
      case D3D_SVC_MATRIX_COLUMNS:
      {
         switch (typeDesc.Rows)
         {
         case 3:
            desc.constType = typeDesc.Columns == 4 ? GFXSCT_Float3x4 : GFXSCT_Float3x3;
            break;
         case 4:
            desc.constType = typeDesc.Columns == 3 ? GFXSCT_Float4x3 : GFXSCT_Float4x4;
            break;
         }
      }
      break;
      case D3D_SVC_OBJECT:
      case D3D_SVC_STRUCT:
         return false;
      }
   }
   break;

   default:
      AssertFatal(false, "Unknown shader constant class enum");
      break;
   }

   return true;
}


void GFXMetalShader::_buildShaderConstantHandles(GenericConstBufferLayout* layout, bool vertexConst)
{                     
   for (U32 i = 0; i < layout->getParameterCount(); i++)
   {
      GenericConstBufferLayout::ParamDesc pd;
      layout->getDesc(i, pd);

      GFXMetalShaderConstHandle* handle;
      HandleMap::Iterator j = mHandles.find(pd.name);

      if (j != mHandles.end())
      {
         handle = j->value;         
         handle->mShader = this;
         handle->setValid( true );
      } 
      else
      {
         handle = new GFXMetalShaderConstHandle();
         handle->mShader = this;
         mHandles[pd.name] = handle;
         handle->setValid( true );
      }

      if (vertexConst)
      {
         handle->mVertexConstant = true;
         handle->mVertexHandle = pd;
      }
      else
      {
         handle->mPixelConstant = true;
         handle->mPixelHandle = pd;
      }
   }
}

void GFXMetalShader::_buildSamplerShaderConstantHandles( Vector<GFXShaderConstDesc> &samplerDescriptions )
{                     
   Vector<GFXShaderConstDesc>::iterator iter = samplerDescriptions.begin();
   for ( ; iter != samplerDescriptions.end(); iter++ )
   {
      const GFXShaderConstDesc &desc = *iter;

      AssertFatal(   desc.constType == GFXSCT_Sampler || 
                     desc.constType == GFXSCT_SamplerCube || 
                     desc.constType == GFXSCT_SamplerCubeArray ||
                     desc.constType == GFXSCT_SamplerTextureArray,
                     "GFXMetalShader::_buildSamplerShaderConstantHandles - Invalid samplerDescription type!" );

      GFXMetalShaderConstHandle *handle;
      HandleMap::Iterator j = mHandles.find(desc.name);

      if ( j != mHandles.end() )
         handle = j->value;
      else
      {
         handle = new GFXMetalShaderConstHandle();
         mHandles[desc.name] = handle;         
      }

      handle->mShader = this;         
      handle->setValid( true );         
      handle->mPixelConstant = true;
      handle->mPixelHandle.name = desc.name;
      handle->mPixelHandle.constType = desc.constType;
      handle->mPixelHandle.offset = desc.arraySize;         
   }
}

void GFXMetalShader::_buildInstancingShaderConstantHandles()
{
   // If we have no instancing than just return
   if (!mInstancingFormat)
      return;

   U32 offset = 0;
   for ( U32 i=0; i < mInstancingFormat->getElementCount(); i++ )
   {
      const GFXVertexElement &element = mInstancingFormat->getElement( i );
      
      String constName = String::ToString( "$%s", element.getSemantic().c_str() );

      GFXMetalShaderConstHandle *handle;
      HandleMap::Iterator j = mHandles.find( constName );

      if ( j != mHandles.end() )
         handle = j->value; 
      else
      {
         handle = new GFXMetalShaderConstHandle();
         mHandles[ constName ] = handle;         
      }

      handle->mShader = this;
      handle->setValid( true );         
      handle->mInstancingConstant = true;

      // We shouldn't have an instancing constant that is also 
      // a vertex or pixel constant!  This means the shader features
      // are confused as to what is instanced.
      //
      AssertFatal(   !handle->mVertexConstant &&
                     !handle->mPixelConstant,
                     "GFXMetalShader::_buildInstancingShaderConstantHandles - Bad instanced constant!" );

      // HACK:  The GFXMetalShaderConstHandle will check mVertexConstant then
      // fall back to reading the mPixelHandle values.  We depend on this here
      // and store the data we need in the mPixelHandle constant although its
      // not a pixel shader constant.
      //
      handle->mPixelHandle.name = constName;
      handle->mPixelHandle.offset = offset;

      // If this is a matrix we will have 2 or 3 more of these
      // semantics with the same name after it.
      for ( ; i < mInstancingFormat->getElementCount(); i++ )
      {
         const GFXVertexElement &nextElement = mInstancingFormat->getElement( i );
         if ( nextElement.getSemantic() != element.getSemantic() )
         {
            i--;
            break;
         }
         offset += nextElement.getSizeInBytes();
      }
   }
}

GFXShaderConstBufferRef GFXMetalShader::allocConstBuffer()
{
   if (mVertexConstBufferLayout && mPixelConstBufferLayout)
   {
      GFXMetalShaderConstBuffer* buffer = new GFXMetalShaderConstBuffer(this, mVertexConstBufferLayout, mPixelConstBufferLayout);
      mActiveBuffers.push_back( buffer );
      buffer->registerResourceWithDevice(getOwningDevice());
      return buffer;
   } 

   return NULL;
}

/// Returns a shader constant handle for name, if the variable doesn't exist NULL is returned.
GFXShaderConstHandle* GFXMetalShader::getShaderConstHandle(const String& name)
{
   HandleMap::Iterator i = mHandles.find(name);   
   if ( i != mHandles.end() )
   {
      return i->value;
   } 
   else 
   {     
      GFXMetalShaderConstHandle *handle = new GFXMetalShaderConstHandle();
      handle->setValid( false );
      handle->mShader = this;
      mHandles[name] = handle;

      return handle;      
   }      
}

GFXShaderConstHandle* GFXMetalShader::findShaderConstHandle(const String& name)
{
   HandleMap::Iterator i = mHandles.find(name);
   if(i != mHandles.end())
      return i->value;
   else
   {
      return NULL;
   }
}

const Vector<GFXShaderConstDesc>& GFXMetalShader::getShaderConstDesc() const
{
   return mShaderConsts;
}

U32 GFXMetalShader::getAlignmentValue(const GFXShaderConstType constType) const
{   
   const U32 mRowSizeF = 16;
   const U32 mRowSizeI = 16;

   switch (constType)
   {
      case GFXSCT_Float :
      case GFXSCT_Float2 :
      case GFXSCT_Float3 : 
      case GFXSCT_Float4 :
         return mRowSizeF;
         break;
         // Matrices
      case GFXSCT_Float2x2 :
         return mRowSizeF * 2;
         break;
      case GFXSCT_Float3x3 : 
         return mRowSizeF * 3;
         break;
      case GFXSCT_Float4x3:
         return mRowSizeF * 3;
         break;
      case GFXSCT_Float4x4 :
         return mRowSizeF * 4;
         break;   
      //// Scalar
      case GFXSCT_Int :
      case GFXSCT_Int2 :
      case GFXSCT_Int3 : 
      case GFXSCT_Int4 :
         return mRowSizeI;
         break;
      default:
         AssertFatal(false, "Unsupported type!");
         return 0;
         break;
   }
}

void GFXMetalShader::zombify()
{
   // Shaders don't need zombification
}

void GFXMetalShader::resurrect()
{
   // Shaders are never zombies, and therefore don't have to be brought back.
}
