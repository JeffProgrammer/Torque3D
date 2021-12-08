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
#include "gfx/gl/gfxGLShader.h"
#include "gfx/gl/gfxGLDevice.h"
#include "gfx/util/gfxShaderConvert.h"
#include "core/frameAllocator.h"
#include "core/stream/fileStream.h"
#include "core/strings/stringFunctions.h"
#include "math/mPoint2.h"
#include "gfx/gfxStructs.h"
#include "console/console.h"

//we only use a max of single UBO per stage, it is always named _Global. These are the binding points and the name
static const GLuint GFX_GL_UBO_VERT_BIND = 0;
static const GLuint GFX_GL_UBO_FRAG_BIND = 1;
static const GLchar* GFX_GL_UBO_NAME = "_Global";

//-----------------------------------------------------------------------------
// GFXGLConstBufferLayout
//-----------------------------------------------------------------------------

bool GFXGLConstBufferLayout::set(const ParamDesc& pd, const GFXShaderConstType constType, const U32 inSize, const void* data, U8* basePointer)
{
   PROFILE_SCOPE(GFXGLConstBufferLayout_set);
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

bool GFXGLConstBufferLayout::setMatrix(const ParamDesc& pd, const GFXShaderConstType constType, const U32 size, const void* data, U8* basePointer)
{
   PROFILE_SCOPE(GFX3GLConstBufferLayout_setMatrix);

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
      PROFILE_SCOPE(GFXGLConstBufferLayout_setMatrix_not4x4);

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
// GFXGLShaderConstHandle
//-----------------------------------------------------------------------------

GFXGLShaderConstHandle::GFXGLShaderConstHandle()
{
   clear();
}

const String& GFXGLShaderConstHandle::getName() const
{
   if (mVertexConstant)
      return mVertexHandle.name;
   else
      return mFragHandle.name;
}

GFXShaderConstType GFXGLShaderConstHandle::getType() const
{
   if (mVertexConstant)
      return mVertexHandle.constType;
   else
      return mFragHandle.constType;
}

U32 GFXGLShaderConstHandle::getArraySize() const
{
   if (mVertexConstant)
      return mVertexHandle.arraySize;
   else
      return mFragHandle.arraySize;
}

S32 GFXGLShaderConstHandle::getSamplerRegister() const
{
   if (!mValid || !isSampler())
      return -1;

   // We always store sampler type and register index in the pixelHandle,
   // sampler registers are shared between vertex and pixel shaders anyway.

   return mFragHandle.offset;
}

//-----------------------------------------------------------------------------
// GFXGLShaderConstBuffer
//-----------------------------------------------------------------------------

GFXGLShaderConstBuffer::GFXGLShaderConstBuffer(GFXGLShader* shader,
   GFXGLConstBufferLayout* vertexLayout,
   GFXGLConstBufferLayout* fragLayout)
{
   mShader = shader;

   mVertexConstBufferLayout = vertexLayout;
   mVertexConstBuffer = new GenericConstBuffer(vertexLayout);

   mFragConstBufferLayout = fragLayout;
   mFragConstBuffer = new GenericConstBuffer(fragLayout);

   _createBuffers();
}

GFXGLShaderConstBuffer::~GFXGLShaderConstBuffer()
{
   if (mVertexUBO)
      glDeleteBuffers(1, &mVertexUBO);

   if (mFragUBO)
      glDeleteBuffers(1, &mFragUBO);

   SAFE_DELETE(mVertexConstBuffer);
   SAFE_DELETE(mFragConstBuffer);

   if ( mShader )
      mShader->_unlinkBuffer( this );
}

void GFXGLShaderConstBuffer::_createBuffers()
{
   const U32 vertSize = mVertexConstBufferLayout->getBufferSize();
   if (vertSize)
   {
      glGenBuffers(1, &mVertexUBO);

      //allocate memory for the buffer - TODO bufferStorage extension implementation
      glBindBuffer(GL_UNIFORM_BUFFER, mVertexUBO);
      glBufferData(GL_UNIFORM_BUFFER, vertSize, NULL, GL_DYNAMIC_DRAW);
      glBindBuffer(GL_UNIFORM_BUFFER, 0);
   }

   const U32 fragSize = mFragConstBufferLayout->getBufferSize();
   if (fragSize)
   {
      glGenBuffers(1, &mFragUBO);

      //allocate memory for the buffer - TODO bufferStorage extension implementation
      glBindBuffer(GL_UNIFORM_BUFFER, mFragUBO);
      glBufferData(GL_UNIFORM_BUFFER, fragSize, NULL, GL_DYNAMIC_DRAW);
      glBindBuffer(GL_UNIFORM_BUFFER, 0);
   }
}

GFXShader* GFXGLShaderConstBuffer::getShader()
{
   return mShader;
}

// This is kind of cheesy, but I don't think templates would work well here because 
// these functions potentially need to be handled differently by other derived types
template<class T>
inline void GFXGLShaderConstBuffer::SET_CONSTANT(GFXShaderConstHandle* handle, const T& fv,
   GenericConstBuffer* vBuffer, GenericConstBuffer* fBuffer)
{
   AssertFatal(static_cast<const GFXGLShaderConstHandle*>(handle), "Incorrect const buffer type!");
   const GFXGLShaderConstHandle* h = static_cast<const GFXGLShaderConstHandle*>(handle);
   AssertFatal(h, "Handle is NULL!");
   AssertFatal(h->isValid(), "Handle is not valid!");
   AssertFatal(!h->isSampler(), "Handle is sampler constant!");
   AssertFatal(!mShader.isNull(), "Buffer's shader is null!");
   AssertFatal(!h->mShader.isNull(), "Handle's shader is null!");
   AssertFatal(h->mShader.getPointer() == mShader.getPointer(), "Mismatched shaders!");

   if (h->mInstancingConstant)
   {
      dMemcpy(mInstPtr + h->mFragHandle.offset, &fv, sizeof(fv));
      return;
   }
   if (h->mVertexConstant)
      vBuffer->set(h->mVertexHandle, fv);
   if (h->mFragConstant)
      fBuffer->set(h->mFragHandle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const F32 fv) 
{
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point2F& fv) 
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point3F& fv) 
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point4F& fv) 
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const PlaneF& fv) 
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const LinearColorF& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const S32 f)
{ 
   // This is the only type that is allowed to be used
   // with a sampler shader constant type, but it is only
   // allowed to be set from GLSL.
   //
   // So we ignore it here... all other cases will assert.
   //
   if ( ((GFXGLShaderConstHandle*)handle)->isSampler() )
      return;

   SET_CONSTANT(handle, f, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point2I& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point3I& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point4I& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<F32>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point2F>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point3F>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point4F>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<S32>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point2I>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point3I>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point4I>& fv)
{ 
   SET_CONSTANT(handle, fv, mVertexConstBuffer, mFragConstBuffer);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const MatrixF& mat, const GFXShaderConstType matrixType) 
{    
   AssertFatal(handle, "Handle is NULL!" );
   AssertFatal(handle->isValid(), "Handle is not valid!" );

   AssertFatal(static_cast<const GFXGLShaderConstHandle*>(handle), "Incorrect const buffer type!"); 
   const GFXGLShaderConstHandle* h = static_cast<const GFXGLShaderConstHandle*>(handle); 
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
         dMemcpy( mInstPtr+h->mFragHandle.offset, mat, sizeof( mat ) );
         
      // TODO: Support 3x3 and 2x2 matricies?      
      return;
   }

   if (h->mVertexConstant) 
      mVertexConstBuffer->set(h->mVertexHandle, transposed, matrixType); 
   if (h->mFragConstant) 
      mFragConstBuffer->set(h->mFragHandle, transposed, matrixType);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const MatrixF* mat, const U32 arraySize, const GFXShaderConstType matrixType)
{
   AssertFatal(handle, "Handle is NULL!" );
   AssertFatal(handle->isValid(), "Handle is not valid!" );

   AssertFatal(static_cast<const GFXGLShaderConstHandle*>(handle), "Incorrect const buffer type!"); 
   const GFXGLShaderConstHandle* h = static_cast<const GFXGLShaderConstHandle*>(handle); 
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
   if (h->mFragConstant) 
      mFragConstBuffer->set(h->mFragHandle, transposed.begin(), arraySize, matrixType);
}

bool GFXGLShaderConstBuffer::isDirty()
{
   bool ret = mVertexConstBuffer->isDirty();
   ret |= mFragConstBuffer->isDirty();

   return ret;
}

void GFXGLShaderConstBuffer::activate(GFXGLShaderConstBuffer* prevShaderBuffer)
{
   PROFILE_SCOPE(GFXGLShaderConstBuffer_activate);

   if (prevShaderBuffer != this)
   {
      // If the previous buffer is dirty, than we can't compare 
      // against it, because it hasn't sent its contents to the
      // card yet and must be copied.
      if (prevShaderBuffer && !prevShaderBuffer->isDirty())
      {
         PROFILE_SCOPE(GFXGLShaderConstBuffer_activate_dirty_check_1);
         // If the buffer content is equal then we set the dirty
         // flag to false knowing the current state of the card matches
         // the new buffer.
         //
         // If the content is not equal we set the dirty flag to
         // true which causes the full content of the buffer to be
         // copied to the card.
         //
         mVertexConstBuffer->setDirty(!prevShaderBuffer->mVertexConstBuffer->isEqual(mVertexConstBuffer));
         mFragConstBuffer->setDirty(!prevShaderBuffer->mFragConstBuffer->isEqual(mFragConstBuffer));
      }
      else
      {
         // This happens rarely... but it can happen.
         // We copy the entire dirty state to the card.
         PROFILE_SCOPE(GFXGLShaderConstBuffer_activate_dirty_check_2);

         mVertexConstBuffer->setDirty(true);
         mFragConstBuffer->setDirty(true);
      }
   }

   U32 bufferStartOffset;
   U32 bufferUpdateSize;

   if (mVertexConstBuffer->isDirty())
   {
      const U8* pData = mVertexConstBuffer->getDirtyBuffer(&bufferStartOffset, &bufferUpdateSize);
      glBindBuffer(GL_UNIFORM_BUFFER, mVertexUBO);
      glBufferSubData(GL_UNIFORM_BUFFER, bufferStartOffset, bufferUpdateSize, pData);

      glBindBufferRange(GL_UNIFORM_BUFFER, GFX_GL_UBO_VERT_BIND, mVertexUBO, 0, mVertexConstBufferLayout->getBufferSize());
   }

   if (mFragConstBuffer->isDirty())
   {
      const U8* pData = mFragConstBuffer->getDirtyBuffer(&bufferStartOffset, &bufferUpdateSize);
      glBindBuffer(GL_UNIFORM_BUFFER, mFragUBO);
      glBufferSubData(GL_UNIFORM_BUFFER, bufferStartOffset, bufferUpdateSize, pData);

      glBindBufferRange(GL_UNIFORM_BUFFER, GFX_GL_UBO_FRAG_BIND, mFragUBO, 0, mFragConstBufferLayout->getBufferSize());
   }

#ifdef TORQUE_DEBUG
   // Make sure all the constants for this buffer were assigned.
   if (mWasLost)
   {
      mVertexConstBuffer->assertUnassignedConstants(mShader->getVertexShaderFile().c_str());
      mFragConstBuffer->assertUnassignedConstants(mShader->getPixelShaderFile().c_str());
   }
#endif

   // Clear the lost state.
   mWasLost = false;
}

const String GFXGLShaderConstBuffer::describeSelf() const
{
   return String();
}

void GFXGLShaderConstBuffer::onShaderReload( GFXGLShader *shader )
{
}

//-----------------------------------------------------------------------------
// GFXGLShader
//-----------------------------------------------------------------------------

GFXGLShader::GFXGLShader(GFXGLDevice* device) :
   mProgramPipeline(0),
   mVertexProgram(0),
   mFragmentProgram(0),
   mVertexConstBufferLayout(NULL),
   mFragConstBufferLayout(NULL),
   mDevice(device)
{
   VECTOR_SET_ASSOCIATION(mShaderConsts);
}

GFXGLShader::~GFXGLShader()
{
   clearShaders();
   for(HandleMap::Iterator i = mHandles.begin(); i != mHandles.end(); i++)
      delete i->value;

   // delete const buffer layouts
   SAFE_DELETE(mVertexConstBufferLayout);
   SAFE_DELETE(mFragConstBufferLayout);
}

void GFXGLShader::clearShaders()
{
   glDeleteProgramPipelines(1, &mProgramPipeline);
   glDeleteProgram(mVertexProgram);
   glDeleteProgram(mFragmentProgram);
   
   mProgramPipeline = 0;
   mVertexProgram = 0;
   mFragmentProgram = 0;
}

bool GFXGLShader::_init()
{
   PROFILE_SCOPE(GFXGLShader_Init);
   // Don't initialize empty shaders.
   if ( mVertexFile.isEmpty() && mPixelFile.isEmpty() )
      return false;

   clearShaders();

   glGenProgramPipelines(1, &mProgramPipeline);
   
   // Set the macros and add the global ones.
   Vector<GFXShaderMacro> macros;
   macros.merge( mMacros );
   macros.merge( smGlobalMacros );
   
   macros.increment();
   macros.last().name = "TORQUE_SM";
   macros.last().value = 50;
   macros.increment();
   macros.last().name = "TORQUE_VERTEX_SHADER";
   macros.last().value = "";

   if (!mVertexConstBufferLayout)
      mVertexConstBufferLayout = new GFXGLConstBufferLayout();
   else
      mVertexConstBufferLayout->clear();

   if (!mFragConstBufferLayout)
      mFragConstBufferLayout = new GFXGLConstBufferLayout();
   else
      mFragConstBufferLayout->clear();

   mSamplerDescriptions.clear();
   mShaderConsts.clear();

   bool compiledVertexShader = false;
   bool compiledFragShader = false;
   
   // Compile the vertex and pixel shaders if specified.
   if(!mVertexFile.isEmpty())
      compiledVertexShader = _compileShader(mVertexFile, true, macros);

   macros.last().name = "TORQUE_PIXEL_SHADER";
   if(!mPixelFile.isEmpty())
      compiledFragShader = _compileShader(mPixelFile, false, macros);
      
   // If either shader was present and failed to compile, bail.
   if (!compiledVertexShader || !compiledFragShader)
   {
      //failing to compile a shader is a show stopper
      AssertISV(false, "GFXGLShader::initShader - failed to compile shader");
      return false;
   }

   //Bind the separate program stages to the program pipeline
   glUseProgramStages(mProgramPipeline, GL_VERTEX_SHADER_BIT, mVertexProgram);
   glUseProgramStages(mProgramPipeline, GL_FRAGMENT_SHADER_BIT, mFragmentProgram);

   //get the shader constants
   _getShaderConstants(mVertexProgram, mVertexConstBufferLayout, mSamplerDescriptions);
   _getShaderConstants(mFragmentProgram, mFragConstBufferLayout, mSamplerDescriptions);

   // Existing handles are resored to an uninitialized state.
  // Those that are found when parsing the layout parameters
  // will then be re-initialized.
   HandleMap::Iterator iter = mHandles.begin();
   for (; iter != mHandles.end(); iter++)
      (iter->value)->clear();

   _buildShaderConstantHandles(mVertexConstBufferLayout, true);
   _buildShaderConstantHandles(mFragConstBufferLayout, false);

   _buildSamplerShaderConstantHandles(mSamplerDescriptions);
   _buildInstancingShaderConstantHandles();

   
   // Notify Buffers we might have changed in size. 
   // If this was our first init then we won't have any activeBuffers 
   // to worry about unnecessarily calling.
   Vector<GFXShaderConstBuffer*>::iterator biter = mActiveBuffers.begin();
   for ( ; biter != mActiveBuffers.end(); biter++ )   
      ((GFXGLShaderConstBuffer*)(*biter))->onShaderReload( this );
   
   return true;
}

GFXShaderConstHandle* GFXGLShader::getShaderConstHandle(const String& name)
{
   HandleMap::Iterator i = mHandles.find(name);
   if (i != mHandles.end())
   {
      return i->value;
   }
   else
   {
      GFXGLShaderConstHandle* handle = new GFXGLShaderConstHandle();
      handle->setValid(false);
      handle->mShader = this;
      mHandles[name] = handle;

      return handle;
   }
}

GFXShaderConstHandle* GFXGLShader::findShaderConstHandle(const String& name)
{
   HandleMap::Iterator i = mHandles.find(name);
   if (i != mHandles.end())
      return i->value;
   else
   {
      return NULL;
   }
}

GFXShaderConstBufferRef GFXGLShader::allocConstBuffer()
{
   if (mVertexConstBufferLayout && mFragConstBufferLayout)
   {
      GFXGLShaderConstBuffer* buffer = new GFXGLShaderConstBuffer(this, mVertexConstBufferLayout, mFragConstBufferLayout);
      mActiveBuffers.push_back(buffer);
      buffer->registerResourceWithDevice(getOwningDevice());
      return buffer;
   }

   return NULL;
}

void GFXGLShader::bindProgramPipeline()
{
   glBindProgramPipeline(mProgramPipeline);
}

bool GFXGLShader::_compileShader( const Torque::Path &file,
                              bool isVertex, 
                              const Vector<GFXShaderMacro> &macros )
{
   PROFILE_SCOPE(GFXGLShader_CompileShader);

   //setup the conversion options
   GFXShaderConvertOptions options;
   options.filePath = file;
   options.sourceLang = GFXShaderLanguageHLSL;
   options.targetLang = GFXShaderLanguageGLSL;
   options.stage = isVertex ? GFXShaderStageVertex : GFXShaderStagePixel;
   options.macros = macros;
   //TODO: implement optimization - glslang needs spirv_tools added
#ifdef TORQUE_DEBUG
   options.optimize = false; 
#else
   options.optimize = true;
#endif

   //add vulkan define to enable explicit locations from the HLSL shaders - TODO break this up in shadermodel.hlsl and add a new define because...
   //TORQUE_VULKAN may in the future introduce features that break this hlsl->glsl path
   GFXShaderMacro macro = GFXShaderMacro("TORQUE_VULKAN");
   options.macros.push_back(macro);

   GFXShaderConvertOutput output;

   //convert from hlsl to glsl - TODO use GL_ARB_spirv_extensions for drivers that support it?
   if (!GFXShaderConvert::convert(options, output))
   {
      Con::errorf("GFXGLShader::initShader: Failed to convert from hlsl to glsl. Source filename: %s", file.getFullPath().c_str());
      return false;
   }

   //double check we got the right language
   if (output.language != GFXShaderLanguageGLSL)
   {
      Con::errorf("GFXGLShader::initShader: GFXShaderConvert::convert has produced the wrong output language. source filename: %s", file.getFullPath().c_str());
      return false;
   }

   //shader stage we are creating
   GLuint stage = GL_FRAGMENT_SHADER;
   if (isVertex)
      stage = GL_VERTEX_SHADER;

   //glsl source
   const char* pShaderSrc = output.srcString.c_str();

   //create the program - this peforms both compiling and linking
   GLuint program = glCreateShaderProgramv(stage, 1, &pShaderSrc);

   //check for errors. We only check for link errors because of the way glCreateShaderProgramv operates - see above
   GLint linkStatus;
   glGetProgramiv(program, GL_LINK_STATUS, &linkStatus);

   // Dump the info log to the console
   U32 logLength = 0;
   glGetProgramiv(program, GL_INFO_LOG_LENGTH, (GLint*)&logLength);
   if (logLength)
   {
      FrameAllocatorMarker fam;
      char* log = (char*)fam.alloc(logLength);
      glGetProgramInfoLog(program, logLength, NULL, log);

      if (linkStatus == GL_FALSE)
      {
         if (smLogErrors)
         {
            Con::errorf("GFXGLShader::init - Error linking shader!");
            Con::errorf("Program %s : %s", isVertex ? mVertexFile.getFullPath().c_str() :  mPixelFile.getFullPath().c_str(), log);
         }
      }
      else if (smLogWarnings)
      {
         Con::warnf("Program %s : %s", isVertex ? mVertexFile.getFullPath().c_str() : mPixelFile.getFullPath().c_str(), log);
      }
   }

   // If we failed to link, bail.
   if (linkStatus == GL_FALSE)
      return false;

   GLuint blockIndex = glGetUniformBlockIndex(program, GFX_GL_UBO_NAME);
   GLint blockSize = 0;
   if (blockIndex != GL_INVALID_INDEX)
   {
      glUniformBlockBinding(program, blockIndex, isVertex ? GFX_GL_UBO_VERT_BIND : GFX_GL_UBO_FRAG_BIND);
      glGetActiveUniformBlockiv(program, blockIndex, GL_UNIFORM_BLOCK_DATA_SIZE, &blockSize);
   }   

   //set ubo size and store the program
   if (isVertex)
   {
      if(blockSize)
         mVertexConstBufferLayout->setSize(blockSize);

      mVertexProgram = program;
   }
   else
   {
      if(blockSize)
         mFragConstBufferLayout->setSize(blockSize);

      mFragmentProgram = program;
   }

   return true;
}

void GFXGLShader::_getShaderConstants(GLuint program, GFXGLConstBufferLayout* bufferLayout, Vector<GFXShaderConstDesc>& samplerDescriptions)
{
   //use could use spirv_cross for this

   GLint numUniforms;
   glGetProgramiv(program, GL_ACTIVE_UNIFORMS, &numUniforms);
   GLint maxNameLength;
   glGetProgramiv(program, GL_ACTIVE_UNIFORM_MAX_LENGTH, &maxNameLength);

   if (!maxNameLength)
      return;

   maxNameLength++;

   FrameTemp<GLchar> uniformName(maxNameLength);
   S32 bindingPoint = 0;
   for (U32 i = 0; i < numUniforms; i++)
   {
      GLint size;
      GLenum type;
      glGetActiveUniform(program, i, maxNameLength, NULL, &size, &type, uniformName);
      GFXShaderConstDesc desc;

      desc.name = String((char*)uniformName);
      // Remove array brackets from the name
      desc.name = desc.name.substr(0, desc.name.find('['));

      //remove the ubo block name
      if (desc.name.startsWith(GFX_GL_UBO_NAME))
      {
         static String removeStr = String(GFX_GL_UBO_NAME) + ".";
         const dsize_t pos = dStrlen(removeStr.c_str());
         desc.name = desc.name.substr(pos);
      }

      desc.name.insert(0, '$');
      desc.arraySize = size;

      bool isSampler = false;
      switch (type)
      {
      case GL_FLOAT:
         desc.constType = GFXSCT_Float;
         break;
      case GL_FLOAT_VEC2:
         desc.constType = GFXSCT_Float2;
         break;
      case GL_FLOAT_VEC3:
         desc.constType = GFXSCT_Float3;
         break;
      case GL_FLOAT_VEC4:
         desc.constType = GFXSCT_Float4;
         break;
      case GL_INT:
         desc.constType = GFXSCT_Int;
         break;
      case GL_INT_VEC2:
         desc.constType = GFXSCT_Int2;
         break;
      case GL_INT_VEC3:
         desc.constType = GFXSCT_Int3;
         break;
      case GL_INT_VEC4:
         desc.constType = GFXSCT_Int4;
         break;
      case GL_FLOAT_MAT2:
         desc.constType = GFXSCT_Float2x2;
         break;
      case GL_FLOAT_MAT3:
         desc.constType = GFXSCT_Float3x3;
         break;
      case GL_FLOAT_MAT4:
         desc.constType = GFXSCT_Float4x4;
         break;
      case GL_FLOAT_MAT4x3: // jamesu - columns, rows
         desc.constType = GFXSCT_Float4x3;
         break;
      case GL_SAMPLER_1D:
      case GL_SAMPLER_2D:
      case GL_SAMPLER_3D:
      case GL_SAMPLER_1D_SHADOW:
      case GL_SAMPLER_2D_SHADOW:
         desc.constType = GFXSCT_Sampler;
         isSampler = true;
         break;
      case GL_SAMPLER_CUBE:
         desc.constType = GFXSCT_SamplerCube;
         isSampler = true;
         break;
      case GL_SAMPLER_CUBE_MAP_ARRAY:
         desc.constType = GFXSCT_SamplerCubeArray;
         isSampler = true;
         break;
      case GL_SAMPLER_2D_ARRAY:
         desc.constType = GFXSCT_SamplerTextureArray;
         isSampler = true;
         break;
      default:
         AssertFatal(false, "GFXGLShader::_getShaderConstants - unrecognized uniform type");
         // If we don't recognize the constant don't add its description.
         continue;
      }

      if (isSampler)
      {
         GLint location = glGetUniformLocation(program, uniformName);
         glProgramUniform1i(program, location, bindingPoint);
         desc.arraySize = bindingPoint; //we store the binding point in here
         bindingPoint++;
         samplerDescriptions.push_back(desc);
      }
      else
      {
         GLuint index;
         GLint offset;
         //get the index of this uniform name
         glGetUniformIndices(program, 1, &uniformName, &index);
         //get the offset
         glGetActiveUniformsiv(program, 1, &index, GL_UNIFORM_OFFSET, &offset);
         // get the alignment value
         U32 alignBytes = getAlignmentValue(desc.constType);
         //get the size of this uniform
         U32 varSize = getConstantSize(desc.constType) * desc.arraySize;
         //add to the buffer layout
         bufferLayout->addParameter(desc.name, desc.constType, offset, varSize, desc.arraySize, alignBytes);
      }

      mShaderConsts.push_back(desc);
   }
}

void GFXGLShader::_buildShaderConstantHandles(GenericConstBufferLayout* layout, bool vertexConst)
{
   for (U32 i = 0; i < layout->getParameterCount(); i++)
   {
      GenericConstBufferLayout::ParamDesc pd;
      layout->getDesc(i, pd);

      GFXGLShaderConstHandle* handle;
      HandleMap::Iterator j = mHandles.find(pd.name);

      if (j != mHandles.end())
      {
         handle = j->value;
         handle->mShader = this;
         handle->setValid(true);
      }
      else
      {
         handle = new GFXGLShaderConstHandle();
         handle->mShader = this;
         mHandles[pd.name] = handle;
         handle->setValid(true);
      }

      if (vertexConst)
      {
         handle->mVertexConstant = true;
         handle->mVertexHandle = pd;
      }
      else
      {
         handle->mFragConstant = true;
         handle->mFragHandle = pd;
      }
   }
}

void GFXGLShader::_buildSamplerShaderConstantHandles(Vector<GFXShaderConstDesc>& samplerDescriptions)
{
   Vector<GFXShaderConstDesc>::iterator iter = samplerDescriptions.begin();
   for (; iter != samplerDescriptions.end(); iter++)
   {
      const GFXShaderConstDesc& desc = *iter;

      AssertFatal(desc.constType == GFXSCT_Sampler ||
         desc.constType == GFXSCT_SamplerCube ||
         desc.constType == GFXSCT_SamplerCubeArray ||
         desc.constType == GFXSCT_SamplerTextureArray,
         "GFXGLShader::_buildSamplerShaderConstantHandles - Invalid samplerDescription type!");

      GFXGLShaderConstHandle* handle;
      HandleMap::Iterator j = mHandles.find(desc.name);

      if (j != mHandles.end())
         handle = j->value;
      else
      {
         handle = new GFXGLShaderConstHandle();
         mHandles[desc.name] = handle;
      }

      handle->mShader = this;
      handle->setValid(true);
      handle->mFragConstant = true;
      handle->mFragHandle.name = desc.name;
      handle->mFragHandle.constType = desc.constType;
      handle->mFragHandle.offset = desc.arraySize;
   }
}

void GFXGLShader::_buildInstancingShaderConstantHandles()
{
   // If we have no instancing than just return
   if (!mInstancingFormat)
      return;

   U32 offset = 0;
   for (U32 i = 0; i < mInstancingFormat->getElementCount(); i++)
   {
      const GFXVertexElement& element = mInstancingFormat->getElement(i);

      String constName = String::ToString("$%s", element.getSemantic().c_str());

      GFXGLShaderConstHandle* handle;
      HandleMap::Iterator j = mHandles.find(constName);

      if (j != mHandles.end())
         handle = j->value;
      else
      {
         handle = new GFXGLShaderConstHandle();
         mHandles[constName] = handle;
      }

      handle->mShader = this;
      handle->setValid(true);
      handle->mInstancingConstant = true;

      // We shouldn't have an instancing constant that is also 
      // a vertex or pixel constant!  This means the shader features
      // are confused as to what is instanced.
      //
      AssertFatal(!handle->mVertexConstant &&
         !handle->mFragConstant,
         "GFXGLShader::_buildInstancingShaderConstantHandles - Bad instanced constant!");

      // HACK:  The GFXGLShaderConstHandle will check mVertexConstant then
      // fall back to reading the mPixelHandle values.  We depend on this here
      // and store the data we need in the mPixelHandle constant although its
      // not a pixel shader constant.
      //
      handle->mFragHandle.name = constName;
      handle->mFragHandle.offset = offset;

      // If this is a matrix we will have 2 or 3 more of these
      // semantics with the same name after it.
      for (; i < mInstancingFormat->getElementCount(); i++)
      {
         const GFXVertexElement& nextElement = mInstancingFormat->getElement(i);
         if (nextElement.getSemantic() != element.getSemantic())
         {
            i--;
            break;
         }
         offset += nextElement.getSizeInBytes();
      }
   }
}

/// Returns our list of shader constants, the material can get this and just set the constants it knows about
const Vector<GFXShaderConstDesc>& GFXGLShader::getShaderConstDesc() const
{
   return mShaderConsts;
}

/// Returns the alignment value for constType
U32 GFXGLShader::getAlignmentValue(const GFXShaderConstType constType) const
{
   const U32 mRowSizeF = 16;
   const U32 mRowSizeI = 16;

   switch (constType)
   {
   case GFXSCT_Float:
   case GFXSCT_Float2:
   case GFXSCT_Float3:
   case GFXSCT_Float4:
      return mRowSizeF;
      // Matrices
   case GFXSCT_Float2x2:
      return mRowSizeF * 2;
   case GFXSCT_Float3x3:
      return mRowSizeF * 3;
   case GFXSCT_Float4x3:
      return mRowSizeF * 3;
   case GFXSCT_Float4x4:
      return mRowSizeF * 4;
      //// Scalar
   case GFXSCT_Int:
   case GFXSCT_Int2:
   case GFXSCT_Int3:
   case GFXSCT_Int4:
      return mRowSizeI;
   default:
      AssertFatal(false, "Unsupported type!");
      return 0;
   }
}

U32 GFXGLShader::getConstantSize(const GFXShaderConstType constType) const
{

   switch (constType)
   {
   case GFXSCT_Float:
      return 4;
   case GFXSCT_Float2:
      return 8;
   case GFXSCT_Float3:
      return 12;
   case GFXSCT_Float4:
      return 16;
      // Matrices
   case GFXSCT_Float2x2:
      return 16;
   case GFXSCT_Float3x3:
      return 36;
   case GFXSCT_Float4x3:
      return 48;
   case GFXSCT_Float4x4:
      return 64;
      //// Scalar
   case GFXSCT_Int:
      return 4;
   case GFXSCT_Int2:
      return 8;
   case GFXSCT_Int3:
      return 12;
   case GFXSCT_Int4:
      return 16;
   default:
      AssertFatal(false, "Unsupported type!");
      return 0;
   }
}

void GFXGLShader::zombify()
{
   // Shaders don't need zombification
}

void GFXGLShader::resurrect()
{
   // Shaders are never zombies, and therefore don't have to be brought back.
}
