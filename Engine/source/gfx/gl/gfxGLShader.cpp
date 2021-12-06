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

class GFXGLShaderConstHandle : public GFXShaderConstHandle
{
   friend class GFXGLShader;

public:  
   
   GFXGLShaderConstHandle( GFXGLShader *shader );
   GFXGLShaderConstHandle( GFXGLShader *shader, const GFXShaderConstDesc &desc, GLuint loc, S32 samplerNum );
   virtual ~GFXGLShaderConstHandle();
   
   void reinit( const GFXShaderConstDesc &desc, GLuint loc, S32 samplerNum );

   const String& getName() const { return mDesc.name; }
   GFXShaderConstType getType() const { return mDesc.constType; }
   U32 getArraySize() const { return mDesc.arraySize; }

   U32 getSize() const;
   void setValid( bool valid ) { mValid = valid; }   
   /// @warning This will always return the value assigned when the shader was
   /// initialized.  If the value is later changed this method won't reflect that.
   S32 getSamplerRegister() const { return mSamplerNum; }

   GFXShaderConstDesc mDesc;
   GFXGLShader* mShader;
   GLuint mLocation;
   U32 mOffset;
   U32 mSize;  
   S32 mSamplerNum; 
   bool mInstancingConstant;
};

GFXGLShaderConstHandle::GFXGLShaderConstHandle( GFXGLShader *shader )
 : mShader( shader ), mLocation(0), mOffset(0), mSize(0), mSamplerNum(-1), mInstancingConstant(false)
{
   dMemset(&mDesc, 0, sizeof(mDesc));
   mValid = false;
}

static U32 shaderConstTypeSize(GFXShaderConstType type)
{
   switch(type) 
   {
   case GFXSCT_Float:
   case GFXSCT_Int:
   case GFXSCT_Sampler:
   case GFXSCT_SamplerCube:
   case GFXSCT_SamplerCubeArray:
   case GFXSCT_SamplerTextureArray:
      return 4;
   case GFXSCT_Float2:
   case GFXSCT_Int2:
      return 8;
   case GFXSCT_Float3:
   case GFXSCT_Int3:
      return 12;
   case GFXSCT_Float4:
   case GFXSCT_Int4:
      return 16;
   case GFXSCT_Float2x2:
      return 16;
   case GFXSCT_Float3x3:
      return 36;
   case GFXSCT_Float4x3:
      return 48;
   case GFXSCT_Float4x4:
      return 64;
   default:
      AssertFatal(false,"shaderConstTypeSize - Unrecognized constant type");
      return 0;
   }
}

GFXGLShaderConstHandle::GFXGLShaderConstHandle( GFXGLShader *shader, const GFXShaderConstDesc &desc, GLuint loc, S32 samplerNum ) 
 : mShader(shader), mInstancingConstant(false)
{
   reinit(desc, loc, samplerNum);
}

void GFXGLShaderConstHandle::reinit( const GFXShaderConstDesc& desc, GLuint loc, S32 samplerNum )
{
   mDesc = desc;
   mLocation = loc;
   mSamplerNum = samplerNum;
   mOffset = 0;
   mInstancingConstant = false;
   
   U32 elemSize = shaderConstTypeSize(mDesc.constType);
   AssertFatal(elemSize, "GFXGLShaderConst::GFXGLShaderConst - elemSize is 0");
   mSize = mDesc.arraySize * elemSize;
   mValid = true;
}


U32 GFXGLShaderConstHandle::getSize() const
{
   return mSize;
}

GFXGLShaderConstHandle::~GFXGLShaderConstHandle()
{
}

GFXGLShaderConstBuffer::GFXGLShaderConstBuffer(GFXGLShader* shader, U32 bufSize, U8* existingConstants)
{
   mShader = shader;
   mWasLost = true;

   //Generate vertex and shader stage uniform buffers
   GLuint* buffers[] = { &mVertexUBO, &mFragmentUBO };
   glGenBuffers(2, buffers[0]);

   //allocate memory for the buffers - TODO bufferStorage extension
   glBindBuffer(GL_UNIFORM_BUFFER, mVertexUBO);
   glBufferData(GL_UNIFORM_BUFFER, bufSize, NULL, GL_DYNAMIC_DRAW);

   glBindBuffer(GL_UNIFORM_BUFFER, mFragmentUBO);
   glBufferData(GL_UNIFORM_BUFFER, bufSize, NULL, GL_DYNAMIC_DRAW);

   glBindBuffer(GL_UNIFORM_BUFFER, 0);
}

GFXGLShaderConstBuffer::~GFXGLShaderConstBuffer()
{
   GLuint* buffers[] = { &mVertexUBO, &mFragmentUBO };
   glDeleteBuffers(2, buffers[0]);

   if ( mShader )
      mShader->_unlinkBuffer( this );
}

template<typename ConstType>
void GFXGLShaderConstBuffer::internalSet(GFXShaderConstHandle* handle, const ConstType& param)
{
   AssertFatal(handle, "GFXGLShaderConstBuffer::internalSet - Handle is NULL!" );
   AssertFatal(handle->isValid(), "GFXGLShaderConstBuffer::internalSet - Handle is not valid!" );
   AssertFatal(dynamic_cast<GFXGLShaderConstHandle*>(handle), "GFXGLShaderConstBuffer::set - Incorrect const buffer type");

   GFXGLShaderConstHandle* _glHandle = static_cast<GFXGLShaderConstHandle*>(handle);
   AssertFatal(mShader == _glHandle->mShader, "GFXGLShaderConstBuffer::set - Should only set handles which are owned by our shader");

}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const F32 fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point2F& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point3F& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point4F& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const PlaneF& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const LinearColorF& fv)
{
   internalSet(handle, fv);
}
 
void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const S32 fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point2I& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point3I& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const Point4I& fv)
{
   internalSet(handle, fv);
}

template<typename ConstType>
void GFXGLShaderConstBuffer::internalSet(GFXShaderConstHandle* handle, const AlignedArray<ConstType>& fv)
{
   AssertFatal(handle, "GFXGLShaderConstBuffer::internalSet - Handle is NULL!" );
   AssertFatal(handle->isValid(), "GFXGLShaderConstBuffer::internalSet - Handle is not valid!" );
   AssertFatal(dynamic_cast<GFXGLShaderConstHandle*>(handle), "GFXGLShaderConstBuffer::set - Incorrect const buffer type");

   GFXGLShaderConstHandle* _glHandle = static_cast<GFXGLShaderConstHandle*>(handle);
   AssertFatal(mShader == _glHandle->mShader, "GFXGLShaderConstBuffer::set - Should only set handles which are owned by our shader");
   AssertFatal(!_glHandle->mInstancingConstant, "GFXGLShaderConstBuffer::set - Instancing not supported for array");
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<F32>& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point2F>& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point3F>& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point4F>& fv)   
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<S32>& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point2I>& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point3I>& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const AlignedArray<Point4I>& fv)
{
   internalSet(handle, fv);
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const MatrixF& mat, const GFXShaderConstType matType)
{
   AssertFatal(handle, "GFXGLShaderConstBuffer::set - Handle is NULL!" );
   AssertFatal(handle->isValid(), "GFXGLShaderConstBuffer::set - Handle is not valid!" );
   AssertFatal(dynamic_cast<GFXGLShaderConstHandle*>(handle), "GFXGLShaderConstBuffer::set - Incorrect const buffer type");

   GFXGLShaderConstHandle* _glHandle = static_cast<GFXGLShaderConstHandle*>(handle);
   AssertFatal(mShader == _glHandle->mShader, "GFXGLShaderConstBuffer::set - Should only set handles which are owned by our shader");
   AssertFatal(!_glHandle->mInstancingConstant || matType == GFXSCT_Float4x4, "GFXGLShaderConstBuffer::set - Only support GFXSCT_Float4x4 for instancing");   
}

void GFXGLShaderConstBuffer::set(GFXShaderConstHandle* handle, const MatrixF* mat, const U32 arraySize, const GFXShaderConstType matrixType)
{
   AssertFatal(handle, "GFXGLShaderConstBuffer::set - Handle is NULL!" );
   AssertFatal(handle->isValid(), "GFXGLShaderConstBuffer::set - Handle is not valid!" );

   GFXGLShaderConstHandle* _glHandle = static_cast<GFXGLShaderConstHandle*>(handle);
   AssertFatal(mShader == _glHandle->mShader, "GFXGLShaderConstBuffer::set - Should only set handles which are owned by our shader");  
   AssertFatal(!_glHandle->mInstancingConstant, "GFXGLShaderConstBuffer::set - Instancing not supported for matrix arrays");
}

void GFXGLShaderConstBuffer::activate()
{
   PROFILE_SCOPE(GFXGLShaderConstBuffer_activate);
   mShader->setConstantsFromBuffer(this);
   mWasLost = false;
}

const String GFXGLShaderConstBuffer::describeSelf() const
{
   return String();
}

void GFXGLShaderConstBuffer::onShaderReload( GFXGLShader *shader )
{
   AssertFatal( shader == mShader, "GFXGLShaderConstBuffer::onShaderReload, mismatched shaders!" );

   mWasLost = true;
}

GFXGLShader::GFXGLShader(GFXGLDevice* device) :
   mProgramPipeline(0),
   mVertexProgram(0),
   mFragmentProgram(0),
   mDevice(device),
   mConstBufferSize(0),
   mConstBuffer(NULL)
{
}

GFXGLShader::~GFXGLShader()
{
   clearShaders();
   for(HandleMap::Iterator i = mHandles.begin(); i != mHandles.end(); i++)
      delete i->value;
   
   delete[] mConstBuffer;
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


   bool compiledVertexShader = false;
   bool compiledPixelShader = false;
   
   // Compile the vertex and pixel shaders if specified.
   if(!mVertexFile.isEmpty())
      compiledVertexShader = initShader(mVertexFile, true, macros);

   macros.last().name = "TORQUE_PIXEL_SHADER";
   if(!mPixelFile.isEmpty())
      compiledPixelShader = initShader(mPixelFile, false, macros);
      
   // If either shader was present and failed to compile, bail.
   if(!compiledVertexShader || !compiledPixelShader)
      return false;

   //Bind the separate program stages to the program pipeline
   glUseProgramStages(mProgramPipeline, GL_VERTEX_SHADER_BIT, mVertexProgram);
   glUseProgramStages(mProgramPipeline, GL_FRAGMENT_SHADER_BIT, mFragmentProgram); 

   initConstantDescs();   
   initHandles();
   
   // Notify Buffers we might have changed in size. 
   // If this was our first init then we won't have any activeBuffers 
   // to worry about unnecessarily calling.
   Vector<GFXShaderConstBuffer*>::iterator biter = mActiveBuffers.begin();
   for ( ; biter != mActiveBuffers.end(); biter++ )   
      ((GFXGLShaderConstBuffer*)(*biter))->onShaderReload( this );
   
   return true;
}

void GFXGLShader::initConstantDescs()
{
}

void GFXGLShader::initHandles()
{
}

GFXShaderConstHandle* GFXGLShader::getShaderConstHandle(const String& name)
{
   HandleMap::Iterator i = mHandles.find(name);
   if(i != mHandles.end())
      return i->value;
   else
   {
      GFXGLShaderConstHandle* handle = new GFXGLShaderConstHandle( this );
      mHandles[ name ] = handle;
      
      return handle;
   }
}

GFXShaderConstHandle* GFXGLShader::findShaderConstHandle(const String& name)
{
   HandleMap::Iterator i = mHandles.find(name);
   if(i != mHandles.end())
      return i->value;
   else
   {
      return NULL;
   }
}

void GFXGLShader::setConstantsFromBuffer(GFXGLShaderConstBuffer* buffer)
{
}

GFXShaderConstBufferRef GFXGLShader::allocConstBuffer()
{
   GFXGLShaderConstBuffer* buffer = new GFXGLShaderConstBuffer(this, mConstBufferSize, mConstBuffer);
   buffer->registerResourceWithDevice(getOwningDevice());
   mActiveBuffers.push_back( buffer );
   return buffer;
}

void GFXGLShader::bindProgramPipeline()
{
   glBindProgramPipeline(mProgramPipeline);
}

void GFXGLShader::zombify()
{
   clearShaders();
   dMemset(mConstBuffer, 0, mConstBufferSize);
}

bool GFXGLShader::initShader( const Torque::Path &file, 
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

   //convert from hlsl to glsl - TODO use GL_ARB_spirv_extensions for drivers that support it
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

   //store the program stage
   if (isVertex)
      mVertexProgram = program;
   else
      mFragmentProgram = program;

   return true;
}

/// Returns our list of shader constants, the material can get this and just set the constants it knows about
const Vector<GFXShaderConstDesc>& GFXGLShader::getShaderConstDesc() const
{
   PROFILE_SCOPE(GFXGLShader_GetShaderConstants);
   return mConstants;
}

/// Returns the alignment value for constType
U32 GFXGLShader::getAlignmentValue(const GFXShaderConstType constType) const
{
   // Alignment is the same thing as size for us.
   return shaderConstTypeSize(constType);
}

const String GFXGLShader::describeSelf() const
{
   String ret;
   ret = String::ToString("   Program Pipeline: %i", mProgramPipeline);
   ret += String::ToString("   Vertex Path: %s", mVertexFile.getFullPath().c_str());
   ret += String::ToString("   Pixel Path: %s", mPixelFile.getFullPath().c_str());
   
   return ret;
}
