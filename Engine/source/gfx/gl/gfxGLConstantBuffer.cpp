
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
#include "gfx/gl/gfxGLConstantBuffer.h"

#include "gfx/gl/gfxGLDevice.h"
#include "gfx/gl/gfxGLEnumTranslate.h"
#include "gfx/gl/gfxGLUtils.h"

GFXGLConstantBuffer::GFXGLConstantBuffer( GFXDevice *device,
                                       GFXConstantBufferSlot slot,
                                       U32 size,
                                       GFXBufferType bufferType )
   :  GFXConstantBuffer( device, slot, size, bufferType ),
      mZombieCache(NULL)

{
   glGenBuffers(1, &mBuffer);

   glBindBuffer(GL_UNIFORM_BUFFER, mBuffer);
   glBufferData(GL_UNIFORM_BUFFER, size, NULL, GFXGLBufferType[bufferType]);
}

GFXGLConstantBuffer::~GFXGLConstantBuffer()
{
   glDeleteBuffers(1, &mBuffer);

   if( mZombieCache )
      delete [] mZombieCache;
}

void GFXGLConstantBuffer::lock( void **vertexPtr )
{
   PROFILE_SCOPE(GFXGLConstantBuffer_lock);
   
   // todo: optimize...this is really uh...lazy for now to get it working
   glBindBuffer(GL_UNIFORM_BUFFER, mBuffer);
   lockedVertexPtr = glMapBufferRange(GL_UNIFORM_BUFFER, 0, mSize, GL_MAP_WRITE_BIT);
   *vertexPtr = lockedVertexPtr;
}

void GFXGLConstantBuffer::unlock()
{
   PROFILE_SCOPE(GFXGLVertexBuffer_unlock);

   glUnmapBuffer(GL_UNIIFORM_BUFFER);
   lockedVertexPtr = NULL;
}

void GFXGLConstantBuffer::prepare()
{
   AssertFatal(0, "GFXGLConstantBuffer::prepare - not implemented");
}

void GFXGLConstantBuffer::finish()
{
   
}

void GFXGLConstantBuffer::zombify()
{
   if(mZombieCache || !mBuffer)
      return;
      
   mZombieCache = new U8[mSize];
   glBindBuffer(GL_UNIFORM_BUFFER, mBuffer);
   glGetBufferSubData(GL_UNIFORM_BUFFER, 0, mSize, mZombieCache);
   glBindBuffer(GL_UNIFORM_BUFFER, 0);
   glDeleteBuffers(1, &mBuffer);
   mBuffer = 0;
}

void GFXGLConstantBuffer::resurrect()
{
   if(!mZombieCache)
      return;
   
   glGenBuffers(1, &mBuffer);
   glBindBuffer(GL_UNIFORM_BUFFER, mBuffer);
   glBufferData(GL_UNIFORM_BUFFER, mSize, mZombieCache, GFXGLBufferType[mBufferType]);
   glBindBuffer(GL_UNIFORM_BUFFER, 0);
   
   delete[] mZombieCache;
   mZombieCache = NULL;
}
