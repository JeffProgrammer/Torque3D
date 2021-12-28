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
#include "gfx/metal/gfxMetalVertexBuffer.h"
#include "console/console.h"
#include "gfx/metal/gfxMetalDevice.h"

GFXMetalVertexBuffer::GFXMetalVertexBuffer()
   : GFXVertexBuffer(0,0,0,0,(GFXBufferType)0),
     mBuffer(NULL)
{
}

GFXMetalVertexBuffer::GFXMetalVertexBuffer(GFXDevice *device, 
                                           U32 numVerts, 
                                           const GFXVertexFormat *vertexFormat, 
                                           U32 vertexSize, 
                                           GFXBufferType bufferType,
                                           void* data)
   : GFXVertexBuffer( device, numVerts, vertexFormat, vertexSize, bufferType ),
     mBuffer(NULL)
{
   if (mBufferType != GFXBufferTypeVolatile)
   {
      mBuffer = new GFXMetalBuffer();
      mBuffer->alloc(sizeof(GFXVertexFormat) * numVerts, mBufferType, data);
   }
   else
   {
      
   }
}

GFXMetalVertexBuffer::~GFXMetalVertexBuffer() 
{
   // Volatile buffers are in a pool, we don't own them
   if (mBuffer && mBufferType != GFXBufferTypeVolatile)
   {
      mBuffer->free();
      delete mBuffer;
   }
   mBuffer = NULL;
}

void GFXMetalVertexBuffer::lock(U32 vertexStart, U32 vertexEnd, void **vertexPtr)
{
   PROFILE_SCOPE(GFXMetalVertexBuffer_lock);
}

void GFXMetalVertexBuffer::unlock()
{
   PROFILE_SCOPE(GFXMetalVertexBuffer_unlock);
}

void GFXMetalVertexBuffer::zombify()
{
   AssertFatal(lockedVertexStart == 0 && lockedVertexEnd == 0, "GFXMetalVertexBuffer::zombify - Cannot zombify a locked buffer!");
}

void GFXMetalVertexBuffer::resurrect()
{
}

