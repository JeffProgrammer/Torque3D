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

#include "gfx/metal/gfxMetalDevice.h"
#include "gfx/metal/gfxMetalBuffer.h"

Vector<GFXMetalVolatileBufferManager::VolatileBuffer> GFXMetalVolatileBufferManager::mVolatileBufferPool;

void GFXMetalBuffer::alloc(S32 size, GFXBufferType bufferType, void* data)
{
   mType = bufferType;
   mSize = size;
   
   MTLResourceOptions options;
   if (bufferType == GFXBufferTypeStatic)
      options = MTLResourceStorageModePrivate;
   else
      options = MTLResourceStorageModeShared;
   
   if (data == NULL)
      mBuffer = [GFXMetal->getDevice() newBufferWithLength:size options:options];
   else
      mBuffer = [GFXMetal->getDevice() newBufferWithBytes:data length:size options:options];
}

void GFXMetalBuffer::free()
{
   [mBuffer release];
}

void GFXMetalBuffer::lock(U32 start, U32 end, void **vertexPtr)
{
   AssertFatal(mType == GFXBufferTypeStatic, "Cannot lock a static buffer in metal [for now ;)]");
   
   // For now we only allow updating the whole buffer.
   AssertFatal(start == 0, "Cannot update partial buffer in metal [for now ;)]");
   
   *vertexPtr = [mBuffer contents];
}

void GFXMetalBuffer::unlock()
{
   AssertFatal(mType == GFXBufferTypeStatic, "Cannot unlock a static buffer in metal [for now;)]");
   
   // Only use this for managed mode.
   //[mBuffer didModifyRange:NSRange(0, mSize)];
}

// ----

GFXMetalBuffer* GFXMetalVolatileBufferManager::getVolatileBufferForSize(S32 size)
{
   PROFILE_SCOPE(GFXMetalVolatileBufferManager_getVolatileBufferForSize);
   
   for (auto& data : mVolatileBufferPool)
   {
      if (data.free && data.size == size)
      {
         data.free = false;
         return data.ptr;
      }
   }
   
   return allocate(size);
}

void GFXMetalVolatileBufferManager::updateVolatileBufferCache()
{
   for (auto& data : mVolatileBufferPool)
   {
      if (data.free)
         continue;
      
      data.frame++;
      if (data.frame == FRAME_THRESHOLD)
      {
         data.frame = 0;
         data.free = true;
      }
   }
}

GFXMetalBuffer* GFXMetalVolatileBufferManager::allocate(S32 size)
{
   GFXMetalBuffer *buffer = new GFXMetalBuffer;
   buffer->alloc(size, GFXBufferTypeVolatile, NULL);
   
   VolatileBuffer data;
   data.size = size;
   data.frame = 0;
   data.free = false;
   data.ptr = buffer;
   mVolatileBufferPool.push_back(data);
   
   return buffer;
}
