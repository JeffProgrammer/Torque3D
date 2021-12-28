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

#ifndef _GFX_METAL_BUFFER_H_
#define _GFX_METAL_BUFFER_H_

#import <Metal/Metal.h>
#include "platform/platform.h"

// What we're gonna do
//
// GFXBufferTypeStatic - Private buffer gpu memory only
// GFXBufferTypeDynamic - Change to be "Tripple Buffered" so it can be updated at most once per frame
// GFXBufferTypeVolatile - Use for volatile buffers that are streamed constantly. We will use a pool of these
//                         buffers and cycle through them. Streaming may happen more than once per frame.
//
// In metal, these coorestpond to the following types:
//
// Static - Uses private. Has to use a blitencoder to copy contents from cpu to gpu
// Dynamic - 1 giant buffer under the hood *3 in size (for 3 frames worth of data).
//           Uses 'Managed' buffer type under the hood to do explicit syncing
// Volatile - uses 1 buffer under the hood and updates the contents of the buffer, recycled
//            and put into a free list once it is no longer being used.
//
// Eventually, volatile buffers will be backed by a dedicated memory resource heap that manages
// its own memory with custom allocator for streaming data.
// For now though, we will just create buffers as needed and be slow. Its 'good enough' for a first passthrough.

class GFXMetalBuffer
{
public:
   void alloc(S32 size, GFXBufferType bufferType, void* data);
   void free();
   
   void lock(U32 start, U32 end, void **vertexPtr);
   void unlock();
private:
   id<MTLBuffer> mBuffer;
   GFXBufferType mType;
   S32 mSize;
};

class GFXMetalVolatileBufferManager
{
public:
   GFXMetalBuffer* getVolatileBufferForSize(S32 size);
   
private:
   friend class GFXMetalDevice;
   
   enum
   {
      /// Max number of frames to keep the volatile buffer in use
      /// until we can assume it is no longer being used and can be put back into play
      FRAME_THRESHOLD = 3
   };
   
   /// Called by the GFXMetalDevice when a frame has ended. Do not call more than once per frame
   /// or synching might not work correctly! GFXMetalDevice controls when the frame syncs, not the manager!
   void updateVolatileBufferCache();
   
   GFXMetalBuffer* allocate(S32 size);
   
   struct VolatileBuffer
   {
      S32 size;
      struct
      {
         S16  frame : 16;
         bool free  : 16;
      };
      GFXMetalBuffer* ptr;
   };
   
   static Vector<VolatileBuffer> mVolatileBufferPool;
};

#endif
