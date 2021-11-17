
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

#ifndef _GFXCONSTANTBUFFER_H_
#define _GFXCONSTANTBUFFER_H_

#ifndef _GFXSTRUCTS_H_
#include "gfx/gfxStructs.h"
#endif


//*****************************************************************************
// GFXConstantBuffer - base constant buffer class
//*****************************************************************************
class GFXConstantBuffer : public StrongRefBase, public GFXResource
{
   friend class GFXConstantBufferHandleBase;
   friend class GFXDevice;

public:

   U32 mSize;
   GFXConstantBufferSlot mSlot;
   
   /// GFX buffer type (static, dynamic or volatile).
   GFXBufferType mBufferType;

   /// Device this vertex buffer was allocated on.
   GFXDevice *mDevice;

   bool  isLocked;
   void* lockedVertexPtr;

   GFXConstantBuffer(  GFXDevice *device,
                     GFXConstantBufferSlot slot,
                     U32 size,
                     GFXBufferType bufferType )
      :  mSize( size ),
         mSlot(slot),
         mBufferType( bufferType ),
         mDevice( device ),
         isLocked(false),
         lockedVertexPtr(NULL)
   {
   }
   
   virtual void lock(void **constantBufferPtr) = 0;
   virtual void unlock() = 0;
   virtual void prepare() = 0;

   // GFXResource
   virtual const String describeSelf() const;
};


//*****************************************************************************
// GFXVertexBufferHandleBase
//*****************************************************************************
class GFXConstantBufferHandleBase : public StrongRefPtr<GFXConstantBuffer>
{
   friend class GFXDevice;

protected:

   void set(   GFXDevice *theDevice,
               GFXConstantBufferSlot slot,
               U32 size,
               GFXBufferType type );

   void* lock()
   {
      getPointer()->lock(&getPointer()->lockedVertexPtr);
      return getPointer()->lockedVertexPtr;
   }

   void unlock() ///< unlocks the vertex data, making changes illegal.
   {
      getPointer()->unlock();
   }
};


/// A handle object for allocating, filling, and reading a vertex buffer.
template<class T>
class GFXConstantBufferHandle : public GFXConstantBufferHandleBase
{
   typedef GFXConstantBufferHandleBase Parent;

   /// Sets this vertex buffer as the current
   /// vertex buffer for the device it was allocated on
   void prepare() { getPointer()->prepare(); }

public:

   GFXConstantBufferHandle() {}

   GFXConstantBufferHandle(  GFXDevice *theDevice,
                           GFXConstantBufferSlot slot,
                           U32 size,
                           GFXBufferType type = GFXBufferTypeVolatile )
   {
      set( theDevice, slot, size, type );
   }

   ~GFXConstantBufferHandle() {}

   void set(   GFXDevice *theDevice,
               GFXConstantBufferSlot slot,
               U32 size,
               GFXBufferType type = GFXBufferTypeVolatile )
   {
      Parent::set( theDevice, slot, size, getGFXVertexFormat<T>(), sizeof(T), type );
   }

   T *lock() ///< locks the vertex buffer range, and returns a pointer to the beginning of the vertex array
                                                   ///< also allows the array operators to work on this vertex buffer.
   {
      return (T*)Parent::lock();
   }

   void unlock() { Parent::unlock(); }

   GFXConstantBufferHandle<T>& operator=(GFXConstantBuffer *ptr)
   {
      StrongObjectRef::set(ptr);
      return *this;
   }

};


/// This is a non-typed vertex buffer handle which can be
/// used when your vertex type is undefined until runtime.
class GFXConstantBufferDataHandle : public GFXConstantBufferHandleBase
{
   typedef GFXConstantBufferHandleBase Parent;

protected:

   void prepare() { getPointer()->prepare(); }

public:

   GFXConstantBufferDataHandle()
   {
   }

   void set(   GFXDevice *theDevice,
               GFXConstantBufferSlot slot,
               U32 size,
               GFXBufferType type )
   {
      Parent::set( theDevice, slot, size, type );
   }

   U8* lock()
   {
      return (U8*)Parent::lock();
   }

   void unlock() { Parent::unlock(); }

   GFXConstantBufferDataHandle& operator=( GFXConstantBuffer *ptr )
   {
      StrongObjectRef::set(ptr);
      return *this;
   }
};


#endif // _GFXConstantBUFFER_H_


