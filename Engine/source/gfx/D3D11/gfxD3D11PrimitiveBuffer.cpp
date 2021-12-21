//-----------------------------------------------------------------------------
// Copyright (c) 2015 GarageGames, LLC
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

#include "gfx/D3D11/gfxD3D11Device.h"
#include "gfx/D3D11/gfxD3D11EnumTranslate.h"
#include "gfx/D3D11/gfxD3D11PrimitiveBuffer.h"
#include "core/util/safeRelease.h"

void GFXD3D11PrimitiveBuffer::prepare()
{
   D3D11->_setPrimitiveBuffer(this);
}

void GFXD3D11PrimitiveBuffer::lock(U32 indexStart, U32 indexEnd, void **indexPtr)
{
   AssertFatal(!mLocked, "GFXD3D11PrimitiveBuffer::lock - Can't lock a primitive buffer more than once!");

   mLocked = true;
   D3D11_MAP flags = D3D11_MAP_WRITE_DISCARD;

   switch(mBufferType)
   {
   case GFXBufferTypeStatic:
   case GFXBufferTypeDynamic:
	   flags = D3D11_MAP_WRITE_DISCARD;
	   break;

   case GFXBufferTypeVolatile:
      // Get our range now...
      AssertFatal(indexStart == 0,                "Cannot get a subrange on a volatile buffer.");
      AssertFatal(indexEnd < GFX_MAX_DYNAMIC_INDICES, "Cannot get more than GFX_MAX_DYNAMIC_INDICES in a volatile buffer. Up the constant!");

      // Get the primtive buffer
      mVolatileBuffer = D3D11->mDynamicPB;

      AssertFatal( mVolatileBuffer, "GFXD3D11PrimitiveBuffer::lock - No dynamic primitive buffer was available!");

      // We created the pool when we requested this volatile buffer, so assume it exists...
      if(mVolatileBuffer->mIndexCount + indexEnd > GFX_MAX_DYNAMIC_INDICES) 
      {
         flags = D3D11_MAP_WRITE_DISCARD;
         mVolatileStart = indexStart  = 0;
         indexEnd       = indexEnd;
      }
      else 
      {
         flags = D3D11_MAP_WRITE_NO_OVERWRITE;
         mVolatileStart = indexStart  = mVolatileBuffer->mIndexCount;
         indexEnd                    += mVolatileBuffer->mIndexCount;
      }

      mVolatileBuffer->mIndexCount = indexEnd + 1;
      ib = mVolatileBuffer->ib;

      break;
   }
	
   mIndexStart = indexStart;
   mIndexEnd = indexEnd;
	
   if (mBufferType == GFXBufferTypeStatic)
   {
      U32 sizeToLock = (indexEnd - indexStart) * sizeof(U16);
      mFrameAllocator.lock(sizeToLock);
      *indexPtr = (void*)mFrameAllocator.getlockedPtr();
   }
   else
   {
      D3D11_MAPPED_SUBRESOURCE pIndexData;
      ZeroMemory(&pIndexData, sizeof(D3D11_MAPPED_SUBRESOURCE));

      HRESULT hr = D3D11DEVICECONTEXT->Map(ib, 0, flags, 0, &pIndexData);

      if(FAILED(hr)) 
      {
         AssertFatal(false, "GFXD3D11PrimitiveBuffer::lock - Could not lock primitive buffer.");
      }

      *indexPtr = (U8*)pIndexData.pData + (indexStart * sizeof(U16)) ;		
   }
}

void GFXD3D11PrimitiveBuffer::unlock()
{   
   const U32 totalSize = this->mIndexCount * sizeof(U16);
	  
   if (mBufferType == GFXBufferTypeStatic)
   {
      //set up the update region of the buffer
      D3D11_BOX box;
      box.back  = 1;
      box.front = 0;
      box.top = 0;
      box.bottom =1;
      box.left = mIndexStart *sizeof(U16);
      box.right = mIndexEnd * sizeof(U16);
      //update the real ib buffer
      D3D11DEVICECONTEXT->UpdateSubresource(ib, 0, &box, mFrameAllocator.getlockedPtr(), totalSize, 0);
      mFrameAllocator.unlock();
   }
   else
   {		 
      D3D11DEVICECONTEXT->Unmap(ib,0);   
   }
  
   mLocked = false;
   mIsFirstLock = false;
   mVolatileBuffer = NULL;
}

GFXD3D11PrimitiveBuffer::~GFXD3D11PrimitiveBuffer() 
{
   if( mBufferType != GFXBufferTypeVolatile )
   {
      SAFE_RELEASE(ib);
   }
}

void GFXD3D11PrimitiveBuffer::zombify()
{
   if (mBufferType == GFXBufferTypeStatic)
      return;
            
   AssertFatal(!mLocked, "GFXD3D11PrimitiveBuffer::zombify - Cannot zombify a locked buffer!");

   if (mBufferType == GFXBufferTypeVolatile)
   {
      // We must null the volatile buffer else we're holding
      // a dead pointer which can be set on the device.      
      ib = NULL;
      return;
   }

   // Dynamic buffers get released.
   SAFE_RELEASE(ib);
}

void GFXD3D11PrimitiveBuffer::resurrect()
{
	if ( mBufferType != GFXBufferTypeDynamic )
		return;

	D3D11_BUFFER_DESC desc;
	desc.ByteWidth = sizeof(U16) * mIndexCount;
	desc.Usage = D3D11_USAGE_DYNAMIC;
	desc.BindFlags = D3D11_BIND_INDEX_BUFFER;
	desc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
	desc.MiscFlags = 0;
	desc.StructureByteStride = 0;

	HRESULT hr = D3D11DEVICE->CreateBuffer(&desc, NULL, &ib);

	if(FAILED(hr)) 
	{
		AssertFatal(false, "GFXD3D11PrimitiveBuffer::resurrect - Failed to allocate an index buffer.");
	}
}
