#ifndef GFX_FRAMEALLOCATOR_H
#define GFX_FRAMEALLOCATOR_H

#include "core/frameAllocator.h"

/// Helper class for simulate lock/unlock on gfx resources using FrameAllocator
class GFXFrameAllocator
{
public:
   GFXFrameAllocator()
      : mFrameAllocatorMark(0),
      mFrameAllocatorPtr(NULL)
#if TORQUE_DEBUG
      , mFrameAllocatorMarkGuard(0)
#endif
   {
   }

   U8* lock(const U32 size)
   {
      AssertFatal(!mFrameAllocatorMark && !mFrameAllocatorPtr, "");
      mFrameAllocatorMark = FrameAllocator::getWaterMark();
      mFrameAllocatorPtr = (U8*)FrameAllocator::alloc( size );
#if TORQUE_DEBUG
      mFrameAllocatorMarkGuard = FrameAllocator::getWaterMark();
#endif

      return mFrameAllocatorPtr;
   }

   void unlock()
   {
#if TORQUE_DEBUG
      AssertFatal(mFrameAllocatorMarkGuard == FrameAllocator::getWaterMark(), "");
#endif
      FrameAllocator::setWaterMark(mFrameAllocatorMark);
      mFrameAllocatorMark = 0;
      mFrameAllocatorPtr = NULL;
   }

   U8* getlockedPtr() const { return mFrameAllocatorPtr; }

protected:
   U32 mFrameAllocatorMark;
   U8 *mFrameAllocatorPtr;

#if TORQUE_DEBUG
   U32 mFrameAllocatorMarkGuard;
#endif
};

#endif //GFX_FRAMEALLOCATOR_H
