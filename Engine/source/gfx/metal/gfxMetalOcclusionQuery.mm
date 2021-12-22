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
#include "gfx/metal/gfxMetalOcclusionQuery.h"

#ifdef TORQUE_GATHER_METRICS
// For TickMs define
#include "T3D/gameBase/processList.h"
#endif

GFXMetalOcclusionQuery::GFXMetalOcclusionQuery(GFXDevice *device)
 : GFXOcclusionQuery(device), 
   mTesting(false)   
{
#ifdef TORQUE_GATHER_METRICS
   mTimer = PlatformTimer::create();
   mTimer->getElapsedMs();

   mTimeSinceEnd = 0;
   mBeginFrame = 0;
#endif
}

GFXMetalOcclusionQuery::~GFXMetalOcclusionQuery()
{
#ifdef TORQUE_GATHER_METRICS
   SAFE_DELETE(mTimer);
#endif
}

bool GFXMetalOcclusionQuery::begin()
{
   if(GFXDevice::getDisableOcclusionQuery())
      return true;


   return true;
}

void GFXMetalOcclusionQuery::end()
{
   if (GFXDevice::getDisableOcclusionQuery())
      return;

   mTesting = false;
}

GFXMetalOcclusionQuery::OcclusionQueryStatus GFXMetalOcclusionQuery::getStatus(bool block, U32 *data)
{
   // If this ever shows up near the top of a profile then your system is 
   // GPU bound or you are calling getStatus too soon after submitting it.
   //
   // To test if you are GPU bound resize your window very small and see if
   // this profile no longer appears at the top.
   //
   // To test if you are calling getStatus to soon after submitting it,
   // check the value of mTimeSinceEnd in a debug build. If it is < half the length
   // of time to render an individual frame you could have problems.
   PROFILE_SCOPE(GFXMetalOcclusionQuery_getStatus);

   if (GFXDevice::getDisableOcclusionQuery())
      return NotOccluded;
   return Error;  
}

void GFXMetalOcclusionQuery::zombify()
{

}

void GFXMetalOcclusionQuery::resurrect()
{

}

const String GFXMetalOcclusionQuery::describeSelf() const
{
   // We've got nothing
   return String();
}