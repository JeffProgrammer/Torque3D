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
#include "gfx/metal/gfxMetalEnumTranslate.h"
#include "gfx/metal/gfxMetalPrimitiveBuffer.h"
#include "core/util/safeRelease.h"

GFXMetalPrimitiveBuffer::GFXMetalPrimitiveBuffer(   GFXDevice *device, 
                                                         U32 indexCount, 
                                                         U32 primitiveCount, 
                                                         GFXBufferType bufferType ) 
   : GFXPrimitiveBuffer( device, indexCount, primitiveCount, bufferType )
{
}

GFXMetalPrimitiveBuffer::~GFXMetalPrimitiveBuffer() 
{
}

void GFXMetalPrimitiveBuffer::prepare()
{
	D3D11->_setPrimitiveBuffer(this);
}

void GFXMetalPrimitiveBuffer::lock(U32 indexStart, U32 indexEnd, void **indexPtr)
{	

}

void GFXMetalPrimitiveBuffer::unlock()
{
}

void GFXMetalPrimitiveBuffer::zombify()
{
}

void GFXMetalPrimitiveBuffer::resurrect()
{
}
