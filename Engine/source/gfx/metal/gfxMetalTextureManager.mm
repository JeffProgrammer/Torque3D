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

GFXMetalTextureManager::GFXMetalTextureManager()
{
   ZeroMemory(mCurTexSet, sizeof(mCurTexSet));
}

GFXMetalTextureManager::~GFXMetalTextureManager()
{
   // Destroy texture table now so just in case some texture objects
   // are still left, we don't crash on a pure virtual method call.
   SAFE_DELETE_ARRAY( mHashTable );
}

void GFXMetalTextureManager::_innerCreateTexture( GFXD3D11TextureObject *retTex, 
                                               U32 height, 
                                               U32 width, 
                                               U32 depth,
                                               GFXFormat format, 
                                               GFXTextureProfile *profile, 
                                               U32 numMipLevels,
                                               bool forceMips,
                                               S32 antialiasLevel)
{
   U32 usage = 0;
   U32 bindFlags = 0;
   U32 miscFlags = 0;

   const bool is1D = (height == 1 || width == 1) && (height != width);

   U32 cpuFlags = 0;

   retTex->mProfile = profile;
   retTex->isManaged = false;
}

//-----------------------------------------------------------------------------
// createTexture
//-----------------------------------------------------------------------------
GFXTextureObject *GFXMetalTextureManager::_createTextureObject( U32 height, 
                                                               U32 width,
                                                               U32 depth,
                                                               GFXFormat format, 
                                                               GFXTextureProfile *profile, 
                                                               U32 numMipLevels,
                                                               bool forceMips, 
                                                               S32 antialiasLevel,
                                                               GFXTextureObject *inTex )
{
   GFXD3D11TextureObject *retTex;
   if ( inTex )
   {
      AssertFatal(static_cast<GFXD3D11TextureObject*>( inTex ), "GFXMetalTextureManager::_createTexture() - Bad inTex type!");
      retTex = static_cast<GFXD3D11TextureObject*>( inTex );
      retTex->release();
   }      
   else
   {
      retTex = new GFXD3D11TextureObject(GFX, profile);
      retTex->registerResourceWithDevice(GFX);
   }

   _innerCreateTexture(retTex, height, width, depth, format, profile, numMipLevels, forceMips, antialiasLevel);

   return retTex;
}

bool GFXMetalTextureManager::_loadTexture(GFXTextureObject *aTexture, GBitmap *pDL)
{
   PROFILE_SCOPE(GFXMetalTextureManager_loadTexture);
   return true;          
}

bool GFXMetalTextureManager::_loadTexture(GFXTextureObject *inTex, void *raw)
{
   PROFILE_SCOPE(GFXMetalTextureManager_loadTextureRaw);
   return true;
}

bool GFXMetalTextureManager::_refreshTexture(GFXTextureObject *texture)
{
   return true;
}

bool GFXMetalTextureManager::_freeTexture(GFXTextureObject *texture, bool zombify)
{
   AssertFatal(dynamic_cast<GFXD3D11TextureObject *>(texture),"Not an actual d3d texture object!");
   return true;
}

/// Load a texture from a proper DDSFile instance.
bool GFXMetalTextureManager::_loadTexture(GFXTextureObject *aTexture, DDSFile *dds)
{
   PROFILE_SCOPE(GFXMetalTextureManager_loadTextureDDS);
   return true;
}

void GFXMetalTextureManager::createResourceView(U32 height, U32 width, U32 depth, DXGI_FORMAT format, U32 numMipLevels,U32 usageFlags, GFXTextureObject *inTex)
{

}
