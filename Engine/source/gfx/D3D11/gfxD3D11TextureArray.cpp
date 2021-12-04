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

#include "gfxD3D11TextureArray.h"
#include "gfxD3D11Device.h"
#include "gfxD3D11EnumTranslate.h"
#include "core/util/tVector.h"
#include "gfx/util/screenspace.h"
#include "shaderGen/shaderFeature.h"

GFXD3D11TextureArray::GFXD3D11TextureArray() :
   mSRView(NULL),
   mTextureArray(NULL)
{
}

GFXD3D11TextureArray::~GFXD3D11TextureArray()
{
   SAFE_RELEASE(mSRView);
   SAFE_RELEASE(mTextureArray);
}

void GFXD3D11TextureArray::init()
{
   mTextureArrayDesc.Width = mWidth;
   mTextureArrayDesc.Height = mHeight;
   mTextureArrayDesc.MipLevels = mMipLevels;
   mTextureArrayDesc.ArraySize = mArraySize;
   mTextureArrayDesc.Format = GFXD3D11TextureFormat[mFormat];
   mTextureArrayDesc.SampleDesc.Count = 1;
   mTextureArrayDesc.SampleDesc.Quality = 0;
   mTextureArrayDesc.Usage = D3D11_USAGE_DEFAULT;
   mTextureArrayDesc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
   mTextureArrayDesc.CPUAccessFlags = 0;
   mTextureArrayDesc.MiscFlags = 0;

   HRESULT hr = D3D11DEVICE->CreateTexture2D(&mTextureArrayDesc, NULL, &mTextureArray);
   AssertFatal(SUCCEEDED(hr), "GFXD3D11TextureArray::init failed to create texture array!");

   // create shader resource view
   D3D11_SHADER_RESOURCE_VIEW_DESC desc = {};
   desc.Format = GFXD3D11TextureFormat[mFormat];
   desc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2DARRAY;
   desc.Texture2DArray.MostDetailedMip = 0;
   desc.Texture2DArray.MipLevels = mMipLevels;
   desc.Texture2DArray.FirstArraySlice = 0;
   desc.Texture2DArray.ArraySize = mArraySize;

   hr = D3D11DEVICE->CreateShaderResourceView(mTextureArray, &desc, &mSRView);
   AssertFatal(SUCCEEDED(hr), "GFXD3D11TextureArray::init() failed to create shader resourve view!");
}

void GFXD3D11TextureArray::_setTexture(const GFXTexHandle& texture, U32 slot)
{
   GFXD3D11TextureObject *texObj = dynamic_cast<GFXD3D11TextureObject*>(texture.getPointer());
   ID3D11Texture2D* tex2d = texObj->get2DTex();
   D3D11_TEXTURE2D_DESC desc;
   tex2d->GetDesc(&desc);
   // for each mipmap level...
   for (UINT j = 0; j < desc.MipLevels; ++j)
   {
      const U32 srcSubResource = D3D11CalcSubresource(j, 0, desc.MipLevels);
      const U32 dstSubResource = D3D11CalcSubresource(j, slot, mTextureArrayDesc.MipLevels);
      D3D11DEVICECONTEXT->CopySubresourceRegion(mTextureArray, dstSubResource, 0, 0, 0, tex2d, srcSubResource, NULL);
   }
}

void GFXD3D11TextureArray::setToTexUnit(U32 tuNum)
{
   D3D11DEVICECONTEXT->PSSetShaderResources(tuNum, 1, &mSRView);
}

ID3D11ShaderResourceView* GFXD3D11TextureArray::getSRView()
{
   return mSRView;
}

ID3D11ShaderResourceView** GFXD3D11TextureArray::getSRViewPtr()
{
   return &mSRView;
}

void GFXD3D11TextureArray::zombify()
{
   // Unsupported
}

void GFXD3D11TextureArray::resurrect()
{
   // Unsupported
}
