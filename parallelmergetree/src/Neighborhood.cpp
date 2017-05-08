#include <cassert>
#include "Neighborhood.h"

Neighborhood::iterator::iterator()
: mNeighbors(NULL), mOffsets(NULL), mOrigin(0),mCount(0),mCurrent(0)
{
  mDim[0] = 0;
  mDim[1] = 0;
  mDim[2] = 0;

  mCoords[0] = mCoords[1] = mCoords[2] = 0;
}

Neighborhood::iterator::iterator(LocalIndexType v, LocalIndexType dim[3],int8_t* neighbors,SignedLocalIndexType* offsets, uint8_t count)
: mNeighbors(neighbors), mOffsets(offsets), mOrigin(v),mCount(count),mCurrent(0)
{
  mDim[0] = dim[0];
  mDim[1] = dim[1];
  mDim[2] = dim[2];

  mCoords[0] = v % mDim[0];
  mCoords[1] = (v / mDim[0]) % mDim[1];
  mCoords[2] = v / (mDim[0]*mDim[1]);

  while ((mCurrent < mCount) && (!inside(mCurrent)))
    mCurrent++;
}

Neighborhood::iterator::iterator(const iterator& it) : mNeighbors(it.mNeighbors),mOffsets(it.mOffsets),mOrigin(it.mOrigin),
mCount(it.mCount), mCurrent(it.mCurrent)
{
  mDim[0] = it.mDim[0];
  mDim[1] = it.mDim[1];
  mDim[2] = it.mDim[2];

  mCoords[0] = it.mCoords[0];
  mCoords[1] = it.mCoords[1];
  mCoords[2] = it.mCoords[2];
}

Neighborhood::iterator& Neighborhood::iterator::operator=(const iterator& it)
{
  mNeighbors = it.mNeighbors;
  mOffsets = it.mOffsets;
  mOrigin = it.mOrigin;
  mCount = it.mCount;
  mCurrent = it.mCurrent;

  mDim[0] = it.mDim[0];
  mDim[1] = it.mDim[1];
  mDim[2] = it.mDim[2];

  mCoords[0] = it.mCoords[0];
  mCoords[1] = it.mCoords[1];
  mCoords[2] = it.mCoords[2];

  return *this;
}


bool Neighborhood::iterator::operator!=(const iterator& it)
{
  if ((mCurrent != it.mCurrent) || (mOrigin != it.mOrigin))
    return true;

  return false;
}

Neighborhood::iterator& Neighborhood::iterator::operator++(int i)
{
  mCurrent++;
  while ((mCurrent < mCount) && !inside(mCurrent))
    mCurrent++;

  return *this;
}

bool Neighborhood::iterator::inside(uint8_t i)
{
  assert (i < this->mCount);
  if (   ((mCoords[0] + mNeighbors[3*i + 0] < 0) || (mCoords[0] + mNeighbors[3*i + 0] >= mDim[0]))
      || ((mCoords[1] + mNeighbors[3*i + 1] < 0) || (mCoords[1] + mNeighbors[3*i + 1] >= mDim[1]))
      || ((mCoords[2] + mNeighbors[3*i + 2] < 0) || (mCoords[2] + mNeighbors[3*i + 2] >= mDim[2]))) {


      return false;
  }

  return true;
}

BoundaryType Neighborhood::iterator::boundaryType(SignedLocalIndexType x, SignedLocalIndexType y, SignedLocalIndexType z) const
{
  BoundaryType t(NO_BOUNDARY);

  if (x == 0)
    t.t |= LOW_X_BOUNDARY;
  else if (x == mDim[0]-1)
    t.t |= HIGH_X_BOUNDARY;

  if (y == 0)
    t.t |= LOW_Y_BOUNDARY;
  else if (y == mDim[1]-1)
    t.t |= HIGH_Y_BOUNDARY;

  if (z == 0)
    t.t |= LOW_Z_BOUNDARY;
  else if (z == mDim[2]-1)
    t.t |= HIGH_Z_BOUNDARY;

  return t;
}


Neighborhood::Neighborhood(LocalIndexType dim[3]) : mNeighbors(NULL), mOffsets(NULL), mCount(0)
{
  mDim[0] = dim[0];
  mDim[1] = dim[1];
  mDim[2] = dim[2];
}


Neighborhood::iterator Neighborhood::begin(LocalIndexType origin)
{
  iterator it(origin,mDim,mNeighbors,mOffsets,mCount);

  return it;
}

Neighborhood::iterator Neighborhood::end(LocalIndexType origin)
{
  iterator it(origin,mDim,mNeighbors,mOffsets,mCount);

  it.mCurrent = mCount;
  return it;
}

void Neighborhood::computeOffsets()
{
  for (uint8_t i=0;i<mCount;i++)
    mOffsets[i] = mNeighbors[3*i+2]*mDim[0]*mDim[1] + mNeighbors[3*i+1]*mDim[0] + mNeighbors[3*i];
}


