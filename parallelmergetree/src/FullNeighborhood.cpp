#include "FullNeighborhood.h"

FullNeighborhood::FullNeighborhood(LocalIndexType dim[3]) : Neighborhood(dim)
{
  this->mCount = 26;
  this->mNeighbors = new int8_t[3*this->mCount];
  this->mOffsets = new SignedLocalIndexType[this->mCount];

  uint8_t count = 0;
  for (int8_t k=-1;k<2;k++) {
    for (int8_t j=-1;j<2;j++) {
      for (int8_t i=-1;i<2;i++) {

        if (i*i+j*j+k*k != 0) {
          this->mNeighbors[3*count + 0] = i;
          this->mNeighbors[3*count + 1] = j;
          this->mNeighbors[3*count + 2] = k;

          count++;
        }
      }
    }
  }

  this->computeOffsets();
}

FullNeighborhood::~FullNeighborhood()
{
  delete[] this->mNeighbors;
  delete[] this->mOffsets;
}



