#ifndef NEIGHBORHOOD_H_
#define NEIGHBORHOOD_H_

#include <cstdio>

#include "TypeDefinitions.h"

static const uint8_t NO_BOUNDARY = 0;
static const uint8_t LOW_X_BOUNDARY  = 1;
static const uint8_t HIGH_X_BOUNDARY = 2;

static const uint8_t LOW_Y_BOUNDARY  = 4;
static const uint8_t HIGH_Y_BOUNDARY = 8;

static const uint8_t LOW_Z_BOUNDARY  = 16;
static const uint8_t HIGH_Z_BOUNDARY = 32;

struct BoundaryType {

  //! Default constructor
  BoundaryType(uint8_t tt = 0) : t(tt) {}

  //! Does the BoundaryType of t is equal to or contain the boundary type of u
  bool contains(BoundaryType u) const {return ((t&u.t) == t) ? true : false;}

  //! Ignore all boundaries of the given type
  void ignore(BoundaryType outside) {t &= ~outside.t;}

  //! Return whether this type is on any boundary
  bool isBoundary() const {return (t == NO_BOUNDARY) ? false : true;}

  uint8_t t;
};

class Neighborhood
{
public:

  class iterator {
  public:

    friend class Neighborhood;
    //! Allow the begin() function to use the special constructor
    //friend iterator Neighborhood::begin(GlobalIndexType origin);

    //! Allow the end() function to use the special constructor
    //friend iterator Neighborhood::end(GlobalIndexType origin);

    iterator();
    iterator(const iterator& it);
    ~iterator() {}

    iterator& operator=(const iterator& it);
    bool operator==(const iterator& it) {return !(*this != it);}
    bool operator!=(const iterator& it);

    iterator& operator++(int i);

    LocalIndexType operator*() {return mOrigin + mOffsets[mCurrent];}

    //! Return the boundary type of the origin
    BoundaryType originBoundaryType() const {return boundaryType(mCoords[0],mCoords[1],mCoords[2]);}

    //! Return the boundary type of the current index
    BoundaryType currentBoundaryType() const {return boundaryType(mCoords[0] + mNeighbors[3*mCurrent + 0],
                                                                  mCoords[1] + mNeighbors[3*mCurrent + 1],
                                                                  mCoords[2] + mNeighbors[3*mCurrent + 2]);}


 private:
    iterator(LocalIndexType v, LocalIndexType dim[3],int8_t* neighbors,SignedLocalIndexType* offsets, uint8_t count);

    bool inside(uint8_t);

    //! Return the boundary type encoded as a uint8_t
    BoundaryType boundaryType(SignedLocalIndexType x, SignedLocalIndexType y, SignedLocalIndexType z) const;

    int8_t* mNeighbors;
    SignedLocalIndexType* mOffsets;

    LocalIndexType mOrigin;
    SignedLocalIndexType mCoords[3];

    uint8_t mCount;
    uint8_t mCurrent;

    LocalIndexType mDim[3];
  };

  Neighborhood(LocalIndexType dim[3]);

  ~Neighborhood() {}

  iterator begin(LocalIndexType origin);
  iterator end(LocalIndexType origin);

protected:

  int8_t* mNeighbors;
  SignedLocalIndexType* mOffsets;
  uint8_t mCount;

  LocalIndexType mDim[3];

  void computeOffsets();
};


#endif /* NEIGHBORHOOD_H_ */
