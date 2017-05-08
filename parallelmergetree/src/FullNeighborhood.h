#ifndef FULLNEIGHBORHOOD_H_
#define FULLNEIGHBORHOOD_H_

#include "Neighborhood.h"

class FullNeighborhood : public Neighborhood
{
public:

  FullNeighborhood(LocalIndexType dim[3]);

  ~FullNeighborhood();
};




#endif /* FULLNEIGHBORHOOD_H_ */
