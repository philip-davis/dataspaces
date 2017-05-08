#ifndef UNIONFIND_H_
#define UNIONFIND_H_

#include <vector>
#include <map>
#include <cassert>
#include <stack>

#include "TypeDefinitions.h"

//! Standard union-find implementation
/*! This class implements a default union-find structure using an stl::map
 *  to maintain an index map from label values to local indices. This may
 *  not be the fastest implementation but it is convinient to maintain
 *  labels in a spase index space
 */
class UnionFind
{
public:

  //! Default constructor
  UnionFind() {}

  //! Default destructor
  ~UnionFind() {}

  //! Return the current representative of the given label
  GlobalIndexType rep(GlobalIndexType id);

  //! Add a label
  void addLabel(GlobalIndexType label);

  //! Combine the "from" label with the "to" label
  void mergeLabel(GlobalIndexType from, GlobalIndexType to);

private:

  //! The current representative of the i'th label
  std::vector<GlobalIndexType> mLabel;

  //! An index map to convert global label-indices into local mLabel indices
  std::map<GlobalIndexType,LocalIndexType> mIndexMap;
};


#endif /* UNIONFIND_H_ */
