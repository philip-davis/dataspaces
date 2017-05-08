/*
 * SortedJoinAlgorithm.h
 *
 *  Created on: Feb 13, 2015
 *      Author: landge1
 */

#ifndef SORTEDJOINALGORITHM_H
#define SORTEDJOINALGORITHM_H

#include <cassert>
#include <queue>
#include "TreeNode.h"
#include "Comparison.h"
#include "DataFlow/Controller.h"
#include "TypeDefinitions.h"

struct QElement_t {

  const TreeNode* node;
  int treeIdx;
};

typedef struct QElement_t QElement;

class NodeComp
{
public:
  NodeComp(CmpType cmp) : mCmp(cmp) {}
  ~NodeComp() {}

  bool operator()( const QElement B, const QElement A) const  {
    if (A.node->value() != B.node->value())
      return (mCmp)(A.node->value(), B.node->value());
    else if (A.node->id() != B.node->id())
      return (A.node->id() < B.node->id());
    else // TODO understand why we need SoS on tree index
      return (A.treeIdx > B.treeIdx);
  }

private:
  CmpType mCmp;
};


//! Constructs merge tree by joining the input trees. 
/*  The resultant tree is constructed by joining arcs from the input trees in a 
 *  descending order based on the lower function value node of every arc. 
*/
int sorted_join_algorithm(std::vector<DataBlock>& inputs, 
                          std::vector<DataBlock>& outputs, TaskId task);

#endif // SORTEDJOINALGORITHM_H

