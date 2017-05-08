/*
 * LocalCorrectionAlgorithm.h
 *
 *  Created on: Feb 13, 2015
 *      Author: landge1
 */

#ifndef LOCALCORRECTIONALGORITHM_H
#define LOCALCORRECTIONALGORITHM_H

#include <cassert>
#include <vector>
#include "TreeNode.h"
#include "Comparison.h"
#include "DataFlow/Controller.h"
#include "TypeDefinitions.h"

class SameTreeNodeComp
{
public:
  SameTreeNodeComp(CmpType cmp):mCmp(cmp) {}
  ~SameTreeNodeComp() {}

  bool operator()( const TreeNode A, const TreeNode B) const  {
    if (A.value() != B.value())
      return (mCmp)(A.value(), B.value());
    else // TODO Understand why this is equal
      return (A.id() < B.id());
  }
private:
  CmpType mCmp;
};

//! Constructs the correct local tree restricted to the block of space associated
/*  with this task. Takes as input 
 *  1) the augmented tree from the join routine and
 *  2) the currect local tree
 *  Outputs the corrected local tree.  The ouput tree is not kept in sorted form
 *  i.e. the order of nodes in the mNodes vector for the output tree is not
 *  sorted based on function value.
 *  Note: This function modifies the current local tree and sends it as output.
 *  A new tree is not created.
*/
int local_correction_algorithm(std::vector<DataBlock>& inputs, 
                               std::vector<DataBlock>& outputs, TaskId task);


#endif /*LOCALCORRECTIONALGORITHM_H */

