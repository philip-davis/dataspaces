/*
 * Comparison.h
 *
 *  Created on: Feb 3, 2015
 *      Author: bremer5
 */
#ifndef COMPARISON_H
#define COMPARISON_H

#include "TreeNode.h"
#include <iostream>

//! Typedef for the comparison operator
typedef bool (*CmpType)(FunctionType x, FunctionType y);

//! The greater function to compute a merge tree
inline bool merge_tree_greater(FunctionType x, FunctionType y) {return x > y;}

//! The greater function to compute a split tree
inline bool split_tree_greater(FunctionType x, FunctionType y) {return x < y;}

class IndexComp
{
public:

  IndexComp(const FunctionType* data, CmpType comp) : mData(data), mComp(comp) {}
  ~IndexComp() {}

  //! Comparing two indices of a scalar field.
  /*! Compare the function value at two indices of a scalar field. Note
   *  that for efficiency reasons this does not explicitly tests for f(i) == f(j).
   *  However, the was the algorithm uses the comparison and because 
   *  stl::stable_sort is used we implicitly maintain the default SoS i < j
   *
   * @param i index 1
   * @param j index 2
   * @return 1 if f(i) < f(j) 0 otherwise
   */
  bool operator()(const GlobalIndexType& i, const GlobalIndexType& j) const {
    return (mComp)(mData[i],mData[j]);
  }

private:

  const FunctionType* mData;
  const CmpType mComp;
};


class TreeNodeComp
{
public:
  TreeNodeComp(CmpType cmp):mCmp(cmp) {}
  ~TreeNodeComp() {}

  bool operator()( const TreeNode A, const TreeNode B) const  {
    if (A.value() != B.value())
      return (mCmp)(A.value(), B.value());
    else // TODO Understand why this is equal
      return (A.id() <= B.id());
  }
private:
  CmpType mCmp;
};


#endif /*COMPARISON_H*/ 
