/*
 * BoundaryTreeExtractor.h
 *
 *  Created on: Feb 6, 2015
 *      Author: bremer5
 */

#ifndef BOUNDARYTREEEXTRACTOR_H
#define BOUNDARYTREEEXTRACTOR_H

#include "MergeTree.h"
#include "DataFlow/Task.h"
#include "Comparison.h"
#include <cassert>


//! Extract the boundary tree from the source according to the boundary flags of its nodes
/*! Given a *sorted* merge tree with boundary flags set appropriately this function
 *  extracts the corresponding boundary merge tree and put it into sink. Sorted in this
 *  case means that the mNodes array of either tree is expected to be sorted in "descending"
 *  order.
 *
 * @param source The sorted input tree
 * @param sink The output tree (assumed to be empty)
 * @return 1 if successful; 0 otherwise
 */
int extract_boundary_tree(const MergeTree& source, MergeTree&  sink, TaskId task);


#endif /* BOUNDARYTREEEXTRACTOR_H_ */
