/*
 * SortedUnionFindAlgorithm.h
 *
 *  Created on: Feb 3, 2015
 *      Author: bremer5
 */

#ifndef SORTEDUNIONFINDALGORITHM_H
#define SORTEDUNIONFINDALGORITHM_H

#include <cassert>

#include "DataFlow/Controller.h"

#include "TypeDefinitions.h"
#include "Comparison.h"


//! Construct a local merge tree using a sorted union find approach
/*! This function expects a pointer to a local data block as input
 *  as well as the local bounding box and produce an augmented merge
 *  tree as well as a boundary merge tree as output.
 *
 * @param inputs input[0] is a DataBlock encoded by the make_local_block call
 * @param outputs output[0] the augmented merge tree of the block
 *                output[1] the boundary merge tree
 * @param task
 * @return 1 if successful; 0 otherwise
 */
int sorted_union_find_algorithm(std::vector<DataBlock>& inputs, 
                                std::vector<DataBlock>& outputs, TaskId task);

//! Create a DataBlock from a given pointer and bounding box
/*! This function encodes all necessary information for a local
 *  computation into a DataBlock to be passed into a task. Note
 *  that,the data will be passed as pointer to an array and will
 *  *not* be copied. As such the caller must ensure that the
 *  pointer remains valid until the corresponding task has completed
 *
 * @param data Pointer to a block of data
 * @param low Left lower corner of the bounding box describing this block
 * @param high Right upper corner just outside of the block
 * @param threshold The threshold below/above which data will be ignored
 * @return The DataBlock encoding this information
 */
DataBlock make_local_block(FunctionType* data, 
                           GlobalIndexType low[3], GlobalIndexType high[3], 
                           FunctionType threshold);




#endif /* SORTEDUNIONFINDALGORITHM_H_ */
