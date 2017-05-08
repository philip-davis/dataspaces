/*
 * AugmentedMergeTree.h
 *
 *  Created on: Jan 30, 2015
 *      Author: bremer5
 */

#ifndef AUGMENTEDMERGETREE_H
#define AUGMENTEDMERGETREE_H

#include "MergeTree.h"


class AugmentedMergeTree : public MergeTree
{
public:

  //! Default constructor
  AugmentedMergeTree() : MergeTree(), mLabels(NULL) {
  
    mOrigLow[0] = mLow[0];
    mOrigLow[1] = mLow[1];
    mOrigLow[2] = mLow[2];

    mOrigHigh[0] = mHigh[0];
    mOrigHigh[1] = mHigh[1];
    mOrigHigh[2] = mHigh[2];
  }

  //! Default destructor
  virtual ~AugmentedMergeTree();

  //! Set the bounding box for the local block that corresponds to this tree
  void origLow(GlobalIndexType l[3]);

  //! Set the bounding box for the local block that corresponds to this tree
  void origHigh(GlobalIndexType h[3]);

  //! Returns if a node belongs to vertex that resides in the local block
  bool isLocal(GlobalIndexType i);

  //! The number of original samples in the block
  GlobalIndexType sampleCount() const {return ((mOrigHigh[0] - mOrigLow[0])+1) * 
                                              ((mOrigHigh[1] - mOrigLow[1])+1) * 
                                              ((mOrigHigh[2] - mOrigLow[2])+1);}

  //! Allocated the space for the labels
  void initialize() {mLabels = new GlobalIndexType[this->sampleCount()];}

  //! Return the reference to the label of index i
  GlobalIndexType& label(LocalIndexType i) {return mLabels[i];}

  //! Return the label of index i
  GlobalIndexType label(LocalIndexType i) const {return mLabels[i];}

  //! Encode the augmented merge tree as a DataBlock
  DataBlock encode();

  //! Decode the merge tree from a DataBlock
  void decode(const DataBlock& data);

  void pruneRegularNodes();

  void localMin(TreeNode* node) { mLocalMin = *node; }

  TreeNode* localMin() { return &mLocalMin;}

  virtual void pruneBlockBoundaryExtremum();

  //! Write tree arcs to binary file
  virtual void writeToFileBinary(uint32_t id);

  void computeSegmentation();

protected:

  //! The left lower corner of the local block corresponding to this tree
  GlobalIndexType mOrigLow[3];

  //! The right upper corner of the local block corresponding to this tree
  GlobalIndexType mOrigHigh[3];

  //! The minima node for the local block
  TreeNode mLocalMin;

  //! The array of labels
  GlobalIndexType* mLabels;

  //! The list of pairs from old labels to new label
  std::vector<GlobalIndexType> mHistory;

  //! Determine the size for the encode routine
  virtual LocalIndexType size() const;
};




#endif /* AUGMENTEDMERGETREE_H_ */
