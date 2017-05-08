/*
 * AugmentedMergeTree.cpp
 *
 *  Created on: Jan 31, 2015
 *      Author: bremer5
 */

#include "AugmentedMergeTree.h"
#include <unordered_map>
#include "string.h"
#include <cassert>

void AugmentedMergeTree::origLow(GlobalIndexType l[3])
{
  mOrigLow[0] = l[0];
  mOrigLow[1] = l[1];
  mOrigLow[2] = l[2];
}

void AugmentedMergeTree::origHigh(GlobalIndexType h[3])
{
  mOrigHigh[0] = h[0];
  mOrigHigh[1] = h[1];
  mOrigHigh[2] = h[2];
}

bool AugmentedMergeTree::isLocal(GlobalIndexType i) {

  LocalIndexType x[3];

  x[0] = i % sDimension[0];
  x[1] = (i / sDimension[0]) % sDimension[1];
  x[2] = i / (sDimension[0]*sDimension[1]);

  if ((x[0] < mOrigLow[0]) || (x[0] > mOrigHigh[0]))
    return false;
  if ((x[1] < mOrigLow[1]) || (x[1] > mOrigHigh[1]))
    return false;
  if ((x[2] < mOrigLow[2]) || (x[2] > mOrigHigh[2]))
    return false;

  return true;
}


AugmentedMergeTree::~AugmentedMergeTree()
{
  if (mLabels != NULL)
      delete[] mLabels;
}

DataBlock AugmentedMergeTree::encode()
{
  DataBlock block;
  LocalIndexType base_size;

  MergeTree::compactify();

  block.size = size();

  block.buffer = new char[block.size];
  GlobalIndexType* head = (GlobalIndexType*)block.buffer;

  head[0] = mLow[0];
  head[1] = mLow[1];
  head[2] = mLow[2];

  head[3] = mHigh[0];
  head[4] = mHigh[1];
  head[5] = mHigh[2];

  head[6] = mNodes.size();

  memcpy((char*)block.buffer+7*sizeof(GlobalIndexType),&mNodes[0],
         mNodes.size()*sizeof(TreeNode));
  
  base_size = MergeTree::size();
  
  GlobalIndexType* buff = (GlobalIndexType*)((char*)block.buffer+base_size);
  buff[0] = mOrigLow[0];
  buff[1] = mOrigLow[1];
  buff[2] = mOrigLow[2];
  buff[3] = mOrigHigh[0];
  buff[4] = mOrigHigh[1];
  buff[5] = mOrigHigh[2];

  base_size += sizeof(GlobalIndexType)*6;

  *(TreeNode*)((char*)block.buffer+base_size) = mLocalMin;

  base_size += sizeof(TreeNode);

  memcpy((char*)block.buffer+base_size, mLabels,
         sampleCount()*sizeof(GlobalIndexType));

  base_size += sampleCount()*sizeof(GlobalIndexType);
  memcpy((char*)block.buffer+base_size, &mHistory[0],
         mHistory.size()*sizeof(GlobalIndexType));

  return block;
}

void AugmentedMergeTree::decode(const DataBlock& data)
{
  MergeTree::decode(data);

  LocalIndexType base_size;
  base_size = MergeTree::size();

  GlobalIndexType* buff = (GlobalIndexType*)((char*)data.buffer+base_size);
  mOrigLow[0] = buff[0];
  mOrigLow[1] = buff[1];
  mOrigLow[2] = buff[2];
  mOrigHigh[0] = buff[3];
  mOrigHigh[1] = buff[4];
  mOrigHigh[2] = buff[5];

  base_size += sizeof(GlobalIndexType)*6;

  mLocalMin = *(TreeNode*)((char*)data.buffer + base_size);

  base_size += sizeof(TreeNode);

  // IN principle we could just use the pointer
  //mLabels = (GlobalIndexType*)((char*)data.buffer+base_size);

  mLabels = new GlobalIndexType[sampleCount()];
  memcpy(mLabels,(char*)data.buffer+base_size,
         sampleCount()*sizeof(GlobalIndexType));


  base_size += sampleCount()*sizeof(GlobalIndexType);

  if (data.size - base_size) {
    mHistory.resize((data.size - base_size)/sizeof(GlobalIndexType));

    memcpy(&mHistory[0],(char*)data.buffer+base_size, data.size - base_size);

   // std::cerr << "History Size:: " << mHistory.size() << "\n";
  }
  //else {
  //  std::cerr << "No history!\n ";
  //}
}

LocalIndexType AugmentedMergeTree::size() const
{
  LocalIndexType s;

  // Get the size of the base class
  s = MergeTree::size();

  // space to store the original block coordinates
  s += sizeof(GlobalIndexType)*6;

  // space to store the extrema id
  //s += sizeof(GlobalIndexType);

  // space to store the extrema value
  //s += sizeof(FunctionType);
  
  // space to store local min node
  s += sizeof(TreeNode);

  // space to store the labels
  s += sizeof(GlobalIndexType)*(mOrigHigh[0] - mOrigLow[0]+1)*
                               (mOrigHigh[1] - mOrigLow[1]+1)*
                               (mOrigHigh[2] - mOrigLow[2]+1);

  // space to store the history
  s += sizeof(GlobalIndexType)*mHistory.size();

  return s;
}

// This does not modify the nodes vector but only changes the connectivity of
// the nodes
void AugmentedMergeTree::pruneRegularNodes() {

  std::vector<TreeNode>::iterator it;

  for (it=mNodes.begin(); it!=mNodes.end(); it++) {
    if (it->regular() && 
        !(it->blockBoundaryExtremum() || it->saddle() || it->finalizedSaddle())){
      mHistory.push_back(it->id());
      mHistory.push_back(it->up()->id());
      this->removeNode(&(*it));
      continue;
    }
    if ((it->id() != GNULL) && (it->up() == NULL) && (it->down() == NULL)) {
      //std::cerr << "id : " << it->id() << " index: " << it->index() << "\n";
      mHistory.push_back(it->id());
      //mHistory.push_back(it->up()->id());
      mHistory.push_back(-1);
      this->removeNode(&(*it));
    }
  }
}

void AugmentedMergeTree::pruneBlockBoundaryExtremum()
{
  std::vector<TreeNode>::iterator it;

  for (it=mNodes.begin(); it!=mNodes.end(); it++) {
    if (it->regular() && it->blockBoundaryExtremum() && !(it->saddle() || it->finalizedSaddle())){
      mHistory.push_back(it->id());
      mHistory.push_back(it->up()->id());
      this->removeNode(&(*it));
    }
  }
  
}

void AugmentedMergeTree::writeToFileBinary(uint32_t id){

  char file_name[32];
  sprintf(file_name, "mt_%d.dat", id);
  FILE *fp = fopen(file_name, "wb");

  this->pruneBlockBoundaryExtremum();

  std::vector<GlobalIndexType> segmentation;
  for (int i=0; i<sampleCount(); i++) {
    if (mLabels[i] != GNULL) {
      segmentation.push_back(global(i));
      segmentation.push_back(mLabels[i]);
    }
  }

  // First we store the size of the <vertex id, seg id> pairs
  GlobalIndexType size = segmentation.size();
  fwrite(&size, sizeof(GlobalIndexType), 1, fp);
  //std::cerr << "Size: " << sizeof(LocalIndexType) << "\n";

  // now we store the segmentation
  if (size > 0)
    fwrite((char*)&segmentation[0], sizeof(GlobalIndexType), segmentation.size(),fp);

  //for (int i=0; i<size; i++) {
   // std::cerr << "seg: " << segmentation[i] << "\n";
  //}

  // now we store the tree
  struct NodeValPair {
    GlobalIndexType node_id;
    FunctionType val;
  };

  NodeValPair node_val_pair[2];

  std::vector<TreeNode>::iterator it;

  for (it=mNodes.begin(); it != mNodes.end(); it++) {
    if ((it->down() != NULL) && (it->id()!=GNULL)) {
      //GlobalIndexType arc[2];
      node_val_pair[0].node_id = it->id();
      node_val_pair[0].val = it->value();
      node_val_pair[1].node_id = it->down()->id();
      node_val_pair[1].val = it->down()->value();
      fwrite(node_val_pair, sizeof(NodeValPair), 2, fp);
      //std::cerr << node_val_pair[0].node_id << "," << node_val_pair[0].val << " -> "
      //          << node_val_pair[1].node_id << "," << node_val_pair[1].val << "\n";
    }
  }

  fclose(fp);
}

void AugmentedMergeTree::computeSegmentation() {
  //std::cerr << "Start Computing Segmentation: " << this->id() << "\n";

  this->pruneBlockBoundaryExtremum();

  //for (int i=0; i<mHistory.size(); i=i+2) {
    //std::cerr << mHistory[i] << " -> " << mHistory[i+1] << "\n"; 
  //}

  // used for validation
  // create a node map and check if seg ids are nodes in the tree
  std::vector<TreeNode>::iterator it;

  std::unordered_map<GlobalIndexType, int> node_map;

  for (it=mNodes.begin(); it!=mNodes.end(); it++) {
    if (it->id() != GNULL) {
      node_map[it->id()] = 1;
      //std::cerr << "Node : " << it->id() << "\n";
    }
  }

  // Create a lookup for the history 
  std::unordered_map<GlobalIndexType, GlobalIndexType> history_map;
  for (int i=0; i<mHistory.size(); i=i+2) {
    history_map[mHistory[i]] = mHistory[i+1];
  }

  std::unordered_map<GlobalIndexType, int> seg_ids;

  // Update the labels
  for (int i=0; i<sampleCount(); i++) {
    
    GlobalIndexType curr_label = mLabels[i];
    while(history_map.find(curr_label) != history_map.end()) {
      curr_label = history_map[curr_label];
    }

    mLabels[i] = curr_label;
    if (seg_ids.find(curr_label) == seg_ids.end()) {
      seg_ids[curr_label] = 1;
      //std::cerr << "Seg id: " << curr_label << "\n";
    }

    //assert(node_map.find(curr_label) != node_map.end());
    //std::cerr << "vertex : " << global(i) << " : Seg id: " << curr_label << "\n";
  }
  
  //std::cerr << "Computed Segmentation: " << this->id() << "\n";

}

