/*
 * MergeTree.cpp
 *
 *  Created on: Jan 26, 2015
 *      Author: bremer5
 */

#include <cstring>
#include <cassert>
#include <iostream>

#if USE_TOPO_FILE_PARSER
#include "TopologyFileParser/ClanHandle.h"
#include "TopologyFileParser/FamilyHandle.h"
#include "TopologyFileParser/SimplificationHandle.h"
#include "TopologyFileParser/FeatureElement.h"

using namespace TopologyFileFormat;
#endif

#include "MergeTree.h"

// Initializing static variables
CmpType MergeTree::greater = merge_tree_greater;
GlobalIndexType MergeTree::sDimension[] = {0};


MergeTree::MergeTree()
{
  mEmpty = LNULL;
  mLow[0] = mLow[1] = mLow[2] = LNULL;
  mHigh[0] = mHigh[1] = mHigh[2] = LNULL;
}


void MergeTree::low(GlobalIndexType l[3])
{
  mLow[0] = l[0];
  mLow[1] = l[1];
  mLow[2] = l[2];
}


void MergeTree::high(GlobalIndexType h[3])
{
  mHigh[0] = h[0];
  mHigh[1] = h[1];
  mHigh[2] = h[2];
}


void MergeTree::blockBoundary(GlobalIndexType low[3], 
                              GlobalIndexType high[3]) const 
{
  low[0] = mLow[0];
  low[1] = mLow[1];
  low[2] = mLow[2];

  high[0] = mHigh[0];
  high[1] = mHigh[1];
  high[2] = mHigh[2];
}


BoundaryType MergeTree::blockBoundaryType() const
{
  BoundaryType t(NO_BOUNDARY);

  if (mLow[0] == 0)
    t.t |= LOW_X_BOUNDARY;

  if (mHigh[0] == (sDimension[0]-1))
    t.t |= HIGH_X_BOUNDARY;

  if (mLow[1] == 0)
    t.t |= LOW_Y_BOUNDARY;

  if (mHigh[1] == (sDimension[1]-1))
    t.t |= HIGH_Y_BOUNDARY;

  if (mLow[2] == 0)
    t.t |= LOW_Z_BOUNDARY;

  if (mHigh[2] == (sDimension[2]-1))
    t.t |= HIGH_Z_BOUNDARY;

  return t;
}


LocalIndexType MergeTree::local(GlobalIndexType i) const
{
  LocalIndexType x[3];

  x[0] = i % sDimension[0];
  x[1] = (i / sDimension[0]) % sDimension[1];
  x[2] = i / (sDimension[0]*sDimension[1]);

  x[0] -= mLow[0];
  x[1] -= mLow[1];
  x[2] -= mLow[2];

  return (x[2]*((mHigh[1] - mLow[1])+1) + x[1])*((mHigh[0] - mLow[0])+1) + x[0];
}


GlobalIndexType MergeTree::global(LocalIndexType i) const
{
  GlobalIndexType x[3];

  x[0] = i % ((mHigh[0] - mLow[0])+1);
  x[1] = (i / ((mHigh[0] - mLow[0])+1)) % ((mHigh[1] - mLow[1])+1);
  x[2] = i / (((mHigh[0] - mLow[0])+1)*((mHigh[1] - mLow[1])+1));

  x[0] += mLow[0];
  x[1] += mLow[1];
  x[2] += mLow[2];
  
  return (x[2]*sDimension[1] + x[1])*sDimension[0] + x[0];
}


GlobalIndexType MergeTree::global(GlobalIndexType x[3]) const
{
  return (x[2]*sDimension[1] + x[1])*sDimension[0] + x[0];
}


TreeNode* MergeTree::addNode(GlobalIndexType index, FunctionType value, 
                             BoundaryType t)
{
  LocalIndexType i;

  if (mEmpty == LNULL) {
    i = mNodes.size();
    mNodes.push_back(TreeNode());
  }
  else {
    i = mEmpty;
    if (mNodes[mEmpty].mDown != LNULL) {
      mEmpty = mNodes[mEmpty].mDown;
      mNodes[mEmpty].up(LNULL);
    }
    else
      mEmpty = LNULL;
  }

  // We have to explicitly initialize a node since as a POD a
  // TreeNode is not supposed to have a non-trivial constructor
  mNodes[i].id(index);
  mNodes[i].value(value);
  mNodes[i].index(i);
  mNodes[i].mUp = LNULL;
  mNodes[i].mNext = i;
  mNodes[i].mDown = LNULL;
  mNodes[i].mBitField = 0;

  if (t.isBoundary()) {
    mNodes[i].boundary(true);
  }
  return &mNodes[i];
}


void MergeTree::addEdge(TreeNode* upper, TreeNode* lower)
{
  if (upper->down() == lower)
    return;

  if (upper->down() != NULL)
    removeEdge(upper,upper->down());

  upper->down(lower);

  if (lower->up() == NULL)
    lower->up(upper);
  else {
    upper->next(lower->up()->next());
    lower->up()->next(upper);
  }
}


TreeNode* MergeTree::findNode(GlobalIndexType id)
{
  std::vector<TreeNode>::reverse_iterator it;

  for (it=mNodes.rbegin();it!=mNodes.rend();it++) {
    if (it->id() == id)
      return &(*it);}

  return NULL;
}

TreeNode* MergeTree::getNode(LocalIndexType index)
{
  if (index < mNodes.size())
    return &mNodes[index];
  else
    return NULL;
}


TreeNode* MergeTree::removeNode(TreeNode* node)
{
  // Make sure that the node is regular
  //assert (node->down() != NULL);
  //assert (node->up() != NULL);
  //assert (node->up()->next() == node->up());

  TreeNode* up = NULL;
  TreeNode* down = NULL;

  if (node->up()!=NULL) {
    up = node->up();
    removeEdge(node->up(),node);
  }

  if (node->down()!=NULL) {
    down = node->down();
    removeEdge(node,node->down());
  }

  if (up !=NULL && down !=NULL)
    addEdge(up,down);

  node->up(LNULL);
  node->down(LNULL);
  node->next(LNULL);
  node->id(GNULL);

  // Guard against the pop_back resizing
  LocalIndexType down_index = LNULL;

  if (down != NULL)
    down_index = down->index();

  //if (node->index() != mNodes.size()-1) {
    if (mEmpty == LNULL) {
      mEmpty = node->index();
      mNodes[mEmpty].id(GNULL);
      mNodes[mEmpty].up(LNULL);
      mNodes[mEmpty].down(LNULL);
      node->index(LNULL);
    }
    else {
      mNodes[mEmpty].up(node->index());
      mNodes[node->index()].down(mEmpty);
      mEmpty = node->index();

      mNodes[mEmpty].up(LNULL);
      mNodes[mEmpty].id(GNULL);
      mNodes[mEmpty].index(LNULL);
    }
  //}
  //else
    //mNodes.pop_back();

  if (down_index != LNULL)
    return &mNodes[down_index];
  else
    return NULL;
}


bool MergeTree::boundary(const TreeNode* node) {

  GlobalIndexType coordinate[3];
  GlobalIndexType id = node->id();

  coordinate[0] = id % sDimension[0];
  coordinate[1] = (id / sDimension[0]) % sDimension[1];
  coordinate[2] = id / (sDimension[0]*sDimension[1]);


  if (coordinate[0] != 0 && coordinate[0] != sDimension[0]-1) {
    if (coordinate[0] == mLow[0] || coordinate[0] == mHigh[0])
      return true;
  }
  if (coordinate[1] != 0 && coordinate[1] != sDimension[1]-1) {
    if (coordinate[1] == mLow[1] || coordinate[1] == mHigh[1])
      return true;
  }
  if (coordinate[2] != 0 && coordinate[2] != sDimension[2]-1) {
    if (coordinate[2] == mLow[2] || coordinate[2] == mHigh[2])
      return true;
  }
  
  return false;
}


DataBlock MergeTree::encode()
{
  DataBlock block;

  // Make sure we have no empty spaces
  compactify();

  // Compute the size of the encoded tree
  block.size = size();

  // Allocate the buffer
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

  return block;
}


void MergeTree::decode(const DataBlock& data)
{
  GlobalIndexType* head = (GlobalIndexType*)data.buffer;

  mLow[0] = head[0];
  mLow[1] = head[1];
  mLow[2] = head[2];

  mHigh[0] = head[3];
  mHigh[1] = head[4];
  mHigh[2] = head[5];

  mNodes.resize(head[6]);

  memcpy(&mNodes[0],(char*)data.buffer+7*sizeof(GlobalIndexType),
         mNodes.size()*sizeof(TreeNode));
}


#if USE_TOPO_FILE_PARSER
void MergeTree::outputFeatureHierarchy(std::string output_file)
{
  // Create a vector of features large enough to hold all nodes
  std::vector<FeatureElement> features(mNodes.size());
  std::vector<GlobalIndexType> ids(mNodes.size());
  std::vector<TreeNode>::const_iterator it;
  FunctionType low,high;

  LocalIndexType i=0;

  bool ascending = true;
  if ((mNodes.size() > 1) && (mNodes[1] < mNodes[0]))
    ascending = true;
  else
    ascending = false;

  compactify();

  low = 10e34;
  high = -10e34;
  for (it=mNodes.begin();it!=mNodes.end();it++,i++) {
    features[i].direction(ascending);
    if (it->down() != NULL) {
      features[i].lifeTime(it->down()->value(),it->value());
      features[i].addLink(it->down()->index());
    }
    else
      features[i].lifeTime(it->value(),it->value());

    ids[i] = it->id();

    low = std::min(low,it->value());
    high = std::max(high,it->value());
  }

  ClanHandle clan(output_file);
  FamilyHandle family;
  SimplificationHandle simp;
  IndexHandle index;

  //index.encoding(false);

  Data<GlobalIndexType> tmp(&ids);
  index.setData(&tmp);

  simp.indexHandle(index);
  Data<FeatureElement> tmp2(&features);
  simp.setData(&tmp2);
  //simp.encoding(false);
  simp.setRange(low,high);

  family.add(simp);
  family.range(low,high);

  clan.add(family);
  clan.write();

}
#endif

void MergeTree::compactify()
{
  while (mEmpty != LNULL) {
    
    LocalIndexType tmp = mNodes[mEmpty].mDown;
    
    if (mNodes.back().id() != GNULL) {
      
      assert(mNodes.size()-1 > mEmpty);
      move(mNodes.size()-1, mEmpty);
      mEmpty = tmp;
      mNodes.pop_back();
      
      if (mEmpty != LNULL) 
        mNodes[mEmpty].up(LNULL);
    }
    else {
      // the last node is empty and is in the empty doubly-list. So, we remove
      // the last node from the list and from the vector
      LocalIndexType last_index = mNodes.size()-1;
      LocalIndexType down_index = mNodes[last_index].mDown;
      LocalIndexType up_index = mNodes[last_index].mUp;
      
      if (mNodes[last_index].mUp != LNULL ) {

        if (down_index != LNULL) 
          mNodes[down_index].up(mNodes[last_index].mUp);
        mNodes[up_index].down(down_index);
      }
      else {
        // if last_index up is NULL then we are mEmpty
        if (down_index != LNULL)
          mNodes[down_index].up(LNULL);
        mEmpty = down_index;
      }
      
      mNodes.pop_back();
    }
  }

  for (int i=0; i<mNodes.size(); i++) {
    if (mNodes[i].id() == GNULL)
      assert(mNodes[i].id() != GNULL);
  }
}


void MergeTree::move(LocalIndexType from, LocalIndexType to)
{
  // If we have a down pointer we might have to fix an up pointer
  if ((mNodes[from].mDown != LNULL) && (mNodes[from].down()->mUp == from))
    mNodes[from].down()->up(to);

  // If we have siblings we must change one of them
  if (mNodes[from].mNext != from) {
    LocalIndexType next = mNodes[from].mNext;
    
    while (mNodes[next].mNext != from){
      next = mNodes[next].mNext ;
    }

    mNodes[next].next(to);
  }

  // If we have one or more parents
  if (mNodes[from].mUp != LNULL) {
    LocalIndexType up = mNodes[from].mUp;

    do {
      mNodes[up].mDown = to;

      up = mNodes[up].mNext;
    } while (up != mNodes[from].mUp);
  }

  mNodes[to] = mNodes[from];
  mNodes[to].mIndex = to;

  // If this node has no sibling we set the next to itself
  if (mNodes[from].mNext == from)
    mNodes[to].next(to);

  return;
}


void MergeTree::removeEdge(TreeNode* upper, TreeNode* lower)
{
  assert (upper->down() == lower);

  if (upper->next() == upper) {
    lower->up(LNULL);
  }
  else {
    TreeNode* next = upper->next();
    while (next->next() != upper) {
      next = next->next();
    }

    next->next(upper->next());

    upper->next(upper);

    lower->up(next);
  }

  upper->down(LNULL);
}


LocalIndexType MergeTree::size() const
{
  LocalIndexType s;

  // The low and high corners
  s = 6*sizeof(GlobalIndexType);

  // plus one index for the number of nodes
  s += sizeof(GlobalIndexType);

  // plus the nodes array
  s += (mNodes.size()) * sizeof(TreeNode);

  return s;

}

// This does not modify the nodes vector but only changes the connectivity of
// the nodes
void MergeTree::pruneRegularNodes() {

  std::vector<TreeNode>::iterator it;

  for (it=mNodes.begin(); it!=mNodes.end(); it++) {
    if (it->regular()){
      this->removeNode(&(*it));
    }
  }
}

void MergeTree::createEdgeMap(std::map<GlobalIndexType, GlobalIndexType> &edge_map) {

  std::vector<TreeNode>::iterator it;
 
  for (it=mNodes.begin(); it != mNodes.end(); it++) {
    if (it->id()!=GNULL && it->down() != NULL) {
      edge_map[it->id()] = it->down()->id();
    }
  }
}

void MergeTree::writeToFile(uint32_t id){

  char file_name[32];
  sprintf(file_name, "mt_%d.dot", id);
  FILE *fp = fopen(file_name, "w");

  std::vector<TreeNode>::iterator it;
 
  fprintf(fp, "digraph G {\n");
  for (it=mNodes.begin(); it != mNodes.end(); it++) {
    if (it->id()!=GNULL && it->down() != NULL) {
      fprintf(fp, "%llu->%llu\n", it->id(), it->down()->id());
//      fprintf(fp, "%llu [label=\"%llu, %.10lf\"]\n", it->id(), 
//                                                 it->id(), it->value());
//      if (it->blockBoundaryExtremum()) 
//        fprintf(fp, "%llu [color = \"red\"]\n", it->id());
//
//      if (it->boundary())
//        fprintf(fp, "%llu [style=filled, fillcolor = \"green\"]\n", it->id());
//
//      if (it->finalizedSaddle())
//        fprintf(fp, "%llu [style=filled, fillcolor = \"blue\"]\n", it->id());
//
//      if (it->saddle())
//        fprintf(fp, "%llu [style=filled, fillcolor = \"yellow\"]\n", it->id());
//
//      fprintf(fp, "%llu [label=\"%llu, %.10lf\"]\n", it->down()->id(), 
//                            it->down()->id(), it->down()->value()); 
//      
//      if (it->down()->blockBoundaryExtremum()) 
//        fprintf(fp, "%llu [color = \"red\"]\n", it->down()->id());
//      
//      if (it->down()->boundary())
//        fprintf(fp, "%llu [style=filled, fillcolor = \"green\"]\n", it->down()->id());
//      
//      if (it->down()->finalizedSaddle())
//        fprintf(fp, "%llu [style=filled, fillcolor = \"blue\"]\n", it->down()->id());
//      
//      if (it->down()->saddle())
//        fprintf(fp, "%llu [style=filled, fillcolor = \"yellow\"]\n", it->down()->id());
    }
    //else if (it->up() == NULL) {
    //  fprintf(fp, "%llu\n", it->id());
    //  fprintf(fp, "%llu [label=\"%llu, %.10lf\"]\n", it->id(), 
    //                                             it->id(), it->value());
    //}
  }
  fprintf(fp, "}\n");

  fclose(fp);
}


void MergeTree::pruneBlockBoundaryExtremum()
{
  std::vector<TreeNode>::iterator it;

  for (it=mNodes.begin(); it!=mNodes.end(); it++) {
    if (it->regular() && it->blockBoundaryExtremum() && !(it->saddle() || it->finalizedSaddle())){
      this->removeNode(&(*it));
    }
  }
  
}

void MergeTree::writeToFileBinary(uint32_t id){

  char file_name[32];
  sprintf(file_name, "mt_%d.dat", id);
  FILE *fp = fopen(file_name, "wb");

  this->pruneBlockBoundaryExtremum();

  std::vector<TreeNode>::iterator it;

  for (it=mNodes.begin(); it != mNodes.end(); it++) {
    if ((it->down() != NULL) && (it->id()!=GNULL)) {
      GlobalIndexType arc[2];
      arc[0] = it->id();
      arc[1] = it->down()->id();
      fwrite(arc, sizeof(GlobalIndexType), 2, fp);
    }
  }

  fclose(fp);
}
