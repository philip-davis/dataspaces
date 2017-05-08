/*
 * MergeTree.h
 *
 *  Created on: Jan 26, 2015
 *      Author: bremer5
 */

#ifndef MERGETREE_H
#define MERGETREE_H

#include <vector>
#include <string>

#include "DataFlow/Controller.h"

#include "TreeNode.h"
#include "Comparison.h"
#include "Neighborhood.h"

//! A basic merge tree implementation
/*! This class implements a basic merge tree with the main data
 *  structure being a vector of TreeNodes. Note that the structure
 *  should be kept as simple as possible to allow a fast and easy
 *  en- and decoding through memcopies. However, at this point it
 *  seems to onerous to actually make a merge tree into a POD if
 *  that is even possible
 */
class MergeTree
{
public:

  //! The global size of the grid
  static GlobalIndexType sDimension[3];

  //! The comparison operator
  static CmpType greater;

  //! Default constructor
  MergeTree();

  //! Default destructor
  virtual ~MergeTree() {};

  //! Set the global size of the grid
    /*
   * Index manipulations
   */

  void id(int i) {mId = i;}

  int id() {return mId;}

  //! Set the comparison operator as per merge tree or split tree
  static void setComparator(CmpType cmp) {
    greater = cmp;
  }

  //! Set the dimensions of the global domain
  static void setDimension(GlobalIndexType data_dim[3]) {
    sDimension[0] = data_dim[0];
    sDimension[1] = data_dim[1];
    sDimension[2] = data_dim[2];
  }

  //! The bounding box of the region of the domain that is being represented by
  //  this merge tree. Note that this region grows as the merge tree is joined
  //  with merge trees from neighboring boxes.
  //! Set the lower corner
  void low(GlobalIndexType l[3]); 

  //! Set the upper corner
   /* @param h
   */
  void high(GlobalIndexType h[3]);

  //! Get the bounding box for this tree
  void blockBoundary(GlobalIndexType low[3], GlobalIndexType high[3]) const;
 
  //! Determine on which boundaries the block lies wrt. to the global domain
  BoundaryType blockBoundaryType() const;

  //! Transform the index from global to local index space
  LocalIndexType local(GlobalIndexType i) const;

  //! Transform the index from local to global index space
  GlobalIndexType global(LocalIndexType i) const;
  
  //! Return the global id for the given coordinates
  GlobalIndexType global(GlobalIndexType x[3]) const;

  //! Add a node to the tree and return the corresponding pointer
  TreeNode* addNode(GlobalIndexType index, FunctionType value, BoundaryType t);

  //! Add an edge between to nodes
  void addEdge(TreeNode* upper, TreeNode* lower);

  //! Add an edge between to nodes
  void addEdge(LocalIndexType upper, LocalIndexType lower);

  //! Find a node by doing a reverse linear traversal in teh list
  TreeNode* findNode(GlobalIndexType id);

  //! Find node using the local index in the node vector
  TreeNode* getNode(LocalIndexType index);

 //! Remove a (regular) node and return its child
  TreeNode* removeNode(TreeNode* node);

  //! Determine whether a given node lies on a boundary
  bool boundary(const TreeNode* node);

  //! Read-only access to the nodes array
  const std::vector<TreeNode>& nodes() const { return mNodes;}

  //! Read/Write access to the nodes array
  std::vector<TreeNode>& modifyNodes() { return mNodes;}

  //! Encode the merge tree as a DataBlock
  DataBlock encode();

  //! Decode the merge tree from a DataBlock
  void decode(const DataBlock& data);

#if USE_TOPO_FILE_PARSER
  //! Output the merge tree as family file (must be compiled with
  //! topo file parser enabled
  void outputFeatureHierarchy(std::string output_file);
#endif

  //! Prune the regular nodes in the tree
  // Note that this creates holes in the mNodes vector
  void pruneRegularNodes();

  virtual void pruneBlockBoundaryExtremum();

  void createEdgeMap(std::map<GlobalIndexType, GlobalIndexType> &edge_map);
  
  //! Write tree to dot file
  void writeToFile(uint32_t id);

  //! Write tree arcs to binary file
  virtual void writeToFileBinary(uint32_t id);

protected:

  //! The (dynamic) array of nodes
  std::vector<TreeNode> mNodes;

  //! TreeId for the tree. Used for debugging
  int mId;

  //! The head of the list of empty spots in the nodes array
  LocalIndexType mEmpty;

  //! The left lower corner in space corresponding to this tree
  GlobalIndexType mLow[3];

  //! The right upper corner in space corresponding to this tree
  GlobalIndexType mHigh[3];

   //! Compactify the mNodes array by filling empty spots from the back
  void compactify();

  //! Helper function to move a node in the array
  void move(LocalIndexType from, LocalIndexType to);

  //! Helper function to remove an edge
  void removeEdge(TreeNode* upper, TreeNode* lower);

  //! Determine the size for the encode routine
  virtual LocalIndexType size() const;

};




#endif /* MERGETREE_H_ */
