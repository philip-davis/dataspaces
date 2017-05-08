/*
 * TreeNode.h
 *
 *  Created on: Jan 26, 2015
 *      Author: bremer5
 */

#ifndef TREENODE_H
#define TREENODE_H


#include <cstdlib>

#include "TypeDefinitions.h"

//! A TreeNode serves as the node in a merge tree
/*! A TreeNode serves as a node in a merge tree. Note that, this class
 *  should be a plain-old-datatype (POD) in order for us to safely use
 *  a memcopy to move arrays of nodes. Furthermore, the pointer
 *  arithmacy only works assuming a tightly packed layout
 *
 *  POD means:
 *    - no user defined constructor, copy operator, or destructor
 *    - no virtual functions
 *    - only value elements
 *    - no non-static members that are not PODs
 *    - no baseclass
 *    - no private or protected members
 */

class TreeNode
{
public:

  //! The global id of this node
  GlobalIndexType mId;

  //! The local index of this node
  LocalIndexType mIndex;

  //! The parent index
  LocalIndexType mUp;

  //! The next index
  LocalIndexType mNext;

  //! The down pointer
  LocalIndexType mDown;

  //! The function value of this node
  FunctionType mValue;

  //! Generic bit field to hold various flags
  uint8_t mBitField;

  // Even though all data is public we prefer to pretend it isn't to
  // enforce an interface

  //! The SoS enabled comparision operator
  bool operator<(const TreeNode& node) const;

  //! Return the id
  GlobalIndexType id() const {return mId;}

  //! Return the local index
  LocalIndexType index() const {return mIndex;}

  //! Return the up pointer
  const TreeNode* up() const {return (mUp == LNULL) ? NULL : (this + (int)(mUp - mIndex));}

  //! Return the up pointer
  TreeNode* up() { return (mUp == LNULL)? NULL : (this + (int)(mUp - mIndex));};

  //! Return the next pointer
  const TreeNode* next() const {return (mNext == LNULL) ? NULL : (this + (int)(mNext - mIndex));}

  //! Return the next pointer
  TreeNode* next() {return (mNext == LNULL) ? NULL : (this + (int)(mNext - mIndex));}

  //! Return the down pointer
  const TreeNode* down() const {return (mDown == LNULL) ? NULL : (this + (int)(mDown - mIndex));}

  //! Return the up pointer
  TreeNode* down() {return (mDown == LNULL) ? NULL : (this + (int)(mDown - mIndex));}

  //! Return the value of the node
  FunctionType value() const {return mValue;}

  //! Set the id
  void id(GlobalIndexType i) {mId = i;}

  //! Set the index
  void index(LocalIndexType i) {mIndex = i;}

  //! Set the up pointer
  void up(const TreeNode* u) {mUp = (u - this) + mIndex;}

  //! Set the up pointer
  void up(LocalIndexType u) {mUp = u;}

  //! Set the next pointer
  void next(const TreeNode* n) {mNext = (n - this) + mIndex;}

  //! Set the next pointer
  void next(LocalIndexType n) {mNext = n;}

  //! Set the down pointer
  void down(const TreeNode* d) {mDown = (d - this) + mIndex;}

  //! Set the down pointer
  void down(LocalIndexType d) {mDown = d;}

  //! Set the function values
  void value(FunctionType f) {mValue = f;}

  //! Determiner whether this node is regular
  bool regular() const;

  //TODO Add comment about what the boundary flag means
  //! Return the boundary flag
  bool boundary() const {return getBit(0);}

  //! Set the boundary flag
  void boundary(bool b) {setBit(0,b);}

  //! Return the saddle flag
  bool finalizedSaddle() const {return getBit(1);}

  //! Set the saddle flag
  void finalizedSaddle(bool b) {setBit(1,b);}

  //! Return the pruned flag
  bool pruned() const {return getBit(2);}

  //! Set the pruned flag
  void pruned(bool b) {setBit(2,b);}

  //! Return the block extremum flag. This flag is enabled if the node is a 
  // boundary extremum for the local block
  bool blockBoundaryExtremum() const {return getBit(3);}

  //! Set the block boundary extremum flag
  void blockBoundaryExtremum(bool b) {setBit(3,b);}

  //! Return the visited flag
  bool visited() const {return getBit(4);}

  //! Set the visited flag
  void visited(bool b) {setBit(4,b);}

  //! Return the visited flag
  bool saddle() const {return getBit(5);}

  //! Set the visited flag
  void saddle(bool b) {setBit(5,b);}

  //! Get the i'th bit
  bool getBit(uint8_t i) const {return ((mBitField & (1 << i)) != 0);}

  //! Set the i'th bit
  void setBit(uint8_t i, bool bit) {bit ? (mBitField |= (1 << i)) : mBitField &= ~(1 << i);}
};



#endif /* TREENODE_H_ */
