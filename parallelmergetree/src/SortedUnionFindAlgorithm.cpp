/*
 * SortedUnionFindAlgorithm.cpp
 *
 *  Created on: Feb 3, 2015
 *      Author: bremer5
 */

#include <vector>
#include <algorithm>
#include <iostream>
#include <unordered_map>

#include "SortedUnionFindAlgorithm.h"
#include "AugmentedMergeTree.h"
#include "FullNeighborhood.h"
#include "UnionFind.h"
#include "BoundaryTreeExtractor.h"

//! The number of bits used for prefixing scatter tasks
static const uint8_t sPrefixSize = 4;

//! The number of non-prefix bits
static const uint8_t sPostfixSize = sizeof(TaskId)*8 - sPrefixSize;

//! Bit mask for scatter tasks
static const TaskId sPrefixMask = ((1 << sPrefixSize) - 1) << sPostfixSize;


DataBlock make_local_block(FunctionType* data, GlobalIndexType low[3], 
                           GlobalIndexType high[3], FunctionType threshold)
{
  DataBlock block;

  block.size = sizeof(FunctionType*) + 6*sizeof(GlobalIndexType) 
               + sizeof(FunctionType);
  block.buffer = new char[block.size];

  *(FunctionType**)block.buffer = data;

  GlobalIndexType* tmp = (GlobalIndexType*)((char*)block.buffer + 
                                             sizeof(FunctionType*));

  tmp[0] = low[0];
  tmp[1] = low[1];
  tmp[2] = low[2];

  tmp[3] = high[0];
  tmp[4] = high[1];
  tmp[5] = high[2];

  FunctionType* t = (FunctionType*)((char*)block.buffer + sizeof(FunctionType*) 
                                    + 6*sizeof(GlobalIndexType));

  t[0] = threshold;

  return block;
}

void decode_local_block(const DataBlock& block, FunctionType** data, 
                        GlobalIndexType low[3], GlobalIndexType high[3], 
                        FunctionType& threshold)
{
  *data = *(FunctionType**)block.buffer;

  GlobalIndexType* tmp = (GlobalIndexType*)((char*)block.buffer + 
                                             sizeof(FunctionType*));

  low[0] = tmp[0];
  low[1] = tmp[1];
  low[2] = tmp[2];

  high[0] = tmp[3];
  high[1] = tmp[4];
  high[2] = tmp[5];

  FunctionType* t = (FunctionType*)((char*)block.buffer + sizeof(FunctionType*) 
                                    + 6*sizeof(GlobalIndexType));

  threshold = t[0];
}


int sorted_union_find_algorithm(std::vector<DataBlock>& inputs, 
                                std::vector<DataBlock>& outputs, TaskId task)
{
  assert (inputs.size() == 1);

  FunctionType* data;
  GlobalIndexType low[3];
  GlobalIndexType high[3];
  FunctionType threshold;

  decode_local_block(inputs[0],&data,low,high,threshold);

  // Create the augmented merge tree
  AugmentedMergeTree tree;
  CmpType greater = AugmentedMergeTree::greater;

  tree.low(low);
  tree.high(high);
  tree.origLow(low);
  tree.origHigh(high);
  tree.initialize(); // Make sure we allocate the storage for the labels
  tree.id(task & ~sPrefixMask);

  std::vector<LocalIndexType> order;
  std::vector<LocalIndexType>::iterator oIt;

  // TODO Potentially change initial order to be consistent with "standard" index SoS
  for (LocalIndexType i=0;i<tree.sampleCount();i++) {
    tree.label(i) = GNULL;
    if (greater(data[i],threshold)) {
      order.push_back(i);
    }
  }
  
  // Sort all the vertices above the threshold by descending order.
  IndexComp sort_comp(data,greater);
  std::stable_sort(order.begin(),order.end(),sort_comp);

  TreeNode local_min;
  if (!order.empty()) {
    local_min.id(tree.global(order.back()));
    local_min.value(data[order.back()]);
    local_min.up(LNULL);
    local_min.down(LNULL);
    local_min.next(LNULL);
    tree.localMin(&local_min);
  }

  // Get a neighborhood iterator
  LocalIndexType local_dim[3];
  local_dim[0] = high[0] - low[0] + 1;
  local_dim[1] = high[1] - low[1] + 1;
  local_dim[2] = high[2] - low[2] + 1;

  FullNeighborhood neighborhood(local_dim);
  FullNeighborhood::iterator it;

  // Create a local union find of labels
  UnionFind uf;

  // Some temporary variables
  GlobalIndexType neigh_label;
  TreeNode* new_node;
  TreeNode* old_node;
  TreeNode* neigh_node;
  bool restricted_extremum;
  bool minimum;
  

  // map of nodes from Global to local
  std::unordered_map<GlobalIndexType, LocalIndexType> tree_map;

  // The boundary of this block wrt. the global domain
  BoundaryType block_boundary_type = tree.blockBoundaryType();

  // Boundary type of the origin
  BoundaryType origin_boundary_type;

  // Boundary type of the currently active neighbor
  BoundaryType neigh_boundary_type;

  // For all vertices in descending order
  for (oIt=order.begin();oIt!=order.end();oIt++) {

    // Initialize neighborhood iterator
    it = neighborhood.begin(*oIt);

    // Get the boundary type of the current center but ignore all boundary
    // flags that are due to the global boundary
    origin_boundary_type = it.originBoundaryType();
    origin_boundary_type.ignore(block_boundary_type);

    // Reset the restricted extremum flag
    restricted_extremum = true;
    minimum = true;

    // For all neighbors
    while (it!=neighborhood.end(*oIt)) {

      if (tree.label(*it) != GNULL) { // If the neighbor has already been 
                                      // labeled it is considered higher

        // Get the boundary type of the current neighbor but ignore flags
        // due to the global boundary
        neigh_boundary_type = it.currentBoundaryType();
        neigh_boundary_type.ignore(block_boundary_type);

        // If this neighbor is part of the same boundary component it means
        // this vertex cannot be a restricted extremum
        if (origin_boundary_type.contains(neigh_boundary_type))
          restricted_extremum = false;

        neigh_label = uf.rep(tree.label(*it)); // Find its current active label

        if (tree.label(*oIt) == GNULL) {// If this is the first label we see
          tree.label(*oIt) = neigh_label; // We pass on this label
        }
        else if (neigh_label != tree.label(*oIt)) { // If we see a second 
                                                    // label *oIt is a saddle

          // If the node corresponding to our current label is not *oIt itself
          // then we have not yet created a critical point for *oIt
          if (tree.label(*oIt) != tree.global(*oIt)) {

            // Add a new node into the tree and use its id as label
            new_node = tree.addNode(tree.global(*oIt),data[*oIt],
                                    origin_boundary_type);
            // node to the tree map
            tree_map[tree.global(*oIt)] = *oIt;


            // Find the corresponding old node
            old_node = tree.findNode(tree.label(*oIt));

            // Now set the pointer for the node corresponding to the current label
            tree.addEdge(old_node,new_node);

            // Create a corresponding UF label
            uf.addLabel(new_node->id());

            // And merge the two labels making sure the later one survives
            uf.mergeLabel(old_node->id(),new_node->id());

            // And update our own label
            tree.label(*oIt) = new_node->id();

          }

          // The above if statement took care of the first arc that reached *oIt.
          // Now we take care of the second arc with neigh_label

          // Find the neighboring node
          neigh_node = tree.findNode(neigh_label);

          // And find the saddle (if this is a multi-saddle the above if will not
          // catch and thus new_node might be uninititalized. However, *oIt should
          // be the last node that got created
          new_node = tree.findNode(tree.label(*oIt));

          // Set the appropriate down pointer
          tree.addEdge(neigh_node,new_node);

          // Now we merge the labels
          uf.mergeLabel(neigh_label,tree.label(*oIt));

        } // end-if we see a second/third/... label
      } // end-if we found a labeled neighbor
      else { 
        // if we have a neighbor that is not labeled (and thus below us) but above
        // the threshold then this vertex is not a minimum
        if (greater(data[*it], threshold))
            minimum = false;
      }

      it++;
    } // end-while for all neighbors

    if (tree.label(*oIt) == GNULL) { // If we have not found a higher neighbor

      // Add a new node into the tree and use its id as label
      new_node = tree.addNode(tree.global(*oIt),data[*oIt],origin_boundary_type);

      // Add the label to the UF
      uf.addLabel(new_node->id());

      // Set its id
      tree.label(*oIt) = new_node->id();
    }
    // If this is a restricted extremum and not already a critical point
    else if (restricted_extremum && (tree.label(*oIt) != tree.global(*oIt))) {
      // Make it a valence two node in the tree

      // Add a new node into the tree and use its id as label
      new_node = tree.addNode(tree.global(*oIt),data[*oIt],origin_boundary_type);

      // Add the label to the UF
      uf.addLabel(new_node->id());

      // Find the corresponding old node
      old_node = tree.findNode(tree.label(*oIt));

      // Add the arc to the tree
      tree.addEdge(old_node,new_node);

      // Merge the union find labels
      uf.mergeLabel(old_node->id(),new_node->id());

      // Correct the label
      tree.label(*oIt) = new_node->id();
    }
    // If this is a minimum
    else if (minimum) {


      // we check if this node is already present in the tree. This can happen
      // if the node is a saddle and minima at the same time. Common case when
      // thresholding is enabled. If the node exists we do nothing.
      // We may have multiple minima being added as regular nodes hence a prune
      // is required at the end.
      new_node = tree.findNode(tree.global(*oIt));

      if (new_node == NULL) {
        // Add a new node into the tree and use its id as label
        new_node = tree.addNode(tree.global(*oIt),data[*oIt],origin_boundary_type);

        // we reset the boundary flag as this is a minima and should get pruned
        // Due to the boundary we get multiple minima
        new_node->boundary(false);

        // Add the label to the UF
        uf.addLabel(new_node->id());

        // Find the corresponding old node
        old_node = tree.findNode(tree.label(*oIt));

        // Add the arc to the tree
        tree.addEdge(old_node,new_node);

        // Merge the union find labels
        uf.mergeLabel(old_node->id(),new_node->id());

        // Correct the label
        tree.label(*oIt) = new_node->id();
      }
    }
    
  } // end-for all vertices in sorted order

  // TODO Move up to avoid traversal
  // marking the boundary max as blockBoundaryExtremum. These are used for 
  // starting the traversal in the correction phase.
  std::vector<TreeNode>::iterator lIt;
  for (lIt=tree.modifyNodes().begin(); lIt!=tree.modifyNodes().end(); lIt++) {
  
    if (lIt->boundary())
      lIt->blockBoundaryExtremum(true);
  }

  //tree.writeToFile(task+100);

  // As multiple minima may get added as regular nodes due to decomposition of
  // the domain, we need to prune the tree
  tree.pruneRegularNodes();
  //if (task==623)
  //tree.writeToFile(task);

  MergeTree boundary_tree;

  extract_boundary_tree(tree, boundary_tree, task);
  outputs[1] = tree.encode();


  outputs[0] = boundary_tree.encode();
  //boundary_tree.writeToFile(task+1000);

  //std::cerr << "Task : " << task << " local min : " << tree.localMin()->id() 
  //          << " value : " << tree.localMin()->value() << "\n";
  
  //if (task==33)
  //boundary_tree.writeToFile(task+100000);
  //std::cerr << "Task : " << task << " : Done with sorted union find algorithm\n";

  return 1;
}



