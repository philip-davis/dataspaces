/*
 * BoundaryTreeExtractor.cpp
 *
 *  Created on: Feb 6, 2015
 *      Author: bremer5
 */

#include "BoundaryTreeExtractor.h"
#include <unordered_map>
#include <iostream>
#include <iomanip>

// Idea is to go over all nodes of the source tree in descending order. We add
// nodes to the boundary tree if they follow one of the following conditions
// 1. the node is parent of boundary node
// 2. the node itself is boundary node
// 3. the parent of the node is already present in the boundary tree
//
// As we add nodes in descending order, the nodes in boundary tree are 
// automatically sorted. The nodes vector may contain holes due to a prune. 


int extract_boundary_tree(const MergeTree& source_tree, MergeTree& boundary_tree,
                          TaskId task)
{
  // Iterator to go over source tree nodes
  std::vector<TreeNode>::const_iterator it;
  GlobalIndexType low[3], high[3];
  source_tree.blockBoundary(low, high);

  boundary_tree.low(low);
  boundary_tree.high(high);

  // map for the boundary tree
  std::unordered_map<GlobalIndexType, LocalIndexType> boundary_tree_map;

  // 'prev' is used to verify if the nodes are in the sorted order. Otherwise,
  // it has no contribution to core logic of this routine. As the nodes vector
  // may contain holes, 'prev' is used to keep track of the previous valid
  // location in the vector.
  TreeNode prev;
  prev.id(GNULL);
  TreeNodeComp node_comp(MergeTree::greater);

  // ! For all local tree nodes in descending order
  for (it = source_tree.nodes().begin(); it != source_tree.nodes().end(); it++) {
    
    TreeNode* new_node = NULL;
    const TreeNode* parent_node;
    bool parent_exists = false;

    // If this node has not been pruned
    if (it->id()!=GNULL) {

      // Check if input is sorted
      if (prev.id()!=GNULL && !node_comp(prev, *it)) {
        std::cerr << "ERROR in Boundary tree input : " 
                  << " node: " << prev.id()
                  << std::setprecision(12)
                  << " val: " << prev.value()
                  << " next node: " << it->id()
                  << std::setprecision(12)
                  << " val: " << it->value()
                  << " task: " << task
                  << "\n";
        if (prev.down()==NULL)
          std::cerr << "Node is Minima!\n";
        if (it->down()==NULL)
          std::cerr << "Next node is Minima!\n";

        assert(node_comp(prev, *it));
      }

      // Check if node is parent of boundary node or is itself boundary
      if (((it->down() != NULL) && it->down()->boundary()) || it->boundary()) {

        new_node = boundary_tree.addNode(it->id(), it->value(), it->boundary());
        boundary_tree_map[new_node->id()] = new_node->index();
      
      } // Check if parent exists in boundary tree and node is not regular
      else if (it->up() != NULL && !it->regular()) { 
        
        std::unordered_map<GlobalIndexType, LocalIndexType>::iterator mIt;
        parent_node = it->up();
        
        do {
          mIt = boundary_tree_map.find(parent_node->id());
          if (mIt != boundary_tree_map.end()) {
            parent_exists = true;
            break;
          }      
          parent_node = parent_node->next();

        } while (parent_node != it->up());
      
        if (parent_exists) {
          new_node = boundary_tree.addNode(it->id(), it->value(), it->boundary());
          boundary_tree_map[new_node->id()] = new_node->index();
        }
      }
      else {
        continue;
      }
      
      // If we have reached here we may have added a new node to the tree. 
      // We now have to attach it to its parents if it has a parent/s

      if (new_node!= NULL && it->up() != NULL) {
        
        std::unordered_map<GlobalIndexType, LocalIndexType>::iterator mIt;
        parent_node = it->up();
        int parent_count = 0;
        int parent_count_boundary_tree = 0;

        do {
          parent_count++;
          mIt = boundary_tree_map.find(parent_node->id());
          if (mIt != boundary_tree_map.end()) {
            boundary_tree.addEdge(boundary_tree.getNode(mIt->second), new_node);
            parent_count_boundary_tree++;
          }      
          parent_node = parent_node->next();

        } while (parent_node != it->up());

        // if all parents are not present in boundary tree, then this is a
        // finalized saddle and we set the saddle flag

        // we also mark this as finalized saddle
        if (parent_count != parent_count_boundary_tree)
          new_node->finalizedSaddle(true);

      }

      // if we have added a node we need to update the appropriate flags
      if (new_node!=NULL) {
        // If this node was already a finalized saddle we should flag it
        if (it->finalizedSaddle())
          new_node->finalizedSaddle(true);
      }

      prev = *it;
    } // endif not pruned   
  } // end for all nodes
  
  prev.id(GNULL);
  //if (task == 271) {
  for(it=boundary_tree.nodes().begin(); it!=boundary_tree.nodes().end(); it++) {
      if (prev.id()!=GNULL && !node_comp(prev, *it)) {
        std::cerr << "ERROR in Boundary tree output : " 
                  << " node: " << prev.id()
                  << std::setprecision(12)
                  << " val: " << prev.value()
                  << " next node: " << it->id()
                  << std::setprecision(12)
                  << " val: " << it->value()
                  << "\n";
        assert(node_comp(prev, *it));
      }
      prev = *it;

  }
  //std::cerr << "Done extracting boundary!! Task " << task << "\n";
  return 1;
}

