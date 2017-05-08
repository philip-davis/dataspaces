/*
 * SortedJoinAlgorithm.cpp
 *
 *  Created on: Feb 13, 2015
 *      Author: landge1
 */

#include "SortedJoinAlgorithm.h"
#include "MergeTree.h"
#include "BoundaryTreeExtractor.h"
#include "Comparison.h"
#include "UnionFind.h"

#include <iostream>
#include <iomanip>
#include <vector>
#include <unordered_map>

typedef std::priority_queue<QElement, std::vector<QElement>, NodeComp> PQueue;

void initialize_priority_queue(std::vector<MergeTree>& trees, int node_iterators[], 
                               PQueue& arcs_Q) {
  
  for (int i =0; i<trees.size(); i++) {
    assert(node_iterators[i] == 0);

    QElement new_element;

    // Add first node from every tree to the PQueue
    std::vector<TreeNode>::const_iterator it;

    // Trees could be empty due to thresholding
    if (trees[i].nodes().size() > 0) {
      it=trees[i].nodes().begin();

      new_element.node = &(*it);
      new_element.treeIdx = i;
      arcs_Q.push(new_element);
    }
  }
}


QElement get_highest_node(std::vector<MergeTree>& trees, int node_iterators[], 
                          PQueue& arcs_Q, TaskId task) {

  QElement top;
  top.node = NULL;
  std::vector<TreeNode>::const_iterator it;
  if (!arcs_Q.empty()) {
    top = arcs_Q.top();
    arcs_Q.pop();
    int treeIdx = top.treeIdx;

    //std::cerr << "Poping Node :: " << top.node->id() 
    //          << " Val: " << top.node->value()
    //          << " Tree Id: " << top.treeIdx << "\n"; 
    
    // If we have not reached the end of the nodes for this tree add a node from
    // tree to the arcs queue
    if (node_iterators[treeIdx] < (trees[treeIdx].nodes().size()-1)) {
      it = trees[treeIdx].nodes().begin() + node_iterators[treeIdx] + 1;
      QElement new_element; 
      new_element.node = &(*it);
      new_element.treeIdx = treeIdx;

      //if (task == 919) {
      //std::cerr << "Pushing Node :: " << new_element.node->id() 
      //        << " Val: " << new_element.node->value()
      //        << " Tree Id: " << new_element.treeIdx << "\n\n"; 
      //}

      arcs_Q.push(new_element);

      // increment the node iterator for this tree
      node_iterators[treeIdx]++;
    }
  }
  
  return top;
}


// checks if block covered by input trees do not overlap. 
// Returns true if overlap is encountered else false

bool check_blocks_overlap(std::vector<MergeTree>& trees) {

  GlobalIndexType low_A[3], low_B[3], high_A[3], high_B[3];
  bool overlap = false;

  for (int i=0; i<trees.size(); i++) {
    trees[i].blockBoundary(low_A, high_A);

    for (int j=i+1; j<trees.size(); j++) {
      trees[j].blockBoundary(low_B, high_B);
      
      // Check overlap in x-coordinates
      if (((low_A[0] < low_B[0]) && (low_B[0] < high_A[0])) ||
          ((low_A[0] < high_B[0]) && (high_B[0] < high_A[0]))) {
      
        // Now check overlap in y-coordinate
        if (((low_A[1] < low_B[1]) && (low_B[1] < high_A[1])) ||
          ((low_A[1] < high_B[1]) && (high_B[1] < high_A[1]))) {
          
          // Now check overlap in z-coordinate 
          if (((low_A[0] < low_B[2]) && (low_B[2] < high_A[2])) ||
          ((low_A[0] < high_B[2]) && (high_B[2] < high_A[2]))) {
          
            overlap = true;
          }// end z-if
        }// end y-if
      }// end x-if
    }// end for j
  }// end for i

  return overlap;
}

// Checks if sum of volume of individual blocks is equal to the volume of
// resultant block.
// Returns true if volumes match

bool check_block_structure(GlobalIndexType new_low[3], 
                           GlobalIndexType new_high[3],
                           std::vector<MergeTree>& trees) {
  
  std::unordered_map<GlobalIndexType, int> interior_corners;
  std::unordered_map<GlobalIndexType, int>::iterator mIt;
  GlobalIndexType l[3], h[3];
  GlobalIndexType low_id, high_id;
  bool correct_structure = true;
  
  for (int i=0 ; i<trees.size(); i++) {
    trees[i].blockBoundary(l, h);

    std::cerr << "entered here!\n"; 
    if ((l[0] != new_low[0]) && (l[1] != new_low[1]) && (l[2] != new_low[2])) {
      low_id = trees[i].global(l);
      mIt = interior_corners.find(low_id);
      std::cerr << "low_id : " << low_id << "\n";
      if (mIt == interior_corners.end()) {
        interior_corners[low_id] = 1;
      }
      else {
        std::cerr << "Decreasing\n";
        interior_corners[low_id]=interior_corners[low_id]-1;
      }
    }
    if ((h[0] != new_high[0]) && (h[1] != new_high[1]) && (h[2] != new_high[2])){
      high_id = trees[i].global(h);
      std::cerr << "high_id : " << high_id << "\n";
      mIt = interior_corners.find(high_id);
      if (mIt == interior_corners.end()) {
        interior_corners[high_id] = 1;
      }
      else {
        interior_corners[high_id]--;
      }
    }    
  }

  for (mIt=interior_corners.begin(); mIt!=interior_corners.end(); mIt++) {
    if (mIt->second != 0) {
      std::cerr << "map : " << mIt->first << " sec: " << mIt->second << "\n";
      correct_structure = false;
      break;
    }
  }
  
  return correct_structure;
}



// Computes the block boundary for the join tree using the blocks of the input
// trees. It checks that the resultant block is always a block and not any other
// shape. To test this, we check if any of the blocks overlap with each other
// and if the sum of the volume of input blocks is equal to the volume of the
// resultant block. 

void compute_block_boundary(GlobalIndexType new_low[3], 
                            GlobalIndexType new_high[3],
                            std::vector<MergeTree>& trees) {
 
  GlobalIndexType l[3], h[3];
  
  trees[0].blockBoundary(l, h);

  //std::cerr << "hhh  " << l[0] << " " << l[1] << " " << l[2] << "\n";

  new_low[0] = l[0];
  new_low[1] = l[1];
  new_low[2] = l[2];

  new_high[0] = h[0];
  new_high[1] = h[1];
  new_high[2] = h[2];

  for (int i=1; i<trees.size(); i++) {
    trees[i].blockBoundary(l, h);

    if (l[0] < new_low[0]) new_low[0] = l[0];
    if (l[1] < new_low[1]) new_low[1] = l[1];
    if (l[2] < new_low[2]) new_low[2] = l[2];

    if (h[0] > new_high[0]) new_high[0] = h[0];
    if (h[1] > new_high[1]) new_high[1] = h[1];
    if (h[2] > new_high[2]) new_high[2] = h[2];  
  }
  
  // TODO finish
  //assert(check_block_structure(new_low, new_high, trees));
}


void update_flags(const TreeNode* node, TreeNode* new_node) {
  
  // If the new node is a finalized saddle we update the flag
  if (node->finalizedSaddle()) {
    new_node->finalizedSaddle(true);
  }
}


int sorted_join_algorithm(std::vector<DataBlock>& inputs, 
                          std::vector<DataBlock>& outputs, TaskId task) {
  
  std::vector<MergeTree> input_trees(inputs.size());
  TreeNodeComp node_comp(MergeTree::greater);
  std::vector<TreeNode>::const_iterator it;

  // Decode the input trees
  for (int i=0; i<inputs.size(); i++) {
    input_trees[i].decode(inputs[i]);

    // Check if input trees are sorted
    //if (i==1 && task == 919) {
      for (it=input_trees[i].nodes().begin(); it!=input_trees[i].nodes().end(); it++) {
        if (it > input_trees[i].nodes().begin()) {
          if (((it-1)->id()!=GNULL) && !(node_comp(*(it-1), *it ))) {
            std::cerr << "ERROR in INPUT:: nodes should be sorted" 
                      << " node : " << (it-1)->id()
                      << std::setprecision(12)
                      << " val: " << (it-1)->value()
                      << " : next in vector : " << it->id()
                      << std::setprecision(12)
                      << " val: " << it->value()
                      << " task: " << task 
                      << "\n";
            assert(node_comp(*(it-1), *it));
          }
        }
      }
    //}
   
    // write out the input trees for debugging
    //input_trees[i].writeToFile(10*task+i);
  }

  // Compute the bounding box of the combined boxes of the input trees. We
  // enforce that this should always be a box
  GlobalIndexType new_high[3], new_low[3];
  compute_block_boundary(new_low, new_high, input_trees);

  // New tree being constructed using input trees
  MergeTree join_tree;
  join_tree.low(new_low);
  join_tree.high(new_high);

  // the following array maintains the indices of the node traverals in the
  // input trees. We initialize these to 0 as the nodes in the trees are already
  // sorted based on function value. We increment the corresponding index when a
  // node in the tree is popped from the priority queue.
  int* node_iterators = new int[inputs.size()]();

  // The priority queue that holds the max nodes from each of the input trees. The
  // top of the queue is the maximum node from all these individual max. The
  // size of the queue is equal to the number of input trees.
  NodeComp comp(MergeTree::greater);
  PQueue arcs_Q(comp);

  // Push the max node from every tree in the queue
  initialize_priority_queue(input_trees, node_iterators, arcs_Q);

  // Map for the new tree we are constructing
  std::unordered_map<GlobalIndexType,LocalIndexType> tree_map;

  QElement q_elem;
  q_elem = get_highest_node(input_trees, node_iterators, arcs_Q, task);
  
  // Union find structure used to construct new tree
  UnionFind uf;

  while (q_elem.node != NULL) {
    //if (task == 919 ) {
    //  std::cerr << "Node :: " << q_elem.node->id() 
    //            << std::setprecision(12)
    //            << " Val: " << q_elem.node->value()
    //            << " Tree Id: " << q_elem.treeIdx 
    //            << " Task : " << task << "\n"; 
    //}
    
    const TreeNode* node = q_elem.node;
    TreeNode* new_node;
    int treeIdx = q_elem.treeIdx;

    std::unordered_map<GlobalIndexType,LocalIndexType>::iterator mIt;
    mIt = tree_map.find(node->id());
    if (mIt == tree_map.end()) {

      // Add new node with boundary flag as false
      new_node = join_tree.addNode(node->id(), node->value(), false);
      
      // If the node is already marked as boundary and also lies on the boundary
      // of merging blocks then new node is on the boundary mark it as boundary
      if (node->boundary() && join_tree.boundary(node)) {
        new_node->boundary(true);
      }
            
      tree_map[node->id()] = new_node->index();
      uf.addLabel(node->id());
      //std::cerr << "Adding node: " << new_node->id() << " Task : " << task << "\n";
    }
    else {
      new_node = join_tree.getNode(mIt->second);
      //std::cerr << "Found existing node :: " << new_node->id() << " Task : " << task << "\n";
    }

    // Pass on the finalized saddle flag
    update_flags(node, new_node);
    
    // Check for upper node 
    TreeNode* upper_node;
    const TreeNode* parent_node;

    parent_node = node->up();

    if (node->up() != NULL) {
      do{
        mIt = tree_map.find(parent_node->id());

        // This has to be present in the tree as we are adding node in descending
        // order
        assert(mIt != tree_map.end());
        upper_node = join_tree.getNode(mIt->second);

        // find the lowest decendant of parent in the join tree
        GlobalIndexType descendant_id = uf.rep(upper_node->id());

        // if descendant id is different from new node then add edge
        if (descendant_id != new_node->id()) {
          join_tree.addEdge(join_tree.getNode(tree_map[descendant_id]), new_node);
          uf.mergeLabel(descendant_id, new_node->id());
          //std::cerr << "Edge Added : " << descendant_id << " -> " 
          //          << new_node->id()<< " Task : " << task << "\n";
        }
        parent_node = parent_node->next();
        
      }while(parent_node != node->up());
    }
    q_elem = get_highest_node(input_trees, node_iterators, arcs_Q, task);

  }

  for (it=join_tree.nodes().begin(); it!=join_tree.nodes().end(); it++) {
    if (it->down()!=NULL) {
      if (!(node_comp(*it, *(it->down())))) {
        std::cerr << "ERROR:: node : " << it->id()
                  << std::setprecision(12)
                  << " val: " << it->value()
                  << " : down : " << it->down()->id()
                  << std::setprecision(12)
                  << " val: " << it->down()->value()
                  << " task: " << task 
                  << "\n";
        assert(node_comp(*it, *(it->down())));
      }
    }
  }


  outputs[1] = join_tree.encode();
  for (int i=2; i<outputs.size(); i++) {
    DataBlock output_block;
    output_block.size = outputs[1].size;
    output_block.buffer = new char[output_block.size];
    memcpy(outputs[1].buffer, output_block.buffer, outputs[1].size);
    outputs[i] = output_block;
  }

  //join_tree.writeToFile(task + 5000);
  join_tree.pruneRegularNodes();
  //join_tree.writeToFile(task + 7000);

  MergeTree boundary_tree;
  //std::cerr << "About to extract boundary task :: " << task << "\n";

  extract_boundary_tree(join_tree, boundary_tree, task);

  //boundary_tree.writeToFile(task+10000);

  outputs[0] = boundary_tree.encode();

  //join_tree.writeToFile(task);
  
  //std::cerr << "Join complete! : Task : " << task << "\n";
  
  return 1;
}
