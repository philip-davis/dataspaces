/*
 * LocalCorrectionAlgorithm.cpp
 *
 *  Created on: Feb 23, 2015
 *      Author: landge1
 */

#include "LocalCorrectionAlgorithm.h"
#include "AugmentedMergeTree.h"
#include "TreeNode.h"

#include <unordered_map>
#include <algorithm>
#include <cstdlib>
#include <unistd.h>
#include <stack>


//! The number of bits used for prefixing scatter tasks
static const uint8_t sPrefixSize = 4;

//! The number of non-prefix bits
static const uint8_t sPostfixSize = sizeof(TaskId)*8 - sPrefixSize;

//! Bit mask for scatter tasks
static const TaskId sPrefixMask = ((1 << sPrefixSize) - 1) << sPostfixSize;


//#define DEBUG_PRINTS
const int TASKID=7;
#ifdef DEBUG_PRINTS
//# define PRINT(x) do { std::cerr << "Task :: " << (task & ~sPrefixMask)\
                                << " : " x << std::endl; } while (0)
//# define PRINT_TASK(x) if ((task) == TASKID) 
# define PRINT_TASK(x) if ((task & ~sPrefixMask) == TASKID) \
                       {std::cerr << "Task :: " << TASKID << " : " << x << std::endl; }

#else
# define PRINT(x) do {} while (0)
# define PRINT_TASK(x) do {} while (0)
#endif



TreeNode* find_critical_parent(TreeNode* node){

  TreeNode* parent = node->up();
  bool found_parent = false;

  while((parent->up() != NULL) && // is not maxima
        !((parent->finalizedSaddle()) || (parent->up()->next()!=parent->up())) && // is not saddle
        !(parent->boundary())) { // is not boundary
    parent = parent->up();
  }
  return parent;
}

// Notes: we should maintain block boundary extremum and  only change the
// parents of these nodes. We also start the traversals from these nodes
// It should be easy to identify these nodes as these would be boundary extrema in
// the local computation.
// We do not need to update the parents of any other nodes the below check is
// only to check if the node has become a saddle. We should maintain a node in
// the local tree if its saddle or a finalized saddle. 
// The local tree does not differentiate between a finalized saddle and a saddle
// that only exists in the joined tree which may become regular in the future.
// In that case, the local correction will automatically change the status of
// the node to that of being regular.

void update_flags(TreeNode* lt_node, TreeNode* jt_node, TaskId task) {
  
  assert(lt_node->id() == jt_node->id());

  // Update boundary flags
  if (jt_node->boundary())
    lt_node->boundary(true);
  else
    lt_node->boundary(false);
 
  
  // if this node is a finalized saddle or a saddle in the joined tree mark it
  // as saddle. The local tree does not differentiate between a finalized saddle
  // and a join tree saddle. As in either case, this node needs to be preserved.
  // But if this is not saddle anymore we need to update the local tree saddle
  // flag accordingly since the local tree is persistent
  if (jt_node->finalizedSaddle()) 
    lt_node->finalizedSaddle(true);

  if ((jt_node->up()!=NULL) && (jt_node->up()->next()!=jt_node->up())) {
    lt_node->saddle(true);
  }
  else
    lt_node->saddle(false);

  return;
}

void update_parent(TreeNode* lt_node, AugmentedMergeTree& local_tree, 
                   TreeNode* jt_node, TaskId task,
                   std::unordered_map<GlobalIndexType, LocalIndexType>& local_tree_map) {

  // if this is a max in the joined tree we just return
  if (jt_node->up() == NULL)
    return;

  PRINT_TASK("Updating parent : " << lt_node->id() 
              << " Next: " << lt_node->next()->id());
  
  // if this is non local maxima or local max on the boundary then we need to
  // check if it has changed or has become regular or add a critical parent
  // If new nodes get added below the non local maxima, we prune the maxima
  // later by invoking the "prune_leaf_nodes" routine
  if (lt_node->up() == NULL) {
    
    // If this is still critical then we do nothing, else we find a critical
    // parent and attach it to this as this node has become regular and will
    // get pruned
    if (lt_node->saddle() || jt_node->boundary() || jt_node->finalizedSaddle())
      return;

    TreeNode* node = find_critical_parent(jt_node);
    
    TreeNode* new_parent = local_tree.addNode(node->id(), node->value(), 
                                              node->boundary());

    local_tree_map[new_parent->id()] = new_parent->index();

    update_flags(new_parent, node, task);
    
    // Refresh pointer of lt_node
    lt_node = local_tree.getNode(local_tree_map[jt_node->id()]);
    local_tree.addEdge(new_parent, lt_node);

    PRINT_TASK("New parent added:: " << new_parent->id() 
              << " : Child: " << new_parent->down()->id()
              << " : Child: " << lt_node->id()
              << " : Next : " << lt_node->next()->id()
              << " : Parent: " << lt_node->up()->id());
  }
  return;
}

// This is called only when a critical node is detected below the localmin. The
// nodes below the lt_min_node (argument to this function) are truncated
void update_root(TreeNode* lt_min_node, AugmentedMergeTree& local_tree, 
                 TreeNode* jt_node, TaskId task,
                 std::unordered_map<GlobalIndexType, LocalIndexType>& local_tree_map) {
  
  PRINT_TASK("Updating root : " << lt_min_node->id() 
            << " Join tree node : " << jt_node->id());

  assert(lt_min_node->id() == jt_node->id());
  assert(!jt_node->regular());
  assert(lt_min_node->saddle() || lt_min_node->finalizedSaddle() || lt_min_node->boundary());

  // we remove everything below lt_min_node
  if (lt_min_node->down()!=NULL) {
    TreeNode* node = lt_min_node->down();
    TreeNode* temp;
    PRINT_TASK("Selected root: " << lt_min_node);
    while (node!=NULL) {
      PRINT_TASK("Truncating at root : " << node->id() << " Parent: " 
            << node->up()->id() );
      temp = node->down();
      assert(node->up() == node->up()->next());
      local_tree_map.erase(node->id());
      local_tree.removeNode(node);
      node = temp;
    }
    assert(lt_min_node->down() == NULL);
  }
  return;
}


// This function determines the lowest descendant of high above low node. 
TreeNode* find_integration_node(TreeNode* high, TreeNode* low) {
  
  TreeNodeComp node_comp(MergeTree::greater);
  while((high->down()!=NULL) && (node_comp(*(high->down()), *low))) {
    high = high->down();
    if (high->id() == low->id())
      break;
  }

  return high;
}

// This function merges two branches on which the left and the right node
// reside. We first add an edge between left and right. Then we the merge the
// nodes from both branches like a single iteration of merge sort till we reach
// a common descandant.
void merge_branches(TreeNode* left, TreeNode* right, MergeTree& tree, TaskId task) {

  TreeNodeComp node_comp(MergeTree::greater);
  assert(left->id() != right->id());
  assert(node_comp(*left, *right));

  TreeNode* left_d;
  TreeNode* right_d;

  while ((left!=right) && (left->down()!=NULL)){
  
    left_d = left->down();
    right_d = right->down();

    // we add an edge between left and right
    tree.addEdge(left, right);
    tree.addEdge(right, left_d);

    // we check if we have reached the root of right
    if (right_d == NULL){
      return;
    }

    left = left->down();
    right = right_d;

    left = find_integration_node(left, right);
  }

  // if we not enter the above loop because left->down is NULL
  // then we just attach left to right and return
  if ((left->down() == NULL) && (left != right))
    tree.addEdge(left, right);

  return;
}

// Notes: As the local tree has the local block maxima, we cannot just rely on
// the traversal of both trees to guarantee a corrected local tree at the
// output. The problem arises due to the local block maxima not being present in
// the joined tree if they are regular in which case we can get spurious saddles
// in the output. To avoid this we use the following function to add an edge in
// the local tree. This function resolves any cycles that get formed if an edge
// is added by finding the lowest common descendant and then merging branches
// that form cyles.
void add_edge(TreeNode* head, TreeNode* tail, MergeTree& tree, TaskId task) {

  head = find_integration_node(head, tail);

  // If the tail is already a descendant of the head
  if (head == tail) {
    return;
  }
  else {
    merge_branches(head, tail, tree, task);
  }
  
  return;
}


int local_correction_algorithm(std::vector<DataBlock>& inputs, 
                               std::vector<DataBlock>& outputs, TaskId task) {


  TaskId tempId = task & ~sPrefixMask;
  
  // the augmented joined tree from the join routine
  MergeTree joined_tree;
  joined_tree.decode(inputs[0]);
  //if (tempId == TASKID)
    //joined_tree.writeToFile(task+2000);

  // the local tree with the segmentation information and labels
  AugmentedMergeTree local_tree;
  local_tree.decode(inputs[1]);
  local_tree.id(tempId);

  //if (tempId == TASKID){
  //local_tree.writeToFile(task);
  //}
  
  // Map for finding nodes in joined tree
  std::unordered_map<GlobalIndexType, LocalIndexType> joined_tree_map;

  // Populate above map
  std::vector<TreeNode>::const_iterator jIt;
  for (jIt=joined_tree.nodes().begin(); jIt!=joined_tree.nodes().end(); jIt++) {
    joined_tree_map[jIt->id()] = jIt->index();
  }
  
  // Map for finding nodes in local tree
  std::unordered_map<GlobalIndexType, LocalIndexType> local_tree_map;
  
  // We first create a vector of all the boundary nodes in the local tree, sort
  // it and then start the traversals from the parents of the boundary trees. We
  // find the corresponding node in the joined tree and the traverse both the
  // trees simultaneously and accordingly make changes in the local tree.
  
  // Creating the sorted list of boundary nodes from the local tree.
  // While we make this pass over the local tree we also create the map to look
  // up into the tree.  We need to check if the non-local maxima are present in
  // the joined tree. If they are present, then they lie on branch of a boundary
  // tree and could potentially affect the local tree. So we need to make
  // traversals from them to the root as well and hence add them to the sorted
  // list as well.
  
  std::vector<TreeNode> traversal_nodes;
  std::vector<TreeNode>::iterator lIt;

  int count =0 ;

  PRINT_TASK("Node size: " << local_tree.nodes().size());
  for (lIt=local_tree.modifyNodes().begin(); 
       lIt!=local_tree.modifyNodes().end(); lIt++) {

    // Populate the map for the local tree
    local_tree_map[lIt->id()] = lIt->index();

    if ( lIt->boundary() || lIt->blockBoundaryExtremum()) {
      traversal_nodes.push_back(*lIt);
      PRINT_TASK( "boundary : " << lIt->id());
    }

    if (lIt->up()==NULL && !(local_tree.isLocal(lIt->id()))) {
      traversal_nodes.push_back(*lIt);
      PRINT_TASK( "Non local leaf max : " << lIt->id() 
                  << " Index: " << lIt->index() << " Count : " << count);
    }

    // Mark all the nodes of the local tree as not visited
    lIt->visited(false);

    count++;
  }

  SameTreeNodeComp same_tree_node_comp(MergeTree::greater);
  std::sort(traversal_nodes.begin(), traversal_nodes.end(), same_tree_node_comp);

  TreeNodeComp node_comp(MergeTree::greater);
  std::vector<TreeNode>::iterator tIt;
  std::unordered_map<GlobalIndexType, LocalIndexType>::iterator j_mIt;
  std::unordered_map<GlobalIndexType, LocalIndexType>::iterator lt_mIt;

  TreeNode* lt_node;
  TreeNode* jt_node;
  TreeNode* traversal_node;
  GlobalIndexType lt_node_id;
  GlobalIndexType traversal_node_id;
  
  // Notes: The nodes from the local tree that lie on the boundary tree will
  // always be present in the joined tree as the joined tree is made from the
  // boundary tree of the local trees

  int count1=0;
  PRINT_TASK("Traversal node count " << traversal_nodes.size());
  for (tIt=traversal_nodes.begin(); tIt!=traversal_nodes.end(); tIt++){
    
    PRINT_TASK("Traversal Node : " << tIt->id() << " count: " << count1);
    count1++;
    // Some nodes may get pruned at the root and hence may no longer be part of
    // the tree
    lt_mIt = local_tree_map.find(tIt->id());
    if (lt_mIt == local_tree_map.end())
      continue;
    
    // Get the node in the local tree 
    traversal_node = local_tree.getNode(lt_mIt->second);
    PRINT_TASK("Traversing.1..getting node from tree : " << traversal_node->id() );
    
    // Now we traverse the tree downwards from this node
    while((traversal_node!=NULL) && !(traversal_node->visited())) {

      // Look up the traversal node in the joined tree
      j_mIt = joined_tree_map.find(traversal_node->id());

      PRINT_TASK("Traversing Node : " << traversal_node->id());

      // If we find the node in the joined tree we start the traversals
      if (j_mIt!=joined_tree_map.end()) {

        PRINT_TASK("Found : " << traversal_node->id() << 
                   " value :: " << traversal_node->value() << 
                   " Task : " << task);

        // The node may have been removed from the local tree as the root gets
        // updated during previous traversals. If node is no longer present in
        // local tree we just continue with the next iteration
        lt_mIt = local_tree_map.find(traversal_node->id());
        if (lt_mIt == local_tree_map.end())
          break; 

        // Get the node in the local tree and mark the node as visited
        lt_node = local_tree.getNode(local_tree_map[traversal_node->id()]);
        lt_node_id = lt_node->id();

        if (lt_node->visited()) 
          break;
        else
          lt_node->visited(true);

        // Get the node in the joined tree 
        jt_node = joined_tree.getNode(j_mIt->second);

        // Check if the node has become a saddle. If it was already a saddle,
        // check if the parents have changed. If they have changed or there are
        // new parents, then add the parents that are either on boundary, or are
        // saddle or maxima.
        //
        PRINT_TASK("Found in JT : " << jt_node->id() <<
                   " value :: " << jt_node->value() <<
                   " Found in LT : " << lt_node->id() <<
                   " Task : " << task);

        update_flags(lt_node, jt_node, task);
        // TODO Understand why this only modifies local maxima
        update_parent(lt_node, local_tree, jt_node, task, local_tree_map);   

        // Refresh the pointer to lt_node as we may add a node in the above call
        // which may modify the vector 
        lt_node = local_tree.getNode(local_tree_map[lt_node_id]);
      
        // now traverse both the trees till we reach the root of the tree
        while((jt_node->down() != NULL)) {
          

          if (lt_node->id() == jt_node->id()) {
            update_flags(lt_node, jt_node, task);
            if ((node_comp(*(local_tree.localMin()), *lt_node)) && 
                (lt_node->finalizedSaddle() || lt_node->boundary() || lt_node->saddle())) {
              update_root(lt_node, local_tree, jt_node, task, local_tree_map);
              PRINT_TASK("Updating root and breaking as lt_node is critical min");
              break;
            }
          }
          
          if (lt_node->down() != NULL) {
            PRINT_TASK("Traversing :: LT: " << lt_node->id() << 
                       " Val : " << lt_node->value() <<
                       " : JT : " << jt_node->id() << 
                       " Val : " << jt_node->value());
            
            lt_node->visited(true);
            // if next node is same in both trees and the these nodes have already
            // been visited we check for the parent of the current node and stop the
            // traversal
            if ((lt_node->down()->id() == jt_node->down()->id())) {
               
              // if we have already visited the down nodes we break the traversal
              if ((lt_node->down()->visited())) {
                PRINT_TASK("Found Visited.. Breaking");
                break;
              }

            }
            else { // down nodes are not equal means we need to modify local tree
              assert(lt_node->down()->id() != jt_node->down()->id());

              PRINT_TASK("Found different nodes : LT: " << lt_node->down()->id() <<
                         " Val : " << lt_node->down()->value() <<
                         " :JT: " << jt_node->down()->id() <<
                         " Val : " << jt_node->down()->value());
              
              // if the lt_node down is greater than jt_node down
              // This is possible only if we are lt_node down is a block boundary extremum
              // as they are not present in the joined tree. We use them as
              // markers in the local tree to prune the leaves later
              if (node_comp(*(lt_node->down()), *(jt_node->down()))) {
                
                if (lt_node->down()->blockBoundaryExtremum()) {
                  
                  PRINT_TASK("Fast forward: " << lt_node->down()->id());
                  lt_node = lt_node->down();
                  continue;
                }
                else { //error if we enter this else
                  std::cerr << "ERROR: node : " << lt_node->down()->id()
                            << " JT Node: " << jt_node->down()->id()
                            << " saddle " << lt_node->down()->saddle()
                            << " F saddle " << lt_node->down()->finalizedSaddle()
                            << " boundary  " << lt_node->down()->boundary()
                            << " Task :: " << tempId << " Real : " << task << "\n"; 
                  if (lt_node->down()->down() == NULL)
                    std::cerr << "LT Node is minima!!\n";

                 j_mIt = joined_tree_map.find(lt_node->down()->id());

                 if (j_mIt!=joined_tree_map.end()) {
                   TreeNode* node = joined_tree.getNode(j_mIt->second);
                   std::cerr << "#### node present in JT: " << node->id()
                              << " saddle " << node->saddle()
                              << " F saddle " << node->finalizedSaddle()
                              << " boundary  " << node->boundary()
                              << " up " << node->up()->id() << "\n";
                  if (node->down() == NULL)
                    std::cerr << "JT Node is minima!!\n";
                 }
                 else {
                    std::cerr << "Not found in JT!!\n";
                  }

                  assert(lt_node->down()->blockBoundaryExtremum());
                }
              }
              
              // if the lt_node down is less than jt_node down then we just add 
              // the node in the local tree
    
              // we first try to find the node in the local tree
              lt_mIt = local_tree_map.find(jt_node->down()->id());
              
              // if the node exists we add an edge between current node and the
              // found node
              if (lt_mIt != local_tree_map.end()) {

                PRINT_TASK("Diff node present in local tree: " <<
                           jt_node->down()->id() << " ..adding edge");
              
                TreeNode* node = local_tree.getNode(lt_mIt->second);
                add_edge(lt_node, node, local_tree, tempId);
              }
              else { // we add a new node in the tree

                // adding node may reallocate the vector hence storing id in a
                // variable for later look up in the map
                lt_node_id = lt_node->id();
                
                TreeNode* new_node = local_tree.addNode(jt_node->down()->id(),
                                                        jt_node->down()->value(),
                                                        jt_node->down()->boundary());
                
                lt_node = local_tree.getNode(local_tree_map[lt_node_id]);

                local_tree.addEdge(new_node, lt_node->down());
                PRINT_TASK("Adding node in local tree : " << jt_node->down()->id());
                local_tree.addEdge(lt_node, new_node);
                local_tree_map[new_node->id()] = new_node->index();
                
                PRINT_TASK("Adding new node : " << new_node->id() 
                          << " index : " << new_node->index()
                          << " Parent : " << lt_node->id()//new_node->up()->id() 
                          << " Val : " << lt_node->down()->value()
                          << " Child: " << lt_node->down()->id()
                          << " Val : " << new_node->down()->value());
              }
            } // end-if down nodes are not equal
          } // end-if lt-node down not null
          else { // if lt_node down is null then we add nodes till we reach local min
            
            assert(lt_node->id() == jt_node->id());
            
            update_flags(lt_node, jt_node, task);
            PRINT_TASK("Local tree has hit its root..");
            // add node here 
            lt_mIt = local_tree_map.find(jt_node->down()->id());
            
            // if the node exists we add an edge between current node and the
            // found node
            if (lt_mIt != local_tree_map.end()) {

              PRINT_TASK("At root Diff node present in local tree: " 
                        << jt_node->down()->id() << " ..adding edge");
            
              TreeNode* node = local_tree.getNode(lt_mIt->second);
              add_edge(lt_node, node, local_tree, tempId);
            }
            else {
              // adding node may reallocate the vector hence storing id in a
              // variable for later look up in the map
              lt_node_id = lt_node->id();
              TreeNode* new_node = local_tree.addNode(jt_node->down()->id(),
                                                      jt_node->down()->value(),
                                                      jt_node->down()->boundary());
              
              lt_node = local_tree.getNode(local_tree_map[lt_node_id]);
              PRINT_TASK("Attaching to local tree below : " << lt_node->id() << 
                         "node : " << jt_node->down()->id());
              local_tree.addEdge(lt_node, new_node);
              local_tree_map[new_node->id()] = new_node->index();
              
              
              PRINT_TASK("Adding new node at bottom: " << new_node->id() 
                        << " index : " << new_node->index()
                        << " Parent : " << lt_node->id()//new_node->up()->id() 
                        << " Val : " << lt_node->down()->value()
                        << " Child: " << lt_node->down()->id());
            }
          }// endif lt_node down is null
          // At this point both trees should be at identical nodes
          assert(lt_node->down()->id() == jt_node->down()->id());
          PRINT_TASK("Looping\n");

          update_flags(lt_node->down(), jt_node->down(),task);
          lt_node = lt_node->down();
          jt_node = jt_node->down();
          lt_node_id = lt_node->id();
        } // end-while jt_node down not null
       
        // traversal is complete from the traversal node to the root so we break
        break;
      }// endif node is in join tree
      else {  // if traversal node not in the joined tree we just mark it as 
              // visited and continue the traversal 
        traversal_node->visited(true);
        traversal_node = traversal_node->down();
      }
    }// end while traversal node visited or not null
    PRINT_TASK("Traversing next node... ");

  }// end for traversal nodes

  //if (tempId == TASKID)
  //local_tree.writeToFile(task+3000);
  
  local_tree.pruneRegularNodes();
  
  //if (tempId == TASKID)
  //local_tree.writeToFile(task+4000);
 
  outputs[0] = local_tree.encode();
  PRINT_TASK("DONE LOCAL CORRECTION :: " << task);
  
  return 1;
}


// DEBUG PRINTS

//int max_count=0;
  //int non_local_max_count=0;
  //int local_max_count=0;
  //int min_count=0;
  //int local_min_count=0;
  //int finalized_saddle_count=0;
  //int saddle_count=0;
  //for (lIt=local_tree.modifyNodes().begin(); 
  //     lIt!=local_tree.modifyNodes().end(); lIt++) {
  //  
  //  if (lIt->id()!=GNULL) {

  //      if (lIt->up() == NULL)
  //        max_count++;

  //      if (lIt->up() == NULL && !local_tree.isLocal(lIt->id()))  
  //        non_local_max_count++;

  //      if (lIt->blockBoundaryExtremum()) {
  //        local_max_count++;
  //      }

  //      if (lIt->down() == NULL)
  //        min_count++;

  //      if (lIt->saddle()) {
  //        saddle_count++;
  //      }
  //      if (lIt->finalizedSaddle()) {
  //        finalized_saddle_count++;
  //      }
  //  }
  //}

//std::cerr << "\nTASK :: " << task << " tempid : " << tempId 
  //          << " Max: " << local_max_count  
  //          << " Non-local Max: " << local_max_count  
  //          << " block boundary extremum: " << local_max_count  
  //          << " Min: " << local_min_count  
  //          << " Local block Min: " << local_min_count  
  //          << " Finalized saddles : " << finalized_saddle_count  
  //          << " Saddles: " << saddle_count  
  //          << "\n";

//  std::vector<LocalIndexType> local_max;
//  for (lIt=local_tree.modifyNodes().begin(); 
//       lIt!=local_tree.modifyNodes().end(); lIt++) {
//    
//    if (lIt->id()!=GNULL) {
//      if ((lIt->up()==NULL) && !(local_tree.isLocal(lIt->id()))) {
//        local_max.push_back(lIt->index());
//      }
//
//      //if (task == 536870939 && lIt->id() == 143159260) {
//      //  if (lIt->blockBoundaryExtremum()) {
//      //    //local_max_count++;
//      //    std::cerr << "Maxima: " << lIt->id() << " task : " << tempId << "\n";
//      //  }
//      //  if (lIt->saddle()) {
//      //    //saddle_count++;
//      //    std::cerr << "Saddle: " << lIt->id() << " task : " << tempId << "\n";
//      //  }
//      //}
//    }
//  }
  //local_tree.writeToFile(task);

