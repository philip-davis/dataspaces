/*
 * KWayTaskMap.cpp
 *
 *  Created on: Dec 18, 2014
 *      Author: bremer5
 */

#include "KWayTaskMap.h"
#include <cstdio>

KWayTaskMap::KWayTaskMap(ControllerId controller_count,const KWayMerge* task_graph) :
mControllerCount(controller_count), mTaskGraph(task_graph)
{
}

ControllerId KWayTaskMap::controller(TaskId id) const
{
  TaskId base_id = mTaskGraph->baseId(id);
  std::vector<TaskId> up;

  while (base_id >= mTaskGraph->lvlOffset()[1]) {
    up = mTaskGraph->expand(base_id);
    base_id = up[0];
  }

  // The base_id now is the task id of the leaf task which we simply
  // distribute through round robin
  
  ControllerId cId = base_id % mControllerCount;
  //printf("base id: %d con count : %d cid : %d\n", base_id, mControllerCount, cId);
  return cId;
}

std::vector<TaskId> KWayTaskMap::tasks(ControllerId id) const
{
  std::vector<TaskId> tasks;
  uint8_t k;

  TaskId leaf_count = mTaskGraph->lvlOffset()[1];

  // For all leafs assigned to this controller
  for (TaskId leaf=id;leaf<leaf_count;leaf+=mControllerCount) {
    tasks.push_back(leaf); // Take the leaf

    // Now take its local copies for all rounds
    for (k=1;k<=mTaskGraph->rounds();k++) {
      tasks.push_back(mTaskGraph->roundId(leaf,k));
    }
    // Walk down the tree until your child is no longer
    // assigned to the same controller
    uint8_t lvl = 0;
    TaskId down = leaf;
    TaskId next = mTaskGraph->reduce(down);
    while ((down != next) &&  (down == mTaskGraph->expand(next)[0])) {
      lvl++;
      down = next;

      if (lvl < mTaskGraph->rounds()-1)
        next = mTaskGraph->reduce(next);

      tasks.push_back(down);

      // All lower nodes exist for all levels after this one
      for (k=lvl+1;k<mTaskGraph->rounds();k++)
        tasks.push_back(mTaskGraph->roundId(down,k));
    }// end-while
  } // end-for all leafs

  //printf("Controller id: %d Tasks: ", id);
  //for (int i=0; i<tasks.size(); i++)
  //  printf(" %d ", tasks[i]);
  //printf("\n");
  
  return tasks;
}
