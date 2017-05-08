/*
 * Reduction.cpp
 *
 *  Created on: Dec 15, 2014
 *      Author: bremer5
 */

#include <cmath>

#include "Reduction.h"



Reduction::Reduction(uint32_t leafs, uint32_t valence) : TaskGraph(),
    mValence(valence)
{
  // Find the number of leafs that is a power of valence
  mLeafs = 1;
  mLevels = 0;

  while (mLeafs < leafs) {
    mLeafs *= mValence;
    mLevels++;
  }
}


std::vector<Task> Reduction::localGraph(ControllerId id, const TaskMap* task_map) const
{
  TaskId i;

  // First get all the ids we need
  std::vector<TaskId> ids = task_map->tasks(id);

  // The create the required number of tasks
  std::vector<Task> tasks(ids.size());
  std::vector<Task>::iterator it;

  //! Now assign all the task ids
  for (i=0;i<ids.size();i++)
    tasks[i].id(ids[i]);

  std::vector<TaskId> incoming(mValence); // There will be at most valence many incoming
  std::vector<std::vector<TaskId> > outgoing(1); // and one output
  outgoing[0].resize(1); // With one target

  //! Now assign the callback functions as well as the incoming and outgoing
  for (it=tasks.begin();it!=tasks.end();it++) {
    // First we assign the inputs

    // If we are a leaf
    if (it->id() >= (size() - leafCount())) {
      incoming.resize(1);
      incoming[0] = TNULL;

      it->incoming() = incoming;
    }
    else { // If we are not a leaf

      // We have valence many inputs
      incoming.resize(mValence);
      for (i=0;i<mValence;i++)
        incoming[i] = it->id()*mValence + i + 1;

      it->incoming() = incoming;
    }

    // Then we assign the outputs
    if (it->id() != 0) {// If we are not the root
      it->callback(1);

      outgoing[0][0] = (it->id() - 1) / mValence;
      it->outputs() = outgoing;
    }
    else {  //If we are the root
      it->callback(2);
    }
  }// end-for all tasks

  return tasks;
}
