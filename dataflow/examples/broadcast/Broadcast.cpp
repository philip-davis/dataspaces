/*
 * broadcast.cpp
 *
 *  Created on: Dec 14, 2014
 *      Author: bremer5
 */

#include <cmath>
#include "Broadcast.h"
#include <iostream>

Broadcast::Broadcast(uint32_t endpoints, uint32_t valence) : TaskGraph(),
    mValence(valence)
{
  // Find the number of endpoints that is a power of valence
  mEndpoints = 1;
  mLevels = 0;

  while (mEndpoints < endpoints) {
    mEndpoints *= mValence;
    mLevels++;
  }
}

std::vector<Task> Broadcast::localGraph(ControllerId id, const TaskMap* task_map) const
{
  TaskId i;

  // First get all the ids we need
  std::vector<TaskId> ids = task_map->tasks(id);
//  for (int i=0; i< ids.size(); i++) {
//    std::cout << "tasks:: " << ids[i] << " id:: " << id <<"\n";
//  }

  // The create the required number of tasks
  std::vector<Task> tasks(ids.size());
  std::vector<Task>::iterator it;

  //! Now assign all the task ids
  for (i=0;i<ids.size();i++)
    tasks[i].id(ids[i]);

  std::vector<TaskId> incoming(1); // There will always be 1 incoming
  std::vector<std::vector<TaskId> > outgoing(1); // and one output
  outgoing[0].resize(mValence); // That goes to valence many other tasks

  //! Now assign the callback functions as well as the incoming and outgoing
  for (it=tasks.begin();it!=tasks.end();it++) {
    // If this is a leaf
    if (it->id() >= (size() - pow(mValence,mLevels)))
      it->callback(1);
    else { // If we are not the leaf
      it->callback(0); // We are a relay task

      // And we have valence many outputs
      for (i=0;i<mValence;i++)
        outgoing[0][i] = it->id()*mValence + i + 1;

      it->outputs() = outgoing;
    }

    // If we are not the root we have one incoming
    if (it->id() > 0) {
      incoming[0] = (it->id()-1) / mValence;
      it->incoming() = incoming;
    }
    else { // If we are the root we have one outside input
      incoming[0] = TNULL;
      it->incoming() = incoming;
    }
  }// end-for all tasks

  return tasks;
}


