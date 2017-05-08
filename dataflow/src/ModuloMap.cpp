/*
 * ModuloMap.cpp
 *
 *  Created on: Dec 14, 2014
 *      Author: bremer5
 */

#include "ModuloMap.h"

ModuloMap::ModuloMap(ControllerId controller_count, TaskId task_count) : TaskMap(),
mControllerCount(controller_count), mTaskCount(task_count)
{
}

ControllerId ModuloMap::controller(TaskId id) const
{
  return (id % mControllerCount);
}

std::vector<TaskId> ModuloMap::tasks(ControllerId id) const
{
  std::vector<TaskId> back;

  TaskId t = id;

  while (t < mTaskCount) {
    back.push_back(t);

    t += mControllerCount;
  }

  return back;
}


