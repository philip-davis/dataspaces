/*
 * Task.cpp
 *
 *  Created on: Dec 12, 2014
 *      Author: bremer5
 */

#include "Task.h"


Task::Task(const Task& t) : mId(t.mId), mCallback(t.mCallback)
{
  mIncoming = t.mIncoming;
  mOutgoing = t.mOutgoing;
}

Task& Task::operator=(const Task& t)
{
  mId = t.mId;
  mCallback = t.mCallback;
  mIncoming = t.mIncoming;
  mOutgoing = t.mOutgoing;

  return *this;
}


