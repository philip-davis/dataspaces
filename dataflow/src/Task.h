/*
 * Task.h
 *
 *  Created on: Dec 12, 2014
 *      Author: bremer5
 */

#ifndef TASK_H
#define TASK_H

#include <vector>

#include "Definitions.h"

class TaskGraph;

/*! A task is the abstract description of a piece of idempotent
 *  computation in a dataflow. Each task is identified by a unique
 *  id and stores how many inputs from whom, it needs to run and
 *  how many outputs it will produce and to which task(s) these must
 *  be send.
 */
class Task
{
public:

  //! Default constructor
  Task(TaskId id = TNULL) : mId(id), mCallback(0) {}

  //! Copy constructor
  Task(const Task& t);

  //! Default destructor
  ~Task() {}

  //! Assignment operator
  Task& operator=(const Task& t);

  //! Return the id
  TaskId id() const {return mId;}

  //! Set the task id
  void id(TaskId i) {mId = i;}

  //! Return the callback
  CallbackId callback() const {return mCallback;}

  //! Set the callback
  void callback(CallbackId cb) {mCallback = cb;}

  //! Return the number of incoming messages
  const uint32_t fanin() const {return mIncoming.size();}

  //! Return the number of outgoing messages
  const uint32_t fanout() const {return mOutgoing.size();}

  //! Return the list of tasks expected to produce an input
  const std::vector<TaskId>& incoming() const {return mIncoming;}

  //! Return a reference to the incoming tasks
  std::vector<TaskId>& incoming() {return mIncoming;}

  //! Set the incoming tasks
  void incoming(std::vector<TaskId>& in) {mIncoming = in;}

  //! Return a reference to the outgoing tasks
  const std::vector<std::vector<TaskId> >& outputs() const {return mOutgoing;}

  //! Return a reference to the outgoing tasks
  std::vector<std::vector<TaskId> >& outputs() {return mOutgoing;}

  //! Set the outgoing tasks
  void outputs(const std::vector<std::vector<TaskId> >& out) {mOutgoing = out;}

  //! Return a list of tasks for the given output
  const std::vector<TaskId>& outgoing(uint32_t i) const {return mOutgoing[i];}

  //! Return a reference to the outgoing task for the given output
  std::vector<TaskId>& outgoing(uint32_t i) {return mOutgoing[i];}

private:

  //! The globally unique id of this task
  TaskId mId;

  //! The index of the callback associate with this task
  CallbackId mCallback;

  //! The set of tasks which will produce one of the inputs
  std::vector<TaskId> mIncoming;

  //! A list of outputs and the tasks to which they must be send
  std::vector<std::vector<TaskId> > mOutgoing;
};





#endif /* TASK_H_ */
