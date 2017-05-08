/*
 * Broadcast.h
 *
 *  Created on: Dec 14, 2014
 *      Author: bremer5
 */

#ifndef BROADCAST_H_
#define BROADCAST_H_

#include <stdint.h>
#include <cmath>

#include "Definitions.h"
#include "TaskGraph.h"

//! A broadcast implements a task graph describing an tree based broadcast
class Broadcast : public TaskGraph
{
public:

  /*! Create a tree broadcast with at least the given number of leaf
   *  and the given fanout. All interior nodes will have callback=0
   *  all leaf task will have callback=1
   *
   * @param endpoints The minimal number of leafs
   * @param valence The fanout of the broadcast
   */
  Broadcast(uint32_t endpoints, uint32_t valence);

  //! Default destructor
  virtual ~Broadcast() {}

  //! Compute the fully specified tasks for the
  //! given controller and task map
  virtual std::vector<Task> localGraph(ControllerId id, const TaskMap* task_map) const;

  //! Return the total number of tasks
  TaskId size() const {return (pow(mValence,mLevels+1) - 1) / (mValence-1);}

private:

  //! The minimal number of endpoints
  uint32_t mEndpoints;

  //! The fanout
  uint32_t mValence;

  //! The number of levels
  uint32_t mLevels;
};



#endif /* BROADCAST_H_ */
