/*
 * Reduction.h
 *
 *  Created on: Dec 15, 2014
 *      Author: bremer5
 */

#ifndef REDUCTION_H_
#define REDUCTION_H_

#include <stdint.h>
#include <cmath>

#include "Definitions.h"
#include "TaskGraph.h"

class Reduction : public TaskGraph
{
public:

  /*! Create a tree based reduction with at least the given number of
   *  leafs and the given fanin. All nodes but the root will
   *  have callback=1 and the root will have callback=2. Note that for
   *  simplicity this create a full tree if necessary increasing the
   *  number of leafs. Leafs will be the last leafCount() many tasks
   *
   * @param leafs: The minimal number of leafs to create
   * @param valence: The fanin of the reduction
   */
  Reduction(uint32_t leafs, uint32_t valence);

  //! Default destructor
  virtual ~Reduction() {}

  //! Compute the fully specified tasks for the
  //! given controller id and task map
  virtual std::vector<Task> localGraph(ControllerId id, const TaskMap* task_map) const;

  //! Return the total number of tasks
  TaskId size() const {return (pow(mValence,mLevels+1) - 1) / (mValence-1);}

  //! Return the number of leafs
  TaskId leafCount() const {return pow(mValence,mLevels);}

private:

  //! The number of leafs in the reduction
  uint32_t mLeafs;

  //! The fanout
  uint32_t mValence;

  //! The number of levels in the tree
  uint32_t mLevels;
};



#endif /* REDUCTION_H_ */
